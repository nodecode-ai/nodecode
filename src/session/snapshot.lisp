;;;; snapshot.lisp --- shadow-git workspace snapshots with delta retention.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Whole-tree workspace capture into a bare shadow repository at
;;;; <home>/snapshots/<key>/, KEY the digest of the workspace's canonical path,
;;;; driven through the git CLI. The store belongs to the configured home,
;;;; not a repository a cloned workspace could ship with an fsmonitor hook
;;;; or a filter driver. When that home is inside the workspace, the shared
;;;; snapshot storage is excluded from capture.
;;;; Content addressing makes unchanged captures free (same tree sha) and
;;;; dedups blobs across sessions; the repack lane compacts loose objects
;;;; into packfile delta chains so history costs deltas, not copies.
;;;;
;;;; Contract:
;;;;   - one store per workspace, one index per session (index-<session-id>),
;;;;     so concurrent sessions never contend on index.lock; a new session's
;;;;     index starts as a copy of the newest one (SEED-SESSION-INDEX), so
;;;;     its first walk rehashes what changed, not the whole tree
;;;;   - every captured tree is anchored on the chained ref
;;;;     refs/nodecode/checkpoints; an unanchored write-tree is gc bait
;;;;   - gc.auto stays 0; REPACK-STORE is the only compactor and never prunes
;;;;   - restore is strictly per-path and hash-guarded: a path whose on-disk
;;;;     content differs from the session's last recorded write is a
;;;;     :foreign-write conflict, reported and left alone (never clobbered)
;;;;   - a walk stages at most +SNAPSHOT-WALK-MAX-PATHS+ paths and
;;;;     +SNAPSHOT-WALK-MAX-BYTES+ bytes: past either it is skipped whole,
;;;;     never begun (:workspace-too-large), and a scratch or system folder
;;;;     (/tmp, /usr, the folder of homes) is never a workspace at all
;;;;   - capture NEVER fails a turn: git missing or broken degrades to a
;;;;     durable SESSION.WORKSPACE_CAPTURED fact with a nil tree sha and an
;;;;     unavailable-reason
;;;;   - all subprocess work runs OUTSIDE *STORE-LOCK*; durable facts commit
;;;;     in their own short transactions afterwards
;;;;
;;;; Limitations (by design, documented): writes outside the session's
;;;; recorded cwd are not snapshotted (EVAL has full host authority and
;;;; may write anywhere); a thread writing during restore races the hash
;;;; guard (same TOCTOU exposure as the Zig store).


(in-package #:nodecode.kernel)

(access (live store))

;;; --- constants and specials ----------------------------------------------

(defparameter +snapshot-anchor-ref+ "refs/nodecode/checkpoints")
(defparameter +snapshot-git-timeout-seconds+ 60)
;;; Diffing the staged worktree against it classifies every on-disk path as
;;; departing — the claims-nothing baseline for a chain with no capture.
(defparameter +git-empty-tree-sha+ "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  "The empty tree, hardwired inside git itself (always resolvable, never
stored).")
(defparameter +snapshot-max-output-bytes+ (* 16 1024 1024))
;;; A walk writes a blob for every path it stages before any tree exists, so
;;; a folder the size of a disk fills the disk the home sits on: a 36 GB /tmp
;;; staged 9.9 GB through walks the git bound cut short, one after another,
;;; until the home's disk was full. CHECK-WALK-BOUND counts what a walk would
;;; stage first; a walk past either bound is skipped whole, never begun.
(defparameter +snapshot-walk-max-paths+ 200000
  "The most paths one walk may stage: a large source tree is a tenth of it.")
(defparameter +snapshot-walk-max-bytes+ (* 2 1024 1024 1024)
  "The most bytes one walk may stage.")
(defparameter +snapshot-packed-staging-paths+ 256
  "A staging of more paths than this writes its blobs into one pack, not a
loose object each (STAGE-WORKSPACE).")
(defvar *git-program* "git"
  "Test seam: bind to a nonexistent path to force unavailability.")
;;; A TRUE GLOBAL (not a special): every
;;; thread — hunchentoot workers, turn workers, navigate threads — reads the
;;; same variable, and SETF is the only way to change policy. A DEFVAR here was a
;;; proven leak: the test runner's LET binding was thread-local, so gateway
;;; worker threads kept reading the armed global and ADD -A'd the live repo
;;; into a shadow store. DEFGLOBAL makes a LET binding a compile-time error,
;;; so no call site can reintroduce that split-brain. The test runner SETFs
;;; this NIL for the whole suite; tests that exercise captures flip it around
;;; controlled temp workspaces and restore it. The operator's switch is the
;;; shared config's workspace.captures, read once at gateway start
;;; (START-GATEWAY) — the one config plane, no CLI twin.
(sb-ext:defglobal *workspace-capture-enabled* t
  "Kill switch for boundary captures.")
(defvar *git-available* :unknown
  "Memoized GIT --VERSION probe: :unknown, T, or NIL.")

;;; --- the capture refusal memo --------------------------------------------
;;; A capture that fails is remembered per index (store and session) for a
;;; while. Without the memo, a workspace git cannot stage would walk once per
;;; boundary; with it, once per window. The memo expires so a transient
;;; failure (a lock, a permission fixed by the operator) heals without a
;;; restart.

(define-side-table capture-refusals (:test #'equal :synchronized t :key snapshot-store-index-path)
  "INDEX-PATH -> (REASON . UNIVERSAL-TIME): the last capture failure of that
index and when it was remembered. Strings and integers only.")

(defun capture-refusal (store)
  "The remembered failure reason for STORE's index while its window holds,
else NIL; an expired memo is forgotten on the way out."
  (sb-ext:with-locked-hash-table (*capture-refusals*)
    (let ((entry (capture-refusals store)))
      (cond ((null entry) nil)
            ((> (- (get-universal-time) (cdr entry)) 600)
             (forget-capture-refusals store)
             nil)
            (t (car entry))))))

;;; --- store handle --------------------------------------------------------

(define-record (snapshot-store (:copier nil) (:export :predicate workspace git-dir))
  "Value object: no open handles, no threads — nothing to clean for
SAVE-LISP-AND-DIE or fork."
  (workspace nil :type (or null string))
  (git-dir nil :type (or null string))
  (index-path nil :type (or null string)))

;;; --- condition taxonomy --------------------------------------------------
;;; Internal only: the capture and restore orchestration boundaries convert
;;; every signal into degradation plists. Unavailability is reported, never
;;; signalled past this file.

(define-error snapshot-error (error) ((detail :initform ""))
  (:report "snapshot error: ~a" detail))

(define-condition snapshot-git-unavailable (snapshot-error) ())

(define-error snapshot-git-failed (snapshot-error)
    ((arguments :initform nil) (exit-status :initform nil)
     (stderr :initform ""))
  (:report "git ~{~a~^ ~} exited ~a: ~a" arguments exit-status stderr))

(define-condition snapshot-output-too-large (snapshot-error) ())

(define-error snapshot-walk-too-large (snapshot-error) ()
  (:report "~a" detail))

(define-error invalid-tree-sha (snapshot-error) (value)
  (:report "not a tree sha: ~s" value))

(defun tree-sha-p (value)
  "Whether VALUE is a tree sha: forty hex digits."
  (and (stringp value) (= (length value) 40) (every (lambda (ch) (digit-char-p ch 16)) value)))

(defun validate-tree-sha (value)
  (unless (tree-sha-p value)
    (error 'invalid-tree-sha :value value :detail "malformed tree sha"))
  value)

;;; --- git invocation layer ------------------------------------------------

(defun snapshot-git-environment (store &key (with-work-tree t))
  "The environment laid over this process's for every git call
(RUN-BOUNDED's ENVIRONMENT)."
  ;; Overlaying the parent environment is equivalent to scrubbing it: NOSYSTEM
  ;; plus a null-device global config neutralize inherited configuration, and
  ;; PATH inheritance is exactly what resolves git.
  (nconc
   (list (format nil "GIT_DIR=~a" (snapshot-store-git-dir store)))
   (when with-work-tree
     (list (format nil "GIT_WORK_TREE=~a" (snapshot-store-workspace store))))
   (list (format nil "GIT_INDEX_FILE=~a" (snapshot-store-index-path store))
         "GIT_AUTHOR_NAME=nodecode" "GIT_AUTHOR_EMAIL=nodecode@localhost"
         "GIT_COMMITTER_NAME=nodecode" "GIT_COMMITTER_EMAIL=nodecode@localhost"
         "GIT_CONFIG_NOSYSTEM=1"
         #-win32 "GIT_CONFIG_GLOBAL=/dev/null" #+win32 "GIT_CONFIG_GLOBAL=NUL"
         "GIT_TERMINAL_PROMPT=0"
         ;; WALK-CENSUS reads the lines `add --dry-run' prints: untranslated
         "LC_ALL=C")))

(defun run-snapshot-git (store args &key (with-work-tree t) config input)
  "Run one git plumbing call under the isolation prefix and a walltime bound;
CONFIG, KEY=VALUE strings, rides this one call as -c options, and INPUT, a
string, is its stdin."
  ;; Returns stdout as a string; any exit but 0 signals the snapshot condition
  ;; taxonomy.
  ;;
  ;; core.fsmonitor is the one driver a plain ADD runs that configuration
  ;; alone switches on, and configuration can still arrive from this
  ;; process's environment (GIT_CONFIG_PARAMETERS): the empty value turns it
  ;; off on every git that reads it, as a bool or as a hook path.
  (multiple-value-bind (output error-output status)
      (handler-case
          (run-bounded (list* *git-program* "-c" "core.fsmonitor="
                              ;; GIT_CONFIG_NOSYSTEM drops the system config
                              ;; where Git for Windows turns its fscache on:
                              ;; DIFF-FILES and ADD -A walk a 20,000-file tree
                              ;; in a third of the time with it (T-442).
                              #+win32 "-c" #+win32 "core.fscache=true"
                              (append (loop for setting in config collect "-c" collect setting)
                                      args))
                       :seconds +snapshot-git-timeout-seconds+
                       :input input
                       :environment (snapshot-git-environment
                                     store :with-work-tree with-work-tree)
                       :directory (snapshot-store-workspace store))
        (error (condition)
          (error 'snapshot-git-unavailable
                 :detail (format nil "git spawn failed: ~a" condition))))
    (unless (eql status 0)
      (error 'snapshot-git-failed
             :arguments args :exit-status status
             :stderr (or error-output "")
             :detail (if (eq status :timeout) "timeout" "nonzero exit")))
    (let ((text (or output "")))
      (when (> (length text) +snapshot-max-output-bytes+)
        (error 'snapshot-output-too-large
               :detail (format nil "git output ~a bytes" (length text))))
      text)))

(defun git-line (store args)
  "RUN-SNAPSHOT-GIT's answer with its newline and spaces trimmed: a sha, a word."
  (string-trim '(#\Newline #\Space) (run-snapshot-git store args)))

(defun git-available-p ()
  "Memoized probe: can *GIT-PROGRAM* be spawned at all?"
  (when (eq *git-available* :unknown)
    (setf *git-available*
          (exits-zero-p (list *git-program* "--version"))))
  *git-available*)

;;; --- store lifecycle -----------------------------------------------------

(defun snapshot-store-file (store name)
  "The file NAME inside STORE's shadow git directory."
  (merge-pathnames name (uiop:ensure-directory-pathname (snapshot-store-git-dir store))))

(defun snapshot-storage-path (store)
  "The shared snapshot storage relative to STORE's workspace, or NIL outside it."
  (when-let (relative (uiop:subpathp
                       (uiop:pathname-parent-directory-pathname
                        (truename (snapshot-store-git-dir store)))
                       (truename (snapshot-store-workspace store))))
    (let ((path (uiop:unix-namestring relative)))
      (if (zerop (length path)) "." path))))

(defparameter +store-config+
  (format nil "[core]~%~cbare = false~%~cuntrackedCache = true~%~csymlinks = true~%~
               [gc]~%~cauto = 0~%[index]~%~cversion = 4~%"
          #\Tab #\Tab #\Tab #\Tab #\Tab)
  "What a new store's config says past `git init's own lines.")
;; gc.auto=0 is load-bearing: an auto-gc between capture and undo could prune
;; a tree the undo needs.

(defvar *store-init-lock* (bt2:make-lock :name "snapshot-store-init")
  "Held while a store is initialized: a deferred capture and a tool step's
bracket may open a new workspace's store at once.")

(defparameter +store-attributes+ "* -text -eol -ident -filter -working-tree-encoding"
  "A snapshot store's info/attributes: every path's bytes as they are on disk.")

(defun write-store-attributes (store &aux (path (snapshot-store-file store "info/attributes")))
  "Keep STORE's info/attributes at +STORE-ATTRIBUTES+. => true when it had to
be written."
  ;; A checkpoint is the workspace's bytes. The workspace's own .gitattributes
  ;; serve its repository: `text' and `eol' stored a CRLF file as LF and the
  ;; checkout-index of a restore wrote it back in the store's line ending,
  ;; `ident' and `filter' rewrote content, `working-tree-encoding' re-encoded
  ;; it, and the restore guard hashed through the same conversions, blind to
  ;; all of it. A converted file also skipped the bulk pack for a loose
  ;; object of its own: the first walk of a 540-file repository whose sources
  ;; carry `text eol=lf' spent 1.6 s writing them on Windows (T-442).
  ;; info/attributes outranks every attributes file in the tree, so unsetting
  ;; them all here stores, hashes and restores the bytes on disk.
  (unless (equal +store-attributes+
                 (with-handlers ((error () nil))
                   (string-trim '(#\Return #\Newline #\Space) (uiop:read-file-string path))))
    (with-output-file (stream path)
      (write-line +store-attributes+ stream))
    t))

(defun drop-session-indexes (store)
  "Delete every session index in STORE's git directory: the next walk of each
session is a first walk."
  (dolist (path (with-handlers ((error () nil))
                  (uiop:directory-files (uiop:ensure-directory-pathname
                                         (snapshot-store-git-dir store)))))
    (let ((name (file-namestring path)))
      (when (and (uiop:string-prefix-p "index-" name)
                 (not (uiop:string-suffix-p name ".lock"))
                 (git-index-file-p path))
        (with-handlers ((error () nil))
          (delete-file path))))))

(defun write-store-exclude (store &aux (path (snapshot-store-file store "info/exclude")))
  ;; Written only when it would change: every tool step opens its store, and
  ;; a file written each time is a create on every step (T-442).
  (let ((text (with-output-to-string (stream)
                ;; An undo must never roll back the event log that recorded it — hence
                ;; the sqlite patterns (Zig parity) and the organism's own state dir.
                (dolist (line (list "*.sqlite" "*.sqlite-wal" "*.sqlite-shm" "*.sqlite-journal"
                                    "/.nodecode/"))
                  (write-line line stream))
                ;; The live sqlite store when its path resolves under the workspace (a
                ;; workspace can be $HOME or contain a custom store path): the database
                ;; plus every sidecar SQLite and the store lifecycle put beside it.
                (let ((live *store*))
                  (when (and live (store-open-p live))
                    (let ((db (namestring live.path))
                          (root (snapshot-store-workspace store)))
                      (when (uiop:string-prefix-p root db)
                        (dolist (suffix '("" "-wal" "-shm" "-journal" ".lock" ".owner"))
                          (format stream "/~a~a~%" (subseq db (length root)) suffix)))))))))
    (unless (equal text (with-handlers ((error () nil)) (uiop:read-file-string path)))
      (with-output-file (stream path)
        (write-string text stream)))))

(defun snapshot-git-dir (workspace)
  "The shadow store's git directory for WORKSPACE, a canonical directory
namestring: a folder of the home named by the digest of that path."
  ;; One store per workspace per home: a home's events name trees of its own
  ;; store. Two spellings of one folder (a symlink, a relative path) are one
  ;; canonical path, hence one store.
  (home (format nil "snapshots/~a/" (subseq (sha256-text workspace) 7 39))))

(defparameter +system-directories+
  '("/tmp/" "/var/tmp/" "/dev/shm/" "/private/tmp/" "/private/var/tmp/"
    "/var/" "/usr/" "/etc/" "/opt/" "/srv/" "/mnt/" "/media/" "/boot/" "/run/"
    "/proc/" "/sys/" "/dev/" "/home/" "/Users/" "/Volumes/" "/private/")
  "Scratch space, system trees and the folders of every account, as canonical
directory namestrings: never one workspace.")

(defun system-directory-p (canonical-root)
  "Whether CANONICAL-ROOT, a canonical directory namestring, is a folder no
workspace can be: one of +SYSTEM-DIRECTORIES+, this process's temporary
directory, the folder holding this account's home, or a system folder the
environment names (SystemRoot, ProgramFiles, ProgramData on Windows)."
  ;; Only the folder itself: a project inside /tmp is a workspace like any
  ;; other, and the walk bound answers for its size.
  (flet ((same (path)
           (and path
                (equal canonical-root
                       (ignore-errors
                        (namestring (uiop:ensure-directory-pathname (truename path))))))))
    (or (member canonical-root +system-directories+ :test #'equal)
        (same (uiop:temporary-directory))
        (same (uiop:pathname-parent-directory-pathname (user-homedir-pathname)))
        (let ((account (account-home)))
          (and account (same (uiop:pathname-parent-directory-pathname account))))
        (some #'same (mapcar #'uiop:getenv
                             '("SystemRoot" "ProgramFiles" "ProgramFiles(x86)" "ProgramData"))))))

(defun ensure-snapshot-store (workspace session-id)
  "Open (initializing when absent) the shadow store for WORKSPACE/SESSION-ID."
  ;; => (values SNAPSHOT-STORE-or-NIL unavailable-keyword-or-NIL). Never signals.
  (with-handlers ((snapshot-git-unavailable () (values nil :git-missing))
                  (error () (values nil :store-init-failed)))
    (let ((root (uiop:ensure-directory-pathname workspace)))
      (cond
        ((not (git-available-p)) (values nil :git-missing))
        ((not (eq (path-kind root) :directory)) (values nil :store-init-failed))
        (t (multiple-value-bind (canonical-root refusal) (workspace-standing root)
             (if refusal
                 (values nil refusal)
                 (open-snapshot-store root (snapshot-git-dir canonical-root) session-id))))))))

;;; A workspace's standing -- the folder it canonically is, or why it can be
;;; none -- rests on truenames: its own, $HOME's, the account's, the temporary
;;; folder's and, on Windows, each system folder's, every one a CreateFile and
;;; a path query there (~100 us apiece, T-442). Every tool call asks; the
;;; folders it compares stand still for the life of a process, so it is
;;; decided once for each folder and each set of the names it is compared by.
(define-side-table workspace-standings (:test #'equal :synchronized t)
  "(WORKSPACE HOME ACCOUNT-HOME TEMPORARY . SYSTEM-FOLDERS) -> (CANONICAL-ROOT . REFUSAL).")

(defun workspace-standing (root)
  "(values CANONICAL-ROOT REFUSAL) for the existing folder ROOT: the one
namestring its spellings share, or the reason it is no workspace to stage."
  (let* ((key (list* (namestring root) (namestring (user-homedir-pathname)) (account-home)
                     (namestring (uiop:temporary-directory))
                     (mapcar #'uiop:getenv '("SystemRoot" "ProgramFiles" "ProgramFiles(x86)" "ProgramData"))))
         (known (workspace-standings key)))
    (if known
        (values (car known) (cdr known))
        (flet ((canonical (path) (canonical-workspace path)))
          (let* ((canonical-root (canonical root))
                 ;; $HOME and a filesystem root are never a workspace to stage:
                 ;; refused before any git runs, and recorded as the reason. The
                 ;; account's home too, whatever $HOME this image was handed: a
                 ;; scratch $HOME does not make the real one a workspace. Nor is
                 ;; /tmp or a system folder (SYSTEM-DIRECTORY-P).
                 (refusal (when (or (null canonical-root)
                                    (equal canonical-root (canonical (user-homedir-pathname)))
                                    (let ((account (account-home)))
                                      (and account (equal canonical-root (canonical account))))
                                    ;; no directory, or the filesystem root alone
                                    (null (rest (pathname-directory canonical-root)))
                                    (system-directory-p canonical-root))
                            :workspace-too-large)))
            ;; A folder that would not resolve may yet: only an answer is kept.
            (when canonical-root
              (setf (workspace-standings key) (cons canonical-root refusal)))
            (values canonical-root refusal))))))

(defun canonical-workspace (path)
  "PATH's directory as one namestring for every spelling of it, or NIL when
it names none."
  (ignore-errors
   (namestring (uiop:ensure-directory-pathname (truename path)))))

;;; --- the launch walk ------------------------------------------------------
;;; A workspace's first walk stages every file: about 600 ms for 540 files on
;;; Windows, where the first message of a session would otherwise wait on it
;;; at its first tool call (T-442). A shell starting in a folder knows that
;;; folder before anything else, so it walks it then, on a thread of its own,
;;; into the store's launch index -- while the organism boots and the person
;;; types. The session that follows seeds its index from that one
;;; (SEED-SESSION-INDEX), and the tree it wrote rides beside it, so the
;;; session's first capture is its census alone. The organism is another
;;; process: the walk holds a claim file beside the store for as long as it
;;; runs, and a store opened meanwhile waits for it rather than walk the same
;;; tree a second time beside it.

(defparameter +launch-session-id+ "launch"
  "The session name of the launch walk's index: index-launch in the store.")

(defvar *launch-walk-p* nil
  "True on the thread of a launch walk, which never waits on its own claim.")

(defvar *held-launch-claims* '()
  "The launch walk claims this process holds, released before an exec: the
pid in them is the successor's, whose walk thread is gone.")

(define-side-table armed-workspace-journal (:test #'equal :synchronized t)
  "Workspace root namestring -> (EPOCH . CLOCK): the epoch its journal stood
at when ARM-WORKSPACE-JOURNAL opened it, and the JOURNAL-CLOCK read once it
was armed. Live-only, as the journals are.")

(defun drop-armed-workspace-journals ()
  "Forget every arming: a dumped image holds none of this process's journals."
  (clrhash *armed-workspace-journal*))

(pushnew 'drop-armed-workspace-journals sb-ext:*save-hooks*)

(defun arm-workspace-journal (directory)
  "Open the journal of the workspace DIRECTORY now, ahead of its first
capture, and keep the epoch and clock it was armed at: a launch walk another
process settled after that clock is the workspace as it stands while the
epoch holds (ADOPT-LAUNCH-TREE). => the epoch, or NIL where no journal can
say. Never signals."
  ;; The journal is the one every capture of DIRECTORY reads after (one per
  ;; root namestring), its ignored paths read now: the home's snapshot
  ;; storage when it sits in the workspace and already exists.
  (with-handlers ((error () nil))
    (when-let (canonical (canonical-workspace (uiop:ensure-directory-pathname directory)))
      (let* ((root (namestring (uiop:ensure-directory-pathname directory)))
             (probe (make-snapshot-store
                     :workspace root
                     :git-dir (string-right-trim "/" (namestring (snapshot-git-dir canonical)))))
             (epoch (workspace-journal-epoch
                     root :ignored (lambda () (ignore-errors (journal-ignored-paths probe)))))
             (clock (and epoch (journal-clock))))
        (when clock
          (setf (armed-workspace-journal root) (cons epoch clock))
          epoch)))))

(defun release-launch-walk-claims ()
  "Delete every launch walk claim this process holds."
  (dolist (claim *held-launch-claims*)
    (ignore-errors (delete-file claim)))
  (setf *held-launch-claims* '()))

(defun launch-walk-claim (git-dir)
  "The claim file a launch walk of GIT-DIR's store holds: beside the store,
which may not exist yet."
  (concatenate 'string (string-right-trim "/\\" (namestring git-dir)) ".walking"))

(defun launch-walk-live-p (claim)
  "True while the launch walk holding CLAIM runs: the file stands, younger than
the git bound, and names a live process."
  (with-handlers ((error () nil))
    (let ((date (file-write-date claim))
          (pid (parse-integer (read-text claim) :junk-allowed t)))
      (and date (< (- (get-universal-time) date) +snapshot-git-timeout-seconds+)
           pid (pid-live-p pid)))))

(defun await-launch-walk (git-dir)
  "Return once no launch walk of GIT-DIR's store runs, within the git bound."
  (let ((claim (launch-walk-claim git-dir))
        (deadline (deadline-after +snapshot-git-timeout-seconds+)))
    (loop while (and (probe-file claim) (launch-walk-live-p claim)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.005))))

(defun launch-tree-file (store)
  "The file beside STORE's index naming the tree that index was walked to."
  (concatenate 'string (snapshot-store-index-path store) ".tree"))

(defun walk-at-launch (directory &aux claim)
  "Walk DIRECTORY into its store's launch index and anchor the tree, under
the store's launch claim. => the tree's sha, or NIL when DIRECTORY is no
workspace or another launch walk holds the claim."
  (when-let (root (canonical-workspace (uiop:ensure-directory-pathname directory)))
    (setf claim (launch-walk-claim (snapshot-git-dir root)))
    (ensure-directories-exist claim)
    (when (claim-file claim +snapshot-git-timeout-seconds+)
      (push claim *held-launch-claims*)
      (unwind-protect
           (let ((*launch-walk-p* t))
             (when-let (store (ensure-snapshot-store directory +launch-session-id+))
               (let* ((tree (capture-anchored-tree store "launch walk"))
                      ;; Settled: the census after a first walk writes the
                      ;; untracked cache into its index, and the tree that
                      ;; rides beside the index is for the index as it then is.
                      ;; A census git runs, never the journal's word that
                      ;; nothing moved (Windows): that answers without one,
                      ;; and the session's first capture would write the cache.
                      ;; Its clock is read before the census looks: a journal
                      ;; armed earlier saw every change the census missed.
                      (clock (journal-clock))
                      (settled (and tree
                                    (progn (setf (quiet-trees store) nil)
                                           (capture-workspace-tree store))))
                      (digest (and (equal settled tree) (index-digest store)))
                      (tip (anchor-tip-cache store)))
                 (when (and tree digest)
                   (with-output-file (out (launch-tree-file store))
                     (format out "~a ~a ~a ~a~%" digest tree
                             (if (equal (cdr tip) tree) (car tip) "-")
                             (or clock "-"))))
                 tree)))
        (setf *held-launch-claims* (remove claim *held-launch-claims* :test #'equal))
        (ignore-errors (delete-file claim))))))

(defun prewarm-workspace (directory)
  "Start the launch walk of DIRECTORY on a thread of its own. Never signals."
  (with-handlers ((error () nil))
    (spawn "workspace-launch-walk"
      (with-handlers ((error () nil))
        (walk-at-launch directory)))))

(defun git-index-file-p (path)
  "True when PATH begins with DIRC, the signature git writes at the head of
every index file."
  (with-handlers ((error () nil))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (let ((head (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
        (and (= 4 (read-sequence head stream))
             (equalp head #(68 73 82 67)))))))

(defun newest-session-index (git-dir)
  "The session index in GIT-DIR written last, or NIL: a file named
index-<session-id> that git wrote, never the lock beside one."
  (let ((newest nil) (written -1))
    (dolist (path (with-handlers ((error () nil))
                    (uiop:directory-files (uiop:ensure-directory-pathname git-dir)))
                  newest)
      (let ((name (file-namestring path))
            (date (with-handlers ((error () nil)) (file-write-date path))))
        (when (and (uiop:string-prefix-p "index-" name)
                   (not (uiop:string-suffix-p name ".lock"))
                   date
                   (> date written)
                   (git-index-file-p path))
          (setf newest path written date))))))

(defun seed-session-index (store)
  "Start the index of STORE's session, when it has none yet, as a copy of the
newest index another session of the workspace left. => that index, or NIL."
  ;; Every index here stats the one work tree, so any is a valid start: `git
  ;; add -A' then rehashes only the paths whose stat moved since it was
  ;; written, where an empty index hashes every file again -- the wait a new
  ;; session (/clear opens one) paid in a large folder before its first
  ;; request. The copy is written whole under a name of its own and renamed
  ;; into place, so no reader sees half an index.
  (let ((index (snapshot-store-index-path store)))
    (unless (probe-file index)
      (when-let (source (newest-session-index (snapshot-store-git-dir store)))
        (let ((seed (concatenate 'string (directory-namestring index) "seed-"
                                 (subseq (file-namestring index) (length "index-")))))
          (with-handlers ((error ()
                            (ignore-errors (delete-file seed))
                            nil))
            (uiop:copy-file source seed)
            (rename-file seed index)
            (adopt-launch-tree store source)
            source))))))

(defun adopt-launch-tree (store source)
  "Take the tree a walk wrote beside SOURCE, the index STORE's was copied
from, as the answer for STORE's index while it holds the same bytes; and its
anchor as the tip, when this process knows none. Where this process armed
the workspace's journal before that walk settled and nothing has moved since
(ARM-WORKSPACE-JOURNAL), the tree is the workspace's as it stands: the
session's first capture answers it with no git at all."
  ;; The copy's digest is the source's: the tree answers for it until a
  ;; staging moves it (CAPTURE-WORKSPACE-TREE), with no WRITE-TREE and no
  ;; anchor resolve between the session's census and its first capture.
  (with-handlers ((error () nil))
    (let ((fields (uiop:split-string (string-trim '(#\Newline #\Return #\Space)
                                                  (read-text (concatenate 'string (namestring source) ".tree")))
                                     :separator " "))
          (digest (index-digest store)))
      (destructuring-bind (written tree &optional commit settled) fields
        (when (and digest (equal written digest) (tree-sha-p tree))
          (setf (index-trees store) (cons digest tree))
          (when (and commit (tree-sha-p commit) (null (anchor-tip-cache store)))
            (setf (anchor-tip-cache store) (cons commit tree)))
          ;; Armed before the walk's census looked, and quiet since: every
          ;; change after the arming moved the epoch, and every one before it
          ;; is in what the census saw.
          (let ((armed (armed-workspace-journal (snapshot-store-workspace store)))
                (settled (and settled (parse-integer settled :junk-allowed t))))
            (when (and armed settled (< (cdr armed) settled))
              (let ((epoch (workspace-journal-epoch (snapshot-store-workspace store))))
                (when (and epoch (eql epoch (car armed)))
                  (setf (quiet-trees store) (list* epoch digest tree)))))))))))

(define-side-table opened-snapshot-stores (:test #'equal :synchronized t)
  "(INDEX-PATH . WORKSPACE) -> the store OPEN-SNAPSHOT-STORE opened there in
this process: what opening it set up -- the store's init, its attributes and
excludes, the session's seeded index -- holds until the store is gone, so a
later open of it looks for the store and its index's lock alone.")

(defun drop-opened-snapshot-stores ()
  "Forget every opened store and every decided workspace: a dumped image
holds none of this process's folders."
  (clrhash *opened-snapshot-stores*)
  (clrhash *workspace-standings*))

(pushnew 'drop-opened-snapshot-stores sb-ext:*save-hooks*)

(defun clear-stale-index-lock (store)
  "Delete STORE's index lock when it is older than RUN-SNAPSHOT-GIT's
walltime bound: a process that died mid-walk left it; a younger one may be
a capture."
  (let* ((lock (concatenate 'string (snapshot-store-index-path store) ".lock"))
         (age (and (path-kind lock)
                   (ignore-errors (- (get-universal-time) (file-write-date lock))))))
    (when (and (integerp age) (> age +snapshot-git-timeout-seconds+))
      (ignore-errors (delete-file lock)))))

(defun open-snapshot-store (root git-dir session-id)
  "The store over the workspace ROOT kept in GIT-DIR, with SESSION-ID's index,
initializing GIT-DIR when absent: ENSURE-SNAPSHOT-STORE's last step, past its
refusals."
  (let* ((store (make-snapshot-store
                 :workspace (namestring root)
                 :git-dir (string-right-trim "/" (namestring git-dir))
                 :index-path (namestring
                              (merge-pathnames (format nil "index-~a" session-id) git-dir))))
         (key (cons (snapshot-store-index-path store) (snapshot-store-workspace store)))
         (opened (opened-snapshot-stores key)))
    (when (and opened (path-kind (snapshot-store-file opened "config")))
      (clear-stale-index-lock opened)
      (return-from open-snapshot-store (values opened nil)))
    ;; A session with no index yet seeds it from the newest of the store's,
    ;; and a launch walk of the folder still running is writing that one.
    (unless (or *launch-walk-p* (probe-file (snapshot-store-index-path store)))
      (await-launch-walk git-dir))
    (clear-stale-index-lock store)
    (bt2:with-lock-held (*store-init-lock*)
      (unless (probe-file (snapshot-store-file store "config"))
        (ensure-directories-exist git-dir)
        ;; git refuses init under GIT_WORK_TREE. The probes init makes --
        ;; ignorecase, filemode -- stand in the config it writes; the store's
        ;; own keys follow them in one more section, where the last value of
        ;; a key is the one git reads. Written, not set: five `git config'
        ;; calls were 80 ms of a new store on Windows, where each git start
        ;; costs some 16 ms (T-442).
        (run-snapshot-git store (list "init" "--bare" "--quiet") :with-work-tree nil)
        (with-open-file (stream (snapshot-store-file store "config")
                                :direction :output :if-exists :append)
          (write-string +store-config+ stream))))
    ;; An index staged before the store kept raw bytes vouches for converted
    ;; blobs, and a stat-clean file would keep its converted blob for good:
    ;; dropped, the next walk of each session stages the files again.
    (when (write-store-attributes store)
      (drop-session-indexes store))
    (seed-session-index store)
    (write-store-exclude store)
    ;; Ignore rules do not untrack storage captured by an earlier version.
    (when-let (storage (snapshot-storage-path store))
      (bt2:with-lock-held ((capture-lock store))
        (run-snapshot-git store (list "--literal-pathspecs" "rm" "-r" "-f" "--cached" "--quiet"
                                      "--ignore-unmatch" "--" storage))))
    (setf (opened-snapshot-stores key) store)
    (values store nil)))

;;; --- capture and anchor --------------------------------------------------

;;; Two walks of one index at once end in index.lock: a deferred boundary
;;; capture and a detached call's completion capture take turns here.
(defvar *capture-locks* (make-hash-table :test #'equal)
  "index path -> the lock one git walk of that index holds.")

(defun capture-lock (store)
  (sb-ext:with-locked-hash-table (*capture-locks*)
    (alexandria:ensure-gethash (snapshot-store-index-path store) *capture-locks*
                               (bt2:make-lock :name "workspace-capture"))))

;;; The written-tree cache: one index content is one tree, and `git add -A'
;;; rewrites the index only when the workspace changed — so a capture whose
;;; index is byte for byte the one WRITE-TREE last answered for holds that
;;; same tree, already written and already reachable, and the second git call
;;; of the pair is pure spawn. Most tool calls read; this is the common case,
;;; not the lucky one.
;;;
;;; Keyed by what the index holds, never by how it looks to stat: git
;;; publishes an index by renaming index.lock over it, the inode the rename
;;; frees is the one the next lock file is allocated, and an edit that leaves
;;; the entry count alone leaves the size alone — three stat fields that can
;;; all agree across a real change, inside one second. An index past
;;; +SNAPSHOT-INDEX-DIGEST-CAP+ is not read at all: the digest would cost more
;;; than the call it saves, and that workspace pays both calls as before.

(defparameter +snapshot-index-digest-cap+ (* 32 1024 1024)
  "How large an index may be and still be digested: past the 200,000 paths a
walk may stage. Reading and hashing 8 MB costs some 3 ms, the git call it
stands in for 30 ms on Windows at 20,000 paths (T-442); under a cap of one
megabyte, about 12,000 paths, every capture of a larger tree paid that call.")

(define-side-table index-trees (:test #'equal :synchronized t :key snapshot-store-index-path)
  "index path -> (DIGEST . TREE-SHA): the tree WRITE-TREE answered for that
index file with exactly that content. Strings only. Live-only.")

(defun index-digest (store)
  "The digest of STORE's index file as it now stands, or NIL when it is
absent, unreadable, or larger than +SNAPSHOT-INDEX-DIGEST-CAP+."
  (when-let (octets (file-octets (snapshot-store-index-path store) +snapshot-index-digest-cap+))
    (sha256-text octets)))

(defun staging-pathspec (store)
  "The pathspec a walk of STORE stages under: the whole tree, less the home's
snapshot storage when that sits inside it; NIL when nothing is left out."
  (when-let (storage (snapshot-storage-path store))
    (list "--" "." (concatenate 'string ":(top,exclude,literal)" storage))))

(defun entry-bytes (path)
  "The size of PATH's own directory entry -- a symlink's, never its target's
-- or 0 when it cannot be read."
  ;; Never opened: a fifo in a scratch folder would block the reader.
  (or (ignore-errors
       (sb-posix:stat-size (#-win32 sb-posix:lstat #+win32 sb-posix:stat path)))
      0))

(defun census-path (line)
  "The path of one line `add --dry-run' prints -- add 'PATH', remove 'PATH' --
or NIL for a line that names none."
  ;; Between the first quote and the last: the verb is a translatable word,
  ;; the quotes are kept around the path in every translation, and git
  ;; prints the path as it is.
  (let ((open (position #\' line)) (close (position #\' line :from-end t)))
    (and open close (< open close) (subseq line (1+ open) close))))

(defun walk-census (store)
  "The paths one `add -A' of STORE would change in its index -- untracked
paths the walk's ignore rules leave in, tracked ones whose content or mode
moved, deletions -- from one `add -A --dry-run', which writes nothing, or,
with no index yet, from `ls-files --others'."
  ;; One git call: a step brackets its call with two captures, and on Windows
  ;; starting git is most of what a capture of a small tree costs (T-442).
  ;; `add --dry-run' is the staging's own walk with nothing written: Git for
  ;; Windows' fscache and the preloaded index serve its stats (19 ms at
  ;; 5,000 files, 34 ms at 20,000, where `ls-files --others --modified'
  ;; took 81 and 281 ms), and it hashes a file whose stat moved before it
  ;; names it, so a touch is not a change.
  ;; With no index yet every path is untracked, and listing them is all a
  ;; census can say: `ls-files --others' names them without hashing each,
  ;; which the staging behind it does once.
  (if (not (probe-file (snapshot-store-index-path store)))
      (split-nul (run-snapshot-git store (list* "ls-files" "-z" "--others" "--exclude-standard"
                                                (staging-pathspec store))))
      (let ((paths '()))
        (dolist (line (uiop:split-string (run-snapshot-git store (list* "add" "-A" "--dry-run" "--ignore-errors"
                                                                        (staging-pathspec store)))
                                         :separator '(#\Newline))
                      (nreverse paths))
          (when-let (path (census-path line))
            (push path paths))))))

(defun check-walk-bound (store &aux (paths 0) (bytes 0) census)
  "Signal SNAPSHOT-WALK-TOO-LARGE when one walk of STORE would stage more than
+SNAPSHOT-WALK-MAX-PATHS+ paths or +SNAPSHOT-WALK-MAX-BYTES+ bytes; else
answer how many paths it would stage, 0 when the workspace matches the index,
and the census that listed them."
  ;; What `add -A' writes a blob for: every untracked path and every tracked
  ;; one whose content moved, through the walk's own ignore rules and
  ;; exclusions. A warm index counts only what changed since its last walk; a
  ;; new one counts the whole tree. Counting stops at the first bound passed,
  ;; and a listing longer than RUN-SNAPSHOT-GIT reads is past it already.
  (flet ((too-large (amount)
           (error 'snapshot-walk-too-large
                  :detail (format nil "~a holds more than ~a to stage; a walk stages ~
                                       at most ~:d paths and ~:d bytes"
                                  (snapshot-store-workspace store) amount
                                  +snapshot-walk-max-paths+ +snapshot-walk-max-bytes+))))
    (setf census (handler-case (walk-census store)
                   (snapshot-output-too-large ()
                     (too-large (format nil "~:d bytes of path names"
                                        +snapshot-max-output-bytes+)))))
    (dolist (path census)
      (unless (zerop (length path))
        (incf paths)
        (incf bytes (entry-bytes (concatenate 'string (snapshot-store-workspace store) path)))
        (when (> paths +snapshot-walk-max-paths+)
          (too-large (format nil "~:d paths" +snapshot-walk-max-paths+)))
        (when (> bytes +snapshot-walk-max-bytes+)
          (too-large (format nil "~:d bytes" +snapshot-walk-max-bytes+)))))
    (values paths census)))

(defun stage-workspace (store)
  "Stage STORE's workspace into its session's index: one `add -A' past the
walk bound and without the home's snapshot storage, or none when the census
found nothing to stage. => :PACKED when it staged into one pack, another
true value when it staged loose objects, NIL when it staged nothing."
  ;; The census is the walk: WALK-CENSUS lists what `add -A' would change in
  ;; the index under the same ignore rules and pathspec -- untracked paths,
  ;; content and mode changes, deletions -- and more (a touched file), never
  ;; less: a racy entry is compared by content. An empty census therefore
  ;; leaves the index's tree as it stands, and the walk `add -A' would make
  ;; is skipped: an unchanged workspace -- most tool calls, and every bracket
  ;; of a call that wrote nothing -- costs one git spawn, not two or three.
  ;; That spawn is the cost a step pays on Windows, where starting git and
  ;; stat'ing a tree are an order slower than on Linux (T-442). With no index
  ;; yet there is no tree to keep: the first walk always stages.
  ;; --ignore-errors: one unreadable path does not void the walk; a file the
  ;; process may not read was never going to be restored by it either.
  ;;
  ;; A staging of more than +SNAPSHOT-PACKED-STAGING-PATHS+ paths -- a
  ;; workspace's first walk, a checkout, an unpacked archive -- streams its
  ;; blobs into one pack instead of a loose file each: the first walk of
  ;; 5,000 files took 15.9 s on Windows as loose objects and 0.6 s as one
  ;; pack, the same tree (T-442). The threshold rides `add' alone: a diff
  ;; reading it would take every file past it for binary. The pack is
  ;; deflated at level 1, the level git gives a loose object: 540 files,
  ;; 23 MB, took 0.70 s at the default and 0.50 s at 1 on Windows, the pack
  ;; 5% larger.
  (multiple-value-bind (paths census) (check-walk-bound store)
    (let ((fresh (not (probe-file (snapshot-store-index-path store)))))
      (when (or (plusp paths) fresh)
        (let ((packed (> paths +snapshot-packed-staging-paths+)))
          (unless (and packed fresh (stage-in-parallel store census))
            (run-snapshot-git store (list* "add" "-A" "--ignore-errors" (staging-pathspec store))
                              :config (when packed
                                        '("core.bigFileThreshold=1" "pack.compression=1"))))
          (if packed :packed t))))))

;;; --- a first walk in parallel ---------------------------------------------
;;; One `add' reads, hashes and deflates every file of a first walk on one
;;; core: 520 ms for 540 files on Windows, where opening a file costs what
;;; the file system filter makes it cost (T-442). The census already lists
;;; every path, so the walk is shared out: each part an `add' of its own into
;;; an index of its own, the parts' entries then written as one index here.
;;; An entry is the part's own -- stat data, mode, blob -- and one the part
;;; may have stat'ed in the second its index was written is smudged, the
;;; size git compares zeroed, as git itself smudges an entry its index could
;;; not tell from a later write: the merged index is written later than any
;;; part's, and would otherwise vouch for it.

(defparameter +snapshot-staging-parts+ 4
  "The most `add' processes one first walk is shared out to.")

(defparameter +snapshot-paths-per-part+ 128
  "The fewest paths a part of a first walk is given: fewer, and starting
another git costs more than it shares.")

(sb-alien:define-alien-routine ("EVP_sha1" %evp-sha1) sb-sys:system-area-pointer)

(defun sha1-octets (octets)
  "The SHA-1 of OCTETS, 20 bytes: the checksum an index file ends with."
  (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (octets digest)
      (unless (= 1 (%evp-digest (sb-sys:vector-sap octets) (length octets)
                                (sb-sys:vector-sap digest) (sb-sys:int-sap 0)
                                (%evp-sha1) (sb-sys:int-sap 0)))
        (error "OpenSSL could not digest ~d bytes" (length octets))))
    digest))

(defun octets-u32 (octets at)
  "The big-endian 32-bit word of OCTETS at AT."
  (logior (ash (aref octets at) 24) (ash (aref octets (+ at 1)) 16)
          (ash (aref octets (+ at 2)) 8) (aref octets (+ at 3))))

(defun index-entries (path)
  "The entries of the version 2 index at PATH, each its own octets -- stat
data, mode, object, flags, name and padding -- in the index's order; NIL when
the file is no version 2 index."
  (let ((octets (file-octets path)))
    (when (and octets (> (length octets) 32)
               (equalp (subseq octets 0 4) #(68 73 82 67))
               (= 2 (octets-u32 octets 4)))
      (loop with at = 12
            repeat (octets-u32 octets 8)
            for name-length = (logand #xFFF (logior (ash (aref octets (+ at 60)) 8)
                                                   (aref octets (+ at 61))))
            for name-end = (if (< name-length #xFFF)
                               (+ at 62 name-length)
                               (position 0 octets :start (+ at 62)))
            ;; 62 bytes and the name, NUL-padded to a multiple of 8, at least one NUL
            for size = (* 8 (ceiling (1+ (- name-end at)) 8))
            collect (subseq octets at (+ at size))
            do (incf at size)))))

(defun entry-name (entry)
  "The name octets of one index ENTRY."
  (subseq entry 62 (position 0 entry :start 62)))

(defun octets< (a b)
  "Whether octet vector A sorts before B, byte by byte, as git sorts names."
  (let ((at (mismatch a b)))
    (and at (or (= at (length a))
                (and (< at (length b)) (< (aref a at) (aref b at)))))))

(defun write-merged-index (path parts)
  "Write at PATH one version 2 index of the entries of the index files PARTS,
by name, entries racy against their part smudged. => true when written."
  (let ((entries '()))
    (dolist (part parts)
      (let ((written (- (file-write-date part) +unix-epoch+))
            (own (index-entries part)))
        (unless own (return-from write-merged-index nil))
        (dolist (entry own)
          ;; mtime seconds at 8; the size git compares at 36
          (when (>= (octets-u32 entry 8) written)
            (fill entry 0 :start 36 :end 40))
          (push (cons (entry-name entry) entry) entries))))
    (setf entries (sort entries #'octets< :key #'car))
    (let* ((size (+ 12 (reduce #'+ entries :key (lambda (entry) (length (cdr entry)))) 20))
           (octets (make-array size :element-type '(unsigned-byte 8)))
           (at 12))
      (replace octets #(68 73 82 67 0 0 0 2))
      (loop for shift from 24 downto 0 by 8 for i from 8
            do (setf (aref octets i) (ldb (byte 8 shift) (length entries))))
      (dolist (entry entries)
        (replace octets (cdr entry) :start1 at)
        (incf at (length (cdr entry))))
      (replace octets (sha1-octets (subseq octets 0 at)) :start1 at)
      ;; No dot added: RENAME-FILE would carry a type the target lacks onto it.
      (let ((staged (concatenate 'string path "-merged")))
        (with-open-file (out staged :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
          (write-sequence octets out))
        (rename-file staged path))
      t)))

(defun stage-in-parallel (store census)
  "Stage the paths of CENSUS, a first walk's, into STORE's new index through
as many `add' processes as +SNAPSHOT-STAGING-PARTS+ allows, each a share of
them into an index of its own, then those indexes as STORE's. => true when
staged; NIL, and nothing left behind, when any part failed."
  (let* ((index (snapshot-store-index-path store))
         (count (min +snapshot-staging-parts+
                     (max 1 (floor (length census) +snapshot-paths-per-part+))))
         (shares (make-array count :initial-element '()))
         (parts (loop for k below count collect (format nil "~a.part~d" index k)))
         (failed nil))
    (when (< count 2)
      (return-from stage-in-parallel nil))
    (loop for path in census for k from 0
          do (push path (aref shares (mod k count))))
    (unwind-protect
         (let ((threads
                 (loop for part in parts for k from 0
                       collect (let ((part part)
                                     (listing (concatenate 'string part ".paths"))
                                     (share (aref shares k)))
                                 (with-open-file (out listing :direction :output
                                                              :external-format :utf-8
                                                              :if-exists :supersede)
                                   (dolist (path share)
                                     (write-string path out) (write-char (code-char 0) out)))
                                 (bt2:make-thread
                                  (lambda ()
                                    (handler-case
                                        (run-snapshot-git
                                         (make-snapshot-store
                                          :workspace (snapshot-store-workspace store)
                                          :git-dir (snapshot-store-git-dir store)
                                          :index-path part)
                                         (list "--literal-pathspecs" "add" "--ignore-errors"
                                               (concatenate 'string "--pathspec-from-file=" listing)
                                               "--pathspec-file-nul")
                                         :config '("core.bigFileThreshold=1" "pack.compression=1"
                                                   "index.version=2"))
                                      (error () (setf failed t))))
                                  :name "snapshot-staging-part")))))
           (mapc #'bt2:join-thread threads)
           (and (not failed)
                (every #'probe-file parts)
                (with-handlers ((error () nil))
                  (write-merged-index index parts))))
      (dolist (part parts)
        (ignore-errors (delete-file part))
        (ignore-errors (delete-file (concatenate 'string part ".paths")))))))

(defun fast-import-path (path)
  "PATH as a `fast-import' file command names it: as it is, or C-quoted when
it holds a newline or opens with a double quote."
  (if (or (find #\Newline path) (find #\Return path)
          (and (plusp (length path)) (char= (char path 0) #\")))
      (with-output-to-string (out)
        (write-char #\" out)
        (loop for char across path
              do (case char
                   (#\" (write-string "\\\"" out))
                   (#\\ (write-string "\\\\" out))
                   (#\Newline (write-string "\\n" out))
                   (#\Return (write-string "\\r" out))
                   (t (write-char char out))))
        (write-char #\" out))
      path))

(defun anchor-ref-present-p (store)
  "True when STORE's git directory holds the anchor ref, loose or packed:
read from the files, no git started."
  (or (probe-file (snapshot-store-file store +snapshot-anchor-ref+))
      (with-handlers ((error () nil))
        (let ((packed (snapshot-store-file store "packed-refs")))
          (and (probe-file packed)
               (search (concatenate 'string " " +snapshot-anchor-ref+ (string #\Newline))
                       (read-text packed)))))))

(defun write-packed-tree (store message)
  "Write the trees of STORE's index and an anchor commit of its root under
MESSAGE into one pack, through one `fast-import'. => the root tree's sha, or
NIL when the index holds what this writer leaves to WRITE-TREE or the import
failed."
  ;; WRITE-TREE writes each tree as a loose object, and on Windows a loose
  ;; object costs some 3.5 ms (its temporary file, the rename, the scan the
  ;; file system filter gives every new file): the 54 trees of a 540-file
  ;; workspace's first walk took 200 ms there, the anchor's COMMIT-TREE and
  ;; UPDATE-REF 75 ms more, where one import of the index's listing writes
  ;; trees and commit into one pack and moves the ref in 50 ms (T-442).
  ;; unpackLimit=0 keeps the pack: under 100 objects the import would
  ;; explode it into loose objects again. The ref's tip as the import reads
  ;; it (FROM REF^0, which an absent ref has none of) is the commit's parent,
  ;; and an import that finds the ref moved past that tip by its end leaves
  ;; it -- the compare-and-swap ANCHOR-TREE makes -- and fails, which the
  ;; caller answers by WRITE-TREE over trees this import already packed.
  ;; DELETEALL: the tree is the index's alone, never the tip's with the index
  ;; laid over it.
  (with-handlers ((snapshot-git-failed () (forget-anchor-tip-cache store) nil))
    (let ((listing (run-snapshot-git store (list "ls-files" "-s" "-z"))))
      (when (find (code-char #xFFFD) listing)
        ;; A path git printed in bytes this image could not decode: the
        ;; import would name another path.
        (return-from write-packed-tree nil))
      (let ((input
              (with-output-to-string (out)
                (format out "commit ~a~%mark :1~%committer nodecode <nodecode@localhost> ~d +0000~%data ~d~%~a~%~@[from ~a^0~%~]deleteall~%"
                        +snapshot-anchor-ref+ (unix-now)
                        (length (sb-ext:string-to-octets message :external-format :utf-8))
                        message (and (anchor-ref-present-p store) +snapshot-anchor-ref+))
                (dolist (record (uiop:split-string listing :separator (string (code-char 0))))
                  (unless (zerop (length record))
                    (let* ((tab (position #\Tab record))
                           (head (and tab (uiop:split-string (subseq record 0 tab) :separator " "))))
                      ;; MODE SHA STAGE: a conflicted entry (stage past 0) has
                      ;; no tree of its own to write.
                      (unless (and head (= 3 (length head)) (equal (third head) "0"))
                        (return-from write-packed-tree nil))
                      (format out "M ~a ~a ~a~%" (first head) (second head)
                              (fast-import-path (subseq record (1+ tab)))))))
                (format out "~%ls :1 \"\"~%get-mark :1~%done~%"))))
        (let* ((answer (lines (run-snapshot-git store (list "fast-import" "--quiet" "--done"
                                                            "--cat-blob-fd=1")
                                                :config '("fastimport.unpackLimit=0")
                                                :input input)))
               (root (first answer))
               (tab (and root (position #\Tab root)))
               (fields (and tab (uiop:split-string (subseq root 0 tab) :separator " ")))
               (tree (third fields))
               (commit (second answer)))
          (when (and (equal (second fields) "tree") (tree-sha-p tree) (tree-sha-p commit))
            (setf (anchor-tip-cache store) (cons commit tree))
            tree))))))

(define-side-table quiet-trees (:test #'equal :synchronized t :key snapshot-store-index-path)
  "index path -> (EPOCH DIGEST . TREE-SHA): the tree the walk of that index
read when its workspace's journal stood at EPOCH, the index left at DIGEST.
Strings and numbers only. Live-only.")

(defun journal-ignored-paths (store)
  "What STORE's workspace journal leaves out beside .git: the home's snapshot
storage when it sits inside the workspace, as a relative Windows path."
  (when-let (storage (snapshot-storage-path store))
    (unless (equal storage ".")
      (list (substitute #\\ #\/ (string-right-trim "/" storage))))))

(defvar *walk-anchor-message* "workspace walk"
  "The message a walk that packs its trees anchors them under: the label of
the CAPTURE-ANCHORED-TREE it runs for.")

(defun capture-workspace-tree (store)
  "Stage the whole workspace and return its tree sha. A staging that packs its
objects anchors the tree as it writes it, under *WALK-ANCHOR-MESSAGE*."
  ;; An unchanged workspace
  ;; yields the previous sha, so no-op captures allocate nothing — and an index
  ;; the staging left untouched answers from the written-tree cache, without the
  ;; second git call. One unreadable path does not void the tree: ADD
  ;; --IGNORE-ERRORS stages what it can, and a file the process may not read was
  ;; never going to be restored by it either. One walk of an index at a time
  ;; (CAPTURE-LOCK): a deferred capture still reading the tree and a completion
  ;; capture arriving behind it wait, never collide on index.lock. A walk past
  ;; its bound signals SNAPSHOT-WALK-TOO-LARGE before anything is staged.
  ;;
  ;; Where the workspace keeps a journal (WORKSPACE-JOURNAL-EPOCH, Windows) a
  ;; capture that finds it unmoved since this index's last walk -- the same
  ;; epoch, the index as that walk left it -- answers that walk's tree and
  ;; starts no git. The epoch is read before the walk, so a write landing
  ;; while git reads the tree moves it for the next capture.
  (bt2:with-lock-held ((capture-lock store))
    (let* ((epoch (workspace-journal-epoch (snapshot-store-workspace store)
                                           :ignored (lambda () (journal-ignored-paths store))))
           (quiet (quiet-trees store)))
      (if (and epoch quiet (eql epoch (first quiet))
               (equal (second quiet) (index-digest store)))
          (cddr quiet)
          (let ((staged (stage-workspace store)))
            (let* ((digest (index-digest store))
                   (entry (and digest (index-trees store)))
                   (tree (or (and (equal (car entry) digest) (cdr entry))
                             ;; A staging past +SNAPSHOT-PACKED-STAGING-PATHS+
                             ;; packs its trees too, and anchors them: the
                             ;; import leaves the index without a cache tree,
                             ;; so DIGEST, the index as staged, answers for it.
                             (when-let (tree (and digest (eq staged :packed)
                                                  (write-packed-tree store *walk-anchor-message*)))
                               (setf (index-trees store) (cons digest tree))
                               tree)
                             (let ((tree (validate-tree-sha (git-line store (list "write-tree")))))
                               ;; Read the digest again: WRITE-TREE records the tree it
                               ;; wrote in the index as its cache tree, so the content
                               ;; that answers for this tree is the one the index
                               ;; carries after the call, never the one it carried
                               ;; going in.
                               (when-let (settled (and digest (index-digest store)))
                                 (setf (index-trees store) (cons settled tree)))
                               tree))))
              (when-let (settled (and epoch (index-digest store)))
                (setf (quiet-trees store) (list* epoch settled tree)))
              tree))))))


;;; The anchor tip cache: the ref only ever moves through ANCHOR-TREE in
;;; this process, so its (commit . tree) tip is worth remembering per
;;; git-dir — a no-op boundary then costs zero rev-parse spawns, and the
;;; spawn (a fork of a large image) is the dominant cost of a capture.
;;; Cross-process safety does not rest on the cache being right: the
;;; update-ref below is a compare-and-swap against the cached tip, so a
;;; stale cache loses the swap, re-resolves, and retries instead of
;;; silently overwriting a tip some other process advanced (strictly
;;; stronger than the unguarded update-ref this replaces).

;;; Strings only — nothing here for SAVE-LISP-AND-DIE to trip on.
(define-side-table anchor-tip-cache (:test #'equal :synchronized t :key snapshot-store-git-dir)
  "GIT-DIR -> (COMMIT . TREE) last known anchor tip; (NIL . NIL) when the
ref did not exist at resolve time.")

(defun anchor-tree (store tree-sha message)
  "Chain TREE-SHA onto the anchor ref so gc can never reach it."
  ;; Mandatory after every capture that must stay restorable. When the tree is
  ;; already the chain tip the commit is skipped — a no-op boundary allocates
  ;; zero objects and, with a warm tip cache, spawns no git at all.
  (validate-tree-sha tree-sha)
  (dotimes (attempt 2
            (error 'snapshot-git-failed
                   :arguments (list "update-ref" +snapshot-anchor-ref+)
                   :exit-status 1 :stderr ""
                   :detail "anchor update-ref lost the compare-and-swap twice"))
    (declare (ignorable attempt))
    (let ((tip (or (anchor-tip-cache store)
                   (let* ((shas (handler-case
                                    (lines (run-snapshot-git
                                            store (list "rev-parse" +snapshot-anchor-ref+
                                                        (format nil "~a^{tree}"
                                                                +snapshot-anchor-ref+))))
                                  (snapshot-git-failed () nil)))
                          (tip (if (and (= (length shas) 2) (every #'tree-sha-p shas))
                                   (cons (first shas) (second shas))
                                   (cons nil nil))))
                     (setf (anchor-tip-cache store) tip)))))
      (when (equal tree-sha (cdr tip))
        (return (car tip)))
      (let ((commit (git-line store (nconc (list "commit-tree" tree-sha)
                                           (when (car tip) (list "-p" (car tip)))
                                           (list "-m" message)))))
        (with-handlers ((snapshot-git-failed ()
                          ;; Swap lost: another writer advanced the tip. Re-resolve and
                          ;; retry once so the chain extends their tip instead of forking.
                          (forget-anchor-tip-cache store)))
          ;; Empty old-value = "the ref must not exist yet".
          (run-snapshot-git store (list "update-ref" +snapshot-anchor-ref+
                                        commit (or (car tip) "")))
          (setf (anchor-tip-cache store) (cons commit tree-sha))
          (return commit))))))

(defun capture-anchored-tree (store message)
  "Capture and anchor STORE's tree under MESSAGE."
  ;; => (values TREE-SHA NIL), or
  ;; (values NIL REASON): a remembered CAPTURE-REFUSAL, or a fresh failure, remembered.
  (if-let (refusal (capture-refusal store))
    (values nil refusal)
    (handler-case
        (let ((tree-sha (let ((*walk-anchor-message* message))
                          (capture-workspace-tree store))))
          (anchor-tree store tree-sha message)
          (values tree-sha nil))
      (snapshot-walk-too-large (condition)
        ;; Remembered, so the boundaries behind it inside the window neither
        ;; count nor log again; then one line for the operator. The line is
        ;; written, never signalled: a deferred walk's thread is joined, and
        ;; a thread that signalled a warning joins as an abnormal exit.
        (setf (capture-refusals store) (cons :workspace-too-large (get-universal-time)))
        (ignore-errors
         (format *error-output* "~&WARNING: workspace capture skipped: ~a~%" condition)
         (force-output *error-output*))
        (values nil :workspace-too-large))
      (error ()
        (setf (capture-refusals store) (cons :capture-failed (get-universal-time)))
        (values nil :capture-failed)))))

;;; --- paths in a tree ------------------------------------------------------

(defparameter +pathspec-characters+ (* 64 1024)
  "How much pathspec text one `ls-tree' command line carries: far under any
host's argument limit.")

(defun pathspec-chunks (paths &aux (chunks '()) (chunk '()) (size 0))
  "PATHS in runs of at most +PATHSPEC-CHARACTERS+ characters, in order."
  (dolist (path paths)
    (when (and chunk (> (+ size (length path) 1) +pathspec-characters+))
      (push (nreverse chunk) chunks)
      (setf chunk '() size 0))
    (push path chunk)
    (incf size (1+ (length path))))
  (when chunk (push (nreverse chunk) chunks))
  (nreverse chunks))

(defun path-set (rows &aux (set (make-hash-table :test #'equal)))
  "The :PATH of each of ROWS -- plists -- as an EQUAL hash set."
  (dolist (row rows set)
    (setf (gethash (getf row :path) set) t)))

(defun tree-files-beneath (store tree paths)
  "Every file TREE records at or beneath one of PATHS, as (PATH MODE SHA)
lists in tree order; a path the tree does not hold adds nothing."
  ;; `ls-tree -r' of the paths as pathspecs, in runs a command line carries
  ;; (PATHSPEC-CHUNKS), --literal-pathspecs so a `*' or `[' in a name is not a
  ;; glob. A pathspec matches whole steps: `d' lists d and d/..., never dx.
  ;; Records are <mode> SP <type> SP <sha> TAB <path>.
  (loop for chunk in (pathspec-chunks paths)
        nconc (loop for record in (split-nul (run-snapshot-git
                                              store (list* "--literal-pathspecs" "ls-tree" "-r"
                                                           "-z" tree "--" chunk)))
                    for tab = (position #\Tab record)
                    for fields = (and tab (uiop:split-string (subseq record 0 tab)
                                                             :separator " "))
                    when (= (length fields) 3)
                      collect (list (subseq record (1+ tab)) (first fields) (third fields)))))

;;; --- diff, hashes, restore -----------------------------------------------

(defun split-nul (text)
  "TEXT's NUL-separated fields, an unterminated tail kept, trailing empty ones dropped."
  (ppcre:split "\\x00" text))

(defun parse-name-status-z (records)
  "STATUS PATH pairs (the NUL-split RECORDS) => list of (:path P :status K) plists."
  ;; A ⇒ :added (present on disk, absent in the tree), D ⇒ :deleted, M/T ⇒
  ;; :modified; other statuses are skipped (renames are disabled).
  (loop with fields = (remove "" records :test #'string=)
        for (status path) on fields by #'cddr
        while path
        for kind = (case (char status 0)
                     (#\A :added) (#\D :deleted) ((#\M #\T) :modified))
        when kind
          collect (list :path path :status kind)))

(defun staged-changed-paths (store tree-sha)
  "Already-staged-index-vs-TREE-SHA differences."
  ;; The one diff-index seam: CHANGED-PATHS stages then routes here; read-side
  ;; batch callers (the checkpoint files projection) stage once and call this
  ;; per tree.
  (parse-name-status-z
   (split-nul (run-snapshot-git store (list "diff-index" "--name-status" "--no-renames"
                                            "-z" tree-sha)))))

(defun staged-tree-diffs-batched (store trees)
  "Every tree in TREES diffed against STORE's worktree in ONE child process:
`git add -A' then one `git diff-index' per tree inside a single sh."
  ;; => an EQUAL hash of tree sha -> (:PATH :STATUS) plists (the
  ;; STAGED-CHANGED-PATHS answer, through the same parser), or NIL when the
  ;; batch cannot be built, run or framed — every caller keeps the per-tree path
  ;; as its fallback.
  ;;
  ;; Why a shell at all: a git plumbing call from a serving image costs ~40
  ;; ms, of which ~20 ms is forking the image itself and the rest the
  ;; env/timeout/git exec chain — a no-op `rev-parse' costs as much as a real
  ;; diff. The checkpoint listing needs one staging plus one diff per distinct
  ;; undo-target tree, so its wall time is a spawn count, not git work.
  ;; Batching makes that count 1 for any number of trees.
  ;;
  ;; Framing is a fresh random 160-bit token emitted between diffs, so no path
  ;; can be mistaken for a separator, and each diff's exit status rides in
  ;; after its token; a tree whose diff failed lands :UNAVAILABLE, the read
  ;; side's own handler-case degradation. Only hex-validated shas reach the
  ;; script text.
  (let ((marker (format nil "cpdiff~40,'0x" (random (expt 2 160))))
        (shas (remove-duplicates (remove nil trees) :test #'equal)))
    (when (and shas (equal "git" *git-program*) (every #'tree-sha-p shas))
      (let* ((script
               (format nil "git add -A || exit 9~:{~%git diff-index --name-status --no-renames ~
                            -z ~a; printf '~a\\000%s\\000' \"$?\"~}"
                       (mapcar (lambda (sha) (list sha marker)) shas)))
             (output
               (with-handlers ((error () nil))
                 ;; The batch stages as well: past the walk bound it answers
                 ;; nothing, and the caller's own staging refuses the same way.
                 (check-walk-bound store)
                 (bind (((out _ status)
                         (run-bounded (shell-argv script)
                                      :seconds +snapshot-git-timeout-seconds+
                                      :environment (snapshot-git-environment store)
                                      :directory (snapshot-store-workspace store))))
                   ;; A staging failure (exit 9), a killed batch or a
                   ;; refusal-sized payload answers nothing: the caller
                   ;; re-derives per tree.
                   (and (integerp status) (zerop status)
                        (<= (length out) +snapshot-max-output-bytes+)
                        out)))))
        (when output
          (let ((table (make-hash-table :test #'equal))
                (fields '())
                (pending shas)
                (expect-status nil))
            (dolist (field (split-nul output))
              (cond (expect-status
                     (setf (gethash (pop pending) table)
                           (if (equal field "0")
                               (parse-name-status-z (nreverse fields))
                               :unavailable)
                           fields '()
                           expect-status nil))
                    ((equal field marker) (setf expect-status t))
                    (t (push field fields))))
            ;; Every listed tree must have been framed, or the answer is not
            ;; trustworthy and the caller falls back.
            (and (null pending) (null expect-status) table)))))))

(defun changed-paths (store tree-sha)
  "Worktree-vs-TREE-SHA differences, freshly staged."
  (stage-workspace store)
  (staged-changed-paths store tree-sha))

;;; --- workspace diff documents ---------------------------------------------
;;; The machine-readable delta between two captured trees: per-file numstat
;;; plus complete git-owned unified patches. The capture facts' tree shas
;;; remain the durable authority; disclosure never sheds large/later patches.

(defun workspace-diff-document (store pre-tree post-tree)
  "The complete JSON diff document between two captured trees, or NIL when
the trees are equal, nothing changed, or the derivation degrades."
  (when store
    (diff-document (lambda (args) (run-snapshot-git store args :with-work-tree nil))
                   pre-tree post-tree)))

(defun diff-document (git pre-tree post-tree)
  "The complete JSON diff document between two trees -- or commits -- that
GIT, a function of git's argument list answering its output and signalling
when git fails, can read; NIL when they are equal, nothing changed, or the
derivation degrades."
  ;; Never signals. Shape: {"files": [{"path", "added", "removed", "binary"?,
  ;; "patch"?} ...]}. Binary files have no text patch; a patch/numstat listing
  ;; mismatch drops every patch rather than mispairing one.
  ;; GIT is the one strategy input: a session's
  ;; shadow store (WORKSPACE-DIFF-DOCUMENT) and a cell's own repository
  ;; (generation.lisp FOLDER-DIFF) read through the same derivation.
  (with-handlers ((error () nil))
    (when (and pre-tree post-tree
               (not (equal pre-tree post-tree)))
      (let* ((stats
               ;; per-file numstat in tree order: -z keeps paths unquoted, a binary counts "-"
               (loop for line in (remove "" (split-nul
                                             (funcall git (list "diff-tree" "-r" "--no-renames"
                                                                "-z" "--numstat"
                                                                pre-tree post-tree)))
                                         :test #'string=)
                     for (added removed path) = (ppcre:split "\\t" line :limit 3)
                     when path
                       collect (list :path path
                                     :added (or (parse-integer added :junk-allowed t) 0)
                                     :removed (or (parse-integer removed :junk-allowed t) 0)
                                     :binary (string= added "-"))))
             (total (length stats)))
        (when stats
          (let ((patches (handler-case
                             ;; a "diff --git" section per file, numstat order; 4 context lines, pi-mono's policy
                             (let* ((output (funcall git (list "diff-tree" "-r" "--no-renames"
                                                               "--unified=4" "-p" pre-tree post-tree)))
                                    (sections (ppcre:all-matches-as-strings
                                               "(?s)diff --git .*?(?=diff --git |\\z)"
                                               output)))
                               (and (= (length sections) total) sections))
                           (error () nil)))
                (files '()))
            (loop for entry in stats
                  for patch in (or patches (make-list total))
                  do (let* ((binary (getf entry :binary))
                            (attach (and (not binary) patch)))
                       (push (json-object
                               (:getf entry "path" "added" "removed")
                               :when binary "binary" t
                               :opt "patch" attach)
                             files)))
            (json-object "files" (coerce (nreverse files) 'vector))))))))

(defmacro with-session-snapshot-store ((store session-id) &body body)
  "Run BODY with STORE bound to SESSION-ID's open shadow store, else NIL."
  ;; The four guarantees every read-side capture path needs, stated once: the
  ;; capture kill switch gates the path (git init and ADD -A must never touch
  ;; a workspace's git state while captures are off), the session must carry a
  ;; recorded workspace, the store must open, and nothing signals past this
  ;; form — a degradation is NIL, never a condition thrown into a running
  ;; turn.
  (let ((id (gensym "SESSION-ID")))
    `(when *workspace-capture-enabled*
       (with-handlers ((error () nil))
         (let* ((,id ,session-id)
                (session (find-session ,id)))
           (when-let (,store (and session session.cwd (ensure-snapshot-store session.cwd ,id)))
             ,@body))))))

;;; --- the bracket ---------------------------------------------------------
;;; A unit's workspace_diff is what changed between two walks: one at
;;; dispatch, one when its bodies return. The window is the unit's own
;;; execution and nothing wider — a turn's provider round runs for seconds
;;; or minutes, and without the bracket's own pre-walk a bracket opened on
;;; the previous unit's post-tree would charge every write another agent or
;;; an editor made in that round to the next call. The checkpoint that
;;; follows anchors the closing walk's tree instead of walking again. A
;;; write another process makes while the body runs still lands in the
;;; window: the bracket attributes by time, not by writer.

(defun open-workspace-bracket (turn &key label)
  "Open a mutation window of TURN: the pre side of a bracket, a walk of its
own at dispatch, anchored."
  ;; Gated on the capture kill switch and never signals: => (values STORE
  ;; TREE-SHA), or NIL when the capture degrades.
  (with-session-snapshot-store (store turn.session-id)
    (when-let (tree-sha (capture-anchored-tree
                         store (format nil "baseline ~a" (or label ""))))
      (values store tree-sha))))

(defun close-workspace-bracket (store pre-tree-sha)
  "Close the bracket OPEN-WORKSPACE-BRACKET opened: one walk of STORE."
  ;; => (values POST-TREE-SHA DOCUMENT), DOCUMENT the diff document from
  ;; PRE-TREE-SHA when the tree changed, else NIL. Never signals: a failed walk
  ;; answers (values NIL NIL), and the checkpoint that follows walks for itself.
  (with-handlers ((error () (values nil nil)))
    (let ((post-tree (capture-workspace-tree store)))
      (values post-tree
              (unless (equal post-tree pre-tree-sha)
                (workspace-diff-document store pre-tree-sha post-tree))))))

(defun snapshot-absolute-path (store path)
  (namestring (merge-pathnames path (snapshot-store-workspace store))))

(defun hash-path-contents (store path &aux (absolute (snapshot-absolute-path store path)))
  "Git blob hash of PATH's on-disk content, or NIL when absent."
  (when (probe-file absolute)
    (with-handlers ((snapshot-git-failed () nil))
      (let ((sha (git-line store (list "hash-object" "--" absolute))))
        (and (= (length sha) 40) sha)))))

(defun hash-in-tree (store tree-sha path)
  "Blob sha recorded for PATH in TREE-SHA, or NIL when the tree lacks it."
  (with-handlers ((snapshot-git-failed () nil))
    (third (find path (tree-files-beneath store tree-sha (list path))
                 :key #'first :test #'string=))))

(defun restore-paths (store tree-sha changes expected &aux (restored '())
                                                           (conflicts '())
                                                           (tree-read nil))
  "Apply CHANGES (from CHANGED-PATHS against TREE-SHA) strictly per path,
guarded by the parallel EXPECTED sha list."
  ;; Never a whole-tree checkout: a captured tree holds every session's
  ;; content and whole-tree apply would clobber other sessions' content. =>
  ;; (values RESTORED CONFLICTS).
  (loop for change in changes
        for guard in expected
        for path = (getf change :path)
        for status = (getf change :status)
        do (cond
             ;; A foreign write: PATH's on-disk state departs from GUARD, the
             ;; session's last recorded write. GUARD nil means the session
             ;; recorded the path ABSENT, so on-disk content is a foreign
             ;; write like any other mismatch: restore may neither overwrite
             ;; nor delete content the session never claimed. The one
             ;; no-claim state that restores is absent-and-expected-absent —
             ;; recreating into empty space (the undo-my-own-deletion shape)
             ;; clobbers nothing.
             ((not (equal (hash-path-contents store path) guard))
              (push (list :path path :reason :foreign-write) conflicts))
             ((eq status :added)
              ;; Present on disk, absent in the target tree: revert = delete.
              (with-handlers ((error ()
                                (push (list :path path :reason :not-up-to-date) conflicts)))
                (let ((absolute (snapshot-absolute-path store path)))
                  (when (probe-file absolute) (delete-file absolute))
                  (push change restored))))
             (t
              (with-handlers ((snapshot-git-failed ()
                                (push (list :path path :reason :not-up-to-date)
                                      conflicts)))
                (unless tree-read
                  (run-snapshot-git store (list "read-tree" tree-sha))
                  (setf tree-read t))
                (run-snapshot-git
                 store (list "checkout-index" "-f" "--" path))
                (push change restored)))))
  (values (nreverse restored) (nreverse conflicts)))

;;; --- durable capture fact ------------------------------------------------

(defun record-workspace-capture (session-id
                                 &key turn-id command-id causation-id
                                 boundary-kind tool-call-id tree-sha
                                 pre-tree-sha
                                 unavailable-reason anchor-position
                                 occurred-at
                                 (checkpoint-id (make-durable-id "checkpoint")))
  "Durably record one workspace capture."
  ;; The envelope turn_id column stays
  ;; NIL — a stamped turn id would extend that turn unit's end-sequence in the
  ;; history fold — so the payload carries the turn identity instead.
  ;; OCCURRED-AT, when given, is the fact's clock: the moment the capture began,
  ;; for a capture whose fact lands later than its walk.
  ;; => (values log-position checkpoint-id).
  (check-strings session-id boundary-kind)
  (values (append-session-fact session-id +kind-workspace-captured+
                               (list* :checkpoint-id checkpoint-id
                                      :boundary-kind boundary-kind
                                      (present-plist :turn-id turn-id
                                                     :tool-call-id tool-call-id
                                                     :tree-sha tree-sha
                                                     :pre-tree-sha pre-tree-sha
                                                     :unavailable-reason
                                                     (and unavailable-reason
                                                          (string-downcase unavailable-reason))
                                                     :anchor-position anchor-position))
                               :command-id command-id :causation-id causation-id
                               :occurred-at occurred-at)
          checkpoint-id))

(define-side-table capture-counts (:test #'equal :synchronized t :key snapshot-store-git-dir)
  "Git dir -> anchoring captures since the store's last repack, as this
process counts them: read from the store's capture-count once, written back
every +CAPTURE-COUNT-STRIDE+.")

(defparameter +capture-count-stride+ 8
  "How many captures pass between writes of a store's capture-count: a file
read and written at every tool call was two opens and a directory check on
its round, 0.5 ms of each on Windows (T-442); a process that dies loses at
most this many toward the next repack.")

(defun note-anchoring-capture (store)
  "Count one anchoring capture toward the repack cadence: a repack every 64,
off the provider path; a held repack lock skips this one."
  (let* ((count (sb-ext:with-locked-hash-table (*capture-counts*)
                  (setf (capture-counts store)
                        (1+ (or (capture-counts store)
                                (ignore-errors
                                 (parse-integer
                                  (read-text (snapshot-store-file store "capture-count"))
                                  :junk-allowed t))
                                0)))))
         (repack (and (>= count 64) (acquire-repack-lock store))))
    (when repack (setf (capture-counts store) 0))
    (when (or repack (and (< count 64) (zerop (mod count +capture-count-stride+))))
      (ignore-errors
        (with-output-file (stream (snapshot-store-file store "capture-count"))
          (format stream "~a~%" (if repack 0 count)))))
    (when repack
      (unwind-protect (repack-store store)
        (ignore-errors
         (delete-file (snapshot-store-file store "repack.lock")))))))

(defun checkpoint-capture-boundary (turn &key boundary-kind tool-call-id
                                         pre-tree-sha tree-sha
                                         anchor-position defer store)
  "Capture the workspace at a turn boundary and record the durable fact."
  ;; Runs entirely outside *STORE-LOCK*: git first, then the fact in its own
  ;; transaction. A checkpoint must NEVER fail the turn — every signal
  ;; degrades to a nil result or an unavailable fact.
  ;;
  ;; TREE-SHA, when given, is the tree this boundary already read — a tool
  ;; unit's post-tree, CLOSE-WORKSPACE-BRACKET's — and the boundary anchors it
  ;; and records the fact without a walk of its own: with a warm tip cache a
  ;; no-op anchor spawns no git at all.
  ;;
  ;; DEFER T takes the git walk off the round: the tree is read and anchored
  ;; on a thread of its own while the round goes on to its provider request,
  ;; and the fact lands through DEFER-TURN-FACT — before the turn's next
  ;; durable fact, on the thread that appends it — stamped with this moment,
  ;; when the capture began. The log reads as it did when the walk held the
  ;; round: the capture, then the round that followed it. Only the prompt
  ;; fact the request is sent under lands ahead of it: that fact is written
  ;; before the request goes out, and does not wait for the walk.
  ;;
  ;; => plist (:checkpoint-id ... :tree-sha sha-or-nil :unavailable kw-or-nil),
  ;; :DEFERRED when the walk was handed off, or NIL when even the fact could not
  ;; be recorded. NIL immediately when *WORKSPACE-CAPTURE-ENABLED* is bound off.
  (when *workspace-capture-enabled*
    (with-handlers ((error (condition)
                      (warn "workspace capture skipped: ~a" condition)
                      nil))
      (let* ((session-id turn.session-id)
             (workspace (let ((session (find-session session-id))) (and session session.cwd))))
        (when workspace
          (bind ((label (format nil "checkpoint ~a ~a" boundary-kind
                                turn.turn-id))
                 (captured-at (iso-now))
                 ;; A deferred boundary opens the store on its walk's
                 ;; thread: a workspace's first store is a git start or
                 ;; more, and the round need not wait for it either.
                 ;; A STORE the caller holds -- the tool step's bracket
                 ;; opened it a moment ago -- is not opened again.
                 ((store unavailable) (cond (defer (values nil nil))
                                            (store (values store nil))
                                            (t (ensure-snapshot-store workspace session-id)))))
            (flet ((record (store tree-sha unavailable)
                     (let ((checkpoint-id
                             (nth-value 1 (record-workspace-capture
                                           session-id
                                           :turn-id turn.turn-id
                                           :command-id turn.command-id
                                           :causation-id turn.started-event-id
                                           :boundary-kind boundary-kind
                                           :tool-call-id tool-call-id
                                           :tree-sha tree-sha
                                           :pre-tree-sha pre-tree-sha
                                           :unavailable-reason unavailable
                                           :anchor-position anchor-position
                                           :occurred-at (and defer captured-at)))))
                       (when (and store tree-sha)
                         (note-anchoring-capture store))
                       (list :checkpoint-id checkpoint-id :tree-sha tree-sha
                             :unavailable unavailable))))
              (cond
                (defer
                 (let ((walk (spawn "workspace-capture"
                               (with-handlers ((error () (list nil nil :capture-failed)))
                                 (multiple-value-bind (store unavailable)
                                     (ensure-snapshot-store workspace session-id)
                                   (if store
                                       (list* store (multiple-value-list
                                                     (capture-anchored-tree store label)))
                                       (list nil nil unavailable)))))))
                   (defer-turn-fact
                    turn
                    (lambda ()
                      (destructuring-bind (store tree-sha refusal)
                          (handler-case (bt2:join-thread walk)
                            (error () (list nil nil :capture-failed)))
                        (record store tree-sha refusal))))
                   :deferred))
                ((null store) (record nil nil unavailable))
                (tree-sha
                 (with-handlers ((error () (record store nil :capture-failed)))
                   (anchor-tree store tree-sha label)
                   (record store tree-sha nil)))
                (t (multiple-value-bind (tree-sha refusal)
                       (capture-anchored-tree store label)
                     (record store tree-sha refusal)))))))))))

;;; --- restore orchestration -----------------------------------------------

(defun restore-to-checkpoint (target-tree-sha session-head-tree-sha
                              &key workspace session-id)
  "Restore the workspace to TARGET-TREE-SHA, invoked by the navigation
handler AFTER the head move commits and outside any store transaction."
  ;; The redo preimage — the current tree, captured and anchored BEFORE
  ;; anything is touched — comes back to the caller so the abandoned state
  ;; stays restorable. Conflict expectations come from SESSION-HEAD-TREE-SHA
  ;; (the session's last recorded write), never from disk: comparing against
  ;; disk would make the foreign-write check vacuous.
  ;;
  ;; => plist (:status :restored|:degraded|:skipped
  ;;           :preimage-tree-sha sha-or-nil
  ;;           :changes (...) :conflicts (...)
  ;;           :degradation-reason kw-or-nil).
  ;;
  ;; The capture kill switch gates this whole path, not just capture: with
  ;; *WORKSPACE-CAPTURE-ENABLED* off nothing may touch the workspace's git
  ;; state — the preimage capture here runs ADD -A exactly like a boundary
  ;; capture does.
  (flet ((outcome (status preimage changes conflicts reason)
           (list :status status :preimage-tree-sha preimage
                 :changes changes :conflicts conflicts
                 :degradation-reason reason)))
    (unless *workspace-capture-enabled*
      (return-from restore-to-checkpoint
        (outcome :skipped nil '() '() :captures-disabled)))
    (with-handlers ((error (condition)
                      (outcome :degraded nil '() '()
                               (list :restore-failed (princ-to-string condition)))))
      (multiple-value-bind (store unavailable)
          (ensure-snapshot-store workspace session-id)
        (if (null store)
            (outcome :degraded nil '() '() unavailable)
            (let ((preimage (capture-workspace-tree store)))
              (anchor-tree store preimage "undo-redo-preimage")
              (if (equal preimage target-tree-sha)
                  (outcome :restored preimage '() '() nil)
                  (bind ((changes (changed-paths store target-tree-sha))
                         (expected
                           (loop for change in changes
                                 collect (and session-head-tree-sha
                                              (hash-in-tree store session-head-tree-sha
                                                            (getf change :path)))))
                         ((restored conflicts) (restore-paths store target-tree-sha changes expected)))
                    (outcome :restored preimage restored
                             (loop for conflict in conflicts
                                   for path = (getf conflict :path)
                                   collect (append conflict
                                                   (list :expected-sha
                                                         (hash-in-tree store target-tree-sha path)
                                                         :actual-sha
                                                         (hash-path-contents store path))))
                             nil)))))))))

;;; --- retention / repack lane ---------------------------------------------

(defun repack-store (store)
  "Collapse loose objects into packfile delta chains."
  ;; -a -d deletes only
  ;; loose objects that just became packed, never unreachable ones — this lane
  ;; never prunes. => T, or NIL on any failure (logged, never signalled).
  (with-handlers ((error (condition)
                    (warn "snapshot repack skipped: ~a" condition)
                    nil))
    (run-snapshot-git store (list "repack" "-a" "-d" "-q")) t))

(defun acquire-repack-lock (store)
  "Exclusive-create the repack lockfile; break it past the stale horizon so
a crashed repacker cannot disable compaction forever. => T on acquisition."
  (claim-file (snapshot-store-file store "repack.lock") 600))
