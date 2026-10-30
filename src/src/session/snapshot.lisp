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
;;;;     so concurrent sessions never contend on index.lock
;;;;   - every captured tree is anchored on the chained ref
;;;;     refs/nodecode/checkpoints; an unanchored write-tree is gc bait
;;;;   - gc.auto stays 0; REPACK-STORE is the only compactor and never prunes
;;;;   - restore is strictly per-path and hash-guarded: a path whose on-disk
;;;;     content differs from the session's last recorded write is a
;;;;     :foreign-write conflict, reported and left alone (never clobbered)
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
         "GIT_TERMINAL_PROMPT=0")))

(defun run-snapshot-git (store args &key (with-work-tree t))
  "Run one git plumbing call under the isolation prefix and a walltime bound."
  ;; Returns stdout as a string; any exit but 0 signals the snapshot condition
  ;; taxonomy.
  ;;
  ;; core.fsmonitor is the one driver a plain ADD runs that configuration
  ;; alone switches on, and configuration can still arrive from this
  ;; process's environment (GIT_CONFIG_PARAMETERS): the empty value turns it
  ;; off on every git that reads it, as a bool or as a hook path.
  (multiple-value-bind (output error-output status)
      (handler-case
          (run-bounded (list* *git-program* "-c" "core.fsmonitor=" args)
                       :seconds +snapshot-git-timeout-seconds+
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
  (let ((relative (uiop:subpathp
                   (uiop:pathname-parent-directory-pathname
                    (truename (snapshot-store-git-dir store)))
                   (truename (snapshot-store-workspace store)))))
    (when relative
      (let ((path (uiop:unix-namestring relative)))
        (if (zerop (length path)) "." path)))))

(defun write-store-exclude (store)
  (with-output-file (stream (snapshot-store-file store "info/exclude"))
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
              (format stream "/~a~a~%" (subseq db (length root)) suffix))))))))

(defun snapshot-git-dir (workspace)
  "The shadow store's git directory for WORKSPACE, a canonical directory
namestring: a folder of the home named by the digest of that path."
  ;; One store per workspace per home: a home's events name trees of its own
  ;; store. Two spellings of one folder (a symlink, a relative path) are one
  ;; canonical path, hence one store.
  (home (format nil "snapshots/~a/" (subseq (sha256-text workspace) 7 39))))

(defun ensure-snapshot-store (workspace session-id)
  "Open (initializing when absent) the shadow store for WORKSPACE/SESSION-ID."
  ;; => (values SNAPSHOT-STORE-or-NIL unavailable-keyword-or-NIL). Never signals.
  (with-handlers ((snapshot-git-unavailable () (values nil :git-missing))
                  (error () (values nil :store-init-failed)))
    (flet ((canonical (path)
             (ignore-errors
              (namestring (uiop:ensure-directory-pathname (truename path))))))
      (let* ((root (uiop:ensure-directory-pathname workspace))
             (canonical-root (canonical root)))
        (cond
          ((not (git-available-p)) (values nil :git-missing))
          ((not (uiop:directory-exists-p root)) (values nil :store-init-failed))
          ;; $HOME and a filesystem root are never a workspace to stage:
          ;; refused before any git runs, and recorded as the reason. The
          ;; account's home too, whatever $HOME this image was handed: a
          ;; scratch $HOME does not make the real one a workspace.
          ((or (null canonical-root)
               (equal canonical-root (canonical (user-homedir-pathname)))
               (let ((account (account-home)))
                 (and account (equal canonical-root (canonical account))))
               ;; no directory, or the filesystem root alone
               (null (rest (pathname-directory canonical-root))))
           (values nil :workspace-too-large))
          (t (open-snapshot-store root (snapshot-git-dir canonical-root) session-id)))))))

(defun open-snapshot-store (root git-dir session-id)
  "The store over the workspace ROOT kept in GIT-DIR, with SESSION-ID's index,
initializing GIT-DIR when absent: ENSURE-SNAPSHOT-STORE's last step, past its
refusals."
  (let ((store (make-snapshot-store
                :workspace (namestring root)
                :git-dir (string-right-trim "/" (namestring git-dir))
                :index-path (namestring
                             (merge-pathnames (format nil "index-~a" session-id) git-dir)))))
    ;; An index lock older than RUN-SNAPSHOT-GIT's walltime bound was left by
    ;; a process that died mid-walk; a younger one may be a capture.
    (let* ((lock (concatenate 'string (snapshot-store-index-path store) ".lock"))
           (age (and (probe-file lock)
                     (ignore-errors (- (get-universal-time) (file-write-date lock))))))
      (when (and (integerp age) (> age +snapshot-git-timeout-seconds+))
        (ignore-errors (delete-file lock))))
    (unless (uiop:directory-exists-p git-dir)
      (ensure-directories-exist git-dir)
      ;; git refuses init under GIT_WORK_TREE; config runs bare too.
      (run-snapshot-git store (list "init" "--bare" "--quiet") :with-work-tree nil)
      ;; gc.auto=0 is load-bearing: an auto-gc between capture and undo could
      ;; prune a tree the undo needs.
      (loop for (key value) in '(("core.bare" "false")
                                 ("gc.auto" "0")
                                 ("core.untrackedCache" "true")
                                 ("index.version" "4")
                                 ("core.symlinks" "true"))
            do (run-snapshot-git store (list "config" key value) :with-work-tree nil)))
    (write-store-exclude store)
    ;; Ignore rules do not untrack storage captured by an earlier version.
    (when-let (storage (snapshot-storage-path store))
      (bt2:with-lock-held ((capture-lock store))
        (run-snapshot-git store (list "--literal-pathspecs" "rm" "-r" "-f" "--cached" "--quiet"
                                      "--ignore-unmatch" "--" storage))))
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

(defparameter +snapshot-index-digest-cap+ (* 1024 1024)
  "How large an index may be and still be digested — a megabyte is some
twelve thousand staged paths, and reading it costs a fraction of the git
call the digest stands in for.")

(define-side-table index-trees (:test #'equal :synchronized t :key snapshot-store-index-path)
  "index path -> (DIGEST . TREE-SHA): the tree WRITE-TREE answered for that
index file with exactly that content. Strings only. Live-only.")

(defun index-digest (store)
  "The digest of STORE's index file as it now stands, or NIL when it is
absent, unreadable, or larger than +SNAPSHOT-INDEX-DIGEST-CAP+."
  (when-let (octets (file-octets (snapshot-store-index-path store) +snapshot-index-digest-cap+))
    (sha256-text octets)))

(defun capture-workspace-tree (store)
  "Stage the whole workspace and return its tree sha."
  ;; An unchanged workspace
  ;; yields the previous sha, so no-op captures allocate nothing — and an index
  ;; the staging left untouched answers from the written-tree cache, without the
  ;; second git call. One unreadable path does not void the tree: ADD
  ;; --IGNORE-ERRORS stages what it can, and a file the process may not read was
  ;; never going to be restored by it either. One walk of an index at a time
  ;; (CAPTURE-LOCK): a deferred capture still reading the tree and a completion
  ;; capture arriving behind it wait, never collide on index.lock.
  (bt2:with-lock-held ((capture-lock store))
    (run-snapshot-git store
                      (append (list "add" "-A" "--ignore-errors")
                              (when-let (storage (snapshot-storage-path store))
                                (list "--" "." (concatenate 'string ":(top,exclude,literal)" storage)))))
    (let* ((digest (index-digest store))
           (entry (and digest (index-trees store)))
           (tree (or (and (equal (car entry) digest) (cdr entry))
                     (let ((tree (validate-tree-sha (git-line store (list "write-tree")))))
                       ;; Read the digest again: WRITE-TREE records the tree it
                       ;; wrote in the index as its cache tree, so the content
                       ;; that answers for this tree is the one the index
                       ;; carries after the call, never the one it carried
                       ;; going in.
                       (when-let (settled (and digest (index-digest store)))
                         (setf (index-trees store) (cons settled tree)))
                       tree))))
      tree)))

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
        (let ((tree-sha (capture-workspace-tree store)))
          (anchor-tree store tree-sha message)
          (values tree-sha nil))
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
  (run-snapshot-git store (list "add" "-A"))
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

(defun note-anchoring-capture (store)
  "Count one anchoring capture toward the repack cadence: a repack every 64,
off the provider path; a held repack lock skips this one."
  (let* ((count (1+ (or (ignore-errors
                         (parse-integer
                          (read-text (snapshot-store-file store "capture-count"))
                          :junk-allowed t))
                        0)))
         (repack (and (>= count 64) (acquire-repack-lock store))))
    (when (or repack (< count 64))
      (ignore-errors
        (with-output-file (stream (snapshot-store-file store "capture-count"))
          (format stream "~a~%" (if repack 0 count)))))
    (when repack
      (unwind-protect (repack-store store)
        (ignore-errors
         (delete-file (snapshot-store-file store "repack.lock")))))))

(defun checkpoint-capture-boundary (turn &key boundary-kind tool-call-id
                                         pre-tree-sha tree-sha
                                         anchor-position defer)
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
  ;; when the capture began. The log reads exactly as it did when the walk
  ;; held the round: the capture, then the round that followed it.
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
          (bind (((store unavailable) (ensure-snapshot-store workspace session-id))
                 (label (format nil "checkpoint ~a ~a" boundary-kind
                                turn.turn-id)) (captured-at (iso-now)))
            (flet ((record (tree-sha unavailable)
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
                ((null store) (record nil unavailable))
                (tree-sha
                 (with-handlers ((error () (record nil :capture-failed)))
                   (anchor-tree store tree-sha label)
                   (record tree-sha nil)))
                (defer
                 (let ((walk (spawn "workspace-capture"
                               (with-handlers ((error () (list nil :capture-failed)))
                                 (multiple-value-list
                                  (capture-anchored-tree store label))))))
                   (defer-turn-fact
                    turn
                    (lambda ()
                      (destructuring-bind (tree-sha refusal)
                          (handler-case (bt2:join-thread walk)
                            (error () (list nil :capture-failed)))
                        (record tree-sha refusal))))
                   :deferred))
                (t (multiple-value-bind (tree-sha refusal)
                       (capture-anchored-tree store label)
                     (record tree-sha refusal)))))))))))

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
                  (let* ((changes (changed-paths store target-tree-sha))
                         (expected
                           (loop for change in changes
                                 collect (and session-head-tree-sha
                                              (hash-in-tree store session-head-tree-sha
                                                            (getf change :path))))))
                    (multiple-value-bind (restored conflicts)
                        (restore-paths store target-tree-sha changes expected)
                      (outcome :restored preimage restored
                               (loop for conflict in conflicts
                                     for path = (getf conflict :path)
                                     collect (append conflict
                                                     (list :expected-sha
                                                           (hash-in-tree store target-tree-sha path)
                                                           :actual-sha
                                                           (hash-path-contents store path))))
                               nil))))))))))

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
