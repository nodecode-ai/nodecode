;;;; generation.lisp --- the self sealed: a numbered record of what the organism is made of.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The binary is generation 0 and the folder on disk is the composed
;;;; self: the layer under git, one definition a file, a commit per keep; the
;;;; shipped cells as links into a release; a hub cell cloned at a pinned
;;;; commit. What that left out was a NUMBER for the whole -- a state of the
;;;; self the operator and the model can name, list, and go back to. Autolith
;;;; (lambda-symbolics/autolith) numbers heaps: save-lisp-and-die per
;;;; checkpoint, a pristine core to recover into. Here the heap is a cache
;;;; (launch.lisp, the warm image) and the folder is the material, so a
;;;; generation is a MANIFEST: the binary's version plus one row per folder
;;;; the load reads, each at its revision, as JSON under
;;;; ~/.nodecode/generations/<N>.json.
;;;;
;;;; A generation is sealed by proof, never by a keep alone. The launch-image
;;;; keeper (launch.lisp TEND-LAUNCH-IMAGE) already notices every settled
;;;; change to the folder and asks a fresh child to load it; before it asks
;;;; for an image it now asks a trial child to load the folder and judge every
;;;; override against the lambda list it displaced (provisional.lisp
;;;; PROBE-OVERRIDES). Green: the next number is written, and the image is
;;;; built from the same proof. Red: the key is refused -- no image stands for
;;;; it, the reason stands on the board under `generation' -- until the folder
;;;; moves.
;;;;
;;;; What a manifest is compared by is its rows (name, origin, revision), not
;;;; the launch key: a key moves with every stat, an identity with every
;;;; commit, pin, or content change. An own folder with uncommitted edits
;;;; carries their digest beside its commit, so a dirty self is an identity of
;;;; its own, and a restore to it can say what it cannot put back.
;;;;
;;;; Numbers count from 1 per home and are never reused or pruned; 0 is the
;;;; binary alone and is never written. (GENERATIONS) lists them. The restore
;;;; and the floor are the slices after this one.

(in-package #:nodecode.kernel)

;;; --- where the manifests live -----------------------------------------------

(defun generations-directory ()
  "~/.nodecode/generations/: the manifests, <N>.json each, and the markers
beside them. Read off the home now, so a profile and a test's scratch home
both apply."
  (home "generations/"))

(defparameter +manifest-format+ 1
  "The manifest's format; a reader passes over a manifest of another.")

(defun manifest-path (number)
  (merge-pathnames (format nil "~d.json" number) (generations-directory)))

(defmacro write-json-file (path &rest specs)
  "Write the object JSON-OBJECT builds of SPECS to PATH as indented JSON, atomically."
  `(write-file-atomically ,path (pretty-json (json-object ,@specs))))

(defun refusal-path ()
  "The marker a red verdict leaves: the key it refused, and why."
  (merge-pathnames "refused.json" (generations-directory)))

;;; The session an organism fact lands on when no turn asked for it: the
;;; gateway's boot session, +GATEWAY-SESSION-ID+ (client.lisp), one per home
;;; across every restart.
(defparameter *generation-session-id* "s-GATEWAY"
  "The session the seal and restore facts are recorded on.")

;;; Read once, at load, so a dumped image carries the version it was built
;;; from: the read COMPOSE-USER-AGENT-VERSION (provider.lisp) makes, of the one
;;; rule in nodecode.asd (NODECODE-VERSION).
(defvar *binary-version*
  (or (ignore-errors (asdf:component-version (asdf:find-system "nodecode"))) "0")
  "The version this binary reports, as a manifest records it.")

;;; --- the rows: every folder the load reads, at its revision -----------------

(defun git-root (directory)
  "The repository DIRECTORY sits in -- itself, or the nearest ancestor
carrying a .git -- or NIL."
  (loop for dir = (uiop:ensure-directory-pathname directory)
          then (uiop:pathname-parent-directory-pathname dir)
        repeat 8
        when (probe-file (merge-pathnames ".git" dir)) return dir
        until (null (rest (pathname-directory dir)))))

(defun git-output (root &rest args)
  "What git ARGS print in the repository at ROOT, trimmed; NIL when git fails or cannot run."
  (ignore-errors
   (bind ((directory (string-right-trim "/\\" (sb-ext:native-namestring root)))
          ((out _ status) (run-bounded (list* *git-program* "-C" directory args) :seconds 30)))
     (and (eql status 0) (trimmed out)))))

(defun git-dirty-p (root)
  "Whether the repository at ROOT carries uncommitted changes, tracked or
untracked: what a restore to this state could not put back."
  ;; A git that cannot run reads as clean: no process is no evidence.
  (plusp (length (git-output root "status" "--porcelain"))))

(defun git-tree-hash (root &aux (text (git-output root "rev-parse" "HEAD^{tree}")))
  "The tree HEAD names in the repository at ROOT, or NIL: what a restoring
commit shares with the commit it restores, and so what an own row is
compared by."
  (and (= 40 (length text)) text))

(defun tree-digest (directory &key keep)
  "sha256 over the visible files under DIRECTORY, each by its relative path
and its bytes -- the walk the launch key takes, links followed and dot entries
skipped: the revision of a folder nothing else versions. KEEP keeps the folder
too, so LAY-TREE can put it back whole: each file's bytes among the
generations' objects, by their sha256, and the listing under trees/, by the
digest. A file that does not read is then an error: a listing without it
would put back another folder."
  (let ((start (length (namestring (uiop:ensure-directory-pathname directory))))
        (lines '())
        (files '()))
    (map-visible-file-tree
     directory
     (lambda (path stat &aux (octets (file-octets path)) (relative (subseq path start))
                             (sha (and octets (sha256-text octets))))
       (push (format nil "~a ~a" relative (or sha "unreadable")) lines)
       (when keep
         (unless octets
           (error "~a does not read, so its folder cannot be kept" path))
         ;; a content kept once is kept: the same bytes sealed again cost a stat
         (let ((object (object-path sha)))
           (unless (probe-file object)
             (write-file-atomically object octets)))
         ;; the walk refills STAT for the next file, so its mode is read now
         (push (vector relative sha (logand #o777 (sb-posix:stat-mode stat))) files))))
    (let ((digest (sha256-text (format nil "~{~a~%~}" (sort lines #'string<)))))
      (when keep
        (write-json-file (listing-path digest) "digest" digest
                                               "files" (coerce (nreverse files) 'vector)))
      digest)))

;;; --- the bytes a seal keeps -----------------------------------------------------
;;; A row's revision names a folder; only a repository can put a named one
;;; back. What none can -- uncommitted edits, a folder a checkout holds, a
;;; plain folder -- the seal keeps whole: each file's bytes once, by their
;;; sha256, and one listing a folder, by its TREE-DIGEST, so the same content
;;; sealed again costs a lookup.

(defun objects-directory ()
  "~/.nodecode/generations/objects/: the bytes the seals keep, one file a
content, named by its sha256."
  (merge-pathnames "objects/" (generations-directory)))

(defun object-path (sha)
  "Where the bytes whose sha256 is SHA (`sha256:<hex>') are kept, as a native
namestring."
  (let ((hex (subseq sha 7)))
    (format nil "~a~a/~a" (sb-ext:native-namestring (objects-directory))
            (subseq hex 0 2) (subseq hex 2))))

(defun listing-path (digest)
  "Where the listing of the kept folder whose TREE-DIGEST is DIGEST is."
  (merge-pathnames (format nil "trees/~a.json" (subseq digest 7)) (generations-directory)))

(defun lay-tree (digest target)
  "Put the folder kept under DIGEST (TREE-DIGEST :KEEP) at TARGET, a directory
none stands at yet, each file with its mode, and prove it: the folder laid
digests to DIGEST. => TARGET, as a directory."
  (let ((listing (json-object-file (listing-path digest)))
        (root (sb-ext:native-namestring (uiop:ensure-directory-pathname target))))
    (unless listing
      (error "no folder is kept under ~a" (short-revision digest)))
    (when (probe-file root)
      (error "~a is there already: a kept folder is laid only where none stands" root))
    (ensure-directories-exist root)
    (loop for entry across (json-array listing "files")
          for (relative sha mode) = (coerce entry 'list)
          for octets = (file-octets (object-path sha))
          do (unless (and octets (equal sha (sha256-text octets)))
               (error "the bytes of ~a, kept as ~a, are gone or changed" relative (short-revision sha)))
             (write-file-atomically (concatenate 'string root relative) octets :mode mode))
    (let ((laid (tree-digest root)))
      (unless (equal laid digest)
        (error "the folder laid at ~a digests to ~a, not ~a"
               root (short-revision laid) (short-revision digest))))
    (uiop:ensure-directory-pathname target)))

(defun release-version (directory &aux (cells (nth-value 1 (shipped-relative directory))))
  "The version the release tree DIRECTORY is a folder of, by the name its
installer laid it down under: <releases>/<version>/src/cells/<name>/."
  (folder-name (uiop:pathname-parent-directory-pathname
                (uiop:pathname-parent-directory-pathname cells))))

(defun folder-row (folder &optional (root *cells-directory*) (repositories (make-hash-table :test #'equal)))
  "FOLDER, a CELL-FOLDER under ROOT, as a manifest row: (:name :kind :origin
:revision ...), the origin by the marks the installer and the scribe leave --
a hub marker (HUB-PIN), a release tree (RELEASE-FOLDER-P), a repository
(GIT-ROOT: its HEAD, and :tree the tree it names), else the folder's own
content -- :dirty the content digest of a repository with uncommitted
changes, and :checkout the repository's root when it holds more than the
folder: a checkout the operator works in, which a restore never moves. A
manifest records no :checkout; only the folder as it stands has one."
  (let* ((name (cell-folder-name folder))
         (directory (or (ignore-errors (truename folder.directory)) folder.directory))
         (pin (hub-pin name root))
         (release (and (null pin) (release-folder-p directory)))
         (git (and (null pin) (null release) (git-root directory))))
    (list* :name name
           :kind (if (some #'declared-system-layer (cell-folder-systems folder)) "layer" "cell")
           (cond (pin (list :origin "hub" :revision (second pin) :git (first pin) :path (third pin)))
                 (release (list :origin "release" :revision (release-version directory)))
                 (git (destructuring-bind (revision tree dirty)
                          (alexandria:ensure-gethash (namestring git) repositories
                           (list (or (git-head-sha git) "none") (git-tree-hash git) (git-dirty-p git)))
                        (list* :origin "own" :revision revision :tree tree
                               (append (when dirty (list :dirty (tree-digest directory)))
                                       (unless (equal (namestring git)
                                                      (namestring (uiop:ensure-directory-pathname directory)))
                                         (list :checkout (namestring git)))))))
                 (t (list :origin "digest" :revision (tree-digest directory)))))))

(defun folder-rows (root takes)
  "One row per cell folder under ROOT that TAKES, a predicate on the
CELL-FOLDER, answers true of."
  ;; A checkout can hold many cells. Its HEAD, tree and dirty verdict are
  ;; shared within this read; each dirty folder still has its own digest.
  (let ((repositories (make-hash-table :test #'equal)))
    (loop for directory in (cell-folders root)
          for folder = (make-cell-folder directory)
          when (funcall takes folder)
            collect (folder-row folder root repositories))))

(defun self-rows (&optional (root *cells-directory*))
  "One row per folder the load reads under ROOT, by name: every cell folder
but a knowledge one, which nothing loads."
  (folder-rows root (complement #'cell-folder-knowledge-p)))

(defun memory-rows (&optional (root *cells-directory*))
  "One row per knowledge folder under ROOT: the memories and skills a seal
records beside its rows, which no load reads and so no identity carries."
  (folder-rows root #'cell-folder-knowledge-p))

(defun kept-rows (rows &optional (root *cells-directory*))
  "ROWS, each a folder under ROOT, with :BYTES on every one whose bytes the
seal keeps (TREE-DIGEST :KEEP): a repository with uncommitted edits, a folder
a checkout holds, a folder nothing versions -- what no commit puts back. One
that moved since its row was read keeps none: its bytes are another folder's."
  ;; A checkout's own commit says nothing of a folder in it the checkout
  ;; ignores (a shipped cell pulled at a pin), so its bytes are kept even
  ;; when nothing in the checkout is uncommitted.
  (mapcar (lambda (row &aux (expected (or (getf row :dirty)
                                          (and (equal "digest" (getf row :origin))
                                               (getf row :revision)))))
            (let* ((folder (and root (or expected (getf row :checkout))
                                (folder-under (getf row :name) root)))
                   (digest (and folder
                                (ignore-errors
                                 (tree-digest (or (ignore-errors (truename folder)) folder) :keep t)))))
              (if (and digest (or (null expected) (equal digest expected)))
                  (append row (list :bytes digest))
                  row)))
          rows))

(defun row-identity (row)
  ;; a repository by the tree its HEAD names: a restoring commit is the
  ;; generation it restores, whatever its own hash
  (list (getf row :name) (getf row :origin) (or (getf row :tree) (getf row :revision)) (getf row :dirty)))

(defun self-identity (&optional (rows (self-rows)))
  "What a manifest is compared by: the binary's version, then every row's
(NAME ORIGIN REVISION DIRTY), by name."
  (cons *binary-version*
        (sort (mapcar #'row-identity rows) #'string< :key #'first)))

(defun short-revision (revision)
  "A commit's first seven characters, a digest's first twelve, anything
else whole."
  (cond ((null revision) "?")
        ((uiop:string-prefix-p "sha256:" revision)
         (subseq revision 7 (min (length revision) 19)))
        ((and (= 40 (length revision)) (every (lambda (c) (digit-char-p c 16)) revision))
         (subseq revision 0 7))
        (t revision)))

(defun row-line (row)
  "ROW in one line: name, origin, revision, and `dirty' when it carried
uncommitted edits."
  (format nil "~a ~a ~a~:[~; dirty~]" (getf row :name) (getf row :origin)
          (short-revision (getf row :revision)) (getf row :dirty)))

;;; --- the manifests ----------------------------------------------------------

(defun read-generation-manifest (path &aux (manifest (json-object-file path)))
  "The manifest at PATH, or NIL for one that does not read or is of another
format."
  (and (eql +manifest-format+ (json-value manifest :integer "format"))
       (integerp (json-value manifest :integer "number"))
       manifest))

(defun manifest-number (manifest)
  (json-value manifest :integer "number"))

(defun manifests ()
  "Every generation sealed in this home, newest first."
  (sort (nreverse (loop for path in (directory (merge-pathnames "*.json" (generations-directory)))
                        when (and (parse-integer (or (pathname-name path) "") :junk-allowed t)
                                  (read-generation-manifest path))
                          collect it))
        #'> :key #'manifest-number))

(defvar *manifest-count* nil
  "(STAMP . COUNT): how many manifests MANIFESTS read, and the generations
directory's stat when it read them.")

(defun manifest-count ()
  "How many generations this home has sealed, (length (manifests)), read
again only when the generations directory's stat moves."
  ;; A frame says this count (the home screen's fact block), and MANIFESTS
  ;; decodes every manifest, paid per frame. A seal lands its manifest by
  ;; rename (WRITE-FILE-ATOMICALLY), which moves the directory's mtime. The
  ;; mtime is second-granular, so a count read in the second the directory
  ;; last moved is not kept: a seal later in that second leaves the stat where
  ;; the read found it.
  (let* ((now (sb-ext:get-time-of-day))
         (directory (generations-directory))
         (stat (ignore-errors (sb-posix:stat (sb-ext:native-namestring directory))))
         (stamp (and stat (list (namestring directory) (sb-posix:stat-ino stat)
                                (sb-posix:stat-mtime stat))))
         (memo *manifest-count*))
    (if (and stamp (equal stamp (car memo)))
        (cdr memo)
        (let ((count (length (manifests))))
          (when (and stamp (< (third stamp) now))
            (setf *manifest-count* (cons stamp count)))
          count))))

(defun manifest-rows (manifest &optional (member "rows"))
  "MANIFEST's rows as SELF-ROWS answers them, :BYTES on each whose bytes its
seal kept; MEMBER \"memory\" its knowledge folders' (MEMORY-ROWS), none for a
manifest sealed before seals recorded them."
  (loop for row across (or (json-array manifest member) #())
        collect (json-members row :string "name" "kind" "origin" "revision" "tree" "dirty" "git"
                              "path" "bytes")))

(defun manifest-identity (manifest)
  (cons (json-value manifest :string "binary" "version")
        (sort (mapcar #'row-identity (manifest-rows manifest)) #'string< :key #'first)))

(defun sealed-generation (&optional (identity (self-identity)) (manifests (manifests)))
  "The manifest carrying IDENTITY -- the generation the folder stands at --
or NIL when it matches none."
  (find identity manifests :key #'manifest-identity :test #'equal))

(defun row-json (row)
  (json-object (:getf row "name" "kind" "origin" "revision")
               (:getf row :opt "tree" "dirty" "git" "path" "bytes")))

;;; --- the facts ---------------------------------------------------------------

(defun record-generation-fact (kind action payload owner
                               &key (session-id *generation-session-id*))
  "Append one generation fact on the boot session, minted standing by when
no boot has spoken in it yet (as RECORD-UPGRADE does). => the event id, or
NIL with no open store."
  (when (store-open-p)
    (unless (or (session-created-fact-p session-id) (on-standby-p session-id))
      (standby-session :id session-id))
    (record-organism-fact session-id kind action payload owner)))

;;; --- the seal ------------------------------------------------------------------

(defun seal-generation (key rows &key probe (session-id *generation-session-id*)
                                      (root *cells-directory*))
  "Write the next numbered manifest for ROWS -- the folder under launch KEY,
PROBE the trial child's report -- and record the fact when a store is open.
The manifest carries the bytes no repository can put back (KEPT-ROWS), so
BUILD-GENERATION lays it whole, and the knowledge folders as they stand
(MEMORY-ROWS). ROOT is the folder ROWS were read under. => the number."
  (let ((number (1+ (or (manifest-number (first (manifests))) 0))))
    (write-json-file (manifest-path number)
                     "format" +manifest-format+
                     "number" number
                     "sealed_at" (iso-now)
                     "binary" (json-object "version" *binary-version*
                                           "stamp" *image-stamp*)
                     "key" key
                     "rows" (map 'vector #'row-json (kept-rows rows root))
                     "memory" (map 'vector #'row-json (and root (kept-rows (memory-rows root) root)))
                     "probe" (json-object "verdict" "green"
                                          "overrides" (or (json-value probe :integer "overrides")
                                                          0)))
    ;; a red verdict that stood for an earlier key is over: the folder proved
    (ignore-errors (delete-file (refusal-path)))
    (record-generation-fact +kind-generation-sealed+ "organism.generation_sealed"
                            (list :number number :binary *binary-version* :key key
                                  :rows (map 'vector #'row-line rows))
                            'seal-generation :session-id session-id)
    number))

;;; --- the refusal -------------------------------------------------------------

(defun read-refusal ()
  (json-object-file (refusal-path)))

(defun refused-key-p (key &aux (refusal (read-refusal)))
  "Whether a red verdict stands for launch KEY: the marker names it."
  (and refusal (equal key (json-value refusal :string "key")) t))

(defun refusal-lines (&optional (refusal (read-refusal)))
  "What the standing verdict says, one line a failure."
  (and refusal (coerce (json-array refusal "failures") 'list)))

(defun refusal-text (lines &optional (manifests (manifests)))
  "The board's line under `generation' while a red verdict stands."
  (format nil "the folder as it stands is not sealed: ~{~a~^; ~}~@[. Generation ~d is the ~
               last sealed~]. A keep that mends it seals the next generation; (generations) ~
               lists them."
          lines (manifest-number (first manifests))))

;;; --- the prover ----------------------------------------------------------------

;;; The keeper proves the folder on a thread of its own, and a keep from
;;; /generations proves it on a request's: one at a time, so a number is
;;; never sealed twice and a key never judged twice.
(defvar *seal-lock* (bt2:make-lock :name "seal")
  "Held while the folder is judged and sealed (PROVE-SELF).")

(defun probe-form (report)
  "The form the trial child evaluates: the battery, its report at REPORT."
  (format nil "(nodecode.kernel:probe-overrides ~s)" (sb-ext:native-namestring report)))

(defun probe-failure-lines (report detail &aux (failures (json-array report "failures")))
  "A red verdict in words: one line per failure REPORT names, else what
ASK-CHILD said of the child (DETAIL)."
  (if (plusp (length failures))
      (loop for failure across failures
            collect (format nil "~a takes ~a, and its callers call ~a"
                            (json-value failure :string "name")
                            (json-value failure :string "override")
                            (json-value failure :string "shipped")))
      (list (format nil "the trial child ~a" detail))))

(defun prove-self (key)
  "Judge the folder as it stands under launch KEY, and seal it when it
proves. => :CURRENT when a manifest already carries its identity; (values
:SEALED N) for a new one; (values :RED LINES) when the trial child's verdict
is red, or was for this KEY; NIL with no folder, or no organism to spawn."
  ;; The trial child (ORGANISM-COMMAND, launch.lisp) boots the folder as a
  ;; launch does and runs the battery over the whole report (provisional.lisp
  ;; PROBE-OVERRIDES). A child that dies, hangs, or answers nothing is a red
  ;; verdict too: a folder that kills its loader proves nothing. One key is
  ;; judged once -- the marker answers for it until the folder moves.
  (when *cells-directory*
    (bt2:with-lock-held (*seal-lock*)
      (let ((rows (self-rows)))
        ;; the board's line under `generation' says a red verdict, or is taken down
        (flet ((said (text &rest verdict)
                 (note-standing "generation" text :warning)
                 (values-list verdict)))
          (cond ((sealed-generation (self-identity rows)) (said nil :current))
                ((refused-key-p key)
                 (let ((lines (refusal-lines)))
                   (said (refusal-text lines) :red lines)))
                (t
                 (when-let (command (organism-command "trial"))
                   (bind ((report (ensure-directories-exist
                                   (merge-pathnames (format nil "probe.~d.json" (sb-posix:getpid))
                                                    (generations-directory))))
                          ;; the trial child has 60 seconds to load the folder and judge it
                          ((score detail) (ask-child command (probe-form report) 60))
                          (verdict (json-object-file report)))
                     (ignore-errors (delete-file report))
                     (if (eql 1 score)
                         (said nil :sealed (seal-generation key rows :probe verdict))
                         (let ((lines (probe-failure-lines verdict detail)))
                           (write-json-file (refusal-path) "key" key "at" (iso-now)
                                                            "failures" (coerce lines 'vector))
                           (said (refusal-text lines) :red lines))))))))))))

;;; --- the words ------------------------------------------------------------------

(defvar *generation-label* nil
  "(WITNESS . LABEL): the boot line's last word, and what it was read from.")

(defun generation-label ()
  "The folder's word on the boot line (cells.lisp DESCRIBE-CELLS): `generation
N' when a manifest carries its identity, else `unsealed'."
  ;; The rows cost a git status per repository and a digest per plain folder,
  ;; so the word is read once per layer revision and manifest count -- what a
  ;; keep and a seal move.
  (let ((witness (list (ignore-errors (layer-revision))
                       (length (directory (merge-pathnames "*.json" (generations-directory)))))))
    (if (equal witness (car *generation-label*))
        (cdr *generation-label*)
        (let ((label (cond (*pristine* "pristine (generation 0)")
                           ((ignore-errors (sealed-generation))
                            (format nil "generation ~d" (manifest-number (sealed-generation))))
                           (t "unsealed"))))
          (setf *generation-label* (cons witness label))
          label))))

(defun change-delta (change previous &aux (keeps (getf change :keeps))
                                          (number (manifest-number previous)))
  "A moved row's CHANGE since the manifest PREVIOUS, in the list's words:
the keeps its repository took, the ones a move back undid, else that its
revision moved."
  (cond ((null keeps) (format nil "changed since ~d" number))
        ((getf (first keeps) :undone) (format nil "~d keep~:p undone since ~d" (length keeps) number))
        (t (format nil "+~d keep~:p since ~d" (length keeps) number))))

(defun generation-deltas (manifests &aux (history (make-hash-table :test #'equal)))
  "MANIFESTS' adjacent changes, newest first, one read of each root range."
  (loop for (manifest . older) on manifests
        collect (and older (generation-changes (first older) manifest :history history))))

(defun generations (&aux (manifests (manifests))
                         (current (let ((identity (ignore-errors (self-identity))))
                                    (and identity (sealed-generation identity manifests))))
                         (deltas (generation-deltas manifests)))
  "Every generation sealed in this home, in words: the model's and the
operator's (generations)."
  (with-output-to-string (out)
    (when *pristine*
      (format out "pristine: generation 0, no folder loaded~@[ -- ~a~]~%" (pristine-reason)))
    (if (null manifests)
        (format out "no generation sealed in this home yet~%")
        (loop for (manifest . older) on manifests
              for previous = (first older)
              for changes in deltas
              do (format out "~&generation ~d~:[~;  current~]  ~a  nodecode ~a~%"
                         (manifest-number manifest) (eq manifest current)
                         (json-value manifest :string "sealed_at")
                         (json-value manifest :string "binary" "version"))
                 (dolist (row (manifest-rows manifest))
                   (destructuring-bind (&key name kind origin revision dirty &allow-other-keys) row
                     (format out "  ~a ~a ~a ~a~:[~;  dirty~]~@[  ~a~]~%"
                             name kind origin (short-revision revision) dirty
                             (when-let (change (find-if (lambda (change)
                                                          (and (eq :moved (getf change :change))
                                                               (equal name
                                                                      (getf (getf change :row) :name))))
                                                        changes))
                               (change-delta change previous)))))
                 (loop for kind in '(:added :removed)
                       for names = (loop for change in changes
                                         when (eq kind (getf change :change))
                                           collect (getf (getf change :row) :name))
                       when names do (format out "  ~(~a~) since ~d: ~{~a~^, ~}~%"
                                             kind (manifest-number previous) names))))
    (unless current
      (format out "~&unsealed: the folder as it stands matches no generation~@[ -- ~{~a~^; ~}~]~%"
              (refusal-lines)))))

;;; --- what moved between two generations -------------------------------------
;;; A manifest says what a generation IS; a reader choosing one needs what it
;;; CHANGED: the binary, the folders that came and went, and for a folder in a
;;; repository the commits between its two revisions -- the scribe's subject
;;; names each keep. Read off the manifests and the folders' own history at
;;; the asking, never stored with a manifest.

(defparameter +changes-keep-cap+ 50
  "Keeps listed for one folder's move; older ones go unnamed.")

(defun folder-repository (row)
  "=> (values ROOT PATHSPEC): the repository an own ROW's folder sits in
under the cells directory, and the folder's path inside it -- or NIL."
  ;; A shipped folder linked from a checkout shares that checkout's history,
  ;; so every read is limited to the folder's own path.
  (when (and (equal "own" (getf row :origin)) *cells-directory*)
    (let* ((folder (ignore-errors (truename (folder-under (getf row :name) *cells-directory*))))
           (root (and folder (git-root folder))))
      (when root
        (let ((inside (enough-namestring folder (truename root))))
          (values root (if (plusp (length inside)) inside ".")))))))

(defun folder-history-paths (root from to memo)
  "The touched-path superset of ROOT's FROM/TO range, held only in MEMO:
(T . PATHS) when read, :UNREADABLE when Git cannot say."
  ;; Symmetric history includes either direction; -m and --full-history
  ;; include merge edges, and --no-renames includes both old and new paths.
  ;; This only rules out untouched folders. Touched ones still use the exact
  ;; path-specific log below, with its existing simplification and cap.
  (alexandria:ensure-gethash (list (namestring root) (sort (list from to) #'string<)) memo
   (multiple-value-bind (out code)
       (git-in-layer root "log" "-z" "--format=" "--name-only" "--no-renames"
                     "--full-history" "-m" (format nil "~a...~a" from to) "--")
     (if (eql code 0)
         (cons t (remove "" (uiop:split-string out :separator (list (code-char 0))) :test #'equal))
         :unreadable))))

(defun folder-keeps (root pathspec from to &optional history)
  "The commits that took PATHSPEC in the repository at ROOT from revision
FROM to TO, oldest first, each (:sha :subject); when TO is behind FROM, the
commits a move back undoes, newest first, each also :undone T. NIL when git
cannot say. HISTORY shares touched-path reads within one roster read."
  ;; A Git pattern is not a literal prefix: let Git interpret it unchanged.
  (when (and history (notany (lambda (char) (find char "*?[:\\")) pathspec))
    (let ((paths (folder-history-paths root from to history)))
      (when (and (consp paths)
                 (notany (lambda (path) (or (equal pathspec ".") (uiop:string-prefix-p pathspec path)))
                         (cdr paths)))
        (return-from folder-keeps nil))))
  (flet ((logged (range)
           (multiple-value-bind (out code)
               (git-in-layer root "log" (format nil "--max-count=~d" +changes-keep-cap+)
                             "--format=%h%x09%s" range "--" pathspec)
             (when (eql code 0)
               (loop for line in (uiop:split-string out :separator '(#\Newline))
                     for tab = (position #\Tab line)
                     when tab collect (list :sha (subseq line 0 tab) :subject (subseq line (1+ tab))))))))
    (or (reverse (logged (format nil "~a..~a" from to)))
        (mapcar (lambda (keep) (list* :undone t keep)) (logged (format nil "~a..~a" to from))))))

(defun generation-self (generation)
  "=> (values VERSION ROWS): GENERATION's binary and rows -- a manifest's,
the folder's as it stands for :NOW, nothing yet for NIL."
  (case generation
    (:now (values *binary-version* (self-rows)))
    (t (values (json-value generation :string "binary" "version") (manifest-rows generation)))))

(defun generation-changes (from to &key history)
  "What moves from FROM to TO -- each a manifest, :NOW for the folder as it
stands, or NIL for nothing yet -- the binary first, then the folders by
name: (:change :binary :from V :to V), (:change :added :row ROW), (:change
:removed :row ROW), (:change :moved :row ROW :before ROW :keeps KEEPS).
KEEPS (FOLDER-KEEPS) come with a folder whose repository holds both
revisions; HISTORY shares its touched-path reads."
  ;; A row moved when its identity did (ROW-IDENTITY): what a seal compares,
  ;; so a restoring commit that names an older tree is no move.
  (multiple-value-bind (from-version from-rows) (generation-self from)
    (multiple-value-bind (to-version to-rows) (generation-self to)
      (append
       (when (and from-version to-version (not (equal from-version to-version)))
         (list (list :change :binary :from from-version :to to-version)))
       (loop for name in (sort (distinct (mapcar (lambda (row) (getf row :name))
                                                 (append from-rows to-rows)))
                               #'string<)
             for before = (row-named name from-rows)
             for after = (row-named name to-rows)
             nconc (cond ((null before) (list (list :change :added :row after)))
                         ((null after) (list (list :change :removed :row before)))
                         ((equal (row-identity before) (row-identity after)) nil)
                         (t (list (row-move before after history)))))))))

(defun row-move (before after &optional history &aux (from (getf before :revision))
                                                     (to (getf after :revision)))
  "The :MOVED change from the row BEFORE to AFTER: a repository's keeps
between the two revisions."
  (multiple-value-bind (root pathspec) (folder-repository after)
    (append (list :change :moved :row after :before before)
            (when (and root (equal "own" (getf before :origin)) (not (equal from to)))
              (list :keeps (folder-keeps root pathspec from to history))))))

(defparameter +kept-edits-subject+ "the uncommitted edits, kept from /generations"
  "The subject a keep of the uncommitted edits is committed under.")

(defparameter +discarded-edits-subject+ "the uncommitted edits, discarded from /generations"
  "The message the stash of discarded edits carries.")

;;; --- the history: the generations as the changes they were -----------------
;;; A reader going back wants the act, not the seal: the keeps one session
;;; made, the nodecode update, the cell installed. The history folds adjacent
;;; generations of one act into one change and says each in words -- the
;;; keeps by name and the session that made them, the binary's commits, the
;;; files the layer's keeps moved -- read off the manifests and the
;;; repositories at the asking, never stored with a manifest.

(defun keep-name (subject &aux (cut (search " — session " subject)))
  "A keep's SUBJECT without the session the scribe signs it with."
  (if cut (subseq subject 0 cut) subject))

(defun keep-session (subject &aux (cut (search " — session " subject)))
  "The session the scribe signed a keep's SUBJECT with, or NIL."
  (and cut (subseq subject (+ cut (length " — session ")))))

(defun restored-number (subject)
  "The generation a restore's commit SUBJECT names (RESTORE-LAYER), or NIL."
  (and (uiop:string-prefix-p "restored generation " subject)
       (parse-integer subject :start (length "restored generation ") :junk-allowed t)))

(defun generation-parts (changes builtin &aux (parts '()) (groups '()))
  "CHANGES, what moved into one generation (GENERATION-CHANGES), as the parts
its history says: (:binary FROM TO); (:added NAME ORIGIN), (:removed NAME
ORIGIN); a layer's (:keeps NAME KEEPS), or (:restored N NAME) when they are a
restore's; (:edits NAME) for uncommitted edits that moved under one commit;
and the other folders whose revision moved, one part for those that moved
together -- (:builtin COUNT ORIGIN FROM TO) when one of them is among the
BUILTIN names, the cells this binary ships, else (:moved NAMES ORIGIN FROM
TO)."
  (dolist (change changes)
    (let* ((row (getf change :row))
           (before (getf change :before))
           (name (getf row :name))
           (keeps (getf change :keeps)))
      (case (getf change :change)
        (:binary (push (list :binary (getf change :from) (getf change :to)) parts))
        (:added (push (list :added name (getf row :origin)) parts))
        (:removed (push (list :removed name (getf row :origin)) parts))
        (:moved
         (cond ((equal (getf before :revision) (getf row :revision))
                (push (list :edits name) parts))
               ((and keeps (equal "layer" (getf row :kind)))
                (push (let ((restored (some (lambda (keep) (restored-number (getf keep :subject))) keeps)))
                        (if restored (list :restored restored name) (list :keeps name keeps)))
                      parts))
               (t (let* ((key (list (getf row :origin) (short-revision (getf before :revision))
                                    (short-revision (getf row :revision))))
                         (group (assoc key groups :test #'equal)))
                    (if group (push name (cdr group)) (push (list key name) groups)))))))))
  (loop for ((origin from to) . names) in (reverse groups)
        do (push (if (some (lambda (name) (member name builtin :test #'equal)) names)
                     (list :builtin (length names) origin from to)
                     (list :moved (reverse names) origin from to))
                 parts))
  (nreverse parts))

(defun parts-kind (parts)
  "The kind of change PARTS make: what the history files a generation under."
  (loop for (kind . verbs) in '(("restore" :restored) ("agent" :keeps) ("install" :added)
                                ("remove" :removed) ("nodecode" :binary :builtin) ("cell" :moved))
        when (find-if (lambda (part) (member (first part) verbs)) parts) return kind
        finally (return "edits")))

(defun parts-session (parts &aux (counts '()))
  "The session most of the keeps in PARTS were signed by, or NIL."
  (dolist (part parts)
    (when (eq :keeps (first part))
      (dolist (keep (third part))
        (when-let (session (keep-session (getf keep :subject)))
          (let ((entry (assoc session counts :test #'equal)))
            (if entry (incf (cdr entry)) (push (cons session 1) counts)))))))
  (car (first (stable-sort (nreverse counts) #'> :key #'cdr))))

(defun session-words (session-id title-of)
  "Where keeps of SESSION-ID were made, in words, its title read by TITLE-OF."
  (flet ((titled (id) (let ((title (and id (funcall title-of id))))
                        (and title (plusp (length title)) (format nil "“~a”" (one-line title))))))
    (cond ((null session-id) "")
          ((uiop:string-prefix-p "experience-" session-id)
           (let* ((rest (subseq session-id (length "experience-")))
                  (parent (subseq rest 0 (search "-turn-" rest))))
             (format nil "a reflection after ~a" (or (titled parent) "a turn"))))
          ((equal "layer" session-id) "the layer session")
          ((uiop:string-prefix-p "discord-" session-id)
           (format nil "Discord · ~a" (or (titled session-id) "a thread")))
          (t (or (titled session-id) (format nil "session ~a" session-id))))))

(defparameter +origin-words+
  '(("hub" . "a hub install") ("own" . "under git") ("release" . "built in") ("digest" . "a plain folder"))
  "A row's origin as the history says it.")

(defun listed (names &optional (shown 3))
  "NAMES as a row lists them: the first SHOWN, then how many more."
  (format nil "~{~a~^, ~}~@[ +~d~]" (subseq names 0 (min shown (length names)))
          (and (> (length names) shown) (- (length names) shown))))

(defun layer-numstat (name from to)
  "=> (COUNT ADDED REMOVED PATHS): the files of folder NAME's repository
that moved from revision FROM to TO, PATHS the four that moved most; NIL
when git cannot say."
  (multiple-value-bind (root pathspec) (folder-repository (list :name name :origin "own"))
    (when (and root from to (not (equal from to)))
      (multiple-value-bind (out code)
          (git-in-layer root "diff" "--numstat" (format nil "~a..~a" from to) "--" pathspec)
        (when (eql code 0)
          (let ((files (loop for line in (lines out)
                             for fields = (uiop:split-string line :separator '(#\Tab))
                             when (= 3 (length fields))
                               collect (list (third fields)
                                             (or (parse-integer (first fields) :junk-allowed t) 0)
                                             (or (parse-integer (second fields) :junk-allowed t) 0)))))
            (list (length files) (reduce #'+ files :key #'second) (reduce #'+ files :key #'third)
                  (mapcar #'first (subseq (stable-sort (copy-list files) #'>
                                                       :key (lambda (file) (+ (second file) (third file))))
                                          0 (min 4 (length files)))))))))))

(defun row-named (name rows)
  (find name rows :key (lambda (row) (getf row :name)) :test #'equal))

(defun fold-words (kind all manifests before builtin title-of)
  "=> (values TITLE WHERE NOTES KEEPS FILES) for a change of KIND whose
generations are MANIFESTS, oldest first, ALL their parts, BEFORE the manifest
sealed before them, BUILTIN the names of the cells this binary ships."
  (labels ((parts (verb) (remove verb all :key #'first :test-not #'eq))
           (origin-words (origin) (or (cdr (assoc origin +origin-words+ :test #'equal)) origin))
           (also (verbs)
             ;; what else the change carried, two lines at most
             (let ((notes (distinct
                           (loop for (verb . detail) in all
                                 for note = (and (member verb verbs)
                                                 (case verb
                                                   (:binary (format nil "also: nodecode ~a → ~a"
                                                                    (short-version (first detail))
                                                                    (short-version (second detail))))
                                                   (:builtin (format nil "also: the ~d built-in cells moved with ~a"
                                                                     (first detail)
                                                                     (if (equal "release" (second detail))
                                                                         "a release"
                                                                         "your checkout")))
                                                   (:added (format nil "also: installed ~a" (first detail)))
                                                   (:removed (format nil "also: removed ~a" (first detail)))
                                                   (:moved (format nil "also: ~a changed" (listed (first detail) 2)))))
                                 when note collect note))))
               (subseq notes 0 (min 2 (length notes))))))
    (let ((newest (car (last manifests))))
      (cond
        ((equal kind "first")
         (values "First generation"
                 (format nil "~d folder~:p · nodecode ~a" (length (manifest-rows newest))
                         (short-version (json-value newest :string "binary" "version")))))
        ((member kind '("agent" "restore") :test #'equal)
         (let* ((keeps (loop for (nil nil keeps) in (parts :keeps) append keeps))
                (subjects (distinct (mapcar (lambda (keep) (getf keep :subject)) keeps)))
                (names (distinct (mapcar #'keep-name subjects)))
                (forgot (loop for name in names
                              when (uiop:string-prefix-p "forgot " name) collect (subseq name 7)))
                (kept (remove-if (lambda (name) (uiop:string-prefix-p "forgot " name)) names))
                (layer (or (second (first (parts :keeps))) (third (first (parts :restored)))))
                (files (and layer (layer-numstat
                                   layer (getf (row-named layer (manifest-rows before)) :revision)
                                   (getf (row-named layer (manifest-rows newest)) :revision)))))
           (values (cond ((equal kind "restore")
                          (format nil "Went back to generation ~d" (second (car (last (parts :restored))))))
                         ((equal subjects (list +kept-edits-subject+)) "Kept the uncommitted edits")
                         ((every (lambda (keep) (getf keep :undone)) keeps)
                          (format nil "Undid ~a" (listed names)))
                         (t (format nil "~{~a~^ · ~}"
                                    (append (and kept (list (format nil "Kept ~a" (listed kept))))
                                            (and forgot (list (format nil "~:[F~;f~]orgot ~a"
                                                                      kept (listed forgot 2))))))))
                   (cond ((equal kind "restore") "")
                         ((equal subjects (list +kept-edits-subject+)) "from /generations")
                         (t (session-words (parts-session all) title-of)))
                   (also '(:binary :builtin :added :removed :moved))
                   names files)))
        ((equal kind "nodecode")
         (let ((binaries (parts :binary))
               (builtins (parts :builtin)))
           (if binaries
               (let ((from (short-version (second (first binaries))))
                     (to (short-version (third (car (last binaries))))))
                 (values (if (equal from to)
                             (format nil "nodecode rebuilt at ~a" to)
                             (format nil "nodecode ~a → ~a" from to))
                         (format nil "~{~a~^ · ~}"
                                 (append (and (rest binaries) (list (format nil "~d builds" (length binaries))))
                                         (and builtins (list (format nil "~d built-in cells"
                                                                     (second (car (last builtins))))))))
                         (also '(:added :removed :moved))))
               (destructuring-bind (origin from) (subseq (first builtins) 2 4)
                 (values (format nil "Built-in cells ~a → ~a" from (fifth (car (last builtins))))
                         (format nil "~d cells · ~a" (second (car (last builtins)))
                                 (if (equal "release" origin) "a release" "your checkout"))
                         (also '(:added :removed :moved)))))))
        ((equal kind "edits")
         (let* ((names (distinct (mapcar #'second (parts :edits))))
                (own (remove-if (lambda (name) (member name builtin :test #'equal)) names))
                (built (- (length names) (length own)))
                (count (length manifests)))
           (values (format nil "Uncommitted edits in ~{~a~^ and ~}"
                           (append (and own (list (listed own 2)))
                                   (and (plusp built) (list (format nil "~d built-in cell~:p" built)))))
                   (if (> count 1) (format nil "~d snapshots" count) "one snapshot")
                   (list "Uncommitted files, as nodecode found them: going back here returns the commit, not those files"))))
        ((equal kind "cell")
         (let* ((moved (parts :moved))
                (names (distinct (loop for (nil names) in moved append names))))
           (if (rest names)
               (values (format nil "~a changed" (listed names 2)) (format nil "~d cells" (length names)))
               (destructuring-bind (origin from) (subseq (rest (rest (first moved))) 0 2)
                 (values (format nil "~a ~a → ~a" (first names) from (fifth (car (last moved))))
                         (origin-words origin))))))
        (t
         (let* ((verb (if (equal kind "install") :added :removed))
                (found (parts verb)))
           (values (format nil "~:[Removed~;Installed~] ~a" (eq verb :added)
                           (listed (distinct (mapcar #'second found))))
                   (format nil "~{~a~^, ~}" (distinct (loop for (nil nil origin) in found
                                                             collect (origin-words origin))))
                   (also '(:binary :builtin :moved)) '() nil)))))))

(defun generation-history (manifests &key (deltas (generation-deltas manifests)) builtin
                                          (title-of #'session-title)
                                          &aux (folds '()) (by-number (make-hash-table)))
  "MANIFESTS (newest first) as the changes they were, newest first, each a
plist: :kind (agent, nodecode, cell, install, remove, edits, restore, first),
:from and :to the generations it spans, :first-at and :at when the first and
the last were sealed, :title and :where in words, :notes, :keeps the names of
an agent's keeps, :files (COUNT ADDED REMOVED PATHS) the layer's, :version
the binary at :to, and :nodecode when nodecode alone moved in it -- the
binary, the cells it ships. DELTAS are GENERATION-DELTAS' of MANIFESTS,
BUILTIN the names of the cells this binary ships, TITLE-OF a session's title."
  ;; oldest first: a change takes the next generation of its own act
  (loop for manifest in (reverse manifests)
        for changes in (reverse deltas)
        for first = t then nil
        for parts = (generation-parts changes builtin)
        for kind = (if first "first" (parts-kind parts))
        for session = (and (equal kind "agent") (parts-session parts))
        for at = (or (iso-universal (json-value manifest :string "sealed_at")) 0)
        for fold = (first folds)
        ;; how far apart, in minutes, two generations of one kind may be sealed and
        ;; still read as one change; a kind not named never folds
        for minutes = (cdr (assoc kind '(("agent" . 60) ("nodecode" . 60) ("edits" . 180))
                                  :test #'equal))
        do (setf (gethash (manifest-number manifest) by-number) manifest)
           (if (and fold minutes (equal kind (getf fold :kind)) (equal session (getf fold :session))
                    (<= (- at (getf fold :at)) (* 60 minutes)))
               (setf (getf fold :manifests) (append (getf fold :manifests) (list manifest))
                     (getf fold :parts) (append (getf fold :parts) parts)
                     (getf fold :at) at)
               (push (list :kind kind :session session :at at :manifests (list manifest) :parts parts)
                     folds)))
  (loop for fold in folds
        for generations = (getf fold :manifests)
        for oldest = (first generations)
        for newest = (car (last generations))
        collect (multiple-value-bind (title where notes keeps files)
                    (fold-words (getf fold :kind) (getf fold :parts) generations
                                (gethash (1- (manifest-number oldest)) by-number) builtin title-of)
                  (list :kind (getf fold :kind)
                        :from (manifest-number oldest) :to (manifest-number newest)
                        :first-at (json-value oldest :string "sealed_at")
                        :at (json-value newest :string "sealed_at")
                        :title title :where where :notes notes :keeps keeps :files files
                        :version (json-value newest :string "binary" "version")
                        :nodecode (and (getf fold :parts)
                                       (every (lambda (part)
                                                (or (member (first part) '(:binary :builtin))
                                                    (and (eq :edits (first part))
                                                         (member (second part) builtin :test #'equal))))
                                              (getf fold :parts)))))))


;;; --- the restore: every row back to a manifest ------------------------------
;;;
;;; A restore is a NEW state of the folder, never a rewind of its history: the
;;; layer takes one commit whose tree is the generation's (git read-tree -m -u,
;;; then a commit), so every turn row's definition_revision still resolves and
;;; forward is the same verb; a hub clone is put back at the manifest's commit;
;;; a shipped folder a release laid out stays with the release the binary
;;; runs: the binary and its shipped cells are the kernel's side, which only
;;; an update moves (update.lisp), so a restore is the operator's side alone
;;; and the poller, carrying the kernel forward, never undoes it -- Autolith's
;;; split of tracked source from private mutations; a folder the
;;; manifest had and the home lost is installed,
;;; one the home has and the manifest never had is uninstalled where the
;;; installer put it. A plain folder whose content differs refuses the whole
;;; restore by name before anything moves: nothing versions it, so nothing can
;;; put it back; so do uncommitted edits in a repository of its own, until
;;; they are kept or discarded. A folder whose checkout holds more than it --
;;; the shipped cells a dev box links into its nodecode checkout -- stays where
;;; it stands. The restore leaves generations/restoring.json; the boot that
;;; finds it records the fact and says the line.

(defun restoring-path ()
  (merge-pathnames "restoring.json" (generations-directory)))

(defun folder-under (name root)
  (merge-pathnames (format nil "~a/" name) root))

(defun manifest-named (number)
  "The manifest of generation NUMBER, or an error naming what there is."
  (or (and (integerp number) (plusp number) (read-generation-manifest (manifest-path number)))
      (let ((newest (first (manifests))))
        (error "no generation ~a: ~:[none is sealed yet~;1 to ~d are~]"
               number newest (and newest (manifest-number newest))))))

(defun row-offer (row)
  "A hub ROW as the offer CLONE-INSTALL and INSTALL-CELL take."
  (make-offer :name (getf row :name) :git (getf row :git)
              :commit (getf row :revision) :path (getf row :path)))

(defun an (origin)
  "The article ORIGIN, a row's origin word, takes."
  (if (equal origin "own") "an" "a"))

(defun restore-steps (number then now &aux (steps '()))
  "What restoring generation NUMBER, whose rows are THEN, does to a folder
whose rows are NOW, in order: (:keep NAME NOTE) for a row already at the
manifest's revision, (:stays NAME CHECKOUT) for a folder whose checkout holds
more than it -- the operator's, which a restore leaves where it stands --
(:layer NAME SHA DIRTY) for a repository to move, (:hub NAME ROW) for a clone
to put back at its pin, (:install NAME ROW), (:uninstall NAME),
(:edits NAME SHA) for a repository whose uncommitted edits must be kept or
discarded first -- SHA where it moves after, NIL when its tree stays -- and
(:refuse NAME REASON) for a row nothing can restore."
  ;; Moving a checkout that holds more than the folder would rewrite the
  ;; operator's other work. Its edits are its own business, so they hold
  ;; nothing.
  (flet ((place (row) (or (getf row :tree) (getf row :revision))))
    (dolist (row then)
      (let* ((name (getf row :name))
             (live (row-named name now))
             (origin (getf row :origin))
             (revision (getf row :revision)))
        (push
         (cond
           ((null live)
            (if (member origin '("hub" "release") :test #'equal)
                (list :install name row)
                (list :refuse name (format nil "~a is gone, and generation ~d had it as ~a ~a folder ~
                                                 at ~a: nothing can put it back"
                                           name number (an origin) origin (short-revision revision)))))
           ((getf live :checkout)
            (if (equal (place row) (place live))
                (list :keep name nil)
                (list :stays name (getf live :checkout))))
           ((not (equal origin (getf live :origin)))
            (list :refuse name (format nil "~a is now ~a ~a folder; generation ~d had it as ~a ~a one"
                                       name (an (getf live :origin)) (getf live :origin) number
                                       (an origin) origin)))
           ((equal origin "own")
            (cond ((getf live :dirty)
                   (list :edits name (and (not (equal (place row) (place live))) revision)))
                  ((equal (place row) (place live))
                   (list :keep name (and (getf row :dirty)
                                         (format nil "the uncommitted edits generation ~d had in ~a are not restorable"
                                                 number name))))
                  (t (list :layer name revision (getf row :dirty)))))
           ;; A release's shipped folder is the kernel's side: it stays with
           ;; the release the binary runs, which only an update moves.
           ((or (equal revision (getf live :revision)) (equal origin "release"))
            (list :keep name nil))
           ((equal origin "hub") (list :hub name row))
           (t (list :refuse name (format nil "~a is a plain folder whose content differs from ~
                                               generation ~d's; nothing versions it, so put it back by hand"
                                         name number))))
         steps)))
    (dolist (row now)
      (unless (row-named (getf row :name) then)
        (push (list :uninstall (getf row :name)) steps))))
  (nreverse steps))

(defun restore-plan (number &key (root *cells-directory*) &aux (manifest (manifest-named number)))
  "RESTORE-STEPS for generation NUMBER over the folder under ROOT as it
stands. => (values STEPS MANIFEST)."
  (values (restore-steps number (manifest-rows manifest) (self-rows root)) manifest))

(defun step-refusals (steps)
  "Why STEPS can never restore, one line a folder nothing can put back; NIL
when nothing refuses. Uncommitted edits are no refusal: keeping or
discarding them mends it."
  (loop for (verb nil reason) in steps
        when (eq verb :refuse) collect reason))

(defun step-edits (steps)
  "The folders whose uncommitted edits hold STEPS, by name."
  (loop for (verb name) in steps
        when (eq verb :edits) collect name))

(defun names-text (names)
  "NAMES as one phrase: `a', `a and b', `a and 3 more'."
  (format nil "~a~[~; and ~a~:; and ~:*~d more~]" (first names) (1- (length names)) (second names)))

(defun stays-lines (steps)
  "The folders of STEPS a checkout holds, one line a checkout: they stay."
  (loop with stays = (remove :stays steps :key #'first :test-not #'eq)
        for checkout in (distinct (mapcar #'third stays))
        for names = (loop for (nil name where) in stays when (equal where checkout) collect name)
        collect (format nil "~a stay~:[s~;~] where the checkout at ~a has ~:[it~;them~]: ~
                             a restore never moves a checkout"
                        (names-text names) (rest names) (home-abbreviated checkout) (rest names))))

(defun short-version (version &aux (plus (position #\+ (or version ""))))
  "A binary's VERSION as a line says it: the commit it was built at, else whole."
  (if plus (subseq version (1+ plus) (min (length version) (+ plus 8))) (or version "?")))

(defun plan-lines (steps number &key version)
  "STEPS (RESTORE-STEPS) as a look ahead says them before generation NUMBER
is restored: one line a folder that moves, the notes a kept row carries, the
binary when it stays where generation NUMBER, built as VERSION, had another,
and the folders a checkout holds, one line a checkout."
  (append
   (loop for (verb name . detail) in steps
         for line = (case verb
                      (:keep (first detail))
                      (:layer (format nil "~a to ~a~:[~;; the uncommitted edits it had then do not come back~]"
                                      name (short-revision (first detail)) (second detail)))
                      (:edits (and (first detail)
                                   (format nil "~a to ~a, once its uncommitted edits are kept or discarded"
                                           name (short-revision (first detail)))))
                      (:hub (format nil "~a to its pin ~a" name (short-revision (getf (first detail) :revision))))
                      (:install (format nil "~a comes back" name))
                      (:uninstall (if (installer-put-p name)
                                      (format nil "~a goes: generation ~d never had it" name number)
                                      (format nil "~a stays: it is yours, though generation ~d never had it"
                                              name number))))
         when line collect line)
   (when (and version (not (equal version *binary-version*)))
     (list (format nil "nodecode stays at ~a; generation ~d ran ~a"
                   (short-version *binary-version*) number (short-version version))))
   (stays-lines steps)))

(defun restore-layer (directory sha number session-id)
  "Put the repository at DIRECTORY at SHA's tree -- the working tree and the
index -- and commit it as the restore: history kept, one commit forward."
  (multiple-value-bind (out code err) (git-in-layer directory "read-tree" "-m" "-u" sha)
    (unless (zerop code)
      (error "git read-tree in ~a: ~a" directory (if (plusp (length err)) err out)))
    (prog1 (git-commit-all directory (format nil "restored generation ~d -- session ~a" number session-id))
      ;; the memo the turn rows and the boot line read
      (setf *layer-revision* nil))))

;;; --- a generation laid beside the folder ------------------------------------
;;; A restore converges the folder in place; a bisect wants generation N
;;; whole somewhere else, with the folder it runs beside untouched. Each row
;;; is laid by the most exact thing that can put it back: the bytes its seal
;;; kept, a hub clone at its pin, a repository of its own at its commit, the
;;; release this binary runs. A manifest sealed before seals kept bytes holds
;;; rows nothing can put back -- a folder a checkout held, uncommitted edits,
;;; a plain folder since moved -- and each is laid as it stands today, said.

(defun place-row (row directory)
  "Put ROW's folder at DIRECTORY, where none stands, from what ROW carries
alone: the bytes its seal kept (LAY-TREE), or a hub clone at its pin
(CLONE-INSTALL). => DIRECTORY, or NIL for a row that carries neither."
  (cond ((getf row :bytes) (lay-tree (getf row :bytes) directory))
        ((equal "hub" (getf row :origin)) (clone-install (row-offer row) directory))))

(defun lay-commit (repository revision directory)
  "Put the tree REVISION names in the repository at REPOSITORY at DIRECTORY,
where none stands: git archive, unpacked. The repository moves nothing.
=> DIRECTORY."
  (let* ((root (sb-ext:native-namestring (uiop:ensure-directory-pathname directory)))
         (tar (format nil "~a.tar" (string-right-trim "/\\" root))))
    (when (probe-file root)
      (error "~a is there already" root))
    (ensure-directories-exist root)
    (with-cleanup ((ignore-errors (delete-file tar)))
      (flet ((run (argv what)
               (multiple-value-bind (out err status) (run-bounded argv :seconds 120)
                 (unless (eql 0 status)
                   (error "~a: ~a" what (trimmed (if (plusp (length err)) err out)))))))
        (run (list *git-program* "-C" (sb-ext:native-namestring repository)
                   "archive" "--format=tar" "-o" tar revision)
             (format nil "git archive ~a" (short-revision revision)))
        (run (list "tar" "-xf" tar "-C" root) "tar")))
    (uiop:ensure-directory-pathname directory)))

(defun lay-as-today (name root directory)
  "Put the folder NAME under ROOT, as it stands now, at DIRECTORY: a copy
through the kept bytes. => DIRECTORY, or NIL when ROOT holds no such folder."
  (when-let (live (and root (ignore-errors (truename (folder-under name root)))))
    (lay-tree (tree-digest live :keep t) directory)))

(defun build-generation (number target &key (root *cells-directory*))
  "Lay generation NUMBER's folder at TARGET -- a cells folder none stands at
yet -- beside the folder under ROOT, which nothing here moves. Each row is
laid by the most exact thing there is: what it carries (PLACE-ROW), a hub
already at its pin under ROOT copied rather than cloned, a repository of its
own at its commit (LAY-COMMIT), a release row as the release this binary
runs ships it; and a row nothing can put back, as it stands under ROOT
today. => (values TARGET NOTES): NOTES, one line a row not laid as NUMBER
had it."
  (let ((manifest (manifest-named number))
        (now (self-rows root))
        (target (uiop:ensure-directory-pathname target))
        (notes '()))
    (when (probe-file target)
      (error "~a is there already: a generation is built only where none stands" (namestring target)))
    (ensure-directories-exist target)
    (dolist (row (manifest-rows manifest))
      (let* ((name (getf row :name))
             (origin (getf row :origin))
             (directory (folder-under name target))
             (live (row-named name now)))
        (flet ((today (why)
                 (push (if (lay-as-today name root directory)
                           (format nil "~a as it stands today: ~a" name why)
                           (format nil "~a left out: ~a, and no folder of that name stands today" name why))
                       notes)))
          (cond
            ((getf row :bytes) (place-row row directory))
            ((equal origin "hub")
             (if (and live (equal "hub" (getf live :origin))
                      (equal (getf row :revision) (getf live :revision)))
                 (lay-as-today name root directory)
                 (place-row row directory)))
            ((equal origin "release")
             (if-let (offer (find-offer name))
               (lay-tree (tree-digest (offer-directory offer) :keep t) directory)
               (push (format nil "~a left out: this binary ships no folder of that name" name) notes)))
            ((not (equal origin "own"))
             (if (and live (equal (getf row :revision) (getf live :revision)))
                 (lay-as-today name root directory)
                 (today (format nil "generation ~d kept only its digest" number))))
            ((and live (getf live :checkout))
             (today (format nil "generation ~d kept none of its bytes, and the checkout it sits in ~
                                 does not version it" number)))
            ((not (and live (equal "own" (getf live :origin))))
             (today (format nil "generation ~d had it as a repository of its own, which is gone" number)))
            (t
             (handler-case
                 (progn (lay-commit (truename (folder-under name root)) (getf row :revision) directory)
                        (when (getf row :dirty)
                          (push (format nil "~a at ~a, without the uncommitted edits generation ~d had"
                                        name (short-revision (getf row :revision)) number)
                                notes)))
               (error (condition)
                 (ignore-errors (uiop:delete-directory-tree directory :validate t
                                                                      :if-does-not-exist :ignore))
                 (today (format nil "its commit ~a would not lay: ~a"
                                (short-revision (getf row :revision)) condition)))))))))
    (values target (nreverse notes))))

(defun restore-generation (number &key (root *cells-directory*) (session-id *generation-session-id*))
  "Converge the folder under ROOT to generation NUMBER: every step of its
RESTORE-PLAN carried out in order, then the restoring marker written for the
boot that runs it. Refuses, before anything moves, when any step is a
refusal. => LINES, what was done, in words."
  (bind (((steps _) (restore-plan number :root root)))
    (when-let (refused (append (step-refusals steps)
                               (mapcar (lambda (name)
                                         (format nil "~a has uncommitted edits; keep or discard them first" name))
                                       (step-edits steps))))
      (error "generation ~d cannot be restored: ~{~a~^; ~}" number refused))
    (let ((lines '())
          (from (ignore-errors (manifest-number (sealed-generation)))))
      (dolist (step steps)
        (destructuring-bind (verb name &rest detail) step
          (ecase verb
            (:keep (when (first detail) (push (first detail) lines)))
            (:stays)
            (:layer (destructuring-bind (sha dirty) detail
                      (restore-layer (folder-under name root) sha number session-id)
                      (push (format nil "~a at ~a~:[~;, the uncommitted edits of that time left aside~]"
                                    name (short-revision sha) dirty)
                            lines)))
            (:hub (let ((row (first detail)))
                    (uninstall-cell name :root root)
                    (place-row row (folder-under name root))
                    (push (format nil "~a cloned at ~a" name (short-revision (getf row :revision))) lines)))
            (:install (let ((row (first detail)))
                        (install-cell name :root root
                                            :offers (if (equal "hub" (getf row :origin))
                                                        (list (row-offer row))
                                                        (shipped-cells)))
                        (push (format nil "~a installed" name) lines)))
            (:uninstall (if (uninstall-cell name :root root)
                            (push (format nil "~a taken out" name) lines)
                            (push (format nil "~a left in place: it is yours, and generation ~d never had it"
                                          name number)
                                  lines))))))
      (setf lines (append (nreverse lines) (stays-lines steps)))
      (write-json-file (restoring-path) "to" number
                                        "from" (or from :null)
                                        "session" session-id
                                        "at" (iso-now)
                                        "lines" (coerce lines 'vector))
      lines)))

;;; --- uncommitted edits: kept as a commit, or set aside ----------------------
;;; Uncommitted edits in a folder that is a repository of its own -- the
;;; layer -- hold every restore (RESTORE-STEPS :EDITS), since nothing could
;;; put them back. The operator keeps them, as one commit the folder then
;;; proves as the next generation, or discards them into the repository's
;;; stash, where `git stash pop' finds them again. Either leaves the
;;; dot-directories out, as the scribe does (GIT-EXCLUDE-DOT-DIRECTORIES): a
;;; cell's ledger is no edit. A folder a checkout holds is the operator's to
;;; commit, and neither touches it.

(defun edit-paths (directory)
  "The uncommitted paths of the repository at DIRECTORY, `git status' lines,
its untracked dot-directories left out: what a keep commits."
  (multiple-value-bind (out code) (git-in-layer directory "status" "--porcelain")
    (when (eql code 0)
      (loop for line in (lines out)
            unless (and (uiop:string-prefix-p "?? ." line) (uiop:string-suffix-p line "/"))
              collect line))))

(defun edits-rows (rows)
  "The rows of ROWS (SELF-ROWS) whose uncommitted edits hold a restore: a
repository of its own, dirty."
  (remove-if-not (lambda (row)
                   (and (equal "own" (getf row :origin)) (getf row :dirty) (null (getf row :checkout))))
                 rows))

(defun edits-folder (name &optional (root *cells-directory*))
  "The folder NAME under ROOT, a directory, when it is a repository of its
own; else an error saying why its edits are not this one's to keep or
discard."
  (let* ((folder (and root (ignore-errors (truename (folder-under name root)))))
         (git (and folder (git-root folder))))
    (cond ((null folder) (error "no folder ~a under ~a" name (and root (namestring root))))
          ((null git) (error "~a is not under git: it has no edits to keep or discard" name))
          ((not (equal (namestring git) (namestring folder)))
           (error "~a is part of the checkout at ~a: commit or stash its edits there" name (namestring git)))
          (t folder))))

(defun keep-edits (name &key (root *cells-directory*) &aux (folder (edits-folder name root)))
  "Commit every uncommitted edit of the folder NAME as one commit, and prove
the folder it leaves: the generation that seals is one a restore can come
back to. => (values SHA VERDICT DETAIL): the commit, \"\" when nothing was
uncommitted, then what PROVE-SELF answered."
  (git-exclude-dot-directories folder)
  (let ((sha (git-commit-all folder +kept-edits-subject+ :notice (lambda (line) (error "~a" line)))))
    ;; the memo the turn rows and the boot line read
    (setf *layer-revision* nil)
    (multiple-value-call #'values sha (prove-self (folder-key)))))

(defun discard-edits (name &key (root *cells-directory*))
  "Set every uncommitted edit of the folder NAME aside into its
repository's stash, the folder back at its last commit, and prove it.
=> (values COUNT VERDICT DETAIL): how many paths went, then what PROVE-SELF
answered."
  (let* ((folder (edits-folder name root))
         (count (progn (git-exclude-dot-directories folder) (length (edit-paths folder)))))
    (when (plusp count)
      (multiple-value-bind (out code err)
          (apply #'git-in-layer folder `(,@(git-identity folder) "stash" "push"
                                         "--include-untracked" "-m" ,+discarded-edits-subject+))
        (unless (zerop code)
          (error "git stash in ~a: ~a" (namestring folder) (if (plusp (length err)) err out)))))
    (multiple-value-call #'values count (prove-self (folder-key)))))

;;; --- the boot that runs a restore -------------------------------------------

(defun record-generation-restored (marker &key (session-id *generation-session-id*))
  "The fact for the restore MARKER (restoring.json decoded) on the boot
session. => the event id, or NIL with no open store."
  (record-generation-fact +kind-generation-restored+ "organism.generation_restored"
                          (list :to (json-value marker :integer "to")
                                :from (or (json-value marker :integer "from") :null)
                                :session (or (json-value marker :string "session") :null)
                                :lines (json-array marker "lines"))
                          'record-generation-restored :session-id session-id))

(defun take-restoring-marker (&key (session-id *generation-session-id*))
  "The restore the last launch made, if one: the marker taken, its fact
recorded, its line answered for the boot to say. => the line, or NIL."
  (when-let (marker (json-object-file (restoring-path)))
    (ignore-errors (delete-file (restoring-path)))
    (record-generation-restored marker :session-id session-id)
    (let ((lines (coerce (json-array marker "lines") 'list)))
      (format nil "restored generation ~d~@[ (from ~d)~]~:[~;: ~{~a~^; ~}~]"
              (json-value marker :integer "to") (json-value marker :integer "from")
              lines lines))))

;;; --- the floor: generation 0, and the self that died before serving ---------
;;;
;;; The binary alone is generation 0: it loads and serves whatever the
;;; folder holds. Without that core, a folder whose load kills the process
;;; would have no image to boot into -- a unit would restart it onto the same
;;; death, and a shell's launch would die in front of its first frame.
;;; Autolith boots a pristine recovery core on a crash exit; here the binary
;;; is that core. A serve leaves a receipt before its load (BOOT-CELLS) and
;;; takes it once its listener is bound (gateway.lisp); a launch that finds a
;;; receipt whose pid is gone has found a self that died before serving,
;;; writes the death marker, and runs this launch pristine -- nothing loaded,
;;; no image followed, nothing kept -- with the sealed generations handed to
;;; the layer session (gateway.lisp HAND-FAILED-SELF). One death retries the
;;; folder at the launch after; a second in a row holds the floor until the
;;; key moves, which a restore does. `--pristine' stands here by hand.

(defvar *pristine-reason* nil
  "Why this process runs pristine, in words, or NIL.")

(defun pristine-reason ()
  (or *pristine-reason* (and *pristine* "asked for on the command line")))

(defun self-receipt-path (&optional (pid (sb-posix:getpid)))
  (merge-pathnames (format nil "booting.~d" pid) (generations-directory)))

(defun death-marker-path ()
  (merge-pathnames "failed.json" (generations-directory)))

(defun write-self-receipt (&optional (key (ignore-errors (folder-key))))
  "Leave this process's receipt before the folder loads: its pid and the
key it loads under. Taken once it serves (TAKE-SELF-RECEIPT); one left by a
pid that is gone is a self that died before serving."
  (ignore-errors
   (write-json-file (self-receipt-path) "pid" (sb-posix:getpid)
                                        "key" (or key :null)
                                        "at" (iso-now)))
  nil)

(defun take-self-receipt ()
  "This process serves: its receipt is no evidence of anything."
  (ignore-errors (delete-file (self-receipt-path)))
  nil)

(defun read-death-marker ()
  (json-object-file (death-marker-path)))

(defun write-death-marker (key deaths line)
  (write-json-file (death-marker-path) "key" key "deaths" deaths "line" line "at" (iso-now)))

(defun decide-pristine ()
  "Whether this launch runs generation 0, from the receipts and the marker
under the generations directory -- *PRISTINE* set when it does. => :BY-HAND
(already pristine: --pristine, or a parent's), :DEATH (a self died before
serving since the last launch), :HELD (two deaths in a row under the key the
folder still has), or NIL: the folder loads."
  ;; One death retries the folder at the launch after; a second in a row
  ;; holds the floor until the key moves, which a restore does. A receipt
  ;; whose key is not the folder's now is a self of a folder since mended:
  ;; taken, counted for nothing.
  (cond (*pristine* :by-hand)
        (*cells-directory*
         (let* ((key (ignore-errors (folder-key)))
                ;; the receipts whose pid is gone, (PATH . KEY): KEY NIL when one could not say
                (dead (loop for path in (dead-receipts (generations-directory))
                            collect (cons path (json-value (json-object-file path) :string "key"))))
                (marker (read-death-marker))
                (marked-key (json-value marker :string "key"))
                (deaths (or (json-value marker :integer "deaths") 0)))
           (dolist (entry dead) (ignore-errors (delete-file (car entry))))
           (cond
             ((and key (find-if (lambda (entry) (or (null (cdr entry)) (equal key (cdr entry)))) dead))
              (let* ((count (if (equal key marked-key) (1+ deaths) 1))
                     (line (format nil "the self loaded under key ~a died before it served~:[~; (~d times in a row)~]"
                                   (subseq key 0 (min 8 (length key))) (> count 1) count)))
                (write-death-marker key count line)
                (setf *pristine* t *pristine-reason* line)
                :death))
             ((and marker key (equal key marked-key))
              (when (>= deaths 2)
                (setf *pristine* t *pristine-reason* (json-value marker :string "line"))
                :held))
             (marker (ignore-errors (delete-file (death-marker-path))) nil))))))
