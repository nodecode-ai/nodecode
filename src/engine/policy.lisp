;;;; policy.lisp --- the agent's own policy: files it edits, every edit a commit.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; With `policy.self' on in the shared config, the agent owns the texts the
;;;; operator otherwise writes for it: its rules (AGENTS.md), its identity
;;;; (SOUL.md) and its system prompt (SYSTEM.md). They are plain files in
;;;; policy/ under the home, and the agent changes one the way it changes any
;;;; file -- edit, write-file, sh -- with nothing to ask and nothing to wait
;;;; for: the edit binds from its next request.
;;;;
;;;; The folder is a git repository on its branch `adopted', and a request
;;;; reads it through POLICY-SYNC, which first commits whatever changed on
;;;; disk. So every edit is a commit, seen in `git log' and undone with `git
;;;; revert', and what a request carries is always a commit, never a file the
;;;; record does not hold. When config policy.record names a remote, each
;;;; commit is pushed there too (POLICY-RECORD-PUSH, policy-record.lisp).
;;;; The sections a request carries are built from here in context.lisp
;;;; (POLICY-SECTIONS), the system message in turn.lisp (POLICY-SYSTEM-PROMPT).

(in-package #:nodecode.evolved)

(defparameter +policy-adopted-ref+ "refs/heads/adopted"
  "The store's branch: its working tree is the policy, its commits the record.")

(define-condition policy-error (simple-error) ()
  (:documentation "A store operation git refused: the reason is git's own words."))

(defvar *policy-lock* (sb-thread:make-mutex :name "policy store")
  "Held while the store is made or committed, so two requests that read it at
once commit an edit once.")

(defvar *policy-texts* (make-hash-table :test 'equal :synchronized t)
  "(COMMIT . NAME) to the text NAME holds at COMMIT, or NIL, and (COMMIT .
:TREE) to the names its tree holds: a commit never changes, so each is read
from git once.")

(defun policy-self-p ()
  "Whether the agent owns its policy: `policy.self' is true in the shared config."
  (eq t (ignore-errors (config-get '("policy" "self")))))

(defun policy-directory ()
  "The store: policy/ under the home, so a profile has its own."
  (nlk:home "policy/"))

(defun policy-git (input &rest args)
  "Run git ARGS in the store, INPUT its stdin when given: (values OUT CODE
ERR), OUT and ERR without their trailing newlines, CODE -1 when git could not
run or ran past a minute."
  (handler-case
      (multiple-value-bind (out err code)
          (nlk:run-bounded (list* nlk::*git-program* "-C"
                                  (sb-ext:native-namestring (policy-directory)) args)
                           :seconds 60 :input input)
        (flet ((stripped (text) (string-right-trim '(#\Newline #\Return) (or text ""))))
          (if (integerp code)
              (values (stripped out) code (stripped err))
              (values "" -1 (format nil "git ~a ran past 60 s" (first args))))))
    (error (condition) (values "" -1 (princ-to-string condition)))))

(defun policy-git! (input &rest args)
  "POLICY-GIT, answering its output, or signalling POLICY-ERROR with git's
words when git fails."
  (multiple-value-bind (out code err) (apply #'policy-git input args)
    (unless (eql code 0)
      (error 'policy-error :format-control "policy store: git ~a: ~a"
                           :format-arguments (list (find-if (lambda (arg) (not (string= arg "-c")))
                                                            (remove-if (lambda (arg) (search "=" arg)) args))
                                                   (if (plusp (length err)) err out))))
    out))

(defun policy-ref-sha (ref)
  "The commit REF names in the store, or NIL: no store, no such ref."
  (multiple-value-bind (out code)
      (policy-git nil "rev-parse" "--verify" "-q" (format nil "~a^{commit}" ref))
    (and (eql code 0) (= 40 (length out)) out)))

(defun policy-name-p (name)
  "Whether NAME can be a file of the store: one plain name at its top, no
folder, no dot file, nothing git or a ref line could misread."
  (and (stringp name) (plusp (length name)) (char/= #\. (char name 0))
       (notany (lambda (c) (or (find c "/\\") (char< c #\Space))) name)))

(defun policy-cached (key thunk)
  "KEY's entry in *POLICY-TEXTS*, made by THUNK on first need."
  (let ((hit (gethash key *policy-texts* :none)))
    (if (eq hit :none)
        (setf (gethash key *policy-texts*) (funcall thunk))
        hit)))

(defun policy-text (name &key (ref +policy-adopted-ref+))
  "The text NAME holds at REF, the last commit by default, exactly as
committed, or NIL when the store, the ref or the file is not there."
  (let ((sha (if (and (= 40 (length ref)) (every (lambda (c) (digit-char-p c 16)) ref))
                 ref
                 (policy-ref-sha ref))))
    (when sha
      (policy-cached (cons sha name)
                     (lambda ()
                       (multiple-value-bind (out err code)
                           (ignore-errors
                            (nlk:run-bounded (list nlk::*git-program* "-C"
                                                   (sb-ext:native-namestring (policy-directory))
                                                   "cat-file" "blob" (format nil "~a:~a" sha name))
                                             :seconds 60))
                         (declare (ignore err))
                         (and (eql code 0) out)))))))

(defun policy-tree-entries (commit)
  "COMMIT's tree as ((NAME . LINE) ...), LINE the entry as `git mktree -z'
reads it back."
  (loop for line in (uiop:split-string (policy-git! nil "ls-tree" "-z" commit)
                                       :separator (string (code-char 0)))
        for tab = (position #\Tab line)
        when tab collect (cons (subseq line (1+ tab)) line)))

(defun policy-commit (ref files message &key (expect nil expect-p))
  "Commit FILES onto REF as MESSAGE and move REF to it, answering the new
commit; REF's own commit when FILES change nothing. FILES is ((NAME . TEXT)
...): TEXT the file's whole new text, NIL to remove it; the rest of REF's
tree is kept. REF moves only from the commit it named when this began --
EXPECT, when given (NIL: REF must not exist yet) -- so a writer that raced
another signals POLICY-ERROR rather than drop its change."
  (dolist (file files)
    (unless (policy-name-p (car file))
      (error 'policy-error :format-control "policy store: ~s is not a file name the store keeps"
                           :format-arguments (list (car file)))))
  (let* ((parent (if expect-p expect (policy-ref-sha ref)))
         (entries (and parent (policy-tree-entries parent))))
    (loop for (name . text) in files
          do (setf entries (remove name entries :key #'car :test #'string=))
             (when text
               (push (cons name (format nil "100644 blob ~a~c~a"
                                        (policy-git! text "hash-object" "-w" "--stdin") #\Tab name))
                     entries)))
    (let ((tree (policy-git! (format nil "~{~a~c~}"
                                     (loop for (nil . line) in entries collect line collect (code-char 0)))
                             "mktree" "-z")))
      (if (and parent (string= tree (policy-git! nil "rev-parse" (format nil "~a^{tree}" parent))))
          parent
          (let ((commit (apply #'policy-git! nil
                               (append (nlk::git-identity (policy-directory))
                                       (list "commit-tree" tree)
                                       (and parent (list "-p" parent))
                                       (list "-m" message)))))
            (policy-git! nil "update-ref" "-m" "policy commit" ref commit
                         (or parent (make-string 40 :initial-element #\0)))
            commit)))))

(defun policy-disk-texts ()
  "The store's files on disk as ((NAME . TEXT) ...): each plain file at its
top (POLICY-NAME-P) that reads as text, so .git and folders stay out."
  (loop for path in (uiop:directory-files (policy-directory))
        for name = (file-namestring path)
        for text = (and (policy-name-p name) (nlk:read-text path))
        when text collect (cons name text)))

(defun policy-shipped-prompt ()
  "The system prompt this release ships, *SYSTEM-PROMPT* (engine/turn, which
loads after this file, hence read by name)."
  (let ((symbol '*system-prompt*))
    (and (boundp symbol) (symbol-value symbol))))

(defun policy-write (name text)
  "Put TEXT in the store's file NAME on disk, or remove the file when TEXT is NIL."
  (let ((path (merge-pathnames name (policy-directory))))
    (if text
        (nlk:write-file-atomically path text)
        (when (probe-file path) (delete-file path)))))

(defun policy-sync (&optional message)
  "Commit what changed in the store on disk since its last commit, as MESSAGE
or a line naming the files, and answer the commit that now holds the policy.
An edit to SYSTEM.md writes SYSTEM.shipped beside it: the shipped prompt it
was written against, so a release that ships another can say so."
  (sb-thread:with-recursive-lock (*policy-lock*)
    (let* ((sha (policy-ensure))
           (disk (policy-disk-texts))
           (committed (policy-cached (cons sha :tree) (lambda () (mapcar #'car (policy-tree-entries sha)))))
           (changed (loop for name in (union (mapcar #'car disk) committed :test #'string=)
                          for text = (cdr (assoc name disk :test #'string=))
                          unless (equal text (policy-text name :ref sha))
                            collect (cons name text))))
      (if (null changed)
          sha
          (let ((prompt (assoc "SYSTEM.md" changed :test #'string=)))
            (when (and prompt (not (assoc "SYSTEM.shipped" changed :test #'string=)))
              (let ((shipped (and (cdr prompt) (policy-shipped-prompt))))
                (unless (equal shipped (policy-text "SYSTEM.shipped" :ref sha))
                  (policy-write "SYSTEM.shipped" shipped)
                  (push (cons "SYSTEM.shipped" shipped) changed))))
            (let ((commit (policy-commit +policy-adopted-ref+ changed
                                         (or message
                                             (format nil "policy: ~{~a~^, ~}"
                                                     (sort (mapcar #'car changed) #'string<)))
                                         :expect sha)))
              ;; the index follows the commit, so git status, diff and revert
              ;; in the store read it as clean
              (policy-git nil "reset" "-q")
              (when (fboundp 'policy-record-push-later)
                (ignore-errors (funcall 'policy-record-push-later)))
              commit))))))

(defun policy-adopt (files message)
  "Write FILES -- ((NAME . TEXT) ...), TEXT NIL to remove one -- into the
store and commit them as MESSAGE: an edit with a reason of its own. Any other
hand's edit binds the same, committed with a line naming its files by the
next POLICY-SYNC."
  (dolist (file files)
    (unless (policy-name-p (car file))
      (error 'policy-error :format-control "policy store: ~s is not a file name the store keeps"
                           :format-arguments (list (car file)))))
  (sb-thread:with-recursive-lock (*policy-lock*)
    (policy-ensure)
    (loop for (name . text) in files do (policy-write name text))
    (policy-sync message)))

(defun policy-ensure ()
  "The store, made on first need, answering the commit `adopted' names. A new
store starts from the operator's SOUL.md, the identity the agent had until
now, and no rules or prompt of its own."
  (sb-thread:with-recursive-lock (*policy-lock*)
    (let ((directory (policy-directory)))
      (unless (probe-file (merge-pathnames ".git/" directory))
        (ensure-directories-exist directory)
        (policy-git! nil "init" "-q")
        (policy-git! nil "symbolic-ref" "HEAD" +policy-adopted-ref+))
      (or (policy-ref-sha +policy-adopted-ref+)
          (let* ((soul (ignore-errors (nlk:read-text (nlk:home "SOUL.md"))))
                 (files (and soul (plusp (length (nlk:trimmed soul))) (list (cons "SOUL.md" soul))))
                 (commit (policy-commit +policy-adopted-ref+ files
                                        (if files
                                            "policy: start from the operator's SOUL.md"
                                            "policy: start empty")
                                        :expect nil)))
            (loop for (name . text) in files
                  unless (probe-file (merge-pathnames name directory))
                    do (policy-write name text))
            (policy-git nil "reset" "-q")
            commit)))))
