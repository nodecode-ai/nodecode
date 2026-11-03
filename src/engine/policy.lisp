;;;; policy.lisp --- the agent's own policy: a git store whose adopted ref binds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; With `policy.self' on in the shared config, the agent owns the texts the
;;;; operator otherwise writes for it: its rules (AGENTS.md), its identity
;;;; (SOUL.md), and whatever else a caller keeps here by name. They live in
;;;; one git repository under the home, policy/, and what binds is the commit
;;;; the ref `adopted' names, never the files on disk: the working tree is the
;;;; agent's draft, HEAD a `draft' branch, and an edit there binds nothing
;;;; until a commit on `adopted' carries it. Every change is such a commit, so
;;;; each one is visible, versioned and undone with git.
;;;;
;;;; The store keeps the record and answers reads; who may move `adopted' --
;;;; the review that sorts a stricter edit from a looser one -- is its
;;;; caller's. A session can be pinned to a commit (POLICY-PIN), so a review
;;;; runs under the text before the change it judges. The sections a request
;;;; carries are built from here in context.lisp (POLICY-SECTIONS).

(in-package #:nodecode.evolved)

(defparameter +policy-adopted-ref+ "refs/heads/adopted"
  "The ref whose commit binds: what a request reads unless its session is pinned.")

(define-condition policy-error (simple-error) ()
  (:documentation "A store operation git refused: the reason is git's own words."))

(defvar *policy-lock* (sb-thread:make-mutex :name "policy store")
  "Held while the store is made, so two first requests make it once.")

(defvar *policy-texts* (make-hash-table :test 'equal :synchronized t)
  "(COMMIT . NAME) to the text NAME holds at COMMIT, or NIL: a commit never
changes, so each is read from git once.")

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

(defun policy-session-ref (session-id)
  "The ref that pins SESSION-ID's reads: refs/pins/<id>, the id kept to what
a ref name may hold."
  (format nil "refs/pins/~a"
          (substitute-if #\- (lambda (c) (not (or (alphanumericp c) (find c "-_")))) session-id)))

(defun policy-read-ref (&optional session-id)
  "The ref SESSION-ID reads its policy at: its pin when one stands, else `adopted'."
  (let ((pin (and session-id (policy-session-ref session-id))))
    (if (and pin (policy-ref-sha pin)) pin +policy-adopted-ref+)))

(defun policy-text (name &key ref session-id)
  "The text NAME holds at REF -- by default SESSION-ID's pin when one stands,
else `adopted' -- or NIL when the store, the ref or the file is not there.
Never the file on disk: a draft binds nothing."
  (let ((sha (policy-ref-sha (or ref (policy-read-ref session-id)))))
    (when sha
      (let* ((key (cons sha name))
             (hit (gethash key *policy-texts* :none)))
        (if (eq hit :none)
            (setf (gethash key *policy-texts*)
                  (multiple-value-bind (out code)
                      (policy-git nil "cat-file" "blob" (format nil "~a:~a" sha name))
                    (and (eql code 0) out)))
            hit)))))

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

(defun policy-adopt (files message)
  "Commit FILES onto `adopted' (POLICY-COMMIT): from the next request on, they
bind. The record is pushed after, when a record is kept off this machine
(POLICY-RECORD-PUSH, policy-record.lisp)."
  (prog1 (policy-commit +policy-adopted-ref+ files message)
    (when (fboundp 'policy-record-push)
      (ignore-errors (funcall 'policy-record-push)))))

(defun policy-pin (session-id commit)
  "Pin SESSION-ID's policy reads to COMMIT, so it runs under that text
whatever `adopted' becomes -- the review of a change runs under the text
before it. NIL for COMMIT unpins. Answers COMMIT."
  (let ((ref (policy-session-ref session-id)))
    (if commit
        (policy-git! nil "update-ref" ref commit)
        (policy-git nil "update-ref" "-d" ref))
    commit))

(defun policy-ensure ()
  "The store, made on first need, answering the commit `adopted' names. A new
store starts from the operator's SOUL.md, the identity the agent had until
now, and no rules of its own; its drafts start as what was adopted."
  (sb-thread:with-mutex (*policy-lock*)
    (let ((directory (policy-directory)))
      (unless (probe-file (merge-pathnames ".git/" directory))
        (ensure-directories-exist directory)
        (policy-git! nil "init" "-q")
        (policy-git! nil "symbolic-ref" "HEAD" "refs/heads/draft"))
      (or (policy-ref-sha +policy-adopted-ref+)
          (let* ((soul (ignore-errors (nlk:read-text (nlk:home "SOUL.md"))))
                 (files (and soul (plusp (length (nlk:trimmed soul))) (list (cons "SOUL.md" soul))))
                 (commit (policy-commit +policy-adopted-ref+ files
                                        (if files
                                            "policy: start from the operator's SOUL.md"
                                            "policy: start empty")
                                        :expect nil)))
            (loop for (name . text) in files
                  for path = (merge-pathnames name directory)
                  unless (probe-file path)
                    do (nlk:write-file-atomically path text))
            commit)))))
