;;;; seams.lisp --- the settings an import runs on, the seams it writes through, and what is already here.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An import never writes a destination itself. Every piece lands through
;;;; the seam that already owns it -- NLE:CONFIG-SET for the shared config,
;;;; the auth writer for a key, a folder's own verb for what a folder owns
;;;; -- so an imported fact is indistinguishable from one the operator set,
;;;; and nothing here has to know a file format it does not own.
;;;;
;;;; The other half is the question every conflict rule asks: does this
;;;; organism already hold the fact? An import that clobbers what is here
;;;; is worse than one that skips, so the answer is read from the same
;;;; surfaces the organism reads.

(in-package #:nodecode.import)

;;; --- the settings ------------------------------------------------------------

(nlk:define-section ("import")
  (:guide "session_budget bounds how many conversations one plan carries, newest first")
  ("session_budget" :integer :default 200 :min 1
                    :doc "conversations one plan carries before the rest wait"))

(defvar *import* nil
  "The settings an import runs on in place of this organism's own
(IMPORT-SETTINGS) while bound; a test binds it.")

(defun import-settings ()
  "The settings an import runs on: :HOME, this organism's own folder, where
the instructions, skills and reports land, and the `import' section's
members, read now -- :SESSION-BUDGET, how many conversations one plan carries
over, newest first, before the rest wait for `/import <world> --only
sessions'."
  (or *import*
      (list* :home (nlk:home)
             (nlk:section-settings (nlk:find-section '("import"))
                                   (nlk:json-value (nle:read-shared-config) :object "import")))))

;;; --- what this organism already holds ---------------------------------------

(defun config-present-p (&rest path)
  "Whether the shared config already carries a member at PATH."
  (nth-value 1 (apply #'nlk:json-value (nle:read-shared-config) :any path)))

(defun auth-key-present-p (id)
  "Whether auth.json already holds a key for ID."
  (and (nle::auth-api-key (ignore-errors (nle::read-auth-file nle::*auth-file-path*)) id) t))

;;; --- the folders an item stands on -------------------------------------------

(defvar *ensure-cell* 'ensure-cell
  "The function that makes a shipped folder present, loaded and started;
a test stubs it and installs the folder itself.")

(defun ensure-cell (name &key fresh)
  "The shipped folder NAME installed and loaded in this process with the
folders it stands on, and every peripheral among them started -- for a
channel adapter, a library with no start of its own, that is its kit."
  ;; FRESH restarts a peripheral already started, so a section written just
  ;; now is the one it reads. => NAME's record.
  ;; One already up is answered as it is: without the early return, repeated
  ;; requests would reread every shipped folder's .asd.
  (nlk:when-let (record (and (not fresh) (nlk:find-cell name)))
    (when (and (null (nlk:cell-failures record))
               (or (not (eq (nlk:cell-kind record) :peripheral)) (nlk:cell-started-p record)))
      (return-from ensure-cell record)))
  (let ((names (nlk:offer-closure (list name) (nlk:shipped-cells))))
    (unless (member name names :test #'string=)
      (fail "this build ships no ~a folder" name))
    (nlk:when-let (absent (remove-if #'nlk:find-cell names))
      (dolist (folder absent) (nlk:install-cell folder))
      (apply #'nlk:load-cell absent))
    (dolist (folder names)
      (let ((record (or (nlk:find-cell folder) (fail "~a did not load" folder))))
        (when (nlk:cell-failures record)
          (fail "~a failed to load: ~a" folder (cdr (first (nlk:cell-failures record)))))
        (when (eq (nlk:cell-kind record) :peripheral)
          (when (or fresh (not (nlk:cell-started-p record)))
            (nle:restart-cells folder))
          (unless (nlk:cell-started-p record)
            (fail "~a did not start~@[: ~a~]" folder (nlk:cell-refusal record))))))
    (nlk:find-cell name)))

(defun need-cell (name &key fresh)
  (funcall *ensure-cell* name :fresh fresh))
