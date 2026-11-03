;;;; policy-record-test.lisp --- dropped stays dropped; the record is pushed, never forced.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun policy-test-git (dir &rest args)
  "Git ARGS in DIR, its trimmed stdout; signals on a failure."
  (string-trim '(#\Space #\Newline)
               (uiop:run-program (list* "git" "-C" (uiop:native-namestring dir) args)
                                 :output :string :error-output :string)))

(defun policy-test-adopt (store files message)
  "Commit FILES ((NAME . TEXT) ...) on STORE's adopted ref, as the store's
POLICY-ADOPT does, without its working tree."
  (declare (ignore store))
  (nle::policy-adopt files message))

(defmacro with-policy-store ((store) &body body)
  "BODY with STORE a fresh git repo whose adopted ref holds AGENTS.md and SOUL.md."
  `(let ((,store (uiop:ensure-directory-pathname (nlk:home "policy/"))))
     (ensure-directories-exist ,store)
     (policy-test-git ,store "init" "-q" "-b" "draft")
     (policy-test-git ,store "config" "user.name" "Policy Test")
     (policy-test-git ,store "config" "user.email" "policy@test.invalid")
     (dolist (file '(("AGENTS.md" . "ask before deleting
") ("SOUL.md" . "I am the test agent.
")))
       (with-open-file (out (merge-pathnames (car file) ,store) :direction :output :if-exists :supersede)
         (write-string (cdr file) out)))
     (policy-test-git ,store "add" "-A")
     (policy-test-git ,store "commit" "-q" "-m" "seed")
     (policy-test-git ,store "update-ref" nle::+policy-adopted-ref+ "HEAD")
     ,@body))

(deftest policy-record-drop-leaves-what-binds-and-the-drafts-alone ()
  (with-policy-store (store)
    (let* ((loosen '(("AGENTS.md" . "delete without asking
")))
           (tree (policy-test-git store "rev-parse" "adopted^{tree}"))
           (drop (nle::policy-record-drop store loosen :reason "flips 3 refusals" :id "p-1")))
      (is (equal tree (policy-test-git store "rev-parse" "adopted^{tree}")) "the adopted tree is unchanged")
      (is (equal drop (policy-test-git store "rev-parse" "adopted")) "the drop is the adopted tip")
      (is (search "Policy-Dropped-Id: p-1" (policy-test-git store "log" "-1" "--format=%B" "adopted")))
      (is (equal "ask before deleting" (string-trim '(#\Newline) (uiop:read-file-string (merge-pathnames "AGENTS.md" store)))) "the working tree, the agent's drafts, is untouched")
      (is (equal drop (nle::policy-dropped-p store loosen)) "the same change is refused")
      (is (null (nle::policy-dropped-p store '(("SOUL.md" . "I am careful.
")))) "another change is not"))))

(defun policy-test-rules (&key (second "ask before deleting") (last "rule 10"))
  (format nil "rule 1~%~a~%rule 3~%rule 4~%rule 5~%rule 6~%rule 7~%rule 8~%rule 9~%~a~%" second last))

(deftest policy-record-refuses-a-dropped-change-on-a-later-base ()
  ;; The same edit after an adopt elsewhere in the file moved the base: the
  ;; hunk is the one dropped, the file's text as a whole is new.
  (with-policy-store (store)
    (policy-test-adopt store `(("AGENTS.md" . ,(policy-test-rules))) "ten rules")
    (let ((drop (nle::policy-record-drop
                 store `(("AGENTS.md" . ,(policy-test-rules :second "delete without asking"))))))
      (policy-test-adopt store `(("AGENTS.md" . ,(policy-test-rules :last "rule 10, stricter"))) "far edit")
      (is (equal drop (nle::policy-dropped-p
                       store `(("AGENTS.md" . ,(policy-test-rules :second "delete without asking"
                                                                  :last "rule 10, stricter"))))) "the same hunk on a new base matches by patch id"))))

(deftest policy-record-refuses-the-same-text-reached-by-other-steps ()
  (with-policy-store (store)
    (let ((drop (nle::policy-record-drop store '(("AGENTS.md" . "delete without asking
")))))
      (policy-test-adopt store '(("AGENTS.md" . "ask first, always
")) "stricter")
      (is (equal drop (nle::policy-dropped-p store '(("AGENTS.md" . "delete without asking
")))) "the dropped text, from another base, matches by its blob"))))

(deftest policy-record-pushes-and-never-forces ()
  (with-policy-store (store)
    (let ((remote (uiop:ensure-directory-pathname (nlk:home "record.git/"))))
      (ensure-directories-exist remote)
      (policy-test-git remote "init" "-q" "--bare")
      (is (eq :no-remote (nle::policy-record-push store :remote nil)))
      (is (eq :pushed (nle::policy-record-push store :remote (uiop:native-namestring remote))))
      (is (equal (policy-test-git store "rev-parse" "adopted")
                 (policy-test-git remote "rev-parse" "policy/adopted")))
      ;; Rewrite the adopted history here: a sibling of the tip replaces it.
      (let ((pushed (policy-test-git store "rev-parse" "adopted"))
            (sibling (nle::policy-record-git-line store (list "commit-tree" "adopted^{tree}" "-m" "rewritten"))))
        (policy-test-git store "update-ref" nle::+policy-adopted-ref+ sibling)
        (is (eq :diverged (nle::policy-record-push store :remote (uiop:native-namestring remote))))
        (is (equal pushed (policy-test-git remote "rev-parse" "policy/adopted")) "the record kept its history")
        (is (assoc nle::*policy-record-notice-key* (nlk:standing-notices) :test #'string=) "a diverged record stands a notice")))))
