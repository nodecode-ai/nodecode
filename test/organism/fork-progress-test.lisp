;;;; fork-progress-test.lisp --- a fork call's live progress: what the
;;;; gateway's watcher collects to tell the parent's shells how each fork
;;;; stands (organism/fork.lisp FORK-COLLECT-PROGRESS), and the note a fork's
;;;; call is listed by.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Real sessions, a real store and real turn workers, as fork-test.lisp has
;;;; them (WITH-FORK-CALL); nothing is stubbed but the provider.

(in-package #:nodecode.test)

(defun fork-progress-field (forks key)
  "KEY of each of FORKS, a frame's fork objects, in order."
  (map 'list (lambda (fork) (gethash key fork)) forks))

(deftest fork-progress-test-a-returned-call-says-how-its-forks-ended-once ()
  (with-fork-call ("p" turn)
    (with-provider-stub (context
                         (let ((task (fork-request-text context "[fork ")))
                           (wire-message "assistant"
                                         :content (format nil "**~a** done~%and more after it"
                                                          (if (and task (search "alpha" task)) "alpha" "beta")))))
      (let ((nle::*executing-tool-call-id* "call-fork"))
        (nlk:fork "the alpha task" "the beta task"))
      (let ((frames (nlk:fork-collect-progress)))
        (is (= 1 (length frames)) "one frame for the call, which has returned")
        (destructuring-bind (session payload) (first frames)
          (let ((forks (gethash "forks" payload)))
            (is (equal "p" session) "published to the session the call ran in")
            (is (equal "call-fork" (gethash "tool_call_id" payload)) "and drawn under the eval call that forked")
            (is (eq t (gethash "done" payload)) "its last frame says so")
            (is (equal '(1 2) (fork-progress-field forks "n")))
            (is (equal '("the alpha task" "the beta task") (fork-progress-field forks "label")) "each fork named by its task's first line")
            (is (equal '("completed" "completed") (fork-progress-field forks "status")))
            (is (equal '("alpha done" "beta done") (fork-progress-field forks "answer")) "each answer's first line, its Markdown marks off")
            (is (every #'integerp (fork-progress-field forks "ms")) "and the time each took"))))
      (is (null (nlk:fork-collect-progress)) "nothing after the last frame: the call has left the registry"))))

(deftest fork-progress-test-a-note-loses-only-its-fork-prefix ()
  (is (equal "grep dpad" (nlk::fork-progress-note ";; fork2 dpad recon: grep dpad")) "a forkN prefix tells siblings apart, and the tree already does")
  (is (equal "read the model" (nlk::fork-progress-note ";; fork 3: read the model")))
  (is (equal "forked view: keep it" (nlk::fork-progress-note ";; forked view: keep it")) "fork with no number is the note's own word")
  (is (equal "plain note" (nlk::fork-progress-note ";; plain note"))))
