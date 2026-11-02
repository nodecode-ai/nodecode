;;;; home-sessions-test.lisp --- Home does not project the session directory.
(in-package #:nodecode.test)

(defun seed-home-sessions (model)
  (setf (gethash model nle::*session-directory*)
        (list :entries
              (loop for (id title) in '(("new" "Latest session")
                                       ("reflection" "[experience] The turn above just ended.")
                                       ("old" "Third session"))
                    collect (nlk:json-object "id" id "title" title
                                            "updated_at" "2026-10-12T10:00:00.000Z"
                                            "working_directory" "/tmp" "message_count" 1)))))

(deftest home-does-not-show-recent-sessions (dolist (size '((48 24) (80 20) (80 24) (146 32))))
  (with-frame-model (model (first size) (second size))
    (seed-home-sessions model)
    (nle::render-state model)
    (let ((rows (coerce (nle::chat-model-line-plain model) 'list)))
      (is (nle::home-screen-p model))
      (is (row-carrying "generations" rows) "Home retains its orientation facts")
      (dolist (needle '("Latest session" "[experience]" "Third session" "sessions"))
        (is (not (row-carrying needle rows)) "the recent-sessions block is absent"))
      (is (not (loop for row in rows thereis
                     (loop for action in (nle::home-row-actions row)
                           thereis (eq :home-session (car (third action)))))) "Home has no stale session hitboxes"))))

(deftest home-other-running-sessions-do-not-animate (with-frame-model (model 80 24))
  (seed-home-sessions model)
  (rendering model (setf (gethash "new" (nle::session-activity-table model)) (nlk::iso-now)))
  (is (eq nil (nle::live-chrome-animating-p model)) "unshown sessions do not drive Home's frame clock")
  (nle:update model (nlt:make-tick-msg))
  (is (not (row-carrying "Latest session" (coerce (nle::chat-model-line-plain model) 'list)))))

(deftest home-session-picker-remains-accessible ()
  (dolist (gesture `((nle::execute-slash-command "sessions" "") (press #\s :mod ,nlt:+mod-ctrl+)))
    (with-frame-model (model 100 30)
      (seed-home-sessions model)
      (with-stubbed-fdefinitions
          ((nle::gateway-model-http-call (shell method path &optional payload)
             (is (equal "/api/gateway/sessions" path))
             (coerce (nle::cached-session-directory shell) 'vector))
           (nle::sessions-preview-arm (&rest args)))
        (apply (first gesture) model (rest gesture))
        (is-shape model.dialog (nle::picker-dialog-p is) (nle::picker-dialog-kind eq :sessions))
        (is (equal "frame-gate" (nle::chat-model-session-id model)) "opening the catalog does not switch sessions")
        (is (zerop (length (nle::editor-value (nle::chat-model-editor model)))))))))
