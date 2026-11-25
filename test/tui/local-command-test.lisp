;;;; local-command-test.lisp --- gates for `!' in the composer: a command run where the shell runs (#505).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. Only a draft that starts with `!' is a command; `!' alone runs
;;;;      nothing and keeps the draft.
;;;;   2. Enter runs it here: its output fills the view as plain text, how it
;;;;      ended beside it, and nothing is sent to the session.
;;;;   3. Enter in the view puts the command and its output in the composer,
;;;;      its exit status when not 0.
;;;;   4. Ctrl+X stops a running command, and so does closing the view.

(in-package #:nodecode.test)

(defun await-local-command (model &key (seconds 10))
  "Tick MODEL's open `!' view until its command ended, and answer the
LOCAL-COMMAND, or NIL when it did not end within SECONDS."
  (let* ((dialog (nle::chat-model-dialog model))
         (bang (nle::picker-row-value (first (nle::picker-dialog-rows dialog)))))
    (loop repeat (* 20 seconds)
          until (nle::local-command-exit bang)
          do (sleep 0.05))
    (when (nle::local-command-exit bang)
      (nle::refresh-local-command-view model dialog bang)
      bang)))

(deftest draft-local-command-is-a-draft-that-starts-with-a-bang ()
  (is (equal "git status" (nle::draft-local-command "!git status")))
  (is (equal "ls -la" (nle::draft-local-command "!  ls -la ")))
  (is (equal "" (nle::draft-local-command "!")))
  (is (null (nle::draft-local-command " !ls")) "a leading blank sends it as a prompt")
  (is (null (nle::draft-local-command "hi !")))
  (is (null (nle::draft-local-command ""))))

(deftest local-command-runs-here-and-enter-attaches-it ()
  (with-frame-model (model 100 30 :session-id "bang")
    (let ((sent '()))
      (with-stubbed-fdefinition (nle::start-composer-turn (m text) (push text sent))
        (type-string model "!printf 'a\\n\\033[31mred\\033[0m\\n'; exit 3" :enter)
        (let ((dialog (nle::chat-model-dialog model)))
          (is (eq :local-command (nle::picker-dialog-kind dialog)))
          (is (equal "" (composer-value model)) "the draft left the composer")
          (let ((bang (await-local-command model))
                (shown (mapcar #'nle::picker-row-label (nle::picker-dialog-rows dialog))))
            (is bang "it ended")
            (is (equal '("a" "red") shown) "its output, colour codes removed")
            (is (search "exit 3" (nle::picker-dialog-context dialog)))))
        (press model :enter)
        (is (null (nle::chat-model-dialog model)) "Enter closes the view")
        (is (equal (format nil "$ printf 'a\\n\\033[31mred\\033[0m\\n'; exit 3~%a~%red~%[exit 3]")
                   (composer-value model)))
        (is (null sent) "nothing went to the session")
        (let ((recalled (first (nle::chat-model-prompt-history model))))
          (is (equal "!printf 'a\\n\\033[31mred\\033[0m\\n'; exit 3" recalled) "Up brings it back"))))))

(deftest local-command-bang-alone-runs-nothing ()
  (with-frame-model (model 100 30 :session-id "bang")
    (type-string model "!" :enter)
    (is (null (nle::chat-model-dialog model)))
    (is (equal "!" (composer-value model)))))

(deftest local-command-stops-on-ctrl-x-and-on-close ()
  (with-frame-model (model 100 30 :session-id "bang")
    (type-string model "!sleep 30" :enter)
    (press model #\x :mod nlt:+mod-ctrl+)
    (let ((bang (await-local-command model :seconds 5)))
      (is bang "Ctrl+X ended it")
      (is (search "stopped" (nle::picker-dialog-context (nle::chat-model-dialog model)))))
    (press model :escape)
    (is (null (nle::chat-model-dialog model)))
    (type-string model "!sleep 30" :enter)
    (let ((bang (nle::picker-row-value (first (nle::picker-dialog-rows (nle::chat-model-dialog model))))))
      (press model :escape)
      (is (null (nle::chat-model-dialog model)) "Esc closes the view")
      (loop repeat 100 until (nle::local-command-exit bang) do (sleep 0.05))
      (is (nle::local-command-exit bang) "and stops what it ran"))))
