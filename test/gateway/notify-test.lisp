;;;; notify-test.lisp --- the desktop notice a turn's end pops.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the section's presence turns it on, which frames
;;;; pop a notice and which stay silent, the scope rule over whether a shell
;;;; received the frame, each host's default command, that a command which
;;;; cannot start is said once and never fails the publish, and -- once --
;;;; that a real turn through PUBLISH-EVENT reaches the stubbed spawn with the
;;;; session's directory in the body.

(in-package #:nodecode.test)

;;; A global, not a LET: the end-to-end test's frame arrives on the turn
;;; worker.
(defvar *notify-launches* (sb-concurrency:make-queue)
  "Argv lists the stubbed NOTIFY-LAUNCH received, oldest first.")

(defun notify-launches ()
  "The captured argv lists, oldest first."
  (sb-concurrency:list-queue-contents *notify-launches*))

(defmacro with-notify ((&key (config "{\"notify\": {}}") (host :linux)) &body body)
  "Run BODY over CONFIG as the shared config, with HOST the one default
commands are made for and NLE::NOTIFY-LAUNCH recording its argv instead of
spawning."
  `(with-temp-file (nle::*shared-config-path* :setf t :contents ,config)
     (with-saved-globals ((nle::*notified-turn* nil)
                          (nle::*notify-failure-said* nil)
                          (*notify-launches* (sb-concurrency:make-queue)))
       (with-stubbed-fdefinitions ((nle::notify-host (&rest args) ,host)
                                   (nle::notify-launch (argv)
                                     (sb-concurrency:enqueue argv *notify-launches*)
                                     nil))
         ,@body))))

(defun notify-op (type &rest keys)
  "CELL-FRAME-OP in the session the notify tests watch, s-notify."
  (apply #'cell-frame-op type (append keys '(:session "s-notify"))))

;;; --- which frames pop ------------------------------------------------------

(deftest notify-pops-on-a-turns-end (with-notify ())
  (nle::notify-turn-end (notify-op "turn.completed") t)
  (is (equal '(("notify-send" "--app-name=Nodecode" "Nodecode"
                "Ready for input — s-notify"))
             (notify-launches)))
  ;; The same turn republished pops nothing more, nor does any other frame.
  (nle::notify-turn-end (notify-op "turn.completed") t)
  (nle::notify-turn-end (notify-op "turn.started" :turn "t2") t)
  (nle::notify-turn-end (notify-op "turn.cancelled" :turn "t2"
                                   :payload (nlk:json-object "reason" "operator"))
                        t)
  (nle::notify-turn-end (list :session-id "s-notify"
                              :kind "item_delta"
                              :payload (nlk:json-object
                                        "turn_id" "t2"
                                        "delta" (nlk:json-object "type" "text" "text" "hi"))
                              :cursor nil
                              :durable-p nil)
                        t)
  (is (= 1 (length (notify-launches))))
  (nle::notify-turn-end (notify-op "turn.failed" :turn "t3"
                                   :payload (nlk:json-object
                                             "detail" (format nil "boom~%more")))
                        t)
  (is-carrying (body (fourth (second (notify-launches))))
    ("Turn failed — s-notify: boom" "a failed turn pops with the detail's first line")
    (:absent "more" "and only the first line")))

(deftest notify-is-off-without-its-section ()
  (dolist (config '("{}" "{\"notify\": {\"enabled\": false}}"))
    (with-notify (:config config)
      (nle::notify-turn-end (notify-op "turn.completed") t)
      (is (null (notify-launches)) config))))

(deftest notify-scope-attached-skips-unwatched-sessions ()
  (loop for (config launches) in '(("{\"notify\": {}}" 0) ("{\"notify\": {\"scope\": \"all\"}}" 1))
        do (with-notify (:config config)
             (nle::notify-turn-end (notify-op "turn.completed") nil)
             (is (= launches (length (notify-launches))) config))))

(deftest notify-command-template-substitutes-inside-words ()
  (with-notify (:config "{\"notify\": {\"command\": [\"x\", \"--message={body}\", \"{title}:{title}\", \" \"]}}")
    (nle::notify-turn-end (notify-op "turn.completed") t)
    (is (equal '(("x" "--message=Ready for input — s-notify" "Nodecode:Nodecode"))
               (notify-launches)))))

;;; --- each host's default ---------------------------------------------------

(deftest notify-windows-toast-carries-the-text-as-base64 ()
  ;; A failed turn's detail is the provider's text: inside the toast's script
  ;; it must stay data, so it travels as base64 and never as itself.
  (let* ((body "Turn failed — s: '); Remove-Item x #")
         (argv (nle::notify-default-command body :windows))
         (script (sb-ext:octets-to-string
                  (cl-base64:base64-string-to-usb8-array (car (last argv)))
                  :external-format :utf-16le)))
    (is (equal "-EncodedCommand" (car (last argv 2))))
    (is (every (lambda (c) (< (char-code c) 128)) (car (last argv))) "an ASCII command line")
    (is (search (nle::notify-base64 body) script) "the body, as base64")
    (is (not (search "Remove-Item" script)) "and never as itself")
    (is (null (member "-WindowStyle" argv :test #'equal)) "Windows Terminal stays up")))

(deftest notify-wsl-pops-the-windows-toast ()
  ;; Linux under WSL draws on Windows' desktop and has no notify-send. Its
  ;; kernel names Microsoft in its release, WSL 2's and WSL 1's alike; without
  ;; this check it would be treated as Linux and fail spawning notify-send.
  (is-table (in in2 want) (eq want (nle::notify-host in in2))
    (:linux "5.15.167.4-microsoft-standard-WSL2" :wsl) (:linux "4.4.0-19041-Microsoft" :wsl)
    (:linux "7.0.11-1-cachyos" :linux) (:darwin "24.6.0" :darwin))
  ;; The toast Windows pops, through the powershell.exe WSL puts on PATH.
  (is (equal (nle::notify-default-command "b" :windows)
             (nle::notify-default-command "b" :wsl))))

(deftest notify-mac-notice-carries-the-text-as-arguments ()
  ;; A Mac has no notify-send. The script is fixed words; the title and body
  ;; reach it as the run handler's arguments, so a detail full of quotes is
  ;; data and never AppleScript.
  (let ((body "Turn failed — s: \" & (do shell script \"x\") & \""))
    (with-notify (:host :darwin)
      (nle::notify-turn-end (notify-op "turn.failed" :payload (nlk:json-object "detail" body)) t)
      (let ((argv (first (notify-launches))))
        (is (equal "osascript" (first argv)))
        ;; The title and body are the last two words, and the script never
        ;; carries them.
        (is (equal (list "Nodecode" (format nil "Turn failed — s-notify: ~a" body))
                   (last argv 2)))
        (is (notany (lambda (word) (search "shell script" word)) (butlast argv 2)))))))

;;; --- a command that cannot start -------------------------------------------

(deftest notify-says-a-failed-command-once (with-notify ())
  ;; A box with no notify-send said so after every turn; once an image is
  ;; enough, and the publish that carried the frame never hears of it.
  (with-stubbed-fdefinition (nle::notify-launch (argv)
                             (error "spawning notify-send: No such file or directory"))
    (nle::notify-turn-end (notify-op "turn.completed" :turn "t1") t)
    (nle::notify-turn-end (notify-op "turn.completed" :turn "t2") t))
  (is (equal '("notify: spawning notify-send: No such file or directory")
             (remove-if-not (lambda (text) (search "notify:" text))
                            (mapcar #'first (nlk:notice-log))))))

;;; --- the real seam ---------------------------------------------------------

(deftest notify-end-to-end-through-a-real-turn (with-temp-store ())
  ;; One real turn against a stubbed provider: the terminal fact travels
  ;; PUBLISH-EVENT on the turn worker, a silent subscribed peer makes the
  ;; frame delivered, and the body names the session's working directory.
  (nlk:create-session :id "notify-e2e" :cwd "/tmp/notify-e2e/proj")
  (let* ((peer (list :notify-peer))
         (gws (progn (install-silent-peer peer)
                     (nle::find-or-create-gw-session "notify-e2e"))))
    ;; A shared config is a file a turn needs a model pinned in.
    (with-notify (:config (format nil "{\"general\": {\"default_model\": ~
                                        {\"provider\": ~s, \"model\": ~s}}, \"notify\": {}}"
                                  nle::*provider* nle::*model*))
      (nlk:with-cleanup ((nle::stop-gw-peer peer))
        (with-provider-stub (nil (wire-message "assistant" :content "done"))
          (bt2:with-lock-held ((nle::gw-session-lock gws)) (setf gws.peers (list peer)))
          (nle:submit "notify-e2e" "hello" :command-id "notify-e2e-cmd")
          (is (await () (not (nlk:active-turn-p "notify-e2e"))))
          (is (await () (notify-launches)))
          (is (equal '(("notify-send" "--app-name=Nodecode" "Nodecode"
                        "Ready for input — proj"))
                     (notify-launches))))))))
