;;;; logout-all-shells-test.lisp --- one sign-out reaches every shell open on the home.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defmacro with-signed-in-home (() &body body)
  "BODY over a home signed in to nodecode.ai with a key and a machine link, its auth.json and
config set where the gateway's own threads read them too."
  `(with-temp-file (nle::*auth-file-path* :setf t)
     (with-temp-file (nle::*shared-config-path* :setf t)
       (setf nle::*account-usage* nil nle::*account-free-usage* nil nle::*account-login* nil)
       (nle::account-login-save (shasht:read-json +account-linked+))
       (nle::save-machine-link "00000000-0000-0000-0000-000000000002" "poll-secret-fixture")
       ,@body)))

(defun account-frames (frames &optional (account "signed_out"))
  "The model_changed frames in FRAMES that say the home's sign-in moved to ACCOUNT."
  (remove-if-not (lambda (frame)
                   (and (equal "model_changed" (frame-type frame))
                        (equal account (nlk:json-value frame :string "payload" "account"))))
                 frames))

(defmacro with-frames-told (((kinds) &key (key "logout-told")) &body body)
  "BODY with KINDS bound to a list that collects (KIND . PAYLOAD) of every frame the gateway
broadcasts meanwhile, newest first."
  `(let ((,kinds (list nil)))
     (nle:hook :frame ,key (lambda (op next)
                             (push (cons (getf op :kind) (getf op :payload)) (car ,kinds))
                             (funcall next op)))
     (nlk:with-cleanup ((nle:unhook :frame ,key))
       ,@body)))

(defun told-account (told)
  "The `account' each model_changed frame in TOLD (WITH-FRAMES-TOLD's) carried."
  (loop for (kind . payload) in (car told)
        when (and (equal kind "model_changed") (hash-table-p payload) (gethash "account" payload))
          collect (gethash "account" payload)))

(deftest logout-tells-every-shell-attached-to-the-organism ()
  ;; Two shells hold real sockets on one gateway, and the sign-out comes over the route a shell's
  ;; /logout posts. Both are told -- a shell that did not ask included -- and the words ride along.
  (with-gateway-session (port "s")
    (with-signed-in-home ()
      (let* ((ready (sb-concurrency:make-mailbox :name "shells attached"))
             (seen (list nil nil))
             (shells (loop for index below 2
                           collect (let ((index index))
                                     (nlk:spawn "logout-shell"
                                       (setf (nth index seen)
                                             (ws-collect-frames (format nil "ws://127.0.0.1:~d/gateway" port)
                                                                :subscribe '("s") :timeout 2
                                                                :send-commands (lambda (ws)
                                                                                 (declare (ignore ws))
                                                                                 (sb-concurrency:send-message ready index)))))))))
        ;; Both shells hold the session before the sign-out, which comes from neither socket.
        (is (loop repeat 2 always (sb-concurrency:receive-message ready :timeout 10)) "two shells attached")
        (gateway-http-status port :post "/api/gateway/account/logout" :body "{}" :token "test-token")
        (mapc #'bt2:join-thread shells)
        (dolist (frames seen)
          (is (= 1 (length (account-frames frames)))
              (format nil "each shell is told once: ~s" (mapcar #'frame-type frames))))
        (let ((told (first (account-frames (first seen)))))
          (is (search "Signed out of nodecode.ai" (nlk:json-value told :string "payload" "text")))
          (is (null (nlk:json-value told :any "payload" "session_id"))))
        (is (null (nle::account-credential)) "the key is gone from the home")))))

(deftest logout-with-nothing-to-sign-out-tells-no-shell ()
  (with-gateway-session (port "s")
    (with-temp-file (nle::*auth-file-path* :setf t)
      (with-temp-file (nle::*shared-config-path* :setf t)
        (with-frames-told ((told))
          (with-gateway-http (port :post "/api/gateway/account/logout" :body "{}" :status 200)
            (is (search "Not signed in" (nlk:json-value body :string "logout")))
            (is (not (gethash "signed_out" body))))
          (is (null (told-account told)) "a sign-out that took nothing clears no screen"))))))

(deftest logout-frame-clears-every-shell-onto-a-fresh-session ()
  ;; What each shell does with the frame: the meter goes, the screen clears onto a new session
  ;; the gateway holds, and the composer says the sign-out.
  (with-gateway-session (port "s")
    (let ((model (gateway-shell port "s" :ui t))
          (nle::*account-meter* (list :signed-in t :label "Founders" :value "41.1k" :tone :success)))
      (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
        (feed-wire model (nlk:encode-unsequenced-frame
                          "model_changed" (nlk:json-object "account" "signed_out"
                                                           "text" "Signed out of nodecode.ai"))))
      (is (null nle::*account-meter*) "the footer's meter is gone")
      (is (not (equal "s" model.session-id)) "the shell left the session it showed")
      (is (nlk:session-exists-p model.session-id) "onto a fresh one the gateway holds")
      (is (null (nle::transcript-rows model.state)) "the screen is clear")
      (is (search "Signed out of nodecode.ai" (nle::composer-status-text model.composer-status))))))

(deftest a-model-frame-without-an-account-clears-nothing ()
  ;; The frame an older organism or a model pick sends: the footer reads its facts again and the
  ;; shell stays where it is.
  (with-gateway-session (port "s")
    (let ((model (gateway-shell port "s" :ui t))
          (nle::*account-meter* (list :signed-in t :label "Founders")))
      (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
        (feed-wire model (nlk:encode-unsequenced-frame "model_changed" (nlk:json-object))))
      (is (equal "s" model.session-id))
      (is nle::*account-meter* "the meter stays"))))

(deftest logout-against-an-older-organism-signs-out-here-and-says-so ()
  ;; A 0.1.9 organism has no sign-out route and answers `404 (not_found): unknown route' -- what
  ;; Thomas's Windows shells showed. The shell signs the home out itself, clears its own screen
  ;; and says what still holds the account, instead of the bare 404.
  (with-gateway-session (port "s")
    (with-signed-in-home ()
      (let ((model (gateway-shell port "s" :ui t))
            (real (fdefinition 'nle::gateway-model-http-call)))
        (with-stubbed-fdefinitions ((nle::send-subscribe (ws session-ids) nil)
                                    (nle::gateway-model-http-call (shell method path body &rest keys)
                                      (if (equal path "/api/gateway/account/logout")
                                          (error 'nle::gateway-http-error :status 404 :code "not_found"
                                                                          :detail "unknown route")
                                          (apply real shell method path body keys))))
          (nle::slash-logout-op model ""))
        (is (null (nle::account-credential)) "the key is gone from the home all the same")
        (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "machine_link")))
        (let ((said (nle::composer-status-text model.composer-status)))
          (is (search "Signed out of nodecode.ai" said))
          (is (search "older build" said) "it says why the other shells still show the account")
          (is (not (search "404" said)) "no bare route error"))
        (is (not (equal "s" model.session-id)) "this shell's screen clears all the same")))))

(deftest nodecode-logout-goes-through-the-running-organism ()
  ;; `nodecode logout' in a terminal of its own, with shells open: the organism signs out, so
  ;; every shell is told; with none serving, the home is signed out in place.
  (with-gateway-session (port "s")
    (with-signed-in-home ()
      (with-frames-told ((told))
        (let ((said (nle::account-logout-text :port port :token "test-token")))
          (is (search "Signed out of nodecode.ai" said))
          (is (not (search "did not answer" said)) "the organism answered"))
        (is (equal '("signed_out") (told-account told)) "the organism told its shells"))
      (is (null (nle::account-credential)))))
  (with-signed-in-home ()
    (is (search "Signed out of nodecode.ai" (nle::account-logout-text :port nil)) "no organism: signed out here")
    (is (null (nle::account-credential))))
  (with-signed-in-home ()
    (let ((said (nle::account-logout-text :port (temp-gateway-port))))
      (is (search "Signed out of nodecode.ai" said) "an organism that does not answer: signed out here")
      (is (search "did not answer" said) "and it says the shells keep the account"))
    (is (null (nle::account-credential)))))

#-win32
(deftest a-shell-starts-the-organism-on-the-release-installed-last ()
  ;; A shell still open from an older release starts the organism through the `current' link,
  ;; never from its own folder: on Windows an organism cannot exec onto `current', so its restart
  ;; ends it and a shell starts the next one, and an old shell's own build won it back every
  ;; time -- 0.1.10 shells on a 0.1.9 organism, every new route a 404.
  (with-temp-directory (root "release-layout")
    (let ((old (merge-pathnames "8c8dfdc47/nodecode" root))
          (new (merge-pathnames "1452d5350/nodecode" root))
          (nle::*serve-program* nil))
      (dolist (file (list old new))
        (with-open-file (out (ensure-directories-exist file) :direction :output :if-exists :supersede)
          (write-line "#!/bin/sh" out)))
      (sb-posix:symlink "1452d5350" (uiop:native-namestring (merge-pathnames "current" root)))
      (with-stubbed-fdefinition (nlk::origin-pathname () old)
        (let ((argv (nle::serve-command #p"/tmp/logout-layout/history.db")))
          (is (equal (uiop:native-namestring (merge-pathnames "current/nodecode" root)) (first argv)))
          (is (equal '("serve" "--store") (subseq argv 1 3))))))))

(deftest a-sign-in-tells-every-shell-to-read-its-meter-again ()
  ;; The organism's /login settles on a thread of its own; the shells hear it the way they hear
  ;; a sign-out, session-less on model_changed, and read their footers again.
  (with-temp-file (nle::*auth-file-path* :setf t)
    (with-temp-file (nle::*shared-config-path* :setf t)
      ;; Set, not bound: the sign-in settles on a thread of its own, which reads the global.
      (setf nle::*account-login* nil nle::*account-usage* nil nle::*account-free-usage* nil)
      (with-stubbed-fdefinition (nle::account-usage-request (url key timeout)
                                  (declare (ignore url key timeout))
                                  (values (shasht:read-json +account-usage-credit+) 200))
        (with-frames-told ((told) :key "sign-in-told")
          (nle::account-login-start "api"
                                    :open (constantly t)
                                    :request (nlk:lambda-ignoring (method path body poll timeout)
                                               (values (shasht:read-json (if (eq method :post) +account-signin-claim+ +account-linked+))
                                                       200)))
          (loop repeat 100 while (eq :waiting (getf nle::*account-login* :state)) do (sleep 0.05))
          (is (eq :signed-in (getf nle::*account-login* :state)) (getf nle::*account-login* :text))
          (is (equal '("signed_in") (told-account told)))
          (is (search "Signed in to nodecode.ai"
                      (gethash "text" (cdr (find "model_changed" (car told) :key #'car :test #'equal))))))))))
