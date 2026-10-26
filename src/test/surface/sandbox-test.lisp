;;;; sandbox-test.lisp --- `nodecode --sandbox': the srt settings, the wrap,
;;;; the flag and the port a jailed gateway is told.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun sandbox-test-settings (&key secret-files policy (exists (constantly t))) "The settings for a fixed home, folder and binary."
  (nle::sandbox-settings :home "/h/.nodecode/" :folder "/w/proj/" :binary "/opt/nc/nodecode"
                         :srt-dir "/opt/srt/" :policy policy :secret-files secret-files
                         :provider-hosts '("openrouter.ai") :exists exists
                         :run-directory "/run/user/1000"))

(deftest sandbox-settings-carry-every-key-srt-requires ()
  (let ((settings (sandbox-test-settings)))
    (dolist (path '(("network" "allowedDomains") ("network" "deniedDomains")
                    ("filesystem" "denyRead") ("filesystem" "allowRead")
                    ("filesystem" "allowWrite") ("filesystem" "denyWrite")))
      (is (vectorp (apply #'nlk:json-value settings :array path)) (format nil "~{~a~^.~} is an array" path)))
    (is (find "openrouter.ai" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=) "a configured provider's host is reachable")
    (is (find "github.com" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=) "the cells repository is reachable")
    (is (find "/w/proj/" (nlk:json-value settings :array "filesystem" "allowWrite") :test #'string=) "the folder is writable")
    (is (find "/opt/nc/nodecode" (nlk:json-value settings :array "filesystem" "denyWrite") :test #'string=) "the binary that runs the jail is not")
    (is (find "/h/.nodecode/secrets/" (nlk:json-value settings :array "filesystem" "denyWrite") :test #'string=) "nor the secrets")
    (is (find "/run/user/1000" (nlk:json-value settings :array "filesystem" "denyRead") :test #'string=) "the runtime directory's sockets are out of reach")
    (is (null (nth-value 1 (nlk:json-value settings :any "network" "tlsTerminate"))) "no masked secret, no TLS termination")))

(deftest sandbox-settings-mount-only-what-exists ()
  (let ((settings (sandbox-test-settings :exists (lambda (path) (search ".ssh" path)))))
    (is (equalp (vector (nle::sandbox-expand "~/.ssh"))
                (nlk:json-value settings :array "filesystem" "denyRead")) "a deny-read entry that is not on disk is left out")))

(deftest sandbox-settings-mask-each-secret-toward-its-host ()
  (let* ((settings (sandbox-test-settings
                    :secret-files '(("/h/.nodecode/secrets/providers.openrouter.api_key" . "openrouter.ai"))))
         (masks (nlk:json-value settings :array "credentials" "files")))
    (is (= 1 (length masks)))
    (is (equal "mask" (nlk:json-value (aref masks 0) :string "mode")))
    (is (equal "^\\s*(\\S+)" (nlk:json-value (aref masks 0) :string "extract")) "only the key's span is masked, never the newline after it")
    (is (equal "deny" (nlk:json-value (aref masks 0) :string "onExtractNoMatch")) "a file with no key in it is denied, not shown")
    (is (equalp #("openrouter.ai") (nlk:json-value (aref masks 0) :array "injectHosts")) "the real key goes only to its provider")
    (is (hash-table-p (nlk:json-value settings :any "network" "tlsTerminate")) "a mask needs TLS termination")))

(deftest sandbox-settings-merge-the-operators-policy ()
  (let ((settings (sandbox-test-settings
                   :policy '(:allowed-domains ("pypi.org") :deny-read ("/srv/keys") :allow-write ("/data")))))
    (is (find "pypi.org" (nlk:json-value settings :array "network" "allowedDomains") :test #'string=))
    (is (find "/srv/keys" (nlk:json-value settings :array "filesystem" "denyRead") :test #'string=))
    (is (find "/data" (nlk:json-value settings :array "filesystem" "allowWrite") :test #'string=))
    (is (search "\"allowAllUnixSockets\":true" (nlk:encode-json-object settings)) "the bridge socket is allowed")))

(deftest sandbox-wrap-jails-the-serve-argv-whole ()
  (let ((argv (nle::sandbox-wrap '("/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
                                 :srt "/opt/srt/srt" :settings "/h/.nodecode/sandbox/srt-settings.json"
                                 :socket "/h/.nodecode/sandbox/run/gateway.sock" :port 41234
                                 :run "/h/.nodecode/sandbox/run/")))
    (is (equal '("env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1" "NODECODE_GATEWAY_PORT=41234"
                 "XDG_RUNTIME_DIR=/h/.nodecode/sandbox/run" "/opt/srt/srt" "--settings"
                 "/h/.nodecode/sandbox/srt-settings.json" "--" "sh" "-c")
               (subseq argv 0 12)) "env, srt, its settings and the -- that keeps the words apart")
    (is (equal '("/h/.nodecode/sandbox/run/gateway.sock" "41234" "/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
               (subseq argv 13)) "the socket and port as $0 $1, then the serve argv word for word")
    (is (search "exec \"$@\"" (nth 12 argv)) "the script execs the serve")))

(deftest sandbox-wrap-command-is-the-argv-itself-outside-a-sandbox ()
  (let ((nle::*sandbox-plan* nil))
    (is (equal '("nodecode" "serve") (nle::sandbox-wrap-command '("nodecode" "serve"))))))

(deftest sandbox-flag-is-a-shells ()
  (let ((nle::*sandbox-asked* nil))
    (nle::parse-launch-arguments '("--sandbox"))
    (is (eq t nle::*sandbox-asked*) "--sandbox is read"))
  (let ((nle::*sandbox-asked* nil))
    (is (search "shell's flag" (refusal-text nle::launch-usage-error
                                 (nle::parse-launch-arguments '("serve" "--sandbox")))) "a serve under --sandbox is refused"))
  (let ((nle::*sandbox-asked* nil))
    (is (search "shell's flag" (refusal-text nle::launch-usage-error
                                 (nle::parse-launch-arguments '("--sandbox" "rpc")))) "so is rpc")))

(deftest sandbox-gateway-port-from-the-environment ()
  (let ((saved (uiop:getenv "NODECODE_GATEWAY_PORT")))
    (unwind-protect
         (progn
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "41234")
           (is (eql 41234 (nle::gateway-port-from-environment)))
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "70000")
           (is (null (nle::gateway-port-from-environment)) "past 65535 is no port")
           (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" "x")
           (is (null (nle::gateway-port-from-environment)) "nor is a word"))
      (nlk::set-environment-variable "NODECODE_GATEWAY_PORT" (or saved "")))))

(deftest sandbox-settings-deny-the-home-and-read-back-only-the-work ()
  (let* ((settings (sandbox-test-settings))
         (deny (nlk:json-value settings :array "filesystem" "denyRead"))
         (allow (nlk:json-value settings :array "filesystem" "allowRead")))
    (is (find (uiop:native-namestring (user-homedir-pathname)) deny :test #'string=) "the user's home is read-denied whole")
    (dolist (path '("/w/proj/" "/h/.nodecode/" "/opt/nc/" "/opt/srt/"))
      (is (find path allow :test #'string=) (format nil "~a reads back" path)))
    (is (not (find (nle::sandbox-expand "~/.ssh") allow :test #'string=)) "and nothing of the credentials does")))

(deftest sandbox-hides-the-homes-own-secrets ()
  (let* ((settings (nle::sandbox-settings :home "/h/.nodecode/" :folder "/w/proj/" :binary "/opt/nc/nodecode"
                                          :exists (constantly t) :run-directory nil :deny-home nil
                                          :hidden (list "/h/.nodecode/auth.json" "/h/.nodecode/operator-token")))
         (deny (nlk:json-value settings :array "filesystem" "denyRead")))
    (is (find "/h/.nodecode/auth.json" deny :test #'string=) "auth.json is not read in the jail")
    (is (find "/h/.nodecode/operator-token" deny :test #'string=) "nor the gateway's token")
    (is (zerop (length (nlk:json-value settings :array "filesystem" "allowRead"))) "without the home deny nothing needs reading back")))
