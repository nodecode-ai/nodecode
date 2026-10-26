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
                 "XDG_RUNTIME_DIR=/h/.nodecode/sandbox/run" "XDG_CACHE_HOME=/h/.nodecode/sandbox/cache"
                 "/opt/srt/srt" "--settings" "/h/.nodecode/sandbox/srt-settings.json" "--" "sh" "-c")
               (subseq argv 0 13)) "env, srt, its settings and the -- that keeps the words apart")
    (is (equal '("/h/.nodecode/sandbox/run/gateway.sock" "41234" "/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
               (subseq argv 14)) "the socket and port as $0 $1, then the serve argv word for word")
    (is (search "exec \"$@\"" (nth 13 argv)) "the script execs the serve")))

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

(deftest sandbox-settings-deny-writing-what-the-launcher-reads ()
  (let ((deny (nlk:json-value (sandbox-test-settings) :array "filesystem" "denyWrite"))
        (allow (nlk:json-value (sandbox-test-settings) :array "filesystem" "allowWrite")))
    (dolist (path '("/h/.nodecode/config.jsonc" "/h/.nodecode/sandbox.jsonc" "/h/.nodecode/sandbox/jailed.json" "/h/.nodecode/secrets/"))
      (is (find path deny :test #'string=) (format nil "~a is the launcher's, not the jail's" path)))
    (is (notany (lambda (path) (search ".cache/nodecode" path)) allow) "the machine's shared cache is not the jail's to write")))

(defun sandbox-test-home ()
  "A scratch home with one cells/ folder under git, one commit in it."
  (let ((home (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "nc-sbx-~36r/" (random (expt 36 8))) (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "cells/vise/" home))
    (with-open-file (o (merge-pathnames "cells/vise/a.lisp" home) :direction :output :if-exists :supersede)
      (write-line "(defun a () 1)" o))
    (uiop:run-program (list "sh" "-c" "git init -q && git add -A && git -c user.email=t@t -c user.name=t commit -qm one")
                      :directory (merge-pathnames "cells/vise/" home))
    home))

(deftest sandbox-mark-holds-its-first-snapshot ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (is (null (nle::sandbox-jailed-refusal home :loads t)) "an unmarked home launches anywhere")
           (nle::sandbox-mark home)
           (let ((first (uiop:read-file-string (nle::sandbox-mark-path home))))
             (with-open-file (o (merge-pathnames "cells/vise/b.lisp" home) :direction :output :if-exists :supersede)
               (write-line "(defparameter *b* (sb-ext:run-program \"/bin/true\" nil))" o))
             (uiop:run-program (list "sh" "-c" "git add -A && git -c user.email=t@t -c user.name=t commit -qm escape")
                               :directory (merge-pathnames "cells/vise/" home))
             (nle::sandbox-mark home)
             (is (equal first (uiop:read-file-string (nle::sandbox-mark-path home))) "a later sandboxed launch never moves the mark")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-mark-refuses-an-unjailed-load ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (search "has run under --sandbox" (nle::sandbox-jailed-refusal home :loads t)) "a plain launch, serve or rpc is refused")
           (is (null (nle::sandbox-jailed-refusal home :loads t :asked t)) "--sandbox runs")
           (is (null (nle::sandbox-jailed-refusal home :loads t :sandboxed "1")) "the organism inside the jail runs")
           (is (null (nle::sandbox-jailed-refusal home :loads nil)) "a verb that loads no folder runs"))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-review-names-what-the-jail-wrote ()
  (let ((home (sandbox-test-home)))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (search "nothing: cells/ is as the mark found it" (nle::sandbox-review-text home)) "an untouched home has nothing to show")
           (with-open-file (o (merge-pathnames "cells/vise/b.lisp" home) :direction :output :if-exists :supersede)
             (write-line "(defun b () 2)" o))
           (uiop:run-program (list "sh" "-c" "git add -A && git -c user.email=t@t -c user.name=t commit -qm escape-commit")
                             :directory (merge-pathnames "cells/vise/" home))
           (ensure-directories-exist (merge-pathnames "cells/planted/" home))
           (let ((text (nle::sandbox-review-text home)))
             (is (search "cells/vise: 1 commit since the mark" text) "a new commit in the layer is counted")
             (is (search "escape-commit" text) "and named")
             (is (search "b.lisp" text) "with the files it touches")
             (is (search "cells/planted: a new folder" text) "a folder the jail planted is named")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(deftest sandbox-release-needs-yes-and-the-outside ()
  (let* ((home (sandbox-test-home))
         (nlk::*home* home)
         (saved (uiop:getenv "NODECODE_SANDBOXED")))
    (unwind-protect
         (progn
           (nle::sandbox-mark home)
           (is (= 2 (nth-value 1 (nle::sandbox-answer '("release")))) "release asks for --yes")
           (nlk::set-environment-variable "NODECODE_SANDBOXED" "1")
           (is (= 1 (nth-value 1 (nle::sandbox-answer '("release" "--yes")))) "never from inside the jail")
           (nlk::set-environment-variable "NODECODE_SANDBOXED" "")
           (is (probe-file (nle::sandbox-mark-path home)) "the mark still stands")
           (is (zerop (nth-value 1 (nle::sandbox-answer '("release" "--yes")))) "from outside, with --yes, it lifts")
           (is (null (probe-file (nle::sandbox-mark-path home))) "and the mark is gone"))
      (nlk::set-environment-variable "NODECODE_SANDBOXED" (or saved ""))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

