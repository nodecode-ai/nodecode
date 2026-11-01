;;;; control-test.lisp --- the operator's control routes: how the organism
;;;; runs, its log, its keys, its cells, the profiles.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every variable a route reads on the gateway's thread is SETF here, never LET:
;;;; the store path, the shared config, the listing cache, the unit runner.

(in-package #:nodecode.test)

(deftest control-service-says-how-the-organism-runs (with-temp-gateway (port))
  ;; No unit folder and no shell: the test image serves headless, and a
  ;; developer SBCL has no binary to run itself again from.
  (with-gateway-http (port :get "/api/gateway/service" :status 200)
    (is-shape body ((:string "runs") "serve") ((:integer "pid") eql (sb-posix:getpid)))
    (is (search "history.db" (nlk:json-value body :string "store")) "the store it serves")
    (is (plusp (nlk:json-value body :integer "store_bytes")) "and its size")
    (is (eq t (nlk:json-value body :any "stop" "able")) "a serve can stop itself")
    (is (not (nlk:json-value body :any "restart" "able")) "but not run a binary it has not got")
    (is (search "installed binary" (nlk:json-value body :string "restart" "text")) "and says why"))
  (is-route (port :get "/api/gateway/service" :token nil) 401 "the operator's alone"))

(deftest control-stop-refuses-a-shells-organism-and-hands-a-serve-its-exit
    (with-temp-gateway (port))
  (with-saved-globals ((nle::*fronted* t) (nle::*serve-next* nil))
    ;; The organism a shell started stops with the operator's terminal, which
    ;; a page never takes.
    (is-route (port :post "/api/gateway/stop") 400 "a shell's organism" "refused")
    (is (null nle::*serve-next*))
    (setf nle::*fronted* nil)
    (with-gateway-http (port :post "/api/gateway/stop" :status 200 :says "a headless serve's")
      (is (search "exits" (nlk:json-value body :string "text")) "the answer says what happens"))
    ;; The answer leaves before the owner thread is told.
    (is-eventually (eq :stop nle::*serve-next*) "the owner is told to let the store go and exit")
    (bt2:wait-on-semaphore nle::*serve-wake* :timeout 0)))

(deftest control-restart-under-the-unit-goes-through-systemd (with-temp-gateway (port))
  (with-scripted-systemd (fake)
    (nlk:gateway-on :program +test-binary+)
    ;; This process is the unit's own.
    (setf (fake-unit-pid (gethash (nlk:gateway-unit-name) (fake-systemd-units fake)))
          (sb-posix:getpid))
    (with-gateway-http (port :get "/api/gateway/service")
      (is-shape body ((:string "runs") "unit") ((:string "unit") (nlk:gateway-unit-name))))
    (setf (fake-systemd-calls fake) '())
    (is-route (port :post "/api/gateway/restart") 200 "a restart is asked of the unit")
    (is-eventually (member "restart" (unit-verbs fake) :test #'equal)
                   "and systemd restarts it")))

(deftest control-service-under-launchd-knows-the-launchers-child-as-the-unit
    (with-temp-gateway (port))
  (with-scripted-launchd (fake)
    (nlk:gateway-on :program +test-binary+)
    ;; This process is the organism the agent's launcher runs: its one child.
    (setf (fake-agent-pid (gethash (nlk:gateway-unit-name) (fake-launchd-agents fake)))
          (- (sb-posix:getpid) 1000))
    (with-gateway-http (port :get "/api/gateway/service")
      (is (equal "unit" (nlk:json-value body :string "runs")) "the agent's own organism, not a serve"))
    (setf (fake-launchd-calls fake) '())
    (is-route (port :post "/api/gateway/restart") 200 "a restart is asked of the agent")
    (is-eventually (member "kickstart" (agent-verbs fake) :test #'equal)
                   "and launchd restarts it")))

#+(or linux darwin)
(deftest control-log-finds-the-file-a-descriptor-writes-to ()
  ;; A launchd agent's log, a severed shell's, a serve's output sent to a file.
  (with-temp-file (path :type "log" :contents "")
    (with-open-file (out path :direction :output :if-exists :append)
      (is (equal (uiop:native-namestring (truename path))
                 (nle::descriptor-path (sb-sys:fd-stream-fd out)))))
    ;; Standard output a device, standard error sent to a file: the file.
    (let ((file (uiop:native-namestring path)))
      (with-stubbed-fdefinition (nle::descriptor-path (fd) (if (= fd 2) file "/dev/null"))
        (is (equal (list :file file) (multiple-value-list (nle::log-source))))))))

(deftest control-log-file-reads-its-tail-then-follows-from-a-cursor ()
  (with-temp-file (path :type "log" :contents (format nil "one~%two~%three~%"))
    (multiple-value-bind (lines cursor) (nle::file-log-lines path 2 nil)
      (is (equal '("two" "three") lines) "the last lines")
      (is (= 14 cursor) "and where they end")
      (alexandria:write-string-into-file (format nil "four~%half") path :if-exists :append)
      (multiple-value-bind (more next) (nle::file-log-lines path 10 cursor)
        (is (equal '("four") more) "what came after, without the line still being written")
        (is (= 19 next))))
    (write-temp-file path (format nil "fresh~%"))
    (is (equal '("fresh") (nle::file-log-lines path 10 19)) "a file cut short is read anew")))

(deftest control-log-journal-follows-by-its-own-cursor (let ((asked '())))
  (with-saved-globals ((nlk:*service-manager* :systemd)
                       (nlk:*unit-runner*
                        (lambda (argv)
                          (push argv asked)
                          (values (format nil "t1 box nodecode[1]: serving~%-- cursor: s=ab;i=2~%") "" 0))))
    (multiple-value-bind (lines cursor) (nle::journal-log-lines 50 nil)
      (is (equal '("t1 box nodecode[1]: serving") lines) "the entries, the cursor line taken out")
      (is (equal "s=ab;i=2" cursor))
      (is (search '("--lines" "50") (first asked) :test #'equal)))
    (nle::journal-log-lines 50 "s=ab;i=2")
    (is (search '("--after-cursor" "s=ab;i=2") (first asked) :test #'equal) "the next read follows it")))

(deftest control-logs-route-answers-the-log-or-why-there-is-none (with-temp-gateway (port))
  ;; Where this image's output goes is the runner's to say: a file, or not.
  (with-gateway-http (port :get "/api/gateway/logs?lines=5")
    (if (= 200 status)
        (progn (is (<= (length (nlk:json-value body :array "lines")) 5))
               (is (stringp (nlk:json-value body :string "cursor"))))
        (is (equal "no_log" (nlk:json-value body :string "error" "code")) "no file, and says so"))))

(deftest control-keys-save-and-take-back-through-the-connect-writer (with-temp-gateway (port))
  ;; The key check is the connect writer's gate and has tests of its own
  ;; (PROVIDER-KEY-CHECK-*): nothing listens on port 9, which it refuses.
  (with-temp-file (config :type "jsonc"
                          :contents "{\"providers\": {\"lane-k\": {\"base_url\": \"http://127.0.0.1:9/v1\", \"sdk\": \"openai-completions\"}}}")
    (with-saved-globals ((nle::*shared-config-path* config)
                         (nle::*provider-models-cache-path* nil)
                         (nle::*models-catalog-path* nil))
      (with-stubbed-fdefinition (nle::provider-key-check (provider key &key vars) nil)
        (flet ((row (body) (wire-row "lane-k" (nlk:json-value body :array "providers"))))
          (with-gateway-http (port :get "/api/gateway/keys")
            (is (equal "none" (nlk:json-value (row body) :string "source")) "a provider named with no key"))
          (with-gateway-http (port :post "/api/gateway/keys" :status 200
                              :body "{\"provider\": \"lane-k\", \"key\": \"sk-test-0123456789abcdef\"}")
            (is (equal "api_key" (nlk:json-value (row body) :string "source")) "saved where /models saves")
            (is (equal "cdef" (nlk:json-value (row body) :string "tail")) "and said by its last four")
            (is (not (search "0123456789" (shasht:write-json body nil))) "never by the rest"))
          (let ((saved (nlk:json-value (nle::read-auth-file nle::*auth-file-path*)
                                       :string "api_keys" "lane-k" "key")))
            (is (equal "sk-test-0123456789abcdef" saved) "the file holds it"))
          (with-gateway-http (port :post "/api/gateway/keys" :body "{\"provider\": \"lane-k\", \"key\": null}")
            (is (equal "none" (nlk:json-value (row body) :string "source")) "a null key takes it out")
            (is (null (nlk:json-value body :string "unchecked"))))
          ;; A key nothing answered may be right: saved, and the answer says it was not checked.
          (with-stubbed-fdefinition (nle::provider-key-check (provider key &key vars)
                                      (values :unchecked "no answer from 127.0.0.1:9 in 5 s"))
            (with-gateway-http (port :post "/api/gateway/keys" :body "{\"provider\": \"lane-k\", \"key\": \"sk-hang\"}")
              (is (= 200 status))
              (is (equal "api_key" (nlk:json-value (row body) :string "source")))
              (is (equal "no answer from 127.0.0.1:9 in 5 s" (nlk:json-value body :string "unchecked")))))
          (with-gateway-http (port :post "/api/gateway/keys" :body "{\"provider\": \"lane-k\", \"key\": null}")
            (is (equal "none" (nlk:json-value (row body) :string "source"))))
          (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "api_keys" "lane-k"))))))))

(deftest control-keys-carry-the-blanks-of-a-catalog-address (with-temp-gateway (port))
  ;; A shell's /models asks the organism to check a key: the values it asked
  ;; for the blanks of the provider's catalog address go to the check, and
  ;; into auth.json beside the key.
  (with-saved-globals ((nle::*provider-models-cache-path* nil)
                       (nle::*models-catalog-path* nil))
    (let ((seen '()))
      (with-stubbed-fdefinition (nle::provider-key-check (provider key &key vars)
                                  (push vars seen)
                                  (values :works nil))
        (with-gateway-http (port :post "/api/gateway/keys" :status 200
                            :body "{\"provider\": \"blanked\", \"key\": \"k\", \"check\": true, \"vars\": {\"NL_BLANK_ACCOUNT\": \"acc-1\", \"odd\": 7}}")
          (is (equal "works" (nlk:json-value body :string "verdict"))))
        (is (equal '(("NL_BLANK_ACCOUNT" . "acc-1")) (first seen)) "the check is given the value, and a member that is no string is dropped")
        (with-gateway-http (port :post "/api/gateway/keys" :status 200
                            :body "{\"provider\": \"blanked\", \"key\": \"k\", \"vars\": {\"NL_BLANK_ACCOUNT\": \"acc-1\"}}"))
        (is (equal "acc-1" (nlk:json-value (nle::read-auth-file nle::*auth-file-path*)
                                           :string "api_keys" "blanked" "vars" "NL_BLANK_ACCOUNT")) "saved with the key")
        (with-gateway-http (port :post "/api/gateway/keys" :body "{\"provider\": \"blanked\", \"key\": null}"))
        (is (null (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "api_keys" "blanked")) "and taken out with it")))))

(deftest control-keys-say-what-is-wrong-with-a-keyless-config-entry (with-temp-gateway (port))
  ;; The config's providers name a key typed where a name goes, a numbered copy
  ;; of a provider whose key is saved, and a provider simply missing its key.
  (with-temp-file (config :type "jsonc"
                          :contents "{\"providers\": {\"sk-test-0123456789abcdefghijklmn\": {}, \"relay-1781313642618\": {\"base_url\": \"http://127.0.0.1:9/v1\", \"sdk\": \"openai-completions\"}, \"relay-1782252028777\": {\"base_url\": \"http://127.0.0.1:9/v1\", \"sdk\": \"openai-completions\"}, \"plain\": {\"base_url\": \"http://127.0.0.1:9/v1\", \"sdk\": \"openai-completions\"}}}")
    (with-saved-globals ((nle::*shared-config-path* config)
                         (nle::*provider-models-cache-path* nil)
                         (nle::*models-catalog-path* nil))
      (with-stubbed-fdefinition (nle::provider-key-check (provider key &key vars) nil)
        (flet ((fix (body id &optional (member "fix"))
                 (nlk:json-value (wire-row id (nlk:json-value body :array "providers")) :string member)))
          (with-gateway-http (port :post "/api/gateway/keys" :status 200
                              :body "{\"provider\": \"relay-1782252028777\", \"key\": \"sk-test-0123456789abcdef\"}")
            (is (equal "pasted_key" (fix body "sk-test-…klmn")) "a key typed where a name goes, said by its prefix and last four")
            (is (not (search "0123456789abcdefghijklmn" (shasht:write-json body nil))) "and never whole")
            (is (equal "keyless_copy" (fix body "relay-1781313642618")) "a numbered copy with no key")
            (is (equal "relay-1782252028777" (fix body "relay-1781313642618" "of")) "of the one that has it")
            (is (null (fix body "relay-1782252028777")) "the keyed one is fine")
            (is (null (fix body "plain")) "one only missing its key is only missing it"))
          ;; Forget takes a flagged entry out of config.jsonc, by the name the listing says; nothing else.
          (with-gateway-http (port :post "/api/gateway/keys" :status 400
                              :body "{\"provider\": \"plain\", \"forget\": true}"))
          (with-gateway-http (port :post "/api/gateway/keys" :status 200
                              :body "{\"provider\": \"relay-1781313642618\", \"forget\": true}")
            (let ((rows (nlk:json-value body :array "providers")))
              (is (null (wire-row "relay-1781313642618" rows)) "forgotten")
              (is (wire-row "relay-1782252028777" rows) "its keyed twin stays")
              (is (wire-row "plain" rows) "and the one it did not flag")))
          (with-gateway-http (port :post "/api/gateway/keys" :status 200
                              :body "{\"provider\": \"sk-test-\\u2026klmn\", \"forget\": true}")
            (is (null (wire-row "sk-test-…klmn" (nlk:json-value body :array "providers"))) "the pasted key is out"))
          (is (not (search "sk-test" (uiop:read-file-string config))) "and out of the file")
          (with-gateway-http (port :post "/api/gateway/keys" :status 200
                              :body "{\"provider\": \"relay-1782252028777\", \"key\": null}")))))))

(deftest control-cells-list-the-folder-as-cells-does (with-temp-gateway (port))
  (with-gateway-http (port :get "/api/gateway/cells" :status 200)
    (let ((rows (nlk:json-value body :array "cells")))
      (is (notany (lambda (row) (nlk:json-value row :any "on")) rows) "what ships is listed, none of it on")))
  (is-route (port :post "/api/gateway/cells" :body "{\"name\": \"no-such\"}") 400
            "a folder that is not there" "invalid_request"))

(deftest control-index-route-lists-opens-saves-and-forgets (with-temp-gateway (port))
  ;; The page's Index tab: what is kept, ranked; one opened with its text; a
  ;; save kept by the scribe as an eval keeps one, a renamed text refused; a
  ;; forget taking the file out.
  (with-knowledge-fixture ()
    (keep-definition-text "(define-memory kn-page \"The operator reads the page first.\" :type :user)")
    (flet ((row (body name) (wire-row name (nlk:json-value body :array "definitions") "name")))
      (with-gateway-http (port :get "/api/gateway/index" :status 200)
        (is (= 1 (nlk:json-value body :integer "hot")))
        (is-shape (row body "kn-page")
          ((:string "kind") "memory") ((:string "type") "user") ((:string "cell") "memory")
          ((:boolean "hot") eq t)))
      (with-gateway-http (port :get "/api/gateway/index?name=kn-page")
        (is (search "(define-memory kn-page" (nlk:json-value body :string "definition" "text"))))
      (with-gateway-http (port :post "/api/gateway/index"
                               :body (nlk:encode-json-object
                                      (nlk:json-object "op" "save" "name" "kn-page"
                                                       "text" "(define-memory kn-page \"The operator reads the page before the log.\" :type :user)")))
        (is (= 200 status))
        (is (search "[memory: kept kn-page]" (nlk:json-value body :string "text")))
        (is (search "before the log" (nlk:json-value (row body "kn-page") :string "description"))))
      (is-route (port :post "/api/gateway/index"
                      :body "{\"op\": \"save\", \"name\": \"kn-page\", \"text\": \"(define-memory kn-other \\\"x.\\\")\"}")
                400 "a new name is a new definition" "invalid_request")
      (with-gateway-http (port :post "/api/gateway/index" :body "{\"op\": \"forget\", \"name\": \"kn-page\"}")
        (is (search "[memory: forgot kn-page]" (nlk:json-value body :string "text")))
        (is (null (row body "kn-page"))))
      (is (null (probe-file (merge-pathnames "tester-memory/kn-page.lisp" nlk::*cells-directory*)))))))

;;; The profiles: the posture's scratch folder holds every named one, and the
;;; default home is only ever read.

(defun control-profile-row (body name)
  "The row the profiles listing BODY gives the profile NAME, or NIL."
  (wire-row name (nlk:json-value body :array "profiles") "name"))

(deftest control-profiles-route-lists-creates-and-uses (with-temp-gateway (port))
  ;; What a clone is taken from: a profile with a SOUL.md of its own.
  (nlk:profile-create "base" :clone :blank :description "the source")
  (write-temp-file (merge-pathnames "SOUL.md" (nlk:profile-home "base")) "Be terse.")
  (with-gateway-http (port :get "/api/gateway/profiles" :status 200)
    (let ((default (control-profile-row body "default")))
      (is (eq default (aref (nlk:json-value body :array "profiles") 0)) "the default home first")
      (is (nlk:json-value default :any "sticky") "booting by default")
      (is (search "not deleted" (nlk:json-value default :string "kept")) "and saying why it stays"))
    (is (equal "the source" (nlk:json-value (control-profile-row body "base") :string "description")))
    (is (null (nlk:json-value (control-profile-row body "base") :any "kept")) "a quiet profile may go"))
  (with-gateway-http (port :post "/api/gateway/profiles" :status 200
                      :body "{\"name\": \"coder\", \"clone\": \"settings\", \"from\": \"base\", \"description\": \"for code\"}")
    (is (search "created coder at" (nlk:json-value body :string "text")) "saying what the shells say")
    (let ((row (control-profile-row body "coder")))
      (is-shape row ((:string "source") "base" "cloned from the profile named")
        ((:string "clone") "settings" "as its settings") ((:string "description") "for code")
        ((:any "soul") eq t "SOUL.md among them"))))
  (is (equal "Be terse." (nlk:read-text (merge-pathnames "SOUL.md" (nlk:profile-home "coder")))))
  (with-gateway-http (port :post "/api/gateway/profiles" :status 200
                      :body "{\"name\": \"writer\", \"clone\": \"blank\", \"soul\": \"Write plainly.\"}")
    (is (search "SOUL.md is saved" (nlk:json-value body :string "text")) "a blank home given a SOUL.md at its making"))
  (is (equal "Write plainly." (nlk:read-text (merge-pathnames "SOUL.md" (nlk:profile-home "writer")))))
  (is-route (port :post "/api/gateway/profiles" :body "{\"name\": \"Coder\"}") 400 "no profile name" "refused")
  (is-route (port :post "/api/gateway/profiles" :body "{\"name\": \"serve\"}") 400 "a launch verb" "refused")
  (is-route (port :post "/api/gateway/profiles" :body "{\"name\": \"coder\"}") 400 "a name taken" "refused")
  (is-route (port :post "/api/gateway/profiles" :body "{\"name\": \"twin\", \"from\": \"nobody\"}") 400
            "a source that is not there" "refused")
  (is-route (port :post "/api/gateway/profiles" :body "{\"name\": \"twin\", \"clone\": \"most\"}") 400
            "a clone the verb does not take" "invalid_request")
  (is (null (nlk:find-profile "twin")) "and a refusal makes nothing")
  (with-gateway-http (port :post "/api/gateway/profiles/use" :body "{\"name\": \"coder\"}")
    (is (= 200 status))
    (is (search "coder boots by default" (nlk:json-value body :string "text")))
    (is (nlk:json-value (control-profile-row body "coder") :any "sticky") "the listing marks it")
    (is (not (nlk:json-value (control-profile-row body "default") :any "sticky")) "and the default no more"))
  (is (equal "coder" (nlk:sticky-profile)) "the sticky file names it")
  (is-route (port :post "/api/gateway/profiles/use" :body "{\"name\": \"nobody\"}") 400 "a profile not there" "refused")
  (is-route (port :get "/api/gateway/profiles" :token nil) 401 "the operator's alone"))

(deftest control-profiles-route-soul-export-and-delete (with-temp-gateway (port))
  (nlk:profile-create "coder" :clone :blank)
  (with-gateway-http (port :get "/api/gateway/profiles/soul?name=coder" :status 200)
    (is-shape body ((:any "present") not "no SOUL.md yet") ((:string "content") "")))
  (with-gateway-http (port :post "/api/gateway/profiles/soul" :body "{\"name\": \"coder\", \"content\": \"Be kind.\"}")
    (is (nlk:json-value body :any "present") "a save writes it")
    (is (search "saved" (nlk:json-value body :string "text"))))
  (with-gateway-http (port :get "/api/gateway/profiles/soul?name=coder")
    (is (equal "Be kind." (nlk:json-value body :string "content")) "read back as saved")
    (is (search "/profiles/coder/SOUL.md" (nlk:json-value body :string "file")) "from its home"))
  (with-gateway-http (port :post "/api/gateway/profiles/soul" :body "{\"name\": \"coder\", \"content\": \"  \"}")
    (is (not (nlk:json-value body :any "present")) "a blank save takes the file out"))
  (is (null (probe-file (merge-pathnames "SOUL.md" (nlk:profile-home "coder")))))
  (is-route (port :get "/api/gateway/profiles/soul?name=nobody") 400 "a profile not there" "refused")
  (multiple-value-bind (file status headers) (gateway-octets port :get "/api/gateway/profiles/export?name=coder")
    (is (= 200 status))
    (is (equal "application/gzip" (gethash "content-type" headers)))
    (is (search "nodecode-profile-coder.tar.gz" (gethash "content-disposition" headers)) "named for the profile")
    (is (and (> (length file) 2) (= #x1f (aref file 0)) (= #x8b (aref file 1))) "a gzip archive"))
  ;; This gateway's own profile stays, and the listing says so before a delete is asked.
  (with-saved-globals ((nlk:*home* (nlk:profile-home "coder")))
    (with-gateway-http (port :get "/api/gateway/profiles")
      (let ((row (control-profile-row body "coder")))
        (is (nlk:json-value row :any "current") "the profile this gateway runs")
        (is (search "this process's own profile" (nlk:json-value row :string "kept")) "says why it stays")))
    (with-gateway-http (port :delete "/api/gateway/profiles?name=coder")
      (is (= 400 status) "a delete of it is refused")
      (is (equal "refused" (nlk:json-value body :string "error" "code")))
      (is (search "this process's own profile" (nlk:json-value body :string "error" "message")) "in the verb's words")))
  (is (uiop:directory-exists-p (nlk:profile-home "coder")) "and the home is still there")
  (is-route (port :delete "/api/gateway/profiles?name=default") 400 "the default home" "refused")
  (with-gateway-http (port :delete "/api/gateway/profiles?name=coder" :status 200)
    (is (null (control-profile-row body "coder")) "gone from the roster"))
  (is (null (uiop:directory-exists-p (nlk:profile-home "coder"))) "and from the disk"))
