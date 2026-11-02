;;;; health-test.lisp --- what this organism is missing, said once.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (fact nle::health-fact))

(defun health-fact-named (key &optional (facts (nle::health-facts)))
  (find key facts :key #'nle::health-fact-key))

(deftest health-says-whether-anything-keeps-the-organism-running ()
  ;; The gateway's fact is read wherever a report is, the landing screen
  ;; included, so it spawns nothing: one file says whether the gateway is
  ;; on, the store's lock who serves.
  (is (null (health-fact-named :gateway)))
  (with-scripted-systemd (fake)
    (is-present (fact (health-fact-named :gateway)) "the fact, once there is a unit folder"
      (is (search "off — nothing keeps this organism running" (nle::health-fact-detail fact)))
      (is (eq :ok (nle::health-fact-tone fact)) "off is a choice, not a fault"))
    (nlk:gateway-on :program "/opt/nodecode/current/nodecode")
    (setf (fake-systemd-calls fake) '())
    (is-present (fact (health-fact-named :gateway)) "the fact with the gateway on"
      (is-shape fact (.detail "on, but nothing serves the store") (.tone eq :warn)))
    (is (null (fake-systemd-calls fake)) "and systemd was never asked")))

(deftest health-says-when-the-organism-runs-pristine
    (with-saved-globals ((nlk:*pristine* nil) (nlk::*pristine-reason* nil)))
  ;; A gateway that boots pristine has no cell, so a shell attached to it
  ;; reads every cell command as unknown; the floor and why are a fact
  ;; /doctor and the landing screen say, not only the layer's session.
  (is (null (health-fact-named :self)) "nothing to say on a folder that loaded")
  (setf nlk:*pristine* t
        nlk::*pristine-reason* "the self loaded under key e04aad67 died before it served")
  (is-present (fact (health-fact-named :self)) "the floor, once it stands on it"
    (is (search "pristine" (nle::health-fact-detail fact)))
    (is (search "died before it served" (nle::health-fact-detail fact)) "and why")
    (is (eq :danger (nle::health-fact-tone fact)) "the landing screen shows :danger")
    (is (search "restart" (nle::health-fact-remedy fact)))))

(deftest health-says-when-no-model-is-pinned (with-saved-globals (nle::*model-config-refusal*))
  ;; A fresh image carries a baked default, so the footer always names SOME
  ;; model and an operator reads that as configured. It is not the same
  ;; thing: a turn binds the shared config's pin, and a file with no pin
  ;; leaves the process on whatever it was dumped with. The operator whose
  ;; migration lost every provider had exactly this and no way to see it.
  (setf nle::*model-config-refusal* nil)
  (with-temp-shared-config ((format nil "{~%  \"general\": {}~%}~%"))
    (is-present (fact (health-fact-named :model)) "the model fact"
      (is (equal "no model configured" (nle::health-fact-detail fact)))
      (is (eq :warn (nle::health-fact-tone fact)) "and it is something to act on")
      (is (search "/models" (nle::health-fact-remedy fact)))))
  (with-temp-shared-config ((format nil "{~%  \"general\": {\"default_model\": ~
                                     {\"provider\": \"anthropic\", \"model\": \"m-1\"}}~%}~%"))
    (is-present (fact (health-fact-named :model)) "the model fact"
      (is (equal "m-1 on anthropic" (nle::health-fact-detail fact)) "names the pin")
      (is (eq :ok (nle::health-fact-tone fact)) "and is not a problem")
      (is (not (nle::health-problem-p fact)) "so a problems-only report drops it")))
  ;; vr-101: a pin whose provider nothing knows refuses every turn, and says
  ;; so here too -- in the organism, whose lanes a turn runs on (a shell asks
  ;; its gateway: HOME-SCREEN-TAKES-A-PINS-VERDICT-FROM-THE-GATEWAY).
  (with-temp-shared-config ((format nil "{~%  \"general\": {\"default_model\": ~
                                     {\"provider\": \"nosuchprov\", \"model\": \"m-1\"}}~%}~%"))
    (with-saved-globals ((nle::*serving* t))
      (is-present (fact (health-fact-named :model)) "the model fact"
        (is (eq :danger (nle::health-fact-tone fact)))
        (is (search "provider nosuchprov is not" (nle::health-fact-remedy fact)))))))

(deftest health-carries-a-refused-model-pin-as-the-danger-it-is ()
  ;; The refusal that survived every import on a real box. It was on screen
  ;; as a shell notice at boot and then scrolled away.
  (with-saved-globals ((nle::*model-config-refusal* "general.default_provider is retired"))
    (is-present (fact (health-fact-named :model)) "the model fact"
      (is (eq :danger (nle::health-fact-tone fact)))
      (is (search "retired" (nle::health-fact-remedy fact))))))

(deftest health-counts-sessions-as-the-directory-lists-them (with-temp-store ())
  ;; The landing screen reads HEALTH-FACTS on every render. Counting by
  ;; the whole session directory — a title, an activity stamp and a message
  ;; count read per session — a /clear over a large store would hold the
  ;; shell's own thread for seconds.
  ;; It is one read over the log now, and it answers what the directory would.
  (seed-user-inputs "health-count-typed" '("first prompt"))
  (ensure-durable-session "health-count-empty")
  (is-present (fact (health-fact-named :sessions)) "the sessions fact"
    (is-shape fact (.detail "2") (.detail (princ-to-string (length (nlk:list-sessions)))))))

(deftest health-report-shows-everything-or-only-what-to-act-on ()
  (with-saved-globals ((nle::*model-config-refusal* nil))
    (with-temp-shared-config ((format nil "{~%  \"general\": {}~%}~%"))
      (let ((whole (nle::health-report-text))
            (problems (nle::health-report-text :problems-only t)))
        (is (search "model" whole) "the whole report names every lane")
        (is (search "cells" whole) "including the ones that are fine")
        (is (search "no model configured" problems) "a problems-only report keeps the problem")
        (is (not (search "cells" problems)))))))

(deftest health-facts-take-an-cells-own-without-learning-its-word ()
  ;; The core must not know the word `channel' to report that one is
  ;; configured and not running. HEALTH-FACTS is an ordinary function, so a
  ;; cell adds its fact through the hook primitive that already exists and
  ;; nothing in the core changes to allow it.
  (with-saved-globals ((nle::*model-config-refusal* nil))
    ;; The hook goes with the test: left on, every later read of the facts
    ;; would carry the fact it adds.
    (nlk:with-cleanup ((nle:unhook 'nle::health-facts "test-channels"))
      (nle:hook 'nle::health-facts "test-channels"
                (lambda (next)
                  (append (funcall next)
                          (list (nle::health-fact :channel "discord"
                                                  "configured, not started"
                                                  :tone :warn
                                                  :remedy "a Hermes gateway holds the token")))))
      (is-present (fact (health-fact-named :channel)) "the cell's own fact"
        (is (equal "configured, not started" (nle::health-fact-detail fact)))
        (is (nle::health-problem-p fact) "and it reaches the problems-only report"))
      (is (search "discord" (nle::health-report-text :problems-only t)))
      (is (health-fact-named :model) "the core's own facts are still there"))))

(deftest gateway-marks-a-target-nothing-configured ()
  ;; The attached shell renders what the gateway answers, so the gateway says
  ;; whether anything chose its target. Without this, a fresh image's
  ;; build-time default would sit in the footer through the whole setup walk
  ;; as though it were the operator's model. The footer says how to get a
  ;; model instead, and names one the moment a pick lands.
  (is (not (or (nle::credential-env "NODECODE_PROVIDER")
               (nle::credential-env "NODECODE_MODEL"))))
  (with-saved-globals (nle::*provider* nle::*model* nle::*endpoint*)
    (with-temp-file (nle::*shared-config-path* :setf t :contents (format nil "{}~%"))
      (with-temp-gateway (port)
        (seed-user-inputs "unset-target" '("hi"))
        (let ((model (gateway-shell port "unset-target")))
          (flet ((land-facts ()
                   ;; The worker's fetch, landed here: no loop runs in a test.
                   (setf (nle::gateway-model-model-facts model)
                         (nle::fetch-gateway-model-facts model)))
                 (footer () (nle::footer-frame-row model 100 nil)))
            (land-facts)
            (multiple-value-bind (provider model-id state) (nle::footer-identity model)
              (is (and provider model-id) "the target is still answered: a turn would bind it")
              (is (eq :unconfigured state) "and marked as nothing's choice")
              (is-carrying (text (footer)) "run /setup to add a model" (:absent "? help")
                (:absent (format nil "~a ~a" provider model-id))))
            (is-carrying (said (nle::slash-models-op model '("m-x"))) "session model: " " m-x")
            (land-facts)
            (nlk:bind (((_ model-id state) (nle::footer-identity model)))
              (is (equal "m-x" model-id) "the footer target is the pick")
              (is (eq :configured state) "and a pick is a configuration"))
            (is (search " m-x · ← sessions" (footer)) "which the footer names")))))))

(deftest health-reaches-a-screen-as-json (with-temp-gateway (port))
  ;; What the Dashboard's `Right now' shows: every fact, a problem's remedy
  ;; beside it and a settled fact's left out.
  (with-saved-globals ((nle::*model-config-refusal* "the pin names no provider"))
    (with-gateway-http (port :get "/api/gateway/health" :status 200)
      (let ((model (find "model" body :key (lambda (fact) (gethash "key" fact)) :test #'equal)))
        (is-shape model ("tone" "danger") ("remedy" "the pin names no provider")))
      (is (every (lambda (fact) (or (not (equal "ok" (gethash "tone" fact)))
                                    (null (nth-value 1 (gethash "remedy" fact)))))
                 body)))))

;;; --- the doctor: what only a look can say ------------------------------------

(deftest doctor-reads-the-store-back-and-says-how-long (with-temp-store ())
  (seed-user-inputs "doctor-read" '("one prompt"))
  (is-present (fact (health-fact-named :integrity (nle::doctor-facts))) "the store's fact"
    (is (eq :ok (nle::health-fact-tone fact)))
    (is-carrying (detail (nle::health-fact-detail fact)) "intact" "read in" "quick_check"))
  (with-temp-file (path :type "db" :contents "this was never a database, and no page of it reads")
    (is-present (fact (nle::store-integrity-fact path)) "a damaged store's fact"
      (is (eq :danger (nle::health-fact-tone fact)) "is the danger it is")
      (is (search "sqlite3" (nle::health-fact-remedy fact)) "and says what reads back what it can")))
  (let ((absent (nle::store-integrity-fact (temp-path "absent" "db"))))
    (is (equal "no store yet" (nle::health-fact-detail absent)) "and a home with none yet is no problem")))

(deftest doctor-disk-row-writes-the-home-from-tilde ()
  ;; The disk row wrote the home whole, `/home/.../.nodecode/', where every
  ;; other row writes `~/.nodecode'. DISK-FACT reads the home on this thread,
  ;; so a LET does, and a home that is not the operator's: no `df' runs.
  (let ((nlk::*home* (merge-pathnames "nc-doctor-probe/.nodecode/" (user-homedir-pathname))))
    (with-stubbed-fdefinition (nlk:run-bounded (argv &key seconds)
                                (values (format nil "Filesystem 1024-blocks Used Available Capacity Mounted on~%~
                                                     /dev/x 4000000 1000000 3000000 25% /~%")
                                        "" 0))
      (is-present (fact (nle::disk-fact 0)) "the disk's fact"
        (is (uiop:string-suffix-p (nle::health-fact-detail fact) "under ~/nc-doctor-probe/.nodecode"))
        (is (null (search (namestring (user-homedir-pathname)) (nle::health-fact-detail fact))))))))

(deftest doctor-says-a-replaced-build-wants-a-restart ()
  ;; What Linux names the executable of a process whose file a rebuild or an
  ;; update replaced: the old path, `(deleted)'.
  (is-present (fact (nle::build-fact "/opt/nodecode/0.9/nodecode (deleted)")) "the replaced build's fact"
    (is (eq :warn (nle::health-fact-tone fact)))
    (is (search "restart" (nle::health-fact-remedy fact))))
  (is (eq :ok (nle::health-fact-tone (nle::build-fact "/opt/nodecode/0.9/nodecode"))) "the build as started")
  (is (null (nle::build-fact nil)) "and nothing where there is no telling"))

(deftest doctor-names-each-declared-section-left-incomplete ()
  (with-saved-globals ((nlk::*sections* (nlk:sections)))
    (nlk:define-section ("doctor-test") (:owner "doctor-test") ("token" :secret :required t))
    (nlk:define-section ("doctor-off") (:owner "doctor-test") ("token" :secret :required t))
    (with-temp-shared-config ("{\"doctor-test\": {}, \"doctor-off\": {\"enabled\": false}}")
      (let ((facts (nle::config-facts)))
        (is (search "incomplete" (nle::health-fact-detail (health-fact-named :config facts))) "the config says it")
        (is-present (fact (find "doctor-test" facts :key #'nle::health-fact-label :test #'equal)) "its own fact"
          (is (equal "still needed: token" (nle::health-fact-detail fact)) "in its setup panel's words")
          (is (eq :warn (nle::health-fact-tone fact))))
        (is (not (find "doctor-off" facts :key #'nle::health-fact-label :test #'equal)) "a vetoed one wants nothing")))
    (with-temp-shared-config ("{\"general\": ")
      (is-present (fact (health-fact-named :config (nle::config-facts))) "a config that does not parse"
        (is (eq :danger (nle::health-fact-tone fact)))))))

(deftest doctor-says-whether-the-default-provider-has-a-key ()
  (with-temp-shared-config ((format nil "{\"general\": {\"default_model\": {\"provider\": \"lane-d\", \"model\": \"m\"}}, ~
                                        \"providers\": {\"lane-d\": {\"base_url\": \"http://127.0.0.1:9/v1\", ~
                                                                   \"sdk\": \"openai-completions\"}}}"))
    (is-present (fact (nle::credential-fact)) "a provider with no key"
      (is (eq :warn (nle::health-fact-tone fact)))
      (is (search "none for lane-d" (nle::health-fact-detail fact))))
    (nle::save-provider-api-key "lane-d" "sk-doctor-0123456789")
    (is (eq :ok (nle::health-fact-tone (nle::credential-fact))) "with one saved")
    ;; The listing /models asks before it saves answered 401.
    (with-stubbed-fdefinition (nle::provider-key-check (provider key)
                                (values :refused (format nil "~a refused ~:[no~;this~] key (HTTP 401)" provider key)))
      (is-present (fact (nle::credential-fact)) "a key the provider refuses"
        (is (eq :danger (nle::health-fact-tone fact)))
        (is (search "HTTP 401" (nle::health-fact-remedy fact)))))
    ;; a check nothing answered is no verdict on the key
    (with-stubbed-fdefinition (nle::provider-key-check (provider key)
                                (values :unchecked "no answer from 127.0.0.1:9 in 5 s"))
      (is (eq :ok (nle::health-fact-tone (nle::credential-fact)))))))

(deftest doctor-says-a-release-is-waiting (with-saved-globals ((nlk::*notices* '())))
  (is (search "nothing waiting" (nle::health-fact-detail (nle::update-fact))))
  (nlk:note-standing "update" "update 0.9.1 is staged: hot · `nodecode update' applies it" :info)
  (is-present (fact (nle::update-fact)) "the release waiting"
    (is (eq :warn (nle::health-fact-tone fact)))
    (is (search "0.9.1 is staged" (nle::health-fact-detail fact)))))

(deftest doctor-route-and-slash-show-the-same-findings (with-temp-gateway (port))
  (with-gateway-http (port :post "/api/gateway/doctor" :status 200)
    (is (realp (nlk:json-value body :number "seconds")) "how long the looks took")
    (let ((keys (map 'list (lambda (fact) (gethash "key" fact)) (nlk:json-value body :array "facts"))))
      (dolist (key '("model" "cells" "integrity" "config" "update"))
        (is (member key keys :test #'equal) (format nil "the ~a fact" key)))))
  (is-route (port :get "/api/gateway/doctor") 405 "it goes and looks, so it is asked for")
  (is-carrying (text (nle:slash "/doctor" :session-id "doctor-slash"))
    "store check" "intact" "update" ("model" "the passive facts too"))
  ;; A shell asks the organism it drives: its gateway, whose store this is.
  (is-carrying (text (nle::slash-doctor-op (gateway-shell port "doctor-shell") ""))
    "store check" ("sessions" "the gateway's own count")))
