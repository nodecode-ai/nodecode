;;;; Model ids are not display labels.
(in-package #:nodecode.test)

(defparameter +model-id-config+
  "{\"general\":{\"default_model\":{\"provider\":\"openai-completions\",\"model\":\"kept\"}},
    \"providers\":{\"a6api\":{\"sdk\":\"google\",\"base_url\":\"http://127.0.0.1:1/v1\",
      \"models\":[{\"id\":\"private alias\"}]}}}")

(defparameter +model-id-catalog+
  "{\"google\":{\"npm\":\"@ai-sdk/google\",\"models\":{
      \"gemini-4-argon\":{\"name\":\"Gemini 4 Argon\"},
      \"exact-id\":{\"name\":\"Other Label\"},
      \"another-id\":{\"name\":\"exact-id\"},
      \"ambiguous-a\":{\"name\":\"AmbiguousLabel\"},
      \"ambiguous-b\":{\"name\":\"AmbiguousLabel\"}}}}")

(defparameter +model-id-cache+
  "{\"a6api\":{\"base_url\":\"http://127.0.0.1:1/v1\",\"models\":[
      {\"id\":\"relay-id\",\"display\":\"Relay Label\"},
      {\"id\":\"Gemini 4 Argon\",\"display\":\"Literal ID\"}]}}")

(defmacro with-ids ((&rest options) &body body)
  `(with-model-listings (:config +model-id-config+ :catalog +model-id-catalog+ ,@options) ,@body))

(defun model-id-refusal (thunk)
  "THUNK's refusal detail, or NIL if it accepted the input."
  (handler-case (progn (funcall thunk) nil)
    (nle::provider-config-error (e) (nle::provider-error-detail e))))

(deftest model-id-selection-refuses-labels-before-any-mutation (with-ids ())
  (let ((nle::*provider* "openai-completions") (nle::*model* "kept")
        (nle::*endpoint* "http://old.invalid/v1")
        (nle::*auxiliary-model* '(:provider "openai-completions" :model "kept" :effort "high")))
    (let ((before (uiop:read-file-string nle::*shared-config-path*)))
      (dolist (label '("Gemini 4 Argon" "AmbiguousLabel" ""))
        (let ((refusal (model-id-refusal
                         (lambda () (nle::select-default-model :provider "a6api" :model label
                                                              :endpoint "http://new.invalid/v1")))))
          (is (stringp refusal) "a display label or empty id is refused")
          (when (equal label "Gemini 4 Argon")
            (is (and refusal (search "gemini-4-argon" refusal)) "the canonical id is actionable")))
        (is (equal "openai-completions" nle::*provider*) "provider stayed put")
        (is (equal "kept" nle::*model*) "model stayed put")
        (is (equal "http://old.invalid/v1" nle::*endpoint*) "endpoint stayed put"))
      (is (model-id-refusal (lambda () (nle::select-auxiliary-model "Gemini 4 Argon" :provider "a6api"))) "auxiliary admission is identical")
      (is (equal '(:provider "openai-completions" :model "kept" :effort "high") nle::*auxiliary-model*) "auxiliary target and effort stayed put")
      (is (equal before (uiop:read-file-string nle::*shared-config-path*)) "no rejected input reached disk"))))

(deftest model-id-selection-keeps-exact-and-private-ids (with-ids ())
  (let ((nle::*provider* "a6api") (nle::*model* "kept") (nle::*endpoint* nil))
    (dolist (id '("gemini-4-argon" "exact-id" "private alias" "unlisted/private-id"))
      (is (equal id (nth-value 1 (nle::select-default-model :model id))) "explicit ids, including private aliases, are preserved")
      (is (equal id (config-pin nle::*shared-config-path* "default_model" "model")) "the persisted id is exact"))))

(deftest model-id-selection-under-a-turns-target-moves-the-image-default (with-ids ())
  ;; Inside a turn, and in an eval it dispatched, *PROVIDER* and *MODEL* are
  ;; the session's own target (RUN, BACKGROUND-CAPABLE-EVAL). A pick made
  ;; there -- (select-model ...), (slash "/models ...") -- still means the
  ;; image's default: it keeps the default's lane, lands on the globals every
  ;; new session reads, and leaves the turn's own binding alone.
  (with-saved-globals ((nle::*provider* "a6api") (nle::*model* "kept") (nle::*endpoint* nil)
                       (nle::*reasoning-effort* nil))
    (let ((nle::*provider* "turn-lane") (nle::*model* "turn-model") (nle::*reasoning-effort* "turn-effort")
          (nle::*default-target-shadowed* t))
      (multiple-value-bind (provider model) (nle::select-default-model :model "exact-id")
        (is (equal '("a6api" "exact-id") (list provider model)) "a model-only pick keeps the default's lane"))
      (nle::select-default-effort "high")
      (let ((turn (list nle::*provider* nle::*model* nle::*reasoning-effort*)))
        (is (equal '("turn-lane" "turn-model" "turn-effort") turn) "the running turn keeps its target")))
    (let ((default (mapcar #'sb-ext:symbol-global-value '(nle::*provider* nle::*model* nle::*reasoning-effort*))))
      (is (equal '("a6api" "exact-id" "high") default) "the image's default moved"))
    (is (equal "exact-id" (config-pin nle::*shared-config-path* "default_model" "model")) "and the shared config")))

(deftest model-id-listing-identity-outranks-a-catalog-label (with-ids (:cache +model-id-cache+))
  (let ((nle::*provider* "a6api") (nle::*model* "kept") (nle::*endpoint* nil))
    (is (equal "Gemini 4 Argon" (nth-value 1 (nle::select-default-model :model "Gemini 4 Argon"))) "an exact provider-listed id wins over another namespace's display name")
    (is-carrying (refusal (model-id-refusal (lambda () (nle::select-default-model :model "Relay Label"))))
      "display name" "relay-id")))

(deftest model-id-config-pins-refuse-labels-by-key (with-ids ())
  (dolist (key '("default_model" "auxiliary_model"))
    (let* ((general (nlk:make-json-object key (nlk:json-object "provider" "a6api" "model" "Gemini 4 Argon")))
           (refusal (refusal-text nlk:config-refusal (nle::config-model-pin general key))))
      (is (and refusal (search (concatenate 'string "general." key) refusal)) "the config error identifies the pin")
      (is (and refusal (search "gemini-4-argon" refusal)) "and its actual model id"))))

(deftest model-id-freeze-refuses-labels-before-a-provider-request (with-ids ())
  (is-carrying (refusal (model-id-refusal (lambda () (nle::snapshot-effective-provider-config "a6api" "Gemini 4 Argon"))))
    "display name" "gemini-4-argon"))

(deftest model-id-gateway-and-slash-refuse-labels-without-pins ()
  ;; The hermetic wrapper LET-binds the catalog path on this thread. Supply
  ;; the owner's decoded fixture to every worker, not a SETF of that binding.
  (let ((catalog (nle::decode-models-catalog
                  (babel:string-to-octets +model-id-catalog+ :encoding :utf-8))))
    (with-stubbed-fdefinition (nle::models-catalog-table () catalog)
      (with-saved-globals (nle::*auxiliary-model*)
        (with-selection-gateway (path port "model-id-guard" :text +model-id-config+ :model "kept")
          (dolist (route '("/api/gateway/model" "/api/gateway/auxiliary-model"
                           "/api/gateway/sessions/model-id-guard/model"))
            (with-gateway-http (port :post route :body "{\"provider\":\"a6api\",\"model\":\"Gemini 4 Argon\"}")
              (is (= 400 status) "each selection consumer refuses the label")
              (is (search "gemini-4-argon" (nlk:encode-json-object body)) "the error gives the id, not a remote 400")))
          (is-carrying (refusal (model-id-refusal (lambda () (nle:select-model "Gemini 4 Argon" :provider "a6api" :session-id "model-id-guard"))))
            "display name" "gemini-4-argon")
          (is-carrying (refusal (model-id-refusal (lambda () (nle:slash "/models a6api AmbiguousLabel" :session-id "model-id-guard"))))
            "display name" "ambiguous-a" "ambiguous-b")
          (with-gateway-http (port :post "/api/gateway/slash"
                              :body "{\"line\":\"/models a6api AmbiguousLabel\",\"session_id\":\"model-id-guard\"}")
            (is (= 400 status) "the attached shell's slash path refuses too")
            (is (search "ambiguous-a" (nlk:encode-json-object body))))
          (is-model-pin "model-id-guard" nil nil "no session provider was pinned" "nor any model")
          (is (equal "kept" (config-pin path "default_model" "model")) "default on disk is unchanged")
          (is (equal "kept" nle::*model*) "default in memory is unchanged"))))))
