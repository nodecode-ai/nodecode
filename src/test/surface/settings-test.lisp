;;;; settings-test.lisp --- every declared setting read and written from the
;;;; page, and config.jsonc whole for the engineer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every variable a route reads on the gateway's thread is SETF here, never LET:
;;;; the shared config, the secrets folder, the section registry.

(in-package #:nodecode.test)

(defparameter +settings-config+
  "{
  // the operator's own note
  \"nl-settings\": {\"lane\": {\"api_key\": \"sk-kept-0123456789\", \"rooms\": [\"r1\"], \"loud\": false}}
}
"
  "A config holding one declared section: a secret-named member, a list
and a false.")

(defmacro with-settings-lane ((port config) &body body &aux (secrets (gensym "SECRETS")))
  "BODY against a gateway on PORT whose shared config is a temp file CONFIG
holding +SETTINGS-CONFIG+, a scratch secrets folder, and two declared
sections: nl-settings.lane -- a token taken by reference, rooms offered by
name, a check -- and nl-settings.shut, whose probe refuses."
  `(with-temp-gateway (,port)
     (with-temp-trees ((,secrets "settings-secrets"))
       (with-temp-file (nle::*shared-config-path* :type "jsonc" :contents +settings-config+ :setf t)
         (with-saved-globals (nlk::*sections* (nle::*secrets-directory* ,secrets))
           (nlk:define-section ("nl-settings" "lane")
             (:guide "make a bot; paste its token")
             (:check (lambda (table)
                       (format nil "token: ok, ~d room~:p" (length (nlk:config-string-list table "rooms")))))
             (:one-of "token_env" "token_file")
             (:any-of "rooms")
             ("token_env" :env :doc "the variable")
             ("token_file" :path :doc "the file")
             ("rooms" :list :doc "ids" :choices (constantly '(("r1" . "#general") ("r2" . "#random"))))
             ("loud" :boolean :default :false :doc "noise")
             ("mode" :choice :options '("a" "b") :doc "which")
             ("api_key" :string :doc "a key written inline"))
           (nlk:define-section ("nl-settings" "shut")
             ("rooms" :list :choices (nlk:lambda-ignoring (table)
                                       (nlk:config-error "token: unauthorized (401)"))))
           (let ((,config nle::*shared-config-path*))
             (declare (ignorable ,config))
             ,@body))))))

(defun settings-row (body key &optional field)
  "The section KEY of a GET /api/gateway/config BODY, or its FIELD."
  (let ((section (wire-row key (nlk:json-value body :array "sections") "key")))
    (if field
        (find field (nlk:json-value section :array "fields")
              :key (lambda (row) (nlk:json-value row :string "name")) :test #'equal)
        section)))

(deftest settings-read-says-each-section-and-never-a-secret (with-settings-lane (port config))
  (with-gateway-http (port :get "/api/gateway/config" :status 200)
    (is-present (lane (settings-row body "nl-settings.lane")) "the declared section"
      (is (search "token" (nlk:json-value lane :string "needs")) "the pasted token is what it still needs")
      (is (equal "now" (nlk:json-value lane :string "applies")) "a section no folder owns applies now")
      (is (eq t (nlk:json-value lane :any "check")))
      (is (search "lane.token_file" (nlk:json-value lane :string "secret" "to")) "where a pasted token goes"))
    (is-present (key (settings-row body "nl-settings.lane" "api_key")) "the secret-named member"
      (is-shape key ((:any "secret") eq t) ((:string "text") "set"))
      (is (null (nth-value 1 (nlk:json-value key :any "value"))) "and no value"))
    (is (equalp #("r1") (nlk:json-value (settings-row body "nl-settings.lane" "rooms") :array "value")))
    (is (eq t (nlk:json-value (settings-row body "nl-settings.lane" "rooms") :any "choices")))
    (is (eq t (nlk:json-value (settings-row body "nl-settings.lane" "rooms") :any "essential")) "an ANY-OF member is essential, a boolean on the wire")
    (is-present (loud (settings-row body "nl-settings.lane" "loud")) "a false"
      (is-shape loud ((:any "set") eq t) ((:any "value") null)))
    (is (not (search "sk-kept" (shasht:write-json body nil))) "the page is never sent a secret"))
  (is-route (port :get "/api/gateway/config" :token nil) 401 "the operator's alone"))

(deftest settings-read-marks-a-providers-section (with-settings-lane (port config))
  ;; A section whose owner resolves a provider's credential is a provider's,
  ;; which /settings gathers on Models.
  (nlk:define-section ("nl-provider") (:owner "nl-provider") ("base_url" :string :doc "where it serves"))
  (nlk:hook :credential "nl-provider" (lambda (op next) (funcall next op)))
  (with-gateway-http (port :get "/api/gateway/config" :status 200)
    (is (eq t (nlk:json-value (settings-row body "nl-provider") :any "provider")))
    (is (not (nth-value 1 (nlk:json-value (settings-row body "nl-settings.lane") :any "provider"))) "any other section says nothing")))

(deftest settings-member-write-keeps-the-operators-comments (with-settings-lane (port config))
  (with-gateway-http (port :post "/api/gateway/config" :status 200
                           :body "{\"path\": [\"nl-settings\", \"lane\", \"mode\"], \"value\": \"b\"}")
    (is (search "mode" (nlk:json-value body :string "said")) "the answer says the diff")
    (is (equal "b" (nlk:json-value (settings-row body "nl-settings.lane" "mode") :string "value"))))
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"lane\", \"loud\"], \"value\": true}") 200)
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"lane\", \"rooms\"], \"remove\": true}") 200)
  (is-carrying (text (uiop:read-file-string config)) ("// the operator's own note" "comments stay")
    "\"mode\": \"b\"" "\"loud\": true" (:absent "rooms" "a removed member is gone")
    ("sk-kept-0123456789" "the secret beside it is untouched"))
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"lane\"], \"value\": {\"loud\": false, \"mode\": \"a\"}}")
            200 "a section's form writes a false inside it")
  (is (search "\"loud\": false" (uiop:read-file-string config)) "as false")
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"lane\", \"api_key\"], \"value\": \"[set]\"}")
            400 "the sentinel never goes back over a secret" "invalid_request")
  (is-route (port :post "/api/gateway/config" :body "{\"path\": []}") 400 "a path is needed" "invalid_request"))

(deftest settings-secret-paste-keeps-it-in-a-0600-file-the-section-names
    (with-settings-lane (port config))
  (with-gateway-http (port :post "/api/gateway/config" :status 200
                           :body "{\"path\": [\"nl-settings\", \"lane\"], \"secret\": \"  bot-123:abc \"}")
    (let ((file (merge-pathnames "nl-settings.lane.token_file" nle::*secrets-directory*)))
      (is (equal (format nil "bot-123:abc~%") (uiop:read-file-string file)) "trimmed, in its own file")
      (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring file))))) "mode 0600")
      (is (search (namestring file) (nlk:json-value body :string "said")) "and the answer names it")
      (is-present (row (settings-row body "nl-settings.lane" "token_file")) "the section names the file"
        (is (equal (namestring file) (nlk:json-value row :string "value")))))
    (is-present (lane (settings-row body "nl-settings.lane")) "the section"
      (is (not (search "token" (nlk:json-value lane :string "needs"))) "it needs the token no more"))
    (is (not (search "bot-123" (shasht:write-json body nil))) "the page never hears the secret back"))
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"shut\"], \"secret\": \"x\"}")
            400 "a section with no secret pair takes none" "invalid_request"))

(deftest settings-probe-runs-the-declared-choices-and-check (with-settings-lane (port config))
  (with-gateway-http (port :post "/api/gateway/config/probe" :status 200
                           :body "{\"path\": [\"nl-settings\", \"lane\"], \"field\": \"rooms\"}")
    (is (equal '("#general" "#random")
               (map 'list (lambda (row) (nlk:json-value row :string "label"))
                    (nlk:json-value body :array "choices"))) "ids offered by name"))
  (with-gateway-http (port :post "/api/gateway/config/probe" :body "{\"path\": [\"nl-settings\", \"lane\"]}")
    (is (equal "token: ok, 1 room" (nlk:json-value body :string "text")) "the check over the section as it reads"))
  (with-gateway-http (port :post "/api/gateway/config/probe" :status 400
                           :body "{\"path\": [\"nl-settings\", \"shut\"], \"field\": \"rooms\"}")
    (is (equal "refused" (nlk:json-value body :string "error" "code")))
    (is (search "unauthorized (401)" (nlk:json-value body :string "error" "message")) "in the probe's own words"))
  ;; A choices function's second value: nothing to pick until the operator
  ;; acts (wait), and what an unset field takes on an Enter (default).
  (nlk:define-section ("nl-settings" "wait")
    ("rooms" :list :choices (nlk:lambda-ignoring (table) (values '() '(:wait "add the bot to a server"))))
    ("owner" :list :choices (nlk:lambda-ignoring (table)
                              (values '(("u1" . "peas  made the app") ("u2" . "ada")) '(:default ("u1"))))))
  (with-gateway-http (port :post "/api/gateway/config/probe" :status 200
                           :body "{\"path\": [\"nl-settings\", \"wait\"], \"field\": \"rooms\"}")
    (is (equalp #() (nlk:json-value body :array "choices")) "nothing to pick from yet")
    (is (equal "add the bot to a server" (nlk:json-value body :string "wait")) "and what the operator does first")
    (is (null (nth-value 1 (nlk:json-value body :any "default"))) "no default named, none sent"))
  (with-gateway-http (port :post "/api/gateway/config/probe" :status 200
                           :body "{\"path\": [\"nl-settings\", \"wait\"], \"field\": \"owner\"}")
    (is (equalp #("u1") (nlk:json-value body :array "default")) "the ids an Enter takes")
    (is (null (nth-value 1 (nlk:json-value body :any "wait"))) "no wait named, none sent"))
  (is-route (port :post "/api/gateway/config/probe" :body "{\"path\": [\"nl-settings\", \"shut\"]}")
            400 "a section with no check" "invalid_request")
  (is-route (port :post "/api/gateway/config/probe" :body "{\"path\": [\"no\", \"such\"]}")
            400 "an undeclared section" "unknown_section"))

(deftest settings-raw-save-puts-the-secrets-back-and-refuses-what-would-not-load
    (with-settings-lane (port config))
  (flet ((save (text stamp)
           (shasht:write-json (nlk:json-object "text" text "stamp" stamp) nil))
         (swap (text old new) (uiop:frob-substrings text (list old) new)))
    (with-gateway-http (port :get "/api/gateway/config/raw")
      (let ((text (nlk:json-value body :string "text"))
            (stamp (nlk:json-value body :string "stamp")))
        (is (search "\"api_key\": \"[set]\"" text) "a secret reads as the sentinel")
        (is (not (search "sk-kept" (shasht:write-json body nil))) "and never as itself")
        (is (search "// the operator's own note" text))
        (is-route (port :post "/api/gateway/config/raw" :body (save "{\"nl-settings\": " stamp))
                  400 "a document that does not read" "invalid_config")
        (let ((edited (swap text "\"loud\": false}}" "\"loud\": true}} // said on the page")))
          (with-gateway-http (port :post "/api/gateway/config/raw" :body (save edited stamp))
            (is (= 200 status) "an edit, with the file as it was read")
            (is (search "1 secret kept" (nlk:json-value body :string "said"))))
          (is-route (port :post "/api/gateway/config/raw" :body (save edited stamp))
                    400 "a stamp the file has moved past" "changed"))))
    (let ((saved (uiop:read-file-string config)))
      (is (search "\"api_key\": \"sk-kept-0123456789\"" saved) "the untouched secret is back")
      (is (search "\"loud\": true}} // said on the page" saved) "the operator's text as written")
      (is (search "// the operator's own note" saved)))
    (with-gateway-http (port :get "/api/gateway/config/raw")
      (let ((stamp (nlk:json-value body :string "stamp"))
            (text (nlk:json-value body :string "text")))
        (flet ((refused (edit)
                 (with-gateway-http (port :post "/api/gateway/config/raw" :body (save edit stamp))
                   (and (= 400 status) (nlk:json-value body :string "error" "message")))))
          (is (search "mode must be one of a|b"
                      (refused (swap text "\"loud\"" "\"mode\": \"c\", \"loud\""))) "a shape refused by name")
          (is (search "no secret there to keep"
                      (refused (swap text "\"loud\"" "\"password\": \"[set]\", \"loud\""))) "a stray sentinel"))))
    (is (not (search "\"mode\"" (uiop:read-file-string config))) "nothing refused was written")))

(deftest settings-raw-editor-masks-what-config-get-masks ()
  ;; One rule names a secret for both readers (SECRET-PATH-P): an MCP
  ;; server's Authorization header and a key list under api_key read as the
  ;; sentinel in the raw editor too, and a save of the text as read puts each
  ;; one back byte for byte.
  (let* ((old "{\"mcp\": {\"servers\": {\"s\": {\"headers\": {\"Authorization\": \"Bearer FAKEHDR6\"}}}},
 \"p\": {\"api_key\": [\"FAKEARR5\"], \"X-Api-Key\": \"FAKEX8\", \"sdk\": \"anthropic\"}}")
         (masked (nle::splice-spans old (loop for (nil start end) in (nle::secret-spans old)
                                              collect (list start end (nlk::json-string-literal
                                                                       nle::*redacted-secret*))))))
    (dolist (fake '("FAKEHDR6" "FAKEARR5" "FAKEX8"))
      (is (not (search fake masked)) fake))
    (is (search "\"sdk\": \"anthropic\"" masked) "what is no secret reads as itself")
    (multiple-value-bind (text count) (nle::restore-secrets masked old)
      (is (equal old text) "the text as read saves back as the file's own bytes")
      (is (= 3 count)))))

;;; --- what the section's own reader refuses ---------------------------------------
;;; A section's reader is what its cell's start runs (NLK:SECTION-SETTINGS,
;;; the declared fields and then the owner's own reading). The write asks the
;;; reader first, and a start it refused is said on the health check, the cells
;;; list and the section itself.

(defmacro with-refusing-section ((port config) &body body)
  "BODY in WITH-SETTINGS-LANE with nl-settings.gate declared too, owned by
the peripheral nl-gatekeeper: seats (an integer, at least 1) and a label, and
a reader that refuses more than 5 seats in its own words, as a cell's
:settings does."
  `(with-settings-lane (,port ,config)
     (nlk:define-section ("nl-settings" "gate")
       (:owner "nl-gatekeeper")
       (:reader (nlk:lambda-ignoring (values table)
                  (when (> (getf values :seats) 5)
                    (nlk:config-error "nl-settings.gate.seats is ~d; a gate is 1 to 5 seats"
                                      (getf values :seats)))
                  values))
       ("seats" :integer :default 2 :min 1 :doc "how many, 1 to 5")
       ("label" :string :doc "a name"))
     ,@body))

(deftest settings-write-refuses-what-the-sections-reader-refuses (with-refusing-section (port config))
  (let ((before (uiop:read-file-string config)))
    (with-gateway-http (port :post "/api/gateway/config"
                             :body "{\"path\": [\"nl-settings\", \"gate\"], \"value\": {\"label\": \"front\", \"seats\": 9}}")
      (is (= 400 status) "refused, not saved with a success line")
      (is (equal "refused" (nlk:json-value body :string "error" "code")))
      (is (equal "nothing was saved: nl-settings.gate.seats is 9; a gate is 1 to 5 seats"
                 (nlk:json-value body :string "error" "message")) "in the reader's own words")
      (is (equal "seats" (nlk:json-value body :string "error" "field")) "naming the member it is about"))
    (is-route (port :post "/api/gateway/config"
                    :body "{\"path\": [\"nl-settings\", \"gate\", \"seats\"], \"value\": 0}")
              400 "a declared bound, member by member" "refused")
    (is (equal before (uiop:read-file-string config)) "nothing refused was written"))
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"gate\"], \"value\": {\"seats\": 4}}")
            200 "a value the reader takes")
  ;; A refusal the file already carries is not this write's: a section broken
  ;; by hand is mended a member at a time.
  (write-temp-file config "{\"nl-settings\": {\"gate\": {\"seats\": 9}}}")
  (setf nle::*shared-config-memo* nil)
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"gate\", \"label\"], \"value\": \"side\"}")
            200 "another member of a section already refused")
  (is-route (port :post "/api/gateway/config"
                  :body "{\"path\": [\"nl-settings\", \"gate\", \"seats\"], \"value\": 3}")
            200 "and the member that mends it"))

(deftest settings-raw-save-refuses-what-the-sections-reader-refuses (with-refusing-section (port config))
  (with-gateway-http (port :get "/api/gateway/config/raw")
    (let ((text (nlk:json-value body :string "text"))
          (stamp (nlk:json-value body :string "stamp")))
      (with-gateway-http (port :post "/api/gateway/config/raw"
                               :body (shasht:write-json
                                      (nlk:json-object "text" (uiop:frob-substrings
                                                               text '("\"lane\": {")
                                                               "\"gate\": {\"seats\": 9}, \"lane\": {")
                                                       "stamp" stamp)
                                      nil) :status 400)
        (is-carrying (message (nlk:json-value body :string "error" "message")) "a gate is 1 to 5 seats"))))
  (is (not (search "gate" (uiop:read-file-string config)))))

(deftest settings-and-every-list-say-an-cell-refused-at-start (with-refusing-section (port config))
  (with-saved-globals (nlk::*notices* (nle::*model-config-refusal* nil)
                       (nlk::*cells* (list (nlk::make-cell
                                             :name "nl-gatekeeper" :kind :peripheral
                                             :start (lambda (config)
                                                      (nlk:section-settings
                                                       (nlk:find-section '("nl-settings" "gate"))
                                                       (nlk:section-table config '("nl-settings" "gate"))))))))
    (write-temp-file config "{\"nl-settings\": {\"gate\": {\"seats\": 9}}}")
    (setf nle::*shared-config-memo* nil)
    (nle::start-cells '("nl-gatekeeper"))
    (let ((why "nl-settings.gate.seats is 9; a gate is 1 to 5 seats"))
      (is (equal why (nlk:cell-refusal (first nlk::*cells*))) "the record keeps the words, not the condition's name")
      (is-present (fact (find "nl-gatekeeper" (nle::health-facts) :key #'nle::health-fact-label :test #'equal)) "its row"
        (is-shape fact (.tone eq :danger) (.detail (format nil "refused at start: ~a" why))
          (.section "nl-settings.gate" "pointing at the section that mends it")))
      (is-present (fact (find :cells (nle::health-facts) :key #'nle::health-fact-key)) "the cells count"
        (is (search "1 refused at start" (nle::health-fact-detail fact)))
        (is (eq :danger (nle::health-fact-tone fact)) "is no longer fine"))
      (with-gateway-http (port :get "/api/gateway/health")
        (is (find "nl-settings.gate" body :key (lambda (fact) (gethash "section" fact)) :test #'equal)))
      (with-gateway-http (port :get "/api/gateway/cells")
        (is-present (row (switch-row body "nl-gatekeeper")) "its row on the cells list"
          (is-shape row ((:string "refused") why) ((:string "section") "nl-settings.gate"))))
      (with-gateway-http (port :get "/api/gateway/config")
        (is (equal why (nlk:json-value (settings-row body "nl-settings.gate") :string "start_refusal")))
        ;; a section whose owner never refused says nothing of the kind
        (is (null (nth-value 1 (nlk:json-value (settings-row body "nl-settings.lane") :any "start_refusal"))))))))
