;;;; secrets-test.lisp --- one secret per file: `nodecode secrets split' and
;;;; the credential chain's secret-file tier.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defparameter +split-auth+
  "{\"api_keys\":{\"alpha\":{\"provider\":\"alpha\",\"key\":\"sk-alpha-0123\"},\"beta\":{\"provider\":\"beta\",\"key\":\"sk-beta-4567\"}},\"oauth_tokens\":{\"gamma\":{\"access\":\"tok-gamma\"}}}"
  "An auth.json holding two API keys and one OAuth login.")

(defmacro with-secret-home ((auth secrets &key (contents '+split-auth+) (config "{}")) &body body)
  "BODY with AUTH a scratch auth.json holding CONTENTS, SECRETS a scratch
secrets folder the chain reads, a scratch config holding CONFIG and no
process key."
  `(with-temp-trees ((,secrets "secrets-split"))
     (with-temp-file (,auth :contents ,contents)
       (with-temp-file (nle::*shared-config-path* :type "jsonc" :contents ,config :setf t)
         (with-saved-globals ((nle::*secrets-directory* (uiop:ensure-directory-pathname ,secrets)))
           (let ((nle::*api-key* nil))
             ,@body))))))

;;; FILE-MODE is generation-test's, which loads first.

(defun secret-file-text (id)
  "The text of provider ID's secret file, or NIL."
  (let ((path (nle::provider-secret-file id)))
    (and (probe-file path) (uiop:read-file-string path))))

(deftest secrets-split-moves-each-key-into-its-own-0600-file ()
  (with-secret-home (auth secrets)
    (let ((report (nle::split-auth-secrets :auth-path auth)))
      (is (equal '("alpha" "beta") (getf report :moved)))
      (is (null (getf report :kept)))
      (is (equal (format nil "sk-alpha-0123~%") (secret-file-text "alpha")))
      (is (equal (format nil "sk-beta-4567~%") (secret-file-text "beta")))
      (is (= #o600 (file-mode (nle::provider-secret-file "alpha"))))
      (is (= #o600 (file-mode auth)))
      (let ((left (nle::read-auth-file auth)))
        (is (null (nlk:json-value left :object "api_keys")))
        (is (equal "tok-gamma" (nlk:json-value left :text "oauth_tokens" "gamma" "access")))))))

(deftest a-key-alone-in-secrets-connects-its-provider ()
  ;; A home set up for --sandbox keeps its key in secrets/ alone, with no
  ;; auth.json entry and no config key: its provider is credentialed, so the
  ;; first frame does not open the Connect walk over it.
  (with-secret-home (auth secrets :contents "{}")
    (let ((nle::*auth-file-path* auth))
      (is (null (nle::configured-provider-inventory)) "no file, no provider")
      (nle::write-secret-file (nle::provider-secret-file "alpha") "sk-alpha-0123")
      ;; an empty file is no key, and a file not named for a provider is no provider
      (with-open-file (out (nle::provider-secret-file "blank") :direction :output :if-exists :supersede))
      (with-open-file (out (merge-pathnames "notes.txt" (uiop:ensure-directory-pathname secrets))
                           :direction :output :if-exists :supersede)
        (write-line "sk-not-a-provider" out))
      (is (equal '("alpha") (nle::secret-file-provider-ids)))
      (is (equal '("alpha") (nle::configured-provider-inventory)) "the file credentials its provider")
      (is (nle::provider-connected-p) "so the provider is connected"))))

(deftest secrets-split-dry-run-writes-nothing ()
  (with-secret-home (auth secrets)
    (let ((before (uiop:read-file-string auth))
          (report (nle::split-auth-secrets :auth-path auth :dry-run t)))
      (is (equal '("alpha" "beta") (getf report :moved)))
      (is (getf report :dry-run))
      (is (null (nle::secret-file-names)))
      (is (equal before (uiop:read-file-string auth))))))

(deftest secrets-split-leaves-a-key-whose-file-holds-another ()
  (with-secret-home (auth secrets)
    (nle::write-secret-file (nle::provider-secret-file "beta") "sk-beta-other")
    (let ((report (nle::split-auth-secrets :auth-path auth)))
      (is (equal '("alpha") (getf report :moved)))
      (is (equal "beta" (first (first (getf report :kept)))))
      (is (equal (format nil "sk-beta-other~%") (secret-file-text "beta")))
      (is (equal "sk-beta-4567" (nle::auth-api-key (nle::read-auth-file auth) "beta")))
      (is (null (nle::auth-api-key (nle::read-auth-file auth) "alpha"))))))

(deftest secrets-split-names-inline-config-keys ()
  (with-secret-home (auth secrets :config "{\"providers\":{\"inl\":{\"api_key\":\"sk-inline\"},\"cmd\":{\"api_key\":\"!pass show x\"}}}")
    (is (equal '("inl") (getf (nle::split-auth-secrets :auth-path auth :dry-run t) :inline)))))

(deftest credential-chain-answers-a-secret-file-before-auth ()
  (with-secret-home (auth secrets :contents "{\"api_keys\":{\"anthropic\":{\"provider\":\"anthropic\",\"key\":\"from-auth\"},\"google\":{\"provider\":\"google\",\"key\":\"from-auth-g\"}}}")
    (nle::write-secret-file (nle::provider-secret-file "anthropic") "from-file")
    (let ((moved (nle::resolve-provider-credential "anthropic" :auth-path auth))
          (unmoved (nle::resolve-provider-credential "google" :auth-path auth)))
      (is (equal "from-file" (nle::credential-key moved)))
      (is (eq :secret-file (nle::credential-source moved)))
      (is (equal "from-auth-g" (nle::credential-key unmoved)))
      (is (eq :api-key (nle::credential-source unmoved))))
    (is (eq :secret-file (nle::provider-auth-state "anthropic" :auth-path auth)))
    (let ((nle::*api-key* "from-config"))
      (is (eq :config (nle::credential-source (nle::resolve-provider-credential "anthropic" :auth-path auth)))))))

(deftest credential-chain-refuses-an-empty-secret-file ()
  (with-secret-home (auth secrets :contents "{\"api_keys\":{\"anthropic\":{\"provider\":\"anthropic\",\"key\":\"from-auth\"}}}")
    (nlk:write-file-atomically (nle::provider-secret-file "anthropic") (format nil "  ~%") :mode #o600)
    (is (signals-error nle::credential-error (nle::resolve-provider-credential "anthropic" :auth-path auth)))
    (is (eq :api-key (nle::provider-auth-state "anthropic" :auth-path auth)))))

(deftest saving-a-key-replaces-its-secret-file ()
  (with-secret-home (auth secrets)
    (nle::split-auth-secrets :auth-path auth)
    (nle::save-provider-api-key "alpha" "sk-alpha-new" :auth-path auth)
    (is (equal (format nil "sk-alpha-new~%") (secret-file-text "alpha")))
    (is (null (nle::auth-api-key (nle::read-auth-file auth) "alpha")))
    (nle::save-provider-api-key "alpha" nil :auth-path auth)
    (is (null (secret-file-text "alpha")))))

(deftest secrets-answer-speaks-split-list-and-refuses-the-rest ()
  (with-secret-home (auth secrets)
    (with-saved-globals ((nle::*auth-file-path* auth))
      (multiple-value-bind (text code stream) (nle::secrets-answer '("split" "--dry-run"))
        (is (search "would move 2 keys" text))
        (is (eql 0 code))
        (is (eq :output stream)))
      (is (null (nle::secret-file-names)))
      (nle::secrets-answer '("split"))
      (let ((text (nle::secrets-answer '("list"))))
        (is (search "providers.alpha.api_key" text))
        (is (not (search "sk-alpha" text))))
      (multiple-value-bind (text code stream) (nle::secrets-answer '("bogus"))
        (is (search "unknown bogus" text))
        (is (eql 2 code))
        (is (eq :error stream))))))
