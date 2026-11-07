;;;; config-test.lisp --- secret resolution and the allowlist floor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The typed accessor family is core waist material now; its tests live in
;;;; test/waist/config-test.lisp. What is proved here is the channel-only
;;;; vocabulary, and that its refusals ride the waist's CONFIG-REFUSAL.

(in-package #:nodecode.test)

(deftest channel-config-secret-exactly-one-of ()
  (flet ((refused (json description &aux (section (cell-json json)))
           (is (signals-error nlk:config-refusal
                 (nck:resolve-channel-secret section "token"))
               description)))
    (refused "{\"token_env\": \"X\", \"token_file\": \"/tmp/y\"}"
             "both _env and _file present is refused")
    (refused "{}" "neither _env nor _file present is refused")
    (refused "{\"token_env\": \"NODECODE_TEST_UNSET_TOKEN_VAR\"}"
             "an unset env var is refused, not returned empty")))

(deftest channel-config-secret-file-trimmed ()
  (with-temp-file (path :contents "  secret-token-value
" :type "txt")
    (let ((section (nlk:json-object "token_file" path)))
      (is (equal "secret-token-value"
                 (nck:resolve-channel-secret section "token")))))
  (with-temp-file (path :contents "   " :type "txt")
    (let ((section (nlk:json-object "token_file" path)))
      (is (signals-error nlk:config-refusal
            (nck:resolve-channel-secret section "token"))))))

(deftest channel-config-fail-closed-allowlist-floor ()
  (is (signals-error nlk:config-refusal
        (nck:require-non-empty-allowlist "testchan"
                                         "allowed_chats" '()
                                         "allowed_users" '())))
  (is (nck:require-non-empty-allowlist "testchan"
                                       "allowed_chats" '()
                                       "allowed_users" '("42")))
  (nlk:when-let (condition (signals-error nlk:config-refusal
                             (nck:require-non-empty-allowlist "testchan"
                                                              "allowed_chats"
                                                              '())))
    (is (search "allowed_chats"
                (nlk:config-refusal-detail condition)))))
