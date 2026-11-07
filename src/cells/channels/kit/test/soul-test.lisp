;;;; soul-test.lisp --- SOUL.md as the session's standing persona section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The contract under proof: the file is the authority and the `soul`
;;;; harness section its projection, brought current before every ask and
;;;; never rewritten when nothing changed — each harness put is a durable
;;;; session_state event, so VER is the witness that an unchanged file
;;;; appended none.

(in-package #:nodecode.test)

(deftest channel-soul-path-resolution ()
  (let ((section (cell-json "{\"soul_file\": \"/srv/voice/SOUL.md\"}")))
    (is (equal "/srv/voice/SOUL.md" (nck:soul-path section))))
  (let ((nck:*soul-default-path* "/nowhere/SOUL.md"))
    (is (equal "/nowhere/SOUL.md" (nck:soul-path (cell-json "{}"))))
    (is (equal "/nowhere/SOUL.md"
               (nck:soul-path (cell-json "{\"soul_file\": \"  \"}")))))
  (let ((nck:*soul-default-path* nil))
    (is (equal (namestring (nlk:home "SOUL.md")) (nck:soul-path (cell-json "{}"))))))

(deftest channel-soul-read ()
  (with-temp-file (path :type "md")
    (is (null (nck:read-soul path)) "an absent file reads as NIL"))
  (with-temp-file (path :contents "

" :type "md")
    (is (null (nck:read-soul path)) "a blank file reads as NIL"))
  (with-temp-file (path :contents "
Be terse. Have opinions.
" :type "md")
    (is (equal "Be terse. Have opinions." (nck:read-soul path))))
  (is (null (nck:read-soul nil)) "no path, nothing to read"))

(deftest channel-soul-apply-tracks-the-file (with-temp-store ())
  (nlk:create-session :id "s-soul")
  (nck:clear-channel-status "testchan")
  (with-temp-file (path :type "md")
    (is (eq :unchanged (nck:apply-soul "testchan" "s-soul" path)))
    (is (null (nlk:get-harness-section "s-soul" nck:+soul-section+)))
    (is (null (getf (nck:channel-status "testchan") :soul)))
    (write-temp-file path "Be terse.
")
    (is (eq :set (nck:apply-soul "testchan" "s-soul" path)))
    (multiple-value-bind (text ver)
        (nlk:get-harness-section "s-soul" nck:+soul-section+)
      (is (equal "Be terse." text) "verbatim, trimmed")
      (is (eq :unchanged (nck:apply-soul "testchan" "s-soul" path)))
      (is (equal ver (nth-value 1 (nlk:get-harness-section
                                   "s-soul" nck:+soul-section+)))))
    (is (search "(present)" (getf (nck:channel-status "testchan") :soul)))
    (write-temp-file path "Be terse. Be kind.")
    (is (eq :set (nck:apply-soul "testchan" "s-soul" path)))
    (is (equal "Be terse. Be kind."
               (nlk:get-harness-section "s-soul" nck:+soul-section+)))
    (delete-file path)
    (is (eq :cleared (nck:apply-soul "testchan" "s-soul" path)))
    (is (null (nlk:get-harness-section "s-soul" nck:+soul-section+)))
    (is (search "(absent)" (getf (nck:channel-status "testchan") :soul)))
    (is (eq :unchanged (nck:apply-soul "testchan" "s-soul" path))))
  (is (eq :unchanged (nck:apply-soul "testchan" "s-soul" nil)))
  (nck:clear-channel-status "testchan"))

(deftest channel-soul-over-budget-refused-loudly (with-temp-store ())
  (nlk:create-session :id "s-big")
  (nck:clear-channel-status "testchan")
  (with-temp-file (path :contents "0123456789abcdef0123" :type "md")
    (let ((warnings (warnings-of (let ((nlk::+max-harness-bytes+ 16))
                                   (is (eq :refused (nck:apply-soul "testchan" "s-big" path)))))))
      (is (= 1 (length warnings)) "exactly one warning")
      (is (search "exceeds the harness budget" (first warnings)))
      (is (null (nlk:get-harness-section "s-big" nck:+soul-section+)))
      (is (search "refused" (getf (nck:channel-status "testchan") :soul)))))
  (nck:clear-channel-status "testchan"))

(deftest channel-soul-status-line ()
  (nck:clear-channel-status "testchan")
  (nck:set-channel-status "testchan" :state :running
                          :soul "/srv/SOUL.md (present)")
  (is (search "soul /srv/SOUL.md (present)" (nck:channels-status-report)))
  (nck:clear-channel-status "testchan"))
