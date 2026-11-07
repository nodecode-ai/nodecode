;;;; feed-test.lisp --- the bundled feed is whole; the live one wins and a failure says so.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest cyber-security-the-bundled-feed-may-be-published ()
  (let* ((text (uiop:read-file-string (nodecode-cyber-security::packaged-path)))
         (problems (nodecode-cyber-security:feed-problems text)))
    (is (null problems) (format nil "the shipped feed passes the publish gate:~{~%  ~a~}" problems))
    (is (plusp (length (nodecode-cyber-security::read-feed text))) "the feed carries advisories")))

(deftest cyber-security-a-broken-feed-is-refused-with-its-reasons ()
  (flet ((problems (advisory)
           (nodecode-cyber-security:feed-problems
            (format nil "{\"format\":1,\"updated\":\"2026-10-22T10:00:00Z\",\"advisories\":[~a]}" advisory)))
         (good (&rest more)
           (format nil "{\"id\":\"x-1\",\"title\":\"X\",\"date\":\"2026-10-22\",\"severity\":\"high\",\"exploited\":false,\"summary\":\"s\",\"affected\":\"a\",\"patch\":[\"upgrade\"],\"sources\":[\"https://example.com/a\"]~{,~a~}}" more)))
    (is (null (problems (good))) "a whole advisory passes")
    (is (null (problems (good "\"checks\":[{\"kind\":\"npm\",\"package\":\"p\",\"fixed\":[\"1.2.3\"]}]"))) "an npm check with its fields passes")
    (is (search "not JSON" (first (nodecode-cyber-security:feed-problems "{nope"))) "broken JSON is named")
    (is (search "kind is not one of" (format nil "~{~a~}" (problems (good "\"checks\":[{\"kind\":\"rpm\"}]")))))
    (is (search "does not parse" (format nil "~{~a~}" (problems (good "\"checks\":[{\"kind\":\"version\",\"command\":[\"php\",\"-v\"],\"fixed\":[\"latest\"]}]")))))
    (is (search "names neither fixed nor bad" (format nil "~{~a~}" (problems (good "\"checks\":[{\"kind\":\"wp-plugin\",\"slugs\":[\"s\"]}]")))))
    (is (search "sources are not links" (format nil "~{~a~}" (problems (cl-ppcre:regex-replace "https://example.com/a" (good) "see X")))))
    (is (search "exploited is missing" (format nil "~{~a~}" (problems (cl-ppcre:regex-replace ",\"exploited\":false" (good) "")))))
    (is (search "id is used twice" (format nil "~{~a~}" (problems (format nil "~a,~a" (good) (good))))))
    (is (search "advisories is not a non-empty list"
                (format nil "~{~a~}" (nodecode-cyber-security:feed-problems "{\"format\":1,\"updated\":\"2026-10-22T10:00:00Z\",\"advisories\":[]}"))))))

(deftest cyber-security-the-live-feed-wins-and-a-failure-says-so (with-stubbed-security)
  (let ((live "{\"format\":1,\"updated\":\"2026-10-23T00:00:00Z\",\"advisories\":[{\"id\":\"only\",\"title\":\"One\",\"checks\":[]}]}"))
    ;; never answered: the bundled copy, and the reason
    (nodecode-cyber-security:load-feed :fetch t)
    (is (eq :bundled (getf nodecode-cyber-security::*feed-state* :source)))
    (is (>= (length nodecode-cyber-security::*feed*) 20))
    (is (search "live fetch failed" (nodecode-cyber-security:feed-status)) "the status says the live feed failed")
    ;; answered: the live copy is the authority
    (setf *sp-fetch-answer* (list live 200))
    (nodecode-cyber-security:load-feed :fetch t)
    (is (eq :remote (getf nodecode-cyber-security::*feed-state* :source)))
    (is (equal '("only") (mapcar (lambda (a) (gethash "id" a)) nodecode-cyber-security::*feed*)))
    (is (search "live, written 2026-10-23" (nodecode-cyber-security:feed-status)))
    ;; answered before, failing now: that copy stays, and the status says so
    (setf *sp-fetch-answer* '("not json" 200))
    (nodecode-cyber-security:load-feed :fetch t)
    (is (eq :stale (getf nodecode-cyber-security::*feed-state* :source)))
    (is (equal '("only") (mapcar (lambda (a) (gethash "id" a)) nodecode-cyber-security::*feed*)))
    (is (search "is not a feed" (nodecode-cyber-security:feed-status)))))
