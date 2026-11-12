;;;; lanes-test.lisp --- the composer lanes cells register in the gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. A registered lane is listed with its sigil at GET /api/gateway/lanes;
;;;;      its owner's stop takes it out.
;;;;   2. POST {lane} answers the rows the cell's function gives now, strings
;;;;      and (TEXT DETAIL) lists alike, bounded.
;;;;   3. A function that signals answers no rows and says why; an unknown
;;;;      lane refuses.
;;;;   4. A lane needs a name and a usable sigil.

(in-package #:nodecode.test)

(deftest gateway-lanes-register-list-and-answer-rows (with-temp-gateway (port))
  (with-saved-globals (nle::*registered-lanes*)
    (nle:register-composer-lane "cell-a" "session" "#"
                                (lambda () (list "left" (list "right" "teachingco")))
                                :description "sessions")
    (nle:register-composer-lane "cell-b" "boom" "!" (lambda () (error "no world")))
    (with-gateway-http (port :get "/api/gateway/lanes")
      (let ((lanes (coerce (gethash "lanes" body) 'list)))
        (is (= 200 status))
        (is (equal '("session" "boom") (mapcar (lambda (lane) (gethash "name" lane)) lanes)))
        (is (equal "#" (gethash "sigil" (first lanes))))
        (is (equal "sessions" (gethash "description" (first lanes))))))
    (with-gateway-http (port :post "/api/gateway/lanes" :body "{\"lane\":\"session\"}")
      (is (equal '(("left" "") ("right" "teachingco"))
                 (map 'list (lambda (row) (list (gethash "text" row) (gethash "detail" row)))
                      (gethash "rows" body)))))
    (with-gateway-http (port :post "/api/gateway/lanes" :body "{\"lane\":\"boom\"}")
      (is (zerop (length (gethash "rows" body))))
      (is (search "no world" (gethash "error" body))))
    (is-route (port :post "/api/gateway/lanes" :body "{\"lane\":\"nope\"}") 400
              "an unknown lane refuses" "invalid_request")
    (is (= 1 (nle:unregister-composer-lanes "cell-a")))
    (is (equal '("boom") (mapcar (lambda (lane) (getf lane :name)) nle::*registered-lanes*)))))

(deftest gateway-lanes-refuse-unusable-declarations ()
  (with-saved-globals (nle::*registered-lanes*)
    ;; a usable declaration registers, so the refusals below are the checks'
    (is (nle:register-composer-lane "c" "x" "#" (constantly nil)))
    (is (signals-error error (nle:register-composer-lane "c" "" "#" (constantly nil))))
    (is (signals-error error (nle:register-composer-lane "c" "x" "ab" (constantly nil))))
    (is (signals-error error (nle:register-composer-lane "c" "x" "/" (constantly nil))))
    ;; the builtin file lane's name is taken
    (is (signals-error error (nle:register-composer-lane "c" "file" "&" (constantly nil))))))

(deftest gateway-lanes-rows-are-bounded ()
  (with-saved-globals (nle::*registered-lanes*)
    (nle:register-composer-lane "c" "many" "#"
                                (lambda () (loop for i below 2000 collect (format nil "r~d" i))))
    (is (= nle::*composer-lane-row-limit* (length (nle::composer-lane-rows "many"))))))

(deftest gateway-lanes-sigils-from-wire-reads-the-shells-table ()
  (is (null (nle::sigils-from-wire nil)) "a shell that sent none")
  (is (null (nle::sigils-from-wire "#")))
  (let ((table (nle::sigils-from-wire (nlk:make-json-object "file" "@" "session" "#" "bad" "ab"))))
    (is (equal '(("file" . #\@) ("session" . #\#))
               (sort (copy-list table) #'string< :key #'car)))))
