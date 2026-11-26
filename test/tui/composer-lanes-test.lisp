;;;; composer-lanes-test.lisp --- lanes a cell provides, in the composer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. A lane the gateway serves joins the sigil table under its name and
;;;;      its own sigil, which sigils.json moves as it moves the file lane.
;;;;   2. A lane's sigil opens it only at the start of the text or after
;;;;      whitespace, so x#include and costs$5 stay text.
;;;;   3. A sigil already taken -- the file lane's @ first, then the earlier
;;;;      lane -- leaves the later lane closed; moving the file lane frees @.
;;;;   4. The shell's effective table, as the lanes that opened, is what a
;;;;      start_turn carries.
;;;;   5. A lane's rows are the ones its last fetch landed, filtered by the
;;;;      typed token; no row answers, no list; Tab completes the name.
;;;;   6. A fetch's answer lands on the tick, into the model.

(in-package #:nodecode.test)

(defmacro with-lanes ((&rest lanes) &body body)
  "Run BODY with the gateway's lanes bound to LANES, (NAME SIGIL) each, and
no sigils.json."
  `(let ((nle::*gateway-lanes*
           (list ,@(mapcar (lambda (lane)
                             `(list :name ,(first lane) :sigil ,(second lane)
                                    :description ""))
                           lanes)))
         (nle::*composer-sigils-path* nil)
         (nle::*composer-sigils-read* nil))
     ,@body))

(deftest composer-lanes-join-the-sigil-table ()
  (with-lanes (("session" "#"))
    (is (eql #\# (nle::composer-sigil "session")))
    (is (eql #\@ (nle::composer-sigil :file)) "the file lane keeps @")
    (is (equal "ri" (nle::composer-lane-filter "ask #ri" "session")))
    (is (equal "ri" (nle::composer-lane-filter "#ri" "session")))
    (is (null (nle::composer-lane-filter "x#include" "session")) "inside a word it is text")
    (is (null (nle::composer-lane-filter "ask #ri now" "session")))))

(deftest composer-lanes-move-through-sigils-json ()
  (with-lanes (("session" "#"))
    (with-temp-file (nle::*composer-sigils-path* :contents "{\"session\": \"!\"}")
      (is (eql #\! (nle::composer-sigil "session")))
      (is (null (nle::composer-lane-filter "ask #ri" "session")))
      (is (equal "ri" (nle::composer-lane-filter "ask !ri" "session"))))))

(deftest composer-lanes-a-taken-sigil-leaves-the-later-lane-closed ()
  (with-lanes (("session" "@") ("domain" "#") ("room" "#") ("bad" "a"))
    (is (eql #\@ (nle::composer-sigil :file)))
    (is (null (nle::composer-sigil "session")) "the file lane holds @")
    (is (eql #\# (nle::composer-sigil "domain")))
    (is (null (nle::composer-sigil "room")) "the earlier lane holds #")
    (is (null (nle::composer-sigil "bad")) "a letter opens nothing")
    (is (null (nle::composer-lane-filter "ask #x" "room"))))
  (with-lanes (("session" "@"))
    (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"&\"}")
      (is (eql #\& (nle::composer-sigil :file)))
      (is (eql #\@ (nle::composer-sigil "session")) "moving the file lane frees @"))))

(deftest composer-lanes-effective-table-rides-start-turn ()
  (with-lanes (("session" "#") ("room" "#"))
    (is (equal '(("file" . "@") ("session" . "#")) (nle::composer-sigil-table)))
    (let ((sigils (gethash "sigils" (nle::start-turn-request "s" "hi"))))
      (is (hash-table-p sigils))
      (is (equal "@" (gethash "file" sigils)))
      (is (equal "#" (gethash "session" sigils)))
      (is (null (gethash "room" sigils)) "a closed lane is not in force"))))

(deftest composer-lanes-rows-filter-and-complete ()
  (with-lanes (("session" "#"))
    (with-frame-model (model 80 24 :session-id "lane-rows")
      (setf (nle::chat-model-lane-rows model)
            (list (list "session" (cons "right" "teachingco") (cons "left" "pult")
                        (cons "previous" "nodecode-cells"))))
      (type-string model "ask #ri")
      (is (equal '("right") (mapcar #'nle::composer-suggestion-text (nle::token-suggestions-for model))))
      (is (equal "teachingco" (nle::composer-suggestion-detail
                               (first (nle::token-suggestions-for model)))))
      (press model :tab)
      (is-composer model "ask #right " "Tab completes the lane's name after its sigil")
      (type-string model "#zz")
      (is (null (nle::token-suggestions-for model)) "no row answers, no list")
      (press model :backspace) (press model :backspace) (press model :backspace)
      (type-string model "x#ri")
      (is (null (nle::token-suggestions-for model)) "x#ri is text"))))

(deftest composer-lanes-leave-the-file-lane-alone ()
  (with-lanes (("session" "#"))
    (with-temp-workspace (dir "README.md" "src/main.lisp")
      (with-frame-model (model 80 24 :session-id "lane-files")
        (setf model.file-catalog (nle::workspace-file-catalog dir) model.file-catalog-loaded t)
        (setf (nle::chat-model-lane-rows model) (list (list "session" (cons "src" ""))))
        (type-string model "#x @sr")
        ;; the token nearest the caret decides the lane
        (is (equal '("src/main.lisp")
                   (mapcar #'nle::composer-suggestion-text (nle::token-suggestions-for model))))))))

(deftest composer-lanes-fetch-lands-on-the-tick ()
  (with-lanes (("session" "#"))
    (with-frame-model (model 80 24 :session-id "lane-land")
      (nle::hand-over-lane-rows model "session" (list (cons "right" "teachingco")))
      (is (null (nle::chat-model-lane-rows model)) "nothing moves before the tick")
      (is (nle::land-composer-catalog-walks model) "the tick says something landed")
      (is (equal '(("session" ("right" . "teachingco"))) (nle::chat-model-lane-rows model))))))
