;;;; composer-sigils-test.lisp --- the composer's sigil table.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. Without an override the file lane opens on @ exactly as before,
;;;;      even inside a word (a@b.de).
;;;;   2. sigils.json moves a lane to another character; @ is then text.
;;;;   3. A moved sigil opens its lane only at the start of the text or
;;;;      after whitespace: a&b, costs$5 and x#include stay text.
;;;;   4. An unusable override (several characters, a letter, a digit,
;;;;      whitespace, the slash lane's /, an unknown lane, broken JSON) leaves
;;;;      the default in force.
;;;;   5. Every image start discards a cached table, even at the same path.

(in-package #:nodecode.test)

(deftest composer-sigils-default-keeps-the-at-lane ()
  (is (eql #\@ (nle::composer-sigil :file)))
  ;; the default sigil keeps opening inside a word
  (is (equal "b.de" (nle::composer-lane-filter "a@b.de" :file)))
  (is (equal "src" (nle::composer-lane-filter "see @src" :file)))
  ;; whitespace after the token closes it
  (is (null (nle::composer-lane-filter "see @src now" :file)))
  (is (null (nle::composer-lane-filter "costs $5" :file)))
  (is (null (nle::composer-lane-filter "#include" :file))))

(deftest composer-sigils-override-moves-the-file-lane ()
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"&\"}")
    (is (eql #\& (nle::composer-sigil :file)))
    (is (null (nle::composer-lane-filter "mail a@b.de" :file)) "@ is plain text once the lane moved")
    (is (null (nle::composer-lane-filter "@src" :file)))
    (is (equal "src" (nle::composer-lane-filter "&src" :file)) "the start of the text opens it")
    (is (equal "src" (nle::composer-lane-filter "see &src" :file)) "a space before opens it")
    (is (null (nle::composer-lane-filter "a&b" :file)) "inside a word it is text")
    (is (null (nle::composer-lane-filter "costs $5" :file)))
    (is (null (nle::composer-lane-filter "#include" :file)))))

(deftest composer-sigils-moved-sigil-needs-a-word-start ()
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"$\"}")
    (is (null (nle::composer-lane-filter "costs$5" :file)))
    ;; at a word start the lane opens; rows show only where a file answers
    (is (equal "5" (nle::composer-lane-filter "costs $5" :file))))
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"#\"}")
    (is (null (nle::composer-lane-filter "x#include" :file)))
    (is (equal "include" (nle::composer-lane-filter "#include" :file)))))

(deftest composer-sigils-unusable-overrides-keep-the-default ()
  (dolist (document '("{\"file\": \"&&\"}" "{\"file\": \"a\"}" "{\"file\": \"7\"}"
                      "{\"file\": \" \"}" "{\"file\": \"/\"}" "{\"file\": \"\"}"
                      "{\"file\": 38}" "{\"nope\": \"&\"}" "not json"))
    (with-temp-file (nle::*composer-sigils-path* :contents document)
      (is (eql #\@ (nle::composer-sigil :file)) document)))
  (with-temp-file (nle::*composer-sigils-path*)
    (is (eql #\@ (nle::composer-sigil :file)) "a missing file keeps the default")))

(deftest composer-sigils-moved-lane-completes-through-the-composer ()
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"&\"}")
    (with-temp-workspace (dir "README.md" "src/main.lisp")
      (with-frame-model (model 80 24 :session-id "sigil-files")
        (setf model.file-catalog (nle::workspace-file-catalog dir) model.file-catalog-loaded t)
        (type-string model "mail a@sr")
        (is (null (nle::token-suggestions-for model)) "@ no longer offers files")
        (type-string model " &sr")
        (is (equal '("src/main.lisp")
                   (mapcar #'nle::composer-suggestion-text (nle::token-suggestions-for model))))
        (press model :tab)
        (is-composer model "mail a@sr &src/main.lisp " "Tab completes the token after the moved sigil")))))

(deftest composer-sigils-cached-table-is-fresh-at-every-image-start ()
  ;; A cell can populate the table before a warm image is saved. Its path
  ;; may stay the same on restart; its bytes need not. Run only this startup
  ;; parameter here so the test's private home/path bindings remain in force.
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"&\"}")
    (let ((nlk::*startup-parameters*
            (remove-if-not (lambda (entry) (eq (car entry) 'nle::*composer-sigils-read*))
                           nlk::*startup-parameters*)))
      (is (= 1 (length nlk::*startup-parameters*)) "the cache has one startup reset")
      (is (eql #\& (nle::composer-sigil :file)))
      (write-temp-file nle::*composer-sigils-path* "{\"file\": \"^\"}")
      (is (eql #\& (nle::composer-sigil :file)) "a running shell keeps its table")
      (nlk:resolve-startup-parameters)
      (is (null nle::*composer-sigils-read*) "a restored image forgets the saved table")
      (is (eql #\^ (nle::composer-sigil :file)) "next launch reads changed bytes at the same path")
      (delete-file nle::*composer-sigils-path*)
      (nlk:resolve-startup-parameters)
      (is (eql #\@ (nle::composer-sigil :file)) "removing the override restores the default"))))

(deftest composer-sigils-moved-lane-opens-after-all-whitespace ()
  (with-temp-file (nle::*composer-sigils-path* :contents "{\"file\": \"&\"}")
    (dolist (separator '(#\Space #\Tab #\Newline #\Return))
      (is (equal "src" (nle::composer-lane-filter (format nil "see~c&src" separator) :file))))))
