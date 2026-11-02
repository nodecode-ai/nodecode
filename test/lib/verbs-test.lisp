;;;; verbs-test.lisp --- a command line's words: the nearest spelling, the
;;;; lookup, the help and the refusal that all read one table.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest verbs-edit-distance-counts-single-edits ()
  (is-table (distance typed word) (= distance (nlk:edit-distance typed word))
    (0 "profile" "profile") (1 "prfile" "profile" "an insertion")
    (1 "profiles" "profile" "a deletion") (1 "profilr" "profile" "a substitution")
    (1 "porfile" "profile" "two neighbours swapped are one edit") (3 "kitten" "sitting")
    (3 "" "abc") (1 "Profile" "profile" "case is an edit")))

(deftest verbs-nearest-words-guess-only-when-close ()
  (let ((words '("serve" "setup" "import" "profile" "resume" "help")))
    (is-table (guess typed) (equal '(guess) (nlk:nearest-words typed words))
      ("profile" "prfile") ("profile" "profiles") ("serve" "sevre")
      ("profile" "prof" "the one word it begins"))
    (is-table (typed) (null (nlk:nearest-words typed words))
      ("se" "two words begun: no guess") ("zzz" "nothing close: no guess") ("")))
  (is (equal '("ac" "bb") (nlk:nearest-words "ab" '("ac" "bb" "zzzz"))) "ties come in table order")
  (is (= 3 (length (nlk:nearest-words "a" '("b" "c" "d" "e")))) "at most three"))

(deftest verbs-find-help-and-refusal-read-one-table ()
  (let ((verbs (list (nlk:make-verb "profile" "profiles" :aliases '("profiles") :arguments "[VERB]")
                     (nlk:make-verb "serve" "run headless" :arguments "[--store PATH]"
                                    :detail "More words.")
                     (nlk:make-verb "trial" "the organism's own child" :internal t)
                     (nlk:make-verb "--profile" "run a profile" :aliases '("-p")
                                    :arguments "NAME" :label "-p, --profile NAME"))))
    (is (eq (first verbs) (nlk:find-verb "profiles" verbs)) "an alias finds its entry")
    (is (eq (fourth verbs) (nlk:find-verb "-p" verbs)))
    (is (null (nlk:find-verb "Profile" verbs)) "exact spelling only")
    (is (eq (third verbs) (nlk:find-verb "trial" verbs)) "an internal verb still parses")
    (is (nlk:help-flag-p "--help"))
    (is (nlk:help-flag-p "-h"))
    (is (not (nlk:help-flag-p "help")) "the word is a verb of its own table, not a flag")
    (is (equal "no command \"prfile\". Did you mean \"profile\"?"
               (nlk:unknown-word-text "command" "prfile" verbs)))
    (is (equal "no command \"trail\"." (nlk:unknown-word-text "command" "trail" verbs)))
    (is (equal "no option \"-P\". Did you mean \"-p\"?" (nlk:unknown-word-text "option" "-P" verbs)))
    (let ((text (nlk:verbs-text verbs)))
      (is (search "  profile [VERB]" text) "name and arguments")
      (is (search "  -p, --profile NAME" text) "a label instead")
      (is (null (search "trial" text)) "internal verbs are not listed")
      (is (every (lambda (line) (> (length line) 26)) (nlk:lines text))))
    (is (equal (format nil "usage: nodecode serve [--store PATH]~%~%Run headless.~%~%More words.~%")
               (nlk:verb-help-text (second verbs) :command "nodecode")))))
