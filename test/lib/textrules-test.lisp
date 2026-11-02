;;;; textrules-test.lisp --- the pattern-rule engine's load-bearing behavior.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Migrated from the guard cell's policy tests, vocabulary stripped. The
;;;; guard's rule TABLES (what a mono-tool organism may and may not deny by
;;;; default) are guard data and stay proved in guard/test; what is proved
;;;; here is the engine: span-scoped allows, position matching, the gate,
;;;; and skip-with-warn compilation.

(in-package #:nodecode.test)

(defun text-ruleset (&rest keys)
  "COMPILE-TEXT-RULES with this file's default rule: \\brm\\b spans to a
recursive/force flag, ungated."
  (apply #'nlk:compile-text-rules
         :rules (list '(:id "rm-recursive"
                        :pattern "\\brm\\b[^)]{0,40}-[a-z]*[rf]\\b"
                        :reason "recursive or forced rm"))
         keys))

(deftest textrules-match-positions-not-substrings (let ((rules (text-ruleset))))
  ;; The reason matches are positions: "format" contains "rm", and a
  ;; SEARCH-based matcher would refuse every formatted output in a tree.
  (is (nlk:text-rules-refusal rules "rm -rf /tmp/x") "the real hit refuses")
  (is (not (nlk:text-rules-refusal rules "(format t \"~a\" x)")))
  (is (not (nlk:text-rules-refusal rules "confirm the swarm"))))

(deftest textrules-allow-override-is-span-scoped ()
  ;; The composable bypass a whole-text override would open: the allow
  ;; pattern matches the trailing literal, and a whole-text override would
  ;; exempt the real hit sitting next to it. Only the matched REGION is
  ;; exempt.
  (let ((rules (text-ruleset :allow '("\\brm -rf /tmp/scratch"))))
    (is (nlk:text-rules-refusal
         rules "(run \"rm -rf /\") \"rm -rf /tmp/scratch\""))
    (is (not (nlk:text-rules-refusal rules "rm -rf /tmp/scratch-7")))))

(deftest textrules-gate-arms-gated-rules-by-co-occurrence ()
  (let ((rules (nlk:compile-text-rules
                :gate "\\brun-program\\b"
                :rules '((:id "gated-rm" :pattern "\\brm\\b" :gated t)
                         (:id "always" :pattern "\\bnope\\b")))))
    (is (not (nlk:text-rules-refusal rules "the rm flag")))
    (is (nlk:text-rules-refusal rules "(run-program \"x\") rm y"))
    (is (nlk:text-rules-refusal rules "nope"))))

(deftest textrules-gated-rule-without-a-gate-is-skipped-loudly (let ((warned nil)))
  ;; Firing it ungated would flip the direction the gate protects.
  (handler-bind ((warning (lambda (condition)
                            (setf warned t)
                            (muffle-warning condition))))
    (let ((rules (nlk:compile-text-rules
                  :rules '((:id "orphan" :pattern "\\brm\\b" :gated t)))))
      (is warned "the skip is reported")
      (is (not (nlk:text-rules-refusal rules "rm -rf /"))))))

(deftest textrules-bad-pattern-is-skipped-not-fatal (handler-bind ((warning #'muffle-warning)))
  (let ((rules (nlk:compile-text-rules
                :rules '((:id "broken" :pattern "(unclosed")
                         (:id "fine" :pattern "\\bnope\\b"))
                :allow '("(also unclosed" "\\bfine\\b"))))
    (is (nlk:text-rules-refusal rules "nope") "the good rule compiled")
    (is (not (nlk:text-rules-refusal rules "anything else")))))

(deftest textrules-refusal-names-rule-reason-and-excerpt ()
  (let ((refusal (nlk:text-rules-refusal (text-ruleset) "rm -rf /tmp/x")))
    (is-carrying refusal ("rm-recursive" "the rule id is named")
      ("recursive or forced rm" "so is the reason") ("rm -rf" "and the matched text is quoted")))
  ;; a decoded form is a base string, and a tool call prints readably
  (is (search "matched \"rm -rf\"" (with-standard-io-syntax
                                       (nlk:text-rules-refusal (text-ruleset) (coerce "rm -rf /tmp/x" 'base-string))))))

(deftest textrules-excerpt-is-bounded ()
  ;; The scanned text is foreign-authored and unbounded; the refusal must
  ;; not carry the whole thing back.
  (is-let* ((long (format nil "rm -rf ~a"
                          (make-string 4000 :initial-element #\x)))
            (refusal (nlk:text-rules-refusal (text-ruleset) long)))
    (refusal is "still refused") ((< (length refusal) 400) is "the refusal stays small")))

(deftest textrules-non-string-text-abstains (let ((rules (text-ruleset))))
  (is-table (in in2) (not (nlk:text-rules-refusal in in2)) (rules nil "no text at all")
    (rules 42 "a non-string abstains") (nil "rm -rf /")))
