;;;; ledger-test.lisp --- a sighting is a line of the use ledger, by name.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: SIGHT refuses a kind it does not know, a sighting
;;;; without its verbatim quote, a name no definition carries and a
;;;; verification without its status or next step; it writes one ledger line
;;;; naming the definition, the session and the turn -- a reflection's
;;;; attributed to its origin and the turn it reflects on -- which the index
;;;; ranks by; SIGHTINGS reads them newest first, and /experience counts them.

(in-package #:nodecode.test)

(deftest experience-cell-sight-writes-a-ledger-line-by-name (with-experience-store ())
  (nlk:create-session :id "s-o" :cwd "/tmp")
  (keep-definition-text "(define-memory kn-quiet \"The operator wants quiet output.\" :type :feedback)")
  (keep-definition-text "(defun kn-runner () \"Run the thing.\" 1)")
  (let ((nlk:*scribe-session-id* "s-o"))
    (is (search "kind must be one of helped, harm, confounded, no-effect, call, verification"
                (experience-refusal #'experience:sight "gap" "kn-quiet" :quote "a verbatim quote")))
    (is (search "no quote, no sighting"
                (experience-refusal #'experience:sight "helped" "kn-quiet" :quote "short")))
    (is (search "no definition named kn-nowhere in the index"
                (experience-refusal #'experience:sight "helped" "kn-nowhere" :quote "a verbatim quote")))
    (is (search ":status and :next belong only to verification"
                (experience-refusal #'experience:sight "helped" "kn-quiet" :quote "a verbatim quote"
                                    :status "passed")))
    (is (search "names :next"
                (experience-refusal #'experience:sight "verification" "the build" :status "failed"
                                    :quote "the build went red")))
    (is (search "sighted helped kn-quiet for s-o"
                (experience:sight "helped" "kn-quiet" :quote "kept the output quiet as asked")))
    (is (search "sighted call kn-runner" (experience:sight "call" "kn-runner" :quote "(kn-runner) => 1")))
    (experience:sight "verification" "the build" :status "passed" :quote "just build complete"))
  ;; a reflection's sighting is its origin's, about the turn it reflects on
  (setf (gethash "experience-s-o-t7" nodecode-experience::*children*) (cons "s-o" "t7"))
  (let ((nlk:*scribe-session-id* "experience-s-o-t7"))
    (experience:sight "harm" "kn-quiet" :quote "quiet output hid the failing test" :note "too quiet")
    (is (search "sighted confounded kn-quiet"
                (experience:sight "confounded" "kn-quiet" :quote "quiet output and a new flag landed together")))
    (is (search "sighted no-effect kn-runner"
                (experience:sight "no-effect" "kn-runner" :quote "(kn-runner) => 1, and the plan stayed as it was"))))
  (let ((lines (nodecode-experience::experience-lines)))
    (is (equal '("helped" "call" "verification" "harm" "confounded" "no-effect")
               (mapcar (lambda (line) (nlk:json-value line :string "kind")) lines)))
    (is-present (harm (fourth lines)) "the reflection's line"
      (is (equal "s-o" (nlk:json-value harm :string "session")) "attributed to the origin")
      (is (equal "t7" (nlk:json-value harm :string "turn")) "and the turn it reflects on")
      (is (equal "too quiet" (nlk:json-value harm :string "note")))))
  (is (= 1 (car (gethash "kn-quiet" (nlk:knowledge-stats)))) "keep + helped - harm; confounded moves nothing")
  (is (= 2 (car (gethash "kn-runner" (nlk:knowledge-stats)))) "nor does a no-effect one: its keep and its call")
  (let ((listing (experience:sightings)))
    (is (search "harm kn-quiet s-o: \"quiet output hid the failing test\" (too quiet)" listing))
    (is (< (search "harm kn-quiet" listing) (search "helped kn-quiet" listing)) "newest first"))
  (is (search "verification the build/passed" (experience:sightings :name "the build")))
  (is (search "experience: 6 sightings (helped 1 · harm 1 · confounded 1 · no-effect 1 · call 1 · checks 1)"
              (nodecode-experience::summary-line))))
