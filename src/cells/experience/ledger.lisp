;;;; ledger.lisp --- sightings, as lines of the core's use ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A sighting is one claim about one turn, always with a verbatim quote
;;;; (backpass's quoteless-discard rule, mechanical here): a definition of
;;;; the index that helped, or whose following did harm; a definition of the
;;;; organism's own that ran and did its job; a verification over a target.
;;;; It is one line of the use ledger the index ranks by (NLK:NOTE-KNOWLEDGE-USE,
;;;; usage.jsonl under the home), naming the definition, the session and the
;;;; turn it is about, and carrying the quote: the ranking is the backward
;;;; pass, so there is no fold of its own to run.

(in-package #:nodecode-experience)

(defparameter +kinds+ '("helped" "harm" "confounded" "no-effect" "call" "verification")
  "helped: a definition of the index was read or followed, and a moment of
the turn shows it paid. harm: following one caused damage. confounded: it
was in play, but the turn cannot tell its part from the rest (several
changes at once, or one outcome all that speaks for it). no-effect: read or
followed, and nothing in the turn changed because of it. call: a definition
of the organism's own ran and did its job. verification: an evidence check
over a target, with a status and, unless it passed, the smallest next step.
The index ranks by helped and harm (NLK::LEDGER-WEIGHT); confounded and
no-effect weigh nothing, so an unproven credit moves no rank.")

(defun experience-lines (&key name)
  "The ledger's sightings -- its lines of +KINDS+ that carry a quote --
oldest first, about NAME when given."
  (loop for line in (nlk:knowledge-ledger-lines)
        when (and (member (nlk:json-value line :string "kind") +kinds+ :test #'equal)
                  (nlk:json-value line :string "quote")
                  (or (null name) (equal name (nlk:json-value line :string "name"))))
          collect line))

(defun turn-span (origin turn)
  "(values START END): the universal times TURN of ORIGIN started and
ended, or NIL for a turn without both."
  (flet ((at (kinds)
           (nlk:iso-universal (nlk:events :session-id origin :turn-id turn :kind kinds
                                          :as :value :columns '("occurred_at")))))
    (let ((start (at nlk::+kind-turn-started+))
          (end (at (list nlk::+kind-turn-completed+ nlk::+kind-turn-failed+
                         nlk::+kind-turn-cancelled+))))
      (and start end (values start end)))))

(defun turn-reads (origin turn)
  "The definitions TURN of ORIGIN read through help, distinct, in the order
read: the ledger's view lines of ORIGIN inside the turn's span."
  (multiple-value-bind (start end) (turn-span origin turn)
    (when start
      (nlk:distinct
       (loop for line in (nlk:knowledge-ledger-lines)
             for at = (nlk:iso-universal (nlk:json-value line :string "at"))
             when (and at (<= start at end)
                       (equal "view" (nlk:json-value line :string "kind"))
                       (equal origin (nlk:json-value line :string "session")))
               collect (nlk:json-value line :string "name"))))))
