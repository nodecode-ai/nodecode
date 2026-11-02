;;;; notice-log-test.lisp --- the log of what was said.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; THE BOARD STANDS; THE LOG REMEMBERS. Without the log, a keyless notice
;;;; (gateway.lisp NOTICE) would reach only the shells attached while it was
;;;; being said. A shell attaching later would replay no board entry for it,
;;;; and the model -- whose prompt's notices block IS the board -- would not
;;;; hear it. These gates are about the two surfaces separately: what stands,
;;;; and what was said.
;;;;
;;;; Its own file on purpose: the board, the log and NOTICE are kernel
;;;; surfaces, and the shared test files under test/surface/ are lanes of
;;;; their own on a dirty checkout.

(in-package #:nodecode.test)

(deftest notice-log-remembers-what-was-said-not-what-stands ()
  (nle:notice "said once" :level :warning)
  (nle:notice "said standing" :key "probe.notice-log")
  ;; Newest first, keyed and keyless alike, each with the level and the time
  ;; a reader of the failure wants beside the text.
  (is (equal '("said standing" "said once") (mapcar #'first (nlk:notice-log))))
  (is (equal '(:info :warning) (mapcar #'second (nlk:notice-log))))
  (is (integerp (third (first (nlk:notice-log)))))
  (is (equal '("probe.notice-log") (mapcar #'first (nlk:standing-notices))) "board: keyed only")
  ;; A clear says nothing: NIL under a key takes the line down, and is not a
  ;; thing that was said.
  (nle:notice nil :key "probe.notice-log")
  (is (null (nlk:standing-notices)))
  (is (equal '("said standing" "said once") (mapcar #'first (nlk:notice-log))))
  ;; A ring, not a ledger: the oldest past its size falls off.
  (let ((nlk::*notice-log* '())
        (nlk:+notice-log-size+ 2))
    (nle:notice "one")
    (nle:notice "two")
    (nle:notice "three")
    (is (equal '("three" "two") (mapcar #'first (nlk:notice-log))) "the newest two")
    (is (= 2 (length (nlk:notice-log))))
    (is (equal '("three") (mapcar #'first (nlk:notice-log :limit 1))) "LIMIT reads the newest")))

(deftest notice-log-is-what-the-prompt-does-not-carry ()
  ;; The live section the model reads each round is the board
  ;; (context.lisp READ-LIVE-SECTIONS -> NLK:STANDING-NOTICES-TEXT), so a
  ;; keyless notice is invisible there while the log has it: the two answers
  ;; are different questions, and this is the one the model asks after a
  ;; failure it was told about only in prose.
  (nle:notice "mcp: server at https://tools.example/x failed: 502")
  (is (null (nlk:standing-notices-text)) "the board says nothing")
  (is (search "502" (first (first (nlk:notice-log)))) "the log has it"))
