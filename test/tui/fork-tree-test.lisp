;;;; fork-tree-test.lisp --- a call that forked shows its forks under it: the
;;;; live session_fork_progress frame folds onto the call's own rows, and the
;;;; Standard viewport draws one line a fork on the rails /sessions draws, both
;;;; under the call's card and under its line in a running round's listing.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun fork-tree-call (call-id note)
  "The started fact of eval call CALL-ID whose form's note is NOTE."
  (nlk::make-json-object "type" "turn.tool_call_started" "turn_id" "turn-1"
                         "call-id" call-id "tool-name" "eval"
                         "arguments" (nlk::encode-json-object
                                      (nlk:json-object "form" (format nil ";; ~a~%(fork \"a\" \"b\")" note)))))

(defun fork-tree-frame (call-id &rest forks)
  "The live session_fork_progress frame saying how CALL-ID's FORKS stand."
  (nlk:json-object "type" "session_fork_progress" "session_id" "s-fork"
                   "tool_call_id" call-id "forks" (coerce forks 'vector)))

(defun fork-tree-forks ()
  "Three forks: one running, one done, one cancelled."
  (list (nlk:json-object "n" 1 "label" "gate" "status" "running" "calls" 2 "note" "read the gate")
        (nlk:json-object "n" 2 "label" "dpad" "status" "completed" "calls" 3 "ms" 126000 "answer" "dpad is a shim")
        (nlk:json-object "n" 3 "label" "snap hook" "status" "cancelled" "calls" 15 "ms" 385000 "answer" "the call stopped")))

(defun fork-tree-fold (state &rest facts)
  "STATE with each of FACTS folded through the shell's one reducer."
  (dolist (fact facts state)
    (setf state (nle::apply-gateway-event state fact))))

(defun fork-tree-of (state call-id)
  "The forks call CALL-ID's row carries in STATE, or NIL."
  (let ((row (nle::find-transcript-row (lambda (row) (equal call-id (nle::transcript-row-tool-call-id row))) state)))
    (and row (hash-table-p (nle::transcript-row-metadata row))
         (gethash "forks" (nle::transcript-row-metadata row)))))

(deftest fork-tree-frame-rides-the-call-that-forked ()
  (let ((state (fork-tree-fold nil (fork-tree-call "c-fork" "three at once") (fork-tree-call "c-other" "something else")
                               (apply #'fork-tree-frame "c-fork" (fork-tree-forks)))))
    (is (= 3 (length (fork-tree-of state "c-fork"))) "the frame's forks ride the call that made them")
    (is (null (fork-tree-of state "c-other")) "and no other call")
    (setf state (fork-tree-fold state (fork-tree-frame "c-fork" (second (fork-tree-forks)))))
    (is (= 1 (length (fork-tree-of state "c-fork"))) "a later frame says how they stand now, in place of the last")
    (is (fork-tree-fold state (fork-tree-frame "c-gone" (first (fork-tree-forks)))) "a frame for a call this shell has no row of changes nothing and fails nothing")))

(deftest fork-tree-standard-draws-one-line-a-fork-under-the-call ()
  (let* ((state (fork-tree-fold nil (fork-tree-call "c-fork" "three at once")
                                (apply #'fork-tree-frame "c-fork" (fork-tree-forks))))
         (row (nle::find-transcript-row (lambda (row) (eq :tool-call (nle::transcript-row-kind row))) state))
         (lines (let ((nle::*tool-detail* :standard) (nle::*color-profile* :none))
                  (mapcar #'nle::lline-text (nle::row-llines row 100)))))
    (is (= 4 (length lines)) "the call's line, then one line a fork")
    (is (search "three at once" (first lines)))
    (is (search "├ ○ fork 1  gate" (second lines)) "a running fork: an open mark on the rail /sessions draws")
    (is (search "2 calls" (second lines)) "its calls so far")
    (is (search "read the gate" (second lines)) "and its newest note")
    (is (search "├ ✓ fork 2  dpad" (third lines)) "an ended fork: its mark")
    (is (search "3 calls · 2m 06s" (third lines)) "its time")
    (is (search "dpad is a shim" (third lines)) "and its answer's first line")
    (is (= (search "read the gate" (second lines)) (search "dpad is a shim" (third lines)) (search "the call stopped" (fourth lines))) "every note starts in one column, however wide each fork's calls, time and ending")
    (is (search "╰ ✗ fork 3  snap hook" (fourth lines)) "the last fork closes the rail")
    (is (search "cancelled" (fourth lines)) "and one that did not complete says how it ended")
    (is (every (lambda (line) (<= (nle::text-display-width line) 100)) lines) "no line runs past the width")))

(deftest fork-tree-a-running-round-lists-the-forks-under-the-call ()
  (let* ((state (fork-tree-fold nil
                                (nlk:json-object "type" "turn.input_committed" "turn_id" "turn-1" "role" "user" "message" "go"
                                                 "prompt-id" "p1" "sequence" 1 "disposition" "started")
                                (fork-tree-call "c-look" "look first")
                                (fork-tree-call "c-fork" "three at once")
                                (apply #'fork-tree-frame "c-fork" (fork-tree-forks))))
         (plain (let ((nle::*tool-detail* :standard) (nle::*color-profile* :none))
                  (loop for block in (nle:project-state state :width 100)
                        append (coerce (nle::display-block-plain-lines block) 'list))))
         (call (position-if (lambda (line) (search "three at once" line)) plain)))
    (is call "the forking call is listed under the folded run")
    (is (and call (search "├ ○ fork 1  gate" (nth (+ call 1) plain))) "its forks stand right under its line")
    (is (and call (search "╰ ✗ fork 3  snap hook" (nth (+ call 3) plain))) "all of them")
    (is (and call (= (position #\├ (nth (+ call 1) plain)) (position-if #'alpha-char-p (nth call plain)))) "the rail stands under the call's first word, not two columns in")))
