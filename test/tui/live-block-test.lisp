;;;; live-block-test.lisp --- a running turn's live block holds still.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

;;; A round's calls stand under the run until the next round lists a call;
;;; the run and the rows under the bar keep the most rows the turn has used.
;;; Only the vertical: the esc cap still follows the bar's words.

(defun round-call (state index id note &optional result (turn "t1"))
  "STATE with eval ID, whose comment is NOTE, streamed at provider INDEX, its
fact landed, and its RESULT when given."
  (let* ((args (eval-args (format nil ";; ~a~%(+ 1 1)" note)))
         (state (nle::ingest-stream-tool state index "eval" args :turn-id turn))
         (state (nle::ingest-tool-call-fact state id "eval" args :turn-id turn)))
    (if result
        (nle::ingest-tool-result-fact state id "eval" result :turn-id turn)
        state)))

(defun run-plain-rows (state)
  "The plain rows of STATE's run as Standard projects it at 100 columns."
  (let* ((nle::*tool-detail* :standard) (nle::*expanded-tool-call* nil)
         (run (find-if (lambda (block) (nle::tool-group-p (nle::display-block-row block)))
                       (nle:project-state state :width 100))))
    (and run (nle::display-block-plain-lines run))))

(deftest a-running-turns-newest-round-stands-until-the-next-lists-a-call ()
  (let* ((state (nle::ingest-message-fact nil "user" "go" :turn-id "t1"))
         (state (nle::ingest-message-fact state "assistant" "" :reasoning "**Reading** both files" :turn-id "t1"))
         (state (round-call state 0 "a" "Read the first file" "1"))
         (state (round-call state 1 "b" "Read the second file" "2")))
    (let ((rows (run-plain-rows state)))
      (is (rows-carry-p "Ran 2 commands" rows) "the run counts both")
      (is (and (rows-carry-p "Read the first file" rows) (rows-carry-p "Read the second file" rows)) "and lists the round, finished as it is"))
    (setf state (nle::ingest-message-fact state "assistant" "" :reasoning "**Counting** the lines" :turn-id "t1"))
    (let ((rows (run-plain-rows state)))
      (is (and (rows-carry-p "Read the first file" rows) (rows-carry-p "Read the second file" rows)) "while the next round thinks"))
    (setf state (round-call state 0 "c" "Count the lines"))
    (let ((rows (run-plain-rows state)))
      (is (rows-carry-p "Count the lines" rows) "the next round's first call takes its place")
      (is (notany (lambda (note) (rows-carry-p note rows)) '("Read the first file" "Read the second file")) "and the round before goes into the count")
      (is (rows-carry-p "Ran 3 commands…" rows) "which counts it"))
    (setf state (nle::ingest-tool-result-fact state "c" "eval" "3" :turn-id "t1"))
    (is (rows-carry-p "Count the lines" (run-plain-rows state)) "finished, it stands while the turn runs")
    (setf state (nle::ingest-turn-terminal state "completed" :turn-id "t1"))
    (is (not (rows-carry-p "Count the lines" (run-plain-rows state))) "and goes with the turn")))

(deftest a-call-without-a-stream-index-is-a-round-of-its-own ()
  (let* ((state (nle::ingest-message-fact nil "user" "go" :turn-id "t1"))
         (state (tool-round state "a" "eval" (eval-args (format nil ";; Read the first file~%(+ 1 1)")) "1" :turn-id "t1"))
         (state (tool-round state "b" "eval" (eval-args (format nil ";; Read the second file~%(+ 1 1)")) "2" :turn-id "t1"))
         (rows (run-plain-rows state)))
    (is (rows-carry-p "Read the second file" rows) "the newest stands")
    (is (not (rows-carry-p "Read the first file" rows)) "the one before it has no round to share")))

(deftest the-live-bar-holds-its-row-while-rounds-give-their-place ()
  (with-frame-model (model 100 40 :session-id "steady-bar")
    (flet ((bar ()
             (nle::compose-frame model)
             (position-if (lambda (row) (search " esc " row)) (nle::frame-plain-rows model)))
           (under ()
             ;; The rows the block holds under the bar.
             (let* ((plains (second (nle::status-blocks model)))
                    (at (position-if (lambda (row) (and (stringp row) (search " esc " row))) plains)))
               (and at (- (length plains) at 1)))))
      (fold-model model (nle::ingest-message-fact "user" "go" :turn-id "t1"))
      (mark-turn-in-flight model :render t)
      (paint-model model
        (nle::ingest-message-fact "assistant" "" :reasoning "**Reading** both files" :turn-id "t1")
        (round-call 0 "a" "Read the first file" "1")
        (round-call 1 "b" "Read the second file" "2"))
      (let ((row (bar)))
        (is row "the bar shows")
        (paint-model model
          (nle::ingest-message-fact "assistant" "" :reasoning (format nil "**Counting** the lines~%~%~{~a~^ ~}" (loop repeat 30 collect "every line of both files counted once")) :turn-id "t1"))
        (is (eql row (bar)) "the next round's thought leaves it where it was")
        (let ((long (under)))
          (paint-model model (round-call 0 "c" "Count the lines"))
          (is (eql row (bar)) "a shorter round leaves canvas above it rather than pulling it up")
          (paint-model model (nle::ingest-message-fact "assistant" "" :reasoning "**Done** counting" :turn-id "t1"))
          (is (eql row (bar)) "and a shorter thought under it moves nothing")
          (is (= long (under)) "which keeps the rows the longer one took")))
      (fold-model model (nle::ingest-turn-terminal "completed" :turn-id "t1"))
      (rendering model (setf model.turn-in-flight nil))
      (is (null (bar)) "a finished turn takes the block with it"))))

(deftest a-finished-call-of-the-standing-round-keeps-its-output ()
  (let* ((args "{\"form\":\";; Tick twice\\n(progn (print 1) (print 2))\"}")
         (state (bg-state (bg-item-delta 0 "eval" args)
                          (bg-call-fact "ticks" args :tool-name "eval")))
         (live (first (nle::state-background-rows state))))
    (setf (nle::bg-band-row-output-tail live) (string-octets (format nil "tick 1~%tick 2~%"))
          (nle::bg-band-row-output-seen-lines live) 2)
    (is (rows-carry-p "⌎ tick 1" (run-plain-rows state)) "its output streams under it")
    (feed-events state (bg-result-fact "ticks" "eval" "=> 2"))
    (is (null (nle::state-background-rows state)) "its exec goes with its result")
    (let ((rows (run-plain-rows state)))
      (is (rows-carry-p "Tick twice" rows) "the call stands, its round not yet given up")
      (is (rows-carry-p "⌎ tick 1" rows) "with the output it showed"))))

