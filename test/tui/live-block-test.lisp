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

(deftest a-streamed-delta-into-the-live-tail-reuses-its-projection-start ()
  ;; A delta that lands in the live tail row's own text buffer never touches
  ;; TAIL_ROWS' list cells (INGEST-STREAM-BODY's coalescing branch appends
  ;; into the row in place): following the bottom, the start
  ;; TAIL-PROJECTION-START chose on the render before is still the right
  ;; one, and TRANSCRIPT-PROJECTION-START must answer the very same cons
  ;; rather than pay to walk back and relay out the tail again.
  (with-frame-model (model 80 24 :session-id "stream-fast-path")
    (paint-model model (nle::ingest-stream-body :text "He" :turn-id "t1"))
    (is (nle::chat-model-follow-bottom model) "a live stream keeps the window pinned to the tail")
    (let ((entry (nle::chat-model-projection-start model))
          (tail (gethash "tail_rows" model.state)))
      (is entry "the first render chose a tail start")
      ;; More of the same delta's words append into the live row in place.
      (paint-model model (nle::ingest-stream-body :text "llo, world" :turn-id "t1"))
      (is (eq tail (gethash "tail_rows" model.state)) "TAIL_ROWS kept its own list cells")
      (is (eq entry (nle::chat-model-projection-start model)) "so the cached start is reused, not rewalked")
      (is (rows-carry-p "Hello, world" (nle::frame-plain-rows model)) "and the reused start still projects the grown row")
      ;; A new row — here, a tool call opening between two text rounds —
      ;; rebuilds TAIL_ROWS (APPEND-TAIL-ROW), so the fast path must step
      ;; aside and the walk below must run again.
      (paint-model model (round-call 0 "a" "Count the lines"))
      (is (not (eq tail (gethash "tail_rows" model.state))) "a new row changes TAIL_ROWS' identity")
      (is (not (eq entry (nle::chat-model-projection-start model))) "and the start is walked afresh, not reused stale"))))

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


;;; A picture the organism looked at stands under the call that saw it, in the
;;; Standard run, until the next call is made: with the rest of its round, and
;;; past the end of its turn.

(defun look-call (state index id note path &optional (turn "t1"))
  "STATE with eval ID, whose comment is NOTE, streamed at provider INDEX and
landed having looked at the 640x400 PNG at PATH: its result carries the
file fact LOOK stamps (exec.lisp IMAGE-FACT)."
  (let* ((args (eval-args (format nil ";; ~a~%(look ~s)" note path)))
         (state (nle::ingest-stream-tool state index "eval" args :turn-id turn))
         (state (nle::ingest-tool-call-fact state id "eval" args :turn-id turn)))
    (nle::ingest-tool-result-fact state id "eval" "[image: shown at the next round]"
                                  :turn-id turn
                                  :metadata (nlk:json-object
                                             "image" (nlk:json-object
                                                      "payload_kind" "file" "source" "look"
                                                      "path" path "png_path" path
                                                      "media_type" "image/png"
                                                      "width" 640 "height" 400 "bytes" 4096)))))

(defun standard-plain-rows (state)
  "Every plain row Standard projects STATE to at 100 columns, on a terminal
that draws pictures."
  (let ((nle::*tool-detail* :standard) (nle::*expanded-tool-call* nil) (nle::*image-protocol* :kitty))
    (loop for block in (nle:project-state state :width 100)
          append (coerce (nle::display-block-plain-lines block) 'list))))

(defparameter +look-caption+ "/tmp/chart.png · image/png · 640x400 · 4.0 KB")

(defun picture-under-p (note rows &aux (line (row-index-of note rows)) (caption (row-index-of +look-caption+ rows)))
  "Whether ROWS show the 640x400 picture's band and caption right under the
line of the call whose comment is NOTE."
  (and line caption
       (= (- caption line 1)
          (nle::image-preview-rows '(:width 640 :height 400) 100))
       (loop for row from (1+ line) below caption
             always (zerop (length (string-trim " " (nth row rows)))))))

(deftest a-look-s-picture-stands-under-its-call-until-the-next-call ()
  (let* ((state (nle::ingest-message-fact nil "user" "go" :turn-id "t1"))
         (state (round-call state 0 "a" "Read the notes" "1"))
         (state (nle::ingest-message-fact state "assistant" "" :reasoning "**Charting** it" :turn-id "t1"))
         (state (look-call state 0 "b" "Look at the chart" "/tmp/chart.png")))
    (let ((rows (standard-plain-rows state)))
      (is (picture-under-p "Look at the chart" rows) "the picture stands under the call that saw it")
      (is (not (rows-carry-p "Read the notes" rows)) "the round before went into the count"))
    (setf state (nle::ingest-message-fact state "assistant" "" :reasoning "**Counting** the lines" :turn-id "t1"))
    (is (picture-under-p "Look at the chart" (standard-plain-rows state)) "while the next round thinks")
    (setf state (round-call state 0 "c" "Count the lines"))
    (let ((rows (standard-plain-rows state)))
      (is (rows-carry-p "Count the lines" rows) "the next call takes its place")
      (is (notany (lambda (needle) (rows-carry-p needle rows)) (list "Look at the chart" +look-caption+)) "and the picture goes with its round"))
    (let ((nle::*tool-detail* :developer) (nle::*image-protocol* :kitty))
      (is (rows-carry-p +look-caption+ (loop for block in (nle:project-state state :width 100)
                                             append (coerce (nle::display-block-plain-lines block) 'list))) "Developer keeps its card"))))

(deftest a-look-in-a-parallel-round-shows-its-picture-with-the-rest ()
  (let* ((state (nle::ingest-message-fact nil "user" "go" :turn-id "t1"))
         (state (round-call state 0 "a" "Read the notes" "1"))
         (state (look-call state 1 "b" "Look at the chart" "/tmp/chart.png"))
         (state (round-call state 2 "c" "List the files" "3")))
    (let ((rows (standard-plain-rows state)))
      (is (picture-under-p "Look at the chart" rows) "the picture stands under its call")
      (is (< (row-index-of "Read the notes" rows) (row-index-of "Look at the chart" rows)
             (row-index-of +look-caption+ rows) (row-index-of "List the files" rows)) "among its round's calls, in the order they were made"))
    (setf state (round-call state 0 "d" "Count the lines"))
    (let ((rows (standard-plain-rows state)))
      (is (notany (lambda (needle) (rows-carry-p needle rows))
                  (list "Read the notes" "Look at the chart" +look-caption+ "List the files")) "the next round's first call takes the place of the whole round"))))

(deftest a-look-s-picture-stands-past-its-turn-until-a-call-is-made ()
  (let* ((state (nle::ingest-message-fact nil "user" "go" :turn-id "t1"))
         (state (look-call state 0 "b" "Look at the chart" "/tmp/chart.png"))
         (state (nle::ingest-message-fact state "assistant" "It rises." :turn-id "t1"))
         (state (nle::ingest-turn-terminal state "completed" :turn-id "t1")))
    (let ((rows (standard-plain-rows state)))
      (is (picture-under-p "Look at the chart" rows) "the answer is no call: the picture stands")
      (is (< (row-index-of +look-caption+ rows) (row-index-of "It rises." rows)) "over the answer"))
    (setf state (nle::ingest-message-fact state "user" "and now?" :turn-id "t2")
          state (nle::ingest-message-fact state "assistant" "" :reasoning "**Checking** again" :turn-id "t2"))
    (is (picture-under-p "Look at the chart" (standard-plain-rows state)) "nor is the next turn's thinking")
    (setf state (round-call state 0 "c" "Count the lines" nil "t2"))
    (let ((rows (standard-plain-rows state)))
      (is (rows-carry-p "Count the lines" rows) "the next turn's first call lists")
      (is (notany (lambda (needle) (rows-carry-p needle rows)) (list "Look at the chart" +look-caption+)) "and the picture goes with the turn that saw it"))))

(deftest a-picture-anchors-under-its-call-and-leaves-no-canvas-when-it-goes ()
  (with-frame-model (model 100 60 :session-id "look-round")
    (nle:update model (nlt:make-kitty-graphics-msg :ok-p t))
    (fold-model model (nle::ingest-message-fact "user" "go" :turn-id "t1"))
    (mark-turn-in-flight model :render t)
    (paint-model model
      (round-call 0 "a" "Read the notes" "1")
      (nle::ingest-message-fact "assistant" "" :reasoning "**Charting** it" :turn-id "t1")
      (look-call 0 "b" "Look at the chart" "/tmp/chart.png"))
    (setf (gethash "b" model.kitty-image-cache) (nlt:make-terminal-image :id 7))
    (nle::compose-frame model)
    (let ((placement (first (nle::image-placements model))))
      (is placement "the standing round's picture is placed")
      (when placement
        (let ((index (nle::band-window-row-index
                      model.band (- placement.y 3 (nth-value 3 (nle::transcript-band-metrics model))))))
          (is (search "Look at the chart" (aref model.line-plain (1- index))) "right under its call's line")
          (is (zerop (length (string-trim " " (aref model.line-plain index)))) "over its own band"))))
    (paint-model model (round-call 0 "c" "Count the lines"))
    (nle::compose-frame model)
    (is (null (nle::image-placements model)) "the next call takes the picture away")
    (is (< (nle::run-lift model 0) 3) "and the rows it took do not stand over the bar as canvas")))

