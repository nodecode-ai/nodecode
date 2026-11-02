;;;; scaling.lisp --- the hot paths, and how their cost may grow.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each case states a contract (SOURCE.LISP) and a SETUP that builds what a
;;;; person holds at size N -- a transcript of N exchanges, a session of N
;;;; turns, N rows of output -- and answers the call to time at that size.
;;;; Sizes are what a heavy day reaches, so a cost that grows with them shows
;;;; at CI's sizes the way it shows on that day.

(in-package #:nodecode.perf.scaling)

(defvar *cases* '()
  "(NAME CONTRACT SIZES SETUP), in the order they were defined.")

(defmacro define-scaling (name (size &key (kind :linear) bound known (sizes ''(250 1000 4000)))
                          &body setup)
  "Define the case NAME: SETUP, run with SIZE bound in an open temp store,
answers a function of one argument (the call's index in its batch) whose
cost KIND bounds: a slope of at most 0.10 for :CONSTANT, 1.10 for :LINEAR.
BOUND, with KNOWN naming where the breach is written up, holds a path that
does not keep KIND yet to the slope it has today."
  `(progn
     (setf *cases* (append (remove ',name *cases* :key #'first)
                           (list (list ',name
                                       (make-scaling-contract
                                        :kind ,kind
                                        :bound ,(or bound (if (eq kind :constant) 0.10 1.10))
                                        :known ,known)
                                       ,sizes
                                       (lambda (,size) ,@setup)))))
     ',name))

;;; --- the shell --------------------------------------------------------------

(defun transcript-model (exchanges &key (width 100) (height 40))
  "A WIDTH x HEIGHT shell over a transcript of EXCHANGES question/answer
pairs, each answer a few paragraphs, painted once."
  (let ((model (make-frame-model width height :session-id "perf-shell")))
    (setf (nle::chat-model-state model)
          (let ((state (nle::chat-model-state model)))
            (dotimes (i exchanges state)
              (setf state (exchange state (format nil "question ~d: what does part ~d do?" i i)
                                    (answer-text i))))))
    (nle::render-state model)
    (nle::compose-frame model)
    model))

(defun answer-text (i)
  "An assistant answer of three short paragraphs and a list, numbered I."
  (format nil "Part ~d reads the folder and keeps what it found.~%~%~
               - it opens the file~%- it reads the rows~%- it writes the summary~%~%~
               That is the whole of part ~d." i i))

(defun key (code)
  (nlt:make-key-press-msg :code code))

(define-scaling keystroke-in-a-long-transcript (n :kind :constant)
  ;; A character typed and taken back with a long conversation above the
  ;; composer: the update and the frame it paints must not see the
  ;; conversation's length.
  (let ((model (transcript-model n))
        (type (key #\x))
        (erase (key :backspace)))
    (lambda (i)
      (nle:update model (if (evenp i) type erase))
      (nle::compose-frame model))))

(define-scaling a-streamed-word-in-a-long-transcript
    (n :kind :constant :bound 0.85
       :known "main 25bc955: instructions n^0.55, bytes n^0.79; TAIL-PROJECTION-START runs GROUP-ROWS over every row per look")
  ;; A word of a streaming answer landing at the tail of a long conversation:
  ;; the fold, the re-projection of the live tail and the frame, as the frame
  ;; clock paints a dirty transcript.
  (let ((model (transcript-model n)))
    (lambda (i)
      (if (zerop (mod (1+ i) 64))
          (fold-model model (nle::ingest-message-fact "assistant" "done"))
          (fold-model model (nle::ingest-stream-body :text "word ")))
      (nle::render-state model)
      (nle::compose-frame model))))

(define-scaling paging-in-a-long-transcript (n :kind :constant)
  ;; PgUp then PgDn at the tail of a long conversation.
  (let ((model (transcript-model n))
        (up (key :page-up))
        (down (key :page-down)))
    (lambda (i)
      (nle:update model (if (evenp i) up down))
      (nle::compose-frame model))))

(define-scaling projecting-a-transcript (n)
  ;; Laying out every block of a transcript (Home, a resize, a theme change):
  ;; linear in the transcript, never worse.
  (let ((state (nle::chat-model-state (transcript-model n))))
    (lambda (i)
      (declare (ignore i))
      (nle::transcript-line-table (nle::project-state state :width 100)))))

;;; --- the store and the wire -------------------------------------------------

(defun seeded-session (turns)
  "A durable session of TURNS completed turns, each with one tool call."
  (let ((session "perf-session"))
    (dotimes (i turns session)
      (seed-completed-turn session (format nil "question ~d" i) (answer-text i)
                           :tools (list (list (format nil "call-~d" i) "eval"
                                              "{\"form\":\"(+ 1 2)\"}" "3"))))))

(define-scaling a-turn-appended-to-a-long-session
    (n :kind :constant :bound 0.75 :sizes '(200 800 3200)
       :known "main 25bc955: instructions n^0.54, bytes n^0.56; COMPLETE-TURN's TURN-CHAIN-DEPTH reads every TURN.STARTED, ADMIT-TURN scans")
  ;; Admitting, journaling and completing one more turn: the store's cost per
  ;; write must not see how long the session already is.
  (let ((session (seeded-session n)))
    (lambda (i)
      (seed-completed-turn session (format nil "more ~d" i) "ok"
                           :tools (list (list (format nil "more-~d" i) "eval" "{}" "1"))))))

(define-scaling folding-a-session-into-the-wire (n :sizes '(200 800 3200))
  ;; The session's visible log read and folded into provider messages: what
  ;; every turn's context starts from. Linear in the session.
  (let ((session (seeded-session n)))
    (lambda (i)
      (declare (ignore i))
      (wire-fold session))))

(define-scaling replaying-a-session
    (n :bound 1.30 :sizes '(200 800 3200)
       :known "main 25bc955: instructions n^1.12; STAMP-UNPOSITIONED-ROWS walks every row after every folded event")
  ;; A session's frames folded as a shell's replay folds them when it opens
  ;; the session (SESSION-TRANSCRIPT): linear in the session.
  (let ((session (seeded-session n)))
    (lambda (i)
      (declare (ignore i))
      (nle::session-transcript session))))

(define-scaling listing-sessions (n :sizes '(100 400 1600))
  ;; The sessions picker's listing: the directory read and laid out as the
  ;; fork tree, every fourth session a fork of the one before it.
  (let ((parent nil))
    (dotimes (i n)
      (let ((id (format nil "perf-~5,'0d" i)))
        (nlk:create-session :id id :cwd (nodecode.test::fixture-workspace)
                            :parent (and parent (zerop (mod i 4)) parent))
        (seed-completed-turn id (format nil "question ~d" i) "ok")
        (setf parent id)))
    (lambda (i)
      (declare (ignore i))
      (nlk:session-tree-order (nlk:list-sessions)))))

;;; --- the wire -----------------------------------------------------------------

(define-scaling a-json-round-trip (n :sizes '(500 2000 8000))
  ;; A payload of N rows encoded and decoded, as every frame and fact is.
  (let ((object (nlk:json-object
                 "rows" (coerce (loop for i below n
                                      collect (nlk:json-object "id" (format nil "row-~d" i)
                                                               "text" (answer-text i)
                                                               "n" i))
                                'vector))))
    (lambda (i)
      (declare (ignore i))
      (nlk:decode-json (nle::encode-json-object object)))))

(define-scaling laying-out-one-long-answer (n :sizes '(200 800 3200))
  ;; One assistant answer of N markdown lines (prose and a list) folded in
  ;; and laid out, its rows new each call so no row's layout is a cache hit:
  ;; linear in its lines.
  (let ((state (nle::chat-model-state (transcript-model 0)))
        (text (with-output-to-string (out)
                (dotimes (i n)
                  (case (mod i 3)
                    (0 (format out "Line ~d says what part ~d does, in a few words.~%" i i))
                    (1 (format out "- item ~d of the list~%" i))
                    (2 (format out "~%")))))))
    (lambda (i)
      (nle::transcript-line-table
       (nle::project-state (exchange state (format nil "write part ~d out" i) text) :width 100)))))

(define-scaling keystroke-in-a-long-draft (n :sizes '(50 200 800))
  ;; A character typed and taken back at the end of a pasted draft of N
  ;; lines: the editor and the composer it paints may walk the draft, never
  ;; more than once per key.
  (let ((model (transcript-model 20))
        (type (key #\x))
        (erase (key :backspace)))
    (type-string model (format nil "~{~a~^~%~}" (loop for i below n collect (format nil "draft line ~d of the paste" i))))
    (lambda (i)
      (nle:update model (if (evenp i) type erase))
      (nle::compose-frame model))))

;;; --- the terminal -----------------------------------------------------------

(define-scaling laying-out-tool-output (n :sizes '(500 2000 8000))
  ;; N rows of output laid out on a screen of N rows: linear.
  (let ((rows (loop for i below n
                    collect (format nil "~5d  (defun part-~d (x) (list x ~d \"value\"))" i i i))))
    (lambda (i)
      (declare (ignore i))
      (nlt:parse-styled-string rows 100 n))))

;;; --- running the gate -------------------------------------------------------

(defun measure-case (name contract sizes setup &key (stream *standard-output*))
  "Run one case at each of SIZES; answers its points and the verdict."
  (let ((points '()))
    (dolist (size sizes)
      (with-temp-store ()
        (let ((fn (funcall setup size)))
          (multiple-value-bind (work bytes calls) (run-scaling-fn fn)
            (push (make-point :size size :work work :bytes bytes :calls calls) points)
            (format stream "  ~a n=~d: ~:[~,0f instructions~;~,1f us~]/call, ~,0f bytes/call (~d calls)~%"
                    name size (eq *work-measure* :cpu)
                    (if (eq *work-measure* :cpu) (* work 1d6) work) bytes calls)))))
    (setf points (nreverse points))
    (values points (verdict contract points))))

(defun run-scaling (&key (only nil) (stream *standard-output*))
  "Run every case (or the names in ONLY); print each slope and verdict, one
line a case; answers the number of cases that broke their contract."
  (format stream "scaling: work is ~a~:[ (this kernel counts no instructions)~;~]~%"
          (work-label) (eq *work-measure* :instructions))
  (let ((broken 0))
    (dolist (entry *cases* broken)
      (destructuring-bind (name contract sizes setup) entry
        (when (or (null only) (member name only))
          (multiple-value-bind (points reasons)
              (flet ((measure ()
                       (nodecode.test::call-with-hermetic-machine-state
                        (lambda () (measure-case name contract sizes setup :stream stream)))))
                ;; A breach must happen twice: a runner busy with another
                ;; job for the length of one size's batch bends one fit, and
                ;; a regression bends both.
                (multiple-value-bind (points reasons) (measure)
                  (if reasons
                      (progn (format stream "  ~(~a~) breached (~{~a~^; ~}); measuring it again~%" name reasons)
                             (measure))
                      (values points reasons))))
            (format stream "~:[ok  ~;FAIL~] ~(~a~) (~(~a~), slope <= ~,2f): ~a n^~,2f, bytes n^~,2f~@[~%       ~a~]~{~%       ~a~}~%"
                    reasons name (scaling-contract-kind contract) (scaling-contract-bound contract)
                    (work-label)
                    (or (exponent points :slot 'point-work) 0)
                    (or (exponent points :slot 'point-bytes) 0)
                    (let ((known (scaling-contract-known contract)))
                      (and known (format nil "known breach of ~(~a~): ~a" (scaling-contract-kind contract) known)))
                    reasons)
            (finish-output stream)
            (when reasons (incf broken))))))))
