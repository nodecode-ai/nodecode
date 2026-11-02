;;;; scroll-hold-test.lisp --- a window read off the tail keeps its place.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

;;; When the operator scrolls up while a turn runs, the viewport would come back
;;; down. A running Standard turn shows only its newest words under its
;;; run, so when the next message takes their place the transcript shrinks by
;;; the words that left; the window counts from the top, and a window scrolled
;;; up by fewer rows than that would be clamped onto the new bottom.

(defvar *hold-position* 1000 "The log position the next fact of turn t1 stands at.")

(defun hold-fact (model kind &rest payload)
  "Fold one durable fact of turn t1 into MODEL's state and render it."
  (setf (nle::chat-model-state model)
        (nle::apply-gateway-event
         (nle::chat-model-state model)
         (nlk::encode-inner-event kind (apply #'nlk:make-json-object payload)
                                  :metadata (nlk:make-json-object
                                             "turn_id" "t1"
                                             "log_position" (incf *hold-position*)
                                             "occurred_at" "2026-10-16T00:00:00.000Z"))))
  (nle::render-state model))

(defun hold-words (model text)
  "Turn t1's next message, TEXT."
  (hold-fact model "turn.assistant_message_completed"
             "role" "assistant" "message" (nlk:make-json-object "role" "assistant" "content" text)))

(defun hold-call (model id)
  "Turn t1 runs eval call ID to its result."
  (hold-fact model "turn.tool_call_started" "call-id" id "tool-name" "eval"
             "arguments" "{\"form\":\"(+ 1 2)\"}")
  (hold-fact model "turn.tool_result" "call-id" id "tool-name" "eval" "result" "3"))

(defun hold-verses (count tag)
  "COUNT lines of words, each naming TAG."
  (format nil "~{~a~^~%~}" (loop for i from 1 to count collect (format nil "~a verse ~d" tag i))))

(defun hold-turn (width height verses)
  "A WIDTH x HEIGHT Standard shell over twelve exchanges, its turn t1 running
with one call done and VERSES lines of words under the run."
  (let ((model (make-frame-model width height :session-id "hold")))
    (setf (nle::chat-model-tool-detail model) :standard
          (nle::chat-model-state model) (exchanges (nle::chat-model-state model) 12))
    (mark-turn-in-flight model)
    (hold-fact model "turn.input_committed" "role" "user" "message" "write verses"
               "prompt-id" "p1" "sequence" 1)
    (hold-fact model "turn.started" "input" "write verses")
    (hold-call model "c1")
    (hold-words model (hold-verses verses "first"))
    model))

(defun hold-rows (model)
  "The text of every row of MODEL's freshly composed frame, its scroll rail
left off: the thumb moves with the content's length, which is not what a row says."
  (nle::compose-frame model)
  (map 'list (lambda (row) (string-right-trim " ▕▐" row)) (nle::frame-plain-rows model)))

(defun hold-wheel (model direction &optional (notches 1))
  (dotimes (notch notches)
    (nle:update model (nlt:make-mouse-wheel-msg :x 5 :y 5 :direction direction))))

(deftest a-window-read-off-the-tail-keeps-its-place-when-the-turn-under-it-shrinks ()
  (with-temp-store ()
    (let* ((model (hold-turn 80 40 24))
           (window (nle::chat-model-band model)))
      (is (nle::chat-model-follow-bottom model) "the tail is followed before the wheel")
      (hold-wheel model :up)
      (is (not (nle::chat-model-follow-bottom model)) "one notch up lets the tail go")
      (let* ((offset (nle::band-window-offset window))
             (shown (hold-rows model))
             (words (position "first verse 1" shown :test #'search))
             ;; The run's title row, the last with text over the words: its
             ;; count moves with the next call, and every row above it stands.
             (run (and words (position-if (lambda (row) (plusp (length (string-trim " " row))))
                                          shown :end words :from-end t))))
        (is (and words run) "the run and its words' first line are on screen")
        ;; The next call and its first short words take the verses' place: the
        ;; transcript is 23 rows shorter, all of them under the window's top.
        (hold-call model "c2")
        (hold-words model "second")
        (let ((now (hold-rows model)))
          (is (not (nle::chat-model-follow-bottom model)) "the shrink locks nothing")
          (is (= offset (nle::band-window-offset window)) "the window keeps its offset")
          (is (equal (subseq shown 0 run) (subseq now 0 run)) "every row above the run stands where it stood")
          (is (search "second" (nth words now)) "the new words come up where the last ones began"))
        ;; Back down to the tail: the follow locks where the window is, with no
        ;; jump, and tracks what grows.
        (hold-wheel model :down)
        (is (nle::chat-model-follow-bottom model) "one notch down at the held place locks the follow")
        (is (= offset (nle::band-window-offset window)) "locking moves nothing")
        (hold-words model (hold-verses 40 "third"))
        (is (nle::band-window-at-bottom-p window) "the locked window follows the growing words")
        (is (find "third verse 40" (hold-rows model) :test #'search) "the newest line is on screen")
        (is (null (nle::band-window-hold window)) "words grown past the held place release it")
        ;; And up again lets it go again.
        (hold-wheel model :up)
        (is (not (nle::chat-model-follow-bottom model)) "the next notch up lets it go again")))))

(deftest a-window-whose-rows-left-lands-on-the-new-bottom-never-a-blank-band ()
  (with-temp-store ()
    (let* ((model (hold-turn 80 24 16))
           (window (nle::chat-model-band model)))
      ;; One notch up from the tail puts the window's top inside the verses,
      ;; and the next words take every one of them away.
      (hold-wheel model :up)
      (is (find "first verse 8" (hold-rows model) :test #'search) "the window reads the verses")
      (hold-call model "c2")
      (hold-words model "second")
      (let ((now (hold-rows model)))
        (is (not (nle::chat-model-follow-bottom model)) "the follow stays where the operator left it")
        (is (null (nle::band-window-hold window)) "nothing the window showed is left to hold")
        (is (nle::band-window-at-bottom-p window) "the window lands on the new bottom")
        (is (find "second" now :test #'search) "the new words are on screen")
        (is (notany (lambda (row) (search "first verse" row)) now) "and the words that left are gone")))))

(deftest pinning-the-tail-releases-a-held-place ()
  (with-temp-store ()
    (let* ((model (hold-turn 80 40 24))
           (window (nle::chat-model-band model)))
      (hold-wheel model :up)
      (hold-call model "c2")
      (hold-words model "second")
      (is (nle::band-window-hold window) "the shrink left a held place")
      (nle::pin-transcript-bottom model)
      (nle::render-state model)
      (is (null (nle::band-window-hold window)) "a pin drops it")
      (is (= (nle::band-window-max-offset window) (nle::band-window-offset window)) "and the window rests on the content's own bottom")
      (is (= (nle::band-window-max-offset window nil) (nle::band-window-offset window)) "which no hold widens"))))
