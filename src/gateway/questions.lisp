;;;; questions.lisp --- the organism asks the person watching a session, and waits. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ASK-OPERATOR is one question put to the person at a surface that said it
;;;; answers them (set_session_subscriptions with "answers": true -- the web
;;;; page and the desktop app that shows it), on the calling thread, which
;;;; waits for a choice. It answers the choice, or NIL and why: :NOBODY when
;;;; no such surface is attached (a channel lane's turn with no page open, a
;;;; TUI alone), so a caller never waits on an empty room; :TIMEOUT when the
;;;; seconds ran out first. A turn's Esc interrupts the wait like any tool
;;;; call's (WITH-CANCELLABLE-WAIT), and the question goes with it.
;;;;
;;;; The open questions are one standing board, "organism.questions", sent
;;;; whole each time it changes and once when an answering surface
;;;; subscribes, so a page that reloads mid-question still shows it. Only
;;;; answering peers are sent it: a shell that never said it answers is never
;;;; handed a kind it does not draw. answer_question settles one; the first
;;;; answer wins and a later one is rejected.
;;;;
;;;; Nothing here grants anything: a cell decides what a question is for (the
;;;; guard asks before a call its rules match runs, when its `ask' is on).

(in-package #:nodecode.evolved)

(defparameter *ask-operator-seconds* 120
  "Seconds ASK-OPERATOR waits for a choice when its caller names none.")

(defstruct (operator-question (:copier nil))
  (id (error "id required") :type string :read-only t)
  (session-id (error "session-id required") :type string :read-only t)
  (title "" :type string :read-only t)
  (detail nil :type (or null string) :read-only t)
  (choices '() :type list :read-only t)
  ;; Unix seconds the wait ends at, for the countdown a surface draws.
  (expires-at 0 :type integer :read-only t)
  (answer nil)
  (answered (bt2:make-semaphore :name "operator-question") :read-only t))

(nlk:access (question operator-question))

(defvar *open-questions* '()
  "The questions waiting for an answer, oldest first.")

(defvar *open-questions-lock* (bt2:make-lock :name "open-questions"))

(defun answering-peer-p (peer)
  (let ((state (find-gw-peer-state peer)))
    (and state (gw-peer-state-attached-p state) (gw-peer-state-answers-p state)
         (not (gw-peer-state-closed-p state)))))

(defun answering-peers ()
  "The attached peers that answer questions."
  ;; Whichever session a page shows: it puts up every session's question,
  ;; named, so the person sees it wherever they are.
  (remove-if-not #'answering-peer-p
                 (sb-ext:with-locked-hash-table (*gateway-peer-states*)
                   (loop for peer being the hash-keys of *gateway-peer-states*
                         collect peer))))

(defun questions-payload ()
  (json-object
   ;; The gateway's clock, so a surface on another machine counts down true.
   "now" (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))
   "questions"
   (coerce (loop for question in (bt2:with-lock-held (*open-questions-lock*)
                                   (copy-list *open-questions*))
                 collect (json-object "question_id" question.id
                                      "session_id" question.session-id
                                      "title" question.title
                                      :opt "detail" question.detail
                                      "choices" (coerce question.choices 'vector)
                                      "expires_at" question.expires-at))
           'vector)))

(defun publish-questions ()
  "The whole board of open questions to every answering peer, through the
:FRAME point like every live frame."
  (call-hooked :frame
               (list :session-id nil :kind "organism.questions"
                     :payload (questions-payload) :cursor nil :durable-p nil)
               (lambda (op)
                 (let ((encoded (nlk:encode-unsequenced-frame "organism.questions"
                                                              (getf op :payload))))
                   (loop for peer in (answering-peers)
                         count (enqueue-peer-frame peer encoded))))
               :isolate t))

(defun ask-operator (title &key (choices '("Allow" "Deny")) detail
                                (seconds *ask-operator-seconds*) session-id)
  "Ask the person watching SESSION-ID (the running turn's when NIL) TITLE,
DETAIL beneath it, offering CHOICES, and wait up to SECONDS for one.
=> the chosen label and :ANSWERED, or NIL and :NOBODY (no surface that
answers is attached) or :TIMEOUT."
  (check-type title string)
  (assert (and choices (every #'stringp choices)) (choices)
          "a question offers at least one choice, each a string")
  (let ((sid (or session-id (getf (turn) :session-id))))
    (unless (and (stringp sid) (answering-peers))
      (return-from ask-operator (values nil :nobody)))
    (let ((question (make-operator-question
                     :id (format nil "q-~(~36r~)" (random (expt 36 10)))
                     :session-id sid :title title :detail detail :choices choices
                     :expires-at (+ (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))
                                    (ceiling seconds)))))
      (bt2:with-lock-held (*open-questions-lock*)
        (setf *open-questions* (append *open-questions* (list question))))
      (unwind-protect
           (progn
             (publish-questions)
             (if (bt2:wait-on-semaphore question.answered :timeout seconds)
                 (values question.answer :answered)
                 (values nil :timeout)))
        (bt2:with-lock-held (*open-questions-lock*)
          (setf *open-questions* (remove question *open-questions*)))
        (ignore-errors (publish-questions))))))

(defun answer-operator-question (session-id question-id answer)
  "Settle QUESTION-ID of SESSION-ID with ANSWER.
=> :OK, or the code the refusal goes out as."
  (bt2:with-lock-held (*open-questions-lock*)
    (let ((question (find question-id *open-questions*
                          :key #'operator-question-id :test #'string=)))
      (cond ((null question) :unknown-question)
            ((not (string= session-id question.session-id)) :question-session-mismatch)
            ((not (member answer question.choices :test #'string=)) :unknown-choice)
            (question.answer :already-answered)
            (t (setf question.answer answer)
               (bt2:signal-semaphore question.answered)
               :ok)))))

(define-sync-command :answer-question ((question-id :text "question_id")
                                       (answer :text "answer"))
  ;; The question card's gesture (web page, desktop app): one of the choices
  ;; ASK-OPERATOR offered, for the question its session's turn waits on.
  ;; No subscription is asked for: a page puts up every session's question
  ;; whichever session it shows, and the id is the question's own.
  (:require (and sid question-id answer) "session_id, question_id and answer are required")
  (let ((outcome (answer-operator-question sid question-id answer)))
    (if (eq outcome :ok)
        (result "accepted" "ok" "answered")
        (result "rejected" (substitute #\_ #\- (string-downcase outcome))
                (ecase outcome
                  (:unknown-question "no open question with that id")
                  (:question-session-mismatch "that question belongs to another session")
                  (:unknown-choice "that answer is not one of the question's choices")
                  (:already-answered "that question is already answered"))))))
