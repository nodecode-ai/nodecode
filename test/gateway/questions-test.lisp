;;;; questions-test.lisp --- the organism asks a person at an answering surface, and waits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ASK-OPERATOR (gateway/questions.lisp) against silent peers whose queues
;;;; stay inspectable: nobody is asked with no answering surface attached; an
;;;; answering one is sent the board, its answer_question settles the wait,
;;;; and the board empties; a bad answer is refused; time runs out; and a
;;;; subscribe that says it answers marks the peer and is sent the board.

(in-package #:nodecode.test)

(defun answering-silent-peer (state &key (answers t))
  "STATE attached, answering questions when ANSWERS."
  (setf (nle::gw-peer-state-attached-p state) t
        (nle::gw-peer-state-answers-p state) answers)
  state)

(defun question-boards (state)
  "The questions arrays of the organism.questions frames queued for STATE, oldest first."
  (loop for frame in (%peer-frames state)
        for parsed = (shasht:read-json frame)
        when (equal "organism.questions" (frame-type parsed))
          collect (coerce (nlk:json-value parsed :array "payload" "questions") 'list)))

(defun command-results (state)
  "The command results queued for STATE, as (STATUS CODE) lists."
  (loop for frame in (%peer-frames state)
        for parsed = (shasht:read-json frame)
        when (equal "gateway_turn_command_result" (frame-type parsed))
          collect (list (nlk:json-value parsed :string "payload" "result" "status")
                        (nlk:json-value parsed :string "payload" "result" "code"))))

(defun drive-answer (peer session-id question-id answer)
  "The answer_question command, the way the WS dispatcher drives it."
  (funcall (gethash :answer-question nle::*sync-command-handlers*)
           peer
           (nlk::make-json-object "session_id" session-id "question_id" question-id
                                  "answer" answer)
           "answer-cmd" "answer-cmd" (list '())))

(defun asking-thread (&rest arguments)
  "ASK-OPERATOR on its own thread; join it for (ANSWER WHY)."
  (nlk:spawn "ask-operator-test"
    (multiple-value-list (apply #'nle:ask-operator arguments))))

(deftest ask-operator-asks-nobody-without-an-answering-surface ()
  (with-session-store ("ask-nobody")
    (let ((started (get-internal-real-time)))
      (is (equal '(nil :nobody) (multiple-value-list
                                 (nle:ask-operator "Run it?" :session-id "ask-nobody" :seconds 30))) "no peer: no question, at once")
      (with-silent-peer (peer state :ask-shell)
        (answering-silent-peer state :answers nil)
        (is (equal '(nil :nobody) (multiple-value-list
                                   (nle:ask-operator "Run it?" :session-id "ask-nobody" :seconds 30))) "a shell that never said it answers is not asked")
        (is (null (question-boards state)) "and is never sent the board"))
      (is (< (- (get-internal-real-time) started) internal-time-units-per-second) "neither waited"))))

(deftest ask-operator-waits-for-the-answer-a-surface-sends ()
  (with-session-store ("ask-answer")
    (with-silent-peer (peer state :ask-page)
      (answering-silent-peer state)
      ;; A second session, whose id an answer may name wrongly.
      (nlk:create-session :id "ask-other")
      (let ((asking (asking-thread "The guard stopped a call: privilege"
                                   :session-id "ask-answer" :seconds 10
                                   :detail "sudo ls" :choices '("Allow once" "Deny"))))
        (is-eventually (question-boards state) "the page is sent the board")
        (let ((question (first (first (question-boards state)))))
          (is (equal "ask-answer" (gethash "session_id" question)) "naming the session")
          (is (equal "sudo ls" (gethash "detail" question)) "with the detail")
          (is (equalp #("Allow once" "Deny") (gethash "choices" question)) "and the choices")
          (is (> (gethash "expires_at" question) 0) "and when it ends")
          (drive-answer peer "ask-answer" (gethash "question_id" question) "Maybe")
          (drive-answer peer "ask-answer" "q-none" "Deny")
          (drive-answer peer "ask-other" (gethash "question_id" question) "Deny")
          (drive-answer peer "ask-answer" (gethash "question_id" question) "Allow once")
          (drive-answer peer "ask-answer" (gethash "question_id" question) "Deny")
          (is (equal '(("Allow once" :answered)) (list (bt2:join-thread asking))) "the first good answer ends the wait with it")
          (let ((results (command-results state)))
            (is (equal '(("rejected" "unknown_choice") ("rejected" "unknown_question")
                         ("rejected" "question_session_mismatch") ("accepted" nil))
                       (subseq results 0 4)) "a choice not offered, an unknown id and another session's id are refused, the good one taken")
            ;; Settled or already gone, by when the waiting thread cleared it.
            (is (member (fifth results) '(("rejected" "already_answered") ("rejected" "unknown_question"))
                        :test #'equal) "and a late answer is refused"))
          (is (null (car (last (question-boards state)))) "and the board empties"))))))

(deftest ask-operator-gives-up-when-time-runs-out ()
  (with-session-store ("ask-timeout")
    (with-silent-peer (peer state :ask-page)
      (answering-silent-peer state)
      (is (equal '(nil :timeout) (multiple-value-list
                                  (nle:ask-operator "Run it?" :session-id "ask-timeout" :seconds 1))) "no answer within the seconds is NIL and :TIMEOUT")
      (is (null nle::*open-questions*) "the question goes")
      (is (null (car (last (question-boards state)))) "and the board says so"))))

(deftest subscribe-that-answers-marks-the-peer-and-gets-the-board ()
  (with-session-store ("ask-subscribe")
    (with-silent-peer (peer state :ask-page)
      (setf (nle::gw-peer-state-attached-p state) t)
      (flet ((subscribe (answers)
               (funcall (gethash :set-session-subscriptions nle::*sync-command-handlers*)
                        peer
                        (apply #'nlk::make-json-object
                               "subscriptions" (vector (nlk::make-json-object "session_id" "ask-subscribe"
                                                                              "rows" t))
                               (when answers (list "answers" t)))
                        nil nil (list '()))))
        (subscribe t)
        (is (nle::gw-peer-state-answers-p state) "answers: true marks the peer")
        (is (question-boards state) "which is sent the open questions at once")
        (subscribe nil)
        (is (not (nle::gw-peer-state-answers-p state)) "a subscribe without it unmarks it")))))

(deftest a-shell-answers-only-its-own-sessions-questions ()
  ;; A terminal shell shows one session: its subscribe says "answers_own",
  ;; it is sent the board, and it is asked only for the session it shows --
  ;; a turn of another session, a channel lane's, is not made to wait on it.
  (with-session-store ("ask-own")
    (nlk:create-session :id "ask-elsewhere")
    (with-silent-peer (peer state :ask-shell)
      (setf (nle::gw-peer-state-attached-p state) t)
      (funcall (gethash :set-session-subscriptions nle::*sync-command-handlers*)
               peer
               (nlk::make-json-object "subscriptions" (vector (nlk::make-json-object "session_id" "ask-own"))
                                      "answers_own" t)
               nil nil (list '()))
      (is (not (nle::gw-peer-state-answers-p state)) "it is not a page")
      (is (question-boards state) "yet it is sent the open questions at once")
      (is (member peer (nle::answering-peers-for "ask-own")) "it answers the session it shows")
      (is (equal '(nil :nobody) (multiple-value-list
                                 (nle:ask-operator "Run it?" :session-id "ask-elsewhere" :seconds 30))) "and nobody is asked for another session")
      (let ((asking (asking-thread "Trust it?" :session-id "ask-own" :seconds 10 :choices '("Yes" "No"))))
        (is-eventually (some #'identity (question-boards state)) "its own session's question reaches it")
        (let ((question (first (car (last (question-boards state))))))
          (drive-answer peer "ask-own" (gethash "question_id" question) "Yes")
          (is (equal '(("Yes" :answered)) (list (bt2:join-thread asking))) "and its answer ends the wait"))))))

(deftest answer-question-and-the-answers-flag-ride-the-wire ()
  (let ((command (nlk::decode-command
                  (nlk::make-json-object "type" "answer_question" "command_id" "c" "correlation_id" "c"
                                         "session_id" "s" "question_id" "q-1" "answer" "Deny"))))
    (is (eq :answer-question (nlk::gateway-command-kind command)) "answer_question decodes")
    (is (equal "q-1" (gethash "question_id" (nlk::gateway-command-request command))) "with its fields"))
  (let* ((wire (nlk:encode-client-command
                (nlk:make-gateway-command
                 :kind :set-session-subscriptions
                 :request (nlk:json-object "subscriptions" (vector "s") "answers" t))))
         (decoded (nlk::decode-command (gethash "command" (shasht:read-json wire)))))
    (is (gethash "answers" (nlk::gateway-command-request decoded)) "a subscribe's answers flag survives encode and decode"))
  (is (member "organism.questions" nlk:+unsequenced-frame-kinds+ :test #'string=) "the board is an unsequenced kind"))
