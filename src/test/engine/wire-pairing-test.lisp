(in-package #:nodecode.test)

;;; --- the wire is well-formed for every log (proofs/WirePairing.lean) ---------
;;; WIRE-MESSAGES-FROM-EVENTS is modelled in proofs/WirePairing.lean, whose
;;; theorem WIRE_WELLFORMED says the wire it folds passes WF for every event
;;; log. These gates tie the model to the function: the proof's five decided
;;; traces (TRACE_*) run through the real fold, and random logs through the
;;; fold pass a port of the proof's checker WFFROM. A gate here fails when
;;; the function moves away from the model; the model moving away from the
;;; function fails the proof's own trace theorems.

(defun pairing-event (position kind &rest args)
  "One durable fact at log POSITION, as the proof's Ev names it: :USER, :ASST
with the stored message's call ids (none for a plain message), :CALL and
:RESULT with a journaled id, :REMINDER."
  (let ((empty (nlk:json-object)))
    (ecase kind
      (:user (make-instance 'nlk::user-message :log-position position :content "u" :payload empty))
      (:asst (make-instance
              'nlk::assistant-message :log-position position :content ""
              :payload (if (eq (first args) :plain)
                           empty
                           (nlk:json-object
                            "message"
                            (nlk:json-object
                             "role" "assistant" "content" ""
                             "tool_calls" (map 'vector
                                               (lambda (id)
                                                 (nlk:json-object "id" id "type" "function"
                                                                  "function" (nlk:json-object "name" "eval" "arguments" "{}")))
                                               args))))))
      (:call (make-instance 'nlk:tool-call-event :log-position position :call-id (first args)
                            :tool-name "eval" :arguments "{}"))
      (:result (make-instance 'nlk:tool-result-event :log-position position :call-id (first args)
                              :tool-name "eval" :result "ok" :metadata empty))
      (:reminder (make-instance 'nlk:turn-context-prompt-event :log-position position
                                :payload (nlk:json-object "text" "remember"))))))

(defun pairing-log (&rest specs)
  "The events of SPECS, each (KIND . ARGS), at log positions 1, 2, 3 ..."
  (loop for spec in specs for position from 1
        collect (apply #'pairing-event position spec)))

(defun wire-shape (messages)
  "MESSAGES as the proof's Msg: :USER, :SYS, (:ASST id...), (:TOOL id) -- a
tool message answering that it has no result as (:TOOL id :stub)."
  (loop for message in messages
        collect (let ((role (gethash "role" message)))
                  (cond ((equal role "tool")
                         (if (equal (gethash "content" message) nle::+orphan-tool-result+)
                             (list :tool (gethash "tool_call_id" message) :stub)
                             (list :tool (gethash "tool_call_id" message))))
                        ((equal role "assistant")
                         (cons :asst (map 'list (lambda (call) (gethash "id" call))
                                          (or (gethash "tool_calls" message) #()))))
                        ((equal role "user") :user)
                        (t :sys)))))

(defun wire-well-formed-p (shape &optional opened seen)
  "The proof's WFFROM on a WIRE-SHAPE: every tool message answers a call of
the assistant message right before it, every call is answered exactly once
before anything else follows, and no call id is issued twice."
  (if (null shape)
      (null opened)
      (let ((message (first shape)))
        (cond ((and (consp message) (eq (first message) :tool))
               (and (member (second message) opened :test #'equal)
                    (wire-well-formed-p (rest shape)
                                        (remove (second message) opened :test #'equal) seen)))
              ((and (consp message) (eq (first message) :asst))
               (let ((calls (rest message)))
                 (and (null opened)
                      (= (length calls) (length (remove-duplicates calls :test #'equal)))
                      (notany (lambda (call) (member call seen :test #'equal)) calls)
                      (wire-well-formed-p (rest shape) calls (append seen calls)))))
              (t (and (null opened) (wire-well-formed-p (rest shape) opened seen)))))))

(deftest the-fold-follows-the-proofs-decided-traces ()
  ;; proofs/WirePairing.lean TRACE_DUPRESULT ... TRACE_FLUSHWHILEOPEN, the
  ;; five shapes the fold on main refused a provider with. A minted id is
  ;; call-POS-K: the message's log position and the call's index in it.
  (flet ((fold (&rest specs) (wire-shape (nle::wire-messages-from-events (apply #'pairing-log specs)))))
    (is (equal '(:user (:asst "a") (:tool "a"))
               (fold '(:user) '(:asst "a") '(:call "a") '(:result "a") '(:result "a"))) "a result recorded twice answers once")
    (is (equal '(:user (:asst "a") (:tool "a" :stub) :user)
               (fold '(:user) '(:asst "a") '(:call "a") '(:user) '(:result "a"))) "a result landing after a later message answers nothing: the call closed as unanswered")
    (is (equal '(:user (:asst "a") (:tool "a") (:asst "call-5-0") (:tool "call-5-0" :stub) :user)
               (fold '(:user) '(:asst "a") '(:call "a") '(:result "a") '(:asst "a") '(:call "a") '(:user))) "an id a later message reuses is minted anew, and that call is answered")
    (is (equal '(:user (:asst "a" "call-2-1") (:tool "a") (:tool "call-2-1" :stub) :user)
               (fold '(:user) '(:asst "a" "a") '(:call "a") '(:call "a") '(:result "a") '(:user))) "an id a message carries twice is minted anew for the second call")
    (is (equal '(:user (:asst "a" "b") (:tool "a") (:tool "b"))
               (fold '(:user) '(:call "a") '(:call "b") '(:result "a") '(:call "c") '(:result "c") '(:result "b"))) "a call fact journaled while a call is open folds as nothing, and the open call keeps its result")))

(deftest the-fold-is-well-formed-on-generated-logs ()
  ;; WIRE_WELLFORMED over a sample of the logs it quantifies: 400 random logs
  ;; of up to 12 facts over three ids and the blank one, plain and stored
  ;; assistant messages, reminders. Seeded, so a failure replays.
  (let ((state (sb-ext:seed-random-state 20261101))
        (ids '("a" "b" "c" ""))
        (bad '()))
    (dotimes (i 400)
      (let* ((specs (loop repeat (1+ (random 12 state))
                          collect (let ((id (nth (random 4 state) ids)))
                                    (case (random 6 state)
                                      (0 '(:user))
                                      (1 (if (zerop (random 4 state))
                                             '(:asst :plain)
                                             (cons :asst (loop repeat (random 3 state)
                                                               collect (nth (random 4 state) ids)))))
                                      ((2 3) (list :call id))
                                      (4 (list :result id))
                                      (t '(:reminder))))))
             (shape (wire-shape (nle::wire-messages-from-events (apply #'pairing-log specs)))))
        (unless (wire-well-formed-p shape)
          (push (list specs shape) bad))))
    (is (equal nil (first bad)) "every generated log folds to a well-formed wire (the first one that does not is shown)")))
