(in-package #:nodecode.test)

;;; --- the wire is well-formed for every log (proofs/WirePairing.lean) ---------
;;; WIRE-MESSAGES-FROM-EVENTS is modelled in proofs/WirePairing.lean, whose
;;; theorem WIRE_WELLFORMED says the wire it folds passes WF for every event
;;; log. These gates tie the model to the function: the proof's five decided
;;; traces (TRACE_*) run through the real fold, and random logs through the
;;; fold pass a port of the proof's checker WFFROM. A gate here fails when
;;; the function moves away from the model; the model moving away from the
;;; function fails the proof's own trace theorems.

(defvar *pairing-turn* "t1" "The turn PAIRING-EVENT's facts belong to.")

(defun pairing-event (position kind &rest args)
  "One durable fact at log POSITION of turn *PAIRING-TURN*, as the proof's Ev
names it: :USER, :ASST with the stored message's call ids (none for a plain
message, :TEXT for one that answers in words), :CALL and :RESULT with a
journaled id, :REMINDER."
  (let ((empty (nlk:json-object))
        (turn *pairing-turn*))
    (ecase kind
      (:user (make-instance 'nlk::user-message :log-position position :turn-id turn
                            :kind nlk::+kind-input-committed+ :content "u" :payload empty))
      (:asst (make-instance
              'nlk::assistant-message :log-position position :turn-id turn
              :kind nlk::+kind-assistant-completed+ :content ""
              :payload (if (eq (first args) :plain)
                           empty
                           (nlk:json-object
                            "message"
                            (nlk:json-object
                             "role" "assistant" "content" (if (eq (first args) :text) "done" "")
                             "tool_calls" (map 'vector
                                               (lambda (id)
                                                 (nlk:json-object "id" id "type" "function"
                                                                  "function" (nlk:json-object "name" "eval" "arguments" "{}")))
                                               (remove :text args)))))))
      (:call (make-instance 'nlk:tool-call-event :log-position position :turn-id turn
                            :kind nlk::+kind-tool-call-started+ :call-id (first args)
                            :tool-name "eval" :arguments "{}"))
      (:result (make-instance 'nlk:tool-result-event :log-position position :turn-id turn
                              :kind nlk::+kind-tool-result+ :call-id (first args)
                              :tool-name "eval" :result "ok" :metadata empty))
      (:reminder (make-instance 'nlk:turn-context-prompt-event :log-position position :turn-id turn
                                :kind nlk::+kind-turn-context-prompt+
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

(deftest repaired-call-ids-do-not-collide-with-journaled-ids ()
  (dolist (specs '(((:user) (:asst "call-5-0") (:result "call-5-0") (:user) (:asst "") (:result ""))
                   ((:user) (:asst "call-5-0" "call-5-0-1") (:result "call-5-0") (:result "call-5-0-1") (:asst "") (:result ""))
                   ((:user) (:asst "call-2-1" "") (:result "call-2-1") (:result ""))
                   ((:user) (:asst "" "call-2-0") (:result "") (:result "call-2-0"))))
    (let* ((events (apply #'pairing-log specs))
           (shape (wire-shape (nle::wire-messages-from-events events))))
      (is (wire-well-formed-p shape) "repair IDs cannot reuse an issued ID")
      (is (equal shape (wire-shape (nle::wire-messages-from-events events))) "collision repair is deterministic"))))

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

;;; --- the request the provider checks ---------------------------------------
;;; The proof covers the fold of one unit. A request takes more steps: the log
;;; splits into turn units, each folded apart, the tail at a boundary rides
;;; behind its eviction stub, and the Anthropic lane merges the wire into
;;; alternating roles. The gate below holds that whole path to the rule the
;;; Messages API enforces with a 400.

(defun turn-log (&rest turns)
  "The events of TURNS, each a list of (KIND . ARGS) opened by its user
message, as turns t1, t2 ... at log positions 1, 2, 3 ..."
  (let ((position 0))
    (loop for specs in turns
          for n from 1
          append (let ((*pairing-turn* (format nil "t~d" n)))
                   (loop for spec in (cons '(:user) specs)
                         collect (apply #'pairing-event (incf position) spec))))))

(defun anthropic-request (events boundary)
  "The Anthropic messages a request over EVENTS sends with BOUNDARY turns evicted."
  (nle::anthropic-messages
   (coerce (nle::messages-at-boundary (nle::history-turns-from-events events) boundary) 'vector)))

(defun anthropic-pairing-fault (messages)
  "The first way MESSAGES break the Messages API's tool pairing, or NIL: every
tool_result answers a tool_use of the message right before it and stands
ahead of any other block, and every tool_use is answered in the message right
after."
  (flet ((blocks (i type)
           (and (< -1 i (length messages))
                (remove type (coerce (gethash "content" (aref messages i)) 'list)
                        :key (lambda (block) (gethash "type" block)) :test-not #'equal))))
    (dotimes (i (length messages))
      (let ((results (blocks i "tool_result"))
            (asked (mapcar (lambda (block) (gethash "id" block)) (blocks (1- i) "tool_use"))))
        (dolist (result results)
          (unless (member (gethash "tool_use_id" result) asked :test #'equal)
            (return-from anthropic-pairing-fault
              (list :unexpected-tool-result i (gethash "tool_use_id" result)))))
        (unless (every (lambda (block) (equal "tool_result" (gethash "type" block)))
                       (subseq (coerce (gethash "content" (aref messages i)) 'list) 0 (length results)))
          (return-from anthropic-pairing-fault (list :text-before-tool-result i)))
        (dolist (use (blocks i "tool_use"))
          (unless (find (gethash "id" use) (blocks (1+ i) "tool_result")
                        :key (lambda (block) (gethash "tool_use_id" block)) :test #'equal)
            (return-from anthropic-pairing-fault
              (list :unanswered-tool-use i (gethash "id" use)))))))))

(deftest the-anthropic-request-pairs-every-tool-result ()
  ;; A result recorded again once its call was answered -- a restart's
  ;; recovery closing a turn that had already finished -- went out on
  ;; v0.1.7 as a tool_result with no tool_use before it: 400 `unexpected
  ;; tool_use_id found in tool_result blocks', on every later round. The same
  ;; generator found it in 410 of these 3,999 requests on v0.1.7.
  (is (null (anthropic-pairing-fault
             (anthropic-request (turn-log '((:asst "a") (:call "a") (:result "a") (:asst :text) (:result "a"))
                                          '())
                                0))) "a result recorded after its call was answered sends no tool_result")
  ;; 2,000 seeded logs of one to three turns over three ids and the blank one,
  ;; each request at every boundary.
  (let ((state (sb-ext:seed-random-state 20261103))
        (ids '("a" "b" "c" ""))
        (bad '()))
    (dotimes (i 2000)
      (let* ((turns (loop repeat (1+ (random 3 state))
                          collect (loop repeat (random 8 state)
                                        collect (let ((id (nth (random 4 state) ids)))
                                                  (case (random 6 state)
                                                    (0 (case (random 3 state)
                                                         (0 '(:asst :plain))
                                                         (1 '(:asst :text))
                                                         (t (cons :asst (loop repeat (1+ (random 2 state))
                                                                              collect (nth (random 4 state) ids))))))
                                                    ((1 2) (list :call id))
                                                    ((3 4) (list :result id))
                                                    (t '(:reminder)))))))
             (events (apply #'turn-log turns)))
        (dotimes (boundary (length turns))
          (let ((fault (anthropic-pairing-fault (anthropic-request events boundary))))
            (when fault (push (list fault :boundary boundary :turns turns) bad))))))
    (is (equal nil (car (last bad))) "every request pairs its tool results (the first that does not is shown)")))
