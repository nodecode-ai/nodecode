;;;; support.lisp --- channel test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Channel tests register into the SAME nodecode.test registry (the
;;;; core DEFTEST, with its hermetic machine-state posture) under a CHANNEL-
;;;; name prefix; RUN-CHANNEL-TESTS runs exactly that slice, so kit/adapter
;;;; test-ops never re-run the core suite and `just test` never runs
;;;; channel tests.

(in-package #:nodecode.test)

;; Hermetic: the core posture does not redirect $HOME, so without this a
;; developer's real ~/.nodecode/SOUL.md would ride into every e2e session.
;; The path names nothing and is never created; tests that want a soul
;; pass soul_file explicitly.
(setf nck:*soul-default-path* (temp-path "absent-soul" "md"))

(define-test-slice "channel" "CHANNEL" :recipe "channels")

(defun press-lines (controls)
  "CONTROLS -- or any tree holding presses' data -- with each press read back
as the line it says (NCK:SAID-LINE), every other datum as it is: a card's
shape asserted in the lines its presses say, not their ids."
  (cond ((and (stringp controls) (nck:said-line controls)))
        ((consp controls) (cons (press-lines (car controls)) (press-lines (cdr controls))))
        (t controls)))

(defun is-inbound (policy candidate expected &optional reason note)
  "One row of the admission decision table: DECIDE-INBOUND once, the action
against EXPECTED under NOTE, the wire reason against REASON (NIL for a row
that admits)."
  ;; A plain defun and not a macro because IS pushes onto the
  ;; *FAILURES* special the runner binds around every test, so the assertion
  ;; counts the same from inside a helper.
  (multiple-value-bind (action wire) (nck:decide-inbound policy candidate)
    (is (eq expected action) note)
    (is (equal reason wire))))

;;; A warning the kit raises is how it speaks about a room it cannot answer in:
;;; a test reads what it said.
(defmacro warnings-of (&body body &aux (texts (gensym "TEXTS")))
  "(values TEXTS VALUE): the text of every warning BODY raised, muffled, in the
order raised, and BODY's own value."
  `(let* ((,texts '())
          (value (handler-bind ((warning (lambda (w) (push (princ-to-string w) ,texts)
                                           (muffle-warning w))))
                   ,@body)))
     (values (reverse ,texts) value)))

;;; --- scripted answers ----------------------------------------------------------

(defun reply (status &rest pairs)
  "A scripted response of STATUS whose body is the JSON object PAIRS build."
  (nck:make-scripted-response status (apply #'nlk:make-json-object pairs)))

(defun scripted-executor (&rest responses)
  "A recording executor answering RESPONSES in order."
  (nck:make-recording-executor :responses responses))

(defun first-chunk (text &optional (limit 2000))
  "The first chunk NCK:SPLIT-TEXT-CHUNKS cuts TEXT into at LIMIT."
  (first (nck:split-text-chunks text limit)))

(defun replies (&rest ids)
  "One 200 {\"id\": ID} scripted response per ID, in order — the answers
the platform gives a post, a thread, a room."
  (mapcar (lambda (id) (reply 200 "id" id)) ids))

(defun rooms-reply (&rest specs)
  "The scripted 200 that answers a room listing with the rooms SPECS name."
  (nck:make-scripted-response 200 (coerce (loop for (id name) in specs
                                                collect (nlk:json-object "id" id "name" name))
                                          'vector)))

(defun scripted-execution (responses &key retry retry-after-fn)
  "Run one bare plan through a recording executor scripted with RESPONSES."
  ;; RETRY opts the plan into the 429/5xx schedule; RETRY-AFTER-FN is the
  ;; protocol pacing hook a body-paced platform installs. Answers the
  ;; execution and the executor, so a caller can assert the recorded plans
  ;; too.
  (let ((executor (nck:make-recording-executor
                   :responses responses :retry-after-fn retry-after-fn)))
    (values (nck:execute-plan
             executor
             (nck:make-request-plan :path "/x" :retry-server-errors retry))
            executor)))

;;; --- reading the recorded plans --------------------------------------------------

(defun recorded-plans (executor)
  (nck:recording-executor-plans executor))

(defun recorded-plan (executor label)
  "The first recorded plan with LABEL, or NIL."
  (find label (recorded-plans executor)
        :key #'nck:request-plan-audit-label :test #'equal))

(defun plan-content (plan)
  (gethash "content" (nck:request-plan-body plan)))

(defun plan-matching-p (plan &key label path method content text ping)
  "True when PLAN carries every given mark: the audit LABEL, the PATH and
METHOD exactly, CONTENT — one string or a list — as substrings of its body's
text and TEXT as the whole of it, and PING as a body that pings."
  (let* ((body (nck:request-plan-body plan))
         (said (and (hash-table-p body) (gethash "content" body))))
    (and (or (null label) (equal label (nck:request-plan-audit-label plan)))
         (or (null path) (equal path (nck:request-plan-path plan)))
         (or (null method) (equal method (nck:request-plan-method plan)))
         (or (null content)
             (and (stringp said)
                  (every (lambda (part) (search part said)) (uiop:ensure-list content))))
         (or (null text) (equal text said))
         (or (null ping) (and (hash-table-p body) (eq t (gethash "ping" body)))))))

(defun plan-field (plan key &aux (body (nck:request-plan-body plan)))
  "KEY on PLAN's body: a JSON object's member, or a multipart body's part."
  (if (listp body) (cdr (assoc key body :test #'equal)) (gethash key body)))

(defmacro is-plan (plan &rest marks &aux (var (if (symbolp plan) plan (gensym "PLAN"))))
  "One IS per mark of MARKS, alternating KEY VALUE pairs PLAN — a request plan,
evaluated once — carries: :METHOD, :PATH, :LABEL (the audit label), :TIMEOUT,
:HEADERS and :BODY equal VALUE; :RETRY T or NIL, whether it retries a server
error; a string KEY, that PLAN-FIELD equal VALUE; and a list KEY, (TYPE . PATH),
that NLK:JSON-VALUE read of its body equal VALUE."
  (flet ((check (key value)
           (let ((read (cond ((stringp key) `(plan-field ,var ,key))
                             ((consp key) `(nlk:json-value (nck:request-plan-body ,var) ,@key))
                             (t (list (ecase key
                                        (:method 'nck:request-plan-method)
                                        (:path 'nck:request-plan-path)
                                        (:label 'nck:request-plan-audit-label)
                                        (:timeout 'nck:request-plan-timeout-seconds)
                                        (:headers 'nck:request-plan-headers)
                                        (:body 'nck:request-plan-body)
                                        (:retry 'nck:request-plan-retry-server-errors))
                                      var)))))
             (cond ((not (eq key :retry)) `(is (equal ,value ,read)))
                   (value `(is ,read))
                   (t `(is (not ,read)))))))
    `(let (,@(unless (eq var plan) `((,var ,plan))))
       ,@(loop for (key value) on marks by #'cddr collect (check key value)))))

(defun plan-matching (plans &rest marks)
  "The first of PLANS matching MARKS (PLAN-MATCHING-P's keys), or NIL."
  (find-if (lambda (plan) (apply #'plan-matching-p plan marks)) plans))

(defun plans-matching (plans &rest marks)
  "Every one of PLANS matching MARKS, in order."
  (remove-if-not (lambda (plan) (apply #'plan-matching-p plan marks)) plans))

(defun await-plan (executor &rest marks &key (timeout 10) &allow-other-keys)
  "Poll EXECUTOR's recorded plans for one matching MARKS, up to TIMEOUT
seconds; the plan, or NIL."
  (let ((marks (uiop:remove-plist-key :timeout marks)))
    (await (:timeout timeout) (apply #'plan-matching (recorded-plans executor) marks))))

(defun await-lane (host session-id &key (timeout 10))
  "Poll HOST's lane table for SESSION-ID's lane, up to TIMEOUT seconds; the
lane, or NIL."
  (await (:timeout timeout) (nck:find-lane (nck:host-lanes host) session-id)))

;;; --- folding facts into a host -----------------------------------------------------

(defun fold-fact (host session kind payload &optional (turn-id "t1"))
  "Fold one durable fact of KIND into HOST's lane for SESSION, as the :FRAME
hook would on the publishing thread."
  (nck:on-fact-with-turn host session kind payload (make-hash-table :test #'equal) turn-id))

(defun fold-delta (host session text &key (turn-id "t1") (type "reasoning"))
  "Fold one live delta of TYPE saying TEXT into HOST's lane for SESSION."
  (nck:on-delta host session
                (nlk:json-object "turn_id" turn-id
                                 "delta" (nlk:json-object "type" type "text" text))))

;;; --- a started host over a scripted executor -----------------------------------------

(defmacro with-ask-host ((host constructor &rest keys &key answers responses (store t)
                          &allow-other-keys)
                         &body body)
  "Run BODY with HOST a started host CONSTRUCTOR builds over EXECUTOR — bound
by that name, a recording executor scripted with RESPONSES — under the
remaining KEYS, stopped on unwind."
  ;; ANSWERS are the provider stub's replies, one assistant message each;
  ;; STORE NIL runs without a temp store.
  (let* ((keys (uiop:remove-plist-keys '(:answers :responses :store) keys))
         (form `(let* ((executor (nck:make-recording-executor :responses ,responses))
                       (,host (,constructor :executor executor ,@keys)))
                  (unwind-protect (progn ,@body)
                    (nck:stop-host ,host)))))
    (when answers
      (setf form `(with-stub-provider ,(mapcar (lambda (answer)
                                                 `(wire-message "assistant" :content ,answer))
                                               answers)
                    ,form)))
    (if store
        `(with-temp-store () ,form)
        form)))

(defun channel-thread-names ()
  (mapcar #'bt2:thread-name (bt2:all-threads)))

;;; --- a platform adapter, end to end ------------------------------------------------

(defun is-typing-before-reply (plans &optional (send "send_message"))
  "Assert PLANS, a lane's recorded plans in order, carry a typing beat and the
reply, the platform's SEND, after it."
  (flet ((at (label) (position label plans :key #'nck:request-plan-audit-label :test #'equal)))
    (is-present (typing-at (at "typing_indicator")) "a typing beat was posted"
      (let ((send-at (at send)))
        (is (and send-at (< typing-at send-at)))))))

(defun is-exchange-written-back-once (room lane redelivered)
  "Assert LANE's turn completed having admitted one turn — REDELIVERED says a
redelivery admitted no second — and ROOM holds the settled exchange once."
  (is (await (:timeout 15) (plusp (count-session-events lane "turn.completed"))))
  (is (= 1 (count-session-events lane "turn.started")) redelivered)
  (is (await (:timeout 15) (plusp (count-session-events room "turn.completed"))))
  (is (= 1 (count-session-events room "turn.started"))))

(defmacro with-adapter-lane ((&key start section bindings plans reply token cleanup after)
                             &body body)
  "Run BODY against an adapter's lane end to end over the real gateway, stopped
on unwind: a provider stub answering REPLY records PROMPTS-SEEN and
MESSAGES-SEEN (what the provider was handed, newest first); BINDINGS, a LET*
list binding EXECUTOR, wrap TOKEN-PATH, a temp file holding TOKEN, and
SOUL-PATH, one holding a haiku soul; STOP is the thunk START, the adapter's
START-CHANNEL, answers for SECTION, a form over both paths."
  ;; BODY sees (PLANS), the PLANS form; CLEANUP runs after the stop, AFTER
  ;; once the lane is down.
  `(with-temp-gateway (port)
     (is (integerp port) "the real gateway serves beside the adapter")
     (let ((prompts-seen '())
           (messages-seen '()))
       (with-provider-stub (context
                             ;; the standing text the provider saw: the system
                             ;; message, then the head the lanes join ahead of the history
                             (push (format nil "~a~%~%~a"
                                           (nle::compiled-turn-context-system-prompt context)
                                           (head-text context))
                                   prompts-seen)
                             (push (nle::compiled-turn-context-messages context) messages-seen)
                             (wire-message "assistant" :content ,reply))
         (let* ,bindings
           (with-temp-file (token-path :contents ,token :type "txt")
             (with-temp-file (soul-path :contents (format nil "Answer in haiku.~%") :type "md")
               (let ((stop (,start ,section :executor executor)))
                 (nlk:with-cleanup ((funcall stop) ,@cleanup)
                   (flet ((plans () ,plans))
                     ,@body))
                 ,@after))))))))

(defun adapter-threads-stopped-p (prefix)
  "True once no thread named under PREFIX runs, polled for ten seconds: what
an adapter's stop thunk leaves behind."
  (await (:timeout 10) (notany (lambda (name) (and name (uiop:string-prefix-p prefix name)))
                               (channel-thread-names))))

(defun is-source (candidate &rest pairs)
  "One IS per KEY VALUE of PAIRS: CANDIDATE's source field KEY (NCK:SOURCE-FIELD,
NIL when absent) is VALUE, or, for a keyword KEY, the candidate's own member."
  (loop for (key value) on pairs by #'cddr
        do (is (equal value (if (keywordp key)
                                (gethash (string-downcase key) candidate)
                                (nck:source-field candidate key)))
               (format nil "~a is ~s" key value))))

(defun is-lane-carrying (platform room lane key &rest needles)
  "Assert ROOM and its LANE exist, the lane alone carries the KEY contract --
every NEEDLE in it, where it runs not -- and the haiku soul WITH-ADAPTER-LANE
wrote, and PLATFORM's status reads that soul as present."
  (is (nlk:session-exists-p room))
  (is (nlk:session-exists-p lane))
  (is-present (contract (nlk:get-harness-section lane key)) "the lane carries its room contract"
    (is (null (search "Where you are" contract)))
    (dolist (needle needles) (is (search needle contract) needle)))
  (is (null (nlk:get-harness-section room key)))
  (is (equal "Answer in haiku." (nlk:get-harness-section lane nck:+soul-section+)))
  (is (null (nlk:get-harness-section room nck:+soul-section+)))
  (is (search "(present)" (getf (nck:channel-status platform) :soul))))
