;;;; ticker.lisp --- one sleeping thread, the fire, and the outcome it reads back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The ticker sleeps until the nearest due instant and is poked by every
;;;; mutation (the eval-guard shape in engine/exec.lisp: a semaphore waited
;;;; on with a timeout). It never polls a fixed cadence.
;;;;
;;;; A fire is one NLE:SUBMIT into the job's session with a command id
;;;; naming the due instant, `cron:<id>:<unix>': the kernel's durable
;;;; admission makes a second submit of the same id a :DUPLICATE that
;;;; spawns nothing, so at-most-once needs no claim record of its own. The
;;;; schedule is advanced BEFORE the submit: a crash between
;;;; the two loses a fire, never doubles one. A job whose previous fire is
;;;; still running is skipped for this due instant and told so, not queued
;;;; behind itself. A due instant seen later than half its period (clamped
;;;; to two minutes and two hours) is a miss: recorded, said, and the
;;;; schedule fast-forwarded past now, so an outage never burst-fires.
;;;;
;;;; The outcome is read off the :FRAME point. The hook runs on the turn
;;;; worker that publishes, so it folds and pokes ONLY -- the newest
;;;; assistant text and the terminal kind land on the in-flight record
;;;; under the lock, and the ticker thread settles it: the job's LAST, the
;;;; standing notice under `cron:<id>' (the board reaches every attached
;;;; shell and every session's live tail), the row. An answer that is
;;;; exactly SILENT settles quietly. A fire runs under its job's timeout as
;;;; the turn's budget (BUDGET-HOOK on NLE:TURN-BUDGET): past it the fire's
;;;; tool calls are refused and its answer is what it has, so a slow fire
;;;; still reports and the ticker never cancels one.
;;;;
;;;; An answer is also RETURNED to the session that scheduled the job (the
;;;; job's ORIGIN) as that session's next request: one user message -- the
;;;; fire line, a colon, the whole answer -- through the same
;;;; NLE:SUBMIT every operator prompt takes, which starts a turn there when
;;;; the session is idle and queues behind the turn it is running otherwise
;;;; (the eval-wake shape: *SESSION-WAKE*). So the operator sees the cron's
;;;; result arrive as a request and the model there answers it in a turn of
;;;; its own. Without this return, a settled exchange recorded while idle
;;;; would land as a detached pair beneath the model's own summary, and a
;;;; steer would fold the text into a running turn without a turn of its
;;;; own. Until the ingress takes the message the report waits on the job's
;;;; row (durably, across a restart).
;;;;
;;;; THREAD RULE: SUBMIT-PROMPT is the one seam that reaches the organism's
;;;; ingress -- a fire into the job's session and a report into its origin
;;;; both go through it -- called on the ticker thread or a verb's caller
;;;; thread, never on the publishing thread; a test stubs it.

(in-package #:nodecode-cron)

(nlk:access (admission nlk::active-input-admission))

(nlk:define-record (fire (:copier nil))
  "One fire in flight: the job, its session, the admission, and what the
turn has said so far."
  ;; STATUS is NIL until a terminal fact lands, then the fact's kind. Guarded
  ;; by *LOCK*.
  (job-id "" :type string)
  (session "" :type string)
  (turn-id nil :type (or null string))
  (answer nil :type (or null string))
  (status nil :type (or null string))
  (detail nil :type (or null string)))

(defvar *inflight* '()
  "Fires whose turn has not settled. Guarded by *LOCK*.")

(defvar *worker* nil
  "The ticker thread while it runs (an NLK:WORKER), or NIL.")

(defun poke ()
  "Wake the ticker: something due, or an outcome changed."
  (nlk:worker-poke *worker*))

;;; --- the seams -------------------------------------------------------------------

(defun submit-prompt (session prompt command-id job-id)
  "PROMPT into SESSION through the one in-process ingress; (values
DISPOSITION TURN-ID)."
  (let ((admission
          ;; Provenance: source kind `cron', source id the job.
          (nle:submit session prompt :command-id command-id :source "cron" :source-id job-id)))
    (values admission.disposition admission.turn-id)))

(defun ensure-job-session (job)
  "Hold the job's session standing by if it is not durable already: the
fire's own prompt is its first block, and a job that never fires leaves no
session behind. Nothing to do without a store."
  (when (nlk:store-open-p)
    (let ((session job.session))
      (or (nlk:session-exists-p session) (progn (nlk:standby-session :id session :cwd job.cwd) t)))))

(defun session-turn-active-p (session)
  (and (nlk:store-open-p) (nlk:active-turn-p session)))

;;; --- the prompt -------------------------------------------------------------------

(defparameter *fire-preamble*
  "[cron ~s, fire ~d, ~a, schedule ~a. This session is the job's durable record. ~
Your final answer, whole, is sent to the session that scheduled this job as its next ~
request, and its first line stands on every attached shell as a notice; answer exactly ~
SILENT when there is nothing to report. A fire has ~a: past it, tool calls are refused, so ~
end with what is done and what is left.]"
  "The line every fire opens with: the job, the count, the local time, the
schedule, the delivery rule, the limit. Data, so a layer can reword it.")

;;; --- outcomes on the record ---------------------------------------------------------

(defun job-timeout-seconds (job)
  (or job.timeout
      (and *cron* (* 60 (setting :timeout-minutes)))
      600))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a turn in a job's session -- a scheduled fire,
(cron:run ...), a fire recovered at boot -- runs under the job's timeout."
  ;; Past it the turn's tool calls are refused and it answers with what it
  ;; has.
  (nlk:if-let (job (with-cron-lock
                     (find (getf turn :session-id) *jobs* :key #'job-session :test #'equal)))
    (list :seconds (job-timeout-seconds job))
    (funcall next turn)))

(defun set-last (job now status &optional line turn-id)
  (setf job.last (list :at now :status status :line line :turn-id turn-id)))

(defun post (job level control &rest args)
  "Say something about JOB on the board under its key."
  ;; Never fails a fire: a notice that cannot be delivered is warned about and
  ;; dropped.
  (nlk:with-handlers ((error (condition)
                        (warn "cron: notice for ~a not posted: ~a" (job-id job) condition)))
    (nle:notice (apply #'format nil control args) :level level :key (format nil "cron:~a" job.id))))

(defparameter +line-limit+ 160
  "How much of an answer's first line rides on the board and the record.")

;;; --- firing ---------------------------------------------------------------------------

(defun advance-job (job now due &aux (next (next-fire job.schedule now :anchor due)))
  "Move the job past the fire due at DUE: the next fire after NOW, an
interval anchored on DUE so its phase holds; none left is :DONE."
  (setf job.next-at next)
  (unless next
    (setf job.state :done))
  next)

(defun fire-job (job now &key extra manual)
  "Fire JOB now: advance its schedule, then submit one prompt into its
session."
  ;; MANUAL is a (cron:run) -- the schedule is left alone and the command id
  ;; names this instant. Returns the FIRE in flight, or NIL when nothing was
  ;; submitted (the previous fire still runs, the admission was a duplicate,
  ;; the ingress refused).
  (let* ((id job.id)
         (due (or job.next-at now))
         (command-id (if manual
                         (format nil "cron:~a:run:~d" id (unix-from-universal now))
                         (format nil "cron:~a:~d" id (unix-from-universal due)))))
    (unless manual
      (advance-job job now due))
    (incf job.fires)
    (when (session-turn-active-p job.session)
      (set-last job now "overrun" "previous fire still running")
      (post job :warning "cron ~a: previous fire still running; this fire skipped" id)
      (return-from fire-job nil))
    (let ((fire (make-fire :job-id id :session job.session))
          ;; The fire that makes the job's session names it for the job, as
          ;; every list reads it: "minute check · every 1m", not the fire's
          ;; header. Once only, so a name the operator gives it stands.
          (unnamed (and (nlk:store-open-p) (nlk:on-standby-p job.session))))
      ;; On the list before the submit: a turn that dies inside the
      ;; submit's own publish still finds its record.
      (push fire *inflight*)
      (or (nlk:with-handlers ((error (condition)
                                (set-last job now "error" (princ-to-string condition))
                                (post job :error "cron ~a not fired: ~a" id condition)
                                nil))
            (ensure-job-session job)
            (multiple-value-bind (disposition turn-id)
                (submit-prompt job.session
                               (format nil "~?~%~%~a~@[~%~%Run context: ~a~]"
                                       *fire-preamble*
                                       (list job.name job.fires (local-text now)
                                             (schedule-display job.schedule)
                                             (duration-text (job-timeout-seconds job)))
                                       job.prompt extra)
                               command-id id)
              (case disposition
                ((:started :queued)
                 (when (and turn-id (null fire.turn-id)) (setf fire.turn-id turn-id))
                 (when (and unnamed (not (nlk:on-standby-p job.session)))
                   (nle:set-session-title job.session (format nil "~a · ~a" job.name
                                                              (schedule-display job.schedule))))
                 fire)
                (:duplicate
                 (set-last job now "duplicate" "this fire was already admitted")
                 (post job :warning "cron ~a: the ~a fire was already admitted; not fired twice"
                       id (local-text due))
                 nil)
                (t
                 (set-last job now "error" (format nil "admission answered ~a" disposition))
                 nil))))
          ;; Nothing was submitted: the fire leaves the list.
          (progn (setf *inflight* (cl:remove fire *inflight*))
                 nil)))))

;;; --- the :FRAME hook ----------------------------------------------------------------------

(defun observe-frame (op)
  "Fold one published frame into the fire it belongs to, if any: the
newest assistant text, the terminal kind. Publishing thread; no I/O."
  (nlk:bind (((type fact _ turn-id) (nlk:frame-fact op)))
    (let ((session (getf op :session-id)))
      (when (and (stringp session) type)
        (with-cron-lock
          ;; The unsettled fire on SESSION whose turn is TURN-ID or not yet known.
          (nlk:when-let (fire (find-if (lambda (fire)
                                         (and (string= fire.session session)
                                              (null fire.status)
                                              (or (null fire.turn-id) (equal fire.turn-id turn-id))))
                                       *inflight*))
            (when (and turn-id (null fire.turn-id))
              (setf fire.turn-id turn-id))
            (cond
              ((equal type "turn.assistant_message_completed")
               (nlk:when-let (text (nlk:fact-message-content fact))
                 (setf fire.answer text)))
              ((member type '("turn.completed" "turn.failed" "turn.cancelled")
                       :test #'equal)
               (setf fire.status type
                     fire.detail (or (nlk:json-value fact :string "detail")
                                            (nlk:json-value fact :string "reason")))
               (poke)))))))))

;;; --- the report ------------------------------------------------------------------------------

;;; Data, so a layer can reword it.
(defparameter *report-input*
  "[cron ~s fired ~a, schedule ~a; its session is ~a]"
  "The first line of the request the origin receives: the job, the fire's
local time, the schedule, the session that holds the whole run.")

(defparameter *report-message*
  "~a: ~a"
  "The whole request: the fire line, a colon, the answer -- the cron and its
output in one user message.
Data, so a layer can reword it.")

;;; --- settling -----------------------------------------------------------------------------

(defun settle-fire (fire now)
  "Take a terminal fire off the list and onto its job: LAST and the board."
  (setf *inflight* (cl:remove fire *inflight*))
  (let* ((id fire.job-id)
         (job (find-job id))
         (status fire.status)
         (detail-line (nlk:first-line (nlk:trimmed (or fire.detail "")) +line-limit+))
         (turn-id fire.turn-id))
    (when job
      (cond
        ((equal status "turn.completed")
         ;; Silence: the marker alone — SILENT, [SILENT], NO_REPLY, NO REPLY,
         ;; trimmed and case-insensitive — or no answer at all. A blank is
         ;; silence too: a real failure arrives as a failure, never as a blank.
         (if (member (nlk:trimmed (or fire.answer ""))
                     '("" "silent" "[silent]" "no_reply" "no reply")
                     :test #'string-equal)
             (set-last job now "silent" nil turn-id)
             (let ((line (nlk:first-line (nlk:trimmed fire.answer) +line-limit+))
                   (origin job.origin))
               (set-last job now "ok" (and (plusp (length line)) line) turn-id)
               ;; The report owed to the origin: none without one, or when it is the job's own.
               (when (and (plusp (length line))
                          (stringp origin) (plusp (length origin))
                          (not (string= origin job.session)))
                 (setf job.report
                       (list :input (format nil *report-input* job.name (local-text now)
                                            (schedule-display job.schedule) job.session)
                             :answer fire.answer :at now)))
               (post job :info "cron ~a: ~a" id
                     (if (plusp (length line)) line "(finished with no text)")))))
        ((equal status "turn.failed")
         (set-last job now "failed" detail-line turn-id)
         (post job :error "cron ~a failed: ~a" id detail-line))
        (t
         (set-last job now "cancelled" detail-line turn-id)
         (post job :info "cron ~a: cancelled~@[: ~a~]" id
               (and (plusp (length detail-line)) detail-line)))))))

;;; --- the tick -----------------------------------------------------------------------------

(defun tick (&optional (now (get-universal-time)))
  "Everything the clock owes at NOW: settle the fires that ended, fire or
miss every due job, write the row once."
  ;; Returns the number of fires started.
  (with-cron-lock
    (let ((fired 0)
          (changed nil))
      (dolist (fire (cl:remove-if-not #'fire-status (copy-list *inflight*)))
        (settle-fire fire now)
        (setf changed t))
      ;; Every pending report, once: it waits without a store or a willing ingress.
      (dolist (job (copy-list *jobs*))
        (let ((origin job.origin)
              (report job.report))
          (cond
            ((or (null report) (null (nlk:store-open-p))))
            ((not (nlk:session-exists-p origin))
             (setf job.report nil
                   changed t)
             (post job :warning "cron ~a: session ~a that scheduled it is gone; ~
                                 the answer stands in ~a"
                   job.id origin job.session))
            ((member (nlk:with-handlers ((error (condition)
                                           (warn "cron: report for ~a not sent to ~a: ~a"
                                                 (job-id job) origin condition)
                                           nil))
                       (submit-prompt origin (format nil *report-message* (getf report :input)
                                                     (getf report :answer))
                                      (format nil "cron:~a:report:~d" (job-id job)
                                              (unix-from-universal (getf report :at)))
                                      (job-id job)))
                     '(:started :queued :duplicate))
             (setf job.report nil
                   changed t)))))
      (dolist (job (copy-list *jobs*))
        (let ((due job.next-at))
          (when (and (eq job.state :active) due (<= due now))
            (setf changed t)
            (if (> (- now due) (schedule-grace job.schedule due))
                (let* ((late (- now due))
                       (next (advance-job job now due)))
                  (set-last job now "missed"
                            (format nil "due ~a, seen ~a late" (local-text due) (duration-text late)))
                  (post job :warning "cron ~a: missed its ~a fire by ~a~@[; next ~a~]"
                        job.id (local-text due) (duration-text late)
                        (and next (at-text next now))))
                (when (fire-job job now)
                  (incf fired))))))
      (when changed
        (persist-jobs))
      fired)))

(defparameter +longest-sleep-seconds+ 3600
  "The ticker looks at the clock at least hourly whatever the registry says.")

(defun seconds-until-wake (now)
  "Seconds to the nearest due instant, at most an hour."
  (with-cron-lock
    (max 0.05 (- (reduce #'min (loop for job in *jobs*
                                     when (and (eq job.state :active) job.next-at)
                                       collect job.next-at
                                     ;; A report the ingress refused for now is tried again soon.
                                     when job.report collect (+ now 60))
                         :initial-value (+ now +longest-sleep-seconds+))
                 now))))
