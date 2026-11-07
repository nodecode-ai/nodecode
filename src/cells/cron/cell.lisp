;;;; cell.lisp --- the primer, the verbs, /cron, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the vocabulary through a <harness> section: START-CELL
;;;; advises the core's READ-HARNESS-SECTIONS to append one virtual ("cron" .
;;;; primer) entry, the websearch shape -- static text, so the prompt prefix
;;;; holds across rounds. The verbs are plain functions in the CRON package,
;;;; every one answering a string, every refusal a CRON-ERROR the eval snippet
;;;; renders as text. The session that calls a verb is NLK:*SCRIBE-SESSION-ID*,
;;;; bound by the eval snippet: it is the job's ORIGIN, whose directory a job
;;;; without a :cwd inherits, and it is how a job's own session is refused
;;;; the scheduling verbs (hermes' rule: a scheduled run cannot schedule).
;;;;
;;;; The web page reads and moves the jobs through /api/cron/jobs, the
;;;; operator's route (NLE:ROUTE), over the same verbs.
;;;;
;;;; Slash output: the answer is the list — one line per job — which a
;;;; headless caller (NLE:SLASH, a room, a test) reads whole, and the panel a
;;;; shell opens over it: a row per job, Enter on one its record (`/cron show
;;;; ID'), whose last rows run, pause or resume, and remove it. `/cron jobs'
;;;; posts the whole table as a notice on every shell. Long-form reads are
;;;; model-facing ((cron:jobs), (cron:show), (cron:runs)).
;;;;
;;;; Config, a sibling top-level key next to `notify' and `websearch':
;;;;   "cron": {"enabled": true, "timeout_minutes": 10}
;;;; Jobs are not config: they live in the store, written by the verbs.

(in-package #:nodecode-cron)

(nlk:access (event nlk::event) (next job))

;;; --- the manual ---------------------------------------------------------------
;;; (help :cron) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "Scheduled prompts are available through the nodecode-cron cell: Lisp functions in the
cron: package, called through eval. Every one returns a string; every refusal is ERROR:
CRON-ERROR with the reason.
  (cron:add \"schedule\" \"prompt\" &key name cwd timeout-minutes model provider)   schedule one.
      Schedules: \"30m\" / \"every 2h\" / \"every hour\" (recurring interval); \"in 45m\" (once, that
      far from now); \"at 17:30\" (once, the next such time); \"every day at 9am\" / \"weekdays at
      9:30\" / \"every monday 9am\" / \"every mon,wed at 18:00\" (recurring, wall clock); five-field
      cron \"0 9 * * 1-5\"; an ISO time \"2026-09-10T09:00\" (once). Wall-clock times are the
      machine's local time. :cwd defaults to this session's directory; :model / :provider pin
      the job's session the way (select-model) pins this one.
  (cron:jobs)   one line per job: id, state, schedule, next fire (wall time and
      time left), last outcome.
  (cron:show \"id\")   the whole record.   (cron:runs \"id\" &optional 10)   the last outcomes.
  (cron:edit \"id\" &key schedule prompt name cwd timeout-minutes)   (cron:pause \"id\")
  (cron:resume \"id\")   (cron:remove \"id\")   (cron:run \"id\" &optional \"extra context\") fires now.
Each job runs in its own durable session, cron-<id>: its transcript is the record, and
(recall-log :session-id \"cron-<id>\" ...) reads it from anywhere. A fire's whole final answer
is sent to the session that scheduled the job as its next request - one user message, the
fire line then the answer - which starts a turn there, or queues behind the turn it is
running. Never poll for it: end your turn, and it arrives as the next one.
Its first line also stands on every attached shell as a notice under cron:<id>; a fire that
answers exactly SILENT, or nothing at all, sends nothing. A fire's time limit (:timeout-minutes,
default 10) is its turn's budget: past it the fire's tool calls are refused and it answers with
what it has. A job's own session cannot add, edit, resume or run jobs."
  "What (help :cron) answers while the cell runs.")

;;; --- the verbs ------------------------------------------------------------------

(defun refuse-from-job-session ()
  "hermes' rule: a scheduled run that could schedule runs is a loop."
  (when (and (stringp nlk:*scribe-session-id*)
             (find nlk:*scribe-session-id* *jobs* :key #'job-session :test #'string=))
    (fail "a scheduled job's session cannot add, edit, resume or run jobs")))

(defun timeout-seconds-from-minutes (minutes)
  (unless (and (realp minutes) (plusp minutes))
    (fail ":timeout-minutes must be a positive number, not ~s" minutes))
  (max 60 (round (* 60 minutes))))

(defun check-prompt (prompt)
  (unless (and (stringp prompt) (plusp (length (string-trim '(#\Space #\Newline) prompt))))
    (fail "prompt must be a non-empty string")))

(define-verb add-job (schedule prompt &key name cwd timeout-minutes model provider
                                          (now (get-universal-time)))
  "The whole of (cron:add) behind an explicit clock: parse, make the record
and its session, pin, persist, poke. Returns the job."
  (check-prompt prompt)
  (with-cron-lock
    (refuse-from-job-session)
    (let* ((parsed (parse-schedule schedule :now now))
           (origin nlk:*scribe-session-id*)
           ;; BASE, or BASE-2, BASE-3, ... the first that is not a job id yet.
           (id (nlk:unused-name (slug (or name prompt)) #'find-job))
           (job (make-job :id id
                          :name (or name id)
                          :schedule parsed
                          :prompt prompt
                          :cwd (or cwd (nlk:find-session-cwd origin))
                          :session (format nil "cron-~a" id)
                          :next-at (next-fire parsed now)
                          :created-at now
                          :origin origin
                          :timeout (and timeout-minutes
                                        (timeout-seconds-from-minutes timeout-minutes)))))
      (ensure-job-session job)
      (when (and (or model provider) (nlk:store-open-p))
        (nlk:record-session-model-selection job.session :provider provider :model model))
      (push job *jobs*)
      (persist-jobs)
      (poke)
      ;; A schedule fires whether or not a shell is open: the organism
      ;; outliving its last shell is the kernel's to arrange, once.
      (nle:keep-running (format nil "the cron job ~a" job.name))
      job)))

(defun add (schedule prompt &key name cwd timeout-minutes model provider)
  "Schedule PROMPT. Answers the job's id, schedule, next fire and session."
  (let ((job (add-job schedule prompt :name name :cwd cwd
                                      :timeout-minutes timeout-minutes
                                      :model model :provider provider)))
    (format nil "job ~a added: ~a; next fire ~a; session ~a"
            job.id (schedule-display job.schedule)
            (next-text job) job.session)))

(define-verb jobs ()
  "Every job, one line each."
  (jobs-text))

(define-verb show (id)
  "The whole record of job ID."
  (with-cron-lock (job-text (require-job id))))

(defmacro define-job-verb (name (id &rest lambda-list) documentation (&key refuse (poke t))
                           &body body)
  "A verb over the one job ID names: the running settings, the cron lock,
the job session's refusal when REFUSE, and JOB bound; BODY answers, and the
registry is written after it -- the ticker woken too, with POKE."
  `(define-verb ,name (,id ,@lambda-list)
     ,documentation
     (with-cron-lock
       ,@(when refuse '((refuse-from-job-session)))
       (let ((job (require-job ,id)))
         (prog1 (progn ,@body)
           (persist-jobs)
           ,@(when poke '((poke))))))))

(define-job-verb edit (id &key schedule prompt name cwd timeout-minutes)
  "Change job ID in place. A new schedule counts from now." (:refuse t)
  (when schedule
    (let* ((now (get-universal-time))
           (parsed (parse-schedule schedule :now now)))
      (setf (job-schedule job) parsed
            (job-next-at job) (next-fire parsed now))
      (when (and (job-next-at job) (eq (job-state job) :done))
        (setf (job-state job) :active))))
  (when prompt
    (check-prompt prompt)
    (setf (job-prompt job) prompt))
  (when name
    (setf (job-name job) name))
  (when cwd
    (setf (job-cwd job) cwd))
  (when timeout-minutes
    (setf (job-timeout job) (timeout-seconds-from-minutes timeout-minutes)))
  (job-text job))

(define-job-verb pause (id)
  "Hold job ID: it keeps its place and fires nothing until resumed." ()
  (setf (job-state job) :paused)
  (format nil "job ~a paused" id))

(define-job-verb resume (id)
  "Let job ID fire again, from the next due instant after now." (:refuse t)
  (let ((next (next-fire (job-schedule job) (get-universal-time))))
    (unless next
      (fail "job ~a has no fire left (~a); (cron:edit ~s :schedule ...) gives it one"
            id (schedule-display (job-schedule job)) id))
    (setf (job-state job) :active
          (job-next-at job) next)
    (format nil "job ~a resumed; next fire ~a" id (at-text next))))

(define-job-verb remove (id)
  "Forget job ID. Its session and transcript stay in the store." ()
  (setf *jobs* (cl:remove job *jobs*))
  (format nil "job ~a removed; its session ~a keeps its history" id (job-session job)))

(define-job-verb run (id &optional extra)
  "Fire job ID now, beside its schedule. EXTRA rides as run context." (:refuse t :poke nil)
  (nlk:if-let (fire (fire-job job (get-universal-time) :extra extra :manual t))
    (format nil "job ~a fired into session ~a~@[ (turn ~a)~]"
            id (job-session job) (fire-turn-id fire))
    (format nil "job ~a not fired: ~a" id (last-text job))))

(define-verb runs (id &optional (limit 10))
  "The last LIMIT outcomes of job ID, newest first, off its session's log."
  (let ((job (with-cron-lock (require-job id))))
    (unless (nlk:store-open-p)
      (fail "no store is open; nothing to read"))
    (let ((events (nlk:events :session-id job.session
                              :kind (list "turn.completed" "turn.failed" "turn.cancelled")
                              :order :newest :limit limit :as :instances)))
      (if (null events)
          (format nil "job ~a has not run yet" id)
          (format nil "~{~a~^~%~}"
                  (mapcar (lambda (event)
                            (format nil "~a  ~a~@[  ~a~]"
                                    event.occurred-at event.kind
                                    (let ((detail (cond ((equal (nlk:event-kind event) "turn.failed")
                                                         (nlk:turn-failed-detail event))
                                                        ((equal (nlk:event-kind event) "turn.cancelled")
                                                         (nlk:turn-cancelled-reason event)))))
                                      (and detail (plusp (length detail))
                                           (nlk:first-line (nlk:trimmed detail) +line-limit+)))))
                          events))))))

;;; --- /cron -------------------------------------------------------------------------

(defparameter +usage+ "/cron [jobs | show ID | run ID | pause ID | resume ID | remove ID]")

(defparameter +no-jobs-line+
  "cron: no jobs; (cron:add \"every day at 9am\" \"...\") through eval"
  "The bare /cron answer, and its hint, while no job is scheduled.")

(defun summary-order (jobs)
  "JOBS as the /cron list reads them: the active ones first, soonest fire
first, then the rest in the order they arrived."
  (append (sort (remove-if-not (lambda (job) (eq job.state :active)) jobs)
                #'< :key (lambda (job) (or job.next-at most-positive-fixnum)))
          (remove-if (lambda (job) (eq job.state :active)) jobs)))

(defun summary-text ()
  "The bare /cron answer: how many jobs, then one line per job — a vertical
list naming each job, its state and its timing."
  ;; Past three jobs it lists the first and points at /cron jobs for the rest;
  ;; the one-line form rides the composer hint.
  (with-cron-lock
    (if (null *jobs*)
        +no-jobs-line+
        (let* ((ordered (summary-order (reverse *jobs*)))
               (shown (subseq ordered 0 (min 3 (length ordered)))))
          (format nil "cron: ~d job~:p~%~{- ~a~^~%~}~@[~%+~d more (/cron jobs)~]"
                  (length ordered)
                  ;; Next fire while active, else the last outcome.
                  (mapcar (lambda (job)
                            (format nil "~a — ~(~a~), ~a"
                                    job.id job.state
                                    (if (eq (job-state job) :active) (next-phrase job) (last-phrase job))))
                          shown)
                  (and (> (length ordered) 3) (- (length ordered) 3)))))))

(defun next-phrase (job)
  "JOB's next fire in a phrase: `next 2026-09-05 09:00 (in 23h)'."
  (if job.next-at (format nil "next ~a" (at-text job.next-at)) "no fire left"))

(defun last-phrase (job &aux (last job.last))
  "JOB's last outcome in a phrase: `last ok 09-04 09:00'."
  (if last
      (format nil "last ~a ~a" (getf last :status) (subseq (local-text (getf last :at)) 5))
      "never fired"))

;;; The panel a shell opens on bare /cron: a row per job marked by how it
;;; stands, Enter its record and Space a pause or a resume. The record ends in
;;; the rows that move the job; every key runs a /cron line, so a shell, the
;;; web page and a room reach the one set of verbs.

(defun firing-p (job)
  (find job.id *inflight* :key #'fire-job-id :test #'string=))

(defun job-mark (job)
  "=> (values MARK TONE): JOB in the panel's mark column -- firing now,
paused, done, its last fire gone wrong, else waiting on its next."
  (cond ((firing-p job) (values "▸" :accent))
        ((eq job.state :paused) (values "‖" :muted))
        ((eq job.state :done) (values "·" :muted))
        ((member (getf job.last :status) '("failed" "cancelled" "missed") :test #'equal)
         (values "!" :tool))
        (t (values "●" :success))))

(defun job-row (job width)
  "JOB's row in the panel: its id, padded to WIDTH, schedule, timing and last
outcome."
  (multiple-value-bind (mark tone) (job-mark job)
    (list :mark mark :mark-tone tone :label (format nil "~va" width job.id)
          :detail (format nil "~a · ~a · ~a"
                          (schedule-display job.schedule)
                          (cond ((firing-p job) "firing now")
                                ((eq job.state :active) (next-phrase job))
                                (t (string-downcase job.state)))
                          (last-phrase job))
          :run (list :enter (format nil "/cron show ~a" job.id)
                     :space (format nil "/cron ~:[resume~;pause~] ~a"
                                    (eq job.state :active) job.id)))))

(defun panel ()
  "The dialog bare /cron answers: every job in the summary's order."
  (with-cron-lock
    (let ((jobs (summary-order (reverse *jobs*))))
      (nle:list-dialog
       :title (format nil "Cron (~d)" (length jobs))
       :context (format nil "~:[no jobs~;~:*~{~a~^ · ~}~]"
                        (loop for state in '(:active :paused :done)
                              for count = (count state jobs :key #'job-state)
                              when (plusp count)
                                collect (format nil "~d ~(~a~)" count state)))
       :rows (let ((width (reduce #'max jobs :key (lambda (job) (length job.id)) :initial-value 0)))
               (mapcar (lambda (job) (job-row job width)) jobs))
       :searchable t
       :search-placeholder "Search jobs"
       :empty-label "No jobs · ask for one, or (cron:add \"every day at 9am\" \"...\") through eval"
       :hint "space pause/resume  enter details  esc close  ↑↓ navigate  type search"))))

(defun wrapped (text width)
  "TEXT as lines of at most WIDTH characters broken between words, its own
line breaks kept; a longer word stands on a line of its own."
  (loop for line in (nlk:lines text)
        append (let ((out '()) (current nil))
                 (dolist (word (nlk:split-words line))
                   (if (and current (> (+ (length current) 1 (length word)) width))
                       (setf out (cons current out) current word)
                       (setf current (if current (format nil "~a ~a" current word) word))))
                 (reverse (cons (or current "") out)))))

(defun record-panel (job)
  "The dialog `/cron show ID' answers: JOB's record a fact a row, the prompt
a row a line, then the rows that run, pause or resume, and remove it."
  (flet ((fact (key text) (list :mark " " :label (format nil "~8a" key) :detail text))
         (act (mark tone label detail verb)
           (list :mark mark :mark-tone tone :label (format nil "~8a" label) :detail detail
                 :run (list :enter (format nil "/cron ~a ~a" verb job.id)))))
    (nle:list-dialog
     :title "Cron"
     :context (format nil "~a~@[ · ~a~]" job.id (and (string/= job.name job.id) job.name))
     :rows (append
            (list (fact "state" (format nil "~(~a~)~:[~; · firing now~] · fired ~d time~:p"
                                        job.state (firing-p job) job.fires))
                  (fact "schedule" (schedule-display job.schedule))
                  (fact "next" (if (eq job.state :active) (next-text job) (format nil "- (~(~a~))" job.state)))
                  (fact "last" (last-text job)))
            (loop for line in (wrapped job.prompt 72)
                  for key = "prompt" then ""
                  collect (fact key line))
            (when job.cwd (list (fact "folder" job.cwd)))
            (list (fact "session" job.session)
                  (fact "timeout" (format nil "~a~:[~; (the section's)~]"
                                          (duration-text (job-timeout-seconds job)) (null job.timeout))))
            (when job.origin (list (fact "from" job.origin)))
            (when job.report
              (list (fact "report" (format nil "waiting to be sent since ~a"
                                           (local-text (getf job.report :at))))))
            (list (act "▸" :accent "Run now" "fire it once, beside its schedule" "run")
                  (if (eq job.state :active)
                      (act "‖" :muted "Pause" "hold it: nothing fires until it is resumed" "pause")
                      (act "●" :success "Resume" "fire again from the next due time" "resume"))
                  (act "×" :danger "Remove" "forget it; its session keeps its history" "remove")))
     :hint "enter run the action  esc back  ↑↓ navigate")))

(defun run-slash (args &aux (words (nlk:split-words args))
                            (head (first words))
                            (id (second words)))
  "The answer to one /cron invocation: what every caller shows, and the
panel a shell opens over it, bare or for one job's record."
  (cond
    ((null head) (values (summary-text) (panel)))
    ((member head '("jobs" "list") :test #'string=)
     (nle:notice (jobs-text) :level :info)
     (format nil "cron: ~d job~:p posted" (length *jobs*)))
    ((null id) (format nil "cron: ~a needs a job id; usage ~a" head +usage+))
    ((string= head "show") (values (show id) (with-cron-lock (record-panel (require-job id)))))
    ((string= head "run") (run id))
    ((string= head "pause") (pause id))
    ((string= head "resume") (resume id))
    ((string= head "remove") (remove id))
    (t (format nil "cron: unknown subcommand ~a; usage ~a" head +usage+))))

;;; --- the page's route ---------------------------------------------------------------
;;; The web page's Control pane reads the registry as its row holds it
;;; (JOB-JSON) and moves a job through the verbs the model calls, so a
;;; refusal is the verb's own words. The arguments ride the query: a verb
;;; takes strings, and the gateway's JSON reader is its own.

(defun jobs-route (env &aux (query (quri:url-decode-params (or (getf env :query-string) ""))))
  "/api/cron/jobs: GET answers {jobs}, every job oldest first; POST ?op=OP
runs one verb -- add (schedule, prompt, name, cwd), run, pause, resume or
remove (id) -- and answers {text, jobs}, the verb's answer and the jobs after."
  (flet ((param (name) (cdr (assoc name query :test #'string=))))
    (let* ((op (and (eq (getf env :request-method) :post) (param "op")))
           (id (param "id"))
           (text (cond ((null op) nil)
                       ((string= op "add")
                        (add (param "schedule") (param "prompt")
                             :name (param "name") :cwd (param "cwd")))
                       ((string= op "run") (run id))
                       ((string= op "pause") (pause id))
                       ((string= op "resume") (resume id))
                       ((string= op "remove") (remove id))
                       (t (fail "no op ~s; add, run, pause, resume or remove" op)))))
      (nlk:json-object :opt "text" text
                       "jobs" (with-cron-lock (map 'vector #'job-json (reverse *jobs*)))))))

;;; --- the entry -----------------------------------------------------------------------

(defvar *ticker* t
  "Whether the start runs the ticker thread; a test drives TICK by hand.")

(defun install ()
  "The registry off the store, the fires recovered with it, the one ticker
thread, and the standing line for the jobs that keep the gateway up."
  (with-cron-lock
    (setf *inflight* '())
    (load-jobs)
    ;; A job whose session still runs a recovered turn gets an in-flight record.
    (dolist (job *jobs*)
      (when (and (session-turn-active-p job.session)
                 (not (find job.id *inflight* :key #'fire-job-id :test #'string=)))
        (push (make-fire :job-id job.id :session job.session) *inflight*)))
    ;; The first tick runs at once: a report or a fire that waited through a
    ;; restart is owed now, not at the next due instant.
    (when (and *ticker* (null *worker*))
      (setf *worker* (nlk:worker-start "cron-ticker" #'tick
                                       :wake (lambda ()
                                               (seconds-until-wake (get-universal-time)))
                                       :prime t))))
  ;; The registry row stays in the store; the in-image list is dropped.
  (nle:on-stop (lambda ()
                 (setf *worker* (nlk:worker-stop *worker*))
                 (with-cron-lock (setf *inflight* '() *jobs* '()))))
  (let ((active (count :active *jobs* :key #'job-state)))
    (when (plusp active)
      (nle:keep-running (format nil "~d cron job~:p" active)))))

(nle:define-cell cron
  (:section ("cron")
    (:guide "timeout_minutes bounds one fire's turn; a job may name a limit of its own")
    ("timeout_minutes" :integer :default 10 :min 1
                       :doc "how long a fire's turn may run before its calls are refused"))
  (:start #'install)
  (:hook :frame (nlk:observer #'observe-frame "cron"))
  (:hook 'nle:turn-budget #'budget-hook)
  (:help :cron "cron:add schedules a prompt (\"every day at 9am\", \"in 45m\"); a fire's answer arrives as your next turn" +primer+)
  (:route "/api/cron/jobs" #'jobs-route)
  (:command "cron" (lambda (args session-id)
                     (declare (ignore session-id))
                     (run-slash args))
            :description "Scheduled jobs: the list, jobs, show ID, run ID, pause ID, resume ID, remove ID"
            :argument-hint "jobs | show ID | run ID | pause ID | resume ID | remove ID"))
