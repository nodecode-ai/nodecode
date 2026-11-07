;;;; cell-test.lisp --- lifecycle, the registry row, the fire, the outcome.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the config surface, the
;;;; hooks and /cron, the registry round trip through a real store, that
;;;; a due job fires once through the stubbed ingress with the schedule
;;;; advanced first, that the outcome read off the :frame point lands on the
;;;; record and the board, that SILENT settles quietly, that lateness within
;;;; grace fires and beyond it misses, that a fire runs under its job's
;;;; timeout as its turn's budget, that a duplicate admission is recorded and not retried, that
;;;; a job's own session is refused the scheduling verbs, and that an answer
;;;; is sent to the session that scheduled the job as its next request --
;;;; one user message, the fire line then the answer -- held on the row
;;;; while the ingress refuses it.

(in-package #:nodecode.test)

(nlk:access (job nodecode-cron::job))

(defun cron-add (schedule prompt &rest keys &key origin &allow-other-keys)
  "ADD-JOB behind the test's clock unless the caller names one, scheduled from
session ORIGIN when given."
  (let ((nlk:*scribe-session-id* (or origin nlk:*scribe-session-id*)))
    (apply #'nodecode-cron::add-job schedule prompt
           (append (alexandria:remove-from-plist keys :origin) (list :now +cron-now+)))))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "cron"
  (:config "timeout_minutes" 3)
  (:hooks :frame 'nle:turn-budget)
  (:help :cron)
  (:command "cron")
  (:running (is (equal nodecode-cron::+primer+ (nle:help :cron)))
            (is (= 3 (nodecode-cron::setting :timeout-minutes)) "timeout_minutes is read")
            (is (bt2:thread-alive-p (nlk:worker-thread nodecode-cron::*worker*)) "the ticker runs"))
  (:stopped (is (null nodecode-cron::*worker*) "and the ticker is gone"))
  (:refused ("timeout_minutes" 0))
  (:idle cron:cron-error (cron:jobs) (cron:add "30m" "x")))

;;; --- the registry -----------------------------------------------------------------------

(deftest cron-cell-registry-round-trips-through-the-store (with-cron-runtime (:store t))
  (let ((job (cron-add "every day at 9am" "Summarize the inbox"
                       :name "Inbox digest" :timeout-minutes 3)))
    (is-shape job (.id "inbox-digest" "the id is the name's slug") (.session "cron-inbox-digest"))
    (is (nlk:on-standby-p "cron-inbox-digest"))
    (is-shape job (.next-at = (cron-local 2026 9 5 9 0)) (.timeout = 180))
    (is (nlk:session-state-get "cron" "jobs") "the registry row exists")
    ;; A second job with the same name gets the next id.
    (is (equal "inbox-digest-2"
               (nodecode-cron::job-id (cron-add "30m" "again" :name "Inbox digest"))))
    (setf nodecode-cron::*jobs* '())
    (is (= 2 (nodecode-cron::load-jobs)) "reloading reads both back")
    (let ((back (nodecode-cron::find-job "inbox-digest")))
      (is-present back "the first job came back"
        (is (equal "Summarize the inbox" (nodecode-cron::job-prompt back)))
        (is (equal "every day at 9am"
                   (nodecode-cron::schedule-display (nodecode-cron::job-schedule back))))
        (is (equal '((0) (9) t t t)
                   (nodecode-cron::schedule-fields (nodecode-cron::job-schedule back))))
        (is-shape back (nodecode-cron::job-next-at = (cron-local 2026 9 5 9 0))
          (nodecode-cron::job-state eq :active) (nodecode-cron::job-timeout = 180))))
    (with-stubbed-fdefinitions ((get-universal-time () +cron-now+))
      (is (search
           "inbox-digest  active  every day at 9am  next 2026-09-05 09:00 (in 23h)  last never fired"
           (cron:jobs))))
    (is (search "prompt: Summarize the inbox" (cron:show "inbox-digest")))
    (is (search "has not run yet" (cron:runs "inbox-digest")))
    (is (search "removed" (cron:remove "inbox-digest-2")))
    (setf nodecode-cron::*jobs* '())
    (is (= 1 (nodecode-cron::load-jobs)) "the removal was written")))

;;; --- the fire ------------------------------------------------------------------------------

(deftest cron-cell-tick-fires-a-due-job-once-and-settles-its-outcome ()
  (with-cron-runtime (:submits t)
    (let* ((job (cron-add "every 2h" "Say hello" :name "hello"))
           (due (cron-local 2026 9 4 12 0)))
      (is (= due (nodecode-cron::job-next-at job)) "due in two hours")
      (is (= 0 (nodecode-cron::tick +cron-now+)) "nothing due yet")
      (is (null (cron-submits)))
      (is (= 1 (nodecode-cron::tick due)) "one fire at the due instant")
      (let ((submit (first (cron-submits))))
        (is-present submit "the submit reached the seam"
          (is-shape submit (:session "cron-hello" "into the job's session")
            (:command-id (format nil "cron:hello:~d" (nodecode-cron::unix-from-universal due)))
            (:job-id "hello"))
          (is (search "Say hello" (getf submit :prompt)) "the prompt is the job's")
          (is (search "[cron \"hello\", fire 1, 2026-09-04 12:00, schedule every 2h."
                      (getf submit :prompt)))
          (is (search "answer exactly SILENT" (getf submit :prompt)))))
      (is-shape job (nodecode-cron::job-next-at = (+ due 7200)) (nodecode-cron::job-fires = 1))
      (is (= 1 (length nodecode-cron::*inflight*)) "one fire in flight")
      (is (equal "t1" (nodecode-cron::fire-turn-id (first nodecode-cron::*inflight*))))
      (is (= 0 (nodecode-cron::tick due)) "the same instant does not fire twice")
      ;; The turn answers, then completes; both frames arrive on the observer.
      (is (eq t (cron-publish "turn.assistant_message_completed" "cron-hello"
                              :payload (nlk:json-object
                                        "message" (nlk:json-object "role" "assistant" "content"
                                                                   (format nil "Hello there~%and more"))))))
      (cron-finish "cron-hello")
      (is (equal "turn.completed"
                 (nodecode-cron::fire-status (first nodecode-cron::*inflight*))))
      (nodecode-cron::tick (+ due 30))
      (is (null nodecode-cron::*inflight*) "the tick settled it")
      (let ((last (nodecode-cron::job-last job)))
        (is-shape last (:status "ok") (:line "Hello there" "the answer's first line")
          (:turn-id "t1")))
      (is (equal "cron hello: Hello there" (cron-board-text "hello")))
      (is (null (nodecode-cron::job-report job)))
      (is (search "last ok 09-04 12:00 \"Hello there\"" (cron:jobs))))))

(deftest cron-cell-a-jobs-session-is-named-for-the-job (with-cron-runtime (:store t))
  ;; Every list reads the session as the job -- "hello · every 2h" -- not as
  ;; its first fire's header, which the model still reads as the prompt. The
  ;; fire that makes the session names it, once.
  (with-stubbed-fdefinitions ((nodecode-cron::submit-prompt (session prompt command-id job-id)
                                (declare (ignore job-id))
                                (let ((admission (nlk:admit-active-turn-input session command-id prompt)))
                                  (values (nlk:active-input-admission-disposition admission)
                                          (nlk:active-input-admission-turn-id admission)))))
    (cron-add "every 2h" "Say hello" :name "hello")
    (is (= 1 (nodecode-cron::tick (cron-local 2026 9 4 12 0))) "the first fire")
    (is (equal "hello · every 2h" (nlk::session-title "cron-hello")))
    (let ((input (nlk:events :session-id "cron-hello" :kind nlk::+kind-input-committed+
                             :as :payload :order :oldest)))
      (is (search "[cron \"hello\", fire 1" (gethash "message" input)) "the prompt still opens on the header"))))

(deftest cron-cell-blank-and-every-silence-spelling-go-quiet ()
  ;; The marker is not one spelling, and a blank answer is silence too: none
  ;; of these reaches the board or the notices.
  (dolist (answer '(" [SILENT] " "NO_REPLY" " no reply " "[SILENT]" ""
                    ;; A fire that never wrote an answer at all is the same silence.
                    nil))
    (with-cron-runtime (:submits t :job (job "in 1m" "Report only news" :name "quiet"))
      (nodecode-cron::tick (+ +cron-now+ 60))
      (is (eq :done (nodecode-cron::job-state job)) "a one-shot is done once fired")
      (is (null (nodecode-cron::job-next-at job)))
      (cron-finish "cron-quiet" answer)
      (nodecode-cron::tick (+ +cron-now+ 90))
      (is (equal "silent" (getf (nodecode-cron::job-last job) :status))
          (format nil "~s settles silently" answer))
      (is (null (cron-board-text "quiet")) "nothing on the board"))))

(deftest cron-cell-failed-and-cancelled-turns-land-on-the-record ()
  (with-cron-runtime (:submits t :job (job "30m" "x" :name "fails"))
    (nodecode-cron::tick (+ +cron-now+ 1800))
    (cron-publish "turn.failed" "cron-fails" :payload (nlk:json-object "detail" "provider said no"))
    (nodecode-cron::tick (+ +cron-now+ 1801))
    (is (equal "failed" (getf (nodecode-cron::job-last job) :status)))
    (is (equal "provider said no" (getf (nodecode-cron::job-last job) :line)))
    (is (equal "cron fails failed: provider said no" (cron-board-text "fails")))
    (is (eq :error (third (cell-notice "cron:fails"))))
    ;; The next fire is cancelled by the operator: the cell never cancels one.
    (nodecode-cron::tick (+ +cron-now+ 3600))
    (cron-publish "turn.cancelled" "cron-fails" :payload (nlk:json-object "reason" "operator"))
    (nodecode-cron::tick (+ +cron-now+ 3601))
    (is (equal "cancelled" (getf (nodecode-cron::job-last job) :status)))
    (is (equal "cron fails: cancelled: operator" (cron-board-text "fails")))))

(deftest cron-cell-lateness-within-grace-fires-and-beyond-it-misses ()
  (with-cron-runtime (:submits t :job (job "every 2h" "x" :name "late"))
    ;; Due at 12:00, seen at 12:30: inside the hour of grace.
    (is (= 1 (nodecode-cron::tick (cron-local 2026 9 4 12 30))) "fires")
    (is (= (cron-local 2026 9 4 14 0) (nodecode-cron::job-next-at job)))
    (cron-finish "cron-late")
    (nodecode-cron::tick (cron-local 2026 9 4 12 31))
    ;; Due at 14:00, seen at 17:10: three hours late, past the grace.
    (is (= 0 (nodecode-cron::tick (cron-local 2026 9 4 17 10))) "does not fire")
    (is (= 1 (length (cron-submits))) "still one submit")
    (is (equal "missed" (getf (nodecode-cron::job-last job) :status)))
    (is (= (cron-local 2026 9 4 18 0) (nodecode-cron::job-next-at job)))
    (is (search
         "cron late: missed its 2026-09-04 14:00 fire by 3h 10m; next 2026-09-04 18:00 (in 50m)"
         (cron-board-text "late")))))

(deftest cron-cell-a-fire-runs-under-its-job-timeout-as-its-budget ()
  ;; The job's limit is its fires' budget: past it a fire's calls are refused
  ;; and it answers with what it has, so a slow fire still reports and the
  ;; ticker cancels nothing.
  (with-cron-runtime (:submits t :job (job "30m" "x" :name "slow" :timeout-minutes 2))
    (is (= 120 (nodecode-cron::job-timeout job)))
    (is (equal '(:seconds 120) (nle::live-turn-budget (nle::open-live-turn "cron-slow" "t1"))))
    (is (null (nle::live-turn-budget (nle::open-live-turn "s-other" "t2"))))
    (nodecode-cron::tick (+ +cron-now+ 1800))
    (is (search "A fire has 2m: past it, tool calls are refused"
                (getf (first (cron-submits)) :prompt)))
    (nodecode-cron::tick (+ +cron-now+ 1800 600))
    (is (equal '("cron-slow") (mapcar #'nodecode-cron::fire-session nodecode-cron::*inflight*)))
    (cron-finish "cron-slow" "Done: 3 of 5 checked; the last 2 are left.")
    (nodecode-cron::tick (+ +cron-now+ 1800 601))
    (is (equal "ok" (getf (nodecode-cron::job-last job) :status)) "and it reports"))
  (with-cron-runtime (:timeout-minutes 5 :job (job "30m" "x" :name "plain"))
    (is (null (nodecode-cron::job-timeout job)) "the job names no limit")
    (is (equal '(:seconds 300) (nle::live-turn-budget (nle::open-live-turn "cron-plain" "t1")))))
  (is (null (nle::live-turn-budget (nle::open-live-turn "cron-slow" "t3")))))

(deftest cron-cell-a-duplicate-admission-is-recorded-not-retried ()
  (with-cron-runtime (:submits (:disposition :duplicate :turn-id nil) :job (job "30m" "x" :name "dup"))
    (is (= 0 (nodecode-cron::tick (+ +cron-now+ 1800))) "nothing started")
    (is (= 1 (length (cron-submits))) "the submit was made")
    (is (null nodecode-cron::*inflight*) "nothing in flight")
    (is (equal "duplicate" (getf (nodecode-cron::job-last job) :status)))
    (is (= (+ +cron-now+ 3600) (nodecode-cron::job-next-at job)))))

(deftest cron-cell-run-fires-now-beside-the-schedule ()
  (with-cron-runtime (:submits t :job (job "every day at 9am" "x" :name "manual"))
    (is (search "fired into session cron-manual (turn t1)" (cron:run "manual" "look at #42")))
    (let ((submit (first (cron-submits))))
      (is (search "Run context: look at #42" (getf submit :prompt)))
      (is (uiop:string-prefix-p "cron:manual:run:" (getf submit :command-id))))
    (is (= (cron-local 2026 9 5 9 0) (nodecode-cron::job-next-at job)))
    (is (= 1 (nodecode-cron::job-fires job)))))

(deftest cron-cell-pause-resume-edit-and-slash ()
  (with-cron-runtime (:submits t :job (job "30m" "x" :name "p"))
    (is (search "paused" (cron:pause "p")))
    (is (= 0 (nodecode-cron::tick (+ +cron-now+ 1800))) "a paused job does not fire")
    (is (search "resumed" (cron:resume "p")))
    (is (eq :active (nodecode-cron::job-state job)))
    (is (search "schedule: every 1h" (cron:edit "p" :schedule "every hour" :prompt "y")))
    (is (equal "y" (nodecode-cron::job-prompt job)))
    (is (signals-error cron:cron-error (cron:edit "p" :timeout-minutes -1)))
    (is (search "the jobs are p" (refusal-text cron:cron-error (cron:show "nope"))))
    (is (search (concatenate 'string "cron: 1 job" (string #\Newline) "- p — active, next")
                (cell-entry "nodecode-cron" "cron" "")))))

(deftest cron-cell-slash-summary-lists-every-job-and-state (with-cron-runtime (:store t))
  (cron-add "every day at 9am" "Summarize the inbox" :name "Inbox digest")
  (cron-add "30m" "Ping" :name "health")
  (cron:pause "health")
  (with-stubbed-fdefinitions ((get-universal-time () +cron-now+))
    (is (equal (concatenate 'string
                            "cron: 2 jobs" (string #\Newline)
                            "- inbox-digest — active, next 2026-09-05 09:00 (in 23h)"
                            (string #\Newline)
                            "- health — paused, never fired")
               (nodecode-cron::summary-text)))
    (dotimes (i 4)
      (cron-add "every 2h" "x" :name (format nil "j~d" i)))
    (is (search (concatenate 'string (string #\Newline) "+3 more (/cron jobs)")
                (nodecode-cron::summary-text)))))

(deftest cron-cell-slash-panel-lists-the-jobs-and-a-row-opens-its-record (with-cron-runtime (:store t))
  ;; Bare /cron answers the summary and the panel a shell opens over it: a row
  ;; per job in the summary's order, marked by how it stands, its Enter the
  ;; record and its Space a pause or a resume. The record ends in the rows that
  ;; run, resume or pause, and remove the job.
  (cron-add "every day at 9am"
            "Summarize the inbox: the five messages that matter most, who sent them, and what each asks of me"
            :name "Inbox digest")
  (cron-add "30m" "Ping" :name "health")
  (cron:pause "health")
  (multiple-value-bind (text panel) (nodecode-cron::run-slash "")
    (is (uiop:string-prefix-p "cron: 2 jobs" text) "the text is the summary")
    (let ((rows (getf panel :rows)))
      (is (equal "Cron (2)" (getf panel :title)))
      (is (equal "1 active · 1 paused" (getf panel :context)))
      ;; the ids padded to one width, so the columns after them line up
      (is (equal '("inbox-digest" "health      ") (mapcar (lambda (row) (getf row :label)) rows)))
      (is (equal '("●" "‖") (mapcar (lambda (row) (getf row :mark)) rows)))
      (is (equal '(:enter "/cron show inbox-digest" :space "/cron pause inbox-digest")
                 (getf (first rows) :run)))
      (is (equal "/cron resume health" (getf (getf (second rows) :run) :space)) "a paused job's Space resumes it")))
  (multiple-value-bind (text record) (nodecode-cron::run-slash "show inbox-digest")
    (is (uiop:string-prefix-p "job inbox-digest" text) "the text is the whole record")
    (let ((rows (getf record :rows)))
      (is (equal "inbox-digest · Inbox digest" (getf record :context)))
      (is-present (prompt (member "prompt  " rows :key (lambda (row) (getf row :label)) :test #'equal)) "a prompt row"
        (is (equal "Summarize the inbox: the five messages that matter most, who sent them,"
                   (getf (first prompt) :detail)))
        ;; and its words wrap onto the rows under it
        (is (equal '("        " "and what each asks of me")
                   (list (getf (second prompt) :label) (getf (second prompt) :detail)))))
      (is (equal '("/cron run inbox-digest" "/cron pause inbox-digest" "/cron remove inbox-digest")
                 (loop for row in rows
                       for line = (getf (getf row :run) :enter)
                       when line collect line)))))
  ;; a paused job's record resumes it, and names no next fire it will not make
  (let ((record (format nil "~s" (nth-value 1 (nodecode-cron::run-slash "show health")))))
    (is (search "/cron resume health" record))
    (is (search "\"- (paused)\"" record)))
  (is (signals-error cron:cron-error (nodecode-cron::run-slash "show nope"))))

(deftest cron-cell-a-job-session-cannot-schedule (with-cron-runtime (:submits t))
  (cron-add "30m" "x" :name "loop")
  (let ((nlk:*scribe-session-id* "cron-loop"))
    (is (signals-error cron:cron-error (cron:run "loop")) "run is refused")
    (is (signals-error cron:cron-error (cron:resume "loop")) "resume is refused")
    (is (search "cannot add" (refusal-text cron:cron-error (cron:add "30m" "z"))))
    (is (search "loop" (cron:jobs)) "reading is allowed"))
  (let ((nlk:*scribe-session-id* "s-operator"))
    (is (equal "s-operator" (nodecode-cron::job-origin (cron-add "30m" "fine" :name "other"))))))

;;; --- the report -------------------------------------------------------------------------------

(deftest cron-cell-answer-is-sent-to-the-origin-as-its-next-request ()
  (with-cron-runtime (:store t :submits t)
    (nlk:create-session :id "s-origin")
    (let ((job (cron-add "every 2h" "Summarize the inbox" :name "inbox" :origin "s-origin"))
          (due (cron-local 2026 9 4 12 0)))
      (is (equal "s-origin" (nodecode-cron::job-origin job)))
      (nodecode-cron::tick due)
      (is (= 1 (length (cron-submits))) "the fire is the only submit so far")
      (cron-finish "cron-inbox" (format nil "Three new threads.~%- a~%- b"))
      (nodecode-cron::tick (+ due 5))
      (is (null (nodecode-cron::job-report job)) "sent in the settling tick")
      (let ((request (first (cron-submits-to "s-origin"))))
        (is-present request "the report reached the ingress as a request into the origin"
          (is (equal "inbox" (getf request :job-id)))
          (is (equal (format nil "cron:inbox:report:~d"
                             (nodecode-cron::unix-from-universal (+ due 5)))
                     (getf request :command-id)))
          (is (equal (format nil "[cron \"inbox\" fired 2026-09-04 12:00, schedule every 2h; ~
                                      its session is cron-inbox]: Three new threads.~%- a~%- b")
                     (getf request :prompt)))))
      (is (= 1 (length (cron-submits-to "s-origin"))) "sent once")
      (is (equal "cron inbox: Three new threads." (cron-board-text "inbox")))
      ;; The next fire settles while the origin runs a turn: the ingress
      ;; queues the request behind it, and that too is delivered.
      (nodecode-cron::tick (+ due 7200))
      (cron-finish "cron-inbox" "Nothing new.")
      (with-captured-submits (:disposition :queued)
        (nodecode-cron::tick (+ due 7205))
        (is (null (nodecode-cron::job-report job)) "a queued request counts as sent")
        (is (search "its session is cron-inbox]: Nothing new."
                    (getf (first (cron-submits-to "s-origin")) :prompt)))))))

(deftest cron-cell-a-report-the-ingress-refuses-waits-on-the-row ()
  (with-cron-runtime (:store t :submits t)
    (nlk:create-session :id "s-origin")
    (let ((job (cron-add "30m" "x" :name "late" :origin "s-origin")))
      (nodecode-cron::tick (+ +cron-now+ 1800))
      (cron-finish "cron-late" "Nothing new.")
      (setf *cron-refuse-session* "s-origin")
      (nodecode-cron::tick (+ +cron-now+ 1805))
      (is (nodecode-cron::job-report job) "refused: the report waits")
      (is (null (cron-submits-to "s-origin")) "nothing reached the origin")
      (is (search "report: waiting to be sent to s-origin" (cron:show "late")))
      (is (<= (nodecode-cron::seconds-until-wake (+ +cron-now+ 1805)) 60))
      ;; The registry row carries the waiting report across a restart.
      (setf nodecode-cron::*jobs* '())
      (nodecode-cron::load-jobs)
      (let ((back (nodecode-cron::find-job "late")))
        (is-present back "the job came back"
          (is (equal "Nothing new." (getf (nodecode-cron::job-report back) :answer)))))
      (setf *cron-refuse-session* nil)
      (nodecode-cron::tick (+ +cron-now+ 1900))
      (is (null (nodecode-cron::job-report (nodecode-cron::find-job "late"))) "sent")
      (let ((request (first (cron-submits-to "s-origin"))))
        (is-present request "the request reached the origin"
          (is (search "[cron \"late\" fired 2026-09-04 10:30" (getf request :prompt)))
          (is (search "cron-late]: Nothing new." (getf request :prompt))))))))

(deftest cron-cell-report-for-a-gone-origin-is-dropped-and-said ()
  (with-cron-runtime (:store t :submits t)
    (let ((job (cron-add "30m" "x" :name "orphan" :origin "s-gone")))
      (nodecode-cron::tick (+ +cron-now+ 1800))
      (cron-finish "cron-orphan" "hello?")
      (nodecode-cron::tick (+ +cron-now+ 1805))
      (is (null (nodecode-cron::job-report job)) "dropped")
      (is (null (cron-submits-to "s-gone")) "nothing sent")
      (is (search "session s-gone that scheduled it is gone" (cron-board-text "orphan")))
      (is (equal "ok" (getf (nodecode-cron::job-last job) :status))))))

(deftest cron-cell-silent-answer-sends-nothing-to-the-origin ()
  (with-cron-runtime (:store t :submits t)
    (nlk:create-session :id "s-quiet")
    (let ((job (cron-add "30m" "x" :name "quiet" :origin "s-quiet")))
      (nodecode-cron::tick (+ +cron-now+ 1800))
      (cron-finish "cron-quiet" "SILENT")
      (nodecode-cron::tick (+ +cron-now+ 1805))
      (is (null (nodecode-cron::job-report job)))
      (is (null (cron-submits-to "s-quiet")) "nothing sent"))))

;;; --- the page's route -------------------------------------------------------------------

(deftest cron-cell-the-page-reads-and-moves-jobs-through-its-route (with-temp-gateway (port))
  (with-cron-runtime (:submits t :job (job "every day at 9am" "x" :name "page"))
    (is-route (port :get "/api/cron/jobs" :token nil) 401 "the operator's route")
    (with-gateway-http (port :get "/api/cron/jobs")
      (is-present (row (first (coerce (nlk:json-value body :array "jobs") 'list))) "the job"
        (is (equal "page" (nlk:json-value row :string "id")))
        (is (equal "every day at 9am" (nlk:json-value row :string "schedule" "display")))))
    (with-gateway-http (port :post "/api/cron/jobs?op=pause&id=page")
      (is (search "job page paused" (nlk:json-value body :string "text")) "the verb's own answer")
      (is (equal "paused" (nlk:json-value (aref (nlk:json-value body :array "jobs") 0) :string "state"))))
    (with-gateway-http (port :post "/api/cron/jobs?op=add&schedule=every%202h&prompt=Look%20around&name=second")
      (is (search "job second added" (nlk:json-value body :string "text")))
      (is (= 2 (length (nlk:json-value body :array "jobs")))))
    (with-gateway-http (port :post "/api/cron/jobs?op=remove&id=nope")
      (is (search "no job \"nope\"" (nlk:json-value body :string "error" "message")) "a refusal in the verb's words"))
    (with-gateway-http (port :post "/api/cron/jobs?op=remove&id=second")
      (is (= 1 (length (nlk:json-value body :array "jobs"))) "gone")))
  (is-route (port :get "/api/cron/jobs") 404 "stopped: the route is gone"))
