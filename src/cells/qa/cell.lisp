;;;; cell.lisp --- the section, the tool, the primer, /qa, the sender.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What leaves this machine, and when, in one place. Sharing is answered
;;;; once — in the setup wizard's panel, or by hand in the config — and never
;;;; asked again: qa.share is `weekly' or `never', and until it is
;;;; either, the cell runs its local half only (the tool, the notes, the
;;;; slash command) and sends nothing. NODECODE_QA=0 in the
;;;; environment turns every send off whatever the config says.
;;;;
;;;; The sender is one worker thread that looks when the period has passed:
;;;; a page folded at that moment, posted once, the cursor moved past it on
;;;; 2xx; a refusal keeps the nonce and looks again in an hour. There is no
;;;; queue, no backoff ladder and no identity that follows the install: the
;;;; nonce is minted per page and the collector forgets it in two weeks.
;;;;
;;;; Config, a top-level key:
;;;;   "qa": {"share": "weekly", "url": "https://preview.nodecode.ai"}

(in-package #:nodecode-qa)

(defparameter +tool-name+ "report_issue")
(defparameter +usage+ "/qa [show | send | notes | push | clear]")
(defparameter +period-seconds+ (* 7 24 60 60)
  "How often a page goes: once a week.")
(defparameter +retry-seconds+ (* 60 60)
  "How long a refused page waits before the next try.")
(defparameter +post-timeout+ 20
  "Seconds one post may take, connect and read alike.")

(defvar *worker* nil
  "The sender thread while it runs, or NIL.")
(defvar *last-error* nil
  "Why the last send was refused, or NIL.")
(defvar *operator* nil
  "Bound true while /qa runs: the one path notes ship on.")

;;; --- the tool -----------------------------------------------------------------

(nlk:define-contract issue-report
  (:doc "Report a tool that misbehaved: it timed out, refused something it should have done, answered the wrong shape, or lacked something you needed. The note stays on this machine for the operator (/qa notes); only a count per tool and symptom may leave, weekly, when the operator shares. Once per problem, then carry on.")
  ("tool" :text :required t
          :doc "the tool that misbehaved, as you called it")
  ("symptom" :text :required t :options +symptoms+
             :doc "what went wrong")
  ("note" :text :default ""
          :doc "one sentence, at most 280 characters: what you asked and what came back"))

;;; The declaration above is the whole of report_issue's shape: the schema the
;;; provider is shown is projected from it, the value this tool's body runs on
;;; is materialized from it, and a call that does not satisfy it is refused
;;; before the body. It replaced a hand-written JSON-schema object beside a
;;; body that read the arguments again and disagreed with it: the schema said
;;; `required: tool, symptom' and the body defaulted both, so a call with
;;; neither was filed under "unknown"/"other" instead of being repaired. The
;;; enum the schema always advertised is enforced for the same reason.

(defun report-issue (report)
  "The report_issue tool: keep the note REPORT carries, answer `noted'."
  ;; REPORT is an ISSUE-REPORT, materialized by the boundary from the one
  ;; declaration — so nothing here re-reads a wire key, re-checks a symptom
  ;; against the enum its own schema advertises, or invents a value for a
  ;; member the model left out. Never an error, never a turn's failure: a note
  ;; that cannot be kept is still noted.
  (nlk:with-handlers ((error () "noted"))
    (keep-note (issue-report-tool report)
               (issue-report-symptom report)
               (issue-report-note report))
    "noted, thanks: kept on this machine for the operator (/qa notes)"))

;;; --- the collector ------------------------------------------------------------

(defun collector-root (&aux (url (string-right-trim "/" nle::*release-url*)))
  "Where pages go unless the section says otherwise: the release host,
beside whose /dl path the collector's route sits, as the release feed's does."
  (if (uiop:string-suffix-p url "/dl")
      (subseq url 0 (- (length url) 3))
      url))

(defun page-url ()
  (format nil "~a/api/qa" (setting :url)))

(defvar *post* 'post-json
  "The POST the collector is asked with, a function of URL and BODY answering
true on 2xx, else (values NIL REASON); a test binds it.")

(defun collector-reason (status answer)
  "The refusal as /qa prints it: STATUS and the sentence the collector's
ANSWER names, `HTTP 400: rounds.0.ttft_ms: Invalid input'."
  (format nil "HTTP ~d~@[: ~a~]" status
          (ignore-errors (nlk:json-value (nlk:decode-json (nlk:body-text answer)) :text "error"))))

(defun post-json (url body)
  "POST BODY (JSON text) to URL => T on 2xx, else (values NIL REASON)."
  ;; Not NLE:HTTP-FETCH: its refusal drops the body, and the collector's 400
  ;; names the first thing wrong for the operator to read. One deadline over
  ;; the whole exchange, as there.
  (handler-case
      (multiple-value-bind (answer status)
          (sb-sys:with-deadline (:seconds +post-timeout+)
            (nlk:http :post url :content body :timeout +post-timeout+
                                :connect-timeout +post-timeout+
                                :headers '(("content-type" . "application/json"))))
        (if (< status 300) t (values nil (collector-reason status answer))))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil (nle:transport-failure-label condition url)))))

;;; --- sending ------------------------------------------------------------------

(defun env-off-p (&optional (value (uiop:getenv "NODECODE_QA")))
  "Whether NODECODE_QA vetoes every send: 0, off, never or false."
  ;; T or NIL, never MEMBER's tail: a settings member is a boolean, and a test
  ;; that stands in for the environment answers one.
  (and (member (string-downcase (or value "")) '("0" "off" "never" "false")
               :test #'string=)
       t))

(defparameter +unanswered+
  "not answered yet: choose weekly or never in /settings, Diagnostics, or set qa.share in config.jsonc"
  "What /qa and /qa send say before qa.share has an answer: the surface that
asks it.")

(defun written-share ()
  "qa.share as config.jsonc says it now, or NIL: unanswered there too, or a
document this process cannot read."
  ;; The cell runs on what it read at its start (SETTING); this is the one
  ;; read of the document as written, so /qa can say an answer given since —
  ;; /settings saves it and leaves the restart to the operator — is waiting.
  (ignore-errors
   (nlk:config-string (nlk:section-table (nle:read-shared-config) '("qa")) "share")))

(defun pending-answer (&aux (running (setting :share)) (written (written-share)))
  "When config.jsonc answers qa.share otherwise than this cell read at its
start: the sentence that says so and how it applies, else NIL."
  (when (and written (not (equal written running)))
    (format nil "qa.share is ~a in config.jsonc, but this cell started ~:[unanswered~;on ~:*~a~]: ~
                 restart nodecode, or ask for (restart-cells \"nodecode-qa\"), to apply it"
            written running)))

(defun send ()
  "Send the page now instead of at the week's end: fold the page since the
cursor and post it; on 2xx the cursor moves past it."
  ;; The nonce is written before the post, so a crash between the two resends
  ;; the same page under the same nonce and the collector keeps one. => the
  ;; sentence the verb answers; a refusal is QA-ERROR.
  ;; An answer written since the start refuses too, whichever way it went:
  ;; a never the operator chose is never overridden by the weekly this
  ;; cell started on, and a weekly waits for the restart that applies it.
  (cond ((setting :off-by-env)
         (fail "NODECODE_QA turns sharing off in this environment"))
        ((pending-answer)
         (fail "~a" (pending-answer)))
        ((equal (setting :share) "never")
         (fail "not sharing: qa.share is never"))
        ((null (setting :share))
         (fail "~a" +unanswered+)))
  (unless (nlk:store-open-p)
    (fail "no store is open"))
  (with-qa-lock
    (nlk:bind ((cursor (ensure-cursor)) (head (log-head))
               (nonce (or (nlk:json-value cursor :text "nonce") (fresh-nonce))) (url (page-url))
               ((page offset) (page cursor head nonce)) (body (nlk:encode-json-object page)))
      (setf (gethash "nonce" cursor) nonce)
      (write-row +cursor-key+ cursor)
      (multiple-value-bind (ok reason) (funcall *post* url body)
        (cond (ok
               (write-row +cursor-key+
                          (nlk:json-object "position" head
                                           "since" (gethash "until" page)
                                           "last_sent" (gethash "until" page)
                                           "stdio_offset" offset))
               (setf *last-error* nil)
               (format nil "sent the page, ~d bytes, to ~a" (length body) url))
              (t
               (setf *last-error* (or reason "refused"))
               (fail "the collector did not take the page: ~a" *last-error*)))))))

(defun seconds-until-due (&optional (now (get-universal-time)))
  "Seconds until the next page is owed: a period after the last one went, 0
once it has passed, 0 with no cursor (the first look begins one)."
  (nlk:if-let (last (nlk:iso-universal (nlk:json-value (cursor) :text "last_sent")))
    (max 0 (- (+ last +period-seconds+) now)) 0))

(defun look ()
  "The sender's step: begin the cursor at the first look, send when the
period has passed, remember a refusal quietly."
  (when (and *qa* (nlk:store-open-p))
    (ensure-cursor)
    (when (zerop (seconds-until-due))
      (handler-case (send)
        (error (condition)
          (setf *last-error* (princ-to-string condition)))))))

(defun wake-seconds (&aux (due (seconds-until-due)))
  "When the sender looks next: at the due moment, an hour after a refusal,
never sooner than a minute."
  (if (and (zerop due) *last-error*)
      +retry-seconds+
      (max 60 due)))

;;; --- the verbs: every one a string --------------------------------------------

(define-verb status-line ()
  "One line: whether pages go, when the next is due, what is kept here."
  (let* ((share (setting :share))
         (since (nlk:json-value (cursor) :text "since"))
         (notes (read-notes)))
    (format nil "qa: ~a~@[ · counting since ~a~] · ~d note~:p kept here~@[, ~d unpushed~]"
            (cond ((setting :off-by-env) "off by NODECODE_QA")
                  ((pending-answer))
                  ((equal share "weekly")
                   (format nil "sharing weekly with ~a, next page in ~a~@[, last refused: ~a~]"
                           (setting :url) (nlk:duration-text (seconds-until-due))
                           *last-error*))
                  ((equal share "never") "not sharing (share: never)")
                  (t +unanswered+))
            (and since (subseq since 0 (min 10 (length since))))
            (length notes)
            (let ((unpushed (count-if-not #'pushed-p notes)))
              (and (plusp unpushed) unpushed)))))

(define-verb show ()
  "The next page exactly as it would go: the kept nonce or a fresh one, the
same fold. Answered whatever share says — it is the transparency."
  (unless (nlk:store-open-p)
    (fail "no store is open"))
  (let* ((cursor (ensure-cursor))
         (nonce (or (nlk:json-value cursor :text "nonce") (fresh-nonce)))
         (body (nlk:encode-json-object (page cursor (log-head) nonce))))
    (format nil "qa: the next page, ~d bytes, as it would go to ~a (the nonce is minted at the send)~%~a"
            (length body) (page-url) body)))

(defun note-line (note)
  (nlk:with-json ((at :text "at") (tool :text "tool") (symptom :text "symptom")
                  (text :string "note") (model :text "model"))
      note
    (format nil "  ~a  ~a  ~a ~a~@[ (~a)~]: ~a"
            (subseq (or at "") 0 (min 16 (length (or at ""))))
            (if (pushed-p note) "pushed" "kept  ")
            (or tool "?") (or symptom "other") model (or text ""))))

(define-verb notes ()
  "The notes the model left, newest first: what /qa push would send."
  (let ((notes (read-notes)))
    (if (null notes)
        "qa: no notes kept"
        (format nil "qa: ~d note~:p kept on this machine, ~d unpushed~%~{~a~^~%~}"
                (length notes) (count-if-not #'pushed-p notes)
                (mapcar #'note-line notes)))))

(defun push-notes ()
  "Ship the unpushed notes' text, the operator's explicit act: never from a
turn, never on the weekly page."
  ;; Tool, symptom, note, provider and model ride; no session, turn, time or
  ;; id does.
  (unless *operator*
    (fail "notes leave only by /qa push, the operator's own act"))
  (when (setting :off-by-env)
    (fail "NODECODE_QA turns sharing off in this environment"))
  (let* ((notes (or (remove-if #'pushed-p (read-notes))
                    (fail "no unpushed notes: a note is what the model files with report_issue ~
                           when one of its tools misbehaves, and none is waiting; /qa notes lists ~
                           what is kept")))
         (url (format nil "~a/api/qa/notes" (setting :url)))
         (body (nlk:encode-json-object
                (nlk:json-object
                 "report" +notes-kind+
                 "version" (nle::effective-version)
                 "platform" (nle::release-platform)
                 "notes" (map 'vector
                              (lambda (note)
                                (nlk:with-json ((tool :text "tool") (symptom :text "symptom")
                                                (text :string "note")
                                                (provider :text "provider") (model :text "model"))
                                    note
                                  (nlk:json-object "tool" (or tool "?")
                                                   "symptom" (or symptom "other")
                                                   "note" (or text "")
                                                   :opt "provider" provider
                                                   :opt "model" model)))
                              notes)))))
    (multiple-value-bind (ok reason) (funcall *post* url body)
      (unless ok
        (fail "the collector did not take the notes: ~a" (or reason "refused")))
      (with-qa-lock
        (write-notes (mapcar (lambda (note)
                               (unless (pushed-p note)
                                 (setf (gethash "pushed" note) t))
                               note)
                             (read-notes))))
      (format nil "pushed ~d note~:p to ~a" (length notes) url))))

(define-verb clear-notes ()
  "Drop every kept note."
  (with-qa-lock
    (format nil "cleared ~d note~:p" (prog1 (length (read-notes)) (write-notes '())))))

;;; --- /qa ---------------------------------------------------------------

(defun run-slash (args session-id)
  "The answer to one /qa invocation."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words args))
         (head (first words))
         (*operator* t))
    (nlk:with-handlers ((qa-error (condition)
                          (format nil "qa: ~a" condition)))
      (cond ((null head) (status-line))
            ((string= head "show") (show))
            ((string= head "send") (send))
            ((string= head "notes") (notes))
            ((string= head "push") (push-notes))
            ((string= head "clear") (clear-notes))
            (t (format nil "qa: unknown subcommand ~a; usage ~a" head +usage+))))))

;;; --- the entry: the one declaration -------------------------------------------

(defun start-sender ()
  "The one thread, primed: its first look begins the cursor when there is
none, and sends only when a period has passed since the last page."
  ;; The declaration's scope owns it — ON-STOP joins it on the way down.
  (with-qa-lock
    (unless *worker*
      (setf *worker* (nlk:worker-start "qa" #'look :wake #'wake-seconds :prime t))
      (nle:on-stop (lambda ()
                     (with-qa-lock
                       (setf *worker* (nlk:worker-stop *worker*))))))))

(nle:define-cell qa
  (:section ("qa")
    (:guide "Once a week the organism can send nodecode.ai one page of counts: the build and platform, requests and failures per provider and model, calls and errors per tool, crashes; never a prompt, a path, a file, a hostname or an id that follows this install; /qa show prints the next page exactly as it would go; share weekly sends it, never keeps everything on this machine; the choice is asked once, and NODECODE_QA=0 turns sending off whatever it says")
    ("share" :choice :required t :options '("weekly" "never")
             :doc "weekly sends one page of counts to nodecode.ai; never keeps it all here")
    ("url" :string :default (collector-root)
           :doc "the collector; the release host by default, a mirror of your own here"))
  ;; Two members the section cannot carry: a url written here is a web
  ;; address, with its trailing slash off before anything formats with it,
  ;; and the environment switch is not config at all. The default is the
  ;; release host's, whatever the install came from.
  (:settings (lambda (values table &aux (url (string-right-trim "/" (getf values :url))))
               (unless (or (null (nlk:config-string table "url"))
                           (some (lambda (scheme) (and (uiop:string-prefix-p scheme url)
                                                       (> (length url) (length scheme))))
                                 '("https://" "http://")))
                 (nlk:config-error "qa.url is ~s; the collector is a web address, http:// or https://"
                                   (getf values :url)))
               (setf (getf values :url) url)
               (list* :off-by-env (env-off-p) values)))
  (:tool +tool-name+ (nlk:contract 'issue-report) 'report-issue)
  (:command "qa" 'run-slash
            :description "What leaves this machine: the next page, the notes, the switch"
            :argument-hint "show | send | notes | push | clear")
  (:start (lambda ()
            (setf *last-error* nil)
            ;; A page may go at all only when sharing is weekly and the
            ;; environment has not vetoed it.
            (when (and (equal (setting :share) "weekly")
                       (not (setting :off-by-env)))
              (start-sender)))))
