;;;; api.lisp --- the verbs, /import, POST /api/import, the watch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The verbs are plain functions in the NIK package, every one answering
;;;; a string, every refusal an IMPORT-ERROR the eval snippet renders as text:
;;;; (nik:worlds) lists what is on the box, (nik:scan) is the dry run's
;;;; report, (nik:import ...) applies. The slash is `/import [WORLD]
;;;; [--source PATH] [--only KINDS] [--overwrite] [--keep-running] [--yes]':
;;;; a preview unless --yes, the report posted as a notice, the summary as
;;;; the composer hint. Keys and tokens come with everything else; a
;;;; foreign gateway that polls bots is stopped and disabled so they answer
;;;; from here, unless --keep-running. Naming no world reads every world
;;;; the box carries and merges them.
;;;;
;;;; The setup wizard's import stage speaks to the same entrypoint by two
;;;; methods, `plan' and `apply', and reads the plan back as JSON; `scan'
;;;; is the third, the redacted machine-readable report that is worth
;;;; pasting into a bug about a home this build read wrong; `verify' asks
;;;; whether the target an import selected actually answers.
;;;;
;;;; /import is a builtin (tui/slash.lisp): a shell walks the import stage, a
;;;; room or the page gets RUN-SLASH's text. The gateway registers the route,
;;;; and starts the watch, as it
;;;; serves (SERVE-GATEWAY); the `import' config section is seams.lisp's.

(in-package #:nodecode.import)

;;; --- the verbs ----------------------------------------------------------------------------

(defun run-import (&key source overwrite (takeover t) only without apply progress (answer #'identity))
  "The plan for SOURCE, applied when APPLY — and then the watch started
for any bot it held for a running foreign gateway. => what ANSWER makes of
the plan, the plan itself unless told otherwise."
  ;; The import's nursery is the import's, as a load's is (NLK:LOAD-CELL): a
  ;; plan of keys conses tens of megabytes. A full import conses 400 MB and
  ;; peaks at 490 MB; without a 512 MB nursery, each collection would copy
  ;; every fact read so far. ANSWER runs inside it, so the plan is garbage
  ;; when the nursery comes back down.
  (nlk:with-nursery ((* 512 1024 1024))
    (let ((plan (make-plan :source source :overwrite overwrite :takeover takeover
                           :only only :without without)))
      (cond (apply
             (apply-plan plan :progress progress)
             (funcall *start-watch*))
            (t (write-report plan)))
      (funcall answer plan))))

(defun worlds (&aux (found (nlk:detect-agent-worlds)))
  "Every coding-agent home on this box, most recently used first, as one
line each: the world, where it sits, and when it was last used."
  ;; A stat probe — nothing is opened.
  (if (null found)
      "no coding-agent home found on this box"
      (format nil "~:{~a~30t~a~@[~60t~a~]~%~}"
              (loop for row in found
                    collect (list (nlk:agent-world-name row.world) (namestring row.root)
                                  (and row.last-used
                                       (nlk:iso-time (- row.last-used nlk:+unix-epoch+))))))))

(defun scan (&key source only)
  "What an import of SOURCE would do, as the report; nothing is written."
  ;; SOURCE names a world or a path; naming none reads every world on the box.
  (run-import :source source :only only :answer #'report-text))

(defun import (&key source overwrite (takeover t) only)
  "Import SOURCE, keys and tokens with it: OVERWRITE replaces what is
already here, ONLY names the kinds (providers, instructions, memory,
skills, mcp, channels, cron, sessions, profiles) comma-separated."
  ;; A foreign gateway polling bots is stopped and disabled so they answer
  ;; from here; TAKEOVER nil leaves it running, and its bots land off and come
  ;; on by themselves once it stops. Answers the report.
  (run-import :source source :overwrite overwrite :takeover takeover
              :only only :apply t :answer #'report-text))

;;; --- does the imported target answer ---------------------------------------------------
;;; A key an import carried over is a key that worked once, somewhere else.
;;; It may be revoked, out of credit or bound to an address that has since
;;; moved, and an operator told "imported" whose first message then fails
;;; has been told something that was not true. So before the walk says the
;;; provider is there, one side call asks it: the smallest completion the
;;; target will run, on the default target alone (no auxiliary override,
;;; no fallback), under a deadline. A refusal is reported in the provider's
;;; own words, with the key the check sent taken out of them — a provider
;;; sometimes quotes the key it refused.

(defvar *verify-target* 'verify-target
  "What checks that this organism's default target answers; a test stubs it.")

(defun target-secrets (&aux (key (ignore-errors (nle::effective-provider-config-api-key
                                                 (nle::snapshot-effective-provider-config)))))
  "The key this organism's default target sends, as a list, or NIL."
  ;; Frozen as the check's request freezes it, so a !command runs and its
  ;; output is the key. A target that does not freeze sends nothing; a
  ;; keyless one's `public' is no secret.
  (and (stringp key) (>= (length key) 8) (list key)))

(defun redact-secrets (text secrets &key (limit 160))
  "TEXT cut to LIMIT with each of SECRETS taken out of it, whole and in part:
every run of eight or more key characters that is a piece of one replaced."
  ;; The values the check sent, never a shape: twelve characters holding a
  ;; letter and a digit took the model id out too, `anthropic/[redacted]'.
  ;; A piece is out as well: a provider may quote a key's head.
  (flet ((piece-p (run) (some (lambda (secret) (cl:search run secret)) secrets)))
    (one-line-of (ppcre:regex-replace-all
                  "[\\w.~+/=-]{8,}"
                  (reduce (lambda (text secret)
                            (ppcre:regex-replace-all (ppcre:quote-meta-chars secret) text "[redacted]"))
                          secrets :initial-value text)
                  (lambda (run) (if (piece-p run) "[redacted]" run))
                  :simple-calls t)
                 limit)))

(defun verify-target (&aux (secrets (target-secrets)))
  "One tiny completion on this organism's default target."
  ;; => (values OK
  ;; REASON CODE STATUS): T, or NIL, the provider's own sentence (or the
  ;; transport fact, below HTTP) with the key taken out, the provider
  ;; failure's reason label (`connection_refused' when nothing listens at
  ;; the endpoint, which is no verdict on the key), or NIL, and the HTTP
  ;; status it answered, or NIL. The sentence, never the failure's report:
  ;; its `[provider model]' and its remedy would be said twice over
  ;; in the walk's closing line.
  (handler-case
      ;; The deadline covers the whole check, a cold endpoint's first token included.
      (sb-sys:with-deadline (:seconds 30)
        ;; One attempt: the walk waits 4 s for it, and a turn's stream retries
        ;; take a silent answer.
        (let ((nle::*auxiliary-model* nil)
              (nle::*max-stream-retries* 0))
          (nle:complete "Answer with the single word ok." "ok" :max-tokens 16 :fallback-p nil))
        (values t nil nil nil))
    ;; A reasoning model spends 16 tokens thinking and says nothing: the
    ;; endpoint took the key and answered 200, which is what is asked.
    (nle::provider-empty-response ()
      (values t nil nil nil))
    (nle::provider-error (condition)
      (values nil
              (redact-secrets (if (nle::provider-error-status condition)
                                  (nle::provider-said (nle::provider-error-detail condition))
                                  (nle::provider-error-detail condition))
                              secrets)
              (nle::provider-failure-reason condition)
              (nle::provider-error-status condition)))
    ((or error sb-sys:deadline-timeout) (condition)
      (values nil (redact-secrets (princ-to-string condition) secrets) nil nil))))

;;; --- /import -------------------------------------------------------------------------------

(defun run-slash (args &aux (words (nlk:split-words args))
                            (head (first words))
                            (source nil) (overwrite nil) (takeover t) (only nil) (yes nil))
  "The answer to one /import: the summary line and where the report file is,
said as a notice too; `--sources' lists the repositories held, `--remove
LABEL' takes one back."
  (cond ((equal head "--sources") (return-from run-slash (sources)))
        ((equal head "--remove")
         (return-from run-slash
           (remove-source (or (second words) (fail "--remove needs a source's label; /import --sources lists them"))))))
  (when (and head (not (uiop:string-prefix-p "--" head)))
    (pop words)
    (unless (or (nlk:find-agent-world head) (parse-source head))
      (fail "~a is not a world this build knows (~{~a~^, ~}) nor a repository (OWNER/REPO, an https URL); ~
             name a path with --source"
            head (nlk:agent-world-names)))
    (setf source head))
  (loop for word = (pop words) while word
        do (alexandria:switch (word :test #'string=)
             ("--source" (setf source (or (pop words) (fail "--source needs a PATH or a repository"))))
             ("--only" (setf only (or (pop words) (fail "--only needs KINDS, comma-separated"))))
             ("--overwrite" (setf overwrite t))
             ("--keep-running" (setf takeover nil))
             ("--yes" (setf yes t))
             ("--dry-run" (setf yes nil))
             (t (fail "unknown word ~a; usage /import [WORLD|OWNER/REPO[#REF]] [--source PATH] [--only KINDS] ~
                       [--overwrite] [--keep-running] [--yes], /import --sources, /import --remove LABEL"
                      word))))
  (let* ((plan (run-import :source source :overwrite overwrite :takeover takeover
                           :only only :apply yes))
         (report (and plan.report-path
                      (format nil "the report is in ~a"
                              (nlk:home-abbreviated (namestring plan.report-path))))))
    ;; The report is a file, and the notice and the hint say where: posted
    ;; whole, it was a toast cut at four rows that left after five seconds,
    ;; and a toast alone takes the path with it (ip-01).
    (nle:notice (if report
                    (format nil "import ~a~:[ (dry run)~;~]: ~a — ~a"
                            (plan-source-line plan) plan.applied (summary-line plan) report)
                    (report-text plan))
                :level :info)
    (if yes
        (format nil "import ~a: ~a~@[ — ~a~]" (plan-source-line plan) (summary-line plan) report)
        (format nil "import ~a (dry run): ~a — /import~@[ ~a~] --yes applies~@[; ~a~]"
                (plan-source-line plan) (summary-line plan) source report))))

(defun param-flag (params key)
  (eq (nlk:json-value params :any key) t))

(defun run-method (method params &key progress)
  "The wizard's methods over the params object {source, only, without,
overwrite, keep_running}, source a world name, a path or a list of world
names: `plan' previews, `apply' writes, `scan' answers the same plan with
nothing written — the pasteable report."
  ;; Nothing is posted: the wizard writes the answer's report into the
  ;; transcript of the shell that walks it. Posted, it was a toast cut at four
  ;; rows that left after five seconds (ip-01).
  (run-import :source (or (nlk:json-value params :text "source")
                          (coerce (nlk:json-value params :array "source") 'list))
              :only (nlk:json-value params :text "only")
              :without (nlk:json-value params :text "without")
              :overwrite (param-flag params "overwrite")
              :takeover (not (param-flag params "keep_running"))
              :apply (equal method "apply") :progress progress :answer #'plan-json))

(defparameter *stream-ping-seconds* 10
  "How often a streamed import says it is still working: well under the
shell's 30 s read, which a plan of a large home outlasted in silence (T-350).")

(defun stream-import (responder run)
  "Answer RESPONDER as server-sent events: RUN called with a progress
function whose every item is an `item' event, its value the closing `result'
event, and a comment every *STREAM-PING-SECONDS* while it works, which the
reader (NLK:READ-EVENTS) skips and its socket counts as a line."
  (let ((writer (funcall responder '(200 (:content-type "text/event-stream"
                                         :cache-control "no-cache"))))
        (lock (bt2:make-lock :name "import-stream"))
        (done (bt2:make-semaphore :name "import-stream-done"))
        ;; read here: the pinger's thread sees only the global value
        (seconds *stream-ping-seconds*)
        (open t))
    (labels ((send (text &optional last)
               ;; LAST shuts the stream in the same hold that writes it: a ping
               ;; whose wait ran out as RUN returned queues on the lock, and
               ;; would land after the result it must never follow.
               (bt2:with-lock-held (lock)
                 (when open
                   (funcall writer text)
                   (when last (setf open nil)))))
             (event (object &optional last)
               (send (format nil "data: ~a~%~%" (nlk:encode-json-object object)) last)))
      (let ((pinger (nlk:spawn "import-stream-ping"
                      (loop until (bt2:wait-on-semaphore done :timeout seconds)
                            ;; a peer gone mid-plan is the plan's to find, not this thread's
                            do (ignore-errors (send (format nil ": working~%~%")))))))
        (nlk:with-cleanup ((bt2:signal-semaphore done)
                           (bt2:join-thread pinger)
                           (bt2:with-lock-held (lock)
                             (setf open nil)
                             (funcall writer "" :close t)))
          (event (nlk:json-object
                  "type" "result" "result"
                  (funcall run (lambda (phase item)
                                 (event (nlk:json-object "type" "item" "phase" phase
                                                         "item" (item-json item))))))
                 t))))))

(defun import-route (env &aux (request (nle::gateway-request-json env))
                               (method (nlk:json-value request :string "method")))
  "/api/import: POST {method, source, only, without, overwrite, keep_running,
stream} runs the setup walk's METHOD in the image that serves the store --
`plan', `apply' and `scan' (RUN-METHOD), `verify' (one small request to the
default target) -- and answers its value; with stream true, the long three
answer as events (STREAM-IMPORT)."
  (cond ((equal method "verify")
         (handler-case (multiple-value-bind (ok reason code status) (funcall *verify-target*)
                         (nlk:json-object "ok" (if ok t :false)
                                          "provider" (or nle::*provider* :null)
                                          "model" (or nle::*model* :null)
                                          "reason" (or reason :null)
                                          "code" (or code :null)
                                          "status" (or status :null)))
           (error (condition)
             (nlk:json-object "ok" :false "reason" (redact-secrets (princ-to-string condition)
                                                                   (target-secrets))))))
        ((member method '("plan" "apply" "scan") :test #'equal)
         (flet ((run (&optional progress)
                  (handler-case (run-method method request :progress progress)
                    (error (condition)
                      (nlk:json-object "status_text" (format nil "import: ~a" condition)
                                       "error" (princ-to-string condition))))))
           (if (param-flag request "stream")
               ;; Live-only outcomes ride this response; the plan's report
               ;; remains the durable authority. No sync frame or poll cache.
               (lambda (responder) (stream-import responder #'run))
               (run))))
        (t (fail "no method ~s; plan, apply, scan or verify" method))))

;;; --- the entry ----------------------------------------------------------------------------

(defun stop-watch ()
  "The watch stopped, and waited for, bounded, to leave."
  (bt2:with-lock-held (*watch-lock*)
    (setf *watch* (nlk:worker-stop *watch*))))
