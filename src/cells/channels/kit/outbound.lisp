;;;; outbound.lisp --- request plans, chunking, retry, executors.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Port of the Zig-era channel kit's outbound.ts: one text chunker, one
;;;; HTTP request-plan shape, one retry/429/backoff execution loop, one
;;;; plan-recording fake executor. Adapters contribute request builders;
;;;; nothing protocol-specific lives here.
;;;;
;;;; Executors are the ONLY seam that performs platform I/O, and they run
;;;; exclusively on delivery-worker / poll threads — never inside a wsd
;;;; callback (package.lisp topology rule).

(in-package #:nodecode-channel-kit)

(defparameter +retry-attempts+ 3)
(defparameter +retry-max-ms+ 3000)

(nlk:define-record (request-plan (:copier nil) (:export :constructor :readers))
  "One planned platform HTTP request."
  ;; PATH joins onto the executor base URL unless absolute. BODY is a
  ;; JSON-encodable object or NIL. AUDIT-LABEL names the request in
  ;; status/warn output — never the URL, which may carry a token.
  (method "POST" :type string)
  (path (error "path required") :type string)
  (body nil)
  (headers '() :type list)
  (timeout-seconds 30 :type real)
  (retry-server-errors nil :type boolean)
  (audit-label "request" :type string))

(nlk:access (plan request-plan))

(defun rest-plan (method path label retry timeout-seconds &optional body headers)
  "The METHOD of BODY to PATH under HEADERS: 5xx and 429 retried when RETRY,
LABEL naming it in audits and failure copy."
  (make-request-plan :method method :path path :body body :headers headers
                     :timeout-seconds timeout-seconds
                     :retry-server-errors retry
                     :audit-label label))

(nlk:define-record (text-chunk (:copier nil) (:export :readers))
  (text "" :type string)
  (index 1 :type integer)
  (total 1 :type integer))

;;; A code fence is a pair of ``` runs — the first opens, the next closes,
;;; left to right, Discord's rule — and a message is its own document: a
;;; fence a chunk leaves open swallows nothing in the next message, where
;;; the next ``` opens instead of closing, and every later chunk renders code
;;; as prose and prose as code. So a chunk cut inside a fence closes it, and
;;; the next chunk opens it again under the same language.

(defparameter +fence+ "```")

(defparameter +fence-carry-floor+ 64
  "The smallest chunk limit a fence is carried across: below it, the lines
that close and reopen one would crowd out the text itself.")

(defun fence-language (text start &aux (end (position #\Newline text :start start)))
  "The language an opening fence names on its own line from START, or \"\"
— a word of at most twenty characters, the only thing Discord reads there."
  (let ((tag (subseq text start (or end start))))
    (if (and (<= 1 (length tag) 20)
             (every (lambda (ch) (or (alphanumericp ch) (find ch "+#-_."))) tag))
        tag
        "")))

(defun open-fence (text end &aux (open nil))
  "(values OPENER LANGUAGE): where the fence open at position END of TEXT
starts and the language it names, or NIL when END stands outside every
fence."
  (loop for at = (search +fence+ text :end2 end)
          then (search +fence+ text :start2 (+ at (length +fence+)) :end2 end)
        while at
        do (setf open (if open nil at)))
  (and open (values open (fence-language text (+ open (length +fence+))))))

(defun chunk-from (text start limit reopen &aux (size (length text)))
  "(values PIECE CUT REOPEN): the chunk of TEXT that starts at START and fits
LIMIT behind REOPEN — the fence line the previous chunk left open — the
position the next chunk starts from, and the fence line it opens with."
  ;; A cut is a full window's last line break, else its last space or tab,
  ;; else the window's end — so a word, a link or a code token is never broken
  ;; across two messages, and a remainder shorter than the window rides one
  ;; chunk whole; only a run longer than the window itself, holding no break
  ;; at all, is cut mid-run. A break on the window's first character would
  ;; hand over a one-character message: not a break.
  (flet ((cut-within (room &aux (end (min size (+ start (max 1 room)))))
           (if (>= end size)
               size
               (let ((line (position #\Newline text :start start :end end :from-end t))
                     (space (position-if (lambda (ch) (member ch '(#\Space #\Tab))) text
                                         :start start :end end :from-end t)))
                 (cond ((and line (> line start)) (1+ line))
                       ((and space (> space start)) (1+ space))
                       (t end))))))
    (let* ((room (- limit (length reopen)))
           (cut (cut-within room))
           (carry (>= limit +fence-carry-floor+))
           (opener nil)
           (language ""))
      (when (and carry (< cut size) (open-fence text cut))
        ;; Cut inside a fence. A block opened in this chunk that fits one
        ;; message whole — or holds nothing yet — moves to the next; any
        ;; other cut closes the fence, and the closing line takes its room.
        (let* ((at (open-fence text cut))
               (close (search +fence+ text :start2 (+ at (length +fence+))))
               (block-end (if close (+ close (length +fence+)) size))
               (first-line (or (position #\Newline text :start at) size)))
          (if (and (> at start)
                   (or (<= (- block-end at) limit)
                       (every (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\Return)))
                              (subseq text (min first-line cut) cut))))
              (setf cut at)
              (progn
                (setf cut (cut-within (- room 4)))
                (multiple-value-setq (opener language) (open-fence text cut))))))
      (when (and (< cut size) (null opener))
        ;; Cut inside a table: one opened in this chunk that fits one message
        ;; whole moves to the next, as a fence block does. Its tail alone is
        ;; pipe rows with no header and no rule -- nothing a platform draws.
        (multiple-value-bind (from to) (table-span-at text cut)
          (when (and from
                     (> from start)
                     (<= (- to from) room)
                     (find-if-not (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\Return)))
                                  text :start start :end from))
            (setf cut from))))
      (let ((piece (subseq text start cut)))
        (values (concatenate 'string reopen piece
                             (cond ((null opener) "")
                                   ((uiop:string-suffix-p piece (string #\Newline)) +fence+)
                                   (t (format nil "~%~a" +fence+))))
                cut
                (if opener (format nil "~a~a~%" +fence+ language) ""))))))

(defun split-text-chunks (text limit)
  "Split trimmed TEXT into code-point-bounded chunks of at most LIMIT
characters; 1-based index/total."
  ;; Every boundary is counted before the cut (CHUNK-FROM), and a fence the
  ;; cut lands inside closes on this chunk and opens again on the next.
  ;; Refuses empty text — an empty outbound message is a caller bug, not a
  ;; delivery.
  (let ((trimmed (nlk:trimmed text))
        (limit (max 1 (truncate limit))))
    (when (zerop (length trimmed))
      (error "outbound message text must not be empty"))
    (let* ((pieces (loop with reopen = ""
                         for start = 0 then cut
                         while (< start (length trimmed))
                         for (piece cut next) = (multiple-value-list
                                                 (chunk-from trimmed start limit reopen))
                         collect piece
                         do (setf reopen next)))
           (total (length pieces)))
      (loop for piece in pieces
            for index from 1
            collect (make-text-chunk :text piece :index index :total total)))))

;;; --- execution results ----------------------------------------------------

(nlk:define-record (execution (:copier nil) (:predicate nil) (:export :constructor :readers))
  (ok-p nil :type boolean)
  (status 0 :type integer)
  (attempts 0 :type integer)
  (body nil)
  (error nil :type (or null string))
  (retry-delays '() :type list))

(nlk:access (result execution))

(defun probe-failure (result)
  "Why RESULT did not answer: its error text, already redacted, else the status."
  (or result.error
      (format nil "status ~a" result.status)))

(defun redact-text (text prefixes)
  "TEXT with every occurrence of a secret in PREFIXES replaced."
  ;; Applied to every failure detail before it can reach a warn line or status
  ;; plist.
  (uiop:frob-substrings text
                        (remove-if-not (lambda (secret)
                                         (and (stringp secret) (plusp (length secret))))
                                       prefixes)
                        "[redacted]"))

(defun execute-with-retry (plan attempt-fn &key retry-after-fn
                                                failure-message-fn
                                                redact-prefixes
                                                (sleep-fn (lambda (delay-ms)
                                                            (sleep (/ delay-ms 1000.0)))))
  "The one retry loop."
  ;; ATTEMPT-FN takes no arguments and returns
  ;; (values STATUS BODY HEADERS TRANSPORT-ERROR); a non-NIL TRANSPORT-ERROR
  ;; ends execution immediately. 429/5xx retries only when the plan opts in,
  ;; delayed by RETRY-AFTER-FN (protocol-derived, e.g. Telegram
  ;; parameters.retry_after, called with STATUS BODY HEADERS), else the
  ;; retry-after header, else linear backoff capped at +RETRY-MAX-MS+.
  (let ((delays '()))
    (flet ((done (attempt ok-p status body &optional error)
             (make-execution :ok-p ok-p :status status :attempts attempt :body body
                             :error error :retry-delays (reverse delays))))
      (loop for attempt from 1 to +retry-attempts+
            do (multiple-value-bind (status body headers transport-error)
                   (funcall attempt-fn)
                 (when transport-error
                   (return (done attempt nil 0 nil (redact-text transport-error redact-prefixes))))
                 (let ((retryable (and plan.retry-server-errors
                                       (or (>= status 500) (= status 429)))))
                   (cond
                     ((and retryable (< attempt +retry-attempts+))
                      (let* ((header (and (hash-table-p headers)
                                          (gethash "retry-after" headers)))
                             (seconds (if (stringp header)
                                          (parse-integer header :junk-allowed t)
                                          header))
                             (delay (min (or (and retry-after-fn
                                                  (funcall retry-after-fn status
                                                           body headers))
                                             (and (integerp seconds) (* seconds 1000))
                                             (* 250 attempt))
                                         +retry-max-ms+)))
                        (push delay delays)
                        (funcall sleep-fn delay)))
                     ((<= 200 status 299)
                      (return (done attempt t status body)))
                     (t
                      (let ((detail (redact-text (cond ((stringp body) body)
                                                       ((hash-table-p body)
                                                        (handler-case (nlk:encode-json-object body)
                                                          (error () (princ-to-string body))))
                                                       (t (princ-to-string body)))
                                                 redact-prefixes)))
                        (return (done attempt nil status body
                                      (if failure-message-fn
                                          (funcall failure-message-fn plan
                                                   status detail)
                                          (format nil "~a ~a failed with ~
                                                       status ~a: ~a"
                                                  plan.method
                                                  plan.audit-label
                                                  status detail)))))))))))))

(defun multipart-body-p (body)
  "True when BODY is a dexador-style multipart alist (name . value)*."
  ;; JSON request bodies are hash-tables; file uploads are this alist.
  (and (consp body)
       (every (lambda (pair)
                (and (consp pair) (stringp (car pair))))
              body)))

;;; --- plists for the model -------------------------------------------------
;;; The eval snippet prints a tool's value with ~S, and a hash table prints as
;;; #<HASH-TABLE ...>: a decoded JSON body is opaque to the model that asked
;;; for it. A keyword plist prints as what it is, (:ID "1" :AUTHOR (:ID "2")),
;;; and reads back the same way. The converters are the waist's now
;;; (NLK:JSON-PLIST / NLK:PLIST-JSON, imported and re-exported by the
;;; package): CONFIG-GET and CONFIG-SET spell an object the same way, so one
;;; shape crosses the eval seam in both directions for every surface.

;;; --- executors ------------------------------------------------------------

(nlk:define-record (plan-executor (:copier nil) (:export :constructor))
  (run (error "run required") :type function))

(nlk:define-record (recording-executor (:include plan-executor) (:copier nil)
                                       (:constructor %make-recording-executor) (:export plans))
  "Test executor: records every plan, answers scripted responses in order,
never sleeps, never touches the network."
  (plans '() :type list)
  (responses '() :type list))

(nlk:access (executor recording-executor))

(defun execute-plan (executor plan)
  (funcall (plan-executor-run executor) plan))

(defun make-scripted-response (status &optional body headers)
  (list status body headers))

(defun make-recording-executor (&key responses retry-after-fn &aux executor
                                                                    (lock (bt2:make-lock :name "recording-executor")))
  "A RECORDING-EXECUTOR answering RESPONSES (MAKE-SCRIPTED-RESPONSE values)
in order, then 200 {\"ok\": true} forever."
  ;; Sleeps are skipped, so a retry-schedule test asserts
  ;; EXECUTION-RETRY-DELAYS instead of waiting. A host's delivery pool runs
  ;; plans on several threads at once: the record and the script are taken
  ;; under one lock, or two plans at the same instant keep one of them.
  (setf executor
        (%make-recording-executor
         :responses (copy-list responses)
         :run (lambda (plan)
                (bt2:with-lock-held (lock)
                  (setf executor.plans (append executor.plans (list plan))))
                (execute-with-retry
                 plan
                 (lambda ()
                   (destructuring-bind (&optional (status 200) body headers)
                       (or (bt2:with-lock-held (lock) (pop executor.responses))
                           (list 200 (nlk:json-object "ok" t) nil))
                     (values status body headers nil)))
                 :retry-after-fn retry-after-fn
                 :sleep-fn (lambda (delay) (declare (ignore delay))))))))

(defun plan-url (plan base-url &aux (path plan.path))
  (if (or (uiop:string-prefix-p "http://" path)
          (uiop:string-prefix-p "https://" path))
      path
      (format nil "~a~a~a"
              (string-right-trim "/" (or base-url ""))
              (if (uiop:string-prefix-p "/" path) "" "/")
              path)))

(defun parse-json-body (body)
  "BODY decoded when it is a JSON object or array — a hash table, a vector
(a message list, a guild's channels) — and its text otherwise: a bare
scalar, a page that is not JSON, a 204's blank."
  ;; Octets decode as UTF-8 first. One shape for both containers, so a caller
  ;; never meets an array as a string it must parse itself.
  (nlk:if-let (text (typecase body
                      (string body)
                      ((vector (unsigned-byte 8))
                       (ignore-errors (sb-ext:octets-to-string body)))))
    (nlk:with-handlers ((error () text))
      (let ((parsed (nlk:decode-json text)))
        (if (or (hash-table-p parsed)
                (and (vectorp parsed) (not (stringp parsed))))
            parsed
            text)))
    body))

(defun make-dexador-executor (&key base-url headers redact-prefixes
                                   failure-message-fn retry-after-fn)
  "The live executor: one dexador request per attempt."
  ;; HEADERS (an alist) merge under the plan's own. Secrets in REDACT-PREFIXES
  ;; never reach an error string; the base URL itself may carry one
  ;; (Telegram), so failure copy names the audit label, never the URL.
  ;;
  ;; A request-plan body that is a multipart alist (string . value)* is sent
  ;; as form-data (Discord file attachments). Any other non-NIL body is JSON.
  (make-plan-executor
   :run
   (lambda (plan)
     (execute-with-retry
      plan
      (lambda ()
        (handler-case
            (let* ((body plan.body)
                   (multipart (multipart-body-p body)))
              (multiple-value-bind (resp-body status response-headers)
                  (nlk:http (intern (string-upcase plan.method) :keyword)
                            (plan-url plan base-url)
                            :headers (append '(("accept" . "application/json"))
                                             headers
                                             plan.headers
                                             (when (and body (not multipart))
                                               '(("content-type" . "application/json"))))
                            :content (cond (multipart body)
                                           (body (nlk:encode-json-object body)))
                            :timeout plan.timeout-seconds :pool t)
                (values status (parse-json-body resp-body) response-headers nil)))
          (error (condition)
            (values 0 nil nil (princ-to-string condition)))))
      :retry-after-fn retry-after-fn
      :failure-message-fn failure-message-fn
      :redact-prefixes redact-prefixes))))

(defun execute-delivery (plans executor &aux (results '()))
  "Execute PLANS in order, stopping at the first failure."
  ;; (values DELIVERED-P RESULTS ERROR).
  (dolist (plan plans (values t (nreverse results) nil))
    (let ((result (execute-plan executor plan)))
      (push result results)
      (unless result.ok-p (return (values nil (nreverse results) result.error))))))

;;; --- one call, in the model's shape ----------------------------------------

(defun request-body (body)
  "BODY as the plan carries it: a keyword plist becomes a JSON object, an
NLK:JSON-OBJECT hash table and a multipart alist pass through, NIL is no
body."
  ;; Anything else is refused here, before a request is built — the executor
  ;; would otherwise encode a bare string as a JSON string literal.
  (cond ((null body) nil)
        ((hash-table-p body) body)
        ((multipart-body-p body) body)
        ((and (consp body) (keywordp (car body))) (plist-json body))
        (t (error "request body must be a keyword plist, an nlk:json-object, ~
                   or a multipart alist ((\"name\" . value) ...); got ~s"
                  (type-of body)))))

(defun call-answer (result)
  "One execution as the plist the eval snippet prints."
  (append (list :status result.status
                :body (json-plist result.body))
          (and result.error
               (list :error result.error))))

(defun call (executor method path &key body headers (timeout 30) (retry t)
                                       read)
  "One METHOD PATH request through EXECUTOR, answered as a plist the eval
snippet prints legibly: (:status N :body VALUE) plus :error TEXT when the
call did not succeed."
  ;; VALUE is the decoded JSON as a keyword plist (arrays as vectors, null as
  ;; :NULL), or the raw text when the body was not JSON. RETRY opts into the
  ;; executor's 429/5xx retries. Blocks on the caller's thread through the
  ;; retry loop.
  ;;
  ;; READ is the path of the object this call changes. A write the platform
  ;; ACCEPTED WITHOUT ANSWERING A BODY — a 204 on a reaction, a pin, a role —
  ;; is followed by one GET of that path, and that read's whole answer rides
  ;; back under :STATE in this same shape, so a read that itself failed says
  ;; so instead of going quietly missing. NIL reads nothing back; the read
  ;; inherits RETRY and TIMEOUT and carries none of the write's headers.
  ;;
  ;; The split is the point. :STATUS is what the platform accepted, :STATE is
  ;; what it holds afterwards. A 2xx on a write proves a request was accepted
  ;; for SOME target; only the read proves the effect, and proves it on the
  ;; object the path names — so a call aimed at the wrong id reports the wrong
  ;; object's state rather than a clean success.
  (let* ((method (string-upcase (string method)))
         (plan (rest-plan method path "request" (and retry t) timeout (request-body body) headers))
         (result (execute-plan executor plan))
         (answer-body result.body)
         (answer (call-answer result)))
    ;; Read back only a write accepted with no body (a 204 leaves a blank string).
    (if (and read
             (not (member method '("GET" "HEAD") :test #'string=))
             (<= 200 result.status 299)
             (or (null answer-body)
                 (and (stringp answer-body)
                      (zerop (length (nlk:trimmed answer-body))))))
        (append answer
                (list :state (call executor "GET" read
                                   :timeout timeout :retry retry)))
        answer)))
