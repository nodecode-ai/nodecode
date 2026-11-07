;;;; page.lisp --- the two rows, the page, the notes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here is instrumentation. The engine already journals every
;;;; provider round (turn.usage, turn.provider_request), every retry and
;;;; failover, every tool result and every turn's end into history.db; the
;;;; page is a fold over those facts after a cursor — counts per provider
;;;; and model, per tool, per failure class — and the fold runs at the send,
;;;; so nothing is queued and nothing can be lost: a page the collector
;;;; refused is folded again next time, from the same cursor, under the
;;;; same nonce.
;;;;
;;;; Two state rows on one pseudo session (the cron registry's precedent: a
;;;; state row needs no session.created fact):
;;;;
;;;;   qa/cursor  {"position", "since", "last_sent", "nonce"?, "stdio_offset"}
;;;;   qa/notes   {"notes": [{"id", "at", "tool", "symptom", "note", ...}]}
;;;;
;;;; The cursor begins at the consent moment: the first page covers what came
;;;; after the operator said yes, never what came before. A note is what the
;;;; model left through report_issue; it stays here, the page carries only
;;;; its tool and symptom as a count, and /qa push is the one way its
;;;; text leaves.

(in-package #:nodecode-qa)

(defparameter +state-session+ "qa"
  "The session id the cell's rows live on.")
(defparameter +cursor-key+ "cursor")
(defparameter +notes-key+ "notes")
(defparameter +notes-kept+ 200
  "How many notes are kept; the oldest past it is dropped.")
(defparameter +note-length+ 280
  "The longest note kept, in characters.")
(defparameter +page-kind+ "nodecode/weekly/1"
  "What a page says it is; the collector reads the shape off this.")
(defparameter +notes-kind+ "nodecode/notes/1"
  "What a by-hand push of notes says it is.")
(defparameter +symptoms+
  '("timeout" "wrong_output" "refused" "schema_mismatch" "missing_capability" "other")
  "What the model may say went wrong; anything else reads as other.")

;;; --- the rows -----------------------------------------------------------------

(defun read-row (key)
  "The row under KEY as decoded JSON, or NIL: absent, or no store open."
  (let ((text (and (nlk:store-open-p) (nlk:session-state-get +state-session+ key))))
    (and text (ignore-errors (nlk:decode-json text)))))

(defun write-row (key object)
  (nlk:session-state-put +state-session+ key (nlk:encode-json-object object))
  object)

(defun log-head ()
  "The newest log position in the store, 0 in an empty one."
  (or (nlk:events :as :value :columns '("max(log_position)")) 0))

(defun cursor ()
  (read-row +cursor-key+))

(defun begin-cursor ()
  "The consent moment, written once: the pages start at the log's head now,
and the first one is due a period from here. Nothing before it is folded."
  (write-row +cursor-key+
             (nlk:json-object "position" (log-head)
                              "since" (nlk:iso-now)
                              "last_sent" (nlk:iso-now)
                              "stdio_offset" (stdio-log-length))))

(defun ensure-cursor ()
  (or (cursor) (begin-cursor)))

(defun fresh-nonce ()
  "Sixteen random bytes as hex: a page's only identity."
  ;; The collector tells two deliveries of one page apart by it and nothing
  ;; else.
  (format nil "~{~(~2,'0x~)~}" (coerce (nlk:random-bytes 16) 'list)))

;;; --- the notes ----------------------------------------------------------------

(defun read-notes ()
  "Every kept note, newest first."
  (coerce (nlk:json-array (read-row +notes-key+) "notes") 'list))

(defun write-notes (notes)
  (write-row +notes-key+ (nlk:json-object "notes" (coerce notes 'vector)))
  notes)

(defun keep-note (tool symptom note)
  "Keep one note the model left: newest first, the oldest past +NOTES-KEPT+
dropped. The running turn names the session, the turn and the model."
  (with-qa-lock
    (nlk:bind ((turn (nle:turn)) (session (getf turn :session-id))
               ((provider model) (if session
                                     (ignore-errors (nlk:session-model-selection session))
                                     (values nil nil)))
               (all (cons (nlk:json-object "id" (nlk:make-durable-id "note")
                                           "at" (nlk:iso-now)
                                           :opt "session" session
                                           :opt "turn" (getf turn :turn-id)
                                           :opt "provider" provider
                                           :opt "model" model
                                           "tool" tool
                                           "symptom" symptom
                                           "note" (nlk:clip note +note-length+ :ellipsis ""))
                          (read-notes))))
      (write-notes (if (> (length all) +notes-kept+)
                       (subseq all 0 +notes-kept+)
                       all))
      (first all))))

(defun pushed-p (note)
  (nlk:json-value note :boolean "pushed"))

;;; --- the fold -----------------------------------------------------------------

(nlk:define-record (lane (:copier nil) (:predicate nil))
  "One provider and model's window."
  (requests 0) (retries 0) (fallbacks 0)
  (input 0) (output 0) (cached 0)
  (ttfts '()))

(nlk:define-record (tally (:copier nil) (:predicate nil))
  "One tool's window."
  (calls 0) (errors 0) (durations '()))

(defun thousands (n)
  "N to the nearest thousand: a page says how much, never exactly."
  (* 1000 (round n 1000)))

(defun median (values)
  "The middle of VALUES, or NIL when there are none."
  (when values
    (let ((sorted (sort (copy-list values) #'<)))
      (nth (floor (length sorted) 2) sorted))))

(defun condition-class (type &aux (text (or type "unknown"))
                                  (colon (position #\: text :from-end t)))
  "A condition type's name as a class: the package dropped, downcased."
  (string-downcase (if colon (subseq text (1+ colon)) text)))

(defun sorted-keys (table)
  "TABLE's keys in a stable order: strings by STRING<, (a . b) pairs by both."
  (sort (loop for key being the hash-keys of table collect key)
        (lambda (a b)
          (if (consp a)
              (or (string< (car a) (car b))
                  (and (string= (car a) (car b)) (string< (cdr a) (cdr b))))
              (string< a b)))))

(defun counts-object (table)
  "TABLE (string -> integer) as a JSON object, keys sorted."
  (apply #'nlk:make-json-object
         (loop for key in (sorted-keys table) append (list key (gethash key table)))))

(defun fold (after until since &aux (lanes (make-hash-table :test #'equal))
                                    (tools (make-hash-table :test #'equal))
                                    (failed (make-hash-table :test #'equal))
                                    (issues (make-hash-table :test #'equal))
                                    (before (1+ until)))
  "=> (values ROUNDS TURNS TOOLS ISSUES) over the facts after AFTER up to
UNTIL (log positions), counts only: per provider and model the rounds,
retries, failovers, tokens to the thousand and the median first-token time;
the turns that ended and how; per tool the calls, the errors and the median
duration; the notes since SINCE by tool and symptom."
  ;; No text from any fact is copied.
  (macrolet ((each ((source &rest specs) &body body)
               `(dolist (fact ,source) (nlk:with-json ,specs fact ,@body))))
    (flet ((lane-of (provider model)
             (alexandria:ensure-gethash (cons (or provider "") (or model "")) lanes (make-lane)))
           (tally-of (name)
             (alexandria:ensure-gethash name tools (make-tally)))
           (facts-of (kind)
             (nlk:events :kind kind :after after :before before :as :payloads))
           (count-of (kind)
             (nlk:events :kind kind :after after :before before :as :count)))
      (each ((facts-of "turn.usage") (provider :text "provider") (model :text "model")
             (input :integer "input-tokens") (output :integer "output-tokens")
             (cached :integer "cached-input-tokens"))
        (let ((lane (lane-of provider model)))
          (incf (lane-requests lane))
          (incf (lane-input lane) (or input 0))
          (incf (lane-output lane) (or output 0))
          (incf (lane-cached lane) (or cached 0))))
      (each ((facts-of "turn.provider_request") (provider :text "provider") (model :text "model")
             (ttft :integer "ttft-ms"))
        ;; A negative wait is a broken measurement, not a time: the page
        ;; leaves it out, and the collector refuses a page that carries one.
        (when (and ttft (not (minusp ttft)))
          (push ttft (lane-ttfts (lane-of provider model)))))
      (each ((facts-of "turn.provider_retry") (provider :text "provider") (model :text "model"))
        (incf (lane-retries (lane-of provider model))))
      (each ((facts-of "turn.provider_fallback")
             (provider :text "from-provider") (model :text "from-model"))
        (incf (lane-fallbacks (lane-of provider model))))
      (each ((facts-of "turn.failed") (type :text "condition-type"))
        (incf (gethash (condition-class type) failed 0)))
      (each ((facts-of "turn.tool_result") (name :text "tool-name") (result :string "result")
             (duration :integer "duration-ms"))
        (let ((tally (tally-of (or name ""))))
          (incf (tally-calls tally))
          (when (and result (uiop:string-prefix-p "ERROR:" result))
            (incf (tally-errors tally)))
          (when duration
            (push duration (tally-durations tally)))))
      (each ((read-notes) (at :text "at") (tool :text "tool") (symptom :text "symptom"))
        (when (and at (string>= at since))
          (incf (gethash (cons (or tool "") (or symptom "other")) issues 0))))
      (values
       (map 'vector (lambda (key &aux (lane (gethash key lanes)))
                      (nlk:json-object "provider" (car key)
                                       "model" (cdr key)
                                       "requests" lane.requests
                                       "retries" lane.retries
                                       "fallbacks" lane.fallbacks
                                       "input_tokens" (thousands lane.input)
                                       "output_tokens" (thousands lane.output)
                                       "cached_tokens" (thousands lane.cached)
                                       :opt "ttft_ms" (median lane.ttfts)))
            (sorted-keys lanes))
       (nlk:json-object "completed" (count-of "turn.completed")
                        "cancelled" (count-of "turn.cancelled")
                        "failed" (counts-object failed))
       (map 'vector (lambda (name &aux (tally (gethash name tools)))
                      (nlk:json-object "tool" name
                                       "calls" tally.calls
                                       "errors" tally.errors
                                       :opt "duration_ms" (median tally.durations)))
            (sorted-keys tools))
       (map 'vector (lambda (key)
                      (nlk:json-object "tool" (car key)
                                       "symptom" (cdr key)
                                       "count" (gethash key issues)))
            (sorted-keys issues))))))

;;; --- crashes: the stdio log's marker lines ------------------------------------
;;; The disabled debugger's last report goes to <store>.stdio.log, one
;;; `Unhandled <TYPE> in thread ...' line ahead of the backtrace. The page
;;; counts those lines per class after the byte offset the cursor keeps; a
;;; log that shrank (rotated, removed) reads from its start again.

(defun stdio-log-path ()
  (and (nlk:store-open-p)
       (nle::stdio-log-path (nlk:store-path nlk::*store*))))

(defun stdio-log-length (&aux (path (stdio-log-path)))
  (or (and path (nlk:file-bytes path)) 0))

(defun crash-lines (offset &aux (table (make-hash-table :test #'equal))
                                (path (stdio-log-path)))
  "=> (values TABLE END): the marker lines' condition classes counted after
byte OFFSET, and the byte the next read starts at."
  (if (not (and path (probe-file path)))
      (values table 0)
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        (let* ((end (file-length stream))
               (start (if (<= offset end) offset 0))
               (bytes (make-array (- end start) :element-type '(unsigned-byte 8))))
          (file-position stream start)
          (read-sequence bytes stream)
          (dolist (line (nlk:lines (sb-ext:octets-to-string
                                    bytes :external-format '(:utf-8 :replacement #\?))))
            (when (uiop:string-prefix-p "Unhandled " line)
              (let* ((from (length "Unhandled "))
                     (to (or (search " in thread" line :start2 from) (length line))))
                (incf (gethash (condition-class (subseq line from to)) table 0)))))
          (values table end)))))

;;; --- the page -----------------------------------------------------------------

(defun page (cursor head nonce &aux (since (or (nlk:json-value cursor :text "since") ""))
                                    (after (or (nlk:json-value cursor :integer "position") 0))
                                    (offset (or (nlk:json-value cursor :integer "stdio_offset") 0)))
  "=> (values PAGE STDIO-END): the whole page — what it is, its NONCE, the
build and the platform, the window from CURSOR's start to now — around the
fold up to HEAD and the crash counts; and the stdio log byte the cursor
moves to when the page is taken."
  (multiple-value-bind (crashes end) (crash-lines offset)
    (multiple-value-bind (rounds turns tools issues) (fold after head since)
      (values (nlk:json-object "report" +page-kind+
                               "nonce" nonce
                               "version" (nle::effective-version)
                               "platform" (nle::release-platform)
                               "since" since
                               "until" (nlk:iso-now)
                               "rounds" rounds
                               "turns" turns
                               "tools" tools
                               "issues" issues
                               "crashes" (counts-object crashes))
              end))))
