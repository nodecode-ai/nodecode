;;;; recall.lisp --- the model-facing history read (the PRO-LONG contract).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The event log is lossless and eviction only moves a visibility floor
;;;; (context.lisp): what leaves the window never leaves the log. This file is
;;;; the read the eviction stub advertises — the lisp analog of the Zig
;;;; v_history + `nodecode db` recall contract, collapsed to two functions
;;;; because eval already is the programmatic-read tool the Zig side had
;;;; to route through a CLI.
;;;;
;;;; Scan large, return small: RECALL-LOG matches SQL-side and returns clipped
;;;; one-line rows, because its output prints straight back into the very
;;;; window eviction just freed; RECALL-ENTRY returns one body whole. No index
;;;; and no relevance layer on purpose — the measured LIKE scan is per-call
;;;; cheap, and a growing interactive log is grep-shaped.
;;;;
;;;; Every row is stamped :at with the envelope's OCCURRED-AT (UTC ISO-8601,
;;;; millisecond precision) and a tool result carries its measured
;;;; :duration-ms, so per-tool timing is recoverable at this read — the
;;;; harness never has to answer "seq-tagged but not timestamped" with raw
;;;; SQL.
;;;;
;;;; Kernel on purpose: the stub is compiled into provider context by the
;;;; evolved engine, and a road sign must keep pointing at something that
;;;; survives a bad mutation.

(in-package #:nodecode.kernel)

(define-errors (event-log-error) ((:export))
  (recall-unscoped () "recall-log has no session scope: no ambient session is bound ~
                       (evaluate inside a turn) and no :session-id was passed")
  (recall-query-empty () "recall-search needs a query: at least one token to look for"))

(defparameter *recall-kinds*
  (list +kind-input-committed+ +kind-assistant-completed+
        +kind-tool-call-started+ +kind-tool-result+)
  "The conversational kinds RECALL-LOG scans by default: operator input,
assistant answers, and the tool traffic between them. Pass :KINDS to reach
any other durable kind.")

;;; Recall never hands the model its own thinking back. Thinking leaves the
;;; window whole and stays in the log for the operator's reads; what the model
;;; can recall is what it said and did -- its answers, its calls, their results
;;; -- on every provider.
(defparameter +recall-match-sql+
  "payload like ? and json_remove(payload, '$.message.reasoning_content',
     '$.message.reasoning_blocks', '$.message.reasoning_redacted_data') like ?"
  "RECALL-LOG's MATCH over an event row: the raw payload as a cheap
prefilter, then the payload without the message's thinking, so a match
answers only what a row can show.")

(defun recall-row (event &key clip payload-thunk)
  "One row: (:seq N :at ISO :kind K [:turn T] [:role R | :tool T]
[:duration-ms MS] [:text ...]) -- never the message's thinking."
  ;; :AT is the envelope's OCCURRED-AT — UTC ISO-8601 with milliseconds — so
  ;; wall time between any two entries is recoverable by subtraction; a tool
  ;; result additionally carries its measured :DURATION-MS when the fact
  ;; recorded one. CLIP, an integer, cuts each body to that many characters
  ;; and flattens it to one line; NIL returns the bodies whole. PAYLOAD-THUNK
  ;; is the whole-entry read's fallback: when supplied and EVENT is of a kind
  ;; with no conversational body, its value rides under :payload instead -- a
  ;; message that says nothing (an answer of tool calls alone) is not one, or
  ;; its raw payload would carry the thinking the row leaves out.
  (flet ((body (text)
           (if clip
               ;; clipped before flattened: the per-row bound must hold against a
               ;; megabyte tool result; the cut is disclosed
               (substitute-if #\Space (lambda (ch) (member ch '(#\Newline #\Return #\Tab)))
                              (clip text clip :ellipsis "…"))
               text)))
    ;; Who produced EVENT — (:role "user") for messages, (:tool "eval") for
    ;; tool traffic — and its conversational body, verbatim. TEXT is NIL (not
    ;; empty) for kinds that carry no such body, so `nothing to say' stays
    ;; apart from `says nothing'.
    (multiple-value-bind (speaker-key speaker text)
        (typecase event
          (message-event (values :role (message-event-role event)
                                 (message-event-content event)))
          (tool-call-event (values :tool (tool-call-event-tool-name event)
                                   (tool-call-event-arguments event)))
          (tool-result-event (values :tool (tool-result-event-tool-name event)
                                     (tool-result-event-result event))))
      (nconc (present-plist :seq event.log-position :at event.occurred-at
                            :kind event.kind :turn event.turn-id)
             (when speaker-key (list speaker-key speaker))
             (present-plist :duration-ms (and (typep event 'tool-result-event)
                                              (tool-result-event-duration-ms event))
                            :text (and text (plusp (length text)) (body text))
                            :payload (and payload-thunk (null speaker-key)
                                          (funcall payload-thunk)))))))

(defun recall-log (&key match kinds before after (limit 20) (clip 200)
                     (session-id *scribe-session-id*))
  "Scan the durable event log and return clipped rows, oldest first."
  ;;
  ;; Each row is tagged :seq and stamped :at (the envelope's UTC wall time), so
  ;; timing questions — what ran, when, for how long — are answerable from the
  ;; scan; tool results carry their measured :duration-ms. MATCH is a SQL LIKE
  ;; fragment tested against the payload less its thinking (+RECALL-MATCH-SQL+)
  ;; — ASCII case-insensitive, % and _ are wildcards. KINDS narrows to a wire kind or a list of them (default
  ;; *RECALL-KINDS*, the conversational set). BEFORE/AFTER bound by :seq — the
  ;; same numbers eviction stubs cite. SESSION-ID defaults to
  ;; the ambient session of the executing turn; pass one explicitly for
  ;; cross-session recall. The scan reads the events the session composes —
  ;; its own, and each anchored ancestor's up to the fork's anchor
  ;; (SESSION-CHAIN-SCOPE) — because that is what its window was made of: a
  ;; Discord lane's evicted history is its room's, and a scan of the lane
  ;; alone answered its one turn. It reads them whole, including turns an undo
  ;; has hidden from the live transcript: recall is the memory that survives
  ;; navigation.
  ;;
  ;; When more rows match than LIMIT, the newest LIMIT rows are returned and
  ;; the first list element is a marker (:matched N :showing M) — page older
  ;; with :before set to the oldest returned :seq. Each row's bodies are
  ;; clipped to CLIP characters; RECALL-ENTRY on a row's :seq returns the
  ;; whole body.
  (unless session-id (error 'recall-unscoped))
  (check-type limit (integer 1))
  (let ((kinds (if (listp kinds) (or kinds *recall-kinds*) (list kinds))))
    (with-store-handle (handle)
      (let* ((scope (session-chain-scope session-id))
             (where (if match
                        (let ((pattern (concatenate 'string "%" match "%")))
                          (list* (concatenate 'string (car scope) " and " +recall-match-sql+)
                                 (append (cdr scope) (list pattern pattern))))
                        scope))
             (query (list :kind kinds :where where :index +session-kind-index+
                          :after after :before before))
             (matched (apply #'events :as :count query))
             (rows (mapcar (lambda (event) (recall-row event :clip clip))
                           (nreverse (apply #'events :order :newest :limit limit query)))))
        (if (> matched (length rows))
            (cons (list :matched matched :showing (length rows)) rows)
            rows)))))

(defun recall-entry (seq)
  "The one event at :SEQ, its body whole."
  ;; Rows shaped like RECALL-LOG's with an unclipped :text; kinds with no
  ;; conversational body return their raw payload JSON under :payload
  ;; instead. NIL when no event holds SEQ.
  (check-type seq (integer 1))
  (when-let (event (events :log-position seq :as :instance))
    (recall-row
     event
     :payload-thunk
     (lambda ()
       (events :log-position seq :as :value :columns '("payload"))))))

;;; --- the whole-log conversation search ------------------------------------
;;; RECALL-LOG answers one session and the ancestors it forked from;
;;; RECALL-SEARCH asks the log as a whole —
;;; which is the shape a search box wants, every session one surface. Same
;;; read, same bound (scan large, return small), with two differences: the
;;; match runs over an event's conversational BODY, never its whole payload
;;; (a raw-payload LIKE also matched the envelope's own keys, so searching
;;; `role' answered every message ever logged), and each row carries the
;;; snippet window around its match, the session it belongs to, and the :seq
;;; its block pins at — enough for a shell to navigate to the exact block.

;;; One expression, so a search's match and its snippet are the same text by
;;; construction. Shapes this store never writes — a JSON null, a content that
;;; is neither — read as the empty string, never as the string "null".
(defparameter +event-body-text-sql+
  "case
     when json_type(payload, '$.message') = 'text' then
       json_extract(payload, '$.message')
     when json_type(payload, '$.message') = 'object' then
       case when json_type(payload, '$.message.content') = 'array' then
         (select group_concat(coalesce(json_extract(part.value, '$.text'), ''), '')
            from json_each(json_extract(payload, '$.message.content')) as part
           where json_extract(part.value, '$.type') = 'text')
       else coalesce(json_extract(payload, '$.message.content'), '') end
     when json_type(payload, '$.arguments') = 'text' then
       json_extract(payload, '$.arguments')
     when json_type(payload, '$.result') = 'text' then
       json_extract(payload, '$.result')
     else ''
   end"
  "The conversational body of an event row, as SQL over `events`: what the
operator said, what the answer carried (a content string, or the text parts
of a content vector), a tool call's arguments, a tool result's body.")

(defparameter +conversation-prefilter-min-prefix+ 3
  "Shortest JSON-safe prefix a token needs before it earns a payload
prefilter: below it the raw scan costs more than the body extraction it
saves, and the scan is short anyway.")

(defun like-literal (text)
  "TEXT as a SQL LIKE pattern body that matches TEXT itself: `%', `_' and
the escape character are escaped, everything else rides verbatim."
  ;; Every pattern built from this is used with ESCAPE '\', so a search for
  ;; `100%' is a search for those characters, not for every row in the log.
  (ppcre:regex-replace-all "[\\\\%_]" text "\\\\\\&"))

(defun conversation-prefilter-prefix (token)
  "The prefix of TOKEN a JSON-encoded payload is guaranteed to carry
verbatim: everything before the first character JSON escapes (`\"', `\\',
a control character — non-ASCII rides through unescaped)."
  ;; NIL when that run is shorter than +CONVERSATION-PREFILTER-MIN-PREFIX+, or
  ;; when TOKEN opens on an escaped character. A prefilter that could drop a
  ;; real match is not one, so the run is taken before the first character the
  ;; payload may spell differently, never reconstructed from around it.
  (let ((end (or (position-if (lambda (character)
                                (or (char< character #\Space)
                                    (member character '(#\" #\\))))
                              token)
                 (length token))))
    (and (>= end +conversation-prefilter-min-prefix+)
         (subseq token 0 end))))

(defun recall-search (query &key (kinds *recall-kinds*) (limit 60)
                            (window 220) (lead 32))
  "Grep the whole log: every session's conversational traffic at once."
  ;; QUERY is split on whitespace and every token must appear in the event's
  ;; conversational body (+EVENT-BODY-TEXT-SQL+), ASCII case-insensitively.
  ;; The tokens are literal — `%', `_' and `\' search for themselves. KINDS is
  ;; RECALL-LOG's conversational set by default; narrower or wider, any
  ;; durable kind whose payload carries a body works. An empty QUERY is
  ;; RECALL-QUERY-EMPTY, never an everything-match.
  ;;
  ;; Rows come newest first, at most LIMIT of them, in RECALL-LOG's own row
  ;; vocabulary plus the session and the block's identity:
  ;;
  ;;   (:session-id S :title T :seq N :at ISO :kind K :turn-id T
  ;;    [:role R | :tool T] :text S)
  ;;
  ;; :SEQ is the log position of the fact the row's block was built from — the
  ;; position the shell's fold stamps the transcript row it made from that
  ;; fact — so a caller that can show a transcript can pin its viewport on
  ;; exactly this block, and :TITLE is what the session directory calls the
  ;; session the block belongs to (the same column the listing reads). :TEXT
  ;; is the WINDOW characters of body around the first token (LEAD of them
  ;; before it, an ellipsis when the cut starts mid-body), never the body
  ;; whole: one page of rows must not pull a megabyte of tool output back, and
  ;; a snippet is what a search row shows.
  ;;
  ;; The scan is a LIKE over every payload; the one prefilter that matters is
  ;; the raw payload carrying the token's JSON-safe prefix before the body
  ;; expression is evaluated at all (CONVERSATION-PREFILTER-PREFIX), which is
  ;; what keeps a rare query near a tenth of a second on a six-figure log
  ;; instead of ten times that. No index, on purpose: the growing log is
  ;; grep-shaped, and this read runs under the store's own lock like every
  ;; other.
  (check-type limit (integer 1))
  (let ((tokens (remove "" (uiop:split-string
                            query :separator '(#\Space #\Tab #\Newline #\Return))
                        :test #'string=)))
    (unless tokens (error 'recall-query-empty))
    (let* ((body +event-body-text-sql+)
           (anchor (first tokens))
           ;; (COLUMN TOKEN), each a LIKE: the raw payload prefilters, then the body matches
           (predicates (append (loop for token in tokens
                                     for prefix = (conversation-prefilter-prefix token)
                                     when prefix collect (list "payload" prefix))
                               (loop for token in tokens collect (list body token))))
           (snippet (format nil
                            "case when instr(lower(~a), lower(?)) > ~d ~
                               then '…' || substr(~a, instr(lower(~a), lower(?)) - ~d, ~d) ~
                               else substr(~a, 1, ~d) end"
                            body (1+ lead) body body lead window
                            body (+ lead window))))
      (loop for (session-id seq turn-id kind role tool at text title)
              in (events :kind kinds
                         :where (cons (format nil "~{~a like ? escape '\\'~^ and ~}"
                                              (mapcar #'first predicates))
                                      (loop for (nil token) in predicates
                                            collect (format nil "%~a%" (like-literal token))))
                         :columns (list "session_id" "log_position" "turn_id" "kind"
                                        "nullif(json_extract(payload, '$.role'), '')"
                                        "nullif(json_extract(payload, '$.tool-name'), '')"
                                        "occurred_at"
                                        snippet
                                        ;; The directory's own title rule, per row:
                                        ;; one authority for what a session is called,
                                        ;; and no second read to name the sessions a
                                        ;; page of matches landed in.
                                        (format nil "nullif(~a, '')" +session-title-column+))
                         :column-arguments (list anchor anchor)
                         :order :newest
                         :limit limit
                         :as :rows)
            collect (present-plist :session-id session-id :seq seq :at at :kind kind
                                   :turn-id turn-id :title title :role role :tool tool
                                   :text (or text ""))))))
