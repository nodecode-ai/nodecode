;;;; state.lisp --- durable session-state KV + harness sections. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The src/db/session_state.zig distillation: an event-sourced KV with a
;;;; materialized fold, scoped by (session, ws, key). The event log is
;;;; append-only — a delete is an event whose value_json is NULL — and the
;;;; fold row's VER is the id of the last event that wrote it, so VER doubles
;;;; as the CAS token (monotonic, globally unique). FOLD-SESSION-STATE-TO-
;;;; WATERMARK rewinds the fold to any gateway log position, which is what
;;;; makes the KV checkpoint-compatible.
;;;;
;;;; Deliberate deviation from Zig, documented at its seam:
;;;;   - The 256KiB value bound is ENFORCED. Zig declares max_value_bytes and
;;;;     never checks it; carrying a dead constant forward would be cargo
;;;;     cult, and an unbounded value in a fold row is a real hazard. The
;;;;     16MiB per-session bound stays undeclared here — Zig never enforces
;;;;     it either, and checking it costs a full-session scan per write.
;;;;
;;;; The lisp organism has no V8 workspace lane, so the pin/fork/merge
;;;; surface does not exist here; WS stays in the schema (default "main") so
;;;; the shape matches Zig row-for-row.

(in-package #:nodecode.kernel)

;;; --- constants (session_state.zig:22-55) ----------------------------------

(defparameter +max-state-keys-per-ws+ 512
  "Per-(session, ws) live-key quota, enforced on new keys only.")

(defparameter +max-state-value-bytes+ (* 256 1024)
  "Bound on one value_json. Declared-but-dead in Zig; enforced here.")

(defparameter +max-harness-bytes+ (* 64 1024)
  "Total text budget for a session's harness sections, checked at the write
seam and re-checked as a floor at read time.")

(defparameter +harness-key-prefix+ "nc:harness:"
  "Session-scoped standing system context, injected every provider round.")

;;; --- conditions ------------------------------------------------------------

(define-error session-state-error (error) ((detail :initform ""))
  (:export)
  (:report "session state: ~a" detail))

(define-condition session-state-quota-exceeded (session-state-error) ()
  (:default-initargs :detail "session state quota exceeded"))

(define-condition state-value-too-large (session-state-error) ()
  (:default-initargs :detail "state value exceeds the 256KiB bound"))

(define-condition harness-budget-exceeded (session-state-error) ()
  (:default-initargs :detail "harness budget exceeded"))

(define-condition invalid-harness-key (session-state-error) ()
  (:default-initargs
   :detail "invalid harness key: must be non-empty and contain no ':'"))

;;; --- schema (schema.zig:178-225) --------------------------------------------

(defparameter *session-state-schema*
  '("create table if not exists session_state_events (
       id integer primary key autoincrement,
       session_id text not null,
       ws text not null default 'main',
       key text not null,
       value_json text,
       log_seq integer not null default 0,
       occurred_at text not null,
       environment_hash text,
       parent_snapshot_seq integer)"
    "create index if not exists idx_session_state_events_session_log
       on session_state_events (session_id, log_seq asc)"
    "create index if not exists idx_session_state_events_key
       on session_state_events (session_id, ws, key, id asc)"
    "create table if not exists session_state (
       session_id text not null,
       ws text not null default 'main',
       key text not null,
       ver integer not null default 0,
       value_json text,
       updated_log_seq integer not null default 0,
       updated_at text not null,
       primary key (session_id, ws, key))"
    ;; Learned context-window evidence per (provider, model). Plain state,
    ;; deliberately not event-sourced: no event, no replay arm. FLOOR is measured
    ;; truth (a round the server processed), CEILING is estimate-grade (our
    ;; token estimate of a request the server rejected as too large); the
    ;; budget belief in provider.lisp resolves floor over ceiling on conflict.
    "create table if not exists context_window_observations (
       provider_id text not null,
       model_id text not null,
       floor_tokens integer,
       ceiling_tokens integer,
       updated_at text not null,
       primary key (provider_id, model_id))"
    ;; The learned-lane rows (lane_observations) are not kept: the wire a
    ;; model rides is the operator's declaration (provider.lisp
    ;; CONFIGURED-PROVIDER-LANE). A store that carries them sheds the table.
    "drop table if exists lane_observations"
    ;; The per-(provider, model) control preferences went on: a
    ;; /think pick in one session moved every session on that target, the
    ;; scheduled jobs with them. A pick is the session's own now, carried on
    ;; its model selection fact (identity.lisp). A store that carried them
    ;; sheds the table.
    "drop table if exists model_control_preferences")
  "The two KV tables plus the plain context-window rows.")

;; Events.lisp assigns *SCHEMA* wholesale at its load; this file loads after
;; it, so appending here is idempotent across image rebuilds.
(setf *schema* (append *events-schema* *session-state-schema*))

;;; --- write path (session_state.zig putLocked) -------------------------------

(defun session-log-watermark (session-id)
  "The session's current event-log position: KV writes stamp it as log_seq so
the fold can be rewound to any checkpoint anchor."
  (events :session-id session-id :as :value :columns '("coalesce(max(log_position), 0)")))

(defun %state-put-locked (handle session-id ws key value-json)
  "Append one event and refresh the fold row inside the caller's transaction."
  ;; Returns the new VER. The key quota binds only when a non-delete write
  ;; would mint a NEW key: overwrites and deletes always pass (the putLocked
  ;; rule).
  (when (and value-json
             (eql 1 (sqlite:execute-single
                     handle
                     "select sum(value_json is not null) >= ? and sum(key = ?) = 0
                        from session_state where session_id = ? and ws = ?"
                     +max-state-keys-per-ws+ key session-id ws)))
    (error 'session-state-quota-exceeded))
  (let ((log-seq (session-log-watermark session-id))
        (now (iso-now)))
    (sqlite:execute-non-query
     handle
     "insert into session_state_events (session_id, ws, key, value_json, log_seq, occurred_at)
      values (?, ?, ?, ?, ?, ?)"
     session-id ws key value-json log-seq now)
    (let ((ver (sqlite:last-insert-rowid handle)))
      (sqlite:execute-non-query
       handle
       "insert into session_state (session_id, ws, key, ver, value_json, updated_log_seq, updated_at)
        values (?, ?, ?, ?, ?, ?, ?)
        on conflict (session_id, ws, key) do update set
          ver = excluded.ver,
          value_json = excluded.value_json,
          updated_log_seq = excluded.updated_log_seq,
          updated_at = excluded.updated_at"
       session-id ws key ver value-json log-seq now)
      ver)))

(defun session-state-put (session-id key value-json &key (ws "main"))
  "Store VALUE-JSON (pre-serialized JSON text, stored verbatim — the nc.state
contract does not validate it) under KEY. Returns the new VER."
  (check-strings session-id key value-json)
  (when (> (length value-json) +max-state-value-bytes+)
    (error 'state-value-too-large))
  (with-store-transaction (handle)
     (%state-put-locked handle session-id ws key value-json)))

(defun session-state-delete (session-id key &key (ws "main"))
  "Append a delete event (NULL value) and tombstone the fold row."
  (check-strings session-id key)
  (with-store-transaction (handle)
     (%state-put-locked handle session-id ws key nil))
  nil)

;;; --- read path ---------------------------------------------------------------

(defun session-state-get (session-id key &key (ws "main"))
  "(values VALUE-JSON VER) for a live key, or NIL. Tombstones read as absent."
  (check-strings session-id key)
  (with-store-handle (handle)
    (values-list
     (or (first (sql-rows
                 handle
                 "select value_json, ver from session_state
                  where session_id = ? and ws = ? and key = ?
                    and value_json is not null"
                 session-id ws key))
         '(nil)))))

;;; --- fold rewind (session_state.zig foldToWatermark) -------------------------

(defun fold-session-state-to-watermark-in-transaction
    (handle session-id watermark-log-seq)
  "The fold-rewind body, composable inside a caller's transaction (the
navigation writer commits it atomically with the head move)."
  (let ((stale (sql-rows
                   handle
                   "select ws, key from session_state
                    where session_id = ? and updated_log_seq > ?"
                   session-id watermark-log-seq)))
       (loop for (ws key) in stale
             for prior = (sql-rows
                          handle
                          "select id, value_json, log_seq from session_state_events
                          where session_id = ? and ws = ? and key = ? and log_seq <= ?
                          order by id desc limit 1"
                          session-id ws key watermark-log-seq)
             do (if prior
                    (destructuring-bind (id value-json log-seq) (first prior)
                      (sqlite:execute-non-query
                       handle
                       "update session_state
                     set ver = ?, value_json = ?, updated_log_seq = ?, updated_at = ?
                     where session_id = ? and ws = ? and key = ?"
                       id value-json log-seq (iso-now) session-id ws key))
                    (sqlite:execute-non-query
                     handle
                     "delete from session_state
                   where session_id = ? and ws = ? and key = ?"
                     session-id ws key)))
       (length stale)))

;;; --- harness sections (session_state.zig:396-459, workspace_lane.zig:638) ----

(defun harness-state-key (name)
  "nc:harness:NAME."
  ;; NAME must be non-empty and colon-free — that restriction is what keeps
  ;; the harness surface from addressing other nc: namespaces.
  (unless (and (stringp name) (plusp (length name))
               (not (find #\: name)))
    (error 'invalid-harness-key))
  (concatenate 'string +harness-key-prefix+ name))

(defun harness-texts (handle session-id)
  "The session's live harness values as (KEY . TEXT), key-ordered, a value
that does not decode to a string skipped."
  ;; Session-scoped: no ws filter, matching the Zig query.
  (loop for (key value-json)
          in (sql-rows
              handle
              "select key, value_json from session_state
               where session_id = ? and value_json is not null
                 and substr(key, 1, ?) = ?
               order by key asc, ws asc"
              session-id (length +harness-key-prefix+) +harness-key-prefix+)
        for text = (ignore-errors (nlk:decode-json value-json))
        when (stringp text)
          collect (cons key text)))

(defun set-harness-section (session-id name text &key (ws "main"))
  "Install standing system context: TEXT is injected into every subsequent
provider round as a <harness key=NAME> block."
  ;; The 64KiB budget binds at this seam so a turn can never be compiled over
  ;; an over-budget system context. Returns the new VER.
  (check-strings session-id text)
  (let ((key (harness-state-key name)))
    (with-store-transaction (handle)
       ;; The write-seam unit: the decoded text length of every other value.
       (when (> (+ (loop for (other . other-text) in (harness-texts handle session-id)
                         unless (equal other key)
                           sum (length other-text))
                   (length text))
                +max-harness-bytes+)
         (error 'harness-budget-exceeded))
       (%state-put-locked handle session-id ws key (json-string-literal text)))))

(defun get-harness-section (session-id name &key (ws "main"))
  "(values TEXT VER) or NIL."
  (bind (((value ver) (session-state-get session-id (harness-state-key name) :ws ws))
         (text (and value (ignore-errors (nlk:decode-json value)))))
    (when (stringp text)
      (values text ver))))

(defun clear-harness-section (session-id name &key (ws "main"))
  (session-state-delete session-id (harness-state-key name) :ws ws))

(defun list-harness-sections (session-id)
  "The session's harness entries as (NAME . TEXT), key-ordered, under the
64KiB read floor: the first entry that does not fit truncates the remainder
(the listHarnessEntries break rule), and malformed values are skipped, never
fatal (HARNESS-TEXTS)."
  (check-type session-id session-id)
  (with-store-handle (handle)
    (loop with budget = +max-harness-bytes+
          for (key . text) in (harness-texts handle session-id)
          while (<= (length text) budget)
          do (decf budget (length text))
          collect (cons (subseq key (length +harness-key-prefix+)) text))))

(defun harness-section-block (name text)
  "The exact injected shape (turn_engine.zig:5450): the text rides verbatim,
unescaped — harness content is the session's own standing context."
  (format nil "<harness key=\"~a\">~%~a~%</harness>" name text))

;;; --- learned context-window observations -----------------------------------
;;; The systematic answer to "detect the overflow without a hardcoded
;;; context_tokens": every provider round is an experiment against the real
;;; window. A successful round's usage total is a PROVEN floor (the server
;;; processed that many tokens). A hard context rejection bounds the window
;;; from above with our estimate of the refused request — estimate-grade
;;; evidence, so a later proven floor above it clears it rather than fighting
;;; it. A finish=length cut contributes only its floor: local serving stacks
;;; cut at their own output caps too, and reading a cut as a ceiling would
;;; durably poison the budget (adversarial review F2). Belief resolution
;;; lives in provider.lisp CONTEXT-WINDOW-BELIEF; this file owns the rows.

(defun observe-context-window (provider-id model-id tokens statement)
  "Write one (PROVIDER-ID, MODEL-ID, TOKENS, now) observation row through
STATEMENT, the upsert that says which bound it moves."
  ;; No-op without an open store. => TOKENS.
  (check-strings provider-id model-id)
  (check-type tokens (integer 1))
  (when (store-open-p)
    (with-store-transaction (handle)
       (sqlite:execute-non-query handle statement provider-id model-id tokens (iso-now)))
    tokens))

(defun observe-context-window-floor (provider-id model-id tokens)
  "Record that (PROVIDER-ID, MODEL-ID) successfully processed TOKENS in one
request+response: the window is proven at least this large."
  ;; Raises the stored floor monotonically and clears an estimate-grade
  ;; ceiling the proof contradicts. No-op without an open store.
  (observe-context-window
   provider-id model-id tokens
   "insert into context_window_observations
      (provider_id, model_id, floor_tokens, updated_at)
    values (?, ?, ?, ?)
    on conflict (provider_id, model_id) do update set
      floor_tokens = max(coalesce(floor_tokens, 0), excluded.floor_tokens),
      ceiling_tokens = case
        when ceiling_tokens is not null
             and max(coalesce(floor_tokens, 0), excluded.floor_tokens)
                 >= ceiling_tokens
          then null
        else ceiling_tokens end,
      updated_at = excluded.updated_at"))

(defun observe-context-window-ceiling (provider-id model-id tokens)
  "Record that (PROVIDER-ID, MODEL-ID) rejected a request estimated at
TOKENS as over its context window."
  ;; Latest evidence wins (aggregator replicas vary per request), but never
  ;; below a proven floor. No-op without an open store.
  (observe-context-window
   provider-id model-id tokens
   "insert into context_window_observations
      (provider_id, model_id, ceiling_tokens, updated_at)
    values (?, ?, ?, ?)
    on conflict (provider_id, model_id) do update set
      ceiling_tokens = max(excluded.ceiling_tokens,
                           coalesce(floor_tokens, 0)),
      updated_at = excluded.updated_at"))

(defun context-window-observation (provider-id model-id)
  "(values FLOOR CEILING) for (PROVIDER-ID, MODEL-ID) — either may be NIL,
and both are NIL when nothing was ever observed or the store is closed."
  (check-strings provider-id model-id)
  (let ((row (when-store-handle (handle)
               (first
                (sql-rows
                 handle
                 "select floor_tokens, ceiling_tokens
                    from context_window_observations
                   where provider_id = ? and model_id = ?"
                 provider-id model-id)))))
    (values-list (mapcar (lambda (bound) (and (integerp bound) bound)) row))))
