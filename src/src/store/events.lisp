;;;; events.lisp --- durable append-only event log. Kernel-owned, locked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Single durable authority. Every fact lands here. Wire kinds use one
;;;; namespace (`turn.*`). Unknown kinds materialize as UNKNOWN-EVENT.
;;;;
;;;; Append contract:
;;;;   - every row carries a canonical aggregate identity, global LOG-POSITION,
;;;;     per-aggregate SEQUENCE, contract hash, and invocation JSON.
;;;;   - SESSION.CREATED is required before any other kind for that session,
;;;;     unless the session is standing by (standby.lisp): a configuration
;;;;     fact waits there and a block materializes the identity first
;;;;   - RECORD-EVENT writes unowned kinds only. Lifecycle and tool kinds are
;;;;     owned: CREATE-SESSION, ADMIT-TURN, COMPLETE-TURN / FAIL-TURN /
;;;;     CANCEL-TURN, RECORD-TOOL-CALL, RECORD-TOOL-RESULT.
;;;;   - owners bind *EVENT-WRITE-OWNER* before APPEND-EVENT-ROW
;;;;   - optional EXPECTED-SEQUENCE must match the aggregate's current sequence
;;;;   - an existing EVENT-ID with identical envelope and payload is a no-op
;;;;   - a colliding EVENT-ID with different content is EVENT-ID-CONFLICT
;;;;   - PAYLOAD-HASH is sha256 of the stored JSON; a mismatch on read signals
;;;;   - CONTRACT-HASH fingerprints the event kind/schema/aggregate contract
;;;;   - INVOCATION-JSON preserves the command source and invocation metadata
;;;;   - OCCURRED-AT is UTC ISO-8601
;;;;   - CAUSATION-ID / CORRELATION-ID are optional causal links
;;;;
;;;; Provider-request reconstruction is a projection. It does not live here.

(in-package #:nodecode.kernel)

;;; --- kind namespace -----------------------------------------------------

;;; --- retention horizons --------------------------------------------------
;;; Every durable kind declares which projection horizon carries it. The log
;;; keeps everything; stream projections carry only what a client folds into
;;; its transcript and admission view. Evidence kinds (provider cadence,
;;; recovery bookkeeping, queue snapshots) stay durable and queryable, but
;;; flow on NEITHER the live stream nor a resume replay: live publication
;;; applies this same suppression, so a continuously-connected peer and a
;;; reconnecting peer fold identical transcripts. Live-horizon UX facts
;;; travel as explicit named frames, never as leaked durable rows.

(defparameter *owned-kind-owners* (make-hash-table :test #'equal)
  "Wire kind -> owner symbol. The one write-authority table for the log.")

(defparameter *replay-suppressed-kinds* '()
  "The evidence kinds: durable, never on the live stream or a resume replay.")

(deftype event-kind () "A kind as it rides the wire, e.g. \"session.created\"." 'string)

(defmacro define-kind (name wire &key owner evidence documentation)
  "Declare the kind constant NAME for WIRE, entering its write OWNER in
*OWNED-KIND-OWNERS* and, when EVIDENCE, the kind in *REPLAY-SUPPRESSED-KINDS*."
  `(progn
     (defparameter ,name ,wire ,@(when documentation (list documentation)))
     ,@(when owner `((setf (gethash ,name *owned-kind-owners*) ',owner)))
     ,@(when evidence `((pushnew ,name *replay-suppressed-kinds* :test #'string=)))))

(define-kind +kind-session-created+ "session.created" :owner create-session)
;; A session's folder from then on: MOVE-SESSIONS writes one per session it
;; moves, and FIND-SESSION and the session directory read the newest.
(define-kind +kind-session-moved+ "session.moved" :owner move-sessions :evidence t)
(define-kind +kind-turn-started+ "turn.started" :owner admit-turn)
(define-kind +kind-input-committed+ "turn.input_committed" :owner admit-turn)
(define-kind +kind-assistant-completed+ "turn.assistant_message_completed" :owner complete-turn)
(define-kind +kind-tool-call-started+ "turn.tool_call_started")
(define-kind +kind-tool-result+ "turn.tool_result")
(define-kind +kind-context-evicted+ "session.context.evicted" :owner advance-history-floor)
;; Within-turn eviction watermarks are budget bookkeeping like the retry
;; facts: the transcript keeps the rows, only the provider window loses them.
(define-kind +kind-turn-context-evicted+ "turn.context.evicted" :evidence t)
;; The prompt a conversation runs under and every change to it after
;; (engine/turn.lisp TURN-PROMPT): the same horizon — the provider window
;; reads it through the fold, no transcript row shows it.
(define-kind +kind-turn-context-prompt+ "turn.context.prompt" :evidence t)
(define-kind +kind-floor-engaged+ "organism.floor_engaged" :owner engage-floor :evidence t)
(define-kind +kind-floor-lowered+ "organism.floor_lowered" :owner lower-floor :evidence t)
(define-kind +kind-organism-upgraded+ "organism.upgraded" :owner record-upgrade :evidence t)
(define-kind +kind-definition-recorded+ "organism.definition_recorded"
  :owner record-definitions :evidence t)
(define-kind +kind-definition-forgotten+ "organism.definition_forgotten"
  :owner record-definitions :evidence t)
;; The self sealed as a numbered generation (organism/generation.lisp): the
;; keeper's trial child proved the folder and the manifest was written.
(define-kind +kind-generation-sealed+ "organism.generation_sealed"
  :owner seal-generation :evidence t)
;; The folder restored to a sealed generation: the boot that finds the
;; restoring marker records what it runs now.
(define-kind +kind-generation-restored+ "organism.generation_restored"
  :owner record-generation-restored :evidence t)
(define-kind +kind-turn-completed+ "turn.completed" :owner complete-turn)
(define-kind +kind-turn-failed+ "turn.failed" :owner fail-turn)
(define-kind +kind-turn-cancelled+ "turn.cancelled" :owner cancel-turn)
(define-kind +kind-turn-suspended+ "turn.suspended" :owner suspend-turn :evidence t)
(define-kind +kind-turn-resume-claimed+ "turn.resume_claimed"
  :owner claim-turn-resumption :evidence t)
(define-kind +kind-turn-queue-snapshot+ "session_input_queue_snapshot_updated"
  :owner admit-active-turn-input :evidence t)
;; turn.usage is no evidence: a resumed client must show its last usage, not an empty lane
(define-kind +kind-turn-usage+ "turn.usage")
;; Chrome, not evidence: a shell folds each retry into its turn's resting line,
;; live and on a resume replay alike.
(define-kind +kind-provider-retry+ "turn.provider_retry")
(define-kind +kind-provider-request+ "turn.provider_request" :evidence t)
;;; Chrome, not evidence — NOT replay-suppressed, so a room's status card and
;;; a resumed client fold the same move, beside the provider_retry facts it
;;; follows.
(define-kind +kind-provider-fallback+ "turn.provider_fallback"
  :documentation "One turn-scoped model failover: the round's request against
FROM-PROVIDER/FROM-MODEL failed and the rest of the turn runs on
TO-PROVIDER/TO-MODEL.")
(define-kind +kind-head-moved+ "session.head_moved" :owner move-session-head :evidence t)
;;; Shared between the writer (the gateway's retract), the redo cursor, which
;;; declines to offer such an origin back, and the session directory, which
;;; forgets a prompt taken back from an empty session.
(defparameter +head-move-reason-retract+ "retract"
  "The SESSION.HEAD_MOVED reason a retraction writes: a cancelled turn that
produced nothing, taken off the visible chain at the operator's request.")
(define-kind +kind-workspace-captured+ "session.workspace_captured"
  :owner record-workspace-capture :evidence t)
;; Config evidence: the selection is served by its read model
;; (SESSION-MODEL-SELECTION), never folded from the transcript.
(define-kind +kind-model-selected+ "session.model_selected" :evidence t)
;; The name: served by the session directory (SESSION-TITLE), never a
;; transcript row. Newest wins, and an event rather than session KV so an
;; undo cannot erase it, like the model selection; SESSION-TITLE reads it
;; ahead of the first prompt's line.
(define-kind +kind-session-title+ "session.title" :evidence t
  :documentation "A name for the session, the operator's (/rename, the title route), a
cell's, or the one a model writes from its first prompt (gateway.lisp
SET-SESSION-TITLE, NAME-SESSION).")
;; A turn's mark that its answer waits on the person it answers: served by the
;; session directory beside the session's newest turn (LIST-SESSIONS), never
;; a transcript row; an event rather than session KV, as the name is.
(define-kind +kind-session-waits+ "session.waits" :evidence t
  :documentation "The mark a turn puts on its answer: it waits on the person it answers --
a question, a go or no-go -- on QUESTION, or on its own last line when that
is empty. Written by the turn, never guessed from its words (gateway.lisp
ANSWER-WAITS).")
;;; Persisted-history retention: the transcript keeps the
;;; block, hydrated from this fact on replay and resume alike. Never provider
;;; history: the context engine walks message kinds only, so a voided attempt
;;; is never fed back.
(define-kind +kind-attempt-voided+ "turn.attempt_voided"
  :documentation "What one failed provider attempt had already streamed — reasoning and
text the user watched arrive — sealed before the retry policy re-runs the
request or lets the failure stand.")

;;; --- standby sessions -----------------------------------------------------
;;; An id minted before its first block is held in memory (standby.lisp) and
;;; is durable once a block lands. Two things keep that promise here: the
;;; gate every session append passes, and the materialize a block writer
;;; calls at the top of its own transaction.

;;; They wait on the standby record and are written, in the order they were
;;; held, when the first block materializes the session. Everything else a
;;; session carries — a turn, a tool call, an exchange — is content, and
;;; content is what makes a session: one landing before the identity does is a
;;; bug in the writer, refused loudly by SESSION-FACT-GATE rather than written
;;; out of order.
(defparameter +standby-waited-kinds+
  (list +kind-model-selected+ +kind-session-title+)
  "Facts a standby session may carry before its first block: the session's
own configuration and the marks a shell puts on it.")

(defun session-fact-gate (session-id aggregate-type kind payload options)
  "The append path's identity gate."
  ;; SESSION-ID must have a SESSION.CREATED fact, be standing by, or be named
  ;; only as attribution by a fact of another aggregate. => NIL when the
  ;; append may proceed as written; :HELD when the fact is a standby session's
  ;; configuration and now waits on its record, in which case the caller
  ;; appends nothing.
  (cond
    ;; The identity itself, and a session that is durable already.
    ((string= kind +kind-session-created+) nil)
    ((session-created-fact-p session-id) nil)
    ((not (on-standby-p session-id))
     (error 'unknown-session :session-id session-id))
    ;; An organism fact names a session as attribution — the boot whose
    ;; floor engagement this is — and asserts nothing about it, so it lands
    ;; whether or not the id has a block yet.
    ((not (equal aggregate-type "session")) nil)
    ((member kind +standby-waited-kinds+ :test #'string=)
     (hold-standby-session-fact session-id kind payload options)
     :held)
    (t (error 'standby-block-refused :session-id session-id :kind kind))))

(defun event-kind-replay-suppressed-p (kind)
  "True when KIND is durable evidence rather than resume-snapshot history."
  (not (null (member kind *replay-suppressed-kinds* :test #'string=))))

(defvar *event-write-owner* nil
  "Bound to the owner symbol while an owned event is appended.")

(defun event-kind-owner (kind)
  (gethash kind *owned-kind-owners*))

(defmacro with-event-owner (owner &body body)
  `(let ((*event-write-owner* ,owner))
     ,@body))

;;; --- event classes ------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The payload column carries JSON text on the wire and the decoded object
  ;; as an initarg; EVENT-ROW- TO-INSTANCE is where the two meet.
  ;;
  ;; EVAL-WHEN because DEFINE-EVENT-ROW-READER expands this table into a
  ;; positional DESTRUCTURING-BIND. Reading 2000 rows through a runtime zip
  ;; cost a plist cons plus eight linear GETF walks and two APPLYs per row,
  ;; and paging the log is the session-hydration hot path — the table stays
  ;; the one authority, but the reader it generates is straight-line code.
  (defparameter +event-row-columns+
    '(("log_position" :log-position "integer primary key autoincrement")
      ("aggregate_type" :aggregate-type "text not null")
      ("aggregate_id" :aggregate-id "text not null") ("session_id" :session-id "text")
      ("subject_id" :subject-id "text") ("turn_id" :turn-id "text")
      ("command_id" :command-id "text") ("event_id" :event-id "text not null unique")
      ("aggregate_sequence" :aggregate-sequence "integer not null")
      ("kind" :kind "text not null") ("schema_version" :schema-version "integer not null")
      ("definition_revision" :definition-revision "text") ("payload" :payload "text not null")
      ("payload_hash" :payload-hash :sha256) ("contract_hash" :contract-hash :sha256)
      ("invocation_json" :invocation-json "text") ("causation_id" :causation-id "text")
      ("correlation_id" :correlation-id "text") ("occurred_at" :occurred-at "text not null"))
    "The event envelope's wire<->slot authority: each column of one events row,
in select order, paired with the initarg it hydrates and its SQL type (:SHA256
a `sha256:' hash, checked)."))

;;; EVENT has one slot per envelope column, named for its initarg and read as
;;; EVENT-<slot> (EVENT-ID is its own name): a slot not named below is NIL
;;; until given, and one named carries its own options instead.
(macrolet ((define-envelope (&rest named)
             `(defclass event ()
                ,(loop for (nil initarg) in +event-row-columns+
                       for slot = (intern (symbol-name initarg))
                       for options = (assoc slot named)
                       collect `(,slot :initarg ,initarg
                                       :reader ,(if (eq slot 'event-id)
                                                    slot
                                                    (intern (format nil "EVENT-~a" slot)))
                                       ,@(if options (rest options) '(:initform nil))))
                (:documentation "Base class for every logged event."))))
  (define-envelope
    (log-position :initform nil
                  :documentation "Globally ordered database position; NIL until the event is logged.")
    (aggregate-type :type string) (aggregate-id :type string) (session-id) (event-id)
    (aggregate-sequence) (schema-version) (kind :type event-kind)
    (payload :initform nil
             :documentation "The stored payload object as decoded, its fields
unrenamed: what the wire carries (gateway.lisp EVENT-FACT-WIRE), and all an
event of no registered kind has.")))
(export (cons 'event (loop for slot in (sb-mop:class-direct-slots (find-class 'event))
                           append (sb-mop:slot-definition-readers slot))))

(defclass unknown-event (event)
  ()
  ;; The reducer's default method treats this as identity, so a mutation that
  ;; introduces a new event kind is total by construction.
  (:documentation
   "Materialized when a row's kind has no registered class."))

;;; --- the kind registry --------------------------------------------------

(defvar *event-class-registry* (make-hash-table :test #'equal)
  "Maps wire kind strings to CLOS class names. Populated by DEFINE-EVENT.")

;;; --- the event declaration -----------------------------------------------
;;; Every logged event used to be written as the same three-part ritual: a
;;; DEFCLASS naming each slot's initarg, reader and initform; an
;;; INITIALIZE-INSTANCE :AFTER re-deriving the wire key, the type check and
;;; the same initform a second time; and a REGISTER-EVENT-CLASS call. The
;;; two halves drifted -- some slots type-checked their payload read and some
;;; took GETHASH's default untyped -- because nothing forced them to agree.
;;; DEFINE-EVENT states each slot once and writes both halves from it, so the
;;; hydration of a slot cannot disagree with its declaration.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +event-slot-options+ '(:from :default :initform :reader :doc :param)
    "Keywords a DEFINE-EVENT slot spec may carry after its wire read.")

  (defun event-reader-name (prefix slot)
    (intern (uiop:strcat (symbol-name prefix) "-" (symbol-name slot)) (symbol-package prefix)))

  (defun event-slot-plan (spec prefix)
    "Parse one DEFINE-EVENT slot spec into (values SLOT-FORM HYDRATION)."
    ;; HYDRATION is NIL for a slot the stored payload does not populate. Both
    ;; the read and any :FROM form refer to PAYLOAD and EVENT, which
    ;; DEFINE-EVENT binds in the method it writes.
    (let* ((spec (if (listp spec) spec (list spec)))
           (name (first spec))
           (tail (rest spec))
           (read nil))
      (unless (and name (symbolp name))
        (error "DEFINE-EVENT: slot name must be a symbol, got ~s" name))
      (cond ((member (first tail) +json-types+)
             (let* ((type (pop tail))
                    (keys (loop while (stringp (first tail)) collect (pop tail))))
               (unless keys
                 (error "DEFINE-EVENT: slot ~s declares type ~s with no wire key"
                        name type))
               (setf read `(json-value payload ,type ,@keys))))
            ((eq (first tail) :from)
             (setf read (second tail) tail (cddr tail))))
      (loop for option in tail by #'cddr
            unless (member option +event-slot-options+)
              do (error "DEFINE-EVENT: slot ~s has unknown option ~s (want one of ~{~s~^ ~})"
                        name option +event-slot-options+))
      (let* ((missing '#:missing)
             (default (getf tail :default missing))
             (defaulted (not (eq default missing))))
        (values `(,name :initarg ,(intern (symbol-name name) :keyword)
                        :reader ,(or (getf tail :reader)
                                     (event-reader-name prefix name))
                        :initform ,(getf tail :initform (and defaulted default))
                        ,@(when (getf tail :doc) (list :documentation (getf tail :doc))))
                (when read
                  `(setf (slot-value event ',name)
                         ,(if defaulted `(or ,read ,default) read)))))))

  (defun event-recorder (record kind specs documentation)
    "The DEFUN a DEFINE-EVENT :RECORD declares over the hydrated slots of SPECS."
    ;; RECORD is NAME or (NAME POSITIONAL... [:check (CHECK...)]): a
    ;; positional is a slot or (SLOT TYPE), STRING unless given, and every
    ;; other slot is a keyword -- its :PARAM, else its name -- defaulting to
    ;; the slot's default. A value is written under its wire key when it is of
    ;; the slot's JSON type, a boolean always; a CHECK, SLOT or (SLOT TYPE),
    ;; refuses a value not of TYPE, by default the slot's own or NIL. The
    ;; write is RECORD-TURN-FACT's (lifecycle.lisp), under NAME as owner.
    (destructuring-bind (name &rest tail) (if (listp record) record (list record))
      (let* ((positional (loop for item in tail until (keywordp item) collect item))
             (checked (getf (member-if #'keywordp tail) :check))
             (required (mapcar (lambda (item) (if (consp item) (first item) item)) positional))
             (fields (remove-if-not (lambda (spec)
                                      (and (consp spec) (member (second spec) +json-types+)))
                                    specs))
             (keys (loop for (slot nil nil . options) in fields
                         unless (member slot required)
                           collect `(,(getf options :param slot)
                                     ,(getf options :default (getf options :initform))))))
        (flet ((lisp-type (type) (contract-lisp-type type nil)))
          `(defun ,name (turn ,@required ,@(when keys (cons '&key keys)))
             ,documentation
             ,@(loop for item in positional
                     collect `(check-type ,@(if (consp item) item (list item 'string))))
             ,@(loop for (slot type nil . options) in fields
                     for check = (assoc slot (mapcar #'uiop:ensure-list checked))
                     when check
                       collect `(check-type ,(getf options :param slot)
                                            ,(or (second check)
                                                 `(or null ,(lisp-type type)))))
             (record-turn-fact turn ,kind ',name
              (nconc ,@(loop for (slot type key . options) in fields
                             for var = (getf options :param slot)
                             collect (if (eq type :boolean)
                                         `(list ,(intern (string-upcase key) :keyword) (and ,var t))
                                         `(when ,(if (eq type :text)
                                                     `(plusp (length ,var))
                                                     `(typep ,var ',(lisp-type type)))
                                            (list ,(intern (string-upcase key) :keyword)
                                                  ,var))))))))))))

(defmacro event-row-constructor (class)
  "A function of one row's +EVENT-ROW-COLUMNS+ values, in select order with
the payload decoded, making an instance of CLASS."
  ;; MAKE-INSTANCE of a constant class with constant initargs compiles to
  ;; SBCL's optimized constructor; APPLY of a class found per row takes the
  ;; general path, at four times the cost of every hydrated event.
  (let ((vars (loop for (nil initarg) in +event-row-columns+
                    collect (make-symbol (symbol-name initarg)))))
    `(lambda ,vars
       (make-instance ',class ,@(loop for (nil initarg) in +event-row-columns+
                                      for var in vars
                                      append (list initarg var))))))

(setf (get 'unknown-event 'row-constructor) (event-row-constructor unknown-event))

(defmacro define-event (name-and-options kind documentation slots &optional record-documentation)
  "Declare one logged event: its class, how a stored payload hydrates it, and
the wire kind that materializes it."
  ;;
  ;;   NAME-AND-OPTIONS  NAME | (NAME [:parent CLASS] [:prefix SYMBOL] [:record RECORD])
  ;;   KIND              the wire kind form, or NIL for an abstract base
  ;;   SLOTS             a list of slot specs
  ;;
  ;; A slot spec is one of
  ;;
  ;;   (NAME TYPE "key"... [options])   read the payload through JSON-VALUE
  ;;   (NAME :from FORM [options])      compute from PAYLOAD directly
  ;;   (NAME [options])                 declared but never hydrated
  ;;
  ;; where options are :DEFAULT (the value a NIL read falls back to, and the
  ;; :INITFORM unless one is given), :INITFORM, :READER (default is
  ;; PREFIX-NAME) and :DOC. TYPE is one of JSON-VALUE's, so a slot's declared
  ;; type is also its payload type check -- the two cannot drift apart.
  ;;
  ;; RECORD declares the turn fact's recorder, RECORD-DOCUMENTATION its
  ;; docstring (EVENT-RECORDER): the writer's payload is the reader's slots,
  ;; so a key cannot be written that no slot hydrates. :PARAM is the
  ;; writer's slot option.
  ;;
  ;; The class and every reader are exported: the declaration states the wire
  ;; kind, the slots, the hydration and the boundary in one place, so a
  ;; package mirror cannot fall behind a slot.
  (destructuring-bind (name &key (parent 'event) (prefix name) record)
      (if (listp name-and-options) name-and-options (list name-and-options))
    (let* ((plans (mapcar (lambda (spec) (multiple-value-list (event-slot-plan spec prefix)))
                          slots))
           (slot-forms (mapcar #'first plans)))
      `(progn
         (defclass ,name (,parent)
           ,slot-forms
           (:documentation ,documentation))
         ;; Always defined, hydrations or not: a slot-less event (the floor
         ;; engaged) still materializes through the :PAYLOAD initarg.
         (defmethod initialize-instance :after ((event ,name)
                                                 &key payload
                                                 &allow-other-keys)
           "Populate slots from the stored payload when materializing."
           (declare (ignorable payload))
           (when (hash-table-p payload)
             ,@(remove nil (mapcar #'second plans))))
         (export '(,name ,@(mapcar (lambda (slot) (getf (rest slot) :reader)) slot-forms)
                   ,@(and record (list (first (uiop:ensure-list record))))))
         ,@(when kind `((setf (get ',name 'row-constructor) (event-row-constructor ,name)
                              (gethash (the event-kind ,kind) *event-class-registry*) ',name)))
         ;; The recorder is its kind's one writer, the owner RECORD-EVENT refuses for.
         ,@(when record
             (list (event-recorder record kind slots record-documentation)
                   `(setf (gethash ,kind *owned-kind-owners*) ',(first (uiop:ensure-list record)))))
         ',name))))

(defun materialize-event (&rest initargs &key kind &allow-other-keys)
  "Construct an EVENT from one row's INITARGS — the +EVENT-ROW-COLUMNS+ plist
with :PAYLOAD already decoded."
  ;; KIND is looked up in *EVENT-CLASS-REGISTRY*. A kind with no registered
  ;; class materializes as an UNKNOWN-EVENT carrying the payload as an opaque
  ;; table.
  (apply #'make-instance
         (or (find-class (gethash kind *event-class-registry*) nil) 'unknown-event)
         initargs))

;;; --- schema -------------------------------------------------------------

;;; Fresh state is required when the old events table is detected; rows are
;;; never silently upcast.
;;; --- the open turns -----------------------------------------------------
;;; Every admitted turn without a terminal fact, kept by the log itself: a
;;; start enters the set and its terminal fact takes it out, inside the
;;; transaction that appends each (lifecycle.lisp +NON-TERMINAL-TURN-SQL+ reads
;;; it). Without the set, an open-turn read would probe terminal facts once
;;; per start ever made. The set is almost always empty, so every such read —
;;; each subscribe, each lifecycle frame, each admission's single-active-turn
;;; check — touches one page. A store whose log predates the set derives it
;;; once, at the open that makes it.

(defparameter +open-turns-table+
  "create table if not exists open_turns (
     log_position integer primary key,
     turn_id text,
     session_id text)"
  "The open turns: each one's start, by its position in the log.")

(defparameter +open-turns-fill+
  (format nil "insert into open_turns (log_position, turn_id, session_id)
      select log_position, turn_id, session_id from events
       where kind = '~a'
         and not exists (select 1 from events as terminal
                          where terminal.turn_id = events.turn_id
                            and terminal.kind in ('~a', '~a', '~a'))"
          +kind-turn-started+
          +kind-turn-completed+ +kind-turn-failed+ +kind-turn-cancelled+)
  "The open turns as the log says them: every start without a terminal fact.")

(defparameter *open-turns-schema*
  (list
   +open-turns-table+
   (format nil "create trigger if not exists a_started_turn_is_open
      after insert on events when new.kind = '~a'
      begin
        insert into open_turns (log_position, turn_id, session_id)
          values (new.log_position, new.turn_id, new.session_id);
      end"
           +kind-turn-started+)
   (format nil "create trigger if not exists a_terminal_fact_closes_its_turn
      after insert on events when new.kind in ('~a', '~a', '~a')
      begin
        delete from open_turns where turn_id = new.turn_id;
      end"
           +kind-turn-completed+ +kind-turn-failed+ +kind-turn-cancelled+))
  "The open turns' table and the two triggers that keep it.")

;;; --- the session directory ----------------------------------------------
;;; The directory (identity.lisp LIST-SESSIONS, SESSION-TITLE) is a projection
;;; the log keeps in step with itself: one row per session, written by a
;;; trigger inside the transaction that appends each row, so no writer can
;;; forget it and a rolled-back append takes its directory change with it.
;;; Derived per request instead, a listing probed the log four times per
;;; session. A store whose directory is empty fills it from the log at
;;; open: a fresh store (nothing to fill) and one written before the directory
;;; existed are the same case.

(defun directory-text-sql (json path)
  "JSON's non-empty text at PATH, else NULL: JSON-VALUE's :text read in SQL.
A session.title's name (a newest title that is not text falls to the
prompt, never to an older title), a turn's invocation source kind."
  (format nil "case when json_type(~a, '~a') = 'text' ~
                    then nullif(json_extract(~a, '~a'), '') end"
          json path json path))

(defun directory-retract-to-root-sql (row)
  "SQL true when ROW (a table alias, or a trigger's NEW) is the head move a
retract writes back to the root: the session's only turn taken back, so the
prompt that named it names nothing and the next prompt names it."
  (format nil "~a.kind = '~a' and json_extract(~a.payload, '$.reason') = '~a' ~
               and json_extract(~a.payload, '$.target-turn-id') is null"
          row +kind-head-moved+ row +head-move-reason-retract+ row))

(defun directory-prompt-sql (payload)
  "A committed prompt PAYLOAD's first line that says something, trimmed and
bounded to 64 characters -- the name a session has from the moment its first
prompt is admitted until one is given it; NULL when its message is not text
or holds no word, so the next prompt names the session."
  ;; A pasted log opening on a blank line is named by its first line of
  ;; text, never by the empty one.
  (let ((text (format nil "ltrim(json_extract(~a, '$.message'), ' ' || char(9, 10, 13))"
                      payload)))
    (format nil "case when json_type(~a, '$.message') = 'text' ~
                      then nullif(rtrim(substr(~a, 1, ~
                                               min(64, coalesce(nullif(instr(~a, char(10)), 0) - 1, ~
                                                                64))), ~
                                        ' ' || char(9, 13)), ~
                                  '') end"
            payload text text)))

(defparameter +directory-turn-kinds+
  (list +kind-turn-started+ +kind-turn-completed+ +kind-turn-failed+ +kind-turn-cancelled+)
  "The facts that begin and end a turn: the newest of them is the session's
newest turn and where it stands, in the directory's TURN_ID and TURN_KIND.")

(defparameter *session-directory-schema*
  (list
   ;; CREATED_AT is the session.created row's clock, NULL until it lands;
   ;; UPDATED_AT the newest row's; TITLE the newest session.title's name;
   ;; PROMPT the first committed prompt's line that says something
   ;; (DIRECTORY-PROMPT-SQL); MESSAGE_COUNT the prompts and
   ;; answers. PARENT and the fork anchor are whatever the created payload
   ;; says: a v1 row's `"parent": false' reads back as 0. SOURCE is what
   ;; SESSION-SOURCE-KIND answers (lifecycle.lisp): the invocation source
   ;; kind of the session's first turn, which is never empty
   ;; (MAKE-INVOCATION-SOURCE) -- NULL until that turn lands. TURN_ID and
   ;; TURN_KIND are the session's newest turn and the fact that began or
   ;; ended it (+DIRECTORY-TURN-KINDS+); WAITS and WAITS_TURN the newest
   ;; session.waits mark's question and the turn that put it.
   "create table if not exists session_directory (
      session_id text primary key,
      created_at text,
      cwd text,
      parent,
      anchor_turn_id,
      title text,
      prompt text,
      updated_at text not null,
      message_count integer not null default 0,
      source text,
      turn_id text,
      turn_kind text,
      waits text,
      waits_turn text) without rowid"
   ;; In order: the row exists before a kind's own update reaches it.
   (format nil "create trigger if not exists session_directory_follows_the_log
      after insert on events when new.session_id is not null
      begin
        insert into session_directory (session_id, updated_at)
          values (new.session_id, new.occurred_at)
          on conflict (session_id) do update set updated_at = excluded.updated_at;
        update session_directory
           set created_at = new.occurred_at,
               cwd = coalesce(json_extract(new.payload, '$.cwd'), ''),
               parent = json_extract(new.payload, '$.parent'),
               anchor_turn_id = json_extract(new.payload, '$.anchor-turn-id')
         where session_id = new.session_id
           and new.kind = '~a' and new.aggregate_type = 'session';
        update session_directory
           set cwd = coalesce(json_extract(new.payload, '$.cwd'), '')
         where session_id = new.session_id
           and new.kind = '~a' and new.aggregate_type = 'session';
        update session_directory set title = ~a
         where session_id = new.session_id and new.kind = '~a';
        update session_directory
           set prompt = coalesce(prompt, ~a), message_count = message_count + 1
         where session_id = new.session_id and new.kind = '~a';
        update session_directory set message_count = message_count + 1
         where session_id = new.session_id and new.kind = '~a';
        update session_directory set source = ~a
         where session_id = new.session_id and new.kind = '~a' and source is null;
        update session_directory set turn_id = new.turn_id, turn_kind = new.kind
         where session_id = new.session_id and new.kind in (~{'~a'~^, ~});
        update session_directory
           set waits = coalesce(json_extract(new.payload, '$.question'), ''),
               waits_turn = new.turn_id
         where session_id = new.session_id and new.kind = '~a';
        update session_directory set prompt = null
         where session_id = new.session_id and ~a;
      end"
           +kind-session-created+ +kind-session-moved+
           (directory-text-sql "new.payload" "$.title") +kind-session-title+
           (directory-prompt-sql "new.payload") +kind-input-committed+
           +kind-assistant-completed+
           (directory-text-sql "new.invocation_json" "$.source.kind") +kind-turn-started+
           +directory-turn-kinds+ +kind-session-waits+
           (directory-retract-to-root-sql "new"))
   ;; The fill: every field as the trigger would have left it. The empty
   ;; directory is asked once, before the walk, so an open whose directory
   ;; stands reads one page of it.
   (format nil "insert or ignore into session_directory
        (session_id, created_at, cwd, parent, anchor_turn_id, title, prompt,
         updated_at, message_count, source, turn_id, turn_kind, waits, waits_turn)
      select session_id, occurred_at,
             coalesce((select json_extract(v.payload, '$.cwd') from events v
                        where v.session_id = events.session_id and v.kind = '~a'
                        order by v.log_position desc limit 1),
                      json_extract(payload, '$.cwd'), ''),
             json_extract(payload, '$.parent'), json_extract(payload, '$.anchor-turn-id'),
             (select ~a from events t
               where t.session_id = events.session_id and t.kind = '~a'
               order by t.log_position desc limit 1),
             (select ~a from events i
               where i.session_id = events.session_id and i.kind = '~a'
                 and ~a is not null
                 and i.log_position > coalesce((select max(r.log_position) from events r
                                                 where r.session_id = events.session_id
                                                   and ~a), 0)
               order by i.log_position limit 1),
             (select u.occurred_at from events u
               where u.session_id = events.session_id
               order by u.log_position desc limit 1),
             (select count(*) from events m
               where m.session_id = events.session_id and m.kind in ('~a', '~a')),
             (select ~a from events s
               where s.session_id = events.session_id and s.kind = '~a'
               order by s.log_position limit 1),
             (select n.turn_id from events n
               where n.session_id = events.session_id and n.kind in (~{'~a'~^, ~})
               order by n.log_position desc limit 1),
             (select n.kind from events n
               where n.session_id = events.session_id and n.kind in (~:*~{'~a'~^, ~})
               order by n.log_position desc limit 1),
             (select coalesce(json_extract(w.payload, '$.question'), '') from events w
               where w.session_id = events.session_id and w.kind = '~a'
               order by w.log_position desc limit 1),
             (select w.turn_id from events w
               where w.session_id = events.session_id and w.kind = '~:*~a'
               order by w.log_position desc limit 1)
        from events
       where kind = '~a' and aggregate_type = 'session'
         and not exists (select 1 from session_directory)"
           +kind-session-moved+
           (directory-text-sql "t.payload" "$.title") +kind-session-title+
           (directory-prompt-sql "i.payload") +kind-input-committed+
           (directory-prompt-sql "i.payload") (directory-retract-to-root-sql "r")
           +kind-input-committed+ +kind-assistant-completed+
           (directory-text-sql "s.invocation_json" "$.source.kind") +kind-turn-started+
           +directory-turn-kinds+ +kind-session-waits+
           +kind-session-created+))
  "The directory's table, the trigger that keeps it, and its fill from the log.")

(defparameter *events-schema*
  `("pragma journal_mode = wal"
    "pragma synchronous = normal"
    ;; One SQLITE_BUSY must never be terminal: without a timeout a single
    ;; cross-process collision (a trial child, an attach probe) fails the
    ;; statement immediately and, on COMMIT, wedges the transaction.
    "pragma busy_timeout = 5000"
    ,(format nil "create table if not exists events (~{~a~^, ~})"
             (loop for (column nil type) in +event-row-columns+
                   collect (format nil (if (eq type :sha256)
                                           "~a text not null check (~:*~a glob ~
                                            'sha256:[0-9A-Fa-f]*' and length(~:*~a) = 71)"
                                           "~a ~a")
                                   column type)))
    "create table if not exists gateway_sequences (
       session_id text primary key,
       sequence integer not null check (sequence >= 0))"
    "create index if not exists events_by_session
       on events (session_id, log_position)"
    ;; Turn-scoped lookups (turn.started input recovery, the boot-resume
    ;; capture guard's bounding subquery) must not scan the whole log.
    "create index if not exists events_by_turn
       on events (turn_id, kind)"
    ;; Kind-scoped session scans (capture projections, parent composition)
    ;; run on the fact-commit hot path; without this they walk every
    ;; session row.
    "create index if not exists events_by_session_kind
       on events (session_id, kind, log_position)"
    ;; Reads scoped by kind alone (the pending-queue pass) walk that kind's
    ;; rows. Without this index, a newest-first scan of the whole log for one
    ;; kind would hold the store lock, and the shell's attach behind it, for
    ;; the duration.
    "create index if not exists events_by_kind
       on events (kind, log_position)"
    ;; Asked-turn projections exclude reflection completions without walking
    ;; every completion's payload on every read.
    ,(format nil "create index if not exists events_completed_by_recorder
       on events (kind, json_extract(payload, '$.recorder'), turn_id)
       where kind = '~a'" +kind-turn-completed+)
    "create unique index if not exists events_by_aggregate_sequence
       on events (aggregate_type, aggregate_id, aggregate_sequence)"
    "create unique index if not exists turn_started_by_command
       on events (aggregate_type, aggregate_id, command_id)
       where kind = 'turn.started'"
    "create unique index if not exists one_terminal_event_per_turn
       on events (turn_id)
       where kind in ('turn.completed', 'turn.failed', 'turn.cancelled')"
    "create unique index if not exists one_session_created
       on events (aggregate_type, aggregate_id)
       where kind = 'session.created'"
    ,@*open-turns-schema*
    ,@*session-directory-schema*)
  "Canonical event envelope schema, the open turns and the session directory
with it.")

(defparameter +session-kind-index+ "events_by_session_kind"
  "The index a read over a fork chain (SESSION-CHAIN-SCOPE) names for EVENTS
:INDEX: each segment is a session seek. Unnamed, a one-kind read is driven
from events_by_kind across every session's rows of that kind — 80 ms against
3 on a 313K-row log, and a random read per row when cold.")

;; Replace a pre-envelope events table rather than guessing its semantics;
;; the projections it kept go with it. A log that predates the open turns
;; derives them here, once: a statement in the schema would ask the log again
;; at every open, and an empty set is the ordinary answer. A directory kept
;; by a trigger other than the one the schema writes goes, with that trigger,
;; and the schema's fill derives it again from the log: the schema creates a
;; trigger only where none stands, so a directory from before a column it
;; keeps now (SOURCE) or before its prompt rule changed would never learn
;; either. SQLite keeps a trigger's text as written from its name on.
(pushnew (lambda (handle)
           (flet ((definition (type name)
                    (sqlite:execute-single
                     handle "select sql from sqlite_master where type = ? and name = ?"
                     type name)))
             (let* ((sql (definition "table" "events"))
                    (trigger (definition "trigger" "session_directory_follows_the_log"))
                    (ours (find "session_directory_follows_the_log" *session-directory-schema*
                                :test #'search)))
               (cond ((null sql))
                     ((notevery (lambda (column) (search column sql))
                                '("log_position" "aggregate_type" "contract_hash"
                                  "invocation_json"))
                      (sqlite:execute-non-query handle "drop table events")
                      (sqlite:execute-non-query handle "drop table if exists open_turns")
                      (sqlite:execute-non-query handle "drop table if exists session_directory"))
                     ((null (definition "table" "open_turns"))
                      (sqlite:execute-non-query handle +open-turns-table+)
                      (sqlite:execute-non-query handle +open-turns-fill+)))
               (unless (and trigger (search (subseq ours (search "after insert" ours)) trigger))
                 (sqlite:execute-non-query
                  handle "drop trigger if exists session_directory_follows_the_log")
                 (sqlite:execute-non-query handle "drop table if exists session_directory"))
               ;; LOG_POSITION is the table's INTEGER PRIMARY KEY, the rowid
               ;; itself: an index on it was one more b-tree every append
               ;; wrote and no read needed.
               (when (definition "index" "events_by_log_position")
                 (sqlite:execute-non-query handle "drop index events_by_log_position")))))
         *schema-prepare-hooks*)
(setf *schema* *events-schema*)

;;; --- payload codec ------------------------------------------------------

(defun payload-object (payload-plist)
  "PAYLOAD-PLIST, a keyword plist, as the JSON object a row stores: each key downcased."
  (apply #'make-json-object (loop for (key value) on payload-plist by #'cddr
                                  collect (string-downcase (symbol-name key)) collect value)))

(defun present-plist (&rest plist)
  "PLIST without the pairs whose value is NIL: how a payload leaves out an
unknown field instead of writing it as false."
  (loop for (key value) on plist by #'cddr
        when value append (list key value)))

;;; --- hash and time ------------------------------------------------------

;;; The digest is OpenSSL's: the image loads libcrypto at every start for TLS
;;; (cl+ssl), and it hashes on the CPU's SHA instructions. Every fact a replay
;;; sends is verified.
(sb-alien:define-alien-routine ("EVP_sha256" %evp-sha256) sb-sys:system-area-pointer)
(sb-alien:define-alien-routine ("EVP_Digest" %evp-digest) sb-alien:int
  (data sb-sys:system-area-pointer) (count sb-alien:size-t) (md sb-sys:system-area-pointer)
  (size sb-sys:system-area-pointer) (type sb-sys:system-area-pointer)
  (engine sb-sys:system-area-pointer))

(defun sha256-text (text &aux (octets (if (stringp text) (utf-8-octets text) (coerce text 'octets)))
                              (digest (make-array 32 :element-type '(unsigned-byte 8)))
                              (out (replace (make-string 71) "sha256:")))
  "Return `sha256:` plus lowercase hex of TEXT's UTF-8 bytes — TEXT a string,
or those bytes themselves."
  (sb-sys:with-pinned-objects (octets digest)
    (unless (= 1 (%evp-digest (sb-sys:vector-sap octets) (length octets)
                              (sb-sys:vector-sap digest) (sb-sys:int-sap 0)
                              (%evp-sha256) (sb-sys:int-sap 0)))
      (error "OpenSSL could not digest ~d bytes" (length octets))))
  (loop for byte across digest
        for at from 7 by 2
        do (setf (char out at) (char "0123456789abcdef" (ash byte -4))
                 (char out (1+ at)) (char "0123456789abcdef" (logand byte 15))))
  out)

(defun short-digest (text &optional (digits 8))
  "DIGITS hex characters of TEXT's SHA-256: a name for it, not a proof."
  (subseq (sha256-text (or text "")) 7 (+ 7 digits)))

;;; Every row read recomputes its contract hash, and a log holds a few dozen
;;; contracts: the FORMAT and the SHA-256 were paid per row.
(defvar *contract-hashes* (make-hash-table :test #'equal :synchronized t)
  "(aggregate-type kind schema-version) -> its contract hash.")

(defun contract-hash-for (aggregate-type kind schema-version
                          &aux (key (list aggregate-type kind schema-version)))
  "Fingerprint the stable event contract identity, not mutable payload data."
  (alexandria:ensure-gethash key *contract-hashes*
    (sha256-text (format nil "~a|~a|~d" aggregate-type kind schema-version))))

(defun iso-now ()
  "UTC ISO-8601 with millisecond precision."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (iso-time (+ sec (/ usec 1000000)))))

(defun iso-8601-p (value)
  (and (stringp value)
       (>= (length value) 20)
       (char= #\T (char value 10))
       (char= #\Z (char value (1- (length value))))))

;;; --- conditions ---------------------------------------------------------

(define-condition event-log-error (store-error) ())

(define-errors (event-log-error) ((:export))
  (event-sequence-conflict (session-id expected actual)
   "event sequence conflict for session ~a: expected ~a, actual ~a" session-id expected actual)
  (event-id-conflict (event-id) "event id ~a already exists with different content" event-id)
  (payload-hash-mismatch (seq stored computed)
   "event seq ~a: payload hash ~a does not match payload ~a" seq stored computed)
  (contract-hash-mismatch (seq stored computed)
   "event seq ~a: contract hash ~a does not match ~
    the row's aggregate_type|kind|schema_version (~a)"
   seq stored computed)
  (event-kind-not-writable (kind)
   "kind ~a must be written by its aggregate owner, not record-event" kind))

;;; --- writes -------------------------------------------------------------

(defun reseed-random-states ()
  "Fresh entropy for this process: a saved core carries the dumping
process's random states, so without this every launch of the same image
would walk the same id sequence — two launches minting the same durable
id."
  ;; Runs from SB-EXT:*INIT-HOOKS* on core start; the launch cache made the
  ;; hazard live (many launches of one dumped image), but the release binary
  ;; itself had it latently.
  (setf *random-state* (make-random-state t))
  (when (boundp '*durable-id-random-state*)
    (setf *durable-id-random-state* (make-random-state t))))

(pushnew 'reseed-random-states sb-ext:*init-hooks*)

(defvar *durable-id-random-state* (make-random-state t)
  "Random state seeded once per image, rather than once per durable ID.")

(defvar *durable-id-lock* (bt2:make-lock :name "durable-id-generator")
  "Protects *DURABLE-ID-RANDOM-STATE* for IDs allocated outside the event writer.")

(defun make-durable-id (prefix)
  (bt2:with-lock-held (*durable-id-lock*)
    (format nil "~a-~36r-~36r" prefix (get-universal-time)
            (random (expt 36 10) *durable-id-random-state*))))
(defun current-aggregate-sequence (aggregate-type aggregate-id)
  (or (events :aggregate-type aggregate-type :aggregate-id aggregate-id
              :as :value :columns '("max(aggregate_sequence)"))
      0))

(defun allocate-gateway-sequence (session-id)
  "Durably allocate one per-peer stream epoch seed for SESSION-ID."
  ;;
  ;;   Each peer's frame stream is sequenced in memory; only the first frame of a
  ;;   peer's attachment consumes a durable allocation. This is intentionally
  ;;   separate from the event log — live-only deltas are not facts — and the
  ;;   table survives gateway restarts so no two attachments ever share a seed.
  (check-type session-id session-id)
  (with-store-transaction (handle)
    (sqlite:execute-non-query
     handle
     "insert into gateway_sequences (session_id, sequence) values (?, 1)
      on conflict(session_id) do update set sequence = sequence + 1"
     session-id)
    (sqlite:execute-single
     handle "select sequence from gateway_sequences where session_id = ?" session-id)))

(defun session-created-fact-p (session-id)
  "True when the store already holds SESSION.CREATED for SESSION-ID."
  ;; The kind is spelled in the statement, never bound: partial indexes name
  ;; kind, so SQLite prepares a statement binding it again at every run, and
  ;; this probe gates every append.
  (events :session-id session-id
          :where (list (load-time-value
                        (format nil "kind = '~a'" +kind-session-created+)))
          :as :exists))

(defun existing-event-row (event-id)
  (events :event-id event-id :as :row
          :columns '("log_position" "aggregate_type" "aggregate_id" "session_id"
                     "kind" "payload" "aggregate_sequence" "contract_hash"
                     "invocation_json")))

(defun same-event-row (existing event-id aggregate-type aggregate-id session-id kind
                       payload-json contract-hash invocation-json
                       &optional (sequence nil sequence-p))
  "EXISTING, the EXISTING-EVENT-ROW under EVENT-ID, held against the envelope
about to be written under it — its aggregate, session, kind, payload text,
contract hash, invocation text, and SEQUENCE when the writer names one."
  ;; => (values LOG-POSITION AGGREGATE-SEQUENCE) of the same fact already there;
  ;; a row that differs is EVENT-ID-CONFLICT.
  (let ((existing-sequence (seventh existing)))
    (if (equal (rest existing)
               (list aggregate-type aggregate-id session-id kind payload-json
                     (if sequence-p sequence existing-sequence) contract-hash invocation-json))
        (values (first existing) existing-sequence)
        (error 'event-id-conflict :event-id event-id))))

(defun append-event-row (handle session-id kind payload-plist &rest options
                         &key aggregate-type aggregate-id subject-id turn-id
                         command-id definition-revision (schema-version 1)
                         event-id expected-sequence causation-id correlation-id
                         invocation occurred-at)
  "Append one canonical event envelope inside the caller's transaction."
  ;; OCCURRED-AT is the envelope's clock, UTC ISO-8601: now unless the caller
  ;; states one. A fact recorded after it happened — an exchange folded in
  ;; from another log, a session made from another record — keeps its own
  ;; time, so the log reads in the order things happened, not the order they
  ;; were learned.
  (let ((owner (event-kind-owner kind)))
    (when (and owner (not (eq *event-write-owner* owner)))
      (error 'event-kind-not-writable :kind kind)))
  (when (and occurred-at (not (iso-8601-p occurred-at)))
    (error 'type-error :datum occurred-at
                       :expected-type '(satisfies iso-8601-p)))
  (let* ((aggregate-type (or aggregate-type "session"))
         (aggregate-id (or aggregate-id session-id))
         (subject-id (or subject-id session-id)))
    (when (eq :held (session-fact-gate session-id aggregate-type kind payload-plist options))
      ;; The fact waits on the standby record and is written under the
      ;; session's own identity when its first block lands.
      (return-from append-event-row (values nil nil nil nil)))
    (let* ((payload-json (handler-case
                             (encode-json-object (payload-object payload-plist))
                           (error (e)
                             (error "event payload failed to serialize as JSON: ~a" e))))
           (payload-hash (sha256-text payload-json))
           (contract-hash (contract-hash-for aggregate-type kind schema-version))
           ;; canonical typed invocation metadata, serialized once at admission
           (invocation-json (when invocation
                              (handler-case (encode-json-object (invocation-json-object invocation))
                                (invocation-error (condition) (error condition))
                                (error (e)
                                  (error "event invocation failed to serialize as JSON: ~a" e)))))
           (resolved-event-id (or event-id (make-durable-id "event"))))
      ;; An EVENT-ID already in the log answers the fact there when it is this
      ;; one (its sequence, and T as the fourth value) and is EVENT-ID-CONFLICT
      ;; when it is not. Without EXPECTED-SEQUENCE the write is one statement,
      ;; the aggregate's next sequence taken inside it.
      (labels ((same (existing)
                 (multiple-value-bind (log-position existing-sequence)
                     (same-event-row existing resolved-event-id aggregate-type aggregate-id
                                     session-id kind payload-json contract-hash invocation-json)
                   (values log-position existing-sequence resolved-event-id t)))
               (insert (sequence)
                 (multiple-value-bind (position sequence)
                     (insert-event-row
                      handle aggregate-type aggregate-id session-id subject-id turn-id
                      command-id resolved-event-id sequence kind schema-version
                      definition-revision payload-json payload-hash contract-hash
                      invocation-json causation-id correlation-id (or occurred-at (iso-now)))
                   (if position
                       (values position sequence resolved-event-id nil)
                       (same (existing-event-row resolved-event-id))))))
        (if expected-sequence
            (if-let (existing (existing-event-row resolved-event-id))
              (same existing)
              (let ((actual (current-aggregate-sequence aggregate-type aggregate-id)))
                (unless (= actual expected-sequence)
                  (error 'event-sequence-conflict :session-id session-id
                         :expected expected-sequence :actual actual))
                (insert (1+ actual))))
            (insert nil))))))

(defun append-session-created-row (handle session-id &key cwd parent
                                                       anchor-turn-id
                                                       anchor-log-position
                                                       command-id occurred-at)
  "Append SESSION.CREATED inside HANDLE's transaction and return its event
id."
  ;; The one place the fact's payload and envelope are written: CREATE-SESSION
  ;; mints through here, and so does MATERIALIZE-STANDBY-SESSION.
  ;;
  ;; ANCHOR-LOG-POSITION is the parent's whole-log watermark the fork froze. A
  ;; caller that read it at mint time passes it, so a fork materialized later
  ;; still composes exactly what it would have then; unstated, it is read
  ;; here.
  (let* ((command-id (or command-id (make-durable-id "command")))
         (created-at (or occurred-at (iso-now)))
         (anchor-log-position
           (and anchor-turn-id
                (or anchor-log-position (session-log-watermark parent)))))
    (with-event-owner 'create-session
      (append-event-row
       handle session-id +kind-session-created+
       (nconc (list :id session-id
                    :cwd (or cwd "")
                    :parent parent
                    :created-at created-at)
              (when anchor-turn-id
                (list :anchor-turn-id anchor-turn-id
                      :anchor-log-position anchor-log-position)))
       :schema-version (if anchor-turn-id 2 1)
       :command-id command-id
       :invocation (default-invocation "session.create" command-id :source "kernel")
       :event-id (make-durable-id "event")
       :occurred-at created-at
       :expected-sequence (current-aggregate-sequence "session" session-id)
       :causation-id (when parent
                       (events :session-id parent :kind +kind-session-created+
                               :as :value :columns '("event_id")))
       :correlation-id command-id))))

(defun %materialize-standby-session (handle session-id cwd parent anchor-turn-id)
  (when-let (record (standby-session-record session-id))
    (cond
      ((session-created-fact-p session-id)
       ;; The identity landed by another road; the record is stale.
       (forget-standby-session session-id)
       nil)
      (t
       (let* ((cwd (or cwd record.cwd ""))
              (parent (or parent record.parent))
              (anchor-turn-id (or anchor-turn-id
                                  record.anchor-turn-id)))
         ;; A standby parent goes first: the child's fact has to name a
         ;; parent the log holds. A session a fork composes is durable with
         ;; the fork — lineage is why it exists, not an idle shell.
         (when (and parent (on-standby-p parent))
           (%materialize-standby-session handle parent nil nil nil))
         (append-session-created-row
          handle session-id :cwd cwd :parent parent :anchor-turn-id anchor-turn-id
          :anchor-log-position record.anchor-log-position)
         ;; Whatever waited on the identity is written under it now, oldest
         ;; first: the facts happened in that order, and the log says so.
         (dolist (fact (take-standby-session-facts session-id))
           (apply #'append-event-row handle session-id
                  (getf fact :kind) (getf fact :payload) (getf fact :options)))
         (forget-standby-session session-id)
         t)))))

(defun materialize-standby-session (session-id &key handle cwd parent anchor-turn-id)
  "Make SESSION-ID durable: write the identity its mint described and the
facts that waited on it, in order."
  ;; HANDLE runs that inside the caller's transaction — the form every block
  ;; writer uses, so identity and first block commit as one — and without one
  ;; this opens its own.
  ;;
  ;; CWD, PARENT and ANCHOR-TURN-ID override what the mint recorded, for the
  ;; fields they name. A standby parent is materialized first.
  ;;
  ;; => T when this call wrote the identity, NIL when the session was durable
  ;; already or was never standing by. ADOPT-EVENT stays strict: an adopted
  ;; envelope presupposes the session exists.
  (check-type session-id session-id)
  (with-store-transaction (handle :within handle)
    (%materialize-standby-session handle session-id cwd parent anchor-turn-id)))

(defun record-event (session-id kind payload-plist &rest options
                     &key &allow-other-keys)
  "Append one unowned canonical event."
  ;; Lifecycle and tool kinds use their owner. OPTIONS are APPEND-EVENT-ROW's
  ;; keywords, forwarded unchanged.
  (check-strings session-id kind)
  (when (event-kind-owner kind)
    (error 'event-kind-not-writable :kind kind))
  (with-store-transaction (handle)
     (apply #'append-event-row handle session-id kind payload-plist options)))

;;; --- reads --------------------------------------------------------------
;;; The envelope's 19 columns are stated once, here. The select list, the row
;;; zip and MATERIALIZE-EVENT's initargs are all derived from this one table,
;;; so a column cannot be added to the schema and forgotten in a reader.

(defparameter +insert-event-sql+
  (let ((columns (mapcar #'first (rest +event-row-columns+))))
    (flet ((parameter (column)
             (1+ (position column columns :test #'string=))))
      (format nil "insert into events (~{~a~^, ~}) values (~{~a~^, ~})
                   on conflict (event_id) do nothing
                   returning log_position, aggregate_sequence"
              columns
              (loop for column in columns
                    for n from 1
                    collect (if (string= column "aggregate_sequence")
                                (format nil "coalesce(?~d, coalesce((select max(aggregate_sequence) ~
                                             from events where aggregate_type = ?~d ~
                                             and aggregate_id = ?~d), 0) + 1)"
                                        n (parameter "aggregate_type")
                                        (parameter "aggregate_id"))
                                (format nil "?~d" n))))))
  "INSERT-EVENT-ROW's statement: every envelope column but LOG_POSITION,
which is the store's own. An unstated AGGREGATE_SEQUENCE is the aggregate's
next, read inside the statement; an EVENT_ID already in the log writes
nothing and returns no row.")

(defun insert-event-row (handle &rest columns)
  "Write one envelope row inside the caller's transaction: COLUMNS are the
eighteen values in the statement's column order, AGGREGATE_SEQUENCE NIL for
the aggregate's next."
  ;; => (values LOG-POSITION AGGREGATE-SEQUENCE), or NIL when EVENT_ID is in
  ;; the log already: nothing is written, and the caller holds the row there
  ;; against its own (SAME-EVENT-ROW).
  ;;
  ;; The row is read back through EVENT-ROW-TO-INSTANCE before the transaction
  ;; commits, and that instance is what the store remembers once the commit
  ;; lands (REMEMBER-COMMITTED-EVENT): a row no reader could hydrate fails the
  ;; write instead of landing, and a consumer draining the memory holds the
  ;; very object PAGE-EVENTS would have built from the row.
  (destructuring-bind (&optional position sequence)
      (first (apply #'sqlite:execute-to-list handle +insert-event-sql+ columns))
    (when position
      (let* ((columns (copy-list columns))
             ;; the payload text's length, the measure the ring's budget is in
             (size (length (nth (load-time-value
                                 (1- (position :payload +event-row-columns+ :key #'second)))
                                columns))))
        (setf (nth (load-time-value
                    (1- (position :aggregate-sequence +event-row-columns+ :key #'second)))
                   columns)
              sequence)
        (let ((event (event-row-to-instance (cons position columns))))
          (after-commit (lambda () (remember-committed-event event size))))
        (values position sequence)))))

(defun decode-event-payload-object (json &aux (value (nlk:decode-json json)))
  "The payload object one events row carries."
  ;; Strict on purpose: the log is written by this image alone, so a payload
  ;; that is not a JSON object is corruption, not a wire shape to be lenient
  ;; about.
  (unless (hash-table-p value)
    (error "event payload is not a JSON object"))
  value)

(defun events (&key kind session-id turn-id command-id event-id log-position
                    aggregate-type aggregate-id after before where index
                    (order :oldest) limit (as :instances) columns
                    column-arguments group-by)
  "The one read over the event log."
  ;; Every reader — a count, an existence probe, a payload, a page of hydrated
  ;; events, a watermark — is one call here, so the table's 22 hand-written
  ;; SELECTs became one generated statement and the index coverage of a scope
  ;; is checkable in one place.
  ;;
  ;; Scope: each non-NIL key among SESSION-ID, TURN-ID, COMMAND-ID, EVENT-ID,
  ;; LOG-POSITION, AGGREGATE-TYPE and AGGREGATE-ID contributes one `column =
  ;; ?'; KIND is a string (`kind = ?') or a list (`kind in (...)'); AFTER and
  ;; BEFORE bound LOG_POSITION strictly (> and <). WHERE is the escape for a
  ;; predicate no key spells — (SQL . ARGUMENTS), AND-appended verbatim;
  ;; inside it the outer row is `events' (a correlated subquery aliases its
  ;; own table and reads events.turn_id for the outer one). INDEX names the
  ;; index the statement must read through (`from events indexed by'): the one
  ;; way SQLite takes a partial index it would not choose unaided — no
  ;; statistics, a literal kind in WHERE — and an index the scope cannot use
  ;; is a statement error, never a silent scan. ORDER is :oldest or :newest by
  ;; LOG_POSITION; LIMIT caps the rows. GROUP-BY groups a column projection
  ;; by SQL expressions or one-based column numbers before ordering/limiting;
  ;; aggregate COLUMNS then return one row per group, not the whole scope.
  ;;
  ;; AS is the projection. :instances and :instance hydrate through
  ;; EVENT-ROW-TO-INSTANCE, the hash-verified reader, and :facts verifies the
  ;; same hashes and hydrates nothing (STORED-FACTs, the payload left as its
  ;; octets) — the two projections that pay SHA-256 per row, which is why the
  ;; cheap ones exist beside them. :payloads
  ;; and :payload decode the payload column alone (DECODE-EVENT-PAYLOAD-OBJECT).
  ;; :rows and :row read COLUMNS, a list of column names or SQL expressions
  ;; ("max(log_position)" is a column here), as one list per row; :value is
  ;; the first column of the first row. :count answers an integer, :exists a
  ;; boolean. The singular projections take the first row in ORDER and answer NIL
  ;; when nothing matches; a caller that wants 0 says (or ... 0) itself, as the
  ;; old readers did.
  ;;
  ;; COLUMN-ARGUMENTS binds the placeholders a COLUMNS expression carries — a
  ;; computed column that needs one, like the search read's snippet window —
  ;; and they bind FIRST, in the order the SELECT list spells them, because a
  ;; statement's parameters bind in the order they appear in it. A caller
  ;; whose columns carry no placeholder leaves it empty and nothing moves.
  ;;
  ;; Runs on the store's own lock and connection, inside or outside a
  ;; transaction: the lock is recursive and the connection is the one the
  ;; transaction holds, so a reader composed into a writer sees the writer's
  ;; uncommitted rows exactly as the hand-written in-transaction twins did.
  (let ((clauses '())
        ;; COLUMN-ARGUMENTS bind first, and `arguments' is built backwards
        ;; (every clause pushes; the final APPLY reverses), so they enter
        ;; reversed here and come out in the order COLUMNS spells them.
        (arguments (reverse column-arguments)))
    (flet ((clause (sql &rest values)
             (push sql clauses)
             (dolist (value values) (push value arguments))))
      (loop for column in '("session_id" "turn_id" "command_id" "event_id" "log_position"
                            "aggregate_type" "aggregate_id")
            for value in (list session-id turn-id command-id event-id log-position
                               aggregate-type aggregate-id)
            when value
              do (clause (concatenate 'string column " = ?") value))
      (cond ((null kind))
            ((listp kind)
             (apply #'clause (format nil "kind in (~{?~*~^, ~})" kind) kind))
            (t (clause "kind = ?" kind)))
      (when after (clause "log_position > ?" after))
      (when before (clause "log_position < ?" before))
      (when where (apply #'clause where)))
    (when (and (member as '(:row :rows :value)) (null columns))
      (error "events: the ~s projection needs :columns" as))
    (when (and group-by (not (member as '(:row :rows :value))))
      (error "events: :group-by needs a column projection, not ~s" as))
    (let* ((limit (cond ((eq as :count) nil)
                        ((member as '(:instance :payload :row :value :exists)) 1)
                        (t limit)))
           (sql (concatenate
                 'string
                 (ecase as
                   ;; The payload arrives as its stored UTF-8 bytes, the form
                   ;; its hash is over and the JSON reader walks. Read as
                   ;; text, it would be decoded to characters and encoded
                   ;; back per row.
                   ((:instance :instances)
                    (load-time-value (format nil "select ~{~a~^, ~} "
                                             (substitute "cast(payload as blob)" "payload"
                                                         (mapcar #'first +event-row-columns+)
                                                         :test #'equal))))
                   ((:payload :payloads) "select cast(payload as blob) ")
                   (:facts
                    (load-time-value
                     (format nil "select ~{~a~^, ~} "
                             '("log_position" "session_id" "turn_id" "command_id"
                               "event_id" "kind" "occurred_at" "cast(payload as blob)"
                               "payload_hash" "aggregate_type" "schema_version"
                               "contract_hash"))))
                   ((:row :rows :value)
                    (format nil "select ~{~a~^, ~} " columns))
                   (:count "select count(*) ")
                   (:exists "select 1 "))
                 (if index
                     (format nil "from events indexed by ~a " index)
                     "from events ")
                 (when clauses
                   (format nil "where ~{~a~^ and ~} " (reverse clauses)))
                 (when group-by
                   (format nil "group by ~{~a~^, ~} " group-by))
                 (unless (member as '(:count :exists))
                   (if (eq order :newest)
                       "order by log_position desc "
                       "order by log_position "))
                 ;; The limit is statement text, not a bound parameter: a
                 ;; bound LIMIT costs SQLite ~12 us per execution on a
                 ;; one-row read (measured: 17.5 us against 6 us), and the
                 ;; append path takes three of these reads per event.
                 (when limit
                   (check-type limit (integer 1))
                   (format nil "limit ~d" limit))))
           (rows (with-store-handle (handle)
                   (apply #'sql-rows handle sql
                          (reverse arguments)))))
      (ecase as
        (:instances (mapcar #'event-row-to-instance rows))
        (:instance (and rows (event-row-to-instance (first rows))))
        (:facts (mapcar #'row-stored-fact rows))
        (:payloads (mapcar (lambda (row) (decode-event-payload-object (first row)))
                           rows))
        (:payload (and rows (decode-event-payload-object (first (first rows)))))
        (:rows rows)
        (:row (first rows))
        ((:value :count) (first (first rows)))
        (:exists (not (null rows)))))))

;;; --- adoption -----------------------------------------------------------
;;; The one write that keeps another image's envelope. RECORD-EVENT and the
;;; owners mint an event here; ADOPT-EVENT lands one minted elsewhere — a
;;; restore from a backup, a sync from another device — with its identity,
;;; sequence, time and provenance as recorded there. The local position and
;;; the two derived columns are this store's own.

;;; What a sync ships is exactly this list, in this order; what ADOPT-EVENT
;;; takes is a plist over its initargs.
(defparameter +event-envelope-columns+
  (remove-if (lambda (column)
               (member (first column)
                       '("log_position" "payload_hash" "contract_hash")
                       :test #'string=))
             +event-row-columns+)
  "The columns that make an envelope portable: +EVENT-ROW-COLUMNS+ minus the
three every store derives for itself — LOG_POSITION is the local order, and
the two hashes are recomputed at adoption.")

(define-error event-envelope-invalid (event-log-error) (detail)
  (:export)
  (:report "event envelope cannot be adopted: ~a" detail))

(defun adopt-event (row &key within)
  "Append an envelope another image recorded -- inside the transaction whose
handle is WITHIN, when given (an import lands a whole file as one)."
  ;; ROW is a plist over the initargs of +EVENT-ENVELOPE-COLUMNS+: the row
  ;; keeps its EVENT-ID, AGGREGATE-SEQUENCE, OCCURRED-AT and invocation as
  ;; recorded there, LOG-POSITION is this store's own, and PAYLOAD-HASH and
  ;; CONTRACT-HASH are derived again here — so nothing can be adopted whose
  ;; payload does not hash to what the read side will verify on every
  ;; hydration.
  ;;
  ;; No owner check: the fact was written by its owner where it happened, and
  ;; this is the same fact arriving. What still holds: SESSION.CREATED first
  ;; for its session; an EVENT-ID already present with identical content is a
  ;; no-op; a colliding one is EVENT-ID-CONFLICT; and the aggregate's sequence
  ;; only moves forward — a row at or below the aggregate's current sequence
  ;; is EVENT-SEQUENCE-CONFLICT, so an out-of-order restore is loud instead of
  ;; silently reshuffled. A malformed row is EVENT-ENVELOPE-INVALID before any
  ;; write.
  ;;
  ;; => (values LOG-POSITION ADOPTED-P), ADOPTED-P NIL when the row already was.
  (flet ((invalid (control &rest arguments)
           (error 'event-envelope-invalid
                  :detail (apply #'format nil control arguments))))
    (destructuring-bind (&key aggregate-type aggregate-id session-id event-id
                           ((:aggregate-sequence sequence)) kind schema-version payload
                           occurred-at subject-id turn-id command-id definition-revision
                           invocation-json causation-id correlation-id &allow-other-keys)
        row
      (loop for name in '("aggregate_type" "aggregate_id" "session_id" "event_id" "kind")
            for value in (list aggregate-type aggregate-id session-id event-id kind)
            unless (and (stringp value) (plusp (length value)))
              do (invalid "~a must be a non-empty string" name))
      (loop for (name value) on (list "aggregate_sequence" sequence "schema_version" schema-version) by #'cddr
            unless (and (integerp value) (plusp value))
              do (invalid "~a must be a positive integer" name))
      (unless (stringp payload)
        (invalid "payload must be JSON text"))
      (handler-case (decode-event-payload-object payload)
        (error (condition)
          (invalid "payload is not a JSON object: ~a" condition)))
      (unless (iso-8601-p occurred-at)
        (invalid "occurred_at must be UTC ISO-8601"))
      (loop for key in '(:subject-id :turn-id :command-id :definition-revision
                         :invocation-json :causation-id :correlation-id)
            for value in (list subject-id turn-id command-id definition-revision
                               invocation-json causation-id correlation-id)
            unless (or (null value) (stringp value))
              do (invalid "~(~a~) must be a string or absent" key))
      (with-store-transaction (handle :within within)
        (unless (or (string= kind +kind-session-created+)
                    (session-created-fact-p session-id))
          (error 'unknown-session :session-id session-id))
        (let ((contract-hash (contract-hash-for aggregate-type kind schema-version))
              (existing (existing-event-row event-id)))
          ;; The no-op answer is a value, never an exit: a non-local exit out of
          ;; the wrapper's closure skips its commit and leaves BEGIN IMMEDIATE
          ;; open on the connection.
          (if existing
              (values (same-event-row existing event-id aggregate-type aggregate-id
                                      session-id kind payload contract-hash
                                      invocation-json sequence)
                      nil)
              (let ((current (current-aggregate-sequence aggregate-type aggregate-id)))
                (unless (> sequence current)
                  (error 'event-sequence-conflict :session-id session-id
                         :expected (1+ current) :actual sequence))
                (values (insert-event-row
                         handle aggregate-type aggregate-id session-id subject-id
                         turn-id command-id event-id sequence kind
                         schema-version definition-revision payload
                         (sha256-text payload) contract-hash invocation-json
                         causation-id correlation-id occurred-at)
                        t))))))))

(defun session-replay-coverage (session-id &optional watermark)
  "(values EVENT-COUNT MAX-LOG-POSITION) for SESSION-ID, bounded to WATERMARK
when given — the one-aggregate-query covered-prefix recheck (the
event_log.zig sessionReplayCoverage shape) a hydration cache pins itself to:
both numbers must match what the cache recorded or the cache is wrong."
  (check-type session-id session-id)
  (values-list (events :session-id session-id
                       :where (and watermark (list "log_position <= ?" watermark))
                       :as :row
                       :columns '("count(*)" "coalesce(max(log_position), 0)"))))

(defun page-events (&key session-id (after 0) (limit *default-page-size*) (as :instances))
  "Return canonical events after global LOG-POSITION, oldest first, in the
EVENTS projection AS: instances, or the :FACTS a replay carries."
  (check-page-size limit)
  (events :session-id session-id :after after :limit limit :as as))

(defun page-events-before (session-id before limit &key (as :instances))
  "Events strictly below LOG-POSITION BEFORE, newest window, returned
oldest first — the lazy history pager's read: page N is the LIMIT events
just under the previous page's floor. AS as for PAGE-EVENTS."
  (check-type session-id session-id)
  (check-page-size limit)
  (nreverse (events :session-id session-id :before before
                    :order :newest :limit limit :as as)))

(defun verify-row-hashes (log-position payload payload-hash
                          aggregate-type kind schema-version contract-hash)
  "Refuse a row whose payload or contract identity no longer hashes to what
it was written with."
  (flet ((hold (mismatch stored computed)
           (unless (equal computed stored)
             (error mismatch :seq log-position :stored stored :computed computed))))
    (hold 'payload-hash-mismatch payload-hash (sha256-text payload))
    ;; The contract hash binds aggregate_type|kind|schema_version at write
    ;; time; recomputing it on read is what makes schema_version trustworthy
    ;; downstream — the SQL CHECK only proves the column LOOKS like a hash.
    (hold 'contract-hash-mismatch contract-hash (contract-hash-for aggregate-type kind schema-version))))

;;; --- stored facts: a row as the wire carries it ---------------------------
;;; A replay and the history pager carry each fact to a shell as it is kept:
;;; the envelope a frame names and the payload's own JSON. Hydrating an EVENT
;;; for that — the payload decoded, a typed instance made — only for the
;;; frame to encode the payload back would dominate a replay's cost. A fact is
;;; hash-verified like an instance and never decoded.

(defstruct (stored-fact (:constructor make-stored-fact
                            (log-position session-id turn-id command-id event-id
                             kind occurred-at payload))
                        (:copier nil) (:predicate nil))
  "One logged fact as the log keeps it, hash-verified: its envelope and its
PAYLOAD, the UTF-8 octets of the payload's JSON text."
  (log-position 0 :type (integer 0) :read-only t)
  (session-id nil :read-only t)
  (turn-id nil :read-only t)
  (command-id nil :read-only t)
  (event-id nil :read-only t)
  (kind nil :read-only t)
  (occurred-at nil :read-only t)
  (payload nil :type octets :read-only t))

(defun row-stored-fact (row)
  "One row of the :FACTS projection as its STORED-FACT."
  (destructuring-bind (log-position session-id turn-id command-id event-id kind
                       occurred-at payload payload-hash aggregate-type schema-version
                       contract-hash)
      row
    (verify-row-hashes log-position payload payload-hash
                       aggregate-type kind schema-version contract-hash)
    (make-stored-fact log-position session-id turn-id command-id event-id
                      kind occurred-at payload)))

(defmacro define-event-row-reader (&aux (vars (loop for (nil initarg) in +event-row-columns+
                                                    collect (intern (symbol-name initarg)))))
  "Define EVENT-ROW-TO-INSTANCE as straight-line code generated from
+EVENT-ROW-COLUMNS+: one DESTRUCTURING-BIND over the select order, then one
literal initarg list."
  ;; The table stays the single authority — a column added there still cannot
  ;; be forgotten by the reader — but no plist is consed and no GETF is walked
  ;; per row.
  `(defun event-row-to-instance (row)
     "Parse one canonical database row into an EVENT instance."
     (destructuring-bind ,vars row
       (declare (ignorable ,@vars))
       (verify-row-hashes log-position payload payload-hash
                          aggregate-type kind schema-version contract-hash)
       ;; Only the reader's refusal is the payload's fault. Anything else
       ;; raised inside it -- a memory fault in an image already corrupt
       ;; goes on as itself.
       (let ((decoded (handler-case (nlk:decode-json payload)
                        (nlk:json-refusal (e)
                          (error "event log-position ~a: payload is not valid JSON: ~a"
                                 log-position e)))))
         (unless (hash-table-p decoded)
           (error "event log-position ~a: payload must be a JSON object, got ~a"
                  log-position (type-of decoded)))
         ;; MATERIALIZE-EVENT's class, through its kind's constructor
         (funcall (get (or (gethash kind *event-class-registry*) 'unknown-event)
                       'row-constructor)
                  ,@(loop for (nil initarg) in +event-row-columns+
                          for var in vars
                          collect (if (eq initarg :payload) 'decoded var)))))))

(define-event-row-reader)

;;; --- what this process committed, in memory -------------------------------
;;;
;;; The store's writer is this process alone (one connection, the fcntl
;;; lease), so what it committed it also knows: the newest events of each
;;; session stay in memory, and a consumer that follows the log — the
;;; gateway's live publication after every committed fact — takes them from
;;; here instead of paging them back out of SQLite on the turn thread, between
;;; a tool result and the next provider request. Retention: live-only; the
;;; log is the record, and a ring that no longer reaches back to a cursor says
;;; so and the consumer pages.
;;;
;;; The bounds are the store's, not each session's: the newest events across
;;; every ring stay, so a session that stopped committing gives way to the
;;; ones still running.

;;; The consumer drains a session right after every commit, so the rings only
;;; have to cover the facts between two drains of each session committing; a
;;; cursor further back misses and pages.
(defparameter *committed-events-capacity* 64
  "How many committed events the rings retain at most between them.")

;;; The oldest event across the rings goes first until it holds; an event over
;;; it on its own is not retained at all.
(defparameter *committed-events-payload-budget* (* 1024 1024)
  "How much payload text, in characters of its JSON, the rings hold at most
between them — the bound that matters when a tool result runs to
megabytes, where the count alone would pin them all live.")

(defstruct (committed-ring (:constructor make-committed-ring (floor))
                           (:copier nil))
  "The newest events of one session, oldest first, each as (EVENT . SIZE) with
its payload text length, and BYTES the sum over them; FLOOR the log position
under which nothing is retained: every event of the session above it is
here."
  ;; Born knowing the session's newest position it does not hold; afterwards
  ;; the position of the last event evicted.
  (retained '() :type list)
  (bytes 0 :type integer)
  (floor 0 :type integer))

(access (ring committed-ring))

(defun committed-ring-evict (ring &aux (oldest (pop ring.retained)))
  "Let RING's oldest event go: the floor becomes its position."
  (setf ring.floor (event-log-position (car oldest)))
  (decf ring.bytes (cdr oldest)))

(defun committed-ring-push (ring event size)
  "Retain EVENT, SIZE characters of payload text, in RING, newest last; the
bounds are held across every ring after (COMMITTED-RINGS-HOLD)."
  ;; An event over the payload budget on its own is not
  ;; retained, and nothing of RING older than it either: the floor passes it, the
  ;; log is paged for that stretch, and what fits after it is retained again.
  ;; => EVENT.
  (cond
    ((> size *committed-events-payload-budget*)
     (loop while ring.retained
           do (committed-ring-evict ring))
     (setf ring.floor (event-log-position event)))
    (t
     (setf ring.retained (nconc ring.retained (list (cons event size))))
     (incf ring.bytes size)))
  event)

(defun committed-rings-hold (rings)
  "Let the oldest event across RINGS, a table of them, go until their count
and their payload text are both within bounds."
  (loop
    (let ((count 0) (bytes 0) (oldest nil) (oldest-position nil))
      (loop for ring being the hash-values of rings
            when ring.retained
              do (let ((position (event-log-position (car (first ring.retained)))))
                   (incf count (length ring.retained))
                   (incf bytes ring.bytes)
                   (when (or (null oldest) (< position oldest-position))
                     (setf oldest ring oldest-position position))))
      (if (and oldest (or (> count *committed-events-capacity*)
                          (> bytes *committed-events-payload-budget*)))
          (committed-ring-evict oldest)
          (return)))))

(defun remember-committed-event (event size &aux (session-id (event-session-id event))
                                                 (store *store*))
  "Retain EVENT, just committed, SIZE characters of payload text, in its
session's ring, and let the oldest events across the rings go until they
hold."
  ;; Runs from AFTER-COMMIT on the writing thread with the store lock held, so
  ;; a session's ring is born at most once per open and every commit of the
  ;; session passes through in log order; a fact with no session has no ring
  ;; to join.
  (when (and session-id (store-open-p store))
    (let* ((table store.committed)
           (ring (or (sb-ext:with-locked-hash-table (table) (gethash session-id table))
                     ;; One query per session per open, so the first drain
                     ;; after boot hits from the cursor the consumer holds
                     ;; instead of paging once to learn what it already had.
                     (let ((born (make-committed-ring
                                  (events :session-id session-id
                                          :before (event-log-position event)
                                          :as :value
                                          :columns '("coalesce(max(log_position), 0)")))))
                       (sb-ext:with-locked-hash-table (table)
                         (setf (gethash session-id table) born))))))
      (sb-ext:with-locked-hash-table (table)
        (committed-ring-push ring event size)
        (committed-rings-hold table)))))

(defun committed-events-after (session-id cursor &aux (store *store*))
  "=> (values EVENTS MISS): SESSION-ID's committed events above log position
CURSOR, oldest first, out of this process's memory — or NIL and :MISS when
the memory does not reach back to CURSOR (nothing committed since the store
opened, or more than the ring holds), in which case the caller pages the
log. Empty EVENTS with no MISS is an answer: nothing after CURSOR."
  (check-type session-id session-id)
  (check-type cursor log-position)
  (if (store-open-p store)
      (sb-ext:with-locked-hash-table (store.committed)
        (let ((ring (gethash session-id store.committed)))
          (if (or (null ring) (< cursor ring.floor))
              (values nil :miss)
              (values (loop for (event) in ring.retained
                            when (> (event-log-position event) cursor)
                              collect event)
                      nil))))
      (values nil :miss)))

(defun event-count (&key session-id)
  "How many event facts are stored. Cheap enough to call per turn."
  (events :session-id session-id :as :count))

;;; --- turn lifecycle events ----------------------------------------------

(define-event turn-started +kind-turn-started+
  "Admission fact. Input is also stored as TURN.INPUT_COMMITTED."
  ((input :string "input" :default "")
   (parent-turn-id :string "parent-turn-id"
                   :doc "Head turn at admission. NIL = root. Absent on
schema-version 1 rows; the fold's linear fallback chains those to the
immediately preceding TURN.STARTED.")
   (wake :array "wake"
         :doc "What woke a turn no one typed (ADMIT-WAKE-TURN): one object
per exited background evaluation it reads -- eval_id, exit_code, shell_exit.
NIL for every other turn.")))

(define-event turn-completed +kind-turn-completed+
  "Logged by finish-turn after each completed provider round."
  ;; A recorded exchange (RECORD-EXCHANGE-TURN) may carry the facts of the
  ;; work behind the pair -- who recorded it, what that work ran, how long it
  ;; took and what it cost -- which the transcript rests the pair's divider
  ;; on; a completion without them reads every one as NIL.
  ((turn-number :integer "turn-number" :default 0)
   (message-count :integer "message-count" :default 0)
   (recorder :text "recorder")
   (provider :text "provider")
   (model :text "model")
   (elapsed-ms :integer "elapsed-ms")
   (input-tokens :integer "input-tokens")
   (cached-input-tokens :integer "cached-input-tokens")
   (output-tokens :integer "output-tokens")
   (reasoning-tokens :integer "reasoning-tokens")
   ;; T when the turn ended at a round boundary it would otherwise have
   ;; continued past, because a steer was parked (COMPLETE-TURN :STEERED).
   (steered :boolean "steered" :default nil)))

(define-event turn-failed +kind-turn-failed+
  "Terminal fact for an admitted turn whose evolved policy signalled."
  ;; The
  ;; evidence slots are present when a provider failure ended the turn
  ;; (FAIL-TURN :EVIDENCE): debug-only retention, read by a diagnosis of a dead
  ;; session, never rendered as transcript. PROVIDER and MODEL are the
  ;; identity the failing request targeted, as a retry fact names it: the
  ;; Dashboard's error rate per model reads them.
  ((detail :string "detail")
   (status :integer "status")
   (provider :string "provider")
   (model :string "model")
   (content-type :string "content-type")
   (request-sha256 :string "request-sha256")
   (response-body :string "response-body")))

(define-event turn-cancelled +kind-turn-cancelled+
  "Terminal fact for an admitted turn cancelled by its owner."
  ((reason :string "reason" :default "")))

;;; --- message events -----------------------------------------------------

(define-event message-event nil
  "Base class for one logged conversational message."
  ;; Subclassed by role so the reducer dispatches on who spoke rather than
  ;; branching on a string.
  ((role :string "role" :default "")
   ;; The round's message is logged either as a bare string or as the wire
   ;; message object. Reading the object's "content" through JSON-VALUE is what
   ;; keeps a tool-only assistant round out of this string slot: its content is
   ;; JSON null, which shasht reads back as the truthy keyword :NULL.
   (content :from (or (json-value payload :string "message")
                      (json-value payload :string "message" "content"))
            :default "")
   ;; The round's thinking trace, carried in the same durable wire message as
   ;; the content it preceded. Reasoning is persisted history, not stream
   ;; ephemera: the reducer hydrates the streamed thinking row from this.
   (reasoning :string "message" "reasoning_content" :default "")
   ;; The display envelope, or NIL: "images", an image fact for each picture
   ;; the message shows -- those an answer names (exec.lisp ANSWER-IMAGES),
   ;; those sent with a prompt (exec.lisp PROMPT-IMAGES). Retention:
   ;; persisted-history, so a replay draws what the live stream drew.
   (metadata :object "metadata")
   (payload)))

(define-event (user-message :parent message-event) +kind-input-committed+
  "What the operator said, logged before the provider is called."
  ())

(define-event (assistant-message :parent message-event) +kind-assistant-completed+
  "What the provider answered, logged after the loop returns."
  ;; A round that continued a cut attempt (turn.attempt_voided, disposition
  ;; continued) carries the merged message — the sealed prefix plus what the
  ;; continuation streamed — because the provider must see the whole thought
  ;; on the next round. CONTINUED-REASONING-CHARS and CONTINUED-CONTENT-CHARS
  ;; are where the continuation begins in each merged string, so a transcript
  ;; that already shows the sealed block hydrates only the continuation under
  ;; it instead of the prefix twice. NIL on a round that continued nothing.
  ((continued-reasoning-chars :integer "continued-reasoning-chars")
   (continued-content-chars :integer "continued-content-chars")))

(define-event (context-evicted-event :prefix context-evicted) +kind-context-evicted+
  "Durable monotone provider-context history floor."
  ((retained-from-sequence :integer "retained-from-sequence" :default 0)
   (start-sequence :integer "evicted-start-sequence" :default 0)
   (end-sequence :integer "evicted-end-sequence" :default 0)
   (turn-count :integer "evicted-turn-count" :default 0)
   (reason :string "reason" :default "budget_fill")
   (source-watermark :integer "source-watermark" :default 0)
   (retained-turn-count :integer "retained-turn-count" :default 0)
   (message-count :integer "evicted-message-count" :default 0)
   (retained-message-count :integer "retained-message-count" :default 0)
   ;; Operator-eviction percent contract and realized token split (the Zig
   ;; SessionContextEvictionEvent fields); zero on budget_fill facts, which
   ;; have no percent contract.
   (requested-percent :integer "requested-percent" :default 0)
   (realized-percent :integer "realized-percent" :default 0)
   (estimated-tokens :integer "evicted-estimated-tokens" :default 0)
   (retained-estimated-tokens :integer "retained-estimated-tokens" :default 0)))

(define-event (turn-context-evicted-event :prefix turn-context-evicted
               :record (record-turn-context-eviction (watermark (integer 0))))
    +kind-turn-context-evicted+
  "Durable monotone WITHIN-turn window watermarks for one oversized live
turn: the rounds it thinned and the rounds it dropped."
  ;; The session floor stays whole-turn; when the sole retained turn itself
  ;; outgrows the budget, its older tool rounds are thinned first -- their
  ;; results cut to a head that names the entry reading them whole, their
  ;; replayed reasoning gone -- through THINNED, and its oldest rounds leave
  ;; the provider window through WATERMARK only when thinning was not enough.
  ;; Both are round boundaries; a fact that only thins carries the watermark
  ;; the turn already had (0 before any round left). The turn's opening user
  ;; input always survives the fold, so a compiled request can never lose its
  ;; task (the Qwen no-user-query 500 class). Counts are cumulative across
  ;; advances so the stub renders from the newest fact alone. Evidence horizon
  ;; like the floor facts: replay-suppressed, transcript rows stay.
  ((watermark :integer "watermark" :default 0)
   (thinned :integer "thinned" :default 0 :param thinned)
   (start-sequence :integer "evicted-start-sequence" :default 0 :param evicted-start-sequence)
   (round-count :integer "evicted-round-count" :default 0 :param evicted-round-count)
   (message-count :integer "evicted-message-count" :default 0 :param evicted-message-count)
   (reason :string "reason" :default "budget_fill"))
  ;; Counts are cumulative across advances — the fold's stub renders from the
  ;; newest fact alone.
  "Durably advance TURN's within-turn window: the turn's conversation events
at or below WATERMARK (a round boundary) leave the provider window, except
the opening user input the fold always retains, and those at or below
THINNED stand thinned.")

(define-event (turn-context-prompt-event :prefix turn-context-prompt
               :record record-turn-context-prompt)
    +kind-turn-context-prompt+
  "The prompt TURN's conversation runs under, or a change to it. A PIN
starts one: BASE is the system prompt, SECTIONS the harness sections as
[name, text] pairs in their order, TOOLS the tool payload and FLOOR the
history floor it was taken at, and every later request of the conversation
sends exactly those. A change carries what moved since the model was last
told: BASE when the system prompt did, SECTIONS as [name, text] for a
section new or rewritten and [name] for one withdrawn, WITHDRAWN every
pinned tool no longer bound, and TEXT, the <system-reminder> the fold sends
where the fact stands."
  ;; A pin carries no TEXT: everything current rides its prefix. WITHDRAWN
  ;; is the whole set on every change, not a delta, so the newest change
  ;; answers it alone.
  ((pin :boolean "pin")
   (base :string "base")
   (sections :array "sections")
   (tools :array "tools")
   (floor-sequence :integer "floor" :param floor)
   (withdrawn :array "withdrawn")
   (text :string "text"))
  "Durably record a prompt fact of TURN: a PIN of BASE, SECTIONS and TOOLS
taken at FLOOR, or a change to the pinned prompt that TEXT tells the model.")

;;; --- floor lifecycle events ---------------------------------------------
;;; The kernel package lock is an ambient property of whichever image is
;;; running. These two facts make its state replayable lineage: engagement is
;;; recorded at boot, and lowering must name who asked and why. Written only
;;; by their owners in evolve.lisp (ENGAGE-FLOOR / LOWER-FLOOR).

(define-event floor-engaged-event +kind-floor-engaged+
  "The kernel floor was locked."
  ())

(define-event floor-lowered-event +kind-floor-lowered+
  "The kernel floor was unlocked, naming who asked and why."
  ((requested-by :string "requested-by" :default "")
   (reason :string "reason" :default "")))

;;; --- scribe provenance event ----------------------------------
;;; Definition provenance captured at the eval seam: which session defined
;;; what, and the layer commit that carries it. The store keeps provenance;
;;; the layer's git history keeps content. Evidence-horizon material like the
;;; floor facts: durable, queryable, replay-suppressed. Written only by its
;;; owner in scribe.lisp.

(macrolet ((define-definition-event (name kind documentation)
             `(define-event ,name ,kind ,documentation
                ((names :array "names" :default #())
                 (operator :string "operator" :default "")
                 (text :string "text" :default "")
                 (layer :string "layer" :default "")
                 (commit :string "commit" :default "")))))
  (define-definition-event definition-recorded-event +kind-definition-recorded+
    "A defining form the organism successfully evaluated, written to its layer
with the exact source text the model wrote (never a re-print) and committed.")

  ;; The tombstone the context engine's defined: clause honours: a name
  ;; forgotten after it was recorded is no longer surviving state.
  (define-definition-event definition-forgotten-event +kind-definition-forgotten+
    "A forgetting form the organism successfully evaluated — FMAKUNBOUND,
MAKUNBOUND, UNHOOK, REMOVE-METHOD, UNINTERN — whose layer files were
deleted and the drop committed."))

;;; --- tool-call and tool-result events -----------------------------------

(define-event (tool-call-event :record (record-tool-call call-id tool-name arguments))
    +kind-tool-call-started+
  "One tool call the provider requested."
  ;; Logged before execution so the question survives a crash during the tool
  ;; run.
  ((call-id :string "call-id" :default "")
   (tool-name :string "tool-name" :default "")
   (arguments :string "arguments" :default ""))
  "Journal one tool call against an admitted TURN before it executes.")

;;; One list, so the complete decode below and the streaming partial-JSON probe
;;; (SHELL-COMMAND-PREVIEW-FROM-ARGS) can never disagree about which keys count.
(defparameter +tool-command-keys+ '("command" "cmd" "code" "form")
  "The argument keys whose string value is the executable text of a tool
call, in the order both readers try them: the Zig shellCommandString fields
(tool_render.zig) plus form, the key eval's schema names its snippet by.")

;;; --- literal references at display time ------------------------------------
;;; ($ "name") is how an eval call carries dense text beside its form: the
;;; body lives in the same call's strings object and the executing reader
;;; resolves the reference. A reader of the ROW resolves it too — the tool
;;; card, the checkpoint listing, the background band's command preview —
;;; or the row shows a placeholder where the operator needs the command.

(defun display-flatten (text)
  "TEXT with every whitespace or control run collapsed to one space and the
ends trimmed: one line, so a resolved body can stand inside a row."
  (string-trim " " (ppcre:regex-replace-all "[\\x00-\\x20\\x7F]+" text " ")))

(defun deref-literal-refs (text lookup)
  "TEXT with each ($ \"name\") reference replaced by the strings entry it
names, flattened to one line by DISPLAY-FLATTEN."
  ;; LOOKUP is a function of one argument — the name — answering that entry as
  ;; a string, or NIL when the call carries none. A reference that does not
  ;; resolve is left exactly as written: a row grows into its text, it never
  ;; invents it.
  (ppcre:regex-replace-all "\\(\\$[ \\t\\n]*\"([^\"]*)\"\\)" text
                           (lambda (reference name &aux (value (funcall lookup name)))
                             (if (stringp value) (display-flatten value) reference))
                           :simple-calls t))

(defun tool-command-string (arguments)
  "The text a tool call's complete ARGUMENTS JSON carries for its row: the
value of the first +TOOL-COMMAND-KEYS+ entry that is a string, with the
call's own literal references resolved — an eval snippet's ($ \"name\") reads
as the strings entry it names, flattened to one line — so the row shows what
will run."
  ;; Answers NIL when ARGUMENTS is empty, is not an object, or names none of
  ;; the keys. Streaming partial JSON decodes as nothing, which is what lets a
  ;; mid-stream transcript header keep the raw name+args shape until the
  ;; durable fact hydrates complete arguments; the checkpoint listing reads
  ;; the durable fact, so it always sees the whole form.
  (when (and arguments (plusp (length arguments)))
    (with-handlers ((error () nil))
      (let ((root (nlk:decode-json arguments)))
        (loop for key in +tool-command-keys+
              for value = (json-value root :string key)
              when value
                return (deref-literal-refs
                        value (lambda (name) (json-value root :string "strings" name))))))))

(define-event (tool-result-event
               :record (record-tool-result call-id tool-name result
                                           :check ((duration-ms (or null (integer 0))) metadata)))
    +kind-tool-result+
  "The result of one tool call. Logged after execution."
  ((call-id :string "call-id" :default "")
   (tool-name :string "tool-name" :default "")
   (result :string "result" :default "")
   ;; The tool body's wall time in milliseconds, measured at the dispatch
   ;; seam (EXECUTE-TOOL-CALL) — the same number the live phase note prints,
   ;; now durable so per-tool timing survives as an event-log fact. NIL on
   ;; facts recorded away from an execution (the cancel backstop's synthetic
   ;; results, background-shell exit facts): an unmeasured duration is
   ;; omitted, never an invented zero.
   (duration-ms :integer "duration-ms")
   ;; Whether the call failed, said by what knew it at the dispatch seam
   ;; (RUN-TOOL-CALL): a body that signalled, a refusal before it ran, a hook's
   ;; veto or a tool's own verdict (NLE:FAILURE) — an eval whose form
   ;; signalled or whose last (sh ...) exited non-zero. A
   ;; reader draws the failure from this, never from the result's text
   ;; Retention: persisted-history, so a replay draws what the live stream drew.
   (failed :boolean "failed" :default nil)
   ;; The optional display-fact envelope RECORD-TOOL-RESULT stores under the
   ;; payload's "metadata" key (image results, the eval snippet's record). A JSON
   ;; object hash, or NIL — exposed so the local fold renders the same cards
   ;; the gateway stream carries.
   (metadata :object "metadata"))
  ;; DURATION-MS, when given, is the tool body's measured wall time in
  ;; milliseconds — the dispatch seam passes its own measurement so per-tool
  ;; timing is a durable fact rather than a live-only phase note. NIL omits
  ;; the key: an unmeasured duration is never an invented zero.
  ;;
  ;; FAILED true records the call as failed; the facts recorded away from an
  ;; execution (the cancel backstop's `interrupted by user', the restart's
  ;; closing text, a background exit) leave it false.
  ;;
  ;; METADATA, when given, is a JSON-object hash table stored verbatim under
  ;; the payload's "metadata" key — the same envelope the Zig turn stream
  ;; reads tool display facts from: "image"
  ;; ({"payload_kind","source","provider_executed", "preliminary"}) for image
  ;; results. The durable fact is the single stream projection for
  ;; both live publication and replay, so metadata written here reaches both
  ;; paths by construction.
  "Journal one tool result against an admitted TURN after it executes.")

;;; --- provider evidence events ---------------------------------------------
;;; Round-scoped provider facts. Token slots default NIL, never zero: a
;;; provider that did not report a number left it unknown, and the fold must
;;; not invent a zero (the Zig disjoint-usage rule, src/ai/core.zig Usage).

(define-event (turn-usage-event :record record-turn-usage) +kind-turn-usage+
  "One provider round's usage and finish reason."
  ;; INPUT-TOKENS is non-cached input only; absent numbers stay NIL (the
  ;; 8-slot disjoint contract of core.zig Usage: context use is input +
  ;; cached-input).
  ((input-tokens :integer "input-tokens")
   (output-tokens :integer "output-tokens")
   (cached-input-tokens :integer "cached-input-tokens")
   (total-tokens :integer "total-tokens")
   (reasoning-tokens :integer "reasoning-tokens")
   (cache-write-tokens :integer "cache-write-tokens")
   (cache-write-5m-tokens :integer "cache-write-5m-tokens")
   (cache-write-1h-tokens :integer "cache-write-1h-tokens")
   (cost-known :boolean "cost_known" :default nil)
   (cost-usd :number "cost_usd" :default 0.0)
   (finish-reason :string "finish-reason")
   (model :string "model" :default "")
   ;; The lane the round actually bound and the effort it ran under —
   ;; identity facts, not counters: the finish divider renders
   ;; `provider/model (effort)` from them rather than from live globals a
   ;; later /models or /think would have already moved. RESPONSE-MODEL is
   ;; the id the provider's own stream named; it stays NIL when the stream
   ;; did not say, and the divider surfaces it only when it disagrees with
   ;; the requested MODEL.
   (provider :string "provider" :default "")
   (response-model :string "response-model")
   (reasoning-effort :string "reasoning-effort")
   ;; True for an attempt whose message was discarded (a context-edge length
   ;; cut fed back into recovery): billed spend, not a durable round.
   (voided :boolean "voided" :default nil)
   ;; True when the counts are the organism's own estimate, not the
   ;; provider's report: a cancel cut the stream before the usage chunk, so
   ;; the prompt is the calibrated context estimate and the output is what
   ;; streamed, chars/4. Renderers mark such counts (`↑~1.1k`).
   (estimated :boolean "estimated" :default nil))
  ;; Unknown numbers are omitted from the payload entirely: NIL is not zero.
  ;; VOIDED marks an attempt whose message was discarded (a context-edge
  ;; length cut fed back into recovery): the spend was real and bills, the
  ;; round is not durable history. ESTIMATED marks counts the organism
  ;; estimated because a cancel cut the stream before the provider reported.
  "Durably record one provider round's usage evidence against TURN.")

(define-event (attempt-voided-event :record (record-attempt-voided :check ((disposition string))))
    +kind-attempt-voided+
  "One failed provider attempt's streamed output, sealed."
  ;; ATTEMPT is the durable attempt index that failed (the same counter
  ;; turn.provider_retry names); REASONING and TEXT are what it streamed,
  ;; whole, in the shape the completed-round message would have carried;
  ;; DETAIL is why it failed; STREAM-DURATION-MS is how long the attempt
  ;; streamed before it did. DISPOSITION is what the retry policy did next:
  ;; `rerun` (the same request went out again), `continued` (the next request
  ;; carried this output back and asked the model to go on from it), or
  ;; `stood` (the failure was terminal — an exhausted budget, a non-retryable
  ;; verdict).
  ((attempt :integer "attempt" :default 1)
   (reasoning :string "reasoning" :default "")
   (text :string "text" :default "")
   (detail :string "detail" :default "")
   (stream-duration-ms :integer "stream-duration-ms")
   (disposition :string "disposition" :default "stood"))
  ;; DETAIL names the failure, STREAM-DURATION-MS how long the attempt
  ;; streamed, DISPOSITION what the retry policy did next (rerun, continued,
  ;; stood). Persisted-history retention, deliberately NOT replay-suppressed:
  ;; the transcript keeps the block on the live stream and on resume alike.
  "Durably seal what one failed provider attempt had streamed — REASONING
and TEXT whole, never clipped: the cancel backstop persists a cut round
unclipped, and this is the same keep-what-was-paid-for rule one failure
class over.")

(define-event (provider-retry-event :record record-provider-retry) +kind-provider-retry+
  "One transient provider failure that will be retried."
  ;; STATUS NIL means the failure carried no HTTP status (timeout, truncation,
  ;; in-stream error). SCOPE names the budget the failure spent
  ;; (provider_request, provider_stream, or provider_contract); STREAM-ATTEMPT
  ;; and REQUEST-RETRIES-USED are the dual budget reducer state when the
  ;; failure was recorded.
  ((attempt :integer "attempt" :default 1)
   (scope :string "scope" :default "provider_stream")
   (stream-attempt :integer "stream-attempt" :default 1)
   (request-retries-used :integer "request-retries-used" :default 0)
   (status :integer "status")
   (delay-seconds :number "delay-seconds" :default 0)
   (detail :string "detail" :default "")
   ;; The identity the failing request targeted — present on retries so a
   ;; session whose every round fails still names its (provider, model);
   ;; provider_request facts record successful rounds only.
   (provider :string "provider" :default "")
   (model :string "model" :default ""))
  ;; PROVIDER and MODEL name the identity the failing request targeted, so a
  ;; failing session's forensics carry it even when no provider_request fact
  ;; ever lands (that fact records successful rounds only).
  "Durably record one transient provider failure that will be retried.")

(define-event (provider-fallback-event :record record-provider-fallback) +kind-provider-fallback+
  "One turn-scoped model failover: the attempt against FROM-PROVIDER/
FROM-MODEL failed (REASON is the classification — a wire status, or the
transport's own label when no status arrived) and the rest of TURN runs on
TO-PROVIDER/TO-MODEL."
  ;; ATTEMPT is the durable attempt index that failed; DETAIL is the failure's
  ;; own line, so the fact reads without the retry facts beside it. Flows on
  ;; the live stream: the status chrome that says a turn moved is this fact,
  ;; not a second post.
  ((from-provider :string "from-provider" :default "")
   (from-model :string "from-model" :default "")
   (to-provider :string "to-provider" :default "")
   (to-model :string "to-model" :default "")
   (reason :string "reason" :default "unclassified")
   (status :integer "status")
   (attempt :integer "attempt" :default 1)
   (detail :string "detail" :default ""))
  ;; Every failure class the chain walked is recorded — a candidate that could
  ;; not be resolved records the refusal as its reason and the chain moves on.
  "Durably record one turn-scoped model failover: the attempt against
FROM-PROVIDER/FROM-MODEL failed with REASON and the rest of TURN runs on
TO-PROVIDER/TO-MODEL.")

(define-event (provider-request-event :record record-turn-provider-request) +kind-provider-request+
  "One provider round's request evidence: which exact request went out
(BODY-SHA256 is sha256: over the serialized wire body) and what the live-only
part stream looked like, rolled up (the turn_engine.zig provider_request
fact)."
  ;; Usage is NOT here — the turn.usage fact is the one usage authority.
  ;; TTFT-MS and STREAM-DURATION-MS stay NIL on a round with no observer,
  ;; where no parts flow. PREFIX-SHARED-BYTES is how many leading bytes this
  ;; body shares with the turn's previous request and PREFIX-DIVERGED-IN where
  ;; the first differing byte falls ("input[0]", "tools"): a round whose
  ;; cached input fell short of the previous prompt is a harness leak when the
  ;; prefix moved and the provider's own miss when it did not. Both stay NIL
  ;; on round zero.
  ((provider :string "provider" :default "")
   (model :string "model" :default "")
   (provider-round :integer "provider-round" :default 0)
   (transport-attempts :integer "transport-attempts" :default 1)
   (provider-message-count :integer "provider-message-count" :default 0)
   (body-sha256 :string "body-sha256" :default "")
   (estimated-context-tokens :integer "estimated-context-tokens")
   (parallel-tool-calls-p :boolean "parallel-tool-calls" :initform t)
   (emitted-tool-calls-p :boolean "emitted-tool-calls")
   (stream-part-count :integer "stream-part-count" :default 0)
   (text-delta-count :integer "text-delta-count" :default 0)
   (ttft-ms :integer "ttft-ms")
   (stream-duration-ms :integer "stream-duration-ms")
   (prefix-shared-bytes :integer "prefix-shared-bytes")
   (prefix-diverged-in :string "prefix-diverged-in"))
  ;; Usage is deliberately absent — the turn.usage fact is the one usage
  ;; authority. Unknown timings are omitted: NIL is not zero.
  "Durably record one successful provider round's request evidence: the
request identity (body sha256, message count, frozen context estimate), the
live stream rollup (part counts, ttft, duration) and, after round zero, how
much of the body the turn's previous request already sent
(PREFIX-SHARED-BYTES) and where the first differing byte fell
(PREFIX-DIVERGED-IN).")
