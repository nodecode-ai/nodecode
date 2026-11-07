;;;; cell.lisp --- the verbs, /experience, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The verbs are plain functions in the EXPERIENCE package, every one
;;;; answering a string, every refusal an EXPERIENCE-ERROR the eval snippet
;;;; renders as text. The session that calls a verb is
;;;; NLK:*SCRIBE-SESSION-ID*: a reflection child's sighting is attributed to
;;;; the origin and the turn it reflects on (package.lisp *CHILDREN*), any
;;;; other session's to itself.
;;;;
;;;; /experience answers one line through the composer inline hint; an
;;;; answer of several lines is posted as a notice as well.
;;;;
;;;; Config, a sibling top-level key next to `index' and `cron':
;;;;   "experience": {"reflect": "tools", "recap": true, "recap_lines": 1}
;;;; and optionally "sources" (the invocation source kinds reflected on),
;;;; "provider" with "model" (a reflection model other than the session's
;;;; own, which gives up the shared prefix), "verification_repair_attempts".

(in-package #:nodecode-experience)

(defparameter +usage+ "/experience [reflect]")

;;; --- the manual ------------------------------------------------------------------------
;;; (help :experience) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives. The reflection is told its verbs in
;;; its own request (reflect.lisp).

(defparameter +primer+
  "The nodecode-experience cell reflects on this session. After a turn that ran a tool or was stopped, a fork of this session reflects on that turn: it records sightings - definitions of the index that helped or did harm, definitions of your own that did their job, verification checks - each with a verbatim quote, in the use ledger the index ranks by; it keeps what the next session would otherwise rediscover as definitions (define-memory, define-skill), one commit each; and it answers a recap. That recap is recorded here as a recorded exchange: a user line beginning `[experience] recap' - of the turn above, or of an earlier turn by number when turns nobody asked for (a cron delivery) landed in between - and under it the recap in your own voice, written by that reflection; read it as your own note to yourself, never as something the operator said. A recorded exchange is silent, never posted to a channel; only a recap the reflection marked (experience:attention), one that demands the operator's attention, is posted to the room as one note that @s the operator. Verbs, in eval, every one a string, every refusal ERROR: EXPERIENCE-ERROR:
  (experience:sight \"kind\" \"name\" &key status next quote note)   one sighting about the turn being reflected on; kind helped|harm|confounded|no-effect|call|verification; name the definition as the index lists it, or a verification's target, which takes :status and (unless passed) :next; :quote is required and verbatim
  (experience:sightings &key name limit)   the sightings in the ledger, newest first
  (experience:attention)   mark this reflection's recap as demanding the operator's attention; only a marked recap is posted to the room, as one note that @s the operator
  (experience:reflect &key session turn)   reflect on a turn now (this session's newest completed one by default)"
  "What (help :experience) answers while the cell runs.")

;;; --- whose turn a verb is about ------------------------------------------------------------

(defun origin-of (session)
  "(values ORIGIN TURN): the session and turn a sighting from SESSION is
about - a reflection child's origin and reflected turn, else SESSION and
its running turn, else its newest completed one."
  (nlk:if-let (entry (and session (gethash session *children*)))
    (values (car entry) (cdr entry))
    (values session
            (or (getf (nle:turn) :turn-id)
                (and session (nlk:store-open-p)
                     (ignore-errors (newest-completed-turn session)))))))

;;; --- the verbs ----------------------------------------------------------------------------

(define-verb sight (kind name &key status next quote note)
  "Record one sighting about the turn this session reflects on: KIND
helped (a definition of the index was read or followed, and it paid), harm
(following it caused damage), confounded (in play, but the turn cannot
tell its part from the rest), no-effect (read or followed, and nothing
changed because of it), call (a definition of your own ran and did its job)
or verification; NAME the definition as the index lists it, or
for a verification the target checked, with :status passed, failed, blocked
or skipped and, unless it passed, :next its smallest follow-up; :quote a
verbatim line from that turn (required). => one line naming the sighting."
  (let ((kind (let ((kind (and (stringp kind) (string-downcase (string-trim " " kind)))))
                (unless (member kind +kinds+ :test #'string=)
                  (fail "kind must be one of ~{~a~^, ~}, not ~s" +kinds+ kind))
                kind))
        (quote (let ((quote (and (stringp quote) (nlk:one-line quote))))
                 (unless (and quote (>= (length quote) 8))
                   (fail "a sighting needs :quote, a verbatim line from the turn it is about ~
                          (at least 8 characters); no quote, no sighting"))
                 (nlk:clip quote 400)))
        (name (let ((name (and (or (stringp name) (and name (symbolp name)))
                               (string-trim " " (string name)))))
                (unless (plusp (length name))
                  (fail "a sighting names the definition it is about, as the index lists it, ~
                         or a verification's target"))
                (nlk:clip (nlk:one-line name) 160)))
        (status (and status (string-downcase (string-trim " " (string status)))))
        (next (and next (nlk:clip (nlk:one-line (string next)) 300))))
    (if (string= kind "verification")
        (progn
          (unless (member status '("passed" "failed" "blocked" "skipped") :test #'string=)
            (fail "a verification :status is passed, failed, blocked or skipped"))
          (when (and (member status '("failed" "blocked" "skipped") :test #'string=)
                     (or (null next) (zerop (length next))))
            (fail "a ~a verification names :next, its smallest follow-up check" status)))
        (when (or status next)
          (fail ":status and :next belong only to verification sightings")))
    (unless (or (null note) (stringp note))
      (fail "note must be a string, not ~s" note))
    (multiple-value-bind (origin turn) (origin-of nlk:*scribe-session-id*)
      (unless origin
        (fail "no session to attribute the sighting to; call this inside a turn"))
      (unless (or (string= kind "verification") (nlk:knowledge-entry-named name origin))
        (fail "no definition named ~a in the index; (index \"text\") finds one" name))
      (nlk:note-knowledge-use (if (string= kind "verification") name (string-downcase name)) kind
                              :session origin
                              :extra (list "turn" turn "quote" quote
                                           "note" (and note (nlk:clip (nlk:one-line note) 300))
                                           "status" status "next" next))
      (format nil "sighted ~a ~a for ~a~@[ (~a)~]~@[ -> ~a~]" kind name origin status next))))

(define-verb sightings (&key name (limit 20))
  "The sightings in the ledger, newest first: about NAME when given, at
most LIMIT lines."
  (check-type limit (integer 1))
  (let ((rows (reverse (experience-lines :name (and name (string-downcase (string name)))))))
    (if rows
        (format nil "~{~a~^~%~}"
                (loop for line in rows
                      repeat limit
                      collect (flet ((value (key) (nlk:json-value line :string key)))
                                ;; One line: when, kind, about what, whose turn, the quote.
                                (format nil "~a ~a ~a~@[/~a~] ~a: ~s~@[ (~a)~]~@[ -> ~a~]"
                                        (nlk:clip (or (value "at") "") 10 :ellipsis "")
                                        (value "kind") (value "name") (value "status")
                                        (value "session") (nlk:clip (value "quote") 160)
                                        (value "note") (value "next")))))
        "no sightings yet")))

(define-verb reflect (&key session turn)
  "Reflect on TURN of SESSION now - this session and its newest completed
turn by default - in a fork of it: its sightings land as it goes, its
recap is recorded only if the session is idle at that turn when it ends."
  (let* ((session (or session nlk:*scribe-session-id* (fail "no session here; pass :session")))
         (turn (or turn (newest-completed-turn session)
                   (fail "session ~a has no completed turn yet" session))))
    (multiple-value-bind (decision child) (reflect-turn session turn "turn.completed" :by-hand t)
      (if child
          (format nil "reflecting on turn ~a of ~a in session ~a; its sightings land as it goes, ~
                       the recap only if ~a is idle at that turn when it ends"
                  turn session child session)
          (format nil "turn ~a of ~a is not reflected on: ~a"
                  turn session (string-downcase (symbol-name decision)))))))

(define-verb attention ()
  "Mark this reflection's recap as demanding the operator's attention: it
is then posted to the room as one note that @s the operator, instead of staying the silent note
to yourself it is by default. Call it only when the operator must see it -
something broke they would act on, a decision only they can make, a risk -
and write the recap to be read by them. => one line saying so; a refusal
when no reflection is in flight."
  (let ((session nlk:*scribe-session-id*))
    (with-experience-lock
      (unless (and session (find-reflection session))
        (fail "no reflection is in flight for this session; ~
               attention marks a reflection's recap"))
      (setf (gethash session *announced*) t)
      (format nil "marked: the recap will reach the operator as a note (~a)"
              session))))

(defun summary-line (&aux (lines (experience-lines)))
  (flet ((kinds (kind)
           (count kind lines :key (lambda (line) (nlk:json-value line :string "kind")) :test #'equal)))
    (format nil "experience: ~d sighting~:p (helped ~d · harm ~d · confounded ~d · no-effect ~d · call ~d · checks ~d) · reflect ~(~a~) · ~a"
            (length lines)
            (kinds "helped") (kinds "harm") (kinds "confounded") (kinds "no-effect") (kinds "call")
            (kinds "verification")
            (setting :reflect)
            (sb-ext:native-namestring (nlk:home "usage.jsonl")))))

(defun run-slash (args session-id)
  "The answer to one /experience invocation."
  (let ((words (nlk:split-words args))
        (nlk:*scribe-session-id* session-id))
    ;; No word at all reads as "": a word is never empty.
    (nlk:dispatch (or (first words) "") string=
      ("" (summary-line))
      ("reflect" (reflect :session session-id))
      (t (format nil "experience: unknown subcommand ~a; usage ~a" (first words) +usage+)))))

;;; --- the entry -----------------------------------------------------------------------------------

(defun clear-tables ()
  (mapc #'clrhash (list *children* *recorded*))
  (with-experience-lock
    (setf *reflections* '()
          *pending* '())))

(defvar *workers* t
  "Whether the start runs the reflector thread; a test drives TICK by hand.")

(defun install ()
  "The live tables emptied and, unless a test drives it, the reflector
thread: what the declaration's clauses do not carry."
  (clear-tables)
  (nle:on-stop #'clear-tables)
  (when *workers*
    (setf *worker* (nlk:worker-start "experience-reflector" #'tick :wake 60))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*))))))

(nle:define-cell experience
  (:section ("experience")
    (:guide "reflect names the turns a fork reflects on: its sightings rank the index, and it keeps memories and skills as definitions")
    ("reflect" :choice :options '("tools" "every" "off") :default "tools"
               :doc "turns that ran a tool or ended failed, every operator turn, or none")
    ("recap" :boolean :default t :doc "record the reflection's recap back into the session")
    ("recap_lines" :integer :default 1 :min 1 :doc "lines a recap may run to")
    ("sources" :list :doc "invocation source kinds reflected on; gateway and in_process when empty")
    ("provider" :string :doc "with model, a reflection model of its own (no shared prefix)")
    ("model" :string :doc "the reflection's model, beside provider")
    ("verification_repair_attempts" :integer :default 1 :min 0
                                    :doc "repair attempts a reflection may make per failed check"))
  ;; A list whose default is not empty.
  (:settings (lambda (values table)
               (declare (ignore table))
               (setf (getf values :sources) (or (getf values :sources) '("gateway" "in_process")))
               values))
  (:start #'install)
  (:help :experience "a fork reflects on each turn that ran a tool; an [experience] recap row is your own note, never the operator's words" +primer+)
  (:hook :frame (nlk:observer #'observe-frame "experience"))
  (:hook :tool #'fence-hook)
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook 'nle:name-session #'naming-hook)
  (:command "experience" #'run-slash
            :description "Experience across sessions: the sightings, reflect"
            :argument-hint "reflect"))
