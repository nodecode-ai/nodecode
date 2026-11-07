;;;; reflect.lisp --- the inner loop: a fork per completed turn, the recap back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; When an operator turn ends, the :FRAME observer queues it and pokes the
;;;; one reflector thread. The reflector FORKS the session at that turn
;;;; (NLK:CREATE-SESSION with the parent and the anchor: the child composes
;;;; the parent's chain by reference, frozen at the fork) and makes the
;;;; child's request byte-identical to the origin's cached prefix: it copies
;;;; the origin's durable harness sections (the index pin among them), its
;;;; model pin, and its eviction floor (floors are per session id, so a bare
;;;; fork would re-derive its own boundary over the whole composed chain).
;;;; Then one NLE:SUBMIT puts the reflection's request into the child - a
;;;; user message behind the cached prefix, the economics of hermes-agent's
;;;; background review - with tools: the reflection records sightings in the
;;;; use ledger and keeps memories and skills as definitions as it goes, one
;;;; commit each, and its final answer is the recap.
;;;;
;;;; The recap goes back into the origin as one recorded exchange
;;;; (NLK:RECORD-EXCHANGE-TURN, the Discord room's write-back seam): the
;;;; user half names what it is, the assistant half is the recap, appended
;;;; behind the turn it recaps - an assistant message at the end of the
;;;; history extends the cached prefix where a harness section would rewrite
;;;; it. It lands only while the origin is idle, checked and written under
;;;; the store's own lock (cron's RECORD-REPORT idiom), and only if no
;;;; OPERATOR turn arrived since the one it recaps: a cron delivery or an
;;;; earlier recap landing meanwhile moves the head but is nobody moving on,
;;;; so the recap still lands, its user half naming the turn it belongs to;
;;;; an operator turn in between means that turn gets its own reflection,
;;;; and this recap is dropped, quietly.
;;;;
;;;; THREAD RULE: the :FRAME hook runs on the turn worker that publishes and
;;;; only folds and pokes; every store read, fork, submit and record runs on
;;;; the reflector thread or a verb's caller thread. A test drives the
;;;; observer by hand and stubs NLE:SUBMIT.

(in-package #:nodecode-experience)

(nlk:access (session nlk::durable-session))

(nlk:define-record (reflection (:copier nil) (:predicate nil))
  "One reflection in flight: the origin and the turn it reflects on, the
child session, and what the child's turn has said so far. STATUS is NIL
until a terminal fact lands, then the fact's kind. Guarded by *LOCK*."
  (origin "" :type string)
  (turn "" :type string)
  (child "" :type string)
  (child-turn nil :type (or null string))
  (command-id "" :type string)
  (started 0 :type integer)
  (answer nil :type (or null string))
  (status nil :type (or null string)))

(defvar *reflections* '()
  "Reflections whose child turn has not settled. Guarded by *LOCK*.")

(defvar *pending* '()
  "((ORIGIN TURN KIND) ...): turns waiting for the reflector, at most one
per origin, the newest. Guarded by *LOCK*.")

(defvar *worker* nil
  "The reflector thread while it runs (an NLK:WORKER), or NIL.")

;;; --- the observer -----------------------------------------------------------------

(defun terminal-kind-p (type)
  (member type '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal))

(defun find-reflection (child)
  (find-if (lambda (reflection) (and (string= reflection.child child) (null reflection.status)))
           *reflections*))

(defun observe-frame (op)
  "Fold one published frame: a child's newest answer and terminal fact, or
an operator turn's end. Publishing thread: tables and a poke, no I/O."
  (nlk:bind (((type fact _ turn-id) (nlk:frame-fact op)))
    (let ((session (getf op :session-id)))
      (when (and (stringp session) type)
        (with-experience-lock
          (let ((reflection (find-reflection session)))
            (cond
              (reflection
               (when (and turn-id (null reflection.child-turn))
                 (setf reflection.child-turn turn-id))
               (cond ((equal type "turn.assistant_message_completed")
                      (nlk:when-let (text (nlk:fact-message-content fact))
                        (setf reflection.answer text)))
                     ((terminal-kind-p type)
                      (setf reflection.status type)
                      (poke))))
              ((and (terminal-kind-p type)
                    (stringp turn-id)
                    (not (ours-p session))
                    (not (gethash turn-id *recorded*)))
               ;; Queued, replacing an older queued turn of the same origin.
               (setf *pending* (cons (list session turn-id type)
                                     (remove session *pending* :key #'first :test #'string=)))
               (poke)))))))))

;;; --- the decision -----------------------------------------------------------------

(defun turn-facts (origin turn)
  "(values SOURCE-KIND DISPOSITION TOOL-CALLS) of TURN in ORIGIN, from the log."
  (let ((row (nlk:events :session-id origin :turn-id turn
                         :kind nlk::+kind-input-committed+
                         :as :row :columns '("invocation_json" "payload"))))
    (values (nlk:json-value (ignore-errors (nlk:decode-json (first row))) :string "source" "kind")
            (nlk:json-value (ignore-errors (nlk:decode-json (second row))) :string "disposition")
            (or (nlk:events :session-id origin :turn-id turn
                            :kind nlk::+kind-tool-call-started+ :as :count)
                0))))

(defun attributed-source (origin source)
  "The source kind a turn of ORIGIN admitted under SOURCE is judged by:
SOURCE, unless it is the kernel's default provenance, `in_process' -- an
exit wake's, a turn that carries no source of its own -- which continues the
work of whoever founded ORIGIN. A team node's wake is the team's, never the
operator's: eight reflections forked on the first team run's nodes."
  (if (equal source "in_process")
      (or (nlk:session-source-kind origin) source)
      source))

(defun reflect-decision (settings kind source disposition tool-calls &key by-hand limited)
  "Whether a turn is reflected on: :REFLECT, or why not - :RECORDED (a
recap this cell wrote), :OFF, :SOURCE (not an operator's turn), :LIMITED
(the provider refused the turn for a limit, LIMITED true), :QUIET (under
`reflect: tools', a completed turn that ran no tool)."
  (cond ((equal disposition "recorded") :recorded)
        (by-hand :reflect)
        ((equal (getf settings :reflect) "off") :off)
        ((not (member source (getf settings :sources) :test #'equal)) :source)
        ;; The reflection would be refused as surely, and would spend the
        ;; next request the operator's key allows.
        (limited :limited)
        ((and (equal (getf settings :reflect) "tools")
              (equal kind "turn.completed")
              (zerop tool-calls))
         :quiet)
        (t :reflect)))

(defun turn-limited-p (origin turn)
  "Whether the provider refused TURN of ORIGIN for a limit: a retry or the
failure itself carries HTTP 429."
  (some (lambda (payload) (eql 429 (nlk:json-value payload :integer "status")))
        (nlk:events :session-id origin :turn-id turn
                    :kind (list nlk::+kind-provider-retry+ nlk::+kind-turn-failed+)
                    :as :payloads)))

(defun newest-completed-turn (session)
  "The newest turn of SESSION that completed and was not a recorded
exchange, or NIL."
  (loop for row in (nlk:events :session-id session :kind nlk::+kind-turn-completed+
                               :order :newest :limit 8 :as :rows :columns '("turn_id"))
        for turn = (first row)
        unless (equal "recorded" (nth-value 1 (turn-facts session turn)))
          return turn))

;;; --- the fork -----------------------------------------------------------------------

(defparameter *reflection-prompt*
  "[experience] The turn above just ended. You are the same assistant, reflecting before the next request; nothing you say here reaches the operator except your final answer - your recap, recorded in the session as your note to yourself and posted to them only when you mark it (step 4). Do these in order, each through eval, then answer.
1. Sightings. For each definition of the index (the index section of the head message, the user message ahead of the first prompt) that mattered in the turn above, and for each definition of your own that did its job - one you kept this turn~@[: ~{~a~^, ~}~], or one an earlier session kept - call (experience:sight \"kind\" \"name\" :quote \"...\" &key note) with :quote a verbatim line from the turn above - no quote, no sighting. kind is helped (read or followed, and the quote is the moment it changed what happened for the better), harm (following it caused damage), call (a definition of yours ran and did its job; :quote the call or its result), confounded (it was in play, but the turn cannot tell its part from the rest: several things changed at once, or one good or bad outcome is all that speaks for it) or no-effect (you read or followed it expecting it to matter, and nothing in the turn changed because of it). A turn that went well is not a moment: a definition in it is helped only when a quote shows it paid, and confounded otherwise - an invented credit misranks it for every later session. name is the definition's name as the index lists it. Nothing stood out: call nothing.
2. Verification. If the turn changed code, config, data, a live surface, or made a claim, reconstruct the intended user-visible invariant and check the narrowest evidence available in the turn, durable facts, or existing safe verbs. For each target, call (experience:sight \"verification\" \"target\" :status \"passed|failed|blocked|skipped\" :next \"...\" :quote \"...\" :note \"...\"). A passed check names the evidence. A failed, blocked or skipped check names its smallest next step. A write receipt is not proof: read the result back. If evidence fails and the requested change is clear, make at most ~d bounded repair attempt~:p per target through existing safe definitions, then verify again. If repair needs a new permission, a broad rewrite, or an unknown external effect, mark it blocked and name the next action. Never invent a command or claim a check ran.
3. Keeps. Keep what the next session would otherwise rediscover, as definitions, each one eval of the form itself: a standing preference or correction the operator stated in so many words - (define-memory name \"one declarative sentence\" :type :feedback), :type :user for who they are; ~@[a fact about this project - (define-memory name \"...\" :type :project :project ~s); ~]a procedure this turn worked out that no definition of yours covers and a later session would repeat - (define-skill name \"one sentence\" ;; when to use ;; the steps, one per ;; line, the closing paren on a line of its own); a procedure this image can run is a defun, not prose. A procedure or lesson rests on this turn's evidence, carried as ;; lines in its form: when (a skill's when-to-use line) - the shape of situation it applies to, the pattern and not this task's facts: too generic ('be thorough') changes nothing, too specific never fires again; evidence - the moment in the turn above that shows it, what went wrong before it and what worked after, quoted; confidence - high or medium, and why. When one good outcome or several changes at once are all that stand behind it, narrow it to what the turn does show, or keep nothing. A fact true only of this folder - a path, a mapping, a value - is written as a fact of it (in this project ...), never as a general rule; procedure is written for any later task it fits. A definition you read this turn~@[ (~{~a~^, ~})~] that was wrong or incomplete: (edit its file old new) - (help 'name) names the file; one that misled you: (unintern 'name). (index \"text\") first: extend what is there before adding a second. Never keep an environment failure, a claim that a tool does not work, an error that resolved, a one-off task, or a method that never worked. Every keep is one commit under this reflection's session, undone with git; nothing to keep is a fine answer.
4. The recap. Answer with the recap alone: one line - at most ~d line~:p - in your own voice, what you would tell yourself before the next request: what was asked, what was done, what is open, what to carry forward, compressed into that line. No preamble, no headings, no line breaks. It is silent - a note to yourself, never posted. If, and only if, it demands the operator's attention (something broke they would act on, a decision only they can make, a risk), call (experience:attention) before you answer and make that one line a straightforward call-to-action - what they should know or do - since it is then posted to the room as one note that @s them."
  "The reflection's request, a format control over the names the turn kept
(a list or NIL), the repair-attempt bound, the origin's project root (or
NIL), the names the turn read (a list or NIL) and the recap's line count.
Data, so a layer can reword it.")

(defun turn-definitions (origin turn)
  "The names TURN of ORIGIN kept, lowercased and distinct: the definition
facts that landed between its start and its end - they carry no turn id,
the span says whose they are. NIL for a turn without both ends."
  (let ((start (nlk:events :session-id origin :turn-id turn :kind nlk::+kind-turn-started+
                           :as :value :columns '("log_position")))
        (end (nlk:events :session-id origin :turn-id turn
                         :kind (list nlk::+kind-turn-completed+ nlk::+kind-turn-failed+
                                     nlk::+kind-turn-cancelled+)
                         :as :value :columns '("log_position"))))
    (when (and start end)
      (nlk:distinct
       (loop for payload in (nlk:events :session-id origin :kind nlk::+kind-definition-recorded+
                                        :after start :before end :as :payloads)
             append (loop for name across (or (nlk:json-value payload :array "names") #())
                          when (stringp name) collect (string-downcase name)))))))

(defun fork-reflection (origin turn)
  "Fork ORIGIN at TURN and put the reflection's request into the child.
=> the child session id."
  (let* ((session (or (ignore-errors (nlk:find-session origin))
                      (fail "no session ~a" origin)))
         (child (nlk:unused-name (format nil "experience-~a-~a" origin turn)
                                 #'nlk:session-exists-p))
         (command-id (format nil "experience:~a:reflect" child))
         (reflection (make-reflection :origin origin :turn turn :child child
                                      :command-id command-id
                                      :started (get-universal-time))))
    ;; The origin's cached prefix needs no copy: the create carries the model
    ;; pin, and an anchored fork stands on its chain's eviction floor and
    ;; harness sections (NLK:RETAINED-HISTORY-FLOORS,
    ;; NLK:STANDING-HARNESS-SECTIONS). A reflection model of its own gives
    ;; that prefix up.
    (nlk:create-session :id child :cwd session.cwd :parent origin :anchor-turn-id turn)
    (when (and (setting :provider) (setting :model))
      (nlk:record-session-model-selection child :provider (setting :provider)
                                                 :model (setting :model)))
    (setf (gethash child *children*) (cons origin turn))
    ;; On the list before the submit: a turn that dies inside the submit's
    ;; own publish still finds its record.
    (with-experience-lock (push reflection *reflections*))
    (handler-case (submit-into child
                               (format nil *reflection-prompt*
                                       (ignore-errors (turn-definitions origin turn))
                                       (setting :verification-repair-attempts)
                                       (ignore-errors (nlk:session-project-root origin))
                                       (ignore-errors (turn-reads origin turn))
                                       (setting :recap-lines))
                               command-id origin)
      (error (condition)
        (with-experience-lock
          (setf *reflections* (remove reflection *reflections*)))
        (remhash child *children*)
        (error condition)))
    child))

(defun reflect-turn (origin turn kind &key by-hand &aux (settings (running-settings)))
  "Reflect on TURN of ORIGIN when the settings say so. => (values DECISION
CHILD), CHILD the reflection session when one was forked."
  (unless (nlk:store-open-p)
    (fail "no store is open"))
  (multiple-value-bind (source disposition tool-calls) (turn-facts origin turn)
    (let ((decision (reflect-decision settings kind (attributed-source origin source)
                                      disposition tool-calls :by-hand by-hand
                                      :limited (turn-limited-p origin turn))))
      (if (eq decision :reflect)
          (values :reflect (fork-reflection origin turn))
          (values decision nil)))))

;;; --- the fence and the budget --------------------------------------------------------
;;; A reflection is the same assistant with the same tools. A session this cell
;;; made -- a reflection -- calls the verbs its request names, keeps
;;; definitions, the memories and skills it keeps included, edits or forgets
;;; them, and runs nothing else. It runs under no budget unless a layer sets
;;; one: the fence bounds what it can do, and the spend line on every result
;;; shows it how long it has run.

(defparameter *budget* nil
  "The limits a reflection runs under (NLE:TURN-BUDGET): NIL,
none, even when a layer caps every other turn. It sees its clock on every
result and nothing stops it. Data, so a layer can set one: (:SECONDS N
:CALLS N), either key optional.")

(defparameter *verb-packages* '("NODECODE-EXPERIENCE")
  "The packages whose exported functions a session this cell made may call.")

(defparameter *read-verbs* '(nle:help nle:index)
  "The core's verbs a session this cell made may call: the ones that read
what is kept.")

(defparameter *kept-operators* '(defun defmacro nle:define-memory nle:define-skill)
  "What a session this cell made keeps, whole: a definition is written to
its folder, never run.")

(defparameter *act-operators* '(nle:edit unintern fmakunbound makunbound)
  "What a session this cell made may do to a definition it read: change its
file, or forget it -- each over values alone.")

(defun verb-p (symbol)
  "Whether SYMBOL names one of *READ-VERBS* or a function exported from one
of *VERB-PACKAGES*."
  (and (symbolp symbol)
       (fboundp symbol)
       (or (member symbol *read-verbs*)
           (let ((package (symbol-package symbol)))
             (and package
                  (member (package-name package) *verb-packages* :test #'string=)
                  (eq :external (nth-value 1 (find-symbol (symbol-name symbol) package))))))))

(defun value-offender (form)
  "The first subform of FORM that acts, or NIL when FORM only computes a
value: a literal or a variable, a quoted datum, a string of the call's own
($ \"name\"), a list or a progn of such, a verb called on such."
  (cond ((atom form) nil)
        ((eq (first form) 'quote)
         (and (not (and (consp (rest form)) (null (cddr form)))) form))
        ((eq (first form) 'nle:$)
         (and (notevery #'atom (rest form)) form))
        ((or (member (first form) '(list progn)) (verb-p (first form)))
         (some #'value-offender (rest form)))
        (t form)))

(defun snippet-offender (form)
  "The first subform of toplevel FORM a session this cell made may not
evaluate, or NIL: a form of *KEPT-OPERATORS* is kept and not run, one of
*ACT-OPERATORS* acts on values alone, and any other form must only compute
a value (VALUE-OFFENDER)."
  (cond ((and (consp form)
              (member (first form) *kept-operators*)
              (consp (rest form))
              (symbolp (second form)))
         nil)
        ((and (consp form) (member (first form) *act-operators*))
         (some #'value-offender (rest form)))
        (t (value-offender form))))

(defun fence-refusal (name arguments)
  "The refusal a session this cell made gets for calling tool NAME with
ARGUMENTS, or NIL when the call may run. A snippet that does not read passes:
the eval tool reports the reader's verdict and runs none of it."
  (if (not (equal name "eval"))
      (format nil "ERROR: refused by nodecode-experience: a reflection works through eval ~
                   alone, so ~a did not run" name)
      (let* ((*package* (find-package '#:nodecode.evolved))
             (form (and (hash-table-p arguments) (gethash "form" arguments)))
             (forms (and (stringp form) (nle::snippet-forms form)))
             (offender (handler-case (some #'snippet-offender forms)
                         ;; a shape the walk cannot take apart is not a value form
                         (error () (first forms)))))
        (when offender
          (format nil "ERROR: refused by nodecode-experience: a reflection calls its ~
                       verbs (experience:sight, experience:attention, help, index), keeps ~
                       definitions (defun, defmacro, define-memory, define-skill) and edits ~
                       or forgets them (edit, unintern, fmakunbound), nothing else; ~a is ~
                       neither, so the snippet did not run"
                  (nlk:clip (nlk:one-line (prin1-to-string (alexandria:ensure-car offender)))
                            80))))))

(defun fence-hook (op next)
  "The :TOOL around-hook that holds a session this cell made to its verbs
and passes every other session's call through untouched."
  (let ((refusal (and (ours-p (getf (nle:turn) :session-id))
                      (fence-refusal (getf op :name) (getf op :arguments)))))
    (if refusal (nle:failure refusal) (funcall next op))))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a session this cell made runs under *BUDGET*,
none unless a layer set one, and never under a cap a layer set for every
other turn."
  (if (ours-p (getf turn :session-id))
      *budget*
      (funcall next turn)))

(defun naming-hook (next session-id text)
  "Advice on NLE:NAME-SESSION: a session this cell made is never named -- a
reflection is a fork a turn, and its name a model call a turn. Every other
session is NEXT's."
  (unless (ours-p session-id)
    (funcall next session-id text)))

;;; --- the recap ----------------------------------------------------------------------

(defparameter *recap-input*
  "[experience] recap of the turn above, by a reflection of this session"
  "The user half of the recorded exchange when it lands right under the
turn it recaps: what the recap under it is. Data, so a layer can reword it.")

(defparameter *recap-input-behind*
  "[experience] recap of turn ~d, \"~a\", by a reflection of this session; ~d turn~:p since, none the operator's"
  "The user half when turns nobody asked for - a cron delivery, an earlier
recap - landed between the recapped turn and this one: a format control
over the turn's number, the first words of its request, and how many
turns came between. Data, so a layer can reword it.")

(defun turns-since (origin turn)
  "((TURN-ID SOURCE DISPOSITION) ...): every input ORIGIN admitted after
TURN completed, oldest first, with the invocation source kind and the
input's disposition. NIL when nothing came after."
  (nlk:when-let (position (nlk:events :session-id origin :turn-id turn
                                      :kind nlk::+kind-turn-completed+
                                      :as :value :columns '("log_position")))
    (flet ((object (text)
             (and (stringp text) (ignore-errors (nlk:decode-json text)))))
      (loop for row in (nlk:events :session-id origin :kind nlk::+kind-input-committed+
                                   :after position :as :rows
                                   :columns '("turn_id" "invocation_json" "payload"))
            collect (list (first row)
                          (nlk:json-value (object (second row)) :string "source" "kind")
                          (nlk:json-value (object (third row)) :string "disposition"))))))

(defun operator-turn-p (source disposition settings)
  "Whether an input of SOURCE and DISPOSITION is the operator's own: a
reflected source kind, and not a recorded exchange."
  (and (member source (getf settings :sources) :test #'equal)
       (not (equal disposition "recorded"))))

(defun recap-input (origin turn since)
  "The user half: the plain line when the recap lands right under its
turn, the naming line when SINCE turns came between."
  (if (null since)
      *recap-input*
      (format nil *recap-input-behind*
              (nlk:turn-chain-depth origin turn)
              (nlk:clip (nlk:one-line
                         (or (nlk:json-value (nlk:events :session-id origin :turn-id turn
                                                         :kind nlk::+kind-input-committed+
                                                         :as :payload)
                                             :string "message")
                             ""))
                        60)
              (length since))))

(defun usage-facts (rows)
  "ROWS, a child turn's turn.usage payloads, summed the way the shell's
meter sums a turn - voided attempts excluded, provider and model the newest
round's - as the exchange facts, or NIL when no row carried a count."
  (let ((rows (remove-if (lambda (row) (nlk:json-value row :boolean "voided")) rows))
        (counts '((:input-tokens . "input-tokens") (:cached-input-tokens . "cached-input-tokens")
                  (:output-tokens . "output-tokens") (:reasoning-tokens . "reasoning-tokens"))))
    (append (loop for (keyword . key) in '((:provider . "provider") (:model . "model"))
                  for newest = (loop for row in (reverse rows) thereis (nlk:json-value row :text key))
                  when newest append (list keyword newest))
            (and (loop for (nil . key) in counts
                         thereis (some (lambda (row) (nlk:json-value row :integer key)) rows))
                 (loop for (keyword . key) in counts
                       append (list keyword (loop for row in rows
                                                  sum (or (nlk:json-value row :integer key) 0))))))))

(defun reflection-facts (reflection)
  "What the child's turn ran, took and cost, as the facts the recap's
completion carries (RECORD-EXCHANGE-TURN :FACTS): recorder `reflection',
the child's provider and model, its start-to-end elapsed, its tokens
summed over its rounds."
  (let* ((child reflection.child)
         (turn (or reflection.child-turn
                   (nlk:events :session-id child :kind nlk::+kind-turn-completed+
                               :order :newest :as :value :columns '("turn_id")))))
    (list* :recorder "reflection"
           (when turn
             (flet ((at (kind) (nlk:events :session-id child :turn-id turn :kind kind
                                           :as :value :columns '("occurred_at"))))
               (let* ((started (at nlk::+kind-turn-started+))
                      (ended (at nlk::+kind-turn-completed+))
                      (elapsed (and started ended
                                    (- (nle::iso-8601-ms ended) (nle::iso-8601-ms started)))))
                 (append (and elapsed (>= elapsed 0) (list :elapsed-ms elapsed))
                         (usage-facts (nlk:events :session-id child :turn-id turn
                                                  :kind nlk::+kind-turn-usage+
                                                  :as :payloads)))))))))

(defun record-recap (reflection settings)
  "Record the child's answer into the origin as one settled exchange, only
while the origin is idle and no operator turn arrived since the reflected
one; turns nobody asked for in between are named in the user half. A
reflection that marked its recap (ATTENTION) records it announced: the
room posts the answer as a note. => the recorded turn id, or NIL when the
recap was dropped."
  (let* ((origin reflection.origin)
         (turn reflection.turn)
         ;; The answer's non-empty lines, at most RECAP-LINES, bounded.
         (lines (remove-if (lambda (line) (zerop (length (string-trim " " line))))
                           (nlk:lines reflection.answer)))
         (answer (nlk:clip (format nil "~{~a~^~%~}"
                                   (subseq lines 0 (min (length lines)
                                                        (getf settings :recap-lines))))
                           1500))
         (announce (gethash reflection.child *announced*))
         (cursor nil)
         (turn-id nil))
    (when (plusp (length answer))
      (bt2:with-recursive-lock-held (nlk::*store-lock*)
        (when (and (nlk:session-exists-p origin)
                   (not (nlk:active-turn-p origin)))
          (let ((since (turns-since origin turn)))
            (unless (some (lambda (entry)
                            (operator-turn-p (second entry) (third entry) settings))
                          since)
              (setf cursor (ignore-errors (nle::session-cursor origin))
                    turn-id (nlk:record-exchange-turn
                             origin (recap-input origin turn since) answer
                             :command-id (format nil "experience:~a:recap" reflection.child)
                             :announce announce
                             :facts (reflection-facts reflection)))
              (when turn-id
                (setf (gethash turn-id *recorded*) t))))))
      (when (and turn-id cursor)
        (handler-case (nle::publish-turn-facts origin cursor)
          (error (condition)
            (warn "experience: recap recorded into ~a but not published: ~a"
                  origin condition)))))
    turn-id))

(defun settle-reflection (reflection &aux (settings *experience*))
  "The child's turn ended: record the recap, or nothing. => T when a recap
was recorded."
  ;; A reflection that fails or answers nothing says so only in its own
  ;; session's log. The line it writes on *ERROR-OUTPUT* is a notice in the
  ;; transcript whenever a shell hosts the organism.
  (and settings
       (equal "turn.completed" reflection.status)
       (getf settings :recap)
       reflection.answer
       ;; Busy, or the operator moved on: that turn gets its own reflection
       ;; and this recap is dropped without a word - the shell already shows
       ;; the newer turn, and the sightings landed as the reflection went.
       (and (record-recap reflection settings) t)))

;;; --- the reflector thread -----------------------------------------------------------

(defun poke ()
  "Wake the reflector: something queued, settled or ended."
  (nlk:worker-poke *worker*))

(defun tick ()
  "One pass of the reflector: fork what is queued, record what settled."
  (dolist (job (with-experience-lock
                 (let* ((busy (loop for reflection in *reflections*
                                    unless reflection.status
                                      collect reflection.origin))
                        (ready (remove-if (lambda (job) (member (first job) busy :test #'string=))
                                          *pending*)))
                   (setf *pending* (set-difference *pending* ready :test #'equal))
                   (reverse ready))))
    (destructuring-bind (origin turn kind) job
      (handler-case (reflect-turn origin turn kind)
        (error (condition)
          (warn "experience: turn ~a of ~a not reflected on: ~a" turn origin condition)))))
  (dolist (reflection (with-experience-lock
                        (let ((done (remove-if-not #'reflection-status *reflections*)))
                          (setf *reflections* (set-difference *reflections* done))
                          done)))
    (unwind-protect
         (handler-case (settle-reflection reflection)
           (error (condition)
             (warn "experience: reflection ~a not settled: ~a" reflection.child condition)))
      ;; The mark served its settling; a stale one must not announce the
      ;; next reflection under the same child id.
      (remhash reflection.child *announced*))))
