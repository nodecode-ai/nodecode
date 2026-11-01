;;;; fork.lisp --- a turn forks itself: its independent tasks run at once,
;;;; each in a fork of the session that holds its whole context. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (fork "task" ... &key seconds from check keep) forks the calling session at
;;;; the turn running it, once per task, puts each task to its fork and waits
;;;; for every fork's turn to end. A fork is an anchored child
;;;; (CREATE-SESSION :PARENT :ANCHOR-TURN-ID): its history composes the
;;;; parent's by reference through the running turn, and the create carries the
;;;; parent's model, so its first request extends the parent's cached prefix and
;;;; it pays for its own work alone. That history ends in the eval call waiting
;;;; on it, which the fold answers `no result recorded (interrupted)': the
;;;; task's words say why, and that a fork does not fork.
;;;;
;;;; Going back is the same verb, never a second one:
;;;;
;;;; :FROM an earlier turn -- -N for N turns back, or its id -- anchors the
;;;; forks there: they hold what the session knew when that turn ended, and
;;;; nothing after it.
;;;;
;;;; :FROM a generation G, or an (OLD NEW) pair, with :CHECK is the way back
;;;; through the generations this home sealed. The check -- a form answering
;;;; true, or a shell command exiting 0 -- is put to a few generations at once,
;;;; each laid into a throwaway home of its own (BUILD-GENERATION) and asked in
;;;; a fresh process of today's binary (TRIAL), so neither the live image nor
;;;; the operator's home is touched; the search halts at the step where the
;;;; check turned false, and says what moved in it.
;;;;
;;;; :KEEP NAME with :CHECK races versions of NAME. Each fork writes its own as
;;;; FORK-<n>/NAME; each version and what stands are put to the check in a
;;;; fresh process laid at the newest generation; those that pass are timed on
;;;; the image's own calls of NAME -- each live call served by one of them, the
;;;; call itself the clock -- and the winner goes in NAME's place HELD
;;;; (HOLD-DEFINITIONS): written to the layer after one clean call made outside
;;;; an eval, put back if that call signals, and (revert 'NAME) puts the old one
;;;; back by hand until then. Beside a way back that found its step, NAME as the
;;;; last good generation's layer had it races too.
;;;;
;;;; Why the kernel: a fork runs in the operator's own image, so what keeps it
;;;; from displacing what stands, or from writing the layer, is the kernel's own
;;;; discipline, never advice a folder may or may not load. DEFINITION-ESCROW
;;;; asks FORK-FENCE before a fork's form runs -- one that defines or forgets a
;;;; name that already answers, or one a sibling claimed, is refused unrun;
;;;; RECORD-DEFINITIONS asks FORK-RECORD, and nothing a fork defines is written;
;;;; REPORT-UNKEPT-CHANGES asks FORK-UNKEPT, and no eval -- the fork's, or one
;;;; open beside it -- is told a fork's definitions went unkept. What a fork
;;;; defined stays live; the layer gets what the parent keeps.
;;;;
;;;; The wait reads the log: a fork's turn has ended when TURN-TERMINAL-KIND
;;;; says how. A fork's turn runs under the call's seconds (the advice on
;;;; NLE:TURN-BUDGET, whose contract is that a session's owner advises it for
;;;; its own sessions): past them its tools are refused and it answers where it
;;;; stands, and one still running *FORK-GRACE-SECONDS* later is cancelled. The
;;;; caller unwinding -- an eval interrupt, its own turn stopped -- cancels
;;;; every fork still running.
;;;;
;;;; Forks are sessions like any other: /sessions draws them under the session
;;;; they came off, and their turns are in the log. Each is titled as it is
;;;; made -- fork N: its task -- so no model call names it, and none is
;;;; reflected on (their source is `fork'). The manual is (help :fork), the
;;;; :FORK entry of the help topics (engine/turn.lisp).

(in-package #:nodecode.kernel)

;;; --- the section, the refusal ----------------------------------------------------

(define-section ("fork")
  (:guide "(fork \"task\" ...) hands a turn's independent tasks to forks of its session; seconds bounds each fork's turn")
  ("seconds" :integer :default 900 :min 30
             :doc "seconds a fork's turn may run before its tools are refused"))

(define-error fork-error (error)
    ((detail :reader fork-error-detail))
  (:report "~a" detail)
  (:documentation "Every refusal FORK makes; DETAIL is the whole report, which the
eval snippet renders as ERROR: FORK-ERROR: <detail>."))

(defun fork-fail (format-control &rest arguments)
  "Signal FORK-ERROR with a formatted detail."
  (error 'fork-error :detail (apply #'format nil format-control arguments)))

(defun fork-setting (key)
  "KEY of the shared config's `fork' section, as its declaration reads it: the
configured value, else the declared default. The shared config is read above
the kernel (NLE::READ-SHARED-CONFIG)."
  (getf (section-settings (find-section '("fork"))
                          (ignore-errors (json-value (funcall 'nle::read-shared-config) :object "fork")))
        key))

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *fork-task-input*
  "[fork ~d of ~d] You are a fork of this session, made at the eval call above, which is your parent's and is still waiting on you -- that is why its result reads interrupted. Do not run it again: a fork cannot fork. Your one task:~%~a~%~:[~;Other forks are working on the other tasks at this moment, in the same folder: change nothing another task could touch. ~]Make no posts, messages or commits unless the task says to. Define only names of your own: you run in the operator's own image, so a form that defines or forgets a name that already answers is refused, and nothing you define is written to the layer -- the parent keeps what it chooses. Your answer goes back to that eval call, not to a person: lead with the result, then only the facts that back it."
  "A fork's input, over its number, how many forks the call made, its task,
and whether it has siblings.")

(defparameter *fork-earlier-input*
  "[fork ~d of ~d] You are a fork of this session, taken back at an earlier turn of it: you hold what this session knew when that turn ended, and nothing after it happened in your record. A fork cannot fork. Your one task:~%~a~%~:[~;Other forks are working on the other tasks at this moment, in the same folder: change nothing another task could touch. ~]Make no posts, messages or commits unless the task says to. Define only names of your own: you run in the operator's own image, so a form that defines or forgets a name that already answers is refused, and nothing you define is written to the layer -- the parent keeps what it chooses. Your answer goes back to the call that made you, not to a person: lead with the result, then only the facts that back it."
  "A fork's input when its call took it back to an earlier turn, over the same
four.")

(defparameter *fork-keep-input*
  "~%~%This call races versions of ~a. Write yours as ~a: one defun taking every call ~a takes today~@[ -- its lambda list is ~a~] -- with any helper inside it in flet or labels, and leave ~a itself alone. The parent puts your version to this check in a fresh process: ~a. The versions that pass are timed on this image's own calls of ~a, and the fastest goes live if it is 5% faster than what stands. Answer with what your version does differently and why it is faster, and nothing else."
  "What a :KEEP call adds to each task, over the name raced, the fork's own
name for its version, the name, its lambda list, the name, the check and the
name.")

(defparameter *fork-bisect-input*
  "~%~%Before you were made, this call went back through the generations this home sealed: ~a"
  "What a way back adds to each task: what it found, in words.")

;;; --- the forks -------------------------------------------------------------------

(defparameter +fork-most+ 4 "The forks one call may make.")

(defparameter +fork-width+ 4
  "How many generations a way back asks at once, each a fresh process booting.")

(defparameter *fork-grace-seconds* 60
  "How long past its budget a fork's turn may still run before it is cancelled.")

(defparameter *fork-probe-seconds* 300
  "How long one fresh process may take to answer a check.")

(defvar *fork-waiting* (make-hash-table :test #'equal :synchronized t)
  "The forks a call is waiting on, by session id: (:SEMAPHORE S :SECONDS N
:ACTS FORMS), ACTS the definitions the fork evaluated, newest first.")

(defvar *fork-owners* (make-hash-table :test #'equal :synchronized t)
  "While a call runs, each name its forks define: the name -> the fork's session.")

(defvar *fork-made* (make-hash-table :test #'equal :synchronized t)
  "Each name a fork has defined in this image -> T: live and never kept, so no
eval's window counts it as that eval's own unkept change (FORK-UNKEPT).")

(defun fork-name (turn n)
  "The session id of TURN's Nth fork: fork-<turn>-<n>, numbered further when
taken."
  (unused-name (format nil "fork-~a-~d" turn n) #'session-exists-p))

(defun fork-session-p (session)
  "Whether SESSION is a fork FORK made: its id is fork-<turn>-<n> (FORK-NAME)."
  (and (stringp session) (uiop:string-prefix-p "fork-turn-" session) t))

(defun fork-running-turn ()
  "The durable turn the calling eval runs in -- which the turn engine binds,
above the kernel, as NLE::*CURRENT-DURABLE-TURN* -- or NIL."
  (let ((symbol (find-symbol "*CURRENT-DURABLE-TURN*" '#:nodecode.evolved)))
    (and symbol (boundp symbol) (symbol-value symbol))))

(defun fork-caller ()
  "(values SESSION TURN): the session whose eval is calling and the id of the
turn running it, or a refusal."
  (let ((session *scribe-session-id*)
        (turn (fork-running-turn)))
    (cond ((fork-session-p session)
           (fork-fail "a fork cannot fork: work your task here, and say in your answer what you would have forked"))
          ((not (and session turn (equal session (durable-turn-session-id turn))))
           (fork-fail "fork forks the turn running it: call it from a session's eval, inside a turn"))
          (t (values session (durable-turn-turn-id turn))))))

(defun fork-turn-from-p (from)
  "Whether FROM names an earlier turn: -N, or a turn's id."
  (or (stringp from) (and (integerp from) (minusp from))))

(defun fork-keepable (name)
  "NAME as the symbol a race keeps, or a refusal: a plain function of this
image, outside a locked package, with no advice on it and no hold."
  (let* ((symbol (if (stringp name) (find-symbol (string-upcase name) '#:nodecode.evolved) name))
         (package (and (symbolp symbol) (symbol-package symbol)))
         (advice (and (symbolp symbol) (cdr (assoc symbol *hooks* :test #'equal)))))
    (cond ((not (and symbol (symbolp symbol) (fboundp symbol)))
           (fork-fail ":keep names a function of this image, and ~s is none" name))
          ((or (macro-function symbol) (special-operator-p symbol))
           (fork-fail ":keep races calls, and ~s is a macro" symbol))
          ((typep (fdefinition symbol) 'generic-function)
           (fork-fail ":keep races one definition, and ~s is a generic function with its methods: race a plain defun" symbol))
          ((and package (sb-ext:package-locked-p package))
           (fork-fail ":keep races what this image may change, and ~s is in the locked package ~a" symbol (package-name package)))
          (advice
           (fork-fail ":keep serves the calls of a name past any advice on it, and ~s carries ~{~a~^, ~}: race a name with none"
                 symbol (mapcar #'car advice)))
          ((find (symbol-name symbol) (held-definitions) :key (lambda (entry) (getf entry :name)) :test #'string=)
           (fork-fail "~s is HELD, waiting on its first clean call: let one pass, or (revert '~(~s~)), first" symbol symbol))
          (t symbol))))

(defun fork-parse (arguments)
  "(values TASKS SECONDS FROM CHECK KEEP) of fork's ARGUMENTS: the tasks,
strings, then :SECONDS N, :FROM X, :CHECK C and :KEEP NAME; or a refusal."
  (let* ((at (or (position-if-not #'stringp arguments) (length arguments)))
         (tasks (subseq arguments 0 at))
         (options (nthcdr at arguments)))
    (unless (and (evenp (length options))
                 (loop for (key) on options by #'cddr
                       always (member key '(:seconds :from :check :keep))))
      (fork-fail "fork takes the tasks, strings, then :seconds N, :from X, :check C and :keep NAME; it was given ~s" options))
    (let* ((seconds (getf options :seconds (fork-setting :seconds)))
           (from (getf options :from))
           (check (getf options :check))
           (keep (getf options :keep))
           (back (and from (not (fork-turn-from-p from)))))
      (cond ((> (length tasks) +fork-most+)
             (fork-fail "fork makes at most ~d forks a call; it was given ~d tasks" +fork-most+ (length tasks)))
            ((some (lambda (task) (zerop (length (trimmed task)))) tasks)
             (fork-fail "a task is words, and one was blank"))
            ((not (and (integerp seconds) (plusp seconds)))
             (fork-fail ":seconds is a whole number of seconds above zero, not ~s" seconds))
            ((and from (not (or (fork-turn-from-p from)
                                (and (integerp from) (plusp from))
                                (and (consp from) (= 2 (length from))
                                     (every (lambda (n) (and (integerp n) (plusp n))) from)))))
             (fork-fail ":from is an earlier turn -- -N for N turns back, or its id -- or a generation G, or an (old new) pair of them; not ~s" from))
            ((and (or back keep) (null check))
             (fork-fail "going back through generations and racing a name are judged by a check: give :check, a form answering true or a shell command exiting 0"))
            ((and check (not (or back keep)))
             (fork-fail ":check judges a way back or a race: give :from G (a generation, or an (old new) pair) or :keep NAME beside it"))
            ((and (null tasks) (not back))
             (fork-fail "fork needs a task: (fork \"task\" ...)~:[~; -- a race needs versions, and each is a fork's~]" keep))
            (t (values tasks seconds from check (and keep (fork-keepable keep))))))))

(defun fork-earlier-turn (session turn from)
  "FROM -- -N, or a turn's id -- as a turn of SESSION before TURN, or a refusal."
  (let ((parents (session-composed-parents session)))
    (if (integerp from)
        (let ((at turn))
          (dotimes (back (- from) at)
            (setf at (gethash at parents))
            (unless at
              (fork-fail ":from ~d goes back past this session's first turn: ~d turn~:p came before this one" from back))))
        (cond ((equal from turn)
               (fork-fail ":from ~a is the turn running this call; leave :from out to fork here" from))
              ((not (nth-value 1 (gethash from parents)))
               (fork-fail ":from ~s is not a turn of this session" from))
              (t from)))))

(defun fork-start (session anchor n words task seconds rival)
  "Fork SESSION at ANCHOR as its Nth fork and put WORDS to it; RIVAL, when the
call races a name, is the name this fork writes its version under, claimed
for it before it runs. => the fork: (:TASK :N :SESSION :RIVAL :TURN), or,
when its turn could not start, :STATUS :FAILED and why."
  (let ((child (fork-name anchor n)))
    (create-session :id child :cwd (find-session-cwd session)
                    :parent session :anchor-turn-id anchor)
    ;; Titled as it is made, so its turn's admission never asks a model to name it.
    (record-event child +kind-session-title+
                  (list :title (format nil "fork ~d: ~a" n (clip (first (lines (trimmed task))) 60))))
    ;; Waited on before the submit: its turn's budget is read as it opens.
    (setf (gethash child *fork-waiting*) (list :seconds seconds :acts '()))
    (when rival (setf (gethash rival *fork-owners*) child))
    (handler-case
        (list :task task :n n :session child :rival rival
              :turn (active-input-admission-turn-id
                     (nle:submit child words :command-id (format nil "fork:~a" child)
                                 :source "fork" :source-id session)))
      (error (condition)
        (list :task task :n n :session child :rival rival :status :failed
              :answer (format nil "its turn could not start: ~a" condition))))))

(defun fork-release (session)
  "Every name SESSION claimed, unclaimed."
  (sb-ext:with-locked-hash-table (*fork-owners*)
    (let ((names (loop for name being the hash-keys of *fork-owners* using (hash-value owner)
                       when (equal owner session) collect name)))
      (dolist (name names) (remhash name *fork-owners*)))))

(defun fork-tasks (session anchor tasks words seconds rivals)
  "Fork SESSION at ANCHOR once per TASK, put each its WORDS, and wait for
every fork's turn to end. => (values RESULTS FORKS): RESULTS as fork
answers them, FORKS each (:N :SESSION :RIVAL :ACTS), what it defined."
  (let ((forks '())
        (call (fork-call-id)))
    ;; However this call ends, no fork outlives the wait for it, and no name
    ;; stays claimed by one; the shells hear how the forks ended.
    (with-cleanup ((fork-progress-end call)
                   (dolist (fork forks)
                         (let ((child (getf fork :session))
                               (started (getf fork :turn)))
                           (remhash child *fork-waiting*)
                           (fork-release child)
                           (when (and started (not (turn-terminal-kind started)))
                             (ignore-errors
                              (request-cancel-turn child started "the call waiting on it stopped"))))))
      (hook 'nle:turn-budget "fork" #'fork-budget)
      (loop for task in tasks
            for text in words
            for n from 1
            do (push (fork-start session anchor n text task seconds (nth (1- n) rivals)) forks))
      (setf forks (reverse forks))
      (fork-progress-begin call session forks)
      (let ((deadline (+ (get-internal-real-time)
                         (* (+ seconds *fork-grace-seconds*) internal-time-units-per-second))))
        (values (loop for fork in forks
                      ;; one that never started is already as answered
                      collect (fork-result fork (and (not (getf fork :status))
                                                (fork-await fork deadline seconds))))
                (loop for fork in forks
                      collect (list :n (getf fork :n) :session (getf fork :session)
                                    :rival (getf fork :rival)
                                    :acts (getf (gethash (getf fork :session) *fork-waiting*) :acts))))))))

(defun fork-await (fork deadline seconds)
  "Wait for FORK's turn to end, or for DEADLINE (internal real time) to pass.
=> the turn's terminal kind, or :TIMEOUT once it has been asked to stop."
  (let ((child (getf fork :session))
        (turn (getf fork :turn)))
    ;; the log says when it ends, read four times a second
    (or (wait-until (lambda () (turn-terminal-kind turn)) deadline 0.25)
        (progn
          (request-cancel-turn child turn (format nil "the fork ran past its ~d seconds" seconds))
          ;; its last words land with its ending
          (wait-until (lambda () (turn-terminal-kind turn)) (deadline-after 10) 0.1)
          :timeout))))

(defun fork-ran (child turn)
  "The first line of each eval CHILD's TURN ran -- its note -- in order, or the
name of any other tool it called."
  (loop for payload in (events :session-id child :turn-id turn
                                   :kind +kind-tool-call-started+ :as :payloads)
        for form = (json-value (ignore-errors
                                    (decode-json (json-value payload :string "arguments")))
                                   :string "form")
        collect (if (and form (plusp (length (trimmed form))))
                    (clip (trimmed (first (lines (trimmed form)))) 120)
                    (or (json-value payload :string "tool-name") "a tool"))))

(defun fork-ending (child turn)
  "What CHILD's TURN said as it failed or was cancelled, or NIL."
  (json-value (events :session-id child :turn-id turn
                              :kind '("turn.failed" "turn.cancelled") :as :payload)
                  :string "detail"))

(defun fork-result (fork kind)
  "FORK as fork answers it, its turn having ended as KIND; one whose turn
never started, as it stands."
  (let ((child (getf fork :session))
        (turn (getf fork :turn))
        (status (getf fork :status)))
    (list :task (getf fork :task) :session child
          :status (or status (fork-turn-status kind))
          :answer (if status (getf fork :answer) (or (turn-answer child turn) (fork-ending child turn)))
          :ran (unless status (fork-ran child turn)))))

(defun fork-turn-status (kind)
  "KIND, the kind of the event a fork's turn ended on, or :TIMEOUT, as the
fork's status: :timeout, :completed, :failed or :cancelled."
  (cond ((eq kind :timeout) :timeout)
        ((equal kind "turn.completed") :completed)
        ((equal kind "turn.failed") :failed)
        (t :cancelled)))

;;; --- live progress: how a call's forks stand, for the shells it shows in ---
;;; A fork works in a session of its own, so the parent's transcript holds only
;;; the call that made it. This registry is what the gateway reads (its
;;; background watcher, gateway.lisp) to tell the parent's shells, live, how
;;; each fork stands: running or how it ended, the calls it made, the note of
;;; its newest, and once it ends, its time and its answer's first line.
;;; Retention: live-only, as the background band is; each fork's own session
;;; keeps the durable truth.

(defstruct fork-call
  "One running fork call: the SESSION it runs in, its FORKS as FORK-TASKS holds
them, when it STARTED, each fork's ENDED (N . MS) as first seen, the frame last
SENT (its JSON text) and PAYLOAD, when it was last READ, and FINAL once the
call has returned."
  session forks (started (get-internal-real-time)) (ended '()) sent payload (read 0) final)

(defvar *fork-calls* (make-hash-table :test #'equal :synchronized t)
  "The tool call id of each fork call whose shells are still to hear how it
ended -> its FORK-CALL.")

(defparameter *fork-progress-seconds* 0.5
  "How often FORK-COLLECT-PROGRESS reads a running call's forks again.")

(defun fork-call-id ()
  "The id of the eval call running this fork, or NIL outside a tool call."
  (let ((id (and (boundp 'nle::*executing-tool-call-id*)
                 (symbol-value 'nle::*executing-tool-call-id*))))
    (and (stringp id) (plusp (length id)) id)))

(defun fork-progress-begin (id session forks)
  "Register call ID's FORKS, run from SESSION, for the shells to hear of."
  (when id
    (sb-ext:with-locked-hash-table (*fork-calls*)
      ;; A call no watcher ever collected (an image with no gateway) goes
      ;; once it is old, so the table never grows past the calls running.
      (loop with old = (- (get-internal-real-time) (* 600 internal-time-units-per-second))
            for key being the hash-keys of *fork-calls* using (hash-value call)
            when (and (fork-call-final call) (< (fork-call-started call) old))
              do (remhash key *fork-calls*))
      (setf (gethash id *fork-calls*) (make-fork-call :session session :forks forks)))))

(defun fork-progress-end (id)
  "Call ID has returned: its next collect says how its forks ended, last."
  (when-let (call (and id (gethash id *fork-calls*)))
    (setf (fork-call-final call) t)))

(defun fork-progress-note (line)
  "A fork's call note as a shell lists it: without its ;; and without the
`forkN:' a fork may open it with to tell its calls from its siblings'."
  (let* ((text (string-left-trim "; " (or line "")))
         (rest (and (>= (length text) 5) (string-equal "fork" text :end2 4)
                    (string-left-trim " " (subseq text 4))))
         (colon (and rest (plusp (length rest)) (digit-char-p (char rest 0))
                     (position #\: rest :end (min (length rest) 40)))))
    (if colon (string-left-trim " " (subseq rest (1+ colon))) text)))

(defun fork-progress-answer (text)
  "The first line of TEXT that says something, its Markdown marks taken off."
  (when-let (line (find-if (lambda (line) (find-if #'alphanumericp line)) (lines (or text ""))))
    (clip (trimmed (string-left-trim "#->* " (remove-if (lambda (char) (member char '(#\* #\`))) line)))
          200 :ellipsis "…")))

(defun fork-progress-row (fork call now)
  "FORK of CALL as its shells draw it at NOW (internal real time)."
  (let* ((n (getf fork :n))
         (child (getf fork :session))
         (turn (getf fork :turn))
         (kind (cond ((getf fork :status) "turn.failed")
                     (turn (turn-terminal-kind turn))))
         (ran (and turn (fork-ran child turn)))
         (ended (and kind (or (cdr (assoc n (fork-call-ended call)))
                              (let ((ms (round (* 1000 (- now (fork-call-started call)))
                                               internal-time-units-per-second)))
                                (push (cons n ms) (fork-call-ended call))
                                ms)))))
    (json-object "n" n
                 "label" (clip (trimmed (or (first (lines (trimmed (or (getf fork :task) "")))) "")) 60 :ellipsis "…")
                 "status" (if kind (string-downcase (fork-turn-status kind)) "running")
                 "calls" (length ran)
                 :opt "note" (and ran (fork-progress-note (car (last ran))))
                 :opt "ms" ended
                 :opt "answer" (and kind (fork-progress-answer
                                          (or (getf fork :answer)
                                              (and turn (or (turn-answer child turn) (fork-ending child turn)))))))))

(defun fork-collect-progress (&aux (now (get-internal-real-time)) (frames '()))
  "A frame for each running fork call whose forks stand otherwise than its last
frame said, read at most every *FORK-PROGRESS-SECONDS*, and one last for each
call that has returned: => (SESSION PAYLOAD) each, the session_fork_progress
frames the gateway's watcher publishes."
  (dolist (entry (sb-ext:with-locked-hash-table (*fork-calls*)
                   (loop for id being the hash-keys of *fork-calls* using (hash-value call)
                         when (or (fork-call-final call)
                                  (>= (- now (fork-call-read call))
                                      (* *fork-progress-seconds* internal-time-units-per-second)))
                           collect (cons id call))))
    (destructuring-bind (id . call) entry
      (let* ((final (fork-call-final call))
             (payload (ignore-errors
                       (json-object "session_id" (fork-call-session call)
                                    "tool_call_id" id
                                    "forks" (coerce (loop for fork in (fork-call-forks call)
                                                          collect (fork-progress-row fork call now))
                                                    'vector)
                                    :when final "done" t)))
             (text (and payload (encode-json-object payload))))
        (setf (fork-call-read call) now)
        (when (and text (not (equal text (fork-call-sent call))))
          (setf (fork-call-sent call) text
                (fork-call-payload call) payload)
          (push (list (fork-call-session call) payload) frames))
        (when final (remhash id *fork-calls*)))))
  (nreverse frames))

(defun fork-progress-snapshots (session)
  "The last frame of each of SESSION's fork calls still running, for a shell
that attaches while they run."
  (sb-ext:with-locked-hash-table (*fork-calls*)
    (loop for call being the hash-values of *fork-calls*
          when (and (equal session (fork-call-session call))
                    (fork-call-payload call)
                    (not (fork-call-final call)))
            collect (fork-call-payload call))))

;;; --- what a fork may define ------------------------------------------------------
;;; A fork runs in the operator's own image: what it evaluates is live there at
;;; once. So it writes only names of its own -- displacing one the image or a
;;; sibling stands on would break both -- and the layer is the parent's.

(defun fork-act-name (act)
  "The name ACT, a defining or forgetting form, defines or forgets: a symbol
or a (SETF symbol) list, or NIL."
  (let ((raw (second act)))
    (cond ((and (consp raw) (eq (first raw) 'quote)) (second raw))
          ((and (consp raw) (eq (first raw) 'setf)) raw)
          ((consp raw) (and (symbolp (first raw)) (first raw)))
          (t raw))))

(defun fork-standing-name-p (name)
  "Whether NAME already answers in this image: a function, a variable or a
class."
  (typecase name
    (null nil)
    (keyword nil)
    (symbol (and (or (fboundp name) (boundp name) (find-class name nil)) t))
    (cons (and (ignore-errors (fboundp name)) t))
    (t nil)))

(defun fork-fence (form)
  "Before FORM evaluates, when a fork's eval runs it (DEFINITION-ESCROW asks):
a fork defines and forgets only names of its own -- one that already answers
in the image, or that a sibling claimed, is refused and the form never runs;
a name new to the image is claimed for the fork. Every other session's form
passes untouched."
  (let ((session *scribe-session-id*))
    (when (fork-session-p session)
      (let ((claimed '()))
        (walk-toplevel-acts
         form
         (lambda (act)
           (let* ((name (fork-act-name act))
                  (owner (and name (gethash name *fork-owners*))))
             (cond ((or (null name) (equal owner session)))
                   (owner
                    (fork-fail "~(~s~) is another fork's: give yours a name of its own. The form was not evaluated." name))
                   ((fork-standing-name-p name)
                    (fork-fail "~(~s~) already answers in the operator's image, which every fork runs in, so a fork may not define or forget it: give yours a name of its own. The form was not evaluated." name))
                   ((definition-key act) (push name claimed))))))
        (dolist (name claimed) (setf (gethash name *fork-owners*) session))))))

(defun fork-record (form session-id)
  "Whether FORM, evaluated in SESSION-ID, is a fork's -- which the scribe never
writes (RECORD-DEFINITIONS asks): what a fork defines stays live in the image,
the parent keeping what it chooses, and is noted for the call waiting on the
fork, its eval told so. => T for a fork's form, NIL for every other."
  (when (fork-session-p session-id)
    (let ((acts '()))
      (walk-toplevel-acts form (lambda (act) (when (definition-key act) (push act acts))))
      (dolist (act acts) (setf (gethash (fork-act-name act) *fork-made*) t))
      (when (gethash session-id *fork-waiting*)
        (setf (getf (gethash session-id *fork-waiting*) :acts)
              (append acts (getf (gethash session-id *fork-waiting*) :acts))))
      (when acts
        (let ((names (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase))
                       (mapcar (lambda (act) (prin1-to-string (fork-act-name act))) (reverse acts)))))
          (format t "~&[fork: ~{~a~^, ~} ~:[is~;are~] live in this image and not kept: the parent keeps what it chooses]~%"
                  names (rest names))))
      t)))

(defun fork-unkept (before)
  "Whether the eval REPORT-UNKEPT-CHANGES would tell what its form changed that
the layer will not carry is told nothing: a fork's is -- what a fork defines
is never kept, and FORK-RECORD says so. In any other eval, each name a fork
defined is taken into BEFORE's snapshot as it stands now, so the window reads
it as no change of that eval's form. => T for a fork's eval, NIL for every
other."
  (cond ((fork-session-p *scribe-session-id*) t)
        ((and before (plusp (hash-table-count *fork-made*)))
         ;; the window takes each such name as it stands now: unchanged
         (let ((snapshot (car before)))
           (dolist (name (sb-ext:with-locked-hash-table (*fork-made*)
                           (loop for name being the hash-keys of *fork-made* collect name)))
             (let ((symbol (if (consp name) (second name) name)))
               (when (symbolp symbol)
                 (loop for (kind object) on (definition-objects symbol) by #'cddr
                       when (and object (eq (eq kind :setf) (consp name)))
                         do (setf (gethash (cons kind symbol) snapshot) object))))))
         nil)
        (t nil)))

;;; --- a check, in a fresh process -------------------------------------------------

(defvar *fork-ask* 'fork-ask-at-once
  "How checks are put to fresh processes: (lambda (JOBS DIRECTORY)) => a row a
job, as FORK-ASK-AT-ONCE answers them. A test's stub.")

(defun fork-check-said (check)
  "CHECK as the words that name it."
  (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase) (*print-pretty* t))
    (if (stringp check)
        (format nil "the shell command ~s exits 0" check)
        (format nil "~s answers true" check))))

(defun fork-check-form (check directory &optional version)
  "What a fresh process answers for CHECK, VERSION -- a DEFUN -- evaluated first:
1 when it passes, 0 when it does not, -1 when it signals. A string is a shell
command, run in DIRECTORY, that passes by exiting 0."
  `(handler-case
       (progn ,@(and version (list version))
              ,(if (stringp check)
                   `(if (eql 0 (nth-value 2 (run-bounded (list "sh" "-c" ,check)
                                                             :seconds 120 :directory ,directory)))
                        1 0)
                   `(if ,check 1 0)))
     (error () -1)))

(defun fork-form-text (form)
  "FORM as text that reads back the same in any package: every symbol written
with its own."
  (with-standard-io-syntax
    (let ((*package* (find-package '#:keyword))
          (*print-circle* t))
      (prin1-to-string form))))

(defun fork-answer-words (answer)
  "Why a fresh process's ANSWER to a check is no pass, in words."
  (case (getf answer :score)
    (0 "the check answered false")
    (-1 "the check signalled")
    (t (format nil "the check could not be asked: ~a" (or (getf answer :detail) "no answer")))))

(defun fork-since (began)
  "Seconds since BEGAN, an internal real time."
  (/ (float (- (get-internal-real-time) began)) internal-time-units-per-second))

(defun fork-in-scratch (function)
  "FUNCTION called on a fresh directory under this home's tmp/, deleted after."
  (let ((directory (uiop:ensure-directory-pathname
                    (home-path (format nil "tmp/fork-~36r/" (random (expt 36 10) (make-random-state t)))))))
    (ensure-directories-exist directory)
    (unwind-protect (funcall function directory)
      (ignore-errors (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))

(defun fork-ask-at-once (jobs directory)
  "Each JOB, (GENERATION TEXT): GENERATION laid into a throwaway home of its own
under DIRECTORY, one after another, then every TEXT asked at once, each in a
fresh process of this organism under its home (TRIAL). => a row a job, in
order: (:SCORE S :DETAIL D :NOTES N :SECONDS T), S the number the process
answered, or NIL when it answered none."
  (let* ((seconds *fork-probe-seconds*)
         (homes (loop for (generation) in jobs
                      for k from 1
                      collect (merge-pathnames (format nil "~d-g~d/" k generation) directory)))
         (laid (loop for (generation) in jobs
                     for home in homes
                     collect (handler-case
                                 (list :notes (nth-value 1 (build-generation
                                                            generation (merge-pathnames "cells/" home))))
                               (error (condition)
                                 (list :failed (format nil "generation ~d could not be laid: ~a" generation condition))))))
         (threads (loop for (generation text) in jobs
                        for home in homes
                        for lay in laid
                        collect (let ((home home) (text text) (lay lay))
                                  (bt2:make-thread
                                   (lambda ()
                                     (if (getf lay :failed)
                                         (list :score nil :detail (getf lay :failed) :seconds 0.0)
                                         (let ((began (get-internal-real-time)))
                                           (handler-case
                                               (multiple-value-bind (score detail)
                                                   (trial text :home home :seconds seconds)
                                                 (list :score score :detail detail :seconds (fork-since began)))
                                             (error (condition)
                                               (list :score nil :detail (princ-to-string condition)
                                                     :seconds (fork-since began)))))))
                                   :name (format nil "fork-trial-g~d" generation))))))
    (unwind-protect
         (loop for thread in threads
               for lay in laid
               collect (append (bt2:join-thread thread) (list :notes (getf lay :notes))))
      (dolist (home homes)
        (ignore-errors (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))))

;;; --- the way back ----------------------------------------------------------------

(defun fork-spread (items width)
  "At most WIDTH of ITEMS, evenly spaced across them."
  (let ((count (length items)))
    (if (<= count width)
        items
        (loop for k from 1 to width collect (nth (floor (* k count) (1+ width)) items)))))

(defun fork-bisect (numbers ask &optional (width +fork-width+))
  "Where NUMBERS -- generations, ascending, two at least -- turn from passing to
failing. ASK takes some of them and answers a row each, asked at once:
(:SCORE S ...), S 1 passing, 0 or -1 failing, NIL unanswered. Both ends go
first, then up to WIDTH numbers between the newest pass and the oldest
failure, a round at a time, until no unasked one lies between; one that
answers nothing is stepped over, never read as the break. The step found is
asked again, both its ends: a fresh boot that answers otherwise the second
time names no step. => (:VERDICT V :GOOD G :BAD B :ROUNDS R :TESTED ROWS
:AGAIN ROWS): V :FOUND, :UNSTEADY when the step's second answers differ,
:CLEAN when the newest passes, :PROGRAM when the oldest fails too,
:UNANSWERED when an end answered nothing."
  (let ((all (coerce numbers 'vector))
        (rows (make-hash-table))
        (rounds 0))
    (labels ((ask (indices)
               (incf rounds)
               (loop for index in indices
                     for row in (funcall ask (mapcar (lambda (index) (aref all index)) indices))
                     do (setf (gethash index rows) (list* :number (aref all index) row))))
             (score (index) (getf (gethash index rows) :score))
             (passed (index) (eql 1 (score index)))
             (failed (index) (member (score index) '(0 -1)))
             (answer (verdict good bad)
               (list :verdict verdict
                     :good (and good (aref all good)) :bad (and bad (aref all bad))
                     :rounds rounds
                     :tested (sort (loop for row being the hash-values of rows collect row)
                                   #'< :key (lambda (row) (getf row :number))))))
      (let ((good 0) (bad (1- (length all))))
        (ask (list good bad))
        (cond ((passed bad) (answer :clean bad nil))
              ((failed good) (answer :program nil good))
              ((not (and (passed good) (failed bad))) (answer :unanswered nil nil))
              (t (loop for between = (loop for index from (1+ good) below bad
                                           unless (nth-value 1 (gethash index rows)) collect index)
                       while between
                       do (ask (fork-spread between width))
                          (let ((failure (loop for index from (1+ good) below bad
                                               when (failed index) return index)))
                            (when failure (setf bad failure)))
                          (let ((pass (loop for index downfrom (1- bad) above good
                                            when (passed index) return index)))
                            (when pass (setf good pass))))
                 (incf rounds)
                 (let ((again (loop for index in (list good bad)
                                    for row in (funcall ask (list (aref all good) (aref all bad)))
                                    collect (list* :number (aref all index) row))))
                   (append (answer (if (and (eql 1 (getf (first again) :score))
                                            (member (getf (second again) :score) '(0 -1)))
                                       :found
                                       :unsteady)
                                   good bad)
                           (list :again again)))))))))

(defun fork-manifest (number)
  "Generation NUMBER's manifest."
  (find number (manifests) :key #'manifest-number))

(defun fork-generations (from)
  "The generations FROM spans, ascending: G to the newest, or an (OLD NEW) pair;
a refusal when they are fewer than two."
  (let* ((sealed (sort (mapcar #'manifest-number (manifests)) #'<))
         (old (if (consp from) (reduce #'min from) from))
         (new (if (consp from) (reduce #'max from) (car (last sealed))))
         (numbers (remove-if-not (lambda (number) (and new (<= old number new))) sealed)))
    (when (< (length numbers) 2)
      (fork-fail "a way back needs two sealed generations to tell apart, and ~s spans ~d of this home's ~d"
            from (length numbers) (length sealed)))
    numbers))

(defun fork-change-line (change)
  "One change between two generations, as a line."
  (flet ((short (revision) (and (stringp revision) (subseq revision 0 (min 7 (length revision)))))
         (subject (commit) (format nil "~a ~a" (getf commit :sha) (clip (getf commit :subject) 90))))
    (let ((row (getf change :row))
          (before (getf change :before)))
      (case (getf change :change)
        (:binary (format nil "the binary ~a -> ~a" (getf change :from) (getf change :to)))
        (:added (format nil "~a added" (getf row :name)))
        (:removed (format nil "~a taken away" (getf row :name)))
        (t (format nil "~a ~:[~a -> ~a~;~2*edited~]~:[~; (uncommitted edits)~]~@[: ~{~a~^; ~}~]"
                   (getf row :name)
                   (equal (getf before :revision) (getf row :revision))
                   (short (getf before :revision)) (short (getf row :revision))
                   (getf row :dirty)
                   (mapcar #'subject (getf change :keeps))))))))

(defun fork-bisect-report (numbers found check)
  "FOUND, what BISECT answered over NUMBERS, as the :BISECT report."
  (let* ((verdict (getf found :verdict))
         (good (getf found :good))
         (bad (getf found :bad))
         (tested (getf found :tested))
         (changes (and (eq verdict :found)
                       (mapcar #'fork-change-line (generation-changes (fork-manifest good) (fork-manifest bad)))))
         (silent (loop for row in tested unless (getf row :score) collect (getf row :number)))
         (ends (intersection silent (list (first numbers) (car (last numbers)))))
         (said (fork-check-said check)))
    (list :verdict verdict :range (list (first numbers) (car (last numbers)))
          :good good :bad bad :changes changes :rounds (getf found :rounds) :tested tested
          :again (getf found :again)
          :says (ecase verdict
                  (:found
                   (format nil "~a: it held at generation ~d, and had gone by ~d, both asked twice~@[ (~{~d~^, ~} between could not be asked)~]. ~:[No folder moved in that step, so the break is outside what a generation records.~;What moved: ~:*~{~a~^; ~}.~]"
                           said good bad
                           (remove-if-not (lambda (number) (< good number bad)) silent)
                           changes))
                  (:unsteady
                   (format nil "~a held at generation ~d and had gone by ~d, but asked again a fresh boot of them answered ~{~a~^ and ~}: the check does not answer the same way twice, so no step is named. A check that reads what a boot evaluates -- a layer variable fetched at load, the network -- answers that way; check what a boot cannot vary."
                           said good bad
                           (mapcar (lambda (row) (format nil "~d: ~a" (getf row :number) (getf row :score)))
                                   (getf found :again))))
                  (:clean
                   (format nil "~a holds at the newest generation, ~d: nothing this home sealed explains the break, so it lies outside what a generation records -- the box, a secret, the world the work runs in."
                           said good))
                  (:program
                   (format nil "~a fails as far back as generation ~d, the oldest asked: every generation ran on today's binary, so no change this home sealed explains it, and the program itself is to blame. Rebuilding old binaries is not built yet."
                           said bad))
                  (:unanswered
                   (format nil "generation~p ~{~d~^ and ~} answered nothing (~a), and a way back needs both ends to answer: give a range whose ends boot."
                           (length ends) ends
                           (getf (find (first ends) tested :key (lambda (row) (getf row :number))) :detail)))))))

(defun fork-way-back (from check session)
  "The way back FROM spans -- G to the newest, or an (OLD NEW) pair -- judged by
CHECK in SESSION's folder. => the :BISECT report."
  (let* ((numbers (fork-generations from))
         (text (fork-form-text (fork-check-form check (find-session-cwd session))))
         (found (fork-in-scratch
                 (lambda (scratch)
                   (fork-bisect numbers
                           (lambda (batch)
                             (funcall *fork-ask* (mapcar (lambda (number) (list number text)) batch) scratch)))))))
    (fork-bisect-report numbers found check)))

;;; --- the race --------------------------------------------------------------------

(defparameter +fork-race-calls+ 20 "The live calls each entrant of a race is timed on.")

(defparameter +fork-race-least+ 8 "The timed calls below which an entrant's time is not read.")

(defparameter *fork-race-seconds* 300 "How long a race listens for live calls.")

(defparameter +fork-race-faster+ 0.05 "How much faster a version has to be to displace what stands.")

(defvar *fork-serving* nil
  "True on a thread inside a call a race serves: a call made within it is what
stands' own, untimed.")

(defun fork-rival-name (n keep)
  "The name fork N writes its version of KEEP under: FORK-<n>/KEEP, in KEEP's
package."
  (intern (format nil "FORK-~d/~a" n (symbol-name keep)) (symbol-package keep)))

(defun fork-keep-words (keep rival check)
  "What a :KEEP call adds to the task of the fork writing RIVAL."
  (let ((*package* (find-package '#:nodecode.evolved))
        (*print-case* :downcase))
    (let ((name (prin1-to-string keep))
          (lambda-list (ignore-errors (sb-kernel:%fun-lambda-list (fdefinition keep)))))
      (format nil *fork-keep-input* name (prin1-to-string rival) name
              (and (consp lambda-list) (prin1-to-string lambda-list))
              name (fork-check-said check) name))))

(defun fork-version (fork keep)
  "What FORK wrote of KEEP -- its newest DEFUN of its own name for it, that name
KEEP again throughout -- as a version, or NIL when it wrote none."
  (let* ((rival (getf fork :rival))
         (act (find-if (lambda (act) (and (eq rival (fork-act-name act))
                                          (string= "DEFUN" (symbol-name (first act)))))
                       (getf fork :acts))))
    (and act (list :label (format nil "fork ~d" (getf fork :n))
                   :source (getf fork :session)
                   :form (subst keep rival act)))))

(defun fork-generation-version (generation keep)
  "KEEP as GENERATION's layer had it, as a version -- read from that
generation's copy of the layer file that defines KEEP today -- or NIL when
KEEP is no layer definition, or that file had none."
  (let* ((folder (format nil "~a/" (layer-name)))
         (path (first (first (definition-location keep))))
         (relative (and path (uiop:subpathp path (merge-pathnames folder *cells-directory*)))))
    (when relative
      (fork-in-scratch
       (lambda (scratch)
         (let ((cells (merge-pathnames "cells/" scratch)))
           (build-generation generation cells)
           (let* ((file (merge-pathnames relative (merge-pathnames folder cells)))
                  (hit (and (probe-file file)
                            (first (file-definitions file :name (symbol-name keep) :read-eval t))))
                  (form (and hit
                             (ignore-errors
                              (destructuring-bind (key fork-start end package) hit
                                (declare (ignore key))
                                (with-standard-io-syntax
                                  (let ((*package* package) (*read-eval* t))
                                    (read-from-string (uiop:read-file-string file) t nil
                                                      :start fork-start :end end))))))))
             (and (consp form) (string= "DEFUN" (symbol-name (first form))) (eq keep (second form))
                  (list :label (format nil "generation ~d" generation)
                        :source (format nil "generation ~d's layer" generation)
                        :form form)))))))))

(defun fork-versions (keep forks fork-bisect)
  "What races in KEEP's place: each fork's version, then, beside a way back that
found its step, KEEP as the last good generation's layer had it."
  (append (remove nil (mapcar (lambda (fork) (fork-version fork keep)) forks))
          (and (eq :found (getf fork-bisect :verdict))
               (remove nil (list (fork-generation-version (getf fork-bisect :good) keep))))))

(defun fork-misfit (form standing)
  "Why FORM cannot stand in for STANDING -- it is no DEFUN, or it does not take
every call STANDING's lambda list takes -- or NIL."
  (let ((old (ignore-errors (sb-kernel:%fun-lambda-list standing)))
        (new (and (consp form) (third form))))
    (cond ((not (and (consp form) (string= "DEFUN" (symbol-name (first form))) (listp new)))
           "it is not a defun")
          ((and (listp old) (not (takes-every-call-p new old)))
           (let ((*package* (or (symbol-package (second form)) *package*)) (*print-case* :downcase))
             (format nil "it takes ~s where every call of it today takes ~s" new old))))))

(defun fork-version-function (form keep)
  "FORM, a DEFUN of KEEP, made a function bound to no name -- its own calls of
KEEP its own -- or NIL when it does not compile clean in this image."
  (destructuring-bind (lambda-list &rest body) (cddr form)
    (handler-case
        (multiple-value-bind (function warned failed)
            (handler-bind ((warning #'muffle-warning) (sb-ext:compiler-note #'muffle-warning))
              (compile nil `(lambda (&rest arguments)
                              (labels ((,keep ,lambda-list ,@body))
                                (apply #',keep arguments)))))
          (declare (ignore warned))
          (and (not failed) function))
      (error () nil))))

(defun fork-median (times)
  "The middle of TIMES, or NIL for none."
  (let ((sorted (sort (copy-list times) #'<))
        (count (length times)))
    (cond ((zerop count) nil)
          ((oddp count) (nth (floor count 2) sorted))
          (t (/ (+ (nth (1- (floor count 2)) sorted) (nth (floor count 2) sorted)) 2)))))

(defun fork-nanoseconds ()
  "The monotonic clock, in nanoseconds: fine enough to time one call, which
GET-INTERNAL-REAL-TIME is not -- SBCL reads Linux's coarse clock for it, one
kernel tick (1 ms here), so a 100 us call timed with it reads 0. Windows'
SB-UNIX has no CLOCK-GETTIME: there it is internal real time, scaled."
  #-win32 (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
            (+ (* seconds 1000000000) nanoseconds))
  #+win32 (* (get-internal-real-time) (floor 1000000000 internal-time-units-per-second)))

(defun fork-race-live (keep entrants)
  "Serve KEEP's live calls among ENTRANTS -- (:LABEL L :FUNCTION F), or
(:LABEL L :STANDING T) for what stands -- one entrant a call in turn, each
call timed, until each has +FORK-RACE-CALLS+ calls or *FORK-RACE-SECONDS* pass. A call
made inside a served one is what stands' own, untimed; a version that
signals is out, and that call is served again by what stands. => a record an
entrant: (:LABEL :STANDING :CALLS :MEDIAN-US :BROKEN)."
  (let* ((lock (bt2:make-lock :name "fork-race"))
         (records (mapcar (lambda (entrant) (list* :calls 0 :times '() :broken nil entrant)) entrants))
         (ring (coerce records 'vector))
         (tick 0)
         (deadline (+ (get-internal-real-time) (* *fork-race-seconds* internal-time-units-per-second))))
    (labels ((open-p (record)
               (and (not (getf record :broken)) (< (getf record :calls) +fork-race-calls+)))
             (pick ()
               (bt2:with-lock-held (lock)
                 (loop repeat (length ring)
                       for record = (aref ring (mod (incf tick) (length ring)))
                       when (open-p record) return record)))
             (timed (record thunk)
               (let ((began (fork-nanoseconds)))
                 (multiple-value-prog1 (let ((*fork-serving* t)) (funcall thunk))
                   (let ((micros (/ (- (fork-nanoseconds) began) 1000.0)))
                     (bt2:with-lock-held (lock)
                       (when (< (getf record :calls) +fork-race-calls+)
                         (incf (getf record :calls))
                         (push micros (getf record :times))))))))
             (serve (next arguments)
               (let ((record (and (not *fork-serving*) (pick))))
                 (cond ((null record) (apply next arguments))
                       ((getf record :standing) (timed record (lambda () (apply next arguments))))
                       (t (let ((broken (block version
                                          (handler-bind
                                              ((error (lambda (condition)
                                                        ;; Esc unwinding a turn is no verdict on the version
                                                        (unless (typep condition 'turn-cancelled-condition)
                                                          (return-from version condition)))))
                                            (return-from serve
                                              (timed record (lambda () (apply (getf record :function) arguments))))))))
                            (bt2:with-lock-held (lock)
                              (setf (getf record :broken)
                                    (handler-case (princ-to-string broken)
                                      (error () (princ-to-string (type-of broken))))))
                            (apply next arguments)))))))
      (hook keep "fork-race" (lambda (next &rest arguments) (serve next arguments)))
      (unwind-protect
           (loop until (or (>= (get-internal-real-time) deadline) (notany #'open-p records))
                 do (sleep 0.1))
        (unhook keep "fork-race"))
      (bt2:with-lock-held (lock)
        (loop for record in records
              collect (list :label (getf record :label) :standing (getf record :standing)
                            :calls (getf record :calls) :median-us (fork-median (getf record :times))
                            :broken (getf record :broken)))))))

(defun fork-timed-records (records)
  "RECORDS timed enough to read: never broken, +FORK-RACE-LEAST+ calls at least."
  (remove-if (lambda (record) (or (getf record :broken) (< (getf record :calls) +fork-race-least+)))
             records))

(defun fork-fastest (records)
  "The version among RECORDS timed enough to read with the least median, or NIL."
  (first (sort (remove-if (lambda (record) (getf record :standing)) (fork-timed-records records))
               #'< :key (lambda (record) (getf record :median-us)))))

(defun fork-choose (standing-passed passing records)
  "The label of the version that wins, or NIL. What stands having passed the
check, the fastest version timed, if +FORK-RACE-FASTER+ faster than it; what stands
having failed it, the fastest version timed, else the first of PASSING that
never signalled."
  (let ((standing (find-if (lambda (record) (getf record :standing)) (fork-timed-records records)))
        (best (fork-fastest records)))
    (cond (standing-passed
           (and best standing
                (< (getf best :median-us) (* (- 1 +fork-race-faster+) (getf standing :median-us)))
                (getf best :label)))
          (best (getf best :label))
          (t (loop for label in passing
                   unless (getf (find label records :key (lambda (record) (getf record :label)) :test #'equal)
                                :broken)
                     return label)))))

(defun fork-swap (keep form)
  "FORM, a DEFUN of KEEP, live in KEEP's place and HELD (HOLD-DEFINITIONS):
written to the layer after one clean call made outside an eval, the
definition it displaced put back if that call signals."
  (let* ((previous (fdefinition keep))
         (*package* (symbol-package keep))
         (text (with-standard-io-syntax
                 (let ((*package* (symbol-package keep)) (*print-case* :downcase)
                       (*print-pretty* t) (*print-right-margin* 100) (*print-readably* nil))
                   (prin1-to-string form)))))
    (handler-bind ((warning #'muffle-warning) (sb-ext:compiler-note #'muffle-warning))
      (eval form))
    ;; its own words say the name runs on the eval's stack; the race's report says what is true
    (let ((*standard-output* (make-broadcast-stream)))
      (hold-definitions (list (list (symbol-name keep) keep previous)) form text (length text)
                            :session-id *scribe-session-id*))
    text))

(defun fork-race-says (keep standing-passed passing records winner)
  "The race's answer, in words."
  (let* ((name (let ((*package* (find-package '#:nodecode.evolved)) (*print-case* :downcase))
                 (prin1-to-string keep)))
         (won (find winner records :key (lambda (record) (getf record :label)) :test #'equal))
         (standing (find-if (lambda (record) (getf record :standing)) records))
         (best (fork-fastest records))
         (broken (loop for record in records when (getf record :broken) collect (getf record :label)))
         (calls (reduce #'+ records :key (lambda (record) (getf record :calls)))))
    (flet ((us (record) (if (and record (getf record :median-us))
                            (format nil "~,1f us" (getf record :median-us))
                            "untimed")))
      (cond
        ((and winner standing-passed)
         (format nil "~a's version is live in ~a's place, HELD: ~a a call against ~a for what stands, over ~d and ~d live calls. One clean call outside an eval writes it to the layer; (revert '~a) puts the old one back until then."
                 winner name (us won) (us standing) (getf won :calls) (getf standing :calls) name))
        (winner
         (format nil "what stands fails the check and ~a's version passes it, so it is live in ~a's place, HELD~@[ (~a a call)~]. One clean call outside an eval writes it to the layer; (revert '~a) puts the old one back until then."
                 winner name (and won (getf won :median-us) (us won)) name))
        ((null passing)
         (format nil "no version passed the check~:[, and what stands fails it too~;~], so ~a stays as it was."
                 standing-passed name))
        ((and best standing (>= (getf standing :calls) +fork-race-least+))
         (format nil "nothing beat what stands by ~d%: it takes ~a a call and the best version, ~a, ~a, so ~a stays as it was."
                 (round (* 100 +fork-race-faster+)) (us standing) (getf best :label) (us best) name))
        (t
         (format nil "~a was called ~d time~:p while the race listened (~d s), too few to time ~d version~:p against what stands, ~d calls each at least~@[; ~{~a~^, ~} signalled on a live call~], so it stays as it was."
                 name calls *fork-race-seconds* (length passing) +fork-race-least+ broken))))))

(defun fork-race (keep fork-versions check session generation)
  "Race VERSIONS -- (:LABEL :SOURCE :FORM), FORM a DEFUN of KEEP -- against what
stands, judged by CHECK: each that takes every call KEEP takes is put to
CHECK in a fresh process laid at GENERATION, beside what stands; those that
pass are timed on KEEP's live calls (FORK-RACE-LIVE); the winner goes in KEEP's
place, HELD (FORK-SWAP). => the :RACE report."
  (let* ((standing (fdefinition keep))
         (directory (find-session-cwd session))
         (shaped (loop for version in fork-versions
                       for why = (fork-misfit (getf version :form) standing)
                       collect (if why (list* :out why version) version)))
         (fit (remove-if (lambda (row) (getf row :out)) shaped))
         (answers (and fork-versions
                       (fork-in-scratch
                        (lambda (scratch)
                          (funcall *fork-ask*
                                   (cons (list generation (fork-form-text (fork-check-form check directory)))
                                         (loop for row in fit
                                               collect (list generation
                                                             (fork-form-text (fork-check-form check directory (getf row :form))))))
                                   scratch)))))
         (standing-passed (eql 1 (getf (first answers) :score)))
         (tried (loop for row in fit
                      for answer in (rest answers)
                      collect (if (eql 1 (getf answer :score))
                                  (let ((function (fork-version-function (getf row :form) keep)))
                                    (if function
                                        (list* :function function row)
                                        (list* :out "it passed the check, and does not compile clean in this image" row)))
                                  (list* :out (fork-answer-words answer) row))))
         (passing (loop for row in tried unless (getf row :out) collect (getf row :label)))
         (entrants (append (and standing-passed (list (list :label "what stands" :standing t)))
                           (loop for row in tried
                                 unless (getf row :out)
                                   collect (list :label (getf row :label) :function (getf row :function)))))
         (records (and passing (or standing-passed (rest passing))
                       (fork-race-live keep entrants)))
         (winner (and passing (fork-choose standing-passed passing records)))
         (form (and winner (getf (find winner tried :key (lambda (row) (getf row :label)) :test #'equal)
                                 :form))))
    (when form (fork-swap keep form))
    (list :keep keep
          :standing (let ((record (find-if (lambda (record) (getf record :standing)) records)))
                      (list :passed standing-passed
                            :detail (and answers (not standing-passed) (fork-answer-words (first answers)))
                            :calls (and record (getf record :calls))
                            :median-us (and record (getf record :median-us))))
          :versions (loop for row in (append (remove-if-not (lambda (row) (getf row :out)) shaped) tried)
                          for record = (find (getf row :label) records
                                             :key (lambda (record) (getf record :label)) :test #'equal)
                          collect (list :label (getf row :label) :source (getf row :source)
                                        :passed (and (member (getf row :label) passing :test #'equal) t)
                                        :out (getf row :out)
                                        :calls (and record (getf record :calls))
                                        :median-us (and record (getf record :median-us))
                                        :broken (and record (getf record :broken))))
          :winner (and form winner)
          :says (if fork-versions
                    (fork-race-says keep standing-passed passing records (and form winner))
                    (format nil "no version of ~(~s~) was written to race, so it stays as it was" keep)))))

(defun fork-newest-generation ()
  "The newest generation this home sealed, or a refusal when it sealed none."
  (let ((numbers (mapcar #'manifest-number (manifests))))
    (if numbers
        (reduce #'max numbers)
        (fork-fail "this home has sealed no generation to try a version in"))))

;;; --- the verb --------------------------------------------------------------------

(defun fork (&rest arguments)
  "Fork the calling session at the turn running it, once per task, and wait
for every fork's turn to end -- taken back to an earlier turn (:FROM -N, or
a turn's id), after a way back through the generations (:FROM G, or an (OLD
NEW) pair, with :CHECK), racing the versions of a name the forks write
(:KEEP NAME, with :CHECK), or both. => one plist a task, in order: (:TASK
:SESSION :STATUS :ANSWER :RAN); with a way back or a race, (:BISECT REPORT
:FORKS RESULTS :RACE REPORT), each there when the call made it."
  (multiple-value-bind (tasks seconds from check keep) (fork-parse arguments)
    (multiple-value-bind (session turn) (fork-caller)
      (let* ((anchor (if (fork-turn-from-p from) (fork-earlier-turn session turn from) turn))
             (fork-bisect (and from (not (fork-turn-from-p from)) (fork-way-back from check session)))
             (rivals (and keep (loop for n from 1 to (length tasks) collect (fork-rival-name n keep))))
             (words (loop for task in tasks
                          for n from 1
                          collect (format nil "~?~@[~a~]~@[~a~]"
                                          (if (equal anchor turn) *fork-task-input* *fork-earlier-input*)
                                          (list n (length tasks) task (rest tasks))
                                          (and fork-bisect (format nil *fork-bisect-input* (getf fork-bisect :says)))
                                          (and keep (fork-keep-words keep (nth (1- n) rivals) check))))))
        (if (not (or fork-bisect keep))
            (values (fork-tasks session anchor tasks words seconds nil))
            (unwind-protect
                 (multiple-value-bind (results forks)
                     (and tasks (fork-tasks session anchor tasks words seconds rivals))
                   (append (and fork-bisect (list :bisect fork-bisect))
                           (and tasks (list :forks results))
                           (and keep (list :race (fork-race keep (fork-versions keep forks fork-bisect) check session
                                                       (fork-newest-generation))))))
              ;; the versions raced were the call's own scratch
              (dolist (rival rivals)
                (when (fboundp rival) (fmakunbound rival)))))))))

;;; --- the budget ------------------------------------------------------------------

(defun fork-budget (next turn)
  "Advice on NLE:TURN-BUDGET, which a session's owner advises for its own
sessions: a fork's turn runs under the seconds its call gave; every other turn
is NEXT's. FORK-TASKS puts it on before a fork's turn can open."
  (let ((waiting (gethash (getf turn :session-id) *fork-waiting*)))
    (if waiting
        (list :seconds (getf waiting :seconds))
        (funcall next turn))))
