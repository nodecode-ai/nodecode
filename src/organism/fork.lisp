;;;; fork.lisp --- a turn forks itself: its independent tasks run at once,
;;;; each in a fork of the session that holds its whole context. Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (fork "task" ... &key seconds from) forks the calling session at
;;;; the turn running it, once per task, puts each task to its fork and waits
;;;; for every fork's turn to end. A fork is an anchored child
;;;; (CREATE-SESSION :PARENT :ANCHOR-TURN-ID): its history composes the
;;;; parent's by reference through the running turn, and the create carries the
;;;; parent's model, so its first request extends the parent's cached prefix and
;;;; it pays for its own work alone. That history ends in the eval call waiting
;;;; on it, which the fold answers `no result recorded (interrupted)': the
;;;; task's words say why, and that a fork does not fork.
;;;;
;;;; Going back is the same verb, never a second one: :FROM an earlier turn
;;;; -- -N for N turns back, or its id -- anchors the forks there: they hold
;;;; what the session knew when that turn ended, and nothing after it.
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

;;; --- the forks -------------------------------------------------------------------

(defparameter +fork-most+ 4 "The forks one call may make.")

(defparameter *fork-grace-seconds* 60
  "How long past its budget a fork's turn may still run before it is cancelled.")

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

(defun fork-parse (arguments)
  "(values TASKS SECONDS FROM) of fork's ARGUMENTS: the tasks, strings, then
:SECONDS N and :FROM X; or a refusal."
  (let* ((at (or (position-if-not #'stringp arguments) (length arguments)))
         (tasks (subseq arguments 0 at))
         (options (nthcdr at arguments)))
    (unless (and (evenp (length options))
                 (loop for (key) on options by #'cddr
                       always (member key '(:seconds :from))))
      (fork-fail "fork takes the tasks, strings, then :seconds N and :from X; it was given ~s" options))
    (let ((seconds (getf options :seconds (fork-setting :seconds)))
          (from (getf options :from)))
      (cond ((> (length tasks) +fork-most+)
             (fork-fail "fork makes at most ~d forks a call; it was given ~d tasks" +fork-most+ (length tasks)))
            ((some (lambda (task) (zerop (length (trimmed task)))) tasks)
             (fork-fail "a task is words, and one was blank"))
            ((not (and (integerp seconds) (plusp seconds)))
             (fork-fail ":seconds is a whole number of seconds above zero, not ~s" seconds))
            ((and from (not (fork-turn-from-p from)))
             (fork-fail ":from is an earlier turn -- -N for N turns back, or its id; not ~s" from))
            ((null tasks)
             (fork-fail "fork needs a task: (fork \"task\" ...)"))
            (t (values tasks seconds from))))))

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

(defun fork-start (session anchor n words task seconds)
  "Fork SESSION at ANCHOR as its Nth fork and put WORDS to it. => the fork:
(:TASK :N :SESSION :TURN), or, when its turn could not start, :STATUS :FAILED
and why."
  (let ((child (fork-name anchor n)))
    (create-session :id child :cwd (find-session-cwd session)
                    :parent session :anchor-turn-id anchor)
    ;; Titled as it is made, so its turn's admission never asks a model to name it.
    (record-event child +kind-session-title+
                  (list :title (format nil "fork ~d: ~a" n (clip (first (lines (trimmed task))) 60))))
    ;; Waited on before the submit: its turn's budget is read as it opens.
    (setf (gethash child *fork-waiting*) (list :seconds seconds :acts '()))
    (handler-case
        (list :task task :n n :session child
              :turn (active-input-admission-turn-id
                     (nle:submit child words :command-id (format nil "fork:~a" child)
                                 :source "fork" :source-id session)))
      (error (condition)
        (list :task task :n n :session child :status :failed
              :answer (format nil "its turn could not start: ~a" condition))))))

(defun fork-release (session)
  "Every name SESSION claimed, unclaimed."
  (sb-ext:with-locked-hash-table (*fork-owners*)
    (let ((names (loop for name being the hash-keys of *fork-owners* using (hash-value owner)
                       when (equal owner session) collect name)))
      (dolist (name names) (remhash name *fork-owners*)))))

(defun fork-tasks (session anchor tasks words seconds)
  "Fork SESSION at ANCHOR once per TASK, put each its WORDS, and wait for
every fork's turn to end. => the results as fork answers them."
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
            do (push (fork-start session anchor n text task seconds) forks))
      (setf forks (reverse forks))
      (fork-progress-begin call session forks)
      (let ((deadline (+ (get-internal-real-time)
                         (* (+ seconds *fork-grace-seconds*) internal-time-units-per-second))))
        (loop for fork in forks
              ;; one that never started is already as answered
              collect (fork-result fork (and (not (getf fork :status))
                                             (fork-await fork deadline seconds))))))))

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

;;; --- the verb --------------------------------------------------------------------

(defun fork (&rest arguments)
  "Fork the calling session at the turn running it, once per task, and wait
for every fork's turn to end, or taken back to an earlier turn (:FROM -N, or
a turn's id). => one plist a task, in order: (:TASK :SESSION :STATUS :ANSWER
:RAN)."
  (multiple-value-bind (tasks seconds from) (fork-parse arguments)
    (multiple-value-bind (session turn) (fork-caller)
      (let ((anchor (if from (fork-earlier-turn session turn from) turn)))
        (fork-tasks session anchor tasks
                    (loop for task in tasks
                          for n from 1
                          collect (format nil (if from *fork-earlier-input* *fork-task-input*)
                                          n (length tasks) task (rest tasks)))
                    seconds)))))

;;; --- the budget ------------------------------------------------------------------

(defun fork-budget (next turn)
  "Advice on NLE:TURN-BUDGET, which a session's owner advises for its own
sessions: a fork's turn runs under the seconds its call gave; every other turn
is NEXT's. FORK-TASKS puts it on before a fork's turn can open."
  (if-let (waiting (gethash (getf turn :session-id) *fork-waiting*))
    (list :seconds (getf waiting :seconds))
    (funcall next turn)))
