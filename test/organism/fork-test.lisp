;;;; fork-test.lisp --- the kernel's fork: forks that hold the context, the
;;;; order, the refusals, the clock and the stop; the fence a fork's forms
;;;; meet; the way back through the generations; the race.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the verb over real sessions, a real store and real
;;;; turn workers: that each task runs in an anchored fork of the calling
;;;; session at the calling turn, whose request carries the parent's exchange
;;;; and the parent's open eval call answered by the fold; that the answers
;;;; come back one a task, in the order asked; that a fork cannot fork and
;;;; each other refusal says why; that a fork past its time is stopped and the
;;;; call does not wait it out; that a call that stops waiting stops its forks;
;;;; that what a fork ran reads back as the notes of its evals; that the
;;;; escrow, the scribe and the unkept report hold a fork to names of its own;
;;;; and that the way back and the race answer as they say. Nothing is
;;;; stubbed but the provider (WITH-PROVIDER-STUB) and, for the way back and
;;;; the race, the fresh processes a check is asked in (*FORK-ASK*).

(in-package #:nodecode.test)

(defun fork-calling-turn (session)
  "A turn admitted on SESSION, as the operator's would be, whose round called
eval and has no result yet: the turn a call of nlk:fork runs in."
  (let* ((command (nlk:make-durable-id "command"))
         (turn (nlk:admit-turn session command "split this up"
                               :invocation (nlk:default-invocation "turn.start" command :source "gateway")))
         (arguments "{\"form\":\"(fork \\\"the task\\\")\"}"))
    (nlk:record-assistant-message
     turn (nlk:json-object "role" "assistant" "content" :null
                           "tool_calls" (vector (nlk:json-object
                                                 "id" "call-fork" "type" "function"
                                                 "function" (nlk:json-object "name" "eval"
                                                                             "arguments" arguments)))))
    (nlk:record-tool-call turn "call-fork" "eval" arguments)
    turn))

(defmacro with-fork-call ((session turn) &body body)
  "BODY over a temp store holding SESSION,
as SESSION's eval inside TURN: an earlier exchange answered, then TURN, whose
eval call is still open, as one calling nlk:fork is."
  `(with-temp-store ()
     (seed-completed-turn ,session "the parent's own question" "the parent's own answer")
     (let* ((,turn (fork-calling-turn ,session))
            (nlk:*scribe-session-id* ,session)
            (nle::*current-durable-turn* ,turn))
       ,@body)))

(defun fork-request-text (context needle)
  "The first message text CONTEXT, one compiled request, carries that holds
NEEDLE, or NIL."
  (loop for message across (coerce (nle::compiled-turn-context-messages context) 'vector)
        for text = (nle::content-text (gethash "content" message))
          thereis (and (stringp text) (search needle text) text)))

(defun fork-field (results key)
  "KEY of each of nlk:fork's RESULTS, in order."
  (mapcar (lambda (result) (getf result key)) results))


;;; --- the kernel's verb ----------------------------------------------------------------

(deftest fork-test-is-a-kernel-verb-and-its-manual-says-so ()
  (is (eq 'nlk:fork (find-symbol "FORK" '#:nodecode.evolved)) "an eval reads (fork ...) as the kernel's verb")
  (is (search "(fork \"task\" ... &key seconds from check keep)" (nle:help :fork)) "(help :fork) answers the manual")
  (is (null (search "fork:run" (nle:help :fork))) "and names no cell's verb")
  (is (= 900 (nlk::fork-setting :seconds)) "a fork's turn runs fifteen minutes at most by default")
  (setf (gethash "fork-turn-b-1" nlk::*fork-waiting*) (list :seconds 42 :acts '()))
  (unwind-protect
       (flet ((next (turn) (declare (ignore turn)) :next))
         (is (equal '(:seconds 42) (nlk::fork-budget #'next (list :session-id "fork-turn-b-1"))) "a fork's turn runs under its call's seconds")
         (is (eq :next (nlk::fork-budget #'next (list :session-id "p"))) "every other turn under what it ran under before"))
    (remhash "fork-turn-b-1" nlk::*fork-waiting*)))

;;; --- the forks ---------------------------------------------------------------------

(deftest fork-test-each-task-runs-in-a-fork-that-holds-the-context ()
  (let ((seen (make-hash-table :test #'equal :synchronized t)))
    (with-fork-call ("p" turn)
      (with-provider-stub (context
                           (let* ((task (fork-request-text context "[fork "))
                                  (which (if (and task (search "alpha" task)) "alpha" "beta")))
                             (setf (gethash which seen)
                                   (list (and (fork-request-text context "the parent's own question") t)
                                         (and (fork-request-text context "no result recorded (interrupted)") t)
                                         (and task (search (format nil "[fork ~:[2~;1~] of 2]" (equal which "alpha")) task) t)))
                             (wire-message "assistant" :content (format nil "~a done" which))))
        (let ((results (nlk:fork "the alpha task" "the beta task")))
          (is (equal '("the alpha task" "the beta task") (fork-field results :task)) "one answer a task, in the order asked")
          (is (equal '(:completed :completed) (fork-field results :status)))
          (is (string= "fork 1: the alpha task" (nlk::session-title (first (fork-field results :session)))) "a fork is titled as it is made, so no model names it")
          (is (assoc "fork" (cdr (assoc 'nle:turn-budget nlk::*hooks*)) :test #'equal) "and its turns run under the call's seconds")
          (is (equal '("alpha done" "beta done") (fork-field results :answer)) "each answer is its own fork's last words")
          (is (equal '(nil nil) (fork-field results :ran)) "neither fork ran a tool")
          (dolist (session (fork-field results :session))
            (is (equal (list "p" (nlk:durable-turn-turn-id turn)) (subseq (nlk::session-fork-edge session) 0 2)) "each is a fork of the calling session at the calling turn"))
          (is (equal '(t t t) (gethash "alpha" seen)) "the alpha fork's request carried the parent's exchange, the open eval call answered, and its own task")
          (is (equal '(t t t) (gethash "beta" seen)) "and so did the beta fork's")
          (is (zerop (hash-table-count nlk::*fork-waiting*)) "no fork is left waited on"))))))

(deftest fork-test-refuses-what-it-cannot-fork ()
  (with-fork-call ("q" turn)
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" "b" "c" "d" "e"))))
      ("at most 4 forks"))
    (is (signals-error nlk:fork-error (nlk:fork)) "no task")
    (is (signals-error nlk:fork-error (nlk:fork "  ")) "a blank task")
    (is (signals-error nlk:fork-error (nlk:fork "a" :timeout 5)) "an option it does not take")
    (let ((nlk:*scribe-session-id* "fork-turn-x-1"))
      (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a"))))
        ("a fork cannot fork")))
    (let ((nle::*current-durable-turn* nil))
      (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a"))))
        ("inside a turn")))
    (is (not (nlk:session-exists-p (format nil "fork-~a-1" (nlk:durable-turn-turn-id turn)))) "no refusal made a fork")))

(deftest fork-test-a-fork-past-its-time-is-stopped ()
  (with-fork-call ("r" turn)
    (let ((nlk::*fork-grace-seconds* 0))
      (with-provider-stub (nil (sleep 3) (wire-message "assistant" :content "late"))
        (let* ((began (get-internal-real-time))
               (result (first (nlk:fork "a slow task" :seconds 1)))
               (took (/ (- (get-internal-real-time) began) internal-time-units-per-second)))
          (is (eq :timeout (getf result :status)) "the fork ran past its second and was asked to stop")
          (is (< took 12) "the call did not wait out the slow fork")
          (is (not (nlk:active-turn-p (getf result :session))) "the fork's turn is over")
          (is (zerop (hash-table-count nlk::*fork-waiting*)) "no fork is left waited on"))))))

(deftest fork-test-a-call-that-stops-waiting-stops-its-forks ()
  (with-fork-call ("s" turn)
    (with-provider-stub (nil (sleep 3) (wire-message "assistant" :content "never read"))
      (let* ((session nlk:*scribe-session-id*)
             (current nle::*current-durable-turn*)
             (child (format nil "fork-~a-1" (nlk:durable-turn-turn-id current)))
             (caller (bt2:make-thread (lambda ()
                                        (let ((nlk:*scribe-session-id* session)
                                              (nle::*current-durable-turn* current))
                                          (catch 'stop (nlk:fork "a task the caller gives up on"))))
                                      :name "fork-caller")))
        (is (await (:timeout 10) (nlk:active-turn-p child)) "the fork's turn runs")
        (nlk:interrupt-thread-safely caller (lambda () (throw 'stop :stopped)))
        (is (eq :stopped (bt2:join-thread caller)) "the call stopped waiting")
        (is (await (:timeout 10) (nlk:events :session-id child :kind "turn.cancelled" :as :exists)) "and its fork's turn was cancelled")
        (is (zerop (hash-table-count nlk::*fork-waiting*)) "no fork is left waited on")))))

(defun fork-test-active-turn (session)
  "The id of SESSION's running turn, or NIL."
  (first (nlk::active-turn-row session)))

(defun fork-test-steer-thread (session current &rest tasks)
  "A thread calling nlk:fork with TASKS as SESSION's eval inside CURRENT, its
results its value."
  (bt2:make-thread (lambda ()
                     (let ((nlk:*scribe-session-id* session)
                           (nle::*current-durable-turn* current))
                       (apply #'nlk:fork tasks)))
                   :name "fork-caller"))

(defmacro with-working-forks ((&key (answer "steered: BANANA")) &body body)
  "BODY with each fork's turn working round after round -- one eval a round --
until a request carries BANANA, which it answers with ANSWER."
  (let ((calls (gensym "CALLS")))
    `(let ((,calls 0))
       (with-provider-stub (context
                            (if (fork-request-text context "BANANA")
                                (wire-message "assistant" :content ,answer)
                                (progn (sleep 0.2)
                                       (wire-message "assistant"
                                                     :tool-calls (vector (wire-tool-call (format nil "work-~d" (incf ,calls))
                                                                                         "eval" "{\"form\":\";; work a little\\n(+ 1 2)\"}"))))))
         ,@body))))

(deftest fork-test-a-steered-fork-answers-through-its-steer ()
  ;; A steer ends the fork's running turn at its next round boundary and runs
  ;; as the next turn: the call waits on that one, and its answer is the fork's.
  (with-fork-call ("sv" turn)
    (with-working-forks ()
      (let* ((child (format nil "fork-~a-1" (nlk:durable-turn-turn-id turn)))
             (caller (fork-test-steer-thread nlk:*scribe-session-id* nle::*current-durable-turn* "a task the person steers")))
        (is (await (:timeout 10) (nlk:active-turn-p child)) "the fork's turn runs")
        (let ((first (fork-test-active-turn child)))
          ;; the person's steer parks on the fork's running turn
          (is (eq :steered (nlk:active-input-admission-disposition
                            (nle:submit child "BANANA: say it now" :steer first :command-id "steer-1"))))
          (let ((result (first (bt2:join-thread caller))))
            (is (eq :completed (getf result :status)) "the fork completed")
            (is (equal "steered: BANANA" (getf result :answer)) "its answer is the steered turn's, not the cut turn's silence")
            (is (equal '("BANANA: say it now") (getf result :steered)) (format nil "and the call hears what the person steered it with: ~s" (getf result :steered)))
            ;; what it ran before the steer still counts
            (is (equal '(";; work a little") (remove-duplicates (getf result :ran) :test #'equal)))
            (is (not (equal first (fork-test-active-turn child))) "the turn it ended on is the steer's")
            (is (not (nlk:active-turn-p child)) "and nothing runs on in the fork")
            (is (zerop (hash-table-count nlk::*fork-waiting*)) "no fork is left waited on")))))))

(deftest fork-test-a-fork-the-person-stops-says-so ()
  (with-fork-call ("so" turn)
    (with-working-forks ()
      (let* ((child (format nil "fork-~a-1" (nlk:durable-turn-turn-id turn)))
             (caller (fork-test-steer-thread nlk:*scribe-session-id* nle::*current-durable-turn* "a task the person stops" "a task left alone")))
        (is (await (:timeout 10) (nlk:active-turn-p child)) "the fork's turn runs")
        (is (nlk:request-cancel-turn child (fork-test-active-turn child) :operator) "the person stops it")
        (await (:timeout 10) (not (nlk:active-turn-p child)))
        (nle:submit (format nil "fork-~a-2" (nlk:durable-turn-turn-id turn)) "BANANA, you can stop"
                    :steer (fork-test-active-turn (format nil "fork-~a-2" (nlk:durable-turn-turn-id turn))) :command-id "steer-2")
        (destructuring-bind (stopped left) (bt2:join-thread caller)
          (is (eq :cancelled (getf stopped :status)) "the stopped fork reads cancelled")
          (is-carrying (text (getf stopped :answer)) "stopped by the operator")
          (is (eq :completed (getf left :status)) "its sibling ran on to its end"))))))

(deftest fork-test-what-a-fork-ran-reads-as-its-notes ()
  (with-fork-call ("t" turn)
    (let* ((worked (seed-completed-turn "worker" "work" "done"
                                        :tools '(("c-1" "eval" "{\"form\":\";; read the hook header\\n(sh \\\"ls\\\")\"}" "ok")
                                                 ("c-2" "report_issue" "{\"tool\":\"eval\"}" "noted"))))
           (ran (nlk::fork-ran "worker" (nlk:durable-turn-turn-id worked))))
      (is (equal '(";; read the hook header" "report_issue") ran) "an eval reads as its note, any other tool as its name"))))

;;; --- what a fork may define ----------------------------------------------------------

(deftest fork-test-a-fork-defines-only-names-of-its-own ()
  (let ((fork "fork-turn-u-1")
        (sibling "fork-turn-u-2"))
    (flet ((escrow (session form)
             ;; the eval seam's own gate, as SESSION's eval runs it
             (let ((nlk:*scribe-session-id* session))
               (nlk:definition-escrow form)
               :passed)))
      (unwind-protect
           (progn
             (is (eq :passed (escrow fork '(defun fork-test-a-fresh-name (x) x))) "a fork defines a name nothing answers to")
             (is (equal fork (gethash 'fork-test-a-fresh-name nlk::*fork-owners*)) "and holds it")
             (is (eq :passed (escrow fork '(defun fork-test-a-fresh-name (x) (1+ x)))) "and defines it again")
             (is-carrying (text (princ-to-string (signals-error nlk:fork-error
                                                   (escrow sibling '(defun fork-test-a-fresh-name (x) x)))))
               ("another fork's"))
             (is-carrying (text (princ-to-string (signals-error nlk:fork-error
                                                   (escrow fork '(progn (defun fork-test-another-name () 1)
                                                                        (defparameter *print-base* 10))))))
               "already answers" "not evaluated")
             (is (null (gethash 'fork-test-another-name nlk::*fork-owners*)) "a refused form claims nothing")
             (is (signals-error nlk:fork-error (escrow fork '(fmakunbound 'nlk:fork))) "nor forgets what stands")
             (is (signals-error nlk:fork-error (escrow fork '(define-hook nlk:trial "k" (next &rest a) (apply next a)))) "nor advises it")
             (is (eq :passed (escrow "p" '(defun fork-test-p-name (x) x))) "any other session's form is the escrow's as before")
             (setf (gethash fork nlk::*fork-waiting*) (list :seconds 1 :acts '()))
             (let ((said (with-output-to-string (*standard-output*)
                           (is (null (nlk:record-definitions '(progn (defun fork-test-a () 1) (defun fork-test-b () 2)) "(progn)" 7 :session-id fork)) "what a fork defines is never written"))))
               (is-carrying (text said)
                 "[fork: " "fork-test-a" "fork-test-b" "are live in this image and not kept"))
             (is (equal '(fork-test-b fork-test-a)
                        (mapcar #'second (getf (gethash fork nlk::*fork-waiting*) :acts))) "and the call waiting on it has it, newest first")
             (is (null (nlk::fork-record '(defun fork-test-c () 3) "p")) "any other session's definitions are the scribe's as before")
             (is (gethash 'fork-test-b nlk::*fork-made*) "what a fork defines is noted as a fork's")
             (let* ((made (intern "FORK-TEST-MADE" '#:nodecode.evolved))
                    (other (intern "FORK-TEST-OTHER" '#:nodecode.evolved))
                    (before (cons (nlk::definition-snapshot)
                                  (sb-ext:with-locked-hash-table (nlk::*kept-definitions*)
                                    nlk::*kept-sequence*))))
               (unwind-protect
                    (flet ((told (session)
                             (let ((nlk:*scribe-session-id* session))
                               (with-output-to-string (*standard-output*)
                                 (nlk:report-unkept-changes before '(progn) nil)))))
                      ;; while an eval's window is open, a fork defines MADE and something else changes OTHER
                      (eval `(defun ,made () 1))
                      (eval `(defun ,other () 2))
                      (setf (gethash made nlk::*fork-made*) t)
                      (is (string= "" (told fork)) "a fork's eval is never told its definitions went unkept")
                      (is-carrying (text (told "p"))
                        "fork-test-other" "changed but not kept")
                      (is (null (search "fork-test-made" (told "p"))) "and no other eval is told a fork's"))
                 (fmakunbound made)
                 (fmakunbound other)
                 (unintern made '#:nodecode.evolved)
                 (unintern other '#:nodecode.evolved))))
        (clrhash nlk::*fork-made*)
        (remhash fork nlk::*fork-waiting*)
        (nlk::fork-release fork)
        (nlk::fork-release sibling))
      (is (zerop (hash-table-count nlk::*fork-owners*)) "a released fork holds no name"))))

;;; --- the way back ----------------------------------------------------------------------

(defun fork-test-asker (broke &key unanswered)
  "A stub ASK over generations: those below BROKE pass, the rest fail, those in
UNANSWERED answer nothing. => (values ASK BATCHES), BATCHES the asks it took."
  (let ((batches '()))
    (values (lambda (numbers)
              (push numbers batches)
              (mapcar (lambda (number)
                        (list :score (cond ((member number unanswered) nil)
                                           ((< number broke) 1)
                                           (t 0))))
                      numbers))
            (lambda () (reverse batches)))))

(deftest fork-test-the-way-back-halts-at-the-step-that-broke ()
  (let ((numbers (loop for n from 100 to 1100 collect n)))
    (multiple-value-bind (ask batches) (fork-test-asker 700)
      (let ((found (nlk::fork-bisect numbers ask 4)))
        (is (eq :found (getf found :verdict)))
        (is (equal '(699 700) (list (getf found :good) (getf found :bad))) "it names the last good generation and the first bad one")
        (is (equal '(100 1100) (first (funcall batches))) "both ends are asked first, together")
        (is (every (lambda (batch) (<= (length batch) 4)) (funcall batches)) "never more than four at once")
        (is (<= (getf found :rounds) 8) "a thousand generations take a handful of rounds")
        (is (equal '(699 700) (car (last (funcall batches)))) "the step found is asked again, both its ends")
        (is (= (+ 2 (length (getf found :tested))) (length (reduce #'append (funcall batches)))) "every other generation is asked once")))
    (is (eq :clean (getf (nlk::fork-bisect numbers (fork-test-asker 5000) 4) :verdict)) "the newest passing: nothing sealed explains it")
    (is (eq :program (getf (nlk::fork-bisect numbers (fork-test-asker 50) 4) :verdict)) "the oldest failing too: the program is to blame")
    (is (eq :unanswered (getf (nlk::fork-bisect numbers (fork-test-asker 700 :unanswered '(1100)) 4) :verdict)) "an end that answers nothing stops it")
    (let* ((asked (make-hash-table))
           (flaky (lambda (batch)
                    (mapcar (lambda (number)
                              (list :score (cond ((< number 700) 1)
                                                 ((and (= number 700) (gethash number asked)) 1)
                                                 (t (setf (gethash number asked) t) 0))))
                            batch)))
           (found (nlk::fork-bisect numbers flaky 4)))
      (is (eq :unsteady (getf found :verdict)) "a generation that answers otherwise the second time names no step")
      (is (equal '((699 1) (700 1)) (mapcar (lambda (row) (list (getf row :number) (getf row :score))) (getf found :again)))))
    (let ((found (nlk::fork-bisect numbers (fork-test-asker 700 :unanswered (loop for n from 600 to 699 collect n)) 4)))
      (is (eq :found (getf found :verdict)))
      (is (equal '(599 700) (list (getf found :good) (getf found :bad))) "one that cannot be asked is stepped over, never read as the break"))
    (is (equal '(1 3) (list (getf (nlk::fork-bisect '(1 3) (fork-test-asker 2) 4) :good)
                            (getf (nlk::fork-bisect '(1 3) (fork-test-asker 2) 4) :bad))) "two generations, one step")))

(deftest fork-test-refuses-a-way-back-or-a-race-it-cannot-judge ()
  (with-fork-call ("v" turn)
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork :from 3))))
      ("judged by a check"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :check '(= 1 1)))))
      (":check judges"))
    (is (signals-error nlk:fork-error (nlk:fork "a" :from 0)) "no generation 0 to start from")
    (is (signals-error nlk:fork-error (nlk:fork "a" :from '(1 2 3) :check t)) "a range is two generations")
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork :from 3 :check t))))
      ("two sealed generations"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork :keep 'car :check t))))
      ("a race needs versions"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :keep 'car :check t))))
      ("locked package"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :keep 'when :check t))))
      ("a macro"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :keep "no-such-function-anywhere" :check t))))
      ("is none"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :from -5))))
      ("past this session's first turn"))
    (is-carrying (text (princ-to-string (signals-error nlk:fork-error (nlk:fork "a" :from "turn-nowhere"))))
      ("not a turn of this session"))
    (is (not (nlk:session-exists-p (format nil "fork-~a-1" (nlk:durable-turn-turn-id turn)))) "no refusal made a fork")))

;;; --- taken back -------------------------------------------------------------------------

(deftest fork-test-a-fork-taken-back-holds-that-turn-and-nothing-after ()
  (let ((seen (make-hash-table :test #'equal :synchronized t)))
    (with-fork-call ("w" turn)
      (with-provider-stub (context
                           (setf (gethash :seen seen)
                                 (list (and (fork-request-text context "the parent's own question") t)
                                       (and (fork-request-text context "split this up") t)
                                       (and (fork-request-text context "no result recorded (interrupted)") t)
                                       (and (fork-request-text context "taken back at an earlier turn") t)))
                           (wire-message "assistant" :content "back then"))
        (let ((result (first (nlk:fork "what did we know then" :from -1))))
          (is (eq :completed (getf result :status)))
          (is (equal "back then" (getf result :answer)))
          (is (equal '(t nil nil t) (gethash :seen seen)) "its request carried the earlier exchange and its own task, and nothing of the turn that made it")
          (is (not (equal (nlk:durable-turn-turn-id turn) (second (nlk::session-fork-edge (getf result :session))))) "it is anchored at the turn before, not the calling one"))))))

;;; --- the race ----------------------------------------------------------------------------

(deftest fork-test-a-race-puts-the-faster-version-in-place-held ()
  (let* ((keep (intern "FORK-TEST-RACED" '#:nodecode.evolved))
         (rival (nlk::fork-rival-name 1 keep))
         (slow `(defun ,keep (n) "slow" (sleep 0.0001) (* n 2))) ; under one tick of the coarse clock, so read on a fine one
         (fast `(defun ,keep (n) "fast" (* n 2)))
         (wrong `(defun ,keep (n) "wrong" (* n 3)))
         (wide `(defun ,keep (n m) "wide" (* n m))))
    (unwind-protect
         (with-fork-call ("x" turn)
           (eval slow)
           (let ((standing (fdefinition keep)))
             (is (equal `(defun ,keep (n) (if (zerop n) 0 (+ 2 (,keep (1- n)))))
                        (getf (nlk::fork-version
                               (list :n 1 :session "fork-turn-x-1" :rival rival
                                     :acts (list `(defun ,rival (n) (if (zerop n) 0 (+ 2 (,rival (1- n)))))
                                                 `(defun ,rival (n) n)))
                               keep)
                              :form)) "a fork's newest version is read, its own name KEEP's throughout")
             (is (= 6 (funcall (nlk::fork-version-function `(defun ,keep (n) (if (zerop n) 0 (+ 2 (,keep (1- n))))) keep) 3)) "a version's own calls are its own")
             (let* ((asked '())
                    (nlk::*fork-ask* (lambda (jobs directory)
                                            (declare (ignore directory))
                                            (setf asked jobs)
                                            (mapcar (lambda (job) (list :score (if (search "wrong" (second job)) 0 1)))
                                                    jobs)))
                    (nlk::*fork-race-seconds* 30)
                    ;; inside an eval, as a model's calls are: the hold judges none of them
                    (calls (bt2:make-thread (lambda ()
                                              (let ((nlk:*evaluating* t))
                                                (loop repeat 3000 do (funcall keep 2) (sleep 0.0005))))
                                            :name "fork-test-live-calls"))
                    (report (nlk::fork-race keep (list (list :label "fork 1" :form slow)
                                                            (list :label "fork 2" :form fast)
                                                            (list :label "fork 3" :form wrong)
                                                            (list :label "fork 4" :form wide))
                                                 `(= 4 (,keep 2)) "x" 7)))
               (bt2:join-thread calls)
               (is (= 4 (length asked)) "what stands and the three versions that take its calls were tried")
               (is (every (lambda (job) (eql 7 (first job))) asked) "each at the generation given")
               (is (equal "fork 2" (getf report :winner)) "the faster version won")
               (is (equal '("fork 4" "fork 1" "fork 2" "fork 3") (mapcar (lambda (row) (getf row :label)) (getf report :versions))))
               (is (search "where every call of it today takes" (getf (first (getf report :versions)) :out)) "one that cannot take the calls is out")
               (is (equal "the check answered false" (getf (fourth (getf report :versions)) :out)) "one that fails the check is out")
               (is (<= 8 (getf (second (getf report :versions)) :calls)) "each version that passed was timed on live calls")
               (is (<= 8 (getf (getf report :standing) :calls)) "and so was what stands")
               (is (search "HELD" (getf report :says)))
               (is (null (cdr (assoc keep nlk::*hooks* :test #'equal))) "the race's advice is gone")
               (is (find "FORK-TEST-RACED" (nlk:held-definitions) :key (lambda (entry) (getf entry :name)) :test #'string=) "the winner is HELD")
               (is (= 4 (let ((nlk:*evaluating* t)) (funcall keep 2))) "and answers, a call inside an eval leaving it held")
               (nlk:revert keep)
               (is (eq standing (fdefinition keep)) "revert puts back what it displaced"))))
      (ignore-errors (nlk:revert keep))
      (fmakunbound keep)
      (unintern keep '#:nodecode.evolved)
      (unintern rival '#:nodecode.evolved))))
