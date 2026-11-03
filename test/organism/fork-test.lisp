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
  (is (search "(fork \"task\" ... &key seconds from)" (nle:help :fork)) "(help :fork) answers the manual")
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

