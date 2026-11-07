;;;; support.lisp --- cron test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Cron tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CRON-CELL-
;;;; name prefix; RUN-CRON-TESTS runs exactly that slice.
;;;;
;;;; The two seams that reach the organism's ingress are
;;;; NODECODE-CRON::SUBMIT-PROMPT (a fire into the job's session, a report
;;;; into its origin) and NODECODE-CRON::REQUEST-CANCEL; WITH-CAPTURED-SUBMITS
;;;; stubs both (the notify LAUNCH pattern) so no test ever starts a turn,
;;;; and answers the disposition the test names; *CRON-REFUSE-SESSION* makes
;;;; the stub refuse one session the way a broken ingress would. The
;;;; ticker thread is never started here: a test installs the cell without
;;;; it (NODECODE-CRON::INSTALL) and runs NODECODE-CRON::TICK by hand with the
;;;; instant it means, so nothing races the assertions.

(in-package #:nodecode.test)

(define-test-slice "cron" "CRON-CELL-" :start nodecode-cron:start-cell)

(defmacro with-cron-runtime ((&key (timeout-minutes 10) store submits job) &body body)
  "Run BODY with the cell started (settings, hooks, /cron, an empty
registry) and no ticker thread; stopped on unwind."
  ;; STORE runs it all over a temp store; SUBMITS — T, or
  ;; WITH-CAPTURED-SUBMITS's keys — captures the two ingress seams around
  ;; BODY; JOB — (VAR CRON-ADD's arguments) — binds VAR to the job BODY runs
  ;; over. NODECODE-CRON::*TICKER* is the one seam the thread hangs off, so
  ;; every test drives TICK by hand.
  (let* ((body (if job `((let ((,(first job) (cron-add ,@(rest job)))) ,@body)) body))
         (form `(let ((nodecode-cron::*ticker* nil))
                  (with-cell-stop ((cron-start "timeout_minutes" ,timeout-minutes))
                    ,@(if submits
                          `((with-captured-submits ,(if (listp submits) submits '()) ,@body))
                          body)))))
    (if store `(with-temp-store () ,form) form)))

(defun cron-publish (type session &key payload)
  "One durable TYPE fact of SESSION's turn through the installed observer."
  (cell-publish "nodecode-cron" (cell-frame-op type :session session :payload payload)))

(defun cron-finish (session &optional answer)
  "The turn's ANSWER (when given), then its completion, both through the
installed observer."
  (when answer
    (cron-publish "turn.assistant_message_completed" session
                  :payload (nlk:json-object "message" (nlk:json-object "role" "assistant"
                                                                       "content" answer))))
  (cron-publish "turn.completed" session))

(defvar *cron-submits* '()
  "(:session :prompt :command-id :job-id) plists the stub received, newest first.")

(defvar *cron-refuse-session*
  nil
  "A session id the submit stub refuses with an error, or NIL.")

(defun cron-submits () (reverse *cron-submits*))

(defun cron-submits-to (session)
  "The captured submits into SESSION, oldest first."
  (cl:remove session (cron-submits) :key (lambda (entry) (getf entry :session))
                                    :test-not #'string=))

(defmacro with-captured-submits ((&key (disposition :started) (turn-id "t1")) &body body)
  "Run BODY with the ingress seam recording instead of reaching the
organism; the submit stub answers (values DISPOSITION TURN-ID), or signals
for the session *CRON-REFUSE-SESSION* names."
  `(nlk:with-cleanup ((setf *cron-submits* '()
                            *cron-refuse-session* nil))
     (setf *cron-submits* '()
           *cron-refuse-session* nil)
     (with-stubbed-fdefinitions
         ((nodecode-cron::submit-prompt (session prompt command-id job-id)
            (when (equal session *cron-refuse-session*)
              (error "ingress refused ~a" session))
            (push (list :session session :prompt prompt
                        :command-id command-id :job-id job-id)
                  *cron-submits*)
            (values ,disposition ,turn-id)))
       ,@body)))

(defun cron-local (year month day hour minute)
  "A local wall-clock instant as universal time."
  (encode-universal-time 0 minute hour day month year))

(defun cron-board-text (id)
  "The standing notice under cron:ID, or NIL."
  (second (cell-notice (format nil "cron:~a" id))))
