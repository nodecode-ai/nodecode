;;;; support.lisp --- goal test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Goal tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a GOAL-CELL-
;;;; name prefix; RUN-GOAL-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. Sessions are real, a goal is facts in a
;;;; real store, its turns are admitted through NLE:SUBMIT and run on real
;;;; workers, its check runs a real shell, its judge is a real fork; the one
;;;; stub is the provider (WITH-PROVIDER-STUB).

(in-package #:nodecode.test)

(define-test-slice "goal" "GOAL-CELL-" :start nodecode-goal:start-cell)

(defmacro with-goal ((&rest sessions) &body body)
  "BODY over a temp store holding SESSIONS, the cell started on its defaults
-- or on the pairs a leading (:config PAIR...) form gives -- and stopped on
unwind."
  (let ((config (and (consp (first body)) (eq :config (first (first body)))
                     (rest (pop body)))))
    `(with-temp-store ()
       ,@(mapcar (lambda (session) `(ensure-durable-session ,session)) sessions)
       (with-cell-stop ((goal-start ,@config))
         ,@body))))

(defmacro on-goal (session &body body)
  "BODY as SESSION's evaluation: (on-goal \"a\" (goal:status))."
  `(let ((nlk:*scribe-session-id* ,session))
     ,@body))

(defun goal-marker (session name)
  "The file NAME in SESSION's folder, removed if a run before left it."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname (nlk:find-session-cwd session)))))
    (when (probe-file path) (delete-file path))
    path))

(defun goal-facts-of (session kind)
  "SESSION's goal facts of KIND, payloads oldest first."
  (nlk:events :session-id session :kind kind :as :payloads))

(defun goal-ending (session)
  "The reason SESSION's newest goal ended, or NIL while it runs."
  (let ((end (car (last (goal-facts-of session "goal.ended")))))
    (and end (nlk:json-value end :string "reason"))))

(defun goal-await-end (session &optional (seconds 30))
  "Poke the worker until SESSION's goal ends and the session idles. => the reason."
  (await (:timeout seconds)
    (nodecode-goal::poke)
    (and (not (nlk:active-turn-p session)) (goal-ending session))))

(defun goal-inputs (session)
  "The inputs SESSION's turns were admitted with, oldest first."
  (loop for payload in (goal-facts-of session "turn.input_committed")
        collect (nlk:fact-message-content payload)))

(defun goal-answers (session)
  "SESSION's assistant answers, oldest first."
  (loop for payload in (goal-facts-of session "turn.assistant_message_completed")
        collect (nlk:fact-message-content payload)))

(defun goal-turns (session)
  "How many turns SESSION was ever admitted, a recorded note included."
  (nlk:events :session-id session :kind "turn.started" :as :count))

(defun goal-turn (session)
  "A turn admitted on SESSION by hand, as the operator's would be: it runs
until the test ends it."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command "go"
                    :invocation (nlk:default-invocation "turn.start" command :source "gateway"))))

(defun goal-request-text (context needle)
  "The first message text CONTEXT, one compiled request, carries that holds
NEEDLE, or NIL."
  (loop for message across (coerce (nle::compiled-turn-context-messages context) 'vector)
        for text = (nle::content-text (gethash "content" message))
          thereis (and (stringp text) (search needle text) text)))

(defun goal-judge-question (context)
  "The judge's question CONTEXT, one compiled request, carries, or NIL when
it is not a judge's."
  (goal-request-text context "[goal check]"))

(defun goal-repo (session)
  "SESSION, made to work in a fresh git repository: README committed and then
edited, draft.txt never added. => its folder."
  (let ((dir (nlk:home (format nil "repo-~a/" session))))
    (ensure-directories-exist dir)
    (flet ((put (name text)
             (with-open-file (out (merge-pathnames name dir) :direction :output :if-exists :supersede)
               (write-string text out))))
      (put "README" (format nil "start~%"))
      (multiple-value-bind (out err status)
          (nlk:run-bounded (list "sh" "-c" "git init -q && git add -A && git -c user.name=t -c user.email=t@t.invalid commit -qm start")
                           :directory (namestring dir))
        (declare (ignore out))
        (assert (eql status 0) () "git init: ~a" err))
      (put "README" (format nil "start~%edited~%"))
      (put "draft.txt" (format nil "draft~%")))
    (nlk:create-session :id session :cwd (namestring dir))
    dir))

(defun goal-file (dir name)
  "The text of the file NAME in DIR, or NIL."
  (let ((path (merge-pathnames name dir)))
    (and (probe-file path) (uiop:read-file-string path))))

(defun goal-git (dir &rest args)
  "What git ARGS print in DIR, trimmed."
  (nlk:trimmed (or (nlk:run-bounded (list* "git" args) :directory (namestring dir)) "")))

(defun goal-judging-request-p (context)
  "Whether CONTEXT, one compiled request, is a judge's: its messages carry
the judge's question."
  (and (goal-judge-question context) t))
