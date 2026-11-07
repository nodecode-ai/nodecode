;;;; support.lisp --- board test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Board tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a BOARD-CELL-
;;;; name prefix; RUN-BOARD-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. Sessions are real, posts are facts in a
;;;; real store, a turn runs through NLE:RUN or, woken, through NLE:SUBMIT on
;;;; a real worker; the one stub is the provider (WITH-PROVIDER-STUB), which
;;;; records what each request carried.

(in-package #:nodecode.test)

(define-test-slice "board" "BOARD-CELL-" :start nodecode-board:start-cell)

(defmacro with-board ((&rest sessions) &body body)
  "BODY over a temp store holding SESSIONS, the cell started on its defaults
-- or on the pairs a leading (:config PAIR...) form gives -- and stopped on
unwind."
  (let ((config (and (consp (first body)) (eq :config (first (first body)))
                     (rest (pop body)))))
    `(with-temp-store ()
       ,@(mapcar (lambda (session) `(ensure-durable-session ,session)) sessions)
       (with-cell-stop ((board-start ,@config))
         ,@body))))

(defun board-as (session thunk)
  "THUNK's values, called as SESSION's evaluation would call it."
  (let ((nlk:*scribe-session-id* session))
    (funcall thunk)))

(defmacro on-board (session &body body)
  "BODY as SESSION's evaluation: (on-board \"a\" (board:post \"hi\"))."
  `(board-as ,session (lambda () ,@body)))

(defun board-texts (context)
  "The board reminders CONTEXT, one compiled request, carries, oldest first."
  (loop for message in (coerce (nle::compiled-turn-context-messages context) 'list)
        for text = (nle::content-text (gethash "content" message))
        when (and (stringp text) (search "[board " text))
          collect text))

(defun board-turn (session source id)
  "A turn admitted on SESSION as SOURCE (ID its source id) would admit it."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command "go"
                    :invocation (nlk:default-invocation "turn.start" command :source source :id id))))

(defun board-turns (session)
  "How many turns SESSION was ever admitted."
  (nlk:events :session-id session :kind "turn.started" :as :count))

(defun board-told (session)
  "Every reminder text SESSION's turns were told, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))
