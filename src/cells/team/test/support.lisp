;;;; support.lisp --- team test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Team tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a TEAM-CELL-
;;;; name prefix; RUN-TEAM-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed. A node is a real session whose turn
;;;; a real worker runs through NLE:SUBMIT; the one stub is the provider
;;;; (WITH-PROVIDER-STUB), which either answers at once -- a node that goes
;;;; idle -- or parks until its turn is cancelled -- a node still working.
;;;; The scorer is a real script and ./verify really runs: a candidate is a
;;;; file holding one integer, its score, and ten or more meets the task.

(in-package #:nodecode.test)

(define-test-slice "team" "TEAM-CELL-" :start nodecode-team:start-cell)

(defparameter +team-scorer+
  "#!/bin/sh
n=$(cat \"$1\" 2>/dev/null) || exit 1
echo \"$n\"
[ \"$n\" -ge 10 ]
"
  "The toy scorer: the candidate file's integer is its score, 10 meets the task.")

(defmacro with-team-runtime ((root scorer &key (nodes 2) (tokens 100000) (seconds 600)
                                              session answer open)
                             &body body)
  "Run BODY over a temp store with the cell started under NODES, TOKENS
and SECONDS; ROOT is a scratch directory and SCORER the toy scorer in it.
SESSION is ensured a durable session first; ANSWER has every turn answer at
once; OPEN, (DIR &key WORKING), is BODY's WITH-OPEN-TEAM. Stopped, and the
tree removed, on unwind."
  `(with-temp-store ()
     (with-temp-workspace (,root)
       (let ((,scorer (write-temp-file (merge-pathnames "scorer.sh" ,root) +team-scorer+)))
         (declare (ignorable ,scorer))
         (with-cell-stop ((team-start "nodes" ,nodes "budget_tokens" ,tokens
                                       "budget_seconds" ,seconds))
           ,@(when session `((ensure-durable-session ,session)))
           ,(cond (answer
                   `(with-provider-stub (nil (wire-message "assistant" :content "ok")) ,@body))
                  (open `(with-open-team (,(first open) ,root ,scorer ,@(rest open)) ,@body))
                  (t `(progn ,@body))))))))

(defmacro with-open-team ((dir root scorer &key working) &body body)
  "BODY over a scored team opened in ROOT's t/ as DIR: its nodes answering at
once and gone idle, or WORKING, parked until cancelled and stopped on unwind."
  `(let ((,dir (team-dir ,root "t")))
     (with-provider-stub (nil ,(if working
                                   '(park-until-cancelled)
                                   '(wire-message "assistant" :content "ok")))
       (nlk:with-cleanup (,@(when working
                              `((ignore-errors (team:stop ,dir)) (await () (team-idle-p ,dir)))))
         (team:open "task" :score ,scorer :dir ,dir)
         ,@(unless working `((is (await () (team-idle-p ,dir)))))
         ,@body))))

(defmacro with-budget-log ((budgets) &body body &aux (lock (gensym "LOCK")))
  "BODY with every turn answering at once and BUDGETS holding what each turn
opened under, (SESSION-ID . BUDGET) as the provider saw it, newest first."
  `(let ((,budgets '())
         (,lock (bt2:make-lock)))
     (with-provider-stub (nil (let ((turn (nle:turn)))
                                (bt2:with-lock-held (,lock)
                                  (push (cons (getf turn :session-id) (getf turn :budget))
                                        ,budgets)))
                              (wire-message "assistant" :content "ok"))
       ,@body)))

(defun team-dir (root name)
  "The directory a test opens its team in, not made yet."
  (merge-pathnames (format nil "~a/" name) root))

(defun team-nodes (dir)
  (getf (nodecode-team::read-config dir) :nodes))

(defun team-idle-p (dir)
  "Whether no node of the team at DIR has a turn running."
  (notany #'nlk:active-turn-p (team-nodes dir)))

(defun team-inputs (id)
  "Every input node ID was ever given, oldest first."
  (mapcar (lambda (payload)
            (or (nlk:json-value payload :string "message")
                (nlk:json-value payload :string "message" "content")))
          (nlk:events :session-id id :kind "turn.input_committed" :as :payloads)))

(defun operator-submit (session prompt &key (source "gateway"))
  "PROMPT into SESSION the way an input of SOURCE arrives => the admission:
\"gateway\", the operator's shell, by default; NIL carries no invocation."
  (let ((command-id (nlk:make-durable-id "command")))
    (nle:submit session prompt
                :command-id command-id
                :invocation (and source
                                 (nlk:make-invocation
                                  :action "turn.start"
                                  :command-id command-id
                                  :correlation-id command-id
                                  :source (nlk:make-invocation-source source)
                                  :transport (nlk:make-invocation-transport "websocket"))))))

(defun team-slash (session text)
  "/team TEXT in SESSION's shell => its answer."
  (nodecode-team::run-slash text session))

(defun seated-count (session)
  "How many node sessions named under SESSION exist."
  (loop for k below 8
        count (nlk:session-exists-p (nodecode-team::node-id session k))))

(defun steered-inputs (id)
  "The steers parked on node ID's running turn: each one's content."
  (loop for prompt across (nlk::queue-prompts id)
        when (nlk::steer-prompt-p prompt)
          collect (gethash "content" prompt)))

(defun settled-p (session)
  "Whether SESSION and every node of its standing team are idle."
  (and (not (nlk:active-turn-p session))
       (team-idle-p (nodecode-team::standing-dir session))))

(defun team-sh (dir command)
  "COMMAND through sh in DIR => (values OUTPUT-TRIMMED EXIT-CODE)."
  (nlk:bind (((output _ status)
              (uiop:run-program (list "sh" "-c" command) :directory dir :output :string
                                :error-output :string :ignore-error-status t)))
    (values (string-trim '(#\Space #\Newline) output) status)))

(defun team-candidate (dir name score)
  "A candidate file NAME under DIR that scores SCORE."
  (write-temp-file (merge-pathnames name dir) (format nil "~d~%" score))
  name)

(defun team-lines (dir name)
  (uiop:read-file-lines (merge-pathnames name dir)))

(defun seed-node-usage (id &key (input 0) (output 0) (reasoning 0) cached)
  "One completed turn on node ID whose one round reported these counts."
  (let ((turn (nlk:admit-turn id (nlk::make-durable-id "command") "again")))
    (nlk:record-turn-usage turn :input-tokens input :output-tokens output
                                :reasoning-tokens reasoning :cached-input-tokens cached)
    (nlk:complete-turn turn nil)))
