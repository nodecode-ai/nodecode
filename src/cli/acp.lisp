;;;; acp.lisp --- `nodecode acp': the organism as an agent an ACP client runs.
;;;; Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Zed, JetBrains and every other Agent Client Protocol client run an agent
;;;; as a process speaking JSON-RPC 2.0 on its stdio, one message a line
;;;; (agentclientprotocol.com, protocol version 1). `nodecode acp' is that
;;;; process for this home's organism. Like `nodecode rpc' it carries no
;;;; second engine: it finds the organism serving the store, or starts one it
;;;; fronts, and every session the client opens is a gateway session that the
;;;; TUI, the web page and the channels see too.
;;;;
;;;; Each session the client opens rides a link of its own -- a gateway
;;;; WebSocket subscribed to that session alone, as the session's shell -- so
;;;; one session's replay never crosses another's. What the gateway sends
;;;; becomes session/update notifications: streamed text and thinking as
;;;; message and thought chunks, a tool call's start and result as tool_call
;;;; and tool_call_update, the title as session_info_update, another
;;;; client's prompt in the same session as a user message, and a question the
;;;; organism puts to whoever watches (the guard's ask) as a
;;;; session/request_permission. A session/prompt is a start_turn, answered
;;;; with its stop reason once the turn's end lands; session/cancel is the
;;;; turn's cancel_turn. The control routes a shell reads over HTTP answer the
;;;; rest: the session directory (session/list), a session's creation in the
;;;; client's folder (session/new), the model catalog and the session's
;;;; target (the model and effort config options), and the slash catalog
;;;; (available commands, run through the same route a channel runs them by).
;;;;
;;;; The organism may go while the client stays -- the shell that started it
;;;; quits, an update restarts it, it crashes. A link the gateway let go is
;;;; attached again to whatever serves the store next, started here when
;;;; nothing does, and resumed past the newest fact it read: a running prompt
;;;; is answered once the turn, resumed on the next boot, ends. stdin's end
;;;; lets go of an organism this process started, as `nodecode rpc' does.

(in-package #:nodecode.evolved)

(defparameter +acp-protocol-version+ 1
  "The Agent Client Protocol version this agent speaks.")

(defparameter *acp-reattach-seconds* 60
  "How long a link the gateway let go looks for the organism serving next --
one restarting after an update included -- before its prompts fail.")

(defparameter *acp-session-page* 100
  "How many sessions one session/list page carries.")

(defparameter *acp-tool-output-chars* 100000
  "The longest tool result a tool_call_update carries whole; past it, its
head, saying how much was left out.")

(defstruct (acp-agent (:copier nil))
  "One ACP client's agent: its stdout, the gateway it reaches, its sessions."
  (output *standard-output*)
  (output-lock (bt2:make-lock :name "acp-output") :read-only t)
  (lock (bt2:make-lock :name "acp-state") :read-only t)
  (port nil)
  (token nil)
  ;; Finds the port the organism serving the store publishes, starting one
  ;; when none does, by a deadline: NIL past it (STORE-ORGANISM-PORT).
  (find-port nil)
  (sessions (make-hash-table :test #'equal) :read-only t)
  ;; Our requests to the client still waiting for its answer: id -> (SESSION . QUESTION-ID).
  (asked (make-hash-table :test #'equal) :read-only t)
  (next-id 0)
  (reattaching nil)
  ;; Whether the client said initialize: every other request waits on it.
  (initialized nil)
  (done (bt2:make-semaphore :name "acp-done") :read-only t)
  (outcome nil))

(nlk:access (agent acp-agent))

(defstruct (acp-session (:copier nil))
  "One session the client opened, and the link it rides."
  (id "" :type string)
  (cwd nil)
  (ws nil)
  ;; :ATTACHING, :ATTACHED, :LOST (the gateway let go; a reattach is due) or :CLOSED.
  (link :attaching)
  (cursor nil)
  ;; How the next whole replay is read: :HISTORY, sent as updates (session/load);
  ;; :QUIET, read for its cursor alone (session/new, session/resume, a resync).
  (replay :quiet)
  ;; The id of the session/load or session/resume answered once its replay ends.
  (opening nil)
  ;; The session/prompt in flight: (:ID RPC-ID :COMMAND ID :TURN ID :CANCELLED BOOL).
  (prompt nil)
  ;; The turns this agent started, and their commands: their input is the
  ;; client's own prompt, never sent back to it.
  (turns '())
  (commands '())
  ;; What the round in flight streamed since the fact that seats it: the
  ;; message's text and its thinking, so the completed message sends only
  ;; what was not streamed.
  (said "")
  (thought "")
  ;; Whether a turn ever ran here: a session not yet begun takes its model
  ;; and effort on its first start_turn (PICK), one begun through its routes.
  (begun nil)
  (pick nil)
  ;; Question id -> the session/request_permission id it went out as.
  (questions (make-hash-table :test #'equal) :read-only t)
  ;; The calls opened and not yet closed: call id -> (TOOL-NAME . ARGUMENTS),
  ;; what the call's result is classified by, and what a turn's end fails.
  (calls (make-hash-table :test #'equal) :read-only t)
  ;; The turn in flight's token counts, summed over its rounds: the prompt's
  ;; answer carries them.
  (usage nil)
  ;; The config options and the slash commands the client was last given,
  ;; as JSON text.
  (told-config nil)
  (told-commands nil))

(nlk:access (session acp-session))

;;; --- the wire ---------------------------------------------------------------

(define-error acp-refusal (error) (code message (data :initform nil))
  (:report "~a" message)
  (:documentation "A request refused with a JSON-RPC error: CODE, MESSAGE, and
DATA, the error's data object when it has one."))

(defun acp-refuse (code control &rest arguments)
  "Refuse the request being answered: CODE and the formatted message."
  (error 'acp-refusal :code code :message (apply #'format nil control arguments)))

(defun acp-refuse-unknown (session-id control &rest arguments)
  "Refuse the request: SESSION-ID names no session to act on (-32002,
resource not found), the error's data naming it as ACP asks."
  (error 'acp-refusal :code -32002 :message (apply #'format nil control arguments)
                      :data (json-object "sessionId" session-id)))

(defun acp-write (agent object)
  "One JSON-RPC message, OBJECT, to the client: one line on stdout."
  (let ((text (encode-json-object object)))
    (handler-case (bt2:with-lock-held (agent.output-lock)
                    (write-line text agent.output)
                    (finish-output agent.output))
      (error (condition) (acp-end agent (format nil "stdout: ~a" condition))))))

(defun acp-respond (agent id result)
  "Answer request ID with RESULT (an object; NIL is the empty one)."
  (acp-write agent (json-object "jsonrpc" "2.0" "id" id
                                "result" (or result (make-hash-table :test #'equal)))))

(defun acp-respond-error (agent id code message &optional data)
  "Answer request ID with the JSON-RPC error CODE, MESSAGE, and DATA when given."
  (acp-write agent (json-object "jsonrpc" "2.0" "id" (if (null id) :null id)
                                "error" (json-object "code" code "message" message
                                                     :opt "data" data))))

(defun acp-notify (agent method params)
  "Send the client the notification METHOD with PARAMS."
  (acp-write agent (json-object "jsonrpc" "2.0" "method" method "params" params)))

(defun acp-request (agent method params &optional context)
  "Send the client the request METHOD with PARAMS, its answer awaited under
CONTEXT (ACP-ANSWERED). => the request's id."
  (let ((id (bt2:with-lock-held (agent.lock)
              (let ((id (format nil "nodecode-~d" (incf agent.next-id))))
                (setf (gethash id agent.asked) context)
                id))))
    (acp-write agent (json-object "jsonrpc" "2.0" "id" id "method" method "params" params))
    id))

(defun acp-update (agent session update)
  "One session/update of SESSION carrying UPDATE."
  (acp-notify agent "session/update" (json-object "sessionId" session.id "update" update)))

(defun acp-text-block (text)
  (json-object "type" "text" "text" text))

(defun acp-chunk (agent session kind text)
  "TEXT as a chunk of KIND (\"agent_message_chunk\" and the like), when it says anything."
  (when (plusp (length text))
    (acp-update agent session (json-object "sessionUpdate" kind "content" (acp-text-block text)))))

(defun acp-end (agent why)
  "End AGENT: WHY is :EOF (stdin closed) or the reason it could not go on."
  (bt2:with-lock-held (agent.lock)
    (unless agent.outcome
      (setf agent.outcome why)
      (bt2:signal-semaphore agent.done))))

;;; --- the gateway's control routes ---------------------------------------------

(defun acp-http (agent method path &optional body)
  "One control call to the gateway AGENT reaches. A refusal is an
ACP-REFUSAL carrying the route's own words."
  (handler-case (gateway-http-session-request agent.port method path body agent.token)
    (gateway-http-error (refusal)
      (acp-refuse (if (eql 404 (gateway-http-error-status refusal)) -32002 -32603)
                  "~a" (or (gateway-http-error-detail refusal) refusal)))
    (error (condition)
      (acp-refuse -32603 "the organism did not answer: ~a" condition))))

(defun acp-session-path (session-id &optional op)
  (format nil "/api/gateway/sessions/~a~@[/~a~]"
          (quri:url-encode session-id) op))

(defun acp-model-value (provider model)
  "The config value naming PROVIDER's MODEL."
  (format nil "~a/~a" provider model))

(defun acp-split-model-value (value)
  "VALUE, a model config value, as (values PROVIDER MODEL): a provider id
never holds a slash, a model id may."
  (let ((slash (position #\/ value)))
    (if (and slash (plusp slash) (< slash (1- (length value))))
        (values (subseq value 0 slash) (subseq value (1+ slash)))
        (acp-refuse -32602 "~s names no model: the value is PROVIDER/MODEL" value))))

(defun acp-target (agent session)
  "The target SESSION's next turn binds, as GET /api/gateway/target answers,
or NIL when the organism cannot say."
  (ignore-errors
   (acp-http agent :get (format nil "/api/gateway/target?session_id=~a"
                                (quri:url-encode session.id)))))

(defun acp-config-options (agent session)
  "SESSION's model and reasoning effort as ACP config options: the model a
select over the organism's catalog, grouped by provider; the effort a select
over the rungs its model admits, when it admits any."
  (let* ((target (acp-target agent session))
         (pick session.pick)
         (provider (or (getf pick :provider) (json-value target :string "provider")))
         (model (or (getf pick :model) (json-value target :string "model")))
         (catalog (ignore-errors (acp-http agent :get "/api/gateway/models")))
         (groups (make-hash-table :test #'equal))
         (order '())
         (options '()))
    (loop for row across (or (json-value catalog :array "models") #())
          for p = (json-value row :text "provider")
          for m = (json-value row :text "model")
          when (and p m)
            do (unless (gethash p groups) (push p order))
               (push (json-object "value" (acp-model-value p m)
                                  "name" (or (json-value row :text "name") m))
                     (gethash p groups)))
    (when (and provider model
               (notany (lambda (option) (equal (gethash "value" option) (acp-model-value provider model)))
                       (gethash provider groups)))
      (unless (gethash provider groups) (push provider order))
      (push (json-object "value" (acp-model-value provider model) "name" model)
            (gethash provider groups)))
    (when (and provider model)
      (push (json-object "id" "model" "name" "Model" "category" "model" "type" "select"
                         "currentValue" (acp-model-value provider model)
                         "options" (map 'vector
                                        (lambda (id)
                                          (json-object "group" id "name" id
                                                       "options" (coerce (reverse (gethash id groups)) 'vector)))
                                        (sort order #'string-lessp)))
            options))
    (let ((efforts (coerce (or (json-value target :array "efforts") #()) 'list))
          (effort (or (getf pick :effort) (json-value target :string "effort"))))
      (when efforts
        (push (json-object "id" "effort" "name" "Reasoning" "category" "thought_level" "type" "select"
                           "currentValue" (or effort "default")
                           "options" (coerce (append (unless effort
                                                       (list (json-object "value" "default"
                                                                          "name" "Model default")))
                                                     (loop for rung in efforts
                                                           collect (json-object "value" rung "name" rung)))
                                             'vector))
              options)))
    (coerce (reverse options) 'vector)))

(defparameter +acp-unlisted-commands+ '("clear" "undo" "redo")
  "The slash commands left out of the list a client offers: each moves the
session's head, and a client has no way to be told its transcript no longer
stands -- an editor's own new thread is its fresh start. Typed, they still run.")

(defun acp-slash-catalog (agent)
  "The slash commands the organism runs for a client, as ACP available
commands, every one of them."
  (let ((catalog (ignore-errors (acp-http agent :get "/api/gateway/slash"))))
    (map 'list
         (lambda (entry)
           (json-object "name" (string-left-trim "/" (json-value entry :string "name"))
                        "description" (or (json-value entry :string "description") "")))
         (remove-if-not (lambda (entry) (json-value entry :text "name"))
                        (or (json-value catalog :array "commands") #())))))

(defun acp-available-commands (agent)
  "The slash commands a client offers its person (ACP-SLASH-CATALOG less
+ACP-UNLISTED-COMMANDS+)."
  (coerce (remove-if (lambda (command)
                       (member (gethash "name" command) +acp-unlisted-commands+ :test #'string=))
                     (acp-slash-catalog agent))
          'vector))

(defun acp-slash-command-p (agent text)
  "Whether TEXT, a prompt, runs one of the organism's slash commands."
  (let ((name (and (plusp (length text)) (char= #\/ (char text 0))
                   (subseq text 1 (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline))) text)))))
    (and (plusp (length name))
         (find name (acp-slash-catalog agent)
               :key (lambda (command) (gethash "name" command)) :test #'string=))))

;;; --- the links ------------------------------------------------------------------

(defun acp-subscribe (session ws)
  "Subscribe WS to SESSION alone, answering the organism's questions,
resumed past the newest fact read when one was."
  (send-gateway-command
   ws :set-session-subscriptions
   (json-object "subscriptions" (vector (json-object "session_id" session.id
                                                     :opt "cursor" session.cursor))
                "answers" t
                "pid" (sb-posix:getpid))))

(defun acp-attach (agent session)
  "Open SESSION's link to the gateway AGENT reaches: handshake, subscribe,
and return once attached. A gateway that is not there is GATEWAY-UNREACHED."
  (let* ((ws (gateway-client agent.port agent.token))
         (ready (bt2:make-semaphore :name "acp-attached"))
         (state (list nil)))
    (wsd:on :message ws
            (lambda (message)
              (if (eq (car state) :attached)
                  (handler-case (acp-on-frame agent session message)
                    (error (condition)
                      (format *error-output* "nodecode acp: a frame of ~a: ~a~%" session.id condition)))
                  (handler-case
                      (let ((frame (nlk:decode-sync-message message)))
                        (case (nlk:sync-message-type frame)
                          (:gateway-connect-challenge
                           (answer-gateway-challenge ws frame "acp" agent.token))
                          (:gateway-connect-ack
                           (setf (car state) :attached)
                           (acp-subscribe session ws)
                           (bt2:signal-semaphore ready))
                          (:gateway-connect-rejected
                           (setf (car state) (or (gethash "message" (nlk:sync-message-payload frame))
                                                 "the gateway refused the connection"))
                           (bt2:signal-semaphore ready))))
                    (error (condition)
                      (setf (car state) (princ-to-string condition))
                      (bt2:signal-semaphore ready))))))
    (wsd:on :close ws
            (lambda (&key reason &allow-other-keys)
              (if (eq (car state) :attached)
                  (acp-link-lost agent session ws reason)
                  (progn (setf (car state) (format nil "the gateway closed the connection~@[: ~a~]"
                                                   (and (plusp (length reason)) reason)))
                         (bt2:signal-semaphore ready)))))
    (setf session.ws ws)
    (handler-case (wsd:start-connection ws)
      (error (condition) (setf (car state) (princ-to-string condition))))
    (when (null (car state))
      (bt2:wait-on-semaphore ready :timeout *rpc-attach-seconds*))
    (unless (eq (car state) :attached)
      (ignore-errors (nlk:sever-websocket ws))
      (error 'gateway-unreached :port agent.port
                                :why (or (car state) "the gateway never answered the handshake")))
    (bt2:with-lock-held (agent.lock)
      (if (eq session.link :closed)
          (ignore-errors (nlk:sever-websocket ws))
          (setf session.link :attached)))
    session))

(defun acp-detach (session)
  "Let go of SESSION's link for good."
  (let ((ws (shiftf session.ws nil)))
    (setf session.link :closed)
    (when ws (ignore-errors (nlk:sever-websocket ws)))))

(defun acp-link-lost (agent session ws why)
  "The gateway let go of SESSION's link WS: attach it again to whatever
serves next, unless the session was closed or the link replaced."
  (when (bt2:with-lock-held (agent.lock)
          (when (and (eq ws session.ws) (eq session.link :attached))
            (setf session.link :lost)
            t))
    (format *error-output* "nodecode acp: the organism let go of ~a~@[ (~a)~]; attaching again~%"
            session.id (and (plusp (length why)) why))
    (acp-reattach agent)))

(defun acp-reattach (agent)
  "Attach every lost link again, to the organism serving the store now --
started here when none does -- each resumed past the newest fact it read.
When none serves within *ACP-REATTACH-SECONDS*, the prompts in flight fail
and the agent ends."
  (when (bt2:with-lock-held (agent.lock)
          (unless agent.reattaching (setf agent.reattaching t)))
    (spawn "acp-reattach"
      (with-cleanup ((bt2:with-lock-held (agent.lock) (setf agent.reattaching nil)))
        (let ((deadline (deadline-after *acp-reattach-seconds*)))
          (loop
            (let ((lost (bt2:with-lock-held (agent.lock)
                          (loop for session being the hash-values of agent.sessions
                                when (eq session.link :lost) collect session))))
              (when (null lost) (return))
              (let ((port (and agent.find-port (funcall agent.find-port deadline))))
                (when (null port)
                  (acp-give-up agent lost "no organism serves this home any more")
                  (return))
                (setf agent.port port)
                (dolist (session lost)
                  (handler-case (acp-attach agent session)
                    (gateway-unreached ()
                      (bt2:with-lock-held (agent.lock)
                        (unless (eq session.link :closed) (setf session.link :lost))))))
                (when (>= (get-internal-real-time) deadline)
                  (acp-give-up agent (remove :attached lost :key #'acp-session-link)
                               "the organism could not be reached again")
                  (return))
                (sleep 0.05)))))))))

(defun acp-give-up (agent sessions why)
  "SESSIONS lost their organism for good: their prompts fail with WHY, and
the agent ends."
  (dolist (session sessions)
    (when-let (prompt (shiftf session.prompt nil))
      (acp-respond-error agent (getf prompt :id) -32603 why)))
  (when sessions
    (acp-end agent why)))

;;; --- facts and frames -----------------------------------------------------------

(defun acp-message-text (message)
  "The text of MESSAGE's content: a string, or an array of parts."
  (let ((content (json-value message :any "content")))
    (cond ((stringp content) content)
          ((vectorp content)
           (with-output-to-string (out)
             (loop for part across content
                   when (equal "text" (json-value part :string "type"))
                     do (write-string (or (json-value part :string "text") "") out))))
          (t ""))))

(defun acp-unsaid (said whole)
  "What of WHOLE was not yet sent, SAID being what was streamed of it: the
rest after it when WHOLE goes on from it, all of it when nothing streamed."
  (cond ((zerop (length said)) whole)
        ((uiop:string-prefix-p said whole) (subseq whole (length said)))
        (t "")))

;;; A call is told the way the transcript tells it (tool-summary.lisp): the
;;; same activity names its title (`Read src/x.lisp', `Ran just lint', the
;;; snippet's note when nothing known ran) and its ACP kind, as it opens from
;;; the snippet alone and again as it closes, once its receipts and its
;;; workspace diff say what it did. The diff rides the close as ACP diff
;;; content, a hunk a block, as the Claude and Codex adapters send theirs.

(defun acp-activity (name arguments &key metadata failed)
  "What the call of tool NAME with ARGUMENTS (its JSON text) did, as the
transcript reads it -- with METADATA, its result's receipts and workspace
diff -- or NIL when the transcript has no words for it. A classifier that
signals is said on stderr, and the call goes out unclassified."
  (handler-case (tool-arguments-activity name (or arguments "") :metadata metadata :error-p failed)
    (error (condition)
      (format *error-output* "nodecode acp: a ~a call went out unclassified: ~a~%" name condition)
      nil)))

(defun acp-call-kind (name activity)
  "The ACP tool kind of a call of tool NAME that did ACTIVITY."
  (if activity
      (case (activity-family activity)
        ((:read :list :image) "read")
        ((:search :recall) "search")
        (:fetch "fetch")
        ((:edit :write) "edit")
        (:command "execute")
        (t "other"))
      (cond ((search "search" name) "search")
            ((search "fetch" name) "fetch")
            (t "other"))))

(defun acp-call-title (name activity &key running)
  "The title of a call of tool NAME that did ACTIVITY: its words, RUNNING
in the present tense, else NAME."
  (let ((title (and activity (activity-title activity :running running))))
    (if (plusp (length title)) title name)))

(defun acp-absolute-path (session path)
  "PATH, as a call or the workspace diff spells a file, absolute: one
relative to SESSION's folder is joined to it."
  (let ((path (if (uiop:string-prefix-p "./" path) (subseq path 2) path)))
    (if (or (uiop:string-prefix-p "/" path) (uiop:string-prefix-p "~" path) (null session.cwd))
        path
        (uiop:native-namestring
         (merge-pathnames (uiop:parse-native-namestring path)
                          (uiop:ensure-directory-pathname (uiop:parse-native-namestring session.cwd)))))))

(defun acp-patch-hunks (patch)
  "PATCH, one file's git diff, as its hunks: (NEW-START OLD-TEXT NEW-TEXT)
each, the lines either side of the hunk reads, its context on both."
  (let ((hunks '()) (start nil) (old '()) (new '()))
    (flet ((close-hunk ()
             (when start
               (push (list start (format nil "~{~a~%~}" (reverse old)) (format nil "~{~a~%~}" (reverse new)))
                     hunks))))
      (dolist (line (uiop:split-string patch :separator '(#\Newline)))
        (cond ((uiop:string-prefix-p "@@" line)
               (close-hunk)
               (setf old '() new '()
                     start (or (ppcre:register-groups-bind ((#'parse-integer from)) ("^@@ -\\S+ \\+(\\d+)" line)
                                 from)
                               1)))
              ((null start))
              ((uiop:string-prefix-p "+" line) (push (subseq line 1) new))
              ((uiop:string-prefix-p "-" line) (push (subseq line 1) old))
              ((uiop:string-prefix-p " " line) (push (subseq line 1) old) (push (subseq line 1) new))))
      (close-hunk))
    (nreverse hunks)))

(defun acp-diff-files (session metadata)
  "The text files a call's result METADATA says it changed, from its
workspace diff, as (PATH CREATED DELETED HUNKS): PATH absolute, HUNKS as
ACP-PATCH-HUNKS reads its patch."
  (loop for file across (json-array metadata "workspace_diff" "files")
        for path = (json-value file :text "path")
        for patch = (json-value file :string "patch")
        when (and path patch)
          collect (list (acp-absolute-path session path)
                        (and (ppcre:scan "(?m)^--- /dev/null$" patch) t)
                        (and (ppcre:scan "(?m)^\\+\\+\\+ /dev/null$" patch) t)
                        (acp-patch-hunks patch))))

(defun acp-diff-content (files)
  "FILES (ACP-DIFF-FILES) as ACP diff content: a block a hunk, a new file's
old text null and a deleted one's new text empty."
  (loop for (path created deleted hunks) in files
        nconc (loop for (nil old new) in hunks
                    collect (json-object "type" "diff" "path" path
                                         "oldText" (if created :null old)
                                         "newText" (if deleted "" new)))))

(defun acp-locations (session activity files)
  "The files a call that did ACTIVITY is about, as ACP locations: those
FILES (ACP-DIFF-FILES) changed first, at their first hunk's line, then every
file its entries name. NIL when it names none."
  (let ((seen '()) (locations '()))
    (flet ((add (path line)
             (unless (member path seen :test #'string=)
               (push path seen)
               (push (json-object "path" path :opt "line" line) locations))))
      (loop for (path nil nil hunks) in files
            do (add path (first (first hunks))))
      (loop for (family source detail) in (and activity (activity-entries activity))
            when (and (stringp source) (plusp (length source))
                      (eq :path (getf (activity-family-plist family) :source)))
              do (add (acp-absolute-path session source)
                      (and (eq family :read) (stringp detail)
                           (ppcre:register-groups-bind ((#'parse-integer line)) ("^(\\d+)" detail) line)))))
    (when locations (coerce (nreverse locations) 'vector))))

(defun acp-shell-commands (name arguments metadata)
  "The shell commands a call of tool NAME with ARGUMENTS ran: what its
receipts say ran, with METADATA, else what its snippet spells."
  (let ((text (tool-argument-text arguments +tool-command-keys+)))
    (cond ((member name *shell-tool-names* :test #'equal) (and text (list text)))
          ((loop for (verb . source) in (receipt-calls metadata)
                 when (and (equal verb "sh") source) collect source))
          (t (loop for (verb . source) in (and text (ignore-errors (snippet-calls text)))
                   when (and (equal verb "sh") (stringp source)) collect source)))))

(defun acp-raw-input (name arguments kind &optional metadata)
  "A call's ARGUMENTS as the client is given them: the object they decode
to, with the command it runs under \"command\" when KIND is execute, as ACP
clients read a command; the text when it is no object."
  (let ((decoded (ignore-errors (nlk:decode-json arguments))))
    (cond ((not (hash-table-p decoded)) arguments)
          (t (when (equal kind "execute")
               (when-let (commands (acp-shell-commands name arguments metadata))
                 (setf (gethash "command" decoded) (format nil "~{~a~^~%~}" commands))))
             decoded))))

(defun acp-tool-call (session payload)
  "The tool_call update a turn.tool_call_started PAYLOAD opens, the call
held in SESSION until its result closes it."
  (let* ((name (or (json-value payload :text "tool-name") "tool"))
         (arguments (or (json-value payload :string "arguments") ""))
         (id (or (json-value payload :text "call-id") (nlk:make-durable-id "call")))
         (activity (acp-activity name arguments))
         (kind (acp-call-kind name activity)))
    (setf (gethash id session.calls) (cons name arguments))
    (json-object "sessionUpdate" "tool_call"
                 "toolCallId" id
                 "title" (acp-call-title name activity :running t)
                 "kind" kind
                 "status" "in_progress"
                 :opt "locations" (acp-locations session activity '())
                 "rawInput" (acp-raw-input name arguments kind))))

(defun acp-result-text (result)
  "RESULT, a tool's answer to the model, as a person reads it: without the
size line an eval's printed output opens with (`[text: 3 lines, 95
characters]'), which the model reads and an editor's output pane need not."
  (let ((newline (position #\Newline result)))
    (if (and newline (uiop:string-prefix-p "[text: " result) (char= #\] (char result (1- newline))))
        (subseq result (1+ newline))
        result)))

(defun acp-exit-code (result)
  "The exit status of the last shell command RESULT reports (`[exit 2]'), or NIL."
  (let ((code nil))
    (ppcre:do-register-groups ((#'parse-integer status)) ("\\[exit (-?\\d+)\\]" result)
      (setf code status))
    code))

(defun acp-tool-result (session payload)
  "The tool_call_update a turn.tool_result PAYLOAD closes its call with:
said again from what the call did -- its title, kind and files -- with the
diff of every file it changed ahead of its result."
  (let* ((id (or (json-value payload :text "call-id") ""))
         (call (gethash id session.calls))
         (name (or (car call) (json-value payload :text "tool-name") "tool"))
         (arguments (cdr call))
         (metadata (json-value payload :object "metadata"))
         (failed (or (json-value payload :boolean "failed")
                     (and (json-value metadata :any "stopped") t)))
         (activity (and arguments (acp-activity name arguments :metadata metadata :failed failed)))
         (kind (and arguments (acp-call-kind name activity)))
         (files (acp-diff-files session metadata))
         (result (acp-result-text (or (json-value payload :string "result") "")))
         (shown (if (> (length result) *acp-tool-output-chars*)
                    (format nil "~a~%... (~:d more characters)"
                            (subseq result 0 *acp-tool-output-chars*)
                            (- (length result) *acp-tool-output-chars*))
                    result)))
    (remhash id session.calls)
    (json-object "sessionUpdate" "tool_call_update"
                 "toolCallId" id
                 "status" (if failed "failed" "completed")
                 :opt "title" (and arguments (acp-call-title name activity))
                 :opt "kind" kind
                 :opt "locations" (acp-locations session activity files)
                 :opt "rawInput" (and (equal kind "execute") (acp-raw-input name arguments kind metadata))
                 :opt "rawOutput" (when-let (code (and (equal kind "execute") (acp-exit-code result)))
                                    (json-object "exitCode" code))
                 "content" (coerce (append (acp-diff-content files)
                                           (list (json-object "type" "content" "content" (acp-text-block shown))))
                                   'vector))))

(defun acp-fail-open-calls (agent session)
  "Close every call of SESSION still open as failed: its turn ended without
its result, and a client would show it running for good."
  (dolist (id (alexandria:hash-table-keys session.calls))
    (remhash id session.calls)
    (acp-update agent session (json-object "sessionUpdate" "tool_call_update"
                                           "toolCallId" id "status" "failed"))))

(defun acp-own-turn-p (session fact)
  "Whether FACT belongs to a turn this agent started."
  (or (member (json-value fact :string "turn_id") session.turns :test #'equal)
      (member (json-value fact :string "event" "metadata" "command_id") session.commands :test #'equal)))

(defun acp-on-fact (agent session fact &key history)
  "Send the client what FACT, one durable fact of SESSION, says: as history
(a load's replay) or as it lands."
  (let ((kind (json-value fact :string "event" "type"))
        (payload (json-value fact :object "event" "payload"))
        (turn (json-value fact :string "turn_id")))
    (when-let (prompt session.prompt)
      ;; A queued prompt learns its turn from the turn's own facts.
      (when (and (null (getf prompt :turn)) turn
                 (equal (getf prompt :command) (json-value fact :string "event" "metadata" "command_id")))
        (setf (getf (acp-session-prompt session) :turn) turn)
        (pushnew turn session.turns :test #'equal)
        (when (getf prompt :cancelled)
          (ignore-errors (send-cancel-turn session.ws session.id turn)))))
    (cond
      ((equal kind "turn.started")
       (setf session.begun t))
      ((equal kind "turn.input_committed")
       (unless (and (not history) (acp-own-turn-p session fact))
         (acp-chunk agent session "user_message_chunk"
                    (let ((message (json-value payload :any "message")))
                      (if (stringp message) message (acp-message-text message))))))
      ((equal kind "turn.assistant_message_completed")
       (let ((message (json-value payload :object "message")))
         (acp-chunk agent session "agent_thought_chunk"
                    (acp-unsaid (if history "" session.thought)
                                (or (json-value message :string "reasoning_content") "")))
         (acp-chunk agent session "agent_message_chunk"
                    (acp-unsaid (if history "" session.said) (acp-message-text message)))
         (setf session.said "" session.thought "")))
      ((equal kind "turn.tool_call_started")
       (acp-update agent session (acp-tool-call session payload)))
      ((equal kind "turn.tool_result")
       (acp-update agent session (acp-tool-result session payload)))
      ((equal kind "turn.usage")
       (unless history
         (acp-count-usage session turn payload)))
      ((member kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal)
       (setf session.said "" session.thought "")
       (acp-fail-open-calls agent session)
       (unless history
         (acp-turn-ended agent session turn kind payload))))))

;;; --- usage ------------------------------------------------------------------------
;;; A prompt's answer carries what its turn spent, and a usage_update after
;;; it what now sits in the context window, of how large a one, and what the
;;; whole session has cost: the meter an editor draws, as the Claude and Codex
;;; adapters feed theirs.

(defparameter +acp-usage-counts+
  '(("input-tokens" . "inputTokens") ("output-tokens" . "outputTokens")
    ("cached-input-tokens" . "cachedReadTokens") ("cache-write-tokens" . "cachedWriteTokens")
    ("reasoning-tokens" . "thoughtTokens") ("total-tokens" . "totalTokens"))
  "Each count a turn.usage fact carries, and the ACP usage field it adds to.")

(defun acp-count-usage (session turn payload)
  "Add one round's turn.usage PAYLOAD to what SESSION's prompt in flight has
spent, when the round is that prompt's TURN's."
  (let ((prompt session.prompt))
    (when (and prompt turn (equal turn (getf prompt :turn)))
      (let ((usage (or session.usage (setf session.usage (make-hash-table :test #'equal)))))
        (loop for (key . field) in +acp-usage-counts+
              for count = (json-value payload :integer key)
              when count
                do (incf (gethash field usage 0) count))))))

(defun acp-turn-usage (session)
  "What SESSION's prompt in flight spent, as ACP usage, or NIL when no round
reported: its total the providers' own when they gave one, else the sum."
  (when-let (usage session.usage)
    (unless (gethash "totalTokens" usage)
      (setf (gethash "totalTokens" usage)
            (loop for field in '("inputTokens" "outputTokens" "cachedReadTokens" "cachedWriteTokens")
                  sum (gethash field usage 0))))
    (dolist (field '("inputTokens" "outputTokens"))
      (unless (gethash field usage) (setf (gethash field usage) 0)))
    usage))

(defun acp-context-window (agent session model)
  "The context window, in tokens, of MODEL as the organism's catalog sizes
it -- the row of SESSION's provider first -- or NIL when it does not say."
  (let* ((provider (or (getf session.pick :provider)
                       (json-value (acp-target agent session) :string "provider")))
         (rows (coerce (json-array (acp-http agent :get "/api/gateway/models") "models") 'list))
         (same (remove model rows :key (lambda (row) (json-value row :string "model"))
                                  :test-not #'equal)))
    (json-value (or (find provider same :key (lambda (row) (json-value row :string "provider"))
                                        :test #'equal)
                    (first same))
                :integer "context")))

(defun acp-usage-update (agent session)
  "Tell the client what sits in SESSION's context window, of how large a
one, and what the session has cost so far, once a round has said; nothing
while the organism cannot size the window. A route that fails is said on
stderr: the meter is the client's comfort, and the answer it rides beside
must still go."
  (handler-case
      (let* ((header (acp-http agent :get (acp-session-path session.id "header")))
             (context (json-value header :object "context_usage"))
             (spent (json-value header :object "session_usage"))
             (model (json-value context :text "model"))
             (size (and model (acp-context-window agent session model))))
        (when (and context size)
          (acp-update agent session
                      (json-object "sessionUpdate" "usage_update"
                                   "used" (loop for key in '("input" "cached" "cache_write")
                                                sum (or (json-value context :integer key) 0))
                                   "size" size
                                   :opt "cost" (when (and (json-value spent :boolean "cost_known")
                                                          (json-value spent :number "cost"))
                                                 (json-object "amount" (json-value spent :number "cost")
                                                              "currency" "USD"))))))
    (error (condition)
      (format *error-output* "nodecode acp: no usage for ~a: ~a~%" session.id condition))))

(defun acp-turn-ended (agent session turn kind payload)
  "TURN of SESSION ended as KIND says: the prompt that waits on it is answered."
  (let ((prompt (bt2:with-lock-held (agent.lock)
                  (let ((prompt session.prompt))
                    (when (and prompt (equal turn (getf prompt :turn)))
                      (setf session.prompt nil)
                      prompt)))))
    (when prompt
      (acp-usage-update agent session)
      (let ((usage (acp-turn-usage session)))
        (setf session.usage nil)
        (flet ((stop (reason)
                 (acp-respond agent (getf prompt :id)
                              (json-object "stopReason" reason :opt "usage" usage))))
          (cond ((getf prompt :cancelled) (stop "cancelled"))
                ((equal kind "turn.completed") (stop "end_turn"))
                ((equal kind "turn.cancelled") (stop "cancelled"))
                (t (acp-respond-error agent (getf prompt :id) -32603
                                      (or (json-value payload :text "detail") "the turn failed")))))))))

(defun acp-on-replay (agent session frame payload)
  "One chunk of SESSION's replay: sent as history while a load waits on it,
folded onto what was read after a resume, read for its cursor otherwise."
  (unless (json-value payload :boolean "replay_tail")
    (let ((resumed (json-value payload :integer "resumed_after"))
          (final (or (null (json-value payload :integer "replay_chunk_index"))
                     (json-value payload :boolean "replay_chunk_final"))))
      (loop for fact across (or (json-value payload :array "accumulated_frames") #())
            for seq = (json-value fact :integer "seq")
            unless (and resumed seq session.cursor (<= seq session.cursor))
              do (cond (resumed (acp-on-fact agent session fact))
                       ((eq session.replay :history) (acp-on-fact agent session fact :history t))
                       ((equal "turn.started" (json-value fact :string "event" "type"))
                        (setf session.begun t)))
                 (when seq (setf session.cursor (max seq (or session.cursor 0)))))
      (when final
        (when-let (cursor (json-value frame :integer "cursor"))
          (setf session.cursor (max cursor (or session.cursor 0))))
        (setf session.replay :quiet)
        (unless resumed
          (acp-title agent session (json-value payload :text "title")))
        (when-let (id (shiftf session.opening nil))
          (acp-respond agent id (json-object "configOptions" (acp-session-config agent session)))
          (acp-commands-update agent session)
          (when session.begun (acp-usage-update agent session)))))))

(defun acp-title (agent session title)
  "Tell the client SESSION's name is TITLE, when it names one."
  (when title
    (acp-update agent session (json-object "sessionUpdate" "session_info_update" "title" title))))

(defun acp-session-config (agent session)
  "SESSION's config options (ACP-CONFIG-OPTIONS), held as the ones the
client was last told."
  (let ((options (acp-config-options agent session)))
    (setf session.told-config (encode-json-object (json-object "o" options)))
    options))

(defun acp-config-update (agent session)
  "Tell the client SESSION's config options when they are not the ones it
was last told: a move the organism announces more than once (a session's
first pin, the default moving under it) goes out once."
  (let* ((options (acp-config-options agent session))
         (text (encode-json-object (json-object "o" options))))
    (unless (equal text session.told-config)
      (setf session.told-config text)
      (acp-update agent session (json-object "sessionUpdate" "config_option_update"
                                             "configOptions" options)))))

(defun acp-commands-update (agent session)
  "Tell the client the slash commands SESSION's prompts may run, when they
are not the ones it was last told: a catalog the organism says moved more
than once goes out once."
  (let* ((commands (acp-available-commands agent))
         (text (encode-json-object (json-object "c" commands))))
    (when (and (plusp (length commands)) (not (equal text session.told-commands)))
      (setf session.told-commands text)
      (acp-update agent session (json-object "sessionUpdate" "available_commands_update"
                                             "availableCommands" commands)))))

(defun acp-on-result (agent session result)
  "A command's RESULT: the start_turn of the prompt in flight names its turn,
or refuses it."
  (let ((prompt session.prompt))
    (when (and prompt (equal (getf prompt :command) (json-value result :string "command_id")))
      (if (equal "rejected" (json-value result :string "status"))
          (when (bt2:with-lock-held (agent.lock)
                  (when (eq prompt session.prompt) (setf session.prompt nil) t))
            (acp-respond-error agent (getf prompt :id)
                               (if (member (json-value result :string "code")
                                           '("unknown_provider" "effort_unsupported" "session_pinned")
                                           :test #'equal)
                                   -32602 -32603)
                               (or (json-value result :text "message") "the prompt was refused")))
          (let ((turn (json-value result :text "response" "turn_id")))
            (setf session.begun t)
            (when turn
              (pushnew turn session.turns :test #'equal)
              (setf (getf (acp-session-prompt session) :turn) turn)
              (when (getf prompt :cancelled)
                (ignore-errors (send-cancel-turn session.ws session.id turn)))))))))

(defun acp-on-delta (agent session payload)
  "One item_delta: streamed text and thinking go out as they come."
  (let* ((delta (json-value payload :object "delta"))
         (type (json-value delta :string "type"))
         (text (or (json-value delta :string "text") "")))
    (cond ((equal type "text")
           (setf session.said (concatenate 'string session.said text))
           (acp-chunk agent session "agent_message_chunk" text))
          ((equal type "reasoning")
           (setf session.thought (concatenate 'string session.thought text))
           (acp-chunk agent session "agent_thought_chunk" text))
          ;; A retry voids what its attempt streamed: the next streams anew.
          ((equal type "stream_reset")
           (setf session.said "" session.thought "")))))

(defparameter +acp-allow-words+ '("allow" "yes" "approve" "accept" "ok" "run" "continue" "proceed")
  "Words a question's choice begins with when it lets the thing asked about happen.")

(defun acp-choice-kind (choice)
  "The permission option kind CHOICE, one answer a question offers, reads as."
  (let ((word (string-downcase (string-trim " " choice))))
    (flet ((starts (words) (some (lambda (w) (uiop:string-prefix-p w word)) words)))
      (cond ((and (starts +acp-allow-words+) (search "always" word)) "allow_always")
            ((starts +acp-allow-words+) "allow_once")
            ((search "always" word) "reject_always")
            (t "reject_once")))))

(defun acp-on-questions (agent session payload)
  "The organism's open questions: each new one of SESSION's goes to the
client as a session/request_permission; one gone before the client answered
is withdrawn."
  (let ((open (remove-if-not (lambda (question)
                               (equal session.id (json-value question :string "session_id")))
                             (or (json-value payload :array "questions") #()))))
    (loop for question across open
          for qid = (json-value question :text "question_id")
          when (and qid (not (gethash qid session.questions)))
            do (setf (gethash qid session.questions)
                     (acp-request agent "session/request_permission"
                                  (json-object
                                   "sessionId" session.id
                                   "toolCall" (json-object "toolCallId" qid
                                                           "title" (or (json-value question :text "title") "")
                                                           :opt "content"
                                                           (when-let (detail (json-value question :text "detail"))
                                                             (vector (json-object "type" "content"
                                                                                  "content" (acp-text-block detail)))))
                                   "options" (map 'vector
                                                  (lambda (choice)
                                                    (json-object "optionId" choice "name" choice
                                                                 "kind" (acp-choice-kind choice)))
                                                  (json-value question :array "choices")))
                                  (cons session qid))))
    (loop for qid in (alexandria:hash-table-keys session.questions)
          unless (find qid open :key (lambda (question) (json-value question :string "question_id"))
                                :test #'equal)
            do (let ((id (gethash qid session.questions)))
                 (remhash qid session.questions)
                 (bt2:with-lock-held (agent.lock) (remhash id agent.asked))
                 (acp-notify agent "$/cancel_request" (json-object "requestId" id))))))

(defun acp-on-frame (agent session message)
  "One frame the gateway sent SESSION's link."
  (let* ((frame (nlk:decode-json message))
         (payload (json-value frame :object "payload"))
         (type (json-value payload :string "type")))
    (cond
      ((equal type "session_stream_resumed") (acp-on-replay agent session frame payload))
      ((equal type "session_stream_event")
       (let ((fact (json-value payload :object "frame"))
             (cursor (json-value frame :integer "cursor")))
         (unless (and cursor session.cursor (<= cursor session.cursor))
           (acp-on-fact agent session fact)
           (when cursor (setf session.cursor cursor)))))
      ((equal type "item_delta") (acp-on-delta agent session payload))
      ((equal type "gateway_turn_command_result")
       (acp-on-result agent session (json-value payload :object "result")))
      ((equal type "organism.questions") (acp-on-questions agent session payload))
      ;; A name is no fact of the stream (session.title is config evidence):
      ;; the organism says it to every shell.
      ((equal type "session_titled")
       (when (equal session.id (json-value payload :string "session_id"))
         (acp-title agent session (json-value payload :text "title"))))
      ;; The model or effort a turn binds moved -- this session's, or the
      ;; default a session with no pin of its own follows -- by a pick made
      ;; anywhere: the client's selects follow it.
      ((equal type "model_changed")
       (when (member (json-value payload :string "session_id") (list nil session.id) :test #'equal)
         (acp-config-update agent session)))
      ;; A cell started or stopped and its commands came or went.
      ((equal type "commands_changed")
       (acp-commands-update agent session))
      ;; The session's history moved under it (a checkpoint move elsewhere):
      ;; its whole replay again, read for its cursor -- what the client
      ;; already shows stays.
      ((equal type "session_resync_required")
       (setf session.cursor nil session.replay :quiet)
       (acp-subscribe session session.ws)))))

;;; --- requests ---------------------------------------------------------------------

(defun acp-session-of (agent params &key (open t))
  "The session PARAMS' sessionId names; with OPEN, one this client opened."
  (let ((id (json-value params :text "sessionId")))
    (unless id (acp-refuse -32602 "sessionId is required"))
    (or (bt2:with-lock-held (agent.lock) (gethash id agent.sessions))
        (and open (acp-refuse-unknown id "session ~a is not open here: session/load or session/resume it first" id)))))

(defun acp-check-session (agent session-id)
  "Refuse the request unless the organism holds a session SESSION-ID: an
unknown one is the client's not-found, naming it. => the session's detail."
  (handler-case (acp-http agent :get (acp-session-path session-id))
    (acp-refusal (refusal)
      (if (eql -32002 (acp-refusal-code refusal))
          (acp-refuse-unknown session-id "session ~a: ~a" session-id (acp-refusal-message refusal))
          (error refusal)))))

(defun acp-cwd (params)
  "The absolute folder PARAMS' cwd names."
  (let ((cwd (json-value params :text "cwd")))
    (unless (and cwd (uiop:absolute-pathname-p (uiop:parse-native-namestring cwd)))
      (acp-refuse -32602 "cwd must be an absolute path"))
    cwd))

(defun acp-open (agent session)
  "Register SESSION and attach its link, its replay read as SESSION.REPLAY says."
  (let ((old (bt2:with-lock-held (agent.lock)
               (prog1 (gethash session.id agent.sessions)
                 (setf (gethash session.id agent.sessions) session)))))
    ;; Opened again: the link before goes, and the prompt it carried is
    ;; answered, since its turn's end now reaches the new link alone.
    (when old
      (when-let (prompt (shiftf (acp-session-prompt old) nil))
        (acp-respond agent (getf prompt :id) (json-object "stopReason" "cancelled")))
      (acp-detach old)))
  (handler-case (acp-attach agent session)
    (gateway-unreached (condition)
      (bt2:with-lock-held (agent.lock) (remhash session.id agent.sessions))
      (acp-refuse -32603 "~a" condition))))

(defun acp-initialize (agent params)
  (declare (ignore params))
  (setf agent.initialized t)
  (json-object
   "protocolVersion" +acp-protocol-version+
   "agentCapabilities" (json-object
                        "loadSession" t
                        "promptCapabilities" (json-object "image" t "audio" nil "embeddedContext" t)
                        "mcpCapabilities" (json-object "http" nil "sse" nil)
                        "sessionCapabilities" (json-object "list" (json-object)
                                                           "resume" (json-object)
                                                           "fork" (json-object)
                                                           "close" (json-object)))
   "authMethods" #()
   "agentInfo" (json-object "name" "nodecode" "title" "Nodecode"
                            "version" (or (ignore-errors (effective-version)) *user-agent-version*))))

(defun acp-new-session (agent params)
  "session/new: a fresh gateway session working in the client's folder."
  (let* ((cwd (acp-cwd params))
         (session (make-acp-session :id (fresh-session-id) :cwd cwd)))
    (acp-http agent :post "/api/gateway/sessions" (json-object "session_id" session.id "cwd" cwd))
    (acp-open agent session)
    (values (json-object "sessionId" session.id
                         "configOptions" (acp-session-config agent session))
            (lambda () (acp-commands-update agent session)))))

(defun acp-fork-session (agent params)
  "session/fork: a new gateway session forked from the one PARAMS name at
its head, as the shell's /fork makes one, working in the client's folder."
  (let* ((cwd (acp-cwd params))
         (parent (json-value params :text "sessionId"))
         (session (make-acp-session :id (fresh-session-id) :cwd cwd)))
    (unless parent (acp-refuse -32602 "sessionId is required"))
    ;; A fork at an earlier point names a cursor the agent issued; this one
    ;; issues none, so a fork that names one would land somewhere else.
    (unless (member (gethash "cursorId" params) '(nil :null))
      (acp-refuse -32602 "~a is no cursor this agent issued: it forks at the newest turn alone"
                  (gethash "cursorId" params)))
    (acp-check-session agent parent)
    (acp-http agent :post "/api/gateway/sessions"
              (json-object "session_id" session.id "parent" parent "cwd" cwd))
    (acp-open agent session)
    (values (json-object "sessionId" session.id
                         "configOptions" (acp-session-config agent session))
            (lambda () (acp-commands-update agent session)))))

(defun acp-load-session (agent id params &key history)
  "session/load (HISTORY: the conversation sent again as updates) or
session/resume: the session's link, answered once its replay is read."
  (let* ((cwd (acp-cwd params))
         (session-id (json-value params :text "sessionId")))
    (unless session-id (acp-refuse -32602 "sessionId is required"))
    ;; The organism's own answer: an unknown id is the client's not-found.
    ;; The folder it works in is the one its files are said from.
    (let ((detail (acp-check-session agent session-id)))
      (acp-open agent (make-acp-session :id session-id
                                        :cwd (or (json-value detail :text "cwd") cwd)
                                        :replay (if history :history :quiet)
                                        :opening id)))
    ;; Answered from the link, once the replay ends (ACP-ON-REPLAY).
    :deferred))

(defun acp-list-sessions (agent params)
  "session/list: this home's sessions, newest first, a page at a time,
those begun in CWD alone when it is given."
  (let* ((cwd (json-value params :text "cwd"))
         (cursor (gethash "cursor" params))
         (start (cond ((member cursor '(nil :null)) 0)
                      ((and (stringp cursor) (plusp (length cursor)) (every #'digit-char-p cursor))
                       (parse-integer cursor))
                      (t (acp-refuse -32602 "~a is no cursor this agent gave" cursor))))
         (rows (remove-if-not (lambda (row) (or (null cwd) (equal cwd (json-value row :string "working_directory"))))
                              (coerce (or (acp-http agent :get "/api/gateway/sessions") #()) 'list)))
         (rows (stable-sort (copy-list rows) #'string>
                            :key (lambda (row) (or (json-value row :string "updated_at") ""))))
         (page (subseq rows (min start (length rows)) (min (+ start *acp-session-page*) (length rows)))))
    (json-object "sessions" (map 'vector
                                 (lambda (row)
                                   (json-object "sessionId" (json-value row :string "id")
                                                "cwd" (or (json-value row :text "working_directory") "")
                                                :opt "title" (json-value row :text "title")
                                                :opt "updatedAt" (json-value row :text "updated_at")))
                                 page)
                 :opt "nextCursor" (when (< (+ start *acp-session-page*) (length rows))
                                     (princ-to-string (+ start *acp-session-page*))))))

(defun acp-close-session (agent params)
  "session/close: its turn cancelled, its prompt answered, its link let go."
  (let ((session (acp-session-of agent params :open nil)))
    (when session
      (acp-cancel agent session)
      (bt2:with-lock-held (agent.lock) (remhash session.id agent.sessions))
      (when-let (prompt (shiftf session.prompt nil))
        (acp-respond agent (getf prompt :id) (json-object "stopReason" "cancelled")))
      (acp-detach session))
    nil))

(defun acp-prompt-content (blocks)
  "The prompt's content BLOCKS as (values TEXT IMAGES): the text a turn is
asked, files the client attached named or written out in it, and its
pictures as (MEDIA-TYPE . BASE64)."
  (let ((images '())
        (parts '()))
    (loop for block across (or blocks #())
          for type = (json-value block :string "type")
          do (cond
               ((equal type "text")
                (push (or (json-value block :string "text") "") parts))
               ((equal type "image")
                (let ((data (json-value block :text "data"))
                      (media (json-value block :text "mimeType")))
                  (when (and data media) (push (cons media data) images))))
               ((equal type "resource_link")
                (push (format nil "~a: ~a" (or (json-value block :text "name") "file")
                              (acp-uri-path (json-value block :string "uri")))
                      parts))
               ((equal type "resource")
                (let* ((resource (json-value block :object "resource"))
                       (text (json-value resource :string "text"))
                       (where (acp-uri-path (json-value resource :string "uri"))))
                  (if text
                      (push (format nil "~a:~%```~%~a~%```" where text) parts)
                      (push (format nil "attached: ~a" where) parts))))))
    (values (format nil "~{~a~^~%~%~}" (reverse parts)) (reverse images))))

(defun acp-uri-path (uri)
  "URI as the path it names when it is a file: URI, else URI itself."
  (cond ((null uri) "")
        ((uiop:string-prefix-p "file://" uri)
         (or (ignore-errors (quri:url-decode (subseq uri (length "file://")))) uri))
        (t uri)))

(defun acp-prompt (agent id params)
  "session/prompt: the turn it starts, answered with its stop reason when it
ends -- or, a line naming a slash command, that command run."
  (let ((session (acp-session-of agent params)))
    (multiple-value-bind (text images) (acp-prompt-content (json-value params :array "prompt"))
      (when (and (zerop (length (string-trim '(#\Space #\Newline #\Tab) text))) (null images))
        (acp-refuse -32602 "the prompt is empty"))
      (let ((running (bt2:with-lock-held (agent.lock) session.prompt)))
        (cond ((null running) (acp-start-prompt agent id session text images))
              ;; The prompt before was cancelled and its turn is still
              ;; winding down: a client that stops a turn to send another
              ;; (T3 Code does, for a message typed mid-turn) has this one
              ;; start once that one is answered, off the reading thread.
              ((getf running :cancelled)
               (spawn "acp-prompt-after-cancel"
                 (handler-case
                     (if (wait-until (lambda () (null session.prompt))
                                     (deadline-after *acp-reattach-seconds*) 0.05)
                         (acp-start-prompt agent id session text images)
                         (acp-refuse -32603 "the cancelled prompt before it in session ~a never ended"
                                     session.id))
                   (acp-refusal (refusal)
                     (acp-respond-error agent id (acp-refusal-code refusal) (acp-refusal-message refusal)
                                        (acp-refusal-data refusal)))))
               :deferred)
              (t (acp-refuse -32602 "a prompt is already running in session ~a" session.id)))))))

(defun acp-start-prompt (agent id session text images)
  "Start the turn request ID asks of SESSION with TEXT and IMAGES -- or, a
line naming a slash command, run it -- answered once it ends. => :DEFERRED."
  (when (and (null images) (acp-slash-command-p agent text))
    (return-from acp-start-prompt (acp-slash agent id session text)))
  (unless (wait-until (lambda () (member session.link '(:attached :closed))) (deadline-after *acp-reattach-seconds*) 0.05)
    (acp-refuse -32603 "the organism is not reachable"))
  (when (eq session.link :closed)
    (acp-refuse-unknown session.id "session ~a is closed" session.id))
  (let ((command (nlk:make-durable-id "command")))
    (bt2:with-lock-held (agent.lock)
      ;; Two prompts that waited on one cancel: the first to land runs.
      (when session.prompt
        (acp-refuse -32602 "a prompt is already running in session ~a" session.id))
      (setf session.prompt (list :id id :command command :turn nil :cancelled nil)
            session.usage nil)
      (push command session.commands))
    (handler-case (send-start-turn session.ws session.id text
                                   :command-id command :images images
                                   :target (unless session.begun session.pick))
      (error (condition)
        (bt2:with-lock-held (agent.lock) (setf session.prompt nil))
        (acp-refuse -32603 "the prompt could not be sent: ~a" condition)))
    :deferred))

(defun acp-slash (agent id session line)
  "Run LINE, a slash command, in SESSION as a channel runs it, its answer
the turn's text."
  (bt2:with-lock-held (agent.lock)
    (setf session.prompt (list :id id :command nil :turn :slash :cancelled nil)))
  (spawn "acp-slash"
    (handler-case
        (let ((answer (acp-http agent :post "/api/gateway/slash"
                                (json-object "line" line "session_id" session.id))))
          (acp-chunk agent session "agent_message_chunk" (or (json-value answer :string "text") ""))
          (when-let (prompt (bt2:with-lock-held (agent.lock)
                              (let ((prompt session.prompt))
                                (when (equal :slash (getf prompt :turn))
                                  (setf session.prompt nil)
                                  prompt))))
            (acp-respond agent id (json-object "stopReason" (if (getf prompt :cancelled) "cancelled" "end_turn")))))
      (acp-refusal (refusal)
        (bt2:with-lock-held (agent.lock) (setf session.prompt nil))
        (acp-respond-error agent id (acp-refusal-code refusal) (acp-refusal-message refusal)))))
  :deferred)

(defun acp-cancel (agent session)
  "Stop SESSION's prompt in flight: its turn is cancelled, and the prompt is
answered `cancelled' when the turn ends."
  (let ((turn (bt2:with-lock-held (agent.lock)
                (when-let (prompt session.prompt)
                  (setf (getf (acp-session-prompt session) :cancelled) t)
                  (getf prompt :turn)))))
    (when (and (stringp turn) session.ws)
      (ignore-errors (send-cancel-turn session.ws session.id turn)))))

(defun acp-set-config-option (agent params)
  "session/set_config_option: the session's model or reasoning effort -- a
session begun takes it through its routes, as a /models pick does; one not
yet begun on its first turn."
  (let* ((session (acp-session-of agent params))
         (config (json-value params :text "configId"))
         (value (json-value params :text "value")))
    (unless value (acp-refuse -32602 "value is required"))
    (cond
      ((equal config "model")
       (multiple-value-bind (provider model) (acp-split-model-value value)
         (if session.begun
             (acp-http agent :post (acp-session-path session.id "model")
                       (json-object "provider" provider "model" model))
             (setf session.pick (list :provider provider :model model
                                      :effort (getf session.pick :effort))))))
      ((equal config "effort")
       (let ((effort (if (equal value "default") nil value)))
         (if session.begun
             (acp-http agent :post (acp-session-path session.id "effort")
                       (json-object "effort" (or effort :null)))
             (setf (getf session.pick :effort) effort))))
      (t (acp-refuse -32602 "no config option ~s: model and effort are" config)))
    (json-object "configOptions" (acp-session-config agent session))))

(defun acp-answered (agent message)
  "The client's answer MESSAGE to a request of ours: a question's choice
goes to the organism."
  (let* ((id (gethash "id" message))
         (context (bt2:with-lock-held (agent.lock)
                    (prog1 (gethash id agent.asked) (remhash id agent.asked)))))
    (when (consp context)
      (destructuring-bind (session . qid) context
        (remhash qid session.questions)
        (let ((choice (and (equal "selected" (json-value message :string "result" "outcome" "outcome"))
                           (json-value message :text "result" "outcome" "optionId"))))
          (when (and choice session.ws)
            (ignore-errors
             (send-gateway-command session.ws :answer-question
                                   (json-object "session_id" session.id
                                                "question_id" qid
                                                "answer" choice)))))))))

(defun acp-dispatch (agent message)
  "One message from the client: a request answered, a notification acted on,
or an answer to a request of ours."
  (let ((method (json-value message :string "method"))
        (id (gethash "id" message))
        (params (or (json-value message :object "params") (make-hash-table :test #'equal))))
    (cond
      ((null method)
       (when id (acp-answered agent message)))
      ((null id)
       (cond ((equal method "session/cancel")
              (when-let (session (ignore-errors (acp-session-of agent params)))
                (acp-cancel agent session)))))
      (t
       (handler-case
           (multiple-value-bind (result after)
               (cond
                 ((equal method "initialize") (acp-initialize agent params))
                 ((not agent.initialized)
                  (acp-refuse -32600 "~a came before initialize: initialize the connection first" method))
                 ((equal method "authenticate") nil)
                 ((equal method "session/new") (acp-new-session agent params))
                 ((equal method "session/load") (acp-load-session agent id params :history t))
                 ((equal method "session/resume") (acp-load-session agent id params))
                 ((equal method "session/fork") (acp-fork-session agent params))
                 ((equal method "session/list") (acp-list-sessions agent params))
                 ((equal method "session/close") (acp-close-session agent params))
                 ((equal method "session/prompt") (acp-prompt agent id params))
                 ((equal method "session/set_config_option") (acp-set-config-option agent params))
                 (t (acp-refuse -32601 "~a is not a method this agent has" method)))
             (unless (eq result :deferred)
               (acp-respond agent id result)
               (when after (funcall after))))
         (acp-refusal (refusal)
           (acp-respond-error agent id (acp-refusal-code refusal) (acp-refusal-message refusal)
                              (acp-refusal-data refusal)))
         (error (condition)
           (acp-respond-error agent id -32603 (princ-to-string condition))))))))

(defun acp-read-line (agent line)
  "One line from the client: a message, or the error a line that is none answers."
  (let ((message (handler-case (nlk:decode-json line)
                   (error () (acp-respond-error agent nil -32700 "not JSON") nil))))
    (cond ((hash-table-p message) (acp-dispatch agent message))
          (message (acp-respond-error agent nil -32600 "a message is one JSON object")))))

;;; --- the process ------------------------------------------------------------------

(defun run-acp (&key port token find-port (input *standard-input*) (output *standard-output*))
  "Serve one ACP client on INPUT and OUTPUT against the gateway at PORT,
TOKEN its operator token, FIND-PORT how a link the gateway let go finds the
organism serving next (a function of a deadline). => :EOF once INPUT ends,
or why the agent could not go on."
  (let ((agent (make-acp-agent :output output :port port :token token :find-port find-port)))
    (spawn "acp-input"
      (loop for line = (handler-case (read-line input nil nil)
                         (error (condition) (acp-end agent (format nil "stdin: ~a" condition)) nil))
            while (and line (null agent.outcome))
            do (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
                 (when (plusp (length text))
                   (handler-case (acp-read-line agent text)
                     (error (condition)
                       (format *error-output* "nodecode acp: ~a~%" condition)))))
            finally (acp-end agent :eof)))
    ;; Parked for the agent's life, each link pinged every idle beat so the
    ;; gateway's read timeout never takes a quiet client for a dead one.
    (loop until (bt2:wait-on-semaphore agent.done :timeout *sync-keepalive-seconds*)
          do (dolist (session (bt2:with-lock-held (agent.lock)
                                (alexandria:hash-table-values agent.sessions)))
               (when (and session.ws (eq session.link :attached))
                 (ignore-errors (wsd:send-ping session.ws)))))
    (dolist (session (bt2:with-lock-held (agent.lock)
                       (alexandria:hash-table-values agent.sessions)))
      (acp-detach session))
    agent.outcome))

(defun acp-main (store-path)
  "`nodecode acp': an Agent Client Protocol agent on stdio for the organism
serving STORE-PATH, started here and fronted by this process when none does.
Exits 0 when stdin ends; signals why when the organism cannot be reached."
  ;; Runs before any folder loads, as `nodecode rpc' does: stdout is the
  ;; client's, so anything else this process would print goes to stderr.
  (let* ((path (merge-pathnames (or store-path nlk::*default-store-path*)))
         (output *standard-output*)
         (*standard-output* *error-output*)
         (outcome nil))
    (flet ((find-port (deadline) (store-organism-port path deadline)))
      (with-cleanup ((stop-front :why (and (eq outcome :eof) "quit")))
        (let ((port (or (find-port (deadline-after *rpc-attach-seconds*))
                        (error "no organism serves ~a" (namestring path)))))
          (setf outcome (run-acp :port port :token (gateway-configured-operator-token)
                                 :find-port #'find-port :output output)))))
    (unless (eq outcome :eof)
      (error "~a" outcome))
    (uiop:finish-outputs)
    (uiop:quit 0 nil)))
