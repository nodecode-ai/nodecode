;;;; cell.lisp --- START-CELL, the /mcp slash command and the page's route.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell entry the loader finds by name: serve /api/mcp, read the `mcp'
;;;; section once, build the registry, start one connect thread per server,
;;;; install the primer advice, register /mcp, return the stop thunk. Config
;;;; is materialized here and closed over; a change the page makes is written
;;;; to config.jsonc and read by the next start.
;;;;
;;;; Slash output: every subcommand answers one line, and none of them
;;;; blocks: the command runs on an HTTP worker of the gateway, so a restart
;;;; is spawned, never awaited, and status reads snapshots. The long-form
;;;; reports are model-facing ((mcp:status), (mcp:tools) via eval). Stop
;;;; takes /mcp back out of the catalog.

(in-package #:nodecode-mcp)

(defparameter +usage+ "/mcp [status | on NAME | off NAME | restart NAME | tools NAME]")

;;; --- subcommands: each answers its text -----------------------------------

(defun do-status (&aux (snapshots (mapcar #'server-snapshot (registry-servers *registry*))))
  (if (null snapshots)
      "MCP: no servers configured (mcp.servers in ~/.nodecode/config.jsonc)"
      (let* ((states (mapcar (lambda (snapshot) (getf snapshot :state)) snapshots))
             (ready (count :ready states))
             (errors (count-if (lambda (state) (member state '(:error :refused))) states)))
        ;; What the operator can do next leads, and names only the verbs the
        ;; states call for: five imported servers pushed `/mcp on NAME' past
        ;; 120 columns.
        (format nil "MCP: ~@[/mcp ~{~a NAME~^ | ~} -- ~]~{~a~^; ~}"
                        (remove nil (list (and (member :disabled states) "on")
                                          (and (intersection '(:error :connecting :disconnected) states)
                                               "restart")
                                          (and (plusp ready) "tools")))
                        ;; `files ready 12 tools', `docs error (exit 1, see ...)', `pg off'.
                        (mapcar (lambda (snapshot &aux (state (getf snapshot :state)))
                                  (format nil "~a ~(~a~)~@[ ~d tool~:p~]~@[ (~a)~]"
                                          (getf snapshot :name) (if (eq state :disabled) "off" state)
                                          (and (eq state :ready) (getf snapshot :tool-count))
                                          (and (member state '(:error :refused))
                                               (getf snapshot :error)
                                               (nlk:one-line (getf snapshot :error) :cap 160))))
                                snapshots)))))

(defun do-tools (name)
  (destructuring-bind (&key state tools error &allow-other-keys) (server-snapshot (find-server name))
    (if (eq state :ready)
        (nlk:one-line
         (format nil "MCP: ~a (~d tool~:p)~@[: ~{~a~^, ~}~]"
                 name (length tools)
                 (mapcar (lambda (tool) (getf tool :name)) tools))
         :cap 200)
        (if (eq state :disabled)
            (format nil "MCP: ~a is off -- /mcp on ~a starts it" name name)
            (format nil "MCP: ~a is ~(~a~)~@[ (~a)~]"
                    name state (and error (nlk:one-line error :cap 120)))))))

(defun register-mcp-command ()
  (nle:register-command
   "nodecode-mcp" "mcp"
   (lambda (args session-id &aux (tokens (nlk:split-words args))
                                 (head (string-downcase (or (first tokens) "")))
                                 (rest (format nil "~{~a~^ ~}" (rest tokens))))
     (declare (ignore session-id))
     (cond
       ((null *registry*) "MCP: cell is not running")
       ((member head '("" "status") :test #'string=) (do-status))
       ((not (member head '("on" "off" "restart" "tools") :test #'string=))
        (format nil "MCP: unknown subcommand ~a; usage ~a" head +usage+))
       ((zerop (length rest))
        (format nil "MCP: usage ~a" +usage+))
       (t
        ;; Both name a server: an unknown one answers with the configured names.
        (nlk:with-handlers ((mcp-unknown-server ()
                              (format nil "MCP: no server named ~a; configured: ~{~a~^, ~}"
                                      rest (or (registry-server-names) '("none"))))
                            (mcp-error (condition)
                              (format nil "MCP: ~a" condition)))
          (cond ((string= head "restart")
                 (restart rest)
                 (format nil "MCP: restarting ~a; /mcp status to follow" rest))
                ((string= head "tools") (do-tools rest))
                (t (format nil "MCP: ~a" (switch-server rest (string= head "on")))))))))
   :label "MCP"
   :description "MCP servers: status, on NAME, off NAME, restart NAME, tools NAME"
   :argument-hint "status | on NAME | off NAME | restart NAME | tools NAME"))

;;; --- the page ---------------------------------------------------------------
;;; The web page's Control pane reads and moves the servers through this
;;; route, served while the cell runs whether or not a server is set: the
;;; page adds the first one here. Add and remove write the member under
;;; mcp.servers through the one config writer (NLE:CONFIG-SET) and start this
;;; folder again, which is how a changed section takes effect; restart is
;;; RESTART, one server's own reconnect; test is TEST-SERVER on a configured
;;; one and PROBE on the entry the Add form holds, before it is saved. The
;;; catalog is catalog.json beside this file: well-known servers, each checked
;;; against its project's own documentation, which the page offers to add.

(defparameter +route+ "/api/mcp")
(defparameter +catalog-route+ "/api/mcp/catalog")

(defun server-json (snapshot)
  "A server's SNAPSHOT as the page lists it: its state, its tools, how it is
reached (a header count, never a value), how long its last connect took, its
last error and its log."
  (destructuring-bind (&key name state error tool-count tools transport log-path timing &allow-other-keys)
      snapshot
    (nlk:json-object "name" name
                     "state" (string-downcase state)
                     "tools" tool-count
                     "tool_names" (map 'vector (lambda (tool) (getf tool :name)) tools)
                     "transport" transport
                     :opt "connect_ms" (first timing)
                     :opt "list_ms" (second timing)
                     :opt "error" (and error (nlk:one-line error :cap 400))
                     :opt "log" log-path)))

(defun test-json (name outcome)
  "A test's OUTCOME (TEST-SERVER, PROBE) for the server NAME as the page
shows it: the tools with what each does and the two times, or the failure and
what the server said."
  (destructuring-bind (&key ok reused connect-ms list-ms tools server-info error said) outcome
    (nlk:json-object "name" name
                     "ok" (and ok t)
                     :opt "reused" (and reused t)
                     :opt "connect_ms" connect-ms
                     :opt "list_ms" list-ms
                     :opt "server" (and server-info
                                        (format nil "~@[~a~]~@[ ~a~]"
                                                (nlk:json-value server-info :string "name")
                                                (nlk:json-value server-info :string "version")))
                     :opt "tools" (and ok (map 'vector (lambda (tool)
                                                         (nlk:json-object "name" (getf tool :name)
                                                                          "description" (getf tool :description)))
                                               tools))
                     :opt "error" (and error (nlk:one-line error :cap 600))
                     :opt "said" said)))

(defun configured-server (name)
  "The mcp.servers entry NAME as config.jsonc says it now, or NIL."
  (nlk:json-value (nle:read-shared-config) :object "mcp" "servers" name))

(defun restart-folder ()
  "Start this folder again on config.jsonc as it is now."
  (nle:restart-cells (nlk:cell-name (nlk:system-cell "nodecode-mcp"))))

(defun request-entry (request)
  "The mcp.servers entry a page REQUEST describes: its `url', with the
`headers' it sends, when it names one; else its `command', with its `args'
and the `env' the process gets."
  (flet ((member-of (key type) (nlk:json-value request type key)))
    (if (member-of "url" :string)
        (nlk:json-object "url" (member-of "url" :string)
                         :opt "headers" (member-of "headers" :object))
        (nlk:json-object "command" (member-of "command" :string)
                         :opt "args" (let ((args (member-of "args" :array))) (and args (plusp (length args)) args))
                         :opt "env" (member-of "env" :object)))))

(defun checked-spec (name entry &aux (spec (parse-server-entry name entry)))
  "ENTRY for the server NAME, validated as START-CELL will read it: its
spec, or the refusal."
  (when spec.refusal (fail "~a" spec.refusal))
  spec)

(defun add-server (name entry)
  "Write ENTRY under mcp.servers as the server NAME, validated as
START-CELL will read it, then start this folder again. => what came of it."
  (checked-spec name entry)
  (when (configured-server name)
    (fail "a server named ~a is set already; remove it first" name))
  (nle:config-set (list "mcp" "servers" name) entry)
  (restart-folder)
  (format nil "~a added~:[, but the mcp section says enabled: false, so no server starts~;; it connects now~]"
          name *registry*))

(defun switch-server (name on)
  "Turn the server NAME ON or off -- its `enabled' member in config.jsonc --
and start this folder again on the file, off the caller's thread. => what
came of it."
  ;; The way a server an import brought over, which lands off, is started
  ;; without editing config.jsonc by hand (vr-116).
  (unless (configured-server name)
    (fail "no server named ~a under mcp.servers" name))
  (nle:config-set (list "mcp" "servers" name "enabled") (if on t :false))
  (nlk:spawn "mcp-switch" (restart-folder))
  (format nil "~a ~:[off~;on; it connects now, /mcp status to follow~]" name on))

(defun remove-server (name)
  "Take the server NAME out of mcp.servers and start this folder again."
  (unless (configured-server name)
    (fail "no server named ~a under mcp.servers" name))
  (nle:config-set (list "mcp" "servers" name) :remove)
  (restart-folder)
  (format nil "~a removed" name))

(defun servers-route (env)
  "/api/mcp: GET answers {enabled, servers}, every running server as its
snapshot says it. POST takes a JSON body naming an op: {op: add, name,
command, args?, env?} or {op: add, name, url, headers?} adds one, {op:
remove, name} and {op: restart, name} move one, and each answers {text,
enabled, servers}; {op: test, name} tests the configured server NAME, and
{op: test, name, command|url ...} the entry the Add form holds, unsaved,
answering {test, enabled, servers}."
  (let* ((request (if (eq (getf env :request-method) :post)
                      (nle::gateway-request-json env)
                      (nlk:make-json-object)))
         (op (nlk:json-value request :string "op"))
         (name (nlk:json-value request :string "name"))
         (unsaved (or (nlk:json-value request :string "command") (nlk:json-value request :string "url")))
         (test (and (equal op "test")
                    (test-json name (if unsaved
                                        (probe (checked-spec name (request-entry request)))
                                        (test-server (find-server name))))))
         (text (cond ((or (null op) test) nil)
                     ((string= op "add") (add-server name (request-entry request)))
                     ((string= op "remove") (remove-server name))
                     ((string= op "restart") (restart name))
                     (t (fail "no op ~s; add, remove, restart or test" op))))
         (section (nlk:json-value (nle:read-shared-config) :object "mcp")))
    (nlk:json-object :opt "text" text
                     :opt "test" test
                     "enabled" (or (null section) (nlk:config-boolean section "enabled" t))
                     "servers" (map 'vector (lambda (server) (server-json (server-snapshot server)))
                                    (and *registry* (registry-servers *registry*))))))

(defun catalog ()
  "The servers catalog.json lists, each marked `added' when a server of its
name is set under mcp.servers."
  (let ((servers (nlk:json-value (nlk:decode-json (uiop:read-file-string
                                                   (asdf:system-relative-pathname "nodecode-mcp" "catalog.json")))
                                 :array "servers"))
        (configured (nlk:json-value (nle:read-shared-config) :object "mcp" "servers")))
    (loop for entry across servers
          do (setf (gethash "added" entry)
                   (and configured (nth-value 1 (gethash (gethash "name" entry) configured)) t)))
    servers))

(defun catalog-route (env)
  "/api/mcp/catalog: GET answers {servers}, the catalog (CATALOG)."
  (declare (ignore env))
  (nlk:json-object "servers" (catalog)))

;;; --- the entry ------------------------------------------------------------

(defun stop-cell (&aux (registry *registry*))
  "Undo START-CELL: the page's route, /mcp, every connection, the generated
functions, the manual. Idempotent."
  (nle:route +route+ nil)
  (nle:unregister-commands "nodecode-mcp")
  (nle:route +catalog-route+ nil)
  (when registry
    (setf *registry* nil)
    ;; Every server at once, each on a thread of its own and with a quarter
    ;; second of grace for stdin and then for SIGTERM before the kill; never
    ;; destroys a thread.
    (setf registry.stopping-p t)
    (dolist (closer (loop for server in registry.servers
                          collect (let ((server server))
                                    (nlk:spawn (format nil "mcp-close-~a" (server-name server))
                                      ;; Closed under its lock when that comes in time, else
                                      ;; from outside: a close is thread-safe.
                                      (let ((*close-grace-seconds* 1/4))
                                        (unless (bt2:with-lock-held (server.call-lock
                                                                     :timeout *stop-lock-seconds*)
                                                  (close-client server)
                                                  t)
                                          (warn "mcp ~a: busy at stop; closing its connection from outside"
                                                (server-name server))
                                          (close-client server)))))))
      (ignore-errors (bt2:join-thread closer)))
    (dolist (server registry.servers)
      (unless (member server.state '(:disabled :refused))
        (set-state server :stopped))
      (undefine-tool-functions server)))
  (refresh-primer)
  t)

(defun start-cell (config &aux (section (and (hash-table-p config) (gethash "mcp" config))))
  "Serve the page's route, connect every server the `mcp' section names and
register /mcp; the stop thunk takes it all down."
  ;; No servers, or enabled: false, connects nothing: the route alone, where
  ;; the page adds the first server.
  (stop-cell)
  (nle:route +route+ #'servers-route)
  (nle:route +catalog-route+ #'catalog-route)
  (when (and section (nlk:config-boolean section "enabled" t))
    (let ((registry (make-registry-from-section section)))
      (when registry.servers
        (setf *registry* registry)
        ;; (help :mcp), and its line on every request
        (refresh-primer)
        (register-mcp-command)
        (dolist (server registry.servers)
          (spawn-connect server)))))
  (lambda () (stop-cell)))
