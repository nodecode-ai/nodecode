;;;; route-test.lisp --- /api/mcp: the servers as the web page lists them, one
;;;; added, tested, restarted and removed; a server tested before it is added;
;;;; and the catalog the page offers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The folder is started the way the gateway starts it, from a record on the
;;;; report and a scratch config.jsonc holding no server at all: the route is
;;;; served anyway, since the page adds the first server there. The server is
;;;; the stdio fixture beside this file.

(in-package #:nodecode.test)

(defun mcp-route-message (body)
  "The refusal text of a route answer BODY."
  (nlk:json-value body :string "error" "message"))

(defun mcp-configured (name)
  "The mcp.servers entry NAME as the scratch config.jsonc says it now."
  (nlk:json-value (nle:read-shared-config) :object "mcp" "servers" name))

(defun mcp-op (&rest members)
  "A POST body for /api/mcp: the JSON object MEMBERS (key value ...) make."
  (nlk:encode-json-object (apply #'nlk:make-json-object members)))

(defun mcp-fixture-path ()
  (namestring (asdf:system-relative-pathname "nodecode-mcp" "test/fixture-server.py")))

(defun mcp-processes-holding (marker)
  "The pids of the processes whose command line holds MARKER."
  (loop for directory in (ignore-errors (uiop:subdirectories #p"/proc/"))
        for pid = (parse-integer (nlk:folder-name directory) :junk-allowed t)
        for line = (and pid (ignore-errors
                             (with-open-file (in (merge-pathnames "cmdline" directory)
                                                 :element-type '(unsigned-byte 8))
                               (let ((octets (make-array 8192 :element-type '(unsigned-byte 8))))
                                 (sb-ext:octets-to-string (subseq octets 0 (read-sequence octets in))
                                                          :external-format :latin-1)))))
        when (and line (search marker line)) collect pid))

(defmacro with-mcp-route ((port) &body body)
  "BODY over a gateway on PORT serving /api/mcp, the folder started as the
gateway starts it on a scratch config.jsonc with no server; stopped on unwind."
  `(with-temp-gateway (,port)
     (with-mcp-log-dir
       (with-temp-file (config :type "jsonc" :contents "{}")
         (with-saved-globals ((nle::*shared-config-path* config)
                              (nlk::*cells* (list (nlk::make-cell :name "nodecode-mcp" :kind :peripheral
                                                                    :systems '("nodecode-mcp")
                                                                    :start #'mcp:start-cell))))
           (nle::start-cells (list "nodecode-mcp"))
           (unwind-protect (progn ,@body)
             (nle::stop-cells)))))))

(deftest mcp-cell-route-adds-restarts-and-removes-a-server (with-mcp-route (port))
  (let ((fixture (mcp-fixture-path)))
    (with-gateway-http (port :get "/api/mcp")
      (is (= 200 status) "served with no server set")
      (is (eq t (nlk:json-value body :any "enabled")))
      (is (equalp #() (nlk:json-value body :array "servers"))))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "add" "name" "probe" "command" "python3"
                                                           "args" (vector fixture)))
      (is (= 200 status))
      (is (search "probe added; it connects now" (nlk:json-value body :string "text"))))
    (let ((entry (mcp-configured "probe")))
      (is (equal "python3" (nlk:json-value entry :string "command")) "written under mcp.servers")
      (is (equalp (vector fixture) (nlk:json-value entry :array "args"))))
    (is (eq :ready (mcp-wait-ready "probe")) "and it connects")
    (with-gateway-http (port :get "/api/mcp")
      (let ((row (find "probe" (nlk:json-value body :array "servers")
                       :key (lambda (row) (nlk:json-value row :string "name")) :test #'equal)))
        (is-present row "the server"
          (is (equal "ready" (nlk:json-value row :string "state")))
          (is (= 10 (nlk:json-value row :integer "tools")) "its tool count")
          (is (search "stdio python3" (nlk:json-value row :string "transport")))
          (is (integerp (nlk:json-value row :any "connect_ms")) "how long its connect took")
          (is (search "probe.stderr.log" (nlk:json-value row :string "log")) "and where its errors go"))))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "add" "name" "probe" "command" "x"))
      (is (= 500 status))
      (is (search "set already" (mcp-route-message body)) "a name taken is refused"))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "add" "name" "bad name" "command" "x"))
      (is (search "only letters, digits" (mcp-route-message body)) "a name a symbol cannot carry"))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "add" "name" "remote" "url" "ftp://host"))
      (is (search "http(s) url" (mcp-route-message body)) "a URL that is not http"))
    (is (null (mcp-configured "remote")) "a refused one is never written")
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "restart" "name" "probe"))
      (is (search "restarting probe" (nlk:json-value body :string "text"))))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "remove" "name" "probe"))
      (is (= 200 status))
      (is (equalp #() (nlk:json-value body :array "servers")) "gone from the list"))
    (is (null (mcp-configured "probe")) "and from mcp.servers")))

(deftest mcp-cell-mcp-test-lists-tools-with-timings-and-says-a-failure (with-mcp-route (port))
  (let ((fixture (mcp-fixture-path))
        (marker (format nil "--probe-~36r" (random (expt 36 8)))))
    ;; A server not added yet, tested from the Add form's fields: its own
    ;; connection, closed before the answer.
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "test" "name" "draft" "command" "python3"
                                                           "args" (vector fixture marker)))
      (is (= 200 status))
      (is-present (test (nlk:json-value body :object "test")) "a test answer"
        (is (nlk:json-value test :any "ok") "the server answered")
        (is (not (nlk:json-value test :any "reused")) "on a connection of its own")
        (is (integerp (nlk:json-value test :any "connect_ms")) "how long the connect took")
        (is (integerp (nlk:json-value test :any "list_ms")) "and the list")
        (is (search "fixture" (nlk:json-value test :string "server")) "the server's own name")
        (let ((echo (find "echo" (nlk:json-value test :array "tools")
                          :key (lambda (tool) (nlk:json-value tool :string "name")) :test #'equal)))
          (is (= 10 (length (nlk:json-value test :array "tools"))) "every tool")
          (is (plusp (length (nlk:json-value echo :string "description"))) "each with what it does")))
      (is (equalp #() (nlk:json-value body :array "servers")) "and nothing was added"))
    (is (null (mcp-configured "draft")) "nothing written")
    (is (null (mcp-processes-holding marker)) "and no process left behind")
    ;; A command that dies at start: the failure, and what it said on its stderr.
    (with-gateway-http (port :post "/api/mcp"
                        :body (mcp-op "op" "test" "name" "broken" "command" "python3"
                                      "args" (vector "-c" "import sys; sys.stderr.write('fixture: no key given\\n'); sys.exit(3)")))
      (is (= 200 status))
      (let ((test (nlk:json-value body :object "test")))
        (is (not (nlk:json-value test :any "ok")) "a failure is an answer, not a refusal")
        (is (plusp (length (nlk:json-value test :string "error"))) "the failure as the client met it")
        (is (search "fixture: no key given" (nlk:json-value test :string "said")) "and the server's own words")))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "test" "name" "bad name" "command" "x"))
      (is (= 500 status))
      (is (search "only letters, digits" (mcp-route-message body)) "an entry refused as an add refuses it"))
    ;; A configured server: its live connection, reused.
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "add" "name" "probe" "command" "python3"
                                                           "args" (vector fixture)))
      (is (= 200 status)))
    (is (eq :ready (mcp-wait-ready "probe")))
    (let ((client (mcp::server-client (mcp-server "probe"))))
      (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "test" "name" "probe"))
        (let ((test (nlk:json-value body :object "test")))
          (is (nlk:json-value test :any "ok"))
          (is (nlk:json-value test :any "reused") "on the connection the model's calls use")
          (is (integerp (nlk:json-value test :any "list_ms")) "listed again, timed")
          (is (= 10 (length (nlk:json-value test :array "tools"))))))
      (is (eq client (mcp::server-client (mcp-server "probe"))) "the same connection, kept"))
    (with-gateway-http (port :post "/api/mcp" :body (mcp-op "op" "test" "name" "ghost"))
      (is (search "no MCP server named" (mcp-route-message body)) "an unknown server refuses"))))

(deftest mcp-cell-mcp-test-probe-keeps-its-deadline-and-leaves-no-process ()
  (with-mcp-log-dir
    (let* ((marker (format nil "--probe-~36r" (random (expt 36 8))))
           (entry (mcp-fixture-entry "--slow-init" marker)))
      (setf (gethash "timeout_ms" entry) 500)
      (let ((outcome (mcp::probe (mcp::parse-server-entry "slow" entry))))
        (is (not (getf outcome :ok)) "a slow initialize fails the test")
        (is (search "initialize" (getf outcome :error)) "at the handshake, named")
        (is (null (mcp-processes-holding marker)) "and the child is gone")))))

(deftest mcp-cell-mcp-catalog-file-names-servers-this-client-can-reach ()
  (let ((servers (nlk:json-value (nlk:decode-json (uiop:read-file-string
                                                   (asdf:system-relative-pathname "nodecode-mcp" "catalog.json")))
                                 :array "servers"))
        (names '()))
    (is (<= 10 (length servers)) "a catalog worth offering")
    (loop for server across servers
          for name = (nlk:json-value server :string "name")
          for command = (nlk:json-value server :string "command")
          for url = (nlk:json-value server :string "url")
          ;; What the page's Add writes: every key it asks for answered.
          for entry = (if url
                          (nlk:json-object "url" url
                                           :opt "headers" (let ((headers (nlk:make-json-object)))
                                                            (loop for header across (or (nlk:json-value server :array "headers") #())
                                                                  do (setf (gethash (nlk:json-value header :string "name") headers)
                                                                           (format nil "~@[~a~]k" (nlk:json-value header :string "prefix"))))
                                                            (and (plusp (hash-table-count headers)) headers)))
                          (nlk:json-object "command" command
                                           "args" (nlk:json-value server :array "args")
                                           :opt "env" (let ((env (nlk:make-json-object)))
                                                        (loop for key across (or (nlk:json-value server :array "env") #())
                                                              do (setf (gethash (nlk:json-value key :string "name") env) "k"))
                                                        (and (plusp (hash-table-count env)) env))))
          for what = (or (nlk:json-value server :string "what") "")
          for keys = (concatenate 'vector (or (nlk:json-value server :array "env") #())
                                  (or (nlk:json-value server :array "headers") #()))
          for refusal = (mcp::server-spec-refusal (mcp::parse-server-entry name entry))
          do (is (not (member name names :test #'equal)) "each name once")
             (push name names)
             (is (<= 1 (length what) 80) "what it does, in one short line")
             (is (not (and command url)) "a command or a URL, not both")
             (is (or command (uiop:string-prefix-p "https://" url)) "a URL over https")
             (is (every #'stringp (nlk:json-value server :array "args")) "arguments are strings")
             (is (uiop:string-prefix-p "https://" (nlk:json-value server :string "homepage")) "a homepage")
             (is (plusp (length (nlk:json-value server :string "source"))) "where its recipe was read")
             (is (every (lambda (key) (and (nlk:json-value key :string "name") (nlk:json-value key :string "ask"))) keys) "every key named and asked for")
             (is (null refusal) "an entry START-CELL reads as it stands"))))

(deftest mcp-cell-mcp-catalog-route-marks-a-server-added (with-mcp-route (port))
  (with-gateway-http (port :get "/api/mcp/catalog")
    (is (= 200 status))
    (let ((deepwiki (find "deepwiki" (nlk:json-value body :array "servers")
                          :key (lambda (row) (nlk:json-value row :string "name")) :test #'equal)))
      (is (equal "https://mcp.deepwiki.com/mcp" (nlk:json-value deepwiki :string "url")))
      (is (not (nlk:json-value deepwiki :any "added")) "not set yet")))
  (nle:config-set '("mcp" "servers" "deepwiki") (nlk:json-object "url" "https://mcp.deepwiki.com/mcp" "enabled" :false))
  (with-gateway-http (port :get "/api/mcp/catalog")
    (let ((deepwiki (find "deepwiki" (nlk:json-value body :array "servers")
                          :key (lambda (row) (nlk:json-value row :string "name")) :test #'equal)))
      (is (nlk:json-value deepwiki :any "added") "added once a server of its name is set"))))

(defun mcp-state-soon (name state)
  "Whether the server NAME, in whichever registry runs now, reaches STATE
within ten seconds."
  (await (:timeout 10) (eq state (ignore-errors (mcp::server-state (mcp-server name))))))

(deftest mcp-cell-slash-on-and-off-switch-a-server (with-mcp-route (port))
  ;; vr-116: an import lands its servers off, and /mcp on NAME is how one
  ;; starts without editing config.jsonc by hand; /mcp off NAME stops one the
  ;; same way. Each is the `enabled' member, written, then the folder started
  ;; again on the file.
  (nle:config-set '("mcp" "servers" "probe")
                  (mcp-entry "command" "python3" "args" (vector (mcp-fixture-path)) "enabled" :false))
  (nle::restart-cells "nodecode-mcp")
  (is (mcp-state-soon "probe" :disabled) "it lands off")
  (is (equal "MCP: /mcp on NAME -- probe off" (mcp-slash "")) "the status says how to start it")
  (let ((line (mcp-slash "on probe")))
    (is (equal "MCP: probe on; it connects now, /mcp status to follow" line)))
  (is (nlk:config-boolean (mcp-configured "probe") "enabled" nil) "written to config.jsonc")
  (is (mcp-state-soon "probe" :ready) "and it connects")
  (is (equal "MCP: probe off" (mcp-slash "off probe")))
  (is (not (nlk:config-boolean (mcp-configured "probe") "enabled" t)))
  (is (mcp-state-soon "probe" :disabled) "off again")
  (let ((line (mcp-slash "on ghost")))
    (is (search "no server named ghost" line) "a name not under mcp.servers"))
  ;; no switch is left running
  (is (await (:timeout 10) (notany (lambda (thread) (equal "mcp-switch" (bt2:thread-name thread)))
                                   (bt2:all-threads)))))
