;;;; config.lisp --- the mcp section of the shared config, typed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The contract is the retired Zig client's, verbatim:
;;;;
;;;;   "mcp": {
;;;;     "servers": {
;;;;       "files":  {"command": "mcp-files", "args": ["--root", "/tmp"],
;;;;                  "env": {"K": "v"}, "timeout_ms": 30000},
;;;;       "remote": {"url": "https://host/mcp", "headers": {"Authorization": "..."}},
;;;;       "off":    {"command": "x", "enabled": false}
;;;;     },
;;;;     "inherit_env_allowlist": ["PATH", "HOME", ...]
;;;;   }
;;;;
;;;; A url without a command is streamable HTTP, anything else is stdio; an
;;;; explicit "transport" ("stdio" | "http" | "streamable-http", or an object
;;;; with that "type") wins. No cwd, no ${VAR} expansion, no per-tool
;;;; allow/deny, no OAuth — as the Zig client had none. Header values are
;;;; secrets: nothing in this cell ever prints one; surfaces show a count.
;;;;
;;;; Each entry validates on its own: a refused entry becomes a spec carrying
;;;; the refusal text (the server shows as `refused` in every status) and its
;;;; siblings still run. The kernel's typed accessors (nlk:config-*) signal
;;;; NLK:CONFIG-REFUSAL, which is the one condition caught here.

(in-package #:nodecode-mcp)

(defparameter +default-timeout-ms+ 30000
  "Per-request budget when an entry names none (the Zig default_call_timeout_ms).")

(defparameter +default-inherit-env+
  '("PATH" "HOME" "USER" "LANG" "LC_ALL" "LC_CTYPE" "TMPDIR" "TMP" "TEMP"
    "SHELL" "TERM" "COLORTERM" "NO_COLOR" "TZ")
  "Environment a child server inherits when the section names no allowlist.")

;;; The serving image's own fd 2 is severed to a log while a TUI owns the
;;; terminal, so a child must never inherit it; a file the operator can name
;;; beats /dev/null when a server dies at startup.
(nlk:define-startup-parameter *log-directory*
  (nlk:home "mcp/")
  "Where a stdio server's stderr lands, one append-only file per server.")

(defstruct (server-spec (:copier nil))
  "One validated mcp.servers entry."
  (name "" :type string)
  (transport :stdio :type keyword)    ; :stdio | :http
  (command nil)                       ; string, stdio
  (args '() :type list)               ; strings, stdio
  (env '() :type list)                ; ((name . value) ...), stdio
  (url nil)                           ; string, http
  (headers '() :type list)            ; ((name . value) ...), http
  (timeout-ms +default-timeout-ms+ :type integer)
  (enabled-p t)
  (refusal nil))                      ; config-refusal text, or NIL

(nlk:access (spec server-spec))

(defun string-alist (section key)
  "KEY's object as ((name . value) ...) with string values, or '() when absent."
  (multiple-value-bind (value present) (nlk::section-value section key)
    (cond ((not present) '())
          ((hash-table-p value)
           (sort (loop for name being the hash-keys of value using (hash-value entry)
                       do (unless (stringp entry)
                            (nlk:config-error "~a.~a must be a string, got ~s" key name entry))
                       collect (cons name entry))
                 #'string< :key #'car))
          (t (nlk:config-error "~a must be an object, got ~s" key value)))))

(defun parse-server-entry (name entry)
  "A SERVER-SPEC for ENTRY, or one carrying the refusal text."
  (nlk:with-handlers ((nlk:config-refusal (condition)
                        (make-server-spec :name (if (stringp name) name (princ-to-string name))
                                          :enabled-p nil
                                          :refusal (princ-to-string condition))))
    ;; [A-Za-z0-9_-]+: what a generated symbol and a log file name can carry.
    (unless (and (stringp name)
                 (plusp (length name))
                 (every (lambda (char) (or (alphanumericp char) (find char "_-"))) name))
      (nlk:config-error "server name ~s: only letters, digits, _ and -" name))
    (unless (hash-table-p entry)
      (nlk:config-error "server ~a must be an object" name))
    (let* ((command (nlk:config-string entry "command"))
           (url (nlk:config-string entry "url"))
           ;; An explicit transport (a string, or an object with that type) wins.
           (transport (multiple-value-bind (value present) (nlk::section-value entry "transport")
                        (if (not present)
                            (if (and url (null command)) :http :stdio)
                            (let ((type (cond ((stringp value) value)
                                              ((hash-table-p value) (nlk:config-string value "type"))
                                              (t (nlk:config-error
                                                  "transport must be a string or an object, got ~s"
                                                  value)))))
                              (cond ((member type '("stdio") :test #'string-equal) :stdio)
                                    ((member type '("http" "streamable-http" "streamable_http")
                                             :test #'string-equal)
                                     :http)
                                    (t (nlk:config-error
                                        "transport must be stdio or http, got ~s" type))))))))
      (ecase transport
        (:stdio
         (unless command
           (nlk:config-error "server ~a: stdio needs a command" name)))
        (:http
         (unless (and url (or (uiop:string-prefix-p "http://" url)
                              (uiop:string-prefix-p "https://" url)))
           (nlk:config-error "server ~a: http needs an http(s) url" name))))
      (make-server-spec
       :name name
       :transport transport
       :command command
       :args (nlk:config-string-list entry "args")
       :env (string-alist entry "env")
       :url url
       :headers (string-alist entry "headers")
       :timeout-ms (let ((value (nlk:config-integer entry "timeout_ms" 0)))
                     (cond ((zerop value) +default-timeout-ms+)
                           ((minusp value)
                            (nlk:config-error "timeout_ms must be positive, got ~a" value))
                           (t value)))
       :enabled-p (nlk:config-boolean entry "enabled" t)))))

(defun parse-mcp-section (section)
  "(values SPECS ALLOWLIST): the servers of SECTION in name order, each its
own validation, and the env allowlist."
  ;; A missing or non-object section is no servers at all.
  (if (hash-table-p section)
      (let ((servers (gethash "servers" section))
            (allowlist
              (if (nth-value 1 (nlk::section-value section "inherit_env_allowlist"))
                  (handler-case
                      (nlk:config-string-list section "inherit_env_allowlist")
                    (nlk:config-refusal (condition)
                      (warn "mcp: ~a; using the default allowlist" condition)
                      +default-inherit-env+))
                  +default-inherit-env+)))
        (values
         (when (hash-table-p servers)
           (mapcar (lambda (name) (parse-server-entry name (gethash name servers)))
                   (sort (loop for name being the hash-keys of servers collect name)
                         #'string<)))
         allowlist))
      (values '() +default-inherit-env+)))

(defun child-environment (spec allowlist &aux (env spec.env))
  "The child's whole environment as \"NAME=VALUE\" strings: the allowlisted
variables that exist here (exported shell functions, the \"() {\" values,
skipped as the reference SDK skips them), then the entry's env on top."
  (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair)))
          (append (loop for name in allowlist
                        for value = (uiop:getenv name)
                        when (and value (not (uiop:string-prefix-p "() {" value))
                                  (not (assoc name env :test #'string=)))
                          collect (cons name value))
                  env)))

(defun stderr-log-path (name)
  (merge-pathnames (format nil "~a.stderr.log" name) *log-directory*))

(defun transport-text (spec)
  "The transport for a status line: the command or the url, a header count
and never a header value."
  (ecase spec.transport
    (:stdio (format nil "stdio ~a~{ ~a~}"
                    spec.command spec.args))
    (:http (let ((count (length spec.headers)))
             (format nil "http ~a~@[ (~d header~:p)~]" spec.url (and (plusp count) count))))))
