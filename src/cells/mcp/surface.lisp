;;;; surface.lisp --- the model-facing vocabulary: CALL and its siblings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mono-tool: the model reaches an MCP server from EVAL as plain
;;;; functions, so this file plus the generated per-tool functions
;;;; (wrappers.lisp) is the whole "tool schema", and the harness primer
;;;; (primer.lisp) is its documentation. Every public function returns a
;;;; STRING: the eval snippet prints the value with ~S, and a hash table would
;;;; print as #<HASH-TABLE>.
;;;;
;;;; Arguments: a keyword names a property of the tool's inputSchema by
;;;; folding case and the -/_ separators (:per-page names per_page,
;;;; :include-snapshot names includeSnapshot), a string names it verbatim.
;;;; Values cross as JSON: strings and numbers as they are, T true, :FALSE
;;;; false, NIL absent, a list or vector an array, a keyword plist a nested
;;;; object. :TIMEOUT (seconds) and :LIMIT (characters) are consumed here.
;;;;
;;;; Results: the content blocks render as the retired Zig client rendered
;;;; them — text verbatim, one line per non-text block naming what came
;;;; back — and are clipped under the 8000-character eval clamp with the
;;;; cut disclosed. isError is the tool's own refusal and arrives as
;;;; MCP-TOOL-ERROR so the eval snippet prints ERROR: and the text.

(in-package #:nodecode-mcp)

(declaim (ftype function tool-function-name))

(defparameter *result-limit* 7000
  "Default character cap on a rendered result — under the 8000 EVAL
clamp so the disclosure survives.")

(defparameter *max-timeout-seconds* 600
  "Ceiling on a caller's :timeout; the eval snippet backgrounds a form at 10 s
unless the model raises yield_time_ms, so a longer wait must be deliberate.")

;;; --- names ----------------------------------------------------------------

(defun fold-name (text)
  "TEXT lowercased with the -/_ separators dropped: the comparison key."
  (remove-if (lambda (char) (find char "-_")) (string-downcase text)))

(defun kebab (text)
  "read_file -> read-file, searchIssues -> search-issues, Foo.Bar -> foo-bar."
  ;; One dash for every run of non-alphanumerics and at every lower-or-digit
  ;; to upper step, none at either end.
  (string-downcase
   (string-trim "-" (ppcre:regex-replace-all
                     '(:alternation
                       (:greedy-repetition 1 nil (:inverted-char-class (:property alpha-char-p)
                                                                       (:property digit-char-p)))
                       (:sequence (:positive-lookbehind (:char-class (:property lower-case-p)
                                                                     (:property digit-char-p)))
                                  (:positive-lookahead (:property upper-case-p))))
                     text "-"))))

(defun schema-properties (schema)
  "The property names of SCHEMA, in the server's order."
  (nlk:when-let (properties (nlk:json-value schema :object "properties"))
    (loop for name being the hash-keys of properties collect name)))

(defun schema-required (schema)
  (nlk:when-let (required (nlk:json-value schema :array "required"))
    (loop for name across required when (stringp name) collect name)))

(defun resolve-key (key schema)
  "The property name KEY addresses: a string verbatim; a keyword by exact
snake_case spelling first, then by fold, then as-is with - for _."
  (cond
    ((stringp key) key)
    ((keywordp key)
     (let* ((plain (substitute #\_ #\- (string-downcase (symbol-name key))))
            (names (schema-properties schema))
            (exact (find plain names :test #'string=))
            (matches (remove-if-not (lambda (name) (string= (fold-name name) (fold-name plain))) names)))
       (cond (exact exact)
             ((null matches) plain)
             ((null (rest matches)) (first matches))
             (t (fail "~s is ambiguous between ~{~a~^ and ~}; pass the name as a string"
                      key matches)))))
    (t (fail "argument name ~s must be a keyword or a string" key))))

;;; --- values ---------------------------------------------------------------

(defun wire-value (value)
  "VALUE as JSON: T true, :FALSE false, :NULL null, NIL absent (the caller
omits it), keyword plists objects, lists and vectors arrays, other symbols
their lowercase names."
  (cond
    ((or (member value '(:false :null t)) (stringp value) (numberp value) (hash-table-p value))
     value)
    ((null value) nil)
    ((and (consp value)
          (keywordp (first value))
          (evenp (length value))
          (loop for (key nil) on value by #'cddr always (keywordp key)))
     (let ((object (nlk:make-json-object)))
       (loop for (key item) on value by #'cddr
             for wire = (wire-value item)
             unless (null wire)
               do (setf (gethash (substitute #\_ #\- (string-downcase (symbol-name key)))
                                 object)
                        wire))
       object))
    ((or (consp value) (vectorp value)) (map 'vector #'wire-value value))
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun build-arguments (schema plist &aux (arguments (nlk:make-json-object))
                                          (timeout nil)
                                          (limit nil))
  "(values ARGUMENTS TIMEOUT LIMIT): the JSON object a tools/call carries,
and the two keys consumed here."
  (unless (evenp (length plist))
    (error 'mcp-error :detail "arguments must be keyword value pairs"))
  (loop for (key value) on plist by #'cddr
        do (case key
             (:timeout
              (unless (and (realp value) (plusp value))
                (error 'mcp-error :detail ":timeout must be a positive number of seconds"))
              (setf timeout (min value *max-timeout-seconds*)))
             (:limit
              (unless (and (integerp value) (plusp value))
                (error 'mcp-error :detail ":limit must be a positive integer"))
              (setf limit value))
             (t (let ((wire (wire-value value)))
                  (unless (null wire)
                    (setf (gethash (resolve-key key schema) arguments) wire))))))
  (values arguments timeout limit))

;;; --- results --------------------------------------------------------------

(defun clip (text &optional (limit *result-limit*))
  "TEXT cut at LIMIT with the cut disclosed."
  (nlk:clip text limit :disclose "raise :limit"))

(defun base64-placeholder (what object key)
  "The line standing in for OBJECT's base64 payload at KEY: WHAT, its type, its size."
  (format nil "[mcp ~a: ~a, ~:d base64 bytes]" what
          (or (nlk:json-value object :string "mimeType") "unknown type")
          (length (or (nlk:json-value object :string key) ""))))

(defun render-result (result &aux (content (nlk:json-value result :array "content")))
  "The tools/call result as text: its content blocks joined by newlines,
or — with no content at all — its structuredContent, else the whole result."
  (if (and content (plusp (length content)))
      (format nil "~{~a~^~%~}"
              ;; Text verbatim, anything else a one-line placeholder naming what came back.
              (loop for block across content
                    for type = (or (nlk:json-value block :string "type") "unknown")
                    collect (cond
                              ((string= type "text") (or (nlk:json-value block :string "text") ""))
                              ((member type '("image" "audio") :test #'string=)
                               (base64-placeholder (format nil "~a content" type) block "data"))
                              ((string= type "resource_link")
                               (format nil "[mcp resource link: ~a]"
                                       (or (nlk:json-value block :string "uri") "?")))
                              ((string= type "resource")
                               (let* ((resource (nlk:json-value block :object "resource"))
                                      (uri (or (nlk:json-value resource :string "uri") "?"))
                                      (text (nlk:json-value resource :string "text")))
                                 (if text
                                     (format nil "[mcp resource ~a]~%~a" uri text)
                                     (base64-placeholder (format nil "resource ~a" uri)
                                                         resource "blob"))))
                              (t (format nil "[mcp ~a content]" type)))))
      (nlk:pretty-json (or (nlk:json-value result :object "structuredContent")
                           result))))

(defun result-error-p (result)
  (or (nlk:json-value result :boolean "isError")
      (nlk:json-value result :boolean "is_error")))

;;; --- the catalog ----------------------------------------------------------

(defun server-tool (server tool-name)
  "The tool plist TOOL-NAME names on a ready server: exact, else the one
tool whose folded name matches."
  ;; NIL while the catalog is empty (the server will say); MCP-UNKNOWN-TOOL
  ;; against a catalog that lacks it.
  (nlk:when-let (tools server.tools)
    (or (find tool-name tools :key (lambda (tool) (getf tool :name)) :test #'string=)
        (let ((matches (remove-if-not
                        (lambda (tool) (string= (fold-name (getf tool :name))
                                                (fold-name tool-name)))
                        tools)))
          (and matches (null (rest matches)) (first matches)))
        (error 'mcp-unknown-tool
               :detail (format nil "~a has no tool ~s; it has ~{~a~^, ~}~:[~; ...~]"
                               (server-name server) tool-name
                               (mapcar (lambda (tool) (getf tool :name))
                                       (subseq tools 0 (min 5 (length tools))))
                               (> (length tools) 5))))))

(defun argument-text (tool &aux (schema (getf tool :schema))
                                (required (schema-required schema)))
  "The tool's arguments as keywords, required ones starred: `:path* :limit`."
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (name)
                    (format nil ":~a~:[~;*~]"
                            (kebab name)
                            (member name required :test #'string=)))
                  (schema-properties schema))))

(defun tool-line (server-name tool)
  (format nil "(mcp:~(~a~)~@[ ~a~]) - ~a"
          (or (getf tool :symbol) (tool-function-name server-name (getf tool :name)))
          (let ((text (argument-text tool))) (and (plusp (length text)) text))
          (nlk:one-line (getf tool :description) :cap 120)))

(defun state-line (snapshot)
  "One status line for a server snapshot."
  (destructuring-bind (&key name state error tool-count transport server-info &allow-other-keys)
      snapshot
    (format nil "~a: ~(~a~)~@[, ~d tool~:p~]~@[ (~a)~], ~a~@[, server ~a~]"
            name state
            (and (eq state :ready) tool-count)
            (and error (nlk:one-line error :cap 200))
            transport
            (and server-info (format nil "~a~@[ ~a~]"
                                     (nlk:json-value server-info :string "name")
                                     (nlk:json-value server-info :string "version"))))))

;;; --- the public functions ---------------------------------------------------

(defun call (server-name tool-name &rest args)
  "Call TOOL-NAME on SERVER-NAME with ARGS (keyword value pairs, see the
file header) and return the result as text."
  ;; (mcp:call "files"
  ;; "read_file" :path "/etc/hosts" :limit 2000)
  (check-type server-name string)
  (check-type tool-name string)
  (nlk:bind ((server (find-server server-name))
             (tool (and (eq server.state :ready)
                        (server-tool server tool-name)))
             (name (if tool (getf tool :name) tool-name))
             ((arguments timeout limit) (build-arguments (and tool (getf tool :schema)) args))
             (result (call-tool server name arguments :timeout-seconds timeout))
             (text (clip (render-result result) (or limit *result-limit*))))
    (when (result-error-p result)
      (error 'mcp-tool-error :server server-name :tool name :detail text))
    text))

(defun tools (&optional server-name)
  "Every tool of every server (or of SERVER-NAME) as one line each: the
call form with its arguments, required ones starred, and the description."
  (let ((servers (if server-name
                     (list (find-server server-name))
                     (registry-servers (running-registry)))))
    (format nil "~{~a~^~%~}"
            (loop for server in servers
                  for snapshot = (server-snapshot server)
                  collect (state-line snapshot)
                  append (mapcar (lambda (tool)
                                   (format nil "  ~a" (tool-line (server-name server) tool)))
                                 (getf snapshot :tools))))))

(defun schema (server-name tool-name)
  "The full inputSchema of TOOL-NAME on SERVER-NAME, as JSON text."
  (let* ((server (find-server server-name))
         (tool (or (server-tool server tool-name)
                   (error 'mcp-unknown-tool
                          :detail (format nil "~a has no catalog yet (~(~a~))"
                                          server-name server.state)))))
    (nlk:pretty-json (getf tool :schema))))

(defun status ()
  "One line per configured server: state, tool count, transport (a header
count, never a value), the last error and the stderr log of a process."
  (nlk:if-let (servers (registry-servers (running-registry)))
    (format nil "~{~a~^~%~}"
            (loop for server in servers
                  for snapshot = (server-snapshot server)
                  collect (format nil "~a~@[~%  stderr: ~a~]"
                                  (state-line snapshot)
                                  (getf snapshot :log-path))))
    "no MCP servers configured (mcp.servers in ~/.nodecode/config.jsonc)"))

(defun restart (server-name &aux (server (find-server server-name)))
  "Reconnect SERVER-NAME on its own thread; (mcp:status) shows the outcome."
  (when (member server.state '(:disabled :refused))
    (error 'mcp-offline :server server-name :detail (or server.error-text "disabled in config")))
  (if (spawn-connect server)
      (format nil "restarting ~a; (mcp:status) to follow" server-name)
      (format nil "~a cannot be restarted now" server-name)))
