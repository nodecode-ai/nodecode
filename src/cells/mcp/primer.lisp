;;;; primer.lisp --- (help :mcp): the manual, and the line every request carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the vocabulary from the manual: a static preamble (the
;;;; forms, the argument rules, how errors read) and the live catalog — one
;;;; line per tool, the call form with its arguments and the description —
;;;; answered by (help :mcp). Every request's help section carries one line
;;;; naming the configured servers, which changes with the config alone, so
;;;; the prompt prefix holds while servers come and go.
;;;;
;;;; Three tiers under one budget: every tool with arguments; then names
;;;; only per server with a pointer to (mcp:tools); then counts. The first
;;;; tier that fits wins.
;;;;
;;;; Nothing is written to any store, so removing the folder leaves no
;;;; trace — the CELLS.md rule that presence is the whole switch.

(in-package #:nodecode-mcp)

(defparameter *primer-budget* 12288
  "Characters the whole manual may take before the catalog degrades a tier.")

(defparameter *help-summary*
  "mcp:SERVER/TOOL calls a tool of a configured MCP server: ~{~a~^, ~}"
  "The help section's line for (help :mcp), a format string over the
configured servers' names.")

(defvar *primer-lock* (bt2:make-lock :name "mcp-primer"))

;;; Vocabulary here is the contract surface.lisp and wrappers.lisp keep;
;;; change them together.
(defparameter +preamble+
  "MCP servers configured by the user are connected through the nodecode-mcp cell. Every remote
tool is a Lisp function in the mcp: package, called through eval; every function returns a STRING.

  (mcp:files/read-file :path \"/etc/hosts\")     a tool: mcp:SERVER/TOOL in kebab-case, keyword arguments
  (mcp:call \"files\" \"read_file\" :path \"x\")    the same call by name; string keys pass verbatim: \"per_page\" 5
  (mcp:tools)  (mcp:tools \"files\")             every tool: call form, arguments (required ones starred), description
  (mcp:schema \"files\" \"read_file\")            the tool's full JSON input schema
  (mcp:status)                                 every server: state, tool count, transport, last error, stderr log
  (mcp:restart \"files\")                        reconnect one server

Rules:
1. Keyword arguments fold to the schema's names (:per-page is per_page, :include-snapshot is
   includeSnapshot). Values: strings and numbers as they are, t is true, :false is false, nil is absent,
   a list is an array, a keyword plist (:a 1 :b \"x\") is a nested object. (describe 'mcp:files/read-file)
   shows a tool's arguments and documentation.
2. Results are text: content blocks joined, images and resources as one-line placeholders, capped near
   7000 characters. Pass :limit N on the call to change the cap.
3. A call blocks until the server answers, up to its timeout (default 30 s); :timeout SECONDS on the
   call changes it. For a tool slower than 10 s pass yield_time_ms on the eval call so the form is
   not backgrounded; if you ever see status running, (eval-await ID) - never re-send the call.
4. ERROR: MCP-TOOL-ERROR is the tool's own refusal: read it, fix the arguments. MCP-OFFLINE means the
   server is down after one reconnect: run (mcp:status), tell the user what it says, do not loop on it.
   MCP-TIMEOUT keeps the connection; retry once with a larger :timeout only if the tool is known to be slow.
5. Inside a definition you keep (a defun), write (mcp:call \"server\" \"tool\" ...) rather than the
   generated name: the generated function exists only while its server is connected.

Catalog:"
  "The static half of the block.")

;;; --- the catalog ------------------------------------------------------------

(defun catalog-tier (snapshots tier)
  "The catalog at TIER: 1 every tool with arguments, 2 names per server,
3 counts per server."
  (with-output-to-string (out)
    (dolist (snapshot snapshots)
      (destructuring-bind (&key name state tools &allow-other-keys) snapshot
        ;; What a server offers, never how it is doing: printing a state here —
        ;; connecting, ready, or error — would move the prompt prefix for every
        ;; session whenever a server came up or dropped. A server that dropped
        ;; keeps the lines of the tools it listed, a call to one answers
        ;; MCP-OFFLINE (rule 4), and (mcp:status) is where a state is read.
        ;; Disabled and refused are the config's word, the same at every boot.
        (cond
          ((member state '(:disabled :refused))
           (format out "~%  ~a: ~(~a~)" name state))
          ((null tools)
           (format out "~%  ~a: no tools listed - (mcp:status) says where it stands" name))
          ((= tier 1)
           (dolist (tool tools)
             (format out "~%  ~a" (tool-line name tool))))
          ((= tier 2)
           (format out "~%  ~a: ~{~a~^ ~} - as (mcp:~a/NAME ...); (mcp:tools ~s) lists the arguments"
                   name
                   (mapcar (lambda (tool)
                             (nlk:if-let (symbol (getf tool :symbol))
                               (subseq (string-downcase (symbol-name symbol))
                                       (1+ (position #\/ (symbol-name symbol))))
                               (getf tool :name)))
                           tools)
                   (kebab name) name))
          (t
           (format out "~%  ~a: ~d tool~:p - (mcp:tools ~s)" name (length tools) name)))))))

(defun refresh-primer (&aux (registry *registry*))
  "Rebuild (help :mcp) from the live registry; no topic without servers."
  (bt2:with-lock-held (*primer-lock*)
    (if (and registry registry.servers (not registry.stopping-p))
        (nle:set-help-topic
         :mcp (format nil *help-summary* (registry-server-names registry))
         ;; The whole manual at the first tier that fits the budget.
         (loop with snapshots = (mapcar #'server-snapshot registry.servers)
               for tier from 1 to 3
               for text = (concatenate 'string +preamble+ (catalog-tier snapshots tier))
               when (or (= tier 3) (<= (length text) *primer-budget*))
                 return text))
        (nle:set-help-topic :mcp nil))))

(defun primer-text ()
  "What (help :mcp) answers, or NIL while no server is configured."
  ;; One read of one variable: the lock is for the rebuild, which it orders.
  (cddr (assoc :mcp nle::*help-topics*)))
