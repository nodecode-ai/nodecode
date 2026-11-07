;;;; package.lisp --- the MCP package: model-facing vocabulary and conditions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MCP so a EVAL form reads (mcp:call ...) or,
;;;; for a generated tool function, (mcp:files/read-file ...). The generated
;;;; symbols are interned and exported here at connect time (wrappers.lisp),
;;;; so the package's export list grows with the catalog; the names below are
;;;; the fixed part of the contract.
;;;;
;;;; RESTART shadows its CL namesake (a restart is not a thing this package
;;;; names); use the package qualified, (mcp:restart "files"), as every
;;;; EVAL form does.
;;;;
;;;; Conditions print as "ERROR: MCP-OFFLINE: ..." through the eval snippet,
;;;; which is the text the harness primer tells the model to act on.

(defpackage #:nodecode-mcp
  (:use #:cl)
  (:nicknames #:mcp)
  (:shadow #:restart)
  (:export
   ;; the cell entry
   #:start-cell
   ;; model-facing vocabulary (see the primer in primer.lisp)
   #:call #:tools #:schema #:status #:restart
   ;; conditions
   #:mcp-error #:mcp-unknown-server #:mcp-unknown-tool #:mcp-offline
   #:mcp-timeout #:mcp-tool-error))

(in-package #:nodecode-mcp)

(nlk:define-peripheral mcp)

(define-condition mcp-unknown-server (mcp-error) ()
  (:documentation "No configured server by that name; DETAIL lists the names."))

(define-condition mcp-unknown-tool (mcp-error) ()
  (:documentation "The server's catalog has no such tool; DETAIL names some."))

(nlk:define-error mcp-offline (mcp-error) ((server :reader mcp-error-server))
  (:report "~a is not available: ~a" server detail)
  (:documentation "The server is not ready and one reconnect did not bring
it back; DETAIL carries the last failure (and the stderr log path for a
process)."))

(nlk:define-error mcp-timeout (mcp-error) ((server :reader mcp-error-server))
  (:report "~a: ~a" server detail)
  (:documentation "The deadline passed; the connection is kept."))

(nlk:define-error mcp-tool-error (mcp-error)
    ((server :reader mcp-error-server) (tool :reader mcp-error-tool))
  (:report "~a/~a: ~a" server tool detail)
  (:documentation "The tool answered isError, or the server refused the call
with a JSON-RPC error; DETAIL is the server's own text."))
