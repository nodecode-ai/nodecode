;;;; invocation.lisp --- canonical command invocation provenance.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; This is the kernel-owned provenance contract. Surfaces construct one typed
;;;; INVOCATION; admission validates its identity and the event log stores its
;;;; canonical JSON representation. No surface-specific plist crosses admission.

(in-package #:nodecode.kernel)

(define-error invocation-error (error) (field message)
  (:report "invalid invocation ~a: ~a" field message))

(define-record (invocation-source (:constructor %make-invocation-source)
                                  (:copier nil) (:export :readers))
  (kind (error "source kind required") :type string :read-only t)
  (id nil :type (or null string) :read-only t))

(define-record (invocation-transport (:constructor %make-invocation-transport)
                                      (:copier nil) (:export :readers))
  (domain (error "transport domain required") :type string :read-only t)
  (connection-id nil :type (or null string) :read-only t))

(define-record (invocation (:constructor %make-invocation) (:copier nil) (:export :readers))
  (action (error "invocation action required") :type string :read-only t)
  (command-id (error "invocation command id required") :type string :read-only t)
  (correlation-id (error "invocation correlation id required") :type string :read-only t)
  (source (error "invocation source required") :type invocation-source :read-only t)
  (transport (error "invocation transport required") :type invocation-transport :read-only t))

(access (value invocation))

(defun invocation-string (value field &key optional)
  (unless (or (and optional (null value)) (and (stringp value) (plusp (length value))))
    (error 'invocation-error :field field
           :message (if optional "must be NIL or a non-empty string" "must be a non-empty string")))
  value)

(defun invocation-typed (value type field)
  "Return VALUE once it is of TYPE, refusing FIELD otherwise."
  ;; Wire-shaped plists never cross this boundary; only the typed structs do.
  (unless (typep value type)
    (error 'invocation-error :field field
           :message (format nil "must be an ~(~a~)" type)))
  value)

(defun make-invocation-source (kind &key id)
  (%make-invocation-source
   :kind (invocation-string kind "source.kind")
   :id (invocation-string id "source.id" :optional t)))

(defun make-invocation-transport (domain &key connection-id)
  (%make-invocation-transport
   :domain (invocation-string domain "transport.domain")
   :connection-id (invocation-string connection-id "transport.connection_id" :optional t)))

(defun make-invocation (&key action command-id correlation-id source transport)
  "Construct a fully typed invocation context."
  ;; SOURCE and TRANSPORT accept their typed structs only; callers must not
  ;; pass wire-shaped plists.
  (%make-invocation
   :action (invocation-string action "action")
   :command-id (invocation-string command-id "command_id")
   :correlation-id (invocation-string correlation-id "correlation_id")
   :source (invocation-typed source 'invocation-source "source")
   :transport (invocation-typed transport 'invocation-transport "transport")))

(defun validate-invocation (value &key command-id)
  "Validate VALUE at the admission boundary and return it."
  ;; The command identity is checked against the durable command when
  ;; supplied; this prevents a surface from journaling provenance
  ;; for another command. Its fields are MAKE-INVOCATION's, checked there and
  ;; read-only since, so the struct's type is the whole of their check.
  (invocation-typed value 'invocation "invocation")
  (when (and command-id (not (string= command-id (invocation-command-id value))))
    (error 'invocation-error :field "command_id" :message "does not match the durable command"))
  value)

(defun default-invocation (action command-id &key (source "in_process") id)
  "Build provenance for an in-process caller that has no transport metadata:
SOURCE is its source kind (\"kernel\" for the kernel's own writes), ID its id."
  (make-invocation
   :action action :command-id command-id :correlation-id command-id
   :source (make-invocation-source source :id id)
   :transport (make-invocation-transport "in_process")))

(defun invocation-json-object (value)
  "Return VALUE in the canonical string-keyed JSON object shape."
  (validate-invocation value)
  (json-object
   "action" value.action
   "command_id" value.command-id
   "correlation_id" value.correlation-id
   "source" (json-object
             "kind" (invocation-source-kind value.source)
             :opt "id" (invocation-source-id value.source))
   "transport" (json-object
                "domain" (invocation-transport-domain
                          value.transport)
                :opt "connection_id" (invocation-transport-connection-id value.transport))))
