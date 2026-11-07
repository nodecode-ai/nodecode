;;;; support.lisp --- websearch test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Websearch tests register into the SAME nodecode.test registry (the
;;;; core DEFTEST, with its hermetic machine-state posture) under a
;;;; WEBSEARCH-CELL- name prefix; RUN-WEBSEARCH-TESTS runs exactly that
;;;; slice, so this system's test-op never re-runs the core suite and `just
;;;; test' never runs websearch tests.
;;;;
;;;; The hermetic posture SETFs NLE:*HOOKS* to '() around every test body and
;;;; restores it on unwind. A test still stops its own cell in an
;;;; UNWIND-PROTECT: *WEBSEARCH* and *PAGES* are globals, not bindings.
;;;;
;;;; The one network call is NODECODE-WEBSEARCH::HTTP; WITH-SCRIPTED-HTTP
;;;; stubs that name (the notify LAUNCH pattern) so no test ever opens a
;;;; socket, records every call, and answers a scripted response.

(in-package #:nodecode.test)

;;; --- config fixtures ---------------------------------------------------------

(defun websearch-providers (&rest pairs)
  "A `providers' object from alternating NAME KEY pairs, as shasht decodes
it."
  (apply #'nlk:make-json-object
         (loop for (name key) on pairs by #'cddr
               append (list name (nlk:json-object "api_key" key)))))

(define-test-slice "websearch" "WEBSEARCH-CELL-" :start web:start-cell
  :defaults (("providers" (websearch-providers "brave" "brave-key"))))

(defmacro with-websearch ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS, stopping it and
clearing the page cache on unwind. (:KEYLESS) configures no provider key."
  `(nlk:with-cleanup ((setf nodecode-websearch::*websearch* nil
                            nodecode-websearch::*pages* '()))
     (setf nodecode-websearch::*pages* '())
     (with-cell-stop ((websearch-start ,@(if (equal config-pairs '(:keyless))
                                              '("providers" (nlk:make-json-object))
                                              config-pairs)))
       ,@body)))

;;; --- the scripted seam ---------------------------------------------------------

(defun response-headers (&rest pairs)
  "A response header table the way dexador answers it: EQUAL, lowercase
string keys."
  (apply #'nlk:make-json-object pairs))

(defun json-body (&rest specs)
  "A JSON text body built from alternating key/value pairs (values may be
objects or vectors)."
  (nlk:encode-json-object (apply #'nlk:make-json-object specs)))

(defstruct (http-call (:copier nil))
  method url headers content timeout max-bytes)

(nlk:access (call http-call))

(defmacro with-scripted-http ((calls &key (status 200) body
                                          (headers '(response-headers "content-type" "application/json"))
                                          truncated text type)
                              &body body-forms)
  "Run BODY-FORMS with NODECODE-WEBSEARCH::HTTP recording every call into
the list CALLS (oldest first) and answering (values BODY STATUS HEADERS
TRUNCATED)."
  ;; BODY, STATUS, HEADERS and TRUNCATED may each be a function of the
  ;; HTTP-CALL, for a script that answers by URL. TEXT is a BODY of its utf-8
  ;; octets, TYPE HEADERS holding only that content-type.
  (let ((call (gensym "CALL"))
        (body (if text `(string-octets ,text) body))
        (headers (if type `(response-headers "content-type" ,type) headers)))
    `(let ((,calls '()))
       (flet ((script (value ,call)
                (if (functionp value) (funcall value ,call) value)))
         (with-stubbed-fdefinition (nodecode-websearch::http
                                    (method url &key headers content (timeout 30) max-bytes)
                                    (let ((,call (make-http-call :method method :url url
                                                                 :headers headers :content content
                                                                 :timeout timeout
                                                                 :max-bytes max-bytes)))
                                      (setf ,calls (append ,calls (list ,call)))
                                      (values (script ,body ,call)
                                              (script ,status ,call)
                                              (script ,headers ,call)
                                              (script ,truncated ,call))))
           ,@body-forms)))))

(defun call-header (call name)
  "The value of header NAME on CALL, or NIL."
  (cdr (assoc name call.headers :test #'string-equal)))

(defun contains-p (text needle)
  (and (stringp text) (cl:search needle text) t))
