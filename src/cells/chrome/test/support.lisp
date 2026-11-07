;;;; support.lisp --- chrome cell test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Chrome tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a CHROME-CELL- name
;;;; prefix; RUN-CHROME-TESTS runs exactly that slice, so this system's
;;;; test-op never re-runs the core suite and `just test` never runs
;;;; chrome tests.
;;;;
;;;; The bridge is a real acceptor on an ephemeral port: CHROME::*BRIDGE-PORT*
;;;; is SETF'd (WITH-SAVED-GLOBALS), never LET-bound, because hunchentoot's
;;;; worker threads read it. START-FAKE-EXTENSION is the extension's poll
;;;; loop in twenty lines: GET /next, run the handler, POST /result.

(in-package #:nodecode.test)

;; CHROME-CELL-, not CHROME-: the TUI suite owns chrome-* names already
;; (test/tui/chrome-test.lisp, window chrome), and a bare CHROME prefix
;; would pull those into this runner.
(define-test-slice "chrome" "CHROME-CELL-" :start chrome:start-cell)

(defparameter +fake-origin+ "chrome-extension://abcdefghijklmnopabcdefghijklmnop")

(defun bridge-http (port method path &key origin body headers (timeout 10))
  "(values STATUS BODY-OBJECT RESPONSE-HEADERS) against the bridge."
  ;; ORIGIN rides as the Origin header; BODY (a JSON object) is encoded. A
  ;; 4xx/5xx arrives as values too, never as a signal.
  (multiple-value-bind (response status response-headers)
      (nlk:http method (format nil "http://127.0.0.1:~d~a" port path)
                :headers (append (when origin (list (cons "origin" origin)))
                                 (when body (list (cons "content-type" "application/json")))
                                 headers)
                :content (and body (nlk:encode-json-object body))
                :timeout timeout :connect-timeout timeout)
    (values status
            (ignore-errors (and (stringp response) (plusp (length response))
                                (shasht:read-json response)))
            response-headers)))

(defun start-fake-extension (port handler &key (origin +fake-origin+)
                                               (name "Nodecode Chrome Connector fake")
                                               &aux (stop nil))
  "A thread playing the extension: poll /next, run (HANDLER COMMAND) =>
(values OK RESULT-OR-ERROR), post /result."
  ;; Returns a stop thunk that ends the loop at its next poll.
  (nlk:spawn "chrome-fake-extension"
    (loop until stop
          do (nlk:with-handlers ((error () (sleep 0.05)))
               (multiple-value-bind (status payload)
                   (bridge-http port :get (format nil "/next?name=~a" (quri:url-encode name))
                                :origin origin :timeout 40)
                 (when (and (eql status 200)
                            (equal (nlk:json-value payload :text "type") "command"))
                   (let ((command (nlk:json-value payload :object "command")))
                     (multiple-value-bind (ok value) (funcall handler command)
                       (bridge-http port :post "/result"
                                    :origin origin
                                    :body (nlk:json-object "id" (gethash "id" command)
                                                           "ok" (if ok t :false)
                                                           :when ok "result" value
                                                           :when (not ok) "error" value)))))))))
  (lambda () (setf stop t)))

(defmacro with-fake-extension ((port handler &rest keys) &body body)
  "Run BODY with a fake extension polling PORT and answering through HANDLER
(START-FAKE-EXTENSION's KEYS), stopped on unwind."
  `(with-cell-stop ((start-fake-extension ,port ,handler ,@keys))
     ,@body))

(defmacro with-chrome-session ((session-var) &body body)
  "A temp store with one durable session, SESSION-VAR bound to its id."
  `(with-temp-store ()
     (let ((,session-var (ensure-durable-session "s-chrome")))
       ,@body)))

(defmacro with-chrome-bridge ((port-var bridge-var &key (version "0.15.46")) &body body)
  "Start a real bridge on an ephemeral port, bound as CHROME::*BRIDGE* too
so SEND finds it; stop it on unwind."
  `(let ((,port-var (temp-gateway-port))
         (,bridge-var nil))
     (declare (ignorable ,port-var ,bridge-var))
     (with-saved-globals (chrome::*bridge* (chrome::*bridge-port* ,port-var))
       (nlk:with-cleanup ((when ,bridge-var (chrome::stop-bridge ,bridge-var))
                          (setf chrome::*bridge* nil))
         (setf ,bridge-var (chrome::start-bridge :port ,port-var :version ,version)
               chrome::*bridge* ,bridge-var)
         ,@body))))
