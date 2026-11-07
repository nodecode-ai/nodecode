;;;; support.lisp --- link test runner, and what the line would have carried.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Link tests register into the shared nodecode.test registry under the
;;;; LINK-CELL- prefix; RUN-LINK-TESTS runs exactly that slice. Most of them
;;;; stand in for the relay by hand: NODECODE-LINK::*SEND* is set (never
;;;; bound: a flow answers on a thread of its own) to post every frame the
;;;; machine sends to a mailbox, and a browser's request is handed to the same
;;;; functions the line's reader calls.

(in-package #:nodecode.test)

(define-test-slice "link" "LINK-CELL-" :start nodecode-link:start-cell
  ;; A relay nothing answers at: a test that wants a line serves one.
  :defaults (("relay" "ws://127.0.0.1:9/line")))

(defparameter +link-origin+ "https://m-test.example.test"
  "The address the relay is taken to have named at welcome.")

(defvar *link-sent* nil
  "The mailbox the machine's frames go to while a test stands in for the relay.")

(defvar *link-held* '()
  "Frames taken off the mailbox for another stream than the one asked about.")

(defmacro with-link ((&key (origin '+link-origin+)) &body body)
  "Run BODY with the link cell started, its frames posted to *LINK-SENT*,
and ORIGIN kept as this machine's address."
  `(with-saved-globals ((nodecode-link::*send* (lambda (frame)
                                                 (sb-concurrency:send-message *link-sent* frame)))
                        (*link-sent* (sb-concurrency:make-mailbox :name "link sent"))
                        (*link-held* '()))
     (with-cell-stop ((link-start))
       (nodecode-link::keep "origin" ,origin "machine" "m-test")
       ,@body)))

(defun link-frame (frame)
  "A sent FRAME read back: (:control OBJECT) or (:data STREAM FLAGS OCTETS)."
  (if (stringp frame)
      (list :control (nlk:decode-json frame))
      (list :data (nodecode-link::frame-stream frame) (nodecode-link::frame-flags frame)
            (nodecode-link::frame-payload frame))))

(defun link-frame-stream (read)
  (if (eq (first read) :control) (gethash "s" (second read)) (second read)))

(defun link-next (id &key (timeout 10))
  "The next frame the machine sent on stream ID, read back, or NIL."
  (let ((held (find id *link-held* :key #'link-frame-stream)))
    (when held
      (setf *link-held* (remove held *link-held* :count 1))
      (return-from link-next held)))
  (loop for frame = (sb-concurrency:receive-message *link-sent* :timeout timeout)
        while frame
        do (let ((read (link-frame frame)))
             (if (eql id (link-frame-stream read))
                 (return read)
                 (setf *link-held* (append *link-held* (list read)))))))

(defun link-answer (id)
  "Stream ID's answer: => (values STATUS HEADERS BODY), HEADERS an alist and
BODY a string, once the machine ends it."
  (let ((status nil) (headers '()) (body '()))
    (loop for read = (link-next id)
          while read
          do (if (eq (first read) :data)
                 (push (fourth read) body)
                 (let ((object (second read)))
                   (nlk:with-json ((type :text "t")) object
                     (cond ((equal type "head")
                            (setf status (gethash "status" object)
                                  headers (map 'list (lambda (pair) (cons (aref pair 0) (aref pair 1)))
                                               (gethash "headers" object))))
                           (t (return)))))))
    (values status headers
            (sb-ext:octets-to-string (apply #'concatenate '(vector (unsigned-byte 8)) (reverse body))
                                     :external-format :utf-8))))

(defvar *link-next-stream* 100)

(defun link-open (method path &key headers body (kind "http") (ip "203.0.113.9") (place "Zagreb, HR"))
  "Hand the machine one browser request as the relay would => its stream number."
  (let ((id (incf *link-next-stream*)))
    (nodecode-link::on-open
     (nlk:make-json-object "t" "open" "s" id "kind" kind "method" method "path" path
                           "headers" (map 'vector (lambda (pair) (vector (car pair) (cdr pair))) headers)
                           "ip" ip "place" place))
    (when (equal kind "http")
      (when body
        (nodecode-link::on-data (nodecode-link::data-frame
                                 id 0 (sb-ext:string-to-octets body :external-format :utf-8))))
      (nodecode-link::on-end id))
    id))

(defun link-request (method path &rest keys)
  "One HTTP request through the machine => (values STATUS HEADERS BODY)."
  (link-answer (apply #'link-open method path keys)))

(defun link-cookie (cookie)
  (cons "cookie" (format nil "other=1; __Host-link=~a" cookie)))

(defun link-own-origin ()
  (cons "origin" +link-origin+))
