;;;; reader-test.lisp --- a long read holds no writer's lock (T-331).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cold Dashboard usage scan held *STORE-LOCK* for 9-17 s on the store's one
;;;; connection, and every durable write waited behind it. WITH-STORE-READER
;;;; moves a scope's reads to a read-only connection of their own: the gates
;;;; below are that the lock stays free while such a read is mid-flight, that
;;;; the reader sees committed rows and refuses writes, and that a writer's own
;;;; reads stay on the transaction's connection.

(in-package #:nodecode.test)

(deftest a-store-reader-reads-off-the-writers-lock (with-session-store ("s-reader"))
  (nlk:record-event "s-reader" "probe" (list :n 1))
  (let ((committed (nlk:events :session-id "s-reader" :as :count))
        (held (sb-thread:make-semaphore))
        (release (sb-thread:make-semaphore))
        (seen nil))
    (let ((reader (sb-thread:make-thread
                   (lambda ()
                     (nlk::with-store-reader ()
                       (nlk::with-store-handle (handle)
                         (setf seen (list (eq handle (nlk::store-handle nlk::*store*))
                                          (nlk:events :session-id "s-reader" :as :count)))
                         (sb-thread:signal-semaphore held)
                         (sb-thread:wait-on-semaphore release :timeout 10))))
                   :name "reader-test")))
      (is (sb-thread:wait-on-semaphore held :timeout 10) "the reader entered its read")
      ;; A writer's turn at the lock, from a third thread, while the reader
      ;; is parked mid-read: it gets the lock and writes within two seconds.
      (let* ((wrote (sb-thread:make-semaphore))
             (writer (sb-thread:make-thread
                      (lambda ()
                        (nlk:record-event "s-reader" "probe" (list :n 2))
                        (sb-thread:signal-semaphore wrote))
                      :name "reader-test-writer")))
        (is (sb-thread:wait-on-semaphore wrote :timeout 2) "a write lands while the reader is mid-read")
        (sb-thread:signal-semaphore release)
        (sb-thread:join-thread reader)
        (sb-thread:join-thread writer))
      (is (equal (list nil committed) seen) "the read ran on its own connection and saw the committed row"))))

(deftest a-store-reader-is-no-second-writer (with-session-store ("s-reader"))
  (let ((before (nlk:events :session-id "s-reader" :as :count)))
    (nlk::with-store-reader ()
      (is (signals-error error (nlk::with-store-handle (handle) (sqlite:execute-non-query handle "create table reader_probe (a)"))) "the reader connection refuses a write")
      (nlk:record-event "s-reader" "probe" (list :n 1)))
    (is (= (1+ before) (nlk:events :session-id "s-reader" :as :count)) "a durable write inside the scope still lands through the writer")))

(deftest a-writers-reads-stay-on-its-transaction (with-temp-store ())
  (nlk::with-store-transaction (handle)
    (nlk::with-store-reader ()
      (nlk::with-store-handle (inner)
        (is (eq handle inner) "inside a transaction the reader scope keeps the transaction's connection")))))
