;;;; store-heal-test.lisp --- a store a failed write broke heals; its lease stays.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A write whose ROLLBACK fails too (a full disk) used to reconnect once and,
;;;; that failing as well, close the store: the lease went to whoever wanted it
;;;; next, and this process refused every later write with STORE-NOT-OPEN until
;;;; it was restarted. These gates hold the store to keeping its lease, closing
;;;; the half-made connection, waiting between tries, and reconnecting on a
;;;; later use once writes go through again.

(in-package #:nodecode.test)

(defun break-store-write ()
  "Run one write whose ROLLBACK fails: its body commits behind the store's
back and then fails, so the store's ROLLBACK finds no transaction."
  (signals-error error
    (nlk::with-store-transaction (handle)
      (sqlite:execute-non-query handle "commit")
      (error "database or disk is full"))))

(defmacro with-full-disk ((full &key (seen (gensym "SEEN")) (tries (gensym "TRIES"))) &body body)
  "Run BODY with every fresh connection's schema step failing while FULL is
true: SEEN collects the connections that step was handed, TRIES counts them."
  `(let* ((,full t) (,seen '()) (,tries 0)
          (nlk::*schema-prepare-hooks*
            (cons (lambda (handle)
                    (push handle ,seen)
                    (incf ,tries)
                    (when ,full (error "database or disk is full")))
                  nlk::*schema-prepare-hooks*)))
     (declare (ignorable ,seen ,tries))
     ,@body))

(defmacro saying ((said) &body body)
  "Run BODY with each warning it raises muffled and its text pushed on SAID."
  `(handler-bind ((warning (lambda (warning)
                             (push (princ-to-string warning) ,said)
                             (muffle-warning warning))))
     ,@body))

(deftest a-failed-rollback-keeps-the-store-and-its-lease (with-temp-store (store))
  (let ((said '()))
    (with-full-disk (full :seen seen)
      (saying (said) (break-store-write))
      (is (eq store nlk::*store*) "the store is still the image's, not closed")
      (is (nlk::store-lock-fd store) "the lease lock is still held")
      (is (nlk::store-lease-mine-p (nlk::store-path store)) "this process still owns the lease")
      (is (probe-file (nlk::lease-path-for-store (nlk::store-path store))) "the owner sidecar still names this process")
      (is (not (nlk:store-open-p)) "the unusable connection is dropped")
      (is (and seen (notany (lambda (handle) (slot-boundp handle 'sqlite::handle)) seen)) "the reconnect whose schema step failed is closed, not leaked")
      (is (= 1 (length said)) "one line says the store broke"))))

(deftest a-broken-store-reconnects-once-writes-go-through (with-temp-store (store))
  (let ((said '()))
    (with-full-disk (full)
      (saying (said)
        (break-store-write)
        (setf (nlk::store-break store :retry-at) 0)
        (is (signals-error nlk::store-broken (nlk::with-store-transaction (handle) handle)) "a write while the disk is still full is refused, saying why")
        (setf full nil
              (nlk::store-break store :retry-at) 0)
        (nlk::with-store-transaction (handle)
          (sqlite:execute-non-query handle "create table heal_probe (x)")
          (sqlite:execute-non-query handle "insert into heal_probe values (42)"))
        (is (eql 42 (nlk::with-store-handle (handle) (sqlite:execute-single handle "select x from heal_probe"))) "the first write once space is back reconnects and lands")
        (is (nlk:store-open-p) "the store serves again")
        (is (nlk::store-lease-mine-p (nlk::store-path store)) "the lease never left")
        (is (null (nlk::store-break store :reason)) "nothing marks it broken any more"))
      (is (= 2 (length said)) "one line when it broke, one when it came back")
      (is (search "reconnected" (first said)) "the last line says it is back"))))

(deftest a-broken-store-waits-between-reconnect-tries (with-temp-store (store))
  (with-full-disk (full :tries tries)
    (handler-bind ((warning #'muffle-warning))
      (break-store-write)
      (is (= 1 tries) "breaking tries one reconnect at once")
      (loop repeat 5
            do (signals-error nlk::store-not-open (nlk::with-store-transaction (handle) handle)))
      (is (= 1 tries) "a use inside the wait tries no reconnect")
      (setf (nlk::store-break store :retry-at) 0)
      (signals-error nlk::store-not-open (nlk::with-store-transaction (handle) handle))
      (is (= 2 tries) "a use after the wait tries again")
      (is (> (nlk::store-break store :delay) 1/2) "each failed try waits longer")
      (is (<= (nlk::store-break store :delay) nlk::*store-retry-limit*) "and never longer than the limit"))))

(deftest closing-a-broken-store-gives-its-lease-up (with-temp-store (store))
  (with-full-disk (full :tries tries)
    (handler-bind ((warning #'muffle-warning))
      (break-store-write))
    (nlk:close-store)
    (is (null nlk::*store*) "a deliberate close still closes")
    (is (not (probe-file (nlk::lease-path-for-store (nlk::store-path store)))) "and retracts the owner sidecar")
    (is (not (nlk::store-lease-held-p (nlk::store-path store))) "and frees the lock")
    (setf full nil
          (nlk::store-break store :retry-at) 0)
    (is (not (nlk::heal-store store)) "a closed store never reconnects")
    (is (= 1 tries) "no try after the close")))
