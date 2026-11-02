;;;; session-admit-test.lisp --- admission keeps durable input out of heap state.

(in-package #:nodecode.test)

(nlk:access (a nle::session))

(deftest admit-does-not-append-to-session-messages ()
  (let ((session (nle::make-session :id "test-admit")))
    (is-shape session (.messages null "session starts with no heap messages") (.in-flight-p not))
    (nle:admit session "hello")
    (is (nodecode.evolved::session-in-flight-p session) "admit sets the latch")
    (is (null session.messages))))

(deftest intern-session-is-one-object-per-id (let ((a (nle:intern-session "intern-same"))
                                                   (b (nle:intern-session "intern-same"))
                                                   (c (nle:intern-session "intern-other"))))
  ;; Heap-only: no store, so intern cannot consult SESSION.CREATED.
  (is (eq a b) "the same id returns the same session")
  (is (not (eq a c)) "a different id is a different session")
  (is (equal "intern-same" a.id) "the interned id is stored"))
