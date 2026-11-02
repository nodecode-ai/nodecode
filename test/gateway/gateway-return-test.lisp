;;;; gateway-return-test.lisp --- A shell leaves the store to its gateway's return.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; a test box, 2026-10-21: an update's restart left the store without an owner
;;;; for eleven seconds (the unit stopping, then its restart delay). Every
;;;; open shell started an organism of its own in that gap, from its own older
;;;; binary; one took the store, and the unit's new gateway waited behind it
;;;; for fifteen minutes while the shells could send nothing.

(in-package #:nodecode.test)

(defun unit-state (active sub &optional result)
  "A gateway state as GATEWAY-STATE folds the service manager's answer."
  (nlk::make-gateway-state :active active :sub sub :result result))

(deftest a-gateway-on-its-way-back-is-returning ()
  (flet ((returning (active sub &optional result)
           (nle::gateway-returning-p (unit-state active sub result))))
    (is (eq t (returning "active" "running")) "a serving gateway")
    (is (eq t (returning "deactivating" "stop-sigterm")) "an update's exit: the unit stopping")
    (is (eq t (returning "activating" "auto-restart")) "the unit's restart delay after a clean exit")
    (is (eq t (returning "inactive" "auto-restart")) "launchd's respawn after exit 0, in systemd's words")
    (is (null (returning "activating" "auto-restart" "exit-code")) "a crash loop's restart is not one")
    (is (null (returning "failed" "failed" "exit-code")) "a failed unit")
    (is (null (returning "inactive" "dead")) "a stopped unit")))

(deftest a-shell-leaves-the-store-to-a-gateway-in-its-restart-gap ()
  (let ((store nlk::*default-store-path*))
    (with-stubbed-fdefinition (nle::gateway-supervises-p (path) (declare (ignore path)) t)
      ;; Between the clean exit and the unit's start: the gateway takes the
      ;; store on the fifth look, and the shell starts nothing of its own.
      (let ((looks 0) (asked 0))
        (is (eq t (nle::await-gateway-store
                   store
                   :state (lambda () (incf asked) (unit-state "activating" "auto-restart"))
                   :held-p (lambda (path) (declare (ignore path)) (> (incf looks) 4))
                   :seconds 5 :pause 0.01)) "it waits out the restart, then the gateway holds the store")
        (is (= 5 looks) "the lock was watched until the gateway took it")
        (is (= 1 asked) "the unit was asked once a second, not every look"))
      ;; A crash loop is not waited on: the shell starts its organism at once.
      (let ((looks 0))
        (is (null (nle::await-gateway-store
                   store
                   :state (lambda () (unit-state "activating" "auto-restart" "exit-code"))
                   :held-p (lambda (path) (declare (ignore path)) (incf looks) nil)
                   :seconds 5 :pause 0.01)) "a crash loop's restart is not waited on")
        (is (= 1 looks) "it answers on its first look"))
      ;; A gateway that never comes for the store is waited on only so long.
      (let ((start (get-internal-real-time)))
        (is (null (nle::await-gateway-store
                   store
                   :state (lambda () (unit-state "deactivating" "stop-sigterm"))
                   :held-p (lambda (path) (declare (ignore path)) nil)
                   :seconds 0.3 :pause 0.01)) "past SECONDS the shell goes on")
        (is (< (nle::since-ms start) 2000) "bounded by SECONDS")))
    (with-stubbed-fdefinition (nle::gateway-supervises-p (path) (declare (ignore path)) nil)
      (let ((asked 0))
        (is (null (nle::await-gateway-store
                   store
                   :state (lambda () (incf asked) (unit-state "active" "running"))
                   :held-p (lambda (path) (declare (ignore path)) nil)
                   :seconds 5 :pause 0.01)) "no gateway supervises the store: nothing to wait for")
        (is (zerop asked) "and no service manager is asked")))))
