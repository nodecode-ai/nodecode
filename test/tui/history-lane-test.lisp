;;;; history-lane-test.lisp --- the fold's history lane: appended in place, stamped once.
(in-package #:nodecode.evolved)

(nodecode.test:deftest nodecode.test::history-lane-appends-and-stamps-past-its-mark ()
  "A row is stamped with the position of the first fact folded after it
appears, and the history lane keeps every row in order, across the fresh
lists a removal or a pushed divider writes."
  (let ((state (make-hash-table :test 'equal))
        (expected '()))
    (flet ((fact (position &optional (text (format nil "row ~d" position)))
             ;; One folded fact: it produces a row, then the fold stamps.
             (let ((row (make-transcript-row :kind :text :role "assistant" :text text :hydrated t)))
               (append-tail-row state row)
               (stamp-unpositioned-rows state position)
               (setf expected (append expected (list row)))
               row))
           (check ()
             (let ((rows (transcript-rows state)))
               (assert (= (length expected) (length rows)))
               (assert (every #'eq expected rows))
               (assert (every #'transcript-row-log-position rows)))))
      ;; 40 facts: 24 rows promoted out of the 16-row tail into history.
      (loop for position from 1 to 40 do (fact position))
      (check)
      (assert (= 24 (length (gethash "history_rows" state))))
      (loop for row in expected for position from 1
            do (assert (= position (transcript-row-log-position row))))
      ;; A removal writes a fresh lane; what follows still lands on its end.
      (let ((gone (nth 4 expected)))
        (remove-transcript-rows state (lambda (row) (eq row gone)))
        (setf expected (remove gone expected)))
      (loop for position from 41 to 60 do (fact position))
      (check)
      ;; A divider pushed on the front is a fresh head, and a fact without a
      ;; position stamps nothing.
      (let ((divider (make-transcript-row :kind :eviction-divider :role "system" :hydrated t
                                          :log-position 0)))
        (push divider (gethash "history_rows" state))
        (push divider expected))
      (let ((late (make-transcript-row :kind :text :role "assistant" :text "late" :hydrated t)))
        (append-tail-row state late)
        (stamp-unpositioned-rows state nil)
        (assert (null (transcript-row-log-position late)))
        (setf expected (append expected (list late)))
        (fact 61)
        (assert (= 61 (transcript-row-log-position late))))
      ;; Settling moves the whole tail onto history in place.
      (settle-tail-rows state)
      (assert (null (gethash "tail_rows" state)))
      (check)
      (loop for position from 62 to 70 do (fact position))
      (settle-tail-rows state)
      (check)
      (assert (= 70 (transcript-row-log-position (car (last (gethash "history_rows" state)))))))))
