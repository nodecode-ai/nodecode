;;;; bodies-test.lisp --- the exact request bodies, kept as deltas
;;;; (store/bodies.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun history-body (rounds &key edit)
  "A request body over ROUNDS rounds of history as a lane lays one out: a
fixed head, one tool output a round, a fixed tail of tools; EDIT sets round
1's output aside, the way an eviction breaks a prefix."
  (nlk:utf-8-octets
   (format nil "{\"model\":\"m\",\"input\":[{\"role\":\"system\",\"content\":\"~a\"}~{,~a~}],\"tools\":[\"~a\"]}"
           (make-string 3000 :initial-element #\s)
           (loop for n below rounds
                 collect (format nil "{\"type\":\"function_call_output\",\"output\":\"~a\"}"
                                 (if (and edit (= n 1))
                                     "set aside"
                                     (format nil "~{line ~d of round ~d — café; ~}"
                                             (loop for line below 40 append (list line n))))))
           (make-string 2000 :initial-element #\t))))

(deftest a-request-body-delta-rebuilds-from-the-body-before-it ()
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8)))
        (three (history-body 3))
        (four (history-body 4)))
    (flet ((kept (base target)
             (let ((delta (nlk::encode-delta base target))
                   (out (make-array (length target) :element-type '(unsigned-byte 8))))
               (is (= (length target) (nlk::apply-delta delta base (length base) out)))
               (is (equalp target out) "rebuilt byte for byte")
               (length delta))))
      (is (< (kept empty three) (length three)) "a body kept whole shrinks where it repeats itself")
      (is (< (kept three four) (+ 100 (- (length four) (length three)))) "a round costs what it added")
      (is (< (kept four (history-body 4 :edit t)) 100) "an early edit costs the edit, not the rest")
      (is (< (kept four four) 10) "the same body is one copy")
      (kept four three)
      (kept three empty))))

(deftest kept-request-bodies-rebuild-exactly-and-the-oldest-go-past-the-cap (with-temp-store ())
  (let ((bodies (loop for n from 1 to 4 collect (history-body n))))
    (loop for body in bodies for round from 0
          do (nlk:keep-request-body "a" "t1" round body))
    ;; A session's first body is kept against the store's newest (a fork's
    ;; parent), and a turn's first against its own session's last: here not
    ;; the body kept last, so it is rebuilt from the store.
    (nlk:keep-request-body "b" "t2" 0 (history-body 5))
    (nlk:keep-request-body "a" "t3" 0 (history-body 4 :edit t))
    (loop for body in bodies for round from 0
          do (multiple-value-bind (octets previous from) (nlk:kept-request-body "a" "t1" round)
               (is (equalp body octets) "rebuilt byte for byte")
               (is (equal (nlk:sha256-text body) (nlk:sha256-text octets)))
               (is (equalp (and (plusp round) (nth (1- round) bodies)) previous))
               (is (equal (and (plusp round) (list "a" "t1" (1- round))) from))))
    (is (equal '("a" "t1" 3) (nth-value 2 (nlk:kept-request-body "b" "t2" 0))))
    (is (equal '("a" "t1" 3) (nth-value 2 (nlk:kept-request-body "a" "t3" 0))))
    (is (equalp (history-body 4 :edit t) (nlk:kept-request-body "a" "t3" 0)))
    (is (null (nlk:kept-request-body "a" "t1" 9)) "a round never kept")
    (multiple-value-bind (bytes count size) (nlk:request-bodies-size)
      (is (= 6 count))
      (is (< (* 3 bytes) size) "kept in a fraction of what was sent"))
    ;; Past the chain bound a body is kept whole and a new chain starts; past
    ;; the cap the oldest chain goes whole, never a row its later rows need.
    (let* ((nlk::*request-chain-bytes* 1)
           (whole (history-body 6))
           (alone (length (nlk::encode-delta (make-array 0 :element-type '(unsigned-byte 8)) whole))))
      (nlk:keep-request-body "c" "t4" 0 whole :cap alone)
      (is (null (nth-value 1 (nlk:kept-request-body "c" "t4" 0))) "kept whole")
      (is (equalp whole (nlk:kept-request-body "c" "t4" 0)))
      (is (null (nlk:kept-request-body "a" "t1" 0)) "the oldest chain went")
      (is (null (nlk:kept-request-body "b" "t2" 0)) "with every row kept against it")
      (is (= 1 (nth-value 1 (nlk:request-bodies-size)))))
    ;; A rebuild that does not come back to its SHA-256 refuses.
    (sqlite:execute-non-query (nlk::require-open-store)
                              "update request_bodies set delta = ?"
                              (nlk::encode-delta (make-array 0 :element-type '(unsigned-byte 8))
                                                 (history-body 2)))
    (is (signals-error error (nlk:kept-request-body "c" "t4" 0)))))

(deftest forgotten-sessions-take-their-bodies-and-leave-the-rest-whole (with-temp-store ())
  ;; B's first body is kept against A's last, C's against B's: forgetting A
  ;; keeps B's first whole, and B's rows stay rooted where the cap finds them.
  (loop for round below 3 do (nlk:keep-request-body "a" "t1" round (history-body (1+ round))))
  (nlk:keep-request-body "b" "t2" 0 (history-body 4))
  (nlk:keep-request-body "b" "t2" 1 (history-body 5))
  (nlk:keep-request-body "c" "t3" 0 (history-body 6))
  (flet ((forget (&rest ids)
           (nlk::with-store-transaction (handle) (nlk::forget-request-bodies handle ids)))
         (chains-hold ()
           (null (nlk::sql-rows (nlk::require-open-store)
                                "select r.id from request_bodies r left join request_bodies b on b.id = r.base
                                  where (r.base is null and r.root <> r.id)
                                     or (r.base is not null and (b.id is null or b.root <> r.root))"))))
    (forget "a")
    (is (null (nlk:kept-request-body "a" "t1" 0)))
    (is (null (nth-value 1 (nlk:kept-request-body "b" "t2" 0))) "kept whole now")
    (is (equalp (history-body 5) (nlk:kept-request-body "b" "t2" 1)))
    (is (equalp (history-body 6) (nlk:kept-request-body "c" "t3" 0)))
    (is (chains-hold) "every base stands, and shares its row's root")
    (forget "b")
    (is (equalp (history-body 6) (nlk:kept-request-body "c" "t3" 0)))
    (is (= 1 (nth-value 1 (nlk:request-bodies-size))))
    (is (chains-hold))))
