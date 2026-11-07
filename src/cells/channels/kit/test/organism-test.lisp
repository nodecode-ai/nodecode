;;;; organism-test.lisp --- the in-process seams an adapter rides.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What client-test proved over the loopback wire, proved in-process: a
;;;; session made durable through the kernel API, a prompt put in through
;;;; NLE:SUBMIT, and the turn's facts read back off a (NLE:HOOK :FRAME ...)
;;;; chain with the kit's op readers. No gateway is started: the frames are
;;;; published by the turn worker whether or not a listener exists.

(in-package #:nodecode.test)

(deftest channel-organism-ensure-session-is-idempotent-and-forks (with-temp-store ())
  (is (nck:ensure-session "chan-room") "ensure mints the session")
  (is (nlk:session-exists-p "chan-room"))
  (is (nlk:on-standby-p "chan-room") "and it stands by until its first block")
  (is (nck:ensure-session "chan-room") "re-ensure is idempotent")
  (is (nck:ensure-session "chan-room-lane" :parent "chan-room"))
  ;; Nothing durable names either one yet; the lane's first block is what
  ;; makes them real, and the parent comes with it through the lane's own
  ;; identity.
  (nlk:materialize-standby-session "chan-room-lane")
  (is (equal "chan-room"
             (nlk:durable-session-parent (nlk:find-session "chan-room-lane"))))
  (is (nck:ensure-session "chan-room-lane" :parent "chan-other")))

(deftest channel-organism-submit-and-frame-readers-roundtrip (with-temp-store ())
  (with-stub-provider ((wire-message "assistant" :content "stub says hi"))
    (nck:ensure-session "chan-e2e")
    (let ((lock (bt2:make-lock :name "chan-e2e-facts"))
          (facts '())
          (hook-thread nil))
      (nle:hook :frame "channel-test"
                (lambda (op next)
                  (when (equal "chan-e2e" (nck:frame-session-id op))
                    (multiple-value-bind (kind payload metadata turn-id)
                        (nck:frame-fact op)
                      (when kind
                        (bt2:with-lock-held (lock)
                          (setf hook-thread (bt2:current-thread))
                          (push (list kind payload metadata turn-id)
                                facts)))))
                  (funcall next op)))
      (nlk:with-cleanup ((nle:unhook :frame "channel-test"))
        (flet ((facts-of (kind)
                 (bt2:with-lock-held (lock)
                   (remove-if-not
                    (lambda (fact) (equal kind (first fact)))
                    facts))))
          (let ((admission (nle:submit "chan-e2e" "hello?"
                                       :command-id "chan-cmd-1")))
            (is (eq :started
                    admission.disposition))
            (is (stringp admission.turn-id)))
          (is (await () (facts-of "turn.assistant_message_completed")))
          (is (await () (facts-of "turn.completed")))
          (is-present (round (first (facts-of
                                     "turn.assistant_message_completed")))
            "the round fact was read"
            (destructuring-bind (kind payload metadata turn-id) round
              (declare (ignore kind))
              (is (equal "stub says hi" (nck:fact-message-content payload)))
              (is (null (nck:fact-message-reasoning payload)))
              (is (hash-table-p metadata))
              (is (stringp turn-id) "the fact carries its turn id")))
          (is (eq :duplicate (submit-disposition "chan-e2e" "hello?" :command-id "chan-cmd-1")))
          (is (= 1 (count-session-events "chan-e2e" "turn.started")))
          (is (not (eq hook-thread (bt2:current-thread)))))))))

(deftest channel-organism-frame-delta-reader ()
  (let ((delta-op (list :session-id "s" :kind "item_delta"
                        :payload (cell-json
                                  "{\"turn_id\": \"t1\",
                                    \"delta\": {\"type\": \"text\",
                                                \"text\": \"hi\"}}")))
        (fact-op (list :session-id "s" :kind "session_stream_event"
                       :payload (cell-json
                                 "{\"frame\": {\"seq\": 3, \"turn_id\": \"t1\",
                                    \"event\": {\"type\": \"turn.completed\",
                                                \"payload\": {},
                                                \"metadata\": {\"turn_id\": \"t1\"}}}}"))))
    (is-present (payload (nck:frame-delta delta-op)) "an item_delta op yields its payload"
      (is-shape payload ("turn_id" "t1") ((:string "delta" "text") "hi")))
    (is (null (nck:frame-delta fact-op)) "a fact op is not a delta")
    (is (null (nck:frame-fact delta-op)) "a delta op is not a fact")
    (multiple-value-bind (kind payload metadata turn-id) (nck:frame-fact fact-op)
      (is (equal "turn.completed" kind))
      (is (hash-table-p payload))
      (is (equal "t1" (gethash "turn_id" metadata)))
      (is (equal "t1" turn-id)))))
