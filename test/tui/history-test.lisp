;;;; history-test.lisp --- prompt-history content and bounded projection.
(in-package #:nodecode.test)

(deftest history-projection-keeps-content-order-exclusions-and-read-bound (with-temp-store ())
  (let ((unicode (format nil "  café 中 😀~%next~cend~cafter-nul" #\Tab #\Null)))
    (flet ((put (session message &key disposition prompt-id)
             (ensure-durable-session session)
             (nlk::with-event-owner 'nlk:admit-turn
               (nlk::with-store-transaction (handle)
                 (nlk::append-event-row handle session "turn.input_committed"
                   (list :message message :disposition disposition :prompt-id prompt-id
                         :ignored (make-string 100000 :initial-element #\x)))))))
      (put "discord-old" "old")
      (put "telegram-parts" (nlk:json-object "content"
                             (vector (nlk:json-object "type" "text" "text" "first")
                                     (nlk:json-object "type" "image" "text" "not text")
                                     (nlk:json-object "type" "text" "text" " second"))))
      (put "cron-object" (nlk:json-object "content" unicode))
      (put "local" unicode)
      (put "excluded-queue" "queued" :disposition "follow-up")
      (put "excluded-id" "reflected" :prompt-id "experience:one")
      (put "experience-session" "reflected")
      (put "excluded-prefix" "[experience] reflection")
      (put "empty" "")
      (put "null" :null)
      (put "absent-content" (nlk:json-object "other" "not content"))
      (put "numeric" 42)
      (put "false" nil)
      (is (equal (list (list "tui" unicode) (list "cron" unicode)
                       '("telegram" "first second") '("discord" "old"))
                 (nle::prompt-history)))
      (is (null (nle::prompt-history :limit 9)) "the limit precedes exclusions, as before")
      (is (equal (list (list "tui" unicode)) (nle::prompt-history :limit 10)))
      (put "latest" "newly committed")
      (is (equal '(("tui" "newly committed")) (nle::prompt-history :limit 1)) "the uncached read sees new facts"))))

(deftest history-string-projection-does-not-decode-event-envelopes (with-session-store ("projection"))
  (nlk:admit-turn "projection" "projection-command" "verbatim prompt")
  (with-stubbed-fdefinitions ((nlk:decode-json (&rest args)
                                (error "plain prompt history must not decode the event envelope")))
    (is (equal '(("tui" "verbatim prompt")) (nle::prompt-history)))))

(deftest sql-text-projection-preserves-large-unicode-and-nul (with-temp-store ())
  (let* ((text (concatenate 'string (make-string 100000 :initial-element #\a)
                           (string #\Null) "café 中 😀" (string #\Newline)))
         ;; Bind bytes: cl-sqlite's string binder itself stops at NUL.
         (result (caar (nlk::sql-rows (nlk::require-open-store) "select cast(? as text)"
                                     (sb-ext:string-to-octets text :external-format :utf-8)))))
    (is (string= text result))
    (is (equal (type-of text) (type-of result)))))
