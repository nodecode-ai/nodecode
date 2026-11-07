;;;; rest-test.lisp --- Slack Web API plans, the thread book, the envelope.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun slack-target (channel &optional message-id)
  (list :channel-id channel :thread-id nil :message-id message-id))

(deftest channel-slack-replies-land-in-the-ask-thread ()
  (let ((book (ncs:make-thread-book)))
    ;; In a channel an answer hangs off its ask; in a DM it is the next line.
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "1.1")))
    (is (null (ncs:reply-thread book (slack-target "D1") "1.1")))
    (is (null (ncs:reply-thread book (slack-target "C1") nil)))
    ;; A reply to a message inside a thread goes in that thread, DM or not.
    (ncs:note-thread-message book "C1" "2.2" "1.1")
    (ncs:note-thread-message book "D1" "2.3" "1.2")
    (ncs:note-thread-message book "C1" "1.1" "1.1")
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "2.2")))
    (is (equal "1.2" (ncs:reply-thread book (slack-target "D1") "2.3")))
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "1.1")))
    ;; Posting in a thread is what makes the bot a speaker there.
    (is (not (ncs:bot-thread-p book "C1" "1.1")))
    (ncs:message-plan book (slack-target "C1") (first-chunk "hi") :reply-to "2.2")
    (is (ncs:bot-thread-p book "C1" "1.1"))
    (is (not (ncs:bot-thread-p book "C2" "1.1")))))

(deftest channel-slack-message-is-a-markdown-block ()
  ;; The kit spells a post's mentions into its text (the platform's MENTION)
  ;; and hands its controls to the chunk they ride: a plan draws what it is
  ;; handed, and never adds a mention of its own.
  (let* ((book (ncs:make-thread-book))
         (chunks (nck:split-text-chunks (format nil "<@U7> # Title~%~%**bold** and `code`") 12000))
         (plan (ncs:message-plan book (slack-target "C1") (first chunks) :reply-to "1.1"
                                 :mentions '("U7")
                                 :controls '((("Stop" "nck:stop:s" :danger)
                                              ("Tools" "nck:tools:s" :secondary))))))
    (is-plan plan :method "POST" :path "/chat.postMessage" :label "send_message" :retry t
                  "channel" "C1" "thread_ts" "1.1")
    (let ((blocks (plan-field plan "blocks")))
      (is (equal "markdown" (nlk:json-value (aref blocks 0) :string "type")))
      (is (equal (format nil "<@U7> # Title~%~%**bold** and `code`")
                 (nlk:json-value (aref blocks 0) :string "text")) "the one mention is the kit's")
      (is-present (buttons (nlk:json-array (aref blocks 1) "elements")) "the controls ride"
        (is (= 2 (length buttons)))
        (is (equal "danger" (nlk:json-value (aref buttons 0) :string "style")))
        (is (equal "nck:stop:s" (nlk:json-value (aref buttons 0) :string "value")))
        (is (null (nlk:json-value (aref buttons 1) :string "style")))
        (is (not (equal (nlk:json-value (aref buttons 0) :string "action_id")
                        (nlk:json-value (aref buttons 1) :string "action_id"))))))
    ;; The notification's words are the text on one line.
    (is (equal (format nil "<@U7> # Title **bold** and `code`") (plan-field plan "text"))))
  ;; A later chunk carries the controls handed to it, in the same thread.
  (let* ((chunks (nck:split-text-chunks (format nil "one~%two") 4))
         (plan (ncs:message-plan (ncs:make-thread-book) (slack-target "C1") (second chunks)
                                 :reply-to "1.1" :mentions '("U7") :controls '(("Stop" "x" :danger)))))
    (is-plan plan :label "send_message_chunk_2_of_2" "thread_ts" "1.1")
    (is (= 2 (length (plan-field plan "blocks"))))
    (is (equal "two" (nlk:json-value (aref (plan-field plan "blocks") 0) :string "text")))))

(deftest channel-slack-a-long-answer-keeps-its-buttons-and-one-mention ()
  ;; Through the kit's own post: the first chunk opens with the mention, the
  ;; last carries the buttons -- an answer longer than one message loses
  ;; neither, and says neither twice.
  (let* ((executor (scripted-executor (reply 200 "ok" t "ts" "1.1") (reply 200 "ok" t "ts" "1.2")))
         (host (nck:make-channel-host :platform (ncs:slack-platform) :executor executor
                                      :text-limit 12000))
         (text (format nil "~a~%~%~a" (make-string 11000 :initial-element #\a)
                       (make-string 3000 :initial-element #\b))))
    (nck:post-message host (slack-target "C1") text :reply-to "0.9" :mentions '("U7")
                                                    :controls '((("Yes" "y" :primary) ("No" "n"))))
    (let ((plans (recorded-plans executor)))
      (is (= 2 (length plans)) "two chunks")
      (flet ((words (plan) (nlk:json-value (aref (plan-field plan "blocks") 0) :string "text")))
        (is (uiop:string-prefix-p "<@U7> a" (words (first plans))) "the mention opens the answer")
        (is (= 1 (count #\@ (format nil "~{~a~}" (mapcar #'words plans)))) "one mention")
        (is (= 1 (length (plan-field (first plans) "blocks"))) "no buttons on the first chunk")
        (is (= 2 (length (plan-field (second plans) "blocks"))) "the buttons ride the last")))))

(deftest channel-slack-answers-keep-the-tables-slack-draws ()
  ;; Slack's markdown block draws a table as its own table block, up to 100
  ;; rows (the header among them) and 20 columns -- past them it drops the
  ;; rest unsaid -- and refuses a message whose tables' cells pass 10,000
  ;; characters. A Slack answer keeps a table inside those as written; one
  ;; past them never posts cut.
  (let ((nck::*table-image-renderer* (constantly nil))
        (drawn (nck:platform-tables (ncs:slack-platform)))
        (table (format nil "| window | spend |~%|---|---|~%| 28h | ¥102 |"))
        (wide (format nil "|~{ h~d |~}~%|~{~a|~}~%|~{ ~d |~}"
                      (loop for i below 21 collect i) (loop repeat 21 collect "---")
                      (loop for i below 21 collect i))))
    (is (equal '(:rows 100 :columns 20 :characters 10000) drawn))
    (is (equal table (nck::answer-tables-as-images table drawn)) "a table Slack draws posts as written")
    (is (search "```" (nck::answer-tables-as-images wide drawn)) "21 columns post as a fence, never cut")))

(deftest channel-slack-card-controls-are-buttons-of-words ()
  ;; The kit's running card sends Stop beside Details as one row; Slack
  ;; refuses the whole post (invalid_blocks) unless every button's text is
  ;; words and its value the data.
  (flet ((buttons (controls)
           (let ((blocks (ncs::message-blocks "card" controls)))
             (if (> (length blocks) 1)
                 (coerce (nlk:json-array (aref blocks 1) "elements") 'list)
                 '())))
         (words (button) (nlk:json-value (nlk:json-value button :object "text") :string "text"))
         (value (button) (nlk:json-value button :string "value")))
    (let ((row (buttons '((("Stop" "nck:stop:s" :danger) ("Details" "nck:details:s" :secondary))))))
      (is (equal '("Stop" "Details") (mapcar #'words row)))
      (is (equal '("nck:stop:s" "nck:details:s") (mapcar #'value row)))
      (is (equal "danger" (nlk:json-value (first row) :string "style"))))
    (is (equal '("Details") (mapcar #'words (buttons '(("Details" "nck:details:s" :secondary))))))
    (is (equal '("Go") (mapcar #'words (buttons '((("Go" "g" :primary) ("Off" "o" :secondary t)))))))
    (is (= 75 (length (words (first (buttons (list (list (make-string 90 :initial-element #\a) "x" nil))))))))
    (is (null (buttons :clear)))
    (is (= 1 (length (ncs::message-blocks "card" '((("Off" "o" :secondary t)))))) "a row of only disabled buttons is no block")))

(deftest channel-slack-menus-and-lines-are-block-kit ()
  ;; A kit menu is a static select, its current option the one shown picked;
  ;; a kit line (NCK::LINE-CHOICE) is a section, its button beside its words.
  (let* ((blocks (ncs::message-blocks
                  "## Card"
                  (list '(:menu "Select effort" (("low" "nck:p:1" "" nil) ("high" "nck:p:2" "Think hard." t)))
                        (list :menu "Too long" (list (list "x" (make-string 151 :initial-element #\x) nil nil)))
                        '(:row "🟩" "Memory" "3 <keys>" ("Open" "nck:p:3" :secondary nil))
                        '(:row nil "Said" nil nil)
                        '(:row nil "" nil ("Run" "nck:p:4" :primary nil)))))
         (select (aref (nlk:json-array (aref blocks 1) "elements") 0)))
    (is (= 5 (length blocks)) "words, a menu, three lines; a menu with no option Slack takes is none")
    (is (equal "static_select" (nlk:json-value select :string "type")))
    (is (equal "Select effort" (nlk:json-value select :string "placeholder" "text")))
    (is (equal '("nck:p:1" "nck:p:2")
               (map 'list (lambda (o) (nlk:json-value o :string "value")) (nlk:json-array select "options"))))
    (is (null (nlk:json-value (aref (nlk:json-array select "options") 0) :object "description")) "an empty description is none")
    (is (equal "nck:p:2" (nlk:json-value select :string "initial_option" "value")))
    (let ((line (aref blocks 2)))
      (is (equal "section" (nlk:json-value line :string "type")))
      (is (equal "🟩 *Memory* · 3 &lt;keys&gt;" (nlk:json-value line :string "text" "text")))
      (is (equal "nck:p:3" (nlk:json-value line :string "accessory" "value"))))
    (is (equal "*Said*" (nlk:json-value (aref blocks 3) :string "text" "text")))
    (is (null (nlk:json-value (aref blocks 3) :object "accessory")) "a line that runs nothing")
    (is (equal "actions" (nlk:json-value (aref blocks 4) :string "type")) "a button with no words")))

(deftest channel-slack-a-footer-is-small-grey-words ()
  ;; The mark a pressed card keeps (NCK::ANSWER-CARD) is a context block
  ;; under its words; the notification says both.
  (let* ((body (ncs::slack-answer-body "Ship it?" "Answered: yes"))
         (blocks (ncs::message-blocks body nil)))
    (is (= 2 (length blocks)))
    (is (equal "Ship it?" (nlk:json-value (aref blocks 0) :string "text")))
    (is (equal "context" (nlk:json-value (aref blocks 1) :string "type")))
    (is (equal "Answered: yes"
               (nlk:json-value (aref (nlk:json-array (aref blocks 1) "elements") 0) :string "text")))
    (is (equal "Ship it? Answered: yes" (ncs::notification-text body)))))

(deftest channel-slack-press-answer-reaches-the-presser-alone ()
  ;; A card's Details press answers in words only its presser sees, in the
  ;; thread the card lives in; a press the kit answers without words plans
  ;; nothing.
  (let ((plans (ncs::ack-plan '(:id "6.6" :user-id "U1" :channel-id "C1" :thread-id "4.4")
                              :text (format nil "Ran 2 commands~%Waited 30s"))))
    (is (= 1 (length plans)))
    (is-plan (first plans) :method "POST" :path "/chat.postEphemeral" :label "control_ack"
                           "channel" "C1" "user" "U1" "thread_ts" "4.4"
                           "text" "Ran 2 commands Waited 30s")
    (is (equal (format nil "Ran 2 commands~%Waited 30s")
               (nlk:json-value (aref (plan-field (first plans) "blocks") 0) :string "text"))))
  (is (null (ncs::ack-plan '(:id "6.6" :user-id "U1" :channel-id "D1")))))

(deftest channel-slack-edit-delete-status-reaction ()
  (let ((edit (ncs:edit-plan (slack-target "C1") "5.5" "done" :controls :clear)))
    (is-plan edit :path "/chat.update" :retry nil "channel" "C1" "ts" "5.5")
    (is (= 1 (length (plan-field edit "blocks"))) "an edit sends the blocks whole: no buttons"))
  (is-plan (ncs:edit-plan (slack-target "C1") "5.5" "done" :retry t) :retry t)
  (is-plan (ncs:delete-plan (slack-target "C1") "5.5") :path "/chat.delete" :retry t
           "channel" "C1" "ts" "5.5")
  (let ((book (ncs:make-thread-book)))
    (is-plan (ncs:typing-plan book (slack-target "C1" "1.1"))
             :path "/assistant.threads.setStatus" :label "typing_indicator"
             "channel_id" "C1" "thread_ts" "1.1" "status" "is working...")
    (is (null (ncs:typing-plan book (slack-target "D1" "1.1"))) "a DM answers in the channel, where Slack shows no status"))
  (let ((plans (ncs:reaction-plans (slack-target "C1") "5.5" "👀")))
    (is (= 1 (length plans)))
    (is-plan (first plans) :path "/reactions.add" "channel" "C1" "timestamp" "5.5" "name" "eyes"))
  (let ((plans (ncs:reaction-plans (slack-target "C1") "5.5" nil :previous "👀")))
    (is-plan (first plans) :path "/reactions.remove" "name" "eyes"))
  ;; The kit's done and failed marks are Slack names too, not glyphs.
  (let ((plans (ncs:reaction-plans (slack-target "C1") "5.5" nck:+reaction-done+ :previous nck:+reaction-seen+)))
    (is (equal '("eyes" "white_check_mark") (mapcar (lambda (plan) (plan-field plan "name")) plans))))
  (is-plan (second (ncs:reaction-plans (slack-target "C1") "5.5" nck:+reaction-failed+ :previous "👀"))
           :path "/reactions.add" "name" "x")
  (is (null (ncs:reaction-plans (slack-target "C1") "5.5" "👀" :previous "👀"))))

(deftest channel-slack-slash-command-answers-through-its-url ()
  (let ((command (ncs:slack-command-candidate
                  (nlk:json-object "text" "help" "channel_id" "C1" "user_id" "U1"
                                   "response_url" "https://hooks.slack.com/commands/1"))))
    (is-plan (ncs:respond-plan command "the answer")
             :method "POST" :path "https://hooks.slack.com/commands/1"
             "response_type" "in_channel" "text" "the answer"))
  (is (null (ncs:respond-plan (ncs:slack-message-candidate (slack-message "C1" "1.1" "hi"))
                              "the answer")) "a message is answered as a reply"))

(deftest channel-slack-a-press-answers-through-its-url ()
  ;; A press's answer becomes the message pressed -- replace_original, which
  ;; reaches an ephemeral panel too -- or, private, a word to the presser
  ;; alone; the hold is the socket's acknowledgement.
  (let ((press (nlk:json-object "text" "/think high"
                                "source" (nlk:json-object "platform" "slack" "channel_id" "C1"
                                                          "pressed" t "reply_to_message_id" "5.5"
                                                          "response_url" "https://hooks.slack.com/actions/1"))))
    (is (eq ncs::+socket-hold+ (ncs:respond-plan press nil)))
    (is-plan (ncs:respond-plan press "## Effort" :controls '(("Back" "b")))
             :path "https://hooks.slack.com/actions/1" "replace_original" t "response_type" "in_channel")
    (let ((private (ncs:respond-plan press "only you" :private t)))
      (is-plan private "response_type" "ephemeral")
      (is (null (plan-field private "replace_original")) "a private word leaves the card be"))))

(deftest channel-slack-a-cards-pictures-upload-once-and-draw ()
  ;; A card's pictures are image blocks naming files Slack holds: each
  ;; uploaded once, shared nowhere, then named on every edit; one that will
  ;; not upload, or that Slack will not draw, is left out and the card lands.
  (with-temp-file (path :contents "png bytes" :type "png")
    (clrhash ncs::*card-pictures*)
    (let* ((recording (scripted-executor (reply 200 "ok" t "upload_url" "https://files.slack.com/upload/v1/p"
                                                "file_id" "F9")
                                         (reply 200)
                                         (reply 200 "ok" t "files" (vector (nlk:json-object "id" "F9")))
                                         (reply 200 "ok" t "ts" "8.8")
                                         (reply 200 "ok" t "ts" "8.8")))
           (executor (ncs:wrap-slack-executor recording))
           (card (list :state :working :images (list (list "step-1.png" (namestring path) "the chart"))))
           (post (ncs:message-plan (ncs:make-thread-book) (slack-target "C1") (first-chunk "Working")
                                   :reply-to "1.1" :card card :controls '(("Stop" "s"))))
           (result (nck:execute-plan executor post)))
      (is (nck:execution-ok-p result))
      (is (equal "send_card" (nck:request-plan-audit-label post)))
      (let* ((steps (recorded-plans recording))
             (sent (fourth steps))
             (blocks (plan-field sent "blocks")))
        (is (equal '("/files.getUploadURLExternal" "https://files.slack.com/upload/v1/p"
                     "/files.completeUploadExternal" "/chat.postMessage")
                   (mapcar #'nck:request-plan-path steps)))
        (is (null (plan-field (third steps) "channel_id")) "a picture is shared nowhere: the bot's own")
        (is (equal '("markdown" "image" "actions") (map 'list (lambda (block) (gethash "type" block)) blocks)) "the picture under the words, above the buttons")
        (is (equal "F9" (nlk:json-value (aref blocks 1) :string "slack_file" "id")))
        (is (equal "the chart" (nlk:json-value (aref blocks 1) :string "alt_text")))
        (is (null (plan-field sent ncs::+pictures-key+)) "the pictures member is never sent"))
      (nck:execute-plan executor (ncs:edit-plan (slack-target "C1") "8.8" "Working" :card card))
      (let ((steps (recorded-plans recording)))
        (is (= 5 (length steps)) "the next edit uploads nothing again")
        (is (equal "/chat.update" (nck:request-plan-path (fifth steps))))
        (is (equal "F9" (nlk:json-value (aref (plan-field (fifth steps) "blocks") 1) :string "slack_file" "id"))))))
  (with-temp-file (path :contents "png bytes" :type "png")
    (clrhash ncs::*card-pictures*)
    (let* ((recording (scripted-executor (reply 200 "ok" nil "error" "missing_scope")
                                         (reply 200 "ok" t "ts" "9.9")
                                         (reply 200 "ok" t "ts" "9.9")))
           (executor (ncs:wrap-slack-executor recording))
           (card (list :state :working :images (list (list "step-1.png" (namestring path) nil)))))
      (is (nck:execution-ok-p (nck:execute-plan executor (ncs:edit-plan (slack-target "C1") "9.9" "Working" :card card))) "a picture that will not upload leaves the card to land without it")
      (nck:execute-plan executor (ncs:edit-plan (slack-target "C1") "9.9" "Working" :card card))
      (let ((steps (recorded-plans recording)))
        (is (equal '("/files.getUploadURLExternal" "/chat.update" "/chat.update")
                   (mapcar #'nck:request-plan-path steps)) "a refused upload is not tried again")
        (is (equal '("markdown") (map 'list (lambda (block) (gethash "type" block))
                                      (plan-field (third steps) "blocks")))))))
  (with-temp-file (path :contents "png bytes" :type "png")
    (clrhash ncs::*card-pictures*)
    (setf (gethash (namestring path) ncs::*card-pictures*) (list :id "F7" :strikes 0))
    (let* ((recording (scripted-executor (reply 200 "ok" nil "error" "invalid_blocks")
                                         (reply 200 "ok" t "ts" "9.9")))
           (executor (ncs:wrap-slack-executor recording))
           (card (list :state :working :images (list (list "step-1.png" (namestring path) nil)))))
      (is (nck:execution-ok-p (nck:execute-plan executor (ncs:edit-plan (slack-target "C1") "9.9" "Working" :card card))) "refused for its picture, the card goes again without it")
      (is (equal '("image") (remove "markdown" (map 'list (lambda (block) (gethash "type" block))
                                                   (plan-field (first (recorded-plans recording)) "blocks"))
                                    :test #'equal)))
      (is (equal 1 (getf (gethash (namestring path) ncs::*card-pictures*) :strikes)) "the refusal is counted"))))

(deftest channel-slack-files-ride-the-upload-steps ()
  ;; Slack takes a file in three calls: a link minted, the bytes sent to it,
  ;; one share. An answer's files go under its words, in its thread, and the
  ;; answer's own ts is what the post answers.
  (with-temp-file (path :contents "png bytes" :type "png")
    (let* ((recording (scripted-executor (reply 200 "ok" t "ts" "7.7")
                                         (reply 200 "ok" t "upload_url" "https://files.slack.com/upload/v1/x"
                                                "file_id" "F1")
                                         (reply 200)
                                         (reply 200 "ok" t "files" (vector (nlk:json-object "id" "F1")))))
           (executor (ncs:wrap-slack-executor recording))
           (plan (ncs:message-plan (ncs:make-thread-book) (slack-target "C1") (first-chunk "a chart")
                                   :reply-to "1.1" :files (list path)))
           (result (nck:execute-plan executor plan)))
      (is (equal "UPLOAD" (nck:request-plan-method plan)))
      (is (nck:execution-ok-p result))
      (is (equal "7.7" (ncs:slack-message-id (nck:execution-body result))) "the answer's ts")
      (let ((steps (recorded-plans recording)))
        (is (equal '("/chat.postMessage" "/files.getUploadURLExternal"
                     "https://files.slack.com/upload/v1/x" "/files.completeUploadExternal")
                   (mapcar #'nck:request-plan-path steps)))
        (is-plan (second steps) "filename" (file-namestring path) "length" 9)
        (is (equal (pathname path) (plan-field (third steps) "file")) "the bytes go as the file part")
        (is-plan (fourth steps) "channel_id" "C1" "thread_ts" "1.1")
        (is (equal "F1" (nlk:json-value (aref (plan-field (fourth steps) "files") 0) :string "id"))))))
  ;; A file posted alone (PLAN-FILE) carries its caption; a refused step is
  ;; the plan's failure.
  (with-temp-file (path :contents "ogg" :type "ogg")
    (let* ((recording (scripted-executor (reply 200 "ok" nil "error" "missing_scope")))
           (plan (ncs:file-plan (ncs:make-thread-book) (slack-target "D1") path :content "Voice reply"
                                                                                  :voice '(:seconds 2)))
           (result (nck:execute-plan (ncs:wrap-slack-executor recording) plan)))
      (is (equal "send_voice" (nck:request-plan-audit-label plan)))
      (is (equal "Voice reply" (plan-field plan "initial_comment")))
      (is (not (nck:execution-ok-p result)))
      (is (search "missing_scope" (nck:execution-error result))))))

(deftest channel-slack-envelope-says-ok ()
  ;; Slack refuses with status 200: the envelope's ok decides.
  (let ((executor (ncs:wrap-slack-executor
                   (scripted-executor (reply 200 "ok" nil "error" "channel_not_found")
                                      (reply 200 "ok" nil "error" "already_reacted")
                                      (reply 200 "ok" t "ts" "9.9")))))
    (let ((refused (nck:execute-plan executor (nck:rest-plan "POST" "/chat.postMessage"
                                                             "send_message" nil 5))))
      (is (not (nck:execution-ok-p refused)))
      (is (search "channel_not_found" (nck:execution-error refused))))
    (let ((settled (nck:execute-plan executor (nck:rest-plan "POST" "/reactions.add"
                                                             "add_reaction" nil 5))))
      (is (nck:execution-ok-p settled) "already on is what was asked"))
    (let ((posted (nck:execute-plan executor (nck:rest-plan "POST" "/chat.postMessage"
                                                            "send_message" nil 5))))
      (is (equal "9.9" (ncs:slack-message-id (nck:execution-body posted)))))
    (is (nck:execution-ok-p (nck:execute-plan executor nil)) "a status beat Slack cannot show is done")))

(deftest channel-slack-address-carries-the-channel ()
  (is (equal "C1:5.5" (ncs:slack-address (slack-target "C1") "5.5")))
  (is (not (equal (ncs:slack-address (slack-target "C1") "5.5")
                  (ncs:slack-address (slack-target "C2") "5.5")))))
