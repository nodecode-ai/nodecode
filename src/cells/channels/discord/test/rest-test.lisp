;;;; rest-test.lisp --- Discord REST plan builders via recording executor.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (answer nck::request-plan) (answer-tail nck::request-plan) (chrome nck::request-plan)
            (edit nck::request-plan) (first-plan nck::request-plan) (result nck::execution)
            (tail-plan nck::request-plan))

(deftest channel-discord-hydrate-self-roles ()
  ;; The roles the bot itself holds come from its own member object in each
  ;; guild it is in. With them the mention rule admits a role pill; without
  ;; them a mention-gated room turns the ask away as mention_required.
  (let ((ncd:*self-role-ids* (make-hash-table :test #'equal))
        (executor (scripted-executor
                   ;; The executor hands a top-level array back decoded, a
                   ;; vector of objects (NCK PARSE-JSON-BODY).
                   (nck:make-scripted-response 200 (vector (nlk:json-object "id" "g1")
                                                           (nlk:json-object "id" "g2")))
                   (reply 200 "roles" (vector "456" "111"))
                   (reply 200 "roles" (vector "789")))))
    (is (equal '("111" "456" "789")
               (sort (copy-list (ncd:hydrate-self-roles executor "999"
                                                       :timeout-seconds 5))
                     #'string<)))
    (is (equal '("111" "456" "789")
               (sort (ncd:discord-self-role-ids) #'string<)))
    (is (equal '("/users/@me/guilds" "/guilds/g1/members/999"
                 "/guilds/g2/members/999")
               (mapcar #'nck:request-plan-path
                       (nck:recording-executor-plans executor)))))
  (let ((ncd:*self-role-ids* (make-hash-table :test #'equal))
        (executor (scripted-executor (nck:make-scripted-response 500 "{}"))))
    (is (eq :failed (ncd:hydrate-self-roles executor "999" :timeout-seconds 5)))
    (is (null (ncd:discord-self-role-ids)))))

(deftest channel-discord-message-plans-chunk-and-reply ()
  (let* ((target '(:channel-id "c1" :thread-id nil :message-id "m0"))
         (text (make-string 2500 :initial-element #\x))
         (chunks (nck:split-text-chunks text 2000))
         (plans (mapcar (lambda (chunk)
                          (ncd:discord-message-plan target chunk :reply-to "m0"))
                        chunks)))
    (is (= 2 (length plans)))
    (is-plan (first plans) :path "/channels/c1/messages" :label "send_message_chunk_1_of_2"
             (:string "message_reference" "message_id") "m0")
    (is-plan (second plans) :label "send_message_chunk_2_of_2" "message_reference" nil)
    (dolist (plan plans)
      (is (equalp #() (nlk:json-value (nck:request-plan-body plan)
                                      :array "allowed_mentions" "parse"))))
    (is (= 2000 (length (plan-content (first plans))))))
  ;; No reply id: no message_reference at all.
  (is-plan (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "hi"))
           :label "send_message" "message_reference" nil)
  ;; Thread targets post into the thread.
  (is-plan (ncd:discord-message-plan '(:channel-id "c1" :thread-id "t9") (first-chunk "hi"))
           :path "/channels/t9/messages")
  ;; A reply from inside a thread references the thread, the channel the POST
  ;; lands in: naming the parent gets the whole post refused (50035
  ;; REPLIES_CANNOT_REFERENCE_OTHER_CHANNEL), which lost the answer to an ask
  ;; typed inside a thread and re-planned its status line every refresh.
  (is-plan (ncd:discord-message-plan '(:channel-id "c1" :thread-id "t9") (first-chunk "hi")
                                     :reply-to "m3")
           :path "/channels/t9/messages" (:string "message_reference" "message_id") "m3"
           (:string "message_reference" "channel_id") "t9")
  (is-plan (ncd:discord-message-plan '(:channel-id "c1" :thread-id nil) (first-chunk "hi")
                                     :reply-to "m4")
           (:string "message_reference" "channel_id") "c1"))

(deftest channel-discord-messages-suppress-embeds ()
  ;; A link an answer names stays a link: every message this bot posts
  ;; carries SUPPRESS_EMBEDS (flags 4), so Discord never unfurls a preview
  ;; card the room did not ask for — the create is the only API-level
  ;; switch, and the upload's payload carries the flag too.
  (is-plan (ncd:discord-message-plan '(:channel-id "c1")
                                     (first-chunk "see https://example.com") :reply-to "m1")
           "flags" 4)
  (let ((payload (discord-upload-payload
                  (ncd:discord-file-message-plan
                   '(:channel-id "c1" :thread-id "t9")
                   #P"/tmp/pea.png" :content "see https://example.com")
                  #P"/tmp/pea.png" "see https://example.com")))
    (is (= 4 (nlk:json-value payload :integer "flags")))))

(deftest channel-discord-default-replies-stay-in-channel ()
  ;; The default target is the parent channel. A reply reference points at
  ;; the ask without creating a Discord platform thread; the channel list
  ;; therefore shows both that the bot saw the ask and where it answered.
  (let ((plan (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "answer") :reply-to "m1")))
    (is-plan plan :method "POST" :path "/channels/c1/messages"
                  "thread_name" nil "auto_archive_duration" nil
                  (:string "message_reference" "message_id") "m1")))

(deftest channel-discord-control-components ()
  ;; The kit's (LABEL DATA STYLE) buttons ride a message as Discord
  ;; components: one action row per lone button — the stop rendered danger,
  ;; Details the neutral one — and :CLEAR the empty array that retires a
  ;; settled card's buttons.
  (let* ((plan (ncd:discord-message-plan
                '(:channel-id "c1") (first-chunk "working")
                :controls (list (list "Stop" "nck:stop:discord-c1-m9" :danger)
                                (list "Details" "nck:details:discord-c1-m9"
                                      :secondary))))
         (rows (nlk:json-value (nck:request-plan-body plan)
                               :array "components")))
    (is (= 2 (length rows)) "one row per control")
    (flet ((button (index)
             (aref (nlk:json-value (aref rows index) :array "components") 0)))
      (is (= 1 (nlk:json-value (aref rows 0) :integer "type")) "an action row")
      (is-shape (button 0) ((:integer "type") = 2 "holding a button")
        ((:integer "style") = 4 "the stop is red") ((:string "label") "Stop")
        ((:string "custom_id") "nck:stop:discord-c1-m9"))
      (is-shape (button 1) ((:integer "style") = 2) ((:string "label") "Details")
        ((:string "custom_id") "nck:details:discord-c1-m9")))
    ;; A settle retires the buttons with the empty components array.
    (let* ((edit (ncd:edit-message-plan '(:channel-id "c1") "s1" "cancelled"
                                        :controls :clear))
           (components (gethash "components" edit.body)))
      (is (vectorp components) "a clear states components")
      (is (zerop (length components)) "…as none")))
  ;; A buttonless message carries no components key at all.
  (is-plan (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "hi"))
           "components" nil)
  ;; Components ride whichever chunk they are handed to: the kit hands them
  ;; to the last, under the words they answer.
  (let ((chunk (second (nck:split-text-chunks (make-string 2500 :initial-element #\x) 2000))))
    (is (gethash "components" (nck:request-plan-body
                               (ncd:discord-message-plan '(:channel-id "c1") chunk
                                                         :controls (list (list "Stop" "d" :danger))))))))

(deftest channel-discord-choice-components ()
  ;; A row of buttons sits side by side, a disabled one says so; a menu is a
  ;; string select whose options carry their data as the value, the current
  ;; one shown picked, and an option whose data Discord would refuse is left
  ;; out rather than cut into something else.
  (let* ((rows (ncd::controls-components
                (list (list (nck:choice "Zero setup" "Zero setup" :style :primary)
                            (nck:choice "Prev" "/models page 0" :disabled t))
                      (list :menu "Select provider"
                            (list (nck:menu-choice "p1" "/models p1" :description "2 models" :current t)
                                  (nck:menu-choice "p2" "/models p2")
                                  (nck:menu-choice "long" (make-string 100 :initial-element #\m)))))))
         (buttons (nlk:json-value (aref rows 0) :array "components"))
         (menu (aref (nlk:json-value (aref rows 1) :array "components") 0))
         (options (nlk:json-value menu :array "options")))
    (is (= 2 (length rows)))
    (is (= 2 (length buttons)) "a row's buttons side by side")
    (is-shape (aref buttons 0) ((:integer "style") = 1) ((:boolean "disabled") null))
    (is (equal "Zero setup" (nck:said-line (nlk:json-value (aref buttons 0) :string "custom_id"))) "a button's custom_id is its press, saying its line")
    (is-shape (aref buttons 1) ((:boolean "disabled") eq t))
    (is-shape menu ((:integer "type") = 3 "a string select") ((:string "placeholder") "Select provider")
      ((:string "custom_id") "nck:menu:1"))
    (is (= 3 (length options)) "a press is a short id, so a line past 100 characters is offered too")
    (is (every (lambda (option) (<= (length (nlk:json-value option :string "value")) 100)) options))
    (is-shape (aref options 0) ((:string "label") "p1")
      ((:string "description") "2 models") ((:boolean "default") eq t))
    (is (equal "/models p1" (nck:said-line (nlk:json-value (aref options 0) :string "value"))))
    (is (equal (make-string 100 :initial-element #\m)
               (nck:said-line (nlk:json-value (aref options 2) :string "value"))))
    (is-shape (aref options 1) ((:boolean "default") null)))
  ;; A menu with nothing to pick is no row at all.
  (is (zerop (length (ncd::controls-components (list (list :menu "Select provider" '())))))))

(deftest channel-discord-typing-plan ()
  (let ((plan (ncd:typing-plan '(:channel-id "c1" :thread-id "t9"))))
    (is-plan plan :path "/channels/t9/typing" :method "POST" :retry nil :body nil)))

(deftest channel-discord-reaction-plans ()
  ;; Discord stacks reactions, so a change removes the previous one by name
  ;; and adds the new one; the emoji rides the path percent-encoded.
  (let ((plans (ncd:reaction-plans '(:channel-id "c1" :thread-id "t9") "m9"
                                   "👀")))
    (is (= 1 (length plans)) "nothing to remove: one PUT")
    (is-plan (first plans) :method "PUT" :label "add_reaction" :body nil
             :path "/channels/t9/messages/m9/reactions/%F0%9F%91%80/@me" :retry t))
  (let ((plans (ncd:reaction-plans '(:channel-id "c1") "m9" "✍"
                                   :previous "👀")))
    (is (= 2 (length plans)) "a change is a DELETE then a PUT")
    (is-plan (first plans) :method "DELETE" :label "remove_reaction"
                           :path "/channels/c1/messages/m9/reactions/%F0%9F%91%80/@me")
    (is-plan (second plans) :method "PUT"
                            :path "/channels/c1/messages/m9/reactions/%E2%9C%8D/@me"))
  (let ((plans (ncd:reaction-plans '(:channel-id "c1") "m9" nil
                                   :previous "✍")))
    (is (= 1 (length plans)) "clearing removes the previous one alone")
    (is-plan (first plans) :method "DELETE"))
  (is-plan (first (ncd:reaction-plans '(:channel-id "c1") "m9" "a-b_c.d~e /"))
           :path "/channels/c1/messages/m9/reactions/a-b_c.d~e%20%2F/@me"))

(deftest channel-discord-read-back-path ()
  ;; Which object a write names. Truncation, not a lookup table: Discord
  ;; writes a reaction, a pin, a role and an overwrite at a sub-path of the
  ;; object that carries them, so the object is the prefix.
  (is-each (ncd:read-back-path)
    ((concatenate 'string "/channels/993101/messages/1543687385819254897"
                  "/reactions/%F0%9F%87%AE%F0%9F%87%B3/@me")
     "/channels/993101/messages/1543687385819254897"
     "a reaction reads back the message whose reactions array answers it")
    ((concatenate 'string "/channels/993101/messages/1543687385819254897" "/reactions")
     "/channels/993101/messages/1543687385819254897" "so does clearing every reaction at once")
    ("/channels/993101/messages/1543687385819254897" "/channels/993101/messages/1543687385819254897"
     "and a delete, whose proof is the 404 the read answers")
    ("/channels/993101/messages/1543687385819254897?x=1"
     "/channels/993101/messages/1543687385819254897"
     "a query string is not part of the object's address")
    ("/channels/993101/pins/1543687385819254897" "/channels/993101/pins"
     "a pin reads back the channel's pins")
    ("/channels/993101/permissions/9931" "/channels/993101"
     "an overwrite reads back the channel that holds it")
    ("/guilds/9931/members/5965/roles/77" "/guilds/9931/members/5965"
     "a role reads back the member")
    ("/guilds/9931/bans/5965" "/guilds/9931/bans/5965" nil)
    ("/channels/993101/typing" nil "a typing beat leaves no object to look at")
    ("/channels/993101/messages" nil "and a post answers with what it created")
    ("/channels/993101/messages/bulk-delete" nil
     "a named sub-route is not a message id: ids are snowflakes")
    ("/interactions/1543687385819254897/tok/callback" nil nil)
    ("/applications/1543687385819254897/commands" nil nil)
    ("/users/@me" nil nil)))

(deftest channel-discord-plans-never-carry-the-token ()
  ;; The token lives in the executor's headers, never in a plan: the
  ;; recording executor sees exactly what tests and status surfaces see.
  (let ((executor (nck:make-recording-executor)))
    (nck:execute-plan executor
                      (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "hello")))
    (nck:execute-plan executor (ncd:typing-plan '(:channel-id "c1")))
    (dolist (plan (nck:recording-executor-plans executor))
      (is (null (nck:request-plan-headers plan)))
      (let ((rendered (nlk:encode-json-object
                       (or (nck:request-plan-body plan)
                           (nlk:json-object)))))
        (is (not (search "Bot " rendered)))))))

(deftest channel-discord-failure-copy (let ((plan (ncd:typing-plan '(:channel-id "c1")))))
  (is-table (needle status detail)
    (search needle (ncd::discord-failure-message plan status detail))
    ("privileged intents" 403 "missing access")
    ("rate limited" 429 "slow down")
    ("status 500" 500 "oops")))

(defun discord-upload-payload (plan path content &aux (body (nck:request-plan-body plan)))
  "Assert PLAN is the send_file upload of PATH into thread t9 — payload_json,
then files[0] — carrying CONTENT and its one attachment; answers the payload."
  (is-plan plan :path "/channels/t9/messages" :label "send_file")
  (is-shape body (nck:multipart-body-p is) (caar "payload_json") (caadr "files[0]") (cdadr path))
  (let* ((payload (shasht:read-json (cdr (first body))))
         (atts (nlk:json-value payload :array "attachments")))
    (is (equal content (gethash "content" payload)))
    (is (and (vectorp atts) (= 1 (length atts))))
    (is (equal "pea.png" (nlk:json-value (aref atts 0) :string "filename")))
    payload))

(deftest channel-discord-file-message-plan ()
  (let ((payload (discord-upload-payload
                  (ncd:discord-file-message-plan '(:channel-id "c1" :thread-id "t9")
                                                 #P"/tmp/pea.png" :content "here")
                  #P"/tmp/pea.png" "here")))
    (is (equalp #() (nlk:json-value payload :array "allowed_mentions" "parse"))))
  (let* ((plan (ncd:discord-file-message-plan '(:channel-id "c1")
                                              #P"pea.png"
                                              :content "here" :reply-to "m7"))
         (payload (shasht:read-json
                   (cdr (first (nck:request-plan-body plan))))))
    (is-shape payload ((:string "message_reference" "message_id") "m7")
      ((:string "message_reference" "channel_id") "c1")
      ((:array "allowed_mentions" "parse") equalp #())))
  (is-plan (ncd:discord-file-message-plan '(:channel-id "c1") #P"x.jpg") :headers nil))

(deftest channel-discord-a-voice-message-is-flagged-and-drawn ()
  ;; A voice message is the one attachment flagged IS_VOICE_MESSAGE, declaring
  ;; how long it plays and its waveform, with no words: Discord refuses words
  ;; on one. It answers the ask the way an answer does.
  (let* ((plan (ncd:discord-file-message-plan
                '(:channel-id "c1" :thread-id "t9") #P"/tmp/voice-message.ogg" :reply-to "m7"
                :voice (list :seconds 2.5 :waveform (make-array 256 :element-type '(unsigned-byte 8)
                                                                    :initial-element 255))))
         (body (nck:request-plan-body plan))
         (payload (shasht:read-json (cdr (first body))))
         (attachment (aref (nlk:json-value payload :array "attachments") 0)))
    (is-plan plan :path "/channels/t9/messages" :label "send_voice")
    (is-shape body (caadr "files[0]") (cdadr #P"/tmp/voice-message.ogg"))
    (is-shape payload ((:integer "flags") 8192) ((:any "content") nil)
      ((:string "message_reference" "message_id") "m7")
      ((:array "allowed_mentions" "parse") equalp #()))
    (is-shape attachment ((:string "filename") "voice-message.ogg")
      ((:number "duration_secs") = 2.5)
      ((:string "waveform") (cl-base64:usb8-array-to-base64-string
                             (make-array 256 :element-type '(unsigned-byte 8) :initial-element 255))))))

(deftest channel-discord-answer-carries-its-files ()
  ;; The answer's message with its picture: one multipart upload carrying
  ;; the content, the attachments array, the reference and the ping — the
  ;; picture never lives beside the answer.
  (let ((payload (discord-upload-payload
                  (ncd:discord-message-plan '(:channel-id "c1" :thread-id "t9")
                                            (first-chunk "the answer") :reply-to "m7"
                                            :ping t :files (list #p"/tmp/pea.png"))
                  #p"/tmp/pea.png" "the answer")))
    (is (equal "m7" (nlk:json-value payload :string "message_reference" "message_id")))
    ;; The upload's reference names the thread the multipart POST lands in, too.
    (is (equal "t9" (nlk:json-value payload :string "message_reference" "channel_id")))
    (is (eq t (gethash "replied_user" (gethash "allowed_mentions" payload))))))

(deftest channel-discord-delete-message-plan ()
  (is-plan (ncd:delete-message-plan '(:channel-id "123" :thread-id "456") "m9")
           :method "DELETE" :path "/channels/456/messages/m9" :label "delete_message"
           :retry t :headers nil))

(deftest channel-discord-only-the-answer-pings ()
  ;; The one mention this adapter emits. parse stays [] everywhere, so no
  ;; text a model produces can ping a role, a bystander, or @everyone.
  (let* ((chunks (nck:split-text-chunks
                  (make-string 2500 :initial-element #\x) 2000))
         (answer (ncd:discord-message-plan '(:channel-id "c1") (first chunks)
                                           :reply-to "m0" :ping t))
         (answer-tail (ncd:discord-message-plan '(:channel-id "c1")
                                                (second chunks) :reply-to "m0" :ping t))
         (chrome (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "working"))))
    (is (eq t (nlk:json-value answer.body :boolean "allowed_mentions" "replied_user")))
    (is (equalp #() (nlk:json-value answer.body :array "allowed_mentions" "parse")))
    (is (null (nlk:json-value answer-tail.body :boolean "allowed_mentions" "replied_user")))
    (is (null (nlk:json-value chrome.body :boolean "allowed_mentions" "replied_user")))))

(deftest channel-discord-an-addressed-post-mentions-the-operator ()
  ;; The note that needs the operator @s them: the id is spelled ahead of the
  ;; first chunk and whitelisted in allowed_mentions.users — parse stays [],
  ;; so the platform's own spelling is the only thing that can reach anyone —
  ;; and a later chunk is the text alone. The spelling is cut with the text.
  ;; Without this, a chunk cut to exactly 2000 would exceed the limit once the
  ;; mention is added, and the platform would reject the post. Lines of 99
  ;; put a cut at exactly 2000 when nothing is spelled.
  (let* ((id "931708065319907338")
         (executor (nck:make-recording-executor))
         (host (test-host :executor executor :platform (ncd:discord-platform :bot-user-id "999")))
         (text (format nil "~{~a~^~%~}"
                       (loop for line below 36
                             collect (make-string 99 :initial-element (if (zerop line) #\a #\x))))))
    (is (= 2000 (length (nck:text-chunk-text (first-chunk text)))) "the text alone fills a chunk")
    (is (nck:post-message host '(:channel-id "c1") text :mentions (list id)))
    (let* ((plans (nck:recording-executor-plans executor))
           (contents (mapcar #'plan-content plans))
           (body (nck:request-plan-body (first plans))))
      (is (= 2 (length plans)))
      (is (every (lambda (content) (<= (length content) 2000)) contents) "every chunk Discord takes")
      (is (eql 0 (search (format nil "<@~a> a" id) (first contents))))
      (is (equalp (vector id) (nlk:json-value body :array "allowed_mentions" "users")))
      (is (equalp #() (nlk:json-value body :array "allowed_mentions" "parse")))
      (is (null (nlk:json-value body :boolean "allowed_mentions" "replied_user")))
      (is (null (search id (second contents)))))))

(deftest channel-discord-edit-message-plan ()
  (let ((plan (ncd:edit-message-plan '(:channel-id "123" :thread-id "456")
                                     "m9" "working")))
    (is-plan plan :path "/channels/456/messages/m9" :method "PATCH" :label "edit_message"
                  "content" "working" :retry nil :headers nil)
    (is (equalp #() (nlk:json-value (nck:request-plan-body plan) :array
                                    "allowed_mentions" "parse"))))
  (is-plan (ncd:edit-message-plan '(:channel-id "1") "m" "done" :retry t) :retry t)
  ;; CONTROLS restates the message's components: a running status line's
  ;; stop button; an edit that says nothing about them leaves them as they
  ;; stand, and the settle that stops the turn retires the line itself.
  (let ((body (nck:request-plan-body
               (ncd:edit-message-plan '(:channel-id "1") "m" "working"
                                      :controls (list (list "Stop" "d" :danger))))))
    (is (= 1 (length (nlk:json-value body :array "components")))))
  (is-plan (ncd:edit-message-plan '(:channel-id "1") "m" "done") "components" nil))

(deftest channel-discord-body-retry-after ()
  (is-table (expected json) (eql expected (ncd::discord-retry-after-ms 429 (cell-json json) nil))
    (750 "{\"retry_after\": 0.75}") (2000 "{\"retry_after\": 2}") (nil "{}"))
  (is (null (ncd::discord-retry-after-ms 429 nil nil)))
  ;; End to end through the retry loop, as the live executor is built.
  (let ((result (scripted-execution (list (nck:make-scripted-response
                                           429 (cell-json "{\"retry_after\": 0.75}")
                                           (nlk:make-json-object "retry-after" "0"))
                                          (nck:make-scripted-response 200))
                                    :retry t :retry-after-fn #'ncd::discord-retry-after-ms)))
    (is-shape result (nck:execution-ok-p is) (.retry-delays '(750)))))

;;; --- the probe ----------------------------------------------------------------

(defun discord-probe-executor (&rest tail)
  "A recording executor answering what a probe reads first — bot nodecode#4821,
its one guild peas, whose channels are #general and #bots beside a voice room —
then TAIL."
  (nck:make-recording-executor
   :responses (list* (reply 200 "id" "1408" "username" "nodecode" "discriminator" "4821")
                     (rooms-reply '("9931" "peas"))
                     (nck:make-scripted-response
                      200 (vector (nlk:json-object "id" "993101" "name" "general" "type" 0)
                                  (nlk:json-object "id" "993107" "name" "bots" "type" 0)
                                  (nlk:json-object "id" "993199" "name" "voice" "type" 2)))
                     tail)))

(deftest channel-discord-probe-lists-what-the-token-sees ()
  (with-temp-file (token-path :contents "tok-secret-value" :type "txt")
    (let ((section (nlk:json-object "bot_token_file" token-path)))
      (let* ((executor (discord-probe-executor (reply 200 "flags" 0)))
             (text (ncd:probe-channel section :executor executor)))
        (is (search "token: ok — bot nodecode#4821 (id 1408)" text) "the identity line")
        (is (search "guild \"peas\" (id 9931): #general 993101, #bots 993107" text))
        (is (not (search "voice" text)) "a voice channel is not a place for a message")
        (is (search "intents: message content not granted" text))
        (is (not (search "tok-secret-value" text)) "the token is never in the answer")
        (is (equal '("/users/@me" "/users/@me/guilds" "/guilds/9931/channels" "/applications/@me")
                   (mapcar #'nck:request-plan-path (nck:recording-executor-plans executor))))
        (is (every (lambda (plan) (equal "GET" (nck:request-plan-method plan)))
                   (nck:recording-executor-plans executor))))
      (let* ((executor (scripted-executor (reply 401 "message" "401: Unauthorized")))
             (text (ncd:probe-channel section :executor executor)))
        (is (search "token: unauthorized (401)" text) "a wrong token is one line")
        (is (= 1 (length (nck:recording-executor-plans executor))))
        (is (not (search "tok-secret-value" text))))))
  (is (signals-error nlk:config-refusal (ncd:probe-channel (nlk:json-object)))))


(deftest channel-discord-declares-its-section-and-offers-channels-by-name ()
  ;; The adapter's one statement of what channels.discord is made of, and
  ;; the choices function a setup panel fills allowed_channels from.
  (let ((section (nlk:find-section '("channels" "discord"))))
    (is-present section "the adapter declares channels.discord"
      (is (equal "nodecode-channel-discord" (nlk:section-owner section)))
      (is (equal '(("bot_token_env" "bot_token_file")) (nlk:section-one-of section)))
      (is (equal '(("allowed_channels" "allowed_users" "allowed_roles")) (nlk:section-any-of section)))
      (is (nlk:section-find-field section "mention_patterns"))
      (dolist (field-name '("free_response_channels"
                            "require_mention_channels"
                            "ignored_channels"))
        (is (nlk:section-find-field section field-name))
        (is (functionp (nlk:section-field-choices
                        (nlk:section-find-field section field-name)))))
      (is (functionp (nlk:section-check section)) "the probe is the check")
      (is (functionp (nlk:section-field-choices
                      (nlk:section-find-field section "allowed_channels"))))
      (is (search "discord.com/developers/applications" (nlk:section-text section)))
      (is (equal "channels.discord: still needed: bot token · allowed_channels or allowed_users or allowed_roles"
                 (nlk:section-summary section nil)))))
  (with-temp-file (token-path :contents "tok-secret-value" :type "txt")
    (let ((section (nlk:json-object "bot_token_file" token-path)))
      (let* ((executor (discord-probe-executor))
             (choices (ncd:probe-channel-choices section :executor executor)))
        (is (equal '(("993101" . "#general  peas") ("993107" . "#bots  peas")) choices))
        (is (equal '("/users/@me" "/users/@me/guilds" "/guilds/9931/channels")
                   (mapcar #'nck:request-plan-path (nck:recording-executor-plans executor)))))
      (let* ((executor (scripted-executor (reply 401 "message" "401: Unauthorized")))
             (refusal (signals-error nlk:config-refusal
                        (ncd:probe-channel-choices section :executor executor))))
        (is (and refusal (search "unauthorized (401)" (princ-to-string refusal))))
        (is (and refusal (not (search "tok-secret-value" (princ-to-string refusal)))))))))

(deftest channel-discord-commands-plan ()
  ;; The catalog as the application's global commands: one bulk overwrite,
  ;; names Discord would refuse left out rather than failing the sync,
  ;; descriptions cut to 100 and never empty, one optional `args' option
  ;; for a command with a usage.
  (let* ((plan (ncd:commands-plan
                "app1"
                (list (list :name "help" :description "Show commands" :usage "")
                      (list :name "models"
                            :description (make-string 150 :initial-element #\d)
                            :usage "/models [provider] [model]"
                            :autocomplete t)
                      (list :name "evict" :description "Drop"
                            :usage "/evict [percent]")
                      (list :name "Bad Name" :description "x" :usage "")
                      (list :name "bare" :description "" :usage ""))
                :timeout-seconds 9))
         (body (nck:request-plan-body plan)))
    (is-plan plan :method "PUT" :path "/applications/app1/commands" :retry t
                  :label "set_commands" :timeout 9)
    (is (= 4 (length body)) "a name Discord would refuse is left out")
    (let ((help (aref body 0)))
      (is-shape help ("name" "help") ("type" = 1 "a chat input command")
        ("description" "Show commands") ("options" null "a bare command takes no option")))
    (let ((models (aref body 1)))
      (is (= 100 (length (gethash "description" models))))
      (is-present (option (aref (gethash "options" models) 0))
        "a command with a usage takes one string option"
        (is-shape option ("type" = 3) ("name" "args") ("description" "/models [provider] [model]")
          ("required" null "and optional") ("autocomplete" eq t))))
    (is (null (gethash "autocomplete" (aref (gethash "options" (wire-row "evict" body "name")) 0))))
    (is (equal "bare" (gethash "description" (wire-row "bare" body "name"))))
    (is (ncd:discord-command-name-p "memory-index"))
    (is (not (ncd:discord-command-name-p "Memory")))
    (is (not (ncd:discord-command-name-p "")))))

(deftest channel-discord-interaction-response-plan ()
  ;; An interaction is answered in two steps: held open first — a deferred
  ;; response, never retried, so a command slower than Discord's three seconds
  ;; still lands — then the held response edited into the answer, mention
  ;; parsing off as on every message.
  (let ((candidate (nlk:json-object "source" (nlk:json-object "interaction_id" "i1"
                                                              "interaction_token" "tok"
                                                              "application_id" "app1"))))
    (is-plan (ncd:interaction-response-plan candidate nil :timeout-seconds 4)
             :method "POST" :path "/interactions/i1/tok/callback"
             :label "interaction_defer" :timeout 4 :retry nil "type" 5 "data" nil)
    ;; A private answer is decided at the hold: the edit takes its flag.
    (is-plan (ncd:interaction-response-plan candidate nil :private t)
             "type" 5 (:integer "data" "flags") 64)
    (let* ((plan (ncd:interaction-response-plan candidate "the answer" :timeout-seconds 4))
           (body (nck:request-plan-body plan)))
      (is-plan plan :method "PATCH" :path "/webhooks/app1/tok/messages/@original"
                    :label "interaction_response" :timeout 4 :retry t)
      (is-shape body ("content" "the answer")
        ((:array "allowed_mentions" "parse") equalp #())
        ("components" null "no controls, the message's stay as they are")))
    ;; Controls ride the answer as its components; :CLEAR takes them away.
    (is (= 1 (length (nlk:json-value
                      (nck:request-plan-body
                       (ncd:interaction-response-plan candidate "pick one"
                                                      :controls (list (nck:choice "a" "a"))))
                      :array "components"))))
    (is (equalp #() (gethash "components" (nck:request-plan-body
                                           (ncd:interaction-response-plan candidate "done"
                                                                          :controls :clear))))))
  ;; A press is answered in place: held as a deferred update, so the answer
  ;; edits the message pressed — unless it is private, a message of its own.
  (let ((press (nlk:json-object "source" (nlk:json-object "interaction_id" "i2"
                                                          "interaction_token" "tok"
                                                          "application_id" "app1"
                                                          "pressed" t))))
    (is-plan (ncd:interaction-response-plan press nil) "type" 6 "data" nil)
    (is-plan (ncd:interaction-response-plan press nil :private t)
             "type" 5 (:integer "data" "flags") 64))
  (is (null (ncd:interaction-response-plan (nlk:json-object "source" (nlk:json-object))
                                           nil))))

(deftest channel-discord-provider-navigation-opens-model-list ()
  (with-temp-store ()
    (with-stubbed-fdefinition (nle:models (&optional provider)
                                (if provider
                                    (values (loop for i from 1 to 30 collect (format nil "m~d" i)) nil)
                                    '(("p1" . 30))))
      (with-stubbed-fdefinition (nle:gateway-target (session-id)
                                  (values "p1" "m1"))
        (let* ((executor (nck:make-recording-executor))
               (host (test-host :name "discord-provider-navigation" :executor executor
                                :platform (ncd:discord-platform :bot-user-id "999" :application-id (lambda () "app1"))))
               (candidate (test-candidate :channel "100")))
          (multiple-value-bind (text controls)
              (nck::room-models-command host candidate "" "discord-100")
            (declare (ignore text))
            (let* ((menu (aref (nlk:json-array (aref (ncd::controls-components controls) 0) "components") 0))
                   (option (aref (nlk:json-array menu "options") 0)))
              (is (null (nlk:json-value option :any "default")) "provider navigation starts unselected on the Discord wire")
              (flet ((press (line)
                       (ncd:route-discord-interaction
                        (nlk:json-object "op" 0 "t" "INTERACTION_CREATE"
                         "d" (nlk:json-object "id" "i1" "token" "tok" "application_id" "app1"
                              "type" 3 "channel_id" "100" "guild_id" "g1"
                              "member" (nlk:json-object "user" (nlk:json-object "id" "u1"))
                              "message" (nlk:json-object "id" "picker" "flags" 64)
                              "data" (nlk:json-object "custom_id" "nck:menu:0" "component_type" 3
                                      "values" (vector line)))))))
                (dolist (line (list (nlk:json-value option :string "value")
                                   (nck:press-token "/models p1 page 2")))
                  (let ((pick (press line)))
                    (nck::respond host pick
                                  (lambda () (nck::run-command host pick (gethash "text" pick)))))))
              (let* ((plans (nck:recording-executor-plans executor))
                     (updates (remove-if-not (lambda (plan)
                                               (equal "interaction_response" (nck:request-plan-audit-label plan))) plans))
                     (first-body (nck:request-plan-body (first updates)))
                     (last-body (nck:request-plan-body (second updates))))
                (is (= 4 (length plans)) "provider choice and paging each defer then update")
                (is-plan (first plans) "type" 6 "data" nil)
                (is-plan (first updates) :method "PATCH" :path "/webhooks/app1/tok/messages/@original")
                (is (search "Select a p1 model" (nlk:json-value first-body :string "content")))
                (is (= 25 (length (nlk:json-array (aref (nlk:json-array (aref (nlk:json-array first-body "components") 0) "components") 0) "options"))))
                (is (= 5 (length (nlk:json-array (aref (nlk:json-array (aref (nlk:json-array last-body "components") 0) "components") 0) "options"))))
                (is (null (nth-value 1 (nck::room-model host "discord-100"))) "provider navigation never changes the room model")))))))))

(deftest channel-discord-interaction-ack-plan ()
  ;; A control press is answered by deferring the interaction update: the
  ;; spinner on the pressed button stops and the message stays as it is;
  ;; the card the press touched settles on its own. A Details press is
  ;; answered with its text, ephemeral: the presser's alone.
  (let ((plans (ncd:interaction-ack-plans
                '(:id "i1" :token "tok" :data "nck:stop:discord-c1-m9")
                :timeout-seconds 4)))
    (is (= 1 (length plans)))
    (is-plan (first plans) :method "POST" :timeout 4 :label "interaction_ack"
             :path "/interactions/i1/tok/callback" :retry nil :headers nil "type" 6))
  (let ((plan (first (ncd:interaction-ack-plans '(:id "i2" :token "tok")
                                                :text (make-string 2500 :initial-element #\s)))))
    (is-plan plan :label "interaction_private_answer" :path "/interactions/i2/tok/callback"
                  :retry nil "type" 4 (:integer "data" "flags") 64)
    (is (= 2000 (length (nlk:json-value (nck:request-plan-body plan) :string "data" "content"))))
    (is (equalp #() (nlk:json-value (nck:request-plan-body plan) :array "data" "allowed_mentions" "parse")))))

(deftest channel-discord-interaction-ack-answers-whole ()
  ;; WHOLE keeps every word: the answer carries the first message's worth and
  ;; each further message is an ephemeral follow-up under the interaction's
  ;; application, in order, never retried -- the board's Show all, whose list
  ;; outgrew one message and lost every section after the first two.
  (let* ((lines (loop for i below 150 collect (format nil "<#~d> · 2d" (+ 1562000000000000000 i))))
         (text (format nil "~{~a~^~%~}" lines))
         (plans (ncd:interaction-ack-plans '(:id "i3" :token "tok" :application-id "a1")
                                           :text text :whole t :timeout-seconds 4))
         (contents (cons (nlk:json-value (nck:request-plan-body (first plans)) :string "data" "content")
                         (mapcar (lambda (plan) (nlk:json-value (nck:request-plan-body plan) :string "content"))
                                 (rest plans)))))
    (is (< 2000 (length text)) "the list is longer than one message")
    (is (= 3 (length plans)))
    (is-plan (first plans) :label "interaction_private_answer" :path "/interactions/i3/tok/callback"
                           :retry nil "type" 4 (:integer "data" "flags") 64)
    (dolist (plan (rest plans))
      (is-plan plan :method "POST" :label "interaction_private_followup" :path "/webhooks/a1/tok"
                    :retry nil :timeout 4 (:integer "flags") 64)
      (is (equalp #() (nlk:json-value (nck:request-plan-body plan) :array "allowed_mentions" "parse"))))
    (is (every (lambda (content) (< (length content) 2000)) contents))
    ;; Every line once, in order, none cut.
    (is (equal lines (loop for content in contents
                           append (uiop:split-string (string-right-trim '(#\Newline) content)
                                                     :separator '(#\Newline))))))
  ;; One message's worth needs no follow-up, nor the application; more
  ;; without it is refused, never posted under a path naming none.
  (is (= 1 (length (ncd:interaction-ack-plans '(:id "i4" :token "tok") :text "short" :whole t))))
  (signals-error error
    (ncd:interaction-ack-plans '(:id "i5" :token "tok") :text (make-string 2500 :initial-element #\s) :whole t)))

(defun card-parts (body)
  "The components inside the one container a card's message BODY holds."
  (coerce (nlk:json-value (aref (nlk:json-value body :array "components") 0) :array "components") 'list))

(defun card-texts (card &optional controls)
  "Every text CARD drawn with CONTROLS shows, in order, a step's row inside
its section included."
  (loop for part across (nlk:json-value (aref (ncd:card-components card controls) 0) :array "components")
        for type = (gethash "type" part)
        when (eql type 10) collect (gethash "content" part)
        when (eql type 9) collect (nlk:json-value (aref (gethash "components" part) 0) :string "content")))

(deftest channel-discord-a-failed-card-shows-every-line-of-its-note ()
  ;; A walked failover chain's failure is a model a line; each is a quoted
  ;; line of the card, and a long note takes its room from the model's words,
  ;; never past Discord's 4000 characters.
  (let* ((note (format nil "every model failed~%1. a/m: refused~%2. b/n · HTTP 400: gone"))
         (texts (card-texts (list :state :failed :elapsed "30s" :headline "Failed at 30s" :note note))))
    (is-carrying (text (format nil "~{~a~%~}" texts))
      "> every model failed" "> 1. a/m: refused" "> 2. b/n · HTTP 400: gone"))
  (let* ((long (format nil "~{~a~^~%~}" (loop repeat 6 collect (make-string 280 :initial-element #\x))))
         (said (make-string 1300 :initial-element #\y))
         (steps (loop repeat 6 collect (list :done (make-string 190 :initial-element #\z) "1s")))
         (texts (card-texts (list :state :failed :elapsed "30s" :headline "Failed at 30s"
                                  :note long :said said :steps steps :meta "m"))))
    (is (<= (reduce #'+ texts :key #'length) 4000) "the whole card stays inside Discord's cap")
    (is (some (lambda (text) (search (make-string 280 :initial-element #\x) text)) texts) "a long note line shows whole")))

(deftest channel-discord-a-card-is-components ()
  ;; A turn's card posts as one container of components (IS_COMPONENTS_V2),
  ;; no words and no embeds: its accent the phase; the state and the time in
  ;; small type over its heading; the thought and the note; what the turn
  ;; said on its way, in a block of its own, its markdown drawn; a row per
  ;; step, one with something to show carrying its Output button; a Waiting block;
  ;; its numbers in small type under a rule; Stop and Details inside it.
  ;; Discord's markdown in a file name stays literal.
  (let* ((card (list :state :working :elapsed "12s" :headline "Running just_lint"
                     :thought "An extra paren in digest-status-text" :earlier 1
                     :steps '((:done "Read kit/digest.lisp" "1s" "nck:step:s1:2")
                              (:running "Running just_lint" "2s" nil))
                     :note "fell back to alt-model" :meta (format nil "mock/mock-fast · 3 steps~%↑5.4k ↓620")
                     :pending '("⌎ and deploy it — after this turn") :said "Checking **the** logs first."))
         (post (nck:request-plan-body
                (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "words") :card card :reply-to "m1"
                                          :controls (list (list (list "Stop" "nck:stop:s1" :danger)
                                                                (list "Details" "nck:details:s1" :secondary))))))
         (parts (card-parts post)))
    (is-shape post ((:integer "flags") = ncd::+discord-components-v2+) ((:any "content") nil)
      ((:any "embeds") nil) ((:any "attachments") nil) ((:string "message_reference" "message_id") "m1"))
    (is-shape (aref (nlk:json-value post :array "components") 0)
      ((:integer "type") = 17) ((:integer "accent_color") = #x5865F2))
    (is (equal (list (format nil "-# working · 12s~%### Running just\\_lint")
                     (format nil "*An extra paren in digest-status-text*~%> fell back to alt-model")
                     "Checking **the** logs first."
                     "-# +1 earlier")
               (mapcar (lambda (part) (gethash "content" part)) (subseq parts 0 4))))
    (is-shape (fifth parts) ((:integer "type") = 9) ((:string "accessory" "label") "Output")
      ((:string "accessory" "custom_id") "nck:step:s1:2") ((:integer "accessory" "style") = 2))
    (is (equal "✓ Read kit/digest.lisp · 1s"
               (nlk:json-value (aref (gethash "components" (fifth parts)) 0) :string "content")))
    (is-shape (sixth parts) ((:integer "type") = 10) ((:string "content") "› Running just\\_lint · 2s"))
    (is-shape (seventh parts) ((:string "content") (format nil "**Waiting**~%⌎ and deploy it — after this turn")))
    (is-shape (eighth parts) ((:integer "type") = 14) ((:any "divider") t))
    (is-shape (ninth parts) ((:string "content") (format nil "-# mock/mock-fast · 3 steps~%-# ↑5.4k ↓620")))
    (is (equal '("Stop" "Details") (map 'list (lambda (button) (gethash "label" button))
                                         (gethash "components" (tenth parts)))))
    (is (= 10 (length parts))))
  ;; An edit is a whole card too, and empties the words and embeds a card
  ;; posted before cards were components would carry.
  (let ((edit (nck:request-plan-body
               (ncd:edit-message-plan '(:channel-id "c1") "s1" "words"
                                      :card (list :state :done :elapsed "30s" :headline "Done in 30s")))))
    (is-shape edit ((:integer "flags") = ncd::+discord-components-v2+) ((:array "embeds") equalp #()))
    (is (eq :null (gethash "content" edit)))
    (is-shape (aref (gethash "components" edit) 0) ((:integer "accent_color") = #x23A55A))
    (is (equal (list (format nil "-# done~%### Done in 30s"))
               (mapcar (lambda (part) (gethash "content" part)) (card-parts edit))))))

(deftest channel-discord-a-cards-pictures-are-a-gallery-sent-once ()
  ;; The pictures a turn looked at are the card's gallery, each named
  ;; attachment://NAME: one its message holds already is kept by its id and
  ;; never sent again, a new one rides the request as files[i] under that
  ;; index, and a new one whose file is gone is left out. A card that touches
  ;; no picture says nothing of attachments; one whose pictures went lists
  ;; none, so the message holds none.
  (let ((new (temp-path "card-picture" "png")))
    (alexandria:write-byte-vector-into-file (coerce #(137 80 78 71) '(vector (unsigned-byte 8))) new
                                            :if-exists :supersede)
    (unwind-protect
         (let* ((card (list :state :working :elapsed "3s" :headline "Looking"
                            :images (list (list "step-1.png" "/gone/old.png" "old.png")
                                          (list "step-2.png" (namestring new) "shot.png")
                                          (list "step-3.png" "/gone/never.png" "never.png"))))
                (form (nck:request-plan-body
                       (ncd:edit-message-plan '(:channel-id "c1") "s1" "words" :card card
                                              :media '(("step-1.png" . "900")))))
                (payload (nlk:decode-json (cdr (assoc "payload_json" form :test #'equal))))
                (gallery (find 12 (card-parts payload) :key (lambda (part) (gethash "type" part)))))
           (is (equal '("payload_json" "files[0]") (mapcar #'car form)) "one file sent, the new one")
           ;; Sent under the name its card calls it by: a copy of the picture.
           (let ((sent (cdr (assoc "files[0]" form :test #'equal))))
             (is (equal "step-2.png" (file-namestring sent)))
             (is (equalp (alexandria:read-file-into-byte-vector new) (alexandria:read-file-into-byte-vector sent))))
           ;; The held one kept by its id, the new one by its index.
           (is (equal '(("900" "step-1.png") (0 "step-2.png"))
                      (map 'list (lambda (kept) (list (gethash "id" kept) (gethash "filename" kept)))
                           (gethash "attachments" payload))))
           (is (equal '(("attachment://step-1.png" "old.png") ("attachment://step-2.png" "shot.png"))
                      (map 'list (lambda (item) (list (nlk:json-value item :string "media" "url")
                                                      (gethash "description" item)))
                           (gethash "items" gallery)))))
      (delete-file new)))
  (flet ((attachments (&rest keys)
           (nlk:json-value (nck:request-plan-body
                            (apply #'ncd:edit-message-plan '(:channel-id "c1") "s1" "w"
                                   :card (list :state :done :headline "Done") keys))
                           :any "attachments")))
    (is (null (attachments)))
    (is (equalp #() (attachments :media '(("step-1.png" . "900"))))))
  ;; What the message holds, by name: a V2 message lists no attachments of its
  ;; own, each picture is the gallery item showing it (Discord's answer to a
  ;; card's post or edit), and the next edit keeps them all by id, uploading
  ;; none again though the files are still here. Without this, an edit would
  ;; re-upload the gallery and a picture open in the viewer would fail to load.
  (let ((shot (temp-path "card-kept" "png")))
    (alexandria:write-byte-vector-into-file (coerce #(137 80 78 71) '(vector (unsigned-byte 8))) shot
                                            :if-exists :supersede)
    (unwind-protect
         (flet ((item (id name)
                  (nlk:json-object "media" (nlk:json-object "url" (format nil "https://cdn.discordapp.com/attachments/c1/~a/~a?ex=1&hm=2" id name)
                                                            "attachment_id" id))))
           (let* ((answer (nlk:json-object
                           "id" "s1" "attachments" #()
                           "components" (vector (nlk:json-object
                                                 "type" 17
                                                 "components" (vector (nlk:json-object "type" 10 "content" "x")
                                                                      (nlk:json-object "type" 12 "items"
                                                                                       (vector (item "900" "step-1.png")
                                                                                               (item "901" "step-2.png"))))))))
                  (media (ncd:message-media answer))
                  (body (nck:request-plan-body
                         (ncd:edit-message-plan '(:channel-id "c1") "s1" "w" :media media
                                                :card (list :state :working :headline "Looking"
                                                            :images (list (list "step-1.png" (namestring shot) "a.png")
                                                                          (list "step-2.png" (namestring shot) "b.png")))))))
             (is (equal '(("step-1.png" . "900") ("step-2.png" . "901")) media))
             (is (hash-table-p body) "a JSON edit, no files part: nothing uploaded again")
             (is (equal '("900" "901") (map 'list (lambda (kept) (gethash "id" kept)) (gethash "attachments" body))))))
      (delete-file shot)))
  (is (null (ncd:message-media (nlk:json-object "id" "s1")))))

(deftest channel-discord-a-card-stays-inside-discords-caps ()
  ;; One V2 message holds 40 components however nested and 4000 characters of
  ;; text: the fullest card the kit draws — four steps with their Output, a
  ;; long thought and note, long words said, six parked rows, ten pictures,
  ;; two lines of numbers, Stop and Details — stays inside both.
  (let* ((long (make-string 3000 :initial-element #\x))
         (card (list :state :working :elapsed "2m05s" :task long :headline long :thought long :earlier 230
                     :steps (loop for n from 1 to 4 collect (list :done long "1s" (format nil "nck:step:s:~d" n)))
                     :note long :meta (format nil "~a~%~a" long long) :pending (make-list 6 :initial-element long)
                     :said long
                     :images (loop for n from 1 to 10 collect (list (format nil "step-~d.png" n) "/gone" "x"))))
         (container (aref (ncd:card-components card (list (list (list "Stop" "a" :danger) (list "Details" "b" :secondary)))
                                               :media (loop for n from 1 to 10
                                                            collect (cons (format nil "step-~d.png" n) (princ-to-string n))))
                          0)))
    (labels ((walk (part)
               (cons part (append (loop for child across (or (gethash "components" part) #()) append (walk child))
                                  (let ((accessory (gethash "accessory" part))) (and accessory (walk accessory)))))))
      (let ((all (walk container)))
        (is (<= (length all) 40) "components")
        (is (<= (loop for part in all when (eql 10 (gethash "type" part)) sum (length (gethash "content" part)))
                4000))))))

(deftest channel-discord-a-command-panel-is-a-components-container ()
  ;; A command's card (NCK:OFFER-CARD) posts as one Components V2 container
  ;; and no words: its tone the accent, its title over the muted words beside
  ;; it, its gauge, its words, its fields a line each, then its lines
  ;; (NCK::LINE-CHOICE) each a section with its button -- one with none its
  ;; words alone -- and its other controls rows at its foot. Answered through
  ;; a held interaction it is the edit's container; words answering a press
  ;; on a message of components are a container of those words, with no
  ;; heading; any other answer clears the embeds the message carried.
  (let* ((panel '(:title "Voice" :context "1 seat" :gauge "🟩⬜" :text "Sitting in voice." :tone :done
                  :fields (("Sitting in" . "<#vc-1>"))))
         (controls (list (nck::line-choice "nightly" (nck:choice "Open" "/jobs show nightly") :mark "●" :detail "every day")
                         (nck::line-choice "" nil :detail "that carries on")
                         (list (nck:choice "Refresh" "/jobs"))))
         (post (nck:request-plan-body (ncd:discord-message-plan '(:channel-id "c1") (first-chunk "words")
                                                                :panel panel :controls controls)))
         (container (aref (nlk:json-value post :array "components") 0))
         (parts (coerce (gethash "components" container) 'list))
         (candidate (nlk:json-object "source" (nlk:json-object "interaction_id" "i1"
                                                               "interaction_token" "tok"
                                                               "application_id" "app1"))))
    (is-shape post ((:any "content") nil) ((:integer "flags") = ncd::+discord-components-v2+))
    (is-shape container ((:integer "type") = 17) ((:integer "accent_color") = #x23A55A))
    (is (equal '(10 10 10 10 9 10 1) (mapcar (lambda (part) (gethash "type" part)) parts)) "heading, gauge, words, fields, a line with its button, a line alone, the foot row")
    (is (equal (list (format nil "### Voice~%-# 1 seat") "🟩⬜" "Sitting in voice." "**Sitting in** · <#vc-1>")
               (mapcar (lambda (part) (gethash "content" part)) (subseq parts 0 4))))
    (let ((section (fifth parts)))
      (is (equal "● **nightly** · every day" (gethash "content" (aref (gethash "components" section) 0))))
      (is (equal "Open" (nlk:json-value section :text "accessory" "label")) "the line's button beside it"))
    (is (equal "that carries on" (gethash "content" (sixth parts))) "a line with no label says its detail alone")
    (let ((answer (nck:request-plan-body (ncd:interaction-response-plan candidate "words" :panel panel))))
      (is (eq :null (gethash "content" answer)) "the words a container takes none of are null")
      (is-shape answer ((:integer "flags") = ncd::+discord-components-v2+))
      (is (= 17 (nlk:json-value (aref (nlk:json-value answer :array "components") 0) :integer "type"))))
    (let* ((source (gethash "source" candidate))
           (answer (progn (setf (gethash "pressed" source) t
                                (gethash "card_flags" source) ncd::+discord-components-v2+)
                          (nck:request-plan-body (ncd:interaction-response-plan candidate "did it"))))
           (words (coerce (gethash "components" (aref (nlk:json-value answer :array "components") 0)) 'list)))
      (is (equal '("did it") (mapcar (lambda (part) (gethash "content" part)) words)) "a press on a container is answered by one"))
    (setf (gethash "card_flags" (gethash "source" candidate)) 0)
    (is (equalp #() (nlk:json-value (nck:request-plan-body (ncd:interaction-response-plan candidate "words"))
                                    :array "embeds")))))

(deftest channel-discord-a-cards-marks-are-the-bots-emojis ()
  ;; Once the bot holds its emojis, a step wears its mark — a green check
  ;; done, a spinner running — and the spinner opens a working card's
  ;; description, so the card moves while the turn works; a settled card has
  ;; no description. Without them the marks are the kit's text.
  (let ((ncd:*card-marks* '(:done "<:nc_done:1>" :running "<a:nc_running:2>" :stopped "<:nc_stopped:3>")))
    (is (equal '("<a:nc_running:2> *Checking it*" "<:nc_done:1> Read a.lisp · 1s"
                 "<a:nc_running:2> Running just lint · 2s")
               (rest (card-texts (list :state :working :elapsed "4s" :headline "Running just lint"
                                       :thought "Checking it"
                                       :steps '((:done "Read a.lisp" "1s" nil) (:running "Running just lint" "2s" nil)))))))
    (is (= 1 (length (card-texts (list :state :done :headline "Done in 4s"))))))
  (let ((ncd:*card-marks* '()))
    (is (equal "› Running · 1s"
               (second (card-texts (list :state :working :headline "x" :steps '((:running "Running" "1s" nil)))))))))

(deftest channel-discord-a-card-is-titled-its-task ()
  ;; Under a task the heading is the ask's, the small type over it says the
  ;; state and the time, settled too; between steps the description says
  ;; what the turn does before its thought, and a running step says it on
  ;; its own row.
  (let ((ncd:*card-marks* '(:running "<a:nc_running:2>")))
    (is (equal (list (format nil "-# working · 9s~%### Fix the red lint") "<a:nc_running:2> Thinking · *An extra paren*")
               (card-texts (list :state :working :elapsed "9s" :task "Fix the red lint"
                                 :headline "Thinking" :thought "An extra paren"))))
    (is (equal "<a:nc_running:2> *An extra paren*"
               (second (card-texts (list :state :working :elapsed "9s" :task "Fix the red lint"
                                         :headline "Running just lint" :thought "An extra paren"
                                         :steps '((:running "Running just lint" "1s" nil))))))))
  (is (equal (list (format nil "-# done · 27s~%### Fix the red lint"))
             (card-texts (list :state :done :elapsed "27s" :task "Fix the red lint" :headline "Done in 27s")))))

(deftest channel-discord-application-emoji-plans ()
  ;; The application's emojis are listed, and one it lacks is uploaded from
  ;; its image as a data URI; a message spells one <:name:id>, <a:name:id>
  ;; when it moves.
  (is-plan (ncd:application-emojis-plan "app-1") :method "GET" :path "/applications/app-1/emojis"
                                                 :label "list_emojis")
  (let ((plan (ncd:create-application-emoji-plan
               "app-1" "nc_done" (asdf:system-relative-pathname "nodecode-channel-discord"
                                                                "emoji/nc_done.png"))))
    (is-plan plan :method "POST" :path "/applications/app-1/emojis" :retry nil "name" "nc_done")
    (is (eql 0 (search "data:image/png;base64,iVBOR" (plan-field plan "image")))))
  (is (equal "<a:nc_running:9>"
             (ncd:emoji-spelling (nlk:json-object "id" "9" "name" "nc_running" "animated" t))))
  (is (equal "<:nc_done:8>" (ncd:emoji-spelling (nlk:json-object "id" "8" "name" "nc_done")))))

(deftest channel-discord-autocomplete-plan ()
  ;; The one answer an autocomplete request takes: its callback, the
  ;; completion result, at most 25 choices cut to Discord's caps, never
  ;; retried — the menu waits three seconds only.
  (let* ((plan (first (ncd:interaction-autocomplete-plans
                       '(:id "i1" :token "tok")
                       (list (list :name "a6api/grok-4.6" :value "a6api grok-4.6")
                             (list :name (make-string 150 :initial-element #\n)
                                   :value (make-string 150 :initial-element #\v))))))
         (body (nck:request-plan-body plan))
         (choices (nlk:json-value body :array "data" "choices")))
    (is-plan plan :method "POST" :path "/interactions/i1/tok/callback"
                  :label "interaction_autocomplete" :retry nil)
    (is (= 8 (gethash "type" body)) "APPLICATION_COMMAND_AUTOCOMPLETE_RESULT")
    (is (= 2 (length choices)))
    (is-shape (aref choices 0) ((:string "name") "a6api/grok-4.6")
      ((:string "value") "a6api grok-4.6"))
    (is (= 100 (length (nlk:json-value (aref choices 1) :string "name"))))
    (is (= 100 (length (nlk:json-value (aref choices 1) :string "value")))))
  (let* ((many (loop for index below 40
                     collect (list :name (format nil "n~d" index)
                                   :value (format nil "v~d" index))))
         (plan (first (ncd:interaction-autocomplete-plans '(:id "i2" :token "tok") many)))
         (choices (nlk:json-value (nck:request-plan-body plan)
                                  :array "data" "choices")))
    (is (= 25 (length choices)) "Discord's menu holds 25 choices"))
  (let* ((plan (first (ncd:interaction-autocomplete-plans '(:id "i3" :token "tok") '())))
         (choices (nlk:json-value (nck:request-plan-body plan)
                                  :array "data" "choices")))
    (is (= 0 (length choices)) "no choices is an answer: nothing to show")))


(deftest channel-discord-failed-steps-have-a-red-cross ()
  ;; Existing application success/spinner emojis must not hide a failure.
  (let ((ncd:*card-marks* '(:done "<:nc_done:1>" :running "<a:nc_running:2>" :stopped "<:nc_stopped:3>")))
    (is (equal '("<a:nc_running:2>" "❌ Eval failed · 1s" "<:nc_done:1> Eval passed · 1s"
                 "<:nc_stopped:3> Eval stopped · 1s" "<a:nc_running:2> Eval running · 1s")
               (rest (card-texts (list :state :working :headline "Checking"
                                       :steps '((:failed "Eval failed" "1s" nil)
                                                (:done "Eval passed" "1s" nil)
                                                (:stopped "Eval stopped" "1s" nil)
                                                (:running "Eval running" "1s" nil))))))))
  (let ((ncd:*card-marks* nil))
    (is (equal "❌ Eval failed · 1s"
               (second (card-texts (list :state :done :headline "Done"
                                         :steps '((:failed "Eval failed" "1s" nil)))))))))


(deftest channel-discord-a-card-note-quotes-each-line ()
  ;; A failed turn's detail is the condition's message without SBCL's `See
  ;; also:' references (FAIL-TURN drops them where the fact is written). The
  ;; card quotes each of its lines, as CARD-TEXT does: a one-line note is one
  ;; quoted line, and no line of the note goes unquoted.
  (let* ((one (list :state :failed :elapsed "2m49s" :task "Update"
                    :note "The bounding indices 0 and 300 are bad for a sequence of length 61."))
         (two (list :state :failed :elapsed "2m49s" :task "Update"
                    :note (format nil "every model failed~%1. a/m: refused"))))
    (is (equal "> The bounding indices 0 and 300 are bad for a sequence of length 61."
               (second (card-texts one))))
    (is (equal (format nil "> every model failed~%> 1. a/m: refused") (second (card-texts two))))))
