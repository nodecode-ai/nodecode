;;;; adapter-test.lisp --- Telegram lane e2e, entirely scripted-executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No fake server: the Bot API surface is one executor seam, so a scripted
;;;; executor drives the whole lane against the real nodecode gateway
;;;; (WITH-TEMP-GATEWAY + provider stub). Covers: poll -> admission -> a
;;;; room and a lane -> typing -> the answer as a reply -> the write-back,
;;;; offset advance, redelivery dedupe, the 409 duplicate-poller posture,
;;;; the platform's own contribution, and the raw-update seam. The digest
;;;; flow itself is the kit's (host-test.lisp).

(in-package #:nodecode.test)

(nlk:access (adapter nct::telegram-adapter))

(defun telegram-scripted-updates ()
  (cell-json (format nil "{\"ok\": true, \"result\": [{\"update_id\": 7, \"message\": ~a}]}"
                      (tg-json 1 "\"text\": \"hi bot\"" :user-name "kim"))))

(defun test-telegram-adapter (&key executor (lanes "tg-test-lanes")
                                   bot-username bot-user-id allowed-updates)
  "A bare adapter over a host, the way the entry builds one, minus the
threads: the request, platform and seam tests drive it directly."
  (nct::%make-telegram-adapter
   :host (test-host :platform (nct:telegram-platform :bot-username bot-username
                                                     :bot-user-id bot-user-id)
                    :executor executor :name lanes)
   :bot-username bot-username
   :bot-user-id bot-user-id
   :allowed-updates (or allowed-updates nct:+default-allowed-updates+)))

(deftest channel-telegram-adapter-end-to-end (with-adapter-lane)
  (:reply "tg reply" :token "fake-telegram-token" :start nct:start-channel
   :bindings ((lock (bt2:make-lock :name "tg-e2e"))
              (recorded '())
              (polls 0)
              (executor
                (nck:make-plan-executor
                 :run (lambda (plan)
                        (bt2:with-lock-held (lock) (push plan recorded))
                        (let ((poll-p (plan-matching-p plan :label "get_updates")))
                          (when poll-p (bt2:with-lock-held (lock) (incf polls)))
                          (nck:make-execution
                           :ok-p t :status 200 :attempts 1
                           :body (cond ((not poll-p)
                                        (cell-json "{\"ok\": true,
                                       \"result\": {\"message_id\": 90}}"))
                                       ;; The same update twice: a poll retry redelivery.
                                       ((<= polls 2) (telegram-scripted-updates))
                                       (t (cell-json "{\"ok\": true}")))))))))
   :section (nlk:json-object
             "enabled" t
             "allowed_chats" (vector "555")
             "owner" (vector "42")
             "require_mention" nil
             "poll_interval_ms" 25
             "get_updates_timeout_seconds" 0
             "request_timeout_seconds" 5
             "token_file" token-path
             "soul_file" soul-path)
   :plans (bt2:with-lock-held (lock) (reverse recorded))
   :after ((is (adapter-threads-stopped-p "channel-telegram"))))
(is (await (:timeout 15) (plan-matching (plans) :label "send_rich_message")))
(let ((all (plans)))
  (is-typing-before-reply all "send_rich_message")
  (is-present (send (plan-matching all :label "send_rich_message")) "send plan recorded"
    (is-plan send (:string "rich_message" "markdown") "tg reply"
             "chat_id" "555" "disable_notification" nil (:any "reply_parameters" "message_id") 1)))
(is-lane-carrying "telegram" "telegram-555" "telegram-555-m1" "telegram-chat"
                  "(Telegram user id 42)" "(nct:request \"POST\" PATH")
(is (wire-row "kim (operator) [m1 u42]: hi bot" (first messages-seen) "content"))
(is-exchange-written-back-once
 "telegram-555" "telegram-555-m1"
 "the redelivered update admitted no second turn")
(is (await (:timeout 10) (find 8 (plans-matching (plans) :label "get_updates")
                               :key (lambda (plan) (plan-field plan "offset"))))))

(deftest channel-telegram-conflict-is-loud-and-slow (with-temp-gateway (port))
  (is (integerp port))
  (let* ((lock (bt2:make-lock :name "tg-409"))
         (polls 0)
         (executor
           (nck:make-plan-executor
            :run (lambda (plan)
                   (declare (ignore plan))
                   (bt2:with-lock-held (lock) (incf polls))
                   (nck:make-execution
                    :ok-p nil :status 409 :attempts 1
                    :error "telegram getUpdates conflict: duplicate ~
                              Telegram poller detected")))))
    (with-temp-file (token-path :contents "tok" :type "txt")
      (let* ((section (nlk:json-object
                       "enabled" t
                       "allowed_chats" (vector "555")
                       "require_mention" nil
                       "poll_interval_ms" 25
                       "get_updates_timeout_seconds" 0
                       "token_file" token-path))
             (stop (handler-bind ((warning #'muffle-warning))
                     (nct:start-channel section :executor executor))))
        (nlk:with-cleanup ((funcall stop))
          (handler-bind ((warning #'muffle-warning))
            (is (await () (plusp (bt2:with-lock-held (lock) polls))))
            (is (await () (let ((detail (getf (nck:channel-status "telegram")
                                              :detail)))
                            (and detail (search "conflict" detail)))))
            (sleep 1.0)
            (is (<= (bt2:with-lock-held (lock) polls) 2))))))))

(deftest channel-telegram-request ()
  ;; The model's one door to the Bot API, through the live executor. A write
  ;; that only acknowledges answers empty, and where Telegram has a read for
  ;; the object the door takes it; a reaction is the case that has none, and
  ;; says so by carrying no :STATE at all.
  (let* ((pinned (cell-json
                  "{\"ok\": true, \"result\":
                     {\"id\": 555, \"type\": \"supergroup\",
                      \"pinned_message\": {\"message_id\": 12}}}"))
         (ack (cell-json "{\"ok\": true, \"result\": true}"))
         (executor (scripted-executor (nck:make-scripted-response 200 ack)
                                      (nck:make-scripted-response 200 ack)
                                      (nck:make-scripted-response 200 pinned)))
         (adapter (test-telegram-adapter :executor executor
                                         :lanes "request-test"))
         (nct::*telegram-adapter* adapter))
    (let ((reacted (nct:request "POST" "/setMessageReaction"
                                :body '(:chat_id "555" :message_id 12
                                        :reaction #((:type "emoji"
                                                     :emoji "x"))))))
      (is-shape reacted (:status = 200 "Telegram accepted the request") (:body null) (:state null)))
    (is-present (plan (first (nck:recording-executor-plans executor))) "the plan the executor saw"
      (is-plan plan :path "/setMessageReaction" "chat_id" "555")
      (is (equal "x" (gethash "emoji" (aref (gethash "reaction" (nck:request-plan-body plan)) 0)))))
    (let ((pin (nct:request "POST" "/pinChatMessage"
                            :body '(:chat_id "555" :message_id 12))))
      (is (null (getf pin :body)))
      (is-present (state (getf pin :state)) "a pin has a read, and the door took it"
        (is (equal '(:message_id 12)
                   (getf (getf (getf state :body) :result) :pinned_message)))))
    (is-present (plans (nck:recording-executor-plans executor))
      "the reaction, the pin, and the pin's read"
      (is (= 3 (length plans)))
      (is-plan (third plans) :method "GET" :path "/getChat?chat_id=555"))
    (let ((nct::*telegram-adapter* nil))
      (is (search "not running" (refusal-text error (nct:request "POST" "/getMe")))))))

(deftest channel-telegram-platform (let ((candidate (tg-message
                                                     "{\"message_id\": 12, \"message_thread_id\": 3,
                      \"chat\": {\"id\": 555, \"type\": \"supergroup\"},
                      \"from\": {\"id\": 42, \"username\": \"kim\"},
                      \"text\": \"@OrgBot hi\"}"))))
  ;; What Telegram contributes to the kit host: the @handle stripped from
  ;; the ask, per-chat message ids that carry the chat in their address,
  ;; the chat's whereabouts in the contract, and the platform's own door and
  ;; seams beneath the kit's.
  (is (equal "hi" (nct:telegram-strip-mention "@OrgBot hi" "orgbot")))
  (is (equal "hi @orgbot" (nct:telegram-strip-mention "hi @orgbot" nil)))
  (is (equal "555:12" (nct:telegram-address '(:channel-id "555") "12")))
  (is (equal "90" (nct:telegram-message-id
                   (cell-json "{\"ok\": true, \"result\": {\"message_id\": 90}}"))))
  (is (equal "Telegram chat 555, topic 3 (thread). The bot is @orgbot, user id 777."
             (nct:telegram-where-text candidate "orgbot" "777")))
  (is (equal "Telegram chat 555, topic 3 (thread)."
             (nct:telegram-where-text candidate nil nil)))
  (let* ((host (test-host :platform (nct:telegram-platform :bot-username "orgbot"
                                                           :bot-user-id "777")
                          :name "tg-platform-test" :owners '("42")))
         (ask (nck:build-ask host candidate "telegram-555-t3-m12" nil))
         (contract (nck:ask-contract ask)))
    (is-shape ask (nck:ask-prompt "kim (operator) [m12 u42]: hi") (nck:ask-room "telegram-555-t3")
      (nck:ask-target '(:channel-id "555" :thread-id "3" :message-id "12")))
    (is-carrying contract
      "one lane of a shared Telegram chat"
      (:absent "Where you are"
       "where the lane runs rides the live tail, not the chat's contract")
      (is (equal (concatenate 'string
                              "Telegram chat 555, topic 3 "
                              "(thread). The bot is @orgbot, user id 777.")
                 (nck:ask-where ask)))
      "(Telegram user id 42)"
      ("[m<message id> u<user id>]" "and explains the handle bracket")
      ("(nct:request \"POST\" PATH" "and teaches the one door to the Bot API")
      (is (search nct::+telegram-api-primer+ contract))
      "do not send it again yourself"
      ("nct:telegram-handle-update (adapter update)"
       "then the adapter's own seam, naming the raw-update seam")
      ("channels.telegram.allowed_updates" "and the config key that widens what reaches it")
      ("nck:deliver-answer (host lane digest)" "beneath the kit's own seams"))))

(deftest channel-telegram-handle-update-is-the-advised-seam ()
  ;; Every raw update reaches TELEGRAM-HANDLE-UPDATE, so advice on it sees
  ;; the kinds the adapter itself drops; the poll's boundary sits outside
  ;; the seam, so advice that signals costs one update, not the poll.
  (let* ((seen '())
         (executor (scripted-executor (nck:make-scripted-response
                                       200 (cell-json
                                            "{\"ok\": true,
                                             \"result\": [{\"update_id\": 7,
                                                          \"message_reaction\": {\"chat\": {\"id\": 555}}},
                                                         {\"update_id\": 8,
                                                          \"edited_message\": {\"chat\": {\"id\": 555}}}]}"))))
         (adapter (test-telegram-adapter
                   :executor executor
                   :lanes "handle-update-test"
                   :allowed-updates '("message" "message_reaction"
                                      "edited_message")))
         (name 'nct:telegram-handle-update))
    (setf nlk:*hooks* '())
    (nlk:with-cleanup ((nlk:unhook name "probe"))
      (nlk:hook name "probe"
                (lambda (next adapter update)
                  (push (gethash "update_id" update) seen)
                  (when (gethash "edited_message" update)
                    (error "advice boom"))
                  (funcall next adapter update)))
      (multiple-value-bind (warnings verdict)
          (warnings-of (nct::telegram-poll-lap adapter (lambda () nil)))
        (is (realp verdict) "the lap returns its interval, not :stop")
        (is (and (= 1 (length warnings))
                 (search "advice boom" (first warnings)))))
      (is (equal '(7 8) (reverse seen)))
      (is (= 9 adapter.offset))
      (is-present (plan (first (nck:recording-executor-plans executor))) "the poll plan"
        (is (equalp #("message" "message_reaction" "edited_message")
                    (plan-field plan "allowed_updates")))))))

(deftest channel-telegram-a-press-says-its-line ()
  ;; A press on a choice is the presser saying its line where the card is, a
  ;; reply to it, admitted as typed words are; its answer stops the button's
  ;; spinner, then becomes the card's next view in place. A private answer --
  ;; Telegram shows nobody a message alone -- and a command typed have no
  ;; return path but the chat. Any other press is a control.
  (with-temp-store ()
    (let* ((data (nck:press-token "/cron show nightly" :back "/cron"))
           (seen '())
           (adapter (test-telegram-adapter :lanes "press-test"))
           (update (lambda (data)
                     (nlk:json-object
                      "update_id" 7
                      "callback_query"
                      (nlk:json-object "id" "cb1" "data" data
                                       "from" (nlk:json-object "id" 42 "username" "kim")
                                       "message" (nlk:json-object "message_id" 90 "text" "## Cron (2)"
                                                                  "chat" (nlk:json-object "id" 555 "type" "private")))))))
      (with-stubbed-fdefinition (nck:handle-candidate (host candidate) (push candidate seen))
        (with-stubbed-fdefinition (nck:control-pressed (host payload) (push payload seen))
          (nct:telegram-handle-update adapter (funcall update data))
          (nct:telegram-handle-update adapter (funcall update "nck:details:s1"))))
      (destructuring-bind (control press) seen
        (is-source press :text "/cron show nightly" "chat_kind" "direct_message" "channel_id" "555"
                   "user_id" "42" "reply_to_message_id" "90" "callback_query_id" "cb1"
                   "press_data" data "card_text" "## Cron (2)" "message_id" nil)
        (is (nck:candidate-pressed-p press))
        (is (nck:candidate-addressed-p press))
        (is (equal "/cron" (nlk:json-value (nck:candidate-press press) :text "back")))
        (is (equal "nck:details:s1" (getf control :data)) "any other press is a control")
        (is-plan (nct:press-response-plan press nil) :path "/answerCallbackQuery" "callback_query_id" "cb1")
        (let ((edit (nct:press-response-plan press "## Cron (2)" :controls (list (nck:choice "Refresh" "/cron")))))
          (is-plan edit :path "/editMessageText" :retry t "chat_id" "555")
          (is (eql 90 (plan-field edit "message_id")) "the card pressed is the message edited")
          (is (equal "/cron" (nck:said-line
                               (gethash "callback_data"
                                        (aref (aref (gethash "inline_keyboard" (plan-field edit "reply_markup")) 0) 0))))))
        (is (null (nct:press-response-plan press "refused" :private t)))
        (is (null (nct:press-response-plan (tg-message "{\"message_id\": 12, \"chat\": {\"id\": 555}, \"text\": \"/cron\"}")
                                           nil)) "a command typed is answered in the chat")))))

(deftest channel-telegram-platform-carries-files ()
  ;; The kit's file seam over Telegram's own plan: the platform answers a
  ;; plan, the file's bytes decide which call, and a file that is not there
  ;; is not a photo.
  (let ((platform (nct:telegram-platform :bot-username "orgbot"
                                         :bot-user-id "777"))
        (path (temp-path "pea" "png")))
    (nlk:with-cleanup ((ignore-errors (delete-file path)))
      (alexandria:write-byte-vector-into-file
       (coerce #(137 80 78 71 13 10 26 10 0 0 0 0) '(vector (unsigned-byte 8)))
       path :if-exists :supersede)
      (is (nct::telegram-image-file-p path))
      (let ((plan (funcall (nck:platform-plan-file platform) '(:channel-id "1")
                           path :content "look")))
        (is-plan plan :path "/sendPhoto" :label "send_photo")
        (is (>= (nck:request-plan-timeout-seconds plan) 60)))))
  (is (not (nct::telegram-image-file-p (temp-path "absent" "png")))))

(deftest channel-telegram-platform-publishes-the-menu ()
  ;; What Telegram contributes for commands: the catalog becomes one
  ;; setMyCommands, and a command's answer is a reply in the chat — a
  ;; Telegram message has no return path of its own.
  (let ((platform (nct:telegram-platform :bot-username "orgbot")))
    (is-present (plans (funcall (nck:platform-plan-commands platform)
                                (list (list :name "help"
                                            :description "Show commands"
                                            :usage ""))
                                :timeout-seconds 5)) "the menu is one plan"
      (is (= 1 (length plans)))
      (is-plan (first plans) :path "/setMyCommands" :timeout 5)
      (is (equal "help" (gethash "command" (aref (plan-field (first plans) "commands") 0)))))
    (is (eq (fdefinition 'nct:press-response-plan) (nck:platform-plan-respond platform)) "a press is answered in place; a command typed, in the chat (PRESS-RESPONSE-PLAN)")))

(deftest channel-telegram-file-fetchers ()
  ;; A photo or a recording admitted from Telegram gains the thunk that
  ;; downloads it — built on the poll thread, run where the ask's turn is
  ;; prepared — and one the platform declares over the ceiling is refused
  ;; before anything reaches the network.
  (let ((adapter (test-telegram-adapter :bot-username "orgbot")))
    (let* ((candidate (tg-said 21 "\"caption\": \"how many calories is this\",
                                 \"photo\": [{\"file_id\": \"f1\",
                                              \"width\": 90, \"height\": 90,
                                              \"file_size\": 500}],
                                 \"voice\": {\"file_id\": \"v1\", \"duration\": 3},
                                 \"reply_to_message\": {\"message_id\": 20,
                                                        \"voice\": {\"file_id\": \"v0\"}}"))
           (attachments (gethash "attachments" candidate)))
      (nct::attach-telegram-file-fetchers adapter candidate)
      (is (every (lambda (attachment) (functionp (gethash "fetch" attachment)))
                 attachments))
      (is (functionp (gethash "fetch" (aref (gethash "attachments" (gethash "reply" candidate))
                                            0))))
      (is (= 2 (length (nck:candidate-attachments candidate)))))
    ;; Over the ceiling: refused before any download is attempted.
    (let* ((candidate (tg-said 22 "\"document\": {\"file_id\": \"d1\",
                                                \"file_name\": \"big.png\",
                                                \"mime_type\": \"image/png\",
                                                \"file_size\": 999999999}"))
           (image (aref (gethash "attachments" candidate) 0)))
      (nct::attach-telegram-file-fetchers adapter candidate)
      (is-values (kind data note) (nck::read-attachment image)
        (kind null "nothing to carry") (data null) ((search "ceiling" note) is)
        ((search "http" note) not)))))

(defun telegram-gap-adapter (&key executor (lanes "tg-gap-test") policy)
  "TEST-TELEGRAM-ADAPTER as orgbot, user 777, under POLICY."
  (let ((adapter (test-telegram-adapter :executor executor :lanes lanes
                                        :bot-username "orgbot" :bot-user-id "777")))
    (when policy (setf (slot-value adapter.host 'nck::policy) policy))
    adapter))

(deftest channel-telegram-an-edit-that-names-the-bot-is-an-ask ()
  ;; As on Discord: an edit wakes the bot exactly when its new words would
  ;; have, typed fresh -- the handle, a wake word -- and never for a message
  ;; that already opened a lane.
  (let* ((seen '())
         (adapter (telegram-gap-adapter
                   :policy (nct:telegram-inbound-policy :allowed-chats '("-100555") :require-mention t
                                                        :mention-patterns '("nodecode")
                                                        :bot-username "orgbot")))
         (taken nil))
    (flet ((edit (text)
             (nct:telegram-handle-update
              adapter (cell-json (format nil "{\"update_id\": 3, \"edited_message\": ~a}"
                                         (tg-json 12 (format nil "\"text\": ~s" text)
                                                  :chat -100555 :type "supergroup"))))))
      (with-stubbed-fdefinition (nck:handle-candidate (host candidate) (push candidate seen))
        (with-stubbed-fdefinition (nck:candidate-taken-p (host candidate) taken)
          (is (edit "@orgbot what changed?") "the handle, added by the edit")
          (is (edit "nodecode: what changed?") "a wake word")
          (is (null (edit "what changed?")) "an edit that addresses nobody")
          (setf taken t)
          (is (null (edit "@orgbot what changed, again?")) "a message that already opened a lane"))))
    (is (= 2 (length seen)))
    (is-source (first (last seen)) :text "@orgbot what changed?" "message_id" "12")))

(deftest channel-telegram-the-catch-up-answers-what-came-while-away ()
  ;; Telegram keeps what nobody polled for and hands it over at the next
  ;; poll. A message said just now is just now; one from while the bot was
  ;; away is answered marked late, unless it is older than catch_up_minutes
  ;; (0: none are) or already opened a lane before the gap.
  (let ((adapter (telegram-gap-adapter))
        (taken nil))
    (flet ((said (minutes-ago)
             (nlk:json-object "text" "hi"
                              "source" (nlk:json-object "chat_kind" "direct_message" "channel_id" "42"
                                                        "user_id" "42" "message_id" "12"
                                                        "sent_ms" (- (nct::unix-ms) (* minutes-ago 60000))))))
      (with-stubbed-fdefinition (nck:candidate-taken-p (host candidate) taken)
        (is (null (gethash "late_minutes" (nck:candidate-source (nct::telegram-catch-up adapter (said 0))))))
        (is (eql 12 (gethash "late_minutes" (nck:candidate-source (nct::telegram-catch-up adapter (said 12))))))
        (is (null (nct::telegram-catch-up adapter (said 90))) "past catch_up_minutes (60)")
        (setf taken t)
        (is (null (nct::telegram-catch-up adapter (said 12))) "answered before the gap")
        (setf taken nil (slot-value adapter 'nct::catch-up-minutes) 0)
        (is (null (nct::telegram-catch-up adapter (said 12))) "catch_up_minutes 0 answers none")
        (is (nct::telegram-catch-up adapter (said 0)) "and still answers what is said now")))))

(deftest channel-telegram-a-reaction-reaches-the-kit-as-context ()
  (let* ((seen '())
         (adapter (telegram-gap-adapter)))
    (with-stubbed-fdefinition (nck:reaction-noticed (host candidate) (push candidate seen) :observe)
      (nct:telegram-handle-update
       adapter (cell-json "{\"update_id\": 4, \"message_reaction\": {\"chat\": {\"id\": 42, \"type\": \"private\"},
                           \"message_id\": 90, \"date\": 1700000000, \"user\": {\"id\": 42, \"username\": \"kim\"},
                           \"old_reaction\": [], \"new_reaction\": [{\"type\": \"emoji\", \"emoji\": \"🔥\"}]}}")))
    (is (= 1 (length seen)))
    (is-source (first seen) :text "🔥" "reaction" "add" "reply_to_message_id" "90"
               "chat_kind" "direct_message")))

(deftest channel-telegram-a-reaction-in-a-topic-lands-in-that-topic ()
  ;; Telegram's message_reaction names the chat and the message, never the
  ;; forum topic. The lane that posted the message knows its topic, so the
  ;; line is the topic's context, read by the topic's next ask -- not the
  ;; General room's, where no ask about it would ever look.
  (let* ((seen '())
         (adapter (telegram-gap-adapter))
         (lanes (nck:host-lanes (nct::telegram-adapter-host adapter))))
    (nck:intern-lane lanes "telegram--100555-t77-m88"
                     :target '(:channel-id "-100555" :thread-id "77" :message-id "88")
                     :trigger-message-id "88" :parent-session-id "telegram--100555-t77"
                     :owner-id "42")
    (nck:bind-lane-address lanes "-100555:90" "telegram--100555-t77-m88")
    (flet ((react (message-id)
             (nct:telegram-handle-update
              adapter (cell-json (format nil "{\"update_id\": 5, \"message_reaction\": {\"chat\": {\"id\": -100555, \"type\": \"supergroup\", \"is_forum\": true}, \"message_id\": ~a, \"date\": 1700000000, \"user\": {\"id\": 42, \"username\": \"kim\"}, \"old_reaction\": [], \"new_reaction\": [{\"type\": \"emoji\", \"emoji\": \"🔥\"}]}}" message-id)))))
      (with-stubbed-fdefinition (nck:reaction-noticed (host candidate) (push candidate seen) :observe)
        (react 90)
        (react 91)))
    (is (= 2 (length seen)))
    (is-source (second seen) :text "🔥" "reply_to_message_id" "90" "channel_id" "-100555"
               "thread_id" "77")
    ;; a message no lane holds keeps no topic: the kit reads it as nothing of ours
    (is (null (gethash "thread_id" (nck:candidate-source (first seen)))))))

(deftest channel-telegram-roles-are-read-off-the-admin-list ()
  ;; A group's admins hold \"administrator\", its creator \"creator\" too, read
  ;; once per group per five minutes.
  (let* ((executor (scripted-executor
                    (nck:make-scripted-response
                     200 (cell-json "{\"ok\": true, \"result\": [
                         {\"status\": \"creator\", \"user\": {\"id\": 42}},
                         {\"status\": \"administrator\", \"user\": {\"id\": 43}}]}"))))
         (adapter (telegram-gap-adapter :executor executor)))
    (is (equal '("creator" "administrator") (nct::telegram-author-roles adapter "-100555" "42")))
    (is (equal '("administrator") (nct::telegram-author-roles adapter "-100555" "43")))
    (is (null (nct::telegram-author-roles adapter "-100555" "44")))
    (is (= 1 (length (nck:recording-executor-plans executor))) "one read serves the five minutes")
    (is-plan (first (nck:recording-executor-plans executor)) :path "/getChatAdministrators")))

(deftest channel-telegram-a-forum-ask-opens-a-topic ()
  ;; Where the chat is a forum, the kit's thread path opens a topic for the
  ;; ask; anywhere else the platform opens none and the ask runs where it was
  ;; typed. Close and remove name the topic's chat, which the chats showed.
  (let* ((platform (nct:telegram-platform :bot-username "orgbot" :bot-user-id "777"
                                          :forum-p (lambda (chat) (equal chat "-100555"))))
         (thread (nck:platform-plan-thread platform))
         (nct::*telegram-adapter* nil))
    (is-plan (funcall thread '(:channel-id "-100555") "Fix the build" :message-id "12")
             :path "/createForumTopic" "chat_id" "-100555")
    (is (null (funcall thread '(:channel-id "-100999") "Fix the build")) "a group that is no forum")
    (is (equal "7" (funcall (nck:platform-thread-id-of platform)
                            (cell-json "{\"ok\": true, \"result\": {\"message_thread_id\": 7}}"))))
    (setf (gethash "7" nct::*topic-chats*) "-100555")
    (is-plan (funcall (nck:platform-plan-close-thread platform) "7")
             :path "/closeForumTopic" "chat_id" "-100555" "message_thread_id" 7)
    (is-plan (funcall (nck:platform-plan-delete-thread platform) "7")
             :path "/deleteForumTopic" "chat_id" "-100555")
    (is (null (funcall (nck:platform-plan-close-thread platform) "8")) "a topic in no chat it knows")
    (is (search "admin can reopen" (nck:platform-closed-note platform)))
    (setf (gethash "42" nct::*telegram-names*) "kim")
    (is (equal "[kim](tg://user?id=42)" (funcall (nck::platform-mention platform) "42")))
    (remhash "7" nct::*topic-chats*)))

(deftest channel-telegram-a-press-names-its-topic ()
  ;; A press on a message in a topic carries the topic, so Close closes it.
  (let ((seen '())
        (adapter (telegram-gap-adapter)))
    (with-stubbed-fdefinition (nck:control-pressed (host payload) (push payload seen))
      (nct:telegram-handle-update
       adapter (nlk:json-object
                "update_id" 7
                "callback_query"
                (nlk:json-object "id" "cb1" "data" "nck:close:42"
                                 "from" (nlk:json-object "id" 42 "username" "kim")
                                 "message" (nlk:json-object "message_id" 90 "message_thread_id" 7
                                                            "chat" (nlk:json-object "id" -100555 "type" "supergroup"))))))
    (is (equal "7" (getf (first seen) :thread-id)))
    (is (equal "-100555" (getf (first seen) :channel-id)))
    (is (equal "-100555" (gethash "7" nct::*topic-chats*)))
    (remhash "7" nct::*topic-chats*)))

(deftest channel-telegram-close-closes-the-topic-it-was-pressed-in ()
  ;; The kit's Close press reads the topic the payload names (a Telegram
  ;; topic lives inside its chat, so the chat is no thread), closes it by
  ;; its chat, and tells the presser in Telegram's terms: locked until an
  ;; admin reopens it.
  (let* ((executor (scripted-executor (nck:make-scripted-response
                                       200 (cell-json "{\"ok\": true, \"result\": true}"))))
         (host (test-host :platform (nct:telegram-platform :bot-username "orgbot" :bot-user-id "777")
                          :executor executor :name "tg-close-test"))
         (nct::*telegram-adapter* nil))
    (setf (gethash "7" nct::*topic-chats*) "-100555")
    (nck::close-pressed host (list :id "cb1" :data "nck:close:42" :user-id "42"
                                   :message-id "90" :channel-id "-100555" :thread-id "7")
                        "42")
    (let ((plans (nck:recording-executor-plans executor)))
      (is-present (ack (find "/answerCallbackQuery" plans :key #'nck:request-plan-path :test #'equal))
        "the presser is answered"
        (is (search "admin can reopen" (plan-field ack "text"))))
      (is-present (close (find "/closeForumTopic" plans :key #'nck:request-plan-path :test #'equal))
        "the topic pressed in is closed"
        (is-plan close "chat_id" "-100555" "message_thread_id" 7)))
    (remhash "7" nct::*topic-chats*)))

(deftest channel-telegram-a-refused-close-is-said-in-its-topic ()
  ;; A close Telegram refuses leaves the topic open and says so in it -- in
  ;; the topic's chat, never in a chat named by the topic's id; a topic
  ;; someone closed already is closed, no refusal.
  (flet ((press (close-answer)
           (let* ((executor (scripted-executor
                             (nck:make-scripted-response 200 (cell-json "{\"ok\": true, \"result\": true}"))
                             close-answer))
                  (host (test-host :platform (nct:telegram-platform :bot-username "orgbot" :bot-user-id "777")
                                   :executor (nct:wrap-telegram-executor executor) :name "tg-close-refused"))
                  (nct::*telegram-adapter* nil))
             (setf (gethash "7" nct::*topic-chats*) "-100555")
             (unwind-protect
                  (progn (nck::close-pressed host (list :id "cb1" :data "nck:close:42" :user-id "42"
                                                        :message-id "90" :channel-id "-100555" :thread-id "7")
                                             "42")
                         (find "/sendRichMessage" (nck:recording-executor-plans executor)
                               :key #'nck:request-plan-path :test #'equal))
               (remhash "7" nct::*topic-chats*)))))
    (is-present (said (press (nck:make-scripted-response
                              400 (cell-json "{\"ok\": false, \"error_code\": 400, \"description\": \"Bad Request: not enough rights to manage topics\"}"))))
      "the refusal is said"
      (is-plan said "chat_id" "-100555" "message_thread_id" 7)
      (is (search "did not close" (gethash "markdown" (plan-field said "rich_message")))))
    (is (null (press (nck:make-scripted-response
                      400 (cell-json "{\"ok\": false, \"error_code\": 400, \"description\": \"Bad Request: TOPIC_NOT_MODIFIED\"}")))) "closed already is closed")))
