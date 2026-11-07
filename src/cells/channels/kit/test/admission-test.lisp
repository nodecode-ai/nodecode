;;;; admission-test.lisp --- pure inbound policy decisions.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun channel-policy (&rest args)
  (apply #'nck:make-inbound-policy
         (append args (list :reasons '(:thread-not-supported "forum_topics_disabled")
                            :require-mention
                            (getf args :require-mention nil)))))

(deftest channel-lane-invocation-names-the-room ()
  ;; What a room's turn says about where it came from: the kind, and the
  ;; lane as the id, so the roster can say which room a running turn
  ;; belongs to.
  (let* ((invocation (nck::lane-invocation "lane-7-msg-99" "lane-7" "turn.steer"))
         (source (nlk:invocation-source invocation)))
    (is (equal "channel" (nlk:invocation-source-kind source)) "a room admits as a channel, not as this image talking to itself")
    (is (equal "lane-7" (nlk:invocation-source-id source)) "and the lane is what it names")
    (is (equal "turn.steer" (nlk:invocation-action invocation)) "the action is the gateway's own spelling")
    (is (equal "lane-7-msg-99" (nlk:invocation-command-id invocation)) "the command is the one the submit carries")
    (is (equal "lane-7-msg-99" (nlk:invocation-correlation-id invocation)) "and it correlates to itself")
    (is (equal "in_process" (nlk:invocation-transport-domain (nlk:invocation-transport invocation))) "the transport is this image: no socket between a room and its turn"))
  (is (equal "turn.start" (nlk:invocation-action (nck::lane-invocation "c" "l" "turn.start"))) "and a first message opens a turn rather than steering one"))

(deftest channel-admission-mention-matchers ()
  (is (nck:discord-mention-p "hey <@123> hi" "123"))
  (is (nck:discord-mention-p "hey <@!123> hi" "123"))
  (is (not (nck:discord-mention-p "hey @123 hi" "123")))
  (is (not (nck:discord-mention-p "hey <@1234> hi" "123")))
  (is (not (nck:discord-mention-p "no mention" "")))
  (is (nck:discord-mention-p "hey <@&456> hi" "123" '("456")))
  (is (not (nck:discord-mention-p "hey <@&456> hi" "123" '("789"))))
  (is (not (nck:discord-mention-p "hey <@&456> hi" "123")))
  (is (nck:telegram-mention-p "ping @MyBot please" "mybot"))
  (is (nck:telegram-mention-p "ping @mybot" "@MyBot"))
  (is (not (nck:telegram-mention-p "no ping" "mybot"))))

(deftest channel-admission-allowlists ()
  (let ((policy (channel-policy :allowed-channels '("100")
                                :allowed-users '("42"))))
    (is-inbound policy (test-candidate :channel "100" :user "42")
                :answer)
    (is-inbound policy (test-candidate :channel "999" :user "42") :reject "channel_not_allowed")
    (is-inbound policy (test-candidate :channel "100" :user "7")
                :reject "user_not_allowed"))
  ;; Empty allowlists restrict nothing; "*" allows everything.
  (is-inbound (channel-policy)
              (test-candidate :channel "any" :user "any") :answer)
  (is-inbound (channel-policy :allowed-channels '("*")) (test-candidate :channel "whatever")
              :answer))

(deftest channel-admission-discord-channel-matching ()
  ;; :discord matching admits a thread whose PARENT is allowlisted.
  (let ((policy (channel-policy :allowed-channels '("parent-1")
                                :channel-match :discord)))
    (is-inbound policy (test-candidate :thread "thread-9" :parent "parent-1") :answer))
  ;; :exact matching does not.
  (let ((policy (channel-policy :allowed-channels '("parent-1")
                                :channel-match :exact)))
    (is-inbound policy (test-candidate :thread "thread-9" :parent "parent-1")
                :reject "channel_not_allowed")))

(deftest channel-admission-bot-and-self-authors ()
  ;; Another bot is let in as allow_bots says: none, by mention, or all. The
  ;; bot's own message never is: in a thread it speaks freely in, its answer
  ;; would be its next ask.
  (let ((policy (channel-policy :self-id "999")))
    (is-inbound policy (test-candidate :is-bot t)
                :reject "bot_author" "bot authors reject by default")
    (is-inbound policy (test-candidate :user "999")
                :reject "self_author" "the bot's own messages reject"))
  (is-inbound (channel-policy :allow-bots :all :self-id "999") (test-candidate :is-bot t) :answer)
  (is-inbound (channel-policy :allow-bots :all :self-id "999") (test-candidate :user "999" :is-bot t)
              :reject "self_author" "every bot let in, and still not itself")
  (let ((policy (channel-policy :allow-bots :mentions :self-id "999" :mention-target "999"
                                :mention-test #'nck:discord-mention-p)))
    (is-inbound policy (test-candidate :is-bot t :text "<@999> status?") :answer)
    (is-inbound policy (test-candidate :is-bot t :text "status?")
                :reject "bot_not_mentioned" "a bot let in by mention only")
    (is-inbound policy (test-candidate :text "status?") :answer nil "a person needs no mention")))

(deftest channel-admission-roles-and-pairing ()
  ;; Who may talk is three grants in one gate: a listed user, one holding a
  ;; listed role, one the operator paired.
  (let ((policy (channel-policy :allowed-users '("42") :allowed-roles '("dev"))))
    (is-inbound policy (test-candidate :user "42") :answer)
    (is-inbound policy (test-candidate :user "7" :roles '("ops" "dev")) :answer nil "one listed role")
    (is-inbound policy (test-candidate :user "7" :roles '("ops")) :reject "user_not_allowed")
    (is-inbound policy (test-candidate :user "7" :kind "direct_message")
                :reject "user_not_allowed" "a direct message carries no roles")
    (setf (gethash "test" nck::*paired-users*) '("7"))
    (unwind-protect
         (is-inbound policy (test-candidate :user "7" :kind "direct_message") :answer nil "paired")
      (remhash "test" nck::*paired-users*)))
  (is-inbound (channel-policy :allowed-roles '("dev")) (test-candidate :user "7")
              :reject "user_not_allowed" "a role list alone restricts"))

(deftest channel-admission-kind-policies ()
  (is-inbound (channel-policy :dm-policy :disabled)
              (test-candidate :kind "direct_message")
              :reject "dm_disabled")
  (is-inbound (channel-policy :group-policy :disabled)
              (test-candidate :kind "group")
              :reject "group_disabled")
  (is-inbound (channel-policy :thread-policy :disabled)
              (test-candidate :thread "t1")
              :reject "forum_topics_disabled")
  (is-inbound (channel-policy :allowed-thread-ids '("t1"))
              (test-candidate :thread "t2")
              :reject "thread_not_allowed"))

(deftest channel-admission-mention-requirement ()
  (let ((policy (channel-policy :require-mention t
                                :mention-target "42"
                                :mention-test #'nck:discord-mention-p)))
    (is-inbound policy (test-candidate :text "yo <@42> hello") :answer)
    (is-inbound policy (test-candidate :text "no mention")
                :observe "mention_required"
                "an unaddressed message in a readable room is context, not a prompt")
    (is-inbound policy (test-candidate :kind "direct_message" :text "no mention") :answer))
  (is-inbound (channel-policy :require-mention t
                              :mention-test #'nck:discord-mention-p)
              (test-candidate :text "anything")
              :reject "mention_required_without_user"
              "require_mention with no known bot identity rejects, loudly"))

(deftest channel-admission-elsewhere-addresses ()
  ;; The mention gate's negative twin: a message that opens by addressing
  ;; another participant — a leading entity mention of another user, or a
  ;; leading name among the platform's other addressees — and carries
  ;; nothing that involves the bot is that participant's: observed, never
  ;; answered, whatever the room's speaking mode. The reported miss:
  ;; "ada, you need to do something".
  (nck:set-other-addressees "test" '("ada"))
  (let ((policy (channel-policy)))
    (is-inbound policy (test-candidate :text "ada, you need to do something")
                :observe "addressed_elsewhere"
                "a message opening by another name is not an ask")
    (is-inbound policy (test-candidate :text "Ada, you need to do something")
                :observe "addressed_elsewhere"
                "the name matches case-insensitively")
    (is-inbound policy (test-candidate :text "yo scrap") :answer)
    (is-inbound policy (test-candidate :text "hello") :answer)
    (is-inbound policy (test-candidate :text "ada needs a look, can you restart it?") :answer)
    (is-inbound policy (test-candidate :kind "direct_message"
                                       :text "ada, you need to do something")
                :answer))
  ;; A leading entity mention of another user is the same fact with no name
  ;; list at all.
  (let ((policy (channel-policy :mention-target "999")))
    (is-inbound policy (test-candidate :text "<@123> can you look at this")
                :observe "addressed_elsewhere")
    (is-inbound policy (test-candidate :text "<@999> can you look at this") :answer)
    (is-inbound (channel-policy) (test-candidate :text "<@123> can you look at this") :answer))
  ;; Addressing us wins; and in a mention-gated room the mention gate still
  ;; has the first word.
  (let ((policy (channel-policy :mention-target "999"
                                :mention-test #'nck:discord-mention-p)))
    (is-inbound policy (test-candidate :text "ada, ask <@999> about it") :answer))
  (is-inbound (channel-policy :require-mention t
                              :mention-target "999" :mention-test #'nck:discord-mention-p)
              (test-candidate :text "ada, you need to do something")
              :observe "mention_required"
              "in a gated room the mention gate speaks first")
  (nck:set-other-addressees "test" '()))

(deftest channel-admission-a-room-speaks-freely-where-no-mention-is-taken ()
  ;; The one fact the mention gate turns on and the lane contract states:
  ;; whether the room reaches the bot without a mention. A gated room
  ;; observes an unmentioned line, so a lane never reads a line the room
  ;; did not mean for it; a room that takes no mention means every line
  ;; for it, and its contract says so — no line is ever marked. Without
  ;; this, every unmentioned line in such a room would be called "(not
  ;; addressed to you)", and a voice note, which can never carry a
  ;; mention, would go NO_REPLY.
  (let ((gated (channel-policy :require-mention t :mention-target "999"
                               :mention-test #'nck:discord-mention-p)))
    (is (not (nck::room-speaks-freely-p gated (test-candidate :text "no mention"))))
    (is (nck::room-speaks-freely-p gated (test-candidate :kind "direct_message"
                                                          :text "no mention")))
    (is (nck::room-speaks-freely-p (channel-policy :require-mention t
                                                   :free-response-channels '("100"))
                                   (test-candidate :channel "100" :text "no mention")))
    (is (nck::room-speaks-freely-p (channel-policy :require-mention nil)
                                   (test-candidate :text "no mention")))
    (is (not (nck::room-speaks-freely-p (channel-policy :require-mention nil
                                                        :require-mention-channels '("100"))
                                        (test-candidate :channel "100" :text "no mention"))))
    (is (nck::room-speaks-freely-p nil (test-candidate :text "no mention")))
    ;; The gate and the fact agree: what the fact calls gated, the gate observes.
    (is-inbound gated (test-candidate :text "no mention") :observe "mention_required")
    (is-inbound (channel-policy :require-mention nil) (test-candidate :text "no mention")
                :answer)))

(deftest channel-admission-addressed-waives-the-mention ()
  ;; An adapter marks a candidate `addressed' when the platform itself
  ;; routed it to the bot — a slash interaction, a reply to the bot's own
  ;; message. The mention gate then has nothing to ask; every other gate
  ;; still does.
  (let ((policy (channel-policy :require-mention t :mention-target "999"
                                :mention-test #'nck:discord-mention-p
                                :allowed-users '("42"))))
    (is-inbound policy (test-candidate :user "42" :text "/help") :observe "mention_required")
    (let ((candidate (test-candidate :user "42" :text "/help" :addressed t)))
      (is (nck:candidate-addressed-p candidate))
      (is-inbound policy candidate :answer))
    (is-inbound policy (test-candidate :user "7" :text "/help" :addressed t)
                :reject "user_not_allowed")
    (is (not (nck:candidate-addressed-p (test-candidate :user "42" :addressed "yes"))))))

(deftest channel-admission-speaking-overrides (let ((policy (channel-policy
                                                             :require-mention nil
                                                             :mention-target "42"
                                                             :mention-test #'nck:discord-mention-p
                                                             :channel-match :discord
                                                             :free-response-channels '("free")
                                                             :require-mention-channels '("gated")
                                                             :ignored-channels '("off")))))
  (is-inbound policy (test-candidate :channel "free" :text "hello")
              :answer nil "free response admits unmentioned messages")
  (is-inbound policy (test-candidate :channel "gated" :text "hello")
              :observe "mention_required"
              "require-mention override observes unmentioned messages")
  (is-inbound policy (test-candidate :channel "off" :text "hello")
              :reject "channel_ignored"
              "ignored channels neither answer nor buffer context")
  (is-inbound policy (test-candidate :channel "other" :text "hello")
              :answer nil "the global false default remains free response")
  (is-inbound policy (test-candidate :thread "thread-1" :parent "gated" :text "hello")
              :observe "mention_required"
              "a Discord thread inherits its parent speaking policy")
  (let ((overlap (channel-policy
                  :require-mention nil
                  :free-response-channels '("same")
                  :require-mention-channels '("same")
                  :ignored-channels '("same"))))
    (is-inbound overlap (test-candidate :channel "same" :text "hello")
                :reject "channel_ignored"
                "ignored wins an overlap")))

(deftest channel-candidate-attachments ()
  ;; The candidate shape carries attachments for every platform, and the
  ;; ingress reads them here: a reference each, never the bytes, and nothing
  ;; about them believed yet — the fetch sniffs what they really are.
  (let ((candidate (nlk:json-object
                    "text" "look at this"
                    "attachments" (vector (nlk:json-object "url" "https://x/1.png"
                                                           "media_type" "image/png"
                                                           "filename" "1.png"
                                                           "size" 5)
                                          (nlk:json-object "media_type" "image/png")))))
    (is (= 1 (length (nck:candidate-attachments candidate))))
    (is (equal "https://x/1.png"
               (gethash "url" (first (nck:candidate-attachments candidate)))))
    (is (equal "1.png"
               (gethash "filename" (first (nck:candidate-attachments candidate))))))
  (is (null (nck:candidate-attachments (nlk:json-object "text" "no attachments"))))
  (is (null (nck:candidate-attachments (nlk:json-object "text" "x" "attachments" :null))))
  (is (null (nck:candidate-attachments
             (nlk:json-object "text" "x" "attachments" (vector "junk"))))))

(deftest channel-candidate-attachments-fetch-thunk ()
  ;; A platform that brings its own credential — Telegram's files are
  ;; reached through the adapter — hands the ingress a thunk instead of a
  ;; url; it is an attachment all the same, and its fetch runs the thunk.
  (let ((candidate (nlk:json-object
                    "text" "look at this"
                    "attachments" (vector (nlk:json-object
                                           "fetch" (lambda () "bytes")
                                           "filename" "1.jpg")))))
    (is (= 1 (length (nck:candidate-attachments candidate))))
    (is (equal "1.jpg"
               (gethash "filename"
                        (first (nck:candidate-attachments candidate)))))))

(deftest channel-document-media-types ()
  ;; A text file is read whole — known by its type, or by its name when the
  ;; platform declares none (Discord leaves content_type off many source
  ;; files). Anything else is a file the lane opens with its tools.
  (is-each (nck::document-media-type-p)
    ("text/markdown; charset=utf-8" nil t "a markdown file")
    ("application/json" nil t "a json file")
    ("video/mp4" nil nil "a video file")
    ("application/pdf" nil nil "a pdf")
    (nil nil nil "no declared type")
    (nil "build.log" t "a log by its name")
    ("application/octet-stream" "main.lisp" t "source a platform typed as bytes")
    (nil "photo.heic" nil "an unknown extension")
    (nil "README" nil "no extension")
    ("application/pdf" "q3.pdf" nil "a pdf by name too")))
