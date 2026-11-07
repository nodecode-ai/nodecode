;;;; adapter.lisp --- the Slack adapter: the platform, the socket lap, the door.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane host -- rooms, lanes, the gate, the digest, the status line, the
;;;; answer, the write-back -- is the kit's (host.lisp, room.lisp). This file
;;;; is what Slack contributes: how a message, an edit, a delete and a status
;;;; beat are spelled (rest.lisp, bound into a PLATFORM), how a mention is
;;;; written, where the channel is, and the Socket Mode lap that turns
;;;; envelopes into candidates.
;;;;
;;;; Thread topology (kit rule: no network I/O on a thread that is not ours):
;;;;   - the wsd reader callback: parse + enqueue onto the inbound queue.
;;;;   - channel-slack-socket (supervised lap): opens the socket, drains the
;;;;     queue, acknowledges each envelope, admits messages through the host,
;;;;     pings to find a dead link. A person's display name is read here, once
;;;;     per person, after the envelope is acknowledged.
;;;;   - the host's :FRAME fold and channel-slack-deliver pool: the kit's.

(in-package #:nodecode-channel-slack)

(nlk:access (host channel-host) (result execution))

;; The thread board (board.lisp, loaded after this file).
(declaim (ftype (function (t t) t) start-slack-board slack-board-pressed note-slack-message)
         (ftype (function (t) t) board-noting-executor))

(defparameter +ping-interval-ms+ 15000
  "How often the lap pings Slack.")

(defparameter +dead-link-ms+ 45000
  "How long a socket may go without a frame or a pong before the lap takes
it for dead and opens another.")

(defstruct (slack-adapter (:copier nil) (:constructor %make-slack-adapter))
  (host nil)
  ;; apps.connections.open speaks with the app-level token, everything else
  ;; with the bot's.
  (app-executor nil)
  (bot-token "" :type string)
  (bot-user-id nil :type (or null string))
  (threads (make-thread-book))
  ;; user id -> display name, NIL for one Slack would not name; the lap's.
  (names (make-hash-table :test #'equal))
  ;; channels.slack.debug_reconnects: Slack moves the socket every ~6 minutes.
  (debug-reconnects-p nil :type boolean)
  ;; Liveness, set by the reader's pong callback and read by the lap.
  (heard-ms 0 :type integer)
  ;; The workspace the app is installed in (auth.test).
  (team-id nil :type (or null string))
  ;; channels.slack.allowed_channels: the conversations a catch-up reads,
  ;; every one the bot is in when the section lists none.
  (channels '() :type list)
  ;; channels.slack.catch_up_minutes, and the Unix ms Slack was last heard
  ;; at while connected, with when that was last written down.
  (catch-up-minutes 60 :type integer)
  (heard-until-ms nil :type (or null integer))
  (heard-saved-ms 0 :type integer))

(nlk:access (adapter slack-adapter))

(defvar *slack-adapter* nil
  "The live Slack adapter, or NIL when the lane is stopped.")

;;; --- what Slack contributes to the host -----------------------------------------------

;;; Data, so an operator's layer can reword it; the ids above it are the
;;; adapter's.
(defparameter +slack-api-primer+
  "Slack's Web API is open to you as the bot, from eval:
  (ncs:request METHOD PATH &key body headers timeout) => (:status N :body VALUE)
PATH is any Web API method: a read is a GET with its arguments in the query, (ncs:request \"GET\" \"/conversations.history?channel=<channel>&limit=20\"); a write is a POST with a keyword plist body, (ncs:request \"POST\" \"/reactions.add\" :body (:channel \"<channel>\" :timestamp \"<ts>\" :name \"thumbsup\")). Objects come back as keyword plists, arrays as vectors. Slack answers a refusal with status 200 too: then :error says what Slack refused and why (missing_scope names the scope the app lacks). A message is named by its channel and its ts -- the m... handle on each line is that ts -- and a person is mentioned as <@U...>. The adapter posts your final answer in the ask's thread: do not post it again yourself."
  "What the lane contract says about reaching Slack itself.")

(defun slack-where-text (candidate bot-user-id &aux (target (channel-target candidate)))
  "Where the channel is and who the bot is, for the lane contract: the
conversation, its kind, the bot's own mention -- whichever are known."
  (format nil "Slack ~a ~a~@[ in workspace ~a~].~@[ The bot is <@~a>.~]~
~:[~; Answers go in the thread under the ask.~]"
          (source-field candidate "chat_kind")
          (or (getf target :channel-id) "unknown")
          (source-field candidate "workspace_id")
          bot-user-id
          (not (dm-channel-p (getf target :channel-id)))))

(defun slack-platform (&key bot-user-id (threads (make-thread-book))
                       &aux (platform (slack-platform-record bot-user-id threads)))
  "The Slack PLATFORM (SLACK-PLATFORM-RECORD), and what it carries beside the
record: an answer's table posts as Slack's own. Its markdown block draws one
as a table block up to 100 rows, the header among them, and 20 columns --
past them it drops the rest unsaid -- and refuses a message whose tables'
cells pass 10,000 characters (probed 2026-10-21)."
  (setf (platform-tables platform) '(:rows 100 :columns 20 :characters 10000))
  platform)

(defun slack-platform-record (bot-user-id threads)
  "The Slack PLATFORM: Web API plans, 12,000-character markdown blocks, the
<@U...> mention, per-channel message ts (so an address carries the channel),
replies threaded under their ask, a status beat Slack shows for two minutes,
reactions by name, and a slash command answered through its response_url.
THREADS is the book the plans and the lap share."
  ;; Slack has no command menu an app may write (the /nodecode command is the
  ;; manifest's) and no thread to open (a reply is the thread): those slots
  ;; stay empty. Its messages carry choices -- buttons, menus, lines -- whose
  ;; presses come back over the socket (SLACK-HANDLE-ENVELOPE), and files
  ;; through its upload steps (FILE-PLAN).
  (make-platform
   :id "slack"
   :name "Slack"
   :noun "channel"
   :owner-label "Slack user id"
   :session-prefix "slack"
   :contract-section "slack-channel"
   :text-limit +slack-text-limit+
   :typing-refresh-ms 20000
   :plan-message (lambda (target chunk &rest keys) (apply #'message-plan threads target chunk keys))
   :plan-edit #'edit-plan
   :plan-delete #'delete-plan
   :plan-typing (lambda (target &rest keys) (apply #'typing-plan threads target keys))
   :plan-reaction #'reaction-plans
   :plan-respond #'respond-plan
   :plan-control-ack #'ack-plan
   :plan-file (lambda (target pathname &rest keys) (apply #'file-plan threads target pathname keys))
   :choices t
   :answer-body #'slack-answer-body
   :mention (lambda (user-id) (format nil "<@~a>" user-id))
   :message-id-of #'slack-message-id
   :address-of #'slack-address
   :strip-mention (lambda (text) (slack-strip-mention text bot-user-id))
   :where-text (lambda (candidate) (slack-where-text candidate bot-user-id))
   :api-primer +slack-api-primer+
   :seams-primer "  ncs:slack-handle-event (adapter event &optional team-id) -- every Events API event the app subscribes to, on the socket thread, after Slack has its acknowledgement: return fast. event is the hash table Slack sent (\"type\" names it)."))

;;; --- ingress (socket lap thread) -----------------------------------------------------

(defun slack-user-name (adapter user-id)
  "USER-ID's display name, asked of Slack once per person (users.info), or
NIL when Slack would not say."
  (when user-id
    (multiple-value-bind (name known) (gethash user-id adapter.names)
      (if known
          name
          (let* ((result (execute-plan (host-executor adapter.host)
                                       (rest-plan "GET" (format nil "/users.info?user=~a" user-id)
                                                  "user_info" nil 10)))
                 (user (and result.ok-p (nlk:json-value result.body :object "user"))))
            (setf (gethash user-id adapter.names)
                  (or (nlk:json-value user :text "profile" "display_name")
                      (nlk:json-value user :text "profile" "real_name")
                      (nlk:json-value user :text "name"))))))))

(defun slack-file-fetcher (token url)
  "A thunk that returns the bytes at URL, fetched with the bot TOKEN, or
signals with the whole truth in its message -- never the token."
  ;; Built when the message is admitted, on the lap thread; called when the
  ;; ask's turn is prepared, on the kit's delivery pool, so no download ever
  ;; parks the socket. The ceiling a file may take is the kit's, read off the
  ;; size the entry declares before this runs.
  (lambda ()
    (multiple-value-bind (body status)
        (nlk:with-handlers ((error () (error "the download from Slack failed")))
          (nlk:http :get url :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
                             :binary t :timeout 30))
      (unless (eql 200 status)
        (error "the download from Slack failed with status ~a" status))
      body)))

(defun attach-slack-file-fetchers (adapter candidate)
  "CANDIDATE's attachment entries each gain the thunk that fetches the file
with the bot token."
  (loop for attachment across (nlk:json-array candidate "attachments")
        for url = (nlk:json-value attachment :string "download")
        when url
          do (setf (gethash "fetch" attachment) (slack-file-fetcher adapter.bot-token url)))
  candidate)

(defun name-speaker (adapter candidate)
  "Give CANDIDATE its speaker's display name: asked for only for a line a
person wrote."
  (let ((user (source-field candidate "user_id")))
    (unless (or (candidate-bot-p candidate) (equal user adapter.bot-user-id))
      (setf (gethash "user_name" (gethash "source" candidate))
            (slack-user-name adapter user)))))

(defun add-thread-context (adapter candidate text)
  "Give CANDIDATE -- a message, TEXT its words as Slack wrote them, in a
thread the bot does not speak in, that says something to the bot -- the
thread's first message as the message it answers: an ask in someone else's
thread reads what the thread is about."
  ;; One read of the thread's first message (conversations.replies), on this
  ;; thread as a name is, and only for a message the bot will answer: a
  ;; thread the bot speaks in is its lane's already, and chatter is none.
  (let ((root (source-field candidate "reply_to_message_id"))
        (channel (source-field candidate "channel_id")))
    (when (and root channel
               (not (eq t (gethash "addressed" (gethash "source" candidate))))
               (mention-involves-p (host-policy adapter.host) text))
      (let* ((result (execute-plan (host-executor adapter.host)
                                   (rest-plan "GET" (format nil "/conversations.replies?channel=~a&ts=~a&limit=1"
                                                            channel root)
                                              "thread_root" nil 10)))
             (messages (and (execution-ok-p result)
                            (nlk:json-array (execution-body result) "messages")))
             (first (and messages (plusp (length messages)) (aref messages 0))))
        (when (and (hash-table-p first) (equal root (nlk:json-value first :string "ts")))
          (let ((reply (slack-reply-context first)))
            (setf (gethash "user_name" reply)
                  (or (slack-user-name adapter (gethash "user_id" reply)) (gethash "user_name" reply)))
            (attach-slack-file-fetchers adapter reply)
            (setf (gethash "reply" candidate) reply)))))))

(defun slack-handle-event (adapter event &optional team-id)
  "One Events API EVENT, as Slack sent it, on the socket lap: a message --
or an edit whose new words say something to the bot -- maps to the shared
candidate and goes through the host, and a reaction reaches the kit as a
line said without typing one; every other event falls through."
  ;; Exported as the seam a layer advises -- (hook 'ncs:slack-handle-event key
  ;; fn) sees joins, whatever the app subscribes to -- with the lap's own
  ;; rule: return fast, this thread is the one that acknowledges. Every
  ;; message in a thread is noted in the thread book first, so a reply to it
  ;; lands in its thread. An edit wakes the bot exactly when its new words
  ;; involve it, by the room's own mention rule (NCK:MENTION-INVOLVES-P): an
  ;; edit of an ask already taken is the same message, which the kit takes
  ;; once. Returns the candidate admitted, or NIL.
  (let ((threads adapter.threads)
        (host adapter.host))
    (if (member (nlk:json-value event :string "type") '("reaction_added" "reaction_removed")
                :test #'equal)
        (nlk:when-let (candidate (slack-reaction-candidate
                                  event :team-id team-id :bot-user-id adapter.bot-user-id
                                  :root-of (lambda (channel ts) (thread-root threads channel ts))))
          (name-speaker adapter candidate)
          (reaction-noticed host candidate)
          candidate)
        (let* ((edited (slack-edited-message event))
               (message (or edited event))
               (text (nlk:json-value message :string "text")))
          ;; What it says of the bot's threads reaches the thread board first.
          (nlk:with-handlers ((error (condition) (warn "slack: the board could not note a message: ~a" condition)))
            (note-slack-message adapter event))
          (note-thread-message threads (nlk:json-value message :string "channel")
                               (nlk:json-value message :string "ts")
                               (nlk:json-value message :string "thread_ts"))
          (nlk:when-let (candidate (and (or (null edited) (mention-involves-p (host-policy host) text))
                                        (slack-message-candidate
                                         message :team-id team-id
                                         :bot-user-id adapter.bot-user-id
                                         :bot-thread-p (lambda (channel root)
                                                         (bot-thread-p threads channel root)))))
            (name-speaker adapter candidate)
            (add-thread-context adapter candidate text)
            (handle-candidate host (attach-slack-file-fetchers adapter candidate))
            candidate)))))

(defun candidate-bot-p (candidate)
  (eq t (gethash "is_bot" (gethash "source" candidate))))

;;; --- catching up -------------------------------------------------------------------
;;; Slack keeps no events for an app with no socket open: what was said while
;;; the bot was away -- a restart, a machine that was off, a dead link -- is
;;; read back from the conversations once a socket says hello again, and each
;;; message the bot missed is handed on, marked late. channels.slack.
;;; catch_up_minutes bounds how far back: 60 unless set, 0 never.

(defparameter +heard-key+ "heard-until"
  "The session-state key under channel-slack the last-heard mark is kept in.")

(defparameter +catch-up-messages+ 50
  "The messages one read of a conversation or a thread takes.")

(defparameter +catch-up-replays+ 100
  "The most messages one catch-up hands on.")

(defparameter +catch-up-gap-ms+ 10000
  "A socket away for less than this missed nothing worth a read: Slack opens
the next before it closes the last.")

(defun unix-ms ()
  "Milliseconds since the Unix epoch, now: the clock a ts counts and a
restart keeps."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun ts-ms (ts)
  "Slack's TS -- seconds and microseconds, 1700000000.000100 -- as Unix
milliseconds, or NIL for no ts."
  (ignore-errors
   (let ((dot (position #\. ts)))
     (+ (* 1000 (parse-integer ts :end dot))
        (if dot
            (floor (parse-integer (subseq (concatenate 'string (subseq ts (1+ dot)) "000000") 0 6))
                   1000)
            0)))))

(defun ms-ts (ms)
  "Unix milliseconds MS as a Slack ts."
  (format nil "~d.~6,'0d" (floor ms 1000) (* 1000 (mod ms 1000))))

(defun note-heard (adapter &key force &aux (now (unix-ms)))
  "Slack was heard now, the socket open: written down once a minute -- or
now, FORCE -- so a crash loses at most that much of the mark."
  (setf adapter.heard-until-ms now)
  (when (or force (> (- now adapter.heard-saved-ms) 60000))
    (setf adapter.heard-saved-ms now)
    (ignore-errors
     (when (nlk:store-open-p)
       (nlk:session-state-put "channel-slack" +heard-key+ (princ-to-string now))))))

(defun last-heard-ms (adapter)
  "When Slack was last heard with the socket open -- this lane's word, else
the store's -- or NIL when it never was."
  (or adapter.heard-until-ms
      (and (nlk:store-open-p)
           (nlk:when-let (text (ignore-errors (nlk:session-state-get "channel-slack" +heard-key+)))
             (parse-integer text :junk-allowed t)))))

(defun catch-up-bound (adapter &aux (heard (last-heard-ms adapter)) (now (unix-ms)))
  "The moment a catch-up reads from -- never further back than
catch_up_minutes -- or NIL when the bot was not away long enough to miss
anything."
  ;; Two seconds before the mark: a message minted as the link died is the
  ;; one most likely missed, and one already taken is skipped anyway.
  (and heard (plusp adapter.catch-up-minutes)
       (> (- now heard) +catch-up-gap-ms+)
       (max (- heard 2000) (- now (* 60000 adapter.catch-up-minutes)))))

(defun catch-up-conversations (adapter)
  "The conversations a catch-up reads: the allowed channels the section
lists, else every one the bot is in."
  (or adapter.channels
      (let ((result (execute-plan (host-executor adapter.host)
                                  (rest-plan "GET" "/users.conversations?types=public_channel,private_channel,mpim,im&exclude_archived=true&limit=200"
                                             "catch_up_conversations" t 30))))
        (if (execution-ok-p result)
            (loop for conversation across (nlk:json-array (execution-body result) "channels")
                  for id = (nlk:json-value conversation :string "id")
                  when id collect id)
            (progn (warn "slack: no conversations to catch up on: ~a" (execution-error result))
                   '())))))

(defun catch-up-message (adapter channel message)
  "Hand the kit MESSAGE, one said in CHANNEL while the bot was away, unless
it is a bot's, is no message a person said, or already opened a lane. => T
when it was handed on."
  (setf (gethash "channel" message) channel)
  (unless (gethash "type" message)
    (setf (gethash "type" message) "message"))
  (unless (or (equal (nlk:json-value message :string "user") adapter.bot-user-id)
              (nlk:json-value message :string "bot_id"))
    (let ((threads adapter.threads))
      (note-thread-message threads channel (nlk:json-value message :string "ts")
                           (nlk:json-value message :string "thread_ts"))
      (nlk:when-let (candidate (slack-message-candidate
                                message :team-id adapter.team-id
                                :bot-user-id adapter.bot-user-id
                                :bot-thread-p (lambda (channel root)
                                                (bot-thread-p threads channel root))))
        (unless (candidate-taken-p adapter.host candidate)
          (setf (gethash "late_minutes" (gethash "source" candidate))
                (max 1 (round (- (unix-ms) (or (ts-ms (nlk:json-value message :string "ts")) (unix-ms)))
                              60000)))
          (name-speaker adapter candidate)
          (handle-candidate adapter.host (attach-slack-file-fetchers adapter candidate))
          t)))))

(defun start-catch-up (adapter since-ms)
  "Read back, on a thread of its own, what the bot's conversations said since
SINCE-MS -- each one's newest messages, and the replies of each thread among
them that moved since -- and hand the kit each message it missed, oldest
first."
  ;; A thread is read when its first message is among a conversation's newest
  ;; (+CATCH-UP-MESSAGES+), however old: a reply in the bot's own thread is
  ;; the follow-up most likely missed.
  (nlk:spawn "channel-slack-catch-up"
    (nlk:with-handlers ((error (condition)
                          (warn "slack: catching up stopped: ~a" condition)))
      (let ((handed 0)
            (oldest (ms-ts since-ms))
            (executor (host-executor adapter.host)))
        (flet ((read-back (path)
                 (let ((result (execute-plan executor (rest-plan "GET" path "catch_up" t 30))))
                   (if (execution-ok-p result)
                       (coerce (nlk:json-array (execution-body result) "messages") 'list)
                       (progn (warn "slack: could not read back ~a: ~a" path (execution-error result))
                              '()))))
               (since-p (ts) (let ((ms (ts-ms ts))) (and ms (> ms since-ms))))
               (hand (channel message)
                 (when (and (< handed +catch-up-replays+) (catch-up-message adapter channel message))
                   (incf handed))))
          (dolist (channel (catch-up-conversations adapter))
            ;; newest first, as Slack answers: handed on oldest first
            (dolist (message (reverse (read-back (format nil "/conversations.history?channel=~a&limit=~d"
                                                         channel +catch-up-messages+))))
              (let ((ts (nlk:json-value message :string "ts")))
                (when (since-p ts)
                  (hand channel message))
                (when (since-p (nlk:json-value message :string "latest_reply"))
                  (dolist (reply (read-back (format nil "/conversations.replies?channel=~a&ts=~a&oldest=~a&limit=~d"
                                                    channel ts oldest +catch-up-messages+)))
                    (let ((reply-ts (nlk:json-value reply :string "ts")))
                      (when (and (since-p reply-ts) (not (equal reply-ts ts)))
                        (hand channel reply)))))))))
        (when (plusp handed)
          (format *error-output* "~&;; slack: ~d message~:p said while the bot was away handed on~%"
                  handed))))))

(defun slack-handle-envelope (adapter type payload)
  "One acknowledged Socket Mode envelope: an Events API callback goes to
SLACK-HANDLE-EVENT, a slash command runs as the command line it spells, a
button press is relayed to the kit, which acts on it."
  (let ((host adapter.host))
    (cond
      ((equal type "events_api")
       (nlk:when-let (event (nlk:json-value payload :object "event"))
         (slack-handle-event adapter event (nlk:json-value payload :string "team_id"))))
      ((equal type "slash_commands")
       (nlk:when-let (candidate (slack-command-candidate payload))
         (handle-candidate host candidate)
         candidate))
      ((equal type "interactive")
       (nlk:when-let (press (slack-control-press payload))
         ;; The thread board's own buttons are the board's (board.lisp).
         (when (slack-board-pressed adapter press)
           (return-from slack-handle-envelope press))
         (let* ((channel (getf press :channel-id))
                (pressed (getf press :message-id))
                (line (said-line (getf press :data)))
                (candidate (and line
                                (slack-press-candidate
                                 payload line
                                 :card-text (pressed-words adapter.threads channel pressed)))))
           ;; the message pressed lives in its thread: what answers the press
           ;; goes there
           (note-thread-message adapter.threads channel pressed (getf press :thread-id))
           (cond (candidate
                  ;; a press on a choice is the presser saying its line,
                  ;; admitted and answered as typed words are
                  (setf (gethash "user_name" (gethash "source" candidate))
                        (or (slack-user-name adapter (getf press :user-id))
                            (getf press :user-name)))
                  (handle-candidate host candidate)
                  candidate)
                 (t (control-pressed host press)
                    press))))))))

;;; --- the socket lap ----------------------------------------------------------------

(defun open-socket-url (adapter)
  "(values URL FATAL-P DETAIL): a one-use wss URL from apps.connections.open,
or why not -- FATAL-P when the token itself is refused, since no wait mends
that."
  (let* ((result (execute-plan adapter.app-executor
                               (api-plan "apps.connections.open" nil "connections_open"
                                         :timeout-seconds 15)))
         (url (and result.ok-p (nlk:json-value result.body :string "url"))))
    (cond (url (values (if adapter.debug-reconnects-p
                           (format nil "~a&debug_reconnects=true" url)
                           url)
                       nil nil))
          (t (values nil
                     (and (some (lambda (code) (search code (or result.error "")))
                                '("invalid_auth" "not_authed" "not_allowed_token_type"
                                  "token_revoked" "account_inactive"))
                          t)
                     (probe-failure result))))))

(defun slack-socket-lap (adapter stop-p)
  "One supervised Socket Mode connection: open, read until Slack moves the
socket or the link dies, and come back for another."
  ;; Returns :stop / :fatal / seconds-to-wait per the kit supervisor contract.
  (when (funcall stop-p) (return-from slack-socket-lap :stop))
  (multiple-value-bind (url fatal-p detail) (open-socket-url adapter)
    (unless url
      (set-channel-status "slack" :state (if fatal-p :degraded :running)
                                  :connected nil :detail detail)
      (warn "slack: no socket: ~a" detail)
      (return-from slack-socket-lap (if fatal-p :fatal 5.0)))
    (let ((inbound (make-work-queue "channel-slack-inbound" :cap 1024))
          (ws (wsd:make-client url))
          (pings 0)
          (pinged-ms (now-ms))
          (connected nil))
      (wsd:on :message ws
              (lambda (message &aux (frame (ignore-errors (nlk:decode-json message))))
                (setf adapter.heard-ms (now-ms))
                (when (hash-table-p frame)
                  (queue-push inbound (list :frame frame)))))
      (wsd:on :close ws
              (lambda (&key code reason)
                (declare (ignore reason))
                (queue-push inbound (list :closed (or code 1000)))))
      (unwind-protect
           (block lap
             (wsd:start-connection ws)
             (setf adapter.heard-ms (now-ms))
             (loop
               (when (funcall stop-p) (return-from lap :stop))
               (when connected (note-heard adapter))
               ;; A socket that answers neither frames nor pings is gone, even
               ;; when no close ever arrives to say so.
               (when (> (- (now-ms) adapter.heard-ms) +dead-link-ms+)
                 (set-channel-status "slack" :connected nil :detail "the socket went quiet")
                 (return-from lap 1.0))
               (when (>= (- (now-ms) pinged-ms) +ping-interval-ms+)
                 (setf pinged-ms (now-ms))
                 (ignore-errors
                  (wsd:send-ping ws (sb-ext:string-to-octets (format nil "~d" (incf pings)))
                                 (lambda () (setf adapter.heard-ms (now-ms))))))
               (multiple-value-bind (item found) (queue-pop inbound 0.5)
                 (when found
                   (ecase (first item)
                     (:closed
                      (set-channel-status "slack" :connected nil)
                      (return-from lap 1.0))
                     (:frame
                      (dolist (action (read-socket-frame (second item)))
                        (ecase (first action)
                          (:hello
                           (set-channel-status "slack" :state :running :connected t :detail nil)
                           ;; what was said while no socket was open, read back
                           (let ((since (catch-up-bound adapter)))
                             (setf connected t)
                             (note-heard adapter :force t)
                             (when since (start-catch-up adapter since))))
                          (:ack
                           (wsd:send ws (nlk:encode-json-object (ack-frame (second action)))))
                          (:envelope
                           ;; One boundary per envelope, outside the seam, so
                           ;; advice that signals costs that envelope and not
                           ;; the socket.
                           (handler-case (slack-handle-envelope adapter (second action)
                                                                (third action))
                             (error (condition)
                               (warn "slack envelope handling signalled: ~a" condition))))
                          (:reconnect (return-from lap 0.1))
                          (:fatal
                           (set-channel-status "slack" :state :degraded :connected nil
                                                       :detail (second action))
                           (return-from lap :fatal))))))))))
        ;; Runs on the lap thread while wsd's reader may be parked in SSL_read
        ;; on this wss: sever the transport, never WSD:CLOSE-CONNECTION
        ;; (cross-thread SSL_free; see NLK:SEVER-WEBSOCKET).
        (when connected (ignore-errors (note-heard adapter :force t)))
        (ignore-errors (nlk:sever-websocket ws))))))

;;; --- the door ----------------------------------------------------------------------

(defun request (method path &rest arguments &key body headers timeout retry
                &aux (host (and *slack-adapter* (slack-adapter-host *slack-adapter*))))
  "One Slack Web API call as the bot: METHOD PATH, answered as
(:status N :body VALUE [:error TEXT]) -- the shape the eval cell prints
legibly, see NCK:CALL."
  ;; PATH is the method, "/chat.postMessage", with a read's arguments in its
  ;; query string; it joins onto the configured api_base. BODY is a keyword
  ;; plist (a JSON object) or an NLK:JSON-OBJECT. Any method the bot's scopes
  ;; allow; nothing here narrows it. The token stays on the executor and never
  ;; appears in the answer. Slack answers every write with the object it made
  ;; or an ok, so nothing is read back. Blocks on the caller's thread.
  (declare (ignore body headers timeout retry))
  (unless (and host host.executor)
    (error "the slack channel is not running"))
  (apply #'call host.executor method path arguments))

;;; --- the channel entry ---------------------------------------------------------------

(defun start-channel (section &key executor)
  "Start the Slack lane from its channels.slack config SECTION."
  ;; Returns a stop thunk. EXECUTOR overrides both live executors -- the
  ;; scripted test seam; it is wrapped in Slack's envelope like the live ones.
  ;;
  ;; Fail-closed: refuses unless at least one of allowed_channels /
  ;; allowed_users is populated. allowed_users says who may talk; owner says
  ;; whose instructions are standing policy (the kit's room, AUTHORITY). An
  ;; owner may always talk: the ids join allowed_users when that list is the
  ;; gate.
  (let* ((settings (nlk:section-settings (nlk:find-section '("channels" "slack")) section))
         (bot-token (resolve-channel-secret section "bot_token"))
         (app-token (resolve-channel-secret section "app_token"))
         (configured-users (getf settings :allowed-users))
         (owners (resolve-owners "slack" (getf settings :owner)
                                 configured-users "allowed_channels"))
         (allowed-users (and configured-users (union configured-users owners :test #'string=)))
         (api-base (config-string section "api_base" +slack-api-base+))
         (threads (make-thread-book))
         (bot (if executor
                  (wrap-slack-executor executor :book threads)
                  (make-slack-executor :api-base api-base :token bot-token :book threads)))
         (app (if executor bot (make-slack-executor :api-base api-base :token app-token)))
         (bot-user-id nil)
         (team-id nil))
    (require-non-empty-allowlist "slack"
                                 "allowed_channels" (getf settings :allowed-channels)
                                 "allowed_users" allowed-users)
    ;; The bot's own user id is the mention, the self filter and the thread
    ;; the bot starts. A failed auth.test is loud: every mention-gated channel
    ;; would reject its asks until the lane restarts.
    (let ((me (execute-plan bot (api-plan "auth.test" nil "auth_test" :retry t
                                                                      :timeout-seconds 15))))
      (if (execution-ok-p me)
          (setf bot-user-id (nlk:json-value (execution-body me) :string "user_id")
                team-id (nlk:json-value (execution-body me) :string "team_id"))
          (warn "slack: auth.test failed (~a); mention-gated channels reject every ask ~
                 until the lane restarts" (execution-error me))))
    (let* ((host (make-host-from-section
                  section (slack-platform :bot-user-id bot-user-id :threads threads)
                  ;; Every write the host makes is told to the thread board:
                  ;; the threads it answers in and what it says there.
                  :executor (board-noting-executor bot)
                  :owners owners
                  :soul-path (soul-path section)
                  :request-timeout 30
                  ;; The declared members name the policy's own keys; the rest
                  ;; are read here.
                  :policy (apply #'slack-inbound-policy
                                 :allowed-users allowed-users
                                 :bot-user-id bot-user-id
                                 settings)))
           (adapter (%make-slack-adapter
                     :host host
                     :app-executor app
                     :bot-token bot-token
                     :bot-user-id bot-user-id
                     :threads threads
                     :team-id team-id
                     :channels (getf settings :allowed-channels)
                     :catch-up-minutes (config-integer section "catch_up_minutes" 60 :min 0)
                     :debug-reconnects-p (config-boolean section "debug_reconnects" nil))))
      (let ((stop-board nil))
        (run-host host "socket" (lambda (stop-p) (slack-socket-lap adapter stop-p))
                  :on-start (lambda ()
                              (setf *slack-adapter* adapter
                                    stop-board (start-slack-board adapter section)))
                  :on-stop (lambda ()
                             (when stop-board
                               (ignore-errors (funcall stop-board))
                               (setf stop-board nil))
                             ;; A stale stop thunk racing a respawned lane must
                             ;; not clear the fresh adapter's binding.
                             (when (eq *slack-adapter* adapter)
                               (setf *slack-adapter* nil))))))))
