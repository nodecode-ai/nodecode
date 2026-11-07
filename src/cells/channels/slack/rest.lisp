;;;; rest.lisp --- Slack Web API request plans, the thread book, the executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Web API method is a POST (or, for a read, a GET) under
;;;; https://slack.com/api/ carrying the bot token as a bearer. Slack answers
;;;; nearly every refusal as HTTP 200 with {"ok": false, "error": "..."}, so
;;;; the executor reads the envelope: a 200 that says ok false is a failure
;;;; naming Slack's error. A 429 carries Retry-After, which the kit's retry
;;;; loop already reads.
;;;;
;;;; A message is posted as blocks: one markdown block, which renders the
;;;; standard Markdown a model writes (headings, lists, code, links), and --
;;;; on a running status line -- an actions block holding its buttons. `text'
;;;; rides beside the blocks as the notification's words. An edit sends the
;;;; blocks whole, so a button taken away is gone.

(in-package #:nodecode-channel-slack)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

(defparameter +slack-api-base+ "https://slack.com/api")

;;; The markdown block's own ceiling: 12,000 characters across a message's
;;; markdown blocks.
(defparameter +slack-text-limit+ 12000)

;;; --- the thread book -------------------------------------------------------------
;;; Which thread a message lives in, and which threads the bot speaks in. A
;;; reply goes in the thread its ask lives in -- Slack refuses nothing but
;;; files a reply to a reply as a thread of its own -- and a thread the bot
;;; answers in is one a person may go on in without mentioning it. Learned
;;; from the events (the lap thread) and from the plans (the delivery pool),
;;; so the tables are synchronized; live-only, and bounded.

(defparameter +thread-book-cap+ 20000
  "Entries either table holds before it starts over.")

(defparameter +card-text-cap+ 1000
  "Pressable messages whose words the book keeps before it starts over.")

(defstruct (thread-book (:copier nil))
  ;; "<channel>:<ts>" -> the ts of the thread's first message.
  (roots (make-hash-table :test #'equal :synchronized t))
  ;; "<channel>:<root ts>" -> T once the bot has posted in that thread.
  (spoken (make-hash-table :test #'equal :synchronized t))
  ;; "<channel>:<ts>" -> the words of a message the bot posted with
  ;; something to press on it: a press on it answers under those words
  ;; (NCK::ANSWER-CARD), and Slack hands a press back the message as rich
  ;; text blocks, never the Markdown it was posted as.
  (texts (make-hash-table :test #'equal :synchronized t)))

(defun pressed-words (book channel ts)
  "The words of the message TS in CHANNEL the bot posted with controls, or
NIL when the book holds none."
  (and book channel ts (gethash (book-key channel ts) (thread-book-texts book))))

(nlk:access (book thread-book))

(defun book-key (channel ts)
  (format nil "~a:~a" channel ts))

(defun book-note (table key value)
  "Set KEY to VALUE in TABLE, starting it over once it holds the cap."
  (when (>= (hash-table-count table) +thread-book-cap+)
    (clrhash table))
  (setf (gethash key table) value))

(defun note-thread-message (book channel ts root)
  "Remember that message TS in CHANNEL lives in the thread ROOT starts."
  (when (and channel ts root (not (equal ts root)))
    (book-note book.roots (book-key channel ts) root)))

(defun thread-root (book channel ts)
  "The first message of the thread message TS in CHANNEL lives in, or NIL
when the book has not seen it in one."
  (gethash (book-key channel ts) book.roots))

(defun bot-thread-p (book channel root)
  "Whether the bot has posted in the thread ROOT starts in CHANNEL."
  (and (gethash (book-key channel root) book.spoken) t))

(defun dm-channel-p (channel)
  "Whether CHANNEL is a direct message: Slack's DM ids begin with D."
  (uiop:string-prefix-p "D" (or channel "")))

(defun reply-thread (book target reply-to &aux (channel (getf target :channel-id)))
  "The thread a reply to REPLY-TO goes in: the thread REPLY-TO already lives
in, else -- outside a DM -- the thread REPLY-TO starts; NIL posts in the
channel itself."
  ;; A DM reads as a chat, so an ask typed there is answered there; in a
  ;; channel the answer hangs off the ask, the way Slack's own apps answer.
  (and reply-to
       (or (gethash (book-key channel reply-to) book.roots)
           (and (not (dm-channel-p channel)) reply-to))))

;;; --- plans -----------------------------------------------------------------------

(defun api-plan (method body label &key retry (timeout-seconds 30))
  "The POST of BODY to the Web API METHOD: 5xx and 429 retried when RETRY,
LABEL naming it in audits and failure copy."
  (rest-plan "POST" (format nil "/~a" method) label retry timeout-seconds body))

(defun plain-text (text limit)
  "TEXT as a Block Kit plain_text object, cut to LIMIT characters: Slack
refuses a whole message (invalid_blocks) over one that runs longer."
  (nlk:json-object "type" "plain_text" "text" (nlk:clip text (1- limit) :ellipsis "…")))

(defun mrkdwn-escape (text)
  "TEXT with the three characters Slack's mrkdwn reads as markup escaped."
  (uiop:frob-substrings (or text "") '("&" "<" ">")
                        (lambda (match emit)
                          (funcall emit (cond ((string= match "&") "&amp;")
                                              ((string= match "<") "&lt;")
                                              (t "&gt;"))))))

(defun control-button (control index &aux (style (third control)))
  "One kit button -- (LABEL DATA STYLE DISABLED) -- as a Block Kit button
whose value carries DATA back on a press; INDEX tells it from the other
elements on its message."
  (nlk:json-object "type" "button"
                   "action_id" (format nil "nodecode-~d" index)
                   "text" (plain-text (first control) 75)
                   "value" (second control)
                   :when (member style '(:danger :primary))
                   "style" (string-downcase (symbol-name style))))

(defun control-menu (menu index)
  "One kit menu -- (:MENU PLACEHOLDER OPTIONS) -- as a Block Kit static
select: each option (LABEL DATA DESCRIPTION CURRENT) carries DATA back as
the picked value, CURRENT is the one shown picked. NIL for a menu with no
option Slack would take."
  ;; An option's value is at most 150 characters and a select 100 options; an
  ;; option whose data runs longer is left out rather than cut, since cut data
  ;; names something else. Its words and description are plain text of at
  ;; most 75, and an empty description is none.
  (let ((options (loop for (label data description current) in (third menu)
                       when (<= 1 (length data) 150)
                         collect (cons current
                                       (nlk:json-object
                                        "text" (plain-text (if (plusp (length label)) label data) 75)
                                        "value" data
                                        :when (plusp (length description))
                                        "description" (plain-text description 75))))))
    (when options
      (let ((options (subseq options 0 (min 100 (length options)))))
        (nlk:json-object "type" "static_select"
                         "action_id" (format nil "nodecode-~d" index)
                         "placeholder" (plain-text (or (second menu) "Pick one") 150)
                         "options" (coerce (mapcar #'cdr options) 'vector)
                         :opt "initial_option" (cdr (find-if #'car options)))))))

(defun actions-block (elements)
  "ELEMENTS side by side in one actions block, NIL for none: Slack takes 25."
  (and elements
       (nlk:json-object "type" "actions"
                        "elements" (coerce (subseq elements 0 (min 25 (length elements))) 'vector))))

(defun control-line (line index)
  "One kit line -- (:ROW MARK LABEL DETAIL BUTTON), NCK::LINE-CHOICE -- as a
section: its words, BUTTON beside them. A line with no words is its button
alone, and one with neither is NIL."
  (destructuring-bind (mark label detail button) (rest line)
    (let* ((label (and (plusp (length label)) label))
           (detail (and (plusp (length detail)) detail))
           (words (format nil "~@[~a ~]~@[*~a*~]~:[~; · ~]~@[~a~]"
                          mark (and label (mrkdwn-escape label)) (and label detail)
                          (and detail (mrkdwn-escape detail))))
           (button (and button (not (fourth button)) (control-button button index))))
      (cond ((plusp (length (nlk:trimmed words)))
             (nlk:json-object "type" "section"
                              "text" (nlk:json-object "type" "mrkdwn"
                                                      "text" (nlk:clip words 2999 :ellipsis "…"))
                              :opt "accessory" button))
            (button (actions-block (list button)))))))

(defun control-blocks (controls)
  "CONTROLS -- the kit's buttons, rows of buttons, menus and lines -- as Block
Kit blocks in order: a lone button or a row of them an actions block, a menu
its static select in one, a line its section with its button beside it;
:CLEAR or NIL none."
  ;; Block Kit draws no disabled button: one is left out, and a row of only
  ;; those is no block. Slack takes 50 blocks a message, one of them the
  ;; words; the kit's cards stay inside, and anything past it is dropped here,
  ;; not refused there.
  (loop with index = 0
        for control in (unless (eq controls :clear) controls)
        for block = (cond ((eq (first control) :menu)
                           (nlk:when-let (menu (control-menu control (incf index)))
                             (actions-block (list menu))))
                          ((eq (first control) :row)
                           (control-line control (incf index)))
                          (t (actions-block
                              (loop for button in (if (consp (first control)) control (list control))
                                    unless (fourth button)
                                      collect (control-button button (incf index))))))
        when block collect block into blocks
        finally (return (subseq blocks 0 (min 48 (length blocks))))))

;;; A line under a message's words drawn small and grey -- the mark a card
;;; pressed keeps, "Answered: ..." (NCK::ANSWER-CARD) -- rides the text after
;;; this private-use character, which MESSAGE-BLOCKS draws as a context block.
(defparameter +footer-mark+ (string (code-char #xE000)))

(defun slack-answer-body (text footer)
  "The platform's ANSWER-BODY: TEXT, FOOTER under it as small grey words."
  (format nil "~a~a~a" text +footer-mark+ footer))

(defun split-footer (text)
  "(values WORDS FOOTER): TEXT before and after its footer mark, FOOTER NIL
when it carries none."
  (let ((at (search +footer-mark+ (or text ""))))
    (if at
        (values (subseq text 0 at) (subseq text (+ at (length +footer-mark+))))
        (values text nil))))

(defun message-blocks (text controls)
  "TEXT as a markdown block -- its footer, when it carries one, a context
block under it -- followed by CONTROLS, the kit's buttons, menus and lines
(CONTROL-BLOCKS), or :CLEAR for none."
  (multiple-value-bind (words footer) (split-footer text)
    (coerce (append (and (plusp (length (nlk:trimmed words)))
                         (list (nlk:json-object "type" "markdown" "text" words)))
                    (and footer
                         (list (nlk:json-object
                                "type" "context"
                                "elements" (vector (nlk:json-object
                                                    "type" "mrkdwn"
                                                    "text" (nlk:clip (mrkdwn-escape footer) 2999
                                                                     :ellipsis "…"))))))
                    (control-blocks controls))
            'vector)))

(defun message-body (target text &key thread controls)
  "The members every post and edit carry: the channel, the blocks, and the
notification's words."
  (nlk:json-object "channel" (getf target :channel-id)
                   :opt "thread_ts" thread
                   "text" (notification-text text)
                   "blocks" (message-blocks text controls)))

(defun notification-text (text)
  "TEXT as the notification's words: one line, its footer a sentence after it."
  (multiple-value-bind (words footer) (split-footer text)
    (nlk:clip (nlk:one-line (format nil "~a~@[ ~a~]" words footer)) 1000)))

(defun message-plan (book target chunk &key reply-to ping controls files mentions card panel
                                            (timeout-seconds 30))
  "One chat.postMessage CHUNK (a kit text-chunk), in the thread REPLY-TO
lives in or starts (REPLY-THREAD); every chunk of one post goes in the same
thread."
  ;; The kit decides what each chunk carries: CONTROLS ride the last, under
  ;; the words they answer, and MENTIONS are spelled into the first chunk's
  ;; text already (the platform's MENTION), so they are passed through as they
  ;; come. Slack has no silent message, so PING changes nothing: a thread
  ;; notifies whoever follows it, the way it does for any reply. FILES are
  ;; posted as their own messages (FILE-PLAN). A turn's CARD goes as its words
  ;; (the chunk) and its pictures (NOTE-PICTURES), labelled send_card -- the
  ;; turn's chrome, which the thread board tells from words said -- and a
  ;; command's PANEL as its words.
  ;; Posting in a thread notes that the bot speaks there. Retried: a post
  ;; that fails is an answer lost.
  (declare (ignore ping mentions panel))
  (let ((thread (reply-thread book target reply-to))
        (channel (getf target :channel-id)))
    (when thread
      (book-note book.spoken (book-key channel thread) t))
    (let ((body (note-pictures (message-body target chunk.text :thread thread :controls controls) card))
          (label (cond (card "send_card")
                       ((> chunk.total 1)
                        (format nil "send_message_chunk_~a_of_~a" chunk.index chunk.total))
                       (t "send_message"))))
      (if files
          ;; the answer's pictures go under its words, in its thread
          (upload-plan target files :thread thread :message body :label label
                                    :timeout-seconds (max 60 timeout-seconds))
          (api-plan "chat.postMessage" body label :retry t :timeout-seconds timeout-seconds)))))

;;; --- files -----------------------------------------------------------------------
;;; Slack takes a file in three calls -- a link minted for its bytes
;;; (files.getUploadURLExternal), the bytes sent to that link, and one share of
;;; every file so sent (files.completeUploadExternal) -- so a file's plan is one
;;; UPLOAD plan, which the Slack executor runs as those steps (RUN-UPLOAD). The
;;; app needs files:write.

(defparameter +upload-method+ "UPLOAD"
  "The method of a plan the Slack executor runs as Slack's upload steps.")

(defun upload-plan (target pathnames &key thread comment message (label "send_file")
                                          (timeout-seconds 60))
  "The plan that shares PATHNAMES, files, where TARGET is -- in THREAD when
given -- COMMENT the words above them; MESSAGE, a chat.postMessage body, is
posted first and the files go under it, in its thread. Retried step by step."
  (make-request-plan :method +upload-method+ :path "/files.completeUploadExternal"
                     :body (nlk:json-object "channel_id" (getf target :channel-id)
                                            :opt "thread_ts" thread
                                            :opt "initial_comment" comment
                                            "files" (coerce (mapcar #'namestring pathnames) 'vector)
                                            :opt "message" message)
                     :timeout-seconds timeout-seconds
                     :retry-server-errors t
                     :audit-label label))

(defun file-plan (book target pathname &key content reply-to voice (timeout-seconds 60))
  "The platform's PLAN-FILE: one file shared where TARGET is, in the thread
REPLY-TO lives in or starts (REPLY-THREAD), CONTENT the words above it. A
VOICE reply's Ogg Opus is shared as the audio file it is: Slack draws it a
player, and has no voice message a bot can send."
  (let ((thread (reply-thread book target reply-to)))
    (when thread
      (book-note book.spoken (book-key (getf target :channel-id) thread) t))
    (upload-plan target (list (pathname pathname))
                 :thread thread
                 :comment (and (plusp (length content)) content)
                 :label (if voice "send_voice" "send_file")
                 :timeout-seconds (max 60 (or timeout-seconds 0)))))

(defun file-octets (path)
  "The size of the file at PATH, in octets."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun run-upload (executor plan &aux (body plan.body) (timeout plan.timeout-seconds))
  "Run an UPLOAD-PLAN through EXECUTOR, the Slack executor: the message it
carries posted first; then, file by file, a link minted and the bytes sent
to it; then one share of them all, under the message, in its thread. => the
message's execution when the plan carries one, else the share's; the first
failure when a step fails."
  (let* ((message (gethash "message" body))
         (posted (and message
                      (execute-plan executor (api-plan "chat.postMessage" message plan.audit-label
                                                       :retry t :timeout-seconds timeout))))
         (files '()))
    (when (and posted (not (execution-ok-p posted)))
      (return-from run-upload posted))
    (loop for path across (gethash "files" body)
          for file = (pathname path)
          for minted = (execute-plan executor
                                     (api-plan "files.getUploadURLExternal"
                                               (nlk:json-object "filename" (file-namestring file)
                                                                "length" (file-octets file))
                                               "upload_url" :retry t :timeout-seconds timeout))
          do (unless (execution-ok-p minted)
               (return-from run-upload minted))
             (let ((sent (execute-plan executor
                                       (rest-plan "POST" (nlk:json-value (execution-body minted)
                                                                         :string "upload_url")
                                                  "upload_bytes" t timeout (list (cons "file" file))))))
               (unless (execution-ok-p sent)
                 (return-from run-upload sent))
               (push (nlk:json-object "id" (nlk:json-value (execution-body minted) :string "file_id")
                                      "title" (file-namestring file))
                     files)))
    (let ((shared (execute-plan executor
                                (api-plan "files.completeUploadExternal"
                                          (nlk:json-object
                                           "files" (coerce (nreverse files) 'vector)
                                           "channel_id" (gethash "channel_id" body)
                                           :opt "thread_ts" (or (and message (gethash "thread_ts" message))
                                                                (gethash "thread_ts" body))
                                           :opt "initial_comment" (gethash "initial_comment" body))
                                          "share_files" :retry t :timeout-seconds timeout))))
      (if (and posted (execution-ok-p shared)) posted shared))))

;;; --- a card's pictures ------------------------------------------------------------
;;; A turn's card shows the newest pictures the turn looked at (its :IMAGES)
;;; as image blocks under its words. Slack draws a block's picture from a
;;; file it holds, so each is uploaded once -- shared nowhere, the bot's own --
;;; and named by its id on every post and edit after: a card edits every few
;;; seconds, and a picture sent again would be a new file each time. The
;;; pictures ride the plan's body under +PICTURES-KEY+, which the Slack
;;; executor turns into blocks (RUN-WITH-PICTURES) and never sends. Needs
;;; files:write; a picture that will not upload is left out.

(defparameter +pictures-key+ "nodecode_pictures"
  "The body member a card's post or edit carries its pictures in, (NAME PATH
DESCRIPTION) each.")

(defparameter +card-pictures-cap+ 500
  "Pictures the upload memory holds before it starts over.")

(defparameter +picture-strikes+ 3
  "Times Slack may refuse to draw a picture before it is left out.")

(defvar *card-pictures* (make-hash-table :test #'equal :synchronized t)
  "A card picture's path -> (:ID FILE-ID :STRIKES N), the file Slack holds it
as and the times Slack refused to draw it, or :REFUSED for one left out.")

(defun note-pictures (body card)
  "BODY carrying CARD's pictures for the Slack executor to draw, when it has
any. => BODY."
  (nlk:when-let (images (and (listp card) (getf card :images)))
    (setf (gethash +pictures-key+ body) images))
  body)

(defun upload-private (executor file name timeout)
  "Upload FILE as NAME through EXECUTOR, shared nowhere: the bot's own, for a
block to draw. => its id, or NIL when a step failed."
  (let ((minted (execute-plan executor (api-plan "files.getUploadURLExternal"
                                                 (nlk:json-object "filename" name "length" (file-octets file))
                                                 "picture_url" :timeout-seconds timeout))))
    (when (execution-ok-p minted)
      (let ((id (nlk:json-value (execution-body minted) :string "file_id"))
            (url (nlk:json-value (execution-body minted) :string "upload_url")))
        (and id url
             (execution-ok-p (execute-plan executor (rest-plan "POST" url "picture_bytes" t timeout
                                                               (list (cons "file" file)))))
             (execution-ok-p (execute-plan executor
                                           (api-plan "files.completeUploadExternal"
                                                     (nlk:json-object "files" (vector (nlk:json-object "id" id "title" name)))
                                                     "picture_share" :timeout-seconds timeout)))
             id)))))

(defun upload-picture (executor path name timeout)
  "The id of the file Slack holds the picture at PATH as -- uploaded now, as
NAME, when it holds none -- or NIL for a picture left out: one whose file is
gone, that would not upload, or that Slack would not draw."
  (let ((known (gethash path *card-pictures*)))
    (cond ((eq known :refused) nil)
          (known (getf known :id))
          (t (let* ((file (and path (probe-file path)))
                    (id (and file (upload-private executor file name timeout))))
               (when (>= (hash-table-count *card-pictures*) +card-pictures-cap+)
                 (clrhash *card-pictures*))
               (setf (gethash path *card-pictures*) (if id (list :id id :strikes 0) :refused))
               id)))))

(defun strike-picture (path &aux (known (gethash path *card-pictures*)))
  "Slack refused once more to draw the picture at PATH: left out from the
+PICTURE-STRIKES+th time."
  (when (consp known)
    (setf (gethash path *card-pictures*)
          (if (>= (1+ (getf known :strikes)) +picture-strikes+)
              :refused
              (list :id (getf known :id) :strikes (1+ (getf known :strikes)))))))

(defun plan-with-pictures (plan shown &aux (body (make-hash-table :test #'equal)))
  "PLAN's request with SHOWN -- (PATH ID ALT) each -- as image blocks under
its words and above its buttons, its pictures member gone."
  (maphash (lambda (key value)
             (unless (equal key +pictures-key+)
               (setf (gethash key body) value)))
           plan.body)
  (let* ((blocks (coerce (or (gethash "blocks" body) #()) 'list))
         (at (or (position-if-not (lambda (block)
                                    (member (gethash "type" block) '("markdown" "context") :test #'equal))
                                  blocks)
                 (length blocks)))
         (images (loop for (nil id alt) in shown
                       collect (nlk:json-object "type" "image" "slack_file" (nlk:json-object "id" id)
                                                "alt_text" (nlk:clip alt 1999 :ellipsis "…")))))
    (setf (gethash "blocks" body) (coerce (append (subseq blocks 0 at) images (nthcdr at blocks)) 'vector)))
  (make-request-plan :method plan.method :path plan.path :body body :headers plan.headers
                     :timeout-seconds plan.timeout-seconds
                     :retry-server-errors plan.retry-server-errors
                     :audit-label plan.audit-label))

(defun run-with-pictures (executor plan &aux (timeout plan.timeout-seconds))
  "Run PLAN, a card's post or edit carrying its pictures, through EXECUTOR,
the Slack executor: each picture uploaded once (UPLOAD-PICTURE) and drawn
under the card's words. Refused for its pictures (invalid_blocks: a file
not ready yet, or not one Slack draws), the card goes again without them,
and a picture refused +PICTURE-STRIKES+ times is left out from then on."
  (let* ((shown (loop for (name path description) in (gethash +pictures-key+ plan.body)
                      for id = (upload-picture executor path name timeout)
                      when id collect (list path id (or description name))))
         (result (execute-plan executor (plan-with-pictures plan shown))))
    (if (and shown (not (execution-ok-p result))
             (search "invalid_blocks" (or (execution-error result) "")))
        (progn (dolist (picture shown) (strike-picture (first picture)))
               (execute-plan executor (plan-with-pictures plan '())))
        result)))

(defun edit-plan (target message-id text &key retry controls card media (timeout-seconds 30))
  "chat.update: the status line settled in place, its buttons with it."
  ;; RETRY opts into 5xx/429 retries: a running update leaves it off (the next
  ;; tick supersedes a missed edit), the terminal settle turns it on. CONTROLS
  ;; NIL keeps the message buttonless: an edit sends the blocks whole. A
  ;; CARD goes as its words (TEXT) and its pictures, every one named again
  ;; (RUN-WITH-PICTURES): what the message held (MEDIA) is the executor's to
  ;; know.
  (declare (ignore media))
  (let ((body (note-pictures (message-body target text :controls controls) card)))
    (setf (gethash "ts" body) message-id)
    (api-plan "chat.update" body "edit_message" :retry (and retry t)
                                                :timeout-seconds timeout-seconds)))

(defun delete-plan (target message-id &key (timeout-seconds 30))
  "chat.delete -- retiring a status line once the answer it stood in for has
landed. Retried: a status line outliving its answer is a duplicate."
  (api-plan "chat.delete" (nlk:json-object "channel" (getf target :channel-id) "ts" message-id)
            "delete_message" :retry t :timeout-seconds timeout-seconds))

(defun ack-plan (press &key text (timeout-seconds 30))
  "A control PRESS's answer to the presser alone: TEXT, a card's Details or a
refusal, as an ephemeral message where the press was made
(chat.postEphemeral); nothing without TEXT, the socket's acknowledgement
having answered the press."
  ;; Not retried: a missed answer is pressed again.
  (when text
    (list (api-plan "chat.postEphemeral"
                    (nlk:json-object "channel" (getf press :channel-id)
                                     "user" (getf press :user-id)
                                     :opt "thread_ts" (getf press :thread-id)
                                     "text" (nlk:clip (nlk:one-line text) 1000)
                                     "blocks" (message-blocks text nil))
                    "control_ack" :timeout-seconds timeout-seconds))))

(defun typing-plan (book target &key (timeout-seconds 30))
  "assistant.threads.setStatus: `is working...' under the thread the answer
will land in, or NIL where the answer lands in the channel itself -- a DM --
since Slack shows the status only in a thread."
  ;; Slack clears it when the bot posts in the thread, and after two minutes;
  ;; the kit re-asserts it on the platform's cadence. Not retried: a missed beat
  ;; corrects itself on the next tick.
  (nlk:when-let (thread (reply-thread book target (getf target :message-id)))
    (api-plan "assistant.threads.setStatus"
              (nlk:json-object "channel_id" (getf target :channel-id)
                               "thread_ts" thread
                               "status" "is working...")
              "typing_indicator" :timeout-seconds timeout-seconds)))

(defparameter +reaction-names+ '(("👀" . "eyes") ("✅" . "white_check_mark") ("❌" . "x"))
  "Slack names a reaction by its emoji name: the kit's glyphs -- seen, done,
failed (NCK:+REACTION-SEEN+ and its two) -- spelled that way.")

(defun reaction-name (emoji)
  "EMOJI as Slack names it: the kit's glyph through the table, a name as is."
  (or (cdr (assoc emoji +reaction-names+ :test #'equal))
      (string-trim ":" emoji)))

(defun reaction-plans (target message-id emoji &key previous (timeout-seconds 30))
  "The bot's one reaction on MESSAGE-ID: PREVIOUS removed first -- Slack
stacks reactions -- then EMOJI added, or nothing added for NIL."
  ;; Retried: the mark holds for the whole turn, so its clear must land even
  ;; through a 429. already_reacted and no_reaction read as success (the
  ;; executor), the state asked for being the state there.
  (flet ((plan (method glyph label)
           (api-plan method (nlk:json-object "channel" (getf target :channel-id)
                                             "timestamp" message-id
                                             "name" (reaction-name glyph))
                     label :retry t :timeout-seconds timeout-seconds)))
    (unless (equal previous emoji)
      (append (and previous (list (plan "reactions.remove" previous "remove_reaction")))
              (and emoji (list (plan "reactions.add" emoji "add_reaction")))))))

(defparameter +socket-hold+ (make-request-plan :path "/" :audit-label "respond_hold")
  "The hold on a slash command's answer, which the socket's acknowledgement
already is: Slack keeps a response_url open half an hour. Executing it is
done (WRAP-SLACK-EXECUTOR).")

(defun respond-plan (candidate text &key private controls panel (timeout-seconds 30)
                                    &aux (url (source-field candidate "response_url")))
  "The answer to a slash command or a press, through the response_url it
arrived with. A command's answer goes in the channel for everyone to read,
or, PRIVATE, to the person who asked alone (ephemeral); a press's answer
becomes the message pressed -- unless it is PRIVATE and that message is
the channel's, when it is a word to the presser alone under it. CONTROLS
ride under it. With TEXT NIL, the hold the kit asks for first, which the
socket's acknowledgement already is (+SOCKET-HOLD+). NIL for anything
else, which is answered as a reply."
  ;; A PANEL goes as its words: TEXT. replace_original edits the message the
  ;; press came from, an ephemeral one too -- which chat.update cannot reach
  ;; -- and an ephemeral one stays seen by its one reader.
  (declare (ignore panel))
  (let ((pressed (candidate-pressed-p candidate))
        (seen-by-one (eq t (gethash "card_ephemeral" (gethash "source" candidate)))))
    (cond ((null url) nil)
          ((null text) +socket-hold+)
          (t (rest-plan "POST" url "respond" t timeout-seconds
                        (nlk:json-object "response_type" (if private "ephemeral" "in_channel")
                                         :when (and pressed (or seen-by-one (not private)))
                                         "replace_original" t
                                         "text" (notification-text text)
                                         "blocks" (message-blocks text controls)))))))

(defun slack-message-id (body)
  "The ts of the message a chat.postMessage execution created."
  (nlk:json-value body :string "ts"))

(defun slack-address (target message-id)
  "The address-book key for one Slack message: its ts is unique within its
channel, so the channel rides in the key."
  (book-key (getf target :channel-id) message-id))

;;; --- the executor ----------------------------------------------------------------

(defparameter +settled-errors+ '("already_reacted" "no_reaction" "message_not_found")
  "Slack errors that mean the state asked for is the state there: a reaction
already on or already off, a status line already gone.")

(defun slack-failure-message (plan status detail)
  "One failed call in words: Slack's error code when the envelope names one."
  (format nil "slack ~a failed with status ~a: ~a" plan.audit-label status detail))

(defun note-posted-text (book plan result)
  "Keep in BOOK the words of the message a successful post or edit PLAN made
(RESULT its execution) when it carries something to press: a press on it
answers under them (PRESSED-WORDS)."
  (let ((body plan.body))
    (when (and book result.ok-p (hash-table-p body)
               (member plan.path '("/chat.postMessage" "/chat.update") :test #'equal))
      (let* ((blocks (coerce (or (gethash "blocks" body) #()) 'list))
             (words (loop for block in blocks
                          when (equal "markdown" (gethash "type" block))
                            return (gethash "text" block)))
             (ts (or (and (hash-table-p result.body) (nlk:json-value result.body :string "ts"))
                     (gethash "ts" body)))
             (key (book-key (gethash "channel" body) ts))
             (texts (thread-book-texts book)))
        (if (and words ts (some (lambda (block)
                                  (member (gethash "type" block) '("actions" "section") :test #'equal))
                                blocks))
            (progn (when (>= (hash-table-count texts) +card-text-cap+) (clrhash texts))
                   (setf (gethash key texts) words))
            (remhash key texts))))))

(defun wrap-slack-executor (inner &key book)
  "An executor speaking Slack's envelope in the kit's terms: a 200 that says
ok false fails with Slack's error, the errors that mean `already so' succeed,
a plan that is NIL -- a typing beat Slack cannot show -- or the socket's
hold on a command's answer (+SOCKET-HOLD+) is done, an UPLOAD plan runs
as Slack's upload steps (RUN-UPLOAD), and a card carrying pictures uploads
them once and draws them (RUN-WITH-PICTURES). BOOK, the thread book, keeps the words
of each pressable message posted (NOTE-POSTED-TEXT)."
  (let ((executor nil))
    (setf executor
          (make-plan-executor
           :run (lambda (plan)
                  (cond
                    ((or (null plan) (eq plan +socket-hold+))
                     (make-execution :ok-p t :status 200 :attempts 0))
                    ((equal plan.method +upload-method+) (run-upload executor plan))
                    ((and (hash-table-p plan.body) (gethash +pictures-key+ plan.body))
                     (run-with-pictures executor plan))
                    (t
                     (let* ((result (execute-plan inner plan))
                            (body result.body)
                            (code (and (hash-table-p body)
                                       (eq nil (gethash "ok" body t))
                                       (or (nlk:json-value body :string "error") "error"))))
                       (when (and result.ok-p code)
                         (if (member code +settled-errors+ :test #'string=)
                             (setf result.body nil)
                             (setf result.ok-p nil
                                   result.error (slack-failure-message plan result.status code))))
                       (note-posted-text book plan result)
                       result))))))
    executor))

(defun make-slack-executor (&key (api-base +slack-api-base+) token book)
  "The live executor: the Web API under API-BASE with TOKEN as the bearer,
which never reaches a failure text; BOOK as WRAP-SLACK-EXECUTOR's."
  (wrap-slack-executor
   (make-dexador-executor
    :base-url api-base
    :headers (list (cons "authorization" (format nil "Bearer ~a" token)))
    :redact-prefixes (list token)
    :failure-message-fn #'slack-failure-message)
   :book book))
