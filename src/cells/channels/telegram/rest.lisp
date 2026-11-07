;;;; rest.lisp --- Telegram Bot API request plans, classifier, executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported failure semantics from the Zig-era pack: 429 retry_after comes
;;;; from the response BODY (parameters.retry_after), not only the header;
;;;; a getUpdates 409 conflict means a duplicate poller holds the token; a
;;;; send against a dead topic must not silently retry without
;;;; message_thread_id (that could deliver to the wrong topic).
;;;;
;;;; The executor base URL embeds the bot token (<api_base>/bot<token>), so
;;;; both bot<token> and the bare token ride the redaction list and failure
;;;; copy names audit labels, never URLs.

(in-package #:nodecode-channel-telegram)

(nlk:access (chunk text-chunk) (plan request-plan) (result execution))

(defparameter +telegram-api-base+ "https://api.telegram.org")
(defparameter +telegram-message-text-limit+ 16384
  "Characters per message when an answer is split: half of a rich message's
32768, leaving room for the entities and line breaks RICH-MARKDOWN adds.")

(defparameter +telegram-caption-limit+ 1024
  "UTF-16 code units a media message's caption holds — the count the Bot API
keeps, an astral character costing two. A text message holds far more, so an
answer whose words outrun this keeps a clipped caption on its file and goes
on in text messages after it: the Bot API refuses the whole call — the
picture among it — when a caption does not fit.")

(defun clip-caption (text limit)
  "(values CAPTION REST): the longest head of TEXT that fits LIMIT UTF-16
code units, cut back to its last line break, else its last space, when one
falls in the half it keeps, and the words it left; both trimmed."
  (let ((units 0)
        (cut 0))
    (loop for index from 0 below (length text)
          for size = (if (> (char-code (aref text index)) #xffff) 2 1)
          while (<= (+ units size) limit)
          do (incf units size)
          do (setf cut (1+ index)))
    (if (>= cut (length text))
        (values text "")
        ;; A cut inside a word reads as a typo in a caption, so it backs up
        ;; to the last line break, else the last space, it can keep.
        (let* ((floor (truncate cut 2))
               (break (or (position #\Newline text :start floor :end cut :from-end t)
                          (position #\Space text :start floor :end cut :from-end t)))
               (head (subseq text 0 (or break cut))))
          (values (string-right-trim '(#\Space #\Tab) head)
                  (string-left-trim '(#\Space #\Tab #\Newline) (subseq text (or break cut))))))))

(defun target-body (target &aux (thread (getf target :thread-id)))
  (nlk:json-object "chat_id" (getf target :channel-id)
                   :when thread
                   "message_thread_id" (or (parse-integer thread :junk-allowed t) thread)))

(defun reply-parameters (reply-to)
  "REPLY-TO as reply_parameters: allow_sending_without_reply, so an ask
deleted meanwhile still gets its answer."
  (nlk:json-object "message_id" (or (parse-integer reply-to :junk-allowed t) reply-to)
                   "allow_sending_without_reply" t))

(defun message-body (target message-id)
  "The body naming MESSAGE-ID in TARGET's chat, and no topic: the id names the message."
  (nlk:json-object "chat_id" (getf target :channel-id)
                   "message_id" (or (parse-integer message-id :junk-allowed t) message-id)))

(defun upload-parts (target reply-to &optional caption)
  "The form-data parts an upload leads with, in order: TARGET's chat, the
topic it names, CAPTION when it says anything, and REPLY-TO as
reply_parameters."
  (append (list (cons "chat_id" (getf target :channel-id)))
          (and (getf target :thread-id)
               (list (cons "message_thread_id" (format nil "~a" (getf target :thread-id)))))
          (and (stringp caption) (plusp (length (string-trim " " caption)))
               (list (cons "caption" caption)))
          (and (stringp reply-to) (plusp (length reply-to))
               (list (cons "reply_parameters"
                           (nlk:encode-json-object (reply-parameters reply-to)))))))

;;; --- a turn's card (the kit's DIGEST-CARD, drawn as a rich message) --------
;;; The kit owns what a card says; how it looks is the platform's. Here: one
;;; rich message (Bot API 10.1), Telegram's own blocks laid out as Discord's
;;; container is: the phase a pill at its head, a disabled button blue while
;;; the turn works, green done, red failed, Telegram's grey otherwise; the
;;; ask's task as a heading; what the turn does now and its thought; its
;;; steps a checklist, ticked as each lands; a rule, and its numbers in the
;;; footer's small type. Every edit restates the whole card.

(defparameter +card-pill-styles+ '((:working . "primary") (:done . "success") (:failed . "danger"))
  "The style a card's pill wears for each state: blue working, green done,
red failed; any other state Telegram's own face.")

(defun telegram-escape (text)
  "TEXT with the characters Telegram's HTML reads as markup spelled as
entities, the double quote too, so it may stand in an attribute."
  (with-output-to-string (out)
    (loop for ch across text
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char ch out))))))

(defun callback-data-p (data)
  "Whether DATA fits a button's callback_data, 1-64 bytes: one past them
refuses the whole message, so a button whose data runs longer is left out
rather than cut into something else."
  (and (stringp data)
       (<= 1 (length (sb-ext:string-to-octets data :external-format :utf-8)) 64)))

(defun card-html (card &optional controls)
  "CARD (NCK:DIGEST-CARD) as the HTML of one Telegram rich message: the pill
saying its state and time; the ask's task as a heading; what the turn is
doing when no step shows it, beside its thought in italics; its note; the
words it says on its way; its steps a checklist, one a stop or a failure cut
short struck through; the input parked behind it; under a rule the footer:
the steps before those shown, then its numbers; and CONTROLS, the card's
buttons (LINE-CONTROLS: a row, or a lone button), in a row of their own
inside the message, Stop red. Its pictures are left out."
  ;; A rich message holds 32768 characters; the kit caps every part (a step at
  ;; 80, the note at 200, the thought past 100), and the task and the words
  ;; said are cut here.
  (destructuring-bind (&key state elapsed task headline thought steps earlier note meta pending images said)
      card
    (declare (ignore images))
    (flet ((esc (text) (ppcre:regex-replace-all "\\n" (telegram-escape text) "<br>")))
      (let* ((working (eq state :working))
             ;; What the turn does now is the running step's row, or, between
             ;; steps, the line under the heading.
             (now (and working (not (find :running steps :key #'first)) headline))
             (footer (remove nil (list (and earlier (plusp earlier) (format nil "+~d earlier" earlier))
                                       (and meta (esc meta)))))
             (buttons (loop for (label data style disabled)
                              in (loop for control in (unless (eq controls :clear) controls)
                                       append (if (consp (first control)) control (list control)))
                            when (and (not disabled) (callback-data-p data))
                              collect (format nil "<tg-button type=\"callback_data\"~@[ style=\"~(~a~)\"~] data=\"~a\">~a</tg-button>"
                                              (find style '(:danger :primary)) (telegram-escape data)
                                              (telegram-escape label)))))
        (format nil "~{~a~}"
                (remove nil
                        (list (format nil "<tg-button-row align=\"left\"><tg-button type=\"disabled\"~@[ style=\"~a\"~]>~a</tg-button></tg-button-row>"
                                      (cdr (assoc state +card-pill-styles+))
                                      (telegram-escape (if working (format nil "Working · ~a" elapsed) headline)))
                              (and task (format nil "<h3>~a</h3>" (esc (nlk:clip task 200 :ellipsis "…"))))
                              (and (or now thought)
                                   (format nil "<p>~{~a~^ · ~}</p>"
                                           (remove nil (list (and now (format nil "<b>~a</b>" (esc now)))
                                                             (and thought (format nil "<i>~a</i>" (esc thought)))))))
                              (and note (format nil "<p><i>~a</i></p>" (esc note)))
                              (and said (format nil "<p>~a</p>" (esc (nlk:clip said 1300 :ellipsis "…"))))
                              (and steps
                                   (format nil "<ul>~{~a~}</ul>"
                                           (loop for (mark words time) in steps
                                                 collect (format nil "<li><input type=\"checkbox\"~:[~; checked~]>~a · ~a</li>"
                                                                 (eq mark :done)
                                                                 (case mark
                                                                   (:failed (format nil "~a ~a" (nck:step-mark mark) (esc words)))
                                                                   (:stopped (format nil "<s>~a</s>" (esc words)))
                                                                   (otherwise (esc words)))
                                                                 time))))
                              (and pending (format nil "<p><b>Waiting</b><br>~{~a~^<br>~}</p>" (mapcar #'esc pending)))
                              (and footer (format nil "<hr/><footer>~{~a~^ · ~}</footer>" footer))
                              (and buttons (format nil "<tg-button-row align=\"left\">~{~a~}</tg-button-row>"
                                                   buttons)))))))))

(defun rich-message (text card controls)
  "The InputRichMessage that posts or edits a message: a CARD and its
CONTROLS as HTML, every word taken as written, so a path's /dev is no bot
command; else TEXT as rich Markdown (RICH-MARKDOWN)."
  (if card
      (nlk:json-object "html" (card-html card controls) "skip_entity_detection" t)
      (nlk:json-object "markdown" (rich-markdown text))))

;;; --- a message's words (rich Markdown) ---------------------------------------
;;; Every message but a file is a rich message, its words Markdown Telegram
;;; parses itself, so what the model writes -- in the markdown Discord draws --
;;; reads as it does there: bold, lists, code, quotes, and tables besides.
;;; Telegram's parser is GitHub's Markdown with HTML, and some of its habits
;;; would change what a chat line says: a lone line break would join the lines
;;; it parts; a tag it does not know would vanish without a word
;;; (`Vec<String>' read `Vec') and one it knows would open (an unclosed
;;; <details> would fold the rest of the message away); a picture by a link
;;; that holds none would refuse the whole message
;;; (RICH_MESSAGE_PHOTO_NO_MEDIA_FOUND); a word after a line's leading #
;;; would be a heading; a table would need a blank line over it. Outside
;;; code, then, RICH-MARKDOWN spells < as an entity, escapes the ! of ![
;;; and a leading # that opens no heading, keeps every line break (two
;;; spaces before it) and opens a table with a blank line. Code passes as
;;; written: Telegram draws it literally, entities and all. An & stays as
;;; it is: a link's address decodes no entity, so &amp; would break it.

(defun blank-line-p (line)
  "Whether LINE holds nothing but spaces and tabs."
  (every (lambda (ch) (member ch '(#\Space #\Tab))) line))

(defun fence-run (line &aux (start (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) line)))
  "(values CHAR LENGTH BARE-P) of the run of three or more ` or ~ that starts
LINE, a code fence's, BARE-P when nothing follows it; NIL when none does."
  ;; A backtick fence's info string holds no backtick: ```a``` is a code span.
  (when (and start (member (char line start) '(#\` #\~)))
    (let* ((char (char line start))
           (end (or (position char line :start start :test-not #'char=) (length line))))
      (when (and (>= (- end start) 3)
                 (not (and (char= char #\`) (find #\` line :start end))))
        (values char (- end start) (blank-line-p (subseq line end)))))))

(defun table-rule-p (line)
  "Whether LINE is a table's delimiter row: dashes between pipes."
  (and (find #\| line)
       (ppcre:scan "^\\s*\\|?\\s*:?-+:?\\s*(\\|\\s*:?-+:?\\s*)*\\|?\\s*$" line)
       t))

(defun code-span-end (line start length)
  "Where the code span whose opening run of LENGTH backticks ends at START
ends in LINE: past the next run of exactly LENGTH; NIL when none closes it."
  (do ((at (position #\` line :start start) (position #\` line :start end))
       (end start))
      ((null at) nil)
    (setf end (or (position #\` line :start at :test-not #'char=) (length line)))
    (when (= (- end at) length) (return end))))

(defun markdown-line (line &aux (size (length line))
                                (lead (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) line)))
  "LINE outside its code spans, which pass as written: < spelled as an
entity, and the ! of ![ and a leading # that opens no heading escaped."
  (with-output-to-string (out)
    (do ((at 0)) ((>= at size))
      (let ((ch (char line at))
            (after (and (< (1+ at) size) (char line (1+ at)))))
        (if (char= ch #\`)
            (let* ((run (or (position #\` line :start at :test-not #'char=) size))
                   (end (or (code-span-end line run (- run at)) run)))
              (write-string line out :start at :end end)
              (setf at end))
            (progn
              (cond ((char= ch #\<) (write-string "&lt;" out))
                    (t (when (or (and (char= ch #\!) (eql after #\[))
                                 (and (char= ch #\#) (eql at lead) after
                                      (not (member after '(#\# #\Space #\Tab)))))
                         (write-char #\\ out))
                       (write-char ch out)))
              (incf at)))))))

(defun rich-markdown (text &aux (lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
                                (fence nil))
  "TEXT, markdown as a chat platform draws it, as Telegram's rich Markdown
saying the same."
  ;; FENCE is the (CHAR . LENGTH) of the code fence a line stands in: a run of
  ;; the same CHAR at least LENGTH long with nothing after it closes it.
  (with-output-to-string (out)
    (dotimes (index (length lines))
      (let ((line (aref lines index))
            (next (and (< (1+ index) (length lines)) (aref lines (1+ index)))))
        (unless (zerop index) (terpri out))
        (multiple-value-bind (char length bare-p) (fence-run line)
          (cond (fence
                 (write-string line out)
                 (when (and (eql char (car fence)) (>= length (cdr fence)) bare-p)
                   (setf fence nil)))
                (char
                 (write-string line out)
                 (setf fence (cons char length)))
                (t
                 (when (and next (find #\| line) (table-rule-p next)
                            (plusp index) (not (blank-line-p (aref lines (1- index)))))
                   (terpri out))
                 (write-string (markdown-line line) out)
                 (when (and next (not (blank-line-p line)) (not (blank-line-p next)))
                   (write-string "  " out)))))))))

(defun media-group-plan (target files caption reply-to timeout-seconds)
  "One sendMediaGroup carrying FILES, CAPTION above the first of them — all
photos only when every file's bytes render inline."
  (let* ((paths (mapcar #'pathname files))
         (photos-p (every #'telegram-image-file-p paths))
         (media (coerce
                 (loop for index from 0 below (length paths)
                       collect (nlk:json-object
                                "type" (if photos-p "photo" "document")
                                "media" (format nil "attach://file~d" index)
                                :when (and (zerop index) (plusp (length caption)))
                                "caption" caption))
                 'vector)))
    (bot-plan "/sendMediaGroup"
              (append (upload-parts target reply-to)
                      (list (cons "media" (nlk:encode-json-object media)))
                      (loop for path in paths
                            for index from 0
                            collect (cons (format nil "file~d" index) path)))
              timeout-seconds t "send_media_group")))

(defun message-plan (target chunk &key reply-to ping controls mentions card label
                                         (timeout-seconds 60))
  "One sendRichMessage of CHUNK's words — CARD's HTML when CARD, its pictures
left out — its CONTROLS inside it."
  (let ((body (target-body target)))
    (setf (gethash "rich_message" body) (rich-message chunk.text card controls))
    (when (and reply-to (= 1 chunk.index))
      (setf (gethash "reply_parameters" body) (reply-parameters reply-to)))
    (unless (or ping mentions)
      (setf (gethash "disable_notification" body) t))
    (when (and controls (not card))
      (setf (gethash "reply_markup" body) (controls-markup controls)))
    (bot-plan "/sendRichMessage" body timeout-seconds t
              (let ((label (or label "send_rich_message")))
                (if (> chunk.total 1)
                    (format nil "~a_chunk_~a_of_~a" label chunk.index chunk.total)
                    label)))))

(defun continuation-plans (target text &key ping controls mentions (timeout-seconds 60))
  "The plans for TEXT — the words a media message's caption could not hold —
as text messages after the file that carries the answer, CONTROLS on the
last."
  ;; Neither replies to the ask nor notifies again: the file is the answer's
  ;; first message, and its face. A text message holds far more than a
  ;; caption, so this is where an answer's words are never lost.
  (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) text)))
    (loop for chunk in (split-text-chunks text +telegram-message-text-limit+)
          for last = (= chunk.index chunk.total)
          collect (message-plan target chunk
                                :ping ping
                                :controls (and last controls)
                                :mentions mentions
                                :label "answer_continuation"
                                :timeout-seconds timeout-seconds))))

(defun send-message-plan (target chunk &key reply-to ping controls files mentions card panel media
                                             (timeout-seconds 60))
  "The plans for CHUNK (a kit text-chunk) or of a turn's CARD: its file first,
then the words a media message's caption could not hold."
  ;; CHUNK's words go as rich Markdown (RICH-MARKDOWN); a CARD as its HTML
  ;; (CARD-HTML), its CONTROLS inside it and its pictures (MEDIA) left out; a
  ;; command's PANEL as its words.
  (declare (ignore panel media))
  ;; REPLY-TO makes chunk 1 a reply to the ask through reply_parameters —
  ;; allow_sending_without_reply, so an ask deleted meanwhile still gets its
  ;; answer — and later chunks read as a continuation. PING is whether the
  ;; message may notify: the answer does, being the one message that should
  ;; reach its asker; chrome — status lines, notices — goes out with
  ;; disable_notification, so a turn that ran twelve minutes reaches the
  ;; person exactly once, when it has something to say. MENTIONS names user
  ;; ids the post addresses: Telegram spells a bare id no mention, so an
  ;; addressed post notifies — the note that needs the operator reaches them.
  ;; CONTROLS — the kit's (LABEL . DATA) buttons — rides the first chunk as
  ;; the message's inline keyboard; an empty list posts it buttonless. FILES —
  ;; the answer's own picture — ride the first chunk too: one file as the
  ;; caption-carrying file message, several as one media group, so the message
  ;; that carries the answer carries the picture. A rich message unfurls no
  ;; link, so a link stays a link.
  (if (and files (= 1 chunk.index))
      ;; A media message's caption holds a quarter of what a text message
      ;; does, and the Bot API refuses the whole upload — picture and all —
      ;; for one over +TELEGRAM-CAPTION-LIMIT+ (400 "message caption is too
      ;; long"), which is how an answer carrying a picture used to be lost
      ;; whole. So the caption is the head of the answer that fits, and the
      ;; words it could not hold follow as text messages.
      (multiple-value-bind (caption rest)
          (clip-caption chunk.text +telegram-caption-limit+)
        (cons (if (rest files)
                  (media-group-plan target files caption reply-to timeout-seconds)
                  (telegram-file-plan target (first files)
                                       :content caption
                                       :reply-to reply-to
                                       :timeout-seconds timeout-seconds))
              (continuation-plans target rest
                                  :ping ping
                                  :controls (and (= chunk.index chunk.total) controls)
                                  :mentions mentions
                                  :timeout-seconds timeout-seconds)))
      (list (message-plan target chunk :reply-to reply-to :ping ping :controls controls
                          :mentions mentions :card card
                          :timeout-seconds timeout-seconds))))

(defun telegram-image-file-p (pathname)
  "Whether PATHNAME's bytes open an image the platform renders inline: the
file's own leading bytes, never its name, so a .txt that is a PNG still
posts as a photo. A file that cannot be read is not one."
  (nlk:with-handlers ((error () nil))
    (with-open-file (in pathname :element-type '(unsigned-byte 8))
      (let* ((octets (make-array 12 :element-type '(unsigned-byte 8)
                                    :initial-element 0))
             (read (read-sequence octets in)))
        (and (nle:image-media-type (subseq octets 0 read)) t)))))

(defun telegram-file-plan (target pathname &key content reply-to voice (timeout-seconds 60)
                                             (photo-p (telegram-image-file-p pathname)))
  "One sendPhoto (PHOTO-P), sendVoice (VOICE) or sendDocument carrying
PATHNAME — the Bot API renders a photo inline, plays a voice message and
files a document, and which one this is is what the bytes say
(TELEGRAM-IMAGE-FILE-P) unless the caller says."
  ;; CONTENT is the caption above it; REPLY-TO makes it a reply to the ask
  ;; through reply_parameters — allow_sending_without_reply, so an ask deleted
  ;; meanwhile still gets its picture — and the topic the target names follows
  ;; the file into it. VOICE, (:seconds S :waveform W), makes PATHNAME — Ogg
  ;; Opus — a voice message S seconds long; Telegram draws its own waveform.
  ;; The bytes ride as form-data, so the plan is a multipart alist, the shape
  ;; the executor already speaks.
  (multiple-value-bind (method part name)
      (cond (voice (values "/sendVoice" "voice" "send_voice"))
            (photo-p (values "/sendPhoto" "photo" "send_photo"))
            (t (values "/sendDocument" "document" "send_document")))
    (bot-plan method
              (append (upload-parts target reply-to content)
                      (and voice (list (cons "duration"
                                             (princ-to-string (ceiling (getf voice :seconds))))))
                      (list (cons part (pathname pathname))))
              timeout-seconds t name)))

(defun edit-message-plan (target message-id text &key retry controls card media
                                                      (timeout-seconds 60))
  "editMessageText restates a rich message in place — a turn's CARD (CARD-HTML),
its CONTROLS inside it and its pictures (MEDIA) left out, else TEXT as rich
Markdown; it deliberately carries no message_thread_id — the message id
already names the message."
  (declare (ignore media))
  ;; RETRY opts into 5xx/429 retries: running updates leave it off (the next
  ;; tick supersedes a missed edit), the terminal settle turns it on (nothing
  ;; follows it). Under TEXT, CONTROLS re-states the message's inline keyboard
  ;; — the kit's (LABEL . DATA) buttons, an empty list taking them away; NIL
  ;; leaves the keyboard as it is. A card's are part of the card. An edit
  ;; carrying text makes a rich message plain, so TEXT goes as Markdown too.
  (let ((body (message-body target message-id)))
    (setf (gethash "rich_message" body) (rich-message text card controls))
    (when (and controls (not card))
      (setf (gethash "reply_markup" body) (controls-markup controls)))
    (bot-plan "/editMessageText" body timeout-seconds (and retry t) "edit_message_text")))

(defun delete-message-plan (target message-id &key (timeout-seconds 60))
  "deleteMessage — retiring a status line once the answer it stood in for
has landed."
  ;; The bot may delete its own messages for 48 hours, which chrome always is.
  ;; Retried: a chrome message that outlives its answer is a duplicate the
  ;; reader has to reconcile, and nothing else will clean it up.
  (bot-plan "/deleteMessage" (message-body target message-id) timeout-seconds t "delete_message"))

(defun typing-plan (target &key (timeout-seconds 60) &aux (body (target-body target)))
  "sendChatAction typing: Telegram shows it for about five seconds, so the
platform re-asserts it on a shorter cadence than Discord's."
  ;; Posted into the topic when the target has one. Not retried: a missed beat
  ;; corrects itself on the next tick.
  (setf (gethash "action" body) "typing")
  (bot-plan "/sendChatAction" body timeout-seconds nil "typing_indicator"))

(defparameter +reaction-glyphs+ '(("✅" . "👍") ("❌" . "👎"))
  "The kit's marks Telegram's fixed reaction set lacks, each as the one of
its own that says the same.")

(defun reaction-plans (target message-id emoji &key previous
                                                    (timeout-seconds 60))
  "setMessageReaction — the bot's one reaction on MESSAGE-ID: EMOJI, or
none for NIL."
  ;; Telegram replaces the bot's whole reaction set in one call, so PREVIOUS
  ;; is not needed and one plan does every transition. Bots react in private
  ;; chats without rights; the emoji must be one of the platform's fixed set
  ;; (👀 is in it, ⏳ and ✅ are not: +REACTION-GLYPHS+). Retried: the mark
  ;; holds for the whole turn, so its clear must land even through a 429; a
  ;; chat that forbids reactions still refuses every time.
  (declare (ignore previous))
  (let ((body (message-body target message-id))
        (glyph (and emoji (or (cdr (assoc emoji +reaction-glyphs+ :test #'equal)) emoji))))
    (setf (gethash "reaction" body)
          (if glyph (vector (nlk:json-object "type" "emoji" "emoji" glyph)) #()))
    (list (bot-plan "/setMessageReaction" body timeout-seconds t "set_message_reaction"))))

(defun controls-markup (controls)
  "CONTROLS — the kit's buttons, rows of buttons and menus — as Telegram's
InlineKeyboardMarkup, in order: a lone button a row of its own, a row's
buttons side by side, a menu one row per option; :CLEAR the empty keyboard
that takes a message's buttons away; no rows for none."
  ;; A button is (LABEL DATA STYLE DISABLED): :DANGER draws red and :PRIMARY
  ;; blue, as on Discord, the rest Telegram's own face. Telegram draws no
  ;; disabled button and no select, so a disabled one is left out, and a
  ;; menu's options become buttons, the current one checked, a description
  ;; after its label; a line (NCK:LINE-CHOICE) is a button saying it, and
  ;; one with no button is said in the message's words alone; a button
  ;; whose data does not fit (CALLBACK-DATA-P) is left out.
  (flet ((button (label data &optional style)
           (and (callback-data-p data)
                (nlk:json-object "text" label "callback_data" data
                                 :when (member style '(:danger :primary)) "style" (string-downcase style)))))
    (nlk:json-object
     "inline_keyboard"
     (coerce
      (loop for control in (unless (eq controls :clear) controls)
            append (cond ((eq (first control) :menu)
                          (loop for (label data description current) in (third control)
                                for button = (button (format nil "~:[~;✓ ~]~a~@[ · ~a~]" current label description)
                                                     data)
                                when button collect (vector button)))
                         ((eq (first control) :row)
                          (destructuring-bind (mark label detail choice) (rest control)
                            (let ((button (and choice (not (fourth choice))
                                               (button (format nil "~@[~a ~]~a~@[ · ~a~]" mark label detail)
                                                       (second choice) (third choice)))))
                              (and button (list (vector button))))))
                         (t (let ((row (loop for (label data style disabled) in (if (consp (first control))
                                                                                      control
                                                                                      (list control))
                                             for button = (and (not disabled) (button label data style))
                                             when button collect button)))
                              (and row (list (coerce row 'vector)))))))
      'vector))))

(defun bot-plan (path body timeout-seconds retry label)
  "The POST of BODY to the Bot API method PATH: 5xx and 429 retried when
RETRY, LABEL naming it in audits and failure copy."
  (rest-plan "POST" path label retry timeout-seconds body))

(defun answer-callback-plans (payload &key text (timeout-seconds 60))
  "answerCallbackQuery — stops the spinner on the press PAYLOAD carries; TEXT,
a card's Details, shows the presser alone an alert of it."
  ;; Without TEXT the card the press touched is the answer. An alert holds 200
  ;; characters and no markdown: the head of the details, read plainly.
  (list (bot-plan "/answerCallbackQuery"
                  (nlk:json-object "callback_query_id" (getf payload :id)
                                   :when text "text" (nlk:clip (plain-details text) 199 :ellipsis "…")
                                   :when text "show_alert" t)
                  timeout-seconds nil "answer_callback_query")))

(defun press-response-plan (candidate text &key private controls panel (timeout-seconds 30))
  "The answer a press on a card's choice takes, in two steps as a Discord
interaction's: with TEXT NIL, answerCallbackQuery -- the spinner on the
button stops; with TEXT, editMessageText of the card pressed, which becomes
the answer, CONTROLS its keyboard. A PRIVATE answer to a command typed in a
group goes to the asker's own chat with the bot (PRIVATE-ANSWER-PLAN), held
open by a typing beat there that fails where the asker never opened one. NIL
-- the answer posted in the chat -- for any other private answer, which
Telegram cannot show one person alone in a group, and for a typed command
that is not private."
  (declare (ignore panel))
  (let ((query (source-field candidate "callback_query_id"))
        (card (source-field candidate "reply_to_message_id"))
        (pressed (candidate-pressed-p candidate))
        (user (source-field candidate "user_id")))
    (cond ((and private (not pressed) user
                (not (equal "direct_message" (source-field candidate "chat_kind"))))
           ;; The hold is the reachability probe: Telegram refuses a bot's
           ;; first word to someone who never opened its chat (403), and a
           ;; refused hold sends the kit's answer into the group as before.
           (if (null text)
               (typing-plan (list :channel-id user) :timeout-seconds timeout-seconds)
               (private-answer-plan candidate text controls :timeout-seconds timeout-seconds)))
          ((not (and query card pressed)) nil)
          ((null text)
           (first (answer-callback-plans (list :id query) :timeout-seconds timeout-seconds)))
          (private nil)
          (t (edit-message-plan (channel-target candidate) card text
                                :retry t :controls controls :timeout-seconds timeout-seconds)))))

(defun private-answer-plan (candidate text controls &key (timeout-seconds 30)
                                                     &aux (user (source-field candidate "user_id")))
  "sendRichMessage of TEXT to CANDIDATE's author in their own chat with the
bot: a command's private answer, asked in a group, that only its asker reads.
It opens by saying where it was asked, a one-line quote."
  ;; Not retried: the hold just proved the chat open, and a retry after an
  ;; answer lost in transit would say it twice.
  (let ((body (nlk:json-object "chat_id" user))
        (where (or (source-field candidate "chat_title") "the group")))
    (setf (gethash "rich_message" body)
          (rich-message (format nil "> From ~a~%~%~a" (markdown-line where) text) nil controls))
    (when controls
      (setf (gethash "reply_markup" body) (controls-markup controls)))
    (bot-plan "/sendRichMessage" body timeout-seconds nil "send_private_answer")))

;;; --- forum topics, a group's admins, a mention ----------------------------------
;;; A forum group's topic is Telegram's thread: the bot opens one per ask
;;; (createForumTopic) where it may manage topics, puts it away (closeForumTopic,
;;; open again once anyone writes in it... or an admin reopens it) and removes
;;; the one a silent turn left empty (deleteForumTopic). Every topic call names
;;; its chat: a topic id is a message id, unique only inside its group.

(defun topic-plan (target name &key (timeout-seconds 30) &aux (thread (getf target :thread-id)))
  "createForumTopic NAME in TARGET's chat, or, a TARGET naming a :THREAD-ID,
editForumTopic: that topic renamed NAME."
  ;; A create is not retried: it ADDS a topic, and a retry after an answer
  ;; lost in transit would open a second one for the same ask. A rename is.
  (let ((body (nlk:json-object "chat_id" (getf target :channel-id)
                               "name" (nlk:clip (nlk:one-line name) 127 :ellipsis "…"))))
    (if thread
        (progn (setf (gethash "message_thread_id" body) (or (parse-integer thread :junk-allowed t) thread))
               (bot-plan "/editForumTopic" body timeout-seconds t "rename_forum_topic"))
        (bot-plan "/createForumTopic" body timeout-seconds nil "create_forum_topic"))))

(defun topic-id-of (body)
  "The topic a createForumTopic answer BODY names, as a string, or NIL."
  (number-string (nlk:json-value body :any "result" "message_thread_id")))

(defun topic-close-plan (chat-id thread-id &key (timeout-seconds 30))
  "closeForumTopic: THREAD-ID in CHAT-ID put away -- no one but an admin
writes in it until it is reopened."
  ;; Retried: a close that fails leaves it open; closing a closed topic
  ;; changes nothing.
  (bot-plan "/closeForumTopic"
            (nlk:json-object "chat_id" chat-id
                             "message_thread_id" (or (parse-integer thread-id :junk-allowed t) thread-id))
            timeout-seconds t "close_forum_topic"))

(defun topic-delete-plan (chat-id thread-id &key (timeout-seconds 30))
  "deleteForumTopic: THREAD-ID in CHAT-ID and every message in it, gone."
  (bot-plan "/deleteForumTopic"
            (nlk:json-object "chat_id" chat-id
                             "message_thread_id" (or (parse-integer thread-id :junk-allowed t) thread-id))
            timeout-seconds t "delete_forum_topic"))

(defun chat-admins-plan (chat-id &key (timeout-seconds 10))
  "getChatAdministrators of CHAT-ID: who runs the group, the creator among them."
  (bot-plan "/getChatAdministrators" (nlk:json-object "chat_id" chat-id)
            timeout-seconds t "get_chat_administrators"))

(defun mention-markdown (user-id name)
  "The rich Markdown that mentions USER-ID by NAME -- a link to tg://user,
which Telegram draws as the person and notifies them by -- or NIL without an
id. NAME loses what would end the link's label early."
  (when (and (stringp user-id) (every #'digit-char-p user-id) (plusp (length user-id)))
    (format nil "[~a](tg://user?id=~a)"
            (let ((label (remove-if (lambda (ch) (find ch "[]()\\`*_")) (or name ""))))
              (if (plusp (length (nlk:trimmed label))) (nlk:trimmed label) "you"))
            user-id)))

(defun plain-details (text)
  "A card's details TEXT (NCK:DETAILS-TEXT) without its markdown: no bold,
no fences, no quote marks."
  (ppcre:regex-replace-all "(?m)^```.*\\n?|\\*\\*|^> " text ""))

(defun telegram-message-id (body)
  "The id of the message a send created, as a string."
  (number-string (nlk:json-value body :any "result" "message_id")))

(defun telegram-address (target message-id)
  "The address-book key for one Telegram message."
  ;; Message ids are per chat, so the chat rides in the key: message 12 in two
  ;; chats is two addresses.
  (format nil "~a:~a" (getf target :channel-id) message-id))

(defun telegram-command-name-p (name)
  "Telegram's rule for a menu entry: 1-32 of [a-z0-9_]."
  (and (stringp name) (<= 1 (length name) 32)
       (every (lambda (ch)
                (or (char<= #\a ch #\z) (char<= #\0 ch #\9) (char= ch #\_)))
              name)))

(defun set-my-commands-plan (entries &key (timeout-seconds 60))
  "setMyCommands with ENTRIES — (:name :description :usage) plists — as the
bot's command menu in the default scope, the one every chat falls back to."
  ;; A name outside Telegram's [a-z0-9_]{1,32} would refuse the whole call, so
  ;; it is left out; a description is cut to Telegram's 256 and never empty.
  ;; Retried: the menu is set once per catalog shape, and nothing else will
  ;; set it.
  (bot-plan "/setMyCommands"
            (nlk:json-object
             "commands"
             (coerce (loop for entry in entries
                           for name = (getf entry :name)
                           when (telegram-command-name-p name)
                             collect (nlk:json-object
                                      "command" name
                                      "description"
                                      (nlk:clip (or (nlk:json-value (getf entry :description) :text)
                                                    name)
                                                256 :ellipsis "")))
                     'vector))
            timeout-seconds t "set_my_commands"))

(defun get-file-plan (file-id &key (timeout-seconds 60))
  "getFile — the download path FILE-ID names."
  ;; The file endpoint itself is not a Bot API method: TELEGRAM-FILE-URL joins
  ;; the answer's file_path onto the token-bearing file root.
  (bot-plan "/getFile" (nlk:json-object "file_id" file-id) timeout-seconds t "get_file"))

(defun telegram-retry-after-ms (status body headers)
  "Telegram's authoritative 429 pacing rides the body:
{\"parameters\": {\"retry_after\": seconds}}."
  ;; Takes precedence over the retry-after header when present.
  (declare (ignore status headers))
  (let ((retry-after (nlk:json-value body :integer "parameters" "retry_after")))
    (and retry-after (* (max 0 retry-after) 1000))))

(defun telegram-failure-message (plan status detail &aux (lower (string-downcase detail))
                                                         (label plan.audit-label))
  (flet ((says (&rest needles)
           (some (lambda (needle) (search needle lower)) needles))
         (sends-p () (uiop:string-prefix-p "send_rich_message" label))
         (copy (text)
           (format nil "~? (status ~a: ~a)" text '() status detail)))
    (cond
      ((and (sends-p) (says "thread not found"))
       (copy "telegram sendRichMessage forum topic was not found or is ~
                not accessible; verify channels.telegram.~
                allowed_threads/message_thread_id and that the topic ~
                still exists; not retried without message_thread_id ~
                because that could deliver to the wrong topic"))
      ((and (sends-p)
            (says "chat not found" "bot was blocked" "forbidden"
                  "not enough rights"))
       (copy "telegram sendRichMessage target chat is unavailable or the ~
                bot lacks access; verify channels.telegram.~
                allowed_chats, bot membership, and bot permissions for ~
                the delivery chat"))
      ((and (sends-p) (= status 429))
       (copy "telegram sendRichMessage rate limited; retry later or ~
                reduce channel output volume"))
      ((and (equal label "get_updates") (says "conflict"))
       (copy "telegram getUpdates conflict: duplicate Telegram ~
                poller detected; stop the other nodecode Telegram lane ~
                or switch it to a different bot token"))
      (t (format nil "telegram Bot API ~a ~a failed with status ~a: ~a"
                 plan.method label status detail)))))

(defun telegram-not-modified-p (blob)
  "T when BLOB (a string, hash-table body, or formatted error) is Telegram's
400 'message is not modified' — the edit was a no-op because the live
message already matches — or TOPIC_NOT_MODIFIED: a topic closed, reopened or
renamed that already was."
  (flet ((has (s &aux (s (and (stringp s) (string-downcase s))))
           (and s (or (search "message is not modified" s)
                      (search "topic_not_modified" s)))))
    ;; An object says it wherever it says it: its description, or deeper.
    (cond
      ((has blob) t)
      ((hash-table-p blob) (has (ignore-errors (nlk:encode-json-object blob)))))))

(defun telegram-acknowledgement-p (body)
  "Whether BODY is Telegram's bare acknowledgement, {\"ok\":true,
\"result\":true}."
  ;; Every Bot API call answers inside an envelope, and for a write that
  ;; creates something the envelope carries the object — but for a pin, a
  ;; title, a member right, a deletion, a typing beat, the command menu and a
  ;; REACTION it carries the boolean T, which says the request was accepted
  ;; and nothing whatever about what is there.
  (and (hash-table-p body)
       (eq t (gethash "ok" body))
       (eq t (gethash "result" body))))

(defun wrap-telegram-executor (inner)
  "An executor speaking Telegram's envelope in the kit's terms: 'message is
not modified' is success, and a bare acknowledgement is no answer at all."
  (make-plan-executor
   :run (lambda (plan &aux (result (execute-plan inner plan)))
          ;; Telegram's 400 `message is not modified' is success.
          (when (and (not result.ok-p)
                     (= 400 result.status)
                     (or (telegram-not-modified-p result.error)
                         (telegram-not-modified-p result.body)))
            (setf result.ok-p t
                  result.error nil))
          ;; A bare acknowledgement is no body, so NCK:CALL reads the write back.
          (when (and result.ok-p (telegram-acknowledgement-p result.body)) (setf result.body nil))
          result)))

;;; --- what a write names -----------------------------------------------------

(defun telegram-method-name (path)
  "The Bot API method PATH names, without its leading slash or any query
string: \"/pinChatMessage\" => \"pinChatMessage\"."
  (let* ((path (subseq path 0 (or (position #\? path) (length path))))
         (start (position #\/ path :from-end t)))
    (if start (subseq path (1+ start)) path)))

(defun read-back-path (path body)
  "The read whose answer shows what a write of BODY to PATH changed, or NIL
when nothing this door can read shows it."
  ;; Telegram addresses its objects in the BODY and not in the path — the path
  ;; is only the method name — so this is a lookup where the Discord adapter
  ;; gets to truncate, and the read carries its ids in a query string:
  ;;
  ;;   pin, unpin, chat title / description / photo / permissions
  ;;       => getChat, whose pinned_message and title answer for them
  ;;   promote, restrict, ban, unban, an administrator's custom title
  ;;       => getChatMember, whose status and rights answer for them
  ;;   setMyCommands, deleteMyCommands
  ;;       => getMyCommands
  ;;
  ;; Nothing here reads a MESSAGE. A reaction, an edit and a deletion answer
  ;; with an acknowledgement and this door cannot confirm any of them, so they
  ;; read back nothing and are reported as sent rather than as landed. A send
  ;; needs no read: Telegram answers it with the message it created.
  (let* ((method (telegram-method-name path))
         ;; The ids live in a plist or object body; a multipart upload has none.
         (object (cond ((hash-table-p body) body)
                       ((and (consp body) (keywordp (car body))) (plist-json body))))
         (chat (and object (gethash "chat_id" object)))
         (user (and object (gethash "user_id" object))))
    (flet ((in (&rest names) (member method names :test #'string=)))
      (cond
        ((and chat (in "pinChatMessage" "unpinChatMessage"
                       "unpinAllChatMessages" "setChatTitle"
                       "setChatDescription" "setChatPhoto"
                       "deleteChatPhoto" "setChatPermissions"))
         (format nil "/getChat?chat_id=~a" chat))
        ((and chat user (in "promoteChatMember" "restrictChatMember"
                            "banChatMember" "unbanChatMember"
                            "setChatAdministratorCustomTitle"))
         (format nil "/getChatMember?chat_id=~a&user_id=~a" chat user))
        ((in "setMyCommands" "deleteMyCommands") "/getMyCommands")))))

(defun make-telegram-executor (&key (api-base +telegram-api-base+) token)
  (wrap-telegram-executor
   (make-dexador-executor
    :base-url (format nil "~a/bot~a" (string-right-trim "/" api-base) token)
    :redact-prefixes (list (format nil "bot~a" token) token)
    :failure-message-fn #'telegram-failure-message
    :retry-after-fn #'telegram-retry-after-ms)))

;;; --- attachments (files the token reaches) ------------------------------
;;; A photo, recording or document a bot may download: getFile names a path, and the
;;; bytes come from the file endpoint — the one fetch outside the plan
;;; machinery, because it answers bytes and not the Bot API's JSON
;;; envelope. The root carries the token exactly as the executor's base
;;; URL does, and it is never printed: a file that cannot be fetched is
;;; reported by what happened, never by the URL it happened to.

(defvar *telegram-file-root* nil
  "The Bot API file endpoint root — <api_base>/file/bot<token> — the live
lane set at start, or NIL when the lane is stopped.")

(defun telegram-file-url (file-path)
  "The download URL for FILE-PATH, getFile's answer; a refusal when the
lane is not running."
  ;; Exported for tests; the caller that matters is TELEGRAM-FILE-FETCHER's
  ;; download.
  (unless (and (stringp *telegram-file-root*)
               (plusp (length *telegram-file-root*)))
    (error "the telegram channel is not running"))
  (format nil "~a/~a" *telegram-file-root* file-path))
