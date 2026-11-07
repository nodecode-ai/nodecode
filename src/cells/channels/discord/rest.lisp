;;;; rest.lisp --- Discord REST v10 request plans and executor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure plan builders (tested against the recording executor) plus the one
;;;; live executor carrying the Bot authorization header. Semantics ported
;;;; from the Zig-era pack: 2000-char chunks, allowed_mentions {parse: []}
;;;; on every message so channel replies can never ping, message_reference
;;;; (quote-reply) on chunk 1 only, typing POST re-asserted on the kit's
;;;; refresh cadence.

(in-package #:nodecode-channel-discord)

(defparameter +discord-rest-api-base+ "https://discord.com/api/v10")
(defparameter +discord-message-content-limit+ 2000)
;;; A thread created WITHOUT a starter message has to name its type — the one
;;; field the hung-off-a-message form does not take.
(defparameter +discord-public-thread-type+ 11
  "Discord's PUBLIC_THREAD.")

;;; Every message this bot posts carries it, so a link an answer names stays a
;;; link — clickable — and never unfurls a preview card the room did not ask
;;; for. Discord has no server-wide switch for this; the flag on the message
;;; itself is the only API-level one.
(defparameter +discord-suppress-embeds+ (ash 1 2)
  "SUPPRESS_EMBEDS.")

(defparameter +discord-message-content-intent+ (ash 1 15)
  "MESSAGE_CONTENT, the privileged intent the Developer Portal must grant.")

(defparameter +discord-default-intents+
  (logior (ash 1 0) (ash 1 7) (ash 1 9) (ash 1 10) (ash 1 12) (ash 1 13)
          +discord-message-content-intent+)
  "GUILDS | GUILD_VOICE_STATES | GUILD_MESSAGES | GUILD_MESSAGE_REACTIONS |
DIRECT_MESSAGES | DIRECT_MESSAGE_REACTIONS | MESSAGE_CONTENT. Voice states are
not privileged, and without them /voice join cannot find who asked nor hear
that the bot sat down.")

(defparameter +discord-without-content-detail+
  "message content intent not granted in the Developer Portal: answering mentions, replies and DMs only"
  "What the channel status says while the lane runs without MESSAGE_CONTENT.")

(defun intents-without-refused (code intents)
  "The intents to identify with again after the gateway closed with CODE, or
NIL when the close leaves nothing to retry with."
  ;; 4014 is a privileged intent the Developer Portal has not granted. Without
  ;; MESSAGE_CONTENT Discord still sends the words of a message that mentions
  ;; the bot, a reply to it and a DM — the whole of a room that requires a
  ;; mention — so the lane carries on with that rather than stopping for good
  ;; over a toggle the guide calls optional. A 4014 with the intent already
  ;; dropped is another intent the section asked for, and stays fatal.
  (and (eql code 4014)
       (logtest intents +discord-message-content-intent+)
       (logandc2 intents +discord-message-content-intent+)))

(defun target-rest-channel (target)
  "REST posts land in the thread when the target has one."
  (or (getf target :thread-id) (getf target :channel-id)))

;;; --- a turn's card (the kit's DIGEST-CARD, drawn as Components V2) ---------
;;; The kit owns what a card says; how it looks is the platform's. Here: one
;;; container of components, its accent the phase — blurple while the turn
;;; works, green done, grey stopped or queued, red failed — so a room scanned
;;; at a glance reads which asks are still being worked on. Each step is a row
;;; of its own, carrying the button that shows its presser what the step
;;; answered; the pictures the turn looked at are a gallery; the card's own
;;; buttons sit inside it. A Components V2 message is its components alone —
;;; no words, no embeds — and keeps that flag for good, so every edit of a
;;; card is a whole card.

(defparameter +discord-components-v2+ (ash 1 15)
  "The IS_COMPONENTS_V2 message flag: the message is its components alone,
from its post on.")

(defparameter +card-gallery-max+ 10
  "Pictures one gallery holds, Discord's cap.")

(defvar *card-marks* '()
  "The application emojis a card's marks wear — (:done SPELLING :running
SPELLING :stopped SPELLING) — once the bot holds them (ENSURE-CARD-MARKS);
empty, the kit's text marks. Live-only: every start asks Discord again.")

(defun card-mark (mark)
  "MARK as a card draws it: its application emoji, else its text."
  (or (getf *card-marks* mark) (nck:step-mark mark)))

(defparameter +card-colours+
  '((:working . #x5865F2) (:queued . #x80848E) (:done . #x23A55A)
    (:stopped . #x80848E) (:failed . #xDA373C) (:paused . #xF0B232))
  "The accent a card wears for each state, a command panel's for its tone.")

(defun discord-escape (text)
  "TEXT with Discord's markdown characters escaped, so a file name's
underscores stay underscores."
  (ppcre:regex-replace-all "([*_~`|\\\\])" text "\\\\\\1"))

(defun text-display (text cap)
  "TEXT as one text display, cut at CAP characters, the cut's mark counted."
  (nlk:json-object "type" 10 "content" (nlk:clip text cap :ellipsis "…")))

(defun staged-picture (name path)
  "PATH copied to a file named NAME, in a folder of its own under the cache:
the name its upload is sent under is the name its card calls it by
(attachment://NAME). Folders a day old go first."
  ;; Discord pairs a reference with the uploaded file's own name, and an upload
  ;; is named after its file; the copy happens once per picture.
  (let ((root (nlk:cache-path "nodecode/card-pictures/")))
    (dolist (old (ignore-errors (uiop:subdirectories root)))
      (when (< (or (ignore-errors (file-write-date old)) 0) (- (get-universal-time) 86400))
        (ignore-errors (uiop:delete-directory-tree old :validate t))))
    (let ((copy (merge-pathnames name (merge-pathnames (format nil "~a/" (nlk:make-durable-id "card")) root))))
      (ensure-directories-exist copy)
      (uiop:copy-file path copy)
      copy)))

(defun card-pictures (images media)
  "The gallery IMAGES make — a card's pictures, (NAME PATH DESCRIPTION) each,
oldest first — on a message that holds MEDIA, (NAME . ATTACHMENT-ID) per
picture it carries already. => (values ITEMS UPLOADS ATTACHMENTS): the
gallery's items, the files this request sends (files[i], in order), and the
attachments array: what the message keeps, by id, and what it gains, by
index."
  ;; A picture the message carries is kept, never sent again: a card edits
  ;; every two seconds, and each upload would be a new file that clients load
  ;; again. A new one whose file is gone is left out rather than refused.
  ;; Every item names its file attachment://NAME, so a request that touches
  ;; pictures lists every one it keeps: Discord resolves the names against
  ;; that list (discord-api-docs#7529).
  (let ((items '()) (uploads '()) (attachments '()))
    (loop for (name path description) in (last images +card-gallery-max+)
          for kept = (cdr (assoc name media :test #'equal))
          when (or kept (and path (probe-file path)))
            do (push (nlk:json-object "id" (or kept (length uploads)) "filename" name) attachments)
               (unless kept
                 (setf uploads (append uploads (list (staged-picture name path)))))
               (push (nlk:json-object "media" (nlk:json-object "url" (format nil "attachment://~a" name))
                                      :when description
                                      "description" (nlk:clip description 1023 :ellipsis "…"))
                     items))
    (values (nreverse items) uploads (nreverse attachments))))

(defun card-components (card controls &key media)
  "CARD (NCK:DIGEST-CARD) and its CONTROLS as a Components V2 message: one
container, its accent the state; the state and the time in small type over
the ask's task as its heading; what the turn is doing when no step shows it,
its thought in italics and its note; the words it says on its way, or is
writing, in a block of their own; a row per step, with an Output button
where the step has something to show; a Waiting block for the input parked
behind it; its pictures as a gallery; its numbers in small type under a rule;
CONTROLS, rows of buttons, at its foot. A step wears its mark (CARD-MARK) — a
green check done, a spinner running — and while the turn works the spinner
opens the description too, so a working card always moves. A card whose ask
has no title yet is titled what the turn does now, or how it ended. MEDIA is
what the message holds already (CARD-PICTURES). => (values COMPONENTS UPLOADS
ATTACHMENTS)."
  ;; Discord's caps on one V2 message: 40 components however nested, 4000
  ;; characters across its text. Each part is cut so the whole stays inside
  ;; both.
  (destructuring-bind (&key state elapsed task headline thought steps earlier note meta pending images said)
      card
    (multiple-value-bind (items uploads attachments) (card-pictures images media)
      (let* ((working (eq state :working))
             ;; Under a task, what the turn does now is the running step's row,
             ;; or — between steps — the description's opening words.
             (now (and task working (not (find :running steps :key #'first)) headline))
             (opening (format nil "~{~a~^ ~}"
                              (remove nil (list (and working (getf *card-marks* :running))
                                                (and now (discord-escape now))
                                                (and now thought "·")
                                                (and thought (format nil "*~a*" (discord-escape thought)))))))
             (description (format nil "~{~a~^~%~}"
                                  (remove nil (list (and (plusp (length opening)) opening)
                                                    ;; Every line of the note, as CARD-TEXT draws it
                                                    ;; (DIGEST-NOTE-LINES): a walked failover chain's
                                                    ;; failure is a model a line, each with what its
                                                    ;; provider said.
                                                    (and note (format nil "~{> ~a~^~%~}"
                                                                      (mapcar #'discord-escape
                                                                              (nck:digest-note-lines note))))))))
             ;; The description takes up to 1100 characters; what it takes past
             ;; 500 comes off what the model said, so the card's text stays
             ;; inside Discord's 4000.
             (description-cap (min 1100 (max 500 (length description))))
             (parts
               (append
                (list (text-display (format nil "-# ~(~a~)~@[ · ~a~]~%### ~a" state
                                            (and (or working (and task (not (eq state :queued)))) elapsed)
                                            (nlk:clip (discord-escape (or task headline)) 200 :ellipsis "…"))
                                    300))
                (and (plusp (length description)) (list (text-display description description-cap)))
                ;; The model's own words, its markdown drawn as it wrote it.
                (and said (list (text-display said (- 1300 (- description-cap 500)))))
                (and earlier (plusp earlier) (list (text-display (format nil "-# +~d earlier" earlier) 40)))
                (loop for (mark words time press) in steps
                      for row = (text-display (format nil "~a ~a · ~a" (card-mark mark) (discord-escape words) time)
                                              200)
                      collect (if press
                                  (nlk:json-object "type" 9 "components" (vector row)
                                                   "accessory" (nlk:json-object "type" 2 "style" 2
                                                                                "label" "Output"
                                                                                "custom_id" press))
                                  row))
                (and pending (list (text-display (format nil "**Waiting**~%~{~a~^~%~}" pending) 550)))
                (and items (list (nlk:json-object "type" 12 "items" (coerce items 'vector))))
                (and meta (list (nlk:json-object "type" 14 "divider" t "spacing" 1)
                                (text-display (format nil "~{-# ~a~^~%~}"
                                                      (uiop:split-string (discord-escape meta)
                                                                         :separator '(#\Newline)))
                                              250)))
                (and controls (not (eq controls :clear)) (coerce (controls-components controls) 'list)))))
        (values (vector (nlk:json-object "type" 17
                                         "accent_color" (or (cdr (assoc state +card-colours+))
                                                            (cdr (assoc :working +card-colours+)))
                                         "components" (coerce parts 'vector)))
                uploads attachments)))))

(defun card-body (target card controls &key media reply-to edit)
  "CARD and its CONTROLS as one Components V2 message body on TARGET (CARD-
COMPONENTS), a silent reply to REPLY-TO; EDIT for a PATCH, which empties the
words and embeds a message posted before cards were components would still
carry. => (values BODY UPLOADS)."
  (multiple-value-bind (components uploads attachments) (card-components card controls :media media)
    (values (nlk:json-object
             "flags" +discord-components-v2+
             :when edit "content" :null
             :when edit "embeds" #()
             "components" components
             "allowed_mentions" (nlk:json-object "parse" #())
             :when reply-to
             "message_reference" (nlk:json-object "message_id" reply-to
                                                  "channel_id" (target-rest-channel target)
                                                  "fail_if_not_exists" nil)
             ;; Pictures touched or dropped: the list of what stays, possibly none.
             :when (or attachments media) "attachments" (coerce attachments 'vector))
            uploads)))

(defun message-media (body)
  "The pictures the message BODY describes carries: (FILENAME . ID) per
picture its components show, the MEDIA a card's next edit keeps them by."
  ;; A Components V2 message lists no attachments of its own: a file it
  ;; carries is the media item showing it, its attachment_id and its name the
  ;; URL's last segment. Read from the attachments array, always empty, every
  ;; edit uploaded the gallery again, a new file each time, and a picture open
  ;; in the viewer failed to load.
  (labels ((walk (part)
             (let* ((media (nlk:json-value part :object "media"))
                    (id (nlk:json-value media :string "attachment_id"))
                    (url (nlk:json-value media :string "url"))
                    (path (and url (subseq url 0 (or (position #\? url) (length url)))))
                    (name (and path (subseq path (1+ (or (position #\/ path :from-end t) -1))))))
               (append (and id (plusp (length name)) (list (cons name id)))
                       (loop for item across (or (nlk:json-value part :array "items") #())
                             append (walk item))
                       (loop for child across (or (nlk:json-value part :array "components") #())
                             append (walk child))))))
    (loop for part across (or (nlk:json-value body :array "components") #())
          append (walk part))))

(defun panel-components (panel controls)
  "PANEL, a command's card (NCK:OFFER-CARD), and its CONTROLS as one
Components V2 container -- the card a turn is drawn as (CARD-COMPONENTS):
its tone the accent; the words said over it; its title, the muted words
beside it in small type under it; its picture in one row (:GAUGE); its
words; its fields a line each; then CONTROLS, its lines (NCK:LINE-CHOICE)
each a section with its button, the rest rows at its foot."
  ;; Discord's caps on one V2 message: 40 components however nested, 4000
  ;; characters across its text; each part is cut so the whole stays inside.
  (destructuring-bind (&key title context gauge text fields tone notice) panel
    (let ((parts (append
                  (and notice (plusp (length notice)) (list (text-display notice 900)))
                  ;; a panel of words alone (an answer to a press) has no heading
                  (and (or title context)
                       (list (text-display (format nil "~@[### ~a~]~:[~;~%~]~@[-# ~a~]"
                                                   (and title (nlk:clip (discord-escape title) 200 :ellipsis "…"))
                                                   (and title context)
                                                   (and context (discord-escape context)))
                                           400)))
                  (and gauge (plusp (length gauge)) (list (text-display gauge 200)))
                  (and text (plusp (length text)) (list (text-display text 1500)))
                  (and fields
                       (list (text-display (format nil "~{~a~^~%~}"
                                                   (loop for (name . value) in (subseq fields 0 (min 25 (length fields)))
                                                         collect (format nil "**~a** · ~a" name value)))
                                           900)))
                  (and controls (not (eq controls :clear))
                       (coerce (controls-components controls :v2 t) 'list)))))
      (vector (nlk:json-object "type" 17
                               "accent_color" (or (cdr (assoc tone +card-colours+))
                                                  (cdr (assoc :working +card-colours+)))
                               "components" (coerce parts 'vector))))))

;;; --- controls (the buttons and menus the kit hangs on a message) ----------
;;; The kit owns the vocabulary — a button, a row of buttons, a menu (NCK's
;;; controls section) — and which message carries it; how a platform renders
;;; one and how a press comes back is the platform's own. Here: components on
;;; the message, a component interaction off the gateway.

(defun control-style (control)
  "CONTROL's Discord button style: :DANGER the red a stop carries, :PRIMARY
the blurple a call to action carries, anything else the neutral grey."
  (ecase (or (third control) :secondary)
    (:danger 4)
    (:primary 1)
    (:secondary 2)))

(defun control-button (control)
  "One kit button — (LABEL DATA STYLE DISABLED) — as a Discord button: its
custom_id carries DATA back verbatim on a press."
  (nlk:json-object
   "type" 2
   "style" (control-style control)
   "label" (nlk:clip (first control) 79 :ellipsis "…")
   "custom_id" (second control)
   :when (fourth control) "disabled" t))

(defun control-menu (menu index)
  "One kit menu — (:MENU PLACEHOLDER OPTIONS) — as a Discord string select,
the INDEXth on its message: each option (LABEL DATA DESCRIPTION CURRENT)
carries DATA back as the picked value, CURRENT marks the one shown picked.
NIL for a menu with no option Discord would take."
  ;; A value is at most 100 characters and a select 25 options; an option
  ;; whose data runs longer is left out rather than cut, since cut data names
  ;; something else. The custom_id only tells two menus on one message apart.
  (let ((options (loop for (label data description current) in (third menu)
                       when (<= 1 (length data) 100)
                         collect (nlk:json-object
                                  "label" (nlk:clip label 99 :ellipsis "…")
                                  "value" data
                                  :when description "description" (nlk:clip description 99 :ellipsis "…")
                                  :when current "default" t))))
    (when options
      (nlk:json-object
       "type" 3
       "custom_id" (format nil "nck:menu:~d" index)
       "placeholder" (nlk:clip (second menu) 149 :ellipsis "…")
       "options" (coerce (subseq options 0 (min 25 (length options))) 'vector)))))

(defun control-line (line v2)
  "One kit line (NCK:LINE-CHOICE), (:ROW MARK LABEL DETAIL BUTTON): in a
Components V2 message (V2) a section, its words beside its button, or its
words alone where it has none; elsewhere a row of one button saying them, or
nothing."
  (destructuring-bind (mark label detail button) (rest line)
    (flet ((said (bold)
             ;; mark, label and detail, a line with no label its detail alone
             (let ((label (and (plusp (length label)) label)))
               (format nil "~@[~a ~]~@[~a~]~:[~; · ~]~@[~a~]" mark
                       (and label (if bold (format nil "**~a**" (discord-escape label)) label))
                       (and label detail)
                       (and detail (if bold (discord-escape detail) detail))))))
      (if v2
          (let ((words (text-display (said t) 300)))
            (if button
                (nlk:json-object "type" 9 "components" (vector words) "accessory" (control-button button))
                words))
          (and button
               (nlk:json-object "type" 1 "components"
                                (vector (control-button (list* (said nil) (rest button))))))))))

(defun controls-components (controls &key v2)
  "CONTROLS — the kit's buttons, rows of buttons, menus and lines — as
Discord's components array: an action row each, in order — a lone button
alone, a row's buttons side by side, a menu its select — and a line its
section inside a Components V2 message (V2, CONTROL-LINE); :CLEAR the empty
array that takes a message's components away; no rows for none."
  ;; Discord takes five rows and five buttons a row; the kit's cards stay
  ;; inside both, and anything past them is dropped here, not refused there.
  ;; A V2 message counts its components instead (PANEL-COMPONENTS).
  (cond
    ((eq controls :clear) #())
    (controls
     (coerce
      (loop for control in controls
            for index from 0
            for part = (if (eq (first control) :row)
                           (control-line control v2)
                           (let ((inner (cond ((eq (first control) :menu)
                                               (nlk:when-let (menu (control-menu control index)) (list menu)))
                                              ((consp (first control))
                                               (mapcar #'control-button (subseq control 0 (min 5 (length control)))))
                                              (t (list (control-button control))))))
                             (and inner (nlk:json-object "type" 1 "components" (coerce inner 'vector)))))
            when part
              collect part into parts
            finally (return (if v2 parts (subseq parts 0 (min 5 (length parts))))))
      'vector))))

(defun discord-message-plan (target chunk &key reply-to (timeout-seconds 30) ping controls files
                                                mentions card panel media)
  "One sendMessage chunk, a command's PANEL drawn as its embed, or a turn's
CARD drawn as its components (CARD-BODY), MEDIA the pictures it holds."
  ;; REPLY-TO quote-replies the triggering user message on chunk 1
  ;; only — later chunks read as a continuation, not as repeated replies to
  ;; the same prompt.
  ;;
  ;; FILES — the answer's own picture — ride chunk 1 as a multipart upload:
  ;; the message that carries the answer carries the picture, with the same
  ;; reference, the same ping and the same attachments array every upload
  ;; speaks.
  ;;
  ;; The reference names the channel the POST lands in — the thread when the
  ;; target has one — because Discord refuses a reply whose reference names
  ;; another channel (50035 REPLIES_CANNOT_REFERENCE_OTHER_CHANNEL), and an
  ;; ask typed inside a thread is answered from that thread.
  ;;
  ;; PING sets allowed_mentions.replied_user on that first chunk, so the person
  ;; who asked is notified when their answer lands. MENTIONS — user ids the post
  ;; must reach — are named in allowed_mentions.users; the kit spells them
  ;; <@id> ahead of the text before cutting it (the platform's MENTION), so
  ;; the first chunk carries them: the note that needs the operator reaches them.
  ;; Those two are the only pings this adapter emits, and both are the kit's
  ;; decision; parse stays [] on every message, so no content in a model's
  ;; answer can ping a role, a bystander, or @everyone. Chrome — status lines,
  ;; notices — never pings; a turn that ran twelve minutes should reach its
  ;; asker exactly once, when it has something to say. CONTROLS — the kit's
  ;; buttons and menus — ride the chunk they are handed to (the kit hands them
  ;; to the last) as the message's components; an absent list posts it bare.
  ;; Every message carries SUPPRESS_EMBEDS, so a link an answer names stays a
  ;; link and never unfurls a preview card — but a PANEL, which is a message
  ;; of components (PANEL-COMPONENTS) and posts no words. A CARD is a
  ;; message of components: its CONTROLS sit inside it, and the pictures it
  ;; gains ride as uploads.
  (let ((first-p (= 1 (text-chunk-index chunk))))
    (multiple-value-bind (body files)
        (if card
            (card-body target card controls :media media :reply-to (and first-p reply-to))
            (values (message-body target (if panel "" (text-chunk-text chunk))
                                  :reply-to (and first-p reply-to) :ping ping
                                  :mentions mentions :controls controls :files (and first-p files)
                                  :panel panel)
                    (and first-p files)))
      (rest-plan "POST" (format nil "/channels/~a/messages" (target-rest-channel target))
                 (let ((kind (if files "send_file" "send_message")))
                   (if (> (text-chunk-total chunk) 1)
                       (format nil "~a_chunk_~a_of_~a" kind
                               (text-chunk-index chunk) (text-chunk-total chunk))
                       kind))
                 t timeout-seconds (if files (discord-file-parts body files) body)))))

(defun message-body (target content &key reply-to ping mentions controls files panel)
  "One message's JSON body: CONTENT, flagged SUPPRESS_EMBEDS, pinging only the
MENTIONS and — with PING — the author of REPLY-TO, quote-replying REPLY-TO in
the channel the POST lands in, CONTROLS as its components, FILES attached; a
PANEL is a message of components (PANEL-COMPONENTS), CONTROLS inside it, and
no words."
  (nlk:json-object
   :when (not panel) "content" content
   "flags" (if panel +discord-components-v2+ +discord-suppress-embeds+)
   "allowed_mentions" (nlk:json-object "parse" #()
                                       :when mentions
                                       "users" (coerce mentions 'vector)
                                       :when (and reply-to ping)
                                       "replied_user" t)
   :when reply-to
   "message_reference" (nlk:json-object "message_id" reply-to
                                        "channel_id" (target-rest-channel target)
                                        "fail_if_not_exists" nil)
   :when (or controls panel)
   "components" (if panel
                    (panel-components panel controls)
                    (controls-components controls))
   :when files
   "attachments" (coerce (loop for path in files
                               for index from 0
                               collect (nlk:json-object
                                        "id" index
                                        "filename" (file-namestring path)))
                         'vector)))

(defun discord-file-parts (payload pathnames)
  "PAYLOAD as the payload_json multipart part, plus one files[i] part per
PATHNAME, in order — the body every upload speaks (REST v10)."
  (cons (cons "payload_json" (nlk:encode-json-object payload))
        (loop for path in pathnames
              for index from 0
              collect (cons (format nil "files[~d]" index) (pathname path)))))

(defparameter +discord-voice-message+ 8192
  "The IS_VOICE_MESSAGE message flag: the one attachment plays as a voice
message, its waveform drawn, rather than as a file.")

(defun discord-file-message-plan (target pathname &key content reply-to voice timeout-seconds
                                  &aux (path (pathname pathname)))
  "One Discord multipart file upload (REST v10 POST /channels/.../messages): a
minute is the floor, a file being heavier than a message."
  ;; PATHNAME is files[0]; CONTENT is the optional caption. payload_json
  ;; carries allowed_mentions {parse: []} and attachments[{id:0,filename}] so
  ;; the file renders as an attachment, never as a mention — a picture never
  ;; pings; the answer is the one message that reaches its asker. REPLY-TO
  ;; quote-replies that message, the same reference an answer carries.
  ;;
  ;; VOICE, (:seconds S :waveform OCTETS), makes PATHNAME — Ogg Opus — a voice
  ;; message: flagged IS_VOICE_MESSAGE, its attachment declaring duration_secs
  ;; and the waveform as base64, and no words, which Discord refuses on one
  ;; (Hermes adapter_media.py, OpenClaw voice-message.ts).
  (setf reply-to (and (stringp reply-to) (plusp (length reply-to)) reply-to))
  (rest-plan "POST" (format nil "/channels/~a/messages" (target-rest-channel target))
             (if voice "send_voice" "send_file") t (max 60 (or timeout-seconds 0))
             (discord-file-parts
              (if voice
                  (nlk:json-object
                   "flags" +discord-voice-message+
                   "allowed_mentions" (nlk:json-object "parse" #())
                   :when reply-to
                   "message_reference" (nlk:json-object "message_id" reply-to
                                                        "channel_id" (target-rest-channel target)
                                                        "fail_if_not_exists" nil)
                   "attachments" (vector (nlk:json-object
                                          "id" 0 "filename" (file-namestring path)
                                          "duration_secs" (getf voice :seconds)
                                          "waveform" (cl-base64:usb8-array-to-base64-string
                                                      (getf voice :waveform)))))
                  (message-body target (or content "") :reply-to reply-to :files (list path)))
              (list path))))

(defun edit-message-plan (target message-id text &key (timeout-seconds 30)
                                                      retry controls card media)
  "PATCH one message — a turn's card editing in place, or a message's words."
  ;; RETRY opts into 5xx/429 retries: running updates leave it off (the next
  ;; tick supersedes a missed edit), the terminal settle turns it on (nothing
  ;; follows it). A CARD restates the whole message, its CONTROLS inside it
  ;; and the pictures it holds (MEDIA) kept, those it gains uploaded
  ;; (CARD-BODY): a card's message is its components for good, so an edit of
  ;; one is always a card. Without a card the words are the message, and
  ;; CONTROLS — the kit's (LABEL . DATA) buttons — restates its components;
  ;; an absent list leaves them as they stand.
  (multiple-value-bind (body files)
      (if card
          (card-body target card controls :media media :edit t)
          (nlk:json-object "content" text
                           "allowed_mentions" (nlk:json-object "parse" #())
                           :when controls "components" (controls-components controls)))
    (rest-plan "PATCH" (format nil "/channels/~a/messages/~a"
                               (target-rest-channel target) message-id)
               "edit_message" retry timeout-seconds
               (if files (discord-file-parts body files) body))))

(defun delete-message-plan (target message-id &key (timeout-seconds 30))
  "DELETE one message — retiring a status line once the answer it stood in
for has landed."
  ;; Retried: a chrome message that outlives its answer is a duplicate the
  ;; reader has to reconcile, and nothing else will clean it up. Ordered after
  ;; the answer's POST by the caller, so a failure leaves a stale line rather
  ;; than a gap where the answer should be.
  (rest-plan "DELETE" (format nil "/channels/~a/messages/~a"
                              (target-rest-channel target) message-id)
             "delete_message" t timeout-seconds))

(defun typing-plan (target &key (timeout-seconds 30))
  "Discord shows \"is typing…\" for ~10s per POST; the delivery worker's
tick re-asserts this while a turn is in flight."
  ;; Not retried: a missed typing beat corrects itself on the next tick.
  (rest-plan "POST" (format nil "/channels/~a/typing" (target-rest-channel target))
             "typing_indicator" nil timeout-seconds))

(defun thread-create-plan (target name &key message-id (timeout-seconds 30))
  "POST /channels/<parent>/messages/<message>/threads — a public thread hung
off the ask's own message, named NAME, so the question and the surface its
answer lands on are one click apart — or, with no MESSAGE-ID, POST
/channels/<parent>/threads: a public thread of the channel's own, which is
what an ask ROUTED INTO this channel gets, its message having stayed in the
room it was typed in. A TARGET naming a :THREAD-ID is PATCH
/channels/<thread>: that thread, named NAME."
  ;; Needs Create Public Threads and Send Messages in the parent — a thread
  ;; without a starter message needs Manage Threads too — and the name is what
  ;; the thread list and the channel read. A create is not retried: it ADDS a
  ;; surface, and a retry after an answer lost in transit would open a second
  ;; thread on the same message. A rename is: a thread's creator may rename
  ;; it without Manage Threads, and Discord allows two a thread in ten
  ;; minutes, so a 429 waits its turn.
  (nlk:if-let (thread (getf target :thread-id))
    (rest-plan "PATCH" (format nil "/channels/~a" thread) "rename_thread" t timeout-seconds
               (nlk:json-object "name" name))
    (rest-plan "POST"
               (format nil "/channels/~a~@[/messages/~a~]/threads"
                       (getf target :channel-id) message-id)
               "create_thread" nil timeout-seconds
               (nlk:json-object "name" name
                                :when (null message-id) "type" +discord-public-thread-type+))))

(defun thread-delete-plan (thread-id &key (timeout-seconds 30))
  "DELETE /channels/<thread> — a thread is a channel, and the bot that opened
one may remove it: the permission the guild withholds (Manage Threads) is not
asked of a thread's own creator."
  ;; Retried: an empty thread that outlives the silent turn it was opened for
  ;; is litter in the room, and nothing else will clean it up. Ordered before
  ;; the working line's own delete by the caller, because removing the thread
  ;; removes the line inside it.
  (rest-plan "DELETE" (format nil "/channels/~a" thread-id)
             "delete_thread" t timeout-seconds))

(defun thread-close-plan (thread-id &key (timeout-seconds 30))
  "PATCH /channels/<thread> {archived: true}: the thread put away, out of
the channel's open threads, kept and searchable, and open again once anyone
writes in it."
  ;; The bot that opened a thread may archive it, as it may rename it.
  ;; Retried: a close that fails leaves the thread open, and a second close of
  ;; a closed thread changes nothing.
  (rest-plan "PATCH" (format nil "/channels/~a" thread-id) "close_thread" t timeout-seconds
             (nlk:json-object "archived" t)))

;;; --- the bot's own emojis (a card's marks) ------------------------------------
;;; An application owns emojis of its own, which its bot may use in any
;;; server without anyone's Nitro: the card's green check, its spinner and its
;;; cross are three of them, uploaded from emoji/ once and listed thereafter.

(defun application-emojis-plan (application-id &key (timeout-seconds 30))
  "GET /applications/<id>/emojis — the emojis the application owns, as
{\"items\": [...]}."
  (rest-plan "GET" (format nil "/applications/~a/emojis" application-id)
             "list_emojis" t timeout-seconds))

(defun create-application-emoji-plan (application-id name pathname &key (timeout-seconds 30))
  "POST /applications/<id>/emojis — PATHNAME's image as the application's
emoji NAME, a data URI of its bytes."
  ;; Not retried: a create that landed and lost its answer would make a second.
  (rest-plan "POST" (format nil "/applications/~a/emojis" application-id)
             "create_emoji" nil timeout-seconds
             (nlk:json-object "name" name
                              "image" (format nil "data:image/~(~a~);base64,~a" (pathname-type pathname)
                                              (cl-base64:usb8-array-to-base64-string
                                               (nck:read-octets pathname))))))

(defun emoji-spelling (emoji)
  "How a message writes EMOJI, an emoji object: <:name:id>, <a:name:id> when
it moves."
  (format nil "<~:[~;a~]:~a:~a>" (nlk:json-value emoji :boolean "animated")
          (nlk:json-value emoji :string "name") (nlk:json-value emoji :string "id")))

(defun reaction-plans (target message-id emoji &key previous
                                                    (timeout-seconds 30))
  "The bot's one reaction on MESSAGE-ID: PREVIOUS removed by name first —
Discord stacks reactions, so a change is a DELETE then a PUT on the @me
reaction — then EMOJI added, or nothing added for NIL."
  ;; Needs Add Reactions and Read Message History. Retried: the mark holds for
  ;; the whole turn, so its clear must land even through a 429. Discord
  ;; addresses a unicode reaction by its emoji, percent-encoded as UTF-8.
  (flet ((plan (method glyph label)
           (rest-plan method (format nil "/channels/~a/messages/~a/reactions/~a/@me"
                                     (target-rest-channel target) message-id
                                     (quri:url-encode glyph :encoding :utf-8))
                      label t timeout-seconds)))
    (unless (equal previous emoji)
      (append (and previous (list (plan "DELETE" previous "remove_reaction")))
              (and emoji (list (plan "PUT" emoji "add_reaction")))))))

(defun discord-command-name-p (name)
  "Discord's rule for an application command name: 1-32 of lowercase
letters, digits, `-' and `_' (the ASCII slice of its ^[-_\\p{L}\\p{N}]{1,32}$)."
  (and (stringp name) (<= 1 (length name) 32)
       (every (lambda (ch)
                (or (char<= #\a ch #\z) (char<= #\0 ch #\9)
                    (char= ch #\-) (char= ch #\_)))
              name)))

(defun commands-plan (application-id entries &key (timeout-seconds 30))
  "PUT /applications/<id>/commands — the bulk overwrite that makes ENTRIES
((:name :description :usage) plists) the application's global command
menu, every previous command gone."
  ;; A command with a usage takes one optional string option, `args', the
  ;; verbatim tail the interaction carries back — completed on the fly when
  ;; the entry is marked :AUTOCOMPLETE. A name Discord would refuse is left
  ;; out — one bad name fails the whole sync; descriptions are cut to
  ;; Discord's 100 and never empty. Retried: the menu is set once per catalog
  ;; shape.
  (rest-plan "PUT" (format nil "/applications/~a/commands" application-id)
             "set_commands" t timeout-seconds
             (coerce
              (loop for entry in entries
                    for name = (getf entry :name)
                    for usage = (or (getf entry :usage) "")
                    when (discord-command-name-p name)
                      collect (nlk:json-object
                               "name" name
                               "type" 1
                               "description"
                               (nlk:clip (or (nlk:json-value (getf entry :description) :text) name)
                                         100 :ellipsis "")
                               :when (plusp (length usage))
                               "options" (vector
                                          (nlk:json-object
                                           "type" 3
                                           "name" "args"
                                           "description"
                                           (nlk:clip usage 100 :ellipsis "")
                                           "required" nil
                                           :when (getf entry :autocomplete)
                                           "autocomplete" t))))
              'vector)))

(defun interaction-response-plan (candidate text &key private controls panel (timeout-seconds 30))
  "The answer an interaction takes, in two steps: with TEXT NIL, POST
/interactions/<id>/<token>/callback type 5 — the person sees the bot
thinking, and the interaction stays open fifteen minutes instead of three
seconds; PRIVATE flags it ephemeral, so the answer is theirs alone; with
TEXT, PATCH /webhooks/<application>/<token>/messages/@original, the held
response edited into the answer, CONTROLS its components, a PANEL (a
command's card) a message of components in place of TEXT (PANEL-
COMPONENTS), and none without one. A press on a message's choice
(source.pressed) is held with type 6 instead, unless PRIVATE: the answer
then edits the message pressed, in place. NIL for a CANDIDATE that is no
interaction."
  ;; The hold is not retried: a late one is refused as an unknown interaction,
  ;; and a second try would be too. The edit is, like any message edit.
  ;; allowed_mentions parse [] as on every message. After a type 6 hold the
  ;; original IS the message the component sits on — Discord's deferred
  ;; update — so a card answers a press by becoming its next view; a private
  ;; answer to a press (a refusal) is a message of its own, type 5's. A
  ;; message of components stays one from its post on, so words answering a
  ;; press on one are drawn as a panel of those words.
  (let ((id (source-field candidate "interaction_id"))
        (token (source-field candidate "interaction_token"))
        (application (source-field candidate "application_id"))
        (panel (or panel
                   (and text (candidate-pressed-p candidate) (not private)
                        ;; an integer, which SOURCE-FIELD (strings) would not read
                        (let ((flags (gethash "card_flags" (candidate-source candidate))))
                          (and (integerp flags) (logtest +discord-components-v2+ flags)))
                        (list :text text)))))
    (cond ((not (and id token)) nil)
          ((null text)
           ;; Ephemeral is decided here or never: the edit takes the hold's.
           (interaction-callback-plan id token
                                      (if (and (candidate-pressed-p candidate) (not private))
                                          (nlk:json-object "type" 6)
                                          (nlk:json-object "type" 5
                                                           :when private
                                                           "data" (nlk:json-object "flags" 64)))
                                      "interaction_defer" timeout-seconds))
          (application
           (rest-plan "PATCH" (format nil "/webhooks/~a/~a/messages/@original" application token)
                      "interaction_response" t timeout-seconds
                      (if panel
                          (nlk:json-object "flags" +discord-components-v2+
                                           "content" :null
                                           "embeds" #()
                                           "allowed_mentions" (nlk:json-object "parse" #())
                                           "components" (panel-components panel (and (consp controls) controls)))
                          (nlk:json-object "content" text
                                           "embeds" #()
                                           "allowed_mentions" (nlk:json-object "parse" #())
                                           :when controls
                                           "components" (controls-components controls))))))))

(defun interaction-callback-plan (interaction-id token body label timeout-seconds)
  "POST /interactions/<id>/<token>/callback carrying BODY, named LABEL: the one
answer an interaction takes."
  ;; Not retried — a late answer is refused as an unknown interaction, and a
  ;; second try would be too.
  (rest-plan "POST" (format nil "/interactions/~a/~a/callback" interaction-id token)
             label nil timeout-seconds body))

(defun interaction-ack-plans (payload &key text whole (timeout-seconds 30))
  "The one answer a control press takes: POST
/interactions/<id>/<token>/callback, type 6 (deferred update) — the
spinner on the pressed button stops and the message stays as it is; the
card the press touched settles on its own — or, with TEXT, type 4 flagged
ephemeral: TEXT answers the presser alone, a card's Details, clipped to one
message. WHOLE answers every word of TEXT: the answer carries the first
message's worth and each further one is a follow-up, POST
/webhooks/<application>/<token>, ephemeral too."
  ;; PAYLOAD is the kit's normal press plus the interaction token and
  ;; application that return path needs. Not retried: a late answer is
  ;; refused as an unknown interaction and a second try would be too, and a
  ;; follow-up tried again could say its words twice. The follow-ups run
  ;; after the answer, in order (EXECUTE-DELIVERY): Discord takes none before it.
  (let* ((limit (1- +discord-message-content-limit+))
         (messages (cond ((null text) '())
                         (whole (mapcar #'text-chunk-text (split-text-chunks text limit)))
                         (t (list (nlk:clip text limit :ellipsis "…")))))
         (application (getf payload :application-id)))
    (when (and (rest messages) (null application))
      (error "discord: a press's answer past one message needs its interaction's application id"))
    (flet ((private (content)
             (nlk:json-object "content" content
                              "flags" 64
                              "allowed_mentions" (nlk:json-object "parse" #()))))
      (cons (interaction-callback-plan (getf payload :id) (getf payload :token)
                                       (if messages
                                           (nlk:json-object "type" 4 "data" (private (first messages)))
                                           (nlk:json-object "type" 6))
                                       (if text "interaction_private_answer" "interaction_ack")
                                       timeout-seconds)
            (loop for content in (rest messages)
                  collect (rest-plan "POST" (format nil "/webhooks/~a/~a" application (getf payload :token))
                                     "interaction_private_followup" nil timeout-seconds
                                     (private content)))))))

(defun interaction-autocomplete-plans (payload choices &key (timeout-seconds 3))
  "POST /interactions/<id>/<token>/callback — the one answer the autocomplete
request PAYLOAD takes, type 8: at most 25 of CHOICES, (:name :value) plists cut
to Discord's 100-character caps."
  ;; Not retried: a late answer is refused as an unknown interaction, and the
  ;; menu waits three seconds only.
  (list (interaction-callback-plan
         (getf payload :id) (getf payload :token)
         (nlk:json-object
          "type" 8                          ; the autocomplete result: no deferral admitted
          "data" (nlk:json-object
                  "choices"
                  (coerce
                   (loop for choice in (subseq choices 0 (min 25 (length choices)))
                         collect (nlk:json-object
                                  "name" (nlk:clip (or (getf choice :name) "") 100 :ellipsis "")
                                  "value" (nlk:clip (or (getf choice :value) "") 100 :ellipsis "")))
                   'vector)))
         "interaction_autocomplete" timeout-seconds)))

(defun hydrate-self-roles (executor bot-user-id &key (timeout-seconds 30))
  "Ask Discord which roles the bot itself holds and note them as addresses:
for every guild it is in, its own member object there, the role ids it
carries."
  ;; The mention rule reads what this notes — a room where the bot's pill was
  ;; the one picked has addressed it, and without the roles an ask that did
  ;; address the bot is turned away as mention_required. Answers the role ids,
  ;; or :FAILED and a warning when the guild list itself could not be read.
  ;; Blocks: the caller's thread, never a ws callback.
  (let ((guilds (execute-plan executor
                              (rest-plan "GET" "/users/@me/guilds"
                                         "hydrate_guilds" nil timeout-seconds))))
    (if (not (execution-ok-p guilds))
        (progn
          (warn "discord: could not read the bot's guilds (~a); a <@&role> ~
                 mention of the bot will read as mention_required"
                (execution-error guilds))
          :failed)
        (note-self-roles
         (loop for guild across (or (probe-array (execution-body guilds)) #())
               for id = (nlk:json-value guild :string "id")
               when id
                 append (let ((member (execute-plan
                                       executor
                                       (rest-plan "GET"
                                                  (format nil "/guilds/~a/members/~a"
                                                          id bot-user-id)
                                                  "hydrate_self_roles" nil
                                                  timeout-seconds))))
                          (when (execution-ok-p member)
                            (coerce (nlk:json-array (execution-body member) "roles")
                                    'list))))))))

;;; --- what a write names -----------------------------------------------------

(defun read-back-path (path)
  "The GET whose answer shows what a write to PATH changed, or NIL when it
changed nothing a read can show."
  ;; Truncation, not a lookup table: Discord writes a reaction, a pin, a role
  ;; and a permission overwrite at a sub-path of the object that carries them,
  ;; so the object is the prefix.
  ;;
  ;;   /channels/C/messages/M/reactions/...  the message, whose `reactions'
  ;;                                         array says whether it is there —
  ;;                                         and which is ABSENT from the object
  ;;                                         when the message carries none
  ;;   /channels/C/messages/M                the message; a delete makes the
  ;;                                         read a 404, which is the proof
  ;;   /channels/C/pins/M                    the channel's pins
  ;;   /channels/C/permissions/O             the channel and its overwrites
  ;;   /guilds/G/members/U/roles/R           the member and its roles
  ;;   /guilds/G/bans/U                      the ban
  ;;
  ;; Everything else reads nothing. A typing beat and an interaction callback
  ;; leave no object; a POST that answers with what it created — a message, a
  ;; thread, a webhook — is already its own evidence, and NCK:CALL reads back
  ;; only a write that answered empty.
  (let ((segments (remove "" (uiop:split-string (subseq path 0 (position #\? path))
                                                :separator "/")
                          :test #'string=)))
    ;; A shape is how many leading segments name the object, then the path it
    ;; matches: :id a numeric snowflake (`bulk-delete' in a message slot is a
    ;; route, not a message id) and a trailing :rest whatever follows, or nothing.
    (loop for (keep . shape) in '((4 "channels" :id "messages" :id)
                                  (4 "channels" :id "messages" :id "reactions" :rest)
                                  (3 "channels" :id "pins" :id)
                                  (2 "channels" :id "permissions" :id)
                                  (4 "guilds" :id "members" :id "roles" :id)
                                  (4 "guilds" :id "bans" :id))
          when (and (if (eq :rest (car (last shape)))
                        (>= (length segments) (1- (length shape)))
                        (= (length segments) (length shape)))
                    (every (lambda (want segment)
                             (case want
                               (:id (every #'digit-char-p segment))
                               (:rest t)
                               (t (equal want segment))))
                           shape segments))
            return (format nil "~{/~a~}" (subseq segments 0 keep)))))

(defun discord-failure-message (plan status detail)
  (format nil "Discord REST ~a ~a ~?"
          (request-plan-method plan) (request-plan-audit-label plan)
          (case status
            (403 "forbidden; verify the bot's guild membership, channel ~
                  permissions, and privileged intents (status ~a: ~a)")
            (429 "rate limited; retry later (status ~a: ~a)")
            (t "failed with status ~a: ~a"))
          (list status detail)))

(defun discord-retry-after-ms (status body headers)
  "Discord's 429 pacing rides the body as {\"retry_after\": seconds}, a
float."
  ;; The retry-after header carries the same number rounded, and the kit's
  ;; header parse truncates it to an integer — 0.75s reads as 0 — so the body
  ;; wins when present.
  (declare (ignore status headers))
  (let ((seconds (and (hash-table-p body) (gethash "retry_after" body))))
    (and (realp seconds) (round (* (max 0 seconds) 1000)))))

(defun make-discord-executor (&key (api-base +discord-rest-api-base+) token)
  (make-dexador-executor
   :base-url api-base
   :headers (list (cons "authorization" (format nil "Bot ~a" token))
                  (cons "user-agent" "Nodecode Discord Channel Lane"))
   :redact-prefixes (list token)
   :failure-message-fn #'discord-failure-message
   :retry-after-fn #'discord-retry-after-ms))
