;;;; admission.lisp --- pure inbound admission policy.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Port of the Zig-era channel kit's decideInboundPolicy
;;;; (extensions/sdk/src/channels/admission.ts) narrowed to the semantics the
;;;; two live packs use. Pure: policy struct + normalized candidate + the
;;;; platform's other-addressee table in, (values ACCEPTED-P REASON) out.
;;;; Rejection reasons are required by construction — a policy that can
;;;; reject must say why.
;;;;
;;;; The candidate is the shared ingress shape both adapters normalize into:
;;;; a hash-table {"text": ..., "source": {"platform", "chat_kind",
;;;; "channel_id", "parent_channel_id", "thread_id", "user_id",
;;;; "workspace_id", "message_id", "is_bot", "addressed", ...}}. chat_kind
;;;; is one of "direct_message" | "group" | "channel" | "thread". addressed
;;;; is the adapter's word that the platform itself routed the message to
;;;; the bot — a Discord slash interaction, a reply to one of the bot's own
;;;; messages — so the mention gate has nothing left to ask. joined is its
;;;; word that a person typed the message in a thread the bot already takes
;;;; part in (MARK-JOINED-THREAD, host.lisp): the thread is a conversation the
;;;; bot is in, so it speaks freely there.
;;;;
;;;; Allowlist semantics (idAllowed): an empty list restricts nothing, a "*"
;;;; member allows everything, otherwise membership. The fail-closed floor is
;;;; enforced at config time (REQUIRE-NON-EMPTY-ALLOWLIST), not here. Who may
;;;; talk is three grants in one gate (USER-ALLOWED-P): allowed_users, a role
;;;; allowed_roles names, and a pairing the operator approved.

(in-package #:nodecode-channel-kit)

(nlk:define-record (inbound-policy (:copier nil) (:export :constructor))
  "One normalized allow/deny policy for a channel lane."
  ;; Scope = Discord guild. Empty list restricts nothing.
  (allowed-scopes '() :type list)
  ;; Primary channel/chat allowlist. :channel-match :discord also matches the
  ;; candidate's parent_channel_id and thread_id, so allowlisting a parent
  ;; channel admits its threads.
  (allowed-channels '() :type list)
  (allowed-thread-ids '() :type list)
  (allowed-users '() :type list)
  ;; Role ids (Discord): an author holding any one is allowed as a listed
  ;; user is. The adapter puts the author's roles on source.role_ids.
  (allowed-roles '() :type list)
  (dm-policy :enabled :type keyword)          ; :enabled | :disabled
  (group-policy :enabled :type keyword)       ; :enabled | :disabled
  (thread-policy :enabled :type keyword)      ; forum topics: :enabled | :disabled
  (require-mention t :type boolean)
  ;; Explicit speaking overrides. Adapters may match parent and thread ids
  ;; when CHANNEL-MATCH is :DISCORD.
  (free-response-channels '() :type list)
  (require-mention-channels '() :type list)
  (ignored-channels '() :type list)
  ;; The id/username a mention must reference, and the matcher over
  ;; (TEXT TARGET). NIL target with REQUIRE-MENTION is itself a rejection.
  (mention-target nil :type (or null string))
  (mention-test nil :type (or null function))
  ;; Another bot's message: :none rejects it, :mentions takes one that
  ;; mentions this bot and rejects the rest, :all admits it as a person's.
  (allow-bots :none :type (member :none :mentions :all))
  ;; Own user id: a message the bot wrote itself always rejects, whatever
  ;; ALLOW-BOTS says, or its own answer in a thread it speaks freely in
  ;; would be its next ask.
  (self-id nil :type (or null string))
  (channel-match :exact :type keyword)        ; :exact | :discord
  ;; keyword -> wire reason string; missing keys fall back to the key's name
  ;; underscored, so a policy lists only the reasons it spells differently.
  (reasons '() :type list))

(nlk:access (policy inbound-policy))

(defun candidate-source (candidate)
  (or (nlk:json-value candidate :object "source") (make-hash-table :test #'equal)))

(defun source-field (candidate key &aux (value (gethash key (candidate-source candidate))))
  "A string field off the candidate's source, NIL for absent/null."
  (and (stringp value) (plusp (length value)) value))

(defun handle-tag (candidate)
  "The ids the adapter knows about a message, as \" [m<message> u<user>]\",
or \" [m<message> u<user> r<replied-to>]\" when it answers another, or
\"\" when it knows none."
  ;; A line that is a reply names the message it answers as r<id> — the
  ;; message a "this" in the ask means. These are the handles the model needs
  ;; to reach a message over the platform API — react to it, reply to it,
  ;; fetch it, pin it — and they ride every line it reads, ambient chatter
  ;; included, so anything it has seen is addressable. Adapter-computed,
  ;; before the colon: a participant who types a bracket puts it after the
  ;; colon, in their own text.
  (format nil "~@[ [~{~a~^ ~}]~]"
          (loop for (prefix key) in '(("m" "message_id") ("u" "user_id")
                                      ("r" "reply_to_message_id"))
                for value = (source-field candidate key)
                when value collect (concatenate 'string prefix value))))

(defun candidate-bot-p (candidate &aux (is-bot (gethash "is_bot" (candidate-source candidate))))
  "Whether CANDIDATE's author is a bot, by the adapter's word."
  (and is-bot (not (eq is-bot :null))))

(defun candidate-joined-p (candidate)
  "Whether the adapter marked CANDIDATE as typed in a thread the bot already
takes part in (source.joined true)."
  (eq t (gethash "joined" (candidate-source candidate))))

(defun candidate-addressed-p (candidate)
  "Whether the adapter marked CANDIDATE as routed to the bot by the platform
itself (source.addressed true): a slash interaction, a reply to the bot's
own message. Such a message needs no mention to be an ask."
  (eq t (gethash "addressed" (candidate-source candidate))))

(defun candidate-pressed-p (candidate)
  "Whether CANDIDATE is a press on a choice a message carries (source.pressed
true): its answer, unless private, takes the place of the message pressed."
  (eq t (gethash "pressed" (candidate-source candidate))))

(defun leading-addressee (text)
  "The lowercase name TEXT opens with, or NIL: leading whitespace, an
optional @, then a word up to a , : ; or - separator within forty
characters."
  ;; "ada, you need to do something" names "ada"; a sentence that never
  ;; opens with a name names nobody — matching it against a list is the
  ;; caller's business.
  (ppcre:register-groups-bind (name) ("^[ \\t\\n\\r]*@?([^,:;-]{0,39})[,:;-]" (or text ""))
    (let ((name (string-trim '(#\Space #\Tab) (string-downcase name))))
      (and (plusp (length name)) name))))

(defun leading-mention-id (text)
  "The user id TEXT opens by entity-mentioning, <@id> or <@!id>, or NIL."
  ;; A leading role mention (<@&id>) names no user and answers NIL: a role
  ;; pill is a broadcast, not an addressee.
  (ppcre:register-groups-bind (id) ("^[ \\t\\n\\r]*<@!?(\\d+)>" (or text "")) id))

;;; A message that opens by naming one of these — and carries nothing that
;;; involves us — is that participant's: observed, never answered. An adapter
;;; sets its own from config at start through SET-OTHER-ADDRESSEES, so a
;;; config change lands on a running room without a relaunch.
(defvar *other-addressees* (make-hash-table :test #'equal)
  "Platform id -> names, other than the bot's, that a room may address.")

(defun set-other-addressees (platform names)
  "Record NAMES as the other addressees of PLATFORM's rooms: each a string a
message may open by naming, matched case-insensitively. Empty clears."
  (setf (gethash platform *other-addressees*)
        (mapcar #'string-downcase (remove-if-not #'stringp names))))

(defun addressed-elsewhere-p (policy candidate text)
  "Whether TEXT opens by addressing someone who is not the bot: a leading
entity mention of another user, or a leading name among the platform's
other addressees."
  ;; Says nothing about a message that never opens with an address, which is
  ;; most of what a room says.
  (nlk:if-let (id (leading-mention-id text))
    (let ((target policy.mention-target))
      (and target (not (equal id target))))
    (let ((name (leading-addressee text)))
      (and name
           (member name (gethash (source-field candidate "platform")
                                 *other-addressees*)
                   :test #'string-equal)
           t))))

(defun candidate-attachments (candidate)
  "The files the adapter normalized onto CANDIDATE — or onto the reply it
carries, the message it answers (CANDIDATE-REPLY) — in the order they
arrived: one hash table per attachment, carrying the \"url\" to fetch, the
platform's \"id\" for the file, the \"media_type\", \"filename\" and
\"size\" the platform gave it, and for a recording the \"seconds\" it
declared."
  ;; Empty for a candidate that carries none — the shape admits attachments
  ;; for every platform, and this is the one read of them on the ingress. An
  ;; entry carries the "url" to fetch, or — a platform whose files are reached
  ;; with the adapter's own credential — the "fetch" thunk that returns the
  ;; bytes; an entry with neither carries nothing anything can fetch. Nothing
  ;; here is believed to be what it declares: the fetch sniffs the bytes and
  ;; decides.
  (loop for attachment across (or (nlk:json-value candidate :array "attachments") #())
        when (and (hash-table-p attachment)
                  (or (gethash "url" attachment)
                      (functionp (gethash "fetch" attachment))))
          collect attachment))

(defparameter +document-extensions+
  '("txt" "text" "md" "markdown" "rst" "org" "tex" "log" "csv" "tsv" "json" "jsonl"
    "ndjson" "yaml" "yml" "toml" "ini" "cfg" "conf" "env" "xml" "html" "htm" "css"
    "js" "mjs" "cjs" "ts" "tsx" "jsx" "py" "rb" "go" "rs" "java" "kt" "swift" "c"
    "h" "cc" "cpp" "hpp" "cs" "php" "lua" "lisp" "lsp" "asd" "el" "clj" "scm" "zig"
    "sh" "bash" "zsh" "fish" "ps1" "sql" "diff" "patch" "srt" "vtt")
  "Extensions a file is read as text by when its platform declares no text type:
Discord leaves content_type off many source files.")

(defun document-media-type-p (media-type filename)
  "Whether a file a platform declares as MEDIA-TYPE and names FILENAME reads
as text: a text/ type, a structured-text application type, or a name whose
extension is a text format."
  (or (and (stringp media-type)
           (ppcre:scan "(?i)^(?:text/|application/(?:json|x-ndjson|ld\\+json|xml|x-yaml|yaml|toml|javascript|x-sh|x-shellscript|sql|x-python|csv)\\b)"
                       media-type)
           t)
      (and (stringp filename)
           (nlk:when-let (dot (position #\. filename :from-end t))
             (and (member (string-downcase (subseq filename (1+ dot))) +document-extensions+
                          :test #'string=)
                  t)))))

(defun document-attachment-p (attachment &aux (type (gethash "media_type" attachment)))
  "Whether ATTACHMENT declares a text file — by its type, or by its name when
the type says nothing about it."
  (and (not (image-attachment-p attachment))
       (not (recording-attachment-p attachment))
       (document-media-type-p type (gethash "filename" attachment))))

(defun image-attachment-p (attachment &aux (type (gethash "media_type" attachment)))
  "Whether ATTACHMENT declares an image — the line's words for a message
that carries one and says nothing."
  (and (stringp type) (uiop:string-prefix-p "image/" (string-downcase type))))

(defun recording-attachment-p (attachment &aux (type (gethash "media_type" attachment)))
  "Whether ATTACHMENT declares a recording — audio, or a video that declares
its length the way a voice note does (Telegram's round video note) — which
rides the room's chatter to the next ask where an image does not."
  (and (stringp type)
       (or (ppcre:scan "(?i)^audio/" type)
           (and (ppcre:scan "(?i)^video/" type) (realp (gethash "seconds" attachment))))
       t))

(defun video-file-p (attachment &aux (type (gethash "media_type" attachment)))
  "Whether ATTACHMENT declares a video that is no recording: a file to open,
not words to hear, whatever its container holds."
  (and (stringp type) (ppcre:scan "(?i)^video/" type)
       (not (recording-attachment-p attachment))
       t))

;;; The users the operator let in by pairing (host.lisp, PAIR-USER), by
;;; platform id: read from the store at a host's start and changed as the
;;; operator pairs and unpairs, so the gate reads it live.
(defvar *paired-users* (make-hash-table :test #'equal :synchronized t)
  "Platform id -> the user ids the operator paired.")

(defun candidate-role-ids (candidate &aux (roles (gethash "role_ids" (candidate-source candidate))))
  "The role ids the adapter says CANDIDATE's author holds where it spoke."
  (and (or (listp roles) (vectorp roles)) (remove-if-not #'stringp (coerce roles 'list))))

(defun user-allowed-p (policy candidate &aux (user (source-field candidate "user_id"))
                                             (users policy.allowed-users)
                                             (roles policy.allowed-roles))
  "Whether CANDIDATE's author may drive the bot under POLICY: anyone when
neither allowed_users nor allowed_roles restricts, else a listed user, one
holding a listed role, or one the operator paired."
  (or (and (null users) (null roles))
      (and (member "*" users :test #'equal) t)
      (and user (member user users :test #'equal) t)
      (and (intersection roles (candidate-role-ids candidate) :test #'equal) t)
      (and user
           (member user (gethash (source-field candidate "platform") *paired-users*)
                   :test #'equal)
           t)))

(defun id-allowed-p (allowed value)
  (or (null allowed)
      (member "*" allowed :test #'equal)
      (and value (member value allowed :test #'equal) t)))

(defun channel-listed-p (policy candidate channels)
  "Whether CANDIDATE matches one of CHANNELS; an empty list matches none."
  (let ((channel (source-field candidate "channel_id"))
        (parent (source-field candidate "parent_channel_id"))
        (thread (source-field candidate "thread_id")))
    (or (member "*" channels :test #'equal)
        ;; An entry names the active channel or, for Discord, its parent or thread.
        (loop for entry in channels
              thereis (or (equal entry channel)
                          (and (eq policy.channel-match :discord)
                               (or (equal entry parent)
                                   (equal entry thread))))))))

(defun room-speaks-freely-p (policy candidate)
  "Whether CANDIDATE's room reaches the bot without a mention under POLICY:
a direct message, a thread the bot takes part in, a channel that speaks
freely, or a policy that takes no mention for this channel."
  ;; The one fact the mention gate turns on and the lane contract states:
  ;; DECIDE-INBOUND observes an unmentioned line only where this is NIL, so a
  ;; lane never reads a line the room did not mean for it, and the contract
  ;; tells the lane which room it is in from the same answer — never a mark on
  ;; the line. A host with no policy — a bare test host, a synthetic ask — has
  ;; no mention rule to gate on: its room speaks freely.
  (or (not (typep policy 'inbound-policy))
      (equal "direct_message" (source-field candidate "chat_kind"))
      (candidate-joined-p candidate)
      (channel-listed-p policy candidate policy.free-response-channels)
      (not (or (channel-listed-p policy candidate
                                 policy.require-mention-channels)
               policy.require-mention))))

(defun inbound-reason (policy key)
  "The wire reason POLICY gives a rejection or an observation KEY: its own
spelling, else the key's name underscored (:user-not-allowed is
\"user_not_allowed\")."
  (or (getf policy.reasons key)
      (substitute #\_ #\- (string-downcase (symbol-name key)))))

(defun decide-inbound (policy candidate)
  "(values ACTION REASON): evaluate POLICY against one normalized candidate."
  ;; ACTION is one of
  ;;
  ;;   :answer   — every gate passes; the candidate is addressed to us.
  ;;   :observe  — every gate passes EXCEPT the mention requirement, or the
  ;;               message opens by addressing another participant. The message
  ;;               belongs to a room we are allowed to read but is not addressed
  ;;               to us: recordable context, not a prompt.
  ;;   :reject   — a scope, channel, user, author, or chat-kind gate refused it.
  ;;               Nothing about this message may be recorded.
  ;;
  ;; REASON is the wire reason string for :reject and :observe, NIL for
  ;; :answer. The two decisions are separate on purpose — "may I read this
  ;; room" and "was this said to me" answer different questions, and an
  ;; adapter that keeps a shared record of the room needs both.
  (let ((chat-kind (source-field candidate "chat_kind"))
        (self policy.self-id)
        (channels policy.allowed-channels)
        (text (and (hash-table-p candidate) (gethash "text" candidate))))
    (labels ((decide (action key)
               (return-from decide-inbound (values action (inbound-reason policy key))))
             (reject (key) (decide :reject key))
             (observe (key) (decide :observe key)))
      (when (and self (equal self (source-field candidate "user_id")))
        (reject :self-author))
      (when (and (candidate-bot-p candidate) (eq policy.allow-bots :none))
        (reject :bot-author))
      (unless (id-allowed-p policy.allowed-scopes (source-field candidate "workspace_id"))
        (reject :scope-not-allowed))
      (unless (or (null channels) (channel-listed-p policy candidate channels))
        (reject :channel-not-allowed))
      (unless (user-allowed-p policy candidate)
        (reject :user-not-allowed))
      (when (and (equal chat-kind "thread")
                 (eq policy.thread-policy :disabled))
        (reject :thread-not-supported))
      (unless (id-allowed-p policy.allowed-thread-ids (source-field candidate "thread_id"))
        (reject :thread-not-allowed))
      (when (and (equal chat-kind "direct_message") (eq policy.dm-policy :disabled))
        (reject :dm-disabled))
      (when (and (equal chat-kind "group")
                 (eq policy.group-policy :disabled))
        (reject :group-disabled))
      ;; The speaking mode: ignored wins overlaps, then free response, then mention
      ;; required; an unlisted channel uses the global REQUIRE-MENTION default.
      (when (channel-listed-p policy candidate policy.ignored-channels) (reject :channel-ignored))
      ;; A bot let in by mention only: two bots that each speak freely in a
      ;; thread would otherwise answer each other for as long as they run.
      (when (and (candidate-bot-p candidate) (eq policy.allow-bots :mentions)
                 (not (mention-involves-p policy (if (stringp text) text ""))))
        (reject :bot-not-mentioned))
      (when (and (not (room-speaks-freely-p policy candidate))
                 (not (candidate-addressed-p candidate)))
        ;; A missing mention target is a configuration failure, not a quiet
        ;; room message: it must stay loud, so it rejects rather than
        ;; degrading into silent observation of everything.
        (unless policy.mention-target
          (reject :mention-required-without-user))
        (unless (mention-involves-p policy (if (stringp text) text ""))
          (observe :mention-required)))
      ;; Someone else's address: a message that opens by naming another
      ;; participant, and carries nothing that involves us, belongs to that
      ;; participant — the room may be free to speak and this message still
      ;; was not addressed to us. The gate above asks whether the room may
      ;; talk to us; this asks whether it did.
      (when (and (not (candidate-addressed-p candidate))
                 (not (equal chat-kind "direct_message"))
                 (not (mention-involves-p policy (if (stringp text) text "")))
                 (addressed-elsewhere-p policy candidate text))
        (observe :addressed-elsewhere))
      (values :answer nil))))

;;; --- the two live mention matchers ---------------------------------------

(defun discord-mention-p (text user-id &optional role-ids)
  "Discord mentions are entity references, not names: <@id> or <@!id> for a
user, <@&id> for a role."
  ;; ROLE-IDS are the roles the bot itself holds — Discord gives every bot a
  ;; managed role carrying its own name, so the room's picker offers that pill
  ;; beside the user pill and either one addresses it.
  (and (stringp user-id) (plusp (length (string-trim " " user-id)))
       (or (search (format nil "<@~a>" user-id) text)
           (search (format nil "<@!~a>" user-id) text)
           (some (lambda (role)
                   (and (stringp role)
                        (plusp (length role))
                        (search (format nil "<@&~a>" role) text)))
                 role-ids))
       t))

(defun telegram-mention-p (text username)
  "Telegram mentions are case-insensitive @handle substrings."
  (let ((handle (if (and (plusp (length username))
                         (char= #\@ (char username 0)))
                    username
                    (concatenate 'string "@" username))))
    (and (search (string-downcase handle) (string-downcase text)) t)))

(defun mention-involves-p (policy text &aux (target policy.mention-target)
                                            (test policy.mention-test))
  "Whether TEXT addresses the bot under POLICY's mention rule — the entity
mention, the wake word, whatever the section configured."
  ;; The one answer to `was this said to me': admission asks it of every
  ;; message it reads, and the edit path asks it of every edit it sees, so the
  ;; two can never disagree about what counts as being addressed. NIL without
  ;; a mention target: a room that cannot say what would address it involves
  ;; nobody.
  (and target test (funcall test text target) t))
