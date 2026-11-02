;;;; identity.lisp --- kernel-owned session aggregate.
;;;;
;;;; Create is a command. The durable fact is SESSION.CREATED. Interning a
;;;; heap session is a cache of that fact, not a way to mint an identity.
;;;; ADMIT-TURN refuses a session-id that has no created fact.

(in-package #:nodecode.kernel)

(access (event event))

(define-condition session-identity-error (error) ())

(define-errors (session-identity-error) ((:export))
  (unknown-session (session-id) "session ~a has not been created" session-id)
  (duplicate-session (session-id) "session ~a already exists" session-id)
  (unknown-parent-session (session-id) "parent session ~a has not been created" session-id)
  (invalid-session-parent (session-id) "session ~a cannot be its own parent" session-id)
  (invalid-session-anchor (session-id anchor)
   "anchor turn ~a is not a turn of session ~a's parent" anchor session-id)
  (conflicting-session-edge (session-id)
   "session ~a cannot be created detached from a parent it also anchors: a detached edge composes nothing of that parent, so there is no turn of it to fork at (leave the anchor turn out)"
   session-id)
  (standby-block-refused (session-id kind)
   "~a landed on standby session ~a before its created fact: its writer materializes the identity first (MATERIALIZE-STANDBY-SESSION) inside the writing transaction)"
   kind session-id))

(define-record (durable-session (:constructor %make-durable-session) (:export :readers))
  (id (error "id required") :type string :read-only t)
  (cwd (error "cwd required") :type string :read-only t)
  (parent nil :type (or null string) :read-only t)
  (created-at (error "created-at required") :type string :read-only t)
  ;; Fork anchor: the parent turn this session branched from, and the
  ;; parent's log watermark frozen at fork time. NIL on non-forked sessions.
  (anchor-turn-id nil :type (or null string) :read-only t)
  (anchor-log-position nil :type (or null log-position) :read-only t))

(access (session durable-session))

(define-event session-created +kind-session-created+
  "Admission fact for a session aggregate."
  ;; Schema-version 2 adds the fork anchor pair; version 1 rows read as
  ;; unanchored.
  ((cwd :string "cwd" :default "")
   (parent :text "parent")
   (anchor-turn-id :text "anchor-turn-id")
   (anchor-log-position :integer "anchor-log-position")))

(define-event session-moved +kind-session-moved+
  "A session's folder from here on, and the one it had: MOVE-SESSIONS writes it."
  ((cwd :string "cwd" :default "")
   (from :string "from" :default "")))

(defun session-exists-p (session-id)
  "True when SESSION-ID is a session this image answers for: the open store
has its SESSION.CREATED fact, or it was minted and is still standing by."
  (check-type session-id session-id)
  (and (store-open-p)
       (or (session-created-fact-p session-id)
           (on-standby-p session-id))))

(defun resolve-session-cwd (cwd)
  "CWD as a created fact carries it: an absolute directory string."
  ;; NIL, the empty string and "." all mean this process's working directory,
  ;; a leading ~ is the home it runs under, and a `.' or `..' step is read as
  ;; `cd' reads it, by the text -- so `../webshop' typed is the folder beside
  ;; this one, never `proj/../webshop' (a drive's path, on Windows, is left
  ;; as it is). The directory's existence is never required here
  ;; (CHECK-SESSION-CWD asks).
  (let* ((raw (if (stringp cwd)
                  (trimmed cwd)
                  ""))
         (base (uiop:getcwd))
         (path (namestring
                (uiop:ensure-absolute-pathname
                 (cond ((or (string= raw "") (string= raw ".")) base)
                       ((string= raw "~") (user-homedir-pathname))
                       ((uiop:string-prefix-p "~/" raw) (merge-pathnames (subseq raw 2) (user-homedir-pathname)))
                       ((uiop:absolute-pathname-p raw) (pathname raw))
                       (t (merge-pathnames raw base)))
                 base))))
    (if (uiop:string-prefix-p "/" path)
        (let ((steps '()))
          (dolist (step (uiop:split-string path :separator "/"))
            (cond ((member step '("" ".") :test #'string=))
                  ((string= step "..") (pop steps))
                  (t (push step steps))))
          (format nil "/~{~a~^/~}~:[~;/~]" (reverse steps) (and steps (uiop:string-suffix-p path "/"))))
        path)))

(define-error missing-session-folder (session-identity-error) (path kind)
  (:export)
  ;; PATH as RESOLVE-SESSION-CWD spells it; the words drop its closing slash.
  (:report (lambda (condition stream)
             (format stream (ecase (missing-session-folder-kind condition)
                              (:absent "there is no folder at ~a")
                              (:not-folder "~a is a file, not a folder")
                              (:unreachable "~a cannot be reached"))
                     (folder-words (missing-session-folder-path condition))))))

(defun folder-words (path &aux (native (native-path path)))
  "PATH, a directory string, as a sentence names it: no closing slash but the root's."
  (if (string= native "") "/" native))

(defun check-session-cwd (cwd &aux (resolved (resolve-session-cwd cwd)))
  "CWD resolved as RESOLVE-SESSION-CWD resolves it => the absolute directory
string, refused (MISSING-SESSION-FOLDER) when no folder is there to work in."
  ;; Asked where an operator names the folder a new session opens in: a
  ;; session in a folder that is not there fails its every sh call on the
  ;; spawn, and a typo there became the page's next default. The log itself
  ;; never requires it -- an imported session keeps another machine's folder.
  (flet ((refuse (kind)
           (error 'missing-session-folder :path resolved :kind kind)))
    (handler-case
        (unless (= sb-posix:s-ifdir (logand sb-posix:s-ifmt (sb-posix:stat-mode (sb-posix:stat (folder-words resolved)))))
          (refuse :not-folder))
      (sb-posix:syscall-error (condition)
        (refuse (if (member (sb-posix:syscall-errno condition) (list sb-posix:enoent sb-posix:enotdir))
                    :absent
                    :unreachable)))))
  resolved)

(defun check-session-lineage (id parent anchor-turn-id)
  "Refuse a lineage no session can be minted or created with: an empty ID, a
PARENT that is the empty string or ID itself, and an ANCHOR-TURN-ID with no
parent to be a turn of."
  ;; ID names the refused session where it is already known.
  (when id
    (check-type id session-id)
    (when (string= id "")
      (error 'type-error :datum id :expected-type '(string 1))))
  (when parent
    (check-type parent string)
    (when (string= parent "")
      (error 'unknown-parent-session :session-id parent))
    (when (equal parent id)
      (error 'invalid-session-parent :session-id id)))
  (when anchor-turn-id
    (check-type anchor-turn-id turn-id)
    (unless parent
      (error 'invalid-session-anchor :session-id (or id "")
             :anchor anchor-turn-id))))

(defun standby-session (&key id cwd parent anchor-turn-id)
  "Hold a session id standing by: minted now, durable when its first block
lands (standby.lisp, MATERIALIZE-STANDBY-SESSION)."
  ;; Nothing durable names it meanwhile — no SESSION.CREATED, no configuration
  ;; fact, no transcript row — so an image that ends with the id still
  ;; standing by leaves no session behind, and the directory never lists one.
  ;;
  ;; CWD resolves exactly as CREATE-SESSION resolves it, and unstated beside a
  ;; PARENT it is the parent's: a fork works where the session it came off
  ;; works, never where the gateway process happens to run. PARENT is the session
  ;; this one composes and ANCHOR-TURN-ID the turn of the parent's tree it
  ;; forks at; unstated, a durable parent's CURRENT head is the anchor, read
  ;; here, because a mint is what freezes a fork and not the later
  ;; materialization. A parent still standing by has no turns of its own, so
  ;; the anchor is the one that parent was minted with, read up the chain, and
  ;; the child's materialization brings the parent into the log first.
  ;;
  ;; A session that is already durable is returned untouched. => the id.
  (check-session-lineage id parent anchor-turn-id)
  (let* ((session-id (or id (make-durable-id "session")))
         (open (store-open-p))
         (durable-parent (and parent open (session-created-fact-p parent))))
    ;; A parent has to be a session this image holds — durable, or standing by
    ;; ahead of the child — and an anchor the caller STATED has to be a turn
    ;; of that parent's tree, which a fork standing by composes from its mint
    ;; (SESSION-FORK-EDGE). The anchors this function derives itself (the
    ;; chain read below) are the parent's own head and need no such check.
    (when (and open parent (not (or durable-parent (on-standby-p parent))))
      (error 'unknown-parent-session :session-id parent))
    (when (and open anchor-turn-id parent
               (not (session-known-turn-p parent anchor-turn-id)))
      (error 'invalid-session-anchor :session-id session-id
             :anchor anchor-turn-id))
    (unless (and open (session-created-fact-p session-id))
      ;; The anchor read here is what freezes the fork: the head a durable
      ;; parent has now, or — for a parent still standing by — the anchor
      ;; that parent itself was minted with, resolved up the chain. The
      ;; watermark is frozen only when the parent is already in the log;
      ;; a standby parent's log is born at materialization, so its child's
      ;; watermark is read then, against the fact the parent got there.
      (let ((anchor (or anchor-turn-id
                        (and parent open
                             (or (and durable-parent (session-head-turn-id parent))
                                 (standby-session-head-turn-id parent))))))
        (register-standby-session
         session-id
         ;; Resolved here as CREATE-SESSION resolves it, so the fact
         ;; materialized later carries a directory, never an empty string.
         :cwd (cond (cwd (resolve-session-cwd cwd))
                    (durable-parent (durable-session-cwd (find-session parent)))
                    ((and parent (standby-session-record parent))
                     (standby-record-cwd (standby-session-record parent)))
                    (t (resolve-session-cwd nil)))
         :parent parent :anchor-turn-id anchor
         :anchor-log-position (and anchor durable-parent (session-log-watermark parent)))))
    session-id))

(defun find-session (session-id)
  "Return the durable session for SESSION-ID, or NIL."
  ;; Its folder is the newest SESSION.MOVED's, else the created fact's: a
  ;; session follows its folder when that moves (MOVE-SESSIONS).
  (check-type session-id session-id)
  (when-let (event (and (store-open-p)
                        (events :session-id session-id :kind +kind-session-created+
                                :as :instance)))
    (let ((moved (events :session-id session-id :kind +kind-session-moved+
                         :order :newest :limit 1 :as :instance)))
      (%make-durable-session
       :id event.session-id
       :cwd (if moved (session-moved-cwd moved) (session-created-cwd event))
       :parent (session-created-parent event)
       :created-at event.occurred-at
       :anchor-turn-id (session-created-anchor-turn-id event)
       :anchor-log-position (session-created-anchor-log-position event)))))

(defun move-sessions (from to &key sessions)
  "Move every session whose folder is FROM, or a folder inside it, to the same
place under TO, one SESSION.MOVED each => ((SESSION-ID OLD NEW) ...) in the
order the sessions were created. SESSIONS, a list of ids, moves only those."
  ;; A session's folder is a fact of its log, so a folder renamed or archived
  ;; on disk strands every session recorded there -- each sh call fails on
  ;; the spawn and every relative path misses -- and a room meant to work in
  ;; another checkout keeps its old one. This is the move, for both. TO, and
  ;; every folder under it a session lands in, must be there, or nothing is
  ;; written; FROM need not be (the usual call follows a rename). A child
  ;; created after the move inherits the folder its parent has now.
  (let ((from (string-right-trim "/" (resolve-session-cwd from)))
        (to (string-right-trim "/" (check-session-cwd to))))
    (flet ((place (cwd &aux (folder (string-right-trim "/" cwd)))
             ;; FOLDER's place under TO when it is FROM or inside it, refused
             ;; when no folder is there
             (when (or (string= folder from)
                       (uiop:string-prefix-p (concatenate 'string from "/") folder))
               (let ((place (concatenate 'string to (subseq folder (length from)))))
                 (check-session-cwd (if (string= place "") "/" place))))))
      (if (string= from to)
          '()
          (with-store-transaction (handle)
            (let ((moves (loop for (id cwd) in (sql-rows handle (format nil "select d.session_id, d.cwd
                                                                    from session_directory d
                                                                    join events e on e.session_id = d.session_id
                                                                     and e.kind = '~a'
                                                                   order by e.log_position"
                                                                  +kind-session-created+))
                               for place = (and (or (null sessions)
                                                    (member id sessions :test #'string=))
                                                (place cwd))
                               when place
                                 collect (list id cwd place))))
              (with-event-owner 'move-sessions
                (loop for (id cwd place) in moves
                      do (append-event-row handle id +kind-session-moved+
                                           (list :cwd place :from cwd))))
              moves))))))

(defun session-fork-edge (session-id)
  "SESSION-ID's edge to the session it forks => (PARENT ANCHOR-TURN-ID
ANCHOR-LOG-POSITION), or NIL: the one its SESSION.CREATED froze or, until its
first block makes it durable, the one its mint did (STANDBY-SESSION)."
  ;; The lineage readers stand on this, so a fork composes its ancestors from
  ;; the moment it is minted: what a shell attached to it is replayed, and the
  ;; turns a fork of it may anchor at, do not wait for its first prompt.
  (if-let (session (find-session session-id))
    (list session.parent session.anchor-turn-id session.anchor-log-position)
    (when-let (record (standby-session-record session-id))
      (list record.parent record.anchor-turn-id record.anchor-log-position))))

;;; --- the composed turn tree -------------------------------------------------
;;; A session composes the sessions it forked from by reference, so a turn
;;; its tree holds may live in any of them. SESSION-LINEAGE (checkpoint.lisp)
;;; walks the same link; this answers the membership question with one
;;; indexed probe per anchored ancestor instead of the whole composed map.

(defun session-known-turn-p (session-id turn-id)
  "True when TURN-ID has a TURN.STARTED in SESSION-ID's composed tree: its
own log, or that of any session it composes by reference."
  ;; A fork link with no anchor is provenance only and contributes nothing,
  ;; exactly as the lineage walk treats it.
  (check-strings session-id turn-id)
  (loop for id in (session-standing-chain session-id)
        thereis (events :session-id id :turn-id turn-id :kind +kind-turn-started+ :as :exists)))

(defun create-session (&key id cwd parent command-id anchor-turn-id occurred-at
                          detached)
  "Record SESSION.CREATED."
  ;; ID is generated when omitted. CWD becomes absolute. PARENT, when
  ;; supplied, must already exist and must not be ID. ANCHOR-TURN-ID forks the
  ;; session from that turn — one of PARENT's own, or of a session PARENT
  ;; composes by reference, which is what a parent that is itself a fresh fork
  ;; holds: the child's history composes the parent chain through the anchor
  ;; by reference, frozen at the parent's log watermark — a parent rewind
  ;; after the fork cannot change the child. DETACHED records the edge to
  ;; PARENT as provenance alone: the child hangs under that session and
  ;; composes none of its history, so a session spawned by another — a team
  ;; seat — begins fresh while every surface still reads it as that
  ;; session's node. The two are the only edges a session has: an anchor, or
  ;; none. OCCURRED-AT, when stated (UTC ISO-8601), is when the session was
  ;; created — a session recorded from another log keeps its own birth time
  ;; on the fact and its envelope both. A child runs on what its PARENT runs
  ;; on: the parent's model selection — provider, model and reasoning effort
  ;; — is written as the child's own in the same transaction, unless the
  ;; child, an id minted standing by, already picked one. An anchored fork's
  ;; requests then extend the parent's cached prefix, and a detached seat
  ;; works on its opener's model.
  (when (and detached anchor-turn-id)
    (error 'conflicting-session-edge :session-id (or id "")))
  (check-session-lineage id parent anchor-turn-id)
  (let* ((session-id (or id (make-durable-id "session")))
         ;; An absolute directory string; existence is not required.
         (resolved (resolve-session-cwd cwd))
         ;; Read before the transaction opens. A parent on the gateway
         ;; default carries nothing.
         (pin (and parent (null (session-model-selection-payload session-id))
                   (with-json ((provider :text "provider") (model :text "model")
                               (effort :text "effort"))
                       (session-model-selection-payload parent)
                     (present-plist :provider provider :model model :effort effort)))))
    (with-store-transaction (handle)
       ;; A parent still standing by becomes durable here: the child's fact
       ;; cannot name a parent the log does not hold, and lineage is the one
       ;; reason a session without a block of its own is real.
       (when (and parent (on-standby-p parent))
         (materialize-standby-session parent :handle handle))
       (when parent
         (unless (session-created-fact-p parent)
           (error 'unknown-parent-session :session-id parent))
         ;; A parented create without an explicit anchor forks at the
         ;; parent's CURRENT head (the Zig forkSession default); a parent
         ;; with no turns yet degrades to a plain unanchored child, and a
         ;; DETACHED create reads no head at all: its edge is provenance
         ;; and its history begins at its own first turn.
         (unless (or anchor-turn-id detached)
           (setf anchor-turn-id (session-head-turn-id parent))))
       (when (session-created-fact-p session-id)
         (error 'duplicate-session :session-id session-id))
       ;; A turn of PARENT's composed tree, not merely of PARENT's own
       ;; log: a parent that is itself a fresh fork has no turns of its
       ;; own, and the head a child forks at is the ancestor turn it
       ;; forked at.
       (when (and anchor-turn-id (not (session-known-turn-p parent anchor-turn-id)))
         (error 'invalid-session-anchor :session-id session-id
                :anchor anchor-turn-id))
       (if (on-standby-p session-id)
           ;; The id was minted standing by: this command completes the
           ;; identity its mint described — a field this call names wins —
           ;; and writes whatever waited on it, all in this transaction.
           (let ((record (standby-session-record session-id)))
             (materialize-standby-session
              session-id :handle handle
              :cwd (or (and cwd resolved) (and record record.cwd) resolved)
              :parent (or parent record.parent)
              :anchor-turn-id (unless detached (or anchor-turn-id record.anchor-turn-id))))
           (append-session-created-row
            handle session-id :cwd resolved :parent parent
            :anchor-turn-id anchor-turn-id
            ;; The parent's whole log watermark, frozen forever: the log is
            ;; append-only, so everything the parent does after the fork is
            ;; strictly above it and never leaks in.
            :anchor-log-position (and anchor-turn-id (session-log-watermark parent))
            :command-id (or command-id (make-durable-id "command")) :occurred-at occurred-at))
       (when pin
         (append-event-row handle session-id +kind-model-selected+ pin))
       ;; the fact just written, read back inside this transaction
       (find-session session-id))))

;;; --- session directory read models ----------------------------------------
;;; The picker projections (/sessions) read the directory the log
;;; keeps in step with itself (events.lisp, the session directory): a listing
;;; is one walk of a row per session, and no prompt is decoded here.

(defparameter +session-title-column+
  "(select coalesce(d.title, d.prompt, '') from session_directory d
     where d.session_id = events.session_id)"
  "The title rule as an EVENTS column over an outer row of the session: the
newest session.title fact's non-empty title (the operator's /rename, a
cell naming its own sessions, a model's title), else the first line
of text of the first committed prompt that holds one, trimmed and bounded to
64 characters -- the name a session has from its first prompt's admission,
while that turn still runs -- else the empty string for a session with no
input yet.")

(defun session-title (session-id)
  "The session's title by +SESSION-TITLE-COLUMN+'s rule, or the empty string."
  (or (when-store-handle (handle)
        (sqlite:execute-single
         handle
         "select coalesce(title, prompt, '') from session_directory
           where session_id = ? and created_at is not null"
         session-id))
      ""))

(defun session-waits (session-id)
  "(values QUESTION TURN-ID): the newest mark a turn of SESSION-ID put on its
answer, that it waits on the person it answers (the session.waits fact the
directory keeps) -- QUESTION \"\" for the answer's own last line -- and the
turn that put it; NIL when none did."
  (values-list (when-store-handle (handle)
                 (first (sql-rows handle
                                  "select waits, waits_turn from session_directory
                                    where session_id = ? and waits is not null"
                                  session-id)))))

(defun list-sessions ()
  "Session directory, newest activity first."
  ;; Millisecond clocks can tie; the id breaks the tie so the directory order
  ;; is a function of the log. => list of plists.
  (when-store-handle (handle)
    (loop for (session-id created-at cwd title updated-at message-count parent-raw anchor-raw
               source prompt turn-id turn-kind waits waits-turn)
            in (sql-rows handle
                         "select session_id, created_at, cwd,
                                 coalesce(title, prompt, ''), updated_at,
                                 message_count, parent, anchor_turn_id, source, prompt,
                                 turn_id, turn_kind, waits, waits_turn
                            from session_directory
                           where created_at is not null
                           order by updated_at desc, session_id")
          collect (list :id session-id
                        :title title
                        ;; The first prompt's line that says something
                        ;; (DIRECTORY-PROMPT-SQL), beside the name it may
                        ;; have become: what the session was asked first.
                        :prompt (and (stringp prompt) prompt)
                        :created-at created-at
                        :updated-at updated-at
                        :working-directory (or cwd "")
                        ;; A v1 row's `"parent": false' reads back as 0,
                        ;; never a name. The fork anchor beside it: an anchor
                        ;; turn means the session composes that parent's
                        ;; history; a parent with no anchor is provenance
                        ;; alone -- a session spawned by another that begins
                        ;; fresh.
                        :parent (and (stringp parent-raw) parent-raw)
                        :anchor-turn-id (and (stringp anchor-raw) anchor-raw)
                        :message-count message-count
                        ;; What SESSION-SOURCE-KIND answers, kept by the
                        ;; directory: the source that began the session.
                        :source source
                        ;; The fact that began or ended its newest turn
                        ;; (+DIRECTORY-TURN-KINDS+), NIL before one.
                        :turn-kind turn-kind
                        ;; What its answer waits on, the person it answers:
                        ;; the question the newest turn marked it with (""
                        ;; for the answer's own last line) once that turn
                        ;; completed; NIL while it runs, after it failed or
                        ;; was stopped, and once a later turn began.
                        :waits (and (stringp waits)
                                    (equal waits-turn turn-id)
                                    (equal turn-kind +kind-turn-completed+)
                                    waits)))))

(defun session-tree-order (entries &key expanded &aux (by-id (make-hash-table :test #'equal))
                                                      (children (make-hash-table :test #'equal))
                                                      (stamps (make-hash-table :test #'equal))
                                                      (placed (make-hash-table :test #'equal))
                                                      (out '()))
  "ENTRIES (LIST-SESSIONS plists, any order) in the order the sessions
picker draws them: a session created as a fork of another — its :PARENT —
hangs right under that parent as a line one lane deeper, and a fork of a
fork stacks with it."
  ;; Every row comes back with the shape the checkpoint tree carries
  ;; (CHECKPOINT-TREE-ORDER), so the one glyph renderer draws both: :DEPTH, 0
  ;; on a session that began on its own; :LANES, one boolean per lane the row
  ;; sits inside, outermost first, T while that lane still has a later fork
  ;; below it; and :FORK, T on the row that opens a lane.
  ;;
  ;; EXPANDED, when given, is the fold: a predicate on a session id asked
  ;; before that session's forks are drawn, so a false answer keeps its branch
  ;; out of the listing — the sessions picker's collapsed parent — while the
  ;; branch still ranks by its whole shape, and a row a collapsed session
  ;; hides never resurfaces at the root. NIL draws every fork, the listing the
  ;; walk has always drawn.
  ;;
  ;; Roots keep the directory's own order, newest activity first, and a
  ;; session with children sorts by the NEWEST activity anywhere in its branch
  ;; — a fork the operator just used floats its whole line up rather than hide
  ;; under a stale parent. Children of one session follow it in that same
  ;; order, ties keeping the directory's. A session whose parent is not among
  ;; ENTRIES — a parent the listing's limit cut — is a root; so is anything
  ;; the walk cannot reach, so the listing loses no row.
  (dolist (entry entries)
    (setf (gethash (getf entry :id) by-id) entry))
  (dolist (entry entries)
    (let ((parent (getf entry :parent)))
      ;; Buckets fill in directory order and EMIT reverses them; a root is a
      ;; child of NIL.
      (push (getf entry :id) (gethash (and (not (equal parent (getf entry :id)))
                                           (gethash parent by-id)
                                           parent)
                                      children))))
  (labels ((branch-stamp (id)
             ;; The newest activity anywhere in ID's branch. Seeded with
             ;; ID's own stamp before the walk, so a parent cycle reads a
             ;; half answer instead of spinning.
             (or (gethash id stamps)
                 (let ((stamp (setf (gethash id stamps)
                                    (or (getf (gethash id by-id) :updated-at) ""))))
                   (setf (gethash id stamps)
                         (reduce (lambda (stamp child) (if (string> child stamp) child stamp))
                                 (gethash id children) :key #'branch-stamp :initial-value stamp)))))
           (ranked (ids)
             ;; Newest branch first; STABLE-SORT keeps the directory's
             ;; own order among equal stamps.
             (stable-sort (copy-list ids) #'string> :key #'branch-stamp))
           (branch-drawn-p (id)
             ;; T unless the fold holds ID inside a collapsed ancestor:
             ;; every session from ID's own parent up must answer
             ;; EXPANDED. A root — and every row when no fold is in
             ;; force — is drawn.
             (or (null expanded)
                 (loop with seen = (make-hash-table :test #'equal)
                       for parent = (getf (gethash id by-id) :parent)
                         then (getf (gethash parent by-id) :parent)
                       while (and parent
                                  (gethash parent by-id)
                                  (not (equal parent id))
                                  (not (gethash parent seen)))
                       do (setf (gethash parent seen) t)
                       always (funcall expanded parent))))
           (kids-of (id)
             ;; The forks EMIT draws under ID: none while the fold holds
             ;; ID collapsed.
             (if (and expanded (not (funcall expanded id)))
                 '()
                 (ranked (gethash id children))))
           (emit (id depth lanes fork)
             (unless (gethash id placed)
               (setf (gethash id placed) t)
               (push (append (gethash id by-id)
                             (list :depth depth :lanes lanes :fork fork))
                     out)
               (loop for (kid . more) on (kids-of id)
                     do (emit kid (1+ depth)
                              (append lanes (list (and more t)))
                              t)))))
    (dolist (id (ranked (reverse (gethash nil children))))
      (emit id 0 '() nil))
    ;; Anything the fork walk could not place — a cycle the log should
    ;; never hold — still lists, at the root; a row the fold hides inside
    ;; a collapsed branch is not the walk's to lose, so it stays out.
    (dolist (entry entries)
      (when (branch-drawn-p (getf entry :id))
        (emit (getf entry :id) 0 '() nil))))
  (nreverse out))

;;; --- per-session model selection ------------------------------------------
;;; A durable session-scoped config fact, read newest-wins. Deliberately NOT
;;; session KV: checkpoint navigation folds the KV to the head watermark,
;;; and a model choice must survive an undo.

(defun record-session-model-selection (session-id &key provider model effort)
  "Durably select SESSION-ID's provider/model override and its reasoning
EFFORT pick, one rung of its target's ladder (SESSION-MODEL-EFFORT reads it)."
  ;; All NIL clears — the session follows the gateway default again. Each
  ;; fact is the whole selection, so a writer that moves one part carries the
  ;; others. => log position.
  (check-type session-id session-id)
  (record-event session-id +kind-model-selected+
                (present-plist :provider provider :model model :effort effort)))

(defun session-model-selection-payload (session-id)
  "The newest selection fact's payload for SESSION-ID — held on its standby
record until its first block, durable after — or NIL."
  (check-type session-id session-id)
  (and (store-open-p)
       (if (on-standby-p session-id)
           (standby-session-fact session-id +kind-model-selected+)
           (events :session-id session-id :kind +kind-model-selected+
                   :order :newest :as :payload))))

(defun session-model-selection (session-id)
  "=> (values PROVIDER MODEL): the newest selection for SESSION-ID, both NIL
when the session follows the gateway default."
  ;; A cleared selection encodes an empty payload, which reads as both NIL.
  (with-json ((provider :text "provider") (model :text "model"))
      (session-model-selection-payload session-id)
    (values provider model)))

(defun session-model-effort (session-id)
  "SESSION-ID's reasoning-effort pick — the rung its newest selection
carries — or NIL when it picked none."
  (json-value (session-model-selection-payload session-id) :text "effort"))
