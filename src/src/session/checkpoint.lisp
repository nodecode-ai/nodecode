;;;; checkpoint.lisp --- turn-tree checkpoints: head, navigation, fork, list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; pi-mono model, not the Zig hidden-frame port: turns form an append-only
;;;; tree via PARENT-TURN-ID stamps on TURN.STARTED, and each session has one
;;;; durable movable head. A rewind appends SESSION.HEAD_MOVED; redo is the
;;;; same fact aimed at the abandoned head (PREVIOUS-TURN-ID in every move
;;;; makes it addressable); the visible transcript is exactly the head's
;;;; ancestor chain. Nothing is hidden, deleted, or renumbered destructively
;;;; — abandoned branches stay navigable forever.
;;;;
;;;; Fork composes, never copies: a child session created with an anchor
;;;; references the parent chain through that anchor, frozen at the parent's
;;;; fork-time log watermark. A parent rewind after the fork cannot change
;;;; the child.
;;;;
;;;; Rules that keep this sound (each pinned by a test):
;;;;   - the eviction floor is monotonic and never rewound; a head move
;;;;     below it leaves provider-context exclusions standing
;;;;   - the KV fold rewinds on backward moves only; redo never re-folds
;;;;     forward, because the KV event stream is linear and branch-blind —
;;;;     folding forward would resurrect abandoned-branch writes
;;;;   - client-held KV CAS tokens are stale after any head move; clients
;;;;     must re-read after SESSION_RESYNC_REQUIRED

(in-package #:nodecode.kernel)

;;; --- head, parents, visible chain ----------------------------------------

(defun session-head-stamp (session-id)
  "LOG-POSITION of the newest SESSION.HEAD_MOVED, or 0. The hydration-cache
validity token for visibility: coverage alone cannot see a head move, which
appends above any pinned watermark while changing the visible set below it."
  (check-type session-id session-id)
  (events :session-id session-id :kind +kind-head-moved+
          :as :value :columns '("coalesce(max(log_position), 0)")))

(defun session-lineage (session-id)
  "Fork ancestry as ((ancestor-session-id anchor-turn-id anchor-log-position)
...) root-first, excluding SESSION-ID itself."
  ;; Empty for non-forked sessions; a parent link without an anchor is
  ;; provenance only and contributes nothing. A fork standing by has its
  ;; ancestry already, from its mint (SESSION-FORK-EDGE).
  (check-type session-id session-id)
  (nreverse (loop repeat 64            ; ancestry depth guard
                  for (parent anchor position) = (session-fork-edge session-id)
                    then (session-fork-edge parent)
                  while (and parent anchor)
                  collect (list parent anchor (or position 0)))))

(defun session-chain-scope (session-id &aux (segments (session-lineage session-id)))
  "The events SESSION-ID composes, as an EVENTS :WHERE clause (SQL .
ARGUMENTS): its own, and each anchored ancestor's up to the log position its
fork froze that ancestor at."
  ;; The one statement of what a fork inherits for the reads that are not a
  ;; fold: the floors its window stands on (RETAINED-HISTORY-FLOORS) and the
  ;; model's own read of the log (RECALL-LOG). The cut is the anchor, as
  ;; COLLECT-VISIBLE-SESSION-EVENTS cuts: what an ancestor records after the
  ;; fork froze it is invisible to the fork. Head visibility is not applied —
  ;; an eviction fact carries no turn, and recall is the memory that survives
  ;; navigation.
  (cons (format nil "(~{~a~^ or ~})"
                (append (loop repeat (length segments)
                              collect "(session_id = ? and log_position <= ?)")
                        (list "session_id = ?")))
        (append (loop for (ancestor nil position) in segments
                      append (list ancestor position))
                (list session-id))))

(defun session-composed-parents (session-id)
  "TURN-ID -> parent map across the fork lineage plus SESSION-ID itself."
  (with-store-handle (handle)
    (let ((combined (make-hash-table :test #'equal)))
      ;; Ancestors root-first, SESSION-ID last: a later segment's entry wins.
      (dolist (id (session-standing-chain session-id) combined)
        (loop for turn-id being the hash-keys of (session-turn-parents id) using (hash-value parent)
              do (setf (gethash turn-id combined) parent))))))

;;; --- the standing block a fork inherits -----------------------------------
;;; The composed history is half of what a fork inherits; the standing
;;; <harness> sections above it are the other half. Both are the prompt the
;;; chain already sent, and a provider's cache is a prefix match: a newborn
;;; fork that re-derives its own standing block from live material diverges
;;; inside the system prompt and pays its whole inherited history uncached
;;; (12 room lanes, 146-157k prompt tokens each, 640 cached).
;;; So the chain owns the block: a session reads every anchored ancestor's
;;; sections and its own, nearest wins, and a pinned section is written on
;;; the chain's root (KNOWLEDGE.LISP PIN-INDEX-SECTION) where every later
;;; fork reads the same bytes.

(defun session-standing-chain (session-id)
  "SESSION-ID's anchored fork chain, root first, SESSION-ID last: the
sessions whose standing sections it runs under."
  ;; A parent link with no anchor is provenance only and contributes nothing,
  ;; exactly as the lineage walk treats it.
  (append (mapcar #'first (session-lineage session-id)) (list session-id)))

(defun session-standing-owner (session-id)
  "The session that owns SESSION-ID's standing block: the root of its
anchored fork chain, or SESSION-ID itself when it forks nothing."
  ;; One conversation, one standing block — a lane that forks a room reads and
  ;; pins the room's, so every lane of that room sends the same prefix.
  (first (session-standing-chain session-id)))

(defun standing-harness-sections (session-id)
  "The standing sections SESSION-ID runs under as (NAME . TEXT), name-ordered:
every session in its fork chain read root-first, a nearer session's entry of
a name replacing the one above it."
  ;; Under the same 64KiB read floor LIST-HARNESS-SECTIONS applies, over the
  ;; composition.
  (let ((merged (remove-duplicates (mapcan #'list-harness-sections
                                           (session-standing-chain session-id))
                                   :key #'car :test #'string=)))
    (loop with budget = +max-harness-bytes+
          for entry in (sort merged #'string< :key #'car)
          while (<= (length (cdr entry)) budget)
          do (decf budget (length (cdr entry)))
          collect entry)))

(defvar *visible-turn-sets* (make-hash-table :test #'equal)
  "SESSION-ID -> (AUTHORITY CHAIN-SET HEAD-TURN-ID HEAD-STAMP), the last
answer SESSION-VISIBLE-TURN-SET gave, good while the head authority position
holds. Retention: live-only; a store opening clears it.")

(defun forget-visible-turn-sets ()
  "Drop every memoized visible set: a store that opens is another log."
  (sb-ext:with-locked-hash-table (*visible-turn-sets*)
    (clrhash *visible-turn-sets*)))

(pushnew 'forget-visible-turn-sets *store-open-hooks*)

(defun session-head-authority (session-id)
  "LOG-POSITION of the newest fact that set SESSION-ID's head — a
TURN.STARTED, a SESSION.HEAD_MOVED or the SESSION.CREATED whose anchor a fork
starts at, the three kinds SESSION-HEAD-TURN-ID reads — or 0. The memo key
for the visible set: the head stamp alone would not do, since a TURN.STARTED
re-points the head with no head-move fact."
  ;; The created fact is a key too: a fork standing by is read from its mint
  ;; (SESSION-FORK-EDGE), and the memo it leaves is stale once its identity
  ;; lands.
  (events :session-id session-id
          :kind (list +kind-turn-started+ +kind-head-moved+ +kind-session-created+)
          :as :value :columns '("coalesce(max(log_position), 0)")))

(defun session-visible-turn-set (session-id)
  "=> (values CHAIN-SET HEAD-TURN-ID HEAD-STAMP) for SESSION-ID's current
head."
  ;; CHAIN-SET holds exactly the visible turn ids, across fork lineage.
  ;;
  ;; Memoized per session on SESSION-HEAD-AUTHORITY: the chain is a function
  ;; of the head and of the parent links, and a link never changes once its
  ;; TURN.STARTED is in the log, so the answer stands until a fact that can
  ;; re-point the head lands. The gateway asks after every committed fact of a
  ;; running turn; between the facts that costs one indexed max instead of the
  ;; head reads and a scan of every TURN.STARTED in the lineage, a JSON decode
  ;; each. The set returned is shared: readers only.
  (check-type session-id session-id)
  (let* ((authority (session-head-authority session-id))
         (entry (sb-ext:with-locked-hash-table (*visible-turn-sets*)
                  (gethash session-id *visible-turn-sets*))))
    (if (and entry (eql (first entry) authority))
        (values-list (rest entry))
        (let* ((head (session-head-turn-id session-id))
               (stamp (session-head-stamp session-id))
               (chain-set (chain-set-from (session-composed-parents session-id) head)))
          (sb-ext:with-locked-hash-table (*visible-turn-sets*)
            (setf (gethash session-id *visible-turn-sets*)
                  (list authority chain-set head stamp)))
          (values chain-set head stamp)))))

(defun turn-visible-p (turn-id visible-set)
  "Turn-bearing facts are visible on the head chain only; turn-less facts
(session.created, evictions, floor facts, head moves, captures) always are."
  (or (null turn-id)
      (not (null (gethash turn-id visible-set)))))

(defun event-visible-p (event visible-set)
  "Whether EVENT's turn is visible (TURN-VISIBLE-P)."
  (turn-visible-p event.turn-id visible-set))

;;; --- the head-move writer ------------------------------------------------

(defun move-session-head (session-id target-turn-id &rest options)
  "Append SESSION.HEAD_MOVED re-pointing SESSION-ID's head."
  ;; TARGET-TURN-ID NIL is the root (before the first turn). Refuses while a
  ;; turn is active — the caller holds the gateway latch, and this
  ;; in-transaction re-check is the durable authority. OPTIONS are
  ;; MOVE-SESSION-HEAD-IN-TRANSACTION's :REASON and :BOUNDARY-KIND. => plist
  ;; (:previous-turn-id ... :target-turn-id ... :log-position ...).
  (check-type session-id session-id)
  (when target-turn-id (check-type target-turn-id turn-id))
  (with-store-transaction (handle)
     (apply #'move-session-head-in-transaction handle session-id target-turn-id options)))

(defun move-session-head-in-transaction (handle session-id target-turn-id
                                         &key (reason "rewind")
                                         (boundary-kind "turn"))
  "The head-move body, composable inside a caller's transaction."
  ;; A fork standing by shows the turns it carries (SESSION-FORK-EDGE), so an
  ;; operator can rewind in it before its first prompt: the move is its first
  ;; block, and makes it durable as a prompt does.
  (materialize-standby-session session-id :handle handle)
  (when (active-turn-p session-id)
    (error 'turn-already-active :session-id session-id))
  (let ((previous (session-head-turn-id session-id)))
    (list :previous-turn-id previous
          :target-turn-id target-turn-id
          :log-position
          (with-event-owner 'move-session-head
            (append-event-row
             handle session-id +kind-head-moved+
             (list* :reason reason
                    :boundary-kind boundary-kind
                    :source-watermark (session-log-watermark session-id)
                    (present-plist :target-turn-id target-turn-id
                                   :previous-turn-id previous)))))))

(defun last-head-move-origin (session-id)
  "PREVIOUS-TURN-ID of the newest SESSION.HEAD_MOVED, or NIL — the redo
cursor: where the operator most recently navigated away from."
  ;; A redo is possible exactly while this origin exists off the current head
  ;; chain.
  ;;
  ;; A retraction is not a navigation: its origin is an empty cancelled turn
  ;; the operator took back (the prompt went home to the composer), so it
  ;; offers no redo target.
  (check-type session-id session-id)
  (let ((payload (events :session-id session-id :kind +kind-head-moved+
                         :order :newest :as :payload)))
    (unless (equal (json-value payload :string "reason") +head-move-reason-retract+)
      (values (json-value payload :text "previous-turn-id")))))

(defun redo-origin (session-id &aux (origin (last-head-move-origin session-id)))
  "Where a redo returns to: the last head move's origin once off the visible chain, else NIL."
  (and origin (not (gethash origin (session-visible-turn-set session-id)))
       (session-known-turn-p session-id origin) origin))

(defun turn-end-log-position (turn-id)
  "The newest LOG-POSITION of TURN-ID's events, or 0 — THE watermark rule
for navigation folds and preimage anchoring."
  ;; One definition; every caller routes here, inside a transaction or out.
  (events :turn-id turn-id :as :value :columns '("coalesce(max(log_position), 0)")))

(defun navigate-session-head (session-id target-turn-id &rest options)
  "One durable transaction for a navigation: the head move (which re-checks
the active-turn authority) and the KV fold commit together, so a crash can
never leave the KV folded to an abandoned branch while the head still
points elsewhere."
  ;; The fold watermark — the target turn's newest event position, 0 at root —
  ;; is computed inside the same transaction. On forward moves the stale set
  ;; is empty by construction: the KV event stream is linear and branch-blind,
  ;; so redo never re-folds. => the head-move plist.
  (with-store-transaction (handle)
     ;; Watermark BEFORE the head-move append: the fold boundary is the
     ;; target turn's newest event as the log stands, stated by order of
     ;; operations rather than by relying on the head-move fact keeping a
     ;; NIL turn_id column.
     (let ((watermark (if target-turn-id (turn-end-log-position target-turn-id) 0)))
       (prog1 (apply #'move-session-head-in-transaction handle session-id target-turn-id options)
         (fold-session-state-to-watermark-in-transaction handle session-id watermark)))))

;;; --- workspace capture projections ---------------------------------------

(defun session-event-payloads (session-id kind &key order limit where)
  "(TURN-ID PAYLOAD-TABLE LOG-POSITION OCCURRED-AT) for SESSION-ID's KIND events, oldest
first; ORDER :NEWEST reads the tail of the kind instead, at most LIMIT rows of
it (the listing's own tail, never the whole kind), and WHERE ((SQL .
ARGUMENTS), the log reader's own escape) narrows the rows the log hands over
at all."
  ;; The knobs exist for the tail read (SESSION-CHECKPOINTS :LIMIT): the
  ;; payload decode is the listing's cost — 139 ms for a session's 1093 tool
  ;; results, 47 ms for its 1089 calls — so what is not listed is never
  ;; decoded. A full read passes none of them and reads exactly as before.
  (loop for (turn-id payload position occurred-at)
          in (events :session-id session-id :kind kind :where where
                     :order (or order :oldest)
                     :limit limit
                     :as :rows :columns '("turn_id" "payload" "log_position" "occurred_at"))
        collect (list turn-id (decode-event-payload-object payload) position occurred-at)))

(defun session-turn-endings (turn-ids &aux (endings (make-hash-table :test #'equal)))
  "TURN-ID -> (:ENDED-AT :ENDING :STEERS) for the turns among TURN-IDS: when
and how each ended -- its terminal event's instant and kind, `completed',
`failed' or `cancelled', both absent while it runs -- and how many inputs
the operator committed into it after the one that began it."
  ;; One grouped read over the log's (turn_id, kind) index, nothing decoded:
  ;; the facts are the rows' own columns. Turn ids are unique across the
  ;; store, so a fork's inherited turns answer from their own session.
  (when turn-ids
    (let ((ids (remove-duplicates turn-ids :test #'equal)))
      (loop for (turn-id ended-at ending inputs)
              in (events :kind (list +kind-turn-completed+ +kind-turn-failed+
                                     +kind-turn-cancelled+ +kind-input-committed+)
                         :where (list* (format nil "turn_id in (~{~a~^, ~})"
                                               (make-list (length ids) :initial-element "?"))
                                       ids)
                         :as :rows :group-by '(1)
                         :columns (list "turn_id"
                                        (format nil "max(case when kind <> '~a' then occurred_at end)"
                                                +kind-input-committed+)
                                        (format nil "max(case when kind <> '~a' then kind end)"
                                                +kind-input-committed+)
                                        (format nil "sum(kind = '~a')" +kind-input-committed+)))
            do (setf (gethash turn-id endings)
                     (list :ended-at ended-at
                           :ending (and ending (subseq ending (1+ (position #\. ending))))
                           :steers (max 0 (1- (or inputs 0))))))))
  endings)

(defun session-turn-answers (turn-ids &aux (answers (make-hash-table :test #'equal)))
  "TURN-ID -> the text of the last answer each of TURN-IDS gave."
  ;; One grouped read, only the message text decoded: SQLite answers a bare
  ;; column from the row MAX() picked, the turn's last answer. Without
  ;; grouping, log order would send the planner down the kind's whole index.
  ;; The listing asks it of a reflection's recaps alone: what a recap said
  ;; is the recap, where an ask's row reads the ask.
  (when turn-ids
    (let ((ids (remove-duplicates turn-ids :test #'equal)))
      (loop for (turn-id text)
              in (events :kind +kind-assistant-completed+
                         :where (list* (format nil "turn_id in (~{~a~^, ~})"
                                               (make-list (length ids) :initial-element "?"))
                                       ids)
                         :as :rows :group-by '(1)
                         :columns '("turn_id" "json_extract(payload, '$.message.content')"
                                    "max(log_position)"))
            when (stringp text)
              do (setf (gethash turn-id answers) text))))
  answers)

(defun session-tool-calls (session-id call-ids)
  "CALL-ID -> (:COMMAND :STARTED-AT) for the calls among CALL-IDS and no
others: the command text that call's TOOL-CALL-STARTED event named, when it
named one, and the instant it started."
  ;; The full map decodes every call the session ever made; a listing keeps a
  ;; few dozen rows, and the rows it keeps name the calls it needs — so the
  ;; predicate runs in SQL over the log's own index and only those payloads
  ;; are decoded. A call the map does not carry reads exactly as the full
  ;; scan's miss did: the row falls back to the tool's name.
  (let ((wanted (remove-duplicates (remove "" call-ids :test #'equal) :test #'equal))
        (calls (make-hash-table :test #'equal)))
    (when wanted
      (loop for (nil table nil started-at) in
            (session-event-payloads
             session-id +kind-tool-call-started+
             ;; The call ids are already-read log values and bind as
             ;; parameters; the placeholder list is the only text built.
             :where (list* (format nil "json_extract(payload, '\$.call-id') in (~{~a~^, ~})"
                                   (make-list (length wanted) :initial-element "?"))
                           wanted))
            for call-id = (json-value table :string "call-id")
            when call-id
              do (setf (gethash call-id calls)
                       (list :command (tool-command-string (json-value table :string "arguments"))
                             :started-at started-at))))
    calls))

(defun session-workspace-captures (session-id &key turn-ids)
  "Capture facts across the fork lineage plus SESSION-ID, oldest first.

TURN-IDS, when given, keeps only the captures of those turns: a tail listing
knows the turns it kept before it attaches their capture facts, and a turn's
captures all carry its id, so the filter is exact while the read stays the
few rows the listing can use rather than every capture the session made."
  ;; Ancestor segments are cut at their anchor watermark so post-fork parent
  ;; captures never leak in: one read of the chain (SESSION-CHAIN-SCOPE), in
  ;; log order, which is segment order — an ancestor's cut precedes the fork.
  (loop with scope = (session-chain-scope session-id)
        for (payload position)
          in (events :kind +kind-workspace-captured+ :index +session-kind-index+
                     ;; The TURN_ID COLUMN is NULL on these events — the
                     ;; capture's turn rides in the payload — so the
                     ;; filter reads the fact, not the table's column.
                     :where (if turn-ids
                                (list* (format nil "~a and json_extract(payload, '$.turn-id') ~
                                                    in (~{~a~^, ~})"
                                               (car scope)
                                               (make-list (length turn-ids) :initial-element "?"))
                                       (append (cdr scope) turn-ids))
                                scope)
                     :as :rows :columns '("payload" "log_position"))
        for table = (decode-event-payload-object payload)
        collect (nconc (loop for key in '(:checkpoint-id :boundary-kind :turn-id
                                          :tool-call-id :tree-sha :pre-tree-sha)
                             collect key
                             collect (json-value table :string
                                                 (string-downcase key)))
                       (list :anchor-position (json-value table :integer "anchor-position")
                             :log-position position))))

(defun close-detached-workspace-window (session-id tool-call-id)
  "Close a detached tool call's workspace window."
  ;; A call that detaches past its yield window keeps writing after its
  ;; tool_result bracket closed; the completion is the window's true end.
  ;; Recaptures the session workspace and derives the cumulative diff from the
  ;; call's ORIGINAL baseline (the pre tree its yield capture stamped), so the
  ;; one document spans everything the call wrote. => (values DOCUMENT
  ;; TREE-SHA YIELD-TREE-SHA), all NIL when captures are off, the call has no
  ;; capture fact, nothing changed since the yield capture, or the derivation
  ;; degrades. Never signals; runs git only, no store writes — the caller owns
  ;; the durable facts.
  (when-let (capture (and *workspace-capture-enabled*
                          ;; The capture lookup rides its own guard: it is the
                          ;; only store read on this path, and the never-signals
                          ;; contract has to cover it too.
                          (ignore-errors
                           (find tool-call-id
                                 (session-workspace-captures session-id)
                                 :key (lambda (entry) (getf entry :tool-call-id))
                                 :test #'equal
                                 :from-end t))))
    (with-session-snapshot-store (store session-id)
      (when-let (yield-tree (getf capture :tree-sha))
        (let ((tree-sha (capture-workspace-tree store)))
          (unless (equal tree-sha yield-tree)
            (anchor-tree store tree-sha
                         (format nil "completion ~a" tool-call-id))
            (values (workspace-diff-document
                     store (or (getf capture :pre-tree-sha) yield-tree)
                     tree-sha)
                    tree-sha
                    yield-tree)))))))

(defun turn-boundary-captured-p (session-id turn-id boundary-kind)
  "True when SESSION-ID already carries a workspace capture for TURN-ID at
BOUNDARY-KIND."
  ;; The boot-resume guard: RUN re-enters for a claimed turn and must not
  ;; stamp a second user_message capture — duplicate captures make predecessor
  ;; resolution ambiguous for undo.
  ;;
  ;; Capture facts deliberately leave the envelope turn_id column NIL (the
  ;; payload carries turn identity), so the scan is bounded by the turn's own
  ;; TURN.STARTED position instead: any capture for this turn postdates its
  ;; admission, and for the boot-resume case that bound cuts the scan to the
  ;; interrupted tail of the log rather than the session's whole history.
  (loop for table
          in (events :session-id session-id :kind +kind-workspace-captured+
                     :where (list "log_position >=
                                     coalesce((select log_position from events as started
                                                 where started.turn_id = ?
                                                   and started.kind = ? limit 1), -1)"
                                  turn-id +kind-turn-started+)
                     :as :payloads)
        thereis (and (equal (gethash "turn-id" table) turn-id)
                     (equal (gethash "boundary-kind" table) boundary-kind))))

(defun chain-capture-tree (capture chain-set &aux (turn-id (getf capture :turn-id)))
  "CAPTURE's tree sha when it has one and stands on CHAIN-SET, or on no turn
at all; else NIL."
  (and (or (null turn-id) (gethash turn-id chain-set))
       (getf capture :tree-sha)))

(defun restore-tree-for-checkpoint (captures target-checkpoint-id parents)
  "The tree to restore for TARGET: the newest capture strictly before it on
its own chain in PARENTS with a tree sha — a tool_result capture postdates
the write it must reverse, so the pre-state is always the predecessor's tree."
  ;; Only the first checkpoint of the chain uses its own tree. => sha or NIL.
  (when-let (index (position target-checkpoint-id captures
                             :key (lambda (capture) (getf capture :checkpoint-id))
                             :test #'equal))
    (let ((chain-set (chain-set-from parents (getf (nth index captures) :turn-id))))
      (or (loop for i from (1- index) downto 0
                thereis (chain-capture-tree (nth i captures) chain-set))
          (getf (nth index captures) :tree-sha)))))

(defun session-head-tree (captures parents head &aux (newest (make-hash-table :test #'equal)) loose)
  "The tree the session last recorded at HEAD — the conflict baseline for
restore, never current disk: the newest capture of the nearest turn up HEAD's
chain in PARENTS that has one, else the newest capture on no turn."
  ;; Nearest before newest. A redo records the tree it leaves as a capture of
  ;; the ancestor it moves off (the preimage), a fact newer in the log than
  ;; every capture of the turn it moves to; newest-on-the-chain made that
  ;; ancestor's tree the baseline, so the next rewind read the head's own
  ;; files as foreign drift and left them on disk.
  (dolist (capture captures)
    (when-let (tree (getf capture :tree-sha))
      (if (getf capture :turn-id)
          (setf (gethash (getf capture :turn-id) newest) tree)
          (setf loose tree))))
  (or (loop with seen = (make-hash-table :test #'equal)
            for turn-id = head then (gethash turn-id parents)
            while (and turn-id (not (gethash turn-id seen)))
            do (setf (gethash turn-id seen) t)
            thereis (gethash turn-id newest))
      loose))

;;; --- changed-files projection (the Zig undoImpactFiles derivation) --------
;;; What restoring a checkpoint would ACTUALLY touch: the worktree-vs-
;;; target-tree diff, where the target is the predecessor capture's tree —
;;; never rollback receipts — MINUS the paths the restore engine would
;;; refuse as foreign-write conflicts (disk departing from the session's
;;; last recorded write). The raw diff is the Zig undoImpactFiles shape;
;;; the conflict subtraction is a deliberate divergence: a workspace shared
;;; across sessions accrues foreign drift, and advertising paths a restore
;;; will refuse reads as cross-session leakage. Absent knowledge (no
;;; git, no workspace, captures disabled, no captured tree, no derivable
;;; conflict set) degrades to an EMPTY list rather than a wrong one.

(defun checkpoint-diff-store (session-id)
  "SESSION-ID's snapshot store with the worktree staged once for read-side
diffs, or NIL when no diff can be derived (captures disabled, sessionless,
no workspace, store unavailable)."
  ;; Never signals. The capture kill switch gates this path like every other:
  ;; staging runs ADD -A, which must never touch a workspace's git state while
  ;; captures are off.
  (with-session-snapshot-store (store session-id)
    (stage-workspace store)
    store))

(defun conflict-baseline-tree (captures parents head)
  "The tree the restore conflict set is measured against: the session's last
recorded write at HEAD (SESSION-HEAD-TREE), else the empty tree — a session
that recorded nothing claims nothing, so every on-disk path is foreign."
  ;; Named because two readers want the same answer: the conflict set below,
  ;; and the batched read side, which asks for this tree's diff once and
  ;; spends it twice (a row whose undo target IS this tree can only be
  ;; conflicts, so it counts zero without a second diff).
  (or (session-head-tree captures parents head)
      +git-empty-tree-sha+))

(defun checkpoint-changed-files (session-id checkpoint-id)
  "Distinct workspace paths restoring CHECKPOINT-ID would actually touch:
the staged-worktree-vs-target-tree diff against the checkpoint's undo-target
tree (RESTORE-TREE-FOR-CHECKPOINT over the capture's own chain), minus the
restore engine's conflict set (the staged paths departing from
CONFLICT-BASELINE-TREE — foreign drift is refused, so it is never advertised)."
  ;; The staging and both diffs spend ONE child process rather than three,
  ;; which is what the tree picker's per-row highlight fetch waits on. =>
  ;; (values paths known-p): KNOWN-P is NIL only for an unknown checkpoint id;
  ;; every degradation answers (values '() T).
  (let* ((captures (session-workspace-captures session-id))
         (capture (find checkpoint-id captures
                        :key (lambda (entry) (getf entry :checkpoint-id))
                        :test #'equal))
         (parents (and capture (session-composed-parents session-id))))
    (if (null capture)
        (values '() nil)
        (values (checkpoint-tree-paths
                 session-id captures parents
                 (restore-tree-for-checkpoint captures checkpoint-id parents))
                t))))

(defun checkpoint-tree-paths (session-id captures parents tree)
  "The paths restoring TREE (or NIL) touches -- its staged diff minus the
conflict set; NIL when a diff is unavailable."
  ;; One batch stages the worktree and diffs TREE and the conflict baseline;
  ;; the fallback stages the index itself (the ADD -A the batch would have
  ;; run) and answers per tree.
  (let* ((live (and tree (with-session-snapshot-store (live session-id) live)))
         (baseline (and live (conflict-baseline-tree
                              captures parents (nth-value 1 (session-visible-turn-set session-id)))))
         (batch (and live (staged-tree-diffs-batched live (list baseline tree))))
         (staged (and live (not batch) (checkpoint-diff-store session-id))))
    (flet ((tree-paths (tree)
             (cond (batch (gethash tree batch :unavailable))
                   ((null staged) :unavailable)
                   (t (with-handlers ((error () :unavailable))
                        (staged-changed-paths staged tree))))))
      (let* ((baseline-paths (and live (tree-paths baseline)))
             (conflicts (and live (listp baseline-paths) (path-set baseline-paths)))
             (paths (and conflicts (if (equal tree baseline) baseline-paths (tree-paths tree)))))
        (and (listp paths)
             (loop for record in paths
                   for path = (getf record :path)
                   unless (gethash path conflicts)
                     collect path))))))

(defun session-turn-origins (session-id &aux (origins (make-hash-table :test #'equal)))
  "TURN-ID -> TURN-ORIGIN, for the turns of SESSION-ID's composed history that
no one asked: an exit's wake and a reflection's recap."
  ;; The facts that say so are the turn.started rows that carry a wake and the
  ;; turn.completed rows that carry a recorder: SQLite narrows to them before
  ;; anything is decoded, so a session's whole history is no cost of a listing
  ;; that shows its tail. An ancestor's turns past the fork's anchor come too;
  ;; they are on no chain the fork counts.
  (dolist (id (session-standing-chain session-id) origins)
    (loop for (turn-id table) in (session-event-payloads
                                  id +kind-turn-started+
                                  :where '("json_extract(payload, '$.wake') is not null"))
          for origin = (turn-origin (gethash "wake" table) nil)
          when origin do (setf (gethash turn-id origins) origin))
    (loop for (turn-id table) in (session-event-payloads
                                  id +kind-turn-completed+
                                  :where '("json_extract(payload, '$.recorder') is not null"))
          for origin = (turn-origin nil (gethash "recorder" table))
          when origin do (setf (gethash turn-id origins) origin))))

(defun session-checkpoints (session-id &key limit)
  "Every implicit checkpoint of SESSION-ID's composed tree, oldest first:
each TURN.STARTED is a user_message checkpoint, each TURN.TOOL_RESULT a
tool_result checkpoint. A user_message row of a turn no one asked -- a wake, a
reflection's recap -- names its :ORIGIN and has no :TURN-NUMBER; the number of
one that was asked counts the asks on its chain, and none of those.

LIMIT answers the newest that many rows and reads the log's tail to find
them: each kind comes back newest-first under the same cut, and only the kept
turns' captures and the kept calls' commands are read. The answer is exactly
(LAST (no-limit answer) LIMIT) — a row of the merged tail has fewer than
LIMIT rows above it in its own segment too, so each segment's own tail is a
superset of what the merge keeps, and the chain filter only ever removes
rows."
  ;; Off-chain branches are included — they are the redo/branch picker —
  ;; marked by :ON-HEAD-CHAIN. Ancestor sessions contribute only their anchor
  ;; chains (off-chain ancestor branches belong to the parent's own tree).
  (when limit (check-type limit (integer 1) "a positive row count"))
  (bind (((chain-set head) (session-visible-turn-set session-id))
         (parents (session-composed-parents session-id)) (captures '())
         (origins (session-turn-origins session-id))
         (position (lambda (row) (getf row :log-position))))
    (flet ((capture-for (turn-id tool-call-id boundary)
             (loop for capture in (reverse captures)
                   when (and (equal (getf capture :boundary-kind) boundary)
                             (equal (getf capture :turn-id) turn-id)
                             (or (null tool-call-id)
                                 (equal (getf capture :tool-call-id)
                                        tool-call-id)))
                     return capture))
           (rows-for (session-id)
             ;; Unordered within a kind: the listing sorts every segment's
             ;; rows by position below. The tool results come first: they
             ;; name the calls whose commands are worth decoding, and only
             ;; those calls' payloads are read.
             (let* ((results (session-event-payloads
                              session-id +kind-tool-result+
                              :order (and limit :newest) :limit limit))
                    (calls (session-tool-calls
                            session-id
                            ;; The call ids as the results name them:
                            ;; RESULTS are still the log's payload
                            ;; tuples here, not this function's rows.
                            (loop for (nil table) in results
                                  collect (or (json-value table :string "call-id") "")))))
               (nconc
                (loop for (turn-id table position started-at) in
                      (session-event-payloads session-id +kind-turn-started+
                                              :order (and limit :newest) :limit limit)
                      collect (list :kind "user_message" :turn-id turn-id
                                    :prompt (or (json-value table :string "input") "")
                                    :started-at started-at
                                    :log-position position))
                ;; A call's span: its TOOL_CALL_STARTED to this result.
                (loop for (turn-id table position ended-at) in results
                      for call-id = (or (json-value table :string "call-id") "")
                      for call = (gethash call-id calls)
                      collect (list :kind "tool_result" :turn-id turn-id
                                    :tool-call-id call-id
                                    :tool-name (or (json-value table :string "tool-name") "")
                                    :tool-command (getf call :command)
                                    :started-at (getf call :started-at)
                                    :ended-at ended-at
                                    :log-position position))))))
      (with-store-handle (handle)
        (let* ((rows (loop for (segment-id segment-chain)
                             in (append
                                 (loop for (id anchor) in (session-lineage session-id)
                                       collect (list id (chain-set-from parents anchor)))
                                 (list (list session-id nil)))
                           ;; The kept rows of this segment: its own newest LIMIT,
                           ;; by position — and the count measured on the SORTED
                           ;; list, never on the variable SORT scrambled (its head
                           ;; is no longer the list's head once it moves).
                           nconc (loop for row in (last (sort (rows-for segment-id) #'<
                                                              :key position)
                                                        (or limit most-positive-fixnum))
                                       when (or (null segment-chain)
                                                (gethash (getf row :turn-id) segment-chain))
                                         collect row)))
               (rows (last (sort rows #'< :key position) (or limit most-positive-fixnum))))
          ;; The captures of the kept turns, and only those — SETF, never a
          ;; rebinding: CAPTURE-FOR closed over the outer binding, and a
          ;; fresh binding here would leave every row it attaches to
          ;; reading NIL (no checkpoint id, no tree).
          (setf captures
                (session-workspace-captures
                 session-id
                 :turn-ids (and limit (distinct (mapcar (lambda (row) (getf row :turn-id)) rows)))))
          (loop with endings = (session-turn-endings
                                (loop for row in rows
                                      when (equal (getf row :kind) "user_message")
                                        collect (getf row :turn-id)))
                with answers = (session-turn-answers
                                (loop for row in rows
                                      when (equal "reflection" (gethash (getf row :turn-id) origins))
                                        collect (getf row :turn-id)))
                for row in rows
                for turn-id = (getf row :turn-id)
                for capture = (capture-for turn-id (getf row :tool-call-id) (getf row :kind))
                collect (append
                         row
                         (list :tree-sha (getf capture :tree-sha)
                               :checkpoint-id (getf capture :checkpoint-id)
                               :on-head-chain
                               (not (null (gethash turn-id chain-set))))
                         (when (equal (getf row :kind) "user_message")
                           (let ((origin (gethash turn-id origins)))
                             (append
                              (list :parent-turn-id (gethash turn-id parents)
                                    :origin origin
                                    ;; The asks on the chain, this one included.
                                    :turn-number (and (null origin)
                                                      (loop for id being the hash-keys
                                                              of (chain-set-from parents turn-id)
                                                            count (null (gethash id origins))))
                                    :is-head (equal turn-id head))
                              ;; When and how it ended, and the steers it took.
                              (gethash turn-id endings)
                              ;; A recap's row reads what it said.
                              (let ((answer (gethash turn-id answers)))
                                (and answer (list :answer answer)))))))))))))

(defun checkpoint-tree-order (entries)
  "ENTRIES (SESSION-CHECKPOINTS plists, any order) in tree order, each with
the shape the picker draws: :DEPTH, :LANES and :FORK."
  ;; The first line of turns is the trunk: oldest first, a turn's prompt row
  ;; then its tool rows. Among a turn's children the OLDEST continues the line
  ;; at the same depth and comes last; every later child — a turn the operator
  ;; branched from it afterwards — is a line of its own one lane deeper,
  ;; whole, right under the parent's row. So with yo, yo1, yo2 first and
  ;; hola!, que? branched from yo1 later: `yo, yo1, [hola!, que?], yo2'; and x
  ;; branched from hola! after que?: `hola!, [x], que?'.
  ;;
  ;; :LANES is one boolean per lane the row sits inside, outermost first: T
  ;; while that lane's parent has more later lines forking off below this row,
  ;; NIL once this was its last. :FORK is T on the prompt row where a later
  ;; line forks off its parent. Together they draw the tree: `├' or `╰' on a
  ;; fork row, `│' down a lane still open, blank once it closed — so `yay, no'
  ;; and `hello!, bye!' read as two lines off turn1, not four siblings. Chain
  ;; membership plays no part in the shape (the ● and ◇ carry it): the head
  ;; sits wherever its line is. A prompt whose parent is not among ENTRIES — a
  ;; root, or a parent the listing's limit cut — joins the root group, the
  ;; later roots one lane deep above the oldest; a tool row whose turn is
  ;; absent leads on the trunk.
  (let ((ordered (sort (copy-list entries) #'<
                       :key (lambda (entry) (getf entry :log-position))))
        (prompts (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (tools (make-hash-table :test #'equal))
        (out '()))
    (dolist (entry ordered)
      (if (equal (getf entry :kind) "user_message")
          (let ((parent (getf entry :parent-turn-id)))
            (setf (gethash (getf entry :turn-id) prompts) entry)
            ;; A prompt whose parent is not listed joins the root group: the children of NIL.
            (push (getf entry :turn-id) (gethash (and (gethash parent prompts) parent) children)))
          (push entry (gethash (getf entry :turn-id) tools))))
    (labels ((emit-row (entry depth lanes fork)
               (push (append entry (list :depth depth :lanes lanes :fork fork))
                     out))
             (emit-lines (ids depth lanes)
               ;; Every later child of one parent, each a line of its own one
               ;; lane deeper; the lane stays open while more follow. Then the
               ;; oldest, continuing this line at DEPTH.
               (loop for (id . more) on (rest ids)
                     do (emit id (1+ depth) (append lanes (list (and more t)))
                              t))
               (when ids
                 (emit (first ids) depth lanes nil)))
             (emit (id depth lanes fork)
               ;; ID's own rows — the prompt marked FORK when it opens a later
               ;; line — then the lines branched from it later, then the oldest
               ;; child continuing this line.
               (emit-row (gethash id prompts) depth lanes fork)
               (dolist (tool (reverse (gethash id tools)))
                 (emit-row tool depth lanes nil))
               (emit-lines (reverse (gethash id children)) depth lanes)))
      (dolist (entry ordered)
        (when (and (not (equal (getf entry :kind) "user_message"))
                   (not (gethash (getf entry :turn-id) prompts)))
          (emit-row entry 0 '() nil)))
      (emit-lines (reverse (gethash nil children)) 0 '()))
    (nreverse out)))
