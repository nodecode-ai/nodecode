;;;; client-session.lisp --- the running sessions, a session and its nodes, /sessions, attach-or-own. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the running sessions ----------------------------------------------------
;;; Which sessions have a turn in flight right now, and since when: the
;;; gateway's organism.activity frames (activity.lisp) -- the whole map each
;;; time it changes, and once more as this shell subscribes -- folded into a
;;; table of this shell's own. The directory /sessions lists is a durable
;;; read of what the store holds; this is the one live column painted over
;;; it, so a session nobody here started -- a room's, a cron job's, another
;;; shell's -- reads as busy in the same place, under the spinner its turn
;;; is running. Live-only: never replayed, and nothing dials for it.

;;; An open picker's fold holds the table itself, so rebuilding rows after a
;;; frame reads the map as it now stands; the keys are weak.
(defvar *session-activity* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: a shell's model -> its running-session table, session id ->
the ISO stamp the running turn's own durable fact carries.")

(defun session-activity-table (model)
  "MODEL's running-session table — every session with a turn in flight, and
when it started — made empty on first read: a shell that heard from nobody
has no session running."
  (alexandria:ensure-gethash model *session-activity* (make-hash-table :test #'equal)))

(defun ingest-session-activity (model payload &aux (table (session-activity-table model))
                                                   (next (make-hash-table :test #'equal)))
  "Fold one organism.activity frame into MODEL's table."
  ;; Every frame carries the whole map, so the table is rebuilt rather than
  ;; patched: a session whose turn settled leaves it here, and a row naming a
  ;; session twice keeps the last word. => (values TABLE MOVED-P): MOVED-P
  ;; says the map differs from the one the table held — the snapshot every
  ;; subscribe answers (the preview lane re-subscribes per highlight) mostly
  ;; carries the map the shell already holds, and re-laying a large directory
  ;; for it would cost time and a collection.
  (loop for row across (json-array payload "sessions")
        for id = (and (hash-table-p row) (gethash "id" row))
        for started = (and (hash-table-p row) (gethash "started_at" row))
        when (and (stringp id) (stringp started))
          do (setf (gethash id next) started))
  (let ((moved (not (equalp next table))))
    (when moved
      (clrhash table)
      (maphash (lambda (id started) (setf (gethash id table) started)) next))
    (values table moved)))

(defun refresh-sessions-dialog (model &key tick &aux (dialog model.dialog))
  "Draw an open /sessions picker again over the running-session table: the
directory rows re-laid from the fold, the running ones wearing the live
spinner and their clock, the age column re-read at the paint, and the
highlight reseated on the session it held."
  ;; On a TICK the paint lands only
  ;; when the spinner has moved to its next frame — the frame clock's own quantum
  ;; (STATUS-MARK) — so a dialog over a directory of settled sessions costs the
  ;; clock nothing, and the elapsed second rides the same paint.
  ;;
  ;; A box holding a query shows the gateway's conversation hits instead of
  ;; the directory: those rows are not a listing and carry no live column, so
  ;; this declines and leaves them as they are. => T when the rows were drawn
  ;; again.
  (when (and (picker-dialog-p dialog) (eq dialog.kind :sessions))
    (let ((state (gethash dialog *session-search*)))
      ;; A dialog whose directory read is still out holds no fold to lay
      ;; again: its rows are the read's to produce.
      (when (and state (getf state :fold) (string= "" (string-trim " " dialog.search)))
        (let* ((activity (session-activity-table model))
               (now-ms (iso-8601-ms (nlk::iso-now)))
               (frame (and (plusp (hash-table-count activity))
                           (floor now-ms *status-running-frame-ms*))))
          (unless (and tick (equal frame (getf state :activity-frame)))
            (let ((seat (picker-highlighted-value dialog)))
              (setf (getf state :activity-frame) frame
                    ;; The age column is a clock too: re-read it as the rows
                    ;; are laid again, or a dialog left open says just now
                    ;; hours later.
                    (sessions-fold-now-ms (getf state :fold)) now-ms
                    dialog.rows
                    (sessions-fold-rows (getf state :fold) activity)
                    ;; The running count's spinner turns with the rows'.
                    dialog.context
                    (sessions-dialog-context (getf state :fold) activity))
              (picker-reseat-selection dialog seat)
              ;; A tick's own frame repaints the chrome it asked for; the
              ;; rest of the time nothing else would.
              (when (and (not tick) model.band)
                (render-state model))
              t)))))))

(defun running-elapsed-ms (started now-ms &aux (started-ms (iso-8601-ms started)))
  "Milliseconds STARTED — the ISO stamp a running turn's own durable fact
carries — lies before NOW-MS, never negative: a start in the future (clock
skew across hosts) clamps to zero rather than running a clock backwards."
  ;; => the number, or NIL when STARTED does not parse.
  (and started-ms (max 0 (- now-ms started-ms))))

(defun running-clock-label (elapsed-ms &aux (seconds (floor elapsed-ms 1000)))
  "The running clock's text in the one dialect this shell's dialogs speak:
`42s', `3m 12s', `1h 04m'."
  ;; The /sessions directory's running row reads it, so a turn in flight is
  ;; clocked in this shell's words.
  (if (< seconds 60) (format nil "~ds" seconds) (format-turn-elapsed elapsed-ms)))

(defun session-picker-entry (entry)
  "One wire session row as the plist SESSION-TREE-ORDER walks and folds: the
three fields the walk reads, with the wire object itself kept under :ENTRY
for the row to read its title, directory, age and count from."
  (list :id (gethash "id" entry)
        :parent (json-value entry :string "parent_id")
        :updated-at (json-value entry :string "updated_at")
        :entry entry))

(defun session-picker-index (entries &aux (by-id (make-hash-table :test #'equal))
                                          (children (make-hash-table :test #'equal)))
  "The sessions dialog's fold indexes over the wire ENTRIES: session id ->
its entry, and session id -> the ids listing it as parent, as the listing
scanned them."
  ;; => (values BY-ID CHILDREN). A PARENT_ID no entry answers —
  ;; or a row parenting itself — indexes nothing; SESSION-TREE-ORDER reads such
  ;; a row as a root too.
  (dolist (entry entries)
    (setf (gethash (gethash "id" entry) by-id) entry))
  (dolist (entry entries)
    (when-let (parent (sessions-fold-parent by-id (gethash "id" entry)))
      (push (gethash "id" entry) (gethash parent children))))
  (values by-id children))

;;; --- a session and its nodes --------------------------------------------------
;;;
;;; One row per session, and the sessions that came off it hang under it
;;; exactly as the listing's own tree (SESSION-TREE-ORDER) lays them: its
;;; forks, its frames, and — while the team cell runs — the seats it
;;; started. The wire names the edge each node hangs by (RELATION):
;;; `inherited' for a fork that composes the session's history, `detached'
;;; for a node that began fresh. Nothing sits above a session: a session is
;;; the unit a person works in, and what came off it is its own branch.

(defun make-sessions-picker-fold (entries &optional (now-ms (iso-8601-ms (nlk::iso-now))))
  "The dialog's fold over a directory read: the listing, the index over its
PARENT_ID edges, and every session folded — the door state, one Right from
the nodes it holds. A reflection (source `experience', NLK::+NOTE-SOURCE+)
is the organism's own note on a turn, not a session anyone opens: it is
left out, as the web page leaves it, and counts as no fork."
  (setf entries (remove nlk::+note-source+ entries
                        :test #'equal :key (lambda (entry) (json-value entry :string "source"))))
  (multiple-value-bind (by-id children) (session-picker-index entries)
    (make-sessions-fold :entries entries :by-id by-id
                        :children children :now-ms now-ms)))

(defun session-picker-descendant-starts (children activity)
  "The running turns projected up their fork chains: id -> the ISO start of
the newest turn running anywhere UNDER it, for every session in ENTRIES
that is not running itself but has a running descendant — a fork or a spawn
at any depth."
  ;; A collapsed parent hides its branch whole, so this is how `▸ parent' says
  ;; that something inside it is live without the operator opening it
  ;; (SESSION-PICKER-ROW's dimmed mark). The walk reads the parent links off
  ;; CHILDREN, the listing's own PARENT_ID edges (SESSION-PICKER-INDEX), so a
  ;; listing a LIMIT cut short stops where its rows stop instead of inventing
  ;; ancestry, and a cycle ends at the first repeat. NIL or empty ACTIVITY is
  ;; no ancestors. => the table.
  (let ((table (make-hash-table :test #'equal)))
    (when (and activity (plusp (hash-table-count activity)))
      (let ((parent (make-hash-table :test #'equal)))
        (maphash (lambda (up kids) (dolist (kid kids) (setf (gethash kid parent) up))) children)
        (loop for id being the hash-keys of activity using (hash-value started)
              do (let ((seen (make-hash-table :test #'equal)))
                   (loop for up = (gethash id parent) then (gethash up parent)
                         while (and (stringp up) (not (gethash up seen)))
                         do (setf (gethash up seen) t)
                            (let ((held (gethash up table)))
                              (when (or (null held) (string< held started))
                                (setf (gethash up table) started))))))
        ;; A session running its own turn wears the live mark, never the dim
        ;; one: its own start is the clock its row paints.
        (loop for id being the hash-keys of activity do (remhash id table))))
    table))

(defun session-local-day (stamp &aux (ms (if (stringp stamp) (iso-8601-ms stamp) stamp)))
  "The local date STAMP -- an ISO instant, or epoch milliseconds -- falls on
(LOCAL-DATE's key), or NIL when it does not parse."
  (and ms (local-date (+ nlk:+unix-epoch+ (floor ms 1000)))))

(defun session-day-heading (day now-ms)
  "The heading over DAY's sessions: `Today', `Yesterday', else `Mon 13 Oct'.
It names the day and is never chosen; a rule runs from it to the edge."
  (let* ((now (+ nlk:+unix-epoch+ (floor now-ms 1000)))
         (label (cond ((equal day (local-date now)) "Today")
                      ((equal day (local-date (- now 86400))) "Yesterday")
                      (t (destructuring-bind (year month date) day
                           (nth-value 1 (local-date (encode-universal-time 0 0 12 date month year)))))))
         (row (make-picker-row :label label :heading t)))
    (setf (picker-row-paint row)
          (lambda-ignoring (selected room)
            (list (make-seg :text " " :tone :muted)
                  (make-seg :text label :tone :muted :bold t)
                  (make-seg :text " " :tone :muted)
                  (make-seg :text (make-string (max 0 (- room (text-display-width label) 2))
                                               :initial-element #\─)
                            :tone :scrollbar-rail))))
    row))

(defun session-picker-rows (entries children open now-ms &optional activity)
  "The sessions dialog's rows: the listing's own fork tree
(SESSION-TREE-ORDER), laid out again from the wire ENTRIES with the dialog's
fold — a session's forks are drawn only while its id is in OPEN, so a
collapsed session hides its branch whole and every lane describes the rows
actually on screen — under a heading for each day the sessions moved on."
  ;; CHILDREN (id -> the ids under it) supplies the forks a row counts.
  ;; ACTIVITY is the running-session table this shell holds
  ;; (SESSION-PICKER-ROW's clock and spinner); NIL draws a directory of
  ;; settled rows. A fork lists under its session's day, whatever day it moved
  ;; on itself: the day is a top-level row's. => picker rows, in drawing order.
  (let ((underway (session-picker-descendant-starts children activity))
        (day nil)
        (rows '()))
    (loop for row in (nlk:session-tree-order
                      (mapcar #'session-picker-entry entries)
                      :expanded (lambda (id) (and (gethash id open) t)))
          for id = (getf row :id)
          for kids = (gethash id children)
          do (when (zerop (or (getf row :depth) 0))
               (let ((moved (session-local-day (getf row :updated-at))))
                 (when (and moved (not (equal moved day)))
                   (setf day moved)
                   (push (session-day-heading day now-ms) rows))))
             (push (session-picker-row row now-ms
                                       :children (and kids (length kids))
                                       :day day
                                       :started-at (and activity
                                                        (gethash id activity))
                                       :descendant-started-at (gethash id underway))
                   rows))
    (nreverse rows)))

(defun session-picker-prefix (row updated-at now-ms &key (field :label) (offset 0) prefix)
  "Prefix ROW with PREFIX when non-NIL, otherwise its relative update age,
muted by the shared picker renderer."
  ;; A search's rows: its hits and the names that answer the query.
  (prefix-picker-row row (or prefix
                             (if (iso-8601-ms updated-at)
                                 (relative-age-label updated-at now-ms)
                                 "time unknown"))
                     :field field :offset offset))

(defparameter +session-ledger-sources+ '("channel" "cron" "team")
  "The sources a /sessions row names in a chip: a session a room, a cron job
or a team seat began. A shell's and the organism's own say nothing.")

;;; A room's record line opens on its speaker and the ids it carries --
;;; `<speaker> [m<message> u<user>]: <words>' (the channels kit, room.lisp) --
;;; which a first prompt cut at 64 characters is mostly made of, often to the
;;; point that the cut leaves the bracket open. A lane no name was given for
;;; is named by that prompt.
(defparameter +session-room-line+
  (ppcre:create-scanner "^([^\\[\\n]{0,64}?) *\\[m[0-9]+(?: u[0-9]+)?[^\\]\\n]*(?:\\]: *|$)")
  "The speaker tag a room's line opens on: the speaker, then the ids.")

(defun session-spoken (text)
  "TEXT as a row says it: a room's line as its words, or -- when the cut left
none -- its speaker; any other text as it is."
  (multiple-value-bind (from to starts ends) (and text (ppcre:scan +session-room-line+ text))
    (if from
        (let ((words (string-trim " " (subseq text to)))
              (speaker (subseq text (aref starts 0) (aref ends 0))))
          (cond ((plusp (length words)) words)
                ((plusp (length speaker)) speaker)))
        text)))

(defun session-age-label (updated now-ms day)
  "A /sessions row's age: how long ago UPDATED was, on a row of today or one
whose own day is not the DAY it lists under; the clock time it moved at
under an earlier day's heading, which already says the day."
  (let ((moved (session-local-day updated)))
    (cond ((null moved) "unknown")
          ((and (equal moved day)
                (not (equal moved (session-local-day now-ms))))
           (format-clock-minute (iso-8601-ms updated)))
          (t (relative-age-label updated now-ms)))))

(defun session-ledger-paint (&key mark mark-tone age age-tone indent name forks count source)
  "The painter (PICKER-ROW-PAINT) of one /sessions row: an edge, the MARK and
the AGE in a column of their own, the tree's INDENT and the NAME in the room
the rest leaves, and flush right the FORKS it holds, its COUNT of messages
and the SOURCE chip -- each dropping, the source first, when the row is too
narrow for the name to keep a reading's width."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (title (if selected :dialog-accent :system))
           (right (remove nil
                          (list (and forks
                                     (list (make-seg :text (format nil "~9@a" (format nil "~d fork~:p" forks))
                                                     :tone plain)))
                                (and count (plusp count)
                                     (list (make-seg :text (format nil "~9@a" (format nil "~d msg~:p" count))
                                                     :tone plain)))
                                (and source
                                     (list (make-seg :text (format nil " ~a " source)
                                                     :tone (style-for-tone :sheet-chip)))))))
           ;; The edge, a blank, the mark, a blank, the age, two blanks.
           (lead 14))
      (flet ((after (right) (if right (+ 2 (lane-width right)) 0)))
        ;; The right lane gives way before the name's reading width does, the
        ;; source first; then the name gives way, down to a word's width.
        (loop while (and right (< (- room lead 28 (after right)) 0))
              do (setf right (butlast right)))
        (let* ((right-width (lane-width right))
               (title-width (max 8 (- room lead (after right))))
               (left (append
                      (list (selection-edge-seg selected plain)
                            (make-seg :text " " :tone plain)
                            (make-seg :text (if (plusp (length mark)) mark " ") :tone mark-tone)
                            (make-seg :text " " :tone plain)
                            (make-seg :text (format nil "~8@a" (clip-text-to-width age 8))
                                      :tone (or age-tone plain))
                            (make-seg :text "  " :tone plain)
                            (make-seg :text indent :tone :muted)
                            (let* ((room (max 0 (- title-width (text-display-width indent))))
                                   (shown (clip-text-to-width name room)))
                              (make-seg :text (concatenate 'string shown
                                                           (make-string (max 0 (- room (text-display-width shown)))
                                                                        :initial-element #\Space))
                                        :tone title :bold selected))))))
          (values (append left
                          (when right
                            (cons (make-seg :text (make-string (max 2 (- room (segs-display-width left) right-width))
                                                               :initial-element #\Space)
                                            :tone plain)
                                  (lane-segs right))))
                  (and selected :panel-lift)))))))

(defun lane-width (lane)
  "The columns LANE, cells of segments a row lays two columns apart, spans."
  (loop for cell in lane
        for first = t then nil
        sum (+ (segs-display-width cell) (if first 0 2))))

(defun lane-segs (lane)
  "LANE's cells as one run of segments, two columns apart."
  (loop for cell in lane
        for first = t then nil
        unless first collect (make-seg :text "  " :tone :muted)
        append cell))

(defun session-picker-row (row now-ms &key children day started-at descendant-started-at)
  "One /sessions row, a ledger line: its age -- a running turn's clock in its
place -- its name, and flush right the forks it holds, its messages and where
it came from. DAY is the date of the heading it lists under (SESSION-AGE-LABEL)."
  ;; The row's slots keep what the box's own matcher reads -- the name -- and
  ;; the mark a test reads; PICKER-ROW-PAINT lays it.
  (let ((entry (getf row :entry)))
    (with-json ((id (gethash "id" entry))
                (title :text "title")
                (wd :text "working_directory")
                (updated :text "updated_at")
                (source :string "source")
                (count :integer "message_count")
                (indent (tree-lane-prefix (getf row :fork) (getf row :lanes)))
                (name (or (session-spoken title) id))
                (elapsed (and started-at (running-elapsed-ms started-at now-ms)))
                (underway (and (null elapsed) descendant-started-at
                               (running-elapsed-ms descendant-started-at now-ms)))
                (mark (cond (elapsed (status-mark elapsed))
                            (underway (status-mark underway))
                            (t "")))
                (picker-row (make-picker-row :mark mark
                                             :mark-tone (if elapsed :tool :muted)
                                             :label (concatenate 'string indent name)
                                             :path (or wd "")
                                             :value id)))
      entry
      (setf (picker-row-paint picker-row)
            (session-ledger-paint :mark mark :mark-tone (if elapsed :tool :muted)
                                  :age (if elapsed
                                           (running-clock-label elapsed)
                                           (session-age-label updated now-ms day))
                                  :age-tone (and elapsed :tool)
                                  :indent indent :name name
                                  :forks (and children (plusp children) children)
                                  :count count
                                  :source (find source +session-ledger-sources+ :test #'equal)))
      picker-row)))

(defstruct sessions-fold
  "The open /sessions dialog's fold: the listing it folds over (ENTRIES, the
wire rows), the indexes into it (BY-ID, id -> entry; CHILDREN, id -> the
ids under it), OPEN — the session ids whose forks are drawn, the one fact
the gestures move — and NOW-MS, the age column's clock, read once at open."
  ;; The dialog owns it: Esc drops it, and a reopened picker starts with every
  ;; parent collapsed.
  entries
  by-id
  children
  (open (make-hash-table :test #'equal))
  now-ms)

(nlk:access (fold sessions-fold))

(defun sessions-fold-parent (by-id id &aux (parent (gethash "parent_id" (gethash id by-id))))
  "The session ID hangs under in the listing BY-ID indexes, or NIL when it lists as
a root: its PARENT_ID, one the listing's own rows answer to and not the id itself."
  (and (stringp parent)
       (not (equal parent id))
       (gethash parent by-id)
       parent))

(defun sessions-dialog-context (fold activity)
  "The /sessions dialog's context label: how many sessions are running right
now, under the live spinner, or nothing -- the tabs beside it already count
the ones listed."
  (let ((started (loop for entry in fold.entries
                       for at = (and activity (gethash (gethash "id" entry) activity))
                       when at collect at)))
    (if started
        (format nil "~a ~d running"
                (status-mark (or (running-elapsed-ms (first started) (iso-8601-ms (nlk::iso-now))) 0))
                (length started))
        "")))

(defun sessions-fold-directory (fold id &aux (seen (make-hash-table :test #'equal)))
  "The folder ID's tree was opened in: the working directory of the session
FOLD's listing hangs it from at the top, spelled as WORKSPACE-DIRECTORY
spells a shell's (no trailing separator), or the empty string."
  ;; A fork, a frame and a team seat belong to the folder their tree began in,
  ;; whatever directory they run in themselves, so a tree is listed whole or
  ;; not at all. A cycle ends at its first repeat.
  (loop for up = (sessions-fold-parent fold.by-id id)
        while (and up (not (gethash up seen)))
        do (setf (gethash id seen) t id up))
  (string-right-trim "/\\" (or (json-value (gethash id fold.by-id) :string "working_directory") "")))

(defun sessions-fold-rows (fold &optional activity)
  "FOLD's rows as the dialog holds them: SESSION-PICKER-ROWS over its
listing and the sessions opened so far — a session's own nodes under it —
the running sessions among them drawn live off ACTIVITY (the shell's
running-session table). Each row's KEY is the folder its tree was opened in
(SESSIONS-FOLD-DIRECTORY), the bucket the folder tab selects."
  (let ((rows (session-picker-rows fold.entries fold.children fold.open fold.now-ms activity))
        (heading nil))
    (dolist (row rows rows)
      (if (picker-row-heading row)
          (setf heading row)
          (let ((folder (sessions-fold-directory fold (picker-row-value row))))
            (setf (picker-row-key row) folder)
            (when heading (pushnew folder (day-heading-folders heading) :test #'equal)))))))

(nlk:define-side-table day-heading-folders (:weakness :key)
  "A /sessions day heading -> the folders the sessions listed under it were
opened in (SESSIONS-FOLD-ROWS): the folder tab keeps a heading over its own
rows alone.")

(defun sessions-folder-tabs (fold &aux (here (workspace-directory)))
  "The /sessions tab strip: the sessions opened in this shell's folder, then
every session — each counting FOLD's listing, and no count while the read
that brings it is out."
  (list (list :label "This folder"
              :count (and fold (count here fold.entries
                                      :test #'equal
                                      :key (lambda (entry)
                                             (sessions-fold-directory
                                              fold (gethash "id" entry)))))
              :value here
              ;; A day's heading stands while a row under it is this folder's.
              :match (lambda (row folder)
                       (if (picker-row-heading row)
                           (member folder (day-heading-folders row) :test #'equal)
                           (equal folder (picker-row-key row)))))
        (list :label "All" :count (and fold (length fold.entries)) :value nil)))

(defun sessions-fold-key (model dialog fold direction &aux (id (picker-highlighted-value dialog)))
  "One Left/Right on the open /sessions picker — the fold the highlight
drives."
  ;; Right opens the highlighted session, the nodes it holds splicing in
  ;; under it: its forks, its frames, and the seats a team it opened is
  ;; running. Left closes the highlighted session's own nodes, or — holding
  ;; none open — closes the session it hangs under and parks the cursor
  ;; there. A gesture with nothing to fold declines, leaving the key the
  ;; no-op it was. The rows lay out again and the cursor seats on the session
  ;; the gesture left it on. => T when the fold moved.
  (when id
    (fold-arrow model direction id fold.open (gethash id fold.children)
                (sessions-fold-parent fold.by-id id)
                (lambda (seat)
                  ;; The rows under the dialog, laid out again with the fold
                  ;; as it now stands, the cursor back on SEAT's session.
                  (setf dialog.rows (sessions-fold-rows fold (session-activity-table model)))
                  (picker-reseat-selection dialog seat)))))

;;; --- a picked session attached ------------------------------------------------

(defun attach-picked-session (model value &aux (session-id (if (consp value) (car value) value)))
  "Enter on a /sessions row, VALUE its session id or a hit's
(SESSION-ID . POSITION): attach to that session, or say MODEL already is."
  ;; POSITION, which a conversation-search row carries, is the durable log
  ;; position of the block the search grabbed: the attached session rests on
  ;; that block rather than on the tail, which is the whole point of searching
  ;; your own conversation. The pin is armed before the replay's rows land and
  ;; applied again there (the replay replaces the preview's page with the
  ;; whole session, so the line index that holds now is not the one that holds
  ;; in a moment). The acknowledgement is composer status; => NIL.
  (if (equal session-id model.session-id)
      (progn (sessions-preview-clear model)
             (show-status model "Already attached to this session"))
      (switch-gateway-session model session-id))
  (when (consp value)
    (pin-transcript-position model session-id (cdr value)))
  nil)

;;; --- the pending-exec view -------------------------------------------------
;;; The band shows a pending exec in a five-line window, all it kept once a
;;; click opens it there; /execs opens the same object — the row's own
;;; retained output — at panel height, live while it runs, with the interrupt gesture on the exec it is
;;; about. One reader of the row's buffer, so the band and the panel can never
;;; disagree about what the exec printed. The row's own bounds are the
;;; buffer's (1 KiB head + 2 KiB tail); the omission line says how many of the
;;; stream's lines those bounds dropped. Its command heads it as the call
;;; wrote it, in the five-row window a card's output takes until Ctrl+O, and
;;; an exec that failed ends on the report of the condition it failed on.

(defun bg-row-output-lines (row &aux (window (background-kept-window row))
                                     (omitted (getf window :omitted 0)))
  "ROW's retained output as display lines for the panel => (values LINES
OMITTED TOTAL): BACKGROUND-KEPT-WINDOW, what an open band exec shows, its
omission clause a line of its own."
  (values (append (getf window :head)
                  (when (plusp omitted)
                    (list (format nil "⋮ +~d line~:p hidden" omitted)))
                  (getf window :tail))
          omitted
          (bg-row-written-lines row)))

(defun exec-view-rows (exec-id lines &optional report)
  "The panel's rows — one picker row per output line, then REPORT, the
condition the exec failed on, `error: ' first and in the danger tone — every
row's VALUE the exec id, which is the interrupt gesture's argument."
  (let ((report (split-lines report)))
    (append (loop for line in (or lines (and (null report) (list "(no output yet)")))
                  collect (make-picker-row :label line :value exec-id))
            (loop for line in report
                  for first = t then nil
                  collect (make-picker-row :label (if first (format nil "error: ~a" line) line)
                                           :tone :danger :value exec-id)))))

(defun exec-view-context (band live)
  "The view's header for BAND, its exec: running and how much it wrote while
LIVE, else how it ended (the exit's document, EXEC-END-WORDS) -- `exited 2',
not only that it did."
  ;; The title beside it already names the exec (`Exec 1000'), so the context
  ;; says only its state: it read `Exec 1000  exec 1000 · exited 2'.
  (if live
      (bind (((_ omitted total) (bg-row-output-lines band)))
        (format nil " running · ~d line~:p~@[ · +~d hidden~] "
                total (and (plusp omitted) omitted)))
      (format nil " ~a "
              (if-let (exited (bg-row-exited band))
                (exit-end exited t)
                "exited"))))

(defun exec-view-command (model band)
  "=> ROWS: the explicitly opened exec view shows its whole command."
  (values (command-rows band.command (max 8 (docked-panel-room model.term-width)) :open t)))

(defun exec-view-hints (live)
  "The explicit exec view's interrupt, close and scroll controls."
  (append (when live '(("ctrl+x" . "interrupt")))
          '(("esc" . "close") ("up/down" . "scroll"))))

(defun refresh-exec-view (model dialog band &aux (rows dialog.rows)
                                                  (live (find band (state-background-rows model.state))))
  "Re-read BAND, DIALOG's exec, into its rows, header and hints: live while
the band holds it, and once it left, how it ended with the interrupt gesture
gone. => T when the panel moved."
  (bind ((context (exec-view-context band live))
         (command (exec-view-command model band))
         (hints (exec-view-hints live))
         (new (exec-view-rows (bg-band-row-shell-session-id band) (bg-row-output-lines band)
                              (and (not live) (json-value (bg-row-exited band) :string "error")))))
    (unless live
      (setf dialog.on-delete nil))
    (unless (and (equal (mapcar #'picker-row-label rows) (mapcar #'picker-row-label new))
                 (equal context dialog.context)
                 (equal (mapcar #'segs-text command) (mapcar #'segs-text dialog.summary-rows))
                 (equal hints dialog.footer-hints))
      ;; The tail stays pinned only while the operator was already
      ;; there; a scrolled-up reader is not yanked back by new output.
      (let ((at-tail (>= dialog.selection
                         (1- (length rows)))))
        (setf dialog.rows new
              dialog.context context
              dialog.summary-rows command
              dialog.footer-hints hints)
        (when at-tail
          (setf dialog.selection
                (1- (length dialog.rows)))))
      t)))

(defun open-exec-view (model exec-id &aux (live (find-band-row model.state nil exec-id))
                                          (band (or live (find exec-id (state-ended-band-rows model.state)
                                                                 :key #'bg-band-row-shell-session-id))))
  "Open the exec view on EXEC-ID: its command, its retained output as the
panel's rows, refreshed on every tick while it runs, and how it ended once it
exited -- an exec that ended before the view opened included."
  ;; The view holds the row itself, so it still reads it once the band let it
  ;; go; a row the band holds again under the id (a resubscribe) takes over.
  ;; The refresh fills it, so it opens as a tick finds it.
  (open-picker-dialog
   model (format nil "Exec ~d" exec-id) (exec-view-rows exec-id nil) nil
   :kind :exec
   :context " exited "
   :empty-label "No output yet"
   :footer-hints (exec-view-hints live)
   :on-tick (lambda (model dialog)
              (setf band (or (find-band-row model.state nil exec-id) band))
              (when (and band (refresh-exec-view model dialog band))
                (render-status-frame model)
                t))
   :on-delete (and live
                   (lambda (model value)
                     (interrupt-band-exec model value)
                     (format nil "Interrupt requested (exec ~d)" value))))
  (when band
    (refresh-exec-view model (chat-model-dialog model) band))
  nil)

(defun slash-execs-op (model)
  "/execs: open the exec view, a running exec's live output -- Ctrl+X there
interrupts it -- or how one that ended here ended, with its last lines."
  ;; The view is the band row's retained output at panel height, live, with
  ;; the interrupt gesture.
  ;; => notice string, or NIL. One exec opens its view directly; more open the
  ;; picker, running first and then the ended, newest first, and Enter on a
  ;; row opens that exec's view (the dialog closes first, so a handler may
  ;; open its successor).
  (with-slash-notice ("execs")
    (let* ((running (remove-if-not (lambda (row) (eq :running (bg-band-row-status row)))
                                   (state-background-rows model.state)))
           (rows (append running (state-ended-band-rows model.state))))
      (cond
        ((null rows) "no exec has run here yet")
        ((null (rest rows))
         (open-exec-view model (bg-band-row-shell-session-id (first rows))))
        (t
         (open-picker-dialog
          model (format nil "Execs (~d)" (length rows))
          (loop for row in rows
                collect (make-picker-row
                         :label (command-preview (bg-band-row-command row))
                         :detail (format nil "exec ~d · ~a"
                                         (bg-band-row-shell-session-id row)
                                         (if (member row running)
                                             "running"
                                             (exit-end (bg-row-exited row) t)))
                         :value (bg-band-row-shell-session-id row)))
          (lambda (model id) (open-exec-view model id))
          :kind :execs
          :hint "enter expand  esc close  ↑↓ navigate"
          :empty-label "No execs"))))))

(defun leave-session-op (model)
  "Leave the attached session running: go home on a fresh session the way
/clear does, and open /sessions there with the highlight on the session just
left, once the directory read lands."
  ;; Nothing is marked: a session runs on whoever is attached, and /sessions
  ;; shows which are running. => notice string, or NIL.
  (with-slash-notice ("leave")
    (let ((id model.session-id))
      (slash-clear-op model)
      (slash-sessions-op model :seat id))))

;;; --- the /sessions transcript preview ------------------------------------
;;; The Zig sessions_dialog_preview lane (model/root.zig
;;; pendingGatewaySessionsDialogHistory + sessionsDialogPreview): the
;;; highlighted row's durable history is read once over the history pager,
;;; folded through the same frame authority the subscription replay uses,
;;; and substituted for the transcript underneath the dialog. Enter promotes
;;; the already-folded rows into the live state, so the transcript flips the
;;; moment the session is chosen instead of waiting on the replay that
;;; rebuilds it authoritatively.

(defun sessions-preview-clear (model)
  "Drop the armed preview, stop following its session — the attached
session alone again — and repaint the live transcript."
  ;; The viewport pin goes with it: a preview that ends takes the found-block
  ;; promise with it.
  (clear-transcript-pin model)
  (when model.preview-session-id
    (setf model.preview-session-id nil
          model.preview-activity nil)
    (when model.ws (send-subscribe model.ws (subscription-entries model))))
  (when model.transcript-preview (setf model.transcript-preview nil) (render-state model)))

(defun sessions-preview-install (model session-id entry position &key (follow t))
  "Seat ENTRY — a (SESSION-ID . STATE) page — under the dialog and repaint:
the window rests on POSITION's block when the row named one, else on the
newest rows as a preview always did. FOLLOW has the lane follow the session
live from here."
  ;; The newest rows are laid out by the paint that shows them
  ;; (RENDER-STATE-AT-PAINT), so a held arrow that crosses three rows between
  ;; two frames lays out one.
  (setf model.transcript-preview entry)
  (cond (position (pin-transcript-position model session-id position))
        (t (pin-transcript-bottom model)
           (render-state-at-paint model)))
  (when follow (preview-follow model session-id))
  model)

(defun sessions-preview-arm (model value &key memo (follow t) (position (and (consp value) (cdr value)))
                             &aux (session-id (if (consp value) (car value) value)))
  "Fold the durable history of VALUE's session into MODEL's preview lane and
repaint; FOLLOW has the lane follow it live at once."
  ;; VALUE is a row's value: a session id, or a conversation-search hit's
  ;; (SESSION-ID . POSITION), whose POSITION is the default below.
  ;; The attached session previews nothing — the transcript underneath already
  ;; is it — and a view already under the dialog never refetches. POSITION, on a
  ;; conversation-search row, is the durable log position of the block the row
  ;; matched: the page ENDS 64 events past that block (READ-SESSION-PAGE) and
  ;; the window rests on it
  ;; (PIN-TRANSCRIPT-POSITION) rather than on the newest rows — reading from the
  ;; block is what makes an old hit previewable at all, since the newest page
  ;; would not hold it and the context under it is what the operator lands in.
  ;; MEMO, when the
  ;; dialog passes one, is its read-ahead (SESSIONS-PREVIEW-READ-AHEAD), keyed
  ;; on (session . position): a row it already folded installs in this pass, a
  ;; row whose read is out installs when the page lands. A first visit reads
  ;; its page off the loop thread (READ-SESSION-PAGE, seated by
  ;; INSTALL-SESSION-PAGE): the read is a gateway round trip — a history page,
  ;; its hash check, JSON both ways — and a shell that waits for it on the loop
  ;; thread would answer no key until it lands. A failed read leaves the lane
  ;; empty so the next highlight retries (the Zig cleared-target re-selection);
  ;; no failure is kept as a page.
  (let ((key (cons session-id position)))
    (cond
      ((or (null session-id)
           (equal session-id model.session-id))
       (sessions-preview-clear model))
      ((and (equal session-id (car model.transcript-preview))
            (if position
                (equal key (gethash model *transcript-pins*))
                (null (gethash model *transcript-pins*))))
       (when follow (preview-follow model session-id)))
      ((and memo (consp (gethash key memo)))
       (sessions-preview-install model session-id (gethash key memo) position
                                 :follow follow))
      ;; The dialog's reads are the read-ahead's, which starts this row's
      ;; first: a held arrow outrunning them crossed rows faster than pages
      ;; land, and a read a row queued the gateway past a second. A landing
      ;; on a row whose read failed reads it again.
      ((null memo)
       (sessions-preview-read model key nil :follow follow))
      ((eq :failed (gethash key memo))
       (remhash key memo)))
    (when memo (sessions-preview-read-ahead model memo :follow follow))
    model))

;;; A /sessions dialog reads ahead: every landing, and every page that lands,
;;; tops up the reads of the rows nearest the highlight, so a held arrow
;;; arrives on rows already folded and each one paints in the frame that
;;; highlights it. Its memo holds a row's fold, :READING while the row's read
;;; is out, or :FAILED once it failed -- read again only when a landing asks,
;;; never by the read-ahead, so a gateway refusing every read is not asked
;;; again the moment it refuses -- and lets go of the oldest folds outside
;;; the reach: a fold is
;;; ~350 KB, and a dialog that kept every row it passed held them all until
;;; it closed.
(defparameter *sessions-preview-reach* 3
  "Rows on either side of the /sessions highlight whose pages are read ahead.")

(defparameter *sessions-preview-reads* 3
  "Preview pages one /sessions dialog reads at once.")

(defparameter *sessions-preview-kept* 16
  "Folded previews one /sessions dialog keeps.")

(defun sessions-preview-read (model key memo &key follow &aux (dialog model.dialog))
  "Read KEY's page off the loop and seat it (INSTALL-SESSION-PAGE); MEMO, when
given, holds :READING for KEY meanwhile, and the landing reads further ahead."
  ;; A page for a row the highlight has moved off changes nothing on screen,
  ;; so its landing owes no frame.
  (when memo (setf (gethash key memo) :reading))
  (off-loop model
            ;; A hit's page ends 64 events past its block: the events
            ;; strictly below its position + 65.
            (lambda () (read-session-page model (car key) (and (cdr key) (+ (cdr key) 65))))
            (lambda (model page)
              (prog1 (if (install-session-page model key memo page :follow follow) t :quiet)
                (when (and memo (eq dialog model.dialog))
                  (sessions-preview-read-ahead model memo :follow follow))))))

(defun sessions-preview-read-ahead (model memo &key follow &aux (dialog model.dialog))
  "Start reads for the rows within *SESSIONS-PREVIEW-REACH* of the open
dialog's highlight that MEMO holds nothing for, nearest first, while fewer
than *SESSIONS-PREVIEW-READS* are out; then drop the oldest folds outside
that reach past *SESSIONS-PREVIEW-KEPT*."
  (when (picker-dialog-p dialog)
    (let* ((at dialog.selection)
           (from (max 0 (- at *sessions-preview-reach*)))
           (near (stable-sort
                  (loop for row in (nthcdr from (picker-visible-rows dialog))
                        for index from from to (+ at *sessions-preview-reach*)
                        for key = (preview-row-key (picker-row-value row))
                        when (and key (not (equal (car key) model.session-id)))
                          collect (cons (abs (- index at)) key))
                  #'< :key #'car))
           (entries (alexandria:hash-table-values memo))
           (out (count :reading entries))
           (excess (- (count-if #'consp entries) *sessions-preview-kept*)))
      (loop for (nil . key) in near
            while (< out *sessions-preview-reads*)
            unless (gethash key memo)
              do (sessions-preview-read model key memo :follow follow)
                 (incf out))
      ;; A table walks in the order its keys went in, oldest first.
      (loop for key being the hash-keys of memo using (hash-value entry)
            while (plusp excess)
            when (and (consp entry) (not (rassoc key near :test #'equal)))
              do (remhash key memo)
                 (decf excess)))))

(defun read-session-page (model session-id before)
  "GET SESSION-ID's history page for the preview lane: the 128 events strictly
below log position BEFORE, the newest when NIL. => (FRAMES . OLDER), OLDER the
position the page behind it ends under, NIL when this one starts the session."
  ;; 128 is the page a held arrow reads one of a row. Without this limit,
  ;; the session_history_max_page_limit of 256 would make the frames under
  ;; it wait. 128 is the first page only; a rested highlight reads the
  ;; pages behind it (SESSIONS-PREVIEW-OLDER), and the replay an Enter
  ;; starts reads the rest.
  (let ((page (session-control-call model :get (format nil "history?limit=128~@[&before=~d~]" before)
                                    :session-id session-id)))
    (cons (gethash "frames" page)
          (and (gethash "has_more" page) (gethash "next_before" page)))))

(defun preview-row-key (value)
  "The (SESSION-ID . POSITION) a row whose value is VALUE previews, or NIL: a
directory row's value is its session id, a conversation-search hit's is
(SESSION-ID . LOG-POSITION)."
  (cond ((consp value) (cons (car value) (cdr value)))
        ((stringp value) (cons value nil))))

(defun preview-highlight-key (model &aux (dialog model.dialog))
  "The (SESSION-ID . POSITION) the open dialog's highlighted row previews, or NIL."
  (when (picker-dialog-p dialog)
    (preview-row-key (picker-highlighted-value dialog))))

(defun sessions-preview-follow-at-rest (model dialog)
  "Have the preview lane follow the session shown under DIALOG live once
DIALOG's highlight has rested *PICKER-HIGHLIGHT-REST*. => NIL: the follow
itself moves nothing on screen."
  ;; A subscription per row crossed was a gateway command on the loop -- a
  ;; store read an in-process gateway answers under the lock the read-ahead
  ;; holds -- and the attached session's own subscription re-answered each
  ;; time; the rows a held arrow crosses are read, not watched.
  (let ((shown (car model.transcript-preview)))
    (when (and shown
               (not (equal shown model.preview-session-id))
               (picker-highlight-rested-p dialog))
      (preview-follow model shown))
    nil))

(defun picker-highlight-rested-p (dialog)
  "Whether DIALOG's highlight has rested *PICKER-HIGHLIGHT-REST*."
  (>= (- (get-internal-real-time) dialog.last-highlight-at)
      (* *picker-highlight-rest* internal-time-units-per-second)))

;;; A preview starts as the newest page, 128 events, with nothing behind it:
;;; a page is counted in events where the view counts lines, and Standard
;;; folds a turn's tool trail to one row. The route answers where the page
;;; behind a page ends (next_before, has_more); the preview keeps that and
;;; every event its fold has taken beside the entry (PREVIEW-PAGING), and once
;;; the highlight rests reads the page behind whenever the fold projects less
;;; than a state's first projection does (*TAIL-PROJECTION-SCREENS* screens) or
;;; the operator has scrolled to its top. Without that read, a preview whose
;;; fold projects less than a state's first projection would stop scrolling
;;; before the oldest ask. A held arrow never pays for it: the page a row gets
;;; first is the 128 it always was, and a row the highlight has moved off reads
;;; no further.

(nlk:define-side-table preview-paging (:weakness :key)
  "Preview entry (SESSION-ID . STATE) -> a plist: :EVENTS every event folded
into its state, oldest first, in a vector that grows with the live follow;
:BEFORE the log position the page behind them ends under, NIL at the
session's start; :READING T while that page is out; :CHECKED how many events
the last look at the fold's length saw.")

(defun preview-page-entry (session-id page &key title position)
  "The preview entry for SESSION-ID's PAGE, a READ-SESSION-PAGE answer: its
fold, with the events behind it and where the page behind them ends kept.
TITLE is the name the directory lists the session under, POSITION the log
position of the hit the page was read around, NIL for the newest page."
  ;; The header shows the session, not the page (SESSION-DISPLAY-TITLE,
  ;; HEADER-STATUS-PARTS): a fold that does not hold the session's whole
  ;; history -- a page with one behind it, or one around a hit -- carries the
  ;; session's name rather than the first prompt it happens to start at, and
  ;; its usage lane waits for the gateway's sums (PREVIEW-INSTALL-HEADER).
  (let* ((events (frames-events (car page)))
         (state (fold-events events))
         (entry (cons session-id state))
         (partial (or (cdr page) position)))
    (setf (preview-paging entry)
          (list :events (make-array (length events) :adjustable t :fill-pointer t
                                                    :initial-contents events)
                :before (cdr page) :newest (null position)
                :reading nil :checked nil :header nil))
    (when partial (setf (gethash "usage_pending" state) t))
    (cond (title (setf (gethash "session_title" state) title))
          (partial (setf (gethash "session_title" state) "")))
    entry))

(defun preview-carry-slots (from to landed)
  "The slots a preview's state keeps beside its rows, FROM the state a refold
replaces TO the one it made: the name and the wait for the gateway's usage,
and with LANDED those sums themselves, which no page of rows recomputes."
  (dolist (key (if landed
                   '("session_title" "usage_pending" "session_usage" "context_usage")
                   '("session_title" "usage_pending")))
    (multiple-value-bind (value present) (gethash key from)
      (if present (setf (gethash key to) value) (remhash key to))))
  to)

(defun preview-keep-event (entry event)
  "Keep EVENT, which the live follow just folded into ENTRY's state, beside
the events behind it, so a page read later folds onto the same history."
  (when-let (paging (preview-paging entry))
    (vector-push-extend event (getf paging :events))))

(defun preview-wants-older-p (model entry &aux (window model.band) (paging (preview-paging entry)))
  "Whether ENTRY, the preview under MODEL's dialog, should read the page
behind it: the operator reads at the top of its fold, or its fold projects
fewer than *TAIL-PROJECTION-SCREENS* terminal heights of lines."
  (let ((rows (length (getf paging :events)))
        (start model.projection-start))
    (and window
         (or (and (not model.follow-bottom)
                  (< (band-window-offset window) window.height)
                  ;; Rows above the window that are not yet projected are the
                  ;; scroll's to grow (GROW-TRANSCRIPT-PROJECTION), not a page's.
                  start (eq (car start) (cdr entry)) (null (cdr start)))
             ;; The look lays the fold out, so it is taken once for each
             ;; event count rather than at every tick.
             (and (/= rows (or (getf paging :checked) -1))
                  (progn (setf (getf paging :checked) rows)
                         (with-render-scope (model)
                           (null (tail-projection-start
                                  (cdr entry) (projection-width model)
                                  (* *tail-projection-screens* model.term-height))))))))))

(defun install-older-page (model entry page)
  "Fold PAGE, the read behind ENTRY's oldest events, in front of them and
repaint when ENTRY is still the preview. => T when that moved the screen."
  ;; The fold goes again from the oldest event: the reducer folds forward
  ;; only. The live follow's events are among those kept, so a session that
  ;; ran while it was previewed keeps what it said. A failed read ends the
  ;; reach for this entry, so a tick does not ask again.
  (let ((paging (preview-paging entry)))
    (setf (getf paging :reading) nil
          (getf paging :checked) nil)
    (cond
      ((off-loop-failure page)
       (setf (getf paging :before) nil)
       nil)
      (t
       (let* ((older (frames-events (car page)))
              (events (getf paging :events))
              (all (make-array (+ (length older) (length events)) :adjustable t :fill-pointer t
                               :initial-contents (concatenate 'vector older events))))
         (setf (cdr entry) (preview-carry-slots (cdr entry) (fold-events all)
                                                (eq :landed (getf paging :header)))
               (getf paging :events) all
               (getf paging :before) (cdr page))
         ;; The newest page's history read to its start is the whole session:
         ;; its fold is exact, and its first prompt its name.
         (when (and (getf paging :newest) (null (cdr page)))
           (remhash "usage_pending" (cdr entry))
           (when (equal "" (gethash "session_title" (cdr entry)))
             (remhash "session_title" (cdr entry))))
         (when (eq entry model.transcript-preview)
           ;; A window that reads is on a fold projected whole (PREVIEW-WANTS-
           ;; OLDER-P), and the new fold's rows are new: it is projected whole
           ;; again, or the window would land at the top of one tail window.
           (keep-window-from-bottom
            model (lambda ()
                    (unless model.follow-bottom
                      (setf model.projection-start (cons (cdr entry) nil)))
                    (render-state model)))
           t))))))

(defun sessions-preview-older (model dialog &aux (entry model.transcript-preview)
                                                 (paging (and entry (preview-paging entry))))
  "Read the page behind the preview under DIALOG once its highlight rests, when
the preview wants it (PREVIEW-WANTS-OLDER-P), one page at a time. => NIL:
nothing moves on screen until the page lands."
  (when (and paging
             (getf paging :before)
             (not (getf paging :reading))
             (picker-highlight-rested-p dialog)
             (preview-wants-older-p model entry))
    (setf (getf paging :reading) t)
    (let ((before (getf paging :before)))
      (off-loop model
                (lambda () (read-session-page model (car entry) before))
                (lambda (model page)
                  (if (install-older-page model entry page) t :quiet)))))
  nil)

;;; The header of a shell shows the session it is on: its title, and its usage
;;; lane -- what the session has spent and how full its window is. A preview
;;; of a session that its page does not hold whole cannot sum that lane, so the
;;; gateway sums it (GET sessions/ID/header) once the highlight rests, and the
;;; numbers are seated on the previewed state, where the header reads them like
;;; an attached session's: the rounds the live follow folds past THROUGH, the
;;; newest round the gateway counted, add to them as they land.

(defun usage-slot-plist (object)
  "The usage plist a JSON usage slot (USAGE-SLOT-JSON's) stands for."
  (when (hash-table-p object)
    (loop for key being the hash-keys of object using (hash-value value)
          nconc (list (intern (string-upcase (substitute #\- #\_ key)) :keyword) value))))

(defun usage-event-position (event &aux (event (gateway-event-payload event)))
  "The log position of EVENT, one the reducer folds, when it is a turn.usage
fact; MOST-POSITIVE-FIXNUM for one that carries none, a fact not yet kept."
  (and (equal "turn.usage" (json-value event :any "type"))
       (or (json-value event :integer "metadata" "log_position") most-positive-fixnum)))

(defun preview-install-header (entry facts)
  "Seat FACTS, the gateway's answer for ENTRY's session, on its state: the
name, the session's usage through the newest round counted, and every round
the entry kept past that one folded on top."
  (let* ((state (cdr entry))
         (through (or (json-value facts :integer "through") 0))
         (sums (initial-chat-state))
         (title (json-value facts :string "title")))
    (setf (gethash "session_usage" sums) (usage-slot-plist (gethash "session_usage" facts))
          (gethash "context_usage" sums) (usage-slot-plist (gethash "context_usage" facts)))
    (loop for event across (getf (preview-paging entry) :events)
          for position = (usage-event-position event)
          when (and position (> position through))
            do (apply-gateway-event sums event))
    (setf (gethash "session_usage" state) (gethash "session_usage" sums)
          (gethash "context_usage" state) (gethash "context_usage" sums))
    (when (and title (plusp (length title)))
      (setf (gethash "session_title" state) title))
    (remhash "usage_pending" state)
    state))

(defun install-header-facts (model entry facts)
  "Take the gateway's header answer for ENTRY. => T when it moved the screen."
  ;; A failed read leaves the lane waiting, as a page that could not be read
  ;; leaves the preview empty: a wrong number is worse than none.
  (let ((paging (preview-paging entry)))
    (cond ((off-loop-failure facts) (setf (getf paging :header) :failed) nil)
          (t (preview-install-header entry facts)
             (setf (getf paging :header) :landed)
             (eq entry model.transcript-preview)))))

(defun sessions-preview-header (model dialog &aux (entry model.transcript-preview)
                                                  (paging (and entry (preview-paging entry))))
  "Ask the gateway for the header facts of the preview under DIALOG once its
highlight rests, when its fold cannot sum them. => NIL: nothing moves on screen
until they land."
  (when (and paging
             (gethash "usage_pending" (cdr entry))
             (null (getf paging :header))
             (picker-highlight-rested-p dialog))
    (setf (getf paging :header) :reading)
    (off-loop model
              (lambda () (session-control-call model :get "header" :session-id (car entry)))
              (lambda (model facts)
                (if (install-header-facts model entry facts) t :quiet))))
  nil)

(defun install-session-page (model key memo page &key (follow t))
  "Seat one preview page: fold it here — the loop thread's own fold, so the
reducer never runs beside a stream frame — keep it in MEMO, and install it
under the dialog when the highlight still rests on the row KEY was read for.
=> T when that moved the preview."
  ;; A page for a row the operator has moved off is memoized and otherwise
  ;; dropped, the way a search answer to a query the box has moved off is. A
  ;; failed read leaves the lane empty and MEMO holding no page for its row,
  ;; so the next highlight there reads it again.
  (cond
    ((off-loop-failure page)
     (when memo (setf (gethash key memo) :failed))
     (when (equal key (preview-highlight-key model))
       (sessions-preview-clear model)
       t))
    (t
     (let ((entry (preview-page-entry (car key) page :title (directory-title model (car key))
                                                     :position (cdr key))))
       (when memo (setf (gethash key memo) entry))
       (when (equal key (preview-highlight-key model))
         (sessions-preview-install model (car key) entry (cdr key) :follow follow)
         t)))))

(defun read-session-directory (model &aux (entries (coerce (gateway-model-http-call
                                                            model :get "/api/gateway/sessions" nil)
                                                           'list)))
  "GET the gateway's session directory. => its entries."
  ;; Every read that lands also replaces this shell's own copy
  ;; (*SESSION-DIRECTORY*), which is what the next keystroke opens on.
  (setf (gethash model *session-directory*)
        (list :entries entries :at (get-internal-real-time)))
  entries)

(defun slash-sessions-op (model &key seat)
  "/sessions, and the Ctrl+S chord over it: open the session picker. It opens
on the keystroke, and the directory read — a gateway round trip — happens on
a thread of its own and fills the dialog already on screen
(INSTALL-SESSION-DIRECTORY, OPEN-SESSIONS-PICKER-AWAITING). SEAT, a session
id, takes the highlight once its row is listed (Left's session just left)."
  ;; Nothing in the dialog's frame needs that answer, so nothing waits for it:
  ;; the notice a failed read owes lands in the composer status from the install
  ;; rather than as this op's answer. => NIL.
  (open-sessions-picker-now model :seat seat)
  ;; Closing or replacing this picker cancels its pending read's
  ;; UI effect. An old completion never reopens or fills another.
  (dialog-read model
               (lambda () (read-session-directory model))
               (lambda-ignoring (model dialog value)
                 (install-session-directory model value :seat seat)))
  nil)

;;; --- the /sessions conversation search --------------------------------------
;;; The picker's box is a title filter until the operator types in it; from
;;; there it is a grep over every session's conversation, answered by the
;;; gateway (POST /api/gateway/sessions/search). The box keeps one truth —
;;; what the operator typed — and the search hangs three facts off it: what is
;;; staged, what is in flight, and which query the rows on screen answer. Both
;;; live in a table keyed by the dialog rather than in slots, because a
;;; running image cannot grow a structure, and the dialog's own slots belong
;;; to the picker.
;;;
;;; A hit row's value is (SESSION-ID . LOG-POSITION) — the pair Enter needs to
;;; land on the block — while the directory's rows keep their plain session
;;; id. The two shapes are told apart where they are read, never guessed at.

;;; The dialog ranks nothing (the gateway answers newest first), so this is
;;; also how many hits a box shows, and a page that hit the bound says so with
;;; a `+'.
(defparameter *session-search-limit* 60
  "Rows one conversation search asks the gateway for.")

;;; The read is a store scan behind an HTTP round trip, and it runs on a
;;; thread of its own either way.
(defparameter *session-search-rest* 0.15
  "Seconds a typed query rests before the search fires: typing `docker' is one
search, not six.")

;;; :FOLD is the listing's fold (the rows an empty box shows), :STAGED the
;;; query the box holds now, :STAGED-AT when that query last changed, :FIRED
;;; the query a read is out for, :ROWS-QUERY the query the rows on screen
;;; answer (`' for the directory). The dialog takes its state with it: the
;;; keys are weak.
(defvar *session-search* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: the open /sessions dialog -> its search state, a plist.")

(defun read-session-search (model query)
  "POST the gateway's conversation search for QUERY => its rows."
  (gethash "rows" (gateway-model-http-call
                   model :post "/api/gateway/sessions/search"
                   (json-object "query" query "limit" *session-search-limit*))))

(defun session-search-picker-rows (json-rows fold &aux (now-ms (iso-8601-ms (nlk::iso-now))))
  "The conversation-search rows: one per matching block, the session's title
leading, the matched line next, the speaker and the block's age last."
  ;; A snippet is ONE line (NLK:ONE-LINE) — the row is a row whatever the
  ;; message held. Each row's value is (SESSION-ID . LOG-POSITION), the pair
  ;; the attach and the viewport pin read.
  (loop for row across (if (vectorp json-rows) json-rows #())
        for session-id = (json-value row :string "session_id")
        for seq = (json-value row :integer "seq")
        for at = (json-value row :text "occurred_at")
        for snippet = (nlk:one-line (json-value row :text "snippet"))
        for role = (json-value row :string "role")
        when (and session-id seq)
          collect (session-picker-prefix
                   (make-picker-row
                   :key (or (json-value row :text "title") session-id)
                   :label (or snippet "")
                   ;; How a hit names its speaker: `you' for the operator, `agent'
                   ;; for the answer, the tool's own name for tool traffic.
                   :detail (format nil "~a~@[ · ~a~]"
                                   (dispatch role equal ("user" "you") ("assistant" "agent")
                                     (t (or (json-value row :text "tool") "log")))
                                   (and at (relative-age-label at now-ms)))
                   :value (cons session-id seq))
                   (when-let (entry (and fold (gethash session-id (sessions-fold-by-id fold))))
                     (json-value entry :text "updated_at"))
                   now-ms :field :key)))

(defun session-name-picker-rows (fold query hits &aux (now-ms (iso-8601-ms (nlk::iso-now))))
  "The sessions in FOLD's listing whose name holds every word of QUERY and
that no row of HITS already finds, as rows leading the search's: a name the
operator gave is found by its words, which no conversation holds."
  (let ((words (split-words (string-downcase query)))
        (found (mapcar (lambda (row) (car (picker-row-value row))) hits)))
    (loop for entry in (and fold words fold.entries)
          for id = (gethash "id" entry)
          for title = (json-value entry :text "title")
          when (and title (not (member id found :test #'equal))
                    (every (lambda (word) (search word (string-downcase title))) words))
            collect (session-picker-prefix
                     (make-picker-row
                     :label title
                     :detail "name"
                     :value id) (json-value entry :text "updated_at") now-ms))))

(defun sessions-search-tick (model dialog)
  "The /sessions dialog's frame-clock tick, and the search box's own clock:
stage what the box holds, and once that has rested *SESSION-SEARCH-REST* ask
the gateway."
  ;; An emptied box needs no round trip — the session directory comes back at
  ;; once, the rows the dialog opened with, the fold's own. One read is out
  ;; per query: a keystroke landing while a read is in flight stages behind
  ;; it, and the answer to a query the box has moved off is dropped where it
  ;; lands. A live shell reads on a thread of its own — the read is a store
  ;; scan behind an HTTP round trip, and on the loop thread every frame would
  ;; wait for it. With no live program (tests drive this tick directly, and
  ;; must not spawn threads) the read and the install run inline. => T when
  ;; the rows moved (the directory came back under an emptied box); a fired
  ;; read answers by its own message.
  (when-let (state (gethash dialog *session-search*))
    (let ((query (string-trim " " dialog.search)))
      (unless (equal query (getf state :staged))
        (setf (getf state :staged) query
              (getf state :staged-at) (get-internal-real-time)))
      (cond
        ((or (member (getf state :staged) (list (getf state :rows-query) (getf state :fired)) :test #'equal)
             (< (since-ms (getf state :staged-at)) (* 1000 *session-search-rest*)))
         nil)
        ((zerop (length query))
         (let ((fold (getf state :fold)))
           (setf (getf state :rows-query) ""
                 dialog.rows (sessions-fold-rows
                                              fold (session-activity-table model))
                 dialog.context
                 (sessions-dialog-context fold (session-activity-table model)))
           (picker-seat-top dialog))
         (picker-rows-filtered-for dialog nil)
         (picker-notify-highlight model dialog :instant t)
         (render-state model)
         t)
        (t
         (setf (getf state :fired) query)
         (off-loop model
                   (lambda () (read-session-search model query))
                   (lambda (model value) (install-session-search model query value)))
         nil)))))

(defun install-session-search (model query value)
  "Install one conversation-search answer on the /sessions dialog QUERY was
read for."
  ;; An answer to a query the box has moved off is dropped: the box is the one
  ;; truth about what the operator is looking for. A failed read leaves the
  ;; rows where they were and says so beside the title.
  (let* ((dialog model.dialog)
         (state (and (picker-dialog-p dialog)
                     (eq :sessions dialog.kind)
                     (gethash dialog *session-search*))))
    (when (and state (equal query (getf state :fired)))
      (setf (getf state :fired) nil))
    (when (and state
               (equal query (string-trim " " dialog.search)))
      (if-let (failure (off-loop-failure value))
          (setf dialog.context
                (format nil " search failed: ~a" failure))
          (let* ((hits (session-search-picker-rows value (getf state :fold)))
                 (rows (append (session-name-picker-rows (getf state :fold) query hits) hits)))
            (setf dialog.rows rows
                  dialog.selection 0
                  dialog.view-offset 0
                  dialog.context
                  ;; `~:p' adds the `s' to the noun before it, so the noun
                  ;; has to be the singular that takes one (match, not matchs).
                  (format nil " ~d ~:[matches~;match~]~:[~;+~]"
                          (length rows) (= (length rows) 1)
                          (= (length hits) *session-search-limit*))
                  (getf state :rows-query) query)
            ;; The rows are the gateway's: the dialog shows them verbatim
            ;; (*PICKER-FILTERED-ROWS*) instead of re-filtering them.
            (picker-rows-filtered-for dialog query)
            ;; The first hit previews like any selection landing: the
            ;; transcript underneath rests on the block before Enter does.
            (picker-notify-highlight model dialog :instant t)))
      (render-state model))))

;;; Said on either tab: this folder's list is the one a picker opens on, and an
;;; empty store is the only way the other one is empty too.
(defparameter +sessions-empty-label+ "No sessions in this folder yet · tab shows all"
  "The /sessions body while its list holds no rows.")

(defun open-sessions-picker-dialog (model fold &key seat
                                              &aux (memo (make-hash-table :test #'equal)))
  "Open the /sessions dialog over FOLD's rows — none, under the reading copy,
while the directory read is still out and FOLD is NIL — and file its own
state beside it: FOLD, and MEMO, the preview read-ahead kept for its lifetime.
SEAT, a session id, takes the highlight when its row is listed."
  ;; Every landing is LIVE, the open's included: this picker's handler never
  ;; waits on the loop -- a row's history read (measured ~70 ms off a
  ;; 1600-session store) runs off it and was usually started rows before the
  ;; highlight arrived, and the fold it lands as is laid out by the paint
  ;; that shows it -- so a held arrow previews every row it crosses. Staged,
  ;; a scrub showed one row in three hundred milliseconds.
  ;; No title: the strip's chips say which list is on screen, and the
  ;; running count stands at the right of them.
  (open-picker-dialog
   model "" (if fold (sessions-fold-rows fold (session-activity-table model)) '())
   #'attach-picked-session
   :empty-label (if fold +sessions-empty-label+ "Reading the sessions…")
   :highlight-delivery :live
   :kind :sessions
   :searchable t
   ;; The fold: Left and Right open and close the highlighted session's forks
   ;; under it (the selection spends Up/Down alone). Every parent starts
   ;; collapsed — the door state — and the fold dies with the dialog. A box
   ;; holding a query shows the gateway's hits instead, which are no tree, so
   ;; the gesture declines there — and so does a fold that is not there yet
   ;; (the read still out: the rows arrive with it).
   :on-arrow (lambda (model dialog direction)
               (let ((fold (getf (gethash dialog *session-search*) :fold)))
                 (when (and fold (not (plusp (length (string-trim " " dialog.search)))))
                   (sessions-fold-key model dialog fold direction))))
   ;; Every selection landing — keys, search edits, hover — hydrates the
   ;; highlighted session into the transcript underneath, the way /themes
   ;; engages the highlighted palette; a hit also carries the block to rest
   ;; on. The memo lives as long as this dialog: scrubbing back through
   ;; the rows it holds costs render only, and closing the dialog drops it
   ;; so a reopened picker never shows a stale preview.
   :on-highlight (lambda (model row)
                   (sessions-preview-arm model (picker-row-value row) :memo memo :follow nil))
   :on-cancel #'sessions-preview-clear
   :on-rename #'sessions-rename-start
   ;; The dialog's clock, three readers: the box's own (the conversation search
   ;; fires once the query has rested), the running-session spinner, whose
   ;; paint lands each time it moves to its next frame, and the live follow of
   ;; the row the highlight rests on.
   :on-tick (lambda (model dialog &aux (moved (sessions-search-tick model dialog))
                                       (drawn (refresh-sessions-dialog model :tick t)))
              (sessions-preview-follow-at-rest model dialog)
              (sessions-preview-older model dialog)
              (sessions-preview-header model dialog)
              (or moved drawn))
   :search-placeholder "Search names and conversations"
   :no-match-label "No matching sessions"
   :footer-hints :sessions)
  ;; The context row names the directory's shape — the rows it holds and the
  ;; sessions running among them. A dialog the keystroke opened before its
  ;; read landed keeps the plain label until the read fills it
  ;; (OPEN-SESSIONS-PICKER).
  (let ((dialog model.dialog))
    (when (and fold (picker-dialog-p dialog))
      (setf dialog.context
            (sessions-dialog-context fold (session-activity-table model))))
    ;; The picker opens on this folder's sessions, and Tab reaches every one.
    (register-picker-tabs dialog (sessions-folder-tabs fold))
    ;; The seat is placed among the rows the open tab shows, and previewed:
    ;; SEAT's row, else the first session under the heading the list opens on.
    (when (and fold (picker-dialog-p dialog))
      (if (and seat (find seat (picker-visible-rows dialog) :key #'picker-row-value :test #'equal))
          (picker-reseat-selection dialog seat)
          (picker-seat-top dialog))
      (picker-notify-highlight model dialog)))
  ;; The dialog's own state, filed beside it — a table, not a slot: a running
  ;; image cannot grow a structure the picker already holds instances of.
  ;; Every key the dialog writes is born here: SETF of an absent plist key
  ;; conses onto a local binding nothing else holds, so a key added later
  ;; would never be seen again (the throttle below reads its own).
  (setf (gethash model.dialog *session-search*)
        (list :fold fold :staged "" :staged-at 0 :fired nil
              :rows-query "" :activity-frame nil))
  model)

;;; --- F2: a session renamed where it is listed ---------------------------------
;;; The field opens over the list on the highlighted row, directory row or
;;; search hit alike, holding its name; Enter posts it to the title route off
;;; the loop, and the name that route answers -- the one now in effect -- is
;;; written into the listing the dialog holds, which is also this shell's
;;; cached directory, so every row naming the session reads it at once.

(defun sessions-rename-start (model dialog)
  "F2 on /sessions: the highlighted session's name in the dialog's field."
  (declare (ignore model))
  (let* ((value (picker-highlighted-value dialog))
         ;; A directory row's value is its id, a search hit's (ID . SEQ).
         (id (if (consp value) (car value) value))
         (fold (getf (gethash dialog *session-search*) :fold))
         (entry (and fold (stringp id) (gethash id fold.by-id))))
    (when (stringp id)
      (setf dialog.input-label "Name"
            ;; A hit's KEY is its session's name; a directory row's is its folder.
            dialog.input (or (and entry (json-value entry :string "title"))
                             (and (consp value)
                                  (picker-row-key (selected-of (picker-visible-rows dialog)
                                                               dialog.selection)))
                             "")
            dialog.input-context "empty gives back the name its first prompt makes"
            ;; The sessions keys mean nothing to a field: its own take the footer.
            dialog.footer-hints nil
            dialog.hint "enter rename  esc keep the name  ctrl+u clear"
            dialog.on-input (lambda (model text) (sessions-rename-finish model dialog id text))))
    t))

(defun sessions-rename-finish (model dialog id text)
  "Enter (TEXT) or Esc (NIL) on the /sessions name field: back to the list,
and TEXT posted as ID's name off the loop."
  (setf dialog.input-label nil
        dialog.on-input nil
        dialog.hint nil
        dialog.footer-hints :sessions)
  (when text
    (off-loop model
              (lambda ()
                (json-value (session-control-call model :post "title" :session-id id
                                                  :body (json-object "title" text))
                            :string "title"))
              (lambda (model name) (install-session-name model dialog id name)))))

(defun install-session-name (model dialog id name)
  "Land the name the title route answered for ID in the /sessions DIALOG:
its listing's entry, its directory rows and the search hits on the session."
  (if-let (failure (off-loop-failure name))
      (show-status model (format nil "Rename failed: ~a" failure) :error-p t)
      (let* ((state (gethash dialog *session-search*))
             (fold (getf state :fold))
             (entry (and fold (gethash id fold.by-id)))
             (updated (and entry (json-value entry :text "updated_at")))
             (now-ms (iso-8601-ms (nlk::iso-now))))
        (when entry
          (setf (gethash "title" entry) name))
        (if (plusp (length (getf state :rows-query)))
            ;; The search's rows: a hit's KEY is its session's name, and a
            ;; name row is its name.
            (dolist (row dialog.rows)
              (let ((value (picker-row-value row)))
                (cond ((and (consp value) (equal id (car value)))
                       (setf (picker-row-key row) name)
                       (session-picker-prefix row updated now-ms :field :key))
                      ((equal id value)
                       (setf (picker-row-key row) "" (picker-row-label row) name)
                       (session-picker-prefix row updated now-ms)))))
            (when fold
              (let ((seat (picker-highlighted-value dialog)))
                (setf dialog.rows (sessions-fold-rows fold (session-activity-table model)))
                (picker-reseat-selection dialog seat))))))
  (render-state model))

;;; Every read of the directory lands here first (READ-SESSION-DIRECTORY), so
;;; the picker a keystroke opens has rows to draw at once — the fresh read is
;;; what catches it up, and the dialog is what it catches up in. The keys are
;;; weak: a switched-away shell takes its copy with it.
(defvar *session-directory* (make-hash-table :test #'eq :weakness :key)
  "Weak memo: an attached shell -> the session directory it last read, a
plist (:entries ... :at internal-time).")

(defun cached-session-directory (model)
  "The entries of MODEL's last directory read, or NIL when this shell has
never read one."
  (getf (gethash model *session-directory*) :entries))


(defun refresh-session-directory (model)
  "Warm the shared directory off-loop for the session picker.
One read in flight per shell, preserving cached rows until the answer lands."
  (unless (eq (getf (gethash model *session-directory*) :at) :reading)
    (setf (getf (gethash model *session-directory*) :at) :reading)
    (off-loop model
              (lambda () (read-session-directory model))
              (lambda (model value)
                (when-let (failure (off-loop-failure value))
                  (setf (getf (gethash model *session-directory*) :at) nil)
                  (show-status model (format nil "Sessions failed: ~a" failure) :error-p t))
                (if (off-loop-failure value) t :quiet)))))

(defun directory-title (model session-id)
  "SESSION-ID's name in MODEL's last directory read, or NIL."
  (json-value (find session-id (cached-session-directory model)
                    :key (lambda (entry) (json-value entry :any "id")) :test #'equal)
              :text "title"))

(defun open-sessions-picker-now (model &key seat &aux (entries (cached-session-directory model)))
  "Open the /sessions picker NOW, on the keystroke, and let the read catch it
up (INSTALL-SESSION-DIRECTORY)."
  ;; The read is a gateway round trip — ~150 ms against a store holding 1600
  ;; sessions — and the dialog is a frame nothing in that answer is needed
  ;; for: the tab row, the search box and the footer are this shell's own
  ;; code, and a directory this shell already read is rows on screen at once.
  ;; So the picker paints on the keystroke: with the last listing, or with the
  ;; reading copy when there is none yet, and the answer lands in the same
  ;; dialog either way.
  (open-sessions-picker-dialog model (and entries (make-sessions-picker-fold entries))
                               :seat seat)
  (setf (getf (gethash model.dialog *session-search*) :awaiting) t)
  model)

(defun sessions-picker-awaiting-p (model &aux (dialog model.dialog))
  "The /sessions dialog on screen, opened by a keystroke and waiting for the
directory read that fills it where it stands."
  (and (picker-dialog-p dialog)
       (eq dialog.kind :sessions)
       (getf (gethash dialog *session-search*) :awaiting)
       dialog))

(defun open-sessions-picker (model value &key seat)
  "Open the sessions picker on VALUE, the directory a read answered, the
highlight on SEAT's row when it is listed."
  ;; A picker the
  ;; keystroke already opened is filled where it stands — the dialog the operator
  ;; is looking at is the one the rows land in. Any other time (nothing open, or
  ;; a dialog that is not this picker) the picker opens here, as it always has.
  ;; => the notice when the directory could not be read or drawn, else NIL.
  (or (when-let (failure (off-loop-failure value))
        (format nil "Sessions failed: ~a" failure))
      (with-slash-notice ("sessions")
          (bind ((fold (make-sessions-picker-fold value))
                 (activity (session-activity-table model))
                 (awaiting (sessions-picker-awaiting-p model)))
            (if awaiting
                (let* ((state (gethash awaiting *session-search*))
                       ;; The row the dialog held: nothing while the reading
                       ;; copy showed, the operator's own seat when the
                       ;; listing on screen was this shell's last read --
                       ;; or the session the opener named.
                       (seat (or seat (picker-highlighted-value awaiting))))
                  (setf (getf state :fold) fold
                        (getf state :awaiting) nil
                        awaiting.rows
                        (sessions-fold-rows fold activity)
                        awaiting.context
                        (sessions-dialog-context fold activity)
                        awaiting.empty-label +sessions-empty-label+)
                  ;; The counts arrive with the rows, on the tab the operator
                  ;; may already have moved to while the read was out.
                  (register-picker-tabs awaiting (sessions-folder-tabs fold)
                                        :active (picker-dialog-tab awaiting))
                  (picker-reseat-selection awaiting seat)
                  ;; The open-time highlight, live like every landing: the
                  ;; highlighted session's history read starts off the loop
                  ;; here, and the rows paint without waiting on it.
                  (picker-notify-highlight model awaiting)
                  (when model.band (render-state model)))
                (open-sessions-picker-dialog model fold :seat seat))
            nil))))

(defun install-session-directory (model value &key seat)
  "Fill the pending /sessions dialog, or open one, on the directory VALUE a
read answered, the highlight on SEAT's row when it is listed."
  ;; its answer is only ever why the directory could not be read or drawn
  (when-let (notice (open-sessions-picker model value :seat seat))
    (show-status model notice :error-p t))
  (render-state model))

(defun switch-to-fresh-gateway-session (model &optional parent)
  "Create a fresh session at the gateway — a fork of PARENT when one is named,
which the server anchors at the parent's current head — and switch MODEL to
it. => NIL; the switch owns its composer status."
  ;; A fresh session opens in this shell's folder; a fork names none and works
  ;; where its parent does (NLK:STANDBY-SESSION).
  (switch-gateway-session
   model (gethash "session_id"
                  (gateway-model-http-call
                   model :post "/api/gateway/sessions"
                   (json-object "session_id" (fresh-session-id)
                                :opt "parent" parent
                                :opt "cwd" (and (null parent) (workspace-directory)))))))

(defun slash-clear-op (model)
  "Start a fresh session and switch to it; NIL on success, refusal text on failure."
  (with-slash-notice ("clear")
    (switch-to-fresh-gateway-session model)))

(defun slash-fork-op (model)
  "Fork a new session at the current head and switch to it."
  ;; Successful feedback is transient composer status; refusals still answer.
  (with-slash-notice ("fork")
    (let ((parent model.session-id))
      (switch-to-fresh-gateway-session model parent)
      (show-status model (format nil "Forked from ~a" parent))
      nil)))

(defun slash-evict-op (model percent &optional through)
  "Force a context eviction of PERCENT of the retained history, or of every
retained turn through the turn id THROUGH."
  ;; NIL PERCENT means the operator typed bare /evict: the route stays the
  ;; single owner of the 50% default. Returns notice text.
  (with-slash-notice ("evict")
      (let ((response (session-control-call
                       model :post "evict"
                       ;; Bare /evict posts an empty object: the route
                       ;; is the single owner of the 50% default.
                       :body (json-object :opt "percent" percent :opt "through" through))))
        ;; The Zig contextEvictMessage ack, counted in words: it reports
        ;; the realized eviction, not the request.
        (format nil "Evicted ~d turn~:p (~d% of history context) · ~d message~:p retained · recall: see log"
                (gethash "evicted_turn_count" response)
                (gethash "realized_percent" response)
                (gethash "retained_message_count" response)))))

;;; --- attach-or-own entry ------------------------------------------------
;;; Distilled from the Zig TUI's foreground-gateway ladder (src/tui/root.zig)
;;; and the database lifetime lease (src/db/lease.zig): exactly one process
;;; owns the store and serves it; every UI — including the owner's own — is a
;;; WebSocket client of that gateway. A second launch does not die on the
;;; ownership lock; it reads the endpoint the owner published in the store's
;;; holder sidecar and attaches.

(defun boot-owned-organism (&key session-id store-path)
  "The ownership boot spine shared by the foreground and headless serve
entries: take the store's ownership lock, seed the session, and raise the
floor."
  ;; The folder already loaded in MAIN, so the image is the composed self by
  ;; the time the lock is taken. Returns (VALUES SESSION-ID OPERATOR-TOKEN).
  ;; Signals NLK::STORE-IN-USE, store untouched, when a live process already
  ;; owns it.
  (let ((sid (or session-id (fresh-session-id)))
        ;; The token every local first-party surface shares, minted on first use.
        (token (or (gateway-configured-operator-token)
                   (let ((token (format nil "~(~{~2,'0x~}~)" (coerce (random-bytes 32) 'list))))
                     ;; The directory closes before the file exists, so no one else can
                     ;; open the token file while it is being written.
                     (write-secret-file (home "operator-token") token)
                     token))))
    (nlk:note-boot-phase "store-token")
    (nlk:open-store :path (or store-path nlk::*default-store-path*))
    (nlk:note-boot-phase "store-opened")
    ;; The boot session is minted standing by: it becomes durable with the
    ;; first block, so a boot nobody spoke in leaves no session at all. The
    ;; floor engagement below names it as the boot that engaged, and lands
    ;; either way (an organism fact asserts nothing about the session).
    (nlk:standby-session :id sid)
    (nlk:engage-floor :session-id sid)
    (values sid token)))

;;; Spelled the way FRESH-SESSION-ID spells the rest.
(defparameter +gateway-session-id+ "s-GATEWAY"
  "The session a gateway's boot names: one per home across every restart,
so a unit that comes up at each login and after each crash leaves one
session behind, not one a start.")

;; PAUSE bounds how long a shell's close leaves the store unowned, beside the boot.
(defun await-free-store (path &key (held-p #'nlk::store-lease-held-p) (pause 1) tick)
  "Return once no live process holds PATH's ownership lock — at once when
none does."
  ;; The gateway's patience: a shell that owns the store keeps it until it
  ;; closes, and the gateway takes the organism over then instead of losing an
  ;; election it was never meant to contest. Says once on stdout, the unit's
  ;; journal, whom it waits on. => how many looks it took.
  (loop for looks from 0
        while (funcall held-p path)
        do (when (zerop looks)
             (format t "~&waiting: pid ~a owns ~a; serving once it lets go~%"
                     (or (nlk::read-store-holder path) "?") (namestring path))
             (finish-output))
           (when tick (funcall tick))
           (sleep pause)
        finally (return looks)))

(defvar *gateway-run* nil
  "T in the process `nodecode gateway run' serves: the one the home's unit
runs, and brings back on `current' from any exit.")

;;; A headless owner has no shell to quit and, run by hand, no unit to restart
;;; it: the operator's stop and restart reach it here. The owner thread parks
;;; on the semaphore, so either one is taken within the moment, and both let
;;; the store go through the same cleanup a kill runs first.
(defvar *serve-next* nil
  "What the headless owner does once it lets the store go: NIL while it
serves; :STOP to exit, :RESTART to run its own command line again.")

(defvar *serve-wake* (bt2:make-semaphore :name "serve-wake")
  "What the headless owner parks on between watchdog beats.")

(defparameter *stop-seconds* 3
  "How long a headless owner's stop may run before it is cut short
(CUT-SHORT): a shell that relaunches waits on its store.")

(defvar *stopping* nil
  "T once this headless owner was told to stop.")

(defun cut-short ()
  "End this process now: everything it started killed, then gone without an
unwind -- the kernel frees the store's lock with it."
  ;; An exit that unwinds waits on every thread it cannot stop, up to SBCL's
  ;; exit timeout: a channel's read, an MCP server's pipe. Under a SIGTERM
  ;; that lands mid-stop, before CLOSE-STORE, that wait would hold the store
  ;; while a relaunched shell dials a gateway already gone.
  #-win32 (dolist (pid (proc-descendants (sb-posix:getpid)))
            (ignore-errors (sb-posix:kill pid sb-posix:sigkill)))
  (sb-ext:exit :code 143 :abort t))

(defun serve-next (what)
  "Have the headless owner WHAT (:STOP or :RESTART) once it lets the store go;
a stop that has not let it go within *STOP-SECONDS* is cut short (CUT-SHORT)."
  (when (and (eq what :stop) *serving* (not *stopping*))
    (setf *stopping* t)
    (spawn "stop-deadline" (sleep *stop-seconds*) (cut-short)))
  (setf *serve-next* what)
  (bt2:signal-semaphore *serve-wake*))

(defun stop-on-sigterm ()
  "SIGTERM stops this headless owner as a quit does (SERVE-NEXT :STOP), and
one that lands while it already stops cuts it short."
  ;; SBCL's own handler exits from wherever the signal lands, and from inside
  ;; STOP-GATEWAY that unwind skipped CLOSE-STORE. The work goes to a thread
  ;; of its own, never the handler's.
  #-win32 (sb-sys:enable-interrupt sb-unix:sigterm
                                   (lambda-ignoring (signal info context)
                                     (spawn "serve-sigterm"
                                       (if *stopping* (cut-short) (serve-next :stop))))))

(defun serve-organism-candidate (&key session-id store-path (port *gateway-port*) wait)
  "Boot the organism as a HEADLESS gateway owner, or lose the election
quietly."
  ;; This is the `nodecode serve` entry and the process a client's
  ;; resurrection rung spawns: probe the ownership lock first so a losing
  ;; candidate skips the boot, then boot and serve until killed. Returns :LOST
  ;; when another process owns the store — never an error, because losing the
  ;; election is the expected fate of most candidates.
  ;;
  ;; WAIT is the gateway's own — `nodecode gateway run', what its unit runs:
  ;; instead of losing, wait while another process owns the store (a shell the
  ;; operator keeps open) and take the organism over the moment that process
  ;; lets go (AWAIT-FREE-STORE); a race for it lost is waited out again, so it
  ;; never answers :LOST. Its boot names +GATEWAY-SESSION-ID+.
  ;;
  ;; The authoritative election is the fcntl lock inside OPEN-STORE; the probe
  ;; is only an economy. START-GATEWAY publishes the bound port in the holder
  ;; sidecar, which is how the clients that spawned this candidate find their
  ;; way back to it.
  (setf *gateway-run* (and wait t))
  (let ((path (merge-pathnames (or store-path nlk::*default-store-path*)))
        (session-id (or session-id (and wait +gateway-session-id+))))
    ;; Notify before waiting for a shell-owned store: the service is initialized
    ;; even while it is intentionally idle, and the same owner thread must keep
    ;; the watchdog alive in that wait.
    (nlk:systemd-ready)
    (format t "~&[CP-ISSUE-20260912-001] gateway ready~%")
    (let ((heartbeat-noted nil))
      (loop
        ;; A candidate that loses did not die: the receipt BOOT-CELLS left
        ;; before its load goes with it, or the launch after reads a self that
        ;; died before serving and runs pristine -- no cell, so no cell's
        ;; command. Most visible on Windows, where every serve loads directly
        ;; and so leaves one.
        (if wait
            (await-free-store path :tick #'nlk:systemd-watchdog)
            (when (nlk::store-lease-held-p path)
              (nlk:take-self-receipt)
              (return :lost)))
        (with-handlers ((nlk::store-in-use (refusal)
                          (format t "~&election lost: ~a~%" refusal)
                          (finish-output)
                          (unless wait
                            (nlk:take-self-receipt)
                            (return :lost))))
          ;; The models.dev catalog is parsed beside the boot, not by its
          ;; first reader: the first turn's model, /models and the setup
          ;; walk's import of keys all read it. A reader that comes while it
          ;; is in flight waits on its lock (READ-JSON-FILE-MEMO). The parse
          ;; conses 46 MB and keeps a fraction.
          (spawn "models-catalog"
            (nlk:with-nursery ((* 64 1024 1024))
              (ignore-errors (models-catalog-table))))
          (multiple-value-bind (sid token)
              (boot-owned-organism :session-id session-id :store-path path)
            (with-cleanup ((ignore-errors (stop-gateway))
                           (nlk:note-phase "stop store")
                           (ignore-errors (nlk:close-store))
                           (nlk:note-phase "stop done"))
              ;; The store's open and a warm image's folder walk overlapped;
              ;; a stale image does not serve (NLK:AWAIT-WARM-VERIFICATION).
              (nlk:await-warm-verification)
              ;; The boot's pages go back behind the peripherals, not here: a
              ;; trim's collection is up to 291 ms of stop-the-world, and here
              ;; it landed on the handshake of the shell attaching.
              (let ((bound (start-gateway :port port
                                          :operator-token token
                                          :trim t)))
                (format t "~&serving store ~a on port ~d (session ~s)~%"
                        (namestring path) bound sid)
                (finish-output)
                ;; What ends this process ends it within moments: a stop's
                ;; deadline, SIGTERM as a stop, and an exit that waits at most
                ;; a second on threads it cannot stop.
                (setf *serving* t
                      sb-ext:*exit-timeout* 1)
                (stop-on-sigterm)
                (watch-front)
                ;; Park; the gateway runs on hunchentoot threads and this
                ;; process lives until killed or asked (SERVE-NEXT). The
                ;; heartbeat is deliberately on this owner thread.
                (loop
                  (nlk:systemd-watchdog)
                  (unless heartbeat-noted
                    (setf heartbeat-noted t)
                    (format t "~&[CP-ISSUE-20260912-002] gateway heartbeat~%"))
                  (when *serve-next* (return))
                  (bt2:wait-on-semaphore *serve-wake*
                                         :timeout (/ nlk:*gateway-watchdog-seconds* 3)))))))
        ;; The store is let go by now: an exec keeps every open descriptor,
        ;; and a lease still held would be inherited into a deadlock. So
        ;; would a connection a page still holds, read by nobody after.
        (case *serve-next*
          (:stop (return :stopped))
          (:restart
           (dolist (name (nlk::directory-names #+linux "/proc/self/fd" #-linux "/dev/fd"))
             (let ((fd (parse-integer name :junk-allowed t)))
               (when (and fd (> fd 2)) (ignore-errors (sb-posix:close fd)))))
           (return (format t "~&restart refused: ~a~%"
                           (nlk::exec-program (nlk:organism-binary) sb-ext:*posix-argv*)))))))))

(defun attach-or-own-organism (&key session-id store-path setup prelude)
  "The binary's entry: paint the shell NOW, boot the organism BEHIND it."
  ;; Nothing in the first frame needs a gateway — the session id is
  ;; client-chosen, the band is in-memory, the composer placeholder is the
  ;; :connecting lifecycle — so the organism starts on the organism-boot
  ;; thread while the operator already sees and types into the shell. It runs
  ;; in a process of its own (FRONT-ORGANISM), and owner and attach are one
  ;; code path: whichever process wins the fcntl lock serves, and every shell —
  ;; the one that started it included — finds the endpoint through the holder
  ;; sidecar the reconnector polls.
  (let ((path (merge-pathnames (or store-path nlk::*default-store-path*))))
    ;; The line names the session the shell ended on, not the one it launched
    ;; with: a /sessions pick, a /fork or Left moves a shell off its first id.
    (let ((ended
            (run-chat-gateway
             :session-id (or session-id (fresh-session-id)) :store-path path :setup setup
             ;; Start the organism when the store is free, else attach to the live owner.
             :boot
             ;; The boot starts from the first moment, before the program is
             ;; built; SHELL, which answers the program once it exists, it has
             ;; no use for.
             (lambda-ignoring (shell)
               ;; PRELUDE is the folder decision when the caller has put it off until
               ;; the shell could paint (a TUI launch): it loads the folder here, or
               ;; replaces this process with a published image, or returns having
               ;; done one of them. The organism this thread goes on to boot is
               ;; still under the rule — nothing serves before the folder is
               ;; loaded — only the paint moved ahead of it. It is decided once:
               ;; the takeover rung boots this again in a process whose folder is
               ;; long loaded, and decided again, it hands a shell to a fresh image
               ;; from behind its frame, which then boots as a launch of its own.
               (when prelude (funcall (shiftf prelude nil)))
               ;; A warm image that vouched for itself answers before this shell
               ;; starts an organism from it: a stale one hands the process back.
               (nlk:await-warm-verification)
               (nlk:note-boot-phase "boot-start")
               ;; A gateway starting behind this launch — at login, after a crash,
               ;; in the gap an update's restart leaves — gets its moment to take the
               ;; store first: a shell that won that race would leave the gateway
               ;; waiting behind it, the wrong way round (AWAIT-GATEWAY-STORE).
               (await-gateway-store path)
               ;; A store nobody holds gets its organism from this shell, in a
               ;; process of its own (FRONT-ORGANISM); a held one is attached to
               ;; as it is, through the sidecar the reconnector polls.
               (unless (nlk::store-lease-held-p path)
                 (front-organism path)
                 (nlk:note-boot-phase "organism-serving"))))))
      ;; A shell that never attached has no session to resume (ENDED NIL),
      ;; and one whose own boot died was ended by it (CALL-ORGANISM-BOOT): the
      ;; launch is refused on its line -- MAIN prints `nodecode: ' and it, and
      ;; exits 1. A successor still runs inside the severance MAIN gave it, so
      ;; the line is said on the process's own stdio, not into the log.
      (cond (ended (nlk::call-with-unsevered-stdio
                    (lambda ()
                      (format t "~&To resume: nodecode resume ~a~@[ --store ~a~]~%"
                              ended (and store-path (namestring path))))))
            (*organism-boot-failure* (error "~a" *organism-boot-failure*))))))
