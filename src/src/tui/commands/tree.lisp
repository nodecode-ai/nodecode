;;;; tree.lisp --- /tree: files preview, the metro, the turn fold. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; The Zig checkpoint chrome: the selected /tree row lazily fetches and
;;; shows its changed files ("3 file changes: a.zig, b.zig, c.zig, +2
;;; more"), and picking a user-prompt row interposes the two-option
;;; "Restore checkpoint" dialog before any navigation fires.

(defun checkpoint-file-change-label (count)
  "\"N file change(s)\" — the Zig fileChangeLabel (singular drops the s)."
  (format nil "~d file change~:p" count))

(defun checkpoint-file-list-label (paths &key (max-paths 3))
  "Up to MAX-PATHS paths \", \"-joined with a \", +K more\" overflow tail
(the Zig session_dialogs fileListLabel)."
  (let* ((shown (subseq paths 0 (min max-paths (length paths))))
         (overflow (- (length paths) (length shown))))
    (format nil "~{~a~^, ~}~@[, +~d more~]"
            shown (and (plusp overflow) overflow))))

(defun checkpoint-files-suffix (count &optional paths)
  "The selected-row files chrome: \"  ·  N file changes\" bare until the
path list lands, then \": p1, p2, p3, +K more\" (Zig dialogs.zig selected
tree-row composition). Empty PATHS keep the bare count."
  (format nil "  ·  ~a~@[: ~a~]"
          (checkpoint-file-change-label count)
          (and paths (checkpoint-file-list-label paths))))

(defun checkpoint-row-ensure-files (row fetch &aux (value (picker-row-value row))
                                                   (checkpoint-id (getf value :checkpoint-id)))
  "The lazy files arm for one highlighted /tree ROW (Zig
pendingSessionTreeCheckpointFiles): rows without a capture or with zero
count never fetch; a loaded row never refetches; FETCH => (values files
ok) — a failed fetch (OK NIL) leaves the row bare so the next highlight
retries. => ROW."
  ;; A row with a capture is fetched whether or not the listing carried a
  ;; count: the files answer IS the count (|paths a rewind would touch|,
  ;; the same computation the listing used to run for every row), so this
  ;; one call lands the number and the list together and the confirm
  ;; stage's `(N files changed)' label reads what the rewind would do.
  (when (and checkpoint-id (not (getf value :files-loaded)))
    (multiple-value-bind (files ok) (funcall fetch checkpoint-id)
      (when ok
        (let ((count (length files)))
          (setf (getf (picker-row-value row) :files-loaded) t
                (getf (picker-row-value row) :files) files
                (getf (picker-row-value row) :file-change-count) count
                (picker-row-suffix row)
                ;; No files: no chrome (a bare `0 file changes' is chrome
                ;; the Zig never drew either).
                (if (plusp count)
                    (checkpoint-files-suffix count files)
                    ""))))))
  row)

(defun checkpoint-option-rows (value &aux (count (getf value :file-change-count)))
  "The moves a picked prompt row offers, in order: continue from the turn —
the head lands on it, its answer kept, so the next prompt branches from there
(absent on the head row: the session is already there) — then rewind to
before its message, the prompt back in the composer, and the same rewind
with the code changes when a capture recorded them (VALUE's
:FILE-CHANGE-COUNT pre-rendered, the static label while it is unknown)."
  (append
   (unless (getf value :head-p)
     (list (make-picker-row :label "Continue from this turn"
                            :detail "new turns branch from its answer"
                            :value (list :move :continue))))
   (list (make-picker-row :label "Rewind to before this message"
                          :detail "the prompt returns to the composer"
                          :value (list :move :rewind :restore-files nil)))
   (when (getf value :checkpoint-id)
     (list (make-picker-row
            :label (if (integerp count)
                       (format nil "Rewind to before this message + code changes (~d file~:p changed)"
                               count)
                       "Rewind to before this message + code changes")
            :value (list :move :rewind :restore-files t))))))

(defun open-checkpoint-dialog (model value move &key preview)
  "The /tree confirm stage over a picked prompt row: its moves as options,
Enter calling MOVE with the chosen kind (:CONTINUE or :REWIND) and its
RESTORE-FILES-P, Esc closing without moving. PREVIEW, when given, draws the
highlighted move: a function of the option's value answering the painted rows
that stand in the header's third row (TREE-MOVE-ROWS), drawn again as the
highlight moves."
  ;; VALUE is the picked row's value plist: its :DETAIL (`turn 3 · branch') is
  ;; the tab's context and its :SUMMARY the header's third row. => NIL.
  (let ((rows (checkpoint-option-rows value)))
    (apply #'open-picker-dialog
           model "Checkpoint" rows
           (lambda-ignoring (model option)
             (funcall move (getf option :move) (getf option :restore-files))
             nil)
           :context (format nil " ~a" (or (getf value :detail) ""))
           :subtitle (or (json-value (getf value :summary) :text)
                         "Continue from this turn, or rewind to before its message")
           ;; The option rows keep the `> ' gutter on every profile.
           :option-marker t
           :footer-hints :checkpoint
           (when preview
             ;; The picture is a pure read of the graph the tree opened on, so
             ;; every landing draws at once.
             (list :summary-rows (funcall preview (picker-row-value (first rows)))
                   :highlight-delivery :live
                   :on-highlight (lambda (model row)
                                   (when row
                                     (setf (picker-dialog-summary-rows (chat-model-dialog model))
                                           (funcall preview (picker-row-value row)))))))))
  nil)

(defun checkpoint-row-excerpt (text)
  "TEXT's first line cut at 48 columns — the /tree row's excerpt of a prompt
or of a tool command — and \"\" for anything that is not a string."
  (if (stringp text)
      (subseq text 0 (min (or (position #\Newline text) (length text)) 48))
      ""))

(defun tree-lane-prefix (fork lanes)
  "The tree glyphs in front of a /tree or /sessions row: one two-column cell
per lane the row sits inside (LANES, outermost first, T while that lane's
line has more forks below), `│ ' down an open lane and blank down a closed
one, except the innermost cell on a FORK row — where a later line leaves its
parent — which reads `├ ' with more forks below or `╰ ' as the last."
  ;; So off turn1: `├ yay', `│ no', `├ hello!', `│ bye!', `╰ no!' — three
  ;; lines, not five siblings; and off a session two forks were taken from, `├
  ;; fork', `╰ fork'.
  (format nil "~{~a~}"
          (loop for (open . more) on lanes
                collect (cond (more (if open "│ " "  "))
                              ((and fork open) "├ ")
                              (fork "╰ ")
                              (open "│ ")
                              (t "  ")))))

;;; --- the /tree metro ------------------------------------------------------
;;; The picture the /tree rows paint: the session's turns in the order they
;;; ran, one row each, on a graph of one rail per line -- the head's line the
;;; leftmost rail, a line branched off it a rail of its own to the right,
;;; joined to the turn it left by a fork -- with the turn's clock time ahead
;;; of the rails and, flush right, how it ended, the steers it took, its calls
;;; and how long it ran (no bar: a log-scaled one would draw short and long
;;; runs alike beside the time that already says how long it ran). A
;;; reflection's recap is its note on the turn it follows, never a turn of its
;;; own: it folds under that turn. The confirm stage draws the move it offers
;;; on the same graph (TREE-MOVE-ROWS). The rows keep their slots -- label,
;;; detail, value -- for the search, the tests and the moves; the paint
;;; (PICKER-ROW-PAINT) is only the look.

(defstruct metro-line
  "One line of the /tree graph: its LANE (0 is the head's line), whether it is
the head's line (MAIN), the node id it left (FROM), its node ids in run order
(NODES), and the rows it spans, START to END -- an END a later line's fork
stretches down to the row that line starts on."
  (lane 0) (main nil) (from nil) (nodes '()) (start 0) (end -1))

(defun metro-recap-p (entry)
  "Whether listing prompt ENTRY is a reflection's recap: a note on a turn."
  (equal "reflection" (gethash "origin" entry)))

(defun metro-cron (prompt)
  "A cron fire's (JOB . HH:MM) when PROMPT opens the way a cron prompt does --
the job's name quoted after `[cron', then `fired', a date and the minute --
else NIL."
  (when (and (stringp prompt) (alexandria:starts-with-subseq "[cron \"" prompt))
    (let* ((close (position #\" prompt :start 7))
           (clock (and close (+ close 19))))
      (when (and close
                 (eql close (search "\" fired " prompt :start2 close))
                 (< (+ clock 4) (length prompt))
                 (char= #\: (char prompt (+ clock 2))))
        (cons (subseq prompt 7 close) (subseq prompt clock (+ clock 5)))))))

(defun metro-clock (ms)
  "The local HH:MM of epoch milliseconds MS; NIL for NIL."
  (when ms
    (multiple-value-bind (second minute hour)
        (decode-universal-time (+ nlk:+unix-epoch+ (floor ms 1000)))
      (declare (ignore second))
      (format nil "~2,'0d:~2,'0d" hour minute))))

(defun metro-span-ms (entry)
  "Listing ENTRY's span in milliseconds, its STARTED_AT to its ENDED_AT; NIL
while it has no end (an absent end is no zero)."
  (let ((start (json-value entry :string "started_at"))
        (end (json-value entry :string "ended_at")))
    (let ((from (and start (iso-8601-ms start)))
          (to (and end (iso-8601-ms end))))
      (and from to (max 0 (- to from))))))

(defun metro-ask-key (node)
  "The opening of NODE's ask a retry repeats: its first 22 characters."
  (let ((prompt (or (json-value (getf node :entry) :string "prompt") "")))
    (subseq prompt 0 (min 22 (length prompt)))))

(defun metro-nodes (entries children &aux (prompts (make-hash-table :test #'equal))
                                          (by-id (make-hash-table :test #'equal))
                                          (nodes '()))
  "The /tree graph's turns over the listing ENTRIES, in the order they ran: a
plist per prompt row that is no recap -- :ID, :ENTRY, :PARENT (the nearest
listed turn above it that is no recap; NIL at a root, or under a turn the
listing's limit cut), :CALLS (CHILDREN's tool rows under it), :RECAPS (the
recap rows folded under it), :HEAD-P (it, or a recap of it, is the head),
:ON-CHAIN, :START (epoch ms), :SPAN (ms; NIL while it runs), :ENDING,
:STEERS, :CRON (METRO-CRON) and :RETRY (a cancelled ask its parent was asked
again after). A recap whose turn the listing does not carry stays a node.
=> (values NODES BY-ID), BY-ID the node of each id."
  (dolist (entry entries)
    (when (checkpoint-turn-entry-p entry)
      (setf (gethash (gethash "turn_id" entry) prompts) entry)))
  (flet ((up (entry)
           ;; The nearest listed ancestor that is no recap.
           (do ((parent (gethash "parent_turn_id" entry)
                        (gethash "parent_turn_id" (gethash parent prompts))))
               ((not (and (stringp parent) (gethash parent prompts))) nil)
             (unless (metro-recap-p (gethash parent prompts))
               (return parent)))))
    (dolist (entry entries)
      (when (checkpoint-turn-entry-p entry)
        (let ((id (gethash "turn_id" entry))
              (parent (up entry))
              (start (json-value entry :string "started_at")))
          (unless (and parent (metro-recap-p entry))
            (push (setf (gethash id by-id)
                        (list :id id :entry entry :parent parent
                              :calls (gethash id children) :recaps '()
                              :head-p (eq t (gethash "is_head" entry))
                              :on-chain (eq t (gethash "on_head_chain" entry))
                              :start (and start (iso-8601-ms start))
                              :span (metro-span-ms entry)
                              :ending (json-value entry :string "ending")
                              :steers (or (json-value entry :integer "steers") 0)
                              :cron (metro-cron (gethash "prompt" entry))
                              :retry nil
                              :position (or (json-value entry :integer "log_position") 0)))
                  nodes)))))
    ;; Each recap onto its turn, in the listing's order: a recap at the head
    ;; puts the head on its turn, the one the operator reads it under.
    (dolist (entry entries)
      (when (and (checkpoint-turn-entry-p entry) (metro-recap-p entry))
        (let ((node (gethash (up entry) by-id)))
          (when node
            (setf (getf node :recaps) (append (getf node :recaps) (list entry)))
            (when (eq t (gethash "is_head" entry))
              (setf (getf node :head-p) t)))))))
  (let ((nodes (stable-sort (nreverse nodes) #'< :key (lambda (node) (getf node :position)))))
    ;; A cancelled ask whose parent was asked the same again later says so.
    (dolist (node nodes)
      (when (and (equal "cancelled" (getf node :ending)) (getf node :parent)
                 (null (getf node :cron)))
        (let ((key (metro-ask-key node)))
          (setf (getf node :retry)
                (and (find-if (lambda (other)
                                (and (not (eq other node))
                                     (equal (getf other :parent) (getf node :parent))
                                     (> (getf other :position) (getf node :position))
                                     (equal key (metro-ask-key other))))
                              nodes)
                     t)))))
    (values nodes by-id)))

(defun metro-layout (nodes main-p &aux (index (make-hash-table :test #'equal))
                                       (holder (make-hash-table :test #'equal))
                                       (main (make-metro-line :main t))
                                       (lines (list main)))
  "NODES (METRO-NODES) on rails, a plist: :LINES (METRO-LINEs, the head's line
first), :HOLDER (node id -> its line) and :LANES (how many). A node MAIN-P
(called with its id) puts on the head's line stands on lane 0; any other
continues the line of the turn it left while that turn ends its line, else
starts a line of its own, in the first lane right of the head's whose line
ended above the row it starts on."
  (loop for node in nodes
        for i from 0
        for id = (getf node :id)
        do (setf (gethash id index) i)
           (let ((line (if (funcall main-p id)
                           main
                           (let* ((parent (getf node :parent))
                                  (up (and parent (gethash parent holder))))
                             (if (and up (not (metro-line-main up))
                                      (equal parent (first (metro-line-nodes up))))
                                 up
                                 (first (push (make-metro-line :from parent) lines)))))))
             (push id (metro-line-nodes line))
             (setf (gethash id holder) line)))
  (setf lines (nreverse lines))
  (dolist (line lines)
    (let ((ids (setf (metro-line-nodes line) (reverse (metro-line-nodes line)))))
      (when ids
        (setf (metro-line-start line) (gethash (first ids) index)
              (metro-line-end line) (gethash (car (last ids)) index)))))
  ;; The rail a line left runs on down to the row the line starts on.
  (dolist (line (rest lines))
    (let ((up (and (metro-line-from line) (gethash (metro-line-from line) holder))))
      (when up
        (setf (metro-line-end up) (max (metro-line-end up) (metro-line-start line))))))
  (let ((ends (make-array 1 :adjustable t :fill-pointer 1
                            :initial-element (metro-line-end main))))
    (dolist (line (rest lines))
      (let ((lane (loop for k from 1
                        while (and (< k (fill-pointer ends))
                                   (>= (aref ends k) (metro-line-start line)))
                        finally (return k))))
        (setf (metro-line-lane line) lane)
        (if (< lane (fill-pointer ends))
            (setf (aref ends lane) (metro-line-end line))
            (vector-push-extend (metro-line-end line) ends))))
    (list :lines lines :holder holder :lanes (fill-pointer ends))))

(defun metro-glyph (node main head)
  "NODE's mark on its rail, (GLYPH . TONE): the head's (its id HEAD), a failed
turn's, a cancelled one's, else one on the head's line (MAIN) or off it."
  (cond ((equal (getf node :id) head) (cons "◉" :tool))
        ((equal (getf node :ending) "failed") (cons "✕" :diff-removed))
        ((equal (getf node :ending) "cancelled") (cons "◦" :muted))
        (main (cons "●" :dialog-accent))
        (t (cons "○" :success))))

(defun metro-cells (layout i node &key sub head)
  "The graph cells of row I, NODE's, two columns a lane, as (TEXT . TONE): a
rail down every line spanning the row, NODE's mark on its own, and on the row
a line starts the fork joining it to the rail it left. SUB draws a row hung
under row I instead: the rails running on below it. HEAD is the head's id."
  (let* ((holder (getf layout :holder))
         (own (gethash (getf node :id) holder))
         (cells (make-array (* 2 (getf layout :lanes)) :initial-element (cons " " nil))))
    (flet ((rail (line) (if (metro-line-main line) :list-mark :scrollbar-rail)))
      (dolist (line (getf layout :lines))
        (when (and (metro-line-nodes line)
                   (<= (metro-line-start line) i (metro-line-end line)))
          (let ((column (* 2 (metro-line-lane line))))
            (cond ((and (not sub) (eq line own))
                   (setf (aref cells column) (metro-glyph node (metro-line-main line) head)))
                  ((if sub (< i (metro-line-end line)) (> i (metro-line-start line)))
                   (setf (aref cells column) (cons "│" (rail line))))))))
      (let ((up (and (not sub) own (not (metro-line-main own))
                     (equal (getf node :id) (first (metro-line-nodes own)))
                     (metro-line-from own)
                     (gethash (metro-line-from own) holder))))
        (when up
          (let ((from (* 2 (metro-line-lane up)))
                (to (* 2 (metro-line-lane own)))
                (last (>= i (metro-line-end up))))
            (setf (aref cells from)
                  (cons (if (> from to) (if last "╯" "┤") (if last "╰" "├")) (rail up)))
            (loop for x from (1+ (min from to)) below (max from to)
                  do (setf (aref cells x) (cons "─" :scrollbar-rail)))))))
    (coerce cells 'list)))

(defun metro-cell-segs (cells)
  "CELLS (METRO-CELLS) as segments."
  (loop for (text . tone) in cells
        collect (make-seg :text text :tone (or tone :muted))))

(defun metro-label-text (node)
  "NODE's words on a row: its ask's excerpt, or a cron fire's job and minute."
  (let ((cron (getf node :cron)))
    (if cron
        (format nil "cron ~a · fired ~a" (car cron) (cdr cron))
        (checkpoint-row-excerpt (gethash "prompt" (getf node :entry))))))

(defun metro-row (left label suffix words columns room selected)
  "One painted /tree row ROOM wide: LEFT, then LABEL -- SUFFIX after it, the
selected row's files chrome -- in the room the right lane leaves, then the right
lane flush right: WORDS (cells of segments, two columns apart) ahead of the
fixed COLUMNS. The WORDS give way first, then the COLUMNS, before the label goes
under twelve columns. => (values SEGS GROUND)."
  (let ((plain (if selected :system :muted))
        (lead (segs-display-width left)))
    (flet ((right-width ()
             (+ (if words (+ (lane-width words) 2) 0) (segs-display-width columns))))
      (loop while (and words (< (- room lead 1 (right-width)) 12))
            do (setf words (butlast words)))
      (when (< (- room lead 1 (right-width)) 12)
        (setf columns '()))
      (let* ((right (append (when words
                              (append (lane-segs words) (list (make-seg :text "  " :tone plain))))
                            columns))
             (open (max 0 (- room lead (segs-display-width right) (if right 1 0))))
             (shown (clip-segs label (if suffix
                                         (max (min 12 open) (- open (text-display-width suffix)))
                                         open)
                               :mark *dialog-row-overflow-mark*))
             (tail (and suffix (clip-text-to-width suffix (max 0 (- open (segs-display-width shown))))))
             (body (append left shown
                           (when (plusp (length tail)) (list (make-seg :text tail :tone :muted))))))
        (values (append body
                        (list (make-seg :text (make-string (max (if right 1 0)
                                                                (- room (segs-display-width body)
                                                                   (segs-display-width right)))
                                                           :initial-element #\Space)
                                        :tone plain))
                        right)
                (and selected :panel-lift))))))

(defun metro-node-paint (row node cells &key clock repeat (arrow "  ") head)
  "The painter (PICKER-ROW-PAINT) of NODE's /tree row ROW: the edge, its CLOCK
time (blank under a row of the same minute, REPEAT, unless selected), its graph
CELLS, the HEAD tag on the head's row, the fold's ARROW, its ask -- a cron
fire's job and minute -- and, flush right, how it ended, its steers, its calls,
how long it ran, and a pen when a recap folds under it. The
selected row carries ROW's files chrome after the ask."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (quiet :sheet-undone)
           (ending (getf node :ending))
           (span (getf node :span))
           (calls (length (getf node :calls)))
           (steers (getf node :steers))
           (cron (getf node :cron))
           (left (append
                  (list (if selected
                            (make-seg :text "▌" :tone :dialog-accent)
                            (make-seg :text " " :tone plain))
                        (make-seg :text " " :tone plain)
                        (make-seg :text (if (and clock (or selected (not repeat))) clock "     ")
                                  :tone (if selected :system quiet))
                        (make-seg :text "  " :tone plain))
                  (metro-cell-segs cells)
                  (list (make-seg :text " " :tone plain))
                  (when head
                    ;; A chip stands on its own ground, so its tone is the
                    ;; resolved style (a keyword tone takes the row's).
                    (list (make-seg :text " HEAD " :tone (style-for-tone :sheet-chip-on) :bold t)
                          (make-seg :text " " :tone plain)))
                  (list (make-seg :text arrow :tone quiet))))
           (label (if cron
                      (list (make-seg :text " cron " :tone (style-for-tone :sheet-chip-set))
                            (make-seg :text (format nil " ~a" (car cron)) :tone :system)
                            (make-seg :text (format nil " · fired ~a" (cdr cron)) :tone quiet))
                      (list (make-seg :text (metro-label-text node)
                                      :tone (cond (head :title)
                                                  ((equal ending "cancelled") quiet)
                                                  ((getf node :on-chain) :foreground)
                                                  (t :muted))
                                      :bold head))))
           (suffix (let ((text (picker-row-suffix row)))
                     (and selected (plusp (length text)) text)))
           (words (remove nil
                          (list (cond ((getf node :retry)
                                       (list (make-seg :text "re-asked ↓" :tone quiet :italic t)))
                                      ((equal ending "failed")
                                       (list (make-seg :text "failed" :tone :diff-removed)))
                                      ((equal ending "cancelled")
                                       (list (make-seg :text "cancelled" :tone quiet)))
                                      ((and (getf node :start) (null ending))
                                       (list (make-seg :text "open" :tone quiet))))
                                (and (plusp steers)
                                     (list (make-seg :text (format nil "+~d steer~:p" steers)
                                                     :tone :syntax-command))))))
           (columns (list (make-seg :text (format nil "~8@a" (if (plusp calls) (format nil "~d call~:p" calls) ""))
                                    :tone quiet)
                          (make-seg :text (format nil "  ~7@a" (if span (format-turn-elapsed span) ""))
                                    :tone plain)
                          (make-seg :text (if (getf node :recaps) "  ✎" "   ") :tone quiet)
                          (make-seg :text " " :tone plain))))
      (metro-row left label suffix words columns room selected))))

(defun metro-call-paint (entry cells)
  "The painter (PICKER-ROW-PAINT) of a call's /tree row, listing ENTRY, hung
under its turn: the rails running on (CELLS), the tool's name and what it was
asked to do, and flush right how long the call ran."
  (lambda (selected room)
    (let* ((plain (if selected :system :muted))
           (span (metro-span-ms entry))
           (left (append (list (if selected
                                   (make-seg :text "▌" :tone :dialog-accent)
                                   (make-seg :text " " :tone plain))
                               (make-seg :text "        " :tone plain))
                         (metro-cell-segs cells)
                         (list (make-seg :text " " :tone plain)
                               (make-seg :text "    ┆ " :tone :scrollbar-rail))))
           (label (list (make-seg :text (format nil "~a  " (or (json-value entry :text "tool_name") "tool"))
                                  :tone :tool)
                        (make-seg :text (or (json-value entry :text "tool_command") "")
                                  :tone (if selected :foreground :muted))))
           (columns (list (make-seg :text (format nil "~7@a" (if span (format-turn-elapsed span) ""))
                                    :tone :sheet-undone)
                          (make-seg :text "    " :tone plain))))
      (metro-row left label nil nil columns room selected))))

(defun metro-recap-row (entry cells)
  "The /tree row of recap ENTRY folded under its turn: a heading -- the arrows
pass over it -- of the rails running on (CELLS), a pen, and what the recap
said, or when it ran while the listing carries no words of it."
  (let* ((said (or (json-value entry :text "answer")
                   (metro-clock (let ((start (json-value entry :string "started_at")))
                                  (and start (iso-8601-ms start))))
                   ""))
         (row (make-picker-row :label (format nil "recap ~a" said) :heading t)))
    (setf (picker-row-paint row)
          (lambda (selected room)
            (declare (ignore selected))
            (values (clip-segs (append (list (make-seg :text "         " :tone :muted))
                                       (metro-cell-segs cells)
                                       (list (make-seg :text " " :tone :muted)
                                             (make-seg :text "    ✎ recap  " :tone :sheet-undone)
                                             (make-seg :text said :tone :muted :italic t)))
                               room :mark *dialog-row-overflow-mark*)
                    nil)))
    row))

(defun metro-chain (by-id id &aux (chain (make-hash-table :test #'equal)))
  "ID and every turn above it by BY-ID's parents: the line a head on ID stands
on, as a set."
  (loop for node = (gethash id by-id) then (gethash (getf node :parent) by-id)
        while node
        do (setf (gethash (getf node :id) chain) t))
  chain)

(defun tree-move-name (node)
  "How a move names NODE's turn: `turn 4', `the 14:31 cron fire', `the wake'."
  (let ((entry (getf node :entry))
        (cron (getf node :cron)))
    (cond (cron (format nil "the ~a cron fire" (cdr cron)))
          ((gethash "turn_number" entry) (format nil "turn ~a" (gethash "turn_number" entry)))
          ((equal "wake" (gethash "origin" entry)) "the wake")
          (t "this turn"))))

(defun tree-move-sentence (node option value leaving joining)
  "OPTION's move on NODE, the turn of picked row VALUE, in a sentence: where
the head goes, the turns it takes off the head's line (LEAVING) and puts on it
(JOINING), and the files."
  (let ((name (tree-move-name node))
        (count (length leaving)))
    (if (eq (getf option :move) :continue)
        (format nil "Continue from ~a: the head moves here~@[; ~a~]~@[; ~a~]."
                name
                (when leaving
                  (let ((calls (reduce #'+ leaving :key (lambda (turn) (length (getf turn :calls)))))
                        (ms (reduce #'+ leaving :key (lambda (turn) (or (getf turn :span) 0)))))
                    (format nil "~d turn~:p (~d call~:p~@[, ~a~]) stay~:[s~;~] as a branch"
                            count calls (and (plusp ms) (format-turn-elapsed ms)) (/= count 1))))
                (when (> (length joining) 1)
                  (format nil "~d turns join the head line" (length joining))))
        (format nil "Rewind to before ~a: its ask returns to the composer; ~d turn~:p leave~:[s~;~] the head line (/redo returns); ~a."
                name count (/= count 1)
                (let ((files (getf value :file-change-count)))
                  (cond ((not (getf option :restore-files)) "files stay as they are")
                        ((integerp files) (format nil "~d file~:p restored" files))
                        (t "its files restored")))))))

(defun tree-move-rows (state value option room)
  "The confirm stage's picture of OPTION, a move on the turn whose row VALUE
was picked: up to five rows of STATE's /tree graph around that turn, relaid
with the head's line where the move leaves it -- the turn tagged `here'
(continue) or `before this' (rewind), each row the move takes off the head's
line `leaves head', each it puts on `joins head', the head it lands on `HEAD'
-- then a blank and the move in a sentence, two rows, so every option draws as
many rows. => painted rows, ROOM wide; NIL for a turn the graph does not hold."
  (let* ((nodes (getf state :nodes))
         (by-id (getf state :by-id))
         (node (gethash (getf value :turn-id) by-id)))
    (when node
      (let* ((continue (eq (getf option :move) :continue))
             (head (if continue (getf node :id) (getf node :parent)))
             (chain (metro-chain by-id head))
             (layout (metro-layout nodes (lambda (id) (gethash id chain))))
             (leaving (remove-if-not (lambda (turn) (and (getf turn :on-chain)
                                                         (not (gethash (getf turn :id) chain))))
                                     nodes))
             (joining (remove-if (lambda (turn) (or (getf turn :on-chain)
                                                    (not (gethash (getf turn :id) chain))))
                                 nodes))
             (at (position node nodes))
             ;; Five rows, the sentence's two and a blank: the panel's cap
             ;; (*DIALOG-FRAME-MAX-HEIGHT*) keeps every option in view under them.
             (from (max 0 (min (- at 2) (- (length nodes) 5))))
             (sentence (wrapped-lines (tree-move-sentence node option value leaving joining)
                                      (max 20 (1- room)))))
        (append
         (loop for i from from below (min (length nodes) (+ from 5))
               for turn = (nth i nodes)
               for id = (getf turn :id)
               for on = (gethash id chain)
               for leaves = (and (getf turn :on-chain) (not on))
               for tag = (cond ((eq turn node) (cons (if continue " here " " before this ") :sheet-chip-on))
                               (leaves (cons " leaves head " :sheet-chip))
                               ((and on (not (getf turn :on-chain))) (cons " joins head " :diff-added-emphasis))
                               ((equal id head) (cons " HEAD " :sheet-chip-on)))
               collect (clip-segs
                        (append (list (make-seg :text (format nil " ~5a  " (or (metro-clock (getf turn :start)) ""))
                                                :tone :sheet-undone))
                                (metro-cell-segs (metro-cells layout i turn :head head))
                                (list (make-seg :text " " :tone :muted))
                                (when tag
                                  (list (make-seg :text (car tag) :tone (style-for-tone (cdr tag)) :bold t)
                                        (make-seg :text " " :tone :muted)))
                                (list (make-seg :text (metro-label-text turn)
                                                :tone (cond ((equal id head) :title)
                                                            ((or leaves (equal (getf turn :ending) "cancelled")) :sheet-undone)
                                                            (on :foreground)
                                                            (t :muted)))))
                        room :mark *dialog-row-overflow-mark*))
         (list '())
         (loop for k below 2
               for line = (nth k sentence)
               collect (if line
                           (clip-segs (list (make-seg :text (format nil " ~a" line) :tone :tool))
                                      room :mark *dialog-row-overflow-mark*)
                           '())))))))

(defun tree-fold-context (state)
  "The /tree tab's context: how many turns, on how many lines, from the first
one's clock time to the last one's end."
  (let* ((nodes (getf state :nodes))
         (lines (count-if #'metro-line-nodes (getf (getf state :layout) :lines)))
         (starts (loop for node in nodes when (getf node :start) collect (getf node :start)))
         (ends (loop for node in nodes
                     when (getf node :start)
                       collect (+ (getf node :start) (or (getf node :span) 0)))))
    (format nil " ~d turn~:p · ~d line~:p~@[ · ~a~]"
            (length nodes) lines
            (and starts (format nil "~a → ~a"
                                (metro-clock (reduce #'min starts))
                                (metro-clock (reduce #'max ends)))))))

;;; --- the /tree turn fold --------------------------------------------------
;;; The listing is one row per checkpoint, and a session of 20 turns and 270
;;; tool calls is 290 of them: the turns drown in their own tool traffic. The
;;; dialog folds it the way /sessions folds forks — a turn is one row, its
;;; tool rows are the branch under it, drawn only while the turn is open —
;;; and every turn starts closed, so /tree opens on the turns themselves and
;;; one Right opens the turn the operator is looking at.
;;;
;;; The fold rides the dialog in a side table rather than a slot: a running
;;; image cannot grow a structure a slot, and the fold must reach a shell that
;;; is already up.

(nlk:define-side-table tree-fold (:weakness :key)
  "The open /tree dialog -> its fold, a plist: :ENTRIES the listing in its own
order, :TURNS the ids the listing names as turns, :CHILDREN the tool rows
filed under each turn id, :OPEN the turn ids whose tool rows are drawn — the
one fact the Left/Right gesture moves. Esc drops the dialog and the fold with
it, so a reopened picker starts with every turn closed.")

(defun checkpoint-turn-entry-p (entry)
  "Whether listing ENTRY is a turn's own row — its prompt, the row the fold
keeps while its tool rows hang under it."
  (equal "user_message" (gethash "kind" entry)))

(defun tree-fold-index (entries &aux (turns (make-hash-table :test #'equal))
                                     (children (make-hash-table :test #'equal)))
  "The /tree dialog's fold over the listing ENTRIES: the turns it names and,
under each, the tool rows that followed it — filed by their own TURN_ID,
against the turns seen so far, so a row can never hang under a turn the
listing did not list. Every turn starts closed."
  (dolist (entry entries)
    (let ((turn-id (gethash "turn_id" entry)))
      (when (stringp turn-id)
        (if (checkpoint-turn-entry-p entry)
            (setf (gethash turn-id turns) t)
            (when (gethash turn-id turns)
              (alexandria:appendf (gethash turn-id children) (list entry)))))))
  ;; The graph the rows paint: the turns in the order they ran (METRO-NODES)
  ;; on the rails of the head's line as the listing stands (METRO-LAYOUT).
  (multiple-value-bind (nodes by-id) (metro-nodes entries children)
    (list :entries entries :turns turns :children children
          :open (make-hash-table :test #'equal)
          :nodes nodes :by-id by-id
          :layout (metro-layout nodes (lambda (id) (getf (gethash id by-id) :on-chain))))))

(defun tree-fold-rows (state &aux (turns (getf state :turns))
                                  (open (getf state :open))
                                  (layout (getf state :layout))
                                  (nodes (getf state :nodes))
                                  (head (getf (find-if (lambda (node) (getf node :head-p)) nodes) :id))
                                  (last-clock nil)
                                  (rows '()))
  "STATE's fold laid out on its graph (the /tree metro): a turn's own row in
the order the turns ran, and while the turn is open its recaps and then its
tool rows under it, the rails running on beside them. A tool row whose turn the
listing never named stays a row of its own, ahead of the graph, so nothing the
listing carried is lost."
  (dolist (entry (getf state :entries))
    (let ((turn-id (gethash "turn_id" entry)))
      (unless (or (checkpoint-turn-entry-p entry)
                  (and (stringp turn-id) (gethash turn-id turns)))
        (push (checkpoint-picker-row entry :child t) rows))))
  (loop for node in nodes
        for i from 0
        for id = (getf node :id)
        for kids = (getf node :calls)
        for opened = (and (gethash id open) t)
        for row = (checkpoint-picker-row (getf node :entry)
                                         :children (and kids (length kids))
                                         :open opened)
        for clock = (metro-clock (getf node :start))
        do ;; A recap at the head puts the head on its turn: the turn's row
           ;; offers what the head's does.
           (when (getf node :head-p)
             (setf (getf (picker-row-value row) :head-p) t))
           (setf (picker-row-paint row)
                 (metro-node-paint row node (metro-cells layout i node :head head)
                                   :clock clock :repeat (equal clock last-clock)
                                   :head (equal id head)
                                   :arrow (cond ((not (or kids (getf node :recaps))) "  ")
                                                (opened "▾ ")
                                                (t "▸ "))))
           (setf last-clock clock)
           (push row rows)
           (when opened
             (let ((under (metro-cells layout i node :sub t)))
               (dolist (recap (getf node :recaps))
                 (push (metro-recap-row recap under) rows))
               (dolist (kid kids)
                 (let ((kid-row (checkpoint-picker-row kid :child t)))
                   (setf (picker-row-paint kid-row) (metro-call-paint kid under))
                   (push kid-row rows))))))
  (nreverse rows))

(defun tree-fold-turn-value (state turn-id)
  "The row value TURN-ID's own row carries in STATE's fold as it now stands —
the seat a gesture leaves the cursor on."
  (loop for row in (tree-fold-rows state)
        for value = (picker-row-value row)
        when (and (equal turn-id (getf value :turn-id))
                  (equal "user_message" (getf value :kind)))
          return value))

(defun fold-arrow (model direction id open children-p parent relay)
  "One Left/Right over a picker fold whose OPEN table holds the ids drawn
open: Right opens ID when it has children (CHILDREN-P) and is closed; Left
closes ID when it is open, else PARENT (NIL at a root) with the cursor parked
there. RELAY lays the dialog's rows out again, the cursor back on the id it is
handed, and MODEL repaints. => T when the fold moved; a gesture with nothing
to fold declines, leaving the key the no-op it was."
  (when-let (target (ecase direction
                      (:right (and children-p (not (gethash id open)) id))
                      (:left (if (gethash id open) id parent))))
    (if (eq direction :right) (setf (gethash target open) t) (remhash target open))
    (funcall relay target)
    (when model.band
      (render-state model))
    t))

(defun tree-fold-gesture (model dialog direction &aux (state (tree-fold dialog)))
  "One Left/Right on the open /tree picker — the fold the highlight drives.
Right opens the highlighted turn's tool rows under it; Left closes the
highlighted turn, or — on a tool row — the turn it hangs under, parking the
cursor on that turn. A gesture with nothing to fold declines, leaving the key
the no-op it was, and so does a box holding a query: the rows on screen are
the search's, not the fold's. => T when the fold moved."
  (when (and state
             (zerop (length (string-trim " " dialog.search))))
    (let* ((value (picker-highlighted-value dialog))
           (turn-id (and value (getf value :turn-id))))
      (when (and (stringp turn-id) (gethash turn-id (getf state :turns)))
        ;; A turn row whose tool rows are drawn closes them. A tool row's turn
        ;; is open (the row is on screen), so the climb from it closes that
        ;; turn -- its parent here -- and parks on it.
        (fold-arrow model direction turn-id (getf state :open)
                    ;; A turn opens over its calls and the recaps folded under it.
                    (let ((node (gethash turn-id (getf state :by-id))))
                      (and node (or (getf node :calls) (getf node :recaps)) t))
                    (unless (equal "user_message" (getf value :kind)) turn-id)
                    (lambda (seat-turn)
                      ;; The rows under the dialog, laid out again with the fold
                      ;; as it now stands, the cursor back on SEAT-TURN's row.
                      (setf dialog.rows (tree-fold-rows state))
                      (picker-reseat-selection dialog
                                               (tree-fold-turn-value state seat-turn))))))))

(defun checkpoint-picker-row (entry &key child children open)
  "One /tree picker row from a checkpoint listing entry."
  ;; A turn row leads with the fold's own column — `▸ ' while the tool rows
  ;; under it are folded away, `▾ ' while they are drawn, blank when the turn
  ;; holds none — then the chain glyph and the prompt; the detail beside it
  ;; says how many calls the fold is holding back. A tool row reads by what
  ;; the tool was asked to do — eval's form, a shell's command — and by the
  ;; tool's name only when the listing carried no command for it, hanging one
  ;; indent deeper while it is a turn's child (CHILD). The label opens with the
  ;; tree glyphs of the lanes the listing put it in (TREE-LANE-PREFIX). These
  ;; slots are what the search reads and the moves carry; the /tree dialog
  ;; paints the row as the metro (TREE-FOLD-ROWS). The count suffix sizes the
  ;; rewind to before the prompt, one of the moves every prompt row offers
  ;; (CHECKPOINT-OPTION-ROWS).
  (with-json ((kind (gethash "kind" entry))
              (user-p (equal kind "user_message"))
              (head-p :boolean "is_head")
              (on-chain :boolean "on_head_chain")
              (fork :boolean "fork")
              (lanes (coerce (json-array entry "lanes") 'list))
              (indent (tree-lane-prefix fork lanes))
              (prompt (checkpoint-row-excerpt (gethash "prompt" entry)))
              (command (checkpoint-row-excerpt (gethash "tool_command" entry)))
              (caret (if (and (not child) children) (if open "▾ " "▸ ") ""))
              (origin :text "origin")
              (label (if user-p
                         (format nil "~a~a~a ~a"
                                 indent caret
                                 (cond (head-p "●") (on-chain "○") (t "◇"))
                                 prompt)
                         (format nil "~a~a· ~a"
                                 indent
                                 (if child "    " "  ")
                                 (or (json-value command :text)
                                     (gethash "tool_name" entry)
                                     "tool"))))
              ;; The descriptor the picked row hands the confirm stage (`turn
              ;; 3 · branch') stays clean: the calls count is the row's own
              ;; display, never part of what a picked row IS.
              ;; A turn no one asked -- a reflection's recap, an exit's wake --
              ;; carries no number: it says what it is instead.
              (descriptor (if user-p
                              (format nil "~a~a~a"
                                      (dispatch origin equal ("reflection" "recap") ("wake" "woke")
                                        (t (format nil "turn ~a" (or (gethash "turn_number" entry) "?"))))
                                      (if on-chain "" " · branch")
                                      (if head-p " · head" ""))
                              (if on-chain "" "branch")))
              (detail (if (and user-p children (not open))
                          (format nil "~a · ~d call~:p" descriptor children)
                          descriptor))
              (checkpoint-id :text "checkpoint_id")
              (files :integer "file_change_count" :default 0))
      entry
    (make-picker-row
     :label label :detail detail
     ;; The bare count renders immediately; the path tail lands via the
     ;; highlight fetch.
     :suffix (if (plusp files) (checkpoint-files-suffix files) "")
     :value (list :kind kind
                  :turn-id (gethash "turn_id" entry)
                  :head-p head-p
                  :on-chain on-chain
                  ;; The confirm stage's tab context: `turn 3 · branch'.
                  :detail descriptor
                  :checkpoint-id checkpoint-id
                  ;; The prompt excerpt doubles as the restore dialog's
                  ;; third-row summary (the Zig item.summary hand-off).
                  :summary (if user-p prompt "")
                  :file-change-count files))))

(defun slash-tree-op (model)
  "Open the checkpoint tree picker."
  ;; Returns notice text. The mouse click-to-restore gesture (ui.lisp) calls
  ;; it too.
  (with-slash-notice ("tree")
      (let* ((response (session-control-call model :get "checkpoints"))
             ;; The fold lays the listing out as the metro: one row per turn
             ;; in the order the turns ran, on the rails of their lines, the
             ;; recaps and tool rows of a turn under it while it is open --
             ;; and the selection opens on the head wherever its line sits.
             (state (tree-fold-index
                     (coerce (gethash "checkpoints" response) 'list)))
             (rows (tree-fold-rows state)))
        (open-picker-dialog
         model "Session tree" rows
         (lambda (model value)
           (flet ((navigate (boundary &optional (restore-files t))
                    (send-navigate-checkpoint
                     model.ws
                     model.session-id
                     :target-turn-id (getf value :turn-id)
                     :boundary boundary
                     :restore-files restore-files)))
             (cond
               ((null (getf value :turn-id)) "checkpoint carries no turn")
               ((equal (getf value :kind) "user_message")
                ;; A prompt row is a turn, and the confirm stage offers its two
                ;; moves: continue from it — the turn boundary lands the head
                ;; on the turn, answer kept, conversation only, so the next
                ;; prompt branches from there (the operator reads
                ;; a row as being AT that turn) — or rewind to before its
                ;; message, the head on its parent and the prompt back in the
                ;; composer, with or without the code changes. The same menu
                ;; on the head chain and off it: a branch row's continue is
                ;; the return to that branch, and on the head row only the
                ;; rewind remains, which is /undo. Nothing fires until the
                ;; operator picks.
                (open-checkpoint-dialog
                 model value
                 (lambda (move restore-files)
                   (ecase move
                     (:continue (navigate "turn" nil))
                     (:rewind (navigate "user_message" restore-files))))
                 ;; The move drawn on the graph the tree opened on.
                 :preview (lambda (option)
                            (tree-move-rows state value option
                                            (docked-panel-room model.term-width)))))
               (t
                ;; Tool rows keep direct Enter-navigate — the head on the
                ;; turn, the workspace at that call — the Zig /tree dialog
                ;; had no confirm stage for them either.
                (navigate "turn")
                nil))))
         :searchable t
         :on-arrow #'tree-fold-gesture
         :selection (or (position-if (lambda (row) (getf (picker-row-value row) :head-p)) rows) 0)
         ;; The graph's measure: ` 18 turns · 2 lines · 13:22 → 14:31'.
         :context (tree-fold-context state)
         :search-placeholder "Search checkpoints"
         :empty-label "No checkpoints available"
         :no-match-label "No checkpoints match this filter"
         :footer-hints :session-tree
         ;; The highlight fetch is a whole staged-diff read on the gateway —
         ;; 25-60 ms measured, and a good deal more on a session whose
         ;; workspace is huge — so it is STAGED, never paid between the
         ;; keystroke and the rows: the picker paints from the listing it
         ;; just read, and the selected row's files chrome lands on the
         ;; frame clock (OPEN-PICKER-DIALOG :HIGHLIGHT-DELIVERY, the
         ;; /sessions picker's own shape).
         :highlight-delivery :staged
         :on-highlight
         (lambda (model row)
           (checkpoint-row-ensure-files
            row
            (lambda (checkpoint-id)
              (with-handlers ((error () (values nil nil)))
                (let ((object (session-control-call
                               model :get
                               (format nil "checkpoints/~a/files" checkpoint-id))))
                  (values (coerce (gethash "files" object) 'list) t)))))))
        ;; The fold the dialog's Left/Right gesture moves, filed beside the
        ;; dialog — Esc drops it, so a reopened picker starts folded.
        (setf (tree-fold model.dialog) state)
        nil)))
(defun relative-age-label (updated-at now-ms)
  "The Zig relativeAgeLabel copy: \"just now\", \"Nm ago\", \"Nh ago\",
\"Nd ago\"; a stamp that does not parse passes through verbatim."
  ;; The shell's ISO-8601-MS is this image's one reader for the stamp shape
  ;; the wire writes, and it declines the same garbage the Zig days-from-civil
  ;; parser declined.
  (let* ((updated (iso-8601-ms updated-at))
         (diff (and updated (floor (- now-ms updated) 1000))))
    (cond ((null updated) updated-at)
          ((< diff 60) "just now")
          (t (format nil "~a ago" (nlk:duration-text diff :coarse t))))))

;;; --- the running sessions ----------------------------------------------------