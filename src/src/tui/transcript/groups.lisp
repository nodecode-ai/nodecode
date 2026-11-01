;;;; groups.lisp --- the group pass, the projected tail, transcript rhythm. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; Routine calls fold. A run of calls that only read, search, list, fetch,
;;; recall, check or wait — ROW-ACTIVITY says so — becomes one TOOL-GROUP
;;; that renders as one line, `Read 4 files, Searched 'tool-row''; while
;;; thoughts are folded the thoughts between those calls join the run, so a
;;; turn that read its way to an answer reads as one line and the answer. The
;;; pass is pure over the rows: the rows are never touched, and replay folds
;;; the same groups the live stream does because both build the same rows.
;;;
;;; A reflection's recap folds the same way. The experience cell records it
;;; behind the turn it recaps, a pair written whole (NLK:RECORD-EXCHANGE-TURN)
;;; whose completion names its recorder `reflection': a question the operator
;;; never asked, its answer and its divider, nine rows after every tool turn.
;;; It is the machine's note to itself, so it draws as one quiet line under
;;; that turn, `Reflection recap · ' and its first words, which opens on the
;;; answer and its divider as a group opens on its calls -- the web page's
;;; drawRecap. Other recorded pairs draw as they are.

(defstruct (tool-group (:constructor make-tool-group (key)))
  "A routine run, completed Standard work trail, or recap projected as one item."
  ;; MEMBERS are its rows in order, the thoughts between its calls included;
  ;; a recap's are its prompt row and the rows of its turn after it. KEY is
  ;; its accordion identity, (:GROUP call-id) or (:RECAP turn-id), stable
  ;; while its first member is. LINE and BLOCK memoize its folded rendering;
  ;; TITLED-FROM is what LINE was worded from, (RUNNING . ACTIVITIES). TAIL
  ;; is what stands under its folded title a line each: a running turn's
  ;; newest round (NEWEST-ROUND) and any call still running here or as a
  ;; live exec, and THOUGHT a running turn's newest thought, the paragraph
  ;; under the live bar (LIVE-THOUGHT-ROWS).
  key
  (members '())
  (tail '())
  (thought nil)
  (line nil)
  (titled-from nil)
  (block nil))

(nlk:access (group tool-group) (pressed tool-group) (recap tool-group))

(defmethod print-object ((group tool-group) stream)
  "Identity, never the memo: BLOCK holds the DISPLAY-BLOCK whose row is this
group, so the default structure print never ends."
  (print-unreadable-object (group stream :type t :identity t)
    (format stream "~s ~d member~:p" group.key
            (length group.members))))

(nlk:define-side-table tool-groups (:weakness :key)
  "Weak memo: a group's first call row -> its TOOL-GROUP, so a group keeps
its identity across projections — the hover and the accordion hold it by
identity and by key while its run grows.")

(nlk:define-side-table row-groups (:weakness :key)
  "Weak memo: a row -> the TOOL-GROUP the last projection folded it into, so
a click on an open group's member knows which group it belongs to.")

(defun routine-call-p (row)
  "Whether ROW is a call that joins a run of routine calls (GROUP-ROWS): its
words group, and no background process of it ran on past it."
  (and (eq :tool-call row.kind) (not (row-edited-p row))
       (not (member row.tool-status '("exited" "background") :test #'equal))
       (when-let (activity (row-activity row)) (activity-groupable-p activity))))

(defun reflection-recap-p (row)
  "Whether ROW opened a reflection's recap: the input row of a recorded pair
whose completion named its recorder `reflection'."
  ;; The name lands on the input row with the completion (INGEST-TURN-TERMINAL);
  ;; until then the row is a recorded pair's like any other.
  (and (transcript-row-p row) (equal "reflection" row.recorded) row.turn-id t))

(defun row-mutation-path (row &aux (summary (row-notice-summary row))
                                   (path (and (equal "Mutate" (json-value summary :string "name"))
                                              (json-value summary :string "path"))))
  "A successful Mutate receipt's explicit nonempty affected-file path, or NIL."
  (and path (plusp (length path)) path))

(nlk:define-side-table mutation-groups (:weakness :key)
  "Live-only first file receipt -> its stable mutation disclosure.")

(defun group-mutation-rows (rows &aux (items '()) (run '()) (turn nil))
  "Adjacent successful file receipts of one turn as one Mutate disclosure."
  (labels ((flush ()
             (when run
               ;; Retain the first receipt for every affected file. Deduplicating
               ;; only PATHS left repeated file blocks in MEMBERS; dropping the
               ;; whole run is not deduplication: one file still needs its receipt.
               (let ((members (remove-duplicates (nreverse run)
                                                 :key #'row-mutation-path
                                                 :test #'equal :from-end t)))
                 (if (rest members)
                     (let* ((anchor (first members))
                            (group (or (mutation-groups anchor)
                                       (setf (mutation-groups anchor)
                                             (make-tool-group (list :mutation anchor))))))
                       (setf (tool-group-members group) members)
                       (forget-row-groups group)
                       (dolist (row members) (setf (row-groups row) group))
                       (push group items))
                     (dolist (row members)
                       (forget-row-groups row)
                       (push row items))))
               (setf run nil))))
    (dolist (row rows)
      (cond ((row-mutation-path row)
             (when (and run (not (equal turn (transcript-row-turn-id row)))) (flush))
             (setf turn (transcript-row-turn-id row))
             (push row run))
            (t (flush)
               (push row items))))
    (flush))
  (nreverse items))
(defun transcript-background-row (row backgrounds)
  "The live exec owned by ROW's call id, or by its unadopted provider index.
Adopted calls never match a later round's reused stream index."
  (when (and (transcript-row-p row) (eq :tool-call (transcript-row-kind row)))
    (find-if (lambda (exec &aux (call (bg-band-row-tool-call-id exec)))
               (if (and (stringp call) (plusp (length call)))
                   (equal call (transcript-row-tool-call-id row))
                   (and (null (transcript-row-tool-call-id row))
                        (transcript-row-stream-index row)
                        (eql (transcript-row-stream-index row) (bg-band-row-stream-index exec)))))
             backgrounds)))


(nlk:define-side-table live-run-rows (:weakness :key)
  "A running turn's newest run -> the most rows it and the canvas row over the
live bar have taken this turn (RUN-LIFT). GROUP-ROWS opens the entry while
the turn runs and drops it when the turn ends.")

(nlk:define-side-table held-window (:weakness :key)
  "A listed call -> (WIDTH . BLOCK): the output window its exec last showed
under it (LISTED-OUTPUT), which it keeps once the exec is gone while its
round stands under the run.")

(defun newest-round (members &aux (round '()) (receipts '()) (later nil))
  "The calls and receipts of run MEMBERS' newest tool round, in order: back
from the newest call to the thought before it or to the call its round
opened with."
  ;; Provider stream indices restart at 0 every round (INGEST-STREAM-TOOL),
  ;; so a call is of the round of the call after it only while its index is
  ;; below that one's; a call without one is a round of its own. A receipt
  ;; follows its call. A thought after the round is the next round's, still
  ;; arriving: the round stands until that one lists a call.
  (flet ((done () (return-from newest-round (or round receipts))))
    (dolist (row (reverse members) (or round receipts))
      (cond ((eq :reasoning (transcript-row-kind row))
             (when (or round receipts) (done)))
            ((eq :tool-call (transcript-row-kind row))
             (if (or (null later)
                     (let ((index (transcript-row-stream-index row))
                           (next (transcript-row-stream-index later)))
                       (and index next (< index next))))
                 (setf round (cons row (append receipts round)) receipts '() later row)
                 (done)))
            ((row-notice-summary row) (push row receipts))))))

(defun group-rows (rows &optional (context rows) backgrounds)
  "ROWS as projection items, in order: a row, or a TOOL-GROUP for each run
of routine calls. Reflection recaps appear only in the Developer viewport."
  ;; A Developer run holds consecutive calls of one turn that group
  ;; (ACTIVITY-GROUPABLE-P) and, while thoughts are folded, the thoughts between them;
  ;; a thought after a run's last call stays its own item. Anything else breaks
  ;; the run: an operator prompt, an answer with text, a plan, a notice, a call
  ;; that changed something or failed, a thought opened on its own, a new turn.
  ;; A recap's group holds its prompt row and every row of its turn after it.
  ;; Consume them before the viewport branches: Standard emits no recap item
  ;; (including padding), while Developer keeps its existing accordion.
  ;; A Standard run is a turn's whole work trail, live or done, under one
  ;; `Ran N commands' that counts up as calls land: its calls, thoughts and
  ;; receipts. Only an unsummarized notice splits it. A run stays folded,
  ;; live or done, until a click opens it to its
  ;; calls' names, one line each; under the folded title stands the newest
  ;; round of a running turn, finished calls too, until the next round lists
  ;; its first call, and then goes into the count. A call handed off to the
  ;; background stays where it ran while its exec lives, its output streaming
  ;; under it. It stood under the run as the exec's code block. /execs stops it.
  ;; Developer keeps that block in the call's place. The model's words never
  ;; fold, and a turn's words have one place, under its run: the newest
  ;; takes it from the one before, which fades out of it (TURN-WORDS-ON). While
  ;; the turn runs, its newest finished thought stands under the live bar
  ;; (the THOUGHT the run holds, LIVE-THOUGHT-ROWS). CONTEXT carries the
  ;; turns' ends and words even for a partial projection.
  (let ((standard (eq *tool-detail* :standard))
        (items '())
        (run '())
        (held '())
        (aside '())
        (recap nil)
        ;; ENDED: the turns that have. NEWEST: each turn's newest words.
        (ended (make-hash-table :test #'equal))
        (newest (make-hash-table :test #'equal)))
    (when standard
      (dolist (row context)
        (when-let (turn row.turn-id)
          (cond (row.outcome (setf (gethash turn ended) t))
                ((and (assistant-text-p row) (position-if-not #'blank-char-p row.text))
                 (setf (gethash turn newest) row))))))
    (labels ((alone (row) (close-run) (forget-row-groups row) (push row items))
             (close-run ()
               (when run
                 (let* ((members (reverse run))
                        (first-call (find :tool-call members :key #'transcript-row-kind))
                        (anchor (if standard (first members) first-call))
                        (group (or (tool-groups anchor)
                                   (setf (tool-groups anchor) (make-tool-group nil))))
                        (key (if standard
                                 (list :process (or (and first-call (transcript-row-tool-call-id first-call))
                                                    (transcript-row-turn-id anchor)))
                                 (list :group (tool-row-expand-key anchor)))))
                   (unless (equal key group.key)
                     (setf group.line nil group.titled-from nil group.block nil))
                   (setf group.key key group.members members)
                   (dolist (member members)
                     (setf (row-groups member) group))
                   (push group items)))
               (dolist (row (append (reverse aside) (reverse held)))
                 (forget-row-groups row)
                 (push row items))
               (setf run '() held '() aside '())))
      (dolist (row rows)
        (cond
          ;; A Standard call the band holds is a call like any other.
          ((and (not standard) (transcript-background-row row backgrounds))
           (alone row))
          ;; Associate recap members in both modes without removing facts;
          ;; only Developer emits the group into the visible projection.
          ((and recap (equal row.turn-id (transcript-row-turn-id (first recap.members))))
           (setf recap.members (append recap.members (list row))
                 (row-groups row) recap))
          ((reflection-recap-p row)
           (close-run)
           (setf recap (or (tool-groups row) (setf (tool-groups row) (make-tool-group nil)))
                 recap.key (list :recap row.turn-id)
                 recap.members (list row)
                 (row-groups row) recap)
           (unless standard (push recap items)))
          ((and standard row.turn-id
                (not row.outcome) (not (transcript-user-prompt-row-p row)))
           (let ((open (or run aside)))
             (when (and open (not (row-turn-matches-p row (transcript-row-turn-id (first open)))))
               (close-run)))
           (cond ((or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row))
                  (setf run (cons row (append held run)) held '()))
                 ;; Unsummarized notices and diagnostics must remain visible.
                 ((eq :system-notice row.kind) (alone row))
                 ;; The turn's words stand under its run, in the one place
                 ;; the newest holds; the others give it up.
                 ((assistant-text-p row)
                  (forget-row-groups row)
                  (when-let (on (and (eq row (gethash row.turn-id newest))
                                     (turn-words-on row (not (gethash row.turn-id ended)))))
                    (push on aside)))
                 ((or run aside) (push row held))
                 (t (forget-row-groups row) (push row items))))
          (standard (alone row))
          ((routine-call-p row)
           (when (and run (not (row-turn-matches-p row (transcript-row-turn-id (first run)))))
             (close-run))
           (setf run (cons row (append held run))
                 held '()))
          ;; Assistant text with nothing to say yet renders no block and breaks no run.
          ((and run
                (or (and (eq :reasoning (transcript-row-kind row))
                         (not (detail-open-p row))
                         (row-turn-matches-p row (transcript-row-turn-id (first run))))
                    (and (assistant-text-p row) (zerop (length (trim-block-text row.text))))))
           (push row held))
          (t (alone row))))
      (close-run))
    (let ((items (nreverse items))
          (seen '()))
      (when standard
        ;; The newest run of a running turn holds its thought.
        (dolist (item (reverse items))
          (when (and (tool-group-p item) (eq :process (first (tool-group-key item))))
            (let* ((members (tool-group-members item))
                   (turn (transcript-row-turn-id (first members)))
                   ;; The turn's newest run, while the turn runs.
                   (live (and (not (gethash turn ended)) (not (member turn seen :test #'equal))))
                   (round (and live (newest-round members))))
              (setf (tool-group-tail item)
                    ;; A turn that ended never left a call running.
                    (remove-if-not (lambda (row) (or (member row round :test #'eq)
                                                     (and (equal "running" (transcript-row-tool-status row))
                                                          (not (gethash turn ended)))
                                                     (transcript-background-row row backgrounds)))
                                   members)
                    (tool-group-thought item)
                    (and live
                         (find-if (lambda (row) (and (eq :reasoning (transcript-row-kind row))
                                                     (position-if-not #'blank-char-p (transcript-row-text row))))
                                  members :from-end t)))
              (cond (live (unless (live-run-rows item) (setf (live-run-rows item) 0)))
                    ((live-run-rows item)
                     ;; Its turn over or its place taken, what it held goes.
                     (forget-live-run-rows item)
                     (dolist (row members) (forget-held-window row))))
              (push turn seen)))))
      (group-mutation-rows items))))

(defparameter +words-fade-ms+ '(150 250)
  "How quickly, out and then in, a turn's words give their place to newer
ones: quickly enough to read a fast model. Without this, a slow out and in
would leave words a model sends a moment apart never all the way up.")

(defvar *turn-words* (make-hash-table :test 'equal)
  "A turn id -> the FADE of the place its words stand in, while the turn
runs and until that place stands still after it has ended.")

(defun turn-words-on (row live)
  "What the place of the words of ROW's turn shows, ROW being the newest of
them: ROW, or while it fades in behind them the words it took the place from.
LIVE: the turn is running, the only time its words fade."
  ;; History lays out as it stands: a turn met ended never fades, and one
  ;; whose place stood still once it ended lets the place go. A newest row
  ;; that only changed hands -- the durable fact seating a streamed row's
  ;; words, or a turn that ended -- takes the place with no fade.
  (let* ((turn row.turn-id)
         (fade (and *fades* (gethash turn *turn-words*))))
    (cond ((not *fades*) row)
          ((null fade)
           (when live
             (setf (gethash turn *turn-words*) (apply #'make-fade row +words-fade-ms+)))
           row)
          (t (unless (eq row (fade-shown fade))
               (if (and live (string/= (transcript-row-text (fade-shown fade)) row.text))
                   (fade-to fade row)
                   (setf (fade-shown fade) row)))
             (prog1 (fade-on fade)
               (when (and (not live) (fade-settled-p fade))
                 (remhash turn *turn-words*)))))))

(defun tool-group-open-p (group &optional (key *expanded-tool-call*))
  "Whether the accordion owns GROUP or one of its members. A member clicked
open under the run's shut title, (:FOLD KEY) (OPEN-KEY), leaves it shut."
  ;; A run opened while it was all thought is keyed by its turn; its first
  ;; call rekeys it, and the run stays open.
  (and key (or (equal key (tool-group-key group))
               (and (eq :process (first (tool-group-key group)))
                    (equal key (list :process (transcript-row-turn-id (first (tool-group-members group))))))
               (member key (tool-group-members group) :key #'tool-row-expand-key :test #'equal)
               (and (consp key)
                    (case (first key)
                      (:diff (member (second key) (tool-group-members group) :key #'tool-row-expand-key :test #'equal))
                      (:mutation (member (second key) (tool-group-members group) :test #'eq))))) t))

(defun run-trail-row-p (row)
  "Whether ROW is one a run lists under its title, a line each: a call or a
receipt."
  (or (eq :tool-call (transcript-row-kind row)) (and (row-notice-summary row) t)))

(defun receipt-call-id (row)
  "The id of the call whose run left Mutate receipt ROW, or NIL: the load the
organism announced names it."
  (when-let (summary (row-notice-summary row))
    (json-value summary :string "call_id")))

(defun call-receipts (call rows &aux (id (and (eq :tool-call (transcript-row-kind call))
                                              (transcript-row-tool-call-id call))))
  "The Mutate receipts among ROWS that CALL's own run left, in their order."
  (and id (remove-if-not (lambda (row) (equal id (receipt-call-id row))) rows)))

(defun without-folded-receipts (rows)
  "ROWS, a run's listed lines, without each receipt whose call ROWS lists:
that call's own line says what it kept (RUN-TAIL-LLINE)."
  ;; Listed apart, a call and its receipt were one step said twice, its comment
  ;; grey and then the same comment teal, and the receipt could stand under a
  ;; later call when it landed after it.
  (remove-if (lambda (row)
               (when-let (id (receipt-call-id row))
                 (find-if (lambda (call) (and (eq :tool-call (transcript-row-kind call))
                                              (equal id (transcript-row-tool-call-id call))))
                          rows)))
             rows))

(defun kept-path-text (path)
  "A kept file's PATH for a line: from home as `~/...', the way a person reads
their own paths (NLK:HOME-ABBREVIATED)."
  (nlk:one-line (nlk:home-abbreviated path)))

(defun kept-files-text (receipts &aux (paths (remove-duplicates (remove nil (mapcar #'row-mutation-path receipts))
                                                                :test #'equal)))
  "What RECEIPTS kept, for a call's line: its one file, or how many files.
NIL when they name none."
  (cond ((null paths) nil)
        ((rest paths) (format nil "~d files" (length paths)))
        (t (kept-path-text (first paths)))))

(nlk:define-side-table step-in (:weakness :key)
  "Weak memo: an lline -> STEPPED-IN's copy of it, one object while the source
stands, so LLINE-RENDER's memo holds.")

(defun stepped-in (line)
  "LINE two columns in, its spans and hang with it -- but for a line hooked
in the gutter (⎿, ⋮), whose words already stand two in from the hook."
  (cond ((member line.role '(:tool :tool-omission)) line)
        ((step-in line))
        (t (setf (step-in line)
                 (copy-with lline line :text (concatenate 'string "  " line.text)
                                       :spans (mapcar (lambda (span) (shifted-span span 2)) line.spans)
                                       :hang (+ 2 line.hang))))))

(defun without-note (row llines)
  "LLINES, what listed call ROW holds, without the note heading its code:
the line it is listed on says those words already."
  (if (and (rest llines) (equal (tool-summary-words row) (string-left-trim "; " (lline-text (first llines)))))
      (rest llines)
      llines))

(defun run-member-open-p (row &aux (key (tool-row-expand-key row)) (open (open-key)))
  "Whether a run's listed ROW shows what it holds under its line -- the
accordion holds it, or one of the files it changed, under the run's title
shut or open."
  (or (detail-open-p key)
      (and (consp open) (eq :diff (first open)) (equal (second open) key))))

(defun run-diff-call (members)
  "The call among a run's MEMBERS whose diffs the run shows unasked: the
newest whose snapshot saw files change, with every file it changed. NIL
when none did."
  ;; Each call that changed files showed its diffs, so a turn of N edits
  ;; stood N diff renders tall. A newer edit takes the place of the one
  ;; before it, whose line still opens its own (RUN-MEMBER-OPEN-P).
  (find-if #'row-edited-p members :from-end t))

(defun clip-lline (line width)
  "LINE cut to its first row at WIDTH, its spans with it. => LINE."
  (let ((end (length (setf line.text (clip-text-to-width line.text (lline-widths line width))))))
    (setf line.spans (loop for span in line.spans
                           when (< span.start end)
                             collect (copy-with mspan span :end (min end span.end))))
    line))

(nlk:define-side-table tail-llines (:weakness :key)
  "Weak memo: a call or receipt row -> RUN-TAIL-LLINE's line, one object while
what the row says and the width stand, so LLINE-RENDER's memo holds.")

(defun ember-call-p (row)
  "Whether call ROW still runs, here or in the background: its listed line
burns amber and the frame's ember warms it."
  (and (member row.tool-status '("running" "background") :test #'equal) t))

(nlk:define-side-table ember-line (:weakness :key)
  "A painted transcript row -> the running call whose listed line it is, so
the frame's ember (EMBER-OVERLAY) finds that line on screen.")

(defun elided-path-text (text budget &aux (separator (if (find #\/ text) #\/ #\\)))
  "Path TEXT in BUDGET display columns where its folders can give way: its
first segment, an …, then as many of its last segments as fit, down to the
file's own name. TEXT itself when it fits or has no folder to drop."
  ;; ~/.nodecode/cells/you/read-pdf.lisp -> ~/…/you/read-pdf.lisp -> ~/…/read-pdf.lisp
  (let ((parts (uiop:split-string text :separator (string separator))))
    (if (or (<= (text-display-width text) budget) (< (length parts) 3))
        text
        (flet ((elided (keep)
                 (format nil "~a~c…~{~c~a~}" (first parts) separator
                         (loop for part in (last parts keep) collect separator collect part))))
          (or (loop for keep from (- (length parts) 2) downto 1
                    for candidate = (elided keep)
                    when (<= (text-display-width candidate) budget) return candidate)
              (elided 1))))))

(defun fitted-tail-lline (build words kept width)
  "(BUILD WORDS KEPT), a listed row's line, cut to its first row at WIDTH so
what follows WORDS stays whole: KEPT, the path a call kept, gives up its
middle folders first, then WORDS shorten. => LINE."
  ;; Cut at its end, a phone-wide row read `... kept in my layer · ~/…':
  ;; the file it kept, gone; a running call lost its `running' the same way.
  ;; Words cut under a dozen columns say nothing: there the line's end goes.
  (let* ((line (funcall build words kept))
         (room (lline-widths line width)))
    (flet ((over () (- (text-display-width (lline-text line)) room)))
      (when (and kept (plusp (over)))
        (setf kept (elided-path-text kept (- (text-display-width kept) (over)))
              line (funcall build words kept)))
      (when (and words (plusp (over)))
        (let ((keep (- (text-display-width words) (over))))
          (when (>= keep 12)
            (setf line (funcall build (clip-text-to-width words keep) kept)))))
      (clip-lline line width))))

(defun run-tail-lline (row width &optional receipts)
  "A call or receipt ROW as one line under its folded run, on the column of
the run title's words: no label, a call's words grey, amber while it runs
here or in the background, and a Mutate's teal. A call that left RECEIPTS,
the Mutates its run kept, is teal itself, what it kept after its words.
Too wide for its row, the line gives way inside, never its end (FITTED-TAIL-LLINE)."
  ;; Two columns in from the title they read as a step off it.
  ;; A running call in grey would read as stopped; its amber is the
  ;; ground the ember warms (EMBER-OVERLAY).
  (memoized (tail-llines row)
    (width row.tool-name row.tool-args (length (or row.tool-args "")) row.tool-status row.tool-exit-code
     row.metadata row.error-p (row-load-errors row) row.text row.text-rev (first receipts) (length receipts))
    ;; Opened, a run lists these lines where it showed Eval and Mutate cards
    ;; a receipt names its file after its comment, and a call
    ;; that changed files counts the lines.
    (if-let (summary (row-notice-summary row))
      (let ((comment (json-value summary :text "comment"))
            (path (json-value summary :text "path")))
        (fitted-tail-lline
         (lambda (words kept)
           (let ((line (make-lline :text (or words kept (nlk:one-line (json-value summary :string "name")))
                                   :tone :success)))
             (when (and words kept)
               (append-lline-run line (format nil " · ~a" kept) :tone :muted))
             line))
         (and comment (nlk:one-line comment)) (and path (kept-path-text path)) width))
      (fitted-tail-lline
       (lambda (words kept)
         (let ((line (make-lline :text words
                                 :tone (cond ((ember-call-p row) :tool) (receipts :success) (t :muted)))))
           (when kept
             (append-lline-run line (format nil " · ~a" kept) :tone :muted))
           (append-row-diff-stats line row)
           (append-status-text line (if (row-load-errors row) "error" row.tool-status)
                               (tool-failure-text row) (row-exit-document row))
           line))
       (nlk:one-line (or (tool-summary-words row) (json-value row.tool-name :text) "…"))
       (kept-files-text receipts) width))))

(defun paragraph-start (text end &aux (line-end end))
  "Where the paragraph of TEXT that ends at END starts: the line after the
nearest blank line above END, else 0."
  (loop (let* ((newline (position #\Newline text :end line-end :from-end t))
               (line-start (if newline (1+ newline) 0)))
          (when (and (< line-end end)
                     (not (position-if-not #'blank-char-p text :start line-start :end line-end)))
            (return (1+ line-end)))
          (if newline (setf line-end newline) (return 0)))))

(defun thought-heading-p (text start end)
  "Whether the paragraph of TEXT from START to END is a heading: one line, a
`# ...' or a `**...**'."
  (let ((title (string-trim '(#\Space #\Tab #\Return) (subseq text start end))))
    (and (not (find #\Newline title))
         (or (alexandria:starts-with #\# title)
             (and (alexandria:starts-with-subseq "**" title) (alexandria:ends-with-subseq "**" title))))))

(defun thought-heading-start (text start)
  "Where the heading right above the paragraph of TEXT at START starts, or
NIL when no heading stands there."
  (when-let (above (position-if-not #'blank-char-p text :end start :from-end t))
    (let ((heading (paragraph-start text (1+ above))))
      (and (thought-heading-p text heading (1+ above)) heading))))

(defun finished-thought-end (row &aux (text row.text))
  "Where the finished thinking of thought ROW ends: its whole text once it
is done; while it arrives, before the paragraph being written -- a paragraph
is done once a blank line closes it -- and the heading over that paragraph.
NIL while nothing of it is finished."
  (when-let (at (position-if-not #'blank-char-p text :from-end t))
    (let ((start (paragraph-start text (1+ at))))
      (cond ((not (thinking-p row)) (1+ at))
            ;; Closed, and no heading opening the next one.
            ((and (>= (count #\Newline text :start at) 2)
                  (not (thought-heading-p text start (1+ at))))
             (1+ at))
            (t (when-let (done (position-if-not #'blank-char-p text
                                                :end (if (>= (count #\Newline text :start at) 2)
                                                         start
                                                         (or (thought-heading-start text start) start))
                                                :from-end t))
                 (1+ done)))))))

(defun thought-paragraph-llines (text end &aux (start (paragraph-start text end)))
  "The paragraph of thought TEXT ending at END, whole, as Markdown lines
under the heading that opens it when one does, blank and rule lines dropped:
the newest thinking the model has finished, until a newer paragraph is done
and takes its place."
  ;; A summary's `**Finding the label**' is a paragraph of its own, and the
  ;; words under it say nothing alone. Under the reasoning's shadow, a step
  ;; quieter than the calls' grey: without this, at the same tone it would
  ;; read as another call.
  (setf start (or (thought-heading-start text start) start))
  (loop for line in (markdown-llines (subseq text start end) :shadow t)
        unless (or line.rule (not (position-if-not #'blank-char-p line.text)))
          collect (copy-with lline line :tone :muted :italic t)))

(nlk:define-side-table thought-llines (:weakness :key)
  "Weak memo: a thought row -> THOUGHT-PARAGRAPH-LLINES' lines, the same
objects while the row's words and where its finished part ends stand, so
LLINE-RENDER's memo holds across ticks.")

(defun finished-thought (group)
  "The newest finished thinking of run GROUP, (ROW . END), or NIL: a thought
still arriving shows only the paragraphs it has closed, and one with none
leaves the place to the thought before it."
  (loop for row in (reverse (tool-group-members group))
        when (eq :reasoning (transcript-row-kind row))
          do (when-let (end (finished-thought-end row))
               (return (cons row end)))))

(defparameter +thought-fade-ms+ '(100 150)
  "How quickly, out and then in, a finished thought gives the place under
the live bar to the next: the first comes up, a newer one takes the place
quickly -- IN the time each of its tokens takes.  A fast model's thoughts
come a moment apart, each read, not watched rising.")

(defparameter +thought-token-step-ms+ 15
  "Milliseconds between one token of a finished thought starting to come up
and the next: the thought arrives a word at a time, in reading order
(fade every token), faster than it is read.")

(defparameter +thought-sweep-ms+ 450
  "The longest the tokens of a thought take to start coming up, first to
last: a long one steps faster rather than taking longer.")

(nlk:define-side-table thought-fades (:weakness :key)
  "A model -> the FADE of the place under its live bar where the newest
finished thought stands.")

(defun live-thought-rows (model width)
  "The newest finished thought of the running turn MODEL's transcript shows
-- in the run that holds its THOUGHT (GROUP-ROWS) -- as its newest closed
paragraph's rows under the live bar at WIDTH => (values PAINTED-LIST
PLAIN-LIST), NIL when there is none."
  ;; Without this, a thought row would stand above the bar while a run has
  ;; calls, and the operator would read it as the wrong side of the line
  ;; saying the model is thinking. Only what the model has finished
  ;; thinking shows, faded in, never the words still arriving. The
  ;; projection the frame shows holds the run; under a dialog that is the
  ;; previewed session's, whose bar the frame shows too.
  (when (or (chat-model-turn-in-flight model)
            (and (chat-model-dialog model) (chat-model-transcript-preview model)))
    (let* ((block (find-if (lambda (row) (and (tool-group-p row) (tool-group-thought row)))
                           (seventh (chat-model-projection-cache model))
                           :key #'display-block-row :from-end t))
           (group (and block (display-block-row block)))
           (thought (and group (finished-thought group)))
           (fade (thought-fades model)))
      ;; The place fades only between thoughts of one run: a thought of a run
      ;; gone, or of a turn before, never goes out under this one.
      (cond ((not (and *fades* thought)) (setf fade nil (thought-fades model) nil))
            ((or (null fade) (not (member (car (fade-on fade)) (tool-group-members group))))
             (setf fade (setf (thought-fades model) (apply #'make-fade thought +thought-fade-ms+))))
            (t (fade-to fade thought)))
      (when-let (on (if fade (fade-on fade) thought))
        (destructuring-bind (row . end) on
          (multiple-value-bind (rows plains)
              (llines-rows (memoized (thought-llines row) (row.text end)
                             (thought-paragraph-llines row.text end))
                           width)
            ;; What holds the place comes up a token at a time.
            (when (and fade (equal on (fade-shown fade)))
              (let ((tokens (loop for plain in plains
                                  sum (count-if #'plusp (uiop:split-string plain :separator " ") :key #'length))))
                (setf (fade-tokens fade) (max 1 tokens)
                      (fade-step fade) (min +thought-token-step-ms+
                                            (floor +thought-sweep-ms+ (max 1 tokens))))))
            (values (if fade (mark-fading-rows rows fade on) rows) plains)))))))

(defun listed-output (row width backgrounds &aux (held (held-window row)))
  "What call ROW's exec has written, as the list of the one block a run lists
under the call's line at WIDTH; once the exec is gone, the block it last
showed (HELD-WINDOW), so a finished call of the standing round keeps it."
  (if-let (exec (transcript-background-row row backgrounds))
    (multiple-value-bind (rows plains) (exec-output-rows exec width 3 :expandable nil)
      (when rows
        (list (cdr (setf (held-window row)
                         (cons width (make-display-block :retention :live-only :kind :tool-call
                                                         :row exec :lines rows
                                                         :plain-lines plains)))))))
    (and held (eql width (car held)) (list (cdr held)))))

(defun tool-group-display-block (group width &optional backgrounds)
  "GROUP rendered: its title, then a run's listing (LISTING) or another
group's open members, or while closed its TAIL and any call clicked open under
it, with their canonical padding. A process title has one gap before its
first visible member. A listed call still running as one of BACKGROUNDS,
the state's live execs, shows what it has written under it."
  ;; The closed block is memoized on the group while its line, the width, the
  ;; theme and the hover stand; an open group, or one listing its calls,
  ;; composes on every frame, because the bodies it shows may still be moving
  ;; (a listed call's line is memoized on its row, RUN-TAIL-LLINE).
  ;; OPEN: the accordion holds the group or one of its members.
  (let* ((open (tool-group-open-p group))
         ;; The one line, present tense while a call runs: `Reading 2 files…'.
         (calls (remove :tool-call group.members :key #'transcript-row-kind :test-not #'eq))
         (running (some (lambda (row) (equal "running" row.tool-status))
                        calls))
         (activities (mapcar #'row-activity calls))
         ;; Worded again only when a call's activity or the tense moved: the
         ;; words are a function of those alone (ROW-ACTIVITY is memoized per
         ;; row), and wording a group -- its file links percent-encoded -- was
         ;; two fifths of a full render of a long session. A
         ;; recap has no calls and a line of its own.
         (process (eq :process (first group.key)))
         (mutation (eq :mutation (first group.key)))
         (disclosure (or process mutation))
         ;; The call whose diffs the run shows unasked (RUN-DIFF-CALL).
         (diff-call (and process (run-diff-call group.members)))
         ;; A run lists its trail a line each (LISTING), the one view live
         ;; and opened: the Eval, Think and Mutate cards it opened to were the
         ;; view before it.
         (listing (cond (open (and process (remove-if-not #'run-trail-row-p group.members)))
                        ;; A call clicked open under the shut title stays
                        ;; listed there past its round, until a click
                        ;; closes it.
                        ;; So does the run's newest call that changed files:
                        ;; what an edit changed is the turn's work, and shut
                        ;; away behind `Ran 2 commands' it read as commands
                        ;; run. An earlier edit gives it the place.
                        ((and process (or diff-call (some #'run-member-open-p group.members)))
                         (remove-if-not (lambda (row) (or (member row group.tail :test #'eq)
                                                          (run-member-open-p row)
                                                          (eq row diff-call)))
                                        group.members))
                        (t group.tail)))
         ;; A disclosure's state rides the gutter: ▸ shut, ▾ open.
         (role (if open :disclosure-open :disclosure))
         (words (cond
                  (mutation
                   ;; GROUP-MUTATION-ROWS keeps one receipt per file, so the members count them.
                   (let ((fresh (title-phrases-lline (list (list "Mutate" "" nil nil)) :verb-tone :success)))
                     (setf fresh.role role fresh.gutter-tone :muted)
                     (append-title-detail fresh (format nil "~d files" (length group.members)))
                     fresh))
                  (process
                   ;; A live run counts up as its calls land, `…' while one runs.
                   ;; Without this, one that has run nothing yet would say `Ran 0
                   ;; commands' and would read as nothing happening.
                   ;; A call that changed files counts as the files it
                   ;; edited, not as a command: `Edited 2 files +4 -2'.
                   (let* ((mutations (count-if #'row-notice-summary group.members))
                          (mutated (if (plusp mutations) (format nil ", Mutated ~d time~:p" mutations) ""))
                          (edited (remove-if-not #'row-edited-p calls))
                          (files (loop for row in edited
                                       append (loop for file across (json-array row.metadata "workspace_diff" "files")
                                                    collect (json-value file :text "path"))))
                          (commands (- (length calls) (length edited)))
                          (line (make-lline :text (cond ((and edited files)
                                                         (format nil "~:[Edited~;Ran ~:*~d command~:p, edited~] ~d file~:p~a~:[~;…~]"
                                                                 (and (plusp commands) commands)
                                                                 (length (remove-duplicates files :test #'equal))
                                                                 mutated running))
                                                        ((or calls (plusp mutations))
                                                         (format nil "Ran ~d command~:p~a~:[~;…~]" (length calls)
                                                                 mutated running))
                                                        ((some (lambda (row) (and (eq :reasoning row.kind) (thinking-p row)))
                                                               group.members)
                                                         "Thinking…")
                                                        (t "Thought"))
                                            :tone :muted :role role :gutter-tone :muted)))
                     (when (and edited files (not running))
                       (loop for row in edited
                             sum (loop for file across (json-array row.metadata "workspace_diff" "files")
                                       sum (or (json-value file :integer "added") 0))
                               into added
                             sum (loop for file across (json-array row.metadata "workspace_diff" "files")
                                       sum (or (json-value file :integer "removed") 0))
                               into removed
                             finally (append-lline-run line (format nil " +~d" added) :tone :diff-added)
                                     (append-lline-run line (format nil " -~d" removed) :tone :diff-removed)))
                     line))
                  ;; A reflection's recap as its one quiet line: `Reflection recap', and
                  ;; while it is closed the first words of its answer, cut to WIDTH so the
                  ;; line stays one row. The web page's recap summary, whose words hide
                  ;; while it is open: the answer is right under it then.
                  ((eq :recap (first group.key))
                   (let* ((answer (find-if #'assistant-text-p group.members))
                          (fresh (make-lline :text (if (and answer (not open))
                                                       (format nil "Reflection recap · ~a"
                                                               (nlk:one-line
                                                                (transcript-row-text answer)))
                                                       "Reflection recap")
                                             :tone :muted)))
                     (setf fresh.text (clip-text-to-width fresh.text (lline-widths fresh width)))
                     fresh))
                  ((equal (cons running activities) group.titled-from) group.line)
                  (t
                   (let ((fresh (title-phrases-lline
                                 (activity-entries-phrases
                                  (distinct-entries (loop for activity in activities
                                                          when activity append activity.entries))
                                  :running running))))
                     (dolist (call calls) (append-title-detail fresh (tool-note call)))
                     (when running (setf fresh.text (concatenate 'string fresh.text "…")))
                     (setf group.titled-from (cons running activities))
                     fresh))))
         ;; The title is WORDS, the line GROUP already holds kept while the two lay out
         ;; the same: one lline object per unchanged title, so LLINE-RENDER's memo holds.
         (title (if (and group.line (lline-content-equal group.line words))
                    group.line
                    (setf group.line words)))
         (hover (eq group *hover-row*)))
    (flet ((compose (&aux (llines (if disclosure (list (cons title group))
                                      (list (cons title group) (cons (block-pad-lline nil) group))))
                          ;; How many lines the member listed before this one took, NIL before
                          ;; the first.
                          (previous nil)
                          ;; The listed lines of calls still running, whose rows the ember warms.
                          (embers '()))
             (when (or listing (and open (not process)))
               ;; An open recap shows its answer and its divider; its
               ;; question, which no one here asked, stays in the log.
               (loop for member in (without-folded-receipts (or listing group.members))
                     for body = (cond
                                  (listing
                                   (cons (let ((line (run-tail-lline member width (call-receipts member group.members))))
                                           ;; A click opens it, so the pointer accents
                                           ;; it as it does the title.
                                           (cond ((eq member *hover-row*) (disclosure-hover-lline line))
                                                 (t (when (ember-call-p member) (push line embers))
                                                    line)))
                                         ;; A listed line clicked open shows what its
                                         ;; card held, two columns in under it, the
                                         ;; card's title aside.
                                         (append
                                          ;; A call that forked lists its forks under it,
                                          ;; the rail under the call's first word: stepped
                                          ;; in, it stood two columns right of everything.
                                          (fork-tree-llines member width)
                                          (cond ((run-member-open-p member)
                                                 (mapcar #'stepped-in (without-note member (rest (row-body-llines member width)))))
                                                ;; The run's newest call that changed
                                                ;; files shows them, each diff's head,
                                                ;; shut or open; an earlier one's line
                                                ;; opens its own.
                                                ((eq member diff-call)
                                                 (mapcar #'stepped-in (workspace-diff-llines member))))
                                          (listed-output member width backgrounds))))
                                  ((tool-group-p member) (list (tool-group-display-block member width backgrounds)))
                                  (mutation
                                   (cons (make-lline :text (format nil "  ~a" (kept-path-text (row-mutation-path member)))
                                                     :tone :muted)
                                         (when (detail-open-p member)
                                           (system-notice-llines (trim-block-text (transcript-row-text member)) nil))))
                                  ((not (reflection-recap-p member))
                                   (if (eq *tool-detail* :standard)
                                       (list (row-display-block member :width width))
                                       (row-body-llines member width))))
                     when (and body (not (and (display-block-p (first body))
                                              (null (display-block-lines (first body))))))
                       ;; A canvas row between the title and its first call,
                       ;; a listed run's too.
                       do (when (and process (null previous)
                                     (not (and (display-block-p (first body))
                                               (transcript-row-p (display-block-row (first body)))
                                               (row-block-padding-p (display-block-row (first body))))))
                            (push (cons (block-pad-lline nil) group) llines))
                          ;; A canvas row between two listed calls when either
                          ;; shows more than its line: run together, the next
                          ;; call's line read as more of the last one's output.
                          (when (and listing previous (or (> previous 1) (rest body)))
                            (push (cons (block-pad-lline nil) group) llines))
                          (unless disclosure (push (cons (block-pad-lline nil) group) llines))
                          (setf previous (length body))
                          (dolist (line body) (push (cons line member) llines))))
             ;; Process boundaries are spaced by TRANSCRIPT-LINE-TABLE,
             ;; reusing a neighbouring assistant's canonical canvas pad.
             (when (not disclosure)
               (push (cons (block-pad-lline nil) group) llines))
             (setf llines (nreverse llines))
             (when hover
               ;; Completed work is a text disclosure, not a filled card.
               ;; Accent only its title; keep the shared line and members intact.
               (setf llines (loop for (line . owner) in llines
                                  collect (cons (cond ((not disclosure)
                                                       (copy-with lline line
                                                                  :fill (or line.fill :hover)))
                                                      ((eq line title)
                                                       (disclosure-hover-lline title))
                                                      (t line))
                                                owner))))
             (loop for (line . owner) in llines
                   for block-p = (display-block-p line)
                   for (rows plain-rows) = (if block-p
                                               (list (display-block-lines line)
                                                     (display-block-plain-lines line))
                                               (multiple-value-list (lline-render line width)))
                   do (when (member line embers :test #'eq)
                        (dolist (row rows) (setf (ember-line row) owner)))
                   append rows into lines
                   append plain-rows into plains
                   append (or (and block-p (display-block-refs line))
                              (make-list (length rows) :initial-element owner))
                     into refs
                   finally (return (make-display-block
                                    :retention (if mutation :live-only :persisted-history)
                                    :kind :tool-call
                                    :row group
                                    :refs refs
                                    :lines lines
                                    :plain-lines plains)))))
      ;; A shut run listing only the settled call whose diffs it shows has the
      ;; same lines until the pointer or the accordion moves: those two ride
      ;; its memo, NIL for a run listing nothing so they never wake it.
      (if (or open (some (lambda (row) (or (member row group.tail :test #'eq) (run-member-open-p row)))
                         listing))
          (compose)
          (memoized (tool-group-block group) (title width *palette* *color-profile* hover disclosure
                                                    (and listing *hover-row*) (and listing *expanded-tool-call*))
            (compose))))))

(defun project-state (state &key (width 80) start end prompt)
  "Project conversation in chronological order. Pending execs replace their
own call's display block, never the fixed composer status. Subscribe snapshots
without a call row remain live-only blocks, in their registry order."
  (let* ((all (transcript-rows state))
         (backgrounds (state-background-rows state))
         (tail (or (and start (member start all :test #'eq)) all))
         (selected (if (and prompt start (notany #'transcript-user-prompt-row-p tail))
                       (let ((question (find-if #'transcript-user-prompt-row-p (ldiff all tail) :from-end t)))
                         (if question (cons question tail) tail))
                       tail))
         (rows (loop for row in selected until (eq row end)
                     unless (eq :pending-input (transcript-row-kind row)) collect row)))
    (append
     (loop for item in (group-rows rows all backgrounds)
           for exec = (transcript-background-row item backgrounds)
           for block = (if (tool-group-p item) (tool-group-display-block item width backgrounds)
                           (row-display-block (or exec item) :width width))
           ;; A turn's words in a place still fading (TURN-WORDS-ON).
           do (when-let (fade (and *fades* (assistant-text-p item)
                                   (gethash item.turn-id *turn-words*)))
                (when block (mark-fading-rows (display-block-lines block) fade item)))
           when (and block (display-block-lines block)) collect block)
     (unless end
       (loop for exec in backgrounds
             unless (some (lambda (row) (eq exec (transcript-background-row row backgrounds))) all)
               collect (row-display-block exec :width width))))))

;;; --- the projected tail ---------------------------------------------------
;;; A transcript projects from the row its window has grown to, not from its
;;; first: the window shows a few dozen lines of what can be thousands of rows.
;;; A state met for the first time — the replay an attach or a resume lands —
;;; projects only enough of its tail to fill *TAIL-PROJECTION-SCREENS* screens,
;;; and a session previewed one window (TRANSCRIPT-PROJECTION-START). Without
;;; this, a first projection of a large transcript would lay out every row
;;; before its first frame, stalling the UI thread. Scrolling to within a
;;; window of where the projection starts grows it (GROW-TRANSCRIPT-PROJECTION);
;;; Home and a found-block pin project it whole.

(defparameter *tail-projection-screens* 3
  "How many terminal heights of lines a state's first projection covers.")

(defun run-breaking-row-p (row)
  "Whether ROW closes any run of routine calls before it (GROUP-ROWS), so a
projection that starts at it groups every row after it as the whole one does."
  ;; A call closes one when it joins none -- it changed something, or failed.
  ;; A Standard call is taken as a boundary too: PROJECTION-GROUP-OPENING
  ;; moves any boundary inside a Standard run, live or completed, back to the
  ;; run's first row.
  (case row.kind
    (:tool-call (or (eq *tool-detail* :standard) (not (routine-call-p row))))
    ((:reasoning :pending-input) nil)
    (:text (not (and (equal "assistant" row.role)
                     (zerop (length (trim-block-text row.text))))))
    (t t)))

(defun projection-group-opening (rows index &aux (turn (transcript-row-turn-id (aref rows index))))
  "INDEX moved to its group's first row: a mutation, Standard process, or recap."
  (or (when (and turn (eq *tool-detail* :standard))
        ;; A Standard run is its turn's work trail, live or done (GROUP-ROWS).
        (flet ((work-p (row) (and (equal turn row.turn-id)
                                 (or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row)))))
          (let ((first (position-if #'work-p rows))
                (last (position-if #'work-p rows :from-end t)))
            (and first last (<= first index last) first))))
      (when (row-mutation-path (aref rows index))
        (loop while (and (plusp index) (row-mutation-path (aref rows (1- index)))
                         (equal turn (transcript-row-turn-id (aref rows (1- index)))))
              do (decf index)
              finally (return index)))
      (and turn (position-if (lambda (row) (and (reflection-recap-p row) (equal turn row.turn-id)))
                             rows :start (max 0 (- index 2)) :end (1+ index)))
      index))

(defun tail-projection-start (state width lines &optional from)
  "The latest row of STATE a projection can start at whose rows before row
FROM -- where a growing projection starts now; the end when NIL -- project
at WIDTH to LINES lines, or NIL for the first row."
  ;; The look walks back from FROM a run at a time -- a row that closes the
  ;; runs before it (RUN-BREAKING-ROW-P) and the calls after it, which group
  ;; as the whole projection groups them, and never a row inside a recap
  ;; (PROJECTION-GROUP-OPENING) -- and lays out only that run, so a projection takes in
  ;; at most one run past what it asked for. Without this, walking back
  ;; a row count at a time would lay out rows of every height many times
  ;; the lines asked for, and walking from the end would project the whole
  ;; tail at every look.
  (let* ((rows (coerce (transcript-rows state) 'vector))
         (end (or (and from (position from rows)) (length rows)))
         (total 0))
    (when (plusp end)
      (loop for index = (projection-group-opening rows (or (position-if #'run-breaking-row-p rows :end end :from-end t) 0))
            do (incf total (reduce #'+ (project-state state :width width :start (aref rows index)
                                                            :end (and (< end (length rows)) (aref rows end)))
                                   :key (lambda (block) (length block.lines))))
            until (or (zerop index) (>= total lines))
            do (setf end index)
            finally (return (and (plusp index) (aref rows index)))))))

;;; --- transcript rhythm -----------------------------------------------------

(defun transcript-line-table (blocks &aux (lines '())
                                          (plains '())
                                          (refs '())
                                          (previous-fill nil)
                                          (previous-heavy nil))
  "Flatten BLOCKS into parallel paint, copy and hit-test lanes."
  ;; Blocks stand one canvas row apart: two touching canvas pads share one
  ;; row, a canvas pad on either side is the row, and a user, filled or
  ;; process block with none gets an unowned one. Without this, two messages
  ;; would stand two rows apart and a message would sit one row off a run.
  ;; Never filter body-owned blank rows.
  (flet ((emit (painted plain ref)
           (push painted lines) (push plain plains) (push ref refs))
         (canvas-pad-p (plain owner)
           (and plain (zerop (length (string-trim '(#\Space) plain)))
                (transcript-row-p owner)
                (row-block-padding-p owner)
                (null (row-block-fill owner)))))
    (dolist (block blocks)
      (when (display-block-lines block)
        (let* ((fill block.fill)
               (heavy (or (and (transcript-row-p block.row)
                               (eq :text (transcript-row-kind block.row))
                               (equal "user" (transcript-row-role block.row)))
                          (and (tool-group-p block.row)
                               (eq :process (first (tool-group-key block.row))))))
               (opening-pad-p (canvas-pad-p (first block.plain-lines)
                                           (if block.refs (first block.refs) block.row)))
               (closing-pad-p (canvas-pad-p (first plains) (first refs))))
          (when lines
            (cond ((and closing-pad-p opening-pad-p)
                   ;; The earlier block gives its closing pad up: a block's
                   ;; own first row stays its pad (IMAGE-BAND-AT).
                   (pop lines) (pop plains) (pop refs))
                  ((or closing-pad-p opening-pad-p))
                  ((or (and previous-fill fill) previous-heavy heavy)
                   (emit "" "" nil))))
          (loop with refs = block.refs
                for line in (display-block-lines block)
                for plain-tail = block.plain-lines then (rest plain-tail)
                for owner = (if refs (pop refs) block.row)
                do (emit line (or (first plain-tail) "") owner))
          (setf previous-fill fill previous-heavy heavy)))))
  (values (nreverse lines) (nreverse plains) (nreverse refs)))

;;;; C2 additions to ui.lisp --- the live program adapter

;;; --- TUI model and generics ---------------------------------------------