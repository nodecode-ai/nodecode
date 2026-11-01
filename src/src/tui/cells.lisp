;;;; cells.lisp --- /cells: every cell on a sheet in the middle of the frame,
;;;; the chosen one laid open beside the list.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The sheet formats what the organism's cells routes answer (control.lisp):
;;;; the listing -- every cell this release ships and every folder this home
;;;; holds or the organism loaded, each as it stands there -- and the hub's
;;;; index, read on a route of its own so the list never waits for it. Home
;;;; lists this machine's cells, those that need a look first, then by where
;;;; they come from, the ones off last; Hub lists the index by what each
;;;; entry is. The chosen cell is laid open beside the list: what it is for,
;;;; every failure and skipped file whole, what it adds, its settings,
;;;; whether it runs, its folder. Space turns it on or off through the
;;;; organism (POST /api/gateway/cells), whose sentence is said over it; a
;;;; hub cell asks in place, where it comes from and that it runs with full
;;;; access in view, and a second Space installs it. Enter opens its section
;;;; in /settings. A wide window shows the list beside the cell; a narrow
;;;; one, one at a time. Live-only: the organism's records, read as the sheet
;;;; opens and answered anew by every switch.

(in-package #:nodecode.evolved)

;;; --- the sheet ------------------------------------------------------------------

(defstruct (cells-sheet (:include sheet-dialog))
  "/cells: the rows the listing route last answered (HOME), its standing
NOTICES and why it could not be read (FAILED); the hub's entries (HUB, NIL
until its index is read), why it could not be (HUB-FAILED) or was read from
the copy kept of it (HUB-STALE); where the operator is -- the TAB, :HOME or
:HUB, the cell CHOSEN by name, the QUERY, the first line the list and the
cell show (TOP, PANE-TOP) and, one view at a time, whether the cell is
OPEN -- the hub cell whose install the last Space ASKED, what the last
switch SAID ((NAME TEXT TONE)); and what the last paint left: the RAIL's
width, 0 when it showed one view at a time."
  (home #()) (notices #()) (failed nil) (hub nil) (hub-failed nil) (hub-stale nil)
  (tab :home) (chosen nil) (query "" :type string) (top 0) (pane-top 0) (open nil)
  (asked nil) (said nil) (rail 0))

(nlk:access (cells cells-sheet))

(defun row-name (row)
  (json-value row :string "name"))

(defun cell-of (cells name)
  "Everything CELLS knows of NAME as one object: the hub's entry under the
listing's row. Whether it is on is the listing's word alone -- its folder
is in the listing whenever it is in the home -- and `hub' says the index
offers it."
  (let ((home (find name cells.home :key #'row-name :test #'equal))
        (hub (find name cells.hub :key #'row-name :test #'equal))
        (cell (make-hash-table :test 'equal)))
    (dolist (row (list hub home))
      (when row (maphash (lambda (key value) (setf (gethash key cell) value)) row)))
    (setf (gethash "on" cell) (and home (json-value home :boolean "on"))
          (gethash "hub" cell) (and hub t))
    cell))

(defun cell-offered-p (cell)
  "Whether CELL is the hub's to install: offered there, and not on."
  (and (json-value cell :boolean "hub") (not (json-value cell :boolean "on"))))

(defun cell-fixed-p (cell)
  "Whether /cells cannot turn CELL off: the operator's own folder, or one
others stand on."
  (or (equal "own" (json-value cell :string "source"))
      (and (json-value cell :boolean "on") (json-value cell :text "kept"))))

(defun cell-match-p (row query)
  "Whether QUERY is in ROW's name, label or what it is for."
  (some (lambda (key) (let ((text (json-value row :string key)))
                        (and text (search query text :test #'char-equal))))
        '("label" "name" "description")))

;;; --- the list -------------------------------------------------------------------

(defparameter +home-groups+
  '((:needs "Needs you" :danger) (:shipped "Shipped" :system) (:hub "From the hub" :system)
    (:own "Your own" :system) (:off "Off" :system))
  "The heads Home lists its cells under, in order, each in its tone.")

(defun home-group (row)
  "The head of +HOME-GROUPS+ the listing's ROW stands under."
  (let ((source (json-value row :string "source")))
    (cond ((member (json-value row :string "level") '("error" "warning") :test #'equal) :needs)
          ((not (json-value row :boolean "on")) :off)
          ((equal source "shipped") :shipped)
          ((equal source "hub") :hub)
          (t :own))))

(defun hub-group (row)
  "The head the hub's entry ROW stands under: the folder of the index it is
in -- providers, tools, rooms -- else the index's own name."
  (let* ((path (json-value row :string "path"))
         (slash (and path (position #\/ path))))
    (cond (slash (string-capitalize (subseq path 0 slash)))
          ((equal "community" (json-value row :string "index")) "Community")
          (t "More"))))

(defun cells-items (cells &aux (query (string-trim " " cells.query)))
  "The list as it shows, each item (:HEAD LABEL TONE COUNT) or (:CELL NAME):
the tab's cells under their heads, by label; under a query, what it finds
in both, Home's first and a name once."
  (let ((items '()) (listed '()))
    (flet ((group (label tone rows)
             (when-let (rows (sort (remove-if (lambda (row) (member (row-name row) listed :test #'equal)) rows)
                                   #'string-lessp :key (lambda (row) (json-value row :string "label"))))
               (push (list :head label tone (length rows)) items)
               (dolist (row rows)
                 (push (row-name row) listed)
                 (push (list :cell (row-name row)) items))))
           (found (rows)
             (remove-if-not (lambda (row) (or (zerop (length query)) (cell-match-p row query)))
                            (coerce rows 'list))))
      (when (or (eq cells.tab :home) (plusp (length query)))
        (let ((rows (found cells.home)))
          (loop for (key label tone) in +home-groups+
                do (group label tone (remove key rows :key #'home-group :test-not #'eq)))))
      (when (or (eq cells.tab :hub) (plusp (length query)))
        (let ((rows (found cells.hub)))
          (dolist (label (remove-duplicates (mapcar #'hub-group rows) :test #'equal :from-end t))
            (group (if (plusp (length query)) (format nil "Hub · ~a" label) label)
                   :system (remove label rows :key #'hub-group :test-not #'equal))))))
    (nreverse items)))

(defun cells-names (items)
  (loop for item in items when (eq :cell (first item)) collect (second item)))

(defun cells-focus (cells names)
  "The name of the cell the keys act on: the one chosen while the list
shows it, else its first."
  (if (member cells.chosen names :test #'equal) cells.chosen (first names)))

;;; --- calls, off the loop --------------------------------------------------------

(defun cells-land (cells answer)
  "Land the listing route's ANSWER as CELLS's rows."
  (setf cells.home (json-array answer "cells")
        cells.notices (json-array answer "notices")
        cells.failed nil))

(defun cells-read (model cells)
  "Read the listing off the loop, and the hub's index apart from it: the
network's, so the list never waits for it. Each lands while CELLS is still
open."
  (sheet-call model cells
              (lambda () (gateway-model-http-call model :get "/api/gateway/cells" nil))
              (lambda-ignoring (model answer)
                (if-let (failure (off-loop-failure answer))
                  (setf cells.failed failure)
                  (cells-land cells answer)))
              "Reading cells…")
  (off-loop model
            (lambda () (gateway-model-http-call model :get "/api/gateway/cells/hub" nil))
            (lambda (model answer)
              (if (not (eq cells (chat-model-dialog model)))
                  :quiet
                  (progn (if-let (failure (or (off-loop-failure answer) (json-value answer :text "error")))
                           (setf cells.hub-failed failure)
                           (setf cells.hub (json-array answer "cells")
                                 cells.hub-stale (json-value answer :text "stale")
                                 cells.hub-failed nil))
                         t)))))

(defun cells-switch (model cells name on &optional commit)
  "Turn NAME ON or off through the organism, a hub cell at COMMIT, the pin
it was read at; the listing it answers lands, and what came of it is said
over the cell, in the tone of how it now stands, or why it was refused."
  (sheet-call model cells
              (lambda ()
                ;; a hub install clones before it loads
                (handler-case (gateway-model-http-call model :post "/api/gateway/cells"
                                                       (json-object "name" name "on" on :opt "commit" commit
                                                                    "seconds" nlk:*cell-switch-seconds*)
                                                       :read-timeout nlk:*cell-switch-seconds*)
                  (gateway-http-error (refusal) (list :refused (gateway-http-error-detail refusal)))))
              (lambda-ignoring (model answer)
                (if-let (refused (or (off-loop-failure answer)
                                     (and (consp answer) (eq :refused (first answer)) (second answer))))
                  (setf cells.said (list name refused :danger))
                  (progn (cells-land cells answer)
                         (let ((row (find name cells.home :key #'row-name :test #'equal)))
                           (setf cells.said (list name (answer-text answer)
                                                  (if (and on row (eq :needs (home-group row)))
                                                      :danger
                                                      :success)))))))
              (format nil "Turning ~a ~:[off~;on~]…" (cell-label name) on)))

(defun cells-space (model cells name)
  "Space on NAME: off when it is on, on when it is off; a hub cell asks
first, in place, and a second Space installs it at the commit read."
  (let* ((cell (cell-of cells name))
         (on (json-value cell :boolean "on")))
    (cond (cells.busy)
          ((and (cell-offered-p cell) (not (equal cells.asked name)))
           (setf cells.asked name))
          (t (setf cells.asked nil)
             (cells-switch model cells name (not on)
                           (and (cell-offered-p cell) (json-value cell :text "commit")))))))

(defun cells-settings (model cells name)
  "Open the section NAME declares in /settings."
  (when-let (section (json-value (cell-of cells name) :text "section"))
    (slash-settings-op model section)))

;;; --- the keys -----------------------------------------------------------------

(defun cells-choose (cells name)
  (unless (equal name cells.chosen)
    (setf cells.pane-top 0 cells.asked nil))
  (setf cells.chosen name))

(defun cells-move (cells names delta)
  "Choose the cell DELTA along NAMES, stopping at its ends."
  (when names
    (let ((at (or (position (cells-focus cells names) names :test #'equal) 0)))
      (cells-choose cells (nth (max 0 (min (1- (length names)) (+ at delta))) names)))))

(defun cells-search (cells query)
  "Make QUERY what the list finds, staying on the chosen cell while it does."
  (setf cells.query query cells.top 0))

(defun cells-tab (cells tab)
  "Show TAB's cells, from its first."
  (unless (eq tab cells.tab)
    (setf cells.tab tab cells.chosen nil cells.top 0 cells.pane-top 0 cells.asked nil cells.open nil)))

(defun cells-key (model cells key &key ctrl alt shift)
  "Every key the shell's chords leave the open sheet."
  (declare (ignore shift))
  (let* ((names (cells-names (cells-items cells)))
         (name (cells-focus cells names))
         (asked cells.asked))
    ;; any key but Space leaves a hub cell's question
    (unless (eql key #\Space) (setf cells.asked nil))
    (cond
      ((and asked (eq key :escape)))
      ;; typing searches; Space stays the switch
      ((sheet-type-query cells.query key ctrl alt (lambda (query) (cells-search cells query))))
      ((eql key #\Space) (when name (cells-space model cells name)))
      ((eq key :escape)
       (cond ((plusp (length cells.query)) (cells-search cells ""))
             ((and cells.stacked cells.open) (setf cells.open nil))
             (t (dismiss-dialog model nil))))
      ((member key '(:up :down)) (cells-move cells names (if (eq key :up) -1 1)))
      ((eq key :home) (when names (cells-choose cells (first names))))
      ((eq key :end) (when names (cells-choose cells (car (last names)))))
      ((member key '(:page-up :page-down))
       (setf cells.pane-top (max 0 (+ cells.pane-top (if (eq key :page-up) -10 10)))))
      ((and (member key '(:left :right)) cells.stacked cells.open)
       (when (eq key :left) (setf cells.open nil)))
      ((member key '(:left :right)) (cells-tab cells (if (eq key :left) :home :hub)))
      ((eq key :tab) (cells-tab cells (if (eq cells.tab :home) :hub :home)))
      ((and (eq key :enter) cells.stacked (not cells.open))
       (when name (setf cells.open t cells.pane-top 0)))
      ((eq key :enter) (when name (cells-settings model cells name))))))

(defun cells-paste (model cells text)
  "A paste lands in the query."
  (declare (ignore model))
  (cells-search cells (clip (concatenate 'string cells.query text) *max-dialog-search-chars* :ellipsis "")))

(defun cells-wheel (model cells delta x y)
  "A notch on the list moves the choice; on the cell, it scrolls what the
cell says."
  (declare (ignore model y))
  (if (if cells.stacked cells.open (>= (- x (second cells.layout)) cells.rail))
      (setf cells.pane-top (max 0 (+ cells.pane-top delta)))
      (cells-move cells (cells-names (cells-items cells)) delta)))

;;; --- what the chosen cell says --------------------------------------------------

(defun slab-ground (tone)
  "The ground of a slab that says something in TONE: the tone washed over
the pane's, the question's own for :SHEET-ASK."
  (if (eq tone :sheet-ask)
      (sheet-ground :sheet-ask)
      (blend-rgb (visual-style-fg (style-for-tone tone)) (sheet-ground :sheet) 16)))

(defun short-commit (commit)
  (subseq commit 0 (min 7 (length commit))))

(defun hub-source (cell)
  "Where CELL comes from on the hub: its repository, and the folder in it."
  (format nil "~a~@[/~a~]"
          (ppcre:regex-replace "\\A(https://)?(.*?)(\\.git)?\\z" (or (json-value cell :string "git") "") "\\2")
          (json-value cell :text "path")))

(defun pack-segs (segs width)
  "SEGS, (TEXT TONE [BOLD]) runs, packed into lines WIDTH columns wide, each
run whole on its line; a blank run never opens one."
  (let ((lines '()) (line '()) (used 0))
    (dolist (seg segs)
      (let ((wide (text-display-width (first seg))))
        (when (and line (> (+ used wide) width))
          (push (nreverse line) lines)
          (setf line '() used 0))
        (unless (and (null line) (zerop (length (string-trim " " (first seg)))))
          (push seg line)
          (incf used wide))))
    (when line (push (nreverse line) lines))
    (or (nreverse lines) (list '()))))

(defun cell-runs (cell)
  "=> (values WORDS TONE): whether CELL runs in the organism, or NIL when
nothing of it is loaded there. An organism older than the listing's
`started' says nothing of whether a peripheral runs, and neither does this."
  (let ((on (json-value cell :boolean "on"))
        (loaded (json-value cell :boolean "loaded"))
        (kind (json-value cell :text "kind"))
        (reported (nth-value 1 (json-value cell :any "started")))
        (failures (json-array cell "failures")))
    (cond ((find-if-not (lambda (failure) (json-value failure :text "file")) failures)
           (values "× did not load" :danger))
          ((json-value cell :text "refused") (values "× its start was refused" :danger))
          ((plusp (length failures))
           (values (format nil "! loaded without ~d file~:p" (length failures)) :sheet-restart))
          ((json-value cell :boolean "started") (values "● running" :success))
          ((and kind (not on)) (values "off: its code stays loaded until a restart" :muted))
          ((and loaded (equal kind "peripheral")) (values (if reported "loaded, not running" "loaded") :muted))
          (loaded (values "loaded: nothing to start, it answers when called" :muted))
          (on (values "on: it loads at the next start" :muted)))))

(defun cell-state-glyph (cell)
  "=> (values GLYPH TONE): what of CELL wants a look at a glance -- failing,
something to say, running, waiting to load -- or NIL."
  (let ((level (json-value cell :string "level")))
    (cond ((equal level "error") (values "×" :danger))
          ((equal level "warning") (values "!" :sheet-restart))
          ((json-value cell :boolean "started") (values "●" :success))
          ((and (json-value cell :boolean "on") (not (json-value cell :text "kind"))) (values "◦" :muted)))))

(defun cell-source-segs (cell)
  "Where CELL comes from, as runs, and what turning it on brings with it."
  (let ((on (json-value cell :boolean "on"))
        (source (json-value cell :string "source"))
        (brings (coerce (json-array cell "brings") 'list)))
    (append
     (list (list (cond ((equal source "shipped") "Ships with nodecode")
                       ((and on (equal source "hub"))
                        (format nil "From the hub~@[ at ~a~]"
                                (and (json-value cell :text "commit") (short-commit (json-value cell :string "commit")))))
                       ((json-value cell :boolean "hub")
                        (if (equal "community" (json-value cell :string "index")) "On the community hub" "On the hub"))
                       ((equal source "own") "Your own folder: /cells never takes it out")
                       (t "Its folder is gone from this home"))
                 :muted))
     (when (and (not on) brings)
       (list (list " · turning it on brings " :muted) (list (format nil "~{~a~^, ~}" brings) :foreground))))))

(defun cell-lines (cells name width)
  "What the pane says of NAME, WIDTH wide, top to bottom, each line one of
(:TEXT SEGS ACTION), (:SLAB GROUND SEGS), or (:LIP GROUND SIDE), the half
row that opens or closes a slab. Errors and refusals are said whole."
  (let* ((cell (cell-of cells name))
         (lines '())
         (on (json-value cell :boolean "on"))
         (offered (cell-offered-p cell))
         (label-width 10)
         (room (max 8 (- width label-width))))
    (labels ((add (&rest line) (push line lines))
             (blank () (add :text '()))
             (wrap (text width)
               (loop for part in (or (split-lines text) '(""))
                     append (or (wrapped-lines part width) '(""))))
             (para (text tone)
               (dolist (piece (wrap text width)) (add :text (list (list piece tone)))))
             (slab (ground specs)
               (add :lip ground :top)
               (loop for (text tone bold) in specs
                     do (dolist (piece (wrap text (- width 4)))
                          (add :slab ground (list (list piece tone bold)))))
               (add :lip ground :bottom)
               (blank))
             (label (head) (list (if head (format nil "~va" label-width head) (make-string label-width :initial-element #\Space))
                                 :muted))
             (fact (head segs &optional action)
               (loop for line in (pack-segs segs room)
                     for first = t then nil
                     do (add :text (cons (label (and first head)) line) action)))
             (fact-text (head text tone)
               (loop for piece in (wrap text room)
                     for first = t then nil
                     do (add :text (list (label (and first head)) (list piece tone))))))
      ;; what the last switch of it came to, then what it is for
      (when (equal name (first cells.said))
        (destructuring-bind (text tone) (rest cells.said)
          (slab (slab-ground tone) (list (list text :foreground)))))
      (cond ((json-value cell :text "description") (para (json-value cell :string "description") :foreground))
            ((equal "own" (json-value cell :string "source"))
             (para "Its .asd does not say what it is for." :muted)))
      (blank)
      (add :text (cell-source-segs cell))
      (blank)
      ;; what stands in its way, whole
      (let ((failures (json-array cell "failures")))
        (when (plusp (length failures))
          (slab (slab-ground :danger)
                (cons (list (if (find-if-not (lambda (failure) (json-value failure :text "file")) failures)
                                "It did not load"
                                (format nil "~d file~:p did not load" (length failures)))
                            :danger t)
                      (loop for failure across failures
                            collect (list (format nil "~@[~a: ~]~a" (json-value failure :text "file")
                                                  (json-value failure :string "text"))
                                          :foreground))))))
      (when-let (refused (json-value cell :text "refused"))
        (slab (slab-ground :danger) (list (list "Its start was refused" :danger t) (list refused :foreground))))
      (when-let (warnings (append (loop for skip across (json-array cell "skipped")
                                        collect (format nil "Skipped ~a: ~a" (json-value skip :string "file")
                                                        (json-value skip :string "reason")))
                                  (loop for collision across (json-array cell "collides")
                                        collect (format nil "Collides: ~a" collision))
                                  (unless on
                                    (loop for system across (json-array cell "missing")
                                          collect (format nil "Needs ~a, which this nodecode cannot find" system)))))
        (slab (slab-ground :sheet-restart) (mapcar (lambda (text) (list text :foreground)) warnings)))
      ;; the question a hub cell asks before it runs here
      (when offered
        (slab (slab-ground :sheet-ask)
              (append (list (list (format nil "From ~a~@[ at ~a~]." (hub-source cell)
                                          (and (json-value cell :text "commit")
                                               (short-commit (json-value cell :string "commit"))))
                                  :foreground t)
                            (list "It runs inside Nodecode with full access to your files and keys." :foreground))
                      (when cells.hub-stale
                        ;; the reason names the index and what came of reading it
                        (list (list (format nil "This is the copy last read: ~a" cells.hub-stale)
                                    :muted)))
                      (when (equal cells.asked name)
                        (list (list "Space again installs it; any other key leaves it." :sheet-restart t))))))
      (when-let (kept (and on (json-value cell :text "kept")))
        (para (format nil "Stays on: ~a." kept) :muted)
        (blank))
      ;; what it is, as facts
      (loop for notice across cells.notices
            for key = (json-value notice :string "key")
            when (and key (or (string= key name) (uiop:string-prefix-p (format nil "~a:" name) key)))
              do (fact-text "Says" (json-value notice :string "text") :foreground))
      (let ((commands (json-array cell "commands"))
            (defines (json-value cell :integer "defines")))
        (cond ((or (plusp (length commands)) defines)
               (fact "Adds" (append (loop for command across commands
                                          append (list (list (format nil " /~a " command) :key-chip) (list " " :muted)))
                                    (and defines (list (list (format nil "~d definition~:p" defines) :muted))))))
              ((not (json-value cell :text "kind")) (fact "Adds" (list (list "known once it loads" :muted))))))
      (when-let (section (json-value cell :text "section"))
        (fact "Settings" (list (list section :session-title) (list "   enter opens it" :muted))
              (lambda (model cells) (cells-settings model cells name))))
      (multiple-value-bind (words tone) (cell-runs cell)
        (when words (fact "Runs" (list (list words tone)))))
      (loop for slow across (json-array cell "slow")
            do (fact-text "Slow" (format nil "~a took ~d ms to load; every direct launch pays it"
                                         (json-value slow :string "file") (json-value slow :integer "ms"))
                          :sheet-restart))
      ;; an offered one says where it comes from in its question
      (when (and (json-value cell :text "git") (not offered))
        (fact-text "Source" (hub-source cell) :muted))
      (when-let (folder (json-value cell :text "folder"))
        (fact-text "Folder" folder :muted)
        (when-let (from (json-value cell :text "from"))
          (fact-text nil (format nil "→ ~a" from) :muted))))
    (nreverse lines)))

;;; --- painting -----------------------------------------------------------------

(defun cells-keys (cells name)
  "The keys that act where the operator is, in the order the key row names them."
  (let* ((cell (and name (cell-of cells name)))
         (on (and cell (json-value cell :boolean "on"))))
    (if (and name (equal cells.asked name))
        '(("space" "install it") ("esc" "leave it"))
        (append
         (cond ((null cell) nil)
               ((cell-offered-p cell) '(("space" "install")))
               ((cell-fixed-p cell) nil)
               (t (list (list "space" (if on "turn off" "turn on")))))
         (cond ((and cells.stacked (not cells.open)) (and cell '(("enter" "open"))))
               ((and cell (json-value cell :text "section")) '(("enter" "settings"))))
         (if (and cells.stacked cells.open)
             '(("←" "back"))
             (list (list "←→" (if (eq cells.tab :home) "hub" "home"))))
         (if (plusp (length cells.query)) '(("esc" "clear")) '(("type" "search") ("esc" "close")))))))

(defun cells-empty-text (cells)
  "What the list says while it shows nothing."
  (let ((query (string-trim " " cells.query)))
    (cond ((plusp (length query)) (format nil "Nothing matches “~a”" query))
          ((eq cells.tab :hub)
           (cond (cells.hub-failed "The hub could not be read")
                 ((null cells.hub) "Reading the hub…")
                 (t "The hub lists nothing this machine runs")))
          (cells.failed nil)
          (cells.busy "Reading cells…")
          (t "No cells"))))

(defun paint-cells-tabs (cells canvas r x width ground)
  "The tabs side by side from X on R, the one shown filled, each with its
count; at the right, how this machine's cells stand when there is room."
  (let ((query (string-trim " " cells.query))
        (at x))
    (loop for (tab label rows) in (list (list :home "Home" cells.home) (list :hub "Hub" cells.hub))
          for text = (if (and (eq tab :hub) (null rows))
                         (format nil " ~a " label)
                         (format nil " ~a ~d " label (if (plusp (length query))
                                                         (count-if (lambda (row) (cell-match-p row query)) rows)
                                                         (length rows))))
          for wide = (text-display-width text)
          while (<= (+ at wide) (+ x width))
          do (canvas-put canvas at r text (if (eq tab cells.tab)
                                              (sheet-ink :sheet-chip :bold t)
                                              (sheet-ink :muted :ground ground)))
             (sheet-hit cells r at (+ at wide)
                        (let ((tab tab))
                          (lambda-ignoring (model cells) (cells-tab cells tab))))
             (incf at (1+ wide)))
    (let* ((running (and (some (lambda (row) (nth-value 1 (json-value row :any "started"))) cells.home)
                         (count-if (lambda (row) (json-value row :boolean "started")) cells.home)))
           (needs (count :needs cells.home :key #'home-group))
           (summary (format nil "~{~a~^ · ~}"
                            (remove nil (list (and running (format nil "~d running" running))
                                              (and (plusp needs) (format nil "~d need~:[s~;~] you" needs (/= needs 1))))))))
      (when (and (plusp (length summary)) (<= (+ at 4 (text-display-width summary)) (+ x width)))
        (canvas-put canvas (- (+ x width) (text-display-width summary)) r summary
                    (sheet-ink :muted :ground ground))))))

(defun paint-cells-row (cells canvas r x width name chosen ground)
  "NAME's row on R from X, WIDTH wide: its switch, its label and what of it
wants a look, lifted when CHOSEN."
  (let* ((cell (cell-of cells name))
         (lift (sheet-ground :sheet-focus))
         (here (if chosen lift ground))
         (on (json-value cell :boolean "on"))
         (offered (cell-offered-p cell)))
    (when chosen
      (canvas-fill canvas x r width 1 (sheet-ink nil :ground lift))
      (when (eq (effective-color-profile) :none) (canvas-put canvas x r "›" nil)))
    (canvas-put canvas (+ x 2) r (cond (offered "+ ") (on "━●") (t "●━"))
                (cond (offered (sheet-ink :muted :ground here))
                      ((and on (not (cell-fixed-p cell))) (sheet-ink (sheet-ground :sheet-chip-on) :ground here))
                      (on (sheet-ink :muted :ground here))
                      (t (sheet-ink :sheet-undone :ground here))))
    (canvas-put canvas (+ x 5) r (clip-text-to-width (or (json-value cell :string "label") name) (max 1 (- width 8)))
                (sheet-ink (cond (chosen :dialog-accent) ((or on offered) :foreground) (t :muted))
                           :ground here :bold chosen))
    (multiple-value-bind (glyph tone) (cell-state-glyph cell)
      (when glyph (canvas-put canvas (+ x width -2) r glyph (sheet-ink tone :ground here))))
    (sheet-hit cells r x (+ x width)
               (lambda-ignoring (model cells)
                 (if (and chosen cells.stacked)
                     (setf cells.open t cells.pane-top 0)
                     (cells-choose cells name))))))

(defun paint-cells-list (cells canvas items name top rows x width ground)
  "ITEMS down ROWS rows from TOP, from X, WIDTH wide, on GROUND: a head over
each group, a blank row between groups, scrolled to keep NAME in view, its
head with it when there is room."
  (let* ((lines (loop for item in items
                      for first = t then nil
                      when (and (eq :head (first item)) (not first)) collect (list :gap)
                      collect item))
         (at (or (position-if (lambda (line) (and (eq :cell (first line)) (equal name (second line)))) lines) 0))
         (offset (max 0 (min cells.top at) (- at rows -1))))
    (loop while (and (plusp offset) (member (first (nth (1- offset) lines)) '(:head :gap))
                     (< (- at (1- offset)) rows))
          do (decf offset))
    (setf offset (max 0 (min offset (- (length lines) rows)))
          cells.top offset)
    (loop for i from 0 below rows
          for line = (nth (+ offset i) lines)
          for r = (+ top i)
          while line
          do (case (first line)
               (:head (destructuring-bind (label tone count) (rest line)
                        (let ((end (canvas-put canvas (+ x 3) r (clip-text-to-width label (max 1 (- width 9)))
                                               (sheet-ink tone :ground ground :bold t))))
                          (canvas-put canvas (+ end 2) r (princ-to-string count) (sheet-ink :muted :ground ground)))))
               (:cell (paint-cells-row cells canvas r x width (second line) (equal name (second line)) ground))))
    (paint-scroll-marks canvas (+ x width -1) top (+ top rows -1) offset rows (length lines) ground)))

(defun cell-switch-chips (cells cell ground)
  "CELL's switch as (TEXT INK ACTION) chips on GROUND: on and off, the one
it is filled; install for a hub cell; for one /cells cannot turn off, why."
  (let* ((name (json-value cell :string "name"))
         (on (json-value cell :boolean "on"))
         (none (eq (effective-color-profile) :none))
         (flip (lambda (model cells) (cells-space model cells name))))
    (flet ((held (text ink) (list (if none (format nil "[~a]" (string-trim " " text)) text) ink))
           (free (text action) (list text (sheet-ink :muted :ground ground) action)))
      (cond ((cell-offered-p cell)
             (list (append (held " install " (sheet-ink (if (equal cells.asked name) :sheet-chip-on :sheet-chip-set)))
                           (list flip))))
            ((equal "own" (json-value cell :string "source"))
             (list (free "your own folder" nil)))
            ((cell-fixed-p cell)
             (list (append (held " on " (sheet-ink :sheet-chip)) (list nil)) (free "  kept on" nil)))
            (on (list (append (held " on " (sheet-ink :sheet-chip-on)) (list nil)) (free " off " flip)))
            (t (list (free " on " flip) (append (held " off " (sheet-ink :sheet-chip)) (list nil))))))))

(defun paint-cell-band (cells canvas r px pw name)
  "NAME's head on R: its label on a band that fades into the pane, and its
switch at the right."
  (let* ((cell (cell-of cells name))
         (pane (sheet-ground :sheet))
         (lift (sheet-ground :sheet-focus))
         (chips (cell-switch-chips cells cell pane))
         (chips-width (reduce #'+ chips :key (lambda (chip) (text-display-width (first chip)))))
         (room (max 4 (- pw chips-width 10)))
         (at (canvas-put canvas px r (clip-text-to-width (format nil " ~a " (json-value cell :string "label")) room)
                         (sheet-ink :title :ground lift :bold t))))
    (loop for step from 1 to 8
          while (< at (- (+ px pw) chips-width 1))
          do (setf at (canvas-put canvas at r " " (sheet-ink nil :ground (blend-rgb pane lift (round (* 100 step) 9))))))
    (let ((x (- (+ px pw) chips-width)))
      (loop for (text ink action) in chips
            do (let ((end (canvas-put canvas x r text ink)))
                 (when action (sheet-hit cells r x end action))
                 (setf x end))))))

(defun paint-cell-pane (cells canvas name top bottom px pw)
  "The cell NAME laid open from TOP to BOTTOM in the pane at PX, PW wide:
its band, then what it says, scrolled from PANE-TOP; with no cell, why the
list could not be read."
  (let ((pane (sheet-ground :sheet)))
    (if (null name)
        (when-let (failure (or cells.failed (and (eq cells.tab :hub) cells.hub-failed)))
          (loop for line in (loop for part in (split-lines failure) append (wrapped-lines part pw))
                for r from top to bottom
                do (canvas-put canvas px r line (sheet-ink :danger :ground pane))))
        (let* ((rows (max 0 (- bottom top 1)))
               (lines (cell-lines cells name pw))
               (offset (max 0 (min cells.pane-top (- (length lines) rows)))))
          (paint-cell-band cells canvas top px pw name)
          (setf cells.pane-top offset)
          (loop for i from 0 below rows
                for line = (nth (+ offset i) lines)
                for r = (+ top 2 i)
                while line
                do (destructuring-bind (kind &rest parts) line
                     (ecase kind
                       (:text (destructuring-bind (segs &optional action) parts
                                (paint-segs canvas r px pw segs pane)
                                (when action (sheet-hit cells r px (+ px pw) action))))
                       (:slab (destructuring-bind (ground segs) parts
                                (canvas-fill canvas px r pw 1 (sheet-ink nil :ground ground))
                                (paint-segs canvas r (+ px 2) (- pw 4) segs ground)))
                       (:lip (destructuring-bind (ground side) parts
                               (unless (eq (effective-color-profile) :none)
                                 (canvas-put canvas px r (make-string pw :initial-element (if (eq side :top) #\▄ #\▀))
                                             (sheet-ink ground :ground pane))))))))
          (paint-scroll-marks canvas (+ px pw 1) (+ top 2) bottom offset rows (length lines) pane)))))

(defun paint-cells-wide (cells canvas edges)
  "The wide sheet: the list in a rail beside the chosen cell. => the
caret's cell, or NIL."
  (with-wide-sheet (canvas edges (if (>= width 110) 30 28))
    (let* ((gap (if (< inner 22) 1 2))
           (title-row (+ top (1- gap)))
           (hint-row (- (+ top inner) gap))
           (items (cells-items cells))
           (name (cells-focus cells (cells-names items)))
           (cursor nil))
      (setf cells.rail rail-width)
      (canvas-put canvas 3 title-row "Cells" (sheet-ink :title :ground rail :bold t))
      (if items
          (paint-cells-list cells canvas items name (+ title-row gap) (- hint-row title-row gap -1)
                            0 rail-width rail)
          (when-let (text (cells-empty-text cells))
            (canvas-put canvas 3 (+ title-row gap) (clip-text-to-width text (- rail-width 4))
                        (sheet-ink :muted :ground rail))))
      ;; the search field
      (setf cursor (paint-search-field canvas px title-row pw cells.query "Search cells and the hub" field))
      (paint-cells-tabs cells canvas (+ title-row gap) px pw pane)
      (paint-cell-pane cells canvas name (+ title-row gap gap) (- hint-row gap) px pw)
      (paint-keys cells canvas hint-row px (+ px pw) (cells-keys cells name) cells.flash pane)
      cursor)))

(defun paint-cells-stacked (cells canvas edges)
  "The narrow sheet: the list, or the chosen cell, one view at a time. =>
the caret's cell, or NIL."
  (with-stacked-sheet (canvas edges)
    (let* ((items (cells-items cells))
           (name (cells-focus cells (cells-names items)))
           (open (and cells.open name))
           (cursor nil))
      (setf cells.rail 0)
      (cond
        (open
         (let ((at (canvas-put canvas px header-row "Cells › " (sheet-ink :muted :ground pane))))
           (sheet-hit cells header-row px at
                      (lambda-ignoring (model cells) (setf cells.open nil)))
           (canvas-put canvas at header-row
                       (clip-text-to-width (json-value (cell-of cells name) :string "label") (max 1 (- (+ px pw) at)))
                       (sheet-ink :title :ground pane :bold t))))
        ((plusp (length cells.query))
         (let ((at (canvas-put canvas px header-row "Search " (sheet-ink :muted :ground pane))))
           (setf cursor (cons (canvas-put canvas at header-row (clip-tail cells.query (max 1 (- pw 8)))
                                          (sheet-ink :foreground :ground pane))
                              header-row))))
        (t
         (let ((at (canvas-put canvas px header-row "Cells" (sheet-ink :title :ground pane :bold t))))
           (paint-cells-tabs cells canvas header-row (+ at 3) (- (+ px pw) at 3) pane))))
      (cond (open (paint-cell-pane cells canvas name (+ header-row 2) (- hint-row 2) px pw))
            (items (paint-cells-list cells canvas items name (+ header-row 2) (- hint-row header-row 3)
                                     (- px 2) (+ pw 4) pane))
            (t (when-let (text (cells-empty-text cells))
                 (canvas-put canvas px (+ header-row 2) (clip-text-to-width text pw) (sheet-ink :muted :ground pane)))
               (paint-cell-pane cells canvas nil (+ header-row 4) (- hint-row 2) px pw)))
      (paint-keys cells canvas hint-row px (+ px pw) (cells-keys cells name) cells.flash pane)
      cursor)))

(defparameter +cells-sheet-size+ '(120 34)
  "The columns and rows the sheet asks for: what a wide window gives it.")

(defun cells-paint (model cells width height)
  "The sheet for a WIDTH x HEIGHT frame: wide from 84 columns, the list
beside the chosen cell; narrower, one view at a time."
  (paint-sheet model cells width height +cells-sheet-size+ #'paint-cells-wide #'paint-cells-stacked))

(defun open-cells-dialog (model)
  "/cells on a shell: the sheet at once over the shell, the listing and the
hub's index read off the loop."
  (let ((cells (make-cells-sheet :kind :cells :title "Cells"
                                 :paint #'cells-paint :key #'cells-key :paste #'cells-paste
                                 :click #'sheet-click :wheel #'cells-wheel)))
    (setf (chat-model-dialog model) cells)
    (cells-read model cells)
    nil))
