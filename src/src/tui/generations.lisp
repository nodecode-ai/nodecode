;;;; generations.lisp --- /generations: the history of the self on a sheet in
;;;; the middle of the frame, and the way back to a change in it.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The sheet formats what the gateway's generations routes answer
;;;; (gateway.lisp): the list, every generation folded into the change it was
;;;; and said in words (NLK:GENERATION-HISTORY), with why going back to one can
;;;; never work; the plan, what one restore would move, read before anything
;;;; does; the edits, uncommitted ones kept or discarded; the rollback. A wide
;;;; window shows the days in a rail beside the changes, newest first across
;;;; days; a narrow one shows the days, or the changes, one view at a time.
;;;; The row the operator stands on is the state going back leaves the folder
;;;; in: the newer rows it would undo grey, an ↶ beside each, and a red line
;;;; marks how far back anything can come. Enter asks in place, and Esc on the
;;;; question stays on the row.

(in-package #:nodecode.evolved)

;;; --- the changes, as the list route answers them ------------------------------

(defstruct history-change
  "One row of the history as the list route says it -- KIND, the
generations FROM and TO it spans, TITLE and WHERE in words, its NOTES, an
agent's KEEPS, the layer's FILES (COUNT ADDED REMOVED PATHS), the binary
VERSION at TO, why going back to it can never work (REFUSAL), whether
nodecode alone moved in it (NODECODE) -- and when, on the operator's clock:
AT and FIRST-AT universal times, CLOCK and FIRST-CLOCK their wall clock, DAY
its day's place on the rail, the newest 0."
  kind from to title where notes keeps files version refusal nodecode
  at first-at clock first-clock day)

(nlk:access (change history-change))

(defstruct history-day
  "One day on the rail: its LABEL (Today, Yesterday, Wed 14 Oct), the LONG
label that says its date too, and the KEY (YEAR MONTH DATE) it is."
  label long key)

(nlk:access (day history-day))

(defparameter +history-kinds+
  '(("agent" "✦" :success "agent change" "agent changes" :agent)
    ("nodecode" "↑" :tool "nodecode update" "nodecode updates" :nodecode)
    ("install" "+" :success "install" "installs" :cells)
    ("remove" "−" :danger "removal" "removals" :cells)
    ("cell" "•" :system "cell change" "cell changes" :cells)
    ("edits" "~" :muted "uncommitted snapshot" "uncommitted snapshots" nil)
    ("restore" "↺" :sheet-restart "restore" "restores" nil)
    ("first" "○" :muted "first generation" "first generations" nil))
  "Each kind of change, in the order a count of them reads: its glyph, the
tone it is painted in, its words for one and for many, and the filter
besides All that shows it.")

(defparameter +history-filters+ '((:all "All") (:agent "Agent") (:nodecode "nodecode") (:cells "Cells"))
  "The filters Tab steps through, in order.")

(defun kind-entry (kind)
  (or (assoc kind +history-kinds+ :test #'equal) (car (last +history-kinds+))))

(defun change-shown-p (change filter)
  "Whether FILTER, one of +HISTORY-FILTERS+, shows CHANGE."
  (or (eq filter :all) (eq filter (sixth (kind-entry change.kind)))))

(defun wall-clock (universal)
  "UNIVERSAL on the operator's wall clock: `11:14 PM'."
  (format-clock-minute (* 1000 (- universal nlk:+unix-epoch+))))

(defun history-changes (answer &optional (now (get-universal-time)))
  "=> (values CHANGES DAYS): the list route's ANSWER as the sheet's rows,
newest first, and the days they fall on, newest first, each change knowing
its own; NOW says which day is today."
  ;; A seal's stamp is UTC as the manifest wrote it; the sheet says the
  ;; operator's own clock, as the transcript does (FORMAT-CLOCK-MINUTE).
  (let ((today (local-date now))
        (yesterday (local-date (- now 86400)))
        (days '())
        (changes '()))
    (loop for entry across (json-array answer "changes")
          for at = (or (iso-universal (json-value entry :string "at")) 0)
          for first-at = (or (iso-universal (json-value entry :string "first_at")) at)
          do (multiple-value-bind (key words) (local-date at)
               (unless (find key days :key #'history-day-key :test #'equal)
                 (setf days (append days (list (make-history-day
                                                :key key
                                                :label (cond ((equal key today) "Today")
                                                             ((equal key yesterday) "Yesterday")
                                                             (t words))
                                                :long (cond ((equal key today) (format nil "Today, ~a" words))
                                                            ((equal key yesterday) (format nil "Yesterday, ~a" words))
                                                            (t words)))))))
               (push (make-history-change
                      :kind (json-value entry :string "kind")
                      :from (json-value entry :integer "from") :to (json-value entry :integer "to")
                      :title (or (json-value entry :string "title") "")
                      :where (or (json-value entry :string "where") "")
                      :notes (coerce (json-array entry "notes") 'list)
                      :keeps (coerce (json-array entry "keeps") 'list)
                      :files (when-let (files (json-value entry :object "files"))
                               (list (json-value files :integer "count") (json-value files :integer "added")
                                     (json-value files :integer "removed")
                                     (coerce (json-array files "paths") 'list)))
                      :version (json-value entry :string "version")
                      :refusal (json-value entry :string "refusal")
                      :nodecode (eq t (json-value entry :any "nodecode"))
                      :at at :first-at first-at :clock (wall-clock at) :first-clock (wall-clock first-at)
                      :day (position key days :key #'history-day-key :test #'equal))
                     changes)))
    (values (coerce (nreverse changes) 'vector) (coerce days 'vector))))

;;; --- the sheet ------------------------------------------------------------------

(defstruct (generations-sheet (:include sheet-dialog))
  "/generations: the rows the list route last answered (CHANGES, newest
first), the DAYS they fall on, the generation the folder stands at (NOW,
NIL while it stands at none), the binary's VERSION and whether a restore
FOLLOWS nodecode's own changes -- a release's, never a checkout's -- the
EDITS that hold every restore ((NAME . COUNT) ...), and why the last read
FAILED; where the
operator is -- FOCUS on the :PANE of changes, the :RAIL of days or the
:BANNER of edits, AT the place in the list as it shows, DAY the rail's, the
FILTER and the QUERY, the first line each list shows (TOP, RAIL-TOP) -- the
question asked in place (ASK: :TO the generation, :PLAN what the plan
route answered, :FAILED why the last step failed, :GOING once it is
answered), what was SAID above the list ((TEXT TONE)), the generation
/generations N ASKED for until the rows land, and whether the last paint was
STACKED, one view at a time."
  (changes #()) (days #()) (now nil) (version nil) (follows nil) (edits '()) (failed nil)
  (focus :pane) (at 0) (day 0) (filter :all) (query "" :type string) (top 0) (rail-top 0)
  (ask nil) (said nil) (asked nil))

(nlk:access (history generations-sheet))

(defun history-current (history)
  "The place of the change the folder stands at among HISTORY's changes, or
NIL while it stands at none."
  (let ((now history.now))
    (and now (position-if (lambda (change) (<= change.from now change.to)) history.changes))))

(defun history-wall (history)
  "The place of the newest change from which no older one can come back,
or NIL when the oldest can."
  (let ((changes history.changes))
    (loop with wall = nil
          for index from (1- (length changes)) downto 0
          while (history-change-refusal (aref changes index))
          do (setf wall index)
          finally (return wall))))

(defun history-match-p (history change query)
  "Whether QUERY finds CHANGE: a number, or #N, the generations it spans;
words anywhere in what it says, the kind and the day it falls on."
  (let ((digits (string-left-trim "#" query)))
    (if (and (plusp (length digits)) (every #'digit-char-p digits))
        (<= change.from (parse-integer digits) change.to)
        (let ((day (aref history.days change.day)))
          (some (lambda (text) (and text (search query text :test #'char-equal)))
                (list* change.title change.where (fourth (kind-entry change.kind)) day.label day.long
                       (append change.keeps (fourth change.files) change.notes)))))))

(defun history-visible (history)
  "The places of the changes the list shows, newest first: those the filter
takes, and of them the ones the query finds."
  (let ((query (string-trim " " history.query)))
    (loop for change across history.changes
          for index from 0
          when (and (change-shown-p change history.filter)
                    (or (zerop (length query)) (history-match-p history change query)))
            collect index)))

(defun history-focus (history &optional (visible (history-visible history)))
  "The place of the change the keys act on -- the one AT stands on in the
list as it shows, while the list has the focus -- else NIL."
  (and (eq history.focus :pane) visible
       (nth (min history.at (1- (length visible))) visible)))

(defun history-select (history index)
  "Stand on the change at INDEX, when the list shows it."
  (when-let (at (position index (history-visible history)))
    (setf history.at at history.focus :pane)))

(defun history-stays-p (history change)
  "Whether a restore leaves CHANGE as it is: nodecode alone moved in it, and
a restore here moves no nodecode -- the binary and its cells follow a
release, never a checkout."
  (and change.nodecode (not history.follows)))

(defun history-moved (history focus index current)
  "What going to the change at FOCUS does to the one at INDEX, the folder
standing at CURRENT (NIL: above the newest): :UNDONE, :REDONE, :AHEAD for a
change past where the folder stands that it leaves past, else NIL -- NIL
too for one a restore leaves as it is."
  (let ((cur (or current -1)))
    (cond ((history-stays-p history (aref history.changes index)) nil)
          ;; going to a change that cannot come back undoes nothing
          ((and focus (history-change-refusal (aref history.changes focus))) (and (< index cur) :ahead))
          ((and focus (> focus cur) (<= cur index) (< index focus)) :undone)
          ((and focus (< focus cur) (<= focus index) (< index cur)) :redone)
          ((< index cur) :ahead))))

(defun history-between (history focus current)
  "=> (values CHANGES STAYING): the changes going to FOCUS undoes, or does
again, from CURRENT, and how many between them a restore leaves as they are."
  (let ((cur (or current -1)) (changes '()) (staying 0))
    (loop for index from (max 0 (min focus cur)) below (max focus cur)
          for change = (aref history.changes index)
          do (if (history-stays-p history change) (incf staying) (push change changes)))
    (values (nreverse changes) staying)))

(defun cost-words (history focus current)
  "What going to FOCUS from CURRENT costs, in words: how many changes of
each kind it undoes, or does again, and what it leaves as it is. => (values
WORDS BACK), BACK when it goes back."
  (multiple-value-bind (between staying) (history-between history focus current)
    (let ((back (> focus (or current -1))))
      (values (format nil "~{~a~^; ~}"
                      (append
                       (cond (between
                              (list (format nil "~:[Redoes~;Undoes~] ~d change~:p: ~a~:[~;, and what changed ~
                                                 since the last seal~]"
                                            back (length between) (kinds-summary between)
                                            (and back (null current)))))
                             ((and back (null current)) (list "Undoes what changed since the last seal"))
                             (t (list "Nothing of yours moves")))
                       (and (plusp staying) (list "nodecode stays as it is"))))
              back))))

(defun kinds-summary (changes)
  "How many CHANGES of each kind, in words: `3 agent changes · 1 nodecode update'."
  (format nil "~{~a~^ · ~}"
          (loop for (kind nil nil one many) in +history-kinds+
                for count = (count kind changes :key #'history-change-kind :test #'equal)
                when (plusp count) collect (format nil "~d ~a" count (if (= count 1) one many)))))

(defun edits-words (edits)
  "EDITS, ((NAME . COUNT) ...), as the banner says them."
  (if (rest edits)
      (format nil "~{~a~^, ~} have uncommitted edits" (mapcar #'car edits))
      (format nil "~a has ~d uncommitted edit~:p" (car (first edits)) (cdr (first edits)))))

(defun change-span (change)
  "When CHANGE was made and the generations it spans, and how long ago."
  (format nil "~:[~a–~;~*~]~a · generation~:[s ~d–~; ~*~]~d · ~a ago"
          (eql change.at change.first-at) change.first-clock change.clock
          (eql change.from change.to) change.from change.to
          (duration-text (max 0 (- (get-universal-time) change.at)) :coarse t)))

(defun change-where-words (change)
  "Where CHANGE was made, in words: a session's title or the layer session
after `in', anything else as it is; NIL for nowhere said."
  (let ((where change.where))
    (cond ((zerop (length where)) nil)
          ((or (uiop:string-prefix-p "“" where) (uiop:string-prefix-p "the " where)) (format nil "in ~a" where))
          (t where))))

(defun no-generation-words (history number)
  "That no generation NUMBER is sealed, and which are."
  (if (plusp (length history.changes))
      (format nil "There is no generation ~d: 1 to ~d are sealed" number
              (history-change-to (aref history.changes 0)))
      (format nil "There is no generation ~d: none is sealed yet" number)))

(defun change-details (change width &key where)
  "The lines a chosen CHANGE carries under its row, WIDTH wide, each a list
of (TEXT TONE) runs: its keeps, where it was made unless the row says it
whole (WHERE NIL), the files it moved, its notes, when, and why it can
never come back."
  (let ((lines '()))
    (flet ((add (text tone &optional (most 1))
             (let ((wrapped (wrapped-lines text width)))
               (when (> (length wrapped) most)
                 (setf wrapped (subseq wrapped 0 most))
                 (setf (car (last wrapped))
                       (concatenate 'string (take-display-prefix (car (last wrapped)) (1- width)) "…")))
               (dolist (line wrapped) (push (list (list line tone)) lines)))))
      (when (rest change.keeps) (add (format nil "~{~a~^ · ~}" change.keeps) :system 2))
      (when-let (words (and where (change-where-words change)))
        (add words :muted))
      (when-let (files change.files)
        (destructuring-bind (count added removed paths) files
          (push (list (list (format nil "~d file~:p  " count) :muted)
                      (list (format nil "+~d" added) :diff-added) (list (format nil " −~d" removed) :diff-removed)
                      (list (format nil "  ~{~a~^, ~}" paths) :muted))
                lines)))
      (dolist (note change.notes) (add note :muted))
      (add (change-span change) :muted)
      (when change.refusal (add (format nil "Can't come back: ~a" change.refusal) :danger 4)))
    (nreverse lines)))

;;; --- calls, off the loop --------------------------------------------------------

(defun history-install (history answer)
  "Land the list route's ANSWER as HISTORY's rows, the focus kept on the
change it was on, else the one the folder stands at."
  (let ((was (when-let (focus (history-focus history))
               (history-change-to (aref history.changes focus)))))
    (multiple-value-bind (changes days) (history-changes answer)
      (setf history.changes changes
            history.days days
            history.now (json-value answer :integer "now")
            history.version (json-value answer :string "version")
            history.follows (eq t (json-value answer :any "nodecode_follows"))
            history.edits (loop for entry across (json-array answer "edits")
                                collect (cons (json-value entry :string "name")
                                              (or (json-value entry :integer "count") 0)))
            history.failed nil)
      (let ((index (or (and was (position was changes :key #'history-change-to))
                       (history-current history)
                       0)))
        (setf history.at (or (position index (history-visible history)) 0)
              history.day (min history.day (max 0 (1- (length days)))))
        (when (and (eq history.focus :banner) (null history.edits))
          (setf history.focus :pane))))))

(defun history-read (model history)
  "Read the list off the loop, and land it while HISTORY is still open; then
ask about the generation /generations N named."
  (sheet-call model history
              (lambda () (gateway-model-http-call model :get "/api/gateway/generations" nil :read-timeout 120))
              (lambda (model answer)
                (if-let (failure (off-loop-failure answer))
                  (setf history.failed failure)
                  (progn (history-install history answer)
                         (when-let (number history.asked)
                           (setf history.asked nil)
                           (history-ask-number model history number)))))
              "Reading the generations…"))

(defun history-holding (history number)
  "The place of the change that spans generation NUMBER, or NIL."
  (position-if (lambda (change) (<= change.from number change.to)) history.changes))

(defun history-ask (model history index &optional (to (history-change-to (aref history.changes index))))
  "Ask, in place, before going to generation TO of the change at INDEX: the
question opens now, and what the restore would move lands under it once the
plan route answers."
  (setf history.ask (list :to to) history.said nil)
  (sheet-call model history
              (lambda ()
                (gateway-model-http-call model :get (format nil "/api/gateway/generations/plan?to=~d" to) nil
                                         :read-timeout 60))
              (lambda (model answer)
                (declare (ignore model))
                (when (and history.ask (eql to (getf history.ask :to)) (not (getf history.ask :going)))
                  (if-let (failure (off-loop-failure answer))
                    (setf (getf history.ask :failed) failure)
                    (setf (getf history.ask :plan) answer))))
              "Reading what going back moves…"))

(defun history-ask-number (model history number)
  "/generations N: stand on the change that holds generation NUMBER and ask
about NUMBER itself; say so when no change holds it."
  (let ((index (history-holding history number)))
    (if index
        (progn (setf history.filter :all history.query "")
               (history-select history index)
               (history-ask model history index number))
        (setf history.said (list (no-generation-words history number) :danger)))))

(defun ask-edits (history)
  "The edits that hold the restore asked about, ((NAME . COUNT) ...): the
plan's once it answered, else the list's."
  (let ((plan (getf history.ask :plan)))
    (if plan
        (loop for entry across (json-array plan "edits")
              collect (cons (json-value entry :string "name") (or (json-value entry :integer "count") 0)))
        history.edits)))

(defun ask-refusal (history)
  "Why the restore asked about can never work: the plan's once it answered,
else the list's for the change it is the end of."
  (destructuring-bind (&key to plan &allow-other-keys) history.ask
    (if plan
        (json-value plan :string "refusal")
        (let ((change (aref history.changes (history-holding history to))))
          (and (eql to change.to) change.refusal)))))

(defun edits-calls (model names action)
  "Keep or discard, by ACTION, the uncommitted edits of each folder NAMES,
off the loop. => the organism's sentences, in order."
  (loop for name in names
        collect (answer-text (gateway-model-http-call model :post "/api/gateway/generations/edits"
                                                      (json-object "name" name "action" action)
                                                      ;; a keep proves the folder in a trial child
                                                      :read-timeout 300))))

(defun history-go (model history how)
  "Go to the generation asked about, the uncommitted edits that hold it
first kept or discarded by HOW (:KEEP, :DISCARD, or NIL when none hold it).
The organism's sentence closes the sheet into the composer status; a refusal
stays on the question, whole, after whatever went first."
  (let ((to (getf history.ask :to))
        (names (and how (mapcar #'car (ask-edits history)))))
    (setf (getf history.ask :going) t (getf history.ask :failed) nil)
    (sheet-call model history
                (lambda ()
                  (let ((said '()))
                    (handler-case
                        (progn
                          (setf said (edits-calls model names (if (eq how :keep) "keep" "discard")))
                          (list :said said
                                :text (answer-text (gateway-model-http-call
                                                    model :post "/api/gateway/generations/rollback"
                                                    (json-object "number" to) :read-timeout 300))))
                      (gateway-http-error (refusal)
                        (list :said said :failed (gateway-http-error-detail refusal))))))
                (lambda (model answer)
                  (let ((failed (or (off-loop-failure answer) (getf answer :failed)))
                        (said (format nil "~{~a. ~}" (getf answer :said))))
                    (if failed
                        (progn (setf history.ask (list* :failed (format nil "~a~a" said failed)
                                                        (alexandria:remove-from-plist history.ask :failed :going)))
                               (when (getf answer :said) (history-read model history)))
                        (progn (dismiss-dialog model nil)
                               (show-status model (format nil "~a~a" said (getf answer :text)))
                               (render-state model)))))
                (case how
                  (:keep "Keeping the edits, then going back…")
                  (:discard "Discarding the edits, then going back…")
                  (t "Going back…")))))

(defun history-edits (model history action)
  "Keep or discard, by ACTION, every uncommitted edit that holds a restore,
from the banner: the organism's sentence said over the list, read again."
  (let ((names (mapcar #'car history.edits)))
    (sheet-call model history
                (lambda ()
                  (handler-case (list :said (edits-calls model names action)
                                      :answer (gateway-model-http-call model :get "/api/gateway/generations" nil
                                                                       :read-timeout 120))
                    (gateway-http-error (refusal) (list :failed (gateway-http-error-detail refusal)))))
                (lambda (model answer)
                  (declare (ignore model))
                  (let ((failed (or (off-loop-failure answer) (getf answer :failed))))
                    (when (getf answer :answer) (history-install history (getf answer :answer)))
                    (setf history.said (if failed
                                           (list failed :danger)
                                           (list (format nil "~{~a~^. ~}" (getf answer :said)) :success)))))
                (if (equal action "keep") "Keeping the uncommitted edits…" "Discarding the uncommitted edits…"))))

;;; --- the keys -----------------------------------------------------------------

(defun history-search (history query)
  "Make QUERY what the list finds, from its first find; emptied, the list
stands on the change the last find did."
  (let ((was (history-focus history)))
    (setf history.query query history.said nil history.top 0 history.focus :pane history.at 0)
    (when (and (zerop (length query)) was)
      (history-select history was))))

(defun history-step-filter (history delta)
  "The filter DELTA along +HISTORY-FILTERS+, round, the list kept on the
change it was on when the filter shows it."
  (let ((was (history-focus history))
        (at (position history.filter +history-filters+ :key #'first)))
    (setf history.filter (first (nth (mod (+ at delta) (length +history-filters+)) +history-filters+))
          history.top 0 history.at 0)
    (when was (history-select history was))))

(defun history-move (history delta)
  "Move AT DELTA changes along the list as it shows, stopping at its ends;
up past the first, the banner of edits when one stands."
  (let ((count (length (history-visible history))))
    (setf history.said nil)
    (if (and (minusp delta) (zerop history.at) history.edits (zerop (length history.query)))
        (setf history.focus :banner)
        (setf history.at (max 0 (min (1- count) (+ history.at delta)))))))

(defun history-chosen-day (history focus)
  "The day the rail marks: its own on the rail, the chosen change's else."
  (cond ((eq history.focus :rail) history.day)
        (focus (history-change-day (aref history.changes focus)))
        (t 0)))

(defun history-open-day (history day)
  "Open DAY: stand on its newest change the list shows."
  (let ((index (find-if (lambda (index) (eql day (history-change-day (aref history.changes index))))
                        (history-visible history))))
    (setf history.top 0)
    (if index (history-select history index) (setf history.focus :pane))))

(defun history-ask-key (model history key)
  "A key while a question stands: Esc stays on the row; Enter goes, the
edits that hold it kept first; d discards them, then goes."
  (let ((edits (ask-edits history))
        (refused (ask-refusal history)))
    (cond ((getf history.ask :going))
          ((eq key :escape) (setf history.ask nil))
          (refused)
          ((eq key :enter) (history-go model history (and edits :keep)))
          ((and edits (characterp key) (char-equal key #\d)) (history-go model history :discard)))))

(defun history-pane-key (model history key)
  (let* ((visible (history-visible history))
         (focus (history-focus history visible)))
    (case key
      (:up (history-move history -1))
      (:down (history-move history 1))
      (:page-up (history-move history -10))
      (:page-down (history-move history 10))
      (:home (setf history.at 0))
      (:end (setf history.at (max 0 (1- (length visible)))))
      ((:left :escape)
       (setf history.day (history-chosen-day history focus) history.focus :rail history.said nil))
      (:enter (when (and focus (not (eql focus (history-current history))))
                (history-ask model history focus))))))

(defun history-rail-key (model history key)
  (case key
    (:up (setf history.day (max 0 (1- history.day))))
    (:down (setf history.day (min (max 0 (1- (length history.days))) (1+ history.day))))
    ((:right :enter #\Space) (history-open-day history history.day))
    (:escape (dismiss-dialog model nil))))

(defun history-banner-key (model history key)
  (case key
    (:down (setf history.focus :pane history.at 0))
    (:enter (unless history.busy (history-edits model history "keep")))
    (:delete (unless history.busy (history-edits model history "discard")))
    (:left (setf history.focus :rail history.day 0))
    (:escape (dismiss-dialog model nil))))

(defun history-query-key (model history key)
  (let ((focus (history-focus history)))
    (case key
      (:escape (history-search history ""))
      (:up (history-move history -1))
      (:down (history-move history 1))
      (:enter (when (and focus (not (eql focus (history-current history))))
                (history-ask model history focus))))))

(defun generations-key (model history key &key ctrl alt shift)
  "Every key the shell's chords leave the open sheet."
  (let ((typed (and (characterp key) (not ctrl) (not alt))))
    (cond
      (history.ask (history-ask-key model history key))
      ;; Typing searches every change; Space is a key of its own until a
      ;; query is going.
      ((or (and typed (graphic-char-p key) (char/= key #\Space))
           (and typed (eql key #\Space) (plusp (length history.query)))
           (and (plusp (length history.query))
                (or (eq key :backspace)
                    (and ctrl (characterp key) (member key '(#\u #\w) :test #'char-equal)))))
       (when-let (query (edit-text-field history.query key :ctrl ctrl :alt alt
                                                           :max *max-dialog-search-chars*))
         (history-search history query)))
      ((eq key :tab) (history-step-filter history (if shift -1 1)))
      ((eq history.focus :banner) (history-banner-key model history key))
      ((plusp (length history.query)) (history-query-key model history key))
      ((eq history.focus :rail) (history-rail-key model history key))
      (t (history-pane-key model history key)))))

(defun generations-paste (model history text)
  "A paste lands in the query."
  (declare (ignore model))
  (unless history.ask
    (history-search history (clip (concatenate 'string history.query text)
                                  *max-dialog-search-chars* :ellipsis ""))))

(defun generations-wheel (model history delta x y)
  "A notch over the sheet moves the way the arrows do."
  (declare (ignore model x y))
  (unless history.ask
    (if (eq history.focus :rail)
        (setf history.day (max 0 (min (1- (length history.days)) (+ history.day delta))))
        (progn (setf history.focus :pane) (history-move history delta)))))

;;; --- painting -----------------------------------------------------------------

(defun paint-segs (canvas row x width segs ground)
  "SEGS, (TEXT TONE [BOLD]) runs, from X on ROW, cut at WIDTH columns."
  (let ((end (+ x width)))
    (loop for (text tone bold) in segs
          while (< x end)
          do (setf x (canvas-put canvas x row (clip-text-to-width text (- end x))
                                 (sheet-ink tone :ground ground :bold bold) end)))))

(defun history-keys (history visible)
  "The keys that act where the operator is, in the order the key row names them."
  (let* ((focus (history-focus history visible))
         (current (history-current history))
         (go (and focus (not (eql focus current))
                  (list (list "enter" (cond ((history-change-refusal (aref history.changes focus)) "why not")
                                            ((and current (< focus current)) "go forward")
                                            (t "go back here")))))))
    (cond ((eq history.focus :banner)
           '(("enter" "keep them") ("del" "discard them") ("↓" "changes") ("esc" "close")))
          ((plusp (length history.query)) (append '(("↑↓" "move")) go '(("esc" "clear"))))
          ((eq history.focus :rail)
           (list '("↑↓" "day") (list (if history.stacked "enter" "→") "open") '("tab" "filter")
                 '("type" "search") '("esc" "close")))
          (t (append '(("↑↓" "change")) go '(("←" "days") ("tab" "filter") ("type" "search")))))))

(defun ask-card (history width)
  "The question asked in place, WIDTH wide. => (values LINES KEYS): LINES
of (TEXT TONE [BOLD]) runs, and the keys that answer it."
  (destructuring-bind (&key to plan failed going &allow-other-keys) history.ask
    (let* ((index (history-holding history to))
           (change (aref history.changes index))
           (current (history-current history))
           (refusal (ask-refusal history))
           (edits (ask-edits history))
           (forward (and current (< index current)))
           (stamp (format nil "~a, ~a" (history-day-long (aref history.days change.day))
                          (if (eql to change.to) change.clock (format nil "generation ~d" to))))
           (cost (cost-words history index current))
           (moves (cond (plan (loop for line across (json-array plan "lines")
                                    collect (list line :muted)))
                        ((not failed) (list (list "Reading what going back moves…" :muted)))))
           (head (list (list (format nil "~:[Go back~;Go forward~] to ~a?" forward stamp) :foreground t)
                       (list (format nil "   generation ~d" to) :muted)))
           (wrap (lambda (text tone) (mapcar (lambda (line) (list (list line tone))) (wrapped-lines text width))))
           (moved (loop for (text tone) in moves append (funcall wrap text tone)))
           (failed-lines (and failed (funcall wrap failed :danger))))
      (cond
        (refusal
         (values (append (list (list (list (format nil "Can't go back to ~a" stamp) :foreground t)))
                         (funcall wrap (format nil "~@(~a~)~a." (subseq refusal 0 1) (subseq refusal 1)) :danger)
                         (list (list (list "Pick a change above the red line." :muted))))
                 '(("esc" "back"))))
        (edits
         (values (append (list head)
                         (funcall wrap (format nil "~a, and going back would lose ~:[it~;them~]."
                                               (edits-words edits) (or (rest edits) (/= 1 (cdr (first edits)))))
                                  :sheet-restart)
                         (list (list (list "Enter keeps them first, as a change you can come back to." :muted)))
                         (funcall wrap cost :muted)
                         moved
                         failed-lines)
                 (if going '() '(("enter" "keep them, go back") ("d" "discard them, go back") ("esc" "stay")))))
        (t
         (values (append (list head)
                         (funcall wrap cost :sheet-restart)
                         moved
                         (list (list (list "Sessions, memory and settings stay; nodecode restarts on it once nothing is running."
                                           :muted)))
                         failed-lines)
                 (if going '() (list (list "enter" (if forward "go forward" "go back")) '("esc" "stay")))))))))

(defun paint-ask (canvas row px pw lines)
  "The question's LINES from ROW, on its own ground with a row of it above and below."
  (let ((ground (sheet-ground :sheet-ask)))
    (canvas-fill canvas (- px 2) (1- row) (+ pw 4) (+ (length lines) 2) (sheet-ink nil :ground ground))
    (loop for segs in lines
          for r from row
          do (paint-segs canvas r px pw segs ground))))

(defun history-bottom (history focus current)
  "The line under the list, as (TEXT TONE [BOLD]) runs: where going to
FOCUS would stand and what it costs; on the rail or the banner, what their
keys do."
  (cond
    ((eq history.focus :banner)
     '(("Keep: one commit, sealed as a generation you can come back to. Discard: into git stash." :muted)))
    ((and (eq history.focus :rail) (zerop (length history.query)))
     '(("→ opens the day. " :muted) ("↶" :sheet-restart) (" marks the days going back would undo." :muted)))
    ((null focus)
     (and (plusp (length history.changes))
          '(("Pick a change: this line says what going back to it undoes." :muted))))
    ((eql focus current)
     (list (list "● You are here" :success t)
           (list (format nil "   generation ~d · nodecode ~a" history.now
                         (nlk::short-version (history-change-version (aref history.changes focus))))
                 :muted)))
    ((history-change-refusal (aref history.changes focus))
     (list (list "× Can't go back here: " :danger)
           (list (history-change-refusal (aref history.changes focus)) :muted)))
    (t (multiple-value-bind (words back) (cost-words history focus current)
         (list (list (if back "↶ " "↷ ") :sheet-restart) (list words :muted))))))

(defun paint-history-row (history canvas r px pw index focus current wide)
  "The row of the change at INDEX on R: its time, its glyph, its title, and
where it was made when WIDE; greyed with an ↶ when going to FOCUS undoes it."
  (let* ((change (aref history.changes index))
         (on (eql index focus))
         (moved (history-moved history focus index current))
         (quiet (member moved '(:undone :ahead)))
         (lift (sheet-ground :sheet-focus))
         (ground (if on lift (sheet-ground :sheet)))
         (now (eql index current))
         (kind (kind-entry change.kind))
         (right (+ px pw)))
    (when on
      (canvas-fill canvas (- px 2) r (+ pw 4) 1 (sheet-ink nil :ground lift))
      (when (eq (effective-color-profile) :none) (canvas-put canvas (- px 2) r "›" nil)))
    (case moved
      (:undone (canvas-put canvas (- px 2) r "↶" (sheet-ink :sheet-restart :ground ground)))
      (:redone (canvas-put canvas (- px 2) r "↷" (sheet-ink :success :ground ground))))
    (canvas-put canvas px r (format nil "~8@a" change.clock)
                (sheet-ink (cond (on :foreground) (quiet :sheet-undone) (t :muted)) :ground ground))
    (canvas-put canvas (+ px 9) r (if now "●" (second kind))
                (sheet-ink (cond (now :success) (quiet :sheet-undone) (t (third kind))) :ground ground))
    (cond (now
           (canvas-put canvas (- right 3) r "now" (sheet-ink :success :ground ground :bold t))
           (decf right 5))
          ((and wide (>= pw 70) (plusp (length change.where)))
           (let ((where (clip-text-to-width change.where (min (text-display-width change.where)
                                                              (floor (* pw 32) 100)))))
             (canvas-put canvas (- right (text-display-width where)) r where
                         (sheet-ink (if quiet :sheet-undone :muted) :ground ground))
             (decf right (+ 2 (text-display-width where))))))
    (canvas-put canvas (+ px 11) r (clip-text-to-width change.title (max 1 (- right px 11)))
                (sheet-ink (cond (on :foreground) (quiet :sheet-undone) (t :system)) :ground ground :bold on))
    (sheet-hit history r (- px 2) (+ px pw 2)
               (lambda (model history)
                 (if (and (eql index (history-focus history)) (not (eql index (history-current history))))
                     (history-ask model history index)
                     (progn (history-select history index) (setf history.said nil)))))))

(defun where-whole-p (change pw wide)
  "Whether CHANGE's row in a pane PW wide says where it was made whole."
  (and wide (>= pw 70) (<= (text-display-width change.where) (floor (* pw 32) 100))))

(defun history-lines (history visible focus pw wide)
  "The list's lines for VISIBLE, newest first, in a pane PW wide, WIDE when
rows say where they were made: a day's head where a day begins, the red line
above the newest change nothing older can come back past, a row a change,
and under FOCUS's row what it carries. => (values LINES FOCUS-LINE)."
  (let* ((lines '()) (focus-line 0) (day nil) (walled nil)
         (whole (zerop (length (string-trim " " history.query))))
         (wall (and whole (history-wall history))))
    ;; the folder as it stands, when no generation is it yet
    (when (and whole visible (null history.now))
      (push (list :unsealed) lines))
    (dolist (index visible)
      (let ((change (aref history.changes index)))
        (unless (eql change.day day)
          (setf day change.day)
          (push (list :head day) lines))
        (when (and wall (not walled) (>= index wall))
          (setf walled t)
          (push (list :wall) lines))
        (when (eql index focus) (setf focus-line (length lines)))
        (push (list :row index) lines)
        (when (eql index focus)
          (dolist (segs (change-details change (- pw 11) :where (not (where-whole-p change pw wide))))
            (push (list :detail segs) lines)))))
    (values (nreverse lines) focus-line)))

(defun day-count (history day)
  "How many changes of DAY the filter shows."
  (count-if (lambda (change) (and (eql day change.day) (change-shown-p change history.filter)))
            history.changes))

(defun day-refused-p (history day)
  "Whether no change of DAY can come back."
  (every (lambda (change) (or (not (eql day change.day)) change.refusal)) history.changes))

(defun paint-history-list (history canvas lines focus-line top rows px pw focus current wide)
  "LINES down ROWS rows from TOP in the pane at PX, PW wide, scrolled to keep
the chosen change and what it carries in view -- on the rail, the chosen
day's head at the top."
  (when (plusp rows)
    (let* ((pane (sheet-ground :sheet))
           (lift (sheet-ground :sheet-focus))
           (offset history.top))
      (if (null focus)
          (let ((head (position-if (lambda (line) (and (eq :head (first line)) (eql history.day (second line))))
                                   lines)))
            (setf offset (max 0 (min (or head 0) (- (length lines) rows)))))
          (let ((last focus-line)
                (head (if (and (plusp focus-line) (not (eq :row (first (nth (1- focus-line) lines)))))
                          (1- focus-line)
                          focus-line)))
            (loop while (eq :detail (first (nth (1+ last) lines))) do (incf last))
            ;; what the change carries is kept in view while its row is
            (setf offset (max 0 (min head (max (min offset head) (- last rows -1)))))))
      (setf history.top offset)
      (loop for i from 0 below rows
            for line = (nth (+ offset i) lines)
            for r = (+ top i)
            while line
            do (ecase (first line)
                 (:row (paint-history-row history canvas r px pw (second line) focus current wide))
                 (:detail (canvas-fill canvas (- px 2) r (+ pw 4) 1 (sheet-ink nil :ground lift))
                          (paint-segs canvas r (+ px 11) (- pw 11) (second line) lift))
                 (:head
                  (let* ((day (aref history.days (second line)))
                         (count (format nil "~d change~:p" (day-count history (second line))))
                         (at (canvas-put canvas px r (clip-text-to-width day.long pw)
                                         (sheet-ink (if (day-refused-p history (second line)) :muted :foreground)
                                                    :ground pane :bold t))))
                    (when (and (zerop (length history.query))
                               (<= (+ at 4 (length count)) (+ px pw)))
                      (canvas-put canvas (- (+ px pw) (length count)) r count (sheet-ink :muted :ground pane)))))
                 (:unsealed
                  (let ((at (canvas-put canvas px r "● Now" (sheet-ink :success :ground pane :bold t))))
                    (canvas-put canvas (+ at 3) r
                                (clip-text-to-width "the folder matches no generation yet; it seals once it proves"
                                                    (- (+ px pw) at 3))
                                (sheet-ink :muted :ground pane))))
                 (:wall
                  (let* ((words " nothing below this line can come back ")
                         (ink (sheet-ink :danger :ground pane))
                         (at (canvas-put canvas px r "───" ink)))
                    (setf at (canvas-put canvas at r (clip-text-to-width words (- (+ px pw) at)) ink))
                    (when (< at (+ px pw))
                      (canvas-put canvas at r (make-string (- (+ px pw) at) :initial-element #\─) ink))))))
      (when (plusp offset)
        (canvas-put canvas (+ px pw 1) top "▴" (sheet-ink :muted :ground pane)))
      (when (< (+ offset rows) (length lines))
        (canvas-put canvas (+ px pw 1) (+ top rows -1) "▾" (sheet-ink :muted :ground pane))))))

(defun paint-history-chips (history canvas r px pw ground)
  "The filters side by side on R, the one shown filled, each with its count."
  (let ((at px))
    (loop for (key name) in +history-filters+
          for count = (count-if (lambda (change) (change-shown-p change key)) history.changes)
          for text = (format nil " ~a ~d " name count)
          for width = (text-display-width text)
          while (<= (+ at width) (+ px pw))
          do (canvas-put canvas at r text (if (eq key history.filter)
                                              (sheet-ink :sheet-chip :bold t)
                                              (sheet-ink :muted :ground ground)))
             (sheet-hit history r at (+ at width)
                        (let ((key key))
                          (lambda (model history)
                            (declare (ignore model))
                            (let ((was (history-focus history)))
                              (setf history.filter key history.top 0 history.at 0)
                              (when was (history-select history was))))))
             (incf at (1+ width)))))

(defun paint-history-notes (history canvas r px pw ground tight)
  "What stands over the list from R: why the last read failed, what was
said, the banner of edits. => the row the list starts on."
  (when history.failed
    (dolist (line (wrapped-lines history.failed pw))
      (canvas-put canvas px r line (sheet-ink :danger :ground ground))
      (incf r))
    (incf r))
  (when-let (said history.said)
    (let ((lines (wrapped-lines (first said) pw)))
      (dolist (line (subseq lines 0 (min 3 (length lines))))
        (canvas-put canvas px r line (sheet-ink (second said) :ground ground))
        (incf r))
      (incf r)))
  (when history.edits
    (let* ((on (eq history.focus :banner))
           (lift (sheet-ground :sheet-focus))
           (here (if on lift ground)))
      (when on (canvas-fill canvas (- px 2) r (+ pw 4) 1 (sheet-ink nil :ground lift)))
      (let ((at (canvas-put canvas px r "! " (sheet-ink :sheet-restart :ground here :bold t))))
        (canvas-put canvas at r (clip-text-to-width (format nil "~a, so going back is paused"
                                                            (edits-words history.edits))
                                                    (- (+ px pw) at))
                    (sheet-ink (if on :foreground :system) :ground here)))
      (sheet-hit history r (- px 2) (+ px pw 2)
                 (lambda (model history)
                   (declare (ignore model))
                   (setf history.focus :banner history.said nil)))
      (incf r (if tight 1 2))))
  r)

(defun history-empty-text (history)
  "What the list says while it shows nothing."
  (cond ((plusp (length (string-trim " " history.query)))
         (format nil "Nothing matches “~a”" (string-trim " " history.query)))
        (history.failed nil)
        (history.busy "Reading the generations…")
        ((zerop (length history.changes)) "No generation sealed yet")
        (t "No change of this kind")))

(defun paint-history-days (history canvas top rows width rail pane focus current)
  "The days down ROWS rows from TOP in the rail, WIDTH wide, scrolled to keep
the chosen one in view: the chosen one on the pane's ground, so it reads as
the tab the list hangs from."
  (let* ((days history.days)
         (chosen (history-chosen-day history focus))
         (offset (max 0 (min history.rail-top chosen) (- chosen rows -1))))
    (setf offset (min offset (max 0 (- (length days) rows)))
          history.rail-top offset)
    (loop for d from offset below (min (length days) (+ offset rows))
          for r from top
          for day = (aref days d)
          do (let* ((on (and (= d chosen) (zerop (length history.query)) (not (eq history.focus :banner))))
                    (ground (if on pane rail))
                    (count (princ-to-string (day-count history d))))
               (when on (canvas-fill canvas 0 r width 1 (sheet-ink nil :ground pane)))
               (when (and focus
                          (find d history.changes :key #'history-change-day)
                          (loop for change across history.changes
                                for index from 0
                                always (or (not (eql d change.day))
                                           (eq :undone (history-moved history focus index current)))))
                 (canvas-put canvas 1 r "↶" (sheet-ink :sheet-restart :ground ground)))
               (canvas-put canvas 3 r (clip-text-to-width day.label (- width 9))
                           (sheet-ink (cond ((not on) (if (day-refused-p history d) :muted :system))
                                            ((eq history.focus :rail) :dialog-accent)
                                            (t :foreground))
                                      :ground ground :bold on))
               (canvas-put canvas (- width 2 (length count)) r count (sheet-ink :muted :ground ground))
               (sheet-hit history r 0 width
                          (lambda (model history)
                            (declare (ignore model))
                            (setf history.query "" history.ask nil history.said nil history.focus :rail
                                  history.day d)))))
    (when (plusp offset)
      (canvas-put canvas (- width 2) top "▴" (sheet-ink :muted :ground rail)))
    (when (< (+ offset rows) (length days))
      (canvas-put canvas (- width 2) (+ top rows -1) "▾" (sheet-ink :muted :ground rail)))))

(defun paint-history-wide (history canvas edges)
  "The wide sheet: the days in a rail beside the changes. => the caret's
cell, or NIL."
  (with-wide-sheet (canvas edges (if (>= width 104) 24 21))
    (let* ((tight (< inner 22))
           (title-row (+ top (if tight 0 1)))
           (hint-row (+ top inner (if tight -1 -2)))
           (visible (history-visible history))
           (focus (history-focus history visible))
           (current (history-current history))
           (changes history.changes)
           (cursor nil)
           (r (+ title-row 2)))
      (canvas-put canvas 3 title-row "Generations" (sheet-ink :title :ground rail :bold t))
      (paint-history-days history canvas (+ title-row 2) (- hint-row 2 title-row 2) rail-width rail pane
                          focus current)
      (when (plusp (length changes))
        (canvas-put canvas 3 (1- hint-row)
                    (clip-text-to-width (format nil "~d sealed" (history-change-to (aref changes 0)))
                                        (- rail-width 4))
                    (sheet-ink :muted :ground rail))
        (canvas-put canvas 3 hint-row
                    (clip-text-to-width (format nil "since ~a" (nth-value 1 (local-date (history-change-first-at
                                                                                          (aref changes (1- (length changes)))))))
                                        (- rail-width 4))
                    (sheet-ink :muted :ground rail)))
      ;; the search field
      (canvas-fill canvas (1- px) title-row (+ pw 2) 1 (sheet-ink nil :ground field))
      (if (plusp (length history.query))
          (setf cursor (cons (canvas-put canvas px title-row (clip-tail history.query (1- pw))
                                         (sheet-ink :composer :ground field))
                             title-row))
          (canvas-put canvas px title-row (clip-text-to-width "Search changes, cells, sessions, #numbers" (1- pw))
                      (sheet-ink :muted :ground field)))
      (unless tight
        (paint-history-chips history canvas r px pw pane)
        (incf r 2))
      (setf r (paint-history-notes history canvas r px pw pane tight))
      (multiple-value-bind (card keys) (and history.ask (ask-card history pw))
        (let* ((room (max 0 (- hint-row r 4)))
               (card (and card (subseq card 0 (min (length card) room))))
               (list-end (if card (- hint-row (length card) 3) (- hint-row 4))))
          (multiple-value-bind (lines focus-line) (history-lines history visible focus pw t)
            (if lines
                (paint-history-list history canvas lines focus-line r (1+ (- list-end r)) px pw focus current t)
                (when-let (text (history-empty-text history))
                  (canvas-put canvas px r (clip-text-to-width text pw) (sheet-ink :muted :ground pane)))))
          (if card
              (paint-ask canvas (- hint-row 1 (length card)) px pw card)
              (paint-segs canvas (- hint-row 2) px pw (history-bottom history focus current) pane))
          (paint-keys history canvas hint-row px (+ px pw) (if history.ask keys (history-keys history visible))
                      history.flash pane)))
      cursor)))

(defun paint-history-stacked (history canvas edges)
  "The narrow sheet: the days, or the changes, or a query's finds, one view
at a time. => the caret's cell, or NIL."
  (with-stacked-sheet (canvas edges)
    (let* ((visible (history-visible history))
           (focus (history-focus history visible))
           (current (history-current history))
           (days-view (and (member history.focus '(:rail :banner)) (zerop (length history.query))))
           (cursor nil)
           (r (+ header-row 2)))
      (flet ((right (text)
               (when (< (+ (text-display-width text) 16) pw)
                 (canvas-put canvas (- (+ px pw) (text-display-width text)) header-row text
                             (sheet-ink :muted :ground pane)))))
        (cond
          ((plusp (length history.query))
           (let* ((count (length visible))
                  (at (canvas-put canvas px header-row "Search " (sheet-ink :muted :ground pane))))
             (setf cursor (cons (canvas-put canvas at header-row (clip-tail history.query (max 1 (- pw 22)))
                                            (sheet-ink :foreground :ground pane))
                                header-row))
             (right (if (plusp count) (format nil "~d match~:[es~;~]" count (= count 1)) "no match"))))
          (days-view
           (canvas-put canvas px header-row "Generations" (sheet-ink :title :ground pane :bold t))
           (right "type to search"))
          (t
           (let* ((chosen (history-chosen-day history focus))
                  (at (canvas-put canvas px header-row "Generations › " (sheet-ink :muted :ground pane))))
             (sheet-hit history header-row px at
                        (lambda (model history)
                          (declare (ignore model))
                          (setf history.day (history-chosen-day history (history-focus history))
                                history.focus :rail)))
             (when (plusp (length history.days))
               (canvas-put canvas at header-row
                           (clip-text-to-width (history-day-label (aref history.days chosen)) (max 1 (- pw 30)))
                           (sheet-ink :title :ground pane :bold t))
               (right (format nil "‹ ~d/~d ›" (1+ chosen) (length history.days))))))))
      (when (>= inner 18)
        (paint-history-chips history canvas r px pw pane)
        (incf r 2))
      (when (>= inner 14)
        (setf r (paint-history-notes history canvas r px pw pane nil)))
      (multiple-value-bind (card keys) (and history.ask (ask-card history pw))
        (let* ((gap (if (>= inner 16) 1 0))
               (bottom-row (- hint-row 1 gap))
               (room (max 0 (- hint-row r 4)))
               (card (and card (subseq card 0 (min (length card) room))))
               (list-end (if card (- hint-row (length card) 3) (- bottom-row 1 gap)))
               (rows (1+ (- list-end r))))
          (if days-view
              (let ((offset (max 0 (min history.rail-top history.day) (- history.day rows -1))))
                (setf history.rail-top offset)
                (loop for d from offset below (min (length history.days) (+ offset rows))
                      for y from r
                      for day = (aref history.days d)
                      do (let* ((on (and (= d history.day) (eq history.focus :rail)))
                                (ground (if on (sheet-ground :sheet-focus) pane))
                                (count (princ-to-string (day-count history d))))
                           (when on (canvas-fill canvas (- px 2) y (+ pw 4) 1 (sheet-ink nil :ground ground)))
                           (canvas-put canvas px y (clip-text-to-width day.long (- pw 8))
                                       (sheet-ink (cond (on :dialog-accent)
                                                        ((day-refused-p history d) :muted)
                                                        (t :system))
                                                  :ground ground :bold on))
                           (canvas-put canvas (- (+ px pw) (length count)) y count (sheet-ink :muted :ground ground))
                           (sheet-hit history y (- px 2) (+ px pw 2)
                                      (lambda (model history)
                                        (declare (ignore model))
                                        (if (and (= d history.day) (eq history.focus :rail))
                                            (history-open-day history d)
                                            (setf history.day d history.focus :rail history.said nil)))))))
              (multiple-value-bind (lines focus-line) (history-lines history visible focus pw nil)
                (if lines
                    (paint-history-list history canvas lines focus-line r rows px pw focus current nil)
                    (when-let (text (history-empty-text history))
                      (canvas-put canvas px r (clip-text-to-width text pw) (sheet-ink :muted :ground pane))))))
          (cond (card (paint-ask canvas (- hint-row 1 (length card)) px pw card))
                ((>= inner 12) (paint-segs canvas bottom-row px pw (history-bottom history focus current) pane)))
          (paint-keys history canvas hint-row px (+ px pw) (if history.ask keys (history-keys history visible))
                      history.flash pane)))
      cursor)))

(defparameter +generations-sheet-size+ '(112 32)
  "The columns and rows the sheet asks for: what a wide window gives it.")

(defun generations-paint (model history width height)
  "The sheet for a WIDTH x HEIGHT frame: wide from 84 columns, the days
beside the changes; narrower, one view at a time."
  (paint-sheet model history width height +generations-sheet-size+ #'paint-history-wide #'paint-history-stacked))

(defun slash-generations-op (model args &aux (tokens (split-words (or args ""))))
  "/generations on a shell: the sheet at once over the shell, the history
read off the loop; /generations N asks, in place, before going to N."
  (let ((number (and tokens (null (rest tokens)) (ignore-errors (parse-integer (first tokens))))))
    (if (and tokens (null number))
        "generations takes a generation's number, or nothing: /generations [N]"
        (let ((history (make-generations-sheet :kind :generations :title "Generations"
                                               :paint #'generations-paint :key #'generations-key
                                               :paste #'generations-paste :click #'sheet-click
                                               :wheel #'generations-wheel :asked number)))
          (setf (chat-model-dialog model) history)
          (history-read model history)
          nil))))
;;; --- /generations without a shell ---------------------------------------------
;;; A room, the model's eval and the headless surface read the history the
;;; sheet does -- the list route's answer (GENERATIONS-ANSWER), installed as
;;; the sheet installs it -- as words, and as the dialog a room draws as one
;;; card (LIST-DIALOG): every change to page through and pick, newest first.
;;; A pick says /generations N, which asks
;;; before anything moves, as the sheet's Enter does (ASK-CARD); the lines
;;; its question names -- /generations N go, or keep or discard when
;;; uncommitted edits hold it -- go.

(defparameter +history-said+ 20
  "How many of the newest changes /generations says in words without a shell.")

(defun history-of (answer)
  "The list route's ANSWER on a sheet no shell paints: its changes and days,
where the folder stands, the edits that hold a restore (HISTORY-INSTALL)."
  (let ((history (make-generations-sheet :kind :generations :title "Generations")))
    (history-install history answer)
    history))

(defun history-context (history)
  "Where HISTORY's folder stands and how much there is: `on #9 · 6 changes
· 9 generations'."
  (let* ((changes history.changes)
         (newest (if (plusp (length changes)) (history-change-to (aref changes 0)) 0)))
    (format nil "~:[past #~d, unsealed~*~;~*on #~d~] · ~d change~:p · ~d generation~:p"
            history.now newest history.now (length changes) newest)))

(defun history-edits-words (edits)
  "The uncommitted EDITS that hold every restore, ((NAME . COUNT) ...), as the
list says them once over its rows; NIL for none."
  (and edits (format nil "~a: going back keeps or discards ~:[it~;them~] first"
                     (edits-words edits) (or (rest edits) (/= 1 (cdr (first edits)))))))

(defun history-row (history index current)
  "The change at INDEX as a dialog's row, CURRENT the place of the one the
folder stands at: the generation it ends at and what it was, when and where,
its kind's glyph, and the line that asks about going back to it."
  (let* ((change (aref history.changes index))
         (kind (kind-entry change.kind)))
    (list :label (format nil "#~d ~a" change.to change.title)
          :detail (format nil "~@[~a · ~]~a ~a~@[ · ~a~]"
                          (cond ((eql index current) "you are here")
                                (change.refusal "can't come back"))
                          (history-day-label (aref history.days change.day)) change.clock
                          (change-where-words change))
          :mark (second kind) :mark-tone (third kind)
          :run (list :enter (format nil "/generations ~d" change.to)))))

(defun history-dialog (history)
  "HISTORY as the dialog a room draws as one card: every change, newest
first, a pick asking before going back to it."
  (let ((current (history-current history)))
    (list-dialog :title "Generations" :context (history-context history)
                 :summary (history-edits-words history.edits)
                 :hint "enter go back to  esc close"
                 :empty-label "No generation sealed yet"
                 :rows (loop for index below (length history.changes)
                             collect (history-row history index current)))))

(defun history-words (history)
  "HISTORY in words: where the folder stands, the edits that hold a restore,
the newest +HISTORY-SAID+ changes a line each, and how to pick one."
  (let* ((changes history.changes)
         (current (history-current history))
         (said (min +history-said+ (length changes))))
    (if (zerop (length changes))
        "No generation sealed yet: the first is sealed once a launch's folder proves."
        (format nil "Generations · ~a~@[~%~a~]~{~%~a~}~%~@[… ~d older; ~]/generations N asks before going back to N."
                (history-context history) (history-edits-words history.edits)
                (loop for index below said
                      collect (destructuring-bind (&key label detail mark &allow-other-keys)
                                  (history-row history index current)
                                (format nil "~a ~a · ~a" mark label detail)))
                (and (> (length changes) said) (- (length changes) said))))))

(defun history-ask-answer (history index number plan)
  "(values TEXT DIALOG): /generations NUMBER without a shell, the change at
INDEX holding it -- the question the sheet's Enter asks (ASK-CARD), PLAN the
plan route's answer: what going there undoes and moves, the edits that hold
it, and the lines that go; or why it never can, or that the folder stands
there already."
  (let* ((change (aref history.changes index))
         (current (history-current history))
         (forward (and current (< index current)))
         (go (if forward "go forward" "go back"))
         (stamp (format nil "~a, ~a" (history-day-long (aref history.days change.day))
                        (if (eql number change.to) change.clock (format nil "generation ~d" number))))
         (context (format nil "generation ~d · ~a" number change.title))
         (refusal (json-value plan :string "refusal"))
         (edits (loop for entry across (json-array plan "edits")
                      collect (cons (json-value entry :string "name") (or (json-value entry :integer "count") 0))))
         ;; a room is public: the checkout a restore leaves is named, never where it sits
         (moves (loop for line across (json-array plan "lines")
                      collect (list :label (ppcre:regex-replace "the checkout at \\S+ has" line "the checkout has")))))
    (flet ((answer (dialog how)
             ;; the card's words, the rows that run nothing, then the line that goes
             (values (format nil "~a~@[ · ~a~]~@[~%~a~]~{~%~a~}~@[~%~a~]"
                             (getf dialog :title) (getf dialog :context) (getf dialog :summary)
                             (loop for row in (getf dialog :rows) unless (getf row :run) collect (getf row :label))
                             how)
                     (dialog-json dialog)))
           (runs (label how)
             (list :label label :run (list :enter (format nil "/generations ~d ~a" number how)))))
      (cond
        (refusal
         (answer (list-dialog :title (format nil "Can't go back to ~a" stamp) :context context
                              :summary (format nil "~@(~a~)~a." (subseq refusal 0 1) (subseq refusal 1))
                              :tone :danger :hint "esc back"
                              :empty-label "Pick a change above the red line.")
                 "Pick a change above the red line."))
        ((and (eql number history.now) (null edits))
         (answer (list-dialog :title (format nil "You are on ~a" stamp) :context context
                              :summary "Nothing moves: the folder stands here." :hint "esc back"
                              :empty-label "Pick another change to go to.")
                 nil))
        (edits
         (answer (list-dialog :title (format nil "~@(~a~) to ~a?" go stamp) :context context
                              :summary (cost-words history index current) :tone :warn
                              :hint (format nil "enter ~a  esc stay" go)
                              :rows (append
                                     (list (list :label (format nil "~a, and going back would lose ~:[it~;them~]."
                                                                (edits-words edits)
                                                                (or (rest edits) (/= 1 (cdr (first edits)))))))
                                     moves
                                     (list (runs "Keep the edits first, as a change you can come back to" "keep")
                                           (runs "Discard the edits into git stash first" "discard"))))
                 (format nil "/generations ~d keep keeps the edits first, as a change you can come back to; ~
                              /generations ~d discard sets them aside in git stash; then it goes."
                         number number)))
        (t
         (answer (list-dialog :title (format nil "~@(~a~) to ~a?" go stamp) :context context
                              :summary (cost-words history index current) :tone :warn
                              :hint (format nil "enter ~a  esc stay" go)
                              :rows (append
                                     moves
                                     (list (list :label "Sessions, memory and settings stay; nodecode restarts on it once nothing is running.")
                                           (runs (format nil "~@(~a~) to #~d" go number) "go"))))
                 (format nil "/generations ~d go does it." number)))))))

(defun history-go-words (number how)
  "/generations NUMBER HOW without a shell: the uncommitted edits that hold
the restore kept or discarded first, by HOW (`keep', `discard'; `go' touches
none), then the restore -- the organism's sentences, a refusal the last of
them, never a signal."
  (let ((said '()))
    (handler-case
        (progn
          (unless (string-equal how "go")
            (loop for entry across (json-array (generation-plan-answer number) "edits")
                  do (push (generation-edits-text (json-value entry :string "name") (string-downcase how)) said)))
          (format nil "~{~a. ~}~a" (reverse said) (rollback number)))
      (error (condition) (format nil "~{~a. ~}~a" (reverse said) condition)))))

(defun headless-generations (args session-id &aux (tokens (split-words (or args ""))))
  "/generations without a shell -- a room, the model's eval, the headless
surface: the history in words, and as the card a room draws (HISTORY-DIALOG);
/generations N the question before going to N (HISTORY-ASK-ANSWER); and
/generations N go -- keep or discard first when uncommitted edits hold it --
the restore, this organism exiting once no turn is in flight."
  (declare (ignore session-id))
  (let ((number (and tokens (ignore-errors (parse-integer (first tokens)))))
        (how (second tokens)))
    (handler-case
        (cond ((null tokens)
               (let ((history (history-of (generations-answer))))
                 (values (history-words history) (dialog-json (history-dialog history)))))
              ((and number (null (rest tokens)))
               (let* ((history (history-of (generations-answer)))
                      (index (history-holding history number)))
                 (if index
                     (history-ask-answer history index number (generation-plan-answer number))
                     (no-generation-words history number))))
              ((and number (null (cddr tokens)) (member how '("go" "keep" "discard") :test #'string-equal))
               (history-go-words number how))
              (t "generations takes nothing, a generation's number, or the number and go, keep or discard: /generations [N [go|keep|discard]]"))
      (error (condition) (princ-to-string condition)))))
