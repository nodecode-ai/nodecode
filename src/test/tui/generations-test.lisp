;;;; generations-test.lisp --- /generations: the history of the self on a
;;;; sheet, and the way back to a change in it (tui/generations.lisp).
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The gateway's routes are answered from fixtures built here: six changes
;;;; over four days, the newest an hour ago -- nodecode moving alone, an
;;;; agent's keeps, uncommitted edits, a cell installed, then two the past
;;;; can never give back -- with the layer's uncommitted edits holding every
;;;; restore. The stamps are written from the operator's own clock, so the
;;;; sheet says the times they were made at whatever the zone.

(in-package #:nodecode.test)

(defun local-iso (year month date hour minute)
  "The UTC stamp a seal writes for HOUR:MINUTE on the operator's clock."
  (nlk::iso-time (- (encode-universal-time 0 minute hour date month year) nlk:+unix-epoch+)))

(defun history-entry (&rest fields &key kind from to at first-at title where keeps files refusal nodecode)
  (declare (ignore fields))
  (nlk:json-object "kind" kind "from" from "to" to "at" at "first_at" (or first-at at)
                   "title" title "where" (or where "") "version" "0.0.1+bbbbbbb"
                   :when nodecode "nodecode" t
                   :opt "keeps" keeps
                   :opt "files" files
                   "refusal" (or refusal :null)))

(defun earlier-today (seconds)
  "The universal time SECONDS ago, or local midnight when that was yesterday:
a change made a minute ago reads as today's even in the first minute of a day."
  (let ((now (get-universal-time)))
    (multiple-value-bind (second minute hour) (decode-universal-time now)
      (- now (min seconds (+ second (* 60 minute) (* 3600 hour)))))))

(defun history-answer (&key follows (edits '(("mylayer" . 13))) (now 9))
  "The list route's answer the sheet is read from."
  (nlk:json-object
   "now" (or now :null) "version" "0.0.1+bbbbbbb" "nodecode_follows" follows
   "edits" (map 'vector (lambda (entry) (nlk:json-object "name" (car entry) "count" (cdr entry))) edits)
   "changes" (vector
              (history-entry :kind "nodecode" :from 9 :to 9 :nodecode t
                             :at (nlk::iso-time (- (earlier-today 60) nlk:+unix-epoch+))
                             :title "nodecode aaaaaaa → bbbbbbb" :where "14 built-in cells")
              (history-entry :kind "agent" :from 6 :to 8
                             :at (local-iso 2026 10 14 23 14) :first-at (local-iso 2026 10 14 23 7)
                             :title "Kept rt-time, sf-time +1" :where "“Analyze the run”"
                             :keeps #("rt-time" "sf-time" "json-objs")
                             :files (nlk:json-object "count" 3 "added" 40 "removed" 2
                                                     "paths" #("rt-time.lisp")))
              (history-entry :kind "edits" :from 5 :to 5 :at (local-iso 2026 10 14 22 45)
                             :title "Uncommitted edits in mylayer" :where "one snapshot")
              (history-entry :kind "install" :from 4 :to 4 :at (local-iso 2026 10 13 9 0)
                             :title "Installed nodecode-lsp" :where "a hub install")
              (history-entry :kind "agent" :from 2 :to 3 :at (local-iso 2026 10 12 18 0)
                             :title "Kept old-thing" :where "the layer session"
                             :refusal "nl-gone is gone, and generation 3 had it as an own folder at abc1234: nothing can put it back")
              (history-entry :kind "first" :from 1 :to 1 :at (local-iso 2026 10 12 9 0)
                             :title "First generation" :where "9 folders · nodecode aaaaaaa"
                             :refusal "nl-gone is gone, and generation 1 had it as an own folder at abc1234: nothing can put it back"))))

(defun plan-answer (number)
  "The plan route's answer for generation NUMBER."
  (nlk:json-object "to" number
                   "lines" (vector (format nil "mylayer to 1234567, once its uncommitted edits are kept or discarded")
                                   (format nil "nodecode stays at bbbbbbb; generation ~d ran aaaaaaa" number))
                   "refusal" (if (<= number 3)
                                 "nl-gone is gone, and generation 3 had it as an own folder at abc1234: nothing can put it back"
                                 :null)
                   "edits" (vector (nlk:json-object "name" "mylayer" "count" 13))))

(defmacro with-history-gateway ((calls &key (answer '(history-answer))) &body body)
  "Run BODY with the shell's gateway answering the generations routes, every
call it was asked pushed onto CALLS as (METHOD PATH BODY)."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &key raw stream read-timeout)
                                 (push (list method path body) ,calls)
                                 (cond ((search "/plan?to=" path)
                                        (plan-answer (parse-integer path :start (+ 9 (search "/plan?to=" path)))))
                                       ((search "/edits" path)
                                        (nlk:json-object "text" (format nil "Kept ~a's uncommitted edits as abc1234; sealed as generation 10"
                                                                        (nlk:json-value body :string "name"))))
                                       ((search "/rollback" path)
                                        (nlk:json-object "text" (format nil "generation ~d restored: mylayer at 1234567; this organism exits once no turn is in flight, and the next launch runs it"
                                                                        (nlk:json-value body :integer "number"))))
                                       (t ,answer)))
       ,@body)))

(defun history-sheet (model &optional (args ""))
  "Open /generations on MODEL, the routes answering at once, => the sheet."
  (nle::slash-generations-op model args)
  model.dialog)

(defun history-on (model)
  "The change the sheet's keys act on."
  (let* ((history model.dialog)
         (focus (nle::history-focus history)))
    (and focus (aref (nle::generations-sheet-changes history) focus))))

(defun posted (calls)
  "The posts among CALLS, oldest first, as (PATH WHAT): the folder and action
an edits call names, the number a rollback does."
  (loop for (method path body) in (reverse calls)
        when (eq method :post)
          collect (list path (or (nlk:json-value body :string "action")
                                 (nlk:json-value body :integer "number")))))

(deftest generations-opens-a-sheet-at-once-and-lands-the-history-off-the-loop ()
  ;; The list is git reads of a second or two on a live home: the sheet
  ;; stands on the keystroke under its reading line, the rows land when the
  ;; read does, and a read that lands after Esc moves nothing.
  (with-frame-model (model 120 40 :session-id "history-open")
    (let ((pending '()))
      (with-stubbed-fdefinition (nle::off-loop (model thunk continuation)
                                  (setf pending (append pending (list (cons thunk continuation)))))
        (with-history-gateway (calls)
          (nle::editor-insert model.editor "/generations")
          (press model :enter)
          (is (nle::generations-sheet-p model.dialog) "the sheet, not a docked picker")
          (is (null calls) "nothing read on the loop")
          (is (sheet-row model "Reading the generations…"))
          (destructuring-bind (thunk . continuation) (pop pending)
            (funcall continuation model (funcall thunk)))
          (is (equal '((:get "/api/gateway/generations" nil)) calls) "one read, the list")
          (is (= 6 (length (nle::generations-sheet-changes model.dialog))))
          (is (equal "nodecode aaaaaaa → bbbbbbb" (nle::history-change-title (history-on model))) "it stands on the change the folder stands at")
          (is (sheet-row model "● You are here" "generation 9"))
          (press model :escape) (press model :escape)
          (is (null model.dialog) "Esc to the days, Esc again closes")
          (nle::slash-generations-op model "")
          (press model :escape) (press model :escape)
          (destructuring-bind (thunk . continuation) (pop pending)
            (is (eq :quiet (funcall continuation model (funcall thunk))) "a closed sheet takes nothing")))))))

(deftest the-history-reads-by-day-in-the-operators-clock-with-a-red-line ()
  ;; Days in the rail and as heads over their changes; a change's time on
  ;; the operator's clock; the red line over the newest change nothing older
  ;; can come back past; the edits that hold every restore said over it.
  (with-frame-model (model 120 40 :session-id "history-days")
    (with-history-gateway (calls)
      (let ((history (history-sheet model)))
        (is (equal '("Today") (list (nle::history-day-label (aref (nle::generations-sheet-days history) 0)))))
        (is (= 4 (length (nle::generations-sheet-days history))) "four days")
        (let ((rows (sheet-rows model)))
          (is (row-carrying "! mylayer has 13 uncommitted edits, so going back is paused" rows))
          (is (row-carrying "Today, " rows) "today's head says its date too")
          (is (row-carrying (nth-value 1 (nle::local-date (encode-universal-time 0 0 12 14 10 2026))) rows))
          (is (row-carrying "11:14 PM ✦ Kept rt-time, sf-time +1" rows) "the wall clock it was made at")
          (is (row-carrying "nothing below this line can come back" rows))
          (is (< (row-index-of "Installed nodecode-lsp" rows :required t)
                 (row-index-of "nothing below this line can come back" rows :required t)
                 (row-index-of "Kept old-thing" rows :required t)) "the line stands between what can come back and what cannot")
          (is (row-carrying "All 6" rows) "the filters with their counts")
          (is (row-carrying "Agent 2" rows)))))))

(deftest going-back-greys-what-it-undoes-and-says-what-stays ()
  ;; The row the operator stands on is where going back leaves the folder:
  ;; the newer changes it undoes grey with an ↶, and nodecode moving alone is
  ;; no change a restore here undoes -- it follows a release, never a
  ;; checkout -- so it stays unmarked, and the line under the list says so.
  (with-frame-model (model 120 40 :session-id "history-undo")
    (with-history-gateway (calls)
      (history-sheet model)
      (press model :down)
      (is (equal "Kept rt-time, sf-time +1" (nle::history-change-title (history-on model))))
      (let ((rows (sheet-rows model)))
        (is (row-carrying "Nothing of yours moves; nodecode stays as it is" rows))
        (is (null (row-carrying "↶" (remove-if-not (lambda (row) (search "nodecode aaaaaaa" row)) rows))) "the nodecode row is not marked undone")
        (is (row-carrying "rt-time · sf-time · json-objs" rows) "the chosen change's keeps under it")
        (is (row-carrying "3 files  +40 −2  rt-time.lisp" rows) "and the files they moved")
        (is (row-carrying "11:07 PM–11:14 PM · generations 6–8" rows) "when, and which generations"))
      (press model :down)
      (is (sheet-row model "Undoes 1 change: 1 agent change; nodecode stays as it is"))
      (is (sheet-row model "↶" "Kept rt-time") "the agent's change greys with an ↶")))
  (with-frame-model (model 120 40 :session-id "history-follows")
    (with-history-gateway (calls :answer (history-answer :follows t))
      (history-sheet model)
      (press model :down) (press model :down)
      (is (sheet-row model "Undoes 2 changes: 1 agent change · 1 nodecode update") "a release's nodecode goes back with the rest"))))

(deftest the-question-asks-in-place-and-esc-stays-on-the-row ()
  ;; Enter asks where the row is: what going back costs, what the plan
  ;; route says moves and stays, the edits that hold it. Esc answers no and
  ;; leaves the operator on the same change.
  (with-frame-model (model 120 40 :session-id "history-ask")
    (with-history-gateway (calls)
      (history-sheet model)
      (press model :down)
      (press model :enter)
      (is (equal "/api/gateway/generations/plan?to=8" (second (first calls))) "read ahead, before anything moves")
      (let ((rows (sheet-rows model)))
        (is (row-carrying "Go back to" rows))
        (is (row-carrying "generation 8" rows))
        (is (row-carrying "mylayer has 13 uncommitted edits, and going back would lose them." rows))
        (is (row-carrying "mylayer to 1234567, once its uncommitted edits are kept or discarded" rows) "the plan's lines")
        (is (row-carrying "keep them, go back" rows))
        (is (row-carrying "discard them, go back" rows)))
      (press model :escape)
      (is (null (nle::generations-sheet-ask model.dialog)) "Esc answers no")
      (is (nle::generations-sheet-p model.dialog) "and the sheet stays open")
      (is (equal "Kept rt-time, sf-time +1" (nle::history-change-title (history-on model))) "on the same row")
      (is (null (posted calls)) "nothing was asked of the organism"))))

(deftest enter-keeps-the-edits-then-goes-back-and-d-discards-them-first ()
  (with-frame-model (model 120 40 :session-id "history-keep")
    (with-history-gateway (calls)
      (history-sheet model)
      (press model :down)
      (press model :enter)
      (press model :enter)
      (is (equal '(("/api/gateway/generations/edits" "keep") ("/api/gateway/generations/rollback" 8)) (posted calls)) "the edits kept, then the restore")
      (is (null model.dialog) "the sheet closes on the answer")
      (is-carrying (text (nle::status-said model))
        ("Kept mylayer's uncommitted edits as abc1234" "the keep's sentence")
        ("generation 8 restored" "then the restore's"))))
  (with-frame-model (model 120 40 :session-id "history-discard")
    (with-history-gateway (calls)
      (history-sheet model)
      (press model :down)
      (press model :enter)
      (dialog-keys model #\d)
      (is (equal '(("/api/gateway/generations/edits" "discard") ("/api/gateway/generations/rollback" 8)) (posted calls)) "d discards them first"))))

(deftest a-change-that-cannot-come-back-says-why-and-goes-nowhere ()
  (with-frame-model (model 120 40 :session-id "history-refused")
    (with-history-gateway (calls)
      (history-sheet model)
      (loop repeat 4 do (press model :down))
      (is (equal "Kept old-thing" (nle::history-change-title (history-on model))))
      (is (sheet-row model "× Can't go back here: nl-gone is gone"))
      (is (sheet-row model "enter" "why not"))
      (press model :enter)
      (is (sheet-row model "Can't go back to"))
      (is (sheet-row model "Nl-gone is gone, and generation 3 had it as an own folder") "the reason, whole")
      (is (sheet-row model "Pick a change above the red line."))
      (press model :enter)
      (dialog-keys model #\d)
      (is (null (posted calls)) "neither Enter nor d moves anything")
      (press model :escape)
      (is (equal "Kept old-thing" (nle::history-change-title (history-on model)))))))

(deftest the-banner-keeps-the-edits-in-place-and-the-list-reads-again ()
  (with-frame-model (model 120 40 :session-id "history-banner")
    (with-history-gateway (calls :answer (history-answer))
      (let ((history (history-sheet model)))
        (press model :up)
        (is (eq :banner (nle::generations-sheet-focus history)) "up from the newest change: the edits")
        (is (sheet-row model "keep them") "its keys")
        (press model :enter)
        (is (equal '(("/api/gateway/generations/edits" "keep")) (posted calls)))
        (is (equal "/api/gateway/generations" (second (first calls))) "then the list again")
        (is (sheet-row model "Kept mylayer's uncommitted edits as abc1234") "the organism's sentence over the list")
        (press model :down)
        (is (eq :pane (nle::generations-sheet-focus history)))))))

(deftest typing-searches-every-change-and-tab-steps-the-filters ()
  (with-frame-model (model 120 40 :session-id "history-search")
    (with-history-gateway (calls)
      (let ((history (history-sheet model)))
        (dialog-keys model #\j #\s #\o #\n)
        (is (equal '(1) (nle::history-visible history)) "a keep's name finds its change")
        (press model :escape)
        (is (equal "" (nle::generations-sheet-query history)))
        (is (equal "Kept rt-time, sf-time +1" (nle::history-change-title (history-on model))) "Esc clears the search and stays on what it found")
        (dialog-keys model #\# #\3)
        (is (equal "Kept old-thing" (nle::history-change-title (history-on model))) "#3 finds the change that holds it")
        (press model :escape)
        (press model :tab)
        (is (eq :agent (nle::generations-sheet-filter history)))
        (is (equal '(1 4) (nle::history-visible history)) "the agent's changes alone")
        (press model :tab) (press model :tab) (press model :tab)
        (is (eq :all (nle::generations-sheet-filter history)) "round to All")))))

(deftest the-days-rail-opens-a-day-and-esc-closes ()
  (with-frame-model (model 120 40 :session-id "history-rail")
    (with-history-gateway (calls)
      (let ((history (history-sheet model)))
        (press model :left)
        (is (eq :rail (nle::generations-sheet-focus history)))
        (press model :down) (press model :down)
        (press model :right)
        (is (eq :pane (nle::generations-sheet-focus history)))
        (is (equal "Installed nodecode-lsp" (nle::history-change-title (history-on model))) "the day's newest change")
        (press model :escape) (press model :escape)
        (is (null model.dialog))))))

(deftest generations-n-asks-about-n-in-place ()
  (with-frame-model (model 120 40 :session-id "history-n")
    (with-history-gateway (calls)
      (history-sheet model "7")
      (is (equal "/api/gateway/generations/plan?to=7" (second (first calls))))
      (is (equal "Kept rt-time, sf-time +1" (nle::history-change-title (history-on model))) "on the change that holds 7")
      (is (sheet-row model "generation 7"))
      (is (equal "generations takes a generation's number, or nothing: /generations [N]"
                 (nle::slash-generations-op model "x y")))))
  (with-frame-model (model 120 40 :session-id "history-n-missing")
    (with-history-gateway (calls)
      (history-sheet model "70")
      (is (sheet-row model "There is no generation 70: 1 to 9 are sealed")))))

(deftest the-sheet-folds-to-the-window-and-never-loses-the-change ()
  ;; Wide from 84 columns, the days beside the changes; narrower, the days or
  ;; the changes one view at a time; under 50 columns or 16 rows the whole
  ;; frame. Every row is the window's width, and the chosen change shows at
  ;; every size.
  (with-frame-model (model 120 40 :session-id "history-sizes")
    (with-history-gateway (calls)
      (history-sheet model)
      (press model :down)
      (flet ((size (width height)
               (nle:update model (nlt:make-window-size-msg :width width :height height))
               (sheet-rows model)))
        (is (row-carrying "Search changes, cells, sessions, #numbers" (size 120 40)) "wide: the search field")
        (is (row-carrying (nth-value 1 (nle::local-date (encode-universal-time 0 0 12 12 10 2026)))
                          (size 120 40)) "and the days")
        (dolist (size '((120 40) (100 30) (80 24) (70 20) (60 16) (48 14) (40 12)))
          (let ((rows (apply #'size size)))
            (is (row-carrying "Kept rt-time" rows) (format nil "~{~ax~a~} shows the change" size))
            (destructuring-bind (painted x y width) (nle::sheet-dialog-layout model.dialog)
              (declare (ignore x y))
              (is (every (lambda (row) (= width (nle::text-display-width (nle::plain-frame-row row)))) painted)
                  (format nil "~{~ax~a~} rows are the window's width" size)))))
        (is (row-carrying "Generations › " (size 70 20)) "narrow: the day it is on")
        (press model :left)
        (is (row-carrying "type to search" (size 70 20)) "narrow: the days alone")))))
;;; --- /generations without a shell: a room's card ------------------------------
;;; A room, the model's eval and the headless surface read the history the
;;; sheet does: one card of the changes to page and pick from, a pick that
;;; asks before anything moves, and the restore its question names.

(defun row-enter (row)
  "The line a dialog ROW's Enter runs, as the answer's JSON says it."
  (nlk:json-value row :string "run" "enter"))

(defun dialog-rows (dialog)
  (coerce (nlk:json-array dialog "rows") 'list))

(deftest generations-without-a-shell-answer-a-card-of-every-change ()
  ;; Never the manifests in words (821,001 characters, 775 generations of 23
  ;; folder rows each, on the operator's home): the changes the sheet lists,
  ;; newest first, each picked by the line that asks about going back to it,
  ;; the words the newest twenty.
  (with-stubbed-fdefinition (nle::generations-answer () (history-answer))
    (multiple-value-bind (text dialog) (nle::headless-generations "" nil)
      (let ((rows (dialog-rows dialog)))
        (is (equal '("/generations 9" "/generations 8" "/generations 5" "/generations 4" "/generations 3" "/generations 1")
                   (mapcar #'row-enter rows)) "a row a change, newest first, each asking about the generation it ends at")
        (is (equal "#8 Kept rt-time, sf-time +1" (nlk:json-value (second rows) :string "label")) "a row leads with the generation and what the change was")
        (is (search "you are here" (nlk:json-value (first rows) :string "detail")) "the change the folder stands at says so")
        (is (search "in “Analyze the run”" (nlk:json-value (second rows) :string "detail")) "where it was made, as the sheet says it")
        (is (search "can't come back" (nlk:json-value (fifth rows) :string "detail")) "a change past the red line says it cannot come back")
        (is (equal "✦" (nlk:json-value (second rows) :string "mark")) "its kind's glyph is its mark")
        (is (zerop (length (nlk:json-array dialog "tabs"))) "no tabs: a room's card gives its selected tab and Refresh one press, which Discord refuses")
        (is (equal "on #9 · 6 changes · 9 generations" (nlk:json-value dialog :string "context")))
        (is (search "mylayer has 13 uncommitted edits" (nlk:json-value dialog :string "summary")) "the edits that hold every restore are said once, over the list")
        (is (search "enter go back to" (nlk:json-value dialog :string "hint")) "what a pick does, for the menu a room draws"))
      (is (search "#8 Kept rt-time, sf-time +1" text) "the words say the changes")
      (is (= 8 (count #\Newline text)) "a title, the edits, six changes, the way to pick: nine lines")))
  (let ((many (history-answer)))
    (setf (gethash "changes" many)
          (coerce (loop for to from 30 downto 1
                        collect (history-entry :kind "agent" :from to :to to :at (local-iso 2026 10 14 12 (mod to 60))
                                               :title (format nil "Kept k~d" to)))
                  'vector))
    (with-stubbed-fdefinition (nle::generations-answer () many)
      (multiple-value-bind (text dialog) (nle::headless-generations "" nil)
        (is (= 30 (length (dialog-rows dialog))) "the card holds every change; the room pages it")
        (is (search "Kept k11" text) "the twentieth newest is said")
        (is (not (search "Kept k10" text)) "the twenty-first is not")
        (is (search "10 older" text) "the rest are counted")))))

(deftest generations-pick-without-a-shell-asks-before-anything-moves ()
  ;; A pick says /generations N, which asks -- what going back undoes and
  ;; moves, the edits that hold it -- and moves nothing; its rows name the
  ;; lines that do.
  (let ((rolled '()))
    (with-stubbed-fdefinitions ((nle::generations-answer () (history-answer))
                                (nle::generation-plan-answer (to) (plan-answer to))
                                (nle::rollback (number &key restart) (push number rolled) "restored"))
      (multiple-value-bind (text dialog) (nle::headless-generations "8" nil)
        (let ((rows (dialog-rows dialog)))
          (is (null rolled) "asking moves nothing")
          (is (uiop:string-prefix-p "Go back to " (nlk:json-value dialog :string "title")))
          (is (equal '("/generations 8 keep" "/generations 8 discard") (remove nil (mapcar #'row-enter rows))) "edits hold it: keep them first, or discard them")
          (is (find "mylayer to 1234567, once its uncommitted edits are kept or discarded" rows
                    :key (lambda (row) (nlk:json-value row :string "label")) :test #'equal) "what the restore moves, a line a folder")
          (is (equal "Nothing of yours moves; nodecode stays as it is" (nlk:json-value dialog :string "summary")) "what going back undoes: the one change between moved nodecode alone, which a checkout keeps")
          (is (equal "warn" (nlk:json-value dialog :string "tone"))))
        (is (search "/generations 8 keep" text) "the words name the line that goes"))
      (multiple-value-bind (text dialog) (nle::headless-generations "3" nil)
        (declare (ignore text))
        (is (uiop:string-prefix-p "Can't go back to " (nlk:json-value dialog :string "title")))
        (is (null (remove nil (mapcar #'row-enter (dialog-rows dialog)))) "nothing to press past the red line")
        (is (equal "danger" (nlk:json-value dialog :string "tone"))))
      (multiple-value-bind (text dialog) (nle::headless-generations "40" nil)
        (is (search "There is no generation 40: 1 to 9 are sealed" text))
        (is (null dialog))))
    (with-stubbed-fdefinitions ((nle::generations-answer () (history-answer :edits '()))
                                (nle::generation-plan-answer (to) (let ((plan (plan-answer to)))
                                                                    (setf (gethash "edits" plan) #())
                                                                    plan)))
      (multiple-value-bind (text dialog) (nle::headless-generations "4" nil)
        (declare (ignore text))
        (is (equal '("/generations 4 go") (remove nil (mapcar #'row-enter (dialog-rows dialog)))) "no edits hold it: one line goes"))
      (multiple-value-bind (text dialog) (nle::headless-generations "9" nil)
        (is (search "You are on" text) "the generation the folder stands at")
        (is (null (remove nil (mapcar #'row-enter (dialog-rows dialog)))) "nothing to go to")))))

(deftest generations-go-without-a-shell-keeps-or-discards-then-restores ()
  ;; The line a pick's question names: the edits that hold it kept or
  ;; discarded first, then the restore, the organism's sentences its answer.
  (let ((calls '()))
    (with-stubbed-fdefinitions ((nle::generations-answer () (history-answer))
                                (nle::generation-plan-answer (to) (plan-answer to))
                                (nle::generation-edits-text (name action)
                                  (push (list :edits name action) calls)
                                  (format nil "Kept ~a's uncommitted edits as abc1234; sealed as generation 10" name))
                                (nle::rollback (number &key restart)
                                  (push (list :rollback number) calls)
                                  (format nil "generation ~d restored" number)))
      (let ((text (nle::headless-generations "8 keep" nil)))
        (is (equal '((:edits "mylayer" "keep") (:rollback 8)) (reverse calls)) "keep, then go back")
        (is (search "Kept mylayer's uncommitted edits as abc1234" text))
        (is (search "generation 8 restored" text)))
      (setf calls '())
      (nle::headless-generations "8 discard" nil)
      (is (equal '((:edits "mylayer" "discard") (:rollback 8)) (reverse calls)) "discard, then go back")
      (setf calls '())
      (nle::headless-generations "8 go" nil)
      (is (equal '((:rollback 8)) calls) "go touches no edits")
      (setf calls '())
      (is (search "/generations [N [go|keep|discard]]" (nle::headless-generations "8 now" nil)) "anything else is the usage")
      (is (null calls) "and moves nothing"))
    (with-stubbed-fdefinitions ((nle::generations-answer () (history-answer))
                                (nle::generation-plan-answer (to) (plan-answer to))
                                (nle::rollback (number &key restart) (error "mylayer has 13 uncommitted edits")))
      (is (search "mylayer has 13 uncommitted edits" (nle::headless-generations "8 go" nil)) "a refusal is the answer, never a signal"))))

