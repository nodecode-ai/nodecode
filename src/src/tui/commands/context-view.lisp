;;;; context-view.lisp --- /context as a picture. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- /context as a picture --------------------------------------------------
;;; A shell paints /context as a docked picker: the window as a grid of
;;; cells, each part of the prompt in its own tone, the free room and the
;;; stretch past the eviction line under them, and the parts as the list
;;; beside it, each opening to what it holds -- history to its turns, where
;;; Enter evicts through one. The parts are the record's estimate
;;; (SESSION-CONTEXT-ESTIMATE) scaled to the newest round's measured prompt
;;; when one exists, so the grid and the header lane read one occupancy.

(defparameter +context-parts+
  '((:prompt "standing prompt" "context-prompt")
    (:tools "tools" "context-tools")
    (:index "skills and memories" "context-index")
    (:notes "notes" "context-notes")
    (:history "history" "thought"))
  "The parts /context splits the window into, in the grid's order, as (KEY
LABEL TONE): TONE the theme tone a part's cells and chip paint in.")

(defparameter +context-usage+
  "Usage: /context [prompt | tools | index | notes | history | time]"
  "What /context answers an argument it does not open.")

(defparameter +context-grid-width+ 40
  "The grid's cells across; with +CONTEXT-GRID-HEIGHT+ down, one cell is
1/400 of the window.")

(defparameter +context-grid-height+ 10
  "The grid's cells down: two to a text row, so five rows.")

(defparameter +context-palette+
  (append '("context-free" "context-zone" "context-other" "disclosure-hover")
          (mapcar #'third +context-parts+))
  "The grid's palette (LIST-DIALOG's picture): the free cells, the free cells
past the eviction line, another part's under a part's own view, a turn an
eviction takes, then each part's tone in +CONTEXT-PARTS+ order.")

(defparameter +context-section-names+
  '(("operator-rules" . "operator rules · AGENTS.md")
    ("instructions" . "project instructions")
    ("help" . "help topics")
    ("resumed" . "resume note"))
  "The words /context's prompt view names a harness section by, where its
own name is not them.")

(defun context-palette-char (entry)
  "The picture character that paints +CONTEXT-PALETTE+'s ENTRY."
  (digit-char (position entry +context-palette+ :test #'equal) 36))

(defun context-tone (key)
  "The tone part KEY paints in, as the keyword a row's mark takes."
  (intern (string-upcase (third (assoc key +context-parts+))) :keyword))

(defun context-breakdown (session-id provider model-id context-usage)
  "SESSION-ID's window split into +CONTEXT-PARTS+ as a plist: :TOTAL the
occupancy (CONTEXT-USAGE's measured prompt, else the record's estimate),
:ESTIMATED whether it is the estimate, :PARTS each part as (:KEY :TOKENS
:ITEMS), ITEMS (TEXT TOKENS) pairs, and :UNITS the retained history units
(HISTORY-UNIT-FACTS) with :TOKENS scaled like the parts. NIL when the
record holds nothing to estimate."
  (multiple-value-bind (estimate parts) (session-context-estimate session-id provider model-id)
    (when (and estimate (plusp estimate))
      (let* ((measured (and context-usage (context-occupancy-tokens context-usage)))
             (total (if (and measured (plusp measured)) measured estimate))
             (factor (/ total estimate))
             (sections (getf parts :sections))
             (notes (loop for unit in (getf parts :units) append (getf unit :notes))))
        (labels ((scaled (tokens) (round (* factor tokens)))
                 (part (key tokens pairs)
                   (list :key key :tokens tokens
                         :items (loop for (text . n) in pairs collect (list text (scaled n))))))
          (let* ((index (scaled (or (cdr (assoc "index" sections :test #'string=)) 0)))
                 (prompt (max 0 (- (scaled (getf parts :prompt)) index)))
                 (tools (scaled (getf parts :tools)))
                 (noted (scaled (reduce #'+ notes :key #'cdr)))
                 (units (loop for unit in (getf parts :units)
                              collect (list* :tokens (scaled (getf unit :tokens)) unit))))
            (list :total total
                  :estimated (not (and measured (plusp measured)))
                  :units units
                  :parts (list (part :prompt prompt
                                     (remove "index" sections :key #'car :test #'string=))
                               (part :tools tools (getf parts :tool-sizes))
                               (part :index index '())
                               (part :notes noted notes)
                               (list :key :history
                                     :tokens (max 0 (- total prompt tools index noted))
                                     :items (loop for unit in units
                                                  collect (list (getf unit :prompt)
                                                                (getf unit :tokens))))))))))))

(defun context-scale (breakdown window)
  "The tokens the grid's whole spans: WINDOW, or BREAKDOWN's total when no
source knows the window."
  (or window (max 1 (getf breakdown :total))))

(defun context-share (tokens breakdown window)
  "TOKENS as a share of the grid's whole, `36.0%'."
  (format-occupancy-percent tokens (context-scale breakdown window)))

(defun context-grid-picture (breakdown window &key focus (width +context-grid-width+)
                                                     (height +context-grid-height+))
  "The grid LIST-DIALOG paints for BREAKDOWN, as rows of +CONTEXT-PALETTE+
characters: WIDTH cells across and HEIGHT down, each an equal share of
WINDOW, filled in +CONTEXT-PARTS+ order, then the free room, the zone's past
the eviction line. FOCUS, a part's key, paints the other parts' cells muted."
  (let* ((cells (* width height))
         (unit (/ (context-scale breakdown window) cells))
         (spans '())
         (start 0))
    ;; Each part a span of the window.
    (flet ((span (tokens entry)
             (push (list start (+ start tokens) (context-palette-char entry)) spans)
             (incf start tokens)))
      (dolist (part (getf breakdown :parts))
        (destructuring-bind (&key key tokens &allow-other-keys) part
          (span tokens (if (and focus (not (eq focus key)))
                           "context-other"
                           (third (assoc key +context-parts+)))))))
    (loop for row below height
          collect (let ((line (make-string width)))
                    (dotimes (column width line)
                      (let* ((middle (* unit (+ 0.5 column (* row width))))
                             (span (find-if (lambda (span) (and (<= (first span) middle)
                                                                (< middle (second span))))
                                            spans)))
                        (setf (char line column)
                              (cond (span (third span))
                                    ((and window (>= middle (* window *context-eviction-trigger-ratio*)))
                                     (context-palette-char "context-zone"))
                                    (t (context-palette-char "context-free"))))))))))

(defparameter +context-strip-cells+ 10
  "The cells of the grid's one-row rendition, a room's card's (LIST-DIALOG's
:STRIP): a tenth of the window each.")

(defun context-picture (rows &optional strip)
  "The picture LIST-DIALOG carries for the grid ROWS: beside the list where
the panel is wide enough; STRIP, its one-row rendition."
  (list :rows rows :palette +context-palette+ :beside t :strip strip
        :label "The window as a grid"))

(defun context-grid (breakdown window &key focus)
  "The picture of BREAKDOWN in WINDOW (CONTEXT-PICTURE): the grid, and the
same window in +CONTEXT-STRIP-CELLS+ cells; FOCUS as CONTEXT-GRID-PICTURE's."
  (context-picture (context-grid-picture breakdown window :focus focus)
                   (first (context-grid-picture breakdown window :focus focus
                                                                 :width +context-strip-cells+ :height 1))))

(defun context-occupancy-tone (tokens window)
  "The tone TOKENS in WINDOW warm to: :DANGER past the eviction line, :WARN
from three quarters of the window, else NIL -- and NIL with no window."
  (let ((share (and window (plusp window) (/ tokens window))))
    (cond ((null share) nil)
          ((>= share *context-eviction-trigger-ratio*) :danger)
          ((>= share 3/4) :warn))))

(defun context-summary (label grade window total)
  "The line under /context's title => (values TEXT TONE): what it runs on,
where the window came from and where eviction starts -- or, in the danger
tone, that the next request evicts."
  (let ((trigger (round (* 100 *context-eviction-trigger-ratio*))))
    (cond ((null window)
           (values (format nil "~@[~a · ~]window unknown · nothing evicts until a window is known" label)
                   :muted))
          ((>= total (* window *context-eviction-trigger-ratio*))
           (values (format nil "past the ~d% line: the next request evicts the oldest turns" trigger)
                   :danger))
          (t (values (format nil "~@[~a · ~]~a window · evicts at ~d% (~a), down to ~d%"
                             label grade trigger
                             (format-token-count (* window *context-eviction-trigger-ratio*))
                             (round (* 100 *context-eviction-floor-ratio*)))
                     :muted)))))

(defun context-title-context (breakdown window &aux (total (getf breakdown :total))
                                                    (mark (if (getf breakdown :estimated) "~" "")))
  "The muted words beside /context's title: `88.5k of 200k · 44.3%', `~'
before an estimate."
  (if window
      (format nil "~a~a of ~a · ~a" mark (format-token-count total)
              (format-token-count window) (format-occupancy-percent total window))
      (format nil "~a~a" mark (format-token-count total))))

(defun context-part-label (part breakdown)
  "PART's row label: its name, and how many it holds where that says more."
  (let ((name (second (assoc (getf part :key) +context-parts+))))
    (case (getf part :key)
      ((:tools :notes) (format nil "~a · ~d" name (length (getf part :items))))
      (:history (format nil "~a · ~d turn~:p" name (length (getf breakdown :units))))
      (t name))))

(defun context-amount (tokens breakdown window)
  "A row's detail: TOKENS and their share, right-aligned so rows line up."
  (format nil "~6@a  ~6@a" (format-token-count tokens) (context-share tokens breakdown window)))

(defun context-dialog (breakdown window summary tone rounds trailing)
  "The docked picker /context answers with: the grid beside the parts, each
opening its own view (/context KEY; the index, /index), the free room, and
the over-time row when ROUNDS (CONTEXT-ROUNDS, TRAILING its second value)
holds any."
  (list-dialog
   :title "Context"
   :context (context-title-context breakdown window)
   :summary summary :summary-tone tone
   ;; The state a room's card wears: a known window is fine until it warms.
   :tone (and window (or (context-occupancy-tone (getf breakdown :total) window) :success))
   :picture (context-grid breakdown window)
   :hint "enter open  esc close  up/down navigate"
   :rows (append
          ;; A part holding nothing is no row; history always is one.
          (loop for part in (getf breakdown :parts)
                for key = (getf part :key)
                when (or (eq key :history) (plusp (getf part :tokens)))
                collect (list :label (context-part-label part breakdown)
                              :detail (context-amount (getf part :tokens) breakdown window)
                              :mark "██" :mark-tone (context-tone key)
                              :run (list :enter (if (eq key :index)
                                                    "/index"
                                                    (format nil "/context ~(~a~)" key)))))
          ;; No window, no free room: the grid's whole is then what is in it,
          ;; and its free row would read a measured zero.
          (when window
            (let ((free (max 0 (- window (getf breakdown :total)))))
              (list (list :label "free" :detail (context-amount free breakdown window)
                          :mark "██" :mark-tone :context-free))))
          (when rounds
            (list (list :label "over time"
                        :detail (format nil "~d round~:p · ~d eviction~:p"
                                        (length rounds) (+ (count-if #'second rounds)
                                                           (if trailing 1 0)))
                        :mark "▸" :mark-tone :muted
                        :run (list :enter "/context time")))))))

(defun context-first-line (text)
  "TEXT's first non-blank line, trimmed: what a row names a prompt or note by."
  (find-if #'plusp (mapcar #'trimmed (lines text)) :key #'length))

(defun context-history-rows (breakdown window)
  "The history view's rows: each retained turn by its number and size, in
the order the turns end -- where an eviction through one cuts -- and, the
selected row's, what evicting through it frees, which Enter does (/evict
through TURN); the newest turn and a running one say why they stay."
  ;; A turn's late fact (a background exit) is a unit of its own after the
  ;; next turn began: the turn's row gathers its units and stands where its
  ;; last one does, since an eviction through it takes every unit up to there.
  (let* ((units (coerce (getf breakdown :units) 'vector))
         (count (length units))
         (cap (min (1- count) (1+ (or (position-if (lambda (unit) (getf unit :terminal)) units
                                                   :from-end t)
                                      -1))))
         (turns '())
         (total (getf breakdown :total)))
    (loop for unit across units
          for index from 0
          for id = (getf unit :turn-id)
          for turn = (or (find id turns :key (lambda (turn) (getf turn :turn-id)) :test #'equal)
                         ;; Every key a row reads is in place from the start, so
                         ;; the SETF GETFs below write this plist, not a copy.
                         (car (push (list :turn-id id :number (getf unit :number) :tokens 0
                                          :last 0 :terminal nil :prompt "")
                                    turns)))
          do (incf (getf turn :tokens) (getf unit :tokens))
             (setf (getf turn :last) index
                   (getf turn :terminal) (getf unit :terminal))
             (unless (context-first-line (getf turn :prompt))
               (setf (getf turn :prompt) (getf unit :prompt))))
    (loop for turn in (sort turns #'< :key (lambda (turn) (getf turn :last)))
          for evictable = (< (getf turn :last) cap)
          for freed = (reduce #'+ units :end (1+ (getf turn :last))
                                        :key (lambda (unit) (getf unit :tokens)))
          collect (list :key (format nil "t~d" (getf turn :number))
                        :label (or (context-first-line (getf turn :prompt)) "(no prompt in the window)")
                        ;; Beside the grid a row has some forty columns: the
                        ;; size, and on the selected row what Enter frees --
                        ;; the hint row names the key.
                        :detail (format nil "~6@a" (format-token-count (getf turn :tokens)))
                        :suffix (cond ((not evictable)
                                       (if (getf turn :terminal) "  · kept: newest" "  · kept: running"))
                                      (t (format nil "  −~a → ~a"
                                                 (format-token-count freed)
                                                 (context-share (max 0 (- total freed)) breakdown window))))
                        :run (and evictable
                                  (list :enter (format nil "/evict through ~a" (getf turn :turn-id))))))))

(defun context-part-answer (key breakdown window)
  "/context KEY => (values TEXT DIALOG): the part's items, largest first --
history's turns oldest first -- as lines, and as the picker that shows them
beside the grid with the other parts muted."
  (let* ((part (find key (getf breakdown :parts) :key (lambda (part) (getf part :key))))
         (rows (if (eq key :history)
                   (context-history-rows breakdown window)
                   (loop for (text tokens) in (sort (copy-list (getf part :items)) #'> :key #'second)
                         collect (list :label (or (cdr (assoc text +context-section-names+ :test #'string=))
                                                  (context-first-line text)
                                                  "(empty)")
                                       :detail (context-amount tokens breakdown window)))))
         (about (ecase key
                  (:prompt "sent with every request; no eviction reaches it")
                  (:tools "each tool's schema, sent with every request; /cells unloads a cell's")
                  (:index "the skills and memories index; /index opens it")
                  (:notes "what the model wrote under [[note]] in context.txt; they leave with their turn")
                  (:history "the conversation; the oldest turns leave first at the eviction line"))))
    (values (format nil "~a · ~a~%~{~a~%~}" (second (assoc key +context-parts+))
                    (context-amount (getf part :tokens) breakdown window)
                    (loop for row in rows
                          collect (format nil "  ~@[~a  ~]~a  ~a"
                                          (getf row :key) (getf row :label) (getf row :detail))))
            (dialog-json
             (list-dialog
              :title (format nil "Context / ~a" (second (assoc key +context-parts+)))
              :context (context-amount (getf part :tokens) breakdown window)
              :summary about :summary-tone :muted
              :picture (context-grid breakdown window :focus key)
              :hint (if (eq key :history)
                        "enter evict through  esc back  up/down navigate"
                        "esc back  up/down navigate")
              :empty-label "Nothing here"
              :rows rows)))))

(defun context-rounds (session-id &aux (rounds '()) (pending nil))
  "SESSION-ID's provider rounds, oldest first, each (TOKENS EVICTION): the
prompt the round placed in the window (CONTEXT-OCCUPANCY-TOKENS over its
turn.usage fact) and the session.context.evicted fact that moved the floor
since the round before, NIL for none. The second value is an eviction no
round has followed yet."
  (dolist (event (nlk:events :session-id session-id
                             :kind (list "turn.usage" "session.context.evicted")
                             :order :oldest :as :instances)
                 (values (nreverse rounds) pending))
    (typecase event
      (nlk:context-evicted-event (setf pending event))
      (nlk:turn-usage-event
       ;; A round that reported no prompt (a voided attempt, a side
       ;; call's empty usage) placed nothing in the window.
       (unless (nlk:turn-usage-event-voided event)
         (let ((state (make-hash-table :test #'equal)))
           (apply-event state event)
           (let ((tokens (context-occupancy-tokens (gethash "context_usage" state))))
             (when (plusp tokens)
               (push (list tokens pending) rounds)
               (setf pending nil)))))))))

(defun context-time-picture (rounds scale window)
  "ROUNDS (CONTEXT-ROUNDS) as a column chart in the grid's palette: each of
+CONTEXT-GRID-WIDTH+ columns the largest prompt of the rounds it spans,
over SCALE tokens, a column holding the first round after an eviction in
the eviction's tone."
  (let* ((count (length rounds))
         (columns (loop for column below +context-grid-width+
                        for from = (floor (* column count) +context-grid-width+)
                        for to = (max (1+ from) (floor (* (1+ column) count) +context-grid-width+))
                        for span = (subseq rounds (min from (1- count)) (min to count))
                        collect (cons (reduce #'max span :key #'first)
                                      (some #'second span)))))
    (loop for row below +context-grid-height+
          for level = (- +context-grid-height+ row)
          collect (coerce
                   (loop for (tokens . evicted) in columns
                         collect (context-palette-char
                                  (cond ((>= (* +context-grid-height+ (/ tokens scale)) (- level 0.5))
                                         (if evicted "disclosure-hover" "thought"))
                                        ((and window (>= (/ (- level 0.5) +context-grid-height+)
                                                         *context-eviction-trigger-ratio*))
                                         "context-zone")
                                        (t "context-free"))))
                   'string))))

(defun context-eviction-row (label eviction before after)
  "The over-time row of EVICTION, a session.context.evicted fact: LABEL (the
round it came before), the turns it took, the prompt BEFORE it and AFTER."
  (list :key "▾"
        :label (format nil "~a · ~d turn~:p" label (nlk:context-evicted-turn-count eviction))
        :detail (format nil "~a → ~a · ~a"
                        (format-token-count before)
                        (format-token-count after)
                        (let ((reason (nlk:context-evicted-reason eviction)))
                          (dispatch reason equal
                            ("operator_requested" "asked") ("budget_fill" "at the line")
                            ("provider_rejected" "refused by the provider") (t reason))))))

(defun context-time-answer (rounds trailing breakdown window)
  "/context time => (values TEXT DIALOG): each round's prompt from the log as
a chart, and each eviction as a row -- the round it came before, the turns it
took, the prompt before and after it; TRAILING, one no round has followed,
against the occupancy now."
  (if (null rounds)
      "no provider round in the log yet"
      (let* ((peak (reduce #'max rounds :key #'first))
             (scale (or window (max 1 peak)))
             (now (if breakdown (getf breakdown :total) (first (car (last rounds)))))
             (rows (append
                    (loop for (tokens eviction) in rounds
                          for previous = 0 then before
                          for before = tokens
                          for number from 1
                          when eviction
                            collect (context-eviction-row (format nil "round ~d" number)
                                                          eviction previous tokens))
                    (when trailing
                      (list (context-eviction-row "now" trailing (first (car (last rounds))) now)))
                    (list (list :label "peak" :detail (format-token-count peak))
                          (list :label "now" :detail (format-token-count now))))))
        (values (format nil "~d round~:p · peak ~a~%~{~a~%~}" (length rounds) (format-token-count peak)
                        (loop for row in rows
                              collect (format nil "  ~a  ~a" (getf row :label) (getf row :detail))))
                (dialog-json
                 (list-dialog
                  :title "Context / over time"
                  :context (format nil "~d round~:p · top: ~a" (length rounds) (format-token-count scale))
                  :summary "each round's prompt, read from the session log" :summary-tone :muted
                  :picture (context-picture (context-time-picture rounds scale window))
                  :hint "esc back  up/down navigate"
                  :rows rows))))))

(defun headless-context (args session-id)
  "The /context answer without a shell: SESSION-ID's usage facts folded
from its durable turn.usage events (the newest 512 rounds) and the window
belief for its resolved target -- the text, and the docked picker a shell
paints it as (CONTEXT-DIALOG). ARGS opens a part (+CONTEXT-PARTS+'s keys)
or `time', each round's prompt from the log."
  ;; A session with no round at all has its occupancy estimated from the
  ;; durable record (SESSION-CONTEXT-ESTIMATE), and a window no source knows
  ;; is reported with the floor the evidence proves.
  (require-headless-session session-id "context")
  (let* ((asked (string-downcase (string-trim " " (or args ""))))
         (part (find asked +context-parts+ :key #'first :test #'string-equal))
         (rows (nlk:events :session-id session-id :kind "turn.usage"
                           :order :newest :limit 512 :as :instances))
         (newest (first rows))
         (session-state (make-hash-table :test #'equal))
         (turn-state (make-hash-table :test #'equal))
         (context-state (make-hash-table :test #'equal)))
    (unless (or part (zerop (length asked)) (string= asked "time"))
      (return-from headless-context +context-usage+))
    ;; Each fact folds through the fold's own turn.usage reducer (APPLY-EVENT).
    (dolist (event rows) (apply-event session-state event))
    (dolist (event rows) (when (equal newest.turn-id event.turn-id) (apply-event turn-state event)))
    (when newest (apply-event context-state newest))
    ;; The window is the one the session's next turn binds (GATEWAY-TARGET:
    ;; its pin over the gateway's default), so a room that has run no round
    ;; yet still knows it; only an unconfigured default, which names no model
    ;; a person chose, gives way to the newest round's.
    (bind (((target-provider target-model _ _ unconfigured) (gateway-target session-id))
           (provider (if (and unconfigured newest)
                         (nlk:turn-usage-event-provider newest)
                         target-provider))
           (model-id (if (and unconfigured newest)
                         (nlk:turn-usage-event-model newest)
                         target-model))
           (effort (and newest (nlk:turn-usage-event-reasoning-effort newest)))
           (label (status-identity-with-effort provider model-id effort))
           (context-usage (gethash "context_usage" context-state))
           ;; A prompt measured before the newest eviction counts turns the
           ;; window no longer holds: the record's estimate is the occupancy
           ;; until the next round reports one.
           (evicted (and newest
                         (nlk:events :session-id session-id :kind "session.context.evicted"
                                     :order :newest :limit 1 :as :instance)))
           (measured (and context-usage
                          (not (and evicted (> (nlk:event-log-position evicted)
                                               (nlk:event-log-position newest))))
                          context-usage))
           ((window grade) (context-window-view provider model-id))
           (breakdown (context-breakdown session-id provider model-id measured)))
      (cond
        ((string= asked "time")
         (multiple-value-call #'context-time-answer (context-rounds session-id) breakdown window))
        (part
         (if breakdown
             (context-part-answer (first part) breakdown window)
             "nothing in the window yet"))
        (t
         (values (context-report-text provider model-id
                                      :label label
                                      :session-id session-id
                                      :usage (gethash "usage" turn-state)
                                      :session-usage (gethash "session_usage" session-state)
                                      :context-usage context-usage)
                 (and breakdown
                      (dialog-json
                       (multiple-value-bind (summary tone)
                           (context-summary label grade window (getf breakdown :total))
                         (multiple-value-call #'context-dialog breakdown window summary tone
                           (context-rounds session-id)))))))))))

(defun slash (line &key (session-id nlk:*scribe-session-id*))
  "Run one slash command LINE — \"/models grok-4.6\", \"/think high\",
\"/help\" — without a shell, against SESSION-ID (by default the session
this eval is running for), and answer the text a shell would show; the
second value is the panel a cell's command answered with (DIALOG-JSON),
for a caller that draws one."
  ;; The builtins with a headless arm (SLASH-COMMAND-HEADLESS) land on the
  ;; same canonical surfaces the shells' /models, /think, /undo, /redo,
  ;; /clear, /evict, /context, /cells and /gc do; a cell's command is an
  ;; entry of the same shape whose arm runs it (REGISTER-COMMAND);
  ;; (SLASH-CATALOG) lists them all and /help renders that list. A command
  ;; whose meaning is a dialog or a shell move answers that it needs an
  ;; interactive shell; an unknown name says so. Operation failures signal, as
  ;; the verbs they land on do — an unknown lane, an off-ladder effort, a turn
  ;; in flight.
  (check-type line string)
  (multiple-value-bind (name args) (parse-slash-input line)
    (unless name
      (error "not a slash command: ~s" line))
    (let ((entry (or (find-builtin-slash-command name) (find-registered-command name))))
      (cond
        ((null entry) (format nil "unknown command /~a; /help lists them" name))
        (entry.headless (funcall entry.headless args session-id))
        (t (format nil "/~a needs an interactive shell" entry.name))))))

(defun model-pick-body (tokens)
  "The body a pick of TOKENS posts to a model route: (MODEL) or (PROVIDER MODEL)."
  (json-object :opt "provider" (and (= 2 (length tokens)) (first tokens))
               "model" (first (last tokens))))

(defun shared-config-failure (object)
  "Why a model route's answer OBJECT says the shared config was not saved, or NIL."
  (and (equal "failed" (json-value object :string "shared_config"))
       (json-value object :string "shared_config_error")))

(defun slash-models-op (model tokens)
  "Show (no TOKENS) or change (1 token: model; 2 tokens: provider model)
the active selection. Returns notice text: the effective selection, or the
pick landed on THIS SESSION (SELECT-SESSION-MODEL): the durable per-session
fact outranks the gateway default for this session's turns and survives
restarts, and the pick is the default new sessions start on."
  (with-slash-notice ("model selection")
      (if (null tokens)
          (let ((object (gateway-model-http-call model :get
                                                 "/api/gateway/model" nil))
                (facts (gateway-model-facts model)))
            (format nil "provider: ~a~%model: ~a~%endpoint: ~a~%~a~
                         ~@[~%session override: ~a~]"
                    (gethash "provider" object)
                    (gethash "model" object)
                    (or (json-value object :string "endpoint")
                        (format nil "none: ~a" (json-value object :text "refusal")))
                    (apply #'auxiliary-model-summary
                           (mapcar (lambda (key) (json-value object :string "auxiliary" key))
                                   '("provider" "model" "effort")))
                    (when (getf facts :pinned)
                      (format nil "~@[~a ~]~a"
                              (getf facts :pin-provider)
                              (or (getf facts :pin-model) "(lane default)")))))
          (let ((object (session-control-call model :post "model" :body (model-pick-body tokens))))
            ;; The accepted selection moved the session's target, so the
            ;; cached facts — identity, pin provenance and the effort resolved
            ;; against the old target — are all stale together.
            (refresh-gateway-model-facts model)
            (let ((picked (json-value object :string "selected_model")))
              (format nil "session model: ~@[~a ~]~a~:[~; — new sessions start on it too~]~@[~%shared config: NOT saved — ~a~]"
                      (json-value object :string "selected_provider")
                      (or picked "(cleared)")
                      picked
                      (shared-config-failure object)))))))

(defun slash-default-model-op (model tokens)
  "Land TOKENS (1 token: model on the default's lane; 2 tokens: provider
model) as the gateway default the way Control's Settings does (POST
/api/gateway/model): every session not yet begun starts on it, and the
shared config keeps it. Returns notice text."
  ;; No session is pinned; a shell whose session has not begun learns from
  ;; the model_changed frame the op broadcasts.
  (with-slash-notice ("default model selection")
      (let ((object (gateway-model-http-call model :post "/api/gateway/model"
                                             (model-pick-body tokens))))
        (refresh-gateway-model-facts model)
        (format nil "default model: ~a/~a~@[~%shared config: NOT saved — ~a~]"
                (json-value object :string "provider")
                (json-value object :string "model")
                (shared-config-failure object)))))

(defun slash-auxiliary-model-op (model tokens &key (effort nil effort-p))
  "The arm behind the Auxiliary tab and `/models aux': show (no TOKENS) or
change (1 token: model; 2 tokens: provider model; `default': none) the
auxiliary model — the target a session's naming, a recap or a summary runs
on — with EFFORT, at that rung (NIL: the lightest). The gateway's
auxiliary-model op lands it (POST /api/gateway/auxiliary-model), or answers
it (GET)."
  ;; Lands the image's target and general.auxiliary_model together and pins no
  ;; session: no session facts move, so nothing is refreshed. Returns notice
  ;; text.
  (with-slash-notice ("auxiliary model selection")
      (let ((object (gateway-model-http-call
                     model (if tokens :post :get) "/api/gateway/auxiliary-model"
                     (cond ((equalp tokens '("default")) (json-object "model" :null))
                           (effort-p (json-object "provider" (first tokens) "model" (second tokens)
                                                  "effort" (or effort :null)))
                           (tokens (model-pick-body tokens))))))
        (format nil "~a~@[~%shared config: NOT saved — ~a~]"
                (auxiliary-model-summary (json-value object :string "provider")
                                         (json-value object :string "model")
                                         (json-value object :string "effort"))
                (shared-config-failure object)))))

(defun slash-decider-model-op (model tokens)
  "The arm behind the Decider tab and `/models decider': show (no TOKENS) or
pin (a decider id, then optionally its model; `none' takes it out) the
decider general.decider_model names, through the gateway's decider-model op
(GET / POST /api/gateway/decider-model). Returns notice text."
  (with-slash-notice ("decider model selection")
      (let ((object (gateway-model-http-call
                     model (if tokens :post :get) "/api/gateway/decider-model"
                     (cond ((equalp tokens '("none")) (json-object "provider" :null))
                           (tokens (json-object "provider" (first tokens)
                                                :opt "model" (second tokens)))))))
        (format nil "~a~@[~%shared config: NOT saved — ~a~]"
                (decider-model-summary (json-value object :string "provider")
                                       (json-value object :string "model"))
                (shared-config-failure object)))))

(defun switch-gateway-session (model session-id)
  "Attach MODEL to SESSION-ID: reset folded state and watermarks, then
resubscribe — the replay that answers rebuilds the transcript."
  ;; A /sessions preview already holding SESSION-ID's folded history seeds the
  ;; new state (the Zig adoptSessionsDialogPreview promotion), so the
  ;; transcript carries the entered session from this frame on rather than
  ;; reading empty until the replay lands. The replay still rebuilds it
  ;; authoritatively.
  ;;
  ;; An empty destination is the home screen (Zig create_session +
  ;; isHomeScreen): the landing band is its feedback. A destination with
  ;; history acknowledges the switch in the five-second composer hint, never
  ;; in transcript chrome. Return NIL so slash/picker callers cannot put the
  ;; acknowledgement back at the transcript tail.
  (let* ((preview model.transcript-preview)
         (adopted (and (equal session-id (car preview)) (cdr preview))))
    (invalidate-gateway-model-facts model)
    ;; A found-block pin belongs to the session it was found in; the caller
    ;; that means to keep one pins again right after the switch.
    (clear-transcript-pin model)
    (setf (slot-value model 'session-id) session-id
          model.state (or adopted (initial-chat-state))
          ;; The preview lane empties either way, and the switch resubscribes
          ;; to its destination alone: the follow ends with the lane.
          model.transcript-preview nil
          model.preview-session-id nil
          model.preview-activity nil
          model.last-sequence nil
          model.last-cursor nil
          model.reconnect-required nil
          model.replay-pending-turn-id nil
          model.quiet-resync nil)
    (note-turn-flight-settled model)
    (clear-retract-stash model)
    ;; The pending start_turn belongs to the session we just left.
    ;; Keeping it fails the gateway home-screen-p gate and leaves a
    ;; blank band after /clear mid-turn.
    (clear-pending-command model)
    (show-status model nil)
    (when adopted
      ;; The adopted rows are the transcript now; paint them before the
      ;; subscribe rather than leaving the previous session's frame up.
      (pin-transcript-bottom model)
      (render-state model))
    (send-subscribe model.ws (list session-id))
    (note-shell-presence session-id)
    ;; The facts this shell held were the session it left: its pin, its
    ;; effort. The switch blanked them (INVALIDATE-GATEWAY-MODEL-FACTS) and
    ;; nothing else refetches for a switch -- the gateway broadcasts
    ;; MODEL_CHANGED only when a target moves, and a subscribe is
    ;; no fresh connection -- so without this read the footer and the turn
    ;; bar would say `setting up…' for as long as the shell stays here. Scoped
    ;; to the destination: the slot above already names it.
    (refresh-gateway-model-facts model)
    (when (transcript-rows model.state)
      (show-status model (format nil "Switched to ~a" session-id)))
    nil))

(defun slash-navigate-op (model direction)
  "Fire an undo/redo navigation. Returns notice text."
  (send-navigate-checkpoint model.ws
                            model.session-id
                            :direction direction)
  nil)

