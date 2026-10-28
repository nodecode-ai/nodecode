;;;; timestamps.lisp --- transcript clocks and owning-turn hover details.
;;;; SPDX-License-Identifier: MIT
;;;; Durable event times/usage survive replay; first-paint clocks and pointer
;;;; ownership are live-only. No timestamp is inferred from message text.

(in-package #:nodecode.evolved)


;; Every view that folds gateway events notes into this one table -- the
;; shell's own fold, and the gateway's rows views, each on the thread serving
;; it -- so it is synchronized, and each read-modify-write holds its lock.
(defvar *transcript-time-facts* (make-hash-table :test 'equal :synchronized t) "Persisted-history event clocks: log position -> universal-time-ms; turn id -> timing plist.")

(defvar *transcript-live-times* (make-hash-table :test 'eq :weakness :key) "Live-only first-paint clocks for rows without durable event ownership.")

(defvar *message-timestamp-hovers* (make-hash-table :test 'eq :weakness :key) "Live-only pointer owner for timestamp tooltips: model -> (row x y).")

(defun note-transcript-time (position at kind turn-id)
  "Record event clocks before projection; never replace historical time with render time."
  (when-let (ms (and at (if (integerp at) at (iso-8601-ms at))))
    (when (integerp position) (setf (gethash position *transcript-time-facts*) ms))
    (when turn-id
      (sb-ext:with-locked-hash-table (*transcript-time-facts*)
        (let ((timing (copy-list (gethash turn-id *transcript-time-facts*))))
          (cond ((equal kind "turn.started") (setf (getf timing :started) ms))
                ((member kind '("turn.completed" "turn.cancelled" "turn.failed") :test #'equal)
                 (setf (getf timing :ended) ms)
                 (when (and (getf timing :started) (<= (getf timing :started) ms))
                   (setf (getf timing :elapsed) (- ms (getf timing :started))))))
          (setf (gethash turn-id *transcript-time-facts*) timing))))))

;; Retain canonical turn totals beside timestamp facts, then expose them on the user's timestamp.
(defun note-transcript-usage (turn-id usage)
  "Snapshot the turn's canonical usage accumulator into its timestamp facts."
  (when turn-id
    (sb-ext:with-locked-hash-table (*transcript-time-facts*)
      (let ((timing (copy-list (gethash turn-id *transcript-time-facts*))))
        (dolist (key '(:provider :model :response-model :effort :finish-reason))
          (when (json-value (getf usage key) :text)
            (setf (getf timing key) (getf usage key))))
        (setf (getf timing :usage) (copy-list usage)
              (gethash turn-id *transcript-time-facts*) timing)))))

(defun transcript-clock (row)
  "Use the original fact's time for history and a stable first-paint clock for live ephemera."
  (let* ((position (transcript-row-log-position row))
         (time
          (if position
              (gethash position *transcript-time-facts*)
              (alexandria:ensure-gethash row *transcript-live-times* (iso-8601-ms (iso-now))))))
    (if time
        (format-clock-minute (- time (* 1000 +unix-epoch+)))
        "time unknown")))

(defun stamp-transcript-llines (lines clock width)
  "LINES with CLOCK muted at the right end of the first one's first row at
WIDTH, or on a row of its own above them where that row cannot be cut short."
  ;; The clock holds its place and the words wrap short of it, below.
  ;; A first line laid out to the full row -- a table's row, a rule, an
  ;; image band, a code line -- would break on a row cut short, so the
  ;; clock stands over it.
  (if (or (null lines) (< width *aside-min-width*))
      lines
      (let* ((first (first lines))
             (aside (make-seg :text clock :tone :muted)))
        (if (and (not (lline-rule first)) (not (lline-image first)) (not (lline-grid first))
                 (member (lline-role first) '(:prose :status)))
            (cons (copy-with lline first :aside aside) (rest lines))
            (cons (make-lline :text "" :tone (lline-tone first) :fill (lline-fill first) :aside aside)
                  lines)))))

(defun message-timestamp-row-p (row)
  "Exact Boolean eligibility: only user/assistant message blocks carry timestamps."
  (and (transcript-row-p row) (eq (transcript-row-kind row) :text)
       (not (null (member (transcript-row-role row) '("user" "assistant") :test #'equal)))))

;; Preserve eviction history: that legacy row shape shares :FINISH-DIVIDER, but only terminal lifecycle dividers are redundant.
(defun redundant-finish-divider-p (row)
  "Exact Boolean: a terminal turn divider, not the context-eviction count notice sharing its row shape."
  (and (transcript-row-p row) (eq row.kind :finish-divider) (not (null row.outcome))))

;; Remove fractional seconds from timestamp hover; keep actual recorded milliseconds and truthful subsecond display.
(defun format-message-elapsed (elapsed-ms)
  "Human-readable whole-second display; the recorded duration remains unchanged."
  (check-type elapsed-ms (integer 0 *))
  (if (< 0 elapsed-ms 1000)
      "<1s"
      (bind (((days rest) (floor elapsed-ms 86400000)) ((hours rest) (floor rest 3600000))
             ((minutes rest) (floor rest 60000)))
        (format nil "~@[~dd ~]~@[~dh ~]~@[~dm ~]~ds" (and (plusp days) days)
                (and (plusp hours) hours) (and (plusp minutes) minutes) (floor rest 1000)))))

(defun message-timestamp-details (row)
  "Owning-turn elapsed/model/effort; user timestamps also show that turn's usage."
  (let* ((timing (gethash (transcript-row-turn-id row) *transcript-time-facts*))
         (elapsed (getf timing :elapsed))
         (model (or (json-value (getf timing :response-model) :text)
                    (json-value (getf timing :model) :text))))
    (append
     (list (cond (elapsed (format nil "Elapsed: ~a" (format-message-elapsed elapsed)))
                 ((and (getf timing :started) (not (getf timing :ended))) "Elapsed: turn still running")
                 (t "Elapsed: unavailable"))
           (format nil "Model: ~a" (if model (status-identity-label (getf timing :provider) model) "unavailable"))
           (format nil "Effort: ~a" (or (json-value (getf timing :effort) :text) "unavailable")))
     (when (equal "user" (transcript-row-role row))
       (list (format nil "Usage: ~a"
                     (let ((meter (usage-meter-text (getf timing :usage))))
                       (if meter (string-left-trim " ·" meter) "unavailable"))))))))

;; Add timestamp-only hit testing and an exact, wrapped turn-details overlay using the existing mouse/render seams.
(defun message-timestamp-span (model line)
  "=> (values ROW LEFT RIGHT): the message whose clock transcript LINE stands
at its end, and the display columns of LINE the clock takes (RIGHT
inclusive), or NIL -- a message's first-line clock only, never a quoted
timestamp in its body."
  (let ((row (and line (transcript-item-at-line model line)))
        (plains (chat-model-line-plain model))
        (refs (chat-model-line-rows model)))
    (when (and (message-timestamp-row-p row) (< line (length plains)) (< line (length refs)))
      (let ((first line))
        (loop while (and (plusp first) (eq row (aref refs (1- first)))) do (decf first))
        (loop while (and (< first (length plains)) (eq row (aref refs first))
                         (not (position-if-not #'blank-char-p (aref plains first)))) do (incf first))
        (when (= first line)
          ;; The clock is the first row's last words.
          (let* ((plain (string-right-trim " " (aref plains line)))
                 (clock (transcript-clock row))
                 (start (- (length plain) (length clock))))
            (when (and (> start 2) (string= clock plain :start2 start)
                       (string= "  " plain :start2 (- start 2) :end2 start))
              (let ((left (text-display-width (subseq plain 0 start))))
                (values row left (+ left (text-display-width clock) -1))))))))))

(defun message-timestamp-at-position (model x y)
  "Return the message owning its first-line timestamp cell, never a quoted timestamp in its body."
  (unless (or (chat-model-dialog model) (toast-at model x y))
    (multiple-value-bind (line column) (frame-transcript-position model x y)
      (multiple-value-bind (row left right) (message-timestamp-span model line)
        (when (and row (<= left column right)) row)))))

(defun without-message-clock (model frame-row text)
  "TEXT, frame row FRAME-ROW's plain text, with the clock a message's first
line ends on blanked, column for column: the shell's stamp, not the
message's words, so a copy leaves it behind."
  (multiple-value-bind (top bottom left) (transcript-lane model)
    (declare (ignore bottom))
    (bind (((_ _ _ top-pad) (transcript-band-metrics model))
           ((row from to) (message-timestamp-span
                           model (band-window-row-index (chat-model-band model) (- frame-row top top-pad)))))
      (if row
          (let ((start (plain-column-index text (+ left from)))
                (end (plain-column-index text (+ left to 1))))
            (concatenate 'string (subseq text 0 start)
                         (make-string (- end start) :initial-element #\Space)
                         (subseq text end)))
          text))))

(defun without-line-clock (model line text)
  "TEXT, transcript LINE's plain text, with the clock a message's first line
ends on blanked, column for column -- WITHOUT-MESSAGE-CLOCK for a line read
off the transcript rather than the frame."
  (multiple-value-bind (row from to) (message-timestamp-span model line)
    (if row
        (let ((start (plain-column-index text from))
              (end (plain-column-index text (1+ to))))
          (concatenate 'string (subseq text 0 start)
                       (make-string (- end start) :initial-element #\Space)
                       (subseq text end)))
        text)))

(defun message-timestamp-tooltip-rows (model row)
  "Wrapped tooltip fields using the shared display-width and line-break authority."
  (let* ((session (header-status-text model :detail :developer))
         (texts (append (message-timestamp-details row)
                        (when (and session (plusp (length session)))
                          (list (format nil "Session: ~a" session))))))
    (tooltip-rows texts (min (- (frame-width model) 2)
                             (+ 4 (reduce #'max texts :key #'text-display-width))))))

;; Move timestamp usage collection to the shared durable-event boundary for live and replay parity.
(defmethod apply-event :around (state (event nlk:event))
  (note-transcript-time event.log-position event.occurred-at event.kind event.turn-id)
  (let ((projected (call-next-method)))
    (when (equal "turn.usage" (nlk:event-kind event))
      (note-transcript-usage (nlk::event-turn-id event) (gethash "usage" projected)))
    projected))

(defun note-message-timestamp-hover (model msg)
  "Update live-only timestamp pointer ownership; return whether it changed."
  (let* ((was (gethash model *message-timestamp-hovers*))
         (x (nlt:mouse-event-x msg)) (y (nlt:mouse-event-y msg))
         (row (and (null (nlt:mouse-event-button msg)) (message-timestamp-at-position model x y)))
         (now (and row (list row x y))))
    (if now (setf (gethash model *message-timestamp-hovers*) now) (remhash model *message-timestamp-hovers*))
    (not (equal was now))))

(defun message-timestamp-overlay (model)
  "Timestamp detail painter, if the pointer still owns its timestamp cell."
  (let* ((hover (gethash model *message-timestamp-hovers*))
         (row (and hover (eq (first hover) (message-timestamp-at-position model (second hover) (third hover))) (first hover))))
    (when row
      (let* ((rows (message-timestamp-tooltip-rows model row))
             (width (text-display-width (plain-frame-row (first rows))))
             (x (max 0 (min (1- (second hover)) (- (frame-width model) width))))
             (below (third hover))
             (y (if (<= (+ below (length rows)) (frame-height model)) below
                    (max 0 (- below (length rows) 1)))))
        (lambda (buffer) (nlt:paint-rows buffer rows :x x :y y))))))
