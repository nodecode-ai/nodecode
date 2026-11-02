;;;; toast-test.lisp --- what the system says unasked, top right: the rules
;;;; the web page's toast() keeps, the borderless look, where the stack sits
;;;; and what the pointer and Esc do to it.

(in-package #:nodecode.test)

(nlk:access (toast nle::toast) (place nle::placed-toast))

(defun toast-texts (model)
  "The words of MODEL's toasts, newest first."
  (mapcar #'nle::toast-text (nle::chat-model-toasts model)))

(defun find-toast (model text)
  "MODEL's toast saying TEXT."
  (find text (nle::chat-model-toasts model) :key #'nle::toast-text :test #'string=))

(defun age-toast (toast seconds)
  "Make TOAST's five seconds begin SECONDS ago."
  (setf toast.born (- (get-internal-real-time) (floor (* seconds internal-time-units-per-second)))))

(defun ms-after (stamp ms)
  "The internal time MS milliseconds after STAMP."
  (+ stamp (floor (* ms internal-time-units-per-second) 1000)))

(defun toasted-frame (model)
  "MODEL's frame as the painter lays it out: its rows in cells, then the
view's overlay over them."
  (let* ((rows (nle::compose-frame model))
         (buffer (nlt:parse-styled-string rows (nle::frame-width model) (nle::frame-height model)))
         (overlay (nle::frame-overlay model)))
    (when overlay (funcall overlay buffer))
    buffer))

(defun buffer-text (buffer y &key (from 0) (to (nlt::screen-buffer-width buffer)))
  "The characters row Y of BUFFER paints from column FROM to TO."
  (with-output-to-string (out)
    (loop for x from from below to
          for cell = (nlt:screen-buffer-ref buffer x y)
          when (plusp (nlt:cell-width cell))
            do (write-char (nlt:cell-char cell) out))))

(defun top-count (model)
  "How many times the newest toast was said."
  (nle::toast-count (first (nle::chat-model-toasts model))))

(deftest notification-popups-are-hidden-by-default-with-notice-data-intact ()
  (dolist (config '("{}" "{\"tui\": {}}" "{\"tui\": {\"notification_popups\": false}}"))
    (with-temp-file (nle::*shared-config-path* :setf t :contents config)
      (with-frame-model (model 80 24 :session-id "quiet-notices")
        (nle:update model (nle::make-system-notice-msg :text "POPUP-HIDDEN-NEWS"))
        (nle:update model (nle::make-system-notice-msg :text "POPUP-HIDDEN-ERROR" :error-p t))
        (nle:update model (nle::make-system-notice-msg :text "COMPOSER-ANSWER" :answer t))
        (let ((buffer (toasted-frame model)))
          (is (null (nle::chat-model-toast-layout model)) "absent and explicit Off produce no popup layout")
          (is (null (nle::toasts-overlay model)) "no overlay paints over the transcript")
          (is (null (nle::toast-at model 50 5)) "hidden notices intercept no pointer cells")
          (is (not (nle::toasts-cover-rows-p model 3 8)) "hidden notices do not suppress images")
          (is (not (loop for y below 24 thereis (search "POPUP-HIDDEN" (buffer-text buffer y)))) "news and errors are both hidden")
          (is (loop for y below 24 thereis (search "COMPOSER-ANSWER" (buffer-text buffer y))) "direct command answers still paint")
          (is (equal '("POPUP-HIDDEN-ERROR" "POPUP-HIDDEN-NEWS") (toast-texts model)) "visibility does not discard notice data"))))))

(defmacro with-popup-display (&body body)
  `(with-temp-file (nle::*shared-config-path* :setf t :contents "{\"tui\": {\"notification_popups\": true}}")
     (let ((nle::*color-profile* :truecolor)) ,@body)))

(deftest toast-keeps-the-web-pages-rules (with-frame-model (model 80 24 :session-id "toast-rules"))
  (is (nle::toast model "gateway: up") "a toast goes up")
  (is (not (nle::toast model "   ")) "blank words are not a toast")
  (nle::toast model "worker: failed" :error-p t)
  (is (nle::toast model "gateway: up") "the same words again move the stack")
  (is (equal '("gateway: up" "worker: failed") (toast-texts model)) "back on top, as one")
  (is (= 2 (top-count model)) "counting up")
  (nle::toast model "discord: token refused" :key "discord" :error-p t)
  (nle::toast model "discord: connected" :key "discord")
  ;; A keyed notice replaces the toast under its key, and says what it says
  ;; once in this shell however often the board is handed over.
  (is (equal '("discord: connected" "gateway: up") (subseq (toast-texts model) 0 2)))
  (is (not (nle::toast model "discord: connected" :key "discord")))
  (is (= 1 (top-count model)) "without counting up")
  ;; A fourth takes down the oldest news, not the older error; with no news
  ;; up the oldest error leaves.
  (nle::toast model "cron: 2 jobs")
  (is (equal '("cron: 2 jobs" "discord: connected" "worker: failed") (toast-texts model)))
  (dolist (text '("a" "b" "c")) (nle::toast model text :error-p t))
  (is (equal '("c" "b" "a") (toast-texts model))))

(deftest toast-news-leaves-after-five-seconds-and-waits-under-the-pointer ()
  (with-frame-model (model 80 24 :session-id "toast-time")
    (nle::toast model "news")
    (nle::toast model "error" :error-p t)
    (nle::toast model "stays" :stays t)
    (is (not (nle::expire-toasts model)) "nothing leaves inside five seconds")
    (dolist (toast (nle::chat-model-toasts model)) (age-toast toast 6))
    (nle::hover-toast model (find-toast model "news"))
    (is (not (nle::expire-toasts model)) "news under the pointer waits")
    (nle::hover-toast model nil)
    (is (not (nle::expire-toasts model)) "leaving it starts its five seconds again")
    (age-toast (find-toast model "news") 6)
    (is (nle::expire-toasts model) "then it leaves")
    (is (equal '("stays" "error") (toast-texts model)) "an error and one that stays do not")))

(deftest toast-cuts-long-words-and-counts-repeats-after-them ()
  (let ((toast (nle::make-toast (format nil "~{~a~^ ~}" (loop repeat 40 collect "word")))))
    (setf toast.count 3)
    (multiple-value-bind (lines tail) (nle::toast-text-lines toast 20)
      (is (= 4 (length lines)) "four lines at most")
      (is (every (lambda (line) (<= (nle::text-display-width line) 20)) lines))
      (is (char= #\… (char (car (last lines)) (1- (length (car (last lines)))))) "cut with …")
      ;; The cut leaves the count room on the last line.
      (is (<= (+ (nle::text-display-width (car (last lines))) (length tail)) 20))
      (is (string= " ×3" tail))))
  (let ((toast (nle::make-toast "twelve chars")))
    (setf toast.count 12)
    (multiple-value-bind (lines tail) (nle::toast-text-lines toast 14)
      (is (equal '("twelve chars" "") lines) "a count with no room takes a line of its own")
      (is (string= "×12" tail)))))

(deftest toast-foot-stands-whole-under-a-cut ()
  ;; A toast's foot -- where the rest of it is -- is its last line, whatever
  ;; the cut takes of the words above it.
  (let ((lines (nle::toast-text-lines
                (nle::make-toast (format nil "~{~a~^ ~}" (loop repeat 40 collect "word"))
                                 :foot "the rest is in the transcript")
                30)))
    (is (= nle::*toast-text-lines* (length lines)))
    (is (equal "the rest is in the transcript" (car (last lines))))
    (is (search "…" (car (last (butlast lines)))) lines)))

(deftest toast-paints-top-right-without-a-border (with-popup-display)
  (with-frame-model (model 80 24 :session-id "toast-look")
    (nle::toast model "discord: the bot token was refused (401)" :error-p t)
    (let* ((bare (nlt:parse-styled-string (nle::compose-frame model) 80 24))
           (buffer (toasted-frame model))
           (place (first (nle::chat-model-toast-layout model)))
           (canvas (nlt:cell-bg (nlt:screen-buffer-ref bare 0 4))))
      (flet ((bg (buffer x y) (nlt:cell-bg (nlt:screen-buffer-ref buffer x y)))
             (row (y) (buffer-text buffer y :from 34 :to 78)))
        ;; 44 columns from row 3, ending where the transcript's text does, one
        ;; blank column short of the scroll rail in column 79.
        (is (and (= 34 place.x) (= 3 place.y) (= 44 place.width)))
        (is (string= (make-string 44 :initial-element #\▄) (row 3)) "a half-row edge above")
        (is (equal (bg bare 40 3) (bg buffer 40 3)) "its other half is the cell behind it")
        (is (search "discord: the bot token" (row 4)) "the words")
        (is (char= #\× (nlt:cell-char (nlt:screen-buffer-ref buffer 75 4))) "the × on the first row")
        (is (string= (make-string 44 :initial-element #\▀) (row 6)) "a half-row edge below")
        (is (equal canvas (bg buffer 33 4)) "a blank canvas column to its left")
        ;; The blank column before the rail, and the rail, are the frame's own.
        (is (loop for y from 3 to 6
                  always (loop for x in '(78 79)
                               always (nlt::cell-equal (nlt:screen-buffer-ref bare x y)
                                                       (nlt:screen-buffer-ref buffer x y)))))
        ;; And no line anywhere around it.
        (is (not (find-if (lambda (ch) (find ch "╭╮╰╯─│┌┐└┘"))
                          (format nil "~{~a~}" (loop for y from 3 to 6 collect (row y))))))
        (nle::hover-toast model place.toast)
        (is (not (equal (bg buffer 40 4) (bg (toasted-frame model) 40 4))) "the pointer lifts it")))))

(deftest toast-stack-fits-a-narrow-window-and-stops-above-the-composer (with-popup-display)
  (with-frame-model (model 58 20 :session-id "toast-narrow")
    (dolist (text '("one" "two" "three")) (nle::toast model text))
    (nle::compose-frame model)
    (let ((layout (nle::chat-model-toast-layout model)))
      (is (= 2 (length layout)) "two up below 60 columns")
      ;; From the gutter to where the text ends, one blank column short of the
      ;; scroll rail in column 57.
      (is (every (lambda (place) (and (= 2 place.x) (= 54 place.width))) layout))
      (is (equal '("three" "two") (mapcar (lambda (place) (nle::toast-text place.toast)) layout)))))
  (with-frame-model (model 80 11 :session-id "toast-floor")
    (dotimes (i 3) (nle::toast model (format nil "notice ~d" i)))
    (let* ((rows (nle::compose-frame model))
           (composer (position-if (lambda (row) (search "Connecting" row)) rows))
           (layout (nle::chat-model-toast-layout model)))
      (is composer "the composer says the shell is connecting")
      (is (< (length layout) 3) "a short window holds fewer")
      ;; None reaches the composer's rows.
      (is (every (lambda (place) (< (+ place.y (length place.rows)) composer)) layout)))))

(deftest toast-closes-on-its-x-and-on-esc-and-holds-under-the-pointer (with-popup-display)
  (with-frame-model (model 80 24 :session-id "toast-input")
    (nle::toast model "older" :error-p t)
    (nle::toast model "newer" :error-p t)
    (nle::compose-frame model)
    ;; Mouse cells are 1-based, as the terminal reports them: the newest
    ;; toast's first row of words is screen row 4, its × column 75.
    (flet ((move (x y) (nth-value 2 (mouse-update model #'nlt:make-mouse-motion-msg (1+ x) (1+ y))))
           (click (x y) (mouse-update model #'nlt:make-mouse-click-msg (1+ x) (1+ y) :button :left)))
      (let ((place (first (nle::chat-model-toast-layout model))))
        (is (not (move 50 4)) "the pointer coming onto a toast repaints")
        (is (eq place.toast (nle::chat-model-toast-hover model)) "and holds it")
        (is (move 51 4) "moving on it moves nothing")
        (is (search "×" (buffer-text (toasted-frame model) 4 :from 34 :to 78)))
        (is (not (move 5 12)) "leaving it repaints")
        (is (null (nle::chat-model-toast-hover model)) "and lets it go")
        (move 50 2)
        (is (null (nle::chat-model-toast-hover model)) "the row above the stack is no toast")
        (click 50 4)
        (is (equal '("newer" "older") (toast-texts model)) "a press on the words takes nothing down")
        (is (null (nle::chat-model-selection-anchor model)) "and arms nothing under it")
        (click 78 4)
        (is (equal '("newer" "older") (toast-texts model)) "the blank before the rail is no ×")
        (click 75 4)
        (is (equal '("older") (toast-texts model)) "a press on the × takes it down")))
    (nle::toast model "newest" :error-p t)
    (press model :escape)
    (is (equal '("older") (toast-texts model)) "Esc with nothing else to do takes the newest down")
    (type-string model "draft" :escape)
    (is (equal '("older") (toast-texts model)) "not while the composer holds text")))

(deftest toast-countdown-drains-and-stands-still-under-the-pointer ()
  (let* ((toast (nle::make-toast "news"))
         (born toast.born))
    (is-table (in want) (= want (nle::toast-strip toast 44 in))
      (born 44 "the whole edge as it goes up") ((ms-after born 2500) 22 "half at 2.5 s")
      ((ms-after born 2520) 175/8 "and it moves in eighths of a column")
      ((ms-after born 4990) 1/8 "an eighth of a column at the end") ((ms-after born 5000) 0))
    (setf toast.held 3000)
    (is (= 141/8 (nle::toast-strip toast 44 (ms-after born 9000))) "held, it stands where it was caught")
    (is (null (nle::toast-strip (nle::make-toast "boom" :error-p t) 44 born)) "one that stays has none")))

(deftest toast-countdown-paints-on-the-bottom-edge (with-popup-display)
  (with-frame-model (model 80 24 :session-id "toast-strip")
    (nle::toast model "worker: failed" :error-p t)
    (nle::toast model "cron: 2 jobs")
    (age-toast (find-toast model "cron: 2 jobs") 2.5)
    ;; The news toast on rows 3-6 over columns 34-77 -- its edge, its words, a
    ;; row of ground, the edge its strip is on -- and the error on rows 7-9.
    (let ((buffer (toasted-frame model)))
      (flet ((fg (x y) (nlt:cell-fg (nlt:screen-buffer-ref buffer x y)))
             (bg (x y) (nlt:cell-bg (nlt:screen-buffer-ref buffer x y))))
        (let ((lit (loop for x from 34 below 78 count (equal (fg x 6) (fg 34 6)))))
          (is (loop for x from 34 below 78 always (equal (bg x 5) (bg 34 4))) "a row of ground under the words")
          (is (not (equal (fg 34 6) (fg 77 6))) "the strip is a shade of its own")
          (is (equal (fg 77 6) (fg 77 3)) "the rest of the edge is the ground")
          (is (<= 21 lit 23) "and half the edge is left at 2.5 s")
          ;; Between the two, at most one cell fades from the one to the other.
          (is (<= (loop for x from 34 below 78 count (not (member (fg x 6) (list (fg 34 6) (fg 77 6)) :test #'equal))) 1)))
        (is (char= #\▄ (nlt:cell-char (nlt:screen-buffer-ref buffer 40 7))) "the error follows right under it")
        (is (loop for x from 34 below 78 always (equal (fg x 9) (fg 34 9))) "an error keeps a plain edge")))))

(deftest toast-countdown-steps-cost-a-frame-and-a-held-one-costs-none (with-popup-display)
  (with-frame-model (model 80 24 :session-id "toast-tick")
    (nle::toast model "cron: 2 jobs")
    (let ((toast (find-toast model "cron: 2 jobs")))
      (flet ((quiet-tick-p () (nth-value 2 (nle:update model (nlt:make-tick-msg)))))
        (nle::compose-frame model)
        (age-toast toast 1)
        (is (not (quiet-tick-p)) "a strip that lost columns since the last frame paints")
        (nle::compose-frame model)
        (nle::hover-toast model toast)
        (nle::compose-frame model)
        (age-toast toast 2)
        (is (quiet-tick-p) "held under the pointer, it stands still and costs no frame")
        (nle::hover-toast model nil)
        (is (not (quiet-tick-p)) "let go, it is whole again")
        (is (= 44 (nle::toast-strip toast 44 (get-internal-real-time))))))))
