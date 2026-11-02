;;;; select-mode-test.lisp --- keyboard selection over the transcript.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. Ctrl+Y opens a cursor on the first words of the last line the
;;;;      window shows; the hardware cursor parks there and the composer
;;;;      names the mode's keys.
;;;;   2. h l, w b e, 0 $, j k, g G move it by characters, words, logical
;;;;      lines, rows and to the transcript's ends, across lines; a wrapped
;;;;      line's rows are one logical line.
;;;;   3. The page keys and Ctrl+U/Ctrl+D move it by pages, and the window
;;;;      scrolls to keep it shown.
;;;;   4. v marks what the keys cross, o swaps the mark's ends, y copies the
;;;;      words the mark covers and leaves, the mark kept.
;;;;   5. V marks whole logical lines, a wrapped paragraph whole.
;;;;   6. While the mode runs no key reaches the composer: no letter types,
;;;;      Left never leaves the session, Enter never submits; Esc takes a
;;;;      mark away, then leaves.
;;;;   7. A drag's mark standing when the mode opens is the mode's own, and
;;;;      a press closes the mode.
;;;;   8. The quit chord still quits; Ctrl+D pages instead.

(in-package #:nodecode.test)

(defun select-paragraph ()
  "A paragraph long enough to wrap over several rows at 80 columns."
  (format nil "~{word~2,'0d~^ ~}" (loop for i from 1 to 40 collect i)))

(defun select-open (model)
  "Press Ctrl+Y on MODEL: open the keyboard selection."
  (press model #\y :mod nlt:+mod-ctrl+))

(defun select-at (model)
  "The words from the keyboard selection's cursor to the end of its line, the
row's padding left off."
  (multiple-value-bind (line index)
      (nle::select-cursor-position model (nle::transcript-select model))
    (string-right-trim " " (subseq (nle::select-line-text model line) index))))

(defun select-line (model)
  "The transcript line the keyboard selection's cursor stands on."
  (values (nle::select-cursor-position model (nle::transcript-select model))))

(defun osc52-text (io)
  "The text the terminal-io message IO hands the terminal's clipboard (OSC 52)."
  (let* ((written (with-output-to-string (out) (funcall (nlt:terminal-io-msg-thunk io) out)))
         (start (+ (search "]52;c;" written) 6))
         (end (position-if (lambda (char) (member char '(#\Bel #\Esc))) written :start start)))
    (cl-base64:base64-string-to-string (subseq written start end))))

(deftest ctrl-y-opens-a-keyboard-selection-on-the-last-shown-words ()
  ;; The steal from Codex's select mode: the transcript's words could be
  ;; marked only by a drag. Ctrl+Y sets a cursor on them, and the hardware
  ;; cursor stands on it instead of the composer's caret.
  (with-frame-model (model 80 24 :session-id "select-open"
                     :paint ((nle::ingest-message-fact
                              "assistant" (format nil "alpha beta gamma~%~%delta epsilon"))))
    (is (null (nle::transcript-select model)))
    (nle::show-status model "Completed")
    (select-open model)
    (is (nle::transcript-select model) "Ctrl+Y opens the mode")
    (is (null (nle::status-said model)) "a passing notice gives the mode's line its place")
    (is (eql 0 (search "delta" (select-at model))) "the cursor stands on the first words of the last line shown")
    (let* ((rows (nle::compose-frame model))
           (cell (nle::chat-model-parked-cursor model)))
      (is (and cell (eql (car cell) (search "delta" (nle::plain-frame-row (nth (cdr cell) rows))))) "the hardware cursor parks on that cell, not on the composer")
      (is (rows-carry-p "Selecting" rows) "the composer says the mode")
      (is (rows-carry-p " mark " rows) "and names its keys"))))

(deftest select-mode-moves-by-characters-words-lines-and-ends ()
  ;; w b e and h l cross lines; 0 and $ take the wrapped paragraph's ends,
  ;; whichever of its rows the cursor is on; g and G the transcript's.
  (with-frame-model (model 80 24 :session-id "select-motions"
                     :paint ((nle::ingest-message-fact
                              "assistant" (format nil "one two three~%~%~a~%~%last words here"
                                                  (select-paragraph)))))
    (select-open model)
    (is (eql 0 (search "last words" (select-at model))))
    (press model #\b)
    (is (equal "word40" (select-at model)) "b goes back to the word on the line above")
    (let ((row (select-line model)))
      (press model #\0)
      (is (eql 0 (search "word01" (select-at model))) "0 is the wrapped line's start")
      (is (< (select-line model) row) "on the paragraph's first row")
      (press model #\$)
      (is (equal "0" (select-at model)) "$ is its last character")
      (is (= row (select-line model)) "on its last row"))
    (press model #\0 #\e)
    (is (eql 0 (search "1 word02" (select-at model))) "e is the word's last character")
    (press model #\w)
    (is (eql 0 (search "word02" (select-at model))) "w the next word's first")
    (let ((row (select-line model))
          (column (third (nle::select-state-cursor (nle::transcript-select model)))))
      (press model #\j)
      (is (= (1+ row) (select-line model)) "j is a row down")
      (is (= column (third (nle::select-state-cursor (nle::transcript-select model)))) "at the same column")
      (press model #\k)
      (is (= row (select-line model)) "k is a row back up")
      (loop repeat 12 until (string= "" (select-at model)) do (press model #\j))
      (is (string= "" (select-at model)) "j reaches the blank under the paragraph")
      (is (= column (third (nle::select-state-cursor (nle::transcript-select model)))) "which keeps the column, never the margin's"))
    (press model #\g)
    (is (eql 0 (search "one two" (select-at model))) "g is the transcript's first words")
    (press model #\h)
    (is (eql 0 (search "one two" (select-at model))) "h stops at the first")
    (press model #\$ #\l)
    (is (eql 0 (search "word01" (select-at model))) "l goes on to the next line's words")
    (press model #\G)
    (is (equal "e" (select-at model)) "G is the transcript's last character")
    (press model #\h)
    (is (equal "re" (select-at model)) "h is one character back")
    (press model :right :left :mod nlt:+mod-ctrl+)
    (is (equal "here" (select-at model)) "Ctrl+Left goes back by a word")))

(deftest select-mode-pages-and-keeps-its-cursor-in-the-window ()
  (with-frame-model (model 80 24 :session-id "select-pages"
                     :paint ((nle::ingest-message-fact
                              "assistant"
                              (format nil "~{line ~2,'0d~^~%~%~}" (loop for i from 1 to 40 collect i)))))
    (let ((window model.band))
      (select-open model)
      (is (eql 0 (search "line 40" (select-at model))))
      (let ((offset (nle::band-window-offset window))
            (line (select-line model)))
        (press model :page-up)
        (is (< (select-line model) line) "PgUp moves the cursor up")
        (is (< (nle::band-window-offset window) offset) "and the window with it")
        (is (nle::select-cursor-cell model) "which shows the cursor"))
      (press model #\g)
      (is (eql 0 (search "line 01" (select-at model))) "g reaches the first line")
      (is (nle::select-cursor-cell model))
      (press model #\G)
      (is (search "line 40" (nle::select-line-text model (select-line model))))
      (is (nle::band-window-at-bottom-p window) "G brings the window to the bottom")
      (is (nle::select-cursor-cell model))
      (let ((line (select-line model)))
        (press model #\u :mod nlt:+mod-ctrl+)
        (is (< (select-line model) line) "Ctrl+U goes up half a page")
        (press model #\d :mod nlt:+mod-ctrl+)
        (is (= line (select-line model)) "Ctrl+D comes back down")))))

(deftest select-mode-v-marks-o-swaps-and-y-copies ()
  ;; The mark is the drag's own: the copy reads it off the transcript's lines
  ;; and hands it to the clipboard writer, here the terminal's OSC 52.
  (with-frame-model (model 80 24 :session-id "select-copy"
                     :paint ((nle::ingest-message-fact "assistant" "alpha beta gamma delta")))
    (let ((program (nlt:make-program model)))
      (setf (nlt:program-running program) t
            (nle::chat-model-program model) program)
      (with-saved-globals ((nlt:*clipboard-use-command* nil))
        (select-open model)
        (press model #\w #\v)
        (is (equal "b" (nle::frame-selection-text model)) "v marks the cursor's character")
        (press model #\e #\w #\e)
        (is (equal "beta gamma" (nle::frame-selection-text model)) "the mark follows the cursor")
        (press model #\o #\b)
        (is (equal "alpha beta gamma" (nle::frame-selection-text model)) "after o the keys move the mark's other end")
        (press model #\y)
        (is (null (nle::transcript-select model)) "y leaves the mode")
        (is (equal "alpha beta gamma" (nle::frame-selection-text model)) "its mark stays washed")
        (let ((io (sb-concurrency:receive-message (nlt::program-mailbox program) :timeout 5)))
          (is (nlt:terminal-io-msg-p io))
          (is (equal "alpha beta gamma" (osc52-text io)) "and the clipboard took its words"))))))

(deftest select-mode-big-v-marks-whole-logical-lines ()
  (with-frame-model (model 80 24 :session-id "select-lines"
                     :paint ((nle::ingest-message-fact
                              "assistant" (format nil "first line~%~%~a~%~%after" (select-paragraph)))))
    (let* ((plains model.line-plain)
           (start (row-index-of "word01" plains :required t))
           (end (row-index-of "word40" plains :required t)))
      (is (< start end) "the paragraph wraps")
      (is (not (nle::plain-row-continues (aref plains start))) "its first row opens it")
      (is (loop for row from (1+ start) to end always (nle::plain-row-continues (aref plains row))) "every later row goes on it"))
    (select-open model)
    (press model #\k #\k)
    (is (search "word40" (nle::select-line-text model (select-line model))) "on the paragraph's last row")
    (press model #\V)
    (let ((first (first (nle::mark-frame-cells model model.selection)))
          (opening (nle::select-line-text model (row-index-of "word01" model.line-plain :required t))))
      (is (= (second first) (+ 2 (search "word01" opening))) "its wash opens on the first word, never in the margin"))
    (let ((text (nle::frame-selection-text model)))
      (is (eql 0 (search "word01" text)) "V marks from the paragraph's first row")
      (is (search "word40" text) "to its last")
      (is (not (search "after" text)))
      (is (not (search "first line" text))))
    (press model #\j #\j)
    (is (search "after" (nle::frame-selection-text model)) "j carries the mark a line down")
    (press model #\o #\g)
    (let ((text (nle::frame-selection-text model)))
      (is (eql 0 (search "first line" text)) "o then g carries its other end to the top")
      (is (search "after" text) "the far end stays"))
    (press model #\v)
    (is (eql 0 (search "first line" (nle::frame-selection-text model))) "v keeps the mark, by characters")
    (is (not (search "after" (nle::frame-selection-text model))))))

(deftest select-mode-keeps-every-key-from-the-composer ()
  (with-frame-model (model 80 24 :session-id "select-keys"
                     :paint ((nle::ingest-message-fact "assistant" "alpha beta")))
    ;; Left on an idle composer leaves the session; in the mode it moves.
    (select-open model)
    (press model :left)
    (is (equal "select-keys" model.session-id) "Left never leaves the session")
    (press model :escape)
    (is (null (nle::transcript-select model)) "Esc with nothing marked leaves")
    (type-string model "draft")
    (select-open model)
    (press model #\x #\z #\Q #\Space)
    (is (equal "draft" (composer-value model)) "no letter types into the composer")
    (is (nle::chat-model-selection model) "Space marks")
    (press model :escape)
    (is (null (nle::chat-model-selection model)) "Esc takes the mark away first")
    (is (nle::transcript-select model) "and the mode stays")
    (press model :enter)
    (is (null (nle::transcript-select model)) "Enter with nothing marked leaves")
    (is (equal "draft" (composer-value model)) "and submits nothing")
    (press model #\x)
    (is (equal "draftx" (composer-value model)) "out of the mode, keys type again")))

(deftest select-mode-takes-up-a-drag-mark-and-a-press-ends-it ()
  (with-frame-model (model 80 24 :session-id "select-adopt"
                     :paint ((nle::ingest-message-fact "assistant" "alpha beta gamma delta")))
    (let* ((y (mouse-y-for-text model "alpha"))
           (text (nle::select-line-text model (row-index-of "alpha" model.line-plain :required t)))
           (x0 (+ 3 (search "beta" text)))
           (x1 (+ 3 (search "gamma" text) 4)))
      (mouse-update model #'nlt:make-mouse-click-msg x0 y :button :left)
      (mouse-update model #'nlt:make-mouse-motion-msg x1 y :button :left)
      (mouse-update model #'nlt:make-mouse-release-msg x1 y :button :left)
      (is (equal "beta gamma" (nle::frame-selection-text model)))
      (select-open model)
      (is (equal "a delta" (select-at model)) "the cursor stands on the mark's last character")
      (press model #\w #\e)
      (is (equal "beta gamma delta" (nle::frame-selection-text model)) "and moves the drag's mark")
      (click-at model 3 y)
      (is (null (nle::transcript-select model)) "a press closes the mode"))))

(deftest select-mode-lets-the-quit-chord-through ()
  (with-frame-model (model 80 24 :session-id "select-quit"
                     :paint ((nle::ingest-message-fact "assistant" "alpha beta")))
    (select-open model)
    (is (null (nth-value 1 (nle:update model (nlt:make-key-press-msg :code #\d :mod nlt:+mod-ctrl+)))) "Ctrl+D pages, never quits")
    (is (nth-value 1 (nle:update model (nlt:make-key-press-msg :code #\q :mod nlt:+mod-ctrl+))) "the quit chord still quits")))
