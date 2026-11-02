;;;; editor-test.lisp --- the composer's editing engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from cl-tuition's tests/test-textarea.lisp (Copyright (C) 2025
;;;; Anthony Green, MIT) for the operations the composer keeps, plus the
;;;; engine's own contract: wrap through BREAK-FRAGMENTS, measure through
;;;; TEXT-DISPLAY-WIDTH, a height computed from the content, a caret that
;;;; walks visual rows, and no silent truncation of a long insert.

(in-package #:nodecode.test)

(nlk:access (ed nle::editor))

(defun ed-make (value &rest args &aux (ed (apply #'nle::make-editor args)))
  "An editor holding VALUE with the caret homed at (0 0)."
  (nle::editor-insert ed value)
  (setf ed.row 0
        ed.col 0)
  ed)

(defun ed-cursor (ed)
  (list ed.row ed.col))

(defun ed-set-cursor (ed row col)
  (setf ed.row row
        ed.col col)
  ed)

(defun ed-key (ed code &optional (mod 0))
  "Feed one key press; => whether the text changed."
  (nle::editor-handle-key ed (nlt:make-key-press-msg :code code :mod mod)))

(defun ed-position (ed)
  (multiple-value-list (nle::editor-cursor-screen-position ed)))

(defmacro ed-walk (editor &body steps)
  "Drive EDITOR, bound as ED, through STEPS in order."
  ;; A step is (:at ROW COL) to home the caret; (ACTION EXPECTED WHY) to run
  ;; ACTION — a (KEY [MOD]) press, or any form over ED — and assert EXPECTED
  ;; under WHY, a (ROW COL) caret or the buffer's value; any other form runs
  ;; as it is.
  `(let ((ed ,editor))
     ,@(loop for step in steps
             collect (cond ((eq (first step) :at)
                            `(ed-set-cursor ed ,@(rest step)))
                           ((consp (first step))
                            (destructuring-bind (action expected why) step
                              `(progn
                                 ,(if (or (keywordp (first action)) (characterp (first action)))
                                      `(ed-key ed ,@action)
                                      action)
                                 ,(if (and (consp expected) (integerp (first expected)))
                                      `(is (equal ',expected (ed-cursor ed)) ,why)
                                      `(is (equal ,expected (nle::editor-value ed)) ,why)))))
                           (t step)))))

;;; --- the buffer ---------------------------------------------------------

(deftest editor-starts-empty (let ((ed (nle::make-editor))))
  (is-shape ed (nle::editor-value "" "an empty editor has an empty value")
    (nle::editor-line-count = 1 "one empty line") (nle::editor-height = 1 "one row")
    (ed-cursor '(0 0) "the caret is home")))

(deftest editor-insert-advances-the-caret (ed-walk (nle::make-editor))
  ((nle::editor-insert ed "abc") "abc" "the text lands")
  (is (equal '(0 3) (ed-cursor ed)) "the caret follows the insert")
  (:at 0 1)
  ((nle::editor-insert ed "X") "aXbc" "an insert lands at the caret")
  (is (equal '(0 2) (ed-cursor ed)) "and moves it past the text"))

(deftest editor-insert-splits-lines-on-newlines (ed-walk (nle::make-editor))
  (nle::editor-insert ed (format nil "a~%b~%c"))
  (is (= 3 (nle::editor-line-count ed)) "three logical lines")
  (is (equal '(2 1) (ed-cursor ed)) "the caret ends after the last part")
  (:at 0 1)
  ((nle::editor-insert ed (format nil "1~%2")) (format nil "a1~%2~%b~%c")
   "a mid-buffer multi-line insert splits the current line")
  (is (equal '(1 1) (ed-cursor ed)) "the caret ends after the last part"))

(deftest editor-reset-empties-the-buffer (ed-walk (ed-make (format nil "one~%two")))
  ((nle::editor-reset ed) "" "reset empties the text")
  (is (equal '(0 0) (ed-cursor ed)) "and homes the caret")
  (is (= 1 (nle::editor-height ed)) "back to one row"))

(deftest editor-value-is-memoized-until-an-edit (let ((ed (ed-make "abc"))))
  (let ((first (nle::editor-value ed)))
    (ed-key ed :right)
    (is (eq first (nle::editor-value ed)))
    (ed-key ed :end nlt:+mod-ctrl+)
    (nle::editor-insert ed "d")
    (is (equal "abcd" (nle::editor-value ed)))))

(deftest editor-placeholder-round-trips (let ((ed (nle::make-editor :placeholder "Ready"))))
  (is (equal "Ready" ed.placeholder) "the placeholder is kept"))

(deftest editor-keeps-a-long-insert-whole ()
  ;; The textarea it replaces silently truncated an insert past its
  ;; 1000-line default, so a long paste lost its tail without a word.
  (let ((ed (nle::make-editor))
        (text (format nil "~{line ~d~^~%~}"
                      (loop for i from 1 to 2000 collect i))))
    (nle::editor-insert ed text)
    (is (= 2000 (nle::editor-line-count ed)) "every line is kept")
    (is (equal text (nle::editor-value ed)) "the value is the whole insert")))

(deftest editor-char-limit-trims-then-blocks (ed-walk (nle::make-editor :char-limit 5))
  (nle::editor-insert ed "abc")
  ((nle::editor-insert ed "defgh") "abcde" "the insert is cut to the budget")
  ((nle::editor-insert ed "xyz") "abcde" "a full buffer takes nothing"))

;;; --- document and line motion ------------------------------------------

(deftest editor-left-and-right-wrap-across-lines (ed-walk (ed-make (format nil "ab~%cd")))
  (:at 0 2)
  ((:right) (1 0) "Right at a line end reaches the next line")
  ((:left) (0 2) "Left at a line start returns to the end above")
  (:at 0 0)
  ((:left) (0 0) "Left at the very start holds"))

(deftest editor-home-and-end-chords (ed-walk (ed-make "alpha beta"))
  (:at 0 4)
  ((#\e nlt:+mod-ctrl+) (0 10) "Ctrl+E reaches the line end")
  ((#\a nlt:+mod-ctrl+) (0 0) "Ctrl+A reaches the line start")
  ((:end) (0 10) "End reaches the line end")
  ((:home) (0 0) "Home reaches the line start")
  ((#\f nlt:+mod-ctrl+) (0 1) "Ctrl+F steps right")
  ((#\b nlt:+mod-ctrl+) (0 0) "Ctrl+B steps left"))

(deftest editor-ctrl-home-and-end-reach-the-document-ends ()
  (ed-walk (ed-make (format nil "aaa~%bbb"))
    ((:end nlt:+mod-ctrl+) (1 3) "Ctrl+End reaches the document end")
    ((:home nlt:+mod-ctrl+) (0 0) "Ctrl+Home reaches the document start")))

;;; --- words ---------------------------------------------------------------

(deftest editor-word-forward-walks (ed-walk (ed-make "alpha beta gamma"))
  ((nle::editor-word ed 1) (0 6) "past alpha, to beta")
  ((nle::editor-word ed 1) (0 11) "to gamma")
  ((nle::editor-word ed 1) (0 16) "to the line end"))

(deftest editor-word-backward-walks (ed-walk (ed-make "alpha beta gamma"))
  (:at 0 16)
  ((nle::editor-word ed -1) (0 11) "to the start of gamma")
  ((nle::editor-word ed -1) (0 6) "to the start of beta")
  ((nle::editor-word ed -1) (0 0) "to the start of alpha"))

(deftest editor-word-chords-move-a-word (ed-walk (ed-make "alpha beta"))
  ((#\f nlt:+mod-alt+) (0 6) "Alt+F moves a word forward")
  ((#\b nlt:+mod-alt+) (0 0) "Alt+B moves a word back")
  ((:right nlt:+mod-ctrl+) (0 6) "Ctrl+Right moves a word forward")
  ((:right nlt:+mod-ctrl+) (0 10) "and then to the line end")
  ((:left nlt:+mod-ctrl+) (0 6) "Ctrl+Left moves a word back")
  ((:left nlt:+mod-alt+) (0 0) "Alt+Left is the macOS spelling of the same"))

(deftest editor-deletes-words ()
  (ed-walk (ed-make "alpha beta")
    ((nle::editor-word ed 1 :delete t) "beta" "forward deletes through the word and its space")
    (is (equal '(0 0) (ed-cursor ed)) "the caret holds"))
  (ed-walk (ed-make "alpha beta")
    (:at 0 6)
    ((nle::editor-word ed -1 :delete t) "beta" "backward deletes the word and its space")
    (is (equal '(0 0) (ed-cursor ed)) "the caret lands at the word's start")))

(deftest editor-word-deletion-chords ()
  (ed-walk (ed-make "foo bar baz")
    (:at 0 11)
    (is (ed-key ed #\w nlt:+mod-ctrl+) "Ctrl+W changes the text")
    (is (equal "foo bar " (nle::editor-value ed)) "Ctrl+W deletes the word before the caret")
    ((:backspace nlt:+mod-ctrl+) "foo " "Ctrl+Backspace deletes the word before the caret")
    ((:backspace nlt:+mod-alt+) "" "Alt+Backspace too"))
  (ed-walk (ed-make "alpha beta")
    ((:delete nlt:+mod-ctrl+) "beta" "Ctrl+Delete deletes the word after the caret")
    ((#\d nlt:+mod-alt+) "" "Alt+D too")))

(deftest editor-transposes-characters ()
  (ed-walk (ed-make "abc")
    (:at 0 3)
    ((nle::editor-transpose-chars ed) "acb" "at the end the last two swap"))
  (ed-walk (ed-make "abc")
    (:at 0 1)
    ((nle::editor-transpose-chars ed) "bac" "mid-line the pair around the caret swaps")
    (is (equal '(0 2) (ed-cursor ed)) "and the caret advances"))
  (ed-walk (ed-make "abc")
    ((nle::editor-transpose-chars ed) "abc" "at the start nothing swaps"))
  (ed-walk (ed-make "abc")
    (:at 0 3)
    ((#\t nlt:+mod-ctrl+) "acb" "Ctrl+T dispatches the transpose")))

(deftest editor-changes-word-case ()
  (ed-walk (ed-make "hello world")
    ((nle::editor-case-word ed #'string-capitalize) "Hello world" "capitalize")
    (is (equal '(0 5) (ed-cursor ed)) "the caret moves past the word")
    ((nle::editor-case-word ed #'string-upcase) "Hello WORLD" "uppercase the next word"))
  (ed-walk (ed-make "HELLO WORLD")
    ((nle::editor-case-word ed #'string-downcase) "hello WORLD" "lowercase"))
  (ed-walk (ed-make "hello")
    ((#\u nlt:+mod-alt+) "HELLO" "Alt+U dispatches uppercase")
    (:at 0 0)
    ((#\l nlt:+mod-alt+) "hello" "Alt+L dispatches lowercase")
    (:at 0 0)
    ((#\c nlt:+mod-alt+) "Hello" "Alt+C dispatches capitalize")))

;;; --- deletion -----------------------------------------------------------

(deftest editor-backspace-and-delete-merge-lines ()
  (ed-walk (ed-make (format nil "ab~%cd"))
    (:at 1 0)
    ((:backspace) "abcd" "Backspace at a line start joins it onto the line above")
    (is (equal '(0 2) (ed-cursor ed)) "the caret sits at the join"))
  (ed-walk (ed-make (format nil "ab~%cd"))
    (:at 0 2)
    ((:delete) "abcd" "Delete at a line end joins the next line on")
    (is (equal '(0 2) (ed-cursor ed)) "the caret holds")))

(deftest editor-kills-to-the-line-ends (ed-walk (ed-make "alpha beta"))
  (:at 0 5)
  ((#\k nlt:+mod-ctrl+) "alpha" "Ctrl+K kills to the line end")
  (:at 0 2)
  ((#\u nlt:+mod-ctrl+) "pha" "Ctrl+U kills to the line start")
  (is (equal '(0 0) (ed-cursor ed)) "and homes the caret"))

(deftest editor-ctrl-d-deletes-at-the-caret (ed-walk (ed-make "abc"))
  ((#\d nlt:+mod-ctrl+) "bc" "Ctrl+D deletes the character at the caret"))

;;; --- newlines and text --------------------------------------------------

(deftest editor-newline-chords-split-the-line (dolist (chord (list (list :enter 0)
                                                                   (list #\m nlt:+mod-ctrl+)
                                                                   (list #\j nlt:+mod-ctrl+))))
  (let ((ed (ed-make "ab")))
    (ed-set-cursor ed 0 1)
    (is (ed-key ed (first chord) (second chord)) "the chord changes the text")
    (is (equal (format nil "a~%b") (nle::editor-value ed)))
    (is (equal '(1 0) (ed-cursor ed)) "the caret starts the new line")
    (is (= 2 (nle::editor-height ed)) "the height follows the content")))

(deftest editor-plain-characters-insert-and-chords-do-not (let ((ed (nle::make-editor))))
  (is (ed-key ed #\a) "a plain character changes the text")
  (is (equal "a" (nle::editor-value ed)) "and lands in the buffer")
  (is (null (ed-key ed #\g nlt:+mod-ctrl+)) "an unbound ctrl chord is a gesture, not text")
  (is (null (ed-key ed #\g nlt:+mod-alt+)) "an unbound alt chord too")
  (is (equal "a" (nle::editor-value ed)) "neither typed its base letter")
  (is (null (ed-key ed :left)) "motion alone reports no text change")
  (is (null (ed-key ed :f5)) "an unknown key is ignored"))

;;; --- visual rows: wrap, height, window, caret cell ----------------------

(deftest editor-wraps-at-its-width ()
  (let ((ed (ed-make "abcdefghij" :width 5)))
    (is (= 2 (nle::editor-height ed)) "ten characters at width five are two rows")
    (is (equal '("abcde" "fghij") (nle::editor-visible-lines ed))))
  (let ((ed (nle::make-editor :width 5)))
    (is (= 1 (nle::editor-height ed)) "an empty buffer is still one row")
    (is (equal '("") (nle::editor-visible-lines ed)) "one blank row")))

(deftest editor-height-is-capped-and-recomputed ()
  (let ((ed (ed-make "abcdefghij" :width 5 :max-height 1)))
    (is (= 1 (nle::editor-height ed)) "the cap wins over the content")
    (is (= 1 (length (nle::editor-visible-lines ed))) "one visible row"))
  (let ((ed (ed-make "abcdefghij" :width 10)))
    (is (= 1 (nle::editor-height ed)) "the text fits one row at width ten")
    (setf ed.width 5)
    (is (= 2 (nle::editor-height ed)) "narrowing the width re-wraps")
    (setf ed.max-height 1)
    (is (= 1 (nle::editor-height ed)) "and a new cap applies at once")))

(deftest editor-caret-cell-is-the-painted-cell (let ((ed (ed-make "abcdefghij" :width 5))))
  (ed-set-cursor ed 0 7)
  (is (equal '(2 1) (ed-position ed)) "the caret sits on the second row, two columns in")
  (ed-set-cursor ed 0 5)
  (is (equal '(5 0) (ed-position ed)))
  (ed-set-cursor ed 0 0)
  (is (equal '(0 0) (ed-position ed)) "the start is the first cell"))

(deftest editor-caret-measures-columns-not-characters ()
  ;; Three ideographs are two columns each: width four holds two of them.
  (let ((ed (ed-make (coerce '(#\日 #\本 #\語) 'string) :width 4)))
    (is-shape ed (nle::editor-height = 2 "the third ideograph wraps")
      (nle::editor-visible-lines (list (coerce '(#\日 #\本) 'string) (string #\語))))
    (ed-set-cursor ed 0 1)
    (is (equal '(2 0) (ed-position ed)) "one ideograph in is two columns in")
    (ed-set-cursor ed 0 3)
    (is (equal '(2 1) (ed-position ed)) "past the wrapped ideograph on the second row")))

(deftest editor-window-follows-the-caret ()
  (let ((ed (ed-make "abcdefghijklmno" :width 5 :max-height 2)))
    (is (equal '("abcde" "fghij") (nle::editor-visible-lines ed)))
    (ed-set-cursor ed 0 12)
    (is (equal '("fghij" "klmno") (nle::editor-visible-lines ed)))
    (is (equal '(2 1) (ed-position ed)) "the caret cell is relative to the window")
    (ed-set-cursor ed 0 0)
    (is (equal '("abcde" "fghij") (nle::editor-visible-lines ed)))))

(deftest editor-window-clamps-to-the-content ()
  (let ((ed (ed-make (format nil "a~%b~%c") :max-height 5)))
    (setf ed.offset 9)
    (is-shape ed (nle::editor-visible-lines '("a" "b" "c")) (.offset = 0 "nothing to scroll"))))

(deftest editor-vertical-motion-walks-visual-rows ()
  ;; Stepping the logical row instead would jump the caret past every
  ;; wrapped row of the line it sits in.
  (ed-walk (ed-make "abcdefghijklmno" :width 5)
    (:at 0 12)
    ((:up) (0 7) "Up keeps the offset within the row above")
    ((:up) (0 2) "and again")
    ((:up) (0 2) "at the top it holds")
    ((:down) (0 7) "Down returns")
    ((:down) (0 12) "to where it started")
    ((:down) (0 12) "at the bottom it holds")))

(deftest editor-vertical-motion-crosses-logical-lines ()
  (ed-walk (ed-make (format nil "abcdefghij~%kl") :width 20)
    (:at 0 8)
    ((:down) (1 2) "Down onto a shorter line clamps to its end")
    ((:up) (0 2) "Up keeps the clamped offset")))

(deftest editor-counts-the-lines-its-window-hides ()
  ;; EDITOR-HIDDEN-LINES counts logical lines, the ones the operator typed: a
  ;; line the window cuts in two counts on the side its hidden part is, so the
  ;; first line starting in view is always ABOVE + 1.
  (flet ((hidden (ed row col)
           (ed-set-cursor ed row col)
           (multiple-value-list (nle::editor-hidden-lines ed))))
    (let ((ed (ed-make (format nil "a~%b~%c~%d~%e~%f") :width 10 :max-height 3)))
      (is (equal '(0 3) (hidden ed 0 0)) "at the top, three below")
      (is (equal '(3 0) (hidden ed 5 1)) "at the end, three above"))
    (let ((ed (ed-make (format nil "x~%~a~%z" (make-string 20 :initial-element #\y))
                       :width 10 :max-height 2)))
      (is (equal '(0 2) (hidden ed 0 0)) "the wrapped line continues below, so it counts there")
      (is (equal '(2 0) (hidden ed 2 1)) "and above once its start is out of view"))
    (is (equal '(0 0) (hidden (nle::make-editor :max-height 3) 0 0)) "an empty editor hides nothing")))

;;; Ctrl+U kills to start, then removes an already-empty logical line.

(deftest editor-ctrl-u-removes-only-the-empty-line ()
  (dolist (case (list (list (format nil "above~%") 1 0 "above" '(0 5) t)
                     (list (format nil "above~%~%below") 1 0 (format nil "above~%below") '(0 5) t)
                     (list (format nil "above~%~%~%below") 2 0 (format nil "above~%~%below") '(1 0) t)
                     (list (format nil "~%below") 0 0 "below" '(0 0) t)
                     (list "" 0 0 "" '(0 0) nil)
                     (list (format nil "above~%below") 1 0 (format nil "above~%below") '(1 0) nil)
                     (list (format nil "above~%below") 1 3 (format nil "above~%ow") '(1 0) t)
                     (list (format nil "above~%  ") 1 2 (format nil "above~%") '(1 0) t)
                     (list (format nil "界abcdef~%") 1 0 "界abcdef" '(0 7) t)))
    (destructuring-bind (value row col expected cursor changed) case
      (let ((ed (ed-make value :width 4)))
        (ed-set-cursor ed row col)
        ;; Populate both memos before mutating, so stale paint/value fails.
        (nle::editor-value ed)
        (nle::editor-visible-lines ed)
        (let ((epoch (nle::editor-epoch ed)))
          (is (eq changed (ed-key ed #\u nlt:+mod-ctrl+)) "Ctrl+U reports exactly whether text changed")
          (is (= (+ epoch (if changed 1 0)) (nle::editor-epoch ed)) "one text edit bumps the epoch once"))
        (is (equal expected (nle::editor-value ed)) "only the current empty line or prefix is removed")
        (is (equal cursor (ed-cursor ed)) "an empty line parks on the adjacent previous line's end")
        (is (plusp (nle::editor-line-count ed)) "the editor always retains at least one logical line")
        (is (nle::editor-visible-lines ed) "the changed editor still has visible rows")))))

(deftest editor-repeated-ctrl-u-clears-then-removes-the-line ()
  (let ((ed (ed-make (format nil "above~%draft"))))
    (ed-set-cursor ed 1 5)
    (is (ed-key ed #\u nlt:+mod-ctrl+))
    (is (equal (format nil "above~%") (nle::editor-value ed)) "the first Ctrl+U keeps the newly emptied line")
    (is (equal '(1 0) (ed-cursor ed)))
    (is (ed-key ed #\u nlt:+mod-ctrl+))
    (is (equal "above" (nle::editor-value ed)) "the second Ctrl+U removes that empty line")
    (is (equal '(0 5) (ed-cursor ed)))))
