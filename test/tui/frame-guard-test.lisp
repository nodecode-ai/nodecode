;;;; frame-guard-test.lisp --- the display authority and the frame guard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One bug class, locked at its three seams. Content and control share the
;;;; frame string the renderer parses, so a control codepoint in content must
;;;; paint as a Control Picture and measure as the one column it paints — or
;;;; the gutter cell, the padding, and every row below move. The gates:
;;;;
;;;;   G1. The authority (theme.lisp DISPLAY-GLYPH / DISPLAY-TEXT /
;;;;       CHAR-DISPLAY-WIDTH): pictures, widths, the uncopied clean path,
;;;;       measure = paint through the renderer's own parser for every
;;;;       token of the adversarial alphabet.
;;;;   G2. The guard (ui.lisp GUARD-FRAME-ROWS): a clean row passes as the
;;;;       same object; a foreign ESC, a control, an over-wide row and a
;;;;       short row each repair to exactly WIDTH cells and name the breach;
;;;;       the strict posture signals instead.
;;;;   G3. The property: random adversarial content through every content
;;;;       lane — user, assistant, reasoning, tool arguments, tool result,
;;;;       workspace diff, transport notice, queued steer, composer paste —
;;;;       at random geometry, at random scroll, under both profiles. Every
;;;;       frame row, parsed by the renderer's cell parser at height 2: never
;;;;       wraps, keeps the gutter cell where the scrollbar math puts it,
;;;;       carries no style or link the shell did not write, and reads back
;;;;       as the plain lane says. A new edge case is one alphabet entry.
;;;;   G4. The btop capture that started it, verbatim, as a tool result.

(in-package #:nodecode.test)

(nlk:access (vp nle::band-window))

;;; --- fixtures ---------------------------------------------------------------

(defun esc (&rest parts)
  "ESC followed by PARTS."
  (apply #'concatenate 'string (string #\Escape) parts))

(defun codes (&rest points)
  "The string of the characters whose code points are POINTS."
  (map 'string #'code-char points))

(defparameter *adversarial-alphabet*
  (append (list (esc))
          (mapcar #'esc '("[" "[31m" "[1;97m" "[0m"      ; torn CSI, content SGR
                          "[?1049h" "[2J" "[0;0f" "]"))  ; private mode, clear, home, torn OSC
          (list (esc "]8;;http://x" (esc "\\"))          ; OSC-8 link open
                (esc "]0;title" (codes 7)))              ; OSC with a BEL terminator
          (mapcar #'esc '("P" "_" "^" "(B" "\\"))        ; DCS / APC / PM openers, charset, bare ST
          ;; BEL, BS, CR, NUL, SO, SI, DEL, NEL, C1 CSI / OSC, LS, PS, TAB
          (mapcar #'codes '(7 8 13 0 14 15 127 #x85 #x9B #x9D #x2028 #x2029 9))
          (list "漢字" "日本語テキスト"
                (codes #x65 #x301)                       ; combining
                (codes #x1F468 #x200D #x1F469)           ; ZWJ family
                (codes #x2764 #xFE0F)                    ; VS16 heart
                (codes #x202E) (codes #xFFFD)            ; RLO, replacement
                "word" "another" "x" " " "  " "ERROR:" "Failed to get size of terminal!"))
  "Tokens random content is drawn from: every way a byte can be control to
the renderer or the terminal, beside the width-table hazards (wide,
combining, joiner, variation selector, bidi) and plain words.")

(defun renderer-cells (row width)
  "ROW parsed by the renderer's cell parser into a WIDTH x 2 buffer: row 0 is
the painted row, row 1 is where an over-wide row would have wrapped."
  (nlt:parse-styled-string (list row) width 2))

(defun expected-painted (plain)
  "What the renderer shows for PLAIN: the authority's pictures, tabs as the
space cell the compositor paints them as, zero-width codepoints dropped."
  (remove-if (lambda (ch) (zerop (nle::char-display-width ch)))
             (substitute #\Space #\Tab (nle::display-text plain))))

(defun blank-cell-p (cell)
  (and (char= #\Space cell.char) (null cell.fg) (null cell.bg) (zerop cell.attrs) (null cell.link)))

(defun shell-color-p (color foreground-p)
  "Whether COLOR is one the truecolor shell writes: NIL or a 24-bit triple."
  (or (null color)
      (uiop:string-prefix-p (if foreground-p "38;2;" "48;2;") color)))

;;; Blink and reverse are content's alone — a cell carrying one got its SGR
;;; from content. Strikethrough is the shell's too: a ~~run~~ draws struck.
(defparameter +shell-attrs+
  (logior nlt:+attr-bold+ nlt:+attr-faint+
          nlt:+attr-italic+ nlt:+attr-underline+ nlt::+attr-strikethrough+)
  "The attribute bits STYLED can set.")

(defun diff-metadata (text)
  "A workspace_diff result envelope whose one file's patch adds TEXT's lines."
  (workspace-diff (list "path" (concatenate 'string "a/" text ".txt") "added" 1 "removed" 0
                        "patch" (format nil "--- a/x~%+++ b/x~%@@ -1,1 +1,2 @@~% ctx~%~{+~a~%~}"
                                        (nle::split-lines text)))))

(defun check-frame (model width height label)
  "Every invariant of one composed frame, through the renderer's parser."
  (let* ((rows (nle::compose-frame model))
         (vp model.band)
         (scroll (nle::band-window-offset vp))
         ;; Canvas over the content: a promptless short band's slack, or a
         ;; click's anchor; a band holding a prompt is top-aligned.
         (slack (nle::band-window-top-padding vp))
         (plains model.line-plain)
         (text-width (nle::transcript-text-width width))
         (painted-p (not (eq (nle::effective-color-profile) :none))))
    (is (= height (length rows)) (format nil "~a: ~d rows" label height))
    (multiple-value-bind (thumb-top thumb-height)
        (nle:scrollbar-thumb vp.height vp.height (+ vp.height (nle::band-window-max-offset vp)) scroll)
      (loop for row in rows
            for index from 0
            for buffer = (renderer-cells row width)
            for band-row = (- index 3)
            do (is (loop for x below width
                         always (blank-cell-p (nlt:screen-buffer-ref buffer x 1)))
                   (format nil "~a: row ~d never wraps into the next row"
                           label index))
               (macrolet ((at (what) `(format nil "~a: row ~d col ~d ~a" label index x ,what)))
                 (loop for x below width
                       for cell = (nlt:screen-buffer-ref buffer x 0)
                       do (cond (painted-p
                                 (is (nlt:cell-bg cell) (at "carries the canvas bg"))
                                 (is (shell-color-p cell.fg t) (at "fg is the shell's"))
                                 (is (shell-color-p cell.bg nil) (at "bg is the shell's"))
                                 (is (zerop (logandc2 cell.attrs
                                                      +shell-attrs+))
                                     (at "attrs are the shell's")))
                                (t (is (and (null cell.fg) (null cell.bg) (zerop cell.attrs))
                                       (at "unstyled under :none"))))
                          (is (notany (lambda (ch) (nle::control-codepoint-p (char-code ch)))
                                      (nlt:cell-link cell))
                              (at "link is clean"))))
               ;; A status too tall for the composer opens as a docked panel
               ;; (SHOW-STATUS), which takes the band's tail rows and moves
               ;; the rest: the band's rows are its own only without one.
               (when (and (<= 0 band-row) (< band-row vp.height) (null (nle::chat-model-dialog model)))
                 (let ((gutter (nlt:cell-char
                                (nlt:screen-buffer-ref buffer (- width 1) 0)))
                       (bar-p (plusp (nle::band-window-max-offset vp)))
                       (thumb-p (<= thumb-top band-row (+ thumb-top thumb-height -1))))
                   (is (char= gutter (cond ((not bar-p) #\Space) (thumb-p #\▐) (t #\Space)))
                       (format nil "~a: band row ~d rail cell at col ~d ~
                                    is ~:[none~;~:[track~;thumb~]~]"
                               label band-row (- width 1) bar-p thumb-p))
                   (is (char= #\Space (nlt:cell-char (nlt:screen-buffer-ref buffer (- width 2) 0)))
                       (format nil "~a: band row ~d keeps a blank before the rail" label band-row))
                   ;; The band's row -> content-index map is its own geometry
                   ;; (BAND-WINDOW-ROW-INDEX): while a prompt pins, the band's
                   ;; rows are the prompt's and the pinned row above the content
                   ;; window maps to the row it paints, not to scroll+row. A row
                   ;; that owns no content (the band's spacer) maps to NIL.
                   (let ((line (nle::band-window-row-index vp (- band-row slack))))
                     (when (and line (< line (length plains)))
                       (let ((expected (string-right-trim
                                        " " (expected-painted
                                             (aref plains line))))
                             (actual (string-right-trim
                                      " " (subseq (painted-row buffer 0)
                                                  2))))
                         (is (uiop:string-prefix-p expected actual)
                             (format nil "~a: band row ~d reads as its ~
                                          plain lane: ~s vs ~s"
                                     label band-row expected
                                     (subseq actual 0 (min (length actual)
                                                           text-width)))))))))))
    rows))

;;; --- G1: the authority ------------------------------------------------------

(deftest frame-guard-authority-pictures-every-control ()
  (is-each (nle::display-glyph)
    (#\Escape (code-char #x241B) "ESC paints as its Control Picture")
    ((code-char 0) (code-char #x2400) "NUL paints as its Control Picture")
    (#\Return (code-char #x240D) "CR paints as its Control Picture")
    (#\Newline (code-char #x240A) "a newline inside a row is control, pictured")
    ((code-char 127) (code-char #x2421) "DEL paints as its Control Picture")
    ((code-char #x9B) (code-char #x2426) "a C1 byte paints as the substitute picture")
    (#\Tab #\Tab "tab is the compositor's one-space cell, not a picture")
    (#\a #\a "printable text is itself"))
  (is-each (nle::char-display-width)
    (#\Escape 1 "ESC measures one column")
    ((code-char #x9B) 1 "a C1 byte measures one column")
    ((code-char #x241B) 1 "the picture itself measures one column")
    ((code-char #x301) 0 "combining marks still measure zero")
    (#\漢 2 "wide still measures two")))

(deftest frame-guard-display-text-copies-only-on-a-control ()
  (is-let* ((clean "plain text, 漢字, tabs\tand all")) ((nle::display-text clean) eq clean))
  (is-let* ((name (coerce (format nil "a~cb" #\Escape) 'simple-base-string))
            (shown (nle::display-text name)))
    (shown string= "a␛b") ((length shown) = (length name)))
  (is (string= (nle::display-text (esc "[31mred")) "␛[31mred")))

(deftest frame-guard-measure-equals-paint-for-every-token (let ((nle::*color-profile* :truecolor)))
  ;; The authority's promise, token by token through the renderer's parser:
  ;; a styled run of any alphabet token paints exactly the cells
  ;; TEXT-DISPLAY-WIDTH counted, never wraps, and sets no style of its own.
  (dolist (token *adversarial-alphabet*)
    (let* ((text (concatenate 'string "[" token "]"))
           (measured (nle::text-display-width text))
           (width (+ measured 2))
           (buffer (renderer-cells (nle:styled text (nle::style-for-tone :foreground)) width)))
      (is (loop for x below width always (blank-cell-p (nlt:screen-buffer-ref buffer x 1)))
          (format nil "token ~s never wraps" token))
      (is (char= #\] (nlt:cell-char (nlt:screen-buffer-ref buffer (1- measured) 0)))
          (format nil "token ~s: the closing bracket lands at the measured ~
                         column ~d" token (1- measured)))
      (is (blank-cell-p (nlt:screen-buffer-ref buffer measured 0))
          (format nil "token ~s paints nothing past its measure" token))
      (is (loop for x below measured
                for cell = (nlt:screen-buffer-ref buffer x 0)
                always (and (shell-color-p cell.fg t)
                            (zerop (logandc2 cell.attrs
                                             +shell-attrs+))
                            (null cell.link)))
          (format nil "token ~s carries no style or link of its own"
                  token)))))

(deftest frame-guard-osc8-url-drops-controls ()
  (let ((wrapped (nle::osc8-wrap "x" (format nil "http://a~cb~cc" #\Bel
                                             #\Escape))))
    (is (search "http://abc" wrapped))
    (is (= 4 (count #\Escape wrapped)))))

;;; --- G2: the guard ----------------------------------------------------------

(defun guarded (row width &optional bg &aux (nle::*frame-guard-strict* nil))
  (nle::guard-frame-row row width bg))

(deftest frame-guard-passes-a-clean-row-untouched (let ((nle::*color-profile* :truecolor)))
  (let* ((row (concatenate 'string
                           (nle:styled "ab" (nle::style-for-tone :foreground))
                           (nle::osc8-wrap
                            (nle:styled "cd" (nle::style-for-tone :muted))
                            "http://x")
                           (nle::blank-frame-row 4 '(1 2 3)))))
    (is-values (out reason) (guarded row 8 '(1 2 3)) (out eq row "a clean row is the same object")
      (reason null "and names no breach"))))

(deftests frame-guard (row width reason expected why)
  ;; Each case is a breach, its width, the reason the guard names, and the
  ;; row it repairs to: the picture of what it held, at exactly WIDTH cells.
  (multiple-value-bind (out breach) (guarded row width)
    (is (eq reason breach) why)
    (is (string= expected out) "the repair pictures what the row held")
    (is (= width (nle::text-display-width out)) "at exactly the width"))
  (pictures-a-foreign-escape
   (concatenate 'string "ab" (esc "[?25l") "c") 12 :foreign-escape
   "ab␛[?25lc   "
   "a private-mode CSI is not the shell's")
  (pictures-a-raw-control-and-a-newline
   (format nil "a~cb~cc" #\Return #\Newline) 5 :control
   "a␍b␊c" "a raw control names the breach")
  (pads-a-short-row "ab" 6 :short "ab    " "two cells in six columns"))

(deftest frame-guard-cuts-an-over-wide-row (let ((nle::*color-profile* :truecolor)))
  (nlk:bind ((row (nle::osc8-wrap
                   (nle:styled "abcdefgh" (nle::style-for-tone :foreground))
                   "http://x")) ((out reason) (guarded row 5)))
    (is (eq :overflow reason) "eight cells in five columns")
    (let ((buffer (renderer-cells out 5)))
      (is (string= "abcde" (painted-row buffer 0)))
      (is (loop for x below 5 always (blank-cell-p (nlt:screen-buffer-ref buffer x 1))))
      (is (uiop:string-suffix-p out (concatenate 'string nle::*sgr-reset*
                                                 nle::*osc8-close*))))))

(deftest frame-guard-strict-posture-signals ()
  ;; The suite posture binds *FRAME-GUARD-STRICT*; a breach is an error.
  (is-let* ((c (signals-error nle::frame-guard-violation
                 (nle::guard-frame-rows (list "ab") 6 1 nil))))
    ((nle::frame-guard-violation-reason c) eq :short) ((nle::frame-guard-violation-index c) eql 0))
  (is (eq :height (nle::frame-guard-violation-reason
                   (signals-error nle::frame-guard-violation
                     (nle::guard-frame-rows (list "abcdef") 6 2 nil))))))

(deftest frame-guard-live-posture-repairs-and-counts (let ((nle::*frame-guard-strict* nil)
                                                           (before nle::*frame-guard-repairs*)
                                                           (nle::*frame-guard-noticed* t)))
  (let ((rows (nle::guard-frame-rows (list "ab" (esc "[?25l")) 4 3 nil)))
    (is (= 3 (length rows)) "live: the frame is brought to height")
    (is (equal (list "ab  " "␛[?2" "    ") rows))
    (is (= (+ before 3) nle::*frame-guard-repairs*))))

;;; --- G3: the property -------------------------------------------------------

(deftests frame-guard-property-holds-under (profile)
  (let ((nle::*color-profile* profile)
        (*random-state* (sb-ext:seed-random-state 20260821)))
    (with-temp-store ()
      (dotimes (sample 24)
        (let* ((width (+ 20 (random 120)))
               (height (+ 12 (random 20)))
               (text (with-output-to-string (out)
                       (dotimes (i (1+ (random 40)))
                         (write-string (nth (random (length *adversarial-alphabet*))
                                            *adversarial-alphabet*)
                                       out)
                         (when (zerop (random 9)) (write-char #\Newline out)))))
               (label (format nil "~a sample ~d (~dx~d)"
                              profile sample width height))
               (model (make-frame-model width height
                                        :session-id "frame-guard-property")))
          ;; TEXT through every content lane the shell projects into the band and the composer.
          (mark-turn-in-flight model)
          (fold-model model
            (nle::ingest-message-fact "user" text :turn-id "t1")
            (nle::ingest-message-fact "assistant" text :turn-id "t1" :reasoning text)
            (nle::ingest-tool-call-fact "c1" "eval" text :turn-id "t1")
            (nle::ingest-tool-result-fact "c1" "eval" text :turn-id "t1"
                                          :metadata (diff-metadata text))
            (nle::ingest-tool-call-fact "c2" "eval"
                                        (format nil "{\"form\":~a}"
                                                (shasht:write-json text nil))
                                        :turn-id "t1")
            (nle::ingest-tool-result-fact "c2" "eval" text :turn-id "t1"
                                          :error-p t)
            (nle::ingest-pending-input-fact text :prompt-id "p-1"))
          (nle::show-status model text)
          (nle::attach-composer-paste model text)
          (nle::render-state model)
          (check-frame model width height label)
          ;; A random scroll moves the thumb and the visible window.
          (let* ((vp model.band)
                 (room (- vp.count vp.height)))
            (when (plusp room)
              (setf (nle::band-window-offset vp) (random (1+ room)))
              (check-frame model width height
                           (format nil "~a scrolled" label))))))))
  (none :none)
  (truecolor :truecolor))

;;; --- G4: the capture --------------------------------------------------------

(deftest frame-guard-btop-capture-cannot-move-the-gutter ()
  (let ((nle::*color-profile* :truecolor)
        ;; What `script -qec btop /dev/null' writes on a pty with no size.
        (capture (concatenate 'string
                              (esc "[?1049h") (esc "[?25l") (esc "[?1002h") (esc "[?1015h")
                              (esc "[?1006h") (esc "[?1002l") (esc "[?1015l") (esc "[?1006l")
                              (esc "[2J") (esc "[0;0f") (esc "[0m") (esc "[?1049l")
                              (esc "[?25h") (esc "[0;91m") "ERROR: " (esc "[1;97m")
                              "Failed to get size of terminal!" (esc "[0m")
                              (string #\Return) (string #\Newline))))
    (with-frame-model (model 120 24 :session-id "btop"
                       :paint ((nle::ingest-tool-call-fact "c1" "eval"
                                                           "{\"form\":\"(uiop:run-program ...)\"}"
                                                           :turn-id "t1")
                               (nle::ingest-tool-result-fact "c1" "eval" capture
                                                             :turn-id "t1")))
      ;; Its output paints once the call is open.
      (rendering model (setf (nle::chat-model-expanded-tool-call model) "c1"))
      (let* ((rows (check-frame model 120 24 "btop"))
             (painted (mapcar (lambda (row) (painted-row (renderer-cells row 120) 0))
                              rows)))
        (is (rows-carry-p (nle::display-text "ERROR: ") painted))
        (is (rows-carry-p "␛[?1049h" painted))
        (is (notany (lambda (row &aux (buffer (renderer-cells row 120)))
                      (loop for x below 120
                            thereis (equal "91" (nlt:cell-fg
                                                 (nlt:screen-buffer-ref buffer x 0)))))
                    rows))))))
