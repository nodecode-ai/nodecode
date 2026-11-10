;;;; terminal-test.lisp --- the modes, the transitions, the clipboard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from cl-tuition's tests/test-terminal-lifecycle.lisp and the
;;;; clipboard cases of tests/test-terminal-caps.lisp (Copyright (C) 2025
;;;; Anthony Green, MIT). The lifecycle contract: the prelude and the
;;;; epilogue set and clear DEC private modes and nothing else — never an
;;;; erase, never a cursor home — because the primary buffer is what
;;;; ESC[?1049l hands back on exit.

(in-package #:nodecode.test)

(nlk:access (program nlt::program) (renderer nlt::renderer))

(defun dec-private-modes (string &aux (re (format nil "~C\\[\\?(\\d+)([hl])" #\Escape)) (modes '()))
  "STRING parsed as a run of ESC [ ? digits (h|l) sequences: a list of
(NUMBER . #\\h-or-#\\l), or :MALFORMED for any byte outside that grammar."
  (if (cl-ppcre:scan (format nil "\\A(?:~a)*\\z" re) string)
      (cl-ppcre:do-register-groups ((#'parse-integer number) switch) (re string (nreverse modes))
        (push (cons number (char switch 0)) modes))
      :malformed))

(defun written (writer &rest arguments)
  "What WRITER writes when handed a stream and then ARGUMENTS."
  (with-output-to-string (s) (apply writer s arguments)))

(defun console-record (&key (type 1) (down t) (repeat 1) (unit 0))
  "One INPUT_RECORD's 20 bytes as ReadConsoleInputW lays them down: the
event type at 0, bKeyDown at 4, wRepeatCount at 8, uChar at 14."
  (let ((octets (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((put16 (at value)
             (setf (aref octets at) (ldb (byte 8 0) value)
                   (aref octets (1+ at)) (ldb (byte 8 8) value))))
      (put16 0 type)
      (put16 4 (if down 1 0))
      (put16 8 repeat)
      (put16 14 unit))
    octets))

(deftest console-records-carry-the-keys-a-raw-terminal-sends ()
  ;; What the Windows input stream reads: key-down characters only, Enter's
  ;; CR among them, which SBCL's own console reader deletes.
  (let* ((records (list (console-record :unit (char-code #\a))
                        (console-record :unit (char-code #\a) :down nil)
                        (console-record :unit 13)
                        (console-record :unit 0)
                        (console-record :type 2 :unit (char-code #\m))
                        (console-record :unit (char-code #\x) :repeat 3)
                        (console-record :unit #xD83D)
                        (console-record :unit #xDE00)))
         (octets (apply #'concatenate '(vector (unsigned-byte 8)) records)))
    (multiple-value-bind (text pending) (nlt::console-records-text octets (length records))
      (is (equal (format nil "a~Cxxx~C" #\Return (code-char #x1F600)) text))
      (is (null pending) "nothing held back")))
  (multiple-value-bind (text pending)
      (nlt::console-records-text (console-record :unit #xD83D) 1)
    (is (equal "" text) "a high half alone waits")
    (is (eql #xD83D pending) "and comes back pending")
    (is (equal (string (code-char #x1F600))
               (nlt::console-records-text (console-record :unit #xDE00) 1 pending)))))

;;; Windows Terminal and conhost send Shift+Enter as the bare CR Enter is, and
;;; ignore the kitty flags; under win32-input-mode (DEC 9001) each key event
;;; arrives as ESC [ Vk;Sc;Uc;Kd;Cs;Rc _ instead. The reports below are the
;;; ones Windows Terminal 1.24 sent for real keys, and the bytes each must come
;;; back as are what conhost's own legacy encoding sent for the same key.

(defun win32-report (vk scan unit state &key (down 1) (repeat 1))
  (format nil "~C[~D;~D;~D;~D;~D;~D_" #\Escape vk scan unit down state repeat))

(defun win32-press (vk scan unit state)
  "A key's press and release, the two reports a keystroke sends."
  (concatenate 'string (win32-report vk scan unit state)
               (win32-report vk scan unit state :down 0)))

(deftest win32-input-reports-come-back-as-the-legacy-keys ()
  (is-table (vk scan unit state expected) (equal expected (nlt::win32-input-text (win32-press vk scan unit state)))
    (13 28 13 16 (csi "13;2u") "Shift+Enter is the kitty Shift+Enter")
    (13 28 13 0 (string #\Return) "Enter is CR")
    (13 28 13 256 (string #\Return) "keypad Enter is CR")
    (13 28 10 8 (string #\Newline) "Ctrl+Enter is LF")
    (13 28 13 2 (format nil "~C~C" #\Escape #\Return) "Alt+Enter is ESC CR")
    (65 30 97 0 "a" "a")
    (65 30 65 16 "A" "Shift+a")
    (67 46 3 8 (string (code-char 3)) "Ctrl+C")
    (88 45 120 2 (format nil "~Cx" #\Escape) "Alt+x")
    (81 16 64 9 "@" "AltGr+Q composes, no ESC")
    (16 42 0 16 "" "a bare modifier sends nothing")
    (32 57 32 8 "" "Ctrl+Space sends nothing")
    (27 1 27 0 (string #\Escape) "Escape")
    (8 14 8 0 (string (code-char 127)) "Backspace is DEL")
    (8 14 127 8 (string (code-char 8)) "Ctrl+Backspace is BS")
    (9 15 9 0 (string #\Tab) "Tab")
    (9 15 9 16 (csi "Z") "Shift+Tab is CSI Z")
    (38 72 0 256 (csi "A") "Up")
    (38 72 0 264 (csi "1;5A") "Ctrl+Up")
    (36 71 0 256 (csi "H") "Home")
    (112 59 0 0 (format nil "~COP" #\Escape) "F1 is SS3 P")
    (113 60 0 16 (csi "1;2Q") "Shift+F2")
    (116 63 0 0 (csi "15~") "F5")
    (46 83 0 256 (csi "3~") "Delete")
    (33 73 0 264 (csi "5;5~") "Ctrl+PageUp")))

(deftest win32-input-keeps-what-is-not-a-report ()
  ;; A paste, a mouse report and everything a terminal without 9001 sends
  ;; arrive as text; a paste's line breaks come as reports among it.
  (let ((legacy (format nil "hi~A~C~A~A~C[" (csi "<0;10;5M") #\Return (csi "1;5A")
                        (csi "200~") #\Escape)))
    (is (equal legacy (nlt::win32-input-text legacy)) "untouched, Alt+[ at the end too"))
  (let ((pasted (format nil "~Aa~Ab~A" (csi "200~") (win32-press 13 28 13 0) (csi "201~"))))
    (is (equal (format nil "~Aa~Cb~A" (csi "200~") #\Return (csi "201~")) (nlt::win32-input-text pasted)) "a paste's CR report is a CR"))
  (is (equal "aaa" (nlt::win32-input-text (win32-report 65 30 97 0 :repeat 3))) "a repeat count repeats"))

(deftest win32-input-holds-a-report-cut-in-two ()
  (let ((shift-enter (win32-report 13 28 13 16)))
    (multiple-value-bind (text tail) (nlt::win32-input-text (subseq shift-enter 0 9))
      (is (equal "" text) "nothing of a cut report is sent")
      (is (equal (subseq shift-enter 0 9) tail) "it waits as the tail")
      (is (equal (csi "13;2u") (nlt::win32-input-text (concatenate 'string tail (subseq shift-enter 9)))) "and the rest completes it")))
  (multiple-value-bind (text tail surrogate)
      (nlt::win32-input-text (win32-press 0 0 #xD83D 0))
    (is (equal "" text) "a high half alone waits")
    (is (equal "" tail))
    (is (equal (string (code-char #x1F600)) (nlt::win32-input-text (win32-press 0 0 #xDE00 0) surrogate)) "for its low half")))

(deftest win32-shift-enter-decodes-apart-from-enter ()
  (is (key-is (key-from-string (nlt::win32-input-text (win32-press 13 28 13 16))) :enter :mod nlt:+mod-shift+) "Shift+Enter reaches the keymap as shift+enter")
  (let ((enter (key-from-string (nlt::win32-input-text (win32-press 13 28 13 0)))))
    (is (key-is enter :enter))
    (is (zerop (logand nlt:+mod-shift+ (nlt:key-event-mod enter))) "Enter carries no Shift")))

(deftest kitty-keyboard-asks-windows-for-win32-input ()
  ;; The kitty flags are what tell Shift+Enter apart on Unix terminals;
  ;; on Windows the same switch turns on win32-input-mode, and its undo off.
  (let ((push (written #'nlt:push-kitty-keyboard))
        (pop (written #'nlt:pop-kitty-keyboard)))
    (is (search (csi ">1u") push))
    (is (search (csi "<u") pop))
    (is (eq #+win32 t #-win32 nil (and (search (csi "?9001h") push) t)))
    (is (eq #+win32 t #-win32 nil (and (search (csi "?9001l") pop) t)))))

;;; Quit used to hand back a blank terminal: the library's launch prelude
;;; erased the primary buffer (ESC[2J ESC[H) before the first frame entered
;;; the alt screen, so ESC[?1049l on exit restored the wipe.

(deftest prelude-sets-dec-private-modes-and-nothing-else (let ((out (written #'nlt:enter-modes))))
  (is (listp (dec-private-modes out)) "the prelude is modes only")
  (is-table (sequence) (null (search (csi sequence) out))
    ("2J" "no screen erase") ("3J" "no scrollback erase") ("H" "no cursor home")
    ("?1049h" "the alternate screen is the first frame's to enter"))
  (is (search (csi "?2004h") out) "bracketed paste on")
  (is (search (csi "?2031h") out) "colour-scheme reports on")
  (is (search (csi "?1004h") out) "focus reports on"))

(defun without-cursor-shape (string)
  "STRING with its DECSCUSR (ESC [ N SP q) cursor-shape sequences taken out."
  (cl-ppcre:regex-replace-all (format nil "~C\\[\\d* q" #\Escape) string ""))

(deftest epilogue-shows-the-cursor-last (let* ((out (written #'nlt:exit-modes))
                                               (shape (search (csi "0 q") out))
                                               (show (search (csi "?25h") out))))
  (is (and shape show (< shape show)) "the terminal's own cursor shape back, before the cursor shows")
  (is (listp (dec-private-modes (without-cursor-shape out))) "the rest of the epilogue is modes only")
  (is (search (csi "?2004l") out) "bracketed paste off")
  (is (search (csi "?2031l") out) "colour-scheme reports off")
  (is (search (csi "?1004l") out) "focus reports off")
  (is (and show (= (+ show (length (csi "?25h"))) (length out))) "cursor shown last"))

(deftest every-mode-the-prelude-sets-the-epilogue-clears ()
  (let ((on (dec-private-modes (written #'nlt:enter-modes)))
        (off (dec-private-modes (without-cursor-shape (written #'nlt:exit-modes)))))
    (is (and (listp on) (listp off)) "both parse")
    ;; 1049 pairs with DECAWM 7 (on with 7l, off with 7h), so compare the
    ;; numbers touched rather than h/l per number.
    (is (null (set-difference (mapcar #'car on) (mapcar #'car off))))))

(defun transition (previous current)
  (with-output-to-string (s) (nlt:apply-terminal-transitions previous current s)))

(deftest transitions-follow-the-view-state ()
  (let ((plain (nlt:make-view (list "x")))
        (alt (nlt:make-view (list "x") :alt-screen t))
        (mouse (nlt:make-view (list "x") :mouse-mode :all-motion))
        (cursor (nlt:make-view (list "x") :cursor (nlt:make-cursor :x 1 :y 2)))
        (keys (nlt:make-view (list "x") :keyboard-enhancements t)))
    (is (search (csi "?1049h") (transition nil alt)) "the first alt frame enters the alt screen")
    (is (equal "" (transition alt alt)) "an unchanged view emits nothing")
    (is (search (csi "?1049l") (transition alt plain)) "leaving the alt screen")
    (is (search (csi "?1003h") (transition nil mouse)) "all-motion tracking on")
    (is (search (csi "?1003l") (transition mouse plain)) "tracking off when the mode goes")
    (is (search (csi "1 q") (transition nil cursor)) "a cursor appearing sets the blinking block")
    (is (null (search (csi "1 q") (transition cursor cursor))) "and only once")
    (is (search (csi ">1u") (transition nil keys)) "the enhancements push disambiguation, flag 1")
    (is (equal "" (transition keys keys)) "and only once")
    (is (search (csi "<u") (transition keys plain)) "dropping the enhancements pops the mode")))

(deftest a-terminal-without-any-event-tracking-still-gets-the-wheel ()
  ;; Termux accepts 1003 and drops it: asked for 1003 alone, it sent the
  ;; wheel as Up/Down and the prompt walked its history (#89).
  ;; Button events first, so a terminal that keeps one mode ends on 1003.
  (is (equal '((1002 . #\h) (1003 . #\h) (1006 . #\h))
             (dec-private-modes (written #'nlt:enable-mouse :all-motion))))
  (is (equal '((1002 . #\h) (1006 . #\h))
             (dec-private-modes (written #'nlt:enable-mouse :cell-motion))))
  ;; Every mode tracking turned on is turned off again.
  (is (null (set-difference '(1002 1003 1006)
                            (mapcar #'car (dec-private-modes (written #'nlt:disable-mouse)))))))

(defun osc2-writes (string &aux (found '()))
  "The OSC 2 payloads STRING carries, in order: what a terminal would read
as a window title, the empty string included."
  (cl-ppcre:do-register-groups (body)
      ((format nil "(?s)~C\\]2;(.*?)(?:~C\\\\|\\z)" #\Escape #\Escape) string (nreverse found))
    (push body found)))

(deftest an-inherited-screen-is-not-entered-again ()
  ;; A switch's successor paints a terminal that is already in the alternate
  ;; screen, holding the frame the image before it left there: entering again
  ;; would clear that frame, and the operator would watch a blank terminal
  ;; for as long as the first paint takes.
  (let ((alt (nlt:make-view (list "x") :alt-screen t)))
    (let ((nlt:*inherit-screen* t))
      (is (null (search (csi "?1049h") (transition nil alt))))
      (is (search (csi "?1003h") (transition nil (nlt:make-view (list "x")
                                                                :mouse-mode :all-motion)))))
    (is (search (csi "?1049h") (transition nil alt)))
    ;; The flag is the first frame's to consume: after it, frames are
    ;; transitions against our own paint, and a view that leaves and re-enters
    ;; the alt screen is followed.
    (let ((nlt:*inherit-screen* t))
      (nlt:render (nlt:make-renderer)
                  (nlt:make-view (list "x") :alt-screen t))
      (is (null nlt:*inherit-screen*) "the first frame consumes the flag"))))

(deftest a-refused-handover-leaves-a-terminal-a-shell-can-use ()
  ;; The successor that cannot serve is the last process on that terminal:
  ;; it gives back every mode the image before it may have switched on.
  (let ((out (with-output-to-string (s) (nlt:release-inherited-modes s))))
    (is-table (in) (search in out) ((csi "?1049l") "the alternate screen is released")
      ((csi "?1003l") "mouse tracking is off") ((csi "?2004l") "bracketed paste is off")
      ((csi "<u") "the kitty keyboard flags are popped")
      ((csi "?25h") "the cursor is visible again"))))

(deftest transitions-name-the-window-only-when-the-title-moved ()
  ;; A title is live-only presentation: written on the frame that changed
  ;; it, on no other, and never at all when the frame does not ask for one.
  (let ((plain (nlt:make-view (list "x")))
        (named (nlt:make-view (list "x") :title "default · nodecode"))
        (renamed (nlt:make-view (list "x") :title "coder · nodecode")))
    (is (equal '("default · nodecode") (osc2-writes (transition nil named))))
    (is (null (osc2-writes (transition named named))) "an unchanged title writes nothing")
    (is (equal '("coder · nodecode") (osc2-writes (transition named renamed))))
    (is (equal '("") (osc2-writes (transition named plain))))
    (is (null (osc2-writes (transition nil plain))) "a NIL title never writes an escape")
    (is (null (osc2-writes (transition plain plain))) "and still writes nothing on a repeat")))

(deftest the-exit-path-gives-the-window-title-back ()
  ;; The same unwind that leaves the alternate screen releases the title,
  ;; so an error path does not leave the shell wearing the organism's name.
  (flet ((undo (view &aux (program (nlt::make-program nil))
                          (renderer (nlt:make-renderer)))
           (with-output-to-string (stream)
             (setf program.renderer renderer
                   program.tty-stream stream
                   renderer.output-stream stream
                   renderer.last-view-state view)
             (nlt::%undo-view-modes program))))
    (is (equal '("") (osc2-writes (undo (nlt:make-view (list "x") :title "coder · nodecode")))))
    (is (null (osc2-writes (undo (nlt:make-view (list "x"))))))))

(deftest first-frame-enters-the-alt-screen-before-painting-and-never-erases ()
  (let* ((out (with-output-to-string (stream)
                (nlt:render (nlt:make-renderer :output-stream stream :width 4 :height 2)
                            (nlt:make-view (list "ab") :alt-screen t))))
         (alt-on (search (csi "?1049h") out))
         (home (search (csi "H") out)))
    (is alt-on "the first frame enters the alt screen")
    (is (and alt-on (null (search (csi "?1049h") out :start2 (1+ alt-on)))))
    (is (and alt-on home (< alt-on home)) "before the first paint")
    (is (null (search (csi "2J") out)) "and never erases")))

;;; Windows Terminal flickered the caret and every cell that moves: a frame
;;; painted its changed cells with the hardware cursor still shown, and parked
;;; it at the caret only after the synchronized update had closed. ConPTY
;;; re-emits a console's output on its own schedule and cuts it where it
;;; likes, so any point inside a frame can reach the screen, and the cursor
;;; stood on whatever the paint wrote last -- the spinner, a countdown --
;;; before it jumped back. Hidden for the paint and shown only once parked,
;;; inside the update, it is never seen anywhere but the caret.

(defun second-frame (rows cursor)
  "The bytes a 4x2 frame of ROWS with CURSOR writes over a blank frame."
  (let ((renderer (nlt:make-renderer :output-stream (make-broadcast-stream)
                                     :width 4 :height 2)))
    (nlt:render renderer (nlt:make-view (list "    " "    ") :cursor cursor))
    (with-output-to-string (stream)
      (setf renderer.output-stream stream)
      (nlt:render renderer (nlt:make-view rows :cursor cursor)))))

(deftest a-frame-shows-the-cursor-only-at-its-caret ()
  (let* ((out (second-frame (list "   x" "    ") (nlt:make-cursor :x 1 :y 1)))
         (hide (search (csi "?25l") out))
         (paint (search "x" out))
         (show (search (csi "?25h") out))
         (tail (concatenate 'string (csi "2;2H") (csi "?25h") (csi "?2026l"))))
    (is (and hide paint (< hide paint)) "the cursor hides before the paint writes a cell")
    (is (and show (null (search (csi "?25h") out :start2 (1+ show)))) "and is shown once")
    (is (uiop:string-suffix-p out tail) "parked at the caret, then shown, inside the synchronized update"))
  (let ((out (second-frame (list "   x" "    ") nil)))
    (is (search (csi "?25l") out) "a frame without a caret paints hidden")
    (is (null (search (csi "?25h") out)) "and never shows the cursor")
    (is (uiop:string-suffix-p out (csi "?2026l")) "its update closes last")))

(deftest a-typed-key-pays-for-hiding-the-cursor-with-its-motion ()
  ;; The paint sends no home of its own, and the caret is reached from where
  ;; the paint left off: a key typed at the caret costs no motion at all.
  (let ((typed (second-frame (list "x   " "    ") (nlt:make-cursor :x 1 :y 0)))
        (below (second-frame (list "x" "    ") (nlt:make-cursor :x 1 :y 1))))
    (is (string= (concatenate 'string (csi "?2026h") (csi "?25l") (csi "H") "x" (csi "?25h") (csi "?2026l")) typed) "the frame is the key, the cursor hidden around it, and nothing else")
    (is (uiop:string-suffix-p below (concatenate 'string "x" (csi "B") (csi "?25h") (csi "?2026l"))) "a caret elsewhere is reached by the shortest motion from the paint's end")))

(deftest terminal-size-answers-two-positive-integers ()
  (multiple-value-bind (width height) (nlt:terminal-size)
    (is (and (integerp width) (plusp width)) "a positive width")
    (is (and (integerp height) (plusp height)) "a positive height")))

(defconstant +tiocswinsz+ #+linux #x5414 #+darwin #x80087467 #-(or linux darwin) #x5414)

(deftest terminal-size-reads-both-dimensions-off-a-real-terminal ()
  #+win32 (is nil "cannot exist on Windows: no pseudo-terminal takes TIOCSWINSZ there; the console's size is read through GetConsoleScreenBufferInfo")
  #-win32
  ;; The live binary once died at boot setting the renderer's height to NIL:
  ;; OR over the ioctl form kept only its primary value. A pty with a size
  ;; set through TIOCSWINSZ exercises the real path, not the fallback.
  (macrolet ((libc (name result argument)
               `(sb-alien:alien-funcall
                 (sb-alien:extern-alien ,name (function ,result sb-alien:int)) ,argument)))
    (let ((ran nil)
          ;; A fresh pseudo-terminal through libc's EXTERN-ALIENs (sb-posix wraps none).
          (master (libc "posix_openpt" sb-alien:int (logior sb-posix:o-rdwr sb-posix:o-noctty))))
      (when (>= master 0)
        (nlk:with-cleanup ((sb-posix:close master))
          (libc "grantpt" sb-alien:int master)
          (libc "unlockpt" sb-alien:int master)
          (let ((slave (sb-posix:open (libc "ptsname" sb-alien:c-string master)
                                      (logior sb-posix:o-rdwr sb-posix:o-noctty))))
            (nlk:with-cleanup ((sb-posix:close slave))
              (sb-alien:with-alien ((ws (sb-alien:struct nlt::winsize)))
                (setf (sb-alien:slot ws 'nlt::rows) 11
                      (sb-alien:slot ws 'nlt::cols) 37
                      (sb-alien:slot ws 'nlt::xpixel) 0
                      (sb-alien:slot ws 'nlt::ypixel) 0)
                (sb-unix:unix-ioctl slave +tiocswinsz+ (sb-alien:alien-sap ws))
                (setf ran t)
                (multiple-value-bind (width height) (nlt:window-size slave)
                  (is (= 37 width) "the width the pty was given")
                  (is (= 11 height) "and the height, as a second value"))
                (multiple-value-bind (width height) (nlt:terminal-size slave)
                  (is (and (= 37 width) (= 11 height)))))))))
      (is ran "this host handed out a pty")
      (multiple-value-bind (width height) (nlt:window-size 0)
        (declare (ignorable width))
        (is (or (null width) (and (plusp width) (plusp height))))))))

;;; --- the clipboard -------------------------------------------------------

(deftest osc52-write-carries-base64-for-the-clipboard ()
  (is (string= (format nil "~C]52;c;aGVsbG8=~C\\" #\Escape #\Escape)
               (nlt::%osc52-sequence "hello"))))

(deftest osc52-carries-the-text-as-utf-8 (let* ((text (format nil "row 006 ~C golf ~C done"
                                                           (code-char #xb7) (code-char #x2022)))
                                                (utf-8 (cl-base64:usb8-array-to-base64-string
                                                        (sb-ext:string-to-octets text :external-format :utf-8)))))
  (is (search utf-8 (nlt::%osc52-sequence text)) "a dot or a bullet pastes as itself, not as Latin-1"))

(deftest osc52-is-wrapped-for-a-multiplexer (let* ((seq (nlt::%osc52-sequence "x"))
                                                   (tmux (nlt::%wrap-passthrough seq :tmux))
                                                   (screen (nlt::%wrap-passthrough seq :screen))))
  (is (string= (format nil "~CPtmux;" #\Escape) (subseq tmux 0 7)) "tmux takes DCS tmux;")
  (is (search (format nil "~C~C]52;" #\Escape #\Escape) tmux) "with every ESC doubled")
  (is (string= (format nil "~C\\" #\Escape) (subseq tmux (- (length tmux) 2))) "ended by ST")
  (is (string= (format nil "~CP" #\Escape) (subseq screen 0 2)) "screen takes DCS chunks")
  (is (string= (format nil "~C\\" #\Escape) (subseq screen (- (length screen) 2))) "ended by ST")
  (is (string= seq (nlt::%wrap-passthrough seq nil)) "no multiplexer, no wrapping"))

(deftest osc52-behind-tmux-goes-bare-and-wrapped (let ((out (make-string-output-stream))
                                                       (seq (nlt::%osc52-sequence "x"))))
  ;; A default tmux drops both the bare sequence (set-clipboard external) and
  ;; the passthrough (allow-passthrough off); either option turned on must be
  ;; enough, so both go, the bare one first.
  (nlt:copy-to-clipboard "x" :stream out :multiplexer :tmux)
  (let ((written (get-output-stream-string out)))
    (is (eql 0 (search seq written)) "the bare sequence first: tmux's own clipboard write")
    (is (search (nlt::%wrap-passthrough seq :tmux) written) "then the passthrough to the outer terminal"))
  (nlt:copy-to-clipboard "x" :stream out :multiplexer nil)
  (is (string= seq (get-output-stream-string out)) "no multiplexer: the sequence once, bare"))

(deftest clipboard-falls-back-to-osc52-without-a-helper (let ((out (make-string-output-stream))))
  (is (eq :osc52 (nlt:copy-to-clipboard "clip me" :stream out)) "the backend is OSC 52")
  (is (search "]52;" (get-output-stream-string out)) "written on the stream"))

(deftest clipboard-helper-runner-keys-off-the-exit-status ()
  (is (null (nlk:exits-zero-p '("nlt-missing-clipboard-helper") :input "text")))
  ;; Through sh: Windows has no `true' or `false' on its PATH.
  (is (eq t (nlk:exits-zero-p (nlk:shell-argv "true") :input "text")) "exit 0 is success")
  (is (null (nlk:exits-zero-p (nlk:shell-argv "false") :input "text")) "exit 1 is failure"))

;;; --- the descriptor never crosses a dump ---------------------------------

(deftest terminal-state-is-forgotten-before-a-dump ()
  ;; A descriptor is a fact about one process: an image that carried the
  ;; number called termios on whatever the next process had there (a
  ;; release built from a terminal baked fd 10; the owner path's gateway
  ;; socket landed on it and the launch died in TCGETATTR).
  (is (member 'nlt::forget-terminal sb-ext:*save-hooks*))
  ;; Any descriptor stands for the terminal's: a file opens alike everywhere.
  (let* ((path (temp-path "terminal-state" "txt"))
         (fd (sb-posix:open (sb-ext:native-namestring path)
                            (logior sb-posix:o-creat sb-posix:o-wronly) #o600)))
    (nlk:with-cleanup ((ignore-errors (delete-file path)))
      (let ((nlt::*tty-fd* fd)
            (nlt::*tty-stream* nil)
            (nlt::*original-termios* :saved))
        (nlt::forget-terminal)
        (is (null nlt::*tty-fd*) "the descriptor is forgotten")
        (is (null nlt::*tty-stream*) "the stream over it too")
        (is (null nlt::*original-termios*) "and the saved attributes"))
      (is (null (ignore-errors (sb-posix:fstat fd))) "and closed"))))

;;; --- a process with no terminal --------------------------------------------
;;; The device the terminal is opened by is the one seam: a path that will not
;;; open is a process started with no controlling terminal (cron, setsid, ssh
;;; without -t), and every open of the terminal says so by one name.

(deftest a-process-with-no-terminal-is-told-so-by-name ()
  (let ((nlt::*tty-fd* nil)
        (nlt::*tty-stream* nil)
        (nlt::*terminal-device* "/nonexistent/tty"))
    (is (not (nlt:terminal-present-p)) "nothing opens, so no terminal is present")
    (is-values (name text) (handler-case (nlt::tty-fd)
                             (nlt:no-terminal (condition)
                               (values (type-of condition) (princ-to-string condition))))
      (name eq 'nlt:no-terminal "the descriptor asks by name")
      (text "this process has no controlling terminal"))
    #-win32 (is (typep (nth-value 1 (ignore-errors (nlt::open-tty))) 'nlt:no-terminal) "and so does the stream")
    (is (null nlt::*tty-fd*) "a refusal caches nothing")
    (is (null nlt::*tty-stream*))))

#-win32
(deftest a-terminal-that-opens-is-present-and-the-probe-closes-it ()
  (flet ((next-fd () (let ((fd (sb-posix:open "/dev/null" sb-posix:o-rdonly)))
                       (sb-posix:close fd)
                       fd)))
    (let ((nlt::*terminal-device* "/dev/null")
          (before (next-fd)))
      (is (nlt:terminal-present-p) "a device that opens is a terminal to draw on")
      (is (= before (next-fd)) "the probe leaves no descriptor behind"))))

;;; VS Code's restored terminal reports the mouse in the legacy encoding, a
;;; column past 95 as one raw byte that is not UTF-8. The strict decoder
;;; signalled on that byte and never got past it: the input loop failed on it
;;; for good and no key was read again.
#-win32
(deftest the-terminal-reads-past-a-byte-that-is-not-utf-8 ()
  (let ((path (format nil "/tmp/nodecode-stray-byte-~d" (sb-posix:getpid))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
             (write-sequence #(27 91 77 67 131 59 122) out))
           (let* ((nlt::*terminal-device* path)
                  (nlt::*tty-stream* nil)
                  (tty (nlt::open-tty)))
             (unwind-protect
                  (let ((report (nlt:read-key tty)) (key (nlt:read-key tty)))
                    (is (nlt::screen-lost-msg-p report) "the legacy report reads, its raw column byte and all")
                    (is (and (nlt:key-press-msg-p key) (eql #\z (nlt:key-event-code key))) "and the key after it reads"))
               (close tty))))
      (ignore-errors (delete-file path)))))

;;; --- the truecolor probe ---------------------------------------------------

(deftest truecolor-probe-sets-asks-and-resets-in-one-write ()
  (let ((out (with-output-to-string (s) (nlt:request-truecolor s))))
    (is-let* ((sentinel (search (csi "38:2:1:2:3m") out))
              (readback (search (format nil "~CP$qm~C\\" #\Escape #\Escape) out))
              (tcap (search (format nil "~CP+q524742~C\\" #\Escape #\Escape) out)))
      (sentinel is "the sentinel foreground is set") (readback is "DECRQSS asks for the SGR state")
      (tcap is "XTGETTCAP asks for the RGB capability")
      ((and sentinel readback (< sentinel readback)) is)
      ((search (csi "0m") out :start2 (or readback 0)) is))))
