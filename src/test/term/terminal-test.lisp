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

;;; Quit used to hand back a blank terminal: the library's launch prelude
;;; erased the primary buffer (ESC[2J ESC[H) before the first frame entered
;;; the alt screen, so ESC[?1049l on exit restored the wipe.

(deftest prelude-sets-dec-private-modes-and-nothing-else (let ((out (written #'nlt:enter-modes))))
  (is (listp (dec-private-modes out)) "the prelude is modes only")
  (is-table (sequence) (null (search (csi sequence) out))
    ("2J" "no screen erase") ("3J" "no scrollback erase") ("H" "no cursor home")
    ("?1049h" "the alternate screen is the first frame's to enter"))
  (is (search (csi "?2004h") out) "bracketed paste on"))

(deftest epilogue-shows-the-cursor-last (let* ((out (written #'nlt:exit-modes))
                                               (show (search (csi "?25h") out))))
  (is (listp (dec-private-modes out)) "the epilogue is modes only")
  (is (search (csi "?2004l") out) "bracketed paste off")
  (is (and show (= (+ show (length (csi "?25h"))) (length out))) "cursor shown last"))

(deftest every-mode-the-prelude-sets-the-epilogue-clears ()
  (let ((on (dec-private-modes (written #'nlt:enter-modes)))
        (off (dec-private-modes (written #'nlt:exit-modes))))
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
    (is (search (csi "2 q") (transition nil cursor)) "a cursor appearing sets the steady block")
    (is (null (search (csi "2 q") (transition cursor cursor))) "and only once")
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

(deftest terminal-size-answers-two-positive-integers ()
  (multiple-value-bind (width height) (nlt:terminal-size)
    (is (and (integerp width) (plusp width)) "a positive width")
    (is (and (integerp height) (plusp height)) "a positive height")))

(defconstant +tiocswinsz+ #+linux #x5414 #+darwin #x80087467 #-(or linux darwin) #x5414)

(deftest terminal-size-reads-both-dimensions-off-a-real-terminal ()
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
  (is (eq t (nlk:exits-zero-p '("true") :input "text")) "exit 0 is success")
  (is (null (nlk:exits-zero-p '("false") :input "text")) "exit 1 is failure"))

;;; --- the descriptor never crosses a dump ---------------------------------

(deftest terminal-state-is-forgotten-before-a-dump ()
  ;; A descriptor is a fact about one process: an image that carried the
  ;; number called termios on whatever the next process had there (a
  ;; release built from a terminal baked fd 10; the owner path's gateway
  ;; socket landed on it and the launch died in TCGETATTR).
  (is (member 'nlt::forget-terminal sb-ext:*save-hooks*))
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (nlk:with-cleanup ((ignore-errors (sb-posix:close read-fd)))
      (let ((nlt::*tty-fd* write-fd)
            (nlt::*tty-stream* nil)
            (nlt::*original-termios* :saved))
        (nlt::forget-terminal)
        (is (null nlt::*tty-fd*) "the descriptor is forgotten")
        (is (null nlt::*tty-stream*) "the stream over it too")
        (is (null nlt::*original-termios*) "and the saved attributes"))
      (is (null (ignore-errors (sb-posix:fstat write-fd)))))))

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
