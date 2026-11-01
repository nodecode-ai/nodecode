;;;; terminal.lisp --- the tty, raw mode, the DEC modes, the clipboard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's terminal.lisp and the transition emitter of
;;;; its protocol.lisp (see NOTICE), reduced to the modes the shells set:
;;;; the alternate screen (with autowrap off), SGR mouse tracking, bracketed
;;;; paste, the kitty keyboard flags, the cursor, and OSC 52 for the
;;;; clipboard. Every emitter takes the stream it writes; nothing here
;;;; touches *STANDARD-OUTPUT*, which a TUI severs from the terminal.
;;;;
;;;; The lifecycle sets modes and never erases or positions. The primary
;;;; buffer is what ESC[?1049l hands back on exit, so it must reach
;;;; ESC[?1049h untouched: an erase before it would come back as a blank
;;;; screen when the program quits. The renderer paints every cell of the
;;;; first frame itself, so no clear is needed on either buffer.

(in-package #:nodecode.term)

(nlk:access (current view-state) (was view-state))

;;; --- the terminal ----------------------------------------------------------
;;; Two shapes of one contract. On POSIX the terminal is /dev/tty: one
;;; descriptor for termios and the size ioctl, one bidirectional stream for
;;; keys and frames. On Windows it is the console: CONIN$ and CONOUT$, a
;;; mode word on each, and since Windows 10 a virtual-terminal mode on both
;;; that makes the console speak a Unix terminal's bytes — ESC sequences in
;;; for keys and mouse, ESC sequences out for everything the renderer
;;; paints — so the decoder and the painter are the same code on both. SBCL
;;; writes a console handle in UTF-16 (WriteConsoleW), so the output stream
;;; is :utf-16le; the input side is read here, from the console's input
;;; records (see "console input"), because SBCL's reader deletes Enter.
;;; There is no SIGWINCH, so the input loop measures the screen buffer
;;; instead (program.lisp).

(defvar *tty-fd* nil
  "The descriptor the attribute and size calls use: /dev/tty, or CONOUT$
on Windows.")

(defvar *console-input-fd* nil
  "Windows only: the CONIN$ descriptor the input mode is set on.")

(defvar *tty-stream* nil
  "The bidirectional stream over the terminal a running program reads keys
from and paints frames to.")

(defvar *original-termios* nil
  "The terminal attributes before raw mode — the termios, or on Windows the
two console modes and two code pages — restored on exit.")

#+win32
(progn
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode) sb-alien:int
    (handle (sb-alien:signed 64))
    (mode (sb-alien:unsigned 32) :out))
  (sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode) sb-alien:int
    (handle (sb-alien:signed 64))
    (mode (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("GetConsoleCP" %get-console-cp) (sb-alien:unsigned 32))
  (sb-alien:define-alien-routine ("GetConsoleOutputCP" %get-console-output-cp)
      (sb-alien:unsigned 32))
  (sb-alien:define-alien-routine ("SetConsoleCP" %set-console-cp) sb-alien:int
    (cp (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("SetConsoleOutputCP" %set-console-output-cp) sb-alien:int
    (cp (sb-alien:unsigned 32)))
  (sb-alien:define-alien-type nil
    (sb-alien:struct console-screen-buffer-info
      (size-x sb-alien:short) (size-y sb-alien:short)
      (cursor-x sb-alien:short) (cursor-y sb-alien:short)
      (attributes sb-alien:unsigned-short)
      (left sb-alien:short) (top sb-alien:short)
      (right sb-alien:short) (bottom sb-alien:short)
      (max-x sb-alien:short) (max-y sb-alien:short)))
  (sb-alien:define-alien-routine ("GetConsoleScreenBufferInfo" %get-console-screen-buffer-info)
      sb-alien:int
    (handle (sb-alien:signed 64))
    (info (* (sb-alien:struct console-screen-buffer-info))))
  ;; Input mode bits.
  (defconstant +enable-processed-input+ #x1)
  (defconstant +enable-line-input+ #x2)
  (defconstant +enable-echo-input+ #x4)
  (defconstant +enable-window-input+ #x8)
  (defconstant +enable-mouse-input+ #x10)
  (defconstant +enable-quick-edit-mode+ #x40)
  (defconstant +enable-extended-flags+ #x80)
  (defconstant +enable-virtual-terminal-input+ #x200)
  ;; Output mode bits.
  (defconstant +enable-processed-output+ #x1)
  (defconstant +enable-virtual-terminal-processing+ #x4)
  (defconstant +disable-newline-auto-return+ #x8)
  (defconstant +utf-8-code-page+ 65001)

  (defun console-handle (fd)
    (sb-win32::get-osfhandle fd))

  (defun console-mode (fd)
    "FD's console mode word; signals when FD is not a console."
    (multiple-value-bind (ok mode) (%get-console-mode (console-handle fd))
      (when (zerop ok)
        (error "not a console: descriptor ~d" fd))
      mode))

  (sb-alien:define-alien-routine ("GetNumberOfConsoleInputEvents"
                                  %get-number-of-console-input-events)
      sb-alien:int
    (handle (sb-alien:signed 64))
    (count (sb-alien:unsigned 32) :out))
  (sb-alien:define-alien-routine ("ReadConsoleInputW" %read-console-input) sb-alien:int
    (handle (sb-alien:signed 64))
    (records sb-sys:system-area-pointer)
    (length (sb-alien:unsigned 32))
    (read (sb-alien:unsigned 32) :out)))

;;; --- console input ---------------------------------------------------------
;;; SBCL's own console reader cannot carry a raw terminal. It drops every CR
;;; it reads (win32_read_console strips char 13 for the Unix newline), and in
;;; raw mode CR is the whole of Enter, so Enter never arrived; and a read
;;; that held nothing but CRs waits for more, which kept the input thread,
;;; and the exit behind it, until the next key. So the console's input
;;; records are read here: ReadConsoleInputW, only as many as are waiting,
;;; so nothing blocks, and with virtual-terminal input on, every key and
;;; mouse report arrives as key-down records carrying the characters of its
;;; sequence.

(defconstant +console-record-bytes+ 20
  "sizeof (INPUT_RECORD): a WORD event type, two bytes of padding, then the
16-byte event union, whose KEY_EVENT_RECORD is bKeyDown (4), wRepeatCount
(2), wVirtualKeyCode (2), wVirtualScanCode (2), uChar (2) and
dwControlKeyState (4).")

(defun console-records-text (octets count &optional pending)
  "The text COUNT console input records in OCTETS carry => (values TEXT
PENDING)."
  ;; Only key-down key events with a character count: releases, bare modifier
  ;; keys, and mouse, focus, menu and buffer-size records carry nothing. A
  ;; character repeats its record's repeat count. A surrogate pair is one
  ;; character; a high half whose low half has not been read yet comes back as
  ;; PENDING, to be passed in with the next batch.
  (flet ((u16 (at) (logior (aref octets at) (ash (aref octets (1+ at)) 8))))
    (let ((out (make-string-output-stream)))
      (loop for base from 0 below (* count +console-record-bytes+) by +console-record-bytes+
            when (and (= 1 (u16 base))
                      (or (plusp (u16 (+ base 4))) (plusp (u16 (+ base 6)))))
              do (let ((repeat (max 1 (u16 (+ base 8))))
                       (unit (u16 (+ base 14))))
                   (flet ((emit (code)
                            (dotimes (r repeat) (write-char (code-char code) out))))
                     (cond ((zerop unit))
                           ((<= #xD800 unit #xDBFF) (setf pending unit))
                           ((and pending (<= #xDC00 unit #xDFFF))
                            (emit (+ #x10000 (ash (- pending #xD800) 10) (- unit #xDC00)))
                            (setf pending nil))
                           (t (setf pending nil)
                              (emit unit))))))
      (values (get-output-stream-string out) pending))))

#+win32
(progn
  (defclass console-input-stream (sb-gray:fundamental-character-input-stream)
    ((fd :initarg :fd :reader console-input-stream-fd)
     (handle :initarg :handle :reader console-input-stream-handle)
     (text :initform "" :accessor console-input-stream-text)
     (index :initform 0 :accessor console-input-stream-index)
     (pending :initform nil :accessor console-input-stream-pending))
    (:documentation "The console's input records as a character stream: what
READ-KEY reads on Windows, in place of an fd-stream over CONIN$."))

  (defun console-input-buffered-p (stream)
    (< (console-input-stream-index stream)
       (length (console-input-stream-text stream))))

  (defun console-input-fill (stream)
    "Move the key input the console holds into STREAM's buffer without
waiting. => true when a character is buffered."
    (let ((handle (console-input-stream-handle stream)))
      (multiple-value-bind (ok waiting) (%get-number-of-console-input-events handle)
        (when (and (/= ok 0) (plusp waiting))
          (nlk:bind ((wanted (min waiting 256))
                     (octets (make-array (* wanted +console-record-bytes+)
                                         :element-type '(unsigned-byte 8)))
                     ((ok read) (sb-sys:with-pinned-objects (octets)
                                  (%read-console-input handle (sb-sys:vector-sap octets) wanted))))
            (when (and (/= ok 0) (plusp read))
              (multiple-value-bind (text pending)
                  (console-records-text octets read (console-input-stream-pending stream))
                (setf (console-input-stream-pending stream) pending)
                (when (plusp (length text))
                  (setf (console-input-stream-text stream)
                        (concatenate 'string
                                     (subseq (console-input-stream-text stream)
                                             (console-input-stream-index stream))
                                     text)
                        (console-input-stream-index stream) 0))))))))
    (console-input-buffered-p stream))

  (defmethod sb-gray:stream-read-char-no-hang ((stream console-input-stream))
    (when (or (console-input-buffered-p stream) (console-input-fill stream))
      (prog1 (char (console-input-stream-text stream) (console-input-stream-index stream))
        (incf (console-input-stream-index stream)))))

  (defmethod sb-gray:stream-read-char ((stream console-input-stream))
    (loop (nlk:when-let (ch (sb-gray:stream-read-char-no-hang stream)) (return ch))
          (sleep 0.005)))

  (defmethod sb-gray:stream-unread-char ((stream console-input-stream) character)
    (if (plusp (console-input-stream-index stream))
        (decf (console-input-stream-index stream))
        (setf (console-input-stream-text stream)
              (concatenate 'string (string character) (console-input-stream-text stream))))
    nil)

  (defmethod sb-gray:stream-listen ((stream console-input-stream))
    (or (console-input-buffered-p stream) (console-input-fill stream)))

  (defmethod sb-gray:stream-clear-input ((stream console-input-stream))
    (setf (console-input-stream-text stream) ""
          (console-input-stream-index stream) 0)
    nil)

  (defmethod close ((stream console-input-stream) &key abort)
    (declare (ignore abort))
    (when (open-stream-p stream)
      (ignore-errors (sb-posix:close (console-input-stream-fd stream))))
    (call-next-method)))

(defvar *terminal-device* #-win32 "/dev/tty" #+win32 "CONOUT$"
  "What the terminal is opened by: the controlling terminal, or on Windows
the console's output.")

(nlk:define-error no-terminal (error) ()
  (:report "this process has no controlling terminal") (:export)
  (:documentation "The terminal cannot be opened: the process was started with
none (cron, a container run without -t, a shell tool, setsid)."))

(defun terminal-descriptor ()
  "A new descriptor on the terminal; NO-TERMINAL when there is none to open."
  ;; The one place the device is opened: the descriptor, the stream and the
  ;; probe below all come through here, so the failure has one name.
  (handler-case (sb-posix:open *terminal-device* sb-posix:o-rdwr)
    (sb-posix:syscall-error () (error 'no-terminal))))

(defun terminal-present-p ()
  "Whether this process has a terminal to draw on."
  (handler-case (progn (sb-posix:close (terminal-descriptor)) t)
    (no-terminal () nil)))

(defun tty-fd ()
  "A descriptor for the controlling terminal, opened on first use."
  (or *tty-fd* (setf *tty-fd* (terminal-descriptor))))

#+win32
(defun console-input-fd ()
  "The CONIN$ descriptor, opened on first use."
  (or *console-input-fd*
      (setf *console-input-fd* (sb-posix:open "CONIN$" sb-posix:o-rdwr))))

(defun open-tty ()
  "The stream over the terminal, opened on first use."
  ;; Opening the terminal directly keeps the TUI working while the process's
  ;; own stdio are pipes or severed.
  (or *tty-stream*
      (setf *tty-stream*
            #-win32
            (sb-sys:make-fd-stream (terminal-descriptor)
                                   :input t :output t
                                   :element-type 'character
                                   :buffering :line
                                   :external-format :utf-8
                                   ;; A terminal's read and write sides are
                                   ;; independent channels. Without this a
                                   ;; bidirectional fd-stream gets SBCL's
                                   ;; file-oriented SYNCHRONIZE-OUTPUT, which
                                   ;; lets the painting thread RESET-BUFFER
                                   ;; the input buffer under the reading
                                   ;; thread's stale head snapshot; the next
                                   ;; refill dies in SYSTEM-AREA-UB8-COPY.
                                   :dual-channel-p t
                                   :auto-close t
                                   :name "/dev/tty")
            ;; Two handles, two streams: the console's input and output
            ;; sides are different objects, so the independent channels
            ;; the POSIX stream has to ask for come for free here. Input is
            ;; CONSOLE-INPUT-STREAM (above); output is SBCL's fd-stream.
            ;;
            ;; On Windows an fd-stream's descriptor IS a HANDLE: SBCL writes
            ;; through WriteConsoleW only when the handle it holds answers
            ;; GetConsoleMode. So the output stream gets the OS handle behind
            ;; the CRT descriptor _open returns. Handed the CRT number itself
            ;; (3), SBCL took it for a handle, found no console, and fell
            ;; through to WriteFile on whatever object held that value: the
            ;; first frame died with "The handle is invalid" or "The request
            ;; is not supported".
            #+win32
            (make-two-way-stream
             (let ((fd (sb-posix:open "CONIN$" sb-posix:o-rdwr)))
               (make-instance 'console-input-stream :fd fd :handle (console-handle fd)))
             (sb-sys:make-fd-stream (console-handle (sb-posix:open "CONOUT$" sb-posix:o-rdwr))
                                    :output t :element-type 'character
                                    :buffering :line
                                    :external-format :utf-16le
                                    :auto-close t :name "CONOUT$")))))

(defun close-tty ()
  "Close the terminal stream when open."
  (when *tty-stream*
    #-win32 (ignore-errors (close *tty-stream*))
    #+win32 (progn (ignore-errors (close (two-way-stream-input-stream *tty-stream*)))
                   (ignore-errors (close (two-way-stream-output-stream *tty-stream*))))
    (setf *tty-stream* nil)))

(defun forget-terminal ()
  "Drop this process's terminal state — the stream, the descriptors, the
attributes raw mode saved — closing what it owns."
  ;;
  ;; A descriptor is a fact about ONE process. An image that carries the NUMBER
  ;; reopens nothing: the next process calls termios on whatever it happens to
  ;; have at that number. A release built from a terminal dumped *TTY-FD* 10
  ;; (the launch rehearsal reads the size while making the model), and on the
  ;; owner path the gateway's socket lands on 10, so the first launch of the
  ;; day — the one that starts the gateway — died in TCGETATTR with
  ;; "Inappropriate ioctl for device" before it painted; the attach path,
  ;; whose descriptors fall differently, ran on. Runs from SB-EXT:*SAVE-HOOKS*,
  ;; beside the connection pool's flush, so no dumped image carries one.
  (close-tty)
  (dolist (fd (list *tty-fd* *console-input-fd*))
    (when fd (ignore-errors (sb-posix:close fd))))
  (setf *tty-fd* nil *console-input-fd* nil *original-termios* nil)
  (values))

(pushnew 'forget-terminal sb-ext:*save-hooks*)

;;; --- raw mode ------------------------------------------------------------

(define-modify-macro clearf (&rest bits) (lambda (n &rest bits) (logandc2 n (apply #'logior bits)))
  "Clear BITS in the place.")

(defun enter-raw-mode ()
  "Put the terminal in raw mode."
  ;; POSIX: cfmakeraw, keeping OPOST so a
  ;; newline still carries the carriage return — no canonical buffering, no
  ;; echo, no signal keys, no flow control, no CR/NL translation, 8-bit
  ;; characters, one byte at a time. Windows: line editing, echo and Ctrl-C
  ;; processing off and quick-edit off (so the mouse reaches the program) on
  ;; CONIN$, virtual-terminal sequences on both sides, a newline that does not
  ;; also return, and UTF-8 code pages both ways.
  #-win32
  (let* ((fd (tty-fd))
         (raw (sb-posix:tcgetattr fd)))
    (unless *original-termios*
      (setf *original-termios* (sb-posix:tcgetattr fd)))
    (clearf (sb-posix:termios-lflag raw) sb-posix:icanon sb-posix:echo sb-posix:echoe
            sb-posix:echok sb-posix:echonl sb-posix:isig
            sb-posix:iexten)
    (clearf (sb-posix:termios-iflag raw) sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk
            sb-posix:istrip sb-posix:inlcr sb-posix:igncr
            sb-posix:icrnl sb-posix:ixon)
    (clearf (sb-posix:termios-cflag raw) sb-posix:csize sb-posix:parenb)
    (setf (sb-posix:termios-cflag raw) (logior (sb-posix:termios-cflag raw) sb-posix:cs8))
    (let ((cc (sb-posix:termios-cc raw)))
      (setf (aref cc sb-posix:vmin) 1
            (aref cc sb-posix:vtime) 0))
    (sb-posix:tcsetattr fd sb-posix:tcsanow raw))
  #+win32
  (let* ((in (console-input-fd))
         (out (tty-fd))
         (in-mode (console-mode in))
         (out-mode (console-mode out)))
    (unless *original-termios*
      (setf *original-termios*
            (list in-mode out-mode (%get-console-cp) (%get-console-output-cp))))
    (%set-console-mode (console-handle in)
                       (logior (logandc2 in-mode
                                         (logior +enable-processed-input+
                                                 +enable-line-input+
                                                 +enable-echo-input+
                                                 +enable-quick-edit-mode+))
                               +enable-extended-flags+
                               +enable-window-input+
                               +enable-mouse-input+
                               +enable-virtual-terminal-input+))
    (%set-console-mode (console-handle out)
                       (logior out-mode
                               +enable-processed-output+
                               +enable-virtual-terminal-processing+
                               +disable-newline-auto-return+))
    (%set-console-cp +utf-8-code-page+)
    (%set-console-output-cp +utf-8-code-page+)))

(defun exit-raw-mode ()
  "Restore the attributes ENTER-RAW-MODE saved and release the descriptors."
  (when *original-termios*
    #-win32
    (sb-posix:tcsetattr (tty-fd) sb-posix:tcsanow *original-termios*)
    #+win32
    (destructuring-bind (in-mode out-mode in-cp out-cp) *original-termios*
      (%set-console-mode (console-handle (console-input-fd)) in-mode)
      (%set-console-mode (console-handle (tty-fd)) out-mode)
      (%set-console-cp in-cp)
      (%set-console-output-cp out-cp))
    (setf *original-termios* nil)
    (dolist (fd (list *tty-fd* *console-input-fd*))
      (when fd (sb-posix:close fd)))
    (setf *tty-fd* nil *console-input-fd* nil)))

;;; --- the window size -----------------------------------------------------
;;; One ioctl, no subprocess: the SIGWINCH handler asks for the size from
;;; signal context, where a fork+exec (`stty size`) has no business. On
;;; Windows the screen buffer's window rectangle is the same answer.

#-win32
(sb-alien:define-alien-type nil
  (sb-alien:struct winsize
    (rows sb-alien:unsigned-short)
    (cols sb-alien:unsigned-short)
    (xpixel sb-alien:unsigned-short)
    (ypixel sb-alien:unsigned-short)))

(defun window-size (fd)
  "The size of the terminal behind FD as (values WIDTH HEIGHT), from
TIOCGWINSZ — GetConsoleScreenBufferInfo on Windows; NIL when FD is not a
terminal or it reports no size."
  (multiple-value-bind (cols rows)
      (ignore-errors
       #-win32
       (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
         (when (sb-unix:unix-ioctl fd ; TIOCGWINSZ
                                   #+darwin #x40087468 #-darwin #x5413
                                   (sb-alien:alien-sap ws))
           (values (sb-alien:slot ws 'cols) (sb-alien:slot ws 'rows))))
       #+win32
       (sb-alien:with-alien ((info (sb-alien:struct console-screen-buffer-info)))
         (unless (zerop (%get-console-screen-buffer-info (console-handle fd)
                                                         (sb-alien:addr info)))
           (values (1+ (- (sb-alien:slot info 'right) (sb-alien:slot info 'left)))
                   (1+ (- (sb-alien:slot info 'bottom) (sb-alien:slot info 'top)))))))
    (when (and cols rows (plusp cols) (plusp rows))
      (values cols rows))))

(defun terminal-size (&optional fd)
  "The terminal's size as (values WIDTH HEIGHT) in cells, from TIOCGWINSZ
on FD (the terminal by default); 80 by 24 when there is no terminal or it
reports no size."
  ;; Both values always: a caller binds them as a pair, and OR over the ioctl
  ;; form once dropped the height.
  (multiple-value-bind (width height)
      (window-size (or fd (ignore-errors (tty-fd))))
    (values (or width 80) (or height 24))))

;;; --- the DEC modes -------------------------------------------------------

(defmacro define-escape (spec sequences &optional documentation)
  "Define the function of a STREAM that SPEC names -- NAME, or (NAME :FLUSH T)
-- writing SEQUENCES, strings each led by ESC, and under FLUSH forcing them out."
  (destructuring-bind (name &key flush) (alexandria:ensure-list spec)
    `(defun ,name (stream)
       ,@(and documentation (list documentation))
       (format stream "~a" ,(format nil "~{~C~a~}" (mapcan (lambda (s) (list #\Escape s))
                                                             sequences)))
       ,@(and flush '((force-output stream))))))

(define-escape enter-alt-screen ("[?1049h" "[?7l")
  "The alternate screen, with autowrap (DECAWM) off: every row is placed
absolutely, so a wrap could only fire on a glyph width the terminal
disagrees on, and a wrap at the bottom-right cell would scroll and shift
the whole frame.")

(define-escape exit-alt-screen ("[?1049l" "[?7h")
  "Back to the primary screen, autowrap restored for the shell.")

(defun enable-mouse (stream mode)
  "SGR mouse tracking: :CELL-MOTION reports motion with a button held,
:ALL-MOTION reports every motion."
  ;; Button-event tracking (1002) goes first in both modes. Termux accepts
  ;; any-event tracking (1003) and drops it, so 1003 alone left it untracked:
  ;; in the alternate screen it sends a wheel turn as Up/Down, and the prompt
  ;; walked its history while the transcript stood still (#89). A terminal
  ;; that keeps one mouse mode -- xterm, tmux -- ends on the last one set.
  (when mode
    (format stream "~C[?1002h" #\Escape)
    (ecase mode
      (:cell-motion)
      (:all-motion (format stream "~C[?1003h" #\Escape)))
    (format stream "~C[?1006h" #\Escape)))

(define-escape disable-mouse ("[?1002l" "[?1003l" "[?1006l"))

(defun set-window-title (stream text)
  "OSC 2: the window title the terminal shows, TEXT as it was given."
  ;; A terminal that does not take it ignores the sequence; a multiplexer
  ;; passes it through to the window it owns. Nothing is escaped but the
  ;; terminator — a control character in TEXT is the caller's to have kept
  ;; out.
  (format stream "~C]2;~A~C\\" #\Escape text #\Escape))

(define-escape reset-window-title ("]2;" "\\")
  "Give the window back to whatever names it — an empty OSC 2, which every
terminal reads as `no title of mine'; the shell repaints its own on the
next prompt.")

(define-escape push-kitty-keyboard ("[>1u")
  "The kitty keyboard's disambiguation, flag 1: the one enhancement a shell asks for.")

(define-escape pop-kitty-keyboard ("[<u"))

;;; The first frame then takes that screen for
;;; this shell rather than entering the alternate screen again, which clears
;;; it and would leave a blank terminal for as long as the paint takes. Set
;;; once, by the launch that knows it is a successor (tui/entry.lisp,
;;; *HANDOVER-LAUNCH*).
(defvar *inherit-screen* nil
  "T in an image painting a screen it inherited: a switch handed this
process the terminal a TUI already owns — the alternate screen entered, a
frame on it, the cursor hidden.")

(defun release-inherited-modes (&optional stream)
  "Give back what an image that ran before this one may have switched on:
the alternate screen, mouse tracking, bracketed paste, the kitty keyboard
flags, the cursor."
  ;; Written by a successor that turns out unable to serve — it is the only
  ;; process left, and nothing in this image knows what its predecessor had
  ;; switched on, so the release is unconditional.
  (nlk:when-let (stream (or stream (ignore-errors (open-tty))))
    (dolist (undo '(exit-alt-screen disable-mouse pop-kitty-keyboard exit-modes))
      (ignore-errors (funcall undo stream)))))

;;; --- the truecolor probe --------------------------------------------------

(define-escape (request-truecolor :flush t)
    ("[0m" "[38:2:1:2:3m" "P$qm" "\\" "[0m" "P+q524742" "\\")
  ;; Two questions ride one write, either answer suffices: a sentinel RGB
  ;; foreground (1 2 3) is set and read back with DECRQSS — a terminal that
  ;; kept it honors truecolor — and XTGETTCAP asks for the RGB capability
  ;; (524742 is "RGB" in hex). The decoder turns either reply into a
  ;; TRUECOLOR-MSG; silence keeps the environment's guess. The sentinel is
  ;; reset in the same write, so no painted cell ever wears it.
  "Ask the terminal itself whether its SGR honors 24-bit color — the
environment cannot say for sure (ssh forwards TERM but strips COLORTERM).")

(define-escape (request-background :flush t) ("]11;?" "\\")
  ;; The decoder turns the reply into a BACKGROUND-MSG; a terminal that does
  ;; not know the question says nothing.
  "Ask the terminal the colour of its own background (OSC 11): what a theme
that leaves the canvas to the terminal stands on.")

(define-escape (enter-modes :flush t) ("[?25l" "[?2004h" "[?2031h")
  ;; Modes only: nothing here erases or positions. The alternate screen and
  ;; mouse tracking are the view-state's, switched by the frames. A terminal
  ;; that does not know 2031 ignores it, as DECSET does any mode it lacks.
  "The modes a session runs under: cursor hidden, bracketed paste, and the
colour-scheme report (DEC 2031): a terminal that follows the desktop between
light and dark says when it turns, and the shell asks its background again.")

(define-escape (exit-modes :flush t) ("[?2031l" "[?2004l" "[?25h")
  "Undo ENTER-MODES in reverse: the colour-scheme report and bracketed paste
off, the cursor shown last.")

(defun apply-terminal-transitions (previous current stream &aux (was (or previous (make-view '()))))
  "Write to STREAM the mode changes between view-states PREVIOUS (NIL for
the first frame) and CURRENT: the alternate screen, the mouse mode, the
cursor shape when a cursor appears, the kitty keyboard flags pushed when
they come and popped when they go away, the window title when it moved."
  ;; A title is written on the frame that changed it and on no other:
  ;; the sequence is cheap but a terminal that flashes its titlebar on
  ;; every write would do so sixty times a second.
  (when (and current.title (not (equal was.title current.title)))
    (set-window-title stream current.title))
  (when (and was.title (not current.title))
    (reset-window-title stream))
  ;; A successor that inherited a live screen is already in the alternate
  ;; screen, with its predecessor's frame on it: entering again would
  ;; clear that frame.
  (when (and current.alt-screen (not was.alt-screen)
             (not (and *inherit-screen* (null previous))))
    (enter-alt-screen stream))
  (when (and was.alt-screen (not current.alt-screen)) (exit-alt-screen stream))
  (unless (eq was.mouse-mode current.mouse-mode)
    (when was.mouse-mode (disable-mouse stream))
    (enable-mouse stream current.mouse-mode))
  ;; The steady block, the same cursor the Zig TUI shows.
  (when (and current.cursor (not was.cursor)) (format stream "~C[2 q" #\Escape))
  (when (and current.keyboard-enhancements (not was.keyboard-enhancements))
    (push-kitty-keyboard stream))
  (when (and was.keyboard-enhancements (not current.keyboard-enhancements))
    (pop-kitty-keyboard stream)))

;;; --- the clipboard -------------------------------------------------------
;;; The external helper chain first (wl-copy, xclip, xsel: the same chain
;;; the Zig TUI runs), then OSC 52 through the terminal, wrapped for a
;;; tmux or screen the terminal sits behind.

(defvar *clipboard-use-command* #-win32 t #+win32 nil
  "Whether clipboard writes try the helper chain before OSC 52. NIL forces
OSC 52 — the Windows default: the helper chain is Wayland's and X11's, and
Windows Terminal takes OSC 52.")

(defun %wrap-passthrough (sequence &optional (multiplexer
                                             (cond ((uiop:getenv "TMUX") :tmux)
                                                   ((uiop:string-prefix-p
                                                     "screen" (uiop:getenv "TERM"))
                                                    :screen))))
  "SEQUENCE wrapped so MULTIPLEXER passes it to the outer terminal: tmux
takes DCS tmux; with every ESC doubled, screen takes 256-byte DCS chunks."
  ;; MULTIPLEXER defaults to the one this process runs under: :TMUX under
  ;; tmux, :SCREEN under GNU screen, else NIL.
  (case multiplexer
    (:tmux
     (format nil "~CPtmux;~A~C\\" #\Escape
             (ppcre:regex-replace-all (string #\Escape) sequence
                                      (make-string 2 :initial-element #\Escape))
             #\Escape))
    (:screen
     (with-output-to-string (s)
       (loop for start from 0 below (length sequence) by 256
             do (format s "~CP~A~C\\" #\Escape
                        (subseq sequence start (min (length sequence) (+ start 256)))
                        #\Escape))))
    (t sequence)))

(defun %osc52-sequence (content)
  "The OSC 52 write of CONTENT to the clipboard selection."
  ;; Base64 of the UTF-8 octets: CL-BASE64:STRING-TO-BASE64-STRING takes each
  ;; character's code as one byte, so a dot or a bullet pasted as Latin-1.
  (format nil "~C]52;c;~A~C\\" #\Escape
          (cl-base64:usb8-array-to-base64-string
           (sb-ext:string-to-octets content :external-format :utf-8))
          #\Escape))

(defun copy-text-command (content)
  "Try the helper chain with CONTENT: the name of the helper that accepted it
(\"wl-copy\", \"xclip\" ...), or NIL when none did."
  ;; Each helper an argv fed the text on stdin: macOS's pasteboard, else
  ;; Wayland's and X11's. A helper with no display to reach exits nonzero (a
  ;; wl-copy outside a Wayland session, an xclip with no X server), so a host
  ;; that has the program but not its display answers NIL here, never T.
  (some (lambda (argv) (and (nlk:exits-zero-p argv :input content) (first argv)))
        #+darwin '(("pbcopy"))
        #-darwin '(("wl-copy")
                   ("xclip" "-selection" "clipboard")
                   ("xsel" "--clipboard" "--input"))))

(defun copy-to-clipboard (content &key stream
                                       (multiplexer (cond ((uiop:getenv "TMUX") :tmux)
                                                          ((uiop:string-prefix-p
                                                            "screen" (uiop:getenv "TERM"))
                                                           :screen))))
  "Write CONTENT to the system clipboard through OSC 52 on STREAM, wrapped
for the MULTIPLEXER this process runs under. Answers :OSC52: the sequence
was written, and whether the terminal honours it is the terminal's to say."
  ;; Behind tmux the bare sequence goes first, then the wrapped one: tmux takes
  ;; the bare one as its own clipboard write (set-clipboard on) and passes the
  ;; wrapped one to the outer terminal only under allow-passthrough, and it
  ;; ships both off, so either alone is dropped by a default tmux without a
  ;; word. The two land wherever either is on; with both on, the clipboard is
  ;; set twice to the same text.
  (let ((sequence (%osc52-sequence content)))
    (when (eq multiplexer :tmux) (write-string sequence stream))
    (write-string (%wrap-passthrough sequence multiplexer) stream))
  (force-output stream)
  :osc52)
