;;;; input-test.lisp --- the decoder: terminal bytes to messages.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from cl-tuition's tests/test-input.lisp and the mouse cases of
;;;; tests/test-terminal-caps.lisp (Copyright (C) 2025 Anthony Green, MIT),
;;;; plus the cases the fork never had: bracketed paste and the replies the
;;;; shell never asks for. Every case drives real bytes through READ-KEY —
;;;; a hand-built message proves nothing about a decoder.

(in-package #:nodecode.test)

(nlk:access (msg nlt::mouse-event) (press nlt::mouse-motion-msg) (release nlt::mouse-motion-msg))

(defun key-from (&rest bytes)
  "The message the decoder answers for the terminal BYTES."
  (nlt:read-key (make-string-input-stream (map 'string #'code-char bytes))))

(defun key-from-string (string)
  (nlt:read-key (make-string-input-stream string)))

(defun csi (tail)
  (format nil "~C[~A" #\Escape tail))

(defun key-is (msg code &key mod (text nil text-p))
  "MSG is a key press of CODE carrying every modifier in MOD."
  (and (nlt:key-press-msg-p msg)
       (eql code (nlt:key-event-code msg))
       (or (null mod) (= mod (logand mod (nlt:key-event-mod msg))))
       (or (not text-p) (equal text (nlt:key-event-text msg)))))

(deftest decoder-names-the-c0-bytes ()
  (is-table (test key byte) (test key (nlt:ctrl-char-to-key (code-char byte)))
    (eq :backspace 8 "BS is Backspace") (eq :tab 9 "HT is Tab") (eq :enter 13 "CR is Enter")
    (char= #\j 10) (char= #\a 1 "Ctrl+A is a") (char= #\c 3 "Ctrl+C is c")
    (char= #\z 26 "Ctrl+Z is z")))

(deftest decoder-reads-plain-and-control-keys ()
  (is (key-is (key-from 97) #\a :text "a") "a graphic byte is its key with its text")
  (is (key-is (key-from 127) :backspace) "DEL is Backspace")
  (is (key-is (key-from 1) #\a :mod nlt:+mod-ctrl+) "a C0 byte is its letter with ctrl")
  (is (key-is (key-from 10) #\j :mod nlt:+mod-ctrl+) "LF is Ctrl+J, never Enter")
  (is (key-is (key-from 8) :backspace :mod nlt:+mod-ctrl+))
  (is (and (key-is (key-from 13) :enter) (zerop (nlt:key-event-mod (key-from 13)))))
  (is (and (key-is (key-from 9) :tab) (zerop (nlt:key-event-mod (key-from 9)))))
  (is (null (key-from)) "no bytes, no message"))

(deftest decoder-reads-alt-chords ()
  (is (and (key-is (key-from 27 13) :enter :mod nlt:+mod-alt+)
           (not (nlt:mod-contains (nlt:key-event-mod (key-from 27 13)) nlt:+mod-ctrl+))))
  (is (and (key-is (key-from 27 9) :tab :mod nlt:+mod-alt+)
           (not (nlt:mod-contains (nlt:key-event-mod (key-from 27 9)) nlt:+mod-ctrl+))))
  (is (key-is (key-from 27 27) :escape :mod nlt:+mod-alt+))
  (is (key-is (key-from 27 1) #\a :mod (logior nlt:+mod-ctrl+ nlt:+mod-alt+)))
  (is (key-is (key-from 27 120) #\x :mod nlt:+mod-alt+) "ESC x is Alt+x")
  (is (key-is (key-from 27 127) :backspace :mod nlt:+mod-alt+) "ESC DEL is Alt+Backspace")
  (is (key-is (key-from 27) :escape) "a lone ESC is the Escape key"))

(defclass stalling-tty (sb-gray:fundamental-character-input-stream)
  ((inner :initarg :inner) (looks :initform 0))
  (:documentation "A tty whose every LISTEN takes 50 ms: the decoder's thread
stopped by a collection, as an image an eval keeps collecting stops it."))

(defmethod sb-gray:stream-read-char ((stream stalling-tty))
  (read-char (slot-value stream 'inner) nil :eof))

(defmethod sb-gray:stream-listen ((stream stalling-tty))
  (incf (slot-value stream 'looks))
  (sleep 0.05)
  (listen (slot-value stream 'inner)))

(deftest decoder-times-a-lone-esc-by-the-clock (let ((tty (make-instance 'stalling-tty :inner (make-string-input-stream (string #\Escape))))))
  "The wait for the rest of a sequence is 40 ms of the clock: stalled past
it, the decoder looks once more and answers Escape."
  (is (key-is (nlt:read-key tty) :escape) "a lone ESC is still Escape")
  (is (= 2 (slot-value tty 'looks)) "one look in the window and one past it, not nine"))

(deftest decoder-reads-csi-keys ()
  (is-table (sequence key) (key-is (key-from-string (csi sequence)) key)
    ("A" :up "CSI A") ("B" :down "CSI B") ("C" :right "CSI C") ("D" :left "CSI D")
    ("H" :home "CSI H") ("F" :end "CSI F")
    ("2~" :insert "CSI 2~") ("3~" :delete "CSI 3~") ("5~" :page-up "CSI 5~")
    ("6~" :page-down "CSI 6~"))
  (is (key-is (key-from-string (csi "Z")) :tab :mod nlt:+mod-shift+) "CSI Z is backtab")
  (is (key-is (key-from-string (csi "1;2A")) :up :mod nlt:+mod-shift+) "CSI 1;2A is Shift+Up")
  (is (key-is (key-from-string (csi "1;5C")) :right :mod nlt:+mod-ctrl+) "CSI 1;5C is Ctrl+Right")
  (is (key-is (key-from-string (csi "1;3D")) :left :mod nlt:+mod-alt+) "CSI 1;3D is Alt+Left")
  (is (key-is (key-from-string (csi "3;5~")) :delete :mod nlt:+mod-ctrl+) "CSI 3;5~ is Ctrl+Delete")
  (is (key-is (key-from-string (csi "99~")) :unknown) "an unnamed ~ key is :unknown")
  (is (key-is (key-from-string (csi "x")) :unknown) "an unnamed final byte is :unknown"))

(deftest decoder-reads-ss3-keys ()
  (is-table (control key) (key-is (key-from-string (format nil control #\Escape)) key)
    ("~COA" :up "SS3 A") ("~COH" :home "SS3 H") ("~COF" :end "SS3 F")
    ("~COP" :f1 "SS3 P") ("~COQ" :f2 "SS3 Q, what tmux and xterm send for F2") ("~COR" :f3 "SS3 R")
    ("~COS" :f4 "SS3 S")))

(deftest decoder-reads-function-keys ()
  (is-table (sequence key) (key-is (key-from-string (csi sequence)) key)
    ("P" :f1 "CSI P") ("Q" :f2 "CSI Q") ("S" :f4 "CSI S") ("1Q" :f2 "CSI 1Q, kitty's F2")
    ("11~" :f1) ("12~" :f2 "CSI 12~") ("13~" :f3 "kitty's F3") ("15~" :f5) ("17~" :f6) ("21~" :f10)
    ("24~" :f12))
  (is (key-is (key-from-string (csi "1;2Q")) :f2 :mod nlt:+mod-shift+) "CSI 1;2Q is Shift+F2")
  (is (key-is (key-from-string (csi "12;5~")) :f2 :mod nlt:+mod-ctrl+) "CSI 12;5~ is Ctrl+F2")
  (is (not (key-is (key-from-string (csi "1;1R")) :f3)) "a cursor report is never F3"))

(deftest decoder-reads-kitty-keys ()
  (is (key-is (key-from-string (csi "97u")) #\a :text "a") "CSI 97u is a")
  (is (key-is (key-from-string (csi "97;5u")) #\a :mod nlt:+mod-ctrl+) "CSI 97;5u is Ctrl+a")
  (is (null (key-from-string (csi "97;5:3u"))))
  (is (key-is (key-from-string (csi "97;5:2u")) #\a :mod nlt:+mod-ctrl+) "a repeat is a press")
  (is (key-is (key-from-string (csi "13;2u")) :enter :mod nlt:+mod-shift+))
  (is (key-is (key-from-string (csi "106;5u")) #\j :mod nlt:+mod-ctrl+) "CSI 106;5u is Ctrl+J")
  (is (key-is (key-from-string (csi "27u")) :escape) "CSI 27u is Escape")
  (is (key-is (key-from-string (csi "57352u")) :up) "CSI 57352u is Up")
  (is (key-is (key-from-string (csi "97:65;2u")) #\a :mod nlt:+mod-shift+))
  (is (key-is (key-from-string (csi "97;1;233u")) #\a :text (string (code-char 233)))))

(deftest decoder-consumes-the-replies-the-shell-never-asks-for ()
  (let ((stream (make-string-input-stream
                 (concatenate 'string
                              (csi "I") (csi "O") (csi "?1u")
                              (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Escape #\Escape)
                              (format nil "~CP>|xterm(370)~C\\" #\Escape #\Escape)
                              "z"))))
    (is (null (nlt:read-key stream)) "a focus-in report is nothing")
    (is (null (nlt:read-key stream)) "a focus-out report is nothing")
    (is (null (nlt:read-key stream)) "a kitty keyboard query answer is nothing")
    ;; The background report is one the shell asks for (NLT:REQUEST-BACKGROUND):
    ;; it arrives as the colour, never as a key.
    (is (equal '(0 0 0) (nlt:background-msg-rgb (nlt:read-key stream))) "a background report is its colour")
    (is (null (nlt:read-key stream)) "a DCS version report is nothing")
    (is (key-is (nlt:read-key stream) #\z) "and the key after them still decodes")))

(deftest decoder-reads-the-colour-scheme-report ()
  ;; DEC 2031, which NLT:ENTER-MODES switches on: a terminal that follows the
  ;; desktop between light and dark says which it turned to.
  (is-table (reply scheme) (eq scheme (nlt:color-scheme-msg-scheme (key-from-string (csi reply))))
    ("?997;1n" :dark "997;1 is dark")
    ("?997;2n" :light "997;2 is light"))
  (let ((stream (make-string-input-stream
                 (concatenate 'string (csi "?997;3n") (csi "?2031;1$y") "z"))))
    (is (null (nlt:read-key stream)) "a scheme the report does not name is nothing")
    (is (null (nlt:read-key stream)) "a mode report is still nothing")
    (is (key-is (nlt:read-key stream) #\z) "and the key after them still decodes")))

(deftest decoder-reads-one-bracketed-paste ()
  (flet ((paste (text) (key-from-string (concatenate 'string (csi "200~") text (csi "201~")))))
    (is (nlt:paste-msg-p (paste (format nil "hello~%world"))) "the paste arrives as one message")
    (is-table (text) (equal text (nlt:paste-msg-text (paste text)))
      ((format nil "hello~%world"))
      ((concatenate 'string "a" (csi "1m") "b"))
      ("" "an empty paste is an empty text"))))

(defun mouse-from (payload)
  "The message for ESC [ < PAYLOAD."
  (nlt:parse-mouse-sequence (make-string-input-stream payload)))

(deftest decoder-reads-sgr-mouse-at-one-based-cells ()
  ;; SGR coordinates are already 1-based cells: the wire values report the
  ;; cell verbatim, with no offset on either axis.
  (let ((msg (mouse-from "64;10;5M")))
    (is-shape msg (nlt:mouse-wheel-msg-p is "cb 64 is the wheel")
      (nlt:mouse-wheel-direction eq :up "up") (.x = 10 "x verbatim") (.y = 5 "y verbatim")))
  (let ((msg (mouse-from "0;1;1M")))
    (is (and (= 1 msg.x) (= 1 msg.y))))
  (let ((msg (mouse-from "81;3;4M")))
    (is (and (nlt:mouse-wheel-msg-p msg) (eq :down (nlt:mouse-wheel-direction msg))
             (nlt:mod-contains (nlt:mouse-event-mod msg) nlt:+mod-ctrl+))))
  (is (eq :left (nlt:mouse-wheel-direction (mouse-from "66;1;1M"))) "cb 66 is wheel left")
  (is (eq :right (nlt:mouse-wheel-direction (mouse-from "67;1;1M"))) "cb 67 is wheel right")
  (let ((press (mouse-from "0;2;3M"))
        (release (mouse-from "2;2;3m")))
    (is (and (nlt:mouse-click-msg-p press) (eq :left press.button)))
    (is (and (nlt:mouse-release-msg-p release) (eq :right release.button))))
  (let ((msg (mouse-from "35;7;8M")))
    (is (and (nlt:mouse-motion-msg-p msg) (null (nlt:mouse-event-button msg))
             (= 7 msg.x) (= 8 msg.y))))
  (let ((msg (mouse-from "36;7;8M")))
    (is (and (nlt:mouse-motion-msg-p msg) (eq :left (nlt:mouse-event-button msg))
             (nlt:mod-contains (nlt:mouse-event-mod msg) nlt:+mod-shift+))))
  (is (eq :button-8 (nlt:mouse-event-button (mouse-from "128;1;1M"))) "cb 128 is button 8")
  (is (eq :button-9 (nlt:mouse-event-button (mouse-from "129;1;1M"))) "cb 129 is button 9")
  (let ((msg (key-from-string (csi "<35;10;20M"))))
    (is (and (nlt:mouse-motion-msg-p msg) (= 10 msg.x) (= 20 msg.y)))))

(deftest decoder-reads-the-truecolor-probe-replies ()
  (let ((msg (key-from-string (format nil "~CP1$r0;38:2::1:2:3m~C\\" #\Escape #\Escape))))
    (is (nlt:truecolor-msg-p msg) "a DECRQSS SGR read-back is the truecolor message")
    (is (nlt:truecolor-msg-ok-p msg) "the colon-form sentinel read back means honored"))
  (is-table (control ok) (let ((msg (key-from-string (format nil control #\Escape #\Escape))))
                           (and (nlt:truecolor-msg-p msg) (ok (nlt:truecolor-msg-ok-p msg))))
    ("~CP1$r0;38;2;1;2;3m~C\\" identity) ("~CP1$r0m~C\\" null)
    ("~CP1+r524742~C\\" identity) ("~CP1+r524742=38~C\\" identity))
  (is-table (control) (null (key-from-string (format nil control #\Escape #\Escape)))
    ("~CP0+r524742~C\\") ("~CP0$r~C\\") ("~CP>|xterm(370)~C\\")))

(deftest decoder-reads-the-kitty-graphics-reply ()
  (let ((msg (key-from-string (format nil "~C_Gi=1;OK~C\\" #\Escape #\Escape))))
    (is (nlt:kitty-graphics-msg-p msg) "the APC reply is the graphics message")
    (is (nlt:kitty-graphics-msg-ok-p msg) "OK means supported"))
  (let ((msg (key-from-string (format nil "~C_Gi=1;ENOTSUPPORTED~C\\" #\Escape #\Escape))))
    (is (and (nlt:kitty-graphics-msg-p msg) (null (nlt:kitty-graphics-msg-ok-p msg)))))
  (is (null (key-from-string (format nil "~C_Xother~C\\" #\Escape #\Escape)))))
