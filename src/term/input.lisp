;;;; input.lisp --- terminal bytes to messages.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's input.lisp (see NOTICE), carrying the
;;;; nodecode fork's decoder rules: LF is Ctrl+J and never Enter, ESC ESC is
;;;; the alt-modified Escape, ESC followed by a C0 byte is a ctrl+alt chord,
;;;; SGR mouse coordinates are 1-based on the wire and reported as they are.
;;;; The terminal replies the shells never ask for (focus, colour, version,
;;;; mode reports, clipboard reads) are consumed and dropped rather than
;;;; surfaced; the replies they do ask for (the kitty graphics query, the
;;;; truecolor probe, the background query, the colour-scheme report) surface
;;;; as their messages.
;;;;
;;;; READ-KEY takes the stream it reads: the input thread hands it the tty,
;;;; a test hands it a string stream, so the same decode runs in both.

(in-package #:nodecode.term)

(defun read-key (stream &optional eof)
  "One message from the bytes waiting on STREAM, or NIL when none wait; EOF
once STREAM has ended."
  ;; A reply the shell does not consume (a query answer) also answers NIL.
  ;; A terminal ends when it hangs up: its window closed, its ssh dropped, its
  ;; pty's master gone (T-348).
  (nlk:when-let (ch (read-char-no-hang stream nil eof))
    (when (eq ch eof) (return-from read-key eof))
    (let ((code (char-code ch)))
      (cond ((= code 27)
             ;; up to 40 ms for the rest of a sequence; a lone ESC is Escape.
             ;; The window is the clock's, never a count of sleeps: a
             ;; collection can stop every thread for a whole pause, so counting
             ;; sleeps would hold Esc past the window.
             (nlk:wait-until (lambda () (listen stream)) (nlk:deadline-after 1/25))
             (let* ((waiting (listen stream))
                    (char (and waiting (read-char stream nil nil)))
                    (code (and char (char-code char))))
               (cond
                 ((null char) (unless waiting (make-key-press-msg :code :escape)))
                 ((char= char #\[) (parse-csi-sequence stream))
                 ;; SS3 and one byte: arrows, Home, End in application mode,
                 ;; F1-F4 in every mode (an R here is F3, never a report).
                 ((char= char #\O)
                  (let ((final (read-char stream nil nil)))
                    (make-key-press-msg :code (or (%final-byte-key final)
                                                  (and (eql final #\R) :f3)
                                                  :unknown))))
                 ;; OSC: the background query's answer, `11;rgb:R/G/B' with
                 ;; one to four hex digits a channel, is a BACKGROUND-MSG;
                 ;; the replies the shell never asked for are consumed.
                 ((char= char #\])
                  (ppcre:register-groups-bind (r g b)
                      ("^11;rgba?:([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})/([0-9a-fA-F]{1,4})"
                       (%read-until-st stream))
                    (make-background-msg
                     :rgb (mapcar (lambda (hex)
                                    (round (* 255 (parse-integer hex :radix 16))
                                           (1- (expt 16 (length hex)))))
                                  (list r g b)))))
                 ;; DCS: a DECRQSS SGR read-back keeping the probe's 1 2 3, or XTGETTCAP RGB.
                 ((char= char #\P)
                  (let ((payload (%read-until-st stream)))
                    (cond ((and (search "$r" payload) (uiop:string-suffix-p payload "m"))
                           (make-truecolor-msg :ok-p (and (or (search ":1:2:3" payload)
                                                              (search ";1;2;3" payload))
                                                          t)))
                          ((and (uiop:string-prefix-p "1+r" payload) (search "524742" payload))
                           (make-truecolor-msg :ok-p t)))))
                 ;; APC: the kitty graphics reply (G ...) becomes a
                 ;; KITTY-GRAPHICS-MSG, OK when the terminal said so.
                 ((char= char #\_)
                  (let ((payload (%read-until-st stream)))
                    (when (and (plusp (length payload)) (char= (char payload 0) #\G))
                      (make-kitty-graphics-msg :ok-p (and (search ";OK" payload) t)))))
                 ;; Alt + a graphic key.
                 ((graphic-char-p char)
                  (make-key-press-msg :code char :mod +mod-alt+ :text (string char)))
                 ;; ESC DEL: Alt+Backspace.
                 ((= code 127) (make-key-press-msg :code :backspace :mod +mod-alt+))
                 ;; ESC ESC: the alt-modified Escape (and the double-Esc gesture).
                 ((= code 27) (make-key-press-msg :code :escape :mod +mod-alt+))
                 ;; ESC + C0: the legacy encoding of an alt-modified control chord
                 ;; (Alt+Enter is ESC CR, Alt+Tab is ESC HT). Decoded like the bare
                 ;; byte with the alt bit added; falling through to Escape read
                 ;; Alt+Enter as cancel.
                 ((< code 32) (c0-key-press char +mod-alt+))
                 (t (make-key-press-msg :code :escape)))))
            ;; ASCII DEL is what most terminals send for Backspace.
            ((= code 127) (make-key-press-msg :code :backspace))
            ((< code 32) (c0-key-press ch))
            (t (make-key-press-msg :code ch :text (string ch)))))))

(defun ctrl-char-to-key (char)
  "The key a C0 byte stands for: BS is Backspace, HT is Tab, CR is Enter,
every other byte is its letter (LF is #\\j: a terminal in raw mode has
ICRNL off, so Enter sends CR and a bare LF can only be the Ctrl+J chord,
which is exactly the distinction a composer binds for newline-not-submit;
vaxis's parseGround decodes 0x0a the same way)."
  (case (char-code char)
    (8 :backspace)
    (9 :tab)
    (13 :enter)
    (otherwise (code-char (+ (char-code char) 96)))))

(defun c0-key-press (char &optional (mod 0) &aux (key (ctrl-char-to-key char)))
  "The key press C0 byte CHAR stands for: its key, MOD added to the modifier
that key carries."
  ;; Enter and Tab carry none: outside the kitty protocol a terminal has no
  ;; way to say Ctrl+Enter, so the ctrl bit on their bytes is an artifact of
  ;; the ASCII encoding, not a gesture (vaxis reports them bare). Backspace
  ;; (0x08, distinct from DEL) and every letter chord keep their real ctrl
  ;; bit.
  (make-key-press-msg :code key :mod (logior mod (if (member key '(:enter :tab)) 0 +mod-ctrl+))))

(defun %modifier-mask (parameter)
  "The bitmask a CSI modifier PARAMETER carries, 1-based (2 shift, 3 alt,
5 ctrl, ...): kitty's form, which xterm's modified keys share."
  (ldb (byte 8 0) (1- parameter)))

(defun %final-byte-key (char)
  "The cursor or function key a CSI or SS3 final byte names, or NIL when
CHAR names none."
  ;; One table for the three decoders that read it: the bare CSI branch, the
  ;; modified-arrow branch, and READ-KEY's SS3 branch — a key added here
  ;; reaches all three instead of one. F3's R is not here: after CSI it is
  ;; also a cursor position report, so F3 is read from 13~ and SS3 R.
  (case char
    (#\A :up) (#\B :down) (#\C :right)
    (#\D :left) (#\H :home) (#\F :end)
    (#\P :f1) (#\Q :f2) (#\S :f4)))

(defun %read-parameters (stream first-digit &aux (groups '())
                                                 (group '())
                                                 (current first-digit)
                                                 (final nil))
  "The CSI parameters waiting on STREAM, as (values GROUPS FINAL): one
group per ';'-separated parameter, each holding its ':'-separated
sub-values oldest first, and FINAL the byte that closed them (NIL when the
stream ran out)."
  ;; FIRST-DIGIT is the digit the caller already consumed. One reader for the
  ;; two decoders that speak this grammar: the CSI key branch and the SGR
  ;; mouse branch.
  (flet ((close-group ()
           (push current group)
           (push (nreverse group) groups)
           (setf group '() current 0)))
    (loop for c = (read-char stream nil nil)
          while c
          do (cond ((digit-char-p c)
                    (setf current (+ (* current 10) (digit-char-p c))))
                   ((char= c #\;) (close-group))
                   ((char= c #\:) (push current group) (setf current 0))
                   (t (close-group) (setf final c) (return)))))
  (values (nreverse groups) final))

(defun parse-csi-sequence (stream &aux (ch (read-char stream nil nil)))
  "The message for ESC [ and what follows on STREAM: SGR mouse, arrows,
Home/End, backtab, the numbered keys (Delete, Insert, PgUp, PgDn, F1-F12),
modified arrows, kitty CSI-u keys, and bracketed paste."
  (cond
    ((eql ch #\<) (parse-mouse-sequence stream))
    ;; Backtab is a CSI final byte too, but it is not a cursor key.
    ((eql ch #\Z) (make-key-press-msg :code :tab :mod +mod-shift+))
    ((%final-byte-key ch) (make-key-press-msg :code (%final-byte-key ch)))
    ;; Focus reports and query answers (CSI I, CSI O, CSI ? ...): the
    ;; shell enables none of them; consumed. One CSI ? reply is asked for:
    ;; DEC 2031's colour-scheme report, ? 997 ; 1 n dark and ; 2 n light,
    ;; which ENTER-MODES switches on.
    ((member ch '(#\I #\O)) nil)
    ((eql ch #\?)
     (let ((reply (with-output-to-string (out)
                    (loop for c = (read-char stream nil nil)
                          while c
                          do (write-char c out)
                          until (alpha-char-p c)))))
       (ppcre:register-groups-bind (scheme) ("^997;([12])n$" reply)
         (make-color-scheme-msg :scheme (if (string= scheme "1") :dark :light)))))
    ((and ch (digit-char-p ch))
     (nlk:bind (((groups term) (%read-parameters stream (digit-char-p ch)))
                (params (mapcar #'first groups))
                (key (case (first params)
                       (2 :insert) (3 :delete) (5 :page-up) (6 :page-down)
                       (11 :f1) (12 :f2) (13 :f3) (14 :f4) (15 :f5) (17 :f6)
                       (18 :f7) (19 :f8) (20 :f9) (21 :f10) (23 :f11) (24 :f12))))
       (cond
         ((eql term #\u) (parse-kitty-key-sequence groups))
         ;; CSI 1 ; mod key, and kitty's bare CSI 1 key for F1-F4.
         ((and (%final-byte-key term)
               (<= 1 (length params) 2)
               (= (first params) 1))
          (make-key-press-msg :code (%final-byte-key term)
                              :mod (%modifier-mask (or (second params) 1))))
         ((and (eql term #\~) key)
          (make-key-press-msg :code key
                              :mod (if (= (length params) 2) (%modifier-mask (second params)) 0)))
         ((and (eql term #\~) (eql (first params) 200))
          (make-paste-msg :text (%read-until stream (format nil "~C[201~~" #\Escape))))
         (t (make-key-press-msg :code :unknown)))))
    (t
     (loop while (listen stream) do (read-char-no-hang stream nil nil))
     (make-key-press-msg :code :unknown))))

;;; --- the kitty keyboard protocol -----------------------------------------

(defun parse-kitty-key-sequence (groups)
  "The message for CSI key[:shifted[:base]] ; modifiers[:event] ; text u,
GROUPS being the parameter groups."
  ;; A release (event 3) answers NIL: the shell never asks for event types, so
  ;; one can only be noise.
  (let* ((mod-group (second groups))
         (text-group (third groups))
         (codepoint (or (first (first groups)) 0))
         ;; Each run: its first codepoint, then the keys numbered consecutively from it.
         (code (loop for (start . keys)
                       in '((9 :tab) (13 :enter) (27 :escape) (127 :backspace)
                            (57344 :escape :enter :tab :backspace :insert :delete :left :right
                             :up :down :page-up :page-down :home :end :caps-lock :scroll-lock
                             :num-lock :print-screen :pause :menu :f1 :f2 :f3 :f4 :f5 :f6 :f7
                             :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20)
                            (57399 :kp-0 :kp-1 :kp-2 :kp-3 :kp-4 :kp-5 :kp-6 :kp-7 :kp-8 :kp-9
                             :kp-decimal :kp-divide :kp-multiply :kp-subtract :kp-add
                             :kp-enter :kp-equal)
                            (57441 :left-shift :left-ctrl :left-alt :left-super :left-hyper
                             :left-meta :right-shift :right-ctrl :right-alt :right-super
                             :right-hyper :right-meta))
                     thereis (and (<= start codepoint) (nth (- codepoint start) keys))
                     finally (return (if (<= 32 codepoint #x10FFFF)
                                         (code-char codepoint)
                                         :unknown))))
         (text (cond (text-group
                      (map 'string #'code-char
                           (remove-if-not (lambda (cp) (and (integerp cp) (< 0 cp #x110000)))
                                          text-group)))
                     ((< 31 codepoint 127) (string (code-char codepoint)))
                     (t ""))))
    (unless (eql (second mod-group) 3)
      (make-key-press-msg :code code :mod (%modifier-mask (or (first mod-group) 1)) :text text))))

;;; --- SGR mouse -----------------------------------------------------------

(defun parse-mouse-sequence (stream)
  "The message for ESC [ < Cb ; Cx ; Cy (M or m) read from STREAM."
  ;;
  ;; Cb: bits 0-1 the button, bit 2 shift, bit 3 alt, bit 4 ctrl, bit 5
  ;; motion, bit 6 the wheel (64-67 up/down/left/right), bit 7 the extra
  ;; buttons 8-11. Cx and Cy arrive 1-based and are reported 1-based: SGR
  ;; (DECSET 1006) differs from X10 tracking only in encoding the coordinates
  ;; as decimal parameters instead of bytes offset by 32 — the upper-left cell
  ;; is 1;1 in both. An offset added here shifts every hit-test one column right
  ;; and one row down.
  (nlk:bind (((groups final) (%read-parameters stream 0)) (params (mapcar #'first groups))
             (cb (or (first params) 0)) (x (or (second params) 0)) (y (or (third params) 0))
             (low (logand cb 3)) (extra (logbitp 7 cb))
             (button (nth low (if extra
                                  '(:button-8 :button-9 :button-10 :button-11)
                                  '(:left :middle :right nil))))
             ;; Cb bits 2-4 are shift, alt and ctrl, the mask's own low three.
             (mod (ldb (byte 3 2) cb)))
    (cond
      ;; A payload the stream cut short, or one a stray byte closed, is
      ;; not a mouse report at all.
      ((not (member final '(#\M #\m))) (make-key-press-msg :code :unknown))
      ((and (logbitp 6 cb) (not extra))
       (make-mouse-wheel-msg :x x :y y :mod mod
                             :direction (nth low '(:up :down :left :right))))
      ((logbitp 5 cb) (make-mouse-motion-msg :x x :y y :button button :mod mod))
      ((eql final #\M) (make-mouse-click-msg :x x :y y :button button :mod mod))
      (t (make-mouse-release-msg :x x :y y :button button :mod mod)))))

;;; --- paste, SS3, APC, DCS ------------------------------------------------

(defun %read-until (stream &rest ends &aux (text (make-array 64 :element-type 'character
                                                                  :adjustable t :fill-pointer 0)))
  "The characters on STREAM up to the first of the strings ENDS, that end
consumed and not kept; everything left when none arrives."
  ;; An ESC that does not start an end is kept as text, and an end right
  ;; behind one still ends the read.
  (loop for ch = (read-char stream nil nil)
        while ch
        do (vector-push-extend ch text)
           (nlk:when-let (end (find-if (lambda (end) (uiop:string-suffix-p text end)) ends))
             (decf (fill-pointer text) (length end))
             (return)))
  (coerce text 'simple-string))

(defun %read-until-st (stream)
  "The bytes up to the string terminator (ESC \\ or BEL), the terminator
consumed."
  (%read-until stream (format nil "~C\\" #\Escape) (string #\Bel)))
