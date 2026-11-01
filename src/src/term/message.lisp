;;;; message.lisp --- what the program loop moves: messages, commands, the view.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's protocol.lisp (see NOTICE), reduced to the
;;;; vocabulary the shells speak: the model protocol (INIT, UPDATE, VIEW),
;;;; key, mouse, paste, window-size, tick and quit messages, the kitty
;;;; graphics reply, the terminal-io request a command uses to write escape
;;;; bytes on the loop thread, the declarative view-state, and the two
;;;; command helpers. Terminal queries, focus reports, key releases, the
;;;; progress bar and the window title had no consumer and are gone.
;;;;
;;;; The Elm shape: a model, INIT answering an optional first command, UPDATE
;;;; folding one message into the model and answering (values model cmd),
;;;; VIEW answering a view-state. A command is a thunk the worker thread
;;;; funcalls; a message it answers goes back through SEND. The generics are
;;;; CLOS on purpose — the evolved package imports them and the organism
;;;; redefines its own UI methods at run time.

(in-package #:nodecode.term)

;;; --- the model protocol --------------------------------------------------

(defgeneric init (model)
  (:documentation "Initialize MODEL and answer an optional first command."))

(defgeneric update (model msg)
  ;; QUIET true says the fold changed nothing a frame would show -- a clock
  ;; tick that found nothing moving -- and the loop then owes the message no
  ;; paint.
  (:documentation "Fold MSG into MODEL; answer (values new-model cmd quiet)."))

(defmethod update ((model t) msg)
  (declare (ignore msg))
  (values model nil))

(defgeneric view (model)
  (:documentation "Render MODEL to a view-state."))

(defgeneric settle-before-first-frame (model)
  ;; The launch frame is the one an operator reads before they can do
  ;; anything, so a model with a fact still coming — an identity, a status —
  ;; may spend a bounded moment here to put the answer in that frame instead
  ;; of a placeholder the next frame replaces. Bounded is the whole contract:
  ;; the wait is short enough to be invisible and must never be a peer's
  ;; latency (a launcher's hand-over, ui.lisp DECIDE-FOLDER-BEHIND-THE-FRAME,
  ;; waits on the frame this precedes). The term layer knows nothing about
  ;; what is being awaited.
  (:documentation "A bounded settle a model may take before this process
paints its first frame, answering whether anything was awaited. The default
settles for nothing.")
  (:method (model)
    (declare (ignore model))
    nil))

;;; --- messages ------------------------------------------------------------

(defclass message () ()
  (:documentation "Root class of every message."))

(defmacro defmessage (name slots &key documentation (superclass 'message))
  "Define message class NAME with SLOTS (DEFCLASS slot specs): a MESSAGE,
printed as any instance is, its class name and identity."
  ;; The constructor and the predicate are not emitted here: a message that
  ;; wants the pair, this file's and a shell's alike, takes it from
  ;; DEFINE-MESSAGE-VOCABULARY below. SUPERCLASS is MESSAGE or a family of
  ;; its own (MOUSE-EVENT).
  `(defclass ,name (,superclass)
     ,slots
     (:documentation ,(or documentation (format nil "Message ~A." name)))))

(defmacro define-message-vocabulary (&rest names)
  "Define, for each message class in NAMES, the constructor MAKE-NAME and
the predicate NAME-P, both interned in the class name's own package."
  ;; MAKE-NAME forwards its keywords to MAKE-INSTANCE, so a slot's :INITFORM
  ;; is its default and no second copy of that default exists; NAME-P is a
  ;; TYPEP over the class. Written by hand these are four lines per message
  ;; that say nothing the class did not already say.
  (flet ((sym (fmt name) (intern (format nil fmt (symbol-name name))
                                 (symbol-package name))))
    `(progn
       ,@(loop for name in names
               collect `(defun ,(sym "MAKE-~A" name) (&rest initargs)
                          (apply #'make-instance ',name initargs))
               collect `(defun ,(sym "~A-P" name) (object)
                          (typep object ',name))))))

;;; modifiers: a bitmask on key and mouse events, laid out as the kitty
;;; keyboard protocol lays its own out -- shift 1, alt 2, ctrl 4, super 8,
;;; hyper 16, meta 32, caps lock 64, num lock 128 -- so a decoder keeps a
;;; report's bits as they arrive. The shells read shift, alt and ctrl.

(defconstant +mod-shift+ #b001)
(defconstant +mod-alt+   #b010)
(defconstant +mod-ctrl+  #b100)

(defun mod-contains (mod flag)
  "Whether bitmask MOD carries FLAG."
  (logtest mod flag))

;;; keys

(defclass key-press-msg (message)
  ((code :initarg :code :initform nil :reader key-event-code
         :documentation "A character for a printable key, a keyword for a
special key (:enter :escape :tab :backspace :delete :insert :up :down :left
:right :home :end :page-up :page-down :f1 ... :unknown).")
   (mod :initarg :mod :initform 0 :reader key-event-mod
        :documentation "Modifier bitmask.")
   (text :initarg :text :initform "" :reader key-event-text
         :documentation "The text the key produced, when any."))
  (:documentation "A key press."))

(defmessage paste-msg
  ((text :initarg :text :initform nil :reader paste-msg-text))
  :documentation "One bracketed paste (ESC [ 200 ~ ... ESC [ 201 ~).")

(defmessage window-size-msg
  ((width :initarg :width :initform nil :reader window-size-msg-width)
   (height :initarg :height :initform nil :reader window-size-msg-height))
  :documentation "The terminal's size in cells, at start and on SIGWINCH.")

;;; mouse: coordinates are 1-based cells, as SGR reports them

(defclass mouse-event (message)
  ((x :initarg :x :initform nil :reader mouse-event-x :documentation "Column, 1-based.")
   (y :initarg :y :initform nil :reader mouse-event-y :documentation "Row, 1-based.")
   (mod :initarg :mod :initform 0 :reader mouse-event-mod
        :documentation "Modifier bitmask.")
   (button :initarg :button :initform nil :reader mouse-event-button
           :documentation ":left, :middle, :right, or :button-8 .. :button-11; NIL
for a motion with no button held and for a wheel turn."))
  (:documentation "Base class of the mouse events."))

(defmessage mouse-click-msg () :superclass mouse-event :documentation "A button pressed.")
(defmessage mouse-release-msg () :superclass mouse-event :documentation "A button released.")
(defmessage mouse-motion-msg () :superclass mouse-event :documentation "The pointer moved.")

(defclass mouse-wheel-msg (mouse-event)
  ((direction :initarg :direction :initform nil :reader mouse-wheel-direction
              :documentation ":up, :down, :left, or :right")
   (count :initarg :count :initform 1 :accessor mouse-wheel-count
          :documentation "Wheel events coalesced into this one."))
  (:documentation "The wheel turned."))

;;; tick, quit, the kitty graphics reply, the terminal-io request

(defmessage tick-msg
  ((time :initarg :time :initform (get-internal-real-time) :reader tick-msg-time))
  :documentation "A frame clock tick; the shell sends these itself.")

(defmessage quit-msg () :documentation "Stop the program loop.")

(defmessage kitty-graphics-msg
  ((ok-p :initarg :ok-p :initform nil :reader kitty-graphics-msg-ok-p))
  :documentation "The terminal's answer to the kitty graphics query (APC
ESC _ G ... ESC \\): OK-P when it reported OK, so images can be placed.")

(defmessage truecolor-msg
  ((ok-p :initarg :ok-p :initform nil :reader truecolor-msg-ok-p))
  :documentation "The terminal's answer to the truecolor probe (either DCS
reply): OK-P when its SGR honors 24-bit color, whatever the environment
claimed.")

(defmessage background-msg
  ((rgb :initarg :rgb :initform nil :reader background-msg-rgb))
  :documentation "The terminal's answer to the background query (OSC 11):
RGB, its background colour.")

(defmessage color-scheme-msg
  ((scheme :initarg :scheme :initform nil :reader color-scheme-msg-scheme))
  :documentation "The terminal's report that its colour scheme turned (DEC
2031, CSI ? 997 ; 1 n or 2 n): SCHEME, :DARK or :LIGHT.")

;;; A command that must write escape bytes (the clipboard fallback, the
;;; graphics query) answers this so the write is serialized with rendering
;;; instead of racing it from the worker.
(defmessage terminal-io-msg
  ((thunk :initarg :thunk :initform nil :reader terminal-io-msg-thunk))
  :documentation "Run THUNK with the terminal output stream on the loop
thread.")

;;; The other half of every message above, declared in one place: the
;;; constructor a caller reaches for and the predicate a dispatch tests.

(define-message-vocabulary
  key-press-msg paste-msg window-size-msg
  mouse-click-msg mouse-release-msg mouse-motion-msg mouse-wheel-msg
  tick-msg quit-msg kitty-graphics-msg truecolor-msg background-msg color-scheme-msg
  terminal-io-msg)

;;; --- the declarative view ------------------------------------------------

(nlk:define-record (cursor (:constructor make-cursor (&key (x 0) (y 0))) (:export t))
  "Where the hardware cursor parks: zero-based cell X, Y. Its shape is the
steady block."
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(nlk:define-record (view-state (:constructor make-view (rows &key alt-screen mouse-mode
                                                        keyboard-enhancements cursor images
                                                        title overlay))
                               (:export :constructor rows keyboard-enhancements cursor title
                                        overlay))
  "What one frame declares: ROWS (the rendered rows, one string each, top to
bottom, each already styled and cut to the width), whether the ALT-SCREEN is on, the MOUSE-MODE (:cell-motion, :all-motion, or
NIL), whether the kitty keyboard's disambiguation is on (KEYBOARD-ENHANCEMENTS;
NIL pops the kitty keyboard mode), the CURSOR (NIL hides it), the kitty
IMAGES to place over the content (a list of IMAGE-PLACEMENT), the TITLE
the window wears (NIL says nothing and leaves whatever the shell set), and
the OVERLAY: NIL, or a function the painter calls with the frame's
SCREEN-BUFFER once ROWS are laid out into it and before it is diffed."
  ;; An overlay restyles cells in place -- a mark over whatever the rows
  ;; painted -- so a frame that moves only the mark lays out no row twice.
  (rows '() :type list)
  (alt-screen nil)
  (mouse-mode nil)
  (keyboard-enhancements nil)
  (cursor nil)
  (images '() :type list)
  (title nil)
  (overlay nil))

;;; --- commands ------------------------------------------------------------
;;; A command is a thunk the worker funcalls; the message it answers, if any,
;;; is sent to the loop. A list of commands runs each.

(defun quit-cmd ()
  "A command that stops the program."
  (lambda () (make-quit-msg)))

(defun terminal-io-cmd (write)
  "A command that has the loop thread call WRITE with the terminal stream."
  (lambda () (make-terminal-io-msg :thunk write)))
