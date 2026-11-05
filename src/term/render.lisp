;;;; render.lisp --- one frame onto the terminal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's renderer.lisp (see NOTICE).
;;;;
;;;; A frame is the view-state's content laid out into cells, its overlay
;;;; applied to them, diffed against the previous frame, and written inside a
;;;; synchronized update so the terminal shows no tearing. The mode
;;;; transitions between the two view-states go first, the image placements
;;;; are diffed after the cells, and the cursor, hidden for the whole paint,
;;;; is placed and shown last, before the update closes, reached from where
;;;; the paint left it.

(in-package #:nodecode.term)

(nlk:access (cursor cursor) (view view-state))

(nlk:define-record (renderer (:constructor make-renderer (&key (output-stream *standard-output*)
                                                               (width 80) (height 24))))
  "The painter's memory: the last frame's cells and view-state, the
terminal's size, the placements on screen."
  (last-buffer nil)
  (last-view-state nil)
  (output-stream *standard-output*)
  (width 80 :type fixnum)
  (height 24 :type fixnum)
  (last-images '() :type list))

;;; A frame used to build a fresh WIDTH x HEIGHT grid of cells, which is 828
;;; KiB on a 157x72 window, every frame. Two grids alternating is enough to
;;; paint anything — the new one is diffed against the last one and then
;;; becomes it — so the third and later frames of a geometry allocate nothing.
;;;
;;; One scratch for the process, not a slot on the renderer, and deliberately:
;;; redefining the renderer struct under a running paint leaves the live
;;; instance obsolete, and an accessor on the new slot would read past an old
;;; instance's last slot. The scratch is only ever a retired front buffer — a
;;; renderer's own last-buffer is never handed out — so two renderers sharing
;;; it cannot hurt either one's diff.
(defvar *frame-scratch* nil
  "The cell grid the next frame is parsed into: the buffer one paint retired.")

;;; A launcher's hand-over waits for the first of them (cli/entry.lisp,
;;; DECIDE-FOLDER-BEHIND-THE-FRAME): a successor is told it inherited a live
;;; screen, and that is only true once a frame is really on it. A
;;; process-level counter, not a renderer slot — redefining the renderer
;;; struct under a running paint leaves the live instance obsolete. Zeroed
;;; at every image start: the release dump paints four frames warming the
;;; shell up, and a launch that started counting at four handed its process
;;; over before painting anything, so the successor never entered the
;;; alternate screen and the whole session painted over the shell's
;;; scrollback (a multiplexer drew its own scrollbar beside the rail).
(nlk:define-startup-parameter *frames-on-screen* 0
  "How many frames this process has painted and flushed to its terminal.")

(defun wait-for-frames-past (count &optional (deadline-ms 2000))
  "Wait until this process has painted more than COUNT frames, at most
DEADLINE-MS."
  ;; => T when they landed, NIL at the deadline. A caller that must
  ;; not land inside a frame waits on the number, not on a clock
  (nlk:wait-until (lambda () (> *frames-on-screen* count))
                  (nlk:deadline-after (/ deadline-ms 1000)) 0.001))

(defun render (renderer view &aux (stream renderer.output-stream)
                                  (width renderer.width)
                                  (height renderer.height))
  "Paint the view-state VIEW through RENDERER."
  (apply-terminal-transitions renderer.last-view-state view stream)
  (setf renderer.last-view-state view)
  ;; The screen is this shell's from here on: whatever INHERIT-SCREEN asked of
  ;; the first frame, later frames are transitions against our own paint.
  (setf *inherit-screen* nil)
  ;; A geometry change (SIGWINCH) makes the previous cells meaningless:
  ;; erase and paint the whole frame rather than diff mismatched grids.
  (let ((old renderer.last-buffer))
    (when (and old (or (/= width old.width) (/= height old.height)))
      (format stream "~C[2J" #\Escape)
      (setf renderer.last-buffer nil)))
  (let ((new (parse-styled-string view.rows width height
                                  :buffer (reusable-screen-buffer
                                           *frame-scratch* width height))))
    ;; A half-block picture is cells like any other, laid in under the
    ;; overlay, so a toast or the selection's wash still draws over it.
    (paint-block-images new view.images)
    (nlk:when-let (overlay view.overlay)
      (funcall overlay new))
    ;; A sixel that moved or went is erased by writing its cells again, and
    ;; one the diff is about to write over is drawn again after it.
    (forget-departed-sixels renderer.last-buffer renderer.last-images view.images)
    ;; The cursor hides before the paint moves it and shows only once it is
    ;; parked, all inside the update. A terminal that holds the update shows
    ;; the frame whole either way; but ConPTY re-emits a console's output on
    ;; its own schedule and cuts it where it likes, so on Windows any point
    ;; of the paint can reach the screen. Shown through the paint, the cursor
    ;; was drawn on whatever cell the frame wrote last (a spinner, a
    ;; countdown) and then jumped back: the caret flickered, and so did
    ;; everything that moves.
    ;;
    ;; The six bytes that hide it are paid for by the motion: the paint
    ;; starts from a cursor whose place it does not assume, so it sends no
    ;; home and goes straight to its first changed cell, and the caret is
    ;; reached from where the paint left off -- a typed key ends its paint
    ;; right at the caret, and costs no motion at all.
    (format stream "~C[?2026h~C[?25l" #\Escape #\Escape)
    (multiple-value-bind (x y damaged)
        (let ((damaged (damaged-sixels renderer.last-buffer new renderer.last-images view.images)))
          (multiple-value-bind (x y)
              (render-diff renderer.last-buffer new stream :cursor-x nil :cursor-y nil)
            (values x y damaged)))
      (multiple-value-bind (images placed)
          (%render-image-placements renderer.last-images
                                    view.images
                                    stream
                                    :force (null renderer.last-buffer)
                                    :damaged damaged)
        (setf renderer.last-images images)
        (nlk:when-let (cursor view.cursor)
          (move-cursor stream (and (not placed) x) (and (not placed) y)
                       cursor.x cursor.y)
          (format stream "~C[?25h" #\Escape))))
    (format stream "~C[?2026l" #\Escape)
    (force-output stream)
    ;; The frame is on the terminal: the count a launcher's hand-over waits
    ;; on before it replaces itself with a successor that will be told it
    ;; inherited a live screen.
    (incf *frames-on-screen*)
    ;; The buffer just diffed stops being the frame and becomes the next
    ;; frame's scratch: the painter alternates between two grids and
    ;; allocates cells only when the geometry moves. The scratch is a
    ;; retired FRONT buffer, never a live one, so no renderer ever parses
    ;; into a grid something else is still diffing against.
    (setf *frame-scratch* (shiftf renderer.last-buffer new))))
