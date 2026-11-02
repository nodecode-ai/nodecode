;;;; program.lisp --- the loop: one model, its messages, its frames.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Rewritten from cl-tuition's program.lisp, thread-pool.lisp and
;;;; errors.lisp (see NOTICE) to the shape the shells run: three threads
;;;; and a mailbox. The caller of RUN is the loop thread — it drains the
;;;; mailbox, folds every message into the model through UPDATE, hands the
;;;; commands to the worker, and paints the newest model at a bounded frame
;;;; rate. The input thread decodes the tty into messages. The one worker
;;;; funcalls commands
;;;; in order (the single-writer discipline: a turn occupies it for its
;;;; whole duration, so anything that must not wait behind a turn — the
;;;; frame clock — sends straight to the mailbox instead). Any thread may
;;;; SEND.
;;;;
;;;; The mailbox is a lock and a condition variable: the loop blocks until
;;;; something arrives, no polling. Signal handlers never touch it, and never
;;;; touch the terminal either — a handler runs on whichever thread the
;;;; signal lands on, and terminal I/O from a thread that interrupted another
;;;; mid-write blocks on the stream for good — so SIGWINCH and SIGTSTP only
;;;; defer a request to the loop through a short-lived thread. The loop
;;;; thread suspends the whole process itself and, since KILL returns to it
;;;; on SIGCONT, resumes right there; no SIGCONT handler exists.

(in-package #:nodecode.term)

(nlk:access (previous mouse-wheel-msg) (vs view-state))

;;; --- frame pacing --------------------------------------------------------

;;; Model updates still run as soon as mailbox messages arrive. Only the
;;; terminal projection is paced, so a burst of stream deltas is folded into
;;; the newest frame instead of reparsing the whole screen once per message.
;;;
;;; The ceiling is a fast display's refresh: a frame the pointer or a key
;;; asks for lands on the next one, where a 60 Hz ceiling left a drag or a
;;; wheel on every other refresh of a 120 Hz screen. A frame costs about
;;; 1.5 ms on a 200x50 window. What animates without input -- a streaming
;;; turn, the spinner -- keeps the shell's own frame clock (tui/model.lisp,
;;; *FRAME-CLOCK-PERIOD-SECONDS*), so a turn paints no more often than it did.
(defparameter *frame-period-seconds* 1/120
  "Maximum physical terminal paint rate for the program loop: 120 frames a second.")

;;; --- the mailbox ---------------------------------------------------------

;;; An SB-CONCURRENCY mailbox: any thread posts to it, one thread takes from it.

(defun mailbox-drain (mailbox &key (timeout 1))
  "Every item posted so far, oldest first; blocks up to TIMEOUT seconds
for the first and answers NIL when none arrived."
  ;; A mailbox refuses a zero timeout, and the paint loop's is zero while a
  ;; frame is overdue.
  (or (sb-concurrency:receive-pending-messages mailbox)
      (when (plusp timeout)
        (nlk:bind (((first found) (sb-concurrency:receive-message mailbox :timeout timeout)))
          (and found (cons first (sb-concurrency:receive-pending-messages mailbox)))))))

;;; --- the program ---------------------------------------------------------

(defparameter *repeat-report-seconds* 60
  "How long an error caught again at the same place is counted rather than
reported again.")

(defvar *reported* (make-hash-table :test 'equal :synchronized t)
  "(WHERE TYPE TEXT) -> (REPORTED-AT . REPEATS): the errors REPORT-ERROR
reported within the last *REPEAT-REPORT-SECONDS*, and how often each came
back since.")

(defun report-error (where condition)
  "The debug-only error report: the bounded condition and its backtrace on
*ERROR-OUTPUT*, which a TUI keeps in its sidecar log, never notifications."
  ;; The same error caught at the same place again within
  ;; *REPEAT-REPORT-SECONDS* is counted, not reported. Without this, a fold
  ;; that fails at every message would print a backtrace at every message and
  ;; the sidecar log would grow without bound. Once the window lapses the
  ;; error reports again, its count since the last report first.
  (let ((*print-level* 3) (*print-length* 12))
    (ignore-errors
      (let* ((text (nlk:safe-condition-report condition))
             (key (list where (type-of condition) text))
             (now (get-internal-real-time))
             (window (* *repeat-report-seconds* internal-time-units-per-second))
             (seen (gethash key *reported*)))
        (cond ((and seen (< (- now (car seen)) window)) (incf (cdr seen)))
              (t (when (> (hash-table-count *reported*) 64)
                   (sb-ext:with-locked-hash-table (*reported*)
                     (maphash (lambda (old value)
                                (unless (< (- now (car value)) window) (remhash old *reported*)))
                              *reported*)))
                 (setf (gethash key *reported*) (cons now 0))
                 (when (and seen (plusp (cdr seen)))
                   (format *error-output* "~&terminal core: the error at ~A came back ~D time~:P since its last report~%"
                           where (cdr seen)))
                 (format *error-output* "~&terminal core caught error at ~A:~%~A~%" where text)
                 (sb-debug:print-backtrace :stream *error-output* :print-thread t :emergency-best-effort t)
                 (finish-output *error-output*)))))))

(nlk:define-record (program (:constructor make-program (model)))
  "A running model."
  ;; ERROR-HANDLER is called with (WHERE CONDITION) for an error caught on any
  ;; of the program's threads. LOOP-THREAD is whichever thread is inside
  ;; EVENT-LOOP, so a caller can ask whether it is already standing in the
  ;; fold it would otherwise post to.
  model
  (renderer (make-renderer))
  (mailbox (sb-concurrency:make-mailbox))
  (commands (sb-concurrency:make-mailbox))
  (running nil)
  (tty-stream nil)
  (worker nil)
  (input-thread nil)
  (loop-thread nil)
  (error-handler #'report-error))

(defun send (program msg)
  "Post MSG to PROGRAM's loop from any thread; dropped while the program is
not running."
  (when program.running
    (sb-concurrency:send-message program.mailbox msg)
    msg))

(defun on-loop-thread-p (program)
  "Whether this is the thread draining PROGRAM's mailbox."
  ;; Work that already holds the model can run here instead of posting: a SEND
  ;; from the loop thread is a message to itself, which costs a whole batch,
  ;; and for anything the loop calls while handling one — an error report
  ;; above all — it feeds the loop that is already failing. A handler that
  ;; answers T here folds in place and leaves the mailbox as it found it, so
  ;; the loop blocks on the next drain rather than spinning on its own output.
  (eq program.loop-thread (bt2:current-thread)))

(defun %report (program where condition)
  (ignore-errors (funcall program.error-handler where condition)))

(defun resize-program (program &optional (post #'send))
  "Measure the terminal, size PROGRAM's renderer to it, and tell the model
through POST — SEND, or %DEFER from a signal handler. => (WIDTH . HEIGHT)."
  (multiple-value-bind (width height) (terminal-size)
    (setf (renderer-width program.renderer) width (renderer-height program.renderer) height)
    (funcall post program (make-window-size-msg :width width :height height))
    (cons width height)))

(defun write-clipboard (program text &optional report)
  "Put TEXT on the system clipboard without waiting behind the worker: the
helper chain (wl-copy, xclip, xsel) runs on a thread of its own, and when
no helper takes the text the OSC 52 fallback is posted as a terminal-io
request the loop thread writes. REPORT, when given, is called once with what
happened -- (:COMMAND NAME) when helper NAME took the text, :OSC52 when the
sequence reached the terminal, :FAILED when neither did -- on the clipboard
thread or the loop thread, whichever learned it. Answers the thread."
  ;; OSC 52 is a request the terminal may ignore (several ship with it off,
  ;; and a default tmux drops it), so :OSC52 says only that it was written:
  ;; the caller must not say `copied' on it.
  (flet ((said (outcome) (when report (ignore-errors (funcall report outcome)))))
    (nlk:spawn "nlt-clipboard"
      (let ((helper (and *clipboard-use-command* (ignore-errors (copy-text-command text)))))
        (cond (helper (said (list :command helper)))
              ((send program (make-terminal-io-msg
                              :thunk (lambda (stream)
                                       (said (handler-case (copy-to-clipboard text :stream stream)
                                               (error () :failed)))))))
              ;; A program that stopped drops the request: nothing was written.
              (t (said :failed)))))))

;;; --- commands and the worker ---------------------------------------------

(defun run-command (program cmd)
  "Queue CMD for the worker: a thunk, or a list of them."
  (cond ((listp cmd) (dolist (c cmd) (run-command program c)))
        ((functionp cmd) (sb-concurrency:send-message program.commands cmd))))

(defun start-worker (program)
  "Start PROGRAM's one worker thread; it funcalls each queued command and
sends the message it answers."
  (unless (and program.worker (bt2:thread-alive-p program.worker))
    (setf program.worker
          (nlk:spawn "nlt-worker"
            (loop named worker
                  do (dolist (cmd (mailbox-drain program.commands))
                       (when (eq cmd :stop) (return-from worker))
                       (nlk:with-handlers ((error (e) (%report program :command e)))
                         (nlk:when-let (msg (funcall cmd)) (send program msg))))))))
  program.worker)

(defun stop-worker (program)
  "Stop the worker after the commands already queued, and join it."
  (nlk:when-let (worker program.worker)
    (sb-concurrency:send-message program.commands :stop)
    (when (bt2:thread-alive-p worker)
      (bt2:join-thread worker))
    (setf program.worker nil)))

;;; --- the loop ------------------------------------------------------------

(defun coalesce-wheel (messages &aux (result '())
                                     (previous nil))
  "Consecutive wheel messages in one direction folded into the first, its
COUNT the number folded."
  (dolist (msg messages)
    (cond ((and (mouse-wheel-msg-p msg)
                previous
                (eq (mouse-wheel-direction msg) (mouse-wheel-direction previous)))
           (incf previous.count))
          (t (push msg result)
             (setf previous (and (mouse-wheel-msg-p msg) msg)))))
  (nreverse result))

(defun handle-messages (program messages &key (paint-p t) &aux (renderer program.renderer)
                                                               (updated nil)
                                                               (commands '()))
  "Fold MESSAGES into the model and queue the commands they answer."
  ;; When PAINT-P is true, paint the newest model after the batch -- unless
  ;; every fold in it answered QUIET (UPDATE's third value), when nothing on
  ;; screen moved and the batch owes no frame. The event loop passes NIL here
  ;; while a frame is inside its pacing window, so model updates remain prompt
  ;; while physical terminal paints coalesce. A quit message stops the
  ;; program; :SUSPEND hands the terminal back and stops the process until
  ;; SIGCONT; a terminal-io message runs its thunk on this thread with the
  ;; terminal stream; a window-size message resizes the renderer before the
  ;; model sees it.
  ;;
  ;; A fold that fails costs its own message: it is reported here, on the
  ;; loop's thread, the frame is owed for what it moved before it failed, and
  ;; the batch goes on. Without this, a method on UPDATE failing at every
  ;; message would unwind to the loop, taking every message drained behind it
  ;; and every command the batch had answered; a shell folding one stream
  ;; frame in each batch would fall behind its gateway and never catch up.
  (dolist (msg (coalesce-wheel messages))
    (cond
      ((quit-msg-p msg)
       (setf program.running nil)
       (return-from handle-messages))
      ((eq msg :suspend)
       ;; Hand the terminal back, stop until SIGCONT, retake it, and forget the last frame.
       (%undo-view-modes program)
       (exit-modes program.tty-stream)
       (exit-raw-mode)
       #-win32 (sb-posix:kill (sb-posix:getpid) sb-posix:sigstop)
       (enter-raw-mode)
       (enter-modes program.tty-stream)
       (setf renderer.last-view-state nil renderer.last-buffer nil renderer.last-images '())
       (resize-program program)
       (setf updated t))
      ((terminal-io-msg-p msg)
       (let ((stream (or program.tty-stream renderer.output-stream)))
         (nlk:with-handlers ((error (e) (%report program :terminal-io e)))
           (funcall (terminal-io-msg-thunk msg) stream)
           (force-output stream))))
      (t
       (when (window-size-msg-p msg)
         (setf renderer.width (window-size-msg-width msg)
               renderer.height (window-size-msg-height msg)))
       (nlk:with-handlers ((error (e) (%report program :event-loop e) (setf updated t)))
         (multiple-value-bind (model cmd quiet) (update program.model msg)
           (setf program.model model)
           (unless quiet (setf updated t))
           (when cmd (push cmd commands)))))))
  (run-command program (nreverse commands))
  (when (and updated paint-p)
    (render renderer (view program.model)))
  updated)

(defun event-loop (program &aux last-painted-at pending-paint)
  "Drain and handle messages until the program stops."
  ;; An error in a batch is reported and the loop continues; a fold's own is
  ;; HANDLE-MESSAGES', which goes on with the batch.
  ;;
  ;; The report runs on this thread, which is what makes the retry safe: a
  ;; handler is free to reach for the model through ON-LOOP-THREAD-P and
  ;; finish the job in place. One that posts a message instead hands the loop
  ;; its own failure as the next batch, and a fold that fails on every message
  ;; then has nothing to block on — the drain never empties and the loop turns
  ;; as fast as the failure reproduces.
  (setf program.loop-thread (bt2:current-thread))
  (nlk:with-cleanup ((setf (program-loop-thread program) nil))
    (loop while program.running
          do (nlk:with-handlers ((error (e) (%report program :event-loop e)))
               (let* ((period (round (* *frame-period-seconds*
                                        internal-time-units-per-second)))
                      ;; A pending frame owns the next wake-up: either a
                      ;; message arrives and joins the batch, or the
                      ;; deadline expires and the newest model is painted.
                      (timeout (cond ((not pending-paint) 1)
                                     ((null last-painted-at) 0)
                                     (t (max 0 (/ (- (+ last-painted-at period)
                                                     (get-internal-real-time))
                                                  (coerce internal-time-units-per-second
                                                          'double-float))))))
                      (messages (mailbox-drain
                                 program.mailbox
                                 :timeout timeout)))
                 (when (and messages
                            (handle-messages program messages :paint-p nil))
                   (setf pending-paint t))
                 (when (and pending-paint
                            (or (null last-painted-at)
                                (>= (- (get-internal-real-time) last-painted-at)
                                    period)))
                   ;; A frame is owed once: a paint that fails is reported
                   ;; and dropped, and the next message owes the next one.
                   ;; Kept owed, it was retried on every turn of this loop
                   ;; with nothing to wait on, one report a turn.
                   (setf pending-paint nil)
                   (render program.renderer (view program.model))
                   (setf last-painted-at (get-internal-real-time))))))))

(defun input-loop (program &aux (ended nil))
  "Decode the tty into messages until the program stops, or the tty ends."
  ;; A tty ends when it hangs up -- its window closed, its ssh dropped, its
  ;; pty's master gone -- and nothing types there again: what came before the
  ;; end is handed on, then a quit. Polled on, it held the process alive with
  ;; no terminal anywhere, and the shell later fronted an organism of its
  ;; own (T-348). On Windows the loop also measures the console every tenth
  ;; of a second, the resize signal that platform lacks.
  (let ((stream program.tty-stream)
        #+win32 (size (multiple-value-call #'cons (terminal-size)))
        #+win32 (measure-at (get-internal-real-time)))
    (loop while (and program.running (not ended))
          do (nlk:with-handlers ((error (e) (%report program :input e)))
               (let ((events (loop for msg = (read-key stream :ended)
                                   while (and msg (not (eq msg :ended)))
                                   collect msg
                                   finally (setf ended (eq msg :ended)))))
                 (dolist (msg events) (send program msg))
                 (cond (ended (send program (make-quit-msg)))
                       ((null events) (sleep 0.001)))
                 #+win32
                 (when (>= (get-internal-real-time) measure-at)
                   ;; no SIGWINCH here: resize when the measured size moved
                   (unless (equal size (multiple-value-call #'cons (terminal-size)))
                     (setf size (resize-program program)))
                   (setf measure-at (nlk:deadline-after 1/10))))))))

;;; --- signals -------------------------------------------------------------
;;; A handler runs on whichever thread the signal interrupts: it neither
;;; takes the mailbox lock nor writes the terminal; it hands the request to
;;; a short-lived thread whose SEND the loop thread serves.

(defun %defer (program msg)
  (nlk:spawn "nlt-signal" (send program msg)))

#-win32
(defun on-signal (program signal)
  "Hand what SIGNAL asks of PROGRAM to its loop: SIGWINCH re-measures,
SIGTSTP suspends, SIGHUP -- its terminal hung up, the window closed -- quits."
  (cond ((= signal sb-posix:sigwinch) (resize-program program #'%defer))
        ((= signal sb-posix:sighup) (%defer program (make-quit-msg)))
        (t (%defer program :suspend))))

;;; --- run -----------------------------------------------------------------

(defun %undo-view-modes (program &aux (renderer program.renderer)
                                      (stream program.tty-stream)
                                      (vs renderer.last-view-state))
  "Clear what the last frame's view-state switched on: tracking, the kitty
keyboard flags, the images, the window title, the alternate screen."
  (when vs
    (when vs.mouse-mode (disable-mouse stream))
    (when vs.keyboard-enhancements (pop-kitty-keyboard stream))
    (when renderer.last-images (clear-kitty-images stream))
    (when vs.title (reset-window-title stream))
    (when vs.alt-screen (exit-alt-screen stream))
    (force-output stream)))

;;; Bound T only where an exec is promised — a refused exec calls
;;; RELEASE-TERMINAL before it prints a word.
(defvar *keep-terminal* nil
  "T while this process is about to be replaced by a successor that will
paint this same terminal: the alternate screen, the raw settings and the
open tty are the successor's to inherit, so RUN's unwind leaves every one of
them alone.")

(defun release-terminal (program &aux (tty program.tty-stream))
  "Give the terminal back the way RUN's unwind would have: the view modes,
the raw settings, the tty closed."
  ;; The refusal path of a promised handover calls this; a handover that
  ;; happens never does.
  (ignore-errors (%undo-view-modes program))
  (when tty
    (ignore-errors (exit-modes tty))
    (ignore-errors (exit-raw-mode)))
  (ignore-errors (close-tty))
  (setf program.tty-stream nil)
  t)

(defun run (program &aux (tty (open-tty))
                         (renderer program.renderer)
                         (handlers '()))
  "Run PROGRAM on the terminal until it quits: open /dev/tty, raw mode, the
session modes, the worker and the input thread, the first window size, the
model's INIT command, the first frame, then the loop."
  ;; On the way out — an error included — every mode is undone, the threads
  ;; are joined, the signal handlers restored, raw mode left, the tty closed.
  (setf program.tty-stream tty
        renderer.output-stream tty)
  (unwind-protect
       (progn
         ;; Inside the protection, not before it: ENTER-MODES is the first
         ;; write to the terminal, and when it fails (a Windows console
         ;; stream over a bad handle did) the cleanup below must still
         ;; leave raw mode. Before it, the failure skipped EXIT-RAW-MODE
         ;; and the console kept reporting the mouse into the shell the
         ;; program had exited to.
         (enter-raw-mode)
         (enter-modes tty)
         (setf program.running t)
         ;; SIGWINCH re-measures, SIGTSTP asks the loop to suspend, SIGHUP to
         ;; quit (ON-SIGNAL); Windows has none of them.
         (setf handlers
               #+win32 '()
               #-win32
               (loop for signal in (list sb-posix:sigwinch sb-posix:sigtstp sb-posix:sighup)
                     collect (cons signal
                                   (sb-sys:enable-interrupt
                                    signal (nlk:lambda-ignoring (signal code scp)
                                             (on-signal program signal))))))
         (start-worker program)
         (setf program.input-thread
               (nlk:spawn "nlt-input" (input-loop program)))
         (resize-program program)
         (run-command program (init program.model))
         ;; A model with a read still in flight may settle for it here, so
         ;; the first frame carries the answer rather than a placeholder
         ;; (the model protocol's one bounded wait: SETTLE-BEFORE-FIRST-FRAME).
         (settle-before-first-frame program.model)
         ;; The landmark the launch is judged by: what this process had on
         ;; its terminal, and when (NODECODE_PHASE_LOG).
         (nlk:note-boot-phase "first-frame")
         (render renderer (view program.model))
         (event-loop program))
    (setf program.running nil)
    ;; A promised successor paints this terminal next: the screen, the raw
    ;; settings and the tty stay exactly as the last frame left them, and
    ;; nothing else here is worth unwinding for a process about to be
    ;; replaced. RELEASE-TERMINAL is what the refusal path calls instead.
    (unless *keep-terminal*
      (ignore-errors (%undo-view-modes program))
      (ignore-errors (stop-worker program))
      (nlk:when-let (thread program.input-thread)
        (ignore-errors (bt2:join-thread thread))
        (setf program.input-thread nil))
      (ignore-errors
       (dolist (entry handlers)
         (when (cdr entry)
           #-win32 (sb-sys:enable-interrupt (car entry) (cdr entry)))))
      (ignore-errors (exit-modes tty))
      (ignore-errors (exit-raw-mode))
      (close-tty))))
