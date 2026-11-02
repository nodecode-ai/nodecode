;;;; program-test.lisp --- the loop, the worker, the mailbox.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The fork had no tests for its loop; these pin the contract the shells
;;;; lean on: a send from any thread reaches UPDATE, wheel messages coalesce,
;;;; a quit stops the loop, a terminal-io request runs on the loop thread
;;;; with the terminal stream, the one worker runs commands in order and
;;;; joins on stop, a window size resizes the renderer before the model sees
;;;; it, and an error in UPDATE reaches the handler slot instead of the loop.
;;;; The loop is driven over a string-stream renderer: no tty is needed.

(in-package #:nodecode.test)

(nlk:access (up1 nlt::mouse-wheel-msg) (up3 nlt::mouse-wheel-msg))

(defclass probe-model ()
  ((seen :initform '() :accessor probe-seen)
   (frames :initform 0 :accessor probe-frames)
   (quiet :initform nil :accessor probe-quiet)
   (answer :initform nil :accessor probe-answer
           :documentation "A function of the message answering the command."))
  (:documentation "A model that remembers what it was told."))

(defmethod nlt:update ((model probe-model) msg)
  (push msg (probe-seen model))
  (values model (and (probe-answer model) (funcall (probe-answer model) msg))
          (probe-quiet model)))

(defmethod nlt:view ((model probe-model))
  (incf (probe-frames model))
  (nlt:make-view (list "probe")))

(defun make-probe-program (&aux (program (nlt:make-program (make-instance 'probe-model))))
  "A program over a probe model painting into a string stream, running."
  (setf (nlt:renderer-output-stream program.renderer)
        (make-string-output-stream))
  (setf program.running t)
  program)

(defun seen-of-type (model type)
  (remove-if-not (lambda (msg) (typep msg type)) (probe-seen model)))

(defmacro with-event-loop ((program loop-thread) &body body)
  "Run BODY with LOOP-THREAD running PROGRAM's event loop; the program is
stopped and the thread joined however BODY leaves."
  `(let ((,loop-thread (nlk:spawn "test-loop" (nlt:event-loop ,program))))
     (declare (ignorable ,loop-thread))
     (unwind-protect (progn ,@body)
       (setf (nlt:program-running ,program) nil)
       (ignore-errors (bt2:join-thread ,loop-thread)))))

(deftest loop-receives-sends-from-any-thread-and-stops-on-quit (let* ((program (make-probe-program))
                                                                      (model program.model)))
  (with-event-loop (program loop-thread)
    (bt2:join-thread
     (nlk:spawn "test-sender" (nlt:send program (nlt:make-key-press-msg :code #\a))))
    (is (await () (seen-of-type model 'nlt:key-press-msg)))
    (is (plusp (probe-frames model)) "and the frame after it is painted")
    (nlt:send program (nlt:make-quit-msg))
    (is (await () (not (bt2:thread-alive-p loop-thread))))
    (is (null program.running) "and the program is no longer running")
    (is (null (nlt:send program (nlt:make-tick-msg))))))

(deftest an-ended-terminal-quits-the-program (let* ((program (make-probe-program))
                                                    (model program.model)))
  ;; A terminal that hangs up -- its window closed, its ssh dropped, its pty's
  ;; master gone -- reads as the end of its stream. Nothing types there again,
  ;; so the input loop hands on what came before the end and quits the
  ;; program, rather than polling a dead tty for as long as the process lives
  ;; (T-348: an orphan shell polled one for 39 minutes, then fronted an
  ;; organism of its own).
  (setf (nlt::program-tty-stream program) (make-string-input-stream "a"))
  (with-event-loop (program loop-thread)
    (let ((input (nlk:spawn "test-input" (nlt::input-loop program))))
      (unwind-protect
           (progn
             (is (await () (not (bt2:thread-alive-p input))) "the input loop ends")
             (is (await () (not (bt2:thread-alive-p loop-thread))) "and the program with it")
             (is (seen-of-type model 'nlt:key-press-msg) "the key before the end still reaches the model"))
        (setf (nlt:program-running program) nil)
        (ignore-errors (bt2:join-thread input))))))

#-win32
(deftest a-hangup-quits-the-program (let ((program (make-probe-program))))
  ;; SIGHUP is what closing a terminal window sends: the program quits
  ;; through its loop, as a quit message stops it (T-348).
  (with-event-loop (program loop-thread)
    (nlt::on-signal program sb-posix:sighup)
    (is (await () (not (bt2:thread-alive-p loop-thread))) "the loop stops")
    (is (null program.running) "and the program is no longer running")))

(deftest loop-paces-physical-paints (with-saved-globals (nlt:*frame-period-seconds*))
  (setf nlt:*frame-period-seconds* 0.1)
  (let ((program (make-probe-program)))
    (with-event-loop (program loop-thread)
      (dotimes (i 5)
        (nlt:send program
                  (nlt:make-key-press-msg
                   :code (code-char (+ (char-code #\a) i))))
        ;; polled at 5 ms: a 100 ms rest would outlast the frame period
        (is (await (:tries 400 :rest 0.005)
              (= (length (seen-of-type program.model 'nlt:key-press-msg)) (1+ i)))))
      (is (= 1 (probe-frames program.model)))
      (is (await () (= 2 (probe-frames program.model))))
      (nlt:send program (nlt:make-quit-msg)))))

(deftest loop-coalesces-wheel-messages ()
  (let* ((up1 (nlt:make-mouse-wheel-msg :x 1 :y 1 :direction :up))
         (up2 (nlt:make-mouse-wheel-msg :x 1 :y 1 :direction :up))
         (down (nlt:make-mouse-wheel-msg :x 1 :y 1 :direction :down))
         (key (nlt:make-key-press-msg :code #\a))
         (up3 (nlt:make-mouse-wheel-msg :x 1 :y 1 :direction :up))
         (batch (nlt:coalesce-wheel (list up1 up2 down key up3))))
    (is (equal (list up1 down key up3) batch) "consecutive same-direction wheels fold into the first")
    (is (= 2 up1.count) "which counts the folded ones")
    (is (= 1 up3.count) "a wheel after a key starts afresh")))

(deftest loop-runs-terminal-io-on-its-own-thread-with-the-terminal-stream ()
  (let* ((program (make-probe-program))
         (stream (nlt:renderer-output-stream program.renderer))
         (ran-on nil) (ran-with nil))
    (nlt:handle-messages
     program (list (nlt:make-terminal-io-msg
                    :thunk (lambda (s) (setf ran-on (bt2:current-thread) ran-with s)))))
    (is (eq (bt2:current-thread) ran-on) "the thunk runs on the loop thread")
    (is (eq stream ran-with) "with the terminal stream, the renderer's when there is no tty")
    (is (null (probe-seen program.model)) "and the model never sees it")))

(deftest loop-paints-nothing-for-a-quiet-fold (let ((program (make-probe-program))))
  ;; UPDATE's third value: a fold that changed nothing a frame would show
  ;; -- a clock tick over an idle dialog -- costs the batch no paint.
  (setf (probe-quiet program.model) t)
  (is (not (nlt:handle-messages program (list (nlt:make-tick-msg)))) "a quiet batch reports no update")
  (is (zerop (probe-frames program.model)) "and paints no frame")
  (setf (probe-quiet program.model) nil)
  (is (nlt:handle-messages program (list (nlt:make-tick-msg))) "a fold that is not quiet updates")
  (is (= 1 (probe-frames program.model)) "and paints"))

(deftest loop-resizes-the-renderer-before-the-model-sees-the-size ()
  (let* ((program (make-probe-program)) (renderer program.renderer) (model program.model))
    (setf (probe-answer model)
          (nlk:lambda-ignoring (msg)
            (is (and (= 100 renderer.width) (= 30 renderer.height)))
            nil))
    (nlt:handle-messages program (list (nlt:make-window-size-msg :width 100 :height 30)))
    (is (= 1 (length (seen-of-type model 'nlt:window-size-msg))) "the model saw the size")
    (is (= 1 (probe-frames model)) "one frame was painted for the batch")))

(deftest worker-runs-commands-in-order-and-joins-on-stop ()
  (let* ((program (make-probe-program))
         (order '())
         (worker (nlt:start-worker program)))
    (nlk:with-cleanup ((ignore-errors (nlt:stop-worker program)))
      (is (bt2:thread-alive-p worker) "the worker is alive")
      (is (eq worker (nlt:start-worker program)) "there is exactly one worker")
      (nlt:run-command program (list (lambda () (push 1 order) nil)
                                     (lambda () (push 2 order) (nlt:make-tick-msg))))
      (is (await () (= 2 (length order))) "both commands ran")
      (is (equal '(1 2) (reverse order)) "in order")
      (is (await () (sb-concurrency:list-mailbox-messages program.mailbox)))
      (nlt:stop-worker program)
      (is (not (bt2:thread-alive-p worker)) "stop joins the worker")
      (is (null program.worker) "and forgets it")
      (is (null (probe-seen program.model)) "nothing reached UPDATE without a loop"))))

(deftest clipboard-writes-off-the-worker (let ((program (make-probe-program))))
  ;; The helper chain runs on its own thread; with no helper taking the
  ;; text, the OSC 52 fallback arrives at the loop as a terminal-io request.
  ;; SETF, not LET: the writer's thread never sees a test-thread binding.
  (with-saved-globals ((nlt:*clipboard-use-command* nil))
    (let ((writer (nlt:write-clipboard program "clip me")))
      (is (bt2:threadp writer) "the write runs on its own thread")
      (is (await () (sb-concurrency:list-mailbox-messages program.mailbox)))
      (let ((msg (first (sb-concurrency:list-mailbox-messages program.mailbox))))
        (is (nlt:terminal-io-msg-p msg) "as a terminal-io request")
        (is (search "]52;c;" (with-output-to-string (out)
                               (funcall (nlt:terminal-io-msg-thunk msg) out)))))
      (is (null (sb-concurrency:list-mailbox-messages (nlt::program-commands program)))))))

(deftest clipboard-writes-report-what-took-the-text (let ((program (make-probe-program))))
  ;; Issue #31: a write nothing took looked exactly like one that worked.
  ;; The report comes once the writer knows: OSC 52 when only the terminal
  ;; was written to (from the loop thread, after the write), FAILED when a
  ;; stopped program dropped even that.
  (with-saved-globals ((nlt:*clipboard-use-command* nil))
    (let* ((said '())
           (writer (nlt:write-clipboard program "clip me" (lambda (outcome) (push outcome said)))))
      (bt2:join-thread writer)
      (is (null said) "nothing is said before the terminal is written")
      (let ((msg (first (sb-concurrency:list-mailbox-messages program.mailbox))))
        (is (nlt:terminal-io-msg-p msg))
        (funcall (nlt:terminal-io-msg-thunk msg) (make-string-output-stream))
        (is (equal '(:osc52) said) "OSC 52 written is said as OSC 52, never as a helper")))
    (setf program.running nil)
    (let* ((said '())
           (writer (nlt:write-clipboard program "clip me" (lambda (outcome) (push outcome said)))))
      (bt2:join-thread writer)
      (is (equal '(:failed) said) "a dropped request is a failed write"))))

(deftest errors-reach-the-handler-slot-and-the-loop-goes-on (let* ((program (make-probe-program))
                                                                   (reports '())))
  (setf program.error-handler (lambda (where condition) (push (cons where condition) reports)))
  (setf (probe-answer program.model)
        (lambda (msg)
          (when (nlt:tick-msg-p msg) (error "update blew up"))
          nil))
  (with-event-loop (program loop-thread)
    (nlt:send program (nlt:make-tick-msg))
    (is (await () reports) "the error reaches the handler")
    (is (eq :event-loop (car (first reports))) "named by where it was caught")
    (nlt:send program (nlt:make-key-press-msg :code #\b))
    (is (await () (seen-of-type program.model 'nlt:key-press-msg)))
    (nlt:send program (nlt:make-quit-msg))
    (is (await () (not (bt2:thread-alive-p loop-thread))) "quit still stops it"))
  ;; A command that fails is reported too, and does not kill the worker.
  (let ((program (make-probe-program))
        (reports '()))
    (setf program.error-handler (lambda (where condition) (push (cons where condition) reports)))
    (nlt:start-worker program)
    (nlk:with-cleanup ((nlt:stop-worker program))
      (nlt:run-command program (lambda () (error "command blew up")))
      (is (await () reports) "a failing command is reported")
      (is (eq :command (car (first reports))) "as a command error")
      (is (bt2:thread-alive-p program.worker) "the worker survives it"))))

(deftest a-failing-fold-costs-its-own-message-not-the-batch (let* ((program (make-probe-program))
                                                                   (reports '())
                                                                   (ran '())))
  ;; If a method on UPDATE fails at every message, without this each
  ;; failure would take the rest of its batch with it -- stream frames
  ;; among them -- and every command the batch has answered, so the shell
  ;; would fall behind its gateway for good. The messages drained behind a
  ;; failing fold still fold, the commands the others answered still run,
  ;; and the frame is painted.
  (setf program.error-handler (lambda (where condition) (push (cons where condition) reports)))
  (setf (probe-answer program.model)
        (lambda (msg)
          (if (nlt:tick-msg-p msg)
              (error "update blew up")
              (lambda () (push (nlt:key-event-code msg) ran) nil))))
  (nlt:start-worker program)
  (nlk:with-cleanup ((nlt:stop-worker program))
    ;; the batch owes its frame
    (is (nlt:handle-messages program (list (nlt:make-key-press-msg :code #\a)
                                           (nlt:make-tick-msg)
                                           (nlt:make-key-press-msg :code #\b))))
    (is (= 3 (length (probe-seen program.model))) "every message of the batch was folded")
    (is (equal '(:event-loop) (mapcar #'car reports)) "the failing fold is reported, once")
    (is (await () (= 2 (length ran))) "and the commands the others answered ran")
    (is (equal '(#\a #\b) (reverse ran)) "in order")
    (is (= 1 (probe-frames program.model)) "one frame for the batch")))

(deftest a-failure-that-repeats-reports-once-a-window
    (with-saved-globals ((nlt::*reported* (make-hash-table :test 'equal :synchronized t))
                         nlt::*repeat-report-seconds*))
  ;; Without this, a fold failure at every message would print a backtrace
  ;; each time. The same failure at the same place reports once a window,
  ;; and says how often it came back when it reports again.
  (flet ((reports (text)
           (loop with start = 0
                 for at = (search "caught error at" text :start2 start)
                 while at
                 count t
                 do (setf start (1+ at)))))
    (let* ((failure (make-condition 'simple-error :format-control "the same failure"))
           (out (with-output-to-string (*error-output*)
                  (dotimes (i 40) (nlt:report-error :event-loop failure)))))
      (is (= 1 (reports out)) out)
      (let ((elsewhere (with-output-to-string (*error-output*)
                         (nlt:report-error :command failure)))
            (another (with-output-to-string (*error-output*)
                       (nlt:report-error :event-loop (make-condition 'simple-error
                                                                     :format-control "another failure")))))
        (is (= 1 (reports elsewhere)) "the same failure caught elsewhere is a report of its own")
        (is (= 1 (reports another)) "and so is another failure caught here"))
      (setf nlt::*repeat-report-seconds* 0)
      (is-carrying (again (with-output-to-string (*error-output*)
                            (nlt:report-error :event-loop failure)))
        (is (= 1 (reports again)) "once the window lapses it reports again")
        ("came back 39 times" "with its count since the last report")))))

(deftest the-loop-reports-on-its-own-thread-and-says-so (let* ((program (make-probe-program))
                                                               (seen '())))
  ;; The edge the freeze turned on: EVENT-LOOP catches on the thread it
  ;; drains on, so a handler that SENDs is a producer into the very queue
  ;; whose consumer just failed — and a failure that reproduces on every
  ;; message then refills the mailbox as fast as it empties, which is the
  ;; one thing MAILBOX-DRAIN's wait cannot rate-limit. ON-LOOP-THREAD-P is
  ;; how a handler tells the two callers apart: fold here, post from
  ;; anywhere else.
  (is (not (nlt:on-loop-thread-p program)))
  (setf program.error-handler
        (nlk:lambda-ignoring (where condition)
          (push (cons where (nlt:on-loop-thread-p program)) seen)))
  (setf (probe-answer program.model)
        (lambda (msg) (when (nlt:tick-msg-p msg) (error "update blew up"))))
  (with-event-loop (program loop-thread)
    (is (await () program.loop-thread))
    (is-shape program (.loop-thread eq loop-thread) (nlt:on-loop-thread-p not))
    (nlt:send program (nlt:make-tick-msg))
    (is (await () seen) "a failing fold reaches the handler")
    (is (equal '(:event-loop . t) (first seen))))
  (is (null program.loop-thread)))
