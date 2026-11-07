;;;; stdio.lisp --- a child process speaking newline-delimited JSON-RPC.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The stdio transport: one frame per line on the child's stdin and
;;;; stdout. The child's stderr goes to an append-only file under
;;;; *LOG-DIRECTORY* — never to this image's fd 2 (severed to a log while a
;;;; TUI owns the terminal) and never to /dev/null (the first question about
;;;; a server that dies at startup is "what did it print").
;;;;
;;;; Reads are bounded without a reader thread: READ-CHAR-NO-HANG drains
;;;; what the stream holds, and when it holds nothing the thread parks in
;;;; SB-SYS:WAIT-UNTIL-FD-USABLE for what remains of the deadline. A plain
;;;; READ-LINE after the wait would still block on a half-written line; the
;;;; partial line a deadline interrupts is kept on the transport and resumed
;;;; by the next read, so a late reply never turns into garbage.
;;;;
;;;; The single spawn goes through LAUNCH, the one name a test stubs.

(in-package #:nodecode-mcp)

(defparameter *close-grace-seconds* 1
  "How long CLOSE waits for the child to exit on its own after stdin closes,
and again after SIGTERM, before SIGKILL.")

(defstruct (stdio-transport (:copier nil))
  pid                                   ; the child, leading a process group
  process                               ; its SB-EXT:PROCESS on the fork path
  (exit nil)                            ; its exit status, once reaped
  input
  output
  log-path
  (label "stdio" :type string)
  (partial nil)                         ; a line a deadline cut short
  (closed-p nil))

(defun launch (argv &key environment error-log)
  "Spawn ARGV with both pipes ours and stderr appended to ERROR-LOG.
=> (values PID OUTPUT INPUT PROCESS)."
  ;; The one process spawn in the cell, so a test stubs this name. Through
  ;; the kernel's spawn, not a fork: the cell starts every server at once,
  ;; and a fork caught by a collection while it held the allocator's locks
  ;; would freeze the whole image — its MCP-connecting neighbour waiting on
  ;; those locks, the collection waiting on the neighbour.
  (nlk:bind (((pid output _ process input)
              (nlk:spawn-program argv :input :stream :error-output error-log
                                      :environment environment)))
    (values pid output input process)))

(defun start-stdio (spec allowlist)
  "Launch SPEC's command; a STDIO-TRANSPORT, or TRANSPORT-CLOSED naming why."
  (let ((log-path (stderr-log-path spec.name)) (argv (cons spec.command spec.args)))
    (handler-case
        (progn
          ;; Create the log (:APPEND opens without O_CREAT) and stamp the launch.
          (ensure-directories-exist log-path)
          (with-open-file (out log-path :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
            (format out "~&;; mcp ~a: launched ~a ~a~{ ~a~}~%"
                    spec.name (nlk:iso-time (nlk:unix-now) :millis nil) spec.command spec.args))
          (multiple-value-bind (pid output input process)
              (launch argv
                      :environment (child-environment spec allowlist)
                      :error-log log-path)
            (make-stdio-transport
             :pid pid
             :process process
             :input input
             :output output
             :log-path log-path
             :label (format nil "stdio ~a" spec.command))))
      (error (condition)
        (error 'transport-closed :detail (format nil "cannot launch ~a: ~a" spec.command condition))))))

(defmethod transport-send ((transport stdio-transport) text deadline)
  (declare (ignore deadline))
  (when (stdio-transport-closed-p transport)
    (error 'transport-closed :detail "process closed"))
  (check-frame text)
  (handler-case
      (let ((stream (stdio-transport-input transport)))
        (write-string text stream)
        (write-char #\Newline stream)
        (finish-output stream))
    (error (condition)
      (error 'transport-closed
             :detail (format nil "write to the process failed (~a); ~a"
                             condition (exit-text transport))))))

(defun read-frame (transport deadline &aux (stream (stdio-transport-output transport))
                                           (fd (sb-sys:fd-stream-fd stream))
                                           (buffer (make-string-output-stream))
                                           (count 0))
  "One line from the child, or :TIMEOUT, or :CLOSED at EOF."
  (when (stdio-transport-partial transport)
    (write-string (stdio-transport-partial transport) buffer)
    (setf count (length (stdio-transport-partial transport))
          (stdio-transport-partial transport) nil))
  (loop
    (let ((char (read-char-no-hang stream nil :eof)))
      (cond
        ((eq char :eof) (return :closed))
        ((null char)
         (let ((remaining (seconds-remaining deadline)))
           (when (or (zerop remaining)
                     (not (sb-sys:wait-until-fd-usable fd :input remaining)))
             (setf (stdio-transport-partial transport)
                   (get-output-stream-string buffer))
             (return :timeout))))
        ((char= char #\Newline)
         (return (get-output-stream-string buffer)))
        (t
         (write-char char buffer)
         (when (> (incf count) +frame-limit+)
           (error 'transport-closed
                  :detail "frame over the 1 MiB limit; connection dropped")))))))

(defmethod transport-receive ((transport stdio-transport) deadline)
  (when (stdio-transport-closed-p transport)
    (return-from transport-receive :closed))
  (loop
    (let ((frame (handler-case (read-frame transport deadline)
                   (transport-closed (condition) (error condition))
                   (error (condition)
                     (error 'transport-closed
                            :detail (format nil "read from the process failed (~a); ~a"
                                            condition (exit-text transport)))))))
      (cond
        ((keywordp frame) (return frame))
        ((zerop (length (string-trim '(#\Space #\Tab #\Return) frame))))
        (t
         (let ((message (handler-case (nlk:decode-json frame) (error () :unreadable))))
           (if (hash-table-p message)
               (return message)
               (warn "mcp ~a: skipped a line that is not a JSON object: ~a"
                     (stdio-transport-label transport)
                     (if (> (length frame) 80) (subseq frame 0 80) frame)))))))))

(defun child-exited (transport)
  "TRANSPORT's child's exit status once it has exited, else NIL."
  (or (stdio-transport-exit transport)
      (and (stdio-transport-pid transport)
           (setf (stdio-transport-exit transport)
                 (nlk:child-exit (stdio-transport-pid transport)
                                 (stdio-transport-process transport))))))

(defun exit-text (transport)
  "\"still running\", or how the child ended, for an error line."
  (cond ((null (stdio-transport-pid transport)) "no process")
        ((null (child-exited transport)) "process still running")
        (t (format nil "process exited with ~a; see ~a"
                   (stdio-transport-exit transport)
                   (namestring (stdio-transport-log-path transport))))))

(defun await-exit (transport seconds)
  (loop repeat (ceiling (* seconds 10))
        until (child-exited transport)
        do (sleep 0.1)))

(defmethod transport-close ((transport stdio-transport))
  (unless (stdio-transport-closed-p transport)
    (setf (stdio-transport-closed-p transport) t)
    (let ((pid (stdio-transport-pid transport)))
      (ignore-errors (close (stdio-transport-input transport)))
      (when pid
        ;; Stdin closed, then SIGTERM to the child's group, then the whole
        ;; group killed: a server npx or uvx started is a tree, not a process.
        (await-exit transport *close-grace-seconds*)
        (unless (child-exited transport)
          #-win32 (ignore-errors (sb-posix:kill (- pid) sb-posix:sigterm))
          (await-exit transport *close-grace-seconds*))
        (unless (child-exited transport)
          (nlk:kill-tree pid)
          (await-exit transport *close-grace-seconds*))
        (ignore-errors (close (stdio-transport-output transport)))))
    t))
