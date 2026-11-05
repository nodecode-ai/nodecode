;;;; rpc.lisp --- `nodecode rpc': the gateway on a program's stdin and stdout.
;;;; Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A program drives the organism the way Pi's `--mode rpc' is driven: it
;;;; spawns `nodecode rpc', writes one JSON command a line and reads one JSON
;;;; frame a line. Nothing here is a second protocol. A line in is the command
;;;; object a shell's client_command frame carries, read by the gateway's own
;;;; decoder (protocol.lisp DECODE-COMMAND); a line out is a sync frame exactly
;;;; as the gateway sent it. The shell does for the program what every shell
;;;; does for itself and no more: it finds the organism serving the store
;;;; through its holder sidecar, or starts one it fronts (FRONT-ORGANISM),
;;;; presents the operator token, makes sure its session exists and subscribes
;;;; to it, so the first lines out are that session's replay -- or, given the
;;;; cursor a program last read, only what landed past it (a resume, the
;;;; replay frame's resumed_after saying so). On a command it
;;;; fills in what a shell's own senders do (SEND-GATEWAY-COMMAND): a
;;;; command_id minted when the line names none, the correlation_id the same,
;;;; and its own session for a command that names no session_id.
;;;;
;;;; Its life is the program's: stdin's end lets go of an organism this shell
;;;; started -- a turn still running there stops with it -- and leaves one
;;;; that was already serving as it was; the gateway's end ends this shell,
;;;; nonzero.
;;;;
;;;; The wire, the lines both ways included, is protocol.schema.json beside
;;;; gateway/protocol.lisp.

(in-package #:nodecode.evolved)

(defun rpc-command-text (line session-id)
  "LINE, one command a program wrote, as the client_command frame the gateway
reads: a command_id minted when LINE names none, its correlation_id the same,
and SESSION-ID for a command that names no session. A line the gateway would
refuse is a PROTOCOL-ERROR here, before it leaves."
  (let ((object (handler-case (nlk:decode-json line)
                  (error (condition)
                    (nlk::protocol-refuse :invalid-json "invalid JSON: ~a" condition)))))
    (nlk::protocol-object object "command")
    ;; A subscription is the one command with no identity (DECODE-COMMAND).
    (unless (equal "set_session_subscriptions" (gethash "type" object))
      (let ((id (or (json-value object :text "command_id") (nlk:make-durable-id "command"))))
        (setf (gethash "command_id" object) id)
        (unless (json-value object :text "correlation_id")
          (setf (gethash "correlation_id" object) id))))
    (let* ((command (nlk::decode-command object))
           (request (nlk:gateway-command-request command)))
      (unless (or (eq :set-session-subscriptions command.kind) (gethash "session_id" request))
        (setf (gethash "session_id" request) session-id))
      (nlk:check-sync-frame-size (nlk:encode-client-command command)))))

(defparameter *rpc-attach-seconds* 30
  "How long the shell looks for an organism serving its store, and waits for
that gateway's handshake, before it gives up.")

(define-error gateway-unreached (error) (port why)
  (:report "cannot reach the gateway on port ~d: ~a" port why)
  (:documentation "Nothing answered as a gateway on PORT before this shell
attached -- WHY, the dial's own words: the organism that published PORT went
between its sidecar's read and the dial, so another is looked for."))

(defun run-rpc (&key port token session-id cursor (input *standard-input*) (output *standard-output*)
                     on-end)
  "Attach to the gateway at PORT as SESSION-ID's shell, resumed past CURSOR
when given, then carry INPUT's command lines to it (RPC-COMMAND-TEXT) and its
sync frames to OUTPUT, one a line, until INPUT ends (:EOF) or the gateway lets
go (why, a string), which ON-END, when given, is handed before the socket is
let go. A gateway gone before the attach is GATEWAY-UNREACHED, signalled
before anything reaches OUTPUT."
  ;; A line that is not a command answers on OUTPUT as the gateway answers a
  ;; frame it refuses, a sync_protocol_error, and the next line is read. The
  ;; handshake frames are this shell's own business and never reach OUTPUT.
  (handler-case (ensure-gateway-session session-id port token)
    ((or usocket:socket-error stream-error) (condition)
      (error 'gateway-unreached :port port :why condition)))
  (let ((ws (gateway-client port token))
        (lock (bt2:make-lock :name "rpc-output"))
        (attached nil)
        (ready (bt2:make-semaphore :name "rpc-attached"))
        (done (bt2:make-semaphore :name "rpc-done"))
        (outcome (list nil)))
    (labels ((end (why)
               ;; The first end is the outcome: the input's, or the socket's.
               (sb-ext:compare-and-swap (car outcome) nil why)
               (bt2:signal-semaphore ready)
               (bt2:signal-semaphore done))
             (say (text)
               (handler-case (bt2:with-lock-held (lock)
                               (write-line text output)
                               (finish-output output))
                 (error (condition) (end (format nil "stdout: ~a" condition))))))
      (wsd:on :message ws
              (lambda (message)
                (if attached
                    (say message)
                    (handler-case
                        (let ((frame (nlk:decode-sync-message message)))
                          (case (nlk:sync-message-type frame)
                            (:gateway-connect-challenge
                             (answer-gateway-challenge ws frame "rpc" token))
                            (:gateway-connect-ack
                             (setf attached t)
                             (send-subscribe ws (list (list session-id :cursor cursor)))
                             (bt2:signal-semaphore ready))
                            (:gateway-connect-rejected
                             (end (or (gethash "message" (nlk:sync-message-payload frame))
                                      "the gateway refused the connection")))))
                      (error (condition) (end (princ-to-string condition)))))))
      (wsd:on :close ws
              (lambda (&key reason &allow-other-keys)
                (let ((why (format nil "the gateway closed the connection~@[: ~a~]"
                                   (and (plusp (length reason)) reason))))
                  ;; Closed before the ack is an owner that went as this
                  ;; shell dialled it.
                  (end (if attached why (make-condition 'gateway-unreached :port port :why why))))))
      ;; Severed, not closed off its reader: from this thread a close waits
      ;; out the parked reader's interruption, half a second on every exit.
      (with-cleanup ((ignore-errors (nlk:sever-websocket ws)))
        (handler-case (wsd:start-connection ws)
          (error (condition) (end (make-condition 'gateway-unreached :port port :why condition))))
        (bt2:wait-on-semaphore ready :timeout *rpc-attach-seconds*)
        (unless (or attached (car outcome))
          (end "the gateway never answered the handshake"))
        (when (typep (car outcome) 'gateway-unreached)
          (error (car outcome)))
        (when attached
          (spawn "rpc-input"
            (loop for line = (handler-case (read-line input nil nil)
                               (error (condition) (end (format nil "stdin: ~a" condition)) nil))
                  while (and line (null (car outcome)))
                  do (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
                       (when (plusp (length text))
                         (handler-case (wsd:send ws (rpc-command-text text session-id))
                           (nlk:protocol-error (condition)
                             (say (nlk::encode-sync-protocol-error
                                   (string-downcase (nlk:protocol-error-code condition))
                                   (nlk:protocol-error-message condition))))
                           (error (condition) (end (format nil "send: ~a" condition))))))
                  finally (end :eof))))
        ;; Parked for the connection's life, a ping each idle beat so the
        ;; gateway's read timeout never takes a quiet program for a dead one,
        ;; and a ping a dead socket refuses is the gateway's end.
        (loop until (bt2:wait-on-semaphore done :timeout *sync-keepalive-seconds*)
              do (handler-case (wsd:send-ping ws)
                   (error (condition) (end (format nil "the gateway is gone: ~a" condition)))))
        (when on-end (funcall on-end (car outcome)))
        (car outcome)))))

(defun store-organism-port (path deadline &key (held-p #'nlk::store-lease-held-p)
                                              (published #'live-published-endpoint)
                                              (start #'front-organism))
  "The port the organism serving the store at PATH publishes, once one does,
or NIL past DEADLINE (DEADLINE-AFTER's): a live owner's at once; else, each
time no process holds the store, one this shell starts (START)."
  ;; A store held with nothing published is an owner on its way in, booting,
  ;; or on its way out, stopping. Either is waited out, never taken for one
  ;; that serves: the moment it lets go another is started, and a candidate
  ;; that lost to a shell starting one beside it leaves that one's port to
  ;; read. One look and one candidate lost every launch that came while an
  ;; owner stopped (#226).
  (wait-until (lambda ()
                (or (funcall published path)
                    (unless (ignore-errors (funcall held-p path))
                      (funcall start path)
                      (funcall published path))))
              deadline 0.01))

(defun rpc-main (session-id cursor store-path)
  "`nodecode rpc': attach to the organism serving STORE-PATH, or start one
this shell fronts, and carry a program's lines both ways (RUN-RPC) on
SESSION-ID, or a fresh session, resumed past CURSOR when given. Exits 0 when
stdin ends; signals the reason when the gateway lets go first."
  ;; Runs before any folder loads: the organism is a process of its own and
  ;; this one only carries lines. Stdout is the program's, so anything else
  ;; this process would print goes to stderr.
  (let* ((path (merge-pathnames (or store-path nlk::*default-store-path*)))
         (output *standard-output*)
         (*standard-output* *error-output*))
    (let ((deadline (deadline-after *rpc-attach-seconds*))
          (session-id (or session-id (fresh-session-id)))
          (outcome nil))
      (with-cleanup ((stop-front :why (and (eq outcome :eof) "quit")))
        (loop
          (let ((port (or (store-organism-port path deadline)
                          (error "no organism serves ~a" (namestring path)))))
            (handler-case
                (return (setf outcome (run-rpc :port port :token (gateway-configured-operator-token)
                                               :session-id session-id :cursor cursor :output output
                                               ;; Told the moment its program lets go, before
                                               ;; this shell's own teardown: told after it,
                                               ;; the organism advertised itself meanwhile and
                                               ;; took the next program's shell only to drop it
                                               ;; as it went (#226).
                                               :on-end (lambda (why)
                                                         (when (eq why :eof) (tell-front "quit"))))))
              ;; Its owner went between the sidecar's read and the dial: a
              ;; handover like any other, waited out until the deadline.
              (gateway-unreached (condition)
                (when (>= (get-internal-real-time) deadline)
                  (error "no organism serves ~a: ~a" (namestring path) condition))
                (sleep 0.05))))))
      (unless (eq outcome :eof)
        (error "~a" outcome))
      (uiop:finish-outputs)
      (uiop:quit 0 nil))))
