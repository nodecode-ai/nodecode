;;;; warm.lisp --- the dump runs the launch once, so the operator never does.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded by nodecode/bin only, at the end of the release build,
;;;; immediately before program-op dumps the image. First calls are where
;;;; SBCL compiles at runtime — discriminating functions, effective methods,
;;;; MAKE-INSTANCE constructors — and a freshly dumped image makes the
;;;; operator's first launch pay all of them: ~140 ms inside the first
;;;; control request alone, measured against a live local gateway, against
;;;; ~1 ms for the second. Running the launch path here instead leaves that
;;;; compiled dispatch in the core, where every launch finds it.
;;;;
;;;; Hermetic by construction: every startup parameter naming an operator
;;;; file is NIL for every thread, the HTTP round trips go to the gateway's own
;;;; listener over a throwaway store under the temp directory, the
;;;; connection pool is cleared, the listener stopped and its workers waited
;;;; out, and the store deleted before returning — the dumped image carries
;;;; compiled code and dispatch caches, never a socket, a thread, or a
;;;; build-machine fact. A failure here fails the build out loud: a
;;;; dump that could not warm is a dump whose launch cost regressed.

(in-package #:nodecode.evolved)

;; The operator's files, out of every thread's reach: a LET binds this thread
;; alone, and the gateway's workers the attach below dials read the global
;; values. Each is a startup parameter, which the image computes again at its
;; start.
(dolist (name '(nlk::*cells-directory* nlk::*layer-cache-directory* *shared-config-path*
                *theme-selection-path* *composer-history-path* *keybinds-path*
                *composer-catalog-cache-path* *models-catalog-path* *provider-models-cache-path*))
  (setf (symbol-value name) nil))

;; The attach launch once against the gateway, as the header says; an error reaches
;; the build's real stderr with its backtrace before it propagates out of the silence.
(let ((stderr *error-output*))
  (handler-bind ((error (lambda (condition)
                          (ignore-errors
                           (format stderr "~&warm-image: ~a~%" condition)
                           (uiop:print-backtrace :stream stderr :condition condition)
                           (finish-output stderr)))))
    (let ((*gateway-commands* '())
          (*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream)))
      (nlk:load-cells)
      ;; The gateway is this image's own listener over a throwaway store, so the
      ;; owner's side of the attach warms with the shell's: clack, hunchentoot,
      ;; flexi-streams and chunga would otherwise compile their dispatch inside
      ;; every owner boot's first requests, with the shell's session ensure
      ;; waiting behind it.
      ;; A worker whose client goes quiet waits out the read timeout: a second
      ;; here, the image's own value restored after.
      (let ((store (ephemeral-store-path))
            (*gateway-connection-read-timeout* 1)
            (timeout hunchentoot:*default-connection-timeout*))
        (nlk:open-store :path store)
        (unwind-protect
             (let* ((port (open-gateway-listener
                           :operator-token "warm-token"
                           :workspace-captures nlk::*workspace-capture-enabled*))
                    (sid (ensure-gateway-session (fresh-session-id) port "warm-token"))
                    (model (make-instance 'gateway-chat-model
                                          :session-id sid :ws nil
                                          :http-port port :http-token "warm-token"))
                    (renderer (nlt::make-renderer
                               :output-stream (make-broadcast-stream)
                               :width 120 :height 40)))
               (engage-theme)
               (dolist (cmd (remove-if-not #'functionp (uiop:ensure-list (nlt:init model))))
                 (let ((msg (funcall cmd)))
                   (when (and msg (not (nlt:terminal-io-msg-p msg)))
                     (nlt:update model msg))))
               (nlt:update model (nlt:make-window-size-msg :width 120 :height 40))
               (nlt::render renderer (nlt:view model))
               (editor-insert model.editor "warm")
               (nlt:update model (nlt:make-tick-msg :time 0.05))
               (nlt::render renderer (nlt:view model))
               (editor-reset model.editor)
               (nlt::render renderer (nlt:view model))
               ;; And the sync stream every shell attaches by, the organism
               ;; running in a process of its own: the WebSocket upgrade, the
               ;; challenge, the subscribe and the replay folded into the
               ;; shell, websocket-driver's client and server dispatch with
               ;; them -- a launch's first attach paid it on its handshake.
               ;; The replay's ready frame arms the shell's one boot trim
               ;; (TRIM-BEHIND-THE-READY-FRAME), a thread no dump may hold
               ;; and a flag no launch may inherit set: held off here, and
               ;; left unarmed for the image.
               (setf *boot-trim-done* t)
               (let* ((replayed (bt2:make-semaphore :name "warm-replayed"))
                      (ws (gateway-client port "warm-token")))
                 (wsd:on :message ws
                         (lambda (msg &aux (frame (decode-sync-message msg)))
                           (case (nlk:sync-message-type frame)
                             (:gateway-connect-challenge
                              (answer-gateway-challenge ws frame "tui" "warm-token" "warm"))
                             (:gateway-connect-ack
                              (send-subscribe ws (subscription-entries model)))
                             (:projection
                              (nlt:update model (make-instance 'gw-projection-msg :frame frame))
                              (bt2:signal-semaphore replayed)))))
                 (wsd:start-connection ws)
                 ;; CLOSE-CONNECTION closes the socket, then destroys the read
                 ;; thread it saw alive -- which that close has just ended, so
                 ;; on a loaded box the destroy finds it exited and signals
                 ;; (both arms of the 7de1bc51d publish, built side by side).
                 ;; An exited reader is the state the close is for.
                 (with-cleanup ((handler-case (wsd:close-connection ws)
                                  (bt2:bordeaux-threads-error () nil)))
                   (unless (bt2:wait-on-semaphore replayed :timeout 10)
                     (error "warm-image: the sync stream never replayed"))))
               (setf *boot-trim-done* nil)
               (nlt::render renderer (nlt:view model))
               (fresh-session-id))
          ;; Order carries the shutdown: dropping the pool closes the client
          ;; side, which ends each kept-alive request loop; the stop joins the
          ;; acceptor; the workers are waited out, bounded, and a dump they
          ;; would outlive is refused out loud.
          (dex:clear-connection-pool)
          (stop-gateway)
          (setf hunchentoot:*default-connection-timeout* timeout)
          (nlk:close-store)
          (dolist (suffix '("" "-wal" "-shm" "-journal" ".lock" ".owner"))
            (ignore-errors (delete-file (concatenate 'string (namestring store) suffix))))
          (flet ((workers ()
                   (remove-if-not (lambda (thread)
                                    (search "hunchentoot-worker" (or (sb-thread:thread-name thread) "")))
                                  (sb-thread:list-all-threads))))
            (loop repeat 150 while (workers) do (sleep 0.02))
            ;; Without this, a worker still in a kept-alive read after that
            ;; would wait for a read timeout SBCL never applies to a socket
            ;; on Windows: end it, and its socket with it.
            (dolist (worker (workers))
              (ignore-errors (sb-thread:terminate-thread worker)))
            (loop repeat 150 while (workers) do (sleep 0.02))
            (when (workers)
              (error "warm-image: a gateway worker did not stop"))))))))
