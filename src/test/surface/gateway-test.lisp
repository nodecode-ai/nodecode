;;;; gateway-test.lisp --- G-series gate: WebSocket gateway and client.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   G2a. session_stream_resumed arrives with an empty events array when
;;;;        the session has no history.
;;;;   G2b. session_stream_resumed arrives with typed durable events when the
;;;;        store has real persisted history, and G2c. a reconnecting client
;;;;        receives the same typed events (one gate, G2C-...).
;;;;
;;;; Tests run without a provider API key: turns are not executed. Store
;;;; seeding goes through the kind owners.

(in-package #:nodecode.test)

(nlk:access (admission nlk::active-input-admission) (gws nle::gw-session) (other nle::gw-session)
            (second nle::picker-dialog))

;;; --- fixtures ------------------------------------------------------------

(defun temp-gateway-port ()
  "Ask the kernel for a currently-free port: bind 127.0.0.1:0, read the
assignment, release."
  ;; The old pid+random draw could hand two consecutive fixtures the same
  ;; port, and hunchentoot's ADDRESS-IN-USE-ERROR signals on its own acceptor
  ;; thread where --non-interactive turns it into image death; the kernel's
  ;; ephemeral allocator never assigns a port that is currently listening or
  ;; lingering.
  (nle::local-port 0))

(defmacro with-gateway-scratch ((dir tag &key store) &body body)
  "WITH-TEMP-DIRECTORY for the gates that open a store and start the gateway
by hand: whatever BODY started and opened is stopped and closed on the way
out — gateway first — before the tree goes."
  ;; STORE non-NIL opens DIR's history.db inside the cleanup, before BODY.
  `(with-temp-directory (,dir ,tag :close-store t)
     (nlk:with-cleanup ((ignore-errors (nle:stop-gateway)))
       ,@(when store `((nlk:open-store :path (merge-pathnames "history.db" ,dir))))
       ,@body)))

(defmacro with-temp-gateway ((port-var) &body body &aux (dir (gensym "DIR")))
  "Open a store, start the gateway on a kernel-assigned port, run BODY, and
clean up."
  ;; The store is opened before the gateway thread starts so the gateway can
  ;; serve replay requests immediately. PORT-VAR is bound to the port
  ;; START-GATEWAY returns — the canonical :port 0 random-port path, so every
  ;; test through this macro exercises it.
  `(with-gateway-scratch (,dir "gw" :store t)
     (let ((,port-var (nle:start-gateway :port 0 :operator-token "test-token"
                                         :workspace-captures nil)))
       (declare (ignorable ,port-var))
       ;; start-gateway's acceptance latch has already confirmed the
       ;; accept loop is live; no settle sleep is needed or wanted.
       ,@body)))

(defmacro with-gateway-session ((port session-id &rest create-keys) &body body)
  "WITH-TEMP-GATEWAY over a store holding one created session, SESSION-ID;
CREATE-KEYS reach CREATE-SESSION."
  `(with-temp-gateway (,port)
     (nlk:create-session :id ,session-id ,@create-keys)
     ,@body))

(defmacro is-eventually (form description &key (seconds 5))
  "Wait up to SECONDS for FORM to answer true, then assert FORM under
DESCRIPTION — the settle-then-check a gate on another thread's work writes."
  `(is (await (:tries ,(* 10 seconds)) ,form) ,description))

(defun detached-shell (&optional (session-id "s"))
  "A gateway shell on SESSION-ID with neither a socket nor an endpoint."
  (make-instance 'nle:gateway-chat-model :session-id session-id :ws nil))

(defun gateway-shell (port session-id &key ws ui)
  "A gateway shell on SESSION-ID whose control calls reach PORT's gateway under
the fixture's operator token; WS stands in for its socket, and UI non-NIL
initializes its terminal widgets."
  (let ((model (make-instance 'nle::gateway-chat-model :session-id session-id :ws ws
                                                       :http-port port :http-token "test-token")))
    (when ui (nle::initialize-chat-model-ui model))
    model))

(deftest gateway-stop-joins-the-acceptor (with-temp-gateway (port))
  ;; CLACK:STOP is DESTROY-THREAD plus a fixed sleep; hunchentoot's own stop
  ;; runs inside the dying handler thread. STOP-GATEWAY returning while that
  ;; thread is still alive is the Ctrl-C incident: image exit reaps the
  ;; listener and handler in arbitrary order and the handler's cleanup
  ;; prints a JOIN-THREAD-ERROR backtrace as its last act.
  (let ((handler-thread (clack.handler::handler-acceptor nle::*gateway-handler*)))
    (is (bt2:thread-alive-p handler-thread) "handler thread alive while serving")
    (nle:stop-gateway)
    (is (not (bt2:thread-alive-p handler-thread)))))

(defun ws-collect-frames (url &key (timeout 2) send-commands subscribe pid headers
                                   (token "test-token"))
  "Open a WS client to URL, its upgrade carrying HEADERS and its connect TOKEN,
subscribe it to SUBSCRIBE (SUBSCRIBE-COMMAND's list, PID its pid) and call
SEND-COMMANDS with it when given, collect every frame until no frame arrives
within TIMEOUT seconds, and return the list of parsed JSON hash-tables in
arrival order."
  (let ((inbox (sb-concurrency:make-mailbox :name "frames"))
        (ws (wsd:make-client url :additional-headers headers))
        (commands-sent nil))
    ;; Collect every inbound text frame and complete the canonical handshake.
    ;; A frame is posted once the handshake step it drives has run, so the
    ;; drain below never takes the wait SEND-COMMANDS spends for silence.
    (wsd:on :message ws
            (lambda (msg &aux (parsed (handler-case (shasht:read-json msg) (error () nil))))
              (when (hash-table-p parsed)
                (unless commands-sent
                  (let ((type (gethash "type" parsed)))
                    (cond ((string= type "gateway_connect_challenge")
                           (wsd:send ws (nlk:encode-gateway-connect
                                         (gethash "challenge_nonce" parsed)
                                         "test-client" "test" "test" "linux" "test-instance"
                                         :operator-token token)))
                          ((string= type "gateway_connect_ack")
                           (setf commands-sent t)
                           (when subscribe (send-command ws (subscribe-command subscribe :pid pid)))
                           (when send-commands (funcall send-commands ws))))))
                (sb-concurrency:send-message inbox parsed))))
    ;; Start the connection in a thread so we can drive it from here.
    (let* ((conn-thread
             (nlk:spawn "ws-collect-frames" (ignore-errors (wsd:start-connection ws))))
           ;; Drain frames until TIMEOUT with no new arrival.
           (frames (loop for frame = (sb-concurrency:receive-message inbox :timeout timeout)
                         while frame collect frame)))
      (ignore-errors (wsd:close-connection ws))
      (ignore-errors (bt2:join-thread conn-thread))
      (nconc frames (sb-concurrency:receive-pending-messages inbox)))))

(defmacro frames-while ((ws port &key (timeout 2) subscribe pid) &body body)
  "Every frame one client of PORT's gateway sees while BODY, with WS bound
to the client, drives it after the handshake — WS-COLLECT-FRAMES with the
URL and the sender's lambda spelled once."
  `(ws-collect-frames (format nil "ws://127.0.0.1:~d/gateway" ,port) :timeout ,timeout
                      :subscribe ,subscribe :pid ,pid
                      :send-commands (lambda (,ws) (declare (ignorable ,ws)) ,@body)))

(defun feed-wire (model wire)
  "Hand MODEL one WIRE frame the way the loop thread does; answers MODEL."
  (nlt:update model (make-instance 'nle::gw-projection-msg
                                   :frame (nlk:decode-sync-message wire)))
  model)

(defun fact-frame (kind turn-id &key (session-id "s") (seq 5) payload command-id
                                     (occurred-at "2026-08-21T00:00:00.000Z"))
  "One durable fact of TURN-ID as a replay or the live stream carries it: SEQ
is its sequence, its log position and the number its event id carries."
  (durable-fact-frame :seq seq :session-id session-id :turn-id turn-id :kind kind
                      :payload payload :event-id (format nil "event-~d" seq) :log-position seq
                      :command-id command-id :occurred-at occurred-at))

(defun resumed-frame (facts &key (sequence 3) (cursor 0) (admission "ready"))
  "Session \"s\"'s replay of FACTS as one final session_stream_resumed chunk."
  (nlk:encode-session-stream-resumed "s" sequence (coerce facts 'vector)
                                     :cursor cursor :admission-state admission
                                     :replay-chunk-index 0 :replay-chunk-final-p t))

(defun subscribe-command (session-ids &key pid)
  "Return a canonical typed subscription command; PID, when given, rides it
as the shell's process the way SEND-SUBSCRIBE sends it."
  ;; An entry of SESSION-IDS is an id or, as SEND-SUBSCRIBE takes it, (ID
  ;; :LIVE-ONLY T).
  (nlk:encode-client-command
   (nlk:make-gateway-command
    :kind :set-session-subscriptions
    :request (nlk:json-object
              "subscriptions" (map 'vector (lambda (entry)
                                             (destructuring-bind (sid &key live-only)
                                                 (uiop:ensure-list entry)
                                               (nlk:json-object "session_id" sid
                                                                :opt "live_only" live-only)))
                                   session-ids)
              :opt "pid" pid))))

(defun send-command (ws command)
  "Send an already encoded canonical command to WS."
  (wsd:send ws command))

(defun frame-type (frame)
  "The payload discriminator one wire frame carries, or NIL."
  ;; NLK:JSON-VALUE
  ;; already declines a non-object at every step, so no reader here needs its own
  ;; HASH-TABLE-P guard between the frame and its payload.
  (nlk:json-value frame :string "payload" "type"))

(defun frames-of-type (frames kind)
  "The FRAMES whose payload type is KIND, in arrival order."
  (remove-if-not (lambda (frame) (equal kind (frame-type frame))) frames))

(defun replay-frames (frames)
  "Flatten bounded replay chunks in wire order."
  (loop for chunk in (frames-of-type frames "session_stream_resumed")
        append (coerce (nlk:json-value chunk :array
                                       "payload" "accumulated_frames")
                       'list)))

(defun subscribed-frames (port session-id &key (timeout 2))
  "Every frame one client sees after subscribing to SESSION-ID and sending
nothing else."
  ;; Gates that send a second command on the same connection stay on
  ;; FRAMES-WHILE on purpose: their ordering against the replay is the thing
  ;; they pin, and folding them in here would erase it.
  (frames-while (ws port :timeout timeout :subscribe (list session-id))))

(defun frame-carrying (frames value &rest path)
  "The first of FRAMES whose payload carries VALUE, a string, at PATH — or NIL."
  (find value frames :test #'equal
                     :key (lambda (frame) (apply #'nlk:json-value frame :string "payload" path))))

(defun find-frame (frames kind)
  "Return the first canonical projection frame of payload type KIND."
  (frame-carrying frames kind "type"))

(defun install-silent-peer (peer &aux (state (nle::make-gw-peer-state :socket peer)))
  "Register a peer state with no writer thread so queued frames stay inspectable."
  (sb-ext:with-locked-hash-table (nle::*gateway-peer-states*)
    (setf (gethash peer nle::*gateway-peer-states*) state))
  state)

(defmacro with-silent-peer ((peer state name) &body body)
  "Run BODY with PEER a fresh silent peer — a list holding NAME, an identity
the registry keys on — and STATE its registered peer state, dropped from the
registry however BODY leaves."
  `(let* ((,peer (list ,name))
          (,state (install-silent-peer ,peer)))
     (declare (ignorable ,state))
     (nlk:with-cleanup ((nle::stop-gw-peer ,peer)) ,@body)))


;;; --- gate: G2a — empty session replay -----------------------------------

(deftest g2a-session-stream-resumed-with-empty-session (with-gateway-session (port "g2a-empty"))
  (is-present (resumed (find-frame (subscribed-frames port "g2a-empty") "session_stream_resumed"))
      "session_stream_resumed frame arrived"
      (is (equal "g2a-empty" (gethash "session_id" resumed)))
      (let ((replay (gethash "accumulated_frames" (gethash "payload" resumed))))
        (is (and (vectorp replay) (= 1 (length replay)))))))

(deftest g2d-a-fork-standing-by-is-replayed-the-turns-it-carries (with-temp-gateway (port))
  ;; A fork's replay was empty until its first prompt made it durable: a shell
  ;; that /fork switched to sat on the home screen over the history it forks.
  (seed-completed-turn "g2d-parent" "hello" "answer")
  (nlk:standby-session :id "g2d-fork" :parent "g2d-parent")
  (let ((frames (subscribed-frames port "g2d-fork")))
    (is (find "hello" (replay-frames frames) :test #'equal
                                             :key (lambda (frame)
                                                    (nlk:json-value frame :string "event" "payload" "message"))) "the parent's prompt is in it")
    (is (eq t (nlk:json-value (car (last (frames-of-type frames "session_stream_resumed")))
                              :boolean "payload" "replay_chunk_final")) "and the replay ends")))

;;; --- the organism's own voice: NLE:NOTICE ----------------------------------
;;; A refusal or a state change reaches every attached shell as a frame, and
;;; with a key it stands: a shell attaching later gets the board right after
;;; its first replay, so the rows land after the replayed transcript.

(deftest gateway-notice-reaches-attached-peers-and-stands-for-late-ones ()
  (with-gateway-session (port "notice-s")
    (let ((frames
            (frames-while (ws port :subscribe '("notice-s"))
              (is (equal "discord: refused — no token"
                         (nle:notice "discord: refused — no token"
                                     :level :error :key "channels.discord")))
              (nle:notice "said once"))))
      (is-present (frame (frame-carrying frames "discord: refused — no token" "text"))
          "the keyed notice reached the attached peer"
          (is-shape frame (frame-type "organism.notice") ((:string "payload" "level") "error")
            ((:string "payload" "key") "channels.discord")
            ("sequence" eql 0 "outside the session's sequence")
            ("session_id" null "and session-less")))
      (is (frame-carrying frames "said once" "text") "so did the transient one"))
    (is (equal '("channels.discord") (mapcar #'first (nlk:standing-notices))))
    ;; A second shell attaches after the fact.
    (let* ((frames (subscribed-frames port "notice-s"))
           (resumed (position "session_stream_resumed" frames :key #'frame-type :test #'equal))
           (notice (position "organism.notice" frames :key #'frame-type :test #'equal)))
      (is (and resumed notice (< resumed notice)))
      (is (= 1 (length (frames-of-type frames "organism.notice")))))
    (nle:notice nil :key "channels.discord")
    (is (null (nlk:standing-notices)) "a NIL text clears the key")))

(deftest gateway-notice-puts-up-a-toast (let ((model (detached-shell))))
  ;; The client side: the frame becomes a toast, error-level one that stays,
  ;; a keyed one in place of the toast under its key, before any watermark
  ;; check — and never a transcript row.
  (with-stubbed-fdefinition (nle::render-state (m) nil)
    (setf (nle::gateway-model-last-sequence model) 9)
    (loop for (text level error-p) in '(("discord: refused — no token" "error" t)
                                        ("discord: running, connected" "info" nil))
          do (feed-wire model (nlk:encode-unsequenced-frame "organism.notice"
                               (nlk:json-object "text" text "level" level "key" "discord")))
             (let ((toast (first (nle::chat-model-toasts model))))
               (is-present (toast toast) (format nil "~s is a toast" text)
                 (is-shape toast (nle::toast-text text) (nle::toast-stays eq error-p)))
               (is (not (transcript-carries-p (nle::chat-model-state model) text)) "no row")))
    ;; The second under the same key took the first one's place.
    (is (= 1 (length (nle::chat-model-toasts model))))
    (is (eql 9 (nle::gateway-model-last-sequence model)))))

(deftest gateway-loaded-reaches-the-shells-in-other-processes ()
  ;; While the gateway serves, NLK:SOURCE-LOADED sends every attached shell
  ;; the file and the names, session-less and unsequenced like a notice, and
  ;; answers how many were told.
  (with-gateway-session (port "loaded-s")
    (let* ((told nil)
           (shells nil)
           (frames (frames-while (ws port :subscribe '("loaded-s") :pid 424242)
                     (await (:tries 50) (nle::attached-shells))
                     (setf shells (nle::attached-shells)
                           told (nlk:source-loaded
                                 "/tree/probe.lisp"
                                 '(("DEFUN" . "probe-a")
                                   ("DEFPARAMETER" . "*probe-b*")))))))
      (is (equal '(("loaded-s" 424242)) shells))
      (is (eql 1 told) "the seam counts the one attached shell")
      (is-present (frame (find-frame frames "organism.loaded")) "the frame reached the peer"
        (is (equal "/tree/probe.lisp" (nlk:json-value frame :string "payload" "path")))
        (is (equal '(("DEFUN" . "probe-a") ("DEFPARAMETER" . "*probe-b*"))
                   (map 'list (lambda (entry)
                                (cons (gethash "operator" entry) (gethash "label" entry)))
                        (nlk:json-value frame :array "payload" "definitions"))))
        (is (eql 0 (gethash "sequence" frame)) "outside the session's sequence")
        (is (null (gethash "session_id" frame)) "and session-less")))))

(defmacro with-loaded-probe ((model path session-id file forms) &body body &aux (root (gensym)))
  "BODY with MODEL a detached shell on SESSION-ID and PATH, FILE in a source tree, holding FORMS."
  `(with-source-tree (,root)
     (let ((,model (detached-shell ,session-id)) (,path (merge-pathnames ,file ,root)))
       (write-temp-file
        ,path (format nil ,(concatenate 'string "(in-package #:nodecode.evolved)~%~%" forms)))
       (with-stubbed-fdefinition (nle::render-state (m) nil) ,@body))))

(deftest gateway-loaded-folds-into-the-shell-as-a-load
    (with-loaded-probe (model path "s-mine" "loaded-probe.lisp"
                        "(defun loaded-probe-a () :a)~%~%~
                         (defun loaded-probe-b () :b)~%~%~
                         (defun loaded-origin-a () :a)~%~%~
                         (defun loaded-origin-b () :b)~%~%~
                         (defun loaded-origin-c () :c)~%"))
  ;; The client side: a shell in another process evaluates the named
  ;; definitions from its own copy of the file — and only those — and says
  ;; so as a system row; a file outside its trees loads nothing and says
  ;; nothing, which is what a shell on another machine sees.
  ;;
  ;; The client side of GATEWAY-LOADED-NAMES-THE-SESSION-WHOSE-TURN-LOADED's
  ;; rule: the shell evaluates what any session's turn made live -- its image
  ;; is the one that changed -- and
  ;; draws the system row only for the session it shows, or for a load the
  ;; payload gives no session (the operator's own, outside any turn).
  ;; Another session's edit changes the image silently: that session's
  ;; receipt already carries the load.
  (flet ((live-p (name &aux (symbol (uiop:find-symbol* name '#:nodecode.evolved nil)))
           (and symbol (fboundp symbol)))
         (rows ()
           (nle::transcript-rows (nle::chat-model-state model))))
    (feed-wire model (loaded-frame (namestring path) "loaded-probe-a"))
    (is (eq :a (funcall (uiop:find-symbol* "LOADED-PROBE-A" '#:nodecode.evolved))))
    (is (not (fboundp (uiop:find-symbol* "LOADED-PROBE-B" '#:nodecode.evolved))))
    (is-present (row (find "loaded-probe.lisp — this shell: live here: loaded-probe-a" (rows)
                           :key #'nle::transcript-row-text :test #'search))
        "the shell says what it loaded, as a system row"
        (is (eq :system-notice (nle::transcript-row-kind row))))
    (let ((count (length (rows))))
      (feed-wire model (loaded-frame "/nowhere/else.lisp" "loaded-probe-b"))
      (is (= count (length (rows))) "a file outside this image's trees says nothing")
      (is (not (fboundp (uiop:find-symbol* "LOADED-PROBE-B" '#:nodecode.evolved)))))
    (let ((count (length (rows))))
      (feed-wire model (loaded-frame (namestring path) "loaded-origin-a" "s-other"))
      (is (live-p "LOADED-ORIGIN-A") "another session's edit is live here")
      (is (= count (length (rows))) "and says nothing in this session")
      (feed-wire model (loaded-frame (namestring path) "loaded-origin-b" "s-mine"))
      (is (live-p "LOADED-ORIGIN-B"))
      (is (= (1+ count) (length (rows))) "this session's own edit draws the row")
      (is-present (row (find "loaded-origin-b" (rows)
                             :key #'nle::transcript-row-text :test #'search))
          "naming what went live"
          (is (eq :system-notice (nle::transcript-row-kind row))))
      (feed-wire model (loaded-frame (namestring path) "loaded-origin-c" nil))
      (is (live-p "LOADED-ORIGIN-C"))
      (is (= (+ 2 count) (length (rows))) "a load outside any turn draws it too"))))

(deftest gateway-loaded-puts-back-a-load-that-breaks-the-frame (with-source-tree (root))
  ;; The fold verifies the shell's own frame: a load that breaks a frame which
  ;; composed is put back, and the row says so.
  (let ((model (detached-shell "s-mine"))
        (path (merge-pathnames "frame-probe.lisp" root)))
    (nle::initialize-chat-model-ui model)
    (flet ((probe () (funcall (uiop:find-symbol* "LOADED-FRAME-PROBE" '#:nodecode.evolved)))
           (rows () (nle::transcript-rows (nle::chat-model-state model))))
      (write-temp-file path (format nil "(in-package #:nodecode.evolved)~%~%~
                                         (defun loaded-frame-probe () :painted)~%"))
      (nlk:load-source path)
      (with-stubbed-fdefinition (nle::render-state (m) (probe))
        (write-temp-file path (format nil "(in-package #:nodecode.evolved)~%~%~
                                           (defun loaded-frame-probe () (error \"no frame\"))~%"))
        (feed-wire model (loaded-frame (namestring path) "loaded-frame-probe"))
        (is (eq :painted (probe)) "the shell keeps the definition that paints")
        (is-present (row (find "put back" (rows) :key #'nle::transcript-row-text :test #'search))
            "and says the load was put back"
            (is (search "no frame" (nle::transcript-row-text row))))))))

(deftest gateway-loaded-names-the-session-whose-turn-loaded ()
  ;; Without this, a `[ui.lisp — this shell: live: …]' row would be added
  ;; after each edit to the transcript of the session the operator is typing
  ;; in. The frame is still for every shell -- the image is one -- but its
  ;; payload names the session whose turn made the definitions live, the turn
  ;; bound on the announcing thread, so a shell keeps the row to that
  ;; session. Outside any turn the payload names none; the envelope is
  ;; session-less either way.
  (with-gateway-session (port "loaded-origin")
    (let* ((frames (frames-while (ws port :subscribe '("loaded-origin") :pid 424243)
                     (await (:tries 50) (nle::attached-shells))
                     (let ((nle::*current-durable-turn*
                             (nlk::%make-durable-turn :session-id "s-editor" :turn-id "t"
                                                      :command-id "c"
                                                      :definition-revision "base")))
                       (nlk:source-loaded "/tree/probe.lisp" '(("DEFUN" . "probe-a"))))
                     (nlk:source-loaded "/tree/probe.lisp" '(("DEFUN" . "probe-b")))))
           (loaded (frames-of-type frames "organism.loaded")))
      (flet ((frame-of (label)
               (find label loaded
                     :test #'equal
                     :key (lambda (frame)
                            (let ((definitions (nlk:json-value frame :array "payload" "definitions")))
                              (and (vectorp definitions) (plusp (length definitions))
                                   (gethash "label" (aref definitions 0))))))))
        (is (= 2 (length loaded)) "both loads reached the peer")
        (is-present (frame (frame-of "probe-a")) "the load a turn made"
          (is (equal "s-editor" (nlk:json-value frame :string "payload" "session_id")))
          (is (null (gethash "session_id" frame)) "and the envelope stays session-less"))
        (is-present (frame (frame-of "probe-b")) "the load outside any turn"
          (is (null (nlk:json-value frame :any "payload" "session_id"))))))))

;;; --- gate: G2b/G2c — seeded history replay, and a reconnect restores it ------

(deftest g2c-reconnect-sees-same-transcript (with-temp-gateway (port))
  (let ((sid "g2c-reconnect"))
    (seed-completed-turn sid "first question" "first answer")
    (nlk:bind ((frames1 (subscribed-frames port sid)) (frames2 (subscribed-frames port sid)))
      (is-present (both (and (find-frame frames1 "session_stream_resumed")
                             (find-frame frames2 "session_stream_resumed")))
        "both connections received session_stream_resumed"
        (let ((replay1 (coerce (replay-frames frames1) 'vector))
              (replay2 (coerce (replay-frames frames2) 'vector)))
          (is (>= (length replay1) 4) "the seeded history replays")
          (is (equal "turn.input_committed" (nlk:json-value (aref replay1 1) :any "event" "type")))
          (is (integerp (gethash "seq" (aref replay1 1))) "as typed, sequenced durable events")
          (is (= (length replay1) (length replay2)))
          (loop for i from 0 below (length replay1) do
            (is (equal (nlk:json-value (aref replay1 i) :any "event" "metadata" "event_id")
                       (nlk:json-value (aref replay2 i) :any "event" "metadata" "event_id"))
                (format nil "event ~d identical across reconnects" i))))))))

(deftest replay-admission-state-reflects-durable-turn-lifecycle ()
  (with-session-store ("gw-admission")
    (is (equal "ready" (nle::replay-admission-state "gw-admission")))
    (let ((turn (nlk:admit-turn "gw-admission" "gw-admission-command" "hello")))
      (is (equal "active" (nle::replay-admission-state "gw-admission")))
      (nlk:cancel-turn turn "test cancellation")
      (is (equal "ready" (nle::replay-admission-state "gw-admission"))))))

(deftest replay-frame-exposes-cursor-and-admission-state (with-session-store ("gw-admission-frame"))
  (is-let* ((before (nlk:event-count :session-id "gw-admission-frame"))
            (wire (nlk:encode-session-stream-resumed
                   "gw-admission-frame" 1 #() :cursor 1 :admission-state "ready"))
            (frame (nlk:decode-sync-message wire))
            (decoded frame.payload))
    (frame.cursor = 1 "cursor survives decoding") ((gethash "admission_state" decoded) "ready")
    ((nlk:event-count :session-id "gw-admission-frame") = before "and encoding writes no history")))

(deftest gateway-client-does-not-open-a-local-store ()
  "A Gateway client seeds from network replay and never acquires SQLite."
  (let ((state (nle::initial-chat-state)))
    (is (not (nlk:store-open-p)))
    (is-shape state (nle::transcript-rows null) ("messages" null))))

(deftest gateway-client-reducer-replay-clears-and-folds-events ()
  (let ((state (nle::fold-accumulated-frames
                (vector (nlk::make-json-object
                         "seq" 1 "event"
                         (nlk::encode-inner-event
                          "turn.input_committed"
                          (nlk::make-json-object "role" "user" "message" "hello")))))))
    (is (= 1 (length (nle::transcript-rows state))) "replay folds durable facts")
    (is (equal "hello" (nle::transcript-row-text
                        (first (nle::transcript-rows state)))))
    (is (null (nle::transcript-rows (nle::fold-accumulated-frames #()))))))

(deftest gateway-event-payload-preserves-typed-inner-event-envelope ()
  ;; The replay fold's input shape: EVENT-FACT-WIRE nests durable fields
  ;; under "payload" and attribution (turn_id, log_position, occurred_at)
  ;; under "metadata". Flattening dropped the metadata — the client half of
  ;; the boot-resume unkillable-turn hole — and merging instead would
  ;; collide with payloads that own a "metadata" field (turn.tool_result's
  ;; background_shell), so the envelope must pass through whole.
  (let* ((inner (nlk::encode-inner-event
                 "turn.tool_result"
                 (nlk::make-json-object
                  "call-id" "call-1" "tool-name" "snippet" "result" ""
                  "metadata" (nlk::make-json-object
                              "background_shell"
                              (nlk::make-json-object "status" "exited")))
                 :metadata (nlk::make-json-object
                            "turn_id" "turn-envelope" "log_position" 9)))
         (normalized (nle::gateway-event-payload inner)))
    (is (eq inner normalized) "a typed inner event passes through whole")
    (is (equal "turn-envelope"
               (gethash "turn_id" (gethash "metadata" normalized))))
    (is (equal "exited"
               (nlk:json-value normalized :any "payload" "metadata" "background_shell" "status")))))

(deftest gateway-client-replay-arms-esc-cancel-for-adopted-turn (let ((model (detached-shell))
                                                                      (cancels '())))
  ;; A shell that learns about a running turn only from the subscribe replay
  ;; — boot resume, gateway resurrection, session switch, a second client —
  ;; must arm the same cancel state a live turn.started or its own
  ;; start_turn ack arms. Without this, the replay's final "active"
  ;; admission would arm ACTIVE-TURN-ID to NIL while TURN-IN-FLIGHT is T:
  ;; the footer would say "esc interrupts" while Esc would silently arm the
  ;; Esc-Esc tree gesture.
  (with-stubbed-fdefinitions ((nle::send-cancel-turn (ws session-id turn-id &key retract)
                                (push (list session-id turn-id) cancels))
                              (nle::render-state (m) nil))
    (feed-wire model (resumed-frame
                      (list (fact-frame "turn.input_committed" "turn-resumed" :seq 1
                                        :command-id "command-1"
                                        :payload (nlk::make-json-object "role" "user"
                                                                        "message" "hello"))
                            (fact-frame "turn.started" "turn-resumed" :seq 2 :command-id "command-1"
                                        :payload (nlk::make-json-object "input" "hello")))
                      :sequence 7 :cursor 2 :admission "active"))
    (is-shape model (nle::gateway-model-active-turn-id "turn-resumed")
      (nle::chat-model-turn-in-flight is))
    (nlt:update model (nlt:make-key-press-msg :code :escape))
    (is (equal '(("s" "turn-resumed")) cancels))))

(deftest gateway-client-sequence-admission-deduplicates-and-detects-gaps ()
  (let ((model (detached-shell)))
    (setf (nle::gateway-model-last-sequence model) 4
          (nle::gateway-model-last-cursor model) 8)
    (is-table (verdict initargs)
      (eq verdict (nle::gateway-frame-accepted-p
                   model (apply #'nlk::make-sync-message :type :projection initargs)))
      (:duplicate '(:sequence 4) "an equal live sequence is not applied twice")
      (:duplicate '(:sequence 5 :cursor 8) "a durable cursor is receiver-side event identity")
      (:gap '(:sequence 6) "forward sequence jump is a gap")
      (:apply '(:sequence 5) "next sequence is applied"))))

(deftest gateway-client-gap-resubscribes-exactly-once-per-epoch (let ((model (detached-shell))
                                                                      (subscribes 0)))
  ;; The watermark stays put until the resumed snapshot replaces it, so every
  ;; in-flight frame behind a gap classifies :gap too. RECONNECT-REQUIRED must
  ;; gate the resubscribe or each queued frame fires another full replay job.
  (setf (nle::gateway-model-last-sequence model) 4)
  (with-stubbed-fdefinitions ((nle::send-subscribe (ws session-ids) (incf subscribes))
                              (nle::render-state (m) nil))
    (flet ((gap-frame (seq)
             (make-instance 'nle::gw-projection-msg
                            :frame (nlk::make-sync-message
                                    :type :projection :sequence seq
                                    :payload (nlk::make-json-object
                                              "type" "item_delta")))))
      (nlt:update model (gap-frame 6))
      (is (= 1 subscribes) "first gap triggers one resubscribe")
      (is (nle::gateway-model-reconnect-required model))
      (nlt:update model (gap-frame 7))
      (nlt:update model (gap-frame 9))
      (is (= 1 subscribes))
      ;; The resumed snapshot closes the epoch; a fresh gap re-arms.
      (nle::note-resumed-watermark
       model (nlk::make-sync-message :type :projection :sequence 1))
      (is (not (nle::gateway-model-reconnect-required model)))
      (nlt:update model (gap-frame 40))
      (is (= 2 subscribes) "a gap in the new epoch resubscribes once"))))

(defun frame-dropped-p (program model session &aux (released nil))
  "Whether dispatching one SESSION view delta through PROGRAM to MODEL drops it:
a dropped frame releases its ingress slot at once, a dispatched one hands the
release to UPDATE."
  (nle::dispatch-inbound-frame
   program model
   (nlk::encode-projection-event session 1 "session_input_queue_snapshot_updated" (nlk::make-json-object))
   :release (lambda () (setf released t)))
  released)

(deftest gateway-client-dispatch-filter-follows-session-switch ()
  ;; The /clear stall: the reader thread's session filter must read the
  ;; model's live session id, never a connect-time snapshot — after
  ;; SWITCH-GATEWAY-SESSION every frame the fresh session sends (its replay,
  ;; its start_turn command results, its stream) carries the new id, and a
  ;; frozen filter drops them all, so a submitted prompt spins forever.
  ;; Dispatch is observable through the :release callback: a dropped frame
  ;; releases its ingress slot immediately; a dispatched frame hands the
  ;; release to UPDATE.
  (let* ((model (detached-shell "boot-session"))
         (program (nlt:make-program model)))
    (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
      ;; The /clear leg: re-point the model at the fresh session.
      (nle::switch-gateway-session model "fresh-session"))
    (is (not (frame-dropped-p program model "fresh-session")))
    (is (frame-dropped-p program model "boot-session"))))

(deftest gateway-client-folds-live-and-durable-tool-calls (let ((state (nle::initial-chat-state))))
  (flet ((delta (arguments)
           (nlk::make-json-object
            "type" "item_delta"
            "delta" (nlk::make-json-object "type" "tool_call" "index" 0
                                            "name" "lookup" "arguments" arguments))))
    (feed-events state (delta "{") (delta "{\"q\":1}"))
    (let ((rows (nle::transcript-rows state)))
      (is (= 1 (length rows)))
      (is (equal "{\"q\":1}" (nle::transcript-row-tool-args (first rows))))))
  (feed-events state
    (nlk::encode-inner-event
     "turn.tool_call_started"
     (nlk::make-json-object "call-id" "call-1" "tool-name" "lookup"
                            "arguments" "{\"q\":1}")))
  (let ((row (first (nle::transcript-rows state))))
    (is-shape row (nle::transcript-row-hydrated is) (nle::transcript-row-tool-call-id "call-1")))
  (feed-events state (nlk::make-json-object "type" "turn.completed"))
  (is (null (gethash "tail_rows" state)))
  (is (= 2 (length (nle::transcript-rows state)))))

(deftest queued-prompt-folds-once-in-transcript ()
  ;; A follow-up INPUT_COMMITTED is queue state (a :pending-input row); its
  ;; promotion commits a second INPUT_COMMITTED with disposition "started"
  ;; and the same prompt id, which clears the pending row — the transcript
  ;; ends with exactly ONE user message for the queued prompt.
  (flet ((commit (sequence disposition)
           (nlk::encode-inner-event
            "turn.input_committed"
            (nlk::make-json-object "role" "user" "message" "queued prompt"
                                   "prompt-id" "prompt-q" "sequence" sequence
                                   "disposition" disposition)))
         (user-messages (rows)
           (count-if (lambda (row) (and (eq :text row.kind) (equal "user" row.role))) rows)))
    (let ((state nil))
      (feed-events state (commit 2 "follow-up"))
      (let ((rows (nle::transcript-rows state)))
        (is (= 1 (count :pending-input rows :key #'nle::transcript-row-kind)))
        (is (zerop (user-messages rows))))
      (feed-events state (commit 1 "started"))
      (let ((rows (nle::transcript-rows state)))
        (is (zerop (count :pending-input rows :key #'nle::transcript-row-kind)))
        (is (= 1 (user-messages rows)))))))

(deftest a-steer-cuts-the-turn-and-runs-next-in-the-transcript ()
  ;; The pending band shows a steer from the queue snapshot the moment it
  ;; is admitted. The running turn then rests on a `steered' divider — it
  ;; ended at a round boundary because the steer was parked — and the
  ;; steer's promotion starts the next turn: its started INPUT_COMMITTED
  ;; retires the band row and lands the message as that turn's first row.
  ;; The live stream and a replay fold the same frames, so both agree.
  (let ((state nil)
        (snapshot
          (nlk::make-json-object
           "type" "session_input_queue_snapshot_updated"
           "session_id" "s-steer"
           "snapshot" (nlk::make-json-object
                       "count" 0 "active_turn" "turn-s"
                       "pending_prompts"
                       (vector (nlk::make-json-object
                                "prompt_id" "p-steer" "command_id" "p-steer"
                                "content" "lol" "sequence" 1
                                "disposition" "steer"))))))
    (flet ((fold (kind payload &optional (turn "turn-s"))
             (setf state (nle::apply-gateway-event
                          state (nlk::encode-inner-event
                                 kind payload
                                 :metadata (nlk::make-json-object "turn_id" turn)))))
           (rows ()
             (mapcar (lambda (row) (list row.kind row.role row.text)) (nle::transcript-rows state))))
      (fold "turn.input_committed"
            (nlk::make-json-object "role" "user" "message" "yo" "prompt-id" "p-start"
                                   "sequence" 1 "disposition" "started"))
      (fold "session_input_queue_snapshot_updated" snapshot)
      (is (equal '((:text "user" "yo") (:pending-input "user" "lol")) (rows)))
      ;; The submitting shell painted the row first, and a reconnect replays
      ;; the snapshot: one band row per prompt id either way.
      (fold "session_input_queue_snapshot_updated" snapshot)
      (is (= 1 (count :pending-input (nle::transcript-rows state)
                      :key #'nle::transcript-row-kind)))
      (fold "turn.assistant_message_completed"
            (nlk::make-json-object "role" "assistant"
                                   "message" (nlk::make-json-object
                                              "role" "assistant" "content" "yo! what's up?")))
      (fold "turn.completed" (nlk::make-json-object "turn-number" 1 "message-count" 2 "steered" t))
      (is (equal '((:text "user" "yo")
                   (:pending-input "user" "lol")
                   (:text "assistant" "yo! what's up?")
                   (:finish-divider "system" "• steered"))
                 (rows)))
      (fold "turn.started" (nlk::make-json-object "input" "lol" "parent-turn-id" "turn-s") "turn-s2")
      (fold "turn.input_committed"
            (nlk::make-json-object "role" "user" "message" "lol" "prompt-id" "p-steer"
                                   "sequence" 1 "disposition" "started")
            "turn-s2")
      (is (equal '((:text "user" "yo")
                   (:text "assistant" "yo! what's up?")
                   (:finish-divider "system" "• steered")
                   (:text "user" "lol"))
                 (rows)))
      ;; A completion nobody cut rests on the plain divider.
      (fold "turn.completed" (nlk::make-json-object "turn-number" 2 "message-count" 2) "turn-s2")
      (is (equal '(:finish-divider "system" "• completed") (first (last (rows))))))))

(deftest turn-part-projector-projects-exactly-the-live-kinds (let ((published '())))
  ;; The projector's drop set is a deliberate contract, not an accident: only
  ;; accumulated live state crosses the item_delta wire, and brackets, round
  ;; metadata, signatures, assembled calls, errors, and finish stay with their
  ;; durable-fact/replay owners. Feeding the whole alphabet pins both halves.
  (with-stubbed-fdefinition (nle::publish-event (session-id kind payload &rest args)
                              (push (cons kind payload) published))
    (let ((project (nle::turn-part-projector "proj-s" "proj-t")))
      (dolist (kind '(:stream-start :response-metadata
                      :text-start :text-delta :text-end
                      :reasoning-start :reasoning-delta :reasoning-end
                      :reasoning-signature
                      :tool-input-start :tool-input-delta :tool-input-end
                      :tool-call :error :finish :stream-retry))
        (funcall project
                 (nle::make-stream-part :kind kind :text "x" :index 0
                                        :id "id-0" :tool-name "eval"
                                        :arguments "{}")))))
  (let ((frames (nreverse published)))
    (is (every (lambda (entry) (equal "item_delta" (car entry))) frames))
    (is (equal '("text" "reasoning" "tool_call" "tool_call" "stream_reset")
               (mapcar (lambda (entry) (nlk:json-value (cdr entry) :any "delta" "type")) frames)))
    (is (equal "" (nlk:json-value (cdr (third frames)) :any "delta" "arguments")))))

(deftest gateway-client-normalizes-json-null-wire-fields (let ((state (nle::initial-chat-state))))
  ;; A tool-only assistant round carries content: null on the wire, and tool
  ;; facts may carry null ids/results; shasht reads JSON null as the truthy
  ;; keyword :NULL, which an OR guard passes straight into the string-typed
  ;; transcript-row text slot (the event-loop TYPE-ERROR crash).
  (feed-events state
    (nlk::encode-inner-event
     "turn.assistant_message_completed"
     (nlk::make-json-object
      "message" (nlk::make-json-object "role" "assistant"
                                       "content" :null)))
    (nlk::encode-inner-event
     "turn.tool_result"
     (nlk::make-json-object "call-id" :null "tool-name" :null
                            "result" :null)))
  (let ((rows (nle::transcript-rows state)))
    (is (plusp (length rows)) "the null-result tool fact still lands a row")
    (is (loop for row in rows
              always (stringp row.text)))))

(deftest gateway-client-reconciliation-distinguishes-unrelated-active-turn ()
  (let* ((model (detached-shell))
         (payload (nlk::make-json-object "admission_state" "active"
                                         "accumulated_frames" #())))
    (setf (nle::gateway-model-pending-command-id model) "pending"
          (nle::gateway-model-pending-correlation-id model) "correlation"
          (nle::gateway-model-pending-prompt model) "hello")
    (nle::reconcile-pending-command model payload)
    (is (equal "pending" (nle::gateway-model-pending-command-id model)))))

(deftest gateway-replay-queues-one-bounded-writer-job (with-temp-store ())
  (seed-user-inputs "bounded-replay"
                    (loop for i below 210 collect (format nil "m~d" i)))
  ;; The replay reads the runtime fold, whole.
  (let ((events (nle::collect-visible-session-events "bounded-replay")))
    (is (= 631 (length events)))
    (is (equal "session.created" (nlk:event-kind (first events))))
    (is (equal "turn.completed" (nlk:event-kind (car (last events))))))
  (let* ((peer (list :test-peer))
         (gws (nle::find-or-create-gw-session "bounded-replay"))
         (state (nle::ensure-gw-peer-state peer)))
    (bt2:with-lock-held ((nle::gw-session-lock gws))
      (nle::send-replay-under-lock gws peer))
    (bt2:with-lock-held ((nle::gw-peer-state-lock state))
      (is (<= (length (nle::gw-peer-state-queue state)) 1)))
    (nle::stop-gw-peer peer)))

(defun overflow-peer (peer state)
  "Fill STATE's queue to the byte bound, then offer PEER one frame more;
answers what the enqueue did."
  (bt2:with-lock-held ((nle::gw-peer-state-lock state))
    (setf (nle::gw-peer-state-queued-bytes state) nle::+gateway-max-peer-queue-bytes+))
  (nle::enqueue-peer-frame peer "one-too-many"))

(deftest gateway-peer-queue-bound-is-bytes-never-frames ()
  "One bound, bytes. A peer holding thousands of small frames is a healthy
shell behind a replay -- a running session publishes 100-200 deltas a
second, ~150 bytes each, and the writer drains nothing until the replay
ends -- never a stalled one: a 128-frame bound dropped it 19 KB behind, a
second into every switch to a running session. The byte
bound still cuts a peer that stops reading, without blocking the publisher."
  (with-silent-peer (peer state :bytes-not-frames)
    (is (loop repeat 4096 always (nle::enqueue-peer-frame peer "x")))
    (bt2:with-lock-held ((nle::gw-peer-state-lock state))
      (is (= 4096 (length (nle::gw-peer-state-queue state))) "all of them queued")
      (is-shape state (nle::gw-peer-state-queue-tail eq (last (nle::gw-peer-state-queue state)))
        (nle::gw-peer-state-queued-bytes = 4096 "counted in bytes")))
    (is (not (nle::gw-peer-state-closed-p state)))
    (is (not (overflow-peer peer state)))
    (is (nle::gw-peer-state-closed-p state) "and the peer is dropped")))

;; A peer the gateway can write to and hang up with no socket: SENT keeps
;; every frame's text, CLOSED-P records the hang-up. No docstring: the WS
;; base class's metaclass path refuses one before the slot list on this SBCL.
(defclass recording-peer-ws (wsd::ws)
  ((sent :initform '() :accessor recording-peer-ws-sent)
   (closed-p :initform nil :accessor recording-peer-ws-closed-p)))

;; Always open, so WSD's own :AROUND methods pass every send and close through.
(defmethod wsd::ready-state ((ws recording-peer-ws)) :open)

(defmethod wsd:send ((ws recording-peer-ws) data &key &allow-other-keys)
  (push data (recording-peer-ws-sent ws))
  t)

(defmethod wsd:close-connection ((ws recording-peer-ws) &optional reason code)
  (declare (ignore reason code))
  (setf (recording-peer-ws-closed-p ws) t)
  t)

(deftest a-dropped-peer-hears-why-and-is-hung-up ()
  "Eviction is not silent: the dropped peer's writer sends one permanent
sync_protocol_error naming the drop, then hangs the connection up, so the
client's own reconnect ladder — and the replay it earns — runs. Without it, a
shell would keep painting a dead stream after its turn had completed."
  (let* ((peer (make-instance 'recording-peer-ws :require-masking nil))
         (state (nle::ensure-gw-peer-state peer)))
    ;; The peer object IS the transport, as a socket peer's ws is: the
    ;; registry keys on it, and the state's socket slot is read-only.
    (nlk:with-cleanup ((ignore-errors (nle::stop-gw-peer peer)))
      (is (not (overflow-peer peer state)))
      (is (nle::gw-peer-state-closed-p state) "overflow closes the peer")
      (await (:tries 400 :rest 0.005) (recording-peer-ws-closed-p peer))
      (is (recording-peer-ws-closed-p peer))
      (let* ((wire (car (recording-peer-ws-sent peer)))
             (frame (and wire (shasht:read-json wire)))
             (payload (and frame (gethash "payload" frame))))
        (is-table (test value key) (test value (and payload (gethash key payload)))
          (equal "sync_protocol_error" "type") (equal "peer_evicted" "code") (eq t "permanent")))
      (is (null (nle::find-gw-peer-state peer))))))

(deftest an-evicted-in-process-shell-is-hung-up-and-attaches-again ()
  "The image's own shell has no socket to close, so the writer's end reaches
it through the hang-up its attach gave -- the contract a socket peer's close
is. Without this, an evicted shell would keep its link, stay `connected'
and paint nothing while its turn runs on, and its next subscribe would
remake the peer state without a sink, so the new writer would die on the
first frame. One eviction through the in-process link is the whole gate:
the diagnostic reaches the sink, the hang-up fires once, the registry is
clean, the stale handle takes nothing and remakes nothing, and a fresh
attach earns its replay."
  (with-gateway-session (port "s-evicted-shell")
    (let* ((frames '()) (hung-up 0)
           (subscribe (nle::json-object
                       "subscriptions"
                       (vector (nle::json-object "session_id" "s-evicted-shell"))
                       "pid" (sb-posix:getpid)))
           (link (nle::attach-loopback-gateway-peer
                  (lambda (text) (push text frames))
                  :hang-up (lambda () (incf hung-up))))
           (state (nle::find-gw-peer-state link))
           (gws (nle::find-or-create-gw-session "s-evicted-shell")))
      (is (typep link 'nle::gw-loopback-client) "the shell attaches in-process")
      (is (null gws.peers) "no subscriber before the shell asks")
      (nle::send-gateway-command link :set-session-subscriptions subscribe)
      ;; The dispatch carries the shell's own handle as the peer: with CLIENT
      ;; bound twice in one LET*, it closed over the first, NIL binding, the
      ;; subscribe registered NIL, and the window sat on `waiting' with an
      ;; empty transcript while the turn ran on.
      (is-eventually (member link gws.peers) "the session's subscriber is the shell handle itself")
      (is (not (member nil gws.peers)) "NIL is never the subscriber")
      (is-eventually frames "the subscribe replays to the shell's own sink")
      (is (not (overflow-peer link state)))
      (is-eventually (plusp hung-up) "the writer's end hangs the in-process link up")
      (is (= 1 hung-up) "once")
      (let ((payload (gethash "payload" (shasht:read-json (first frames)))))
        (is (equal "sync_protocol_error" (and payload (gethash "type" payload))))
        (is (equal "peer_evicted" (and payload (gethash "code" payload)))))
      (is-eventually (null (nle::find-gw-peer-state link))
                     "a dropped peer pins no registry entry")
      ;; A peer with no registry state takes nothing, and the enqueue makes
      ;; none for it: a state remade on a late publish ran a second writer
      ;; for a link the first hung up.
      (let ((seen (length frames)))
        (is (not (nle::enqueue-peer-frame link "late")))
        (is (null (nle::find-gw-peer-state link)) "and no state was made")
        (nle::send-gateway-command link :set-session-subscriptions subscribe)
        (sleep 0.1)
        (is (= seen (length frames)) "a hung-up link carries nothing, either way"))
      (let* ((again '())
             (link2 (nle::attach-loopback-gateway-peer (lambda (text) (push text again)))))
        (nle::send-gateway-command link2 :set-session-subscriptions subscribe)
        (is-eventually again "the next attach -- the shell's next lap -- earns its replay")
        (nle::stop-gw-peer link2)))))

(deftest gateway-client-rejects-local-store-path ()
  (is (signals-error error
        (nle:start-client-organism
         :store-path "/tmp/should-not-open/history.db"
         :port 1))))

(deftest the-resume-line-names-the-session-the-shell-ended-on ()
  ;; A /sessions pick, a /fork or Left moves a shell off the id it launched
  ;; with; the line printed at exit has to open the session it left, not an
  ;; empty launch session.
  (with-stubbed-fdefinition (nle:run-chat-gateway (&rest args)
                              "s-ended")
    (let ((line (with-output-to-string (*standard-output*)
                  (nle:start-client-organism :session-id "s-launched" :port 1))))
      (is (search "\"s-ended\"" line) "the line names where the shell ended")
      (is (not (search "s-launched" line)) "and not where it launched"))))

(deftest gateway-command-start-turn-uses-zig-content-array ()
  (let* ((wire (nlk:encode-client-command
                (nlk:make-gateway-command
                 :kind :start-turn :command-id "cmd" :correlation-id "corr"
                 :request (nlk:json-object "session_id" "session-a" "prompt" "yo"))))
         (content (nlk:json-value (shasht:read-json wire) :any "command" "request" "content")))
    (is (vectorp content) "start_turn uses a content array")
    (is (equal "yo" (gethash "text" (aref content 0))))))

(deftest gateway-session-create-preserves-requested-identity (let ((calls '())
                                                                     (nle::*workspace-directory* "/the/shell")))
  (with-stubbed-fdefinition (nle::gateway-http-session-request (port method path body token)
                              (push (list method path body) calls)
                              (if (eq method :get)
                                  (error 'nle::gateway-http-error :status 404
                                         :code "unknown_session" :detail "missing")
                                  body))
    (is (equal "session-a" (nle::ensure-gateway-session "session-a" 1 "token")))
    (let* ((post (first calls))
           (request (third post)))
      (is (eq :post (first post)) "not-found triggers creation")
      (is (equal "session-a" (gethash "session_id" request)))
      ;; A gateway in another process cannot know the shell's folder.
      (is (equal "/the/shell" (gethash "cwd" request)) "and names the shell's folder"))))

(deftest a-fresh-session-opens-in-the-shells-folder-and-a-fork-in-its-parents (with-temp-gateway (port))
  ;; The session a shell opens records the shell's folder, never the gateway
  ;; process's -- the two differ whenever the gateway runs in the background
  ;; -- and a fork names none: it works where the session it came off works.
  ;; The shell's folder is there, as a shell's is: the gateway refuses one
  ;; that is not.
  (nlk:create-session :id "far-parent" :cwd "/parent/folder/")
  (with-temp-trees ((shell "shell-folder"))
    (ensure-directories-exist shell)
    (let ((model (gateway-shell port "far-parent"))
          (nle::*workspace-directory* (string-right-trim "/" (namestring shell))))
      (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
        (flet ((opened-cwd ()
                 (gethash "cwd" (nle::gateway-model-http-call
                                 model :get (format nil "/api/gateway/sessions/~a"
                                                    (nle::chat-model-session-id model))
                                 nil))))
          (nle::switch-to-fresh-gateway-session model)
          (is (equal nle::*workspace-directory* (opened-cwd)) "/clear opens in the shell's folder")
          (nle::switch-to-fresh-gateway-session model "far-parent")
          (is (equal "/parent/folder/" (opened-cwd)) "/fork opens in its parent's"))))))

(deftest gateway-session-create-refuses-non-not-found-errors (let ((calls 0)))
  (with-stubbed-fdefinition (nle::gateway-http-session-request (&rest args)
                              (incf calls)
                              (error 'nle::gateway-http-error :status 401
                                     :code "operator_auth_failed" :detail "denied"))
    (is (signals-error nle::gateway-http-error
          (nle::ensure-gateway-session "session-a" 1 "wrong")))
    (is (= 1 calls) "no POST follows a non-not-found lookup failure")))


(defmacro with-gateway-http ((port method path &rest options) &body assertions
                             &aux (code (getf options :status)) (says (getf options :says)))
  "Call the gateway's HTTP control surface and run ASSERTIONS over the answer,
with STATUS and BODY bound to the two values GATEWAY-HTTP-STATUS returns."
  ;; The operator token defaults to the fixture's own, because a route test is
  ;; almost never about the token: an authorized call had to spell `:token
  ;; "test-token"' at every one of fifty call sites, and the handful of tests
  ;; that ARE about the boundary — no token, wrong token — say so by passing
  ;; their own :TOKEN and are legible precisely because they are the
  ;; exception.
  ;;
  ;; BODY is bound IGNORABLE, so a status-only assertion needs no IGNORE
  ;; declaration. :STATUS CODE asserts the answer's code first, under :SAYS.
  `(multiple-value-bind (status body)
       (gateway-http-status ,port ,method ,path
                            ,@(if (member :token options)
                                  '()
                                  '(:token "test-token"))
                            ,@(alexandria:remove-from-plist options :status :says))
     (declare (ignorable status body))
     ,@(when code `((is (= ,code status) ,@(when says (list says)))))
     ,@assertions))

(defmacro is-route ((port method path &rest options) code &optional description error)
  "Assert the control surface answers CODE for one call -- and, given ERROR, that
its body names that error code -- reading nothing else."
  ;; The status-only half of WITH-GATEWAY-HTTP, which most boundary tests are:
  ;; a refusal is a code and a reason, and spelling both on one line keeps a
  ;; run of them readable as the table of refusals it is.
  `(with-gateway-http (,port ,method ,path ,@options)
     (is (= ,code status) ,@(when description (list description)))
     ,@(when error `((is (equal ,error (nlk:json-value body :string "error" "code")))))))

(defun gateway-http-status (port method path &key body token)
  (multiple-value-bind (response status)
      (nlk:http method (format nil "http://127.0.0.1:~d~a" port path)
                :headers `(,(cons "content-type" "application/json")
                           ,@(when token
                               (list (cons "authorization" (format nil "Bearer ~a" token)))))
                :content body)
    (values status (handler-case (shasht:read-json response) (error () nil)))))

(defun gateway-octets (port method path &key content type)
  "One call to PORT's gateway under the fixture's token, the body sent and
answered as octets. => (values BODY STATUS HEADERS)."
  (nlk:http method (format nil "http://127.0.0.1:~d~a" port path)
            :binary t :content content
            :headers `(("authorization" . "Bearer test-token")
                       ,@(when type (list (cons "content-type" type))))))

(deftest gateway-http-auth-and-exact-session-identity (with-temp-gateway (port))
  (is-route (port :post "/api/gateway/sessions" :body "{}") 400
            "missing session id is invalid" "invalid_request")
  (with-gateway-http (port :post "/api/gateway/sessions"
                      :body "{\"session_id\":\"exact-http-id\"}")
    (is (= 200 status) "authorized exact session creation succeeds")
    (is (equal "exact-http-id" (gethash "session_id" body))))
  (is-route (port :post "/api/gateway/sessions" :body "{\"session_id\":\"exact-http-id\"}") 409
            "duplicate exact identity is refused")
  (is-route (port :get "/api/gateway/sessions/exact-http-id" :token nil) 401
            "missing HTTP token fails closed")
  (is-route (port :get "/api/gateway/sessions/exact-http-id" :token "wrong") 401
            "wrong HTTP token fails closed"))

(deftest gateway-checkpoints-route-serves-query-string-requests (with-temp-gateway (port))
  ;; The real TUI always sends ?limit=N; route matching must strip the
  ;; query string or the request falls through to the catch-all, and LIMIT
  ;; must keep the newest entries (the Zig list drops from the front).
  (seed-user-inputs "cp-limit" '("first" "second" "third"))
  (with-gateway-http (port :get "/api/gateway/sessions/cp-limit/checkpoints")
    (is (= 200 status) "bare checkpoint listing works")
    (is (= 3 (length (gethash "checkpoints" body)))))
  (with-gateway-http (port :get "/api/gateway/sessions/cp-limit/checkpoints?limit=1" :status 200)
    (let ((entries (gethash "checkpoints" body)))
      (is (= 1 (length entries)) "limit bounds the listing")
      (is (equal "third" (gethash "prompt" (aref entries 0))))))
  (is-route (port :get "/api/gateway/sessions/cp-limit/checkpoints?limit=0") 400
            "a malformed limit is refused, not ignored")
  (is-route (port :get "/api/gateway/sessions/cp-limit/checkpoints?foo=1") 400
            "unknown query keys are refused"))

(deftest gateway-binds-kernel-assigned-port-and-publishes-it (with-temp-gateway (port))
  ;; :port 0 is the canonical start path: START-GATEWAY returns the port it
  ;; accepted on and advertises it in the store's holder sidecar. With
  ;; kernel-assigned ports the sidecar is the ONLY way attachers learn the
  ;; endpoint, so the publication is part of the start contract, not
  ;; best-effort chrome. WITH-TEMP-GATEWAY's PORT is START-GATEWAY's answer.
  (is (and (integerp port) (plusp port)))
  (is (nle::await-gateway-acceptance port :attempts 3))
  (is (not (nle::await-gateway-acceptance (temp-gateway-port) :attempts 2)))
  (is (eql port (nth-value 1 (nlk::read-store-holder (nlk:store-path nlk::*store*))))))

(defun raw-gateway-status (port path &rest headers)
  "The status PORT's gateway answers a GET of PATH carrying exactly HEADERS,
\"Name: value\" strings -- the Host a client library would add included, so a
gate can send any Host, or none."
  (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character))
        (crlf (coerce '(#\Return #\Newline) 'string)))
    (nlk:with-cleanup ((usocket:socket-close socket))
      (let ((stream (usocket:socket-stream socket)))
        (format stream "GET ~a HTTP/1.1~a~{~a~}~a" path crlf
                (loop for header in headers append (list header crlf)) crlf)
        (force-output stream)
        (parse-integer (second (uiop:split-string (read-line stream) :separator " ")))))))

(deftest a-client-hanging-up-mid-answer-ends-only-its-connection (with-temp-gateway (port))
  ;; An answer larger than the socket buffers is still being written when the
  ;; client, having read the status line, hangs up. Before the hook, clack's
  ;; 500 to that same socket failed under the disabled debugger and the
  ;; process exited, this run with it.
  (let ((body (make-string (* 4 1024 1024) :initial-element #\x)))
    (nlk:with-cleanup ((nle:route "/t-big" nil))
      (nle:route "/t-big" (lambda (env) (declare (ignore env))
                            (list 200 '(:content-type "text/plain") (list body)))
                 :public t)
      (dotimes (i 3)
        (is (= 200 (raw-gateway-status port "/t-big" (format nil "Host: 127.0.0.1:~d" port)))))
      (is-route (port :get "/healthz" :token nil) 200 "and the gateway still answers"))))

(deftest the-gateway-logs-to-standard-error-never-the-stream-it-started-under ()
  ;; A cold cell compile on a connection's thread ends in `undefined
  ;; variable' warnings, and hunchentoot logged each one to the stream the
  ;; gateway started under -- a shell's error stream, where every line became
  ;; a raw `[date [WARNING]] Warning while processing connection: ...' toast
  ;; over the setup walk (the hermes-real walk on a cold cache). Every line
  ;; hunchentoot logs goes to the process's standard error, the organism's log.
  (let ((log (make-string-output-stream))
        (destination nil))
    (let ((*error-output* log))
      (with-temp-gateway (port)
        (nlk:with-cleanup ((nle:route "/t-warns" nil))
          (nle:route "/t-warns" (lambda (env) (declare (ignore env))
                                  (signal 'simple-warning :format-control "a warning on a connection")
                                  (hunchentoot:log-message* :warning "a line hunchentoot logs")
                                  (setf destination (hunchentoot:acceptor-message-log-destination
                                                     hunchentoot:*acceptor*))
                                  (nlk:json-object "ok" t))
                     :public t)
          (is-route (port :get "/t-warns" :token nil) 200))))
    (is (eq sb-sys:*stderr* destination))
    (is-carrying (said (get-output-stream-string log))
      (:absent "a warning on a connection") (:absent "a line hunchentoot logs"))))

(deftest the-gateway-answers-only-at-a-loopback-name (with-temp-gateway (port))
  ;; DNS rebinding: a page on the attacker's name, resolved to 127.0.0.1, is
  ;; same-origin with whatever answers there -- and says its own name in Host.
  (is (= 200 (raw-gateway-status port "/healthz" (format nil "Host: 127.0.0.1:~d" port))))
  (is (= 200 (raw-gateway-status port "/healthz" (format nil "Host: localhost:~d" port))))
  (is (= 403 (raw-gateway-status port "/healthz" (format nil "Host: attacker.example:~d" port))) "a rebound name")
  (is (= 403 (raw-gateway-status port "/healthz" "Host: localhost.attacker.example")) "a loopback lookalike")
  (is (= 403 (raw-gateway-status port "/healthz")) "and so is no name at all"))

(deftest a-socket-opens-only-from-the-gateways-own-pages (with-temp-gateway (port))
  ;; A browser opens a socket to any origin and only says whose page asked.
  (flet ((upgrade (host &rest origin)
           (apply #'raw-gateway-status port "/gateway" (format nil "Host: ~a" host)
                  "Upgrade: websocket" "Connection: Upgrade"
                  "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" "Sec-WebSocket-Version: 13"
                  origin)))
    (let ((here (format nil "127.0.0.1:~d" port)))
      (is (= 101 (upgrade here)) "a shell's socket names no origin")
      (is (= 101 (upgrade here (format nil "Origin: http://~a" here))) "the gateway's own page")
      (is (= 101 (upgrade "localhost:8080" "Origin: http://localhost:8080")) "and through a tunnel")
      (is (= 403 (upgrade here "Origin: https://attacker.example")) "another site's page")
      (is (= 403 (upgrade here (format nil "Origin: http://127.0.0.1:~d" (1+ port)))) "another local server's page")
      (is (= 403 (upgrade here (format nil "Origin: http://localhost:~d" port))) "another name is another origin"))))

(deftest a-socket-whose-upgrade-carries-the-bearer-connects-without-the-token (with-temp-gateway (port))
  ;; A proxy that adds the operator's Bearer to every request it forwards (the
  ;; link cell) serves a page that never holds the token: the upgrade is the
  ;; operator at the door, as every HTTP route takes it. GATEWAY-ENDPOINT is
  ;; how such a proxy in the image finds the door and the token.
  (multiple-value-bind (url token) (nle:gateway-endpoint)
    (flet ((connect (&rest headers)
             ;; The answer to the connect, the frame after the challenge.
             (gethash "type" (second (ws-collect-frames (format nil "ws~a/gateway" (subseq url 4))
                                                        :timeout 0.5 :headers headers
                                                        :token nil)))))
      (is (equal (format nil "http://127.0.0.1:~d" port) url))
      (is (equal "test-token" token))
      ;; The Bearer at the upgrade and no token in the connect; neither; a
      ;; wrong Bearer, which is none.
      (is (equal "gateway_connect_ack" (connect (cons "authorization" (format nil "Bearer ~a" token)))))
      (is (equal "gateway_connect_rejected" (connect)))
      (is (equal "gateway_connect_rejected" (connect (cons "authorization" "Bearer wrong")))))))

(defun quiet-socket-open-after (port seconds)
  "Open a socket to PORT's gateway that never sends a byte, and answer whether
it is still open SECONDS later."
  (let ((ws (wsd:make-client (format nil "ws://127.0.0.1:~d/gateway" port)))
        (closed nil))
    (wsd:on :close ws (lambda (&key code reason) (declare (ignore code reason)) (setf closed t)))
    (let ((thread (nlk:spawn "quiet-socket" (ignore-errors (wsd:start-connection ws)))))
      (sleep seconds)
      (prog1 (and (not closed) (eq :open (wsd:ready-state ws)))
        (ignore-errors (wsd:close-connection ws))
        (ignore-errors (bt2:join-thread thread))))))

(deftest a-quiet-socket-lives-on-the-gateways-pings ()
  ;; A browser cannot ping, so its pong to the gateway's ping is the only byte
  ;; a quiet tab sends inside the read timeout. The ping cadence is read by the
  ;; peer's writer thread, so it is set, not bound.
  (let ((nle::*gateway-connection-read-timeout* 1))
    (with-saved-globals ((nle::*gateway-ping-seconds* 60))
      (with-temp-gateway (port)
        (is (not (quiet-socket-open-after port 3)) "unpinged, the read timeout hangs it up"))
      (setf nle::*gateway-ping-seconds* 0.3)
      (with-temp-gateway (port)
        (is (quiet-socket-open-after port 3) "pinged, it stays open")))))

(deftest a-route-answers-its-path-and-the-gateways-own-routes-win (with-temp-gateway (port))
  (flet ((text (path)
           (multiple-value-bind (body status) (nlk:http :get (format nil "http://127.0.0.1:~d~a" port path))
             (list status body)))
         (plain (text) (list 200 '(:content-type "text/plain") (list text))))
    (nlk:with-cleanup ((dolist (path '("/t-page" "/t-dir/" "/t-dir/deeper/" "/t-fails" "/healthz" "/t-own"))
                     (nle:route path nil)))
      (nle:route "/t-page" (lambda (env) (declare (ignore env)) (nlk:json-object "page" t)) :public t)
      (nle:route "/t-dir/" (lambda (env) (plain (getf env :path-info))) :public t)
      (nle:route "/t-dir/deeper/" (lambda (env) (declare (ignore env)) (plain "deeper")) :public t)
      (nle:route "/t-fails" (lambda (env) (declare (ignore env)) (error "no")) :public t)
      (nle:route "/healthz" (lambda (env) (declare (ignore env)) (plain "shadowed")) :public t)
      (nle:route "/t-own" (lambda (env) (declare (ignore env)) (nlk:json-object "own" t)))
      (with-gateway-http (port :get "/t-page" :token nil)
        (is (= 200 status) "a public route answers without the operator token")
        (is (eq t (gethash "page" body)) "a JSON value is a 200's body"))
      (is-route (port :get "/t-own" :token nil) 401 "any other is the operator's" "operator_auth_failed")
      (is-route (port :get "/t-own" :token "wrong") 401 "and a wrong token is none")
      (with-gateway-http (port :get "/t-own")
        (is (eq t (gethash "own" body)) "the operator's token reaches it"))
      (is (equal '(200 "/t-dir/a/b.js") (text "/t-dir/a/b.js")) "a directory answers what lies under it")
      (is (equal '(200 "deeper") (text "/t-dir/deeper/x")) "the longest directory wins")
      (is-route (port :get "/t-dir" :token nil) 404 "a directory is not its own bare name")
      (is-route (port :get "/t-fails" :token nil) 500 "a failing route answers 500" "route_failed")
      (with-gateway-http (port :get "/healthz" :token nil)
        (is (equal "gateway" (gethash "component" body)) "the gateway's own route wins"))
      (nle:route "/t-page" nil)
      (is-route (port :get "/t-page" :token nil) 404 "a route taken back is gone")
      (is (signals-error error (nle:route "t-page" #'identity)) "a path starts with /"))))

(deftest a-gateway-takes-back-the-port-its-store-had-last ()
  ;; A page the gateway served -- a tab, a bookmark -- is at one origin, so a
  ;; restart that moved the port lost every one of them for good.
  (with-gateway-scratch (dir "port" :store t)
    (flet ((start () (nle:start-gateway :port 0 :operator-token "test-token" :workspace-captures nil)))
      (let ((first (start)))
        (nle:stop-gateway)
        (is (= first (start)) "a restart finds the port where it was")
        (nle:stop-gateway)
        (let ((holder (usocket:socket-listen "127.0.0.1" first :reuse-address t)))
          (nlk:with-cleanup ((usocket:socket-close holder))
            (is (/= first (start)) "and a port someone else took is left to them")))))))

(deftest gateway-service-routes-and-hygiene (with-temp-gateway (port))
  ;; /healthz is unauthenticated liveness; identity is an operator GET;
  ;; unknown routes 404 uniformly (no banner fingerprint, no
  ;; namespace oracle); declared-non-JSON bodies are refused with 415.
  (with-gateway-http (port :get "/healthz" :token nil)
    (is (= 200 status) "healthz answers without auth")
    (is (eq t (gethash "ok" body)) "healthz body carries ok:true")
    (is (equal "gateway" (gethash "component" body)) "and the component"))
  (is-route (port :get "/api/gateway/identity" :token nil) 401 "identity requires the operator token")
  (with-gateway-http (port :get "/api/gateway/identity")
    (is (= 200 status) "identity answers the operator")
    (is (equal "nodecode-gateway" (gethash "service" body)))
    (is (vectorp (gethash "tools" body)) "identity lists the tool registry")
    ;; `nodecode gateway' asks this route who serves: the build, the
    ;; uptime, the standing board.
    (is (equal nle::*user-agent-version* (gethash "version" body)) "identity names the build")
    (is (integerp (gethash "started_at" body)) "and when the gateway started, in unix seconds")
    (is (<= (abs (- (nlk::unix-now) (gethash "started_at" body))) 60) "which is now, give or take")
    (is (vectorp (gethash "notices" body)) "and the lines it keeps standing"))
  (is-route (port :get "/definitely/not/a/route" :token nil) 404
            "unknown routes 404 instead of a 200 banner" "not_found")
  (is-route (port :get "/api/gateway/sessions/with/deep/path") 404
            "unmatched session subpaths 404 — no oracle")
  (let ((status (nth-value 1 (gateway-octets port :post "/api/gateway/sessions" :type "text/plain"
                                                    :content "{\"session_id\":\"ct-refused\"}"))))
    (is (= 415 status) "a declared-non-JSON body is refused, not parsed")))

(deftest an-organism-that-reports-no-build-reads-as-older-than-this-shell (let ((said '())))
  ;; The connect ack's `version' arrived with the gateway that knows to send
  ;; it, so an ack without one is not silence to pass over: that organism
  ;; predates the field, and is therefore certainly older than this shell.
  ;; Treating it as nothing to say would leave a freshly installed binary
  ;; running against a pre-upgrade organism with no sign on screen; the import
  ;; would then run inside the old image and bring over neither the provider
  ;; keys nor the bot.
  (with-stubbed-fdefinitions
      ((nle::post-shell-notice (prog text &key error-p stays) (push text said) nil))
    (let ((nle::*organism-build-noted* nil))
      (nle::note-organism-build nil (nlk:json-object "type" "gateway_connect_ack"))
      (is (= 1 (length said)) "an ack with no version is itself the answer")
      (is (search "older than this shell" (first said)) "and says which way it runs")
      (nle::note-organism-build nil (nlk:json-object "type" "gateway_connect_ack"))
      (is (= 1 (length said)) "said once, not again on every reconnect")
      (nle::note-organism-build nil (nlk:json-object "version" "0.0.1+deadbee"))
      (is (= 2 (length said)) "a build that does name itself is still noted")
      (is (search "0.0.1+deadbee" (first said)) "by name")
      (nle::note-organism-build nil (nlk:json-object "version" nle::*user-agent-version*))
      (is (= 2 (length said)) "and this shell's own build says nothing"))))

(deftest gateway-session-directory-routes (with-temp-gateway (port))
  ;; GET /sessions lists the directory with derived titles, laid out as the
  ;; session fork tree (a child right under its parent, one lane deeper, the
  ;; DEPTH/LANES/FORK shape the checkpoint listing also sends); a
  ;; Zig-lexicon array.
  (seed-user-inputs "dir-a" '("first session prompt"))
  (seed-user-inputs "dir-b" '("second session prompt"))
  (nlk:create-session :id "dir-child" :parent "dir-a")
  (with-gateway-http (port :get "/api/gateway/sessions" :status 200 :says "the directory lists")
    (is (vectorp body) "the list route returns a bare array")
    (is (= 3 (length body)) "every session appears")
    (is-present (entry (wire-row "dir-a" body)) "dir-a is listed"
      (is-shape entry ("title" "first session prompt") ("message_count" = 1)
        ("depth" = 0 "a session with no parent is a root"))
      (is (equal '() (coerce (gethash "lanes" entry) 'list)))
      (is (null (gethash "fork" entry)) "and opening none"))
    (let* ((rows (coerce body 'list))
           (parent-index (position (wire-row "dir-a" rows) rows))
           (child-index (position (wire-row "dir-child" rows) rows))
           (child (nth child-index rows)))
      (is (= (1+ parent-index) child-index))
      (is-shape child ("depth" = 1 "one lane deeper") ("fork" eq t "on the row that opens a lane"))
      (is (equal '(nil) (coerce (gethash "lanes" child) 'list)))
      (is (equal "dir-a" (gethash "parent_id" child)) "the parent rides along")))
  (with-gateway-http (port :get "/api/gateway/sessions?limit=1")
    (is (= 200 status) "a limited listing works")
    (is (= 1 (length body)) "limit bounds the directory")))

(deftest gateway-session-search-greps-every-conversation (with-temp-gateway (port))
  ;; POST /api/gateway/sessions/search is the /sessions box's grep over every
  ;; session: body, not query string (a search box holds spaces and quotes),
  ;; one row per matching block with the session's title and the durable seq a
  ;; shell can pin its viewport at, and a hit the session's head no longer
  ;; carries dropped rather than served.
  (seed-user-inputs "search-a" '("find the wombat in here"))
  (seed-completed-turn "search-b" "nothing much"
                       "a wombat sighting in the second session")
  (with-gateway-http (port :post "/api/gateway/sessions/search"
                      :body "{\"query\":\"wombat\",\"limit\":10}")
    (is (= 200 status) "the search answers")
    (is (equal "wombat" (gethash "query" body)) "and says what it answered")
    (is-let* ((rows (gethash "rows" body)))
      ((length rows) = 2 "one row per matching body, across sessions")
      ((gethash "session_id" (aref rows 0)) "search-b")
      ((search "wombat" (gethash "snippet" (aref rows 0))) is)
      ((gethash "title" (aref rows 0)) is "the session title rides the row")
      ((integerp (gethash "seq" (aref rows 0))) is) ((gethash "role" (aref rows 0)) "assistant")))
  ;; A turn the head no longer carries is not a place to fly a shell.
  (nlk:move-session-head "search-a" nil)
  (with-gateway-http (port :post "/api/gateway/sessions/search"
                      :body "{\"query\":\"wombat\",\"limit\":10}")
    (is (= 1 (length (gethash "rows" body)))))
  (is-route (port :post "/api/gateway/sessions/search"
             :body "{\"query\":\"   \"}") 400 "an empty query is refused" "invalid_request")
  (is-route (port :get "/api/gateway/sessions/search") 405 "the op is a POST")
  (with-gateway-http (port :post "/api/gateway/sessions/search"
                      :body "{\"query\":\"wombat\",\"limit\":0}")
    (is (= 400 status) "a limit of zero is refused, never guessed")))

(deftest gateway-provider-models-refresh-fetches-in-the-gateway (with-temp-gateway (port))
  ;; POST /api/gateway/provider-models/refresh is how a shell's /models
  ;; refreshes: the fetch runs in the gateway's image, whose cells are
  ;; started and so advise it, and the answer names the ids it wrote.
  (let ((asked nil))
    (with-stubbed-fdefinition (nle::refresh-provider-models (ids)
                               (setf asked ids)
                               (list (first ids)))
      (with-gateway-http (port :post "/api/gateway/provider-models/refresh"
                          :body "{\"ids\":[\"cline-pass\",\"openai\"]}" :status 200)
        (is (equalp #("cline-pass") (gethash "refreshed" body)) "the ids it wrote"))
      (is (equal '("cline-pass" "openai") asked) "every id asked is fetched")
      (is-route (port :post "/api/gateway/provider-models/refresh" :body "{\"ids\":[]}") 400
                "an empty ask is refused" "invalid_request")
      (is-route (port :get "/api/gateway/provider-models/refresh") 405 "the op is a POST"))))

(defun is-model-pin (session-id provider model provider-why model-why)
  "Assert SESSION-ID's durable model pin reads back as PROVIDER and MODEL, NIL
for a field left to the gateway, each under its own WHY."
  (is-values (pinned-provider pinned-model) (nlk:session-model-selection session-id)
    (pinned-provider provider provider-why) (pinned-model model model-why)))

(deftest a-json-answer-leaves-as-its-own-octets ()
  ;; The body the server writes as it is, and its length, so the reader
  ;; takes it in one read: a history page encoded as a string consed ten
  ;; times its size.
  (destructuring-bind (status headers body)
      (nle::gateway-json-response 200 (nlk:json-object "text" "café"))
    (is (= 200 status))
    (is (typep body '(vector (unsigned-byte 8))))
    (is (= (length body) (getf headers :content-length)))
    (is (equal "café" (gethash "text" (nlk:decode-json body))))))

(deftest gateway-history-pages-and-session-model (with-temp-gateway (port))
  ;; /history walks older visible frames below BEFORE (the lazy pager);
  ;; /model records the durable per-session selection that RUN binds.
  (seed-user-inputs "pager" '("one" "two" "three"))
  (with-gateway-http (port :get "/api/gateway/sessions/pager/history?limit=2")
    (is (= 200 status) "history pages")
    (is (plusp (length (gethash "frames" body))) "frames flow")
    (is (integerp (gethash "next_before" body)) "the floor comes back")
    (let ((next (gethash "next_before" body)))
      (is-values (status2 body2)
          (gateway-http-status
           port :get (format nil "/api/gateway/sessions/pager/history?before=~d&limit=2" next)
           :token "test-token")
        (status2 = 200 "the next page pages") ((< (gethash "next_before" body2) next) is))))
  (with-gateway-http (port :post "/api/gateway/sessions/pager/model"
                      :body "{\"model\":\"session-scoped-model\"}")
    (is (= 200 status) "the selection lands")
    (is (equal "session-scoped-model" (gethash "selected_model" body))))
  (is-model-pin "pager" nle::*provider* "session-scoped-model"
                "a model-only pick keeps the lane it resolved to" "the durable read model serves the selection")
  (is-route (port :post "/api/gateway/sessions/pager/model" :body "{}") 200
            "an empty body clears the selection")
  (is-model-pin "pager" nil nil "the empty body cleared the provider" "and the model")
  (is-route (port :post "/api/gateway/sessions/pager/model"
             :body "{\"provider\":\"no-such-lane\"}") 400
            "unknown lanes are refused at selection time" "unknown_provider"))

(deftest gateway-replay-is-bounded-and-complete-on-the-wire (with-temp-gateway (port))
  (seed-user-inputs "chunked-wire"
                    (loop for i below 140 collect (format nil "wire-~d" i)))
  (let* ((frames (subscribed-frames port "chunked-wire" :timeout 0.5))
         (chunks (frames-of-type frames "session_stream_resumed"))
         (facts (replay-frames frames)))
    (is (> (length chunks) 1) "large replay is split into bounded frames")
    (is (= 421 (length facts)) "bounded replay delivers every durable fact")
    (is (<= (length chunks) 8) "in batches, not a frame per fact")
    (is (gethash "replay_chunk_final" (gethash "payload" (car (last chunks)))))
    (is (every (lambda (frame)
                 (<= (length (nlk::encode-json-object frame))
                     nlk:+max-sync-frame-bytes+))
               chunks))
    ;; The cursor rides the final chunk alone.
    (is (every (lambda (chunk) (not (nth-value 1 (gethash "cursor" chunk)))) (butlast chunks)))
    (is (integerp (gethash "cursor" (car (last chunks)))))))

(deftest a-replay-warms-the-fold-its-first-prompt-reads (with-temp-gateway (port))
  ;; The first prompt after a resume folded the session's whole log in front
  ;; of its request; the replay leaves that fold in the hydration cache.
  (nle::clear-hydration-cache)
  (seed-user-inputs "warmed" '("one" "two" "three"))
  (subscribed-frames port "warmed" :timeout 0.5)
  (is-eventually (gethash "warmed" nle::*hydration-cache*) "the replay warmed the fold"))

(deftest an-oversized-command-is-refused-and-the-connection-lives ()
  ;; A start_turn past the frame bound. The gateway refuses it before
  ;; decoding, as a recoverable sync_protocol_error naming both sizes, and
  ;; the same connection goes on serving: the subscribe sent behind the
  ;; refusal still earns its replay.
  (with-gateway-session (port "gw-oversized")
    (let* ((frames (frames-while (ws port)
                     (send-command ws (make-string (1+ nlk:+max-sync-frame-bytes+)
                                                   :initial-element #\Space
                                                   :element-type 'base-char))
                     (send-command ws (subscribe-command (list "gw-oversized")))))
           (refusal (frame-carrying (frames-of-type frames "sync_protocol_error")
                                    "frame_too_large" "code")))
      (is-present refusal "the oversized frame is refused by code"
        (is (not (nlk:json-value refusal :boolean "payload" "permanent")))
        (is (search "16 MiB sync frame bound"
                    (nlk:json-value refusal :string "payload" "message"))))
      (is (find-frame frames "session_stream_resumed")))))

(defun submit-disposition (session-id prompt &rest keys)
  "The disposition of the admission NLE:SUBMIT answers for PROMPT on SESSION-ID."
  (nlk:active-input-admission-disposition (apply #'nle:submit session-id prompt keys)))

(deftests start-turn-spawns-no-worker (session-id prompt command-id disposition check)
  ;; A durably admitted, still-active turn stands in for an in-flight worker.
  (with-session-store (session-id)
    (nlk:admit-active-turn-input session-id "gw-a1-command" "hello")
    (let ((provider-calls 0)
          (before (nlk:event-count :session-id session-id)))
      (declare (ignorable before))
      (with-provider-stub (nil (incf provider-calls) (wire-message "assistant" :content "no"))
        (is (eq disposition (submit-disposition session-id prompt :command-id command-id)))
        (is (not (nle::gw-session-turn-active-p (nle::find-or-create-gw-session session-id))))
        (sleep 0.3)
        (is (zerop provider-calls))
        check)))
  ;; The start replayed under its admitted command id is that turn, again.
  (when-replayed "gw-a1-dup" "hello" "gw-a1-command" :duplicate
   (is (= before (nlk:event-count :session-id "gw-a1-dup"))))
  ;; Another start behind the running turn waits in the queue for it.
  (when-queued "gw-a1-queue" "second" "gw-queue-cmd-b" :queued
   (is (equal '("gw-a1-queue") (nlk::sessions-with-pending-prompts)))))

(deftest boot-recovery-drains-a-stranded-queue (with-session-store ("gw-drain"))
  (let* ((admission (nlk:admit-active-turn-input "gw-drain" "gw-drain-cmd-a" "first"))
         (turn admission.turn))
    ;; Queue a follow-up behind the active turn, then terminalize the turn
    ;; without promotion: a crash between the terminal fact and dequeue.
    (nlk:admit-active-turn-input "gw-drain" "gw-drain-cmd-b" "second")
    (nlk:fail-turn turn (make-condition 'simple-error
                                        :format-control "synthetic crash")))
  (let ((requests '()))
    (with-provider-stub (context
                         (push (map 'list (lambda (message)
                                            (gethash "content" message))
                                    context.messages)
                               requests)
                         (wire-message "assistant" :content "drained"))
      (nle::start-gateway-recovery '())
      (is-eventually (not (nlk:active-turn-p "gw-drain"))
                     "the drained turn ran to a terminal fact")
      (is (= 1 (length requests)))
      (is (= 1 (count "second" (first requests) :test #'equal)))
      (is (null (nlk::sessions-with-pending-prompts))))))

(deftest boot-recovery-dispatches-behind-the-cells ()
  ;; A lane a cell brings is registered by its START-CELL: a turn this boot
  ;; picks up that rides one (claude-code/...) is dispatched only once the
  ;; cells are up. Ahead of them it failed as a config error 2 s into the
  ;; boot that resumed it.
  (let ((steps '())
        (lock (bt2:make-lock :name "boot-steps"))
        (done (bt2:make-semaphore :name "boot-done")))
    (flet ((note-step (what) (bt2:with-lock-held (lock) (push what steps)))
           (recovery-p (what) (and (consp what) (eq (first what) :recovery))))
      (with-stubbed-fdefinitions ((nle::start-cells () (note-step :cells))
                                  (nle::start-gateway-recovery (resumable)
                                    (note-step (list :recovery resumable)))
                                  (nle::say-note (text))
                                  (nlk:take-restoring-marker ())
                                  (nle::boot-release ())
                                  (nlk:gateway-by-default-p () nil)
                                  (nle::hand-broken-layer ())
                                  (nle::hand-failed-self ()
                                    (bt2:signal-semaphore done)))
        (with-saved-globals ((nle::*gateway-handler* :boot-test-handler)
                             nle::*replay-sent-semaphore*)
          (nle::start-cells-behind-the-listener nil :resumable '(:a-suspended-turn))
          (bt2:signal-semaphore nle::*replay-sent-semaphore*)
          (is (bt2:wait-on-semaphore done :timeout 10) "the boot's cell thread ran to its end")
          (let ((order (reverse steps)))
            (is (equal '(:recovery (:a-suspended-turn)) (find-if #'recovery-p order)) "the boot dispatched the turns it picked up")
            (let ((cells (position :cells order))
                  (recovery (position-if #'recovery-p order)))
              (is (and cells recovery (< cells recovery)) "the turns were dispatched after the cells started"))))))))

;;; --- the note a boot says when it is back -----------------------------------

(deftest the-boot-note-carries-no-memory-or-title (with-temp-store ())
  ;; The words the model writes of the note are posted in every channel's
  ;; home room, where other people read them: nothing the organism keeps --
  ;; a memory, a skill, a session's title -- rides the note or its side call,
  ;; even to a model that echoes all it is handed.
  (let ((asked '()) (said '()))
    (with-stubbed-fdefinitions ((nle::index-text (session-id)
                                  (format nil "[memory]~%- gpu-credits: 28 programs filed, two answered"))
                                (nlk::session-title (session-id) "fix the payroll leak")
                                (nle::broadcast-organism (kind payload &key session-id)
                                  (push (cons kind (nlk:json-value payload :string "text")) said)))
      (with-saved-globals (nle::*resumed-turns* nle::*organism-note*)
        (nlk:session-state-put "gateway" "run"
                               (nlk:encode-json-object
                                (nlk:json-object "started" 1 "stopped" (- (get-universal-time) 60))))
        (let ((note (nle::note-back
                     (list (list (nlk::%make-durable-turn :session-id "s-cut" :turn-id "t-cut"
                                                          :command-id "c-cut" :definition-revision "base")
                                 "go on")))))
          (is-carrying note
            ("nodecode is running again" "the note")
            ("It picks up the turn the stop cut off." "the turn it picks up, counted")
            (:absent "gpu-credits" "no memory")
            (:absent "memories and skills" "no index")
            (:absent "payroll" "no session title"))
          (is-carrying (resumed (gethash "t-cut" nle::*resumed-turns*))
            ("A restart cut this turn off" "the turn picked up is told")
            (:absent "payroll" "and named no title"))
          (with-provider-stub (context
                               (let ((request (nlk:encode-json-object (nle::messages-with-head context))))
                                 (push request asked)
                                 (wire-message "assistant" :content request)))
            (nle::say-note note)
            (is (= 1 (length asked)) "one side call")
            (is-carrying (request (first asked))
              ("nodecode is running again" "the side call is handed the note")
              (:absent "gpu-credits" "and no memory")
              (:absent "payroll" "and no title")
              (:absent "your memories" "and is not told to speak from them"))
            (is (equal "organism.note" (car (first said))) "the words are said")
            (is-carrying (posted (cdr (first said)))
              ("nodecode is running again" "what is posted")
              (:absent "gpu-credits" "posts no memory")
              (:absent "payroll" "posts no title"))))))))

(deftest back-note-says-how-long-why-and-what-it-picks-up (let ((now 100000)))
  ;; Every return, the operator's own quit too, for the model to say.
  (flet ((note (run &optional (turns 0))
           (nle::back-note (and run (apply #'nlk:make-json-object run)) turns now)))
    (is (null (note nil)) "a home's first boot")
    ;; The operator's own quit too.
    (is (equal "nodecode is running again after 3m away."
               (note (list "started" 1 "stopped" (- now 180) "why" "quit"))))
    (is (equal "nodecode is running again after 5s away."
               (note (list "started" 1 "stopped" (- now 5)))) "what stopped it is never told")
    (is (equal "nodecode is running again after 3m away: it restarted for an update."
               (note (list "started" 1 "stopped" (- now 180) "why" "an update"))))
    ;; A run that died left no stop: its gap is not known. The turns it
    ;; picks up are counted, never named.
    (is (equal "nodecode is running again. It picks up the turn the stop cut off."
               (note (list "started" 1) 1)))
    (is (equal "nodecode is running again after 2h away. It picks up the turn the stop cut off."
               (note (list "started" 1 "stopped" (- now 7200) "why" "quit") 1)))
    (is (equal "nodecode is running again after 45s away. It picks up 4 turns the stop cut off."
               (note (list "started" 1 "stopped" (- now 45)) 4)))))

(deftest say-note-says-the-models-words (let ((asked '()) (said '())))
  ;; "we should just make a note that it is back so
  ;; that the model can respond to the note in a natural way, it shouldn't be
  ;; hardcoded "Back."". The note is the side call's message; what the model
  ;; writes is said, once, and stands for a shell that attaches.
  (with-saved-globals (nle::*organism-note*)
    (with-stubbed-fdefinition (nle::broadcast-organism (kind payload &key session-id)
                                (push (cons kind (nlk:json-value payload :string "text")) said))
      (with-provider-stub (context
                           (push (nlk:encode-json-object (nle::messages-with-head context)) asked)
                           (wire-message "assistant" :content "  I'm back after a quick restart.  "))
        (is (equal "I'm back after a quick restart." (nle::say-note "nodecode is running again after 5s away.")))
        (is (search "nodecode is running again after 5s away." (first asked)) "the note is the message")
        (is (equal '(("organism.note" . "I'm back after a quick restart.")) said) "the words, said once")
        (is (equal "I'm back after a quick restart." (nle::organism-note)) "and kept for a shell that attaches")
        (is (null (nle::say-note "")) "an empty note asks nothing")
        (is (= 1 (length asked))))
      (setf said '())
      (with-provider-stub (nil (error "the model is down"))
        (is (null (handler-bind ((warning #'muffle-warning)) (nle::say-note "nodecode is running again."))))
        (is (null said) "a model that cannot be reached says nothing"))
      ;; The operator turns it off in /settings: no word, and no call.
      (setf asked '())
      (with-temp-file (nle::*shared-config-path* :setf t :type "jsonc"
                                                 :contents "{\"welcome\": {\"note\": false}}")
        (with-provider-stub (context
                             (push (nlk:encode-json-object (nle::messages-with-head context)) asked)
                             (wire-message "assistant" :content "Back."))
          (is (null (nle::say-note "nodecode is running again after 5s away.")))
          (is (null asked) "welcome.note off asks the model nothing")
          (is (null said) "and says nothing"))))))

(deftest a-boot-that-picks-up-a-cut-off-turn-says-it-is-back (let ((requests '())))
  ;; If nodecode is cut off and returns, it should brief the user up, that
  ;; it is back and just brief quickly. When a restart for an update cuts a
  ;; turn off, the turn opens telling the model to say it is back, the model
  ;; is handed the boot's note -- how long it was gone, why, the turn it
  ;; picks up -- and a shell attaching hears what it wrote behind its replay.
  (with-gateway-scratch (dir "back")
    (let ((path (merge-pathnames "history.db" dir))
          (note "nodecode is running again after 3m away: it restarted for an update. It picks up the turn the stop cut off."))
      (nlk:open-store :path path)
      (nlk:create-session :id "back-cut")
      (with-saved-globals (nlk::*recovery-boot-id* nle::*organism-note*)
        (nlk:admit-turn "back-cut" "back-cut-cmd" "fix the flaky test")
        (nlk:session-state-put "gateway" "run"
                               (nlk:encode-json-object
                                (nlk:json-object "started" 1 "stopped" (- (get-universal-time) 180)
                                                 "why" "an update")))
        (reopen-store path)
        (with-provider-stub (context
                             (push (nlk:encode-json-object (nle::messages-with-head context))
                                   requests)
                             (wire-message "assistant" :content "Back -- on with the flaky test."))
          (let ((port (nle:start-gateway :port 0 :operator-token "test-token" :workspace-captures nil)))
            (is-eventually (not (nlk:active-turn-p "back-cut")) "the turn it picked up ran to its end")
            ;; The picked-up turn opens telling the model to say it is back.
            (is (find-if (lambda (request)
                           (and (search "telling the user you are back and what you were doing" request)
                                (search note request)))
                         requests))
            ;; The model is handed the note, and its words are said.
            (is-eventually (nle::organism-note) "the model's words of the note" :seconds 10)
            (is (find-if (lambda (request)
                           (and (search note request)
                                (not (search "telling the user you are back" request))))
                         requests))
            (is (equal "Back -- on with the flaky test." (nle::organism-note)))
            ;; A shell attaching hears them behind its replay.
            (is (equal (list "Back -- on with the flaky test.")
                       (mapcar (lambda (frame) (nlk:json-value frame :string "payload" "text"))
                               (frames-of-type (subscribed-frames port "back-cut") "organism.note"))))
            (nle:stop-gateway)
            (is (null (nle::organism-note)) "a stopped gateway's note goes with it")
            ;; And its stop is kept for the next boot.
            (is (nlk:json-value (nlk:decode-json (nlk:session-state-get "gateway" "run")) :integer "stopped"))))))))

;;; --- the layer's files a boot did not load, the model's to mend -------------

(defun broken-layer (dir &key skipped failures)
  "*CELLS* as a boot leaves it with one layer, `mine' under DIR, whose
SKIPPED and FAILURES are these."
  (setf nlk::*cells* (list (nlk::make-cell :name "mine" :kind :layer
                                             :path (merge-pathnames "mine/" dir)
                                             :skipped skipped :failures failures))))

(deftest a-boot-hands-each-layer-file-it-did-not-load-once (let ((handed '())))
  ;; Without this, files of a layer skipped or FAILED would be said on the
  ;; boot line and the board at every boot and acted on by nobody. A boot
  ;; hands them to the model in the session `layer' when one was not handed
  ;; before; a file mended is forgotten there, so one that breaks again is
  ;; handed again.
  (with-gateway-scratch (dir "layer" :store t)
    (with-saved-globals (nlk::*cells*)
      (with-stubbed-fdefinition (nle::submit (session prompt &rest keys)
                                  (push (cons session prompt) handed))
        (let ((watch '("watch.lisp" . "names *manifest*, which this image does not define"))
              (cua '("cua-call.lisp" . "READ error during LOAD:
  Symbol \"CUA-DRIVER/START-SESSION\" not found in the NODECODE-MCP package.
    Line: 8, Column: 106
    Stream: #<SB-INT:FORM-TRACKING-STREAM for \"file /x/cua-call.lisp\">")))
          (broken-layer dir :skipped (list watch) :failures (list cua '(nil . "the folder failed as a unit")))
          (is (= 2 (length (nle::hand-broken-layer))) "a file of the layer each, the unit's failure none")
          (is (equal '("layer") (mapcar #'car handed)) "one turn, in the session `layer'")
          (is (nlk:session-exists-p "layer"))
          (is-carrying (prompt (cdr (first handed)))
            ("this build did not load 2 files of your layer" "how many")
            ("mine/watch.lisp: names *manifest*, which this image does not define" "a skipped file, with why")
            ("mine/cua-call.lisp: READ error during LOAD: Symbol \"CUA-DRIVER/START-SESSION\" not found in the NODECODE-MCP package. Line: 8, Column: 106"
             "a failed one, its why on one line")
            (:absent "Stream:" "never the stream the reader read")
            ("define it again so it loads against this build" "to mend")
            ("eval (fmakunbound 'name) or (makunbound '*name*) as a toplevel form" "or to forget, as the scribe sees it"))
          (is (null (nle::hand-broken-layer)) "the same files are not handed again")
          (broken-layer dir :failures (list cua))
          (is (null (nle::hand-broken-layer)) "one mended is no news")
          (broken-layer dir :skipped (list watch) :failures (list cua))
          (is (= 2 (length (nle::hand-broken-layer))) "one mended and broken again is handed again")
          (broken-layer dir)
          (is (null (nle::hand-broken-layer)) "a layer that loads hands nothing")
          (is (= 2 (length handed))))))))

(deftest a-boot-turns-the-layer-files-it-did-not-load-over-to-the-model (let ((requests '())))
  ;; The integration seam: a gateway that starts with a layer file it did not
  ;; load runs a turn on it, the model answering in the session `layer'.
  (with-gateway-scratch (dir "layer-boot" :store t)
    (with-saved-globals (nlk::*cells* nle::*organism-note*)
      (broken-layer dir :skipped '(("watch.lisp" . "names *manifest*, which this image does not define")))
      (with-provider-stub (context
                           (push (nlk:encode-json-object (nle::messages-with-head context))
                                 requests)
                           (wire-message "assistant" :content "watch.lisp: forgotten, its manifest is gone."))
        (nle:start-gateway :port 0 :operator-token "test-token" :workspace-captures nil)
        (is-eventually (find-if (lambda (request)
                                  (search "this build did not load 1 file of your layer" request))
                                requests)
                       "the boot handed the file to the model" :seconds 15)
        (is-eventually (not (nlk:active-turn-p "layer")) "and its turn ran to its end")
        ;; What it handed is kept for the next boot.
        (is (equal '("watch.lisp")
                   (map 'list #'file-namestring
                        (nlk:json-array (nlk:decode-json (nlk:session-state-get "gateway" "layer"))
                                        "files"))))))))

;;; --- gate: per-peer sync fan-out ------------------------------------------
;;; Cluster B regression seams: each peer owns a dense stream sequence, live
;;; deltas never touch durable state, and a gap-recovery resubscribe is never
;;; suppressed by the peer's own replay watermark.

(defun peer-frame-sequences (state)
  "Wire sequences of the queued items, replay jobs included, FIFO order."
  (bt2:with-lock-held ((nle::gw-peer-state-lock state))
    (mapcar (lambda (item)
              (if (nle::gw-replay-job-p item)
                  (nle::gw-replay-job-sequence item)
                  (gethash "sequence" (shasht:read-json item))))
            (nle::gw-peer-state-queue state))))

(defun durable-gateway-sequence (session-id)
  (or (sqlite:execute-single
       (nlk::require-open-store)
       "select sequence from gateway_sequences where session_id = ?" session-id)
      0))

(defun subscribe-silent-peer (session-id peer
                              &aux (gws (nle::find-or-create-gw-session session-id)))
  (bt2:with-lock-held ((nle::gw-session-lock gws))
    (pushnew peer gws.peers)
    (nle::send-replay-under-lock gws peer)))

(defun text-delta-payload (text)
  (nlk::make-json-object
   "turn_id" "turn-b" "delta" (nlk::make-json-object "type" "text" "text" text)))

(deftest per-peer-streams-stay-dense-across-subscribers-and-live-deltas ()
  (with-session-store ("gw-fanout")
    (with-silent-peer (peer-a state-a :fanout-a)
      (with-silent-peer (peer-b state-b :fanout-b)
        (subscribe-silent-peer "gw-fanout" peer-a)
        (let ((durable-after-a (durable-gateway-sequence "gw-fanout"))
              (events-before (nlk:event-count :session-id "gw-fanout")))
          (is (nle::publish-event "gw-fanout" "item_delta"
                                  (text-delta-payload "one")))
          ;; A second subscriber attaches mid-stream and gets its replay,
          ;; and behind it the round so far as one delta.
          (subscribe-silent-peer "gw-fanout" peer-b)
          (is (nle::publish-event "gw-fanout" "item_delta"
                                  (text-delta-payload "two")))
          (let ((a (peer-frame-sequences state-a))
                (b (peer-frame-sequences state-b)))
            (is (equal a (list (first a) (1+ (first a)) (+ 2 (first a)))))
            (is (equal b (list (first b) (1+ (first b)) (+ 2 (first b))))))
          (is (= (1+ durable-after-a) (durable-gateway-sequence "gw-fanout")))
          (is (= events-before (nlk:event-count :session-id "gw-fanout"))))))))

;;; --- gate: a shell that attaches mid-round is sent the round so far -------
;;; Item deltas are live-only, and the log holds a round once the fact that
;;; closes it commits: without this, a shell that attaches mid-round would
;;; replay the log and open on whatever streams next, mid-sentence, until the
;;; turn ends. Each gate attaches one shell before the turn streams and
;;; others partway, and holds each late one to the first, row for row.

(defun attached-shell (session-id &aux (view (nle::make-rows-view :client (list :shell)
                                                                  :session-id session-id)))
  "An in-process shell subscribed to SESSION-ID, folding what it is sent as a
shell does: => a function answering its transcript rows as (KIND ROLE TEXT
TOOL-ARGS HYDRATED VOIDED), oldest first, or with :STATE its state."
  ;; The fold is a client's rows view, unregistered: it folds a replay out of
  ;; sight and seats it at its last chunk, then each live frame, as a shell does.
  (nle::attach-rows-link view)
  (lambda (&optional what)
    (bt2:with-lock-held ((nle::rows-view-lock view))
      (if (eq what :state)
          (nle::rows-view-state view)
          (mapcar (lambda (row)
                    (list (nle::transcript-row-kind row) (nle::transcript-row-role row)
                          (and (nle::transcript-row-text row) (copy-seq (nle::transcript-row-text row)))
                          (nle::transcript-row-tool-args row)
                          (nle::transcript-row-hydrated row) (nle::transcript-row-voided row)))
                  (nle::transcript-rows (nle::rows-view-state view)))))))

(defun round-in-flight (session-id prompt)
  "Admit PROMPT on SESSION-ID and publish its admission as the gateway does:
=> a function publishing one item_delta of the turn, (TYPE &rest FIELDS), or
with :FACTS publishing every fact committed since; and the turn."
  (let* ((turn (nlk:admit-turn session-id (nlk::make-durable-id "command") prompt))
         (published (nle::publish-turn-facts session-id 0)))
    (values (lambda (type &rest fields)
              (if (eq type :facts)
                  (setf published (nle::publish-turn-facts session-id published))
                  (nle::publish-event session-id "item_delta"
                                      (nlk:json-object "turn_id" (nlk:durable-turn-turn-id turn)
                                                       "delta" (apply #'nlk::make-json-object "type" type fields)))))
            turn)))

(defmacro is-caught-up ((late early) description)
  "Wait for LATE's rows to be EARLY's, then look again once the stream has
had time to send anything more: a row sent twice arrives after the match."
  `(progn (is-eventually (equal (funcall ,early) (funcall ,late)) ,description)
          (sleep 0.2)
          (is (equal (funcall ,early) (funcall ,late)) ,description)))

(deftest a-shell-that-attaches-mid-round-is-sent-the-round-so-far-once
    (with-gateway-session (port "s-mid-round"))
  (multiple-value-bind (stream turn) (round-in-flight "s-mid-round" "walk me through it")
    (let ((early (attached-shell "s-mid-round"))
          (args "{\"form\": \"(+ 1 2)\"}"))
      (funcall stream "reasoning" "text" "weighing ")
      (funcall stream "reasoning" "text" "it")
      (funcall stream "text" "text" "Part 1 ")
      (funcall stream "text" "text" "streamed")
      (let ((mid-text (attached-shell "s-mid-round")))
        (funcall stream "text" "text" ", then more")
        (funcall stream "tool_call" "index" 0 "name" "eval" "arguments" "{\"form\": \"(+ 1")
        (let ((mid-call (attached-shell "s-mid-round")))
          (funcall stream "tool_call" "index" 0 "name" "eval" "arguments" args)
          (is-eventually (equal args (fourth (car (last (funcall early)))))
                         "the shell there from the start has the whole round")
          ;; Its thinking, its text and its call, each one row.
          (is (equal '((:text "user" "walk me through it")
                       (:reasoning "assistant" "weighing it")
                       (:text "assistant" "Part 1 streamed, then more"))
                     (mapcar (lambda (row) (subseq row 0 3)) (butlast (funcall early)))))
          (is-caught-up (mid-text early) "a shell that attached mid-sentence reads it from the first word")
          (is-caught-up (mid-call early) "a shell that attached mid-call has the call so far")
          ;; The round closes: its message, then its call, seat what it streamed.
          (nlk:record-assistant-message turn (nlk:json-object "role" "assistant"
                                                              "content" "Part 1 streamed, then more"
                                                              "reasoning_content" "weighing it"))
          (funcall stream :facts)
          (nlk:record-tool-call turn "c1" "eval" args)
          (funcall stream :facts)
          (nlk:record-tool-result turn "c1" "eval" "3")
          (funcall stream :facts)
          (let ((seated (attached-shell "s-mid-round")))
            (is-eventually (every #'fifth (funcall early)) "the facts seat every streamed row")
            (is-caught-up (seated early) "a shell attached once the round closed reads it off the log alone"))
          ;; The next round streams, and its message commits before the turn
          ;; publishes it: a shell attaching between reads the fact, not the stream.
          (funcall stream "text" "text" "The sum is 3.")
          (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "The sum is 3."))
          (let ((between (attached-shell "s-mid-round")))
            (funcall stream :facts)
            (is-eventually (equal "The sum is 3." (third (car (last (funcall early)))))
                           "the second round reached the first shell")
            (is-caught-up (between early) "a fact committed and not yet published is not sent twice")
            (is (= 1 (count "The sum is 3." (funcall between) :key #'third :test #'equal))))
          (nlk:complete-turn turn nil)
          (funcall stream :facts)
          (let ((partial (nle::gw-session-partial (nle::find-or-create-gw-session "s-mid-round"))))
            (is (null (nle::stream-partial-rows partial)) "the turn's end leaves nothing in flight"))
          (let ((after (attached-shell "s-mid-round")))
            (is-caught-up (after early) "and a shell attached after the turn reads it whole")))))))

(deftest a-shell-that-attaches-during-a-retry-reads-its-countdown
    (with-gateway-session (port "s-mid-retry"))
  ;; The failed attempt's text is sealed by its fact, which lands before the
  ;; reset; what the late shell owes the early one is the countdown its bar reads.
  (multiple-value-bind (stream turn) (round-in-flight "s-mid-retry" "try it")
    (let ((early (attached-shell "s-mid-retry")))
      (funcall stream "text" "text" "Half an ans")
      (nlk:record-attempt-voided turn :attempt 1 :text "Half an ans" :detail "the stream was cut"
                                      :disposition "rerun")
      (funcall stream :facts)
      (funcall stream "stream_reset" "retry" (nlk:json-object "attempt" 2 "limit" 5 "delay_seconds" 8))
      (is-eventually (gethash "retry_status" (funcall early :state)) "the first shell counts down")
      (let ((late (attached-shell "s-mid-retry")))
        (is-caught-up (late early) "the voided attempt, once")
        (flet ((countdown (shell) (nle::retry-status-text (gethash "retry_status" (funcall shell :state))
                                                          (get-internal-real-time))))
          (is (equal (countdown early) (countdown late)) "and the countdown the first shell reads")))
      (funcall stream "text" "text" "A whole answer")
      (let ((resumed (attached-shell "s-mid-retry")))
        (is-eventually (equal "A whole answer" (third (car (last (funcall resumed)))))
                       "once the stream moves, the rerun's text")
        (is (null (gethash "retry_status" (funcall resumed :state))) "and no countdown")))))

(deftest a-shell-folds-the-retries-its-turn-was-sent-through
    (with-gateway-session (port "s-retried"))
  ;; A shell folds only the durable kinds it declares. Without this,
  ;; turn.provider_retry reaching a shell would fold into nothing, so the
  ;; line a retried turn leaves would stand in a store fold and in no shell.
  (multiple-value-bind (stream turn) (round-in-flight "s-retried" "try it")
    (let ((live (attached-shell "s-retried")))
      (nlk:record-provider-retry turn :status 503 :delay-seconds 6.0 :provider "mock"
                                      :detail "Service is too busy.")
      (funcall stream :facts)
      (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "answered"))
      (nlk:complete-turn turn nil)
      (funcall stream :facts)
      (flet ((line (shell)
               (third (find-if (lambda (row) (search "Waited" (or (third row) ""))) (funcall shell)))))
        (is-eventually (line live) "the live shell leaves the line")
        (is (equal "• Waited 6.0s through 1 retry · mock 503: Service is too busy." (line live)))
        (let ((late (attached-shell "s-retried")))
          (is-eventually (line late) "and a shell attached after reads it off the replay"))))))

(deftest a-replayed-retry-counts-down-what-is-left-of-its-delay ()
  ;; A shell switched back mid-backoff was sent the whole delay again, and
  ;; counted 8s down from the moment it attached.
  (let ((partial (nle::make-stream-partial :turn-id "t1")))
    (nle::stream-partial-delta partial "t1" (nlk:json-object "type" "stream_reset"
                                                             "retry" (nlk:json-object "attempt" 2 "limit" 5
                                                                                      "delay_seconds" 8.0
                                                                                      "status" 503)))
    (decf (nle::stream-partial-reset-at partial) (* 3 internal-time-units-per-second))
    (let ((retry (nlk:json-value (nle::stream-partial-replayed-reset partial) :object "retry")))
      (is (<= 4.9 (nlk:json-value retry :number "delay_seconds") 5.0) "three seconds on, five are left")
      (is (eql 503 (nlk:json-value retry :integer "status")) "the rest rides as it was"))
    ;; The kept reset is not rewritten.
    (is (= 8.0 (nlk:json-value (nle::stream-partial-reset partial) :number "retry" "delay_seconds")))))

(deftest gap-recovery-resubscribe-replay-is-never-suppressed (with-temp-store ())
  (seed-completed-turn "gw-resub" "hello" "answer")
  (with-silent-peer (peer state :resub-peer)
    (is (subscribe-silent-peer "gw-resub" peer))
    ;; The first replay armed the peer's durable-cursor watermark at
    ;; the session cursor; a gap-recovery resubscribe replays the
    ;; same cursor and must still transmit.
    (is (subscribe-silent-peer "gw-resub" peer))
    (bt2:with-lock-held ((nle::gw-peer-state-lock state))
      (is (= 2 (count-if #'nle::gw-replay-job-p
                         (nle::gw-peer-state-queue state)))))
    ;; A live durable frame at or below the watermark stays suppressed
    ;; without burning this peer's sequence.
    (is (nle::publish-event "gw-resub" "session_stream_event"
                            (nlk::make-json-object "frame" (nlk::make-json-object))
                            :cursor (nle::session-cursor "gw-resub")
                            :durable-p t))
    (bt2:with-lock-held ((nle::gw-peer-state-lock state))
      (is (= 2 (length (nle::gw-peer-state-queue state)))))))

;;; --- gate: peer-writer failure is diagnosable and fully reclaimed ---------

(deftest a-poisoned-replay-says-why-before-tombstoning-the-peer (with-temp-store (store))
  ;; End-to-end D4 seam: a corrupt row is refused by the read-side contract
  ;; check, the failed replay emits a sync_protocol_error naming the cause on
  ;; the still-healthy socket, and the tombstoned peer's registry entry is
  ;; released by its exiting writer instead of pinning the socket forever.
  (ensure-durable-session "gw-poisoned")
  (nlk:record-event "gw-poisoned" "probe" (list :n 1))
  (sqlite:execute-non-query
   (nlk::require-open-store)
   "update events set schema_version = 99
       where session_id = ? and kind = 'probe'"
   "gw-poisoned")
  (let* ((socket (make-instance 'recording-peer-ws :require-masking nil))
         (state (nle::ensure-gw-peer-state socket)))
    (nle::enqueue-peer-frame
     socket (nle::make-gw-replay-job
             :session-id "gw-poisoned"
             :cursor (nle::session-cursor "gw-poisoned"))
     :session-id "gw-poisoned")
    (is-eventually (bt2:with-lock-held ((nle::gw-peer-state-lock state))
                     (nle::gw-peer-state-closed-p state))
                   "the failed replay tombstones the peer")
    (is-present (diagnostic (frame-carrying (frames-of-type (mapcar #'shasht:read-json
                                                                    (recording-peer-ws-sent socket))
                                                            "sync_protocol_error")
                                            "peer_writer_failed" "code"))
      "the tombstone is preceded by a diagnostic frame"
      (is (search "contract hash" (nlk:json-value diagnostic :string "payload" "message"))))
    (is-eventually (not (nle::find-gw-peer-state socket))
                   "the exiting writer releases the peer's registry entry")))

(deftest a-fact-past-the-frame-bound-replays-whole ()
  ;; A chunk of one fact is never bisected further: past the bound it goes
  ;; whole rather than tombstoning the peer and leaving the session
  ;; unreadable to every shell that ever attaches.
  (let* ((socket (make-instance 'recording-peer-ws :require-masking nil))
         (job (nle::make-gw-replay-job :session-id "gw-whole" :cursor 1))
         (fact (nlk:json-object
                "event" (nlk:json-object
                         "payload" (make-string nlk:+max-sync-frame-bytes+
                                                :initial-element #\x
                                                :element-type 'base-char)))))
    (is (= 1 (nle::send-replay-chunk (install-silent-peer socket) job (vector fact) 0 t)))
    (is (= 1 (length (recording-peer-ws-sent socket))))
    (is (> (length (first (recording-peer-ws-sent socket))) nlk:+max-sync-frame-bytes+))
    (nle::stop-gw-peer socket)))

(deftest stop-gw-peer-reclaims-the-registry-entry (let* ((peer (list :reclaim-peer))
                                                         (state (install-silent-peer peer))))
  (is (eq state (nle::find-gw-peer-state peer)))
  (nle::stop-gw-peer peer)
  (is (not (nle::find-gw-peer-state peer))))

(deftest client-resumed-frame-resets-the-sequence-epoch (let ((model (detached-shell "epoch"))))
  (setf (nle::gateway-model-last-sequence model) 900
        (nle::gateway-model-last-cursor model) 40)
  (nle::note-resumed-watermark
   model (nlk::make-sync-message :type :projection :sequence 5 :cursor 12))
  (is-shape model (nle::gateway-model-last-sequence = 5) (nle::gateway-model-last-cursor = 12))
  (is (eq :apply (nle::gateway-frame-accepted-p
                  model (nlk::make-sync-message :type :projection
                                                :sequence 6 :cursor 13)))))

(deftest command-result-queue-depth-is-always-an-integer ()
  (let* ((payload (nle::command-result-payload
                   "cmd" "corr" "sid" "cancel_turn" "accepted" "ok" "done"
                   "turn-1" nil)))
    (is (eql 0 (gethash "queue_depth" (gethash "response" payload))))))

;;; --- gate: admitted turns are never orphaned and always published ----------
;;; S-cluster regression seams: the worker latch is only ever transiently held,
;;; so a :STARTED admission waits it out; if it never clears, the turn fails
;;; durably instead of wedging ACTIVE-TURN-P; and the admission facts land on
;;; the live stream, not only in reconnect replay.

(deftest started-admission-waits-out-a-transient-worker-latch (with-session-store ("gw-latch-wait"))
  (let ((gws (nle::find-or-create-gw-session "gw-latch-wait"))
        (provider-calls 0))
    (with-provider-stub (nil (incf provider-calls)
                             (wire-message "assistant" :content "waited"))
      ;; A finishing worker holds the latch briefly between its
      ;; terminal fact and its cleanup; simulate that window.
      (bt2:with-lock-held ((nle::gw-session-lock gws))
        (setf gws.turn-active-p t))
      (nlk:spawn "latch-release"
        (sleep 0.15)
        (bt2:with-lock-held ((nle::gw-session-lock gws)) (setf gws.turn-active-p nil)))
      (is (eq :started (submit-disposition "gw-latch-wait" "hello"
                                           :command-id "gw-latch-wait-cmd")))
      (is-eventually (not (nlk:active-turn-p "gw-latch-wait"))
                     "the admitted turn ran once the latch cleared")
      (is (= 1 provider-calls)))))

(deftest latch-timeout-fails-the-admitted-turn-durably (with-session-store ("gw-latch-dead"))
  (let ((gws (nle::find-or-create-gw-session "gw-latch-dead"))
        (nle::*turn-latch-timeout-seconds* 0.2))
    (with-silent-peer (peer state :latch-dead-peer)
      (nlk:with-cleanup ((bt2:with-lock-held ((nle::gw-session-lock gws))
                           (setf (nle::gw-session-turn-active-p gws) nil
                                 (nle::gw-session-peers gws) '())))
        (bt2:with-lock-held ((nle::gw-session-lock gws))
          (setf gws.peers (list peer)
                gws.turn-active-p t))
        (is (signals-error nle:turn-worker-unavailable
              (nle:submit "gw-latch-dead" "hello"
                          :command-id "gw-latch-dead-cmd")))
        ;; The WebSocket shell maps that signal to the requester's
        ;; rejected result. Drive the start_turn handler directly.
        (funcall (gethash :start-turn nle::*sync-command-handlers*)
                 peer
                 (nlk:json-object "session_id" "gw-latch-dead"
                                  "prompt" "hello")
                 "gw-latch-dead-cmd-2" "gw-latch-dead-cmd-2"
                 (list (list "gw-latch-dead")))
        (is (not (nlk:active-turn-p "gw-latch-dead")))
        (let ((kinds (mapcar #'nlk:event-kind
                             (nlk:page-events :session-id "gw-latch-dead"))))
          (is (member "turn.failed" kinds :test #'equal)))
        (is (row-carrying "worker_unavailable" (%peer-frames state)))))))

(deftest an-admission-whose-publication-fails-is-failed-durably
    (with-session-store ("gw-frame-signal"))
  ;; The admission's first publication runs on the submitting thread, before
  ;; the worker exists. When it fails there -- here a :FRAME hook
  ;; hands the fan-out an op it cannot take, an error out of NEXT that
  ;; isolation passes through -- the turn is failed durably and the latch
  ;; freed, so the session's next submit runs instead of queueing behind a
  ;; turn nobody owns.
  (let ((provider-calls 0)
        (armed t))
    (nle:hook :frame "test-signal"
              (lambda (op next)
                (funcall next (if (and armed (equal (getf op :session-id) "gw-frame-signal"))
                                  :not-an-op
                                  op))))
    (nlk:with-cleanup ((nle:unhook :frame "test-signal"))
      (with-provider-stub (nil (incf provider-calls)
                               (wire-message "assistant" :content "ran"))
        (is (signals-error error
              (nle:submit "gw-frame-signal" "hello" :command-id "gw-frame-signal-1")))
        (is (not (nlk:active-turn-p "gw-frame-signal")) "the admitted turn has its terminal fact")
        (is (not (nle::gw-session-turn-active-p (nle::find-or-create-gw-session "gw-frame-signal"))) "the worker latch is free")
        (is (member "turn.failed"
                    (mapcar #'nlk:event-kind (nlk:page-events :session-id "gw-frame-signal"))
                    :test #'equal))
        (is (zerop provider-calls))
        (setf armed nil)
        (is (eq :started (submit-disposition "gw-frame-signal" "again"
                                             :command-id "gw-frame-signal-2")))
        (is-eventually (not (nlk:active-turn-p "gw-frame-signal"))
                       "the next submit ran")
        (is (= 1 provider-calls))))))

(deftest a-signalling-frame-hook-never-fails-a-turn (with-session-store ("gw-frame-bug"))
  ;; A :FRAME hook's own error is stepped over and said, never the
  ;; publish's failure -- the turn it watched runs to its end, the hook stays
  ;; registered, and its failure stands on the board until it is unhooked.
  (let ((provider-calls 0))
    (nle:hook :frame "test-bug"
              (lambda (op next)
                (when (equal (getf op :session-id) "gw-frame-bug")
                  (error "the frame hook's own bug"))
                (funcall next op)))
    (nlk:with-cleanup ((nle:unhook :frame "test-bug"))
      (with-provider-stub (nil (incf provider-calls)
                               (wire-message "assistant" :content "ran"))
        (is (eq :started (submit-disposition "gw-frame-bug" "hello" :command-id "gw-frame-bug-1")))
        (is-eventually (not (nlk:active-turn-p "gw-frame-bug")) "the watched turn ended")
        (is (= 1 provider-calls))
        (is (member "turn.completed" (mapcar #'nlk:event-kind (nlk:page-events :session-id "gw-frame-bug")) :test #'equal) "it completed")
        (is (assoc "test-bug" (cdr (assoc :frame nlk:*hooks*)) :test #'equal) "the hook stays registered")
        (is (find "hook :frame test-bug" (nlk:standing-notices) :key #'first :test #'string=) "its failure stands on the board")))
    (is (not (find "hook :frame test-bug" (nlk:standing-notices) :key #'first :test #'string=)) "unhooking it clears the board")))

(deftest admission-facts-reach-the-live-stream (with-session-store ("gw-live-adm"))
  (with-silent-peer (peer state :live-admission-peer)
    (let ((gws (nle::find-or-create-gw-session "gw-live-adm")))
      (with-provider-stub (nil (wire-message "assistant" :content "live"))
        (bt2:with-lock-held ((nle::gw-session-lock gws)) (setf gws.peers (list peer)))
        (nle:submit "gw-live-adm" "hello" :command-id "gw-live-adm-cmd")
        (await (:tries 50) (not (nlk:active-turn-p "gw-live-adm")))
        (let ((live (%peer-frames state)))
          (is (row-carrying "turn.input_committed" live))
          (is (row-carrying "turn.started" live)))))))

(deftest submit-builds-the-provenance-its-source-names (with-session-store ("gw-sourced"))
  ;; An in-process submitter with no record of its own -- cron, a team, a
  ;; frame -- names its source kind and id, and SUBMIT builds the record.
  (with-provider-stub (nil (wire-message "assistant" :content "ok"))
    (nle:submit "gw-sourced" "hello" :command-id "gw-sourced-cmd" :source "cron" :source-id "job-1")
    (await (:tries 50) (not (nlk:active-turn-p "gw-sourced"))))
  (let ((json (shasht:read-json (nlk:event-invocation-json
                                 (session-event "gw-sourced" :kind "turn.started")))))
    (is-shape json ("action" "turn.start") ("command_id" "gw-sourced-cmd"))
    (is-shape (gethash "source" json) ("kind" "cron") ("id" "job-1"))
    (is (equal "in_process" (gethash "domain" (gethash "transport" json))))))

(deftest a-sessions-first-prompt-names-it (with-temp-store ())
  ;; A session wears its first prompt's line until a model writes its title,
  ;; one side call on the auxiliary target, and is named once. A name given
  ;; while the call runs outranks the model's, and a prompt of five words or
  ;; fewer is its own name and costs no call.
  (let ((nle::*session-naming* :inline)
        (asked '()))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (push (list session-id user) asked)
                                (when (equal session-id "gw-given")
                                  (nle:set-session-title session-id "Given meanwhile"))
                                "Title: \"Pelican nest survey.\"")
      (with-provider-stub (nil (wire-message "assistant" :content "ok"))
        (flet ((ask (session-id prompt command-id)
                 (ensure-durable-session session-id)
                 (nle:submit session-id prompt :command-id command-id)
                 (await (:tries 50) (not (nlk:active-turn-p session-id)))))
          (ask "gw-named" "count the pelican nests along the north shore" "gw-named-1")
          (is (equal "Pelican nest survey" (nlk::session-title "gw-named")) "the model's title")
          ;; one call, on the session's own target when no auxiliary one is set
          (is (equal '(("gw-named" "count the pelican nests along the north shore")) asked))
          (ask "gw-named" "and now the south shore too, the same way" "gw-named-2")
          (is (= 1 (length asked)) "a named session is named once")
          (ask "gw-given" "count the pelican nests along the north shore" "gw-given-1")
          (is (equal "Given meanwhile" (nlk::session-title "gw-given")) "a given name stands")
          (setf asked '())
          (ask "gw-short" "fix the auth bug" "gw-short-1")
          (is (equal "fix the auth bug" (nlk::session-title "gw-short")))
          (is (null asked) "a short prompt is its own name"))))))

(deftest a-written-title-reads-a-title ()
  ;; What a naming call answered, as a session's name: the first line that
  ;; says something, its label, quotes and period off, cut at a word; an
  ;; answer to the message instead of its name is no title.
  (is-each (nle::written-title)
    ("Capital of Australia vs Sydney" "Capital of Australia vs Sydney" "a bare title")
    ("Title: \"Fix the auth bug.\"" "Fix the auth bug" "label, quotes and period off")
    ("**Title:** Fix the auth bug" "Fix the auth bug" "a bold label")
    ((format nil "~%**Deploy notes**~%more") "Deploy notes" "the first line that says something")
    ("The capital of Australia is Canberra and not Sydney, which many assume since it is bigger"
     nil "an answer, not a name")
    ("Supercalifragilistic expialidocious antidisestablishmentarianism floccinaucinihilipilification"
     "Supercalifragilistic expialidocious antidisestablishmentarianism" "cut at a word")
    ("" nil "nothing")
    (nil nil "no reply")))

(defmacro with-retract-turn ((turn turn-id peer state session-id &key prompt prior (parked t))
                             stub &body body)
  "Over a temp store where SESSION-ID holds one earlier prompt, subscribe PEER (a
silent peer, STATE its state) and start PROMPT with CALL-PROVIDER answering STUB;
run BODY with TURN the admitted turn, TURN-ID its id and PRIOR the head before it
— once its worker parked in the provider wait, unless PARKED is NIL."
  `(with-temp-store ()
     (seed-user-inputs ,session-id '("earlier"))
     (with-silent-peer (,peer ,state ,session-id)
       (with-provider-stub (nil ,@stub)
         (let* (,@(when prior `((,prior (nlk:session-head-turn-id ,session-id))))
                (,turn (let ((gws (nle::find-or-create-gw-session ,session-id)))
                         (bt2:with-lock-held ((nle::gw-session-lock gws))
                           (setf (nle::gw-session-peers gws) (list ,peer)))
                         (nlk:active-input-admission-turn
                          (nle:submit ,session-id ,prompt
                                      :command-id (format nil "~a-cmd" ,session-id)))))
                (,turn-id (nlk:durable-turn-turn-id ,turn)))
           ,@(when parked `((is (%wait-for-parked-worker ,turn) "the worker parked in its wait")))
           ,@body)))))

(defun park-until-cancelled ()
  "A provider stub's answer that never comes: a cancellable wait on the current
turn, which a cancel of that turn interrupts."
  (nlk:with-cancellable-wait (nle::*current-durable-turn*)
    (sleep 30)))

(defun %wait-for-parked-worker (turn)
  "Wait until the worker is inside its cancellable provider wait."
  (await () (nlk::cancellation-signal-waiting-threads (nlk::turn-signal turn))))

(defun %wait-for-worker-exit (session-id &aux (gws (nle::find-or-create-gw-session session-id)))
  "Wait until the session's worker released the gateway latch (cleanup done,
retraction included)."
  (await (:tries 40) (not (bt2:with-lock-held ((nle::gw-session-lock gws)) gws.turn-active-p))))

(defun %drive-cancel (peer session-id turn-id &key retract)
  "Drive the cancel_turn sync command the way the WS dispatcher does."
  (funcall (gethash :cancel-turn nle::*sync-command-handlers*)
           peer
           (apply #'nlk::make-json-object
                  "session_id" session-id "turn_id" turn-id
                  (when retract (list "retract" t)))
           "cancel-cmd" "cancel-cmd"
           (list (list session-id))))

(defun %peer-frames (state)
  (bt2:with-lock-held ((nle::gw-peer-state-lock state))
    (remove-if-not #'stringp (copy-list (nle::gw-peer-state-queue state)))))

(defun %visible-user-inputs (session-id)
  (loop for event in (nle::collect-visible-session-events session-id)
        when (typep event 'nlk:turn-started)
          collect (nlk:turn-started-input event)))

(defun %provider-user-texts (session-id)
  "The user-role contents the provider fold would send for SESSION-ID."
  (loop for message in (nle::wire-messages-from-events
                        (nle::collect-visible-session-events session-id))
        when (equal "user" (gethash "role" message))
          collect (gethash "content" message)))

(deftest retracting-cancel-rewinds-the-head-and-resyncs-peers ()
  ;; Esc before anything streamed, with the take-it-back flag: the turn ends
  ;; cancelled with zero rounds, the gateway moves the head back to the
  ;; parent under the worker's latch, every peer is told to resync, and the
  ;; prompt is gone from the visible chain — and so from the provider fold.
  (with-retract-turn (turn turn-id peer state "gw-retract" :prompt "take me back" :prior prior)
      ((park-until-cancelled))
    (%drive-cancel peer "gw-retract" turn-id :retract t)
    (is (%wait-for-worker-exit "gw-retract") "the worker settled")
    (is (equal "turn.cancelled" (nlk:turn-terminal-kind turn-id)))
    (is (zerop (nlk:turn-assistant-round-count turn-id)))
    (is (equal prior (nlk:session-head-turn-id "gw-retract")))
    (is (equal '("earlier") (%visible-user-inputs "gw-retract")))
    (is (equal '("earlier") (%provider-user-texts "gw-retract")))
    (is (null (nlk::last-head-move-origin "gw-retract")))
    (let ((frames (%peer-frames state)))
      (is (row-carrying "turn.cancelled" frames))
      (is-present (undo (row-carrying "session_checkpoint_undo_applied" frames))
          "peers were told the head moved"
          (is (search "\"reason\":\"retract\"" undo))
          (is (search turn-id undo)))
      (is (row-carrying "session_resync_required" frames))
      (is (< (row-index-of "turn.cancelled" frames)
             (row-index-of "session_resync_required" frames))))))

(deftest plain-cancel-never-retracts ()
  ;; Without the flag a cancel of an unproduced turn is the cancel it always
  ;; was: the turn stays on the chain, nobody resyncs.
  (with-retract-turn (turn turn-id peer state "gw-plain-cancel" :prompt "stays")
      ((park-until-cancelled))
    (%drive-cancel peer "gw-plain-cancel" turn-id)
    (is (%wait-for-worker-exit "gw-plain-cancel") "the worker settled")
    (is (equal turn-id (nlk:session-head-turn-id "gw-plain-cancel")))
    (is (equal '("earlier" "stays") (%visible-user-inputs "gw-plain-cancel")))
    (is (not (row-carrying "session_resync_required" (%peer-frames state))))))

(deftest retract-is-refused-once-a-round-closed ()
  ;; A cancel that lands after the provider returned keeps the closed round
  ;; (paid for, durable) — and with it the prompt: retraction is refused on
  ;; the durable round count, not on what the client believed.
  (with-retract-turn (turn turn-id peer state "gw-retract-late" :prompt "answered" :parked nil)
      ((nlk:request-cancel-turn
        "gw-retract-late"
        (nlk:durable-turn-turn-id nle::*current-durable-turn*)
        "esc" t)
       (wire-message "assistant" :content "kept answer"))
    (is (%wait-for-worker-exit "gw-retract-late") "the worker settled")
    (is (equal "turn.cancelled" (nlk:turn-terminal-kind turn-id)))
    (is (= 1 (nlk:turn-assistant-round-count turn-id)))
    (is (equal turn-id (nlk:session-head-turn-id "gw-retract-late")))
    (is (not (row-carrying "session_checkpoint_undo_applied" (%peer-frames state))))))

(deftests esc-with (session-id answer disposition input keys parked-why ran-why)
  (let ((calls 0))
    (with-retract-turn (turn turn-id peer state session-id :prompt "first")
        ((if (= 1 (incf calls))
             (park-until-cancelled)
             (wire-message "assistant" :content answer)))
      (is (eq disposition (apply #'submit-disposition session-id input
                                 :command-id (format nil "~a-cmd-2" session-id) keys))
          parked-why)
      (%drive-cancel peer session-id turn-id :retract t)
      (is (await (:tries 40)
            (and (= 2 calls) (not (nlk:active-turn-p session-id))))
          ran-why)
      ;; The cut turn rests cancelled, unretracted; the parked input runs next.
      (is (equal "turn.cancelled" (nlk:turn-terminal-kind turn-id)))
      (is (zerop (nlk:turn-assistant-round-count turn-id)))
      (let* ((starts (remove-if-not
                      (lambda (event) (typep event 'nlk:turn-started))
                      (nle::collect-session-events session-id)))
             (next-start (find input starts
                               :key #'nlk:turn-started-input
                               :test #'equal)))
        (is-present next-start "the parked input became its own turn"
            (is (equal turn-id (nlk:turn-started-parent-turn-id next-start)))
            (is (equal (nlk:event-turn-id next-start)
                       (nlk:session-head-turn-id session-id)))))
      (is (equal (list "earlier" "first" input)
                 (%visible-user-inputs session-id)))
      (is (equal (list "earlier" "first" input)
                 (%provider-user-texts session-id)))
      (let ((frames (%peer-frames state)))
        (is (row-carrying "turn.cancelled" frames))
        (is (not (row-carrying "session_checkpoint_undo_applied" frames)))
        (is (not (row-carrying "session_resync_required" frames))))))
  ;; Esc while a steer is parked is not the operator taking the prompt back:
  ;; the running turn is cut where it stands (the stub's 30 s wait is
  ;; interrupted, not waited out), rests on `cancelled' with its prompt on
  ;; the chain, and the steer runs at once as the next turn on top of it.
  ;; The retract flag the composer sends is refused on the parked input, so
  ;; no head moves and nobody resyncs — what the operator asked for and then
  ;; hurried past is still what the next turn picks up from.
  (a-steer-parked-cuts-the-turn-and-runs-the-steer-on-it
   "gw-retract-steer" "steer answered" :steered "and also this" (list :steer turn-id)
   "the steer parked on the running turn"
   "the steer ran as the next turn within seconds of the cancel")
  ;; A follow-up queued behind the running turn, then Esc: the same cut.
  ;; The turn stops where it stands and rests on `cancelled', the queued
  ;; prompt is promoted onto it at once as the next turn, and the cut
  ;; prompt stays in the chain and in the provider fold ahead of it.
  (a-follow-up-queued-cuts-the-turn-and-runs-it-next
   "gw-retract-queue" "second answered" :queued "second" nil
   "the follow-up queued behind the running turn"
   "the follow-up ran as the next turn within seconds of the cancel"))

(deftest frame-hook-sees-every-live-frame-and-can-veto (with-session-store ("gw-frame-hook"))
  ;; The :FRAME point on PUBLISH-EVENT: an in-process observer receives
  ;; each live frame of every session without a socket, on the publishing
  ;; thread; one that returns without calling NEXT keeps the frame from
  ;; every peer. This is the seam channel cells observe through.
  (with-silent-peer (peer state :frame-hook-peer)
    (let ((gws (nle::find-or-create-gw-session "gw-frame-hook"))
          (seen '())
          (seen-lock (bt2:make-lock)))
      (flet ((own-p (op) (equal (getf op :session-id) "gw-frame-hook")))
        ;; Registered globally, not LET-bound: the frames are published
        ;; from the turn worker, and a fresh thread sees the global value.
        (nle:hook :frame "test-observe"
                  (lambda (op next)
                    (when (own-p op)
                      (bt2:with-lock-held (seen-lock)
                        (push (list (getf op :kind)
                                    (nlk:json-value (getf op :payload) :string
                                                    "frame" "event" "type"))
                              seen)))
                    (funcall next op)))
        (nle:hook :frame "test-veto"
                  (lambda (op next)
                    (if (own-p op) nil (funcall next op)))))
      (nlk:with-cleanup ((nle:unhook :frame "test-observe") (nle:unhook :frame "test-veto"))
        (with-provider-stub (nil (wire-message "assistant" :content "hooked"))
          (bt2:with-lock-held ((nle::gw-session-lock gws)) (setf gws.peers (list peer)))
          (nle:submit "gw-frame-hook" "hello" :command-id "gw-frame-hook-cmd")
          (await (:tries 50) (not (nlk:active-turn-p "gw-frame-hook")))
          (let ((types (bt2:with-lock-held (seen-lock)
                         (mapcar #'second (reverse seen)))))
            (is (member "turn.started" types :test #'equal))
            (is (member "turn.completed" types :test #'equal))
            (is (every (lambda (entry)
                         (equal "session_stream_event" (first entry)))
                       seen)))
          (is (null (%peer-frames state)))
          (is (eq :duplicate (submit-disposition "gw-frame-hook" "hello"
                                                 :command-id "gw-frame-hook-cmd"))))))))

;;; --- gate: resume/replay lane contract ------------------------------------
;;; Tier-1 TUI foundation seams: cursor presence, batch paging, retention
;;; horizons, and the operator eviction control op.

(deftest a-long-sessions-replay-sends-its-newest-turns-first (with-temp-gateway (port))
  ;; The first pass carries what the shell's header reads from before it --
  ;; the first prompt, the usage it sums -- then the newest turns from a
  ;; prompt, and ends final; the whole replay follows it from chunk 0. The
  ;; bounds are the writer thread's, so they are set, never bound.
  (let ((turn (nlk:admit-turn (ensure-durable-session "tail-wire") "tail-priced" "priced")))
    (nlk:record-turn-usage turn :model "m" :input-tokens 3 :output-tokens 5)
    (nlk:complete-turn turn nil))
  (seed-user-inputs "tail-wire" (loop for i below 40 collect (format nil "tail-~d" i)))
  (with-saved-globals ((nle::*replay-tail-minimum* 32) (nle::*replay-tail-facts* 7))
    (let* ((chunks (frames-of-type (subscribed-frames port "tail-wire" :timeout 0.5)
                                   "session_stream_resumed"))
           (tail (loop for chunk in chunks
                       while (nlk:json-value chunk :boolean "payload" "replay_tail")
                       collect chunk))
           (whole (nthcdr (length tail) chunks)))
      (flet ((kinds (chunks)
               (mapcar (lambda (fact) (nlk:json-value fact :string "event" "type"))
                       (replay-frames chunks))))
        (is (nlk:json-value (car (last tail)) :boolean "payload" "replay_chunk_final") "ends final")
        ;; the first prompt and the usage, then the turn holding the 7th newest fact and on
        (is (equal (list* "turn.input_committed" "turn.usage"
                          (loop repeat 3
                                append '("turn.input_committed" "turn.started" "turn.completed")))
                   (kinds tail)))
        (is (equal "tail-37" (nlk:json-value (third (replay-frames tail))
                                             :string "event" "payload" "message")))
        (is (eql 0 (nlk:json-value (first whole) :integer "payload" "replay_chunk_index")) "anew")
        (is (notany (lambda (chunk) (nlk:json-value chunk :boolean "payload" "replay_tail")) whole))
        (is (nlk:json-value (car (last whole)) :boolean "payload" "replay_chunk_final"))
        (is (= 125 (length (kinds whole))) "and carries every fact")))))

(deftest a-replay-folds-out-of-sight-and-its-first-pass-paints (let ((model (detached-shell))
                                                                     (painted '())
                                                                     (trims 0)))
  ;; Whatever renders before a replay's last chunk shows the transcript that
  ;; was; the first pass paints at its own last chunk; the whole replay takes
  ;; its place at its own, and only then is the shell settled on it.
  (with-stubbed-fdefinitions ((nle::render-state (m)
                                (push (mapcar #'nle::transcript-row-text
                                              (nle::transcript-rows (nle::chat-model-state m)))
                                      painted))
                              (nle::trim-behind-the-ready-frame () (incf trims)))
    (flet ((prompt (seq text)
             (fact-frame "turn.input_committed" (format nil "turn-~d" seq) :seq seq
                         :payload (nlk::make-json-object "role" "user" "message" text)
                         :command-id (format nil "command-~d" seq)
                         :occurred-at "2026-09-22T23:43:34.000Z"))
           (shown () (mapcar #'nle::transcript-row-text
                             (nle::transcript-rows (nle::chat-model-state model)))))
      (feed-wire model (nlk:encode-session-stream-resumed
                        "s" 7 (vector (prompt 3 "newest")) :cursor 3 :admission-state "ready"
                        :replay-chunk-index 0 :replay-chunk-final-p t :replay-tail-p t))
      (is (equal '(("newest")) painted) "the first pass paints")
      (is (zerop trims) "and settles nothing")
      (feed-wire model (nlk:encode-session-stream-resumed
                        "s" 7 (vector (prompt 1 "oldest")) :admission-state "ready"
                        :replay-chunk-index 0))
      (is (equal '("newest") (shown)) "the whole replay folds out of sight")
      (feed-wire model (nlk:encode-session-stream-resumed
                        "s" 7 (vector (prompt 2 "middle") (prompt 3 "newest")) :cursor 3
                        :admission-state "ready" :replay-chunk-index 1 :replay-chunk-final-p t))
      (is (equal '("oldest" "middle" "newest") (shown)) "and takes the first pass's place")
      (is (equal '("oldest" "middle" "newest") (first painted)))
      (is (= 1 trims) "the shell is settled on the whole replay"))))

(deftest replay-suppresses-durable-evidence-kinds (with-gateway-session (port "retention-wire"))
  (with-durable-turn (turn "retention-wire" :command-id "retention-cmd" :input "hello" :complete t)
    (nlk:record-turn-usage turn :model "m" :input-tokens 3 :output-tokens 5)
    (nlk:record-turn-provider-request turn :provider "p" :model "m"))
  (let* ((frames (subscribed-frames port "retention-wire" :timeout 0.5))
         (kinds (mapcar (lambda (fact)
                          (gethash "type" (gethash "event" fact)))
                        (replay-frames frames))))
    (is-table (kind) (member kind kinds :test #'equal)
      ("turn.input_committed" "history facts replay")
      ("turn.completed" "terminal facts replay")
      ("turn.usage" "usage replays: a resumed client rebuilds its token lane from the
           session's recorded usage instead of showing an empty lane"))
    (is (not (member "turn.provider_request" kinds :test #'equal)))
    (is (= (gethash "cursor" (car (last (frames-of-type frames "session_stream_resumed"))))
           (nlk::session-log-cursor "retention-wire")))))

(deftest gateway-evict-route-folds-percent-share (with-temp-gateway (port))
  ;; Four identical input-only turns: equal token estimates make the
  ;; forced-planner boundary walk exact — 50% of four equal turns is the
  ;; two oldest, realized share exactly 50.
  (seed-user-inputs "evict-http" '("ctx" "ctx" "ctx" "ctx"))
  (with-gateway-http (port :post "/api/gateway/sessions/evict-http/evict"
                      :body "{\"percent\":50}" :status 200 :says "authorized eviction succeeds")
    (is-table (key expected) (= expected (gethash key body))
      ("evicted_turn_count" 2 "the percent target folds the oldest whole turns")
      ("requested_percent" 50 "the response echoes the request")
      ("realized_percent" 50 "equal turns realize the requested share exactly")
      ("evicted_message_count" 2 "the fold counts messages on the evicted side")
      ("retained_message_count" 2 "the fold counts messages on the retained side"))
    (is (and (stringp (gethash "event_id" body))
             (plusp (length (gethash "event_id" body)))))
    (is (plusp (gethash "source_watermark" body)))
    (is (plusp (gethash "evicted_estimated_tokens" body)))
    (is (= (gethash "retained_from_sequence" body)
           (nlk:retained-history-floor "evict-http"))))
  (with-gateway-http (port :post "/api/gateway/sessions/evict-http/evict" :body "{}" :status 200)
    (is-shape body ("evicted_turn_count" = 1) ("requested_percent" = 50))
    (is (= (gethash "retained_from_sequence" body)
           (nlk:retained-history-floor "evict-http"))))
  (is-route (port :post "/api/gateway/sessions/evict-http/evict" :body "{}") 409
            nil "no_history_to_evict")
  (is-route (port :post "/api/gateway/sessions/evict-http/evict" :body "{\"percent\":0}") 400
            "zero percent is malformed, not a request")
  (is-route (port :post "/api/gateway/sessions/evict-http/evict" :body "{\"percent\":101}") 400
            "percent beyond 100 is malformed")
  (is-route (port :post "/api/gateway/sessions/evict-http/evict" :body "{}" :token "wrong") 401
            "eviction requires operator authentication")
  (is-route (port :post "/api/gateway/sessions/missing/evict" :body "{}") 404
            "eviction refuses unknown sessions")
  (let* ((frames (subscribed-frames port "evict-http" :timeout 0.5))
         (evicted (remove "session.context.evicted" (replay-frames frames) :test-not #'equal
                          :key (lambda (fact) (gethash "type" (gethash "event" fact))))))
    (is (= 2 (length evicted)) "both eviction dividers replay in order")
    (let ((payload (gethash "payload" (gethash "event" (first evicted)))))
      (is (equal "operator_requested" (gethash "reason" payload)))
      (is (plusp (gethash "source-watermark" payload)))
      (is (plusp (gethash "evicted-message-count" payload)))
      (is-shape payload ("requested-percent" = 50) ("realized-percent" = 50))
      (is (plusp (gethash "evicted-estimated-tokens" payload))))))

;;; =========================================================================
;;; the model selection is durable, and config is the only place it lives
;;; =========================================================================

(defmacro with-model-selection-config ((path-var &optional (text "{~%  \"general\": {}~%}~%"))
                                       &body body &aux (dir (gensym "DIR")))
  "Point *SHARED-CONFIG-PATH* at a fresh config for BODY and bind PATH-VAR to
it."
  ;; SETF, not LET: the routes under test run on the gateway's connection
  ;; thread, which would never see a test-thread binding. Also saves and
  ;; restores the provider globals the selection moves.
  `(with-temp-directory (,dir "model-selection")
     (let ((,path-var (write-temp-file (merge-pathnames "config.jsonc" ,dir) (format nil ,text))))
       (with-saved-globals ((nodecode.evolved::*shared-config-path* ,path-var)
                            nle::*provider* nle::*model* nle::*endpoint* nle::*reasoning-effort*
                            nle::*auxiliary-model* nle::*model-config-refusal*)
         ,@body))))

(defmacro with-selection-gateway ((path port session-id &key text (model "boot-model") pinned)
                                  &body body)
  "WITH-MODEL-SELECTION-CONFIG, over TEXT when given -- PINNED, one whose
general.default_model is openai-completions/MODEL -- around a WITH-TEMP-GATEWAY
whose provider globals are openai-completions/MODEL and whose store holds the
created session SESSION-ID."
  `(with-model-selection-config
       (,path ,@(cond (text (list text))
                      (pinned (list (format nil "{\"general\": {\"default_model\": ~
                                                 {\"provider\": \"openai-completions\", ~
                                                  \"model\": ~s}}}" model)))))
     (with-temp-gateway (,port)
       (setf nle::*provider* "openai-completions"
             nle::*model* ,model)
       (nlk:create-session :id ,session-id)
       ,@body)))

(defun config-pin (path key &optional member &aux (nle::*shared-config-path* path)
                                                  (nle::*shared-config-memo* nil))
  "general.<KEY>.<MEMBER> — a member of one of the two model pin objects, or
the flat general.<KEY> itself without MEMBER — read straight off disk, through
the one reader."
  (apply #'nlk:json-value (nle::read-shared-config) :string "general" key
         (and member (list member))))

(deftest gateway-model-selection-survives-restart (with-selection-gateway (path port "restart"))
  ;; /api/gateway/model used to be three defparameters a restart reverted, then
  ;; plain gateway_config rows that outranked the operator's own config file and
  ;; were written by this route alone. Now the pair goes to
  ;; general.default_model as {provider, model}, and the boot lane reads
  ;; only that. Globals are SETF'd, not LET-bound: the POST executes on the
  ;; gateway's connection thread.
  (setf nle::*endpoint* nil)
  (with-gateway-http (port :post "/api/gateway/model"
                      :body "{\"provider\":\"anthropic\",\"model\":\"claude-x\"}")
    (is (= 200 status) "the model op accepts the selection")
    (is-shape body ("provider" "anthropic") ("shared_config" "written")))
  (is (equal "anthropic" (config-pin path "default_model" "provider")))
  (is (equal "claude-x" (config-pin path "default_model" "model")))
  ;; Simulate the restart: revert the globals, run the boot lane.
  (setf nle::*provider* "openai-completions"
        nle::*model* "boot-model"
        nle::*endpoint* nil)
  (nle::restore-model-config)
  (is (equal "anthropic" nle::*provider*) "the boot lane reapplies the provider")
  (is (equal "claude-x" nle::*model*) "and the model")
  (is (null nle::*endpoint*) "and the lane default endpoint stands"))

(deftest gateway-model-restore-seeds-an-unknown-provider-and-refuses-it (with-model-selection-config)
  (path "{~%  \"general\": {\"default_model\": {\"provider\": \"retired-lane\", \"model\": \"still-good\"}}~%}~%")
;; vr-101: skipping it left the baked default lane under the pin's model, and
;; every turn went to OpenCode Zen as "public". The pin is seeded as written,
;; and every shell is told at boot why it cannot serve.
(setf nle::*provider* "openai-completions"
      nle::*model* "m0"
      nle::*endpoint* nil)
(handler-bind ((warning #'muffle-warning))
  (nle::restore-model-config))
(is (equal "retired-lane" nle::*provider*))
(is (equal "still-good" nle::*model*))
(is (search "provider retired-lane is not" (gethash "refusal" (nle::gateway-target-json))))
(is (equal "retired-lane" (config-pin path "default_model" "provider"))))

(deftest the-model-route-answers-a-selection-with-no-address-and-why ()
  ;; Without this, a provider with no endpoint of its own (providers.acme, a
  ;; key, no base_url) would make GET /api/gateway/model a 400, and the
  ;; Settings picker would disappear. The route answers the selection, no
  ;; endpoint, and the refusal every shell shows; an explicit endpoint on
  ;; the model op is the address a pick rides.
  (with-selection-gateway (path port "no-address"
                           :text "{~%  \"providers\": {\"acme\": {\"sdk\": \"openai-completions\", \"api_key\": \"k\"}}~%}~%")
    (setf nle::*provider* "acme" nle::*model* "acme-large" nle::*endpoint* nil)
    (with-gateway-http (port :get "/api/gateway/model" :status 200)
      (is (eq :null (gethash "endpoint" body)))
      (is (search "providers.acme has no base_url" (gethash "refusal" body)))
      (is (equal (nle::selection-refusal "acme") (gethash "refusal" body)) "in the words every shell shows"))
    (is (search "endpoint: none: providers.acme has no base_url" (nle::model-selection-summary)) "as /models reads it")
    (setf nle::*provider* "openai-completions")
    (is-route (port :post "/api/gateway/model" :body "{\"provider\":\"acme\",\"model\":\"acme-large\"}") 400
              "a pick with no address is refused" "unknown_provider")
    (is (equal "openai-completions" nle::*provider*) "and moves nothing")
    (with-gateway-http (port :post "/api/gateway/model"
                        :body "{\"provider\":\"acme\",\"model\":\"acme-large\",\"endpoint\":\"http://127.0.0.1:9/v1/chat/completions\"}")
      (is (= 200 status) "one with an explicit endpoint rides it")
      (is-shape body ("endpoint" "http://127.0.0.1:9/v1/chat/completions") ("refusal" eq :null)))
    (is (null (nle::selection-refusal "acme")) "the override is its address while it stands")))

(deftest a-session-model-pick-moves-the-default-for-sessions-not-yet-begun ()
  ;; A pick is the default for new sessions, and a session already begun keeps
  ;; its own. The pick moves the default; a begun session's first turn pins the
  ;; pair it ran (RUN), which no move of the default reaches.
  (with-selection-gateway (path port "pick-session" :pinned t)
    (nlk:create-session :id "begun")
    (nlk:record-session-model-selection "begun" :provider "openai-completions" :model "boot-model")
    (nlk:create-session :id "fresh")
    (with-gateway-http (port :post "/api/gateway/sessions/pick-session/model"
                        :body "{\"model\":\"picked\"}")
      (is (= 200 status) "a model-only pick is accepted")
      (is-shape body ("selected_provider" "openai-completions") ("selected_model" "picked")
                ("shared_config" "written")))
    (is-model-pin "pick-session" "openai-completions" "picked"
                  "the session is pinned to the lane the pick resolved to" "and the pick")
    (is (equal "openai-completions" (config-pin path "default_model" "provider")) "the config's default moved")
    (is (equal "picked" (config-pin path "default_model" "model")) "as a pair")
    (is (equal "picked" nle::*model*) "as did the image's")
    (with-gateway-http (port :get "/api/gateway/target?session_id=begun")
      (is (equal "boot-model" (nlk:json-value body :string "model")) "a session already begun keeps its model")
      (is (equal "picked" (nlk:json-value body :string "default" "model")) "beside the default the target names"))
    (with-gateway-http (port :get "/api/gateway/target?session_id=fresh")
      (is (equal "picked" (nlk:json-value body :string "model")) "a session not yet begun starts on the pick"))
    (is-route (port :post "/api/gateway/sessions/pick-session/model"
               :body "{\"provider\":\"no-such-lane\",\"model\":\"m\"}") 400
              "an unknown lane is refused" "unknown_provider")
    (is-model-pin "pick-session" "openai-completions" "picked" "and the pin stands" "unmoved")
    (is (equal "picked" (config-pin path "default_model" "model")) "as does the default")))

(deftest model-selection-is-the-same-fact-from-either-client ()
  ;; "No matter the client" is the whole point: the owner shell's in-process
  ;; /models and an attached client's HTTP pick land the same two facts, the
  ;; session's own pin and the default, each pick's in turn.
  (with-selection-gateway (path port "owner-shell" :pinned t)
    (nle:slash "/models anthropic claude-x" :session-id "owner-shell")
    (is-model-pin "owner-shell" "anthropic" "claude-x" "the owner shell pinned its own session" "and the model")
    (nlk:create-session :id "attached")
    (with-gateway-http (port :post "/api/gateway/sessions/attached/model"
                        :body "{\"provider\":\"anthropic\",\"model\":\"claude-x\"}")
      (is (= 200 status) "the attached client's pick is accepted"))
    (is-model-pin "attached" "anthropic" "claude-x" "the attached client's the same" "fact")
    (is (equal "anthropic" (config-pin path "default_model" "provider")) "both moved the default")
    (is (equal "claude-x" (config-pin path "default_model" "model")) "as a pair")
    (is (equal "claude-x" nle::*model*))))

(deftest gateway-session-model-clear-leaves-the-default-alone (with-selection-gateway)
  (path port "clear-session" :model "kept" :pinned t)
(nlk:record-session-model-selection "clear-session" :model "pinned")
(with-gateway-http (port :post "/api/gateway/sessions/clear-session/model"
                    :body "{\"provider\":null,\"model\":null}")
  (is (= 200 status) "clearing is accepted")
  (is (null (gethash "shared_config" body))))
(is-model-pin "clear-session" nil nil "the pin is gone" "on both fields")
(is (equal "kept" (config-pin path "default_model" "model")))
(is (equal "kept" nle::*model*) "as are the globals"))

;;; --- start_turn names the target a fresh session runs on ---------------------

(defun %drive-start-turn (peer session-id command-id &rest members)
  "Drive the start_turn handler for SESSION-ID as PEER, subscribed to it, with
the prompt hello and MEMBERS, wire keys and values alternating, on the
request."
  (funcall (gethash :start-turn nle::*sync-command-handlers*)
           peer
           (apply #'nlk:make-json-object "session_id" session-id "prompt" "hello" members)
           command-id command-id
           (list (list session-id))))

(deftest a-start-turn-pins-the-target-a-fresh-session-runs-on
    (with-session-store ("pin-fresh"))
  ;; A program driving rpc names the model of the one task it starts: that
  ;; session runs on it, and the default every other session takes stays.
  (with-model-selection-config
      (path "{\"general\": {\"default_model\": {\"provider\": \"openai-completions\", \"model\": \"boot-model\"}}}")
    (setf nle::*provider* "openai-completions"
          nle::*model* "boot-model")
    (let ((seen '()))
      (with-silent-peer (peer state :pin-fresh-peer)
        (with-provider-stub (nil (push (list nle::*provider* nle::*model*) seen)
                                 (wire-message "assistant" :content "ok"))
          (%drive-start-turn peer "pin-fresh" "pin-fresh-1" "model" "cheap-model" "effort" "low")
          (is-eventually (not (nlk:active-turn-p "pin-fresh")) "the turn ran")
          (is (row-carrying "turn admitted" (%peer-frames state)) "once admitted")))
      (is (equal '(("openai-completions" "cheap-model")) seen) "on the model it named, the session's lane kept")
      (is-model-pin "pin-fresh" "openai-completions" "cheap-model" "the session is pinned to it" "as a pair")
      (is (equal "low" (nlk:session-model-effort "pin-fresh")) "at the effort it named")
      (is (equal "boot-model" (config-pin path "default_model" "model")) "the config's default stays")
      (is (equal "boot-model" nle::*model*) "as does the image's"))))

(deftest a-begun-session-takes-only-its-own-target-from-a-start-turn
    (with-session-store ("pin-begun"))
  ;; A begun session keeps what it began on: another target is refused before
  ;; anything is admitted, and its own -- a reconnecting client's resend -- is
  ;; taken.
  (with-model-selection-config (path)
    (setf nle::*provider* "openai-completions"
          nle::*model* "boot-model")
    (nlk:record-session-model-selection "pin-begun" :provider "openai-completions" :model "boot-model")
    (let ((calls 0))
      (with-silent-peer (peer state :pin-begun-peer)
        (with-provider-stub (nil (incf calls) (wire-message "assistant" :content "ok"))
          (%drive-start-turn peer "pin-begun" "pin-begun-1" "model" "cheap-model")
          (is (row-carrying "session_pinned" (%peer-frames state)) "another model is refused")
          (is (= 0 calls) "before any turn is admitted")
          (is-model-pin "pin-begun" "openai-completions" "boot-model" "the pin stands" "unmoved")
          (%drive-start-turn peer "pin-begun" "pin-begun-2"
                             "provider" "openai-completions" "model" "boot-model")
          (is-eventually (= 1 calls) "its own target is taken, and the turn runs")
          (is-eventually (not (nlk:active-turn-p "pin-begun")) "to its end"))))))

(deftest a-start-turn-target-refused-pins-nothing
    (with-session-store ("pin-refused"))
  ;; The members are checked before anything is written: a refused target
  ;; leaves the session as it was and admits no turn.
  (with-model-selection-config (path)
    (setf nle::*provider* "openai-completions"
          nle::*model* "boot-model")
    (let ((calls 0))
      (with-silent-peer (peer state :pin-refused-peer)
        (with-provider-stub (nil (incf calls) (wire-message "assistant" :content "ok"))
          (%drive-start-turn peer "pin-refused" "pin-refused-1" "provider" "no-such-lane" "model" "m")
          (is (row-carrying "unknown_provider" (%peer-frames state)) "an unknown lane is refused by its reason")
          (%drive-start-turn peer "pin-refused" "pin-refused-2" "model" "cheap-model" "effort" "turbo")
          (is (row-carrying "effort_unsupported" (%peer-frames state)) "as is a rung the target lacks")
          (is (= 0 calls) "neither admitted a turn")
          (is-model-pin "pin-refused" nil nil "and neither pinned" "the session"))))))

(deftest an-admission-refused-after-the-pin-takes-the-pin-back
    (with-session-store ("pin-undo"))
  ;; The pin lands before admission; an admission refused after it leaves the
  ;; session following the default, as it was.
  (let ((gws (nle::find-or-create-gw-session "pin-undo"))
        (nle::*turn-latch-timeout-seconds* 0.2))
    (with-model-selection-config (path)
      (setf nle::*provider* "openai-completions"
            nle::*model* "boot-model")
      (with-silent-peer (peer state :pin-undo-peer)
        (nlk:with-cleanup ((bt2:with-lock-held ((nle::gw-session-lock gws))
                             (setf (nle::gw-session-turn-active-p gws) nil
                                   (nle::gw-session-peers gws) '())))
          (bt2:with-lock-held ((nle::gw-session-lock gws))
            (setf (nle::gw-session-peers gws) (list peer)
                  (nle::gw-session-turn-active-p gws) t))
          (%drive-start-turn peer "pin-undo" "pin-undo-1" "model" "cheap-model")
          (is (row-carrying "worker_unavailable" (%peer-frames state)) "the admission is refused")
          (is-model-pin "pin-undo" nil nil "and the pin it followed is taken back" "whole"))))))

(deftest select-model-lands-the-pick-and-every-shell-learns ()
  ;; The model-facing verb must be the whole meaning of a pick — the
  ;; durable pin, the default new sessions start on, and a frame every
  ;; attached shell refreshes its footer from — in one call, on the
  ;; ambient session.
  (with-selection-gateway (path port "select-verb" :pinned t)
    (let ((frames
            (frames-while (ws port :subscribe '("select-verb"))
              (let ((nlk:*scribe-session-id* "select-verb"))
                (is-carrying (answer (nle:select-model "claude-x" :provider "anthropic"))
                  "pinned to anthropic/claude-x" "the default for new sessions now")))))
      (is-present (frame (find-frame frames "model_changed"))
        "the attached peer learned the target moved"
        (is-shape frame ("sequence" eql 0))
        (is (null (nlk:json-value frame :string "payload" "session_id")) "session-less: the default moved")))
    (is-model-pin "select-verb" "anthropic" "claude-x"
                  "the pin records the provider as asked" "and the model")
    (is (equal "anthropic" (config-pin path "default_model" "provider")))
    (is (equal "claude-x" (config-pin path "default_model" "model")) "the default moved as a pair")
    (is (equal "claude-x" nle::*model*))
    ;; Clearing follows the route's contract: pin gone, default kept.
    (is (search "takes the default again"
                (nle:select-model nil :session-id "select-verb")))
    (is-model-pin "select-verb" nil nil "the pin is gone" "on both fields")
    (is (equal "claude-x" (config-pin path "default_model" "model")) "and the default stands")
    ;; No ambient session and none passed refuses rather than guessing.
    (let ((nlk:*scribe-session-id* nil))
      (is (nth-value 1 (ignore-errors (nle:select-model "claude-x")))))))

(defun session-effort (session-id)
  "The reasoning pick SESSION-ID's selection fact carries, or NIL."
  (nlk:session-model-effort session-id))

(deftest select-effort-lands-the-pick-on-its-session-and-the-default ()
  ;; Without the whole model-facing verb in one call, the agent would have no
  ;; verb and spin on the kernel table. The model-facing verb must be the
  ;; whole meaning of a /think pick — the pick checked against its actual
  ;; target's ladder, the frame every attached footer refreshes from, and a
  ;; receipt naming the horizon — in one call, on the ambient session, with no
  ;; shell attached. A pick is the default every new session starts at, as a
  ;; model pick is, and a session that holds its own effort keeps it.
  (with-selection-gateway (path port "effort-verb" :pinned t)
    (nlk:record-session-model-selection "effort-verb" :model "pinned-model")
    (nlk:create-session :id "begun")
    (nlk:record-session-model-selection "begun" :provider "openai-completions" :model "pinned-model"
                                                :effort "low")
    (nlk:create-session :id "fresh")
    (let ((frames
            (frames-while (ws port :subscribe '("fresh"))
              (let* ((nlk:*scribe-session-id* "effort-verb")
                     (answer (nle:select-effort "high")))
                (is-carrying answer "openai-completions/pinned-model: provider default -> high"
                  "the default for new sessions now" "applies from the next turn")))))
      (is-present (frame (find-frame frames "model_changed"))
        "a peer on another session learned the default moved"
        (is (null (nlk:json-value frame :any "payload" "session_id")) "a session-less frame")))
    (is (equal "high" (session-effort "effort-verb")))
    (is-model-pin "effort-verb" nil "pinned-model" "the pick keeps the model pin" "as it was")
    (is (equal "high" nle::*reasoning-effort*) "the image's default effort moved")
    (is (equal "high" (config-pin path "default_model" "effort")) "and the shared config's")
    (is (equal "boot-model" (config-pin path "default_model" "model")) "the default's model unmoved")
    (with-gateway-http (port :get "/api/gateway/target?session_id=fresh")
      (is (equal "high" (gethash "effort" body)) "a session not yet begun starts at it"))
    (with-gateway-http (port :get "/api/gateway/target?session_id=begun")
      (is (equal "low" (gethash "effort" body)) "a session with its own keeps it"))
    (let ((nlk:*scribe-session-id* "effort-verb"))
      ;; Off the ladder refuses before anything moves, and names it.
      (multiple-value-bind (value condition)
          (ignore-errors (nle:select-effort "ultra"))
        (is (null value) "an off-ladder rung refuses")
        (is (and condition
                 (search "the ladder:" (princ-to-string condition)))))
      (is (equal "high" (session-effort "effort-verb")))
      (is (equal "high" (config-pin path "default_model" "effort")) "and moves nothing")
      ;; Default is the model's own reasoning, here and for new sessions.
      (is (search "high -> provider default" (nle:select-effort nil)))
      (is (null (session-effort "effort-verb")))
      (is (null nle::*reasoning-effort*))
      (is (null (config-pin path "default_model" "effort")) "the config's effort is taken out")
      (is (equal "boot-model" (config-pin path "default_model" "model")) "and its pin stays"))
    ;; No ambient session and none passed refuses rather than guessing.
    (let ((nlk:*scribe-session-id* nil))
      (is (nth-value 1 (ignore-errors (nle:select-effort "high")))))))

(deftest a-reasoning-pick-rides-a-model-pick-its-new-model-admits ()
  ;; webfix4 first-03: a model pick keeps the session's reasoning when the new
  ;; model offers the same rung, and drops it when it does not, so the next
  ;; turn never sends a rung its endpoint refuses.
  (let ((nle::*model-capability-overrides*
          '(("rich-model" . (:effort-levels ("low" "high")))
            ("poor-model" . (:effort-levels ("low")))))
        (nle::*models-catalog-path* nil))
    (with-selection-gateway (path port "ride-along" :model "rich-model")
      (nle:select-effort "high" :session-id "ride-along")
      (is (not (search "effort" (uiop:read-file-string path))) "with no default pin the effort writes none")
      (nle:select-model "rich-model" :session-id "ride-along")
      (is (equal "high" (session-effort "ride-along")) "the same ladder keeps the pick")
      (nle:select-model "poor-model" :session-id "ride-along")
      (is (null (session-effort "ride-along")) "a ladder without the rung drops it")
      (nle:select-effort "low" :session-id "ride-along")
      (nle:select-model nil :session-id "ride-along")
      (is (equal "low" (session-effort "ride-along")) "clearing the model pin keeps a pick the default admits"))))

;;; --- the two model pins of the general section --------------------------------

(deftest general-model-pins-seed-the-default-and-the-auxiliary ()
  ;; general.default_model and general.auxiliary_model are two objects of
  ;; one shape; the seed reads both, the default's effort binding at the
  ;; explicit tier, the auxiliary's staying on its own plist.
  (with-model-selection-config
      (path "{~%  \"general\": {~
             ~%    \"default_model\": {\"provider\": \"openai-completions\", \"model\": \"main-x\", \"effort\": \"high\"},~
             ~%    \"auxiliary_model\": {\"provider\": \"openai-completions\", \"model\": \"side-x\", \"effort\": \"off\"}~
             ~%  }~%}~%")
    (setf nle::*provider* "openai-completions"
          nle::*model* "boot-model"
          nle::*auxiliary-model* nil nle::*reasoning-effort* nil)
    (is (nle::restore-model-config) "the default changed the globals")
    (is (equal "main-x" nle::*model*) "the default pin's model")
    (is (equal "openai-completions" nle::*provider*) "and its provider")
    (is (or (equal "high" nle::*reasoning-effort*)
            (uiop:getenv "NODECODE_REASONING_EFFORT")))
    (is (equal '(:provider "openai-completions" :model "side-x" :effort "off")
               nle::*auxiliary-model*))
    (is (null nle::*model-config-refusal*) "a clean section leaves no refusal")
    (is (equal "side-x" (config-pin path "auxiliary_model" "model")))))

(deftest a-written-pin-takes-the-retired-flat-key-with-it ()
  ;; An operator arriving from a build before the pin objects carries
  ;; general.default_provider as a STRING, and that one member refuses the
  ;; whole section. Writing the pin object beside it does not take it away, so
  ;; the refusal survived every write the organism could make and the only way
  ;; out was hand-editing JSON — which is what an operator walked into through
  ;; a Hermes import that had done its own job correctly. The canonical writer
  ;; must not leave a file its canonical reader refuses.
  (with-model-selection-config
      (path "{~%  \"general\": {\"default_provider\": \"anthropic\", \"default_model\": \"claude-x\"}~%}~%")
    (is (eq :written (nle::persist-shared-model-pin "default_model"
                                                    "openai-completions" "new-model")))
    (is (null (config-pin path "default_provider")))
    (is (equal "new-model" (config-pin path "default_model" "model")))
    (is (equal "openai-completions" (config-pin path "default_model" "provider")))
    (handler-bind ((warning #'muffle-warning))
      (nle::restore-model-config))
    (is (null nle::*model-config-refusal*))))

(deftest retired-flat-model-keys-are-refused-by-name ()
  ;; The flat general.default_provider / default_model strings of the Zig
  ;; schema are refused, never read: the globals stand, the refusal names
  ;; the keys and the shape to write, and every shell sees it as the
  ;; snapshot's refusal. A pin missing its lane refuses the same way.
  (with-model-selection-config
      (path "{~%  \"general\": {\"default_provider\": \"anthropic\", \"default_model\": \"claude-x\"}~%}~%")
    (setf nle::*provider* "openai-completions"
          nle::*model* "boot-model"
          nle::*auxiliary-model* '(:provider "openai-completions" :model "stale"))
    (handler-bind ((warning #'muffle-warning))
      (is (null (nle::restore-model-config)) "nothing is seeded"))
    (is (equal "boot-model" nle::*model*) "the model stands")
    (is (equal '(:provider "openai-completions" :model "stale") nle::*auxiliary-model*))
    (is-present (refusal nle::*model-config-refusal*) "the refusal"
      (is (search "default_provider" refusal) "names the retired keys")
      (is (search "{\"provider\"" refusal) "and the shape to write"))
    (is (equal nle::*model-config-refusal*
               (gethash "refusal" (nle::gateway-target-json))))
    (is (equal "anthropic" (config-pin path "default_provider"))))
  (with-model-selection-config
      (path "{~%  \"general\": {\"auxiliary_model\": {\"model\": \"lonely\"}}~%}~%")
    (setf nle::*auxiliary-model* nil)
    (handler-bind ((warning #'muffle-warning))
      (nle::restore-model-config))
    (is (null nle::*auxiliary-model*) "a pin missing its provider seeds nothing")
    (is-present (refusal nle::*model-config-refusal*) "and refuses"
      (is (search "auxiliary_model" refusal) "by its key")
      (is (search "provider" refusal) "naming the missing member"))))

(deftest auxiliary-model-is-the-pin-or-the-session-target-at-its-weakest-rung ()
  ;; AUXILIARY-MODEL answers the configured pin with its effort — the
  ;; configured rung, else the weakest the model declares — and without a
  ;; pin the session's own target (its durable pin over the process
  ;; default) at that weakest rung; WITH-AUXILIARY-MODEL binds the three
  ;; specials RUN binds and drops the endpoint override only for a
  ;; configured target.
  (with-saved-globals (nle::*auxiliary-model*)
    (let ((nle::*model-capability-overrides*
            '(("side-x" . (:effort-levels ("off" "low" "high")))
              ("pinned-x" . (:effort-levels ("minimal" "medium")))))
          (nle::*models-catalog-path* nil))
      (with-gateway-session (port "aux-session")
        (nlk:record-session-model-selection "aux-session" :model "pinned-x")
        (setf nle::*auxiliary-model* '(:provider "openai-completions" :model "side-x"))
        (is-values (provider model effort configured) (nle:auxiliary-model)
          (provider "openai-completions" "the pin's provider") (model "side-x" "the pin's model")
          (effort "off" "no configured effort: the weakest declared rung")
          (configured is "and it says it is configured"))
        (setf nle::*auxiliary-model* '(:provider "openai-completions" :model "side-x" :effort "low"))
        (is (equal "low" (nth-value 2 (nle:auxiliary-model))) "a configured rung wins")
        (let ((nle::*endpoint* "http://override.example/v1"))
          (nle:with-auxiliary-model ()
            (is (equal "side-x" nle::*model*) "the binder binds the model")
            (is (equal "low" nle::*reasoning-effort*) "and the effort")
            (is (null nle::*endpoint*))))
        (setf nle::*auxiliary-model* nil)
        (is-values (provider model effort configured) (nle:auxiliary-model "aux-session")
          (provider nle::*provider* "no pin: the session's lane is the default's")
          (model "pinned-x" "and its model is the session's own pin")
          (effort "minimal" "at the weakest rung that model declares")
          (configured null "and it says so"))
        (setf nle::*auxiliary-model* nil)
        (setf nle::*model-capability-overrides* '())
        (nlk:bind (((_ model effort) (nle:auxiliary-model)))
          (is (equal nle::*model* model) "no pin, no session: the process default")
          (is (null effort) "a model declaring no ladder sends no effort"))
        (let ((nle::*endpoint* "http://override.example/v1"))
          (nle:with-auxiliary-model (:session-id "aux-session")
            (is (equal "pinned-x" nle::*model*) "the fallback binds the session's pin")
            (is (equal "http://override.example/v1" nle::*endpoint*))))))))

(deftest the-auxiliary-pick-lands-in-the-image-and-the-shared-config ()
  ;; /models aux from either shell, and the model-facing form, all land the
  ;; same two facts: this image's auxiliary target and
  ;; general.auxiliary_model; an effort the operator wrote survives the
  ;; merge, an unknown lane is refused before anything moves, no session is
  ;; pinned, and a null model takes the pin out of both.
  (with-saved-globals (nle::*auxiliary-model*)
    (with-selection-gateway
        (path port "owner-shell"
         :text "{~%  \"general\": {\"auxiliary_model\": {\"provider\": \"openai-completions\", \"model\": \"old\", \"effort\": \"off\"}}~%}~%")
      (setf nle::*auxiliary-model* '(:provider "openai-completions" :model "old" :effort "off"))
      (is-present (notice (nle:slash "/models aux anthropic claude-h" :session-id "owner-shell"))
        "the in-process /models aux answers"
        (is (search "auxiliary model: anthropic/claude-h (off)" notice)))
      (is (equal '(:provider "anthropic" :model "claude-h" :effort "off")
                 nle::*auxiliary-model*))
      (is-table (value field) (equal value (config-pin path "auxiliary_model" field))
        ("anthropic" "provider") ("claude-h" "model") ("off" "effort"))
      (is-model-pin "owner-shell" nil nil "no session is pinned" "on either field")
      (is (equal "boot-model" nle::*model*) "and the default did not move")
      ;; The attached client, over HTTP: a model-only pick keeps the
      ;; default's lane.
      (with-gateway-http (port :post "/api/gateway/auxiliary-model"
                          :body "{\"model\":\"claude-h2\"}")
        (is (= 200 status) "the auxiliary-model op accepts the pick")
        (is-shape body ("provider" "openai-completions") ("model" "claude-h2")
          ("shared_config" "written")))
      (is (equal "claude-h2" (config-pin path "auxiliary_model" "model")))
      (is-route (port :post "/api/gateway/auxiliary-model"
                 :body "{\"provider\":\"no-such-lane\",\"model\":\"m\"}") 400
                "an unknown lane is refused" "unknown_provider")
      (is (equal "claude-h2" (getf nle::*auxiliary-model* :model)))
      (with-gateway-http (port :get "/api/gateway/auxiliary-model" :status 200)
        (is (equal "claude-h2" (gethash "model" body)) "GET answers the target"))
      (with-gateway-http (port :get "/api/gateway/model")
        (is (equal "claude-h2" (nlk:json-value body :string "auxiliary" "model"))))
      (is (search "auxiliary model: openai-completions/claude-h2" (nle:slash "/models aux")))
      ;; Its reasoning effort: a rung of its own ladder, which GET answers,
      ;; lands in the image and the file; null gives back its lightest; a rung
      ;; it does not offer is refused before anything moves.
      (with-gateway-http (port :get "/api/gateway/auxiliary-model" :status 200)
        (is (find "high" (gethash "efforts" body) :test #'equal) "GET answers the ladder"))
      (with-gateway-http (port :post "/api/gateway/auxiliary-model"
                          :body "{\"provider\":\"openai-completions\",\"model\":\"claude-h2\",\"effort\":\"high\"}")
        (is (= 200 status))
        (is-shape body ("effort" "high") ("shared_config" "written")))
      (is (equal "high" (getf nle::*auxiliary-model* :effort)))
      (is (equal "high" (config-pin path "auxiliary_model" "effort")))
      (is-route (port :post "/api/gateway/auxiliary-model"
                 :body "{\"provider\":\"openai-completions\",\"model\":\"claude-h2\",\"effort\":\"bogus\"}") 400
                "a rung the model does not offer is refused" "effort_unsupported")
      (is (equal "high" (getf nle::*auxiliary-model* :effort)))
      (with-gateway-http (port :post "/api/gateway/auxiliary-model"
                          :body "{\"provider\":\"openai-completions\",\"model\":\"claude-h2\",\"effort\":null}")
        (is (= 200 status)))
      (is (null (getf nle::*auxiliary-model* :effort)))
      (is (null (config-pin path "auxiliary_model" "effort")))
      ;; Same as main: a null model takes the pin out of the image and the file.
      (with-gateway-http (port :post "/api/gateway/auxiliary-model"
                          :body "{\"provider\":null,\"model\":null}")
        (is (= 200 status) "the auxiliary-model op takes a clear")
        (is-shape body ("model" :null) ("shared_config" "written")))
      (is (null nle::*auxiliary-model*))
      (is (null (config-pin path "auxiliary_model" "model")))
      (is-route (port :post "/api/gateway/auxiliary-model" :body "{}") 400
                "an absent model is no clear" "invalid_request"))))

(deftest gateway-decider-model-pins-general-decider-model ()
  ;; The Decider tab and /models decider land one fact through one route:
  ;; general.decider_model, the {provider, model} pin naming a decider. An id
  ;; no decider has is refused before anything moves; null takes it out.
  (with-selection-gateway (path port "decider-shell")
    (with-gateway-http (port :get "/api/gateway/decider-model" :status 200)
      (is-shape body ("provider" :null) ("model" :null))
      (is (equal '("djev") (map 'list (lambda (row) (gethash "id" row)) (gethash "deciders" body))) "DJev is the decider offered"))
    (with-gateway-http (port :post "/api/gateway/decider-model" :body "{\"provider\":\"djev\"}" :status 200)
      (is-shape body ("provider" "djev") ("model" "dgemma") ("base_url" "http://127.0.0.1:8011")
        ("shared_config" "written")))
    (is-table (value field) (equal value (config-pin path "decider_model" field))
      ("djev" "provider") ("dgemma" "model"))
    (is-route (port :post "/api/gateway/decider-model" :body "{\"provider\":\"nope\"}") 400
              "an id no decider has is refused" "unknown_provider")
    (is (equal "djev" (config-pin path "decider_model" "provider")) "and nothing moved")
    (is (search "decider model: djev/dgemma" (nle:slash "/models decider" :session-id "decider-shell")))
    (is (equal "boot-model" nle::*model*) "no chat model moved")
    (with-gateway-http (port :post "/api/gateway/decider-model" :body "{\"provider\":null}" :status 200)
      (is-shape body ("provider" :null) ("shared_config" "written")))
    (is (null (config-pin path "decider_model" "provider")))
    (is-route (port :post "/api/gateway/decider-model" :body "{}") 400
              "an absent provider is no clear" "invalid_request")))

(deftest gateway-failover-route-reads-and-writes-the-chain ()
  ;; /models' Backup tab lands its picks through one route: failover.chain and
  ;; failover.on, each backup with what history says of it. A half entry or an
  ;; id no trigger has is refused before anything moves; Test asks each
  ;; backup once.
  (with-selection-gateway (path port "failover-shell")
    (with-stubbed-fdefinitions
        ((nle::failover-record (provider model) (list :answered-at nil :tried 2 :rescued 1))
         (nle::probe-model (provider model) (values nil "overloaded" 0.4)))
      (with-gateway-http (port :get "/api/gateway/failover" :status 200)
        (is (zerop (length (gethash "chain" body))) "no chain configured")
        (is (equal '("rate_limit" "outage" "slow_start" "broken_answer" "refusal")
                   (map 'list (lambda (row) (gethash "id" row)) (gethash "triggers" body))) "every trigger, on"))
      (with-gateway-http (port :post "/api/gateway/failover"
                               :body "{\"chain\":[{\"provider\":\"alt\",\"model\":\"m2\"}],\"on\":[\"outage\"]}" :status 200)
        (is (equal "written" (gethash "shared_config" body)))
        (is (equal "m2" (gethash "model" (aref (gethash "chain" body) 0))))
        (is (= 1 (gethash "rescued" (aref (gethash "chain" body) 0))) "the backup carries its record"))
      (let ((nle::*shared-config-memo* nil))
        (is (equal '((:provider "alt" :model "m2")) (nle::failover-chain)))
        (is (equal '("outage") (nle::failover-triggers))))
      (is-route (port :post "/api/gateway/failover" :body "{\"on\":[\"nope\"]}") 400
                "an id no trigger has is refused" "invalid_request")
      (is-route (port :post "/api/gateway/failover" :body "{\"chain\":[{\"provider\":\"alt\"}]}") 400
                "a half entry is refused" "invalid_request")
      (let ((nle::*shared-config-memo* nil))
        (is (equal '("outage") (nle::failover-triggers)) "and nothing moved"))
      (with-gateway-http (port :post "/api/gateway/failover/test" :body "{}" :status 200)
        (let ((result (aref (gethash "results" body) 0)))
          (is (equal "m2" (gethash "model" result)))
          (is (equal "overloaded" (gethash "reason" result)) "each backup says what it answered"))))))

(deftest the-models-route-answers-the-pickers-own-list (with-temp-gateway (port))
  ;; A browser cannot read this home's catalog caches; the route answers the
  ;; list /models draws (MODEL-CHOICES), a failed listing apart.
  (with-stubbed-fdefinitions ((nle::model-choices ()
                                (list (list :provider "p" :model "m" :name "Em" :ctx 1000)
                                      (list :provider "q" :placeholder "401 unauthorized")))
                              (nle::connected-provider-ids () (list "p"))
                              (nle::listable-provider-ids () (list "q" "p"))
                              (nle::selection-refusal (id) (cond ((equal id "q") "no lane")
                                                                 ((equal id "zz") "provider zz is not one this box knows")))
                              ;; the default model's provider, neither connected nor listable
                              (nle::shared-model-pin (&optional key) (declare (ignore key)) "zz")
                              (nle::provider-base-url (id)
                                (if (equal id "p") "http://127.0.0.1:8080/v1" "https://q.example/v1")))
    (with-gateway-http (port :get "/api/gateway/models" :status 200)
      ;; The provider facts a shell's picker reads, a cell's lane
      ;; among them, live only in this image.
      (is (equal '(("p" t t) ("q" nil t) ("zz" nil nil))
                 (map 'list (lambda (row) (list (gethash "id" row) (gethash "connected" row)
                                               (gethash "listable" row)))
                      (gethash "providers" body))) "each provider this image drives, and the pin's")
      ;; Why no turn can run on one, in the gateway's words: a shell's own
      ;; lanes lack every cell's (PROVIDER-TURN-REFUSAL).
      (is (equal '(nil "no lane" "provider zz is not one this box knows")
                 (map 'list (lambda (row) (gethash "refusal" row)) (gethash "providers" body))))
      (is (equal '(("q" . "no lane") ("zz" . "provider zz is not one this box knows"))
                 (getf (nle::gateway-models-facts body) :refusals)) "a provider no turn can run on here says why to the shell")
      (is (equal '("Em") (mapcar #'nle::picker-row-label
                                 (butlast (getf (nle::gateway-models-facts body) :rows)))) "a shell folds the answer into picker rows")
      (is (equal '("p") (getf (nle::gateway-models-facts body) :connected)))
      (is (equal '("p") (getf (nle::gateway-models-facts body) :local)) "a provider this machine serves is Local to the shell too")
      (is-shape (aref (gethash "models" body) 0)
                ((:string "provider") "p") ((:string "model") "m")
                ((:string "name") "Em") ((:integer "context") 1000))
      (is-shape (aref (gethash "unlisted" body) 0)
                ((:string "provider") "q") ((:string "reason") "401 unauthorized")))))

(deftest a-gateway-that-reports-no-providers-says-so-in-the-connect-list ()
  ;; A gateway older than the providers field returns a /api/gateway/models
  ;; answer with no providers field. Without this, its xiaomi key would sit
  ;; intact in auth.json, /models would list xiaomi as `9 models' with nothing
  ;; connected, and the operator would read a wiped key.
  (let ((old (nlk:decode-json "{\"models\":[{\"provider\":\"xiaomi\",\"model\":\"mimo\"}],\"unlisted\":[]}"))
        (new (nlk:decode-json "{\"models\":[{\"provider\":\"xiaomi\",\"model\":\"mimo\"}],\"unlisted\":[],\"providers\":[]}")))
    (is (eq t (getf (nle::gateway-models-facts old) :unreported)) "an answer without the field is unreported")
    (is (null (getf (nle::gateway-models-facts new) :unreported)) "an empty providers field is a report: nothing connected")
    (with-stubbed-fdefinitions ((nle::models-catalog-table () nil))
      (flet ((first-label (body)
               (with-saved-globals ((nle::*serving* nil)
                                    (nle::*gateway-models* (nle::gateway-models-facts body)))
                 (nle::picker-row-label (first (nle::connect-picker-rows))))))
        (is (search "Saved keys unknown: the gateway runs an older build" (first-label old)) "the list leads with what it cannot know")
        (is (equal "Other provider" (first-label new)) "a reporting gateway's list is as it was")))))

(deftest the-model-changed-frame-refreshes-attached-facts (let ((model (detached-shell))))
  ;; The client half: the frame invalidates the cached footer facts and
  ;; kicks the off-loop refetch — consumed by kind before any watermark
  ;; check, and only when the scope is this session or the gateway default.
  (with-stubbed-fdefinition (nle::render-state (m) nil)
    (setf (nle::gateway-model-last-sequence model) 9)
    (setf (nle::gateway-model-model-facts model)
          '(:provider "a6api" :model "glm-5.3-flash"))
    (let ((epoch (nle::gateway-model-facts-epoch model)))
      ;; Another session's move is not this shell's business.
      (feed-wire model (nlk:encode-unsequenced-frame
                        "model_changed"
                        (nlk:json-object "session_id" "other")))
      (is-shape model (nle::gateway-model-model-facts '(:provider "a6api" :model "glm-5.3-flash"))
        (nle::gateway-model-facts-epoch = epoch))
      ;; This session's move stales the cache and advances the epoch.
      (feed-wire model (nlk:encode-unsequenced-frame
                        "model_changed"
                        (nlk:json-object "session_id" "s")))
      (is (eq :unresolved (nle::gateway-model-model-facts model)))
      (is (> (nle::gateway-model-facts-epoch model) epoch)))))

(deftest start-gateway-inherits-capture-kill-switch (with-gateway-scratch (dir "gwks"))
  ;; START-GATEWAY with no explicit policy must not re-arm workspace
  ;; captures: the switch is a process global (SB-EXT:DEFGLOBAL), the suite
  ;; switches it off, and every worker thread reads the same variable — so the
  ;; assertion that matters is made from a FRESH thread, which is exactly
  ;; what a hunchentoot worker is.
  (let ((port (temp-gateway-port)))
    (with-workspace-captures (nil)
      (nlk:open-store :path (merge-pathnames "history.db" dir))
      (nle:start-gateway :port port :operator-token "test-token")
      ;; Let hunchentoot finish binding before the teardown STOP —
      ;; stopping a half-started acceptor trips its shutdown race.
      (sleep 0.25)
      (is (null nlk::*workspace-capture-enabled*))
      (is (null (sb-thread:join-thread
                 (sb-thread:make-thread
                  (lambda () nlk::*workspace-capture-enabled*))))))))

(deftest gateway-models-op-lands-on-the-session (with-temp-gateway (port))
  ;; The gateway shell's /models lands a durable per-session fact, and the
  ;; default new sessions start on (SELECT-SESSION-MODEL).
  (seed-user-inputs "models-op" '("hi"))
  (let ((model (gateway-shell port "models-op")))
    (is (search "m-x — new sessions start on it too"
                (nle::slash-models-op model '("m-x"))))
    (is-model-pin "models-op" nle::*provider* "m-x" "the lane the pick resolved to" "the session fact reads back")
    ;; The op queues an ASYNC cache refresh (a fetch thread posting to the
    ;; program loop — no loop runs in a test); land it synchronously here.
    ;; The durable fact and the footer's read of it are what this gate
    ;; pins, not the thread hop.
    (setf (nle::gateway-model-model-facts model)
          (nle::fetch-gateway-model-facts model))
    (is-values (provider model-id) (nle::footer-identity model) (provider nle::*provider*)
      (model-id "m-x"))
    ;; A fresh attach renders no identity until its worker fetch lands —
    ;; the frame path never dials — and reads the durable fact the moment
    ;; the connect-ack's command answers.
    (let ((fresh (gateway-shell port "models-op")))
      (is (null (nth-value 1 (nle::footer-identity fresh))))
      (setf (nle::gateway-model-model-facts fresh)
            (nle::fetch-gateway-model-facts fresh))
      (is (equal "m-x" (nth-value 1 (nle::footer-identity fresh)))))
    (is (search (format nil "session override: ~a m-x" nle::*provider*)
                (nle::slash-models-op model '())))))

(deftest attached-client-footer-reads-the-gateway-not-its-own-globals (with-temp-gateway (port))
  ;; The reported defect: a second ./nodecode attaches to the running
  ;; organism's gateway, owns no store, and rendered its own unrestored
  ;; compiled defaults in the footer while the owner showed the real model.
  ;; The gateway is the authority for the identity a turn here would bind.
  ;;
  ;; The session pin: the pin overlay is computed once, gateway-side, and rides
  ;; with the facts: an attached client names the pinned target without a store
  ;; of its own, and the effort it shows is the session's own pick, which rides
  ;; a model pick that admits it. A second shell on a session of its own, so the
  ;; first one's facts stay out of it.
  (seed-user-inputs "attach-footer" '("hi"))
  (seed-user-inputs "attach-pin" '("hi"))
  ;; The gateway's process-effective target is a6api-oai/gpt-5.6-sol and this thread's provider
  ;; globals stay at the compiled defaults: the second-TUI shape, reproduced in one image. A
  ;; process that merely attached to someone else's gateway never opened its store and never ran
  ;; RESTORE-GATEWAY-MODEL-CONFIG, so its own globals still read `openai-completions
  ;; deepseek-v4-flash-free` while the gateway serves the real selection. SETF establishes the
  ;; gateway thread's value, the surrounding LET gives this thread its own binding -- SBCL
  ;; specials are per-thread once bound.
  ;;
  ;; a6api-oai is declared under providers: a target naming a provider this
  ;; box does not know is refused by name, the /models pick included. The
  ;; frame path never dials, so each read lands the shell's cache the way its
  ;; worker would (no loop runs in a test).
  (with-model-selection-config
      (path "{\"providers\": {\"a6api-oai\": {\"sdk\": \"openai-completions\", \"base_url\": \"http://127.0.0.1:1/v1\"}}}")
    (with-saved-globals ((nle::*provider* "a6api-oai") (nle::*model* "gpt-5.6-sol"))
      (let ((nle::*provider* "openai-completions") (nle::*model* "deepseek-v4-flash-free"))
        (nlk:record-session-model-selection
         "attach-footer" :effort "high")
        (nlk:record-session-model-selection
         "attach-pin" :effort "low")
        (let ((model (gateway-shell port "attach-footer")))
          (setf (nle::gateway-model-model-facts model)
                (nle::fetch-gateway-model-facts model))
          (is-values (provider model-id) (nle::footer-identity model) (provider "a6api-oai")
            (model-id "gpt-5.6-sol"))
          (is (equal "high" (nle::footer-reasoning-effort model))))
        (let ((model (gateway-shell port "attach-pin")))
          (is (search "a6api-oai gpt-5.6-pin"
                      (nle::slash-models-op model '("gpt-5.6-pin"))))
          (setf (nle::gateway-model-model-facts model)
                (nle::fetch-gateway-model-facts model))
          (is-values (provider model-id) (nle::footer-identity model) (provider "a6api-oai")
            (model-id "gpt-5.6-pin"))
          (is (equal "low" (nle::footer-reasoning-effort model)))
          (is (search "session override: a6api-oai gpt-5.6-pin"
                      (nle::slash-models-op model '()))))))))

;;; --- gate: the target route and the effort pick ----------------------------

(deftest the-target-carries-the-selection-refusal ()
  ;; Without this, a provider named no sdk would come up and say nothing
  ;; until the first turn failed. The target every attached footer reads
  ;; carries why the target cannot serve, in the words that name the key to
  ;; set; a target that resolves carries null.
  (with-temp-shared-config ("{
  \"providers\": {
    \"nosdk\": {\"base_url\": \"https://nosdk.example/v1\", \"api_key\": \"k\"},
    \"fine\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://fine.example/v1\", \"api_key\": \"k\"}
  }
}")
    (let ((nle::*models-catalog-path* nil))
      (let ((nle::*provider* "nosdk") (nle::*model* "m"))
        (is (search "providers.nosdk.sdk"
                    (gethash "refusal" (nle::gateway-target-json)))))
      (let ((nle::*provider* "fine") (nle::*model* "m"))
        (is (eq :null (gethash "refusal" (nle::gateway-target-json))))))))

(deftest attached-shell-surfaces-the-refusal-when-its-facts-land ()
  ;; The client half: the facts that carry a refusal post it as the notice
  ;; once per landing — at attach, and again after the target moved and is
  ;; still refused — never on a refresh of the same refused facts.
  (let ((model (detached-shell))
        (refusal "provider \"nosdk\" names no sdk: set providers.nosdk.sdk"))
    (with-stubbed-fdefinition (nle::render-state (m) nil)
      (flet ((land (facts)
               (nle::install-gateway-model-facts
                model facts (nle::gateway-model-facts-epoch model))))
        (land (list :provider "nosdk" :model "m" :refusal refusal))
        (is (equal refusal (nle::status-said model)))
        (nle::show-status model nil)
        (land (list :provider "nosdk" :model "m" :refusal refusal))
        (is (null (nle::status-said model)))
        (nle::invalidate-gateway-model-facts model)
        (land (list :provider "nosdk" :model "m" :refusal refusal))
        (is (equal refusal (nle::status-said model)))
        (nle::show-status model nil)
        (land (list :provider "fine" :model "m" :refusal nil))
        (is (null (nle::status-said model)))))))

(deftest the-effort-route-lands-a-sessions-pick (with-gateway-session (port "ctl-routes"))
  ;; The default's target, with the ladder it admits and no pick.
  (with-gateway-http (port :get "/api/gateway/target" :status 200 :says "the target answers")
    (is-shape body ("provider" nle::*provider*) ("model" nle::*model*)
      ("effort" eq :null "the default carries no pick"))
    (is (find "high" (gethash "efforts" body) :test #'equal) "beside the ladder it admits"))
  ;; A pick lands on a session: the route lives under the session.
  (is-route (port :post "/api/gateway/sessions/no-such-session/effort" :body "{\"effort\":\"high\"}")
            404 "a pick for no session is refused" "unknown_session")
  (with-gateway-http (port :post "/api/gateway/sessions/ctl-routes/effort" :body "{\"effort\":\"high\"}")
    (is (= 200 status) "a pick answers with the session's target")
    (is-shape body ("effort" "high")))
  (is (equal "high" (session-effort "ctl-routes")) "on the session's own fact")
  (with-gateway-http (port :get "/api/gateway/target?session_id=ctl-routes")
    (is (equal "high" (gethash "effort" body)) "the session's target reads it back"))
  (with-gateway-http (port :get "/api/gateway/target")
    (is (equal "high" (gethash "effort" body)) "and it is the default's effort now"))
  ;; An off-ladder rung and a malformed body are refused, not stored.
  (is-route (port :post "/api/gateway/sessions/ctl-routes/effort" :body "{\"effort\":\"ultra\"}")
            400 "an off-ladder rung is refused" "effort_unsupported")
  (is-route (port :post "/api/gateway/sessions/ctl-routes/effort" :body "{\"effort\":3}")
            400 "an effort is a rung's name" "invalid_request")
  (is (equal "high" (session-effort "ctl-routes")) "and the pick stands")
  ;; The ladder the target declares is the ladder the route admits — every
  ;; rung of it, including the ones above `high` this vocabulary gained for
  ;; adaptive thinking. Which rungs those are is the target's business, so
  ;; the gate reads the offer and spends its strongest rung rather than
  ;; naming one and hoping.
  (let ((strongest
          (with-gateway-http (port :get "/api/gateway/target")
            (let ((efforts (gethash "efforts" body)))
              ;; Weakest to strongest.
              (and (vectorp efforts) (plusp (length efforts)) (alexandria:last-elt efforts))))))
    (is (stringp strongest) "the target offers rungs to pick from")
    (when strongest
      (with-gateway-http (port :post "/api/gateway/sessions/ctl-routes/effort"
                          :body (format nil "{\"effort\":~s}" strongest))
        (is (= 200 status) "every offered rung is admitted")
        (is (equal strongest (gethash "effort" body))))))
  ;; Null returns the session to the model's own posture.
  (with-gateway-http (port :post "/api/gateway/sessions/ctl-routes/effort" :body "{\"effort\":null}")
    (is (= 200 status) "a clear answers")
    (is (eq :null (gethash "effort" body)) "the pick is cleared"))
  (is (null (session-effort "ctl-routes")))
  ;; Auth posture matches every other control op.
  (is-route (port :get "/api/gateway/target" :token nil) 401 "missing token fails closed"))

(deftest the-target-aims-at-a-session-not-yet-opened (with-temp-gateway (port))
  ;; a pick on the web opener waits for the session its first message opens,
  ;; and meanwhile offers the reasoning of the model it aims at, not the
  ;; default's; nothing is pinned or moved.
  (with-saved-globals ((nle::*model-capability-overrides* '(("aimed-model" . (:effort-levels ("low" "max")))))
                       (nle::*models-catalog-path* nil))
    (with-gateway-http (port :get "/api/gateway/target?provider=openai-completions&model=aimed-model")
      (is (= 200 status) "the target answers for the aim")
      (is-shape body ((:string "model") "aimed-model")
        ((:string "default" "model") nle::*model* "the default is named beside it")
        ("pin" eq :null "and nothing is pinned"))
      (is (equal '("off" "low" "max") (coerce (gethash "efforts" body) 'list)) "the aimed model offers Off beside its enabled ladder"))
    (is (not (equal "aimed-model" nle::*model*)) "the default did not move")))

;;; --- gate: tool-metadata stream contract -----------------------------------

(deftest tool-result-metadata-rides-replay (with-temp-gateway (port))
  ;; The image display fact of the Zig turn stream: tool_result metadata
  ;; written once at the durable fact is what BOTH live publication and
  ;; replay project (one encoder, ACCUMULATED-EVENT-FRAME), so the replay
  ;; assertion covers the shared path.
  (ensure-durable-session "meta-replay")
  (let ((metadata (nlk:json-object
                   "image" (nlk:json-object "payload_kind" "base64"
                                            "source" "tool_output"
                                            "provider_executed" :false
                                            "preliminary" :false)))
        (turn (nlk:admit-turn "meta-replay"
                              (nlk::make-durable-id "command")
                              "look at a picture")))
    (record-tool-use turn "c1" "look" "{}" "seen" :metadata metadata)
    (nlk:complete-turn turn nil)
    (let ((frames (subscribed-frames port "meta-replay")))
      (is-present (result (wire-row "turn.tool_result" (replay-frames frames) "event" "type"))
          "the tool result replays"
          (is-present (meta (gethash "metadata" (gethash "payload" (gethash "event" result))))
            "metadata rides the durable payload"
            (let ((image (gethash "image" meta)))
              (is-shape image ("payload_kind" "base64") ("provider_executed" null))))))))

(deftest gateway-image-route-serves-the-file-a-look-names (with-temp-gateway (port))
  ;; A page cannot open the path a LOOK's image fact names, so the route
  ;; serves that file's bytes as the fact's media type -- only a path some
  ;; call's fact names -- and once the file is gone, the PNG a terminal was
  ;; given stands in.
  (uiop:with-temporary-file (:pathname picture :type "jpg")
    (uiop:with-temporary-file (:pathname preview :type "png")
      (alexandria:write-byte-vector-into-file (octets 255 216 255 1) picture :if-exists :supersede)
      (alexandria:write-byte-vector-into-file (octets 137 80 78 71) preview :if-exists :supersede)
      (ensure-durable-session "look-http")
      (with-durable-turn (turn "look-http" :input "look" :complete t)
        (record-tool-use turn "c-look" "eval" "{}" "[image]"
                         :metadata (nlk:json-object
                                    "image" (nlk:json-object "payload_kind" "file" "source" "look"
                                                             "path" (namestring picture)
                                                             "media_type" "image/jpeg"
                                                             "png_path" (namestring preview)))))
      (flet ((fetch (call &optional (token "test-token"))
               (multiple-value-bind (body status headers)
                   (nlk:http :get (format nil "http://127.0.0.1:~d/api/gateway/sessions/look-http/images/~a" port call)
                             :headers (and token (list (cons "authorization" (format nil "Bearer ~a" token))))
                             :binary t)
                 (list status (gethash "content-type" headers) (coerce body 'list)))))
        (is (equal '(200 "image/jpeg" (255 216 255 1)) (fetch "c-look")) "the named file, as its media type")
        (is (= 401 (first (fetch "c-look" nil))) "the operator token is required")
        (is-route (port :get "/api/gateway/sessions/look-http/images/c-none") 404 "a call that names no image" "no_image")
        (delete-file picture)
        (is (equal '(200 "image/png" (137 80 78 71)) (fetch "c-look")) "the file gone, the terminal's PNG")))))

(deftest gateway-image-route-serves-the-picture-an-answer-names (with-temp-gateway (port))
  ;; An answer's picture has no call: its fact rides the message, named by its
  ;; bytes, and the route serves the copy that fact names -- to the session
  ;; whose fact it is, and no other.
  (uiop:with-temporary-file (:pathname copy :type "png")
    (alexandria:write-byte-vector-into-file (octets 137 80 78 71) copy :if-exists :supersede)
    (ensure-durable-session "answer-http")
    (ensure-durable-session "answer-other")
    (with-durable-turn (turn "answer-http" :input "draw" :complete t)
      (nlk:record-assistant-message
       turn (nlk:json-object "role" "assistant" "content" "![c](chart.png)")
       :metadata (nlk:json-object
                  "images" (vector (nlk:json-object "payload_kind" "file" "source" "answer"
                                                    "target" "chart.png" "id" "0a1b" "path" "chart.png"
                                                    "media_type" "image/png" "copy_path" (namestring copy))))))
    (multiple-value-bind (body status headers)
        (gateway-octets port :get "/api/gateway/sessions/answer-http/images/0a1b")
      (is (equal '(200 "image/png" (137 80 78 71)) (list status (gethash "content-type" headers) (coerce body 'list))) "the copy the answer's fact names"))
    (is-route (port :get "/api/gateway/sessions/answer-http/images/0a1c") 404 "an id no fact names" "no_image")
    (is-route (port :get "/api/gateway/sessions/answer-other/images/0a1b") 404 "another session's picture" "no_image")
    (let ((frames (subscribed-frames port "answer-http")))
      (is-present (fact (wire-row "turn.assistant_message_completed" (replay-frames frames) "event" "type")) "the message replays"
        (is (equal '("0a1b") (map 'list (lambda (image) (gethash "id" image)) (nlk:json-array fact "event" "payload" "metadata" "images"))) "its picture's fact with it")))))

(deftest gateway-slash-routes-list-run-and-complete-the-channels-catalog (with-temp-gateway (port))
  ;; A client with no image of its own reads the catalog a channel publishes,
  ;; runs a line the way a channel does, and asks what an argument completes to.
  (ensure-durable-session "slash-http")
  (with-gateway-http (port :get "/api/gateway/slash")
    (let* ((commands (coerce (gethash "commands" body) 'list))
           (names (mapcar (lambda (entry) (gethash "name" entry)) commands))
           (models (find "models" commands :key (lambda (entry) (gethash "name" entry)) :test #'equal)))
      (is (= 200 status))
      (is (equal (mapcar (lambda (entry) (getf entry :name)) (nle:slash-catalog)) names) "SLASH-CATALOG, in its order")
      (is (plusp (length (gethash "description" (first commands)))) "each with what it does")
      (is (eq t (gethash "autocomplete" models)) "/models completes its argument")))
  (is-route (port :get "/api/gateway/slash" :token nil) 401 "the operator token is required")
  (with-gateway-http (port :post "/api/gateway/slash" :body "{\"line\":\"/help\"}")
    (is (equal (nle:slash "/help") (gethash "text" body)) "/help needs no session"))
  (with-gateway-http (port :post "/api/gateway/slash" :body "{\"line\":\"/nope\",\"session_id\":\"slash-http\"}")
    (is (search "unknown command /nope" (gethash "text" body)) "an unknown name says so"))
  (with-gateway-http (port :post "/api/gateway/slash" :body "{\"line\":\"/context\",\"session_id\":\"slash-http\"}")
    (is (= 200 status) "a session's command runs against it")
    (is (equal (nle:slash "/context" :session-id "slash-http") (gethash "text" body))))
  (is-route (port :post "/api/gateway/slash" :body "{\"line\":\"/context\"}") 400
            "a session's command with none refuses" "invalid_request")
  (is-route (port :post "/api/gateway/slash" :body "{}") 400 "a line is required" "invalid_request")
  (with-gateway-http (port :post "/api/gateway/slash/completions" :body "{\"name\":\"help\",\"text\":\"\"}")
    (is (equalp #() (gethash "choices" body)) "a command that completes nothing offers nothing"))
  (with-gateway-http (port :post "/api/gateway/slash/completions" :body "{\"name\":\"models\",\"text\":\"\"}")
    (is (= (length (nle:slash-completions "models" "")) (length (gethash "choices" body))) "/models offers the catalog"))
  (is-route (port :post "/api/gateway/slash/completions" :body "{}") 400 "a name is required" "invalid_request"))

(deftest gateway-slash-route-names-sessionless-commands-and-carries-their-panel (with-temp-gateway (port))
  ;; The web page's opener runs a command that acts on no session as it is,
  ;; rather than opening an untitled session for it: the catalog says which,
  ;; builtin or a cell's. A command that answers with a panel (LIST-DIALOG)
  ;; hands it to the page with its line, as a shell opens it.
  (nlk:with-cleanup ((nle:unregister-commands "t-panel"))
    (nle:register-command "t-panel" "t-panel"
                          (lambda (args session-id)
                            (values (format nil "panel ~a~@[ in ~a~]" args session-id)
                                    (nle:list-dialog :title "Panel" :context "on"
                                                     :rows (list (list :label "one" :detail "the first"
                                                                       :value "/t-panel one")))))
                          :session nil)
    (with-gateway-http (port :get "/api/gateway/slash")
      (flet ((session (name)
               (gethash "session" (wire-row name (gethash "commands" body) "name"))))
        (is (eq t (session "models")) "/models moves a session's model")
        (is (null (session "help")) "/help acts on no session")
        (is (null (session "t-panel")) "nor does a command registered :session nil")))
    (with-gateway-http (port :post "/api/gateway/slash" :body "{\"line\":\"/t-panel two\"}")
      (is (equal "panel two" (gethash "text" body)) "run with no session")
      (is-present (dialog (nlk:json-value body :object "dialog")) "and its panel"
        (is (equal "on" (gethash "context" dialog)))
        (is (equal "/t-panel one" (gethash "value" (aref (gethash "rows" dialog) 0))))))
    (with-gateway-http (port :post "/api/gateway/slash" :body "{\"line\":\"/help\"}")
      (is (null (nlk:json-value body :any "dialog")) "a line alone carries no panel"))))

;;; --- gate: reconnect endpoint rediscovery ----------------------------------

(deftest reconnect-rediscovers-endpoint-from-lease-sidecar ()
  ;; The self-heal ladder's rediscovery half: after a sustained outage the
  ;; client re-resolves the gateway from the store's holder sidecar — the
  ;; same {pid, port} authority ATTACH-OR-OWN-ORGANISM boots from.
  (with-temp-directory (dir "heal" :close-store t)
    (let ((store-path (merge-pathnames "history.db" dir)))
      (nlk:open-store :path store-path)
      (nlk::publish-store-endpoint 45999)
      (is (= 45999 (nle::rediscover-gateway-endpoint store-path 42691)))
      (is (null (nle::rediscover-gateway-endpoint store-path 45999)))
      (nlk::publish-store-endpoint nil)
      (is (null (nle::rediscover-gateway-endpoint store-path 42691))))))

;;; --- gate: dirty-death resurrection -----------------------------------------
;;; The self-heal ladder's respawn half: a dirty owner death (lock freed with
;;; the dead process, sidecar left advertising it) licenses spawning one
;;; detached headless serve candidate; the fcntl lease inside OPEN-STORE is
;;; the whole election, so racing candidates need no further coordination.

(deftest resurrection-fires-only-on-a-paced-dirty-death ()
  ;; The pure gate the reconnect ladder consults before spawning: dirty
  ;; death only, past the attempt threshold and the grace window, inside
  ;; the budget, with the previous candidate's boot window honored. The
  ;; tunables are bound so the matrix locks the gate's shape, not the
  ;; shipped defaults.
  (let ((nle::*sync-resurrect-after-attempts* 3)
        (nle::*sync-resurrect-grace-seconds* 3.0)
        (nle::*sync-resurrect-rearm-seconds* 30.0)
        (nle::*sync-resurrect-budget* 3))
    (flet ((due (verdict failures elapsed attempts last-spawn)
             (nle::resurrection-due-p verdict :failures failures :elapsed elapsed
                                              :attempts attempts :last-spawn-elapsed last-spawn)))
      (is (due :dirty-death 3 3.0 0 nil))
      (is-table (verdict failures elapsed attempts last-spawn)
        (not (due verdict failures elapsed attempts last-spawn))
        (:live 9 99.0 0 nil "a live owner is never resurrected")
        (:clean-shutdown 9 99.0 0 nil "a clean shutdown is never resurrected")
        (:dirty-death 2 99.0 0 nil "below the attempt threshold nothing spawns")
        (:dirty-death 9 1.0 0 nil "the grace window is honored")
        (:dirty-death 9 99.0 3 99.0 "the spawn budget is a hard stop")
        (:dirty-death 9 99.0 1 5.0 "a booting candidate is given its window"))
      (is (due :dirty-death 9 99.0 1 31.0))))
  ;; The shipped defaults are the recovery-latency contract: one failed lap
  ;; over a confirmed dirty death spawns immediately — the verdict is the
  ;; confirmation (a dirty death is never a blip, and clean teardown makes
  ;; lock-free-with-sidecar unobservable), so total heal stays under ~2s.
  (is (nle::resurrection-due-p :dirty-death :failures 1 :elapsed 0.0
                               :attempts 0 :last-spawn-elapsed nil)))

(deftest serve-command-is-binary-only ()
  ;; A dev image runs under a bare sbcl runtime that cannot replay this
  ;; organism from an argv, so a serve derives its program from the
  ;; dumped executable only — or from the explicit override tests inject.
  (let ((nle::*serve-program* nil))
    (is (null (nle::serve-command #p"/tmp/x/history.db"))))
  (let ((nle::*serve-program* (list "/opt/nodecode")))
    (is (equal (list "/opt/nodecode" "serve" "--store" "/tmp/x/history.db")
               (nle::serve-command #p"/tmp/x/history.db")))))

;;; --- the organism a shell fronts ---------------------------------------------

(deftest a-fronted-organism-that-dies-at-boot-says-its-own-line (with-temp-directory (dir "front-dies"))
  ;; The organism runs in a process of its own, so the reason a boot died is
  ;; the last line it wrote to the store's log -- MAIN's `nodecode: ' line --
  ;; and the shell's line is that reason, in the organism's own words.
  (let ((nle::*serve-program*
          (list "sh" "-c" "echo 'nodecode: the store is not a database; move it aside' >&2; exit 1"
                "front-stub"))
        (nle::*front* nil))
    (let ((condition (signals-error nle::organism-exited
                       (nle::front-organism (merge-pathnames "history.db" dir)))))
      ;; The organism's words are the shell's line.
      (is (equal '("the store is not a database; move it aside" t)
                 (multiple-value-list (nle::plain-error-text condition)))))
    (is (null nle::*front*) "a boot that died is no organism the shell fronts")))

(deftest a-shell-that-quits-hands-its-organism-the-reason (with-temp-directory (dir "front-quits"))
  ;; The pipe on the organism's stdin is how it lives with the shell: the
  ;; shell writes why it lets go, closes it, and waits for the organism to go.
  (let* ((marker (merge-pathnames "why.txt" dir))
         (nle::*front* nil))
    (multiple-value-bind (pid out err process pipe)
        (nlk::spawn-program (list "sh" "-c" (format nil "read why; echo \"$why\" > ~a" (namestring marker)))
                            :output nil :error-output nil :input :stream)
      (declare (ignore out err))
      (setf nle::*front* (list :pipe pipe :waiter (nlk:spawn "front-test-waiter"
                                                    (nlk::wait-for-child pid process)))))
    (nle::stop-front :why "quit")
    (is (null nle::*front*))
    ;; The organism read the reason before the shell stopped waiting.
    (is (equal "quit" (string-trim '(#\Newline) (uiop:read-file-string marker))))))

(deftest a-fronted-organism-stops-when-its-shell-lets-go ()
  ;; The organism's side: the line the shell wrote is the stop's reason, and
  ;; the end of the pipe, a word or none, stops it.
  (with-saved-globals ((nle::*serve-next* nil) (nle::*stop-reason* nil) (nle::*fronted* nil))
    (nlk::set-environment-variable nle::+front-variable+ "1")
    (nlk:with-cleanup ((sb-posix:unsetenv nle::+front-variable+))
      (nle::watch-front (make-string-input-stream (format nil "quit~%")))
      (is-eventually (eq :stop nle::*serve-next*) "the organism is told to let go")
      (is (equal "quit" nle::*stop-reason*) "keeping why, for the next boot's note")
      (is (eq t nle::*fronted*)))
    (bt2:wait-on-semaphore nle::*serve-wake* :timeout 0)))

(deftest resurrection-spawns-a-detached-candidate (with-temp-directory (dir "resurrect-spawn"))
  ;; SPAWN-RESURRECTION-CANDIDATE detaches the candidate through setsid and
  ;; hands it exactly the serve argv. The stub records what it received;
  ;; polling bounds the detached race.
  (let* ((path (merge-pathnames "history.db" dir))
         (marker (merge-pathnames "spawned.txt" dir))
         (nle::*serve-program*
           (list "sh" "-c" (format nil "printf '%s ' \"$@\" > ~a"
                                   (namestring marker))
                 "resurrect-stub")))
    (is (nle::spawn-resurrection-candidate path))
    (is-eventually (probe-file marker) "the detached candidate ran")
    (let ((argv (uiop:read-file-string marker)))
      (is (search "serve" argv) "the candidate is a serve entry")
      (is (search (namestring path) argv)))))

(deftest serve-candidate-loses-a-held-election-quietly ()
  ;; The probe economy: a candidate that sees a live owner returns :LOST
  ;; before the heredity replay, store untouched. The python holder is the
  ;; same foreign-process fixture the store lease tests use — fcntl locks
  ;; are per-process, so only a real second process can hold one against us.
  (with-temp-directory (dir "resurrect-election")
    (let ((path (merge-pathnames "history.db" dir)))
      (with-lock-holder ((nlk::lock-path-for-store path))
        (is (eq :lost (nle:serve-organism-candidate :store-path path)))
        (is (not (probe-file (nlk::lease-path-for-store path))))
        (is (not (probe-file path)))))))

;;; --- start-confirmed acceptance latch ----------------------------------------
;;; The latch itself is GATEWAY-BINDS-KERNEL-ASSIGNED-PORT-AND-PUBLISHES-IT's.

(deftest await-port-answers-refusal-in-milliseconds ()
  ;; The raw poll is what keeps the latch off the boot clock: a refused
  ;; connect answers NIL in microseconds per lap, where the HTTP probe it
  ;; replaced burned a 1 s connect timeout on the same refusal.
  (let ((started (get-internal-real-time)))
    (is (not (nle:await-port (temp-gateway-port) :attempts 5)))
    (is (< (- (get-internal-real-time) started)
           (/ internal-time-units-per-second 2))))
  (let ((listener (usocket:socket-listen "127.0.0.1" 0)))
    (nlk:with-cleanup ((usocket:socket-close listener))
      (is (nle:await-port (usocket:get-local-port listener) :attempts 5)))))

(deftest gateway-http-refuses-without-an-endpoint ()
  ;; Paint-first: the shell is on screen before any gateway exists, so a
  ;; control call from that window must refuse structurally — the gateway
  ;; is unreachable — never dial a malformed URL.
  (let ((err (signals-error nle::gateway-http-error
               (nle::gateway-http-session-request
                nil :get "/api/gateway/identity" nil nil))))
    (is (equal "gateway_unreachable" (nle::gateway-http-error-code err)))))

(deftest organism-boot-death-keeps-its-reason-in-plain-words
    (let ((program (nlt:make-program (detached-shell "s")))))
  ;; The organism boots behind the painted shell. A boot that dies keeps one
  ;; line for the launch to exit on and the setup walk's wait to end on. It
  ;; says why in words written for the operator, and points at the store's
  ;; log only for a condition with none (`The slot ... is unbound in the
  ;; object #<SYSTEM ...>'). The boot's thread leaves a shell
  ;; that never attached to its own reconnector, which ends it
  ;; (A-DEAD-FIRST-BOOT-ENDS-ITS-SHELL-THROUGH-ITS-OWN-QUIT).
  (setf (nlt:program-running program) t)
  (with-temp-directory (dir "boot-death")
    (let ((store (merge-pathnames "history.db" dir))
          (blocked (merge-pathnames "a-file/history.db" dir))
          (nle::*organism-boot-failure* nil))
      (flet ((die-of (condition store)
               (nle::call-organism-boot (lambda (prog) (declare (ignore prog)) (error condition))
                                        (lambda () program) store)
               ;; nothing posted to the shell from the boot's thread
               (is (null (nlt::mailbox-drain (nlt::program-mailbox program) :timeout 0)))
               nle::*organism-boot-failure*))
        ;; no words of its own: the condition is the log's, and the line names where
        (is-carrying (line (die-of (make-condition 'simple-error :format-control "the store is a lie") store))
          "could not start" "history.db.stdio.log" (:absent "the store is a lie"))
        (is (search "the store is a lie" (uiop:read-file-string (nle::stdio-log-path store))))
        ;; The store's own sentence is the line, never behind the log.
        (let ((unreadable (make-condition 'nlk::store-unreadable
                                          :path (namestring (merge-pathnames ".nodecode/history.db"
                                                                             (user-homedir-pathname)))
                                          :reason "database disk image is malformed")))
          (is-carrying (line (die-of unreadable store))
            (:= "could not start: ~/.nodecode/history.db is not a nodecode store (database disk image is malformed); move it aside and run nodecode again"))
          ;; A log that cannot be written -- its folder is a file -- takes nothing from it.
          (write-temp-file (merge-pathnames "a-file" dir) "")
          (is-carrying (line (die-of unreadable blocked))
            (:= "could not start: ~/.nodecode/history.db is not a nodecode store (database disk image is malformed); move it aside and run nodecode again")))
        ;; With no words and no log, the line says the log refused, in the system's words.
        (is-carrying (line (die-of (make-condition 'simple-error :format-control "the store is a lie") blocked))
          "could not start" "could not be written to" "(not a directory)"
          (:absent "the store is a lie") (:absent "the reason is in"))))))

(deftest a-takeover-that-dies-leaves-its-shell-up-and-says-so (let ((seen '())))
  ;; A shell that attached served a session; when it takes over an organism
  ;; whose owner let the store go and that boot dies, it stays up, is told on
  ;; its own words, and never reads `could not start' of a nodecode that
  ;; started long ago.
  (let* ((model (detached-shell "s"))
         (program (nlt:make-program model))
         (nle::*organism-boot-failure* nil)
         (nle::*system-notice-hook* (nlk:lambda-ignoring (text error-p)
                                      (push text seen))))
    (setf (nle::gateway-model-ws model) :served)
    (with-temp-directory (dir "takeover-death")
      (nle::call-organism-boot
       (lambda (prog) (declare (ignore prog))
         (error 'nlk::store-unreadable
                :path (namestring (merge-pathnames ".nodecode/history.db" (user-homedir-pathname)))
                :reason "database disk image is malformed"))
       (lambda () program) (merge-pathnames "history.db" dir)))
    (is-carrying (said (first seen))
      (:= "could not take the store over: ~/.nodecode/history.db is not a nodecode store (database disk image is malformed); move it aside and run nodecode again"))
    (is (equal (first seen) nle::*organism-boot-failure*))
    (is (notany #'nlt:quit-msg-p (nlt::mailbox-drain (nlt::program-mailbox program) :timeout 0)))))

(deftest a-dead-first-boot-ends-its-shell-through-its-own-quit ()
  ;; The shell's own reconnector ends a shell whose organism died at boot
  ;; before it ever attached, by the quit its loop reads -- what /exit sends
  ;; -- however late that loop starts: a quit sent once before it ran was
  ;; dropped, and the shell stayed up over a dead organism. A shell that never
  ;; attached has no session to resume.
  (with-temp-directory (dir "dead-first-boot")
    (with-saved-globals ((nle::*organism-boot-failure* nil))
      (let ((quit nil))
        (with-stubbed-fdefinition (nlt:run (program)
                                    (sleep 0.2)
                                    (setf (nlt:program-running program) t)
                                    (setf quit (loop repeat 200
                                                     thereis (some #'nlt:quit-msg-p
                                                                   (nlt::mailbox-drain
                                                                    (nlt::program-mailbox program)
                                                                    :timeout 0.05))))
                                    (setf (nlt:program-running program) nil))
          ;; nothing to resume
          (is (null (nle:run-chat-gateway :session-id "s-dead"
                                          :store-path (merge-pathnames "history.db" dir)
                                          :boot (nlk:lambda-ignoring (shell)
                                                  (error "the store is a lie"))))))
        (is quit "the shell's loop read a quit")
        (is (search "could not start" nle::*organism-boot-failure*))))))

(deftest the-folder-is-decided-once-however-often-the-organism-boots (let ((decided 0)))
  ;; The takeover rung boots the launch's organism again, in a process whose
  ;; folder the first boot already decided: deciding it again handed a shell
  ;; that had served for an hour to a fresh warm image from behind its frame.
  (with-stubbed-fdefinitions
      ((nle::boot-owned-organism (&rest args) (error "no store for this test"))
       (nle:run-chat-gateway (&key boot &allow-other-keys)
         (dotimes (lap 2)
           (ignore-errors (funcall boot (lambda () nil))))
         nil))
    (nle::attach-or-own-organism :session-id "s-twice" :prelude (lambda () (incf decided))))
  (is (= 1 decided)))

(deftest a-dead-boot-refuses-the-launch-on-its-line ()
  ;; The shell a dead boot ended leaves its launch to end as a refused one
  ;; does: MAIN prints `nodecode: ' and the line on the error this answers,
  ;; and exits 1. A shell that never attached (RUN-CHAT-GATEWAY answers NIL)
  ;; prints no resume line, which named a session nothing ever served; one
  ;; that attached prints it whatever a later takeover's boot did.
  (flet ((launch (failure ended &aux (nle::*organism-boot-failure* nil) (text nil))
           (with-stubbed-fdefinition (nle:run-chat-gateway (&rest args)
                                       (setf nle::*organism-boot-failure* failure)
                                       ended)
             (list (with-output-to-string (*standard-output*)
                     (setf text (handler-case (progn (nle::attach-or-own-organism :session-id "s-x")
                                                     nil)
                                  (error (e) (princ-to-string e)))))
                   text))))
    (destructuring-bind (out text) (launch "could not start: the store is a lie" nil)
      (is (equal "could not start: the store is a lie" text))
      (is (not (search "To resume" out))))
    (destructuring-bind (out text) (launch "could not take the store over: why" "s-served")
      (is (null text) "the launch ends as an /exit does")
      (is (search "To resume: nodecode resume s-served" out)))
    (destructuring-bind (out text) (launch nil nil)
      (is (null text))
      (is (equal "" out) "a shell that never attached names nothing to resume"))))

(deftest sidecar-endpoint-answers-only-for-a-live-publisher ()
  ;; A dead owner's sidecar is leavings, not an endpoint: seeding a dial
  ;; from one would wedge the launch.
  (with-temp-directory (dir "sidecar-live")
    (let ((store-path (merge-pathnames "history.db" dir)))
      (flet ((hold (pid &aux (lease (nlk::lease-path-for-store store-path)))
               (nlk::write-store-holder lease :pid pid :port 45999)))
        (is (null (nle::live-published-endpoint store-path)))
        (hold 999999999)
        (is (null (nle::live-published-endpoint store-path)))
        ;; Nor while THIS process holds the lease: OPEN-STORE takes the lock a
        ;; beat before it rewrites the sidecar, and the lock is then its own.
        (let ((lock (nlk::lock-file (nlk::lock-path-for-store store-path))))
          (nlk:with-cleanup ((nlk::unlock-file lock))
            (is (null (nle::live-published-endpoint store-path)))))
        (hold (sb-posix:getpid))
        (is (eql 45999 (nle::live-published-endpoint store-path)))
        ;; Another process's lock vouches for a pid that is alive: an organism
        ;; that took the lock and has not yet rewritten a dead owner's sidecar
        ;; sent a shell dialing that owner's port.
        (with-lock-holder ((nlk::lock-path-for-store store-path))
          (hold 999999999)
          (is (null (nle::live-published-endpoint store-path)) "a dead owner's sidecar under a new holder's lock")
          (hold (sb-posix:getppid))
          (is (eql 45999 (nle::live-published-endpoint store-path)) "a live pid's"))))))

(deftest a-stop-that-runs-long-is-cut-short (let ((cut 0)))
  ;; A serve's stop has *STOP-SECONDS*: past it the process ends at once
  ;; (CUT-SHORT), the kernel freeing the store's lock, where an exit that
  ;; waited on a thread it could not stop held the store 60 s. Armed only in
  ;; a serve: this image, which stops gateways all day, never.
  (with-saved-globals ((nle::*serving* nil) (nle::*stopping* nil) (nle::*serve-next* nil)
                       (nle::*stop-seconds* 0.05))
    (with-stubbed-fdefinition (nle::cut-short () (incf cut))
      (nle::serve-next :stop)
      (sleep 0.3)
      (is (zerop cut) "outside a serve, a stop is never cut short")
      (setf nle::*serving* t)
      (nle::serve-next :restart)
      (sleep 0.3)
      (is (zerop cut) "a restart has no deadline")
      (nle::serve-next :stop)
      (is-eventually (= 1 cut) "a serve's stop is cut short past its seconds")
      (nle::serve-next :stop)
      (sleep 0.3)
      (is (= 1 cut) "once"))
    (loop while (bt2:wait-on-semaphore nle::*serve-wake* :timeout 0))))

(deftest a-stopping-gateway-advertises-no-endpoint-first (with-temp-gateway (port))
  ;; The sidecar loses its port before the peripherals stop: a shell that
  ;; launches meanwhile waits for the store instead of attaching to an
  ;; organism on its way out.
  (let ((advertised :unread))
    (with-stubbed-fdefinition (nle::stop-cells (&optional names)
                                (setf advertised (nth-value 1 (nlk::read-store-holder
                                                               (nlk::store-path nlk::*store*)))))
      (is (eql port (nth-value 1 (nlk::read-store-holder (nlk::store-path nlk::*store*)))))
      (nle:stop-gateway))
    (is (null advertised) "no port while the cells stop")))

;;; --- /sessions transcript preview --------------------------------------------

(defun sessions-dialog-highlight (model session-id &aux (dialog model.dialog))
  "Land the /sessions highlight on SESSION-ID through the shared selection
seam — exactly what a key, a search edit, and a hover all reach."
  (setf dialog.selection
        (position session-id (nle::picker-visible-rows dialog)
                  :key #'nle::picker-row-value :test #'equal))
  (nle::picker-notify-highlight model dialog)
  ;; Selection moves stage the highlight; deliver it the way the frame
  ;; clock's tick would (the enqueue contract).
  (nle::picker-flush-highlight model))

(defun transcript-carries-p (state text)
  (find text (nle::transcript-rows state)
        :key #'nle::transcript-row-text :test #'equal))

(deftest session-picker-rows-draw-the-fork-lane-and-the-fold ()
  ;; The sessions dialog's rows: the listing's fork tree re-laid with the
  ;; dialog's fold — a collapsed parent hides its branch whole and names the
  ;; forks it holds at the right of its row — and the /tree picker's own
  ;; glyphs (TREE-LANE-PREFIX) describe only the rows actually drawn.
  (flet ((entry (id parent stamp title)
           (nlk::make-json-object "id" id "title" title
                                  "parent_id" parent
                                  "updated_at" stamp
                                  "working_directory" "~/work"
                                  "message_count" 2)))
    (let* ((entries (list (entry "s-a" nil "2026-01-01T00:00:00.000Z" "root")
                          (entry "s-b" "s-a" "2026-01-02T00:00:00.000Z" "branch work")
                          (entry "s-c" "s-b" "2026-01-05T00:00:00.000Z" "deep fork")
                          (entry "s-d" "s-a" "2026-01-04T00:00:00.000Z" "sibling")))
           (now (nle::iso-8601-ms "2026-01-01T00:01:00.000Z")))
      (let ((children (nth-value 1 (nle::session-picker-index entries))))
        (flet ((rows (open &aux (table (make-hash-table :test #'equal)))
                 (dolist (id open) (setf (gethash id table) t))
                 (remove-if #'nle::picker-row-heading
                            (nle::session-picker-rows entries children table now)))
               (labels-of (rows) (mapcar #'nle::picker-row-label rows)))
          (is (equal '("root") (labels-of (rows '()))))
          (is (search "2 forks" (session-row-text (first (rows '())))))
          (is (equal '("root" "├ branch work" "╰ sibling")
                     (labels-of (rows '("s-a")))))
          (is (search "1 fork " (session-row-text (second (rows '("s-a"))))))
          (is (equal '("root" "├ branch work" "│ ╰ deep fork" "╰ sibling")
                     (labels-of (rows '("s-a" "s-b")))))
          (is (equal '("root" "├ branch work" "│ ╰ deep fork" "╰ sibling")
                     (labels-of (rows '("s-a" "s-b" "s-c"))))))))))

(deftest leave-opens-the-session-picker-on-the-session-left (with-temp-gateway (port))
  ;; Left on an empty composer creates the shell's next session without ending the one left,
  ;; then draws the real gateway directory with the old session selected.
  (nlk:create-session :id "leave-old" :cwd (nle::workspace-directory))
  (let ((model (gateway-shell port "leave-old" :ui t)))
    (nlt:update model (nlt:make-key-press-msg :code :left))
    (is (not (equal "leave-old" model.session-id)) "the shell moves to a fresh session")
    (is (nlk:find-session "leave-old") "the session left still exists")
    (is-present (dialog model.dialog) "leave opens the session picker"
      (is (eq :sessions (nle::picker-dialog-kind dialog)))
      (is (equal "leave-old" (nle::picker-highlighted-value dialog)) "the session left is selected")
      (is (equal '("leave-old") (session-labels dialog))))
    (is (null (nle::status-said model)))))

(deftest ctrl-s-opens-the-sessions-picker-through-the-keyboard (with-temp-gateway (port))
  ;; The keyboard door to the picker: the Zig open_session_launcher chord,
  ;; driven through the real update ladder on a gateway shell, opens the
  ;; picker on the gateway's own listing — the sessions opened in this
  ;; shell's folder, Tab reaching every folder's — and the same chord closes
  ;; it again, the toggle the history panel taught.
  (nlk:create-session :id "mom" :cwd (nle::workspace-directory)
                      :occurred-at "2026-01-01T00:00:00.000Z")
  (nlk:create-session :id "away" :cwd "/elsewhere/"
                      :occurred-at "2025-12-31T00:00:00.000Z")
  (let ((model (gateway-shell port "mom" :ui t)))
    (nlt:update model (nlt:make-key-press-msg :code #\s :mod nlt:+mod-ctrl+))
    (is-present (dialog (nle::chat-model-dialog model)) "Ctrl+S opens the picker"
      (is-shape dialog (nle::picker-dialog-title "") (nle::picker-dialog-kind eq :sessions)
        (session-labels '("mom")))
      (nlt:update model (nlt:make-key-press-msg :code :tab))
      (is (equal '("mom" "away") (session-labels dialog)) "Tab lists every folder's sessions"))
    (is (null (nle::status-said model)))
    (nlt:update model (nlt:make-key-press-msg :code #\s :mod nlt:+mod-ctrl+))
    (is (null model.dialog) "and Ctrl+S again closes it")))

(deftest the-sessions-picker-paints-before-its-directory-lands (with-temp-gateway (port))
  ;; Ctrl+S on a shell with a live program opens the picker on the keystroke
  ;; and lets the gateway's answer fill the dialog already on screen: the
  ;; felt cost is the frame, never the round trip. Both halves are asserted,
  ;; because the second is the one a stale dialog would quietly lose.
  (nlk:create-session :id "mom" :cwd (nle::workspace-directory)
                      :occurred-at "2026-01-01T00:00:00.000Z")
  (let ((model (gateway-shell port "mom" :ui t)))
    ;; What SLASH-SESSIONS-OP does with a live program: open first, read after.
    (nle::open-sessions-picker-now model)
    (let ((dialog model.dialog))
      (is-present dialog "the picker is up before the read lands" nil)
      (is-shape dialog (.title "") (.rows null "with no rows yet")
        (.empty-label "Reading the sessions…"))
      (is (null (nle::refresh-sessions-dialog model :tick t)))
      ;; The read lands.
      (nle::install-session-directory model (nle::read-session-directory model))
      (is (eq dialog model.dialog))
      (is-shape dialog (session-labels '("mom")) (.empty-label nle::+sessions-empty-label+))
      (is (null (getf (gethash dialog nle::*session-search*) :awaiting)))
      ;; And the cache that read left behind: the NEXT keystroke opens on it,
      ;; so the second /sessions in a shell shows rows at once.
      (nlt:update model (nlt:make-key-press-msg :code #\s :mod nlt:+mod-ctrl+))
      (is (null model.dialog) "the chord closes the picker")
      (nle::open-sessions-picker-now model)
      (let ((second model.dialog))
        (is-shape second (session-labels '("mom")) (.empty-label nle::+sessions-empty-label+))))))

(deftest sessions-dialog-folds-forks-under-the-parent (with-temp-gateway (port))
  ;; The /sessions door state and its two gestures on a real listing: a
  ;; parent opens and closes its forks under Right and Left, the count rides
  ;; the collapsed row, the cursor parks where the gesture left the tree,
  ;; and the fold dies with the dialog — every reopening starts folded.
  (dolist (spec '(("mom" nil "2026-01-01T00:00:00.000Z")
                  ("kid-a" "mom" "2026-01-02T00:00:00.000Z")
                  ("kid-b" "mom" "2026-01-03T00:00:00.000Z")
                  ("grand" "kid-a" "2026-01-04T00:00:00.000Z")
                  ("uncle" nil "2025-12-31T00:00:00.000Z")))
    (destructuring-bind (id parent stamp) spec
      (nlk:create-session :id id :cwd (nle::workspace-directory) :parent parent
                          :occurred-at stamp)))
  ;; The bandless gateway shell the other gates use handles folds fine —
  ;; the preview lane's repaint does not: pin / render want a band, so
  ;; this gate initializes the shell's UI the way the hydrate gate does.
  (let ((model (gateway-shell port "mom" :ui t)))
    (nle::slash-sessions-op model)
    (let ((dialog model.dialog))
      (labels ((rows () (nle::picker-visible-rows dialog))
             (labels-of () (session-labels dialog))
             (selected () (nle::picker-row-value (nth dialog.selection (rows)))))
        (is (equal '("mom" "uncle") (labels-of)))
        (is (search "2 forks" (session-row-text (find "mom" (rows) :key #'nle::picker-row-value :test #'equal))))
        (is (equal "mom" (selected)) "the cursor opens on the first session, past the day over it")
        ;; One gesture per step: the rows it leaves drawn and the session the
        ;; cursor rests on, each checked when the step names it.
        (loop for (key shown selected why)
                in '((:right ("mom" "├ kid-a" "╰ kid-b" "uncle") "mom"
                      "the cursor stays on the session it opened")
                     (:right ("mom" "├ kid-a" "╰ kid-b" "uncle"))
                     (:down nil "kid-a" "the cursor walks the visible rows")
                     (:right ("mom" "├ kid-a" "│ ╰ grand" "╰ kid-b" "uncle"))
                     (:left ("mom" "├ kid-a" "╰ kid-b" "uncle") "kid-a"
                      "and the cursor stays on it")
                     (:down nil "kid-b")
                     (:left ("mom" "uncle") "mom" "and parks the cursor there")
                     (:left ("mom" "uncle"))
                     (:down nil "uncle" "past the day between them")
                     (:right ("mom" "uncle")))
              do (nle::picker-dialog-key model key)
                 (when shown (is (equal shown (labels-of)) (format nil "~s draws ~s" key shown)))
                 (when selected (is (equal selected (selected)) (or why selected))))
        (nle::picker-dialog-key model :escape)
        (is (null model.dialog) "Esc closes the picker")
        (nle::slash-sessions-op model)
        (is (equal '("mom" "uncle") (session-labels model.dialog)))))))

(deftest sessions-listing-opens-the-picker-when-it-lands ()
  ;; /sessions reads the directory off the loop thread, so the picker opens
  ;; on the message that read posts: its entries open the dialog, and a
  ;; failed read opens nothing and says why.
  (with-frame-model (model 100 30 :session-id "s-here")
    (with-stubbed-fdefinitions ((nle::sessions-preview-arm (m session-id &key memo)))
      ;; The read comes home the way OFF-LOOP brings it: its value, handed to
      ;; the continuation that installs it on the loop.
      (nle:update model (make-instance 'nle::off-loop-msg
                                       :value '(:error "connection refused")
                                       :continuation #'nle::install-session-directory))
      (is (null model.dialog) "a failed read opens nothing")
      (is (equal "Sessions failed: connection refused" (nle::status-said model)))
      (nle::install-session-directory
       model (list (listing-row "s-here" "here" "2026-01-01T00:00:00.000Z")))
      (is-present (dialog (nle::chat-model-dialog model)) "the listing opens the picker"
        (is (equal '("here") (session-labels dialog)))))))

(deftest sessions-preview-memo-adopts-without-refetching (with-temp-store ())
  ;; The scrub-back contract: a row already in the dialog's memo adopts into
  ;; the preview lane with no gateway round trip — the model here points at a
  ;; dead port, so any HTTP attempt would clear the lane. The memo is keyed on
  ;; (session . position): a conversation-search row's entry is the page that
  ;; ends at ITS block, so a second hit in the same session is another read.
  (let ((model (make-frame-model 100 30 :http-port 1))
        (memo (make-hash-table :test #'equal)))
    (setf (gethash (cons "s-memo" nil) memo)
          (cons "s-memo" model.state))
    (nle::sessions-preview-arm model "s-memo" :memo memo)
    (is (equal "s-memo" (car model.transcript-preview)))
    (nle::sessions-preview-clear model)
    (setf (gethash (cons "s-memo" 42) memo)
          (cons "s-memo" model.state))
    (nle::sessions-preview-arm model "s-memo" :memo memo :position 42)
    (is (equal "s-memo" (car model.transcript-preview)))))

(deftest a-preview-page-for-a-row-the-highlight-left-is-kept-not-shown (with-temp-store ())
  ;; The read comes home on its own thread: a page for a row the operator
  ;; has moved off (here: no dialog at all) is memoized for the next visit
  ;; and never seated; a failed read is never memoized.
  (let ((model (make-frame-model 100 30 :http-port 1))
        (memo (make-hash-table :test #'equal)))
    (nle::install-session-page model (cons "s-gone" nil) memo (cons (vector) nil))
    (is (null model.transcript-preview) "not seated")
    (is (gethash (cons "s-gone" nil) memo) "but memoized for the next visit")
    (nle::install-session-page model (cons "s-fail" nil) memo '(:error "dead port"))
    (is (not (consp (gethash (cons "s-fail" nil) memo))) "a failure is never kept as a page")))

(defun open-read-ahead-dialog (model count selection)
  "Open a picker over the sessions s-0 .. s-COUNT-1 on MODEL, highlighting
SELECTION: the rows a /sessions read-ahead walks."
  (nle::open-picker-dialog
   model "Sessions"
   (loop for index below count
         for id = (format nil "s-~d" index)
         collect (nle::make-picker-row :label id :value id))
   (lambda (model value) (declare (ignore model value)) nil)
   :selection selection)
  model.dialog)

(deftest the-sessions-read-ahead-folds-the-rows-around-the-highlight (with-temp-store ())
  ;; A held arrow arrives on rows already folded: a landing reads the rows
  ;; within reach of the highlight -- never the attached session -- and the
  ;; memo lets go of the oldest folds outside that reach past its keep.
  (let ((model (make-frame-model 100 30 :http-port 1 :session-id "s-6"))
        (memo (make-hash-table :test #'equal))
        (read '())
        (nle::*sessions-preview-reach* 2)
        (nle::*sessions-preview-kept* 5))
    (with-stubbed-fdefinitions ((nle::read-session-page (model session-id before)
                                  (push session-id read)
                                  (cons (vector) nil)))
      (let ((dialog (open-read-ahead-dialog model 12 4)))
        (nle::sessions-preview-arm model "s-4" :memo memo :follow nil)
        (is (equal "s-4" (car model.transcript-preview)) "the highlighted row is seated")
        ;; Its reach is read; the attached session is not.
        (is (equal '("s-2" "s-3" "s-4" "s-5") (sort (copy-list read) #'string<)))
        (setf read '()
              dialog.selection 0)
        (nle::sessions-preview-arm model "s-0" :memo memo :follow nil)
        (is (equal '("s-0" "s-1") (sort (copy-list read) #'string<)) "only what the memo lacks")
        (is (<= (count-if #'consp (alexandria:hash-table-values memo)) 5) "the keep holds")
        (is (consp (gethash (cons "s-2" nil) memo)) "a fold within the reach stays")
        (is (null (gethash (cons "s-4" nil) memo)) "the oldest outside it went first")))))

(deftest a-failed-preview-read-is-read-again-only-when-a-landing-asks (with-temp-store ())
  ;; A gateway refusing every read is not asked again the moment it refuses:
  ;; the read-ahead passes a failed row by, and a landing on it reads it again.
  (let ((model (make-frame-model 100 30 :http-port 1 :session-id "s-attached"))
        (memo (make-hash-table :test #'equal))
        (reads 0)
        (nle::*sessions-preview-reach* 1))
    (with-stubbed-fdefinitions ((nle::read-session-page (model session-id before)
                                  (incf reads)
                                  (error "dead port")))
      (open-read-ahead-dialog model 3 1)
      (nle::sessions-preview-arm model "s-1" :memo memo :follow nil)
      (is (= 3 reads) "each row within reach read once")
      (is (eq :failed (gethash (cons "s-1" nil) memo)))
      (nle::sessions-preview-read-ahead model memo)
      (is (= 3 reads) "the read-ahead does not retry")
      (nle::sessions-preview-arm model "s-1" :memo memo :follow nil)
      (is (= 4 reads) "a landing does"))))

;;; Without this, a preview at the newest page with nothing behind it would
;;; show no older page to scroll to when Standard folds a turn's tool trail to
;;; a row. A rested highlight reads the pages behind it.

(defun history-page (session-id before limit)
  "READ-SESSION-PAGE's answer out of the open store, LIMIT events a page,
its frames through the JSON the wire carries them in."
  (let* ((rows (nlk:page-events-before session-id
                                       (or before (1+ (nle::session-cursor session-id)))
                                       limit :as :facts))
         (floor (and rows (nlk::stored-fact-log-position (first rows)))))
    (cons (nlk:decode-json
           (nlk:encode-json-octets
            (coerce (nle::visible-event-frames session-id rows
                                               (nlk:session-visible-turn-set session-id))
                    'vector)))
          (and floor (nlk:events :session-id session-id :before floor :as :exists)
               floor))))

(defun seed-preview-history (session-id turns)
  "TURNS completed turns on SESSION-ID, each with a tool call."
  (dotimes (i turns)
    (seed-completed-turn session-id (format nil "ask ~d" i) (format nil "answer ~d" i)
                         :tools (list (list (format nil "c~d" i) "bash" "{\"command\":\"ls\"}" "out")))))

(defun rest-highlight (dialog)
  "DIALOG's highlight, rested for as long as the picker asks."
  (decf dialog.last-highlight-at
        (ceiling (* 2 nle::*picker-highlight-rest* internal-time-units-per-second))))

(defmacro with-paged-preview ((model dialog entry) &body body)
  "Run BODY with the newest page of s-1, ten events a page, seated as the
preview under a picker highlighting it; reads behind it come out of the store."
  `(let* ((,model (make-frame-model 100 30 :http-port 1 :session-id "s-attached"
                                           :tool-detail :standard))
          (memo (make-hash-table :test #'equal))
          (nle::*picker-highlight-rest* 60)
          (,dialog (open-read-ahead-dialog ,model 2 1)))
     (setf (nle::picker-dialog-last-highlight-at ,dialog) (get-internal-real-time))
     (with-stubbed-fdefinitions ((nle::read-session-page (model session-id before)
                                   (history-page session-id before 10)))
       (nle::install-session-page ,model (cons "s-1" nil) memo (history-page "s-1" nil 10) :follow nil)
       (let ((,entry (nle::chat-model-transcript-preview ,model)))
         (is (equal "s-1" (car ,entry)) "the newest page is seated")
         ,@body))))

(defun preview-events (entry)
  (length (getf (nle::preview-paging entry) :events)))

(deftest a-rested-preview-reads-back-until-its-fold-fills-three-screens (with-temp-store ())
  ;; Nothing is read while the highlight moves: a held arrow pays for its 128
  ;; events and no more. Rested, a fold that projects under the screens a
  ;; state's first projection covers reads the page behind it, one a tick, and
  ;; stops once it fills them -- short of the session's start.
  (seed-preview-history "s-1" 40)
  (with-paged-preview (model dialog entry)
    (let ((events (preview-events entry)))
      (nle::sessions-preview-older model dialog)
      (is (= events (preview-events entry)) "the highlight has not rested: nothing is read")
      (rest-highlight dialog)
      (nle::sessions-preview-older model dialog)
      (is (> (preview-events entry) events) "rested, a short fold reads the page behind it")
      (loop repeat 40 do (nle::sessions-preview-older model dialog))
      (is (getf (nle::preview-paging entry) :before) "it stopped short of the session's start")
      (let ((events (preview-events entry)))
        (loop repeat 5 do (nle::sessions-preview-older model dialog))
        (is (= events (preview-events entry)) "a fold that fills the screens reads no more"))
      (is (nle::with-render-scope (model)
            (nle::tail-projection-start (cdr entry) (nle::projection-width model)
                                        (* nle::*tail-projection-screens* model.term-height))) "and the fold projects past those screens"))))

(deftest a-preview-scrolled-to-its-top-reads-the-page-behind-it (with-temp-store ())
  ;; Whatever filled the screens, the operator can read on: at the top of the
  ;; fold the next tick reads another page, and the window keeps the rows it
  ;; was on.
  (seed-preview-history "s-1" 40)
  (with-paged-preview (model dialog entry)
    (rest-highlight dialog)
    (loop repeat 40 do (nle::sessions-preview-older model dialog))
    (nle::scroll-transcript model :home)
    (let ((events (preview-events entry))
          (anchor (nle::band-window-count model.band)))
      (nle::sessions-preview-older model dialog)
      (is (> (preview-events entry) events) "at the top, the page behind it is read")
      (is (> (nle::band-window-count model.band) anchor) "and its rows are in the transcript")
      (is (not model.follow-bottom) "the window stayed where the operator was reading"))))

(deftest a-page-read-behind-a-preview-keeps-what-the-live-follow-folded (with-temp-store ())
  ;; The reducer folds forward only, so a page behind folds again from its
  ;; oldest event: the events the live follow folded are kept beside the page's.
  (seed-preview-history "s-1" 6)
  (with-saved-globals ((nle::*tail-projection-screens* 100))
    (with-paged-preview (model dialog entry)
      (let ((cursor (nle::session-cursor "s-1")))
        (seed-completed-turn "s-1" "live ask" "live answer")
        (dolist (frame (coerce (car (history-page "s-1" nil (- (nle::session-cursor "s-1") cursor)))
                               'list))
          (let ((event (gethash "event" frame)))
            (setf (cdr entry) (nle::apply-gateway-event (cdr entry) event))
            (nle::preview-keep-event entry event))))
      (is (transcript-carries-p (cdr entry) "live answer") "the live follow folded it")
      (rest-highlight dialog)
      (loop repeat 20 do (nle::sessions-preview-older model dialog))
      (is (null (getf (nle::preview-paging entry) :before)) "every page behind was read")
      (is (transcript-carries-p (cdr entry) "live answer") "and none lost what the live follow folded")
      (is (transcript-carries-p (cdr entry) "ask 0") "the oldest turn came in front"))))

(deftest a-failed-older-read-ends-the-preview-reach (with-temp-store ())
  ;; A gateway that refuses the page behind is not asked again each tick.
  (seed-preview-history "s-1" 40)
  (with-paged-preview (model dialog entry)
    (let ((reads 0))
      (with-stubbed-fdefinitions ((nle::read-session-page (model session-id before)
                                    (incf reads)
                                    (error "dead port")))
        (rest-highlight dialog)
        (loop repeat 5 do (nle::sessions-preview-older model dialog))
        (is (= 1 reads) "asked once")
        (is (null (getf (nle::preview-paging entry) :before)) "and the reach ends")))))

;;; The header of a /sessions preview is the previewed session's: its title and
;;; its usage lane, as the gateway sums them when the page does not hold the
;;; whole session.

(defun seed-usage-turn (session-id input answer &key (in 1000) (out 200) cost)
  "A completed turn on SESSION-ID whose one round used IN tokens in and OUT out."
  (ensure-durable-session session-id)
  (let ((turn (nlk:admit-turn session-id (nlk::make-durable-id "command") input)))
    (apply #'nlk:record-turn-usage turn :provider "vendor" :model "m1"
           :input-tokens in :output-tokens out
           (and cost (list :cost-known t :cost-usd cost)))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" answer))
    (nlk:complete-turn turn nil)))

(defun frame-header (model)
  "The header row of MODEL's frame, its trailing canvas trimmed."
  (string-right-trim " " (nth 1 (nle::compose-frame model))))

(defun header-facts (session-id)
  "SESSION-HEADER-JSON as the wire carries it."
  (nlk:decode-json (nlk:encode-json-octets (nle::session-header-json session-id))))

(deftest the-gateway-answers-the-facts-a-sessions-header-shows (with-temp-gateway (port))
  (seed-usage-turn "hdr" "hdr ask 0" "ok" :in 1000 :out 200 :cost 0.25)
  (seed-usage-turn "hdr" "hdr ask 1" "ok" :in 4000 :out 100 :cost 0.5)
  (with-gateway-http (port :get "/api/gateway/sessions/hdr/header")
    (is (= 200 status))
    (is (equal "hdr ask 0" (gethash "title" body)) "named by its first prompt")
    (let ((sum (gethash "session_usage" body)) (newest (gethash "context_usage" body)))
      (is (= 5000 (gethash "input" sum)) "every round summed")
      (is (= 300 (gethash "output" sum)))
      (is (gethash "cost_known" sum))
      (is (< (abs (- 0.75 (gethash "cost" sum))) 1d-6) "the priced rounds' cost")
      (is (= 4000 (gethash "input" newest)) "the newest round alone fills the window")
      (is (equal "m1" (gethash "model" newest))))
    (is (plusp (gethash "through" body)) "the newest round it counted"))
  (nle::set-session-title "hdr" "renamed")
  (with-gateway-http (port :get "/api/gateway/sessions/hdr/header")
    (is (equal "renamed" (gethash "title" body)) "a rename is the title"))
  (seed-user-inputs "quiet" '("no rounds yet"))
  (with-gateway-http (port :get "/api/gateway/sessions/quiet/header")
    (is (zerop (gethash "through" body)) "no rounds, none counted")
    (is (zerop (hash-table-count (gethash "session_usage" body))))))

(deftest a-preview-header-is-the-previewed-sessions-not-the-attached-ones (with-temp-store ())
  ;; A page that holds the whole session sums its own usage; the header reads
  ;; the session the transcript shows, and goes back to the attached one when
  ;; the dialog closes.
  (seed-usage-turn "s-attached" "attached ask" "ok" :in 100 :out 10)
  (seed-usage-turn "s-1" "preview ask 0" "ok" :in 1000 :out 200)
  (seed-usage-turn "s-1" "preview ask 1" "ok" :in 1000 :out 200)
  (let* ((model (make-frame-model 120 30 :http-port 1 :session-id "s-attached"
                                         :tool-detail :developer))
         (memo (make-hash-table :test #'equal))
         (attached (frame-header model)))
    (is (search "attached ask" attached) "attached: its own title")
    (open-read-ahead-dialog model 2 1)
    (nle::install-session-page model (cons "s-1" nil) memo (history-page "s-1" nil 100) :follow nil)
    (let ((header (frame-header model)))
      (is (search "preview ask 0" header) "previewing: the previewed session's title")
      (is (search "s-1" header) "and its id")
      (is (not (search "attached ask" header)))
      (is (search "↑ 2k" header) "its rounds, summed")
      (is (search "↓ 400" header)))
    (setf model.dialog nil)
    (nle::clear-transcript-pin model)
    (setf model.transcript-preview nil)
    (is (search "attached ask" (frame-header model)) "the dialog closed: the attached session's")))

(deftest a-long-sessions-preview-header-waits-for-the-gateways-sums (with-temp-store ())
  ;; The first page holds a few turns of thirty: its sums are not the
  ;; session's, so the lane is empty and the title blank until the gateway has
  ;; summed them; they then hold through older pages and take the rounds the
  ;; live follow adds.
  (dotimes (i 30) (seed-usage-turn "s-1" (format nil "long ask ~2,'0d" i) "ok" :in 1000 :out 100))
  (with-saved-globals ((nle::*tail-projection-screens* 100))
    (with-paged-preview (model dialog entry)
      (setf model.tool-detail :developer)
      (let ((header (frame-header model)))
        (is (not (search "↑" header)) "the page's sums are not shown")
        (is (not (search "long ask" header)) "nor a prompt from the middle of the log"))
      (with-stubbed-fdefinitions ((nle::session-control-call (model method op &key body session-id)
                                    (header-facts session-id)))
        (rest-highlight dialog)
        (nle::sessions-preview-header model dialog)
        (let ((header (frame-header model)))
          (is (search "long ask 00" header) "the session's own title")
          (is (search "↑ 30k" header) "the whole session, summed"))
        (loop repeat 40 do (nle::sessions-preview-older model dialog))
        (is (null (getf (nle::preview-paging entry) :before)) "every page behind was read")
        (let ((header (frame-header model)))
          (is (search "↑ 30k" header) "older pages leave the sums where they were")
          (is (search "long ask 00" header)))
        (let ((cursor (nle::session-cursor "s-1")))
          (seed-usage-turn "s-1" "live ask" "live answer" :in 1000 :out 100)
          (dolist (frame (coerce (car (history-page "s-1" nil (- (nle::session-cursor "s-1") cursor)))
                                 'list))
            (let ((event (gethash "event" frame)))
              (setf (cdr entry) (nle::apply-gateway-event (cdr entry) event))
              (nle::preview-keep-event entry event))))
        (is (search "↑ 31k" (frame-header model)) "a round the follow folds is added")
        (loop repeat 3 do (nle::sessions-preview-older model dialog))
        (is (search "↑ 31k" (frame-header model)) "and stays")))))

;;; The name a session is given -- the operator's /rename, a cell's, the title
;;; a model writes beside the first turn -- is config evidence and flows on no
;;; stream. The header of the shell showing the session wears it from a
;;; session_titled frame, and from the replay when the shell attaches later.

(defun replay-title (frames)
  "The name the last session_stream_resumed chunk of FRAMES carries, or NIL."
  (nlk:json-value (car (last (frames-of-type frames "session_stream_resumed")))
                  :string "payload" "title"))

(defun titled-names (frames)
  "The (SESSION-ID . TITLE) of each session_titled frame of FRAMES, in order."
  (mapcar (lambda (frame) (cons (nlk:json-value frame :string "payload" "session_id")
                                (nlk:json-value frame :string "payload" "title")))
          (frames-of-type frames "session_titled")))

(deftest a-sessions-name-is-told-to-the-shells-attached-and-to-the-one-attaching (with-temp-gateway (port))
  (seed-usage-turn "named" "fix the flaky parser test in the lexer" "done")
  (is (search "fix the flaky parser" (replay-title (subscribed-frames port "named"))) "unnamed: its first prompt's line")
  (let* ((frames (frames-while (ws port :subscribe '("named"))
                   (is (equal "Flaky lexer test" (nle::set-session-title "named" "Flaky lexer test")))
                   (nle::set-session-title "named" "")
                   (nle::set-session-title "named" "Lexer flake")))
         (told (titled-names frames)))
    ;; Each name given is told once, the one now in effect: the prompt's line
    ;; when /rename default gives it back.
    (is (equal '(("named" . "Flaky lexer test") ("named" . "fix the flaky parser test in the lexer")
                 ("named" . "Lexer flake"))
               told))
    (is (equal "Lexer flake" (replay-title (subscribed-frames port "named"))) "a later attach reads it off the replay"))
  (nlk:create-session :id "unprompted")
  (is (null (replay-title (subscribed-frames port "unprompted"))) "no prompt, no name to carry"))

(deftest the-header-wears-the-name-a-session-is-given (with-temp-store ())
  (seed-usage-turn "s-attached" "attached ask" "ok" :in 100 :out 10)
  (let ((model (make-frame-model 120 30 :http-port 1 :session-id "s-attached"))
        (facts (car (history-page "s-attached" nil 100))))
    (flet ((titled (session-id title)
             (feed-wire model (nlk:encode-unsequenced-frame
                               "session_titled"
                               (nlk:json-object "session_id" session-id "title" title))))
           (replayed (sequence &rest key-args)
             (feed-wire model (apply #'nlk:encode-session-stream-resumed
                                     "s-attached" sequence facts :cursor 0 :admission-state "ready"
                                     :replay-chunk-index 0 :replay-chunk-final-p t key-args))))
      (is (search "attached ask" (frame-header model)) "unnamed: its first prompt")
      (titled "someone-else" "Another session's name")
      (is (not (search "Another session" (frame-header model))) "a name given to another session")
      (titled "s-attached" "Generated title")
      (let ((header (frame-header model)))
        (is (search "Generated title" header) "the name lands as it is given")
        (is (not (search "attached ask" header))))
      ;; The replay swaps in a state folded out of sight: the name it carries
      ;; is the one in effect at the swap, never the one the shell held.
      (replayed 3 :title "Renamed meanwhile")
      (is (search "Renamed meanwhile" (frame-header model)) "an attach shows the name the replay carries")
      (replayed 4)
      (let ((header (frame-header model)))
        (is (search "attached ask" header) "and the first prompt's line when it carries none")
        (is (not (search "Renamed meanwhile" header)))))))

(deftest the-sessions-preview-follows-the-row-the-highlight-rests-on (with-temp-store ())
  ;; A held arrow's rows are read, not watched: the live follow waits for the
  ;; highlight to rest.
  (let* ((model (make-frame-model 100 30 :http-port 1 :session-id "s-attached"))
         (nle::*picker-highlight-rest* 0.05)
         (dialog (open-read-ahead-dialog model 2 0)))
    (setf model.transcript-preview (cons "s-0" (nle::initial-chat-state))
          dialog.last-highlight-at (get-internal-real-time))
    (nle::sessions-preview-follow-at-rest model dialog)
    (is (null (nle::gateway-model-preview-session-id model)) "not while the highlight moves")
    (sleep 0.06)
    (nle::sessions-preview-follow-at-rest model dialog)
    (is (equal "s-0" (nle::gateway-model-preview-session-id model)) "the row it rests on")))

(deftest a-thread-that-changed-nothing-on-screen-owes-no-frame (with-temp-store ())
  ;; A page for a row the highlight has left lands on the loop with nothing to
  ;; show; its continuation says so, and the loop paints nothing for it.
  (let ((model (make-frame-model 100 30)))
    (flet ((landing-quiet-p (answer)
             (third (multiple-value-list
                     (nle:update model (make-instance 'nle::off-loop-msg
                                                      :value nil
                                                      :continuation (constantly answer)))))))
      (is (landing-quiet-p :quiet))
      (is (not (landing-quiet-p t)) "any other answer is a frame"))))

(deftest gateway-sessions-dialog-hydrates-the-highlighted-session (with-temp-gateway (port))
  ;; The Zig sessions_dialog_preview lane (pendingGatewaySessionsDialogHistory
  ;; + sessionsDialogPreview + adoptSessionsDialogPreview) on this shell:
  ;; every /sessions selection landing folds the highlighted session's durable
  ;; history into the transcript underneath the dialog — the /themes
  ;; live-preview shape — while the live state stays resident, and Enter
  ;; promotes the already-folded rows so the switch renders before the
  ;; subscription replay lands.
  (seed-user-inputs "preview-here" '("attached prompt"))
  (seed-user-inputs "preview-there" '("other session prompt"))
  (let ((model (gateway-shell port "preview-here" :ui t))
        ;; The shell sits in the folder the fixture sessions were opened in.
        (nle::*workspace-directory* (string-right-trim "/" (namestring (fixture-workspace))))
        (subscribed '())
        (history-reads 0))
    (with-stubbed-fdefinitions ((nle::send-subscribe (ws session-ids)
                                  (push (first session-ids) subscribed))
                                (nle::gateway-http-session-request (port method path body token &rest options)
                                  (when (search "/history" path) (incf history-reads))
                                  (apply original port method path body token options)))
      (flet ((visible-p (text) (transcript-carries-p (nle::visible-transcript-state model) text))
             (live-p (text) (transcript-carries-p model.state text)))
        (setf model.state
              (nle::initial-chat-state))
        ;; The live transcript this dialog opens over.
        (paint-model model (nle::ingest-message-fact "user" "attached prompt"))

        (nle::slash-sessions-op model)
        (is (nle::chat-model-dialog model) "/sessions opened")

        (sessions-dialog-highlight model "preview-there")
        (is (visible-p "other session prompt"))
        (is (live-p "attached prompt"))
        (let ((reads history-reads))
          (sessions-dialog-highlight model "preview-there")
          (is (= reads history-reads)))

        (sessions-dialog-highlight model "preview-here")
        (is (null model.transcript-preview))
        (is (visible-p "attached prompt"))

        (sessions-dialog-highlight model "preview-there")
        (nle::picker-dialog-key model :escape)
        (is (null model.transcript-preview))
        (is (visible-p "attached prompt"))
        (is (equal "preview-here" model.session-id))

        ;; Enter: the folded rows are the transcript before any replay.
        (nle::slash-sessions-op model)
        (sessions-dialog-highlight model "preview-there")
        (nle::picker-dialog-key model :enter)
        (is (equal "preview-there" model.session-id))
        (is (equal '("preview-there")
                   (remove "preview-here" subscribed :test #'equal)))
        (is (live-p "other session prompt"))
        (is (null model.transcript-preview))))))

;;; --- /sessions conversation search -----------------------------------------
;;; The box that greps every session's conversation: its rows come from the
;;; gateway's search (POST /api/gateway/sessions/search), the dialog shows them
;;; verbatim, and a chosen hit pins the attached viewport on the block it
;;; matched rather than on the tail.

(defun sessions-search-settle (model dialog query &key (rest 2))
  "Type QUERY into the open /sessions box and let the search land: the box is
mutated, one tick stages the query, the query is rested the way a pause would
rest it, and the next tick fires — the debounce the frame clock carries out,
without a sleep."
  (nle::picker-search-mutate dialog query)
  (nle::sessions-search-tick model dialog)
  (let ((state (gethash dialog nle::*session-search*)))
    (setf (getf state :staged-at)
          (- (get-internal-real-time) (* rest internal-time-units-per-second))))
  (nle::sessions-search-tick model dialog))

(deftest sessions-search-box-greps-conversations (with-temp-gateway (port))
  (seed-user-inputs "grep-here" '("attached session opening"))
  ;; The directory lists newest activity first, and a millisecond tie lists
  ;; by id, grep-here ahead: in a warm full run both seeds landed in one
  ;; millisecond. The second seed waits the tick out, so it is the newer.
  (sleep 0.002)
  (seed-completed-turn "grep-there"
                       (concatenate 'string
                                    (make-string 100 :initial-element #\p)
                                    "pelican"
                                    (make-string 300 :initial-element #\p)
                                    " marsupial lives here")
                       "a pelican answer about the marsupial")
  (let ((model (gateway-shell port "grep-here" :ui t))
        ;; The shell sits in the folder the fixture sessions were opened in.
        (nle::*workspace-directory* (string-right-trim "/" (namestring (fixture-workspace))))
        (searches 0)
        (history-paths '()))
    (with-stubbed-fdefinitions
        ((nle::send-subscribe (ws session-ids) nil)
         (nle::gateway-http-session-request (port method path body token &rest options)
           (when (search "/sessions/search" path) (incf searches))
           (when (search "/history" path) (push path history-paths))
           (apply original port method path body token options)))
      (nle::slash-sessions-op model)
      (let ((dialog model.dialog)
            (directory (list (make-string 64 :initial-element #\p) "attached session opening")))
        (is (equal directory (session-labels dialog)))
        (sessions-search-settle model dialog "pelican marsupial")
        (is (= 1 searches) "the rested query is one gateway read")
        (let ((rows (nle::picker-visible-rows dialog)))
          (is (= 2 (length rows)) "one row per matching block, newest first")
          (is (search "marsupial" (nle::picker-row-label (first rows))))
          (is (null (nle::search-score
                     "pelican marsupial"
                     (list (nle::picker-row-key (second rows))
                           (nle::picker-row-label (second rows))
                           (nle::picker-row-detail (second rows))))))
          (is (char= #\… (char (nle::picker-row-label (second rows)) 0)))
          (is (search "pelican" (nle::picker-row-label (second rows))))
          (is (search "you" (nle::picker-row-detail (second rows))))
          (is (equal "grep-there" (car (nle::picker-row-value (second rows)))))
          (is (integerp (cdr (nle::picker-row-value (second rows)))))
          (is (equal " 2 matches" dialog.context)))
        ;; The highlighted hit's page is the transcript underneath, and the
        ;; window rests on the block it matched — the previewed session's,
        ;; not the attached one's.
        (let* ((seq (cdr (nle::picker-row-value (first (nle::picker-visible-rows dialog)))))
               (band model.band))
          (is-shape model (nle::transcript-shown-session-id "grep-there") (.follow-bottom null))
          (is (= (nle::band-window-offset band)
                 (min (nle::transcript-line-for-position model seq)
                      (nle::band-window-max-offset band)))))
        ;; The newest read is the search hit's own page; the one before it
        ;; is the directory highlight at open (the newest rows).
        (is (search "before=" (first history-paths)))
        ;; Emptying the box needs no round trip: the directory comes back.
        (sessions-search-settle model dialog "")
        (is (= 1 searches) "clearing the box asks the gateway nothing")
        (is (equal directory (session-labels dialog)))))))

(deftest sessions-search-enter-pins-the-viewport-on-the-block (with-temp-gateway (port))
  (seed-user-inputs "pin-here" '("attached session opening"))
  ;; The block sits in the middle of a long conversation: turns before it to
  ;; scroll past and turns after it to land above.
  (dotimes (i 6)
    (seed-completed-turn "pin-there" (format nil "before ~d" i)
                         (format nil "earlier ~d ~{~a ~}" i
                                 (make-list 24 :initial-element "words"))))
  (seed-completed-turn "pin-there" "the pelican lives in this block" "noted")
  (dotimes (i 6)
    (seed-completed-turn "pin-there" (format nil "after ~d" i)
                         (format nil "later ~d ~{~a ~}" i
                                 (make-list 24 :initial-element "words"))))
  (let ((model (gateway-shell port "pin-here" :ui t)))
    (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
      (nle::slash-sessions-op model)
      (let ((dialog model.dialog))
        (sessions-search-settle model dialog "pelican")
        (let* ((row (first (nle::picker-visible-rows dialog)))
               (seq (cdr (nle::picker-row-value row))))
          (is (equal "pin-there" (car (nle::picker-row-value row))))
          (nle::picker-dialog-key model :enter)
          (is-shape model (.session-id "pin-there") (.dialog null "and closes the box"))
          (is (nle::transcript-pin-p model "pin-there"))
          (is (null model.follow-bottom))
          (let ((offset (nle::band-window-offset model.band)))
            (is (plusp offset) "the window moved off the top")
            (is (equal seq
                       (nle::transcript-row-log-position
                        (nle::transcript-item-at-line model offset))))
            (nle::render-state model)
            (is (equal offset (nle::band-window-offset model.band))))
          (nle::scroll-transcript model :line-down)
          (is (null (nle::transcript-pin-p model "pin-there"))))))))

;;; --- /sessions F2: a session renamed where it is listed -----------------------

(deftest sessions-picker-f2-renames-the-highlighted-session (with-temp-gateway (port))
  (seed-user-inputs "f2-here" '("attached prompt"))
  (seed-user-inputs "f2-there" '("the pelican session"))
  (let ((model (gateway-shell port "f2-here" :ui t))
        ;; The shell sits in the folder the fixture sessions were opened in.
        (nle::*workspace-directory* (string-right-trim "/" (namestring (fixture-workspace)))))
    (with-stubbed-fdefinition (nle::send-subscribe (ws session-ids) nil)
      (nle::slash-sessions-op model)
      (let ((dialog model.dialog))
        (sessions-dialog-highlight model "f2-there")
        (dialog-keys model :f2)
        (is (equal "Name" dialog.input-label) "F2 opens the name field")
        (is (equal "the pelican session" dialog.input) "holding the session's name")
        (nle::picker-dialog-key model #\u :ctrl t)
        (apply #'dialog-keys model (coerce "Pelican work" 'list))
        (dialog-keys model :enter)
        (is (null dialog.input-label) "Enter goes back to the list")
        (is (eq dialog model.dialog) "which stays open")
        (is (equal "Pelican work" (nlk::session-title "f2-there")) "the gateway holds the name")
        (is (member "Pelican work" (visible-labels dialog) :test #'equal) "the row reads it")
        (is (equal "f2-there" (nle::picker-highlighted-value dialog)) "and keeps the cursor")
        (dialog-keys model :f2 #\x :escape)
        (is (null dialog.input-label) "Esc leaves the field")
        (is (eq dialog model.dialog) "and not the picker")
        (is (equal "Pelican work" (nlk::session-title "f2-there")) "naming nothing")
        ;; The name is found by its own words, which no conversation holds.
        (sessions-search-settle model dialog "work")
        (is (equal '("Pelican work") (visible-labels dialog)) "a given name is searched")
        ;; A search hit renames its session too, and reads the new name.
        (sessions-search-settle model dialog "pelican")
        (dialog-keys model :f2)
        (is (equal "Pelican work" dialog.input) "a hit's field holds its session's name")
        (nle::picker-dialog-key model #\u :ctrl t)
        (apply #'dialog-keys model (coerce "Found by search" 'list))
        (dialog-keys model :enter)
        (is (equal "Found by search" (nlk::session-title "f2-there")))
        (let ((hit (first (nle::picker-visible-rows dialog))))
          (is (equal "Found by search" (nle::picker-row-key hit)) "the hit reads it"))))))

;;; --- gate: a start_turn's images reach the provider --------------------------
;;; The whole seam in one proof: the client stages an attachment, the codec
;;; carries it as an image content item, the gateway decodes it, the worker
;;; hands it to the turn, and the compiled context attaches it to the visible
;;; user message the provider round is built from.

(deftest start-turn-images-reach-the-provider-over-the-wire ()
  (with-gateway-session (port "image-wire")
    (let ((seen nil))
      (with-stubbed-fdefinition (nle::call-provider (context)
                                  (setf seen (coerce (nle::compiled-turn-context-messages context)
                                                     'list))
                                  (wire-message "assistant" :content "ok"))
        (frames-while (ws port :subscribe '("image-wire"))
          (nle::send-start-turn ws "image-wire" "[Image #1] what is this?"
                                :command-id "image-wire-1"
                                :images '(("image/png" . "QUJD")))))
      (is-present seen "the admitted turn reached the provider"
        (let ((parts (nle::message-content-parts
                      (gethash "content" (car (last seen))))))
          (is (= 2 (length parts)))
          (is (equal "[Image #1] what is this?" (gethash "text" (first parts))))
          (is (equal "data:image/png;base64,QUJD"
                     (gethash "url" (gethash "image_url" (second parts))))))))))

;;; --- the harness macros ---------------------------------------------------
;;; WITH-PROVIDER-STUB's restore is invisible when it works and silent when it
;;; does not, so it is asserted rather than trusted. WITH-GATEWAY-HTTP's token
;;; default is asserted by the route gates themselves: GATEWAY-SERVICE-ROUTES-
;;; AND-HYGIENE reads /status with the default and with :token nil, and
;;; GATEWAY-HTTP-AUTH-AND-EXACT-SESSION-IDENTITY sends an explicit wrong one.

(deftest with-provider-stub-restores-the-real-provider ()
  (let ((real (fdefinition 'nodecode.evolved::call-provider)))
    (with-provider-stub (nil (wire-message "assistant" :content "stub"))
      (is (not (eq real (fdefinition 'nodecode.evolved::call-provider)))))
    (is (eq real (fdefinition 'nodecode.evolved::call-provider)))
    (ignore-errors
      (with-provider-stub (nil (wire-message "assistant" :content "stub"))
        (error "synthetic unwind")))
    (is (eq real (fdefinition 'nodecode.evolved::call-provider)))))

(deftest with-provider-stub-accepts-the-streaming-lambda-list ()
  ;; CALL-PROVIDER takes (CONTEXT &KEY ON-PART); a stub that took only CONTEXT
  ;; failed as a wrong-argument error inside the turn instead of as a test.
  (with-provider-stub (context (wire-message "assistant" :content "ok"))
    (is (equal "ok" (gethash "content"
                             (nodecode.evolved::call-provider nil :on-part (constantly nil)))))))

(defun wipe-operator-token-for-hostile-cell ()
  "The hostile cell's payload, compiled HERE so it wipes the same token
symbol the test's own START-GATEWAY reference reads."
  ;; The evolve tests rebuild the evolved package mid-suite; a cell source
  ;; file compiled after that rebuild resolves the token name against the
  ;; rebuilt package and its wipe lands on a symbol the gateway under test
  ;; never consults.
  (setf nle::*gateway-operator-token* nil))

(deftest gateway-start-refuses-cell-that-disturbs-operator-auth ()
  ;; The seam contract locked here: no listener stays up whose operator auth
  ;; was disturbed during cell startup — the cells' thread tears it down and
  ;; fails with the reason. The cell is a folder: loaded
  ;; the way MAIN loads it, started the way the gateway starts every
  ;; peripheral.
  (with-gateway-scratch (root "cells" :store t)
    (with-saved-globals (nlk::*cells*)
      (let ((dir (merge-pathnames "hostile-cell/" root)))
        (write-temp-file (merge-pathnames "hostile-cell.asd" dir)
                         "(defsystem \"hostile-cell\" :components ((:file \"hostile\")))")
        (write-temp-file (merge-pathnames "hostile.lisp" dir)
                         "(defpackage #:hostile-cell (:use #:cl) (:export #:start-cell))
(in-package #:hostile-cell)
(defun start-cell (config)
  (declare (ignore config))
  (funcall (uiop:find-symbol* '#:wipe-operator-token-for-hostile-cell
                              '#:nodecode.test))
  nil)")
        (let ((asdf:*central-registry* asdf:*central-registry*)
              ;; LOAD-CELLS freezes the image's loaded systems; rebind
              ;; the set so the freeze cannot outlive this test into
              ;; later suite lanes.
              (asdf::*immutable-systems* asdf::*immutable-systems*))
          (let ((record (first (nlk:load-cells :root root))))
            (is (and record (eq :peripheral (nlk:cell-kind record)))))
          ;; The cells start behind the start, once the shell has its frame:
          ;; the start answers, and the cells' thread tears the listener
          ;; down and fails with the reason. That failure ends the process
          ;; it escapes in, so the test takes it from the debugger hook.
          (let ((said nil))
            (with-saved-globals ((sb-ext:*invoke-debugger-hook*
                                  (lambda (condition hook)
                                    (declare (ignore hook))
                                    (push (princ-to-string condition) said)
                                    (sb-thread:abort-thread))))
              (let ((started (nle:start-gateway :port 0 :operator-token "test-token"
                                                :workspace-captures nil)))
                (is started "the start answers before the cells run"))
              (bt2:signal-semaphore nle::*replay-sent-semaphore*)
              (is (await (:tries 300 :rest 0.01) (null nle::*gateway-handler*)) "the listener is torn down")
              (is (await (:tries 300 :rest 0.01)
                    (find "disturbed the gateway operator token" said :test #'search)) "and the reason said"))))))))

;;; --- accepted sockets never wait on Nagle ----------------------------------

(deftest gateway-accepted-sockets-carry-tcp-nodelay ()
  ;; Every pooled control request from an attaching shell paid Linux's 40 ms
  ;; delayed ACK against hunchentoot's two-segment responses; the gateway's
  ;; acceptor marks each accepted socket before the first byte is read.
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
         (client (usocket:socket-connect "127.0.0.1" (usocket:get-local-port listener)))
         (accepted (usocket:socket-accept listener)))
    (nlk:with-cleanup ((ignore-errors (usocket:socket-close client))
                       (ignore-errors (usocket:socket-close accepted))
                       (ignore-errors (usocket:socket-close listener)))
      (is (not (usocket:socket-option accepted :tcp-nodelay)))
      (is (eq accepted (nle::mark-interactive-socket accepted)))
      (is (usocket:socket-option accepted :tcp-nodelay))
      (is (find-method #'hunchentoot:process-connection '(:before)
                       (list (find-class 'clack.handler.hunchentoot::clack-acceptor)
                             (find-class t))
                       nil)))))

(deftest the-image-topic-holds-still-while-shells-come-and-go ()
  ;; While the gateway serves, (help :image) says what this process owns,
  ;; that a shell may run in another, and the rule, and every request's help
  ;; section carries its line — and nothing a relaunch or a shell attaching
  ;; moves: the pid, the port and the shells stand in the prompt.
  ;; (attached-shells) is the read.
  (with-temp-gateway (port)
    (is (null (assoc "image" (nle::read-live-sections nil) :test #'string=)))
    ;; its line rides every request
    (is (search (format nil "  :image - ~a" nle::*image-help-summary*) (cell-section nil "help")))
    (flet ((image () (nle:help :image)))
      (let ((quiet (image)))
        (is-present (text quiet) "(help :image) answers it"
          (is (search "This image owns the store " text) text)
          (is (search "(attached-shells)" text) "where the shells are read")
          (is (search "sent to every attached shell in another process" text) "and the rule")
          ;; the store's path is a temp name and may spell any digits, so the
          ;; wording is what is looked for
          (is-carrying text (:absent "image is pid" "no pid") (:absent " port " "no port")
            (:absent "Shells attached" "no list of shells")))
        (let* ((a (nlk:durable-session-id (nlk:create-session)))
               (b (nlk:durable-session-id (nlk:create-session)))
               (gws (nle::find-or-create-gw-session a))
               (other (nle::find-or-create-gw-session b)))
          (setf (nle::gw-peer-state-pid (nle::ensure-gw-peer-state :shell-here)) (sb-posix:getpid))
          (setf (nle::gw-peer-state-pid (nle::ensure-gw-peer-state :shell-there)) 424242)
          (nle::ensure-gw-peer-state :shell-mute)
          (push :shell-here gws.peers)
          (push :shell-there other.peers)
          (push :shell-mute other.peers)
          (is (equal (list (sb-posix:getpid)) (rest (assoc a (nle::attached-shells) :test #'string=))))
          (is (equal '(nil 424242) (rest (assoc b (nle::attached-shells) :test #'string=))))
          (is (string= quiet (image)) "three shells attached, the same bytes")
          (let ((nle::*current-durable-turn* (nlk::%make-durable-turn :session-id a :turn-id "t" :command-id "c" :definition-revision "base")))
            (is (string= quiet (image)) "and the same from inside a turn"))))))
  (is (null (assoc :image nle::*help-topics*)) "off the manual with the gateway"))

;;; --- the gateway: the organism kept running by the operating system ---------
;;; organism/background.lisp is the unit and its verbs (organism/
;;; background-test); these are its seams here: the patient owner `gateway
;;; run' is, the cell's word that turns it on, and the resurrection rung
;;; handing a dead owner to systemd. WITH-SCRIPTED-SYSTEMD is that file's.

(deftest gateway-wait-takes-the-store-when-its-owner-lets-go ()
  ;; `gateway run' never loses the election: while a shell owns the store
  ;; it waits, and takes the organism over the moment the shell lets go. The
  ;; same wait thread can carry the systemd watchdog tick.
  (let ((looks 0)
        (ticks 0)
        (said (make-string-output-stream)))
    (is (= 2 (let ((*standard-output* said))
               (nle::await-free-store #p"/tmp/nodecode-no-store/history.db"
                                      :held-p (nlk:lambda-ignoring (path)
                                                (< (incf looks) 3))
                                      :pause 0
                                      :tick (lambda () (incf ticks))))))
    (is (= 2 ticks) "the waiting owner refreshes its watchdog")
    (is (= 1 (count #\Newline (get-output-stream-string said)))))
  (is (zerop (nle::await-free-store #p"/tmp/nodecode-no-store/history.db"
                                    :held-p (constantly nil) :pause 0))))

(deftest keep-running-turns-the-gateway-on-once-and-never-over-an-off (with-scripted-systemd (fake))
  (with-saved-globals ((nlk:*ephemeral* t))
    (let ((said '()))
      (nle:hook :frame "keep-running-test"
                (lambda (op next)
                  (when (equal (getf op :kind) "organism.notice")
                    (push (gethash "text" (getf op :payload)) said))
                  (funcall next op)))
      (is (null (nle:keep-running "the Fake bot")) "an ephemeral organism keeps nothing running")
      (setf nlk:*ephemeral* nil)
      (with-stubbed-fdefinition (nlk:organism-binary () "/opt/nodecode/current/nodecode")
        (let ((thread (nle:keep-running "the Fake bot")))
          (is thread "the first ask turns it on, on a thread of its own")
          (when thread (bt2:join-thread thread))))
      (is (nlk:gateway-installed-p) "the gateway is on")
      (is (row-carrying "keeps the Fake bot running after this shell closes" said))
      (is (null (nle:keep-running "the Fake bot")) "once: a gateway already on is left be")
      (nlk:gateway-off)
      (is (null (nle:keep-running "the Fake bot")) "and never over the operator's off"))))

(deftest resurrection-asks-systemd-when-the-gateway-supervises-the-store ()
  ;; A dirty death under the gateway is systemd's to repair: the organism
  ;; comes back under the unit `gateway off' stops, never beside it.
  (with-scripted-systemd (fake)
    (let ((store nlk::*default-store-path*))
      (is (not (nle::gateway-supervises-p store)) "no unit, no supervisor")
      (nlk:gateway-on :program "/opt/nodecode/current/nodecode")
      (is (nle::gateway-supervises-p store) "the gateway supervises the home's own store")
      (is (not (nle::gateway-supervises-p #p"/tmp/nodecode-elsewhere/history.db")))
      (setf (fake-systemd-calls fake) '())
      (is-values (launched how) (nle::spawn-resurrection-candidate store)
        (launched (list "systemctl" "--user" "start" (nlk:gateway-unit-name))) (how eq :systemd))
      (is (member "start" (unit-verbs fake) :test #'equal) "systemd was asked"))))

;;; --- the live publication out of memory ------------------------------------
;;; PUBLISH-TURN-FACTS drains what the store committed and still holds; it
;;; pages the log only when that memory does not reach back to the cursor.
;;; Whichever fed it, a peer must fold one transcript: frame for frame, byte
;;; for byte, cursor included, off-chain and evidence facts dropped alike.

(defmacro with-ring-publisher ((session-id) &body body)
  "Run BODY with a :FRAME hook capturing SESSION-ID's live frames and three
local functions over it: (PUBLISHED CURSOR) runs PUBLISH-TURN-FACTS from
CURSOR and answers (CURSOR (KIND CURSOR PAYLOAD-JSON) ...), the cursor it
returned and every frame it put on the wire; (KINDS-OF PUBLICATION) the
event types one such answer carried; (FORGET-COMMITTED) empties the store's
ring so the next publication pages."
  (let ((seen (gensym "SEEN"))
        (lock (gensym "LOCK"))
        (key (format nil "test-ring-~a" session-id)))
    `(let ((,seen '())
           (,lock (bt2:make-lock)))
       (nle:hook :frame ,key
                 (lambda (op next)
                   (when (equal (getf op :session-id) ,session-id)
                     (bt2:with-lock-held (,lock)
                       (push (list (getf op :kind) (getf op :cursor)
                                   (nlk:encode-json-object (getf op :payload)))
                             ,seen)))
                   (funcall next op)))
       (nlk:with-cleanup ((nle:unhook :frame ,key))
         (flet ((published (cursor)
                  (bt2:with-lock-held (,lock) (setf ,seen '()))
                  (let ((answered (nle::publish-turn-facts ,session-id cursor)))
                    (cons answered (bt2:with-lock-held (,lock) (reverse ,seen)))))
                (kinds-of (publication)
                  (loop for (nil nil json) in (rest publication)
                        collect (nlk:json-value (nlk:decode-json json) :string
                                                "frame" "event" "type")))
                (forget-committed (&aux (table (nlk::store-committed nlk::*store*)))
                  (sb-ext:with-locked-hash-table (table) (clrhash table))))
           (declare (ignorable #'published #'kinds-of #'forget-committed))
           ,@body)))))

(deftest the-live-publication-from-memory-is-the-paged-publication (with-session-store ("gw-ring"))
  (with-ring-publisher ("gw-ring")
    (let ((pre (nle::session-cursor "gw-ring")))
      ;; turn A, then the head moved back to root: A is an abandoned branch
      (seed-completed-turn "gw-ring" "first" "one")
      (nlk:move-session-head "gw-ring" nil)
      (seed-completed-turn "gw-ring" "second" "two"
                           :tools '(("c1" "eval" "(+ 1 2)" "3")))
      (nlk:record-workspace-capture "gw-ring" :boundary-kind "user_message")
      (let* ((pages 0)
             (from-memory
               (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                           (error "the log was paged: ~s" args))
                 (published pre)))
             (from-log
               (progn
                 (forget-committed)
                 (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                             (incf pages)
                                             (apply original args))
                   (published pre)))))
        (is (plusp pages) "with nothing in memory the publisher pages")
        (is (equal from-memory from-log))
        (is (= (first from-memory) (nle::session-cursor "gw-ring")))
        (let ((kinds (kinds-of from-memory)))
          (is (= 1 (count "turn.started" kinds :test #'equal)))
          (is-table (kind) (member kind kinds :test #'equal)
            ("turn.input_committed" "the head chain's facts pass")
            ("turn.tool_call_started" "tool facts pass")
            ("turn.assistant_message_completed" "the reply passes"))
          (is (not (member "session.workspace_captured" kinds :test #'equal)))
          (is (not (member "session.head_moved" kinds :test #'equal))))
        ;; A turn started after the drain is the head with no head-move
        ;; fact: the memoized visible set must follow it.
        (seed-completed-turn "gw-ring" "third" "three")
        (let ((next (published (first from-memory))))
          (is (= (first next) (nle::session-cursor "gw-ring")))
          (is (= 1 (count "turn.started" (kinds-of next) :test #'equal)))
          (is (member "turn.completed" (kinds-of next) :test #'equal)))))))

(deftest a-payload-over-the-byte-budget-is-let-go-and-the-publisher-pages ()
  ;; The ring is bounded by payload text as well as count: a tool result
  ;; over the budget is not retained, nor anything older, and the floor
  ;; passes it — so a publication from before it pages that stretch of the
  ;; log, and the frames are the frames all the same.
  (with-saved-globals ((nlk::*committed-events-payload-budget* 4096))
    (with-session-store ("gw-ring-bytes")
      (with-ring-publisher ("gw-ring-bytes")
        (let ((pre (nle::session-cursor "gw-ring-bytes")))
          (seed-completed-turn "gw-ring-bytes" "run it" "done"
                               :tools (list (list "c1" "eval" "(big)"
                                                  (make-string 8192 :initial-element #\x))))
          (let ((big (nlk:events :session-id "gw-ring-bytes" :kind "turn.tool_result"
                                 :as :value :columns '("log_position"))))
            (is (eq :miss (nth-value 1 (nlk:committed-events-after "gw-ring-bytes" (1- big)))))
            (multiple-value-bind (drained miss)
                (nlk:committed-events-after "gw-ring-bytes" big)
              (is (null miss) "what came after it fits again")
              (is (equal '("turn.assistant_message_completed" "turn.completed")
                         (mapcar #'nlk:event-kind drained)))))
          (let* ((pages 0)
                 (mixed (with-stubbed-fdefinition (nlk:page-events (&rest args)
                                                    (incf pages)
                                                    (apply original args))
                          (published pre)))
                 (from-log (progn
                             (forget-committed)
                             (published pre))))
            (is (plusp pages) "the publisher paged the stretch the ring let go")
            (is (equal mixed from-log) "and the frames are the frames, byte for byte")
            (is (member "turn.tool_result" (kinds-of mixed) :test #'equal))
            (is (= (first mixed) (nle::session-cursor "gw-ring-bytes")))))))))

;;; --- the door: the input cap, and the worker's terminal fact -----------------------

(deftest an-oversized-input-is-refused-at-submit-and-no-turn-is-born ()
  ;; SUBMIT read no size, and ADMIT refused on the worker, before RUN's
  ;; handlers. The refusal is the submitter's now.
  (with-session-store ("gw-oversized-input")
    (let ((input (make-string (1+ nle::*max-input-characters*) :initial-element #\x)))
      (is (typep (signals-error nle::input-too-large
                   (nle:submit "gw-oversized-input" input :command-id "gw-oversized-cmd"))
                 'nle::input-too-large) "the submitter's own answer")
      (is (not (nlk:active-turn-p "gw-oversized-input")) "no turn is running")
      (is (null (nlk:session-head-turn-id "gw-oversized-input")) "and none was born")
      (is (null (find "turn.started" (nlk:page-events :session-id "gw-oversized-input")
                      :key #'nlk:event-kind :test #'equal)) "nothing was admitted"))))

(deftest an-admitted-turn-whose-run-cannot-start-is-failed-not-stranded ()
  ;; The worker's backstop, whatever the cause: RUN's outcome handlers open
  ;; after its first ADMIT, so a signal there -- here the heap latch of a
  ;; session already in flight -- left the admitted turn open with no owner.
  (with-session-store ("gw-stranded-arm")
    (let ((session (nle::intern-session "gw-stranded-arm")))
      (with-saved-globals (((nle::session-in-flight-p session) t))
        (let* ((admission (nle:submit "gw-stranded-arm" "hello" :command-id "gw-stranded-cmd"))
               (turn-id (nlk:durable-turn-turn-id (nlk:active-input-admission-turn admission))))
          (is (eq :started (nlk:active-input-admission-disposition admission)) "the admission itself stands")
          (is-eventually (nlk:turn-terminal-kind turn-id)
                         "the turn ends on a durable fact however RUN left it" :seconds 10)
          (is (equal "turn.failed" (nlk:turn-terminal-kind turn-id)) "and the fact says it failed, never left open"))))))

;;; --- the shell's gateway-backed commands -----------------------------------
;;; /think and a cell's commands, driven from a frame shell
;;; attached to a fixture gateway: the snapshot, the mutation and the run
;;; cross the control routes the way they do for every shell.

(defun gateway-frame-model (port session-id)
  "A frame shell on SESSION-ID whose control calls reach PORT's gateway."
  (make-frame-model 80 24 :session-id session-id :http-port port :http-token "test-token"))

(defmacro with-gateway-frame ((model session-id provider model-id) &body body)
  "BODY with MODEL a frame shell on SESSION-ID over a fixture gateway whose
default target is PROVIDER/MODEL-ID, restored after."
  `(with-saved-globals (nle::*provider* nle::*model*)
     (with-gateway-session (port ,session-id)
       (setf nle::*provider* ,provider nle::*model* ,model-id)
       (let ((,model (gateway-frame-model port ,session-id)))
         ,@body))))

(deftest slash-think-walks-the-reasoning-ladder (with-gateway-frame (model "think-dlg"
                                                                   "openai-completions"
                                                                   "think-model"))
  (with-slash-dialog (dialog model "think" "") "/think opens silently"
    (is-present dialog "the think dialog is open"
      (is (search "Reasoning effort" dialog.title))
      (let ((rows dialog.rows))
        (is (= 8 (length rows)))
        (is (equal "Default" (nle::picker-row-label (first rows))))
        (is (equal "off" (nle::picker-row-label (second rows)))))
      (is (= 0 dialog.selection))))
  ;; Enter on `high` (Default, off, minimal, low, medium, high).
  (dialog-keys model :down :down :down :down :down :enter)
  (is (equal "high" (session-effort "think-dlg")))
  ;; Reopen: the requested rung is preselected and marked current.
  (nle::execute-slash-command model "think" "")
  (let ((dialog model.dialog))
    (is (= 5 dialog.selection))
    (is (search "current" (nle::picker-row-detail
                           (nth 5 dialog.rows)))))
  ;; Default clears the preference.
  (dialog-keys model :up :up :up :up :up :enter)
  (is (null (session-effort "think-dlg"))))

(deftest slash-think-accepts-an-argument (with-gateway-frame (model "think-arg" "openai-completions"
                                                                   "think-arg"))
  (let ((entry (nle::find-slash-command "think")))
    (is (equal "/think [rung]" entry.usage))
    (is (equal '("high" "xhigh")
                (mapcar (lambda (choice) (getf choice :value))
                        (nle:slash-completions "think" "h")))))
  (multiple-value-bind (notice cmd)
      (nle::execute-slash-command model "think" "high")
    (is (null cmd) "an argument pick starts no turn")
    (is (search "applies from the next turn" notice)))
  (is (equal "high" (session-effort "think-arg")))
  (nle::execute-slash-command model "think" "")
  (is (nle::picker-dialog-p model.dialog))
  (multiple-value-bind (notice cmd)
      (nle::execute-slash-command model "think" "high low")
    (is (null cmd) "malformed arguments start no turn")
    (is (equal "usage: /think [rung]" notice))))

(deftest cell-commands-join-the-slash-surface (with-temp-gateway (port))
  ;; A cell's command runs where the cell runs: a shell reads it off GET
  ;; /api/gateway/slash and runs it through POST /api/gateway/slash, the
  ;; verbatim argument tail and the shell's session riding the line, and the
  ;; answer's text is the shell's notice.
  (let ((calls '()))
    (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
      (nle:register-command "demo-pack" "greet"
                            (lambda (args session-id)
                              (push (list args session-id) calls)
                              (format nil "hi ~a" args))
                            :description "Say hello" :argument-hint "[name]")
      ;; A condition the command does not catch is the route's refusal.
      (nle:register-command "demo-pack" "boom"
                            (nlk:lambda-ignoring (args session-id)
                              (error "kaboom"))
                            :description "Always breaks" :catches 'type-error)
      ;; Shadowing a builtin is refused.
      (nle:register-command "demo-pack" "help" (constantly "shadowed") :description "Shadow attempt")
      (let ((model (gateway-frame-model port "ext-slash")))
        ;; init read the gateway's commands.
        (is-present (entry (nle::find-slash-command "greet")) "the registration is a live slash entry"
          (is (equal "/greet [name]" (nle::slash-command-usage entry))))
        (is (null (nle::find-gateway-command "help")) "a builtin's name stays the builtin's")
        (is (= 1 (length (nle::slash-suggestions "/gr"))))
        (is (find "/greet [name]" (nle::help-dialog-rows model)
                  :key #'nle::picker-row-label :test #'equal))
        (type-string model "/gr")
        (is (equal "eet" (nle::composer-ghost-remainder model)))
        (nle::editor-reset (nle::chat-model-editor model))
        (type-string model "/greet World  again" :enter)
        (is (equal '("World  again" "ext-slash") (first calls)))
        (is (equal "hi World  again" (nle::status-said model)))
        (is-carrying (notice (nle::execute-slash-command model "boom" ""))
          ("/boom failed: ") ("kaboom" "the refusal names the command's own condition"))
        ;; A command the cell took back since the shell read the catalog.
        (nle:unregister-commands "demo-pack")
        (is (search "unknown command /greet" (nle::execute-slash-command model "greet" "")))))))

(deftest cell-command-answer-opens-its-dialog (with-temp-gateway (port))
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (register-picker-command)
    (let ((model (gateway-frame-model port "ext-dialog")))
      (is (nle::find-gateway-command "pick") "the command joins the catalog")
      (with-slash-dialog (dialog model "pick" "") "the answer's dialog opens here"
        (is-shape dialog (nle::picker-dialog-title "Pick (2)")
          (nle::picker-dialog-context "two of them")
          (nle::picker-dialog-hint "esc close  enter pick")
          (nle::picker-dialog-empty-label "Nothing to pick")
          (nle::picker-dialog-searchable is "typing filters it") (row-labels '("alpha" "beta"))
          (row-keys '("" "user   ") "the key column rides along"))
        (is (equal "the first one"
                   (nle::picker-row-detail (first (nle::picker-dialog-rows dialog)))))
        (let ((marked (second (nle::picker-dialog-rows dialog))))
          (is-shape marked (nle::picker-row-mark "• ") (nle::picker-row-mark-tone eq :success))))
      ;; Enter hands the highlighted row's value to the composer.
      (nle::picker-dialog-key model :enter)
      (is (null (nle::chat-model-dialog model)) "choosing closes the dialog")
      (is (equal "$alpha " (composer-value model))))))

(deftest cell-command-row-keys-run-their-lines-where-the-command-ran (with-temp-gateway (port))
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (let ((items (list (list (cons "a" t) (cons "b" t)))))
      (register-rows-command items)
      (let ((model (gateway-frame-model port "ext-rows")))
        (flet ((labels-and-details ()
                 (mapcar (lambda (row) (list (nle::picker-row-label row) (nle::picker-row-detail row)))
                         (nle::picker-dialog-rows (nle::chat-model-dialog model))))
               (title () (nle::picker-dialog-title (nle::chat-model-dialog model)))
               (selection () (nle::picker-dialog-selection (nle::chat-model-dialog model))))
          (with-slash-dialog (dialog model "rows" "") "the list opens"
            (is (equal '(("a" "on") ("b" "on")) (labels-and-details))))
          (dialog-keys model #\Space)
          (is (equal "a flipped" (nle::status-said model)) "Space ran the row's line, and the panel's key row says so")
          (is (equal '(("b" "on") ("a" "off")) (labels-and-details)) "and the list was read again")
          (is (= 1 (selection)) "the cursor on the row it moved, wherever the list put it")
          (dialog-keys model :enter)
          (is (equal "Row" (title)) "Enter opened the row's record over the list")
          (dialog-keys model :escape)
          (is (equal "Rows" (title)) "Esc came back to the list")
          (is (= 1 (selection)) "on the row the record was opened from")
          (dialog-keys model :enter :enter)
          (is (equal "Rows" (title)) "Enter on a row with nothing to run goes back too")
          (dialog-keys model :enter :down :enter)
          (is (equal "a dropped" (nle::status-said model)))
          (is (equal "Rows" (title)) "a record whose row is gone gives way to the list")
          (is (equal '("b") (row-labels (nle::chat-model-dialog model))))
          (dialog-keys model :escape)
          (is (null (nle::chat-model-dialog model)) "Esc on the list it all opened from closes it"))))))

(deftest cell-command-trigger-opens-its-dialog-at-a-word-start (with-temp-gateway (port))
  ;; A command registered with a TRIGGER answers that character typed at a
  ;; word's start -- the index's `$' opening /index, which this picker
  ;; stands in for: the character stays in the draft, the dialog opens, and
  ;; the pick takes its place. Mid-word it is text, and Esc leaves it as typed.
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (nle:unregister-commands "index")
    (register-picker-command :trigger "$")
    (let ((model (gateway-frame-model port "ext-trigger")))
      (is-shape (nle::find-gateway-command "pick")
        (nle::slash-command-trigger "$" "the trigger crosses the catalog"))
      (type-string model "costs a" #\$)
      (is (null (nle::chat-model-dialog model)) "mid-word it is text")
      (type-string model " " #\$)
      (is (nle::picker-dialog-p (nle::chat-model-dialog model)) "at a word's start it opens")
      (is (equal "costs a$ $" (composer-value model)) "with the character in the draft")
      (nle::picker-dialog-key model :enter)
      (is (equal "costs a$ $alpha " (composer-value model)) "the pick takes its place")
      (press model #\$)
      (nle::picker-dialog-key model :escape)
      (is (null (nle::chat-model-dialog model)))
      (is (equal "costs a$ $alpha $" (composer-value model)) "Esc leaves it as typed"))))

(deftest cell-command-without-a-dialog-answers-its-text (with-temp-gateway (port))
  ;; A command whose answer is its text alone opens nothing: the text is the
  ;; shell's notice, as a builtin's answer is.
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (nle:register-command
     "line-pack" "line"
     (constantly "just a line")
     :description "Answers with a line")
    (let ((model (gateway-frame-model port "ext-line")))
      (is (equal "just a line" (nle::execute-slash-command model "line" "")))
      (is (null (nle::chat-model-dialog model)) "no dialog, no panel"))))

(deftest cell-command-dialog-tabs-narrow-the-list (with-temp-gateway (port))
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (register-tabs-command)
    (let ((model (gateway-frame-model port "ext-tabs")))
      (with-slash-dialog (dialog model "scope" "") "the answer's dialog opens here"
        (is-table (expected key) (equal expected (mapcar (lambda (tab) (getf tab key))
                                                         (nle::picker-dialog-tabs dialog)))
          ('(3 2 1) :count) ('("all" "user" "project") :label))
        (is (zerop (nle::picker-dialog-tab dialog)) "and the first tab is the one selected")
        (is (null (nle::picker-dialog-tab-value dialog)) "whose all-rows tab filters nothing")
        (is (equal '("alpha" "beta" "gamma") (visible-labels dialog)))
        (dialog-keys model :tab)
        (is (equal "user" (nle::picker-dialog-tab-value dialog)) "Tab walks to the next tab")
        (is (equal '("alpha" "beta") (visible-labels dialog)))
        (is (zerop (nle::picker-dialog-selection dialog)) "the cursor comes home with the tab")
        (nle::picker-search-mutate dialog "gamma")
        (is (null (visible-labels dialog)))
        (nle::picker-search-mutate dialog "")
        (dialog-keys model :tab)
        (is (equal '("gamma") (visible-labels dialog)) "the project tab holds its one row")
        (dialog-keys model :tab)
        (is (null (nle::picker-dialog-tab-value dialog)) "and the strip wraps back around")
        (is (equal '("alpha" "beta" "gamma") (visible-labels dialog)))
        ;; The strip paints: every tab a chip carrying its count, two columns
        ;; apart, the selected one in the held chip -- in brackets where no
        ;; color can say it.
        (let ((segs (nle::docked-dialog-tab-segs dialog)))
          (is (equal '("[all  3]" "  " " user  2 " "  " " project  1 ")
                     (mapcar #'nle::seg-text segs)))
          (is (eq (nle::style-for-tone :sheet-chip-on) (nle::seg-tone (first segs))))
          (is (eq (nle::style-for-tone :sheet-chip) (nle::seg-tone (third segs)))))
        (let ((nle::*color-profile* :truecolor))
          (is (equal " all  3 " (nle::seg-text (first (nle::docked-dialog-tab-segs dialog)))) "a chip's fill says which is held"))
        ;; The row spends no column on the bucket the strip carries, and an
        ;; untabbed dialog's row still leads with its key.
        (let ((row (first (nle::picker-dialog-rows dialog))))
          (is (equal "alpha"
                     (nle::seg-text (first (nle::dialog-entry-segs row nil :tabbed t)))))
          (is (notany (lambda (seg) (string= "user" (nle::seg-text seg)))
                      (nle::dialog-entry-segs row nil :tabbed t)))
          (is (equal "user" (nle::seg-text (first (nle::dialog-entry-segs row nil)))))))
      ;; The strip goes with the dialog.
      (nle::picker-dialog-key model :enter)
      (is (null (nle::chat-model-dialog model)) "choosing closes it"))))

(deftest init-reads-the-gateway-commands-off-the-loop-thread (with-temp-gateway (port))
  ;; INIT installs no catalog itself — over the gateway the read is an HTTP
  ;; round trip the first frame must not wait on — it answers a command the
  ;; worker runs, whose message UPDATE installs; a read that fails answers
  ;; nothing, and the catalog in hand stays.
  (with-saved-globals (nle::*registered-commands* nle::*gateway-commands*)
    (nle:register-command "cmd-pack" "wave" (constantly nil) :description "Wave hello")
    (let* ((model (gateway-shell port "catalog-cmd"))
           (commands (nlt:init model)))
      (is (null (nle::find-gateway-command "wave")))
      (is (and (listp commands) (every #'functionp commands)))
      (let ((messages (remove nil (mapcar #'funcall commands))))
        (is (= 1 (count-if (lambda (msg) (typep msg 'nle::command-catalog-msg))
                           messages)))
        (dolist (msg messages)
          (when (typep msg 'nle::command-catalog-msg)
            (nle:update model msg)))
        (is (nle::find-gateway-command "wave"))))
    ;; The attach shell with no gateway to ask: the read fails,
    ;; the command answers nothing, the catalog stays.
    (let ((read (nle::read-command-catalog-cmd (detached-shell "catalog-cmd-gw"))))
      (is (null (funcall read)) "a failed read answers no message")
      (is (nle::find-gateway-command "wave")))))

(deftest gateway-quiet-notice-stands-and-is-sent-to-no-shell (with-gateway-session (port "quiet-s"))
  ;; Without :quiet, a channel adapter installed with no section would draw a
  ;; card on every launch. A :quiet notice stands -- the model, /cells and
  ;; (cells) read the board -- and no shell is sent it, attached now or later;
  ;; it is not in the log of what was said.
  (let ((frames (frames-while (ws port :subscribe '("quiet-s"))
                  (nle:notice "discord: unconfigured — quiet probe"
                              :level :quiet :key "channels.discord")
                  (nle:notice "telegram: refused — loud probe"
                              :level :error :key "channels.telegram"))))
    (is (null (frame-carrying frames "discord: unconfigured — quiet probe" "text")))
    (is (frame-carrying frames "telegram: refused — loud probe" "text") "a refusal was"))
  ;; Both stand; a shell attaching later is sent the board but the quiet one.
  (is (equal '(("channels.discord" "discord: unconfigured — quiet probe" :quiet)
               ("channels.telegram" "telegram: refused — loud probe" :error))
             (nlk:standing-notices)))
  (is (equal '("telegram: refused — loud probe")
             (mapcar (lambda (frame) (nlk:json-value frame :string "payload" "text"))
                     (frames-of-type (subscribed-frames port "quiet-s") "organism.notice"))))
  (is (not (find "discord: unconfigured — quiet probe" (nlk:notice-log) :key #'first :test #'equal)))
  (signals-error error (nle:notice "said to no one, standing nowhere" :level :quiet))
  (nle:notice nil :key "channels.discord")
  (nle:notice nil :key "channels.telegram"))

(deftest gateway-generations-routes-list-read-ahead-and-restore (with-cell-fixture (root))
  ;; The list, the read ahead and the restore as a client reaches them: GET
  ;; answers the history, the generation the folder stands at and the edits
  ;; that hold a restore; GET plan says what a restore would do before
  ;; anything moves; POST converges and takes the one idle exit.
  (with-temp-trees ((cache "warm-cache"))
    (with-prover-fixture (prover)
      ;; SETF, not LET: the routes run on the gateway's threads, which see
      ;; the globals and not this thread's fixture bindings.
      (with-saved-globals ((nlk::*cells-directory* root) (nlk::*warm-cache-directory* cache)
                           (nlk::*image-stamp* "stamp-w")
                           (nlk::*organism-command* (list "/bin/sh" prover)))
        (write-layer-folder root "nl-warm" '(("probe" . "(defun cells-probe-warm () :warm)")))
        (let ((exited '()))
          (is (eq :sealed (nlk:prove-self (nlk::folder-key))))
          (with-temp-gateway (port)
            (with-saved-globals ((nle::*restart-wait* nil) (nle::*idle-poll-seconds* 0.05)
                                 (nle::*restart-exit* (lambda (to) (push to exited))))
              (with-stubbed-fdefinition (nlk:any-active-turn-p () nil)
                (with-gateway-http (port :get "/api/gateway/generations" :status 200)
                  (is (eql 1 (nlk::json-value body :integer "now")) "the folder stands at 1")
                  (is (zerop (length (nlk:json-array body "edits"))))
                  (is-present (change (aref (nlk:json-array body "changes") 0)) "one change, the first seal"
                    (is (equal "first" (nlk::json-value change :string "kind")))
                    (is (eql 1 (nlk::json-value change :integer "to")))
                    (is (equal "1 folder · nodecode" (subseq (nlk::json-value change :string "where") 0 19)))
                    (is (null (nlk::json-value change :string "refusal")) "it can come back")))
                (with-gateway-http (port :get "/api/gateway/generations/plan?to=1" :status 200)
                  (is (zerop (length (nlk:json-array body "lines"))) "the folder stands at 1: nothing moves")
                  (is (null (nlk::json-value body :string "refusal"))))
                (with-gateway-http (port :get "/api/gateway/generations/plan?to=7")
                  (is (= 400 status) "a generation nothing sealed")
                  (is (search "no generation 7" (nlk::json-value body :string "error" "message"))))
                (with-gateway-http (port :post "/api/gateway/generations/rollback" :body "{\"number\": 1}")
                  (is (= 200 status))
                  (is (search "generation 1" (nlk::json-value body :string "text"))))
                (is (null (nle::held-release)) "a restore holds nothing")
                (bt2:join-thread nle::*restart-wait*)
                (is (equal '("generation 1") exited) "the one idle exit, once")))))))))

(deftest the-failed-self-is-handed-to-the-layer-session-once-per-death (with-temp-gateway (port))
  ;; A boot that runs pristine after a death hands the layer session the
  ;; floor and the list, once per death.
  (let ((handed '()))
    (with-temp-directory (cells "pristine-cells")
      (with-saved-globals ((nlk:*pristine* t) (nlk::*cells-directory* cells))
        (with-stubbed-fdefinition (nle:submit (session-id prompt &rest keys)
                                   (push (cons session-id prompt) handed)
                                   nil)
          (is (null (nle::hand-failed-self)) "no marker: nothing to hand")
          (nlk::write-death-marker "k1" 1 "the self loaded under key k1 died before it served")
          (is (eql 1 (nle::hand-failed-self)))
          (is (equal "layer" (car (first handed))))
          (is-carrying (text (cdr (first handed)))
            ("died before it served" "the death line") ("generation 0" "the floor") ("(rollback N)" "the way back"))
          (is (null (nle::hand-failed-self)) "handed once")
          (nlk::write-death-marker "k1" 2 "twice")
          (is (eql 2 (nle::hand-failed-self)) "a new death is handed again")
          (is (= 2 (length handed))))))))

(deftest mutation-announcement-carries-the-existing-snippet-note-and-clears-its-scope ()
  (let ((seen nil))
    (with-stubbed-fdefinition (nle::broadcast-organism (kind payload)
                               (push (cons kind payload) seen)
                               (values t 1))
      (nle::eval-string (format nil ";; Fold finished work~%(announce-loaded #p\"/tree/probe.lisp\" '((\"DEFUN\" . \"probe\")))"))
      (is (equal "Fold finished work" (nlk:json-value (cdar seen) :string "comment")))
      (nle::announce-loaded #p"/tree/probe.lisp" '(("DEFUN" . "probe")))
      (is (null (nlk:json-value (cdar seen) :any "comment")) "the previous snippet's note never leaks"))))

(deftest loaded-mutation-comment-folds-into-a-live-only-summarized-system-row
    (with-loaded-probe (model path "s-mutation" "compact-mutation.lisp"
                        "(defun compact-mutation-probe () :live)~%"))
  (feed-wire model
    (nlk:encode-unsequenced-frame "organism.loaded"
      (nlk:json-object "path" (namestring path)
         "definitions" (vector (nlk:json-object "operator" "DEFUN" "label" "compact-mutation-probe"))
         "session_id" "s-mutation" "comment" "Fold finished work")))
  (is (eq :live (funcall (uiop:find-symbol* "COMPACT-MUTATION-PROBE" '#:nodecode.evolved))))
  (let* ((row (find :system-notice (nle::transcript-rows (nle::chat-model-state model)) :key #'nle::transcript-row-kind))
         (summary (nlk:json-value (nle::transcript-row-metadata row) :object "summary")))
    (is (eq :live-only (nle::transcript-row-retention row)))
    (is-shape summary ((:string "name") "Mutate") ((:string "comment") "Fold finished work"))
    (is (search "compact-mutation-probe" (nle::transcript-row-text row)))))

(deftest loaded-mutation-without-a-comment-names-the-file-not-a-fabricated-plan
    (with-loaded-probe (model path "s-mutation" "no-comment.lisp"
                        "(defun no-comment-probe () :live)~%"))
  (feed-wire model (loaded-frame (namestring path) "no-comment-probe" "s-mutation"))
  (let* ((row (car (last (nle::transcript-rows (nle::chat-model-state model)))))
         (summary (nlk:json-value (nle::transcript-row-metadata row) :object "summary")))
    (is (equal "no-comment.lisp" (nlk:json-value summary :string "comment")))
    (is (eq :live-only (nle::transcript-row-retention row)))))

(deftest a-held-mutation-is-a-visible-diagnostic-not-a-folded-success
    (with-loaded-probe (model path "s-mutation" "held-mutation.lisp"
                        "(defparameter *held-mutation-probe* (error \"rewrite refused\"))~%"))
  (feed-wire model
    (nlk:encode-unsequenced-frame "organism.loaded"
      (nlk:json-object "path" (namestring path)
        "definitions" (vector (nlk:json-object "operator" "DEFPARAMETER" "label" "*held-mutation-probe*"))
        "session_id" "s-mutation" "comment" "Attempt rewrite")))
  (let* ((row (car (last (nle::transcript-rows (nle::chat-model-state model)))))
         (lines (nle::display-block-plain-lines (nle::row-display-block row :width 100))))
    (is (nle::transcript-row-error-p row))
    (is (rows-carry-p "rewrite refused" lines))
    (is (not (rows-carry-p "Mutate" lines)))
    (is (not (nle::togglable-item-p model row)))))

(deftest mutation-comment-reaches-an-attached-peer-through-the-source-loaded-hook ()
  (with-gateway-session (port "mutate-peer")
    (let* ((frames (frames-while (ws port :subscribe '("mutate-peer") :pid 424244)
                     (await (:tries 50) (nle::attached-shells))
                     (let ((nle::*current-durable-turn*
                             (nlk::%make-durable-turn :session-id "mutate-peer" :turn-id "mutate-turn"
                                                      :command-id "c" :definition-revision "base")))
                       (nle::eval-string
                         (format nil ";; Fold finished work~%(nlk:source-loaded #p\"/tree/probe.lisp\" '((\"DEFUN\" . \"probe-a\")))")))))
           (frame (find-frame frames "organism.loaded")))
      (is frame)
      (is-shape frame ((:string "payload" "comment") "Fold finished work")
        ((:string "payload" "session_id") "mutate-peer") ("sequence" eql 0)))))
