;;;; protocol-test.lisp --- canonical Gateway sync contract.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (command nlk::gateway-command) (decoded nlk::gateway-command) (frame nlk::sync-message))

(defun connect-payload (nonce &rest keys)
  "The decoded connect a linux tui client sends answering NONCE; KEYS reach the encoder."
  (shasht:read-json (apply #'nlk:encode-gateway-connect nonce "client" "1" "tui"
                           "linux" "instance" keys)))

(defun command-wire (kind request &rest keys)
  "KIND's client command over REQUEST as the wire carries it; KEYS reach MAKE-GATEWAY-COMMAND."
  (nlk:encode-client-command (apply #'nlk:make-gateway-command :kind kind :request request keys)))

(deftest protocol-connect-challenge-round-trips ()
  (let ((frame (nlk:decode-sync-message (nlk:encode-gateway-connect-challenge "nonce-1"))))
    (is-shape frame (.type eq :gateway-connect-challenge) (.protocol-version = 1))
    (is (equal "nonce-1"
               (gethash "challenge_nonce" frame.payload)))))

(deftest protocol-connect-validates-challenge-client-and-scopes ()
  (let* ((payload (connect-payload "nonce-1" :operator-token "test-token"))
         (scopes (nlk::validate-gateway-connect payload "nonce-1" "test-token")))
    (is (equal '(("sync.read" "sync.write")) (list scopes)))))

(deftest protocol-connect-authorized-at-the-door-needs-no-token ()
  ;; The socket's upgrade carried the operator's Bearer (gateway.lisp).
  (is (equal '("sync.read" "sync.write")
             (nlk::validate-gateway-connect (connect-payload "nonce-1") "nonce-1" "test-token"
                                            :authorized t))))

(deftests protocol-connect-refuses (payload nonce token)
  (is (signals-error nlk:protocol-error (nlk::validate-gateway-connect payload nonce token)))
  (challenge-mismatch (connect-payload "wrong") "expected" nil)
  (invalid-operator-token (connect-payload "nonce-1" :operator-token "wrong") "nonce-1" "expected"))

(deftest protocol-command-uses-canonical-envelope ()
  (let* ((request (nlk:json-object "session_id" "session-a"
                                  "prompt" "hello"
                                  "generate_response" t))
         (wire (command-wire :start-turn request :command-id "cmd-1" :correlation-id "corr-1"))
         (json (shasht:read-json wire))
         (command (nlk:sync-message-payload (nlk:decode-sync-message wire))))
    (is (equal "client_command" (gethash "type" json)))
    (is-shape command (.kind eq :start-turn) (.command-id "cmd-1") (.correlation-id "corr-1"))
    (is (equal "hello"
               (gethash "prompt" command.request)))))

(deftest protocol-projection-carries-sequence-cursor-and-type ()
  (let* ((payload (nlk:json-object "turn_id" "turn-1"))
         (wire (nlk:encode-projection-event "session-a" 12 "turn_started" payload
                                            :cursor 44 :durable-p t))
         (json (shasht:read-json wire))
         (decoded (nlk:decode-sync-message wire)))
    (is (= 12 (gethash "sequence" json)) "sequence is outer envelope state")
    (is (= 44 (gethash "cursor" json)) "cursor is explicit")
    (is (eq :projection (nlk:sync-message-type decoded)) "projection has typed kind")
    (is (equal "turn_started"
               (gethash "type" (nlk:sync-message-payload decoded))))))

(deftest protocol-accepts-global-command-result-without-session ()
  (let ((frame
          (nlk:decode-sync-message
           "{\"sequence\":0,\"payload\":{\"type\":\"gateway_turn_command_result\",\"result\":{\"session_id\":\"session-a\",\"command_id\":\"cmd-a\",\"correlation_id\":\"corr-a\",\"command\":\"start_turn\",\"status\":\"rejected\",\"code\":\"turn_busy\",\"message\":\"busy\"}}}")))
    (is-shape frame (.type eq :projection) (.session-id null))))

(deftest protocol-refuses-malformed-sync-messages ()
  ;; One test, one case per hostile message: each case keeps its own
  ;; description, so a regression still names the refusal it lost.
  (is-table (json) (signals-error nlk:protocol-error (nlk:decode-sync-message json))
    ("{\"type\":\"client_command\",\"command\":{\"type\":\"steer_turn\",\"command_id\":\"c\",\"correlation_id\":\"x\"}}"
     "undeployed steer command is not advertised")
    ("{\"type\":\"client_command\",\"command\":{\"type\":\"future_command\",\"command_id\":\"c\",\"correlation_id\":\"x\"}}"
     "unknown commands fail with structured protocol error")
    ("{\"type\":\"gateway_connect_challenge\",\"protocol_min\":2,\"protocol_max\":2,\"challenge_nonce\":\"n\"}"
     "unsupported protocol ranges fail closed")
    ("{\"sequence\":1,\"session_id\":\"s\",\"payload\":{\"type\":\"future_projection\"}}"
     "unknown projection discriminators fail closed")
    ("{\"sequence\":-1,\"session_id\":\"s\",\"payload\":{\"type\":\"turn_started\"}}")
    ("{\"sequence\":1,\"session_id\":\"\",\"payload\":{\"type\":\"turn_started\"}}")
    ("{\"sequence\":1,\"session_id\":\"s\",\"cursor\":-2,\"payload\":{\"type\":\"turn_started\"}}")
    ("{\"sequence\":2,\"payload\":{\"type\":\"gateway_turn_command_result\",\"result\":{\"session_id\":\"s\"}}}")))

(deftest protocol-accepts-zig-session-stream-event ()
  (let ((frame (nlk:decode-sync-message
                "{\"sequence\":7,\"session_id\":\"session-a\",\"payload\":{\"type\":\"session_stream_event\",\"session_id\":\"session-a\",\"frame\":{\"seq\":7,\"session_id\":\"session-a\",\"turn_id\":\"turn-a\",\"event\":{\"type\":\"turn_completed\"}}}}")))
    (is (eq :projection frame.type))
    (is (equal "session_stream_event"
               (gethash "type" frame.payload)))))

(deftest protocol-normalizes-zig-content-delta ()
  (let* ((content (vector (nlk::make-json-object "type" "text" "text" "hello")))
         (request (nlk:json-object "session_id" "s" "content" content)))
    (is (equal "hello" (gethash "prompt" (nlk::normalize-gateway-request :start-turn request))))))

(deftest protocol-replay-uses-canonical-accumulated-frames ()
  (let* ((wire (nlk:encode-session-stream-resumed
                "session-replay" 8 #() :admission-state "ready"))
         (json (shasht:read-json wire))
         (frame (nlk:decode-sync-message wire))
         (payload frame.payload))
    (is (= 8 frame.sequence) "replay preserves sequence watermark")
    (is (equal "session_stream_resumed" (gethash "type" payload)))
    (is (vectorp (gethash "accumulated_frames" (gethash "payload" json))))
    (is (= 0 (length (gethash "accumulated_frames" payload))))))

(deftest protocol-inner-event-preserves-metadata-and-unknown-types ()
  ;; DECODE-INNER-EVENT is called for its refusal, never for a carrier: the
  ;; envelope it validates is what the client folds. An unknown discriminator
  ;; therefore passes through whole — a foreign gateway's newer fact must not
  ;; cost the replay frame it rode in.
  (let* ((known (nlk::encode-inner-event
                 "item_delta"
                 (nlk::make-json-object "text" "hello")
                 :metadata (nlk::make-json-object "source" "provider")))
         (unknown (nlk::encode-inner-event
                   "future_event_kind"
                   (nlk::make-json-object "opaque" t)
                   :metadata (nlk::make-json-object "revision" 3)))
         (wire (format nil
                       "{\"sequence\":1,\"session_id\":\"s\",\"payload\":{\"type\":\"session_stream_resumed\",\"session_id\":\"s\",\"admission_state\":\"ready\",\"accumulated_frames\":[{\"seq\":1,\"event\":~a},{\"seq\":2,\"event\":~a}]}}"
                       (nlk::encode-json-object known) (nlk::encode-json-object unknown)))
         (frame (nlk:decode-sync-message wire)))
    (is (eq :projection frame.type) "typed inner events decode in replay")
    (let ((events (nlk::projection-events frame.payload)))
      (is (equal "provider"
                 (nlk:json-value (aref events 0) :text "metadata" "source")))
      (is (equal "future_event_kind"
                 (nlk:json-value (aref events 1) :text "type")))
      (is (equal 3 (nlk:json-value (aref events 1) :integer "metadata" "revision"))))))

(deftest protocol-subscription-command-carries-the-shell-pid ()
  ;; The shell's process rides its subscription both ways through the codec:
  ;; the encoder kept only the subscriptions, so every
  ;; attached shell read "pid unknown" in the image section, and a source
  ;; load could not tell the owner's own shell from one in another process.
  (let ((request (nlk:json-object "subscriptions" (vector (nlk:json-object "session_id" "s"))
                                 "pid" 424242)))
    (let* ((wire (command-wire :set-session-subscriptions request))
           (decoded (nlk:sync-message-payload (nlk:decode-sync-message wire))))
      (is (eql 424242 (nlk:json-value (shasht:read-json wire) :integer "command" "pid")))
      (is (eql 424242 (gethash "pid" decoded.request)))))
  (let ((decoded (nlk:sync-message-payload
                  (nlk:decode-sync-message
                   "{\"type\":\"client_command\",\"command\":{\"type\":\"set_session_subscriptions\",\"subscriptions\":[{\"session_id\":\"s\"}],\"pid\":\"x\"}}"))))
    (is (eq :set-session-subscriptions decoded.kind) "a subscription decodes")
    (is (null decoded.command-id) "with no command identity")
    (is (null (gethash "pid" decoded.request)))))

;;; --- user image attachments -------------------------------------------------
;;; The wire carries a turn's images as image items in the prompt's content
;;; array, the Zig appendPromptContentItems / parseImageAttachments shape. The
;;; codec is the only surface that speaks it: above it, images are
;;; (MEDIA-TYPE . BASE64) conses in the request's `images` field.

(deftest protocol-start-turn-carries-images-in-content ()
  (let* ((request (nlk::make-json-object
                   "session_id" "session-a"
                   "prompt" "look at this"
                   "images" (list (cons "image/png" "QUJD"))))
         (wire (command-wire :start-turn request :command-id "cmd-1" :correlation-id "corr-1"))
         (json (shasht:read-json wire))
         (content (gethash "content" (gethash "request" (gethash "command" json))))
         (command (nlk:sync-message-payload (nlk:decode-sync-message wire)))
         (decoded command.request))
    (is (= 2 (length content)) "the text item leads and the image follows")
    (is-table (expected key object) (equal expected (gethash key object))
      ("text" "type" (aref content 0) "the prompt text is the first content item")
      ("image" "type" (aref content 1) "the attachment rides as an image content item")
      ("base64" "type" (gethash "source" (aref content 1))
       "an encoded attachment rides as a base64 source")
      ("image/png" "media_type" (gethash "source" (aref content 1)) "the media type rides the source")
      ("QUJD" "data" (gethash "source" (aref content 1)) "the payload rides the source"))
    (is (null (gethash "images" (gethash "request" (gethash "command" json)))))
    (is-shape decoded ("prompt" "look at this") ("images" '(("image/png" . "QUJD"))))))

(deftest protocol-image-attachments-skip-unusable-items ()
  (flet ((image (&rest source)
           (nlk::make-json-object "type" "image"
                                  "source" (apply #'nlk::make-json-object source))))
    (let* ((content (vector (nlk::make-json-object "type" "text" "text" "hi")
                            ;; no source at all
                            (nlk::make-json-object "type" "image")
                            ;; a source shape this wire does not speak
                            (image "type" "url" "url" "http://x/y.png")
                            ;; the retired `file` source: a path is data now,
                            ;; never something the gateway goes and reads
                            (image "type" "file" "path" "/tmp/notes.png")
                            (image "type" "base64" "media_type" "image/jpeg" "data" "Zm9v")))
           (normalized (nlk::normalize-gateway-request
                        :start-turn
                        (nlk::make-json-object "session_id" "s" "content" content))))
      (is-shape normalized ("prompt" "hi") ("images" '(("image/jpeg" . "Zm9v")))))))

(defun loaded-frame (path label &optional session-id)
  "The organism.loaded frame naming one DEFUN, LABEL, of the file at PATH, made
live by SESSION-ID's turn when one is named."
  (nlk:encode-unsequenced-frame
   "organism.loaded" (nlk:json-object "path" path
                                      "definitions" (vector (nlk:json-object "operator" "DEFUN"
                                                                             "label" label))
                                      :opt "session_id" session-id)))

(deftests protocol-organism (wire checks refusals)
  (let* ((frame (nlk:decode-sync-message wire))
         (payload (nlk:sync-message-payload frame)))
    (is (eq :projection (nlk:sync-message-type frame)))
    (is (eql 0 (nlk:sync-message-sequence frame)) "outside every session's sequence")
    (is (null (nlk:sync-message-session-id frame)) "and belongs to no session")
    checks
    (dolist (refused refusals)
      (is (signals-error nlk:protocol-error (nlk:decode-sync-message refused)))))
  (notice-is-unsequenced-and-session-less
   (nlk:encode-unsequenced-frame "organism.notice"
    (nlk:json-object "text" "discord: refused — x" "level" "error"
                     "key" "channels.discord"))
   (is-table (expected key) (equal expected (gethash key payload))
     ("organism.notice" "type")
     ("discord: refused — x" "text")
     ("error" "level")
     ("channels.discord" "key"))
   '("{\"sequence\": 0, \"payload\": {\"type\": \"organism.notice\"}}"))
  (loaded-is-unsequenced-and-session-less
   (loaded-frame "/tree/probe.lisp" "probe")
   (progn
     (is-shape payload ("type" "organism.loaded") ("path" "/tree/probe.lisp"))
     (is (equal "probe" (gethash "label" (aref (gethash "definitions" payload) 0)))))
   '("{\"sequence\": 0, \"payload\": {\"type\": \"organism.loaded\", \"definitions\": []}}"
     "{\"sequence\": 0, \"payload\": {\"type\": \"organism.loaded\", \"path\": \"/x.lisp\"}}"))
  (loaded-names-the-session-that-made-it-live-in-its-payload
   (loaded-frame "/tree/probe.lisp" "probe" "s-editor")
   (is (equal "s-editor" (gethash "session_id" payload)))
   '("{\"sequence\": 0, \"payload\": {\"type\": \"organism.loaded\", \"path\": \"/x.lisp\", \"definitions\": [], \"session_id\": 7}}"
     "{\"sequence\": 0, \"payload\": {\"type\": \"organism.loaded\", \"path\": \"/x.lisp\", \"definitions\": [], \"session_id\": \"\"}}"))
  (activity-frame-decodes-session-less-and-needs-sessions
   (nlk:encode-unsequenced-frame "organism.activity"
    (nlk:json-object
     "sessions" (vector (nlk:json-object "id" "s"
                                         "started_at" "2026-09-06T10:00:00.000Z"))))
   (progn
     (is (equal "organism.activity" (gethash "type" payload)))
     (is (equal "s" (nlk:json-value (aref (gethash "sessions" payload) 0) :string "id"))))
   '("{\"sequence\": 0, \"payload\": {\"type\": \"organism.activity\"}}")))

(deftest protocol-every-unsequenced-kind-decodes (dolist (kind nlk:+unsequenced-frame-kinds+))
  ;; THE TWO ENDS READ ONE LIST. A kind a producer may broadcast and the
  ;; decoder does not admit reaches a shell as `unsupported projection type'
  ;; and is dropped — which is what extensions_changed (now commands_changed)
  ;; did from the day it was published until this test. Session-less is the
  ;; other half: a kind missing
  ;; from the session-less set is refused for having no session instead.
  (let* ((probe (assoc kind '(("organism.notice" "text" "x")
                              ("organism.notices" "notices" #())
                              ("organism.note" "text" "x")
                              ("organism.cells" "names" #())
                              ("organism.activity" "sessions" #())
                              ("organism.loaded" "path" "/x.lisp" "definitions" #()))
                       :test #'string=))
         (wire (nlk:encode-unsequenced-frame kind (apply #'nlk:make-json-object (rest probe))))
         (frame (handler-case (nlk:decode-sync-message wire)
                  (nlk:protocol-error (condition) (princ-to-string condition)))))
    (is (not (stringp frame)) (format nil "~a decodes: ~a" kind frame))
    (unless (stringp frame)
      (is-shape frame (.type eq :projection) (.sequence eql 0) (.session-id null))
      (is (equal kind (gethash "type" frame.payload))))))

(deftest protocol-frame-bound-refuses-past-the-bound ()
  ;; The one bound both ends of the transport measure against: at it the
  ;; frame passes through unchanged; one past it the refusal names both sizes.
  (let ((at (make-string nlk:+max-sync-frame-bytes+ :initial-element #\a
                                                     :element-type 'base-char))
        (past (make-string (1+ nlk:+max-sync-frame-bytes+) :initial-element #\a
                                                           :element-type 'base-char)))
    (is (eq at (nlk:check-sync-frame-size at)) "a frame at the bound passes through")
    (is-present (condition (signals-error nlk:frame-too-large (nlk:check-sync-frame-size past)))
        "a frame one past the bound is refused"
        (is (eq :frame_too_large (nlk:protocol-error-code condition)))
        (is (= (1+ nlk:+max-sync-frame-bytes+) (nlk:frame-too-large-bytes condition)))
        (is (= nlk:+max-sync-frame-bytes+ (nlk:frame-too-large-bound condition)))
        (is (search "16 MiB sync frame bound" (nlk:protocol-error-message condition))))))

(defun protocol-schema-defs ()
  "The $defs of protocol.schema.json, the checked-in wire beside the protocol's owner."
  (gethash "$defs" (nlk:decode-json (uiop:read-file-string
                                     (asdf:system-relative-pathname
                                      "nodecode" "src/wire/protocol.schema.json")))))

(deftest protocol-schema-names-every-kind-the-code-does ()
  ;; A program reads the schema instead of this source, so its kind lists are
  ;; the code's, in the code's order: a kind added on one side alone is red.
  (let ((defs (protocol-schema-defs))
        (commands (mapcar #'car nlk::+gateway-command-kinds+)))
    (is (equal commands (coerce (nlk:json-value defs :array "CommandType" "enum") 'list)))
    ;; One command shape per kind, each named by its own type.
    (is (null (set-exclusive-or
               commands
               (loop for entry across (nlk:json-value defs :array "Command" "oneOf")
                     for name = (subseq (gethash "$ref" entry) (length "#/$defs/"))
                     collect (nlk:json-value defs :string name "properties" "type" "const"))
               :test #'equal)))
    (is (equal (append nlk::+session-frame-kinds+ nlk:+unsequenced-frame-kinds+)
               (coerce (nlk:json-value defs :array "Payload" "properties" "type" "enum") 'list)))))

(deftest protocol-schema-names-the-start-turn-target ()
  ;; A program finds the members a fresh session's target rides in the schema,
  ;; beside the prompt (gateway.lisp PIN-START-TURN-TARGET).
  (let ((members (nlk:json-value (protocol-schema-defs) :object
                                 "StartTurn" "properties" "request" "properties")))
    (is (and members (every (lambda (key) (gethash key members)) '("provider" "model" "effort"))))))

(deftest protocol-resume-and-state-ride-the-wire ()
  ;; A resumed replay says the position it resumed past; get_state decodes as
  ;; a flat command naming its session.
  (let ((frame (nlk:decode-sync-message
                (nlk:encode-session-stream-resumed "s" 3 #() :cursor 9 :admission-state "ready"
                                                             :replay-chunk-final-p t
                                                             :resumed-after 7))))
    (is (eql 7 (gethash "resumed_after" frame.payload)))
    (is (eql 9 frame.cursor)))
  (let ((command (nlk::decode-command
                  (nlk:decode-json "{\"type\":\"get_state\",\"command_id\":\"c\",\"correlation_id\":\"c\",\"session_id\":\"s\"}"))))
    (is-shape command (.kind eq :get-state))
    (is (equal "s" (gethash "session_id" command.request)))))

(deftest organism-loaded-comment-is-typed-and-optional ()
  (let* ((frame (nlk:decode-sync-message
                 (nlk:encode-unsequenced-frame "organism.loaded"
                   (nlk:json-object "path" "/tree/probe.lisp" "definitions" #()
                                    "comment" "Fold finished work"))))
         (payload (nlk:sync-message-payload frame)))
    (is (equal "Fold finished work" (gethash "comment" payload))))
  (dolist (value '(7 "" #("wrong")))
    (is (signals-error nlk:protocol-error
          (nlk:decode-sync-message
           (nlk:encode-unsequenced-frame "organism.loaded"
             (nlk:json-object "path" "/tree/probe.lisp" "definitions" #() "comment" value)))))))
