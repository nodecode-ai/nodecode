;;;; invocation-test.lisp --- canonical invocation provenance tests.

(in-package #:nodecode.test)

(deftest invocation-serializes-zig-shaped-context ()
  (let* ((value (test-invocation
                 :source (nlk:make-invocation-source "gateway" :id "session-1")
                 :transport (nlk:make-invocation-transport
                             "websocket" :connection-id "connection-1")))
         (json (nlk:invocation-json-object value)))
    (is-shape json ("action" "turn.start") ("command_id" "command-1")
      ((:string "source" "kind") "gateway")
      ((:string "transport" "connection_id") "connection-1"))))

(deftests invocation-rejects (form)
  (is (signals-error nlk:invocation-error form))
  (plist-at-construction (test-invocation :source '(:kind "gateway")
                                          :transport (nlk:make-invocation-transport "websocket")))
  (command-identity-mismatch (nlk:validate-invocation (test-invocation) :command-id "command-2")))

(deftest invocation-admission-persists-canonical-json (with-session-store ("invocation-session"))
  (nlk:admit-turn "invocation-session" "command-1" "hello" :invocation (test-invocation))
  (let* ((event (session-event "invocation-session" :kind "turn.started"))
         (json (shasht:read-json event.invocation-json)))
    (is (equal "turn.start" (gethash "action" json)))))
