;;;; package.lisp --- NODECODE-CHANNEL-SLACK package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Slack channel adapter: Socket Mode frames (socket.lisp), Slack events
;;;; as the kit's candidates (events.lisp), Web API plans and the executor
;;;; (rest.lisp), what the tokens can see (probe.lisp), and the platform plus
;;;; the socket lap the kit's host runs over (adapter.lisp). The room/lane
;;;; topology, the gate and the digest delivery are the kit's.

(defpackage #:nodecode-channel-slack
  (:documentation "Slack channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:ncs)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing Web API call, the raw-event seam, and the platform
   #:start-channel #:request #:slack-handle-event #:slack-platform #:slack-where-text
   ;; probe.lisp --- what the tokens can see; the section declaration beside
   ;; it is read through the kernel
   #:probe-channel #:probe-channel-choices #:probe-user-choices
   ;; socket.lisp --- pure
   #:read-socket-frame #:ack-frame
   ;; events.lisp --- pure, exported for table-driven tests
   #:slack-plain-text #:slack-mention-p #:slack-strip-mention
   #:slack-message-candidate #:slack-command-candidate #:slack-control-press
   #:slack-press-candidate #:slack-inbound-policy
   ;; rest.lisp
   #:make-thread-book #:note-thread-message #:bot-thread-p #:reply-thread #:pressed-words
   #:message-plan #:edit-plan #:delete-plan #:typing-plan #:reaction-plans
   #:respond-plan #:file-plan #:upload-plan #:slack-message-id #:slack-address
   #:wrap-slack-executor #:make-slack-executor))
