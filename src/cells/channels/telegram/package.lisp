;;;; package.lisp --- NODECODE-CHANNEL-TELEGRAM package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Telegram channel adapter: getUpdates long-polling (poll.lisp), Bot
;;;; API request plans and failure classification (rest.lisp), and the
;;;; platform plus the poll lap the kit's host runs over (adapter.lisp). The
;;;; room/lane topology, the gate and the digest delivery are the kit's.

(defpackage #:nodecode-channel-telegram
  (:documentation "Telegram channel adapter for the Lisp organism.")
  (:use #:cl #:nodecode-channel-kit)
  (:nicknames #:nct)
  (:export
   ;; adapter.lisp --- the channel entry (kit host convention), the
   ;; model-facing Bot API call, the raw-update seam, and the platform
   #:start-channel #:request #:telegram-handle-update #:telegram-platform
   #:telegram-strip-mention #:telegram-where-text
   ;; probe.lisp --- what the token can see; the section declaration
   ;; (nlk:define-section) beside it is read through the kernel
   #:probe-channel #:probe-chat-choices #:probe-user-choices
   ;; poll.lisp --- pure, exported for table-driven tests
   #:map-telegram-message #:map-telegram-press #:next-update-offset #:+default-allowed-updates+
   #:get-updates-plan #:telegram-inbound-policy
   ;; rest.lisp
   #:send-message-plan #:press-response-plan #:telegram-file-plan #:edit-message-plan
   #:delete-message-plan #:typing-plan #:reaction-plans #:answer-callback-plans
   #:read-back-path #:telegram-acknowledgement-p #:set-my-commands-plan
   #:telegram-command-name-p #:get-file-plan #:telegram-file-url
   #:telegram-message-id #:telegram-address #:telegram-retry-after-ms
   #:telegram-failure-message #:telegram-not-modified-p #:wrap-telegram-executor
   ;; board.lisp --- the thread board: the topics the bot opened, as it saw
   ;; them, and the message it keeps; exported for tests
   #:start-telegram-board #:*telegram-board* #:board-noting-executor
   #:note-telegram-write #:note-telegram-update #:telegram-board-pass
   #:telegram-board-pressed #:telegram-board-text #:telegram-board-line
   #:telegram-board-rows #:telegram-board-topics #:topic-key))
