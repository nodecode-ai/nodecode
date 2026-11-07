;;;; socket.lisp --- Slack Socket Mode: one frame read into what to do. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Socket Mode is how a Slack app hears its workspace without a public
;;;; address. apps.connections.open, called with the app-level token, answers
;;;; a one-use wss URL; every event, slash command and button press arrives on
;;;; that socket as an envelope, which the app acknowledges by its id within
;;;; three seconds or Slack sends it again. Slack refreshes the socket on its
;;;; own schedule and says so first with a `disconnect' frame; the lap then
;;;; opens a new one. The I/O is adapter.lisp's.
;;;;
;;;; Actions are tagged lists: (:hello) (:ack ENVELOPE-ID)
;;;; (:envelope TYPE PAYLOAD) (:reconnect REASON) (:fatal REASON).

(in-package #:nodecode-channel-slack)

(defun ack-frame (envelope-id)
  "The frame that acknowledges ENVELOPE-ID."
  (nlk:json-object "envelope_id" envelope-id))

(defun read-socket-frame (frame &aux (type (nlk:json-value frame :string "type"))
                                     (id (nlk:json-value frame :string "envelope_id")))
  "The actions one decoded Socket Mode FRAME asks for, in order."
  ;; An envelope is acknowledged before anything reads it, whatever it holds:
  ;; one this adapter does not act on is still one Slack must stop resending.
  ;; A disconnect other than link_disabled (Socket Mode turned off for the
  ;; app) is Slack moving the socket, and the answer is a fresh one.
  (cond
    ((equal type "hello") (list (list :hello)))
    ((equal type "disconnect")
     (let ((reason (or (nlk:json-value frame :string "reason") "disconnect")))
       (list (if (equal reason "link_disabled")
                 (list :fatal "Socket Mode is off for this app: turn it on under Socket Mode in the app's settings")
                 (list :reconnect reason)))))
    (id (list* (list :ack id)
               (nlk:when-let (payload (nlk:json-value frame :object "payload"))
                 (list (list :envelope type payload)))))))
