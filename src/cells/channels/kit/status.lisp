;;;; status.lisp --- the /channels status surface, and the adapter's voice.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One live-only status plist per adapter, written by adapter threads and
;;;; read by the /channels slash command (nle:register-command). A change of
;;;; STATE is the adapter
;;;; speaking: the new status line goes out as a standing notice under the
;;;; section's key (NLE:NOTICE), so every attached shell sees "discord:
;;;; running, connected" the moment READY lands and "discord: stopped —
;;;; discord_gateway_fatal_close_4004" the moment it dies, instead of in a
;;;; log — and the model reads the same line in its cells section.
;;;; Retention: live-only — nothing here is durable or replayed.

(in-package #:nodecode-channel-kit)

;;; Mutated in place under its lock, never rebound — worker threads see the
;;; one table.
(defvar *adapter-status* (make-hash-table :test #'equal)
  "channel-id -> status plist.")

(defun channel-status-line (channel-id status)
  "One adapter's row: state, connected, sessions, delivered, soul, detail."
  (format nil "~a: ~a~@[, connected~*~]~
               ~@[, ~a session~:p~]~
               ~@[, ~a delivered~]~@[, soul ~a~]~@[ — ~a~]"
          channel-id
          (string-downcase
           (symbol-name (or (getf status :state) :unknown)))
          (getf status :connected)
          (getf status :sessions)
          (getf status :delivered-count)
          (getf status :soul)
          (getf status :detail)))

(defun set-channel-status (channel-id &rest fields)
  "Merge FIELDS (a plist) into CHANNEL-ID's status."
  ;; Well-known keys: :state (:starting :running :degraded :stopped :refused
  ;; :unconfigured), :connected, :detail, :last-event-at-ms, :delivered-count,
  ;; :sessions, :soul (the SOUL.md note). A :state that differs from the last
  ;; one is announced: the row as a standing notice under the section's key.
  (let ((announce nil))
    (sb-ext:with-locked-hash-table (*adapter-status*)
      (let* ((current (gethash channel-id *adapter-status*))
             (before (getf current :state)))
        (loop for (key value) on fields by #'cddr
              do (setf (getf current key) value))
        (setf (gethash channel-id *adapter-status*) current)
        (let ((after (getf current :state)))
          ;; :starting is transient by nature and stays in /channels.
          (when (and (not (eq before after))
                     (member after '(:running :degraded :stopped :refused :unconfigured)))
            (setf announce (channel-status-line channel-id current))))))
    (when announce
      (nle:notice announce
                  :level (case (getf fields :state)
                           ((:stopped :refused) :error)
                           (:degraded :warning)
                           ;; installed, no section: normal, so it stands unsaid
                           (:unconfigured :quiet)
                           (t :info))
                  :key (channel-notice-key channel-id))))
  (values))

(defun note-delivery (channel-id delivered error &optional context)
  "Record one delivery attempt against CHANNEL-ID's status: success bumps
:delivered-count and clears :detail, failure warns (CONTEXT names the lane
when the adapter has one) and leaves the reason in :detail."
  ;; The counter lives on the plist that already publishes it — the adapter
  ;; slot it replaces had no other reader.
  (if delivered
      (set-channel-status channel-id :detail nil :delivered-count
                          (1+ (getf (channel-status channel-id) :delivered-count 0)))
      (progn
        (warn "~a delivery~@[ to ~a~] failed: ~a" channel-id context error)
        (set-channel-status channel-id :detail error))))

(defun channel-status (channel-id)
  (sb-ext:with-locked-hash-table (*adapter-status*)
    (copy-list (gethash channel-id *adapter-status*))))

(defun clear-channel-status (channel-id)
  (sb-ext:with-locked-hash-table (*adapter-status*)
    (remhash channel-id *adapter-status*))
  (nle:notice nil :key (channel-notice-key channel-id))
  (values))

(defun clear-all-channel-status ()
  (let ((ids (sb-ext:with-locked-hash-table (*adapter-status*)
               (loop for id being the hash-keys of *adapter-status* collect id))))
    (dolist (id ids) (clear-channel-status id)))
  (values))

(defun channels-status-report ()
  "The /channels answer: one line per adapter, deterministic order."
  (let ((rows (sb-ext:with-locked-hash-table (*adapter-status*)
                (loop for id being the hash-keys of *adapter-status* using (hash-value status)
                      collect (cons id (copy-list status))))))
    (if (null rows)
        "no channel adapters running"
        (format nil "~{~a~^~%~}"
                (loop for (id . status) in (sort rows #'string< :key #'car)
                      collect (channel-status-line id status))))))

(defun channels-json (&key text)
  "Every adapter's status, as the web page's Control pane reads it: its
state, whether it is connected, when it last heard or said anything, its
standing detail and last refusal, and what it has delivered; then the
shipped adapters this home has not installed (ABSENT-ADAPTERS), who asks to
be let in and who is (PAIRING-JSON), and TEXT, what a verb just said."
  (let ((rows (sb-ext:with-locked-hash-table (*adapter-status*)
                (loop for id being the hash-keys of *adapter-status* using (hash-value status)
                      collect (cons id (copy-list status))))))
    (nlk:json-object
     "channels" (map 'vector
                     (lambda (row &aux (status (cdr row)))
                       (nlk:json-object "id" (car row)
                                        "state" (string-downcase (or (getf status :state) :unknown))
                                        "connected" (and (getf status :connected) t)
                                        :opt "last_event_at_ms" (getf status :last-event-at-ms)
                                        :opt "detail" (getf status :detail)
                                        :opt "last_rejection" (getf status :last-rejection)
                                        :opt "delivered" (getf status :delivered-count)
                                        :opt "sessions" (getf status :sessions)
                                        :opt "soul" (getf status :soul)))
                     (sort rows #'string< :key #'car))
     "absent" (coerce (absent-adapters) 'vector)
     "pairing" (pairing-json)
     :opt "text" text)))

(defun channels-command (args &aux (words (nlk:split-words (or args ""))))
  "/channels ARGS: the status report, or the pairing verbs — pair CODE,
unpair USER-ID, paired."
  (cond ((null words) (channels-status-report))
        ((and (string-equal (first words) "pair") (second words)) (pair (second words)))
        ((and (string-equal (first words) "unpair") (second words)) (unpair (second words)))
        ((string-equal (first words) "paired") (pairing-report))
        (t "usage: /channels [pair CODE | unpair USER-ID | paired]")))

(defun register-channel-commands ()
  "Register /channels, /stop, /sethome, /agent and /threads, each answering its report
whole; /agent completes its room's agents."
  (flet ((command (name run description &optional argument-hint complete)
           (nle:register-command "nodecode-channel-kit" name run
                                 :description description :argument-hint argument-hint
                                 :complete complete)))
    (command "channels" (lambda (args session-id)
                          (declare (ignore session-id))
                          (channels-command args))
             "Channel status; pair CODE lets in whoever a direct message gave it"
             "pair CODE | unpair USER-ID | paired")
    (command "stop" (lambda (args session-id)
                      (declare (ignore args))
                      (nth-value 1 (stop-room-turn session-id)))
             "Stop the newest turn running in this room")
    (command "sethome" 'sethome-command
             "Make this channel the bot's home: restarts, pairing requests and refusals are said here"
             "off")
    (command "agent" (lambda (args session-id) (agent-command args session-id))
             "Which agent answers here; NAME hands this room to another"
             "NAME | default" 'agent-choices)
    (command "threads" 'board-command "Every open thread, by who moves next")))
