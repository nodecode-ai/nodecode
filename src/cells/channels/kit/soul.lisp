;;;; soul.lisp --- SOUL.md: the operator's persona file as standing context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The convention hermes-agent and openclaw share: a SOUL.md the operator
;;;; edits holds the agent's voice — tone, stance, boundaries — apart from
;;;; operating rules. Here it is the channel kit's: ~/.nodecode/SOUL.md by
;;;; default, channels.<id>.soul_file per channel, and it reaches the model
;;;; as the session's `soul` harness section, verbatim, after the base
;;;; prompt and after any room contract the adapter installed.
;;;;
;;;; The file is the one authority and the section is its projection, kept
;;;; current at the one moment every ask passes through: APPLY-SOUL runs
;;;; before each NLE:SUBMIT, so an edit is live on the next message and a
;;;; deleted file clears the section. It compares before it writes — every
;;;; harness put appends a durable session_state event, and a Telegram chat
;;;; is one session forever — so an unchanged file costs one read, no event.
;;;;
;;;; Nothing is seeded and nothing is trimmed to fit: an absent file is a
;;;; documented state /channels shows (the boilerplate to copy is SOUL.md
;;;; beside this file), and a file over the 64KiB harness budget is refused
;;;; with one warning per ask while the ask still runs.

(in-package #:nodecode-channel-kit)

(defparameter +soul-section+ "soul"
  "The harness section key the persona rides under.")

;;; NIL resolves SOUL.md under the home at each call — never at load, so a
;;; cached fasl carries no machine's home, and a profile's soul is the
;;; profile's. Tests point it at a path that does not exist so nobody's real
;;; soul leaks into a fixture.
(defvar *soul-default-path* nil
  "Where SOUL.md lives when a channel section names no soul_file.")

(defun soul-path (section)
  "The SOUL.md path for one channel SECTION: its soul_file when set, else
the default. Always a namestring."
  (or (config-string section "soul_file")
      (namestring (or *soul-default-path*
                      (nlk:home "SOUL.md")))))

(defun read-soul (path)
  "The persona text at PATH, trimmed, or NIL when the file is absent or
blank."
  ;; A file that exists but cannot be read signals: present-but-broken is
  ;; loud, never quietly absent.
  (nlk:when-let (truename (and path (probe-file path)))
    (let ((text (nlk:trimmed (uiop:read-file-string truename))))
      (and (plusp (length text)) text))))

(defun soul-status (path &optional (state (if (read-soul path) :present :absent)))
  "The one-line /channels note for PATH."
  ;; STATE is :present, :absent, or :refused; left out, the file decides
  ;; between the first two.
  (format nil "~a (~(~a~)~:[~;: exceeds the harness budget~])" path state (eq state :refused)))

(defun apply-soul (channel-id session-id path)
  "Bring SESSION-ID's soul section in line with the file at PATH, and note
the outcome on CHANNEL-ID's status."
  ;; Returns :SET when the section was written, :CLEARED when a stale one was
  ;; removed, :UNCHANGED when file and section already agree (or PATH is NIL),
  ;; :REFUSED when the text exceeds the harness budget — warned, section left
  ;; absent, the ask still runs.
  (if (null path)
      :unchanged
      (let ((text (read-soul path))
            (current (nlk:get-harness-section session-id +soul-section+)))
        (cond
          ((and text (not (equal text current)))
           (handler-case
               (progn
                 (nlk:set-harness-section session-id +soul-section+ text)
                 (set-channel-status channel-id
                                     :soul (soul-status path :present))
                 :set)
             (nlk:harness-budget-exceeded ()
               (warn "~a: SOUL.md at ~a (~d chars) exceeds the harness ~
                      budget; ~a runs without it"
                     channel-id path (length text) session-id)
               (set-channel-status channel-id
                                   :soul (soul-status path :refused))
               :refused)))
          ((and (null text) current)
           (nlk:clear-harness-section session-id +soul-section+)
           (set-channel-status channel-id :soul (soul-status path :absent))
           :cleared)
          (t :unchanged)))))
