;;;; organism.lisp --- what an adapter takes from the organism it lives in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A channel cell is loaded INTO the gateway image, so it reaches the
;;;; organism the way any in-image code does: sessions through the kernel
;;;; session API, prompts through NLE:SUBMIT, and every live frame through a
;;;; (NLE:HOOK :FRAME ...) chain on PUBLISH-EVENT. There is no client here —
;;;; no socket, no handshake, no subscription set, no operator token. The
;;;; readers below only name the pieces of a :FRAME op and of a fact payload
;;;; that every adapter reads the same way.
;;;;
;;;; THREAD RULE, unchanged in spirit from the loopback era: a :FRAME hook
;;;; runs on whichever thread published the frame (the turn worker, the
;;;; background-exec watcher), so it folds and enqueues ONLY; all platform
;;;; REST runs on the adapter's delivery worker. A hook that signals fails
;;;; the publish for the turn that produced it — wrap anything that can.

(in-package #:nodecode-channel-kit)

;;; Data, so an operator's layer can reword it; each adapter adds its own
;;; seams beneath it.
(defparameter +channel-seams-primer+
  "Reshaping the channel: every function in the channel kit (nck) and in this adapter is plain Lisp and advisable. (define-hook NAME \"key\" (next . args) ...) wraps one and is kept in your layer, re-registered at every boot; a bare (hook ...) lasts until restart; (unhook 'NAME \"key\") removes either, and takes a define-hook out of your layer as well. Call next to run the rest, or not to replace it. The seams:
  nck:decide-inbound (policy candidate) => (values :answer|:observe|:reject reason) — admission of one message; candidate is the hash table the user line is built from.
  nck:execute-plan (executor plan) => execution — every outbound call, the adapter's and yours; nck:request-plan-method/-path/-body read the plan.
  nck:digest-final-text (digest) — the answer text as it is about to be posted; nck:digest-card (digest now) — the card edited in place while a turn runs and settled above its answer, a plist each platform draws (nck:card-text its plain lines).
  nck:deliver-answer (host lane digest) — posting one turn's answer; nck:post-message (host target text &key reply-to ping) — any chunked post to a target.
  nck:answer-file (path &key platform channel thread) — hand one file to the answer the running turn is about to deliver: it rides the answer's own message, in the thread and in the line the ask's room keeps, so a picture shows where the answer shows and never beside it. Answers (:session-id S :turn-id T :files (PATH ...)); a room with no running turn is an error. nck:post-file (path &key platform channel thread content reply-to) — post one file (an image, a chart, a log) into a room as its own message, when the file is not the answer's picture: the platform's own upload, the created message back as (:status N :body VALUE), or :status 0 and why when that platform carries no file. nck:fetch-image (url) — bring an image down from the web as a file to post, its bytes sniffed and a page that is not an image refused.
  An answer may carry a picture when the task is better shown than told — a real image borrowed from the web as illustration, or one you annotated when the marks are the point; hand it to the answer itself with nck:answer-file, so it rides the message that answers the ask rather than posting beside it; neither is required, and the words carry the answer when they can.
  nck:handle-tag (candidate) — the [m.. u.. r..] bracket on a user line."
  "What a channel's contract says about reshaping the channel itself: the
kit's advisable seams, the same for every adapter.")

;;; --- sessions ---------------------------------------------------------------

(defun ensure-session (session-id &key cwd parent)
  "Hold SESSION-ID standing by if it is not durable already; true either
way."
  ;; Nothing durable names a room or a lane until its first block — the lane's
  ;; ask, or the room's write-back — so a room nobody spoke in leaves no
  ;; session in the store, and the directory lists none.
  ;;
  ;; PARENT forks the new session from that session's CURRENT head: the child
  ;; composes the parent's history by reference, frozen at the parent's log
  ;; watermark, so anything the parent records afterwards is invisible to it.
  ;; Ignored when the session already exists — lineage is fixed at creation.
  ;; The fork anchor is read here, at the mint: a durable parent's current
  ;; head, or — for a parent still standing by — the anchor that parent was
  ;; minted with, and the child's materialization brings the parent into the
  ;; log first, below the child.
  (or (nlk:session-exists-p session-id)
      (progn (nlk:standby-session :id session-id :cwd cwd :parent parent)
             t)))

;;; --- :FRAME op readers ------------------------------------------------------
;;; OP is (:session-id ID :kind WIRE-KIND :payload HASH :cursor POS
;;; :durable-p BOOL) — the exact arguments PUBLISH-EVENT was called with.

(defun frame-session-id (op)
  (getf op :session-id))

(defun frame-delta (op)
  "When OP is a live item_delta frame, return its payload — the object
holding \"turn_id\" and \"delta\" {\"type\", \"text\" | tool fields}."
  ;; NIL otherwise.
  (and (equal "item_delta" (getf op :kind))
       (values (nlk:json-value (getf op :payload) :object))))

(defun frame-head-moved-p (op)
  "Whether OP is the live frame a session's head move publishes — a /new,
an /undo, a rewind, from whichever surface ran it."
  (equal "session_checkpoint_undo_applied" (getf op :kind)))

;;; --- fact readers -----------------------------------------------------------

(defun fact-message-reasoning (payload)
  "The round's thinking from a turn.assistant_message_completed fact —
`reasoning_content` on the wire message, present whenever the provider
streamed reasoning."
  ;; NIL when the round did no thinking. Display material for a live status
  ;; line: a round whose content is JSON null still thought out loud, and a
  ;; channel that shows nothing for it reads as dead.
  (values (nlk:json-value payload :text "message" "reasoning_content")))

(defun fact-message-tool-calls (payload)
  "Whether the round's wire message called tools."
  ;; A round that called tools keeps the turn going, so the text it carried is
  ;; commentary — something the turn says while it works, never the answer it
  ;; ends with. Reads the same shape the engine reads: `tool_calls', a vector,
  ;; JSON null when absent.
  (plusp (length (nlk:json-value payload :array "message" "tool_calls"))))
