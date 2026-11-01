;;;; rows.lisp --- a session's transcript as rows, for a client that renders
;;;; and does not fold.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A shell folds the sync stream itself (APPLY-GATEWAY-EVENT, tui/client.lisp).
;;;; A browser cannot: the fold is Lisp, and a tool card's words come from
;;;; reading the snippet it ran (tool-summary.lisp). So a subscription entry carrying
;;;; "rows": true has the fold done here, by the same reducer over the same
;;;; frames. An in-process peer (ATTACH-LOOPBACK-GATEWAY-PEER) subscribes to the
;;;; session in the client's place and folds every frame the gateway would have
;;;; sent the client; the client is sent session_rows frames instead -- the rows
;;;; that changed since the last one, the ids that went, the order when it
;;;; moved -- by its own writer, at most *ROWS-FLUSH-SECONDS* apart, so a
;;;; stream coalesces behind a client that reads slower than it arrives.
;;;;
;;;; Retention: live-only. A client that attaches again is sent the whole
;;;; transcript again (reset), folded from the log and the round in flight by
;;;; the replay every subscription earns (gateway.lisp SEND-REPLAY-UNDER-LOCK).

(in-package #:nodecode.evolved)

(defstruct (rows-view (:copier nil))
  "One client's rows of one session: the fold, and what the client holds of it."
  (client nil :read-only t)
  (session-id "" :read-only t)
  (lock (bt2:make-lock :name "rows-view") :read-only t)
  ;; The in-process peer subscribed in the client's place, and how many times
  ;; in a row it was hung up before a replay reached its end.
  (link nil)
  (hang-ups 0 :type fixnum)
  (state (initial-chat-state))
  ;; A replay folds out of sight until its last chunk, as a shell's does.
  (staging nil)
  (ready-p nil)
  (active-p nil)
  (verb nil)
  ;; Row -> its id, and id -> what the client was last sent of it.
  (ids (make-hash-table :test #'eq :weakness :key) :read-only t)
  (next-id 0 :type fixnum)
  (sent (make-hash-table) :read-only t)
  (order '())
  (turn nil)
  (reset-p t)
  (flush-pending-p nil)
  (flushed-at 0))

(nlk:access (view rows-view) (row transcript-row) (session nlk:durable-session))

(defparameter *rows-flush-seconds* 1/30
  "The least time between two session_rows frames to one client.")

(defvar *rows-views* (make-hash-table :test #'eq :synchronized t)
  "Each client's rows views: the client's socket -> its ROWS-VIEWs.")

;;; --- what a client holds of a row ------------------------------------------

(defparameter *row-signature*
  (list #'transcript-row-text (lambda (row) (length row.text)) #'transcript-row-text-rev
        #'transcript-row-kind #'transcript-row-role #'transcript-row-turn-id
        #'transcript-row-prompt-id #'transcript-row-steer-p #'transcript-row-recorded
        #'transcript-row-evicted-p
        #'transcript-row-tool-call-id #'transcript-row-tool-name #'transcript-row-tool-args
        (lambda (row) (length (or row.tool-args ""))) #'transcript-row-tool-status
        #'transcript-row-tool-exit-code #'transcript-row-metadata #'transcript-row-hydrated
        #'transcript-row-voided #'transcript-row-error-p #'transcript-row-outcome
        #'transcript-row-thought-ms
        #'transcript-row-reasoning-tokens)
  "Every field a row renders from, read as a client was last sent it: the
text first, by identity and length -- a streamed row appends in place.")

(defun row-signature (row)
  (map 'simple-vector (lambda (reader) (funcall reader row)) *row-signature*))

(defun row-unchanged-p (row signature)
  (and signature
       (every (lambda (reader was) (eql (funcall reader row) was)) *row-signature* signature)))

(defun row-tool-activity (row)
  "What ROW's call did, in the shell's words (TOOL-ARGUMENTS-ACTIVITY), or NIL."
  ;; A call whose arguments are still streaming may not read yet.
  (with-handlers ((error () nil))
    (tool-arguments-activity row.tool-name row.tool-args
                             :metadata row.metadata :error-p row.error-p)))

(defun row-tool-title (row activity &aux (running (member row.tool-status '("running" "background")
                                                          :test #'equal)))
  "The words ROW's tool card is titled by, the shell's own (ACTIVITY-TITLE):
a snippet titled by its code says only that it evaluated, the code standing
beside the words (ROW-JSON's \"code\")."
  (when activity
    (if (activity-code-title-p activity)
        (getf (activity-family-plist :eval) (if running :present :past))
        (activity-title activity :running running))))

(defun row-tool-commands (row)
  "The shell command ROW's call ran, as its card's `$ ' line shows it, in a
vector: a snippet that is one (sh \"...\") with a literal command (EXEC-TITLE).
NIL for any other snippet, whose code is what it ran."
  (when (equal "eval" row.tool-name)
    (when-let (snippet (nlk:tool-command-string row.tool-args))
      (let ((command (exec-title snippet)))
        (and (not (eq command snippet)) (vector command))))))

(defun row-tool-shown (row)
  "ROW's result as its card shows the operator (TOOL-RESULT-SHOWN), or NIL
where the card says something else in its place: a stop, whose card is what
the call wrote and who stopped it, and a job handed to the background, whose
output is in its box."
  (unless (or (json-value row.metadata :object "stopped")
              (json-value row.metadata :object "background_shell")
              (null row.hydrated))
    (tool-result-shown row.tool-name row.text row.metadata)))

(defun row-json (row id signature &aux (text row.text) (was (and signature (svref signature 0)))
                                       (sent-length (and signature (svref signature 1)))
                                       (appended (and (eq text was) (> (length text) sent-length))))
  "ROW, whose id is ID, as its client is sent it: whole, or with SIGNATURE --
what the client last had -- its text as what was appended since, when that
is all that happened to it."
  (json-object "id" id
               "kind" (substitute #\_ #\- (string-downcase row.kind))
               "role" row.role
               :opt "turn_id" row.turn-id
               :opt "prompt_id" row.prompt-id
               :opt "steer" row.steer-p
               :opt "recorded" row.recorded
               :opt "evicted" row.evicted-p
               :opt "live" (not row.hydrated)
               :opt "voided" (if (stringp row.voided) row.voided (and row.voided t))
               :opt "error" row.error-p
               :opt "outcome" row.outcome
               :opt "thought_ms" row.thought-ms
               :opt "reasoning_tokens" row.reasoning-tokens
               :opt "metadata" row.metadata
               :when appended "append" (subseq text sent-length)
               :when (not appended) "text" (copy-seq text)
               :when (eq :tool-call row.kind) "tool"
               (let ((activity (row-tool-activity row)))
                 (json-object "name" (or row.tool-name "")
                              "args" (or row.tool-args "")
                              :opt "status" row.tool-status
                              :opt "call_id" row.tool-call-id
                              :opt "exit_code" row.tool-exit-code
                              :opt "title" (row-tool-title row activity)
                              :opt "code" (and activity (activity-code-title-p activity)
                                               activity.head)
                              :opt "family" (and activity (string-downcase activity.family))
                              :opt "commands" (row-tool-commands row)
                              ;; the operator's reading of the result; "text" stays the model's
                              :opt "shown" (row-tool-shown row)
                              :opt "exec" (row-exec-json row)))))

(defun rows-view-news-p (view key signature reset)
  "True, remembering SIGNATURE under KEY, when VIEW's client was last sent
something else for KEY -- always after a RESET."
  (unless (and (not reset) (equal signature (gethash key view.sent)))
    (setf (gethash key view.sent) signature)
    t))

(defun exec-title (command)
  "What an exec is titled by: the shell command itself when COMMAND, the
snippet, opens on an (sh \"...\") whose literal command is the one call it
makes, else COMMAND."
  ;; Read statically (SNIPPET-CALLS), never by the reader: a title is no reason to
  ;; intern a snippet's names. A command still streaming reads as far as it has
  ;; come.
  (multiple-value-bind (calls unknown) (snippet-calls command)
    (let ((call (first calls)))
      (if (and (null (rest calls)) (null unknown)
               (equal "sh" (car call)) (stringp (cdr call))
               (ppcre:scan "\\A\\((?i:sh)[\\s\"]"
                           (subseq command (nlk::scribe-skip-blank command 0 (length command)))))
          (cdr call)
          command))))

(defun exec-tail-text (lines cut)
  "LINES, the non-empty lines an exec kept, as its tail is sent: joined, the
first left out when CUT -- a tail kept by bytes begins partway into a line."
  (format nil "~{~a~^~%~}" (if cut (rest lines) lines)))

(defun exec-json (band)
  "One running exec of the band (BG-BAND-ROW) as a client is sent it: its id
for interrupt_exec, the call that ran it -- an id no later image reuses, as
exec ids restart with each -- what it runs (EXEC-TITLE), and every line of
its output the band keeps, CUT when it wrote more before them."
  (let* ((lines (remove "" (nlk:lines (octets-to-text band.output-tail)) :test #'equal))
         (cut (> (bg-row-written-lines band) (length lines))))
    (json-object :opt "exec_id" band.shell-session-id :opt "call_id" band.tool-call-id
                 "command" (exec-title band.command) "status" (string-downcase band.status)
                 "tail" (exec-tail-text lines cut)
                 :when cut "cut" t)))

(defun row-exec-json (row)
  "ROW's exec as a closed card, when ROW is a call that ran on in the
background and exited: EXEC-JSON's shape, read from the exit's document on
the row (APPLY-BACKGROUND-SHELL-EXIT), with how it ended (EXEC-END), its
last (sh ...)'s status and the report of the condition it failed on. NIL for
every other row."
  (when-let (shell (row-exit-document row))
    (let ((shell-exit (json-value shell :integer "shell_exit"))
          (cut (json-value shell :boolean "tail_cut")))
      (json-object :opt "exec_id" (json-value shell :integer "shell_session_id")
                   :opt "call_id" row.tool-call-id
                   "command" (exec-title (or (json-value shell :string "command") ""))
                   "status" "exited"
                   "end" (exec-end (json-value shell :integer "exit_code") shell-exit)
                   :opt "shell_exit" shell-exit
                   "tail" (exec-tail-text (remove "" (nlk:lines (or (json-value shell :string "tail") ""))
                                                  :test #'equal)
                                          cut)
                   :when cut "cut" t
                   :opt "error" (json-value shell :string "error")
                   :opt "stopped" (json-value shell :object "stopped")))))

(defun context-json (state session-id &aux (usage (gethash "context_usage" state))
                                           (model (getf usage :model))
                                           (provider (or (getf (gethash "usage" state) :provider)
                                                         (nlk:session-model-selection session-id)
                                                         *provider*)))
  "What the newest provider round placed in SESSION-ID's window, and the
window it has (CONTEXT-WINDOW-VIEW), from the folded STATE."
  (json-object "used" (context-occupancy-tokens usage)
               :opt "window" (context-window-view provider model)
               :opt "model" model))

(defun rows-view-payload (view)
  "The session_rows payload for what changed in VIEW since its client's last
one, or NIL when nothing did: its rows, the turn, the execs running in it,
and what its context holds."
  ;; The caller holds VIEW's lock.
  (let ((reset view.reset-p) (rows '()) (order '()) (gone '()) (live (make-hash-table)))
    (loop for row in (append (gethash "history_rows" view.state) (gethash "tail_rows" view.state))
          for id = (or (gethash row view.ids) (setf (gethash row view.ids) (incf view.next-id)))
          for signature = (and (not reset) (gethash id view.sent))
          do (push id order)
             (setf (gethash id live) t)
             (unless (row-unchanged-p row signature)
               (push (row-json row id signature) rows)
               (setf (gethash id view.sent) (row-signature row))))
    (setf order (nreverse order))
    ;; Rows arrive at the end and leave from anywhere; an order is sent only
    ;; when the ones that stayed are no longer where the client has them.
    (let* ((kept (loop for id in view.order
                       if (gethash id live) collect id
                       else do (push id gone) (remhash id view.sent)))
           (moved (let ((at (mismatch kept order))) (and at (< at (length kept)))))
           ;; What the turn is doing reads as a shell's live bar reads it
           ;; (tui/transcript/ingest.lisp, the bar's head): a retrying provider ahead of the activity, until
           ;; the stream moves and the reducer retires it. The rows are sent
           ;; when something changes, never on a clock, so the retry carries no
           ;; countdown: one would stand frozen at its first second.
           (retry (gethash "retry_status" view.state))
           (turn (list view.active-p (and view.active-p (gethash "turn_id" view.state))
                       (and view.active-p
                            (if retry
                                (format nil "~a~@[ · ~a~]" (retry-status-text retry) (retry-said retry))
                                view.verb))))
           ;; The band's rows are replaced, never edited, when an exec writes:
           ;; comparing them costs a walk of a few conses.
           (bands (gethash "background_processes" view.state))
           (execs (rows-view-news-p view :execs
                                    (mapcar (lambda (band)
                                              (list band.shell-session-id band.tool-call-id
                                                    band.status band.output-tail))
                                            bands)
                                    reset))
           (usage (gethash "context_usage" view.state))
           (context (and usage (rows-view-news-p view :context usage reset))))
      (setf view.order order view.reset-p nil)
      (when (or reset rows gone moved execs context (not (equal turn view.turn)))
        (setf view.turn turn)
        (json-object "session_id" view.session-id
                     "rows" (coerce (nreverse rows) 'vector)
                     :opt "gone" (and gone (coerce gone 'vector))
                     :opt "order" (and (or reset moved) (coerce order 'vector))
                     :opt "reset" reset
                     :opt "execs" (and execs (coerce (mapcar #'exec-json bands) 'vector))
                     :opt "context" (and context (context-json view.state view.session-id))
                     "turn" (destructuring-bind (active turn-id verb) turn
                              (json-object "active" (and active t)
                                           :opt "turn_id" turn-id
                                           :opt "verb" verb)))))))

;;; --- the fold ----------------------------------------------------------------

(defun rows-view-receive (view text)
  "Fold one frame TEXT the gateway sent VIEW's link, as the client's shell
would have, and ask for a flush once a replay has reached its end."
  (let* ((payload (nlk:sync-message-payload (nlk:decode-sync-message text)))
         (kind (and (hash-table-p payload) (gethash "type" payload))))
    (bt2:with-lock-held (view.lock)
      (cond
        ((equal kind "session_stream_resumed")
         ;; A long session's replay runs twice, a first pass of its newest
         ;; turns and then the whole log (SEND-REPLAY-PASS); each starts at
         ;; chunk 0 and each is put in place at its last chunk.
         (setf view.staging
               (fold-accumulated-frames (gethash "accumulated_frames" payload)
                                        (if (plusp (or (gethash "replay_chunk_index" payload) 0))
                                            view.staging
                                            (initial-chat-state))))
         (when (gethash "replay_chunk_final" payload)
           (setf view.state view.staging view.staging nil
                 view.ready-p t view.reset-p t view.hang-ups 0
                 view.active-p (equal "active" (gethash "admission_state" payload))
                 view.verb nil)))
        ;; A parked steer has no fact of its own until promotion: the queue
        ;; snapshot frame is what puts it in the band, live.
        ((member kind '("session_stream_event" "item_delta"
                        "session_background_process_updated"
                        "session_input_queue_snapshot_updated")
                 :test #'equal)
         (multiple-value-bind (state verdict) (apply-gateway-event view.state payload)
           (setf view.state state)
           (case (getf verdict :lifecycle)
             (:started (setf view.active-p t view.verb nil))
             ((:completed :failed :cancelled) (setf view.active-p nil view.verb nil)))
           (when-let (verb (getf verdict :activity))
             (setf view.verb verb))))
        ;; An undo or a rewind moved the session's head: the log folds again.
        ((equal kind "session_resync_required")
         (setf view.ready-p nil)
         (subscribe-rows-link view))))
    ;; One flush waits on the client's writer at a time.
    (when (and view.ready-p (not (bt2:with-lock-held (view.lock) (shiftf view.flush-pending-p t))))
      (enqueue-peer-frame view.client (lambda (state) (flush-rows-view view state))))))

(defun flush-rows-view (view state)
  "On the client's writer, STATE's: send what changed in VIEW since the last
frame, no sooner than *ROWS-FLUSH-SECONDS* after it."
  (let ((wait (- (+ view.flushed-at (* *rows-flush-seconds* internal-time-units-per-second))
                 (get-internal-real-time))))
    (when (plusp wait)
      (sleep (/ wait internal-time-units-per-second))))
  (when-let (text (bt2:with-lock-held (view.lock)
                    (setf view.flush-pending-p nil
                          view.flushed-at (get-internal-real-time))
                    (when-let (payload (rows-view-payload view))
                      (nlk:encode-projection-event view.session-id 0 "session_rows" payload))))
    (gw-peer-send-text state text)))

;;; --- the link ----------------------------------------------------------------

(defun subscribe-rows-link (view)
  (send-gateway-command view.link :set-session-subscriptions
                        (json-object "subscriptions" (vector view.session-id))))

(defun attach-rows-link (view &aux (link nil))
  "Subscribe a fresh in-process peer to VIEW's session in its client's place."
  (setf link (attach-loopback-gateway-peer
              (lambda (text) (rows-view-receive view text))
              ;; The gateway ended the link -- dropped at its queue bound, a
              ;; replay that failed: attach again while the client still
              ;; wants the rows, and give up on a link that never gets
              ;; through a replay.
              :hang-up (lambda ()
                         (when (and (eq link view.link)
                                    (sb-ext:with-locked-hash-table (*rows-views*)
                                      (member view (gethash view.client *rows-views*)))
                                    (< (incf view.hang-ups) 3))
                           (attach-rows-link view))))
        view.link link
        view.ready-p nil)
  (when link (subscribe-rows-link view)))

(defun open-rows-view (client session-id)
  "CLIENT's rows of SESSION-ID, opened now unless it already holds them."
  (or (sb-ext:with-locked-hash-table (*rows-views*)
        (find session-id (gethash client *rows-views*) :key #'rows-view-session-id :test #'string=))
      (let ((view (make-rows-view :client client :session-id session-id)))
        (sb-ext:with-locked-hash-table (*rows-views*)
          (push view (gethash client *rows-views*)))
        (attach-rows-link view)
        view)))

(defun session-rows-links (session-id)
  "The links every client's rows views of SESSION-ID subscribe through."
  (sb-ext:with-locked-hash-table (*rows-views*)
    (loop for view in (alexandria:mappend #'identity (alexandria:hash-table-values *rows-views*))
          when (and view.link (string= view.session-id session-id))
            collect view.link)))

(defun close-session-rows-views (session-ids)
  "Close every client's rows views of SESSION-IDS: sessions that are gone."
  (loop for (client . views) in (sb-ext:with-locked-hash-table (*rows-views*)
                                  (nreverse (alexandria:hash-table-alist *rows-views*)))
        do (close-rows-views client (set-difference (mapcar #'rows-view-session-id views)
                                                    session-ids :test #'string=))))

(defun close-rows-views (client &optional keep &aux (closing '()))
  "Close CLIENT's rows views, every one or those of sessions KEEP does not name."
  (sb-ext:with-locked-hash-table (*rows-views*)
    (let ((views (gethash client *rows-views*)))
      (setf closing (remove-if (lambda (view) (member view.session-id keep :test #'string=))
                               views))
      (if (set-difference views closing)
          (setf (gethash client *rows-views*) (set-difference views closing))
          (remhash client *rows-views*))))
  (dolist (view closing)
    (gateway-link-close (shiftf view.link nil))))

;;; --- a session as Markdown ----------------------------------------------------
;;; The export a person reads: the rows a shell attached to the session shows,
;;; folded by the shell's reducer from the frames its replay sends
;;; (SESSION-FRAMES), written out as Markdown. What a shared copy is read for
;;; -- the prompts, the answers, the plans, one line per call -- and not the
;;; rest the log keeps: reasoning, a call's output, an attempt that failed and
;;; the shell's own notices stay in the JSON lines export, and so does a turn
;;; still running.
;;;
;;; Retention: derived on request, never stored.

(defun local-stamp (iso)
  "ISO as this machine's local date and minute, or NIL."
  (when-let (universal (nlk:iso-universal iso))
    (bind (((_ minute hour day month year) (decode-universal-time universal)))
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute))))

(defun session-transcript (session-id)
  "SESSION-ID's rows as a shell's replay folds them, and a table of each turn
id to the stamp its first prompt was committed at."
  ;; The frames go through the wire's own text, which is what a shell folds:
  ;; a fact's payload rides as the log keeps it.
  (let ((frames (map 'vector (lambda (frame) (nlk:decode-json (encode-json-object frame)))
                     (session-frames session-id)))
        (stamps (make-hash-table :test #'equal)))
    (loop for frame across frames
          for turn = (json-value frame :text "turn_id")
          when (and turn (equal nlk::+kind-input-committed+
                                (json-value frame :string "event" "type")))
            do (unless (gethash turn stamps)
                 (setf (gethash turn stamps)
                       (json-value frame :text "event" "metadata" "occurred_at"))))
    (values (transcript-rows (fold-accumulated-frames frames)) stamps)))

(defun markdown-tool-line (row &aux (activity (row-tool-activity row)))
  "ROW, a tool call, as one list item: its tool, the words the shell's card
is titled by (ACTIVITY-TITLE), and whether it looked at a picture."
  (format nil "- `~a`~@[ ~a~]~:[~; · image~]"
          (or row.tool-name "tool")
          (let ((title (and activity (activity-title activity))))
            (and title (plusp (length title)) (nlk:one-line title)))
          (json-value row.metadata :object "image")))

(defun session-markdown (session-id)
  "SESSION-ID as a Markdown document, its settled turns as a shell attached to
it shows them: a fork's parent turns above its own, a turn the head left out."
  (let ((session (or (nlk:find-session session-id)
                     (error 'nlk:unknown-session :session-id session-id)))
        (running (first (nlk::active-turn-row session-id)))
        (blocks '())
        (tools '())
        (turns '())
        (speaker nil))
    (multiple-value-bind (rows stamps) (session-transcript session-id)
      (flet ((say (text) (push text blocks))
             (flush () (when tools (push (format nil "~{~a~^~%~}" (reverse tools)) blocks)
                         (setf tools '())))
             (speaking (who)
               (unless (eq who speaker)
                 (setf speaker who)
                 (push "**Nodecode**" blocks))))
        (dolist (row rows)
          (unless (or row.voided (and running (equal running row.turn-id)))
            (case row.kind
              (:text
               (when (plusp (length (string-trim '(#\Space #\Newline) row.text)))
                 (flush)
                 (cond ((equal "user" row.role)
                        (setf speaker :you)
                        (pushnew row.turn-id turns :test #'equal)
                        (say (format nil "**~a**~@[ · ~a~]"
                                     (cond ((stringp row.recorded)
                                            (format nil "Recorded by ~a" row.recorded))
                                           (row.recorded "Recorded")
                                           (t "You"))
                                     (local-stamp (gethash row.turn-id stamps))))
                        (say row.text)
                        (let ((pictures (length (json-array row.metadata "images"))))
                          (when (plusp pictures)
                            (say (format nil "_Sent with ~d picture~:p_" pictures)))))
                       ((equal "assistant" row.role)
                        (speaking :nodecode)
                        (say row.text)))))
              (:tool-call
               (speaking :nodecode)
               (push (markdown-tool-line row) tools)))))
        (flush))
      (with-output-to-string (out)
        (let ((parent session.parent)
              (name (nlk:one-line (nlk::session-title session-id))))
          (format out "# ~a~%~%- Session: `~a` · ~a~%- Started ~a · ~d turn~:p~%~
                       ~@[- Forked from ~a~%~]"
                  (if (plusp (length name)) name session-id) session-id session.cwd
                  (local-stamp session.created-at) (length turns)
                  (and parent session.anchor-turn-id
                       (format nil "\"~a\" (`~a`)" (nlk:one-line (nlk::session-title parent)) parent))))
        (format out "~{~%~a~%~}" (reverse blocks))
        (when running
          (format out "~%_A turn is running: this copy ends before it._~%"))))))

(defun markdown-file-name (session-id)
  "The name SESSION-ID's Markdown downloads as: its name in lower case, every
run of anything but an ASCII letter or digit one dash, else its id."
  (let ((slug (string-trim "-" (ppcre:regex-replace-all
                                "[^a-z0-9]+" (string-downcase (nlk::session-title session-id)) "-"))))
    (format nil "~a.md" (if (plusp (length slug))
                            (string-right-trim "-" (clip slug 60 :ellipsis ""))
                            (substitute #\- #\: session-id)))))
