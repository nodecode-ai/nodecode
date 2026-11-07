;;;; delivery.lisp --- bounded queue, delivery worker, typing, lanes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The delivery worker is where every platform REST call runs (package.lisp
;;;; topology rule): WS reader callbacks enqueue closures here and return.
;;;; The queue is bounded at 256 jobs — overflow drops the job with one loud
;;;; warning rather than ever blocking the enqueuing reader, because a reader
;;;; stalled on a full queue is exactly the peer the gateway tombstones.

(in-package #:nodecode-channel-kit)

(defun now-ms ()
  "Monotonic-enough milliseconds for throttles and typing cadence."
  (truncate (* (get-internal-real-time) 1000)
            internal-time-units-per-second))

;;; --- bounded work queue ---------------------------------------------------

(defstruct (work-queue (:copier nil)
                       (:constructor make-work-queue
                           (name &key (cap 256)
                            &aux (mailbox (sb-concurrency:make-mailbox :name name)))))
  name cap mailbox)

(nlk:access (queue work-queue))

(defun queue-depth (queue) (sb-concurrency:mailbox-count queue.mailbox))

(defun queue-push (queue item)
  "Enqueue ITEM; NIL + one warning per drop when the queue is full."
  ;; Never blocks: the caller may be a WS reader whose stall tombstones its
  ;; peer. The cap is a memory bound, so two pushers racing past it by one
  ;; is no harm.
  (if (< (queue-depth queue) queue.cap)
      (progn (sb-concurrency:send-message queue.mailbox item) t)
      (warn "~a queue full (~a); dropping work" queue.name queue.cap)))

(defun queue-pop (queue timeout-seconds)
  "(values ITEM FOUND-P), waiting at most TIMEOUT-SECONDS."
  ;; A mailbox refuses a zero timeout, so a pop that must not wait asks
  ;; without hanging.
  (if (plusp timeout-seconds)
      (sb-concurrency:receive-message queue.mailbox :timeout timeout-seconds)
      (sb-concurrency:receive-message-no-hang queue.mailbox)))

;;; --- delivery worker ------------------------------------------------------

(defstruct (delivery-worker (:copier nil) (:constructor %make-delivery-worker))
  (queue nil)
  (threads '() :type list)
  (stop-p nil))

(nlk:access (worker delivery-worker))

(defun start-delivery-worker (name &key tick-fn (tick-seconds 0.5) (workers 1))
  "A pool of WORKERS threads draining one bounded closure queue."
  ;; TICK-FN, when given, runs between jobs at roughly TICK-SECONDS cadence on
  ;; the FIRST thread only — the typing-refresh and card-flush seam, which
  ;; must stay single-writer. A job or tick that signals is one warning, never
  ;; a worker's death.
  ;;
  ;; More than one thread matters as soon as jobs are platform REST calls and
  ;; lanes outnumber them: with a single drainer, one call parked on its
  ;; request timeout stalls every other lane's delivery behind it. The pool
  ;; bounds that head-of-line stall at WORKERS concurrent slow calls; the
  ;; queue's own cap still bounds memory.
  (check-type workers (integer 1))
  (let* ((queue (make-work-queue name))
         (worker (%make-delivery-worker :queue queue)))
    (setf worker.threads
          (loop for index from 0 below workers
                collect
                (let ((tick-fn (and (zerop index) tick-fn))
                      (thread-name (if (zerop index)
                                       name
                                       (format nil "~a-~d" name index))))
                  (nlk:spawn thread-name
                    (let ((last-tick (now-ms)))
                      (flet ((run (thunk what)
                               (handler-case (funcall thunk)
                                 (error (condition)
                                   (warn "~a ~a signalled: ~a" thread-name what condition)))))
                        (loop
                          (when worker.stop-p (return))
                          (multiple-value-bind (job found)
                              (queue-pop queue tick-seconds)
                            (when worker.stop-p (return))
                            (when found
                              (run job "job"))
                            (when (and tick-fn
                                       (>= (- (now-ms) last-tick)
                                           (truncate (* tick-seconds 1000))))
                              (setf last-tick (now-ms))
                              (run tick-fn "tick"))))))))))
    worker))

(defun delivery-worker-enqueue (worker job)
  (queue-push worker.queue job))

(defun stop-delivery-worker (worker)
  (setf worker.stop-p t)
  ;; One message per drainer: a message wakes exactly one parked drainer,
  ;; which sees the stop and leaves it unrun, and the rest would sit out
  ;; their tick timeout before noticing the stop.
  (loop repeat (length worker.threads)
        do (sb-concurrency:send-message (work-queue-mailbox worker.queue) nil))
  (dolist (thread worker.threads)
    (ignore-errors (bt2:join-thread thread)))
  t)

;;; --- typing controller (pure state) ---------------------------------------
;;; Discord shows "is typing…" ~10s per POST; an in-flight turn re-asserts it
;;; every +TYPING-REFRESH-MS+. The cap (+TYPING-CAP-MS+) measures the turn's
;;; SILENCE, not the indicator's age: the tick hands it the surface's newest
;;; sighting (DIGEST-SEEN-MS — the same stamp the stall notice reads), beats
;;; keep going while the turn keeps producing, and stop once it has shown
;;; nothing for the cap — a wedged turn cannot type forever, and a long
;;; working one never loses its bubble mid-turn.

(defparameter +typing-refresh-ms+ 8000)
(defparameter +typing-cap-ms+ 300000
  "How long a surface's turn may show nothing before its typing beat stops:
a silence cap, not an age cap — a turn that keeps producing keeps its beat
however long it runs.")

(nlk:define-record (typing-state (:copier nil) (:export :constructor))
  (active-p nil :type boolean)
  (started-at-ms 0 :type integer)
  (last-sent-ms 0 :type integer))

(defun typing-note-started (state now)
  (setf (typing-state-active-p state) t
        (typing-state-started-at-ms state) now
        (typing-state-last-sent-ms state) 0)
  state)

(defun typing-note-stopped (state)
  (setf (typing-state-active-p state) nil)
  state)

(defun typing-due-p (state now &key (refresh-ms +typing-refresh-ms+) seen-at-ms)
  "True when a typing POST should go out now: active, within the cap of the
turn's newest sighting — SEEN-AT-MS when the caller has one (the surface's
newest production, DIGEST-SEEN-MS), the start otherwise — and REFRESH-MS
past the last send (a fresh start, last-sent 0, is immediately due)."
  ;; The refresh is the platform's: Discord shows a beat for ~10 s, Telegram
  ;; for ~5 s.
  (and (typing-state-active-p state)
       (< (- now (max (typing-state-started-at-ms state) (or seen-at-ms 0)))
          +typing-cap-ms+)
       (or (zerop (typing-state-last-sent-ms state))
           (>= (- now (typing-state-last-sent-ms state)) refresh-ms))))

(defun typing-note-sent (state now)
  (setf (typing-state-last-sent-ms state) now)
  state)

;;; --- lane registry --------------------------------------------------------
;;; One lane per deterministic session id: the platform target to deliver to,
;;; the trigger message for reply threading, and the per-turn digest. Adapters
;;; must stay independent of iteration order (MAP-LANES walks a hash table).
;;;
;;; A lane may be a fork: PARENT-SESSION-ID names the session it composes its
;;; history from and writes its settled exchange back to. The address book
;;; beside the table maps platform message ids to the lane that owns them, so
;;; a reply gesture on any message the lane produced routes back to it — the
;;; branch structure of a flat channel, without platform threads.

(nlk:define-record (channel-lane (:copier nil) (:conc-name lane-)
                                 (:constructor %make-channel-lane)
                                 (:export session-id target trigger-message-id parent-session-id
                                          owner-id last-active-ms prompt active-turn-id digest
                                          reactions lock where said))
  (session-id (error "session-id required") :type string)
  ;; Adapter-owned plist: (:channel-id ... :thread-id ...) etc.
  (target '() :type list)
  (trigger-message-id nil :type (or null string))
  ;; The session this lane forked from and writes its exchange back to.
  (parent-session-id nil :type (or null string))
  ;; The platform user whose message opened the lane: the fairness key of an
  ;; admission scheduler, and the speaker of the write-back. Never part of
  ;; the session id — provenance is data, not topology.
  (owner-id nil :type (or null string))
  ;; Newest activity, for idle reaping.
  (last-active-ms 0 :type integer)
  ;; The text the lane's current turn was asked, verbatim — the user half of
  ;; a write-back into PARENT-SESSION-ID. Set from the ask at admission and
  ;; from each TURN.STARTED after that: a steer promotes into a turn of its
  ;; own, so the lane's prompt follows the turn and never accumulates.
  (prompt nil :type (or null string))
  ;; A delivery job for this lane is already queued or running: the tick
  ;; plans at most one outstanding flush per lane, so a pool of drainers
  ;; cannot double-post one status line.
  (flush-in-flight-p nil :type boolean)
  (active-turn-id nil :type (or null string))
  ;; Per-turn delivery digest (digest.lisp): the status-line + final-answer
  ;; fold. Guarded by the lane lock.
  (digest nil)
  ;; The reactions the bot has left, platform message id -> emoji, and
  ;; whether the platform refused one — the lane then stops reacting
  ;; (host.lisp, opt-in). Guarded by the lane lock.
  (reactions '() :type list)
  (reactions-refused-p nil :type boolean)
  (lock nil)
  ;; The platform's sentence about where this lane runs — guild, channel,
  ;; thread, the bot's own id — carried here so the live-section read can
  ;; find it by session id (LANE-WHERE-SECTION). It rides BEHIND the
  ;; history: it is true of this lane alone, and every byte ahead of the
  ;; history is the prefix all the room's lanes share (nc-private#35).
  ;; Appended last: a slot appended to a structure is a redefinition a live
  ;; image takes, while one inserted among the others leaves every accessor
  ;; reading past it.
  (where nil :type (or null string))
  ;; How the newest ask was said (ASK-VOICE): its answer's voice message
  ;; turns on it (LANE-VOICE-REPLIES). Appended last, for the reason WHERE is.
  (voice nil :type (member nil :note :channel))
  ;; The agent the lane runs as (AGENT-FOR), NIL the channel itself: a reply
  ;; to the lane goes on as it, whoever wrote it. Appended last, for the
  ;; reason WHERE is.
  (agent nil :type (or null string))
  ;; The newest ask's own words (ASK-SAID): what its turn's card is named
  ;; from. Appended last, for the reason WHERE is.
  (said "" :type string))

(nlk:access (lane channel-lane))

(defstruct (lane-table (:copier nil)
                       (:constructor make-lane-table (name &aux (lock (bt2:make-lock :name name)))))
  (table (make-hash-table :test #'equal))
  ;; Platform message id -> session id. Every message a lane produced or was
  ;; opened by is an address for it.
  (addresses (make-hash-table :test #'equal))
  (lock nil))

(nlk:access (lanes lane-table))

(defun lane-delivery-target (lane target)
  "TARGET as LANE's refresh of where it delivers — the surface the message
that just arrived was typed on — except for a lane that answers from a
thread: it keeps the thread."
  ;; A reply to the ask that opened the thread, and a reply to the pointer
  ;; line the channel kept, are both typed in the PARENT channel; neither is
  ;; where the work runs, and letting the reply move the lane there would post
  ;; its next answer beside the ask with no thread around it. Every other lane
  ;; takes the newest target: a reply is typed on its own surface, where the
  ;; refresh changes nothing.
  (if (getf lane.target :thread-id)
      lane.target
      target))

(defun intern-lane (lanes session-id &key target trigger-message-id
                                          parent-session-id owner-id)
  "The lane for SESSION-ID, created on first sight."
  ;; TARGET and TRIGGER-MESSAGE-ID refresh on every call: replies land where
  ;; the newest triggering message came from — except that a lane that answers
  ;; from a thread keeps its thread (LANE-DELIVERY-TARGET), because the ask a
  ;; reply addresses lives outside it. PARENT-SESSION-ID and OWNER-ID are set
  ;; on creation and left alone afterwards — a lane's lineage and its opener
  ;; do not change under it.
  (bt2:with-lock-held ((lane-table-lock lanes))
    (let ((lane (alexandria:ensure-gethash session-id lanes.table
                                           (%make-channel-lane
                                            :session-id session-id
                                            :parent-session-id parent-session-id
                                            :owner-id owner-id
                                            :last-active-ms (now-ms)
                                            :lock (bt2:make-lock :name session-id)))))
      (when target
        (setf lane.target (lane-delivery-target lane target)))
      (when trigger-message-id
        (setf lane.trigger-message-id trigger-message-id))
      (setf lane.last-active-ms (now-ms))
      lane)))

(defun find-lane (lanes session-id)
  (bt2:with-lock-held ((lane-table-lock lanes))
    (gethash session-id lanes.table)))

(defun bind-lane-address (lanes address session-id)
  "Route ADDRESS — one platform message id — back to SESSION-ID's lane."
  (when (and (stringp address) (stringp session-id))
    (bt2:with-lock-held ((lane-table-lock lanes))
      (setf (gethash address lanes.addresses) session-id)))
  address)

(defun lane-for-address (lanes address)
  "The live lane owning ADDRESS, or NIL."
  ;; An address whose lane was reaped resolves to nothing — the reply opens a
  ;; fresh lane instead.
  (when (stringp address)
    (bt2:with-lock-held ((lane-table-lock lanes))
      (let ((session-id (gethash address lanes.addresses)))
        (and session-id (gethash session-id lanes.table))))))

;;; --- the ask a lane opened a thread for ---------------------------------------
;;; A lane the kit put in a thread of its own keeps the ask that opened it:
;;; the room the pointer goes to while the turn works, the room the answer
;;; surfaces in when it lands, and why that answer carries no reference of
;;; its own — the message it answers lives in the parent channel, not in the
;;; thread it hangs off. It rides a table rather than a CHANNEL-LANE slot for
;;; the reason *RECORDED-TURNS* does: lanes are live the moment their adapter
;;; runs, and the kit redefines no structure a running image already holds
;;; instances of. Spent when the answer lands, and dropped with the lane.

;;; :ROOM is the room the ask was typed in as ROOM-SESSION-ID names it — the
;;; record its exchange settles into beside the thread's own once the answer
;;; lands there (FINISH-TURN). :POINTER-ID rides while the pointer is up, and
;;; :LANDED says the answer reached the room the ask was typed in. The entry
;;; OUTLIVES both: what a thread lane is — a lane whose ask lives outside its
;;; own room — is a fact about every later post from it, not only about the
;;; first answer.
(nlk:define-side-table lane-origin (:test #'eq :synchronized t)
  "Lane -> (:channel-id :message-id :thread-id :room :landed [:note]
[:pointer-id]) for a lane the kit opened a thread for: where its ask was
typed, its message and the thread it opened — the answer landed there or
not. Empty for every lane that runs where its ask was typed.")

(defun lane-thread-origin (lane &aux (origin (lane-origin lane)))
  "The :channel-id/:message-id/:thread-id the lane still owes the ask's room
an answer for, or NIL — for a lane that never opened a thread, and for one
whose answer has landed there."
  (and origin (not (getf origin :landed)) origin))

(defun lane-reference (lane message-id)
  "MESSAGE-ID as the message a post from LANE may reference, or NIL when the
post may carry none: a lane the kit put in a thread answers from there, and
the ask it answers lives in the parent channel — or in the room the ask was
typed in, when the kit ROUTED it — neither of which is a message the thread
holds."
  ;; A platform refuses the whole post over a reference it cannot resolve, so
  ;; the line that says whose answer this is does not need one: the thread is
  ;; the lane.
  (if (lane-origin lane) nil message-id))

(defun forget-thread-origin (lane)
  "The answer landed in the ask's room: the lane owes nothing more."
  ;; Everything the origin says stays, because a thread lane keeps answering
  ;; without a reference.
  (nlk:when-let (origin (lane-origin lane))
    (setf (lane-origin lane) (list* :landed t origin)))
  nil)

(defun note-thread-spoken (thread-id)
  "Something the room can read reached the thread THREAD-ID: a word the turn
said out loud or a line somebody typed."
  ;; Whatever lane the kit opened that thread for holds something now, and a
  ;; silent answer never takes it away (LANE-EMPTY-THREAD) — the alternative
  ;; deletes what the room already read.
  ;;
  ;; Keyed on the THREAD and not on a lane because a line typed in a thread
  ;; opens a lane of its own: the lane that owns the thread is never the lane
  ;; the line arrives on. One entry per thread lane, so the walk is the table.
  (when thread-id
    (loop for lane being the hash-keys of *lane-origin*
            using (hash-value origin)
          when (and (equal thread-id (getf origin :thread-id))
                    (not (getf origin :spoken)))
            ;; Rewritten in the table for the FORGET-THREAD-ORIGIN reason: SETF
            ;; over the local would leave the table saying it is still empty.
            do (setf (lane-origin lane) (list* :spoken t origin))))
  nil)

(defun lane-empty-thread (lane &aux (origin (lane-origin lane)))
  "The origin of a thread the kit opened for LANE's ask that nothing but
chrome ever went into, or NIL."
  ;; A thread carrying a landed answer, a word the turn spoke or a line
  ;; somebody typed is not empty; neither is a lane the kit never opened a
  ;; thread for. What a silent answer retires (RETIRE-THREAD).
  (and origin
       (not (getf origin :landed))
       (not (getf origin :spoken))
       (getf origin :thread-id)
       origin))

;;; --- the files a turn handed to its answer ------------------------------------
;;; ANSWER-FILE (host.lisp) records here, and the terminal delivery takes
;;; what its own turn recorded: an answer carries the files its turn handed
;;; over, wherever the answer lands. A table rather than a lane slot for the
;;; LANE-ORIGIN reason: lanes are live the moment their adapter runs,
;;; and the kit redefines no structure a running image already holds
;;; instances of.

(defvar *answer-files* (make-hash-table :test #'equal :synchronized t)
  "Session id -> (:turn-id ID :files (PATHNAME ...)): the files a running
turn handed to its answer with ANSWER-FILE, in the order they were handed.")

(defun take-answer-files (lane turn-id &aux (entry (gethash lane.session-id *answer-files*)))
  "The files LANE's TURN handed to its answer, in the order they were
handed — NIL when it handed none."
  ;; The delivery TAKES them: an answer carries its files once. An entry
  ;; another turn left behind is dropped as this one is taken, so a turn that
  ;; ended without delivering cannot leak a picture into the next answer.
  (remhash lane.session-id *answer-files*)
  (and entry (equal turn-id (getf entry :turn-id)) (getf entry :files)))

(defun add-answer-files (lane turn-id paths &aux (entry (gethash lane.session-id *answer-files*)))
  "Hand PATHS to the answer LANE's TURN-ID delivers, after the files that turn
handed already. => every file the answer now carries, in the order handed."
  (let ((files (append (and (equal turn-id (getf entry :turn-id)) (getf entry :files)) paths)))
    (setf (gethash lane.session-id *answer-files*) (list :turn-id turn-id :files files))
    files))

(defvar *answer-choices* (make-hash-table :test #'equal :synchronized t)
  "Session id -> (:turn-id ID :labels (LABEL ...)): the choices a running turn
handed to its answer with ANSWER-CHOICES, the last set handed.")

(defun take-answer-choices (lane turn-id &aux (entry (gethash lane.session-id *answer-choices*)))
  "The choices LANE's TURN-ID handed to its answer, or NIL — taken, as files
are (TAKE-ANSWER-FILES)."
  (remhash lane.session-id *answer-choices*)
  (and entry (equal turn-id (getf entry :turn-id)) (getf entry :labels)))

;;; An answer that waits on the person it answers -- a question, a go or no-go --
;;; is marked so by its turn (ANSWER-WAITS; choices on it mark it too), never
;;; guessed from its words: a fact in the session's log (NLE:ANSWER-WAITS),
;;; which every listing of sessions reads. The mark is written down beside the
;;; messages the answer posted as, too, so whoever reads the room's newest
;;; message reads whether it waits: a reply after it moves the room on, and
;;; the mark with it.

(defun answer-mark (lane turn-id)
  "(values WAITS-P QUESTION) of the mark LANE's TURN-ID put on its answer
(ANSWER-WAITS: the session's newest, NLK:SESSION-WAITS), QUESTION NIL for the
answer's own last line."
  (multiple-value-bind (question turn) (and (nlk:store-open-p) (nlk:session-waits lane.session-id))
    (if (and question (equal turn turn-id))
        (values t (and (plusp (length question)) question))
        (values nil nil))))

(defun mark-choices-waiting (lane turn-id)
  "Mark LANE's TURN-ID answer, posted with choices on it, as waiting: choices
wait on whoever presses one, whether or not the turn said so."
  ;; As NOTE-ANSWER-WAITING: the answer has posted, and a store that refuses
  ;; the mark costs the listing one ball, never the answer.
  (when (nlk:store-open-p)
    (nlk:with-handlers ((error (condition)
                          (warn "could not mark ~a waiting: ~a" lane.session-id condition)))
      (nle:answer-waits :session lane.session-id :turn turn-id))))

(defun waiting-key (room message-id)
  "The state key the mark on MESSAGE-ID, posted in ROOM, is written under."
  (format nil "waiting:~a:~a" room message-id))

(defun note-answer-waiting (host room message-ids question)
  "Write down that the answer HOST posted in ROOM as MESSAGE-IDS waits on the
person it answers, on QUESTION -- or, with none, on its own last line."
  ;; The answer has posted: a store that refuses the mark costs the board one
  ;; line, never the answer's card.
  (when (nlk:store-open-p)
    (nlk:with-handlers ((error (condition)
                          (warn "~a could not mark ~a waiting: ~a" (host-id host) room condition)))
      (let ((json (nlk:encode-json-object (nlk:json-object "question" (or question "")))))
        (dolist (id message-ids)
          (nlk:session-state-put (format nil "channel-~a" (host-id host)) (waiting-key room id) json))))))

(defun answer-waiting (host room message-id)
  "What the answer HOST posted in ROOM as MESSAGE-ID waits on: the question
its turn named, \"\" for its own last line, or NIL when it waits on nobody."
  (let ((json (and (nlk:store-open-p)
                   (nlk:session-state-get (format nil "channel-~a" (host-id host))
                                          (waiting-key room message-id)))))
    (and json (or (nlk:json-value (nlk:decode-json json) :string "question") ""))))

;;; Discord takes ten attachments on one message and refuses a plan that names
;;; more, so no delivery builds one.
(defparameter +answer-file-count-cap+ 10
  "Files one answer's message carries.")

(defparameter +answer-file-byte-cap+ (* 8 1024 1024)
  "Largest file one answer attaches, in bytes — under Discord's default
upload allowance of 10 MiB, so one oversize file cannot take a whole answer
down with it. A larger file stays on the host for POST-FILE.")

(defun attachable-file-size (pathname)
  "PATHNAME's size in bytes when it names an existing regular file; NIL for
a directory, a missing path, or anything unreadable."
  ;; LSTAT keeps the check off the file's contents — a submodule or a dangling
  ;; path never rides an answer.
  (nlk:with-handlers ((error () nil))
    (let ((stat (sb-posix:lstat (namestring pathname))))
      (when (sb-posix:s-isreg (sb-posix:stat-mode stat))
        (sb-posix:stat-size stat)))))

(defun collect-answer-files (lane turn-id &aux (files '()))
  "The files one answer carries: what the turn handed with ANSWER-FILE —
the model's explicit choice, in the order handed."
  ;; TAKEN, so an answer carries its files once; existing regular files only,
  ;; never more than one message's practical cap.
  (dolist (path (take-answer-files lane turn-id))
    (when (< (length files) +answer-file-count-cap+)
      (let* ((probe (ignore-errors (probe-file path)))
             (size (and probe (attachable-file-size probe))))
        (cond ((null size) (warn "~a answer file ~a is gone or names no file" lane.session-id path))
              ((> size +answer-file-byte-cap+)
               (warn "~a answer file ~a is ~:d bytes, over the attach cap"
                     lane.session-id probe size))
              ((member probe files :test #'equal) nil)
              (t (setf files (append files (list probe))))))))
  files)

;;; A lane is a fork frozen at its room's head, so a head move on the room — a
;;; /new, an /undo, a rewind from a shell — leaves it composing a record the
;;; room no longer holds. A settled one is dropped at once (RETIRE-ROOM-LANES);
;;; one still running answers its turn and goes with the next reap, and until
;;; then no line talks to it without a reply gesture (CONVERSATION-LANE). A
;;; table rather than a slot, for the LANE-ORIGIN reason.
(nlk:define-side-table lane-retired (:test #'eq :synchronized t)
  "Lane -> T once its room's head moved under it.")

(defun remove-lane (lanes session-id)
  "Drop the lane and every address that routed to it."
  (bt2:with-lock-held ((lane-table-lock lanes))
    (let ((addresses lanes.addresses) (lane (gethash session-id lanes.table)))
      (when lane
        ;; The lane is being reaped: forget its thread origin entirely.
        (forget-lane-origin lane)
        (forget-lane-retired lane)
        (remhash lane.session-id *answer-files*)
        (remhash lane.session-id *answer-choices*))
      (remhash session-id lanes.table)
      ;; Removing the entry the walk stands on is the one change a walk allows.
      (loop for address being the hash-keys of addresses
              using (hash-value owner)
            when (equal owner session-id) do (remhash address addresses))))
  t)

(defun lane-count (lanes)
  (bt2:with-lock-held ((lane-table-lock lanes))
    (hash-table-count lanes.table)))

(defun map-lanes (lanes function)
  "Call FUNCTION with each lane."
  ;; Iteration order is unspecified; callers stay order-independent. The
  ;; lanes are taken under the table lock and called outside it.
  (mapc function (bt2:with-lock-held ((lane-table-lock lanes))
                   (loop for lane being the hash-values of lanes.table collect lane)))
  (values))
