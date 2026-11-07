;;;; board.lisp --- the thread board: one message saying which threads wait on whom.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; channels.discord.board_channel names a channel the bot keeps ONE message
;;;; in, edited in place: every open thread the bot started, by who moves next
;;;; -- the sections, a thread's state and when it closes are the kit's
;;;; (NCK:BOARD-ROW); this file reads Discord's threads and draws the message, a
;;;; stacked list that reads the same on a phone as on a desktop.
;;;;
;;;; Closing is Discord's archive: the thread leaves the
;;;; channel's open threads, kept and searchable, and opens again once anyone
;;;; writes in it. An answer's own Close button (the kit's CLOSE-PRESSED)
;;;; closes one at once; the board's Close all done closes every Done thread.
;;;;
;;;; One authority per fact, and the board owns none of them: open or closed
;;;; is Discord's archive, who spoke last and what is the thread's own newest
;;;; message, running and waiting are the kit's lanes. The board keeps only its
;;;; message's id, in the store, so a restart edits the same message. A pass
;;;; reads the guild's open threads once and a thread's newest words only when
;;;; its newest message moved, and edits the message only when what it says
;;;; changed.

(in-package #:nodecode-channel-discord)

(defparameter +board-pass-seconds+ 60
  "How often the board reads the threads again.")

(defparameter +board-closes-per-pass+ 40
  "The most threads one pass closes: a backlog closes over several passes.")

(defparameter +board-state+ "channel-board"
  "The state namespace the board keeps its message's id in, one key a board
channel.")

(defparameter +board-accents+
  '(:needs #xFA8B3E :noans #xF23F43 :running #x23A55A :done #x80848E)
  "Each of the kit's sections (NCK:+BOARD-SECTIONS+) by the accent its
container wears.")

(defstruct (board (:copier nil) (:constructor %make-board))
  (adapter nil)
  (channel "" :type string)
  (guild nil)
  ;; Thread id -> (LAST-MESSAGE-ID BOT-P TEXT SAID-ID): its newest words, the
  ;; message they are, and the newest message they were read at.
  (heard (make-hash-table :test #'equal :synchronized t))
  (message-id nil)
  ;; The JSON of the message as last sent, so a pass that changes nothing
  ;; edits nothing.
  (sent nil)
  ;; The newest pass's open threads, (:id :state :quiet-ms :question) each.
  (rows '() :type list)
  ;; Threads past their quiet the newest pass left for the next.
  (closing 0 :type integer)
  ;; Wakes the board's thread: a press that changed something, or the stop.
  (queue (make-work-queue "channel-discord-board" :cap 16))
  (stop-p nil))

(defvar *board* nil
  "The live thread board, or NIL when no board_channel is set.")

;;; --- the pure halves ------------------------------------------------------------------

(defun board-line (row &key question)
  "ROW as the board's line: the thread's link and its age -- running or
waiting for one whose turn does (NCK:BOARD-WHEN) -- and, with QUESTION, the
question it ends on in small print under it."
  (format nil "<#~a> · ~a~@[~%-# ~a~]"
          (getf row :id)
          (board-when row)
          (and question (getf row :question) (discord-escape (getf row :question)))))

(defun board-list-line (row question-p)
  "ROW as Show all and /threads list it (NCK:BOARD-LIST-TEXT): its line, and
with QUESTION-P the question it waits on after a dash."
  (format nil "~a~@[ — ~a~]" (board-line row)
          (and question-p (getf row :question) (discord-escape (getf row :question)))))

(defun board-section-data (key)
  "The data a section's own Show all sends: ncd:board:all:<section>."
  (format nil "ncd:board:all:~(~a~)" key))

(defun board-show-text (data rows)
  "What a Show all press sending DATA answers: every one of ROWS by section
for the board's own, every thread of one section for a section's
(BOARD-SECTION-DATA) -- never cut, however many messages it takes -- or NIL
for a press that is no Show all this board draws."
  (flet ((listed (rows) (board-list-text rows #'board-list-line :limit most-positive-fixnum)))
    (if (equal data "ncd:board:all")
        (listed rows)
        (nlk:when-let (section (find data +board-sections+
                                     :key (lambda (section) (board-section-data (first section)))
                                     :test #'equal))
          (let ((members (board-section-rows rows (first section))))
            (if members
                (listed members)
                (format nil "No thread is under ~a now." (second section))))))))

(defun board-components (rows &key (closing 0))
  "ROWS, the open threads newest first, as the board's Components V2 message:
a heading; for each section holding a thread, a container wearing its
accent, its name and count over its newest +BOARD-ROWS+ threads (Needs you's
each with the question it ends on), and a section holding more its own Show
all; the counts in small print, CLOSING the threads left to close; Show all,
and Close all done while any is done."
  ;; Discord's caps on one message: 40 components however nested, 4000
  ;; characters across its text. Four sections of a text and a one-button row
  ;; each, a heading, a footer and two buttons -- 21 -- stay inside both.
  (let* ((sections
           (loop for (key name) in +board-sections+
                 for accent = (getf +board-accents+ key)
                 for members = (board-section-rows rows key)
                 for shown = (subseq members 0 (min +board-rows+ (length members)))
                 when members
                   collect (nlk:json-object
                            "type" 17 "accent_color" accent
                            "components"
                            (coerce
                             (cons (text-display
                                    (format nil "**~a** · ~d~%~{~a~^~%~}"
                                            name (length members)
                                            (mapcar (lambda (row) (board-line row :question (eq key :needs)))
                                                    shown))
                                    850)
                                   ;; The threads this section leaves out are one
                                   ;; press away, under the section they belong to.
                                   (and (> (length members) (length shown))
                                        (list (nlk:json-object
                                               "type" 1
                                               "components"
                                               (vector (control-button
                                                        (list (format nil "Show all ~d" (length members))
                                                              (board-section-data key) :secondary)))))))
                             'vector))))
         (done (length (board-section-rows rows :done))))
    (coerce (append
             (list (text-display "### Threads" 40))
             (or sections (list (text-display "No thread is open." 40)))
             (list (text-display (format nil "-# ~d open~[~:;~:* · ~d more closing, quiet too long~] · done threads close after ~d h quiet, any after ~d days"
                                         (length rows) closing
                                         (floor +board-done-close-ms+ 3600000)
                                         (floor +board-quiet-close-ms+ 86400000))
                                 200)
                   (nlk:json-object
                    "type" 1
                    "components"
                    (coerce (append (list (control-button (list "Show all" "ncd:board:all" :secondary)))
                                    (and (plusp done)
                                         (list (control-button (list (format nil "Close all done (~d)" done)
                                                                     "ncd:board:done" :danger)))))
                            'vector))))
            'vector)))

;;; --- the board's pass -------------------------------------------------------------------

(defun board-call (board method path &optional body)
  "One REST call as the bot: (values OK BODY STATUS ERROR)."
  (let ((result (execute-plan (host-executor (discord-adapter-host (board-adapter board)))
                              (rest-plan method path "board" t 30 body))))
    (values (execution-ok-p result) (execution-body result)
            (execution-status result) (execution-error result))))

(defun board-words (board thread-id last-id &aux (bot (discord-adapter-bot-user-id (board-adapter board)))
                                                 (known (gethash thread-id (board-heard board))))
  "(values BOT-P TEXT READ-P SAID-ID) of THREAD-ID's newest words: its newest
message with words or files in it, SAID-ID, a card's wordless message passed
over -- read again only when the thread's newest message, LAST-ID, moved.
READ-P NIL: the thread could not be read."
  (if (and known (equal (first known) last-id))
      (values (second known) (third known) t (fourth known))
      (multiple-value-bind (ok body) (board-call board "GET" (format nil "/channels/~a/messages?limit=10" thread-id))
        (if (not (and ok (vectorp body)))
            (values nil nil nil)
            (let* ((said (find-if (lambda (message)
                                    (and (member (nlk:json-value message :integer "type") '(0 19))
                                         (or (plusp (length (nlk:trimmed (or (nlk:json-value message :string "content") ""))))
                                             (plusp (length (or (nlk:json-value message :array "attachments") #()))))))
                                  body))
                   (bot-p (and said (equal bot (nlk:json-value said :string "author" "id"))))
                   (text (and said (nlk:json-value said :string "content")))
                   (said-id (and said (nlk:json-value said :string "id"))))
              (setf (gethash thread-id (board-heard board)) (list last-id bot-p text said-id))
              (values bot-p text t said-id))))))

(defun board-close (board thread-id)
  "Close THREAD-ID (THREAD-CLOSE-PLAN). => whether it closed."
  (let ((result (execute-plan (host-executor (discord-adapter-host (board-adapter board)))
                              (thread-close-plan thread-id))))
    (or (execution-ok-p result)
        (progn (warn "discord: the board could not close thread ~a: ~a" thread-id (execution-error result))
               nil))))

(defun board-pass (board &aux (adapter (board-adapter board)) (now (unix-ms)))
  "Read the open threads, close those past their quiet, and say the rest in
the board's message. => the open threads' rows."
  (unless (board-guild board)
    (multiple-value-bind (ok body) (board-call board "GET" (format nil "/channels/~a" (board-channel board)))
      (when ok (setf (board-guild board) (nlk:json-value body :string "guild_id")))))
  (nlk:when-let (guild (board-guild board))
    (multiple-value-bind (ok body status error)
        (board-call board "GET" (format nil "/guilds/~a/threads/active" guild))
      (unless ok
        (warn "discord: the board could not read the open threads (~a): ~a" status error)
        (return-from board-pass nil))
      (let ((phases (board-lane-phases adapter.host))
            (rows '())
            (closing '()))
        (loop for thread across (or (nlk:json-value body :array "threads") #())
              for id = (nlk:json-value thread :string "id")
              for last = (or (nlk:json-value thread :string "last_message_id") id)
              for quiet = (- now (snowflake-ms last))
              for phase = (gethash id phases)
              when (equal adapter.bot-user-id (nlk:json-value thread :string "owner_id"))
                do (if (and (null phase) (>= quiet +board-quiet-close-ms+))
                       ;; Quiet past any thread's time: it closes whatever was said last.
                       (push id closing)
                       (multiple-value-bind (bot-p text read-p said-id) (board-words board id last)
                         (when (or read-p phase)
                           ;; The mark is read each pass, never kept: an answer
                           ;; read the moment it posted is marked a moment later.
                           (multiple-value-bind (row close-p)
                               (board-row id :phase phase :bot-p bot-p :text text :quiet-ms quiet
                                             :waits (and bot-p said-id
                                                         (answer-waiting adapter.host id said-id)))
                             (cond (row (push row rows))
                                   (close-p (push id closing))))))))
        (let ((closed (loop for id in closing
                            repeat +board-closes-per-pass+
                            count (board-close board id))))
          (setf (board-rows board) (sort rows #'< :key (lambda (row) (getf row :quiet-ms)))
                (board-closing board) (- (length closing) closed)))
        (board-publish board)
        (board-rows board)))))

(defun board-publish (board &aux (channel (board-channel board)))
  "Say the newest pass in the board's message: edit it in place, or post it
when there is none yet or it was deleted."
  (let* ((body (nlk:json-object "flags" +discord-components-v2+
                                "components" (board-components (board-rows board)
                                                               :closing (board-closing board))
                                "allowed_mentions" (nlk:json-object "parse" #())))
         (json (nlk:encode-json-object body))
         (id (board-message-id board)))
    (unless (equal json (board-sent board))
      (multiple-value-bind (edited _ status error)
          (if id
              (board-call board "PATCH" (format nil "/channels/~a/messages/~a" channel id) body)
              (values nil nil 404 nil))
        (declare (ignore _))
        (cond (edited (setf (board-sent board) json))
              ;; Only a message that is gone is posted again: any other refusal
              ;; keeps the one board, and the next pass tries the edit again.
              ((eql status 404)
               (multiple-value-bind (posted message) (board-call board "POST" (format nil "/channels/~a/messages" channel) body)
                 (if posted
                     (let ((new (nlk:json-value message :string "id")))
                       (setf (board-message-id board) new (board-sent board) json)
                       (when (nlk:store-open-p)
                         (nlk:session-state-put +board-state+ channel new)))
                     (warn "discord: the board could not post in ~a" channel))))
              (t (warn "discord: the board could not edit its message (~a): ~a" status error)))))))

;;; --- its buttons and its command -------------------------------------------------------

(defun board-pressed (press &aux (data (getf press :data)) (board *board*))
  "Adapter thread: a press on one of the board's own buttons, answered on a
thread of its own -- Show all lists every open thread, a section's Show all
every one of that section, to the presser alone and whole (BOARD-SHOW-TEXT);
Close all done, a room operator's, closes every Done thread -- or NIL for any
other press."
  (when (and board (stringp data) (eql 0 (search "ncd:board:" data)))
    (nlk:spawn "channel-discord-board-press"
      (nlk:with-handlers ((error (condition) (warn "discord: a board press failed: ~a" condition)))
        (let* ((host (discord-adapter-host (board-adapter board)))
               (rows (board-rows board))
               (done (board-section-rows rows :done))
               (closer (and (equal data "ncd:board:done") (operator-p host (getf press :user-id)))))
          (execute-delivery (interaction-ack-plans
                             press
                             :whole t
                             :text (cond ((board-show-text data rows))
                                         ((not (equal data "ncd:board:done")) "This button is out of date.")
                                         ((not closer) "Only the room's operators can close threads from the board.")
                                         ((null done) "No thread is done.")
                                         (t (format nil "Closing ~d done thread~:p." (length done)))))
                            (host-executor host))
          (when (and closer done)
            (dolist (row done) (board-close board (getf row :id)))
            (queue-push (board-queue board) :pass)))))
    t))

(defun start-board (adapter section &aux (channel (config-string section "board_channel")))
  "Start the thread board channels.discord.board_channel names, on a thread
of its own. => its stop thunk, or NIL when no board channel is named."
  (when (plusp (length channel))
    (let ((board (%make-board :adapter adapter :channel channel
                              :message-id (and (nlk:store-open-p)
                                               (nlk:session-state-get +board-state+ channel)))))
      ;; One board edits the message: a board already running stands down.
      (nlk:when-let (old *board*)
        (setf (board-stop-p old) t)
        (queue-push (board-queue old) :stop))
      (setf *board* board)
      ;; /threads is the kit's; it answers this board's list.
      (let ((thunk (register-board "discord" (lambda () (board-list-text (board-rows board) #'board-list-line)))))
        (nlk:spawn "channel-discord-board"
          (loop until (board-stop-p board)
                do (nlk:with-handlers ((error (condition) (warn "discord: a board pass failed: ~a" condition)))
                     (board-pass board))
                   (queue-pop (board-queue board) +board-pass-seconds+)))
        (lambda ()
          (setf (board-stop-p board) t)
          (queue-push (board-queue board) :stop)
          (unregister-board "discord" thunk)
          (when (eq *board* board)
            (setf *board* nil)))))))
