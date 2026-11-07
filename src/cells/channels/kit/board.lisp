;;;; board.lisp --- what a thread board says, on every platform that keeps one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A thread board is one message a platform keeps, edited in place: every
;;;; open thread the bot started, by who moves next. What a thread's state is,
;;;; when it closes by itself and how its age and question read are the kit's
;;;; (here); reading the threads and drawing the message are the platform's
;;;; (Discord's board.lisp reads its archive and draws Components V2;
;;;; Telegram's reads the topics it opened and draws a rich message).
;;;;
;;;; Needs you: the thread's newest words are an answer of the bot's its turn
;;;; marked waiting on someone (ANSWER-WAITS, or choices on it) -- marked,
;;;; never guessed from a question mark.
;;;; No answer: someone else spoke last, or nobody did, and nothing runs there.
;;;; Running: a turn runs there, or waits to. Done: the bot answered and asked
;;;; nothing. Most urgent first.
;;;;
;;;; A board closes what is finished: a Done thread after 48 hours quiet, any
;;;; other after 7 days, never one a turn runs or waits in. /threads answers
;;;; the board of the platform it is typed on (every board, typed elsewhere).

(in-package #:nodecode-channel-kit)

(defparameter +board-done-close-ms+ (* 48 60 60 1000)
  "How long a Done thread stays open with nothing said in it.")

(defparameter +board-quiet-close-ms+ (* 7 24 60 60 1000)
  "How long any thread stays open with nothing said in it.")

(defparameter +board-rows+ 5
  "The threads a section shows, newest first; Show all lists every one.")

(defparameter +board-sections+
  '((:needs "Needs you") (:noans "No answer") (:running "Running") (:done "Done"))
  "The board's sections in the order it shows them: the state and its name.
Running holds the threads whose turn waits, too.")

(defun board-said-lines (text)
  "TEXT's lines with words in them, small print (`-#') left out."
  (remove-if (lambda (line) (or (zerop (length line)) (eql 0 (search "-#" line))))
             (mapcar #'nlk:trimmed (uiop:split-string (or text "") :separator '(#\Newline)))))

(defun board-state (&key phase bot-p waits)
  "One thread's state: PHASE, its lane's :RUNNING or :QUEUED, first; else
from its newest words, the bot's when BOT-P: an answer marked waiting
(WAITS, ANSWER-WAITING's answer) :NEEDS, the bot's otherwise :DONE,
anyone else's or none :NOANS."
  (cond ((member phase '(:running :queued)) phase)
        ((not bot-p) :noans)
        (waits :needs)
        (t :done)))

(defun board-close-p (state quiet-ms)
  "Whether a thread in STATE, QUIET-MS since anything was said in it, closes
by itself: never while a turn runs or waits there, a Done one after
+BOARD-DONE-CLOSE-MS+, any other after +BOARD-QUIET-CLOSE-MS+."
  (case state
    ((:running :queued) nil)
    (:done (>= quiet-ms +board-done-close-ms+))
    (t (>= quiet-ms +board-quiet-close-ms+))))

(defun board-age (ms &aux (minutes (floor (max ms 0) 60000)))
  "MS as the board says an age: minutes, then hours, then days."
  (cond ((< minutes 60) (format nil "~dm" (max 1 minutes)))
        ((< minutes (* 48 60)) (format nil "~dh" (floor minutes 60)))
        (t (format nil "~dd" (floor minutes 1440)))))

(defun board-question (text)
  "The question TEXT ends on, as a line the board can show under its thread."
  (nlk:when-let (last (car (last (board-said-lines text))))
    (nlk:clip (ppcre:regex-replace-all "\\*\\*|__|`" (ppcre:regex-replace "^[-*>#]+\\s*" last "") "")
              90 :ellipsis "…")))

(defun board-when (row)
  "How the board dates ROW: running or waiting for one whose turn does, else
its age."
  (case (getf row :state)
    (:running "running")
    (:queued "waiting")
    (t (board-age (getf row :quiet-ms)))))

(defun board-section-rows (rows key)
  "The ROWS section KEY shows: :RUNNING's are those running and waiting."
  (remove-if-not (lambda (row) (member (getf row :state)
                                       (if (eq key :running) '(:running :queued) (list key))))
                 rows))

(defun board-row (id &key phase bot-p waits text quiet-ms)
  "Thread ID's row, (:ID :STATE :QUIET-MS :QUESTION), from its lane's PHASE
and its newest words -- the bot's when BOT-P, TEXT, marked WAITS -- or NIL
when the thread is to close (BOARD-CLOSE-P): as the second value then, T."
  ;; A Needs-you row carries the question it waits on: the one its turn
  ;; named, else the line its answer ends on.
  (let ((state (board-state :phase phase :bot-p bot-p :waits waits)))
    (if (board-close-p state quiet-ms)
        (values nil t)
        (list :id id :state state :quiet-ms quiet-ms
              :question (and (eq state :needs)
                             (if (plusp (length waits))
                                 (nlk:clip waits 90 :ellipsis "…")
                                 (board-question text)))))))

(defun board-lane-phases (host &key (key (lambda (target) (getf target :thread-id)))
                               &aux (phases (make-hash-table :test #'equal)))
  "KEY of a lane's target -> :RUNNING where a turn of HOST's runs in that
thread, :QUEUED where one only waits to. KEY answers NIL for a lane in no
thread; a platform whose thread ids are unique only inside a chat keys by
both."
  (map-lanes (host-lanes host)
             (lambda (lane &aux (thread (funcall key (lane-target lane)))
                                (digest (lane-digest lane)))
               (when thread
                 (cond ((lane-active-turn-id lane)
                        (setf (gethash thread phases) :running))
                       ((and digest (eq :queued (turn-digest-phase digest)))
                        (unless (gethash thread phases)
                          (setf (gethash thread phases) :queued)))))))
  phases)

(defun board-list-text (rows line &key (limit 1900))
  "Every one of ROWS by section, LINE's line a thread -- LINE takes a row and
whether its section shows its question -- cut to LIMIT characters with what
was left out counted: what Show all and /threads answer."
  (let ((lines '()) (left 0) (size 0))
    (loop for (key name) in +board-sections+
          for members = (board-section-rows rows key)
          when members
            do (dolist (text (cons (format nil "**~a** · ~d" name (length members))
                                   (mapcar (lambda (row) (funcall line row (eq key :needs))) members)))
                 (if (< (+ size (length text) 1) limit)
                     (progn (push text lines) (incf size (1+ (length text))))
                     (incf left))))
    (cond ((null rows) "No thread is open.")
          (t (format nil "~{~a~^~%~}~[~:;~:*~%+~d more lines~]" (reverse lines) left)))))

;;; --- /threads ----------------------------------------------------------------------------

(defvar *boards* (make-hash-table :test #'equal :synchronized t)
  "Platform id -> the thunk answering its running board as /threads' text.")

(defun register-board (platform-id thunk)
  "THUNK answers /threads for PLATFORM-ID's board from now on. => THUNK."
  (setf (gethash platform-id *boards*) thunk))

(defun unregister-board (platform-id thunk)
  "PLATFORM-ID's board stops answering /threads -- when THUNK is still the
one answering, so a stale stop never takes a newer board's place."
  (when (eq thunk (gethash platform-id *boards*))
    (remhash platform-id *boards*)))

(defun board-command (args session-id)
  "/threads: every open thread, by who moves next -- the board of the
platform SESSION-ID's room is on, or, typed outside every room, each board
under its platform's name."
  (declare (ignore args))
  (let* ((host (and session-id
                    (find-if (lambda (host) (find-lane (host-lanes host) session-id)) *hosts*)))
         (id (and host (platform-id (host-platform host)))))
    (cond (id (let ((thunk (gethash id *boards*)))
                (if thunk
                    (funcall thunk)
                    (format nil "No board here: channels.~a names no place to keep one." id))))
          ((zerop (hash-table-count *boards*)) "No board is running.")
          (t (format nil "~{~a~^~%~%~}"
                     (loop for platform being the hash-keys of *boards* using (hash-value thunk)
                           collect (format nil "**~a**~%~a" (string-capitalize platform)
                                           (funcall thunk))))))))
