;;;; board-test.lisp --- what a thread board says, on every platform.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A thread's state and when it closes, the row a board draws, the list Show
;;;; all and /threads answer, and /threads finding the board of the room it is
;;;; typed in.

(in-package #:nodecode.test)

(deftest channel-board-sorts-a-thread-by-who-moves-next ()
  ;; A turn running or waiting there decides first; else the newest words:
  ;; the bot's answer marked waiting (NCK:ANSWER-WAITING) needs you, the
  ;; bot's otherwise are done -- a question mark marks nothing -- anyone
  ;; else's, or none, wait for an answer.
  (is (eq :running (nck:board-state :phase :running :bot-p t :waits "Which one?")))
  (is (eq :queued (nck:board-state :phase :queued)))
  (is (eq :needs (nck:board-state :bot-p t :waits "Push the pin?")))
  ;; marked, on the answer's own last line
  (is (eq :needs (nck:board-state :bot-p t :waits "")))
  ;; an unmarked answer waits on nobody, whatever it ends on
  (is (eq :done (nck:board-state :bot-p t)))
  ;; someone else spoke last
  (is (eq :noans (nck:board-state :bot-p nil :waits "Push?")))
  ;; nobody said anything
  (is (eq :noans (nck:board-state))))

(deftest channel-board-closes-what-went-quiet ()
  ;; Never while a turn runs or waits; a done thread after 48 hours quiet;
  ;; any other after 7 days.
  (let ((hour (* 60 60 1000)) (day (* 24 60 60 1000)))
    (is (not (nck:board-close-p :running (* 30 day))))
    (is (not (nck:board-close-p :queued (* 30 day))))
    (is (not (nck:board-close-p :done (* 47 hour))))
    (is (nck:board-close-p :done (* 48 hour)))
    (is (not (nck:board-close-p :needs (* 6 day))))
    (is (nck:board-close-p :needs (* 7 day)))
    (is (nck:board-close-p :noans (* 7 day)))))

(deftest channel-board-row-carries-what-it-waits-on ()
  ;; A Needs-you row carries the question its turn named, else the line its
  ;; answer ends on; a row past its quiet is no row, and says it closes.
  (let ((hour (* 60 60 1000)))
    (is (equal '(:id "7" :state :needs :quiet-ms 3600000 :question "Push the pin?")
               (nck:board-row "7" :bot-p t :waits "Push the pin?" :text "Done." :quiet-ms hour)))
    (is (equal "Which branch?"
               (getf (nck:board-row "7" :bot-p t :waits "" :quiet-ms hour
                                        :text (format nil "I can land it.~%~%**Which branch?**~%-# 3 steps"))
                     :question)))
    (is (null (getf (nck:board-row "7" :bot-p t :text "Which branch?" :quiet-ms hour) :question)))
    (is (eq :running (getf (nck:board-row "7" :phase :running :quiet-ms (* 900 hour)) :state)))
    (multiple-value-bind (row close-p) (nck:board-row "7" :bot-p t :quiet-ms (* 48 hour))
      (is (null row))
      (is (eq t close-p)))
    (is (equal "running" (nck:board-when '(:state :running :quiet-ms 0))))
    (is (equal "waiting" (nck:board-when '(:state :queued :quiet-ms 0))))
    (is (equal "3h" (nck:board-when (list :state :done :quiet-ms (* 3 hour)))))
    (is (equal "3d" (nck:board-when (list :state :noans :quiet-ms (* 72 hour)))))))

(deftest channel-board-list-says-every-section-in-order ()
  ;; Show all and /threads: every thread under its section, most urgent
  ;; first, in the platform's own line, cut with what was left out counted.
  (let ((rows (list (list :id "2" :state :done :quiet-ms 60000)
                    (list :id "1" :state :needs :quiet-ms 3600000 :question "Push it?")
                    (list :id "3" :state :queued :quiet-ms 0)))
        (line (lambda (row question-p)
                (format nil "#~a ~a~@[ ~a~]" (getf row :id) (nck:board-when row)
                        (and question-p (getf row :question))))))
    (is (equal (format nil "**Needs you** · 1~%#1 1h Push it?~%**Running** · 1~%#3 waiting~%**Done** · 1~%#2 1m")
               (nck:board-list-text rows line)))
    (is (equal "No thread is open." (nck:board-list-text '() line)))
    (let ((cut (nck:board-list-text (loop for i below 300 collect (list :id (format nil "~d" i) :state :done :quiet-ms 60000))
                                    line :limit 200)))
      (is (< (length cut) 220))
      (is (search "more lines" cut)))))

(deftest channel-board-threads-answers-the-board-running ()
  ;; /threads typed outside every room says each running board under its
  ;; platform; a board stops answering only for the thunk that is its own.
  (with-saved-globals (nck::*boards*)
    (setf nck::*boards* (make-hash-table :test #'equal :synchronized t))
    (is (equal "No board is running." (nck:board-command "" nil)))
    (let ((thunk (nck:register-board "telegram" (lambda () "No thread is open."))))
      (is (search "No thread is open." (nck:board-command "" nil)))
      (is (search "**Telegram**" (nck:board-command "" nil)))
      (nck:unregister-board "telegram" (lambda () "a stale board"))
      (is (search "No thread is open." (nck:board-command "" nil)) "a stale stop leaves the board")
      (nck:unregister-board "telegram" thunk)
      (is (equal "No board is running." (nck:board-command "" nil))))))
