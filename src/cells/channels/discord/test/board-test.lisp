;;;; board-test.lisp --- the thread board's pure halves.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The message the board says its threads in, held inside Discord's caps on
;;;; one message; what decides a thread's section and when it closes is the
;;;; kit's (its board-test.lisp).

(in-package #:nodecode.test)

(defun board-test-parts (components)
  "Every component inside COMPONENTS, however nested, in order."
  (loop for part across components
        collect part
        append (let ((inner (nlk:json-value part :array "components")))
                 (and inner (board-test-parts inner)))
        append (let ((accessory (nlk:json-value part :object "accessory")))
                 (and accessory (list accessory)))))

(deftest channel-discord-board-message-holds-inside-discords-caps ()
  ;; Sections in the board's order, each its accent, its newest five threads
  ;; and, holding more, a Show all of its own; Needs you's threads carry their
  ;; question; Show all and, while any is done, Close all done; and the whole
  ;; inside Discord's caps of 40 components and 4000 characters.
  (let* ((rows (append (loop for i below 9
                             collect (list :id (format nil "10~d" i) :state :needs :quiet-ms (* (1+ i) 3600000)
                                           :question "Should I push it?"))
                       (list (list :id "200" :state :running :quiet-ms 60000)
                             (list :id "201" :state :queued :quiet-ms 1000)
                             (list :id "300" :state :done :quiet-ms 7200000))))
         (components (ncd:board-components rows :closing 3))
         (parts (board-test-parts components))
         (containers (remove-if-not (lambda (part) (eql 17 (nlk:json-value part :integer "type"))) parts))
         (texts (loop for part in parts
                      when (eql 10 (nlk:json-value part :integer "type"))
                        collect (nlk:json-value part :string "content")))
         (buttons (remove-if-not (lambda (part) (eql 2 (nlk:json-value part :integer "type"))) parts)))
    (is (equal '(#xFA8B3E #x23A55A #x80848E)
               (mapcar (lambda (part) (nlk:json-value part :integer "accent_color")) containers)) "Needs you, Running, Done: a section with no thread is left out")
    (let ((needs (find "**Needs you**" texts :test (lambda (needle text) (search needle text)))))
      (is (search "**Needs you** · 9" needs))
      (is (= 5 (count-matches "<#10" needs)) "the newest five")
      (is (search "<#100> · 1h" needs) "newest first, with its age")
      (is (search "-# Should I push it?" needs) "each with the question it ends on")
      (is (not (search "more" needs)) "what it leaves out is a button, not words"))
    ;; Needs you, holding nine, shows all of them from inside its own section.
    (is (equal '("ncd:board:all:needs")
               (loop for part in (board-test-parts (nlk:json-value (first containers) :array "components"))
                     when (eql 2 (nlk:json-value part :integer "type"))
                       collect (nlk:json-value part :string "custom_id"))))
    (is (find-if (lambda (text) (search "<#201> · waiting" text)) texts) "a waiting turn sits under Running")
    (is (find-if (lambda (text) (search "12 open · 3 more closing" text)) texts))
    ;; Running and Done, holding no more than they show, have no Show all of their own.
    (is (equal '("Show all 9" "Show all" "Close all done (1)")
               (mapcar (lambda (part) (nlk:json-value part :string "label")) buttons)))
    (is (<= (length parts) 40) "inside Discord's 40 components")
    (is (<= (reduce #'+ texts :key #'length) 4000) "and its 4000 characters")
    (is (equal '("Show all 9" "Show all")
               (mapcar (lambda (part) (nlk:json-value part :string "label"))
                       (remove-if-not (lambda (part) (eql 2 (nlk:json-value part :integer "type")))
                                      (board-test-parts (ncd:board-components (butlast rows)))))) "no Close all done while nothing is done"))
  ;; Every section full: a Show all in each, and still inside both caps, with
  ;; every custom_id once, as Discord requires of one message.
  (let* ((rows (loop for state in '(:needs :noans :running :done)
                     append (loop for i below 40
                                  collect (list :id (format nil "15620~2,'0d~13,'0d" (position state '(:needs :noans :running :done)) i)
                                                :state state :quiet-ms (* (1+ i) 60000)
                                                :question (and (eq state :needs) (make-string 200 :initial-element #\q))))))
         (parts (board-test-parts (ncd:board-components rows :closing 99)))
         (ids (loop for part in parts
                    when (eql 2 (nlk:json-value part :integer "type"))
                      collect (nlk:json-value part :string "custom_id"))))
    (is (equal '("ncd:board:all:needs" "ncd:board:all:noans" "ncd:board:all:running" "ncd:board:all:done"
                 "ncd:board:all" "ncd:board:done")
               ids))
    (is (<= (length parts) 40))
    (is (<= (loop for part in parts
                  when (eql 10 (nlk:json-value part :integer "type"))
                    sum (length (nlk:json-value part :string "content")))
            4000))))

(deftest channel-discord-board-show-all-answers-every-thread ()
  ;; Show all lists every open thread, whatever it takes -- it was cut to one
  ;; message, so a board of 71 never reached Running or Done -- and a
  ;; section's own Show all lists that section's every thread and no other.
  (let* ((rows (append (loop for i below 34 collect (list :id (format nil "3~18,'0d" i) :state :noans :quiet-ms 60000))
                       (loop for i below 7 collect (list :id (format nil "7~18,'0d" i) :state :running :quiet-ms 1000))
                       (loop for i below 60 collect (list :id (format nil "9~18,'0d" i) :state :done :quiet-ms 7200000))))
         (all (ncd:board-show-text "ncd:board:all" rows))
         (running (ncd:board-show-text "ncd:board:all:running" rows)))
    (is (every (lambda (row) (search (format nil "<#~a>" (getf row :id)) all)) rows) "every thread, none cut")
    (is (not (search "more lines" all)))
    (is (search "**Running** · 7" running))
    (is (= 7 (count-matches "<#7" running)))
    (is (not (or (search "<#3" running) (search "<#9" running))) "that section's alone")
    (is (equal "No thread is under Needs you now." (ncd:board-show-text "ncd:board:all:needs" rows)))
    (is (null (ncd:board-show-text "ncd:board:all:gone" rows)) "a section this board does not draw is out of date")
    (is (null (ncd:board-show-text "ncd:board:done" rows)))
    ;; The whole list rides as many messages as it needs.
    (is (< 1 (length (ncd:interaction-ack-plans '(:id "i" :token "t" :application-id "a") :text all :whole t))))))

(deftest channel-discord-board-list-answers-every-thread ()
  ;; Show all and /threads list every open thread by section, cut to one
  ;; message with what was left out counted.
  (let ((rows (list (list :id "1" :state :needs :quiet-ms 3600000 :question "Push it?")
                    (list :id "2" :state :done :quiet-ms 60000))))
    (is (equal (format nil "**Needs you** · 1~%<#1> · 1h — Push it?~%**Done** · 1~%<#2> · 1m")
               (nck:board-list-text rows #'ncd:board-list-line)))
    (is (equal "No thread is open." (nck:board-list-text '() #'ncd:board-list-line)))
    (let ((cut (nck:board-list-text (loop for i below 200 collect (list :id (format nil "~d" (+ 1000 i)) :state :done :quiet-ms 60000))
                                     #'ncd:board-list-line)))
      (is (< (length cut) 2000))
      (is (search "more lines" cut)))))

(deftest channel-discord-thread-close-plan-archives ()
  (is-plan (ncd:thread-close-plan "42") :method "PATCH" :path "/channels/42" :label "close_thread"
           (:boolean "archived") t))

(defun count-matches (needle text &aux (count 0) (start 0))
  "How many times NEEDLE occurs in TEXT."
  (loop for at = (search needle text :start2 start)
        while at do (incf count) (setf start (1+ at)))
  count)
