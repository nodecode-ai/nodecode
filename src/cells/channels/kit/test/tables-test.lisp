;;;; tables-test.lisp --- an answer's tables ride it as pictures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure reading and the delivery's own move; the one test that draws for
;;;; real runs the shipped renderer (tools/table-png.py), every other test
;;;; scripts *TABLE-IMAGE-RENDERER* so behavior is what is under test.

(in-package #:nodecode.test)

(deftest channel-tables-read-cells-trimmed ()
  (is (equal '("piece" "where") (nck::table-cells "| piece | where |")))
  (is (equal '("piece" "where") (nck::table-cells "piece | where")))
  (is (equal '("" "2") (nck::table-cells "| | 2 |")) "an empty cell stays a cell"))

(deftest channel-tables-read-rules-not-rows ()
  (is (nck::table-rule-line-p "|---|---|"))
  (is (nck::table-rule-line-p "|---|:--:|") "alignment colons belong to the rule")
  (is (not (nck::table-rule-line-p "| a | b |")))
  (is (not (nck::table-rule-line-p "| a | - |")) "a cell with anything but dashes is a data row, not a rule"))

(deftest channel-tables-read-blocks-outside-fences ()
  ;; A table is header, rule, rows; the same rows inside a fence are code a
  ;; turn wrote on purpose and are never drawn over.
  (let ((text (format nil "intro~%| a | b |~%|---|---|~%| 1 | 2 |~%outro"))
        (fenced (format nil "```~%| a | b |~%|---|---|~%| 1 | 2 |~%```")))
    (let ((blocks (nck::answer-table-blocks text)))
      (is (= 1 (length blocks)))
      (destructuring-bind (start end rows) (first blocks)
        (declare (ignore end))
        (is (= 1 start))
        (is (equal (list '("a" "b") '("1" "2")) rows))))
    (is (null (nck::answer-table-blocks fenced)))))

(deftest channel-tables-become-pictures-and-leave-one-blank ()
  (let* ((nck::*table-image-renderer*
           (lambda (rows)
             (declare (ignore rows))
             #p"/tmp/pea.png"))
         (text (format nil "before~%~%| a | b |~%|---|---|~%| 1 | 2 |~%~%after")))
    (multiple-value-bind (stripped images) (nck::answer-tables-as-images text)
      (is (equal (list #p"/tmp/pea.png") images))
      (is (not (find #\| stripped)) "the pipes are gone: the picture carries the table")
      (is (search "before" stripped))
      (is (search "after" stripped))
      (is (search (format nil "before~%~%after") stripped)))))

(deftest channel-tables-fall-back-to-a-fence ()
  ;; No picture (no renderer, no python3, too wide): the rows post in a
  ;; fenced block, and raw pipe rows still never post on their own.
  (let* ((nck::*table-image-renderer* (lambda (rows) (declare (ignore rows)) nil))
         (text (format nil "| a | b |~%|---|---|~%| 1 | 2 |")))
    (multiple-value-bind (stripped images) (nck::answer-tables-as-images text)
      (is (null images))
      (is (search "```" stripped))
      (is (search "| a | b |" stripped) "the rows survive inside the fence"))))

(deftest channel-tables-outside-the-caps-are-not-drawn ()
  ;; A dataset is not a card: the renderer is never asked for a table the
  ;; picture would be unreadable at.
  (let* ((asked nil)
         (nck::*table-image-renderer*
           (lambda (rows)
             (declare (ignore rows))
             (setf asked t)
             #p"/tmp/pea.png"))
         (long (cons '("a" "b") (loop repeat 61 collect '("1" "2"))))
         (wide (list (loop repeat 15 collect "h") (loop repeat 15 collect "1"))))
    (is (null (nck::table-picture long)) "61 rows are a dataset, not a card")
    (is (null (nck::table-picture wide)) "15 columns do not fit a card")
    (is (null asked) "neither was offered to the renderer")))

(deftest channel-tables-ride-the-answer-that-carries-them ()
  ;; The delivery's own move: the answer's text loses the pipes and the
  ;; picture joins the files the answer's message carries — the same ride
  ;; ANSWER-FILE's files take.
  (with-file-host (host "table-ride" :responses (replies "a1"))
    (let ((nck::*table-image-renderer*
            (lambda (rows)
              (declare (ignore rows))
              #p"/tmp/pea.png"))
          (digest (nck:make-turn-digest
                   :turn-id "t9" :phase :completed
                   :answer (format nil "spend:~%| window | spend |~%|---|---|~%| 28h | ¥102 |"))))
      (setf (nck:lane-active-turn-id lane) "t9")
      (nck::answer-tables-into-files lane digest)
      (let ((text (nck:digest-final-text digest)))
        (is (search "spend:" text))
        (is (not (find #\| text)) "the text that posts carries no pipe rows"))
      (nck::deliver-answer host lane digest)
      (let ((plan (first (nck:recording-executor-plans executor))))
        (is (search "spend:" (plan-content plan)))
        (is (equalp #("pea.png") (plan-field plan "files")))))))

(deftest channel-tables-the-room-keeps-the-table-as-written ()
  ;; The picture is for the people reading; the room's record keeps the
  ;; table as the model wrote it. It used to keep the text that posted, and
  ;; a later turn read its own answer back with the list missing.
  (with-file-host (host "table-record" :responses (replies "a1"))
    (let ((nck::*table-image-renderer* (lambda (rows) (declare (ignore rows)) #p"/tmp/pea.png"))
          (answer (format nil "spend:~%| window | spend |~%|---|---|~%| 28h | ¥102 |"))
          (recorded '()))
      (setf (nck:lane-active-turn-id lane) "t9")
      (with-stubbed-fdefinition (nck::write-back-rooms (host lane text typed-room) (push text recorded))
        (nck::finish-turn host lane (nck:make-turn-digest :turn-id "t9" :phase :completed :answer answer)))
      (is (equal (list answer) recorded))
      (is (not (find #\| (plan-content (first (nck:recording-executor-plans executor)))))))))

(deftest channel-tables-a-platform-that-draws-them-keeps-them ()
  ;; Slack's markdown block draws a table as its own table block: there the
  ;; answer posts its tables as the model wrote them, and no picture is
  ;; drawn for one.
  (with-file-host (host "table-native" :responses (replies "a1"))
    (let ((asked nil)
          (answer (format nil "spend:~%| window | spend |~%|---|---|~%| 28h | ¥102 |")))
      (setf (nck:platform-tables (nck::host-platform host)) '(:rows 100 :columns 20 :characters 10000))
      (setf (nck:lane-active-turn-id lane) "t9")
      (let ((nck::*table-image-renderer* (lambda (rows)
                                           (declare (ignore rows))
                                           (setf asked t)
                                           #p"/tmp/pea.png")))
        (with-stubbed-fdefinition (nck::write-back-rooms (host lane text typed-room) nil)
          (nck::finish-turn host lane (nck:make-turn-digest :turn-id "t9" :phase :completed
                                                            :answer answer))))
      (let ((plan (first (nck:recording-executor-plans executor))))
        (is (search "| 28h | ¥102 |" (plan-content plan)) "the table posts as written")
        (is (null (plan-field plan "files")) "no picture rides the answer")
        (is (null asked) "no picture was drawn")))))

(deftest channel-tables-past-what-the-platform-draws-are-drawn-here ()
  ;; Past its rows or columns Slack drops the rest unsaid, and past the cell
  ;; characters a message's tables may total it refuses the message whole: a
  ;; table past the platform's limits, or past what the tables before it left
  ;; of the characters, is drawn here as before.
  (let ((nck::*table-image-renderer* (lambda (rows) (declare (ignore rows)) #p"/tmp/pea.png"))
        (drawn '(:rows 3 :columns 2 :characters 12))
        (small (format nil "| a | b |~%|---|---|~%| 1 | 2 |"))
        (wide (format nil "| a | b | c |~%|---|---|---|~%| 1 | 2 | 3 |"))
        (long (format nil "| a | b |~%|---|---|~%| 1 | 2 |~%| 3 | 4 |~%| 5 | 6 |"))
        (heavy (format nil "| aaaa | bbbb |~%|---|---|~%| 1 | 2 |")))
    (flet ((drawn-here (text) (nth-value 1 (nck::answer-tables-as-images text drawn))))
      (is (equal small (nck::answer-tables-as-images small drawn)) "inside the limits it stays as written")
      (is (null (drawn-here small)))
      (is (= 1 (length (drawn-here wide))) "a column past them is drawn")
      (is (= 1 (length (drawn-here long))) "a row past them, the header counted, is drawn")
      (is (null (drawn-here heavy)) "ten cell characters fit twelve")
      (multiple-value-bind (text files)
          (nck::answer-tables-as-images (format nil "~a~%~%~a" small heavy) drawn)
        (is (search "| a | b |" text) "the first stays as written")
        (is (not (search "aaaa" text)) "the second needs ten of the eight the first left")
        (is (= 1 (length files)))))))

(deftest channel-tables-draw-a-real-picture (let ((png (nck::render-table-image
                                                         (list '("piece" "where")
                                                               '("detect" "kit/tables.lisp")
                                                               '("draw" "tools/table-png.py"))))))
  ;; The shipped seam, end to end: rows in, a PNG card out.
  (is (typep png 'pathname) "the renderer answers a file")
  (when (typep png 'pathname)
    (let ((magic (subseq (alexandria:read-file-into-byte-vector png) 0 8)))
      (is (equalp #(137 80 78 71 13 10 26 10) magic) "the file is a PNG"))
    (ignore-errors (delete-file png))))
