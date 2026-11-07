;;;; tables.lisp --- an answer's tables ride it as pictures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Discord renders no tables: raw pipe rows arrive as literal text. When
;;;; the answer a turn is about to deliver carries a GitHub-style table,
;;;; the delivery replaces it with a PNG drawn locally (tools/table-png.py
;;;; through Pillow) and hands the picture to the answer's own message the
;;;; way ANSWER-FILE does. No renderer, no python3, no font, or a table
;;;; too wide to draw falls back to a fenced code block, the house rule
;;;; before the picture existed: raw pipes never post.
;;;;
;;;; A platform whose messages draw a table themselves -- Slack's markdown
;;;; block turns one into its own table block -- says how big a table it
;;;; draws (PLATFORM-TABLES); there a table inside that posts as the model
;;;; wrote it, and one past it is drawn here as before.

(in-package #:nodecode-channel-kit)

(defparameter +table-row-cap+ 60
  "Rows one table picture draws. Longer is a dataset, not a card.")

(defparameter +table-column-cap+ 14
  "Columns one table picture draws.")

(defparameter +table-cell-cap+ 200
  "Characters one cell may carry before the table falls back to a fence.")

;;; ROWS is the header row first, then the body rows, cells trimmed. Tests
;;; rebind this; production uses RENDER-TABLE-IMAGE.
(defvar *table-image-renderer* (lambda (rows) (render-table-image rows))
  "ROWS -> a PNG pathname, or NIL when no picture can be drawn here.")

(nlk:define-side-table platform-tables (:test #'eq :weakness :key :synchronized t)
  "What a PLATFORM's messages draw of a GitHub-style table themselves, as
(:ROWS R :COLUMNS C :CHARACTERS N): the most rows -- the header among them --
and columns one table may hold, and the cell characters an answer's tables
may total. NIL when they draw none. A table inside them posts as the model
wrote it; any other is drawn here (ANSWER-TABLES-AS-IMAGES).")
;; A table hung beside the platform rather than a slot of it, as
;; PLATFORM-PLAN-CLOSE-THREAD is: a running image holds its platforms.

(defun table-cell-characters (rows)
  "The characters ROWS' cells hold, the header's among them."
  (loop for row in rows sum (loop for cell in row sum (length cell))))

(defun table-drawn-there-p (rows drawn left)
  "Whether a platform that draws tables up to DRAWN (PLATFORM-TABLES) draws
one of ROWS itself, with LEFT of its cell characters still free."
  (and drawn
       (<= (length rows) (getf drawn :rows 0))
       (<= (reduce #'max rows :key #'length :initial-value 0) (getf drawn :columns 0))
       (<= (table-cell-characters rows) left)))

;;; --- drawing ----------------------------------------------------------------

(defun table-temp-path (type)
  "A fresh path in the host's temp directory for one render artifact."
  (merge-pathnames
   (format nil "nck-table-~d-~a.~a" (get-universal-time)
           (symbol-name (gensym "T")) type)
   (uiop:temporary-directory)))

(defun render-table-image (rows)
  "ROWS -> a PNG pathname drawn through tools/table-png.py, or NIL when
the picture cannot be drawn on this host: no script, no python3, no
monospace font, a table too wide."
  ;; Never signals -- the delivery this runs under must not fail for a
  ;; picture's sake.
  (nlk:with-handlers ((error () nil))
    (let ((script (ignore-errors
                    (asdf:system-relative-pathname "nodecode-channel-kit"
                                                   "tools/table-png.py"))))
      (when (and script (probe-file script))
        (let ((tsv (table-temp-path "tsv"))
              (png (table-temp-path "png"))
              (result nil))
          (nlk:with-cleanup ((ignore-errors (delete-file tsv))
                             (unless result (ignore-errors (delete-file png))))
            (with-open-file (stream tsv :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
              (dolist (row rows)
                (format stream "~{~a~^~c~}~%"
                        (loop for (cell . more) on row
                              collect (substitute #\Space #\Tab cell)
                              when more collect #\Tab))))
            (when (and (eql 0 (nth-value 2 (nlk:run-bounded
                                             (list "python3" (uiop:native-namestring script)
                                                   (uiop:native-namestring tsv)
                                                   (uiop:native-namestring png))
                                             :seconds 30 :output nil :error-output nil)))
                       (attachable-file-size png))
              (setf result png))
            result))))))

;;; --- reading tables out of an answer ------------------------------------------

(defun table-split-lines (text)
  "TEXT's lines; a trailing carriage return on a line is trimmed."
  (mapcar (lambda (line) (string-right-trim '(#\Return) line))
          (uiop:split-string text :separator '(#\Newline))))

(defun table-cells (line)
  "LINE's cells: split on the pipes, drop the empty ends a leading or
trailing pipe makes, trim every cell."
  (mapcar (lambda (cell) (string-trim '(#\Space #\Tab) cell))
          (uiop:split-string
           (ppcre:regex-replace "\\A\\|?(.*?)\\|?\\z" (string-trim " " line) "\\1")
           :separator "|")))

(defun table-carries-a-pipe-p (line)
  "A line a table row could be: not blank, and carrying a pipe."
  (and (plusp (length (string-trim " " line)))
       (find #\| line)))

(defun table-fence-line-p (line &aux (trimmed (string-trim " " line)))
  "LINE opens or closes a ``` fence."
  (and (>= (length trimmed) 3)
       (string= "```" trimmed :end2 3)))

(defun table-rule-cell-p (cell &aux (trimmed (string-trim '(#\Space #\Tab) cell))
                                    (core (string-trim ":" trimmed)))
  "CELL is one cell of a rule line: dashes, at most one colon on each end."
  (and (plusp (length core))
       (every (lambda (c) (char= c #\-)) core)
       (<= (- (length trimmed) (length core)) 2)))

(defun table-rule-line-p (line &aux (cells (table-cells line)))
  "LINE is a table's rule line: pipe-separated cells of dashes only."
  (and (table-carries-a-pipe-p line)
       (plusp (length cells))
       (every #'table-rule-cell-p cells)))

(defun answer-table-blocks (text &aux (lines (table-split-lines text))
                                      (blocks '())
                                      (fence nil)
                                      (count (length lines)))
  "Every GitHub-style table TEXT carries: a list of (START END ROWS) --
line indices, END inclusive; ROWS is the header first, the rule line left
out. Rows inside a ``` fence are code and never read."
  (loop for index = 0 then (1+ index)
        for line = (nth index lines)
        while (< index count)
        do (cond
                 ((table-fence-line-p line)
                  (setf fence (not fence)))
                 ((and (not fence)
                       (table-carries-a-pipe-p line)
                       (< (1+ index) count)
                       (table-rule-line-p (nth (1+ index) lines)))
                  ;; The table runs on while the lines past its rule carry a pipe.
                  (let ((end (1+ index)))
                    (loop while (and (< (1+ end) count)
                                     (table-carries-a-pipe-p (nth (1+ end) lines)))
                          do (incf end))
                    (push (list index end (mapcar #'table-cells
                                                  (cons line (subseq lines (+ index 2) (1+ end)))))
                          blocks)
                    (setf index end)))))
  (nreverse blocks))

(defun table-span-at (text cut &aux (starts (coerce (loop for at = 0 then (1+ newline)
                                                          for newline = (position #\Newline text :start at)
                                                          collect at
                                                          while newline)
                                                    'vector)))
  "(values START END) of the table in TEXT a cut at CUT parts -- where its
header line starts and its last line ends -- or NIL when CUT parts no
table's rows."
  (loop for (first last) in (answer-table-blocks text)
        for start = (aref starts first)
        for end = (or (position #\Newline text :start (aref starts last)) (length text))
        when (< start cut end)
          return (values start end)))

(defun table-picture (rows)
  "ROWS -> a PNG pathname, or NIL when the table is outside what a card
draws or the renderer refuses."
  (when (and (<= (length rows) +table-row-cap+)
             (<= (reduce #'max rows :key #'length) +table-column-cap+)
             (loop for row in rows
                   never (loop for cell in row
                               thereis (> (length cell) +table-cell-cap+))))
    (funcall *table-image-renderer* rows)))

;;; --- the delivery's move ------------------------------------------------------

(defun answer-tables-as-images (text &optional drawn)
  "TEXT with every table it carries replaced: a picture that will ride the
answer's message (the second value, in the order the tables appeared), or
-- when no picture can be drawn -- the same rows in a fenced block. A table
the platform draws itself, inside DRAWN (PLATFORM-TABLES), stays as written."
  ;; Never signals. => (values TEXT FILES); (values NIL NIL) for a non-string.
  (when (stringp text)
    (handler-case
        (let ((blocks (answer-table-blocks text)))
          (if (null blocks)
              (values text nil)
              (let ((lines (table-split-lines text))
                    (out '())
                    (images '())
                    ;; The blank line a picture's removal left doubled.
                    (skip nil)
                    (cursor 0)
                    ;; The cell characters the platform's tables have left.
                    (left (getf drawn :characters 0))
                    (replaced nil))
                (flet ((copy (from below)
                         (loop for i from from below below
                               unless (eql i skip)
                                 do (push (nth i lines) out))))
                  (loop for (start end rows) in blocks
                        for kept = (table-drawn-there-p rows drawn left)
                        for path = (and (not kept) (table-picture rows))
                        for before = (and (plusp start)
                                          (nth (1- start) lines))
                        for after = (and (< (1+ end) (length lines))
                                         (nth (1+ end) lines))
                        do (copy cursor start)
                           (cond (kept
                                  (decf left (table-cell-characters rows))
                                  (copy start (1+ end)))
                                 ((null path)
                                  (push "```" out)
                                  (copy start (1+ end))
                                  (push "```" out))
                                 ((not (and before after)))
                                 ;; One blank where the table stood, not two.
                                 ((and (string= "" before)
                                       (string= "" after))
                                  (setf skip (1+ end)))
                                 ((and
                                   (plusp (length (string-trim " " before)))
                                   (plusp (length (string-trim " " after))))
                                  (push "" out)))
                           (unless kept (setf replaced t))
                           (when path (push path images))
                           (setf cursor (1+ end)))
                  (copy cursor (length lines)))
                (if (not replaced)
                    (values text nil)
                    (let ((joined (format nil "~{~a~^~%~}" (nreverse out))))
                      (values (if (plusp (length (nlk:trimmed joined)))
                                  joined
                                  "*(table attached)*")
                              (nreverse images)))))))
      (error (condition)
        (warn "channel: reading answer tables failed: ~a" condition)
        (values text nil)))))

(defun answer-tables-into-files (lane digest &optional platform)
  "The delivery's own move before DELIVER-ANSWER: a completed answer that
carries tables gets them drawn -- the text that will post loses the raw
pipe rows, and the pictures join the answer's own files, riding its
message the way ANSWER-FILE's do. A table PLATFORM draws itself
(PLATFORM-TABLES) stays as written."
  ;; An answer with no tables is untouched; a refusal is already a fenced
  ;; block (the transform's own fallback). The answer must survive this: a
  ;; picture is never worth a failed delivery.
  (handler-case
      (let ((text (and (eq digest.phase :completed)
                       digest.answer)))
        (when (stringp text)
          (multiple-value-bind (stripped images)
              (answer-tables-as-images text (and platform (platform-tables platform)))
            (unless (equal stripped text)
              (setf digest.answer stripped)
              (when (and images digest.turn-id)
                (add-answer-files lane digest.turn-id images))))))
    (error (condition)
      (warn "channel: answer tables for ~a failed: ~a"
            lane.session-id condition)
      nil)))
