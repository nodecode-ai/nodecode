;;;; made-cells-test.lisp --- a cell the organism makes when asked for one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: NEW-CELL writes a folder of kept code beside the
;;;; layer -- cell.lisp, NAME.lisp, an .asd listing the two -- commits and
;;;; loads it, and makes it the session's current cell; the cell's code is
;;;; its files, so an eval in that session defines live and keeps nothing,
;;;; its output naming the file to write, and a write into the file makes it
;;;; live and commits it; a name a cell's file holds stays there from any
;;;; session; LEAVE-CELL sends the next keep back to the layer; ENTER-CELL
;;;; takes a made cell up again after a restart, and a memory is still
;;;; knowledge; with :SOURCE the folder lives elsewhere, linked in, and a
;;;; write through it goes live; a bad name and a taken folder are refused.

(in-package #:nodecode.test)

(defmacro with-made-cell-fixture (() &body body)
  "BODY over WITH-KNOWLEDGE-FIXTURE's folder, no session in a cell, and the
report of loaded cells put back after."
  `(with-knowledge-fixture ()
     (with-saved-globals ((nlk::*cells* nlk::*cells*))
       (clrhash nlk::*session-cells*)
       (nlk:with-cleanup ((clrhash nlk::*session-cells*))
         ,@body))))

(defun make-test-cell (name &optional (session "s-cell"))
  "NEW-CELL NAME for SESSION, what it prints dropped. => the folder."
  (let ((nlk:*scribe-session-id* session))
    (let ((*standard-output* (make-broadcast-stream)))
      (nlk:new-cell name))))

(defun cell-file (cell name)
  "NAME.lisp in the fixture's made cell CELL, when it is there."
  (probe-file (merge-pathnames (format nil "tester-~a/~a.lisp" cell name) nlk::*cells-directory*)))

(defun layer-file (name)
  "NAME.lisp in the fixture's layer, when it is there."
  (probe-file (merge-pathnames (format nil "~a.lisp" name) (nlk:layer-directory))))

(defun write-cell-file (cell name text)
  "Write NAME.lisp of the fixture's made cell CELL through the write verb the
model uses. => its receipt."
  (nle::write-file (merge-pathnames (format nil "tester-~a/~a.lisp" cell name) nlk::*cells-directory*) text))

(deftest made-cell-writes-two-files-and-keeps-no-eval (with-made-cell-fixture ())
  (is (search "[weather: made"
              (with-output-to-string (*standard-output*)
                (let ((nlk:*scribe-session-id* "s-cell"))
                  (nlk:new-cell "weather" :description "Weather where the operator is.")))))
  (let ((directory (nlk:cell-directory "weather")))
    (is (equal (merge-pathnames "tester-weather/" nlk::*cells-directory*) directory))
    (is (eq :code (nlk:kept-folder-kind directory)) "a folder of kept code")
    (is (equal '("cell" "weather")
               (sort (mapcar #'pathname-name (directory (merge-pathnames "*.lisp" directory))) #'string<)))
    (let ((asd (nlk:read-text (merge-pathnames "tester-weather.asd" directory))))
      (is (search "Weather where the operator is." asd))
      (is (search "(:file \"cell\") (:file \"weather\")" asd) "the .asd lists the two"))
    (is (search "(in-package #:tester-weather)" (nlk:read-text (cell-file "weather" "weather"))))
    (is (search "new cell weather - session s-cell" (knowledge-git "tester-weather" "log" "--format=%s" "-1")))
    (is (nlk:find-cell "tester-weather") "loaded, on the report"))
  (is (equal "weather" (nlk:current-cell "s-cell")))
  (is (null (nlk:current-cell "s-other")) "another session's code stays the layer's")
  (let ((said (keep-definition-text "(defun mc-forecast () \"Tomorrow's.\" :sunny)" :session "s-cell")))
    (is (search "[weather: mc-forecast is live in this image and not kept" said))
    (is (search "weather.lisp with write-file or edit" said) "it names the file to write"))
  (is (null (cell-file "weather" "mc-forecast")) "an eval leaves no file")
  (is (null (search "mc-forecast" (nlk:read-text (merge-pathnames "tester-weather/tester-weather.asd" nlk::*cells-directory*)))))
  (is (string= "" (knowledge-git "tester-weather" "status" "--porcelain")) "nothing changed in the folder")
  (is (null (uiop:directory-exists-p (nlk:layer-directory))) "the layer gained nothing")
  (is (eq :sunny (funcall (find-symbol "MC-FORECAST" '#:nodecode.evolved))) "and it runs"))

(deftest made-cell-a-write-is-live-and-a-commit (with-made-cell-fixture ())
  (make-test-cell "bell")
  (let ((receipt (let ((nlk:*scribe-session-id* "s-cell"))
                   (write-cell-file "bell" "bell" (format nil "(in-package #:tester-bell)~%~%(defun mc-ring () :ring)~%")))))
    (is (search "live here: mc-ring" receipt) "the write is the live change"))
  (is (eq :ring (funcall (find-symbol "MC-RING" "TESTER-BELL"))))
  (is (string= "" (knowledge-git "tester-bell" "status" "--porcelain")) "committed whole")
  (is (search "bell.lisp: mc-ring - session s-cell" (knowledge-git "tester-bell" "log" "--format=%s" "-1")))
  (let ((nlk:*scribe-session-id* "s-cell"))
    (nle::edit (cell-file "bell" "bell") ":ring)" ":rung)"))
  (is (eq :rung (funcall (find-symbol "MC-RING" "TESTER-BELL"))) "an edit is live too")
  ;; the new cell, the write, the edit: one commit each
  (is (= 3 (length (remove "" (uiop:split-string (knowledge-git "tester-bell" "log" "--format=%s")
                                                 :separator '(#\Newline))
                           :test #'string=)))))

(deftest made-cell-leave-sends-evals-to-the-layer-and-a-name-its-file-holds-stays-there (with-made-cell-fixture ())
  (make-test-cell "clock")
  (write-cell-file "clock" "clock" (format nil "(in-package #:tester-clock)~%~%(defun nle::mc-tick () \"One.\" 1)~%"))
  (is (equal "clock" (nlk:leave-cell "s-cell")))
  (is (null (nlk:current-cell "s-cell")))
  (is (search "[layer: kept mc-other]" (keep-definition-text "(defun mc-other () \"Two.\" 2)" :session "s-cell")))
  (is (layer-file "mc-other"))
  (let ((said (keep-definition-text "(defun mc-tick () \"One, again.\" 11)" :session "s-later")))
    (is (search "[clock: mc-tick is live in this image and not kept" said) "a name the cell's file holds stays there, from any session")
    (is (search "clock.lisp holds it, so edit it there" said)))
  (is (null (layer-file "mc-tick")) "never a second copy in the layer"))

(deftest made-cell-an-eval-in-it-leaves-the-layer-and-the-files-as-they-were (with-made-cell-fixture ())
  (keep-definition-text "(defun mc-built () \"Built before the cell.\" :old)" :session "s-cell")
  (is (layer-file "mc-built"))
  (make-test-cell "built")
  (is (search "not kept" (keep-definition-text "(defun mc-built () \"Tried in the cell.\" :new)" :session "s-cell")))
  (is (search ":old" (nlk:read-text (layer-file "mc-built"))) "the layer's text is as it was")
  (is (search "not kept" (keep-definition-text "(fmakunbound 'mc-built)" :session "s-cell")) "a forget is a try too")
  (is (layer-file "mc-built") "and takes no file"))

(deftest made-cell-enter-takes-it-up-again-and-a-memory-stays-knowledge (with-made-cell-fixture ())
  (make-test-cell "notes")
  (clrhash nlk::*session-cells*)          ; what a restart forgets
  (is (null (nlk:current-cell "s-new")))
  (is (equal (nlk:cell-directory "notes") (nlk:enter-cell "notes" "s-new")))
  (is (search "[notes: mc-note is live in this image and not kept"
              (keep-definition-text "(defun mc-note () \"A note.\" t)" :session "s-new")))
  (is (search "[memory: kept mc-fact]"
              (keep-definition-text "(define-memory mc-fact \"A fact, not code.\" :type :reference)" :session "s-new")))
  (is (kept-text "mc-fact") "knowledge is the knowledge cell's, in a cell or not"))

(deftest made-cell-lives-in-a-source-folder-linked-in (with-made-cell-fixture ())
  (let ((source (merge-pathnames "code/linked/" (uiop:pathname-parent-directory-pathname nlk::*cells-directory*))))
    (let ((nlk:*scribe-session-id* "s-cell") (*standard-output* (make-broadcast-stream)))
      (nlk:new-cell "linked" :source source))
    (let ((link (merge-pathnames "tester-linked" nlk::*cells-directory*)))
      (is (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat (namestring link)))) "cells/ holds a link")
      (is (equal (namestring (truename source))
                 (namestring (truename (uiop:ensure-directory-pathname link)))) "to the source folder"))
    (is (probe-file (merge-pathnames "linked.lisp" source)) "the files are the source folder's")
    (is (probe-file (merge-pathnames ".git/" source)) "a repository of its own there")
    (let ((receipt (nle::write-file (merge-pathnames "linked.lisp" source)
                                    (format nil "(in-package #:tester-linked)~%~%(defun mc-linked () :there)~%"))))
      (is (search "live here: mc-linked" receipt) "a write at the source goes live"))
    (is (eq :there (funcall (find-symbol "MC-LINKED" "TESTER-LINKED"))))
    (is (string= "" (knowledge-git "tester-linked" "status" "--porcelain")) "and is committed")
    (let ((nlk:*scribe-session-id* "s-cell"))
      ;; a source folder that holds files is refused
      (is (search "refused" (or (handler-case (progn (nlk:new-cell "again" :source source) nil)
                                  (error () "refused"))
                              ""))))))

(deftest made-cell-declares-a-command-that-starts (with-made-cell-fixture ())
  ;; cell.lisp gives the cell its package; the code file holds the defun and
  ;; the define-cell, written; the start the define-cell made is taken up,
  ;; and it registers the command under the cell's folder name
  (make-test-cell "clock")
  (is (find-package "TESTER-CLOCK") "cell.lisp made the package")
  (is (null (nlk:note-cell-start (nlk:find-cell "tester-clock"))) "nothing to start yet")
  (write-cell-file "clock" "clock"
                   (format nil "(in-package #:tester-clock)~%~%~
                                (defun clock (args session-id) (declare (ignore args session-id)) \"noon\")~%~%~
                                (define-cell clock (:section (\"tester-clock-section\")) (:command \"tester-clock\" 'clock :description \"The time.\"))~%"))
  (let ((code (nlk:read-text (cell-file "clock" "clock"))))
    (is (search "(defun clock" code))
    (is (search "(define-cell clock" code) "one file holds both"))
  (let ((start (nlk:note-cell-start (nlk:find-cell "tester-clock"))))
    (is (functionp start) "the start is taken up")
    (let ((stop (funcall start (make-hash-table :test 'equal))))
      (nlk:with-cleanup ((funcall stop))
        (let ((entry (find "tester-clock" nle::*registered-commands*
                           :key #'nle::slash-command-name :test #'string-equal)))
          (is entry "the command is registered")
          (is (equal "tester-clock" (nle::slash-command-owner entry)) "under the folder's name")
          (is (equal "noon" (funcall (nle::slash-command-headless entry) "" "s-cell"))))))))

(deftest made-cell-the-help-recipe-leaves-no-drift (with-made-cell-fixture ())
  ;; the recipe (help :cells) teaches -- new-cell, the code file written whole
  ;; with its defun and define-cell, restart-cells -- answers drift :clean t:
  ;; what new-cell's scaffold and define-cell expand into (the peripheral's
  ;; condition and its reader, *stop*, start-cell, stop-cell) is the file's
  ;; own, not drift; and so it stays when cell.lisp is written again through
  ;; the write verb, which makes it live from text with no pathname
  (make-test-cell "tokyo")
  (nlk:with-cleanup ((nle::stop-cells '("tester-tokyo")))
    (write-cell-file "tokyo" "tokyo"
                     (format nil "(in-package #:tester-tokyo)~%~%~
                                  (defun tokyo (args session-id) (declare (ignore args session-id)) \"noon in Tokyo\")~%~%~
                                  (define-cell tokyo (:section (\"tester-tokyo-section\")) (:command \"tester-tokyo\" 'tokyo :description \"The time.\"))~%"))
    (is (equal (nlk:cell-directory "tokyo") (nlk:cell-directory "tester-tokyo")) "the folder's whole name finds it too")
    (let ((report (nle:restart-cells "tester-tokyo")))
      (is (search "/tester-tokyo" report) (format nil "the restart names the command it registered: ~a" report)))
    (let ((drift (nlk:cell-drift "tokyo")))
      (is (null (remove "TESTER-TOKYO" (getf drift :live-only) :key #'first :test-not #'string=))
          (format nil "nothing of the cell's own package is drift: ~s" (getf drift :live-only)))
      (is (null (getf drift :not-live)) "and the files are live"))
    (let ((cell (merge-pathnames "tester-tokyo/cell.lisp" nlk::*cells-directory*)))
      (nle::write-file cell (nlk:read-text cell)))
    (nle:restart-cells "tester-tokyo")
    (let ((drift (nlk:cell-drift "tokyo")))
      (is (null (remove "TESTER-TOKYO" (getf drift :live-only) :key #'first :test-not #'string=))
          (format nil "cell.lisp written again leaves no drift either: ~s" (getf drift :live-only))))))

(defun chime-code (&rest commands)
  "The made cell chime's code file: one DEFINE-CELL declaring COMMANDS."
  (format nil "(in-package #:tester-chime)~%~%(defun chime (args session-id) (declare (ignore args session-id)) \"ding\")~%~%~
               (define-cell chime (:section (\"tester-chime-section\"))~{ (:command ~s 'chime)~})~%"
          commands))

(deftest made-cell-a-changed-define-cell-starts-at-restart (with-made-cell-fixture ())
  ;; the start a restart runs is the DEFINE-CELL as it stands now, never the
  ;; one the record took up first: a clause added later is installed at the
  ;; folder's restart, written through the write verb or edited outside and
  ;; reloaded
  (make-test-cell "chime")
  (flet ((registered (name)
           (find name nle::*registered-commands* :key #'nle::slash-command-name :test #'string-equal)))
    (nlk:with-cleanup ((nle::stop-cells '("tester-chime")))
      (write-cell-file "chime" "chime" (chime-code "tester-chime-a"))
      (nle:restart-cells "tester-chime")
      (is (registered "tester-chime-a") "the first declaration starts")
      (write-cell-file "chime" "chime" (chime-code "tester-chime-a" "tester-chime-b"))
      (nle:restart-cells "tester-chime")
      (is (registered "tester-chime-b") "a clause added since is installed at restart-cells")
      (is (registered "tester-chime-a") "beside the first")
      (nlk:write-file-atomically (cell-file "chime" "chime") (chime-code "tester-chime-c"))
      (nlk:reload-cell "tester-chime")
      (is (registered "tester-chime-c") "a define-cell edited outside starts at reload-cell")
      (is (null (registered "tester-chime-a")) "and what it no longer declares is off"))))

(deftest layer-two-kinds-of-one-name-keep-two-files (with-made-cell-fixture ())
  ;; a DEFCLASS beside a DEFUN of its name keeps the DEFUN's file and takes
  ;; its own; a DEFMACRO of the DEFUN's name is the same function and takes
  ;; its file, as a second DEFUN would
  (keep-definition-text "(defun mc-point () :fn)" :session "s-layer")
  (keep-definition-text "(defclass mc-point () ())" :session "s-layer")
  (is (search "(defun mc-point" (nlk:read-text (layer-file "mc-point"))))
  (is (search "(defclass mc-point" (nlk:read-text (layer-file "mc-point-defclass"))))
  (keep-definition-text "(defmacro mc-point () :macro)" :session "s-layer")
  (is (search "(defmacro mc-point" (nlk:read-text (layer-file "mc-point"))) "the macro took the defun's file")
  (is (search "(defclass mc-point" (nlk:read-text (layer-file "mc-point-defclass"))) "the class kept its own"))

(deftest made-cell-a-file-written-into-it-joins-its-load-order (with-made-cell-fixture ())
  ;; a write loads a file live; the cell's .asd must list it too, or the
  ;; next start never reads it
  (make-test-cell "bell")
  (let* ((directory (nlk:cell-directory "bell"))
         (path (merge-pathnames "ring.lisp" directory))
         (asd (merge-pathnames "tester-bell.asd" directory)))
    (nlk:write-file-atomically path (format nil "(in-package #:tester-bell)~%(defun mc-ring () :ring)~%"))
    (is (search "listed in tester-bell.asd" (or (nlk::load-source path) "")))
    (is (search "(:file \"ring\")" (nlk:read-text asd)))
    (is (not (search "listed in" (or (nlk::load-source path) ""))) "listed once")
    (is (= 1 (/ (length (ppcre:all-matches "\\(:file \"ring\"\\)" (nlk:read-text asd))) 2)))))

(deftest made-cell-refuses-a-bad-name-and-a-taken-folder (with-made-cell-fixture ())
  (flet ((refused (thunk)
           (handler-case (let ((*standard-output* (make-broadcast-stream))) (funcall thunk) nil)
             (error (condition) (princ-to-string condition)))))
    (is (search "lowercase letters" (refused (lambda () (nlk:new-cell "Tokyo Time")))))
    (is (search "memory names" (refused (lambda () (nlk:new-cell "memory")))))
    (ensure-directories-exist (merge-pathnames "nodecode-hand/" nlk::*cells-directory*))
    (let ((said (refused (lambda () (nlk:new-cell "hand")))))
      (is (search "pick another name" said) "a shipped or hub cell's folder answers to the name"))
    (make-test-cell "twice")
    (is (search "(enter-cell \"twice\")" (refused (lambda () (nlk:new-cell "twice")))))
    (is (search "no cell absent was made here" (refused (lambda () (nlk:enter-cell "absent")))))))

(deftest made-cell-off-parks-it-whole-and-on-takes-it-back (with-made-cell-fixture ())
  ;; Off on a made cell keeps every file: the marker goes in, out of its
  ;; history, the boot passes over it, and its session keeps to the layer.
  (make-test-cell "weather")
  (write-cell-file "weather" "weather" (format nil "(in-package #:tester-weather)~%~%(defun mc-parked () \"Kept.\" 1)~%"))
  (let ((names (lambda (folders) (mapcar #'nlk:folder-name folders))))
    (is (nlk:cell-parkable-p "tester-weather"))
    (is (not (nlk:cell-parkable-p (nlk::layer-name))) "the layer the organism writes to never parks")
    (is (not (nlk:cell-parkable-p "tester-memory")) "nor a knowledge cell")
    (is (equal (nlk:cell-directory "weather") (nlk:park-cell "tester-weather")))
    (is (nlk:cell-parked-p "tester-weather"))
    (is (search "mc-parked" (nlk:read-text (cell-file "weather" "weather"))) "its work is all there")
    (is (null (nlk:current-cell "s-cell")) "its session keeps to the layer again")
    (is (string= "" (knowledge-git "tester-weather" "status" "--porcelain")) "the marker stays out of its history")
    (is (not (member "tester-weather" (funcall names (nlk::cell-folders)) :test #'string=)) "no boot loads it")
    (is (member "tester-weather" (funcall names (nlk:every-cell-folder)) :test #'string=) "it is still listed")
    (is (equal (nlk:cell-directory "weather") (nlk:unpark-cell "tester-weather")))
    (is (not (nlk:cell-parked-p "tester-weather")))
    (is (member "tester-weather" (funcall names (nlk::cell-folders)) :test #'string=) "the boot loads it again")
    (is (null (nlk:unpark-cell "tester-weather")) "on twice is nothing to do")))
