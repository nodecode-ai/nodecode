;;;; made-cells-test.lisp --- a cell the organism makes when asked for one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: NEW-CELL writes a folder of kept code beside the
;;;; layer, commits and loads it, and makes it the session's current cell;
;;;; what that session keeps then lands in the cell and not the layer, the
;;;; eval's output naming the cell; LEAVE-CELL sends the next keep back to
;;;; the layer, while a name the cell holds stays in it from any session; a
;;;; name kept into a cell leaves the layer, so it loads from one folder;
;;;; forgetting takes a cell's file and its .asd entry out; ENTER-CELL takes a
;;;; made cell up again after a restart, and a memory is still knowledge; a
;;;; bad name and a taken folder are refused.

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

(deftest made-cell-takes-what-its-session-keeps (with-made-cell-fixture ())
  (is (search "[weather: made"
              (with-output-to-string (*standard-output*)
                (let ((nlk:*scribe-session-id* "s-cell"))
                  (nlk:new-cell "weather" :description "Weather where the operator is.")))))
  (let ((directory (nlk:cell-directory "weather")))
    (is (equal (merge-pathnames "tester-weather/" nlk::*cells-directory*) directory))
    (is (eq :code (nlk:kept-folder-kind directory)) "a folder of kept code")
    (is (search "Weather where the operator is."
                (nlk:read-text (merge-pathnames "tester-weather.asd" directory))))
    (is (search "new cell weather - session s-cell" (knowledge-git "tester-weather" "log" "--format=%s" "-1")))
    (is (nlk:find-cell "tester-weather") "loaded, on the report"))
  (is (equal "weather" (nlk:current-cell "s-cell")))
  (is (null (nlk:current-cell "s-other")) "another session's code stays the layer's")
  (is (search "[weather: kept mc-forecast]"
              (keep-definition-text "(defun mc-forecast () \"Tomorrow's.\" :sunny)" :session "s-cell")))
  (is (cell-file "weather" "mc-forecast"))
  (is (search "(:file \"mc-forecast\")"
              (nlk:read-text (merge-pathnames "tester-weather/tester-weather.asd" nlk::*cells-directory*))))
  (is (string= "" (knowledge-git "tester-weather" "status" "--porcelain")) "committed whole")
  (is (null (uiop:directory-exists-p (nlk:layer-directory))) "the layer gained nothing")
  (is (eq :sunny (funcall (find-symbol "MC-FORECAST" '#:nodecode.evolved))) "and it runs"))

(deftest made-cell-leave-sends-new-code-to-the-layer-and-held-names-stay (with-made-cell-fixture ())
  (make-test-cell "clock")
  (keep-definition-text "(defun mc-tick () \"One.\" 1)" :session "s-cell")
  (is (equal "clock" (nlk:leave-cell "s-cell")))
  (is (null (nlk:current-cell "s-cell")))
  (is (search "[layer: kept mc-other]" (keep-definition-text "(defun mc-other () \"Two.\" 2)" :session "s-cell")))
  (is (layer-file "mc-other"))
  (let ((said (keep-definition-text "(defun mc-tick () \"One, again.\" 11)" :session "s-later")))
    (is (search "[clock: kept mc-tick]" said) "a name the cell holds stays in it, from any session"))
  (is (null (layer-file "mc-tick"))))

(deftest made-cell-a-name-kept-into-a-cell-leaves-the-layer (with-made-cell-fixture ())
  (keep-definition-text "(defun mc-built () \"Built before the cell.\" :old)" :session "s-cell")
  (is (layer-file "mc-built"))
  (make-test-cell "built")
  (let ((said (keep-definition-text "(defun mc-built () \"Built, now the cell's.\" :new)" :session "s-cell")))
    (is (search "[built: kept mc-built]" said))
    (is (search "[layer: moved mc-built to built]" said)))
  (is (cell-file "built" "mc-built"))
  (is (null (layer-file "mc-built")) "one home: no second copy loads after it")
  (is (null (search "mc-built" (nlk:read-text (nlk::layer-system-file))))))

(deftest made-cell-forgetting-takes-the-file-and-its-entry-out (with-made-cell-fixture ())
  (make-test-cell "gone")
  (keep-definition-text "(defun mc-gone () \"Soon gone.\" 0)" :session "s-cell")
  (is (search "[gone: forgot mc-gone]" (keep-definition-text "(fmakunbound 'mc-gone)" :session "s-cell")))
  (is (null (cell-file "gone" "mc-gone")))
  (is (null (search "mc-gone" (nlk:read-text (merge-pathnames "tester-gone/tester-gone.asd"
                                                                nlk::*cells-directory*))))))

(deftest made-cell-enter-takes-it-up-again-and-a-memory-stays-knowledge (with-made-cell-fixture ())
  (make-test-cell "notes")
  (clrhash nlk::*session-cells*)          ; what a restart forgets
  (is (null (nlk:current-cell "s-new")))
  (is (equal (nlk:cell-directory "notes") (nlk:enter-cell "notes" "s-new")))
  (is (search "[notes: kept mc-note]" (keep-definition-text "(defun mc-note () \"A note.\" t)" :session "s-new")))
  (is (search "[memory: kept mc-fact]"
              (keep-definition-text "(define-memory mc-fact \"A fact, not code.\" :type :reference)" :session "s-new")))
  (is (kept-text "mc-fact") "knowledge is the knowledge cell's, in a cell or not"))

(deftest made-cell-declares-a-command-that-starts (with-made-cell-fixture ())
  ;; cell.lisp gives the cell its package; a DEFUN and a DEFINE-CELL of one
  ;; name keep two files; the start the DEFINE-CELL made after the load is
  ;; taken up, and it registers the command under the cell's folder name
  (make-test-cell "clock")
  (is (find-package "TESTER-CLOCK") "cell.lisp made the package")
  (is (search "(:file \"cell\")"
              (nlk:read-text (merge-pathnames "tester-clock/tester-clock.asd" nlk::*cells-directory*))))
  (is (null (nlk:note-cell-start (nlk:find-cell "tester-clock"))) "nothing to start yet")
  (keep-definition-text "(defun clock (args session-id) (declare (ignore args session-id)) \"noon\")"
                        :session "s-cell" :package "TESTER-CLOCK")
  (keep-definition-text "(define-cell clock (:section (\"tester-clock-section\")) (:command \"tester-clock\" 'clock :description \"The time.\"))"
                        :session "s-cell" :package "TESTER-CLOCK")
  (is (search "(defun clock" (nlk:read-text (cell-file "clock" "clock"))) "the defun keeps its file")
  (is (search "(define-cell clock" (nlk:read-text (cell-file "clock" "clock-define-cell"))))
  (is (search "(:file \"clock-define-cell\")"
              (nlk:read-text (merge-pathnames "tester-clock/tester-clock.asd" nlk::*cells-directory*))))
  (let ((start (nlk:note-cell-start (nlk:find-cell "tester-clock"))))
    (is (functionp start) "the start is taken up")
    (let ((stop (funcall start (make-hash-table :test 'equal))))
      (nlk:with-cleanup ((funcall stop))
        (let ((entry (find "tester-clock" nle::*registered-commands*
                           :key #'nle::slash-command-name :test #'string-equal)))
          (is entry "the command is registered")
          (is (equal "tester-clock" (nle::slash-command-owner entry)) "under the folder's name")
          (is (equal "noon" (funcall (nle::slash-command-headless entry) "" "s-cell"))))))))

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
  (keep-definition-text "(defun mc-parked () \"Kept.\" 1)" :session "s-cell")
  (let ((names (lambda (folders) (mapcar #'nlk:folder-name folders))))
    (is (nlk:cell-parkable-p "tester-weather"))
    (is (not (nlk:cell-parkable-p (nlk::layer-name))) "the layer the organism writes to never parks")
    (is (not (nlk:cell-parkable-p "tester-memory")) "nor a knowledge cell")
    (is (equal (nlk:cell-directory "weather") (nlk:park-cell "tester-weather")))
    (is (nlk:cell-parked-p "tester-weather"))
    (is (cell-file "weather" "mc-parked") "its work is all there")
    (is (null (nlk:current-cell "s-cell")) "its session keeps to the layer again")
    (is (string= "" (knowledge-git "tester-weather" "status" "--porcelain")) "the marker stays out of its history")
    (is (not (member "tester-weather" (funcall names (nlk::cell-folders)) :test #'string=)) "no boot loads it")
    (is (member "tester-weather" (funcall names (nlk:every-cell-folder)) :test #'string=) "it is still listed")
    (is (equal (nlk:cell-directory "weather") (nlk:unpark-cell "tester-weather")))
    (is (not (nlk:cell-parked-p "tester-weather")))
    (is (member "tester-weather" (funcall names (nlk::cell-folders)) :test #'string=) "the boot loads it again")
    (is (null (nlk:unpark-cell "tester-weather")) "on twice is nothing to do")))
