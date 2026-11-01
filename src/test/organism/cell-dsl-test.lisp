;;;; cell-dsl-test.lisp --- the cell composition DSL and the scope under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two seams, proved apart. STARTING/ON-STOP is the generic registration
;;;; scope: what it collects, the order it runs it in, that it runs once, and
;;;; that a body leaving non-locally takes its own prefix with it. DEFINE-CELL
;;;; is the declaration over it: one demo cell declared at the foot of this
;;;; header contributes a tool, a help topic, a point hook, a slash command and a
;;;; start of its own, so what is asserted below is the composition and not
;;;; one shipped folder's shape. The last test takes the same declaration
;;;; through the real folder loader and the gateway's lifecycle, because the
;;;; cell report's REFUSED state and RESTART-CELLS are the contract the
;;;; declaration must not have moved.
;;;;
;;;; The demo's package is a throwaway; its section, "nl-demo", declares no
;;;; required member, so it never opens a setup panel and the suite's other
;;;; readers of NLK:*SECTIONS* are untouched by it.

(in-package #:nodecode.test)

(defpackage #:nodecode-cell-demo
  (:documentation "A throwaway cell: the composition DSL's own consumer.")
  (:use #:cl)
  (:export #:start-cell))

(nlk:define-peripheral nodecode-cell-demo::demo :not-running t)

(defvar *demo-start-fails* nil
  "When true the demo's :START signals, after it has registered everything
before it: the partial-start seam.")

(defvar *demo-starts* 0 "How many times the demo's :START ran.")
(defvar *demo-stops* 0 "How many times its own inverse ran.")

(nle:define-cell nodecode-cell-demo::demo
  (:section ("nl-demo")
    (:owner "nodecode-cell-demo")
    (:guide "the demo section")
    ("mode" :choice :options '("quiet" "loud") :default "quiet" :doc "how loud")
    ("retries" :integer :default 3 :min 1 :doc "how many tries")
    ("label" :string :doc "a name")
    ("tags" :list :doc "tags")
    ("on" :boolean :default t :doc "whether"))
  ;; `where' is not a declared member: the escape the nested and the
  ;; computed take, beside the values the declaration derived.
  (:settings (lambda (values table)
               (list* :where (nlk:config-string table "where" "here") values)))
  (:tool "demo_tool" (list :description "the demo tool")
         (lambda (arguments) (declare (ignore arguments)) "the demo tool ran"))
  (:help :demo "the demo line" "the demo manual")
  (:hook :frame (lambda (op next) (funcall next op)))
  ;; Registered and taken back, never run: the gates read the catalog.
  (:command "demo" #'identity :description "the demo command" :argument-hint "args")
  (:start (lambda ()
            (incf *demo-starts*)
            (nle:on-stop (lambda () (incf *demo-stops*)))
            (when *demo-start-fails*
              (error "demo: the lease is held")))))

(defun demo-start (&rest pairs)
  "START-CELL against a literal shared config holding one `nl-demo'
section built from alternating key/value PAIRS — the way the gateway calls
it."
  (nodecode-cell-demo:start-cell
   (nlk:json-object "nl-demo" (apply #'nlk:make-json-object pairs))))

(defun demo-setting (key)
  (nodecode-cell-demo::setting key))

(defun demo-installed ()
  "What the demo has on the image right now, as a plist of booleans."
  (list :tool (and (nle::find-tool "demo_tool") t)
        :command (and (nle::find-registered-command "demo") t)
        :help (and (assoc :demo nle::*help-topics*) t)
        :frame (and (cell-hook "nodecode-cell-demo" :frame) t)
        :settings (and nodecode-cell-demo::*demo* t)))

(defun demo-all (flag)
  "DEMO-INSTALLED's answer when every contribution is FLAG."
  (list :tool flag :command flag :help flag :frame flag :settings flag))

(defmacro with-demo ((&rest pairs) &body body)
  "Run BODY with the demo started on PAIRS, stopped on unwind, the tool
registry saved around it and the two counters zeroed."
  `(with-saved-globals (nle:*tools* (*demo-starts* 0) (*demo-stops* 0))
     (nlk:with-cleanup ((nodecode-cell-demo::stop-cell)) (demo-start ,@pairs) ,@body)))

;;; --- the scope ------------------------------------------------------------

(deftest cell-scope-undoes-newest-first-and-only-once (let ((log '())))
  (let ((stop (nle:starting
                (push :one log)
                (nle:on-stop (lambda () (push :undo-one log)))
                (push :two log)
                (nle:on-stop (lambda () (push :undo-two log))))))
    (is (functionp stop) "STARTING answers the thunk that closes it")
    (is (equal '(:two :one) log) "the body ran, nothing undone yet")
    (funcall stop)
    (is (equal '(:undo-one :undo-two :two :one) log))
    (funcall stop)
    (is (equal '(:undo-one :undo-two :two :one) log))))

(deftest cell-scope-unwinds-its-own-prefix-when-the-body-signals (let ((log '())))
  (is (signals-error error
        (nle:starting
          (nle:on-stop (lambda () (push :undo-one log)))
          (nle:on-stop (lambda () (push :undo-two log)))
          (error "halfway"))))
  (is (equal '(:undo-one :undo-two) log)))

(deftest cell-scope-an-inverse-that-signals-does-not-strand-the-rest (let ((log '())))
  (let ((stop (nle:starting
                (nle:on-stop (lambda () (push :first log)))
                (nle:on-stop (lambda () (error "this inverse is broken")))
                (nle:on-stop (lambda () (push :last log))))))
    ;; RUN-STOPS warns the inverse that signalled; the gate is the two
    ;; good inverses, not the noise.
    (handler-bind ((warning #'muffle-warning))
      (funcall stop)))
  (is (equal '(:first :last) log) "the two good inverses both ran"))

(deftest cell-scope-on-stop-outside-a-scope-refuses ()
  (is (signals-error error (nle:on-stop (lambda () nil)))))

;;; --- the declaration: what it materializes --------------------------------

(deftest cell-declaration-materializes-the-section-into-typed-settings ()
  (with-demo ("mode" "loud" "retries" 7 "label" "  x  "
              "tags" (vector "a" " " "b") "on" nil "where" "there")
    (is-table (key value) (equal value (demo-setting key))
      (:mode "loud" "a choice inside its options") (:retries 7 "an integer")
      (:label "x" "a string, trimmed as CONFIG-STRING trims") (:where "there")
      (:tags '("a" "b") "a list, empties dropped") (:on nil "an explicit JSON false stays false"))
    (is (signals-error error (demo-setting :nonesuch))))
  (with-demo ()
    (is-table (key value) (equal value (demo-setting key))
      (:mode "quiet" "the declared default") (:retries 3) (:tags nil) (:where "here")
      (:label nil "no default is absent, not a refusal") (:on t "a boolean default")))
  (is (signals-error nodecode-cell-demo::demo-error (demo-setting :mode))))

(deftest cell-declaration-refuses-a-member-the-declaration-rules-out ()
  (is-table (key value) (signals-error nlk:config-refusal (demo-start key value))
    ("mode" "shouty") ("retries" 0) ("retries" "three"))
  (is (equal (demo-all nil) (demo-installed))))

(deftest cell-declaration-derives-the-other-four-section-readers-too ()
  (is-present (section (nlk:find-section '("nl-demo"))) "the declaration registered its section"
    (is (equal "nodecode-cell-demo" (nlk:section-owner section)))
    (is (null (nlk:section-problems section nil)))
    (is (equal "nl-demo: complete" (nlk:section-summary section nil)))
    (is (search "retries (integer, at least 1, default 3)" (nlk:section-text section)))
    (is (search "the demo section" (nlk:section-text section)))
    ;; The :settings clause is the section's own reader, so a writer that asks
    ;; the section before it writes reads what the start reads.
    (is (equal "there" (getf (nlk:section-settings section (nlk:json-object "where" "there")) :where)))))

;;; --- the declaration: what it installs, and takes back --------------------

(deftest cell-declaration-installs-every-contribution-and-takes-it-all-back ()
  (with-saved-globals (nle:*tools* (*demo-starts* 0) (*demo-stops* 0))
    (let ((stop (demo-start)))
      (is (functionp stop) "START-CELL answers a thunk")
      (is (eq stop #'nodecode-cell-demo::stop-cell))
      (is (equal (demo-all t) (demo-installed)))
      (is (equal "the demo tool ran"
                 (funcall (third (nle::find-tool "demo_tool")) (nlk:make-json-object))))
      (is (equal "the demo manual" (nle:help :demo)) "(help :demo) answers the manual")
      ;; and every request carries its line, never its text
      (is (search "  :demo - the demo line" (cell-section "s1" "help")))
      (let ((entry (nle::find-registered-command "demo")))
        (is-present entry "the command is in the catalog, under the cell's key"
          (is (equal "nodecode-cell-demo" (nle::slash-command-owner entry)))))
      (is (= 1 *demo-starts*))
      (funcall stop)
      (is (equal (demo-all nil) (demo-installed)))
      (is (= 1 *demo-stops*) "and ran the declaration's own inverse")
      (is (null nodecode-cell-demo::*stop*) "the scope is spent")
      (funcall stop)
      (is (= 1 *demo-stops*) "a second stop is a no-op")
      (nodecode-cell-demo::stop-cell)
      (is (= 1 *demo-stops*) "and so is STOP-CELL after it"))))

(deftest cell-declaration-a-second-start-leaves-one-of-everything (with-demo ("mode" "quiet"))
  (demo-start "mode" "loud")
  (is (= 2 *demo-starts*) "the second start ran")
  (is (= 1 *demo-stops*) "over the first, which was stopped first")
  (is (equal "loud" (demo-setting :mode)) "the newer settings are the running ones")
  (is (= 1 (count :demo nle::*help-topics* :key #'car)) "one topic")
  (is (= 1 (length (cdr (assoc :frame nle:*hooks*)))) "one frame hook")
  (is (= 1 (count "demo" nle::*registered-commands*
                  :key #'nle::slash-command-name :test #'string=)))
  (is (= 1 (count "demo_tool" nle:*tools* :key #'first :test #'string=)))
  (nodecode-cell-demo::stop-cell)
  (is (equal (demo-all nil) (demo-installed)))
  (is (= 2 *demo-stops*)))

(deftest cell-declaration-a-start-that-fails-halfway-leaves-nothing ()
  (with-saved-globals (nle:*tools* (*demo-starts* 0) (*demo-stops* 0) (*demo-start-fails* t))
    (nlk:with-cleanup ((nodecode-cell-demo::stop-cell))
      (is (signals-error error (demo-start)))
      (is (equal (demo-all nil) (demo-installed)))
      (is (= 1 *demo-stops*) "including the inverse :START itself had recorded")
      (is (null nodecode-cell-demo::*stop*) "and nothing is left to stop")
      ;; the siblings' seam: the next try is clean
      (setf *demo-start-fails* nil)
      (demo-start)
      (is (equal (demo-all t) (demo-installed))))))

;;; --- through the folder loader and the gateway's lifecycle ----------------

(deftest cell-declaration-keeps-the-report-refusal-and-restart-contract (with-cell-fixture (root))
  ;; The same declaration in a real folder: the loader marks it a peripheral,
  ;; a start that refuses is the record's REFUSAL and one standing notice with
  ;; NOTHING installed, and RESTART-CELLS starts it without a relaunch.
  (with-saved-globals (nle:*tools*)
    (write-cell-folder
     root "nl-declared"
     '(("cell" . "(defpackage #:nl-declared (:use #:cl) (:export #:start-cell #:*ready*))
(in-package #:nl-declared)
(defvar *ready* nil)
(nodecode.kernel:define-peripheral declared :not-running t)
(nodecode.evolved:define-cell declared
  (:section (\"nl-declared\")
    (\"mode\" :choice :options '(\"a\" \"b\") :default \"a\" :doc \"which\"))
  (:help :declared \"the declared line\" \"the declared manual\")
  (:command \"declared\" 'identity :description \"the declared command\")
  (:start (lambda ()
            (unless *ready* (error \"nl-declared refuses: not ready\")))))")))
    (let* ((record (first (load-cell-fixture root)))
           (ready (uiop:find-symbol* '#:*ready* '#:nl-declared)))
      (is-present record "the folder loaded"
        (is (eq :peripheral (nlk:cell-kind record))))
      (nle::start-cells)
      (is (null (nlk:cell-started-p record)) "the refused start is not started")
      (is (search "not ready" (nlk:cell-refusal record)) "the refusal is on the record")
      (is (search "nl-declared not started: nl-declared refuses: not ready"
                  (board-line "nl-declared")))
      (is (search "REFUSED" (nlk:cells-report-text '("nl-declared"))))
      (is (null (nle::find-registered-command "declared")))
      (is (null (assoc :declared nle::*help-topics*)))
      (setf (symbol-value ready) t)
      (let ((answer (nle:restart-cells "nl-declared")))
        (is (nlk:cell-started-p record) "RESTART-CELLS started it")
        (is (null (nlk:cell-refusal record)) "and cleared the refusal")
        (is (search "cell nl-declared: peripheral" answer)))
      (is-present (entry (nle::find-registered-command "declared")) "/declared is in the catalog"
        (is (equal "nl-declared" (nle::slash-command-owner entry))))
      (is (equal "the declared manual" (nle:help :declared)))
      (nle::stop-cells)
      (is (null (nlk:cell-started-p record)))
      (is (null (nle::find-registered-command "declared")))
      (is (null (assoc :declared nle::*help-topics*))))))
