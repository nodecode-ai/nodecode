;;;; scribe-test.lisp --- definitions recognized, read, recorded.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test operates on a fixture layer under an isolated per-run temp
;;;; directory — never the operator's folder, never a fixed /tmp path a
;;;; concurrent suite in a shared checkout would collide on.
;;;; The shipped source the reads and override headers consult is a fixture
;;;; tree too, bound as *SOURCE-ROOT*.

(in-package #:nodecode.test)

;;; --- fixtures -------------------------------------------------------------

(defvar *scribe-dir* (format nil "/tmp/nl-scribe-~a/" (sb-posix:getpid))
  "Per-process fixture root: shipped source tree, cell folder, stores.")

(defmacro with-scribe-fixture (&body body)
  "Run BODY with the cell folder, the layer name and the shipped source
root all pointed at a fresh fixture tree, cleaning up however BODY leaves."
  ;; The shipped-definition index is dropped on both sides so no test reads
  ;; another's tree. SETF, not LET: the eval tool runs on a worker thread,
  ;; which must write the same fixture layer this thread reads.
  `(with-saved-globals ((nlk::*cells-directory* (merge-pathnames "cells/" *scribe-dir*))
                        (nlk::*layer-name* "tester")
                        (nlk::*layer-revision* nil)
                        (nlk::*source-root* (merge-pathnames "src/" *scribe-dir*)))
     (ignore-errors (nlk:remove-tree *scribe-dir*))
     (ensure-directories-exist nlk::*source-root*)
     (nlk:forget-shipped-definitions)
     (nlk:with-cleanup ((nlk:forget-shipped-definitions)
                        (ignore-errors (nlk:remove-tree *scribe-dir*)))
       ,@body)))

(defun write-shipped-definition (name definition)
  "Write NAME under the fixture's shipped source root as a file in EVOLVED
holding DEFINITION alone, and drop the index."
  (prog1 (write-temp-file (merge-pathnames name nlk::*source-root*) (evolved-source definition))
    (nlk:forget-shipped-definitions)))

(defun layer-file-text (file-name &aux (path (merge-pathnames file-name (nlk:layer-directory))))
  "The text of FILE-NAME (with extension) in the fixture layer, or NIL."
  (and (probe-file path) (uiop:read-file-string path)))

(defun layer-git (&rest args)
  "git ARGS in the fixture layer: the stripped output."
  (values (apply #'nlk::git-in-layer (nlk:layer-directory) args)))

(defun layer-commits ()
  "How many commits the fixture layer carries."
  (length (nlk:lines (layer-git "log" "--format=%h"))))

;;; --- recognition -----------------------------------------------------------

(deftest scribe-definition-keys-name-the-unit-of-change ()
  (flet ((key (text &aux (*package* (find-package '#:nodecode.evolved)) (*read-eval* nil))
           (nlk:definition-key (read-from-string text))))
    (is-table (expected text) (equal expected (key text))
      ('(:operator "DEFUN" :name "SCRIBE-K1") "(defun scribe-k1 () 1)")
      ('(:operator "DEFPARAMETER" :name "*SCRIBE-K2*") "(defparameter *scribe-k2* 2)")
      ('(:operator "DEFUN" :name "(SETF SCRIBE-K3)") "(defun (setf scribe-k3) (v) v)"
       "a setf function name is a name")
      ('(:operator "DEFMETHOD" :name "SCRIBE-K4"
         :qualifiers ("AROUND") :specializers ("CHAT-MODEL" "T"))
       "(defmethod scribe-k4 :around ((m chat-model) x) x)"
       "a method key carries qualifiers and specializers")
      ('(:operator "DEFMETHOD" :name "SCRIBE-K5" :qualifiers () :specializers ("EQL-:FOO"))
       "(defmethod scribe-k5 ((kind (eql :foo))) kind)"
       "an eql specializer keys on its printed object")
      ('(:operator "DEFSTRUCT" :name "SCRIBE-K6") "(defstruct (scribe-k6 (:copier nil)) a)"))
    (is (null (key "(setf *scribe-k7* 1)")) "a setf is not a definition")
    (is (null (key "(+ 1 2)")))))

(deftest scribe-file-names-are-one-per-definition ()
  (is-table (expected key) (string= expected (nlk::definition-file-name key))
    ("compose-frame" '(:operator "DEFUN" :name "COMPOSE-FRAME"))
    ("_system-prompt_" '(:operator "DEFPARAMETER" :name "*SYSTEM-PROMPT*")
     "earmuffs become underscores, invertibly")
    ("setf-foo" '(:operator "DEFUN" :name "(SETF FOO)"))
    ("view-around-chat-model-t"
     '(:operator "DEFMETHOD" :name "VIEW" :qualifiers ("AROUND") :specializers ("CHAT-MODEL" "T"))
     "each method gets its own file")))

(deftest scribe-scanner-yields-spans-on-form-boundaries ()
  (let* ((text "(in-package #:nodecode.evolved)
;; a comment between forms
(defun scan-a () :a)

#| block |#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter scan-b 2)
  (defun scan-c () 3))
")
         (hits (loop for (key start end package) in (nlk:toplevel-definitions text)
                     collect (list (getf key :operator) (getf key :name)
                                   (subseq text start end)
                                   (package-name package)))))
    (is (= 3 (length hits)) "three defining forms found")
    (is-table (expected which) (equal expected (which hits))
      ('("DEFUN" "SCAN-A" "(defun scan-a () :a)" "NODECODE.EVOLVED") first
       "every hit carries the package the file declares — LOAD-SOURCE reads it")
      ('("DEFPARAMETER" "SCAN-B" "(defparameter scan-b 2)" "NODECODE.EVOLVED") second
       "eval-when subforms carry their own spans")
      ('("DEFUN" "SCAN-C" "(defun scan-c () 3)" "NODECODE.EVOLVED") third)))
  ;; A file whose package this image lacks carries the designator itself, so
  ;; the load side can refuse it by name instead of defining its names in
  ;; whatever package happened to be current.
  (let ((hits (nlk:toplevel-definitions
               (format nil "(in-package #:no-such-package-here)~%(defun scan-d () 4)~%"))))
    (is (= 1 (length hits)) "the definition is still found")
    (is (equal "NO-SUCH-PACKAGE-HERE" (fourth (first hits))))))

(deftest scribe-atomic-write-keeps-a-typeless-name (with-scribe-fixture)
  (let ((path (merge-pathnames "Makefile" *scribe-dir*)))
    (nlk:write-file-atomically path "all:")
    (is (equal "all:" (nlk:read-text path)) "Makefile, not Makefile.tmp")))

(deftest scribe-atomic-write-keeps-a-private-file-the-owners-alone (with-scribe-fixture)
  ;; :PRIVATE is the mode of a file that holds secrets (config.jsonc): 0600
  ;; when the write creates it, narrowed to its owner's bits when another
  ;; writer left it readable by the group or by others.
  (let ((path (merge-pathnames "secrets.jsonc" *scribe-dir*)))
    (flet ((mode () (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
      (nlk:write-file-atomically path "{}" :mode :private)
      (is (= #o600 (mode)) "created 0600")
      (sb-posix:chmod (namestring path) #o644)
      (nlk:write-file-atomically path "{}" :mode :private)
      (is (= #o600 (mode)) "0644 narrowed to 0600")
      (sb-posix:chmod (namestring path) #o400)
      (nlk:write-file-atomically path "{}" :mode :private)
      (is (= #o400 (mode)) "an owner's own narrower bits kept"))))

;;; --- the eval seam writes the layer ------------------------------------------

(deftest scribe-records-a-defun-as-one-committed-layer-file (with-scribe-fixture)
  (let ((text "(defun scribe-r1 () ;; keep me
  :one)"))
    (nle:eval-string text)
    (is-present (file (layer-file-text "scribe-r1.lisp"))
      "the definition has its own file in the layer"
      (is (search text file) "the file carries the model's own bytes, comment included")
      (is-carrying file "(in-package :nodecode.evolved)" (:absent nlk::+override-marker+)))
    (is-present (asd (layer-file-text "tester.asd")) "the layer's .asd exists"
      (is (search "nodecode.kernel:layer" asd) "it is a LAYER system")
      (is (search "(:file \"scribe-r1\")" asd) "and lists the file"))
    (is (search "scribe-r1" (layer-git "log" "--oneline" "-1")))
    (is (string= "" (layer-git "status" "--porcelain")))))

(deftest scribe-commits-its-own-files-past-one-staged-by-hand (with-scribe-fixture)
  ;; With nothing else staged the commit takes the index whole; with a file
  ;; the operator staged by hand it names its own, and the hand file stays
  ;; staged, uncommitted.
  (nle:eval-string "(defun scribe-c1 () 1)")
  (write-temp-file (merge-pathnames "hand.txt" (nlk:layer-directory)) "mine")
  (layer-git "add" "hand.txt")
  (nle:eval-string "(defun scribe-c2 () 2)")
  (let ((committed (nlk:lines (layer-git "show" "--name-only" "--format=" "HEAD"))))
    (is (member "scribe-c2.lisp" committed :test #'string=))
    (is (not (member "hand.txt" committed :test #'string=)) "the hand file is not swept in"))
  (is (equal '("hand.txt") (nlk:lines (layer-git "diff" "--cached" "--name-only"))) "it waits staged")
  (is (= 2 (layer-commits))))

(deftest scribe-records-the-completed-text-of-a-snippet (with-scribe-fixture)
  ;; A snippet of two definitions, the second never closed: each lands in its
  ;; own file, and the second carries the closers the reader added — the
  ;; bytes that evaluated are the bytes that must load.
  (nle:eval-string
   (format nil "(defun scribe-snippet-a () 1)~%(defun scribe-snippet-b ()~%  (+ (scribe-snippet-a) 1"))
  (is (search "(defun scribe-snippet-a () 1)" (layer-file-text "scribe-snippet-a.lisp")))
  (is (search "(+ (scribe-snippet-a) 1))" (layer-file-text "scribe-snippet-b.lisp"))))

(deftest scribe-records-each-progn-member-in-its-own-file (with-scribe-fixture)
  (nle:eval-string
   "(progn (defun scribe-r2a () 1) (defparameter scribe-r2b 2))")
  (is (search "(defun scribe-r2a () 1)" (layer-file-text "scribe-r2a.lisp")))
  (is (search "(defparameter scribe-r2b 2)" (layer-file-text "scribe-r2b.lisp")))
  (is (null (search "scribe-r2a" (layer-file-text "scribe-r2b.lisp"))))
  (is (search "(:file \"scribe-r2a\") (:file \"scribe-r2b\")"
              (remove #\Newline (layer-file-text "tester.asd"))))
  (is (= 1 (layer-commits)) "one eval, one commit"))

(deftest scribe-redefinition-rewrites-the-same-file-and-keeps-its-slot (with-scribe-fixture)
  (nle:eval-string "(defun scribe-r3 () :first)")
  (nle:eval-string "(defun scribe-r3-other () :other)")
  (nle:eval-string "(defun scribe-r3 () :second)")
  (is (search ":second" (layer-file-text "scribe-r3.lisp")) "the file is rewritten")
  (is (null (search ":first" (layer-file-text "scribe-r3.lisp"))))
  (is (search "(:file \"scribe-r3\") (:file \"scribe-r3-other\")"
              (remove #\Newline (layer-file-text "tester.asd"))))
  (is (= 3 (layer-commits)) "every splice is its own commit"))

(deftest scribe-asserts-the-layer-class-on-every-write (with-scribe-fixture)
  ;; An .asd from before the LAYER class: components, no :class. Without
  ;; this rewrite, the loader would take such a file for a compiled
  ;; library.
  (write-temp-file (merge-pathnames "tester.asd" (nlk:layer-directory))
                   "(defsystem \"tester\" :description \"old\" :serial t :components ((:file \"scribe-c1\")))")
  (write-temp-file (merge-pathnames "scribe-c1.lisp" (nlk:layer-directory))
                   "(in-package :nodecode.evolved) (defun scribe-c1 () :old)")
  (nle:eval-string "(defun scribe-c2 () :new)")
  (let ((text (nlk:one-line (layer-file-text "tester.asd"))))
    (is (search ":class nodecode.kernel:layer" text)
        (format nil "the rewrite asserts the class: ~a" text))
    (is-carrying text ":depends-on (\"nodecode\")" ("\"old\"" "a human's description survives")
      "(:file \"scribe-c1\") (:file \"scribe-c2\")"))
  (is-present (record (first (load-cell-fixture nlk::*cells-directory*))) "the record"
    (is (eq :layer (nlk:cell-kind record)))))

(deftest scribe-skips-failed-and-non-defining-forms (with-scribe-fixture)
  (nle:eval-string "(defvar scribe-r4-x (error \"boom\"))")
  (is (null (layer-file-text "scribe-r4-x.lisp")) "a failed eval writes nothing")
  (nle:eval-string "(+ 1 2)")
  (is (null (probe-file (nlk:layer-directory)))))

(deftest scribe-refuses-floor-names-on-the-evals-own-output (with-scribe-fixture)
  ;; Unlocked, so the eval itself succeeds and the scribe's own refusal
  ;; is what stands between the floor and the layer.
  (with-floor-restored
    (nlk:unlock-floor)
    (nlk:with-cleanup ((ignore-errors
                        (fmakunbound (find-symbol "SCRIBE-FLOOR-PROBE" '#:nodecode.kernel))))
      (is-eval "(defun nodecode.kernel::scribe-floor-probe () :no)"
        ("floor material" "the refusal is on the eval's output, where the model reads it")
        (is (null (probe-file (nlk:layer-directory))) "nothing was written")))))

(deftest scribe-records-the-override-of-a-shipped-definition (with-scribe-fixture)
  (write-shipped-definition "tui/fix.lisp" "(defun scribe-r6 () :shipped)")
  (nle:eval-string "(defun scribe-r6 () :layered)")
  (is-present (file (layer-file-text "scribe-r6.lisp")) "the override file"
    (is (search (format nil "~atui/fix.lisp@" nlk::+override-marker+) file))
    (multiple-value-bind (relative recorded current)
        (nlk::layer-file-override (merge-pathnames "scribe-r6.lisp"
                                                   (nlk:layer-directory)))
      (is (string= "tui/fix.lisp" relative))
      (is (string= recorded current) "the digest matches the shipped text now")
      ;; An upgrade moves the shipped definition.
      (write-shipped-definition "tui/fix.lisp" "(defun scribe-r6 () :shipped-v2)")
      (let ((stale (nlk::stale-layer-file (merge-pathnames "scribe-r6.lisp"
                                                           (nlk:layer-directory)))))
        (is (equal "scribe-r6.lisp" (first stale)))))))

(deftest scribe-a-stale-override-is-skipped-at-boot (with-scribe-fixture)
  ;; An override recorded over one shipped text does not load over another.
  ;; Without that, a layer's FOOTER-FRAME-ROW taken over an earlier shipped
  ;; text would call a helper the release had inlined — undefined at the
  ;; first frame, the shell dead before its footer.
  (write-shipped-definition "tui/fix.lisp" "(defun scribe-r9 () :shipped)")
  (nle:eval-string "(defun scribe-r9 () (scribe-r9-helper-the-upgrade-inlined))")
  ;; The upgrade: the shipped text moves, the process restarts with it.
  (write-shipped-definition "tui/fix.lisp" "(defun scribe-r9 () :shipped-v2)")
  (eval (read-from-string "(defun nodecode.evolved::scribe-r9 () :shipped-v2)"))
  (is-present (record (first (load-cell-fixture nlk::*cells-directory*))) "the layer's record"
    (is (eq :loaded (nlk:cell-state record))
        (format nil "a stale file is no failure: ~a" (nlk:cell-failures record)))
    (is (equal '("scribe-r9.lisp") (mapcar #'car (nlk::cell-skipped record))))
    (is (null (nlk:cell-overrides record)) "and counts no override")
    (is (search "cell tester: SKIPPED scribe-r9.lisp: the shipped definition it overrides changed (tui/fix.lisp)"
                (nlk:cells-folder-text '("tester")))))
  (is (eq :shipped-v2 (funcall (uiop:find-symbol* "SCRIBE-R9" '#:nodecode.evolved)))))

(deftest scribe-a-definition-the-shipped-source-took-over-is-skipped-at-boot (with-scribe-fixture)
  ;; A definition recorded as the layer's own carries no override line, and a
  ;; release that ships it since must not have the layer's older text loaded
  ;; over its own. Without that, the layer's FAILOVER-CHAIN-FOR, recorded the
  ;; hour before the release took the failover chain in, handed the release's
  ;; "slow_start" to its own FAILOVER-TRIGGER, and every turn on a watched
  ;; chain failed: no applicable method for PROVIDER-ERROR-STATUS.
  (nle:eval-string "(defun scribe-r14 () :layered)")
  (is (null (search nlk::+override-marker+ (layer-file-text "scribe-r14.lisp"))))
  ;; The release takes it in; the process restarts with it.
  (write-shipped-definition "engine/fix.lisp" "(defun scribe-r14 () :shipped)")
  (eval (read-from-string "(defun nodecode.evolved::scribe-r14 () :shipped)"))
  (is-present (record (first (load-cell-fixture nlk::*cells-directory*))) "the layer's record"
    (is (equal '("scribe-r14.lisp") (mapcar #'car (nlk::cell-skipped record))))
    (is (null (nlk:cell-overrides record)) "and counts no override")
    (is (search "cell tester: SKIPPED scribe-r14.lisp: the shipped source took it over (engine/fix.lisp)"
                (nlk:cells-folder-text '("tester")))))
  (is (eq :shipped (funcall (uiop:find-symbol* "SCRIBE-R14" '#:nodecode.evolved)))))

(deftest scribe-the-loader-reports-the-layer-it-wrote ()
  ;; End to end: a definition recorded at the seam, the folder loaded the
  ;; way MAIN loads it, the record saying override.
  (eval (read-from-string "(defun nodecode.evolved::scribe-r7 () :shipped)"))
  (with-scribe-fixture
    (nle:eval-string "(defun scribe-r7 () :layered)")
    ;; Pretend the process restarted with the shipped definition.
    (eval (read-from-string "(defun nodecode.evolved::scribe-r7 () :shipped)"))
    (is-present (record (first (load-cell-fixture nlk::*cells-directory*))) "the layer's record"
      (is-shape record (nlk:cell-name string= "tester") (nlk:cell-kind eq :layer))
      (is (eq :loaded (nlk:cell-state record))
          (format nil "the layer loads: ~a" (nlk:cell-failures record)))
      (is (equal '("SCRIBE-R7")
                 (mapcar (lambda (key) (symbol-name (cdr key)))
                         (nlk:cell-overrides record)))))
    (is (eq :layered (funcall (uiop:find-symbol* "SCRIBE-R7" '#:nodecode.evolved))))))

(deftest scribe-appends-durable-facts-inside-a-turn (with-scribe-fixture)
  (with-floor-store (sid "scribe-rfact")
    (let* ((turn (nlk:admit-turn sid (nlk::make-durable-id "command") "go"))
           (nle::*current-durable-turn* turn))
      (nle:eval-string "(defun scribe-r8 () :fact)")
      (is (search sid (layer-git "log" "-1" "--format=%s")))
      (nle:eval-string "(fmakunbound 'scribe-r8)")
      (nlk:complete-turn turn nil))
    (let ((fact (session-event sid :type 'nlk:definition-recorded-event)))
      (is-present fact "the recorded fact landed in the store"
        (is-shape fact (nlk:event-aggregate-type string= "organism")
          (nlk:definition-recorded-event-text string= "(defun scribe-r8 () :fact)")
          (nlk:definition-recorded-event-names equalp #("SCRIBE-R8"))
          (nlk:definition-recorded-event-layer string= "tester"))
        (is (plusp (length (nlk:definition-recorded-event-commit fact))))
        (is (nlk:event-kind-replay-suppressed-p (nlk:event-kind fact)))))
    (let ((fact (session-event sid :type 'nlk:definition-forgotten-event)))
      (is-present fact "the forgotten fact landed in the store"
        (is (equalp #("SCRIBE-R8") (nlk:definition-forgotten-event-names fact)))
        (is (string= "FMAKUNBOUND" (nlk:definition-forgotten-event-operator fact)))
        (is (string= "tester" (nlk:definition-forgotten-event-layer fact)))
        (is (plusp (length (nlk:definition-forgotten-event-commit fact))))
        (is (nlk:event-kind-replay-suppressed-p (nlk:event-kind fact)))))))

;;; --- the read side -----------------------------------------------------------

(deftest scribe-definition-reads-prefer-the-layer (with-scribe-fixture)
  (write-shipped-definition "engine/fix.lisp" "(defun scribe-r9 ()
  :shipped)")
  (is (string= "(defun scribe-r9 ()
  :shipped)" (nlk:definition-text 'nodecode.evolved::scribe-r9)))
  (nle:eval-string "(defun scribe-r9 () :layered)")
  (is (string= "(defun scribe-r9 () :layered)"
               (nlk:definition-text 'nodecode.evolved::scribe-r9)))
  (let ((locations (nlk:definition-location "scribe-r9")))
    (is (= 2 (length locations)) "both residences are listed")
    (is (search "cells/" (namestring (first (first locations)))))
    (is (search "engine/fix.lisp" (namestring (first (second locations))))))
  (is (null (nlk:definition-text 'nodecode.evolved::scribe-r9-nowhere))))

(deftest scribe-layer-revision-follows-the-commits (with-scribe-fixture)
  (is (string= "base" (nlk::layer-revision)) "no layer, the shipped base")
  (nle:eval-string "(defun scribe-r10 () :one)")
  (let ((first (nlk::layer-revision)))
    (is (uiop:string-prefix-p "tester:" first) "a commit names the layer and its sha")
    (is (search first (nlk:current-definition-revision)))
    (nle:eval-string "(defun scribe-r10 () :two)")
    (is (not (string= first (nlk::layer-revision))))))

(deftest scribe-records-only-the-definition-out-of-a-mixed-progn (with-scribe-fixture)
  ;; A PROGN carrying a definition and a call records the definition alone:
  ;; the call must not run again at every boot.
  (let ((result (nle:eval-string "(progn (defun scribe-r11 (n) (* 2 n)) (scribe-r11 21))")))
    (is (search "[layer: kept scribe-r11" result) "the keep is said on the result")
    (is (uiop:string-suffix-p result "42") "ahead of the value"))
  (is-present (file (layer-file-text "scribe-r11.lisp")) "the definition's file"
    (is (search "(defun scribe-r11 (n) (* 2 n))" file))
    (is (null (search "(scribe-r11 21)" file)) "the call is not in the file")))

(deftest scribe-records-a-define-hook-as-advice-not-a-definition (with-scribe-fixture)
  (write-shipped-definition "engine/fix.lisp" "(defun scribe-r12 (x)
  (list :shipped x))")
  (nle:eval-string "(defun scribe-r12 (x) (list :shipped x))")
  (nle:eval-string "(define-hook scribe-r12 \"one\" (next x) (cons :one (funcall next x)))")
  (nle:eval-string "(define-hook scribe-r12 \"two\" (next x) (cons :two (funcall next x)))")
  (is-present (file (layer-file-text "scribe-r12-one.lisp"))
    "a hook file is keyed by name and hook key"
    (is (search "(define-hook scribe-r12 \"one\"" file) "and carries the form")
    (is (search "scribe-r12 one — define-hook" file) "the header labels it")
    (is (null (search nlk::+override-marker+ file))))
  (is (layer-file-text "scribe-r12-two.lisp"))
  (is (search "(:file \"scribe-r12-two\")" (layer-file-text "tester.asd")))
  (is (string= "(defun scribe-r12 (x) (list :shipped x))"
               (nlk:definition-text 'nodecode.evolved::scribe-r12)))
  (is (= 2 (length (nlk:definition-location "scribe-r12"))))
  ;; Advice on the floor is recorded: the name is what the hook wraps,
  ;; not what it defines.
  (nle:eval-string "(define-hook nlk:definition-text \"floor\" (next name) (funcall next name))")
  (is (layer-file-text "definition-text-floor.lisp"))
  (nlk:unhook 'nlk:definition-text "floor")
  (nlk:unhook 'nodecode.evolved::scribe-r12 "one")
  (nlk:unhook 'nodecode.evolved::scribe-r12 "two"))

;;; --- the result teaches the layer ----------------------------------------------
;;; The system prompt no longer carries the layer's rules; the eval's own
;;; output does, each at the moment it applies.

(deftest scribe-says-kept-by-name (with-scribe-fixture)
  ;; The names alone, the first keep of a session included: where the layer
  ;; lives and how it loads are (help :layer)'s.
  (is-eval "(defun scribe-k1 () :one)"
    ("[layer: kept scribe-k1]" result)
    (:absent "committed" "no filing lecture")
    (is (uiop:string-suffix-p result "SCRIBE-K1") "the value still closes the result"))
  (is-eval "(defun scribe-k2 () :two)" ("[layer: kept scribe-k2]" result))
  (is-eval "(fmakunbound 'scribe-k1)" ("[layer: forgot scribe-k1]" result)))

(deftest scribe-keeps-a-shipped-override-without-advice (with-scribe-fixture)
  (write-shipped-definition "engine/fix.lisp" "(defun scribe-k4 (x)
  (list :shipped x))")
  (is-eval "(defun scribe-k4 (x) (list :mine x))"
    ("[layer: kept scribe-k4]" result)
    (:absent "overrides" "a defun over a shipped one is a keep like any other")))

(deftest scribe-names-a-change-the-layer-will-not-carry (with-scribe-fixture)
  ;; The one blind spot the prompt used to warn about up front: a definition
  ;; built inside another form is live but unkept. Now the result says so
  ;; at the moment it happens, and only then.
  (is-carrying (hidden (nle:eval-string "(eval (read-from-string \"(defun scribe-k5 () :hidden)\"))"))
    ("[layer: scribe-k5 changed but not kept — only a toplevel defining form is recorded"
     hidden)
    (is (null (layer-file-text "scribe-k5.lisp")) "and indeed nothing was written"))
  (is (search "scribe-k6 changed but not kept"
              (nle:eval-string "(setf (fdefinition 'scribe-k6) (lambda () 1))")))
  (is-table (source) (null (search "not kept" (nle:eval-string source)))
    ("(defun scribe-k7 () 1)" "a kept definition is not nagged about")
    ("(defstruct scribe-k8 a b)"
     "a kept form's by-products — accessors, constructor — are its own")
    ("(defparameter *scribe-k9* 1)" "a value is not a definition the report watches")
    ("(setf *scribe-k9* 2)" "so a setf of it says nothing")))

(deftest scribe-spares-a-name-another-form-kept (with-scribe-fixture)
  ;; Without this, a window would name kept definitions "changed but not kept".
  ;; A name the layer takes inside a window is durable; only what nothing
  ;; kept is unkept.
  (let ((before (nlk:definitions-before)))
    ;; Another form — another session, another thread — keeps a definition
    ;; while this window is open.
    (nle:eval-string "(defun scribe-k11 () :kept-elsewhere)")
    (let ((reported (with-output-to-string (*standard-output*)
                      (nlk:report-unkept-changes before '(sh "true") nil))))
      (is (null (search "scribe-k11" reported)) reported)))
  ;; A name held for its first traversal is a keep, pending: a hold armed
  ;; inside another form -- a race's swap, nested in its call -- spares it too.
  (let* ((symbol (intern "SCRIBE-K13" '#:nodecode.evolved))
         (previous (setf (fdefinition symbol) (lambda () :standing)))
         (before (nlk:definitions-before))
         (form `(defun ,symbol () :held)))
    (handler-bind ((warning #'muffle-warning)) (eval form))
    (is (let ((*standard-output* (make-broadcast-stream)))
          (nlk:hold-definitions (list (list "SCRIBE-K13" symbol previous)) form "(defun scribe-k13 () :held)" 27)))
    (let ((reported (with-output-to-string (*standard-output*)
                      (nlk:report-unkept-changes before '(sh "true") nil))))
      (is (null (search "scribe-k13" reported)) reported))
    (nlk:revert symbol)
    (is (eq :standing (funcall symbol)) "and the revert puts back what it displaced"))
  ;; And a change made past its keep, which nothing kept again, is still
  ;; this window's to report.
  (let ((before (nlk:definitions-before)))
    (nle:eval-string "(defun scribe-k12 () :kept-then-changed)")
    (nle:eval-string "(setf (fdefinition 'scribe-k12) (lambda () :changed-after))")
    (let ((reported (with-output-to-string (*standard-output*)
                      (nlk:report-unkept-changes before '(sh "true") nil))))
      (is (search "scribe-k12 changed but not kept" reported) reported))))

(deftest scribe-names-a-hook-call-as-live-only (with-scribe-fixture)
  (nle:eval-string "(defun scribe-k10 (x) x)")
  (let ((call (nle:eval-string "(hook 'scribe-k10 \"live\" (lambda (next &rest args) (apply next args)))"))
        (form (nle:eval-string "(define-hook scribe-k10 \"kept\" (next x) (funcall next x))")))
    (is (search "[layer: (hook ...) advice is live-only; (define-hook name \"key\" (next &rest args) ...) is the kept form]"
                call)
        call)
    (is (null (search "live-only" form)) "the defining form is the kept one")
    (is (search "[layer: kept scribe-k10 kept" form) form))
  (nlk:unhook 'nodecode.evolved::scribe-k10 "live")
  (nlk:unhook 'nodecode.evolved::scribe-k10 "kept"))

;;; --- forgetting ----------------------------------------------------------------

(deftest scribe-forgets-a-definition-at-the-seam (with-scribe-fixture)
  (nle:eval-string "(defun scribe-f1 () :one)")
  (nle:eval-string "(defun scribe-f1-other () :other)")
  (nle:eval-string "(fmakunbound 'scribe-f1)")
  (is (null (layer-file-text "scribe-f1.lisp")) "the forgotten definition's file is gone")
  (is (search ":other" (layer-file-text "scribe-f1-other.lisp")) "its neighbour stays")
  (is-carrying (asd (remove #\Newline (layer-file-text "tester.asd")))
    (:absent "(:file \"scribe-f1\")" (format nil "the .asd lost the entry: ~a" asd))
    ("(:file \"scribe-f1-other\")" "and kept the neighbour's"))
  (is (search "forgot scribe-f1" (layer-git "log" "-1" "--format=%s")))
  (is (string= "" (layer-git "status" "--porcelain")))
  (is (search "tester:" (nlk::layer-revision)) "the revision follows the commit"))

(deftest scribe-forgetting-a-name-the-release-defines-gives-it-back (with-scribe-fixture)
  ;; The layer's upkeep turn, handed files a boot skipped because the release
  ;; took them over, forgot them as told -- (fmakunbound 'failover-trigger) --
  ;; and the act took the release's definition out of the serving image with
  ;; the file: every turn after failed, FAILOVER-CHAIN-FOR undefined.
  (nle:eval-string "(defun scribe-f6 () :layered)")
  (nle:eval-string "(defun scribe-f6-own () :own)")
  (write-shipped-definition "engine/fix.lisp" "(defun scribe-f6 () :shipped)")
  ;; The boot skipped the layer's file; the release's definition stands.
  (eval (read-from-string "(defun nodecode.evolved::scribe-f6 () :shipped)"))
  (is-eval "(fmakunbound 'scribe-f6)"
    "[layer: forgot scribe-f6]"
    "[layer: the release's scribe-f6 stands again (engine/fix.lisp)]")
  (is (null (layer-file-text "scribe-f6.lisp")))
  (is (eq :shipped (funcall (uiop:find-symbol* "SCRIBE-F6" '#:nodecode.evolved))))
  ;; A name only the layer defined leaves the image with its file.
  (is-eval "(fmakunbound 'scribe-f6-own)" (:absent "stands again"))
  (is (not (fboundp (uiop:find-symbol* "SCRIBE-F6-OWN" '#:nodecode.evolved)))))

(deftest scribe-forgets-a-file-whose-definition-never-loaded (with-scribe-fixture)
  ;; A defparameter whose initform needs a running channel would fail at
  ;; every boot, so its name would never be bound. MAKUNBOUND of an unbound
  ;; symbol is a no-op in the image and still the act the scribe answers.
  (let ((directory (nlk:layer-directory)))
    (nlk::write-layer-system (nlk::layer-system-file) "tester" "_scribe-f2_")
    (write-temp-file (merge-pathnames "_scribe-f2_.lisp" directory)
                     "(in-package :nodecode.evolved)
(defparameter *scribe-f2* (error \"the channel is not running\"))")
    (nlk:git-commit-all directory "seed"))
  (let ((result (nle:eval-string "(makunbound '*scribe-f2*)")))
    (is (search "[layer: forgot *scribe-f2*]" result) (format nil "the forget is said: ~a" result))
    (is (null (search "not recorded" result)) (format nil "nothing to complain about: ~a" result)))
  (is (null (layer-file-text "_scribe-f2_.lisp")) "the file is gone")
  (is (null (search "_scribe-f2_" (layer-file-text "tester.asd"))))
  (is (search "forgot *scribe-f2*" (layer-git "log" "-1" "--format=%s")))
  (is (string= "" (layer-git "status" "--porcelain")) "the tree is clean"))

(deftest scribe-forgets-a-file-whose-body-does-not-read-here (with-scribe-fixture)
  ;; The layer cloned to a box without the channel cell: the body names
  ;; NCD, a package this image lacks, so the file neither loads nor reads
  ;; whole. Its key still comes from the head, and the act still forgets it.
  (nle:eval-string "(defun scribe-f8-other () :other)")
  (let ((path (merge-pathnames "_scribe-f8_.lisp" (nlk:layer-directory))))
    (write-temp-file path "(in-package :nodecode.evolved)
(defparameter *scribe-f8* (scribe-no-such-package:request \"POST\" \"/x\"))")
    (nlk::write-layer-system (nlk::layer-system-file) "tester" "_scribe-f8_")
    (nlk:git-commit-all (nlk:layer-directory) "by hand")
    (is (equal '((:operator "DEFPARAMETER" :name "*SCRIBE-F8*"))
               (mapcar #'first (nlk::toplevel-definitions (uiop:read-file-string path))))))
  (nle:eval-string "(makunbound '*scribe-f8*)")
  (is (null (layer-file-text "_scribe-f8_.lisp")) "the file is gone")
  (is (null (search "_scribe-f8_" (layer-file-text "tester.asd"))) "and its entry")
  (is (search "forgot *scribe-f8*" (layer-git "log" "-1" "--format=%s")))
  (is (string= "" (layer-git "status" "--porcelain")) "the tree is clean"))

(deftest scribe-forgetting-an-uncommitted-hand-file-commits-nothing (with-scribe-fixture)
  ;; A human dropped a file and its .asd entry into the layer without
  ;; committing; the act takes them back out and the tree is at HEAD again.
  (nle:eval-string "(defun scribe-f9-other () :other)")
  (write-temp-file (merge-pathnames "_scribe-f9_.lisp" (nlk:layer-directory))
                   (evolved-source "(defparameter *scribe-f9* 9)"))
  (nlk::write-layer-system (nlk::layer-system-file) "tester" "_scribe-f9_")
  (let ((before (layer-git "log" "--format=%h"))
        (result (nle:eval-string "(makunbound '*scribe-f9*)")))
    (is (search "[layer: forgot *scribe-f9*]" result) (format nil "the forget is said: ~a" result))
    (is (null (search "not recorded" result)) (format nil "nothing to complain about: ~a" result))
    (is (null (layer-file-text "_scribe-f9_.lisp")) "the file is gone")
    (is (null (search "_scribe-f9_" (layer-file-text "tester.asd"))) "and its entry")
    (is (string= before (layer-git "log" "--format=%h")) "no commit: HEAD already says so")
    (is (string= "" (layer-git "status" "--porcelain")) "the tree is clean")))

(deftest scribe-forget-then-define-in-one-progn-is-a-rewrite (with-scribe-fixture)
  (nle:eval-string "(defun scribe-f3 () :a)")
  (nle:eval-string "(progn (fmakunbound 'scribe-f3) (defun scribe-f3 () :b))")
  (is-present (file (layer-file-text "scribe-f3.lisp")) "the file is back"
    (is (search ":b" file) "with the new body"))
  (is (= 1 (ppcre:count-matches '(:sequence "(:file \"scribe-f3\")") (layer-file-text "tester.asd"))))
  (is (string= "" (layer-git "status" "--porcelain"))))

(deftest scribe-unhook-forgets-the-hook-file-only (with-scribe-fixture)
  (nle:eval-string "(defun scribe-f4 (x) x)")
  (nle:eval-string "(define-hook scribe-f4 \"twice\" (next x) (* 2 (funcall next x)))")
  (is (layer-file-text "scribe-f4-twice.lisp") "the hook has its file")
  (nle:eval-string "(unhook 'scribe-f4 \"twice\")")
  (is (null (layer-file-text "scribe-f4-twice.lisp")) "unhook takes the hook file")
  (is (layer-file-text "scribe-f4.lisp") "and leaves the function it advised")
  (is (search "forgot scribe-f4 twice" (layer-git "log" "-1" "--format=%s"))
      (format nil "the commit names the hook: ~a" (layer-git "log" "-1" "--format=%s"))))

(deftest scribe-forgetting-an-unrecorded-name-is-silent (with-scribe-fixture)
  (nle:eval-string "(defun scribe-f5 () :a)")
  (let ((before (layer-git "log" "--format=%h"))
        (result (nle:eval-string "(fmakunbound 'scribe-f5-never)")))
    (is (null (search "[layer:" result)) "no line on the eval's output")
    (is (string= before (layer-git "log" "--format=%h")) "no commit")))

(deftest scribe-prunes-a-hand-deleted-file-at-the-next-write (with-scribe-fixture)
  (nle:eval-string "(defun scribe-f6a () :a)")
  (nle:eval-string "(defun scribe-f6b () :b)")
  (delete-file (merge-pathnames "scribe-f6a.lisp" (nlk:layer-directory)))
  (nle:eval-string "(defun scribe-f6c () :c)")
  (is-carrying (asd (remove #\Newline (layer-file-text "tester.asd")))
    (is (null (search "(:file \"scribe-f6a\")" asd))
        (format nil "the entry for the deleted file is pruned: ~a" asd))
    ("(:file \"scribe-f6b\") (:file \"scribe-f6c\")" "the rest keep their order"))
  (is (search "D scribe-f6a.lisp" (layer-git "status" "--porcelain"))))

(deftest scribe-layer-revision-reads-the-repository-files (with-scribe-fixture)
  ;; The shell's first frame asks for the revision; the answer comes from
  ;; .git/HEAD, the ref it names, or packed-refs — never a git process —
  ;; and names the layer the cell report loaded.
  (nle:eval-string "(defun scribe-r13 () :one)")
  (let* ((directory (nlk::layer-directory))
         (expected (nlk::git-in-layer directory "rev-parse" "HEAD")))
    (is (= 40 (length expected)) "the fixture committed")
    (is (equal expected (nlk::git-head-sha directory)))
    (nlk::git-in-layer directory "pack-refs" "--all")
    (is (null (probe-file (merge-pathnames ".git/refs/heads/master" directory))))
    (is (equal expected (nlk::git-head-sha directory)))
    (flet ((revision (&rest cells &aux (nlk::*cells* cells) (nlk::*layer-revision* nil))
             (nlk::layer-revision)))
      (is (equal (format nil "tester:~a" (subseq expected 0 7))
                 (revision (nlk::make-cell :name "tester" :path directory :kind :layer))))
      (is (equal "base" (revision)))
      (let ((empty (merge-pathnames "empty-layer/" nlk::*cells-directory*)))
        (ensure-directories-exist empty)
        (nlk::git-in-layer empty "init" "-q")
        (is (null (nlk::git-head-sha empty)) "a repository with no commit has no sha")
        (is (equal "base" (revision (nlk::make-cell :name "empty-layer" :path empty
                                                    :kind :layer))) "and reads as the base")))))

;;; --- units and snippet packages -------------------------------------------

(defmacro scribe-unit (&body body)
  "A unit operator for the tests: its body is definitions kept together."
  `(progn ,@body))

(deftest scribe-a-unit-is-filed-whole-once-per-definition (with-scribe-fixture)
  ;; A PROGN splits into one file per definition; a unit does not, because
  ;; its definitions may need each other at the next boot. Each name gets
  ;; its own file holding the unit whole, so a name can still be found,
  ;; redefined and forgotten on its own.
  (with-saved-globals ((nlk:*unit-operators* (cons "SCRIBE-UNIT" nlk:*unit-operators*)))
    (let* ((unit "(nodecode.test::scribe-unit (defun scribe-u1 () 1) (defun scribe-u2 () (scribe-u1)))")
           (result (nle:eval-string unit)))
      (is (search "[layer: kept" result) result)
      (is (and (search "scribe-u1" result) (search "scribe-u2" result)))
      (is (search unit (layer-file-text "scribe-u1.lisp")))
      (is (search unit (layer-file-text "scribe-u2.lisp")))
      (is (= 2 (length (nlk:definition-location 'scribe-u1))))
      (is-eval "(fmakunbound 'scribe-u1)"
        (is (null (layer-file-text "scribe-u1.lisp")))
        (is (layer-file-text "scribe-u2.lisp"))
        ("kept whole" "and the act says so")))))

(deftest scribe-a-snippet-package-opens-its-layer-file (with-scribe-fixture)
  ;; A snippet that opens with (in-package ...) reads and defines there, and
  ;; its layer file opens with the same package so the bytes mean at boot
  ;; what they meant at the eval.
  (let ((result (nle:eval-string "(in-package :cl-user) (defun scribe-pkg-1 () :kept)")))
    (is (search "[layer: kept scribe-pkg-1" result) result)
    (is (search "(in-package :common-lisp-user)" (layer-file-text "scribe-pkg-1.lisp")))
    (is (fboundp (find-symbol "SCRIBE-PKG-1" :cl-user)))
    (is (nlk:definition-location 'scribe-pkg-1))
    (nle:eval-string "(in-package :cl-user) (fmakunbound 'scribe-pkg-1)")
    (is (null (layer-file-text "scribe-pkg-1.lisp")))))

(deftest scribe-definition-key-reads-a-head-written-as-a-list ()
  (is-each (nlk:definition-key)
    ('(define (f x y) x) '(:operator "DEFINE" :name "F")
     "a lambda-list head names the function alone")
    ('(define g 5) '(:operator "DEFINE" :name "G") "a bare name is the name")
    ('(define-type (tree :a) (leaf)) '(:operator "DEFINE-TYPE" :name "TREE" :qualifiers ("A"))
     "a type's variables are its identity")
    ('(define-instance (eq shape) (define (== a b) t))
     '(:operator "DEFINE-INSTANCE" :name "EQ" :qualifiers ("SHAPE"))
     "an instance is its class and its type")
    ('(define-instance (eq (tree :a)) (define (== a b) t))
     '(:operator "DEFINE-INSTANCE" :name "EQ" :qualifiers ("(TREE A)"))
     "a compound type reads as its elements")
    ('(defstruct (point (:type list)) x y) '(:operator "DEFSTRUCT" :name "POINT")
     "a structure's options are not identity"))
  (is (null (nlk:definition-key '(declare f (integer -> integer))))))

(deftest scribe-floor-is-judged-in-the-snippet-package ()
  ;; EQ is floor material read in EVOLVED (it is CL's), and nothing at all
  ;; read in a package that does not see the kernel.
  (let ((key '(:operator "DEFINE-INSTANCE" :name "EQ" :qualifiers ("SHAPE"))))
    (is (nlk::floor-key-p key "NODECODE.EVOLVED") "the kernel's EQ is floor")
    (is (null (nlk::floor-key-p key "KEYWORD")) "a package with its own EQ is not")))

(deftest a-load-answers-what-it-reached-and-what-differs (with-scribe-fixture)
  ;; A load of shipped source into the process the model runs in answers with
  ;; every name the file defines. The receipt names the process, the count, the
  ;; forms that differ from the committed file, and that nothing is kept.
  (let* ((repo (merge-pathnames "repo/" *scribe-dir*))
         (file (merge-pathnames "loaded.lisp" repo)))
    (write-temp-file file (format nil "(in-package :nodecode.evolved)~%~%(defun scribe-load-a ()~%  1)~%~%(defun scribe-load-b ()~%  2)~%"))
    (let ((result (nle:eval-string (format nil "(load ~s)" (namestring file)))))
      (is (search (format nil "[load: ~a reached this process (pid ~d) only — 2 definitions re-evaluated;"
                          (namestring file) (sb-posix:getpid))
                  result)
          result)
      (is (search "nothing kept, the file is shipped source" result))
      (is (null (search "changed but not kept" result)) "the name dump is gone"))
    (nlk:git-commit-all repo "base")
    (write-temp-file file (format nil "(in-package :nodecode.evolved)~%~%(defun scribe-load-a ()~%  1)~%~%(defun scribe-load-b ()~%  22)~%~%(defun scribe-load-c ()~%  3)~%"))
    (let ((result (nle:eval-string (format nil "(progn (load ~s) :done)" (namestring file)))))
      (is (search "3 definitions re-evaluated, 2 differing from the committed file: scribe-load-b, scribe-load-c;" result)
          result))
    (nlk:git-commit-all repo "next")
    (is (search "none differing from the committed file" (nle:eval-string (format nil "(load ~s)" (namestring file)))))))

(deftest scribe-sends-a-kept-definition-to-the-shells (with-scribe-fixture)
  ;; A snippet's definition is live where the operator types, not only here:
  ;; the layer file it was written to, and the one definition it holds, go
  ;; to SOURCE-LOADED, and the kept line counts the shells told.
  (with-told-shells (seen 1)
    (let ((out (nle:eval-string "(defun scribe-told-1 () :one)")))
      (is (search "sent to 1 shell" out) out)
      (is (equal '(("scribe-told-1.lisp" ("DEFUN" . "scribe-told-1"))) seen)))))

(deftest keeps-are-refused-under-pristine (with-scribe-fixture)
  ;; The floor keeps nothing: the image act stands, the layer gets no file.
  (let ((nlk:*pristine* t))
    (is (search "[pristine: nothing is kept" (nle:eval-string "(defun pristine-probe (x) x)")))
    (is (null (layer-file-text "pristine-probe.lisp")) "nothing written")
    (is (= 1 (evolved-fn "PRISTINE-PROBE" 1)) "the definition is live, as Lisp has it")))
