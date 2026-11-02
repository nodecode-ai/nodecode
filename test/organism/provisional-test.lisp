;;;; provisional-test.lisp --- a redefinition that must survive its own execution.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Without this, a redefinition of AGENT-LOOP — the shipped turn driver —
;;;; with a body calling a name that does not exist would land in the layer
;;;; before the body ever ran; every later turn would die at the top of the
;;;; loop and the restart would reload the same cut.
;;;;
;;;; Every test drives the scribe fixture (scribe-test.lisp), so the layer
;;;; written or NOT written is a fixture tree, never the operator's folder.
;;;; The stack the eval is judged against is handed in through
;;;; NLK:*CALLER-FRAMES* the way the tool seam hands it in — the dispatch
;;;; stack, not this thread's — except in the one gate that proves a live
;;;; stack read names its own caller.

(in-package #:nodecode.test)

;;; --- fixtures ---------------------------------------------------------------

(defun eval-call (form)
  "A provider reply whose one tool call is eval of FORM."
  (wire-message "assistant" :tool-calls
                (vector (wire-tool-call "c1" "eval"
                                        (nlk:encode-json-object (nlk:json-object "form" form))))))

(defmacro eval-from-stack ((&rest frame-names) text)
  "Evaluate TEXT through the eval seam as though it had been dispatched from
a stack running FRAME-NAMES — one string per frame, the shape
BACKTRACE-NAMES answers."
  `(let ((nlk:*caller-frames* (mapcar #'list (list ,@frame-names))))
     (nle:eval-string ,text)))

;;; --- the stack as evidence --------------------------------------------------

(defun provisional-probe-caller ()
  ;; The CONS is load-bearing: SBCL erases a tail-called frame, so a caller
  ;; that does nothing after the call is not on the stack during it, and
  ;; this gate is about what a live stack read can see.
  (cdr (cons :frames (nlk:backtrace-names :limit 40))))

(deftest provisional-stack-read-names-its-own-caller (let ((frames (provisional-probe-caller))))
  (is (nlk:name-on-stack-p "PROVISIONAL-PROBE-CALLER" frames))
  (is (not (nlk:name-on-stack-p "PROVISIONAL-PROBE-NOT-RUNNING" frames)))
  (is (not (nlk:name-on-stack-p "DEFTEST" '()))))

;;; --- holding ----------------------------------------------------------------

(deftest provisional-definition-on-its-own-stack-is-held-not-written (with-scribe-fixture)
  (nle:eval-string "(defun prov-held (x) (* x 2))")
  (is (search "(* x 2)" (layer-file-text "prov-held.lisp")))
  (is (search "HELD" (eval-from-stack ("PROV-HELD") "(defun prov-held (x) (* x 10))")))
  (is-carrying (text (layer-file-text "prov-held.lisp")) "(* x 2)" (:absent "(* x 10)"))
  (is-present (entry (find "PROV-HELD" (nlk:held-definitions)
                           :key (lambda (e) (getf e :name)) :test #'string=))
    "the held definition is listed as waiting — before anything traverses it"
    (is-shape entry (:state eq :armed "armed") (:restores is "with something to put back")))
  (is (= 20 (evolved-fn "PROV-HELD" 2))))

(deftest provisional-one-clean-traversal-records-the-held-form (with-scribe-fixture)
  (nle:eval-string "(defun prov-confirm (x) (* x 2))")
  (let ((commits (layer-commits)))
    (eval-from-stack ("PROV-CONFIRM") "(defun prov-confirm (x) (* x 10))")
    (is (= commits (layer-commits)) "the hold commits nothing")
    (is (= 30 (evolved-fn "PROV-CONFIRM" 3)) "the first traversal returns")
    (is (search "(* x 10)" (layer-file-text "prov-confirm.lisp")))
    (is (= (1+ commits) (layer-commits)) "one traversal, one commit")
    (is (null (nlk:held-definitions)) "nothing is held any more")
    (is (= 50 (evolved-fn "PROV-CONFIRM" 5)) "the definition still answers")
    (is (= (1+ commits) (layer-commits)))))

(deftest provisional-a-batch-records-only-once-every-held-name-has-returned (with-scribe-fixture)
  ;; One form, two names on its stack: a clean return of one is no proof of
  ;; the other. Redefining AGENT-LOOP and EXECUTE-TOOL-CALL together, the
  ;; next tool call returns long before the loop does, and would otherwise
  ;; write a loop that never ran (proofs/SelfEditHold.lean, trace pairBroken).
  (nle:eval-string "(progn (defun prov-batch-a (x) (* x 3)) (defun prov-batch-b (x) (* x 5)))")
  (let ((commits (layer-commits)))
    (eval-from-stack ("PROV-BATCH-A" "PROV-BATCH-B")
                     "(progn (defun prov-batch-a (x) (* x 2)) (defun prov-batch-b (x) (prov-batch-absent x)))")
    (is (= 4 (evolved-fn "PROV-BATCH-A" 2)) "one held name returns")
    (is (= commits (layer-commits)) "the batch still commits nothing")
    (is (= 2 (length (nlk:held-definitions))) "both names wait on the one that has not run")
    (signals-error undefined-function (evolved-fn "PROV-BATCH-B" 1))
    (is (= commits (layer-commits)) "the broken batch never reaches the layer")
    (is (null (nlk:held-definitions)))
    (is (= 3 (evolved-fn "PROV-BATCH-A" 1)) "the revert puts back every name of the batch")
    (is (= 5 (evolved-fn "PROV-BATCH-B" 1)))))

(deftest provisional-the-last-held-name-to-return-records-the-batch-once (with-scribe-fixture)
  ;; proofs/SelfEditHold.lean, trace pairClean
  (nle:eval-string "(progn (defun prov-pair-a (x) (* x 3)) (defun prov-pair-b (x) (* x 5)))")
  (let ((commits (layer-commits)))
    (eval-from-stack ("PROV-PAIR-A" "PROV-PAIR-B")
                     "(progn (defun prov-pair-a (x) (* x 2)) (defun prov-pair-b (x) (* x 7)))")
    (is (= 4 (evolved-fn "PROV-PAIR-A" 2)))
    (is (= 6 (evolved-fn "PROV-PAIR-A" 3)) "a second return of the same name proves nothing new")
    (is (= commits (layer-commits)))
    (is (= 14 (evolved-fn "PROV-PAIR-B" 2)))
    (is (= (1+ commits) (layer-commits)) "the last name's return writes the batch, once")
    (is (search "(* x 7)" (layer-file-text "prov-pair-b.lisp")))
    (is (null (nlk:held-definitions)))))

(deftest provisional-signalling-traversal-reverts-and-wakes-the-session (with-scribe-fixture)
  ;; The kernel's one session-ingress seam collects (SESSION-ID . TEXT).
  (let* ((wakes '())
         (nlk:*session-wake* (lambda (session-id text) (push (cons session-id text) wakes))))
    (with-floor-store (sid "prov-revert")
      (nle:eval-string "(defun prov-revert (x) (* x 2))")
      ;; Inside a turn, the way the eval seam always runs: the session the
      ;; hold carries is the one the wake has to reach.
      (let* ((turn (nlk:admit-turn sid (nlk::make-durable-id "command") "go"))
             (nle::*current-durable-turn* turn))
        (eval-from-stack ("PROV-REVERT")
                         "(defun prov-revert (x) (declare (ignore x)) (error \"kaboom\"))")
        (nlk:complete-turn turn nil))
      (signals-error simple-error (evolved-fn "PROV-REVERT" 1))
      (is (= 8 (evolved-fn "PROV-REVERT" 4)))
      (is (search "(* x 2)" (layer-file-text "prov-revert.lisp")))
      (is (null (nlk:held-definitions)) "and nothing is left holding")
      (is-present (wake (first wakes)) "the session is woken through the one ingress"
        (is (string= sid (car wake)) "in the session that defined it")
        (is-carrying (text (cdr wake)) ("PROV-REVERT" "naming the definition")
          ("SIMPLE-ERROR" "the condition that killed it") ("kaboom" "and what it said")))
      (is (= 1 (length wakes)) "once, however many frames unwound"))))

(deftest provisional-re-arming-inherits-the-original-escrow (with-scribe-fixture)
  (nle:eval-string "(defun prov-inherit (x) (* x 2))")
  (eval-from-stack ("PROV-INHERIT")
                   "(defun prov-inherit (x) (declare (ignore x)) (error \"v2\"))")
  (eval-from-stack ("PROV-INHERIT")
                   "(defun prov-inherit (x) (declare (ignore x)) (error \"v3\"))")
  (signals-error simple-error (evolved-fn "PROV-INHERIT" 1))
  (is (= 6 (evolved-fn "PROV-INHERIT" 3))))

(deftest provisional-a-cancel-is-not-a-verdict (with-scribe-fixture)
  (nle:eval-string "(defun prov-cancel (x) (* x 2))")
  (eval-from-stack ("PROV-CANCEL")
                   "(defun prov-cancel (x) (declare (ignore x))
                        (error 'turn-cancelled-condition :turn-id \"t-prov\"))")
  (signals-error nlk:turn-cancelled-condition (evolved-fn "PROV-CANCEL" 1))
  (is (find "PROV-CANCEL" (nlk:held-definitions)
            :key (lambda (e) (getf e :name)) :test #'string=))
  (is (null (search "(defun prov-cancel (x) (declare"
                    (or (layer-file-text "prov-cancel.lisp") ""))))
  ;; Release it rather than leave an armed encapsulation on an EVOLVED
  ;; symbol for the rest of the suite.
  (is (search "reverted" (nlk:revert "PROV-CANCEL")))
  (is (= 4 (evolved-fn "PROV-CANCEL" 2))))

;; the model called the helper it had just
;; defined from its next eval, and that return wrote the layer.
(deftest provisional-a-keyword-list-that-shifts-its-callers-calls-is-refused ()
  ;; (x &optional y) -> (x &key y): the caller's (f 1 2) would hand 2 to the
  ;; keyword parser (proofs/SelfEditHold.lean, main_unsound).
  (is (not (nlk::takes-every-call-p '(x &key y) '(x &optional y))))
  (is (not (nlk::takes-every-call-p '(x &key k) '(x y &key k))))
  (is (not (nlk::takes-every-call-p '(x &optional y &key k) '(x &key k))))
  (is (not (nlk::takes-every-call-p '(x &key k &allow-other-keys) '(x &rest r))))
  ;; under &allow-other-keys only the keyword part's length must be even: (f 1) is odd
  (is (not (nlk::takes-every-call-p '(&rest a &key &allow-other-keys) '(x &optional y))))
  (is (nlk::takes-every-call-p '(&rest a &key &allow-other-keys) '(a b)))
  ;; what takes every call is still taken
  (is (nlk::takes-every-call-p '(x &optional y z w) '(x y &optional z)))
  (is (nlk::takes-every-call-p '(x &optional y &key k) '(x y)))
  (is (nlk::takes-every-call-p '(x &optional y &key k j) '(x &optional y &key k)))
  (is (nlk::takes-every-call-p '(x &rest r &key k) '(x &key k)))
  (is (nlk::takes-every-call-p '(x &rest r) '(x y &optional z &key k))))

(deftest provisional-a-call-from-inside-an-eval-proves-nothing (with-scribe-fixture)
  (nle:eval-string "(defun prov-inside (x) (* x 2))")
  (eval-from-stack ("PROV-INSIDE") "(defun prov-inside (x) (* x 10))")
  (let ((commits (layer-commits)))
    (is (search "30" (nle:eval-string "(prov-inside 3)")) "the model's own call answers")
    (is (= commits (layer-commits)) "and commits nothing")
    (is (find "PROV-INSIDE" (nlk:held-definitions) :key (lambda (e) (getf e :name)) :test #'string=) "the definition is still held")
    (is (= 40 (evolved-fn "PROV-INSIDE" 4)) "a call from outside an eval")
    (is (search "(* x 10)" (layer-file-text "prov-inside.lisp")) "is the proof")
    (is (= (1+ commits) (layer-commits)))
    (is (null (nlk:held-definitions)))))

(deftest provisional-a-definition-that-cannot-take-its-callers-calls-is-refused (with-scribe-fixture)
  (nle:eval-string "(defun prov-shape (x &key y) (list x y))")
  (let ((commits (layer-commits)))
    (is-carrying (text (eval-from-stack ("PROV-SHAPE") "(defun prov-shape (acts) acts)"))
      ("PROV-SHAPE" "naming the definition") ("(x &key y)" "the calls it must take")
      ("(acts)" "what it takes instead") ("not evaluated" "and that nothing ran"))
    (is (equal '(1 2) (evolved-fn "PROV-SHAPE" 1 :y 2)) "the running definition stands")
    (is (null (nlk:held-definitions)))
    (is (= commits (layer-commits))))
  ;; a definition that takes every call is held as before
  (is (search "HELD" (eval-from-stack ("PROV-SHAPE") "(defun prov-shape (x &rest more) (list x more))")))
  (is (search "reverted" (nlk:revert "PROV-SHAPE")))
  ;; (NEW OLD): the first four take every call, the rest do not
  (is (equal '(t t t t nil nil nil nil nil)
             (loop for (new old) in '(((a &rest r) (a &key b)) ((a &optional c) (a)) ((a &key b c) (a &key b))
                                      (() ()) ((a) (a &key b)) ((a b) (a)) ((a) (a &optional c))
                                      ((a &key c) (a &key b)) ((a &key b) (&rest r)))
                   collect (and (nlk::takes-every-call-p new old) t)))))

;;; --- the floor that is kept -------------------------------------------------

(deftest provisional-revert-restores-the-pristine-definition (with-scribe-fixture)
  (nle:eval-string "(defun prov-pristine (x) (* x 2))")
  (let ((nlk:*pristine-definitions* (make-hash-table :test 'equal))
        (symbol (find-symbol "PROV-PRISTINE" (find-package '#:nodecode.evolved))))
    (setf (gethash (cons :function symbol) nlk:*pristine-definitions*)
          (fdefinition symbol))
    ;; A redefinition nothing is standing on: written at once, no hold.
    (nle:eval-string "(defun prov-pristine (x) (* x 100))")
    (is (= 300 (evolved-fn "PROV-PRISTINE" 3)) "the image runs the new one")
    (is (search "reverted" (nlk:revert "PROV-PRISTINE")))
    (is (= 6 (evolved-fn "PROV-PRISTINE" 3)))
    (is (search "already" (nlk:revert "PROV-PRISTINE")))
    (is (search "nothing" (nlk:revert "PROV-NEVER-DEFINED-AT-ALL")))))

;;; --- the incident itself ----------------------------------------------------

(deftest provisional-the-turn-loop-survives-redefining-itself ()
  ;; The model asks for a new AGENT-LOOP from inside a tool call that
  ;; AGENT-LOOP is running, and the new body calls a name the image does
  ;; not have. Without the hold, this would write the layer, kill every
  ;; later turn in the image, and survive a restart.
  (let ((shipped (fdefinition 'nle:agent-loop)))
    ;; Whatever the gate found, this image keeps its loop.
    (nlk:with-cleanup ((ignore-errors (nlk:revert "AGENT-LOOP"))
                       (setf (fdefinition 'nle:agent-loop) shipped))
      ;; The scribe fixture's tree by SETF, not LET: the eval tool runs on a worker thread.
      (with-scribe-fixture
        (let ((session (nle::make-session)))
          (nle:admit session "give yourself a heartbeat")
          (with-stub-provider
              ((eval-call "(defun agent-loop (session &rest keys) (declare (ignore session keys)) (current-time-string))")
               (wire-message "assistant" :content "unreachable"))
            (is (signals-error undefined-function (nle:agent-loop session))))
          (is (null (layer-file-text "agent-loop.lisp")))
          (is (null (nlk:held-definitions)))
          (is (eq shipped (fdefinition 'nle:agent-loop))))))))

(defvar *prov-shipped-run-tool-call* nil
  "The shipped dispatcher, for a redefinition of it that wraps what it displaced.")

(deftest provisional-a-loop-redefined-beside-its-dispatcher-cannot-outlive-the-image ()
  ;; One eval redefines RUN-TOOL-CALL, wrapping the shipped one, and
  ;; AGENT-LOOP, with a body calling a name the image does not have; the
  ;; round carries a second tool call. That call returns through the new
  ;; dispatcher before the loop is entered again, and a batch confirmed by
  ;; one name's return wrote the broken loop there: every later turn died,
  ;; and the restart loaded it again (proofs/SelfEditHold.lean, pairBroken).
  (let ((shipped (fdefinition 'nle:agent-loop))
        (dispatch (fdefinition 'nle::run-tool-call)))
    (setf *prov-shipped-run-tool-call* dispatch)
    (nlk:with-cleanup ((ignore-errors (nlk:revert "AGENT-LOOP"))
                       (ignore-errors (nlk:revert "RUN-TOOL-CALL"))
                       (setf (fdefinition 'nle:agent-loop) shipped
                             (fdefinition 'nle::run-tool-call) dispatch))
      (with-scribe-fixture
        (let ((session (nle::make-session)))
          (nle:admit session "wrap your dispatcher")
          (with-stub-provider
              ((wire-message "assistant" :tool-calls
                             (vector (wire-tool-call "c1" "eval" (nlk:encode-json-object (nlk:json-object "form" "(progn (defun run-tool-call (run turn) (funcall nodecode.test::*prov-shipped-run-tool-call* run turn)) (defun agent-loop (session &rest keys) (declare (ignore session keys)) (prov-loop-absent)))")))
                                     (wire-tool-call "c2" "eval" (nlk:encode-json-object (nlk:json-object "form" "(+ 1 2)")))))
               (wire-message "assistant" :content "unreachable"))
            (is (signals-error undefined-function (nle:agent-loop session))))
          (is (null (layer-file-text "agent-loop.lisp")) "the loop that never ran is never written")
          (is (null (nlk:held-definitions)))
          (is (eq shipped (fdefinition 'nle:agent-loop)) "the image runs its own loop again")
          (is (eq dispatch (fdefinition 'nle::run-tool-call)) "and its own dispatcher"))))))

(deftest provisional-a-helper-named-like-the-turn-runner-leaves-the-runner-running ()
  ;; ARC run od3 bp35 end to end, offline: the model named a game helper RUN,
  ;; the turn runner standing under its eval. The image ran the helper on
  ;; every later submit, (run prompt :session ...) refused nine arguments,
  ;; and the game died before another provider round.
  (let ((shipped (fdefinition 'nle:run))
        (nlk:*session-wake* (lambda (session-id text) (declare (ignore session-id text)))))
    (nlk:with-cleanup ((ignore-errors (nlk:revert "RUN"))
                       (setf (fdefinition 'nle:run) shipped))
      (with-scribe-fixture
        (with-floor-store (sid "prov-run")
          (let ((session (nle:intern-session sid)))
            (with-stub-provider
                ((eval-call "(progn (defun run (acts) acts) (run 1))")
                 (wire-message "assistant" :content "helper defined")
                 (wire-message "assistant" :content "the next turn ran"))
              (nle:run "play" :session session)
              ;; the next submit runs a turn
              (is (null (handler-case (progn (nle:run "go on" :session session) nil) (error (e) e))))))
          (is (= 2 (count-session-events sid "turn.completed")) "both turns complete")
          ;; the model is told the name is taken
          (is (search "not evaluated" (nlk:tool-result-event-result (session-event sid :kind "turn.tool_result"))))
          (is (null (layer-file-text "run.lisp")))
          (is (null (nlk:held-definitions)))
          (is (eq shipped (fdefinition 'nle:run))))))))

;;; --- the battery ------------------------------------------------------------
;;; The hold's one rule asked of a whole folder at once, the way the keeper's
;;; trial child asks it: every shipped name the folder redefines, judged
;;; against the lambda list the image booted with. The folder is a cells-test
;;; fixture tree, the floor the snapshot LOAD-CELLS takes of this image.

(defun battery-shipped (name lambda-list &aux (*package* (find-package '#:nodecode.evolved)))
  "Give EVOLVED a shipped-looking function NAME taking LAMBDA-LIST, for a
fixture folder to redefine."
  ;; read in EVOLVED, as a layer file is, so the lambda list's names are its
  (eval (read-from-string (format nil "(defun ~a ~a nil)" name lambda-list))))

(deftest overrides-take-their-callers-calls-judges-every-override-against-what-it-displaced ()
  (battery-shipped "prov-battery-shape" "(x &key y)")
  (battery-shipped "prov-battery-fine" "(x &key y)")
  (battery-shipped "prov-battery-any" "(a b)")
  (with-cell-fixture (root)
    ;; bp35's shape first; two overrides that take every call; a fresh name
    (write-layer-folder root "nl-battery"
                        '(("prov-battery-shape" . "(defun prov-battery-shape (acts) acts)")
                          ("prov-battery-fine" . "(defun prov-battery-fine (x &rest more) (list x more))")
                          ("prov-battery-any" . "(defun prov-battery-any (&rest args &key &allow-other-keys) args)")
                          ("prov-battery-new" . "(defun prov-battery-new () :new)")))
    (let ((nlk:*pristine-definitions* nil))
      (let ((records (load-cell-fixture root)))
        (is (eq :loaded (nlk:cell-state (first records))) "the folder loads; nothing refuses it")
        (multiple-value-bind (ok failures judged) (nlk:overrides-take-their-callers-calls records)
          (is (not ok) "one override cannot take its callers' calls")
          (is (= 3 judged) "three shipped names were redefined; the fresh one is not judged")
          (is (equal '("PROV-BATTERY-SHAPE") (mapcar #'first failures)) "and it is named")))
      (let ((report (temp-path "battery-red" "json")))
        (is (= 0 (nlk:probe-overrides report)) "the trial form answers 0")
        (let ((json (nlk:decode-json (uiop:read-file-string report))))
          (is-shape json ((:string "verdict") string= "red") ((:integer "overrides") = 3))
          (is-present (failure (aref (nlk:json-array json "failures") 0)) "the report carries the failure"
            (is (string= "PROV-BATTERY-SHAPE" (nlk:json-value failure :string "name")))
            (is (string= "(x &key y)" (nlk:json-value failure :string "shipped")) "the calls it must take")
            (is (string= "(acts)" (nlk:json-value failure :string "override")) "what it takes instead"))
          (is (= 1 (length (nlk:json-array json "failures")))))))))

(deftest overrides-take-their-callers-calls-passes-a-folder-whose-overrides-take-every-call ()
  (battery-shipped "prov-battery-green" "(x &optional y)")
  (with-cell-fixture (root)
    (write-layer-folder root "nl-battery-green"
                        '(("prov-battery-green" . "(defun prov-battery-green (x &optional y z) (list x y z))")))
    (let ((nlk:*pristine-definitions* nil))
      (load-cell-fixture root)
      (let ((report (temp-path "battery-green" "json")))
        (is (= 1 (nlk:probe-overrides report)) "the trial form answers 1")
        (let ((json (nlk:decode-json (uiop:read-file-string report))))
          (is-shape json ((:string "verdict") string= "green") ((:integer "overrides") = 1))
          (is (= 0 (length (nlk:json-array json "failures")))))))))

(deftest overrides-take-their-callers-calls-counts-a-name-that-lost-its-function ()
  ;; A start that FMAKUNBOUNDs a shipped name leaves its callers nothing to
  ;; call: a failure, whatever the lambda lists said.
  (battery-shipped "prov-battery-gone" "(x)")
  (let* ((symbol (find-symbol "PROV-BATTERY-GONE" '#:nodecode.evolved))
         (nlk:*pristine-definitions* (make-hash-table :test 'equal)))
    (setf (gethash (cons :function symbol) nlk:*pristine-definitions*) (fdefinition symbol))
    (fmakunbound symbol)
    (multiple-value-bind (ok failures)
        (nlk:overrides-take-their-callers-calls
         (list (nlk::make-cell :name "nl-gone" :overrides (list (cons :function symbol)))))
      (is (not ok))
      (is (equal '("PROV-BATTERY-GONE") (mapcar #'first failures)))
      (is (eq :unbound (third (first failures))))))
  ;; the floor holds nothing for a name: nothing to judge, nothing failed
  (let ((nlk:*pristine-definitions* nil))
    (is (nlk:overrides-take-their-callers-calls
         (list (nlk::make-cell :name "nl-none"
                               :overrides (list (cons :function (find-symbol "PROV-BATTERY-GONE" '#:nodecode.evolved)))))))))
