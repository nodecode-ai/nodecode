;;;; test-slice.lisp --- what every cell's test system says the same way.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell registers its tests into the SHARED nodecode.test registry
;;;; (the core DEFTEST, with its hermetic machine-state posture) under its own
;;;; name prefix, and runs exactly that slice: so `just <cell>-test'
;;;; never re-runs the core suite, and `just test' never runs a cell's.
;;;; That runner — the prefix predicate, the live-image refusal, the pass/FAIL
;;;; loop, the count line and the two loud endings — was written out once per
;;;; cell in five files that differed only in a name and a prefix string. It
;;;; is DEFINE-TEST-SLICE here instead, one authority for the contract.
;;;;
;;;; Helpers follow it for the same reason. CELL-HINT-SEGMENT reads back
;;;; the composer inline hint a slash entrypoint answers with, which the MCP
;;;; and chrome cells both assert on. The poll a cell asserts a
;;;; cross-thread effect through is the core suite's AWAIT, by :TIMEOUT.
;;;;
;;;; This is development tooling, like the rest of src/tools/: nothing in
;;;; src/src names it, no shipped cell loads it, and the organism never
;;;; sees it. It reaches the image only as a component of nodecode/test,
;;;; the core test system every cell's /test system already depends on, so
;;;; each cell lane gets the one copy and no cell's .asd has to point
;;;; outside its folder for it.

(in-package #:nodecode.test)

;;; --- the slice runner -------------------------------------------------------

(defmacro define-test-slice (name prefix &key (recipe name) start defaults)
  "Define RUN-<NAME>-TESTS over the shared registry, and
with START the cell's <NAME>-CONFIG and <NAME>-START under DEFAULTS."
  ;; NAME is the cell's short name as a string ("guard"): it names the
  ;; functions, the count line and the failure line; the live-image refusal
  ;; points at the `just <RECIPE>-test' recipe, RECIPE defaulting to NAME.
  ;; PREFIX is the symbol-name prefix the cell's DEFTESTs carry ("GUARD",
  ;; "MCP-CELL-") — usually longer than NAME on purpose, so a core test that
  ;; happens to start with the word is not collected.
  ;;
  ;; The runner is RUN-ALL's loop narrowed to the slice: the same refusal to
  ;; run in an image with an open store (the suite rebuilds machine state and
  ;; would take a serving session down with it), one *FAILURES* binding per
  ;; test, an unhandled error recorded rather than thrown, a pass/FAIL line
  ;; each, and two loud endings — the failure list, and a zero count, which
  ;; means the cell's test files did not load rather than that everything
  ;; passed.
  (let ((runner (intern (format nil "RUN-~:@(~a~)-TESTS" name))))
    `(progn
       ;; <NAME>-CONFIG builds a whole shared config holding one NAME section
       ;; from alternating key/value PAIRS, as shasht would decode it (string
       ;; keys, JSON arrays as vectors); DEFAULTS are (KEY FORM) pairs a section
       ;; lacking KEY gains. <NAME>-START calls START -- the cell's START-CELL --
       ;; against that config, the way the gateway calls it.
       ,@(when start
           (let ((config (intern (format nil "~:@(~a~)-CONFIG" name))))
             `((defun ,config (&rest pairs)
                 ,(format nil "A whole shared config with one `~a' section built from PAIRS." name)
                 (let ((section (apply #'nlk:make-json-object pairs)))
                   ,@(loop for (key form) in defaults
                           collect `(alexandria:ensure-gethash ,key section ,form))
                   (nlk:json-object ,name section)))
               (defun ,(intern (format nil "~:@(~a~)-START" name)) (&rest pairs)
                 "START-CELL against a literal config, the way the gateway calls it."
                 (,start (apply #',config pairs))))))
       (defun ,runner (&aux (entries (remove ,prefix *tests* :test-not #'uiop:string-prefix-p
                                             :key (lambda (entry) (symbol-name (car entry))))))
         ,(format nil "Run every ~a test. Same live-image refusal as RUN-ALL: the ~
suite rebuilds machine state, so it never runs inside a serving image." prefix)
         (prog1 (run-suite entries ,(format nil "run-~a-tests refuses to run in a live image: ~
store ~~a is open; run it in a scratch image (just ~a-test)" name recipe) ,name)
           (unless entries
             (error ,(format nil "run-~a-tests matched no tests — registration is broken"
                             name))))))))

;;; --- the cell's config, its start, its hooks -------------------------------

(defmacro with-cell-stop ((start) &body body)
  "Run BODY with STOP bound to the thunk START answers, funcalled on unwind
unless BODY cleared it: a cell started for one test is never left installed."
  `(let ((stop ,start))
     (nlk:with-cleanup ((when stop (funcall stop))) ,@body)))

(defun cell-hook (owner name)
  "OWNER's function on NAME's chain — a point keyword or an advised
function name — or NIL once it is unhooked."
  (cdr (assoc owner (cdr (assoc name nle:*hooks* :test #'equal)) :test #'string=)))

(defmacro define-cell-lifecycle-tests (name &body clauses)
  "Define the four lifecycle tests every cell's slice wrote out by hand."
  ;; What NLE:DEFINE-CELL owns is one contract, so it is asserted from one
  ;; declaration: a start installs what the folder declares and a stop takes
  ;; every bit of it back, a second start leaves one of everything, a vetoed
  ;; section installs nothing and still answers a stop, and a refused one
  ;; signals with nothing left behind.
  ;;
  ;; NAME is the folder's short name as a string ("cron"): it names the tests
  ;; <NAME>-CELL-..., the <NAME>-START that DEFINE-TEST-SLICE defines, the
  ;; key every contribution is filed under (nodecode-<NAME>) and the settings
  ;; variable NODECODE-<NAME>::*<NAME>*. The clauses, all optional:
  ;;
  ;;   (:config PAIR...)    the section a healthy start reads
  ;;   (:hooks POINT...)    the points the start hooks, as forms; the first is
  ;;       the witness the idempotence, veto and refusal tests read
  ;;   (:help TOPIC)        the manual topic the start puts on (help): the
  ;;       witness when no point is hooked
  ;;   (:command SPELLING)  the slash command the start registers
  ;;   (:running FORM...)   the folder's own assertions, inside the start
  ;;   (:stopped FORM...)   its own assertions after the stop
  ;;   (:refused (PAIR...)...)   one config per section that must signal
  ;;   (:idle CONDITION FORM...)   verbs that, with nothing started, refuse
  ;;       as CONDITION saying the cell is not running
  ;;   (:fixture WRAPPER ARG...)   what every test runs inside, as DEFTEST's
  ;;       fixture: a scratch port, a stubbed seam
  ;; A clause head outside these nine refuses at expansion, as a misspelled keyword does.
  (destructuring-bind (&key config hooks ((:help (&optional topic)))
                       ((:command (&optional command))) running stopped refused idle fixture
                       &aux (key (format nil "nodecode-~a" name))
                       (start (intern (format nil "~:@(~a~)-START" name)))
                       (variable (intern (format nil "*~:@(~a~)*" name)
                                         (format nil "NODECODE-~:@(~a~)" name))))
      (loop for (head . arguments) in clauses nconc (list head arguments))
    (flet ((test-name (suffix) (intern (format nil "~:@(~a-CELL-~a~)" name suffix)))
           (point-text (point)
             (format nil "~s" (if (and (consp point) (eq (first point) 'quote))
                                  (second point)
                                  point)))
           (witness ()
             (cond (hooks `(cell-hook ,key ,(first hooks)))
                   (topic `(assoc ,topic nle::*help-topics*))
                   (command `(nle::find-registered-command ,command))
                   (t variable))))
      `(progn
         (deftest ,(test-name "installs-and-stops") ,fixture
           (with-cell-stop ((,start ,@config))
             (is (functionp stop) "start-cell answers a stop thunk")
             ,@(loop for point in hooks
                     collect `(is (functionp (cell-hook ,key ,point))
                                  ,(format nil "the hook on ~a" (point-text point))))
             ,@(when topic
                 ;; the line rides every request, the text only (help TOPIC)'s answer
                 `((is-present (entry (assoc ,topic nle::*help-topics*))
                     ,(format nil "(help ~s) is on the manual" topic)
                     (let ((prompt (nle::harness-overlay-prompt
                                    "" (nle::read-harness-sections "s1"))))
                       ;; every request carries its line, and never its text
                       (is (search (format nil "  ~(~s~) - ~a" ,topic (second entry)) prompt))
                       (is (and (stringp (cddr entry)) (null (search (cddr entry) prompt))))))))
             ,@(when command
                 `((is-present (entry (nle::find-registered-command ,command))
                     ,(format nil "/~a is registered" command)
                     (is (equal ,key (nle::slash-command-owner entry))))))
             ,@running)
           ,@(loop for point in hooks
                   collect `(is (null (cell-hook ,key ,point))
                                ,(format nil "stop unhooks ~a" (point-text point))))
           ,@(when topic
               `((is (null (assoc ,topic nle::*help-topics*))
                     ,(format nil "stop takes ~s off the manual" topic))))
           ,@(when command
               `((is (null (nle::find-registered-command ,command))
                     ,(format nil "and takes /~a out of the catalog" command))))
           (is (null ,variable) "settings cleared")
           ,@stopped)
         (deftest ,(test-name "start-is-idempotent") ,fixture
           ;; START-CELL can run twice in one image: a reload, or a gateway
           ;; restarted without a fresh process.
           (,start ,@config)
           (with-cell-stop ((,start ,@config))
             ,@(when hooks
                 `((is (= 1 (length (cdr (assoc ,(first hooks) nle:*hooks* :test #'equal)))))))
             ,@(when topic
                 `((is (= 1 (count ,topic nle::*help-topics* :key #'car)) "one topic")))
             (is ,(witness) "and one of everything"))
           (is (null ,(witness)) "so one stop is enough"))
         (deftest ,(test-name "disabled-installs-nothing") ,fixture
           (with-cell-stop ((,start "enabled" nil))
             (is (functionp stop) "a stop thunk even when disabled")
             (is (null ,(witness)) "but nothing installed")
             (is (null ,variable) "and no settings")))
         ,@(when refused
             ;; The gateway's START-CELLS owns the failure-class split: the
             ;; signal becomes one loud warning and the cell absent.
             `((deftest ,(test-name "refused-config-is-a-signal") ,fixture
                 ,@(loop for pairs in refused
                         collect `(is (signals-error nlk:config-refusal (,start ,@pairs))))
                 (is (null ,(witness)) "and nothing was installed")
                 (is (null ,variable) "nor any settings"))))
         ,@(when idle
             `((deftest ,(test-name "verbs-refuse-while-not-running") ,fixture
                 ,@(loop for form in (rest idle)
                         collect `(is (search "not running"
                                              (refusal-text ,(first idle) ,form)))))))))))

(defun cell-publish (owner op &optional (answer t))
  "Run OP through OWNER's :frame hook with a NEXT that answers ANSWER: the
observer funcalled by hand, on this thread."
  (funcall (cell-hook owner :frame) op (lambda (op) (declare (ignore op)) answer)))

(defun cell-frame-op (type &key (session "s1") (turn "t1") payload)
  "A session_stream_event :frame op carrying one durable fact of TYPE in
TURN of SESSION — the exact shape PUBLISH-TURN-FACTS publishes: PAYLOAD (an
empty object when NIL) under the event, the turn id in its metadata."
  (list :session-id session
        :kind "session_stream_event"
        :payload (nlk:json-object "frame" (durable-fact-frame :seq 3 :session-id session
                                                              :turn-id turn :kind type
                                                              :payload payload))
        :cursor 3
        :durable-p t))

(defun cell-json (text)
  "TEXT decoded as shasht decodes a wire body: a fixture literal."
  (shasht:read-json text))

(defun cell-notice (key)
  "The standing notice under KEY, (KEY TEXT LEVEL), or NIL."
  (assoc key (nlk:standing-notices) :test #'string=))

;;; --- a cell's slash command ---------------------------------------------------

(defun cell-entry (owner command args &optional session-id)
  "Run OWNER's registered slash COMMAND with ARGS, as SLASH runs it for every
caller: (values TEXT DIALOG), DIALOG the panel's JSON or NIL."
  (let ((entry (nle::find-registered-command command)))
    (assert (and entry (equal owner (nle::slash-command-owner entry))) ()
            "~a registers no /~a" owner command)
    (funcall (nle::slash-command-headless entry) args session-id)))

;;; --- what a transcript calls a verb ----------------------------------------------

(defmacro is-verb-receipts ((settings &rest calls) &key receipts source groups title
                                                        (stamped "the calls ride the result fact, a refusal included"))
  "Run CALLS inside a tool call with SETTINGS, the cell's settings special,
NIL — so each verb refuses — and assert the calls are stamped on the result
fact as RECEIPTS (STAMPED says so), and that the eval snippet SOURCE, read with
that metadata, groups (GROUPS says why) under TITLE."
  `(let ((nle::*executing-tool-call-id* "c1")
         (nle::*tool-result-metadata* nil)
         (,settings nil))
     ,@(loop for call in calls collect `(ignore-errors ,call))
     (is (equal ',receipts (nle::receipt-calls nle::*tool-result-metadata*)) ,stamped)
     (let ((activity (nle::tool-call-activity "eval" ,source :metadata nle::*tool-result-metadata*)))
       (is (nle::activity-groupable-p activity) ,groups)
       (is (equal ,title (nle::activity-title activity)) (nle::activity-title activity)))))

(defun cell-section (session name)
  "The harness section NAME the hooked chain answers for SESSION, or NIL."
  (cdr (assoc name (nle::read-harness-sections session) :test #'string=)))
