;;;; support.lisp --- the test runner.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Deliberately dependency-free. Every later slice's tests register here, and
;;;; RUN-ALL is what nodecode.asd's test-op invokes once this file exists.
;;;; No test framework is pulled in: the organism forks and snapshots itself, and
;;;; a runner that holds no foreign state and no threads is one less thing to
;;;; reason about at those boundaries.

(in-package #:nodecode.test)

(nlk:access (block nle::display-block) (event nlk::event) (model nle::chat-model))

(defvar *tests* '()
  "Registered tests, in definition order.")

(defvar *failures* nil
  "Failure descriptions accumulated by the test currently running.")

(defmacro with-saved-globals ((&rest places) &body body)
  "Run BODY with every place in PLACES saved on entry and SETF back on unwind,
restored in reverse order. A place written (PLACE VALUE) is also SETF to VALUE
before BODY runs — the entry half a call site used to write out by hand."
  ;; SETF and not LET, and that is the whole reason this exists. These places
  ;; are read off other threads — the gateway's connection and worker threads,
  ;; the frame clock — and an SBCL special bound with LET is thread-local: the
  ;; binding is invisible to every thread but this one, so a LET changes
  ;; nothing the code under test can see while looking exactly as though it
  ;; did. The price of mutating instead is that the restore is not automatic,
  ;; which is why it lives in one UNWIND-PROTECT here rather than at every
  ;; call site. The (PLACE VALUE) shape mutates through that same door, so the
  ;; entry SETF and its restore can no longer drift apart at a call site; a
  ;; bare symbol saves and restores without setting, exactly as before.
  (let* ((names (mapcar (lambda (p) (if (consp p) (first p) p)) places))
         (saved (loop for name in names collect (gensym "SAVED"))))
    `(let ,(loop for name in names for temp in saved collect `(,temp ,name))
       (nlk:with-cleanup ((setf ,@(mapcan #'list (reverse names) (reverse saved))))
         ,@(loop for p in places
                 when (consp p) collect `(setf ,(first p) ,(second p)))
         ,@body))))

(defmacro with-floor-restored (&body body)
  "Run BODY and put the kernel's package floor back however it was found."
  ;; The floor is image state, not a test's. A gate that lowers it to let an
  ;; eval through must raise it again for every gate after it, and one that
  ;; raises it must not leave it raised for a gate that assumed it down —
  ;; which is why the restore names the found state instead of one direction.
  `(let ((was nodecode.kernel::*floor-locked*))
     (unwind-protect (progn ,@body)
       (if was (nodecode.kernel:lock-floor) (nodecode.kernel:unlock-floor)))))

(defun call-with-hermetic-machine-state (thunk)
  "Run THUNK with this host's machine state out of reach, both directions:
host files and caches must not skew assertions, and no test may write an
operator file."
  ;; This is the posture RUN-ALL once installed for the whole suite; it lives
  ;; here so a single test funcalled outside RUN-ALL carries it too — a subset
  ;; run without it once rewrote the real ~/.nodecode/config.jsonc through a
  ;; /models test's persist path.
  ;;
  ;; The color profile is pinned to :none so display assertions see byte-stable
  ;; plain text regardless of the invoking terminal's TERM/COLORTERM. The
  ;; SETF'd variables are read by gateway worker threads, which never see a
  ;; test-thread LET binding; WITH-SAVED-GLOBALS restores them on exit so a
  ;; standalone run leaves the image as it found it. Tests that cover captures or
  ;; the configured-provider tier install their own fixture inside this extent
  ;; (WITH-WORKSPACE-CAPTURES; LET for main-thread reads, SETF for route tests).
  (let ((nodecode.evolved:*color-profile* :none)
        ;; Fetched provider listings are machine state too — and NIL is
        ;; also what keeps a /models test from spawning a fetch thread.
        (nodecode.evolved::*provider-models-cache-path* nil)
        (nodecode.evolved::*context-tokens-env-override* nil)
        ;; The frame guard signals instead of repairing: every gate that
        ;; composes a frame asserts the display authority's contract (every
        ;; row exactly the terminal width, no ESC the shell did not write).
        (nodecode.evolved::*frame-guard-strict* t)
        ;; Composer prompt history is machine state, same as the catalog
        ;; cache: NIL disables persistence; tests that cover it bind their
        ;; own temp path.
        (nodecode.evolved::*composer-history-path* nil)
        ;; Theme selection persistence is machine state too.
        (nodecode.evolved::*theme-selection-path* nil)
        ;; Keybind overrides are machine state: the suite asserts the
        ;; declarative defaults; override tests bind their own document.
        (nodecode.evolved::*keybinds-path* nil)
        ;; A shell's copy of the gateway's cell commands is runtime state:
        ;; the suite asserts the builtin catalog; a test that reads the
        ;; gateway's commands sets its own entries inside this binding.
        (nodecode.evolved::*gateway-commands* '())
        ;; Highlight delivery pacing is live-UX state: tests flush by hand
        ;; and assert delivery, not the rest window.
        (nodecode.evolved::*picker-highlight-rest* 0)
        ;; The home (paths.lisp): a scratch folder of this test's own —
        ;; deleted on unwind when a test wrote there.
        (scratch-home (temp-path "home")))
    (with-saved-globals
        (;; Boundary captures off: a test driving RUN with a default-cwd
         ;; session must never ADD -A the live repo into a shadow store.
         (nodecode.kernel::*workspace-capture-enabled* nil)
         ;; Around-hooks are runtime state: a test that registers one must
         ;; not guard every test after it. SETF, not LET: the :FRAME chain
         ;; runs on the turn worker that publishes, and a LET here would hide
         ;; every hook a test registers from exactly that thread.
         (nodecode.evolved:*hooks* '())
         ;; The snippets each session watches are runtime state too: a test's
         ;; read snippet must not run again under the next test's acts.
         (nodecode.evolved::*watches* (make-hash-table :test #'equal :synchronized t))
         ;; The standing notice board is runtime state a test's cell or
         ;; channel posts to; the next test starts from an empty board --
         ;; and the notice log beside it, which every notice rides into.
         (nodecode.kernel::*notices* '())
         (nodecode.kernel::*notice-log* '())
         ;; The palette and the canvas the terminal answered: a shell's entry
         ;; engages the configured theme, and a picker previews one, on
         ;; whatever thread runs them -- left behind, every later frame in the
         ;; run paints in that theme (a transparent one paints no canvas).
         nodecode.evolved:*palette*
         nodecode.evolved::*terminal-canvas*
         ;; The shared operator config (~/.nodecode/config.jsonc): NIL keeps
         ;; this host's providers/defaults out of lane resolution, /models
         ;; rows, and boot seeding — and keeps every persist path away from
         ;; the operator's real file.
         (nodecode.evolved::*shared-config-path* nil)
         ;; The default model and effort: a pick moves them
         ;; (SELECT-DEFAULT-MODEL, SELECT-DEFAULT-EFFORT SETF them on the
         ;; gateway's thread), and one test's pick must not be the next
         ;; test's default.
         nodecode.evolved::*provider* nodecode.evolved::*model* nodecode.evolved::*endpoint*
         nodecode.evolved::*reasoning-effort*
         ;; The models.dev disk cache is machine state, not fixture state:
         ;; capability tests bind their own catalog path, everything else runs
         ;; against the flat defaults regardless of what this host has cached.
         ;; SETF, not LET: a gateway worker reads it too, and a refusal it
         ;; words must be the one the test thread words.
         (nodecode.evolved::*models-catalog-path* nil)
         ;; The models.dev fetch: NIL keeps every catalog read off the
         ;; network. SETF, not LET: a gateway worker would refetch a stale
         ;; cache. The refresh gates bind their own.
         (nodecode.evolved::*models-catalog-url* nil)
         ;; The cell folder (~/.nodecode/cells/): NIL means the eval seam
         ;; records nothing and the loader loads nothing. Every defining form
         ;; a test evals would otherwise land as a file and a commit in the
         ;; operator's real layer.
         ;; SETF: the turn worker evals on its own thread. Scribe tests bind
         ;; their own fixture folder inside this extent.
         (nodecode.kernel::*cells-directory* nil)
         ;; A warm image's folder check (BOOT-CELLS' no-receipt path) and its
         ;; answer: a check one test left behind is one the next boot awaits,
         ;; and its failure hands the whole process to a fresh binary
         ;; (HAND-BACK-TO-ORIGIN) -- the suite ends there, half run.
         (nodecode.kernel::*warm-verification* nil)
         (nodecode.kernel::*warm-verification-failure* nil)
         ;; The cell RECORDS, not just the folder they came from. LOAD-CELLS
         ;; and RESTART-CELLS SETF this globally, so a fixture's folders
         ;; outlived the test that made them and every later test that reads
         ;; (NLK:CELLS) saw them — a slash gate asserting an empty folder
         ;; failed or passed on which test ran before it, and it took a
         ;; pristine baseline to tell that apart from a real break. Restoring
         ;; it makes the folder a test's own, the way the directory already is.
         (nodecode.kernel::*cells* '())
         ;; The layer cache (~/.cache/nodecode/layers/): NIL loads every
         ;; layer fixture from source, and keeps a fixture's compiled files
         ;; out of the operator's cache. The one test that covers the cache
         ;; binds its own directory inside this extent.
         (nodecode.kernel::*layer-cache-directory* nil)
         ;; The operator's peripheral cache, the same way: NIL leaves a
         ;; fixture's compile to ASDF's user cache.
         (nodecode.kernel::*peripheral-cache-directory* nil)
         ;; No hub: /setup's Choose offers the shipped folders alone rather
         ;; than reading the index off the network.
         (nodecode.kernel::*hub-index-url* nil)
         (nodecode.kernel::*community-index-url* nil)
         ;; The warm-image launch cache: NIL keeps every test's boot on the
         ;; direct path and every fixture folder out of the operator's
         ;; cache; the warm tests bind their own directory inside this.
         (nodecode.kernel::*warm-cache-directory* nil)
         ;; What this process learned of each saved key's check: a key a
         ;; test left unanswered must not read `not checked' in the next.
         (nodecode.evolved::*key-checks* (make-hash-table :test #'equalp))
         ;; The worlds the first frame probes for a past to bring over, and
         ;; the marker an import leaves: NIL keeps the setup wizard's unasked
         ;; offer off the operator's real homes. The import stage's tests
         ;; bind their own fixture worlds inside this extent.
         (nodecode.evolved::*import-worlds* nil)
         (nodecode.evolved::*import-marker* nil)
         ;; A session's first prompt names it in a side call (NAME-SESSION):
         ;; NIL keeps every test's submit off the model and its sessions on
         ;; their prompts' lines. The naming gates bind :INLINE inside this.
         (nodecode.evolved::*session-naming* nil)
         ;; The release feed: no test opens a socket to a release host, and
         ;; a refusal with no reason is the silent one -- a gateway boot
         ;; under this posture starts a reader that says nothing. The
         ;; reader's own tests bind a stub of their own.
         (nodecode.evolved::*feed-open* (constantly nil))
         ;; Nor asks one for anything, a release's change list among it: a
         ;; gateway's /settings read hears that the host lists nothing. A
         ;; release host's own tests serve a folder the real fetch reads
         ;; (WITH-RELEASE-HOST) or bind a fetch of their own, and start from
         ;; no list fetched.
         (nodecode.evolved::*release-fetch* (lambda (url) (values nil (format nil "no release host in a test: ~a" url))))
         (nodecode.evolved::*release-list* nil)
         ;; Which release's changes the operator last opened is machine
         ;; state, as the theme selection is.
         (nodecode.evolved::*changes-seen-path* nil)
         ;; The home (paths.lisp): a scratch folder of this test's own, so
         ;; every path read through (nlk:home ...) at call time — the
         ;; operator token, SOUL.md, the memory and skills folders, a
         ;; profile's files — lands nowhere near the operator's; and the
         ;; three startup parameters no posture covered before follow it,
         ;; so a subset run can never own the real store or read the real
         ;; credentials by default.
         (nodecode.kernel::*home* scratch-home)
         (nodecode.kernel::*default-store-path* (merge-pathnames "history.db" scratch-home))
         (nodecode.evolved::*auth-file-path* (merge-pathnames "auth.json" scratch-home))
         (nodecode.evolved::*secrets-directory* (merge-pathnames "secrets/" scratch-home))
         ;; The profile ladder's two files and the wrapper folder follow the
         ;; scratch home too: a profile test creates and deletes homes under
         ;; it, never under ~/.nodecode/profiles/, and writes no command
         ;; into ~/.local/bin/.
         (nodecode.kernel::*profiles-directory* (merge-pathnames "profiles/" scratch-home))
         (nodecode.kernel::*active-profile-path* (merge-pathnames "active-profile" scratch-home))
         (nodecode.kernel::*wrapper-directory* nil)
         ;; The gateway's unit folder: NIL is a machine with no background
         ;; gateway, so no test writes a unit where the operator's systemd
         ;; reads, and no cell's start turns one on. The gateway gates
         ;; bind a scratch folder and script the runner inside this extent.
         (nodecode.kernel::*unit-directory* nil)
         (nodecode.kernel::*unit-runner* 'nodecode.kernel::run-unit-program)
         (nodecode.kernel::*shipped-launcher* nil)
         ;; Which service manager the gateway speaks to stays
         ;; the host's; a gate that scripts the other arm
         ;; SETFs it, and it comes back.
         nodecode.kernel::*service-manager*)
      (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree scratch-home :validate t)))
        (funcall thunk)))))

(defmacro deftest (name fixture &body body)
  "Register a test: NAME reports it, BODY runs inside FIXTURE's form."
  ;; FIXTURE is () or the opening of the form BODY runs inside —
  ;; (with-temp-store (store)), (let ((a 1))) — spliced back as
  ;; (WRAPPER ARGS... BODY...), so a test reads as its name, its fixture and
  ;; its assertions. The body runs under CALL-WITH-HERMETIC-MACHINE-STATE
  ;; however the test is invoked — RUN-ALL or a direct funcall — so machine
  ;; state cannot leak in either direction.
  `(progn
     (setf *tests* (append (remove ',name *tests* :key #'car)
                           (list (cons ',name
                                       (lambda ()
                                         (call-with-hermetic-machine-state
                                          (lambda ()
                                            ,@(if fixture `((,@fixture ,@body)) body))))))))
     ',name))

(defmacro is (form &optional description &aux (value (gensym "VALUE")))
  "Assert FORM is true."
  ;; Records a failure rather than unwinding, so one test can report several
  ;; problems in a single run.
  `(let ((,value (handler-case ,form
                   (error (e) (push (format nil "~a signalled ~a"
                                            ,(or description (format nil "~s" form)) e)
                                    *failures*)
                     :error))))
     (unless ,value
       (push ,(or description (format nil "~s was false" form)) *failures*))
     ,value))

(defmacro is-present (binding description &body assertions)
  "Assert a value is there under DESCRIPTION, then run ASSERTIONS only when it
is."
  ;; BINDING is either (VAR FORM) — bind VAR to FORM for both — or a bare VAR
  ;; the caller already bound.
  ;;
  ;; The guard is what this owns. IS records a failure rather than unwinding,
  ;; so every assertion over an optional projection has to stop itself from
  ;; reading through a missing value, and written by hand that is a LET, an IS
  ;; and a WHEN at every site. It is deliberately not (WHEN (IS ...) ...): IS
  ;; answers the keyword :ERROR when its form signals, :ERROR is true, and
  ;; that guard would then run the assertions in exactly the case the value is
  ;; absent. The value is bound through IS instead and the guard tests the
  ;; binding, so a projection that signals lands as one recorded failure under
  ;; DESCRIPTION — not as an unhandled error that takes the rest of the test's
  ;; assertions down with it.
  (let ((variable (if (consp binding) (first binding) binding)))
    `(let ((,variable (is ,(if (consp binding) (second binding) variable) ,description)))
       (when (and ,variable (not (eq ,variable :error)))
         ,@assertions))))

(defmacro signals-error (condition-type &body body &aux (result (gensym "RESULT")))
  "Assert BODY signals CONDITION-TYPE, and answer the condition that was caught."
  ;; The answer is the condition object itself: a test that asserts on the
  ;; condition's slots reads them off this value instead of hand-writing its
  ;; own HANDLER-CASE, and a test that wants only the fact of the refusal
  ;; keeps using it in boolean position, since a condition is true. Both
  ;; failure paths — nothing signalled, and something else signalled — record
  ;; and answer NIL.
  `(let ((,result
           (handler-case (progn ,@body nil)
             (,condition-type (caught) caught)
             (error (e) (push (format nil "expected ~a, got ~a" ',condition-type e) *failures*)
               :wrong))))
     (when (null ,result)
       (push (format nil "expected ~a, nothing signalled" ',condition-type) *failures*))
     (unless (eq ,result :wrong) ,result)))

(defmacro refusal-text (condition-type &body body)
  "The text of the CONDITION-TYPE that BODY signals."
  `(princ-to-string (signals-error ,condition-type ,@body)))

(defun run-tests (entries &aux (failed '()))
  "Run each of ENTRIES, (NAME . THUNK) as *TESTS* holds them, under its own
*FAILURES*: an unhandled error is recorded rather than thrown, and each test
prints a pass or FAIL line. Answers the names that failed, in order."
  (dolist (entry entries (reverse failed))
    (let ((*failures* '()))
      (handler-case (funcall (cdr entry))
        (error (e) (push (format nil "unhandled ~a" e) *failures*)))
      (cond (*failures*
             (push (car entry) failed)
             (format t "~&FAIL ~a~%" (car entry))
             (dolist (failure (reverse *failures*))
               (format t "       ~a~%" failure)))
            (t (format t "~&pass ~a~%" (car entry)))))))

(defun run-all ()
  "Run every registered test."
  ;; Returns T when all passed; signals an error otherwise so ASDF's test-op
  ;; fails loudly rather than reporting success.
  ;;
  ;; Refuses to run in an image with an open store: the suite is destructive
  ;; to its host — the evolve tests rebuild the evolved package — so inside a
  ;; serving image it takes the session down with it. Same posture as
  ;; STORE-ALREADY-OPEN, one seam earlier.
  ;;
  ;; Machine-state neutralization is not installed here: every registered test
  ;; carries CALL-WITH-HERMETIC-MACHINE-STATE from its DEFTEST expansion, so
  ;; the posture holds for subset and standalone runs exactly as it does for
  ;; this loop.
  (run-suite *tests* "run-all refuses to run in a live image: store ~a is open — the ~
                      suite rewrites global config state and rebuilds the evolved ~
                      package; run it in a scratch image (just test)"))

(defun run-shard (index count)
  "Run shard INDEX (from 0) of COUNT: every registered test whose place in the
suite is INDEX modulo COUNT. COUNT processes, one per shard, run the whole
suite between them; the split is fixed by the suite's order, so a red shard
reruns alone with the same two numbers."
  (check-type count (integer 1))
  (check-type index (integer 0))
  (assert (< index count) (index count) "shard ~d is past the last of ~d" index count)
  (run-suite (loop for entry in *tests*
                   for place from 0
                   when (= (mod place count) index)
                     collect entry)
             "run-shard refuses to run in a live image: store ~a is open — the ~
              suite rewrites global config state and rebuilds the evolved ~
              package; run it in a scratch image (just test)"
             (format nil "shard ~d/~d" (1+ index) count)))

(defun run-suite (entries refusal &optional name)
  "Run ENTRIES as one suite: refused while a store is open (REFUSAL, a format
control over its path), then the count line and an error naming every
failure, each under NAME when the suite is a cell's slice. Answers T."
  (when (nlk:store-open-p)
    (error refusal (nlk:store-path nlk::*store*)))
  (let ((failed (run-tests entries)))
    (format t "~&~%~d ~@[~a ~]test~p, ~d failed~%"
            (length entries) name (length entries) (length failed))
    (finish-output)
    (when failed
      (error "~a test failures: ~{~a~^ ~}" (or name "nodecode") failed))
    t))

;;; --- the stubbing seam ---------------------------------------------------
;;; A test that wants a scripted function replaces one fdefinition and must put
;;; it back — a restore that is missed, or skipped by a non-local exit, poisons
;;; every later test in the run with a stale stub. That save/restore was written
;;; out by hand in six files; it lives here once instead, as one primitive and
;;; the provider-shaped specialisation the loop tests reach for.

(defmacro with-stubbed-fdefinition ((name lambda-list &body stub) &body body)
  "Run BODY with NAME's function definition replaced by STUB over LAMBDA-LIST,
restoring the real definition however BODY leaves — a returned value, a
non-local exit, or an unhandled signal."
  ;; ORIGINAL is bound inside STUB to the definition that was displaced, so a
  ;; pass-through stub can observe an argument and still finish the real call:
  ;;
  ;;   (with-stubbed-fdefinition (nlk:page-events (&rest args)
  ;;                              (push (getf args :after) pages)
  ;;                              (apply original args))
  ;;     ...)
  ;;
  ;; Every parameter is declared IGNORABLE, because most stubs answer the same way
  ;; whatever they are asked and a hand-written IGNORE at the call site is one more
  ;; thing to keep in step with the real lambda list. The assignments run inside
  ;; SB-EXT:WITHOUT-PACKAGE-LOCKS: some targets are exported kernel symbols, where
  ;; the SETF is a lock violation, and the wrapper costs nothing everywhere else.
  ;; The saved definition is the one under a hook's encapsulation
  ;; (NLK::UNADVISED-FUNCTION): FDEFINITION answers the encapsulation itself,
  ;; whose definition the stub's SETF replaced, so restoring it put the stub
  ;; back for the rest of the run.
  (let ((saved (gensym "SAVED"))
        (variables (loop for item in (set-difference lambda-list lambda-list-keywords)
                         for (name nil supplied) = (alexandria:ensure-list item)
                         collect (if (consp name) (second name) name)
                         when supplied collect it)))
    `(let ((,saved (nlk::unadvised-function ',name)))
       (nlk:with-cleanup ((sb-ext:without-package-locks
                            (setf (fdefinition ',name) ,saved)))
         (sb-ext:without-package-locks
           (setf (fdefinition ',name)
                 (lambda ,lambda-list
                   (declare (ignorable ,@variables))
                   (let ((original ,saved))
                     (declare (ignorable original))
                     ,@stub))))
         ,@body))))

(defmacro with-stubbed-fdefinitions ((&rest stubs) &body body)
  "Nest WITH-STUBBED-FDEFINITION over STUBS — each one a (NAME LAMBDA-LIST
. STUB) spec — installed left to right and restored right to left:

  (with-stubbed-fdefinitions ((nle::send-subscribe (ws ids))
                              (nle::send-command (ws command) :sent))
    ...)

A test that scripts two functions at once says so in one form, and the nesting
order is the restore order whatever happens in between."
  (reduce (lambda (stub inner) `(with-stubbed-fdefinition ,stub ,inner)) stubs
          :from-end t :initial-value `(progn ,@body)))

(defun local-noon-unix ()
  "Noon today on this box's own clock, as Unix seconds."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (- (encode-universal-time 0 0 12 date month year) nlk:+unix-epoch+)))

(defmacro with-clock-at-local-noon (&body body)
  "Run BODY with the wall clock (NLK:UNIX-NOW and NLK::ISO-NOW) stopped at
noon today on this box's own clock."
  ;; A fixture dated minutes or hours before now -- a session 10m old -- falls
  ;; on yesterday when the suite runs just after local midnight, and /sessions
  ;; then files it under Yesterday and says its clock time where `10m ago'
  ;; stood. Under this clock every such fixture lands on today, whatever hour
  ;; the suite runs at and in whatever zone.
  (alexandria:with-gensyms (noon)
    `(let ((,noon (local-noon-unix)))
       (with-stubbed-fdefinitions ((nlk:unix-now () ,noon)
                                   (nlk::iso-now () (nlk:iso-time ,noon)))
         ,@body))))

(defmacro with-provider-stub ((context &body stub) &body body)
  "Run BODY with CALL-PROVIDER answering STUB, restoring the real definition on
unwind however BODY leaves."
  ;; CONTEXT names the compiled turn context the loop hands the provider. NIL
  ;; — the usual case, since most stubs answer the same way whatever they are
  ;; asked — binds a gensym instead, and WITH-STUBBED-FDEFINITION's IGNORABLE
  ;; covers it:
  ;;
  ;;   (with-provider-stub (nil (incf calls) (wire-message "assistant" :content "ok"))
  ;;     (nle:run "hello" :session session))
  ;;
  ;; The stub is given CALL-PROVIDER's whole lambda list, ON-PART included.
  ;; That is the point of owning it here: a hand-written (LAMBDA (CONTEXT))
  ;; stub is a program error on every loop path that streams, and it fails as
  ;; a wrong-argument error inside the turn rather than as a visible test
  ;; failure.
  `(with-entry-stub (nodecode.evolved::call-provider ,context nil ,@stub) ,@body))

(defmacro with-entry-stub ((name context on-part &body stub) &body body)
  "Run BODY with NAME, a provider entry over (CONTEXT &KEY ON-PART), answering
STUB; NIL for CONTEXT or ON-PART binds a gensym the IGNORABLE covers."
  ;; The keyword is spelled explicitly because the variable may be a gensym:
  ;; a bare `&key G123' would name the keyword :G123 and refuse the loop's
  ;; :ON-PART.
  `(with-stubbed-fdefinition
       (,name (,(or context (gensym)) &key ((:on-part ,(or on-part (gensym))) nil))
        ,@stub)
     ,@body))

(defmacro with-stub-provider ((&rest replies) &body body &aux (queue (gensym "QUEUE")))
  "Run BODY with CALL-PROVIDER answering REPLIES in order, then erroring if the
loop asks for more than were scripted."
  ;; The scripted-queue flavour of WITH-PROVIDER-STUB, which owns the restore
  ;; for both.
  `(let ((,queue (list ,@replies)))
     (with-provider-stub (nil (if ,queue
                                  (pop ,queue)
                                  (error "stub provider: loop requested more rounds than scripted")))
       ,@body)))

;;; --- the process-global seam ---------------------------------------------

(defmacro with-workspace-captures ((value) &body body)
  "SETF the process-global capture kill switch to VALUE around BODY, restoring
the prior value on unwind."
  ;; The switch is a DEFGLOBAL: a LET binding would be thread-local and
  ;; invisible to gateway worker threads, so mutation is the only honest way
  ;; to flip it in a test — WITH-SAVED-GLOBALS owns that restore.
  `(with-saved-globals ((nodecode.kernel::*workspace-capture-enabled* ,value))
     ,@body))

(defvar *fixture-workspace* nil
  "Lazily created per-process temp directory used as fixture-session cwd.")

(defun fixture-workspace ()
  "A per-process temp directory for fixture sessions' cwd."
  ;; Defense in depth: even if the capture kill switch is ever armed by
  ;; mistake, a capture against a fixture session touches this scratch
  ;; directory, never the live repo.
  (or *fixture-workspace*
      (setf *fixture-workspace*
            (ensure-directories-exist (format nil "/tmp/nl-fixture-~a/" (sb-posix:getpid))))))

(defun temp-path (tag &optional type)
  "A fresh path under the system temp directory, unique to this process and
this call."
  ;; TYPE is the file extension; NIL names a directory instead, with the
  ;; trailing slash a pathname needs to merge relative names against it.
  ;; Nothing is created here — WITH-TEMP-FILE writes the file,
  ;; WITH-TEMP-WORKSPACE makes the directory, and the tests that cover an
  ;; absent config want the path to name nothing at all.
  (let ((name (format nil "nl-~a-~a-~36r" tag (sb-posix:getpid) (random (expt 36 8)))))
    (merge-pathnames (if type
                         (format nil "~a.~a" name type)
                         (format nil "~a/" name))
                     (uiop:temporary-directory))))

(defun write-temp-file (path &optional (contents ""))
  "Create PATH, and any parent directory it needs, holding CONTENTS."
  ;; The contentless call is the `touch' a workspace fixture wants: the walks
  ;; under test key on the name existing, not on what is in it.
  (nlk::with-output-file (out path) (write-string contents out))
  path)

(defmacro with-temp-file ((place &key contents (type "json") setf) &body body
                          &aux (path (gensym "PATH")) (text (gensym "CONTENTS")))
  "Run BODY with PLACE naming a fresh temp file of TYPE, deleted on unwind."
  ;; CONTENTS, when given, is written before BODY runs; without it the path
  ;; names a file that does not exist, which is what a test covering an absent
  ;; config asserts against.
  ;;
  ;; SETF non-NIL mutates PLACE instead of LET-binding it, and the choice
  ;; belongs to the call site because it is load-bearing: a special that
  ;; gateway worker threads read — *SHARED-CONFIG-PATH* is the one — must be
  ;; SETF, since a LET binding is thread-local and those threads would go on
  ;; reading the operator's real config. Everything only this thread reads
  ;; takes the LET, which restores itself even when the cleanup is skipped.
  ;;
  ;; PLACE is bound to a NAMESTRING, not a pathname. That is a contract, not a
  ;; convenience: MODELS-CATALOG-TABLE gates its read on (STRINGP
  ;; *MODELS-CATALOG-PATH*), so a pathname here reads as "no catalog configured"
  ;; and the fixture silently tests nothing.
  `(let* ((,path (namestring (temp-path "file" ,type)))
          (,text ,contents))
     (nlk:with-cleanup ((ignore-errors (delete-file ,path)))
       (when ,text (write-temp-file ,path ,text))
       (,(if setf 'with-saved-globals 'let) ((,place ,path))
        ,@body))))

(defmacro await ((&key (tries 100) (rest 0.1) timeout) &body test)
  "Evaluate TEST until it answers non-NIL or TRIES looks (TIMEOUT seconds' worth
when given) are up, resting REST seconds between; answers what the last look gave."
  ;; The hand-rolled shape this replaces spelled one wait three times over: a
  ;; NIL variable, a LOOP that SETFs it, and a test on the variable
  ;; afterwards, with the value outliving the wait that produced it. TIMEOUT
  ;; is how a cell's cross-thread assertions say it.
  `(loop repeat ,(if timeout `(ceiling ,timeout ,rest) tries)
         thereis (progn ,@test)
         do (sleep ,rest)))

(defmacro with-temp-trees ((&rest specs) &body body)
  "Run BODY with each (VAR TAG) of SPECS bound to a fresh scratch directory
path tagged TAG, removing every tree on the way out however BODY leaves,
last-named first."
  ;; Nothing is created here, exactly as in TEMP-PATH: the fixture writes into
  ;; the path, and what is under test decides whether the directory has to
  ;; exist beforehand. The unwind is the whole point — twenty fixtures each
  ;; restated it, and a restated cleanup is the shape that quietly stops
  ;; reversing. WITH-TEMP-DIRECTORY is the one-tree, 0700, created door;
  ;; this is the variadic one for fixtures that stage two or three trees.
  `(let ,(loop for (var tag) in specs collect `(,var (temp-path ,tag)))
     (unwind-protect (progn ,@body)
       ,@(loop for (var) in (reverse specs)
               collect `(ignore-errors (uiop:delete-directory-tree ,var :validate t))))))

(defmacro with-temp-workspace ((dir &rest files) &body body)
  "Run BODY with DIR bound to a fresh temp directory holding FILES, deleting
the tree on unwind."
  ;; Each FILES entry is a workspace-relative name; its parent directories are
  ;; made and the file is created empty, because the catalogs and walks under
  ;; test key on names. `(with-temp-workspace (dir "README.md" "src/main.lisp"
  ;; "node_modules/x.js") ...)' is the whole fixture, nesting included.
  `(with-temp-trees ((,dir "workspace"))
     (ensure-directories-exist ,dir)
     ,@(loop for file in files
             collect `(write-temp-file (merge-pathnames ,file ,dir)))
     ,@body))

(defun ensure-durable-session (session-id)
  "Issue CREATE-SESSION when the store is open and SESSION-ID has no fact."
  ;; Fixture sessions always get a scratch cwd — never the live repo.
  (when (and (nlk:store-open-p) (not (nlk:session-exists-p session-id)))
    (nlk:create-session :id session-id :cwd (fixture-workspace)))
  session-id)

(defun intern-durable-session (session-id)
  "Create SESSION-ID if needed, then intern the one live heap object."
  (nle:intern-session (ensure-durable-session session-id)))

(defun wire-fold (session-id)
  "SESSION-ID's visible log folded into wire messages, oldest first: the fold
hydration runs, read whole and uncached."
  (values (nle::wire-messages-from-events
           (nle::collect-visible-session-events session-id))))

(defun record-tool-use (turn call-id tool arguments result &rest result-keys)
  "Journal on TURN one call of TOOL as CALL-ID with ARGUMENTS and its RESULT,
RESULT-KEYS reaching the result fact."
  (nlk:record-tool-call turn call-id tool arguments)
  (apply #'nlk:record-tool-result turn call-id tool result result-keys))

(defun seed-completed-turn (session-id input assistant
                            &key command-id tools)
  "Admit INPUT on SESSION-ID, journal TOOLS, and complete with ASSISTANT."
  ;; TOOLS is a list of (call-id tool-name arguments result).
  (ensure-durable-session session-id)
  (let ((turn (nlk:admit-turn session-id
                              (or command-id (nlk::make-durable-id "command"))
                              input)))
    (dolist (spec tools)
      (destructuring-bind (call-id tool-name arguments result) spec
        (record-tool-use turn call-id tool-name arguments result)))
    (nlk:record-assistant-message
     turn (nlk:json-object "role" "assistant" "content" assistant))
    (nlk:complete-turn turn nil)
    turn))

;;; --- knowledge: a fixture folder and a keep the eval seam's way ---------------

(defmacro with-knowledge-fixture ((&key (layer "tester")) &body body &aux (root (gensym "ROOT")))
  "Run BODY over a fresh cell folder: the layer named LAYER, its knowledge cell
LAYER-memory beside it once something is kept, a fixture shipped source, and
the scan's memo dropped on both sides."
  ;; SETF, not LET, as the scribe fixture: a turn worker writes the same folder.
  `(with-temp-directory (,root "knowledge")
     (with-saved-globals ((nlk::*cells-directory* (merge-pathnames "cells/" ,root))
                          (nlk::*layer-name* ,layer)
                          (nlk::*layer-revision* nil)
                          (nlk::*source-root* (merge-pathnames "src/" ,root)))
       (ensure-directories-exist nlk::*source-root*)
       (nlk:forget-shipped-definitions)
       (clrhash nlk::*knowledge-memo*)
       (nlk:with-cleanup ((nlk:forget-shipped-definitions) (clrhash nlk::*knowledge-memo*))
         ,@body))))

(defun kept-text (name &optional (cell "tester-memory"))
  "The text of NAME.lisp in the fixture's CELL, or NIL."
  (nlk:read-text (merge-pathnames (format nil "~a/~a.lisp" cell name) nlk::*cells-directory*)))

(defun keep-definition-text (text &key session)
  "Evaluate TEXT, one toplevel form, the way the eval seam does -- read in
EVOLVED, evaluated, recorded with its own bytes -- under SESSION. => what the
scribe printed."
  (let* ((*package* (find-package '#:nodecode.evolved))
         (form (let ((*read-eval* nil)) (read-from-string text))))
    (with-output-to-string (*standard-output*)
      (eval form)
      (nlk:record-definitions form text (length text) :session-id session))))

(defun seed-user-inputs (session-id texts)
  "Seed completed input-only turns for history projection tests."
  (ensure-durable-session session-id)
  (dolist (text texts)
    (nlk:complete-turn (nlk:admit-turn session-id (nlk::make-durable-id "command") text) nil))
  session-id)

(defun seed-unasked-turns (session-id)
  "SESSION-ID's chain, in order: an ask; the recap a reflection recorded behind
it; a second ask; a pair an import recorded, which is an ask too; and a turn an
exit woke, which ran a model round of its own."
  (seed-completed-turn session-id "first ask" "first answer")
  (nlk:record-exchange-turn session-id "[experience] recap of the turn above, by a reflection of this session"
                            "Recap: nothing to carry forward." :facts (list :recorder "reflection"))
  (seed-completed-turn session-id "second ask" "second answer")
  (nlk:record-exchange-turn session-id "imported ask" "imported answer" :facts (list :recorder "claude-code"))
  (let ((wake (nlk:admit-wake-turn session-id (nlk::make-durable-id "command")
                                   :wake (vector (nlk:json-object "eval_id" 1003 "exit_code" 0 "shell_exit" 2)))))
    (nlk:record-turn-usage wake :provider "vendor" :model "m1" :input-tokens 900 :output-tokens 40)
    (nlk:record-assistant-message wake (nlk:json-object "role" "assistant" "content" "The build failed."))
    (nlk:complete-turn wake nil))
  session-id)

;;; --- reading the log back -------------------------------------------------
;;; The seeds above have one reader shape between them: pull SESSION-ID's
;;; events out of the log and narrow them. Narrowing is by wire kind (the
;;; string on the fact) or by event class (the CLOS type the kind materializes
;;; as), and the two are never mixed at one site, so one keying serves both.

(defun session-events (session-id &key kind type (created t))
  "SESSION-ID's events, oldest first, narrowed to wire KIND or to event class
TYPE."
  ;; :CREATED NIL drops the session.created fact every fixture starts with —
  ;; what a test asserting on a turn's own events wants, and the reason the
  ;; whole list is rarely the right answer.
  (loop for event in (nlk:page-events :session-id session-id)
        when (and (or (null kind) (equal kind event.kind))
                  (or (null type) (typep event type))
                  (or created (not (equal "session.created" event.kind))))
          collect event))

(defun session-event (session-id &key kind type)
  "The oldest of SESSION-ID's events matching KIND or TYPE, or NIL."
  ;; The singular read: a test that names one fact asserts on the fact, not on
  ;; a list of one.
  (first (session-events session-id :kind kind :type type)))

(defun count-session-events (session-id kind)
  "How many of SESSION-ID's events carry wire KIND — the idempotence read,
where the count is the claim and the events themselves are not."
  (length (session-events session-id :kind kind)))

(defun event-kinds-for (session-id)
  "The wire kinds of SESSION-ID's events, in log order."
  ;; The ordering read: an assertion that names the sequence of facts a lane
  ;; must have written.
  (mapcar #'nlk:event-kind (session-events session-id)))

(defun fold-state-to-watermark (session-id watermark)
  "SESSION-ID's KV fold rewound to WATERMARK, in a transaction of its own."
  (nlk::with-store-transaction (handle)
    (nlk::fold-session-state-to-watermark-in-transaction handle session-id watermark)))

;;; --- shared fixtures ----------------------------------------------------

(defun test-provider-config (&key (provider "openai-completions")
                                  (model "test-model"))
  "The flat provider config the engine tests compile against: a local
openai-completions endpoint and a one-character key, none of which any test
asserts on."
  ;; Every keyword defaults, so a lane test that does care about one field
  ;; names that field and inherits the rest.
  (nle::make-effective-provider-config :provider provider
                                       :lane (if (ignore-errors (nle::find-lane-by-name provider))
                                                 provider
                                                 (or (nle::resolve-model-lane provider "test-model")
                                                     "openai-completions"))
                                       :endpoint "http://local"
                                       :model model :api-key "k"))

(defun head-text (context &aux (head (nle::compiled-turn-context-head-message context)))
  "The text of CONTEXT's head message — the standing sections the system
message does not carry — or the empty string when it has none."
  (if head (gethash "content" head) ""))

(defun local-context (messages &rest request-keys)
  "Compile a session-less turn context over MESSAGES with the flat test config
and BASE as the system prompt — the storeless engine fixture, where MESSAGES
is the whole projection because there is no session to fold history from."
  ;; REQUEST-KEYS reach MAKE-CONTEXT-REQUEST ahead of the defaults, so
  ;; :HARNESS-SECTIONS and :VISIBLE-USER-IMAGES stay expressible without a
  ;; second fixture, and a test that wants a different prompt or config simply
  ;; says so: the leftmost keyword argument is the one a lambda list keeps.
  (nle::compile-turn-context
   (apply #'nle::make-context-request
          (append request-keys
                  (list :session-id nil
                        :provider-config (test-provider-config)
                        :base-system-prompt "BASE"
                        :messages messages)))))

(defun test-invocation (&rest keys)
  "An in-process test source's turn.start invocation under command-1 and
correlation-1."
  ;; KEYS reach MAKE-INVOCATION ahead of those defaults, so the leftmost
  ;; keyword, the one a lambda list keeps, is the caller's.
  (apply #'nlk:make-invocation
         (append keys
                 (list :action "turn.start" :command-id "command-1"
                       :correlation-id "correlation-1"
                       :source (nlk:make-invocation-source "test")
                       :transport (nlk:make-invocation-transport "in_process")))))

(defmacro fold-model (model &body calls &aux (place (gensym "MODEL")))
  "Thread MODEL's folded state through CALLS in order, newest state last."
  ;; Each CALL is a reducer written without its state argument, because the
  ;; state is what the fold supplies: (fold-model model
  ;; (nle::ingest-turn-usage :input 4)) is the accessor-heavy SETF of
  ;; CHAT-MODEL-STATE to the reducer's answer over the same accessor. The
  ;; reducers answer a fresh state rather than mutating one, so this SETF is
  ;; the fold's only writer and the order of CALLS is the order the facts
  ;; arrived in.
  `(let ((,place ,model))
     ,@(loop for (reducer . arguments) in calls
             collect `(setf (nle::chat-model-state ,place)
                            (,reducer (nle::chat-model-state ,place) ,@arguments)))))

(defun exchange (state question answer &rest keys)
  "STATE with the operator's QUESTION and its ANSWER folded in, both under KEYS:
the pair a transcript gate folds, written as one FOLD-MODEL call."
  (apply #'nle::ingest-message-fact (apply #'nle::ingest-message-fact state "user" question keys)
         "assistant" answer keys))

(defun block-body-lines (block)
  "BLOCK's painted rows without the padding rows it owns above and below its
body."
  ;; Every display block carries both pads (the Zig block_padding lines), so a
  ;; test that names body content reads past them; the padding contract itself
  ;; is proved by BLOCKS-OWN-THEIR-PADDING-ROWS.
  (butlast (rest block.lines)))

(defun block-words (block)
  "BLOCK-BODY-LINES as the words read: the clock a message's first row carries
at its right end cut away, with the gap the words left it."
  (let* ((lines (block-body-lines block))
         (clock (and lines (nle::message-timestamp-row-p block.row)
                     (nle::transcript-clock block.row)))
         (at (and clock (search clock (first lines) :from-end t))))
    (if at
        (cons (string-right-trim " " (subseq (first lines) 0 at)) (rest lines))
        lines)))

;;; --- the durable-turn fixture ---------------------------------------------
;;; Every engine gate that drives a turn by hand opens the same way: admit a
;;; turn on a fixture session, make it the current one, and cancel it under
;;; `test cleanup' however the body leaves. That scaffold was written out at
;;; forty sites — the UNWIND-PROTECT and the cancel most of all — and it
;;; lives here once instead.

(defmacro with-durable-turn ((turn session-id &key (command-id '(nlk::make-durable-id "command"))
                                   (input "hi") current (retry-delay nil retry-p) complete)
                             &body body)
  "Run BODY with TURN admitted on SESSION-ID under COMMAND-ID carrying INPUT,
cancelled under `test cleanup' however BODY leaves — unless BODY set TURN to
NIL to say it ended the turn itself, or COMPLETE completes it after BODY."
  ;; CURRENT non-NIL also binds it as
  ;; *CURRENT-DURABLE-TURN*; RETRY-DELAY, when given, binds *RETRY-BASE-DELAY*.
  `(let* ((,turn (nlk:admit-turn ,session-id ,command-id ,input))
          ,@(when current `((nle::*current-durable-turn* ,turn)))
          ,@(when retry-p `((nle::*retry-base-delay* ,retry-delay))))
     (nlk:with-cleanup ((when ,turn (nlk:cancel-turn ,turn "test cleanup")))
       ,@body
       ,@(when complete `((nlk:complete-turn ,turn nil) (setf ,turn nil))))))

(defmacro with-streaming-provider-stub ((context on-part &body stub) &body body)
  "Run BODY with CALL-PROVIDER-STREAMING answering STUB, restored on unwind:
the streaming-entry flavour of WITH-PROVIDER-STUB."
  ;; CONTEXT and ON-PART name the stub's two arguments; NIL for either binds a
  ;; gensym the IGNORABLE covers.
  `(with-entry-stub (nodecode.evolved::call-provider-streaming ,context ,on-part ,@stub) ,@body))

;;; --- text that carries needles --------------------------------------------

(defmacro is-carrying (binding &body checks &aux (var (if (consp binding) (first binding) binding)))
  "Assert a text carries every CHECK."
  ;; BINDING is (VAR FORM) or a VAR already
  ;; bound, as in IS-PRESENT. Each CHECK is a needle (its own description), a
  ;; (needle description) pair, (:absent needle [description]) for a negative gate,
  ;; (:= text [description]) for the whole answer, or any other form, spliced as
  ;; is with VAR in scope. Answers the text.
  `(let (,@(when (consp binding) (list binding)))
     ,@(loop for check in checks
             collect (cond ((stringp check) `(is (search ,check ,var) ,check))
                           ((stringp (first check))
                            `(is (search ,(first check) ,var) ,@(rest check)))
                           ((eq (first check) :absent)
                            `(is (null (search ,(second check) ,var)) ,@(cddr check)))
                           ((eq (first check) :=)
                            `(is (equal ,(second check) ,var) ,@(cddr check)))
                           (t check)))
     ,var))

(defmacro is-eval (source &body checks)
  "Eval SOURCE through the snippet and run CHECKS over its answer, bound as
RESULT — IS-CARRYING's check shapes, so a bare form may read RESULT too."
  `(is-carrying (result (nle:eval-string ,source)) ,@checks))

(defun octets (&rest bytes)
  (coerce bytes 'nlk:octets))

(defun replace-once (text needle replacement &aux (at (search needle text)))
  "TEXT with the first NEEDLE replaced."
  ;; The expected document of a splice test is spelled as the fixture plus one
  ;; substitution, so the assertion cannot drift away from the fixture it is
  ;; checking.
  (concatenate 'string (subseq text 0 at) replacement
               (subseq text (+ at (length needle)))))

;;; --- rows that carry a needle ---------------------------------------------

(defun row-carrying (needle rows &key (start 0))
  "The first of ROWS, at or past START, carrying NEEDLE — or NIL."
  (find-if (lambda (row) (search needle row)) rows :start start))

(defun row-index-of (needle rows &key (start 0) required)
  "The 0-based index of the first of ROWS, at or past START, carrying NEEDLE,
or NIL. REQUIRED signals on a miss instead, naming the needle."
  (or (position needle rows :test #'search :start start)
      (and required (error "no row carries ~s" needle))))

(defun rows-carry-p (needle rows)
  "Whether any of ROWS carries NEEDLE."
  (and (row-carrying needle rows) t))

(defun wire-row (id rows &rest path)
  "The first object of ROWS — a wire array or list — whose string member at
PATH, by default \"id\", is ID; or NIL."
  (find id (coerce rows 'list) :test #'equal
        :key (lambda (row) (apply #'nlk:json-value row :string (or path '("id"))))))

(defun durable-fact-frame (&key seq session-id turn-id kind payload event-id
                                log-position command-id occurred-at)
  "One durable fact as the wire carries it: SEQ, SESSION-ID and TURN-ID beside
the inner event, whose attribution rides a metadata envelope of its own and is
never flattened into the payload."
  ;; Every field is an explicit keyword with no default, because an omission
  ;; is what several call sites are proving: a frame that carries no
  ;; command_id or no log_position must reach the fold without one, and a
  ;; default here would quietly stamp it with something the gateway never
  ;; wrote. The envelope itself is the invariant this owns — metadata beside
  ;; the payload, never merged into it, which is what a payload owning its
  ;; own "metadata" field would collide with.
  (nlk:json-object
   "seq" seq "session_id" session-id "turn_id" turn-id
   "event" (nlk::encode-inner-event kind payload
                                    :metadata (nlk:json-object
                                               :opt "event_id" event-id
                                               :opt "log_position" log-position
                                               "turn_id" turn-id
                                               :opt "command_id" command-id
                                               :opt "occurred_at" occurred-at))))

;;; --- driving a model ------------------------------------------------------

(defmacro paint-model (model &body calls)
  "FOLD-MODEL over CALLS, then render MODEL's state, as the loop does after a fold."
  `(progn (fold-model ,model ,@calls) (nle::render-state ,model)))

(defmacro rendering (model &body forms)
  "Run FORMS, then render MODEL's state; answers MODEL."
  `(progn ,@forms (nle::render-state ,model) ,model))

(defmacro feed-events (state &body events)
  "Fold EVENTS through APPLY-GATEWAY-EVENT into STATE, a place, in order —
the SETF a projection test writes once per fact, written once."
  `(progn
     ,@(loop for event in events
             collect `(setf ,state (nle::apply-gateway-event ,state ,event)))))

(defun mark-turn-in-flight (model &key render)
  "Put MODEL's turn in flight with its clock started now, and RENDER its state
when asked; answers MODEL."
  (setf model.turn-in-flight t model.turn-started-at (get-internal-real-time))
  (when render (nle::render-state model))
  model)

;;; --- one assertion per row ------------------------------------------------

(defmacro is-each ((&rest call) &body rows)
  "One IS per ROW — (ARGUMENT... EXPECTED DESCRIPTION) — that CALL over the
row's arguments answers EXPECTED under EQUAL: a table of cases reads as one
form, and every row still fails under its own description."
  `(progn
     ,@(loop for row in rows
             collect (destructuring-bind (expected description) (last row 2)
                       `(is (equal ,expected (,@call ,@(butlast row 2)))
                            ,description)))))

(defmacro is-table ((&rest params) form &body rows)
  "One IS per ROW — (VALUE... [DESCRIPTION]) — over FORM with each of PARAMS
replaced by the row's VALUE in its place: a run of assertions that differ in a
few places reads as one template and its rows."
  ;; The substitution is textual, so every row expands to the very IS it
  ;; replaces, its failure text included.
  `(progn
     ,@(loop for row in rows
             collect `(is ,(sublis (mapcar #'cons params row) form)
                          ,@(nthcdr (length params) row)))))

(defmacro is-shape (subject &body checks)
  "One IS per CHECK over a read of SUBJECT: a run of assertions over one value
reads as one form, and every check still fails under its own text."
  ;; A CHECK is (PATH [PREDICATE] [EXPECTED] [DESCRIPTION]) and expands to the
  ;; very IS it replaces. PATH is the read: a symbol F is (F SUBJECT), a symbol
  ;; .SLOT the dotted reader SUBJECT.SLOT, a string K (GETHASH K SUBJECT), a
  ;; keyword K (GETF SUBJECT K), a list (NLK:JSON-VALUE SUBJECT . PATH).
  ;; PREDICATE is EQ, EQL, =, EQUALP or STRING= over (PREDICATE EXPECTED READ),
  ;; NULL or NOT over (PREDICATE READ), or IS for the read's own truth; with
  ;; none, the second element is EXPECTED under (EQUAL EXPECTED READ). A NIL
  ;; SUBJECT makes every PATH the read itself: IS-VALUES and IS-LET* below.
  `(progn
     ,@(loop for (path op . tail) in checks
             for read = (cond ((null subject) path)
                              ((consp path) `(nlk:json-value ,subject ,@path))
                              ((stringp path) `(gethash ,path ,subject))
                              ((keywordp path) `(getf ,subject ,path))
                              ((char= #\. (char (symbol-name path) 0))
                               (intern (concatenate 'string (symbol-name subject)
                                                    (symbol-name path))))
                              (t (list path subject)))
             collect (case op
                       (is `(is ,read ,@tail))
                       ((null not) `(is (,op ,read) ,@tail))
                       ((eq eql = equalp string=) `(is (,op ,(first tail) ,read) ,@(rest tail)))
                       (t `(is (equal ,op ,read) ,@tail))))))

(defmacro is-values ((&rest vars) form &body checks)
  "IS-SHAPE's CHECKS over the values of FORM, bound to VARS in order: each
check's read is a form over VARS."
  `(multiple-value-bind ,vars ,form (is-shape nil ,@checks)))

(defmacro is-let* ((&rest bindings) &body checks)
  "IS-SHAPE's CHECKS over BINDINGS, bound as LET* binds them: each check's read
is a form over the variables they bind."
  `(let* ,bindings (is-shape nil ,@checks)))

;;; --- one deftest per case -------------------------------------------------

(defmacro deftests (prefix (&rest params) body &body cases)
  "One DEFTEST per CASE — (SUFFIX VALUE...) is named PREFIX-SUFFIX — running
BODY with each of PARAMS standing for the case's VALUE."
  ;; The values are SYMBOL-MACROLET'd, so one may be a form over BODY's own
  ;; bindings. Runner granularity and the suite count are unchanged.
  `(progn
     ,@(loop for (suffix . values) in cases
             collect `(deftest ,(intern (format nil "~a-~a" prefix suffix)
                                        (symbol-package prefix))
                        (symbol-macrolet ,(mapcar #'list params values))
                        ,body))))
