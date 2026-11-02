;;;; events-test.lisp --- C0 gate: durable append-only event log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Eight gate claims from plan C0:
;;;;
;;;;   1. Appended events read back in seq order, monotonic across close/reopen.
;;;;   2. Adding the events table to a store with messages/turns leaves existing
;;;;      rows intact and readable.
;;;;   3. With the floor locked, redefining an event-log writer is refused with
;;;;      symbol-package-locked-error.
;;;;   4. A log carrying a hostile model-authored payload still boots: every
;;;;      payload from the measured set replays as an inert string, and no read
;;;;      interns a symbol or resolves a package.
;;;;   5. An unknown event kind materializes as an inert unknown-event, and the
;;;;      fold treats it as identity (deferred to C1, which defines the reducer).
;;;;   6. An event whose payload fails to write leaves no partial row, asserted
;;;;      by count rather than by inspection.
;;;;   7. 2000 appends stay within the measured 69ms envelope.
;;;;   8. Paging all 2000 back stays under 10ms.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------
;;; Reuse store-test.lisp's WITH-TEMP-STORE and TEMP-DIRECTORY.

(defun record-probe (session-id n &rest keys)
  "Record one probe event on SESSION-ID carrying a plausible payload for turn
N; KEYS reach RECORD-EVENT, whose answer this is."
  (apply #'nlk:record-event session-id "probe"
         (list :turn n :content (format nil "turn ~a completed" n)) keys))

;;; --- lifecycle and ordering ---------------------------------------------

(deftest events-append-and-read-back-in-seq-order (with-session-store ("s-1"))
  (let ((seqs (loop for n from 1 to 3 collect (record-probe "s-1" n))))
    (is (apply #'< seqs) "seq is monotonic")
    (is (equal seqs (mapcar #'nlk:event-log-position (session-events nil :created nil)))
        (format nil "the three recorded events read back in seq order: ~s" seqs))))

(deftest events-survive-close-and-reopen ()
  (with-restartable-store (path "s-1" :cwd (fixture-workspace))
    (record-probe "s-1" 1)
    (reopen-store path)
    (let ((events (session-events nil :created nil)))
      (is (= 1 (length events)) "event survived close/reopen")
      (is (equal "probe" (nlk:event-kind (first events)))))))

(deftest the-open-turn-reads-walk-the-open-set (with-session-store ("s-1"))
  ;; The open-turn reads (ACTIVE-TURN-STARTS, ANY-ACTIVE-TURN-P, ACTIVE-TURN-P,
  ;; the boot recovery pass) seek the set the log keeps: an admission enters
  ;; it, the terminal fact takes it out, and a store without the set derives
  ;; it from the log at open.
  (flet ((open-set ()
           (sqlite:execute-to-list (nlk::require-open-store)
                                   "select turn_id from open_turns")))
    (is (null (nlk:any-active-turn-p)) "no turn in flight")
    (is (null (nlk:active-turn-starts)))
    (let ((turn (nlk:admit-turn "s-1" "s-1-cmd" "hello")))
      (is (nlk:any-active-turn-p) "the admitted turn is open")
      (is (nlk:active-turn-p "s-1"))
      (is (equal '("s-1") (mapcar #'car (nlk:active-turn-starts))))
      (is (equal (list (list (nlk:durable-turn-turn-id turn))) (open-set)))
      (sqlite:execute-non-query (nlk::require-open-store) "drop table open_turns")
      (nlk::apply-schema (nlk::require-open-store))
      (is (equal (list (list (nlk:durable-turn-turn-id turn))) (open-set)) "derived at open")
      (nlk:complete-turn turn nil)
      (is (null (open-set)) "the terminal fact takes it out")
      (is (null (nlk:any-active-turn-p)) "a settled turn leaves the read")
      (is (null (nlk:active-turn-p "s-1")))
      (is (null (nlk:active-turn-starts))))))

(deftest stored-facts-carry-the-logged-payload-under-its-hash-check (with-session-store ("s-1"))
  ;; A replay reads STORED-FACTs: each row's envelope and its payload's own
  ;; bytes, laid into the frame undecoded — verified against the hash the row
  ;; was written with, as an instance is.
  (nlk:admit-turn "s-1" "s-1-cmd" "hello")
  (let ((instances (nlk:events :session-id "s-1"))
        (facts (nlk:events :session-id "s-1" :as :facts)))
    (is (equal (mapcar #'nlk:event-log-position instances)
               (mapcar #'nlk::stored-fact-log-position facts)))
    (is (every (lambda (event stored)
                 (equalp (nlk::event-payload event)
                         (nlk:decode-json (nlk::stored-fact-payload stored))))
               instances facts) "each fact's bytes are its instance's payload"))
  (sqlite:execute-non-query
   (nlk::require-open-store)
   "update events set payload_hash = 'sha256:0000000000000000000000000000000000000000000000000000000000000000'
       where session_id = ?"
   "s-1")
  (is (signals-error nlk:payload-hash-mismatch
        (nlk:events :session-id "s-1" :as :facts))))

(defun statements-read-by (thunk &aux (statements '()))
  "The SQL of every row read (SQL-ROWS) THUNK runs, in order."
  (nlk:hook 'nlk::sql-rows "statement-probe"
            (lambda (next &rest arguments)
              (push (second arguments) statements)
              (apply next arguments)))
  (unwind-protect (funcall thunk)
    (nlk:unhook 'nlk::sql-rows "statement-probe"))
  (reverse statements))

(deftest a-read-scoped-by-kind-alone-walks-that-kind (with-session-store ("s-1"))
  ;; The statement EVENTS builds for a read scoped by kind alone is planned
  ;; through events_by_kind, never as a scan of the whole log.
  (let* ((statement (first (statements-read-by
                            (lambda ()
                              (nlk:events :kind nlk::+kind-session-title+ :order :newest
                                          :as :rows :columns '("session_id" "payload"))))))
         (plan (format nil "~{~a~^; ~}"
                       (mapcar #'fourth
                               (sqlite:execute-to-list
                                (nlk::require-open-store)
                                (concatenate 'string "explain query plan " statement)
                                nlk::+kind-session-title+)))))
    (is (search "USING INDEX events_by_kind" plan) plan)))

(deftest a-fork-chain-read-seeks-its-sessions (with-session-store ("s-1"))
  ;; A kind over a fork chain would otherwise be driven from events_by_kind
  ;; across every session's rows: the chain's reads name the session index.
  (let ((chain-reads (remove-if-not
                      (lambda (statement) (search "(session_id = ?" statement))
                      (statements-read-by
                       (lambda ()
                         (nlk::retained-history-floors "s-1")
                         (nlk:recall-log :session-id "s-1" :kinds nlk::+kind-tool-result+))))))
    (is (= 3 (length chain-reads)))
    (is (every (lambda (statement) (search "indexed by events_by_session_kind" statement))
               chain-reads))))

(deftest events-table-schema-survives-close-and-reopen-with-extra-tables ()
  ;; Verify that a database whose DDL was created with extra tables (the A3
  ;; messages/turns shape) still accepts event writes after reopening.
  ;; record-turn and store-turn-count are gone; the test now seeds the legacy
  ;; shape directly via raw SQL and checks the event log independently.
  (with-temp-directory (dir "events" :close-store t)
    ;; Create a store with a legacy schema that includes extra tables.
    ;; The legacy shape has no directory, so the projections that open over
    ;; it (model history's triggers on session_directory) stay out of it.
    (with-saved-globals (nlk::*schema* nlk::*store-open-hooks*)
      (setf nlk::*store-open-hooks* '())
      (setf nlk::*schema*
            (list "pragma journal_mode = wal"
                  "pragma synchronous = normal"
                  "create table if not exists legacy_rows (id integer primary key)"
                  ;; The events table must be present so open-store can write later.
                  (find-if (lambda (ddl) (search "create table if not exists events" ddl))
                           nlk::*events-schema*)))
      (nlk:open-store :path (merge-pathnames "history.db" dir))
      ;; Insert a sentinel into the legacy table directly.
      (sqlite:execute-non-query
       (nlk::require-open-store)
       "insert into legacy_rows (id) values (?)" 42)
      (nlk:close-store))
    ;; Reopen with the full current schema. Events table already exists
    ;; so CREATE TABLE IF NOT EXISTS is a no-op; the legacy table stays.
    (nlk:open-store :path (merge-pathnames "history.db" dir))
    ;; The legacy row is still there.
    (is (= 42
           (sqlite:execute-single
            (nlk::require-open-store)
            "select id from legacy_rows")))
    (ensure-durable-session "s-1")
    (record-probe "s-1" 2)
    (is (= 1 (count-session-events nil "probe")))))

;;; --- package lock -------------------------------------------------------

(deftest lock-floor-refuses-event-writer-redefinition (with-floor-restored)
  ;; The floor comes back however it was found: unlocking on the way in and
  ;; unlocking again on the way out left it down for every gate after this one.
  (nodecode.kernel:unlock-floor)
  (is (eval '(defun nlk::event-probe-before-lock () :allowed)))
  (nodecode.kernel:lock-floor)
  (signals-error sb-ext:symbol-package-locked-error
    (eval '(defun nlk:record-event (sid kind payload)
             (declare (ignore sid kind payload)) :mutated))))

;;; --- hostile payloads ---------------------------------------------------

(deftest hostile-payloads-read-as-inert-strings (with-temp-store ())
  ;; The measured set: package resolution, locked symbol, reader macro, huge
  ;; float, unmatched paren, malformed plist, unbounded symbol interning.
  ;; All must parse to inert values; none may crash, intern, or resolve packages.
  (flet ((count-symbols (&aux (n 0))
           (do-all-symbols (s) (declare (ignore s)) (incf n)) n))
    (let ((before (count-symbols)))
      (ensure-durable-session "s-hostile")
      (dolist (payload (list (list :body "NOSUCHPKG::gotcha")
                             (list :body "SB-KERNEL::internal")
                             (list :body "#.(error 'reader-macro)")
                             (list :body "1e999999")
                             (list :body "((((")
                             (list :body "(:a :b :c)")
                             (list :body "brand-new-symbol-one brand-new-symbol-two")))
        (nlk:record-event "s-hostile" "probe" payload))
      (let ((events (session-events "s-hostile" :created nil)))
        (is (= 7 (length events)) "all 7 hostile payloads stored")
        (is (every (lambda (e) (typep e 'nlk:unknown-event)) events))
        (is (= before (count-symbols)) "no symbols interned during read")
        (is (null (find-package "NOSUCHPKG")) "no package created")))))

;;; --- read-side contract verification ------------------------------------

(deftest read-refuses-a-row-whose-contract-triple-drifted (with-session-store ("s-contract"))
  ;; The contract hash binds aggregate_type|kind|schema_version at write time.
  ;; The SQL CHECK only proves the column is hash-shaped; recomputing on read
  ;; is what catches a drifted or tampered triple — here schema_version, the
  ;; one column of the triple with no constraint of its own.
  (nlk:record-event "s-contract" "probe" (list :n 1))
  (sqlite:execute-non-query
   (nlk::require-open-store)
   "update events set schema_version = 99
       where session_id = ? and kind = 'probe'"
   "s-contract")
  (is (signals-error nlk:contract-hash-mismatch
        (nlk:page-events :session-id "s-contract"))))

;;; --- write failure atomicity --------------------------------------------

(deftest failed-payload-leaves-no-partial-row (with-session-store ("s-1"))
  (nlk:record-event "s-1" "before" (list :n 1))
  (is (signals-error error
        (nlk:record-event "s-1" "broken" (list :fn #'car))))
  (is (= 2 (nlk::event-count)) "failed write left no partial row"))

;;; --- performance gates --------------------------------------------------

(deftest durable-ids-are-distinct-with-shared-random-state ()
  (let ((ids (make-hash-table :test #'equal)))
    (loop repeat 5000
          do (setf (gethash (nlk::make-durable-id "event") ids) t))
    (is (= 5000 (hash-table-count ids)) "5000 generated IDs are distinct")))

(deftest two-thousand-appends-under-250ms (with-session-store ("s-perf"))
  ;; The window times the appends alone: a nursery that earlier tests left
  ;; nearly full would bill their collection, some 60 ms, to it.
  (sb-ext:gc)
  (let ((start (get-internal-real-time)))
    (dotimes (i 2000)
      (record-probe "s-perf" i))
    (let* ((end (get-internal-real-time))
           (elapsed-ms (/ (* 1000.0 (- end start)) internal-time-units-per-second)))
      (format t "       append: 2000 events in ~,1fms, ~,3fms each (gate ~,1fms)~%"
              elapsed-ms (/ elapsed-ms 2000) 250.0)
      (is (< elapsed-ms 250.0)
          (format nil "2000 appends took ~,1fms (gate: <250ms)" elapsed-ms)))))

(deftest paging-two-thousand-under-50ms (with-session-store ("s-perf"))
  (dotimes (i 2000)
    (record-probe "s-perf" i))
  (let ((start (get-internal-real-time))
        (collected 0)
        (cursor 0))
    (loop
      (let ((page (nlk:page-events :session-id "s-perf" :after cursor :limit 500)))
        (when (null page) (return))
        (incf collected (count "probe" page
                               :key #'nlk:event-kind :test #'equal))
        (setf cursor (nlk:event-log-position (first (last page))))))
    (let* ((end (get-internal-real-time))
           (elapsed-ms (/ (* 1000.0 (- end start)) internal-time-units-per-second)))
      (format t "       page: 2000 events in ~,1fms across 4 pages (gate ~,1fms)~%"
              elapsed-ms 50.0)
      (is (= 2000 collected) "paged all 2000 events")
      (is (< elapsed-ms 50.0)
          (format nil "paging 2000 events took ~,1fms (gate: <50ms)" elapsed-ms)))))

;;; --- append contract ----------------------------------------------------

(deftest expected-sequence-accepts-a-match (with-session-store ("s-1"))
  (record-probe "s-1" 1 :expected-sequence 1)
  (record-probe "s-1" 2 :expected-sequence 2)
  (is (= 2 (count-session-events "s-1" "probe")))
  (is (equal '(1 2 3)
             (mapcar #'nlk:event-aggregate-sequence
                     (nlk:page-events :session-id "s-1")))))

(deftest expected-sequence-rejects-a-mismatch (with-session-store ("s-1"))
  (record-probe "s-1" 1)
  (is (signals-error nlk:event-sequence-conflict
        (record-probe "s-1" 2 :expected-sequence 0)))
  (is (= 2 (nlk:event-count :session-id "s-1"))))

(deftest duplicate-event-id-is-idempotent (with-session-store ("s-1"))
  ;; The same id and content answers the stored seq, over a stale expected
  ;; sequence too; the same id with other content is refused. None appends.
  (let ((seq (record-probe "s-1" 1 :event-id "event-dup")))
    (is (= seq (record-probe "s-1" 1 :event-id "event-dup")) "duplicate id returns the stored seq")
    (is (eql seq (record-probe "s-1" 1 :event-id "event-dup" :expected-sequence 0)))
    (is (signals-error nlk:event-id-conflict (record-probe "s-1" 2 :event-id "event-dup")))
    (is (= 2 (nlk:event-count :session-id "s-1")))))

(deftest sha256-text-is-the-standard-digest ()
  ;; Every payload_hash on disk was written by SHA256-TEXT, so its value
  ;; cannot move with the library under it: FIPS 180-2 vectors, and a string
  ;; hashes as its UTF-8 bytes, held in any octet vector.
  (is (equal "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
             (nlk:sha256-text "")))
  (is (equal "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (nlk:sha256-text "abc")))
  (is (equal (nlk:sha256-text "é")
             (nlk:sha256-text (make-array 2 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#xc3 #xa9) :adjustable t)))))

(deftest canonical-envelope-carries-aggregate-and-provenance (with-temp-store ())
  (nlk:create-session :id "envelope-session" :command-id "create-command")
  (nlk:record-event "envelope-session" "probe" (list :n 1)
                    :command-id "probe-command"
                    :invocation (test-invocation
                                 :action "probe" :command-id "probe-command"
                                 :correlation-id "probe-correlation"
                                 :source (nlk:make-invocation-source "test" :id "unit")))
  (let ((event (session-event "envelope-session" :kind "probe")))
    (is (= (nlk:event-log-position event) event.log-position))
    (is-table (expected reader) (equal expected (reader event))
      ("session" nlk:event-aggregate-type "aggregate type is durable")
      ("envelope-session" nlk:event-aggregate-id "aggregate id is durable")
      ("envelope-session" nlk:event-subject-id "session subject defaults to the aggregate"))
    (is (ppcre:scan "^sha256:[0-9A-Fa-f]{64}\\z" event.contract-hash))
    (is (ppcre:scan "^sha256:[0-9A-Fa-f]{64}\\z" event.payload-hash))
    (is (nlk::iso-8601-p event.occurred-at))
    (is (stringp event.invocation-json))))

(deftest canonical-global-position-spans-aggregates (with-session-store ("aggregate-a"))
  (nlk:create-session :id "aggregate-b")
  (nlk:record-event "aggregate-a" "probe" (list :n 1))
  (nlk:record-event "aggregate-b" "probe" (list :n 2))
  (let ((events (session-events nil :created nil)))
    (is (= 2 (length events)) "both aggregate events are visible")
    (is (< (nlk:event-log-position (first events))
           (nlk:event-log-position (second events))))
    (is (= 2 (nlk:event-aggregate-sequence (first events))))
    (is (= 2 (nlk:event-aggregate-sequence (second events))))))

(deftest stale-events-table-is-replaced-not-upcast ()
  (with-temp-directory (dir "stale-events" :close-store t)
    (let ((path (merge-pathnames "history.db" dir)))
      (nlk:open-store :path path)
      (nlk:close-store)
      (let ((handle (sqlite:connect path)))
        (nlk:with-cleanup ((sqlite:disconnect handle))
          (sqlite:execute-non-query handle "drop table events")
          (sqlite:execute-non-query
           handle
           "create table events (
                        seq integer primary key autoincrement,
                        session_id text not null,
                        kind text not null,
                        payload text not null,
                        recorded_at real not null)")
          (sqlite:execute-non-query
           handle
           "insert into events (session_id, kind, payload, recorded_at)
                      values ('s-1', 'turn.completed', '{}', 1.0)")))
      (nlk:open-store :path path)
      (let ((sql (sqlite:execute-single
                  (nlk::require-open-store)
                  "select sql from sqlite_master where type='table' and name='events'")))
        (is (search "payload_hash" sql) "reopen installs payload_hash")
        (is (search "occurred_at" sql) "reopen installs occurred_at"))
      (is (zerop (nlk:event-count)) "stale rows are not upcast")
      (nlk:close-store))))

;;; --- the event declaration ----------------------------------------------
;;; DEFINE-EVENT writes the class, the payload hydration and the kind
;;; registration from one spec, so a slot's declared type is also the type
;;; check its stored payload passes through. These prove that single source.

(nlk::define-event declared-probe-event nil
  "Throwaway class exercising DEFINE-EVENT's own contract."
  ((name :string "name" :default "anon")
   (tally :integer "tally")
   (extra :object "extra")))

(defun probe-event (&rest pairs)
  "Materialize a DECLARED-PROBE-EVENT with PAIRS as its stored payload."
  (make-instance 'declared-probe-event
                 :payload (apply #'nlk:make-json-object pairs)))

(deftest a-declared-slot-type-is-also-its-payload-check ()
  (is-let* ((wrong (probe-event "name" 7 "tally" "12"))) ((declared-probe-event-name wrong) "anon")
    ((declared-probe-event-tally wrong) null))
  (let ((right (probe-event "name" "ada" "tally" 12)))
    (is (equal "ada" (declared-probe-event-name right)) "declared string reads")
    (is (eql 12 (declared-probe-event-tally right)) "declared integer reads")))

(deftest a-declared-default-covers-an-absent-key (let ((empty (probe-event))))
  (is-shape empty (declared-probe-event-name "anon") (declared-probe-event-extra null)))

(deftest the-declaration-is-what-registers-the-kind ()
  (is (typep (nlk::materialize-event
              :kind nlk::+kind-turn-usage+
              :payload (nlk:json-object "input-tokens" 5))
             'nlk::turn-usage-event)))

(deftest a-malformed-slot-spec-is-refused-at-macroexpansion ()
  (is-table (spec)
    (nth-value 1 (ignore-errors (macroexpand-1 '(nlk::define-event probe nil "doc" (spec)))))
    ((slot :string))
    ((slot :string "key" :defualt ""))))

;;; --- DEFINE-ERROR --------------------------------------------------------
;;; The refusal-declaration waist (src/conditions.lisp). What is locked here is
;;; the part a hand-written condition kept getting wrong: which reader a report
;;; argument resolves to, and which arguments are not slots at all.

(nlk::define-error probe-error (error)
    ((detail :initform "none") (code :initarg :status))
  (:report "probe failed (~a): ~a" code detail))

(nlk::define-error probe-child-error (probe-error) (extra)
  (:report "child ~a of ~a" extra detail))

(nlk::define-error probe-silent-error (probe-error) ()
  (:report "nothing to say"))

(defparameter +probe-ceiling+ 7)

(nlk::define-error probe-constant-error (error) (requested)
  (:report "~a exceeds ~a" requested +probe-ceiling+))

(deftest define-error-derives-initarg-reader-and-report ()
  (is-let* ((condition (make-condition 'probe-error :detail "disk" :status 42)))
    ((probe-error-detail condition) "disk") ((probe-error-code condition) = 42)
    ((format nil "~a" condition) "probe failed (42): disk"))
  (is (equal "none" (probe-error-detail (make-condition 'probe-error)))))

(deftest define-error-report-reaches-an-inherited-slot ()
  ;; The defect this replaces: a child's report has to name the ANCESTOR's
  ;; reader, and nothing checked that it did.
  (is (equal "child ring of disk"
             (format nil "~a" (make-condition 'probe-child-error :detail "disk" :extra "ring")))))

(deftest define-error-handles-argument-less-and-constant-reports ()
  (is-table (expected condition) (equal expected (format nil "~a" condition))
    ("nothing to say" (make-condition 'probe-silent-error))
    ("9 exceeds 7" (make-condition 'probe-constant-error :requested 9))))

;;; --- the one read over the log ------------------------------------------

(deftest events-projections-answer-the-seeded-log (with-session-store ("s-1"))
  ;; EVENTS is the one read every event reader became. Seed two sessions
  ;; with probe kinds and ask every projection, scope, order and bound the
  ;; readers use, against what the log plainly holds.
  (ensure-durable-session "s-2")
  (let* ((probe-kinds '("probe.a" "probe.b"))
         (a (nlk:record-event "s-1" "probe.a" '(:n 1)))
         (b (nlk:record-event "s-1" "probe.b" '(:n 2)))
         (c (nlk:record-event "s-2" "probe.a" '(:n 3)))
         (d (nlk:record-event "s-1" "probe.a" '(:n 4))))
    (flet ((seqs (events) (mapcar #'nlk:event-log-position events))
           (ns (payloads) (mapcar (lambda (p) (gethash "n" p)) payloads))
           (in-s-1 (&rest keys) (apply #'nlk:events :session-id "s-1" keys)))
      ;; :count and :exists
      (is-table (expected keys) (= expected (apply #'nlk:events :as :count keys))
        (3 (list :session-id "s-1" :kind probe-kinds) "a kind list counts across kinds")
        (2 '(:session-id "s-1" :kind "probe.a") "a kind string counts one kind")
        (0 '(:session-id "nope") ":count answers 0, never NIL"))
      (is (nlk:events :session-id "s-2" :kind "probe.a" :as :exists))
      (is (not (nlk:events :session-id "s-2" :kind "probe.b" :as :exists)))
      ;; :instances -- order, bounds, limit
      (is-table (expected keys)
        (equal expected (seqs (apply #'nlk:events :session-id "s-1" :kind probe-kinds keys)))
        ((list a b d) '() "oldest first by default")
        ((list d b) '(:order :newest :limit 2) ":newest with a limit is the newest window")
        ((list b d) (list :after a) ":after is strict")
        ((list a) (list :before b) ":before is strict"))
      (is (= d (nlk:event-log-position (in-s-1 :kind "probe.a" :order :newest :as :instance))))
      (is (null (nlk:events :session-id "nope" :as :instance)))
      ;; :payloads / :payload
      (is (equal '(1 4) (ns (in-s-1 :kind "probe.a" :as :payloads))))
      (is (= 3 (gethash "n" (nlk:events :log-position c :as :payload))))
      (is (= 2 (gethash "n" (nlk:events
                             :event-id (nlk:event-id
                                        (nlk:events :log-position b
                                                    :as :instance))
                             :as :payload))))
      (is (null (nlk:events :session-id "nope" :as :payload)))
      ;; :rows / :row / :value, with expressions as columns
      (is (equal (list (list "probe.a" a) (list "probe.b" b) (list "probe.a" d))
                 (in-s-1 :kind probe-kinds :as :rows :columns '("kind" "log_position"))))
      (is (equal (list 3 d)
                 (in-s-1 :kind probe-kinds :as :row :columns '("count(*)" "max(log_position)"))))
      (is (= d (in-s-1 :as :value :columns '("max(log_position)"))))
      (is (equal "probe.b" (in-s-1 :after a :as :value :columns '("kind"))))
      (is (null (nlk:events :session-id "nope" :as :value :columns '("kind"))))
      ;; :where -- a predicate no key spells
      (is (= 1 (in-s-1 :where (list "json_extract(payload, '$.n') = ?" 4) :as :count)))
      ;; s-1 holds a probe.b, so both of its probe.a rows match; s-2's does not.
      (is (= 2 (nlk:events :kind "probe.a"
                           :where (list "exists (select 1 from events as other
                                             where other.session_id = events.session_id
                                               and other.kind = ?)"
                                        "probe.b")
                           :as :count)))
      (signals-error error (nlk:events :as :value)))))

;;; --- adoption -----------------------------------------------------------
;;; ADOPT-EVENT lands an envelope another store recorded, keeping its identity
;;; and deriving the hashes again; the read side's verification is the proof.

(defun envelope-rows (session-id &aux (columns (mapcar #'first nlk:+event-envelope-columns+))
                                      (keys (mapcar #'second nlk:+event-envelope-columns+)))
  "SESSION-ID's rows over the portable columns, oldest first, as the plists
ADOPT-EVENT takes — the shape a sync ships."
  (loop for values in (nlk:events :session-id session-id :as :rows :columns columns)
        collect (loop for key in keys
                      for value in values
                      when value append (list key value))))

(deftest events-adopt-keeps-the-envelope-and-derives-the-hashes (let ((rows nil)))
  (with-temp-store ()
    (seed-completed-turn "s-1" "ping" "pong"
                         :tools '(("c1" "eval" "(+ 1 2)" "3")))
    (setf rows (envelope-rows "s-1")))
  (is (< 4 (length rows)) "the seed left a turn's worth of owned-kind rows")
  (with-temp-store ()
    (is (null (nlk:session-exists-p "s-1")) "a fresh store")
    (let ((answers (mapcar (lambda (row) (multiple-value-list (nlk:adopt-event row)))
                           rows)))
      (is (every #'second answers) "every row adopted")
      (is (apply #'< (mapcar #'first answers)) "local positions climb in order"))
    (is (nlk:session-exists-p "s-1") "the session exists from its adopted fact")
    (is (equal rows (envelope-rows "s-1")))
    (is (= (length rows) (length (nlk:events :session-id "s-1"))))
    (is (find "s-1" (nlk:list-sessions) :key (lambda (entry) (getf entry :id))
                                        :test #'equal))))

(deftest events-adopt-is-idempotent-and-refuses-collision-rewind-and-orphan (let ((rows nil)))
  (with-session-store ("s-1")
    (record-probe "s-1" 1)
    (record-probe "s-1" 2)
    (setf rows (envelope-rows "s-1")))
  (is (= 3 (length rows)) "created plus two probes")
  (with-temp-store ()
    (destructuring-bind (created first-probe second-probe) rows
      (signals-error nlk:unknown-session (nlk:adopt-event first-probe))
      (nlk:adopt-event created)
      (nlk:adopt-event first-probe)
      (multiple-value-bind (position adopted) (nlk:adopt-event first-probe)
        (is (integerp position) "the same row again answers its position")
        (is (null adopted) "and adopts nothing"))
      (let ((forged (copy-list first-probe)))
        (setf (getf forged :payload) "{\"content\":\"forged\"}")
        (signals-error nlk:event-id-conflict (nlk:adopt-event forged)))
      (let ((rewound (copy-list second-probe)))
        (setf (getf rewound :event-id) "event-rewound"
              (getf rewound :aggregate-sequence) 2)
        (signals-error nlk:event-sequence-conflict (nlk:adopt-event rewound)))
      (let ((malformed (copy-list second-probe)))
        (setf (getf malformed :event-id) "event-malformed"
              (getf malformed :payload) "[1]")
        (signals-error nlk:event-envelope-invalid (nlk:adopt-event malformed)))
      (is (= 2 (nlk:events :session-id "s-1" :as :count))))))

;;; --- what this process committed, in memory --------------------------------
;;; The store remembers the instances it committed, per session, and the live
;;; publisher drains them instead of paging the log back out of SQLite. The
;;; claim is byte-level: a drained instance IS the instance the pager builds.

(defun same-event-p (a b)
  "A and B hydrate alike: one class, every slot bound alike and EQUAL, a
payload object by its JSON text."
  (and (eq (class-of a) (class-of b))
       (every (lambda (slot &aux (name (sb-mop:slot-definition-name slot)))
                (and (eq (slot-boundp a name) (slot-boundp b name))
                     (or (not (slot-boundp a name))
                         (let ((x (slot-value a name))
                               (y (slot-value b name)))
                           (if (hash-table-p x)
                               (and (hash-table-p y)
                                    (equal (nlk:encode-json-object x)
                                           (nlk:encode-json-object y)))
                               (equal x y))))))
              (sb-mop:class-slots (class-of a)))))

(defun drained-seqs (session-id cursor)
  "The seqs COMMITTED-EVENTS-AFTER drains for SESSION-ID past CURSOR, or its
miss when the memory cannot answer."
  (multiple-value-bind (drained miss) (nlk:committed-events-after session-id cursor)
    (or miss (mapcar #'nlk:event-log-position drained))))

(deftest committed-events-drain-as-the-pager-reads-them (with-session-store ("s-1"))
  (ensure-durable-session "s-2")
  (let ((first (record-probe "s-1" 1)))
    (record-probe "s-2" 2)            ; another session's row interleaves
    (seed-completed-turn "s-1" "hello" "hi" :tools '(("c1" "eval" "(+ 1 2)" "3")))
    (record-probe "s-1" 3)
    (multiple-value-bind (drained miss) (nlk:committed-events-after "s-1" 0)
      (is (null miss) "everything since the open is in memory")
      (let ((paged (nlk:page-events :session-id "s-1" :after 0)))
        (is (= (length paged) (length drained)) "the session's rows, no other session's")
        (is (every #'same-event-p drained paged))
        (is (member 'nlk:tool-result-event drained :key #'type-of))))
    (is (equal (mapcar #'nlk:event-log-position (nlk:page-events :session-id "s-1" :after first))
               (drained-seqs "s-1" first)))
    (is (null (drained-seqs "s-1" (nlk::session-log-cursor "s-1"))))))

(deftest a-rolled-back-fact-is-not-remembered (with-session-store ("s-1"))
  (let ((before (nlk::session-log-cursor "s-1")))
    (nlk:with-handlers ((error () nil))
      (nlk::call-with-store-transaction
       (lambda (handle)
         (nlk::append-event-row handle "s-1" "probe" (list :turn 9))
         (error "abort the transaction"))))
    (is (null (drained-seqs "s-1" before)))
    (is (= before (nlk::session-log-cursor "s-1")))))

(deftest a-cursor-below-the-ring-misses-and-the-log-pages ()
  (with-saved-globals ((nlk::*committed-events-capacity* 4))
    (with-session-store ("s-1")
      ;; created plus six probes; the ring keeps the newest four
      (destructuring-bind (p1 p2 p3 p4 p5 p6)
          (loop for n from 1 to 6 collect (record-probe "s-1" n))
        (is (eq :miss (drained-seqs "s-1" 0)))
        (is (eq :miss (drained-seqs "s-1" p1)))
        (is (equal (list p3 p4 p5 p6)
                   (drained-seqs "s-1" p2)) "the floor is the position last evicted")
        (is (equal (list p6) (drained-seqs "s-1" p5)))))))

(deftest the-rings-share-one-bound-and-the-oldest-session-gives-way ()
  (with-saved-globals ((nlk::*committed-events-capacity* 4))
    (with-session-store ("s-1")
      (let* ((p1 (record-probe "s-1" 1))
             (p2 (record-probe "s-1" 2)))
        (ensure-durable-session "s-2")
        ;; the four newest events across the rings are all s-2's now
        (destructuring-bind (q1 q2 q3 q4)
            (loop for n from 1 to 4 collect (record-probe "s-2" n))
          (is (eq :miss (drained-seqs "s-1" p1)))
          (is (null (drained-seqs "s-1" p2)) "above its floor the answer stands")
          (is (equal (list q2 q3 q4)
                     (drained-seqs "s-2" q1)) "the session still committing keeps its newest"))))))

(deftest a-reopened-store-remembers-nothing-until-it-commits ()
  (with-restartable-store (path "s-1" :cwd (fixture-workspace))
    (let ((before (record-probe "s-1" 1)))
      (reopen-store path)
      (is (eq :miss (drained-seqs "s-1" 0)))
      (is (eq :miss (drained-seqs "s-1" before)))
      (let ((after (record-probe "s-1" 2)))
        (is (equal (list after)
                   (drained-seqs "s-1" before)) "born knowing the newest position it does not hold")
        (is (eq :miss (drained-seqs "s-1" (1- before))))))))
;;; --- eval-snippet literal references ----------------------------------------
;;; ($ "name") is the eval wire's dense-text reference; the row must read the
;;; strings entry it names, not the placeholder.

(deftest eval-snippet-literals-resolve-in-the-displayed-command ()
  (is-table (command arguments) (equal command (nlk:tool-command-string arguments))
    ("(sh echo hi there)"
     "{\"form\": \"(sh ($ \\\"cmd\\\"))\", \"strings\": {\"cmd\": \"echo hi\\nthere\"}}")
    ("(sh ($ \"gone\"))" "{\"form\": \"(sh ($ \\\"gone\\\"))\", \"strings\": {\"cmd\": \"x\"}}")
    ("(sh plain)" "{\"form\": \"(sh plain)\"}")))

(deftest events-grouped-columns-aggregate-the-scoped-log (with-session-store ("s-1"))
  (let ((a (nlk:record-event "s-1" "probe.a" '(:n 2)))
        (b (nlk:record-event "s-1" "probe.b" '(:n 7)))
        (c (nlk:record-event "s-1" "probe.a" '(:n 3))))
    (is (equal (list (list "probe.a" 2 5 c) (list "probe.b" 1 7 b))
               (sort (nlk:events :session-id "s-1" :kind '("probe.a" "probe.b")
                                 :as :rows :columns '("kind" "count(*)" "sum(json_extract(payload, '$.n'))" "max(log_position)")
                                 :group-by '(1))
                     #'string< :key #'first)))
    (is (equal (list (list "probe.a" 1 2 a))
               (nlk:events :session-id "s-1" :kind '("probe.a" "probe.b")
                           :before b :as :rows
                           :columns '("kind" "count(*)" "sum(json_extract(payload, '$.n'))" "max(log_position)")
                           :group-by '("kind"))))
    (is (null (nlk:events :session-id "missing" :as :rows
                          :columns '("kind" "count(*)") :group-by '(1))))
    (is (signals-error error (nlk:events :as :count :group-by '("kind"))))))
