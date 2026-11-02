;;;; identity-test.lisp --- session aggregate: create is a command.

(in-package #:nodecode.test)

(nlk:access (child nlk::durable-session) (foreign nle::session) (found nlk::durable-session)
            (leaf nlk::durable-session) (live nle::session) (mid nlk::durable-session)
            (parent nlk::durable-session) (turn nlk::durable-turn))

(deftest create-session-records-session-created (with-temp-store ())
  (let* ((session (nlk:create-session :id "id-create" :cwd "."
                                      :command-id "create-1"))
         (events (nlk:page-events :session-id "id-create"))
         (event (first events)))
    (is (equal "id-create" (nlk:durable-session-id session)) "id is stored")
    (is (uiop:absolute-pathname-p (nlk:durable-session-cwd session)))
    (is (null (nlk:durable-session-parent session)) "root has no parent")
    (is (nlk::iso-8601-p (nlk:durable-session-created-at session)))
    (is (= 1 (length events)) "create writes one fact")
    (is (typep event 'nlk:session-created) "the fact is session.created")
    (is-shape event (.kind "session.created" "wire kind matches") (.command-id "create-1")
      (.correlation-id "create-1") (.causation-id null "a root session has no cause"))
    (is (nlk:session-exists-p "id-create") "the id is findable")
    (is (equal (nlk:durable-session-cwd session)
               (nlk:durable-session-cwd (nlk:find-session "id-create"))))))

(deftest create-session-resolves-relative-cwd (with-temp-store ())
  (let ((session (nlk:create-session :id "id-rel" :cwd "subdir")))
    (is (uiop:absolute-pathname-p (nlk:durable-session-cwd session)))
    (is (search "subdir" (nlk:durable-session-cwd session)))))

(deftest create-session-generates-an-id-when-omitted (with-temp-store ())
  (let ((session (nlk:create-session)))
    (is (stringp (nlk:durable-session-id session)) "an id is generated")
    (is (nlk:session-exists-p (nlk:durable-session-id session)))))

(deftest create-session-refuses-a-taken-id-or-a-bad-parent (with-session-store ("id-dup"))
  (is-table (condition create) (signals-error condition create)
    (nlk:duplicate-session (nlk:create-session :id "id-dup"))
    (nlk:unknown-parent-session (nlk:create-session :id "id-child" :parent "id-missing"))
    (nlk:invalid-session-parent (nlk:create-session :id "id-self" :parent "id-self")))
  (is (= 1 (nlk:event-count :session-id "id-dup")))
  (is (not (nlk:session-exists-p "id-child"))))

(deftest child-session-links-to-parent (with-temp-store ())
  (let* ((parent (nlk:create-session :id "id-parent"))
         (child (nlk:create-session :id "id-child" :parent "id-parent"))
         (event (first (nlk:page-events :session-id "id-child"))))
    (is (equal "id-parent" child.parent))
    (is (equal (nlk::event-id (first (nlk:page-events :session-id parent.id)))
               event.causation-id))))

(deftest find-session-survives-reopen ()
  (with-restartable-store (path "id-reopen" :cwd "/tmp/nodecode-identity")
    (reopen-store path)
    (let ((found (nlk:find-session "id-reopen")))
      (is found "the session is still there")
      (is-shape found (.cwd "/tmp/nodecode-identity") (.parent null)))))

(deftest writes-refuse-an-unknown-session (with-temp-store ())
  (is-table (call) (signals-error nlk:unknown-session call)
    ((nlk:admit-turn "id-never" "command-1" "hello"))
    ((nlk:record-event "id-never" "probe" (list :n 1)))
    ((nlk:record-exchange-turn "id-never" "hi" "there")))
  (is (zerop (nlk:event-count :session-id "id-never"))))

(deftest intern-session-is-a-cache-of-the-created-fact (with-temp-store (store))
  (is (signals-error nlk:unknown-session
        (nle:intern-session "id-intern-missing")))
  (nlk:create-session :id "id-intern" :cwd "/tmp/cached-cwd")
  (is (eq (nle:intern-session "id-intern") (nle:intern-session "id-intern"))))

(deftest record-event-refuses-owned-kinds (with-session-store ("id-owned-kinds"))
  (dolist (kind (alexandria:hash-table-keys nlk::*owned-kind-owners*))
    (is (signals-error nlk:event-kind-not-writable
          (nlk:record-event "id-owned-kinds" kind (list :x 1)))
        (format nil "record-event refuses ~a" kind)))
  (is (= 1 (nlk:event-count :session-id "id-owned-kinds"))))

(deftest append-event-row-refuses-owned-kinds-without-owner (with-session-store ("id-append-owner"))
  (is (signals-error nlk:event-kind-not-writable
        (nlk::append-event-row (nlk::require-open-store)
                               "id-append-owner" "turn.completed"
                               (list :turn-number 1 :message-count 0))))
  (is (= 1 (nlk:event-count :session-id "id-append-owner"))))

(deftest open-store-make-session-does-not-mint (with-temp-store (store))
  (is (signals-error nle::heap-session-mint-refused
        (nle::make-session)))
  (is (signals-error nlk:unknown-session
        (nle::make-session :id "id-mint")))
  (nlk:create-session :id "id-mint")
  (is (eq (nle::make-session :id "id-mint") (nle:intern-session "id-mint"))))

(deftest run-interns-the-live-session (with-session-store ("id-run"))
  (let ((foreign (nle::%make-session :id "id-run")))
    (with-provider-stub (nil (wire-message "assistant" :content "ok"))
      (nle:run "hello" :session foreign :command-id "id-run-cmd"))
    (let ((live (nle:intern-session "id-run")))
      (is (not (eq foreign live)))
      (is (not live.in-flight-p))
      (is (null foreign.messages))
      (is (null live.messages))
      (nle:hydrate-session live)
      (is (plusp (length live.messages))))))

(deftest create-session-forks-a-child-off-a-fresh-fork (with-temp-store ())
  ;; A parented create without an explicit anchor forks at the parent's
  ;; CURRENT head (the Zig forkSession default) with a frozen watermark.
  ;; A session that is itself a fresh fork has no turns of its own: its head
  ;; is the ancestor turn it forked at. A child forked off it anchors there —
  ;; a turn of the session's COMPOSED tree, which is what the composed fold
  ;; walks. An immediate-parent-only check refused it, and that is how a
  ;; channel lane could not be opened in a thread whose room had just been
  ;; forked off the channel: "anchor turn turn-... is not a turn of session
  ;; chat-100-t42's parent" — the room IS the parent, and its head is the
  ;; channel's turn.
  (seed-user-inputs "fork-root" '("one" "two"))
  (let* ((head (nlk:session-head-turn-id "fork-root"))
         (mid (nlk:create-session :id "fork-mid" :parent "fork-root"))
         (leaf (nlk:create-session :id "fork-leaf" :parent "fork-mid")))
    (is (stringp head) "the parent has a head to anchor at")
    (is (equal head mid.anchor-turn-id))
    (is (integerp mid.anchor-log-position))
    (is (equal head (nlk:session-head-turn-id "fork-mid")))
    (is (equal head leaf.anchor-turn-id))
    (is (nlk:session-known-turn-p "fork-leaf" head))
    (is (equal '("fork-root" "fork-mid")
               (mapcar #'first (nlk:session-lineage "fork-leaf"))))))

(deftest create-session-keeps-a-stated-time (with-temp-store ())
  ;; A session recorded from another log keeps its own birth time: the
  ;; fact's created-at and its envelope both carry the stated clock.
  (let* ((then "2026-06-23T15:05:03.000Z")
         (session (nlk:create-session :id "old" :cwd "." :occurred-at then))
         (event (first (nlk:page-events :session-id "old"))))
    (is (equal then (nlk:durable-session-created-at session)) "created-at is the stated time")
    (is (equal then event.occurred-at) "and so is the envelope's clock")
    (is (equal then (nlk:durable-session-created-at (nlk:find-session "old"))))
    (is (signals-error type-error (nlk:create-session :id "bad" :cwd "." :occurred-at "then")))
    (is (null (nlk:session-exists-p "bad")) "and nothing was written")))

(deftest list-sessions-derives-each-entry-from-the-log (with-temp-store ())
  ;; The directory against its rules: the newest non-empty text title wins,
  ;; a newest title that is not usable falls to the first prompt's first line
  ;; bounded to 64 characters (never to an older title), a session with no
  ;; input titles as the empty string; the count is prompts plus answers;
  ;; activity is the newest row's clock; SESSION-TITLE reads the same rule. And the directory an open fills from
  ;; the log is the one the appends kept.
  (flet ((entry (id)
           (find id (nlk:list-sessions)
                 :key (lambda (row) (getf row :id)) :test #'equal)))
    (seed-completed-turn "ls-named" (format nil "first line~%second line") "answer")
    (nle:set-session-title "ls-named" "Old name")
    (nle:set-session-title "ls-named" "New name")
    (seed-user-inputs "ls-long" (list (make-string 100 :initial-element #\x)))
    (seed-user-inputs "ls-lines" (list (format nil "top line~%rest") "later prompt"))
    (nle:set-session-title "ls-lines" "Stale name")
    (nle:set-session-title "ls-lines" "")
    (seed-user-inputs "ls-typed" '("typed prompt"))
    (nlk:record-event "ls-typed" nlk::+kind-session-title+ (list :title 42)
                      :aggregate-type "session" :aggregate-id "ls-typed" :subject-id "ls-typed")
    (nlk:create-session :id "ls-silent" :cwd "/tmp/ls" :parent "ls-named")
    (is (equal "New name" (getf (entry "ls-named") :title)) "the newest name")
    (is (= 2 (getf (entry "ls-named") :message-count)) "a prompt and its answer")
    (is (equal (make-string 64 :initial-element #\x) (getf (entry "ls-long") :title)))
    (is-shape (entry "ls-lines") (:title "top line") (:message-count = 2))
    (is (equal "typed prompt" (getf (entry "ls-typed") :title)))
    (is-present (silent (entry "ls-silent")) "a session with no input is listed"
      (is-shape silent (:title "") (:message-count = 0) (:parent "ls-named")
        (:working-directory "/tmp/ls")))
    (is (equal (nlk:event-occurred-at (car (last (session-events "ls-named"))))
               (getf (entry "ls-named") :updated-at)))
    (is (equal '("New name" "top line" "" "")
               (mapcar #'nlk::session-title '("ls-named" "ls-lines" "ls-silent" "ls-none"))))
    (let ((kept (nlk:list-sessions)))
      (sqlite:execute-non-query (nlk::require-open-store) "delete from session_directory")
      (nlk::apply-schema (nlk::require-open-store))
      (is (equal kept (nlk:list-sessions)) "the fill derives what the appends kept"))))

(deftest a-prompt-taken-back-from-an-empty-session-names-nothing (with-temp-store ())
  ;; A retract back to the root takes the session's only prompt off its chain:
  ;; the directory forgets the name it gave, and the next prompt names the
  ;; session. A rewind to the root keeps the prompt that was answered. The
  ;; fill derives the same names from the log.
  (flet ((ask (id prompt)
           (nlk:complete-turn (nlk:admit-turn id (nlk::make-durable-id "command") prompt) nil)))
    (dolist (id '("tb-retract" "tb-rewind")) (nlk:create-session :id id :cwd "/tmp/tb"))
    (ask "tb-retract" "taken back")
    (nlk::move-session-head "tb-retract" nil :reason nlk:+head-move-reason-retract+)
    (is (equal "" (nlk::session-title "tb-retract")) "the taken-back prompt names nothing")
    (ask "tb-retract" "the real ask")
    (ask "tb-rewind" "answered once")
    (nlk::move-session-head "tb-rewind" nil)
    (is (equal '("the real ask" "answered once")
               (mapcar #'nlk::session-title '("tb-retract" "tb-rewind"))))
    (let ((kept (nlk:list-sessions)))
      (sqlite:execute-non-query (nlk::require-open-store) "delete from session_directory")
      (nlk::apply-schema (nlk::require-open-store))
      (is (equal kept (nlk:list-sessions)) "the fill derives what the appends kept"))))

(deftest session-source-the-directory-keeps-is-what-session-source-kind-answers
    (with-temp-store ())
  ;; SOURCE is the invocation source kind of a session's first turn, a
  ;; recorded exchange's as much as an admitted one's; a later turn by another
  ;; road leaves it, and a session no turn began has none. The listing's
  ;; column and SESSION-SOURCE-KIND agree on every session, whether the
  ;; trigger wrote the row, the fill derived it, or an open rebuilt a
  ;; directory written before the column.
  (let ((ids '("ss-room" "ss-seat" "ss-inline" "ss-filed" "ss-shell" "ss-quiet")))
    (flet ((admit (id source &aux (command (nlk::make-durable-id "command")))
             (nlk:complete-turn (nlk:admit-turn id command "ask"
                                                :invocation (nlk:default-invocation
                                                             "turn.start" command :source source))
                                nil))
           (column ()
             (mapcar (lambda (id)
                       (getf (find id (nlk:list-sessions) :key (lambda (row) (getf row :id)) :test #'equal)
                             :source))
                     ids)))
      (nlk:create-session :id "ss-room" :cwd (namestring (fixture-workspace)))
      (admit "ss-room" "channel")
      (admit "ss-room" "gateway")
      (nlk:create-session :id "ss-seat" :cwd (namestring (fixture-workspace))
                          :parent "ss-room" :detached t)
      (admit "ss-seat" "team")
      ;; A recorded exchange runs no turn: it is recorded whole, under the
      ;; source its recorder names; one that names none is the kernel's.
      (dolist (id '("ss-inline" "ss-filed"))
        (nlk:create-session :id id :cwd (namestring (fixture-workspace)) :parent "ss-room" :detached t))
      (nlk:record-exchange-turn "ss-inline" "Task: how many" "3" :source "team")
      (nlk:record-exchange-turn "ss-filed" "q" "a")
      (nlk:standby-session :id "ss-shell" :cwd (namestring (fixture-workspace)))
      (admit "ss-shell" "gateway")
      (nlk:create-session :id "ss-quiet" :cwd (namestring (fixture-workspace)))
      (is (equal '("channel" "team" "team" "in_process" "gateway" nil) (column)) "the first turn's, none before one")
      (is (equal (column) (mapcar #'nlk:session-source-kind ids)) "the column is what SESSION-SOURCE-KIND answers")
      (let ((handle (nlk::require-open-store)))
        (sqlite:execute-non-query handle "delete from session_directory")
        (nlk::apply-schema handle)
        (is (equal (column) (mapcar #'nlk:session-source-kind ids)) "and so is the fill's")
        ;; A directory an older build wrote: no SOURCE, and a trigger that
        ;; never writes one.
        (sqlite:execute-non-query handle "drop trigger session_directory_follows_the_log")
        (sqlite:execute-non-query handle "create trigger session_directory_follows_the_log
                                            after insert on events begin select 1; end")
        (sqlite:execute-non-query handle "drop table session_directory")
        (sqlite:execute-non-query handle "create table session_directory (
                                            session_id text primary key, created_at text, cwd text,
                                            parent, anchor_turn_id, title text, prompt text,
                                            updated_at text not null,
                                            message_count integer not null default 0) without rowid")
        (nlk::apply-schema handle)
        (is (search "source" (sqlite:execute-single
                              handle "select sql from sqlite_master where name = 'session_directory'")) "the open rebuilds it with the column")
        (is (equal '("channel" "team" "team" "in_process" "gateway" nil) (column)) "filled again from the log")
        (admit "ss-quiet" "cron")
        (is (equal "cron" (nlk:session-source-kind "ss-quiet")))
        (is (equal (column) (mapcar #'nlk:session-source-kind ids)) "and the trigger with it keeps it")))))

(deftest the-directory-keeps-the-newest-turn-and-the-mark-on-its-answer (with-temp-store ())
  ;; A session's newest turn and the fact that began or ended it, and what its
  ;; answer waits on: the question its turn marked it with, or "" for the
  ;; answer's own last line, once that turn completed -- never while it runs,
  ;; after it failed or was stopped, or once a later turn began. The fill
  ;; derives the same from the log, and an open rebuilds a directory written
  ;; before the columns.
  (let ((ids '("dw-asks" "dw-line" "dw-on" "dw-failed" "dw-marked-failed" "dw-stopped"
               "dw-running" "dw-quiet")))
    (flet ((admit (id) (nlk:admit-turn id (nlk::make-durable-id "command") "ask"))
           (mark (id turn &optional question)
             (nlk:record-event id nlk::+kind-session-waits+ (and question (list :question question))
                               :aggregate-type "session" :aggregate-id id :subject-id id
                               :turn-id (nlk:durable-turn-turn-id turn)))
           (column ()
             (mapcar (lambda (id &aux (row (find id (nlk:list-sessions)
                                                 :key (lambda (row) (getf row :id)) :test #'equal)))
                       (list (getf row :turn-kind) (getf row :waits)))
                     ids)))
      (dolist (id ids) (nlk:create-session :id id :cwd (namestring (fixture-workspace))))
      (let ((turn (admit "dw-asks"))) (mark "dw-asks" turn "Push the pin?") (nlk:complete-turn turn nil))
      (let ((turn (admit "dw-line"))) (mark "dw-line" turn) (nlk:complete-turn turn nil))
      (let ((turn (admit "dw-on"))) (mark "dw-on" turn "Ship it?") (nlk:complete-turn turn nil))
      (nlk:complete-turn (admit "dw-on") nil)
      (nlk:fail-turn (admit "dw-failed") (make-condition 'simple-error :format-control "boom"))
      (let ((turn (admit "dw-marked-failed")))
        (mark "dw-marked-failed" turn "Retry?")
        (nlk:fail-turn turn (make-condition 'simple-error :format-control "boom")))
      (nlk:cancel-turn (admit "dw-stopped") "stopped by the operator")
      (mark "dw-running" (admit "dw-running") "Which one?")
      (let ((kept '(("turn.completed" "Push the pin?") ("turn.completed" "") ("turn.completed" nil)
                    ("turn.failed" nil) ("turn.failed" nil) ("turn.cancelled" nil)
                    ("turn.started" nil) (nil nil)))
            (handle (nlk::require-open-store)))
        (is (equal kept (column)) "each session's newest turn, and the question its answer waits on")
        (is-table (id said)
          (equal said (nlk:session-waits id))
          ("dw-asks" "Push the pin?" "SESSION-WAITS reads the newest mark")
          ("dw-running" "Which one?" "a running turn's too, beside the turn that put it")
          ("dw-failed" nil "and none where no turn marked one"))
        (sqlite:execute-non-query handle "delete from session_directory")
        (nlk::apply-schema handle)
        (is (equal kept (column)) "the fill derives the same from the log")
        ;; A directory an older build wrote: no turn and no mark, and a
        ;; trigger that keeps neither.
        (sqlite:execute-non-query handle "drop trigger session_directory_follows_the_log")
        (sqlite:execute-non-query handle "create trigger session_directory_follows_the_log
                                            after insert on events begin select 1; end")
        (sqlite:execute-non-query handle "drop table session_directory")
        (sqlite:execute-non-query handle "create table session_directory (
                                            session_id text primary key, created_at text, cwd text,
                                            parent, anchor_turn_id, title text, prompt text,
                                            updated_at text not null,
                                            message_count integer not null default 0,
                                            source text) without rowid")
        (nlk::apply-schema handle)
        (is (search "waits_turn" (sqlite:execute-single
                                  handle "select sql from sqlite_master where name = 'session_directory'")) "the open rebuilds it with the columns")
        (is (equal kept (column)) "filled again from the log")
        (let ((turn (admit "dw-quiet")))
          (mark "dw-quiet" turn "Merge?")
          (nlk:complete-turn turn nil))
        (is (equal '("turn.completed" "Merge?") (car (last (column)))) "and the trigger with it keeps them")))))

(defun %tree-sessions (specs)
  "Directory rows as SESSION-TREE-ORDER reads them, one per (ID PARENT DAY) of SPECS,
last active at midnight on DAY of January 2026."
  (loop for (id parent day) in specs
        collect (list :id id :parent parent
                      :updated-at (format nil "2026-01-~2,'0dT00:00:00.000Z" day))))

(defun %tree-shape (rows)
  "The tree walk's ROWS as (ID DEPTH FORK LANES)."
  (mapcar (lambda (row) (list (getf row :id) (getf row :depth) (getf row :fork) (getf row :lanes)))
          rows))

(deftest session-tree-order-hangs-forks-under-their-session ()
  ;; The /sessions listing in the fork tree a child session makes: a session
  ;; created from another — /fork, or one a cell spawned off a turn —
  ;; hangs right under its parent one lane deeper, carrying the same
  ;; DEPTH/LANES/FORK shape the checkpoint tree does, so the /tree picker's
  ;; glyph renderer draws this dialog too. Roots keep the directory's own
  ;; order (newest activity first), a branch sorts by the newest row
  ;; anywhere in it, a parent the listing cut leaves its child a root, and
  ;; no row is lost.
  (flet ((shape (sessions) (%tree-shape (nlk:session-tree-order sessions)))
         (drawn (rows)
           (mapcar (lambda (row)
                     (format nil "~a~a" (nle::tree-lane-prefix (getf row :fork) (getf row :lanes))
                             (getf row :id)))
                   rows)))
    (let ((family (%tree-sessions '(("a" nil 1) ("b" "a" 2) ("c" "a" 3) ("d" "c" 4)))))
      (is (equal '(("a" 0 nil ())
                   ("c" 1 t (t))
                   ("d" 2 t (t nil))
                   ("b" 1 t (nil)))
                 (shape family)))
      (is (equal '("a" "├ c" "│ ╰ d" "╰ b")
                 (drawn (nlk:session-tree-order (reverse family))))))
    (is (equal '(("a" 0 nil ())
                 ("b" 1 t (nil))
                 ("z" 0 nil ()))
               (shape (%tree-sessions '(("z" nil 5) ("a" nil 1) ("b" "a" 9))))))
    (is (equal '(("c" 0 nil ()) ("b" 0 nil ()))
               (shape (%tree-sessions '(("b" "a" 2) ("c" "a" 3))))))
    (is (= 2 (length (nlk:session-tree-order (%tree-sessions '(("self" "self" 1) ("a" nil 2)))))))))

(deftest session-tree-order-folds-a-collapsed-branch ()
  ;; The sessions picker's fold on the shared walk: a collapsed session
  ;; keeps its forks — and their branches — out of the listing, a row a
  ;; collapsed parent hides never resurfaces at the root, the branch still
  ;; ranks by its whole shape, and an all-open fold is exactly the listing
  ;; the walk drew before folds existed.
  (flet ((walk (sessions open)
           (nlk:session-tree-order
            sessions
            :expanded (lambda (id) (member id open :test #'equal)))))
    (let ((family (%tree-sessions '(("a" nil 1) ("b" "a" 2) ("c" "a" 3) ("d" "c" 8) ("z" nil 5)))))
      (is (equal (%tree-shape (nlk:session-tree-order family))
                 (%tree-shape (nlk:session-tree-order family
                                                      :expanded (constantly t)))))
      (is (equal '(("a" 0 nil ()) ("z" 0 nil ())) (%tree-shape (walk family '()))))
      (is (equal '(("a" 0 nil ()) ("c" 1 t (t)) ("b" 1 t (nil)) ("z" 0 nil ()))
                 (%tree-shape (walk family '("a")))))
      (is (equal '(("a" 0 nil ()) ("z" 0 nil ())) (%tree-shape (walk family '("c"))))))))

(deftest a-session-is-named-by-its-first-prompt-from-its-admission (with-temp-store ())
  ;; The directory's name for a session nobody named: the first line of text
  ;; of its first prompt, trimmed and bounded to 64 characters, from the
  ;; moment that prompt is admitted -- a page's sidebar and /sessions read it
  ;; while the first turn runs, not after it ends. A prompt that opens on
  ;; blank lines is named by its first line of text, and one with no word
  ;; leaves the name to the next prompt; a recap recorded behind the turn and
  ;; a later prompt leave the name be; a name given wins, and the empty one
  ;; hands the prompt's line back. The fill derives the same, and an open
  ;; rebuilds a directory an older trigger kept.
  (flet ((title (id) (nlk::session-title id)))
    (nlk:standby-session :id "fp-live" :cwd (namestring (fixture-workspace)))
    (let ((turn (nlk:admit-turn "fp-live" (nlk::make-durable-id "command")
                                (format nil "  Fix the flaky checkout test  ~%and say why"))))
      (is (equal "Fix the flaky checkout test" (title "fp-live")) "named while its first turn runs")
      (nlk:complete-turn turn nil))
    (nlk:record-exchange-turn "fp-live" "[experience] recap of the turn above" "Fixed it.")
    (seed-user-inputs "fp-live" '("a later prompt"))
    (is (equal "Fix the flaky checkout test" (title "fp-live")) "a recap and a later prompt leave it")
    (seed-user-inputs "fp-pasted" (list (format nil "~%~%   ~%~CTraceback (most recent call last):~%  File x"
                                                #\Tab)))
    (is (equal "Traceback (most recent call last):" (title "fp-pasted")) "the first line of text")
    (seed-user-inputs "fp-blank" (list (format nil "  ~%~%") "then words"))
    (is (equal "then words" (title "fp-blank")) "a prompt with no word leaves the name to the next")
    (nle:set-session-title "fp-live" "Checkout flake")
    (is (equal "Checkout flake" (title "fp-live")) "a name given wins")
    (nle:set-session-title "fp-live" "")
    (is (equal "Fix the flaky checkout test" (title "fp-live")) "the empty name hands it back")
    (let ((kept (nlk:list-sessions))
          (handle (nlk::require-open-store)))
      (sqlite:execute-non-query handle "delete from session_directory")
      (nlk::apply-schema handle)
      (is (equal kept (nlk:list-sessions)) "the fill derives what the appends kept")
      ;; What an older trigger left: the pasted log's blank first line as its name.
      (sqlite:execute-non-query
       handle "update session_directory set prompt = '' where session_id = 'fp-pasted'")
      (sqlite:execute-non-query handle "drop trigger session_directory_follows_the_log")
      (sqlite:execute-non-query handle "create trigger session_directory_follows_the_log
                                          after insert on events begin select 1; end")
      (nlk::apply-schema handle)
      (is (equal kept (nlk:list-sessions)) "an open rebuilds it from the log")
      (seed-user-inputs "fp-after" '("  kept by the trigger again"))
      (is (equal "kept by the trigger again" (title "fp-after")) "and the trigger with it"))))

(deftest create-session-carries-its-parents-model (with-temp-store ())
  ;; A child runs on what its parent runs on: provider, model and reasoning
  ;; effort, written in the transaction that creates it -- an anchored fork
  ;; and a detached seat alike. A parent on the gateway default leaves its
  ;; child there, and a child that picked its own while standing by keeps it.
  (nlk:create-session :id "pinned")
  (nlk:record-session-model-selection "pinned" :provider "p" :model "m" :effort "high")
  (seed-completed-turn "pinned" "hello" "hi")
  (nlk:create-session :id "fork" :parent "pinned")
  (nlk:create-session :id "seat" :parent "pinned" :detached t)
  (nlk:create-session :id "plain")
  (nlk:create-session :id "plain-child" :parent "plain")
  (nlk:standby-session :id "own" :parent "pinned")
  (nlk:record-session-model-selection "own" :provider "q" :model "n")
  (nlk:create-session :id "own" :parent "pinned")
  (flet ((pin (id)
           (multiple-value-bind (provider model) (nlk:session-model-selection id)
             (list provider model (nlk:session-model-effort id))))
         (picks (id)
           (nlk:events :session-id id :kind nlk::+kind-model-selected+ :as :count)))
    (is (equal '("p" "m" "high") (pin "fork")) "an anchored fork runs on its parent's provider, model and effort")
    (is (= 1 (picks "fork")) "the pin is one fact, written with the creation")
    (is (equal '("p" "m" "high") (pin "seat")) "a detached seat runs on its opener's")
    (is (equal '(nil nil nil) (pin "plain-child")) "a parent on the gateway default leaves its child there")
    (is (= 0 (picks "plain-child")) "writing no pick for it")
    (is (equal '("q" "n" nil) (pin "own")) "a child that picked its own while standing by keeps it")))
