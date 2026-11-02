;;;; sessions-test.lisp --- a session whole: deleted, exported, imported
;;;; (store/sessions.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   W1. A delete takes the session and every session under it out of each
;;;;       table that holds a session's rows, in one transaction; another
;;;;       session stays whole and every one of its rows still verifies.
;;;;   W2. A turn open anywhere in that tree refuses the delete and the export,
;;;;       and nothing is written.
;;;;   W3. A session exported and imported into another store reads as it did:
;;;;       its history, its fork's composed history, its timeline, its standing
;;;;       sections, its KV and its directory row -- every position a fact or a
;;;;       KV row names carried to where its row landed.
;;;;   W4. An exported fork brings the sessions it composes; a store that holds
;;;;       them already keeps its own and the fork composes them in place; a
;;;;       session the store holds refuses the whole file.
;;;;   W5. A prune picks the sessions whose tree was last active before its
;;;;       cut, begun by one source or holding no message when asked,
;;;;       oldest first, a session under a pick never named again; a turn
;;;;       open in a tree keeps it and is named apart.

(in-package #:nodecode.test)

(defun seed-transfer-sessions ()
  "S-A with three turns -- the third on a branch after a rewind to the first
-- a tool call, a KV key rewound with it and a standing section; S-B forked
from its third turn with one of its own; S-C spawned from it detached, with
one. => the turn id S-B forks at."
  (let ((first (seed-completed-turn "s-a" "first" "one"
                                    :tools '(("c1" "eval" "{\"form\":\"(+ 1 2)\"}" "3")))))
    (nlk:session-state-put "s-a" "k" "1")
    (nlk:set-harness-section "s-a" "soul" "be kind")
    (seed-completed-turn "s-a" "second" "two")
    (nlk:session-state-put "s-a" "k" "2")
    (nlk::navigate-session-head "s-a" (nlk:durable-turn-turn-id first))
    (let ((third (nlk:durable-turn-turn-id (seed-completed-turn "s-a" "third" "three"))))
      (nlk:create-session :id "s-b" :cwd (fixture-workspace) :parent "s-a" :anchor-turn-id third)
      (seed-completed-turn "s-b" "fork" "forked")
      (nlk:create-session :id "s-c" :cwd (fixture-workspace) :parent "s-a" :detached t)
      (seed-completed-turn "s-c" "seat" "seated")
      third)))

(defun session-reading (session-id)
  "What SESSION-ID reads as, in terms a second store must repeat: its
history, its timeline, the sections it stands under, its KV key, its
directory row. A tool result's [seq N] is a recall handle into the store
that reads it, so it is read without its number."
  (list (mapcar (lambda (message)
                  (ppcre:regex-replace-all "\\[seq \\d+" (nlk:encode-json-object message) "[seq"))
                (wire-fold session-id))
        (nlk:encode-json-object (nle::session-timeline-json session-id))
        (nlk::standing-harness-sections session-id)
        (nlk:session-state-get session-id "k")
        (find session-id (nlk:list-sessions) :key (lambda (entry) (getf entry :id)) :test #'equal)))

(defun session-rows-by-table (ids)
  "How many rows each table that holds a session's facts has for IDS."
  (loop for table in nlk::+session-tables+
        collect (nlk::with-store-handle (handle)
                  (apply #'sqlite:execute-single handle
                         (format nil "select count(*) from ~a where session_id in ~a"
                                 table (nlk::in-list ids))
                         ids))))

(defun positions-stand-on-own-rows-p (session-id)
  "Whether every position SESSION-ID's KV rows and head moves name is one of
its own rows' -- true where they were written, and after an import only when
each was carried to where its row landed."
  (let ((own (nlk:events :session-id session-id :as :rows :columns '("log_position"))))
    (flet ((own-p (position) (member position own :key #'first)))
      (and (every #'own-p (nlk::with-store-handle (handle)
                            (mapcar #'first (nlk::sql-rows handle "select log_seq from session_state_events
                                                                   where session_id = ?"
                                                           session-id))))
           (every (lambda (payload) (own-p (gethash "source-watermark" payload)))
                  (nlk:events :session-id session-id :kind nlk::+kind-head-moved+ :as :payloads))))))

(deftest delete-a-session-takes-its-tree-from-every-table-and-leaves-others-whole (with-temp-store ())
  (seed-transfer-sessions)
  (seed-completed-turn "s-other" "keep me" "kept")
  (nlk:session-state-put "s-other" "k" "9")
  (nlk::allocate-gateway-sequence "s-a")
  (let* ((tree '("s-a" "s-b" "s-c"))
         (before (session-rows-by-table tree))
         (other (session-reading "s-other"))
         (plan (nlk:delete-session "s-a" :dry-run t)))
    (is-shape plan (:sessions tree "the session first, then what hangs under it")
      (:turns = 5 "three turns of its own and one each under it") (:events = (first before)))
    (is (equal before (session-rows-by-table tree)) "a dry run writes nothing")
    (is (equal plan (nlk:delete-session "s-a")))
    (is (every #'zerop (session-rows-by-table tree)) "no table keeps a row of the tree")
    (is (notany #'nlk:session-exists-p tree))
    (is (eq :miss (nth-value 1 (nlk:committed-events-after "s-a" 0))) "the memory of its commits went too")
    (is (equal '("s-other") (mapcar (lambda (entry) (getf entry :id)) (nlk:list-sessions))))
    (is (equal other (session-reading "s-other")) "another session reads as it did")
    (is (nlk:events :session-id "s-other") "and every row of it still verifies")
    (signals-error nlk:unknown-session (nlk:delete-session "s-a"))))

(deftest delete-a-session-refuses-a-running-turn-anywhere-in-its-tree (with-temp-store ())
  (seed-transfer-sessions)
  (nlk:admit-turn "s-c" (nlk::make-durable-id "command") "still going")
  (let ((before (session-rows-by-table '("s-a" "s-b" "s-c"))))
    (is (equal '("s-c") (nlk::session-running-sessions
                         (signals-error nlk:session-running (nlk:delete-session "s-a" :dry-run t)))))
    (signals-error nlk:session-running (nlk:delete-session "s-a"))
    (signals-error nlk:session-running (nlk:export-sessions "s-a"))
    (is (equal before (session-rows-by-table '("s-a" "s-b" "s-c"))) "nothing was written")
    (is (equal '("s-b") (getf (nlk:delete-session "s-b") :sessions)) "a tree without it goes")))

(deftest a-session-file-imported-into-another-store-reads-as-it-did (let ((file nil) (readings nil)))
  (with-temp-store ()
    (seed-transfer-sessions)
    (setf readings (mapcar #'session-reading '("s-a" "s-b" "s-c"))
          file (nlk:export-sessions "s-a"))
    (is (every #'positions-stand-on-own-rows-p '("s-a" "s-b"))))
  (with-temp-store ()
    ;; Rows of its own first, so every imported row lands at another position.
    (seed-completed-turn "s-z" "earlier" "here first")
    (seed-completed-turn "s-z" "again" "still here")
    (let ((answer (nlk:import-sessions file)))
      (is-shape answer (:sessions '("s-a" "s-b" "s-c")) (:kept null))
      (is (plusp (getf answer :events))))
    ;; History, the fork's composed history, timeline, sections, KV, directory.
    (is (equal readings (mapcar #'session-reading '("s-a" "s-b" "s-c"))))
    (is (every #'positions-stand-on-own-rows-p '("s-a" "s-b")) "every named position was carried")
    (is (equal '("s-a" "s-b" "s-c")
               (nlk::sessions-present-sessions
                (signals-error nlk:sessions-present (nlk:import-sessions file)))))
    (signals-error nlk:session-file-invalid
      (nlk:import-sessions (sb-ext:string-to-octets (format nil "{\"events\":{}}~%"))))))

(deftest a-session-file-of-a-fork-brings-what-it-composes-or-composes-it-in-place
    (let ((file nil) (reading nil)))
  (with-temp-store ()
    (seed-transfer-sessions)
    (setf reading (session-reading "s-b")
          file (nlk:export-sessions "s-b"))
    ;; Into the store it came from, its ancestor still there: kept, composed
    ;; in place at the positions it always had.
    (nlk:delete-session "s-b")
    (let ((answer (nlk:import-sessions file)))
      (is-shape answer (:sessions '("s-b")) (:kept '("s-a"))))
    (is (equal reading (session-reading "s-b"))))
  (with-temp-store ()
    (seed-completed-turn "s-z" "earlier" "here first")
    (is (equal '("s-b" "s-a") (getf (nlk:import-sessions file) :sessions)) "the ancestor comes along")
    (is (equal reading (session-reading "s-b")))
    (is (not (nlk:session-exists-p "s-c")) "a sibling does not")))

;;; --- W5: prune ------------------------------------------------------------------

(defun seed-old-session (id day &key parent (source "in_process") (turns 1))
  "Session ID begun by SOURCE on DAY of January 2026, with TURNS exchanges
on that day, hung detached under PARENT when given. => ID."
  ;; Each exchange is recorded on that day under SOURCE: a session's source is
  ;; its first turn's (SESSION-SOURCE-KIND).
  (let ((at (format nil "2026-01-~2,'0dT09:00:00.000Z" day)))
    (nlk:create-session :id id :cwd (namestring (fixture-workspace))
                        :parent parent :detached (and parent t) :occurred-at at)
    (loop repeat turns
          do (nlk:record-exchange-turn id "old ask" "old answer" :occurred-at at :source source))
    id))

(deftest prune-picks-sessions-by-age-source-and-emptiness-with-what-hangs-under-them
    (with-temp-store ())
  (seed-old-session "p-empty" 1 :turns 0)
  (seed-old-session "p-team" 2 :source "team")
  (seed-old-session "p-cron" 3 :source "cron")
  (seed-old-session "p-cron-kid" 4 :parent "p-cron" :source "checkpoint")
  ;; An old session a fork used today keeps them both.
  (seed-old-session "p-stale" 5 :source "cron")
  (nlk:create-session :id "p-hot" :cwd (namestring (fixture-workspace)) :parent "p-stale" :detached t)
  (seed-completed-turn "p-hot" "today" "fresh")
  ;; An old fork under a session used today goes alone.
  (seed-completed-turn "p-live" "today" "fresh")
  (seed-old-session "p-fork" 6 :parent "p-live" :source "checkpoint")
  (let ((march (nlk:iso-universal "2026-03-01T00:00:00Z")))
    (flet ((picks (&rest keys) (apply #'nlk:prune-picks :now march keys)))
      ;; Every old tree, oldest first; a session under a pick goes with it
      ;; and is not named.
      (is (equal '("p-empty" "p-team" "p-cron" "p-fork") (picks :days 30)) "every old tree")
      (is (null (picks :days 90)) "none is ninety days old on the first of March")
      (is (equal '("p-team") (picks :days 30 :source "team")) "only what a team seated")
      (is (equal '("p-cron") (picks :days 30 :source "cron")) "a stale parent with a fresh fork stays")
      (is (equal '("p-cron-kid" "p-fork") (picks :days 30 :source "checkpoint")) "every old fork goes alone")
      (is (equal "team" (nlk:session-source-kind "p-team")) "the source is the reader's")
      (is (null (nlk:session-source-kind "p-empty")) "which names none for a session no turn began")
      (is (equal '("p-empty") (picks :days 30 :source "")) "the empty source is a session it names none for")
      (is (equal '("p-empty") (picks :days 30 :empty t)) "only sessions nothing was said in")
      (is (null (picks :days 30 :source "cron" :empty t)))))
  ;; What the picks name is what the delete takes: the kid goes with its parent.
  (is (equal '("p-cron" "p-cron-kid") (getf (nlk:delete-session "p-cron" :dry-run t) :sessions)))
  ;; A turn open anywhere in a tree keeps it, and says so; the rest still go.
  (seed-old-session "p-run" 7 :source "cron")
  (nlk:admit-turn "p-run" (nlk::make-durable-id "command") "still going")
  (multiple-value-bind (picks running)
      (nlk:prune-picks :days 30 :now (+ (get-universal-time) (* 400 86400)))
    (is (equal '("p-run") running) "a running session is kept, named apart")
    (is (not (member "p-run" picks :test #'equal)))
    (is (member "p-live" picks :test #'equal) "and a year on, everything else is old")))
