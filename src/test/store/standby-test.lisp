;;;; standby-test.lisp --- a session id held in memory until its first block.
;;;;
;;;; The rule these tests hold: a session is durable when it has its first
;;;; block, and not before. Everything else a shell or a lane does to an id
;;;; it has not spoken in — mint it, name its model, fill in its identity —
;;;; leaves the log exactly as it was.

(in-package #:nodecode.test)

(nlk:access (fork nlk::durable-session))

(deftest a-standby-session-writes-nothing (with-temp-store ())
  (let ((id (nlk:standby-session :id "sb-quiet" :cwd ".")))
    (is (equal "sb-quiet" id) "the mint answers its id")
    (is (nlk:on-standby-p id) "the id stands by")
    (is (nlk:session-exists-p id) "and answers as a session this image serves")
    (is (null (nlk:find-session id)) "with no durable identity")
    (is (= 0 (nlk:event-count :session-id id)) "the log never heard of it")
    (is (notany (lambda (entry) (equal id (getf entry :id)))
                (nlk:list-sessions)))))

(deftest the-first-block-materializes-the-identity (with-temp-store ())
  (let ((id (nlk:standby-session :id "sb-block" :cwd "subdir")))
    (nlk:admit-turn id "sb-command-1" "hello")
    (is-shape id (nlk:session-exists-p is "the session is real after its block")
      (nlk:on-standby-p not "and no longer standing by")
      (event-kinds-for '("session.created" "turn.input_committed" "turn.started")))
    (is (search "subdir" (nlk:durable-session-cwd (nlk:find-session id))))))

(deftest a-configuration-fact-waits-for-the-first-block (with-temp-store ())
  (let ((id (nlk:standby-session :id "sb-held" :cwd ".")))
    (nlk:record-session-model-selection id :provider "p" :model "m")
    (is (= 0 (nlk:event-count :session-id id)))
    (nlk:admit-turn id "sb-command-2" "hello")
    (is (equal '("session.created" "session.model_selected"
                 "turn.input_committed" "turn.started")
               (event-kinds-for id)))
    (is (equal '("p" "m")
               (multiple-value-list (nlk:session-model-selection id))))))

(deftest a-held-selection-reads-back-while-standing-by (with-temp-store ())
  ;; the web opener pins the session its first
  ;; message opens before that message lands. Every surface names the held pin
  ;; meanwhile, reasoning and all, and the first block makes it the durable one.
  (let ((id (nlk:standby-session :id "sb-read" :cwd ".")))
    (nlk:record-session-model-selection
     id :provider "p" :model "m" :effort "high")
    (flet ((pin () (list (multiple-value-list (nlk:session-model-selection id))
                         (nlk:session-model-effort id))))
      (is (= 0 (nlk:event-count :session-id id)) "nothing is durable yet")
      (is (equal '(("p" "m") "high") (pin)) "the held pin reads back")
      (nlk:admit-turn id "sb-command-3" "hello")
      (is (equal '(("p" "m") "high") (pin)) "and reads the same once it is durable"))))

(deftest a-block-without-materialization-is-refused (with-temp-store ())
  (let ((id (nlk:standby-session :id "sb-refuse" :cwd ".")))
    (is (signals-error nlk:standby-block-refused
          (nlk::session-fact-gate id "session" "turn.tool_result" '() '())))
    (is (= 0 (nlk:event-count :session-id id)) "and nothing was written")))

(deftest materialization-brings-a-standby-parent-into-the-log (with-temp-store ())
  (let* ((room (nlk:standby-session :id "sb-room" :cwd "."))
         (lane (nlk:standby-session :id "sb-lane" :cwd "." :parent room)))
    (is (= 0 (nlk:event-count :session-id room)) "the room is quiet too")
    (nlk:admit-turn lane "sb-command-3" "hello")
    (is (nlk:session-exists-p room) "the parent is durable with the child")
    (is (equal room (nlk:durable-session-parent (nlk:find-session lane))))
    (is (equal '("session.created") (event-kinds-for room)))))

(deftest a-fork-mint-works-where-its-parent-works (with-temp-store ())
  ;; A mint naming a parent and no directory takes the parent's, durable or
  ;; standing by -- never the directory of the process that minted it -- and
  ;; a stated one still wins.
  (nlk:create-session :id "sb-far" :cwd "/elsewhere/project/")
  (nlk:standby-session :id "sb-far-room" :cwd "/elsewhere/room/")
  (nlk:standby-session :id "sb-far-fork" :parent "sb-far")
  (nlk:standby-session :id "sb-far-lane" :parent "sb-far-room")
  (nlk:standby-session :id "sb-far-stated" :cwd "/stated/" :parent "sb-far")
  (flet ((cwd (id) (nlk:standby-record-cwd (nlk:standby-session-record id))))
    (is (equal "/elsewhere/project/" (cwd "sb-far-fork")) "a durable parent's directory")
    (is (equal "/elsewhere/room/" (cwd "sb-far-lane")) "a standing-by parent's directory")
    (is (equal "/stated/" (cwd "sb-far-stated")) "a stated directory wins")))

(deftest a-fork-anchor-is-frozen-at-the-mint (with-temp-store ())
  (nlk:create-session :id "sb-root" :cwd ".")
  (nlk:record-exchange-turn "sb-root" "q1" "a1")
  (let* ((anchor-turn (nlk:session-head-turn-id "sb-root"))
         (anchor-position (nlk::session-log-watermark "sb-root")))
    (nlk:standby-session :id "sb-fork" :cwd "." :parent "sb-root")
    ;; the parent moves on while the child stands by
    (nlk:record-exchange-turn "sb-root" "q2" "a2")
    (nlk:materialize-standby-session "sb-fork")
    (let ((fork (nlk:find-session "sb-fork")))
      (is-shape fork (.anchor-turn-id anchor-turn) (.anchor-log-position = anchor-position)))))

(deftest create-session-completes-a-standby-mint (with-temp-store ())
  (nlk:standby-session :id "sb-complete" :cwd "subdir")
  (nlk:record-session-model-selection "sb-complete" :model "m")
  (let ((session (nlk:create-session :id "sb-complete" :command-id "sb-create-1")))
    (is (equal "sb-complete" (nlk:durable-session-id session)))
    (is (search "subdir" (nlk:durable-session-cwd session)))
    (is (equal '("session.created" "session.model_selected")
               (event-kinds-for "sb-complete")))
    (is (not (nlk:on-standby-p "sb-complete")))))

(deftest minting-a-durable-id-is-a-no-op (with-temp-store ())
  (nlk:create-session :id "sb-durable" :cwd ".")
  (is (equal "sb-durable" (nlk:standby-session :id "sb-durable" :cwd "/tmp")))
  (is (not (nlk:on-standby-p "sb-durable")))
  (is (= 1 (nlk:event-count :session-id "sb-durable"))))

(deftest a-fork-standing-by-composes-its-parent-from-its-mint (with-temp-store ())
  ;; It has no created fact yet, and is not the empty session it reads as: the
  ;; ancestry its mint froze is what a replay, the visible chain and a fork of
  ;; it read, and it stays what they read once its first prompt makes it durable.
  (nlk:create-session :id "sb-parent" :cwd ".")
  (let* ((one (nlk:record-exchange-turn "sb-parent" "q1" "a1"))
         (two (nlk:record-exchange-turn "sb-parent" "q2" "a2"))
         (frozen (nlk::session-log-watermark "sb-parent")))
    (nlk:standby-session :id "sb-old" :parent "sb-parent" :anchor-turn-id one)
    (is (equal `(("sb-parent" ,one ,frozen)) (nlk:session-lineage "sb-old")) "its parent, frozen at the mint")
    (is (equal one (nlk:session-head-turn-id "sb-old")) "its head is the turn it forks at")
    (let ((visible (nlk:session-visible-turn-set "sb-old")))
      (is (gethash one visible) "that turn is what it shows")
      (is (not (gethash two visible)) "and a later one is not"))
    (nlk:standby-session :id "sb-deep" :parent "sb-old" :anchor-turn-id one)
    (is (equal `(("sb-parent" ,one ,frozen) ("sb-old" ,one 0))
               (nlk:session-lineage "sb-deep")) "a fork of it hangs under it, root first")
    (nlk:admit-turn "sb-old" "sb-command-4" "q3")
    (is (equal `(("sb-parent" ,one ,frozen)) (nlk:session-lineage "sb-old")) "the same once it is durable")
    (is (gethash one (nlk:session-visible-turn-set "sb-old")) "and it still shows that turn")))

(deftest a-fork-of-a-standing-by-session-anchors-only-at-a-turn-it-carries (with-temp-store ())
  (nlk:create-session :id "sb-root" :cwd ".")
  (nlk:create-session :id "sb-elsewhere" :cwd ".")
  (let ((carried (nlk:record-exchange-turn "sb-root" "q" "a"))
        (elsewhere (nlk:record-exchange-turn "sb-elsewhere" "q" "a")))
    (nlk:standby-session :id "sb-fork" :parent "sb-root" :anchor-turn-id carried)
    (nlk:standby-session :id "sb-plain" :cwd ".")
    (is (equal "sb-grandchild"
               (nlk:standby-session :id "sb-grandchild" :parent "sb-fork" :anchor-turn-id carried)) "a turn the fork carries")
    (is (signals-error nlk:invalid-session-anchor
          (nlk:standby-session :id "sb-stranger" :parent "sb-fork" :anchor-turn-id elsewhere)) "not one of another session's")
    (is (signals-error nlk:invalid-session-anchor
          (nlk:standby-session :id "sb-child" :parent "sb-plain" :anchor-turn-id carried)) "nor one of a session that carries none")))

(deftest a-head-move-makes-a-fork-standing-by-durable (with-temp-store ())
  ;; The turns a fork carries can be rewound before its first prompt: the move
  ;; is its first block, so its identity lands with it.
  (nlk:create-session :id "sb-mover" :cwd ".")
  (let ((one (nlk:record-exchange-turn "sb-mover" "q1" "a1")))
    (nlk:record-exchange-turn "sb-mover" "q2" "a2")
    (nlk:standby-session :id "sb-moved" :parent "sb-mover")
    (is (nlk:on-standby-p "sb-moved") "minted, standing by")
    (nlk:move-session-head "sb-moved" one)
    (is (not (nlk:on-standby-p "sb-moved")) "durable with the move")
    (is (equal '("session.created" "session.head_moved") (event-kinds-for "sb-moved")))
    (is (equal one (nlk:session-head-turn-id "sb-moved")) "at the turn it was moved to")))

(deftest an-eviction-makes-a-fork-standing-by-durable (with-temp-store ())
  ;; The context a fork carries can be cut before its first prompt as its
  ;; turns can be rewound: the cut is its first block.
  (nlk:create-session :id "sb-heavy" :cwd ".")
  (nlk:record-exchange-turn "sb-heavy" "first" "alpha")
  (nlk:record-exchange-turn "sb-heavy" "second" "beta")
  (nlk:standby-session :id "sb-cut" :parent "sb-heavy")
  (is (nle::evict-session-context "sb-cut" :percent 50) "a plan over the turns it carries")
  (is (not (nlk:on-standby-p "sb-cut")) "durable with the cut")
  (is (equal '("session.created" "session.context.evicted") (event-kinds-for "sb-cut"))))
