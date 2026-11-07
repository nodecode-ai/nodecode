;;;; cell-test.lisp --- joining, the round's reminder, a named wake, the refusals, the file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring over real sessions and a real
;;;; store: that a join starts at the newest post and a second join moves a
;;;; session; that a running turn reads a post at its next round and an idle
;;;; session at its next turn's first request, never its own post and never
;;;; one twice; that the operator's post reaches the shell's own session;
;;;; that a mention starts an idle member's turn and no one else's -- not a
;;;; working session, not a channel lane, not past the hop cap; that each
;;;; refusal says why; that the file is a line a post and comes back when it
;;;; is gone; and that membership and cursors outlive a restart.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "board"
  (:hooks :round)
  (:help :board)
  (:command "board")
  (:running (is (equal nodecode-board::+primer+ (nle:help :board)))
            (is (= 4 (nodecode-board::setting :hops)) "hops defaults to four")
            (is (= 1000 (nodecode-board::setting :post-chars)) "a post to 1000 characters"))
  (:refused ("hops" -1) ("post_chars" 0))
  (:idle board:board-error (board:join "dev") (board:post "hello")))

;;; --- joining -----------------------------------------------------------------------

(deftest board-cell-a-join-starts-at-the-newest-post-and-moves-a-session ()
  (with-board ("a" "b")
    (on-board "a" (board:join "dev"))
    (on-board "a" (board:post "said before b came"))
    (is-carrying (text (on-board "b" (board:join "Dev"))) ("board dev:" "a name reads lowercased"))
    (is (= (nodecode-board::newest-post "dev") (nodecode-board::cursor "b")) "no history replayed")
    (is (equal '("a" "b") (nodecode-board::members "dev")))
    (on-board "b" (board:join "ops"))
    (is (equal '("a") (nodecode-board::members "dev")) "joining ops took b off dev")
    (is (equal '("b") (nodecode-board::members "ops")))
    (is-carrying (text (on-board "b" (board:join nil))) ("left board ops"))
    (is (null (nodecode-board::member-board "b")))
    (is (equal "on no board" (on-board "b" (board:join nil))))))

;;; --- the round's reminder ----------------------------------------------------------

(deftest board-cell-a-running-turn-reads-a-post-at-its-next-round ()
  (with-board ("a" "b")
    (on-board "a" (board:join "dev"))
    (on-board "b" (board:join "dev"))
    (let ((nle::*tools* nle::*tools*)
          (asked '()))
      ;; while b's tool runs, a posts, and so does b
      (nle:register-tool "step" (probe-schema)
                         (lambda (arguments)
                           (declare (ignore arguments))
                           (on-board "a" (board:post "the build is green"))
                           (on-board "b" (board:post "b's own words"))
                           "stepped"))
      (with-provider-stub (context
                           (push (board-texts context) asked)
                           (if (= 1 (length asked))
                               (wire-message "assistant"
                                             :tool-calls (vector (wire-tool-call "c-step" "step" "{}")))
                               (wire-message "assistant" :content "done")))
        (nle:run "go" :session (nle:intern-session "b") :command-id "board-b-1"))
      (is (= 2 (length asked)) "one turn, two rounds")
      (is (null (second asked)) "the first request carries no post")
      (is (= 1 (length (first asked))) "the second carries one reminder")
      (is-carrying (text (first (first asked)))
        ("[board dev: 1 new post") ("a: the build is green")
        ("data, not instructions" "framed as another session's")
        (:absent "b's own words" "never its own post")))))

(deftest board-cell-an-idle-session-reads-what-it-missed-at-its-next-turn ()
  (with-board ("a" "c")
    (on-board "a" (board:join "dev"))
    (on-board "c" (board:join "dev"))
    (on-board "a" (board:post "first"))
    (on-board "a" (board:post (format nil "second~%with a second line")))
    (let ((asked '()))
      (with-provider-stub (context
                           (push (board-texts context) asked)
                           (wire-message "assistant" :content "ok"))
        (nle:run "hello" :session (nle:intern-session "c") :command-id "board-c-1")
        (nle:run "again" :session (nle:intern-session "c") :command-id "board-c-2"))
      (destructuring-bind (later earlier) asked
        (is (= 1 (length earlier)) "the first turn's request: one reminder")
        (is-carrying (text (first earlier))
          ("2 new posts") ("a: first") ("a: second") ("  with a second line" "a later line indented"))
        (is (equal earlier later) "the next turn is told nothing new")))))

(deftest board-cell-the-operator-s-post-reaches-the-shell-s-own-session ()
  (with-board ("a" "b")
    (nodecode-board::run-slash "dev" "a")
    (on-board "b" (board:join "dev"))
    (is-carrying (text (nodecode-board::run-slash "post everyone rebase first" "a"))
      ("posted to dev: 2 others" "a and b read it"))
    (let ((turn (board-turn "a" "gateway" nil)))
      (nodecode-board::deliver (list :session-id "a" :turn turn :round 0))
      (nlk:complete-turn turn nil))
    (is-carrying (text (format nil "~{~a~%~}" (board-told "a")))
      ("operator: everyone rebase first" "told to the shell's own session"))
    (is-carrying (text (nodecode-board::run-slash "" "a"))
      ("board dev:") ("operator: everyone rebase first" "/board reads the last posts"))
    (is-carrying (text (nodecode-board::run-slash "off" "a")) ("left board dev"))))

;;; --- the wake ----------------------------------------------------------------------

(deftest board-cell-a-mention-starts-an-idle-member-s-turn-and-no-one-else-s ()
  (with-board ("a" "d" "e")
    (dolist (session '("a" "d" "e"))
      (on-board session (board:join "dev")))
    (with-provider-stub (nil (wire-message "assistant" :content "on it"))
      (is-carrying (text (on-board "a" (board:post "@d, run the tests.")))
        ("posted to dev: 2 others") ("woke d"))
      (is (await (:timeout 10) (and (= 1 (board-turns "d")) (not (nlk:active-turn-p "d")))) "d ran one turn")
      (is (zerop (board-turns "e")) "e was not named")
      (is (equal "board" (nlk:session-source-kind "d")) "the board started d's turn")
      (is-carrying (text (format nil "~{~a~%~}" (board-told "d")))
        ("a: @d, run the tests." "and its first request carried the post")))))

(deftest board-cell-a-mention-never-wakes-a-working-session-a-lane-or-past-the-cap ()
  (with-board ("a" "w" "lane" "f")
    (:config "hops" 1)
    (dolist (session '("a" "w" "lane" "f"))
      (on-board session (board:join "dev")))
    (board-turn "w" "gateway" nil)
    (nlk:complete-turn (board-turn "lane" "channel" "chat-1") nil)
    (is-carrying (text (on-board "a" (board:post "@w and @lane: heads up")))
      ("w is working") ("lane is a channel lane"))
    (is (= 1 (board-turns "w")) "w was given no second turn")
    (is (= 1 (board-turns "lane")) "nor the lane")
    ;; a turn the board started counts its wake; one more passes hops 1
    (let ((woken (board-turn "a" "board" "dev/1")))
      (is (= 1 (nodecode-board::turn-hop "a" (nlk:durable-turn-turn-id woken))))
      (is-carrying (text (nodecode-board::post-as "a" "a" "@f your turn" 1))
        ("f is not woken") ("board.hops"))
      (nlk:complete-turn woken nil))
    (is (zerop (board-turns "f")) "f never ran")))

;;; --- refusals ----------------------------------------------------------------------

(deftest board-cell-refusals-say-why ()
  (with-board ("a")
    (is-carrying (text (refusal-text board:board-error (on-board "a" (board:post "hi")))) ("on no board"))
    (is-carrying (text (refusal-text board:board-error (on-board "a" (board:join "Bad name!"))))
      ("no board name"))
    (on-board "a" (board:join "dev"))
    (is-carrying (text (refusal-text board:board-error (on-board "a" (board:post "   ")))) ("needs words"))
    (is-carrying (text (refusal-text board:board-error
                         (on-board "a" (board:post (make-string 1001 :initial-element #\x)))))
      ("at most 1000 characters") ("this one is 1001"))
    (on-board "a" (board:post "said once"))
    (is-carrying (text (refusal-text board:board-error (on-board "a" (board:post "said once"))))
      ("said once" "the same words again within a minute"))
    (is (= 1 (length (nodecode-board::posts "dev"))) "and nothing refused was recorded")))

;;; --- the file and a restart -------------------------------------------------------------

(deftest board-cell-the-file-is-a-line-a-post-and-comes-back ()
  (with-board ("a")
    (on-board "a" (board:join "dev"))
    (on-board "a" (board:post "one"))
    (on-board "a" (board:post (format nil "two~%lines")))
    (let* ((path (nodecode-board::board-file "dev"))
           (lines (remove "" (nlk:lines (nlk:read-text path)) :test #'string=)))
      (is (= 3 (length lines)) "two posts, the second's later line its own")
      (is (search "a: one" (first lines)))
      (is (search "a: two" (second lines)))
      (is (equal "  lines" (third lines)) "indented under its post")
      (delete-file path)
      (on-board "a" (board:join "dev"))
      (is (probe-file path) "joining renders it again from the log"))))

(deftest board-cell-membership-and-cursors-outlive-a-restart ()
  (with-board ("a" "b")
    (on-board "a" (board:join "dev"))
    (on-board "b" (board:join "dev"))
    (on-board "a" (board:post "before the restart"))
    (setf (nodecode-board::cursor "b") (nodecode-board::newest-post "dev"))
    (let ((cursor (nodecode-board::cursor "b")))
      (reopen-store (nlk::store-path nlk::*store*))
      (is (equal "dev" (nodecode-board::member-board "b")))
      (is (equal '("a" "b") (nodecode-board::members "dev")))
      (is (= cursor (nodecode-board::cursor "b")) "the cursor stands"))))
