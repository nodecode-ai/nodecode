;;;; cell-test.lisp --- lifecycle, open, what a node has left, the seat, watch, stop.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, over real node
;;;; sessions and a real ./verify: that OPEN makes the directory and starts N
;;;; nodes on one byte-identical prompt whose own slot command hands out
;;;; distinct slots; that a node's second turn opens under what its first
;;;; left, never under a fresh budget; that no operator input seats a team
;;;; and /team TEXT seats one standing team per session, puts every later
;;;; /team to the same nodes -- a steer of a working node's turn, a follow-up
;;;; to an idle one -- and refuses a shell with no session and a node's own;
;;;; that a spent node is never prompted again and a fresh one takes its seat
;;;; at the next /team, and that the standing clock re-bases at each /team
;;;; while the tokens do not; that /team alone reads the team and /team stop
;;;; stops it; that a node's definitions never reach the layer, and a
;;;; relative DIR is the calling session's; that WATCH answers the SCORES
;;;; maximum only once a run of ./verify stands behind it,
;;;; prompts an idle node a bounded number of times and never after SOLVED;
;;;; and that STOP ends every node's turn and leaves the directory as it was.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "team"
  (:config "nodes" 2)
  (:hooks 'nle:turn-budget 'nlk:record-definitions)
  (:help :team)
  (:command "team")
  (:running (is (equal nodecode-team::+primer+ (nle:help :team)))
            (is (= 2 (nodecode-team::setting :nodes)) "nodes is read")
            (is (= 1500000 (nodecode-team::setting :budget-tokens)) "and the defaults stand"))
  (:refused ("nodes" 0) ("nodes" 6) ("budget_tokens" 0))
  (:idle team:team-error (team:open "task" :score "/bin/true") (team:watch "/tmp/nowhere/")
         (team:stop "/tmp/nowhere/") (team:best "/tmp/nowhere/")))

;;; --- open --------------------------------------------------------------------------

(deftest team-cell-open-writes-the-directory-and-starts-n-identical-nodes ()
  (with-team-runtime (root scorer :nodes 2 :session "opener" :answer t)
    (let* ((nlk:*scribe-session-id* "opener")
           (answer (team:open "Make the number big." :score scorer))
           (dir (first (uiop:subdirectories (nlk:home "team/opener/"))))
           (nodes (and dir (team-nodes dir))))
      (is-present dir "the directory sits in the opener's team area, under the home"
        (is (search (namestring dir) answer) "and the answer names it")
        (is (await () (team-idle-p dir)) "both nodes ran their first turn")
        ;; the layout
        (dolist (name '("TASK.md" "LOG" "SCORES" "DISCONFIRMED" "ADOPTED" "best.lock"
                        "team.sexp" "verify" "score" "slots/" "best/"))
          (is (probe-file (merge-pathnames name dir)) name))
        (is (null (probe-file (merge-pathnames "SOLVED" dir))) "SOLVED is a node's to write")
        (is (equal '("Make the number big.") (team-lines dir "TASK.md")))
        ;; the nodes
        (is (= 2 (length nodes)))
        (is (equal "opener" (getf (nodecode-team::read-config dir) :origin)))
        (is (equal (list "team-opener-node-0" "team-opener-node-1") nodes) "each node is named team-<team>-node-<k>: team is the session it hangs under")
        (dolist (id nodes)
          (is (equal (namestring dir) (nlk:find-session-cwd id)) "its directory is the team's")
          (is (equal "opener" (nlk:durable-session-parent (nlk:find-session id))) "a node of the session that opened the team")
          (is (null (nlk:durable-session-anchor-turn-id (nlk:find-session id))) "detached: composing nothing of it, the seat begins fresh")
          (is (equal "team" (nlk:session-source-kind id)) "provenance: source kind team"))
        ;; one body, byte-identical, behind each node's own marker
        (let* ((first-input (first (team-inputs (first nodes))))
               (second-input (first (team-inputs (second nodes))))
               (body (subseq first-input (+ 3 (length (first nodes))))))
          (is (uiop:string-prefix-p (format nil "[~a] " (first nodes)) first-input) "the node's marker leads its first input")
          (is (uiop:string-prefix-p (format nil "[~a] " (second nodes)) second-input))
          (is (equal body (subseq second-input (+ 3 (length (second nodes))))) "and the body behind the marker is byte-identical")
          (is (uiop:string-prefix-p (format nil "Make the number big.~%~%You are one of 2 identical")
                                    body))
          ;; the paragraph is one paragraph
          (is (null (find #\Newline body :start (length "Make the number big.  "))))
          ;; the slot command the prompt itself gives: one mkdir, one winner
          (let* ((open (position #\` body))
                 (claim (subseq body (1+ open) (position #\` body :start (1+ open)))))
            (is (search "$(seq 0 1)" claim) "the slots are 0 to N-1")
            (is (equal "0" (team-sh dir claim)) "the first claim wins slot 0")
            (is (equal "1" (team-sh dir claim)) "the second a distinct one")
            (is (equal "" (team-sh dir claim)) "and the team is full")
            (is (probe-file (merge-pathnames "slots/slot-1/" dir))))))
      ;; a team of one reads the task alone
      (is (equal "solo task" (nodecode-team::node-prompt "solo task" 1))))))

(deftest team-cell-open-refuses-what-it-cannot-start ()
  (with-team-runtime (root scorer :nodes 1 :answer t)
    (let ((dir (team-dir root "t")))
      (is (search ":score" (refusal-text team:team-error
                             (team:open "task" :score (merge-pathnames "no-scorer" root) :dir dir))) "a scorer that is not there")
      (is (null (probe-file dir)) "a refused open makes nothing")
      (is (signals-error team:team-error (team:open "  " :score scorer :dir dir)) "no task")
      (team:open "task" :score scorer :dir dir)
      (is (await () (team-idle-p dir)))
      (is (search "already holds a team"
                  (refusal-text team:team-error (team:open "task" :score scorer :dir dir))))
      (let ((nlk:*scribe-session-id* (first (team-nodes dir))))
        (is (search "node cannot open"
                    (refusal-text team:team-error
                      (team:open "task" :score scorer :dir (team-dir root "nested")))))))))

;;; --- budget ------------------------------------------------------------------------

(deftest team-cell-a-node-prompted-again-runs-under-what-it-has-left ()
  (with-team-runtime (root scorer :nodes 1 :tokens 1000 :seconds 600)
    (let ((dir (team-dir root "t")))
      (with-budget-log (budgets)
        (team:open "task" :score scorer :dir dir)
        (is (await () (team-idle-p dir)))
        (let* ((id (first (team-nodes dir)))
               (config (nodecode-team::read-config dir))
               (opened (getf config :opened-at))
               (next (lambda (turn) (declare (ignore turn)) :next)))
          (is (= 1000 (getf (cdar budgets) :tokens)) "the first turn opened under the whole budget")
          (is (<= 590 (getf (cdar budgets) :seconds) 600) "and the team's clock")
          ;; cache reads are free, as the kernel bills them
          (seed-node-usage id :input 300 :output 100 :reasoning 50 :cached 9000)
          (is (equal '(:input 300 :output 100 :reasoning 50) (nodecode-team::node-spend id)))
          (is (= 550 (nodecode-team::node-left id config)))
          ;; prompted again, in the same session: the real ingress, the real meter
          (nodecode-team::submit-prompt id "again" "team-test-again" nil)
          (is (await () (= 2 (length budgets))) "the second turn reached the provider")
          (is (await () (team-idle-p dir)))
          (is (= 550 (getf (cdar budgets) :tokens)) "and opened under what was left, not 1000")
          ;; spent: the turn still opens, under a budget its first call exhausts
          (seed-node-usage id :output 600)
          (is (= -50 (nodecode-team::node-left id config)))
          (is (= 1 (getf (nodecode-team::budget-hook next (list :session-id id)) :tokens)))
          ;; the clock is the team's: it never restarts with a turn
          (is (= 10 (nth-value 1 (nodecode-team::node-left id config (+ opened 590)))))
          (is (= -100 (nth-value 1 (nodecode-team::node-left id config (+ opened 700)))))
          ;; every other session falls to NEXT
          (ensure-durable-session "team-lookalike")
          (is (eq :next (nodecode-team::budget-hook next (list :session-id "team-lookalike"))))
          (is (eq :next (nodecode-team::budget-hook next (list :session-id "s1")))))))))

;;; --- the seat ----------------------------------------------------------------------
;;; /team TEXT is the only way a standing team is seated: the first real runs
;;; seated one on every operator input, a status question included.

(deftest team-cell-an-operator-input-seats-no-team ()
  (with-team-runtime (root scorer :nodes 2 :session "opener" :answer t)
    (dolist (source '("gateway" "team" "cron" nil))
      (let ((admission (operator-submit "opener" "Make the build green." :source source)))
        (is (eq :started (nlk:active-input-admission-disposition admission)) "the session's own turn runs")
        (is (await () (settled-p "opener")))
        (is (null (probe-file (nodecode-team::standing-dir "opener"))) (format nil "source ~a seats nothing" source))
        (is (zerop (seated-count "opener")))))))

(deftest team-cell-slash-team-seats-the-team ()
  (with-team-runtime (root scorer :nodes 2 :session "opener" :answer t)
    (let* ((line (team-slash "opener" "  Make the build green.  "))
           (dir (nodecode-team::standing-dir "opener"))
           (nodes (team-nodes dir)))
      (is (search "2 nodes took it" line) "the composer line says so")
      (is (equal (list "team-opener-node-0" "team-opener-node-1") nodes) "team.nodes nodes, named under the session")
      (is (await () (settled-p "opener")) "every node ran its first turn")
      (is (zerop (count-session-events "opener" "turn.started")) "and the session itself took no turn")
      (is (equal "opener" (getf (nodecode-team::read-config dir) :origin)) "the team hangs under the operator's session")
      (is (equal '("Make the build green.") (team-lines dir "TASK.md")))
      ;; the same layout, unscored
      (dolist (name '("TASK.md" "LOG" "DISCONFIRMED" "ADOPTED" "best.lock" "best/" "team.sexp"))
        (is (probe-file (merge-pathnames name dir)) name))
      (dolist (name '("score" "verify" "SCORES" "slots/"))
        (is (null (probe-file (merge-pathnames name dir))) (format nil "no ~a: the team has no scorer" name)))
      (dolist (id nodes)
        (is (equal (namestring dir) (nlk:find-session-cwd id)) "its directory is the standing one")
        (is (equal "opener" (nlk:durable-session-parent (nlk:find-session id))) "a node of the operator's session")
        (is (null (nlk:durable-session-anchor-turn-id (nlk:find-session id))) "detached")
        (is (equal "team" (nlk:session-source-kind id)) "provenance: source kind team")
        (is (= 1 (length (team-inputs id)))))
      ;; the node's marker, the operator's own words, then one paragraph shared by all
      (let* ((first-input (first (team-inputs (first nodes))))
             (second-input (first (team-inputs (second nodes))))
             (body (subseq first-input (+ 3 (length (first nodes))))))
        (is (uiop:string-prefix-p (format nil "[~a] Make the build green.~%~%You are one of 2 identical"
                                          (first nodes))
                                  first-input))
        (is (uiop:string-prefix-p (format nil "[~a] Make the build green." (second nodes)) second-input))
        (is (equal body (subseq second-input (+ 3 (length (second nodes))))) "byte-identical behind the marker")
        (is (null (find #\Newline body :start (length "Make the build green.  "))) "one paragraph")
        (is (search (nlk:find-session-cwd "opener") body) "the operator's directory rides in the task")
        (is (search "no scorer" body))
        (is (search "nothing you define is kept past this image" body)))
      (is (search "best: none -- the team has no scorer" (team:best dir))))))

(deftest team-cell-a-second-slash-team-prompts-the-same-seats ()
  (with-team-runtime (root scorer :nodes 2 :session "opener" :answer t)
    (team-slash "opener" "first")
    (is (await () (settled-p "opener")))
    (team-slash "opener" "second")
    (let ((dir (nodecode-team::standing-dir "opener")))
      (is (await () (and (every (lambda (id) (= 2 (length (team-inputs id)))) (team-nodes dir))
                         (settled-p "opener"))) "each node took the second input")
      (is (= 2 (seated-count "opener")) "still team.nodes sessions")
      (dolist (id (team-nodes dir))
        (is (equal (format nil "[~a] second" id) (second (team-inputs id))) "the marker, then the operator's text")
        (is (= 2 (nodecode-team::node-turns id)) "an idle node took it as a follow-up")
        (is (null (steered-inputs id)) "nothing waits as a steer"))
      (is (= 1 (length (uiop:subdirectories (nlk:home "team/opener/")))) "one directory")
      (is (= 1 (length (directory (merge-pathnames "**/team.sexp" (nlk:home "team/"))))) "one team.sexp"))))

(deftest team-cell-slash-team-refuses-what-it-cannot-seat ()
  (with-team-runtime (root scorer :nodes 2 :answer t)
    (is (search "has no session" (refusal-text team:team-error (team-slash "ghost" "work"))) "an id no shell minted")
    (is (search "has no session" (refusal-text team:team-error (team-slash nil "work"))) "a shell with no session at all")
    ;; a shell that has not prompted yet stands by: /team makes it a session
    (nlk:standby-session :id "fresh" :cwd (namestring root))
    (is (search "no standing team yet" (team-slash "fresh" "")) "a read needs no session")
    (is (nlk:on-standby-p "fresh") "and makes none")
    (is (search "2 nodes took it" (team-slash "fresh" "work")) "the first /team seats")
    (is (nlk:session-exists-p "fresh") "and the standing-by shell is a session now")
    (is (equal "fresh" (nlk:durable-session-parent (nlk:find-session "team-fresh-node-0"))) "the nodes hang under it")
    (is (await () (settled-p "fresh")))
    (ensure-durable-session "opener")
    (team-slash "opener" "first")
    (is (await () (settled-p "opener")))
    (is (search "seats no team" (refusal-text team:team-error (team-slash "team-opener-node-0" "work"))) "a node's own shell")
    (is (zerop (seated-count "team-opener-node-0")))
    (is (= 1 (length (team-inputs "team-opener-node-0"))) "and the node took nothing")
    ;; the standing directory cannot be made: the refusal says so, nothing is minted
    (ensure-durable-session "blocked")
    (write-temp-file (nlk:home "team/blocked/standing") "not a directory")
    (is (search "was not seated" (refusal-text team:team-error (team-slash "blocked" "seat me"))))
    (is (zerop (seated-count "blocked")))))

(deftest team-cell-a-seated-nodes-first-turn-is-capped ()
  (with-team-runtime (root scorer :nodes 2 :tokens 1000 :seconds 600 :session "opener")
    (with-budget-log (budgets)
      (team-slash "opener" "capped")
      (is (await () (settled-p "opener")))
      (let ((nodes (team-nodes (nodecode-team::standing-dir "opener"))))
        (is (= 2 (length nodes)))
        (dolist (id nodes)
          (let ((budget (cdr (assoc id budgets :test #'equal))))
            (is (eql 1000 (getf budget :tokens)) "the first turn opened under the whole budget")
            (is (<= 590 (or (getf budget :seconds) 0) 600) "and the team's clock")))))))

;;; --- the bounds --------------------------------------------------------------------

(deftest team-cell-a-spent-node-is-replaced-at-the-next-slash-team-not-prompted ()
  (with-team-runtime (root scorer :nodes 2 :tokens 1000 :session "opener" :answer t)
    (team-slash "opener" "first")
    (is (await () (settled-p "opener")))
    (let ((dir (nodecode-team::standing-dir "opener"))
          (spent "team-opener-node-0")
          (kept "team-opener-node-1")
          (fresh "team-opener-node-2"))
      ;; node 0 spends past its lifetime cap
      (seed-node-usage spent :output 1000)
      (is (nodecode-team::node-spent-p spent (nodecode-team::read-config dir)))
      (let ((inputs (length (team-inputs spent)))
            (turns (nodecode-team::node-turns spent)))
        (is (= 2 (seated-count "opener")) "nothing mints between inputs")
        (team-slash "opener" "second")
        (is (await () (settled-p "opener")))
        (let* ((config (nodecode-team::read-config dir))
               (nodes (getf config :nodes)))
          (is (equal (list spent kept fresh) nodes) "the next free k takes the seat; the spent node stays listed")
          (is (= 2 (count-if-not (lambda (id) (nodecode-team::node-spent-p id config)) nodes)) "the seat is back at team.nodes")
          (is (= inputs (length (team-inputs spent))) "the spent node was not prompted")
          (is (= turns (nodecode-team::node-turns spent)) "and no turn of its opened")
          (is (equal config (nodecode-team::node-config spent)) "still guarded and capped by team.sexp")
          (is (equal (format nil "[~a] second" kept) (second (team-inputs kept))) "the usable node took the input")
          (is (= 1 (length (team-inputs fresh))))
          (is (uiop:string-prefix-p (format nil "[~a] second~%~%You are one of 2 identical" fresh)
                                    (first (team-inputs fresh))) "the fresh node carries the new input, with the paragraph a first input carries")
          (is (equal (namestring dir) (nlk:find-session-cwd fresh)) "from the standing directory")
          (is (equal "opener" (nlk:durable-session-parent (nlk:find-session fresh))) "under the operator's session")
          (is (null (nlk:durable-session-anchor-turn-id (nlk:find-session fresh))) "detached"))))))

(deftest team-cell-the-standing-clock-re-bases-at-each-slash-team ()
  (with-team-runtime (root scorer :nodes 1 :tokens 1000 :seconds 600 :session "opener")
    (with-budget-log (budgets)
      (team-slash "opener" "first")
      (is (await () (settled-p "opener")))
      (let* ((dir (nodecode-team::standing-dir "opener"))
             (id (first (team-nodes dir)))
             (config (nodecode-team::read-config dir))
             (aged (- (getf config :opened-at) 1000)))
        ;; the first input's window closed 1000 s ago, and 300 tokens are spent
        (setf (getf config :opened-at) aged)
        (nodecode-team::write-config dir config)
        (is (minusp (nth-value 1 (nodecode-team::node-left id config))) "its seconds are over")
        (seed-node-usage id :output 300)
        (team-slash "opener" "second")
        (is (await () (settled-p "opener")))
        (is (= 2 (count id budgets :key #'car :test #'equal)) "the same node's second turn opened")
        (let ((budget (cdr (assoc id budgets :test #'equal))))
          (is (<= 590 (or (getf budget :seconds) 0) 600) "under the full configured seconds: the clock is this input's")
          (is (eql 700 (getf budget :tokens)) "while the tokens stay the node's lifetime cap"))
        (is (< aged (getf (nodecode-team::read-config dir) :opened-at)) ":opened-at in team.sexp was re-based")))))

(deftest team-cell-slash-team-steers-each-working-node ()
  (with-team-runtime (root scorer :nodes 2 :session "opener")
    (let ((released nil))
      (with-provider-stub (nil (if released
                                   (wire-message "assistant" :content "ok")
                                   (park-until-cancelled)))
        (let ((dir (nodecode-team::standing-dir "opener")))
          (nlk:with-cleanup ((setf released t)
                             (ignore-errors (team:stop dir))
                             (await () (settled-p "opener")))
            (team-slash "opener" "first")
            (let ((nodes (team-nodes dir)))
              (is (await () (= 2 (count-if #'nlk:active-turn-p nodes))) "both nodes are working")
              (let ((working (mapcar #'nodecode-team::node-active-turn nodes)))
                (team-slash "opener" "change course")
                (loop for id in nodes
                      for turn in working
                      do (is (equal turn (nodecode-team::node-active-turn id)) "the node's turn runs on")
                         (is (equal (list (format nil "[~a] change course" id)) (steered-inputs id)) "and the input waits as a steer of it")
                         (is (= 1 (length (team-inputs id))) "never a follow-up")))
              ;; once the running turns end, each steer runs as its node's next turn
              (setf released t)
              (team:stop dir)
              (is (await () (settled-p "opener")))
              (dolist (id nodes)
                (is (equal (format nil "[~a] change course" id) (second (team-inputs id))))))))))))

(deftest team-cell-slash-team-alone-reads-the-team-and-stop-stops-it ()
  (with-team-runtime (root scorer :nodes 2 :session "opener")
    (let ((released nil))
      (with-provider-stub (nil (if released
                                   (wire-message "assistant" :content "ok")
                                   (park-until-cancelled)))
        (is (search "no standing team yet" (team-slash "opener" "")) "before the first /team")
        (is (search "no standing team to stop" (team-slash "opener" "stop")))
        (let ((dir (nodecode-team::standing-dir "opener")))
          (nlk:with-cleanup ((setf released t)
                             (ignore-errors (team:stop dir))
                             (await () (settled-p "opener")))
            (team-slash "opener" "work")
            (is (await () (= 2 (count-if #'nlk:active-turn-p (team-nodes dir)))) "both nodes are working")
            (is (search "best: none -- the team has no scorer" (team-slash "opener" "  ")) "the digest")
            (let ((text (team-slash "opener" "stop")))
              (is (search "2 nodes asked to stop" text))
              (is (search "is kept" text)))
            (is (await () (team-idle-p dir)) "every node's turn ended")
            (dolist (id (team-nodes dir))
              (is (= 1 (count-session-events id "turn.cancelled")) "one cancel each")
              (is (= 1 (length (team-inputs id))) "and nothing is prompted after it"))))))))

;;; --- what a node keeps -------------------------------------------------------------
;;; The first real runs filed four node definitions into the operator's layer,
;;; one of them appending to a dead team's LOG from every boot.

(deftest team-cell-a-nodes-definitions-stay-out-of-the-layer ()
  (with-team-runtime (root scorer :nodes 1 :open (dir))
    (let ((calls '()))
      (let ((node (first (team-nodes dir)))
            (next (lambda (&rest arguments) (push arguments calls) :recorded)))
        (is (null (nodecode-team::layer-hook next '(defun s0-x () 1) "(defun s0-x () 1)" 18
                                             :session-id node)) "a node's definition is not filed")
        (let ((nlk:*scribe-session-id* node))
          (is (null (nodecode-team::layer-hook next '(defun s0-x () 1) "(defun s0-x () 1)" 18)) "nor under the node's own ambient id"))
        (is (null calls) "the layer was never reached")
        (ensure-durable-session "opener")
        (is (eq :recorded (nodecode-team::layer-hook next '(defun op-x () 1) "(defun op-x () 1)" 18
                                                     :session-id "opener")) "another session's falls through")
        (is (equal '(((defun op-x () 1) "(defun op-x () 1)" 18 :session-id "opener")) calls) "with its arguments whole")))))

(deftest team-cell-a-nodes-eval-defines-live-and-files-nothing ()
  ;; The same fence through the real eval seam and the real scribe, over the
  ;; scribe tests' own layer fixture: the definition is live, no file is kept.
  (with-team-runtime (root scorer :nodes 1 :open (dir))
    (ensure-durable-session "opener")
    ;; the snippet names its session off the turn it runs in (EVAL-FORM-PRINTED)
    (flet ((eval-in (session text)
             (let ((nle::*current-durable-turn*
                     (nlk:admit-turn session (nlk::make-durable-id "command") "a snippet")))
               (prog1 (nle:eval-string text)
                 (nlk:complete-turn nle::*current-durable-turn* nil)))))
      (with-scribe-fixture
        (eval-in (first (team-nodes dir)) "(defun s0-team-fence-probe () 1)")
        (is (eql 1 (funcall (find-symbol "S0-TEAM-FENCE-PROBE" '#:nodecode.evolved))) "a node's defun is live in the image")
        (is (null (layer-file-text "s0-team-fence-probe.lisp")) "and never filed in the layer")
        (eval-in "opener" "(defun team-fence-kept-probe () 2)")
        (is (layer-file-text "team-fence-kept-probe.lisp") "another session's is")))))

(deftest team-cell-a-relative-dir-is-the-calling-sessions ()
  (with-team-runtime (root scorer :nodes 1 :open (dir))
    (let ((named (namestring (truename dir))))
      (let ((nlk:*scribe-session-id* (first (team-nodes dir))))
        (is (search named (team:best ".")) "from a node, `.' is its team"))
      (nlk:create-session :id "beside" :cwd (namestring root))
      (let ((nlk:*scribe-session-id* "beside"))
        (is (search named (team:best "t")) "from a session, a name under its directory")
        (is (search "no team at" (refusal-text team:team-error (team:best "elsewhere"))))))))

(deftest team-cell-watch-answers-the-scores-maximum-only-once-verify-confirms-it ()
  (with-team-runtime (root scorer :nodes 2 :open (dir))
    ;; two honest scores through ./verify, and one line no run stands behind
    (is (equal "score=4 met=0" (team-sh dir (format nil "./verify 0 ~a"
                                                    (team-candidate dir "work-0/a" 4)))))
    (is (equal "score=7 met=0" (team-sh dir (format nil "./verify 1 ~a"
                                                    (team-candidate dir "work-1/b" 7)))))
    (team-candidate dir "work-0/forged" 3)
    (team-sh dir "echo '[slot 0 12:00:00Z] score=99 met=1 ref=work-0/forged' >> SCORES")
    (is (= 3 (length (team-lines dir "SCORES"))))
    (let ((answer (team:watch dir :poll 0.05))
          (scores (team-lines dir "SCORES")))
      (is (search "not solved" answer))
      (is (search "best: score 7 by slot 1, artifact work-1/b, confirmed by ./verify" answer))
      (is (null (search "forged" answer)) "the unconfirmed maximum is nobody's best")
      ;; the confirm path ran, highest claim first, as slot `parent'
      (is (= 5 (length scores)))
      (is (search "[slot parent " (fourth scores)))
      (is (search "score=3 met=0 ref=work-0/forged" (fourth scores)))
      (is (search "score=7 met=0 ref=work-1/b" (fifth scores)))
      (is (search "best: score 7 by slot 1" (team:best dir)) "and a parent line is no claim"))
    ;; an idle node with budget left is prompted again, a bounded number of times
    (dolist (id (team-nodes dir))
      (is (equal (make-list 3 :initial-element
                             (format nil "[~a] ~a" id nodecode-team:*continue*))
                 (rest (team-inputs id))) "the continue prompt carries the node's marker"))))

(deftest team-cell-watch-ends-on-solved-and-stops-the-stragglers ()
  (with-team-runtime (root scorer :nodes 2 :open (dir :working t))
    (let ((nodecode-team::*grace-polls* 2))
      (is (= 2 (count-if #'nlk:active-turn-p (team-nodes dir))) "both nodes are working")
      ;; ./verify exits 0 once the task is met
      (is (eql 0 (nth-value 1 (team-sh dir (format nil "./verify 0 ~a"
                                                   (team-candidate dir "work-0/win" 12))))))
      (team-sh dir "echo 'slot 0 score 12 ./verify 0 work-0/win' > SOLVED")
      (let ((answer (team:watch dir :poll 0.05)))
        (is (search "SOLVED" answer))
        (is (search "best: score 12 (task met) by slot 0, artifact work-0/win" answer)))
      (is (await () (team-idle-p dir)) "the stragglers were stopped")
      (dolist (id (team-nodes dir))
        (is (= 1 (count-session-events id "turn.cancelled")) "one cancel each")
        (is (= 1 (length (team-inputs id))) "and nobody is prompted after SOLVED")))))

(deftest team-cell-watch-ends-at-the-deadline ()
  (with-team-runtime (root scorer :nodes 1 :seconds 600 :open (dir :working t))
    (let ((nodecode-team::*grace-polls* 1))
      ;; the team opened 1000 s ago: its 600 s are over
      (let ((config (nodecode-team::read-config dir)))
        (setf (getf config :opened-at) (- (getf config :opened-at) 1000))
        (nodecode-team::write-config dir config))
      (is (search "best: none" (team:watch dir :poll 0.05)))
      (is (await () (team-idle-p dir)) "the node was stopped")
      (is (= 1 (length (team-inputs (first (team-nodes dir))))) "and never prompted again"))))

;;; --- stop --------------------------------------------------------------------------

(deftest team-cell-stop-ends-every-node-and-keeps-the-directory ()
  (with-team-runtime (root scorer :nodes 2 :open (dir :working t))
    (team-sh dir "mkdir slots/slot-0 && echo 'by hand' > slots/slot-0/approach")
    (team-sh dir (format nil "./verify 0 ~a" (team-candidate dir "work-0/a" 5)))
    (is (search "2 nodes asked to stop" (team:stop dir "enough")))
    (is (await () (team-idle-p dir)) "every node's turn ended")
    (dolist (id (team-nodes dir))
      (is (= 1 (count-session-events id "turn.cancelled")))
      (is (nlk:session-exists-p id) "the session stays, its transcript the record"))
    ;; the directory is as it was
    (dolist (name '("TASK.md" "LOG" "SCORES" "DISCONFIRMED" "ADOPTED" "best.lock" "team.sexp"
                    "verify" "score" "slots/slot-0/approach" "work-0/a" "best/"))
      (is (probe-file (merge-pathnames name dir)) name))
    (is (equal '("by hand") (team-lines dir "slots/slot-0/approach")))
    (is (search "0 nodes asked to stop" (team:stop dir)) "a second stop finds nobody")
    (is (= 1 (length (team-lines dir "SCORES"))) "and a stop scores nothing")
    (is (search "best: score 5 by slot 0" (team:best dir)) "and the team still answers")))
