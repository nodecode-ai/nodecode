;;;; nodecode-prs.asd --- pull request triage: a PR ranked minutes after each push.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `prs' section, starts one watch thread
;;;; over the repositories it names, advises NLE:TURN-BUDGET for the rank
;;;; turns, puts (help :prs) on the manual and registers /prs.
;;;;
;;;; What omp's robomp and Hermes's sweeper do from a bot account, reduced to
;;;; what the kernel does not already own: a GitHub seam, five verbs whose
;;;; rules live in code (four ranks, a COMMENT-only review, three close
;;;; reasons each with a citation checked, no push), and one sleeping thread
;;;; that turns a new head sha into one turn. The rest -- at-most-once
;;;; admission by command id, the session as the PR's record, the budget --
;;;; is the organism's, reached through its in-process seams.
;;;;
;;;; Every dependency is already in the serving image (cl-ppcre, dexador
;;;; through NLK:HTTP, bordeaux-threads ride with nodecode).

(defsystem "nodecode-prs"
  :description "Pull requests ranked and reviewed minutes after each push"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "github")
               (:file "verbs")
               (:file "watch")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-prs/test"))))

(defsystem "nodecode-prs/test"
  :description "PR triage tests. Registered into the shared nodecode.test registry; RUN-PRS-TESTS filters by the PRS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-prs" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-prs-tests)))
