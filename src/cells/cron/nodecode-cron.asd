;;;; nodecode-cron.asd --- scheduled prompts: a job is a durable session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional quicklisp/ASDF cell, NOT part of the organism core: nothing in
;;;; src/src names this system. The folder loader (kernel cells.lisp) loads
;;;; it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which reads the `cron' config
;;;; section, loads the job registry off the store, starts one ticker thread,
;;;; observes the :FRAME point for the turns it fires, puts a primer on every
;;;; session's harness sections so the model knows (cron:add ...) exists, and
;;;; registers /cron.
;;;;
;;;; A sixth axis in the catalogue: channels ADD a surface, the guard
;;;; INTERCEPTS, chrome gives the organism a PERIPHERAL, notify OBSERVES
;;;; egress, websearch is VOCABULARY -- this one gives the organism a CLOCK:
;;;; a prompt that arrives on its own, into a session of its own. hermes-agent's
;;;; cron/ (jobs.json, a 60 s tick, a fresh session per fire, executions and
;;;; notepad ledgers, delivery lanes) reduced to what the kernel does not
;;;; already own: a schedule grammar, one sleeping thread, and a registry row.
;;;; The rest -- at-most-once admission by command id, the transcript as the
;;;; run ledger, the notice board as delivery, the per-session model pin,
;;;; cancel -- is the organism's, reached through its in-process seams.
;;;;
;;;; Every dependency is already in the serving image (cl-ppcre, shasht,
;;;; bordeaux-threads ride with nodecode). The .asd sits INSIDE its folder:
;;;; install by putting -- or symlinking -- the directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-cron"
  :description "Prompts that run on a schedule"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "schedule")
               (:file "jobs")
               (:file "ticker")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cron/test"))))

(defsystem "nodecode-cron/test"
  :description "Cron tests. Registered into the shared nodecode.test registry; RUN-CRON-TESTS filters by the CRON-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cron" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "schedule-test")
               (:file "cell-test")
               (:file "import-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cron-tests)))
