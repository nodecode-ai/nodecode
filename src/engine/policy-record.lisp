;;;; engine/policy-record.lisp --- the record of the agent's policy, pushed off the box.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every edit of the agent's own policy is a commit on its store's `adopted'
;;;; branch (engine/policy). On the box that history is the agent's to
;;;; rewrite: it owns the process and the files. POLICY-RECORD-PUSH puts
;;;; `adopted' on refs/heads/policy/adopted at the remote config policy.record
;;;; names, a plain push and never a forced one. With that branch protected on
;;;; the forge (no force-push, admins included) a rewritten history cannot
;;;; land there: the push is refused as diverged and a standing notice says so.

(in-package #:nodecode.evolved)

(defparameter *policy-record-branch* "refs/heads/policy/adopted"
  "Where POLICY-RECORD-PUSH puts `adopted' on the record's remote.")

(defparameter *policy-record-notice-key* "policy-record"
  "The standing notice a record that cannot be pushed raises.")

(defun policy-record-remote ()
  "The record's remote, config policy.record (a URL or a remote name), or NIL."
  (let ((value (policy-config-current '("policy" "record"))))
    (and (stringp value) (plusp (length (string-trim " " value))) (string-trim " " value))))

(defun policy-record-say (text)
  "Stand TEXT as the record's notice, or take it down when TEXT is NIL."
  (unless (equal text (second (assoc *policy-record-notice-key* (nlk:standing-notices) :test #'string=)))
    (ignore-errors (notice text :level :warning :key *policy-record-notice-key*))))

(defun policy-record-push (&key (remote (policy-record-remote)))
  "Push the store's adopted branch to *POLICY-RECORD-BRANCH* at REMOTE, never
forced: :PUSHED, :NO-REMOTE when none is set, :DIVERGED when the remote's
branch is not an ancestor -- the history here was rewritten, or another
writer reached the branch -- or :FAILED. A refused push stands a notice until
one lands."
  (if (null remote)
      :no-remote
      (multiple-value-bind (out code err)
          (policy-git nil "push" "--porcelain" remote
                      (format nil "~a:~a" +policy-adopted-ref+ *policy-record-branch*))
        (let ((said (format nil "~a~%~a" out err)))
          (cond ((eql code 0)
                 (policy-record-say nil)
                 :pushed)
                ((or (search "[rejected]" said) (search "non-fast-forward" said) (search "fetch first" said))
                 (policy-record-say
                  (format nil "policy record: ~a refused the push of ~a, which no longer ~
                               extends what it holds. The adopted history here was rewritten ~
                               or another writer reached the branch; nothing is forced."
                          remote *policy-record-branch*))
                 :diverged)
                (t
                 (policy-record-say
                  (format nil "policy record: the push to ~a failed: ~a" remote
                          (string-trim '(#\Space #\Newline) err)))
                 :failed))))))

(defun policy-record-push-later ()
  "POLICY-RECORD-PUSH on a thread of its own when a remote is set, so the
request that committed an edit never waits on the network. The thread
carries the home and the remote it was started under: a thread inherits no
binding, so a profile's or a test's home would otherwise be lost."
  (let ((remote (policy-record-remote))
        (home nlk::*home*))
    (when remote
      (sb-thread:make-thread (lambda ()
                               (let ((nlk::*home* home))
                                 (ignore-errors (policy-record-push :remote remote))))
                             :name "policy record push"))))
