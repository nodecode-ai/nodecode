;;;; policy-review.lisp --- an edit to the agent's own policy, from draft to verdict.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Under policy.self the agent writes its own rules (AGENTS.md), its voice
;;;; (SOUL.md) and its system prompt (SYSTEM.md) in the policy store
;;;; (engine/policy.lisp): a draft on the store's working tree binds nothing,
;;;; the adopted ref binds. This file is the one road from a draft to the
;;;; adopted ref.
;;;;
;;;;   POLICY-PROPOSE  files and a declared direction. A file the charter
;;;;                   holds is refused, and so is a change once dropped.
;;;;                   POLICY-SORT puts it on one of two paths:
;;;;     :stricter     adopted at once and reviewed afterwards, so a
;;;;                   loosening that called itself stricter is reverted;
;;;;     :shadow       kept off the adopted ref until a review adopts it:
;;;;                   every looser edit, and every edit to a file in
;;;;                   *POLICY-ALWAYS-SHADOW* (the system prompt, which no
;;;;                   one sorts reliably, and which shapes its own review).
;;;;   the review      a scheduled job after *POLICY-SHADOW-HOURS*: its own
;;;;                   session, none of the proposing turn's context, its
;;;;                   policy reads pinned to the proposal's base commit so
;;;;                   the edit never judges itself. It reads
;;;;                   POLICY-REVIEW-EVIDENCE, the replay of the real asks
;;;;                   since the proposal under the old and the new text,
;;;;                   and ends in POLICY-DECIDE.
;;;;   POLICY-DECIDE   :adopt or :drop. A drop is sticky: the record keeps
;;;;                   it and a resubmission is refused. A dropped stricter
;;;;                   edit is reverted, and a stricter edit whose replay
;;;;                   shows a looser flip cannot be kept.
;;;;   lapse           a proposal no review decided by its deadline lapses
;;;;                   (POLICY-REVIEW-SWEEP): a shadow edit never binds, a
;;;;                   stricter one is reverted. Only reviewed text survives.
;;;;
;;;; Nothing here is a security boundary: the agent owns this image and can
;;;; skip any of it. What the road buys is a record. Every step is a commit
;;;; on the adopted ref or a drop the record pushes off the machine, so an
;;;; edit that skipped the road shows afterwards.
;;;;
;;;; The store, the replay, the record and the charter are other files'
;;;; (policy, policy-replay, policy-record, charter); this one reaches them
;;;; through POLICY-SEAM, so it loads and is tested without them.

(in-package #:nodecode.evolved)

;;; --- knobs ------------------------------------------------------------------

(defparameter *policy-shadow-hours* 24
  "Hours a proposal waits before its review: the window of real asks the
replay judges it on.")

(defparameter *policy-review-grace-hours* 48
  "Hours past the review time a proposal may wait for its verdict before it
lapses.")

(defparameter *policy-always-shadow* '("SYSTEM.md")
  "Policy files whose every edit takes the shadow path, whatever direction
it declares. The system prompt: a rewrite of it changes how every rule is
read, its review's included, so no edit to it is applied unreviewed.")

(defparameter *policy-review-timeout-minutes* 30
  "The review job's turn budget.")

;;; --- seams ------------------------------------------------------------------

(defvar *policy-seams* nil
  "Plist of seam keyword -> function, laid over +POLICY-SEAM-NAMES+. Tests
bind it; nothing else needs to.")

(defparameter +policy-seam-names+
  '(:self-p "POLICY-SELF-P" :text "POLICY-TEXT" :adopt "POLICY-ADOPT"
    :pin "POLICY-PIN" :ensure "POLICY-ENSURE" :directory "POLICY-DIRECTORY"
    :replay "POLICY-REPLAY"
    :charter-refusal "CHARTER-REFUSAL" :dropped-p "POLICY-DROPPED-P"
    :record-drop "POLICY-RECORD-DROP" :record-push "POLICY-RECORD-PUSH")
  "Each seam's function, by name: the store's, the replay's, the record's and
the charter's, found in this package or the kernel's at call time.")

(define-condition policy-review-error (error)
  ((reason :initarg :reason :reader policy-review-error-reason))
  (:report (lambda (condition stream)
             (write-string (policy-review-error-reason condition) stream))))

(defun policy-refuse (control &rest args)
  (error 'policy-review-error :reason (apply #'format nil control args)))

(defun policy-seam-function (key)
  "The function seam KEY calls, or NIL when nothing provides it."
  (or (getf *policy-seams* key)
      (let ((name (getf +policy-seam-names+ key)))
        (loop for package in '(#:nodecode.evolved #:nodecode.kernel)
              for symbol = (and name (find-symbol name package))
              when (and symbol (fboundp symbol)) return (symbol-function symbol)))))

(defun policy-seam (key &rest args)
  "Call seam KEY with ARGS; refuse with the reason when nothing provides it."
  (let ((function (policy-seam-function key)))
    (unless function
      (policy-refuse "policy review: ~(~a~) is not loaded" (getf +policy-seam-names+ key key)))
    (apply function args)))

(defun policy-store-directory ()
  "The policy store: its own git repository under the home."
  (or (getf *policy-seams* :store)
      (let ((directory (policy-seam-function :directory)))
        (if directory (funcall directory) (home "policy/")))))

;;; --- the store's git ----------------------------------------------------------

(defun store-git (args &key input environment (store (policy-store-directory)))
  "What git ARGS print in the policy store, trimmed; refuse with git's own
words when it fails."
  (multiple-value-bind (out err status)
      (run-bounded (list* nlk::*git-program* "-C" (string-right-trim "/" (namestring store)) args)
                   :seconds 60 :input input :environment environment)
    (unless (eql status 0)
      (policy-refuse "git ~{~a~^ ~} failed: ~a" args (trimmed (or err out ""))))
    (string-right-trim '(#\Newline #\Return) (or out ""))))

(defun adopted-head ()
  "The commit the adopted ref names."
  (store-git '("rev-parse" "--verify" "refs/heads/adopted^{commit}")))

(defun store-commit (base files message)
  "A commit whose parent is BASE and whose tree is BASE's with FILES laid
over it -- ((NAME . TEXT) ...), a NIL text removing NAME -- built in a
private index, so neither the working tree nor any ref moves. => its sha."
  (let* ((index (format nil "~a.git/policy-review-index-~d"
                        (namestring (policy-store-directory)) (random 1000000000)))
         (environment (list (format nil "GIT_INDEX_FILE=~a" index))))
    (unwind-protect
         (progn
           (store-git (list "read-tree" base) :environment environment)
           (loop for (name . text) in files
                 do (if text
                        (store-git (list "update-index" "--add" "--cacheinfo"
                                         (format nil "100644,~a,~a"
                                                 (store-git '("hash-object" "-w" "--stdin") :input text)
                                                 name))
                                   :environment environment)
                        (store-git (list "update-index" "--force-remove" name)
                                   :environment environment)))
           (store-git (list "commit-tree" (store-git '("write-tree") :environment environment)
                            "-p" base "-m" message)
                      :environment (append environment (store-identity))))
      (when (probe-file index) (delete-file index)))))

(defun store-identity ()
  "The author a proposal commit carries when the store names none. A
proposal ref is private to the store; only the adopted ref is pushed."
  (if (plusp (length (ignore-errors (store-git '("config" "user.name")))))
      '()
      '("GIT_AUTHOR_NAME=nodecode" "GIT_AUTHOR_EMAIL=nodecode@localhost"
        "GIT_COMMITTER_NAME=nodecode" "GIT_COMMITTER_EMAIL=nodecode@localhost")))

;;; --- proposals ----------------------------------------------------------------
;;; One proposal is one plist, kept readably beside the store's git data:
;;; operational state, not policy. What binds is the adopted ref, and what
;;; was dropped is the record's.

(defun proposals-directory ()
  (merge-pathnames ".git/policy-proposals/" (policy-store-directory)))

(defun proposal-path (id)
  (merge-pathnames (format nil "~a.sexp" id) (proposals-directory)))

(defun save-proposal (proposal)
  (let ((path (proposal-path (getf proposal :id))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*package* (find-package '#:keyword)))
          (prin1 proposal out))))
    proposal))

(defun policy-proposal (id)
  "The proposal ID, or a refusal naming it."
  (let ((path (proposal-path id)))
    (unless (probe-file path) (policy-refuse "no policy proposal ~a" id))
    (with-open-file (in path :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*package* (find-package '#:keyword))
              (*read-eval* nil))
          (read in))))))

(defun policy-proposals (&key state)
  "Every proposal, oldest first; only those in STATE when given."
  (let ((all (mapcar (lambda (path) (policy-proposal (pathname-name path)))
                     (directory (merge-pathnames "*.sexp" (proposals-directory))))))
    (sort (if state (remove state all :key (lambda (p) (getf p :state)) :test-not #'eq) all)
          #'< :key (lambda (p) (getf p :created)))))

(defun new-proposal-id (now)
  (multiple-value-bind (s m h day month year) (decode-universal-time now 0)
    (format nil "p~d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~36r" year month day h m s (random (expt 36 3)))))

(defun proposal-files (proposal ref)
  "PROPOSAL's files as ((NAME . TEXT) ...) at REF: :base or :commit."
  (loop with commit = (getf proposal ref)
        for name in (getf proposal :names)
        collect (cons name (policy-seam :text name :ref commit))))

;;; --- sorting ------------------------------------------------------------------

(defun policy-sort (file declared)
  "=> :STRICTER or :SHADOW: the path an edit to FILE declared DECLARED takes.
Only a declared :STRICTER edit to a file outside *POLICY-ALWAYS-SHADOW*
binds before its review."
  (if (and (eq declared :stricter)
           (not (member file *policy-always-shadow* :test #'string=)))
      :stricter
      :shadow))

(defun proposal-path-for (names declared)
  (if (every (lambda (name) (eq :stricter (policy-sort name declared))) names) :stricter :shadow))

;;; --- the road -----------------------------------------------------------------

(defun policy-propose (files &key (direction :looser) reason session-id
                              (now (get-universal-time)) (schedule t))
  "Propose FILES, ((NAME . TEXT) ...) with a NIL text removing NAME, as one
edit to the adopted policy, declared DIRECTION (:STRICTER or :LOOSER) for
REASON. Refused when policy.self is off, when the charter holds a file,
when nothing changes, and when the record already dropped this change. A
:STRICTER edit binds now; the rest wait in shadow. Either way a review is
scheduled. => the proposal plist."
  (unless (policy-seam :self-p)
    (policy-refuse "policy.self is off: the operator's files are the policy"))
  (unless (member direction '(:stricter :looser))
    (policy-refuse "direction is :stricter or :looser, not ~s" direction))
  (unless (and files (every (lambda (f) (and (consp f) (stringp (car f)))) files))
    (policy-refuse "files are ((NAME . TEXT) ...)"))
  (let ((charter (policy-seam :charter-refusal (mapcar #'car files))))
    (when charter (policy-refuse "~a" charter)))
  (when (policy-seam-function :ensure) (policy-seam :ensure))
  (policy-review-sweep :now now)
  (let* ((names (mapcar #'car files))
         (base (adopted-head))
         (id (new-proposal-id now))
         (commit (store-commit base files
                               (format nil "policy: propose ~a (~(~a~))~@[~%~%~a~]~%~%Policy-Proposal: ~a"
                                       id direction reason id)))
         (diff (store-git (list "diff" base commit)))
         (tree (store-git (list "rev-parse" (format nil "~a^{tree}" commit))))
         (path (proposal-path-for names direction)))
    (when (zerop (length diff))
      (policy-refuse "the proposal changes nothing the adopted policy says"))
    (let ((dropped (policy-seam :dropped-p (policy-store-directory) files)))
      (when dropped
        (policy-refuse "this change was dropped before (~a); a dropped change stays dropped"
                       dropped)))
    (store-git (list "update-ref" (format nil "refs/proposed/~a" id) commit))
    (let ((proposal (list :id id :created now :declared direction :path path
                          :names names :base base :commit commit :tree tree
                          :reason reason :session-id (or session-id (ignore-errors (eval-session-id)))
                          :review-at (+ now (* 3600 *policy-shadow-hours*))
                          :deadline (+ now (* 3600 (+ *policy-shadow-hours* *policy-review-grace-hours*)))
                          :state :pending)))
      (when (eq path :stricter)
        (setf (getf proposal :adopted)
              (policy-seam :adopt files
                           (format nil "policy: ~a stricter, binds now; review due~%~%Policy-Proposal: ~a~%Policy-Path: stricter"
                                   id id))))
      (save-proposal proposal)
      (when schedule
        (setf (getf proposal :job) (schedule-policy-review proposal))
        (save-proposal proposal))
      proposal)))

(defun review-prompt (id)
  (format nil "Policy review ~a. You are the review of an edit your agent proposed to its own policy, run apart from the turn that wrote it. Call (nle::policy-review-evidence ~s) and read the replay: the real asks since the proposal, decided under the old text and under the new. Then call (nle::policy-decide ~s :adopt \"why\") or (nle::policy-decide ~s :drop \"why\"). Drop an edit that lets the agent do what its charter or the old text refused without a reason the evidence supports; a stricter edit whose replay shows a looser flip cannot be kept. Answer SILENT."
          id id id id))

(defun schedule-policy-review (proposal &aux (id (getf proposal :id)))
  "Schedule PROPOSAL's review as a once-only job at its review time, and pin
the job's session to the proposal's base, so the review reads the policy as
it stood before the edit. => the job's session id."
  (let ((seconds (max 60 (- (getf proposal :review-at) (get-universal-time)))))
    (let* ((schedule (or (getf *policy-seams* :schedule)
                         (let ((add (find-symbol "ADD" "CRON")))
                           (and add (fboundp add) (symbol-function add)))))
           (answer (if schedule
                       (funcall schedule (format nil "in ~dm" (ceiling seconds 60)) (review-prompt id)
                                :name (format nil "policy-review-~a" id)
                                :timeout-minutes *policy-review-timeout-minutes*)
                       (policy-refuse "the cron cell is not loaded: nothing would run the review")))
           (marker (search "session " answer :from-end t))
           (session (and marker (string-right-trim " .;" (subseq answer (+ marker 8))))))
      (when session
        (policy-seam :pin session (getf proposal :base)))
      session)))

(defun policy-review-evidence (id &key session-id (now (get-universal-time)))
  "The evidence PROPOSAL ID is judged on: the replay of the real asks since
it was proposed, under the text before it and under its own. Pins the
calling session to the proposal's base first. => plist."
  (let* ((proposal (policy-proposal id))
         (session (or session-id (ignore-errors (eval-session-id)))))
    (unless (eq (getf proposal :state) :pending)
      (policy-refuse "proposal ~a is ~(~a~), not pending" id (getf proposal :state)))
    (when (and session (equal session (getf proposal :session-id)))
      (policy-refuse "the session that proposed ~a cannot review it" id))
    (when session (policy-seam :pin session (getf proposal :base)))
    (let ((replay (policy-seam :replay (proposal-files proposal :base) (proposal-files proposal :commit)
                               :since (getf proposal :created)
                               :until (min now (getf proposal :review-at)))))
      (setf (getf proposal :evidence)
            (list :read-at now :read-by session
                  :turns (getf replay :turns) :flips (getf replay :flips) :noise (getf replay :noise)
                  :looser (getf replay :looser) :stricter (getf replay :stricter)))
      (save-proposal proposal)
      (list :proposal id :path (getf proposal :path) :declared (getf proposal :declared)
            :reason (getf proposal :reason)
            :diff (store-git (list "diff" (getf proposal :base) (getf proposal :commit)))
            :replay replay
            :rule (if (eq (getf proposal :path) :stricter)
                      "Bound since it was proposed. Any looser flip means it was not stricter: drop it, which reverts it."
                      "Not applied yet. Adopt binds it; drop keeps it off for good.")))))

(defun stale-names (proposal from)
  "Of PROPOSAL's files, those whose adopted text no longer reads as at FROM
(:base or :commit): another edit reached them since."
  (let ((at (getf proposal from)))
    (loop for name in (getf proposal :names)
          unless (equal (policy-seam :text name :ref at) (policy-seam :text name :ref "refs/heads/adopted"))
            collect name)))

(defun revert-proposal (proposal why)
  "Put back the text PROPOSAL's files had before it, where nothing has moved
them since. => the revert commit, or NIL when nothing was left to revert."
  (let* ((moved (stale-names proposal :commit))
         (files (remove-if (lambda (f) (member (car f) moved :test #'string=))
                           (proposal-files proposal :base))))
    (when files
      (policy-seam :adopt files
                   (format nil "policy: revert ~a~%~%~a~%~%Policy-Proposal: ~a~%Policy-Verdict: revert"
                           (getf proposal :id) why (getf proposal :id))))))

(defun policy-decide (id verdict why &key session-id (now (get-universal-time)))
  "End proposal ID's review with VERDICT, :ADOPT or :DROP, for WHY. Only a
session other than the proposer's, after the review time, with the evidence
read. A shadow edit adopted binds; dropped, it never does. A stricter edit
kept stays; dropped, or kept against a looser flip, it is reverted. Every
drop goes to the record. => the proposal plist."
  (let* ((proposal (policy-proposal id))
         (session (or session-id (ignore-errors (eval-session-id))))
         (evidence (getf proposal :evidence)))
    (unless (member verdict '(:adopt :drop))
      (policy-refuse "the verdict is :adopt or :drop, not ~s" verdict))
    (unless (and (stringp why) (plusp (length (trimmed why))))
      (policy-refuse "a verdict carries its reason"))
    (unless (eq (getf proposal :state) :pending)
      (policy-refuse "proposal ~a is ~(~a~), not pending" id (getf proposal :state)))
    (when (and session (equal session (getf proposal :session-id)))
      (policy-refuse "the session that proposed ~a cannot decide it" id))
    (when (< now (getf proposal :review-at))
      (policy-refuse "proposal ~a is reviewed after ~a h, not before" id *policy-shadow-hours*))
    (unless evidence
      (policy-refuse "read the evidence first: (policy-review-evidence ~s)" id))
    (when (and (eq verdict :adopt) (eq (getf proposal :path) :stricter)
               (plusp (or (getf evidence :looser) 0)))
      (setf verdict :drop
            why (format nil "declared stricter, but the replay shows ~d looser flip~:p; ~a"
                        (getf evidence :looser) (getf evidence :looser) why)))
    (let ((message (format nil "~a~%~%Policy-Proposal: ~a~%Policy-Verdict: ~(~a~)~@[~%Policy-Reviewer: ~a~]"
                           why id verdict session)))
      (ecase verdict
        (:adopt
         (if (eq (getf proposal :path) :stricter)
             (setf (getf proposal :state) :kept)
             (let ((stale (stale-names proposal :base)))
               (if stale
                   (setf (getf proposal :state) :lapsed
                         why (format nil "~{~a~^, ~} changed since it was proposed: propose again against the adopted text" stale))
                   (setf (getf proposal :adopted)
                         (policy-seam :adopt (proposal-files proposal :commit)
                                      (format nil "policy: adopt ~a~%~%~a" id message))
                         (getf proposal :state) :adopted)))))
        (:drop
         (when (eq (getf proposal :path) :stricter)
           (setf (getf proposal :reverted) (revert-proposal proposal why)))
         (setf (getf proposal :dropped)
               (policy-seam :record-drop (policy-store-directory) (proposal-files proposal :commit)
                            :reason why :id id)
               (getf proposal :state) :dropped))))
    (setf (getf proposal :verdict) verdict (getf proposal :why) why
          (getf proposal :decided-at) now (getf proposal :decided-by) session)
    (save-proposal proposal)
    (when session (ignore-errors (policy-seam :pin session nil)))
    (ignore-errors (policy-seam :record-push (policy-store-directory)))
    proposal))

(defun policy-review-sweep (&key (now (get-universal-time)))
  "Lapse every pending proposal past its deadline: a shadow edit simply
never binds, a stricter one is reverted. => the lapsed proposals."
  (loop for proposal in (and (probe-file (proposals-directory)) (policy-proposals :state :pending))
        when (> now (getf proposal :deadline))
          collect (progn
                    (when (eq (getf proposal :path) :stricter)
                      (setf (getf proposal :reverted)
                            (revert-proposal proposal "no review decided it by its deadline")))
                    (setf (getf proposal :state) :lapsed (getf proposal :decided-at) now
                          (getf proposal :why) "no review decided it by its deadline")
                    (save-proposal proposal))))

;;; --- the system prompt ----------------------------------------------------------
;;; Under policy.self the system message is the adopted SYSTEM.md, read at the
;;; caller's pin like every policy file, so a review pinned to a proposal's
;;; base runs under the prompt from before the edit. No SYSTEM.md, or the mode
;;; off, is *SYSTEM-PROMPT*. A proposal that writes SYSTEM.md also writes
;;; SYSTEM.shipped, the shipped prompt it was written against; when a release
;;; ships another, POLICY-PROMPT-DRIFT says so and offers the three-way merge,
;;; which goes through the road as a proposal of its own. The agent's prompt
;;; is never reverted to the shipped one silently.

(defun policy-system-prompt (&optional session-id)
  "The system message SESSION-ID's requests carry."
  (or (and (policy-seam-function :self-p) (policy-seam :self-p)
           (ignore-errors (policy-seam :text "SYSTEM.md" :session-id session-id)))
      *system-prompt*))

(defun prompt-files (text)
  "The files a proposal of TEXT as the system prompt writes."
  (list (cons "SYSTEM.md" text) (cons "SYSTEM.shipped" *system-prompt*)))

(defun policy-propose-prompt (text &key reason session-id)
  "Propose TEXT as the agent's system prompt. Always the shadow path."
  (policy-propose (prompt-files text) :direction :looser :reason reason :session-id session-id))

(defun policy-prompt-drift ()
  "NIL while the adopted SYSTEM.md was written against the prompt this
release ships; else (:was SHIPPED-THEN :now SHIPPED-NOW :merged TEXT-OR-NIL),
TEXT the clean three-way merge of the agent's prompt with the release's
change, NIL when the two conflict."
  (let ((mine (policy-seam :text "SYSTEM.md" :ref "refs/heads/adopted"))
        (was (policy-seam :text "SYSTEM.shipped" :ref "refs/heads/adopted")))
    (when (and mine was (string/= was *system-prompt*))
      (list :was was :now *system-prompt* :merged (merge-three mine was *system-prompt*)))))

(defun merge-three (mine base theirs)
  "git merge-file's clean merge of MINE and THEIRS over BASE, or NIL."
  (let ((dir (format nil "~a.git/policy-merge-~d/" (namestring (policy-store-directory)) (random 1000000000))))
    (ensure-directories-exist dir)
    (unwind-protect
         (flet ((put (name text)
                  (let ((path (format nil "~a~a" dir name)))
                    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
                      (write-string text out))
                    path)))
           (multiple-value-bind (out err status)
               (run-bounded (list nlk::*git-program* "merge-file" "-p"
                                  (put "mine" mine) (put "base" base) (put "theirs" theirs))
                            :seconds 30)
             (declare (ignore err))
             (and (eql status 0) out)))
      (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore))))
