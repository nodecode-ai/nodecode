;;;; cell.lisp --- the primer, the settings, /prs, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the vocabulary through the manual: while the cell
;;;; runs, (help :prs) answers the primer and every request's help section
;;;; carries one line naming it. /prs is the watch's last view -- no network,
;;;; so the slash answers at once; (prs:queue) is the fresh one.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `websearch':
;;;;   "prs": {"repos": ["owner/name"], "token_file": "~/.nodecode/secrets/github-token",
;;;;           "checks": ["just lint"]}
;;;; The token is named, never written: an environment variable (token_env)
;;;; or a file (token_file) -- `gh auth token > FILE' makes one.

(in-package #:nodecode-prs)

;;; --- the manual ---------------------------------------------------------------
;;; (help :prs) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "Pull request triage is available through the nodecode-prs cell: Lisp functions in the prs:
package, called through eval. Every one returns a string; every refusal is ERROR: PRS-ERROR with
the reason. :repo is \"owner/name\", and may be left out while prs.repos names one repository.
  (prs:queue &optional \"owner/name\")   every open PR, one line each, p0 first: number, its rank
      and the head it was given at (or unranked, or pushed since), age, author, title.
  (prs:show N &key repo)   one PR whole: author, head and base, size, mergeable state, checks at
      the head, other open PRs touching the same files, the last rank's review, the description,
      the files and the diff (clipped).
  (prs:rank N :p0|:p1|:p2|:p3 &key repo sha review comments close cite)   the one write: the
      review:pN label (any other review:p label comes off) and ONE review of event COMMENT at the
      head - never an approval, never a change request. :review is what the author reads;
      :comments is a list of (:path \"file\" :line N :body \"text\") on lines of the diff; :sha
      refuses when the PR moved past it. :close closes after the review, only as :duplicate
      (:cite \"#N\"), :on-main (:cite a commit on the default branch) or :cannot-reproduce (:cite
      the commit tried), each citation checked first; any other close is the operator's.
  (prs:prep N &key repo)   the PR fetched into the repository's scratch clone, rebased onto its
      base, and prs.checks run there; answers the clone's directory, the rebase and each check.
      It runs the PR's code: read the diff first. Nothing is ever pushed.
  (prs:merge N &key repo sha method)   merge the contributor's own PR as \"Merge PR #N: title
      (@author)\", by prs.merge_method unless :method says. prep and merge refuse in a session
      begun by the watch or by cron.
Ranks: p0 merge now (small, right, fixes something); p1 merge this week; p2 worth landing after
changes; p3 unlikely to land. The watch polls prs.repos every poll_seconds - no tokens while
nothing moves - and gives each new head of someone else's ready PR one turn in session
prs-<repo>-<n>, its request carrying (prs:show N); there a snippet may only call prs:show, prs:queue
and prs:rank with literal arguments. A head whose turn ended without a rank waits for its next
push, or a (prs:rank ...) from here. /prs lists what the watch saw last."
  "What (help :prs) answers while the cell runs.")

;;; --- /prs ---------------------------------------------------------------------------

(defun watched-text (repo)
  "REPO as the watch last listed it: one line per open PR with the rank it has seen."
  (let ((pulls (with-prs-lock (gethash repo *pulls*))))
    (if (null pulls)
        (format nil "~a: nothing listed yet~@[ (~a)~]" repo
                (and (member repo *failing* :test #'string=) "its last poll failed"))
        (format nil "~a: ~d open~{~%#~a~}" repo (length pulls)
                (mapcar (lambda (pull)
                          (let ((mark (cdr (with-prs-lock (gethash (key repo (number-of pull)) *marks*)))))
                            (format nil "~d  ~a  @~a  ~a" (number-of pull)
                                    (cond ((nlk:json-value pull :boolean "draft") "draft")
                                          ((null mark) "unranked")
                                          ((equal (getf mark :sha) (head-of pull)) (getf mark :rank))
                                          (t (format nil "~a, pushed since" (getf mark :rank))))
                                    (author-of pull) (nlk:one-line (title-of pull) :cap 70))))
                        pulls)))))

(defun run-slash (args)
  "The answer to one /prs invocation: `poll' wakes the watch now."
  (let ((repos (setting :repos)))
    (cond ((null repos)
           "prs: no repository is watched; set prs.repos in config.jsonc, or (prs:queue \"owner/name\")")
          ((equal (nlk:trimmed (or args "")) "poll")
           (nlk:worker-poke *worker*)
           "prs: polling now")
          (t (format nil "~{~a~^~%~}" (mapcar #'watched-text repos))))))

;;; --- the entry -----------------------------------------------------------------------

(defun secret (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun read-settings (values table)
  "The declared VALUES, plus the token the section names: read once, here."
  (declare (ignore table))
  (let ((env (getf values :token-env))
        (file (getf values :token-file)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (dolist (repo (getf values :repos))
      (unless (ppcre:scan "^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$" repo)
        (nlk:config-error "repos holds ~s; a repository is \"owner/name\"" repo)))
    (list* :token (cond (env (or (secret (uiop:getenv env))
                                 (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                        (file (or (secret (ignore-errors (uiop:read-file-string (nlk:expand-home file))))
                                  (nlk:config-error "token_file ~a cannot be read, or is empty" file))))
           values)))

(defun install ()
  "Fresh caches, and the watch thread when repos names any."
  (with-prs-lock
    (dolist (table (list *etags* *pulls* *seen* *marks* *files*))
      (clrhash table))
    (setf *running* '() *failing* '() *login* nil))
  (when (and *watch* (setting :repos))
    (setf *worker* (nlk:worker-start "prs-watch" #'poll :wake (setting :poll-seconds) :prime t))
    (nle:keep-running (format nil "the pull request watch on ~{~a~^, ~}" (setting :repos))))
  (nle:on-stop (lambda ()
                 (setf *worker* (nlk:worker-stop *worker*))
                 (dolist (repo *failing*)
                   (nle:notice nil :key (format nil "prs:~a" repo)))
                 (setf *failing* '()))))

(nle:define-cell prs
  (:section ("prs")
    (:guide "repos is every owner/name the watch ranks; the token is named, never written: token_env (an environment variable) or token_file (a file; `gh auth token > FILE' writes one), with repo scope for a private repository; checks are shell commands (prs:prep) runs in the rebased clone")
    (:one-of "token_env" "token_file")
    ("repos" :list :doc "owner/name of every repository the watch ranks; empty, the verbs still answer")
    ("token_env" :env :doc "environment variable holding a GitHub token")
    ("token_file" :path :doc "file holding a GitHub token")
    ("poll_seconds" :integer :default 60 :min 15 :doc "how often the watch asks GitHub what moved")
    ("concurrent" :integer :default 2 :min 1 :doc "how many rank turns run at once")
    ("turn_minutes" :integer :default 5 :min 1 :doc "how long a rank turn may run before its calls are refused")
    ("model" :string :doc "the model a PR's session is pinned to; unset, the default")
    ("provider" :string :doc "the provider that model is on")
    ("checks" :list :doc "shell commands (prs:prep) runs in the rebased clone, in order: \"just lint\"")
    ("check_minutes" :integer :default 15 :min 1 :doc "how long one check may run")
    ("merge_method" :choice :options '("squash" "merge" "rebase") :default "squash"
                    :doc "how (prs:merge) lands a PR"))
  (:settings 'read-settings)
  (:start #'install)
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook :tool #'judge-only)
  (:help :prs "prs:queue lists open pull requests, prs:rank labels and reviews one; the watch ranks each push" +primer+)
  (:command "prs" (lambda (args session-id)
                    (declare (ignore session-id))
                    (run-slash args))
            :description "Pull requests the watch saw last; poll asks GitHub now"
            :argument-hint "poll"))
