;;;; verbs.lisp --- queue, show, rank, prep, merge: the rules, in code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What a maintainer's prose would ask of a triage agent is what these
;;;; verbs refuse to do otherwise. RANK is the one write a rank turn has: four
;;;; ranks, one label family, one review of event COMMENT at the head the
;;;; model was shown (never an approval, never a change request), and a close
;;;; only for three reasons, each with a citation checked against the
;;;; repository before anything is written. PREP runs the PR's code in a
;;;; scratch clone whose push URL goes nowhere. MERGE lands the contributor's
;;;; own PR. PREP and MERGE are the operator's: a session begun by the watch
;;;; or by cron is refused them, read off the session's first admission.

(in-package #:nodecode-prs)

(defun resolve-repo (repo)
  "REPO, else the one repository the section watches, else a refusal."
  (cond (repo (check-repo repo))
        ((= 1 (length (setting :repos))) (first (setting :repos)))
        (t (fail "name the repository: :repo \"owner/name\"~@[ (watched: ~{~a~^, ~})~]"
                 (setting :repos)))))

(defun expect-head (repo n head sha)
  "HEAD, when SHA is NIL or names it; a refusal when the PR moved past SHA."
  (when (and sha (not (and (stringp sha) (>= (length sha) 7) (uiop:string-prefix-p sha head))))
    (fail "~a moved: its head is ~a now, not ~a; a request for the new head is on its way"
          (key repo n) (short head) sha))
  head)

(defparameter +unattended-sources+ '("prs" "cron")
  "Invocation sources whose sessions may not prep or merge: nobody is watching them.")

(defun calling-source ()
  "The source kind that began the session this verb is called from, or NIL."
  (nlk:session-source-kind nlk:*scribe-session-id*))

(defun refuse-unattended (verb)
  (let ((source (calling-source)))
    (when (member source +unattended-sources+ :test #'equal)
      (fail "(prs:~(~a~)) is the operator's: this session was begun by ~a, which runs unattended"
            verb source))))

;;; --- reading ---------------------------------------------------------------------

(defun rank-state (repo pull)
  "Where PULL stands: draft, unranked, the rank at its head, or pushed since."
  (let* ((head (head-of pull))
         (mark (and (not (nlk:json-value pull :boolean "draft")) (mark repo (number-of pull) head))))
    (cond ((nlk:json-value pull :boolean "draft") "draft")
          ((null mark) "unranked")
          ((equal (getf mark :sha) head) (format nil "~a at ~a" (getf mark :rank) (short head)))
          (t (format nil "~a at ~a, pushed since" (getf mark :rank) (short (getf mark :sha)))))))

(defun state-order (state)
  (cond ((ppcre:scan "^p[0-3] at [0-9a-f]+$" state) (digit-char-p (char state 1)))
        ((equal state "unranked") 4)
        ((equal state "draft") 6)
        (t 5)))

(defun age (iso &optional (now (get-universal-time)))
  (let ((then (nlk:iso-universal iso)))
    (if then (nlk:duration-text (- now then) :coarse t) "?")))

(defun queue-text (repo)
  (let ((rows (mapcar (lambda (pull) (cons (rank-state repo pull) pull)) (open-pulls repo))))
    (if (null rows)
        (format nil "~a: no open pull requests" repo)
        (format nil "~a: ~d open~{~%~a~}" repo (length rows)
                (mapcar (lambda (row &aux (pull (cdr row)))
                          (format nil "#~d  ~a  ~a  @~a (~(~a~))  ~a"
                                  (number-of pull) (car row)
                                  (age (nlk:json-value pull :string "created_at"))
                                  (author-of pull)
                                  (nlk:json-value pull :string "author_association")
                                  (nlk:one-line (title-of pull) :cap 80)))
                        (stable-sort rows #'< :key (lambda (row) (state-order (car row)))))))))

(define-verb queue (&optional repo)
  "Every open PR of REPO -- of every watched repository when NIL -- one line each, p0 first."
  (let ((repos (if repo (list (check-repo repo)) (setting :repos))))
    (unless repos
      (fail "no repository is watched: (prs:queue \"owner/name\")"))
    (format nil "~{~a~^~%~%~}" (mapcar #'queue-text repos))))

(defun checks-line (repo head)
  "The check runs at HEAD as one line: how many passed, failed (named) and are pending."
  (let* ((runs (coerce (nlk:json-array (api :get (path repo "/commits/~a/check-runs?per_page=100" head))
                                       "check_runs")
                       'list))
         (conclusion (lambda (run) (nlk:json-value run :string "conclusion")))
         (failed (loop for run in runs
                       when (member (funcall conclusion run)
                                    '("failure" "timed_out" "cancelled" "action_required") :test #'equal)
                         collect (nlk:json-value run :string "name"))))
    (if (null runs)
        "none"
        (format nil "~d passed, ~d failed~@[ (~{~a~^, ~})~], ~d pending"
                (count-if (lambda (run) (member (funcall conclusion run) '("success" "neutral" "skipped")
                                                :test #'equal))
                          runs)
                (length failed) failed
                (count-if-not (lambda (run) (equal (nlk:json-value run :string "status") "completed")) runs)))))

(defun overlaps (repo n files)
  "The other open PRs of REPO touching any of FILES, one phrase each; a PR
whose files GitHub would not list is named as unread, not a failure."
  (let ((mine (file-names files)))
    (loop for other in (open-pulls repo)
          for m = (number-of other)
          for theirs = (and (/= m n)
                            (handler-case (file-names (pull-files repo m (head-of other)))
                              (prs-error () :unread)))
          for shared = (and (listp theirs) (intersection mine theirs :test #'string=))
          when (eq theirs :unread)
            collect (format nil "#~d (its files could not be read)" m)
          when shared
            collect (format nil "#~d ~s (~{~a~^, ~}~:[~;, ...~])"
                            m (nlk:one-line (title-of other) :cap 60)
                            (subseq shared 0 (min 3 (length shared))) (> (length shared) 3)))))

(defparameter +diff-limit+ 40000
  "How much of the diff a dossier carries; (prs:prep N) holds the rest.")

(defun diff-text (files)
  (nlk:clip (with-output-to-string (out)
              (dolist (file files)
                (nlk:with-json ((name :string "filename") (status :string "status")
                                (added :integer "additions" :default 0)
                                (removed :integer "deletions" :default 0)
                                (patch :string "patch"))
                    file
                  (format out "~&=== ~a (~a +~d -~d)~%~a~%" name status added removed
                          (or patch "(no patch: binary, or too large for the API)")))))
            +diff-limit+ :disclose "(prs:prep N) checks the whole change out"))

(defun dossier (repo n)
  "Everything a rank reads about PR N of REPO, as one text."
  (let* ((pull (pull repo n))
         (head (head-of pull))
         (files (pull-files repo n head))
         (mark (mark repo n head))
         (overlaps (overlaps repo n files)))
    (nlk:with-json ((title :string "title") (body :string "body" :default "")
                    (association :string "author_association")
                    (label :string "head" "label") (base :string "base" "ref")
                    (added :integer "additions" :default 0) (removed :integer "deletions" :default 0)
                    (mergeable :string "mergeable_state" :default "unknown")
                    (draft :boolean "draft"))
        pull
      (format nil "PR ~a: ~a~%by @~a (~(~a~)), opened ~a ago, updated ~a ago~%~
                   head ~a (~a) onto ~a; +~d -~d in ~d file~:p; mergeable: ~a~:[~;; draft~]~%~
                   labels: ~:[none~;~:*~{~a~^, ~}~]~%checks at head: ~a~%last rank: ~a~%~
                   ~@[overlaps open PRs: ~{~a~^; ~}~%~]~%--- description ---~%~a~%~%--- files ---~%~
                   ~{~a~%~}~%--- diff ---~%~a"
              (key repo n) title (author-of pull) association
              (age (nlk:json-value pull :string "created_at")) (age (nlk:json-value pull :string "updated_at"))
              (short head) label base added removed (length files) mergeable draft
              (label-names pull) (checks-line repo head)
              (if mark
                  (format nil "~a at ~a~:[, pushed since~;~] (~a)~%~a" (getf mark :rank)
                          (short (getf mark :sha)) (equal (getf mark :sha) head) (getf mark :url)
                          (nlk:clip (getf mark :text) 1500))
                  "none")
              overlaps
              (if (plusp (length (nlk:trimmed body))) (nlk:clip body 4000 :disclose t) "(none)")
              (mapcar (lambda (file)
                        (nlk:with-json ((name :string "filename") (status :string "status")
                                        (plus :integer "additions" :default 0)
                                        (minus :integer "deletions" :default 0))
                            file
                          (format nil "~a +~d -~d  ~a" status plus minus name)))
                      files)
              (diff-text files)))))

(define-verb show (n &key repo)
  "PR N whole: who, what, checks, overlapping PRs, the last rank, the diff."
  (dossier (resolve-repo repo) n))

;;; --- the one write a rank turn has ---------------------------------------------------

(defparameter +ranks+ '("p0" "p1" "p2" "p3"))

(defun rank-name (rank)
  (let ((name (and (or (stringp rank) (symbolp rank)) (string-downcase (string rank)))))
    (or (find name +ranks+ :test #'equal)
        (fail "a rank is :p0, :p1, :p2 or :p3, not ~s" rank))))

(defun cited-number (cite)
  (ppcre:register-groups-bind ((#'parse-integer number)) ("^#?([0-9]+)$" (nlk:trimmed cite)) number))

(defun cited-sha (cite)
  (and (ppcre:scan "^[0-9a-f]{7,40}$" (nlk:trimmed cite)) (nlk:trimmed cite)))

(defun on-default-branch-p (repo sha)
  "Whether commit SHA is in the history of REPO's default branch."
  (let ((default (nlk:json-value (api :get (path repo "")) :string "default_branch")))
    (member (nlk:json-value (api :get (path repo "/compare/~a...~a" sha default)) :string "status")
            '("ahead" "identical") :test #'equal)))

(defun close-line (repo n close cite)
  "The line a closing review opens with, CITE checked against REPO first."
  ;; Three reasons, each with a citation a reader can follow; any other close
  ;; is the operator's, not a rank's.
  (let ((reason (and (or (stringp close) (symbolp close))
                     (find close '(:on-main :duplicate :cannot-reproduce) :test #'string-equal)))
        (cite (and (stringp cite) cite)))
    (unless reason
      (fail ":close is :on-main, :duplicate or :cannot-reproduce, not ~s; any other close is ~
             the operator's" close))
    (let ((number (and cite (cited-number cite)))
          (sha (and cite (cited-sha cite))))
      (ecase reason
        (:duplicate
         (unless (and number (/= number n))
           (fail ":close :duplicate needs :cite \"#N\", the PR or issue this one repeats"))
         (api :get (path repo "/issues/~d" number))
         (format nil "Closing as a duplicate of #~d." number))
        (:on-main
         (unless sha
           (fail ":close :on-main needs :cite, the commit on the default branch that already has this"))
         (unless (on-default-branch-p repo sha)
           (fail "~a is not on ~a's default branch; :close :on-main cites a commit that is" sha repo))
         (format nil "Closing: the default branch already has this change, as ~a." sha))
        (:cannot-reproduce
         (unless sha
           (fail ":close :cannot-reproduce needs :cite, the commit the problem was tried at"))
         (api :get (path repo "/commits/~a" sha))
         (format nil "Closing: the problem does not reproduce at ~a." sha))))))

(defun review-comments (comments)
  "COMMENTS, (:path :line :body) plists, as the review's line comments."
  (map 'vector (lambda (comment)
                 (let ((path (and (listp comment) (evenp (length comment)) (getf comment :path)))
                       (line (and (listp comment) (evenp (length comment)) (getf comment :line)))
                       (body (and (listp comment) (evenp (length comment)) (getf comment :body))))
                   (unless (and (stringp path) (integerp line) (plusp line) (stringp body))
                     (fail "a comment is (:path \"file\" :line N :body \"text\"), not ~s" comment))
                   (nlk:json-object "path" path "line" line "side" "RIGHT" "body" body)))
       comments))

(defun set-rank-label (repo n pull rank)
  "PR N wears review:RANK and no other review:p label."
  (let ((label (format nil "review:~a" rank))
        (names (label-names pull)))
    (dolist (stale names)
      (when (and (ppcre:scan "^review:p[0-3]$" stale) (string/= stale label))
        (api :delete (path repo "/issues/~d/labels/~a" n (quri:url-encode stale)))))
    (unless (member label names :test #'string=)
      (api :post (path repo "/issues/~d/labels" n) :body (nlk:json-object "labels" (vector label))))
    label))

(define-verb rank (n rank &key repo sha review comments close cite)
  "Rank PR N: the review:RANK label and one COMMENT review at its head; with CLOSE, closed citing CITE."
  (let* ((repo (resolve-repo repo))
         (rank (rank-name rank))
         (pull (pull repo n))
         (head (expect-head repo n (head-of pull) sha)))
    (unless (equal (nlk:json-value pull :string "state") "open")
      (fail "~a is ~a, not open" (key repo n) (nlk:json-value pull :string "state")))
    (unless (and (stringp review) (plusp (length (nlk:trimmed review))))
      (fail ":review is what the author reads, and it is empty"))
    (let* ((closing (and close (close-line repo n close cite)))
           (posted (api :post (path repo "/pulls/~d/reviews" n)
                        :body (nlk:json-object
                               "commit_id" head "event" "COMMENT"
                               "body" (format nil "~@[~a~%~%~]~a~%~%~?" closing (nlk:trimmed review)
                                              +marker+ (list head rank))
                               :opt "comments" (and comments (review-comments comments)))))
           (url (nlk:json-value posted :string "html_url")))
      (with-prs-lock
        (setf (gethash (key repo n) *marks*)
              (cons head (list :sha head :rank rank :text (nlk:trimmed review) :url url))))
      (set-rank-label repo n pull rank)
      (when closing
        (api :patch (path repo "/pulls/~d" n) :body (nlk:json-object "state" "closed")))
      (nle:notice (format nil "prs ~a ranked ~a~:[~; and closed~]: ~a" (key repo n) rank closing
                          (nlk:first-line (nlk:trimmed review) 120))
                  :level :info)
      (format nil "~a ranked ~a at ~a: label review:~a, review ~a~@[; closed: ~a~]"
              (key repo n) rank (short head) rank url closing))))

;;; --- the operator's two ------------------------------------------------------------

(defun clone-dir (repo)
  "The scratch clone of REPO: a cache, recomputed from GitHub at will."
  (nlk:cache-path (format nil "nodecode/prs/~a/" (substitute #\- #\/ (check-repo repo)))))

(defun clone-url (repo)
  "Where REPO is fetched from; a test points it at a local repository."
  (format nil "https://github.com/~a.git" repo))

(defun git-environment ()
  "The token as an http header only git sees, never on a command line."
  (list "GIT_TERMINAL_PROMPT=0" "GIT_CONFIG_COUNT=1"
        "GIT_CONFIG_KEY_0=http.https://github.com/.extraheader"
        (format nil "GIT_CONFIG_VALUE_0=AUTHORIZATION: basic ~a"
                (cl-base64:string-to-base64-string (format nil "x-access-token:~a" (token))))))

(defun git (dir &rest arguments)
  "git ARGUMENTS in DIR: => its output, or a refusal with its error output."
  (multiple-value-bind (out err status)
      (nlk:run-bounded (list* "git" arguments) :directory dir :seconds 300
                                                 :environment (git-environment))
    (unless (eql status 0)
      (fail "git ~{~a~^ ~} ~:[exited ~a~;timed out~*~]: ~a" arguments (eq status :timeout) status
            (nlk:trimmed (or err ""))))
    out))

(defun ensure-clone (repo dir)
  (unless (probe-file (merge-pathnames ".git/" dir))
    (ensure-directories-exist dir)
    (git dir "init" "--quiet")
    (git dir "remote" "add" "origin" (clone-url repo))
    ;; Nothing here pushes, and the clone could not if something did.
    (git dir "remote" "set-url" "--push" "origin" "prs-never-pushes")))

(defun rebase (dir onto)
  "Rebase DIR's HEAD onto ONTO: T, or NIL and the conflicting files after an abort."
  (let ((status (nth-value 2 (nlk:run-bounded
                              (list "git" "-c" "user.name=nodecode-prs" "-c" "user.email=prs@nodecode.invalid"
                                    "-c" "commit.gpgsign=false" "rebase" "--quiet" onto)
                              :directory dir :seconds 300))))
    (or (eql status 0)
        (let ((conflicts (nlk:lines (git dir "diff" "--name-only" "--diff-filter=U"))))
          (nlk:run-bounded (list "git" "rebase" "--abort") :directory dir)
          (values nil conflicts)))))

(defun run-check (dir command)
  "COMMAND through sh in DIR under the check limit: a pass line, or a FAIL line and its output's tail."
  (let ((started (get-internal-real-time)))
    (multiple-value-bind (out err status)
        (nlk:run-bounded (list "sh" "-c" (format nil "exec 2>&1; ~a" command))
                         :directory dir :seconds (* 60 (setting :check-minutes)))
      (declare (ignore err))
      (let ((took (nlk:duration-text (/ (nlk:elapsed-ms started) 1000))))
        (if (eql status 0)
            (format nil "pass  ~a (~a)" command took)
            (format nil "FAIL  ~a (~:[exit ~a~;timed out~*~], ~a)~{~%      ~a~}" command
                    (eq status :timeout) status took
                    (last (nlk:lines out) 20)))))))

(define-verb prep (n &key repo)
  "PR N checked out in the repository's scratch clone, rebased onto its base, the checks run there. Never pushes."
  (refuse-unattended :prep)
  (let* ((repo (resolve-repo repo))
         (pull (pull repo n))
         (base (nlk:json-value pull :string "base" "ref"))
         (dir (clone-dir repo))
         (branch (format nil "pr-~d" n)))
    (ensure-clone repo dir)
    (git dir "fetch" "--quiet" "origin" (format nil "+refs/heads/~a:refs/remotes/origin/~a" base base)
         (format nil "+refs/pull/~d/head:refs/prs/~d" n n))
    (git dir "checkout" "--quiet" "--force" "--detach" (format nil "refs/prs/~d" n))
    (multiple-value-bind (rebased conflicts) (rebase dir (format nil "origin/~a" base))
      (if (not rebased)
          (format nil "~a does not rebase onto ~a: conflicts in ~{~a~^, ~}; ~a is at the PR's own head"
                  (key repo n) base conflicts (namestring dir))
          (progn
            (git dir "branch" "--force" branch "HEAD")
            (format nil "~a rebased onto ~a in ~a, branch ~a~%~a~%~:[checks: none configured (prs.checks)~;~
                         checks:~:*~{~%  ~a~}~]"
                    (key repo n) base (namestring dir) branch
                    (nlk:trimmed (git dir "diff" "--stat" (format nil "origin/~a" base) "HEAD"))
                    (mapcar (lambda (command) (run-check dir command)) (setting :checks))))))))

(define-verb merge (n &key repo sha method)
  "Merge PR N, the contributor's own commits, as \"Merge PR #N: title (@author)\"."
  (refuse-unattended :merge)
  (let* ((repo (resolve-repo repo))
         (pull (pull repo n))
         (head (expect-head repo n (head-of pull) sha))
         (method (let ((method (or method (setting :merge-method))))
                   (if (and method (symbolp method)) (string-downcase (symbol-name method)) method)))
         (title (format nil "Merge PR #~d: ~a (@~a)" n (title-of pull) (author-of pull))))
    (unless (member method '("squash" "merge" "rebase") :test #'equal)
      (fail ":method is \"squash\", \"merge\" or \"rebase\", not ~s" method))
    (when (nlk:json-value pull :boolean "draft")
      (fail "~a is a draft: its author has not asked for a merge" (key repo n)))
    (let ((merged (api :put (path repo "/pulls/~d/merge" n)
                       :body (nlk:json-object "commit_title" title "merge_method" method "sha" head))))
      (nle:notice (format nil "prs ~a merged: ~a" (key repo n) title) :level :info)
      (format nil "~a merged (~a) as ~a: ~a" (key repo n) method
              (short (or (nlk:json-value merged :string "sha") "")) title))))
