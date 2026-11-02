;;;; pulls.lisp --- the GitHub pull request a session's branch stands in,
;;;; for the session list's second line: open, draft, merged or closed, and
;;;; how its checks went. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row asks with its folder; the answer is what the cache holds, at once.
;;;; A key with no fresh answer is queued, and one worker asks GitHub a single
;;;; GraphQL query per repository for every queued branch of it, then writes
;;;; the answers back. The list never waits on GitHub: a cold key reads null,
;;;; and an answer that changes a row tells every attached client
;;;; (session_list_changed), which reads the list again. A push moves the
;;;; branch's remote-tracking ref, and the next read asks again at once.

(in-package #:nodecode.evolved)

(defvar *pull-requests* (make-hash-table :test #'equal)
  "(REPO . BRANCH) -> (PULL CHECKED-AT FRESH-FOR): the newest pull request
GitHub named for BRANCH of REPO (`owner/name') as a plist (PULL-REQUEST-OF),
or NIL for none; the unix time it was asked; and how many seconds that answer
stays fresh.")

(defvar *pull-requests-queued* (make-hash-table :test #'equal)
  "The (REPO . BRANCH) keys the worker asks GitHub about next.")

(defvar *pull-requests-lock* (bt2:make-lock :name "pull requests")
  "Guards *PULL-REQUESTS*, *PULL-REQUESTS-QUEUED* and *PULL-REQUESTS-WORKER*.")

(defvar *pull-requests-worker* nil
  "The thread draining *PULL-REQUESTS-QUEUED*, or NIL while none runs.")

(defvar *pull-request-fetcher* 'github-pull-requests
  "The function (REPO BRANCHES) -> an alist (BRANCH . PULL), or :FAILED when
no answer came: GitHub in the organism, a stand-in under test.")

(defvar *github-token* :unread
  "The token GitHub is asked with (GITHUB-TOKEN), NIL for none, :UNREAD
before the first ask.")

(defparameter +pull-request-batch+ 40
  "The most branches one GraphQL query asks about.")

(defparameter +pull-request-settle+ 0.2
  "Seconds a starting worker waits before it takes the queue.")

(defvar *pull-request-stamps* (make-hash-table :test #'equal)
  "(REPO BRANCH COMMON-DIR) -> (STAMP FOLDER READ-AT): what the remotes of the
checkout at COMMON-DIR hold for BRANCH as last read (FOLDER-PUSH-STAMP), the
folder a row read it off, and when a row last did. A push or a fetch moves
it, and whoever sees it moved -- a read, or the worker re-reading the
checkouts rows read lately -- has GitHub asked again.")

(defvar *pull-requests-wake* (bt2:make-condition-variable :name "pull requests queued")
  "Notified when a key is queued while the worker waits out a step.")

(defparameter +pull-request-watch-for+ 120
  "Seconds after a row's read the worker keeps re-reading its checkout's refs,
every +PULL-REQUEST-FOLLOW-STEP+: a page reads the list every half minute, so
while one is open a push is seen within a step, not at its next read.")

(defvar *pull-request-follows* (make-hash-table :test #'equal)
  "(REPO . BRANCH) -> the unix time until which a branch a read just saw
pushed, still with no pull request, is asked again every
+PULL-REQUEST-FOLLOW-STEP+ seconds by the worker itself, no read waited for:
the step after a push is most often to open its pull request.")

(defparameter +pull-request-follow-step+ 5
  "Seconds between the worker's asks about a branch it follows.")

(defparameter +pull-request-follow-for+ 120
  "Seconds a branch is followed after a read saw it pushed.")

(defun pull-request-fresh-seconds (pull &optional pushed)
  "How long the answer PULL stays fresh: an open or draft pull request a
minute, its checks still moving; none half a minute on a PUSHED branch, the
next step on one is to open its pull request, and five minutes on one that is
not, or is its repository's default; a merged or closed one half an hour."
  (cond ((and (null pull) pushed) 30)
        ((null pull) 300)
        ((member (getf pull :state) '("open" "draft") :test #'equal) 60)
        (t 1800)))

;;; --- the checkout a folder sits in -----------------------------------------

(defun first-line-of (path)
  "PATH's first line without its line end, or NIL when PATH is no file."
  (with-open-file (in path :if-does-not-exist nil)
    (when in
      (when-let (line (read-line in nil))
        (string-right-trim '(#\Return #\Space #\Tab) line)))))

(defun linked-git-dirs (dot dir)
  "(values GIT-DIR COMMON-DIR) the .git file DOT in DIR names: a linked
worktree's own `gitdir:' and the repository its `commondir' points back to."
  (let ((line (first-line-of dot)))
    (when (and line (uiop:string-prefix-p "gitdir:" line))
      (let* ((named (uiop:parse-unix-namestring (string-trim " " (subseq line 7))
                                                :ensure-directory t))
             (git (probe-file (uiop:merge-pathnames* named dir)))
             (common (first-line-of (and git (merge-pathnames "commondir" git)))))
        (when git
          (values git
                  (or (and common
                           (probe-file (uiop:merge-pathnames*
                                        (uiop:parse-unix-namestring common :ensure-directory t)
                                        git)))
                      git)))))))

(defun folder-git-dirs (folder)
  "(values GIT-DIR COMMON-DIR) of the checkout FOLDER sits in, the nearest
up the tree: GIT-DIR holds its HEAD -- a linked worktree's own
.git/worktrees/NAME/ -- and COMMON-DIR the repository's config. NIL outside
every checkout."
  (when (and (stringp folder) (uiop:absolute-pathname-p folder))
    (loop repeat 64
          for dir = (uiop:ensure-directory-pathname folder)
            then (uiop:pathname-parent-directory-pathname dir)
          for dot = (merge-pathnames ".git" dir)
          for dot-dir = (uiop:ensure-directory-pathname dot)
          do (cond ((uiop:directory-exists-p dot-dir) (return (values dot-dir dot-dir)))
                   ((uiop:file-exists-p dot) (return (linked-git-dirs dot dir))))
          until (equal dir (uiop:pathname-parent-directory-pathname dir)))))

(defun folder-branch (folder)
  "The branch FOLDER's checkout has out, or NIL when its HEAD is detached or
FOLDER is in no checkout."
  (when-let (git (folder-git-dirs folder))
    (let ((head (first-line-of (merge-pathnames "HEAD" git))))
      (when (and head (uiop:string-prefix-p "ref: refs/heads/" head))
        (subseq head 16)))))

(defun checkout-remote-urls (common-dir)
  "The (NAME . URL) remotes the config in COMMON-DIR names, in file order."
  (with-open-file (in (merge-pathnames "config" common-dir) :if-does-not-exist nil)
    (when in
      (loop with remote = nil
            for raw = (read-line in nil)
            while raw
            for line = (string-trim '(#\Space #\Tab #\Return) raw)
            for eq = (position #\= line)
            if (uiop:string-prefix-p "[" line)
              do (setf remote (and (uiop:string-prefix-p "[remote \"" line)
                                   (when-let (end (position #\" line :start 9))
                                     (subseq line 9 end))))
            else if (and remote eq (string-equal (string-trim '(#\Space #\Tab) (subseq line 0 eq)) "url"))
                   collect (cons remote (string-trim '(#\Space #\Tab) (subseq line (1+ eq))))))))

(defun github-repository-of-url (url)
  "`owner/name' of a remote URL on github.com in any spelling git takes --
scp-like git@github.com:owner/name.git, ssh://, https:// with or without a
user in front -- else NIL."
  (let* ((at (search "github.com" url))
         (end (and at (+ at 10))))
    (when (and at (plusp at) (< end (length url))
               (member (char url (1- at)) '(#\@ #\/))
               (member (char url end) '(#\: #\/)))
      (let* ((path (string-right-trim "/" (subseq url (1+ end))))
             (path (if (uiop:string-suffix-p path ".git") (subseq path 0 (- (length path) 4)) path))
             (slash (position #\/ path)))
        (when (and slash (plusp slash) (< (1+ slash) (length path))
                   (not (find #\/ path :start (1+ slash))))
          path)))))

(defun folder-github-repository (folder)
  "The github.com repository FOLDER's checkout pushes to, `owner/name': its
origin remote's, else the first remote on github.com; NIL when none is."
  (when-let (common (nth-value 1 (folder-git-dirs folder)))
    (let ((remotes (checkout-remote-urls common)))
      (some (lambda (remote) (github-repository-of-url (cdr remote)))
            (append (remove "origin" remotes :key #'car :test-not #'equal)
                    (remove "origin" remotes :key #'car :test #'equal))))))

(defun commit-id-p (line)
  "Whether LINE is a whole commit id: 40 hex digits, or 64 in a SHA-256
repository."
  (and (member (length line) '(40 64)) (every (lambda (c) (digit-char-p c 16)) line)))

(defun packed-ref (common-dir ref)
  "The commit packed-refs in the repository at COMMON-DIR names for REF, or NIL."
  (with-open-file (in (merge-pathnames "packed-refs" common-dir) :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
            while line
            for space = (position #\Space line)
            when (and space (commit-id-p (subseq line 0 space))
                      (string= ref (string-right-trim '(#\Return) (subseq line (1+ space)))))
              return (subseq line 0 space)))))

(defun repository-ref (common-dir ref)
  "The line the repository at COMMON-DIR keeps for REF (`refs/...'): its loose
file's -- a commit, or `ref: ...' for a symbolic one -- else packed-refs'
commit; NIL when it has none."
  (or (first-line-of (merge-pathnames (uiop:parse-unix-namestring ref) common-dir))
      (packed-ref common-dir ref)))

(defun folder-push-stamp (folder branch)
  "(values STAMP DEFAULT-P) for BRANCH of FOLDER's checkout: STAMP the commit
each remote held for it when last pushed or fetched, ((REMOTE . COMMIT) ...),
NIL when no remote has it -- never pushed; DEFAULT-P whether it is its
repository's default branch, the one a remote's HEAD names (main or master
when none does), whose pull requests are rare."
  (when-let (common (nth-value 1 (folder-git-dirs folder)))
    (let ((remotes (mapcar #'car (checkout-remote-urls common)))
          (named nil))
      (values (loop for remote in remotes
                    for commit = (repository-ref common (format nil "refs/remotes/~a/~a" remote branch))
                    for head = (repository-ref common (format nil "refs/remotes/~a/HEAD" remote))
                    when (and head (uiop:string-prefix-p "ref: " head))
                      do (push (subseq head (+ 5 (length (format nil "refs/remotes/~a/" remote)))) named)
                    when (and commit (commit-id-p commit))
                      collect (cons remote commit))
              (if named
                  (and (member branch named :test #'equal) t)
                  (and (member branch '("main" "master") :test #'equal) t))))))

;;; --- asking GitHub ----------------------------------------------------------

(defun gh-hosts-path ()
  "Where gh keeps hosts.yml: $GH_CONFIG_DIR, else $XDG_CONFIG_HOME/gh, else
~/.config/gh."
  (let ((dir (or (uiop:getenvp "GH_CONFIG_DIR")
                 (when-let (xdg (uiop:getenvp "XDG_CONFIG_HOME"))
                   (uiop:strcat (string-right-trim "/" xdg) "/gh"))
                 (uiop:native-namestring (merge-pathnames ".config/gh" (user-homedir-pathname))))))
    (uiop:strcat (string-right-trim "/" dir) "/hosts.yml")))

(defun gh-hosts-token (path)
  "The github.com oauth_token in gh's hosts.yml at PATH -- the active
account's, the one nearest the host's own level -- or NIL."
  (with-open-file (in path :if-does-not-exist nil)
    (when in
      (loop with inside = nil and best = nil and best-indent = nil
            for line = (read-line in nil)
            while line
            for indent = (position #\Space line :test-not #'char=)
            when indent
              do (cond ((zerop indent)
                        (setf inside (string= (string-right-trim '(#\Space #\Return) line) "github.com:")))
                       ((and inside (uiop:string-prefix-p "oauth_token:" (subseq line indent))
                             (or (null best-indent) (< indent best-indent)))
                        (setf best (string-trim '(#\Space #\Return #\" #\') (subseq line (+ indent 12)))
                              best-indent indent)))
            finally (return (and best (plusp (length best)) best))))))

(defun gh-cli-token ()
  "What `gh auth token' answers -- the token gh keeps in the keyring -- or NIL."
  (ignore-errors
   (let ((token (string-trim '(#\Space #\Newline #\Return)
                             (uiop:run-program '("gh" "auth" "token" "--hostname" "github.com")
                                               :output :string :error-output nil
                                               :ignore-error-status t))))
     (and (plusp (length token)) (not (find #\Space token)) token))))

(defun github-token ()
  "The token the organism asks GitHub with: GH_TOKEN, GITHUB_TOKEN, the one
gh keeps in hosts.yml, else gh's keyring -- NIL when there is none, and then
no pull request is looked up. Read once; a refusal forgets it."
  ;; Only the worker reads it: `gh auth token' is a process, never run on a
  ;; request's thread.
  (when (eq *github-token* :unread)
    (setf *github-token* (or (uiop:getenvp "GH_TOKEN") (uiop:getenvp "GITHUB_TOKEN")
                             (gh-hosts-token (gh-hosts-path)) (gh-cli-token))))
  *github-token*)

(defun pull-requests-query (count)
  "The GraphQL query that asks one repository about COUNT branches, the
branch names riding as variables $b0..$bN, never spliced into the text."
  (format nil "query($owner: String!, $name: String!~{, $~a: String!~}) { repository(owner: $owner, name: $name) {~{ ~a: pullRequests(headRefName: $~:*~a, last: 1) { nodes { number state isDraft url commits(last: 1) { nodes { commit { statusCheckRollup { state } } } } } }~} } }"
          (loop for i below count collect (format nil "b~d" i))
          (loop for i below count collect (format nil "b~d" i))))

(defun pull-request-of (node)
  "The pull request GraphQL NODE describes, (:number :state :url :checks):
STATE open, draft, merged or closed; CHECKS passing, failing, pending, or NIL
when the head commit has none."
  (let ((rollup (json-value (first (coerce (or (json-value node :array "commits" "nodes") #()) 'list))
                            :text "commit" "statusCheckRollup" "state")))
    (list :number (json-value node :integer "number")
          :state (let ((state (json-value node :text "state")))
                   (cond ((equal state "MERGED") "merged")
                         ((equal state "CLOSED") "closed")
                         ((json-value node :boolean "isDraft") "draft")
                         (t "open")))
          :url (json-value node :text "url")
          :checks (cond ((member rollup '("SUCCESS") :test #'equal) "passing")
                        ((member rollup '("FAILURE" "ERROR") :test #'equal) "failing")
                        ((member rollup '("PENDING" "EXPECTED") :test #'equal) "pending")))))

(defun github-pull-requests (repo branches &aux (token (github-token)) (slash (position #\/ repo)))
  "One GraphQL ask about BRANCHES of REPO (`owner/name'): an alist (BRANCH .
PULL), PULL the newest pull request whose head is BRANCH (PULL-REQUEST-OF) or
NIL for none; :FAILED when there is no token or no answer. Asked at
$GITHUB_GRAPHQL_URL when set, the name GitHub's own runners give the
endpoint, else api.github.com."
  (if (null token)
      :failed
      (let ((variables (make-hash-table :test #'equal)))
        (setf (gethash "owner" variables) (subseq repo 0 slash)
              (gethash "name" variables) (subseq repo (1+ slash)))
        (loop for branch in branches for i from 0
              do (setf (gethash (format nil "b~d" i) variables) branch))
        (multiple-value-bind (body status)
            (http-fetch (or (uiop:getenvp "GITHUB_GRAPHQL_URL") "https://api.github.com/graphql")
                        :content (nlk:encode-json-object
                                  (make-json-object "query" (pull-requests-query (length branches))
                                                    "variables" variables))
                        :headers `(("authorization" . ,(format nil "Bearer ~a" token))
                                   ("content-type" . "application/json"))
                        :timeout 20)
          (let* ((parsed (and body (ignore-errors (nlk:decode-json (nlk:body-text body)))))
                 (repository (and parsed (json-value parsed :object "data" "repository"))))
            (when (and (stringp status) (search "HTTP 401" status))
              (setf *github-token* :unread))
            (if (null repository)
                :failed
                (loop for branch in branches for i from 0
                      for nodes = (coerce (or (json-value repository :array (format nil "b~d" i) "nodes") #())
                                          'list)
                      collect (cons branch (and nodes (pull-request-of (first nodes)))))))))))

;;; --- the cache and its worker -----------------------------------------------

(defun note-push-stamp (key seen stamp default-p &key folder (now (nlk:unix-now)))
  "Under *PULL-REQUESTS-LOCK*: hold STAMP as what the checkout SEEN names has
for KEY's branch -- FOLDER, when given, a row's read just now. Answers T when
STAMP moved from what was held: KEY is queued then, and followed while it has
no pull request and is pushed and not the default (DEFAULT-P)."
  (multiple-value-bind (entry known) (gethash seen *pull-request-stamps*)
    (let ((moved (and known (not (equal (first entry) stamp))))
          (pushed (and stamp (not default-p))))
      (setf (gethash seen *pull-request-stamps*)
            (list stamp (or folder (second entry)) (if folder now (third entry))))
      (when moved
        (setf (gethash key *pull-requests-queued*) (or (gethash key *pull-requests-queued*) pushed))
        (when (and pushed (null (first (gethash key *pull-requests*))))
          (setf (gethash key *pull-request-follows*) (+ now +pull-request-follow-for+))))
      moved)))

(defun watched-checkouts (now)
  "Under the lock: (KEY SEEN FOLDER) for each checkout a row read within
+PULL-REQUEST-WATCH-FOR+ of NOW."
  (loop for seen being the hash-keys of *pull-request-stamps* using (hash-value entry)
        for (nil folder read-at) = entry
        when (and read-at (< (- now read-at) +pull-request-watch-for+))
          collect (list (cons (first seen) (second seen)) seen folder)))

(defun reread-push-stamps ()
  "Read again the refs of every checkout a row read lately: a push seen here
is asked about at once and followed, no read waited for."
  (loop for (key seen folder) in (bt2:with-lock-held (*pull-requests-lock*)
                                   (watched-checkouts (nlk:unix-now)))
        ;; A checkout that cannot be read now is passed over, not taken for
        ;; one no remote holds.
        for read = (ignore-errors (multiple-value-list (folder-push-stamp folder (cdr key))))
        when read
          do (bt2:with-lock-held (*pull-requests-lock*)
               (note-push-stamp key seen (first read) (second read)))))

(defun announce-pull-requests-moved ()
  "Tell every attached client a row's pull request reads differently now:
one session_list_changed frame, and a page reads the list again."
  ;; A frame no client took is no loss: the list's own next read draws it.
  (ignore-errors
   (broadcast-organism "session_list_changed" (make-json-object "reason" "pr"))))

(defun store-pull-requests (repo batch answer &aux (now (nlk:unix-now)) (moved nil))
  "Hold ANSWER -- an alist (BRANCH . PULL), or :FAILED -- for BATCH, the
branches of REPO it was asked about, each (BRANCH . PUSHED) as queued; when
an answer differs from what the rows read, every client is told."
  (bt2:with-lock-held (*pull-requests-lock*)
    (loop for (branch . pushed) in batch
          for key = (cons repo branch)
          for held = (first (gethash key *pull-requests*))
          do (setf (gethash key *pull-requests*)
                   (if (consp answer)
                       (let ((pull (cdr (assoc branch answer :test #'equal))))
                         (unless (equal pull held) (setf moved t))
                         (when pull (remhash key *pull-request-follows*))
                         (list pull now (pull-request-fresh-seconds pull pushed)))
                       ;; No answer: what was held stays, asked again in ten minutes.
                       (list held now 600)))))
  (when moved (announce-pull-requests-moved)))

(defun take-queued-pull-requests ()
  "(values KEYS FOLLOWING): the queued keys, each ((REPO . BRANCH) . PUSHED),
the queue emptied, and whether a branch is still followed, the follows past
their time dropped. With neither, the worker leaves in the same step, so a
key queued after it finds no worker and starts one."
  (bt2:with-lock-held (*pull-requests-lock*)
    (let ((keys (loop for key being the hash-keys of *pull-requests-queued*
                        using (hash-value pushed)
                      collect (cons key pushed)))
          (now (nlk:unix-now)))
      (clrhash *pull-requests-queued*)
      (loop for key being the hash-keys of *pull-request-follows* using (hash-value until)
            when (<= until now) collect key into done
            finally (dolist (key done) (remhash key *pull-request-follows*)))
      (let ((following (plusp (hash-table-count *pull-request-follows*)))
            (watching (and (watched-checkouts now) t)))
        (unless (or keys following watching) (setf *pull-requests-worker* nil))
        (values keys following watching)))))

(defun queue-followed-pull-requests ()
  "Queue every branch the worker follows, as pushed."
  (bt2:with-lock-held (*pull-requests-lock*)
    (loop for key being the hash-keys of *pull-request-follows*
          do (setf (gethash key *pull-requests-queued*) t))))

(defun ask-pull-requests (keys)
  "Ask GitHub about KEYS, each ((REPO . BRANCH) . PUSHED), a repository and a
batch at a time, and hold the answers."
  (dolist (repo (remove-duplicates (mapcar #'caar keys) :test #'equal))
    (loop with branches = (loop for ((r . b) . pushed) in keys
                                when (equal r repo) collect (cons b pushed))
          while branches
          do (let ((batch (subseq branches 0 (min +pull-request-batch+ (length branches)))))
               (setf branches (nthcdr (length batch) branches))
               (store-pull-requests
                repo batch (ignore-errors (funcall *pull-request-fetcher* repo (mapcar #'car batch))))))))

(defun drain-pull-requests ()
  "Ask about every queued key, a repository and a batch at a time, and every
followed branch each +PULL-REQUEST-FOLLOW-STEP+, until the queue stands empty
and nothing is followed: the worker's whole life."
  ;; One list read queues every row's key within a few milliseconds; the
  ;; settle lets them all ride the first ask instead of a key at a time.
  (sleep +pull-request-settle+)
  (unwind-protect
       (flet ((clock () (/ (get-internal-real-time) internal-time-units-per-second)))
         (loop with due = (+ (clock) +pull-request-follow-step+)
               do (multiple-value-bind (keys following watching) (take-queued-pull-requests)
                    (cond (keys (ask-pull-requests keys))
                          ((not (or following watching)) (return))
                          ((>= (clock) due)
                           (setf due (+ (clock) +pull-request-follow-step+))
                           (queue-followed-pull-requests)
                           (reread-push-stamps))
                          ;; A step's rest, cut short by a key a read queues.
                          (t (bt2:with-lock-held (*pull-requests-lock*)
                               (when (zerop (hash-table-count *pull-requests-queued*))
                                 (bt2:condition-wait *pull-requests-wake* *pull-requests-lock*
                                                     :timeout (max 0.01 (- due (clock)))))))))))
    (bt2:with-lock-held (*pull-requests-lock*)
      (when (eq *pull-requests-worker* (bt2:current-thread))
        (setf *pull-requests-worker* nil)))))

(defun session-pull-request (folder &aux (repo (folder-github-repository folder))
                                         (branch (and repo (folder-branch folder))))
  "The pull request FOLDER's branch stands in, as the cache holds it now --
(:number :state :url :checks) or NIL -- queuing a fresh ask when that answer
is stale or missing. Never waits on GitHub."
  (when branch
    (multiple-value-bind (stamp default-p) (folder-push-stamp folder branch)
      (let ((key (cons repo branch)) (now (nlk:unix-now))
            (seen (list repo branch (nth-value 1 (folder-git-dirs folder)))))
        (bt2:with-lock-held (*pull-requests-lock*)
          (destructuring-bind (&optional pull checked-at fresh-for) (gethash key *pull-requests*)
            ;; The stamp first, always: pushed or fetched since it was last
            ;; read, the key is queued (and followed) there.
            (when (or (note-push-stamp key seen stamp default-p :folder folder :now now)
                      (null checked-at) (> (- now checked-at) fresh-for))
              (setf (gethash key *pull-requests-queued*)
                    (or (gethash key *pull-requests-queued*) (and stamp (not default-p))))
              (when *pull-requests-worker*
                (bt2:condition-notify *pull-requests-wake*)))
            ;; Even with nothing to ask, the worker watches this checkout's
            ;; refs for a while (REREAD-PUSH-STAMPS).
            (unless *pull-requests-worker*
              (setf *pull-requests-worker*
                    (bt2:make-thread #'drain-pull-requests :name "pull requests")))
            pull))))))

(defun session-pull-request-json (folder)
  "FOLDER's pull request as a session row's `pr': {number, state, url,
checks}, checks null when its head has none; null for none, or none known yet."
  (if-let (pull (session-pull-request folder))
    (make-json-object "number" (getf pull :number) "state" (getf pull :state)
                      "url" (getf pull :url) "checks" (or (getf pull :checks) :null))
    :null))
