;;;; pulls-test.lisp --- the pull request a session row names: the checkout
;;;; read off git's own files, the GitHub answer folded, and the cache the
;;;; list reads without waiting.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   P1. A folder's branch and its github.com repository are read off the
;;;;       checkout's own files, up the tree from the folder: a plain .git
;;;;       directory, a linked worktree's .git file and its commondir; a
;;;;       detached HEAD has no branch, a folder outside a checkout neither.
;;;;   P2. Every spelling git takes for a github.com remote names owner/name;
;;;;       another host names nothing.
;;;;   P3. gh's hosts.yml gives the active account's github.com token.
;;;;   P4. A GraphQL pull request folds to open, draft, merged or closed, and
;;;;       its head's check rollup to passing, failing, pending or none.
;;;;   P5. The row reads the cache and never waits: a cold key answers NIL
;;;;       at once and is asked off the thread, one ask per repository; a
;;;;       failed ask keeps what was held.
;;;;   P6. GET /api/gateway/sessions carries `pr' on each row: the object for
;;;;       a branch with a pull request, null for a folder with none.
;;;;   P7. A push moves the branch's remote-tracking ref (loose or packed), and
;;;;       the next read asks again however fresh the answer was; none on a
;;;;       pushed branch is asked again in half a minute, on an unpushed one
;;;;       or the default branch in five.
;;;;   P8. An answer that changes what a row reads tells every client, one
;;;;       session_list_changed frame; the same answer again, or none, tells
;;;;       nobody.
;;;;   P9. The worker re-reads the refs of the checkouts rows read lately, so
;;;;       a push is seen with no read; a branch newly pushed, with no pull
;;;;       request, is followed: asked again every step until its pull
;;;;       request appears (and every client is told) or the follow runs out,
;;;;       and the worker leaves once nothing is followed or watched.

(in-package #:nodecode.test)

(defun fake-checkout (root &key (branch "feat/x") (url "git@github.com:acme/widget.git") head)
  "Lay a checkout at ROOT: .git/HEAD on BRANCH (or the raw HEAD line), a
config whose origin is URL; answers ROOT."
  (write-temp-file (merge-pathnames ".git/HEAD" root)
                   (format nil "~a~%" (or head (format nil "ref: refs/heads/~a" branch))))
  (write-temp-file (merge-pathnames ".git/config" root)
                   (format nil "[core]~%~cbare = false~%[remote \"origin\"]~%~curl = ~a~%~cfetch = +refs/heads/*:refs/remotes/origin/*~%"
                           #\Tab #\Tab url #\Tab))
  root)

(defmacro with-pull-request-cache ((&key fetcher) &body body)
  "Run BODY over an empty pull request cache and queue, FETCHER standing in
for GitHub; the organism's own are put back on the way out."
  `(with-saved-globals ((nle::*pull-requests* (make-hash-table :test #'equal))
                        (nle::*pull-request-stamps* (make-hash-table :test #'equal))
                        (nle::*pull-request-follows* (make-hash-table :test #'equal))
                        (nle::+pull-request-follow-step+ 0.2)
                        (nle::+pull-request-follow-for+ 2)
                        (nle::+pull-request-watch-for+ 1)
                        (nle::*pull-requests-queued* (make-hash-table :test #'equal))
                        (nle::*pull-requests-worker* nil)
                        (nle::*pull-request-fetcher* ,fetcher))
     (unwind-protect (progn ,@body)
       (await (:tries 50) (null nle::*pull-requests-worker*)))))

(deftest pull-request-checkouts-are-read-off-git-files (with-temp-trees ((root "pulls")))
  (let* ((repo (fake-checkout (merge-pathnames "repo/" root)))
         (deep (namestring (ensure-directories-exist (merge-pathnames "repo/src/deep/" root))))
         (tree (merge-pathnames "wt/" root))
         (detached (fake-checkout (merge-pathnames "loose/" root) :head "0123456789abcdef0123456789abcdef01234567"))
         (plain (namestring (ensure-directories-exist (merge-pathnames "plain/" root)))))
    (write-temp-file (merge-pathnames ".git/worktrees/wt/HEAD" repo) (format nil "ref: refs/heads/wt-branch~%"))
    (write-temp-file (merge-pathnames ".git/worktrees/wt/commondir" repo) (format nil "../..~%"))
    (write-temp-file (merge-pathnames ".git" tree)
                     (format nil "gitdir: ~a~%" (namestring (merge-pathnames ".git/worktrees/wt" repo))))
    (is (equal "feat/x" (nle::folder-branch (namestring repo))) "the checkout's own branch")
    (is (equal "feat/x" (nle::folder-branch deep)) "a folder deep inside reads the checkout above it")
    (is (equal "acme/widget" (nle::folder-github-repository deep)) "and the repository its origin pushes to")
    (is (equal "wt-branch" (nle::folder-branch (namestring tree))) "a linked worktree reads its own HEAD")
    (is (equal "acme/widget" (nle::folder-github-repository (namestring tree))) "and the repository's config through commondir")
    (is (null (nle::folder-branch (namestring detached))) "a detached HEAD has no branch")
    (is (null (nle::folder-branch plain)) "a folder outside every checkout has none")
    (is (null (nle::folder-github-repository plain)) "nor a repository")
    (is (null (nle::folder-branch "")) "no folder, no branch")))

(deftest pull-request-remotes-name-github-repositories ()
  (dolist (url '("git@github.com:acme/widget.git" "git@github.com:acme/widget"
                 "https://github.com/acme/widget.git" "https://github.com/acme/widget/"
                 "ssh://git@github.com/acme/widget.git" "https://x-access-token:t0k@github.com/acme/widget.git"))
    (is (equal "acme/widget" (nle::github-repository-of-url url)) url))
  (dolist (url '("git@gitlab.com:acme/widget.git" "https://notgithub.com/acme/widget"
                 "https://github.com/acme" "https://github.com/acme/widget/tree/main" "/srv/git/widget.git"))
    (is (null (nle::github-repository-of-url url)) url)))

(deftest pull-request-token-is-gh-active-account (with-temp-file (path :type "yml" :contents (format nil "gitlab.com:~%    oauth_token: not-this~%github.com:~%    users:~%        alice:~%            oauth_token: gho_alice~%        bob:~%            oauth_token: gho_bob~%~%    git_protocol: https~%    oauth_token: gho_bob~%    user: bob~%")))
  (is (equal "gho_bob" (nle::gh-hosts-token path)) "the host's own token is the active account's")
  (is (null (nle::gh-hosts-token "/nonexistent/hosts.yml")) "no file, no token"))

(deftest pull-request-nodes-fold-to-state-and-checks ()
  (flet ((fold (json) (nle::pull-request-of (nlk:decode-json json))))
    (let ((open (fold "{\"number\":75,\"state\":\"OPEN\",\"isDraft\":false,\"url\":\"https://github.com/acme/widget/pull/75\",\"commits\":{\"nodes\":[{\"commit\":{\"statusCheckRollup\":{\"state\":\"SUCCESS\"}}}]}}")))
      (is (equal '(:number 75 :state "open" :url "https://github.com/acme/widget/pull/75" :checks "passing") open)))
    (is (equal "draft" (getf (fold "{\"number\":1,\"state\":\"OPEN\",\"isDraft\":true,\"url\":\"u\"}") :state)))
    (is (equal "merged" (getf (fold "{\"number\":1,\"state\":\"MERGED\",\"isDraft\":false,\"url\":\"u\"}") :state)))
    (is (equal "closed" (getf (fold "{\"number\":1,\"state\":\"CLOSED\",\"isDraft\":true,\"url\":\"u\"}") :state)) "a closed draft is closed")
    (is (equal "failing" (getf (fold "{\"number\":1,\"state\":\"OPEN\",\"commits\":{\"nodes\":[{\"commit\":{\"statusCheckRollup\":{\"state\":\"FAILURE\"}}}]}}") :checks)))
    (is (equal "pending" (getf (fold "{\"number\":1,\"state\":\"OPEN\",\"commits\":{\"nodes\":[{\"commit\":{\"statusCheckRollup\":{\"state\":\"PENDING\"}}}]}}") :checks)))
    (is (null (getf (fold "{\"number\":1,\"state\":\"OPEN\",\"commits\":{\"nodes\":[{\"commit\":{\"statusCheckRollup\":null}}]}}") :checks)) "a head with no checks has none")))

(deftest pull-request-reads-never-wait-on-github (with-temp-trees ((root "pulls")))
  (let* ((asks '())
         (lock (bt2:make-lock))
         (repo (namestring (fake-checkout (merge-pathnames "repo/" root))))
         (other (namestring (fake-checkout (merge-pathnames "other/" root) :branch "no-pr")))
         (pull '(:number 7 :state "draft" :url "https://github.com/acme/widget/pull/7" :checks "pending")))
    (with-pull-request-cache (:fetcher (lambda (owner-name branches)
                                         (sleep 1)
                                         (bt2:with-lock-held (lock) (push (cons owner-name branches) asks))
                                         (list (cons "feat/x" pull) (cons "no-pr" nil))))
      (let ((started (get-internal-real-time)))
        (is (null (nle::session-pull-request repo)) "a cold key answers nothing yet")
        (is (null (nle::session-pull-request other)))
        (is (< (- (get-internal-real-time) started) (* 0.2 internal-time-units-per-second)) "and does not wait for the second-long ask"))
      (is-eventually (equal pull (nle::session-pull-request repo)) "the worker's answer is read next time")
      (is (null (nle::session-pull-request other)) "a branch with no pull request reads none")
      (is (equal '(("acme/widget" "feat/x" "no-pr")) (mapcar (lambda (ask) (cons (car ask) (sort (copy-list (cdr ask)) #'string<))) asks)) "one ask for both branches of the repository")
      (setf nle::*pull-request-fetcher* (lambda (owner-name branches) (declare (ignore owner-name branches)) :failed))
      (setf (second (gethash '("acme/widget" . "feat/x") nle::*pull-requests*)) 0)
      (nle::session-pull-request repo)
      (is-eventually (plusp (second (gethash '("acme/widget" . "feat/x") nle::*pull-requests*)))
                     "a stale key is asked again")
      (is (equal pull (nle::session-pull-request repo)) "and a failed ask keeps what was held"))))

(deftest pull-request-rides-the-session-row (with-temp-gateway (port))
  (with-temp-trees ((root "pulls"))
    (let ((repo (namestring (fake-checkout (merge-pathnames "repo/" root))))
          (plain (namestring (ensure-directories-exist (merge-pathnames "plain/" root)))))
      (with-pull-request-cache (:fetcher (lambda (owner-name branches) (declare (ignore owner-name branches)) :failed))
        (setf (gethash '("acme/widget" . "feat/x") nle::*pull-requests*)
              (list '(:number 75 :state "open" :url "https://github.com/acme/widget/pull/75" :checks nil)
                    (nlk:unix-now) 600))
        (nlk:create-session :id "pr-on" :cwd repo)
        (nlk:create-session :id "pr-off" :cwd plain)
        (with-gateway-http (port :get "/api/gateway/sessions" :status 200 :says "the directory lists")
          (is-present (row (wire-row "pr-on" body)) "the session in the checkout is listed"
            (is-shape (gethash "pr" row) ("number" = 75) ("state" "open")
              ("url" "https://github.com/acme/widget/pull/75"))
            (is (eq :null (gethash "checks" (gethash "pr" row) :absent)) "no checks reads null"))
          (is-present (row (wire-row "pr-off" body)) "the session outside a checkout is listed"
            (is (member (gethash "pr" row :absent) '(nil :null)) "with a null pr")))))))

(deftest pull-request-push-asks-again (with-temp-trees ((root "pulls")))
  (let* ((asks 0)
         (lock (bt2:make-lock))
         (repo (fake-checkout (merge-pathnames "repo/" root)))
         (folder (namestring repo))
         (main (fake-checkout (merge-pathnames "main/" root) :branch "main"))
         (one (make-string 40 :initial-element #\a))
         (two (make-string 40 :initial-element #\b)))
    (flet ((fresh-for (branch) (third (gethash (cons "acme/widget" branch) nle::*pull-requests*)))
           (asked () (bt2:with-lock-held (lock) asks)))
      (write-temp-file (merge-pathnames ".git/refs/remotes/origin/HEAD" repo) (format nil "ref: refs/remotes/origin/develop~%"))
      (with-pull-request-cache (:fetcher (lambda (owner-name branches)
                                           (declare (ignore owner-name))
                                           (bt2:with-lock-held (lock) (incf asks))
                                           (mapcar (lambda (branch) (cons branch nil)) branches)))
        ;; Following a pushed branch, and watching its refs, are P9's; here
        ;; every read is the only ask.
        (setf nle::+pull-request-follow-for+ 0
              nle::+pull-request-watch-for+ 0)
        (is (null (nle::folder-push-stamp folder "feat/x")) "a branch no remote holds has no stamp")
        (nle::session-pull-request folder)
        (is-eventually (eql 300 (fresh-for "feat/x")) "none on an unpushed branch: five minutes")
        (nle::session-pull-request folder)
        (sleep 0.5)
        (is (= 1 (asked)) "a fresh answer is not asked again")
        (write-temp-file (merge-pathnames ".git/refs/remotes/origin/feat/x" repo) (format nil "~a~%" one))
        (is (equal `(("origin" . ,one)) (nle::folder-push-stamp folder "feat/x")) "the loose ref a push writes")
        (nle::session-pull-request folder)
        (is-eventually (= 2 (asked)) "a push moves the ref: the next read asks again")
        (is-eventually (eql 30 (fresh-for "feat/x")) "none on a pushed branch: half a minute")
        (nle::session-pull-request folder)
        (sleep 0.5)
        (is (= 2 (asked)) "the same ref asks nothing")
        (delete-file (merge-pathnames ".git/refs/remotes/origin/feat/x" repo))
        (write-temp-file (merge-pathnames ".git/packed-refs" repo)
                         (format nil "# pack-refs with: peeled fully-peeled sorted~%~a refs/remotes/origin/feat/x~%" two))
        (is (equal `(("origin" . ,two)) (nle::folder-push-stamp folder "feat/x")) "a packed ref reads the same")
        (nle::session-pull-request folder)
        (is-eventually (= 3 (asked)) "and moving it asks again")
        (write-temp-file (merge-pathnames ".git/packed-refs" main)
                         (format nil "~a refs/remotes/origin/main~%" one))
        (is (nth-value 1 (nle::folder-push-stamp (namestring main) "main")) "main, with no remote HEAD, is the default")
        (is (nth-value 1 (nle::folder-push-stamp folder "develop")) "a remote HEAD names the default")
        (is (not (nth-value 1 (nle::folder-push-stamp folder "main"))) "and then main is none")
        (nle::session-pull-request (namestring main))
        (is-eventually (eql 300 (fresh-for "main")) "none on the pushed default branch: five minutes")))))

(deftest pull-request-change-tells-every-client ()
  (let ((kinds '())
        (lock (bt2:make-lock))
        (pull '(:number 9 :state "open" :url "https://github.com/acme/widget/pull/9" :checks nil)))
    (nle:hook :frame "pulls-moved-test"
              (lambda (op next)
                (bt2:with-lock-held (lock) (push (getf op :kind) kinds))
                (funcall next op)))
    (nlk:with-cleanup ((nle:unhook :frame "pulls-moved-test"))
      (with-pull-request-cache (:fetcher (lambda (owner-name branches) (declare (ignore owner-name branches)) :failed))
        (flet ((told (answer)
                 (bt2:with-lock-held (lock) (setf kinds '()))
                 (nle::store-pull-requests "acme/widget" '(("feat/x" . t)) answer)
                 (bt2:with-lock-held (lock) (count "session_list_changed" kinds :test #'equal))))
          (is (= 0 (told '(("feat/x")))) "none where the row read none tells nobody")
          (is (= 1 (told `(("feat/x" . ,pull)))) "a pull request appearing tells every client once")
          (is (= 0 (told `(("feat/x" . ,pull)))) "the same answer again tells nobody")
          (is (= 0 (told :failed)) "no answer tells nobody")
          (is (equal pull (first (gethash '("acme/widget" . "feat/x") nle::*pull-requests*))) "and keeps what was held")
          (is (= 1 (told `(("feat/x" . ,(list :number 9 :state "merged" :url "https://github.com/acme/widget/pull/9" :checks nil))))) "a merge tells them"))))))

(deftest pull-request-push-is-followed (with-temp-trees ((root "pulls")))
  (let* ((asks 0)
         (lock (bt2:make-lock))
         (kinds '())
         (repo (fake-checkout (merge-pathnames "repo/" root)))
         (folder (namestring repo))
         (pull '(:number 12 :state "open" :url "https://github.com/acme/widget/pull/12" :checks nil)))
    (flet ((asked () (bt2:with-lock-held (lock) asks)))
      (nle:hook :frame "pulls-follow-test"
                (lambda (op next)
                  (bt2:with-lock-held (lock) (push (getf op :kind) kinds))
                  (funcall next op)))
      (nlk:with-cleanup ((nle:unhook :frame "pulls-follow-test"))
        ;; GitHub knows the pull request from the fourth ask on: opened a
        ;; moment after the push.
        (with-pull-request-cache (:fetcher (lambda (owner-name branches)
                                             (declare (ignore owner-name))
                                             (let ((n (bt2:with-lock-held (lock) (incf asks))))
                                               (mapcar (lambda (branch) (cons branch (and (>= n 4) pull))) branches))))
          (nle::session-pull-request folder)
          (is-eventually (= 1 (asked)) "a cold key is asked once")
          (is-eventually (null nle::*pull-requests-worker*) "an unpushed branch is not followed: the worker leaves")
          ;; A row read the checkout a moment ago, as a page's poll does: the
          ;; worker re-reads its refs, and sees the push with no read at all.
          (setf nle::+pull-request-watch-for+ 30)
          (nle::session-pull-request folder)
          (write-temp-file (merge-pathnames ".git/refs/remotes/origin/feat/x" repo)
                           (format nil "~a~%" (make-string 40 :initial-element #\c)))
          (is-eventually (equal pull (first (gethash '("acme/widget" . "feat/x") nle::*pull-requests*)))
                         "the worker sees the push and asks on its own until the pull request is there")
          (is (= 4 (asked)) "with no read between: the push, then two follows")
          (setf nle::+pull-request-watch-for+ 1)
          (is (member "session_list_changed" (bt2:with-lock-held (lock) kinds) :test #'equal) "and every client is told")
          (is (zerop (hash-table-count nle::*pull-request-follows*)) "the follow ends with it")
          (is-eventually (null nle::*pull-requests-worker*) "and the worker leaves")
          (setf (gethash '("acme/widget" . "feat/y") nle::*pull-request-follows*) (+ (nlk:unix-now) 1))
          (setf nle::*pull-request-fetcher* (lambda (owner-name branches) (declare (ignore owner-name)) (mapcar #'list branches)))
          (setf (second (gethash '("acme/widget" . "feat/x") nle::*pull-requests*)) 0)
          (nle::session-pull-request folder)
          (is-eventually (zerop (hash-table-count nle::*pull-request-follows*)) "a follow with no pull request runs out")
          (is-eventually (null nle::*pull-requests-worker*) "and then the worker leaves too"))))))
