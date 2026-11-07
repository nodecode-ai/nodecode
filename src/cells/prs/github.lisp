;;;; github.lisp --- the cell's one network seam, and what it reads back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves to GitHub goes through HTTP, so a test stubs
;;;; that one name and never opens a socket. API answers values for a 2xx or a
;;;; 304 and refuses (PRS-ERROR, through REDACT) for anything else, in
;;;; GitHub's own words, so the model reads why a review was not taken.
;;;;
;;;; Two things are read back often enough to be kept: the files a PR touches
;;;; at a head (a head's files never change, so they are kept by sha), and the
;;;; newest rank a PR was given, found by the marker every rank's review
;;;; carries -- GitHub is the record of what was ranked, so a restart, a rank
;;;; by hand and another organism's rank all read the same.

(in-package #:nodecode-prs)

(defparameter +api+ "https://api.github.com"
  "Where the REST API answers.")

(defun http (method url &rest arguments)
  "The cell's one network call, NLK:HTTP; tests stub this name."
  (apply #'nlk:http method url arguments))

(defun token ()
  (or (getf (running-settings) :token)
      (fail "no GitHub token: set token_env or token_file in the prs section of ~~/.nodecode/config.jsonc, ~
             then (restart-cells \"nodecode-prs\")")))

(defun github-message (body)
  "What GitHub said in an error BODY: its message and each error's own."
  (let ((json (ignore-errors (nlk:decode-json body))))
    (if (hash-table-p json)
        (format nil "~a~{; ~a~}" (nlk:json-value json :string "message")
                (loop for error across (nlk:json-array json "errors")
                      collect (or (nlk:json-value error :string "message")
                                  (nlk:one-line (nlk:encode-json-object error) :cap 200))))
        (nlk:one-line (if (stringp body) body "") :cap 300))))

(defun api (method path &key body etag)
  "One GitHub REST call: => (values JSON STATUS HEADERS), JSON NIL on a 304
or an empty answer."
  ;; PATH is absolute (\"/repos/...\") or a whole URL, a Link header's next
  ;; page. A status of 400 and up refuses with GitHub's words.
  (let ((headers `(("authorization" . ,(format nil "Bearer ~a" (token)))
                   ("accept" . "application/vnd.github+json")
                   ("x-github-api-version" . "2022-11-28")
                   ("user-agent" . "nodecode-prs")
                   ,@(and etag `(("if-none-match" . ,etag)))
                   ,@(and body '(("content-type" . "application/json"))))))
    (multiple-value-bind (text status answer-headers)
        ;; A read GitHub answered 5xx is asked once more: a read can fail
        ;; once and succeed on retry.
        (loop for attempt from 1
              do (multiple-value-bind (text status answer-headers)
                     (handler-case
                         (http method (if (uiop:string-prefix-p "https://" path) path (concatenate 'string +api+ path))
                               :headers headers :content (and body (nlk:encode-json-object body)) :timeout 30)
                       (nlk:turn-cancelled-condition (condition) (error condition))
                       (error (condition) (fail "GitHub did not answer ~a ~a: ~a" method path condition)))
                   (unless (and (eq method :get) (= attempt 1) (integerp status) (>= status 500))
                     (return (values text status answer-headers)))))
      (cond ((eql status 304) (values nil status answer-headers))
            ((and (integerp status) (< status 400))
             (values (and text (plusp (length text)) (nlk:decode-json text)) status answer-headers))
            (t (fail "GitHub answered ~a to ~a ~a: ~a" status method path (github-message text)))))))

(defun api-all (path &key etag)
  "Every page of the list at PATH as one list: => (values ITEMS STATUS
HEADERS), the first page's STATUS and HEADERS; ITEMS NIL on a 304."
  (multiple-value-bind (page status headers) (api :get path :etag etag)
    (let ((items (coerce (or page #()) 'list))
          (next (next-page headers)))
      (loop while next
            do (multiple-value-bind (more more-status more-headers) (api :get next)
                 (declare (ignore more-status))
                 (setf items (append items (coerce (or more #()) 'list))
                       next (next-page more-headers))))
      (values items status headers))))

(defun next-page (headers)
  "The URL a Link header names as rel=\"next\", or NIL."
  (let ((link (and (hash-table-p headers) (gethash "link" headers))))
    (and (stringp link)
         (ppcre:register-groups-bind (url) ("<([^>]+)>;\\s*rel=\"next\"" link) url))))

;;; --- names ---------------------------------------------------------------------

(defun check-repo (repo)
  (unless (and (stringp repo) (ppcre:scan "^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$" repo))
    (fail "a repository is \"owner/name\", not ~s" repo))
  repo)

(defun path (repo control &rest arguments)
  "\"/repos/REPO\" and CONTROL formatted over ARGUMENTS."
  (format nil "/repos/~a~?" (check-repo repo) control arguments))

(defun key (repo n) (format nil "~a#~d" repo n))

(defun short (sha) (subseq sha 0 (min 7 (length sha))))

;;; --- a pull request, as GitHub answers it -------------------------------------------

(defun pull (repo n)
  "PR N of REPO."
  (unless (and (integerp n) (plusp n))
    (fail "a pull request is a positive number, not ~s" n))
  (api :get (path repo "/pulls/~d" n)))

(defun number-of (pull) (nlk:json-value pull :integer "number"))
(defun head-of (pull) (nlk:json-value pull :string "head" "sha"))
(defun author-of (pull) (nlk:json-value pull :string "user" "login"))
(defun title-of (pull) (nlk:json-value pull :string "title"))

(defun label-names (pull)
  (loop for label across (nlk:json-array pull "labels") collect (nlk:json-value label :string "name")))

(defun open-pulls (repo)
  "REPO's open PRs, oldest first."
  (api-all (path repo "/pulls?state=open&sort=created&direction=asc&per_page=100")))

;;; --- kept by head ----------------------------------------------------------------------

(defvar *files* (make-hash-table :test #'equal)
  "\"repo#n@sha\" -> the files PR n touches at that head. Guarded by *LOCK*.")

(defvar *marks* (make-hash-table :test #'equal)
  "\"repo#n\" -> (HEAD . MARK): the newest rank found while the PR's head was
HEAD, MARK NIL for none. Guarded by *LOCK*.")

(defun pull-files (repo n head)
  "The files PR N of REPO touches at HEAD, with their patches."
  (let ((key (format nil "~a@~a" (key repo n) head)))
    (or (with-prs-lock (gethash key *files*))
        (let ((files (api-all (path repo "/pulls/~d/files?per_page=100" n))))
          (with-prs-lock (setf (gethash key *files*) files))))))

(defun file-names (files)
  (mapcar (lambda (file) (nlk:json-value file :string "filename")) files))

;;; A rank's review ends with this line; it is the only record of a rank.
(defparameter +marker+ "<!-- nodecode-prs sha=~a rank=~a -->")

(defun marker-of (text)
  "(values SHA RANK) the marker in TEXT names, or NIL."
  (and (stringp text)
       (ppcre:register-groups-bind (sha rank)
           ("<!-- nodecode-prs sha=([0-9a-f]{40}) rank=(p[0-3]) -->" text)
         (values sha rank))))

(defun last-mark (repo n)
  "The newest review on PR N carrying the marker: (:SHA :RANK :TEXT :URL), or NIL."
  (let ((found nil))
    (dolist (review (api-all (path repo "/pulls/~d/reviews?per_page=100" n)) found)
      (let ((body (nlk:json-value review :string "body")))
        (multiple-value-bind (sha rank) (marker-of body)
          (when sha
            (setf found (list :sha sha :rank rank
                              :text (nlk:trimmed (ppcre:regex-replace "<!-- nodecode-prs [^>]*-->" body ""))
                              :url (nlk:json-value review :string "html_url")))))))))

(defun mark (repo n head)
  "The newest rank PR N of REPO carries, read once per HEAD."
  (let ((entry (with-prs-lock (gethash (key repo n) *marks*))))
    (if (and entry (equal (car entry) head))
        (cdr entry)
        (let ((mark (last-mark repo n)))
          (with-prs-lock (setf (gethash (key repo n) *marks*) (cons head mark)))
          mark))))
