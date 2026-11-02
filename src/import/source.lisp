;;;; source.lisp --- a repository as an import source
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A repository brings its skills and MCP servers the way a harness home
;;;; does: the walk reads it as a path home, the plan says what lands, and
;;;; APPLY-PLAN lands each through the seam that owns its kind. It is named the
;;;; way people paste one -- owner/repo, github:owner/repo, an https URL, a
;;;; GitHub tree URL, file:///path for a repository on this box -- a folder of
;;;; it after the repository's name, and #REF for a branch, a tag or a commit.
;;;;
;;;; The fetch clones it at that commit into sources/ in this organism's home,
;;;; the folder named host/owner/repo@COMMIT with its .git gone: the folder is
;;;; the pin, and the scripts and references a skill names stay there to be
;;;; read. That name, and the folder of it a source names, is the label every
;;;; skill it brings carries as its :origin -- the index ranks it as an
;;;; import's until this organism's own use vouches for it, and REMOVE-SOURCE
;;;; takes it back by that. A repository is somebody else's project, not a
;;;; harness home: its CLAUDE.md and memory are its maintainers', so a source
;;;; plans skills and MCP servers alone, and its servers land off as every
;;;; imported server does.

(in-package #:nodecode.import)

(defparameter +source-kinds+ '("skills" "mcp")
  "The kinds a repository source carries.")

(defparameter +source-marker+ ".source"
  "The file a fetched source keeps its URL and commit in.")

;;; --- the name ---------------------------------------------------------------

(defun name-segment-p (text)
  "Whether TEXT is one segment of a repository's name: a host, an owner, the
repository, a folder of it."
  (and (plusp (length text))
       (not (member text '("." "..") :test #'string=))
       (every (lambda (ch) (or (alphanumericp ch) (find ch "._-"))) text)))

(defun parse-source (text &aux (text (if (stringp text) (string-trim '(#\Space #\Tab) text) "")))
  "TEXT as a repository: (:URL :NAME :REF :PATH), NAME the host/owner/repo it
is kept and labelled under, REF what #REF names, PATH the folder of it named
after the repository; NIL when TEXT names none -- a world, or a path that is
there. An https URL that is no repository's is an error, never a path."
  (when (plusp (length text))
    (let* ((hash (position #\# text))
           (ref (and hash (subseq text (1+ hash))))
           (text (if hash (subseq text 0 hash) text)))
      (labels ((segments (string)
                 (remove "" (uiop:split-string string :separator "/") :test #'string=))
               (bare (repo)
                 (if (and (> (length repo) 4) (string-equal ".git" repo :start2 (- (length repo) 4)))
                     (subseq repo 0 (- (length repo) 4))
                     repo))
               (named (url host owner repo path)
                 (let ((repo (bare repo)))
                   (and (every #'name-segment-p (list* host owner repo path))
                        (or (null ref) (plusp (length ref)))
                        (list :url (or url (format nil "https://~a/~a/~a" host owner repo))
                              :name (format nil "~a/~a/~a" host owner repo)
                              :ref ref
                              :path (and path (format nil "~{~a~^/~}" path)))))))
        (cond
          ((uiop:string-prefix-p "file://" text)
           (let ((parts (segments (subseq text 7))))
             (or (and (>= (length parts) 2)
                      (named text "local" (car (last parts 2)) (car (last parts)) nil))
                 (fail "~a names no repository: file:///path/to/repo is one on this box" text))))
          ((uiop:string-prefix-p "https://" text)
           ;; https://HOST/OWNER/REPO, and a GitHub tree or blob URL's /tree/REF/FOLDER
           (destructuring-bind (&optional host owner repo &rest more) (segments (subseq text 8))
             (let ((tree (and (member (first more) '("tree" "blob") :test #'string=) (rest more))))
               (when tree
                 (setf ref (or ref (first tree))))
               (or (and host owner repo (or (null more) tree)
                        (named nil host owner repo (rest tree)))
                   (fail "~a names no repository: https://HOST/OWNER/REPO, or a GitHub ~
                          /tree/REF/FOLDER URL" text)))))
          ((uiop:string-prefix-p "github:" text)
           (let ((parts (segments (subseq text 7))))
             (or (and (>= (length parts) 2)
                      (named nil "github.com" (first parts) (second parts) (cddr parts)))
                 (fail "~a names no repository: github:OWNER/REPO" text))))
          ;; OWNER/REPO[/FOLDER] is GitHub's, when nothing of that name is here
          ((and (not (find (char text 0) "./~"))
                (find #\/ text)
                (not (ignore-errors (probe-file (nlk:expand-home text)))))
           (let ((parts (segments text)))
             (and (>= (length parts) 2)
                  (named nil "github.com" (first parts) (second parts) (cddr parts))))))))))

(defun source-kinds (only &aux (asked (kind-list only)))
  "ONLY, the kinds asked of a repository source, narrowed to the ones a
repository carries."
  (if (null asked)
      (copy-list +source-kinds+)
      (or (intersection asked +source-kinds+ :test #'string=)
          (fail "a repository brings ~{~a~^ and ~} alone, not ~{~a~^, ~}" +source-kinds+ asked))))

;;; --- the fetch --------------------------------------------------------------

(defun sources-directory ()
  "Where fetched sources are kept: sources/ in this organism's home."
  (merge-pathnames "sources/" (uiop:ensure-directory-pathname (getf (import-settings) :home))))

(defun source-git (directory &rest arguments)
  "git ARGUMENTS run in DIRECTORY, prompting for nothing: => (values OUTPUT
STATUS ERROR), each text trimmed."
  (multiple-value-bind (out err status)
      (nlk::run-bounded (list* nlk::*git-program* "-C" (uiop:native-namestring directory) arguments)
                        :seconds 600 :environment '("GIT_TERMINAL_PROMPT=0"))
    (values (string-trim '(#\Newline #\Space) (or out ""))
            status
            (string-trim '(#\Newline #\Space) (or err "")))))

(defun fetch-source (source &aux (root (sources-directory)) (name (getf source :name)))
  "SOURCE cloned at the commit its ref names -- its default branch's head
without one -- into ROOT, once: => (values FOLDER COMMIT), FOLDER
host/owner/repo@COMMIT/ with no .git. The clone runs no hook and fetches no
submodule, and a symbolic link that leaves the repository refuses it."
  (let* ((staging (merge-pathnames (format nil ".staging/~a/" (substitute #\- #\/ name)) root))
         (native (string-right-trim "/" (uiop:native-namestring staging))))
    (flet ((git (directory &rest arguments)
             (multiple-value-bind (out status err) (apply #'source-git directory arguments)
               (unless (eql status 0)
                 (fail "~a: git ~a failed: ~a" name (first arguments) err))
               out))
           (commit-of (ref)
             (multiple-value-bind (out status)
                 (source-git staging "rev-parse" "--verify" "--quiet" (format nil "~a^{commit}" ref))
               (and (eql status 0) (plusp (length out)) out))))
      (when (probe-file staging) (nlk::remove-tree staging))
      (ensure-directories-exist (uiop:pathname-parent-directory-pathname staging))
      (unwind-protect
           (progn
             (git root "clone" "--quiet" "--no-checkout" "--filter=blob:none" "--" (getf source :url) native)
             (let* ((ref (getf source :ref))
                    (commit (if ref
                                (or (commit-of ref)
                                    (commit-of (format nil "origin/~a" ref))
                                    (fail "~a has no branch, tag or commit ~a" name ref))
                                (or (commit-of "HEAD") (fail "~a has no commit" name))))
                    (folder (merge-pathnames (format nil "~a@~a/" name (subseq commit 0 12)) root)))
               (unless (probe-file folder)
                 (git staging "checkout" "--quiet" "--detach" commit)
                 (nlk::remove-tree (merge-pathnames ".git/" staging))
                 (let ((leaving (remove-if (lambda (link)
                                             (let ((target (ignore-errors (truename link))))
                                               (and target (uiop:subpathp target (truename staging)))))
                                           (nlk::repository-links staging))))
                   (when leaving
                     (fail "~a carries symbolic link~p that leave~:*~[~;s~:;~] it: ~{~a~^, ~}"
                           name (length leaving) leaving)))
                 (with-open-file (out (merge-pathnames +source-marker+ staging)
                                      :direction :output :if-exists :supersede)
                   (format out "~a~%~a~%" (getf source :url) commit))
                 (ensure-directories-exist (uiop:pathname-parent-directory-pathname folder))
                 (sb-posix:rename native (string-right-trim "/" (uiop:native-namestring folder))))
               (values folder commit)))
        (when (probe-file staging) (ignore-errors (nlk::remove-tree staging)))))))

(defun source-home (source)
  "SOURCE fetched, as the HOME the walk reads: its folder, or the folder of
it SOURCE names, labelled host/owner/repo@COMMIT[/FOLDER]."
  (multiple-value-bind (folder commit) (fetch-source source)
    (let* ((path (getf source :path))
           (root (if path (merge-pathnames (format nil "~a/" path) folder) folder)))
      (unless (and (probe-file root) (uiop:subpathp (truename root) (truename folder)))
        (fail "~a has no folder ~a" (getf source :name) path))
      (make-home :name (format nil "~a@~a~@[/~a~]" (getf source :name) (subseq commit 0 12) path)
                 :roots (list (truename root))))))

;;; --- what a source brought, and taking it back -------------------------------

(defun source-folders (&aux (root (sources-directory)))
  "Every fetched source under ROOT: (LABEL FOLDER URL COMMIT) each."
  (loop for host in (and (probe-file root) (uiop:subdirectories root))
        unless (hidden-name-p (nlk:folder-name host))
          append (loop for owner in (uiop:subdirectories host)
                       append (loop for folder in (uiop:subdirectories owner)
                                    for marker = (probe-file (merge-pathnames +source-marker+ folder))
                                    when marker
                                      collect (destructuring-bind (&optional url commit &rest more)
                                                  (uiop:read-file-lines marker)
                                                (declare (ignore more))
                                                (list (format nil "~a/~a/~a" (nlk:folder-name host)
                                                              (nlk:folder-name owner) (nlk:folder-name folder))
                                                      folder url commit))))))

(defun source-skills (label &aux (inside (format nil "~a/" label)))
  "The names of the definitions whose :origin is LABEL, or a folder of it."
  (loop for entry in (nlk::knowledge-entries)
        for origin = (nlk::knowledge-entry-origin entry)
        when (and (stringp origin) (or (string= origin label) (uiop:string-prefix-p inside origin)))
          collect (nlk::knowledge-entry-name entry)))

(defun source-servers (folder &aux (home (make-home :name "source" :roots (list folder))))
  "The MCP servers FOLDER declares that are configured here as it declares
them: a server of the same name configured otherwise is not the source's."
  (read-homes (list home) :sessions nil :text nil)
  (loop for fact in (home-facts home)
        for name = (fact-id fact)
        for declared = (getf (fact-value fact) :object)
        for configured = (and (eq (fact-kind fact) :mcp)
                              (nlk:json-value (nle:read-shared-config) :object "mcp" "servers" name))
        when (and configured
                  (equal (gethash "command" configured) (gethash "command" declared))
                  (equal (gethash "url" configured) (gethash "url" declared))
                  (equalp (coerce (or (gethash "args" configured) #()) 'list)
                          (coerce (or (gethash "args" declared) #()) 'list)))
          collect name))

(defun forget-definitions (names label)
  "Forget each of NAMES, kept definitions, as one act: their files leave the
knowledge cell in one commit, under LABEL's import session."
  (let* ((*package* (find-package '#:nodecode.evolved))
         (form (cons 'progn (loop for name in names
                                  collect `(fmakunbound ',(intern (string-upcase name) *package*)))))
         (text (prin1-to-string form)))
    (eval form)
    (nlk:record-definitions form text (length text) :session-id (nlk:import-session (list label)))))

(defun sources ()
  "Every repository this organism holds skills or MCP servers from: one line
each -- its label, the skills it brought and the servers it declares that are
configured here."
  (let ((rows (source-folders)))
    (if (null rows)
        "no repository source here: /import OWNER/REPO brings one's skills and MCP servers"
        (format nil "~{~a~^~%~}"
                (loop for (label folder) in rows
                      collect (let ((skills (source-skills label))
                                    (servers (source-servers folder)))
                                (format nil "~a: ~d skill~:p~@[ (~{~a~^, ~})~], ~d server~:p~@[ (~{~a~^, ~})~]"
                                        label (length skills) skills (length servers) servers)))))))

(defun remove-source (label &aux (rows (source-folders)))
  "Take back what the repository source LABEL brought -- each skill whose
origin it is, each MCP server it declares that is configured as it declares
it -- and delete its folder. => what came of it."
  (let ((row (or (find label rows :key #'first :test #'string=)
                 (fail "no source ~a here~@[; there ~:*~[~;is~:;are~] ~]~{~a~^, ~}"
                       label (and rows (length rows)) (mapcar #'first rows)))))
    (destructuring-bind (label folder &rest more) row
      (declare (ignore more))
      (let ((skills (source-skills label))
            (servers (source-servers folder)))
        (when skills (forget-definitions skills label))
        (dolist (name servers)
          (nle:config-set (list "mcp" "servers" name) :remove))
        (nlk::remove-tree folder)
        (format nil "~a removed: ~d skill~:p~@[ (~{~a~^, ~})~], ~d server~:p~@[ (~{~a~^, ~})~], and its folder"
                label (length skills) skills (length servers) servers)))))
