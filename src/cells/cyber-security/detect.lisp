;;;; detect.lisp --- what is installed here, and whether it is a fixed build.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One check is one JSON object of the feed, its KIND naming how it reads
;;;; the machine. Every kind answers the same plist,
;;;;   (:status :vulnerable|:ok|:absent|:skipped|:error :found TEXT :why TEXT)
;;;; and every kind reads through *RUN* or the file system, so a test stubs
;;;; one name and never touches the machine.
;;;;
;;;;   version    run COMMAND, read the first version in its output
;;;;   npm        a package's version in DIR's package-lock.json / node_modules
;;;;   composer   a package's version in DIR's composer.lock
;;;;   wp-plugin  a WordPress plugin's Version: header under DIR/wp-content/plugins
;;;;   command    run COMMAND; output (or output matching MATCH) is a hit
;;;;   files      any path matching one of PATHS is a hit
;;;;
;;;; A version is judged against the check's own lists: BAD (exact builds,
;;;; "*" for any), MIN (older is unaffected) and FIXED (one entry per
;;;; release line). A version on a line FIXED names is vulnerable below that
;;;; entry; a version on a line it does not name is vulnerable when older
;;;; than the newest fix (an end-of-life line) and fine when newer. A line is
;;;; the first LINE parts of a version: 2 by default (PHP 8.3, GitLab 19.4),
;;;; 1 where a project numbers its lines by major alone (Node 24).

(in-package #:nodecode-cyber-security)

(defun run-command (argv)
  "Run ARGV with no shell of ours => (VALUES OUTPUT EXIT). A program that
is not installed answers exit 127, as a shell would."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program argv :output :string :error-output nil
                                 :ignore-error-status t :input nil)
        (declare (ignore err))
        (values (or out "") code))
    (error () (values "" 127))))

(defparameter *run* 'run-command
  "How a check runs a command: (funcall *RUN* ARGV) => (VALUES OUTPUT EXIT).")

;;; --- versions ------------------------------------------------------------------

(defun parse-version (text)
  "The first dotted version in TEXT as a list of integers, or NIL."
  (let ((m (and text (cl-ppcre:scan-to-strings "\\d+(?:\\.\\d+)+" text))))
    (when m (mapcar #'parse-integer (cl-ppcre:split "\\." m)))))

(defun version< (a b)
  "A is an older version than B (lists of integers, missing parts are 0)."
  (loop for i from 0 below (max (length a) (length b))
        for x = (or (nth i a) 0) and y = (or (nth i b) 0)
        when (< x y) return t
        when (> x y) return nil))

(defun version-string (v) (format nil "~{~d~^.~}" v))

(defun same-line-p (a b &optional (depth 2))
  "A and B share a release line: their first DEPTH parts agree (2 for PHP's
8.3, 1 for Node's 24)."
  (loop for i below depth always (eql (or (nth i a) 0) (or (nth i b) 0))))

(defun judge (version check)
  "VERSION (a list) against CHECK's bad/min/fixed => (VALUES STATUS WHY)."
  (let ((bad (field check "bad"))
        (min (parse-version (field check "min")))
        (fixed (remove nil (mapcar #'parse-version (field check "fixed"))))
        (shown (version-string version)))
    (cond ((member "*" bad :test #'equal)
           (values :vulnerable (format nil "~a is installed" shown)))
          ((some (lambda (b) (equal (parse-version b) version)) bad)
           (values :vulnerable (format nil "~a is the bad build" shown)))
          ((and min (version< version min))
           (values :ok (format nil "~a predates the affected range" shown)))
          ((null fixed)
           (values :ok (format nil "~a is not a bad build" shown)))
          (t
           (let ((line (let ((depth (field check "line" 2)))
                         (find-if (lambda (f) (same-line-p f version depth)) fixed)))
                 (newest (reduce (lambda (a b) (if (version< a b) b a)) fixed)))
             (cond (line
                    (if (version< version line)
                        (values :vulnerable (format nil "~a < ~a" shown (version-string line)))
                        (values :ok (format nil "~a >= ~a" shown (version-string line)))))
                   ((version< version newest)
                    (values :vulnerable (format nil "~a is on a line with no fix; move to ~{~a~^ / ~}"
                                                shown (mapcar #'version-string fixed))))
                   (t (values :ok (format nil "~a is newer than every fix" shown)))))))))

;;; --- placeholders -------------------------------------------------------------

(defun fill-in (string dir host)
  "STRING with {dir} and {host} replaced."
  (let ((s string))
    (when dir (setf s (cl-ppcre:regex-replace-all "\\{dir\\}" s (string-right-trim "/" dir))))
    (when host (setf s (cl-ppcre:regex-replace-all "\\{host\\}" s host)))
    s))

(defun needs (check)
  "What CHECK cannot run without: :dir, :host or NIL."
  (let ((n (field check "needs"))
        (kind (field check "kind")))
    (cond ((equal n "dir") :dir)
          ((equal n "host") :host)
          ((member kind '("npm" "composer" "wp-plugin") :test #'equal) :dir))))

;;; --- the kinds ----------------------------------------------------------------

(defun read-json-file (path)
  (when (probe-file path)
    (handler-case (shasht:read-json (uiop:read-file-string path)) (error () nil))))

(defun npm-version (dir package)
  "PACKAGE's installed version under DIR, from the lockfile or node_modules."
  (let* ((lock (read-json-file (merge-pathnames "package-lock.json" (uiop:ensure-directory-pathname dir))))
         (packages (and (hash-table-p lock) (gethash "packages" lock)))
         (entry (and (hash-table-p packages)
                     (gethash (concatenate 'string "node_modules/" package) packages))))
    (or (and (hash-table-p entry) (gethash "version" entry))
        (let ((pj (read-json-file (merge-pathnames (concatenate 'string "node_modules/" package "/package.json")
                                                   (uiop:ensure-directory-pathname dir)))))
          (and (hash-table-p pj) (gethash "version" pj))))))

(defun composer-version (dir packages)
  "The first of PACKAGES found in DIR's composer.lock, as its version string."
  (let ((lock (read-json-file (merge-pathnames "composer.lock" (uiop:ensure-directory-pathname dir)))))
    (when (hash-table-p lock)
      (loop for p across (or (gethash "packages" lock) #())
            when (and (hash-table-p p) (member (gethash "name" p) packages :test #'equal))
              return (string-left-trim "vV" (gethash "version" p))))))

(defun wp-plugin-version (dir slugs)
  "The Version: header of the first plugin folder in SLUGS under DIR."
  (dolist (slug slugs)
    (let ((folder (merge-pathnames (format nil "wp-content/plugins/~a/" slug)
                                   (uiop:ensure-directory-pathname dir))))
      (dolist (file (directory (merge-pathnames "*.php" folder)))
        (let ((v (cl-ppcre:register-groups-bind (v)
                     ("(?im)^[ \\t/*#@]*Version:\\s*([0-9][0-9.]*)"
                      (handler-case (uiop:read-file-string file) (error () "")))
                   v)))
          (when v (return-from wp-plugin-version v)))))))

(defun run-check (check &key dir host)
  "One CHECK of the feed against this machine => (:status :found :why)."
  (let ((kind (field check "kind"))
        (need (needs check)))
    (flet ((argv () (mapcar (lambda (a) (fill-in a dir host)) (field check "command")))
           (versioned (text)
             (let ((v (parse-version text)))
               (if (null v)
                   (list :status :absent :why "not installed here")
                   (multiple-value-bind (status why) (judge v check)
                     (list :status status :found (version-string v) :why why))))))
      (cond
        ((and (eq need :dir) (null dir))
         (list :status :skipped :why "needs the site's folder (:dir)"))
        ((and (eq need :host) (null host))
         (list :status :skipped :why "needs the site's host name (:host)"))
        ((equal kind "version")
         (multiple-value-bind (out code) (funcall *run* (argv))
           (if (or (/= code 0) (null (parse-version out)))
               (list :status :absent :why "not installed here")
               (let ((result (versioned out)))
                 ;; A distro build backports the fix under the old number.
                 (if (and (eq (getf result :status) :vulnerable) (field check "backport"))
                     (let ((proof (string-trim '(#\Space #\Newline #\Tab)
                                               (funcall *run* (mapcar (lambda (a) (fill-in a dir host))
                                                                      (field check "backport"))))))
                       (if (plusp (length proof))
                           (list :status :ok :found (getf result :found)
                                 :why (format nil "distro build carries the fix (~a)"
                                              (subseq proof 0 (min 80 (length proof)))))
                           result))
                     result)))))
        ((equal kind "npm")
         (let ((v (npm-version dir (field check "package"))))
           (if v (versioned v) (list :status :absent :why "not in this project"))))
        ((equal kind "composer")
         (let ((v (composer-version dir (field check "packages"))))
           (if v (versioned v) (list :status :absent :why "not in this project"))))
        ((equal kind "wp-plugin")
         (let ((v (wp-plugin-version dir (field check "slugs"))))
           (if v (versioned v) (list :status :absent :why "plugin not installed"))))
        ((equal kind "command")
         (multiple-value-bind (out code) (funcall *run* (argv))
           (let ((text (string-trim '(#\Space #\Newline #\Tab #\Return) out))
                 (match (field check "match")))
             (cond ((= code 127) (list :status :absent :why "its tool is not installed here"))
                   ((zerop (length text)) (list :status :ok :why "nothing found"))
                   ((and match (not (cl-ppcre:scan match text)))
                    (list :status :ok :found (first-line text) :why "no match"))
                   (t (list :status :vulnerable :found (first-line text) :why "found"))))))
        ((equal kind "files")
         (let ((hits (loop for p in (field check "paths")
                           append (directory (fill-in p dir host)))))
           (if hits
               (list :status :vulnerable :found (namestring (first hits)) :why "found")
               (list :status :ok :why "nothing found"))))
        (t (list :status :error :why (format nil "unknown check kind ~a" kind)))))))

(defun first-line (text)
  (let ((nl (position #\Newline text)))
    (subseq text 0 (min 160 (or nl (length text))))))
