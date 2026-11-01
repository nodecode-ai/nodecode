;;;; files.lisp --- a session's own folder from the web page: a directory
;;;; listed, a file read or saved, a file put there. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The folder is the one the session works in -- its durable directory
;;;; (SESSION-WORKSPACE-DIRECTORY), or the one it was minted with while it
;;;; stands by -- which the page shows as the composer's folder chip and the
;;;; head's where. A path the page names is relative to it, and what it names,
;;;; every link on the way followed (NLK::REAL-PATH), must be the folder or
;;;; lie under it: an absolute path, a step that climbs, and a link that leads
;;;; out are refused before anything is read or written. The listing shows a
;;;; link that stays inside as what it names; one that leads out, or nowhere,
;;;; is listed as a link, and opening it is refused like any other way out.
;;;;
;;;; A file is answered as what it is: text (UTF-8 with no NUL) up to
;;;; *FOLDER-TEXT-BYTES* as text/plain, an image IMAGE-MEDIA-TYPE names up to
;;;; *FOLDER-IMAGE-BYTES* as itself, and anything else as a JSON word saying
;;;; why it is not shown; ?download=1 sends any file to be saved. An upload is
;;;; the request body written beside the others through one rename
;;;; (NLK:WRITE-FILE-ATOMICALLY), so a torn upload leaves nothing; a name
;;;; already there is refused unless the page asked to replace it.
;;;;
;;;; Retention: none. The folder is the operator's, read as it is now.

(in-package #:nodecode.evolved)

(defparameter *folder-text-bytes* (* 1024 1024)
  "The largest file the Files view is sent as text.")

(defparameter *folder-image-bytes* (* 16 1024 1024)
  "The largest image the Files view is sent to draw.")

(defparameter *folder-upload-bytes* (* 100 1024 1024)
  "The largest file an upload writes.")

(defparameter *folder-entries-cap* 2000
  "The most rows one listing sends; the rest are counted.")

(define-error folder-refused (error) (status code detail)
  (:report "~a" detail))

(defun folder-refuse (status code control &rest args)
  "Refuse the request as STATUS with CODE, the formatted text its message."
  (error 'folder-refused :status status :code code :detail (apply #'format nil control args)))

(defun session-workspace-directory (session-id &aux (cwd (nlk:find-session-cwd session-id)))
  "SESSION-ID's durable workspace when it is an absolute directory name, else
NIL."
  ;; Never an empty or a relative one: a store opened over either walks this
  ;; process's own directory, which is a live checkout often enough. On
  ;; Windows an absolute one leads with a drive, C:\ or C:/, or a share's \\.
  (and (stringp cwd) (plusp (length cwd))
       (or (find (char cwd 0) nlk::+path-separators+)
           #+win32 (and (< 2 (length cwd)) (alpha-char-p (char cwd 0)) (char= #\: (char cwd 1))
                        (find (char cwd 2) nlk::+path-separators+)))
       cwd))

(defun session-folder (session-id)
  "SESSION-ID's folder with every link on the way to it followed, as a native
path with no trailing slash. Refused when the session names none, or the one
it names is not a directory any more."
  (let* ((cwd (json-value (or (session-workspace-directory session-id)
                              (when-let (record (nlk:standby-session-record session-id))
                                (nlk:standby-record-cwd record)))
                          :text))
         (real (and cwd (nlk::real-path (uiop:parse-native-namestring cwd :ensure-directory t)))))
    (cond ((not cwd) (folder-refuse 404 "no_folder" "this session names no folder"))
          ((not (and real (uiop:directory-exists-p real)))
           (folder-refuse 404 "no_folder" "this session's folder, ~a, does not exist" cwd))
          (t (nlk::native-path real)))))

(defun folder-query (env key)
  "The value of KEY in ENV's query, percent-decoded, or NIL."
  ;; The one query scanner reads values verbatim; a path or a file's name
  ;; rides percent-encoded, as the page's encodeURIComponent writes it.
  (when-let (raw (cdr (assoc key (gateway-query-parameters env) :test #'string=)))
    (quri:url-decode raw)))

(defun folder-steps (rel)
  "The steps of REL, a path relative to a session's folder separated by
+FOLDER-SEPARATORS+, the empty ones and `.' dropped. Refused when REL is
absolute, climbs, or holds a NUL -- or on Windows a `:', a drive's or a
stream's."
  (when (find (code-char 0) rel)
    (folder-refuse 400 "invalid_path" "a path holds no NUL"))
  ;; Split on `/' alone, `..\..\x' was one step on Windows: it climbed out
  ;; unseen, and a name not there outside answered 404, not outside_folder.
  #+win32 (when (find #\: rel)
            (folder-refuse 400 "invalid_path" "a path holds no colon"))
  (let ((steps (remove-if (lambda (step) (member step '("" ".") :test #'string=))
                          (uiop:split-string rel :separator nlk::+path-separators+))))
    (when (or (and (plusp (length rel)) (find (char rel 0) nlk::+path-separators+))
              (member ".." steps :test #'string=))
      (folder-refuse 403 "outside_folder" "the path ~a leads outside the session's folder" rel))
    steps))

(defun steps-path (steps)
  "The relative path STEPS spell, `/' between them."
  (format nil "~{~a~^/~}" steps))

(defun folder-resolve (folder steps)
  "The native path STEPS name under FOLDER, every link on the way followed,
and what is there, :DIRECTORY, :FILE or :OTHER, with its stat. Refused when
nothing is there and when it lies outside FOLDER."
  (let* ((rel (steps-path steps))
         (real (nlk::real-path (sb-ext:parse-native-namestring
                                (cond (steps (nlk::path-under folder rel))
                                      ((string= folder "") "/")
                                      (t folder))))))
    ;; Nothing there is a 404 only inside: under a link that leads out, the
    ;; way there is refused first, so no answer tells what lies outside.
    (unless real
      (when (rest steps) (folder-resolve folder (butlast steps)))
      (folder-refuse 404 "not_found" "nothing is at ~a" rel))
    (let ((path (nlk::native-path real)))
      (unless (nlk::path-within-p path folder)
        (folder-refuse 403 "outside_folder" "the path ~a leads outside the session's folder" rel))
      (multiple-value-bind (kind stat) (nlk::lstat-kind path)
        (unless (member kind '(:directory :file :other))
          (folder-refuse 404 "not_found" "nothing is at ~a" rel))
        (values path kind stat)))))

;;; --- a directory, listed ------------------------------------------------------------

(defun folder-row (folder directory name &aux (path (nlk::path-under directory name)))
  "NAME in DIRECTORY, a native path under FOLDER, as the listing's row, or NIL
when it went meanwhile. => (NAME KIND STAT LEADS): KIND :DIRECTORY, :FILE or
:OTHER, or :LINK for a link that leads outside FOLDER or nowhere; LEADS is
:INSIDE, :OUTSIDE or :NOWHERE for a link and NIL for anything else."
  (multiple-value-bind (kind stat) (nlk::lstat-kind path)
    (if (not (eq kind :link))
        (and (member kind '(:directory :file :other)) (list name kind stat nil))
        ;; A link to nothing resolves to itself (SBCL's TRUENAME stops at it).
        (let* ((target (nlk::real-path (sb-ext:parse-native-namestring path)))
               (inside (and target (nlk::path-within-p (nlk::native-path target) folder))))
          (multiple-value-bind (kind stat) (if inside (nlk::lstat-kind (nlk::native-path target)) :absent)
            (if (member kind '(:directory :file :other))
                (list name kind stat :inside)
                (list name :link nil (if (and target (not inside)) :outside :nowhere))))))))

(defun folder-listing (folder steps directory)
  "The listing of DIRECTORY, the native path STEPS name under FOLDER:
directories first, then by name, at most *FOLDER-ENTRIES-CAP* rows."
  (unless (handler-case (progn (sb-posix:access directory (logior sb-posix:r-ok sb-posix:x-ok)) t)
            (sb-posix:syscall-error () nil))
    (folder-refuse 403 "unreadable" "the folder ~a cannot be read" (or (car (last steps)) "itself")))
  (let* ((rows (sort (loop for name in (nlk::directory-names directory)
                           for row = (folder-row folder directory name)
                           when row collect row)
                     (lambda (a b)
                       (let ((a-dir (eq :directory (second a))) (b-dir (eq :directory (second b))))
                         (if (eq a-dir b-dir) (string-lessp (first a) (first b)) a-dir)))))
         (more (max 0 (- (length rows) *folder-entries-cap*))))
    (json-object
     "folder" folder
     "path" (steps-path steps)
     "kind" "directory"
     "entries" (map 'vector
                    (lambda (row)
                      (destructuring-bind (name kind stat leads) row
                        (json-object "name" name
                                     "kind" (string-downcase kind)
                                     :when (eq kind :file) "size" (sb-posix:stat-size stat)
                                     :when stat "modified" (nlk:iso-time (sb-posix:stat-mtime stat) :millis nil)
                                     :when (char= #\. (char name 0)) "hidden" t
                                     :opt "link" (and leads (string-downcase leads)))))
                    (subseq rows 0 (- (length rows) more)))
     :when (plusp more) "more" more)))

;;; --- a file, answered as what it is --------------------------------------------------

(defun file-head (path count)
  "The first COUNT bytes of the file at the native PATH, or fewer when it is shorter."
  (with-open-file (in (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (let ((head (make-array count :element-type '(unsigned-byte 8))))
      (subseq head 0 (read-sequence head in)))))

(defun attachment-disposition (name)
  "A Content-Disposition saving the file as NAME: an ASCII name for every
reader, and the name itself as RFC 5987 spells it."
  (format nil "attachment; filename=\"~a\"; filename*=UTF-8''~a"
          (map 'string (lambda (char) (if (and (char< #\Space char #\Rubout) (not (find char "\"\\"))) char #\_))
               name)
          (quri:url-encode name)))

(defun download-answer (body type name &rest headers)
  "BODY, octets or a file, as a 200 of TYPE that saves it as NAME
(ATTACHMENT-DISPOSITION), HEADERS after."
  (list 200 (list* :content-type type :content-disposition (attachment-disposition name) headers) body))

(defun folder-file (steps path stat download)
  "The file at the native PATH, which STEPS name, answered as the Files view
reads it (see the file's head); DOWNLOAD sends it to be saved."
  ;; A file past the text cap is not read to learn whether it is text: its
  ;; head holding no NUL makes it text too large, else it is binary. One this
  ;; process may not read is refused in words before any open; without that,
  ;; the open's own error would print SBCL's pathname syntax and absolute path.
  (handler-case (sb-posix:access path sb-posix:r-ok)
    (sb-posix:syscall-error (condition)
      (folder-refuse 403 "unreadable" "this file can't be read here~:[~; (permission denied)~]"
                     (eql sb-posix:eacces (sb-posix:syscall-errno condition)))))
  (let* ((name (car (last steps)))
         (file (sb-ext:parse-native-namestring path))
         (size (sb-posix:stat-size stat))
         (headers (list :x-content-type-options "nosniff" :cache-control "private, no-cache"))
         (sniffed (image-media-type (file-head path 16)))
         (text (and (not sniffed) (<= size *folder-text-bytes*)
                    (when-let (octets (nlt:file-octets file *folder-text-bytes*))
                      ;; Text is UTF-8 with no NUL (NLK::UTF-8-TEXT signals on anything else).
                      (and (not (find 0 octets)) (ignore-errors (nlk::utf-8-text octets))
                           octets)))))
    (cond (download (apply #'download-answer file "application/octet-stream" name headers))
          ((and sniffed (<= size *folder-image-bytes*))
           (list 200 (list* :content-type sniffed headers) file))
          (text (list 200 (list* :content-type "text/plain; charset=utf-8" :content-length (length text) headers)
                      text))
          (t (json-object "kind" "file"
                          "path" (steps-path steps)
                          "name" name
                          "size" size
                          "modified" (nlk:iso-time (sb-posix:stat-mtime stat) :millis nil)
                          "why" (if (or sniffed (and (> size *folder-text-bytes*)
                                                     (not (find 0 (file-head path 4096)))))
                                    "too_large"
                                    "binary"))))))

;;; --- a file, put there ------------------------------------------------------------------

(defun copy-body (env length sink &aux (body (getf env :raw-body))
                                       (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
  "Read LENGTH bytes of ENV's body, handing each piece to SINK, a function of
the buffer and how much of it was read."
  (loop with left = length
        while (plusp left)
        do (let ((got (read-sequence buffer body :end (min left (length buffer)))))
             (when (zerop got)
               (nlk::protocol-refuse :truncated_request "HTTP request body ended early"))
             (funcall sink buffer got)
             (decf left got))))

(defun drain-upload (env)
  "Read a refused upload's body and let it go."
  ;; An answer sent before its request's body is read leaves that body on a
  ;; connection the browser keeps open, where it is read as the start of the
  ;; next request: a page that retried with replace=1 over the same connection
  ;; had its retry refused as a bad request.
  (ignore-errors (copy-body env (or (getf env :content-length) 0) (constantly nil))))

(defun folder-upload (env folder steps name replace)
  "Write ENV's body as the file NAME in the directory STEPS name under FOLDER.
=> the directory's listing. A name already there is refused unless REPLACE,
and a directory's name always."
  (cond ((or (null name) (zerop (length name)))
         (folder-refuse 400 "invalid_name" "name the file (name=...)"))
        ((find #\/ name) (folder-refuse 400 "invalid_name" "a file's name holds no slash"))
        ;; Windows walks `\' as `/': `..\x' was written in the folder above.
        #+win32 ((find #\\ name) (folder-refuse 400 "invalid_name" "a file's name holds no backslash"))
        #+win32 ((find #\: name) (folder-refuse 400 "invalid_name" "a file's name holds no colon"))
        ((find (code-char 0) name) (folder-refuse 400 "invalid_name" "a file's name holds no NUL"))
        ((member name '("." "..") :test #'string=)
         (folder-refuse 400 "invalid_name" "a file's name is not . or ..")))
  (let ((length (getf env :content-length)))
    (cond ((null length)
           (folder-refuse 411 "length_required" "an upload says its length (Content-Length)"))
          ((> length *folder-upload-bytes*)
           (folder-refuse 413 "request_too_large" "an upload is at most ~d MiB"
                          (floor *folder-upload-bytes* (* 1024 1024)))))
    (multiple-value-bind (directory kind) (folder-resolve folder steps)
      (unless (eq kind :directory)
        (folder-refuse 400 "not_a_folder" "the path ~a is not a folder" (steps-path steps)))
      (let* ((target (nlk::path-under directory name))
             (there (nlk::lstat-kind target)))
        (cond ((eq there :directory)
               (folder-refuse 409 "is_folder" "a folder named ~a is here already" name))
              ((and (not (eq there :absent)) (not replace))
               (folder-refuse 409 "file_exists" "a file named ~a is here already" name)))
        (nlk:write-file-atomically
         target
         (lambda (out) (copy-body env length (lambda (buffer got) (write-sequence buffer out :end got))))
         :parents nil :mode (and (eq there :file) :keep))
        (folder-listing folder steps directory)))))

;;; --- the route ------------------------------------------------------------------------

(define-gateway-route handle-gateway-files-http (env session-id)
  "GET /api/gateway/sessions/<id>/files?path=REL — the session's folder at
REL, the folder itself when absent: a directory's listing {folder, path, kind,
entries: [{name, kind, size, modified, hidden, link}], more}, or the file
(FOLDER-FILE), with &download=1 to save it. POST ...?path=DIR&name=NAME
[&replace=1] writes the body as the file NAME in DIR and answers DIR's
listing. A path or a name that leads outside the folder is a 403
outside_folder."
  (:route nil "files")
  (:on (folder-refused (condition)
         (when (eq (getf env :request-method) :post) (drain-upload env))
         (gateway-error-response (folder-refused-status condition) (folder-refused-code condition)
                                 (folder-refused-detail condition))))
  (let* ((folder (session-folder session-id))
         (steps (folder-steps (or (folder-query env "path") "")))
         (flag (lambda (key) (member (folder-query env key) '("1" "true") :test #'equal))))
    (case (getf env :request-method)
      (:get (multiple-value-bind (path kind stat) (folder-resolve folder steps)
              (case kind
                (:directory (folder-listing folder steps path))
                (:file (folder-file steps path stat (funcall flag "download")))
                (t (folder-refuse 400 "not_a_file" "the path ~a is neither a file nor a folder"
                                  (steps-path steps))))))
      (:post (folder-upload env folder steps (folder-query env "name") (funcall flag "replace")))
      (t (gateway-error-response 405 "method_not_allowed" "files is a GET or POST control operation")))))

;;; --- the folder a new session would open in -----------------------------------------
;;;
;;; Before a session is, the page names the folder its first message opens it
;;; in, and a folder the operator types is checked before it is taken:
;;; resolved as the session route resolves it (NLK:CHECK-SESSION-CWD -- a
;;; leading ~ the home, a relative path under the gateway's own folder) and
;;; said to be there, or why not in the words that route would refuse it with.

(define-gateway-route handle-gateway-folder-http (env)
  "GET /api/gateway/folder?path=P — the folder a new session naming P would
work in, and whether it is there => {path, exists, why}: PATH absolute, WHY the
refusal's words when EXISTS is false, else null. Without P, the gateway's own
folder: where a session that names none opens."
  (:route :get "/api/gateway/folder" "folder check")
  (let ((path (folder-query env "path")))
    (handler-case (make-json-object "path" (nlk:check-session-cwd path) "exists" t "why" :null)
      (nlk:missing-session-folder (condition)
        (make-json-object "path" (nlk:missing-session-folder-path condition) "exists" :false
                          "why" (princ-to-string condition))))))
