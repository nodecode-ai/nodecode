;;;; fetch.lisp --- a pinned archive, fetched once into the cache.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three things the channels run are too big to ship and too fixed to
;;;; rebuild: the speech engine that reads a recording (transcribe.lisp), the
;;;; one that speaks an answer (speech.lisp), and libdave, the library
;;;; Discord's end-to-end encrypted voice is not reachable without
;;;; (discord/dave.lisp). Each is an archive at a URL, pinned to a digest,
;;;; opened into a folder under the cache, and marked so the next boot knows
;;;; it is whole.
;;;;
;;;; One primitive, INSTALL-PINNED-ARCHIVE, with the two things that differ
;;;; passed in: where it goes and how it opens. A digest that does not match
;;;; refuses and keeps nothing — the marker is written last, so an
;;;; interrupted unpack leaves a folder and never a claim.

(in-package #:nodecode-channel-kit)

(defun installed-marker (archive directory)
  "The file INSTALL-PINNED-ARCHIVE writes once ARCHIVE is whole under
DIRECTORY. Its content is the digest that was verified."
  (merge-pathnames (format nil "~a.installed" archive) directory))

(defun archive-installed-p (archive directory)
  (and (probe-file (installed-marker archive directory)) t))

(defun machine-build (builds)
  "The row of BUILDS, ((PLATFORM ARCHITECTURE) ARCHIVE SHA256 ...) lists, for
this machine, past its key, as values: (values ARCHIVE SHA256 ...), or NIL."
  (values-list (rest (assoc (list (nlk:platform) (nlk:architecture)) builds :test #'equal))))

(defun download-file (url pathname)
  "URL's body written to PATHNAME, streamed a megabyte at a time."
  (let ((body (dex:get url :want-stream t :force-binary t :max-redirects 5
                           :connect-timeout 20 :read-timeout 120
                           :use-connection-pool nil)))
    (nlk:with-cleanup ((ignore-errors (close body)))
      (with-open-file (out pathname :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
        (uiop:copy-stream-to-stream body out :element-type '(unsigned-byte 8)
                                             :buffer-size (* 1024 1024))))))

(defun unpack-command (kind file directory &aux (from (uiop:native-namestring file))
                                                (into (uiop:native-namestring directory)))
  "The command that opens FILE into DIRECTORY. KIND is how it is packed."
  (ecase kind
    (:tar-bz2 (list "tar" "-xjf" from "-C" into))
    (:tar-gz (list "tar" "-xzf" from "-C" into))
    (:zip (list "unzip" "-q" "-o" from "-d" into))))

(defparameter +archive-suffixes+
  '((:tar-bz2 . "tar.bz2") (:tar-gz . "tar.gz") (:zip . "zip")))

(defun install-pinned-archive (archive sha256 url-format directory
                               &key (kind :tar-bz2) into)
  "ARCHIVE fetched from URL-FORMAT, checked against SHA256 and opened into
DIRECTORY, unless its marker already says it is there."
  ;;
  ;; INTO names a folder to make and unpack into. Absent — the usual case — the
  ;; archive carries its own top-level folder and is opened where it lands. A
  ;; zip that carries bare lib/ and include/ folders has none, and would strew
  ;; them across the cache: that one passes its own name.
  ;; => T when this call installed it, NIL when it already was.
  (unless (archive-installed-p archive directory)
    (let* ((suffix (or (cdr (assoc kind +archive-suffixes+))
                       (error "no suffix for archive kind ~s" kind)))
           (file (merge-pathnames (format nil "~a.~a" archive suffix) directory))
           (target (if into
                       (merge-pathnames (format nil "~a/" into) directory)
                       directory)))
      (ensure-directories-exist target)
      (nlk:with-cleanup ((ignore-errors (delete-file file)))
        (download-file (format nil url-format archive) file)
        (let ((digest (ironclad:byte-array-to-hex-string
                       (ironclad:digest-file :sha256 file))))
          (unless (string-equal digest sha256)
            (error "~a arrived with sha256 ~a, not the pinned ~a: refused"
                   archive digest sha256)))
        (nlk:bind (((_ err status)
                    (nlk:run-bounded (unpack-command kind file target) :seconds 600)))
          (unless (eql status 0)
            (error "unpacking ~a failed: ~a" archive (nlk:one-line err))))
        (nlk:with-output-file (out (installed-marker archive directory))
          (format out "~a~%" sha256))))
    t))

(defun call-with-scratch-directory (function)
  "FUNCTION called with a fresh directory, removed however it returns."
  ;; Audio work writes files a child process reads; none of them outlive the
  ;; call.
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "nodecode-speech-~36r"
                                             (random (expt 36 8) (make-random-state t)))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    (unwind-protect (funcall function directory)
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun write-octets (octets pathname)
  (alexandria:write-byte-vector-into-file octets pathname :if-exists :supersede)
  pathname)

(defun read-octets (pathname)
  (alexandria:read-file-into-byte-vector pathname))

(defun ffmpeg-present-p ()
  (ignore-errors (eql 0 (nth-value 2 (nlk:run-bounded '("ffmpeg" "-version")
                                                      :seconds 10)))))
