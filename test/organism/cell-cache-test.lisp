;;;; cell-cache-test.lisp --- a peripheral's fasls answer to what its files say.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ASDF judges a fasl current when it is dated after its source. A checkout,
;;;; a copy that keeps dates, an archive unpacked: each can put new text under
;;;; an older date than the fasl a previous text compiled to, and a boot then
;;;; loads the previous text's code while every file on disk says otherwise.
;;;; Each case below is that boot, made in this process: the folder's system
;;;; is cleared between loads, as a fresh process never had it.

(in-package #:nodecode.test)

(defun backdate-file (path seconds)
  "Set PATH's write date SECONDS before now, as a copy that keeps an old date
leaves it."
  (let ((unix (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0) seconds)))
    (sb-posix:utimes (namestring path) unix unix)))

(defun rewrite-peripheral (root name forms &rest options)
  "Write peripheral NAME under ROOT anew with FORMS, its every file dated a day
back, and forget its system as a new process would not know it."
  (let ((directory (apply #'write-peripheral-folder root name :forms forms options)))
    (dolist (file (directory (merge-pathnames "*.*" directory)))
      (backdate-file file 86400))
    (asdf:clear-system name)
    directory))

(defun reload-fixture (root &rest names)
  "Forget NAMES' systems, then load ROOT as a new boot would: => the records."
  (dolist (name names) (asdf:clear-system name))
  (load-cell-fixture root))

(defun record-named (name records)
  (find name records :key #'nlk:cell-name :test #'string=))

(defun package-call (package name)
  (funcall (uiop:find-symbol* name package)))

(defmacro with-peripheral-cache ((cache &optional (stamp "stamp-content")) &body body)
  "BODY with peripherals compiling under a scratch CACHE for STAMP."
  `(with-temp-directory (,cache "peripheral-content-cache")
     (nlk:with-cleanup ((asdf:initialize-output-translations))
       (let ((nlk::*peripheral-cache-directory* ,cache)
             (nlk::*image-stamp* ,stamp))
         ,@body))))

(defun peripheral-fasls (cache name)
  "Every fasl under CACHE compiled for the folder NAME."
  (directory (merge-pathnames (format nil "**/~a/**/*.fasl" name) cache)))

(deftest cell-cache-new-text-under-an-older-date-is-compiled (with-cell-fixture (root))
  ;; Given a peripheral loaded once, its fasl compiled; when its file says
  ;; something else under a date older than that fasl; then the next boot
  ;; runs what the file says.
  (with-peripheral-cache (cache)
    (write-peripheral-folder root "nl-cc-probe" :exports '("probe") :forms '("(defun probe () :old)"))
    (load-cell-fixture root)
    (is (eq :old (package-call "NL-CC-PROBE" "PROBE")) "the first text runs")
    (rewrite-peripheral root "nl-cc-probe" '("(defun probe () :new)") :exports '("probe"))
    (reload-fixture root "nl-cc-probe")
    (is (eq :new (package-call "NL-CC-PROBE" "PROBE")) "the boot runs the new text, not the fasl")))

(deftest cell-cache-unchanged-text-reuses-its-fasl (with-cell-fixture (root))
  ;; Given a peripheral loaded once; when nothing in its folder changes; then
  ;; the next boot loads the same fasl and compiles nothing.
  (with-peripheral-cache (cache)
    (write-peripheral-folder root "nl-cc-same" :exports '("probe") :forms '("(defun probe () :same)"))
    (load-cell-fixture root)
    (let* ((fasls (peripheral-fasls cache "nl-cc-same"))
           (dates (mapcar #'file-write-date fasls)))
      (is (= 1 (length fasls)) "one fasl is compiled")
      (sleep 1.1)
      (reload-fixture root "nl-cc-same")
      (let ((found (mapcar #'namestring (peripheral-fasls cache "nl-cc-same"))))
        (is (equal (mapcar #'namestring fasls) found) "the boot finds the same fasl"))
      (is (equal dates (mapcar #'file-write-date fasls)) "and compiles nothing")
      (is (eq :same (package-call "NL-CC-SAME" "PROBE"))))))

(deftest cell-cache-a-hidden-file-changes-nothing (with-cell-fixture (root))
  ;; Given a loaded peripheral; when only a file under a hidden directory
  ;; changes -- a checkout's .git -- then its fasl stays; when a file the
  ;; folder shows changes, it compiles anew.
  (with-peripheral-cache (cache)
    (let ((directory (write-peripheral-folder root "nl-cc-hidden" :forms '("(defun probe () 1)"))))
      (write-temp-file (merge-pathnames ".git/HEAD" directory) "ref: one")
      (load-cell-fixture root)
      (let ((before (mapcar #'namestring (peripheral-fasls cache "nl-cc-hidden"))))
        (write-temp-file (merge-pathnames ".git/HEAD" directory) "ref: two")
        (reload-fixture root "nl-cc-hidden")
        (let ((after (mapcar #'namestring (peripheral-fasls cache "nl-cc-hidden"))))
          (is (equal before after) "a hidden file is not the folder's text"))
        (write-temp-file (merge-pathnames "notes.sexp" directory) "(:read-at-compile t)")
        (reload-fixture root "nl-cc-hidden")
        ;; a file it shows is, and the folder compiles beside its first fasl
        (is (= 2 (length (peripheral-fasls cache "nl-cc-hidden"))))))))

(deftest cell-cache-a-dependency-back-at-old-text-recompiles-its-dependent (with-cell-fixture (root))
  ;; Given TOP expanding BASE's macro; when BASE changes and then goes back to
  ;; its first text, every file dated old as a checkout dates it; then TOP
  ;; runs the expansion of the text BASE says now, at every step.
  (with-peripheral-cache (cache)
    (flet ((base (value)
             (rewrite-peripheral root "nl-cc-base" (list (format nil "(defmacro value () ~s)" value))
                                 :exports '("value")))
           (top-value () (package-call "NL-CC-TOP" "TOP-VALUE")))
      (base :one)
      (rewrite-peripheral root "nl-cc-top" '("(defun top-value () (nl-cc-base:value))")
                          :exports '("top-value") :depends-on '("nl-cc-base"))
      (reload-fixture root "nl-cc-base" "nl-cc-top")
      (is (eq :one (top-value)) "the first text")
      (base :two)
      (reload-fixture root "nl-cc-base" "nl-cc-top")
      (is (eq :two (top-value)) "the changed dependency")
      (base :one)
      (reload-fixture root "nl-cc-base" "nl-cc-top")
      (is (eq :one (top-value)) "the dependency back at its first text"))))

(deftest cell-cache-new-text-that-fails-to-compile-fails-the-cell (with-cell-fixture (root))
  ;; Given a peripheral loaded from a good text; when its file holds a text
  ;; that cannot be read, dated older than the good fasl; then the boot
  ;; reports the cell failed rather than load the good text's fasl.
  (with-peripheral-cache (cache)
    (write-peripheral-folder root "nl-cc-broken" :forms '("(defun probe () :good)"))
    (load-cell-fixture root)
    (rewrite-peripheral root "nl-cc-broken" '("(defun probe () :broken"))
    (is-present (record (record-named "nl-cc-broken" (reload-fixture root "nl-cc-broken"))) "the folder has a record"
      (is (eq :failed (nlk:cell-state record)) "the cell failed")
      (is (nlk:cell-failures record) "and says why"))))

(deftest cell-cache-a-record-names-the-text-it-loaded (with-cell-fixture (root))
  ;; Given a loaded peripheral; then its record names the folder's content
  ;; key, which is what the folder's files key to on disk; when a file
  ;; changes after the boot, the disk's key differs from the record's: this
  ;; image no longer runs what the disk says.
  (with-peripheral-cache (cache)
    (let ((directory (write-peripheral-folder root "nl-cc-named" :forms '("(defun probe () 1)"))))
      (is-present (record (record-named "nl-cc-named" (load-cell-fixture root))) "the folder has a record"
        (let ((key (nlk::cell-content-key record)))
          (is (and (stringp key) (plusp (length key))) "the record names a key")
          (is (equal key (nlk::folder-content-key directory)) "the key of the files on disk")
          (write-peripheral-folder root "nl-cc-named" :forms '("(defun probe () 2)"))
          (is (not (equal key (nlk::folder-content-key directory))) "an edit after the boot shows"))))))

(deftest cell-cache-keeps-the-keys-in-use-and-the-newest-beside (with-cell-fixture (root))
  ;; Given a peripheral compiled under four texts, every key's last load an
  ;; hour and more back; when it boots a fifth; then its directory keeps the
  ;; fifth and the newest before it, and nothing else.
  (with-peripheral-cache (cache)
    (let ((folder (merge-pathnames "stamp-content/nl-cc-pruned/" cache)))
      (dotimes (n 5)
        (rewrite-peripheral root "nl-cc-pruned" (list (format nil "(defun probe () ~d)" n)))
        (reload-fixture root "nl-cc-pruned")
        ;; age what this boot touched, so each key's last load stands n hours apart
        (dolist (directory (uiop:subdirectories folder))
          (when (< (- (get-universal-time) (file-write-date directory)) 60)
            (dolist (file (uiop:directory-files directory))
              (backdate-file file (* 3600 (- 6 n))))
            (backdate-file directory (* 3600 (- 6 n))))))
      (is (= 2 (length (uiop:subdirectories folder))) "the newest two keys stay")
      (is (= 4 (package-call "NL-CC-PRUNED" "PROBE")) "and the last text runs"))))
