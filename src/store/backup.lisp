;;;; backup.lisp --- this home kept whole: a backup made now, the backups
;;;; kept, one brought back as a profile beside it. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A backup is the whole archive NLK:EXPORT-HOME writes -- the rows a clone
;;;; carries, the keys and secrets among them, and the store as SQLite copies
;;;; it while the gateway goes on writing -- kept in this home's backups/,
;;;; 0700, each archive 0600 beside a small JSON file saying what it holds.
;;;; `nodecode profile export NAME PATH --whole' writes the same archive
;;;; anywhere, so bringing one back is PROFILE-IMPORT: a profile of its own
;;;; beside this home, never this home overwritten. It keeps its own copies of
;;;; what it holds -- the memory as it was, not a link to the memory as it is
;;;; now, which is the point of going back to it.
;;;;
;;;; What this file adds over the export is what a page needs of a file that
;;;; takes a minute to make: a name, a place, a note, and a job read as it goes
;;;; (*BACKUP-JOB*); one runs at a time, and an archive of a gigabyte is sent
;;;; from its file, never read into the image.

(in-package #:nodecode.evolved)

(defun backups-directory ()
  "Where this home keeps its backups."
  (nlk:home "backups/"))

(defun backup-name-char-p (char)
  "Whether CHAR may stand in a backup's name: an ASCII letter or digit, a
dot, a dash, an underscore."
  (and (char< char #\Rubout) (or (alphanumericp char) (find char "._-"))))

(defun backup-note-file (name)
  "The JSON file beside the backup NAME that says what it holds."
  (merge-pathnames (format nil "~a.json" (subseq name 0 (search ".tar.gz" name :from-end t)))
                   (backups-directory)))

(defun make-backup (&key (step (constantly nil)))
  "This home as one archive under BACKUPS-DIRECTORY: EXPORT-HOME's whole
archive of it, with the store this process serves, and a note beside it.
STEP is called with what it is doing as it goes. => the archive's name."
  ;; The name carries now, local time: nodecode-PROFILE-2026-09-26-153012.tar.gz.
  (bind ((directory (backups-directory))
         ((s m h d mo y) (get-decoded-time))
         (name (format nil "nodecode-~a-~4,'0d-~2,'0d-~2,'0d-~2,'0d~2,'0d~2,'0d.tar.gz"
                       (substitute-if-not #\- #'backup-name-char-p (nlk:profile-label)) y mo d h m s))
         (archive (merge-pathnames name directory)))
    (when (probe-file archive)
      (nlk::protocol-refuse :backup_exists "a backup named ~a was made this second" name))
    ;; The folder first, closed to everyone else: nothing under it is ever
    ;; readable by another user, the keys in a half-made archive included.
    (ensure-directories-exist directory)
    #-win32 (sb-posix:chmod (uiop:native-namestring directory) #o700)
    (multiple-value-bind (path holds) (nlk:export-home (nlk:home) archive :whole t :store (store-file) :step step)
      (write-file-atomically (backup-note-file name)
                             (encode-json-object
                              (json-object "created_at" (nlk:iso-now)
                                           "profile" (nlk:profile-label)
                                           "version" (effective-version)
                                           :opt "sessions" (and (member "history.db" holds :test #'string=)
                                                                (nlk:store-open-p) (session-count))
                                           "holds" (coerce holds 'vector)))
                             :mode #o600)
      (file-namestring path))))

;;; --- the backups kept ------------------------------------------------------------

(defun backup-file (name)
  "The archive NAME under BACKUPS-DIRECTORY, or a refusal: a name no backup
is made under, or one that is not there."
  ;; The name is all a request says, so it can never step out of the folder.
  (unless (and (stringp name) (uiop:string-suffix-p name ".tar.gz") (char/= #\. (char name 0))
               (every #'backup-name-char-p name))
    (nlk::protocol-refuse :invalid_request "~s names no backup" name))
  (or (probe-file (merge-pathnames name (backups-directory)))
      (nlk::protocol-refuse :unknown_backup "no backup named ~a" name)))

(defun backups (&aux (directory (backups-directory)))
  "Every backup this home keeps, newest first, each a JSON object: what its
note says it holds, with its name and size."
  (loop for path in (sort (remove-if-not (lambda (path) (uiop:string-suffix-p (file-namestring path) ".tar.gz"))
                                         (and (uiop:directory-exists-p directory)
                                              (uiop:directory-files directory)))
                          #'> :key #'file-write-date)
        for name = (file-namestring path)
        for note = (ignore-errors (decode-json (read-text (backup-note-file name))))
        collect (copy-json-object (and (hash-table-p note) note)
                                  "name" name "bytes" (nlk:file-bytes path))))

(defun delete-backup (name &aux (path (backup-file name)))
  "Remove the backup NAME and its note."
  (delete-file path)
  (ignore-errors (delete-file (backup-note-file name))))

;;; --- the job: one at a time, read as it goes --------------------------------------

(defvar *backup-job* nil
  "The backup or restore this process runs, or ran last, as a plist: :VERB,
:STATE (:running :done :failed), :STEP, :NAME, :PROFILE, :TEXT when done,
:ERROR when failed, :STARTED and :ENDED. Each change is a fresh plist laid
over the last, so a reader never sees one half-written.")

(defvar *backup-job-lock* (bt2:make-lock :name "backup-job")
  "Held while a job is looked at and started, so two never start at once.")

(defun note-backup-job (&rest pairs)
  "Lay PAIRS over *BACKUP-JOB*."
  (setf *backup-job* (append pairs *backup-job*)))

(defun start-backup-job (verb work &rest pairs)
  "Run WORK, a function of the STEP function that answers what it did, on a
thread of its own as the job VERB; PAIRS say what it is over. Refused
while one runs."
  (bt2:with-lock-held (*backup-job-lock*)
    (when (eq :running (getf *backup-job* :state))
      (nlk::protocol-refuse :job_running "a ~(~a~) is running; one at a time" (getf *backup-job* :verb)))
    (setf *backup-job* (list* :verb verb :state :running :step "starting" :started (nlk:unix-now) pairs))
    (spawn (format nil "backup-~(~a~)" verb)
      (handler-case (let ((text (funcall work (lambda (step) (note-backup-job :step step)))))
                      (note-backup-job :state :done :text text :ended (nlk:unix-now)))
        (error (condition)
          (note-backup-job :state :failed :error (princ-to-string condition) :ended (nlk:unix-now)))))))

(defun backups-json (&aux (job *backup-job*))
  "The backups kept and the job, as the routes answer them."
  (json-object "folder" (namestring (backups-directory))
               "backups" (coerce (backups) 'vector)
               :opt "job" (and job
                               (json-object "verb" (string-downcase (getf job :verb))
                                            "state" (string-downcase (getf job :state))
                                            (:getf job "step")
                                            (:getf job :opt "name" "profile" "text" "error")
                                            "seconds" (- (or (getf job :ended) (nlk:unix-now)) (getf job :started))))))

;;; --- /backup ------------------------------------------------------------------------

(defun headless-backup (args session-id)
  "/backup: a backup of this home now, said as where it went."
  (declare (ignore session-id))
  (or (slash-no-args-guard "backup" args)
      (let ((path (backup-file (make-backup))))
        (format nil "backed up to ~a (~a); it holds this home's keys, so keep it where you keep passwords"
                (namestring path) (size-text (nlk:file-bytes path))))))

(defun slash-backup-op (model args)
  "/backup on a shell: HEADLESS-BACKUP, made off the loop."
  (notice-off-loop model "backup: copying this home…" (lambda () (headless-backup args nil))))

;;; --- the routes ------------------------------------------------------------------

(define-gateway-route handle-gateway-backups-http (env)
  "GET /api/gateway/backups — the backups this home keeps and the job
running or last run (BACKUPS-JSON). POST starts a backup and answers at once,
the job running; DELETE ?name=NAME removes one. Each answers the listing."
  (:route nil "/api/gateway/backups")
  (case (getf env :request-method)
    (:post (start-backup-job :backup (lambda (step &aux (name (make-backup :step step)))
                                       (format nil "~a · ~a" name (size-text (nlk:file-bytes (backup-file name)))))))
    (:delete (delete-backup (folder-query env "name"))))
  (backups-json))

(define-gateway-route handle-gateway-backup-file-http (env)
  "GET /api/gateway/backups/file?name=NAME — the archive, sent to be saved."
  ;; A path as the body is sent by the server from the file, a piece at a
  ;; time: an archive of a gigabyte is never read into the image.
  (:route :get "/api/gateway/backups/file" "backup download")
  (let* ((name (folder-query env "name"))
         (path (backup-file name)))
    (download-answer path "application/gzip" name)))

(define-gateway-route handle-gateway-backup-restore-http (env)
  "POST /api/gateway/backups/restore {name, profile} — the backup NAME
brought back as the profile PROFILE (NLK:PROFILE-IMPORT): a home of its own
beside this one, which nothing here changes. Answers at once, the job
running; `nodecode -p PROFILE' opens it once it is done."
  (:route :post "/api/gateway/backups/restore" "backup restore")
  (with-json ((name :string "name") (profile :string "profile")) (gateway-request-json env)
    (require-non-empty (list name profile) "name and profile must be non-empty strings" :required t)
    (let ((path (backup-file name)))
      ;; A name malformed or taken is refused now, not once the archive is unpacked.
      (when (handler-case (nlk:find-profile profile)
              (nlk:profile-error (condition) (nlk::protocol-refuse :invalid_profile "~a" condition)))
        (nlk::protocol-refuse :profile_exists "a profile named ~a already exists" profile))
      (start-backup-job :restore (lambda (step)
                                   (funcall step "unpacking")
                                   (nlk:profile-import path :name profile)
                                   (format nil "`nodecode -p ~a' opens it" profile))
                        :name name :profile profile))
    (backups-json)))
