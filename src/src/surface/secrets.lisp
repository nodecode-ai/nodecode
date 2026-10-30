;;;; secrets.lisp --- one secret per file: `nodecode secrets' moves the API
;;;; keys auth.json holds into files of their own under secrets/.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; auth.json holds every API key in one JSON object, beside the OAuth logins.
;;;; A sandbox stands a sentinel in for a whole file and gives the real value
;;;; back only on the way out to the host the file is for; it cannot do that
;;;; for one member of a JSON object, which the sentinel would leave unparsed.
;;;; So each key goes to its own 0600 file, providers.<id>.api_key, the name a
;;;; pasted section secret takes (settings.lisp, WRITE-SECRET-FILE), and the
;;;; credential chain reads that file before auth.json (provider.lisp,
;;;; PROVIDER-SECRET-KEY). OAuth logins stay in auth.json: a login refreshes
;;;; itself, which a file a sandbox masks could not take.

(in-package #:nodecode.evolved)

(defun inline-config-key-providers ()
  "The provider ids whose config.jsonc api_key is the key itself, not a
!command: a key no file move reaches."
  (sort (loop for (id . entry) in (ignore-errors (provider-config-entries))
              for value = (trimmed-config-string entry "api_key")
              when (and value (plusp (length value)) (char/= #\! (char value 0)))
                collect id)
        #'string<))

(defun split-auth-secrets (&key (auth-path *auth-file-path*) (directory *secrets-directory*) dry-run)
  "Move every API key auth.json holds at api_keys.<id>.key to its own file
 under DIRECTORY (PROVIDER-SECRET-FILE), then take the moved entries out of
 auth.json, every other member kept. => a plist: :MOVED the ids moved, :KEPT
 (ID REASON) for each left in auth.json, :INLINE the providers whose
 config.jsonc api_key is a literal, :DRY-RUN. DRY-RUN writes nothing and
 answers what would move."
  ;; A file already holding the same key counts as moved, so a split that
  ;; stopped half way finishes on the next run; one holding another key is
  ;; left, both of them, for the operator to choose. The files are written
  ;; before auth.json is: an interruption leaves a key in both places, never
  ;; in neither.
  (let* ((auth (read-auth-file auth-path))
         (api-keys (and auth (json-value auth :object "api_keys")))
         (moved '())
         (kept '()))
    (when api-keys
      (dolist (id (sort (loop for id being the hash-keys of api-keys collect id) #'string<))
        (let ((key (nlk:trimmed (or (auth-api-key auth id) "")))
              (file (provider-secret-file id directory)))
          (cond ((null file)
                 (push (list id "no file can be named for this id") kept))
                ((zerop (length key))
                 (push (list id "its entry holds no key") kept))
                ((secret-file-regular-p file)
                 (if (equal key (provider-secret-key id :directory directory :probe t))
                     (push id moved)
                     (push (list id (format nil "~a holds another key; both left"
                                            (file-namestring file)))
                           kept)))
                ((probe-file file)
                 (push (list id (format nil "~a is not a regular file" (file-namestring file)))
                       kept))
                (t
                 (unless dry-run (write-secret-file file key))
                 (push id moved))))))
    (setf moved (nreverse moved)
          kept (nreverse kept))
    (when (and moved (not dry-run))
      (update-auth-file (lambda (auth)
                          (when-let (keys (json-value auth :object "api_keys"))
                            (dolist (id moved) (remhash id keys))
                            (when (zerop (hash-table-count keys))
                              (remhash "api_keys" auth))))
                        :auth-path auth-path))
    (list :moved moved :kept kept :inline (inline-config-key-providers) :dry-run (and dry-run t))))

(defun secret-file-names (&optional (directory *secrets-directory*))
  "The names of the files under the secrets folder DIRECTORY, sorted; never
their contents."
  (sort (mapcar #'file-namestring
                (ignore-errors (uiop:directory-files (uiop:ensure-directory-pathname directory))))
        #'string<))

(defun split-report-text (report)
  "What `nodecode secrets split' prints for REPORT (SPLIT-AUTH-SECRETS)."
  (destructuring-bind (&key moved kept inline dry-run) report
    (with-output-to-string (out)
      (if moved
          (format out "~:[moved~;would move~] ~d key~:p from auth.json to secrets/: ~{~a~^, ~}~%"
                  dry-run (length moved) moved)
          (format out "auth.json holds no API key to move~%"))
      (dolist (entry kept)
        (format out "kept in auth.json: ~a (~a)~%" (first entry) (second entry)))
      (when inline
        (format out "still inline in config.jsonc, readable inside a sandbox; move by hand: ~{~a~^, ~}~%"
                inline))
      (when dry-run
        (format out "nothing written (--dry-run)~%")))))

(defun secrets-answer (words)
  "The `secrets' command line as (values TEXT EXIT-CODE STREAM): WORDS the
arguments after `secrets'. `split' moves auth.json's API keys to files of
their own, `split --dry-run' says what would move, `list' names the files;
anything else is a refusal, 2 and :ERROR. A key file that cannot be read or
written is 1 and :ERROR."
  (with-handlers ((credential-error (refusal) (values (princ-to-string refusal) 1 :error))
                  (file-error (refusal) (values (princ-to-string refusal) 1 :error)))
    (cond ((equal words '("split"))
           (values (split-report-text (split-auth-secrets)) 0 :output))
          ((equal words '("split" "--dry-run"))
           (values (split-report-text (split-auth-secrets :dry-run t)) 0 :output))
          ((equal words '("list"))
           (values (let ((names (secret-file-names)))
                     (if names
                         (format nil "secrets/ holds ~d file~:p:~%~{  ~a~%~}" (length names) names)
                         (format nil "secrets/ holds no file~%")))
                   0 :output))
          (t
           (values (format nil "secrets: ~:[no verb~;~:*unknown ~{~a~^ ~}~]; split [--dry-run] moves auth.json's API keys to secrets/, list names the files there"
                           words)
                   2 :error)))))
