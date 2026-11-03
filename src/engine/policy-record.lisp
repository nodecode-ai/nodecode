;;;; engine/policy-record.lisp --- the record of what the review adopted and dropped.
;;;;
;;;; The policy store (engine/policy) binds what its `adopted' ref holds. This
;;;; file keeps two things about that ref that the agent, owning its own
;;;; process, could otherwise quietly undo:
;;;;
;;;;   - a dropped change stays dropped. POLICY-RECORD-DROP writes an empty
;;;;     commit on `adopted' whose trailers name the change -- its patch id
;;;;     (git patch-id --stable, which ignores where in the file a hunk lands)
;;;;     and the blob each file would have held -- and POLICY-DROPPED-P finds it
;;;;     again, so the review refuses a resubmission on any later base, and one
;;;;     that reaches the same text by other steps.
;;;;   - the history leaves the box. POLICY-RECORD-PUSH pushes `adopted' to
;;;;     refs/heads/policy/adopted at the remote config policy.record names, a
;;;;     plain push and never a forced one. With that branch protected on the
;;;;     forge (no force-push, admins included) a rewritten history cannot land
;;;;     there: the push is refused as diverged and a standing notice says so.
;;;;
;;;; Plumbing only (commit-tree, update-ref, a scratch index): nothing here
;;;; touches the store's working tree, which holds the agent's drafts.

(in-package #:nodecode.evolved)

(defparameter *policy-record-branch* "refs/heads/policy/adopted"
  "Where POLICY-RECORD-PUSH puts `adopted' on the record's remote.")

(defparameter *policy-record-notice-key* "policy-record"
  "The standing notice a record that cannot be pushed raises.")

(defun policy-record-store (&optional store)
  "STORE as a directory pathname, the home's policy store by default."
  (uiop:ensure-directory-pathname (or store (policy-directory))))

(defun policy-record-git (store args &key input environment (seconds 30))
  "Run git ARGS in STORE: (values STDOUT STDERR STATUS)."
  (run-bounded (list* "git" "-C" (uiop:native-namestring (policy-record-store store)) args)
               :seconds seconds :input input :environment environment))

(defun policy-record-git-line (store args &key input environment)
  "Git ARGS' trimmed stdout in STORE; signals with git's own words on a failure."
  (multiple-value-bind (out err status) (policy-record-git store args :input input :environment environment)
    (unless (eql status 0)
      (error "git ~{~a~^ ~}: ~a" args (string-trim '(#\Space #\Newline) (or err (princ-to-string status)))))
    (string-trim '(#\Space #\Newline #\Tab) (or out ""))))

(defun policy-adopted-commit (store)
  "The commit `adopted' names in STORE."
  (policy-record-git-line store (list "rev-parse" "--verify" (format nil "~a^{commit}" +policy-adopted-ref+))))

(defun policy-change (store files)
  "What FILES -- ((NAME . TEXT-OR-NIL) ...), as POLICY-COMMIT takes them --
would make of STORE's adopted tree: (values PATCH-ID BLOBS TREE). PATCH-ID is
NIL for a change that changes nothing; BLOBS is ((NAME . BLOB-OR-\"-\") ...)
sorted by name, \"-\" for a file the change removes."
  (let* ((base (policy-adopted-commit store))
         (index (uiop:tmpize-pathname (uiop:merge-pathnames* "policy-index" (uiop:temporary-directory))))
         (env (list (format nil "GIT_INDEX_FILE=~a" (uiop:native-namestring index))))
         (blobs '()))
    (nlk:with-cleanup ((ignore-errors (delete-file index)))
      (policy-record-git-line store (list "read-tree" base) :environment env)
      (dolist (file files)
        (destructuring-bind (name . text) file
          (if text
              (let ((blob (policy-record-git-line store (list "hash-object" "-w" "--stdin") :input text)))
                (policy-record-git-line store (list "update-index" "--add" "--cacheinfo"
                                             (format nil "100644,~a,~a" blob name))
                                 :environment env)
                (push (cons name blob) blobs))
              (progn
                (policy-record-git-line store (list "update-index" "--force-remove" "--" name) :environment env)
                (push (cons name "-") blobs)))))
      (let* ((tree (policy-record-git-line store (list "write-tree") :environment env))
             (diff (policy-record-git-line store (list "diff-tree" "-p" "--no-color" "--full-index"
                                                (format nil "~a^{tree}" base) tree)))
             (id (and (plusp (length diff))
                      (first (uiop:split-string
                              (policy-record-git-line store (list "patch-id" "--stable")
                                               :input (format nil "~a~%" diff))
                              :separator " ")))))
        (values (and (plusp (length id)) id)
                (sort blobs #'string< :key #'car)
                tree)))))

(defun policy-trailer-values (message key)
  "The values of MESSAGE's trailer lines KEY: VALUE, in order."
  (let ((prefix (format nil "~a: " key)))
    (loop for line in (uiop:split-string message :separator '(#\Newline))
          when (uiop:string-prefix-p prefix line)
            collect (string-trim " " (subseq line (length prefix))))))

(defun policy-record-drop (store files &key reason id)
  "Record on STORE's adopted ref that the change FILES was dropped, and answer
the record's commit. An empty commit -- the adopted tree is unchanged, so what
binds is unchanged -- whose trailers POLICY-DROPPED-P matches; then the record
is pushed (POLICY-RECORD-PUSH)."
  (multiple-value-bind (patch blobs) (policy-change store files)
    (let ((message (with-output-to-string (out)
                     (format out "policy: drop~@[ ~a~]~%~%~@[~a~%~%~]" id reason)
                     (format out "Policy-Dropped-Patch: ~a~%" (or patch "-"))
                     (loop for (name . blob) in blobs
                           do (format out "Policy-Dropped-File: ~a ~a~%" name blob))
                     (when id (format out "Policy-Dropped-Id: ~a~%" id)))))
      ;; Compare-and-swap on the ref, as POLICY-COMMIT does: an adopt that lands
      ;; between the read and the write moves the base, and the drop goes on
      ;; top of it instead of erasing it.
      (loop repeat 5
            do (let* ((base (policy-adopted-commit store))
                      (commit (policy-record-git-line
                               store (append (nlk::git-identity (policy-record-store store))
                                             (list "commit-tree" (format nil "~a^{tree}" base) "-p" base "-F" "-"))
                               :input message)))
                 (multiple-value-bind (out err status)
                     (policy-record-git store (list "update-ref" "-m" "policy: drop" +policy-adopted-ref+ commit base))
                   (declare (ignore out err))
                   (when (eql status 0)
                     (ignore-errors (policy-record-push store))
                     (return-from policy-record-drop commit)))))
      (error "policy-record-drop: ~a kept moving under the drop" +policy-adopted-ref+))))

(defun policy-dropped-records (store)
  "STORE's drop records, newest first: (COMMIT PATCH BLOBS) each."
  (let ((log (policy-record-git-line store (list "log" "--format=%H%x1f%B%x1e"
                                          "--grep=^Policy-Dropped-Patch: " +policy-adopted-ref+))))
    (loop for record in (uiop:split-string log :separator (string (code-char #x1e)))
          for (commit message) = (uiop:split-string (string-left-trim '(#\Newline) record)
                                                    :separator (string (code-char #x1f)))
          when (and commit (plusp (length commit)) message)
            collect (list commit
                          (first (policy-trailer-values message "Policy-Dropped-Patch"))
                          (sort (loop for value in (policy-trailer-values message "Policy-Dropped-File")
                                      for space = (position #\Space value :from-end t)
                                      when space collect (cons (subseq value 0 space) (subseq value (1+ space))))
                                #'string< :key #'car)))))

(defun policy-dropped-p (store files)
  "The drop record that refuses the change FILES, or NIL: one with the same
patch id (the same edit, on any base), or the same text in every file it
names (the same result, reached by other steps)."
  (multiple-value-bind (patch blobs) (policy-change store files)
    (loop for (commit dropped-patch dropped-blobs) in (policy-dropped-records store)
          when (or (and patch (equal patch dropped-patch))
                   (and blobs (equal blobs dropped-blobs)))
            return commit)))

(defun policy-record-remote ()
  "The record's remote, config policy.record (a URL or a remote name), or NIL."
  (let ((value (policy-config-current '("policy" "record"))))
    (and (stringp value) (plusp (length (string-trim " " value))) (string-trim " " value))))

(defun policy-record-say (text)
  "Stand TEXT as the record's notice, or take it down when TEXT is NIL."
  (unless (equal text (second (assoc *policy-record-notice-key* (nlk:standing-notices) :test #'string=)))
    (ignore-errors (notice text :level :warning :key *policy-record-notice-key*))))

(defun policy-record-push (&optional store &key (remote (policy-record-remote)))
  "Push STORE's adopted ref to *POLICY-RECORD-BRANCH* at REMOTE, never forced:
:PUSHED, :NO-REMOTE when none is set, :DIVERGED when the remote's branch is
not an ancestor -- the history here was rewritten, or another writer reached
the branch -- or :FAILED. A refused push stands a notice until one lands."
  (if (null remote)
      :no-remote
      (multiple-value-bind (out err status)
          (policy-record-git store (list "push" "--porcelain" remote
                                  (format nil "~a:~a" +policy-adopted-ref+ *policy-record-branch*))
                      :seconds 60)
        (let ((said (format nil "~a~%~a" (or out "") (or err ""))))
          (cond ((eql status 0)
                 (policy-record-say nil)
                 :pushed)
                ((or (search "[rejected]" said) (search "non-fast-forward" said) (search "fetch first" said))
                 (policy-record-say
                  (format nil "policy record: ~a refused the push of ~a, which no longer ~
                               extends what it holds. The adopted history here was rewritten ~
                               or another writer reached the branch; nothing is forced."
                          remote *policy-record-branch*))
                 :diverged)
                (t
                 (policy-record-say
                  (format nil "policy record: the push to ~a failed: ~a" remote
                          (string-trim '(#\Space #\Newline) (or err (princ-to-string status)))))
                 :failed))))))
