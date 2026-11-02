;;;; backup-test.lisp --- this home kept whole, and brought back as a profile.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The home is the posture's scratch one and the store the fixture's, so a
;;;; backup here reads nothing of the operator's. The job runs on a thread of
;;;; its own: *BACKUP-JOB* is SETF, never LET.

(in-package #:nodecode.test)

(defun backup-test-seed-home (&aux (home nlk:*home*))
  "A home holding what a backup must carry and what it must leave: settings,
keys, secrets by a link to another folder, a knowledge cell by a link and a
shipped cell by a link, and the state that is this machine's alone. => the
shipped cell link's target."
  (dolist (file '("config.jsonc" "auth.json" "SOUL.md"
                  "operator-token" "link/state.json" "composer-history"))
    (write-temp-file (merge-pathnames file home) "x"))
  ;; Every target sits in a folder no row claims, so the backup carries them
  ;; only through the links.
  (let ((shared (merge-pathnames "elsewhere/secrets/" home))
        (knowledge (merge-pathnames "elsewhere/tester-memory/" home))
        (cell (merge-pathnames "elsewhere/cell" home)))
    (write-temp-file (merge-pathnames "bot.token" shared) "x")
    (nlk:make-symlink shared (merge-pathnames "secrets" home))
    (write-temp-file (merge-pathnames "tester-memory.asd" knowledge)
                     "(defsystem \"tester-memory\" :class nodecode.kernel:layer :properties ((:knowledge . t)) :components ())")
    (write-temp-file (merge-pathnames "kept.lisp" knowledge) "remembered")
    (ensure-directories-exist (merge-pathnames "cells/" home))
    (nlk:make-symlink knowledge (merge-pathnames "cells/tester-memory" home))
    (nlk:make-symlink cell (merge-pathnames "cells/shipped" home))
    (namestring cell)))

(defun backup-test-mode (path)
  (logand #o777 (sb-posix:stat-mode (sb-posix:stat (string-right-trim "/" (uiop:native-namestring path))))))

(defun backup-test-sessions (store)
  "How many sessions the store file STORE holds, read as the doctor reads."
  (nle::call-with-store-reader store (lambda (db)
                                       (sqlite:execute-single db "select count(*) from events where kind = ?"
                                                              nlk::+kind-session-created+))))

(deftest backup-carries-the-home-whole-and-comes-back-as-a-profile (with-temp-gateway (port))
  (seed-user-inputs "s-kept" '("remember this"))
  (seed-user-inputs "s-also" '("and this"))
  (let* ((cell (backup-test-seed-home))
         (steps '())
         (name (nle::make-backup :step (lambda (step) (push step steps))))
         (archive (merge-pathnames name (nle::backups-directory))))
    (is (equal '("copying the store" "packing" "compressing") (reverse steps)) "it says how it goes")
    (is (uiop:string-prefix-p (format nil "nodecode-~a-" (nlk:profile-label)) name) "named by whose home it is")
    (is (= #o600 (backup-test-mode archive)) "the archive is the operator's alone")
    (is (= #o700 (backup-test-mode (nle::backups-directory))) "and so is its folder")
    (is-carrying (listed (nlk::run-checked (list "tar" "-tzf" (uiop:native-namestring archive))
                                           (lambda (err) (error "tar: ~a" err))))
      "/history.db" "/config.jsonc" "/auth.json" "/secrets/bot.token" "/cells/tester-memory/kept.lisp" "/SOUL.md"
      (:absent "operator-token" "this home's gateway token stays") (:absent "link/" "and its link")
      (:absent "composer-history" "and what was typed") (:absent "backups" "and no backup holds another")
      (:absent "elsewhere" "and nothing no row claims"))
    (nlk:with-json ((sessions :integer "sessions") (holds :array "holds"))
        (nlk:decode-json (nlk:read-text (nle::backup-note-file name)))
      (is (= 2 sessions) "its note counts the sessions")
      (is (find "auth.json" holds :test #'equal) "and names what it holds"))
    (is (equal "back" (nlk:profile-import archive :name "back")))
    (let ((home (nlk:profile-home "back")))
      (is (probe-file (merge-pathnames "secrets/bot.token" home)) "the keys come back")
      (is (null (nlk:symlink-target (merge-pathnames "secrets" home))) "a linked row comes back as its content")
      (is (null (nlk:symlink-target (merge-pathnames "cells/tester-memory" home))) "and so does a linked knowledge cell")
      (is (equal "remembered" (nlk:read-text (merge-pathnames "cells/tester-memory/kept.lisp" home))))
      (is (equal cell (nlk:symlink-target (merge-pathnames "cells/shipped" home))) "a cell's link as a link")
      (is (= 2 (backup-test-sessions (merge-pathnames "history.db" home))) "and the store holds the sessions"))))

(deftest backup-routes-make-list-send-restore-and-delete (with-temp-gateway (port))
  (with-saved-globals ((nle::*backup-job* nil))
    (seed-user-inputs "s-route" '("kept"))
    (write-temp-file (merge-pathnames "config.jsonc" nlk:*home*) "{}")
    (flet ((job () (with-gateway-http (port :get "/api/gateway/backups") (nlk:json-value body :object "job"))))
      (with-gateway-http (port :post "/api/gateway/backups")
        (is (= 200 status) "the answer comes at once")
        (is (equal "backup" (nlk:json-value body :string "job" "verb"))))
      (is-eventually (equal "done" (nlk:json-value (job) :string "state")) "and the backup is made")
      (let ((name (with-gateway-http (port :get "/api/gateway/backups")
                    (let ((rows (nlk:json-value body :array "backups")))
                      (is (= 1 (length rows)))
                      (is (= 1 (nlk:json-value (aref rows 0) :integer "sessions")))
                      (nlk:json-value (aref rows 0) :string "name")))))
        (multiple-value-bind (file status headers)
            (gateway-octets port :get (format nil "/api/gateway/backups/file?name=~a" name))
          (is (= 200 status))
          (is (equal "application/gzip" (gethash "content-type" headers)))
          (is (search (format nil "filename=\"~a\"" name) (gethash "content-disposition" headers)))
          (is (= (length file) (nlk:file-bytes (merge-pathnames name (nle::backups-directory)))) "the whole archive"))
        (is-route (port :get "/api/gateway/backups/file?name=../history.db") 400 "never a path out of the folder")
        (is-route (port :get (format nil "/api/gateway/backups/file?name=~a" name) :token nil) 401
                  "the operator's alone")
        (is-route (port :post "/api/gateway/backups/restore"
                        :body (format nil "{\"name\": ~s, \"profile\": \"default\"}" name))
                  400 "a profile that exists is refused at once" "profile_exists")
        (with-gateway-http (port :post "/api/gateway/backups/restore"
                            :body (format nil "{\"name\": ~s, \"profile\": \"again\"}" name))
          (is (= 200 status))
          (is (equal "restore" (nlk:json-value body :string "job" "verb"))))
        (is-eventually (equal "done" (nlk:json-value (job) :string "state")) "the restore lands")
        (is (search "nodecode -p again" (nlk:json-value (job) :string "text")) "and says how to open it")
        (is (= 1 (backup-test-sessions (merge-pathnames "history.db" (nlk:profile-home "again")))))
        (with-gateway-http (port :delete (format nil "/api/gateway/backups?name=~a" name))
          (is (equalp #() (nlk:json-value body :array "backups")) "a delete takes it"))
        (is (null (probe-file (nle::backup-note-file name))) "and its note")))))

(deftest backup-slash-makes-one-where-the-page-lists-it (with-temp-store ())
  (write-temp-file (merge-pathnames "config.jsonc" nlk:*home*) "{}")
  (is-carrying (text (nle:slash "/backup" :session-id "backup-slash"))
    "backed up to" "keep it where you keep passwords")
  (is (= 1 (length (nle::backups))) "the one the page lists")
  (is (search "does not take arguments" (nle:slash "/backup now" :session-id "backup-slash"))))

(deftest backup-job-runs-one-at-a-time-and-says-why-it-failed ()
  (with-saved-globals ((nle::*backup-job* nil))
    (let ((gate (bt2:make-semaphore)))
      (nle::start-backup-job :backup (lambda (step) (funcall step "waiting") (bt2:wait-on-semaphore gate) "made"))
      (is (signals-error nlk:protocol-error (nle::start-backup-job :backup #'identity)) "a second waits")
      (bt2:signal-semaphore gate)
      (is-eventually (eq :done (getf nle::*backup-job* :state)) "the first ends")
      (is (equal "made" (getf nle::*backup-job* :text))))
    (nle::start-backup-job :restore (lambda (step) (declare (ignore step)) (error "no room")))
    (is-eventually (eq :failed (getf nle::*backup-job* :state)) "a failure ends the job")
    (is (equal "no room" (getf nle::*backup-job* :error)) "saying why")))
