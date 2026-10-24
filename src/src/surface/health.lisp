;;;; health.lisp --- what this organism is missing, said once, in one place.
;;;; Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An operator's first hour went: an install that reported success while a
;;;; different binary answered to the name, a config refusal that survived
;;;; every import, twenty-five of their own definitions absent behind a
;;;; component error, a channel that produced no line at all, and a provider
;;;; list emptied by one wrapped line three sections away. Every one of those
;;;; was known to the process while the operator was guessing. None of them
;;;; was on any screen.
;;;;
;;;; This is the one place that answers `what is wrong with this organism
;;;; right now'. It is a projection, never a source: every fact here is read
;;;; from an owner that already holds it — the cell records, the shared
;;;; config's model pin, the session store — and nothing is cached, so a fact
;;;; cannot go stale against the thing it describes.
;;;;
;;;; It carries no cell's vocabulary. A channel that is configured and not
;;;; running is a fact worth having, and the kernel must not know the word
;;;; `channel' to have it: HEALTH-FACTS is an ordinary function, so a cell
;;;; adds its own with the hook primitive that already exists —
;;;;
;;;;   (nle:hook 'nle:health-facts "channels"
;;;;             (lambda (next) (append (funcall next) (my-facts))))
;;;;
;;;; — and nothing here changes to let it.

(in-package #:nodecode.evolved)

(defstruct (health-fact (:constructor health-fact (key label detail &key tone remedy section))
                        (:copier nil))
  "One thing worth knowing about this organism."
  ;; KEY is what the fact is about (:model, :cells, ...) so a consumer can
  ;; filter without reading prose; LABEL is the lane name a report prints;
  ;; DETAIL the fact itself; TONE :ok, :warn or :danger; REMEDY what to do
  ;; about it, or NIL when there is nothing to do because nothing is wrong;
  ;; SECTION the key of the config section it is about, when it is about
  ;; one, where a screen opens that section's form.
  (key nil)
  (label "" :type string)
  (detail "" :type string)
  (tone :ok)
  (remedy nil)
  (section nil))

(nlk:access (fact health-fact))

(defun health-problem-p (fact)
  "Whether FACT is something the operator would want to act on."
  (member fact.tone '(:warn :danger)))

(defun session-count ()
  "How many sessions the open store holds, one count over the log, a
reflection's note left out (NLK::+NOTE-SOURCE+); NIL when it cannot be read."
  (ignore-errors (nlk:events :kind nlk::+kind-session-created+ :as :count
                             :where (list "session_id not in (select session_id from session_directory
                                                               where source = ?)"
                                          nlk::+note-source+))))

(defun cell-section-key (name)
  "The key of the config section the cell NAME declares, or NIL."
  (when-let (section (find name (nlk:sections) :key #'nlk:section-owner :test #'string=))
    (nlk:section-key (nlk:section-path section))))

(defun health-facts (&aux (refusal *model-config-refusal*)
                          (records (or (ignore-errors (nlk:cells)) '()))
                          (loaded (remove-if-not (lambda (record) (eq record.state :loaded)) records))
                          (failed (remove-if-not (lambda (record) (eq record.state :failed)) records))
                          ;; loaded, and its start refused: a peripheral that does nothing
                          (refused (remove-if-not (lambda (record) record.refusal) loaded)))
  "Everything worth saying about this organism right now, in reading order."
  ;; An ordinary function on purpose: a cell that knows a fact the core
  ;; does not — a channel configured and not running, a cron job that has
  ;; never fired — adds it by hooking this one, and no seam here has to learn
  ;; its vocabulary to allow it.
  (remove nil
          (append
           (list (with-json ((provider :string "general" "default_model" "provider")
                             (model :string "general" "default_model" "model"))
                     ;; A turn binds the config's pin, never the baked default the footer names.
                     (and (not refusal) (ignore-errors (read-shared-config)))
                   (cond
                     (refusal (health-fact :model "model" "the config's model pin was refused"
                                           :tone :danger :remedy refusal))
                     ((and provider model)
                      ;; a pin whose provider no lane serves refuses every turn
                      ;; (vr-101), asked where the lanes run: a shell's own
                      ;; lanes lack every cell's (PROVIDER-TURN-REFUSAL)
                      (let ((why (provider-turn-refusal provider)))
                        (health-fact :model "model" (format nil "~a on ~a" model provider)
                                     :tone (if why :danger :ok) :remedy why)))
                     (t (health-fact :model "model" "no model configured"
                                     :tone :warn
                                     :remedy "pick one in /models"))))
                 (health-fact :cells "cells"
                              (if records
                                  (format nil "~d loaded~@[ · ~d refused at start~]~@[ · ~d FAILED~]"
                                          (length loaded)
                                          (and refused (length refused))
                                          (and failed (length failed)))
                                  "none installed")
                              :tone (if (or failed refused) :danger :ok)))
           (mapcar (lambda (record)
                     (health-fact :cell (nlk:cell-name record)
                                  (or (cdr (first record.failures)) "did not load")
                                  :tone :danger
                                  :remedy (format nil "/cells names the condition~@[ · ~a~]"
                                                  (car (find-if #'car record.failures)))))
                   failed)
           ;; What it refused is in its own section, so that is where the fact points.
           (mapcar (lambda (record &aux (name (nlk:cell-name record)))
                     (health-fact :cell name (format nil "refused at start: ~a" record.refusal)
                                  :tone :danger
                                  :section (cell-section-key name)
                                  :remedy "change what it refused in its section of config.jsonc, then restart it"))
                   refused)
           (list
            ;; What keeps the organism running once its last shell closes: no process spawned.
            (when (nlk:gateway-unit-path)
              (multiple-value-bind (verdict pid) (nlk::store-death-verdict nlk::*default-store-path*)
                (let ((owner (and (eq verdict :live) pid)))
                  (apply #'health-fact :gateway "gateway"
                         (cond
                           ((not (nlk:gateway-installed-p))
                            '("off — nothing keeps this organism running once its last shell closes; `nodecode gateway on' does"))
                           ((and owner (nlk:gateway-process-p owner))
                            (list (format nil "on — serving, pid ~d" owner)))
                           (owner
                            (list (format nil "on — takes the organism over when pid ~d closes" owner)))
                           (t '("on, but nothing serves the store"
                                :tone :warn
                                :remedy "`nodecode gateway' says where it stands, `nodecode gateway logs' why")))))))
            ;; What this store holds — orientation, not a
            ;; problem. An operator who has just imported a
            ;; thousand sessions has no other way to see
            ;; that it landed. One count over the log, never
            ;; the directory's length: the landing screen
            ;; reads this on every render, and LIST-SESSIONS
            ;; reads each session's title and counts.
            (when-let (sessions (if (nlk:store-open-p) (session-count) 0))
              (health-fact :sessions "sessions"
                           (format nil "~d" sessions)))
            ;; What the exact request bodies take (bodies.lisp), beside what
            ;; they were as sent and the cap the oldest go past: one read
            ;; over a covering index, never the deltas. A refused cap
            ;; refuses every turn (RUN reads it), so that is said instead.
            (multiple-value-bind (bytes count size) (ignore-errors (nlk:request-bodies-size))
              (let ((cap (handler-case (request-bodies-cap) (error (condition) condition))))
                (cond ((typep cap 'error)
                       (health-fact :requests "requests kept" "request_bodies.keep_mb was refused"
                                    :tone :danger :remedy (princ-to-string cap)))
                      ((and count (plusp count))
                       (health-fact :requests "requests kept"
                                    (format nil "~:d call~:p in ~a (~a as sent), up to ~a · request_bodies.keep_mb"
                                            count (size-text bytes) (size-text size)
                                            (size-text cap)))))))))))

(defun health-report-text (&key (facts (health-facts)) problems-only)
  "FACTS as the block a report prints, or NIL when there is nothing to say."
  ;; PROBLEMS-ONLY keeps the ones an operator would act on — what a screen
  ;; with other work to do shows, where a whole report shows everything.
  (when-let (facts (if problems-only (remove-if-not #'health-problem-p facts) facts))
    (let ((width (loop for fact in facts maximize (length fact.label))))
      (format nil "~:{~&~va  ~a~@[ — ~a~]~%~}"
              (loop for fact in facts
                    collect (list width fact.label fact.detail
                                  (and (health-problem-p fact) fact.remedy)))))))

;;; --- the doctor: what only a look can say ---------------------------------------
;;; HEALTH-FACTS is what an owner already holds, cheap enough for every paint
;;; of the landing screen and every open of the Dashboard. A doctor goes and
;;; looks: it reads the whole store back through a connection of its own,
;;; asks whether the build on disk is still the one running, what the disk
;;; has free, whether config.jsonc parses and every section declared in it is
;;; whole, and whether the default model's provider takes its key.
;;; DOCTOR-FACTS is the passive facts and those looks in one list; /doctor
;;; prints it and the web page's Check everything shows it, so the two never
;;; say different things. An ordinary function too: a cell with a check of
;;; its own hooks it the way it hooks HEALTH-FACTS.

(defun store-file ()
  "The store this process serves, else the one its home opens."
  (if (nlk:store-open-p) (nlk:store-path nlk::*store*) nlk::*default-store-path*))

(defun store-bytes (path)
  "The store at PATH with its log, in bytes: the log is most of a store's
size until it checkpoints."
  (+ (or (nlk:file-bytes path) 0) (or (nlk:file-bytes (format nil "~a-wal" (namestring path))) 0)))

(defun call-with-store-reader (path function)
  "FUNCTION called with a SQLite connection of its own to the store at PATH,
closed after."
  ;; Never the store's own connection: every use of that one holds
  ;; *STORE-LOCK*, and a check that reads a gigabyte, or a copy that writes
  ;; one, would stop every turn and route in the image for as long as it
  ;; took. A second connection to a WAL database reads one consistent
  ;; snapshot while the writer goes on. PATH must exist: a connect makes an
  ;; empty database where there is none.
  (sqlite:with-open-database (db (namestring path) :busy-timeout 5000)
    (funcall function db)))

(defun store-integrity-fact (&optional (path (store-file)))
  "Whether the store at PATH reads back whole (SQLite's quick_check), and how
long reading it took."
  (if (not (probe-file path))
      (health-fact :integrity "store check" "no store yet")
      (let* ((start (get-internal-real-time))
             (rows (handler-case (call-with-store-reader
                                  path (lambda (db) (mapcar #'first (sqlite:execute-to-list db "pragma quick_check"))))
                     (error (condition) (list (first-line (princ-to-string condition))))))
             (took (format nil "~a read in ~,1f s" (size-text (store-bytes path))
                           (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
        (if (equal rows '("ok"))
            (health-fact :integrity "store check" (format nil "intact · ~a (quick_check)" took))
            (health-fact :integrity "store check"
                         (format nil "damaged: ~a~@[ (and ~d more)~] · ~a"
                                 (first rows) (and (rest rows) (length (rest rows))) took)
                         :tone :danger
                         :remedy (format nil "back it up now, before anything else writes to it; ~
                                              `sqlite3 ~a .recover' reads back what it can" (namestring path)))))))

(defun build-fact (&optional (exe #+linux (ignore-errors (sb-posix:readlink "/proc/self/exe")) #-linux nil))
  "Whether the executable this process runs, EXE, is still the file on disk:
Linux names one that was replaced `... (deleted)', a rebuild or an update
having put another where it was. NIL where there is no telling."
  (when exe
    (if (uiop:string-suffix-p exe " (deleted)")
        (health-fact :build "build" "the file this process runs from was replaced on disk after it started"
                     :tone :warn :remedy "a restart runs the build that is there now")
        (health-fact :build "build" (format nil "running as started · ~a" (or (nlk:organism-binary) exe))))))

(defun disk-fact (&optional (store-size (store-bytes (store-file))))
  "What the disk under the home has free, against what the store needs to
grow and to be copied whole by a backup."
  ;; `df -Pk' is POSIX, on Linux and macOS alike; Windows says nothing here.
  #+win32 (declare (ignore store-size))
  #-win32
  (multiple-value-bind (out err status)
      (ignore-errors (nlk:run-bounded (list "df" "-Pk" (uiop:native-namestring (nlk:home))) :seconds 5))
    (declare (ignore err))
    ;; The line under the header: FILESYSTEM BLOCKS USED AVAILABLE CAPACITY%.
    (when-let (line (and (eql status 0) (second (lines out))))
      (ppcre:register-groups-bind ((#'parse-integer blocks available))
          ("\\s(\\d+)\\s+\\d+\\s+(\\d+)\\s+\\d+%" line)
        (let ((total (* 1024 blocks)) (free (* 1024 available)))
          (health-fact :disk "disk" (format nil "~a free of ~a under ~a" (size-text free) (size-text total)
                                            (home-abbreviated (namestring (nlk:home))))
                       ;; Under 256 MB free a store's next write can fail.
                       :tone (cond ((< free (* 256 1024 1024)) :danger)
                                   ((< free (max (* 2 1024 1024 1024) (* 3 store-size))) :warn)
                                   (t :ok))
                       :remedy "free some space: the store grows with every turn, and a backup writes a copy of it"))))))

(defun config-facts ()
  "Whether config.jsonc parses, then every declared section it leaves wanting,
each in the words /settings shows (SECTION-NEEDS)."
  ;; A section the config vetoes (\"enabled\": false) wants nothing, and one
  ;; with no required member is whole however absent (SECTION-NEEDS-SETUP-P).
  ;; A section's own CHECK is not run: it dials the platform the section is
  ;; for, with the operator's token.
  (multiple-value-bind (config refusal) (read-shared-config)
    (if refusal
        (list (health-fact :config "config" "config.jsonc does not parse, so nothing reads it"
                           :tone :danger :remedy refusal))
        (let ((wanting (remove-if-not (lambda (section) (nlk:section-needs-setup-p section config))
                                      (nlk:sections))))
          (cons (health-fact :config "config"
                             (cond ((null config) "no config.jsonc yet")
                                   (wanting (format nil "parses · ~d of ~d declared section~:p incomplete"
                                                    (length wanting) (length (nlk:sections))))
                                   (t (format nil "parses · ~d declared section~:p, all complete"
                                              (length (nlk:sections))))))
                (mapcar (lambda (section &aux (key (nlk:section-key (nlk:section-path section))))
                          (health-fact :section key
                                       (nlk:section-needs section (nlk:section-table config (nlk:section-path section)))
                                       :tone :warn :section key
                                       :remedy "/settings sets it; or set it in config.jsonc and restart its cell"))
                        wanting))))))

(defun credential-fact (&aux (config (ignore-errors (read-shared-config))))
  "Whether the default model's provider has a key, and, for a key of its
own, whether the provider's model listing refuses it (PROVIDER-KEY-CHECK,
the free GET /models asks before it saves)."
  (with-json ((provider :string "general" "default_model" "provider")) config
    (when provider
      (let* ((credential (ignore-errors (resolve-provider-credential provider :probe t)))
             (source (and credential (credential-source credential)))
             (where (format nil "~a · ~a" provider
                            (case source
                              (:config "set in config.jsonc") (:api-key "saved in auth.json")
                              (:env "from the environment") (t (string-downcase source)))))
             ;; Only a key of its own is asked about: a !command is not run
             ;; for a question, and a login is its cell's to check.
             (key (and (member source '(:config :api-key :env)) (credential-key credential)))
             (refusal (and (plusp (length key)) (char/= #\! (char key 0))
                           (multiple-value-bind (verdict words) (provider-key-check provider key)
                             (and (member verdict '(:refused :unreachable)) words)))))
        (cond
          ((member source '(nil :public))
           (health-fact :credential "provider key" (format nil "none for ~a: a turn on it sends none" provider)
                        :tone :warn :remedy "add one: Settings on the web page, /models in a shell"))
          (refusal (health-fact :credential "provider key" (format nil "~a failed its key check" provider)
                                :tone :danger :remedy refusal))
          (t (health-fact :credential "provider key" where)))))))

(defun update-fact ()
  "A release waiting, or why none could be looked for: the poller's standing
notice, else how the config has it look."
  (if-let (notice (find "update" (nlk:standing-notices) :key #'first :test #'string=))
    (health-fact :update "update" (second notice)
                 :tone :warn :remedy "Update now on the web page's Gateway tab; `nodecode update' in a shell")
    (health-fact :update "update" (format nil "nothing waiting · update.mode ~a" (update-settings)))))

(defun doctor-facts ()
  "HEALTH-FACTS and every look a doctor takes, in reading order."
  (remove nil (append (health-facts) (list (store-integrity-fact) (build-fact) (disk-fact))
                      (config-facts) (list (credential-fact) (update-fact)))))

(defun health-facts-json (facts)
  "FACTS for a screen: each fact's key, label, detail and tone, its remedy
when it is a problem, and the config section it is about when it is about one."
  (map 'vector (lambda (fact)
                 (json-object "key" (string-downcase (princ-to-string fact.key))
                              "label" fact.label "detail" fact.detail
                              "tone" (string-downcase (symbol-name fact.tone))
                              :opt "remedy" (and (health-problem-p fact) fact.remedy)
                              :opt "section" fact.section))
       facts))

(define-gateway-route handle-gateway-health-http (env)
  "GET /api/gateway/health — HEALTH-FACTS for a screen (HEALTH-FACTS-JSON)."
  (:route :get "/api/gateway/health" "health")
  (health-facts-json (health-facts)))

(define-gateway-route handle-gateway-doctor-http (env)
  "POST /api/gateway/doctor — DOCTOR-FACTS, looked at now, as the health
route answers facts, with how long the looks took. => {facts, seconds}."
  ;; A POST because it goes and looks: it reads the whole store, and dials the
  ;; default provider's listing.
  (:route :post "/api/gateway/doctor" "doctor")
  (let* ((start (get-internal-real-time))
         (facts (health-facts-json (doctor-facts))))
    (json-object "facts" facts
                 "seconds" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
