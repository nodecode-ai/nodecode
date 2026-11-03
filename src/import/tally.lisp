;;;; tally.lisp --- which models a home ran, how much, and how lately.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A home's config names the model it is set to; what it ran is in its
;;;; ledgers, and on a working box the two part ways. The default model an
;;;; import sets is the one the operator ran, so it is read where the worlds
;;;; record use.
;;;; Two shapes hold every such record:
;;;;
;;;;   a daily ledger   a JSON array of days, each a date beside a map of
;;;;                    model -> tokens (Claude Code's stats-cache.json)
;;;;   a session table  a sqlite table of threads or sessions, each a model
;;;;                    (or a JSON object naming its provider too), token
;;;;                    counts, a time and maybe the reasoning effort it ran
;;;;                    at (Codex, OpenCode, Hermes)
;;;;
;;;; Where a world keeps one is its row's data (NLK:AGENT-WORLD-TALLY); what
;;;; it holds is read by shape, columns matched by name as the session
;;;; reader matches them. Only the last +TALLY-DAYS+ are read, through the
;;;; table's own time index.
;;;;
;;;; A model's SCORE is its share of the home's tokens on each day it ran,
;;;; halved for every +TALLY-HALF-LIFE+ days back. A day of a home's work
;;;; counts one whatever that home's token accounting, so worlds compare, and
;;;; a model run every day this fortnight outranks one run harder in July. A
;;;; session that used no tokens ran nothing.
;;;;
;;;; Which credential a row ran on is the plan's question
;;;; (TALLIED-MODEL-FACTS): use of a sign-in that stays behind picks nothing.

(in-package #:nodecode.import)

(defparameter +tally-days+ 90
  "How many days back a world's use counts.")

(defparameter +tally-half-life+ 14
  "The days over which a day of use comes to count half as much.")

(defvar *tally-now* nil
  "The time a tally is read at, in seconds since 1970, or NIL for now; a
test binds it.")

(defun tally-now ()
  (or *tally-now* (- (get-universal-time) nlk:+unix-epoch+)))

;;; --- the daily ledger ----------------------------------------------------------

(defun day-seconds (text)
  "A `YYYY-MM-DD' date as the seconds since 1970 of its noon UTC, or NIL."
  (and (stringp text)
       (ppcre:register-groups-bind ((#'parse-integer year month day))
           ("\\A(\\d{4})-(\\d{2})-(\\d{2})\\z" text)
         (ignore-errors (- (encode-universal-time 0 0 12 day month year 0) nlk:+unix-epoch+)))))

(defun ledger-rows (object &aux (rows '()))
  "The (SECONDS PROVIDER MODEL TOKENS EFFORT) rows of a daily ledger OBJECT:
each array member's days, a date beside a map of numbers."
  (when (hash-table-p object)
    (loop for days being the hash-values of object
          when (vectorp days)
            do (loop for day across days
                     for at = (and (hash-table-p day) (day-seconds (gethash "date" day)))
                     when at
                       do (loop for counts being the hash-values of day
                                when (and (hash-table-p counts)
                                          (every #'realp (alexandria:hash-table-values counts)))
                                  do (maphash (lambda (model tokens) (push (list at nil model tokens nil) rows))
                                              counts)))))
  rows)

;;; --- the session table ----------------------------------------------------------

(defun json-effort (text)
  "The reasoning effort a row's JSON TEXT names -- its own `effort' or
`variant', or one an object inside it names (Hermes's
reasoning_config.effort) -- or NIL."
  (let ((object (and (stringp text) (uiop:string-prefix-p "{" text) (ignore-errors (nlk:decode-json text)))))
    (and (hash-table-p object)
         (let ((index (object-index object)))
           (or (text-of index "effort" "reasoningEffort" "variant")
               (loop for value being the hash-values of object
                     thereis (and (hash-table-p value)
                                  (text-of (object-index value) "effort" "reasoningEffort"))))))))

(defun row-model (text provider)
  "=> (values MODEL PROVIDER EFFORT): a row's model TEXT beside its PROVIDER,
or the JSON object OpenCode keeps there, which names both and the effort."
  (if (and (stringp text) (uiop:string-prefix-p "{" text))
      (let ((index (object-index (ignore-errors (nlk:decode-json text)))))
        (values (text-of index "id" "modelID" "model")
                (or (text-of index "providerID" "provider") provider)
                (json-effort text)))
      (values text provider nil)))

(defun table-rows (path since &aux (db (sqlite:connect (namestring path) :busy-timeout 2000)))
  "The (SECONDS PROVIDER MODEL TOKENS EFFORT) rows after SINCE, in seconds
since 1970, of the sqlite store at PATH: its threads or sessions table, when
it has a model, a time and a token count."
  (nlk:with-cleanup ((ignore-errors (sqlite:disconnect db)))
    (let ((tables (mapcar #'first (sqlite:execute-to-list
                                   db "select name from sqlite_master where type = 'table'"))))
      (dolist (table '("threads" "sessions" "session") '())
        (when (find table tables :test #'string-equal)
          ;; pragma table_info: (cid name type notnull default pk)
          (let* ((columns (sqlite:execute-to-list db (format nil "pragma table_info(~a)" (quoted table))))
                 (names (mapcar #'second columns))
                 (model (column-like names "model" "model_id"))
                 (provider (column-like names "model_provider" "billing_provider" "provider" "provider_id"))
                 (at (column-like names "updated_at" "time_updated" "started_at" "created_at" "time_created"))
                 (effort (column-like names "reasoning_effort" "effort"))
                 (settings (column-like names "model_config" "config"))
                 (tokens (loop for (nil name type) in columns
                               when (and (cl:search "token" (normalize-key name))
                                         (member type '("INTEGER" "INT" "REAL") :test #'string-equal))
                                 collect name)))
            (when (and model at tokens)
              (let* ((newest (caar (sqlite:execute-to-list
                                    db (format nil "select max(~a) from ~a" (quoted at) (quoted table)))))
                     ;; OpenCode's times are milliseconds
                     (scale (if (and (realp newest) (> newest 1d11)) 1000 1)))
                (return
                  (loop for (provider-text model-text used stamp effort-text settings-text)
                          in (sqlite:execute-to-list
                              db (format nil "select ~a, ~a, ~{coalesce(~a, 0)~^ + ~}, ~a, ~a, ~a from ~a where ~a > ?"
                                         (if provider (quoted provider) "NULL") (quoted model)
                                         (mapcar #'quoted tokens) (quoted at)
                                         (if effort (quoted effort) "NULL") (if settings (quoted settings) "NULL")
                                         (quoted table) (quoted at))
                              (* since scale))
                        for (name named rung) = (multiple-value-list (row-model model-text provider-text))
                        for ran = (or (and (stringp effort-text) (plusp (length effort-text))
                                           effort-text)
                                      rung (json-effort settings-text))
                        when (and (stringp name) (realp stamp))
                          collect (list (/ stamp scale) (provider-id-text named) name used
                                        (and ran (string-downcase ran)))))))))))))

;;; --- the score -------------------------------------------------------------------

(defun tally-scores (rows &aux (now (tally-now))
                               (since (- now (* +tally-days+ 86400)))
                               (days (make-hash-table))
                               (models (make-hash-table :test #'equal)))
  "ROWS, (SECONDS PROVIDER MODEL TOKENS EFFORT) each, as one plist per
provider and model -- :PROVIDER :MODEL :SCORE :TOKENS :DAYS :LAST, and the
:EFFORT most of its tokens ran at -- the highest score first. A row out of
the window or of no tokens counts nothing."
  (let ((rows (loop for row in rows
                    for (at nil model tokens) = row
                    when (and (realp tokens) (plusp tokens) (<= since at (+ now 86400))
                              (plusp (length model)))
                      collect row
                      and do (incf (gethash (floor at 86400) days 0) tokens))))
    (loop for (at provider model tokens effort) in rows
          for day = (floor at 86400)
          for entry = (alexandria:ensure-gethash (cons provider model) models
                        (list :provider provider :model model :score 0 :tokens 0 :days '() :last 0
                              :efforts '()))
          do (incf (getf entry :score)
                   (* (/ tokens (gethash day days))
                      (expt 0.5d0 (/ (max 0 (- now at)) 86400 +tally-half-life+))))
             (incf (getf entry :tokens) tokens)
             (pushnew day (getf entry :days))
             (setf (getf entry :last) (max (getf entry :last) at))
             (when effort
               ;; (EFFORT . TOKENS) per rung it ran at
               (nlk:if-let (rung (assoc effort (getf entry :efforts) :test #'string=))
                 (incf (cdr rung) tokens)
                 (push (cons effort tokens) (getf entry :efforts))))))
  (sort (loop for entry being the hash-values of models
              do (setf (getf entry :days) (length (getf entry :days))
                       (getf entry :efforts) (car (first (sort (getf entry :efforts) #'> :key #'cdr))))
              collect (list* :effort (getf entry :efforts) (alexandria:remove-from-plist entry :efforts)))
        #'> :key (lambda (entry) (getf entry :score))))

;;; --- a home's tally ----------------------------------------------------------------

(defun newest-match (pattern)
  "The newest file PATTERN, relative to the agent home and maybe wild
(`.codex/state_*.sqlite'), names, or NIL: an older one a world migrated off
would count its use twice."
  (first (sort (or (ignore-errors (directory (nlk:agent-home-path pattern))) '())
               #'> :key (lambda (path) (or (ignore-errors (file-write-date path)) 0)))))

(defun tally-facts (home &aux (world (home-world home)) (facts '()) (notes '()))
  "=> (FACTS NOTES): the :TALLY facts the stores HOME's world records use in
yield, one per provider and model of each store, and a note for each store
that would not read."
  (when world
    (dolist (pattern (nlk:agent-world-tally world))
      (nlk:when-let (path (newest-match pattern))
        (let ((source (enough-namestring path (nlk:agent-home-path ""))))
          (handler-case
              (dolist (entry (tally-scores
                              (if (member (pathname-type path) '("db" "sqlite" "sqlite3") :test #'equalp)
                                  (table-rows path (- (tally-now) (* +tally-days+ 86400)))
                                  (ledger-rows (nlk:decode-json (nlk:read-text path))))))
                (push (make-fact :kind :tally :world (home-world-name home) :source source
                                 :id (format nil "~@[~a/~]~a" (getf entry :provider) (getf entry :model))
                                 :value entry)
                      facts))
            (error (condition)
              (push (format nil "~a: ~a" source condition) notes)))))))
  (list (nreverse facts) notes))
