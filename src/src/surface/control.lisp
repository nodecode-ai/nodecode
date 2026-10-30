;;;; control.lisp --- the operator's control routes: how this organism runs,
;;;; its log, its keys, its cells. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The web page's Control pane moves what a shell moves with its own verbs,
;;;; through the same owners: `nodecode gateway' (background.lisp) says how
;;;; the organism runs and restarts it, /models saves a key
;;;; (SAVE-PROVIDER-API-KEY), /cells lists the folder and turns a cell on
;;;; (INSTALL-AND-LOAD-CELL) or off (UNINSTALL-CELL), and
;;;; RESTART-CELLS starts a peripheral again on the config as it is now.
;;;; Each route is a shell over its owner and keeps nothing of its own. What
;;;; a cell runs -- a channel, a scheduled job, a skill, an MCP server --
;;;; it serves from its own route (NLE:ROUTE), never from here.

(in-package #:nodecode.evolved)

;;; --- how this organism runs, and its stop and restart --------------------------

(defun serving-mode (state)
  "How this organism runs, STATE being its home's NLK:GATEWAY-STATE: :UNIT
when this process is the gateway unit's, :SHELL when a shell started it and
holds it (WATCH-FRONT), :SERVE when it runs headless on its own (`nodecode
serve', a `gateway run' by hand)."
  (cond ((eql (sb-posix:getpid) (nlk:gateway-state-main-pid state)) :unit)
        (*fronted* :shell)
        (t :serve)))

(defun lifecycle (mode verb)
  "What VERB (:RESTART or :STOP) does to this organism running as MODE:
(values TEXT ABLE), TEXT saying why it cannot be done from here when ABLE is
NIL."
  ;; Under its unit the service manager does both, as `nodecode gateway' does:
  ;; there is no stop that the unit would not start again at the next login
  ;; but `off'. A headless serve does both itself (SERVE-NEXT), and so does a
  ;; shell's restart: the organism runs its own command line again, still the
  ;; shell's, which attaches to it again. A shell's organism stops with the
  ;; operator's terminal, which a page does not take away.
  (ecase mode
    (:unit (values (if (eq verb :restart)
                       "the unit starts it again on this build, with config.jsonc as it is now"
                       "the gateway is turned off, as `nodecode gateway off' does: it stops now and no longer starts at login; `nodecode gateway on' brings it back")
                   t))
    (:serve (cond ((eq verb :stop)
                   (values "it lets the store go and exits; `nodecode serve', or opening nodecode, starts it again" t))
                  ((nlk:organism-binary)
                   (values "it lets the store go and runs its own command line again: this build, with config.jsonc as it is now" t))
                  (t (values "this image was not started from an installed binary, so it cannot run itself again" nil))))
    (:shell (if (eq verb :restart)
                (lifecycle :serve verb)
                (values "a shell runs this organism for its terminal: quit that shell" nil)))))

(define-gateway-route handle-gateway-service-http (env)
  "GET /api/gateway/service — how this organism runs (its unit, a headless
serve, a shell), where the gateway stands as `nodecode gateway' says it, the
home and the store, and what a restart and a stop would do here."
  (:route :get "/api/gateway/service" "service")
  (let* ((state (nlk:gateway-state))
         (mode (serving-mode state))
         (store (and (nlk:store-open-p) (namestring (nlk:store-path nlk::*store*)))))
    (flet ((verb (verb)
             (multiple-value-bind (text able) (lifecycle mode verb)
               (json-object "text" text "able" (and able t)))))
      (json-object "pid" (sb-posix:getpid)
                   "runs" (string-downcase mode)
                   "unit" (nlk:gateway-state-unit state)
                   "gateway" (nlk::gateway-summary state :command "nodecode gateway")
                   "home" (namestring nlk:*home*)
                   :opt "store" store
                   :opt "store_bytes" (and store (store-bytes store))
                   "restart" (verb :restart)
                   ;; A shell of a newer build asks its restart for the first
                   ;; moment no turn is in flight; a gateway without this word
                   ;; would restart at once, stopping every turn it runs.
                   "restart_idle" t
                   "stop" (verb :stop)))))

(defvar *lifecycle-wait* nil
  "The thread holding a stop or restart for the first moment no turn is in
flight (LIFECYCLE-HTTP :IDLE), or NIL.")

(defun lifecycle-http (verb &key idle &aux (mode (serving-mode (nlk:gateway-state))))
  "VERB (:RESTART or :STOP) this organism the way it runs, once the answer
is out -- under IDLE, once no turn is in flight either; refused, saying why,
where it cannot be done from here."
  (multiple-value-bind (text able) (lifecycle mode verb)
    (unless able
      (nlk::protocol-refuse :refused "~a" text))
    (flet ((act ()
             (cond ((member mode '(:serve :shell)) (serve-next verb))
                   ((eq verb :restart) (nlk:gateway-restart))
                   (t (nlk:gateway-off)))))
      ;; The process that answers is the one going away: the answer leaves
      ;; first, and the page waits for the gateway to come back. Held for
      ;; idle, the first poll is that wait, and one hold serves every ask.
      (if idle
          (unless (and *lifecycle-wait* (bt2:thread-alive-p *lifecycle-wait*))
            (setf *lifecycle-wait*
                  (spawn (format nil "gateway-~(~a~)-idle" verb)
                    (loop do (sleep *idle-poll-seconds*)
                          while (ignore-errors (nlk:any-active-turn-p)))
                    (act))))
          (spawn (format nil "gateway-~(~a~)" verb)
            (sleep 0.3)
            (act))))
    (json-object "runs" (string-downcase mode) "text" text :when idle "when" "idle")))

(define-gateway-route handle-gateway-restart-http (env)
  "POST /api/gateway/restart — this organism starts again on what its home
installed last, with config.jsonc as it is now: its unit restarts it, a
headless serve runs itself again; one that cannot is refused, saying why. A
body {\"when\": \"idle\"} holds it for the first moment no turn is in flight."
  (:route :post "/api/gateway/restart" "restart")
  ;; A caller from before the hold posts no body, or one that is not JSON:
  ;; its restart is the one it always was, never a refusal.
  (let ((request (ignore-errors (gateway-request-json env))))
    (lifecycle-http :restart :idle (and (hash-table-p request)
                                        (equal "idle" (gethash "when" request))))))

(define-gateway-route handle-gateway-stop-http (env)
  "POST /api/gateway/stop — this organism stops: under its unit the gateway
is turned off, a headless serve exits; a shell's is refused, saying why."
  (:route :post "/api/gateway/stop" "stop")
  (lifecycle-http :stop))

;;; --- the log ---------------------------------------------------------------------
;;; Under its systemd unit the organism's output is the journal, what `nodecode
;;; gateway logs' reads; otherwise it is whatever file this process writes to
;;; -- a launchd agent's log, a shell's severed stdio log, a serve's output
;;; sent to a file. A read is bounded: the last lines, or what came after the
;;; cursor the last read answered, which is how a page follows it.

(defparameter +log-window+ (* 256 1024)
  "The most of a log file one read takes, in bytes.")

(defun descriptor-path (fd)
  "The path the open descriptor FD names, or NIL where the system cannot say."
  #+linux (ignore-errors (sb-posix:readlink (format nil "/proc/self/fd/~d" fd)))
  ;; macOS has no /proc: fcntl's F_GETPATH (50) writes the path into a buffer
  ;; of MAXPATHLEN (1024) bytes.
  #+darwin (ignore-errors
            (let ((path (sb-alien:make-alien sb-alien:char 1024)))
              (unwind-protect (progn (sb-posix:fcntl fd 50 path)
                                     (sb-alien:cast path sb-alien:c-string))
                (sb-alien:free-alien path))))
  #-(or linux darwin) (progn fd nil))

(defun log-source (&aux (file (loop for fd in '(1 2)
                                    for path = (descriptor-path fd)
                                    when (and path (eq :file (nlk::lstat-kind path)))
                                      return path)))
  "Where this organism's log is read: (values :FILE PATH) for the regular
file its standard output or error goes to, (values :JOURNAL UNIT) for
systemd's journal, else (values NIL WHY)."
  (cond (file (values :file file))
        ;; systemd sets it for a process whose output it takes.
        ((uiop:getenv "JOURNAL_STREAM") (values :journal (nlk:gateway-unit-name)))
        (t (values nil (format nil "this organism writes to the terminal it was started in ~
                                      and keeps no log; send its output to a file, or run it as ~
                                      the gateway (`nodecode gateway on')")))))

(defun file-log-lines (path lines after)
  "(values LINES CURSOR) off the file at PATH: what it holds past byte AFTER,
else its last LINES lines, from a window of +LOG-WINDOW+ bytes at most;
CURSOR is the byte its last whole line ends at."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((end (file-length in))
           ;; A file now shorter than the cursor was cut or rotated: read it anew.
           (from (if (and after (<= after end)) after 0))
           (start (max from (- end +log-window+)))
           (octets (make-array (- end start) :element-type '(unsigned-byte 8))))
      (file-position in start)
      (read-sequence octets in)
      ;; A line still being written waits for its newline.
      (let* ((stop (1+ (or (position 10 octets :from-end t) -1)))
             (read (lines (sb-ext:octets-to-string octets :end stop
                                                         :external-format '(:utf-8 :replacement #\?)))))
        ;; A window that begins inside a line drops that line's tail.
        (when (> start from) (pop read))
        (values (if after read (last read lines)) (+ start stop))))))

(defun journal-log-lines (lines after)
  "(values LINES CURSOR) off the gateway unit's journal: the entries after the
journal cursor AFTER, else its last LINES; CURSOR is the journal's own."
  (multiple-value-bind (out err status)
      (apply #'nlk::unit-run (append (nlk:gateway-log-argv :lines lines)
                                     '("--quiet" "--show-cursor")
                                     (and after (list "--after-cursor" after))))
    (unless (eql status 0)
      (error "journalctl could not read the gateway's log: ~a" (first-line (or err out ""))))
    (let ((cursor after))
      (values (remove-if (lambda (line)
                           (when (uiop:string-prefix-p "-- cursor: " line)
                             (setf cursor (subseq line 11))))
                         (lines out))
              cursor))))

(define-gateway-route handle-gateway-logs-http (env)
  "GET /api/gateway/logs?lines=N&after=CURSOR — this organism's log: its last
N lines (200, at most 2000), or what was written after CURSOR, which every
answer carries for the next read to follow from."
  (:route :get "/api/gateway/logs" "logs")
  (let ((lines (min 2000 (or (gateway-query-integer env "lines") 200)))
        (after (folder-query env "after")))
    (multiple-value-bind (source where) (log-source)
      (unless source
        (nlk::protocol-refuse :no_log "~a" where))
      (multiple-value-bind (read cursor)
          (if (eq source :journal)
              (journal-log-lines lines after)
              (file-log-lines where lines (and after (parse-integer after))))
        (json-object "source" (string-downcase source) "where" where
                     "lines" (coerce read 'vector) "cursor" (princ-to-string cursor))))))

;;; --- the keys ----------------------------------------------------------------------
;;; /models's writer and file (SAVE-PROVIDER-API-KEY, auth.json), read the
;;; way a turn resolves a credential but without running a !command. A key
;;; never leaves the gateway: one says its last four characters, and only
;;; when it is long enough that four give nothing away.

(defun key-tail (key)
  "KEY's last four characters, when KEY is long enough that they say nothing
of the rest, else NIL."
  (and (stringp key) (>= (length key) 16) (char/= #\! (char key 0))
       (subseq key (- (length key) 4))))

(defun numbered-stem (id)
  "ID without the -NNNN (four digits or more) a copy was numbered with, else ID."
  (let ((dash (position #\- id :from-end t)))
    (if (and dash (<= 4 (- (length id) dash 1))
             (every #'digit-char-p (subseq id (1+ dash))))
        (subseq id 0 dash)
        id)))

(defun provider-fix (id sources)
  "(values FIX OF): what is wrong with ID, a provider config.jsonc's providers
name with no key -- \"pasted_key\" when ID reads as a key (KEY-SHAPED-P);
\"keyless_copy\" when OF, another of SOURCES ((ID . SOURCE) each) numbered
otherwise (NUMBERED-STEM), has a key -- else NIL."
  (if (key-shaped-p id)
      "pasted_key"
      (let ((of (car (find-if (lambda (row)
                                (and (string-not-equal (car row) id)
                                     (string-equal (numbered-stem (car row)) (numbered-stem id))
                                     (not (equal "none" (cdr row)))))
                              sources))))
        (and of (values "keyless_copy" of)))))

(defun masked-id (id)
  "ID, a key saved where a provider's name goes, as the listing says it: the
prefix keys share and its last four (KEY-TAIL), never the rest."
  (format nil "~a…~@[~a~]" (or (ppcre:scan-to-strings "^(?:[A-Za-z]{1,8}[-_]){1,2}" id) "")
          (key-tail id)))

(defun keys-rows (&aux (ids (configured-provider-inventory))
                       (named (copy-list ids))
                       (entries (json-value (read-shared-config) :object "providers")))
  "Every provider this home names -- in auth.json, in the config's providers,
as the default model's -- as (ID SOURCE TAIL FIX OF): where its key comes
from and its last four, and, for one only the config's providers name and
with no key, what is wrong with it (PROVIDER-FIX)."
  (when entries
    (loop for id being the hash-keys of entries do (pushnew id ids :test #'string-equal)))
  (when *provider* (pushnew *provider* ids :test #'string-equal))
  (let ((rows (mapcar (lambda (id)
                        (let* ((credential (ignore-errors (resolve-provider-credential id :probe t)))
                               (source (credential-source credential)))
                          (list id
                                (if (member source '(nil :public))
                                    "none"
                                    (substitute #\_ #\- (string-downcase source)))
                                (and (member source '(:config :api-key :env))
                                     (key-tail (credential-key credential))))))
                      (sort ids #'string-lessp))))
    (mapcar (lambda (row)
              (destructuring-bind (id source tail) row
                (multiple-value-bind (fix of)
                    (and (equal source "none")
                         entries (nth-value 1 (gethash id entries))
                         (not (member id named :test #'string-equal))
                         (not (and *provider* (string-equal id *provider*)))
                         (provider-fix id (mapcar (lambda (each) (cons (first each) (second each))) rows)))
                  (list id source tail fix of))))
            rows)))

(defun said-id (row)
  "The id the listing says ROW, a KEYS-ROWS row, by: a pasted key's MASKED-ID."
  (if (equal (fourth row) "pasted_key") (masked-id (first row)) (first row)))

(defun keys-json (&optional unchecked)
  "KEYS-ROWS as the page reads them -- each id as SAID-ID, since a key never
leaves the gateway, with its catalog name, where its key comes from, its
tail, and what is wrong with it as `fix' and `of' -- and the providers
/models opens on, each with the page that makes its key."
  (json-object
   "file" (namestring *auth-file-path*)
   "providers" (map 'vector
                    (lambda (row)
                      (destructuring-bind (id source tail fix of) row
                        (json-object "id" (said-id row)
                                     :opt "name" (json-value (models-catalog-table) :text id "name")
                                     "source" source
                                     :opt "tail" tail
                                     :opt "fix" fix
                                     :opt "of" of)))
                    (keys-rows))
   "suggest" (map 'vector (lambda (entry) (json-object "id" (car entry) "page" (cdr entry)))
                  +connect-pinned-providers+)
   :opt "unchecked" unchecked))

(defun forget-provider-entry (said &aux (row (find-if (lambda (row) (and (fourth row) (string= said (said-id row))))
                                                      (keys-rows))))
  "Take out of config.jsonc the providers entry the listing says as SAID and
flags (a FIX); refused for any other, so no entry a turn runs on goes."
  (unless row
    (nlk::protocol-refuse :invalid_request "~a is not an entry this list says to forget" said))
  (refuse-section-write (list "providers" (first row)) :remove)
  (config-set (list "providers" (first row)) :remove))

(define-gateway-route handle-gateway-keys-http (env)
  "GET /api/gateway/keys — every provider this home names and where its key
comes from (KEYS-JSON). POST {provider, key} saves KEY to auth.json the way
/models does, once the provider has not refused it; {provider, key: null}
takes a saved one out; {provider, forget: true} takes out of config.jsonc an
entry the listing flags, PROVIDER as the listing says it
(FORGET-PROVIDER-ENTRY). Each answers the listing; a key saved that nothing
answered adds `unchecked', what happened. POST {provider, key, check: true}
saves nothing and answers {verdict, words?}, PROVIDER-KEY-CHECK's verdict
(works, refused, unreachable, unchecked, or none when nothing was asked):
a shell's /models asks it here, where a cell's lane and its address run."
  (:route nil "/api/gateway/keys")
  (let ((unchecked nil) (checked nil))
    (when (eq (getf env :request-method) :post)
      (let* ((request (gateway-request-json env))
             (provider (json-value request :string "provider"))
             (key (json-value request :string "key")))
        (require-non-empty (list provider) "provider must be a non-empty string" :required t)
        (cond
          ((eq t (gethash "forget" request))
           (forget-provider-entry provider))
          ((eq t (gethash "check" request))
           (multiple-value-bind (verdict words) (and key (provider-key-check provider (nlk:trimmed key)))
             (setf checked (json-object "verdict" (if verdict (string-downcase verdict) "none")
                                        :opt "words" words))))
          (t
           (multiple-value-bind (verdict words) (and key (provider-key-check provider (nlk:trimmed key)))
             (when (member verdict '(:refused :unreachable))
               (error 'nlk:protocol-error :code :key_refused :field "key"
                                          :message (format nil "~a; nothing was saved" words)))
             (setf unchecked (and (eq verdict :unchecked) words))
             (save-provider-api-key provider key))))))
    (or checked (keys-json unchecked))))

;;; --- the cells ---------------------------------------------------------------------
;;; Presence is consent: a cell is on while its folder is in the
;;; one folder. On is INSTALL-AND-LOAD-CELL over the folder and the offered
;;; ones it stands on, then its start; off takes
;;; the folder out (UNINSTALL-CELL) and stops its peripheral. Code a folder
;;; loaded stays in this image until a restart, and a folder taken out does
;;; not start again (%START-CELLS): the page says which rows wait for the
;;; restart. The hub's index is the network's, read on a route of its own so
;;; the listing never waits for it.

(defun cell-off-refusal (name)
  "Why the cell NAME cannot be turned off from here, or NIL: the folders
that stand on it."
  ;; A cell that must stay for a reason of its own advises this (NLE:HOOK),
  ;; as the web cell does for the page it serves.
  (when-let (needers (mapcar #'cell-label (nlk:cell-dependents name)))
    (format nil "~{~a~^, ~} need~:[s~;~] it: turn ~:*~:[that~;those~] off first"
            needers (rest needers))))

(defun cell-level (record)
  "How RECORD stands: :ERROR when it did not load or its start was refused,
:WARNING when it loaded with something to say, :STARTED while it runs."
  (cond ((or (assoc nil record.failures) record.refusal) :error)
        ((or record.failures record.collisions record.skipped) :warning)
        (record.started-p :started)))

(defun cell-row-detail (record &aux (unit (assoc nil record.failures)))
  "RECORD in one line: why it failed when it failed as a unit, else its
kind, whether it runs, how many definitions it made when it made any, and
what it has to say."
  (if unit
      (format nil "failed: ~a" (first-line (princ-to-string (cdr unit))))
      (format nil "~(~a~)~:[~;, started~]~@[, refused: ~a~]~@[ · ~d definition~:p~]~
                   ~@[ · ~d failed~]~@[ · ~d skipped~]~@[ · collides with ~{~a~^, ~}~]"
              record.kind record.started-p
              (and record.refusal (first-line (princ-to-string record.refusal)))
              (and record.defined (length record.defined))
              (and record.failures (length record.failures))
              (and record.skipped (length record.skipped))
              (remove-duplicates (mapcar #'cdr record.collisions) :test #'string=))))

(defun folder-description (directory)
  "What the first .asd in DIRECTORY that says what it is for says, or \"\"."
  (or (ignore-errors
       (find-if #'plusp (mapcar #'nlk::declared-system-description
                                (nlk::cell-folder-systems (nlk::make-cell-folder directory)))
                :key #'length))
      ""))

(defun cell-row (name offer offers &aux (record (nlk:find-cell name))
                                         (on (nlk:installed-cell-p name))
                                         (folder (and on (merge-pathnames (format nil "~a/" name)
                                                                          nlk::*cells-directory*)))
                                         (truename (and folder (ignore-errors (truename folder))))
                                         (pin (if (and offer (nlk:offer-git offer))
                                                  (list (nlk:offer-git offer) (nlk:offer-commit offer)
                                                        (nlk:offer-path offer))
                                                  (and on (not offer) (nlk:hub-pin name)))))
  "The cell NAME as the page and /cells list it -- OFFER its entry in
OFFERS, shipped or the hub's, NIL for a folder that is on offer from
neither: what it is for, where it comes from, whether it is on (its folder
present) and where that folder points, what it brings or cannot find, how
it stands in this image -- whether it runs, each failure, skipped file and
collision whole, the files slow to load, how many definitions it made and
the commands it registered -- why its start was refused when it was, and
the config section it declares."
  (json-object "name" name
               "label" (cell-label name)
               ;; a folder on offer from neither says what its own .asd says
               "description" (cond (offer (nlk:offer-description offer))
                                   (folder (folder-description folder))
                                   (t ""))
               ;; a folder neither shipped nor the hub's is the operator's own, never switched here
               "source" (cond (pin "hub") (offer "shipped") (on "own") (t "gone"))
               "on" on
               :opt "git" (first pin) :opt "commit" (second pin) :opt "path" (third pin)
               ;; which of the hub's indexes lists it: Nodecode's own, or the community's
               :opt "index" (and offer (nlk:offer-index offer))
               :opt "missing" (and offer (nlk:offer-missing offer)
                                   (coerce (nlk:offer-missing offer) 'vector))
               :opt "brings" (when-let (others (and offer (not on)
                                                     (remove name (nlk:offer-closure (list name) offers)
                                                             :test #'string=)))
                               (map 'vector #'cell-label others))
               :opt "kept" (and on (or offer pin) (cell-off-refusal name))
               "loaded" (and record (eq record.state :loaded))
               :opt "kind" (and record (string-downcase record.kind))
               :opt "level" (and record (string-downcase (or (cell-level record) :loaded)))
               :opt "detail" (and record (cell-row-detail record))
               :opt "refused" (and record record.refusal (princ-to-string record.refusal))
               ;; always said, so a shell can tell an organism that reports it from one that predates it
               "started" (and record record.started-p t)
               :opt "failures" (and record record.failures
                                    (map 'vector (lambda (failure)
                                                   (json-object :opt "file" (car failure)
                                                                "text" (princ-to-string (cdr failure))))
                                         record.failures))
               :opt "skipped" (and record record.skipped
                                   (map 'vector (lambda (skip) (json-object "file" (car skip) "reason" (cdr skip)))
                                        record.skipped))
               :opt "collides" (and record record.collisions
                                    (map 'vector (lambda (collision)
                                                   (format nil "~a with ~a" (nlk::snapshot-key-label (car collision))
                                                           (cdr collision)))
                                         record.collisions))
               :opt "slow" (when-let (slow (and record (remove-if (lambda (ms) (< ms nlk::+slow-layer-file-ms+))
                                                                  record.timings :key #'cdr)))
                             (map 'vector (lambda (file) (json-object "file" (car file) "ms" (cdr file))) slow))
               :opt "defines" (and record record.defined (length record.defined))
               :opt "commands" (when-let (commands (loop for entry in *registered-commands*
                                                         when (equal name (slash-command-owner entry))
                                                           collect (slash-command-name entry)))
                                 (coerce commands 'vector))
               :opt "folder" (and folder (namestring folder))
               ;; a shipped folder is a link to where it ships from
               :opt "from" (and truename (string/= (namestring truename) (namestring folder))
                                (namestring truename))
               :opt "section" (cell-section-key name)))

(defun cells-json (&optional text &aux (offers (nlk:shipped-cells))
                                        (root nlk::*cells-directory*))
  "Every cell this release ships, then every other folder this home holds
or this image loaded, each as CELL-ROW says it; then the standing notices,
after TEXT when a switch said what came of it."
  (json-object
   :opt "text" text
   :opt "folder" (and root (namestring root))
   "cells" (map 'vector (lambda (name) (cell-row name (nlk:find-offer name offers) offers))
                 (append (mapcar #'nlk:offer-name offers)
                         (sort (set-difference (union (mapcar #'nlk:folder-name (nlk::cell-folders root))
                                                      (mapcar #'nlk:cell-name (nlk:cells))
                                                      :test #'string=)
                                               (mapcar #'nlk:offer-name offers) :test #'string=)
                               #'string<)))
   "notices" (gethash "notices" (standing-notices-payload))))

(defun install-and-load-cell (name offers)
  "Put the offered folder NAME in the one folder (NLK:INSTALL-CELL) and load
it into this image now (NLK:LOAD-CELL): => (values FAILURES FRESH),
FAILURES a line per thing its load failed on, FRESH NIL when this image had
loaded it already. Signals what the install refuses."
  ;; SWITCH-CELL-ON's step for each folder of the closure it turns on.
  (nlk:install-cell name :offers offers)
  (let ((records (nlk:load-cell name)))
    (values (loop for record in records
                  append (loop for (file . text) in (nlk:cell-failures record)
                               collect (format nil "FAILED ~@[~a: ~]~a" file text)))
            (or records (not (nlk:find-cell name))))))

(defun switch-cell-on (name &key commit &aux (offers (nlk:shipped-cells)))
  "Turn the cell NAME on: every offered folder its closure names installed
and loaded into this image now, then started. =>
what came of it, in a sentence. Refused when COMMIT, the pin a person read,
is not the hub's now."
  ;; A name the release does not ship is looked for on the hub (HUB-OFFERS-FOR).
  ;; INSTALL-CELL refuses a name on offer from neither and a folder whose
  ;; dependencies this image cannot find.
  (unless (nlk:find-offer name offers)
    (setf offers (append offers (hub-offers-for name commit offers))))
  (let ((entry (nlk:find-offer name offers)))
    (when (and commit entry (not (equal commit (nlk:offer-commit entry))))
      (nlk::protocol-refuse :refused "the hub moved ~a from ~a~@[ to ~a~] since it was read: nothing was installed; read it again"
                            (cell-label name) commit (nlk:offer-commit entry))))
  (let* ((names (or (nlk:offer-closure (list name) offers)
                    (error "no cell named ~a is shipped or on the hub" name)))
         (failures (loop for each in names
                         append (mapcar (lambda (line) (format nil "~a: ~a" (cell-label each) line))
                                        (install-and-load-cell each offers)))))
    (start-cells names)
    (broadcast-organism "organism.cells" (json-object "names" (coerce names 'vector)))
    (let ((record (nlk:find-cell name)))
      (format nil "~a is on~@[, with ~{~a~^, ~}~]~:[~;; ~:*~{~a~^; ~}; the next restart loads it again~]~@[; it did not start: ~a~]"
              (cell-label name) (mapcar #'cell-label (remove name names :test #'string=))
              failures (and record record.refusal (first-line (princ-to-string record.refusal)))))))

(defun switch-cell-off (name)
  "Turn the cell NAME off: its folder out of the one folder, its peripheral
stopped. => what came of it, in a sentence. Refused, saying why, while
another folder stands on it or it is the operator's own."
  (when-let (why (cell-off-refusal name))
    (nlk::protocol-refuse :refused "~a stays on: ~a" (cell-label name) why))
  (unless (nlk:uninstall-cell name)
    (nlk::protocol-refuse :refused "~a is ~:[not in ~a~;your own folder in ~a: take it out yourself~]"
                          (cell-label name) (nlk:installed-cell-p name) nlk::*cells-directory*))
  (when (nlk:find-cell name)
    (stop-cells (list name)))
  (format nil "~a is off; its code stays loaded in this organism until a restart" (cell-label name)))

(define-gateway-route handle-gateway-cells-http (env)
  "GET /api/gateway/cells — CELLS-JSON. POST {name, on} turns that cell
on or off (SWITCH-CELL-ON, SWITCH-CELL-OFF), on refused when a commit
rides with it and the hub pins another; POST {name} starts its
peripheral again on config.jsonc as it is now (RESTART-CELLS). A POST
answers the listing, with what came of it as text when it switched."
  (:route nil "/api/gateway/cells")
  (cells-json
   (when (eq (getf env :request-method) :post)
     (let* ((request (gateway-request-json env))
            (name (json-value request :string "name")))
       (require-non-empty (list name) "name must be a non-empty string" :required t)
       (multiple-value-bind (on switch) (json-value request :boolean "on")
         (cond ((not switch) (restart-cells name) nil)
               (on (switch-cell-on name :commit (json-value request :string "commit")))
               (t (switch-cell-off name))))))))

(define-gateway-route handle-gateway-cells-hub-http (env)
  "GET /api/gateway/cells/hub — the hub's entries this platform runs
(NLK:HUB-CELLS) as CELL-ROW says each, the repository and the commit it
is pinned at among them, and stale, why, when an index was read from the
copy kept of it; {url, error} when the index cannot be read."
  (:route :get "/api/gateway/cells/hub" "the cell hub")
  (let ((shipped (nlk:shipped-cells)))
    (handler-case
        (multiple-value-bind (hub stale) (nlk:hub-cells :shipped shipped)
          (json-object :opt "url" nlk:*hub-index-url*
                       "cells" (map 'vector (lambda (entry)
                                               (cell-row (nlk:offer-name entry) entry (append shipped hub)))
                                     hub)
                       :opt "stale" stale))
      (error (condition)
        (json-object :opt "url" nlk:*hub-index-url* "cells" #() "error" (princ-to-string condition))))))

;;; --- providers a cell serves, offered before it is installed ---------------------
;;; A folder's .asd may say what it serves (NLK:OFFER-SERVES): a provider, by
;;; its id, its name and how one signs in to it. The /models picker offers
;;; each such provider of a folder this home has not installed, so picking
;;; OpenCode Go or ChatGPT's Codex installs the folder that drives it
;;; (SWITCH-CELL-ON) instead of asking a person to know its name first. Only
;;; the release's own folders and Nodecode's own index are offered this way:
;;; the community's entries keep /cells' explicit install.

(defvar *provider-offers* nil
  "(READ-AT . OFFERS): the folders PROVIDER-OFFERS last read, the release's
and Nodecode's own index's, at READ-AT, a universal time.")

(defvar *provider-offers-refresh* nil
  "The thread rereading *PROVIDER-OFFERS*, while one runs.")

(defparameter +provider-offers-seconds+ 600
  "How long a read of the offers stands before the next ask rereads them, off
the asking thread.")

(defun read-provider-offers (&aux (shipped (nlk:shipped-cells)))
  "The release's folders and Nodecode's own index's entries (NLK:HUB-CELLS,
its kept copy when the network fails it) into *PROVIDER-OFFERS*; the
release's alone when no index can be read. It fetches no folder: a pick
does, so a launch that never picks downloads no cell."
  (setf *provider-offers*
        (cons (get-universal-time)
              (append shipped (ignore-errors (nlk:hub-cells :shipped shipped :community nil))))))

(defun hub-offers-for (name commit shipped)
  "The hub's entries to turn NAME on from: the copy of Nodecode's own index
read within +PROVIDER-OFFERS-SECONDS+ -- the picker's read -- when it pins
NAME at COMMIT, the pin a person read; else both indexes, read now."
  ;; A pick was two more index reads, own and community, 2.5-6.5 s from a far
  ;; network, for the answer the picker read a moment before. Any other pin,
  ;; or none, is the hub's now as before, so a pin the hub moved is refused.
  (let* ((recent (and commit (ignore-errors (nlk:hub-cells :shipped shipped :community nil
                                                           :max-age +provider-offers-seconds+))))
         (entry (and recent (nlk:find-offer name recent))))
    (if (and entry (equal commit (nlk:offer-commit entry)))
        recent
        (nlk:hub-cells :shipped shipped))))

(defun provider-offers (&aux (memo *provider-offers*))
  "Every provider an offered folder this home has not installed serves, as
(:CELL NAME :COMMIT PIN :PROVIDER ID :NAME LABEL :SIGN-IN HOW :DESCRIPTION
TEXT) plists in the folders' order; HOW is the folder's word, `key' or the
command line that signs in. A folder this image cannot install (OFFER-MISSING)
is not offered. One installed whose provider does not answer yet (a sign-in
left unfinished) is offered still, its plist ending :INSTALLED T, so the list
keeps it and picking it goes straight to the sign-in."
  ;; The first ask reads; a later one past +PROVIDER-OFFERS-SECONDS+ answers
  ;; what it has and rereads off its thread, so /models never waits on the hub.
  (cond ((null memo) (setf memo (read-provider-offers)))
        ((and (> (- (get-universal-time) (car memo)) +provider-offers-seconds+)
              (not *provider-offers-refresh*))
         ;; set before the thread starts, which clears it however it ends
         (setf *provider-offers-refresh* t)
         (spawn "provider-offers"
           (unwind-protect (ignore-errors (read-provider-offers))
             (setf *provider-offers-refresh* nil)))))
  (loop with connected = nil and read = nil
        for entry in (cdr memo)
        for name = (nlk:offer-name entry)
        for installed = (and (or (nlk:find-cell name) (nlk:installed-cell-p name)) t)
        unless (nlk:offer-missing entry)
          append (loop for serves in (nlk:offer-serves entry)
                       for provider = (cdr (assoc "provider" serves :test #'string=))
                       when (and (stringp provider) (plusp (length provider))
                                 ;; an installed folder's provider that answers
                                 ;; is listed as any connected one is
                                 (not (and installed
                                           (member provider
                                                   (if read connected
                                                       (setf read t
                                                             connected (ignore-errors (connected-provider-ids))))
                                                   :test #'string-equal))))
                         collect (append
                                  (list :cell name :commit (nlk:offer-commit entry)
                                        :provider provider
                                        :name (or (cdr (assoc "name" serves :test #'string=)) provider)
                                        :sign-in (or (cdr (assoc "sign_in" serves :test #'string=)) "key")
                                        :description (nlk:offer-description entry))
                                  (and installed (list :installed t))))))

(defun offered-provider-name (id &aux (memo (or *provider-offers*
                                                (progn (ignore-errors (provider-offers))
                                                       *provider-offers*))))
  "The name the folder serving provider ID gives it -- its serves line, as
PROVIDER-OFFERS last read the hub -- installed here or not; NIL when no
folder names it."
  ;; Installed and answering, the provider is listed as any connected one is,
  ;; and models.dev has no row for one such as claude-code: by its id alone,
  ;; `Claude Pro/Max (Claude Code)' became `claude-code' once picked.
  (loop for entry in (cdr memo)
        thereis (loop for serves in (nlk:offer-serves entry)
                      for name = (cdr (assoc "name" serves :test #'string=))
                      when (and (string-equal id (cdr (assoc "provider" serves :test #'string=)))
                                (stringp name) (plusp (length name)))
                        return name)))

;;; --- the index: what the organism keeps --------------------------------------------
;;; The page's Index tab reads and writes the definitions the organism keeps,
;;; the layer's and every knowledge cell's, as the index ranks them. A save is
;;; an eval of the definition's own text, kept by the scribe as the eval seam
;;; keeps one, its file and its commit; a forget is UNINTERN's act, the same
;;; way. Every definition is listed whatever project it is for: the page is
;;; the operator's view of all of them.

(nlk:access (known nlk:knowledge-entry))

(defun index-definition-json (known stats hot &aux (stat (gethash known.name stats)))
  "KNOWN, a definition the organism keeps, as the page lists it: what it is,
where it lives, and what the ledger (STATS) knows of its use."
  (json-object "name" known.name
               "description" (or known.description "")
               "kind" (cond ((nlk::data-entry-p known)
                             (if (string= known.operator "DEFINE-MEMORY") "memory" "skill"))
                            (t "definition"))
               "operator" (string-downcase known.operator)
               :opt "type" (and known.type (string-downcase (symbol-name known.type)))
               :opt "project" known.project
               "cell" known.label
               "file" (sb-ext:native-namestring known.path)
               "hot" (and hot t)
               :opt "origin" known.origin
               "score" (if stat (float (first stat)) 0)
               :opt "last" (and stat (plusp (second stat))
                                (nlk:iso-time (- (second stat) nlk:+unix-epoch+) :millis nil))))

(defun index-json (&optional said &aux (settings (index-settings))
                                       (stale (getf settings :stale-days))
                                       (stats (nlk:knowledge-stats :stale-days stale)))
  "Every documented definition the organism keeps, hot first in rank order,
the counts, and SAID: what the last write printed."
  (multiple-value-bind (hot cold)
      (nlk::hot-knowledge (remove-if-not #'nlk:knowledge-entry-description (nlk:knowledge-entries))
                          stats stale)
    (json-object "hot" (length hot) "cold" (length cold) "budget" (getf settings :hot-budget)
                 "definitions" (concatenate 'vector
                                            (mapcar (lambda (known) (index-definition-json known stats t)) hot)
                                            (mapcar (lambda (known) (index-definition-json known stats nil)) cold))
                 :opt "text" said)))

(defun index-act (text &optional (check (constantly t)))
  "Evaluate TEXT, one toplevel form CHECK accepts, as the eval seam does: the
scribe keeps or forgets what it defines or forgets. => what the scribe
printed. Refused when TEXT does not read, CHECK refuses it, or the form
signals."
  (let* ((*package* (find-package '#:nodecode.evolved))
         (form (handler-case (let ((*read-eval* nil)) (read-from-string text))
                 (error (condition)
                   (nlk::protocol-refuse :invalid_request "the text does not read: ~a" condition)))))
    (funcall check form)
    (string-trim '(#\Newline)
                 (with-output-to-string (*standard-output*)
                   (handler-case (eval form)
                     (error (condition) (nlk::protocol-refuse :refused "~a" condition)))
                   (nlk:record-definitions form text (length text)
                                           :session-id "operator, on the web page")))))

(defun index-keep (name text)
  "Keep TEXT, one definition of NAME (INDEX-ACT): refused when it defines
another name, since a new name is a new definition."
  (index-act text (lambda (form &aux (key (nlk:definition-key form)))
                    (unless (and key (string-equal (getf key :name) name))
                      (nlk::protocol-refuse :invalid_request "the text ~:[defines nothing~;defines ~:*~(~a~), not ~a: a new name is a new definition~]"
                                            (getf key :name) name)))))

(define-gateway-route handle-gateway-index-http (env)
  "GET /api/gateway/index — INDEX-JSON; GET ?name=NAME — that definition
with its text. POST {op: save, name, text} keeps the text as NAME's
definition (INDEX-KEEP), POST {op: forget, name} forgets it; each answers
the listing, with what the scribe said as text."
  (:route nil "/api/gateway/index")
  (if (eq (getf env :request-method) :post)
      (let* ((request (gateway-request-json env))
             (name (json-value request :string "name")))
        (require-non-empty (list name) "name must be a non-empty string" :required t)
        (index-json
         (dispatch (json-value request :string "op") equal
           ("save" (index-keep name (or (json-value request :string "text")
                                        (nlk::protocol-refuse :invalid_request "a save carries its text"))))
           ("forget" (if (ppcre:scan "^[A-Za-z0-9*+._<>=%!?&-]+$" name)
                         (index-act (format nil "(unintern '~(~a~))" name))
                         (nlk::protocol-refuse :invalid_request "~a is no name a definition takes" name)))
           (t (nlk::protocol-refuse :invalid_request "op is save or forget")))))
      (if-let (name (cdr (assoc "name" (gateway-query-parameters env) :test #'string=)))
        (let ((known (find name (nlk:knowledge-entries) :key #'nlk:knowledge-entry-name
                                                        :test #'string-equal)))
          (unless known (nlk::protocol-refuse :not_found "nothing kept is named ~a" name))
          (let ((object (index-definition-json known (nlk:knowledge-stats) nil)))
            (setf (gethash "text" object) (nlk::knowledge-text known))
            (json-object "definition" object)))
        (index-json))))

;;; --- `nodecode add' ---------------------------------------------------------------
;;; The on above, from a terminal: the organism serving this home turns the
;;; cell on through the route, live in every attached shell; with none
;;; running, its folders go in the one folder here and load at the next
;;; launch. Code from the hub is shown before it is cloned -- its repository,
;;; the commit it is pinned at, what it brings -- and installed on a yes; the
;;; commit rides the request, so the route refuses a pin the hub moved since.

(defun add-text (words &key (ask #'terminal-answer) (port (organism-port))
                            (token (gateway-configured-operator-token)))
  "`nodecode add nc://NAME [--yes]': the cell NAME, shipped or the hub's
and its nodecode- prefix optional, installed and on. => (values ANSWER CODE
STREAM)."
  ;; The preview goes to stdout ahead of the question, which ASK puts on
  ;; stderr: a script that says --yes still reads what it installed.
  (when (intersection '("--help" "-h" "help") words :test #'string=)
    (return-from add-text
      (values (nlk:verb-help-text (nlk:find-verb "add" *launch-verbs*) :command "nodecode") 0 :output)))
  (let* ((yes (member "--yes" words :test #'string=))
         (named (remove "--yes" words :test #'string=))
         (word (and (= 1 (length named))
                    (not (uiop:string-prefix-p "-" (first named)))
                    (cell-label (string-right-trim "/" (if (uiop:string-prefix-p "nc://" (first named))
                                                            (subseq (first named) 5)
                                                            (first named))))))
         (shipped (nlk:shipped-cells))
         (offers shipped))
    (unless (plusp (length word))
      (return-from add-text (values "add takes one cell: nodecode add nc://NAME [--yes]" 2 :error)))
    (flet ((named (offers) (or (nlk:find-offer word offers)
                               (nlk:find-offer (format nil "nodecode-~a" word) offers))))
      ;; the hub is read only for a name the release does not ship
      (let ((entry (or (named shipped)
                       (named (setf offers (append shipped (nlk:hub-cells :shipped shipped)))))))
        (unless entry
          (return-from add-text
            (values (format nil "no cell ~s is shipped or on the hub.~@[ Did you mean ~{~s~#[~; or ~:;, ~]~}?~]"
                            word (nlk:nearest-words word (mapcar (lambda (offer) (cell-label (nlk:offer-name offer)))
                                                                 offers)))
                    1 :error)))
        (let* ((name (nlk:offer-name entry))
               (label (cell-label name))
               (fresh (remove-if #'nlk:installed-cell-p (nlk:offer-closure (list name) offers)))
               (brings (mapcar #'cell-label (remove name fresh :test #'string=)))
               (pin (and (nlk:offer-git entry) (member name fresh :test #'string=) (nlk:offer-commit entry))))
          (when pin
            ;; A community entry says so: somebody other than Nodecode wrote it,
            ;; and a person at Nodecode read it at this commit.
            (format t "~a: ~a~%  from    ~a~@[ (~a/)~]~%  commit  ~a~%~@[  index   ~a~%~]~@[  brings  ~{~a~^, ~}~%~]~
                       It runs inside Nodecode with full access to your files and keys.~%"
                    label (nlk:offer-description entry) (nlk:offer-git entry) (nlk:offer-path entry) pin
                    (and (equal "community" (nlk:offer-index entry)) "community, read at this commit by Nodecode")
                    brings)
            (finish-output)
            (unless yes
              (let ((reply (funcall ask (format nil "Install ~a? [y/N]" label))))
                (unless (yes-answer-p reply)
                  (return-from add-text
                    (values (if reply
                                "nothing was installed"
                                (format nil "~a is code from the hub and there is no terminal to ask: nothing was installed; add --yes to install it"
                                        label))
                            1 :error))))))
          (values (cond (port
                         (format nil "~:[~a was already installed; ~;~*~]~a" fresh label
                                 (json-value (gateway-http-session-request
                                              port :post "/api/gateway/cells"
                                              (json-object "name" name "on" t :opt "commit" pin)
                                              token :read-timeout 600)
                                             :string "text")))
                        (fresh
                         (dolist (each fresh) (nlk:install-cell each :offers offers))
                         (format nil "~a is installed~@[, with ~{~a~^, ~}~]; nodecode is not running, so it loads at the next launch"
                                 label brings))
                        (t (format nil "~a is already installed; it loads at the next launch" label)))
                  0 :output))))))

;;; --- the profiles ------------------------------------------------------------------
;;; `nodecode profile' from the page: the roster, a new home, the one a bare
;;; launch boots, a home taken away, an archive to hand on -- each the
;;; kernel's verb (organism/profile.lisp), refused as the verb refuses. A
;;; profile is another home and so another organism, with a gateway and a
;;; page of its own: nothing here moves this page into one. A home's SOUL.md
;;; is read and written here as the file it is; the channel kit reads it
;;; (channels/kit/soul.lisp) as the voice of every channel conversation, and
;;; nothing else in the organism does.

(defun profile-row (record &aux (name (nlk:profile-record-name record))
                                (home (nlk:profile-record-home record))
                                (store (nlk::home-store-path home)))
  "The profile RECORD as the page lists it: what NLK:PROFILES says of it,
what its manifest says it was cloned as and shares, its store's size,
whether it has a SOUL.md, and why it cannot be deleted when it cannot
(NLK::DELETABLE-PROFILE)."
  (json-object "name" name
               "home" (namestring home)
               "description" (or (nlk:profile-record-description record) "")
               :opt "source" (nlk:profile-record-source record)
               :opt "created_at" (nlk:profile-record-created-at record)
               :opt "clone" (json-value (nlk::read-manifest home) :string "clone")
               :opt "shares" (when-let (shared (nlk::manifest-list home "shared"))
                               (coerce shared 'vector))
               "current" (nlk:profile-record-current-p record)
               "sticky" (nlk:profile-record-sticky-p record)
               :opt "pid" (nlk:profile-record-pid record)
               :opt "port" (nlk:profile-record-port record)
               :opt "store_bytes" (and (probe-file store) (store-bytes store))
               "soul" (and (probe-file (merge-pathnames "SOUL.md" home)) t)
               :opt "kept" (handler-case (progn (nlk::deletable-profile name) nil)
                             (nlk:profile-error (refusal) (princ-to-string refusal)))))

(defun profiles-json (&optional text)
  "Every profile as PROFILE-ROW says it, the default home first; the profile
this gateway runs and its home, which a custom $NODECODE_HOME keeps off the
roster; the folder named profiles are made in; and TEXT, what a verb said."
  (json-object :opt "text" text
               "this" (nlk:profile-label)
               "home" (namestring nlk:*home*)
               "folder" (namestring nlk:*profiles-directory*)
               "profiles" (map 'vector #'profile-row (nlk:profiles))))

(defparameter +profile-clones+ '(("settings" . :settings) ("all" . :all) ("blank" . :blank))
  "What NLK:PROFILE-CREATE starts a home with, by the word a request says it in.")

(defun profile-soul (name)
  "NAME's SOUL.md, the file under its home the channel kit reads; refused,
naming the nearest, when there is no profile NAME."
  (merge-pathnames "SOUL.md" (nlk:profile-record-home (nlk:require-profile name))))

(defun save-profile-soul (name content &aux (path (profile-soul name)))
  "NAME's SOUL.md made CONTENT, or taken out when CONTENT is blank: the
channel kit reads a blank file and none alike. => what came of it."
  (cond ((plusp (length (nlk:trimmed content)))
         (nlk:write-file-atomically path content)
         (format nil "~a's SOUL.md is saved; a channel's next message reads it" name))
        ((probe-file path)
         (delete-file path)
         (format nil "~a's SOUL.md is taken out; its channels speak without one" name))
        (t (format nil "~a has no SOUL.md" name))))

(define-gateway-route handle-gateway-profiles-http (env)
  "GET /api/gateway/profiles — PROFILES-JSON. POST {name, clone, from,
description, soul} makes a profile (NLK:PROFILE-CREATE): clone is settings
(the default), all or blank; from the profile it clones, this gateway's own
when left out; soul the SOUL.md it starts with, when not blank. DELETE
?name=NAME takes one away (NLK:PROFILE-DELETE), refused as
NLK::DELETABLE-PROFILE refuses. Each answers the listing, with what came of
it as text."
  (:route nil "/api/gateway/profiles")
  (:on (nlk:profile-error 400 "refused"))
  (profiles-json
   (case (getf env :request-method)
     (:post (let* ((request (gateway-request-json env))
                   (name (json-value request :string "name"))
                   (word (or (json-value request :string "clone") "settings"))
                   (clone (or (cdr (assoc word +profile-clones+ :test #'string=))
                              (nlk::protocol-refuse :invalid_request
                                                    "clone is settings, all or blank, not ~s" word)))
                   (from (json-value request :string "from"))
                   (soul (json-value request :string "soul")))
              (require-non-empty (list name) "name must be a non-empty string" :required t)
              (let ((home (nlk:profile-create name :clone clone :from from
                                                   :description (json-value request :string "description"))))
                (format nil "~a~@[; ~a~]" (nlk::profile-created-text name home clone from)
                        (and soul (plusp (length (nlk:trimmed soul))) (save-profile-soul name soul))))))
     (:delete (let ((name (folder-query env "name")))
                (require-non-empty (list name) "name=NAME says which profile" :required t)
                (nlk:profile-delete name)
                (format nil "deleted ~a: its whole home is gone" name))))))

(define-gateway-route handle-gateway-profile-use-http (env)
  "POST /api/gateway/profiles/use {name} — NAME becomes the profile a launch
that names none boots into (NLK:PROFILE-USE); this gateway stays where it
is. Answers the listing."
  (:route :post "/api/gateway/profiles/use" "profile use")
  (:on (nlk:profile-error 400 "refused"))
  (let ((name (json-value (gateway-request-json env) :string "name")))
    (require-non-empty (list name) "name must be a non-empty string" :required t)
    (nlk:profile-use name)
    (profiles-json (format nil "~a boots by default from the next launch on; this gateway stays on ~a"
                           name (nlk:profile-label)))))

(define-gateway-route handle-gateway-profile-soul-http (env)
  "GET /api/gateway/profiles/soul?name=NAME — NAME's SOUL.md: {name, file,
present, content}. POST {name, content} saves it (SAVE-PROFILE-SOUL), a
blank content taking the file out, and answers the same with what came of
it as text."
  (:route nil "/api/gateway/profiles/soul")
  (:on (nlk:profile-error 400 "refused"))
  (let* ((post (eq (getf env :request-method) :post))
         (request (and post (gateway-request-json env)))
         (name (if post (json-value request :string "name") (folder-query env "name"))))
    (require-non-empty (list name) "name must be a non-empty string" :required t)
    (let* ((text (when post
                   (save-profile-soul name (or (json-value request :string "content")
                                               (nlk::protocol-refuse :invalid_request
                                                                     "content must be a string")))))
           (path (profile-soul name))
           (present (probe-file path)))
      (json-object "name" name "file" (namestring path) "present" (and present t)
                   "content" (if present (nlk:read-text path) "") :opt "text" text))))

(define-gateway-route handle-gateway-profile-export-http (env)
  "GET /api/gateway/profiles/export?name=NAME — NAME's home as the archive
NLK:PROFILE-EXPORT writes, without its store, secrets and credentials, sent
to be saved."
  ;; Made under the cache and read back whole: a home without its store is
  ;; small, and nothing of it stays behind once it is sent.
  (:route :get "/api/gateway/profiles/export" "profile export")
  (:on (nlk:profile-error 400 "refused"))
  (let ((name (folder-query env "name"))
        (path (nlk:cache-path (format nil "nodecode/profile-export-~36r.tar.gz"
                                      (random (expt 36 8) (make-random-state t))))))
    (require-non-empty (list name) "name=NAME says which profile" :required t)
    (with-cleanup ((ignore-errors (delete-file path)))
      (nlk:profile-export name (namestring path))
      (let ((octets (alexandria:read-file-into-byte-vector path)))
        (list 200 (list :content-type "application/gzip"
                        :content-disposition (format nil "attachment; filename=\"nodecode-profile-~a.tar.gz\"" name)
                        :content-length (length octets))
              octets)))))
