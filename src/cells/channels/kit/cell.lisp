;;;; cell.lisp --- the generic cell entry: the channel host.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The gateway's cell lifecycle (START-CELLS) knows one convention:
;;;; START-CELL (config) => stop-thunk.
;;;; THIS system is where the channel use case lives: read channels.<id>
;;;; sections from the passed config, load each enabled adapter system
;;;; nodecode-channel-<id>, call its START-CHANNEL, aggregate stop thunks,
;;;; and register the /channels status command and its route for a page.
;;;;
;;;; A present section is an enabled section (hermes-agent's rule: the token
;;;; being named is the consent); an explicit "enabled": false vetoes. An
;;;; adapter FOLDER that is present without a section is the other half of
;;;; the same rule: the operator installed it and has not said how to run
;;;; it, which is a standing notice naming the section, not silence.
;;;;
;;;; Failure-class split: a channel that refuses (bad config, missing secret,
;;;; missing system) is that adapter's REFUSED status -- one standing notice
;;;; on every shell and in the model's cells section -- and that lane
;;;; absent; siblings and the gateway itself are unaffected. Transient
;;;; transport trouble is each adapter's own supervision, never re-touching
;;;; this host. RESTART-CELLS is the operator's "try again".

(in-package #:nodecode-channel-kit)

(nlk:access (cell nlk::cell))

(defparameter +adapter-system-prefix+ "nodecode-channel-"
  "An adapter system is nodecode-channel-<id>; the kit's own is the one
exception.")

(defun channel-symbol (channel-id name &aux (system (format nil "~a~a" +adapter-system-prefix+
                                                            channel-id)))
  "The adapter's exported NAME, its system loaded first, by the naming
convention: system nodecode-channel-<id> provides package NODECODE-CHANNEL-<ID>."
  (asdf:load-system system)
  (uiop:find-symbol* name (string-upcase system)))

;;; The adapter convention, two entries: START-CHANNEL (section) => stop
;;; thunk, PROBE-CHANNEL (section &key executor) => text. Neither name is
;;; exported by the kit -- an adapter package USES this one, so a kit export
;;; of the same name would become the adapter's own definition site.

(defun present-adapters (&aux (ids '()))
  "The channel ids whose adapter folder this organism loaded: every loaded
record in (NLK:CELLS) declaring a nodecode-channel-<id> system other than
the kit's, sorted."
  ;; A folder that failed to load is the boot line's business, not a setup
  ;; question.
  (dolist (cell (nlk:cells))
    (when (eq cell.state :loaded)
      (dolist (system cell.systems)
        (when (and (uiop:string-prefix-p +adapter-system-prefix+ system)
                   (not (string= system "nodecode-channel-kit")))
          (pushnew (subseq system (length +adapter-system-prefix+)) ids
                   :test #'string=)))))
  (sort ids #'string<))

(defun absent-adapters ()
  "The channel ids of the adapters shipped beside this organism that its
home has not installed: what a page offers to add, not to set up."
  (loop for offer in (nlk:shipped-cells)
        for name = (nlk:offer-name offer)
        when (and (uiop:string-prefix-p +adapter-system-prefix+ name)
                  (string/= name "nodecode-channel-kit")
                  (not (nlk:installed-cell-p name)))
          collect (subseq name (length +adapter-system-prefix+))))

(defun channel-notice-key (channel-id)
  "The board key an adapter's standing line rides under: its section name."
  (format nil "channels.~a" channel-id))

;;; Tests bind a scratch folder.
(nlk:define-startup-parameter *lease-directory* (nlk:cache-path "nodecode/leases/")
  "Where a lane's token lease lives: one lock file per token hash, shared
by every profile on this machine, so two organisms never poll one bot.")

(defun take-channel-lease (id section)
  "Hold the lease on SECTION's token for lane ID: a lock file named by
the token's hash under *LEASE-DIRECTORY*, taken for this process's life,
naming this profile and pid inside."
  ;; Hermes's one rule for profiles worth copying: two homes may not run one
  ;; bot — the platform delivers each update once, to whichever poller asks
  ;; first. => (LOCK . PATH), or NIL for a section without a token; a token
  ;; another live process holds is a config refusal naming the holder.
  (let ((token (or (ignore-errors (resolve-channel-secret section "token"))
                   (ignore-errors (resolve-channel-secret section "bot_token")))))
    (when (and token *lease-directory*)
      (let* ((path (merge-pathnames (nlk::sha256-text token) *lease-directory*))
             (lock (progn (ensure-directories-exist path) (nlk:lock-file path))))
        (unless lock
          (let ((holder (string-trim '(#\Space #\Newline)
                                     (or (ignore-errors (uiop:read-file-string path)) ""))))
            (config-error "channels.~a: this bot token is held by ~a — one organism per ~
                           token; stop that one, or give this profile a token of its own"
                          id (if (plusp (length holder)) holder "another organism"))))
        #-win32
        (ignore-errors
         (with-open-file (out path :direction :output :if-exists :supersede)
           (format out "profile ~a, pid ~d~%" (nlk:profile-name) (sb-posix:getpid))))
        (cons lock path)))))

(defun release-channel-lease (lease)
  "Let LEASE go; the file stays for the next taker to name itself in."
  (when lease
    (ignore-errors (nlk:unlock-file (car lease))))
  nil)

(defun channels-route (env &aux (query (quri:url-decode-params (or (getf env :query-string) ""))))
  "/api/channels: GET answers CHANNELS-JSON; POST ?op=restart starts every
lane again on config.jsonc as it is now, the operator's `try again'
(NLE:RESTART-CELLS over this kit's folder); POST ?op=probe&id=ID answers
{text}, what ID's token can see (PROBE); POST ?op=pair&code=CODE, or
&ask=ID for an ask the page lists, lets that person in (PAIR), and
?op=unpair&user=ID takes it back (UNPAIR), each answering CHANNELS-JSON with
the verb's line as its text."
  (flet ((param (name) (cdr (assoc name query :test #'string=))))
    (let ((op (and (eq (getf env :request-method) :post) (param "op"))))
      (cond ((null op) (channels-json))
            ((string= op "probe") (nlk:json-object "text" (probe (param "id"))))
            ((string= op "pair") (channels-json :text (pair (or (param "code") (ask-code (param "ask"))))))
            ((string= op "unpair") (channels-json :text (unpair (param "user"))))
            ((string= op "restart")
             (nle:restart-cells (nlk:cell-name
                                  (find "nodecode-channel-kit" (nlk:cells)
                                        :key #'nlk:cell-systems
                                        :test (lambda (system systems)
                                                (member system systems :test #'string=)))))
             (channels-json))
            (t (error "channels: no op ~s; restart, probe, pair or unpair" op))))))

(defun start-cell (config)
  "Start every configured channels.<id> adapter and account for every
adapter folder present; return one stop thunk."
  ;; Channel ids start in sorted order — the config object is a hash table and
  ;; MAPHASH order is unspecified, so determinism is imposed here.
  (let* ((channels (and (hash-table-p config) (gethash "channels" config)))
         (configured (and (hash-table-p channels)
                          (loop for id being the hash-keys of channels
                                when (stringp id) collect id)))
         (present (present-adapters))
         (ids (sort (union configured present :test #'string=) #'string<))
         (stops '())
         (unconfigured '()))
    ;; How a recording is read (transcribe.lisp): the section as the config
    ;; says it at start, absent meaning the local transcriber.
    (setf *transcription* (nlk:json-value config :object "transcription"))
    ;; How an answer is said out loud (speech.lisp), read the same way.
    (setf *speech* (nlk:json-value config :object "speech"))
    (dolist (id ids)
      (let ((section (and (hash-table-p channels) (gethash id channels)))
            (declared (nlk:find-section (list "channels" id))))
        (cond
          ((not (hash-table-p section))
           ;; Installed, never described: the one line that names what is
           ;; missing and where the shape is.
           (push id unconfigured)
           (set-channel-status id :state :unconfigured
                               :detail (if declared
                                           (nlk:section-summary declared nil)
                                           (format nil "channels.~a is absent; the adapter declares no section"
                                                   id))))
          ((not (config-boolean section "enabled" t))
           ;; Vetoed: quiet, and nothing stands for it.
           (clear-channel-status id))
          (nlk:*ephemeral*
           ;; A throwaway organism holds no channel: the live one keeps the
           ;; poller and the webhook, and a second would fight it for them.
           (set-channel-status
            id :state :refused
            :detail (format nil "not started: this organism is ephemeral ~
                                 (--ephemeral); the live organism keeps the channel")))
          (t
           (nlk:with-handlers ((error (condition)
                                 (push id unconfigured)
                                 (set-channel-status id :state :refused
                                                     :detail (princ-to-string condition))))
             ;; An incomplete section refuses in its declaration's words.
             (when (and declared (nlk:section-problems declared section))
               (config-error "~a" (nlk:section-summary declared section)))
             ;; The lease first: a token another profile's organism
             ;; already polls refuses this lane before it opens a socket.
             (let* ((lease (take-channel-lease id section))
                    (stop (handler-case (funcall (channel-symbol id '#:start-channel) section)
                            (error (condition)
                              (release-channel-lease lease)
                              (error condition)))))
               (cond ((functionp stop) (push (cons id (lambda ()
                                                        (unwind-protect (funcall stop)
                                                          (release-channel-lease lease))))
                                             stops))
                     (lease (push (cons id (lambda () (release-channel-lease lease)))
                                  stops)))))))))
    (setf *unconfigured* (sort unconfigured #'string<))
    ;; A bot answers whether or not a shell is open: the organism outliving
    ;; its last shell is the kernel's to arrange, once (NLE:KEEP-RUNNING).
    (when stops
      (let ((names (mapcar #'string-capitalize (sort (mapcar #'car stops) #'string<))))
        (nle:keep-running (format nil "the ~{~a~#[~; and ~:;, ~]~} bot~p"
                                  names (length names)))))
    ;; (help :channels) while an adapter needs setup.
    (when *unconfigured*
      (nle:set-help-topic :channels (format nil +setup-summary+ *unconfigured*)
                          (setup-primer-text)))
    ;; Where a lane runs — the one fact of the LANE rather than of the room —
    ;; rides behind the history, so every lane of a room sends the same
    ;; standing prefix and inherits its cache (nc-private#35).
    (nle:hook 'nle::read-live-sections +where-hook-key+ (live-section-advice))
    (register-channel-commands)
    (nle:route "/api/channels" #'channels-route)
    (lambda ()
      (dolist (entry stops)
        (handler-case (funcall (cdr entry))
          (error (condition)
            (warn "channel ~a stop signalled: ~a" (car entry) condition))))
      (nle:set-help-topic :channels nil)
      (nle:unhook 'nle::read-live-sections +where-hook-key+)
      (nle:route "/api/channels" nil)
      (nle:unregister-commands "nodecode-channel-kit")
      (setf *unconfigured* '()
            *transcription* nil)
      (clear-all-channel-status)
      t)))

(defun probe (channel-id &key section executor)
  "Ask the CHANNEL-ID adapter what its configured token can see -- its own
identity, the places it may speak -- as a text the operator or the model
reads to pick ids by name; never the token."
  ;; SECTION defaults to channels.<id> as the shared config reads right now;
  ;; EXECUTOR overrides the adapter's live executor (the scripted test seam).
  ;; Read-only against the platform: one identity call and the listing calls,
  ;; no state changes. A section that is absent, or refuses, answers a text
  ;; saying so.
  (let ((section (or section (nlk:json-value (nle:read-shared-config)
                                             :object "channels" channel-id))))
    (if (not (hash-table-p section))
        (format nil "channels.~a is absent: write the section first, ~
                     (config-set '(\"channels\" ~s) '(...)); the adapter ~
                     folder's config.example.jsonc shows it"
                channel-id channel-id)
        (handler-case
            (funcall (channel-symbol channel-id '#:probe-channel) section
                     :executor executor)
          (nlk:config-refusal (condition)
            (format nil "channels.~a: ~a" channel-id
                    (nlk:config-refusal-detail condition)))))))
