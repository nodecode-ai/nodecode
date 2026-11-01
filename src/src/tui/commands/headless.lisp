;;;; headless.lisp --- headless slash execution. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; The same catalog, run without a shell: a channel cell answering a
;;; Telegram or Discord `/models', a cron job, an eval snippet. One primitive
;;; over the canonical surfaces the two shells already call (SELECT-MODEL,
;;; SELECT-EFFORT, EVICT-SESSION-CONTEXT, the head move, the cells'
;;; commands), answering text where a shell would post a notice. Commands
;;; whose whole meaning is a dialog or a shell move (the pickers, /fork,
;;; /exit) stay the shells' and answer that they need one.

(defun slash-catalog ()
  "Every command a headless caller can run, in /help order: the builtins
with a headless arm, then each cell's registered command, as plists (:name
:description :usage :autocomplete :session :aliases :trigger) — USAGE the
display form when the command takes arguments, \"\" when the bare name is the
whole usage, :AUTOCOMPLETE true for a command whose argument tail a caller can
complete while it is typed (SLASH-COMPLETIONS answers it), :SESSION NIL for
one that acts on no session, which a client with none open runs as it is,
:ALIASES the other names SLASH answers it by, and :TRIGGER the character a
shell's composer opens it on."
  ;; The menu a channel publishes to its platform is this list; a cell's
  ;; command that shadows a builtin name is left out, as SLASH never reaches
  ;; it.
  (loop for entry in (append (remove-if-not #'slash-command-headless *slash-commands*)
                             (remove-if #'find-builtin-slash-command *registered-commands*
                                        :key #'slash-command-name))
        collect (list :name (slash-command-name entry)
                      :description (slash-command-description entry)
                      :usage entry.usage
                      :autocomplete (and entry.completer t)
                      :session entry.session
                      :aliases entry.aliases
                      :trigger entry.trigger)))

(defun models-argument-choices (text session-id)
  "The model catalog as completion choices for the tail TEXT typed so far:
one (:name \"provider/model\" :value \"provider model\") per catalog row
whose `provider/model' contains TEXT, case-insensitively — an empty TEXT
matches every row — in catalog order, read from the caches as they stand
(no refresh: a keystroke costs a read). After `aux ', the same rows pick the
auxiliary model; before it, `aux' is a choice of its own."
  ;; => list of (:name :value) plists, possibly empty.
  (declare (ignore session-id))
  (let* ((tail (string-left-trim '(#\Space #\Tab) (or text "")))
         (aux (and (> (length tail) 3) (string-equal "aux " tail :end2 4)))
         (needle (string-downcase (string-trim '(#\Space #\Tab) (if aux (subseq tail 4) tail)))))
    (append
     (when (and (not aux) (search needle "aux"))
       (list (list :name "aux — the auxiliary model" :value "aux")))
     (if-let (seat (and (not aux) (> (length tail) 7) (string-equal "decider " tail :end2 8)
                        (string-downcase (string-trim '(#\Space #\Tab) (subseq tail 8)))))
       ;; after `decider ', its ids and none
       (loop for id in (append (mapcar #'first +deciders+) '("none"))
             when (search seat id)
               collect (list :name (format nil "decider ~a" id) :value (format nil "decider ~a" id)))
       (when (and (not aux) (search needle "decider"))
         (list (list :name "decider — the decider model" :value "decider"))))
     (when (and (not aux) (search needle "backups"))
       (list (list :name "backups — the backup models" :value "backups")))
     (loop for row in (models-catalog-picker-rows)
           for value = (picker-row-value row)
           for label = (and value
                            (format nil "~a/~a" (first value) (second value)))
           when (and label (search needle (string-downcase label)))
             collect (list :name label
                           :value (format nil "~:[~;aux ~]~a ~a"
                                          aux (first value) (second value)))))))

(defun slash-completions (name text &key session-id)
  "One command's argument completions for the tail TEXT typed so far, in
SESSION-ID, the session the command would act on — the headless seam a
channel's on-the-fly menu answers through, beside SLASH and SLASH-CATALOG:
what SLASH-CATALOG marks :AUTOCOMPLETE, this answers, a builtin's or a
cell's."
  ;; => (:name :value) plists, or NIL for a command that offers none. The
  ;; catalog's COMPLETER slot names the function; it takes the tail typed so far
  ;; and the session id, as a headless arm takes its argument, and returns the
  ;; plists.
  (let* ((entry (or (find-builtin-slash-command name) (find-registered-command name)))
         (completer (and entry entry.completer)))
    ;; A COMPLETER that is a list of words offers the ones holding the tail.
    (if (listp completer)
        (loop with needle = (string-trim '(#\Space #\Tab) (or text ""))
              for word in completer when (search needle word :test #'char-equal)
              collect (list :name word :value word))
        (funcall completer (or text "") session-id))))

(defun slash-help-text (args session-id)
  "The catalog as text, one command per line."
  (declare (ignore args session-id))
  (format nil "~{~a~^~%~}"
          (loop for entry in (slash-catalog)
                collect (format nil "~a — ~a"
                                (or (json-value (getf entry :usage) :text)
                                    (format nil "/~a" (getf entry :name)))
                                (getf entry :description)))))

(defun require-headless-session (session-id name)
  (unless (stringp session-id)
    (error "/~a has no ambient session here — pass :session-id \"s-...\"" name))
  (unless (and (nlk:store-open-p) (nlk:session-exists-p session-id))
    (error "no session ~s in the open store" session-id))
  session-id)

(defun headless-head-move (session-id new-head boundary reason)
  "The durable head move and the peer broadcast, as the WS navigation runs
them (EXECUTE-NAVIGATE-CHECKPOINT) minus the workspace restore: a headless
move is conversation-only."
  (when (nlk:active-turn-p session-id)
    (error "a turn is in flight on ~a; cancel or await it first" session-id))
  (let ((previous (nlk:session-head-turn-id session-id)))
    (move-session-head-durably session-id new-head boundary reason)
    (broadcast-head-moved session-id new-head previous boundary reason)))

(defun headless-undo (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "undo")
  (let ((head (nlk:session-head-turn-id session-id)))
    (if (null head)
        "nothing to undo"
        (let* ((parent (gethash head (nlk:session-composed-parents session-id)))
               (prompt (turn-started-input-of head))
               ;; The prompt as one line of at most 120 characters.
               (flat (nlk:trimmed (substitute #\Space #\Newline (or prompt head)))))
          (headless-head-move session-id parent "user_message" "rewind")
          (format nil "rewound to before: ~a" (clip flat 120))))))

(defun headless-redo (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "redo")
  ;; The newest head move remembers where the operator came FROM; when that
  ;; origin fell off the current chain it is the branch tip redo returns to
  ;; (the WS navigate direction rule).
  (if-let (origin (nlk::redo-origin session-id))
    (progn (headless-head-move session-id origin "turn" "redo")
           (format nil "redone: the head is back on ~a" origin))
    "nothing to redo"))

(defun headless-clear (args session-id)
  (declare (ignore args))
  (require-headless-session session-id "clear")
  (cond ((null (nlk:session-head-turn-id session-id))
         "nothing to clear: the session is at its start")
        (t (headless-head-move session-id nil "turn" "rewind")
           "cleared: the session is back at its start; /redo returns")))

(defun model-catalog-list-text (provider)
  "The catalog the picker shows, as text: every provider with its count —
each row naming its own drill-down — or one provider's ids with its
listing failure beside them. The /models list answer."
  (if provider
      (multiple-value-bind (ids listing-error) (models provider)
        (format nil "~a: ~d model~:p~@[ (listing: ~a)~]~{~%  ~a~}"
                provider (length ids) listing-error ids))
      (format nil "~{~a~^~%~}"
              (loop for (provider . count) in (models)
                    collect (format nil "~a: ~d model~:p — /models list ~a"
                                    provider count provider)))))

(defun headless-models (args session-id &aux (tokens (split-words args))
                                             (aux (equalp (first tokens) "aux"))
                                             (picks (if aux (rest tokens) tokens)))
  "The room's /models: the session target's read, the catalog as text, or a
pick landed on SESSION-ID; after `aux', the auxiliary model's read, or a
pick landed on it -- this image's auxiliary target and general.auxiliary_model
together, pinning no session -- `default' taking it out."
  (cond
    ((and (not aux) (equalp (first tokens) "list"))
     (model-catalog-list-text (second tokens)))
    ((and (equalp (first tokens) "decider") (<= (length tokens) 3))
     (bind ((picks (rest tokens))
            ((_ _ outcome detail)
             (cond ((null picks) (values))
                   ((equalp picks '("none")) (select-decider-model nil))
                   (t (select-decider-model (first picks) :model (second picks))))))
       (multiple-value-bind (id model) (decider-model)
         (format nil "~a~@[~%shared config: ~a~]~:[~;~%/models decider <id> picks one (~{~a~^, ~}); /models decider none takes it out~]"
                 (decider-model-summary id model)
                 (when (eq outcome :failed) (format nil "NOT saved — ~a" detail))
                 (null picks) (mapcar #'first +deciders+)))))
    ((> (length picks) 2)
     "usage: /models [provider] [model] | aux [provider] [model | default] | decider [id | none] | list [provider]")
    ((and aux (null picks))
     (format nil "~a~%/models aux <provider> <model> changes it; /models aux default gives its work back to each session's own model"
             (auxiliary-target-summary)))
    (aux
     (bind (((_ _ outcome detail)
             (if (equalp picks '("default"))
                 (select-auxiliary-model nil)
                 (select-auxiliary-model (first (last picks))
                                         :provider (when (= 2 (length picks)) (first picks))))))
       (format nil "~a~@[~%shared config: ~a~]" (auxiliary-target-summary)
               (when (eq outcome :failed)
                 (format nil "NOT saved — ~a" detail)))))
    ((null tokens)
     (multiple-value-bind (pin-provider pin-model)
         (and (stringp session-id) (nlk:session-model-selection session-id))
       (format nil "~a~@[~%session pin: ~a~]~%~
                      /models <provider> <model> changes it, /models aux <provider> <model> the auxiliary model; ~
                      /models list shows the catalog"
               (model-selection-summary)
               (and (or pin-provider pin-model)
                    (format nil "~a/~a" (or pin-provider "-")
                            (or pin-model "-"))))))
    (t
     (require-headless-session session-id "models")
     (select-model (first (last tokens))
                   :provider (when (= 2 (length tokens)) (first tokens))
                   :session-id session-id))))

(defun headless-think (args session-id &aux (rung (string-trim '(#\Space #\Tab) args)))
  (cond
    ((zerop (length rung))
     (bind (((provider model _ _ _ effort) (gateway-target (and (stringp session-id) session-id)))
            (ladder (effort-ladder provider model)))
       (if (null ladder)
           (format nil "~a/~a offers no reasoning control" provider model)
           (format nil "reasoning effort for ~a/~a: ~a~%ladder: ~{~a~^ ~}~%~
                            /think <rung> sets it for this session and every new one from the next turn; /think default picks the model's own"
                   provider model
                   (or (effective-effort effort provider model) "provider default")
                   ladder))))
    (t
     (require-headless-session session-id "think")
     (select-effort (if (string-equal rung "default") nil rung)
                    :session-id session-id))))

(defun headless-rename (args session-id)
  "The /rename answer: SESSION-ID's name, ARGS made its name, or with
`default' the name its first prompt makes given back (SET-SESSION-TITLE)."
  (require-headless-session session-id "rename")
  (cond
    ((nlk:on-standby-p session-id)
     "nothing to name yet: a session is named once it has a prompt")
    ((zerop (length args))
     (format nil "this session is named ~s~%~
                  /rename NAME renames it; /rename default gives back the name its first prompt makes"
             (nlk::session-title session-id)))
    (t (format nil "renamed: ~a"
               (set-session-title session-id (if (string-equal args "default") "" args))))))

(defun slash-rename-op (model args)
  "/rename on a shell: the gateway's HEADLESS-RENAME for this shell's session."
  ;; The name is a fact in the gateway's store, so that image answers, the
  ;; same text a room or the web page's composer gets.
  (with-slash-notice ("rename")
    (json-value (gateway-slash-answer model (format nil "/rename ~a" args)) :string "text")))

(defun markdown-export-path (args name)
  "Where /export ARGS writes the file the gateway named NAME: ARGS as a path
from this shell's folder, NAME inside it when it is a folder, NAME in this
shell's folder when ARGS is empty."
  (let* ((here (uiop:ensure-directory-pathname (workspace-directory)))
         (given (and (plusp (length args)) (merge-pathnames (nlk:expand-home args) here))))
    (cond ((null given) (merge-pathnames name here))
          ((or (uiop:directory-pathname-p given) (uiop:directory-exists-p given))
           (merge-pathnames name (uiop:ensure-directory-pathname given)))
          (t given))))

(defun slash-export-op (model args)
  "/export [PATH]: this shell's session as Markdown, in a file where this
shell runs."
  ;; The gateway writes the text (SESSION-MARKDOWN) and this shell the file,
  ;; so a shell attached from another machine keeps the file on its own.
  (if (home-screen-p model)
      "nothing to export yet: the session has no prompt"
      (notice-off-loop
       model "export: writing…"
       (lambda ()
         (with-slash-notice ("export")
           (multiple-value-bind (octets headers)
               (gateway-model-http-call
                model :get (format nil "/api/gateway/sessions/~a/export?format=markdown"
                                   model.session-id)
                nil :raw t)
             (let ((path (markdown-export-path
                          args (or (ppcre:register-groups-bind (name)
                                       ("filename=\"([^\"/\\\\]+)\"" (gethash "content-disposition" headers))
                                     name)
                                   "session.md"))))
               (ensure-directories-exist path)
               (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
               (format nil "exported to ~a" (uiop:native-namestring path)))))))))

(defun headless-evict (args session-id)
  (require-headless-session session-id "evict")
  ;; Why nothing went, or the Zig contextEvictMessage ack — the realized
  ;; eviction, not the request. A turn in flight refuses, as the route does:
  ;; its boundary would be planned over a moving log. What went is published,
  ;; so a shell attached to the session sees the divider whoever asked.
  (multiple-value-bind (percent through valid-p) (evict-args args)
    (let* ((busy (and valid-p (nlk:active-turn-p session-id)))
           (cursor (and valid-p (not busy) (session-cursor session-id)))
           (plan (and valid-p (not busy)
                      (evict-session-context session-id :percent percent :through through))))
      (when plan (publish-turn-facts session-id cursor))
      (cond ((not valid-p) *evict-usage-hint*)
            (busy "a turn is in flight; cancel or await it first")
            ((null plan) (or (evict-decline-text session-id :through through) "nothing to evict"))
            (t (format nil "Evicted ~d turn~:p (~d% of history context) · ~d message~:p retained"
                       (getf plan :evicted-turn-count)
                       (getf plan :realized-percent)
                       (getf plan :retained-message-count)))))))

(defun headless-cells (args session-id)
  "This process's folder report — the same local-process rule as /gc."
  (declare (ignore args session-id))
  (render-cells (nlk:cells)))

(defun headless-doctor (args session-id)
  "What is missing rather than what loaded, then what only a look can say
(DOCTOR-FACTS): the facts describe THIS image — its model pin, its folder,
its store."
  (declare (ignore args session-id))
  (health-report-text :facts (doctor-facts)))

(defun notice-off-loop (model placeholder thunk)
  "PLACEHOLDER as a shell's answer now, and the text THUNK answers put in the
composer status once it lands off the loop: a slash whose work takes seconds."
  ;; Without a loop (a test drives the op) THUNK's text is the answer itself.
  (cond ((null model.program) (funcall thunk))
        (t (off-loop model thunk
                     (lambda (model text)
                       (show-status model (or (off-loop-failure text) text)
                                    :error-p (and (off-loop-failure text) t))
                       (pin-transcript-bottom model)
                       (render-state model)))
           placeholder)))

(defun slash-doctor-op (model args)
  "/doctor on a shell: the gateway's HEADLESS-DOCTOR, asked for off the loop."
  ;; The organism a shell drives is its gateway's: the pin a turn binds, the
  ;; folder that started, the store it serves are that image's, and this is
  ;; what the web page's Check everything shows. An attached shell's own image
  ;; holds no store and binds no turn.
  (declare (ignore args))
  (notice-off-loop model "doctor: looking…"
                   (lambda () (json-value (gateway-slash-answer model "/doctor") :string "text"))))

(defun headless-gc (args session-id &aux (arg (string-trim " " args)))
  "The collection counters of this image — the one whose pauses the
operator feels — or, for `reset', a fresh attribution window."
  (declare (ignore session-id))
  (cond
    ((zerop (length arg))
     (render-collection-report (nlk:collection-stats)
                               (nlk:gc-attribution-stats)))
    ((string-equal arg "reset")
     (nlk:reset-gc-attribution)
     "gc attribution window reset")
    (t "Usage: /gc [reset]")))

;;; --- /context as a picture --------------------------------------------------