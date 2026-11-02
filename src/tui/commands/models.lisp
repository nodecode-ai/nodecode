;;;; models.lisp --- the HTTP control senders and /models with its provider catalog. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- HTTP control senders ---------------------------------------------------
;;; Thin typed callers over the gateway's request-reply control ops; each
;;; returns the decoded JSON object (GATEWAY-HTTP-ERROR on refusal). A dialog
;;; or slash surface composes these; nothing here renders. Every one takes the
;;; MODEL its caller already holds and rides GATEWAY-MODEL-HTTP-CALL, so the
;;; port/token pair is named once, in that function, and never plumbed by
;;; hand at a call site.

(defun session-control-call (model method op &key body (session-id model.session-id))
  "One control call under a session's path: METHOD on
/api/gateway/sessions/<SESSION-ID>/OP — this shell's own session unless one
is named — with BODY (a JSON object, or NIL)."
  (gateway-model-http-call
   model method (format nil "/api/gateway/sessions/~a/~a" session-id op) body))

;;; --- the /models provider catalog ------------------------------------------
;;; The list the wizard opens on is the models.dev provider catalog, not the
;;; four SDK lanes this image happens to register: the Zig dialog fills it
;;; from the gateway's models catalog response
;;; (models_catalog.zig CONNECTDIALOGCATALOGFROMGATEWAY — one row per
;;; provider, the catalog display name as the label and `N models` as the
;;; detail), and this shell reads the models.dev cache file every process
;;; keeps current (provider.lisp MODELS-CATALOG-TABLE). No gateway round-trip is
;;; involved on either half: the save side is client-side too, entering
;;; SAVE-PROVIDER-API-KEY into the shared ~/.nodecode/auth.json exactly like
;;; the Zig TUI's persistConnectDialogApiKey.
;;;
;;; Two additions over the Zig rows: a provider this machine can already
;;; authenticate says so after the count, with its credential source, and
;;; once one is connected it heads the list under Suggested, so the list
;;; also answers "which of these am I connected to" and a saved key returns
;;; to it: an operator connects every provider they have a key for, then
;;; Enter on one goes on to its models. The count copy is the Zig one
;;; verbatim.

;;; --- /models ---------------------------------------------------------------
;;; The wizard state machine lives in tui/dialog.lisp (CONNECT-DIALOG); this op
;;; injects the transport-facing halves: the provider rows, the credential
;;; sink (SAVE-PROVIDER-API-KEY into the shared auth.json — client-side on
;;; both shells, exactly like the Zig persistConnectDialogApiKey), and what
;;; Enter on a connected provider opens (the models dialog, the Zig
;;; post-connect flow).

(defparameter +connect-pinned-providers+
  '(("anthropic" . "console.anthropic.com/settings/keys")
    ("openai" . "platform.openai.com/api-keys")
    ("openrouter" . "openrouter.ai/settings/keys")
    ("google" . "aistudio.google.com/apikey")
    ("deepseek" . "platform.deepseek.com/api_keys")
    ("moonshotai" . "platform.moonshot.ai/console/api-keys")
    ("zai" . "z.ai/manage-apikey/apikey-list"))
  "The providers the /models list opens on, in this order, each with the
page that makes its API key; the rest follow by name.")

(defun provider-id-refusal (id)
  "Why this build cannot serve the provider ID, in words the operator reads,
or NIL when a lane drives it."
  ;; Asked before a /models key is: a key saved for a provider no lane speaks
  ;; is a secret in auth.json that nothing can use; without a lane, a
  ;; /models pick would be refused. CONFIGURED-PROVIDER-LANE is the
  ;; resolution a turn runs, so what this accepts is what a turn can dial.
  ;; A key typed where the id goes is refused before any lane is asked, and
  ;; never echoed: a configured one has a lane.
  (if (key-shaped-p id)
      "That reads as an API key, not a provider id: pick the provider from the list, and the key step comes after it."
      (unless (configured-provider-lane id)
        (let ((catalog (models-catalog-table)))
          (cond ((configured-provider-entry id) (selection-refusal id))
                ((and (hash-table-p catalog) (json-value catalog :object id))
                 (format nil "~a is in models.dev, but this build has no lane for its API yet. ~
                              To use it, add providers.~a with an sdk and a base_url to config.jsonc."
                         id id))
                ((null catalog) (format nil "The models.dev list is not loaded yet, so ~a cannot be checked. ~
                                             Try again in a moment." id))
                (t (format nil "~a is not a models.dev provider and no providers.~a is configured. ~
                                Check the id at models.dev."
                           id id)))))))

(defvar *key-checks* (make-hash-table :test 'equalp)
  "By provider id, (VERDICT WORDS) when the key saved for it was checked and
did not answer: a PROVIDER-KEY-CHECK verdict -- :UNCHECKED, :REFUSED,
:UNREACHABLE -- of the key /models saved, or of the import's check of its
target (SETUP-IMPORT-VERIFY), and a sentence naming the provider and what
happened. A key that answered, or that nothing asked about, has no entry.")

;;; Live-only: the list and the walk's step line read it, and a restart knows
;;; what it checks again. Without this, a key saved over an unanswered check
;;; would read `connected' beside the notice that said so.
(defun key-check-state (provider)
  "How the connect list names PROVIDER: `needs attention' when no turn can
run on it (SELECTION-REFUSAL), whatever its key; else, when its key's last
check did not answer, `not checked', `key refused' or `not answering'; else
NIL."
  ;; Without this, a provider entry with no base_url would read `acme connected'
  ;; and `acme not checked' when a key typed for it is saved over the refusal.
  ;; A shell asks the gateway's answer: a cell's lane runs there only.
  (if (provider-turn-refusal provider)
      "needs attention"
      (case (first (gethash provider *key-checks*))
        (:unchecked "not checked")
        (:refused "key refused")
        (:unreachable "not answering"))))

(defun connect-picker-rows ()
  "The provider-step rows: once a provider is connected
(CONNECTED-PROVIDER-IDS), a Suggested heading over the connected
providers, then the pinned providers, then every other
models.dev provider the cached catalog carries that a lane can drive -- a
catalog package one serves, or a connected provider -- then the Zig `Other
provider` entry (setCatalog appends it after the gateway rows, so it closes
the list on both shells)."
  ;; A new operator meets 302.AI, Abacus and abliteration.ai first, and every
  ;; provider whose package no lane speaks, which a turn could only refuse.
  ;; Without a catalog cache only that entry survives; its id step needs the
  ;; catalog too, and says so (PROVIDER-ID-REFUSAL) rather than saving a key
  ;; no lane could use.
  ;; Sorted by display name then id: Zig walked a map order a Lisp hash table lacks.
  ;; A provider an uninstalled folder serves is listed too, and picking it
  ;; installs that folder (PICKER-PROVIDER-OFFERS).
  (let ((catalog (models-catalog-table))
        ;; The ids worth a PROVIDER-AUTH-STATE probe, which opens a file per
        ;; call.
        (connected (picker-connected-provider-ids))
        (listable (picker-listable-provider-ids))
        (model-rows (models-catalog-picker-rows))
        (rows '()))
    (labels ((member-p (id ids) (member id ids :test #'string-equal))
             (provider (row) (getf (picker-row-value row) :provider))
             (models (id)
               (count-if (lambda (row &aux (value (picker-row-value row)))
                           (and value (string-equal (first value) id)))
                         model-rows))
             (row (id label state detail)
               (push (make-picker-row
                      :label label :detail detail
                      ;; CONNECTED: the row reads `connected', and Enter
                      ;; on it goes on with the key it has (CONNECT-DIALOG-KEY)
                      :value (list :provider id
                                   :key-page (cdr (assoc id +connect-pinned-providers+
                                                         :test #'string-equal))
                                   :connected (and (null (key-check-state id))
                                                   (or (not (eq state :none))
                                                       (member-p id connected)
                                                       (member-p id listable)))))
                     rows))
             (pin (row)
               (position (provider row) +connect-pinned-providers+ :key #'car :test #'string-equal))
             (connected-p (row) (member-p (provider row) connected)))
      (when (hash-table-p catalog)
        (loop for provider-id being the hash-keys of catalog using (hash-value provider)
              for id = (and (stringp provider-id)
                            (nlk:trimmed provider-id))
              for listed = (or (member-p id connected) (member-p id listable))
              ;; The count is the models a turn can run on (CATALOG-MODEL-TURN-P),
              ;; the ones /models lists: the row said `OpenAI  52 models' over a
              ;; picker of 42, and a beta tester asked where ten went. A provider
              ;; with none leads nowhere, and is not offered unless it is already
              ;; connected.
              for count = (if listed
                              (models id)
                              (loop for model being the hash-values of
                                      (or (json-value provider :object "models") (make-hash-table))
                                    count (catalog-model-turn-p model)))
              when (and id (plusp (length id)) (hash-table-p provider)
                        (or listed
                            (and (catalog-npm-lane-name (json-value provider :text "npm"))
                                 (plusp count))))
                do (let ((state (if (member-p id connected) (provider-auth-state id) :none)))
                     (row id (or (json-value provider :text "name")
                                 (json-value provider :text "display_name")
                                 id)
                          state
                          ;; The Zig providerModelCountDetail copy, then
                          ;; where a connected provider's credential comes
                          ;; from: `connected (auth.json)', `connected (env)',
                          ;; or `not checked (auth.json)' for a key nothing
                          ;; answered (KEY-CHECK-STATE).
                          (if (eq state :none)
                              (format nil "~d model~:p" count)
                              (format nil "~d model~:p  ~a (~(~a~))" count
                                      (or (key-check-state id) "connected")
                                      (if (eq state :api-key) "auth.json" state)))))))
      ;; Configured and local providers need a row even without a models.dev entry.
      ;; Their models and listing failures still come from the one model catalog.
      (dolist (id (union connected listable :test #'string-equal))
        (unless (find id rows :test #'string-equal :key #'provider)
          (let ((state (if (configured-provider-lane id) (provider-auth-state id) :none)))
            (row id (or (json-value (configured-provider-entry id) :text "display_name")
                        (picker-provider-name id)
                        id)
                 state
                 (format nil "~d model~:p  ~a" (models id)
                         (or (key-check-state id)
                             (if (eq state :none) "configured" "connected")))))))
      ;; A provider a folder not installed here serves (PROVIDER-OFFERS):
      ;; Enter installs the folder first (CONNECT-INSTALL). Its models.dev row,
      ;; when it has one, becomes the folder's -- a key saved from that row
      ;; alone reached a lane that cannot talk to it (OpenCode Go answers 400
      ;; MissingSessionID) -- and one models.dev lacks (ChatGPT's Codex) is
      ;; listed by the name its folder gives it. The row says nothing of the
      ;; install: picking it is the whole of it ("less is more").
      (dolist (offer (picker-provider-offers))
        (let* ((id (getf offer :provider))
               (held (find-if (lambda (row)
                                (and (string-equal id (provider row))
                                     (not (getf (picker-row-value row) :install))))
                              rows)))
          ;; ... save that its cell is still starting, which is why it is
          ;; not among the connected yet
          (cond (held
                 (setf (picker-row-value held) (list* :install offer (picker-row-value held)))
                 (when (getf offer :starting)
                   (setf (picker-row-detail held)
                         (let ((detail (picker-row-detail held)))
                           (format nil "~@[~a  ~]starting" (and (plusp (length detail)) detail))))))
                (t (push (make-picker-row :label (getf offer :name)
                                          :detail (if (getf offer :starting) "starting" "")
                                          :value (list :provider id :install offer))
                         rows)))))
      (let* ((sorted (stable-sort (sort rows #'string-lessp :key #'provider)
                                  (lambda (a b &aux (at (pin a)) (bt (pin b)))
                                    (cond ((and at bt) (< at bt))
                                          (at t)
                                          ((not bt) (string-lessp (picker-row-label a) (picker-row-label b)))))))
             (others (remove-if #'connected-p sorted))
             (pinned (count-if #'pin others)))
        (append
         ;; Without this row, a gateway that does not report its providers
         ;; would leave every saved key reading as gone: a key intact in
         ;; auth.json would be listed with a model count but nothing connected.
         (when (and (not *serving*) (getf *gateway-models* :unreported))
           (list (make-picker-row
                  :label (format nil "Saved keys unknown: the gateway runs an older build~:[~; · `nodecode gateway restart'~]"
                                 (ignore-errors (nlk:gateway-installed-p)))
                  :heading t)))
         ;; Providers with a saved key are listed under Suggested, and Enter on
         ;; one goes on to its models. A Continue row over them, on to every
         ;; connected provider's models at once, would be an extra row the list
         ;; does not need.
         (when connected
           (append (list (make-picker-row :label "Suggested" :heading t))
                   (remove-if-not #'connected-p sorted)
                   (list (make-picker-row :label "Connect another" :heading t))))
         (subseq others 0 pinned)
         ;; A heading between the pinned rows and the rest; without one, 302.AI
         ;; and abliteration.ai would read as picks beside Google.
         (when (< 0 pinned (length others))
           (list (make-picker-row :label "More providers, A to Z" :heading t)))
         (nthcdr pinned others)
         (list (make-picker-row :label "Other provider"
                                :detail "Enter a provider id from models.dev"
                                :value (list :custom t))))))))

(defun gateway-key-check (model provider key &optional vars)
  "PROVIDER-KEY-CHECK of KEY for PROVIDER as MODEL's organism answers it
(POST /api/gateway/keys, check: true), VARS the values of its catalog
address's blanks, as the list (VERDICT WORDS) the /models sink reads: VERDICT
NIL when nothing was asked."
  (let ((answer (gateway-model-http-call model :post "/api/gateway/keys"
                                         (json-object "provider" provider "key" (nlk:trimmed key) "check" t
                                                      :opt "vars" (and vars (alexandria:alist-hash-table
                                                                             vars :test #'equal)))
                                         :read-timeout (+ *provider-models-fetch-timeout-seconds* 15))))
    (list (cdr (assoc (json-value answer :string "verdict")
                      '(("works" . :works) ("refused" . :refused)
                        ("unreachable" . :unreachable) ("unchecked" . :unchecked))
                      :test #'equal))
          (json-value answer :string "words"))))

(defun connect-credential-sink (&optional saved)
  "The /models wizard's ON-SAVE: ask the provider whether it takes the key
(PROVIDER-KEY-CHECK, off the loop), then SAVE-PROVIDER-API-KEY into the
shared auth.json, answering DONE with (MODEL MESSAGE ERROR): ERROR NIL for a
save, :RETRY when nothing listens where the key is checked -- the server's to
start, and the same key is checked again -- else T."
  ;; SAVED, when given, is called with the normalized provider id after a
  ;; successful save — the setup wizard's record of what landed. A refused key
  ;; is not saved: the wizard returns to the key step. A key nothing answered
  ;; (a hang, a 5xx, a 404, or the check itself failing) may still be right:
  ;; it is saved, the message says it was not checked and why, and
  ;; *KEY-CHECKS* keeps it off the list's `connected'.
  ;; A shell asks its organism (GATEWAY-KEY-CHECK): the address a key is
  ;; checked at is the one a turn dials, and a cell that serves the provider
  ;; sets it there only -- OpenCode Go's, checked at models.dev's, answered 404.
  ;; VARS are the ${NAME} blanks of the provider's catalog address the wizard
  ;; asked for before the key: the check dials the address they fill, and
  ;; they are saved with the key.
  (lambda (model provider-id secret done &key vars)
    (off-loop
     model
     (lambda () (if *serving*
                    (multiple-value-list (provider-key-check provider-id (nlk:trimmed secret) :vars vars))
                    (gateway-key-check model provider-id secret vars)))
     ;; VALUE is the check's two values as a list, or OFF-LOOP's failure,
     ;; which is said plainly: its text is a Lisp condition's.
     (lambda (model value)
       (destructuring-bind (&optional verdict words)
           (if (off-loop-failure value) (list :unchecked "the check itself failed") value)
         (case verdict
           (:refused
            (funcall done model (format nil "~a; nothing was saved. Enter pastes another." words) t))
           (:unreachable
            (funcall done model (format nil "~a; nothing was saved. Start the server, then Enter ~
                                             checks again; Esc picks another provider."
                                        words)
                     :retry))
           (t
            (handler-case
                (let* ((id (save-provider-api-key provider-id secret :vars vars))
                       (name (or (json-value (models-catalog-table) :text id "name") id)))
                  (if (eq verdict :unchecked)
                      (setf (gethash id *key-checks*)
                            (list verdict (format nil "~a key was not checked: ~a" name words)))
                      (remhash id *key-checks*))
                  (when saved (funcall saved id))
                  ;; DEVIATION from the Zig materializationSummary copy: the lisp
                  ;; save writes auth.json only (no config.jsonc provider block),
                  ;; and the message says exactly what happened -- `works' only
                  ;; when the provider answered the key.
                  (let ((message (format nil "~a key ~:[saved~;works, saved~] to auth.json~@[, not checked: ~a~]"
                                         name (eq verdict :works) (and (eq verdict :unchecked) words))))
                    ;; A shell's list is its gateway's answer (*GATEWAY-MODELS*),
                    ;; read before this key existed: rebuilt from it, the provider
                    ;; just saved read unconnected and Enter on it asked for the
                    ;; key again. The gateway is read again first, off the loop.
                    (if *serving*
                        (funcall done model message nil)
                        (off-loop model (lambda () (fetch-gateway-models model) t)
                                  (lambda (model value)
                                    (funcall done model
                                             (if-let (failure (off-loop-failure value))
                                               (format nil "~a; the provider list was not read again: ~a"
                                                       message failure)
                                               message)
                                             nil)
                                    nil)))))
              (error (condition)
                (funcall done model
                         (format nil "Could not save the key to ~a (~a); nothing was saved. Enter pastes it again."
                                 (home-abbreviated (uiop:native-namestring (or *auth-file-path* "auth.json")))
                                 (plain-error-text condition))
                         t))))))
       nil))))

(defun open-models-reasoning-picker (model)
  "After /models lands a model, continue through its reasoning-effort view.
The target snapshot is read after the model selection so the ladder and current
rung belong to the new target, just as the setup walk's third stage does."
  ;; Nobody asked for this panel: typing goes past it to the composer
  ;; (PICKER-TYPE-THROUGH), keeping the effort that stands.
  (handler-case
      (progn (open-effort-view model (lambda (model effort) (apply-effort model effort))
                               (lambda (model value) (declare (ignore model value)) nil)
                               :hint "enter continue  esc close  ↑↓ navigate")
             (when-let (dialog (chat-model-dialog model))
               (setf (picker-type-through dialog) t)))
    (error (condition)
      (show-status model (format nil "Reasoning controls unavailable: ~a" condition)
                   :error-p t))))

;;; An Auxiliary pick asks its reasoning effort, as a Main pick asks its own:
;;; without this, a model chosen for side work with no rung chosen would run
;;; at whatever rung the last one had, or its lightest, and nothing would say
;;; which.

(defun open-auxiliary-effort-picker (model)
  "After an Auxiliary pick, the auxiliary model's reasoning-effort view: its
ladder as the gateway answers it (GET auxiliary-model's EFFORTS), the rung it
runs at marked, Default its lightest; a pick lands through the op the model
did. A model that does not reason asks nothing; a read that fails says so in
the composer status."
  (multiple-value-bind (said failed)
      (with-slash-notice ("Reasoning controls are unavailable" :control "~a: ~a")
        (let* ((object (gateway-model-http-call model :get "/api/gateway/auxiliary-model" nil))
               (provider (json-value object :string "provider"))
               (model-id (json-value object :string "model"))
               (ladder (coerce (json-array object "efforts") 'list)))
          (when (and provider ladder)
            (open-think-dialog model provider model-id ladder (json-value object :string "effort")
                               (lambda (model effort)
                                 (slash-auxiliary-model-op model (list provider model-id) :effort effort))
                               :default "The lightest it offers"
                               :subtitle (format nil "~a/~a, the auxiliary model" provider model-id))
            ;; as after a Main pick, typing goes past it (PICKER-TYPE-THROUGH)
            (setf (picker-type-through (chat-model-dialog model)) t))
          nil))
    (when failed (show-status model said :error-p t))))

;;; /models picks two models: Main, the one this session runs on and new
;;; sessions start on, and Auxiliary, the model a session's naming, a recap
;;; or a summary runs on. One list serves both under a strip of two tabs, Tab
;;; between them, and Enter lands its row on the tab in front: a second
;;; command for the auxiliary model was a second place to look, and nothing
;;; in /models said it was there.

(defparameter +models-tabs+
  (flet ((shown (row tab) (member (picker-row-key row) (list "" tab) :test #'string=))
         (own (row tab) (string= (picker-row-key row) tab)))
    (list (list :label "Main" :value "main" :match #'shown)
          (list :label "Auxiliary" :value "aux" :match #'shown)
          (list :label "Decider" :value "decider" :match #'own)
          (list :label "Backup" :value "backup" :match #'own)))
  "/models' strip: Main and Auxiliary show the rows they share, keyed \"\", and
their own; Decider, no chat model, its own alone.")

(defun models-decider-rows (&aux (in-use (decider-model)))
  "The rows only /models' Decider tab shows, keyed `decider': what a decider
answers, then each decider (DECIDER-CHOICES) with its model and where it
answers, and None -- the one in use first, so the tab opens on it."
  (flet ((own (here &rest options)
           (cons here (apply #'make-picker-row :key "decider" options))))
    (let ((choices (append (loop for row in (decider-choices)
                                 for here = (equalp (getf row :id) in-use)
                                 collect (own here :label (getf row :name)
                                              :detail (format nil "~:[~;in use · ~]~a · ~:[~;local · ~]~a"
                                                              here (getf row :model)
                                                              (local-endpoint-p (getf row :base))
                                                              (decider-address (getf row :base)))
                                              :value (list :model (list :decider (getf row :id)))))
                           (list (own (null in-use) :label "None"
                                      :detail (format nil "~:[~;in use · ~]nothing decides" (null in-use))
                                      :value (list :model (list :decider nil)))))))
      (append (list (make-picker-row :key "decider" :label "Judgments as probabilities" :heading t))
              (mapcar #'cdr (stable-sort choices (lambda (a b) (and (car a) (not (car b))))))
              (list (make-picker-row :key "decider" :heading t))))))

(defun decider-address (base)
  "BASE, a decider's base URL, as a row says where it answers: host:port."
  (let ((uri (ignore-errors (quri:uri base))))
    (if (and uri (quri:uri-host uri))
        (format nil "~a~@[:~a~]" (quri:uri-host uri) (quri:uri-port uri))
        base)))

(defun models-local-rows (rows)
  "/models' Local section: ROWS' models whose provider answers from a machine
of the operator's own (PICKER-LOCAL-PROVIDER-IDS), under their heading and
over a blank one; NIL when there are none. Each carries its (PROVIDER
MODEL) pair the way a Frequently used shortcut does."
  (let* ((local (picker-local-provider-ids rows))
         (mine (remove-if-not (lambda (row &aux (value (picker-row-value row)))
                                (and (stringp (first value))
                                     (member (first value) local :test #'string-equal)))
                              rows)))
    (when mine
      (append (list (make-picker-row :label "Local" :heading t))
              (mapcar #'models-root-row mine)
              (list (make-picker-row :heading t))))))

(defun models-own-rows (rows)
  "/models' Nodecode section, the top of its root: nodecode.ai's own models
among ROWS (+OWN-PROVIDER+), each picked the way a Frequently used shortcut
is, under their heading and over a blank one; NIL when ROWS hold none."
  (when-let (own (remove-if-not (lambda (row) (models-provider-row-p row +own-provider+)) rows))
    (append (list (make-picker-row :label "Nodecode" :heading t))
            (mapcar (lambda (row) (if (picker-row-value row) (models-root-row row) row)) own)
            (list (make-picker-row :heading t)))))

(defun models-root-rows (rows history)
  "/models' shared root under its tabs: nodecode.ai's own models
(MODELS-OWN-ROWS), Frequently used from HISTORY, the Local models of ROWS,
then the providers."
  (let* ((providers (connect-picker-rows))
         (root (models-frequent-picker-rows rows history :providers providers))
         (local (models-local-rows rows))
         (at (and local (position (first providers) root :test #'eq)))
         (own (models-own-rows rows))
         (shared (cond ((null at) (append root local))
                       ;; Frequently used closes on its Providers heading: Local goes above it.
                       ((and (plusp at) (picker-row-heading (nth (1- at) root)))
                        (append (subseq root 0 (1- at)) local (subseq root (1- at))))
                       (t (append local (list (make-picker-row :label "Providers" :heading t)) root)))))
    ;; Under the Nodecode section a bare provider list takes a heading of its
    ;; own, or its rows would read as ours.
    (append own
            (when (and own shared (not (picker-row-heading (first shared))))
              (list (make-picker-row :label "Providers" :heading t)))
            shared)))

(defun models-auxiliary-rows (rows)
  "The rows only /models' Auxiliary tab shows, keyed `aux': what the
auxiliary model runs, the one in use -- named as ROWS, the catalog's rows,
name it -- and Same as main, which takes it out. The one in use comes first,
so the tab opens on it."
  (multiple-value-bind (provider id) (shared-model-pin "auxiliary_model")
    (let ((row (and provider (find (list provider id) rows :key #'picker-row-value :test #'equalp))))
      (flet ((own (&rest options) (apply #'make-picker-row :key "aux" options)))
        (append
         ;; what it runs, short enough for the label column at 80 columns
         (list (own :label "Session names, recaps, summaries" :heading t))
         (when provider
           (list (own :label (if row (picker-row-label row) id)
                      :detail (format nil "in use · ~a" provider)
                      :value (list :model (list provider id)))))
         (list (own :label "Same as main"
                    :detail (format nil "~:[~;in use · ~]each session's model" (null provider))
                    :value (list :model :default))
               (own :heading t)))))))

;;; /models' Backup tab is the failover chain every session and room share:
;;; the backups in the order a failing turn tries them, each with what history
;;; says of it, Add, the triggers as rows Enter switches, and Test. The state
;;; is the gateway's (GET /api/gateway/failover), read off the loop for the
;;; dialog that asked and installed only in that identical dialog; a pick
;;; writes through POST and opens the tab again on the answer.

(defvar *models-backup-by-dialog* (make-hash-table :test #'eq :weakness :key)
  "/models dialog -> the failover state its read answered, the route's JSON.")

(defun backup-record-words (entry &aux (at (json-value entry :string "answered_at"))
                                       (tried (or (json-value entry :integer "tried") 0))
                                       (universal (and at (iso-universal at))))
  "What history says of ENTRY, one backup of the route's chain, in words short
enough for a picker's detail column: when it last answered, as how long ago,
and how many chain moves it rescued."
  (format nil "~:[never answered~;answered ~:*~a~]~:[~; · rescued ~d/~d~]"
          (and universal
               (let ((minutes (floor (max 0 (- (get-universal-time) universal)) 60)))
                 (cond ((< minutes 1) "just now")
                       ((< minutes 60) (format nil "~dm ago" minutes))
                       ((< minutes (* 48 60)) (format nil "~dh ago" (floor minutes 60)))
                       (t (format nil "~dd ago" (floor minutes (* 24 60)))))))
          (plusp tried) (or (json-value entry :integer "rescued") 0) tried))

(defun models-backup-rows (state)
  "The rows only /models' Backup tab shows, keyed `backup', over STATE -- the
failover route's answer, NIL while its read is out."
  (flet ((own (&rest options) (apply #'make-picker-row :key "backup" options))
         (act (&rest action) (list :model (list* :backup action))))
    (append
     (list (own :label "Tried in order · enter takes one out" :heading t))
     (cond
       ((null state) (list (own :label "Reading backups…" :detail "")))
       ((off-loop-failure state)
        (list (own :label "Could not read backups" :detail (off-loop-failure state))))
       (t
        (append
         (loop for entry across (or (json-value state :array "chain") #())
               for index from 0
               collect (own :label (format nil "~d. ~a" (1+ index) (json-value entry :string "model"))
                            :detail (format nil "~a · ~a"
                                            (json-value entry :string "provider") (backup-record-words entry))
                            :value (act :remove index)))
         (list (own :label "Add a backup…" :detail "pick a model; it goes last" :value (act :add))
               (own :label "Test backups" :detail "asks each backup once" :value (act :test))
               (own :heading t)
               (own :label "Switch when the model hits" :heading t))
         (loop for trigger across (or (json-value state :array "triggers") #())
               for on = (eq t (gethash "on" trigger))
               collect (own :label (format nil "~:[ ~;✓~] ~a" on (json-value trigger :string "label"))
                            :detail (if on "on · enter turns it off" "off · enter turns it on")
                            :value (act :toggle (json-value trigger :string "id")))))))
     (list (own :heading t)))))

(defun install-backup-state (model dialog value)
  "Install the failover state only in the identical dialog that dispatched the
read: closing, replacing, or reopening the picker discards its old completion."
  (unless (eq dialog model.dialog)
    (return-from install-backup-state :quiet))
  (setf (gethash dialog *models-backup-by-dialog*) value)
  ;; A credential being entered keeps its step's rows and selection.
  (unless (and (eq :model-providers dialog.kind) (eq :provider (connect-dialog-step dialog)))
    (return-from install-backup-state :quiet))
  (restock-models-dialog model dialog (picker-highlighted-value dialog)))

(defun start-models-backup-read (model dialog)
  "Read the failover state off the UI loop, scoped to DIALOG."
  (when model.program
    (off-loop model
              (lambda () (gateway-model-http-call model :get "/api/gateway/failover" nil))
              (lambda (model value) (install-backup-state model dialog value)))))

(defun backup-state-summary (state)
  "The notice a failover write answers: the chain, the triggers on, and a
config that was not saved."
  (let ((chain (coerce (or (json-value state :array "chain") #()) 'list))
        (on (loop for trigger across (or (json-value state :array "triggers") #())
                  when (eq t (gethash "on" trigger)) collect (json-value trigger :string "label"))))
    (format nil "backups: ~:[none~;~:*~{~a~^, ~}~] · switch on: ~:[nothing~;~:*~{~a~^, ~}~]~@[~%shared config: NOT saved — ~a~]"
            (loop for entry in chain
                  collect (format nil "~a/~a" (json-value entry :string "provider") (json-value entry :string "model")))
            on (shared-config-failure state))))

(defun models-backup-write (model body)
  "POST BODY to the failover route: (values NOTICE FAILED), and the state it answered."
  (let ((state nil))
    (multiple-value-bind (notice failed)
        (with-slash-notice ("backup models")
          (setf state (gateway-model-http-call model :post "/api/gateway/failover" body))
          (backup-state-summary state))
      (values notice failed state))))

(defun reopen-models-backup-tab (model state)
  "Open /models on its Backup tab again, STATE in it already, unless another
dialog took the screen meanwhile."
  (unless model.dialog
    (open-models-provider-picker model :tab "backup")
    (when (and state model.dialog)
      (install-backup-state model model.dialog state))))

(defun models-backup-act (model dialog action &aux (state (gethash dialog *models-backup-by-dialog*)))
  "Run ACTION, a Backup tab row's -- (:REMOVE INDEX), (:ADD), (:TOGGLE ID) or
(:TEST) -- over STATE, the failover state DIALOG was showing."
  (flet ((pairs (chain) (map 'list (lambda (entry) (json-object "provider" (json-value entry :string "provider")
                                                              "model" (json-value entry :string "model")))
                             chain))
         (post-state (body)
           (multiple-value-bind (notice failed answer) (models-backup-write model body)
             (show-status model notice :error-p failed)
             (reopen-models-backup-tab model (and (not failed) answer)))))
    (let ((chain (coerce (or (and (hash-table-p state) (json-value state :array "chain")) #()) 'list))
          (triggers (coerce (or (and (hash-table-p state) (json-value state :array "triggers")) #()) 'list)))
      (ecase (first action)
        (:remove
         (post-state (json-object "chain" (coerce (pairs (append (subseq chain 0 (second action))
                                                            (nthcdr (1+ (second action)) chain)))
                                             'vector))))
        (:toggle
         (post-state (json-object
                 "on" (coerce (loop for trigger in triggers
                                    for id = (json-value trigger :string "id")
                                    for on = (eq t (gethash "on" trigger))
                                    when (if (equal id (second action)) (not on) on) collect id)
                              'vector))))
        (:add
         (open-models-picker
          model
          :lands (lambda (model tokens)
                   (models-backup-write
                    model (json-object "chain" (coerce (append (pairs chain)
                                                               (list (json-object "provider" (first tokens)
                                                                                  "model" (second tokens))))
                                                       'vector))))
          :then (lambda-ignoring (model picked)
                  (reopen-models-backup-tab model nil))))
        (:test
         (show-status model "Testing backups…")
         (off-loop model
                   (lambda () (gateway-model-http-call model :post "/api/gateway/failover/test" (json-object)))
                   (lambda (model value)
                     (if (off-loop-failure value)
                         (show-status model (format nil "Couldn't test backups: ~a" (off-loop-failure value)) :error-p t)
                         (show-status model
                                      (format nil "backups: ~:[none to test~;~:*~{~a~^ · ~}~]"
                                              (loop for result across (or (json-value value :array "results") #())
                                                    collect (format nil "~a ~:[✗ ~a~;✓~*~] ~,1f s"
                                                                    (json-value result :string "model")
                                                                    (eq t (gethash "ok" result))
                                                                    (json-value result :string "reason")
                                                                    (or (gethash "seconds" result) 0)))))))))))))

(defun open-models-provider-picker (model &key (tab "main"))
  "Open /models on providers, with retained, model-specific shortcuts and
the local models above them, on TAB: main, aux or decider (+MODELS-TABS+)."
  (let ((dialog nil))
    (open-connect-dialog
     model
     (lambda (&aux (rows (models-catalog-picker-rows)))
       (append (models-auxiliary-rows rows)
               (models-decider-rows)
               (models-backup-rows (gethash dialog *models-backup-by-dialog*))
               (models-root-rows rows (gethash dialog *models-frequent-by-dialog* #()))))
     (connect-credential-sink)
     (lambda (model &optional choice &aux (aux (equal (picker-dialog-tab-value dialog) "aux")))
       (cond
         ;; A Backup row carries (:BACKUP ACTION ...).
         ((and (consp choice) (eq (first choice) :backup))
          (models-backup-act model dialog (rest choice)))
         ;; The Nodecode section's row of a home not signed in carries (:LOGIN).
         ((equal choice '(:login)) (slash-login-op model ""))
         ;; A Decider row carries (:DECIDER ID), None's ID NIL.
         ((and (consp choice) (eq (first choice) :decider))
          (multiple-value-bind (notice failed)
              (slash-decider-model-op model (list (or (second choice) "none")))
            (show-status model notice :error-p failed)))
         ;; A shortcut row carries the original (PROVIDER MODEL) pair; Same as
         ;; main carries :DEFAULT, and takes the auxiliary model out.
         ((or (eq choice :default) (and (consp choice) (stringp (first choice))))
          (multiple-value-bind (notice failed)
              (if aux
                  (slash-auxiliary-model-op model (if (eq choice :default) '("default") choice))
                  (slash-models-op model choice))
            (show-status model notice :error-p failed)
            (unless (or failed (eq choice :default))
              (if aux (open-auxiliary-effort-picker model) (open-models-reasoning-picker model)))))
         ;; A provider row hands back only its provider id.
         (choice
          (open-models-picker
           model :provider choice :lands (if aux :auxiliary :session)
           :then (lambda (model picked)
                   (cond ((null picked) (open-models-provider-picker model :tab (if aux "aux" "main")))
                         (aux (open-auxiliary-effort-picker model))
                         (t (open-models-reasoning-picker model))))))
         ;; Esc from the provider list closes the root.
         (t nil)))
     :title "Models" :kind :model-providers)
    (setf dialog (search-models-from-root (chat-model-dialog model)))
    (register-picker-tabs dialog +models-tabs+
                          :active (position tab +models-tabs+ :key (lambda (entry) (getf entry :value))
                                                              :test #'equal))
    (picker-seat-top dialog)
    (start-models-frequent-read model dialog)
    (start-models-backup-read model dialog)
    (start-provider-models-refresh model))
  (values nil nil))

