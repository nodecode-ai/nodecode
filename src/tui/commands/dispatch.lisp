;;;; dispatch.lisp --- slash-command execution, the models.dev catalog fetch, /context gathering. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- slash-command execution ----------------------------------------------
;;; One execution authority (slash.lisp owns the catalog and parser; ui.lisp
;;; owns the dropdown and the submit seam). Every shell is a GATEWAY-CHAT-MODEL
;;; -- the owner's own included -- so the gateway-backed commands go through
;;; the HTTP control ops, and the durable facts and live fanout happen where
;;; the store's owner runs. Every op returns notice text instead of
;;; signalling: a slash command's failure is chrome, never a crashed shell.

(defmacro with-slash-notice ((label &key (control "~a failed: ~a")) &body body)
  "BODY, with any error answered as notice text instead of signalling — the
one place that rule above is implemented — and T as a second value then, so a
caller that must tell a refusal from an answer need not read the words."
  ;; CONTROL is the FORMAT control the refusal is built with and LABEL its
  ;; first argument, the condition its second, so a lane whose copy is not the
  ;; default "<verb> failed: <why>" shape keeps its own Zig-parity wording at
  ;; the call site rather than losing it to the macro.
  (let ((reason (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (error (,reason) (values (format nil ,control ,label ,reason) t)))))

(defun slash-themes-op (model args)
  "Open the theme picker (no ARGS) or engage ARGS as a theme name; the
choice persists across restarts."
  ;; Cosmetic op: resolution of a named theme stays strict inside
  ;; ENGAGE-THEME; the failure reports here rather than signalling.
  (with-slash-notice ("theme switch")
      (if (zerop (length args))
          (let* ((catalog (theme-catalog))
                 (current (saved-theme-selection))
                 ;; The Zig ThemePreviewSession original_theme capture: Esc
                 ;; restores exactly the palette the dialog opened over,
                 ;; however many rows were previewed in between.
                 (original-palette *palette*))
            (open-picker-dialog
             model "Themes"
             (mapcar (lambda (name)
                       (make-picker-row
                        :label name
                        ;; The Zig renderThemesDockedRow marker: the
                        ;; engaged theme names itself in the row, not
                        ;; only in the header.
                        :detail (if (equal name current) "current" "")
                        :value name))
                     catalog)
             #'slash-themes-op
             ;; Opening lands on the engaged theme (the Zig
             ;; themeDialogSelectionForCurrentTheme), so the open-time
             ;; highlight previews a no-op instead of the first row.
             :selection (or (position (or current (configured-theme-name)) catalog :test #'equal) 0)
             ;; Live hotload (the Zig previewSelectedThemesDialogItem):
             ;; every highlight move — keys, search edits, hover — engages
             ;; the row's theme without persisting. An unresolvable name
             ;; keeps the current palette; preview is cosmetic.
             :on-highlight (lambda-ignoring (model row)
                             (ignore-errors
                               (engage-theme
                                :name (picker-row-value row))))
             :on-cancel (lambda-ignoring (model)
                          (setf *palette* original-palette))
             :searchable t
             :context (if current (format nil "Current ~a" current) "")
             :search-placeholder "Search themes"
             :empty-label "No themes found"
             :hint "enter select  esc close  ↑↓ navigate  type search"))
          (progn (engage-theme :name args)
                 (save-theme-selection args)
                 (show-status model (format nil "Changed to ~a theme" args))))
      nil))

(defun slash-usage (name)
  (format nil "usage: ~a" (slash-command-usage (find-slash-command name))))

(defun auxiliary-model-summary (provider model effort)
  "One line naming the auxiliary model, or what stands in for it, in the
words /models' Auxiliary tab says it in."
  (if provider
      (format nil "auxiliary model: ~a/~a~@[ (~a)~]" provider model effort)
      "auxiliary model: same as main — naming, recaps and summaries run on each session's own model at its lightest effort"))

(defun decider-model-summary (provider model)
  "One line naming the decider general.decider_model picks, or that none is."
  (if provider
      (format nil "decider model: ~a/~a" provider model)
      "decider model: none — /models decider picks one"))

(defun auxiliary-target-summary ()
  "AUXILIARY-MODEL-SUMMARY of this image's auxiliary target as it stands."
  (multiple-value-bind (provider model-id effort configured) (auxiliary-model)
    (auxiliary-model-summary (and configured provider) model-id effort)))

(defun model-selection-summary ()
  "This image's selection the way a /models report opens: provider, model,
endpoint -- or why it has none -- then the auxiliary target."
  (format nil "provider: ~a~%model: ~a~%endpoint: ~a~%~a"
          *provider* *model*
          (handler-case (effective-endpoint)
            (provider-config-error (condition) (format nil "none: ~a" (provider-error-detail condition))))
          (auxiliary-target-summary)))

(defvar *serving* nil
  "T in the process SERVE-ORGANISM-CANDIDATE serves from: the one a stop's
deadline (SERVE-NEXT) and SIGTERM may end.")

(defvar *gateway-models* nil
  "The gateway's GET /api/gateway/models answer as this shell holds it:
(:rows PICKER-ROWS :connected IDS :listable IDS :refusals ((ID . WHY) ...)).
The gateway is the one
authority -- its image runs the cells whose lanes and inventory hooks add
models -- so a shell never builds the list itself.")

(defun gateway-models-facts (body)
  "BODY, a GET /api/gateway/models answer, as *GATEWAY-MODELS* holds it;
:UNREPORTED when the answer carries no providers field at all."
  (flet ((ids (key &optional pairs)
           ;; The ids whose provider holds KEY; with PAIRS, (ID . VALUE) where VALUE is a string.
           (loop for provider across (or (gethash "providers" body) #())
                 for value = (gethash key provider)
                 when (if pairs (stringp value) value)
                   collect (if pairs (cons (gethash "id" provider) value) (gethash "id" provider)))))
    (list :rows (build-models-catalog-picker-rows
                 (append
                  (loop for row across (or (gethash "models" body) #())
                        collect (list :provider (gethash "provider" row)
                                      :model (gethash "model" row)
                                      :name (gethash "name" row)
                                      :ctx (gethash "context" row)))
                  (loop for row across (or (gethash "unlisted" body) #())
                        collect (list :provider (gethash "provider" row)
                                      :placeholder (gethash "reason" row)))))
          :connected (ids "connected")
          :listable (ids "listable")
          :local (ids "local")
          ;; (ID . NAME) for each provider its folder names (OFFERED-PROVIDER-NAME)
          :names (ids "name" t)
          ;; (ID . WHY) for each provider no turn can run on there, WHY the
          ;; gateway's words (PROVIDER-TURN-REFUSAL reads them).
          :refusals (ids "refusal" t)
          ;; Each provider a folder not installed there serves, which picking
          ;; installs (PROVIDER-OFFERS' plists).
          :offers (loop for offer across (or (gethash "offers" body) #())
                        collect (append
                                 (list :cell (gethash "cell" offer) :commit (gethash "commit" offer)
                                       :provider (gethash "provider" offer) :name (gethash "name" offer)
                                       :sign-in (or (gethash "sign_in" offer) "key")
                                       :description (gethash "description" offer))
                                 ;; in already, its provider not answering yet,
                                 ;; and whether its cell is still to start
                                 (and (eq t (gethash "installed" offer)) (list :installed t))
                                 (and (eq t (gethash "starting" offer)) (list :starting t))))
          ;; A gateway older than the providers field sends none: what it
          ;; has keys for is unknown here, not none.
          :unreported (not (nth-value 1 (gethash "providers" body))))))

(defun provider-turn-refusal (provider)
  "Why no turn can run on PROVIDER, in the words that say what to do, or
NIL. The serving image answers from its own lanes (SELECTION-REFUSAL). A
shell runs no cell, so a lane a cell registers exists in the gateway's image
only: a shell takes the gateway's answer (*GATEWAY-MODELS*' :REFUSALS) and
says nothing before that lands."
  ;; A shell asking its own lanes refused every pin a cell serves -- `provider
  ;; claude-code is not one this box knows' on the landing screen and on the
  ;; walk's step line, while every turn on it ran (the operator, 2026-10-22).
  (if *serving*
      (ignore-errors (selection-refusal provider))
      (cdr (assoc provider (getf *gateway-models* :refusals) :test #'string-equal))))

(defun fetch-gateway-models (model)
  "Read MODEL's gateway's model list into *GATEWAY-MODELS*; => the facts."
  (setf *gateway-models*
        (gateway-models-facts
         (gateway-model-http-call model :get "/api/gateway/models" nil))))

(defun gateway-models-landed (model value)
  "The loop's half of a /models list read (START-MODELS-PICKER-WARM): an
open picker restocks, and the landing screen, which says the gateway's
verdict on the pin (PROVIDER-TURN-REFUSAL), is laid out again."
  ;; The facts' read lands beside this one and repaints on its own: without
  ;; this, a list landing after it left the previous list's verdict painted --
  ;; a cold launch's `provider claude-code is not one this box knows', read
  ;; before the cells started, stood after they had (the operator, 2026-10-22).
  (cond ((off-loop-failure value) :quiet)
        ((home-screen-p model)
         (install-refreshed-models model)
         (render-state-at-paint model)
         nil)
        (t (install-refreshed-models model))))

(defun start-models-picker-warm (model)
  "Read the gateway's /models list off the loop, so the first press of
/models paints from it; an open picker restocks when it lands."
  (when (and model.program model.http-port)
    (off-loop model (lambda () (fetch-gateway-models model)) #'gateway-models-landed)))

(defun models-catalog-picker-rows ()
  "The /models picker's rows. The serving image -- the organism, whose cells
run -- builds them from MODEL-CHOICES; a shell reads them from it
(*GATEWAY-MODELS*), NIL before the first read lands."
  (if *serving*
      (build-models-catalog-picker-rows (model-choices))
      (getf *gateway-models* :rows)))

(defun picker-connected-provider-ids ()
  "CONNECTED-PROVIDER-IDS as the serving image answers it."
  (if *serving* (connected-provider-ids) (getf *gateway-models* :connected)))

(defun picker-listable-provider-ids ()
  "LISTABLE-PROVIDER-IDS as the serving image answers it."
  (if *serving* (listable-provider-ids) (getf *gateway-models* :listable)))

(defun picker-provider-offers ()
  "PROVIDER-OFFERS as the serving image answers it."
  (if *serving* (ignore-errors (provider-offers)) (getf *gateway-models* :offers)))

(defun picker-provider-name (id)
  "OFFERED-PROVIDER-NAME as the serving image answers it."
  (if *serving*
      (ignore-errors (offered-provider-name id))
      (cdr (assoc id (getf *gateway-models* :names) :test #'string-equal))))

(defun install-offered-cell (model offer)
  "Install OFFER's folder (PROVIDER-OFFERS) through MODEL's organism at the
pin the picker read, then read its models and its commands again, the
folder's own among them: => (:COMMANDS ENTRIES), or (:REFUSED WHY)."
  ;; Off the loop: a hub install clones before it loads.
  (handler-case
      (progn (gateway-model-http-call model :post "/api/gateway/cells"
                                      (json-object "name" (getf offer :cell) "on" t
                                                   :opt "commit" (getf offer :commit)
                                                   "seconds" nlk:*cell-switch-seconds*)
                                      :read-timeout nlk:*cell-switch-seconds*)
             ;; the folder is in: a reread that fails leaves the lists in hand
             (ignore-errors (fetch-gateway-models model))
             (list :commands (ignore-errors (gateway-command-entries model))))
    (gateway-http-error (refusal) (list :refused (gateway-http-error-detail refusal)))))

(defun picker-local-provider-ids (rows)
  "The providers of ROWS, /models' model rows, served from a machine of the
operator's own (LOCAL-ENDPOINT-P) as the serving image answers it."
  (if *serving*
      (loop with ids = '()
            for row in rows
            for provider = (first (picker-row-value row))
            when (and (stringp provider) (local-endpoint-p (provider-base-url provider)))
              do (pushnew provider ids :test #'string-equal)
            finally (return ids))
      (getf *gateway-models* :local)))

(defun newer-version-p (a b &aux (i 0) (j 0))
  "Whether the model id A lists above B: where they first differ, a run of
digits lists the higher number first (z-10 above z-2, opus-4-6 above
opus-4-5), any other character in alphabetical order, case-insensitively;
an id lists above its own prefix (gpt-5.1 above gpt-5)."
  (loop
    (cond ((>= j (length b)) (return (< i (length a))))
          ((>= i (length a)) (return nil)))
    (let ((x (char a i)) (y (char b j)))
      (if (and (digit-char-p x) (digit-char-p y))
          (let* ((end-a (or (position-if-not #'digit-char-p a :start i) (length a)))
                 (end-b (or (position-if-not #'digit-char-p b :start j) (length b)))
                 (m (parse-integer a :start i :end end-a))
                 (n (parse-integer b :start j :end end-b)))
            (unless (= m n) (return (> m n)))
            (setf i end-a j end-b))
          (progn (unless (char-equal x y) (return (char-lessp x y)))
                 (incf i) (incf j))))))

(defun model-bare-id (id &aux (id (string-downcase id)))
  "ID as models.dev keys the model under any provider: lowercase, past its
last `/' (a router's vendor prefix) and short of its first `:' (a variant
such as `:batch' or `:free')."
  (let ((start (1+ (or (position #\/ id :from-end t) -1))))
    (subseq id start (or (position #\: id :start start) (length id)))))

(defvar *model-release-index* nil
  "(CATALOG . TABLE): per MODEL-BARE-ID, the earliest release date the parsed
models.dev CATALOG gives that model under any provider; rebuilt when the
catalog is reparsed.")

(defun model-release-date (catalog provider model-id)
  "When MODEL-ID was released, as models.dev dates it: its own entry under
PROVIDER, else the earliest date any provider gives its bare id -- a
router's listing (cline, zen) carries no dates of its own -- else NIL."
  (flet ((models-of (provider)
           (let ((entry (and (hash-table-p catalog) (gethash provider catalog))))
             (and (hash-table-p entry) (hash-table-p (gethash "models" entry))
                  (gethash "models" entry)))))
    (let ((own (gethash model-id (or (models-of provider) (make-hash-table)))))
      (or (and (typep own 'catalog-model) (catalog-model-release-date own))
          (let ((index *model-release-index*))
            (unless (and index (eq (car index) catalog))
              (let ((table (make-hash-table :test #'equal)))
                (when (hash-table-p catalog)
                  (loop for id being the hash-keys of catalog
                        do (loop for model being the hash-keys of (or (models-of id) (make-hash-table))
                                   using (hash-value entry)
                                 for date = (and (typep entry 'catalog-model)
                                                 (catalog-model-release-date entry))
                                 for bare = (and date (model-bare-id model))
                                 when (and date (or (null (gethash bare table))
                                                    (string< date (gethash bare table))))
                                   do (setf (gethash bare table) date))))
                (setf index (setf *model-release-index* (cons catalog table)))))
            (values (gethash (model-bare-id model-id) (cdr index))))))))

(defun newer-model-p (a b)
  "Whether the model A, as (DATE BARE-ID PROVIDER MODEL-ID), lists above B:
the later release date first, a dated model above an undated one; then by
the bare id (NEWER-VERSION-P), the provider, the id."
  (destructuring-bind (date-a bare-a provider-a id-a) a
    (destructuring-bind (date-b bare-b provider-b id-b) b
      (cond ((not (equal date-a date-b))
             (or (null date-b) (and date-a (string> date-a date-b))))
            ((string-not-equal bare-a bare-b) (newer-version-p bare-a bare-b))
            ((string-not-equal provider-a provider-b) (string-lessp provider-a provider-b))
            (t (newer-version-p id-a id-b))))))

(defparameter +own-provider+ "nodecode"
  "The provider id of nodecode.ai's own API, the one `nodecode login' wires:
its models lead every model list, and /models pins them (MODELS-OWN-ROWS).")

(defun model-choices ()
  "The models a turn can run on: the models.dev cache, the configured
provider models, and each configured provider's fetched listing
(PROVIDER-MODELS-CACHE-ENTRY), merged and — when any provider is connected (CONNECTED-PROVIDER-IDS) —
retained to the connected set (the Zig gateway_models_catalog pipeline:
mergeConfiguredProviderModels fills a known row's missing context budget and
appends unknown models; retainConfiguredProviders drops unconfigured lanes),
nodecode.ai's own (+OWN-PROVIDER+) first, then newest first (NEWER-MODEL-P)."
  ;; => plists (:provider :model :name :ctx), newest first, then one
  ;; (:provider :placeholder REASON) per provider whose listing failed.
  ;;
  ;; A provider whose listing failed, and whose last answer kept no fetched
  ;; row, trails one placeholder row — `provider/` with the failure as its
  ;; detail — beside whatever the config still types by hand, so the operator
  ;; sees why the listing is missing: an entry naming no sdk cannot be fetched
  ;; at all, and its hand-typed rows alone hid that (one model shown where the
  ;; listing carries ninety, and no reason). There is no unselectable row: its
  ;; value is NIL, and Enter on it reaches SLASH-MODELS-OP with no tokens,
  ;; which just reports the active selection.
  ;;
  ;; NIL when no source yields a row.
  (let ((configured-models (nth-value 1 (configured-provider-inventory))))
    (let* ((catalog (models-catalog-table))
           (listable (listable-provider-ids))
           (retained (union (connected-provider-ids) listable :test #'string-equal))
           ;; One place per (provider, model) pair, found by hash: the fold
           ;; over ~2,000 catalog rows used to ask every row built so far, and
           ;; that quadratic scan is what a rebuild paid.
           (by-pair (make-hash-table :test #'equal))
           (rows '())
           (placeholders '()))
      (flet ((fold (provider model-id display ctx)
               "Fold one (PROVIDER MODEL-ID) in: a known row only gains a
missing context budget and a missing name; an unknown pair joins ROWS.
DISPLAY is the model name when nonblank; provider names never fill it."
               (let* ((key (list (string-downcase provider) (string-downcase model-id)))
                      (name (and (stringp display) (plusp (length (nlk:trimmed display))) display))
                      (hit (gethash key by-pair)))
                 (if hit
                     (setf (getf hit :ctx) (or (getf hit :ctx) ctx)
                           (getf hit :name) (or (getf hit :name) name))
                     (setf (gethash key by-pair)
                           (car (push (list :provider provider :model model-id
                                            :name name :ctx ctx)
                                      rows)))))))
        ;; The models.dev cache's raw (:provider :model :name :ctx) rows first,
        ;; each a model a turn can run on (CATALOG-MODEL-TURN-P).
        (when (hash-table-p catalog)
          (loop for provider-id being the hash-keys of catalog using (hash-value provider)
                for models = (json-value provider :object "models")
                ;; A provider no lane drives lists nothing from the catalog: a
                ;; pick of one is refused at the gateway.
                when (and models (or (null retained)
                                     (member provider-id retained :test #'string-equal))
                          (configured-provider-lane provider-id))
                  do (loop for model-id being the hash-keys of models using (hash-value entry)
                           when (catalog-model-turn-p entry)
                             do (fold provider-id model-id
                                      (catalog-model-name entry)
                                      (let ((value (catalog-model-context entry)))
                                        (and value (plusp value) value))))))
        (dolist (configured configured-models)
          (fold
           ;; Configured rows carry a codename, not a model display name.
           (getf configured :provider-id)
           (getf configured :model-id)
           nil
           (getf configured :context-window)))
        (dolist (provider listable)
          (when-let (entry (provider-models-cache-entry provider))
            (dolist (model (getf entry :models))
              ;; A listing that names a model's kind offers a turn its chat
              ;; models alone: nodecode.ai's image model answers no turn.
              (when (let ((kind (getf model :kind))) (or (null kind) (string-equal kind "chat")))
                (fold provider (getf model :id)
                      (getf model :display)
                      (getf model :context-window))))
            (when (and (getf entry :error) (null (getf entry :models)))
              (push (list :provider provider :placeholder (getf entry :error))
                    placeholders))))
        ;; A row no source above named or sized -- a relay's bare id
        ;; (claude-code, cline) that its own namespace never carries -- takes
        ;; the name most catalog namespaces give that id (the first in
        ;; provider-id order on a tie: the resellers' consensus, as
        ;; MODEL-PRICE reads it) and the window a turn on it resolves
        ;; (MODEL-CONTEXT-TOKENS, the one capability seam, its cell hooks
        ;; included).
        (dolist (row rows)
          (let ((provider (getf row :provider)) (model (getf row :model)))
            (unless (getf row :name)
              (setf (getf row :name)
                    (let ((names (loop for (nil . entry) in (models-catalog-rows model)
                                       for name = (catalog-model-name entry)
                                       when (and (stringp name) (plusp (length (nlk:trimmed name))))
                                         collect name)))
                      (first (stable-sort (copy-list (remove-duplicates names :test #'equal :from-end t))
                                          #'> :key (lambda (name) (count name names :test #'equal)))))))
            (unless (getf row :ctx)
              (setf (getf row :ctx)
                    (let ((ctx (model-context-tokens model provider)))
                      (and (realp ctx) (plusp ctx) ctx)))))))
      (append
       ;; Every list a model is picked from reads this order, its search
       ;; boxes included: their ranking is stable over it. nodecode.ai's own
       ;; models lead it.
       (let ((newest (mapcar #'cdr
                             (sort (loop for row in rows
                                         collect (cons (list (model-release-date catalog (getf row :provider)
                                                                                 (getf row :model))
                                                             (model-bare-id (getf row :model))
                                                             (getf row :provider) (getf row :model))
                                                       row))
                                   #'newer-model-p :key #'car))))
         (flet ((own-p (row) (string-equal (getf row :provider) +own-provider+)))
           (append (remove-if-not #'own-p newest) (remove-if #'own-p newest))))
       ;; Placeholders trail the list, in provider order.
       (sort placeholders #'string-lessp
             :key (lambda (row) (getf row :provider)))))))

(defun build-models-catalog-picker-rows (choices)
  "CHOICES, rows shaped as MODEL-CHOICES answers them, as /models picker
rows; NIL when there are none, and the caller falls back to the textual
selection summary."
  ;; The row's value is the (PROVIDER MODEL) token pair, so Enter routes the
  ;; lane too.
  (mapcar (lambda (row)
            (if (getf row :placeholder)
                (make-picker-row
                 :label (format nil "~a/" (getf row :provider))
                 :detail (format nil "no listing (~a)" (getf row :placeholder))
                 :value nil)
                ;; The model name leads (codename when absent); the provider
                ;; follows in the shared picker's two-space detail column.
                ;; nodecode.ai's own models read as the pair a request names,
                ;; nodecode/<model>, in their own tone on every list.
                (make-picker-row
                 :label (if (string-equal (getf row :provider) +own-provider+)
                            (format nil "~a/~a" (getf row :provider) (getf row :model))
                            (or (getf row :name) (getf row :model)))
                 :tone (and (string-equal (getf row :provider) +own-provider+) :own-model)
                 :detail (getf row :provider)
                 :suffix (if (getf row :ctx)
                             (format nil "  • ctx ~a" (format-token-count (getf row :ctx)))
                             "")
                 :value (list (getf row :provider) (getf row :model)))))
          choices))

(defun models-picker-title (count refreshing-p &key auxiliary)
  "The Zig models header counts the catalog inside the tab label itself
(appendModelsDockedTab); a live fetch says so after it."
  ;; The Auxiliary tab's list names itself in the title, so the operator knows
  ;; which model Enter moves.
  (format nil "~:[Models~;Auxiliary model~] (~d)~:[~; · fetching~]"
          auxiliary count refreshing-p))

(defun start-provider-models-refresh (model)
  "Have the gateway refetch every stale or unlisted provider, on a background
thread, then rebuild an open /models picker from what it landed."
  ;; => T when a thread started. Nothing
  ;; starts without a live program (tests drive the install directly and must
  ;; not spawn threads), without a cache path, with nothing stale, or while a
  ;; refresh is already in flight — the one that lands repaints for both.
  ;;
  ;; The gateway fetches, never this shell: its image is the one whose cells
  ;; are started, so their hooks on a listing run.
  ;; The file it wrote moved under this image's memo, which is dropped here —
  ;; without dropping it, a rewrite of the same size in the same second would
  ;; keep the memo's identity.
  (let ((program model.program)
        (dialog model.dialog)
        (stale (and *provider-models-cache-path* (stale-listable-provider-ids))))
    ;; Every open rereads the gateway's list (its cells add models this
    ;; shell never sees); => T only when a stale listing is being fetched.
    (when (and program
               (let ((thread *provider-models-refresh-thread*))
                 (or (null thread) (not (bt2:thread-alive-p thread)))))
      (setf *provider-models-refresh-thread*
            (off-loop model
                      (lambda ()
                        (with-cleanup ((setf *provider-models-memo* nil
                                             *provider-models-refresh-thread* nil))
                          (when stale
                            (ignore-errors
                              (gateway-model-http-call
                               model :post "/api/gateway/provider-models/refresh"
                               (json-object "ids" (coerce stale 'vector)))))
                          (ignore-errors (fetch-gateway-models model))))
                      (lambda (model value)
                        (if (eq dialog model.dialog)
                            (install-refreshed-models model value)
                            :quiet))))
      (and stale t))))

;;; --- the models.dev catalog fetch ----------------------------------------------
;;; provider.lisp owns the models.dev cache the /models list is built from:
;;; MODELS-CATALOG-TABLE refetches an absent or stale file behind itself. A
;;; fresh box has none, so a connect list would hold the Other-provider entry
;;; alone until the fetch lands. The shell's half is only the wake: the open
;;; connect dialog takes the rows the moment they arrive.

(defun start-models-catalog-fetch (model &aux (program model.program))
  "On a box with no catalog, start REFRESH-MODELS-CATALOG and take its rows
into an open /models list when the fetch lands."
  ;; => T when a waiter started. Nothing starts without a live program
  ;; (tests drive the install directly), with a catalog already there (a stale
  ;; one refreshes behind its reader, and the list keeps the rows it opened
  ;; on), or with no fetch to wait on.
  (when (and program (null (models-catalog-table)))
    (when-let (fetch (refresh-models-catalog))
      (off-loop model (lambda () (bt2:join-thread fetch)) #'install-fetched-catalog)
      t)))

(defun install-fetched-catalog (model value &aux (dialog model.dialog))
  "An open /models list at its provider step takes the fetched catalog's
rows; a failed fetch is the step's own message, and a typed id needs the
catalog too (PROVIDER-ID-REFUSAL)."
  ;; VALUE is what the fetch thread answered — NIL, or why it failed — or the
  ;; OFF-LOOP failure when the join itself signalled.
  ;; The list searches, so its header row is the search row: the failure rides
  ;; the prompt, and names the keys saved here that wait on the list for an
  ;; endpoint. Written as the subtitle, it never showed, and an offline box
  ;; with a deepseek key read an empty list under `Search providers' (xm-204).
  (when (and (connect-dialog-p dialog)
             (eq (connect-dialog-step dialog) :provider))
    (let ((failure (or (off-loop-failure value) value))
          (waiting (sort (set-difference (configured-provider-inventory) (picker-connected-provider-ids)
                                         :test #'string-equal)
                         #'string<)))
      (unless failure
        (setf dialog.rows (funcall (connect-dialog-provider-rows dialog)))
        (picker-seat-top dialog))
      ;; A walk that wrote its own prompt over the loading one keeps it. The
      ;; way on comes before the row runs out at 80 columns.
      (when (or (member dialog.search-placeholder (list +loading-providers+ "Search providers" +search-models-root+)
                        :test #'equal)
                (uiop:string-prefix-p "no models.dev list" dialog.search-placeholder))
        (setf dialog.search-placeholder
              (cond (failure
                     (format nil "no models.dev list (~a) · ~:[reopen this list once online~;~:*~{~a~^ and ~} ~
                                  need~:[s~;~] it · reopen online~]"
                             (first-line failure) waiting (rest waiting)))
                    ((eq dialog.kind :model-providers) +search-models-root+)
                    (t "Search providers")))))
    (when model.band
      (render-state model))))

(defun models (&optional provider)
  "The catalog the shell's /models picker shows, as an answer instead of a
dialog."
  ;; Bare => an alist of (PROVIDER . MODEL-COUNT) over every provider the
  ;; picker would list. With PROVIDER => (values IDS LISTING-ERROR): that
  ;; provider's model ids and, when its fetched listing failed, why — the
  ;; configured models still count. Stale or absent fetched listings are
  ;; refreshed first (workers joined), so the answer is the picker's, not a
  ;; cold cache's. An unconfigured PROVIDER refuses, naming the configured
  ;; set.
  (check-type provider (or null provider-id))
  (let ((known (sort (copy-list (union (connected-provider-ids)
                                       (listable-provider-ids)
                                       :test #'string-equal))
                     #'string-lessp)))
    (when (and provider (not (member provider known :test #'string-equal)))
      (error "no provider ~s is configured~@[ (configured: ~{~a~^, ~})~]"
             provider known))
    (when-let (stale (intersection (if provider (list provider) known)
                                   (stale-listable-provider-ids)
                                   :test #'string-equal))
      (refresh-provider-models stale))
    (let ((rows (models-catalog-picker-rows)))
      (flet ((ids-for (id)
               (loop for row in rows
                     for value = (picker-row-value row)
                     when (and value (string-equal (first value) id))
                       collect (second value))))
        (if provider
            (values (ids-for provider)
                    (getf (provider-models-cache-entry provider) :error))
            (loop for id in known
                  collect (cons id (length (ids-for id)))))))))

(defvar *models-frequent-by-dialog* (make-hash-table :test #'eq :weakness :key)
  "Live-only ranking for the dialog that read it; retained usage owns history.")

(defparameter +frequently-used+ "Frequently used"
  "The heading over a models list's shortcuts from retained history.")

(defun models-frequent-picker-rows (rows history &key providers &aux (frequent '()))
  "Promote at most five available model pairs from retained HISTORY.
With PROVIDERS, the rest of the root is providers, never more model rows."
  (loop for entry across history
        for value = (list (gethash "provider" entry) (gethash "model" entry))
        for row = (find value rows :key #'picker-row-value :test #'equal)
        when (and row (not (member row frequent :test #'eq))) do (push row frequent)
        when (= (length frequent) 5) do (loop-finish))
  (if (null frequent)
      (or providers rows)
      (let ((others (or providers
                        (remove-if (lambda (row) (member row frequent :test #'eq)) rows))))
        (append
         (list (make-picker-row :label +frequently-used+ :heading t))
         (mapcar (lambda (row) (if providers (models-root-row row) row))
                 (reverse frequent))
         (when others
           (append (list (make-picker-row :heading t)
                         (make-picker-row :label (if providers "Providers" "Other models")
                                          :heading t))
                   others))))))

(defun models-dialog-catalog-rows (dialog)
  "The canonical model rows, narrowed by the dialog's existing tab strategy."
  (let* ((rows (models-catalog-picker-rows))
         (tab (nth (picker-dialog-tab dialog) (picker-dialog-tabs dialog)))
         (match (getf tab :match)))
    (if match (remove-if-not (lambda (row) (funcall match row (getf tab :value))) rows) rows)))

(defun models-provider-row-p (row provider)
  "Whether a model row, including a failed-listing row, belongs to PROVIDER."
  (if-let (value (picker-row-value row))
    (string-equal (first value) provider)
    (string-equal (picker-row-label row) (format nil "~a/" provider))))

(defparameter +search-models-root+ "Search providers and models"
  "/models' provider list's search row: its query finds models too
(SEARCH-MODELS-FROM-ROOT).")

(defun models-sign-in-rows ()
  "While this home is not signed in to nodecode.ai (+OWN-PROVIDER+ is not
connected), the one row /models' search offers for it: Enter opens /login;
NIL once it is."
  ;; The provider is on no models.dev list, so a home not signed in met
  ;; nothing of ours: searching `nodecode' in /models found nothing
  ;; (the operator, 2026-10-22).
  (unless (member +own-provider+ (picker-connected-provider-ids) :test #'string-equal)
    (list (make-picker-row :label "Sign in to nodecode.ai" :tone :own-model
                           :detail "nodecode's own models"
                           :value (list :model (list :login))))))

(defun search-models-from-root (dialog)
  "Have DIALOG, /models' provider list, find models as well as providers:
while its box holds a query, every model a turn can run on lists under the
list's own rows, newest first (MODEL-CHOICES), each picked the way a
Frequently used shortcut is (MODELS-ROOT-ROW), and -- while this home is not
signed in to nodecode.ai -- the row that signs it in (MODELS-SIGN-IN-ROWS).
=> DIALOG."
  ;; The rows go on through DIALOG's one rows function, so every restock -- a
  ;; refreshed listing, the history read, a saved key -- builds the list the
  ;; box asks for. SEARCHING is the kind it built last: a keystroke rebuilds
  ;; only when the box fills or empties, and the rest only rerank.
  (let ((root (connect-dialog-provider-rows dialog))
        (searching nil))
    (flet ((querying ()
             (and (eq :provider (connect-dialog-step dialog))
                  (plusp (length (picker-dialog-search dialog))))))
      (setf (connect-dialog-provider-rows dialog)
            (lambda (&aux (rows (funcall root)))
              (if (setf searching (querying))
                  ;; a model the list already offers on both tabs (Frequently
                  ;; used, Local) is not offered twice
                  (let ((held (loop for row in rows
                                    for pair = (getf (picker-row-value row) :model)
                                    when (and (consp pair) (stringp (first pair))
                                              (equal "" (picker-row-key row)))
                                      collect pair)))
                    (append rows
                            (loop for row in (models-catalog-picker-rows)
                                  for pair = (picker-row-value row)
                                  when (and pair (not (member pair held :test #'equal)))
                                    collect (models-root-row row))
                            (models-sign-in-rows)))
                  rows))
            (picker-search-handlers dialog)
            ;; a result step's edit keys reach the query too: they rebuild nothing
            (lambda-ignoring (dialog query)
              (when (and (eq :provider (connect-dialog-step dialog))
                         (not (eq searching (querying))))
                (setf (picker-dialog-rows dialog) (funcall (connect-dialog-provider-rows dialog)))))))
    (when (equal (picker-dialog-search-placeholder dialog) "Search providers")
      (setf (picker-dialog-search-placeholder dialog) +search-models-root+))
    dialog))

(defun models-root-row (row)
  "A model ROW as /models' provider list carries it: a copy whose value,
(:MODEL (PROVIDER MODEL)), the wizard picks as that model instead of asking
a provider's key."
  (let ((copy (copy-picker-row row)))
    (setf (picker-row-value copy) (list :model (picker-row-value row)))
    copy))

(defun restock-models-dialog (model dialog value &key top)
  "Refill DIALOG, MODEL's open /models list, in place: its provider step's rows, else the
catalog under its frequently used head, the cursor back on VALUE's row past the headings
-- or, with TOP, on the first row."
  (setf dialog.rows (if (connect-dialog-p dialog)
                        (funcall (connect-dialog-provider-rows dialog))
                        (models-frequent-picker-rows
                         (models-dialog-catalog-rows dialog)
                         (gethash dialog *models-frequent-by-dialog* #()))))
  (picker-reseat-selection dialog value)
  (setf dialog.selection (past-headings (picker-visible-rows dialog) dialog.selection 1))
  (when top
    (picker-seat-top dialog)
    ;; The Nodecode section stays on top in view; the cursor takes the
    ;; first shortcut, the model a reopen picks again.
    (let* ((visible (picker-visible-rows dialog))
           (at (position-if (lambda (row) (and (picker-row-heading row)
                                               (equal +frequently-used+ (picker-row-label row))))
                            visible)))
      (when at (setf dialog.selection (past-headings visible (1+ at) 1)))))
  (when model.band (render-state model)))

(defun install-frequent-models (model dialog value)
  "Install history only in the identical dialog that dispatched the read.
Closing, replacing, or reopening the picker discards its old completion."
  (unless (eq dialog model.dialog)
    (return-from install-frequent-models :quiet))
  (when-let (failure (off-loop-failure value))
    (show-status model (format nil "Couldn't read frequently used models: ~a" failure) :error-p t)
    (return-from install-frequent-models nil))
  (let* ((history (gethash "models" value))
         (selected (picker-highlighted-value dialog))
         (untouched (and (= dialog.selection (past-headings (picker-visible-rows dialog) 0 1))
                         (zerop (picker-dialog-view-offset dialog))
                         (zerop (length (picker-dialog-search dialog))))))
    (setf (gethash dialog *models-frequent-by-dialog*) history)
    ;; Retain the ranking while a credential is being entered, but never
    ;; replace that step's rows or selection with a late history answer.
    (when (and (eq :model-providers dialog.kind) (not (eq :provider (connect-dialog-step dialog))))
      (return-from install-frequent-models :quiet))
    (restock-models-dialog model dialog selected :top untouched)))

(defun start-models-frequent-read (model dialog)
  "Read retained model history off the UI loop, scoped to this dialog."
  (when model.program
    (off-loop model
              (lambda () (gateway-model-http-call model :get "/api/gateway/models/frequent" nil))
              (lambda (model value) (install-frequent-models model dialog value)))))

(defun open-models-picker (model &key (lands :session) then provider
                                 &aux (rows (models-catalog-picker-rows)) (auxiliary (eq lands :auxiliary)))
  "Open the model detail picker, narrowed to PROVIDER when given: a
provider picked on either of /models' tabs, and setup's model. Open from the
current caches, then refresh the fetched listings behind it."
  ;; LANDS says where Enter lands the row: :SESSION pins this shell's session
  ;; and makes it the default new sessions start on (SLASH-MODELS-OP);
  ;; :AUXILIARY, the Auxiliary tab's, lands the auxiliary model and pins no
  ;; session; :DEFAULT moves the gateway default
  ;; alone and pins no session (SLASH-DEFAULT-MODEL-OP), the setup walk's
  ;; model; a function, (MODEL TOKENS) => (values NOTICE FAILED), lands it
  ;; where its caller keeps a model of its own. THEN, when
  ;; given, is called once the
  ;; dialog is done, with the model and the picked (PROVIDER MODEL) pair — NIL
  ;; on Esc or a refused pick: the setup wizard's continuation, which reports a
  ;; model only when one was picked, and /models' way back to its providers.
  ;; A continuation always gets the list, empty until the refresh below lands
  ;; in it: without it, a walk whose list had not loaded yet went back to its
  ;; providers, the bare /models report standing over their key hints (Mac
  ;; take, 2026-10-21).
  ;; => (values notice cmd).
  (cond
    ((or rows provider then)
     (open-picker-dialog
      model (models-picker-title (length rows) nil :auxiliary auxiliary) rows
      ;; The row's value is the (PROVIDER MODEL) token pair, so Enter
      ;; routes the lane too — the Zig "select for session" semantics.
      (lambda (model tokens)
        (multiple-value-bind (notice failed)
            (if (functionp lands)
                (funcall lands model tokens)
                (ecase lands
                  (:session (slash-models-op model tokens))
                  (:auxiliary (slash-auxiliary-model-op model tokens))
                  (:default (slash-default-model-op model tokens))))
          ;; A pick the gateway refused says why in a toast, over whatever
          ;; comes next: its notice is set after the dialog closes, when no
          ;; frame carries it, and without this the pick would be silent. It
          ;; is no pick to the walk that chained this either.
          (cond (failed
                 (show-status model (or (and (stringp (first tokens)) (provider-id-refusal (first tokens)))
                                        notice)
                              :error-p t)
                 (when then (funcall then model nil))
                 nil)
                (t (when then (funcall then model tokens))
                   notice))))
      :kind (if auxiliary :auxiliary-models :models)
      :searchable t
      :search-placeholder "Search models"
      :empty-label (if rows "No models found" "Loading the model list…")
      ;; The Zig models footer, minus the ctrl+g default and tab
      ;; pagination chords this shell does not bind.
      :hint (case lands
              (:auxiliary "enter use as auxiliary  esc close  ↑↓ navigate")
              (:session "enter select for session  esc close  ↑↓ navigate")
              (t "enter select  esc close  ↑↓ navigate"))
      :on-cancel (and then (lambda (model) (funcall then model nil))))
     (when provider
       (register-picker-tabs model.dialog
                             (list (list :label provider :value provider :match #'models-provider-row-p)))
       (setf (picker-dialog-rows model.dialog) (models-dialog-catalog-rows model.dialog)
             ;; the title says which model Enter moves, the tab whose models
             ;; these are: a `Models' after `Auxiliary model' read as a third
             (picker-dialog-context model.dialog) ""
             (picker-dialog-hint model.dialog) (if auxiliary
                                                    "enter use as auxiliary  esc providers  ↑↓ navigate"
                                                    "enter select for session  esc providers  ↑↓ navigate"))
       (picker-seat-top model.dialog)
       (seat-models-picker model.dialog provider :auxiliary auxiliary))
     (start-models-frequent-read model model.dialog)
     (when (start-provider-models-refresh model)
       (setf (picker-dialog-title model.dialog)
             (models-picker-title (length rows) t :auxiliary auxiliary)))
     (values nil nil))
    (t
     ;; Nothing cached yet: fetch, and report the selection meanwhile —
     ;; reopening once the refresh lands shows the list.
     (start-provider-models-refresh model)
     (values (if auxiliary
                 (slash-auxiliary-model-op model nil)
                 (slash-models-op model nil))
             nil))))

(defun suggested-model (provider &optional word &aux (best nil)
                                                    (best-date nil))
  "The model id models.dev lists for PROVIDER that a turn can run on
(CATALOG-MODEL-TURN-P) and was released last, or NIL."
  ;; The organism works through tools, and release order is the one ranking
  ;; the catalog carries; a tie goes to the id sorting first. WORD, when
  ;; given, is one of the dash-separated words the id must carry.
  (maphash (lambda (id entry &aux (date (catalog-model-release-date entry)))
             (when (and date
                        (or (null word) (member word (uiop:split-string id :separator "-")
                                                :test #'string=))
                        (catalog-model-turn-p entry)
                        (or (null best-date)
                            (string> date best-date)
                            (and (string= date best-date) (string< id best))))
               (setf best id
                     best-date date)))
           (or (json-value (models-catalog-table) :object provider "models")
               (make-hash-table)))
  best)

(defun seat-models-picker (dialog provider &key auxiliary)
  "Start the open /models DIALOG on PROVIDER: on the model the shared
config's default -- with AUXILIARY, its auxiliary model -- already names
there, else, for the default, on its newest (SUGGESTED-MODEL), the search row
saying which. Neither listed, nothing moves."
  ;; The newest model is no suggestion for the auxiliary model, which wants a quick one.
  (let* ((kept (multiple-value-bind (pinned id)
                   (shared-model-pin (if auxiliary "auxiliary_model" "default_model"))
                 (and pinned (string-equal pinned provider) id)))
         (suggested (and (not auxiliary) (suggested-model provider))))
    (flet ((row-at (model-id)
             (and model-id
                  (position-if (lambda (row &aux (value (picker-row-value row)))
                                 (and (consp value)
                                      (string-equal (first value) provider)
                                      (string-equal (second value) model-id)))
                               (picker-visible-rows dialog)))))
      (let* ((kept-at (row-at kept))
             (at (or kept-at (row-at suggested))))
        (when at
          (setf dialog.selection at
                dialog.search-placeholder
                ;; a kept model the list does not carry is said, not passed over in silence
                (if kept-at
                    (format nil "~a, the ~:[model this home names~;auxiliary model now~] · type to search"
                            kept auxiliary)
                    (format nil "~@[~a, the model this home names, is not listed · ~]~
                                 suggested: ~a, the newest with tool calls · type to search"
                            kept suggested))))))))

(defun install-refreshed-models (model &optional landed &aux (dialog model.dialog))
  "Rebuild an open /models picker from the refreshed cache: rows swap in
place, the search text stays, and the selection follows its row value — a
value the refresh dropped leaves the cursor on its seat."
  (declare (ignore landed))
  (when (and (picker-dialog-p dialog)
             ;; The /models catalog in either mode, or a provider list: /models'
             ;; or the setup walk's, which has no kind and opens before the
             ;; gateway's list lands -- its offers (PROVIDER-OFFERS) came with
             ;; it, and a walk that kept its first rows never listed them.
             (or (member dialog.kind '(:models :auxiliary-models))
                 (and (connect-dialog-p dialog)
                      (eq :provider (connect-dialog-step dialog)))))
    (cond ((eq dialog.kind :model-providers) (setf dialog.title "Models"))
          ((not (connect-dialog-p dialog))
           (setf dialog.title (models-picker-title (length (models-dialog-catalog-rows dialog)) nil
                                                   :auxiliary (eq dialog.kind :auxiliary-models))
                 ;; a list opened before anything was cached said it was loading
                 dialog.empty-label "No models found")))
    (restock-models-dialog model dialog (picker-highlighted-value dialog))))

(defparameter +new-profile-action+ :new-profile
  "The roster's own action row, as its row value: ENTER on it opens the name
field rather than switching anywhere. No profile carries it — a name is a
string, and PROFILE-NAME-P refuses anything else — so one EQL on the value
separates the two kinds of row.")

(defun profile-picker-rows ()
  "One row per profile, over the facts NLK:PROFILE-RECORD already carries:
the name, a mark for the one this shell runs, and, as the detail, the
home with whatever else is true of it — a live organism's pid and port,
`boots by default', the description — and, under them all, the row that
makes a new home."
  ;; The create row rides last: the homes read first, the act after them, and
  ;; the search finds it by `new' or `create' like any other row's words.
  (append
   (loop for record in (nlk:profiles)
         for name = (nlk:profile-record-name record)
         for pid = (nlk:profile-record-pid record)
         for description = (nlk:profile-record-description record)
         collect (make-picker-row
                  :label name
                  :mark (if (nlk:profile-record-current-p record) "*" " ")
                  :mark-tone (if (nlk:profile-record-current-p record) :accent :muted)
                  :detail (format nil "~{~a~^  ·  ~}"
                                  (remove nil
                                          (list (string-right-trim
                                                 "/" (namestring (nlk:profile-record-home record)))
                                                (and pid
                                                     (format nil "serving pid ~d~@[ port ~d~]"
                                                             pid (nlk:profile-record-port record)))
                                                (and (nlk:profile-record-sticky-p record)
                                                     "boots by default")
                                                (and description (plusp (length description))
                                                     description))))
                  :value name))
   ;; The roster's last row: the gesture that makes a home instead of entering one. The
   ;; mark column says what the row does -- a plus, in the tone a row that acts gets --
   ;; and the detail says what lands on disk.
   (list (make-picker-row
          :label "New profile"
          :mark "+"
          :mark-tone :accent
          :detail "make a home of its own, this one's settings and secrets carried over"
          :value +new-profile-action+))))

(defun profile-create-context ()
  "The muted rule line under the create field: what ENTER is about to make. A
refusal replaces it on the open field (PROFILE-CREATE-PICKER-INPUT), which is
why the field is still open with the name still in it."
  (format nil "ENTER makes ~a<name>/ — sharing memory, skills and credentials; Esc goes back to the roster"
          (namestring nlk:*profiles-directory*)))

(defun open-profile-create-picker (model)
  "The roster's create row: one field for a new home's name, in the pane the
roster occupies. ENTER and ESC are PROFILE-CREATE-PICKER-INPUT's, which owns
what each gesture means."
  (open-picker-dialog
   model "Profiles" '() nil
   :kind :profiles
   :context " New"
   :subtitle "a-z, 0-9, `-' and `_' — a home of its own that reads your memory, skills and credentials where they are"
   :input-label "New profile"
   :input ""
   :input-context (profile-create-context)
   :hint "enter create  esc back to the roster"
   :on-input (lambda (model text) (profile-create-picker-input model text)))
  nil)

(defun profile-create-picker-input (model text &aux (dialog model.dialog)
                                                    (name (and text (nlk:trimmed text))))
  "ENTER or ESC on the create field. ESC (TEXT NIL) is the way back to the
roster, making nothing. ENTER makes the home the text names — the same
NLK:PROFILE-CREATE the `create' verb calls, so the clone default and every
refusal are that function's, not this gesture's — and reopens the roster
seated on the new row, where the next ENTER takes this shell there. A refusal
leaves the field open, the words in it, and says why underneath: the gesture
was right, the name needs fixing."
  (if (null text)
      (progn (open-profiles-picker model) nil)
      (handler-case
          (open-profiles-picker
           model :seat name
           :notice (format nil "created ~a at ~a — ENTER takes this shell there"
                           name (string-right-trim "/" (namestring (nlk:profile-create name)))))
        (nlk:profile-error (refusal)
          ;; The field and its words stay; the rule line carries the reason.
          (when dialog
            (setf dialog.input-context (format nil "~a" refusal)))
          nil))))

(defmacro with-profile-refusal (&body body)
  "BODY, with a profile refusal answered as the one line it is."
  `(with-handlers ((nlk:profile-error (refusal) (format nil "profile: ~a" refusal))) ,@body))

(defun profile-picker-delete (model value &aux (roster model.dialog))
  "The roster's Delete / Ctrl+X: confirm whole-home removal, then refresh
in place. The canonical owner refuses protected homes before asking and
rechecks them at confirmation; the create action is never a delete target."
  (when (stringp value)
    (labels ((back (&optional notice)
               (setf (picker-dialog-rows roster) (profile-picker-rows)
                     model.dialog roster)
               (picker-reseat-selection roster value)
               (when notice
                 (setf (picker-dialog-context roster) notice)
                 (show-status model notice))
               nil))
      (handler-case
          (nlk:profile-command (list "delete" value) :command "/profile")
        (nlk:profile-needs-yes (question)
          (declare (ignore question))
          (open-picker-dialog
           model "Delete profile"
           (list (make-picker-row :label "Cancel" :value :cancel)
                 (make-picker-row :label (format nil "Delete ~a" value) :value :delete))
           (lambda-ignoring (model answer)
             (if (eq answer :delete)
                 (back (with-profile-refusal
                         (nlk:profile-delete value)
                         (format nil "deleted ~a" value)))
                 (back)))
           :kind :profiles
           :context (format nil " ~a" value)
           :subtitle "Removes this profile's whole home: store, layer and secrets."
           :hint "enter choose  esc cancel"
           :on-cancel (lambda (model) (declare (ignore model)) (back))))
        (nlk:profile-error (refusal)
          (back (format nil "profile: ~a" refusal))))))
  nil)

(defun open-profiles-picker (model &key seat notice)
  "Bare /profile: the roster as a picker rather than a paragraph, on the
/sessions model."
  ;; Enter runs SWITCH-PROFILE-HERE: the kernel answers the plan
  ;; (PROFILE-SWITCH), the shell's performer carries it, and the shell's own
  ;; gesture ends the loop so CALL-WITH-PROFILE-SWITCH's tail can replace the
  ;; process once the terminal is back. Nothing here execs; the create row's
  ;; ENTER opens a field instead. SEAT is the row to leave the cursor on — the
  ;; home just made — and NOTICE the line to land with it.
  (with-slash-notice ("profile")
    (open-picker-dialog
     model "Profiles" (profile-picker-rows)
     ;; ENTER, one gesture per kind of row: the create row opens the name field, a
     ;; profile's name switches this shell into that home.
     (lambda (model value)
       (if (eql value +new-profile-action+)
           (open-profile-create-picker model)
           (switch-profile-here model.program value)))
     :kind :profiles
     :searchable t
     :context " Homes"
     :search-placeholder "Search profiles"
     :empty-label "No profiles"
     :no-match-label "No matching profiles"
     :on-delete #'profile-picker-delete
     :hint "enter switch/create  del/ctrl+x delete  esc close  up/down navigate  type search")
    (when seat
      (picker-reseat-selection model.dialog seat))
    (when notice
      (show-status model notice)))
  nil)

(defun profile-headless-text (args session-id &aux (tokens (split-words (or args ""))))
  "/profile with no shell to open a picker in — a channel lane, the model's
eval, the headless surface: PROFILE-COMMAND over the tail, and bare is the
roster text it always was, since Enter on a row is a gesture only a
terminal has."
  ;; `switch' is declined here rather than performed. The verb is the same one
  ;; either way; what differs is that this process is a gateway serving every
  ;; attached shell, and replacing its image would take all of them with it.
  ;; The decline says where the gesture lives and what moves a home from here.
  (declare (ignore session-id))
  (if (and tokens (string-equal (first tokens) "switch"))
      (format nil "profile: switch relaunches the terminal it is typed in, ~
                     and this organism has none. Run `/profile switch ~a' in ~
                     a shell, or `/profile use ~:*~a' to move the next launch."
              (or (second tokens) "NAME"))
      (with-profile-refusal (nlk:profile-command (or args "") :command "/profile"))))

(defun switch-profile-here (program name)
  "Run NAME in this shell, now: hand the plan to whatever performer this
shell installed — a TUI's parker records it for CALL-WITH-PROFILE-SWITCH's
tail to perform — then end the loop, because the terminal only comes back
when the loop ends and only a process that has given it back may be
replaced."
  ;; The gesture sends the quit as well as the parker does, and that is the
  ;; point rather than a duplicate: a parker is a closure created when the
  ;; shell started, so a shell whose image predates the parker's own send can
  ;; never be handed it — while a definition reaches a running shell live.
  ;; A refusal answers as text: nothing is parked and no loop
  ;; ends.
  (with-profile-refusal
    (multiple-value-bind (binary argv) (nlk:profile-switch name)
      (funcall nlk:*profile-switch-performer* name binary argv)
      (when program (nlt:send program (nlt:make-quit-msg)))
      (format nil "switching to ~a: this shell comes back in that home" name))))

(defun profile-slash-op (model args &aux (tokens (split-words (or args ""))))
  "/profile's answer."
  ;; Bare opens the picker — a roster you can walk and press Enter on, which
  ;; is the gesture the text roster never was; with a verb it is
  ;; PROFILE-COMMAND over the tail, a refusal as one line rather than a
  ;; condition. `switch NAME' is the one verb the shell performs itself,
  ;; through SWITCH-PROFILE-HERE, because moving this shell needs the loop
  ;; that paints it. Either way the machine's homes, not a session's state: it
  ;; runs in this shell's own image on both shells, like /cells.
  (cond
    ((null tokens) (open-profiles-picker model))
    ((and (string-equal (first tokens) "switch")
          (second tokens)
          (null (cddr tokens)))
     (switch-profile-here model.program (second tokens)))
    (t (with-profile-refusal (nlk:profile-command args :command "/profile")))))

(defun gateway-live-facts (state)
  "What the organism serving STATE's store says of itself — its build,
when it started, the lines on its standing board — as the plist
NLK:GATEWAY-REPORT reads, or NIL when it does not answer."
  ;; One GET of the identity route under this home's operator token, bounded
  ;; in seconds: a state printed on a wedged gateway must cost the operator a
  ;; moment, not half a minute.
  (when-let (port (nlk:gateway-state-owner-port state))
    (ignore-errors
     (let ((identity (gateway-http-session-request
                      port :get "/api/gateway/identity" nil
                      (gateway-configured-operator-token)
                      :connect-timeout 2 :read-timeout 3)))
       (list :version (json-value identity :string "version")
             :binary (json-value identity :string "build")
             :started-at (json-value identity :integer "started_at")
             :notices (loop for notice across (json-array identity "notices")
                            when (stringp notice) collect notice))))))

;;; --- /context gathering -----------------------------------------------------
;;; The one place the shells and the headless surface turn live state plus
;;; the window belief into /context's inputs; the renderer is slash.lisp's,
;;; pure.

(defun context-window-view (provider model-id)
  "The window belief for (PROVIDER, MODEL-ID) => (values WINDOW GRADE FLOOR):
GRADE `learned' means durable observations moved it; FLOOR, the largest
prompt proven. NIL when either is not a string."
  ;; /context and the rows a browser is sent both read it.
  (when (and (json-value provider :text) (json-value model-id :text))
    (bind ((prior (model-context-tokens model-id))
           (operator-p (and prior (model-context-tokens-operator-p model-id)))
           (window (context-window-belief provider model-id prior operator-p))
           ((floor ceiling) (and (nlk:store-open-p)
                                 (nlk:context-window-observation provider model-id))))
      (values window
              (cond ((null window) "unknown")
                    (operator-p "configured")
                    ((or (and floor (plusp floor)) (and ceiling (plusp ceiling)))
                     "learned")
                    (t "default"))
              floor))))

(defun context-report-text (provider model-id &rest report
                            &key session-id context-usage &allow-other-keys)
  "RENDER-CONTEXT-REPORT over REPORT — the :LABEL, :SESSION-ID and usage
accumulators a caller gathered — with the window belief for (PROVIDER,
MODEL-ID) and, while CONTEXT-USAGE holds no round, the occupancy estimated
from the session's durable record."
  ;; The shells and the headless surface both answer through it.
  (multiple-value-bind (window grade floor) (context-window-view provider model-id)
    (multiple-value-bind (context-estimate context-parts)
        (and (null context-usage)
             session-id
             (session-context-estimate session-id provider model-id))
      (apply #'render-context-report
             :context-estimate context-estimate
             :context-parts context-parts
             :window window
             :window-grade grade
             :window-floor floor
             report))))
(defun execute-slash-command (model name args &aux (entry (find-builtin-slash-command name))
                                                   (op (and entry entry.op)))
  "Execute the slash command NAME (canonical, slash-less) with raw ARGS on
MODEL's transport."
  ;; Returns (values notice cmd): NOTICE is the live-only result text for the
  ;; composer status (NIL for none), CMD a program command to run after the update
  ;; (NIL for none). The gateway-backed ops call the HTTP control ops.
  ;;
  ;; The catalog's DISPATCH slot says how the shell calls the command's op
  ;; (slash.lisp SLASH-COMMAND); a NIL slot is a command spelled out below.
  (case (and entry entry.dispatch)
    (:plain (values (funcall op model) nil))
    (:args (values (funcall op model args) nil))
    (:name (values (funcall op model name) nil))
    ;; The Zig usage_help argument gate answers before the op runs.
    (:guarded (values (or (slash-no-args-guard name args) (funcall op model)) nil))
    ;; Local-process introspection on either shell (/gc): the facts
    ;; describe THIS image — its collections — not a session's state.
    (:local (values (funcall op args model.session-id) nil))
    (t
     (cond
       ((string= name "exit") (values nil (nlt:quit-cmd)))
       ((string= name "models")
        ;; Bare /models opens the providers under the Main and Auxiliary
        ;; tabs; with no catalog it reports the active selection. /models
        ;; refresh forgets every fetched listing first, so the open
        ;; refetches behind the list, and /models list answers the
        ;; catalog as text — the shape a room gets, now on both shells.
        ;; `aux' leads the same forms for the auxiliary model: bare, the
        ;; list on its Auxiliary tab; a model lands this image's auxiliary
        ;; target and general.auxiliary_model together, pinning no session;
        ;; `default' takes it out of both.
        ;; `decider' leads the decider's: bare, its tab; a decider id pins
        ;; it in general.decider_model, `none' takes it out.
        (let* ((tokens (split-words args))
               (seat (find (first tokens) '("aux" "decider") :test #'equalp))
               (aux (equalp seat "aux"))
               (picks (if seat (rest tokens) tokens)))
          (cond
            ;; `backups' opens the Backup tab: the failover chain is set there.
            ((and (not seat) (member (first tokens) '("backup" "backups") :test #'equalp))
             (if (rest tokens)
                 (values (slash-usage name) nil)
                 (open-models-provider-picker model :tab "backup")))
            ((> (length picks) 2) (values (slash-usage name) nil))
            ((and (not seat) (equalp (first tokens) "list"))
             (values (model-catalog-list-text (second tokens)) nil))
            ((or (null picks) (and (not seat) (equalp picks '("refresh"))))
             (when picks (drop-provider-models-cache (listable-provider-ids)))
             (open-models-provider-picker model :tab (if seat (string-downcase seat) "main")))
            (aux (values (slash-auxiliary-model-op model picks) nil))
            (seat (values (slash-decider-model-op model picks) nil))
            (t (values (slash-models-op model picks) nil)))))
       ((string= name "evict")
        (multiple-value-bind (percent through valid-p) (evict-args args)
          (values (if valid-p (slash-evict-op model percent through) *evict-usage-hint*)
                  nil)))
       ((and (string= name "gateway") (equal "setup" (first (split-words (or args "")))))
        ;; A channel set up step by step, on a sheet over the shell.
        (values (open-channel-setup model (second (split-words args))) nil))
       ((member name '("gateway" "web") :test #'string=)
        ;; The machine's gateway, not a session's state: local, like /profile.
        ;; Its verbs wait on systemd — an off waits on the gateway's own unwind
        ;; — so the answer comes from a thread of its own, and the shell keeps
        ;; painting meanwhile.
        ;; `nodecode web' on the shell: WEB-TEXT's answer in the composer
        ;; status, which is live-only, so the operator token in the link reaches this
        ;; terminal and never the transcript. Off the loop, like /gateway:
        ;; the answer asks the gateway, which may be this image's own.
        (let ((program model.program))
          (spawn (format nil "~a-slash" name)
            (handler-case
                (multiple-value-bind (text error-p)
                    (if (string= name "gateway")
                        (values (with-handlers ((nlk:gateway-error (refusal)
                                                  (format nil "gateway: ~a" refusal)))
                                  (nlk:gateway-text (or args "") :command "/gateway"
                                                                 :live #'gateway-live-facts))
                                nil)
                        (bind (((text _ stream) (web-text (split-words args))))
                          (values text (eq stream :error))))
                  (post-shell-notice program text :error-p error-p :answer t))
              (error (condition)
                (ignore-errors
                 (post-shell-notice program (format nil "~a: ~a" name condition) :answer t)))))
          (values nil nil)))
       ((string= name "context")
        ;; The organism answers it (its headless arm, over POST
        ;; /api/gateway/slash): the occupancy estimate and the learned
        ;; window read its store, which no shell's process holds.
        (values (run-gateway-command model (find-builtin-slash-command "context") args)
                nil))
       (t
        ;; A token no builtin claims may be a cell's command, which runs
        ;; at the gateway with its verbatim argument tail.
        (let ((entry (find-gateway-command name)))
          (values (if entry
                      (run-gateway-command model entry args)
                      (format nil "unknown command /~a" name))
                  nil)))))))

(defun evict-args (args)
  "The parse of /evict's ARGS => (values PERCENT THROUGH VALID-P), the Zig
executeEvictSlashCommand's and the turn form: bare /evict sends neither;
`40` and `40%` both read a percent (the trailing sign is what a user types),
0 and >100 are not valid; `through TURN' names the turn id eviction runs
through. What is not valid renders the usage hint rather than a request."
  (let* ((trimmed (string-trim " " args))
         (through (and (uiop:string-prefix-p "through " trimmed)
                       (string-trim " " (subseq trimmed 8))))
         (percent (and (null through)
                       (ignore-errors (parse-integer (string-right-trim "%" trimmed))))))
    (cond ((zerop (length trimmed)) (values nil nil t))
          (through (values nil through (plusp (length through))))
          (t (values percent nil (and (integerp percent) (< 0 percent 101)))))))

(defun footer-right-candidates (model &aux (port model.http-port))
  "Right-lane segment candidates, longest first. Shed workspace/profile
before the gateway endpoint; its lifecycle is color, never a text suffix. The
caveats a setup walk ended on (SETUP-ISSUE-SEGS) lead each and are shed last."
  (let* ((workspace (workspace-footer-label))
         (profile (nlk:profile-label))
         (separator (make-seg :text " · " :tone :muted))
         ;; what the last setup walk ended on, never the first thing shed
         (issues (setup-issue-segs model)))
    (flet ((part (text target) (hover-seg model text target)))
      (let* ((lead (append (when (and profile (plusp (length profile)))
                            (list (part profile (cons :command "profiles"))))
                          (when (plusp (length workspace))
                            (append (and profile (list separator))
                                    (list (part workspace (file-uri (workspace-directory))))))))
             ;; The setup walk names no gateway before an endpoint means anything.
             (candidates
               (if (and port (not model.setup-walk))
                   ;; The page as it opens, on Sessions (the operator, 2026-10-21:
                   ;; "Clicking gateway should ... go to the SESSIONS part").
                   (let* ((target (format nil "http://127.0.0.1:~d/web/~@[#t=~a~]"
                                          port (and model.http-token (quri:url-encode model.http-token))))
                          (endpoint (hover-seg model "Gateway" target
                                               (ecase model.sync-lifecycle
                                                 (:connected :success)
                                                 ((:connecting :reconnecting) :tool)
                                                 (:failed :danger)))))
                     (list (append lead (and lead (list separator)) (list endpoint))
                           (list endpoint)))
                   (and lead (list lead)))))
        ;; The nodecode.ai meter (account-login.lisp) leads while it fits: it is shed after the
        ;; profile and folder, before the gateway.
        (when-let (meter (account-meter-segs model))
          (setf candidates (append (loop for candidate in candidates
                                         collect (append meter (list separator) candidate))
                                   (last candidates))))
        (if issues
            (append (mapcar (lambda (candidate) (append issues (list separator) candidate)) candidates)
                    (list issues))
            candidates)))))


(defun live-status-hint (model)
  "Keyboard affordances for the live status verb's hover tooltip: the viewport
control, or the empty string when it is unbound. Esc stands on the bar itself
and the steer keys in the composer (COMPOSER-ROWS), so the tooltip no longer
repeats them."
  (or (tool-detail-hint model) ""))

(defun turn-stopping-p (model)
  "Whether Esc asked the turn in flight to stop and no terminal fact has
settled it yet."
  (and model.turn-in-flight model.stop-requested
       (equal model.stop-requested model.active-turn-id)))

(defun fetch-gateway-model-facts (model)
  "Dial the gateway for MODEL's session model facts => the plist, or NIL
(the empty plist) on any refusal."
  ;; One call answers identity, pin provenance and reasoning effort because
  ;; the gateway resolves all three against the same session-effective target;
  ;; an attached client has no store to ask instead. Never called on the loop
  ;; thread — the frame renders the cache; REFRESH-GATEWAY-MODEL-FACTS fills
  ;; it from a thread of its own.
  (ignore-errors (nth-value 3 (target-snapshot model))))

(defun gateway-model-facts (model &aux (facts model.model-facts))
  "The gateway's model facts for this client's session, read from the
MODEL-FACTS cache => a plist (:provider :model :pinned :effort), empty
until a fetch lands."
  ;; NEVER dials: the footer renders every frame on the loop thread, and a
  ;; dial there would stall the whole shell for the connect timeout whenever
  ;; the endpoint is dead. The connect ack queues READ-MODEL-FACTS-CMD on the
  ;; worker; the control ops that move the target refresh in place from their
  ;; own off-loop contexts.
  (if (eq facts :unresolved) '() facts))

(defun publish-gateway-model-facts (model facts epoch)
  "Land FACTS in MODEL's cache — newest EPOCH only — the moment a read
answered. => T when it landed."
  ;; One slot, set to a value the reader owns, read by the frame path: the
  ;; write is a single pointer and the epoch keeps an older read from landing
  ;; over a newer one. Everything a FRAME needs is here; everything a NOTICE
  ;; needs waits for the loop.
  (when (and facts (= epoch model.facts-epoch))
    (setf model.model-facts facts)
    t))

(defun refresh-gateway-model-facts (model)
  "Refetch MODEL's gateway facts on a thread of their own and install the
answer on the loop (the WRITE-CLIPBOARD shape: never the loop thread,
where a dial would stall every frame, and never the
worker, where a wedged gateway would park quit and every queued command
behind a 30 s read)."
  ;; The epoch captured at spawn keeps a slow fetch from overwriting newer
  ;; facts. A shell with no loop to install them on reads nothing at all.
  (let ((epoch (incf model.facts-epoch)))
    ;; The /models list rides the same moments: a port known, a model moved.
    (start-models-picker-warm model)
    (when model.program
      (off-loop model
                (lambda (&aux (facts (fetch-gateway-model-facts model)))
                  ;; PUBLISHED HERE, where the answer lands, not on the loop
                  ;; alone: the launch frame is painted before the loop drains
                  ;; its queue, so a value that only arrives as a message
                  ;; cannot be in the frame that first shows the footer.
                  ;; The loop keeps
                  ;; the other half — the notice it owes and the repaint
                  ;; (INSTALL-GATEWAY-MODEL-FACTS).
                  (publish-gateway-model-facts model facts epoch)
                  facts)
                (lambda (model facts) (install-gateway-model-facts model facts epoch)))))
  nil)

(defun install-gateway-model-facts (model facts epoch)
  "Install the facts a refresh read — newest EPOCH only — and repaint the
footer."
  (when (= epoch model.facts-epoch)
    ;; A NIL answer is a read that never landed — the control surface
    ;; refused, timed out, or the session did not exist yet — and that is no
    ;; evidence about the target: the cache keeps what it held, so
    ;; :UNRESOLVED still reads `setting up…' and an earlier answer stays the
    ;; last one the gateway confirmed. Installing the empty value instead
    ;; would render as a shell that has no model at all.
    (when facts
      (let ((before model.model-facts)
            (refusal (getf facts :refusal)))
        ;; Idempotent when the reader already published it.
        (publish-gateway-model-facts model facts epoch)
        ;; A target that cannot serve says so the moment its facts land —
        ;; at attach, and again whenever the target moves and is still
        ;; refused — never on a mere refresh of the same refused facts.
        (when (and refusal
                   (not (and (listp before) (equal refusal (getf before :refusal)))))
          (show-status model refusal :error-p t)))
      ;; The one landmark that says when the identity became knowable: an
      ;; attach whose facts land after the first frame is the case this path
      ;; exists to measure (NODECODE_PHASE_LOG).
      (nlk:note-boot-phase "model-facts" (getf facts :model))
      ;; A model chosen is measured once, before anyone prompts it (connection.lisp).
      (check-connection model)
      (render-state model))))

(defparameter *first-frame-facts-settle-ms* 25
  "How long the launch frame may wait for a model-facts read that is already
in flight (SETTLE-BEFORE-FIRST-FRAME). Bounded on purpose: the frame this
precedes is the operator's first screen, and a launcher's hand-over waits on
it.")

(defmethod settle-before-first-frame ((model gateway-chat-model))
  "Hold the launch frame for the facts read, briefly, when one is coming."
  ;; The footer's identity is the one fact this shell cannot resolve itself:
  ;; it belongs to the gateway, and the read that fetches it is already out
  ;; (the ladder's preload, which fires as soon as the session exists).
  ;; Waiting for it here keeps `setting up…' off the screen entirely; without
  ;; this, a frame painted before the identity arrives would be a flash the
  ;; operator reads as a stall. Three things bound the wait: only an
  ;; UNRESOLVED cache waits at all (a shell that already knows its target
  ;; paints at once), only a known endpoint waits (an owner still booting its
  ;; gateway has nothing to read from and paints at once), and the deadline
  ;; is 25 ms whatever the peer does — a gateway that never answers costs the
  ;; launch frame 25 ms and nothing more.
  (when (and model.http-port
             (eq model.model-facts :unresolved))
    (wait-until (lambda () (not (eq model.model-facts :unresolved)))
                (deadline-after (/ *first-frame-facts-settle-ms* 1000)) 0.001)))

(defun refresh-gateway-command-catalog (model)
  "Re-read the cell commands MODEL's gateway serves on a thread of its own
and post the answer to the shell (the model-facts shape: never the loop
thread, where a dial stalls every frame, and never the worker)."
  ;; GET /api/gateway/slash stays the authority; the COMMANDS_CHANGED frame
  ;; is the cue that it moved.
  (when-let (program model.program)
    (spawn "command-catalog-fetch"
      (when-let (msg (funcall (read-command-catalog-cmd model))) (nlt:send program msg)))))

(defun invalidate-gateway-model-facts (model)
  "Drop the cached model facts — and advance the epoch, so any fetch
already in flight cannot install what it read before the invalidation."
  ;; Called wherever this client changes the target or learns the gateway may
  ;; have moved under it: a fresh connection, a session switch, a landed model
  ;; selection, a landed effort pick.
  (incf model.facts-epoch)
  (setf model.model-facts :unresolved))

(defun footer-gesture-hint (model &aux (running (hash-table-count (session-activity-table model))))
  "A key hint the footer advertises beside the model, or NIL: the one gesture a
shell has that no slash command spells. `← sessions' whenever this shell
holds an attached session -- which for the gateway shell is every frame, home
screen included: Left answers LEAVE-SESSION-OP from home the same as from a
transcript, so the gesture is discoverable where the operator's eye already
rests, both screens."
  ;; With sessions running the hint counts them -- `← sessions · 2 running' --
  ;; read off the running-session map this shell holds (organism.activity):
  ;; what Left would show at work. The count rides beside the word, never in
  ;; its place: `← 1 running' left unsaid that the arrow is a key and the
  ;; count is sessions.
  (format nil "← sessions~[~:; · ~:*~d running~]" running))

(defun footer-identity (model)
  "The footer's provider/model identity => (values PROVIDER MODEL-ID STATE),
STATE one of :CONFIGURED, :UNCONFIGURED (nothing chose the target) or
:PENDING (this shell has not learned its target yet). The gateway footer
mirrors the turn binding (RUN's session-selection overlay)."
  ;; STATE is :UNCONFIGURED when nothing chose the target
  ;; (DEFAULT-MODEL-CONFIGURED-P): it is the image's build-time default, no
  ;; provider was connected for it, and the footer shows how to get a model
  ;; instead of naming it. :PENDING is no licence to guess: the slot says the
  ;; shell is still setting up rather than naming a lane no turn here would
  ;; bind. Without this, a paint-first launch would name the build-time lane
  ;; for its first frames. The durable per-session pin rides the overlay, so a
  ;; /models selection renders the lane the next turn will bind.
  ;;
  ;; The gateway computes that overlay — its own effective default with each
  ;; pin field on top, and whether anything chose it — and the client renders
  ;; exactly what it answered, so an attached client shows the running
  ;; organism's model rather than its own unrestored process defaults. Until an
  ;; answer lands the state is :PENDING: this cache is the one authority for
  ;; this shell's target, and neither the process globals nor a stale answer
  ;; may stand in for it.
  (if (eq model.model-facts :unresolved)
      (values nil nil :pending)
      (let ((facts (gateway-model-facts model)))
        (values (getf facts :provider) (getf facts :model)
                (if (getf facts :unconfigured) :unconfigured :configured)))))

(defun footer-reasoning-effort (model)
  "The reasoning-effort preference for the footer target, or NIL — the Zig
footerReasoningEffort input to appendReasoningEffort. The /think choice rides
the identity it belongs to: both came out of one target read resolved
against this session, so the gateway shell never asks a
store it may not own."
  (getf (gateway-model-facts model) :effort))

(defun chat-begun-p (model)
  "Whether MODEL's chat has begun: a transcript row, a live turn, or a submit
in flight (the Zig turn_command-projection-empty gate). Until it has, the
shell is on its home screen, a dialog over it or not."
  (or (find-transcript-row (constantly t) model.state)
      model.turn-in-flight
      model.pending-command-id))

(defun home-screen-p (model)
  "Whether the landing/home screen is active (Zig screens.zig isHomeScreen
distilled to the state this shell carries): no chat begun, no notice, and no
dialog but the setup walk's."
  ;; Without this, a fresh box's first frame would be the walk's dialog under a
  ;; blank band and a raw session id, not the landing screen. A draft is typed
  ;; on it, as on the launch's: the screen goes with the submit, not the first
  ;; key.
  (and (not (chat-begun-p model))
       (or (null model.dialog) model.setup-walk)))

(defun home-prompt-text (model)
  "The composer placeholder while home is active (Zig screens.zig
homePromptText): the connection narrative, the Zig table over the lifecycle
states this transport inhabits."
  ;; The Zig "Creating a Gateway session..." arm never arises: this client
  ;; always attaches with a resolved session. Connected is only the
  ;; invitation: the footer's Gateway, in its connected colour, says the rest.
  (case model.sync-lifecycle
    (:connected "Ask anything, or describe a task")
    ((:connecting :reconnecting) "Connecting to Gateway...")
    (:failed "Gateway sync failed - check the endpoint and retry")))

(defun checkpoint-listing-path (model)
  "The control path the tree picker fetches its rows from for MODEL's session,
or NIL when this shell has no session to ask about."
  ;; One writer of the string so the prefetch and the request it serves are
  ;; compared as equals — SLASH-TREE-OP composes the same path, and a shipped
  ;; change to it degrades to a prefetch miss (a fresh dial), never to a wrong
  ;; listing.
  ;;
  ;; No limit: the tree is the whole session. A row window read as a tree with
  ;; its oldest turns missing, which is what the operator saw (/tree beginning
  ;; at turn 11 while turn 1 sat 146 rows below the window's floor).
  ;;
  ;; The session id is read defensively: GATEWAY-MODEL-HTTP-CALL asks this of
  ;; every control call, and the inherited CHAT-MODEL slot can be genuinely
  ;; unbound on a client that has not finished attaching (the same unbound
  ;; slot that once crashed the shell header). A shell with no readable
  ;; session simply has no listing to prefetch.
  (when-let (session (json-value (ignore-errors model.session-id) :text))
    (format nil "/api/gateway/sessions/~a/checkpoints" session)))

(defun gateway-model-http-call (model method path body &key raw stream (read-timeout 30))
  "One control call for MODEL; RAW and READ-TIMEOUT as
GATEWAY-HTTP-SESSION-REQUEST takes them."
  ;; The tree picker's checkpoint listing is served from the prefetch the
  ;; arming Esc started when one is waiting for exactly this path; every other
  ;; call, and every miss, dials the gateway here.
  ;; Named here, once: every sender takes the shell's own gateway model, and
  ;; a model id handed in its place read as a NO-APPLICABLE-METHOD on the
  ;; port reader.
  (check-type model gateway-chat-model
              "an attached shell's gateway model; a snippet has none, and (help :operator) names the verbs that land the same facts")
  (or (and (eq method :get) (null body)
           (equal path (checkpoint-listing-path model))
           ;; Consumed fresh or stale; one in flight is this request, already started.
           (let ((entry *checkpoint-listing-prefetch*))
             (when (and entry (equal path (getf entry :path)))
               (setf *checkpoint-listing-prefetch* nil)
               (when (< (get-internal-real-time) (getf entry :until))
                 ;; Waiting on the started request beats dialing; 1 s bounds a slow gateway.
                 (let ((box (getf entry :box)))
                   (unless (car box)
                     (bt2:wait-on-semaphore (getf entry :semaphore) :timeout 1))
                   (car box))))))
      (gateway-http-session-request model.http-port
                                    method path body
                                    model.http-token :raw raw :stream stream
                                    :read-timeout read-timeout)))

