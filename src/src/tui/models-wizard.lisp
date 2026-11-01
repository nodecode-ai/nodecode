;;;; models-wizard.lisp --- the /models wizard. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; src/tui/model/models_dialog.zig distilled onto the dialog slot: a
;;; four-step flow (provider list -> optional custom id -> API key -> a
;;; failure's result) with the runtime.zig handleConnectDialogKey key ladder.
;;; A saved key returns to the list, which then opens on the connected
;;; providers, so one open connects as many providers as the operator has
;;; keys for. The dialog is transport-free: the shells inject the rows,
;;; ON-SAVE (persist the credential, => (values message error-p)) and
;;; ON-SUCCESS (what Enter on a connected provider opens — the models
;;; dialog, per the Zig flow).

;;; On the two input steps the :EMPTY-LABEL string is the field's own rule
;;; line rather than an empty-list label, which is the slot the Zig body
;;; prints it in. Verbatim minus the connect_setup_required arms — the Lisp
;;; shell has no startup-forced connect flow, so it has no `ctrl+c twice
;;; quits` state to describe.
(defparameter +connect-steps+
  '((:provider
     ;; no context: /models' tab strip says where the list is, and a walk its step
     :context "" :searchable t
     :subtitle "Choose a provider from models.dev."
     ;; /models' own, under its Main and Auxiliary tabs (a walk has no strip)
     :hint "enter select  ←→ main/auxiliary/decider/backup  ctrl+e new key  esc close  ↑↓ navigate"
     ;; a walk's Esc moves it on rather than closing it
     :walk-hint "enter select  ctrl+e new key  esc skip  ←→ steps  ↑↓ navigate  type search"
     :empty-label "Other provider" :no-match-label "No matching providers")
    (:custom-provider
     :context "Provider id" :input-label "Provider id"
     :subtitle "Enter a models.dev provider id."
     :hint "enter continue  esc back  ctrl+u clear  type provider id"
     :empty-label "Provider id must be lowercase letters, numbers, hyphen, or underscore.")
    (:address
     ;; the input label is the blank's name, and the line under the field the
     ;; address it goes in: CONNECT-ENTER-STEP writes both
     :context "Address" :input-label ""
     ;; a control string: CONNECT-ENTER-STEP fills in the provider and the blank
     :subtitle "~a's address needs ~a."
     :hint "enter continue  esc back  ctrl+u clear  type value"
     :empty-label "")
    (:api-key
     :context "API key" :input-label "API key" :input-secret-p t
     ;; a control string: CONNECT-ENTER-STEP fills in the provider and the
     ;; page that makes its key
     :subtitle "Paste your ~a API key.~@[ ~a makes one.~]"
     :hint "enter save  esc back  ctrl+u clear  type secret"
     :empty-label "No secret entered")
    (:checking
     :context "API key"
     ;; a control string: CONNECT-ENTER-STEP fills in the provider and the
     ;; seconds PROVIDER-KEY-CHECK waits
     :subtitle "Asking ~a whether it takes this key, waiting up to ~d seconds."
     :hint "esc back"
     :empty-label "")
    (:installing
     :context "Install"
     ;; a control string: CONNECT-INSTALL fills in the folder and its pin
     :subtitle "Installing ~a~@[ at ~a~], the cell that serves this provider."
     :hint "esc back"
     :empty-label "")
    (:starting
     :context "Start"
     ;; a control string: CONNECT-AWAIT-START fills in the folder
     :subtitle "Starting ~a, the cell that serves this provider."
     :hint "esc back"
     :empty-label "")
    (:signing-in
     :context "Sign in"
     ;; none: the panel names the provider under the page already
     :subtitle ""
     :hint "esc back"
     :empty-label "")
    (:result
     :context "Errors"
     :subtitle "Credential setup needs attention."
     :hint "enter retry  esc back"
     :empty-label ""))
  "The Zig connectDialogView switch, one row per step, keyed by the PICKER
slot each string lands in: the muted context label beside the Connect tab
(connectHeaderStepLabel), the summary the header's third row carries
wherever the step is not searching, the footer copy, the body's empty /
no-match labels, and the input mode's field label and masking on the two
text steps.")

(defparameter +connect-waiting-steps+ '(:checking :installing :starting :signing-in)
  "The steps that wait on the organism -- a key's check, a folder's install,
its cell's start, its sign-in: no field and no list, a clock counting from the
step's start, Esc back, every other key the step's own.")

(defun connect-waiting-step-p (step)
  "Whether STEP, a +CONNECT-STEPS+ key, waits on the organism (+CONNECT-WAITING-STEPS+)."
  (and (member step +connect-waiting-steps+) t))

(defun connect-enter-step (dialog step &aux (spec (rest (assoc step +connect-steps+))))
  "Move DIALOG onto STEP and paint the chrome the step owns onto the picker
slots the panel already reads."
  ;; STEP names a +CONNECT-STEPS+ row, so the two result flavours say which
  ;; they are (:RESULT-OK / :RESULT-ERROR) and both land on the single :RESULT
  ;; the flow tests. Writing the chrome here — rather than deriving it at
  ;; every paint — is what keeps one panel shape: past this line the wizard is
  ;; an ordinary picker whose header and footer move.
  (setf (connect-dialog-step dialog) step)
  (dolist (slot '(searchable context subtitle hint empty-label no-match-label
                  input-label input-secret-p))
    (setf (slot-value dialog slot) (getf spec (intern (string slot) :keyword))))
  ;; The sign-in step has no summary: its header is the title row and a
  ;; blank, so the clock sits one blank under the title, not three.
  (setf dialog.header-rows (and (eq step :signing-in) 2))
  (when-let (walk (connect-dialog-walk dialog))
    (setf dialog.context walk
          dialog.hint (or (getf spec :walk-hint) dialog.hint)))
  ;; The key step asks for the provider's key by name (`Paste your Google API
  ;; key.'), so the name no longer sits under the field.
  (when (eq step :api-key)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)
                                  (connect-dialog-key-page dialog))))
  ;; The address step asks for its blank by name, over the field that bears
  ;; the name.
  (when (eq step :address)
    (let ((blank (first (connect-dialog-blanks dialog))))
      (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog) blank)
            dialog.input-label blank)))
  ;; The check says how long it waits and counts up to it, as the import's
  ;; does: a static `checking...' would read as a hang. The tick repaints
  ;; when the tenth shown moves.
  (setf (connect-dialog-checking-since dialog) (and (connect-waiting-step-p step)
                                                    (get-internal-real-time))
        dialog.on-tick (and (connect-waiting-step-p step)
                            (let ((shown nil))
                              (lambda (model dialog &aux (now (connect-checking-clock dialog)))
                                (declare (ignore model))
                                (unless (equal now shown)
                                  (setf shown now))))))
  (when (eq step :checking)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)
                                  *provider-models-fetch-timeout-seconds*)))
  ;; The two text steps are the picker's input mode (a step with no
  ;; :INPUT-LABEL leaves it): the id step reopens on the id a failed save
  ;; returned from, under its rule line; the key step starts empty, under
  ;; the provider it is for.
  ;; The address step starts on the value the blank has now -- saved with
  ;; the key, or the env var's (PROVIDER-URL-VALUE) -- so Enter keeps it, and
  ;; shows under the field the address it goes in.
  (setf dialog.input (case step
                       (:custom-provider (if (connect-dialog-custom-p dialog)
                                             (connect-dialog-provider-id dialog)
                                             ""))
                       (:address (or (provider-url-value (connect-dialog-provider-id dialog)
                                                         (first (connect-dialog-blanks dialog)))
                                     ""))
                       (t ""))
        dialog.input-context (case step
                               (:api-key "")
                               (:address (or (catalog-address-template (connect-dialog-provider-id dialog)) ""))
                               (t (getf spec :empty-label)))))

(defun connect-checking-clock (dialog &aux (since (connect-dialog-checking-since dialog)))
  "`checking… 1.3s', the checking step's body line, counting from when the
step began; `installing… 1.3s' on the installing step, `starting… 1.3s'
while its cell starts, `waiting for the sign-in… 1.3s' on the sign-in step."
  (format nil "~a… ~a" (case (connect-dialog-step dialog)
                         (:installing "installing")
                         (:starting "starting")
                         (:signing-in "waiting for the sign-in")
                         (t "checking"))
          (format-turn-elapsed (if since (since-ms since) 0))))

(defun connect-provider-name (dialog)
  "The provider as the wizard names it: the catalog label, else the id."
  (or (json-value (connect-dialog-provider-label dialog) :text)
      (connect-dialog-provider-id dialog)))

(defun connect-clear-credential (dialog)
  "Drop everything the flow has gathered about a provider."
  ;; Backing out to the list and picking a fresh entry both start from
  ;; nothing, and the secret goes with the rest (the Zig zero-on-clear
  ;; discipline).
  (setf (connect-dialog-provider-id dialog) ""
        (connect-dialog-provider-label dialog) ""
        (connect-dialog-custom-p dialog) nil
        (connect-dialog-key-page dialog) nil
        (connect-dialog-retry dialog) nil
        (connect-dialog-blanks dialog) '()
        (connect-dialog-blank-values dialog) '()
        dialog.input ""))

(defun connect-ask-blanks-or-key (dialog &aux (id (connect-dialog-provider-id dialog)))
  "Move DIALOG on to its provider's key, through an address step for each
${NAME} blank of the provider's catalog address first, when that address is
the one it dials (CATALOG-ADDRESS-TEMPLATE)."
  ;; Cloudflare Workers AI's address carries the account: without its step,
  ;; the key was checked at, saved for and dialed on the literal
  ;; .../accounts/${CLOUDFLARE_ACCOUNT_ID}/ai/v1 (Elektra, 2026-11-01).
  (setf (connect-dialog-blanks dialog) (when-let (template (catalog-address-template id))
                                         (url-blank-names template))
        (connect-dialog-blank-values dialog) '())
  (connect-enter-step dialog (if (connect-dialog-blanks dialog) :address :api-key)))

(defun connect-ask-key (dialog row &aux (value (and row (picker-row-value row))))
  "Move DIALOG from the provider list to what ROW asks for: Other
provider's id, a provider's key. Any other row moves nothing."
  (when (or (getf value :custom) (getf value :provider))
    (connect-clear-credential dialog)
    (if (getf value :custom)
        (setf (connect-dialog-custom-p dialog) t)
        (setf (connect-dialog-provider-id dialog) (getf value :provider)
              (connect-dialog-provider-label dialog) (picker-row-label row)
              (connect-dialog-key-page dialog) (getf value :key-page)))
    (if (getf value :custom)
        (connect-enter-step dialog :custom-provider)
        (connect-ask-blanks-or-key dialog))))

(defun connect-install (model dialog row &key key &aux (value (picker-row-value row))
                                                     (offer (getf value :install))
                                                     (id (getf value :provider)))
  "Enter on ROW, a provider a folder not installed here serves
(PICKER-PROVIDER-OFFERS): install that folder through the organism, off the
loop (INSTALL-OFFERED-CELL), then go on as the provider asks -- the command
its folder signs in with, a pick when it answers already (a key it has, or a
login the folder brings: Claude Code's is the claude CLI's), else its key
step. A folder in already whose cell has not started yet is waited on first
(CONNECT-AWAIT-START). KEY, Ctrl+E's, asks for a key whatever the folder
signs in with. A refused install says why over the list, and nothing else
moved."
  (connect-clear-credential dialog)
  (setf (connect-dialog-provider-id dialog) id
        (connect-dialog-provider-label dialog) (picker-row-label row))
  (labels ((go-on (model commands)
             ;; the folder is in: on as the provider asks, once its cell is up
             (when commands (set-gateway-commands commands))
             (if (and (not key) (getf offer :starting))
                 (connect-await-start model dialog (getf offer :cell) #'ask)
                 (ask model)))
           (ask (model &aux (how (getf offer :sign-in)))
             (setf dialog.rows (funcall (connect-dialog-provider-rows dialog)))
             (cond
               ;; the folder signs in with a command of its own:
               ;; it runs, and the wizard waits on its words
               ((and (not key) (uiop:string-prefix-p "/" how))
                (connect-sign-in model dialog how))
               ;; it answers already -- a key it had, or a login
               ;; its folder brings, which the reread rows say: a
               ;; pick, as Enter on a connected provider is
               ((and (not key)
                     (or (getf value :connected)
                         (find-if (lambda (row &aux (now (picker-row-value row)))
                                    (and (equal id (getf now :provider))
                                         (getf now :connected)))
                                  dialog.rows)))
                (let ((on-success (connect-dialog-on-success dialog)))
                  (dismiss-dialog model (and on-success
                                             (lambda (model) (funcall on-success model id))))))
               (t (connect-ask-key dialog row)))))
    (if (getf offer :installed)
        ;; in already, its provider not answering yet -- a sign-in Esc left
        ;; unfinished, or a cell the organism has not started yet: nothing
        ;; to install, on to it
        (go-on model nil)
        (progn
          (connect-enter-step dialog :installing)
          (setf dialog.subtitle (format nil dialog.subtitle (getf offer :cell)
                                        (let ((pin (getf offer :commit)))
                                          (and (stringp pin) (subseq pin 0 (min 7 (length pin)))))))
          (off-loop model
                    (lambda () (install-offered-cell model offer))
                    (lambda (model answer)
                      ;; Esc, or another dialog, moved the wizard on: the answer moves nothing
                      (if (not (and (eq dialog (chat-model-dialog model))
                                    (eq :installing (connect-dialog-step dialog))))
                          :quiet
                          (let ((refused (or (off-loop-failure answer) (getf answer :refused))))
                            (if refused
                                (progn
                                  (connect-clear-credential dialog)
                                  (connect-enter-step dialog :provider)
                                  (show-status model (format nil "~a was not installed: ~a" (getf offer :cell) refused)
                                               :error-p t))
                                (go-on model (getf answer :commands)))
                            (render-state model))))))))
  t)

(defun sign-in-facts (text &aux (text (or text "")))
  "The page TEXT, what a folder's sign-in command answered, sends the
operator to, and the device code it asks for there when it names one: =>
(values URL CODE), either NIL."
  ;; The code is the word after `code' that reads as one -- a digit in it, or
  ;; capitals around a dash: `enter the code ABCD-1234' or `NFGT-NHBJ', never
  ;; `send /openai-codex code ADDRESS' nor `Kimi Code sign-in'.
  (let ((words (loop with start = nil
                     for i from 0 to (length text)
                     for blank = (or (= i (length text))
                                     (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
                     if (and blank start) collect (subseq text start i) and do (setf start nil)
                     else if (and (not blank) (null start)) do (setf start i))))
    (values (let ((url (find-if (lambda (word &aux (host (search "://" word)))
                                  ;; a loopback address is where the browser
                                  ;; comes back to, never where to sign in
                                  (and host (< host 6)
                                       (member (subseq word 0 host) '("http" "https") :test #'string-equal)
                                       (notany (lambda (loopback)
                                                 (uiop:string-prefix-p loopback (subseq word (+ host 3))))
                                               '("localhost" "127." "[::1]"))))
                                words)))
              (and url (string-right-trim ".,;)" url)))
            (loop for (word next) on words
                  for code = (and next (string-trim ".,;:()'\"" next))
                  when (and code (string-equal (string-trim ":" word) "code")
                            (<= 4 (length code) 16)
                            (every (lambda (c) (or (alphanumericp c) (char= c #\-))) code)
                            (or (some #'digit-char-p code)
                                (and (find #\- code) (notany #'lower-case-p code))))
                    return code))))

(defun sign-in-page-label (url &aux (from (+ 3 (or (search "://" url) -3))))
  "URL as the sign-in panel names it: no scheme, no query."
  (subseq url from (or (position-if (lambda (c) (member c '(#\? #\#))) url :start from)
                       (length url))))

(defparameter *connect-sign-in-poll-seconds* 2
  "How long CONNECT-SIGN-IN waits between two reads of whether the provider
answers yet.")

(defparameter *connect-start-poll-seconds* 0.5
  "How long CONNECT-AWAIT-START waits between two reads of whether the cell
has started.")

(defun connect-await-start (model dialog cell then &aux (id (connect-dialog-provider-id dialog)))
  "Wait on DIALOG while CELL, the folder serving its provider, in already,
starts in the organism, whose cold boot starts its cells seconds after its
first list is read (START-CELLS-BEHIND-THE-LISTENER): once the gateway's list
reads the provider connected, the pick goes on to its models, as Enter on a
connected provider does; once the cell is up and the provider still does not
answer, THEN, a function of the model, goes on as the provider asks. Esc goes
back to the list (CONNECT-BACK)."
  ;; Enter on Claude Code's row in those seconds asked for an API key, which
  ;; its login -- the claude CLI's -- never takes (tryingET, 2026-10-30), and
  ;; a folder that signs in with a command of its own would have run one its
  ;; cell had not registered yet. Each read lands on the loop, which decides
  ;; whether to read again: the worker never looks at the dialog.
  (labels ((ours (model) (and (eq dialog (chat-model-dialog model))
                              (eq :starting (connect-dialog-step dialog))))
           (wait (model)
             (off-loop model
                       (lambda ()
                         (sleep *connect-start-poll-seconds*)
                         (fetch-gateway-models model)
                         (cond ((member id (picker-connected-provider-ids) :test #'string-equal)
                                :connected)
                               ((find-if (lambda (offer)
                                           (and (string-equal id (getf offer :provider))
                                                (getf offer :starting)))
                                         (picker-provider-offers))
                                :starting)
                               (t :started)))
                       (lambda (model state)
                         (cond ((not (ours model)) :quiet)
                               ((eq state :connected)
                                (let ((on-success (connect-dialog-on-success dialog)))
                                  (dismiss-dialog model (and on-success
                                                             (lambda (model) (funcall on-success model id)))))
                                (render-state model))
                               ;; up, and its provider not answering: on as it asks
                               ((eq state :started)
                                (funcall then model)
                                (render-state model))
                               ;; still starting, or a read that failed: read again
                               (t (wait model) :quiet))))))
    (connect-enter-step dialog :starting)
    (setf dialog.subtitle (format nil dialog.subtitle cell))
    (wait model)))

(defun connect-sign-in (model dialog how &aux (id (connect-dialog-provider-id dialog)))
  "Run HOW, the command a folder just installed signs in with, and wait on
DIALOG for its provider to answer: what the command said -- the address to
open, a device code -- stands in the panel, and once the gateway's list reads
the provider connected the pick goes on to its models, as Enter on a
connected provider does. Esc goes back to the list (CONNECT-BACK), the
folder's sign-in still waiting without it."
  ;; Each read lands on the loop, which decides whether to read again: the
  ;; worker never looks at the dialog.
  (labels ((ours (model) (and (eq dialog (chat-model-dialog model))
                              (eq :signing-in (connect-dialog-step dialog))))
           (wait (model)
             (off-loop model
                       (lambda ()
                         (sleep *connect-sign-in-poll-seconds*)
                         (fetch-gateway-models model)
                         (and (member id (picker-connected-provider-ids) :test #'string-equal) t))
                       (lambda (model connected)
                         (cond ((not (ours model)) :quiet)
                               ((eq t connected)
                                (let ((on-success (connect-dialog-on-success dialog)))
                                  (dismiss-dialog model (and on-success
                                                             (lambda (model) (funcall on-success model id)))))
                                (render-state model))
                               (t (wait model) :quiet))))))
    (connect-enter-step dialog :signing-in)
    (setf dialog.subtitle (format nil dialog.subtitle (connect-provider-name dialog)))
    (off-loop model
              (lambda () (json-value (gateway-slash-answer model how) :string "text"))
              (lambda (model text &aux (failure (off-loop-failure text))
                                       (url (and (not failure) (sign-in-facts text))))
                (cond ((not (ours model)) :quiet)
                      ;; no page to sign in at: the command said why not --
                      ;; over the list, as a refused install says it
                      ((null url)
                       (setf (connect-dialog-result-message dialog) "")
                       (connect-clear-credential dialog)
                       (connect-enter-step dialog :provider)
                       (show-status model (if failure (format nil "~a: ~a" how failure) (or text how))
                                    :error-p t)
                       (render-state model))
                      (t (setf (connect-dialog-result-message dialog) text)
                         ;; the page opens where there is a browser to open it
                         (ignore-errors (open-target url))
                         (wait model)
                         (render-state model)))))))

(defun connect-back (model dialog)
  "One step back (the Zig backConnectDialog): from the provider step the
flow closes; either input step drops the half-gathered credential and
returns to the list; the result -- a failure's -- returns to the step it
interrupted, which is the custom-id step exactly when the id was typed
rather than picked, or to the list when nothing listened where the key was
checked."
  ;; Closing runs the dialog's
  ;; ON-CANCEL: a caller that chains the wizard — the setup walk — moves on by
  ;; it, from a saved key as from the list.
  (ecase (connect-dialog-step dialog)
    (:provider (dismiss-dialog model dialog.on-cancel))
    ((:custom-provider :address :api-key)
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider)
     (when (eq :model-providers (picker-dialog-kind dialog))
       (install-frequent-models model dialog
                                (json-object "models" (gethash dialog *models-frequent-by-dialog* #())))))
    ;; the answer, when it lands, finds the dialog moved on and takes nothing
    (:checking (connect-enter-step dialog :api-key))
    ;; the install goes on in the organism, or the cell's start; its answer
    ;; finds the list
    ((:installing :starting)
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider))
    ;; the folder's sign-in goes on waiting; the list takes nothing from it
    (:signing-in
     (setf (connect-dialog-result-message dialog) "")
     (connect-clear-credential dialog)
     (connect-enter-step dialog :provider))
    (:result
     (setf (connect-dialog-result-message dialog) "")
     (cond
       ;; nothing listened where the key was checked: another key would meet
       ;; the same stopped server, so Esc goes back to the providers
       ((connect-dialog-retry dialog)
        (connect-clear-credential dialog)
        (connect-enter-step dialog :provider))
       (t (connect-enter-step dialog (if (connect-dialog-custom-p dialog) :custom-provider :api-key)))))))

(defun connect-dialog-key (model dialog key &key ctrl alt &aux (step (connect-dialog-step dialog)))
  "The keys the /models wizard takes away from the shared picker ladder
(runtime.zig handleConnectDialogKey): Esc steps back through the flow
instead of closing it, Enter advances the step instead of selecting a row,
and past the provider list every key belongs to the step, because no list is
showing for one to mean anything else."
  ;; => T when the wizard owned the key;
  ;; NIL hands it down to the picker ladder, where the provider list's navigation
  ;; and type-to-filter already live and need no second copy here.
  (cond
    ((and (eq key :escape)
          (or (not (eq step :provider))
              (zerop (length (connect-dialog-search dialog)))))
     (connect-back model dialog)
     t)
    ((and (eq step :provider) ctrl (characterp key) (char-equal key #\e))
     (let ((row (selected-of (picker-visible-rows dialog) (connect-dialog-selection dialog))))
       ;; a provider no installed folder serves yet is installed first
       (if (getf (and row (picker-row-value row)) :install)
           (connect-install model dialog row :key t)
           (connect-ask-key dialog row)))
     t)
    ((eq key :enter)
     ;; The Zig submit + persist ladder; a failed save is the result step's message.
     (ecase step
       (:provider
        (let* ((row (selected-of (picker-visible-rows dialog)
                                 (connect-dialog-selection dialog)))
               (value (and row (picker-row-value row)))
               (on-success (connect-dialog-on-success dialog)))
          (flet ((pick (choice)
                   (dismiss-dialog model (and on-success
                                              (lambda (model) (funcall on-success model choice))))))
            (cond ((getf value :model) (pick (getf value :model)))
                  ;; A provider a folder not installed here serves: the
                  ;; folder first, then what the provider asks for.
                  ((getf value :install) (connect-install model dialog row))
                  ;; A provider whose key answers is a pick, not a key to paste
                  ;; again: Enter picks it. Ctrl+E pastes a new one.
                  ((getf value :connected) (pick (getf value :provider)))
                  (t (connect-ask-key dialog row))))))
       (:custom-provider
        (let ((input dialog.input))
          ;; The Zig validProviderId rule; surrounding whitespace rejects, never trims.
          (when (ppcre:scan "^[a-z0-9][a-z0-9_-]*\\z" input)
            (setf (connect-dialog-provider-id dialog) input
                  (connect-dialog-provider-label dialog) input
                  (connect-dialog-custom-p dialog) t)
            ;; A provider no lane drives is refused before a key is asked
            ;; for: nothing could use the secret. Esc returns to the id.
            (if-let (refusal (provider-id-refusal input))
              (progn (setf (connect-dialog-result-message dialog)
                           (format nil "~a Nothing was saved." refusal))
                     (connect-enter-step dialog :result))
              (connect-ask-blanks-or-key dialog)))))
       (:address
        ;; The value goes into an address: an empty one fills nothing and one
        ;; with a space in it is no part of one, so neither moves on.
        (let ((value (nlk:trimmed dialog.input)))
          (when (and (plusp (length value)) (not (ppcre:scan "\\s" value)))
            (push (cons (pop (connect-dialog-blanks dialog)) value)
                  (connect-dialog-blank-values dialog))
            (connect-enter-step dialog (if (connect-dialog-blanks dialog) :address :api-key)))))
       (:api-key
        ;; The sink answers DONE once the provider has had its say on the
        ;; key, off the loop in a live shell; the step waits on it.
        (let ((on-save (connect-dialog-on-save dialog))
              (secret dialog.input))
          (setf dialog.input ""
                (connect-dialog-retry dialog) nil)
          (connect-enter-step dialog :checking)
          ;; A saved key needs no screen of its own: its line is the
          ;; notice over the list, rebuilt so the provider reads
          ;; connected, Suggested, and selected.
          (flet ((done (model message error-p)
                   (when (and (eq dialog (chat-model-dialog model))
                              (eq :checking (connect-dialog-step dialog)))
                     (cond (error-p
                            (setf (connect-dialog-result-message dialog) (or message ""))
                            (connect-enter-step dialog :result)
                            ;; nothing listened where the key was checked: the
                            ;; result keeps it, and Enter checks it again
                            (when (eq error-p :retry)
                              (setf (connect-dialog-retry dialog) secret
                                    dialog.hint "enter check again  esc other provider")))
                           (t
                            (show-status model message)
                            (connect-clear-credential dialog)
                            (setf dialog.rows (funcall (connect-dialog-provider-rows dialog))
                                  dialog.search "")
                            (picker-seat-top dialog)
                            (connect-enter-step dialog :provider)
                            ;; The answer lands off the loop, where no key's
                            ;; update repaints the band the notice joins.
                            (when (chat-model-band model)
                              (render-state model)))))))
            ;; the address step's values ride along, for the check and the save
            (if on-save
                (apply on-save model (connect-dialog-provider-id dialog) secret #'done
                       (when-let (vars (connect-dialog-blank-values dialog))
                         (list :vars (reverse vars))))
                (done model "no credential sink is wired" t)))))
       ;; a step that waits on the organism takes no Enter
       ((:checking :installing :starting :signing-in) nil)
       ;; a check nothing listened for is checked again, the same key
       (:result (if-let (secret (connect-dialog-retry dialog))
                  (progn (connect-enter-step dialog :api-key)
                         (setf dialog.input secret)
                         (connect-dialog-key model dialog :enter))
                  (connect-back model dialog))))
     t)
    ;; The two input steps hand every other key down to the picker's
    ;; input arm, which edits the field uncapped — a provider id or an
    ;; API key is not a search query, and Zig caps neither.
    ;; The result step has no field of its own: a stray character is
    ;; swallowed exactly as Zig swallows it, and the edit keys still reach
    ;; the provider query underneath, which is what backing out returns to.
    ((eq step :result)
     (unless (and (characterp key) (not ctrl) (not alt)
                  (graphic-char-p key))
       (when-let (new (edit-text-field (connect-dialog-search dialog) key
                                       :ctrl ctrl :alt alt))
         (picker-search-mutate dialog new)))
     t)))

(defparameter +loading-providers+ "Loading providers from models.dev…"
  "The provider list's search row while its first catalog fetch runs.")

(defun open-connect-dialog (model rows on-save on-success &key (title "Connect") kind)
  "Open the /models wizard over MODEL."
  ;; ROWS, a function of no arguments, answers the provider picker rows, at
  ;; open and after each saved key; ON-SAVE persists an entered key;
  ;; ON-SUCCESS is what Enter on a connected provider opens. A box
  ;; without the models.dev cache fetches it behind the list
  ;; (INSTALL-FETCHED-CATALOG), and the search row says so meanwhile: the list
  ;; holds the Other-provider entry alone until it lands.
  (let ((dialog (make-connect-dialog :title title :kind kind :rows (funcall rows) :provider-rows rows
                                     :search-placeholder (if (start-models-catalog-fetch model)
                                                             +loading-providers+
                                                             "Search providers")
                                     :on-save on-save
                                     :on-success on-success)))
    ;; The provider step's own chrome is not a defstruct default: it is the
    ;; first thing the flow enters, through the one seam every later step
    ;; enters through.
    (connect-enter-step dialog :provider)
    (picker-seat-top dialog)
    (setf (chat-model-dialog model) dialog))
  nil)

;;; The slash dropdown's rows render through the shared composer dropdown
;;; panel above (COMPOSER-DROPDOWN-PANEL-ROWS): one Zig row shape for every
;;; completion lane.

;;; --- help dialog -------------------------------------------------------------