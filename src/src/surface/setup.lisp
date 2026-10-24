;;;; setup.lisp --- setup: discovery, provider, model, effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; First launch, `nodecode setup' and /setup show discovery work, then
;;;; provider/model/effort through the existing /models and /think
;;;; dialogs. A cell is turned on in /cells and set in /settings, both over
;;;; the organism's routes; `nodecode import' walks its requested import alone.
;;;; Every write goes through the writer that already owns it: the import
;;;; cell, SAVE-PROVIDER-API-KEY, the default model pick and APPLY-EFFORT.
;;;; Escape during discovery cancels it; later steps let the operator go back
;;;; or finish later.
;;;;
;;;; Opens the three-step first-launch walk while a provider or saved model
;;;; is missing (SETUP-REQUIRED-P). Discovery notices are live-only; only
;;;; what the existing writers write survives the shell's replay.

(in-package #:nodecode.evolved)

(defstruct (setup-run (:copier nil))
  "What one walk of the wizard did, for the summary line."
  ;; The providers this walk connected, newest first.
  (providers '() :type list)
  (model nil)
  ;; (PICKED REASON): the pick the shared config did not take, as it was made,
  ;; and why (SETUP-RECORD-MODEL). The summary says it was not saved where the
  ;; model would stand.
  (unsaved nil)
  (notes '() :type list)
  ;; The notes that qualify what the walk did -- bots held for a gateway still
  ;; running, a step that failed -- newest first. A provider's key that did
  ;; not answer its check is not one: *KEY-CHECKS* says it (SETUP-CAVEATS).
  (caveats '() :type list)
  ;; (PROVIDER . ANSWER): what a provider said when the import's check of its
  ;; key was refused -- `HTTP 401: Incorrect API key provided' -- for the
  ;; closing line, which quotes it.
  (answers '() :type list)
  ;; Whether an import landed something this walk: the stages after it ask
  ;; only what the import could not answer.
  (migrated nil)
  ;; What the walk's import answered so far: the keys' apply, then the rest's
  ;; folded in when it lands (MERGED-IMPORT-VALUE) -- what the marker records.
  (import nil)
  ;; The rest's apply, which runs beside the keys': :LANDED once the keys'
  ;; answer is taken, else (VALUE) when the rest answered first -- held until
  ;; it is, since the rest's count folds into the keys' (SETUP-IMPORT-REST).
  (behind nil)
  ;; The harnesses whose sign-in the import had to leave behind — an OAuth
  ;; grant is that harness's own — so the provider stage can say why it
  ;; is not Suggested.
  (grants '() :type list)
  ;; The setup walk's numbered steps: provider/model/effort — for the
  ;; `step 2/3' each dialog's context carries.
  ;; NIL numbers nothing (`nodecode import codex' walks one stage).
  (steps '() :type list)
  ;; What the walk found, in the order it was found: one (KEY MARK LABEL TEXT)
  ;; per line, a later line under the same KEY in its place. The walk's
  ;; panels and its last page show it (SETUP-LINE-ROWS); the transcript never
  ;; does, so the chat opens on nothing the walk said.
  (ledger '() :type list)
  ;; The import answer the Details page lists item by item: the plan a
  ;; question is asked over, or what the applies landed, the rest folded in.
  (report nil))

(defun provider-connected-p ()
  "Whether any provider resolves a credential from anywhere: a config entry
carrying credentials, an auth.json entry, or the default lane's
environment."
  ;; A default provider no lane serves answers T: that is an operator with a
  ;; config to fix (the first frame's refusal names the key), not a fresh box.
  (or (and (configured-provider-inventory) t)
      (handler-case (not (eq :none (provider-auth-state *provider*))) (error () t))))

(defun setup-required-p ()
  "Whether first launch needs a provider or a saved default model."
  (and *shared-config-path*
       (or (not (provider-connected-p))
           (not (ignore-errors (default-model-configured-p))))
       t))

(defmethod nlt:init :after ((model chat-model))
  "The first frame of a live shell opens the setup wizard when `setup' asked
for it or when the provider/default model is missing, and paints it in."
  ;; A model without a program — a test's, a projection's — never opens it
  ;; unasked.
  (when (and model.program
             (or model.setup-requested (setup-required-p)))
    (slash-setup-op model)
    (render-state model)))
(defun slash-import-op (model args &aux (words (split-words (or args ""))))
  "/import in a shell: the setup walk's import stage over the request ARGS
name, as `nodecode import' opens it -- the plan read off the loop with its
progress, a preview to confirm unless --yes. `--sources' and `--remove LABEL'
are the organism's lists, answered as text."
  (with-slash-notice ("/import")
    (if (member (first words) '("--sources" "--remove") :test #'string=)
        (json-value (gateway-slash-answer model (format nil "/import ~a" args)) :string "text")
        (multiple-value-bind (request rest) (import-request words)
          (when rest
            (error "unknown word ~a; usage ~a" (first rest)
                   (slash-command-usage (find-builtin-slash-command "import"))))
          (setf model.setup-requested request)
          (slash-setup-op model)
          nil))))

(defun headless-import (args session-id)
  "/import where no shell walks it -- a room, the page: the summary and where
the report is, as text (NIK:RUN-SLASH)."
  (declare (ignore session-id))
  (nik:run-slash args))

(defun slash-setup-op (model)
  "/setup, nodecode setup and first launch share discovery/provider/model/effort.
The explicit nodecode import request walks only the import stage."
  (let* ((requested model.setup-requested)
         (request (and (consp requested) (getf requested :import) requested))
         (run (make-setup-run :steps (unless request '(:provider :model :effort)))))
    ;; Launch intent belongs to this walk, never a later /setup invocation.
    (setf model.setup-requested nil
          model.setup-walk run)
    (forget-setup-issues model)
    (if (null request)
        ;; Paint before reading another harness's home: detection runs off the
        ;; loop, and a home it finds is a question (SETUP-IMPORT-OFFER) before
        ;; a file in it is opened.
        (setup-import-wait
         model run "Looking for other coding agents…"
         (lambda () (offered-import-worlds))
         (lambda (model worlds)
           (cond ((off-loop-failure worlds)
                  (setup-caveat run "import: ~a" (off-loop-failure worlds))
                  (setup-import-report run (make-json-object))
                  (setup-stage-provider model run))
                 (worlds (setup-import-offer model run worlds))
                 (t (setup-import-report run (json-object "items" #()))
                    (setup-stage-provider model run)))))
        (let ((next (lambda (model) (setup-stage-finish model run)))
              (subtitle "Loading the import folder and reading the home…"))
          (if (and (getf request :yes) (not (getf request :dry-run)))
              (setup-import-apply model run request (request-takeover request) next subtitle)
              (setup-import-wait
               model run subtitle
               (lambda ()
                 (setup-import-call model "plan" request (request-takeover request)))
               (lambda (model value)
                 (cond
                   ((and (consp value) (eq (first value) :error))
                    (setup-caveat run "import: ~a" (second value))
                    (funcall next model))
                   ((getf request :dry-run)
                    (setup-import-report run value)
                    (setup-note run "import (dry run): ~a" (import-summary-line value))
                    (funcall next model))
                   (t (setup-import-confirm model run request value next)))))))))
  nil)
(defun setup-note (run control &rest arguments)
  "One note for RUN's closing summary: CONTROL formatted with ARGUMENTS."
  (push (apply #'format nil control arguments) (setup-run-notes run)))

(defun setup-caveat (run control &rest arguments)
  "One caveat for RUN's closing summary — something the operator has to
check before trusting what the walk did: CONTROL formatted with ARGUMENTS."
  (push (apply #'format nil control arguments) (setup-run-caveats run)))

(defun setup-record-model (run picked)
  "Put PICKED, the (PROVIDER MODEL) or (MODEL) the /models picker named, on
RUN as its model when the shared config now holds it; when it does not, on RUN
as unsaved, with why."
  ;; The closing block names what the config holds, read back, and not what the
  ;; walk chose. Without this, a pick whose write failed would move this
  ;; process's model and leave the file as it was, and the summary would name a
  ;; model the next launch would not find.
  (multiple-value-bind (provider model) (shared-model-pin)
    (if (and model
             (equal model (first (last picked)))
             (or (null (rest picked)) (string-equal provider (first picked))))
        (setf (setup-run-model run) (list provider model)
              (setup-run-unsaved run) nil)
        (setf (setup-run-unsaved run)
              (list picked
                    (if-let (phrase (shared-config-refusal))
                      (format nil "~a; fix it or move it aside, then pick the model again with /models"
                              phrase)
                      (format nil "~a does not hold the model; pick it again with /models"
                              (home-abbreviated
                               (uiop:native-namestring (or *shared-config-path* "config.jsonc"))))))))))

(defun setup-context (run stage words &aux (steps (setup-run-steps run))
                                           (at (position stage steps)))
  "WORDS, a dialog's context label, then where STAGE sits in RUN's walk —
`Setup · step 2/3' — or WORDS alone when the walk numbers nothing."
  (if at
      (format nil "~a · step ~d/~d" words (1+ at) (length steps))
      words))

;;; --- moving between steps ------------------------------------------------------
;;; Left and Right move the walk a step, back or on, from each step's own
;;; dialog: the providers, the model, the effort. The import runs once and
;;; is not walked back into.

(defun setup-step-arrows (run stage)
  "STAGE's Left and Right: open the walk's step before it or after it."
  ;; The model step has nothing to pick before a provider is connected, so
  ;; the arrows pass over it then. The connect list's key and id steps are
  ;; text fields: the arrows move the walk only from its list.
  (lambda (model dialog key &aux (delta (if (eq key :left) -1 1))
                                 (steps (remove-if-not (lambda (step)
                                                         (case step
                                                           (:provider t)
                                                           (:model (and (connected-provider-ids) t))
                                                           (:effort (and (setup-run-model run) t))))
                                                       (setup-run-steps run)))
                                 (at (position stage steps))
                                 (index (and at (+ at delta)))
                                 (to (and index (< -1 index (length steps)) (nth index steps))))
    (when (and to (or (not (connect-dialog-p dialog)) (eq :provider (connect-dialog-step dialog))))
      (setf model.dialog nil)
      (ecase to
        (:provider (setup-stage-provider model run))
        (:model (setup-stage-model model run))
        (:effort (setup-stage-effort model run)))
      t)))

(defun setup-ledger-note (run entry &aux (key (first entry)))
  "ENTRY, a ledger line (KEY MARK LABEL TEXT), on RUN's ledger: in place of
the line its KEY already names, else after the rest. => the ledger, a fresh
list each time, so a panel tells by identity that it moved."
  (setf (setup-run-ledger run)
        (if (assoc key (setup-run-ledger run))
            (mapcar (lambda (line) (if (eql key (first line)) entry line)) (setup-run-ledger run))
            (append (setup-run-ledger run) (list entry)))))

(defun setup-ledger-lines (run)
  "RUN's ledger as (MARK LABEL TEXT) lines, in the order it was found."
  (mapcar #'rest (setup-run-ledger run)))

(defun setup-mark-tone (mark)
  "The tone a line's MARK paints in: done, to check, under way, else said."
  (cond ((equal mark "✓") :success)
        ((equal mark "!") :warn)
        ((equal mark "…") :dialog-accent)
        (t :muted)))

(defun setup-panel-width (model)
  "The columns a docked panel's row holds its text in at MODEL's width: the
frame less the panel's padding and content column."
  (max 20 (- (frame-width model) *docked-panel-pad-left* *docked-panel-pad-right*
             *docked-panel-content-col*)))

(defun setup-line-rows (lines width &aux (column (max 12 (- width 14))))
  "LINES, each (MARK LABEL TEXT), as a panel's summary rows at WIDTH: the mark
in its tone, the label in an 11-column lane, then the text, wrapped inside
its own column so a long note goes on under its text and not under the
label."
  ;; The block the walk ended on in the transcript read this way; the panel
  ;; keeps its columns (ca-109).
  (loop for (mark label text) in lines
        for tone = (setup-mark-tone mark)
        nconc (loop for piece in (or (loop for part in (split-lines (or text ""))
                                           nconc (wrapped-lines part column))
                                     (list ""))
                    for first = t then nil
                    collect (list (make-seg :text (if first mark " ") :tone tone)
                                  (make-seg :text (format nil " ~11a " (if first label ""))
                                            :tone :foreground)
                                  (make-seg :text piece :tone (if (eq tone :warn) :foreground :muted))))))

(defun setup-import-report (run value &optional (lines '(:keys :configs :memory)))
  "VALUE, an import's plan or answer, on RUN: what it located, LINES of it,
on a first launch's ledger, and its items for the Details page."
  ;; Retention: live-only, as the transcript rows it replaced were. What the
  ;; import wrote stays where its writers put it -- config.jsonc, auth.json,
  ;; the report under ~/.nodecode/import/.
  (when (json-value value :array "items")
    (setf (setup-run-report run) value))
  (when (setup-first-run-p run)
    (dolist (entry (setup-discovery-entries value lines))
      (setup-ledger-note run entry))))

;;; --- stage: import -----------------------------------------------------------
;;; The one stage before the provider. An operator arriving from another
;;; harness has a home full of state — keys, a persona, memory, skills,
;;; bots, jobs, conversations — and the cheapest first frame is the one
;;; that brings it over, so the provider stage after it opens on a provider
;;; connected and the stages that follow ask only what no home answers.
;;;
;;; Discovery costs one stat per world (NLK:DETECT-AGENT-WORLDS), then the
;;; import owner reads and brings over reusable artifacts. A blank box gets
;;; measured zero counts; an unreadable home says counts are unavailable.
;;; Imported keys suggest providers, but the first launch still asks the
;;; operator to choose provider, model and reasoning on every setup entrypoint.
;;; Cell configuration remains a separate /cells action.
;;; What is read and how each piece lands is the import's (src/src/import/),
;;; reached through the organism's route, POST /api/import.
;;;
;;; Unasked, it asks first: another harness's home holds the operator's
;;; keys, bot tokens and memory, and nothing in it is read before they say
;;; Import (SETUP-IMPORT-OFFER). The question names the agents detection
;;; found and costs no plan, so the first frame still waits on the keys
;;; alone once it is answered; with more than one, Choose which ticks them
;;; one by one. Skip, or an agent left unticked, is an answer the marker keeps. The
;;; report is the walk's Details page, an existing fact here always wins,
;;; and `/import <world>' re-runs any of it later. On request — `nodecode
;;; import <world>', whose flags ride the request — the plan's preview and
;;; its yes/no stand: that is somebody asking for this one import, and it
;;; deserves its whole plan before anything is written.

;;; NIL — the hermetic test posture — offers nothing unasked.
(define-startup-parameter *import-worlds* nlk:*agent-worlds*
  "The world manifests this stage probes for a past to bring over.")

;;; It records that run's outcome — what came over and which watched kinds it
;;; left behind — because the fact that an import ran is not the same as this
;;; home having what it came for. NIL leaves none.
(define-startup-parameter *import-marker* (nlk:home "import/imported")
  "The file this stage leaves after an import landed something, so the offer
is made once.")

;;; Only these can be asked about a second time. Memory, skills, sessions and
;;; the rest are invisible from here: nothing short of another plan says
;;; whether they came over, and a stage that cannot tell would offer itself
;;; forever.
(defparameter +import-watched-kinds+ '("providers" "channels")
  "The kinds whose absence this home can see for itself: a provider
credential — no provider resolving at all — and a channels section.")

(defun import-marker-record ()
  "The marker as a JSON object, or NIL where there is none or it does not read
as one."
  (and *import-marker* (nlk::json-object-file *import-marker*)))

(defun read-import-marker (&aux (record (import-marker-record)))
  "The marker's record: (values OUTSTANDING KNOWN)."
  ;; KNOWN is NIL for a marker whose outcome cannot be read — an older build
  ;; wrote a timestamp line and nothing else — and then the kinds it left
  ;; behind are not known from it.
  (when record
    (values (coerce (or (gethash "outstanding" record) #()) 'list) t)))

(defun write-import-marker (&rest members)
  "The marker with MEMBERS — key, value, ... — set and the rest kept; a NIL
value takes its member off."
  (apply #'nlk::update-json-file *import-marker* members))

(defun declined-import-worlds (&aux (record (import-marker-record)))
  "The worlds the operator said Skip to, by name, as the marker records them."
  (and record (coerce (or (gethash "declined" record) #()) 'list)))

(defun import-done-p ()
  "Whether an import has already brought this home what it came for — the
question the unasked offer turns on."
  ;; The marker alone does not answer it: So a marker that names what it left
  ;; behind is believed only while this home no longer lacks those kinds, and
  ;; a marker that names nothing has the home asked directly.
  (and *import-marker* (probe-file *import-marker*)
       ;; A record of declined worlds alone is no import at all: nothing came over.
       (let ((record (import-marker-record))) (or (null record) (gethash "imported" record)))
       (multiple-value-bind (outstanding known) (read-import-marker)
         ;; Still lacking each kind? A config that will not read answers no.
         (notany (lambda (kind)
                   (ignore-errors
                    (cond ((equal kind "providers") (not (provider-connected-p)))
                          ((equal kind "channels")
                           (multiple-value-bind (config error-text) (read-shared-config)
                             (and (null error-text)
                                  (not (nth-value 1 (nlk:json-value config :any "channels")))))))))
                 (if known outstanding +import-watched-kinds+)))))

(defun offered-import-worlds ()
  "The detected worlds the walk offers to import — every one on the box but
those the operator declined — or NIL once an import has brought this home
what it came for."
  ;; The one check the first frame, `setup' and /setup make. A Skip is an
  ;; answer: a declined world is never read again unasked, and `nodecode import
  ;; WORLD' or /import WORLD still offers it.
  ;; Detection is one stat per evidence path over the core table: no file is
  ;; opened and no folder is loaded to answer it, which is what lets a blank
  ;; box reach the cold wizard without paying for either.
  (when-let (found (and *import-worlds*
                        (mapcar (lambda (row) (nlk:agent-world-name (nlk:detected-world-world row)))
                                (ignore-errors (nlk:detect-agent-worlds :worlds *import-worlds*)))))
    (and (not (import-done-p))
         (remove-if (alexandria:rcurry #'member (declined-import-worlds) :test #'string-equal)
                    found))))

(defun await-owner (thunk &key (seconds 60) (pause 0.005) &aux (deadline (deadline-after seconds)))
  "THUNK's value, called again every PAUSE seconds while it signals the
gateway not served yet, for at most SECONDS."
  ;; The walk opens on a shell's first frame, which paints before that shell's
  ;; own gateway serves: a run posted then was answered `no gateway endpoint
  ;; is published yet', the walk moved on, and a fresh install's import did
  ;; nothing. A shell whose own organism died at boot has no
  ;; gateway coming: the walk hears that at once, in the boot's own line,
  ;; not after the whole wait (60 s of `please wait').
  ;; The pause is short: the first frame asks before its organism serves,
  ;; which it does tens of milliseconds later, and a quarter second slept
  ;; past that was a quarter of the walk's wait. It is the reconnector's own
  ;; lap: at 50 ms the walk asked up to 50 ms after the port was in the
  ;; shell's hands, a tenth of a keys import. A refused ask
  ;; before then dials nothing.
  (loop
    (handler-case (return (funcall thunk))
      (gateway-http-error (condition)
        (unless (and (eql 503 (gateway-http-error-status condition))
                     (equal "gateway_unreachable" (gateway-http-error-code condition))
                     (< (get-internal-real-time) deadline))
          (error condition))
        (when *organism-boot-failure*
          (error "~a" *organism-boot-failure*))
        (sleep pause)))))

(defun request-takeover (request)
  "Whether the import REQUEST takes a running foreign gateway's bots over:
the verb's default, which asks first; --keep-running leaves it running, and
so does the first frame's own offer, whose yes names no gateway."
  ;; Stopping and disabling somebody else's live service is not what a yes to
  ;; the first frame's offer gives: its bots land off and come on by
  ;; themselves once that gateway stops, and `nodecode import WORLD' moves them.
  (not (or (getf request :keep-running) (getf request :offered))))


(defun setup-import-call (model method request takeover &key progress)
  "Call the import authority; PROGRESS receives live-only item outcomes from
an apply response, whose terminal result is the same plan JSON as a preview."
  ;; The long methods always stream: a plan of a large home read for longer
  ;; than the shell's 30 s read in silence and failed as an SBCL I/O timeout
  ;; (T-350); the stream's pings carry it. `verify' is one small request.
  (let ((import (getf request :import))
        (stream (and (member method '("plan" "apply" "scan") :test #'equal) t)))
    (await-owner
     (lambda ()
       (let ((response
               (gateway-model-http-call
                model :post "/api/import"
                (make-json-object
                 "method" method
                 "source" (or (getf request :source)
                              (if (consp import) (coerce import 'vector) (json-value import :string))
                              :null)
                 "only" (or (getf request :only) :null)
                 "without" (or (getf request :without) :null)
                 "overwrite" (if (getf request :overwrite) t :false)
                 "keep_running" (if takeover :false t)
                 "stream" (if stream t :false))
                :stream stream)))
         (if (not stream)
             response
             (with-cleanup ((close response :abort t))
               (let ((result nil))
                 (nlk:read-events
                  response
                  (lambda (data &aux (event (nlk:decode-json data)))
                    (dispatch (gethash "type" event) equal
                      ("item" (when progress (funcall progress event)) nil)
                      ("result" (setf result (gethash "result" event)) t)
                      (t (error "unknown import event"))))
                  :idle 60)
                 (unless (hash-table-p result) (error "import stream ended without a result"))
                 (when (json-value result :text "error")
                   (error "~a" (gethash "error" result)))
                 result))))))))

(defun import-summary-line (value &aux (summary (gethash "summary" value)))
  "`3 to import · 2 skipped · 1 conflict · 0 errors' from the plan's summary
— `3 imported' once the plan has been applied, which the plan itself says."
  (flet ((count-of (key) (or (and (hash-table-p summary) (gethash key summary)) 0)))
    (format nil "~d ~a · ~d skipped · ~d conflict~:p · ~d error~:p"
            (count-of "imported") (if (eq t (gethash "applied" value)) "imported" "to import")
            (count-of "skipped") (count-of "conflict") (count-of "error"))))


(defun setup-import-item-label (item)
  "The authority's item as a short display label; no credential or tool text."
  (let* ((kind (json-value item :string "kind"))
         (name (or (cdr (assoc kind '(("providers" . "Provider") ("instructions" . "Instructions")
                                     ("memory" . "Memory") ("skills" . "Skill")
                                     ("sessions" . "Conversation") ("channels" . "Channel"))
                              :test #'equal))
                   (string-capitalize (or kind "Item")))))
    (format nil "~a~@[ · ~a~]" name (json-value item :text "destination"))))

(defun setup-import-wait (model run subtitle thunk continuation &key importing)
  "A work panel off the loop; live progress and completion belong to this dialog."
  (let ((first-run (setup-first-run-p run))
        (program (chat-model-program model))
        (begun (get-internal-real-time))
        (current subtitle) (recent '())
        (dialog nil) (returned nil) (result nil))
    (labels ((work-summary (mark)
               (cons (list (make-seg :text mark :tone :dialog-accent)
                           (make-seg :text (format nil "  ~a" current) :tone :foreground))
                     (loop for (status . label) in recent
                           collect (list (make-seg :text (if (equal status "imported") "✓" "!")
                                                  :tone (if (equal status "imported") :dialog-accent :danger))
                                         (make-seg :text (format nil "  ~a" label) :tone :muted)))))
             (paint (mark)
               (setf (picker-dialog-summary-rows dialog) (work-summary mark)
                     (picker-dialog-header-rows dialog) (+ 2 (length (picker-dialog-summary-rows dialog)))))
             (progress-on-loop (model event)
               (if (not (eq dialog (chat-model-dialog model)))
                   :quiet
                   (let* ((item (gethash "item" event))
                          (label (setup-import-item-label item)))
                     (cond ((equal "importing" (gethash "phase" event))
                            (setf current (format nil "Importing ~a" label)))
                           ((equal "finished" (gethash "phase" event))
                            (setf recent (subseq (cons (cons (gethash "status" item) label) recent)
                                                 0 (min 4 (1+ (length recent)))))
                            (when (equal current (format nil "Importing ~a" label))
                              (setf current subtitle))))
                     (paint (status-mark (elapsed-ms begun (get-internal-real-time)) :search))
                     nil)))
             (report (event)
               ;; Workers only post immutable response values. Model reads,
               ;; row updates and cancellation checks happen on the UI loop.
               (if program
                   (nlt:send program (make-instance 'off-loop-msg :value event :continuation #'progress-on-loop))
                   (progress-on-loop model event)))
             (finish (model)
               ;; Let the first replay settle before appending discovery rows.
               (when (and returned (eq dialog (chat-model-dialog model))
                          (or (not first-run)
                              (not (typep model 'gateway-chat-model))
                              (and model.last-sequence (null model.replay-staging))
                              *organism-boot-failure*))
                 (setf (chat-model-dialog model) nil)
                 (funcall continuation model result)
                 t)))
      (open-picker-dialog
       model "Setup" '()
       (lambda (model value) (declare (ignore model value)) nil)
       :context (if first-run "Preparing Nodecode" (setup-context run :import "Import"))
       :subtitle subtitle
       :summary-rows (when (or first-run importing) (work-summary (status-mark 0 :search)))
       :header-rows (when (or first-run importing) 3)
       :empty-label ""
       :footer-hints (if (or first-run importing) :none '(("esc" . "cancel")))
       :kind :setup-import
       :on-cancel (lambda (model) (setf (chat-model-setup-walk model) nil))
       :on-tick (let ((shown nil))
                  (lambda (model dialog)
                    (or (finish model)
                        (let* ((elapsed (elapsed-ms begun (get-internal-real-time)))
                               (mark (status-mark elapsed :search)))
                          (unless (equal shown mark)
                            (setf shown mark)
                            (if (or first-run importing)
                                (paint mark)
                                (setf (picker-dialog-title dialog) (format nil "Setup  ~a" mark)))
                            t))))))
      (setf dialog (chat-model-dialog model))
      (off-loop model (if importing (lambda () (funcall thunk #'report)) thunk)
                (lambda (model value)
                  (if (eq dialog (chat-model-dialog model))
                      (progn (setf returned t result value) (finish model) nil)
                      :quiet))))))

(defun close-setup-placeholder (model kind &aux (open model.dialog))
  (when (and (picker-dialog-p open) (eq (picker-dialog-kind open) kind)) (setf model.dialog nil)))

(defun setup-world-label (name)
  "What a report calls the world NAME: its label, else NAME itself."
  (if-let (world (nlk:find-agent-world name)) (nlk:agent-world-label world) name))

(defun setup-import-offer (model run worlds)
  "The first frame's question over WORLDS, the detected homes nobody declined:
Import brings them all over (SETUP-IMPORT-OFFERED), Choose which ticks them
one by one (SETUP-IMPORT-CHOOSE); Skip or Esc reads and writes nothing but
the Skip, and the walk goes on to the provider."
  ;; Asked before a plan, which would read every home to count it: the
  ;; question is whether to read them at all.
  (let ((names (mapcar #'setup-world-label worlds)))
    (open-picker-dialog
     model "Setup"
     (append
      (list (make-picker-row :label (if (rest worlds)
                                        (format nil "Import from all ~d" (length worlds))
                                        (format nil "Import from ~a" (first names)))
                             :detail "keys, instructions, memory, skills, MCP servers, bots and jobs"
                             :value :import))
      ;; one agent is all or nothing already
      (when (rest worlds)
        (list (make-picker-row :label "Choose which…"
                               :detail "tick the agents to bring over"
                               :value :choose)))
      (list (make-picker-row :label "Skip"
                             :detail "nothing read or written; /import later"
                             :value :skip)))
     (lambda (model choice)
       (ecase choice
         (:import (setup-import-offered model run worlds))
         (:choose (setup-import-choose model run worlds))
         (:skip (setup-import-skip model run worlds)))
       nil)
     :context (setup-context run :import "Import")
     :subtitle (format nil "Found ~{~a~#[~; and ~:;, ~]~} on this machine — bring ~:[it~;them~] over?"
                       names (rest names))
     :hint "enter select  esc skip  ↑↓ navigate"
     :kind :setup-import-offer
     :option-marker t
     :on-cancel (lambda (model) (setup-import-skip model run worlds)))))

(defun setup-import-skip (model run worlds)
  "Put WORLDS on the marker as declined, nothing of theirs read, and go on to
the provider."
  (ignore-errors (note-import-declined (json-object "worlds" (coerce worlds 'vector))))
  (setup-note run "import skipped — /import brings ~:[it~;them~] over later" (rest worlds))
  (setup-ledger-note run (list :keys "–" "import" "skipped, nothing read"))
  (setup-stage-provider model run))

(defun setup-import-choose (model run worlds)
  "WORLDS as a checklist, every one ticked: Enter brings the ticked ones over
and declines the rest, none ticked being a Skip; Esc goes back to the
question (SETUP-IMPORT-OFFER)."
  (open-picker-dialog
   model "Setup"
   (loop for name in worlds
         for world = (nlk:find-agent-world name)
         collect (make-picker-row :label (setup-world-label name)
                                  :detail (if-let (root (and world (first (nlk:agent-world-roots world))))
                                            (format nil "~~/~a" root)
                                            "")
                                  :value name))
   (lambda (model chosen)
     ;; in the order detection found them, whatever order they were ticked in
     (let ((taken (remove-if-not (lambda (name) (member name chosen :test #'string-equal)) worlds))
           (left (remove-if (lambda (name) (member name chosen :test #'string-equal)) worlds)))
       (cond ((null taken) (setup-import-skip model run worlds))
             (t (when left
                  ;; an unticked agent is a Skip of its own: never read again unasked
                  (ignore-errors (note-import-declined (json-object "worlds" (coerce left 'vector))))
                  (setup-note run "left ~{~a~#[~; and ~:;, ~]~} out — /import brings ~:[it~;them~] over later"
                              (mapcar #'setup-world-label left) (rest left)))
                (setup-import-offered model run taken))))
     nil)
   :context (setup-context run :import "Import")
   :subtitle "Tick the agents to bring over"
   :hint "space toggle  enter import  esc back  ↑↓ navigate"
   :kind :setup-import-choose
   :marks (copy-list worlds)
   :on-toggle (lambda (model value marked) (declare (ignore model value marked)) nil)
   :on-cancel (lambda (model) (setup-import-offer model run worlds))))

(defun setup-import-offered (model run worlds)
  "Bring WORLDS over once the operator said Import: the keys while the walk
waits, the one thing its steps read, and the rest of the homes behind it
(SETUP-IMPORT-REST) -- waiting for a whole home would hold this panel for
seconds on a large home."
  (setup-import-wait
   model run "Importing…"
   (lambda (progress)
     (let ((request (list :import worlds :offered t :only "providers")))
       (setup-import-rest model run request)
       (list request (setup-import-call model "apply" request nil :progress progress))))
   (lambda (model result)
     (prog1 (if-let (why (off-loop-failure result))
              (progn (setup-caveat run "import: ~a" why)
                     (setup-import-report run (make-json-object))
                     (setup-stage-provider model run))
              (destructuring-bind (request value) result
                (setup-import-applied model run request nil
                                      (lambda (model)
                                        ;; The model the operator ran elsewhere, its
                                        ;; key answering: the walk is over, and the
                                        ;; first frame takes a task
                                        (if-let (item (setup-import-ready run))
                                          (progn
                                            (setup-note run "~:[the import's pick~;~:*~a~] — /models changes it"
                                                        (json-value item :string "reason"))
                                            ;; a model run more, on a credential that stayed behind
                                            (when-let (passed (imported-default-model (setup-run-import run) "skipped"))
                                              (setup-note run "~a" (json-value passed :string "reason")))
                                            (setup-stage-finish model run))
                                          (setup-stage-provider model run)))
                                      value :behind t)))
       (setup-import-rest-due model run)))
   :importing t))

(defun setup-import-confirm (model run request value next)
  "The yes/no over the plan VALUE, for an import somebody asked for: import
— keys and tokens with the rest — or skip."
  ;; When a running foreign gateway polls bots the home carries (VALUE's
  ;; `bots') as its systemd unit (VALUE's `takeover'), the first row takes
  ;; them over — the unit stopped and disabled, the bots answering from here —
  ;; and the second leaves it running, the bots coming on by themselves once
  ;; it stops; --keep-running offers the second alone. The report is the
  ;; question's Details row, a page of this dialog that comes back to it: it
  ;; was a toast cut at four rows that left before the operator answered
  ;; (ip-01), then a transcript row the chat opened on.
  (setup-import-report run value)
  (let* ((to-import (or (nlk:json-value value :integer "summary" "imported") 0))
         (bots (coerce (json-value value :array "bots") 'list))
         (unit (json-value value :string "takeover"))
         (takeover (and bots unit (request-takeover request)))
         (noun (format nil "the bot~p" (length bots)))
         (skip (lambda (model)
                 (ignore-errors (note-import-declined value))
                 (setup-note run "import skipped")
                 (funcall next model)))
         (rows (append
                (when takeover
                  (list (make-picker-row
                         :label (format nil "Import ~d item~:p and take ~a over" to-import noun)
                         :detail (format nil "~a is stopped and disabled; ~{~a~^ and ~} answer~:[s~;~] from here now"
                                         unit bots (rest bots))
                         :value :takeover)))
                (list (make-picker-row
                       ;; The plan was made with the takeover, whose stop is
                       ;; one of its items: without it there is one fewer.
                       :label (if takeover
                                  (format nil "Import ~d item~:p, ~a keeps ~a"
                                          (max 0 (1- to-import)) unit noun)
                                  (format nil "Import ~d item~:p" to-import))
                       ;; A gateway running outside its unit is named by no unit.
                       :detail (if bots
                                   (format nil "~{~a~^ and ~} wait~:[s~;~] for ~a to stop, then answer~:[s~;~] from here"
                                           bots (rest bots) (or unit "the other gateway") (rest bots))
                                   "everything the plan lists, keys and tokens with it")
                       :value :keep))
                ;; the report, a page of this dialog that comes back here
                (when (setup-report-items value)
                  (list (make-picker-row :label "Details"
                                         :detail "each item, and why any is skipped"
                                         :value :details)))
                (list (make-picker-row :label "Skip"
                                       :detail "nothing written; /import later"
                                       :value :skip)))))
    (open-picker-dialog
     model "Setup" rows
     (lambda (model choice)
       (ecase choice
         (:takeover (setup-import-apply model run request t next))
         (:keep (setup-import-apply model run request nil next))
         (:details (setup-details-page
                    model value (lambda (model) (setup-import-confirm model run request value next))))
         (:skip (funcall skip model)))
       nil)
     :context (setup-context run :import "Import")
     :subtitle (format nil "~a — Details lists each item" (import-summary-line value))
     :hint "enter select  esc skip  ↑↓ navigate"
     :kind :setup-import-confirm
     :option-marker t
     :on-cancel skip)))

(defun note-import-declined (value)
  "Put the worlds the plan VALUE read on the marker as declined: the import
the walk makes unasked passes them over from now on."
  ;; A Skip is an answer. Without this, a bare launch would read the same home
  ;; and import it unasked, keys, bot token and all.
  (let ((worlds (remove-if-not #'nlk:find-agent-world (coerce (json-array value "worlds") 'list))))
    (when (and *import-marker* worlds)
      (write-import-marker "declined"
                           (remove-duplicates (concatenate 'vector (declined-import-worlds) worlds)
                                              :test #'string-equal :from-end t)))))

(defun note-import-done (value)
  "Leave the marker the offer reads, with what this run landed and what it
left behind; the worlds it read are declined no longer."
  ;; A run that wrote nothing leaves no import on it: an import that did not
  ;; happen does not spend the offer. Taking a world clears its Skip either way.
  (let ((imported (or (nlk:json-value value :integer "summary" "imported") 0))
        (worlds (coerce (or (json-value value :array "worlds") #()) 'list))
        (declined (declined-import-worlds))
        ;; The watched kinds the applied plan VALUE left behind: those
        ;; carrying an item it did not land — skipped, or failed outright.
        ;; Whether that still matters is a question for the home and not for
        ;; the report: a kind named here is asked about again only while this
        ;; home actually lacks it (IMPORT-DONE-P), so a Telegram
        ;; section no .env names, or an OAuth grant only that harness can hold,
        ;; never turns into a standing offer.
        (left '()))
    (loop for item across (or (gethash "items" value) #())
          for kind = (json-value item :string "kind")
          when (and (member kind +import-watched-kinds+ :test #'equal)
                    (member (json-value item :string "status") '("skipped" "error") :test #'equal))
            do (pushnew kind left :test #'equal))
    (let ((kept (remove-if (lambda (world) (member world worlds :test #'string-equal)) declined)))
      (when (and *import-marker* (or (plusp imported) (not (equal kept declined))))
        (apply #'write-import-marker
               "declined" (and kept (coerce kept 'vector))
               (and (plusp imported)
                    (list "at" (nlk::iso-now)
                          "imported" imported
                          "outstanding" (coerce (nreverse left) 'vector))))))))


(defun setup-import-apply (model run request takeover next &optional (subtitle "Importing…"))
  "Apply the plan off the loop — TAKEOVER stopping a running foreign gateway
for its bots — then SETUP-IMPORT-APPLIED."
  (setup-import-wait
   model run subtitle
   (lambda (progress)
     (setup-import-call model "apply" request takeover :progress progress))
   (lambda (model value)
     (setup-import-applied model run request takeover next value))
   :importing t))

(defun setup-import-applied (model run request takeover next value &key behind)
  "What the walk makes of VALUE, the answer of REQUEST's apply: the report,
the marker when something landed, the model the import set, what qualifies
it -- then on to NEXT, past one answer from an imported default model's
provider. BEHIND says the rest of the homes lands later (SETUP-IMPORT-REST):
the keys' count alone, and the rest's landing says what the import did."
  (if-let (why (off-loop-failure value))
    (setup-caveat run "import: ~a" why)
    (progn
      (setf (setup-run-import run) value)
      (setup-import-report run value (if behind '(:keys) '(:keys :configs :memory)))
      (ignore-errors (note-import-done value))
      (when (plusp (or (nlk:json-value value :integer "summary" "imported") 0))
        ;; The shell's config memo predates the import: re-seed from the file.
        (setf (setup-run-migrated run) t
              *shared-config-memo* nil)
        (ignore-errors (seed-model-config-from-shared-config))
        (when (ignore-errors (default-model-configured-p))
          (setf (setup-run-providers run) (list *provider*)
                (setup-run-model run) (list *provider* *model*))))
      (setf (setup-run-grants run) (coerce (json-array value "grants") 'list))
      (setup-import-qualify run value takeover :sources (unless behind value))
      (when (eq t (gethash "sessions_left" value))
        (push "conversations stay where they are for now — /import --only sessions brings them over"
              (setup-run-notes run)))))
  ;; A resolving credential is not a working one: the target answers once first.
  (if (and (not (off-loop-failure value)) (imported-default-model value))
      (setup-import-verify model run request takeover next)
      (funcall next model)))

(defun imported-default-model (value &optional (status "imported"))
  "The item of VALUE, an apply's answer, that set the default model -- or,
STATUS `skipped', passed one over -- or NIL."
  (find-if (lambda (item)
             (and (equal "providers" (json-value item :string "kind"))
                  (equal "general.default_model" (json-value item :string "destination"))
                  (equal status (json-value item :string "status"))))
           (json-value value :array "items")))

(defun setup-import-ready (run &aux (picked (setup-run-model run)))
  "The item by which RUN's import set the default model, when that model's
provider answered the check (SETUP-IMPORT-VERIFY), else NIL."
  (and picked
       (not (gethash (first picked) *key-checks*))
       (imported-default-model (setup-run-import run))))

(defun setup-import-qualify (run value takeover &key (sources value))
  "RUN's notes and caveats from VALUE, an apply's answer: what became of a
foreign gateway whose bots it imported, with SOURCES (an answer, VALUE unless
said) where it came from and what it did, and any item that failed."
  (let ((unit (gethash "takeover" value))
        (bots (coerce (json-value value :array "bots") 'list)))
    ;; What became of somebody else's gateway, when the plan imported bots it also polls:
    ;; stopped is said with the command that gives it back, left running with
    ;; the one that moves them here.
    (when (and (stringp unit) bots)
      (if takeover
          (setup-note run "took over ~a — `systemctl --user enable --now ~a' gives it back"
                      unit unit)
          (when-let (world (find unit nlk:*agent-worlds*
                                 :key #'nlk:agent-world-takeover-unit :test #'equal))
            (setup-caveat run "~a is still running, so ~{~a~^ and ~} stay~:[s~;~] with it — `nodecode import ~a' asks first, then moves ~:[it~;them~] here"
                          unit bots (rest bots) (nlk:agent-world-name world) (rest bots))))))
  (when sources
    (setup-note run "imported from ~:[the box~;~:*~{~a~#[~; and ~:;, ~]~}~]: ~a"
                (coerce (or (gethash "worlds" sources) #()) 'list)
                (import-summary-line sources)))
  ;; An import that partly failed qualifies the walk: under a note, a
  ;; closing toast said `type a task' over it.
  (let ((failed (or (nlk:json-value value :integer "summary" "error") 0)))
    (when (plusp failed)
      (setup-caveat run "import: ~d item~:p failed — Details names ~:*~[~;it~:;them~]"
                    failed))))

;;; The first frame's import waits for the keys alone; the rest of the homes
;;; -- memory, skills, instructions, servers, bots, jobs -- is a second apply
;;; beside it, which nothing the walk asks waits for. It lands where the walk
;;; is: on its ledger while the walk or its last page is open, else as a toast.

(defun merged-import-value (first then)
  "THEN, an apply's answer, with FIRST -- an earlier one, or NIL -- folded in:
the counts summed, the items and the worlds joined."
  (if (null first)
      then
      (let ((value (alexandria:copy-hash-table then))
            (summary (make-json-object)))
        (flet ((both (key) (concatenate 'vector (json-array first key) (json-array then key))))
          (dolist (key '("imported" "skipped" "conflict" "error"))
            (setf (gethash key summary) (+ (or (nlk:json-value first :integer "summary" key) 0)
                                           (or (nlk:json-value then :integer "summary" key) 0))))
          (setf (gethash "summary" value) summary
                (gethash "items" value) (both "items")
                (gethash "worlds" value) (remove-duplicates (both "worlds")
                                                            :test #'string-equal :from-end t)))
        value)))

(defun setup-import-rest (model run request)
  "Bring over beside the keys' REQUEST what it leaves: every kind but the
providers it lands and the conversations; SETUP-IMPORT-REST-LANDED says it
once the keys' answer is taken (SETUP-IMPORT-REST-DUE)."
  ;; Begins once the keys have landed and waits on their walk of the homes and
  ;; the catalog.
  (let ((rest (list* :only nil :without "providers,sessions" request)))
    (off-loop model
              (lambda () (setup-import-call model "apply" rest nil))
              (lambda (model value)
                (if (eq :landed (setup-run-behind run))
                    (setup-import-rest-landed model run value)
                    (progn (setf (setup-run-behind run) (list value)) :quiet))))))

(defun setup-import-rest-due (model run &aux (held (shiftf (setup-run-behind run) :landed)))
  "The keys' answer is taken: the rest's, when it came first, is said now."
  (when (consp held)
    (setup-import-rest-landed model run (first held))))

(defun setup-import-rest-landed (model run value)
  "Say what the import behind the walk landed, VALUE its answer: the counts
over the whole import, the marker brought up to it, and its notes and
caveats -- on RUN's ledger, and its last page again, while the walk or a page
of its end is open, else in a toast."
  (let* ((dialog (chat-model-dialog model))
         (shown (and (picker-dialog-p dialog) (eq run (setup-page-run dialog)) dialog))
         (open (or shown (eq run (chat-model-setup-walk model))))
         (said (if open run (make-setup-run)))
         (whole nil))
    (if-let (why (off-loop-failure value))
      (setup-caveat said "import: ~a" why)
      (progn
        (setf whole (merged-import-value (setup-run-import run) value)
              (setup-run-import run) whole)
        (setup-import-report run whole '(:configs :memory))
        (ignore-errors (note-import-done whole))
        (setup-import-qualify said value nil :sources whole)))
    (cond ((and shown (eq :setup-ready (picker-dialog-kind shown)))
           ;; the page again, on the row the operator was on
           (setup-ready-page model run :selection (picker-dialog-selection shown)))
          ((not open)
           (when-let (lines (or (append (reverse (setup-run-notes said))
                                        (reverse (setup-run-caveats said)))
                                (and whole (list (format nil "import: ~a" (import-summary-line whole))))))
             (toast model (format nil "~{~a~^~%~}" lines) :stays (and (setup-run-caveats said) t)))))
    nil))

(defparameter +import-verify-seconds+ 4
  "How long the walk waits for the imported provider to answer before it goes
on without knowing.")

(defun setup-import-verify (model run request takeover next)
  "Ask the imported target to answer once, and go on to NEXT with what came
first: its answer, the deadline, or Esc."
  ;; An answer that is a refusal names the key; every other end — Esc, the
  ;; deadline, the call itself failing, a port nothing listens on (the
  ;; answer's `connection_refused' code: no verdict on the key) — leaves the
  ;; provider imported and says it was not checked. The wait is the walk's own: a port nobody listens on
  ;; answers at once, but an endpoint that takes the connection and never
  ;; answers holds the call to its own timeout, so the deadline is set here, on
  ;; the loop, and the abandoned call ends by itself. An end after the first is
  ;; dropped. The first end is also the imported key's standing on the
  ;; provider list and the step line (*KEY-CHECKS*): anything but an answer
  ;; read `ready' and `connected' there (am-07, xm-201), and it is what the
  ;; closing block says of the key (SETUP-CAVEATS), with a refusal's ANSWER,
  ;; the provider's own words, beside it.
  ;; WORDS are the provider step's search row (SETUP-PROVIDER-PROMPT): the
  ;; provider and what happened, never the failure's whole report, which
  ;; ran past the row with three remedies.
  (let* ((provider (or *provider* "the imported provider"))
         (name (or (json-value (models-catalog-table) :text provider "name") provider))
         ;; `host:port' of the URL the default target dials, or NIL
         (endpoint (url-address (ignore-errors (effective-endpoint))))
         (settled nil))
    (labels ((settle (model &optional verdict words answer)
               (cond (settled :quiet)
                     (t (setf settled t)
                        (when *provider*
                          (if verdict
                              (setf (gethash *provider* *key-checks*) (list verdict words))
                              (remhash *provider* *key-checks*))
                          (when answer
                            (push (cons *provider* answer) (setup-run-answers run))))
                        (close-setup-placeholder model :setup-import)
                        (funcall next model)
                        nil)))
             (unverified (model why)
               (settle model :unchecked (format nil "~a key was not checked: ~a" name why))))
      (let ((live (list "…" provider
                        (format nil "asking ~:[its endpoint~;~:*~a~], up to ~d seconds"
                                endpoint +import-verify-seconds+)))
            (shown :none))
        ;; What the walk found stands over the live line; an import landing
        ;; behind the walk adds its lines while this waits.
        (flet ((paint (model dialog)
                 (setf shown (setup-run-ledger run)
                       (picker-dialog-summary-rows dialog)
                       (setup-line-rows (append (setup-ledger-lines run) (list live))
                                        (setup-panel-width model))
                       (picker-dialog-header-rows dialog)
                       (+ 2 (length (picker-dialog-summary-rows dialog))))))
          (open-picker-dialog
           model "Setup" '()
           (lambda (model value) (declare (ignore model value)) nil)
           :context (setup-context run :import "Import")
           :subtitle (format nil "Checking that ~a answers~@[ at ~a~]…" provider endpoint)
           :empty-label ""
           :hint "esc skip the check"
           :kind :setup-import
           :on-cancel (lambda (model) (unverified model "the check was skipped"))
           :on-tick (lambda (model dialog)
                      (unless (eq shown (setup-run-ledger run))
                        (paint model dialog)
                        t)))
          (paint model (chat-model-dialog model))))
      (off-loop model (lambda () (setup-import-call model "verify" request takeover))
                (lambda (model answer)
                  (cond ((off-loop-failure answer) (unverified model "the check itself failed"))
                        ((and (hash-table-p answer) (eq t (gethash "ok" answer))) (settle model))
                        ;; the address says it plainly
                        ((equal "connection_refused" (json-value answer :string "code"))
                         (settle model :unreachable
                                 (format nil "nothing listens at ~a for ~a — is its server running?"
                                         (or endpoint "the address it names") name)))
                        (t (let ((code (json-value answer :string "code"))
                                 (status (json-value answer :integer "status"))
                                 (reason (json-value answer :string "reason")))
                             ;; `auth' is a refused key (REFUSED-KEY-ANSWER-P); any
                             ;; other failure, a 5xx or a 404, may not be. Below HTTP
                             ;; the reason is the transport fact in words, the key
                             ;; taken out; a failure no code names is no provider's.
                             (if (equal "auth" code)
                                 (settle model :refused (format nil "~a refused the imported key" name)
                                         (and status (format nil "HTTP ~d~@[: ~a~]" status reason)))
                                 (unverified model
                                             (cond (status (format nil "~a answered HTTP ~d"
                                                                   (or endpoint "its address") status))
                                                   ((and code (plusp (length reason))) reason)
                                                   (t "the check failed")))))))))
      (unless settled
        (off-loop model (lambda () (sleep +import-verify-seconds+))
                  (lambda-ignoring (model elapsed)
                    (unverified model (format nil "nothing answered~@[ at ~a~] within ~d seconds"
                                              endpoint +import-verify-seconds+))))))))

;;; --- stage: provider ---------------------------------------------------------

(defun setup-stage-provider (model run)
  "The /models list: every provider a key can be saved for, the ones this
machine already has a key for Suggested; Enter on one of them goes on to
the model stage, on its models. A saved key returns to it, so a walk connects as many as
the operator has keys for."
  ;; An import that brings a key over does not pass the list over: its
  ;; provider is Suggested like any other, and the operator can still add
  ;; more. One that also sets the default model, its key answering, ends
  ;; the walk before this list (SLASH-SETUP-OP). Esc ends the walk; the
  ;; closing line's health problems and the footer (FOOTER-FRAME-ROW) say
  ;; a provider is still missing. Before this stage only an import names
  ;; the walk's providers (SETUP-IMPORT-APPLY).
  (open-connect-dialog
   model #'setup-provider-rows
   (connect-credential-sink
    (lambda (id &aux (dialog model.dialog))
      (setf (setup-run-providers run)
            (cons id (remove id (setup-run-providers run) :test #'string-equal)))
      ;; the search row now says what the walk still owes (SETUP-PROVIDER-PROMPT)
      (when (connect-dialog-p dialog)
        (setf (connect-dialog-walk dialog) (setup-context run :provider "Step 1. Choose provider"))
        (setup-provider-prompt run dialog))))
   (lambda (model &optional provider)
     (cond ((equal provider '(:login))
            ;; the sign-in row's (MODELS-SIGN-IN-ROWS): nodecode.ai is this step's pick
            (setup-stage-sign-in model run))
           (t
            (when provider
              (setf (setup-run-providers run)
                    (cons provider (remove provider (setup-run-providers run) :test #'string-equal))))
            (setup-stage-model model run provider)))))
  (let ((dialog model.dialog))
    (when (connect-dialog-p dialog)
      ;; A refused key's row is where Enter pastes a new one: the selection
      ;; starts there, not on the first connected provider.
      (when-let (at (setup-provider-prompt run dialog))
        (setf dialog.selection at))
      (setf (connect-dialog-walk dialog) (setup-context run :provider "Step 1. Choose provider")
            (picker-arrow-handlers dialog) (setup-step-arrows run :provider)
            dialog.on-cancel
            (lambda (model) (setup-stage-finish model run)))
      ;; the step's chrome again, now in the walk's words
      (connect-enter-step dialog :provider))))

(defun setup-provider-prompt (run dialog &aux (pinned (shared-model-pin))
                                               (attention (loop for id in (remove nil (cons pinned (setup-run-providers run)))
                                                                for refusal = (provider-turn-refusal id)
                                                                for check = (gethash id *key-checks*)
                                                                when refusal return (list id :refusal refusal)
                                                                when check return (cons id check))))
  "Put on the provider step's search row what the walk owes before the
operator types, and answer the index of the row Enter pastes a new key on
when the key it speaks of was refused, else NIL."
  ;; The default model's provider first, else one this walk connected: when
  ;; no turn can run on it, the refusal's words, which outrank its key;
  ;; when its key did not answer its check, what happened (*KEY-CHECKS*) --
  ;; the step line says `needs attention' over either, in the order the
  ;; closing block says them (SETUP-CAVEATS). After a migration a reason is also owed when
  ;; the sign-in did not come over. The dialog searches, so its header row is
  ;; the search row -- the reason rides the prompt that shows until the
  ;; operator types. Without the row hint, a refused imported key would
  ;; read `the imported key did not answer' and name neither provider nor
  ;; row to fix it on.
  (destructuring-bind (&optional id verdict words) attention
    (let ((at (and (eq verdict :refused)
                   (position-if (lambda (row) (string-equal id (getf (picker-row-value row) :provider)))
                                (picker-visible-rows dialog)))))
      (setf dialog.search-placeholder
            ;; inside 80 columns: what happened, and a refused key's way on
            (cond (attention (format nil "~a~:[~; · Enter on its row pastes a new key~]" words at))
                  ((and (setup-run-migrated run) (setup-run-grants run))
                   (format nil "~{~a~^ and ~}'s sign-in stays there · type to find a provider"
                           (setup-run-grants run)))
                  ((equal dialog.search-placeholder +loading-providers+) +loading-providers+)
                  (t "Search providers")))
      at)))

(defun setup-provider-rows (&aux (rows (connect-picker-rows)) (sign-in (models-sign-in-rows)))
  "The provider step's rows: CONNECT-PICKER-ROWS, led by nodecode.ai's sign-in row
(MODELS-SIGN-IN-ROWS) while this home is not signed in -- the first row of a box with no
provider, so Enter on the first screen signs in; once a key is saved, the head of what is left
to connect, under Suggested."
  ;; nodecode.ai is on no models.dev list: a new buyer's first screen offered every provider but
  ;; ours, and only `nodecode login' before the first launch put it there (checkout audit,
  ;; the operator, 2026-10-23). Under Suggested, the provider a saved key returns to stays the row
  ;; the list rests on (CONNECT-DIALOG-KEY seats the top).
  (let ((at (and sign-in
                 (position-if (lambda (row)
                                (and (picker-row-heading row)
                                     (equal "Connect another" (picker-row-label row))))
                              rows))))
    (cond ((null sign-in) rows)
          (at (append (subseq rows 0 (1+ at)) sign-in (nthcdr (1+ at) rows)))
          (t (append sign-in
                     (list (make-picker-row :heading t)
                           (make-picker-row :label "Or use a provider's key" :heading t))
                     rows)))))

(defun setup-stage-sign-in (model run)
  "nodecode.ai as the provider step's pick: /login's choice of a subscription or API credits,
in the walk's words, then the panel that waits for the browser (SETUP-SIGN-IN-WAIT). Esc returns
to the providers."
  (slash-login-op model ""
                  :pick (lambda (model mode) (setup-sign-in-wait model run mode))
                  :context (setup-context run :provider "Step 1. Sign in to nodecode.ai")
                  :on-cancel (lambda (model) (setup-stage-provider model run))))

(defun setup-sign-in-wait (model run mode)
  "Start MODE's sign-in (ACCOUNT-LOGIN-FROM-SHELL) and wait for it on a walk panel that says
where to allow this machine. Signed in, the walk goes on to nodecode.ai's models, the gateway's
list read again first; a failure says so over the providers. Esc returns to the providers, the
sign-in still waiting in the browser."
  ;; The panel is the walk's while it is open: an answer landing after Esc, or after another
  ;; dialog replaced it, moves nothing (the organism's notice still says how it ended).
  (let ((dialog nil) (started nil))
    (labels ((ours (model) (and dialog (eq dialog (chat-model-dialog model))))
             (say (&rest lines)
               (setf (picker-dialog-summary-rows dialog)
                     (loop for (text tone) in lines
                           collect (list (make-seg :text text :tone tone)))
                     (picker-dialog-header-rows dialog) (+ 2 (length lines))))
             (back (model words)
               (setf (chat-model-dialog model) nil)
               (setup-stage-provider model run)
               (show-status model words :error-p t))
             (models (model)
               (setf (setup-run-providers run)
                     (cons +own-provider+ (remove +own-provider+ (setup-run-providers run) :test #'string-equal)))
               (when (ours model)
                 (setf (chat-model-dialog model) nil)
                 (setup-stage-model model run +own-provider+)))
             (answered (model value &aux (failure (off-loop-failure value))
                                         (login (and (not failure) (json-value value :object "login")))
                                         (state (and login (json-value login :string "state"))))
               (when (ours model)
                 (cond (failure (back model (format nil "nodecode.ai sign-in: ~a" failure)))
                       ((and (not started) (equal state "waiting"))
                        (setf started t)
                        (say (list (format nil "~:[Open~;Opened~] ~a" (eq t (json-value login :boolean "opened"))
                                           (json-value login :string "url"))
                                   :foreground)
                             (list (format nil "code ~a" (json-value login :string "code")) :dialog-accent)
                             (list "Sign in there and allow this machine; this step goes on by itself." :muted)))
                       ((equal state "signed-in")
                        (say (list (or (json-value login :string "text") "Signed in to nodecode.ai") :dialog-accent)
                             (list "Loading nodecode.ai's models…" :muted))
                        ;; a shell holds the gateway's list from before the key existed
                        (if (and (not *serving*) (chat-model-program model) (chat-model-http-port model))
                            (off-loop model (lambda () (fetch-gateway-models model))
                                      (lambda (model value) (declare (ignore value)) (models model)))
                            (models model)))
                       (t (back model (or (and login (json-value login :string "text"))
                                          "nodecode.ai sign-in: the browser did not allow this machine in time")))))))
      (open-picker-dialog
       model "Setup" '()
       (lambda (model value) (declare (ignore model value)) nil)
       :context (setup-context run :provider "Step 1. Sign in to nodecode.ai")
       :subtitle "Asking nodecode.ai…"
       :empty-label ""
       :footer-hints '(("esc" . "back"))
       :kind :setup-sign-in
       :on-cancel (lambda (model) (setup-stage-provider model run)))
      (setf dialog (chat-model-dialog model))
      ;; the panel says it is asking: no `login: asking…' notice stands under it
      (account-login-from-shell model mode :then #'answered)
      nil)))

;;; --- stage: model ------------------------------------------------------------

(defun setup-stage-model (model run &optional chosen)
  "Choose a default model, then effort during setup.
CHOSEN narrows the setup walk to the chosen provider."
  ;; Only a pick is the walk's model, and only once the shared config holds it
  ;; (SETUP-RECORD-MODEL): Esc leaves the target nothing chose, and a pick
  ;; whose write failed chose nothing that lasts, which the closing block
  ;; must not name. The selection starts on
  ;; the suggested model (SEAT-MODELS-PICKER) of the provider chosen, else of
  ;; the one this walk connected last, or of the one connected when there is
  ;; one.
  ;;
  ;; After an import the selection rests on the model it brought over, not on
  ;; the newest: Enter replaced the imported one with the newest (am-07).
  (flet ((next (model picked)
           (cond (picked (setup-record-model run picked) (setup-stage-effort model run))
                 (t (setup-stage-provider model run)))))
    ;; The walk sets the organism up: its pick is the default, not one
    ;; session's pin.
    (open-models-picker model :lands :default :then #'next)
    (let ((dialog model.dialog))
      (when (and (picker-dialog-p dialog) (eq :models dialog.kind))
        (setf dialog.context (setup-context run :model "Step 2. Choose model")
              dialog.hint "enter select  esc skip  ←→ steps  ↑↓ navigate  type search"
              (picker-arrow-handlers dialog) (setup-step-arrows run :model))
        (when-let (provider (or chosen
                                (first (setup-run-providers run))
                                (let ((ids (connected-provider-ids)))
                                  (and ids (null (rest ids)) (first ids)))))
          (register-picker-tabs
           dialog (list (list :label provider :value provider
                              :match (lambda (row id &aux (value (picker-row-value row)))
                                       (and (consp value) (string-equal (first value) id))))))
          (seat-models-picker dialog provider))))))

;;; --- stage: finish -----------------------------------------------------------

(defparameter +setup-try-text+ "type a task, such as \"what is in this folder?\""
  "Where to begin, when nothing is missing: the last page's Start row, what
the bare prompt never said.")

(defun setup-stage-finish (model run)
  "End the walk on its last page (SETUP-READY-PAGE). The walk is over here,
so the footer stops saying one runs."
  ;; It was a block in the transcript and a toast that pointed at it, so the
  ;; chat opened on the walk's own words; the walk's dialog says them now.
  (setf model.setup-walk nil)
  (show-status model nil)
  (setup-ready-page model run))

(nlk:define-side-table setup-page-run (:weakness :key)
  "Live-only: a walk's last page, or the Details page under it -> the
SETUP-RUN it says, so an import landing behind the walk finds the page up.")

(nlk:define-side-table setup-issues (:weakness :key)
  "Live-only: a shell's model -> (:CAVEATS C :RUN R :FACTS F), the caveats
its last walk R ended on when the operator started over them and the model
facts the shell held then, which the footer counts (SETUP-ISSUE-SEGS) until a
walk ends without one or the caveats stop standing.")

(defun setup-issue-segs (model)
  "The footer's chip for the caveats MODEL's last walk ended on that still
stand -- `1 setup issue', linked to /setup -- or NIL."
  ;; The caveats are read again once the target this shell holds moved (its
  ;; model facts are a new answer: a /models pick, say), so a caveat a later
  ;; pick answered leaves the count. Without this, `no model configured' was
  ;; counted on under a model that was set and answering (Mac take,
  ;; 2026-10-21). A read that fails keeps what was counted.
  (when-let (issues (setup-issues model))
    (let ((facts (gateway-model-model-facts model))
          (run (getf issues :run)))
      (unless (eq facts (getf issues :facts))
        (let ((caveats (handler-case (setup-caveats run)
                         (error () (getf issues :caveats)))))
          (setf issues (and caveats (list :caveats caveats :run run :facts facts)))
          (if issues
              (setf (setup-issues model) issues)
              (forget-setup-issues model)))))
    (when issues
      (list (hover-seg model (format nil "~d setup issue~:p" (length (getf issues :caveats)))
                       (cons :command "setup") :warn)))))

(defun setup-ready-lines (run caveats)
  "RUN's last page as (MARK LABEL TEXT) lines: CAVEATS (SETUP-CAVEATS) first,
then the providers, the model, what the walk found and its notes."
  (let ((ids (reverse (setup-run-providers run))))
    (append (loop for (label text) in caveats collect (list "!" label text))
            (when ids
              (list (list "✓" (if (rest ids) "providers" "provider") (format nil "~{~a~^, ~}" ids))))
            (cond ((setup-run-model run)
                   (list (list "✓" "model" (format nil "~{~a/~a~}" (setup-run-model run)))))
                  ((setup-run-unsaved run) (list (list "–" "model" "not saved"))))
            (setup-ledger-lines run)
            (mapcar (lambda (note) (list "·" "note" note)) (reverse (setup-run-notes run))))))

(defun setup-ready-page (model run &key (selection 0))
  "The walk's last page: what it did, what to check first, and the way on --
Start, or over a caveat the way back to the providers first -- with Details
when an import ran. Start and Esc close it on an empty chat."
  ;; A caveat holds the page until it is answered, and Start over one leaves
  ;; its count in the footer (SETUP-ISSUE-SEGS), which no frame retires.
  (let ((caveats (setup-caveats run))
        (report (setup-run-report run)))
    (flet ((start (model)
             (if caveats
                 (setf (setup-issues model) (list :caveats caveats :run run
                                                  :facts (gateway-model-model-facts model)))
                 (forget-setup-issues model))
             nil))
      (open-picker-dialog
       model "Setup"
       (append (if caveats
                   (list (make-picker-row :label "Change provider or key" :detail "back to step 1"
                                          :value :provider)
                         (make-picker-row :label "Start anyway" :detail "the footer keeps the count"
                                          :value :start))
                   (list (make-picker-row :label "Start" :detail +setup-try-text+ :value :start)))
               (when (setup-report-items report)
                 (list (make-picker-row :label "Details" :detail (setup-details-line report)
                                        :value :details))))
       (lambda (model choice)
         (ecase choice
           (:start (start model))
           (:provider
            ;; the walk again, from its first step
            (setf model.setup-walk run)
            (setup-stage-provider model run))
           (:details
            (setup-details-page model report
                                (lambda (model)
                                  (setup-ready-page model run :selection (if caveats 2 1)))
                                run)))
         nil)
       :context (if caveats (format nil "~d thing~:p to check" (length caveats)) "Ready")
       :summary-rows (setup-line-rows (setup-ready-lines run caveats) (setup-panel-width model))
       :selection selection
       :kind :setup-ready
       :option-marker t
       :hint "enter select  esc start  ↑↓ navigate"
       :on-cancel #'start)
      (setf (setup-page-run (chat-model-dialog model)) run))))

(defun setup-report-items (report)
  "REPORT's items as a list; none without a report."
  (and report (coerce (or (json-value report :array "items") #()) 'list)))

(defun setup-details-line (report &aux (summary (gethash "summary" report)))
  "The Details row's words for REPORT: what it lists, and how many did not come over."
  (let ((left (loop for key in '("skipped" "conflict" "error")
                    sum (or (and (hash-table-p summary) (gethash key summary)) 0))))
    (if (plusp left)
        (format nil "what came over, and why ~d did not" left)
        "what came over, item by item")))

(defun setup-details-page (model report back &optional run)
  "REPORT's items, world by world, each a row whose detail says what became
of it; Enter or Esc goes BACK, a function of the model. RUN is the walk the
page belongs to, for an import landing behind it."
  (let* ((items (setup-report-items report))
         (worlds (remove-duplicates (mapcar (lambda (item) (or (json-value item :string "world") "")) items)
                                    :test #'equal :from-end t)))
    (open-picker-dialog
     model "Setup"
     (loop for world in worlds
           append (cons (make-picker-row :label (if (plusp (length world)) world "this box") :heading t)
                        (loop for item in items
                              for status = (json-value item :string "status")
                              for reason = (json-value item :string "reason")
                              when (equal world (or (json-value item :string "world") ""))
                                collect (make-picker-row
                                         :label (format nil "~a ~a"
                                                        (cond ((equal status "imported") "✓")
                                                              ((equal status "error") "!")
                                                              (t "–"))
                                                        (setup-import-item-label item))
                                         :detail (if (plusp (length reason)) reason (or status ""))
                                         :value item))))
     (lambda (model value) (declare (ignore value)) (funcall back model) nil)
     :context "Details"
     :subtitle (import-summary-line report)
     :kind :setup-details
     :hint "enter or esc back  ↑↓ scroll"
     :on-cancel back)
    (let ((dialog (chat-model-dialog model)))
      (picker-seat-top dialog)
      (when run (setf (setup-page-run dialog) run)))))

(defun setup-caveats (run &aux (pin (multiple-value-list (shared-model-pin)))
                               (pinned (first pin))
                               (refusal (and pinned (provider-turn-refusal pinned)))
                               (problems (remove-if-not #'health-problem-p
                                                        (or (ignore-errors (health-facts)) '()))))
  "What qualifies RUN's outcome, as (LABEL TEXT) in the order it is said:
first what stands between a turn and the provider it runs on -- the default
model when no turn can run on its provider (PROVIDER-TURN-REFUSAL), each other
provider the walk connected that no turn can run on, each key of the default
model's provider or of one the walk connected that did not answer its check
(*KEY-CHECKS*), the model's own problem, a model pick the config did not take
-- then each caveat the walk noted, then each other problem the organism
still has."
  ;; What the walk did, and then what is still missing. A wizard that ends on
  ;; its own summary tells the operator what IT touched, which is not the same
  ;; question as whether the organism is ready — a walk can finish cleanly over
  ;; a refused model pin, a folder that failed, or an imported key nothing
  ;; answered for, and the block would read like success. The provider's come
  ;; first, so the last page leads with them. A key's standing is read, never
  ;; asked again: without this, a key typed at Connect and saved `not checked'
  ;; would leave nothing here once its dialog closed, and a refused imported
  ;; key replaced during the walk would still read refused. A provider no turn
  ;; can run on is that, and not a key to check again: without this, a provider
  ;; entry with no base_url would be re-checked as a key and the toast would
  ;; name the model pick instead of the missing base_url.
  (flet ((problem (fact)
           (list "problem"
                 (if (eq fact.key :cell)
                     ;; its first failure was the compiler's whole log, forty rows of Lisp
                     (format nil "~a did not load — /cells names why" (cell-label fact.label))
                     (format nil "~a: ~a~@[ — ~a~]" fact.label fact.detail fact.remedy)))))
    (append
     (when refusal
       (list (list "problem" (format nil "~@[~a on ~]~a cannot run a turn — ~a" (second pin) pinned refusal))))
     (loop for id in (remove-duplicates (remove nil (cons pinned (setup-run-providers run)))
                                        :test #'string-equal :from-end t)
           for pin-p = (and pinned (string-equal id pinned))
           for refused = (if pin-p refusal (provider-turn-refusal id))
           for (verdict words) = (gethash id *key-checks*)
           for answer = (cdr (assoc id (setup-run-answers run) :test #'string-equal))
           for line = (cond ((and refused pin-p) nil)
                            (refused (list "problem" (format nil "~a cannot run a turn — ~a" id refused)))
                            ((eq verdict :refused)
                             (list "check" (format nil "~a~@[ with ~a~] — /models saves a new key"
                                                   words answer)))
                            ((eq verdict :unchecked)
                             (list "check" (format nil "~a — /models takes it again and checks it" words)))
                            (verdict (list "check" words)))
           when line collect line)
     ;; the pin's refusal is said above, as what no turn can run on
     (mapcar #'problem (remove-if-not (lambda (fact)
                                        (and (eq (health-fact-key fact) :model)
                                             (not (and refusal (equal refusal (health-fact-remedy fact))))))
                                      problems))
     (when-let (unsaved (setup-run-unsaved run))
       (list (list "check" (format nil "~{~a~^/~} was not saved: ~a"
                                   (first unsaved) (second unsaved)))))
     (mapcar (lambda (text) (list "check" text)) (reverse (setup-run-caveats run)))
     ;; the cells tally is said folder by folder, each by its own fact
     (mapcar #'problem (remove-if (lambda (fact) (member fact.key '(:model :cells))) problems)))))

(defun setup-first-run-p (run)
  "The short first-launch walk, not the explicit import walk."
  (and run (member :effort (setup-run-steps run)) t))

(defun setup-discovery-entries (value &optional (lines '(:keys :configs :memory)))
  "What the import owner located in VALUE, LINES of it, as ledger lines (KEY
MARK LABEL TEXT): how many, and from where; no secret values. Configs are
distinct source files; memory systems are distinct source worlds."
  (let ((items (json-value value :array "items")) (keys '()) (configs '()) (memories '()))
    (unless items
      (return-from setup-discovery-entries
        (list (list :keys "!" "home" "could not be read, so nothing in it is counted"))))
    (loop for item across items
          for kind = (json-value item :string "kind")
          for destination = (json-value item :string "destination")
          for source = (json-value item :string "source")
          for world = (json-value item :string "world")
          when (member (json-value item :string "status") '("imported" "conflict") :test #'equal)
            do (cond ((and (equal kind "providers") destination
                           (uiop:string-prefix-p "auth.json api_keys." destination))
                      (pushnew (list destination world) keys :test #'equal :key #'first))
                     ((equal kind "memory") (when world (pushnew world memories :test #'equal))))
               (when (and (member kind '("providers" "mcp" "channels" "cron" "standing") :test #'equal)
                          source (plusp (length source)))
                 (pushnew (list world source) configs :test #'equal)))
    (flet ((entry (key count one many text)
             (list key (if (plusp count) "✓" "–") (format nil "~d ~a" count (if (= 1 count) one many))
                   text))
           (worlds (names)
             (format nil "~{~a~^ · ~}"
                     (remove-duplicates (remove-if-not #'plusp names :key #'length)
                                        :test #'equal :from-end t))))
      (loop for line in lines
            collect (ecase line
                      (:keys (entry :keys (length keys) "key" "keys"
                                    (format nil "~{~a~^, ~}"
                                            (loop for (destination world) in (reverse keys)
                                                  collect (format nil "~a~@[ from ~a~]"
                                                                  (subseq destination (length "auth.json api_keys."))
                                                                  (and (plusp (length world)) world))))))
                      (:configs (entry :configs (length configs) "config" "configs"
                                       (worlds (mapcar (lambda (pair) (or (first pair) "")) (reverse configs)))))
                      (:memory (entry :memory (length memories) "memory" "memories"
                                      (worlds (reverse memories)))))))))

(defun setup-stage-effort (model run)
  "The selected model's own reasoning ladder, through /think's writer."
  (with-handlers ((error (condition)
                    (setup-caveat run "reasoning controls unavailable: ~a" condition)
                    (setup-stage-finish model run)))
    (labels ((finish (model) (setup-stage-finish model run))
             (pick (model effort)
               (with-handlers ((error (condition)
                                 (setup-caveat run "reasoning effort was not saved: ~a" condition)
                                 (finish model)))
                 (apply-effort model effort)
                 (setup-note run "reasoning effort: ~a" (or effort "provider default"))
                 (finish model))))
      (open-effort-view model #'pick
                        (lambda (model value) (declare (ignore value)) (finish model)))
      (let ((dialog (chat-model-dialog model)))
        (setf (picker-dialog-context dialog) "Step 3. Choose reasoning effort"
              (picker-dialog-on-cancel dialog) #'finish
              (picker-arrow-handlers dialog) (setup-step-arrows run :effort)
              (picker-dialog-hint dialog) "enter choose  esc keep current  ← back  ↑↓ navigate")))))
