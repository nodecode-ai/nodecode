;;;; provider-offers-test.lisp --- a provider a cell serves, offered by
;;;; /models before that cell is installed, and installed when it is picked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The folder's .asd says what it serves; Nodecode's index carries a copy,
;;;; held to the .asd when the folder is cloned. The picker lists each such
;;;; provider of a folder this home lacks, and Enter installs the folder, then
;;;; asks what the provider asks for: its key, or the command it signs in with.

(in-package #:nodecode.test)

(defparameter +go-asd+
  "(defsystem \"nl-hub-go\" :properties ((:serves (:provider \"nl-go\" :name \"NL Go\" :sign-in \"key\"))) :serial t :components ((:file \"cell\")))"
  "The .asd of a folder serving the provider nl-go, signed in to with a key.")

(defun go-index (serves)
  "A format-1 hub index of one entry, nl-hub-go, whose serves is the JSON text SERVES."
  (format nil "{\"format\": 1, \"cells\": [{\"name\": \"nl-hub-go\", \"git\": \"https://example.org/o/c.git\", \"commit\": ~s, \"description\": \"\", \"serves\": ~a}]}"
          (make-string 40 :initial-element #\a) serves))

(deftest cells-serves-is-read-alike-from-the-asd-and-the-index-and-held-at-the-clone ()
  ;; One shape from both: the .asd's (:serves (:provider ... :sign-in ...))
  ;; and the index's {\"provider\": ..., \"sign_in\": ...} compare equal, and the
  ;; clone refuses an index entry that offers a folder by what the folder
  ;; does not say it serves.
  (let ((indexed (nlk:offer-serves
                  (first (nlk:read-hub-index
                          (go-index "[{\"provider\": \"nl-go\", \"name\": \"NL Go\", \"sign_in\": \"key\"}]")
                          :shipped '())))))
    (is (equal '((("name" . "NL Go") ("provider" . "nl-go") ("sign_in" . "key"))) indexed))
    (with-temp-trees ((remote "serves-remote") (home "serves-home"))
      (let ((root (merge-pathnames "cells/" home)))
        (multiple-value-bind (git commit)
            (hub-fixture-repository remote "nl-hub-go"
                                    (lambda (folder)
                                      (write-temp-file (merge-pathnames "nl-hub-go.asd" folder) +go-asd+)))
          (let ((asd (nlk::declared-system-serves
                      (first (nlk::cell-folder-systems
                              (nlk::make-cell-folder (merge-pathnames "nl-hub-go/" remote)))))))
            (is (equal indexed asd) "the .asd's plist reads as the index's object"))
          (let ((other (hub-offer "nl-hub-go" git commit
                                  :serves (nlk::serves-entries (list '(:provider "nl-other"))))))
            (is (search "serves" (hub-refusal other root)) "an index offering it by another provider is refused"))
          (is (null (nlk::cell-folders root)) "and leaves nothing to load")
          (is (hub-install (hub-offer "nl-hub-go" git commit :serves indexed) root) "the index's own copy installs")))))
  (dolist (serves '("\"nl-go\"" "[\"nl-go\"]" "[{\"provider\": 7}]" "[{}]"))
    (is (search "serves is an array" (refusal-text error (nlk:read-hub-index (go-index serves) :shipped '())))
        serves)))

(deftest provider-offers-name-the-providers-of-folders-this-home-lacks ()
  ;; Each provider an offered folder serves, with the pin a picker installs it
  ;; at; a folder this image could not install is not offered, and one already
  ;; in is offered only for a provider that does not answer yet (a sign-in Esc
  ;; left unfinished), marked :INSTALLED, so the list keeps it.
  (let* ((commit (make-string 40 :initial-element #\b))
         (serves (nlk::serves-entries (list '(:provider "nl-go" :name "NL Go")
                                            '(:provider "nl-codex" :name "NL Codex" :sign-in "/nl-codex login"))))
         (nle::*provider-offers*
           (cons (get-universal-time)
                 (list (nlk::make-offer :name "nl-cell-go" :git "https://example.org/c.git" :commit commit
                                        :description "Go" :serves serves :index "hub")
                       (nlk::make-offer :name "nl-cell-blocked" :serves serves :missing '("nl-no-such"))
                       (nlk::make-offer :name "nl-cell-quiet")))))
    (is (equal (list (list :cell "nl-cell-go" :commit commit :provider "nl-go" :name "NL Go"
                           :sign-in "key" :description "Go")
                     (list :cell "nl-cell-go" :commit commit :provider "nl-codex" :name "NL Codex"
                           :sign-in "/nl-codex login" :description "Go"))
               (nle::provider-offers)))
    (with-stubbed-fdefinitions ((nlk:installed-cell-p (name &optional root)
                                  (and (equal name "nl-cell-go") (or root t)))
                                (nle::connected-provider-ids () '("nl-go")))
      (is (equal (list (list :cell "nl-cell-go" :commit commit :provider "nl-codex" :name "NL Codex"
                             :sign-in "/nl-codex login" :description "Go" :installed t))
                 (nle::provider-offers)) "an installed folder is offered only for the provider not answering yet"))))

(defun go-offer (&key (sign-in "key") (provider "nl-go") (cell "nl-cell-go"))
  "A PROVIDER-OFFERS plist for CELL serving PROVIDER, signed in to by SIGN-IN."
  (list :cell cell :commit (make-string 40 :initial-element #\c) :provider provider
        :name (if (equal provider "nl-go") "NL Go" "NL Codex") :sign-in sign-in :description ""))

(deftest connect-list-offers-a-provider-its-uninstalled-folder-serves ()
  ;; models.dev's row for a provider a folder serves becomes the folder's --
  ;; a key saved from it alone would reach a lane that cannot talk to it --
  ;; and a provider models.dev lacks is listed by the folder's name for it.
  (with-model-listings (:catalog "{
  \"nl-go\": {\"name\": \"NL Go\", \"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://nl-go.test/v1\", \"models\": {\"g\": {\"tool_call\": true}}},
  \"acme-labs\": {\"name\": \"Acme Labs\", \"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://acme-labs.test/v1\", \"models\": {\"one\": {\"tool_call\": true}}}
}")
    (with-stubbed-fdefinition (nle::picker-provider-offers ()
                                (list (go-offer)
                                      (go-offer :provider "nl-codex" :cell "nl-cell-codex"
                                                :sign-in "/nl-codex login")))
      (let* ((rows (nle::connect-picker-rows))
             (go (find "NL Go" rows :key #'nle::picker-row-label :test #'equal))
             (codex (find "NL Codex" rows :key #'nle::picker-row-label :test #'equal))
             (acme (find "Acme Labs" rows :key #'nle::picker-row-label :test #'equal)))
        (is (= 1 (count "NL Go" rows :key #'nle::picker-row-label :test #'equal)) "one row, not two")
        (is (equal "1 model" (nle::picker-row-detail go)) "the row says nothing of the install")
        (is (equal "nl-cell-go" (getf (getf (nle::picker-row-value go) :install) :cell)))
        (is (equal "nl-go" (getf (nle::picker-row-value go) :provider)))
        (is (equal "" (nle::picker-row-detail codex)))
        (is (equal "nl-codex" (getf (nle::picker-row-value codex) :provider)))
        (is (null (getf (nle::picker-row-value acme) :install)) "a provider no folder serves is as it was")))))

(deftest a-connected-provider-its-folder-serves-keeps-the-folders-name ()
  ;; Picked, installed and answering, Claude Code's provider is listed as any
  ;; connected one is, and models.dev has no row for it: it read `claude-code',
  ;; so typing `claude pro' found nothing. Its folder's name stands, from the
  ;; hub's read in the serving image, and a shell takes the gateway's.
  (let ((nle::*provider-offers*
          (cons (get-universal-time)
                (list (nlk::make-offer :name "nl-cell-codex" :commit (make-string 40 :initial-element #\d)
                                       :serves (nlk::serves-entries (list '(:provider "nl-codex" :name "NL Codex"))))))))
    (with-stubbed-fdefinition (nlk:installed-cell-p (name &optional root) name root t)
      (is (equal "NL Codex" (nle::offered-provider-name "nl-codex")) "installed, the folder still names it"))
    (is (null (nle::offered-provider-name "nl-other")))
    ;; the serving image's list (WITH-MODEL-LISTINGS serves)
    (with-model-listings (:catalog "{}")
      (with-stubbed-fdefinitions ((nle::picker-connected-provider-ids () '("nl-codex" "nl-bare"))
                                  (nle::picker-provider-offers () '()))
        (let ((labels (mapcar #'nle::picker-row-label (nle::connect-picker-rows))))
          (is (member "NL Codex" labels :test #'equal) "listed by its folder's name")
          (is (not (member "nl-codex" labels :test #'equal)) "not by its id")
          (is (member "nl-bare" labels :test #'equal) "a provider no folder names keeps its id")))))
  (let ((nle::*serving* nil)
        (nle::*gateway-models*
          (nle::gateway-models-facts
           (nlk:decode-json "{\"models\": [], \"providers\": [{\"id\": \"nl-codex\", \"connected\": true, \"name\": \"NL Codex\"}, {\"id\": \"nl-bare\", \"connected\": true}]}"))))
    (is (equal "NL Codex" (nle::picker-provider-name "nl-codex")) "a shell reads the gateway's name")
    (is (null (nle::picker-provider-name "nl-bare")))))

(deftest a-walk-provider-list-takes-the-offers-the-gateway-list-brings ()
  ;; The setup walk's provider list (a connect dialog of no kind) opens
  ;; before the gateway's list lands, and the offers come with that list:
  ;; its landing restocks the walk as it does /models', or a fresh home's
  ;; walk never lists OpenCode Go's or Codex's folder.
  (let ((offers '()))
    (with-frame-model (model 100 30 :session-id "offer-walk")
      (nle::open-connect-dialog
       model (lambda ()
               (loop for offer in offers
                     collect (nle::make-picker-row :label (getf offer :name)
                                                   :value (list :provider (getf offer :provider)
                                                                :install offer))))
       (nlk:lambda-ignoring (model provider-id secret done) nil)
       nil)
      (is (null (nle::picker-dialog-rows model.dialog)) "nothing landed yet")
      (setf offers (list (go-offer)))
      (nle::install-refreshed-models model)
      (is (equal '("NL Go") (mapcar #'nle::picker-row-label (nle::picker-dialog-rows model.dialog))) "the landed list's offer is listed"))))

(deftest connect-an-offered-provider-its-folder-signs-in-goes-on-to-its-models ()
  ;; Claude Code's folder brings the claude CLI's own login: no key, no
  ;; command. Once it is in, the reread rows list the provider connected,
  ;; and the pick goes on to its models as Enter on a connected provider does.
  (let ((in nil) (picked '()))
    (with-stubbed-fdefinitions ((nle::install-offered-cell (model offer)
                                  model offer
                                  (setf in t)
                                  (list :commands '())))
      (with-frame-model (model 100 30 :session-id "offer-own-login")
        (nle::open-connect-dialog
         model (lambda ()
                 (list (nle::make-picker-row
                        :label "NL Claude"
                        :value (if in
                                   (list :provider "nl-claude" :connected t)
                                   (list :provider "nl-claude"
                                         :install (go-offer :provider "nl-claude" :cell "nl-cell-claude"))))))
         (nlk:lambda-ignoring (model provider-id secret done) nil)
         (lambda (model &optional provider) model (push provider picked)))
        (dialog-keys model :enter)
        (is in "the folder is installed")
        (is (equal '("nl-claude") picked) "then the pick goes on to its models")
        (is (null model.dialog) "with no key asked")))))

(defun open-offer-wizard (model offer)
  "The /models wizard over MODEL listing OFFER's provider as the picker does;
=> the dialog."
  (nle::open-connect-dialog
   model (lambda ()
           (list (nle::make-picker-row :label (getf offer :name)
                                       :value (list :provider (getf offer :provider) :install offer))))
   (nlk:lambda-ignoring (model provider-id secret done) nil)
   nil)
  model.dialog)

(deftest connect-enter-on-an-offered-provider-installs-its-folder-then-signs-in ()
  ;; Enter installs the folder through the organism, at the pin the list
  ;; read, then goes on as the provider asks: here its key step (a sign-in
  ;; command's wait is CONNECT-SIGN-IN's gate). A refused install says why
  ;; over the list and moves nothing else.
  (let ((installed '()))
    (with-stubbed-fdefinitions ((nle::install-offered-cell (model offer)
                                  model
                                  (push offer installed)
                                  (if (equal "nl-cell-gone" (getf offer :cell))
                                      (list :refused "the hub has no nl-cell-gone")
                                      (list :commands '()))))
      (with-frame-model (model 100 30 :session-id "offer-key")
        (let ((dialog (open-offer-wizard model (go-offer))))
          (dialog-keys model :enter)
          (is (equal (list (go-offer)) installed) "the folder, at the pin the list read")
          (is (eq :api-key (nle::connect-dialog-step dialog)) "then its key")
          (is (equal "nl-go" (nle::connect-dialog-provider-id dialog)))))
      (with-frame-model (model 100 30 :session-id "offer-refused")
        (let ((dialog (open-offer-wizard model (go-offer :cell "nl-cell-gone"))))
          (dialog-keys model :enter)
          (is (eq :provider (nle::connect-dialog-step dialog)) "back on the list")
          (is (rows-carry-p "nl-cell-gone was not installed: the hub has no nl-cell-gone"
                            (nle::compose-frame model))))))))

(deftest connect-a-folder-already-in-goes-straight-to-its-sign-in ()
  ;; Esc out of a sign-in leaves the folder installed and its provider not
  ;; answering: the list still offers it (:INSTALLED), and Enter goes on to
  ;; its sign-in with nothing installed again.
  (let ((installs 0))
    (with-stubbed-fdefinition (nle::install-offered-cell (model offer)
                                model offer
                                (incf installs)
                                (list :commands '()))
      (with-frame-model (model 100 30 :session-id "offer-in-already")
        (let ((dialog (open-offer-wizard model (append (go-offer) (list :installed t)))))
          (dialog-keys model :enter)
          (is (= 0 installs) "nothing installed again")
          (is (eq :api-key (nle::connect-dialog-step dialog)) "straight to its key"))))))

(deftest an-installed-folder-whose-cell-waits-for-its-start-is-offered-starting ()
  ;; A cold boot reads /models seconds before its cells start, behind the
  ;; listener: an installed folder's provider answers nothing yet, and its
  ;; offer says the cell is still to start, so a pick waits for the start
  ;; rather than asking for a key -- Claude Code's asked for one (tryingET,
  ;; 2026-10-30). Started, or refused, it says nothing of the kind.
  (let* ((commit (make-string 40 :initial-element #\b))
         (cell (nlk::make-cell :name "nl-cell-go" :kind :peripheral
                               :start (lambda (config) config nil)))
         (nlk::*cells* (list cell))
         ;; the boot's start pass under way (START-CELLS-BEHIND-THE-LISTENER)
         (nlk::*cells-starting* :a-start-pass)
         (nle::*provider-offers*
           (cons (get-universal-time)
                 (list (nlk::make-offer :name "nl-cell-go" :git "https://example.org/c.git" :commit commit
                                        :description "Go" :index "hub"
                                        :serves (nlk::serves-entries (list '(:provider "nl-go" :name "NL Go"))))))))
    (with-stubbed-fdefinition (nle::connected-provider-ids () '())
      (is (equal (list (list :cell "nl-cell-go" :commit commit :provider "nl-go" :name "NL Go"
                             :sign-in "key" :description "Go" :installed t :starting t))
                 (nle::provider-offers)) "loaded, its start to come")
      (let ((nlk::*cells-starting* nil))
        (is (equal '(:installed t) (last (first (nle::provider-offers)) 2)) "no start pass under way, none to wait for"))
      (setf (nlk:cell-refusal cell) "no claude on PATH")
      (is (equal '(:installed t) (last (first (nle::provider-offers)) 2)) "a refused start is none to wait for")
      (setf (nlk:cell-refusal cell) nil
            (nlk:cell-started-p cell) t)
      (is (equal '(:installed t) (last (first (nle::provider-offers)) 2)) "started"))))

(deftest a-cell-still-to-start-is-what-the-providers-its-folder-serves-wait-on ()
  ;; What a turn and the list wait on in a boot's first seconds is read off
  ;; the folder itself, its .asd's serves lines: only the provider a cell
  ;; still to start serves waits, and only until that cell has started.
  (with-temp-trees ((root "starting-serves"))
    (let ((folder (merge-pathnames "nl-hub-go/" root)))
      (write-temp-file (merge-pathnames "nl-hub-go.asd" folder) +go-asd+)
      (let* ((cell (nlk::make-cell :name "nl-hub-go" :path folder :kind :peripheral
                                   :start (lambda (config) config nil)))
             (nlk::*cells* (list cell))
             (nlk::*cells-starting* :a-start-pass))
        (is (equal '("nl-go") (nlk::cell-served-providers cell)) "what its .asd says it serves")
        (is (equal (list cell) (nlk::starting-cells-serving "nl-go")) "its start to come")
        (is (null (nlk::starting-cells-serving "nl-other")) "a provider it does not serve")
        (let ((nlk::*cells-starting* nil))
          (is (null (nlk::starting-cells-serving "nl-go")) "no start pass under way"))
        (setf (nlk:cell-started-p cell) t)
        (is (null (nlk::starting-cells-serving "nl-go")) "started")))))

(deftest the-gateway-list-says-which-folder-in-already-is-still-to-start (with-temp-gateway (port))
  ;; A shell reads the offers off its organism's list: one whose cell is
  ;; still to start says so, and the shell's facts keep it.
  (with-saved-globals ((nle::*provider-models-cache-path* nil) (nle::*models-catalog-path* nil))
    (with-stubbed-fdefinition (nle::provider-offers ()
                                (list (append (go-offer :provider "nl-claude" :cell "nl-cell-claude")
                                              (list :installed t :starting t))
                                      (go-offer)))
      (with-gateway-http (port :get "/api/gateway/models" :status 200)
        (let ((offers (nlk:json-value body :array "offers")))
          (is (eq t (gethash "starting" (aref offers 0))) "the one still to start")
          (is (not (nth-value 1 (gethash "starting" (aref offers 1)))) "and only that one"))
        (is (equal '(t nil) (mapcar (lambda (offer) (getf offer :starting))
                                    (getf (nle::gateway-models-facts body) :offers))) "the shell's facts keep it")))))

(deftest connect-list-says-a-folder-in-already-is-still-starting ()
  ;; Not among the connected yet, and why: its cell is starting.
  (with-model-listings (:catalog "{
  \"nl-go\": {\"name\": \"NL Go\", \"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://nl-go.test/v1\", \"models\": {\"g\": {\"tool_call\": true}}}
}")
    (with-stubbed-fdefinition (nle::picker-provider-offers ()
                                (list (append (go-offer) (list :installed t :starting t))
                                      (append (go-offer :provider "nl-codex" :cell "nl-cell-codex"
                                                        :sign-in "/nl-codex login")
                                              (list :installed t :starting t))))
      (let ((rows (nle::connect-picker-rows)))
        (is (equal "1 model  starting"
                   (nle::picker-row-detail (find "NL Go" rows :key #'nle::picker-row-label :test #'equal))))
        (is (equal "starting"
                   (nle::picker-row-detail (find "NL Codex" rows :key #'nle::picker-row-label :test #'equal))))))))

(deftest connect-a-folder-whose-cell-is-starting-waits-for-it-then-goes-on-to-its-models ()
  ;; Enter on Claude Code's row before the organism's cells started asked
  ;; for an API key, which its login -- the claude CLI's -- never takes
  ;; (tryingET, 2026-10-30). The wizard waits on the start, its clock
  ;; counting, and once the gateway's list reads the provider connected the
  ;; pick goes on to its models, with no key asked.
  (let* ((reads 0) (picked '()) (seen nil)
         (offer (append (go-offer :provider "nl-claude" :cell "nl-cell-claude")
                        (list :installed t :starting t))))
    (with-stubbed-fdefinitions ((nle::install-offered-cell (model offer)
                                  (error "~a installed again for ~a" offer model))
                                (nle::picker-connected-provider-ids ()
                                  (if (< reads 2) '() '("nl-claude")))
                                (nle::picker-provider-offers ()
                                  (if (< reads 2) (list offer) '())))
      (let ((nle::*connect-start-poll-seconds* 0))
        (with-frame-model (model 100 30 :session-id "offer-starting")
          (nle::open-connect-dialog
           model (lambda ()
                   (list (nle::make-picker-row :label "NL Claude"
                                               :value (list :provider "nl-claude" :install offer))))
           (nlk:lambda-ignoring (model provider-id secret done) nil)
           (lambda (model &optional provider) model (push provider picked)))
          (with-stubbed-fdefinition (nle::fetch-gateway-models (m)
                                      (incf reads)
                                      (when (= reads 1)
                                        (let ((frame (nle::compose-frame m)))
                                          (setf seen (list (nle::connect-dialog-step (nle::chat-model-dialog m))
                                                           (rows-carry-p "Starting nl-cell-claude, the cell that serves this provider." frame)
                                                           (rows-carry-p "starting…" frame)))))
                                      nil)
            (dialog-keys model :enter))
          (is (equal '(:starting t t) seen) "the wizard waits on the start, its clock counting")
          (is (= 2 reads) "read again while the cell was starting")
          (is (equal '("nl-claude") picked) "once it answers, the pick goes on to its models")
          (is (null model.dialog) "with no key asked"))))))

(deftest connect-a-folder-whose-cell-started-without-answering-goes-on-as-it-asks ()
  ;; Up, and its provider still not answering -- a key folder with no key
  ;; yet -- the wizard goes on as the provider asks: its key step. Ctrl+E
  ;; asks for a key at once, whatever the cell is doing.
  (let ((offer (append (go-offer) (list :installed t :starting t))) (reads 0))
    (with-stubbed-fdefinitions ((nle::picker-connected-provider-ids () '())
                                (nle::picker-provider-offers () (list (append (go-offer) (list :installed t))))
                                (nle::fetch-gateway-models (model) model (incf reads) nil))
      (let ((nle::*connect-start-poll-seconds* 0))
        (with-frame-model (model 100 30 :session-id "offer-started-quiet")
          (let ((dialog (open-offer-wizard model offer)))
            (dialog-keys model :enter)
            (is (eq :api-key (nle::connect-dialog-step dialog)) "its key, once its cell is up")
            (is (= 1 reads))))
        (with-frame-model (model 100 30 :session-id "offer-starting-key")
          (let ((dialog (open-offer-wizard model offer)))
            (setf reads 0)
            (nle::connect-dialog-key model dialog #\e :ctrl t)
            (is (eq :api-key (nle::connect-dialog-step dialog)) "Ctrl+E: a key now")
            (is (= 0 reads) "no wait")))))))

(deftest connect-steps-that-wait-on-the-organism-take-no-enter-and-esc-goes-back ()
  ;; A key's check, a folder's install, its cell's start, its sign-in: the
  ;; organism answers, and Enter meanwhile moves nothing -- on the sign-in
  ;; step it fell through the Enter dispatch's ECASE. Esc goes back to the list.
  (dolist (step '(:checking :installing :signing-in :starting))
    (with-frame-model (model 100 30 :session-id (format nil "wait-~(~a~)" step))
      (let ((dialog (open-offer-wizard model (go-offer))))
        (nle::connect-enter-step dialog step)
        (dialog-keys model :enter)
        (is (eq step (nle::connect-dialog-step dialog)) (format nil "Enter on ~(~a~)" step))
        (unless (eq step :checking)
          (dialog-keys model :escape)
          (is (eq :provider (nle::connect-dialog-step dialog)) (format nil "Esc on ~(~a~)" step)))))))

(deftest sign-in-facts-read-the-page-and-the-device-code ()
  ;; What the sign-in panel shows of a folder's answer: its first page that
  ;; is not the loopback the browser comes back to, and the device code --
  ;; never a word that only follows `code' (a product called Kimi Code).
  (flet ((facts (text) (multiple-value-list (nle::sign-in-facts text))))
    (is (equal '("https://www.kimi.com/code/authorize_device?user_code=NFGT-NHBJ" "NFGT-NHBJ")
               (facts "Kimi Code sign-in: open https://www.kimi.com/code/authorize_device?user_code=NFGT-NHBJ and enter the code NFGT-NHBJ.")))
    (is (equal '("https://github.com/login/device" "FFC7-AECC")
               (facts "Open https://github.com/login/device and enter the code FFC7-AECC to sign in.")))
    (is (equal '("https://auth.example.test/a?x=1" nil)
               (facts "Open: https://auth.example.test/a?x=1 then return to http://localhost:1455/cb, or send /nl code ADDRESS.")))
    (is (equal '(nil nil) (facts "port 1455 is in use; only http://localhost:1455/auth/callback is allowed")))))

(deftest connect-a-sign-in-that-names-no-page-says-why-over-the-list ()
  ;; A command that answers no page to sign in at -- a refusal naming only
  ;; the loopback address its browser would come back to -- waits for
  ;; nothing: the list comes back and says why, as a refused install does.
  (with-stubbed-fdefinitions ((nle::install-offered-cell (model offer) model offer (list :commands '()))
                              (nle::gateway-slash-answer (model line)
                                model line
                                (nlk:decode-json "{\"text\": \"nl-codex: callback port 1455 is in use; OpenAI accepts only http://localhost:1455/auth/callback\"}"))
                              (nle::open-target (url) url (error "nothing to open")))
    (with-frame-model (model 100 30 :session-id "offer-no-page")
      (let ((dialog (open-offer-wizard model (go-offer :provider "nl-codex" :cell "nl-cell-codex"
                                                       :sign-in "/nl-codex login"))))
        (dialog-keys model :enter)
        (is (eq :provider (nle::connect-dialog-step dialog)) "back on the list")
        (is (rows-carry-p "nl-codex: callback port 1455 is in use" (nle::compose-frame model)))))))

(deftest connect-an-offered-provider-signs-in-in-the-wizard-then-goes-on-to-its-models ()
  ;; A folder that signs in with a command (Codex's /openai-codex login):
  ;; once it is in, the command runs, its page opens in the browser, and the
  ;; wizard stays open on where to sign in, the whole address verbatim
  ;; (dimmed), the device code when there is one, none of the command's
  ;; prose -- until the gateway's list
  ;; reads the provider connected; then the pick goes on to its models. It
  ;; used to close the list and run the command, which ended a setup walk on
  ;; its summary with the address cut off in the status line.
  (let ((ran '()) (picked '()) (reads 0) (seen nil) (opened '())
        (address "https://auth.example.test/authorize?client_id=nl&state=0123456789abcdef"))
    (with-stubbed-fdefinitions ((nle::install-offered-cell (model offer)
                                  model offer
                                  (list :commands '()))
                                (nle::gateway-slash-answer (model line)
                                  model
                                  (push line ran)
                                  (nlk:decode-json
                                   (format nil "{\"text\": \"Open this address to sign in:\\n\\n~a\\n\\nthen enter the code ABCD-1234. If the browser runs elsewhere, send /nl-codex code ADDRESS.\"}" address)))
                                (nle::open-target (url) (push url opened) t)
                                (nle::picker-connected-provider-ids ()
                                  (incf reads)
                                  (if (= reads 1) '() '("nl-codex"))))
      (let ((nle::*connect-sign-in-poll-seconds* 0))
        (with-frame-model (model 100 30 :session-id "offer-sign-in")
          (nle::open-connect-dialog
           model (lambda ()
                   (list (nle::make-picker-row
                          :label "NL Codex"
                          :value (list :provider "nl-codex"
                                       :install (go-offer :provider "nl-codex" :cell "nl-cell-codex"
                                                          :sign-in "/nl-codex login")))))
           (nlk:lambda-ignoring (model provider-id secret done) nil)
           (lambda (model &optional provider) model (push provider picked)))
          (with-stubbed-fdefinition (nle::fetch-gateway-models (m)
                                      ;; the first read: the panel waits on the folder's words
                                      (when (and (= reads 0) (null seen))
                                        (let ((frame (nle::compose-frame m)))
                                          (setf seen (list (nle::connect-dialog-step (nle::chat-model-dialog m))
                                                           (rows-carry-p "Sign in at auth.example.test/authorize" frame)
                                                           (rows-carry-p "Code ABCD-1234" frame)
                                                           (rows-carry-p address frame)
                                                           ;; one blank row between each part, the title's too
                                                           (let ((at (mapcar (lambda (needle) (row-index-of needle frame))
                                                                             (list "Connect Sign in" "waiting for the sign-in" "Sign in at" address "Code ABCD-1234"))))
                                                             (and (every #'integerp at)
                                                                  (mapcar #'- (rest at) (butlast at))))
                                                           (rows-carry-p "Open this address" frame)
                                                           (rows-carry-p "browser runs elsewhere" frame)))))
                                      nil)
            (dialog-keys model :enter))
          (is (equal '("/nl-codex login") ran) "the folder's sign-in runs once")
          (is (equal '(:signing-in t t t (2 2 2 2) nil nil) seen) "the wizard waits on where to sign in, the whole address, the code, a blank row apart, no prose")
          (is (equal (list address) opened) "the page opens in the browser")
          (is (equal '("nl-codex") picked) "once it answers, the pick goes on to its models")
          (is (null model.dialog)))))))

(deftest control-keys-check-answers-the-verdict-and-saves-nothing (with-temp-gateway (port))
  ;; A shell asks here whether a key works: the address a turn dials is set
  ;; where the cells run (OpenCode Go's cell gives Go's), and the check is the
  ;; organism's. Nothing is saved: the shell saves what the verdict lets it.
  (with-saved-globals ((nle::*provider-models-cache-path* nil) (nle::*models-catalog-path* nil))
    (with-stubbed-fdefinition (nle::provider-key-check (provider key &key vars)
                                (if (equal key "sk-good")
                                    (values :works nil)
                                    (values :refused (format nil "~a refused this key (HTTP 401)" provider))))
      (with-gateway-http (port :post "/api/gateway/keys" :status 200
                          :body "{\"provider\": \"nl-go\", \"key\": \" sk-good \", \"check\": true}")
        (is (equal "works" (nlk:json-value body :string "verdict")))
        (is (null (nlk:json-value body :array "providers")) "the verdict, not the listing"))
      (with-gateway-http (port :post "/api/gateway/keys" :status 200
                          :body "{\"provider\": \"nl-go\", \"key\": \"sk-bad\", \"check\": true}")
        (is (equal "refused" (nlk:json-value body :string "verdict")))
        (is (equal "nl-go refused this key (HTTP 401)" (nlk:json-value body :string "words"))))
      (let ((saved (nlk:json-value (nle::read-auth-file nle::*auth-file-path*) :object "api_keys" "nl-go")))
        (is (null saved) "nothing was saved")))))

(deftest connect-credential-sink-in-a-shell-asks-its-organism ()
  ;; A shell runs no cell, so the key is checked where the provider's lane
  ;; runs: the organism's verdict, said as the shell's own would be.
  (with-model-listings (:catalog +connect-catalog-fixture+)
    (with-frame-model (model 80 24 :session-id "connect-sink-shell")
      (let ((asked '()) (said nil) (nle::*serving* nil))
        (clrhash nle::*key-checks*)
        (with-stubbed-fdefinitions ((nle::provider-key-check (provider key &key vars)
                                      (error "a shell checked ~a with ~a itself" provider key))
                                    (nle::gateway-model-http-call (model method path body &rest keys)
                                      (push (list method path (and body (nlk:json-value body :boolean "check")))
                                            asked)
                                      (progn model keys
                                           (if (equal path "/api/gateway/keys")
                                               (nlk:json-object "verdict" "works")
                                               (nlk:json-object "models" #() "providers" #())))))
          (funcall (nle::connect-credential-sink) model "anthropic" "sk-1"
                   (nlk:lambda-ignoring (model message error) (setf said (list message error)))))
        (is (equal '(:post "/api/gateway/keys" t) (car (last asked))) "the organism was asked")
        (is (equal '("Anthropic key works, saved to auth.json" nil) said))))))
