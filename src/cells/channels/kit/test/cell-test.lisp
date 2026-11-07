;;;; cell-test.lisp --- the generic gateway cell hook + the kit host.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Proves the core seam's one convention end to end with in-memory fake
;;;; systems registered as loaded cell records: the gateway calls
;;;; <PACKAGE>::START-CELL (config) after recovery — the whole parsed
;;;; config and nothing else, a cell reaches the organism in-process; the
;;;; returned stop thunk runs first in STOP-GATEWAY's unwind. A failing
;;;; cell is one warning, never a boot failure. The kit's own START-CELL
;;;; is proved as the channel host over a fake channel adapter system.

(in-package #:nodecode.test)

;;; --- in-memory fake cell / channel systems -------------------------------
;;; Componentless ASDF systems registered at load time so ASDF:LOAD-SYSTEM
;;; resolves them; behavior lives in the packages defined here.

(defvar *fake-cell-calls* '())
(defvar *fake-cell-stops* 0)
(defvar *fake-channel-calls* '())
(defvar *fake-channel-stops* 0)

(defpackage #:nodecode-cell-fake
  (:use #:cl)
  (:export #:start-cell))

(defun nodecode-cell-fake:start-cell (config)
  (push (list config) nodecode.test::*fake-cell-calls*)
  (lambda () (incf nodecode.test::*fake-cell-stops*)))

(defpackage #:nodecode-cell-boom
  (:use #:cl)
  (:export #:start-cell))

(defun nodecode-cell-boom:start-cell (config)
  (declare (ignore config))
  (error "boom cell refuses to start"))

(defpackage #:nodecode-channel-fake
  (:use #:cl)
  (:export #:start-channel))

(defun nodecode-channel-fake:start-channel (section)
  (push (list section) nodecode.test::*fake-channel-calls*)
  (lambda () (incf nodecode.test::*fake-channel-stops*)))

;; Registered as componentless immutable systems: behavior lives in the
;; packages above, so ASDF must never plan a define-op against this test
;; file (an asdf:defsystem here records THIS file as the system source and
;; a later load-system walks straight into a define-op circularity).
;; REGISTERED-SYSTEM, never FIND-SYSTEM, asks: this file loads inside an ASDF
;; session (a test-op's), which keeps a FIND-SYSTEM miss for its whole length,
;; so the suite that session goes on to run would find no system here.
(dolist (name '("nodecode-channel-fake"))
  (unless (asdf:registered-system name)
    (asdf::register-system
     (make-instance 'asdf:system :name name :source-file nil))
    (asdf:register-immutable-system name)))

;;; --- the generic gateway lifecycle --------------------------------------------
;;; The folder loader (kernel cells.lisp) is proved in the core suite; what
;;; the kit locks here is the gateway's half — START-CELLS after recovery,
;;; STOP-CELLS first on the way down — over records built by hand.

(defmacro with-fake-cells ((&rest names) &body body)
  `(with-saved-globals (nlk::*cells*)
     (setf nlk::*cells*
           (list ,@(loop for name in names
                         collect `(nlk::make-cell
                                   :name ,name :state :loaded :kind :peripheral
                                   :start (fdefinition (uiop:find-symbol* '#:start-cell
                                                                          (string-upcase ,name)))))))
     ,@body))

(deftest channel-cell-hook-starts-configured-cells ()
  (setf *fake-cell-calls* '() *fake-cell-stops* 0)
  (handler-bind ((warning #'muffle-warning))
    (with-temp-file (nodecode.evolved::*shared-config-path*
                     :type "jsonc"
                     :contents "{\"marker\": 7}"
                     :setf t)
      (with-fake-cells ("nodecode-cell-boom" "nodecode-cell-fake")
        (with-temp-gateway (port)
          ;; The peripherals start on a thread of their own once the listener
          ;; is up (START-CELLS-BEHIND-THE-LISTENER), a shell's replay or a
          ;; few seconds later.
          (is (await (:timeout 10) (nlk:cell-started-p (nlk:find-cell "nodecode-cell-fake"))))
          (is (= 1 (length *fake-cell-calls*)))
          (is-present (call (first *fake-cell-calls*))
            "the START-CELL call carries the contract argument"
            (destructuring-bind (config) call
              (is-shape config (hash-table-p is) ("marker" eql 7))))
          (is (integerp port))
          (is (zerop *fake-cell-stops*))
          (is (nlk:cell-started-p (nlk:find-cell "nodecode-cell-fake")))
          ;; The boom cell signalled at start; its sibling started all the same.
          (is (null (nlk:cell-started-p (nlk:find-cell "nodecode-cell-boom")))))
        ;; WITH-TEMP-GATEWAY's unwind ran STOP-GATEWAY.
        (is (= 1 *fake-cell-stops*))
        (is (notany #'nlk:cell-started-p (nlk:cells)))))))

(deftest channel-cell-hook-absent-config-is-a-no-op (with-temp-gateway (port))
  ;; The suite's hermetic posture: *shared-config-path* is NIL and the
  ;; folder is empty, so the lifecycle does nothing — every existing gateway
  ;; test rides this.
  (is (integerp port))
  (is (notany #'nlk:cell-started-p (nlk:cells))))

;;; --- the kit's channel host -------------------------------------------------

(defmacro with-kit-cell ((config-json) &body body)
  "Run BODY with the kit's START-CELL started on the CONFIG-JSON literal over
zeroed fake-channel counters and a cleared status board — STOP bound to its
thunk, funcalled on unwind — and the registered commands restored after."
  `(with-saved-globals (nodecode.evolved::*registered-commands*)
     (setf *fake-channel-calls* '() *fake-channel-stops* 0)
     (nck:clear-all-channel-status)
     (with-cell-stop ((nck:start-cell (cell-json ,config-json)))
       ,@body)))

(deftest channel-cell-kit-hosts-enabled-channels ()
  (with-kit-cell ("{\"channels\": {\"fake\": {\"enabled\": true,
                                            \"x\": 1},
                                 \"disabled-one\": {\"enabled\": false}}}")
    (is (= 1 (length *fake-channel-calls*)))
    (is-present (call (first *fake-channel-calls*))
      "START-CHANNEL receives its own section and nothing else"
      (destructuring-bind (section) call
        (is (eql 1 (gethash "x" section)))))
    (is (nodecode.evolved::find-registered-command "channels")))
  (is (= 1 *fake-channel-stops*))
  (is (null (nodecode.evolved::find-registered-command "channels")) "stop takes /channels back"))

(deftest channel-cell-kit-present-section-is-enabled ()
  ;; hermes's rule: naming the section is the consent. An explicit false
  ;; still vetoes.
  (with-kit-cell ("{\"channels\": {\"fake\": {\"x\": 2},
                                 \"vetoed\": {\"enabled\": false}}}")
    (is (= 1 (length *fake-channel-calls*)))
    (is (null (nck:channel-status "vetoed")))))

(defun setup-primer ()
  "(help :channels) while an adapter wants setting up, or NIL; its line rides
the help section."
  (let ((topic (assoc :channels nle::*help-topics*)))
    (when topic
      ;; every request carries its line
      (is (search (format nil "  :channels - ~a" (second topic))
                  (cdr (assoc "help" (nle::read-harness-sections nil) :test #'string=))))
      (cddr topic))))

(defun fake-adapter-record (id)
  "A loaded library record for an adapter folder, as LOAD-CELLS builds it."
  (nlk::make-cell :name (format nil "nodecode-channel-~a" id) :state :loaded
                   :kind :library
                   :systems (list (format nil "nodecode-channel-~a" id))))

(deftest channel-cell-kit-present-folder-without-section-stands ()
  ;; The other half of presence-is-consent: an installed adapter with no
  ;; section is a standing notice naming the section, on every shell and
  ;; in (cells), and (help :channels) is on while it lasts.
  (with-saved-globals (nlk::*cells*)
    (setf nlk::*cells* (list (fake-adapter-record "fake")
                              (nlk::make-cell :name "nodecode-channel-kit"
                                               :state :loaded :kind :peripheral
                                               :systems '("nodecode-channel-kit"))
                              (nlk::make-cell :name "broken" :state :failed
                                               :systems '("nodecode-channel-broken"))))
    (is (equal '("fake") (nck:present-adapters)))
    (with-kit-cell ("{\"channels\": {}}")
      (is (eq :unconfigured (getf (nck:channel-status "fake") :state)))
      (is (equal '("fake") nck:*unconfigured*))
      (is-carrying (standing (second (cell-notice "channels.fake")))
        "fake: unconfigured — channels.fake is absent" "declares no section")
      (is-present (text (setup-primer)) "the setup primer is on"
        (is (search "(nck:probe" text) "it teaches the probe")
        (is (search "(config-set" text) "the write")
        (is (search "(restart-cells)" text) "the restart")
        (is (search "channels.fake: the adapter declares no section" text))))
    (is (null (cell-notice "channels.fake")))
    (is (null nck:*unconfigured*))
    (is (null (setup-primer)))))

(deftest channel-cell-kit-refusal-stands ()
  ;; A missing adapter system is one refusal, never a blocked host.
  (with-kit-cell ("{\"channels\": {\"ghost-platform\": {}}}")
    (is (functionp stop) "a missing adapter system never blocks the host")
    (is (eq :refused (getf (nck:channel-status "ghost-platform") :state)))
    (is (search "ghost-platform" (getf (nck:channel-status "ghost-platform") :detail)))
    (is (search "ghost-platform: refused —" (second (cell-notice "channels.ghost-platform"))))
    (is (equal '("ghost-platform") nck:*unconfigured*))))

(deftest channel-status-transition-is-announced ()
  ;; A state change is the adapter speaking: the row goes out as the
  ;; standing notice under the section's key; other field updates do not.
  (nck:clear-all-channel-status)
  (flet ((standing () (cell-notice "channels.fake")))
    (nck:set-channel-status "fake" :state :starting)
    (is (null (standing)) ":starting is not announced")
    (nck:set-channel-status "fake" :state :running :connected t)
    (is (equal "fake: running, connected" (second (standing))))
    (is (eq :info (third (standing))))
    (nck:set-channel-status "fake" :delivered-count 3)
    (is (equal "fake: running, connected" (second (standing))))
    (nck:set-channel-status "fake" :state :stopped :connected nil
                                   :detail "gateway close 4004")
    (is (equal "fake: stopped, 3 delivered — gateway close 4004" (second (standing))))
    (is (eq :error (third (standing))) "a stop is an error-level notice")
    (nck:clear-channel-status "fake")
    (is (null (standing)) "clearing the status clears the board")))

(deftest channel-probe-without-a-section-says-so ()
  (is (search "channels.fake is absent" (nck:probe "fake")))
  (is (search "(config-set '(\"channels\" \"fake\")" (nck:probe "fake"))))

(deftest channel-cell-status-surface (with-saved-globals (nodecode.evolved::*registered-commands*))
  (nck:clear-all-channel-status)
  (is (equal "no channel adapters running" (nck:channels-status-report)))
  (nck:set-channel-status "fake" :state :running :connected t
                                 :sessions 2 :delivered-count 5)
  (nck:set-channel-status "fake" :detail "all well")
  (is-carrying (report (nck:channels-status-report))
    "fake: running" "connected" "5 delivered" "all well")
  (nck:register-channel-commands)
  (is (search "fake: running" (cell-entry "nodecode-channel-kit" "channels" "")))
  (is (search "fake: running" (nle:slash "/channels")))
  (is (every #'nodecode.evolved::find-registered-command '("channels" "stop" "sethome")) "all three")
  (nck:clear-all-channel-status))

(deftest channel-cell-kit-serves-its-status-to-the-operators-page (with-temp-gateway (port))
  (with-kit-cell ("{\"channels\": {\"fake\": {\"enabled\": true}}}")
    (nck:set-channel-status "fake" :state :running :connected t :delivered-count 4
                                   :last-event-at-ms 1790000000000)
    (is-route (port :get "/api/channels" :token nil) 401 "the operator's route")
    (with-gateway-http (port :get "/api/channels")
      (is-present (row (find "fake" (nlk:json-value body :array "channels")
                             :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))
        "the lane"
        (is (equal "running" (nlk:json-value row :string "state")))
        (is (eq t (nlk:json-value row :any "connected")))
        (is (= 4 (nlk:json-value row :integer "delivered")))
        (is (= 1790000000000 (nlk:json-value row :integer "last_event_at_ms"))))
      ;; The adapters shipped beside the organism and not installed, whatever
      ;; this tree ships.
      (is (equal (nck::absent-adapters) (coerce (nlk:json-value body :array "absent") 'list))))
    (with-gateway-http (port :post "/api/channels?op=probe&id=absent")
      (is (search "channels.absent is absent" (nlk:json-value body :string "text")) "the probe's own words")))
  (is-route (port :get "/api/channels") 404 "stopped: the route is gone"))

(deftest channel-cell-kit-stands-down-in-an-ephemeral-organism (let ((nlk:*ephemeral* t)))
  (with-kit-cell ("{\"channels\": {\"fake\": {\"enabled\": true},
                                   \"off\": {\"enabled\": false}}}")
    (is (null *fake-channel-calls*) "no lane started: a throwaway organism holds no channel")
    (is-present (status (nck:channel-status "fake")) "the lane's status says why"
      (is (eq :refused (getf status :state)))
      (is (search "ephemeral" (getf status :detail))))
    (is (null (nck:channel-status "off")) "a vetoed lane stays quiet"))
  (is (= 0 *fake-channel-stops*) "nothing to unwind"))

(deftest channel-cell-kit-reads-a-declared-section-for-the-refusal-and-the-primer ()
  ;; An adapter that declares its section (NLK:DEFINE-SECTION) is reported,
  ;; taught and refused in the declaration's own words — the same words the
  ;; setup wizard's panel shows.
  (with-saved-globals (nlk::*cells* nlk::*sections*)
    (nlk:declare-section
     (nlk:make-section :path '("channels" "decl") :owner "nodecode-channel-decl"
                       :one-of '(("token_env" "token_file"))
                       :fields (list (nlk:make-section-field "token_env" :env :doc "the variable")
                                     (nlk:make-section-field "token_file" :path))
                       :guide "walk"))
    (setf nlk::*cells* (list (fake-adapter-record "decl")))
    (with-kit-cell ("{\"channels\": {}}")
      (is (eq :unconfigured (getf (nck:channel-status "decl") :state)))
      (is-carrying (standing (second (cell-notice "channels.decl")))
        "decl: unconfigured — channels.decl: still needed: token")
      (is-carrying (text (setup-primer))
        ("channels.decl:" "the primer carries the section")
        "token_env (name of an environment variable): the variable"
        ("exactly one of token_env / token_file" "its constraints")
        ("walk" "and its guide")))
    (with-kit-cell ("{\"channels\": {\"decl\": {\"token_env\": \"A\", \"token_file\": \"b\"}}}")
      (is (eq :refused (getf (nck:channel-status "decl") :state)))
      (is (search "exactly one of token_env / token_file may be set, 2 are"
                  (getf (nck:channel-status "decl") :detail)))
      (is (equal '("decl") nck:*unconfigured*) "and is a setup subject"))))

(deftest channel-unconfigured-stands-unsaid-and-a-refusal-is-said ()
  ;; Without this, an adapter installed with no section would draw a card on
  ;; every launch. It stands :quiet -- (cells), /cells and the model still read
  ;; it -- and is said to no shell; a lane refusing to start is said.
  (nck:clear-all-channel-status)
  (flet ((said-p (text) (find text (nlk:notice-log) :key #'first :test #'equal)))
    (nck:set-channel-status "fake" :state :unconfigured :detail "no section, quiet probe")
    (is (equal '("channels.fake" "fake: unconfigured — no section, quiet probe" :quiet)
               (cell-notice "channels.fake")))
    (is (search "fake: unconfigured" (nlk:cells-report-text)) "(cells) lists it")
    (is (not (said-p "fake: unconfigured — no section, quiet probe")) "and no one was told")
    (nck:set-channel-status "fake" :state :refused :detail "token rejected, loud probe")
    (is (eq :error (third (cell-notice "channels.fake"))))
    (is (said-p "fake: refused — token rejected, loud probe") "a refusal is said")
    (nck:clear-channel-status "fake")))

(deftest channel-kit-defines-each-special-before-a-form-reads-it ()
  ;; A cold cache compiles the kit on whichever thread first needs it, and a
  ;; special read ahead of its DEFVAR -- *HOSTS*, *OTHER-ADDRESSEES*,
  ;; *UNCONFIGURED* -- was an `undefined variable' warning at the end of the
  ;; compile, which a connection's thread put up as a raw toast over the
  ;; setup walk. A loaded kit hides it (the names are special by then), so
  ;; the files are read as data, in the system's own order.
  (labels ((names-p (name tree)
             (or (eq name tree)
                 (and (consp tree) (or (names-p name (car tree)) (names-p name (cdr tree)))))))
    (let* ((files (remove-if-not (lambda (component) (typep component 'asdf:cl-source-file))
                                 (asdf:component-children (asdf:find-system "nodecode-channel-kit"))))
           (forms (let ((*package* (find-package "NODECODE-CHANNEL-KIT")))
                    (loop for file in files
                          append (with-open-file (in (asdf:component-pathname file))
                                   (loop for form = (read in nil in)
                                         until (eq form in)
                                         collect form)))))
           (early (loop for form in forms
                        for at from 0
                        when (and (consp form)
                                  (member (first form) '(defvar defparameter nlk:define-startup-parameter))
                                  (position-if (lambda (earlier) (names-p (second form) earlier))
                                               forms :end at))
                          collect (second form))))
      (is (null early)))))
