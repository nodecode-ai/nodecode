;;;; control-cells-test.lisp --- the cells on and off from the web page,
;;;; and the hub's entries beside them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A scratch release -- two shipped folders, one standing on the other --
;;;; and an empty cell folder, both SETF where the gateway's threads read
;;;; them, with ASDF's registry and the report fenced to the test. The hub's
;;;; index is served by the test gateway itself: never the network.

(in-package #:nodecode.test)

(defun write-switch-folder (root name &rest options)
  "A shipped peripheral NAME under ROOT whose start and stop count in
<NAME>::*RUNS*; OPTIONS reach the folder writer."
  (apply #'write-peripheral-folder root name :exports '("*runs*") :forms '("(defvar *runs* 0)")
         :start "(declare (ignore config)) (incf *runs*) (lambda () (decf *runs*))" options))

(defmacro with-switch-fixture ((port base top) &body body)
  "BODY over a gateway on PORT and a scratch release shipping BASE and TOP
(standing on BASE), per-process names, into an empty cell folder."
  `(with-temp-gateway (,port)
     (with-temp-trees ((release "switch-release") (folder "switch-cells"))
       (let ((,base (format nil "nl-sw-base-~36r" (random (expt 36 6))))
             (,top (format nil "nl-sw-top-~36r" (random (expt 36 6)))))
         (ensure-directories-exist (merge-pathnames "src/" release))
         (write-switch-folder (merge-pathnames "cells/" release) ,base)
         (write-switch-folder (merge-pathnames "cells/" release) ,top
                              :depends-on (list ,base) :description "Does the top thing")
         (ensure-directories-exist folder)
         (with-saved-globals ((nlk::*source-root* (merge-pathnames "src/" release))
                              (nlk::*cells-directory* folder)
                              nlk::*cells* asdf:*central-registry* asdf::*immutable-systems*)
           (nlk:with-cleanup ((nle::stop-cells)) ,@body))))))

(defun serve-hub (path &rest entries &aux (index (apply #'hub-index-text entries)))
  "Serve the hub index of ENTRIES (HUB-INDEX-TEXT) at PATH on the test gateway."
  (nle:route path (constantly (list 200 '(:content-type "application/json") (list index)))
             :public t))

(defun switch-row (body name)
  "NAME's row of a cells answer BODY."
  (wire-row name (nlk:json-value body :array "cells") "name"))

(defun switch-body (name on)
  "The POST body turning NAME on, or off when ON is NIL."
  (format nil "{\"name\": ~s, \"on\": ~:[false~;true~]}" name on))

(defun switch-runs (name)
  "How many times the fixture peripheral NAME runs now."
  (symbol-value (uiop:find-symbol* '#:*runs* (string-upcase name))))

(deftest control-cells-on-brings-what-it-stands-on-and-off-takes-it-out
    (with-switch-fixture (port base top))
  (with-gateway-http (port :get "/api/gateway/cells" :status 200)
    (is-present (row (switch-row body top)) "the shipped folder is listed"
      (is-shape row ((:string "description") "Does the top thing" "with what it is for")
        ((:string "source") "shipped") ((:any "on") not "off: its folder is not there"))
      (is (equalp (vector base) (nlk:json-value row :array "brings")) "and what turning it on brings")))
  (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body top t) :status 200)
    (is (search (format nil "~a is on, with ~a" top base) (nlk:json-value body :string "text")))
    (is (nlk:json-value (switch-row body top) :any "loaded") "loaded into the organism now")
    (is (nlk:json-value (switch-row body base) :any "on") "with the folder it stands on"))
  (is (and (nlk:installed-cell-p top) (nlk:installed-cell-p base)) "both are in the one folder")
  (is (= 1 (switch-runs top)) "and it started")
  (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body base nil))
    (is (= 400 status) "a folder another stands on stays on")
    (is (search (format nil "~a needs it" top) (nlk:json-value body :string "error" "message")) "saying which"))
  (is (nlk:installed-cell-p base))
  (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body top nil) :status 200)
    (is (search "stays loaded in this organism until a restart" (nlk:json-value body :string "text")))
    (is-present (row (switch-row body top)) "its row"
      (is-shape row ((:any "on") not "off") ((:any "loaded") is "its code still in this image"))))
  (is (not (nlk:installed-cell-p top)) "its folder is out")
  (is (= 0 (switch-runs top)) "and its peripheral stopped")
  (nle::start-cells)
  (is (= 0 (switch-runs top)) "a start of every peripheral passes it over: presence is consent")
  (is-route (port :post "/api/gateway/cells" :body (switch-body top t)) 200 "on again")
  (is-shape top (nlk:installed-cell-p is) (switch-runs = 1 "started from the code already loaded"))
  (is-route (port :post "/api/gateway/cells" :body (format nil "{\"name\": ~s}" top)) 200
            "a name alone restarts the peripheral")
  (is (= 1 (switch-runs top)) "once"))

(deftest control-cells-an-uninstalled-cells-command-says-how-to-turn-it-on
    (with-switch-fixture (port base top))
  (let ((answer (nle:slash (format nil "/~a" top))))
    (is (search (format nil "the ~a cell ships with this release but is not installed" top) answer))
    (is (search (format nil "`nodecode add nc://~a'" top) answer) "with the way to turn it on"))
  (is (equal "unknown command /nl-sw-nope; /help lists them" (nle:slash "/nl-sw-nope")))
  (is-route (port :post "/api/gateway/cells" :body (switch-body top t)) 200 "on")
  (is (equal (format nil "unknown command /~(~a~); /help lists them" base) (nle:slash (format nil "/~a" base)))))

(deftest control-cells-on-tells-every-shell-to-load-what-it-turned-on
    (with-switch-fixture (port base top))
  ;; A shell in another process loads the folders the organism turned on, as
  ;; its boot would have, so its /cells lists them without a restart; it
  ;; starts none -- the organism runs the peripherals.
  (dolist (name (list base top)) (nlk:install-cell name))
  (feed-wire (detached-shell) (nlk:encode-unsequenced-frame "organism.cells" (nlk:json-object "names" (vector base top))))
  (is (and (nlk:find-cell base) (nlk:find-cell top)) "the shell loaded both")
  (is (= 0 (switch-runs top)) "and started neither")
  (let ((told '()))
    (nle:hook :frame "control-cells-test"
              (lambda (op next)
                (when (equal "organism.cells" (getf op :kind))
                  (push (coerce (gethash "names" (getf op :payload)) 'list) told))
                (funcall next op)))
    (nlk:with-cleanup ((nle:unhook :frame "control-cells-test"))
      (is-route (port :post "/api/gateway/cells" :body (switch-body top t)) 200))
    (is (equal (list (list base top)) told) "on names what it turned on, dependencies first")))

(deftest control-cells-row-says-how-a-cell-stands-whole
    (with-switch-fixture (port base top))
  ;; What /cells lays open beside its list is the listing's: whether the
  ;; cell runs, each failure and skipped file whole, the files slow to load,
  ;; the commands it registered, its folder and where that points; an own
  ;; folder says what its .asd says it is for.
  (is-route (port :post "/api/gateway/cells" :body (switch-body top t)) 200)
  (let ((record (nlk:find-cell top)))
    (setf (nlk::cell-skipped record) '(("probe.lisp" . "it reads a symbol this image no longer has"))
          (nlk::cell-timings record) '(("fast.lisp" . 12) ("slow.lisp" . 310))
          (nlk::cell-failures record) (list (cons "late.lisp" (format nil "line one~%line two")))))
  (write-temp-file (merge-pathnames "nl-sw-mine/nl-sw-mine.asd" nlk::*cells-directory*)
                   "(defsystem \"nl-sw-mine\" :description \"Mine, and it says so\")")
  (nle:register-command top "toppy" (constantly nil))
  (nlk:with-cleanup ((nle:unregister-commands top))
    (with-gateway-http (port :get "/api/gateway/cells" :status 200)
      (let ((row (switch-row body top)))
        (is (nlk:json-value row :boolean "started") "it runs")
        (is (equalp #("toppy") (nlk:json-value row :array "commands")) "the commands it registered")
        (is (equal (format nil "line one~%line two")
                   (nlk:json-value (aref (nlk:json-value row :array "failures") 0) :string "text")) "a failure, whole")
        (is (equal "late.lisp" (nlk:json-value (aref (nlk:json-value row :array "failures") 0) :string "file")))
        (is (equal "it reads a symbol this image no longer has"
                   (nlk:json-value (aref (nlk:json-value row :array "skipped") 0) :string "reason")))
        (is (equalp '(("slow.lisp" 310))
                    (map 'list (lambda (slow) (list (nlk:json-value slow :string "file") (nlk:json-value slow :integer "ms")))
                         (nlk:json-value row :array "slow"))) "only the file past the slow line")
        (is (equal (namestring (merge-pathnames (format nil "~a/" top) nlk::*cells-directory*))
                   (nlk:json-value row :string "folder")))
        (is (nlk:json-value row :text "from") "a shipped folder is a link to where it ships from"))
      (is (equal "Mine, and it says so" (nlk:json-value (switch-row body "nl-sw-mine") :string "description"))))))

(deftest control-cells-off-refuses-the-operators-own-folder-and-what-an-advice-keeps
    (with-switch-fixture (port base top))
  (let ((own (merge-pathnames "nl-sw-mine/nl-sw-mine.asd" nlk::*cells-directory*)))
    (write-temp-file own "(defsystem \"nl-sw-mine\")")
    (with-gateway-http (port :get "/api/gateway/cells")
      (is-present (row (switch-row body "nl-sw-mine")) "the operator's own folder is listed"
        (is-shape row ((:string "source") "own") ((:any "on") is))))
    (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body "nl-sw-mine" nil))
      (is (= 400 status))
      (is (search "your own folder" (nlk:json-value body :string "error" "message")) "never the page's to remove"))
    (is (probe-file own) "and it is left where it is"))
  ;; A cell that must stay for a reason of its own says so through advice.
  (nlk:with-cleanup ((nle:unhook 'nle::cell-off-refusal "control-cells-test"))
    (nle:hook 'nle::cell-off-refusal "control-cells-test"
              (lambda (next name) (if (string= name top) "it is the one in use" (funcall next name))))
    (is-route (port :post "/api/gateway/cells" :body (switch-body top t)) 200)
    (with-gateway-http (port :get "/api/gateway/cells")
      (let ((kept (nlk:json-value (switch-row body top) :string "kept")))
        (is (equal "it is the one in use" kept) "the listing says why before anyone asks")))
    (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body top nil) :status 400)
      (is (search "it is the one in use" (nlk:json-value body :string "error" "message"))))
    (is (nlk:installed-cell-p top)))
  (is-route (port :post "/api/gateway/cells" :body (switch-body "nl-sw-nowhere" t)) 400
            "a name neither shipped nor on the hub"))

(deftest control-cells-hub-lists-a-local-index-and-says-when-it-cannot-be-read
    (with-switch-fixture (port base top))
  (let ((commit (make-string 40 :initial-element #\b))
        (git "https://127.0.0.1:9/o/nl-sw-hubbed.git"))
    (serve-hub "/test-hub/index.json"
               (list :name "nl-sw-hubbed" :git git :commit commit :path "cells/nl-sw-hubbed"))
    (nlk:with-cleanup ((nle:route "/test-hub/index.json" nil))
      (with-saved-globals ((nlk:*hub-index-url* (format nil "http://127.0.0.1:~d/test-hub/index.json" port)))
        (with-gateway-http (port :get "/api/gateway/cells/hub" :status 200)
          (is-present (row (switch-row body "nl-sw-hubbed")) "the entry"
            (is-shape row ((:string "source") "hub") ((:string "git") git "its repository")
              ((:string "commit") commit "the commit it is pinned at")
              ((:string "path") "cells/nl-sw-hubbed"))
            (is (not (nlk:json-value row :any "on")))))
        ;; On looks a name the release does not ship up on the hub and clones
        ;; from there; a clone that cannot be made leaves nothing behind.
        (with-gateway-http (port :post "/api/gateway/cells" :body (switch-body "nl-sw-hubbed" t))
          (is (= 400 status))
          (is (search "git clone failed" (nlk:json-value body :string "error" "message"))))
        (is (not (nlk:installed-cell-p "nl-sw-hubbed")))
        (setf nlk:*hub-index-url* (format nil "http://127.0.0.1:~d/test-hub/none.json" port))
        (with-gateway-http (port :get "/api/gateway/cells/hub")
          (is (= 200 status) "an index that cannot be read is still an answer")
          (is (search "answered 404" (nlk:json-value body :string "error")) "saying why"))))))

(deftest control-cells-hub-offers-the-index-as-last-read-when-it-cannot-be-read
    (with-switch-fixture (port base top))
  ;; An index read is kept under the home: a later read that fails -- preview
  ;; down, a tester's address gone out of its allowed range -- offers that
  ;; copy and says why, where the page shows it.
  (let ((up t)
        (index (hub-index-text (list :name "nl-sw-kept" :git "https://127.0.0.1:9/o/nl-sw-kept.git"
                                     :commit (make-string 40 :initial-element #\d)))))
    (nle:route "/test-hub/flaky.json"
               (nlk:lambda-ignoring (env)
                 (if up
                     (list 200 '(:content-type "application/json") (list index))
                     (list 403 '(:content-type "text/plain") (list "not from there"))))
               :public t)
    (nlk:with-cleanup ((nle:route "/test-hub/flaky.json" nil))
      (with-saved-globals ((nlk:*hub-index-url* (format nil "http://127.0.0.1:~d/test-hub/flaky.json" port)))
        (is (equal '("nl-sw-kept") (mapcar #'nlk:offer-name (nlk:hub-cells :shipped '()))))
        (setf up nil)
        (multiple-value-bind (offers stale) (nlk:hub-cells :shipped '())
          (is (equal '("nl-sw-kept") (mapcar #'nlk:offer-name offers)) "the copy last read")
          (is (search "answered 403" stale) "and why"))
        (with-gateway-http (port :get "/api/gateway/cells/hub")
          (is (switch-row body "nl-sw-kept"))
          (is (search "answered 403" (nlk:json-value body :string "stale"))))))))

(defun add-answer (port ask &rest words &aux (*standard-output* (make-string-output-stream)))
  "NODECODE ADD's answer to WORDS against the organism on PORT (none for NIL),
ASK its terminal: (TEXT CODE STREAM PREVIEW), PREVIEW what it printed first."
  (append (multiple-value-list (nle::add-text words :ask ask :port port :token "test-token"))
          (list (get-output-stream-string *standard-output*))))

(deftest control-cells-add-turns-a-shipped-folder-on-here-or-at-the-next-launch
    (with-switch-fixture (port base top))
  ;; A shipped folder asks nothing. With no organism its folder goes in the
  ;; one folder for the next launch; with one, the route turns it on there.
  (flet ((never (question) (error "asked ~a" question)))
    (is-values (text code) (values-list (add-answer nil #'never (format nil "nc://~a" base)))
      (code = 0) ((search "is installed; nodecode is not running, so it loads at the next launch" text) is))
    (is-shape base (nlk:installed-cell-p is) (nlk:find-cell null "nothing loaded in this image"))
    (is-values (text code) (values-list (add-answer port #'never top))
      (code = 0) ((search (format nil "~a is on" top) text) is "the organism's own sentence"))
    (is (= 1 (switch-runs top)) "started in the organism")
    (is (search "already installed" (first (add-answer port #'never top))))
    (is (= 1 (switch-runs top)) "and not started twice")
    (is-values (text code stream) (values-list (add-answer port #'never (subseq top 1)))
      (code = 1) (stream eq :error)
      ((search (format nil "Did you mean ~s?" top) text) is "a typo names the nearest"))
    (is (= 2 (second (add-answer port #'never "a" "b"))) "one cell at a time")))

(deftest control-cells-add-shows-hub-code-and-installs-it-on-a-yes-at-that-commit
    (with-switch-fixture (port base top))
  (let ((commit (make-string 40 :initial-element #\b))
        (git "https://127.0.0.1:9/o/nl-sw-hubbed.git"))
    (serve-hub "/test-hub/index.json"
               (list :name "nodecode-nl-sw-hubbed" :git git :commit commit :depends-on (list base)))
    (nlk:with-cleanup ((nle:route "/test-hub/index.json" nil))
      (with-saved-globals ((nlk:*hub-index-url* (format nil "http://127.0.0.1:~d/test-hub/index.json" port)))
        (destructuring-bind (text code stream preview) (add-answer port (constantly nil) "nc://nl-sw-hubbed")
          (is (= 1 code)) (is (eq :error stream))
          (is (search "no terminal to ask: nothing was installed; add --yes" text))
          (dolist (shown (list git commit (format nil "brings  ~a" base) "full access to your files and keys"))
            (is (search shown preview) shown)))
        (is (equal "nothing was installed" (first (add-answer port (constantly "n") "nodecode-nl-sw-hubbed"))))
        (is (search "Did you mean \"nl-sw-hubbed\"?" (first (add-answer port (constantly nil) "nl-sw-hubed"))))
        (is (not (or (nlk:installed-cell-p "nodecode-nl-sw-hubbed") (nlk:installed-cell-p base))))
        ;; A yes reaches the organism with the commit it showed, which clones:
        ;; this one cannot be made. A commit the hub no longer pins is refused.
        (is (search "git clone failed"
                    (refusal-text nle::gateway-http-error
                      (add-answer port (lambda (question) (error "asked ~a" question)) "nl-sw-hubbed" "--yes"))))
        (with-gateway-http (port :post "/api/gateway/cells"
                            :body (format nil "{\"name\": \"nodecode-nl-sw-hubbed\", \"on\": true, \"commit\": ~s}"
                                          (make-string 40 :initial-element #\c)) :status 400)
          (is (search "the hub moved nl-sw-hubbed" (nlk:json-value body :string "error" "message"))))
        (is (not (nlk:installed-cell-p "nodecode-nl-sw-hubbed")))))))

(deftest control-cells-hub-reads-both-indexes-and-each-answers-for-itself
    (with-switch-fixture (port base top))
  ;; Nodecode's own index and the community's are read side by side: a name
  ;; Nodecode's lists is never the community's, an index that cannot be read
  ;; leaves the other's entries on offer, and only both failing is a refusal.
  (let ((commit (make-string 40 :initial-element #\c))
        (git "https://127.0.0.1:9/o/nl-sw-listed.git"))
    (flet ((at (path) (format nil "http://127.0.0.1:~d~a" port path)))
      (serve-hub "/test-hub/own.json" (list :name "nl-sw-both" :git git :commit commit))
      (serve-hub "/test-hub/community.json"
                 (list :name "nl-sw-both" :git "https://127.0.0.1:9/o/impostor.git" :commit commit)
                 (list :name "nl-sw-theirs" :git git :commit commit))
      (nlk:with-cleanup ((nle:route "/test-hub/own.json" nil) (nle:route "/test-hub/community.json" nil))
        (let ((offers (nlk:hub-cells :url (at "/test-hub/own.json") :community (at "/test-hub/community.json") :shipped '())))
          (is (equal '("nl-sw-both" "nl-sw-theirs") (mapcar #'nlk:offer-name offers)))
          (is (equal '("hub" "community") (mapcar #'nlk:offer-index offers)))
          (is (equal git (nlk:offer-git (first offers))) "Nodecode's entry, not the community's of the same name"))
        ;; Nodecode's index unread, the community's still offered.
        (is (equal '("nl-sw-both" "nl-sw-theirs")
                   (mapcar #'nlk:offer-name (nlk:hub-cells :url (at "/test-hub/none.json") :community (at "/test-hub/community.json") :shipped '()))))
        (is (equal '("nl-sw-both")
                   (mapcar #'nlk:offer-name (nlk:hub-cells :url (at "/test-hub/own.json") :community (at "/test-hub/none.json") :shipped '()))))
        ;; Every index asked for unread: a refusal naming why.
        (is (search "answered 404"
                    (refusal-text error (nlk:hub-cells :url (at "/test-hub/none.json") :community (at "/test-hub/gone.json") :shipped '()))))
        (is (null (nlk:hub-cells :url nil :community nil :shipped '())))
        ;; `nodecode add' says where a community entry came from, and nothing new of Nodecode's own.
        (with-saved-globals ((nlk:*hub-index-url* (at "/test-hub/own.json"))
                             (nlk:*community-index-url* (at "/test-hub/community.json")))
          (let ((theirs (fourth (add-answer port (constantly "n") "nc://nl-sw-theirs")))
                (own (fourth (add-answer port (constantly "n") "nc://nl-sw-both"))))
            (is (search "index   community" theirs))
            (is (search git theirs))
            (is (not (search "index" own)) "Nodecode's own prints as it did")))))))
