;;;; cell-test.lisp --- START-CELL, its config, and the /chrome entrypoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader contract end to end: START-CELL (config) brings the bridge up
;;;; on the (test-rebound) port and registers /chrome (nle:register-command);
;;;; the stop thunk takes the bridge down. The command is driven exactly as
;;;; SLASH drives it for every caller, and its answer is the text it shows.

(in-package #:nodecode.test)

(defmacro with-chrome-port ((port-var) &body body)
  "BODY with the bridge's port an ephemeral one, PORT-VAR; the bridge, its
settings and the registry restored on unwind."
  `(let ((,port-var (temp-gateway-port)))
     (declare (ignorable ,port-var))
     ;; Onboard's openers are harmless stand-ins: a test never starts a browser.
     (with-saved-globals ((chrome::*bridge-port* ,port-var) chrome::*bridge*
                          chrome::*screenshot-dir* chrome::*background* nle::*registered-commands*
                          (chrome::*chrome-openers* '(("true"))) (chrome::*folder-revealer* nil))
       ,@body)))

(defun slash-chrome (args session-id)
  "Drive /chrome ARGS as every caller does: the answer's text."
  (cell-entry "nodecode-chrome" "chrome" args session-id))

(define-cell-lifecycle-tests "chrome"
  (:fixture with-chrome-port (port))
  (:config "enabled" t "background" nil)
  (:command "chrome")
  (:help :chrome)
  (:running (is chrome::*bridge* "the bridge is the one live bridge")
            (is-values (status body) (bridge-http port :get "/status")
              (status eql 200) ((nlk:json-value body :text "version") "0.15.51.4"))
            (is (equal "0.15.51.4" (chrome::vendored-extension-version)))
            ;; No authorization step: every session reads the manual.
            (is (equal chrome::+primer+ (nle:help :chrome)))
            (is (null (chrome::session-background "s1")) "the section's background default"))
  (:stopped (is (null chrome::*bridge*))
            (is (not (nle:await-port port :attempts 1)) "stop closes the port"))
  (:refused ("screenshot_dir" 5)))

(deftest chrome-cell-disabled-and-refused-configs (with-chrome-port (port))
  (with-cell-stop ((chrome-start "enabled" nil))
    (is (null chrome::*bridge*) "disabled installs nothing")
    (is (not (nle:await-port port :attempts 1))))
  (is (signals-error nlk:config-refusal (chrome-start "screenshot_dir" 5)))
  (is (null chrome::*bridge*))
  ;; A whole config without a chrome section still starts (defaults).
  (with-cell-stop ((chrome:start-cell (make-hash-table :test #'equal)))
    (is chrome::*bridge*)))

(deftest chrome-cell-screenshot-dir-expands-home ()
  (flet ((dir (text) (getf (nlk:section-settings (nlk:find-section '("chrome"))
                                                 (nlk:json-object "screenshot_dir" text))
                           :screenshot-dir)))
    (is (equal (merge-pathnames "shots/" (user-homedir-pathname)) (dir "~/shots")))
    (is (equal #p"/tmp/x/" (dir "/tmp/x")))))

(deftest chrome-cell-slash-dispatch-answers-its-text (with-chrome-session (sid))
  (with-chrome-port (port)
    (with-cell-stop ((chrome-start "background" t))
      (let ((text (slash-chrome "status" sid)))
        (is (search "never polled" text))
        (is (search "background on for this session" text) "the section's default"))
      (is (search "background off for this session" (slash-chrome "background off" sid)))
      (is (null (chrome::session-background sid)))
      (is (search "background is off" (slash-chrome "background" sid)))
      (is (search "background off for this session" (slash-chrome "status" sid)))
      (is (search "background on for this session" (slash-chrome "background on" sid)))
      (is (eq t (chrome::session-background sid)))
      ;; There is no authorization step to take.
      (is (search "Unknown /chrome subcommand authorize" (slash-chrome "authorize 2h" sid)))
      (is (search "Unknown /chrome subcommand bogus" (slash-chrome "bogus" sid)))
      (is (search "/chrome status" (slash-chrome "" sid)) "bare /chrome prints usage")
      (is (search "No session bound" (slash-chrome "background on" nil)))
      (let ((text (slash-chrome "onboard" sid)))
        (is (search "chrome://extensions" text))
        (is (search (namestring (chrome::extension-directory)) text))
        (is (probe-file (merge-pathnames "manifest.json" (chrome::extension-directory)))))
      (with-saved-globals ((chrome::*doctor-timeout-seconds* 0.3))
        (let ((text (slash-chrome "doctor" sid)))
          (is (search "Chrome doctor:" text))
          (is (search "not polling" text) "no extension: the timeout class is the diagnosis")))
      ;; Doctor against a fake extension: version mismatch is a warning. Its page
      ;; checks run in a tab of its own, on the bridge's /status page (a fresh tab
      ;; is about:blank, which no extension may script), closed after.
      (let ((sent '()))
        (with-fake-extension (port (lambda (command &aux (action (gethash "action" command))
                                                         (params (gethash "params" command)))
                                     (push (list action (gethash "sessionKey" params)
                                                 (gethash "url" params))
                                           sent)
                                     (values t (cond
                                                 ((equal action "tab.version")
                                                  (nlk:json-object "extensionVersion" "0.15.40"))
                                                 ((equal action "page.evaluate") 2)
                                                 (t (nlk:json-object "arithmetic" 2
                                                                     "location" "https://example.com/x"
                                                                     "webdriver" :false))))))
          (let ((text (slash-chrome "doctor" sid)))
            (is (search "extension 0.15.40 ok, evaluate ok, probe ok (example.com)" text))
            (is (search "differs from vendored 0.15.51.4" text))))
        (setf sent (reverse sent))
        (is (equal '("tab.version" "page.navigate" "page.evaluate" "page.probe" "automation.cleanup")
                   (mapcar #'first sent)))
        (is (every (lambda (entry) (equal "doctor" (second entry))) sent))
        (is (equal (format nil "http://127.0.0.1:~d/status" port) (third (second sent))))))))

(deftest chrome-cell-installs-the-extension-under-the-home ()
  ;; The folder the operator loads is a copy under the home, refreshed when the
  ;; shipped version changes: Chrome reloads an unpacked extension from the
  ;; folder it was loaded from, and the shipped one moves with every release.
  (let* ((folder (chrome::install-extension))
         (manifest (merge-pathnames "manifest.json" folder))
         (worker (merge-pathnames "service_worker.js" folder)))
    (is (equal folder (chrome::extension-directory)))
    (is (uiop:string-prefix-p (namestring (nlk:home)) (namestring folder)))
    (is (equal (chrome::vendored-extension-version) (chrome::manifest-field folder "version")))
    (is (probe-file worker))
    ;; A current copy is left alone.
    (with-open-file (out worker :direction :output :if-exists :append)
      (write-line "// local mark" out))
    (chrome::install-extension)
    (is (search "// local mark" (uiop:read-file-string worker)))
    ;; An older one is replaced, worker and all.
    (with-open-file (out manifest :direction :output :if-exists :supersede)
      (write-string "{\"version\": \"0.0.1\"}" out))
    (chrome::install-extension)
    (is (equal (chrome::vendored-extension-version) (chrome::manifest-field folder "version")))
    (is (not (search "// local mark" (uiop:read-file-string worker))))))
