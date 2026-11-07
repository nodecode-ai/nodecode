;;;; cell.lisp --- START-CELL and the /chrome slash command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell entry the loader finds by name: read the `chrome' section
;;;; once, refresh the loadable copy of the extension under the home, start
;;;; the bridge advertising the vendored extension's version, put the primer
;;;; on the manual, register /chrome, return the stop thunk. There is no
;;;; authorization step: a running cell is Chrome control for every
;;;; session. Config is materialized here and closed over; nothing rewrites it
;;;; later — the one operator toggle (/chrome background) is a session-state
;;;; row, not config.
;;;;
;;;; Config, a sibling top-level key next to `cells':
;;;;   "cells": ["nodecode-chrome"],
;;;;   "chrome": {"enabled": true,
;;;;              "background": true,
;;;;              "screenshot_dir": "~/.nodecode/chrome-screenshots"}
;;;;
;;;; Slash output: every subcommand answers one line; the long-form reports
;;;; are model-facing ((chrome:status) via eval).

(in-package #:nodecode-chrome)

(defparameter *doctor-timeout-seconds* 10
  "Per-probe wait in /chrome doctor (pi-chrome's 10 s). Tests shorten it.")

(defparameter +usage+
  "/chrome status | doctor | onboard | background [on|off]")

;;; --- the manual -----------------------------------------------------------
;;; (help :chrome) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text. Vocabulary here is the
;;; contract the surface (surface.lisp) keeps; change both together.

(defparameter +primer+
  "Chrome control through the nodecode-chrome cell: it drives the user's real, signed-in
Chrome profile via a companion extension. Everything is a Lisp function in the chrome:
package, called through eval. Every function returns a string.

  (chrome:snapshot &key mode query near-uid containing-text role-filter max-elements limit)
      mode: :auto (default) :interactive :forms :page-map :text :changes :full
      :query \"merge button\" ranks matches; :near-uid \"el-12\" sorts by proximity
  (chrome:inspect \"el-12\")                   one element in depth: context, nearby text/actions
  (chrome:click \"el-12\" &key snapshot)        also :selector \"css\" or :x N :y N instead of a uid
  (chrome:type \"text\" &key uid selector enter snapshot)
  (chrome:fill \"el-3\" \"text\" &key submit snapshot)
  (chrome:key \"Enter\" &key shift ctrl alt meta snapshot)
  (chrome:navigate \"https://...\" &key timeout-ms)
  (chrome:wait-for &key selector expression timeout-ms)
  (chrome:evaluate \"document.title\")          JSON-serializable value, as text
  (chrome:screenshot &key format full-page)   saves a file and returns its PATH; (look PATH) shows it
  (chrome:tabs)  (chrome:status)
  (chrome:send \"page.scroll\" :delta-y 800)    raw access to any extension action; keyword params
      map to camelCase (:include-snapshot -> includeSnapshot, :target -> targetId); returns the
      result as JSON text. Actions: tab.list/new/activate/close/group/ungroup, page.hover/drag/
      tap/upload/scroll, page.console.list, page.network.list, page.network.get, page.probe.
All actions accept :background (default t: Chrome keeps its focus; nil lets the user watch)
and :target / :url-includes / :title-includes to address an existing tab.

Rules:
1. Snapshot before you click or type. Prefer uids over selectors; uids come from the latest
   snapshot and die on navigation, so re-snapshot after navigate or a page-changing click.
2. Pass :snapshot t on click/type/fill/key to verify in one round trip.
3. Without a target, actions run in this session's own automation tab, never the user's
   active tab.
4. Calls block until Chrome answers (usually under 3 s). For navigate, wait-for, screenshot
   and slow pages pass yield_time_ms 40000 on the eval call so the form is not
   backgrounded; if you ever see status running, (eval-await ID) - never re-send the action.
5. CHROME-OFFLINE or a timeout naming the extension: ask the user to run /chrome doctor
   (or /chrome onboard when the extension was never loaded).
6. Output is capped near 6000 characters; zoom with :mode, :query, :near-uid or :limit.
7. Native file pickers, permission prompts and other browser chrome cannot be driven."
  "What (help :chrome) answers while the cell runs.")

(defparameter *chrome-openers*
  (cond ((uiop:os-macosx-p)
         '(("open" "-a" "Google Chrome" "chrome://extensions")
           ("open" "-a" "Chromium" "chrome://extensions")))
        ((uiop:os-windows-p) '())
        (t (loop for exe in '("google-chrome-stable" "google-chrome" "chromium" "chromium-browser")
                 collect (list exe "chrome://extensions"))))
  "The commands /chrome onboard tries, in order, to show chrome://extensions.")

(defparameter *folder-revealer* (and (uiop:os-macosx-p) '("open" "-R"))
  "The command /chrome onboard prefixes to the extension folder to show it in
the file manager, or NIL.")

(defun shipped-extension-directory ()
  (asdf:system-relative-pathname "nodecode-chrome" "browser-extension/"))

(defun extension-directory ()
  "The folder the operator loads unpacked: a copy under the home, because the
shipped folder moves with every release and Chrome reloads an unpacked
extension from the folder it was loaded from."
  (nlk:home "chrome-extension/"))

(defun manifest-field (directory key)
  (ignore-errors
   (nlk:json-value (nlk:decode-json (uiop:read-file-string
                                     (merge-pathnames "manifest.json" directory)))
                   :text key)))

(defun vendored-extension-version ()
  "The version in the shipped manifest.json — what the bridge advertises."
  ;; Read from the file, never hard-coded: the two can only drift together.
  (or (manifest-field (shipped-extension-directory) "version")
      (error "chrome: cannot read ~a" (shipped-extension-directory))))

(defun install-extension (&aux (source (shipped-extension-directory))
                               (target (extension-directory)))
  "Refresh the loadable copy when its version is not the shipped one; => its
folder. The extension, loaded from there, sees the newer version the bridge
advertises and reloads itself onto the new files."
  (unless (equal (manifest-field target "version") (vendored-extension-version))
    (ensure-directories-exist target)
    ;; manifest.json last: a half-copied folder still reads as the old version.
    (dolist (file (sort (uiop:directory-files source) #'string<
                        :key (lambda (file) (if (equal (file-namestring file) "manifest.json")
                                                "~" (file-namestring file)))))
      (uiop:copy-file file (merge-pathnames (file-namestring file) target))))
  target)

(defun open-extensions-page ()
  "Show chrome://extensions with the first *CHROME-OPENERS* command that
works: one still running after a second is a browser it started, reaped
when that exits; one that exited 0 handed the page to a running browser."
  (dolist (argv *chrome-openers*)
    (nlk:bind (((pid _ _ process)
                (ignore-errors (nlk:spawn-program argv :output nil :error-output nil))))
      (when pid
        (let ((code (loop repeat 20
                          for code = (nlk:child-exit pid process)
                          when code return code
                          do (sleep 0.05))))
          (cond ((null code)
                 (nlk:spawn "chrome-onboard-reap" (nlk:wait-for-child pid process))
                 (return t))
                ((zerop code) (return t))))))))

;;; --- subcommands: each answers its text -----------------------------------

(defun connection-line ()
  (cond ((null *bridge*) "bridge not running")
        ((bridge-connected-p *bridge*) "extension connected")
        ((foreign-polling-p *bridge*)
         (format nil "'~a' is polling instead; remove it at chrome://extensions and run /chrome onboard"
                 (bridge-foreign-name *bridge*)))
        ((poll-age-seconds *bridge*) "extension not polling")
        (t "extension never polled; run /chrome onboard")))

(defun do-status (session-id)
  (format nil "~a; background ~:[off~;on~] for this session"
          (connection-line) (session-background session-id)))

(defun do-background (session-id args &aux (word (string-downcase (string-trim " " args))))
  (cond
    ((member word '("on" "off") :test #'string=)
     (set-session-background session-id (string= word "on"))
     (format nil "Chrome background ~a for this session" word))
    (t (format nil "Chrome background is ~:[off~;on~]; /chrome background on|off"
               (session-background session-id)))))

(defun do-doctor ()
  "pi-chrome's doctor, one line: version probe, evaluate, page probe."
  ;; It calls the bridge directly, outside any session.
  ;; The page checks run in a tab of the doctor's own, on the bridge's /status
  ;; page (a fresh tab is about:blank, which no extension may script), closed
  ;; after.
  (nlk:with-handlers ((chrome-error (condition)
                        (format nil "Chrome doctor: ~a" condition)))
    (let ((bridge (or *bridge* (error 'chrome-offline))))
      (flet ((ask (action &rest params)
               (let ((object (nlk:json-object "sessionKey" "doctor" "foreground" :false)))
                 (loop for (key value) on params by #'cddr
                       do (setf (gethash key object) value))
                 (bridge-send bridge action object :timeout *doctor-timeout-seconds*))))
        (nlk:with-cleanup ((ignore-errors (ask "automation.cleanup")))
          (doctor-report bridge (ask "tab.version")
                         (progn (ask "page.navigate" "url" (format nil "~a/status" (bridge-url bridge)))
                                (ask "page.evaluate" "expression" "1+1" "awaitPromise" t))
                         (ask "page.probe")))))))

(defun doctor-report (bridge version value probe)
  "The doctor's line from its three answers."
  (let ((extension-version (jv version :text "extensionVersion"))
        (warnings '()))
    (unless (equal extension-version bridge.version)
      (push (format nil "extension ~a differs from vendored ~a: reload it at chrome://extensions"
                    extension-version bridge.version)
            warnings))
    (unless (eql value 2)
      (push "evaluate did not answer 2" warnings))
    (unless (eql (jv probe :number "arithmetic") 2)
      (push "page probe failed" warnings))
    (when (jv probe :boolean "webdriver")
      (push "Chrome reports navigator.webdriver to sites" warnings))
    (when (foreign-polling-p bridge)
      (push (format nil "'~a' also polls this port: remove it at chrome://extensions"
                    bridge.foreign-name)
            warnings))
    (format nil "Chrome doctor: extension ~a ok, evaluate ok, probe ok (~a)~@[; ~{~a~^; ~}~]"
            extension-version
            (or (ignore-errors (let ((location (jv probe :text "location")))
                                 (or (quri:uri-authority (quri:uri location)) location)))
                "?")
            warnings)))

(defun run-subcommand (session-id args)
  (let* ((tokens (nlk:split-words args))
         (head (string-downcase (or (first tokens) "")))
         (rest (format nil "~{~a~^ ~}" (rest tokens))))
    (macrolet ((with-session (&body body)
                 `(if (and (stringp session-id) (nlk:store-open-p))
                      (progn ,@body)
                      "No session bound to this command")))
      (nlk:dispatch head string=
        ("" (format nil "~a -- ~a" (connection-line) +usage+))
        ("status" (do-status session-id))
        ("background" (with-session (do-background session-id rest)))
        ("doctor" (do-doctor))
         ;; Open chrome://extensions where a Chrome binary is on PATH.
        ("onboard" (let ((folder (namestring (install-extension))))
                     (open-extensions-page)
                     (when *folder-revealer*
                       (nlk:bind (((pid _ _ process)
                                   (ignore-errors (nlk:spawn-program (append *folder-revealer* (list folder))
                                                                     :output nil :error-output nil))))
                         (when pid (nlk:spawn "chrome-onboard-reap" (nlk:wait-for-child pid process)))))
                     (format nil "Load unpacked at chrome://extensions, Developer mode on ~
                                  and left on: ~a (remove any 'Pi Chrome Connector' first)"
                             folder)))
        (t (format nil "Unknown /chrome subcommand ~a. ~a" head +usage+))))))

;;; --- the entry ------------------------------------------------------------

(defun close-bridge ()
  "Stop the one live bridge, if there is one."
  (nlk:when-let (bridge (shiftf *bridge* nil))
    (stop-bridge bridge)))

(nle:define-cell chrome
  (:section ("chrome")
    (:guide "background decides whether actions keep Chrome's focus")
    ("background" :boolean :default t
                  :doc "whether actions keep Chrome's focus by default; /chrome background sets a session's own")
    ("screenshot_dir" :directory :default "chrome-screenshots/"
                      :doc "where a screenshot lands; a leading ~/ expands"))
  ;; A refused section signals NLK:CONFIG-REFUSAL and a bound port a plain
  ;; error — either is the loader's one loud warning.
  (:start (lambda ()
            (setf *screenshot-dir* (setting :screenshot-dir)
                  *background* (setting :background))
            (close-bridge)
            (install-extension)
            (setf *bridge* (start-bridge :version (vendored-extension-version)
                                         :name (manifest-field (shipped-extension-directory) "name")))
            (nle:on-stop #'close-bridge)))
  (:help :chrome "chrome: the operator's real, signed-in Chrome - snapshot, click, type, navigate" +primer+)
  (:command "chrome" (lambda (args session-id) (run-subcommand session-id args))
            :label "Chrome"
            :catches 'chrome-error
            :description "Chrome control: status, doctor, onboard, background"
            :argument-hint "status | doctor | onboard | background [on|off]"))
