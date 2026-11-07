;;;; surface-test.lisp --- the model-facing vocabulary over a fake extension.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest chrome-cell-surface-wire-params-map-keywords ()
  (let ((object (chrome::wire-params
                 "page.click"
                 '(:uid "el-1" :include-snapshot t :target 7 :url-includes "x"
                   :delta-y 800 :mode :page-map :paths ("/a" "/b") :absent nil
                   :explicit-false :false :session-id "s" :timeout 3 :background nil :limit 9)
                 :session-id "s-1" :foreground t)))
    (is-shape object ("uid" "el-1") ("includeSnapshot" eq t)
      ("targetId" eql 7 ":target aliases to targetId") ("urlIncludes" "x") ("deltaY" eql 800)
      ("mode" "pageMap" "keyword values camelCase too")
      ("paths" equalp #("/a" "/b") "lists become arrays"))
    (is (null (nth-value 1 (gethash "absent" object))) "NIL keys are dropped")
    (is (eq :false (gethash "explicitFalse" object)))
    (dolist (reserved '("sessionId" "timeout" "background" "limit"))
      (is (null (nth-value 1 (gethash reserved object))) (format nil "~a never rides" reserved)))
    (is-shape object ("foreground" eq t) ("sessionKey" "session:s-1")
      ("sessionGroupTitle" "Nodecode: s-1" "page.* joins the session group")
      ("joinSessionGroup" eq t)))
  (let ((object (chrome::wire-params "tab.new" '(:url "u") :session-id "s-1" :foreground nil)))
    (is (eq :false (gethash "foreground" object)))
    (is (equal "Nodecode: s-1" (gethash "groupTitle" object)) "tab.new names the group")
    (is (null (nth-value 1 (gethash "sessionGroupTitle" object)))))
  (let ((object (chrome::wire-params "tab.list" '() :session-id nil :foreground nil)))
    (is (null (nth-value 1 (gethash "sessionKey" object))) "no session, no keys")))

(defmacro with-extension-session ((sid port bridge handler) &body body)
  "Temp store + session + bridge + a fake extension running HANDLER; SID is
bound so tests can pass :session-id explicitly (no eval snippet binds the
ambient one here)."
  `(with-chrome-session (,sid)
     (with-chrome-bridge (,port ,bridge)
       (with-fake-extension (,port ,handler)
         ,@body))))

(defun recording-handler (sink &optional (reply (lambda (command) (declare (ignore command)) (nlk:json-object "input" "chrome"))))
  "A handler that pushes every (action . params) onto SINK's car."
  (lambda (command)
    (push (cons (gethash "action" command) (gethash "params" command)) (car sink))
    (values t (funcall reply command))))

(deftest chrome-cell-surface-send-returns-clipped-json-text (let ((seen (list '()))))
  (with-extension-session (sid port bridge
                            (recording-handler
                             seen (lambda (command)
                                    (declare (ignore command))
                                    (nlk:json-object "big" (make-string 9000 :initial-element #\x)))))
    (let ((text (chrome:send "page.probe" :session-id sid :foo-bar 1)))
      (is (stringp text))
      (is (uiop:string-prefix-p "{" text) "send returns JSON text")
      (is (<= (length text) 7200) "clipped under the eval cap")
      (is (search "[truncated" text)))
    (let ((last (first (car seen))))
      (is (equal "page.probe" (car last)))
      (is (eql 1 (gethash "fooBar" (cdr last))))
      ;; JSON false decodes to NIL on the receiving side; PRESENT tells it
      ;; from an absent key.
      (multiple-value-bind (value present) (gethash "foreground" (cdr last))
        (is (and present (null value)) "default background=t rides as foreground false")))
    (let ((short (chrome:send "page.probe" :session-id sid :limit 100)))
      (is (<= (length short) 250)))))

(deftest chrome-cell-surface-background-flag-inverts-to-foreground (let ((seen (list '()))))
  (with-extension-session (sid port bridge (recording-handler seen))
    (chrome:send "page.probe" :session-id sid :background nil)
    (is (eq t (gethash "foreground" (cdr (first (car seen))))) "explicit :background nil")
    (chrome::set-session-background sid nil)
    (chrome:send "page.probe" :session-id sid)
    (is (eq t (gethash "foreground" (cdr (first (car seen))))) "/chrome background off")
    (chrome:send "page.probe" :session-id sid :background t)
    (is (null (gethash "foreground" (cdr (first (car seen))))) "explicit :background t")))

(deftest chrome-cell-surface-wrappers-translate-and-format (let ((seen (list '()))))
  (with-extension-session
      (sid port bridge
       (recording-handler
        seen
        (lambda (command &aux (action (gethash "action" command)))
          (nlk:dispatch action equal
            ("page.snapshot" (nlk:json-object "title" "Page" "mode" (nlk:json-value command :text "params" "mode")))
            ("page.click" (if (nlk:json-value command :boolean "params" "includeSnapshot")
                              (nlk:json-object "result" (nlk:json-object "input" "chrome" "pageMutated" nil)
                                               "snapshot" (nlk:json-object "title" "After" "mode" "auto"))
                              (nlk:json-object "input" "chrome")))
            ("page.navigate" (nlk:json-object "id" 12 "title" "Example Domain"))
            ("page.waitFor" (nlk:json-object "elapsedMs" 310))
            ("page.evaluate" "Example Domain")
            ("tab.list" (vector (nlk:json-object "id" 12 "active" t "title" "Example" "url" "https://e/")))
            ("page.inspect" (nlk:json-object "target" (nlk:json-object "uid" "el-2" "role" "button" "label" "Go")))
            (t (nlk:json-object "input" "chrome"))))))
    (flet ((last-params () (cdr (first (car seen))))
           (last-action () (car (first (car seen)))))
      (let ((text (chrome:snapshot :session-id sid :query "go" :mode :interactive)))
        (is (uiop:string-prefix-p "# Chrome snapshot (interactive)" text))
        (is (equal "interactive" (gethash "mode" (last-params))))
        (is (eql 40 (gethash "maxElements" (last-params))) "default 40 elements requested")
        (is (equal "go" (gethash "query" (last-params)))))
      (is (equal "Clicked el-2" (chrome:click "el-2" :session-id sid)))
      (is (null (nth-value 1 (gethash "includeSnapshot" (last-params)))))
      (let ((text (chrome:click "el-2" :snapshot t :session-id sid)))
        (is (uiop:string-prefix-p "Clicked el-2 - no coarse DOM change" text))
        (is (search "# Chrome snapshot (auto)" text))
        (is (eq t (gethash "includeSnapshot" (last-params)))))
      (is (equal "Clicked #go" (chrome:click nil :selector "#go" :session-id sid)))
      (is (equal "Clicked 10,20" (chrome:click nil :x 10 :y 20 :session-id sid)))
      (is (equal "Typed 5 chars into el-4" (chrome:type "hello" :uid "el-4" :enter t :session-id sid)))
      (is (equal "page.type" (last-action)))
      (is (eq t (gethash "pressEnter" (last-params))))
      (is (equal "hello" (gethash "text" (last-params))))
      (is (equal "Filled el-4" (chrome:fill "el-4" "v" :submit t :session-id sid)))
      (is (eq t (gethash "submit" (last-params))))
      (is (equal "Pressed Enter" (chrome:key "Enter" :ctrl t :session-id sid)))
      (let ((modifiers (gethash "modifiers" (last-params))))
        (is (eq t (gethash "ctrlKey" modifiers)))
        (multiple-value-bind (value present) (gethash "shiftKey" modifiers)
          (is (and present (null value)) "unset modifiers ride as explicit false")))
      (is (equal "Navigated to https://e/ (tab 12: Example Domain)"
                 (chrome:navigate "https://e/" :session-id sid)))
      (is (eql 15000 (gethash "timeoutMs" (last-params))))
      (is (equal "Ready after 310ms" (chrome:wait-for :selector "#done" :session-id sid)))
      (is (equal "selector" (gethash "kind" (last-params))))
      (is (equal "#done" (gethash "value" (last-params))))
      (chrome:wait-for :expression "x > 1" :session-id sid)
      (is (equal "expression" (gethash "kind" (last-params))))
      (is (equal "\"Example Domain\"" (chrome:evaluate "document.title" :session-id sid)))
      (is (eq t (gethash "awaitPromise" (last-params))))
      (is (search "- 12* Example  https://e/" (chrome:tabs :session-id sid)))
      (is (uiop:string-prefix-p "# Chrome inspect el-2" (chrome:inspect "el-2" :session-id sid)))
      (let ((status (chrome:status :session-id sid)))
        (is (search "extension: connected" status))
        (is (search "session s-chrome: background on" status))))))

(deftest chrome-cell-surface-screenshot-writes-a-file ()
  (let* ((png (coerce #(137 80 78 71 13 10 26 10 0 1 2 3) '(vector (unsigned-byte 8))))
         (data-url (concatenate 'string "data:image/png;base64,"
                                (cl-base64:usb8-array-to-base64-string png))))
    (with-temp-directory (dir "shots")
      (with-saved-globals ((chrome::*screenshot-dir* dir))
        (with-extension-session
            (sid port bridge
             (lambda (command)
               (values t (if (nlk:json-value command :boolean "params" "fullPage")
                             (nlk:json-object
                              "fullPage" t
                              "dimensions" (nlk:json-object "width" 1 "height" 2)
                              "tiles" (vector (nlk:json-object "y" 0 "dataUrl" data-url)
                                              (nlk:json-object "y" 900 "dataUrl" data-url)))
                             (nlk:json-object "dataUrl" data-url)))))
          (let* ((text (chrome:screenshot :session-id sid))
                 (path (subseq text (length "Saved Chrome screenshot to "))))
            (is (uiop:string-prefix-p "Saved Chrome screenshot to " text))
            (is-shape path (probe-file is) (pathname-type "png"))
            (let ((bytes (alexandria:read-file-into-byte-vector path)))
              (is (equalp png bytes) "decoded bytes round-trip")))
          (let ((text (chrome:screenshot :full-page t :format :jpeg :session-id sid)))
            (is (search "Saved 2 full-page tiles" text))
            (is (search "tiles are not stitched" text))
            (is (= 2 (length (directory (merge-pathnames "*-tile*.jpeg" dir)))))
            (is (= 1 (length (directory (merge-pathnames "*.json" dir)))))))))))
