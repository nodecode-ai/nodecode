;;;; surface.lisp --- the model-facing vocabulary: SEND and its wrappers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Mono-tool: the model reaches Chrome from EVAL as plain functions,
;;;; so this file is the whole "tool schema" — the harness primer in
;;;; cell.lisp is its documentation and the two must move together. One
;;;; chokepoint, CALL-ACTION, does what pi-chrome's authorizedBridgeSend
;;;; does, minus its authorization: the keyword-to-camelCase parameter
;;;; mapping, the background/foreground inversion, the per-session isolation
;;;; keys, and the bridge call. SEND is that chokepoint returning JSON text; the named
;;;; wrappers exist only where a formatter or an argument translation earns
;;;; the name. Every public function returns a STRING: the eval snippet prints
;;;; the value with ~S, and a hash table would print as #<HASH-TABLE>.
;;;;
;;;; Session identity comes from NLK:*SCRIBE-SESSION-ID*, which the eval snippet
;;;; binds around every form (on the eval thread too); :SESSION-ID overrides
;;;; it for REPL use and tests.

(in-package #:nodecode-chrome)

;;; Outside any workspace on purpose: the boundary capture would otherwise ADD
;;; -A multi-MB images into the shadow store every turn. Under the home,
;;; resolved at process start; START-CELL materializes the config value here
;;; once when the section names one.
(nlk:define-startup-parameter *screenshot-dir* (nlk:home "chrome-screenshots/")
  "Where SCREENSHOT writes files.")

(defparameter *send-text-limit* 7000
  "SEND's JSON text cap — under the 8000 EVAL clamp so the disclosure
survives.")

(defvar *screenshot-counter* 0)

;;; --- parameter mapping ----------------------------------------------------

(defun camel-case (keyword)
  "TARGET-ID -> \"targetId\"; INCLUDE-SNAPSHOT -> \"includeSnapshot\"."
  (let ((parts (uiop:split-string (string-downcase (symbol-name keyword))
                                  :separator "-")))
    (format nil "~a~{~a~}" (first parts)
            (mapcar #'string-capitalize (rest parts)))))

(defun wire-value (value)
  "Lisp value to wire value: keywords become camelCase strings (:page-map ->
\"pageMap\"), lists become arrays, :FALSE stays an explicit false, T and
numbers and strings ride as they are."
  (cond
    ((or (member value '(:false t)) (stringp value) (numberp value) (hash-table-p value)) value)
    ((keywordp value) (camel-case value))
    ((or (listp value) (vectorp value)) (map 'vector #'wire-value value))
    (t (princ-to-string value))))

(defun remove-keys (plist &rest keys)
  (loop for (key value) on plist by #'cddr
        unless (member key keys)
          append (list key value)))

(defun wire-params (action plist &key session-id foreground &aux (object (nlk:make-json-object)))
  "The JSON object the extension receives for ACTION."
  ;; NIL-valued keys are absent (shasht would write them as false); the
  ;; isolation keys are what pi-chrome's authorizedBridgeSend adds: sessionKey
  ;; on everything, the session's tab group on page.* and on
  ;; tab.new/tab.group.
  ;; CALL-ACTION's own keys never ride the wire; :target's wire name is not its camelCase.
  (loop for (key value) on (remove-keys plist :session-id :timeout :background :limit) by #'cddr
        unless (null value)
          do (setf (gethash (if (eq key :target) "targetId" (camel-case key)) object)
                   (wire-value value)))
  (setf (gethash "foreground" object) (if foreground t :false))
  (when session-id
    (setf (gethash "sessionKey" object) (format nil "session:~a" session-id))
    (when (uiop:string-prefix-p "page." action)
      (setf (gethash "sessionGroupTitle" object) (format nil "Nodecode: ~a" session-id)
            (gethash "joinSessionGroup" object) t))
    (when (member action '("tab.new" "tab.group") :test #'string=)
      (setf (gethash "groupTitle" object) (format nil "Nodecode: ~a" session-id))))
  object)

;;; --- a session's background default --------------------------------------
;;; /chrome background on|off writes one session-state row; without it, the
;;; `chrome' section's background, which START sets once.

(defvar *background* t
  "Whether an action that does not say keeps Chrome's focus.")

(defparameter +background-key+ "chrome:background")

(defun session-background (session-id)
  "SESSION-ID's background default: its row, else *BACKGROUND*."
  (let ((row (and (stringp session-id) (nlk:store-open-p)
                  (nlk:session-state-get session-id +background-key+))))
    (if row (equal row "true") *background*)))

(defun set-session-background (session-id background)
  (nlk:session-state-put session-id +background-key+ (if background "true" "false")))

;;; --- the chokepoint -------------------------------------------------------

(defun call-action (action plist)
  "Map, send; return the decoded result."
  ;; PLIST may carry :SESSION-ID, :TIMEOUT (seconds) and :BACKGROUND; an
  ;; unstated :BACKGROUND takes the session's default.
  (check-type action string)
  (let* ((session-id (or (getf plist :session-id) nlk:*scribe-session-id*))
         (background (if (member :background plist)
                         (getf plist :background)
                         (session-background session-id))))
    (bridge-send *bridge* action
                 (wire-params action plist
                              :session-id session-id
                              :foreground (not background))
                 :timeout (or (getf plist :timeout) *send-timeout-seconds*))))

(defun json-text (value &optional (limit *send-text-limit*))
  (clip (nlk:pretty-json value) limit))

(defun send (action &rest params)
  "Run any extension ACTION with keyword PARAMS and return its result as JSON
text."
  ;; (chrome:send "page.scroll" :delta-y 800). Keyword params map to
  ;; camelCase; :session-id, :timeout and :background are consumed here.
  (json-text (call-action action params) (or (getf params :limit) *send-text-limit*)))

;;; --- wrappers -------------------------------------------------------------

(defun snapshot (&rest params &key (mode :auto) (max-elements 40)
                                   (limit *default-limit*) &allow-other-keys)
  "Observe the page: structure, visible actions with uids, forms, text."
  ;; MODE is :auto (default) :interactive :forms :page-map :text :changes
  ;; :full; :query ranks matches, :near-uid sorts by proximity,
  ;; :containing-text and :role-filter narrow, :max-text-chars widens.
  (format-snapshot (call-action "page.snapshot"
                                (list* :mode mode :max-elements max-elements
                                       (remove-keys params :mode :max-elements)))
                   :limit limit))

(defun inspect (uid &rest params &key (limit *default-limit*) &allow-other-keys)
  "One element in depth: its context, nearby text and actions, form context."
  ;; UID may be NIL with :selector "css" instead.
  (format-inspect (call-action "page.inspect" (list* :uid uid params))
                  :limit limit))

(defun page-action (verb target action head params snapshot limit &rest keys)
  "FORMAT-ACTION's answer for ACTION sent with HEAD, :include-snapshot, then
PARAMS without :snapshot and KEYS."
  (format-action verb target
                 (call-action action (append head (list :include-snapshot (and snapshot t))
                                             (apply #'remove-keys params :snapshot keys)))
                 :limit limit))

(defun click (uid &rest params &key snapshot (limit *default-limit*) &allow-other-keys)
  "Click UID (or :selector \"css\", or :x N :y N with UID nil) through Chrome's
real input layer. :snapshot t returns a fresh snapshot after the click."
  (page-action "Clicked" (or uid (getf params :selector)
                             (format nil "~a,~a" (getf params :x) (getf params :y)))
               "page.click" (list :uid uid) params snapshot limit))

(defun type (text &rest params &key enter snapshot (limit *default-limit*) &allow-other-keys)
  "Type TEXT as keystrokes into :uid or :selector (or the focused element);
:enter t presses Enter afterwards."
  (page-action "Typed" (format nil "~d chars into ~a" (length text)
                               (or (getf params :uid) (getf params :selector) "focus"))
               "page.type" (list :text text :press-enter (and enter t))
               params snapshot limit :enter))

(defun fill (uid text &rest params &key submit snapshot (limit *default-limit*) &allow-other-keys)
  "Set UID's value to TEXT (framework-aware); :submit t submits its form."
  (page-action "Filled" (or uid (getf params :selector))
               "page.fill" (list :uid uid :text text :submit (and submit t))
               params snapshot limit :submit))

(defun key (key &rest params &key shift ctrl alt meta snapshot (limit *default-limit*)
                                  &allow-other-keys)
  "Press KEY (\"Enter\", \"Escape\", \"a\", \"ArrowDown\") with modifiers."
  (page-action "Pressed" key "page.key"
               (list :key key
                     :modifiers (nlk:json-object "shiftKey" (if shift t :false)
                                                 "ctrlKey" (if ctrl t :false)
                                                 "altKey" (if alt t :false)
                                                 "metaKey" (if meta t :false)))
               params snapshot limit :shift :ctrl :alt :meta))

(defun navigate (url &rest params &key (timeout-ms 15000) &allow-other-keys)
  "Load URL in the session's automation tab and wait for load."
  (let ((tab (call-action "page.navigate"
                          (list* :url url :timeout-ms timeout-ms
                                 :timeout (+ 2 (/ timeout-ms 1000))
                                 (remove-keys params :timeout-ms :timeout)))))
    (format nil "Navigated to ~a (tab ~a: ~a)" url (jv tab :any "id")
            (compact-line (or (jv tab :text "title") "(untitled)") 80))))

(defun wait-for (&rest params &key selector expression (timeout-ms 10000) &allow-other-keys)
  "Wait until :selector matches or :expression is truthy, up to :timeout-ms."
  (let ((result (call-action "page.waitFor"
                             (list* :kind (if selector :selector :expression)
                                    :value (or selector expression)
                                    :timeout-ms timeout-ms
                                    :timeout (+ 2 (/ timeout-ms 1000))
                                    (remove-keys params :selector :expression
                                                 :timeout-ms :timeout)))))
    (format nil "Ready after ~ams" (or (jv result :number "elapsedMs") "?"))))

(defun evaluate (expression &rest params &key (limit *default-limit*) &allow-other-keys)
  "Evaluate JavaScript EXPRESSION in the page (MAIN world); the JSON value as text."
  (json-text (call-action "page.evaluate"
                          (list* :expression expression :await-promise t params))
             limit))

(defun tabs (&rest params)
  "Every open tab, one per line (id, * for active, [group], title, url)."
  (format-tabs (call-action "tab.list" params)))

(defun screenshot-path (format)
  (ensure-directories-exist *screenshot-dir*)
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
    (merge-pathnames
     (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~d.~a"
             y mo d h m s (incf *screenshot-counter*) format)
     *screenshot-dir*)))

(defun write-data-url (data-url path)
  (let* ((comma (position #\, data-url))
         (octets (cl-base64:base64-string-to-usb8-array
                  (subseq data-url (if comma (1+ comma) 0)))))
    (alexandria:write-byte-vector-into-file octets path :if-exists :supersede)
    (namestring path)))

(defun screenshot (&rest params &key (format :png) full-page &allow-other-keys)
  "Capture the viewport (or :full-page t as tiles) to a file; returns the PATH."
  ;; No image reaches the model — read it back with another tool if you must.
  (let* ((format-name (string-downcase (string format)))
         (result (call-action "page.screenshot"
                              (list* :format format :full-page (and full-page t)
                                     :timeout (if full-page 120 *send-timeout-seconds*)
                                     (remove-keys params :format :full-page :timeout))))
         (tiles (jarray result "tiles")))
    (cond
      ((jv result :text "dataUrl")
       (format nil "Saved Chrome screenshot to ~a"
               (write-data-url (jv result :text "dataUrl") (screenshot-path format-name))))
      (tiles
       (let* ((base (screenshot-path format-name))
              (paths (loop for tile in tiles
                           for i from 0
                           collect (write-data-url
                                    (jv tile :text "dataUrl")
                                    (make-pathname
                                     :name (format nil "~a-tile~d" (pathname-name base) i)
                                     :defaults base)))))
         (alexandria:write-string-into-file
          (nlk:pretty-json (nlk:json-object
                            "tiles" (coerce paths 'vector)
                            "dimensions" (or (jv result :object "dimensions") :null)))
          (make-pathname :type "json" :defaults base) :if-exists :supersede)
         (format nil "Saved ~d full-page tiles as ~a-tile*.~a (manifest ~a.json); ~
                      tiles are not stitched"
                 (length paths) (namestring (make-pathname :type nil :defaults base))
                 format-name (namestring (make-pathname :type nil :defaults base)))))
      (t (error 'chrome-command-failed :action "page.screenshot"
                                       :detail "screenshot returned no dataUrl")))))

(defun status (&key (session-id nlk:*scribe-session-id*))
  "Bridge, extension and this session's background default, as text."
  (with-output-to-string (out)
    (if (null *bridge*)
        (format out "bridge: not running (cell not started)~%")
        (let ((age (poll-age-seconds *bridge*)))
          (format out "bridge: ~a (advertising extension ~a)~%extension: ~a~%"
                  (bridge-url *bridge*) (bridge-version *bridge*)
                  (cond ((null age) "never polled")
                        ((bridge-connected-p *bridge*)
                         (format nil "connected (~a, last poll ~ds ago)"
                                 (or (bridge-client-name *bridge*) "unnamed") age))
                        (t (format nil "not polling (last seen ~ds ago)" age))))))
    (format out "session ~a: background ~:[off~;on~]"
            (or session-id "none bound") (session-background session-id))))
