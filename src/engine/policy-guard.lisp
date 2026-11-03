;;;; engine/policy-guard.lisp --- the fixed rules the agent's own policy cannot reach.
;;;;
;;;; With policy.self on (POLICY-SELF-P, engine/policy) the agent writes its own
;;;; AGENTS.md, SOUL.md and system prompt in the policy store, and a change
;;;; binds once the review adopts it. What it does not write are the fixed
;;;; rules, the operator's:
;;;;
;;;;   - the charter: the home's AGENTS.md, riding as `operator-rules' ahead of
;;;;     the agent's own `agent-rules' and outranking them. On a box whose home
;;;;     file is a read-only link (a dotfiles repo through home-manager) the
;;;;     file system already says so; this says it everywhere else.
;;;;   - the config members that decide the mode and its limits: `policy' (the
;;;;     switch, where the record goes), each platform's `owner' (who the
;;;;     operator is), `guard' (what the guard cell refuses) and the guard
;;;;     cell's own row in `cells'.
;;;;
;;;; This file is the refusal the agent's own verbs meet there: EDIT and
;;;; WRITE-FILE (WRITE-FILE-TEXT, engine/exec) and CONFIG-SET (engine/provider),
;;;; when a tool call runs them. The operator's hand -- a shell's /config, a
;;;; settings page, a text editor -- is no tool call and passes. `policy' is
;;;; refused to a tool call with the mode off too: the switch is the operator's
;;;; to throw, never the agent's to take.
;;;;
;;;; A tripwire, not a wall. The agent owns its process: `sh' writes the file
;;;; and a redefinition lifts the check, as UNLOCK-FLOOR lifts any floor in
;;;; here by design. What holds against that is outside the agent -- the record
;;;; pushed to a protected branch (engine/policy-record) and the credentials and
;;;; caps docs/self-policy.mdx lists as step 0. What the tripwire buys is that
;;;; no turn crosses a fixed rule by accident, and that one that tries is told
;;;; why in the words it reads.

(in-package #:nodecode.evolved)

(defparameter *policy-fixed-config*
  '(("policy") ("channels" "*" "owner") ("guard"))
  "The config members a tool call may not write while policy.self is on, each
a path from the root, \"*\" standing for any one key. `policy' is refused to a
tool call in either mode (POLICY-CONFIG-REFUSAL). The guard cell's row in
`cells' is held apart: the list is the agent's to grow, so only a write that
drops the row is refused.")

(defparameter *policy-guard-cell* "nodecode-guard"
  "The cell whose row in `cells' a tool call may not drop while policy.self is on.")

(defun policy-tool-call-p ()
  "True inside an agent's tool call (*EXECUTING-TOOL-CALL-ID*, engine/turn)."
  ;; Read by name: this file loads before engine/turn defines the variable.
  (let ((symbol '*executing-tool-call-id*))
    (and (boundp symbol) (symbol-value symbol) t)))

(defun policy-self-on-p ()
  "POLICY-SELF-P, NIL where its read of the config signals."
  (ignore-errors (policy-self-p)))

(defun charter-path ()
  "The charter: the home's AGENTS.md, the operator's fixed rules."
  (nlk:home "AGENTS.md"))

(defun policy-path-names (path)
  "PATH's absolute namestrings: as written and, when it exists, its truename,
so a link and the file it reaches are one file."
  (let* ((absolute (merge-pathnames path))
         (true (ignore-errors (probe-file absolute))))
    (remove-duplicates (remove nil (list (namestring absolute) (and true (namestring true))))
                       :test #'string=)))

(defun policy-same-file-p (path other)
  "True when PATH and OTHER name one file, through links either way."
  (intersection (policy-path-names path) (policy-path-names other) :test #'string=))

(defun policy-under-p (path directory)
  "True when PATH sits inside DIRECTORY, through links either way."
  (let ((roots (policy-path-names (uiop:ensure-directory-pathname directory))))
    (some (lambda (name) (some (lambda (root) (uiop:string-prefix-p root name)) roots))
          (policy-path-names path))))

(defun policy-write-refusal (path)
  "Why an agent's EDIT or WRITE-FILE of PATH is refused, or NIL: with
policy.self on, inside a tool call, PATH is the charter, the shared config or
the policy store's history."
  (when (and (policy-tool-call-p) (policy-self-on-p))
    (cond ((policy-same-file-p path (charter-path))
           (format nil "~a is the operator's charter: the fixed rules you work under, ~
                        which you do not edit. Your own rules are AGENTS.md in the ~
                        policy store; propose a change there."
                   (namestring (charter-path))))
          ((and *shared-config-path* (policy-same-file-p path *shared-config-path*))
           (format nil "~a holds the fixed config members (~{~{~a~^.~}~^, ~}); write ~
                        any other member with config-set."
                   (namestring *shared-config-path*) *policy-fixed-config*))
          ((policy-under-p path (merge-pathnames ".git/" (policy-directory)))
           "the policy store's history is the review's to write: a draft goes in the store's working tree and binds once the review adopts it."))))

(defun policy-config-key-match-p (pattern key)
  (or (string= pattern "*") (string-equal pattern key)))

(defun policy-config-touches-p (steps value pattern)
  "True when writing VALUE at STEPS (CONFIG-SET's path and value) would change
the member at PATTERN or anything under it."
  ;; An object value merges member by member (CONFIG-SET), so at an ancestor of
  ;; PATTERN only the members it names reach down; any other value, :REMOVE
  ;; among them, replaces the ancestor and everything under it.
  (cond ((null pattern) t)
        ((null steps)
         (if (and (consp value) (keywordp (first value)))
             (loop for (key inner) on value by #'cddr
                   thereis (and (policy-config-key-match-p (first pattern) (symbol-name key))
                                (policy-config-touches-p '() inner (rest pattern))))
             t))
        ((policy-config-key-match-p (first pattern) (first steps))
         (policy-config-touches-p (rest steps) value (rest pattern)))))

(defun policy-cells-listed (value)
  "The cell names a `cells' value lists."
  (and (or (listp value) (and (vectorp value) (not (stringp value))))
       (remove-if-not #'stringp (coerce value 'list))))

(defun policy-config-current (steps)
  "The shared config's member at STEPS, NIL when absent or unreadable."
  (ignore-errors (let ((config (read-shared-config)))
                   (and config (apply #'json-value config :any steps)))))

(defun policy-config-refusal (path value)
  "Why an agent's CONFIG-SET of VALUE at PATH is refused, or NIL. Inside a tool
call: `policy' always, and with policy.self on every member in
*POLICY-FIXED-CONFIG* and a `cells' write that drops the guard cell."
  (let ((steps (config-path path)))
    (when (policy-tool-call-p)
      (cond ((policy-config-touches-p steps value '("policy"))
             "the `policy' member is the operator's switch: they turn self-policy on or off by hand, never through a tool call.")
            ((not (policy-self-on-p)) nil)
            ((find-if (lambda (pattern) (policy-config-touches-p steps value pattern))
                      *policy-fixed-config*)
             (format nil "~{~a~^.~} is a fixed rule while self-policy is on: who the ~
                          operator is, the mode and the guard are the operator's to set."
                     (find-if (lambda (pattern) (policy-config-touches-p steps value pattern))
                              *policy-fixed-config*)))
            ((and (policy-config-touches-p steps value '("cells"))
                  (member *policy-guard-cell* (policy-cells-listed (policy-config-current '("cells")))
                          :test #'string=)
                  (not (member *policy-guard-cell*
                               (policy-cells-listed
                                (cond ((equal steps '("cells")) (if (eq value :remove) '() value))
                                      ((null steps) (getf value :cells))
                                      (t (policy-config-current '("cells")))))
                               :test #'string=)))
             (format nil "~a stays in `cells' while self-policy is on: the guard cell ~
                          is one of the operator's fixed rules." *policy-guard-cell*))))))

(defun charter-refusal (names)
  "Why a policy proposal over NAMES (store file names) is refused, or NIL: a
name that is not a plain file of the store -- a path, `..', a dot file such as
.git -- could reach the charter, the config or the store's history, which no
proposal changes."
  (let ((bad (find-if-not #'policy-name-p names)))
    (and bad
         (format nil "~s is not a file of the policy store: a proposal changes the ~
                      store's own files, never the charter (~a), the config or the ~
                      store's history." bad (namestring (charter-path))))))
