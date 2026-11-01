;;;; cells-sheet-test.lisp --- /cells: every cell on a sheet, the chosen one laid
;;;; open beside the list (tui/cells.lisp).
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism's cells routes are answered from fixtures built here: a
;;;; home whose own folder failed and whose other own folder skipped a file,
;;;; two shipped cells that run, one standing on the other, a shipped cell
;;;; off, and a hub with a provider and a tool. The switch answers as the
;;;; organism does: the listing anew with its sentence, or a refusal.

(in-package #:nodecode.test)

(defparameter +cursor-commit+ "5055220bcab19b96cd46be432cd9765ed3b13bea")

(defun cell-row-json (name &rest fields &key (source "shipped") (on t) &allow-other-keys)
  "A row as CELL-ROW says it: NAME, its label, where it comes from, whether
it is on, and FIELDS, each keyword the key it is spelled as."
  (let ((row (nlk:json-object "name" name "label" (nle::cell-label name) "source" source "on" on
                              "description" "")))
    (loop for (key value) on (alexandria:remove-from-plist fields :source :on) by #'cddr
          do (setf (gethash (string-downcase key) row) value))
    row))

(defun hub-row-json (name path description)
  (cell-row-json name :source "hub" :on nil :description description :index "hub" :loaded nil
                      :git "https://github.com/nodecode-ai/cells" :commit +cursor-commit+ :path path))

(defun cells-listing (state &optional text)
  "The listing route's answer over STATE, (:DISCORD ON :CURSOR ON), after
TEXT when a switch said what came of it."
  (let ((discord (getf state :discord)))
    (nlk:json-object
     :opt "text" text
     "cells" (remove nil
                     (vector
                      (cell-row-json "nodecode-channel-discord" :on discord
                                     :description "Talk to nodecode through a Discord bot"
                                     :loaded t :kind "peripheral" :level (if discord "started" "loaded")
                                     :started discord :commands #("voice") :defines 41
                                     :section "nl-settings.lane"
                                     :folder (and discord "/home/o/.nodecode/cells/nodecode-channel-discord")
                                     :from (and discord "/opt/nodecode/cells/channels/discord"))
                      (cell-row-json "nodecode-channel-kit" :description "Shared plumbing for the chat bots"
                                     :loaded t :kind "peripheral" :level "started" :started t
                                     :commands #("channels" "where")
                                     :kept "channel-discord needs it: turn that off first")
                      (cell-row-json "exploitbench" :source "own" :description "One V8 bug as a container"
                                     :loaded nil :kind "library" :level "error"
                                     :failures (vector (nlk:json-object
                                                        "text" (format nil "docker: command not found~%~
                                                                            while loading lifecycle.lisp: exited with code 127"))))
                      (cell-row-json "harness" :source "own" :loaded t :kind "layer" :level "warning" :defines 8
                                     :skipped (vector (nlk:json-object "file" "probe.lisp" "reason"
                                                                       "it reads nlk::step-hook, which this image no longer has"))
                                     :slow (vector (nlk:json-object "file" "init.lisp" "ms" 310)))
                      (cell-row-json "nodecode-board" :on nil :loaded nil
                                     :description "Sessions that join a board read each other's posts")
                      (and (getf state :cursor)
                           (cell-row-json "nodecode-cursor" :source "hub" :description "Cursor: a browser sign-in"
                                          :loaded t :kind "library" :level "loaded"
                                          :git "https://github.com/nodecode-ai/cells" :commit +cursor-commit+
                                          :path "providers/nodecode-cursor"))))
     "notices" (vector (nlk:json-object "key" "harness" "text" "harness: one file skipped" "level" "warning")))))

(defun cells-route (state method path body)
  "What the organism answers METHOD on PATH with BODY, STATE a box of what
is on, as the cells routes do."
  (cond ((string= path "/api/gateway/cells/hub")
         (nlk:json-object "cells" (vector (hub-row-json "nodecode-cursor" "providers/nodecode-cursor"
                                                        "Cursor: a browser sign-in")
                                          (hub-row-json "nodecode-lsp" "tools/nodecode-lsp"
                                                        "Language servers for Nodecode"))))
        ((eq method :get) (cells-listing (car state)))
        (t (let ((name (nlk:json-value body :string "name"))
                 (on (nlk:json-value body :boolean "on")))
             (cond ((string= name "nodecode-channel-kit")
                    (error 'nle::gateway-http-error :status 400 :code "refused"
                                                    :detail "channel-kit stays on: channel-discord needs it: turn that off first"))
                   ((string= name "nodecode-channel-discord")
                    (setf (getf (car state) :discord) on)
                    (cells-listing (car state) (if on
                                                   "channel-discord is on"
                                                   "channel-discord is off; its code stays loaded in this organism until a restart")))
                   (t (setf (getf (car state) :cursor) on)
                      (cells-listing (car state) "cursor is on")))))))

(defmacro with-cells-gateway ((calls) &body body)
  "Run BODY with the shell's gateway answering the cells routes from the
fixtures, every call to them pushed onto CALLS as (METHOD PATH BODY); any
other route reaches the real call."
  (let ((state (gensym "STATE")))
    `(let ((,calls '())
           (,state (list (list :discord t :cursor nil))))
       (with-stubbed-fdefinition (nle::gateway-model-http-call (model method path body &rest keys)
                                   (if (uiop:string-prefix-p "/api/gateway/cells" path)
                                       (progn (push (list method path body) ,calls)
                                              (cells-route ,state method path body))
                                       (apply original model method path body keys)))
         ,@body))))

(defun cells-open (model)
  "Open /cells on MODEL, the routes answering at once, => the sheet."
  (nle::open-cells-dialog model)
  model.dialog)

(defun cells-chosen (cells)
  "The name of the cell the sheet's keys act on."
  (nle::cells-focus cells (nle::cells-names (nle::cells-items cells))))

(defun cells-heads (cells)
  "The heads the list shows, in order."
  (loop for item in (nle::cells-items cells) when (eq :head (first item)) collect (second item)))

(defun cells-posts (calls)
  "The switches among CALLS, oldest first, as (NAME ON COMMIT)."
  (loop for (method nil body) in (reverse calls)
        when (eq method :post)
          collect (list (nlk:json-value body :string "name") (nlk:json-value body :boolean "on")
                        (nlk:json-value body :string "commit"))))

(defun click-on (model text needle)
  "A left click on the first cell of TEXT in the sheet's row carrying NEEDLE."
  (destructuring-bind (rows x y width) (progn (sheet-rows model) (nle::sheet-dialog-layout model.dialog))
    (declare (ignore width))
    (let ((r (position-if (lambda (row) (and (search needle row) (search text row))) rows)))
      (assert r () "no row carries ~s and ~s" needle text)
      (nle::sheet-click model model.dialog (+ x (search text (nth r rows))) (+ y r)))))

(deftest cells-opens-a-sheet-at-once-and-lands-the-cells-off-the-loop ()
  ;; The listing and the hub are the organism's to read: the sheet stands on
  ;; the keystroke under its reading line, the listing lands, the hub's index
  ;; apart from it, and a read that lands after Esc moves nothing.
  (with-frame-model (model 120 40 :session-id "cells-open")
    (let ((pending '()))
      (with-stubbed-fdefinition (nle::off-loop (model thunk continuation)
                                  (setf pending (append pending (list (cons thunk continuation)))))
        (with-cells-gateway (calls)
          (flet ((land () (destructuring-bind (thunk . continuation) (pop pending)
                            (funcall continuation model (funcall thunk)))))
            (nle::editor-insert model.editor "/cells")
            (press model :enter)
            (is (nle::cells-sheet-p model.dialog) "a sheet in the middle, not a docked list")
            (is (null calls) "nothing read on the loop")
            (is (sheet-row model "Reading cells…"))
            (land)
            (is (equal '((:get "/api/gateway/cells" nil)) calls) "the listing first")
            (is (sheet-row model " Home 5 ") "its cells counted on their tab")
            (is (sheet-row model "2 running · 2 need you"))
            (is (not (sheet-row model " Hub 2 ")) "the hub still being read")
            (land)
            (is (sheet-row model " Hub 2 ") "then the hub's entries, on a tab of their own"))
          (press model :escape)
          (is (null model.dialog) "Esc closes")
          (nle::open-cells-dialog model)
          (press model :escape)
          (destructuring-bind (thunk . continuation) (pop pending)
            (is (eq :quiet (funcall continuation model (funcall thunk))) "a closed sheet takes nothing")))))))

(deftest cells-lists-what-needs-a-look-first-and-lays-the-chosen-one-open ()
  ;; Home: what needs a look first, then by where each comes from, the ones
  ;; off last. The chosen cell is laid open: its failure whole, every line
  ;; of it; a skipped file and a slow one; the commands it adds, its
  ;; settings, whether it runs, where its folder points; why it stays on.
  (with-frame-model (model 120 40 :session-id "cells-list")
    (with-cells-gateway (calls)
      (let ((cells (cells-open model)))
        (is (equal '("Needs you" "Shipped" "Off") (cells-heads cells)))
        (is (equal "exploitbench" (cells-chosen cells)) "the first that needs a look is chosen")
        (is (sheet-row model "It did not load"))
        (is (sheet-row model "docker: command not found") "its failure, whole")
        (is (sheet-row model "while loading lifecycle.lisp: exited with code 127") "every line of it")
        (is (sheet-row model "your own folder") "an own folder has no switch here")
        (press model :down)
        (is (sheet-row model "Skipped probe.lisp: it reads nlk::step-hook"))
        (is (sheet-row model "Slow" "init.lisp took 310 ms to load; every direct launch pays it"))
        (is (sheet-row model "Says" "harness: one file skipped") "what it posted under its name")
        (press model :down)
        (is (equal "nodecode-channel-discord" (cells-chosen cells)))
        (is (sheet-row model "Talk to nodecode through a Discord bot"))
        (is (sheet-row model "Adds" " /voice " "41 definitions"))
        (is (sheet-row model "Settings" "nl-settings.lane" "enter opens it"))
        (is (sheet-row model "Runs" "● running"))
        (is (sheet-row model "→ /opt/nodecode/cells/channels/discord") "where a shipped folder points")
        (is (sheet-row model "[on]") "its switch, on")
        (is (sheet-row model "space" "turn off") "and the key that turns it off")
        (press model :down)
        (is (sheet-row model "Stays on: channel-discord needs it: turn that off first."))
        (is (sheet-row model "kept on"))
        (is (not (sheet-row model "turn off")) "no switch offered where it cannot move")
        (press model :down)
        (is (equal "nodecode-board" (cells-chosen cells)) "the ones off last")
        (is (sheet-row model "Sessions that join a board read each other's posts"))
        (is (sheet-row model "Adds" "known once it loads"))))))

(deftest cells-space-turns-a-cell-on-or-off-and-says-what-came-of-it ()
  ;; Space turns the chosen cell on or off through the organism; the listing
  ;; it answers lands under the same query, on the same cell, with its
  ;; sentence over it. A refusal is said whole. A click on the switch is
  ;; Space.
  (with-frame-model (model 120 40 :session-id "cells-switch")
    (with-cells-gateway (calls)
      (let ((cells (cells-open model)))
        (press model #\d #\i #\s)
        (is (equal "nodecode-channel-discord" (cells-chosen cells)) "typing finds it")
        (press model #\Space)
        (is (equal '(("nodecode-channel-discord" nil nil)) (cells-posts calls)) "Space on a cell that is on asks for off")
        (is (equal "dis" (nle::cells-sheet-query cells)) "the Space is not the query's")
        (is (eq cells model.dialog) "the same sheet")
        (is (equal "nodecode-channel-discord" (cells-chosen cells)) "on the same cell")
        (is (sheet-row model "channel-discord is off; its code stays loaded"))
        (is (sheet-row model "until a restart"))
        (is (sheet-row model "[off]") "its switch, off")
        (click-on model " on " "[off]")
        (is (equal '("nodecode-channel-discord" t nil) (car (last (cells-posts calls)))) "a click on the switch")
        (is (sheet-row model "channel-discord is on"))
        (press model :escape)
        (is (equal "" (nle::cells-sheet-query cells)) "Esc clears the query first")
        (press model #\k #\i #\t #\Space)
        (is (sheet-row model "channel-kit stays on: channel-discord needs it: turn that off first") "a refusal, whole")
        (is (eq cells model.dialog))))))

(deftest cells-a-hub-cell-asks-in-place-and-a-second-space-installs-it ()
  ;; A hub cell runs here with full access: before it does, where it comes
  ;; from and the commit read stand in view, the first Space asks, any other
  ;; key leaves the question, and the second Space installs it at that
  ;; commit. Home then lists it, from the hub.
  (with-frame-model (model 120 40 :session-id "cells-hub")
    (with-cells-gateway (calls)
      (let ((cells (cells-open model)))
        (press model :right)
        (is (eq :hub (nle::cells-sheet-tab cells)) "Right walks to the hub")
        (is (equal '("Providers" "Tools") (cells-heads cells)) "by what each entry is")
        (is (equal "nodecode-cursor" (cells-chosen cells)))
        (is (sheet-row model "From github.com/nodecode-ai/cells/providers/nodecode-cursor") "where it comes from")
        (is (sheet-row model "5055220") "and the commit read")
        (is (sheet-row model "It runs inside Nodecode with full access to your files and keys."))
        (is (sheet-row model "[install]"))
        (press model #\Space)
        (is (null (cells-posts calls)) "the first Space asks")
        (is (sheet-row model "Space again installs it; any other key leaves it."))
        (press model :escape)
        (is (eq cells model.dialog) "Esc leaves the question, not the sheet")
        (is (not (sheet-row model "Space again installs it")))
        (press model #\Space #\Space)
        (is (equal (list (list "nodecode-cursor" t +cursor-commit+)) (cells-posts calls)) "at the commit read")
        (is (sheet-row model "cursor is on"))
        (press model :left)
        (is (member "From the hub" (cells-heads cells) :test #'equal) "Home lists it, from the hub")))))

(deftest cells-typing-searches-home-and-the-hub ()
  (with-frame-model (model 120 40 :session-id "cells-search")
    (with-cells-gateway (calls)
      (let ((cells (cells-open model)))
        (press model #\c #\u #\r)
        (is (equal '("Hub · Providers") (cells-heads cells)) "a query finds the hub's entries from Home")
        (is (equal "nodecode-cursor" (cells-chosen cells)))
        (press model #\x)
        (is (sheet-row model "Nothing matches “curx”"))))))

(deftest cells-enter-opens-the-cells-own-settings ()
  ;; Enter on a cell that declares a section opens /settings on it, once the
  ;; config is read.
  (with-quick-settings (model port config :width 120 :height 40)
    (with-cells-gateway (calls)
      (cells-open model)
      (press model #\d #\i #\s :enter)
      (let ((settings model.dialog))
        (is (nle::settings-sheet-p settings) "the settings sheet")
        (is (eq :pane (nle::settings-sheet-focus settings)) "open on a setting")
        (is (equal "nl-settings.lane"
                   (nle::settings-section-key (nle::setting-part (nle::settings-current settings)))) "of the cell's own section")))))

(deftest cells-sheet-folds-to-the-window-and-never-loses-the-cell ()
  ;; Wide from 84 columns, the list beside the chosen cell; narrower, the
  ;; list, and Enter opens the cell; under 50 columns or 16 rows the whole
  ;; frame. Every row is the window's width.
  (with-frame-model (model 120 40 :session-id "cells-sizes")
    (with-cells-gateway (calls)
      (cells-open model)
      (flet ((size (width height)
               (nle:update model (nlt:make-window-size-msg :width width :height height))
               (sheet-rows model)))
        (is (row-carrying "Search cells and the hub" (size 120 40)) "wide: the search field")
        (is (row-carrying "docker: command not found" (size 120 40)) "beside the list")
        (dolist (size '((120 40) (100 30) (80 24) (70 20) (60 16) (48 14) (40 12)))
          (let ((rows (apply #'size size)))
            (is (row-carrying "exploitbench" rows) (format nil "~{~ax~a~} shows the cell" size))
            (destructuring-bind (painted x y width) (nle::sheet-dialog-layout model.dialog)
              (declare (ignore x y))
              (is (every (lambda (row) (= width (nle::text-display-width (nle::plain-frame-row row)))) painted)
                  (format nil "~{~ax~a~} rows are the window's width" size)))))
        (is (not (row-carrying "docker: command not found" (size 70 20))) "narrow: the list alone")
        (press model :enter)
        (is (row-carrying "Cells › " (size 70 20)) "Enter opens the cell")
        (is (row-carrying "docker: command not found" (size 70 20)))
        (press model :left)
        (is (row-carrying "Needs you" (size 70 20)) "Left comes back to the list")))))

(deftest cells-runs-claims-nothing-an-older-organism-does-not-report ()
  ;; An organism older than the listing's `started' sends no such key: a
  ;; loaded peripheral is then only loaded, never "not running".
  (is (equal "loaded, not running"
             (nle::cell-runs (cell-row-json "nl-x" :loaded t :kind "peripheral" :started nil))))
  (is (equal "loaded" (nle::cell-runs (cell-row-json "nl-x" :loaded t :kind "peripheral"))))
  (is (equal "● running" (nle::cell-runs (cell-row-json "nl-x" :loaded t :kind "peripheral" :started t)))))
