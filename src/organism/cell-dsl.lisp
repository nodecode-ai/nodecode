;;;; cell-dsl.lisp --- what ONE cell declares about itself: the composition DSL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; cells.lisp, in the kernel, is the FOLDER: which directories loaded, what
;;;; each defined, overrode and collided with, which START-CELL the gateway
;;;; calls after recovery and which stop thunk it funcalls on the way down.
;;;; This file is the other half and lives in EVOLVED, because what it
;;;; composes are evolved registries: one cell's own declaration.
;;;;
;;;; Every shipped peripheral had written the same protocol by hand — read the
;;;; section out of the whole config, guard on `enabled', build a settings
;;;; struct from a parser written beside the struct, INSTALL (hooks, the tool,
;;;; the command, the threads), hand back a lambda over a STOP-CELL that
;;;; re-listed every one of those registrations in reverse. Three things were
;;;; wrong with it and none of them were the author's fault:
;;;;
;;;;   - the inverse list is hand-kept, so it drifts. Six folders registered a
;;;;     slash command and NOT ONE took it back on stop, because until this
;;;;     slice there was no UNREGISTER-EXTENSION to call: a stopped cell
;;;;     kept answering its own /command off a registry it no longer served.
;;;;   - a start that fails halfway leaves its prefix registered. The gateway
;;;;     records the refusal and moves on; the hooks stay.
;;;;   - the settings struct and its parser restate the section declaration
;;;;     (NLK:DEFINE-SECTION) a third and fourth time, in a place that cannot
;;;;     disagree with it loudly.
;;;;
;;;; So: one generic primitive and one declaration over it.
;;;;
;;;; STARTING is the primitive. It opens a registration scope, answers the
;;;; thunk that closes it, and unwinds its own prefix when the body leaves
;;;; non-locally. ON-STOP records one inverse in it. Nothing in either knows
;;;; what a cell is — a lease, a table, a thread, a directory all belong in
;;;; one — and they stay the escape hatch beside the declaration, for the
;;;; contributions no clause covers.
;;;;
;;;; DEFINE-CELL is the declaration. It composes the primitives that already
;;;; existed and replaces none of them: NLK:DEFINE-SECTION for the config,
;;;; NLK:SECTION-SETTINGS for the typed values, HOOK/UNHOOK for advice,
;;;; SET-HELP-TOPIC for a manual, REGISTER-TOOL/UNREGISTER-TOOL,
;;;; REGISTER-COMMAND/UNREGISTER-EXTENSION. Every one is still callable raw.
;;;; It defines START-CELL and STOP-CELL in the cell's own package, which
;;;; is what the folder loader and the gateway lifecycle already look for, so
;;;; nothing about dispatch, reporting or the refusal path changes: a start
;;;; that signals is still the record's REFUSAL and one standing notice.
;;;;
;;;; It deliberately does NOT own identity: NLK:DEFINE-PERIPHERAL still
;;;; declares the condition, FAIL, the settings variable and the lock, because
;;;; those names are needed by the cell's other files, which load before
;;;; this declaration does.

(in-package #:nodecode.evolved)

;;; --- a registration scope -------------------------------------------------

;;; DEFVAR, not
;;; DEFPARAMETER: this file reloading mid-serve must not strand the scope a
;;; running start is filling.
(defvar *registrations* nil
  "The innermost STARTING scope while one runs — a cons whose CAR is the
list of inverses, newest first — or NIL outside one.")

(defun on-stop (fn)
  "Record FN as the inverse of what was just registered, in the innermost
STARTING scope."
  ;; It runs when that scope's thunk runs — after every inverse recorded
  ;; later, before every inverse recorded earlier — so undoing is registration
  ;; order reversed, which is the order a lifecycle needs.
  ;;
  ;; Signalling outside a scope is deliberate: an inverse nothing will ever
  ;; run is exactly the drift this exists to remove. Returns FN.
  (check-type fn function)
  (unless *registrations*
    (error "on-stop: no (starting ...) scope is open on this thread"))
  (push fn (car *registrations*))
  fn)

(defun run-stops (scope)
  "Run SCOPE's inverses newest first, each under its own boundary so one
that signals does not strand the rest."
  ;; The list empties as it goes, which is the whole of idempotence: a second
  ;; run has nothing left to do.
  (loop for fn = (pop (car scope))
        while fn
        do (handler-case (funcall fn) (error (condition) (warn "stopping: ~a" condition))))
  t)

(defun call-starting (thunk &aux (scope (list '()))
                                 (finished nil))
  "STARTING's function."
  (with-cleanup ((unless finished (run-stops scope)))
    (let ((*registrations* scope)) (funcall thunk))
    (setf finished t))
  (lambda () (run-stops scope)))

(defmacro starting (&body body)
  "Run BODY as one registration scope and answer the thunk that undoes it."
  ;; Inside BODY, (ON-STOP fn) records how to take one registration back. BODY
  ;; leaving non-locally — a refused config, a lease another process holds, a
  ;; thread that would not start — runs every inverse recorded so far, newest
  ;; first, before the condition goes up: a start that fails halfway leaves
  ;; nothing behind for its next try to collide with, and its siblings are
  ;; untouched. The answered thunk is idempotent, so calling it twice, or
  ;; calling it after a failed start, does nothing the second time.
  ;;
  ;; The primitive under DEFINE-CELL and the escape hatch beside it:
  ;;
  ;;   (nle:starting
  ;;     (let ((worker (nlk:worker-start "mine" #'step)))
  ;;       (nle:on-stop (lambda () (nlk:worker-stop worker))))
  ;;     (take-the-lease)
  ;;     (nle:on-stop #'release-the-lease))
  `(call-starting (lambda () ,@body)))

;;; --- one cell, declared -------------------------------------------------

(defun cell-setting (settings key)
  "SETTINGS' value at KEY."
  ;; A key the declaration does not carry is an error, never a silent NIL: a
  ;; misspelt setting is a bug, and a plist read with GETF would have answered
  ;; as though the operator had left it out.
  (bind (((_ value tail) (get-properties settings (list key))))
    (unless tail
      (error "no setting ~s; this cell declares ~{~s~^ ~}"
             key (loop for (name nil) on settings by #'cddr collect name)))
    value))

;;; --- the words a shell puts on a cell's calls ---------------------------------
;;;
;;; A shell runs in a process of its own beside the organism
;;; (ATTACH-OR-OWN-ORGANISM): it loads the folder, so every cell's code is
;;; there, and it starts none of them -- the organism does. A cell's hook on a
;;; function that words a call was then laid where the organism words its
;;; rows and digests, never where the shell draws: an acp call read as the
;;; command it ran in a browser and as `acp' in the shell. So the shell lays
;;; those hooks itself, from the declaration, and nothing else of the cell: no
;;; tool, no worker, no settings.

(defparameter *shell-points*
  '(tool-arguments-activity tool-activity-verb tool-result-shown)
  "The functions a shell words a call with, from the call alone: a cell's
:hook on one is laid in the shell too (SHOW-CELLS), not only where it starts.")

(defvar *cell-hooks* (make-hash-table :test 'equal :synchronized t)
  "Each declared cell's :hook clauses, by its key: (PATH . THUNKS), PATH its
section's, each thunk answering (values POINT FN) as the clause evaluates them.")

(defun note-cell-hooks (key path thunks)
  "Keep the cell KEY's section PATH and :hook THUNKS for SHOW-CELL."
  (setf (gethash key *cell-hooks*) (cons path thunks)))

(defun show-cell (key config)
  "Lay the cell KEY's :hook clauses whose point is one of *SHELL-POINTS*, under
KEY, unless CONFIG vetoes its section; nothing else of it. => the points laid."
  (destructuring-bind (&optional path &rest thunks) (gethash key *cell-hooks*)
    (when (and path (section-enabled-p (section-table config path)))
      (loop for thunk in thunks
            for (point fn) = (multiple-value-list (funcall thunk))
            when (member point *shell-points*)
              do (hook point key fn)
              and collect point))))

(defun show-cells (&optional (config (read-shared-config)))
  "SHOW-CELL every loaded cell whose folder is on (NLK:CELL-FOLDER-ON-P),
starting none: a shell's pass over the folder it loaded. A cell whose hook
refuses is said on *ERROR-OUTPUT* and stepped over. => the keys that laid one."
  (loop for cell in (nlk:cells)
        when (and (eq :loaded (nlk::cell-state cell)) (nlk:cell-folder-on-p cell))
          append (loop for system in (nlk::cell-systems cell)
                       for key = (string-downcase system)
                       when (handler-case (show-cell key config)
                              (error (condition)
                                (format *error-output* "~&cell ~a: its calls' words not laid in the shell: ~a~%"
                                        (nlk::cell-name cell) condition)
                                nil))
                         collect key)))

(defmacro define-cell (name &body clauses)
  "Declare the cell NAME: its config section, the typed values it runs
on, everything it contributes, and — derived from that, not restated — how
all of it comes back off."
  ;; Defines START-CELL, STOP-CELL and SETTING in NAME's package (NAME is a
  ;; symbol there, as NLK:DEFINE-PERIPHERAL's is), and answers NAME.
  ;;
  ;; Every contribution is filed under one key, the declaring package's name
  ;; downcased, which by the folder convention is the folder's name:
  ;; NODECODE-QA registers its hooks and its command as "nodecode-qa".
  ;;
  ;; Clauses, in any order; the contributions install in the order written and
  ;; come off in the reverse:
  ;;
  ;;   (:section (PATH...) CLAUSE...)   required. NLK:DEFINE-SECTION verbatim —
  ;;       the fields with their types, defaults and docs, the constraints, the
  ;;       guide. PATH is the member path in the shared config. The section is
  ;;       the authority for FOUR things at once: the settings a page and
  ;;       /settings draw, the model's primer, the cells report's line, and
  ;;       the values below.
  ;;   (:settings FUNCTION)   (lambda (VALUES TABLE) => plist). For a section
  ;;       whose shape the declaration cannot carry — a nested object, a value
  ;;       read off the environment, a default computed from another member.
  ;;       VALUES is what the declaration derived; TABLE the section's object.
  ;;       It is the section's own READER (NLK:SECTION-SETTINGS), so a
  ;;       writer that asks the section before it writes refuses what this
  ;;       refuses, as the start does.
  ;;   (:tool NAME SCHEMA FUNCTION)   REGISTER-TOOL; stop
  ;;       unregisters it.
  ;;   (:command COMMAND RUN &key description label catches session trigger reads)
  ;;       REGISTER-COMMAND under the cell's key, LABEL and CATCHES naming
  ;;       the failure line the way it does (the command and ERROR by
  ;;       default), SESSION NIL for a command that acts on no session,
  ;;       TRIGGER the character typed as the draft's first that opens it;
  ;;       RUN called with no argument answers the command's dialog. An
  ;;       ARGUMENT-HINT or COMPLETE a cell written before still passes is
  ;;       taken and shown nowhere. Stop takes the cell's commands out of
  ;;       the catalog. One clause per command.
  ;;   (:help TOPIC SUMMARY TEXT)   the cell's manual: (help TOPIC) answers
  ;;       TEXT, and every request's help section carries SUMMARY, one line
  ;;       (SET-HELP-TOPIC); stop takes the topic off.
  ;;   (:hook NAME FN)   HOOK on a point (:tool, :frame, :credential) or a
  ;;       function name,
  ;;       under the cell's key; stop unhooks it. On one of *SHELL-POINTS*
  ;;       a shell lays it too, starting nothing (SHOW-CELLS).
  ;;   (:route PATH FN &key public)   ROUTE: PATH answered on the gateway's
  ;;       own port to the operator's token, to anyone under PUBLIC, a PATH
  ;;       ending in / everything under it; stop takes it back.
  ;;   (:start FUNCTION)   (lambda () ...), run inside the scope with the
  ;;       settings variable already set: everything no clause covers — a directory,
  ;;       a worker, a lease. Call ON-STOP there for its inverse.
  ;;
  ;; START-CELL stops a previous start before it begins, so starting twice
  ;; leaves one of everything, and answers STOP-CELL itself — the gateway's
  ;; funcall and (stop-cell) are one path. A vetoed section ("enabled":
  ;; false) installs nothing and still answers a stop. A refusal signals,
  ;; which is what the gateway's lifecycle already renders as the record's
  ;; REFUSAL and one standing notice under the folder's name.
  (let* ((package (symbol-package name))
         (key (string-downcase (package-name package)))
         (variable (intern (format nil "*~a*" (symbol-name name)) package))
         (running (intern "RUNNING-SETTINGS" package))
         (setting (intern "SETTING" package))
         (stop-variable (intern "*STOP*" package))
         (start-cell (intern "START-CELL" package))
         (stop-cell (intern "STOP-CELL" package))
         (settings (gensym "SETTINGS"))
         (hooks '())
         (path nil)
         (section nil)
         (custom nil)
         (seen '())
         (forms '()))
    (flet ((contribute (bindings install uninstall)
             ;; INSTALL under BINDINGS, its inverse UNINSTALL recorded in the scope
             (push `(let ,bindings ,install (on-stop (lambda () ,uninstall))) forms)))
      (dolist (clause clauses)
        (destructuring-bind (head &rest arguments) clause
          ;; Two clauses have one owner each.
          (when (and (member head '(:section :settings)) (member head seen))
            (error "define-cell ~s: one ~s clause at most" name head))
          (push head seen)
          (ecase head
            (:section (setf path (copy-list (first arguments))
                            section (rest arguments)))
            (:settings (setf custom (first arguments)))
            (:tool
             (destructuring-bind (tool schema function &aux (it (gensym "TOOL")))
                 arguments
               (contribute `((,it ,tool)) `(register-tool ,it ,schema ,function)
                           `(unregister-tool ,it))))
            (:command
             (destructuring-bind (spelling run &rest keys
                                  &key description argument-hint label catches session
                                  trigger complete reads)
                 arguments
               (declare (ignore description argument-hint label catches session trigger
                                complete))
               ;; The keys are evaluated, so a bare (:draft) would load as a call
               ;; to an undefined function :DRAFT; say what was meant here instead.
               (when (and (consp reads) (keywordp (first reads)))
                 (error "/~a: :reads ~s is evaluated; quote it: :reads '~(~s~)"
                        spelling reads reads))
               (contribute '() `(register-command ,key ,spelling ,run ,@keys)
                           `(unregister-commands ,key))))
            (:help
             (destructuring-bind (topic summary text &aux (it (gensym "TOPIC"))) arguments
               (contribute `((,it ,topic)) `(set-help-topic ,it ,summary ,text)
                           `(set-help-topic ,it nil))))
            (:hook
             (destructuring-bind (point fn &aux (it (gensym "POINT"))) arguments
               (push `(lambda () (values ,point ,fn)) hooks)
               (contribute `((,it ,point)) `(hook ,it ,key ,fn) `(unhook ,it ,key))))
            (:route
             (destructuring-bind (at fn &key public &aux (it (gensym "PATH"))) arguments
               (contribute `((,it ,at)) `(route ,it ,fn :public ,public) `(route ,it nil))))
            (:start (push `(funcall ,(first arguments)) forms))))))
    (unless path
      (error "define-cell ~s: a (:section (PATH...) ...) clause is required" name))
    `(progn
       (define-section ,path ,@section ,@(when custom `((:reader ,custom))))
       ;; The folder loader starts a package whose START-CELL is external. At
       ;; load, not compile: a DEFPACKAGE in the same file re-runs at load, and
       ;; must not find a symbol exported that it does not list.
       (export ',start-cell ,(package-name package))
       ;; What a shell lays of it, in a process that starts nothing (SHOW-CELLS).
       (note-cell-hooks ,key ',path (list ,@(reverse hooks)))
       (defvar ,variable nil
         "The settings the cell runs on, or NIL while it is not started.")
       (defvar ,stop-variable nil
         "The thunk that undoes the running start, or NIL while stopped.")
       (defun ,setting (key)
         ,(format nil "The running cell's value for KEY, one of the keys the `~a' ~
section declares (WIRE-SYMBOL names them: max_results is :MAX-RESULTS). ~
Refuses while the cell is not running, and on a key the declaration does not carry."
                  (section-key path))
         (cell-setting (,running) key))
       (defun ,stop-cell ()
         "Undo START-CELL: every registration the declaration made, newest
first, then the settings. Idempotent — a second call does nothing."
         (when-let (stop (shiftf ,stop-variable nil)) (funcall stop))
         t)
       (defun ,start-cell (config)
         ,(format nil "Start the cell from CONFIG, the whole parsed shared config: ~
read the `~a' section, materialize the settings, install what the declaration ~
contributes. Answers STOP-CELL, which the gateway funcalls on the way down. A ~
vetoed section installs nothing; a refused one signals NLK:CONFIG-REFUSAL, the ~
loader's one loud warning; a start that fails halfway leaves nothing registered."
                  (section-key path))
         (,stop-cell)
         (let ((table (section-table config ',path)))
           (when (section-enabled-p table)
             (setf ,stop-variable
                   (starting
                     (let ((,settings (section-settings (find-section ',path) table)))
                       (setf ,variable ,settings)
                       (on-stop (lambda () (setf ,variable nil)))
                       ,@(nreverse forms))))))
         #',stop-cell)
       ',name)))
