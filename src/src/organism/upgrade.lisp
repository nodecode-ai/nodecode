;;;; upgrade.lisp --- what a staged tree changes in this image, classified.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An upgrade lays a new tree beside the one this image loaded from: the
;;;; installer stages a version directory and `current' has not moved. Before
;;;; anything touches the live image, CLASSIFY-UPGRADE reads both trees and
;;;; answers, definition by definition, what can go live here through
;;;; LOAD-SOURCE :ONLY and what forces a restart onto the new binary. Pure
;;;; over the two trees and this image's packages: it evaluates nothing.
;;;;
;;;; The classes, each proved in a bare SBCL:
;;;;   hot      defun, defmethod, defvar, defparameter, defsetf, define-*,
;;;;            defclass (CLOS migrates live instances), and a NEW definition
;;;;            of any operator: nothing live depends on it yet.
;;;;   hot+     a changed defmacro, or a changed function the tree declaims
;;;;            inline: live callers keep the old expansion, so every
;;;;            definition in the new tree that names it is evaluated again,
;;;;            as a dependent.
;;;;   restart  a changed defstruct or define-record (live instances go
;;;;            obsolete), defpackage, defconstant, deftype (compiled into
;;;;            its users), a defgeneric whose lambda list moved under its
;;;;            methods, text outside any definition (a toplevel SETF is
;;;;            boot state), a package this image lacks, and a system the
;;;;            new .asd depends on that this image never loaded: that one
;;;;            is a new binary by definition.
;;;; A kernel name goes live under a lowered floor: the lock guards the model's
;;;; mutations, and an upgrade is the maintainer's act. The plan names them so
;;;; the transaction lowers the floor and raises it again.

(in-package #:nodecode.kernel)

(define-refusal upgrade-refusal upgrade-fail "upgrade: ~a"
  "An upgrade that cannot proceed as asked, and why. The image is as it was." :export t)

;;; The build identity stays *USER-AGENT-VERSION* (the evolved side's); what
;;; an ack, the identity route and a build-skew notice compare is this,
;;; through EFFECTIVE-VERSION (update.lisp).
(defvar *effective-version* nil
  "The release label this image's live definitions are at once a hot
upgrade moved them past the build's own -- NIL until one does.")

(defparameter *upgrade-hold-reasons*
  '(("DEFSTRUCT" . "defstruct leaves every live instance obsolete")
    ;; DEFSTRUCT under the record's own name: sent hot, SBCL refused the
    ;; redefinition and every box stayed.
    ("DEFINE-RECORD" . "define-record is a defstruct: every live instance goes obsolete")
    ("DEFPACKAGE" . "defpackage moves symbols under running code")
    ("DEFCONSTANT" . "a constant's redefinition is a continuable error")
    ("DEFTYPE" . "a type's expansion is compiled into its users")
    ("DEFINE-KIND" . "an event kind's contract moves; the store needs the new build"))
  "Operators whose CHANGED definition cannot go live, and why.")

(defparameter *upgrade-hold-names*
  '("*EVENTS-SCHEMA*" "*SCHEMA*" "*OPEN-TURNS-SCHEMA*" "*SESSION-DIRECTORY-SCHEMA*")
  "Names whose CHANGED definition cannot go live whatever its operator: the
store's DDL is applied at open, not at eval.")
;; *EVENTS-SCHEMA* splices the open turns' and the directory's in when it is
;; read, so a change to either leaves its text as it was: each is named here.

(defun tree-systems (root &aux (parent (uiop:pathname-parent-directory-pathname (truename root)))
                               forms unreadable)
  "=> (values FORMS UNREADABLE): each form of the .asd files under ROOT's
parent -- the organism's own and each shipped cell's -- read as data, as
(ASD-DIRECTORY . FORM) in file and text order; and the .asd files that did
not read."
  (dolist (path (sort (directory (merge-pathnames "**/*.asd" parent)) #'string< :key #'namestring))
    (with-handlers ((error () (push (enough-namestring path parent) unreadable)))
      (let ((*package* (find-package "ASDF-USER")) (*read-eval* nil) (read '()))
        (with-open-file (in path)
          (loop for form = (read in nil in) until (eq form in)
                do (push (cons (uiop:pathname-directory-pathname path) form) read)))
        (setf forms (revappend read forms)))))
  (values (nreverse forms) unreadable))

(defun tree-lisp-files (root &aux files systems (ordered '()) (visited '()))
  "The .lisp files under ROOT as namestrings relative to it, in load order:
every one a DEFSYSTEM of TREE-SYSTEMS names, in its component order, each
system after those it depends on; then the rest, sorted. NIL when ROOT is
not a directory."
  ;; Sorted alone, engine/exec.lisp would be evaluated before the
  ;; package.lisp export it reads and the image would refuse the release:
  ;; the .asd is the load order, a directory only
  ;; a file's address.
  (when-let (root (ignore-errors (truename root)))
    (setf files (sort (mapcar (lambda (path) (enough-namestring path root))
                              (directory (merge-pathnames "**/*.lisp" root)))
                      #'string<)
          systems (loop for (directory . form) in (tree-systems root)
                        when (and (consp form) (consp (cdr form)) (symbolp (first form))
                                  (string= (first form) "DEFSYSTEM") (stringp (second form)))
                          collect (list* (string-downcase (second form)) directory (cddr form))))
    (labels ((option (plist key) (loop for (k v) on plist by #'cddr when (eq k key) return v))
             (under (name base &rest keys)
               (uiop:merge-pathnames* (apply #'uiop:parse-unix-namestring name keys) base))
             (walk (components base)
               (dolist (component (and (listp components) components))
                 (when (and (consp component) (stringp (second component)))
                   (case (first component)
                     (:module (walk (option (cddr component) :components)
                                    (under (second component) base :ensure-directory t)))
                     (:file (let ((file (enough-namestring (under (second component) base :type "lisp") root)))
                              (when (member file files :test #'string=)
                                (pushnew file ordered :test #'string=))))))))
             (visit (name &aux (system (assoc name systems :test #'string=)))
               (unless (member name visited :test #'string=)
                 (push name visited)
                 (when-let (plist (cddr system))
                   (dolist (dependency (option plist :depends-on))
                     (when (stringp dependency) (visit (string-downcase dependency))))
                   (walk (option plist :components)
                         (if (stringp (option plist :pathname))
                             (under (option plist :pathname) (second system) :ensure-directory t)
                             (second system)))))))
      (mapc #'visit (mapcar #'first systems))
      (append (reverse ordered) (remove-if (lambda (file) (member file ordered :test #'string=)) files)))))

(defun tree-dependencies (root &aux names)
  "=> (values NAMES UNREADABLE): every system TREE-SYSTEMS' forms depend on,
lowercase strings, and the .asd files that did not read."
  (labels ((take (entry)
             (when-let (name (cond ((typep entry '(or string symbol)) (string entry))
                                   ((consp entry) (find-if #'stringp entry))))
               (pushnew (string-downcase name) names :test #'string=)))
           (walk (tree)
             (when (consp tree)
               (loop for (key value) on tree
                     when (and (eq key :depends-on) (listp value))
                       do (mapc #'take value))
               (do ((rest tree (cdr rest))) ((atom rest)) (walk (car rest))))))
    (multiple-value-bind (forms unreadable) (tree-systems root)
      (mapc (lambda (form) (walk (cdr form))) forms)
      (values names unreadable))))

(defun form-lambda-list (body package)
  "The lambda list of the definition BODY read under PACKAGE, or :UNREADABLE."
  (handler-case (let ((*package* package) (*read-eval* t))
                  (third (read-from-string body)))
    (error () :unreadable)))

(defun defpackage-export-only-change-p (old-body new-body package)
  "Whether the two DEFPACKAGE texts differ in their :EXPORT options alone."
  ;; Re-evaluating that is hot: a new export is made, and one the file dropped
  ;; stays exported as a definition the file dropped stays defined (GONE). Any
  ;; other option -- :USE, :SHADOW, a nickname -- moves symbols under running
  ;; code. Symbols read fresh each time, so options compare as names.
  (labels ((plain (x) (cond ((consp x) (mapcar #'plain x))
                            ((stringp x) (string-upcase x))
                            ((symbolp x) (symbol-name x))
                            (t x)))
           (options (body)
             (with-handlers ((error () :unreadable))
               (let ((*package* package) (*read-eval* nil))
                 (sort (mapcar #'prin1-to-string
                               (remove "EXPORT" (plain (cddr (read-from-string body)))
                                       :key #'first :test #'equal))
                       #'string<)))))
    (let ((old (options old-body)) (new (options new-body)))
      (and (listp old) (listp new) (equal old new)))))

(defun file-package-name (text)
  "The package TEXT's first IN-PACKAGE names, upcased, or NIL."
  (ppcre:register-groups-bind (name) ("(?i)\\(in-package\\s+[#:'\"]*([^\\s)\"]+)" text)
    (string-upcase name)))

(defun hold-reason (entry old-body new-text &aux (key (first entry))
                                                 (operator (getf key :operator))
                                                 (package (fourth entry))
                                                 (new-body (subseq new-text (second entry) (third entry))))
  "Why ENTRY, a definition of NEW-TEXT whose previous text was OLD-BODY (NIL
when new), cannot go live in this image -- or NIL when it can."
  (cond ((not (packagep package))
         (format nil "package ~(~a~) is not in this image" package))
        ((null old-body) nil)
        ((member (getf key :name) *upgrade-hold-names* :test #'string=)
         "the store's schema is applied at open, not at eval")
        ((and (string= operator "DEFPACKAGE") (defpackage-export-only-change-p old-body new-body package))
         nil)
        ((cdr (assoc operator *upgrade-hold-reasons* :test #'string=)))
        ((and (string= operator "DEFGENERIC")
              (not (equal (form-lambda-list old-body package) (form-lambda-list new-body package))))
         "a generic's lambda list moved under its methods")))

(defun inline-names (texts &aux (names '()))
  "Every name the texts declaim inline, upcased."
  (dolist (text texts names)
    (ppcre:do-register-groups (group) ("(?i)\\(declaim\\s+\\(inline([^)]*)\\)" text)
      (dolist (name (split-words group))
        (pushnew (string-upcase name) names :test #'string=)))))

(defun classify-upgrade (old-root new-root &aux held gone floor dependents expanders unloaded
                                                symbols (changed 0) (added 0) (removed 0))
  "Read the tree at NEW-ROOT against the one at OLD-ROOT -- the tree this
image loaded from -- and say what an upgrade onto NEW-ROOT does to this
image, evaluating nothing."
  ;; => a plist:
  ;;   :verdict    :same (nothing for this image to do), :hot (everything goes
  ;;               live here), or :restart
  ;;   :hot        ((FILE . ((OPERATOR . LABEL) ...)) ...): what LOAD-SOURCE :ONLY
  ;;               evaluates, FILE relative to the root, in load order, dependents
  ;;               of a changed macro or inline function included
  ;;   :held       ((LABEL . REASON) ...): what forces the restart
  ;;   :gone       labels the new tree no longer defines (still in the image)
  ;;   :dependents labels evaluated again only because an expander they use moved
  ;;   :floor      hot labels the kernel owns: the transaction lowers the floor
  ;;   :unloaded   files of a package this image never loaded -- a cell that is
  ;;               not enabled here -- which the upgrade leaves on disk
  ;;   :files      (:changed N :added N :removed N :unloaded N)
  (let* ((old-root (truename old-root))
         (new-root (truename new-root))
         (old-files (tree-lisp-files old-root))
         (new-files (tree-lisp-files new-root))
         (texts (make-hash-table :test 'equal))
         (old-texts (make-hash-table :test 'equal))
         (definitions (make-hash-table :test 'equal))
         (hot (make-hash-table :test 'equal))
         (taken (make-hash-table :test 'equal)))
    (flet ((take (file entry &aux (key (first entry)) (label (definition-key-label key)))
             ;; ENTRY of FILE goes live: in load order, its name in the undo log,
             ;; the floor lowered for a kernel name. => its label.
             (push (list (second entry) (getf key :operator) label) (gethash file hot))
             (setf (gethash (cons file (second entry)) taken) t)
             (pushnew (cons (package-name (fourth entry)) (getf key :name)) symbols :test #'equal)
             (when (kernel-owned-name-p (getf key :name)) (pushnew label floor :test #'string=))
             label))
      (dolist (file new-files)
        (setf (gethash file texts)
              (uiop:read-file-string (merge-pathnames file new-root) :external-format :utf-8)))
      (dolist (file old-files)
        (setf (gethash file old-texts)
              (uiop:read-file-string (merge-pathnames file old-root) :external-format :utf-8))
        (unless (gethash file texts)
          (incf removed)
          (dolist (entry (file-definitions (merge-pathnames file old-root) :read-eval t))
            (push (definition-key-label (first entry)) gone))))
      ;; A package the old tree's files declare and this image lacks is one
      ;; this image chose not to load: a cell that is not enabled here. Its
      ;; files, old and new, are not this image's to evaluate. A package no
      ;; old file declares is new with the upgrade, and its definitions hold.
      (let* ((absent (loop for file in old-files
                           for name = (file-package-name (gethash file old-texts))
                           when (and name (not (find-package name))) collect name))
             (inline (inline-names (loop for file in new-files collect (gethash file texts)))))
        (dolist (file new-files)
          (let* ((new-text (gethash file texts))
                 (old-text (gethash file old-texts))
                 (same (and old-text (string= old-text new-text))))
            (if (member (file-package-name new-text) absent :test #'string=)
                (unless same (push file unloaded))
            (handler-case
                (multiple-value-bind (entries lost residue after)
                    (if same
                        (values nil nil nil (toplevel-definitions new-text :read-eval t))
                        (definition-changes old-text new-text))
                  (setf (gethash file definitions) after)
                  (unless same (if old-text (incf changed) (incf added)))
                  (setf gone (append gone lost))
                  (when residue
                    (push (cons file "changes outside any definition are boot state") held))
                  (dolist (pair entries)
                    (let* ((entry (car pair))
                           (key (first entry))
                           (reason (hold-reason entry (cdr pair) new-text)))
                      (cond (reason (push (cons (definition-key-label key) reason) held))
                            (t (take file entry)
                               (when (and (cdr pair)
                                          (or (string= (getf key :operator) "DEFMACRO")
                                              (and (string= (getf key :operator) "DEFUN")
                                                   (member (getf key :name) inline
                                                           :test #'string=))))
                                 (pushnew (getf key :name) expanders :test #'string=)))))))
              (error (condition)
                (push (cons file (format nil "does not read in this image: ~a"
                                         (or (first (lines (princ-to-string condition))) "")))
                      held)))))))
      ;; dependents: every definition in the new tree that calls an expander
      (when expanders
        (let ((use (ppcre:create-scanner
                    (format nil "(?i)\\((?:[\\w.-]+::?)?(?:~{~a~^|~})(?=[\\s()'\"])"
                            (mapcar #'ppcre:quote-meta-chars expanders)))))
          (dolist (file new-files)
            (let ((text (gethash file texts)))
              (dolist (entry (gethash file definitions))
                (let ((key (first entry)))
                  (unless (or (gethash (cons file (second entry)) taken)
                              (not (packagep (fourth entry)))
                              (member (getf key :name) expanders :test #'string=)
                              (member (getf key :operator) '("DEFSTRUCT" "DEFINE-RECORD" "DEFPACKAGE" "DEFCONSTANT" "DEFTYPE")
                                      :test #'string=)
                              (not (ppcre:scan use text :start (second entry) :end (third entry))))
                    (push (take file entry) dependents))))))))
      ;; the .asd gate: a system this image never loaded is a new binary
      (multiple-value-bind (new-systems unreadable) (tree-dependencies new-root)
        (let ((old-systems (tree-dependencies old-root))
              ;; read under the load lock, as UNAVAILABLE-SYSTEMS does: a
              ;; load beside redefines the systems this walks (xa-203)
              (loaded (mapcar #'string-downcase
                              (bt2:with-recursive-lock-held (*cell-load-lock*)
                                (asdf:already-loaded-systems)))))
          (dolist (file unreadable)
            (push (cons file "the system definition does not read") held))
          (dolist (name new-systems)
            (unless (or (member name old-systems :test #'string=)
                        (member name loaded :test #'string=))
              (push (cons name "a system this image never loaded") held)))))
      (let ((plan-hot (loop for file in new-files
                            for entries = (gethash file hot)
                            when entries
                              collect (cons file (mapcar (lambda (entry) (cons (second entry) (third entry)))
                                                         (sort entries #'< :key #'first)))))
            (held (nreverse held))
            (unloaded (nreverse unloaded)))
        (list :verdict (cond (held :restart)
                             ((or plan-hot gone (plusp (+ changed added removed))) :hot)
                             (t :same))
              :hot plan-hot
              :held held
              :gone gone
              :dependents (nreverse dependents)
              :floor (nreverse floor)
              :unloaded unloaded
              :symbols (nreverse symbols)
              :files (list :changed changed :added added :removed removed
                           :unloaded (length unloaded)))))))

(defun upgrade-plan-line (plan &aux (count (loop for (nil . pairs) in (getf plan :hot) sum (length pairs)))
                                    (held (getf plan :held)))
  "PLAN, one sentence: what a receipt, a status line or a room notice says."
  (ecase (getf plan :verdict)
    (:same "same tree: nothing to do")
    (:hot (format nil "hot: ~d definition~:p across ~d file~:p~[~:;, ~:*~d dependent~:p~]~[~:;, ~:*~d under the floor~]"
                  count (length (getf plan :hot))
                  (length (getf plan :dependents)) (length (getf plan :floor))))
    (:restart (format nil "restart: ~a — ~a~[~:; +~:*~d more~]; ~d definition~:p would be hot"
                      (car (first held)) (cdr (first held)) (1- (length held))
                      count))))

;;; --- the transaction --------------------------------------------------------
;;;
;;; CONVERGE is the hot path itself: every hot definition of one or more
;;; plans evaluated from the staged tree, in load order, under a lowered
;;; floor. All or nothing -- a definition that will not evaluate puts every
;;; one before it back from an undo log taken first, and the image is as it
;;; was. What it does not do is the organism's: staging, the facts, the
;;; frames to shells, the link, the restart (update.lisp).

(defun snapshot-definitions (symbols)
  "The undo log for SYMBOLS, ((PACKAGE-NAME . NAME) ...): each name's
function, macro function, value and -- for a generic function -- its
methods, as they stand now."
  ;; A name this image lacks yet is logged unbound, so an upgrade that fails
  ;; leaves it unbound again.
  (loop for (package-name . name) in symbols
        for package = (find-package package-name)
        for symbol = (and package (definition-name-symbol name package))
        ;; a symbol, or (SETF symbol) for the name "(SETF NAME)"
        for designator = (and symbol (if (uiop:string-prefix-p "(SETF " name)
                                         (list 'setf symbol)
                                         symbol))
        when designator
          collect (let ((function (and (fboundp designator) (fdefinition designator))))
                    (list designator
                          :function function
                          :macro (and (symbolp designator) (macro-function designator))
                          :value (if (and (symbolp designator) (boundp designator))
                                     (symbol-value designator)
                                     '%unbound)
                          :methods (and (typep function 'generic-function)
                                        (copy-list (sb-mop:generic-function-methods function)))))))

(defun restore-definitions (log)
  "Put every name in LOG (SNAPSHOT-DEFINITIONS) back as it was: the function
or macro, the method set of a generic, the value."
  ;; Never signals: one name that will not restore must not stop the rest.
  (dolist (entry log)
    (destructuring-bind (designator &key function macro value methods) entry
      (ignore-errors
       (let ((symbol (if (consp designator) (second designator) designator)))
         (cond (macro (setf (macro-function symbol) macro))
               (function
                (setf (fdefinition designator) function)
                (let ((generic (fdefinition designator)))
                  (when (typep generic 'generic-function)
                    (dolist (method (copy-list (sb-mop:generic-function-methods generic)))
                      (unless (member method methods) (remove-method generic method)))
                    (dolist (method methods)
                      (unless (member method (sb-mop:generic-function-methods generic))
                        (add-method generic method))))))
               ((fboundp designator) (fmakunbound designator)))
         (unless (or (eq value '%unbound) (consp designator))
           (setf (symbol-value symbol) value)))))))

(defun converge (plans &key version source-root allow-held check &aux (count 0) (files 0))
  "Make this image agree with the staged trees along PLANS, ((PLAN . ROOT)
...) from CLASSIFY-UPGRADE: each hot definition evaluated from ROOT's file
through LOAD-SOURCE :ONLY, in load order, the kernel floor lowered for it
and raised again after; then the image's self-view moved to SOURCE-ROOT
(the core plan's root by default) and its effective version to VERSION.
A plan whose verdict is :RESTART is accepted under ALLOW-HELD, which is
what an upgrade of a live image means: what the plan holds is boot state
and stays on disk for the new build's own boot, and every definition it
does not hold goes live here. CHECK, a function of no arguments, is asked
once every definition is live: a reason it answers puts the image back."
  ;; All or nothing: a definition that will not evaluate puts every one before
  ;; it back from the undo log and signals UPGRADE-REFUSAL naming it, the
  ;; image as it was; so does a reason CHECK answers -- the image evaluated
  ;; the release and stopped doing what it did before it, which only the
  ;; caller can see. => (values COUNT FILES) evaluated.
  (loop for (plan) in plans
        unless (member (getf plan :verdict) (if allow-held '(:hot :same :restart) '(:hot :same)))
          do (upgrade-fail "~a" (upgrade-plan-line plan)))
  (let* ((log (snapshot-definitions (loop for (plan . nil) in plans
                                          append (getf plan :symbols))))
         (locked *floor-locked*)
         (*source-loaded-names* '()))
    (when locked (unlock-floor))
    (with-cleanup ((when locked (lock-floor)))
      ;; The compiler reports a unit a failing definition aborts on the
      ;; standard streams. The refusal names the failure; the chatter stays
      ;; here.
      (with-handlers ((error (condition)
                        (restore-definitions log)
                        (if (typep condition 'upgrade-refusal)
                            (error condition)
                            (upgrade-fail "~a" condition))))
       (let ((*error-output* (make-broadcast-stream))
             (*standard-output* (make-broadcast-stream)))
        (loop for (plan . root) in plans
              for root-truename = (truename root)
              do (let ((*staged-source-root* root-truename))
                   (loop for (file . pairs) in (getf plan :hot)
                         do (bind (((_ live held)
                                    (load-source (merge-pathnames file root-truename) :only pairs)))
                              (when held
                                (upgrade-fail "~a: ~a — ~a" file
                                              (car (first held)) (cdr (first held))))
                              (when (< (length live) (length pairs))
                                (upgrade-fail "~a: ~d of ~d definition~:p did not evaluate"
                                              file (- (length pairs) (length live))
                                              (length pairs)))
                              (incf count (length live))
                              (incf files)))))
        (when-let (reason (and check (funcall check)))
          (upgrade-fail "~a, so the release is undone" reason)))))
    (when-let (root (or source-root (cdr (first plans))))
      (setf *source-root* (truename root))
      (forget-shipped-definitions))
    (when version (setf *effective-version* version))
    (values count files)))

(defun record-upgrade (session-id &key from to mode line held)
  "Append the organism fact for an upgrade: FROM and TO release labels,
MODE (\"hot\", \"restart\" or \"rollback\"), the plan LINE and, for a
restart, the HELD reasons. => the event id, or NIL with no open store."
  ;; The gateway's boot session stands by until its first block: an
  ;; organism fact needs it known, as the floor's engagement does.
  (record-generation-fact +kind-organism-upgraded+ "organism.upgrade"
                          (list :from from :to to :mode mode :line (or line "")
                                :held (map 'vector
                                           (lambda (entry) (format nil "~a — ~a" (car entry) (cdr entry)))
                                           held))
                          'record-upgrade :session-id session-id))
