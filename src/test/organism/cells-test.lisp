;;;; cells-test.lisp --- the one folder: load, report, start.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test builds a scratch folder tree, points *CELLS-DIRECTORY* at it,
;;;; and asserts the report — overrides, collisions, failures, kinds — rather
;;;; than the loader's internals. ASDF's registry and immutable set are
;;;; rebound around each load so a scratch system cannot outlive its test,
;;;; and every scratch system carries a per-process name so ASDF's own system
;;;; cache never confuses two tests' folders.

(in-package #:nodecode.test)

(nlk:access (needy nlk:offer) (record nlk::cell) (top nlk:offer))

(defun write-cell-folder (root name files &key layer depends-on description)
  "Create ROOT/NAME/NAME.asd declaring FILES — ((basename . source-text) ...)
in order — and each file beside it."
  ;; LAYER writes the layer system class; DEPENDS-ON names sibling folders;
  ;; DESCRIPTION the system's :description.
  (let ((directory (merge-pathnames (format nil "~a/" name) root)))
    (write-temp-file
     (merge-pathnames (format nil "~a.asd" name) directory)
     (format nil "(defsystem ~s~@[ :class ~a~]~@[ :description ~s~]~@[ :depends-on ~s~] :serial t :components (~{(:file ~s)~^ ~}))"
             name
             (and layer "nodecode.kernel:layer")
             description
             depends-on
             (mapcar #'car files)))
    (dolist (file files)
      (write-temp-file (merge-pathnames (format nil "~a.lisp" (car file)) directory)
                       (cdr file)))
    directory))

(defun write-layer-folder (root name files &rest options)
  "WRITE-CELL-FOLDER of a layer under ROOT whose FILES — ((basename .
form-text) ...) — each open in EVOLVED; OPTIONS reach the folder writer."
  (apply #'write-cell-folder root name
         (mapcar (lambda (file) (cons (car file) (evolved-source (cdr file)))) files)
         :layer t options))

(defun load-cell-fixture (root &aux (asdf:*central-registry* asdf:*central-registry*)
                                     (asdf::*immutable-systems* asdf::*immutable-systems*))
  "LOAD-CELLS over ROOT with ASDF state fenced to this call."
  (nlk:load-cells :root root))

(defmacro with-cell-fixture ((root) &body body)
  `(with-temp-directory (,root "cells")
     ;; The sections a loaded cell declares go with its record.
     (with-saved-globals (nlk::*cells* nlk::*sections*)
       ,@body)))

(defun key-labels (keys)
  "Definition keys as (KIND . NAME-STRING): the evolve tests rebuild the
EVOLVED package mid-suite, so a literal EVOLVED symbol read with this file
is not EQ to the one a later eval interns — names compare, symbols do not."
  (mapcar (lambda (key) (cons (car key) (symbol-name (cdr key)))) keys))

(defun evolved-fn (name &rest args)
  "Call the EVOLVED function NAME (a string) with ARGS."
  (apply (uiop:find-symbol* name '#:nodecode.evolved) args))

(defun board-line (key)
  "The text standing on the notice board under KEY, or NIL."
  (second (assoc key (nlk:standing-notices) :test #'string=)))

(deftest cells-missing-folder-is-an-empty-report (let ((root (temp-path "cells-missing"))))
  (is (null (load-cell-fixture root)) "no folder, no cells")
  (is (null (nlk:cells)) "the report is empty too"))

(deftest cells-layer-loads-from-source-and-reports-override ()
  ;; A shipped name redefined by a folder is an OVERRIDE; a fresh name is
  ;; DEFINED; both are live in the image after the load.
  (eval (read-from-string
         "(defun nodecode.evolved::cells-probe-shipped () :shipped)"))
  (with-cell-fixture (root)
    (write-layer-folder root "nl-layer-a"
                        '(("cells-probe-shipped" . "(defun cells-probe-shipped () :layered)")
                          ("cells-probe-fresh" . "(defun cells-probe-fresh () :fresh)")))
    (let* ((records (load-cell-fixture root))
           (record (first records)))
      (is (= 1 (length records)) "one folder, one record")
      (is-present record "the layer record"
        (is (string= "nl-layer-a" (nlk:cell-name record)))
        (is (eq :loaded (nlk:cell-state record)) "the layer loaded")
        (is (eq :layer (nlk:cell-kind record)) "the LAYER class marks the kind")
        (is (equal '((:function . "CELLS-PROBE-SHIPPED"))
                   (key-labels (nlk:cell-overrides record))))
        (is (equal '((:function . "CELLS-PROBE-FRESH"))
                   (key-labels (nlk:cell-defined record))))
        (is (null (nlk:cell-collisions record)) "no collision with one folder"))
      (is (eq :layered (evolved-fn "CELLS-PROBE-SHIPPED")) "the override is live")
      (is (eq :fresh (evolved-fn "CELLS-PROBE-FRESH")) "the definition is live")
      (is (search "overrides cells-probe-shipped" (nlk:cells-folder-text))))))

(deftest cells-two-folders-collide-in-alphabetical-order (with-cell-fixture (root))
  (write-layer-folder root "nl-collide-b"
                      '(("shared" . "(defun cells-probe-collide () :from-b)")))
  (write-layer-folder root "nl-collide-a"
                      '(("shared" . "(defun cells-probe-collide () :from-a)")))
  (let ((records (load-cell-fixture root)))
    (is (equal '("nl-collide-a" "nl-collide-b")
               (mapcar #'nlk:cell-name records)))
    (is (null (nlk:cell-collisions (first records))))
    (is (equal '(((:function . "CELLS-PROBE-COLLIDE") . "nl-collide-a"))
               (let ((collisions (nlk:cell-collisions (second records))))
                 (mapcar #'cons (key-labels (mapcar #'car collisions)) (mapcar #'cdr collisions)))))
    (is (eq :from-b (evolved-fn "CELLS-PROBE-COLLIDE")))))

(deftest cells-failed-folder-isolates-its-siblings (with-cell-fixture (root))
  (write-layer-folder root "nl-broken"
                      '(("boom" . "(error \"this folder cannot load\")")))
  (write-layer-folder root "nl-fine"
                      '(("fine" . "(defun cells-probe-fine () :fine)")))
  (is (= 2 (length (load-cell-fixture root))) "both folders have records")
  (let ((broken (nlk:find-cell "nl-broken"))
        (fine (nlk:find-cell "nl-fine")))
    (is-present broken "the broken record"
      (is (eq :failed (nlk:cell-state broken)))
      (is (equal '("boom.lisp") (mapcar #'car (nlk:cell-failures broken))))
      (is (search "cannot load" (cdr (first (nlk:cell-failures broken))))))
    (is-present fine "the sibling record"
      (is (eq :loaded (nlk:cell-state fine)) "the sibling still loaded"))
    (is (eq :fine (evolved-fn "CELLS-PROBE-FINE")))
    (is-carrying (lines (nlk:cells-folder-text))
      ("cell nl-broken: FAILED boom.lisp: this folder cannot load"
       (format nil "the boot line names file and condition: ~a" lines))
      "cell nl-broken: layer, 0 definitions")))

(deftest cells-a-failure-reads-the-same-at-every-boot (with-cell-fixture (root))
  ;; The folder's report rides the prompt prefix. A symbol read from a
  ;; package that is not there is a reader error naming its stream and heap
  ;; address. Without this, a layer file that names a missing package would
  ;; give the prompt prefix new bytes at every boot, and every conversation
  ;; behind a relaunch would be re-read in full.
  (write-layer-folder root "nl-misread"
                      '(("gone" . "(no-such-package-anywhere:frob)")))
  (load-cell-fixture root)
  (is-present (text (cdr (first (nlk:cell-failures (nlk:find-cell "nl-misread"))))) "the failure is recorded"
    (is (search "gone.lisp" text) (format nil "the stream is still named: ~a" text))
    (is (null (ppcre:scan "\\{[0-9A-F]{6,}\\}" text)) (format nil "no heap address: ~a" text))))

(deftest cells-a-folder-older-than-the-rename-is-told-what-to-do (with-cell-fixture (root))
  ;; `Component "nodecode-lisp" not found' names the missing thing and stops.
  ;; An operator arriving from a build before the rename had eight shipped
  ;; folders and their own 25-definition layer go dark behind that line, and
  ;; read it as the install being broken; it was not, the folders were older
  ;; than the install. The loader knows which folder is shipped and which is
  ;; the operator's own, and the two want opposite remedies, so it says which
  ;; — a folder that fails for any other reason is left to speak for itself.
  ;; Not a layer: a layer's files load directly, so its .asd dependency is
  ;; never resolved and never fails. The folders that went dark were plain
  ;; systems — nodecode-channel-kit and the seven that stand on it.
  (write-cell-folder
   root "my-cell"
   (list (cons "kept" (evolved-source "(defun cells-probe-kept () :kept)")))
   :depends-on '("nodecode-lisp"))
  (write-layer-folder root "nl-other"
                      '(("boom" . "(error \"this folder cannot load\")")))
  (load-cell-fixture root)
  (let ((lines (nlk:cells-folder-text)))
    (is-carrying lines ("cell my-cell: FAILED" "the folder still fails")
      ("predates the rename" (format nil "and the line says why: ~a" lines)) "sed -i")
    (is-present (other (search "cell nl-other: FAILED" lines))
      "the unrelated failure is still reported"
      (is (not (search "predates the rename"
                       (subseq lines other (min (length lines) (+ other 200)))))))))

(deftest cells-failed-layer-file-isolates-its-siblings (with-cell-fixture (root))
  (write-layer-folder
   root "nl-partial"
   '(("first" . "(defun cells-probe-partial-first () :first)")
     ("boom" . "(defparameter *cells-probe-partial-boom* (error \"the channel is not running\"))")
     ("last" . "(defun cells-probe-partial-last () :last)")))
  (load-cell-fixture root)
  (is-present (record (nlk:find-cell "nl-partial")) "the layer's record"
    (is (eq :failed (nlk:cell-state record)) "one failed file fails the record")
    (is (eq :layer (nlk:cell-kind record)) "and it is still known as a layer")
    (is (equal '("boom.lisp") (mapcar #'car (nlk:cell-failures record))))
    (is-carrying (text (cdr (first (nlk:cell-failures record))))
      (is (uiop:string-prefix-p "the channel is not running" text)
          (format nil "the condition leads the text: ~s" text))
      (:absent "compilation unit" (format nil "SBCL's unit summary is dropped: ~s" text))
      (:absent "caught 1 fatal" (format nil "and so is its count: ~s" text)))
    (is (equal '((:function . "CELLS-PROBE-PARTIAL-FIRST")
                 (:function . "CELLS-PROBE-PARTIAL-LAST"))
               (key-labels (nlk:cell-defined record)))))
  (is (eq :last (evolved-fn "CELLS-PROBE-PARTIAL-LAST")))
  (is-carrying (lines (nlk:cells-folder-text))
    ("cell nl-partial: layer, 2 definitions"
     (format nil "the summary counts what loaded: ~a" lines))
    ("cell nl-partial: FAILED boom.lisp: the channel is not running"
     (format nil "the FAILED line names the file: ~a" lines))))

(deftest cells-layer-entry-without-a-file-loads-nothing-and-fails-nothing (with-cell-fixture (root))
  ;; A file deleted by hand before the scribe's next write pruned the .asd:
  ;; the manifest is load order, the folder is what there is.
  (delete-file (merge-pathnames "gone.lisp"
                                (write-layer-folder
                                 root "nl-stale"
                                 '(("kept" . "(defun cells-probe-stale-kept () :kept)")
                                   ("gone" . "(defun cells-probe-stale-gone () :gone)")))))
  (load-cell-fixture root)
  (is-present (record (nlk:find-cell "nl-stale")) "the layer's record"
    (is (eq :loaded (nlk:cell-state record))
        (format nil "a missing file is not a failure: ~a" (nlk:cell-failures record)))
    (is (equal '((:function . "CELLS-PROBE-STALE-KEPT"))
               (key-labels (nlk:cell-defined record)))))
  (is (eq :kept (evolved-fn "CELLS-PROBE-STALE-KEPT")))
  (let ((lines (nlk:cells-folder-text)))
    (is (null (search "FAILED" lines)) (format nil "no FAILED line: ~a" lines))))

(deftest cells-peripheral-is-marked-by-start-cell (with-cell-fixture (root))
  (write-peripheral-folder
   root "nl-peri" :exports '("*log*") :forms '("(defvar *log* '())")
   :start "(push (list :started config) *log*) (lambda () (push :stopped *log*))")
  (let ((record (first (load-cell-fixture root))))
    (is-present record "the peripheral record"
      (is (eq :peripheral (nlk:cell-kind record)) "START-CELL marks a peripheral")
      (is (functionp (nlk:cell-start record)) "the start function is kept")
      (is (null (nlk:cell-started-p record)) "loading does not start"))
    ;; The gateway's lifecycle over the same records.
    (nle::start-cells)
    (is (nlk:cell-started-p record) "START-CELLS started it")
    (is (functionp record.stop) "and kept its stop thunk")
    (nle::stop-cells)
    (is (null record.started-p) "STOP-CELLS cleared it")
    (is (equal '(:stopped (:started nil)) (symbol-value (uiop:find-symbol* '#:*log* '#:nl-peri))))))

(deftest cells-start-cues-the-shells-slash-catalog (with-cell-fixture (root))
  ;; The boot starts the peripherals AFTER the listener, on their own thread,
  ;; and every shell reads the gateway's cell commands at attach: without
  ;; this cue a boot's commands (/skills, /cron, /web, ...) are missing from
  ;; the composer with no notice and no retry. The lifecycle's public verbs
  ;; publish it -- one unsequenced COMMANDS_CHANGED frame.
  (write-peripheral-folder root "nl-cue")
  (load-cell-fixture root)
  (let ((kinds '()))
    (nle:hook :frame "cells-cue-test"
              (lambda (op next)
                (push (getf op :kind) kinds)
                (funcall next op)))
    (nlk:with-cleanup ((nle:unhook :frame "cells-cue-test"))
      (nle::start-cells)
      (is (member "commands_changed" kinds :test #'equal))
      ;; and the lanes they bring: a shell attached before them read a pin a
      ;; cell serves as refused, and kept it (the operator, 2026-10-22)
      (is (member "model_changed" kinds :test #'equal) "every shell reads its model facts again")
      (setf kinds '())
      (nle::stop-cells)
      (is (member "commands_changed" kinds :test #'equal)))))

(deftest cells-restart-starts-a-refused-peripheral (with-cell-fixture (root))
  ;; The onboarding shape: a peripheral refuses its config at boot, the
  ;; refusal stands on the record and the board, the operator fixes the
  ;; config, and (restart-cells) starts it without a relaunch.
  (write-peripheral-folder
   root "nl-flaky" :exports '("*ready*") :forms '("(defvar *ready* nil)")
   :start "(declare (ignore config))
  (unless *ready* (error \"nl-flaky refuses: not ready\")) (lambda () nil)")
  (let* ((record (first (load-cell-fixture root)))
         (ready (uiop:find-symbol* '#:*ready* '#:nl-flaky)))
    (nle::start-cells)
    (is (null record.started-p) "the refused start is not started")
    (is (search "not ready" record.refusal))
    (is (search "nl-flaky not started: nl-flaky refuses: not ready" (board-line "nl-flaky")))
    (is-carrying (lines (nlk:cells-folder-text))
      ("cell nl-flaky: peripheral, 0 definitions, REFUSED" "DESCRIBE-CELLS says REFUSED"))
    (is (search "REFUSED" (nlk:cells-report-text)) "the report text carries it")
    (is (search "not ready" (nlk:cells-report-text)) "with the board line beneath")
    (setf (symbol-value ready) t)
    (let ((answer (nle:restart-cells "nl-flaky")))
      (is (nlk:cell-started-p record) "RESTART-CELLS started it")
      (is (null record.refusal) "and cleared the refusal")
      (is (null (board-line "nl-flaky")))
      (is (search "cell nl-flaky: peripheral, 0 definitions, started" answer)))
    (is (search "no cell folder" (refusal-text error (nle:restart-cells "no-such-folder"))))
    (nle::stop-cells)
    (is (null record.started-p))))

(deftest cells-kernel-name-is-refused-by-the-lock (with-floor-restored)
  (nlk:lock-floor)
  (with-cell-fixture (root)
    (write-layer-folder root "nl-floor"
                        '(("floor" . "(defun nodecode.kernel:open-store (&key path) path)")))
    (is-present (record (first (load-cell-fixture root))) "the record"
      (is (eq :failed (nlk:cell-state record)))
      (is (search "lock" (string-downcase
                            (cdr (first (nlk:cell-failures record)))))))))

(deftest cells-added-methods-are-reported ()
  (eval (read-from-string
         "(defgeneric nodecode.evolved::cells-probe-generic (x))"))
  (with-cell-fixture (root)
    (write-layer-folder root "nl-methods"
                        '(("method" . "(defmethod cells-probe-generic ((x integer)) (* 2 x))")))
    (is-present (record (first (load-cell-fixture root))) "the record"
      (is (equal '((:methods . "CELLS-PROBE-GENERIC"))
                 (key-labels (nlk:cell-methods-added record))))
      (is (null (nlk:cell-overrides record))))))

(deftest cells-folder-depends-on-sibling-resolves (with-cell-fixture (root))
  ;; A folder naming a sibling in :depends-on loads after it regardless of
  ;; the alphabet: both are registered before either loads.
  (write-layer-folder root "nl-dep-a"
                      '(("a" . "(defun cells-probe-dep-a () (cells-probe-dep-z))"))
                      :depends-on '("nl-dep-z"))
  (write-layer-folder root "nl-dep-z" '(("z" . "(defun cells-probe-dep-z () :z)")))
  (is (every (lambda (record) (eq :loaded record.state)) (load-cell-fixture root)))
  (is (eq :z (evolved-fn "CELLS-PROBE-DEP-A")) "the dependency resolved"))

;;; --- startup-resolved paths ----------------------------------

(deftest cells-startup-parameters-resolve-from-the-live-home ()
  ;; The HOME= landmine: a release binary must read the home it was started
  ;; under, never the builder's. RESOLVE-STARTUP-PARAMETERS is what the saved
  ;; core runs first; here it runs against a swapped $HOME, and then up the
  ;; profile ladder above it (paths.lisp LOCATE-HOME), one rung at a time.
  (let ((real-home (sb-posix:getenv "HOME"))
        (real-nodecode-home (sb-posix:getenv "NODECODE_HOME"))
        (scratch (namestring (temp-path "home"))))
    (nlk:with-cleanup ((sb-posix:setenv "HOME" real-home 1)
                       (if real-nodecode-home
                           (sb-posix:setenv "NODECODE_HOME" real-nodecode-home 1)
                           (sb-posix:unsetenv "NODECODE_HOME"))
                       (ignore-errors (uiop:delete-directory-tree (pathname scratch) :validate t))
                       (nlk:resolve-startup-parameters))
      (with-saved-globals ((nlk:*profile-flag* nil))
        (sb-posix:unsetenv "NODECODE_HOME")
        (sb-posix:setenv "HOME" (string-right-trim "/" scratch) 1)
        (nlk:resolve-startup-parameters)
        (is (search scratch (namestring nlk:*home*)) "the home follows $HOME")
        (is (equal "default" (nlk:profile-name)))
        (is-table (path) (search scratch (namestring path))
          (nlk::*cells-directory* "the folder follows $HOME")
          (nlk::*default-store-path* "the store follows $HOME")
          (nle::*shared-config-path* "the shared config follows $HOME"))
        (is (search "/.nodecode/composer-history" (namestring nle::*composer-history-path*)))
        ;; $NODECODE_HOME outranks the default home: a custom folder is a
        ;; profile named after itself.
        (sb-posix:setenv "NODECODE_HOME"
                         (concatenate 'string (string-right-trim "/" scratch) "/elsewhere") 1)
        (nlk:resolve-startup-parameters)
        (is (search "/elsewhere/" (namestring nlk:*home*)) "$NODECODE_HOME is the home")
        (is (search "/elsewhere/history.db" (namestring nlk::*default-store-path*)))
        (is (equal "elsewhere" (nlk:profile-name)) "a custom home is named after its folder")
        ;; The command line outranks the environment.
        (setf nlk:*profile-flag* "coder")
        (nlk:resolve-startup-parameters)
        (is (search "/.nodecode/profiles/coder/" (namestring nlk:*home*)))
        (is (equal "coder" (nlk:profile-name)) "the profile is named")
        (is (search "/.nodecode/profiles/coder/cells/" (namestring nlk::*cells-directory*)))
        (setf nlk:*profile-flag* nil)
        (sb-posix:unsetenv "NODECODE_HOME")
        ;; The sticky file is the last rung before the default, and only
        ;; while the folder it names is there.
        (write-temp-file nlk:*active-profile-path* (format nil "writer~%"))
        (nlk:resolve-startup-parameters)
        (is (equal "default" (nlk:profile-name)))
        (ensure-directories-exist (nlk:profile-home "writer"))
        (nlk:resolve-startup-parameters)
        (is (search "/.nodecode/profiles/writer/" (namestring nlk:*home*)))
        (is (equal "writer" (nlk:sticky-profile)) "the sticky name reads back")))
    ;; Restored, the store is back where the run started: under
    ;; $NODECODE_HOME when the suite was given one, else under $HOME.
    (is (search (or real-nodecode-home real-home) (namestring nlk::*default-store-path*)))
    (is (member 'nlk:resolve-startup-parameters sb-ext:*init-hooks*))))

(deftest cells-source-root-is-the-shipped-tree ()
  (is-present (root nlk:*source-root*) "the source root resolves in the developer image"
    (is (probe-file (merge-pathnames "engine/turn.lisp" root)))))

(deftest cells-layers-load-after-peripherals (with-cell-fixture (root))
  ;; The layer sorts first by name; the peripheral it advises sorts last.
  (write-layer-folder
   root "nl-aaa-layer"
   '(("greet-hook" . "(define-hook nl-zzz-peri:greet \"layer\" (next who) (list :advised (funcall next who)))")))
  (write-cell-folder
   root "nl-zzz-peri"
   '(("cell" . "(defpackage #:nl-zzz-peri (:use #:cl) (:export #:greet))
(in-package #:nl-zzz-peri)
(defun greet (who) (list :hello who))")))
  (let ((records (load-cell-fixture root)))
    (is (equal '("nl-zzz-peri" "nl-aaa-layer")
               (mapcar #'nlk:cell-name records)))
    (is (every (lambda (record) (eq :loaded record.state))
               records))
    (is (equal '(:advised (:hello :x))
               (funcall (uiop:find-symbol* '#:greet '#:nl-zzz-peri) :x)))
    (nlk:unhook (uiop:find-symbol* '#:greet '#:nl-zzz-peri) "layer")))

;;; --- the layer cache: compiled once per image, keyed on the text ----------

(defmacro with-layer-cache ((cache stamp) &body body)
  "Run BODY with CACHE a scratch layer cache, bound as the image's, under image STAMP."
  `(with-temp-directory (,cache "layer-cache")
     (let ((nlk::*layer-cache-directory* ,cache) (nlk::*image-stamp* ,stamp))
       ,@body)))

(defun layer-cache-fasls (cache stamp folder)
  "The fasls CACHE holds for FOLDER under STAMP, by name."
  (sort (mapcar #'file-namestring
                (directory (merge-pathnames (format nil "~a/~a/*.fasl" stamp folder)
                                            cache)))
        #'string<))

(deftest cells-layer-compiles-once-into-the-image-cache (with-cell-fixture (root))
  ;; The first load compiles each layer file into <cache>/<stamp>/<folder>/,
  ;; the next load finds the fasl by the file's text, a rewritten file is a
  ;; new fasl with the old one pruned, and a new image stamp starts empty
  ;; with the stamp before it kept (two builds share one cache).
  (with-layer-cache (cache "stamp-a")
    (write-layer-folder root "nl-cached" '(("probe" . "(defun cells-probe-cached () :one)")))
    (load-cell-fixture root)
    (is (eq :one (evolved-fn "CELLS-PROBE-CACHED")) "the definition is live")
    (let ((first (layer-cache-fasls cache "stamp-a" "nl-cached")))
      (is (= 1 (length first))
          (format nil "one fasl for the one file: ~s" first))
      (is (and first (uiop:string-prefix-p "probe-" (first first))))
      (load-cell-fixture root)
      (is (equal first (layer-cache-fasls cache "stamp-a" "nl-cached")))
      (is (eq :one (evolved-fn "CELLS-PROBE-CACHED")) "and the same definition")
      (write-temp-file (merge-pathnames "nl-cached/probe.lisp" root)
                       (evolved-source "(defun cells-probe-cached () :two)"))
      (load-cell-fixture root)
      (is (eq :two (evolved-fn "CELLS-PROBE-CACHED")))
      (let ((second (layer-cache-fasls cache "stamp-a" "nl-cached")))
        (is (= 1 (length second))
            (format nil "the earlier compilation is pruned: ~s" second))
        (is (not (equal first second)) "and the fasl is a new one")))
    (let ((nlk::*image-stamp* "stamp-b"))
      (load-cell-fixture root)
      (is (eq :two (evolved-fn "CELLS-PROBE-CACHED")) "a new image loads the same definition")
      (is (= 1 (length (layer-cache-fasls cache "stamp-b" "nl-cached"))))
      (is (probe-file (merge-pathnames "stamp-a/" cache)) "the stamp beside it stays"))))

(deftest cells-layer-cache-keeps-a-failing-file-s-diagnostics (with-cell-fixture (root))
  ;; A file the compiler refuses compiles nothing into the cache and loads
  ;; from source, where the same error is recorded by its name; a file that
  ;; compiles but fails to load is cached and fails from its fasl with the
  ;; load's own condition; the file around them loads, from the cache.
  (with-layer-cache (cache "stamp-f")
    (write-layer-folder
     root "nl-cached-fail"
     '(("fine" . "(defun cells-probe-cached-fine () :fine)")
       ("unreadable" . "(defun cells-probe-cached-unreadable ( :never")
       ("unloadable" . "(defparameter *cells-probe-cached-unloadable* (error \"no such thing at load\"))")))
    (is-present (record (first (load-cell-fixture root))) "the layer's record"
      (is (eq :failed (nlk:cell-state record)) "the failing files fail the record")
      (is (equal '("unreadable.lisp" "unloadable.lisp")
                 (mapcar #'car (nlk:cell-failures record))))
      (is (uiop:string-prefix-p "no such thing at load"
                                (cdr (second (nlk:cell-failures record)))))
      (is (search "end of file" (string-downcase
                                 (cdr (first (nlk:cell-failures record)))))
          (format nil "the unreadable file carries the reader's: ~s"
                  (cdr (first (nlk:cell-failures record))))))
    (is (eq :fine (evolved-fn "CELLS-PROBE-CACHED-FINE")) "the sibling loaded")
    (let ((fasls (layer-cache-fasls cache "stamp-f" "nl-cached-fail")))
      (is (equal '("fine" "unloadable")
                 (mapcar (lambda (name) (subseq name 0 (position #\- name)))
                         fasls))
          (format nil "what compiled is cached, what did not read is not: ~s"
                  fasls)))))

;;; --- skipped: a layer file that would not run against this image ------------

(deftest cells-layer-skips-a-file-naming-what-nothing-defines (with-cell-fixture (root))
  ;; What a file's compile found missing is judged against the image and
  ;; the layer's later files: a caller recorded ahead of its helper, its
  ;; variable and its struct loads; a name nothing defines is skipped,
  ;; and so is a file naming a skipped file's definition. Skipped is no
  ;; failure: the file does not reach the image.
  (write-layer-folder
   root "nl-skip"
   '(("caller" . "(defun cells-probe-skip-caller ()
  (list (cells-probe-skip-helper) *cells-probe-skip-var*
        (cells-probe-skip-rec-a (make-cells-probe-skip-rec :a 1))))")
     ("gone" . "(defun cells-probe-skip-gone () (cells-probe-skip-inlined-away 1))")
     ("chained" . "(defun cells-probe-skip-chained () (cells-probe-skip-gone))")
     ("late-bound" . "(defun cells-probe-skip-late () (funcall 'cells-probe-skip-optional))")
     ("helper" . "(defun cells-probe-skip-helper () :helper)")
     ("var" . "(defvar *cells-probe-skip-var* :var)")
     ("rec" . "(defstruct cells-probe-skip-rec a)")))
  (load-cell-fixture root)
  (is-present (record (nlk:find-cell "nl-skip")) "the layer's record"
    (is (eq :loaded (nlk:cell-state record))
        (format nil "a skipped file is no failure: ~a" (nlk:cell-failures record)))
    (is (equal '("gone.lisp" "chained.lisp")
               (mapcar #'car (nlk::cell-skipped record)))
        (format nil "exactly the files naming what nothing defines: ~s"
                (nlk::cell-skipped record))))
  (is (equal '(:helper :var 1) (evolved-fn "CELLS-PROBE-SKIP-CALLER")))
  (is (fboundp (nlk::evolved-symbol "CELLS-PROBE-SKIP-LATE")))
  (is (not (fboundp (nlk::evolved-symbol "CELLS-PROBE-SKIP-GONE"))))
  (is-carrying (lines (nlk:cells-folder-text))
    "cell nl-skip: SKIPPED gone.lisp: names cells-probe-skip-inlined-away, which this image does not define"
    "cell nl-skip: SKIPPED chained.lisp: names cells-probe-skip-gone, which this image does not define"
    (:absent "FAILED" "and no FAILED line")))

(deftest cells-layer-file-waits-for-what-a-later-file-defines (with-cell-fixture (root))
  ;; Without the wait, a file that names a later file would load ahead
  ;; of it and read an unbound variable or call an undefined function. A
  ;; file naming what a later one may define waits for it: it loads behind
  ;; it once it has (an initform calling a later helper runs), it is
  ;; skipped with what it names when it never does, and so is the file
  ;; naming the waiting file's own. Two that name each other load.
  (write-layer-folder
   root "nl-wait"
   '(("unreadable" . "(defun cells-probe-wait-manifest () (nodecode.kernel:no-such-export))")
     ("reader" . "(defun cells-probe-wait-reader () *cells-probe-wait-manifest*)")
     ("tick" . "(defun cells-probe-wait-tick () (cells-probe-wait-reader))")
     ("manifest" . "(defparameter *cells-probe-wait-manifest* (cells-probe-wait-manifest))")
     ("early" . "(defparameter *cells-probe-wait-early* (cells-probe-wait-helper))")
     ("ping" . "(defun cells-probe-wait-ping (n) (if (plusp n) (cells-probe-wait-pong (1- n)) :ping))")
     ("pong" . "(defun cells-probe-wait-pong (n) (if (plusp n) (cells-probe-wait-ping (1- n)) :pong))")
     ("helper" . "(defun cells-probe-wait-helper () :helper)")))
  (load-cell-fixture root)
  (flet ((evolved (name) (uiop:find-symbol* name '#:nodecode.evolved nil)))
    (is-present (record (nlk:find-cell "nl-wait")) "the layer's record"
      (is (equal '("unreadable.lisp") (mapcar #'car (nlk:cell-failures record)))
          (format nil "the one file that does not read fails: ~a" (nlk:cell-failures record)))
      (is (equal '("manifest.lisp" "reader.lisp" "tick.lisp")
                 (mapcar #'car (nlk::cell-skipped record)))
          (format nil "the variable's file, then what waited on it: ~s"
                  (nlk::cell-skipped record))))
    ;; the reader of a variable nothing bound never reached the image, nor did its caller
    (is (not (fboundp (evolved "CELLS-PROBE-WAIT-READER"))))
    (is (not (fboundp (evolved "CELLS-PROBE-WAIT-TICK"))))
    (is (not (nlk:when-let (variable (evolved "*CELLS-PROBE-WAIT-MANIFEST*")) (boundp variable))))
    ;; an initform ahead of its helper ran behind it
    (is (eq :helper (symbol-value (evolved "*CELLS-PROBE-WAIT-EARLY*"))))
    (is (eq :pong (evolved-fn "CELLS-PROBE-WAIT-PING" 3)) "two that name each other load"))
  (is-carrying (lines (nlk:cells-folder-text))
    "cell nl-wait: SKIPPED manifest.lisp: names cells-probe-wait-manifest, which this image does not define"
    "cell nl-wait: SKIPPED reader.lisp: names *cells-probe-wait-manifest*, which this image does not define"
    "cell nl-wait: SKIPPED tick.lisp: names cells-probe-wait-reader, which this image does not define"))

(deftest cells-layer-cache-remembers-what-a-compile-found-missing (with-cell-fixture (root))
  ;; A launch that finds the fasl reads the missing names back beside it
  ;; instead of compiling, and judges them against the image as it stands:
  ;; still skipped while nothing defines the helper, loaded from the same
  ;; fasl once a file that defines it joins the layer.
  (with-layer-cache (cache "stamp-s")
    (let ((user '("user" . "(defun cells-probe-skipc-user () (cells-probe-skipc-helper))")))
      (write-layer-folder root "nl-skip-cached" (list user))
      (is (equal '("user.lisp")
                 (mapcar #'car (nlk::cell-skipped (first (load-cell-fixture root))))))
      (let ((fasls (layer-cache-fasls cache "stamp-s" "nl-skip-cached"))
            (compiles 0))
        (is (= 1 (length fasls)) (format nil "the compile was kept: ~s" fasls))
        (with-stubbed-fdefinition (nlk::compile-layer-file (source fasl)
                                   (incf compiles)
                                   (values nil nil))
          (is (equal '("user.lisp")
                     (mapcar #'car (nlk::cell-skipped (first (load-cell-fixture root))))))
          (is (zerop compiles) "from the list read back, without compiling"))
        (write-layer-folder root "nl-skip-cached"
                            (list user '("helper" . "(defun cells-probe-skipc-helper () :helper)")))
        (is (null (nlk::cell-skipped (first (load-cell-fixture root)))))
        (is (eq :helper (evolved-fn "CELLS-PROBE-SKIPC-USER")) "and the caller runs")
        (is (member (first fasls) (layer-cache-fasls cache "stamp-s" "nl-skip-cached")
                    :test #'string=))))))

(deftest cells-layer-cache-rebinds-a-special-compiled-ahead-of-it (with-cell-fixture (root))
  ;; A worker compiles a caller ahead of the file that proclaims its variable
  ;; special, so the caller's LET binds lexically: the compile notes the
  ;; binding, and the load, the variable special by then, compiles the file
  ;; again, once. The symbols are fresh on both sides of the test.
  (flet ((forget ()
           (dolist (name '("*CELLS-PROBE-REBIND*" "CELLS-PROBE-REBIND-READ" "CELLS-PROBE-REBIND"))
             (nlk:when-let (symbol (find-symbol name '#:nodecode.evolved))
               (unintern symbol '#:nodecode.evolved)))))
    (forget)
    (nlk:with-cleanup ((forget))
      (with-layer-cache (cache "stamp-r")
        (let ((compiles 0))
          (write-layer-folder
           root "nl-rebind"
           '(("var" . "(defvar *cells-probe-rebind* 0)")
             ("read" . "(defun cells-probe-rebind-read () *cells-probe-rebind*)")
             ("bind" . "(defun cells-probe-rebind () (let ((*cells-probe-rebind* 1)) (cells-probe-rebind-read)))")))
          ;; the worker: the layer not loaded, *CELLS-PROBE-REBIND* not special
          (let ((source (merge-pathnames "nl-rebind/bind.lisp" root)))
            (multiple-value-bind (fasl missing)
                (let ((*error-output* (make-broadcast-stream)))
                  (nlk::muffled-load
                   (lambda ()
                     (nlk::compile-layer-file
                      source (ensure-directories-exist (nlk::layer-fasl-path source))))))
              (is (and fasl (probe-file fasl)) "the worker published its fasl")
              (is (member '(:binding "NODECODE.EVOLVED" "*CELLS-PROBE-REBIND*" nil) missing
                          :test #'equal)
                  (format nil "the lexical binding is noted: ~s" missing))))
          (load-cell-fixture root)
          (is (eql 1 (evolved-fn "CELLS-PROBE-REBIND")) "the caller rebinds the special")
          (is-carrying (text (nlk:cells-folder-text))
            (:absent "SKIPPED" "nothing skipped")
            (:absent "FAILED" "nothing failed"))
          (with-stubbed-fdefinition (nlk::compile-layer-file (source fasl)
                                     (incf compiles)
                                     (funcall original source fasl))
            (load-cell-fixture root)
            (is (zerop compiles) "the next load compiles nothing again")
            (is (eql 1 (evolved-fn "CELLS-PROBE-REBIND")) "and still rebinds")))))))

(deftest cells-layer-compile-takes-only-the-files-it-claims ()
  ;; A worker compiles the files whose claim — one MKDIR under CLAIMS — it
  ;; wins, and marks a compile that published nothing `failed', where the
  ;; launch waiting for that fasl looks.
  (with-temp-directory (dir "layer-claims")
    (let ((claims (merge-pathnames "claims/" dir))
          (nursery (sb-ext:bytes-consed-between-gcs))
          (pairs (loop for (name . text) in '(("taken" . "(defun cells-probe-claim-taken () 0)")
                                               ("free" . "(defun cells-probe-claim-free () 1)")
                                               ("broken" . "(defun cells-probe-claim-broken ( :never"))
                       for source = (write-temp-file (merge-pathnames (format nil "~a.lisp" name) dir)
                                                     (format nil "(in-package :nodecode.evolved)~%~a" text))
                       collect (cons (namestring source)
                                     (namestring (make-pathname :type "fasl" :defaults source))))))
      (ensure-directories-exist (merge-pathnames "0/" claims))
      (unwind-protect (let ((*error-output* (make-broadcast-stream)))
                        (nlk::muffled-load (lambda () (nlk::compile-layer-shares pairs 1 (namestring claims))))
                        ;; Without this, a shell that compiles its layer would
                        ;; come back to the organism's nursery, not its own.
                        ;; It comes back on a timer, shortly after (WITH-NURSERY).
                        (is (await (:tries 200 :rest 0.01)
                              (= nursery (sb-ext:bytes-consed-between-gcs))) "the process's own nursery comes back"))
        (setf (sb-ext:bytes-consed-between-gcs) nursery))
      (is (not (probe-file (cdr (first pairs)))) "a file claimed elsewhere is not compiled here")
      (is (probe-file (cdr (second pairs))) "a free one is")
      (is (probe-file (merge-pathnames "1/" claims)) "under its own claim")
      (is (probe-file (merge-pathnames "2/failed/" claims)) "a compile that failed says so"))))

(deftest cells-layer-with-a-pathname-fails-as-a-unit (with-cell-fixture (root))
  ;; A layer's files sit beside its .asd: a :pathname the data read cannot
  ;; honour fails the folder loudly instead of loading nothing.
  (let ((directory (merge-pathnames "nl-relocated/" root)))
    (write-temp-file
     (merge-pathnames "nl-relocated.asd" directory)
     "(defsystem \"nl-relocated\" :class nodecode.kernel:layer :pathname \"src/\" :serial t :components ((:file \"probe\")))")
    (write-temp-file (merge-pathnames "src/probe.lisp" directory)
                     (evolved-source "(defun cells-probe-relocated () :relocated)"))
    (is-present (record (first (load-cell-fixture root))) "the folder's record"
      (is (eq :failed (nlk:cell-state record)) "the folder failed")
      (is (eq :layer (nlk:cell-kind record)) "as a layer")
      (is (equal '(nil) (mapcar #'car (nlk:cell-failures record))))
      (is (search ":pathname" (cdr (first (nlk:cell-failures record))))
          (format nil "naming the option: ~s" (cdr (first (nlk:cell-failures record))))))))

(deftest cells-folder-order-reads-the-asd-as-data (with-cell-fixture (root))
  ;; Ordering never parses an .asd through ASDF: a folder whose .asd does
  ;; not read is ordered by name and fails where it loads, with ASDF's own
  ;; diagnostics, and its readable sibling still loads.
  (write-layer-folder root "nl-order-fine"
                      '(("probe" . "(defun cells-probe-order-fine () :fine)")))
  (write-temp-file (merge-pathnames "nl-order-broken/nl-order-broken.asd" root)
                   "(defsystem \"nl-order-broken\" :components ((:file \"probe\"")
  (let ((records (load-cell-fixture root)))
    (is (equal '("nl-order-broken" "nl-order-fine") (mapcar #'nlk:cell-name records)))
    (is (eq :failed (nlk:cell-state (nlk:find-cell "nl-order-broken"))))
    (is (eq :loaded (nlk:cell-state (nlk:find-cell "nl-order-fine"))))
    (is (eq :fine (evolved-fn "CELLS-PROBE-ORDER-FINE")) "and is live")))

;;; --- the shipped catalog, install, and the load after boot ---------------------

(defun write-peripheral-folder (root name &key exports forms (start "(declare (ignore config)) nil")
                                            depends-on description)
  "WRITE-CELL-FOLDER under ROOT of the one-file peripheral a scratch shipped
folder holds, its package NAME exporting START-CELL and EXPORTS: FORMS, then
START-CELL with START for its body; DEPENDS-ON and DESCRIPTION reach the folder
writer."
  (write-cell-folder
   root name
   (list (cons "cell" (format nil "(defpackage #:~a (:use #:cl) (:export #:start-cell~{ #:~a~}))
(in-package #:~a)~%~{~a~%~}(defun start-cell (config) ~a)"
                              name exports name forms start)))
   :depends-on depends-on :description description))

(deftest cells-shipped-catalog-lists-folders-and-installs-them-into-the-one-folder ()
  ;; The binary ships its cell folders beside its source; the catalog
  ;; reads them as data, install links one under *CELLS-DIRECTORY*, and
  ;; the closure brings what a folder stands on.
  (with-temp-trees ((root "shipped") (folder "cells-install"))
    (let ((source (merge-pathnames "src/" root))
          (cells (merge-pathnames "cells/" root)))
      (ensure-directories-exist source)
      (write-peripheral-folder cells "nl-ship-base")
      (write-peripheral-folder cells "nl-ship-top" :depends-on '("nl-ship-base")
                                                    :description "Does the top thing")
      (write-peripheral-folder cells "nl-ship-needy"
                               :depends-on '("nl-ship-base" "nl-no-such-system-zz"))
      (write-peripheral-folder cells "template")
      (write-peripheral-folder (merge-pathnames "channels/" cells) "nl-ship-chan")
      (is (equal (namestring (truename cells))
                 (namestring (nlk:shipped-cells-directory source))))
      (is (null (nlk:shipped-cells-directory nil)) "no source, no catalog")
      (let ((shipped (nlk:shipped-cells :root cells)))
        (is (equal '("nl-ship-base" "nl-ship-chan" "nl-ship-needy" "nl-ship-top")
                   (mapcar #'nlk:offer-name shipped)))
        (let ((top (nlk:find-offer "nl-ship-top" shipped))
              (needy (nlk:find-offer "nl-ship-needy" shipped)))
          (is-shape top (.depends-on '("nl-ship-base")) (.description "Does the top thing"))
          (is (equal "" needy.description) "none reads empty")
          (is (null top.missing))
          (is (equal '("nl-no-such-system-zz") needy.missing)))
        (is (equal '("nl-ship-base" "nl-ship-top")
                   (nlk:offer-closure '("nl-ship-top") shipped)))
        (is (equal '("nl-ship-base" "nl-ship-top" "nl-ship-chan")
                   (nlk:offer-closure '("nl-ship-top" "nl-ship-chan") shipped)))
        (with-saved-globals ((nlk::*cells-directory* folder))
          (is (nlk:install-cell "nl-ship-top" :offers shipped))
          (is (nlk:installed-cell-p "nl-ship-top"))
          (is (equal '("nl-ship-top") (mapcar #'nlk::folder-name (nlk::cell-folders folder))))
          (is (nlk:install-cell "nl-ship-top" :offers shipped))
          (is (signals-error error (nlk:install-cell "nl-ship-needy" :offers shipped)))
          (is (signals-error error (nlk:install-cell "nl-ship-nope" :offers shipped)))
          (is (nlk:uninstall-cell "nl-ship-top"))
          (is (not (nlk:installed-cell-p "nl-ship-top")))
          (is (null (nlk:uninstall-cell "nl-ship-top")) "nothing to remove twice")
          ;; the operator's own folder is never the installer's to remove
          (write-temp-file (merge-pathnames "mine/mine.asd" folder) "(defsystem \"mine\")")
          (is (null (nlk:uninstall-cell "mine")))
          (is (probe-file (merge-pathnames "mine/mine.asd" folder))))))))

;;; --- the hub: an index of entries, each cloned at its pinned commit ---------

(defun hub-index-text (&rest entries)
  "A format-1 hub index of ENTRIES, each a plist of NAME GIT COMMIT and the
optional PATH, PLATFORMS, DEPENDS-ON, ARCHIVE and SHA256."
  (format nil "{\"format\": 1, \"cells\": [~{~a~^, ~}]}"
          (loop for entry in entries
                collect (destructuring-bind (&key name git commit path platforms depends-on archive sha256) entry
                          (format nil "{\"name\": ~s, \"git\": ~s, \"commit\": ~s, \"description\": \"d\"~
                                     ~@[, \"path\": ~s~]~@[, \"platforms\": [~{~s~^, ~}]~]~
                                     ~@[, \"depends_on\": [~{~s~^, ~}]~]~
                                     ~@[, \"archive\": ~s~]~@[, \"sha256\": ~s~]}"
                                  name git commit path platforms depends-on archive sha256)))))

(deftest cells-hub-index-reads-pinned-entries-and-refuses-a-loose-one ()
  ;; An entry clones from https:// only and pins a full commit: a branch or a
  ;; short id is a moving target, and the pin is what a review read. An index
  ;; that breaks the format is refused whole; another platform's entry and a
  ;; name the release ships are passed over.
  (let* ((commit (make-string 40 :initial-element #\a))
         (git "https://example.org/o/nl-hub-a.git")
         (shipped (list (nlk::make-offer :name "nl-hub-shipped" :systems '("nl-hub-shipped"))))
         (offers (nlk:read-hub-index
                  (hub-index-text (list :name "nl-hub-b" :git git :commit commit
                                        :depends-on '("nl-hub-shipped" "nl-no-such-system-zz"))
                                  (list :name "nl-hub-a" :git git :commit commit :path "cells/nl-hub-a"
                                        :platforms (list nlk::+hub-platform+))
                                  (list :name "nl-hub-elsewhere" :git git :commit commit
                                        :platforms '("plan9"))
                                  (list :name "nl-hub-shipped" :git git :commit commit))
                  :shipped shipped)))
    (is (equal '("nl-hub-a" "nl-hub-b") (mapcar #'nlk:offer-name offers)))
    (let ((top (second offers)))
      (is-shape top (.git git) (.commit commit) (.path nil) (.description "d")
                (.missing '("nl-no-such-system-zz")))
      (is (equal "cells/nl-hub-a" (nlk:offer-path (first offers)))))
    (flet ((refused (&rest entry)
             (refusal-text error (nlk:read-hub-index (hub-index-text entry) :shipped '()))))
      (is (search "https://" (refused :name "nl-hub-a" :git "git@example.org:o/a" :commit commit)))
      (is (search "40-hex" (refused :name "nl-hub-a" :git git :commit "main")))
      (is (search "a name is" (refused :name "../escape" :git git :commit commit)))
      ;; a path is a folder inside the repository, never out of it or into .git
      (dolist (path '("../up" "/abs" "a/../b" ".git/x" "a//b" "a/"))
        (is (search "path is a folder" (refused :name "nl-hub-a" :git git :commit commit :path path))
            path))
      ;; an archive is https:// and comes with the sha256 its bytes must have
      (let ((sha (make-string 64 :initial-element #\0)))
        (is (search "archive is" (refused :name "nl-hub-a" :git git :commit commit :path "a" :archive "http://example.org/a.tar.gz" :sha256 sha)) "not over plain http")
        (is (search "archive is" (refused :name "nl-hub-a" :git git :commit commit :path "a" :archive "https://example.org/a.tar.gz")) "not without its sha256")
        (is (search "archive is" (refused :name "nl-hub-a" :git git :commit commit :path "a" :sha256 sha)) "nor a sha256 alone")
        (let ((entry (first (nlk:read-hub-index (hub-index-text (list :name "nl-hub-a" :git git :commit commit :path "a" :archive "https://example.org/a.tar.gz" :sha256 sha)) :shipped '()))))
          (is (equal "https://example.org/a.tar.gz" (nlk::offer-archive entry)) "the archive is read")
          (is (equal sha (nlk::offer-sha256 entry)) "beside its sha256"))))
    (is (search "format 1" (refusal-text error (nlk:read-hub-index "{\"format\": 2, \"cells\": []}"))))))

(defun hub-fixture-repository (parent folder &optional (prepare #'identity))
  "A git repository at PARENT/FOLDER/ holding the one-file peripheral FOLDER
and whatever PREPARE, called with the folder, puts beside it, committed.
=> (values the repository's native path, the commit id)."
  (funcall prepare (write-peripheral-folder parent folder))
  (let ((native (string-right-trim "/" (uiop:native-namestring
                                         (merge-pathnames (format nil "~a/" folder) parent)))))
    (flet ((git (&rest args)
             (string-trim '(#\Newline)
                          (nlk::run-checked (list* "git" "-C" native "-c" "user.name=nl" "-c" "user.email=nl@nl"
                                                   "-c" "commit.gpgsign=false" args)
                                            (lambda (err) (error "git ~a: ~a" (first args) err))))))
      (git "init" "--quiet")
      (git "add" "-A")
      (git "commit" "--quiet" "-m" "fixture")
      (values native (git "rev-parse" "HEAD")))))

(defun hub-offer (name git commit &rest keys)
  "An entry Nodecode's index lists, offering NAME, its primary system, cloned
from GIT at COMMIT; KEYS come first, so they win."
  (apply #'nlk::make-offer (append keys (list :name name :git git :commit commit
                                              :systems (list name) :index "hub"))))

(defun hub-install (offer root)
  "INSTALL-CELL of OFFER's name under ROOT, OFFER the one entry there is."
  (nlk:install-cell (nlk:offer-name offer) :root root :offers (list offer)))

(defun hub-refusal (offer root)
  "The text of the error HUB-INSTALL of OFFER under ROOT signals."
  (refusal-text error (hub-install offer root)))

(deftest cells-hub-entry-installs-at-its-commit-and-uninstalls ()
  ;; The clone is proven before it moves in: HEAD is the pin and the primary
  ;; system is the entry's name. It carries the .hub marker the uninstaller
  ;; honours, and a failed clone leaves nothing under the root.
  (with-temp-trees ((remote "hub-remote") (home "hub-home"))
    (let ((root (merge-pathnames "cells/" home)))
      (multiple-value-bind (git commit) (hub-fixture-repository remote "nl-hub-peri")
        (is (hub-install (hub-offer "nl-hub-peri" git commit) root))
        (let ((folder (merge-pathnames "nl-hub-peri/" root)))
          (is (probe-file (merge-pathnames "nl-hub-peri.asd" folder)))
          (is (equal (list git commit)
                     (uiop:read-file-lines (merge-pathnames ".hub" folder))))
          (is (null (probe-file (merge-pathnames ".cell-staging/" home))) "the staging folder goes too"))
        (is (nlk:uninstall-cell "nl-hub-peri" :root root))
        (is (not (nlk:installed-cell-p "nl-hub-peri" root)))
        (let ((unpinned (hub-offer "nl-hub-peri" git (make-string 40 :initial-element #\0)))
              (misnamed (hub-offer "nl-hub-other" git commit))
              (misdescribed (hub-offer "nl-hub-peri" git commit :description "Not what it says")))
          (is (search "nl-hub-peri: git checkout" (hub-refusal unpinned root)))
          (is (search "primary system is nl-hub-peri" (hub-refusal misnamed root)))
          (is (search "description is not the .asd's" (hub-refusal misdescribed root)))
          (is (null (nlk::cell-folders root)) "a refused clone leaves nothing to load"))))))

(deftest cells-hub-entry-at-a-path-installs-that-folder-alone ()
  ;; One repository may carry several entries, each a folder of it: the
  ;; entry's path is what moves in, marked with that path, and the rest of the
  ;; clone goes. A path the repository lacks is refused, and so is one that
  ;; reaches out of it through a link.
  (with-temp-trees ((remote "hub-mono-remote") (home "hub-mono-home") (outside "hub-mono-outside"))
    (let ((root (merge-pathnames "cells/" home)))
      (write-peripheral-folder outside "nl-hub-out")
      (multiple-value-bind (git commit)
          (hub-fixture-repository remote "nl-hub-mono"
                                  (lambda (folder &aux (cells (merge-pathnames "cells/" folder)))
                                    (write-peripheral-folder cells "nl-hub-one")
                                    (write-peripheral-folder cells "nl-hub-two")
                                    #-win32
                                    (nlk:make-symlink outside
                                                      (merge-pathnames "elsewhere" folder))))
        (flet ((entry (name path) (hub-offer name git commit :path path)))
          (is (hub-install (entry "nl-hub-one" "cells/nl-hub-one") root))
          (let ((folder (merge-pathnames "nl-hub-one/" root)))
            (is (probe-file (merge-pathnames "nl-hub-one.asd" folder)))
            (is (null (probe-file (merge-pathnames ".git/" folder))) "the folder alone, not the clone")
            (is (equal (list git commit "cells/nl-hub-one")
                       (uiop:read-file-lines (merge-pathnames ".hub" folder))))
            (is (null (probe-file (merge-pathnames ".cell-staging/" home))) "the rest of the clone goes"))
          (is (= 1 (length (nlk::cell-folders root))) "only the entry's folder moved in")
          (is (nlk:uninstall-cell "nl-hub-one" :root root))
          (is (search "has no folder cells/nl-hub-three"
                      (hub-refusal (entry "nl-hub-two" "cells/nl-hub-three") root)))
          #-win32
          (let ((through-a-link (hub-refusal (entry "nl-hub-out" "elsewhere/nl-hub-out") root)))
            (is (search "has no folder elsewhere/nl-hub-out" through-a-link) "a path out through a link"))
          (is (null (nlk::cell-folders root)) "a refused clone leaves nothing to load")
          (is (probe-file (merge-pathnames "nl-hub-out/nl-hub-out.asd" outside)) "and moves nothing out of place"))))))

(deftest cells-hub-folders-of-one-repository-come-out-of-one-kept-copy ()
  ;; Folders of one repository at one commit fetch it once: the first pick
  ;; fills the copy the home keeps, and the next needs no network -- here,
  ;; no remote at all.
  (with-temp-trees ((remote "hub-kept-remote") (home "hub-kept-home"))
    (let ((root (merge-pathnames "cells/" home)))
      (multiple-value-bind (git commit)
          (hub-fixture-repository remote "nl-hub-kept"
                                  (lambda (folder &aux (cells (merge-pathnames "cells/" folder)))
                                    (write-peripheral-folder cells "nl-hub-k1")
                                    (write-peripheral-folder cells "nl-hub-k2")))
        (flet ((entry (name) (hub-offer name git commit :path (format nil "cells/~a" name))))
          (is (hub-install (entry "nl-hub-k1") root) "the first pick fetches")
          (nlk:remove-tree (merge-pathnames "nl-hub-kept/" remote))
          (is (hub-install (entry "nl-hub-k2") root) "the next one has no remote to fetch from")
          (dolist (name '("nl-hub-k1" "nl-hub-k2"))
            (let ((folder (merge-pathnames (format nil "~a/" name) root)))
              (is (probe-file (merge-pathnames (format nil "~a.asd" name) folder)))
              (is (null (probe-file (merge-pathnames ".git/" folder))) "the folder alone")
              (is (equal (list git commit (format nil "cells/~a" name))
                         (uiop:read-file-lines (merge-pathnames ".hub" folder))))))
          (is (null (probe-file (merge-pathnames ".cell-staging/" home))) "the staging folder goes")
          (is (= 2 (length (nlk::cell-folders root))) "both folders, and nothing else, moved in"))))))

#-win32
(deftest cells-hub-entry-carrying-a-symbolic-link-is-refused ()
  ;; A link in the clone could reach anywhere on the machine the boot loader
  ;; then loads from; a profile distribution is refused the same way.
  (with-temp-trees ((remote "hub-link-remote") (home "hub-link-home"))
    (multiple-value-bind (git commit)
        (hub-fixture-repository remote "nl-hub-linked"
                                (lambda (folder)
                                  (nlk:make-symlink "/etc/hostname"
                                                    (merge-pathnames "reach" folder))))
      (let ((root (merge-pathnames "cells/" home)))
        (is (search "symbolic link" (hub-refusal (hub-offer "nl-hub-linked" git commit) root)))
        (is (not (nlk:installed-cell-p "nl-hub-linked" root)))))))

(deftest cells-load-cell-puts-a-folder-on-the-report-after-boot (with-cell-fixture (root))
  ;; LOAD-CELL is the boot loader's per-folder load run later: the record
  ;; lands on (CELLS), a name already there is not loaded twice, a name
  ;; with no folder is skipped.
  (let ((name (format nil "nl-late-~36r" (random (expt 36 6)))))
    (write-peripheral-folder root name)
    (with-saved-globals ((nlk::*cells-directory* root))
      (let ((asdf:*central-registry* asdf:*central-registry*)
            (asdf::*immutable-systems* asdf::*immutable-systems*))
        (let ((records (nlk:load-cell name)))
          (is (= 1 (length records)) "one folder, one record")
          (is-present (record (first records)) "the record"
            (is (eq :loaded (nlk:cell-state record)))
            (is (eq :peripheral (nlk:cell-kind record)) "START-CELL makes it a peripheral")
            (is (eq record (nlk:find-cell name)) "and it is on the report"))
          (is (null (nlk:load-cell name)) "a name already on the report is not loaded twice")
          (is (eq (first records) (nlk:find-cell name)) "its record stands")
          (is (null (nlk:load-cell "nl-no-such-folder")) "a name with no folder is skipped"))))))

(defvar *load-probe* nil
  "The function the scratch folder of WITH-PROBING-FOLDER calls as its file
loads, or NIL.")

(defmacro with-probing-folder ((name root prefix) probe &body body)
  "BODY with NAME a fresh PREFIX-named peripheral under ROOT, the cells
directory, whose file calls PROBE, bound as *LOAD-PROBE*, as it loads."
  `(let ((,name (format nil "~a-~36r" ,prefix (random (expt 36 6)))))
     (write-peripheral-folder
      ,root ,name
      :forms '("(let ((probe (symbol-value (find-symbol \"*LOAD-PROBE*\" \"NODECODE.TEST\"))))
  (when probe (funcall probe)))"))
     (with-saved-globals ((nlk::*cells-directory* ,root) (*load-probe* ,probe))
       ,@body)))

(defun loader-thread (thunk &aux (running (bt2:make-semaphore :name "cells-test-loader-running")))
  "THUNK on a thread of its own, ASDF's state fenced to it as
LOAD-CELL-FIXTURE fences a load's, answered once the thread runs; JOIN-THREAD
answers its values."
  ;; SB-THREAD, not bordeaux-threads: BT2:JOIN-THREAD signals for any
  ;; condition the thread saw, and ASDF signals SYSTEM-OUT-OF-DATE, a plain
  ;; condition, on every load.
  (prog1 (sb-thread:make-thread (lambda (&aux (asdf:*central-registry* asdf:*central-registry*)
                                              (asdf::*immutable-systems* asdf::*immutable-systems*))
                                  (bt2:signal-semaphore running)
                                  (funcall thunk))
                                :name "cells-test-loader")
    (is (bt2:wait-on-semaphore running :timeout 10) "the loader thread runs")))

(deftest cells-a-folder-two-threads-ask-for-loads-once (with-cell-fixture (root))
  ;; The first shell frame asks LOAD-CELL for the import folder while the boot
  ;; thread loads the folder it already carries: two loads of one system at
  ;; once tore ASDF's tables and SBCL's class tables, and the import was lost
  ;; (setup-drive an-02, vr-02). The load is one turn of one lock; the second
  ;; asker finds the first's record on the report.
  (let ((entered (bt2:make-semaphore :name "entered"))
        (release (bt2:make-semaphore :name "release"))
        (loads 0))
    (with-probing-folder (name root "nl-flight")
        (lambda () (incf loads) (bt2:signal-semaphore entered) (bt2:wait-on-semaphore release))
      (let* ((first-thread (loader-thread (lambda () (nlk:load-cell name))))
             (inside (bt2:wait-on-semaphore entered :timeout 60))
             (second-thread (loader-thread (lambda () (nlk:load-cell name)))))
        (is inside "the first asker is inside the folder's load")
        (is (null (bt2:wait-on-semaphore entered :timeout 0.5)) "the second asker waits its turn")
        (bt2:signal-semaphore release :count 2)
        (let ((first-records (sb-thread:join-thread first-thread))
              (second-records (sb-thread:join-thread second-thread)))
          (is (= 1 loads) "one load")
          (is (= 1 (length first-records)) "the first asker's record")
          (is (null second-records) "the second asker loaded nothing")
          (is (= 1 (count name (nlk:cells) :key #'nlk:cell-name :test #'string=)) "one record")
          (is (eq (first first-records) (nlk:find-cell name)) "the first's record stands")
          (is (eq :loaded (nlk:cell-state (nlk:find-cell name)))))))))

(deftest cells-the-boot-load-and-a-request-for-its-folder-load-it-once (with-cell-fixture (root))
  ;; The pair the launch makes: the boot thread's LOAD-CELLS over the folder
  ;; and the wizard's LOAD-CELL for a folder in it.
  (let ((entered (bt2:make-semaphore :name "entered"))
        (release (bt2:make-semaphore :name "release"))
        (loads 0))
    (with-probing-folder (name root "nl-boot")
        (lambda () (incf loads) (bt2:signal-semaphore entered) (bt2:wait-on-semaphore release))
      (let* ((boot (loader-thread (lambda () (nlk:load-cells :root root))))
             (inside (bt2:wait-on-semaphore entered :timeout 60))
             (request (loader-thread (lambda () (nlk:load-cell name)))))
        (is inside "the boot is inside the folder's load")
        (is (null (bt2:wait-on-semaphore entered :timeout 0.5)) "the request waits behind the boot")
        (bt2:signal-semaphore release :count 2)
        (sb-thread:join-thread boot)
        (is (null (sb-thread:join-thread request)) "the request loaded nothing")
        (is (= 1 loads) "one load")
        (is (= 1 (count name (nlk:cells) :key #'nlk:cell-name :test #'string=)))
        ;; And the other order: the request first, the boot's walk after it,
        ;; reads the folder as loaded and does not run its files again.
        (nlk:load-cells :root root)
        (is (= 1 loads) "a folder already loaded is not run again by the walk")
        (is (eq :loaded (nlk:cell-state (nlk:find-cell name))))))))

(deftest cells-broken-link-is-reported-instead-of-vanishing (with-cell-fixture (root))
  ;; A link to a folder that is gone carries no .asd, so the folder walk
  ;; cannot see it: the load reports it as a failed folder naming the stale
  ;; target, rather than letting the cell leave without a word.
  (let ((name (format nil "nl-live-~36r" (random (expt 36 6))))
        (gone (merge-pathnames "nl-gone-folder/" (temp-path "cells-gone"))))
    (write-peripheral-folder root name)
    (nlk:make-symlink gone (merge-pathnames "nl-broken-link" root))
    (is (equal '("nl-broken-link") (nlk:broken-cell-links root)))
    (let ((records (load-cell-fixture root)))
      (is (= 2 (length records)) "the loaded folder and the broken link")
      (let ((loaded (find name records :key #'nlk:cell-name :test #'string=))
            (broken (find "nl-broken-link" records
                          :key #'nlk:cell-name :test #'string=)))
        (is-present loaded "the live folder still loaded"
          (is (eq :loaded (nlk:cell-state loaded))))
        (is-present broken "the broken link has a record"
          (is (eq :failed (nlk:cell-state broken)))
          (is-present (failure (assoc nil (nlk:cell-failures broken))) "it failed as a unit"
            (is-carrying (text (cdr failure)) "the installed folder is gone" "nl-gone-folder"))))
      (is-carrying (text (nlk:cells-folder-text))
        (is (search "cell nl-broken-link: FAILED the installed folder is gone" text))
        (:absent "nl-broken-link: library" "and gives it no summary line")))
    ;; Repaired, the folder loads like any other.
    (sb-posix:unlink (sb-ext:native-namestring
                      (merge-pathnames "nl-broken-link" root)))
    (is (null (nlk:broken-cell-links root)) "nothing broken once the link is gone")))

(defun write-shipped-tree (tree folders &aux (cells (merge-pathnames "src/cells/" tree)))
  "Lay TREE out the way a release or a checkout is laid out: each of
FOLDERS at TREE/src/cells/<rel>/ with an .asd named by its last component,
REL a name, or a list of names for a nested one. => TREE/src/cells/."
  (dolist (rel folders cells)
    (let ((parts (uiop:ensure-list rel)))
      (write-peripheral-folder (merge-pathnames
                                (make-pathname :directory (cons :relative (butlast parts)))
                                cells)
                               (car (last parts))))))

(deftest cells-boot-names-shipped-folders-that-need-explicit-installation ()
  ;; A release can ship a new folder after this home was created. Presence is
  ;; still explicit consent, but boot must name the available choice -- once.
  ;; Without this, boot would read all shipped choices on every launch.
  (with-temp-trees ((tree "catalog-drift") (home "catalog-drift-home"))
    (let* ((source (merge-pathnames "src/src/" tree))
           (cells (write-shipped-tree tree '("nl-available-a" "nl-available-b"))))
      ;; the boot finds the shipped folders beside a source tree that is there
      (ensure-directories-exist source)
      (with-saved-globals ((nlk::*cells-directory* home) (nlk::*source-root* source))
        (let ((shipped (nlk:shipped-cells :root cells)))
          (nlk::boot-cells :spawn nil)
          (is (equal '("nl-available-a" "nl-available-b")
                     (mapcar #'nlk:offer-name
                             (nlk:missing-shipped-cells :root home :shipped shipped))))
          (is (search "2 cells not installed yet: nl-available-a, nl-available-b"
                      (board-line "nodecode-shipped-cells")))
          (is (null (nlk:installed-cell-p "nl-available-a" home)))
          (nlk:install-cell "nl-available-a" :root home :offers shipped)
          ;; named already: the next boot, and an install, say nothing of them
          (is (null (board-line "nodecode-shipped-cells")))
          (nlk::boot-cells :spawn nil)
          (is (null (board-line "nodecode-shipped-cells")))
          (write-shipped-tree tree '("nl-available-c"))
          (nlk::boot-cells :spawn nil)
          (is (search "1 cell not installed yet: nl-available-c"
                      (board-line "nodecode-shipped-cells")))
          (nlk:uninstall-cell "nl-available-a" :root home)
          (is (null (board-line "nodecode-shipped-cells"))))))))

#-win32
(deftest cells-refresh-points-a-link-from-another-tree-at-this-images-folder ()
  ;; Rerunning the installer lays the new release beside the old one and
  ;; never touches the home, so a link the installer made still leads into
  ;; the old tree. The boot points it at this image's folder — a nested one
  ;; by everything after cells/, a dangling one like any other — and
  ;; leaves alone a link this image already serves, a link to a folder of
  ;; the operator's own, a link to a folder this image does not ship, and a
  ;; real folder.
  (with-temp-trees ((scratch "refresh-links"))
    (let* ((old (write-shipped-tree (merge-pathnames "old/" scratch)
                                    '("nl-rf-a" ("channels" "nl-rf-chan") "nl-rf-only-old")))
           (new (write-shipped-tree (merge-pathnames "new/" scratch)
                                    '("nl-rf-a" ("channels" "nl-rf-chan"))))
           (home (merge-pathnames "home/" scratch))
           (kept '("nl-rf-current" "nl-rf-fork" "nl-rf-only-old")))
      (flet ((link (name target) (nlk:make-symlink target (merge-pathnames name home)))
             (points-at (name)
               (nlk:symlink-target (merge-pathnames name home)))
             (own (relative)
               (sb-ext:native-namestring (truename (merge-pathnames relative new))))
             (refresh () (nlk::refresh-installed-cells :root home :shipped-root (truename new))))
        (ensure-directories-exist home)
        (link "nl-rf-a" (merge-pathnames "nl-rf-a/" old))
        (link "nl-rf-chan" (merge-pathnames "channels/nl-rf-chan/" old))
        (link "nl-rf-dangling" (merge-pathnames "gone/src/cells/nl-rf-a/" scratch))
        (link "nl-rf-current" (merge-pathnames "nl-rf-a/" new))
        (link "nl-rf-fork" (write-peripheral-folder (merge-pathnames "fork/" scratch) "nl-rf-a"))
        (link "nl-rf-only-old" (merge-pathnames "nl-rf-only-old/" old))
        (write-temp-file (merge-pathnames "mine/mine.asd" home) "(defsystem \"mine\")")
        (let ((before (mapcar #'points-at kept)))
          (is-values (refreshed failed) (refresh)
            (refreshed '("nl-rf-a" "nl-rf-chan" "nl-rf-dangling")) (failed null "none failed"))
          (is-table (target link) (equal (own target) (points-at link))
            ("nl-rf-a/" "nl-rf-a" "the link leads into this image's tree now")
            ("channels/nl-rf-chan/" "nl-rf-chan"
             "a nested folder is matched by everything after cells/")
            ("nl-rf-a/" "nl-rf-dangling" "a link to nothing is pointed back like any other"))
          (is (null (nlk:broken-cell-links home)) "so nothing is broken")
          (is (equal before (mapcar #'points-at kept)))
          (is (probe-file (merge-pathnames "mine/mine.asd" home)))
          (is (null (refresh))))))))

(deftest cells-refresh-makes-a-copy-another-build-made-again ()
  ;; Where the platform has no links an install is a copy, and its marker
  ;; carries the stamp of the image that made it. A boot of any other build
  ;; — a marker from before the stamp was written included — makes the copy
  ;; again from its own folder, nothing of the old one left over; a copy
  ;; this image made and a folder of the operator's own stand as they are.
  (with-temp-trees ((scratch "refresh-copies"))
    (let* ((old (write-shipped-tree (merge-pathnames "old/" scratch) '("nl-rc-a" "nl-rc-b")))
           (new (write-shipped-tree (merge-pathnames "new/" scratch)
                                    '("nl-rc-a" "nl-rc-b" "nl-rc-c")))
           (home (merge-pathnames "home/" scratch)))
      (flet ((copy (name from stamp &aux (nlk::*image-stamp* stamp))
               (nlk::copy-install from (merge-pathnames (format nil "~a/" name) home)))
             (in-home (relative)
               (probe-file (merge-pathnames relative home)))
             (marker (name)
               (uiop:read-file-lines (merge-pathnames (format nil "~a/.shipped" name) home)))
             (refresh () (nlk::refresh-installed-cells :root home :shipped-root (truename new))))
        (let ((nlk::*image-stamp* "this-build"))
          (write-temp-file (merge-pathnames "nl-rc-a/gone.lisp" old)
                           ";; only the old build carries this")
          (copy "nl-rc-a" (merge-pathnames "nl-rc-a/" old) "old-build")
          (copy "nl-rc-b" (merge-pathnames "nl-rc-b/" old) "old-build")
          ;; the marker a build wrote before it carried a stamp
          (write-temp-file (merge-pathnames "nl-rc-b/.shipped" home)
                           (format nil "~a~%" (sb-ext:native-namestring
                                               (merge-pathnames "nl-rc-b/" old))))
          (copy "nl-rc-c" (merge-pathnames "nl-rc-c/" new) "this-build")
          (write-temp-file (merge-pathnames "nl-rc-c/kept.txt" home) "untouched")
          (write-temp-file (merge-pathnames "mine/mine.asd" home) "(defsystem \"mine\")")
          (is-values (refreshed failed) (refresh)
            (refreshed '("nl-rc-a" "nl-rc-b")) (failed null "none failed"))
          (is (in-home "nl-rc-a/nl-rc-a.asd") "made again from this image's folder")
          (is (not (in-home "nl-rc-a/gone.lisp")) "nothing the old copy carried is left")
          (is (equal (list (sb-ext:native-namestring
                            (truename (merge-pathnames "nl-rc-a/" new)))
                           "this-build")
                     (marker "nl-rc-a")))
          (is (equal "this-build" (second (marker "nl-rc-b"))))
          (is (in-home "nl-rc-c/kept.txt") "a copy this image made is not made again")
          (is (in-home "mine/mine.asd") "the operator's folder is untouched")
          (is (null (refresh))))))))

#-win32
(deftest cells-refresh-takes-out-an-install-of-a-folder-no-longer-shipped ()
  ;; Without this, an install left out of a release that no longer ships
  ;; the folder would fail at every boot with Symbol "SHIPPED-CLOSURE"
  ;; not found in the NODECODE.KERNEL package. An install of a folder
  ;; this build does not ship, out of another release's tree,
  ;; is taken out, a link or a copy; one out of a checkout
  ;; (a newer build booting the same home), out of a tree that is
  ;; gone, or of the operator's own stands.
  (with-temp-trees ((scratch "refresh-retire"))
    (flet ((tree (relative folders &key checkout &aux (tree (merge-pathnames relative scratch)))
             (prog1 (write-shipped-tree tree folders)
               (write-temp-file (merge-pathnames "src/nodecode.asd" tree) "(defsystem \"nodecode\")")
               (when checkout (write-temp-file (merge-pathnames ".git/HEAD" tree) "ref: main")))))
      (let* ((old (tree "old/" '("nl-rt-a" ("import" "nl-rt-gone") "nl-rt-copied")))
             (checkout (tree "checkout/" '("nl-rt-a" "nl-rt-newer") :checkout t))
             (new (tree "new/" '("nl-rt-a")))
             (fork (write-peripheral-folder (merge-pathnames "fork/cells/" scratch) "nl-rt-mine"))
             (home (merge-pathnames "home/" scratch)))
        (flet ((link (name target) (nlk:make-symlink target (merge-pathnames name home)))
               (there (name)
                 (or (nlk:symlink-target (merge-pathnames name home))
                     (probe-file (merge-pathnames (format nil "~a/" name) home))))
               (refresh () (nlk::refresh-installed-cells :root home :shipped-root (truename new))))
          (ensure-directories-exist home)
          (link "nl-rt-a" (merge-pathnames "nl-rt-a/" old))
          (link "nl-rt-gone" (merge-pathnames "import/nl-rt-gone/" old))
          (link "nl-rt-newer" (merge-pathnames "nl-rt-newer/" checkout))
          (link "nl-rt-mine" fork)
          (link "nl-rt-deleted" (merge-pathnames "deleted/src/cells/nl-rt-deleted/" scratch))
          (let ((nlk::*image-stamp* "old-build"))
            (nlk::copy-install (merge-pathnames "nl-rt-copied/" old)
                               (merge-pathnames "nl-rt-copied/" home)))
          (let ((nlk::*image-stamp* "this-build"))
            (is-values (refreshed failed retired) (refresh)
              (refreshed '("nl-rt-a")) (failed null "none failed")
              (retired '("nl-rt-copied" "nl-rt-gone")))
            (is-table (name) (null (there name))
              ("nl-rt-gone" "a link into a release that shipped it last")
              ("nl-rt-copied" "a copy, its marker naming such a folder"))
            (is-table (name) (there name)
              ("nl-rt-newer" "a checkout's folder: a newer build booting this home")
              ("nl-rt-mine" "a folder of the operator's own, wherever it stands")
              ("nl-rt-deleted" "a link into a tree that is gone says nothing of whose it was"))
            (is (null (nth-value 2 (refresh))) "nothing is left to take out")))))))

;;; --- the load side: a file back into this image ------------------------------

(defmacro with-source-tree ((root) &body body)
  "Run BODY with ROOT a fresh directory that IS this image's source root — the
tree LOAD-SOURCE accepts a file from."
  ;; The real root, the checkout this image loaded itself from, is restored on
  ;; the way out.
  `(with-temp-directory (,root "source-tree")
     (with-saved-globals ((nlk::*source-root* (truename ,root)))
       ,@body)))

(defmacro with-told-shells ((seen count) &body body)
  "Run BODY with SEEN bound fresh and SOURCE-LOADED advised to answer COUNT
shells told, pushing each (FILE-NAME . DEFINITIONS) it was handed onto SEEN,
newest first; the advice comes off on unwind."
  `(let ((,seen '()))
     (nlk:hook 'nlk:source-loaded "test-told-shells"
               (lambda (next path definitions)
                 (funcall next path definitions)
                 (push (cons (file-namestring path) definitions) ,seen)
                 ,count))
     (nlk:with-cleanup ((nlk:unhook 'nlk:source-loaded "test-told-shells")) ,@body)))

(defun evolved-source (&rest definitions)
  "A source file's text in EVOLVED: its in-package line, then each of
DEFINITIONS after a blank line."
  (format nil "(in-package #:nodecode.evolved)~%~{~%~a~%~}" definitions))

(deftest load-source-makes-a-changed-definition-live (with-source-tree (root))
  ;; The half the file seam was missing: a definition written to a file of
  ;; this image's own tree runs here at once, and only the ones that changed
  ;; are evaluated again.
  (let ((path (merge-pathnames "probe.lisp" root))
        (before (evolved-source "(defun source-probe-a () 1)" "(defun source-probe-b () :b)")))
    (write-temp-file path before)
    (is-present (line (nlk:load-source path)) "a file of this image's tree answers"
      (is (search "live here: source-probe-a" line) line)
      (is (search "source-probe-b" line) "its neighbour lands with it"))
    (is (eql 1 (evolved-fn "SOURCE-PROBE-A")) "and the definition runs here")
    (let ((untouched (fdefinition (nlk::evolved-symbol "SOURCE-PROBE-B")))
          (after (evolved-source "(defun source-probe-a () 2)" "(defun source-probe-b () :b)")))
      (write-temp-file path after)
      (is-present (line (nlk:load-source path :before before)) "the second write answers"
        (is (search "live here: source-probe-a" line) line)
        (is (not (search "source-probe-b" line))))
      (is (eql 2 (evolved-fn "SOURCE-PROBE-A")) "the new body runs")
      (is (eq untouched (fdefinition (nlk::evolved-symbol "SOURCE-PROBE-B")))))))

(deftest load-source-names-what-it-will-not-evaluate (with-source-tree (root))
  ;; Nothing silent: what a load cannot carry is on the receipt.
  (let ((path (merge-pathnames "held.lisp" root))
        (before (evolved-source "(defstruct source-probe-box slot)" "(defun source-probe-gone () 1)"
                                "(defparameter *source-probe-seen* 0)"))
        (after (evolved-source "(defstruct source-probe-box slot other)"
                               "(setf *source-probe-seen* 1)")))
    (write-temp-file path after)
    (is-present (line (nlk:load-source path :before before)) "the write answers"
      (is (search "not live" line) line)
      (is-carrying line ("source-probe-box" "the structure is named, not evaluated")
        ("gone from the file" "what the file lost is named") ("source-probe-gone" "by name")
        "outside any definition"))))

(deftest load-source-leaves-a-file-outside-this-image-alone ()
  ;; A scratch file, a fixture, another project: writing one changes nothing
  ;; here, and the receipt says nothing about it.
  (let ((path (temp-path "stray" "lisp")))
    (write-temp-file path (evolved-source "(defun source-probe-stray () 1)"))
    (is (null (nlk:load-source path)) "a file outside this image's trees says nothing")
    (is (null (find-symbol "SOURCE-PROBE-STRAY" '#:nodecode.evolved)))))

(deftest load-source-keeps-the-layer-out-of-it (with-source-tree (root))
  ;; The file the definition came from is its durable home. A layer copy would
  ;; shadow it with an override that goes stale at the next edit.
  (with-temp-directory (folder "layer-folder")
    (with-saved-globals ((nlk::*cells-directory* folder))
      (let ((path (merge-pathnames "kept.lisp" root)))
        (write-temp-file path (evolved-source "(defun source-probe-kept () 7)"))
        (is (search "live here: source-probe-kept" (or (nlk:load-source path) "")))
        (is (eql 7 (evolved-fn "SOURCE-PROBE-KEPT")) "and runs here")
        (is (null (directory (merge-pathnames "**/*.lisp" folder))))))))

(deftest load-source-loads-kernel-source-under-the-floor (with-source-tree (root))
  ;; The measurement the design stands on: SBCL's package lock exempts the
  ;; locked package's own code, so kernel source loads under its own package
  ;; with the floor up — while the same redefinition from an evolved snippet is
  ;; refused, which is the floor doing exactly its work.
  (with-floor-restored
    (let ((path (merge-pathnames "floor.lisp" root)))
      (nlk:lock-floor)
      (write-temp-file path (format nil "(in-package #:nodecode.kernel)~%~
                                           (defun source-probe-kernel () :kernel)~%"))
      (is (search "live here: source-probe-kernel" (or (nlk:load-source path) "")))
      (is (eq :kernel (funcall (uiop:find-symbol* "SOURCE-PROBE-KERNEL"
                                                  '#:nodecode.kernel))))
      (is (null (ignore-errors
                 (let ((*package* (find-package '#:nodecode.evolved)))
                   (eval (read-from-string
                          "(defun nodecode.kernel::source-probe-kernel () :evolved)")))))))))

;;; --- the shells: what went live here is told, and a shell told loads it ----

(deftest load-source-only-evaluates-the-named-definitions (with-source-tree (root))
  ;; The shell's half of SOURCE-LOADED: told which definitions of a file the
  ;; organism's image just made live, it evaluates exactly those from its
  ;; own copy, and announces nothing on — it is not the origin.
  (let ((path (merge-pathnames "only.lisp" root)))
    (write-temp-file path (evolved-source "(defun source-only-a () :a)"
                                          "(defun source-only-b () :b)"))
    (is-present (line (nlk:load-source path :only '(("DEFUN" . "source-only-a")))) "the named definition answers"
      (is (search "live here: source-only-a" line) line)
      (is (not (search "source-only-b" line)) "its neighbour is not named")
      (is (not (search "sent to" line)) "and nothing is announced on"))
    (is (eq :a (evolved-fn "SOURCE-ONLY-A")) "the named definition runs here")
    (is (not (fboundp (uiop:find-symbol* "SOURCE-ONLY-B" '#:nodecode.evolved))))
    (is (null (nlk:load-source path :only '(("DEFUN" . "source-only-none")))))))

(deftest load-source-tells-the-shells-what-went-live (with-source-tree (root))
  ;; The origin's half: every definition that went live here reaches
  ;; SOURCE-LOADED as (operator . label), in file order, and the clause
  ;; counts the shells the seam says it told. Nothing listens outside a
  ;; serving gateway, and the seam answers 0 there.
  (let ((path (merge-pathnames "told.lisp" root)))
    (is (eql 0 (nlk:source-loaded path '(("DEFUN" . "x")))))
    (with-told-shells (seen 1)
      (write-temp-file path (evolved-source "(defun source-told-a () 1)"
                                            "(defparameter *source-told-p* 2)"))
      (is-present (line (nlk:load-source path)) "the load answers"
        (is (search "live here: source-told-a, *source-told-p*; sent to 1 shell" line) line))
      (is (equal '(("told.lisp" ("DEFUN" . "source-told-a")
                                ("DEFPARAMETER" . "*source-told-p*")))
                 seen)))))

;;; --- a definition goes live once what it names is defined -------------------

(deftest load-source-takes-what-a-changed-definition-names-from-its-file (with-source-tree (root))
  ;; Without this, a write outside any load can put a new helper in a file
  ;; while an edit changes only its caller. The diff would name the caller
  ;; alone, the shells would be told the caller alone, and they would fail at
  ;; every message on the helper no image has. The load takes the helper from
  ;; the file with its caller, and the shells are told both.
  (let* ((path (merge-pathnames "helper.lisp" root))
         (unloaded (evolved-source "(defun source-helper-new () :helped)"
                                   "(defun source-helper-caller () :old)"))
         (edited (evolved-source "(defun source-helper-new () :helped)"
                                 "(defun source-helper-caller () (source-helper-new))")))
    (write-temp-file path edited)
    (with-told-shells (seen 1)
      (is-present (line (nlk:load-source path :before unloaded)) "the edit answers"
        (is-carrying line ("source-helper-caller" "the edited definition is live")
          ("source-helper-new" "and the helper it names, taken from the file")
          (:absent "not live" "nothing is held")))
      (is (eq :helped (evolved-fn "SOURCE-HELPER-CALLER")) "the caller runs")
      (let ((told (sort (mapcar #'cdr (rest (first seen))) #'string<)))
        (is (equal '("source-helper-caller" "source-helper-new") told) "the shells are told both")))))

(deftest load-source-holds-a-definition-naming-what-nothing-defines (with-source-tree (root))
  ;; Nothing in the image or the file defines what the edit names: run, the
  ;; definition fails at every call, and in a shell every call is a frame. It
  ;; is held, by name, and the image keeps the definition it had.
  (let ((path (merge-pathnames "dangling.lisp" root))
        (before (evolved-source "(defun source-dangling-caller () :old)")))
    (write-temp-file path before)
    (nlk:load-source path)
    (write-temp-file path (evolved-source "(defun source-dangling-caller () (source-dangling-nowhere))"))
    (with-told-shells (seen 1)
      (is-present (line (nlk:load-source path :before before)) "the edit answers"
        (is-carrying line ("not live" "the definition is held")
          ("source-dangling-caller" "by name")
          ("source-dangling-nowhere" "with what it names that nothing defines")
          (:absent "sent to" "and no shell is told")))
      (is (null seen)))
    (is (eq :old (evolved-fn "SOURCE-DANGLING-CALLER")) "the image keeps the definition it had")))

(deftest load-source-only-takes-what-a-named-definition-needs (with-source-tree (root))
  ;; The shell's half: told a caller -- the organism had the helper from a load
  ;; this shell never saw -- it takes the helper from its own copy of the file,
  ;; and one naming what nothing defines is held, never run.
  (let ((path (merge-pathnames "only-needs.lisp" root)))
    (write-temp-file path (evolved-source "(defun source-needs-helper () 3)"
                                          "(defun source-needs-caller () (1+ (source-needs-helper)))"
                                          "(defun source-needs-dangling () (source-needs-nowhere))"))
    (let ((told '(("DEFUN" . "source-needs-caller") ("DEFUN" . "source-needs-dangling"))))
      (is-present (line (nlk:load-source path :only told)) "the fold answers"
        (is-carrying line ("source-needs-helper" "the helper rides its caller")
          ("not live" "the dangling one is held")
          ("source-needs-nowhere" "with what it lacks"))))
    (is (eql 4 (evolved-fn "SOURCE-NEEDS-CALLER")))
    (is (not (fboundp (uiop:find-symbol* "SOURCE-NEEDS-DANGLING" '#:nodecode.evolved))))))

(deftest load-source-puts-back-what-its-verify-refuses (with-source-tree (root))
  ;; A compile cannot see an old caller handing a changed callee arguments it
  ;; no longer takes; VERIFY, what must still run, can. When it signals, every
  ;; definition the load made live is put back as it was, and held.
  (let* ((path (merge-pathnames "verify.lisp" root))
         (frame (lambda () (evolved-fn "SOURCE-VERIFY-FRAME")))
         (before (evolved-source "(defun source-verify-callee (a) a)"
                                 "(defun source-verify-frame () (source-verify-callee 1))"))
         (narrowed (evolved-source "(defun source-verify-callee (a b) (list a b))"
                                   "(defun source-verify-frame () (source-verify-callee 1))")))
    (write-temp-file path before)
    (nlk:load-source path)
    (let ((callee (fdefinition (nlk::evolved-symbol "SOURCE-VERIFY-CALLEE"))))
      (write-temp-file path narrowed)
      (is-present (line (nlk:load-source path :before before :verify frame)) "the load answers"
        (is-carrying line ("not live" "the definition is held")
          ("source-verify-callee" "by name")
          ("put back" "and the receipt says it was put back")))
      (let ((now (fdefinition (nlk::evolved-symbol "SOURCE-VERIFY-CALLEE"))))
        (is (eq callee now) "the image has the definition it had"))
      (is (eql 1 (funcall frame)) "and what calls it runs")
      (write-temp-file path (evolved-source "(defun source-verify-callee (a &optional b) (list a b))"
                                            "(defun source-verify-frame () (source-verify-callee 1))"))
      (is-carrying (accepted (or (nlk:load-source path :before narrowed :verify frame) ""))
        ("live here: source-verify-callee" "a load its verify accepts goes live"))
      (is (equal '(1 nil) (funcall frame))))))

(deftest load-source-loads-definitions-that-name-each-other (with-source-tree (root))
  ;; Waiting is within the load: a caller ahead of its helper goes live behind
  ;; it, a structure's accessors are its own, and definitions that name only
  ;; each other go live on each other's word.
  (let ((path (merge-pathnames "order.lisp" root)))
    (write-temp-file path (evolved-source "(defun source-order-first () (source-order-later))"
                                          "(defun source-order-later () :later)"
                                          "(defun source-order-slot (box) (source-order-box-slot box))"
                                          "(defstruct source-order-box slot)"
                                          "(defun source-order-even-p (n) (if (zerop n) t (source-order-odd-p (1- n))))"
                                          "(defun source-order-odd-p (n) (if (zerop n) nil (source-order-even-p (1- n))))"))
    (is-present (line (nlk:load-source path :before "")) "the load answers"
      (is (not (search "not live until a relaunch" line)) line))
    (is (eq :later (evolved-fn "SOURCE-ORDER-FIRST")))
    (is (eq :in (evolved-fn "SOURCE-ORDER-SLOT" (evolved-fn "MAKE-SOURCE-ORDER-BOX" :slot :in))))
    (is (evolved-fn "SOURCE-ORDER-EVEN-P" 4))))

(deftest shipped-folders-compile-beside-themselves-once-a-release-lays-the-directory-down
    (with-temp-trees ((root "shipped-fasl")))
  ;; A release's src/fasl/ routes every shipped source's fasl there; without
  ;; one — a checkout — ASDF's configuration is left alone, and a source
  ;; outside the shipped folders keeps the user cache either way.
  (let* ((source (merge-pathnames "src/src/" root))
         (cells (merge-pathnames "src/cells/" root))
         (fasl (merge-pathnames "src/fasl/" root)))
    (nlk:with-cleanup ((asdf:initialize-output-translations))
      (let ((nlk::*source-root* source))
        (ensure-directories-exist source)
        (ensure-directories-exist (merge-pathnames "nl-ship-fasl/" cells))
        (is (null (nlk::shipped-fasl-directory)) "no directory: nothing ships compiled")
        (is (null (nlk::cell-output-translations '())) "and ASDF is left alone")
        (ensure-directories-exist fasl)
        (is (equal (namestring (truename fasl))
                   (namestring (nlk::shipped-fasl-directory))))
        (asdf:initialize-output-translations (nlk::cell-output-translations '()))
        (is (equal (namestring (merge-pathnames "nl-ship-fasl/cell.fasl" (truename fasl)))
                   (namestring (asdf:apply-output-translations
                                (merge-pathnames "nl-ship-fasl/cell.fasl" (truename cells))))))
        (let ((foreign (asdf:apply-output-translations
                        (merge-pathnames "elsewhere/cell.fasl" (truename root)))))
          (is (not (uiop:string-prefix-p (namestring (truename root)) (namestring foreign)))))))))

(deftest cells-own-peripherals-compile-once-per-build (with-cell-fixture (root))
  ;; ASDF dates a fasl against the core's load, which a dumped image carries
  ;; from its build, so a fasl another build compiled later read as current
  ;; and kept that build's macro expansions. Each build compiles the
  ;; operator's peripherals into a directory of its own; a layer keeps its own
  ;; cache and takes no rule.
  (write-peripheral-folder root "nl-stamped-peri")
  (write-layer-folder root "nl-stamped-layer" '(("one" . "(defun stamped-layer-one () 1)")))
  ;; A second build is another process: in this one the folder is loaded and
  ;; frozen after the first load, so the second build is read off the rules.
  (with-temp-directory (cache "peripheral-cache")
    (nlk:with-cleanup ((asdf:initialize-output-translations))
      (let ((nlk::*peripheral-cache-directory* cache)
            (source (merge-pathnames "nl-stamped-peri/cell.fasl" (truename root)))
            (folders (mapcar #'nlk::make-cell-folder (nlk::cell-folders root))))
        (flet ((fasl-under (stamp)
                 (namestring (merge-pathnames (format nil "~a/nl-stamped-peri/cell.fasl" stamp)
                                              (truename cache)))))
          (let ((nlk::*image-stamp* "stamp-a"))
            (load-cell-fixture root)
            (is (probe-file (fasl-under "stamp-a")) "the first build compiles into its own directory"))
          (let* ((nlk::*image-stamp* "stamp-b")
                 (rules (nlk::cell-output-translations folders)))
            (asdf:initialize-output-translations rules)
            (let ((fasl (namestring (asdf:apply-output-translations source))))
              (is (equal (fasl-under "stamp-b") fasl) "another build keeps its own, never the first's")
              (is (null (find-if (lambda (rule)
                                   (and (consp rule) (search "nl-stamped-layer" (first rule))))
                                 (rest rules))) "a layer takes no rule"))))))))

(deftest cells-hub-index-marks-which-index-listed-an-entry ()
  ;; An offer carries the index that listed it — Nodecode's own unless it
  ;; says otherwise — so a chooser and `nodecode add' can say so.
  (let ((text (hub-index-text (list :name "nl-hub-marked" :git "https://example.org/o/m.git"
                                    :commit (make-string 40 :initial-element #\d)))))
    (is (equal "hub" (nlk:offer-index (first (nlk:read-hub-index text :shipped '())))))
    (is (equal "community" (nlk:offer-index (first (nlk:read-hub-index text :shipped '() :index "community")))))))

(defun hold-load-lock (&aux (held (bt2:make-semaphore :name "test-load-held"))
                            (release (bt2:make-semaphore :name "test-load-release"))
                            (holder (nlk:spawn "test-load-holder"
                                      (bt2:with-recursive-lock-held (nlk::*cell-load-lock*)
                                        (bt2:signal-semaphore held)
                                        (bt2:wait-on-semaphore release :timeout 60)))))
  "Hold the cell load lock on a thread of its own until the function this
answers is called: it lets go and joins that thread."
  (is (bt2:wait-on-semaphore held :timeout 10) "the holder takes the load lock")
  (lambda () (bt2:signal-semaphore release) (bt2:join-thread holder)))

(deftest cells-asdf-is-read-under-the-load-lock ()
  ;; A load defines its system again, and ASDF's redefinition changes the
  ;; class of the very object a reader walks: the boot's shipped-folder
  ;; notice, reading ASDF beside the first frame's load of the import folder,
  ;; died on `The slot OPERATION-TIMES is unbound' (xa-203). A reader of ASDF
  ;; waits for the load in progress, and answers once it is done.
  (let* ((answered :waiting)
         (release (hold-load-lock)))
    ;; a shipped name asks ASDF nothing, so it never waits on a load
    (is (null (nlk::unavailable-systems '("nl-shipped") '("nl-shipped"))))
    (let ((reader (loader-thread
                   (lambda () (setf answered (nlk::unavailable-systems '("nl-never-defined")))))))
      (sleep 0.2)
      ;; the reader waits while a load holds the lock
      (is (eq :waiting answered))
      (funcall release)
      (sb-thread:join-thread reader)
      ;; and answers once the load is done
      (is (equal '("nl-never-defined") answered)))))

(defun compile-lock-contended-p (path)
  "Whether another process -- a python3 child -- is refused the fcntl lock on
PATH: T while a process holds it."
  (equal "held" (string-trim '(#\Newline)
                             (uiop:run-program
                              (list "python3" "-c" "import fcntl,sys
f=open(sys.argv[1],'a')
try:
    fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)
    print('took')
except OSError:
    print('held')" (namestring path))
                              :output :string))))

(deftest cells-compile-into-one-cache-one-process-at-a-time (with-temp-directory (cache "compile-cache"))
  ;; ASDF publishes a fasl by deleting the one there and renaming its own in,
  ;; and loads one it judged current a moment before: several nodecodes
  ;; starting cold on one cache lost a cell in those gaps, 4 of 40
  ;; (ip-02). A load waits while another process holds the cache's compile
  ;; lock, and holds it itself while it runs.
  (let* ((nlk::*peripheral-cache-directory* cache)
         (path (merge-pathnames "compile.lock" cache))
         (ran nil)
         (loader nil))
    (with-lock-holder (path)
      ;; the loader's thread sees the cache only as this one hands it over: a LET is invisible there
      (setf loader (loader-thread
                    (lambda (&aux (nlk::*peripheral-cache-directory* cache))
                      (nlk::call-with-compile-lock (lambda () (setf ran t))))))
      (sleep 0.3)
      ;; a load waits while another process compiles into the cache
      (is (not ran)))
    (sb-thread:join-thread loader)
    ;; and runs once that process is gone
    (is ran)
    (nlk::call-with-compile-lock
     (lambda ()
       ;; another process finds the cache held while a load runs, a nested one included
       (is (compile-lock-contended-p path))
       (nlk::call-with-compile-lock (lambda () (is (compile-lock-contended-p path))))
       (is (compile-lock-contended-p path))))
    ;; released with the load
    (is (not (compile-lock-contended-p path)))))

(deftest cells-shipped-listing-never-waits-on-a-load (with-saved-globals (nlk::*image-systems*))
  ;; /cells, the walk's Cells step and its summary list the shipped
  ;; folders on the shell's own thread, and a load holds the load lock for as
  ;; long as it compiles -- longer when it waits on another process's compile
  ;; into the cache: without this, reading the listing under that lock would
  ;; freeze the shell. Every name a shipped folder stands on is one the image
  ;; carries, answered off the list its save or its last freeze took.
  (nlk::note-image-systems)
  (let ((release (hold-load-lock))
        (lister nil))
    (nlk:with-cleanup ((funcall release) (when lister (sb-thread:join-thread lister :default nil)))
      (setf lister (sb-thread:make-thread #'nlk:shipped-cells :name "test-listing-lister"))
      (let ((entries (sb-thread:join-thread lister :timeout 20 :default :waited)))
        ;; listed while the load holds the lock
        (is (not (eq :waited entries)))
        (is (consp entries))
        (funcall release)
        (when (consp entries)
          ;; and as ASDF itself answers, asked with no list to go by
          (let ((nlk::*image-systems* '()))
            (is (equal (mapcar #'nlk:offer-missing (nlk:shipped-cells))
                       (mapcar #'nlk:offer-missing entries)))))))))

(deftest cells-a-load-waits-while-another-process-compiles-into-its-cache (with-cell-fixture (root))
  ;; The cure for ip-02 is the compile lock LOAD-CELL-FOLDERS takes around a
  ;; peripheral's ASDF load: several nodecodes starting cold on one cache lost
  ;; a cell while two compiled one folder at once. A folder's load waits
  ;; while another process holds the cache's lock, and runs once it lets go.
  (let ((entered (bt2:make-semaphore :name "test-queued-entered"))
        (loader nil))
    (with-probing-folder (name root "nl-queued") (lambda () (bt2:signal-semaphore entered))
      (with-temp-directory (cache "queued-cache")
        (nlk:with-cleanup ((asdf:initialize-output-translations))
          (with-lock-holder ((merge-pathnames "compile.lock" cache))
            ;; the loader's thread sees the cache only as this one hands it over
            (setf loader (loader-thread (lambda (&aux (nlk::*peripheral-cache-directory* cache))
                                          (nlk:load-cell name))))
            ;; the folder's files do not load while another process holds the cache
            (is (null (bt2:wait-on-semaphore entered :timeout 1)))
            (is (null (nlk:find-cell name))))
          ;; that process gone, the load runs
          (is (bt2:wait-on-semaphore entered :timeout 60))
          (let ((records (sb-thread:join-thread loader)))
            (is (= 1 (length records)))
            (is (eq :loaded (nlk:cell-state (first records))))))))))

;;; --- trusted source readers ------------------------------------------------

(deftest cells-trusted-reader-policy-is-scoped (let ((*read-eval* nil)))
  ;; Tool text stays restricted; executing trusted cell source chooses its
  ;; own reader policy and restores the caller even when loading unwinds.
  (is (eql 3 (nlk::muffled-load (lambda () (read-from-string "#.(+ 1 2)")))) "trusted source can use read-time constants")
  (is (null *read-eval*) "normal return keeps the caller restricted")
  (signals-error simple-error
    (nlk::muffled-load (lambda ()
                        (is (eq t *read-eval*) "trusted source chooses its reader policy")
                        (error "reader unwind probe"))))
  (is (null *read-eval*) "unwinding keeps the caller restricted")
  (signals-error reader-error (read-from-string "#.(+ 1 2)")))

(deftest cells-asdf-read-time-constant-loads-from-restricted-caller (with-cell-fixture (root))
  ;; The Kimi sign-in's epoch constant failed in COMPILE-FILE when an eval
  ;; command enabled the cell: ASDF must not inherit that command's policy.
  (write-peripheral-folder root "nl-reader-peri" :exports '("epoch")
                           :forms '("(defun epoch () #.(encode-universal-time 0 0 0 1 1 1970 0))"))
  (let* ((*read-eval* nil)
         (records (load-cell-fixture root))
         (record (first records)))
    (is (= 1 (length records)))
    (is-present record "the ASDF cell record"
      (is (eq :loaded (nlk:cell-state record)) (format nil "the trusted cell loaded: ~s" (nlk:cell-failures record)))
      (is-shape record (nlk:cell-failures null) (nlk:cell-kind eq :peripheral))
      (when (eq :loaded (nlk:cell-state record))
        (is (= 2208988800 (funcall (uiop:find-symbol* "EPOCH" "NL-READER-PERI"))) "the reader constant survived compilation and loading")))
    (is (null *read-eval*) "ASDF loading does not widen the caller's policy")))

(deftest cells-layer-read-time-constant-loads-from-restricted-caller (with-cell-fixture (root))
  ;; The same shared boundary covers layer compilation and the cached load.
  (write-layer-folder
   root "nl-reader-layer"
   '(("epoch" . "(defun cells-probe-reader-epoch () #.(encode-universal-time 0 0 0 1 1 1970 0))")))
  (with-layer-cache (cache "stamp-reader")
    (let ((*read-eval* nil))
      (dotimes (boot 2)
        (let ((record (first (load-cell-fixture root))))
          (is-present record "the layer cell record"
            (is (eq :loaded (nlk:cell-state record)) (format nil "layer load ~d: ~s" boot (nlk:cell-failures record)))
            (is (null (nlk:cell-failures record)))
            (when (eq :loaded (nlk:cell-state record))
              (is (= 2208988800 (evolved-fn "CELLS-PROBE-READER-EPOCH")) "the layer's read-time constant is live"))))
        (is (null *read-eval*) "layer loading keeps the caller restricted"))
      (is (= 1 (length (layer-cache-fasls cache "stamp-reader" "nl-reader-layer"))) "the trusted source compiled into the cache"))))

#-win32
(deftest cells-an-install-loads-this-builds-own-folder-wherever-its-link-leads ()
  ;; Two builds booting one home take turns pointing its installs at their
  ;; own trees (REFRESH-INSTALLED-CELLS), and ASDF reads a link again at every
  ;; search -- a channel's LOAD-SYSTEM minutes after the boot. A build that
  ;; read through the link loaded the other build's source against its own
  ;; core: the channel kit's DEFPACKAGE clashed with the one loaded, and every
  ;; channel was refused. The entry names the cell; the code is this build's.
  (with-temp-trees ((scratch "own-build"))
    (let* ((name "nl-own-build")
           (old (merge-pathnames "old/src/cells/" scratch))
           (new (merge-pathnames "new/src/cells/" scratch))
           (home (merge-pathnames "home/" scratch))
           (link (merge-pathnames name home)))
      (dolist (tree (list old new))
        (write-peripheral-folder tree name :exports '("build")
                                           :forms (list (format nil "(defun build () ~s)"
                                                                (if (eq tree old) :old :new)))))
      (ensure-directories-exist (merge-pathnames "new/src/src/" scratch))
      (ensure-directories-exist home)
      (nlk:make-symlink (merge-pathnames (format nil "~a/" name) old) link)
      (let ((mine (truename (merge-pathnames (format nil "~a/" name) new))))
        (nlk:with-cleanup ((ignore-errors (asdf:clear-system name))
                           (when (find-package (string-upcase name))
                             (delete-package (string-upcase name))))
          (with-saved-globals (nlk::*cells* nlk::*sections*
                               (nlk::*source-root* (merge-pathnames "new/src/src/" scratch)))
            (let ((asdf:*central-registry* asdf:*central-registry*)
                  (asdf::*immutable-systems* asdf::*immutable-systems*))
              (is-present (record (first (nlk:load-cells :root home))) "the install's record"
                (is (eq :loaded (nlk:cell-state record)) (format nil "~a" (nlk:cell-failures record)))
                (is (equal (namestring (merge-pathnames (format nil "~a/" name) home))
                           (namestring (nlk:cell-path record))) "the record is the entry: presence still decides what starts"))
              (is (eq :new (funcall (uiop:find-symbol* '#:build (string-upcase name)))) "this build's folder loaded, not the one the link leads to")
              ;; the search a channel's start makes, after the boot
              (is (equal mine (truename (asdf:system-source-directory (asdf:find-system name)))) "and ASDF finds the system there at every later search"))))))))

(deftest cells-a-copy-another-build-made-is-read-from-this-builds-own-folder ()
  ;; Where an install is a copy, a copy another build made is that build's
  ;; code until this one makes it again: it is read from this build's own
  ;; folder meanwhile. A copy this build made, a folder of the operator's
  ;; own and a folder no build ships read where they stand.
  (with-temp-trees ((scratch "own-copy"))
    (let* ((old (write-shipped-tree (merge-pathnames "old/" scratch) '("nl-oc-a" "nl-oc-b")))
           (new (write-shipped-tree (merge-pathnames "new/" scratch) '("nl-oc-a" "nl-oc-b")))
           (home (merge-pathnames "home/" scratch))
           (shipped (truename new)))
      (flet ((in-home (name) (merge-pathnames (format nil "~a/" name) home))
             (own (directory) (nlk::own-cell-directory directory shipped)))
        (let ((nlk::*image-stamp* "old-build"))
          (nlk::copy-install (merge-pathnames "nl-oc-a/" old) (in-home "nl-oc-a")))
        (let ((nlk::*image-stamp* "this-build"))
          (nlk::copy-install (merge-pathnames "nl-oc-b/" new) (in-home "nl-oc-b"))
          (write-temp-file (merge-pathnames "mine/mine.asd" home) "(defsystem \"mine\")")
          (is (equal (truename (merge-pathnames "nl-oc-a/" new)) (own (in-home "nl-oc-a"))) "another build's copy reads from this build's folder")
          (is (equal (in-home "nl-oc-b") (own (in-home "nl-oc-b"))) "this build's copy stands")
          (is (equal (in-home "mine") (own (in-home "mine"))) "the operator's folder stands")
          (is (equal (in-home "nl-oc-b") (nlk::own-cell-directory (in-home "nl-oc-b") nil)) "and with nothing shipped, every entry stands"))))))

(deftest cells-a-failed-start-is-tried-again-while-its-gateway-serves (with-cell-fixture (root))
  ;; What refused a start may be gone a moment later -- a network not up at
  ;; boot, an install another build pointed elsewhere -- and a channel left
  ;; down until a relaunch answers nobody. The gateway tries again on a
  ;; capped backoff, says the refusal once, and stops once the start lands,
  ;; the gateway goes, or the refusal is the config's.
  (write-peripheral-folder
   root "nl-retry" :exports '("*ready*" "*tries*") :forms '("(defvar *ready* nil)" "(defvar *tries* 0)")
   :start "(declare (ignore config)) (incf *tries*)
  (unless *ready* (error \"nl-retry refuses: not ready\")) (lambda () nil)")
  (write-peripheral-folder
   root "nl-retry-config" :exports '("*tries*") :forms '("(defvar *tries* 0)")
   :start "(declare (ignore config)) (incf *tries*)
  (nlk:config-error \"nl-retry-config: no token\")")
  (let* ((records (load-cell-fixture root))
         (record (find "nl-retry" records :key #'nlk:cell-name :test #'string=))
         (config (find "nl-retry-config" records :key #'nlk:cell-name :test #'string=))
         (ready (uiop:find-symbol* '#:*ready* '#:nl-retry))
         (tries (uiop:find-symbol* '#:*tries* '#:nl-retry))
         (config-tries (uiop:find-symbol* '#:*tries* '#:nl-retry-config))
         (said '()))
    (flet ((settle (predicate) (loop repeat 200 until (funcall predicate) do (sleep 0.01))))
      (with-saved-globals ((nle::*gateway-handler* :a-serving-gateway)
                           (nle::*cell-retry-seconds* '(0.02 0.05)))
        (with-stubbed-fdefinitions ((nle::notice (text &rest args)
                                      (declare (ignore args))
                                      (when text (push text said))
                                      text))
          (nlk:with-cleanup ((setf nle::*gateway-handler* nil)
                             (nle::stop-cells))
            (nle::start-cells)
            (is (null record.started-p) "the first start refused")
            (settle (lambda () (>= (symbol-value tries) 4)))
            (is (>= (symbol-value tries) 4) "and is tried again")
            (is (= 1 (count-if (lambda (text) (search "nl-retry not started" text)) said)) "the refusal is said once, not at every try")
            (is (= 1 (symbol-value config-tries)) "a config refusal waits for the operator")
            (is (search "no token" (nlk:cell-refusal config)))
            (setf (symbol-value ready) t)
            (settle (lambda () record.started-p))
            (is record.started-p "the try after it healed started it")
            (is (null record.refusal) "its refusal is gone")
            (is (find "cell nl-retry: started" said :test #'equal) "and that is said")
            (let ((after (symbol-value tries)))
              (sleep 0.2)
              (is (= after (symbol-value tries)) "a started folder is not tried again"))))))
    ;; Outside a serving gateway nothing retries: a test's or a tool's start is one try.
    (nle::stop-cells)
    (setf (symbol-value ready) nil (symbol-value tries) 0)
    (nle::start-cells '("nl-retry"))
    (sleep 0.2)
    (is (= 1 (symbol-value tries)) "no gateway, no retry")
    (nle::stop-cells)))
