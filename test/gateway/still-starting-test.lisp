;;;; still-starting-test.lisp --- A shell whose own boot is slow says what it waits on.
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A layer file whose initform called the network on every load held a
;;;; launch's folder for twenty seconds, twice -- in the shell, then in the
;;;; organism it started -- and past its threshold the shell said, in red, that
;;;; the store advertised no gateway and asked whether an owner served, while
;;;; its own boot was still on its way. The shell now names the file.

(in-package #:nodecode.test)

(deftest a-layer-file-is-named-while-it-loads ()
  (with-cell-fixture (root)
    (write-layer-folder root "nl-loading"
                        '(("loading-probe" . "(defparameter *loading-probe* (copy-tree nodecode.kernel::*layer-file-loading*))")))
    (load-cell-fixture root)
    (let ((seen (symbol-value (uiop:find-symbol* "*LOADING-PROBE*" '#:nodecode.evolved))))
      (is (equal "nl-loading" (first seen)) "the cell the loading file is in")
      (is (equal "loading-probe.lisp" (second seen)) "the file loading now, by its name")
      (is (integerp (cddr seen)) "when it started, to say how long it has held the boot"))
    (is (null nlk::*layer-file-loading*) "nothing is named once the load is over")))

(deftest a-stalled-boot-names-what-it-waits-on ()
  (let* ((log #p"/tmp/nl-home/history.db.stdio.log")
         (stalled (nle::still-starting-text
                   31 (list* "my-layer" "_slow_.lisp"
                             (- (get-internal-real-time) (* 19 internal-time-units-per-second)))
                   log))
         (serving (nle::still-starting-text 30 nil log)))
    (is (search "still starting (31 s)" stalled) "how long the shell has waited")
    (is (search "my-layer/_slow_.lisp has been loading for 19 s" stalled) "the layer file holding the boot, and for how long")
    (is (search "the organism has not served yet" serving) "past the folder, the organism it started")
    (is (search "history.db.stdio.log" serving) "and where that organism writes")
    (is (notany (lambda (text) (search "owner serving" text)) (list stalled serving)) "a boot on its way is never asked whether an owner serves")))
