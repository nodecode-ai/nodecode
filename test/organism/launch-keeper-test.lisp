;;;; launch-keeper-test.lisp --- the launch image kept in step with the folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two gates for the launch cache's own hygiene: a process that lives a while
;;;; asks for the image its successor will exec, and the layer cache of one
;;;; build is not what another build's compile deletes.

(in-package #:nodecode.test)

(defmacro with-keeper ((spawns &rest prover) &body body &aux (fixture (gensym "PROVER")))
  "BODY with WITH-PROVER-FIXTURE's trial child (PROVER its options) the organism
command, the keeper's look and proved key cleared, and every builder the keeper
spawns pushed onto SPAWNS instead of run."
  `(with-prover-fixture (,fixture ,@prover)
     (let ((,spawns '()))
       (with-saved-globals ((nlk::*launch-image-stamp* nil) (nlk::*proved-key* nil))
         (with-stubbed-fdefinition (nlk::spawn-detached (command log)
                                    (push command ,spawns) command)
           ,@body)))))

(deftest warm-keeper-asks-for-an-image-the-folder-has-moved-past (with-warm-fixture (key root cache))
  ;; The keeper is what keeps a launch cheap: a process that is going to live a
  ;; while asks for the image the next launch will exec, instead of every launch
  ;; paying the whole load until one of them happens to leave an image behind.
  ;; Since generation.lisp it proves the folder first: the image is built from
  ;; the same proof the manifest is sealed on.
  (with-keeper (spawns)
    (is (equal key (nlk::tend-launch-image)) "a folder with no image asks for one")
    (is (equal '(1) (mapcar #'nlk::manifest-number (nlk:manifests))) "sealed as 1 before the build")
    (is (await () spawns) "and the builder is spawned")
    (is (= 1 (length spawns)) "exactly one")
    (write-fake-image (nlk::warm-image-path key))
    (is (null (nlk::tend-launch-image)) "an image that answers holds the keeper")
    (sleep 0.05)
    (is (= 1 (length spawns)) "and spawns nothing")))

(deftest warm-keeper-asks-no-image-for-a-folder-that-does-not-prove (with-warm-fixture (key root cache))
  ;; A red verdict: no manifest, the key refused and marked, the image a
  ;; build left for it taken down -- a launch then loads the folder cold and
  ;; the boot line says it is unsealed.
  (with-keeper (spawns :score 0 :report +red-report+)
    (write-fake-image (nlk::warm-image-path key))
    (is (null (nlk::tend-launch-image)) "a folder that does not prove is asked no image")
    (is (null (nlk:manifests)) "and seals nothing")
    (is (nlk:refused-key-p key) "the marker names the key")
    (is (null (probe-file (nlk::warm-image-path key))) "the image that stood for it is gone")
    (is (probe-file (nlk::warm-failed-path key)) "and the key is marked failed")
    (sleep 0.05)
    (is (null spawns) "no builder")
    (is (search "RUN" (board-line "generation")) "the board names the override")))

(deftest warm-keeper-keys-the-folder-again-only-when-it-moves (with-warm-fixture (key root cache))
  ;; A look at a folder that has not moved is its stamp alone; without this,
  ;; every look would key it again.
  (with-keeper (spawns)
    (write-fake-image (nlk::warm-image-path key))
    (is (null (nlk::tend-launch-image)) "an image that answers holds the keeper")
    (let ((looked nlk::*launch-image-stamp*))
      (is (equal key (cdr looked)) "the look kept the key it took")
      (nlk::tend-launch-image)
      (is (eq looked nlk::*launch-image-stamp*) "a folder that has not moved is not keyed again")
      (is (null spawns))
      (write-temp-file (merge-pathnames "nl-warm/fresh.lisp" root) "")
      (let ((moved (nlk::tend-launch-image)))
        (is (and moved (not (equal key moved))) "a new file is a new key, and its image asked for")
        (is (equal moved (cdr nlk::*launch-image-stamp*)))
        (is (await () spawns) "the builder is spawned")))))

(deftest the-folder-walk-conses-under-a-kilobyte-a-file (with-temp-trees ((root "walk-cost")))
  ;; The keeper walks the folder every five seconds for the life of a
  ;; process: through a pathname per entry that was ~2.4 KB a file, and on
  ;; the operator's 824 files the walk alone outgrew an idle interval.
  (dotimes (i 100)
    (write-temp-file (merge-pathnames (format nil "d~d/f~d.lisp" (mod i 10) i) root) "x"))
  (let ((files 0))
    (nlk::map-visible-file-tree root (lambda (path stat) (declare (ignore path stat)) (incf files)))
    (let ((before (sb-ext:get-bytes-consed)))
      (nlk::map-visible-file-tree root (lambda (path stat) (declare (ignore path stat))))
      (is (= 100 files) "every file is visited")
      (is (< (- (sb-ext:get-bytes-consed) before) (* 100 1024)) "under a kilobyte a file"))))

(deftest the-folder-walk-follows-a-link-and-cuts-its-cycle (with-temp-trees ((root "walk-cycle")))
  ;; A link back up the tree is followed and cut where it comes round: the
  ;; walk knows a folder by what it is, not by the path it came in by -- on
  ;; Windows by the volume and file index its handle carries, where stat's
  ;; inode is 0 for every file. A junction there, a symbolic link elsewhere.
  (write-temp-file (merge-pathnames "a/f.lisp" root) "x")
  (write-temp-file (merge-pathnames "b/g.lisp" root) "x")
  (let ((up (string-right-trim "/\\" (uiop:native-namestring (merge-pathnames "a/up/" root))))
        (top (string-right-trim "/\\" (uiop:native-namestring root)))
        (seen '()))
    #-win32 (sb-posix:symlink top up)
    #+win32 (uiop:run-program (list "cmd" "/c" "mklink" "/J" up top) :output nil)
    ;; The link goes first: a tree removed through a link back up it is
    ;; removed again under its own name.
    (nlk:with-cleanup (#-win32 (sb-posix:unlink up)
                       #+win32 (uiop:run-program (list "cmd" "/c" "rmdir" up) :output nil))
      (is (probe-file (merge-pathnames "a/up/b/g.lisp" root)) "the link leads back up")
      (nlk::map-visible-file-tree root (lambda (path stat) (declare (ignore stat))
                                         (push (subseq path (length (namestring root))) seen)))
      (is (equal '("a/f.lisp" "b/g.lisp") (sort seen #'string<)) "each file once, the cycle cut"))))

(deftest cells-layer-cache-keeps-the-newest-two-stamps (with-cell-fixture (root))
  ;; A warm update runs two builds at once -- the one serving and the one its
  ;; successor boots -- and without this, a compile under either would delete
  ;; the other's whole directory, so the next launch of whichever lost would
  ;; recompile every layer file. The running stamp and the newest beside it
  ;; stay; older ones go.
  (with-temp-directory (cache "layer-cache-stamps")
    ;; AGE is negative for the older stamps.
    (flet ((stamp-dir (stamp age)
             (backdate (ensure-directories-exist (merge-pathnames (format nil "~a/" stamp) cache))
                       age)))
      (let ((oldest (stamp-dir "stamp-oldest" -300))
            (middle (stamp-dir "stamp-middle" -200))
            (newest (stamp-dir "stamp-newest" -100)))
        (nlk::prune-stamp-cache "stamp-middle" cache)
        (is (null (probe-file oldest)) "a stamp two behind is pruned")
        (is (probe-file middle) "the running stamp stays")
        (is (probe-file newest) "and the newest beside it stays")
        (nlk::prune-stamp-cache "stamp-newest" cache)
        (is (probe-file middle) "the one behind the running stamp stays")
        (is (probe-file newest) "and the running stamp with it")))))

(deftest cells-layer-cache-spares-a-stamp-a-load-marked-in-use (with-cell-fixture (root))
  ;; A third build alive on the machine -- a shell left open on an older
  ;; release -- loads from a directory two behind the pruning build's: a load
  ;; marks its stamp in use, and the mark spares it for the hour; an older
  ;; mark keeps nothing.
  (with-temp-directory (cache "layer-cache-in-use")
    (nlk::note-stamp-in-use cache "stamp-reading")
    (nlk::note-stamp-in-use cache "stamp-left")
    (backdate (nlk::stamp-in-use-marker (merge-pathnames "stamp-left/" cache)) -7200)
    (flet ((stamp-dir (stamp age)
             (backdate (ensure-directories-exist (merge-pathnames (format nil "~a/" stamp) cache))
                       age)))
      (let ((reading (stamp-dir "stamp-reading" -400))
            (left (stamp-dir "stamp-left" -300))
            (middle (stamp-dir "stamp-middle" -200))
            (newest (stamp-dir "stamp-newest" -100)))
        (nlk::prune-stamp-cache "stamp-middle" cache)
        (is (probe-file reading) "a stamp a load marked within the hour stays")
        (is (null (probe-file left)) "a mark two hours old spares nothing")
        (is (probe-file middle) "the running stamp stays")
        (is (probe-file newest) "and the newest beside it")))))
