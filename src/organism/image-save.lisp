;;;; image-save.lisp --- /image save: the running organism kept, and going on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; SAVE-IMAGE is what `/image save NAME', `nodecode image save NAME' and
;;;; (save-image NAME) ask of the organism that serves (images.lisp's route).
;;;; It waits for no turn to be in flight -- the turn that asks is usually
;;;; the one running, and its answer has to land first -- then has the
;;;; serve's owner let the store go and become the image (SERVE-NEXT :IMAGE,
;;;; BECOME-SAVED-IMAGE): the heap dumped by a fork into images.lisp's dump
;;;; path, sealed there, and exec'd in place (heap-image.lisp).

(in-package #:nodecode.evolved)

(defvar *image-wait* nil
  "The thread holding a heap save for the first moment no turn is in flight,
or NIL.")

(defun image-save-refusal (name)
  "Why this organism cannot save its heap as image NAME, or NIL when it can."
  ;; Every way an organism serves -- its unit's, a headless serve, a shell's --
  ;; parks its owner on SERVE-NEXT, and an exec in place keeps the pid a unit
  ;; watches, so all three save the same way.
  (or (image-name-refusal name)
      #+win32 "Windows has no fork: a heap is saved on Linux and macOS"
      (unless *serving*
        "only the organism that serves saves its heap; this process is not it")
      (unless (nlk:organism-binary)
        "this image was not started from an installed binary, so it cannot come back from a save")))

(defun save-image (name)
  "Save this organism's heap -- every value, closure and definition it holds
now -- as image NAME under ~/.nodecode/images/, once no turn is in flight,
and go on from that image in place: same pid, the attached shells dialing it
again as after a restart. => words; signals the refusal where it cannot."
  (let ((refusal (image-save-refusal name)))
    (when refusal (error "~a" refusal))
    ;; Asked at once and every half second, after the answer has left: the
    ;; process that answers is the one about to stop.
    (unless (and *image-wait* (bt2:thread-alive-p *image-wait*))
      (setf *image-wait*
            (spawn "image-save-idle"
              (sleep 0.3)
              (loop while (ignore-errors (nlk:any-active-turn-p))
                    do (sleep 0.5))
              (serve-image name))))
    (format nil "saving image ~a once no turn is in flight: the organism stops for a moment, ~
                 keeps its heap in ~a and goes on from it"
            name (home-abbreviated (namestring (saved-image-directory name))))))

(defun become-saved-image (name &key lease)
  "On the serve's owner, its gateway stopped and its store let go: dump this
heap as image NAME (NLK::FORK-AND-DUMP into PREPARE-SAVED-IMAGE's path), seal
it, and exec it in place, LEASE -- the store's lock, held through it all --
kept for the image to adopt. Returns only when it could not: (values NIL TEXT)."
  (handler-case
      (let* ((key (ignore-errors (nlk::folder-key)))
             (generation (ignore-errors (nlk::manifest-number (nlk::sealed-generation))))
             (build (effective-version))
             (dump (prepare-saved-image name))
             (started (get-internal-real-time)))
        (multiple-value-bind (dumped why)
            (nlk::fork-and-dump dump (list :name name :saved (get-universal-time)
                                           :build build :folder-key key)
                                key)
          (unless dumped
            (ignore-errors (delete-file dump))
            (return-from become-saved-image (values nil why))))
        (seal-saved-image name :folder-key key :generation generation :build build)
        (format t "~&image: saved ~a in ~,1f s; running it in place~%"
                name (/ (- (get-internal-real-time) started) internal-time-units-per-second))
        (finish-output)
        (values nil (format nil "the kernel refused the exec of image ~a: ~a"
                            name (nlk::exec-into-image (saved-image-path name) :lease lease))))
    (error (condition) (values nil (princ-to-string condition)))))

;;; --- a switch: another heap, or the folder, in this organism's place ----------

(defun switch-refusal (name)
  "Why this organism cannot run heap NAME -- NIL: the folder -- in its own
place, or NIL when it can."
  (or (and name (saved-image-refusal name))
      #+win32 "Windows has no exec: restore the image, then start nodecode again"
      (unless *serving*
        "only the organism that serves switches; this process is not it")
      (unless (nlk:organism-binary)
        "this image was not started from an installed binary, so it cannot switch")
      (and (null name) (null (getf nlk::*saved-image* :name))
           "this organism already runs the folder")
      (and *image-wait* (bt2:thread-alive-p *image-wait*)
           "a save or a switch already waits for the turn in flight to end")))

(defun switch-image (name)
  "Run heap NAME -- NIL: the folder, as the installed binary loads it -- in
this organism's place once no turn is in flight: same pid, no restart, the
store's lease held across the exec, so no shell or second organism takes
the store between (SERVE-SWITCH). A restore pointer left for the next launch
gives way to it. => words; signals the refusal where it cannot."
  (when-let (refusal (switch-refusal name)) (error "~a" refusal))
  (ignore-errors (delete-file (restore-pointer-path)))
  ;; Asked at once and every half second, after the answer has left: the
  ;; process that answers is the one about to be replaced.
  (setf *image-wait*
        (spawn "image-switch-idle"
          (sleep 0.3)
          (loop while (ignore-errors (nlk:any-active-turn-p))
                do (sleep 0.5))
          (serve-switch (or name :folder))))
  (format nil "switching to ~a once no turn is in flight: the same organism, its store held throughout"
          (if name (format nil "image ~a" name) "the folder")))
