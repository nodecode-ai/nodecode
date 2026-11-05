;;;; snapshot.lisp --- /snapshot takes one, /snapshots lists them: no words typed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A snapshot is this organism's heap kept whole on this machine
;;;; (images.lisp): values, closures, definitions no file holds. Bare
;;;; `/snapshot' takes one now under a dated name and runs on from it. Bare
;;;; `/snapshots', and bare `nodecode snapshots' in a terminal (the shell
;;;; opened on it), open one list: a row per snapshot this home keeps, newest
;;;; first, and "Fresh from files" -- the cells as their files say, what a
;;;; plain launch runs. Enter switches the organism to a row, F2 renames a
;;;; snapshot, Delete or Ctrl+X removes one after a confirm. A snapshot or a
;;;; switch stops the organism for a moment: the shell says what it waits on
;;;; instead of a gateway error, and says what came of it once the organism
;;;; answers.

(in-package #:nodecode.evolved)

(defparameter *image-save-patience* 120
  "Seconds a shell waits for its organism to come back from a snapshot or a switch.")

(defvar *expected-outage* nil
  "(TEXT . DEADLINE) while this shell waits out a gateway outage it asked
for -- a snapshot, a switch -- with TEXT shown in place of the transport
error, until DEADLINE, an internal real time.")

(defun expected-outage-text ()
  "The words a shell shows for the outage it asked for, or NIL when it
asked for none or has waited past the deadline."
  (let ((expected *expected-outage*))
    (and expected (< (get-internal-real-time) (cdr expected)) (car expected))))

(defparameter +image-folder-action+ :folder
  "The list's \"Fresh from files\" row: the organism as the folder loads it, no snapshot.")

;;; --- what the list shows -----------------------------------------------------

(defun image-picker-rows (&key running)
  "The list's rows: every snapshot this home keeps, newest first (RUNNING,
the one the organism runs from, marked), then fresh from files."
  (let ((pending (restore-pointer)))
    (append
     (loop for name in (sort (saved-image-names) #'string>
                             :key (lambda (name)
                                    (let ((manifest (saved-image-manifest name)))
                                      (or (and manifest (json-value manifest :string "saved_at")) ""))))
           for manifest = (saved-image-manifest name)
           for refusal = (saved-image-refusal name)
           collect (make-picker-row
                    :label name
                    :mark (if (equal name running) "*" " ")
                    :mark-tone (if (equal name running) :accent :muted)
                    :detail (format nil "~{~a~^  ·  ~}"
                                    (remove nil
                                            (list (and manifest (saved-minute-text (json-value manifest :string "saved_at")))
                                                  (and manifest (format nil "~d MB" (megabytes (or (json-value manifest :integer "bytes") 0))))
                                                  (and manifest (json-value manifest :string "build"))
                                                  (and (equal name running) "running now")
                                                  (and (equal name pending) "the next launch boots it")
                                                  (and refusal (if (search "was taken by" refusal)
                                                                   "another build: not switchable"
                                                                   refusal)))))
                    :value name))
     (list (make-picker-row
            :label "Fresh from files"
            :mark (if (and running (eq running :folder)) "*" " ")
            :mark-tone (if (eq running :folder) :accent :muted)
            :detail (if (eq running :folder)
                        "the cells as their files say, no snapshot  ·  running now"
                        "the cells as their files say, no snapshot: what a plain launch runs")
            :value +image-folder-action+)))))

(defun saved-minute-text (iso)
  "ISO, a manifest's saved_at, as the local date and minute it names."
  (let ((time (ignore-errors (nlk::iso-universal iso))))
    (if time
        (multiple-value-bind (s mi h d mo y) (decode-universal-time time)
          (declare (ignore s))
          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h mi))
        (or iso ""))))

(defun dated-image-name (&optional (time (get-universal-time)))
  "A name for a snapshot taken now: its date and minute, with -2, -3... when
this home already keeps one by that name."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time time)
    (declare (ignore s))
    (let ((base (format nil "~d-~2,'0d-~2,'0d-~2,'0d~2,'0d" y mo d h mi)))
      (loop for n from 1
            for name = (if (= n 1) base (format nil "~a-~d" base n))
            unless (probe-file (saved-image-directory name)) return name))))

(defun running-image (model)
  "What MODEL's organism runs from: a snapshot's NAME, :FOLDER, or NIL when
it does not answer. The second value is when that snapshot was taken."
  (let ((answer (ignore-errors
                  (gateway-model-http-call model :get "/api/gateway/images" nil :read-timeout 5))))
    (cond ((null answer) nil)
          ((json-value answer :string "running")
           (values (json-value answer :string "running")
                   (json-value answer :integer "running_saved")))
          (t :folder))))

(defun running-mark (dialog)
  "What DIALOG's rows mark as running: a snapshot's name, :FOLDER, or NIL."
  (let ((row (find "*" (picker-dialog-rows dialog) :key #'picker-row-mark :test #'equal)))
    (and row (let ((value (picker-row-value row))) (if (stringp value) value :folder)))))

;;; --- the list ------------------------------------------------------------------

(defun open-image-picker (model &key seat notice running)
  "Bare /snapshots: the snapshots on a list. RUNNING, when already known,
marks the one the organism runs from; otherwise it is asked off the loop
and the marks land when it answers. SEAT, a row's value, is highlighted."
  (with-slash-notice ("snapshots")
    (open-picker-dialog
     model "Snapshots" (image-picker-rows :running running)
     (lambda (model value)
       (if (eql value +image-folder-action+)
           (switch-from-shell model nil)
           (switch-from-shell model value)))
     :kind :images
     :searchable t
     :context " Snapshots"
     :subtitle "this organism frozen whole, on this machine only"
     :search-placeholder "Search snapshots"
     :empty-label "No snapshots: /snapshot takes one"
     :no-match-label "No matching snapshots"
     :on-delete #'image-picker-delete
     :on-rename #'image-picker-rename
     :hint "enter switch  f2 rename  del delete  esc close  up/down navigate  type search")
    (when seat (picker-reseat-selection model.dialog seat))
    (when notice (show-status model notice))
    (unless running
      (let ((dialog model.dialog))
        (off-loop model (lambda () (running-image model))
                  (lambda (model running)
                    (when (and running (eq model.dialog dialog))
                      (let ((at (picker-highlighted-value dialog)))
                        (setf (picker-dialog-rows dialog) (image-picker-rows :running running))
                        (picker-reseat-selection dialog at))
                      (render-state model)))))))
  nil)

(defun image-picker-rename (model dialog &aux (name (picker-highlighted-value dialog))
                                              (running (running-mark dialog)))
  "F2 on a snapshot's row: one field holding its name; Enter keeps it under
the name typed, Esc goes back to the list. Fresh from files is no snapshot:
nothing happens."
  (when (stringp name)
    (open-picker-dialog
     model "Snapshots" '() nil
     :kind :images
     :context (format nil " Rename ~a" name)
     :subtitle "letters, digits, . _ - : the name it is listed and switched by"
     :input-label "Name"
     :input name
     :input-context "ENTER renames it; Esc goes back to the list"
     :hint "enter rename  esc back to the list"
     :on-input (lambda (model text &aux (new (and text (string-trim " " text))))
                 (cond ((null text) (open-image-picker model :seat name :running running) nil)
                       ((image-name-refusal new)
                        (setf (picker-dialog-input-context model.dialog) (image-name-refusal new))
                        nil)
                       (t (handler-case
                              (let ((said (rename-saved-image name new)))
                                (open-image-picker model :seat new :notice said
                                                         :running (if (equal running name) new running)))
                            (error (condition)
                              (setf (picker-dialog-input-context model.dialog) (princ-to-string condition))))
                          nil)))))
  nil)

(defun image-picker-delete (model value &aux (list model.dialog))
  "Delete or Ctrl+X on a snapshot's row: confirm, then remove it and land
back on the list. Fresh from files is no snapshot: nothing happens."
  (when (stringp value)
    (flet ((back (&optional notice)
             ;; The search found the row just removed: the list comes back whole.
             (setf (picker-dialog-rows list) (image-picker-rows :running (running-mark list))
                   (picker-dialog-search list) ""
                   model.dialog list)
             (forget-picker-visible-memo list)
             (when notice (show-status model notice))
             nil))
      (open-picker-dialog
       model "Delete snapshot"
       (list (make-picker-row :label "Cancel" :value :cancel)
             (make-picker-row :label (format nil "Delete ~a" value) :value :delete))
       (lambda-ignoring (model answer)
         (back (when (eq answer :delete)
                 (handler-case (delete-saved-image value)
                   (error (condition) (format nil "snapshot ~a: ~a" value condition))))))
       :kind :images
       :context (format nil " ~a" value)
       :subtitle "Removes this snapshot from this machine; your files and the running organism are untouched."
       :hint "enter choose  esc cancel"
       :on-cancel (lambda (model) (declare (ignore model)) (back)))))
  nil)

;;; --- a snapshot or a switch, waited out -----------------------------------------

(defun await-running (model target since)
  "Wait for MODEL's organism to answer from TARGET -- a snapshot's name taken
at or after SINCE, or :FOLDER -- for at most *IMAGE-SAVE-PATIENCE*. => T
once it does."
  (loop with deadline = (+ (get-internal-real-time)
                           (* *image-save-patience* internal-time-units-per-second))
        while (< (get-internal-real-time) deadline)
        do (multiple-value-bind (running saved) (running-image model)
             (when (and running (equal running target)
                        (or (eq target :folder) (>= (or saved 0) (or since 0))))
               (return t)))
           (sleep 0.5)))

(defun while-expecting-outage (text thunk)
  "Call THUNK with TEXT standing for the gateway outage it brings."
  (setf *expected-outage*
        (cons text (+ (get-internal-real-time) (* *image-save-patience* internal-time-units-per-second))))
  (unwind-protect (funcall thunk) (setf *expected-outage* nil)))

(defun save-from-shell (model name)
  "Take a snapshot of the attached organism as NAME, off the loop, and say
what came of it once the organism answers from it. => the placeholder."
  (let ((waiting (format nil "snapshot: taking ~a…" name)))
    (notice-off-loop
     model waiting
     (lambda (&aux (since (get-universal-time)))
       (while-expecting-outage
        waiting
        (lambda ()
          (with-handlers ((gateway-http-error (refusal)
                            (format nil "snapshot ~a not taken: ~a" name (gateway-http-error-detail refusal))))
            (gateway-model-http-call model :post "/api/gateway/images/save" (json-object "name" name))
            (if (await-running model name since)
                (let ((manifest (saved-image-manifest name)))
                  (format nil "snapshot ~a taken: ~d MB, running from it now; /snapshots switches back to it later"
                          name (megabytes (or (and manifest (json-value manifest :integer "bytes")) 0))))
                (format nil "snapshot ~a: the organism has not come back within ~d s; /snapshots says what was kept"
                        name *image-save-patience*)))))))))

(defun switch-from-shell (model name)
  "Switch the attached organism to snapshot NAME, or to fresh from files
when NAME is NIL, off the loop, and say so once it answers from there.
=> the placeholder."
  (let* ((what (if name (format nil "snapshot ~a" name) "fresh from files"))
         (waiting (format nil "snapshots: switching to ~a…" what)))
    (notice-off-loop
     model waiting
     (lambda ()
       (while-expecting-outage
        waiting
        (lambda ()
          (with-handlers ((gateway-http-error (refusal)
                            (format nil "snapshots: ~a" (gateway-http-error-detail refusal))))
            (gateway-model-http-call model :post "/api/gateway/images/switch"
                                     (json-object "name" (or name :null)))
            (if (await-running model (or name :folder) nil)
                (if name
                    (format nil "running from snapshot ~a now" name)
                    "running fresh from files now")
                (format nil "the organism has not come back on ~a within ~d s" what *image-save-patience*)))))))))

;;; --- the slashes -----------------------------------------------------------------

(defun slash-snapshot-op (model args)
  "/snapshot on a shell: one taken now, under a dated name. Words after it
are not read: F2 on /snapshots renames it."
  (declare (ignore args))
  (save-from-shell model (dated-image-name)))

(defun slash-snapshots-op (model args)
  "/snapshots on a shell: the list. Words after it are not read: every act
is a row."
  (declare (ignore args))
  (open-image-picker model))

(defun headless-snapshot (args session-id)
  "/snapshot without a shell -- a room, eval: one taken now, dated."
  (declare (ignore args session-id))
  (handler-case (save-image (dated-image-name))
    (error (condition) (format nil "snapshot: ~a" condition))))

(defun headless-snapshots (args session-id)
  "/snapshots without a shell -- a room, eval: the list, since a row to pick
needs a terminal. A room that names a verb is answered as the shell's rows
would act: switch NAME|fresh, rename NAME NEW, delete NAME."
  (declare (ignore session-id))
  (let ((words (split-words (or args ""))))
    (handler-case
        (cond ((null words) (saved-images-text))
              ((and (string-equal (first words) "switch") (= (length words) 2))
               (switch-image (if (string-equal (second words) "fresh") nil (second words))))
              ((and (string-equal (first words) "rename") (= (length words) 3))
               (rename-saved-image (second words) (third words)))
              ((and (string-equal (first words) "delete") (= (length words) 2))
               (delete-saved-image (second words)))
              (t "/snapshots lists the snapshots; a shell's /snapshots is a list to pick from"))
      (error (condition) (format nil "snapshots: ~a" condition)))))
