;;;; image.lisp --- /image: the saved heaps as a picker, no words typed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Bare `/image', and bare `nodecode image' in a terminal (the shell opened
;;;; on it), open one list: a row that keeps this organism's heap now, a row
;;;; per heap this home keeps (images.lisp), and the folder itself. Enter on
;;;; the save row asks a name, a dated one already in the field; Enter on a
;;;; heap switches the organism to it, on the folder back to the folder;
;;;; Delete or Ctrl+X removes a heap after a confirm. A save or a switch stops
;;;; the organism for a moment: the shell says what it waits on instead of a
;;;; gateway error, and says what came of it once the organism answers.

(in-package #:nodecode.evolved)

(defparameter *image-save-patience* 120
  "Seconds a shell waits for its organism to come back from a save or a switch.")

(defvar *expected-outage* nil
  "(TEXT . DEADLINE) while this shell waits out a gateway outage it asked
for -- a save, a switch -- with TEXT shown in place of the transport error,
until DEADLINE, an internal real time.")

(defun expected-outage-text ()
  "The words a shell shows for the outage it asked for, or NIL when it
asked for none or has waited past the deadline."
  (let ((expected *expected-outage*))
    (and expected (< (get-internal-real-time) (cdr expected)) (car expected))))

(defparameter +image-save-action+ :save-image
  "The picker's save row, as its row value: no heap carries it, a name is a string.")

(defparameter +image-folder-action+ :folder
  "The picker's folder row: the organism as the folder loads it, no heap.")

;;; --- what the picker lists ----------------------------------------------------

(defun image-picker-rows (&key running)
  "The picker's rows: keep this organism now, every heap this home keeps
(RUNNING, the one the organism runs from, marked), then the folder."
  (let ((pending (restore-pointer)))
    (append
     (list (make-picker-row
            :label "Save this organism now"
            :mark "+" :mark-tone :accent
            :detail (format nil "keep its heap as it stands, values and all, as ~a" (dated-image-name))
            :value +image-save-action+))
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
                                                  (and refusal (if (search "was saved by" refusal)
                                                                   "another build: not switchable"
                                                                   refusal)))))
                    :value name))
     (list (make-picker-row
            :label "The folder"
            :mark (if (and running (eq running :folder)) "*" " ")
            :mark-tone (if (eq running :folder) :accent :muted)
            :detail (if (eq running :folder)
                        "the cells as their files say, no saved heap  ·  running now"
                        "the cells as their files say, no saved heap: what a plain launch runs")
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
  "A name for a heap kept now: its date and minute, with -2, -3... when
this home already keeps one by that name."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time time)
    (declare (ignore s))
    (let ((base (format nil "~d-~2,'0d-~2,'0d-~2,'0d~2,'0d" y mo d h mi)))
      (loop for n from 1
            for name = (if (= n 1) base (format nil "~a-~d" base n))
            unless (probe-file (saved-image-directory name)) return name))))

(defun running-image (model)
  "What MODEL's organism runs from: a heap's NAME, :FOLDER, or NIL when it
does not answer. The second value is when that heap was saved."
  (let ((answer (ignore-errors
                  (gateway-model-http-call model :get "/api/gateway/images" nil :read-timeout 5))))
    (cond ((null answer) nil)
          ((json-value answer :string "running")
           (values (json-value answer :string "running")
                   (json-value answer :integer "running_saved")))
          (t :folder))))

;;; --- the picker ----------------------------------------------------------------

(defun open-image-picker (model &key seat notice running)
  "Bare /image: the saved heaps on a picker. RUNNING, when already known,
marks the one the organism runs from; otherwise it is asked off the loop and
the marks land when it answers."
  (with-slash-notice ("image")
    (open-picker-dialog
     model "Images" (image-picker-rows :running running)
     (lambda (model value)
       (cond ((eql value +image-save-action+) (open-image-save-field model))
             ((eql value +image-folder-action+) (switch-from-shell model nil))
             (t (switch-from-shell model value))))
     :kind :images
     :searchable t
     :context " Saved heaps"
     :subtitle "this organism's heap kept whole, on this machine only"
     :search-placeholder "Search images"
     :empty-label "No images"
     :no-match-label "No matching images"
     :on-delete #'image-picker-delete
     :hint "enter save/switch  del/ctrl+x delete  esc close  up/down navigate  type search")
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

(defun open-image-save-field (model)
  "The save row's Enter: one field for the heap's name, a dated one in it."
  (open-picker-dialog
   model "Images" '() nil
   :kind :images
   :context " Save"
   :subtitle "letters, digits, . _ - : the name to boot it by later"
   :input-label "Name"
   :input (dated-image-name)
   :input-context "ENTER keeps this organism's heap under that name and runs on from it; Esc goes back to the list"
   :hint "enter save  esc back to the list"
   :on-input (lambda (model text) (image-save-field-input model text)))
  nil)

(defun image-save-field-input (model text &aux (name (and text (string-trim " " text))))
  "ENTER or ESC on the name field: ESC (TEXT NIL) goes back to the list;
ENTER saves under the name, a refusal keeping the field open and saying why."
  (cond ((null text) (open-image-picker model) nil)
        ((image-name-refusal name)
         (setf (picker-dialog-input-context model.dialog) (image-name-refusal name))
         nil)
        (t (setf model.dialog nil)
           (save-from-shell model name))))

(defun image-picker-delete (model value &aux (list model.dialog))
  "Delete or Ctrl+X on a heap's row: confirm, then remove it and land back
on the list. The save and folder rows are no heap: nothing happens."
  (when (stringp value)
    (flet ((back (&optional notice)
             ;; The search found the row just removed: the list comes back whole.
             (setf (picker-dialog-rows list)
                   (image-picker-rows :running (let ((row (find "*" (picker-dialog-rows list)
                                                               :key #'picker-row-mark :test #'equal)))
                                                 (and row (let ((value (picker-row-value row))) (if (stringp value) value :folder)))))
                   (picker-dialog-search list) ""
                   model.dialog list)
             (forget-picker-visible-memo list)
             (when notice (show-status model notice))
             nil))
      (open-picker-dialog
       model "Delete image"
       (list (make-picker-row :label "Cancel" :value :cancel)
             (make-picker-row :label (format nil "Delete ~a" value) :value :delete))
       (lambda-ignoring (model answer)
         (back (when (eq answer :delete)
                 (handler-case (delete-saved-image value)
                   (error (condition) (format nil "image ~a: ~a" value condition))))))
       :kind :images
       :context (format nil " ~a" value)
       :subtitle "Removes this saved heap from this machine; the folder and the running organism are untouched."
       :hint "enter choose  esc cancel"
       :on-cancel (lambda (model) (declare (ignore model)) (back)))))
  nil)

;;; --- a save or a switch, waited out ---------------------------------------------

(defun await-running (model target since)
  "Wait for MODEL's organism to answer from TARGET -- a heap's name saved at
or after SINCE, or :FOLDER -- for at most *IMAGE-SAVE-PATIENCE*. => T once
it does."
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
  "Keep the attached organism's heap as NAME, off the loop, and say what
came of it once the organism answers from that heap. => the placeholder."
  (notice-off-loop
   model (format nil "image: keeping the organism's heap as ~a…" name)
   (lambda (&aux (since (get-universal-time)))
     (while-expecting-outage
      (format nil "image: keeping the organism's heap as ~a…" name)
      (lambda ()
        (with-handlers ((gateway-http-error (refusal)
                          (format nil "image ~a not saved: ~a" name (gateway-http-error-detail refusal))))
          (gateway-model-http-call model :post "/api/gateway/images/save" (json-object "name" name))
          (if (await-running model name since)
              (let ((manifest (saved-image-manifest name)))
                (format nil "image ~a kept: ~d MB, this organism runs from it now; /image switches back to it later"
                        name (megabytes (or (and manifest (json-value manifest :integer "bytes")) 0))))
              (format nil "image ~a: the organism has not come back from the save within ~d s; /image says what was kept"
                      name *image-save-patience*))))))))

(defun switch-from-shell (model name)
  "Switch the attached organism to heap NAME, or to the folder when NAME is
NIL, off the loop, and say so once it answers from there. => the placeholder."
  (let ((what (if name (format nil "image ~a" name) "the folder")))
    (notice-off-loop
     model (format nil "image: switching the organism to ~a…" what)
     (lambda ()
       (while-expecting-outage
        (format nil "image: switching the organism to ~a…" what)
        (lambda ()
          (with-handlers ((gateway-http-error (refusal)
                            (format nil "image: ~a" (gateway-http-error-detail refusal))))
            (gateway-model-http-call model :post "/api/gateway/images/switch"
                                     (json-object "name" (or name :null)))
            (if (await-running model (or name :folder) nil)
                (format nil "the organism runs from ~a now" what)
                (format nil "the organism has not come back on ~a within ~d s" what *image-save-patience*)))))))))

;;; --- the slash ------------------------------------------------------------------

(defun slash-image-op (model args)
  "/image on a shell: the picker. Words after it are not read: every act
is a row."
  (declare (ignore args))
  (open-image-picker model))

(defun headless-image (args session-id)
  "/image without a shell -- a room, eval: the list, since a row to pick
needs a terminal. A room that names a verb is answered as the shell's rows
would act: save [NAME], switch NAME|folder, delete NAME."
  (declare (ignore session-id))
  (let ((words (split-words (or args ""))))
    (handler-case
        (cond ((null words) (saved-images-text))
              ((and (string-equal (first words) "save") (<= (length words) 2))
               (save-image (or (second words) (dated-image-name))))
              ((and (string-equal (first words) "switch") (= (length words) 2))
               (switch-image (if (string-equal (second words) "folder") nil (second words))))
              ((and (string-equal (first words) "delete") (= (length words) 2))
               (delete-saved-image (second words)))
              (t "/image lists the saved heaps; a shell's /image is a picker"))
      (error (condition) (format nil "image: ~a" condition)))))
