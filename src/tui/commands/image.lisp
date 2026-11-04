;;;; image.lisp --- /image: saved heaps from a shell or a room.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; `/image' lists the heaps this home keeps (images.lisp), `/image save
;;;; NAME' asks the organism the shell is attached to to keep its heap and go
;;;; on from it (image-save.lisp), `/image restore NAME' has the next launch
;;;; boot one, `/image delete NAME' removes one. A save stops the organism
;;;; for a moment: the shell says what it waits on instead of a gateway
;;;; error, and says the save once the organism answers from the new heap.

(in-package #:nodecode.evolved)

(defparameter *image-save-patience* 120
  "Seconds a shell waits for its organism to come back from a heap it saved.")

(defvar *expected-outage* nil
  "(TEXT . DEADLINE) while this shell waits out a gateway outage it asked
for -- a heap save -- with TEXT shown in place of the transport error, until
DEADLINE, an internal real time.")

(defun expected-outage-text ()
  "The words a shell shows for the outage it asked for, or NIL when it
asked for none or has waited past the deadline."
  (let ((expected *expected-outage*))
    (and expected (< (get-internal-real-time) (cdr expected)) (car expected))))

(defun image-words (args)
  "ARGS of /image as (VERB NAME): (\"list\" NIL) bare, else the two words,
or NIL when ARGS are not one of the verbs."
  (let ((words (split-words (or args ""))))
    (cond ((null words) (list "list" nil))
          ((and (= (length words) 2)
                (member (first words) '("save" "restore" "delete") :test #'string-equal))
           (list (string-downcase (first words)) (second words))))))

(defparameter +image-usage+ "Usage: /image, or /image save|restore|delete NAME")

(defun headless-image (args session-id)
  "/image without a shell -- a room, eval: the organism that runs this acts."
  (declare (ignore session-id))
  (destructuring-bind (&optional verb name) (image-words args)
    (handler-case
        (cond ((null verb) +image-usage+)
              ((equal verb "list") (saved-images-text))
              ((equal verb "save") (save-image name))
              ((equal verb "restore") (restore-image name))
              (t (delete-saved-image name)))
      (error (condition) (format nil "image ~a: ~a" (or name "") condition)))))

(defun running-image (model)
  "(NAME . SAVED) of the saved heap MODEL's organism runs from, NIL when it
runs from none or does not answer."
  (let ((answer (ignore-errors
                  (gateway-model-http-call model :get "/api/gateway/images" nil :read-timeout 5))))
    (and answer
         (let ((name (json-value answer :string "running")))
           (and name (cons name (json-value answer :integer "running_saved")))))))

(defun await-saved-image (model name since)
  "Wait for MODEL's organism to answer from heap NAME saved at or after
SINCE, a universal time. => its manifest, or NIL past *IMAGE-SAVE-PATIENCE*."
  (loop with deadline = (+ (get-internal-real-time)
                           (* *image-save-patience* internal-time-units-per-second))
        while (< (get-internal-real-time) deadline)
        do (let ((running (running-image model)))
             (when (and running (equal (car running) name) (>= (or (cdr running) 0) since))
               (return (saved-image-manifest name))))
           (sleep 0.5)))

(defun save-image-from-shell (model name)
  "The shell's /image save NAME, off the loop: asked of the organism, then
waited out until it answers from the heap it kept. => the words to show."
  (let ((since (get-universal-time)))
    (setf *expected-outage*
          (cons (format nil "image: keeping the organism's heap as ~a…" name)
                (+ (get-internal-real-time) (* *image-save-patience* internal-time-units-per-second))))
    (unwind-protect
         (with-handlers ((gateway-http-error (refusal)
                           (format nil "image ~a not saved: ~a" name (gateway-http-error-detail refusal))))
           (gateway-model-http-call model :post "/api/gateway/images/save" (json-object "name" name))
           (let ((manifest (await-saved-image model name since)))
             (if manifest
                 (format nil "image ~a kept: ~d MB, this organism runs from it now; /image restore ~a boots it again later"
                         name (megabytes (or (json-value manifest :integer "bytes") 0)) name)
                 (format nil "image ~a: the organism has not come back from the save within ~d s; /image says what was kept"
                         name *image-save-patience*))))
      (setf *expected-outage* nil))))

(defun slash-image-op (model args)
  "/image on a shell: the listing and the verbs over the gateway of the
organism the shell is attached to; a delete is this home's directory."
  (destructuring-bind (&optional verb name) (image-words args)
    (flet ((ask (placeholder thunk) (notice-off-loop model placeholder thunk))
           (answered (method path body)
             (with-handlers ((gateway-http-error (refusal)
                               (format nil "image: ~a" (gateway-http-error-detail refusal))))
               (answer-text (gateway-model-http-call model method path body)))))
      (cond ((null verb) +image-usage+)
            ((equal verb "list")
             (ask "image: reading the saved heaps…"
                  (lambda () (string-right-trim '(#\Newline) (answered :get "/api/gateway/images" nil)))))
            ((equal verb "save")
             (ask (format nil "image: asking the organism to keep its heap as ~a…" name)
                  (lambda () (save-image-from-shell model name))))
            ((equal verb "restore")
             (ask (format nil "image: asking for ~a at the next launch…" name)
                  (lambda () (answered :post "/api/gateway/images/restore" (json-object "name" name)))))
            (t (handler-case (delete-saved-image name)
                 (error (condition) (format nil "image ~a: ~a" name condition))))))))
