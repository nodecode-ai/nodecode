;;;; lanes.lisp --- the composer lanes cells provide: a sigil and its rows. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the lanes cells register ------------------------------------------------
;;; Beside the builtin file lane, a cell can offer a lane of its own: a name,
;;; the sigil that opens it in the composer, and a function answering the
;;; candidates it offers now (the sessions a messaging cell can address, say).
;;; A shell reads the list at GET /api/gateway/lanes, as it reads the commands,
;;; and a lane's rows at POST {lane}: once per typed token, off the keystroke
;;; path, never per key. The rows are a preview; what a prompt means is what
;;; the cell decides when it is submitted. The lifecycle's COMMANDS_CHANGED
;;; cue tells shells the list moved.

(defvar *registered-lanes* '()
  "The composer lanes cells registered (REGISTER-COMPOSER-LANE), oldest first:
plists (:NAME :SIGIL :ROWS :DESCRIPTION :OWNER).")

(defparameter *composer-lane-row-limit* 512
  "The most rows one lane answers: a lane is a preview, not a listing.")

(defun register-composer-lane (owner name sigil rows &key description)
  "Offer the composer lane NAME, opened by the one-character string SIGIL,
whose ROWS -- a function designator of no arguments -- answers its candidates
now: each a string, or a list (TEXT DETAIL). OWNER is the key that takes it
back (UNREGISTER-COMPOSER-LANES). A name registered again is replaced.
=> the lane's plist."
  (check-type owner string)
  (unless (json-value name :text)
    (error "a composer lane needs a non-empty name"))
  (when (string-equal name "file")
    (error "the composer lane name ~s is the builtin file lane's" name))
  (unless (and (stringp sigil) (= 1 (length sigil))
               (let ((char (char sigil 0)))
                 (and (graphic-char-p char) (not (alphanumericp char))
                      (not (blank-char-p char)) (char/= char #\/))))
    (error "composer lane ~a: ~s is not one punctuation character" name sigil))
  (let ((lane (list :name name :sigil sigil :rows rows :owner owner
                    :description (or description ""))))
    (setf *registered-lanes*
          (append (remove name *registered-lanes*
                          :key (lambda (lane) (getf lane :name)) :test #'string-equal)
                  (list lane)))
    lane))

(defun unregister-composer-lanes (owner &aux (before (length *registered-lanes*)))
  "Take every lane OWNER registered out -- its cell is stopping. => how many
went."
  (setf *registered-lanes*
        (remove owner *registered-lanes* :key (lambda (lane) (getf lane :owner)) :test #'equal))
  (- before (length *registered-lanes*)))

(defun find-composer-lane (name)
  "The lane a cell registered as NAME, else NIL."
  (when (stringp name)
    (find name *registered-lanes* :key (lambda (lane) (getf lane :name)) :test #'string-equal)))

(defun composer-lane-rows (name &aux (lane (find-composer-lane name)))
  "NAME's rows now, as (TEXT . DETAIL) conses, at most
*COMPOSER-LANE-ROW-LIMIT*; a row neither a string nor a list starting with
one is dropped. The cell's own error goes up."
  (loop for row in (funcall (getf lane :rows))
        for text = (if (consp row) (first row) row)
        for detail = (if (consp row) (second row) nil)
        while (< count *composer-lane-row-limit*)
        when (json-value text :text)
          collect (cons text (if (stringp detail) detail "")) into rows
          and count t into count
        finally (return rows)))

(define-gateway-route handle-gateway-lanes-http (env)
  "GET /api/gateway/lanes — the composer lanes cells offer, oldest first, each
{name, sigil, description}; POST {lane} answers that lane's rows now,
{rows: [{text, detail}]}, or no rows and the cell's error as {error}. An
unknown lane is a 400."
  (:route (:get :post) "/api/gateway/lanes" "lanes")
  (case (getf env :request-method)
    (:get (make-json-object
           "lanes" (map 'vector (lambda (lane)
                                  (json-object (:getf lane "name" "sigil" "description")))
                        *registered-lanes*)))
    (:post (let ((name (json-value (gateway-request-json env) :text "lane")))
             (require-request (find-composer-lane name) "lane must name a registered lane")
             (handler-case
                 (make-json-object "rows" (map 'vector (lambda (row)
                                                         (make-json-object "text" (car row)
                                                                           "detail" (cdr row)))
                                               (composer-lane-rows name)))
               (error (condition)
                 (make-json-object "rows" #() "error" (princ-to-string condition))))))))
