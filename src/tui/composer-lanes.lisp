;;;; composer-lanes.lisp --- the composer lanes cells offer through the gateway. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- a cell's lane in the composer --------------------------------------------
;;; A lane a cell offers (gateway/lanes.lisp) opens on its sigil as the file
;;; lane opens on @ (COMPOSER-ACTIVE-LANE), and paints from the rows its last
;;; fetch answered. Each new token asks for a fresh fetch, off the keystroke
;;; path on a thread of its own; its answer waits for the loop's tick
;;; (LAND-COMPOSER-CATALOG-WALKS) and the frame after paints it. No keystroke
;;; dials the gateway. The rows are a preview of the names the cell knows now:
;;; a completed token is plain text, and what it means is the cell's to decide
;;; when the prompt is submitted, against the sigil table that prompt carries.

(defun gateway-lane-entries (model)
  "The composer lanes MODEL's gateway offers (GET /api/gateway/lanes), as the
plists *GATEWAY-LANES* holds; NIL from a gateway that offers none, or
predates them."
  (with-handlers ((error () nil))
    (loop for lane across (json-array (gateway-model-http-call model :get "/api/gateway/lanes" nil)
                                      "lanes")
          for name = (json-value lane :text "name")
          when name
            collect (list :name name :sigil (json-value lane :text "sigil")
                          :description (or (json-value lane :string "description") "")))))

(defun lane-suggestion-rows (token rows)
  "The rows of ROWS, (TEXT . DETAIL) each, that TOKEN answers: those whose
text starts with it first, then those holding it elsewhere, each group in the
lane's own order. An empty TOKEN answers every row."
  (flet ((row (row)
           (composer-suggestion (car row)
                                (if (slash-name-prefix-p token (car row))
                                    (subseq (car row) (length token))
                                    "")
                                :lane :detail (cdr row) :replace token)))
    (mapcar #'row
            (append (remove-if-not (lambda (row) (slash-name-prefix-p token (car row))) rows)
                    (remove-if (lambda (row) (or (slash-name-prefix-p token (car row))
                                                 (not (search token (car row) :test #'char-equal))))
                               rows)))))

(defun lane-token-suggestions (model lane token text)
  "LANE's rows for TOKEN, the token at the end of TEXT, from the rows MODEL
holds; a new token asks for the lane's rows again."
  (let ((asked (cons lane (- (length text) (length token)))))
    (unless (equal asked (chat-model-lane-asked model))
      (setf (chat-model-lane-asked model) asked)
      (ask-lane-rows model lane)))
  (lane-suggestion-rows token (cdr (assoc lane (chat-model-lane-rows model) :test #'string-equal))))

;;; One fetcher per model at most: it fetches whichever lane was asked for
;;; last, so a burst of tokens never queues a fetch per keystroke.

(defvar *lane-asks* (make-hash-table :test #'eq)
  "MODEL -> the lane its composer asked rows for last, not yet taken.")

(defvar *lane-fetchers* (make-hash-table :test #'eq)
  "The models whose lane fetcher is running.")

(defvar *lane-results* (make-hash-table :test #'eq)
  "MODEL -> ((LANE . ROWS) ...): fetches that answered and have not landed.")

(defvar *lane-lock* (bt2:make-lock :name "composer-lanes")
  "One lock for the three tables.")

(defun fetch-lane-rows (model lane)
  "LANE's rows as MODEL's gateway answers them now, (TEXT . DETAIL) each."
  (loop for row across (json-array (gateway-model-http-call model :post "/api/gateway/lanes"
                                                            (json-object "lane" lane))
                                   "rows")
        for text = (json-value row :text "text")
        when text collect (cons text (or (json-value row :string "detail") ""))))

(defun hand-over-lane-rows (model lane rows)
  "Leave LANE's ROWS for MODEL's next tick to land."
  (bt2:with-lock-held (*lane-lock*)
    (setf (gethash model *lane-results*)
          (acons lane rows (remove lane (gethash model *lane-results*)
                                   :key #'car :test #'string-equal)))))

(defun ask-lane-rows (model lane &aux (program (chat-model-program model)))
  "Fetch LANE's rows for MODEL on the fetcher thread, starting it if none
runs, and tick the loop when they land. NIL without a program."
  (when program
    (when (bt2:with-lock-held (*lane-lock*)
            (setf (gethash model *lane-asks*) lane)
            (unless (gethash model *lane-fetchers*)
              (setf (gethash model *lane-fetchers*) t)))
      (spawn "nodecode-composer-lanes"
        (loop for asked = (bt2:with-lock-held (*lane-lock*)
                            (or (prog1 (gethash model *lane-asks*) (remhash model *lane-asks*))
                                (progn (remhash model *lane-fetchers*) nil)))
              while asked
              ;; A fetch that fails keeps the rows in hand: they are a preview.
              do (with-handlers ((error () nil))
                   (hand-over-lane-rows model asked (fetch-lane-rows model asked))
                   (nlt:send program (nlt:make-tick-msg))))))
    t))

(defun land-lane-rows (model)
  "Install the lane rows fetched since the last frame into MODEL. => whether
any landed."
  (let ((landed (bt2:with-lock-held (*lane-lock*)
                  (prog1 (gethash model *lane-results*) (remhash model *lane-results*)))))
    (loop for (lane . rows) in (reverse landed)
          do (setf (chat-model-lane-rows model)
                   (acons lane rows (remove lane (chat-model-lane-rows model)
                                            :key #'car :test #'string-equal))))
    (and landed t)))
