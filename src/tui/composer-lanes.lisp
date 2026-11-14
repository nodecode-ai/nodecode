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
    ;; Whatever landed, a lane still wanted is asked again by the next paint:
    ;; the one fetcher answers the lane asked last, so an ask can be passed over.
    (when landed (setf (chat-model-lane-rows-wanted model) nil))
    (and landed t)))

;;; --- a lane's tokens, painted -----------------------------------------------
;;; A token a cell's lane opens -- its sigil at a word start, then the name up
;;; to the next blank -- paints in one of two tones: recognised while the rows
;;; in hand name it, unresolved while they do not. The rows are the ones the
;;; dropdown paints from; a lane whose rows are not in hand paints nothing and
;;; asks for them from the paint, off the keystroke path, once until a fetch
;;; lands. The file lane and the slash lane never paint here: the file lane's
;;; @ paints as it always did, and a slash command's text is the command's.
;;; The tone is a preview, as the rows are: what a name binds is the cell's to
;;; decide on submit.

(defun composer-gateway-sigils ()
  "The open lanes cells offer, as (LANE . SIGIL), in table order."
  (composer-sigil :file)
  (loop for (lane sigil) in (cddr *composer-sigils-read*)
        when (and sigil (not (eq lane :file))) collect (cons lane sigil)))

(defun composer-lane-tokens (text &key prose-p)
  "Each lane token in TEXT as (START END LANE NAME): START at its sigil, END
past its name, trailing sentence punctuation left to the prose. NIL for a
slash command, unless PROSE-P marks an already classified prose line."
  (let ((sigils (composer-gateway-sigils)))
    (when (and sigils (or prose-p (not (slash-input-p text))))
      (loop with length = (length text)
            for start from 0 below length
            for lane = (car (rassoc (char text start) sigils))
            for end = (and lane
                           (or (zerop start) (blank-char-p (char text (1- start))))
                           (let ((stop (or (position-if #'blank-char-p text :start start) length)))
                             (loop while (and (> stop (1+ start))
                                              (find (char text (1- stop)) ".,;:!?)"))
                                   do (decf stop))
                             stop))
            when (and end (> end (1+ start)))
              collect (list start end lane (subseq text (1+ start) end))))))

(defun composer-lane-spans (text lane-rows &key prose-p)
  "TEXT's lane tokens whose lane has rows in LANE-ROWS, as (START END STATE),
STATE :RECOGNISED when a row's text is the name, else :UNRESOLVED.
PROSE-P marks a logical line whose whole draft is already known to be prose."
  (loop for (start end lane name) in (composer-lane-tokens text :prose-p prose-p)
        for rows = (assoc lane lane-rows :test #'string-equal)
        when rows
          collect (list start end (if (find name (cdr rows) :key #'car :test #'string-equal)
                                      :recognised
                                      :unresolved))))

(defun composer-lanes-wanting-rows (text lane-rows)
  "The lanes with a token in TEXT and no rows in LANE-ROWS, first seen first."
  (loop for (nil nil lane) in (composer-lane-tokens text)
        unless (or (assoc lane lane-rows :test #'string-equal)
                   (member lane wanted :test #'string-equal))
          collect lane into wanted
        finally (return wanted)))

(defun composer-visible-lane-spans (model &aux (editor (chat-model-editor model))
                                               (rows (chat-model-lane-rows model)))
  "For each visible composer row, the lane spans on it, (START END STATE) in
the row's own character offsets; the first lane wanting rows is asked for them."
  (unless (chat-model-composer-ask model)
    (let ((wanted (composer-lanes-wanting-rows (editor-value editor) rows)))
      (when-let (lane (find-if-not (lambda (lane)
                                     (member lane (chat-model-lane-rows-wanted model)
                                             :test #'string-equal))
                                   wanted))
        (when (ask-lane-rows model lane)
          (push lane (chat-model-lane-rows-wanted model))))))
  (let ((slash (slash-input-p (editor-value editor))))
    (loop for range in (editor-visible-ranges editor)
          collect (when (and range rows (not slash) (not (chat-model-composer-ask model)))
                    (destructuring-bind (line start end) range
                      (loop for (from to state) in (composer-lane-spans line rows :prose-p t)
                            when (and (< from end) (> to start))
                              collect (list (- (max from start) start)
                                            (- (min to end) start)
                                            state)))))))

(defun lane-span-segs (line spans make-run)
  "LINE cut at SPANS into the runs MAKE-RUN builds, (TEXT TONE-OR-NIL) each:
NIL for the plain text between, the span's tone for each span."
  (loop with at = 0
        for (from to state) in spans
        when (> from at) collect (funcall make-run (subseq line at from) nil) into segs
        collect (funcall make-run (subseq line from to)
                         (if (eq state :recognised) :lane-recognised :lane-unresolved))
          into segs
        do (setf at to)
        finally (return (if (< at (length line))
                            (append segs (list (funcall make-run (subseq line at) nil)))
                            segs))))
