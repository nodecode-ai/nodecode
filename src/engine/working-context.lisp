;;;; working-context.lisp --- the context as an editable file over immutable history.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.evolved)

;;; (context) writes the session's context.txt: a block per message of the
;;; retained history, each headed by the log position it stands for. The next
;;; model-request boundary compares the file with what was written, block by
;;; block, and one context.edited fact says what that history now reads:
;;; messages kept as they are, messages kept with their text changed, and the
;;; model's own notes where it wrote them. A kept message is the very object
;;; the fold built, so the request is byte-identical up to the first change and
;;; the provider's cached prefix holds that far. The log is never edited: the
;;; fold applies the newest visible fact (WORKING-CONTEXT-TURNS), recall reads
;;; every original, and a removed message's header written back restores it.

(defparameter *working-context-contract*
  '(("context.edited" "start" "source" "previous" "items" "first-change"
     "kept" "edited" "removed" "notes" "restored" "chars-before" "chars-after"))
  "The fact an accepted edit leaves, turn-scoped. START and SOURCE are the
first and newest log positions the edited file covered; ITEMS what that
history now reads, in order -- {id} a message kept as it is, {id text calls
args} one kept with its text, its calls or their arguments changed, {note} the
model's own words -- and FIRST-CHANGE the first item that differs from what
the file showed. The counts are the receipt the operator's row reads.")

(defun working-context-fact (session kind)
  "Newest visible KIND, respecting fork anchors and head navigation."
  (when (nlk:store-open-p)
    (let* ((visible (nlk:session-visible-turn-set session))
           (rows (nlk:events :kind kind :order :newest :as :rows
                             :columns '("log_position" "turn_id")
                             :where (nlk:session-chain-scope session)))
           (position (loop for (position turn) in rows
                           when (nlk::turn-visible-p turn visible) return position)))
      (when position (first (nlk:events :log-position position))))))

(defun body-digest (body)
  "BODY's digest as blocks compare: blank space at its end does not count."
  (nlk:short-digest (string-right-trim '(#\Space #\Tab #\Return #\Newline) body) 64))

(defun blank-p (text)
  (zerop (length (nlk:trimmed text))))

(defun context-file (session)
  "A store- and session-local working copy; no project file is claimed."
  (nlk:home (format nil "context/~a/~a/context.txt"
                    (nlk:short-digest (namestring (nlk::store-path nlk::*store*)) 64)
                    (nlk:short-digest session 64))))

(defun context-record-file (session)
  "What the newest export wrote into SESSION's context.txt, beside it."
  (merge-pathnames "context.export.json" (context-file session)))

;;; --- naming and building messages -------------------------------------------

(defun message-id (msg &aux (source (message-source msg)))
  "MSG's name in context.txt: the id MESSAGE-SOURCE holds, as text."
  (typecase source
    (integer (princ-to-string source))
    (string source)))

(defun id-position (id)
  "The log position ID stands for: its leading digits."
  (and id (values (parse-integer id :junk-allowed t))))

(nlk:define-side-table context-item (:weakness :key :synchronized t)
  "MESSAGE -> the item of the edit that built it, when that is more than the
{id} a message kept as it is reads as.")

(defparameter +note-label+ "[context note]"
  "What a model's note opens with on the wire: its own words, never an
executed call and never the operator's.")

(defparameter +edited-result-line+ "[edited in context; (getf (recall-entry ~a) :text) reads the original]"
  "The line an edited tool result ends with, naming the recall that reads it whole.")

(defun note-message (text &aux (msg (message "assistant" (format nil "~a~%~a" +note-label+ text))))
  (setf (message-source msg) :note)
  msg)

(defun note-text (msg &aux (content (message-content msg)))
  "The words of MSG, a note, without its label."
  (if (uiop:string-prefix-p +note-label+ content)
      (string-left-trim '(#\Newline) (subseq content (length +note-label+)))
      content))

(defun tool-call-ids (msg)
  (map 'list (lambda (call) (gethash "id" call)) (json-array msg "tool_calls")))

(defun reasoning-p (msg)
  (loop for key being the hash-keys of msg
          thereis (and (stringp key) (eql 0 (search "reasoning" key)))))

(defun edited-message (raw item strip)
  "RAW, the message ITEM names, as ITEM reads it: its text, its calls or their
arguments replaced where ITEM says, and with STRIP its reasoning gone. RAW
itself when nothing changes, so a kept message stays the very object sent."
  ;; A signed thinking block is bound to the window it was signed in: a round
  ;; at or past the first change replays none (context.lisp, thinned rounds).
  (multiple-value-bind (text text-p) (gethash "text" item)
    (let ((calls (gethash "calls" item))
          (args (gethash "args" item))
          (strip (and strip (reasoning-p raw))))
      (if (not (or text-p calls args strip))
          raw
          (let ((copy (copy-json-object raw))
                (id (message-id raw)))
            (when text-p
              (setf (gethash "content" copy)
                    (cond ((equal "tool" (gethash "role" raw))
                           (format nil "~a~%~?" text +edited-result-line+ (list id)))
                          ((string= text "") :null)
                          (t text))))
            (when (or calls args)
              (if-let (kept (loop for call across (json-array raw "tool_calls")
                                  for call-id = (gethash "id" call)
                                  when (or (null calls) (find call-id calls :test #'equal))
                                    collect (if-let (edited (and args (gethash call-id args)))
                                              (chat-tool-call-object
                                               call-id (json-value call :string "function" "name")
                                               (encode-json-object
                                                (json-object "edited" edited
                                                             "note" (format nil "arguments edited in context; (recall-entry ~a) reads the original" id))))
                                              call)))
                (setf (gethash "tool_calls" copy) (coerce kept 'vector))
                (remhash "tool_calls" copy)))
            (when strip (without-reasoning copy))
            (setf (message-source copy) (message-source raw)
                  (context-item copy) item)
            copy)))))

(defun paired-messages (messages &aux (out '()) (open '()) (held '()))
  "MESSAGES with every call answered before anything else follows it and no
answer standing without its call: a call whose result left answers that it
did, an answer whose call left goes too, and a note or harness line between a
call and its answers follows them."
  (flet ((answer-open ()
           (loop for (id . name) in open
                 do (push (message "tool" "[result not in context]" :name name :tool-call-id id) out))
           (setf open '()))
         (release () (setf out (append held out) held '())))
    (dolist (msg messages)
      (let ((role (gethash "role" msg)))
        (cond ((equal role "tool")
               (when-let (call (assoc (gethash "tool_call_id" msg) open :test #'equal))
                 (setf open (remove call open))
                 (push msg out)
                 (unless open (release))))
              ((and open (or (equal role "system") (eq :note (message-source msg))))
               (push msg held))
              (t (answer-open)
                 (release)
                 (push msg out)
                 (setf open (map 'list (lambda (call)
                                         (cons (gethash "id" call)
                                               (json-value call :string "function" "name")))
                                 (json-array msg "tool_calls")))))))
    (answer-open)
    (release)
    (nreverse out)))

(defun merge-by-order (elements order-of entries &aux (out '()))
  "ELEMENTS, in their order, with ENTRIES -- (ORDER . ELEMENT) pairs, ORDER
ascending -- among them: each entry before the first element of a greater
order and the notes that lead up to it, the rest at the end. ORDER-OF answers
an element's order, NIL for a note."
  ;; A harness line between two kept messages stays where it stood, ahead of
  ;; a note written before the second: the bytes before the note are the ones
  ;; the provider already holds.
  (flet ((flush (bound)
           (loop while (and entries (or (null bound) (< (car (first entries)) bound)))
                 do (push (cdr (pop entries)) out))))
    (loop for (element . rest) on elements
          do (flush (or (funcall order-of element)
                        (loop for next in rest thereis (funcall order-of next))))
             (push element out))
    (flush nil))
  (nreverse out))

;;; --- one projection -----------------------------------------------------------

(defun working-context-turns (session raw)
  "RAW, the session's folded turn units, as its newest visible edit reads them."
  (if-let (edit (working-context-fact session "context.edited")) (edited-turns raw edit) raw))

(defun edited-turns (raw edit &aux (payload (nlk:event-payload edit))
                                   (source (gethash "source" payload))
                                   (edited-at (nlk:event-log-position edit))
                                   (first-change (gethash "first-change" payload))
                                   (named (make-hash-table :test #'equal))
                                   (touched (make-hash-table))
                                   (harness '())
                                   (order 0))
  "RAW as EDIT, a context.edited fact, reads it: its items in place of the
history it covered, each in the unit of the message it names and a note in the
unit of what stands before it, the harness lines of that history where the
fold put them, and everything after it as the fold made it."
  (flet ((covered-p (msg &aux (position (id-position (message-id msg))))
           (and position (<= (gethash "start" payload) position source))))
    (loop for unit in raw
          for u from 0
          do (dolist (msg (history-turn-messages unit))
               (when (covered-p msg)
                 (setf (gethash u touched) t)
                 (if (equal "system" (gethash "role" msg))
                     (push (cons order (list order msg u)) harness)
                     (setf (gethash (message-id msg) named) (list order msg u))))
               (incf order)))
    (let* ((last -1)
           (built (loop for item across (gethash "items" payload)
                        for index from 0
                        for (at msg unit) = (gethash (gethash "id" item) named)
                        if (gethash "note" item)
                          collect (list nil (note-message (gethash "note" item)) nil)
                        else if (and at (> at last))
                               collect (progn (setf last at)
                                              (list at (edited-message msg item (>= index first-change)) unit))))
           (merged (merge-by-order built #'first (nreverse harness)))
           (units (make-hash-table))
           (unit (loop for (nil nil u) in merged thereis u)))
      (loop for (nil msg u) in merged
            do (when u (setf unit u))
               (push msg (gethash unit units)))
      (loop for turn in raw
            for u from 0
            collect (let ((tail (loop for msg in (history-turn-messages turn)
                                      for position = (id-position (message-id msg))
                                      unless (covered-p msg)
                                        ;; generated before the edit, against the window it changed
                                        collect (if (and position (< source position edited-at)
                                                         (equal "assistant" (gethash "role" msg))
                                                         (reasoning-p msg))
                                                    (edited-message msg (json-object "id" (message-id msg)) t)
                                                    msg))))
                      (if (and (not (gethash u touched))
                               (every #'eq tail (history-turn-messages turn)))
                          turn
                          (let* ((messages (paired-messages (append (reverse (gethash u units)) tail)))
                                 (present (loop for msg in messages
                                                when (and (equal "assistant" (gethash "role" msg))
                                                          (integerp (message-source msg)))
                                                  collect (message-source msg))))
                            (record-with #'make-history-turn turn
                                         :messages messages
                                         :round-starts (remove-if-not (lambda (start) (member start present))
                                                                      (history-turn-round-starts turn))
                                         :token-estimate nil
                                         :message-chars nil))))))))

;;; --- the file -------------------------------------------------------------------

(defun open-round-start (messages)
  "How many of MESSAGES come before the round still running -- the last
assistant message whose calls the fold could only answer as unrecorded -- or
all of them."
  (let* ((vector (coerce messages 'vector))
         (last (position-if (lambda (msg) (and (equal "assistant" (gethash "role" msg))
                                               (plusp (length (json-array msg "tool_calls")))))
                            vector :from-end t)))
    (if (and last
             (find-if (lambda (msg &aux (id (message-id msg)))
                        (and id (equal "tool" (gethash "role" msg))
                             (char= #\o (char id (1- (length id))))))
                      vector :start (1+ last)))
        last
        (length vector))))

(defun render-context (session-id)
  "(values TEXT RECORD): SESSION-ID's retained history as context.txt reads it,
a block a message, and the record of what each block was. Harness lines and
the active request stay out: an edit keeps them where they stand."
  (let* ((turns (history-turns-for-session (intern-session session-id)))
         (anchor (car (last turns)))
         (covered (nthcdr (session-floor-boundary session-id turns) turns))
         (all (loop for turn in covered append (history-turn-messages turn)))
         (messages (subseq all 0 (open-round-start all)))
         (nonce (nlk:short-digest
                 (format nil "~a ~a ~a" session-id (get-internal-real-time) (random 1000000)) 6))
         (items '()) (protected '()) (notes 0) (source 0)
         (blocks (make-hash-table :test #'equal))
         (calls (make-hash-table :test #'equal))
         (body
           (with-output-to-string (out)
             (flet ((emit (id kind name text &rest fields)
                      (format out "[[m:~a ~a ~a~@[ ~a~]]]~%~a~%" nonce id kind name text)
                      (setf (gethash id blocks)
                            (apply #'make-json-object "kind" kind "digest" (body-digest text)
                                   "length" (length text) "item" (1- (length items)) fields))))
               (dolist (msg messages)
                 (let ((id (message-id msg)) (role (gethash "role" msg)))
                   (when-let (position (id-position id)) (setf source (max source position)))
                   (cond ((equal role "system"))
                         ((eq :note (message-source msg))
                          (push (json-object "note" (note-text msg)) items)
                          (emit (format nil "n~d" (incf notes)) "note" nil (note-text msg)))
                         ((null id))
                         (t
                          (push (or (context-item msg) (json-object "id" id)) items)
                          (cond ((equal role "user")
                                 (if (member msg (history-turn-messages anchor) :test #'eq)
                                     (push id protected)
                                     (emit id "user" nil (message-content msg))))
                                ((equal role "assistant")
                                 (loop for call across (json-array msg "tool_calls")
                                       do (setf (gethash (gethash "id" call) calls)
                                                (list id (json-value call :string "function" "name")
                                                      (nlk:one-line (json-value call :string "function" "arguments")))))
                                 (let ((text (message-content msg))
                                       (ids (coerce (tool-call-ids msg) 'vector)))
                                   (if (plusp (length text))
                                       (emit id "assistant" nil text "calls" ids "text" t)
                                       (setf (gethash id blocks)
                                             (make-json-object "kind" "assistant" "item" (1- (length items))
                                                               "calls" ids "text" nil)))))
                                ((equal role "tool")
                                 (destructuring-bind (&optional carrier name args)
                                     (gethash (gethash "tool_call_id" msg) calls)
                                   (when carrier
                                     ;; The header names the block: the seq
                                     ;; line the wire ends the result with
                                     ;; would only say it twice.
                                     (let ((result (without-seq-line (message-content msg))))
                                       (emit id "tool" name (format nil "→ ~a~%~a" args result)
                                             "carrier" carrier "call" (gethash "tool_call_id" msg)
                                             "args" args "result" (body-digest result))))))))))))))
         (revision (working-context-fact session-id "context.edited"))
         (text (format nil "[[context ~a]]~@[ ~a~] · keep a block's [[m:~a …]] line to keep that message as it is; delete the block to drop it; edit its text to change it; text under a [[note]] line, or added after a block, is your note; write a removed block's header back to restore it~%~a"
                       nonce (and *live-turn* (context-text *live-turn*)) nonce body)))
    (values text
            (make-json-object "nonce" nonce
                              "start" (if covered (history-turn-start-sequence (first covered)) 0)
                              "source" source
                              "revision" (if revision (nlk:event-id revision) "")
                              "head" (nlk:session-head-stamp session-id)
                              "file" (nlk:short-digest text 64)
                              "chars" (length text)
                              "items" (coerce (reverse items) 'vector)
                              "protected" (coerce (reverse protected) 'vector)
                              "blocks" blocks))))

(defun export-context-file (session-id)
  "Write SESSION-ID's context.txt and the record of what it says from the
context as it stands, unless the file holds an edit the next model request has
not taken yet. => the file's path."
  (let ((path (context-file session-id))
        (record (nlk::json-object-file (context-record-file session-id))))
    (unless (and record (probe-file path)
                 (not (equal (nlk:short-digest (uiop:read-file-string path :external-format :utf-8) 64)
                             (gethash "file" record))))
      (multiple-value-bind (text record) (render-context session-id)
        (nlk:write-file-atomically (context-record-file session-id) (encode-json-object record)
                                   :mode #o600 :directory-mode #o700)
        (nlk:write-file-atomically path text :mode #o600 :directory-mode #o700)))
    path))

(defun context (&key drop keep note)
  "Return this session's context.txt path, written from your context as it
stands unless it holds an edit the next model request has not taken yet;
(help :context) says what the blocks are and what an edit does. With DROP,
KEEP or NOTE, edit without the file: DROP names the blocks that go -- t for
every block, or seqs and (from . to) spans of the seq every tool result ends
with -- KEEP the seqs held back from the drop, NOTE your note at the end,
alone when nothing else changes. The next model request takes it as it takes
the file; the return is what it will take."
  (unless (and nlk:*scribe-session-id* (nlk:store-open-p))
    (error "context: requires a durable session"))
  (let ((path (export-context-file nlk:*scribe-session-id*)))
    (if (or drop keep note)
        (stage-context-edit nlk:*scribe-session-id* path drop keep note)
        (namestring path))))

;;; --- an edit named by seq -----------------------------------------------------------

(defun seq-predicate (spec what)
  "SPEC, the blocks :drop or :keep (WHAT) names -- T for every block, a
(FROM . TO) span of log positions, or a list of seqs, ids and spans -- as a
predicate over a block's id, NIL being an unnamed note."
  (flet ((malformed (entry)
           (error "context: ~a takes t, seqs, ids or (from . to) spans, not ~s" what entry)))
    (cond ((eq spec t) (constantly t))
          ((null spec) (constantly nil))
          ((not (listp spec)) (malformed spec))
          (t
           (let ((ids '()) (spans '()))
             (flet ((span (from to)
                      (if (and (integerp from) (integerp to))
                          (push (cons (min from to) (max from to)) spans)
                          (malformed (cons from to)))))
               (dolist (entry (if (listp (cdr spec)) spec (list spec)))
                 (cond ((integerp entry) (span entry entry))
                       ((stringp entry) (push entry ids))
                       ((and (consp entry) (not (listp (cdr entry)))) (span (car entry) (cdr entry)))
                       ((and (consp entry) (= 2 (length entry))) (span (first entry) (second entry)))
                       (t (malformed entry)))))
             (lambda (id)
               (and (stringp id)
                    (or (member id ids :test #'string=)
                        (when-let (position (id-position id))
                          (loop for (from . to) in spans thereis (<= from position to)))))))))))

(defun context-edit-text (text nonce dropped-p kept-p note)
  "TEXT, a context.txt, as the edit leaves it: without the blocks DROPPED-P
names that KEPT-P does not, NOTE under a [[note]] line at the end =>
(values TEXT DROPPED KEPT), the ids that went and how many blocks stay."
  (let* ((header (format nil "[[context ~a]]" nonce))
         ;; A file edited by hand may have lost its first line: then every
         ;; line of it is body, and the edit is named against the export the
         ;; record holds.
         (headed (if (uiop:string-prefix-p header text) text (format nil "~a~%~a" header text)))
         (dropped '()) (kept 0))
    (values
     (with-output-to-string (out)
       (write-line (first-line headed) out)
       (loop for (id kind name body) in (context-blocks headed nonce)
             do (cond ((and (funcall dropped-p id) (not (funcall kept-p id)))
                       (push (or id "note") dropped))
                      (t (incf kept)
                         (if id
                             (format out "[[m:~a ~a ~a~@[ ~a~]]]~%~a~%" nonce id kind name body)
                             (format out "[[note]]~%~a~%" body)))))
       (unless (or (null note) (blank-p note))
         (format out "[[note]]~%~a~%" note)))
     (nreverse dropped) kept)))

(defun stage-context-edit (session-id path drop keep note)
  "Write the edit DROP, KEEP and NOTE name into SESSION-ID's context.txt at
PATH -- over the edit it holds when one waits, so edits in one round compose
-- => what the next model request will take."
  (let ((record (nlk:decode-json (uiop:read-file-string (context-record-file session-id)
                                                        :external-format :utf-8))))
    (multiple-value-bind (text dropped kept)
        (context-edit-text (uiop:read-file-string path :external-format :utf-8)
                           (gethash "nonce" record) (seq-predicate (or drop (and keep t)) ":drop")
                           (seq-predicate keep ":keep") note)
      (nlk:write-file-atomically path text :mode #o600)
      (format nil "context edit staged: ~d block~:p dropped~@[ (~a)~], ~d kept~:[~;, your note at the end~]; the next model request takes it and reports what it kept, changed and removed"
              (length dropped)
              (cond ((null dropped) nil)
                    ((<= (length dropped) 12) (format nil "~{~a~^, ~}" dropped))
                    (t (format nil "~a … ~a" (first dropped) (car (last dropped)))))
              kept (and note (not (blank-p note)))))))

;;; --- taking an edit -----------------------------------------------------------

(defun context-blocks (text nonce &aux (marker (format nil "[[m:~a " nonce))
                                       (blocks '()) (head (list nil "note" nil)) (lines '()))
  "TEXT, an edited context.txt, after its first line as blocks (ID KIND NAME
BODY): one per header line [[m:NONCE ID KIND NAME]] with the lines under it,
and (NIL \"note\" NIL BODY) for a [[note]] line's and for text above the
first header."
  (flet ((close-block ()
           (push (append head (list (format nil "~{~a~^~%~}" (reverse lines)))) blocks)
           (setf lines '())))
    (dolist (line (rest (uiop:split-string text :separator '(#\Newline))))
      (let ((line (string-right-trim '(#\Return) line)))
        (cond ((and (uiop:string-prefix-p marker line) (uiop:string-suffix-p line "]]"))
               (close-block)
               (let ((parts (uiop:split-string (subseq line (length marker) (- (length line) 2))
                                               :separator '(#\Space))))
                 (setf head (list (first parts) (second parts) (third parts)))))
              ((string= (nlk:trimmed line) "[[note]]")
               (close-block)
               (setf head (list nil "note" nil)))
              (t (push line lines)))))
    (close-block))
  (remove-if (lambda (block) (and (null (first block)) (blank-p (fourth block))))
             (nreverse blocks)))

(defun edited-items (record blocks raw)
  "(values ITEMS FIRST-CHANGE COUNTS): BLOCKS, the edited file, against RECORD,
the export it was edited from, as the items of a context.edited fact over RAW,
the history the fold holds; FIRST-CHANGE NIL when nothing changed."
  (let ((exported (gethash "blocks" record))
        (current (gethash "items" record))
        (start (gethash "start" record))
        (source (gethash "source" record))
        (raw-by-id (make-hash-table :test #'equal))
        ;; (:note TEXT), (:item ITEM ORDER BODY CARRIER) or (:carrier ID), in file order
        (out '())
        ;; assistant id -> (ITEM KEPT-CALLS TEXT-SEEN)
        (carriers (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (restores '())
        (kept 0) (edited 0) (notes 0))
    (loop for msg in (loop for unit in raw append (history-turn-messages unit))
          for order from 0
          for id = (message-id msg)
          for position = (id-position id)
          when (and position (<= start position source))
            do (setf (gethash id raw-by-id) (cons order msg)))
    (labels ((order (id) (car (gethash id raw-by-id)))
             (raw (id) (cdr (gethash id raw-by-id)))
             (current (entry) (copy-json-object (aref current (gethash "item" entry))))
             (same-p (digest body) (equal digest (body-digest body)))
             (note (text)
               (unless (blank-p text)
                 (incf notes)
                 (push (list :note (string-trim '(#\Newline #\Return) text)) out)))
             (carrier (id)
               (alexandria:ensure-gethash id carriers
                                          (progn (push (list :carrier id) out)
                                                 (list (if-let (entry (gethash id exported))
                                                         (current entry)
                                                         (json-object "id" id))
                                                       '() nil))))
             (without-edit-line (text id)
               ;; an edited result keeps one recall line, however often it is edited
               (let ((line (format nil "~%~?" +edited-result-line+ (list id))))
                 (if (uiop:string-suffix-p text line) (subseq text 0 (- (length text) (length line))) text)))
             (exported-block (id kind body)
               (let* ((entry (gethash id exported))
                      (length (gethash "length" entry))
                      (added (and (> (length body) length)
                                  (same-p (gethash "digest" entry) (subseq body 0 length))
                                  (subseq body length)))
                      (body (if added (subseq body 0 length) body))
                      (same (same-p (gethash "digest" entry) body)))
                 (if same (incf kept) (incf edited))
                 (cond ((and (not same) (member kind '("note" "user") :test #'equal)) (note body))
                       ((equal kind "note")
                        (push (list :note (gethash "note" (current entry))) out))
                       ((equal kind "user")
                        (push (list :item (current entry) (order id) body nil) out))
                       ((equal kind "assistant")
                        (let ((c (carrier id)))
                          (setf (third c) t)
                          (unless same (setf (gethash "text" (first c)) body))))
                       (t
                        (let* ((c (carrier (gethash "carrier" entry)))
                               (call (gethash "call" entry))
                               (arrow-p (uiop:string-prefix-p "→ " body))
                               (newline (position #\Newline body))
                               (args (if arrow-p (subseq body 2 (or newline (length body))) (gethash "args" entry)))
                               (result (cond ((not arrow-p) body) (newline (subseq body (1+ newline))) (t ""))))
                          (push call (second c))
                          (unless (equal args (gethash "args" entry))
                            (setf (gethash call (or (gethash "args" (first c))
                                                    (setf (gethash "args" (first c)) (make-json-object))))
                                  args))
                          (push (list :item (if (same-p (gethash "result" entry) result)
                                                (current entry)
                                                (json-object "id" id "text" (without-edit-line result id)))
                                      (order id) result (gethash "carrier" entry))
                                out))))
                 (when added (note added))))
             (issuer (id &aux (call (gethash "tool_call_id" (raw id))))
               ;; the covered assistant message that issued result ID's call
               (loop for other being the hash-keys of raw-by-id using (hash-value entry)
                     when (find call (tool-call-ids (cdr entry)) :test #'equal) return other))
             (finished (id item &aux (msg (raw id)))
               ;; ITEM, carrier ID's, normalized against the message it names; NIL when it is empty
               (when (equal (gethash "text" item) (message-content msg)) (remhash "text" item))
               (when (equal (coerce (gethash "calls" item #()) 'list) (tool-call-ids msg)) (remhash "calls" item))
               (when (and (gethash "args" item) (zerop (hash-table-count (gethash "args" item))))
                 (remhash "args" item))
               (unless (and (blank-p (gethash "text" item (message-content msg)))
                            (null (coerce (gethash "calls" item (tool-call-ids msg)) 'list)))
                 item)))
      (loop for (id kind nil body) in blocks
            for entry = (and id (gethash id exported))
            do (cond ((and entry (not (gethash id seen)) (equal kind (gethash "kind" entry))
                           (or (not (equal kind "assistant")) (gethash "text" entry)))
                      (setf (gethash id seen) t)
                      (exported-block id kind body))
                     ((and id (raw id) (not entry) (not (gethash id seen)))
                      ;; a removed message's header written back: it returns where it stood
                      (setf (gethash id seen) t)
                      (push id restores)
                      (note body))
                     (t (note body))))
      ;; carriers: the text a block removed, the calls whose blocks stayed
      (let ((placed '()) (dropped '()) (last -1) (elements '()))
        (setf out
              (loop for entry in (nreverse out)
                    if (eq (first entry) :carrier)
                      append (let* ((id (second entry))
                                    (state (gethash id carriers))
                                    (item (first state))
                                    (exported-entry (gethash id exported))
                                    (shown (coerce (gethash "calls" exported-entry #()) 'list))
                                    (stayed (remove-if-not (lambda (call) (member call (second state) :test #'equal))
                                                           shown)))
                               (when (and (gethash "text" exported-entry) (not (third state)))
                                 (setf (gethash "text" item) ""))
                               (unless (equal stayed shown)
                                 (setf (gethash "calls" item) (coerce stayed 'vector)))
                               (if (and (raw id) (finished id item))
                                   (list (list :item item (order id) (gethash "text" item) nil))
                                   (progn (push id dropped) '())))
                    else collect entry))
        (let ((present (make-hash-table :test #'equal)))
          (dolist (entry out)
            (when (eq (first entry) :item) (setf (gethash (gethash "id" (second entry)) present) t)))
          (flet ((place (id item)
                   (unless (or (gethash id present) (null (order id)))
                     (setf (gethash id present) t)
                     (push (cons (order id) (list (order id) item)) placed))))
            (dolist (id restores)
              (let ((role (gethash "role" (raw id))))
                (cond ((equal role "tool")
                       (let* ((issuer (issuer id))
                              (call (gethash "tool_call_id" (raw id)))
                              (state (and issuer (gethash issuer carriers))))
                         (when issuer
                           (if (and state (not (member issuer dropped :test #'equal)))
                               (when-let (calls (gethash "calls" (first state)))
                                 (setf (gethash "calls" (first state))
                                       (coerce (remove-if-not (lambda (other)
                                                                (or (equal other call) (find other calls :test #'equal)))
                                                              (tool-call-ids (raw issuer)))
                                               'vector)))
                               (when-let (item (finished issuer (json-object "id" issuer "text" "" "calls" (vector call))))
                                 (place issuer item)))
                           (place id (json-object "id" id)))))
                      ((member role '("user" "assistant") :test #'equal)
                       (place id (json-object "id" id))))))
            (loop for id across (gethash "protected" record)
                  when (raw id)
                    do (place id (json-object "id" id)))))
        ;; Kept messages stay in log order: one moved behind a later one is
        ;; read as a note in the place it was moved to.
        (loop for (kind item order body carrier) in out
              do (cond ((eq kind :note) (push (list nil (json-object "note" item)) elements))
                       ((or (null order) (member carrier dropped :test #'equal) (<= order last))
                        (when (and body (not (blank-p body)))
                          (incf notes)
                          (push (list nil (json-object "note" body)) elements))
                        (when (and order (gethash (gethash "id" item) carriers))
                          (push (gethash "id" item) dropped)))
                       (t (setf last order)
                          (push (list order item) elements))))
        (let* ((items (mapcar #'second
                              (merge-by-order (nreverse elements) #'first
                                              (sort placed #'< :key #'car))))
               (encoded (map 'list #'encode-json-object current))
               (first-change (or (loop for item in items
                                       for was in encoded
                                       for index from 0
                                       unless (equal (encode-json-object item) was) return index)
                                 (and (/= (length items) (length encoded))
                                      (min (length items) (length encoded))))))
          (values items first-change
                  (list :kept kept :edited edited :notes notes :restored (length restores)
                        :removed (loop for id being the hash-keys of exported using (hash-value entry)
                                       count (and (not (gethash id seen))
                                                  (or (not (equal "assistant" (gethash "kind" entry)))
                                                      (gethash "text" entry)))))))))))

(defun edit-receipt (counts first-change total before after)
  "What the model reads of an accepted edit."
  (destructuring-bind (&key kept edited notes restored removed) counts
    (format nil "context edit accepted: ~d kept · ~d edited · ~d removed · ~d note~:p~@[ · ~d restored~]; editable text ~~~a → ~~~a tokens; ~a"
            kept edited removed notes (and (plusp restored) restored)
            (format-token-count (ceiling before 4)) (format-token-count (ceiling after 4))
            (if (zerop first-change)
                "it changes from the first message, so the history after the system prompt is sent uncached once"
                (format nil "the first ~d of ~d messages are unchanged, so the cached prefix holds through them"
                        first-change total)))))

(defun context-edit-line (payload)
  "The words of the transcript row a context.edited fact reads as, under its
`Context' label: the token change, every count that is not zero, and who."
  ;; Without this, a zero count such as `0 edited' would be reported.
  (format nil "~~~a → ~~~a tokens~{ · ~a~} · by the model"
          (format-token-count (ceiling (gethash "chars-before" payload 0) 4))
          (format-token-count (ceiling (gethash "chars-after" payload 0) 4))
          (loop for (key . control) in '(("removed" . "~d removed") ("edited" . "~d edited")
                                         ("notes" . "~d note~:p") ("restored" . "~d restored"))
                for count = (gethash key payload 0)
                when (plusp count) collect (format nil control count))))

(defun working-context-append (turn kind payload)
  "Caller holds the transaction; use the existing fact owner and visibility."
  (nlk:ensure-turn-not-cancelled turn)
  (when (nlk::turn-terminal-p (nlk:durable-turn-turn-id turn))
    (error "context: turn is terminal"))
  (let ((shape (assoc kind *working-context-contract* :test #'equal))
        (object (nlk:plist-json payload)))
    (unless shape (error "context: unknown kind ~a" kind))
    (dolist (field (cdr shape))
      (unless (nth-value 1 (gethash field object)) (error "context: missing ~a" field))))
  (nlk::append-turn-event-row (nlk::store-handle nlk::*store*) turn kind payload
                              :owner 'working-context-append))

(defun accept-context-edit (turn record text)
  "TEXT, context.txt as the model left it, against RECORD, the export it came
from, as one context.edited fact appended under the store lock. => the receipt."
  (nlk::with-store-transaction (handle)
    (let* ((session (nlk:durable-turn-session-id turn))
           (revision (working-context-fact session "context.edited"))
           (nonce (gethash "nonce" record)))
      (unless (uiop:string-prefix-p (format nil "[[context ~a]]" nonce) text)
        (error "context: the first line, [[context ~a]] …, names the export this file came from; keep it" nonce))
      (unless (equal (gethash "revision" record) (if revision (nlk:event-id revision) ""))
        (error "context: another edit was taken after this file was written; (context) writes the current one"))
      (unless (eql (gethash "head" record) (nlk:session-head-stamp session))
        (error "context: the session moved to another branch after this file was written"))
      (multiple-value-bind (items first-change counts)
          (edited-items record (context-blocks text nonce)
                        (history-turns-for-session (intern-session session) :raw t))
        (cond ((null first-change) "context edit changed nothing; context.txt is written afresh")
              (t (working-context-append
                  turn "context.edited"
                  (list* :start (gethash "start" record) :source (gethash "source" record)
                         :previous (if revision (nlk:event-id revision) "")
                         :items (coerce items 'vector) :first-change first-change
                         :chars-before (gethash "chars" record) :chars-after (length text)
                         counts))
                 (edit-receipt counts first-change (length items) (gethash "chars" record) (length text))))))))

(defun context-edit-receipt (turn path text &optional rejected)
  "Durable model-visible receipt and its inspectable file, not a transcript rewrite."
  (when rejected
    (nlk:write-file-atomically (merge-pathnames "context.rejected.txt" path) rejected :mode #o600))
  (nlk:write-file-atomically (merge-pathnames "context.receipt.txt" path) text :mode #o600)
  (nlk:record-turn-context-prompt turn :text (format nil "[~a]" text)))

(defun sync-context-workspace (session)
  "At a model-request boundary, take the edit context.txt holds, or refuse it
with the reason, and write the file afresh either way."
  (when (and session (nlk:store-open-p) *current-durable-turn*
             (equal (session-id session) (nlk:durable-turn-session-id *current-durable-turn*)))
    (let* ((id (session-id session))
           (turn *current-durable-turn*)
           (path (context-file id))
           (record-path (context-record-file id)))
      (when (and (probe-file path) (probe-file record-path))
        (let ((text (uiop:read-file-string path :external-format :utf-8)))
          (multiple-value-bind (receipt rejected)
              (handler-case
                  (let ((record (nlk:decode-json (uiop:read-file-string record-path :external-format :utf-8))))
                    (unless (equal (nlk:short-digest text 64) (gethash "file" record))
                      (accept-context-edit turn record text)))
                (error (e) (values (format nil "context edit rejected: ~a" e) text)))
            (when receipt
              ;; A receipt write failure is not a rejection of an already
              ;; committed edit: let that I/O error stand as itself.
              (context-edit-receipt turn path receipt rejected)
              (unless (equal text (uiop:read-file-string path :external-format :utf-8))
                (error "context: concurrent file write; working copy left intact"))
              (delete-file path)
              (export-context-file id))))))))

(dolist (kind (mapcar #'car *working-context-contract*))
  (setf (gethash kind nlk::*owned-kind-owners*) 'working-context-append))
