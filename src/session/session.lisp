;;;; session.lisp --- the session owner. One owner for conversation state.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session is the organism's working set for one conversation: an id, an
;;;; ordered message list, and the admission state that decides whether a new
;;;; turn may start. This file owns that state and nothing else — the provider
;;;; call and the recursive loop belong to turn.lisp.
;;;;
;;;; Messages are stored in wire shape: hash tables with string keys, exactly as
;;;; shasht parses them. That is deliberate rather than lazy. A provider's
;;;; assistant message must go back into the next request byte-identical,
;;;; including fields this code does not model (reasoning_content, index,
;;;; provider-specific extras). Parsing into a struct and re-emitting would
;;;; silently drop them and break multi-round tool use.
;;;;
;;;; Admission exists for one concrete hazard: the organism's only tool is EVAL,
;;;; so a model can emit (nle:run "...") and re-enter the turn loop from inside
;;;; its own tool call. Without a gate that recursion is unbounded and bills real
;;;; tokens per level. IN-FLIGHT-P is that gate. It is not thread safety — no
;;;; thread exists in this system yet, by A4's requirement.
;;;;
;;;; The pending slot and record-turn drain are gone. Turn traffic is now logged
;;;; directly by turn.lisp through the event log. The working set is rebuilt by
;;;; HYDRATE-SESSION from that log; this file owns the intern table so a
;;;; session-id maps to one heap object for the life of the image.
;;;;
;;;; Intern is a cache. SESSION.CREATED is the authority. With the store open,
;;;; INTERN-SESSION refuses an id that has no created fact and copies cwd/parent
;;;; from that fact onto the heap object. MAKE-SESSION is not exported: it is
;;;; the closed-store test constructor. With the store open it is intern.

(in-package #:nodecode.evolved)

;;; --- limits -------------------------------------------------------------

;;; Bounds what a single turn can push at a provider, and keeps a runaway EVAL
;;; result from becoming the next prompt.
(defparameter *max-input-characters* 100000
  "Ceiling on one user message.")

;;; History is the working set only: the event log moves durable transcript
;;; off-heap, and this bound is what keeps the live heap flat regardless of
;;; how long a conversation runs.
(defparameter *max-history-messages* 400
  "Ceiling on in-heap history length.")

;;; --- conditions ---------------------------------------------------------

(define-condition session-error (error) ()
  (:documentation "Base class for session-level refusals."))

(define-errors (session-error) ()
  (turn-in-flight (id) "session ~a already has a turn in flight; refusing to ~
                        re-enter the loop" id)
  (input-too-large (length limit) "input is ~:D characters, limit is ~:D" length limit)
  (heap-session-mint-refused ()
   "make-session cannot mint an identity while the store is open; create-session then intern-session"))

;;; --- the session --------------------------------------------------------

(define-record (session (:constructor %make-session) (:copier nil)
                         (:export id messages))
  (id (format nil "s-~36r" (random (expt 36 8) (make-random-state t)))
   :type string)
  ;; Newest last, so the list is already in provider order. Replaced wholesale
  ;; by HYDRATE-SESSION from the durable fold; APPEND-MESSAGE only extends the
  ;; current turn after that fold.
  (messages '() :type list)
  ;; Durable source metadata for provider-context whole-turn eviction.
  (history-turns '() :type list)
  (history-boundary 0 :type (integer 0))
  ;; Admission latch. See the header note on EVAL re-entrancy.
  (in-flight-p nil :type boolean))

(defvar *session-registry* (make-hash-table :test #'equal)
  "One heap session per session-id. Surfaces intern; they do not allocate.")

(defun intern-session (session-id)
  "Return the unique SESSION for SESSION-ID in this image."
  ;; With the store open this is a cache of SESSION.CREATED: an id the log
  ;; does not hold signals UNKNOWN-SESSION — unless it was minted and is still
  ;; standing by, a heap session like any other until it
  ;; becomes durable when its first block lands (standby.lisp). With the store
  ;; closed it is heap-only, for isolated loop tests that never touch the log.
  (check-type session-id string)
  (when (and (nlk:store-open-p)
             (not (or (nlk:find-session session-id) (nlk:on-standby-p session-id))))
    (error 'nlk:unknown-session :session-id session-id))
  (sb-ext:with-locked-hash-table (*session-registry*)
    (values (alexandria:ensure-gethash session-id *session-registry*
                                       (%make-session :id session-id)))))

(defun make-session (&key (id nil id-supplied-p))
  "Closed-store heap constructor for isolated tests."
  ;; With the store open this is INTERN-SESSION: ID is required and must
  ;; already have a SESSION.CREATED fact. Surfaces intern; they do not mint.
  (cond
    ((nlk:store-open-p)
     (unless (and id-supplied-p (stringp id) (plusp (length id)))
       (error 'heap-session-mint-refused))
     (intern-session id))
    (id-supplied-p (%make-session :id id))
    (t (%make-session))))

;;; --- message construction -----------------------------------------------

(defun message (role content &key name tool-call-id tool-calls)
  "Build a wire-shaped message."
  ;; Only non-NIL fields are emitted, so a message carries exactly what the
  ;; provider expects and nothing extra.
  (json-object
   "role" role
   ;; A tool-call message legitimately has empty content, and some providers
   ;; reject a missing key. Emit "" rather than omitting it.
   "content" (or content "")
   :opt "name" name
   :opt "tool_call_id" tool-call-id
   :opt "tool_calls" tool-calls))

;;; --- typed content parts ---------------------------------------------------
;;; Wire content is a string OR a vector of typed part objects (the chat wire's
;;; own multi-part shape: {\"type\": \"text\"|\"image_url\"|\"file\", ...}).
;;; This is the lisp analogue of the Zig typed PromptMessage content
;;; (core.zig UserPart/DataContent): one owner of the content shape, so lanes
;;; and projections never re-derive what \"content\" may hold. Stored history
;;; carries whichever shape the wire carried; these helpers are total over both.

(defun make-text-content-part (text)
  "One {type: text} content part."
  (json-object "type" "text" "text" text))

(defun make-image-content-part (url)
  "One {type: image_url} content part."
  ;; URL may be a data: URI or a fetchable address; lanes decide how their
  ;; wire carries each.
  (json-object "type" "image_url"
               "image_url" (json-object "url" url)))

(defun content-part-type (part)
  (and (hash-table-p part) (gethash "type" part)))

(defun message-content-parts (content)
  "Normalize wire CONTENT to a list of typed parts."
  ;; A non-empty string is one text part; a vector keeps its object parts in
  ;; order; NIL/:NULL/"" is empty.
  (cond
    ((stringp content)
     (when (plusp (length content))
       (list (make-text-content-part content))))
    ((vectorp content)
     (coerce (remove-if-not #'hash-table-p content) 'list))))

(defun content-text (content)
  "Flatten wire CONTENT to its text: strings verbatim, part vectors as the
concatenation of their text parts, everything else empty."
  (if (stringp content)
      content
      (with-output-to-string (out)
        (dolist (part (message-content-parts content))
          (when (and (equal (content-part-type part) "text") (stringp (gethash "text" part)))
            (write-string (gethash "text" part) out))))))

(defun message-content (message)
  "The content of a wire-shaped MESSAGE as a string."
  ;; A provider may answer with :NULL content on a pure tool call, and stored
  ;; content may be a typed part vector; both normalize so callers never have
  ;; to test shapes.
  (content-text (and (hash-table-p message) (gethash "content" message))))

;;; --- admission ----------------------------------------------------------

(defun admit (session input)
  "Decide whether SESSION may start a turn carrying INPUT, and record it if so."
  ;; Refuses a re-entrant turn (the EVAL hazard) and an oversized input. On
  ;; success the latch is set and SESSION is returned. The user message is
  ;; logged to the event log by RUN, not added to session-messages.
  ;; HYDRATE-SESSION then rebuilds the working set from that log, so durable
  ;; input and heap history do not overlap. The caller must call FINISH-TURN,
  ;; which RUN does through UNWIND-PROTECT.
  (check-type session session)
  (unless (stringp input)
    (error 'type-error :datum input :expected-type 'string))
  (when session.in-flight-p
    (error 'turn-in-flight :id (session-id session)))
  (when (> (length input) *max-input-characters*)
    (error 'input-too-large :length (length input) :limit *max-input-characters*))
  (setf session.in-flight-p t)
  ;; User message is NOT appended here. It is logged to the event log by RUN.
  ;; HYDRATE-SESSION folds that log back into session-messages before the
  ;; provider runs.
  session)

(defun finish-turn (session)
  "Release SESSION's in-process re-entrancy latch."
  ;; Durable completion is kernel-owned by COMPLETE-TURN, FAIL-TURN, or
  ;; CANCEL-TURN. This cleanup operation is idempotent and never claims an
  ;; outcome.
  (setf session.in-flight-p nil)
  session)

;;; --- history ------------------------------------------------------------

(defun append-message (session message)
  "Append MESSAGE to SESSION's history, trimming the oldest messages past
*MAX-HISTORY-MESSAGES*."
  ;; Trimming is not compaction: nothing is summarized, rewritten, or
  ;; persisted here. The event log owns durable history. This is only the
  ;; bound that keeps the live heap from growing with conversation length.
  (setf session.messages
        (last (append session.messages (list message)) *max-history-messages*))
  message)

(defun forget (session)
  "Drop SESSION's in-heap history: its messages and the turn units they came
from."
  ;; Returns SESSION.
  ;;
  ;; In-heap only: the event log is the durable authority and is not affected.
  (check-type session session)
  (setf session.messages '() session.history-turns '() session.history-boundary 0)
  session)
