;;;; hook.lisp --- around-chains at named points and around any function.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One generic interception seam instead of per-cell wrapping. A hook on
;;;; a POINT is (lambda (op next)): return without calling NEXT to
;;;; short-circuit, call NEXT with a rebuilt OP to rewrite the input,
;;;; transform NEXT's return to rewrite the output, or code both sides to
;;;; wrap. The around shape subsumes observe/veto/pre/post, so a point never
;;;; needs phase suffixes — it is one noun naming the operation wrapped. The
;;;; points today, dispatched by EVOLVED through CALL-HOOKED:
;;;;
;;;;   :TOOL   one tool dispatch (turn.lisp). OP is (:name TOOL-NAME
;;;;           :arguments HASH :call-id ID); the field is :NAME, not :TOOL,
;;;;           so the keyword :TOOL only ever means the point. Runs on the
;;;;           turn worker.
;;;;   :FRAME  one live frame leaving the gateway (PUBLISH-EVENT,
;;;;           gateway.lisp): every session_stream_event, item_delta
;;;;           and background-exec update, for EVERY
;;;;           session, subscribed or not. OP is (:session-id ID :kind
;;;;           WIRE-KIND :payload HASH :cursor POSITION-OR-NIL :durable-p
;;;;           BOOL). Runs on the publishing thread, before the session lock
;;;;           is taken; a hook that filters by session does its own lookup.
;;;;           NEXT returns whether any peer received the frame. Isolated:
;;;;           a hook's own error is said and stepped over, never the
;;;;           frame's failure (CALL-ISOLATED).
;;;;   :CREDENTIAL   one provider credential resolved (provider.lisp
;;;;           RESOLVE-PROVIDER-CREDENTIAL): after the config tier and
;;;;           auth.json's api_keys answered nothing, before the environment
;;;;           ladder. OP is (:provider ID :family KEYWORD :auth PARSED-STORE
;;;;           :auth-path PATH :endpoint URL) — the store this process already
;;;;           parsed, handed over so a hook never re-reads the file, and the
;;;;           address the provider would dial. The hook answers a credential
;;;;           (MAKE-CREDENTIAL: the secret, its provenance, and the request
;;;;           attributes the lanes honor) or runs NEXT to decline. Runs on
;;;;           whichever thread froze the config.
;;;;   :ROUND  one model request about to be built (turn.lisp AGENT-LOOP):
;;;;           every round of every turn, its first included, after the
;;;;           context file's edit is taken and before the history folds,
;;;;           so a fact a hook records there -- a reminder through
;;;;           NLK:RECORD-TURN-CONTEXT-PROMPT's :TEXT -- is in that very
;;;;           request: a running turn hears it without stopping. OP is
;;;;           (:session-id ID :turn TURN :round N), TURN the running
;;;;           durable turn or NIL; NEXT does nothing and answers NIL. Runs
;;;;           on the turn worker. Isolated, like :FRAME.
;;;;
;;;; The same primitive is generalized to ANY function name: (hook
;;;; 'compose-frame "mine" fn) wraps every call of COMPOSE-FRAME, fn receiving
;;;; (next &rest args) — Emacs advice-add, thirty years proven, on a
;;;; primitive that already existed. A layer then wraps a shipped function
;;;; instead of forking its body, and survives upstream rewriting that body
;;;; as long as the signature holds. The mechanism is SB-INT:ENCAPSULATE, the
;;;; one TRACE uses: keyed, removable, and it survives a later DEFUN of the
;;;; same name, a later DEFMETHOD on a wrapped generic, and wraps (SETF name)
;;;; functions. One encapsulation per name,
;;;; owned here, runs the whole keyed chain, so two folders advising one
;;;; function under different keys both run in registration order and
;;;; UNHOOK removes exactly one.
;;;;
;;;; Ingress has no point because it has a function: NLE:SUBMIT is the one
;;;; in-process way to put a prompt into a session, the same path the
;;;; WebSocket start_turn/steer_turn commands are shells over.
;;;;
;;;; DEFINE-HOOK is the same registration as a defining form: a (hook ...)
;;;; call evaluated through eval runs once and is gone at the next
;;;; boot, while the scribe keeps every DEFINE-* form as a layer file that
;;;; loads from source — so advice written as DEFINE-HOOK is the organism's
;;;; durable shape for reshaping any function or point, and the layer loads
;;;; after every peripheral so a hook on a function a cell provides
;;;; finds it (cells.lisp LOAD-CELL-FOLDERS).
;;;;
;;;; Hooks are NOT registered as tools, so re-registering a tool cannot shed
;;;; them. They are read LIVE at each dispatch: a cell's stop must take
;;;; effect for the turn already running. Hooks run on the operating thread;
;;;; a test posture SETFs *HOOKS*, never LETs it (a LET is thread-local).

(in-package #:nodecode.kernel)

;;; NAME is a point keyword or a function name. DEFVAR, not DEFPARAMETER: a
;;; cell system reloading this file mid-serve must not shed every registered
;;; hook.
(defvar *hooks* '()
  "((NAME . ((KEY . FN) ...)) ...), registration order; the first hook
registered on a name is the outermost.")

(defun function-name-p (name)
  "Whether NAME designates a function to advise rather than a point: a
non-keyword symbol or a (SETF symbol) list."
  (or (and (symbolp name) name (not (keywordp name)))
      (and (consp name) (eq (first name) 'setf))))

(defun function-arity (fn &aux (lambda-list (function-lambda-list fn)))
  "How many positional arguments FN accepts: (values MIN MAX), MAX NIL when
unbounded (&rest or &key), or (values NIL NIL) when FN's lambda list cannot
be read — a foreign or interpreted function is not judged."
  ;; CALL-SHAPE (provisional.lisp) reads the list, as the hold reads it.
  (if (listp lambda-list) (values-list (butlast (call-shape lambda-list))) (values nil nil)))

(defun accepts-argument-counts-p (fn low high)
  "Whether FN takes every argument count from LOW to HIGH, HIGH NIL meaning
every count from LOW up."
  ;; An unreadable lambda list answers T: the check is a courtesy at
  ;; registration, never a gate the chain enforces.
  (multiple-value-bind (min max) (function-arity fn)
    (or (null min)
        (and (<= min low)
             (or (null max) (and high (>= max high)))))))

(defun put-in-place (alist key value test &aux (entry (cons key value)))
  "ALIST, fresh, with (KEY . VALUE) where KEY's entry stands under TEST, else at its end."
  (let ((present (assoc key alist :test test)))
    (if present (substitute entry present alist) (append alist (list entry)))))

;;; --- a hook's own error, kept inside it ------------------------------------
;;; At a point whose hooks only watch (:FRAME, :ROUND) one hook's bug is not
;;; the operation's failure: the caller asks CALL-HOOKED to :ISOLATE, and an
;;; error the hook signals itself -- outside its NEXT -- is counted, said once,
;;; and stepped over. Without this, a hook's own error would fail the live
;;; frames of every session, running turns would die, and admissions would be
;;; orphaned. A point whose hooks do the operation (:TOOL, :CREDENTIAL) and
;;; advice around a function stay unisolated: there a hook's error IS the
;;; operation's (a provider cell's walk signals the provider's refusal), and
;;; stepping over it would be a silent fallback.

(defvar *hook-failures* (make-hash-table :test #'equal :synchronized t)
  "(NAME KEY) -> (COUNT STATE): how often that hook's own error was stepped
over since it was registered, and whether that was said (:SAYING, :SAID).")

(defun hook-failure-key (name key)
  "The board key under which NAME's hook KEY's failure stands."
  (format nil "hook ~(~s~) ~a" name key))

(defun hook-failure-text (name key condition)
  "One line: NAME's hook KEY failed with CONDITION and is stepped over."
  (let* ((said (or (ignore-errors (princ-to-string condition))
                   (princ-to-string (type-of condition))))
         (line (subseq said 0 (or (position #\Newline said) (length said)))))
    (format nil "hook ~(~s~) ~s failed and is skipped wherever it fails, until ~
                 it is registered again or unhooked: ~a"
            name key (if (> (length line) 300)
                         (concatenate 'string (subseq line 0 300) "...")
                         line))))

(defun note-hook-failure (name key condition)
  "Say that NAME's hook KEY failed with CONDITION and stand it on the board,
or clear it when CONDITION is NIL. => the text, or NIL."
  ;; The seam START-GATEWAY advises into a NOTICE every attached shell
  ;; shows; unadvised, the board and the serve log.
  (let ((text (and condition (hook-failure-text name key condition))))
    (note-standing (hook-failure-key name key) text :error)
    (when text (format *error-output* "~&~a~%" text))
    text))

(defun hook-failed (name key condition)
  "Count CONDITION, NAME's hook KEY's own error, and say it once per
registration."
  ;; :SAYING holds while the notice is out: the notice is a live frame too,
  ;; and the hook failing on it again must not say it again. A notice that
  ;; could not be said leaves the state clear, so the next failure says it.
  (let* ((id (list name key))
         (say (sb-ext:with-locked-hash-table (*hook-failures*)
                (let ((entry (or (gethash id *hook-failures*)
                                 (setf (gethash id *hook-failures*) (list 0 nil)))))
                  (incf (first entry))
                  (unless (second entry)
                    (setf (second entry) :saying))))))
    (when say
      (let ((said (ignore-errors (note-hook-failure name key condition) t)))
        (sb-ext:with-locked-hash-table (*hook-failures*)
          (when-let (entry (gethash id *hook-failures*))
            (setf (second entry) (and said :said))))))))

(defun forget-hook-failure (name key)
  "Clear NAME's hook KEY's failures, and the board's word on them."
  (let ((entry (sb-ext:with-locked-hash-table (*hook-failures*)
                 (prog1 (gethash (list name key) *hook-failures*)
                   (remhash (list name key) *hook-failures*)))))
    (when (and entry (second entry))
      (ignore-errors (note-hook-failure name key nil)))))

(defun call-isolated (name key invoke next args)
  "INVOKE NAME's hook KEY with its NEXT, the rest of the chain, which takes
ARGS. An error the hook signals itself, outside NEXT, is HOOK-FAILED and
stepped over: the answer NEXT already gave, or NEXT run on ARGS when it never
ran. An error out of NEXT is the operation's and passes through untouched."
  (let ((inside nil) (called nil) (answer '()) (failure nil))
    (flet ((through (&rest next-args)
             (setf called t inside t)
             (unwind-protect (setf answer (multiple-value-list (apply next next-args)))
               (setf inside nil))
             (values-list answer)))
      (block hook
        (handler-bind ((error (lambda (condition)
                                (unless inside
                                  (setf failure condition)
                                  (return-from hook)))))
          (return-from call-isolated (funcall invoke #'through))))
      (hook-failed name key failure)
      (if called (values-list answer) (apply next args)))))

(defun hook (name key fn)
  "Register FN on NAME under string KEY, replacing KEY's fn in place when it
is already registered — replace does not move a hook to the end of the
chain."
  ;; NAME is a point keyword, whose FN is (lambda (op next)), or a function
  ;; name, whose FN is (lambda (next &rest args)) and is installed around
  ;; every call of that function. FN's lambda list is checked against that
  ;; shape here: a fn the chain could not call is refused at registration,
  ;; never installed. Returns KEY.
  (check-type key string)
  (check-type fn function)
  (when (and (function-name-p name) (not (fboundp name)))
    (error "cannot hook ~s: no such function" name))
  ;; at registration, not the first dispatch, whose error takes every call with it
  (if (function-name-p name)
      (multiple-value-bind (low high) (function-arity (unadvised-function name))
        (unless (accepts-argument-counts-p fn (1+ (or low 0)) (and high (1+ high)))
          (error "cannot hook ~s with a function of lambda list ~s: a hook on a ~
                  function receives (next . the arguments of the call), and ~
                  ~s takes ~a, so the hook must take (next ~a); a hook on a ~
                  point — :tool, :frame, :credential, :round — is where fn ~
                  receives (op next)"
                 name (or (ignore-errors (sb-kernel:%fun-lambda-list fn)) "?")
                 name (cond ((null low) "any number of arguments")
                            ((null high) (format nil "at least ~d argument~:p" low))
                            ((= low high) (format nil "~d argument~:p" low))
                            (t (format nil "~d to ~d arguments" low high)))
                 (if (and high (= (or low 0) high))
                     (format nil "~{~a~^ ~}" (loop repeat high collect "arg"))
                     "&rest args"))))
      (unless (accepts-argument-counts-p fn 2 2)
        (error "cannot hook the point ~s with a function of lambda list ~s: ~
                a hook on a point receives (op next); a hook on a function ~
                name receives (next . the arguments of the call)"
               name (or (ignore-errors (sb-kernel:%fun-lambda-list fn)) "?"))))
  (let ((chain (cdr (assoc name *hooks* :test #'equal))))
    ;; Rebuild rather than mutate: dispatching threads read *HOOKS* without a
    ;; lock, so the one SETF below is the whole publication.
    (setf *hooks* (put-in-place *hooks* name (put-in-place chain key fn #'string=) #'equal)))
  ;; a fn registered again is a fresh start: what the old one did wrong is gone
  (forget-hook-failure name key)
  ;; the one encapsulation that routes NAME's calls through its advice chain
  (when (and (function-name-p name) (not (sb-int:encapsulated-p name 'hook)))
    (sb-int:encapsulate name 'hook
                        (lambda (next &rest args)
                          (apply (reduce (lambda (entry inner)
                                           (lambda (&rest args) (apply (cdr entry) inner args)))
                                         (cdr (assoc name *hooks* :test #'equal))
                                         :from-end t :initial-value next)
                                 args))))
  key)

(defmacro define-hook (name key lambda-list &body body)
  "Register (lambda LAMBDA-LIST . BODY) on NAME under string KEY — HOOK as
a defining form, so the scribe keeps it in the layer and the next boot
re-registers it."
  ;; NAME is written unquoted like a DEFUN's: a point keyword,
  ;; whose LAMBDA-LIST is (op next), or a function name, whose LAMBDA-LIST is
  ;; (next . its arguments). Re-evaluating replaces KEY's fn in place; (unhook
  ;; 'NAME KEY) removes it. Returns KEY.
  `(hook ',name ,key (lambda ,lambda-list ,@body)))

(defun unhook (name key &aux (pair (assoc name *hooks* :test #'equal))
                             (chain (cdr pair)))
  "Remove KEY's fn from NAME."
  ;; True when something was removed. The last hook leaving a function name
  ;; takes its encapsulation with it.
  (when (assoc key chain :test #'string=)
    (let ((rest (remove key chain :key #'car :test #'string=)))
      (setf *hooks* (if rest
                        (substitute (cons name rest) pair *hooks*)
                        (remove pair *hooks*)))
      (when (and (null rest) (function-name-p name))
        (ignore-errors (sb-int:unencapsulate name 'hook))))
    (forget-hook-failure name key)
    t))

(defun call-hooked (point op final &key isolate)
  "Run OP through POINT's hook chain with FINAL as the innermost
continuation. ISOLATE steps over a hook's own error (CALL-ISOLATED): the
caller's choice at a point whose hooks only watch."
  ;; Unisolated, callers place this inside their own error discipline: a hook
  ;; that signals fails the operation exactly as the operation itself failing
  ;; would. An error out of FINAL is the operation's either way.
  (funcall (reduce (lambda (entry next)
                     (if isolate
                         (lambda (op)
                           (call-isolated point (car entry)
                                          (lambda (through) (funcall (cdr entry) op through))
                                          next (list op)))
                         (lambda (op) (funcall (cdr entry) op next))))
                   (cdr (assoc point *hooks* :test #'equal))
                   :from-end t :initial-value final)
           op))

(defun unadvised-function (name &aux (info (and (fboundp name) (sb-int:encapsulated-p name 'hook))))
  "The function NAME calls once advice is peeled off: the definition under
this file's encapsulation, or the plain fdefinition."
  ;; What a definition snapshot compares, so advising a function is never
  ;; mistaken for redefining it.
  ;;
  ;; SB-INT:ENCAPSULATED-P answers two different things. For a plain function
  ;; it answers the ENCAPSULATION-INFO holding the definition underneath. For a
  ;; GENERIC FUNCTION it answers the encapsulation's INDEX in the generic's own
  ;; encapsulation list — 0 for the first — because there SBCL wraps the
  ;; discriminating function and leaves the fdefinition, the generic-function
  ;; object itself, untouched. So a generic's unadvised function is simply its
  ;; fdefinition, and only an ENCAPSULATION-INFO is ever asked for a
  ;; definition: without this, reading 0 as one would kill every boot whose
  ;; layer advises a generic.
  (if (sb-impl::encapsulation-info-p info)
      (sb-impl::encapsulation-info-definition info)
      (fdefinition name)))

;;; --- shapes a hook takes, written once -------------------------------------
;;; Every peripheral that watched the :FRAME point had written the same
;;; around: NEXT first, then its own fold inside a HANDLER-CASE. Every one
;;; that taught the model a vocabulary had written the same advice on the
;;; harness read. The shapes live here, beside the primitive they fit.

(defun observer (fn label)
  "A hook for a point that only watches: NEXT runs first, so the operation
lands before FN sees OP; then FN, whose error is warned under LABEL, never
failing the operation; NEXT's value is answered untouched."
  (lambda (op next)
    (prog1 (funcall next op)
      (handler-case (funcall fn op) (error (condition) (warn "~a: ~a" label condition))))))

(defun frame-fact (op &aux (payload (getf op :payload)))
  "When OP, a :FRAME op, carries a durable fact -- a session_stream_event
frame -- (values KIND PAYLOAD METADATA TURN-ID): the fact's wire kind, its
payload object, its envelope metadata, and the turn it belongs to."
  ;; NIL for every other frame kind.
  (when-let (event (and (equal "session_stream_event" (getf op :kind))
                        (json-value payload :object "frame" "event")))
    (let ((metadata (or (json-value event :object "metadata")
                        (make-hash-table :test #'equal))))
      (values (json-value event :string "type")
              (or (json-value event :object "payload")
                  (make-hash-table :test #'equal))
              metadata
              (or (json-value payload :string "frame" "turn_id")
                  (json-value metadata :string "turn_id"))))))

(defun fact-message-content (payload)
  "Assistant text from a turn.assistant_message_completed fact PAYLOAD."
  ;; The stored message is either a bare string or the wire message object,
  ;; whose content is a string, an array of typed parts, or JSON null (:NULL
  ;; -- a tool-only round). NIL when there is nothing deliverable.
  (let* ((message (and (hash-table-p payload) (gethash "message" payload)))
         (text (if (stringp message) message (nle:message-content message))))
    (and (stringp text) (plusp (length text)) text)))
