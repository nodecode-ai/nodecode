;;;; provisional.lisp --- a redefinition that must survive its own execution.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The scribe writes a definition to the layer the moment it evaluates, and
;;;; the layer loads at every boot. For almost every name that is right: the
;;;; act of defining IS the act of keeping, and a definition that turns out
;;;; wrong is forgotten with FMAKUNBOUND, one act in reverse.
;;;;
;;;; The exception is the name the organism is STANDING ON. Without the hold,
;;;; a definition that lands on a name already live in the call path doing the
;;;; defining would be written to the layer before it executes. If the body
;;;; calls a name absent from the image, every later turn would die at the top
;;;; of the loop before a single provider round, and the restart that should
;;;; heal it would load the same cut again. The one ingress that could undo
;;;; the act (an FMAKUNBOUND through eval) would need a turn, and turns are
;;;; exactly what is severed.
;;;;
;;;; The point is order: the harness persists the cut before executing it, so
;;;; no one is present to judge the evidence when it arrives.
;;;;
;;;; Three mechanisms, none of them a curated list:
;;;;
;;;;   THE STACK IS THE EVIDENCE. When the eval seam evaluates a DEFUN for a
;;;;   name, that name is a live frame of the very call doing the defining
;;;;   (run -> agent-loop -> execute-tool-call -> eval). "Load-bearing" is
;;;;   therefore not a judgment to maintain but a measurement to take: is the
;;;;   name I am defining on the stack I am defining it from? That question
;;;;   needs no vocabulary, no registry and no naming convention, and it
;;;;   answers for every future seam, every cell's own ingress and every name
;;;;   nobody thought to protect. NAME-ON-STACK-P is it.
;;;;
;;;;   THE FLOOR IS KEPT. *PRISTINE-DEFINITIONS* is the definition snapshot
;;;;   cells.lisp already takes before the first layer file loads, held for
;;;;   the life of the image: name -> the function object the image booted
;;;;   with. It is pointers, not text, so REVERT needs no reader, no
;;;;   compiler, no source root and no package resolution — which is the
;;;;   property that matters, because an image someone has just cannibalized
;;;;   is frequently too broken to compile its own repair.
;;;;
;;;;   THE WRITE FOLLOWS THE PROOF. A form that redefines a name on its own
;;;;   stack is not written to the layer at all. The new definition is
;;;;   installed in the image (this is Lisp; the model asked for a
;;;;   redefinition and gets one) under a HELD encapsulation, and the layer
;;;;   write is deferred until that definition has been entered and has
;;;;   returned once. One clean traversal and the scribe's ordinary write
;;;;   runs, unchanged, exactly as if it had never been delayed. A form that
;;;;   redefines several such names is one batch, written once the last of
;;;;   them has returned: a clean return of one name is no evidence about
;;;;   another (proofs/SelfEditHold.lean). A traversal that signals before
;;;;   then restores, for every name of its batch, the escrowed definition — the one in
;;;;   place before the traversal — and the session is woken through the one
;;;;   in-process ingress (*SESSION-WAKE*, the gateway's SUBMIT) with the
;;;;   condition that killed it. The image heals itself and then reports,
;;;;   through the loop it just un-broke.
;;;;
;;;;   THE PROOF COMES FROM THE STACK. A traversal proves the definition only
;;;;   when the code it displaced calls it. A call from inside a form the
;;;;   model is evaluating is the model calling its own definition, and it
;;;;   proves nothing. Without the stack proof, a definition of RUN (the turn
;;;;   runner) as (defun run (acts) ...) followed by (run '(...)) from the
;;;;   next eval would be taken as proof, written to the layer, and every
;;;;   later submit's (run prompt :session ...) would die on the argument
;;;;   count.
;;;;
;;;; --- what this deliberately does not do ---------------------------------
;;;;
;;;; It does not refuse a rewrite. Rewriting its own turn loop is the whole
;;;; point of the substrate; the fix is to make the rewrite survivable, not
;;;; forbidden. The one refusal is a measurement: a definition whose lambda
;;;; list cannot take the calls the displaced one takes (TAKES-EVERY-CALL-P)
;;;; can only fail its first traversal. A refusal costs the model a rename,
;;;; while a hold would cost the next turn.
;;;;
;;;; It does not attribute. Any ERROR escaping the first traversal reverts,
;;;; whether or not the new body is what raised it — because the two failure
;;;; modes are not symmetric. A good definition reverted over an unrelated
;;;; provider error costs one re-evaluation and says so in the wake text. A
;;;; bad definition kept because the harness could not prove blame costs the
;;;; whole image. The only exclusion is the turn cancel (Esc is the operator
;;;; unwinding the worker, not a verdict on the code) and the background
;;;; eval's abort, which is deliberately not an ERROR.
;;;;
;;;; It does not time out. A first traversal that never returns is
;;;; indistinguishable from a turn that legitimately runs for ten minutes,
;;;; and a wall clock would revert good definitions on slow provider rounds.
;;;; A body that hangs still hangs; Esc still ends it, and the definition
;;;; stays held (unwritten), so the next boot is clean either way.
;;;;
;;;; Known blind spot: SBCL erases a tail-called frame, so a name that only
;;;; ever appears in tail position is invisible to the stack read and takes
;;;; the ordinary immediate-write path. That is a false negative — the
;;;; behaviour before this file — never a false hold.

(in-package #:nodecode.kernel)

;;; --- the stack as evidence --------------------------------------------------

;; The turn loop sits ~10 frames under the eval seam; the limit only bounds a runaway recursion.
(defun backtrace-names (&key (limit 200) &aux (frames '()))
  "The names on the calling stack, innermost frame first: a list of lists,
one per frame, each holding that frame's symbol names."
  ;; Never signals — a stack read is evidence, and evidence that cannot be
  ;; gathered is simply absent.
  (ignore-errors
   (sb-debug:map-backtrace
    (lambda (frame &aux (names '()))
      ;; Every symbol name inside the frame's debug-fun name. That name is
      ;; a symbol for a plain function, but a list for everything else —
      ;; (SETF X), (LABELS RUN :IN CALL-HOOKED), (SB-PCL::FAST-METHOD VIEW
      ;; ...) — and a bare string for the runtime's own frames ("undefined
      ;; function"). Collecting every symbol in the form means a name shows
      ;; up whether it owns the frame or merely encloses it, which is the
      ;; answer the caller wants: enclosing IS standing on.
      (labels ((walk (node depth)
                 (when (< depth 8)
                   (cond ((and node (symbolp node)) (push (symbol-name node) names))
                         ((consp node)
                          (walk (car node) (1+ depth))
                          (walk (cdr node) (1+ depth)))))))
        (walk (ignore-errors (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))) 0))
      (push names frames))
    :count limit))
  (nreverse frames))

;;; The tool seam binds it at dispatch and the background eval snippet re-binds
;;; it on the thread that evaluates the form, because a detached evaluation
;;; runs on a worker whose own stack knows nothing about the turn that asked
;;; for it — and it is precisely the asking turn's stack that says what the
;;; form is standing on.
(defvar *caller-frames* nil
  "The frames the running operation was dispatched from, as BACKTRACE-NAMES
answers them, or NIL to read the live stack instead.")

(defun name-on-stack-p (name &optional (frames (or *caller-frames* (backtrace-names))))
  "Whether NAME — a string — names a function that is currently executing on
FRAMES."
  ;; The whole load-bearing predicate: no list to maintain, no convention, and
  ;; it answers for names that do not exist yet.
  (and (stringp name)
       (loop for frame in frames
               thereis (member name frame :test #'string=))
       t))

;;; --- the floor that is kept -------------------------------------------------

(defun evolved-symbol (name)
  "NAME — a symbol or a string — as the EVOLVED symbol it designates, or NIL
when nothing of that name is interned there."
  ;; Definitions land in EVOLVED whatever thread or package the caller was
  ;; reading in.
  (cond ((symbolp name) name)
        ((stringp name)
         (find-symbol name (find-package '#:nodecode.evolved)))))

;;; Set once by LOAD-CELLS. Function OBJECTS, not source text, so restoring
;;; one asks nothing of the reader, the compiler or the source tree — an image
;;; that has just had a load-bearing definition cut out from under it often
;;; cannot compile anything at all.
(defvar *pristine-definitions* nil
  "EVOLVED's DEFINITION-SNAPSHOT as the image booted, before the first layer
file loaded: (KIND . SYMBOL) -> object.")

;;; --- holding a definition until it runs -------------------------------------

;;; NIL in an image with no gateway, where a revert says its piece on the
;;; eval's own output and nowhere else.
(defvar *session-wake* nil
  "(SESSION-ID TEXT) — the one function the kernel calls to put text into a
session, set by the surface that owns ingress (gateway.lisp, over SUBMIT).")

(defstruct (held-batch (:conc-name batch-) (:copier nil))
  "One evaluated form held until its load-bearing definitions prove
themselves."
  ;; The form is the unit: a PROGN that defines a helper beside the loop
  ;; persists as a whole or not at all, which is what its author meant.
  (names '())
  ;; the names whose traversal has returned: the form is written when they are all of NAMES
  (proven '())
  (thunk nil)
  (session-id nil)
  (state :armed))

(access (batch held-batch))

(defstruct (held-definition (:conc-name held-) (:copier nil))
  "One held name: the symbol, the definition to put back if the traversal
signals, and the batch whose fate it shares."
  (symbol nil)
  (previous nil)
  (batch nil))

(access (held held-definition) (standing held-definition))

(defvar *evaluating* nil
  "True on a thread while it evaluates a form the model sent: a held
definition entered there is the model calling it, which proves nothing.")

(defun call-shape (lambda-list &aux (state :required) (min 0) (positional 0) (keys '())
                                    (rest nil) (key-p nil) (any nil))
  "LAMBDA-LIST's calls as (MIN MAX KEYS): MAX NIL when unbounded, KEYS :ANY
when any keyword passes, else the keyword names taken."
  (dolist (item lambda-list)
    (case item
      (&optional (setf state :optional))
      ((&rest &body) (setf state :rest rest t))
      (&key (setf state :key key-p t))
      (&allow-other-keys (setf any t))
      (&aux (setf state :aux))
      ;; an implementation's own &-word (SB-INT:&MORE) is no argument
      (t (case (if (and (symbolp item) (uiop:string-prefix-p "&" (symbol-name item))) :skip state)
           (:required (incf min) (incf positional))
           (:optional (incf positional))
           (:key (let ((head (if (consp item) (first item) item)))
                   (push (symbol-name (if (consp head) (first head) head)) keys)))))))
  (list min (unless (or rest key-p) positional)
        (if (or any (and rest (not key-p))) :any keys)))

(defun keyword-start (lambda-list &aux (state :required) (positional 0))
  "How many arguments LAMBDA-LIST binds positionally before its keywords
begin, or NIL when it takes no &KEY."
  (dolist (item lambda-list nil)
    (case item
      (&optional (setf state :optional))
      ((&rest &body &aux) (setf state :other))
      (&key (return positional))
      (t (when (and (member state '(:required :optional))
                    (not (and (symbolp item) (uiop:string-prefix-p "&" (symbol-name item)))))
           (incf positional))))))

(defun keywords-aligned-p (new-start any old-min old-max old-start)
  "Whether a lambda list reading keywords from NEW-START, any keyword when
ANY, reads every call of one taking OLD-MIN to OLD-MAX arguments (NIL,
unbounded) with keywords from OLD-START (NIL when it takes none): under
&ALLOW-OTHER-KEYS only the keyword part's length must come out even (SBCL
checks nothing else there); otherwise the old positional arguments, which
can be any values, must end where the keywords begin, and the old keywords
begin there too."
  (cond ((null new-start) t)
        (any (if old-start
                 (and (evenp (- old-start new-start))
                      (or (<= old-start new-start) (<= old-start old-min)))
                 (and old-max
                      (or (<= old-max new-start)
                          (and (= (max old-min (1+ new-start)) old-max)
                               (evenp (- old-max new-start)))))))
        (old-start (= new-start old-start))
        (t (and old-max (<= old-max new-start)))))

(defun takes-every-call-p (new old)
  "Whether lambda list NEW takes every call lambda list OLD takes: argument
counts, keywords, and where the keywords begin (proofs/SelfEditHold.lean)."
  (destructuring-bind (new-min new-max new-keys) (call-shape new)
    (destructuring-bind (old-min old-max old-keys) (call-shape old)
      (let ((new-start (keyword-start new)) (old-start (keyword-start old)))
        (and (<= new-min old-min)
             (or (null new-max) (and old-max (>= new-max old-max)))
             (or (null old-keys) (eq new-keys :any)
                 (and (listp old-keys) (subsetp old-keys new-keys :test #'string=)))
             ;; NEW reads keywords from NEW-START on: (x &optional y) -> (x &key y)
             ;; would read the caller's (f 1 2) as a keyword list
             (keywords-aligned-p new-start (eq new-keys :any) old-min old-max old-start))))))

(defvar *held* (make-hash-table :test 'equal)
  "NAME -> HELD-DEFINITION for every definition still waiting on its first
traversal.")

;;; A held definition is armed on the thread that evaluated it — a background
;;; eval's worker — and decided on the turn worker that traverses it.
(defvar *held-lock* (bt2:make-lock :name "held-definitions")
  "The table and every batch's state transition.")

(defun definition-escrow (form &optional (frames nil frames-p) &aux refused out)
  "What FORM is about to redefine out from under the stack it is running on:
((NAME SYMBOL PREVIOUS-DEFINITION) ...), empty when it redefines nothing
load-bearing."
  ;; Called BEFORE the eval — the definition it displaces is only readable
  ;; while it is still standing. Signals one thing, the refusal of a DEFUN
  ;; that cannot take its callers' calls, so the form is never evaluated; any
  ;; other defect here must not turn a working eval into a turn error. A
  ;; fork's form that would define or forget a name not its own is refused
  ;; here too, unrun (FORK-FENCE, fork.lisp).
  (fork-fence form)
  (prog1
      (ignore-errors
        ;; No layer, no scribe, no hold: a posture that records nothing (the
        ;; test image, a trial child) has nothing to defer, and the whole
        ;; discipline stays one gate rather than two that can drift.
        (when *cells-directory*
          (let ((frames (if frames-p frames (or *caller-frames* (backtrace-names)))))
            (walk-toplevel-acts
             form
             (lambda (act &aux (key (definition-key act)))
               ;; a macro is never traversed; an added method leaves its generic EQ
               (when (and key (member (getf key :operator) '("DEFUN" "DEFGENERIC")
                                      :test #'string=))
                 (let* ((name (getf key :name))
                        (symbol (and name (evolved-symbol name))))
                   (when (and symbol (fboundp symbol)
                              (name-on-stack-p name frames)
                              (not (find name out :key #'first :test #'string=)))
                     (let* ((previous (unadvised-function symbol))
                            (old (handler-case (sb-kernel:%fun-lambda-list previous) (error () :unknown)))
                            (new (third act)))
                       ;; :UNKNOWN when the displaced function was compiled without its lambda list
                       (when (and (string= "DEFUN" (getf key :operator))
                                  (listp old) (listp new) (not (takes-every-call-p new old)))
                         (setf refused (list name old new)))
                       (push (list name symbol previous) out)))))))
            (nreverse out))))
    (when refused
      (destructuring-bind (name old new) refused
        (error "~a is running on the stack this eval was dispatched from, and its ~
                callers call it as ~(~a~). This definition takes ~(~a~), which cannot ~
                take those calls, so the form was not evaluated. The name belongs to ~
                the code running it: give the new function a name of its own."
               name old new)))))

(defun reverted-text (names condition)
  "What a session is woken with when the first traversal of the held NAMES signalled CONDITION."
  (format nil "[definition reverted] ~{~a~^, ~} ~:[was~;were~] redefined from inside a call to ~
               ~:[it~;them~], so the redefinition was held and never written to the layer. Its ~
               first traversal signalled ~a: ~a~%~%The previous definition is restored and the ~
               image is running on it again; nothing persisted, so a restart is clean. Fix the ~
               body and evaluate it again — or, if that condition looks unrelated to your ~
               change, evaluate the same definition again to retry the traversal."
          names (rest names) (rest names) (type-of condition)
          (handler-case (princ-to-string condition) (error () "<condition could not be printed>"))))

(defun hold-definitions (escrow form form-string end
                         &key (session-id *scribe-session-id*))
  "Hold FORM's recording until every definition in ESCROW has run once."
  ;; Answers the held names, or NIL for an empty ESCROW — the caller then
  ;; records the form the ordinary way, immediately, as it always has.
  ;;
  ;; The layer write is not reimplemented here: the batch carries the scribe's
  ;; own call, replayed verbatim when the traversal confirms.
  (when escrow
    (let* ((package (package-name *package*))
           (batch (make-held-batch
                   :names (mapcar #'first escrow)
                   :session-id session-id
                   :thunk (lambda ()
                            (record-definitions form form-string end
                                                :session-id session-id
                                                :package package)))))
      (bt2:with-lock-held (*held-lock*)
        ;; Re-arming a name that is already held inherits the ORIGINAL
        ;; escrow. The definition standing there right now is the
        ;; unproven one; putting THAT back would restore the very cut
        ;; the hold exists to undo.
        (loop for (name symbol previous) in escrow
              for standing = (gethash name *held*)
              do (setf (gethash name *held*)
                       (make-held-definition
                        :symbol symbol :batch batch
                        :previous (if standing standing.previous previous)))))
      ;; Each name's calls route through the traversal below, keyed apart from the
      ;; hook chain's encapsulation so advice and a hold each come off alone.
      (dolist (entry escrow)
        (let ((name (first entry)))
          (unless (sb-int:encapsulated-p (second entry) 'held)
            (sb-int:encapsulate
             (second entry) 'held
             ;; a normal return is the proof; an ERROR escaping the call is the verdict;
             ;; a call from inside a model's eval is neither
             (lambda (next &rest args &aux (held (bt2:with-lock-held (*held-lock*) (gethash name *held*))))
               (if (or (null held) *evaluating*)
                   (apply next args)
                   (let ((batch held.batch))
                     (handler-bind ((error
                                      (lambda (condition)
                                        ;; Esc is the operator unwinding the worker, not
                                        ;; a verdict on the code. It leaves the batch
                                        ;; armed for the next traversal to judge.
                                        (unless (typep condition 'turn-cancelled-condition)
                                          ;; put back what the hold displaced, before the unwind, then wake
                                          (let ((names (and (claim-batch batch :reverted)
                                                            (release-held batch :restore t)))
                                                (session-id batch.session-id))
                                            (when (and names *session-wake* session-id)
                                              (ignore-errors
                                               (funcall *session-wake* session-id
                                                        (reverted-text names condition)))))))))
                       (multiple-value-prog1 (apply next args)
                         ;; this name returned; once every name of the batch has, the scribe
                         ;; records the form as it would have at the eval
                         (when (prove-held batch name)
                           (release-held batch)
                           (handler-case (funcall batch.thunk)
                             (error (condition)
                               ;; The scribe's own contract: a write defect is a line, never a
                               ;; turn error. This one has no eval output left to land on, so it
                               ;; goes where a mid-turn line can go — nowhere but the log.
                               (warn "held definitions not recorded: ~a" condition)))))))))))))
      (format t "~&[layer: ~{~a~^, ~} ~:[is~;are~] running on the stack this eval was ~
                 dispatched from — the definition is live but HELD: it is written to the ~
                 layer after ~:[one clean traversal~;each name has returned from one clean ~
                 traversal~], and reverts to what it displaced if a traversal signals ~
                 first. (held-definitions) lists what is waiting, (revert 'NAME) puts one ~
                 back by hand.]~%"
              (mapcar #'first escrow) (rest escrow) (rest escrow))
      batch.names)))

(defun claim-batch (batch state)
  "Move BATCH from :ARMED to STATE, once, ever."
  ;; Answers true on the winning claim. A recursive function traverses its own
  ;; encapsulation once per round — AGENT-LOOP tail-calls itself per provider
  ;; round — so confirm and revert both arrive repeatedly and exactly one of
  ;; them may act.
  (bt2:with-lock-held (*held-lock*) (when (eq :armed batch.state) (setf batch.state state) t)))

(defun prove-held (batch name)
  "Count NAME's traversal as returned for BATCH, and claim BATCH :CONFIRMED
when that leaves none of its names unproven: true on the winning claim."
  ;; A return of one held name says nothing about another. Of AGENT-LOOP and
  ;; RUN-TOOL-CALL redefined in one form, the dispatcher returns at the
  ;; round's next tool call, before the loop has entered its new body at all.
  (bt2:with-lock-held (*held-lock*)
    (when (eq :armed batch.state)
      (pushnew name batch.proven :test #'string=)
      (when (subsetp batch.names batch.proven :test #'string=)
        (setf batch.state :confirmed)
        t))))

(defun release-held (batch &key restore)
  "Take BATCH's names out of the held table and off their encapsulations,
restoring each escrowed definition when RESTORE."
  ;; Answers the names it released. Order matters: the encapsulation comes off
  ;; first, because FDEFINITION under a live encapsulation writes UNDER it and
  ;; would leave the trial wrapper standing over a definition it no longer
  ;; owns.
  (bt2:with-lock-held (*held-lock*)
    (loop for name in (batch-names batch)
          for held = (gethash name *held*)
          when (and held (eq batch held.batch))
            do (remhash name *held*)
               (let ((symbol held.symbol))
                 (ignore-errors (sb-int:unencapsulate symbol 'held))
                 (when restore
                   (ignore-errors
                    (if held.previous
                        (setf (fdefinition symbol) held.previous)
                        (fmakunbound symbol)))))
            and collect name)))

;;; --- the operator's and the model's verbs -----------------------------------

(defun held-definitions ()
  "What is waiting on a first traversal: ((:name N :session-id S :state
STATE) ...). The read behind (held)."
  (sort (bt2:with-lock-held (*held-lock*)
          (loop for name being the hash-keys of *held* using (hash-value held)
                collect (list :name name
                              :session-id (batch-session-id held.batch)
                              :state (batch-state held.batch)
                              :returned (and (member name (batch-proven held.batch) :test #'string=) t)
                              :restores (and held.previous t))))
        #'string< :key (lambda (entry) (getf entry :name))))

(defun revert (name &aux (symbol (evolved-symbol name))
                         (label (if symbol (symbol-name symbol) (princ-to-string name)))
                         (held (and symbol
                                    (bt2:with-lock-held (*held-lock*) (gethash label *held*))))
                         ;; the function the name had when the image booted
                         (pristine (and symbol (not held) (hash-table-p *pristine-definitions*)
                                        (gethash (cons :function symbol) *pristine-definitions*))))
  "Put NAME's function definition back and answer what happened, in words."
  ;; The escrow of a definition still held comes first — it is the newest
  ;; thing known to have worked — and the image's pristine boot definition
  ;; second. One SETF of an FDEFINITION: no reader, no compiler, no source
  ;; tree, which is what makes it usable from an image that can no longer
  ;; compile.
  ;;
  ;; Reverting is an IMAGE act and says nothing about the layer: a definition
  ;; that reached the layer is taken out of it the way it went in, with
  ;; (fmakunbound 'NAME).
  (cond
    ((null symbol) (format nil "~a names nothing in this image" label))
    (held
     (claim-batch held.batch :reverted)
     (release-held held.batch :restore t)
     (format nil "~a reverted to the definition it displaced; the held ~
                  form was not recorded" label))
    ((null pristine)
     (format nil "~a has no escrow and was not in the image at boot; ~
                  nothing to revert to" label))
    ((eq pristine (ignore-errors (unadvised-function symbol)))
     (format nil "~a already is the definition this image booted with" label))
    (t
     (setf (fdefinition symbol) pristine)
     (format nil "~a reverted to the definition this image booted with" label))))

;;; --- the battery: the whole layer judged as a name on the stack is ---------
;;;
;;; The hold judges one definition, at the eval, against the stack it was
;;; dispatched from. A folder loads with no stack to read: its files redefine
;;; shipped names at boot, in a fresh process, and the first caller to learn
;;; that an override cannot take its calls is the first turn of the organism
;;; that loaded it. The keeper's trial child (launch.lisp) boots the folder
;;; exactly as a launch does and has the whole report in hand, so the question
;;; the hold asks of one name is asked there of every override at once: does
;;; the function this name now runs take every call the function it displaced
;;; takes? The floor says what was displaced -- *PRISTINE-DEFINITIONS* holds
;;; the booted objects -- and the fdefinition under any advice is what runs.
;;; The verdict gates the seal of a generation and the warm image built from
;;; the same proof (generation.lisp). Nothing here refuses a load or reverts a
;;; name: the folder is the operator's, and the verdict is said, not enforced.

(defun function-lambda-list (function)
  "FUNCTION's lambda list, or :UNKNOWN when it was compiled without one."
  ;; A generic's own lambda list: SB-KERNEL:%FUN-LAMBDA-LIST answers its
  ;; discriminating function's (&REST ARGS) for one, which takes everything.
  (handler-case
      (if (typep function 'generic-function)
          (sb-mop:generic-function-lambda-list function)
          (sb-kernel:%fun-lambda-list function))
    (error () :unknown)))

(defun overrides-take-their-callers-calls (&optional (records (cells)) &aux failures seen)
  "Whether every shipped function a folder in RECORDS redefines still takes
the calls the shipped one takes. => (values OK FAILURES JUDGED): a failure is
(NAME SHIPPED-LAMBDA-LIST OVERRIDE-LAMBDA-LIST), the override's :UNBOUND when
the name has no function left at all; JUDGED counts the names read."
  ;; Every (:function . SYMBOL) override key, each name once whatever folders
  ;; list it -- the live definition is one function. A lambda list that cannot
  ;; be read on either side is no evidence and passes, as the hold's refusal
  ;; passes it; a name whose shipped object the floor does not hold is nothing
  ;; this image can judge.
  (dolist (record records)
    (loop for key in (cell-overrides record)
          for symbol = (cdr key)
          for shipped = (and (eq (car key) :function)
                             (not (member symbol seen))
                             (hash-table-p *pristine-definitions*)
                             (gethash key *pristine-definitions*))
          when shipped
            do (push symbol seen)
               (let ((old (function-lambda-list shipped))
                     (new (if (fboundp symbol)
                              (function-lambda-list (unadvised-function symbol))
                              :unbound)))
                 (when (or (eq new :unbound)
                           (and (listp old) (listp new) (not (takes-every-call-p new old))))
                   (push (list (symbol-name symbol) old new) failures)))))
  (setf failures (sort failures #'string< :key #'first))
  (values (null failures) failures (length seen)))

(defun probe-overrides (report-path)
  "The trial child's form: judge the folder this process loaded, write the
verdict to REPORT-PATH as JSON and answer 1 on green, 0 on red."
  ;; {"verdict":"red","overrides":3,"failures":[{"name":"RUN","shipped":"(prompt
  ;; &key session)","override":"(acts)"}]}: the names as the layer spells
  ;; them, the lambda lists as text, so the keeper's notice and the list
  ;; (generation.lisp) read them without this image's symbols.
  (multiple-value-bind (ok failures judged) (overrides-take-their-callers-calls)
    (flet ((text (lambda-list)
             (if (listp lambda-list)
                 (with-standard-io-syntax
                   (let ((*package* (find-package '#:nodecode.evolved)))
                     (string-downcase (prin1-to-string lambda-list))))
                 (string-downcase (symbol-name lambda-list)))))
      (write-file-atomically
       report-path
       (encode-json-object
        (json-object "verdict" (if ok "green" "red")
                     "overrides" judged
                     "failures" (map 'vector
                                     (lambda (failure)
                                       (destructuring-bind (name old new) failure
                                         (json-object "name" name
                                                      "shipped" (text old)
                                                      "override" (text new))))
                                     failures)))))
    (if ok 1 0)))
