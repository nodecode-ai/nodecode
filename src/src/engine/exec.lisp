;;;; exec.lisp --- background evaluation substrate: registry, band material.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lisp port of src/tools/background_exec.zig, folded onto the one
;;;; tool this image registers: eval (registered here, over turn.lisp's
;;;; eval snippet) parks still-evaluating forms in the registry and keeps them
;;;; evaluating on an image thread under the run tool's yield-and-background
;;;; lifecycle. One authority for every piece of work that outlives its tool
;;;; call: the parked awaits, the gateway watcher's live-only output frames,
;;;; and the band rows all read the same state.
;;;;
;;;; Retention horizons (src/protocol/background_exec.zig authority table):
;;;;   - Durable: the turn.tool_result facts — the backgrounding handoff
;;;;     written by the tool call and the watcher-appended exit fact
;;;;     (lifecycle.lisp RECORD-BACKGROUND-SHELL-EXIT).
;;;;   - Live-only: session_background_process_updated frames (including the
;;;;     subscribe-time registry snapshots) and the band rows they feed.
;;;;     Neither replays.

(in-package #:nodecode.evolved)

(nlk:access (ambient-turn nlk::durable-turn) (stage shell-command))

;;; --- limits and configuration ---------------------------------------------

(defconstant +exec-max-output-bytes+ (* 1024 1024)
  "Total retained output bytes per process; older bytes drop from the front.")
(defconstant +exec-default-poll-yield-ms+ 600000)

;;; --- octet and text plumbing ----------------------------------------------


(defun octets-to-text (octets)
  "Decode sanitized (valid-UTF-8-by-construction) OCTETS to a string; a
defensive latin-1 fallback keeps a torn buffer from crashing a projection."
  (with-handlers ((error () (map 'string #'code-char octets)))
    (sb-ext:octets-to-string (coerce octets 'nlk:octets) :external-format :utf-8)))

(defun utf8-cut (bytes index &key back)
  "Snap INDEX off a UTF-8 continuation byte — forward by default, backward
with BACK — because a cut landing there would leave an invalid partial
character in whatever the cut produces."
  (loop with limit = (if back 0 (length bytes))
        while (and (/= index limit) (= (logand (aref bytes index) #xc0) #x80))
        do (incf index (if back -1 1))
        finally (return index)))

;;; --- streaming text sanitizer (port of src/tools/pty.zig TextSanitizer) ---
;;; Raw terminal bytes -> plain text: printable ASCII, '\n' and '\t' pass, a
;;; lone '\r' becomes '\n' (CRLF collapses), CSI/OSC/DCS/PM/APC sequences,
;;; two-byte ESC sequences, other C0 controls, and invalid UTF-8 drop. It is
;;; STATEFUL: a chunked stream must reuse one sanitizer so an escape sequence
;;; or multi-byte character torn across chunk boundaries never leaks.

(defstruct (text-sanitizer (:copier nil))
  (state :ground :type keyword)
  (utf8-buf (make-array 4 :element-type '(unsigned-byte 8)) :read-only t)
  (utf8-len 0 :type (integer 0 4))
  (utf8-need 0 :type (integer 0 4))
  (pending-cr nil :type boolean))

(nlk:access (sanitizer text-sanitizer))

(defun sanitizer-escape-kind (byte)
  (case byte
    (91 :csi)                              ; [
    ((93 80 94 95 88) :string)             ; ] P ^ _ X
    ((40 41 42 43 35 32 37) :charset)      ; ( ) * + # SP %
    (27 :escape)
    (t :ground)))

(defun octet-buffer (capacity)
  "An empty octet vector that grows, sized for CAPACITY octets."
  (make-array capacity :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun sanitize-octets (sanitizer bytes &aux (out (octet-buffer (max 16 (length bytes))))
                                             (dropped 0)
                                             (buf sanitizer.utf8-buf))
  "Reduce raw terminal BYTES through SANITIZER => (values OCTETS DROPPED):
fresh plain-text octets and how many terminal control sequences this call
removed — every ESC-led sequence and every bare C0/DEL byte."
  ;; State persists across calls for torn sequences. The count is what lets a
  ;; consumer say the controls existed: the reducer's one lossy step,
  ;; otherwise invisible, since the Zig original kept a raw lane beside it and
  ;; this port has none.
  (symbol-macrolet ((state sanitizer.state)
                    (len sanitizer.utf8-len)
                    (need sanitizer.utf8-need)
                    (pending-cr sanitizer.pending-cr))
    (labels ((emit (byte) (vector-push-extend byte out))
             (begin-escape ()
               (incf dropped)
               (setf state :escape))
             (ground-byte (byte)
               (when (plusp need)
                 (cond ((= (logand byte #xc0) #x80)
                        (setf (aref buf len) byte)
                        (incf len)
                        (when (= len need)
                          ;; overlong encodings, surrogates and > U+10FFFF drop (utf8ValidateSlice)
                          (when (let ((code (reduce (lambda (code byte)
                                                      (logior (ash code 6) (logand byte #x3f)))
                                                    buf :start 1 :end len :initial-value
                                                    (ldb (byte (- 7 len) 0) (aref buf 0)))))
                                  (and (>= code (aref #(0 0 #x80 #x800 #x10000) len))
                                       (<= code #x10ffff) (not (<= #xd800 code #xdfff))))
                            (dotimes (i len)
                              (emit (aref buf i))))
                          (setf len 0 need 0))
                        (return-from ground-byte))
                       ;; Torn character: drop it and process this byte fresh.
                       (t (setf len 0 need 0))))
               (when pending-cr
                 (setf pending-cr nil)
                 (emit 10)
                 (when (= byte 10) (return-from ground-byte)))
               (cond
                 ((= byte #x1b) (begin-escape))
                 ((= byte 13) (setf pending-cr t))
                 ((or (= byte 10) (= byte 9) (<= #x20 byte #x7e)) (emit byte))
                 ;; any other byte under #x80 is a C0 control or DEL
                 ((< byte #x80) (incf dropped))
                 (t (when-let (lead (cond ((= (logand byte #xe0) #xc0) 2)
                                          ((= (logand byte #xf0) #xe0) 3)
                                          ((= (logand byte #xf8) #xf0) 4)))
                      (setf (aref buf 0) byte len 1 need lead))))))
      (loop for byte across bytes
            do (case state
                 (:ground (ground-byte byte))
                 (:escape (setf state (sanitizer-escape-kind byte)))
                 (:charset (setf state :ground))
                 (:csi (cond ((<= #x40 byte #x7e) (setf state :ground))
                             ((= byte #x1b) (begin-escape))))
                 (:string (cond ((= byte 7) (setf state :ground))
                                ((= byte #x1b) (setf state :string-escape))))
                 (:string-escape
                  (cond ((= byte 92) (setf state :ground)) ; ST = ESC \ closes the string
                        ;; Any other byte after ESC began a new sequence.
                        (t (incf dropped)
                           (setf state (sanitizer-escape-kind byte)))))))))
  (values out dropped))

(defun note-removed-controls (text dropped)
  "TEXT with one trailing line naming the DROPPED terminal control sequences
the sanitizer removed, unchanged when nothing was."
  ;; The model's lane has no raw stream beside it, so without this line a form
  ;; that printed SGR colour or cursor motion reads as if it printed plain
  ;; text.
  (if (plusp dropped)
      (format nil "~a~&[~d terminal control sequence~:p removed by the output sanitizer]"
              text dropped)
      text))

(defun disclose-sanitized-text (text)
  "TEXT sanitized, closed by the removal line when anything was removed:
the model lane's one shape for text that never streamed through a
sanitizer — a printed eval value, the inline eval's captured output."
  ;; The btop bytes a (uiop:run-program ... :output :string) form RETURNED
  ;; rode into the result as its printed value while only the stream was
  ;; clean.
  (multiple-value-bind (clean dropped)
      ;; a lone surrogate (a form can return (code-char #xD800)) encodes as `?'
      (sanitize-octets (make-text-sanitizer)
                       (sb-ext:string-to-octets
                        text :external-format '(:utf-8 :replacement #\?)))
    (note-removed-controls (octets-to-text clean) dropped)))

;;; --- band preview material (port of src/tui/model/background.zig) ---------
;;; Pure functions shared by the client reducer (client.lisp) and, later, the
;;; band renderer. Previews are bounded on both ends so a chatty process
;;; cannot grow the model without bound.

(defconstant +background-preview-head-bytes+ 1024)
(defconstant +background-preview-head-lines+ 2)

(defun appended-preview-tail (old chunk)
  "OLD ++ CHUNK keeping at most the last 2048 bytes, front trim snapped off
UTF-8 continuation bytes. Fresh simple octet vector."
  (let ((all (concatenate '(vector (unsigned-byte 8)) old chunk)))
    (subseq all (utf8-cut all (max 0 (- (length all) 2048))))))

(defun kept-tail-text (tail cut &aux (at (and cut (position #\Newline tail))))
  "TAIL, the last lines an exec's output kept, as a reader shows them: past
its first line when CUT -- a tail kept by bytes begins partway into a line --
unless that line is all of it."
  (if at (subseq tail (1+ at)) tail))

(defun advance-display-lines (octets &optional (complete 0) dirty limit)
  "(values COMPLETE DIRTY END): the completed non-empty display lines and
whether the open line has visible content, carried from COMPLETE and DIRTY
across sanitized OCTETS; END is where the walk stopped — past the newline
that brought COMPLETE to LIMIT, else the end."
  ;; \r is not content; empty lines never display.
  (loop for byte across octets
        for index from 0
        do (cond ((= byte 10)
                  (when dirty (incf complete))
                  (setf dirty nil)
                  (when (and limit (>= complete limit))
                    (return (values complete dirty (1+ index)))))
                 ((/= byte 13) (setf dirty t)))
        finally (return (values complete dirty (length octets)))))

(defun appended-preview-head (old chunk)
  "Grow the frozen head prefix by one sanitized CHUNK; NIL when the head is
already frozen (2 display lines or the 1 KiB cap) or CHUNK adds nothing."
  (unless (or (zerop (length chunk))
              (>= (length old) +background-preview-head-bytes+))
    (multiple-value-bind (complete dirty) (advance-display-lines old)
      (when (< complete +background-preview-head-lines+)
        (let* ((take (nth-value 2 (advance-display-lines
                                   chunk complete dirty +background-preview-head-lines+)))
               (room (- +background-preview-head-bytes+ (length old)))
               (take (if (> take room) (utf8-cut chunk room :back t) take)))
          (when (plusp take)
            (concatenate '(vector (unsigned-byte 8)) old (subseq chunk 0 take))))))))

(defun last-redraws (octets &aux (out (octet-buffer (length octets)))
                                     (line 0) (return-p nil))
  "OCTETS with each line cut to what follows its last carriage return: what a
terminal shows of a progress bar redrawn in place, where the sanitizer would
run every redraw into one line."
  ;; A return waits for the next byte: CRLF is a newline, and a line that
  ;; ends in a return keeps the redraw it drew last.
  (loop for byte across octets
        do (cond ((= byte 13) (setf return-p t))
                 ((= byte 10) (vector-push byte out) (setf line (fill-pointer out) return-p nil))
                 (t (when return-p (setf (fill-pointer out) line return-p nil))
                    (vector-push byte out))))
  out)

;;; --- band row (the client reducer's state element) --------------------------
;;; Mirror of src/tui/model/background.zig BackgroundProcessRow. :PREPARING
;;; is UI-only — the wire only ever reports running/exited. STREAM-INDEX is a
;;; lisp-dialect addition: the lisp gateway's item_delta tool_call frames
;;; carry a provider index but no call id, so pre-call-id stream fragments
;;; key by index until the durable tool_call_started fact adopts the row.

(defstruct (bg-band-row (:copier nil))
  (shell-session-id nil)
  (tool-call-id nil)
  (stream-index nil)
  ;; What the exec runs, as the call wrote it, newlines and all: a surface
  ;; shows its lines (COMMAND-ROWS), or flattens it where it has one line.
  (command "" :type string)
  (activity-events 0 :type integer)
  (status :running :type (member :preparing :running))
  (output-tail (make-array 0 :element-type '(unsigned-byte 8)))
  (output-head (make-array 0 :element-type '(unsigned-byte 8)))
  (output-seen-lines 0 :type integer)
  (output-line-dirty nil :type boolean)
  (sanitizer (make-text-sanitizer))
  (streamed-args "" :type string))

(defun note-preview-lines (row clean-chunk)
  "Advance ROW's display-line counters over one sanitized chunk."
  (setf (values (bg-band-row-output-seen-lines row)
                (bg-band-row-output-line-dirty row))
        (advance-display-lines clean-chunk (bg-band-row-output-seen-lines row)
                               (bg-band-row-output-line-dirty row))))

(defun command-preview (raw)
  "(values TEXT EXTRA-LINES): flatten a possibly multi-line command into one
line -- a folded band row's, a listing's -- whitespace/control runs collapse
to one space, ends trimmed, EXTRA-LINES counts non-empty source lines beyond
the first."
  (values (string-trim " " (ppcre:regex-replace-all "[\\x00-\\x20\\x7f]+" raw " "))
          (max 0 (1- (count-if (lambda (line) (ppcre:scan "[^\\x00-\\x20\\x7f]" line))
                               (uiop:split-string raw :separator '(#\Newline)))))))

;;; --- partial-JSON command extraction (tool_render.zig port) ----------------
;;; The preparing band row's command preview grows in place while argument
;;; fragments stream: the probe tolerates a truncated tail, returning the
;;; still-open string value of "command"/"cmd"/"code".

(defun decode-json-string-body (encoded)
  "ENCODED — a JSON string's body as the wire carries it, possibly cut
mid-escape while it streams — decoded."
  ;; Every escape JSON has gives its character, and a cut escape is dropped
  ;; rather than guessed.
  (ppcre:regex-replace-all
   ;; past the escape: \u and up to four characters, else one
   ;; A backslash with nothing after it escapes nothing.
   "(?s)\\\\(?:u(.{0,4})|(.)|\\z)" encoded
   (lambda-ignoring (match hex other)
     (cond ((and hex (= 4 (length hex)))
            (let ((code (parse-integer hex :radix 16 :junk-allowed t)))
              (if code (string (code-char code)) "")))
           ;; \" \\ and \/ give their own character, as does an unknown escape
           (other (string (case (char other 0)
                            (#\n #\Newline) (#\t #\Tab) (#\r #\Return)
                            (#\b #\Backspace) (#\f #\Page) (t (char other 0)))))
           (t "")))
   :simple-calls t))

(defun partial-json-string (text keys &key (pick :earliest))
  "The string value TEXT gives one of KEYS -- one key or a list of them --
decoded (DECODE-JSON-STRING-BODY), or NIL. PICK :EARLIEST
takes the key whose name sits earliest in TEXT and answers NIL when its
value is not a string; PICK :SCANNING walks on past an occurrence that names
no string and tries the next one."
  ;; A probe, not a parse: the quoted key it searches for can never be string
  ;; CONTENT (JSON escapes every quote inside a string), and every tool
  ;; argument object this image streams is flat -- eval's schema is `form'
  ;; beside `yield_time_ms' -- so a key hit is a top-level hit. A cut tail is
  ;; tolerated: an unterminated value answers the prefix that has streamed so
  ;; far. :EARLIEST is document order, the order the object walk this replaced
  ;; read the keys in, and a non-string value there answers NIL rather than
  ;; falling through to a later key -- which is what makes a streaming band row
  ;; show the command rather than a ($ "name") placeholder.
  (let ((len (length text))
        (needles (ppcre:create-scanner (format nil "\"(?:~{~a~^|~})\""
                                               (mapcar #'ppcre:quote-meta-chars
                                                       (alexandria:ensure-list keys))))))
    (flet ((body (end)
             ;; the value's encoded body after the key hit ending at END, NIL when the
             ;; key names no string there: past JSON blanks, the colon, blanks and the quote
             (when-let (start (nth-value 1 (ppcre:scan "\\A[ \\t\\n\\r]*:[ \\t\\n\\r]*\""
                                                       text :start end)))
               (let ((end start))
                 (loop while (and (< end len) (char/= #\" (char text end)))
                       do (incf end (if (char= #\\ (char text end)) 2 1)))
                 (subseq text start (min end len))))))
      ;; the first key hit in TEXT answers, or under :SCANNING the first naming a string
      (when-let (encoded (ppcre:do-matches (hit end needles text)
                           (let ((found (body end)))
                             (when (or found (eq pick :earliest)) (return found)))))
        (decode-json-string-body encoded)))))

(defun shell-command-preview-from-args (arguments)
  "The command possibly-incomplete exec-call ARGUMENTS carry, decoded as far
as it has streamed, newlines and all, or NIL until the command key's string
value opens."
  ;; The keys are +TOOL-COMMAND-KEYS+, the list the complete decode
  ;; (TOOL-COMMAND-STRING) reads once the durable fact lands. An eval call's
  ;; own literal references resolve here too — ($ "name") grows into its
  ;; strings entry as that entry streams — so the preparing row shows the
  ;; command, not the placeholder.
  (when-let (command (partial-json-string arguments +tool-command-keys+))
    (nlk:deref-literal-refs
     command
     (lambda (name)
       (partial-json-string arguments name :pick :scanning)))))


;;; --- tool family (src/protocol/cell_effects.zig) ---------------------------

(defun seeds-background-band-p (name)
  "Streamed calls of these tool names seed a preparing background-band row."
  ;; An exec call lives under the transcript status bar for its whole
  ;; in-flight life — argument streaming AND execution (the wire cannot tell
  ;; them apart: the durable result is the first fact after dispatch) — and
  ;; its tool panel joins the transcript when the result retires the row. The
  ;; zig set is the shell families; "eval" is the lisp-dialect addition with
  ;; no cell_effects.zig counterpart — the organism's own exec, which takes
  ;; the same in-flight surface treatment.
  ;; EQUALP compares two strings as STRING-EQUAL does, and a name that is no
  ;; string equals none of them.
  (and (member name '("snippet" "run" "bash" "shell" "sh" "exec_command"
                      "eval")
               :test #'equalp)
       t))

;;; --- the registry -----------------------------------------------------------

(defstruct (exec-job (:copier nil))
  "What an evaluation is and how it stands: the facts an EXEC-PROCESS keeps
and every EXEC-UPDATE of it carries."
  (session-id "" :type string)
  (turn-id nil)
  (tool-call-id "" :type string)
  ;; The evaluating form's source text.
  (command "" :type string)
  (status :running :type (member :running :exited))
  (exit-code nil)
  ;; The exit status of the last (sh ...) this evaluation ran, set by SH as
  ;; each command ends; NIL until one has. The shell's verdict, which
  ;; EXIT-CODE -- the evaluation's own, 0 a value, 1 an error, 130
  ;; interrupted -- does not carry: a form whose command exited 2 still
  ;; returned its output as a value.
  (shell-exit nil)
  (backgrounded nil :type boolean))

(defstruct (exec-process (:include exec-job) (:copier nil))
  (id 0 :type integer)
  ;; Merged stdout+stderr stream in arrival order.
  (output (octet-buffer 4096))
  ;; Absolute stream offset of (aref output 0).
  (base-offset 0 :type integer)
  ;; Absolute stream cursor already returned to the model.
  (model-cursor 0 :type integer)
  (model-sanitizer (make-text-sanitizer))
  ;; Absolute stream cursor already projected to sync subscribers.
  (projected-cursor 0 :type integer)
  (exit-projected nil :type boolean)
  (exit-observed-by-model nil :type boolean)
  ;; One-shot wake consumed by the next parked wait (turn cancellation).
  (wake-pending nil :type boolean)
  ;; The live (sh ...) child of this evaluation, when one is running: the
  ;; liveness scan's subject. NIL between shell commands.
  (child-pid nil)
  ;; Consecutive liveness passes that found a job-control-stopped process
  ;; in the child's tree; the collapse threshold reads this.
  (stalled-ticks 0 :type integer)
  ;; T once an exit's one wake submission is claimed (watcher edge or
  ;; turn-end edge) — the wake fires at most once per evaluation.
  (wake-claimed nil :type boolean)
  ;; The evaluating thread itself: its completion IS the exit, so reap and
  ;; destroy join the same slot.
  (wait-thread nil)
  ;; The value's printed text once exited — never appended to the output
  ;; stream: output is what the form PRINTED, the value rides the tool
  ;; result or a follow-up read.
  (eval-value nil)
  ;; *TOOL-RESULT-METADATA* as the form left it, captured at completion for
  ;; the in-window fast path.
  (eval-metadata nil)
  ;; What the snippet left for the session's tables (its capture), held here
  ;; from the snippet's end until whoever delivers its value to the model
  ;; settles it: the in-window settle thunk, or the wake of a snippet that
  ;; outlived its window. Live-only; dies with the entry.
  (snippet-capture nil)
  ;; The last *EXEC-PREVIEW-BYTES* its (sh ...) child wrote, as they arrived:
  ;; what a band shows while the command runs. Never the model's output --
  ;; SH answers its whole capture as the value, and the model reads that.
  (preview nil)
  ;; T while a subscriber has not been sent PREVIEW's news, or the handoff.
  (preview-dirty nil :type boolean)
  ;; When a preview last left (internal real time): its throttle.
  (preview-at 0 :type integer)
  ;; When it was spawned (internal real time): how long a stopped one ran.
  (started (get-internal-real-time) :type integer)
  ;; Who stopped it, once someone did (EXEC-STOP-PROCESS): (:BY BY :REASON
  ;; WHY :AT WHEN :END OFFSET), END how far its output ran then. The first
  ;; stop is the one a card names.
  (stopped nil))

(nlk:access (process exec-process))

(defun exec-total-len (process) (+ process.base-offset (fill-pointer process.output)))

(defstruct (exec-registry (:copier nil))
  (lock (bt2:make-lock :name "exec-registry") :read-only t)
  ;; The change broadcast: parked waits sleep on this instead of a fixed
  ;; tick, so an exit or fresh output is observed immediately.
  (change (bt2:make-condition-variable :name "exec-change") :read-only t)
  ;; Spawn order, matching the Zig append order: LIST-EVALS reads it
  ;; forward, the newest-running selection walks it from the end.
  (processes '() :type list)
  (next-id 1000 :type integer))

(nlk:access (registry exec-registry))

;;; A true global: gateway worker threads, the watcher, and tool threads all
;;; mutate the one instance in place. Initialized at load time below
;;; (DEFGLOBAL evaluates its initform at compile time, before this file's
;;; struct constructors exist).
(sb-ext:defglobal *exec-registry* nil
  "The image's one background-work registry.")

(unless *exec-registry*
  (setf *exec-registry* (make-exec-registry)))

(defun exec-registry-instance () *exec-registry*)

(defun exec-notify-change (registry)
  (bt2:with-lock-held ((exec-registry-lock registry))
    (bt2:condition-broadcast registry.change)))

(defun exec-find-locked (registry id) (find id registry.processes :key #'exec-process-id))

(defmacro do-registry ((process registry) &body clauses)
  "LOOP CLAUSES over REGISTRY's processes, each as PROCESS, under its lock."
  (alexandria:once-only (registry)
    `(bt2:with-lock-held ((exec-registry-lock ,registry))
       (loop for ,process in (exec-registry-processes ,registry) ,@clauses))))

(defun call-background-running-p (call-id)
  "T when CALL-ID's evaluation is still running in the background: its form
outlived the call's yield window, so the model was handed the running
envelope and the harness will wake the session with the result."
  ;; The one fact a turn's end reads to tell a status from an answer.
  (and (stringp call-id)
       (do-registry (process (exec-registry-instance))
         thereis (and (equal call-id process.tool-call-id)
                      process.backgrounded
                      (eq :running process.status)))))

(defun turn-background-pending (session-id turn-id)
  "How many of SESSION-ID's TURN-ID background evaluations are still unread
— running, or exited and never observed by the model — or NIL when none."
  ;; A turn that ends with this positive has not answered its ask: the result
  ;; it is waiting on arrives through the exit wake, in a later turn.
  (let ((pending
          (do-registry (process (exec-registry-instance))
            count (and (equal session-id process.session-id)
                       (equal turn-id process.turn-id)
                       process.backgrounded
                       (not process.exit-observed-by-model)))))
    (and (plusp pending) pending)))

(defun exec-reap-locked (registry process)
  ;; PROCESS out of REGISTRY once both its observers -- the projection and the
  ;; model -- have seen its exit, answered for the caller to destroy; else NIL.
  (when (and process.exit-projected process.exit-observed-by-model process.wait-thread)
    (setf registry.processes (remove process registry.processes :count 1))
    process))

;;; --- append / stop / reclaim ------------------------------------------------

(defun exec-stop-process (process &key hard message by reason &aux (thread process.wait-thread))
  "Abort a background evaluation: by default the BACKGROUND-EVAL-INTERRUPT
condition injected into its thread; with HARD, thread termination
(SB-THREAD's abort unwinds past every handler, the guarantee a form that
swallows the soft interrupt condition forces). BY names who stopped it --
:OPERATOR from a surface, :TURN when its turn was cancelled for REASON,
:SHUTDOWN, :MODEL through (eval-interrupt) -- for the card that closes it."
  ;; MESSAGE, when given, is the value the soft abort exits with in place of
  ;; the verb's own text. Never under the registry lock (a shutdown holds it
  ;; here): the stop is one slot, set once.
  (when (and by (null process.stopped) (eq :running process.status))
    (setf process.stopped (list :by by :reason reason :at (get-internal-real-time)
                                :end (exec-total-len process))))
  (if hard
      (when (and thread (bt2:thread-alive-p thread))
        (ignore-errors (bt2:destroy-thread thread)))
      ;; Only a still-running eval takes the abort (INTERRUPT-RUNNING).
      (interrupt-running process (lambda ()
                                   (apply #'error 'background-eval-interrupt
                                          (and message (list :message message)))))))

(defun interrupt-running (process fn &aux (thread process.wait-thread))
  ;; FN on PROCESS's evaluating thread while that lives => the thread. The
  ;; interruption may land after the eval completed but before its thread
  ;; exits; signalling then would unwind outside the lifecycle's handler, so
  ;; FN runs there only while the evaluation still runs.
  (when (and thread (bt2:thread-alive-p thread))
    (ignore-errors
     (bt2:interrupt-thread thread (lambda () (when (eq :running process.status) (funcall fn)))))
    thread))

;;; --- the runaway probe -------------------------------------------------------
;;; Without this, a recursion that allocates on every call would overflow the
;;; stack inside the allocator: the guard page would be crossed during
;;; pseudo-atomic operations and SBCL would kill the image instead of signalling
;;; a clean stack exhaustion. A pure recursion overflows cleanly, so the guard
;;; page is not the problem: the problem is reaching it while allocating, and
;;; the fix is never to reach it. The watcher interrupts each running
;;; evaluation with a probe that measures its own stack and the heap, and a
;;; verdict unwinds the evaluation as an ordinary error the eval snippet
;;; answers as text.

(define-error eval-runaway (error) (what)
  (:report "~a" what)
  ;; An ERROR, so the eval snippet's own arm reports it and a form's HANDLER-CASE
  ;; may take it.
  (:documentation
   "An evaluation that will not bottom out: past half its control stack, or
past *EVAL-HEAP-SOFT-FRACTION* of the heap."))

;;;
;;; 0.35, not the 0.7 this began with, because a copying collector needs room to
;;; COPY the live set: the worst case is live data plus a second copy of it, so
;;; the wall is near half the space, not all of it. At 45.5% live the next
;;; collection would die. The collections in that run arrived about five
;;; percentage points apart, so 0.35 leaves two of them in hand; 0.45 would
;;; have been a coin toss. On an 8 GB space this still allows 2.8 GB of live
;;; data inside one evaluation, against frames of four kilobytes.
(defparameter *eval-heap-soft-fraction* 0.35
  "The share of the dynamic space past which the guard unwinds the running
evaluation: heap exhaustion inside a collection is fatal, and the evaluation
holding the data is the one that can let go.")

(defun runaway-frame-name ()
  "The function most present among this thread's top frames, or NIL when
none repeats: the name of the recursion, for the verdict."
  ;; Walked frame by frame with SB-DI, each step tolerating a frame the
  ;; debugger cannot decode: inside an interruption that lands in the
  ;; allocator, LIST-BACKTRACE signals on such a frame and named nothing on
  ;; 195 of 232 probes, where the walk named the recursion on all of them. A
  ;; hundred and twenty frames, because the interruption's own machinery sits
  ;; on top.
  (ignore-errors
   (let ((counts '()))
     (loop for frame = (sb-di:top-frame) then (ignore-errors (sb-di:frame-down frame))
           repeat 120 while frame
           do (let ((name (ignore-errors
                           (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))))
                (when (and name
                           (not (and (symbolp name) (symbol-package name)
                                     (eql 0 (search "SB-" (package-name (symbol-package name)))))))
                  (if-let (entry (assoc name counts :test #'equal))
                    (incf (cdr entry)) (push (cons name 1) counts)))))
     (let ((best (first (sort counts #'> :key #'cdr))))
       (and best (> (cdr best) 3)
            (nlk:clip (princ-to-string (car best)) 80 :ellipsis ""))))))

(defun scan-eval-runaways (registry &aux (backgrounded '()))
  "The watcher's pass over the running evaluations: interrupt each live
evaluating thread with the probe — the registry's backgrounded processes,
and the threads evaluating a snippet in place, which no registry holds."
  ;; Never signals here — a thread that exited between the listing and the
  ;; interrupt is not an event.
  (dolist (process (bt2:with-lock-held (registry.lock) (copy-list registry.processes)))
    ;; only a still-running evaluation takes the verdict (INTERRUPT-RUNNING)
    (when (eq :running process.status)
      (when-let (thread (interrupt-running process 'eval-runaway-thread-probe))
        (push thread backgrounded))))
  ;; A synchronous snippet has no process; its thread carries the evaluation
  ;; itself. Skip one already probed above as a background eval's thread.
  (dolist (thread (guarded-evaluating-threads))
    (unless (member thread backgrounded)
      (ignore-errors
       (bt2:interrupt-thread thread #'eval-runaway-thread-probe)))))

(defun control-stack-usage (&aux (sp (sb-sys:sap-int (sb-vm::current-sp)))
                                 (thread sb-thread:*current-thread*)
                                 (start (sb-thread::thread-control-stack-start thread))
                                 (end (sb-thread::thread-control-stack-end thread)))
  "This thread's control stack as (VALUES USED SPAN), in bytes."
  ;; The bounds come from the thread's own struct: the *CONTROL-STACK-*
  ;; specials read inside an interruption are the main thread's, and a probe
  ;; measuring an evaluating thread against them would never fire. USED is
  ;; counted in the direction the stack grows, which differs by architecture:
  ;; on x86-64 the control stack is the C stack and grows down from END; on
  ;; arm64 it is SBCL's own region, CURRENT-SP is the csp register, and it
  ;; grows up from START. The feature that says which is stripped from
  ;; *FEATURES* at runtime and kept in +INTERNAL-FEATURES+, so a read-time
  ;; conditional here would pick x86-64 on every platform. END - SP on an
  ;; upward stack is the FREE space; without the direction check, an
  ;; evaluation that outlives a watcher pass would be unwound as a recursion
  ;; that names no frame.
  (values (if (member :stack-grows-downward-not-upward sb-impl:+internal-features+)
              (- end sp)
              (- sp start))
          (- end start)))

(defun eval-runaway-thread-probe ()
  "The interruption body for a thread evaluating a snippet in place: signal the
verdict when there is one."
  ;; No process to consult — the thread is on the list only while its
  ;; evaluation runs.
  (when-let (verdict (ignore-errors
                      (bind (((used span) (control-stack-usage))
                             (limit (min (* 16 1024 1024) (floor span 2)))
                             (heap (sb-kernel:dynamic-usage)) (cap (sb-ext:dynamic-space-size)))
                        (cond ((> used limit)
                               (format nil "control stack runaway: ~:d KB of frames on a ~:d KB stack~@[, deepest in ~a~] — a recursion that does not bottom out, unwound before the guard page (an overflow inside an allocation there kills the whole image). Iterate, or bound the depth."
                                       (floor used 1024) (floor span 1024) (runaway-frame-name)))
                              ((> heap (* *eval-heap-soft-fraction* cap))
                               (format nil "heap runaway: ~:d MB of ~:d MB in use during this evaluation — unwound before heap exhaustion, which is fatal inside a collection. Hold less at once."
                                       (floor heap 1048576) (floor cap 1048576)))))))
    (error 'eval-runaway :what verdict)))

;;; --- the guard covers every evaluation, and needs no gateway ----------------
;;; The probe above only ever reached a BACKGROUNDED evaluation: SCAN-EVAL-
;;; RUNAWAYS walked the registry, and the registry holds what SPAWN-BACKGROUND-
;;; EVAL put there, so a synchronous snippet — every snippet the model writes without
;;; asking for a background — was in no list any watcher walked. It was also
;;; driven from the gateway's background watcher alone, so an image that
;;; evaluates without serving (a bench driver, a script, a test) ran unguarded
;;; even for the evaluations the registry did hold. Both halves are fixed here: the
;;; scan walks evaluating threads as well as registered processes, and the
;;; watcher that drives it starts with the first evaluation rather than with a
;;; network listener, so a headless image is guarded exactly like a served one.

;;; A thread may appear more than once (a form that evaluates a snippet
;;; re-enters); the scan probes each thread once.
(sb-ext:defglobal *evaluating-threads* '()
  "Threads currently inside a snippet evaluation, each pushed for the extent of
its evaluation.")

(sb-ext:defglobal *eval-guard-lock* (bt2:make-lock :name "eval-guard")
  "Guards *EVALUATING-THREADS* and the watcher's identity.")

(sb-ext:defglobal *eval-guard-worker* nil)

;;; The incident's recursion grows its stack at the allocator's pace, measured
;;; at ~3 MB/s on an 8 MB stack (the justfile's size): a pass a second found it
;;; at 6 MB, past the 4 MB verdict and 2 MB short of the guard page, so the
;;; passes come five times as often. The heap share is a soft fraction with
;;; the rest of the space still to spend.
(defparameter *eval-guard-interval-seconds* 0.2
  "Seconds between runaway passes.")

(defun eval-runaway-gc-hook ()
  "Run after every collection, on the thread that triggered it."
  ;; A poll cannot win this race: the fatal error is "Heap exhausted during
  ;; garbage collection", so the image dies inside the collection, and between
  ;; one sample and the next a recursion that conses can cross the whole
  ;; remaining space. After a collection the number is exact and the moment is
  ;; the right one — what is still live is what the evaluation is holding, and
  ;; if that is already past the soft fraction no later collection will save
  ;; it. Only a thread evaluating a snippet is unwound; any other thread that
  ;; happens to collect just returns. IGNORE-ERRORS guards the LOOKUP only.
  ;; Wrapping the signal in it too would swallow the very condition the hook
  ;; exists to raise, and the image would still die.
  (when-let (verdict (ignore-errors
                      (let ((heap (sb-kernel:dynamic-usage))
                            (cap (sb-ext:dynamic-space-size)))
                        (when (and (> heap (* *eval-heap-soft-fraction* cap))
                                   (bt2:with-lock-held (*eval-guard-lock*)
                                     (member (bt2:current-thread) *evaluating-threads*)))
                          (format nil "heap runaway: ~:d MB of ~:d MB still live after a collection during this evaluation — unwound before heap exhaustion, which is fatal inside a collection and takes the image. Hold less at once."
                                  (floor heap 1048576) (floor cap 1048576))))))
    (error 'eval-runaway :what verdict)))

(defun ensure-eval-guard ()
  "Start the runaway watcher if it is not already running, and answer its
worker."
  ;; Idempotent and cheap enough to call at the head of every evaluation: the
  ;; common path is one lock hold and a live-thread test.
  (bt2:with-lock-held (*eval-guard-lock*)
    ;; The hook is the guard that matters for the heap; the watcher below
    ;; still catches a control-stack runaway and a leak that never collects.
    (pushnew 'eval-runaway-gc-hook sb-ext:*after-gc-hooks*)
    (if (and *eval-guard-worker*
             (ignore-errors (bt2:thread-alive-p (worker-thread *eval-guard-worker*))))
        *eval-guard-worker*
        (setf *eval-guard-worker*
              (worker-start "eval-guard"
                            (lambda ()
                              (ignore-errors (scan-eval-runaways (exec-registry-instance))))
                            :wake (lambda () *eval-guard-interval-seconds*))))))

(defun call-with-evaluation-guarded (thunk)
  "Run THUNK with this thread on the guarded list and the watcher running."
  ;; The list entry is removed on every exit, normal or not.
  (ensure-eval-guard)
  (let ((self (bt2:current-thread)))
    (bt2:with-lock-held (*eval-guard-lock*)
      (push self *evaluating-threads*))
    (unwind-protect (funcall thunk)
      (bt2:with-lock-held (*eval-guard-lock*)
        (setf *evaluating-threads*
              (remove self *evaluating-threads* :count 1))))))

(defun guarded-evaluating-threads ()
  "The distinct live threads currently evaluating a snippet."
  (remove-duplicates
   (remove-if-not (lambda (thread) (ignore-errors (bt2:thread-alive-p thread)))
                  (bt2:with-lock-held (*eval-guard-lock*)
                    (copy-list *evaluating-threads*)))))

(defun exec-append-output (registry process bytes)
  (bt2:with-lock-held ((exec-registry-lock registry))
    (let ((output process.output))
      (loop for byte across bytes do (vector-push-extend byte output))
      (when (> (fill-pointer output) +exec-max-output-bytes+)
        (let* ((desired (- (fill-pointer output) (/ +exec-max-output-bytes+ 2)))
               ;; never mid-escape or mid-character: forward to an ESC, else past continuations
               (drop (or (position #x1b output :start desired)
                         (utf8-cut output desired))))
          (replace output output :start2 drop :end2 (fill-pointer output))
          (decf (fill-pointer output) drop)
          (incf process.base-offset drop)))))
  (exec-notify-change registry))

(defun exec-destroy-process (process)
  "Reclamation once the process left the registry list: never called with
the registry lock held (it joins the wait thread)."
  (when (eq :running process.status)
    (exec-stop-process process :hard t))
  (when-let (thread (shiftf process.wait-thread nil))
    (ignore-errors (bt2:join-thread thread))))

;;; --- parked waits -----------------------------------------------------------

(defun exec-wait (registry seconds tick done)
  ;; DONE, called under REGISTRY's lock, until it answers true (=> T) or SECONDS
  ;; pass (=> NIL), waiting on the change broadcast in between: the timeout TICK
  ;; only bounds how often a wait re-checks its deadline.
  (loop with deadline = (deadline-after seconds)
        do (bt2:with-lock-held ((exec-registry-lock registry))
             (when (funcall done) (return t))
             (bt2:condition-wait registry.change registry.lock :timeout tick))
        until (>= (get-internal-real-time) deadline)))

(defun exec-park-until (registry id yield-ms &key cursor)
  "Block until YIELD-MS elapses, ID exits or vanishes, the wait is woken via
WAKE-EXEC-SESSION, or — when CURSOR is given — output arrives past it."
  ;; The verdict and the park share one lock hold: that is the missed-wake
  ;; guard, since wait-observable state only ever changes under this lock.
  (exec-wait registry (/ yield-ms 1000) 0.025
             (lambda (&aux (process (exec-find-locked registry id)))
               (or (null process)
                   (eq :exited process.status)
                   (and cursor
                        (> (exec-total-len process) cursor))
                   ;; a wake is one-shot: taken as it is read
                   (shiftf process.wake-pending nil)))))

(defun wake-exec-session (registry session-id)
  "Wake every parked wait on SESSION-ID's processes (turn cancellation must
not leave a tool call parked out a multi-minute yield window)."
  (do-registry (process registry)
    when (equal session-id process.session-id) do (setf process.wake-pending t))
  (exec-notify-change registry))

;;; --- model snapshots --------------------------------------------------------

(defun mark-exec-backgrounded (registry id)
  "The handoff flag: this evaluation outlived its tool call."
  ;; Gates the durable exit fact and row stamping.
  (bt2:with-lock-held ((exec-registry-lock registry))
    (when-let (process (exec-find-locked registry id))
      ;; The handoff is news to a band even when the snippet is silent.
      (setf process.backgrounded t
            process.preview-dirty t)))
  id)

(defun exec-snapshot-since (registry id cursor &aux reap)
  "Copy output at/after CURSOR through the process's streaming model
sanitizer (state persists across contiguous snapshots; a jumped cursor
restarts it) and advance the model cursor."
  ;; Reaps the process when exited,
  ;; fully projected, and observed by the model. Returns a plist
  ;; (:output :status :exit-code), or NIL.
  (prog1 (bt2:with-lock-held ((exec-registry-lock registry))
           (when-let (process (exec-find-locked registry id))
             (let* ((output process.output)
                    (base process.base-offset)
                    (start-offset (max cursor base))
                    (bytes (subseq output (min (- start-offset base) (fill-pointer output)))))
               (unless (= start-offset process.model-cursor)
                 (setf process.model-sanitizer
                       (make-text-sanitizer)))
               (multiple-value-bind (clean dropped)
                   (sanitize-octets process.model-sanitizer bytes)
                 (setf process.model-cursor (exec-total-len process))
                 (when (eq :exited process.status)
                   (setf process.exit-observed-by-model t
                         reap (exec-reap-locked registry process)))
                 (list :output (note-removed-controls
                                (octets-to-text clean) dropped)
                       :status process.status
                       :exit-code process.exit-code)))))
    (when reap (exec-destroy-process reap))))

;;; --- projection collection --------------------------------------------------

(defstruct (exec-update (:include exec-job) (:copier nil))
  "One projection update: an ordered raw chunk at CHUNK-OFFSET in the
absolute stream, gapless across consecutive updates for a live subscriber."
  (shell-session-id 0 :type integer)
  ;; T on the single update that completes projecting an exited backgrounded
  ;; process: the watcher's trigger for the durable exit fact.
  (final nil :type boolean)
  (chunk (make-array 0 :element-type '(unsigned-byte 8)))
  (chunk-offset 0 :type integer)
  ;; A running snippet's live preview (EXEC-PROCESS PREVIEW), whole, or NIL.
  (preview nil)
  ;; An exited backgrounded process's last lines (EXEC-EXIT-TAIL), and T when
  ;; it wrote more before them than they keep; NIL on every other update.
  (tail nil)
  (tail-cut nil :type boolean)
  ;; An exited backgrounded process's error report (EXEC-ERROR-REPORT), when
  ;; its form signalled; NIL on every other update.
  (error nil)
  ;; An exited backgrounded process's stop (STOP-DOCUMENT), when someone
  ;; stopped it; NIL on every other update.
  (stopped nil))

(defun exec-error-report (value)
  "The report of the condition a failed evaluation's VALUE ends on: its text
from the line that opens with ERROR, that word and its colon left off, cut at
1,000 characters. NIL when no line opens with it."
  ;; What leads it -- a closed-parens note, a compiler warning -- is not the
  ;; failure, and a snippet failing at its third form says so in the report.
  (when-let (start (ppcre:scan "(?m)^ERROR" value))
    (nlk:clip (string-left-trim '(#\: #\Space) (string-right-trim '(#\Space #\Newline) (subseq value (+ start 5))))
              1000)))

(defun exec-update-from-process-locked (process chunk chunk-offset
                                        &aux (closed (and (eq :exited process.status) process.backgrounded)))
  (multiple-value-bind (tail tail-cut)
      (and closed (exec-exit-tail process))
    (apply #'make-exec-update
           :shell-session-id process.id
           :chunk chunk
           :chunk-offset chunk-offset
           :tail tail
           :tail-cut tail-cut
           :error (and closed (eql 1 process.exit-code)
                       (exec-error-report (or process.eval-value "")))
           :stopped (and closed (eql 130 process.exit-code)
                         (exec-stop-document-locked process))
           ;; the job's facts, as they stand under the lock
           (loop for slot in (sb-mop:class-slots (find-class 'exec-job))
                 for name = (sb-mop:slot-definition-name slot)
                 append (list (alexandria:make-keyword name) (slot-value process name))))))

(defparameter *exec-preview-bytes* 4096
  "How much of a running command's newest output its live preview keeps.")

(defparameter *exec-preview-interval* 0.4
  "The least seconds between two previews of one running snippet.")

(defun note-exec-preview (process text &aux (registry (exec-registry-instance)))
  "Add TEXT, what PROCESS's (sh ...) child just wrote, to its live preview."
  (let ((octets (sb-ext:string-to-octets text :external-format :utf-8)))
    (bt2:with-lock-held ((exec-registry-lock registry))
      (let ((joined (concatenate '(vector (unsigned-byte 8))
                                 (or process.preview #()) octets)))
        (setf process.preview (subseq joined (max 0 (- (length joined) *exec-preview-bytes*)))
              process.preview-dirty t)))))

(defun exec-exit-tail (process &optional (end (fill-pointer process.output)))
  "=> (values TEXT CUT-P): the last lines PROCESS wrote, as its exit keeps
them for a closed card -- the newest ~2 KiB (APPENDED-PREVIEW-TAIL) of what
its (sh ...) child wrote when one wrote anything, else of what the form
printed before END, each line as a terminal last drew it -- and whether it
wrote more before them than they hold."
  ;; The caller holds the registry lock. The same material a band shows while
  ;; the command runs; the whole of it lives no longer than the process. A
  ;; stopped form's END is where its output stood at the stop: the compiler's
  ;; report of the abort, printed on the way out, is not what it wrote.
  (let* ((output process.output)
         (end (max 0 (min end (fill-pointer output))))
         (raw (or process.preview
                  (subseq output (max 0 (- end *exec-preview-bytes*)) end)))
         (clean (sanitize-octets (make-text-sanitizer) (last-redraws raw)))
         (tail (appended-preview-tail #() clean)))
    (values (octets-to-text tail)
            (or (< (length tail) (length clean))
                (if process.preview
                    (>= (length raw) *exec-preview-bytes*)
                    (> (+ process.base-offset end) (length raw)))))))

(defun exec-collect-updates (registry &aux (updates '())
                                           (reaped '())
                                           (now (get-internal-real-time)))
  "Projection updates for every process with unprojected output or an
unprojected exit, and a running update for each snippet whose preview moved or
that was just handed off -- at most one per *EXEC-PREVIEW-INTERVAL*."
  ;; Chunks start at the projected cursor
  ;; (clamped to the ring base after a trim) and are capped; the last update of a
  ;; fully-projected exited BACKGROUNDED process is marked FINAL exactly once.
  (bt2:with-lock-held ((exec-registry-lock registry))
    (dolist (process (copy-list registry.processes))
      ;; A command that writes nothing the model reads -- (sh ...) answers
      ;; its capture as a value -- still shows in a band while it runs.
      (when (and (eq :running process.status) process.preview-dirty
                 (>= (- now process.preview-at)
                     (* *exec-preview-interval* internal-time-units-per-second)))
        (let ((update (exec-update-from-process-locked
                       process (make-array 0 :element-type '(unsigned-byte 8))
                       process.projected-cursor)))
          (setf (exec-update-preview update) (copy-seq (or process.preview #()))
                process.preview-dirty nil
                process.preview-at now)
          (push update updates)))
      ;; new output, or an exit not yet projected
      (when (or (< process.projected-cursor (exec-total-len process))
                (and (eq :exited process.status) (not process.exit-projected)))
        (let ((output process.output))
          ;; at most 8 chunks of 64 KiB a pass: the cursor keeps up, each frame stays bounded
          (loop repeat 8
                do (let* ((chunk-offset (max process.projected-cursor process.base-offset))
                          (start-index (- chunk-offset process.base-offset))
                          (chunk (subseq output start-index
                                         (min (fill-pointer output) (+ start-index (* 64 1024))))))
                     (push (exec-update-from-process-locked process chunk chunk-offset) updates)
                     (setf process.projected-cursor
                           (+ chunk-offset (length chunk))))
                until (>= process.projected-cursor
                          (exec-total-len process)))
          (when (and (eq :exited process.status)
                     (>= process.projected-cursor
                         (exec-total-len process)))
            (when (and (not process.exit-projected) process.backgrounded)
              ;; Newest push = this process's closing update: every pass pushes at least one.
              (setf (exec-update-final (first updates)) t))
            (setf process.exit-projected t)
            (when-let (done (exec-reap-locked registry process))
              (push done reaped)))))))
  (dolist (process reaped) (exec-destroy-process process))
  (nreverse updates))

(defun exec-collect-session-snapshots (registry session-id)
  "One update per still-RUNNING process of SESSION-ID carrying the full
retained buffer as one chunk at the ring base."
  ;; Observation only: no cursor movement, nothing reaped — the subscribe-time
  ;; re-seed, never replay-seeded.
  (do-registry (process registry)
    when (and (eq :running process.status) (equal session-id process.session-id))
    collect (exec-update-from-process-locked
             process
             (copy-seq process.output)
             process.base-offset)))

;;; --- listing and shutdown ---------------------------------------------------

(defun terminate-all-execs (registry)
  "Abort every still-running evaluation."
  ;; Gateway shutdown calls this so no background work outlives the image that
  ;; owns it.
  (do-registry (process registry)
    when (eq :running process.status) do (exec-stop-process process :hard t :by :shutdown)))

(defun exec-await-all-exits (registry &key (timeout-seconds 5))
  "Block until no registered process still reports :RUNNING, or
TIMEOUT-SECONDS."
  ;; Shutdown's kill→exit-fact ordering needs the exits observed BEFORE the
  ;; final watcher collection; a terminated thread unwinds promptly, so the
  ;; bound only guards a form that somehow outlives its abort. T when
  ;; everything exited, NIL on timeout.
  (exec-wait registry timeout-seconds 0.05
             (lambda ()
               (loop for process in registry.processes never (eq :running process.status)))))

(defun destroy-all-execs (registry)
  "Empty the registry and reclaim every process — the Registry.deinit half
of the Zig shutdown contract, which the image-lifetime DEFGLOBAL registry
otherwise never performs across gateway restarts: remove every entry under
the lock, then kill/join/close outside it (EXEC-DESTROY-PROCESS joins the
wait thread and must not hold the lock)."
  ;; Callers own the durable exit-fact ordering: collect the final updates
  ;; BEFORE this forgets the exits.
  (mapc #'exec-destroy-process
        (bt2:with-lock-held ((exec-registry-lock registry))
          (prog1 registry.processes
            (setf registry.processes '()))))
  (exec-notify-change registry)
  t)

;;; --- tool-call context and the running handoff ------------------------------

(defun clamp-exec-yield (requested default)
  "The window a park honours: REQUESTED when the call named one, else
DEFAULT, never past ten minutes, past what the running turn's budget has
left (meter.lisp BUDGET-REMAINING-MS), nor under 50 ms."
  ;; Every park in this file measures against it, so no wait outlasts the
  ;; turn's budget.
  (max 50 (min 600000
               (or (budget-remaining-ms) 600000)
               (if (integerp requested) requested default))))

(defun running-shell-metadata (id command)
  (make-json-object
   "background_shell" (make-json-object
                       "shell_session_id" id
                       "status" "running"
                       "command" command)))

;;; --- background evaluations: the eval lifecycle ------------------------
;;; The run tool's yield-and-background AX folded over the eval snippet
;;; (turn.lisp EVAL-FORM-PRINTED): a form that outlives its yield window
;;; keeps evaluating on an image thread, registered here — same registry,
;;; same parked waits, same band frames, same durable exit fact. There are
;;; no follow-up TOOLS (the registry carries eval alone, by directive):
;;; the follow-ups are ordinary functions — EVAL-AWAIT, EVAL-STATUS,
;;; LIST-EVALS, EVAL-INTERRUPT — the organism drives through eval
;;; itself.
;;;
;;; The one honest divergence from the ported shell dialect: shells were
;;; isolated OS processes, background evals are threads in the shared image.
;;; A background eval can race later foreground forms on image state — not a
;;; new hazard (the organism can already spawn threads), but named here
;;; rather than forbidden.

(define-error background-eval-interrupt (serious-condition)
    ((message :initform "ERROR: evaluation aborted by (eval-interrupt)"))
  (:report "background evaluation interrupted by (eval-interrupt)")
  ;; A SERIOUS-CONDITION that is not an ERROR — the SB-EXT:TIMEOUT lesson: the
  ;; form's own (handler-case ... (error ...)) arms and the eval snippet's text
  ;; arms must not swallow the abort on its way out. A form that traps this
  ;; anyway is answered by EVAL-INTERRUPT :FORCE, which terminates the thread.
  ;; MESSAGE is the value the aborted evaluation exits with — the verb's own
  ;; text by default, the cancel's when the turn that spawned it was cut.
  (:documentation
   "EVAL-INTERRUPT's soft abort, injected into the evaluating thread."))

;;; The two streams that stand in for a terminal: this eval's output, and the
;;; TUI's *ERROR-OUTPUT* (ui.lisp TUI-ERROR-STREAM). Each is a line buffer
;;; with its own sink, so the buffer is written once.
(defclass line-output-stream (sb-gray:fundamental-character-output-stream)
  ((pending :initform (make-array 128 :element-type 'character
                                      :adjustable t :fill-pointer 0)
            :reader line-output-stream-pending)
   (at-line-start :initform t :accessor line-output-stream-at-line-start))
  (:documentation
   "A character output stream that gathers what is written and hands it to
its LINE-OUTPUT method at every newline and at FORCE-OUTPUT."))

(defgeneric line-output (stream text)
  (:documentation "Take TEXT, what STREAM gathered since it last handed any
on: a line with its newline, or the partial line FORCE-OUTPUT flushed."))

(defun flush-line-output (stream &aux (pending (line-output-stream-pending stream)))
  "Hand what STREAM has gathered to LINE-OUTPUT, when it gathered anything."
  (when (plusp (fill-pointer pending))
    (let ((text (coerce pending 'simple-string)))
      (setf (fill-pointer pending) 0)
      (line-output stream text)))
  nil)

(defmethod sb-gray:stream-write-char ((stream line-output-stream) char)
  (vector-push-extend char (line-output-stream-pending stream))
  (setf (line-output-stream-at-line-start stream) (char= char #\Newline))
  (when (char= char #\Newline)
    (flush-line-output stream))
  char)

(defmethod sb-gray:stream-line-column ((stream line-output-stream))
  ;; NIL means "unknown", under which FRESH-LINE conservatively emits a
  ;; newline; 0 at a line start keeps ~& quiet there.
  (if (line-output-stream-at-line-start stream) 0 nil))

(defmethod sb-gray:stream-force-output ((stream line-output-stream))
  (flush-line-output stream))

(defmethod sb-gray:stream-finish-output ((stream line-output-stream))
  (sb-gray:stream-force-output stream))

(defclass eval-output-stream (line-output-stream)
  ((registry :initarg :registry :reader eval-output-stream-registry)
   (process :initarg :process :reader eval-output-stream-process))
  (:documentation
   "The background eval's *STANDARD-OUTPUT*: printed characters buffer
locally and flush into the process's registry output buffer on newline,
size, or FORCE-OUTPUT — from there one set of cursors serves the parked
awaits, the band frames, and the model snapshots."))

(defmethod line-output ((stream eval-output-stream) text)
  (exec-append-output (eval-output-stream-registry stream)
                      (eval-output-stream-process stream)
                      (sb-ext:string-to-octets text :external-format :utf-8)))

(defmethod sb-gray:stream-write-char :after ((stream eval-output-stream) char)
  (declare (ignore char))
  ;; 512 pending characters flush between newlines, so one long printed line still streams
  (when (>= (fill-pointer (line-output-stream-pending stream)) 512)
    (flush-line-output stream)))

(defun finish-background-eval (registry process exit-code value metadata)
  "Close PROCESS's lifecycle: idempotent (a hard kill's unwind fallback may
race the normal completion path) and wake-broadcasting, so parked awaits
observe the exit immediately."
  ;; The stored VALUE is the model's: sanitized and disclosed here, once, so
  ;; every reader — the in-window result, the eval-status and eval-await
  ;; plists — sees the same clean text.
  (let ((value (and value (disclose-sanitized-text value))))
    (bt2:with-lock-held ((exec-registry-lock registry))
      (when (eq :running process.status)
        (setf process.status :exited
              process.exit-code exit-code
              process.eval-value value
              process.eval-metadata metadata))))
  (exec-notify-change registry))

;;; (SH ...) registers its live child here so
;;; the liveness scan can see the OS process a parked evaluation is actually
;;; waiting on. NIL on every other thread.
(defvar *current-exec-process* nil
  "The registry entry of the evaluation running on this thread, bound for
the eval thread's whole life.")

(defun snippet-backgrounded-p ()
  "Whether the evaluating snippet outlived its eval call: the call handed it
to the background, and its exit is read once and never watched."
  (and *current-exec-process* (exec-process-backgrounded *current-exec-process*)))

(defvar *sh-chunk* nil
  "NIL, or where SH sends each chunk of its child's output as it arrives: the
running evaluation's live preview (RUN-BACKGROUND-EVAL).")

(defun run-background-eval (registry process thunk)
  "The evaluating thread's whole life."
  ;; THUNK receives the live output stream and returns (values VALUE-TEXT
  ;; ERROR-P METADATA); exit codes follow the shell convention — 0 value, 1
  ;; error, 130 interrupted/terminated.
  (let ((stream (make-instance 'eval-output-stream
                               :registry registry :process process))
        (*current-exec-process* process)
        (*sh-chunk* (lambda (text) (note-exec-preview process text))))
    (flet ((finish (exit-code value &optional metadata)
             ;; what the form printed lands before its exit; a no-op once flushed
             (ignore-errors (flush-line-output stream))
             (finish-background-eval registry process exit-code value metadata)))
      (unwind-protect
          (handler-case
              (multiple-value-bind (value-text error-p metadata)
                  (funcall thunk stream)
                (flush-line-output stream)
                (finish (if error-p 1 0) value-text metadata))
            (background-eval-interrupt (interrupt)
              (finish 130 (background-eval-interrupt-message interrupt)))
            ;; The last-resort net. EVAL-FORM-PRINTED already answers form
            ;; errors as text; what lands here is the seam's own failure —
            ;; a condition whose report re-signals when printed, or a cancel
            ;; re-signaled with no worker unwind above it. Nothing may escape
            ;; this thread: under --disable-debugger an unhandled condition
            ;; in any thread quits the whole image (gateway.lisp invariant).
            (serious-condition (c)
              (finish 1 (format nil "ERROR: ~a" (nlk:safe-condition-report c)))))
        ;; A terminated thread (EVAL-INTERRUPT :FORCE) unwinds through here
        ;; without reaching either arm above; after either, the exit is already
        ;; set and FINISH-BACKGROUND-EVAL leaves it as it is.
        (finish 130 "ERROR: evaluation terminated")))))

(defun spawn-background-eval (registry form thunk
                              &key (session-id "local") turn-id
                              (tool-call-id "") command)
  "Register FORM in the registry and start its evaluating thread."
  ;; COMMAND,
  ;; when given, is the display text registered in its place — a form carrying
  ;; ($ "name") references renders its resolved self — while the THUNK still
  ;; evaluates the form itself. Returns (values ID PROCESS) — the struct
  ;; reference stays readable after a reap, so the spawning tool call can fetch
  ;; the value without racing the watcher.
  (let ((process (make-exec-process
                  :session-id (copy-seq session-id)
                  :turn-id (and (stringp turn-id) (copy-seq turn-id))
                  :tool-call-id (copy-seq (or tool-call-id ""))
                  :command (copy-seq (or command form)))))
    (bt2:with-lock-held ((exec-registry-lock registry))
      (setf process.id (shiftf registry.next-id (1+ registry.next-id)))
      (setf registry.processes (append registry.processes (list process))))
    (setf process.wait-thread
          (spawn (format nil "eval-~d" process.id) (run-background-eval registry process thunk)))
    (values process.id process)))

;;; --- the follow-up surface --------------------------------------------------

(defun find-background-eval (registry id)
  (bt2:with-lock-held ((exec-registry-lock registry))
    (exec-find-locked registry id)))

(defun eval-snapshot-plist (registry id)
  (let* ((process (find-background-eval registry id))
         (snapshot (and process
                        (exec-snapshot-since registry id process.model-cursor))))
    (if (null snapshot)
        ;; self-correcting: name the bad id and the ids that do exist
        (list :status :unknown :id id
              :live-evals (do-registry (process registry) collect process.id))
        (append (list :id id
                      :status (getf snapshot :status)
                      :output (getf snapshot :output))
                (when (eq :exited (getf snapshot :status))
                  (list* :value process.eval-value :exit-code (getf snapshot :exit-code)
                         (and process.shell-exit (list :shell-exit process.shell-exit))))
                ;; Polled from the turn that backgrounded it: the rule where it
                ;; is broken. A wake's opener runs in a later turn.
                (when (and (eq :running (getf snapshot :status))
                           *current-durable-turn*
                           (equal process.turn-id (nlk:durable-turn-turn-id *current-durable-turn*)))
                  (list :note "still running: go on with anything else or end your turn - the harness reads its exit to you when it lands; park only when a short wait beats ending the turn"))))))

(defun list-evals ()
  "Background evaluations in spawn order: one plist (:id :status :form
:exit-code :shell-exit) per entry, the form flattened to a preview."
  (receipt-call "list-evals")
  (do-registry (process (exec-registry-instance))
    collect (list :id process.id
                  :status process.status
                  :form (command-preview
                         process.command)
                  :exit-code process.exit-code
                  :shell-exit process.shell-exit)))

(defvar *read-is-the-snippet* nil
  "True while an eval snippet that is one harness read and nothing else
evaluates -- (eval-status ID) as an exit wake opens on it: its card is the
read (NOTE-BACKGROUND-READ).")

(defun note-background-read (snapshot process)
  "SNAPSHOT, what an (eval-status) or (eval-await) read of PROCESS, a
background evaluation, answered, as its call's card shows it when the read
is the whole snippet: under \"background_read\", the job's state, how it ended
and what it wrote, in the operator's lines -- the plist is the model's -- or,
for a job someone stopped, who stopped it, its lines being in its box.
=> SNAPSHOT."
  ;; PROCESS is found before the read: the read that observes an exit reaps it.
  (destructuring-bind (&key status output value exit-code shell-exit &allow-other-keys) snapshot
    (when (and *read-is-the-snippet* (member status '(:running :exited)))
      (let ((stop (and (eql 130 exit-code) process (exec-stop-document process))))
        (display-fact "background_read"
                      (json-object "status" (string-downcase status)
                                   :opt "end" (and (eq :exited status) (exec-end exit-code shell-exit))
                                   :opt "shell_exit" shell-exit
                                   :opt "stopped" stop
                                   "text" (if stop
                                              ""
                                              (string-trim '(#\Newline)
                                                           (tool-result-display-text
                                                            "eval" (format nil "~@[~a~]~@[~&~a~]"
                                                                           (and (plusp (length output)) output)
                                                                           value)))))))))
  snapshot)

(defun eval-status (id)
  "One non-blocking look at background eval ID: (:status :output ...) with
:OUTPUT everything the form printed since the last look (the cursor
advances), plus :VALUE and :EXIT-CODE once exited -- and :SHELL-EXIT, the
status of the last (sh ...) it ran, when it ran one."
  (receipt-call "eval-status" id)
  (let ((process (find-background-eval (exec-registry-instance) id)))
    (note-background-read (eval-snapshot-plist (exec-registry-instance) id) process)))

;;; A deliberate park (EVAL-AWAIT) adopts it as the default bound, so an await
;;; honors the same yield window the tool schema promises for every other form
;;; instead of silently parking to the ten-minute default — 10000 when the
;;; call left yield_time_ms unset, as the schema says. NIL outside a tool
;;; call — the operators then keep their own defaults.
(defvar *exec-tool-yield-ms* nil
  "The executing eval call's effective yield_time_ms, bound around inline
operator dispatch.")

(defun eval-await (id &key (yield-ms (or *exec-tool-yield-ms*
                                         +exec-default-poll-yield-ms+))
                        until-exit)
  "Park until eval ID prints new output or exits — or, with UNTIL-EXIT,
until it exits — then return what EVAL-STATUS returns."
  ;; The park is server-side wall-clock, wakes the moment there is something
  ;; to report, and wakes on turn cancellation. Park only when a short wait
  ;; beats ending the turn: an exited evaluation is read to the model on its
  ;; own (the exit wake), so a long park spends wall clock the harness would
  ;; otherwise not spend at all. Inside an eval call that passed
  ;; yield_time_ms, that window bounds the park unless :YIELD-MS says
  ;; otherwise.
  (receipt-call "eval-await" id)
  (let* ((registry (exec-registry-instance))
         (process (find-background-eval registry id)))
    ;; an unknown id parks nothing: the snapshot names the ids that do exist
    (when process
      (exec-park-until registry id
                       (clamp-exec-yield yield-ms +exec-default-poll-yield-ms+)
                       :cursor (unless until-exit
                                 process.model-cursor)))
    (note-background-read (eval-snapshot-plist registry id) process)))

(defun eval-interrupt (id &key force &aux (registry (exec-registry-instance))
                                          (process (find-background-eval registry id)))
  "Abort background eval ID: the soft interrupt condition by default, thread
termination with FORCE (for a form that traps the condition)."
  ;; Waits briefly for the exit, then returns what EVAL-STATUS returns.
  (when (and process (eq :running process.status)) (exec-stop-process process :hard force :by :model))
  (when process (exec-park-until registry id 2000))
  (eval-snapshot-plist registry id))

;;; --- the exit wake ----------------------------------------------------------
;;; The model is never the one waiting. Without this, a backgrounded
;;; evaluation would sit behind parked awaits burning provider rounds against
;;; an uncached prefix; its exit is read for the model by one (eval-status
;;; ID) call of the harness's own, run like any call it makes, so the result
;;; reaches the model -- and every transcript -- as that call's output, never
;;; as words the operator did not type (codex-rs keeps a background process's
;;; output to its tool results the same way). A turn of its session still
;;; running reads it at its next round boundary: after the tool batch it
;;; landed during, or where a terminal answer would have ended the turn
;;; (turn.lisp AGENT-LOOP). An idle session is woken for it: every exit
;;; waiting then in one turn of the harness's own, with no user message,
;;; opening on the reads. The edges -- the running turn's boundary, the
;;; watcher's final update (exit on an idle session) and the worker's cleanup
;;; (a turn ended with the exit unread) -- race through the claim below: one
;;; read per evaluation, ever. Live-only: the registry dies with the image,
;;; so an exit that never got its read is still durable as the exit fact.

(defun exit-wake-due-p (process)
  "Whether PROCESS's exit still owes its session the one wake: exited,
backgrounded, the wake unclaimed and the exit unread by the model."
  (and (eq :exited process.status) process.backgrounded
       (not process.wake-claimed) (not process.exit-observed-by-model)))

(defun unobserved-exited-evals (registry session-id)
  "(:id N :turn-id T) plists for SESSION-ID's exited backgrounded
evaluations still holding an unclaimed, unobserved exit — what
CLAIM-EXIT-WAKES claims from."
  (do-registry (process registry)
    when (and (equal session-id process.session-id) (exit-wake-due-p process))
    collect (list :id process.id
                  :turn-id process.turn-id)))

(defun claim-exit-wake (registry id)
  "Claim eval ID's one wake: true when this caller won it. The read is left
to the wake turn's (eval-status ID) call -- the reader that advances the
model cursor, marks the exit observed, and reaps."
  (when-let (process (bt2:with-lock-held ((exec-registry-lock registry))
                       ;; one wake per exit, and none for an exit the model already saw
                       (when-let (process (exec-find-locked registry id))
                         (and (exit-wake-due-p process) (setf process.wake-claimed t) process))))
    ;; The wake is where the model reads this snippet's value, so this is
    ;; where its values become the session's * history.
    (when-let (settle (shiftf process.snippet-capture nil)) (funcall settle))
    t))

(defun claim-exit-wakes (registry session-id &optional turn-id)
  "Claim every exit SESSION-ID still owes a read (EXIT-WAKE-DUE-P) whose
spawning turn completed, or is TURN-ID, the session's running turn. => the
ids won, in spawn order. A cancelled or failed spawner's exits are never
claimed: a stopped turn does not come back through the work it left."
  (loop for entry in (unobserved-exited-evals registry session-id)
        for spawner = (getf entry :turn-id)
        when (and (stringp spawner)
                  (or (equal spawner turn-id)
                      (equal nlk::+kind-turn-completed+ (nlk::turn-terminal-kind spawner)))
                  (claim-exit-wake registry (getf entry :id)))
          collect (getf entry :id)))

(defun release-exit-wakes (registry ids)
  "Give back the claimed wakes of IDS that were never read: a turn admitted
since the idle check kept the wake turn from starting, or the running turn
that claimed them ended first. A read exit stays read."
  (bt2:with-lock-held ((exec-registry-lock registry))
    (dolist (id ids)
      (when-let (process (exec-find-locked registry id))
        (setf process.wake-claimed nil)))))

(defun exec-end (exit-code shell-exit)
  "How an evaluation ended, in the word a surface shows it by: \"failed\"
when its form signalled (EXIT-CODE 1), \"stopped\" when someone stopped it
(130), \"exited\" when its last (sh ...) exited SHELL-EXIT and that is not
0, else \"done\"."
  (cond ((eql exit-code 1) "failed")
        ((eql exit-code 130) "stopped")
        ((and (integerp shell-exit) (/= 0 shell-exit)) "exited")
        (t "done")))

;;; A call someone stopped is not a call that failed: the operator's x or
;;; Stop, a cancelled turn, the gateway going down. Its result fact carries
;;; the stop under "stopped" (display metadata, persisted-history) -- who,
;;; why, after how long, and the last lines it wrote, which the model's text
;;; never held: (sh ...) answers its capture only when it returns. The
;;; model's text stays what it was; a surface draws the card from this.

(defun stop-document (by &key reason after-ms tail tail-cut)
  "A stop as a result fact keeps it: BY (:OPERATOR :TURN :SHUTDOWN :MODEL),
the REASON a turn was cancelled for, AFTER-MS the call ran, and TAIL, the
last lines it wrote, CUT when it wrote more before them."
  (json-object "by" (string-downcase by)
               :opt "reason" (and (not (eq by :operator)) reason)
               :opt "after_ms" after-ms
               :opt "tail" (and (plusp (length tail)) tail)
               :when (and tail tail-cut) "tail_cut" t))

(defun exec-stop-document-locked (process &aux (stop process.stopped))
  "PROCESS's stop as STOP-DOCUMENT keeps it, or NIL when no one stopped it.
The caller holds the registry lock."
  (when stop
    (multiple-value-bind (tail cut) (exec-exit-tail process (- (getf stop :end) process.base-offset))
      (stop-document (getf stop :by)
                     :reason (getf stop :reason)
                     :after-ms (elapsed-ms process.started (getf stop :at))
                     :tail tail :tail-cut cut))))

(defun exec-stop-document (process)
  "EXEC-STOP-DOCUMENT-LOCKED under the registry's lock."
  (bt2:with-lock-held ((exec-registry-lock (exec-registry-instance)))
    (exec-stop-document-locked process)))

(defun call-stop-document (turn call-id &optional started)
  "How CALL-ID, a call of TURN cut before it answered, was stopped: its
evaluation's stop when one ran for it, else the cause of TURN's cancel, the
call having run since STARTED (internal real time) when it began at all.
NIL for a call no stop cut."
  (or (when-let (process (and (plusp (length call-id))
                              (do-registry (process (exec-registry-instance))
                                thereis (and (equal call-id process.tool-call-id) process))))
        (exec-stop-document process))
      (when (nlk:turn-cancellation-requested-p turn)
        (multiple-value-bind (reason operator-p) (nlk:turn-cancel-cause turn)
          (stop-document (if operator-p :operator :turn)
                         :reason reason
                         :after-ms (and started (elapsed-ms started)))))))

(defun exec-end-words (exit-code shell-exit &aux (end (exec-end exit-code shell-exit)))
  "EXEC-END as a line of the shell says it: the word, and the status after
`exited' -- `exited 2'."
  (if (equal end "exited") (format nil "exited ~d" shell-exit) end))

(defun exit-wake-document (registry ids)
  "The wake's cause as its start fact records it (NLK:ADMIT-WAKE-TURN): each
of IDS with its exit code and its last (sh ...)'s status."
  ;; Called with the wakes claimed, so no reader has reaped the entries yet.
  (coerce (loop for id in ids
                for process = (find-background-eval registry id)
                collect (json-object "eval_id" id
                                     :opt "exit_code" (and process process.exit-code)
                                     :opt "shell_exit" (and process process.shell-exit)))
          'vector))

(defun exit-wake-message (ids)
  "The wake turn's first round, decided for the model: one eval call reading
each of IDS, as a provider's assistant message would carry them."
  (make-json-object
   "role" "assistant"
   "content" :null
   "tool_calls" (coerce (loop for id in ids
                              collect (chat-tool-call-object
                                       (nlk:make-durable-id "call") "eval"
                                       (encode-json-object
                                        (json-object "form" (format nil "(eval-status ~d)" id)))))
                        'vector)))

;;; --- the eval tool ------------------------------------------------------

;;; EVAL-AWAIT parks deliberately — backgrounding a deliberate park would hand
;;; back an eval id for a form whose whole point is waiting, recursively.
;;; Mutable material: a form that must outwait the yield window without
;;; detaching pushes its operator here in its own quick call first.
(defparameter *inline-eval-operators*
  '(eval-await eval-status eval-interrupt list-evals)
  "Operators whose forms run inline, never backgrounded.")

(defun inline-eval-form-p (form-string)
  "Operator match is by SYMBOL-NAME, not identity: the form's operator may
arrive package-qualified or re-homed (heredity replay re-interns evolved
symbols), and a deliberate park must stay inline regardless."
  ;; Every form of the text is judged, read as the eval tool reads it
  ;; (SNIPPET-FORMS: the parens completed, then READ-SNIPPET-FORMS): a text is
  ;; inline only when all of it is, so no second form rides in behind a
  ;; first. A form that is not a proper list is none of the four.
  (when-let (forms (snippet-forms form-string))
    (every (lambda (form)
             (and (typep form '(cons symbol))
                  (ignore-errors (list-length form))
                  (member (symbol-name (car form)) *inline-eval-operators*
                          :key #'symbol-name :test #'string=)))
           forms)))

;;; --- what a snippet leaves for the snippets after it ------------------------
;;; What a snippet leaves behind is its values: they become the * history the
;;; next snippet binds (*SNIPPET-HISTORIES*). EVAL-FORM-PRINTED (turn.lisp) writes
;;; it from the thread the snippet runs on. A snippet that outlives its yield window
;;; runs on after the turn has moved on, and would leave it whenever it
;;; happened to finish: * the value of a snippet the model has not read. So every
;;; snippet runs against a private copy, bound on its own thread, and what it
;;; left there is lifted out as its capture; the tool answers a settle
;;; thunk as its second value that folds the capture into the session's
;;; history on the turn thread, and a snippet that outlived its yield window is
;;; folded when its wake is composed, since that is when the model reads its
;;; value.
;;; Retention: the capture is live-only.

(defvar *snippet-settle-lock* (bt2:make-lock :name "snippet-settle")
  "The one lock the session tables are seeded from and settled under here:
a settle on the turn thread and a wake composed on the watcher thread never
interleave their read-modify-write of a session's history.")

;;; A capture is what one snippet left in its private session tables, as the
;;; thunk that folds its values, one per form when it finished, into the
;;; session's history: the newest under the depth rule EVAL-FORM-PRINTED
;;; applies, filed under the lock. A snippet that left nothing -- a failed
;;; snippet leaves the history as it was, the REPL's own rule -- captures NIL,
;;; which tells it apart from one whose last value was NIL. Idempotence is the
;;; caller's: a capture is taken from its registry entry as it is settled.

(defun call-snippet-captured (thunk)
  "Run THUNK, the snippet's evaluation, with the session's history rebound to
a private copy seeded from the entry EVAL-FORM-PRINTED files the snippet's
history under -- the ambient turn's session, else \"local\" -- and answer the
capture of what it wrote there ahead of THUNK's own values: (values CAPTURE
THUNK-VALUE...)."
  ;; The copy is a keyed table, since EVAL-FORM-PRINTED reads and writes it
  ;; by key; the seed is compared by identity afterwards, so a snippet that
  ;; wrote nothing captures nothing.
  (bind ((key (or (and *current-durable-turn* (nlk:durable-turn-session-id *current-durable-turn*))
                  "local"))
         (seed-history (bt2:with-lock-held (*snippet-settle-lock*) (snippet-history key)))
         (*snippet-histories* (alexandria:plist-hash-table (list key seed-history) :test #'equal))
         (answers (multiple-value-list (funcall thunk)))
         (history (gethash key *snippet-histories*)))
    (values-list
     (cons (and (not (eq history seed-history))
                (lambda ()
                  (bt2:with-lock-held (*snippet-settle-lock*)
                    ;; the answers become the newest of the key's history, which keeps at
                    ;; most the depth
                    (setf (gethash key *snippet-histories*)
                          (loop for one in (cons (first history) (gethash key *snippet-histories*))
                                repeat *snippet-history-depth* collect one)))))
           answers))))

(defun background-capable-eval (form yield-ms &optional strings)
  "The eval tool body: the eval snippet folded into the run tool's
yield-and-background lifecycle."
  ;; A form finishing inside the yield window
  ;; answers what inline EVAL-STRING answers; one still running detaches as a
  ;; background eval and answers the running envelope. Every answer is the
  ;; model's lane: plain text through the sanitizer, with the removal line —
  ;; the background stream by its streaming sanitizer, the stored value at
  ;; FINISH-BACKGROUND-EVAL, the inline snippet's raw capture here. STRINGS is
  ;; the call's side-channel object, re-bound wherever the form runs so
  ;; ($ "name") reads it on the background thread too.
  ;;
  ;; Answers (values TEXT SETTLE): SETTLE the thunk that files what the snippet
  ;; left for the session's tables (its capture), NIL for a snippet still
  ;; running — its wake files it — and for one that captured nothing.
  (if (inline-eval-form-p form)
      (let ((*exec-tool-yield-ms* (if (integerp yield-ms) yield-ms 10000))
            (*eval-strings* strings)
            (*read-is-the-snippet* (= 1 (length (snippet-forms form)))))
        (bind (((capture text error-p) (call-snippet-captured (lambda () (eval-string form))))
               (text (disclose-sanitized-text text)))
          (values (if error-p (failure text) text) capture)))
      (let* ((registry (exec-registry-instance))
             (ambient-turn *current-durable-turn*)
             ;; The form's ambient turn identity, re-established on the
             ;; evaluating thread: bt2 threads start with fresh bindings,
             ;; and forms that read their own turn or attach result
             ;; metadata must keep working there.
             ;; The dispatching turn's stack rides along too: a form that
             ;; redefines what the turn is running on must be judged
             ;; against THAT stack, not this worker's (provisional.lisp).
             (ambient '(*current-durable-turn* *live-turn* *executing-tool-call-id*
                        *provider-round* nlk:*caller-frames*))
             (ambient-values (mapcar #'symbol-value ambient)))
        (multiple-value-bind (id process)
            (spawn-background-eval
             registry form
             (lambda (stream &aux (*eval-strings* strings)
                                  ;; The evaluation's own: a verb's FAILURE in
                                  ;; the form never reaches the call, whose
                                  ;; verdict is how the form ended (below).
                                  (*tool-call-failed* nil)
                                  (*tool-result-metadata* nil))
               (progv ambient ambient-values
                 (multiple-value-bind (capture value-text error-p)
                     (call-snippet-captured (lambda () (eval-form-printed form stream)))
                   ;; Left on the registry entry for whoever delivers the
                   ;; value to the model: the in-window branch below, or
                   ;; the wake (EVAL-WAKE-TEXT).
                   (setf (exec-process-snippet-capture *current-exec-process*) capture)
                   (values value-text error-p
                           (and (hash-table-p *tool-result-metadata*)
                                *tool-result-metadata*)))))
             :command (if (hash-table-p strings)
                          (nlk:deref-literal-refs
                           form (lambda (name) (gethash name strings)))
                          form)
             :session-id (if ambient-turn ambient-turn.session-id "local")
             :turn-id (and ambient-turn ambient-turn.turn-id)
             :tool-call-id (or *executing-tool-call-id* ""))
          (unwind-protect
               (progn
                 (exec-park-until registry id (clamp-exec-yield yield-ms 10000))
                 (let ((snapshot (exec-snapshot-since registry id 0)))
                   (if (null snapshot)
                       (failure "ERROR: background evaluation vanished")
                       (ecase (getf snapshot :status)
                         (:exited
                          ;; In-window: the inline result, byte for byte —
                          ;; the printed stream recomposed with the value
                          ;; under the same rule EVAL-STRING uses, metadata
                          ;; handed back to the dispatch seam's binding. The
                          ;; call failed when the evaluation ended any way
                          ;; but done (EXEC-END): its form signalled, it was
                          ;; stopped, or its last (sh ...) exited non-zero —
                          ;; any non-zero, a grep's 1 included, the precedent
                          ;; Codex and Claude Code keep; the output still
                          ;; reads whole under the mark.
                          (let* ((output (getf snapshot :output))
                                 (value (or process.eval-value ""))
                                 (exit-code (or (getf snapshot :exit-code) 0))
                                 (stop (and (= 130 exit-code) (exec-stop-document process)))
                                 (text (truncate-eval-result
                                        (cond ((= 1 exit-code)
                                               (concatenate 'string (reader-lines output) value))
                                              ((string= output "") value)
                                              (t (format nil "~a~&~a" output value))))))
                            (when (hash-table-p process.eval-metadata)
                              (setf *tool-result-metadata* process.eval-metadata))
                            ;; A stopped call did not fail: its card says who
                            ;; stopped it and keeps what it had written.
                            (when stop (display-fact "stopped" stop))
                            (values
                             (if (or stop (equal "done" (exec-end exit-code process.shell-exit)))
                                 text
                                 (failure text))
                             (shiftf process.snippet-capture nil))))
                         (:running
                          ;; From here the evaluation outlives its tool
                          ;; call: registry, watcher exit fact, and band
                          ;; all treat it as background.
                          (mark-exec-backgrounded registry id)
                          (setf *tool-result-metadata*
                                (running-shell-metadata id form))
                          (encode-json-object
                           (make-json-object
                            "status" "running"
                            "eval_id" id
                            "output" (truncate-eval-result
                                      (getf snapshot :output))
                            "message" "Form is still evaluating in the background. Do not wait for it: go on with anything else, or end your turn — when the evaluation exits, the harness reads it for you with an (eval-status ID) call whose result carries its remaining output and value: in this turn, after the calls running then, or, once the turn has ended, in a turn that wakes this session. Meanwhile (eval-status ID) is one non-blocking look; (list-evals) lists evaluations; (eval-interrupt ID) aborts one ((eval-interrupt ID :force t) if it traps the soft abort). (eval-await ID) parks at most the yield_time_ms of the call it runs in (10000 when unset), and only when a short wait beats ending the turn — never park a long window.")))))))
            ;; A cancel that cut this call takes its evaluation with it.
            ;; The cancel reaches a parked call as the worker's unwind
            ;; (WITH-CANCELLABLE-WAIT's throw) or as the park's wake, and
            ;; either way the form the model asked for is still running
            ;; on its own thread with its shell children under it. Left
            ;; alone it would run to its own end and nothing would ever
            ;; read the value: a cancelled turn's exits wake nobody. The
            ;; soft abort is what (eval-interrupt) sends, and SH's unwind
            ;; kills the child tree; the watcher's exit fact closes the
            ;; row with the cancel's own text. Only this call's
            ;; evaluation: one handed off at an earlier yield window was
            ;; reported to the model as background and stays that way.
            (when (and ambient-turn
                       (nlk:turn-cancellation-requested-p ambient-turn)
                       (eq :running process.status))
              (multiple-value-bind (reason operator-p) (nlk:turn-cancel-cause ambient-turn)
                (exec-stop-process process :message "interrupted by user"
                                           :by (if operator-p :operator :turn) :reason reason))))))))

(defun ambient-session-directory (&key (turn *current-durable-turn*) present)
  "TURN's session cwd, the evaluating turn's by default; NIL outside a turn or an open store.
PRESENT refuses a folder that is not there any more, naming the move that
brings its sessions to where it went."
  ;; A folder renamed on disk strands every session recorded in it, and the
  ;; spawn's ENOENT named setsid, never the folder.
  (let* ((session (and turn (nlk:find-session (nlk:durable-turn-session-id turn))))
         (cwd (and session (nlk:durable-session-cwd session))))
    (when (and cwd present)
      (handler-case (nlk:check-session-cwd cwd)
        (nlk:missing-session-folder (condition)
          (error "this session's folder cannot be worked in: ~a. If it moved, ~
                  (nlk:move-sessions ~s \"NEW-FOLDER\") moves every session recorded ~
                  there; :directory runs one command elsewhere."
                 condition (string-right-trim "/" cwd)))))
    cwd))

;;; --- child liveness: /proc, the stall verdict -------------------------------
;;; The eval snippet's children are OS processes the harness supervises, not
;;; opaque pipes. A process stopped by job control (state T) can never
;;; finish on its own here — no shell holds job control to resume it, and a
;;; pending SIGTERM does not act on a stopped process — so silence must be
;;; impossible: the watcher's pass names the verdict on the output stream
;;; and collapses the tree, and the evaluation finishes with the kill
;;; status instead of parking its readers forever.

(defun proc-text (directory file)
  "FILE of the /proc DIRECTORY -- a process's or a thread's -- trimmed, or NIL
when it cannot be read."
  (ignore-errors (nlk:trimmed (uiop:read-file-string (merge-pathnames file directory)))))

(defun proc-state (pid)
  "PID's one-letter scheduler state from /proc, or NIL when unreadable."
  ;; The state is the token after the command's closing paren — the comm field
  ;; itself may contain spaces and parens, so the parse anchors on the LAST ).
  ;; NIL off Linux, where there is no /proc: the stall verdict is Linux's, and
  ;; elsewhere a child's session of its own (SESSION-ARGV) keeps the terminal
  ;; out of its reach in the first place.
  #+linux
  (when-let (text (proc-text (format nil "/proc/~d/" pid) "stat"))
    (ppcre:register-groups-bind (state) ("(?s).*\\) *(\\S)" text) (char state 0)))
  #-linux (progn pid nil))

(defun proc-descendants (pid &key (limit 128))
  "PID's descendant pids, breadth-first, bounded by LIMIT: through the
children files on Linux, through one process table elsewhere."
  ;; The table: every process as (PID . PPID) from one ps call, NIL when ps cannot
  ;; say; never on Windows, where KILL-TREE walks the tree itself.
  (let* ((table #+(or linux win32) nil
                #-(or linux win32)
                (multiple-value-bind (out err status)
                    (ignore-errors (nlk:run-bounded (list "ps" "-axo" "pid=,ppid=") :seconds 10))
                  (declare (ignore err))
                  ;; two integers a line, the pid and its parent
                  (when (eql status 0)
                    (loop for (id ppid) on (mapcar #'parse-integer
                                                   (ppcre:all-matches-as-strings "\\d+" out))
                          by #'cddr collect (cons id ppid)))))
         (frontier (list pid)) (found '()))
    (flet ((children (parent)
             ;; a parent's direct children: the union over its threads' children files
             #+linux (ignore-errors
                      (loop for task in (uiop:subdirectories
                                         (format nil "/proc/~d/task/" parent))
                            append (mapcar #'parse-integer
                                           (ppcre:all-matches-as-strings
                                            "\\d+" (or (proc-text task "children") "")))))
             #-linux (loop for (child . owner) in table
                           when (eql owner parent) collect child)))
      (loop while (and frontier (< (length found) limit))
            do (let ((next '()))
                 (dolist (parent frontier)
                   (dolist (child (children parent))
                     (unless (or (eql child pid) (member child found))
                       (push child found)
                       (push child next))))
                 (setf frontier next))))
    (nreverse found)))

(defun kill-process-tree (pid)
  "SIGKILL PID's whole tree: the descendants enumerated BEFORE any of them
dies (a killed parent reparents its children away), then PID, then PID's
process group — which under the sh session wrapper is the whole session
(KILL-TREE, which on Windows is `taskkill /T /F')."
  #-win32
  (dolist (victim (proc-descendants pid))
    (ignore-errors (sb-posix:kill victim sb-posix:sigkill)))
  (kill-tree pid))

(defconstant +exec-stall-ticks+ 10
  "Consecutive liveness passes (~2 s at the watcher's 200 ms tick) a
stopped process survives before its tree is collapsed — long enough that a
build system briefly SIGSTOPping a child passes through untouched.")

(defun scan-exec-child-stalls (registry)
  "One liveness pass over the running evaluations' shell children: a tree
holding a job-control-stopped process across +EXEC-STALL-TICKS+ passes is
collapsed — the verdict lands on the evaluation's output stream, the tree
is SIGKILLed (the one signal a stopped process cannot pend), and the
evaluation finishes with the kill status, waking its readers."
  ;; Returns the collapsed (ID . STOPPED-PID) pairs.
  (loop for (process . pid) in (do-registry (process registry)
                                 for pid = (exec-process-child-pid process)
                                 when (and pid (eq :running (exec-process-status process)))
                                 collect (cons process pid))
        ;; the first job-control-stopped (state T) process in the tree; /proc is Linux's
        for stopped = #+linux (find-if (lambda (candidate) (eql #\T (proc-state candidate)))
                                       (cons pid (proc-descendants pid)))
                      #-linux nil
        if (null stopped)
          do (setf process.stalled-ticks 0)
        else if (>= (incf process.stalled-ticks) +exec-stall-ticks+)
          do (setf process.stalled-ticks 0)
             (exec-append-output
              registry process
              (nlk:utf-8-octets
               (format nil "~%[process ~d stopped by job control and can ~
                            never finish; killing the process tree]~%"
                       stopped)))
             (kill-process-tree pid)
          and collect (cons process.id stopped)))

(defparameter *sh-capture-limit* 1000000
  "The most characters of one command's output SH reads before it cuts the
command off.")

(defun slurp-bounded (stream limit &optional on-chunk &aux (out (make-string-output-stream))
                                                           (buffer (make-string 65536))
                                                           (taken 0))
  "At most LIMIT characters of STREAM. => (values TEXT CUT-P), CUT-P true
when the read stopped at LIMIT rather than at end of file. ON-CHUNK, when
given, is called with each piece as it arrives."
  ;; READ-SEQUENCE rather than SLURP-STREAM-STRING because the whole point is
  ;; to stop: the slurp reads to end of file, and a command whose end of file
  ;; is a gigabyte away takes the image's heap with it on the way there. No
  ;; look past the ceiling to see whether more was coming -- that read blocks
  ;; on a child still thinking, which is the hang this exists to prevent -- so
  ;; output of exactly LIMIT characters is reported cut, which it was.
  ;; With ON-CHUNK the read takes what has arrived rather than filling the
  ;; buffer: READ-SEQUENCE on a pipe waits for 64 KiB or end of file, so a
  ;; command writing a line a second would reach the watcher only at its end.
  (loop for want = (min (length buffer) (- limit taken))
        for got = (cond ((<= want 0) 0)
                        ((null on-chunk) (read-sequence buffer stream :end want))
                        (t (if-let (first (read-char stream nil))
                             (loop initially (setf (char buffer 0) first)
                                   with got = 1
                                   for char = (and (< got want) (read-char-no-hang stream nil))
                                   while char
                                   do (setf (char buffer got) char) (incf got)
                                   finally (return got))
                             0)))
        while (plusp got)
        do (write-string buffer out :end got)
           (when on-chunk (funcall on-chunk (subseq buffer 0 got)))
           (incf taken got))
  (values (get-output-stream-string out) (>= taken limit)))

(defun sh (command &key directory)
  "Run COMMAND — a string through sh -c (/bin/sh; Git for Windows' sh.exe
on Windows), a list as argv with no shell — and answer its output: stdout and stderr merged as they arrived, with a
final [exit N] line when the status is not 0; the status is the second
value."
  ;; Stdin is empty, so a program that reads it sees end of file rather than
  ;; waiting on a terminal nobody holds; a heredoc in the command string is
  ;; the shell's own way to feed it. The child runs in DIRECTORY, else the
  ;; session's cwd -- refused, naming NLK:MOVE-SESSIONS, when that folder is
  ;; gone -- else this image's. A failing command never signals: its
  ;; output is the answer and its status the verdict. A long command rides the
  ;; eval snippet's lifecycle like any form — backgrounded at the yield window,
  ;; and killed when the evaluation unwinds (eval-interrupt), so a stuck child
  ;; does not outlive the call that started it.
  ;;
  ;; A LOUD command is cut instead: the capture stops at *SH-CAPTURE-LIMIT*
  ;; characters, the child's whole tree is killed, and the answer says where
  ;; it was cut. Without the capture limit, the read would run to end of file,
  ;; so `yes', a `find /', or a large build log would be read into the heap in
  ;; full before anything downstream could trim it — and the model sees at
  ;; most *EVAL-RESULT-LIMIT* of it either way, so the bytes past the ceiling
  ;; would never go anywhere but the collector.
  ;;
  ;; The child runs in a session of its own where the OS has them
  ;; (SESSION-ARGV: setsid on Linux), so it has no controlling terminal: a
  ;; program that touches /dev/tty would otherwise be stopped by job control
  ;; from the TUI's terminal; with the session it fails cleanly.
  ;;
  ;; The one shell verb the tool description names. Its predecessor was an
  ;; elided example, (uiop:run-program ... :output :string), whose hidden
  ;; arity — one command, string or list — and (sb-ext:run-program ...)'s
  ;; missing :search t; without the search path already right, a missing
  ;; interpreter would be misread as a missing command. One name with the
  ;; capture, the search path and the stdin already right leaves nothing to
  ;; rediscover.
  (check-type command (or string cons))
  (receipt-call "sh" command)
  ;; A unified diff is a mutation payload, never a shell command. Refuse it
  ;; before launch so a malformed tool route cannot execute diff markers via
  ;; /bin/sh and lose the rest of the turn to exit 2.
  (when (and (stringp command) (ppcre:scan "\\A *(?:\\*\\*\\* Begin Patch|diff --git )" command))
    (return-from sh
      (values "ERROR: patch payload rejected by sh; write the change with (edit path old new) or (write-file path text), the text in strings" 2)))
  (let ((snippet *current-exec-process*))
    ;; SPAWN-PROGRAM, not a fork of this image: a command costs the kernel a
    ;; spawn whatever the heap weighs (os.lisp). Stderr joins stdout in the
    ;; child, so one stream carries both in the order they were written.
    (bind (((pid output-stream _ process)
            (spawn-program (session-argv (if (stringp command)
                                             (shell-argv command)
                                             command))
                           :directory (or directory (ambient-session-directory :present t))
                           :error-output :output)))
      (when snippet (setf (exec-process-child-pid snippet) pid
                          (exec-process-stalled-ticks snippet) 0))
      (with-cleanup ((when snippet (setf (exec-process-child-pid snippet) nil))
                     (when (pid-live-p pid)
                       ;; Descendants first: a killed shell's children reparent to init
                       ;; and would otherwise outlive the call that started them (the
                       ;; timeout/sbcl/tail tree).
                       (kill-process-tree pid)
                       (ignore-errors (wait-for-child pid process)))
                     (ignore-errors (close output-stream)))
        (bind (((captured cut) (slurp-bounded output-stream *sh-capture-limit* *sh-chunk*)))
          ;; The tree goes before the wait: a child still writing into a pipe
          ;; nobody reads would block there forever, and the wait with it.
          (when (and cut (pid-live-p pid)) (kill-process-tree pid))
          (bind ((output (if cut
                             (format nil "~a~%[output cut at ~:d characters]"
                                     captured *sh-capture-limit*)
                             captured))
                 (status (wait-for-child pid process)))
          ;; The status on the evaluation, as the verdict its exit is shown
          ;; with: a surface reads the field, never the [exit N] line.
          (when snippet (setf (exec-process-shell-exit snippet) status))
          (values (if (eql status 0)
                      output
                      (format nil "~a~&[exit ~a]" output status))
                  status)))))))

;;; --- the strings side-channel and the file verbs -------------------------
;;; Literal text used to reach a file only by tunneling JSON -> Lisp string
;;; -> shell -> heredoc. The call's strings object is the one-escaping lane
;;; — JSON only, the layer providers emit natively — and EDIT/WRITE-FILE are
;;; the verbs that consume it, the SH posture applied to files: one name
;;; each, the capture and the anchoring already right.

(defun $ (name)
  "The entry named NAME in this eval call's strings object: the text
exactly as the model sent it, JSON escaping only."
  ;; A missing entry errors with what the call does carry, so the next call
  ;; corrects instead of guessing.
  (check-type name string)
  (let ((strings *eval-strings*)
        ;; a name with whitespace in it, or a long one, is the text itself:
        ;; echoed back as the key to send, it was sent again as the name, 30
        ;; times (gpt-6-astra, 2026-09)
        (literal-p (or (> (length name) 40)
                       (find-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) name))))
    (when (and literal-p (not (and (hash-table-p strings) (nth-value 1 (gethash name strings)))))
      (error "($ ~s) takes an entry's name, not its text: send strings ~
              {\"script\": <this text>} beside the form and write ($ \"script\")"
             (nlk:clip name 60)))
    (unless (hash-table-p strings)
      (error "this eval call carried no strings object; pass ~
              strings {~s: \"...\"} beside the form" name))
    (multiple-value-bind (value presentp) (gethash name strings)
      (cond ((not presentp)
             (error "no strings entry named ~s in this call; it carries ~
                     ~:[nothing~;~:*~{~s~^, ~}~]"
                    name (nreverse (alexandria:hash-table-keys strings))))
            ((not (stringp value))
             (error "strings entry ~s is not a string: ~s" name value))
            (t value)))))

(defun eval-snippet-path (path)
  "PATH resolved the way SH anchors its children: a relative path is
against the session's directory, else the image's."
  (if-let (directory (ambient-session-directory))
    (merge-pathnames path (uiop:ensure-directory-pathname directory))
    (merge-pathnames path)))

(defun write-file-text (path text)
  "The one atomic writer under EDIT and WRITE-FILE: parents created, the
file replaced through a same-directory rename, and the scribe's
shipped-definition memo dropped so DEFINITION-LOCATION answers the new spans
instead of stale ones."
  ;; A crash leaves either the old file or the complete new file, never a
  ;; partially written source file.
  (nlk:write-file-atomically path text)
  (nlk:forget-shipped-definitions))

(defun text-line-count (text)
  "Lines as an editor counts them: a trailing newline closes the last line
rather than opening an empty one."
  (+ (count #\Newline text)
     (if (or (zerop (length text))
             (eql #\Newline (char text (1- (length text)))))
         0 1)))

(defun source-clauses (path text before)
  "The clauses an EDIT or WRITE-FILE receipt ends with for a Lisp file, NIL
for any other file: the balance verdict when the tree breaks, \"parens
balance\" otherwise; then what the write just did to this running image,
said on the receipt: NLK:LOAD-SOURCE evaluates the definitions a file of this
image's own trees gained or changed back into it, and answers the clause —
NIL while the parens are broken, since a tree that does not close is not
loaded a piece at a time."
  ;; BEFORE is the file's text before the write, NIL for a file that did not
  ;; exist.
  (when (member (pathname-type path) '("lisp" "asd" "cl") :test #'equalp)
    (let ((verdict
            ;; where the tree breaks, lexed as the reader lexes it (LEX-FORM-STRUCTURE)
            (multiple-value-bind (tokens code-end missing) (lex-form-structure text)
              (flet ((head (position)
                       (let ((end (or (position #\Newline text :start position) (length text))))
                         (string-right-trim " " (subseq text position (min end (+ position 48)))))))
                (cond
                  ((stringp code-end) code-end)
                  ((null tokens) nil)
                  (t
                   (let ((depth 0) (open nil) (orphan (find :orphan tokens :key #'first)))
                     (dolist (token tokens)
                       (case (first token)
                         (:open (when (zerop depth) (setf open token))
                          (incf depth))
                         (:close (decf depth))))
                     (cond
                       (orphan
                        (format nil "a \")\" at line ~d closes nothing~@[ — the form opened at line ~d (~a) is over-closed~]"
                                (third orphan)
                                (and open (< (second open) (second orphan)) (third open))
                                (and open (< (second open) (second orphan))
                                     (head (second open)))))
                       ((and (plusp missing) open)
                        (format nil "the form opened at line ~d (~a) never closes: ~d paren~:p short"
                                (third open) (head (second open)) missing))))))))))
      (list (or verdict "parens balance")
            (and (null verdict) (nlk:load-source path :before before))))))

;;; --- patch: a kept definition changed as data ------------------------------
;;; The layer keeps the model's definitions as text; the only way to change one
;;; is to send the whole block again. A model may try the Lisp answer,
;;; (subst new old (function-lambda-expression #'f)); the layer would answer
;;; "changed but not kept", since only a toplevel defining form is recorded.
;;; PATCH makes that answer a verb: code is data, a change is a substitution,
;;; and the substituted definition is evaluated and kept as if the model had
;;; written it whole.

(defun replace-subform (old new form &aux (count 0))
  "FORM with every element EQUAL to OLD, at any depth, replaced by NEW; a
dotted tail is kept as it was. => (values PATCHED COUNT), COUNT the elements
it replaced."
  ;; FORM itself is an element, a list's tail never: a tail is not a subform
  ;; anyone wrote.
  (labels ((walk (form)
             (cond ((equal form old) (incf count) new)
                   ((consp form)
                    (nconc (loop for rest on form collect (walk (car rest)))
                           (cdr (last form))))
                   (t form))))
    (values (walk form) count)))

(defun comment-outside-strings-p (text)
  "Whether TEXT, Lisp source, carries a comment: a ; or a #| outside every
string, |symbol| and character literal."
  (loop with in = nil
        with i = 0
        while (< i (length text))
        do (let ((c (char text i)))
             (cond (in (cond ((char= c #\\) (incf i))
                             ((char= c in) (setf in nil))))
                   ((member c '(#\" #\|)) (setf in c))
                   ((char= c #\;) (return t))
                   ((and (char= c #\#) (< (1+ i) (length text)))
                    (case (char text (1+ i))
                      (#\\ (incf i 2))
                      (#\| (return t))))))
           (incf i)))

(defun patch-definition (name old new)
  "The function behind PATCH: NAME's kept definition with its one subform
OLD replaced by NEW, evaluated and kept again. Answers NAME."
  (receipt-call "patch" (string-downcase (princ-to-string name)))
  (let* ((key (nlk::definition-name-key name))
         (files (nlk::layer-files-for-name key)))
    ;; none to patch is written whole, and so are several — methods, the one meant among them
    (cond ((null files)
           (error "~(~a~) has no kept definition to patch; eval the whole definition instead"
                  name))
          ((rest files)
           (error "~(~a~) has ~d kept definitions; eval the one you mean whole"
                  name (length files))))
    (destructuring-bind (path start end) (first files)
      (bind ((file-text (uiop:read-file-string path :external-format :utf-8))
             (home (fourth (first (nlk:toplevel-definitions file-text :name key))))
             (package (or (and home (find-package home))
                          (error "~(~a~) is kept under package ~a, which this image lacks"
                                 name home)))
             (text (subseq file-text start end))
             (form (let ((*package* package) (*read-eval* nil))
                     (read-from-string text)))
             ((patched occurrences) (replace-subform old new form)))
        ;; the substituted form is printed again, and a printer keeps no comment
        (when (comment-outside-strings-p text)
          (error "~(~a~)'s kept definition carries ;; prose a patch would erase; ~
                  (edit ~s old new) changes its file"
                 name (sb-ext:native-namestring path)))
        (when (equal old form)
          (error "~a is the whole of ~(~a~); eval the new definition itself"
                 (source-text old) name))
        (cond ((zerop occurrences)
               (error "~a is not a subform of ~(~a~)'s kept definition, which reads:~%~a"
                      (source-text old) name (nlk:clip text 400)))
              ((> occurrences 1)
               (error "~a occurs ~d times in ~(~a~); give a larger subform that names one place"
                      (source-text old) occurrences name)))
        (let* ((new-text (let ((*package* package))
                           (write-to-string patched :escape t :case :downcase :pretty t
                                                    :right-margin 100 :readably nil
                                                    :length nil :level nil :circle nil)))
               (escrow (nlk:definition-escrow patched)))
          (let ((*package* package))
            (eval patched))
          (format t "~&[patched ~(~a~)]~%" name)
          ;; Kept the way a snippet keeps a definition it evaluated: held while the
          ;; name is running on the stack, else recorded now; and named to the
          ;; snippet as kept, so the change is not reported as one the layer lost.
          (or (nlk:hold-definitions escrow patched new-text (length new-text))
              (nlk:record-definitions patched new-text (length new-text)
                                      :package (package-name package)))
          (pushnew key nlk::*source-loaded-names* :test #'string-equal)
          name)))))

(defmacro patch (name old new)
  "Change one subform of NAME's kept definition and keep the result: OLD,
written as it stands in the definition, is replaced by NEW — both forms,
neither evaluated — and the patched definition is evaluated and kept
again, as if written whole."
  ;; Zero occurrences of OLD errors showing the definition, several error with
  ;; the count: widen OLD until it names one place. (patch tiles (+ x 6) (+ x
  ;; 8)) is a fix that costs its two subforms.
  `(patch-definition ',name ',old ',new))

(defun edit (path old new)
  "Replace the one occurrence of OLD in the file at PATH with NEW and
answer a receipt naming the line."
  ;; Zero occurrences errors with the point
  ;; where the file stops matching OLD — the divergence, never a bare
  ;; not-found — and more than one errors with the count: widen OLD until it
  ;; names one place. A relative PATH is against the session's directory.
  ;; Deliver OLD and NEW through the call's strings object — ($ "old"),
  ;; ($ "new") — rather than escaping them into the form.
  (check-type old string)
  (check-type new string)
  (receipt-call "edit" path)
  (when (zerop (length old))
    (error "OLD is empty; name the text to replace"))
  (let* ((resolved (eval-snippet-path path))
         (text (handler-case (uiop:read-file-string resolved
                                                    :external-format :utf-8)
                 (error (e) (error "cannot read ~a: ~a"
                                   (namestring resolved) e))))
         (start (search old text)))
    (cond
      ((null start)
       ;; the divergence: OLD's longest leading piece that occurs, at the first place it does
       (multiple-value-bind (matched position)
           (loop with best = 0 and at = nil
                 for start below (length text)
                 for common = (or (mismatch old text :start2 start) (length old))
                 when (> common best) do (setf best common at start)
                 finally (return (values best at)))
         (if (zerop matched)
             (error "~a does not contain OLD, not even its first character"
                    (namestring resolved))
             (let ((point (+ position matched)))
               (error "~a does not contain OLD. It matches the first ~d ~
                       character~:p (through line ~d), then diverges:~%~
                       file continues ~s~%OLD continues  ~s"
                      (namestring resolved) matched
                      (1+ (count #\Newline text :end point))
                      (subseq text point (min (length text) (+ point 60)))
                      (subseq old matched (min (length old) (+ matched 60))))))))
      ((search old text :start2 (+ start (length old)))
       (error "OLD occurs ~d times in ~a; widen it until it names one place"
              (ppcre:count-matches `(:sequence ,old) text) (namestring resolved)))
      (t
       (let ((edited (uiop:strcat (subseq text 0 start) new (subseq text (+ start (length old))))))
         (write-file-text resolved edited)
         (format nil "edited ~a: line ~d, -~d +~d line~:p~{~@[; ~a~]~}"
                 (namestring resolved)
                 (1+ (count #\Newline text :end start))
                 (text-line-count old) (text-line-count new)
                 (source-clauses resolved edited text)))))))

(defun write-file (path text)
  "Write TEXT to the file at PATH — parents created, an existing file
replaced whole — and answer a one-line receipt."
  ;; A relative PATH is against the session's directory. Deliver TEXT through
  ;; the call's strings object — ($ "text") — rather than escaping it into the
  ;; form.
  (check-type text string)
  (receipt-call "write-file" path)
  (let* ((resolved (eval-snippet-path path))
         (before (nlk:read-text resolved)))
    (write-file-text resolved text)
    (format nil "wrote ~a: ~d line~:p, ~d character~:p~{~@[; ~a~]~}"
            (namestring resolved) (text-line-count text) (length text)
            (source-clauses resolved text before))))

;;; --- look: an image file, shown at the next round ------------------------
;;; The model has no eyes of its own on the filesystem: an image it wants to
;;; see must ride a provider round as an image part, and the one shape every
;;; lane carries is a user message of image parts (the Zig kernel's
;;; injectToolGeneratedImages; codex's view_image puts the same bytes inside
;;; the function_call_output, which only the Responses wire can hold). The
;;; verb stages the file on the live turn under its own call id (turn.lisp
;;; *TURN-IMAGES*), the engine places it behind this round's tool results
;;; (context.lisp ATTACH-VISIBLE-IMAGES), and the turn's end forgets it —
;;; the horizon an operator's paste has. The receipt is what history keeps.
;;; Without the verb, a model asked to view an image would say it has no
;;; way to see it.

;;; Over it the verb refuses and says so, rather than staging bytes every
;;; remaining round of the turn would then fail on.
(defparameter *image-max-bytes* (* 5 1024 1024)
  "Ceiling on one image LOOK attaches: Anthropic's per-image limit, the
tightest of the lanes'.")

;;; What the operator sees of a LOOK is the other half of the verb. The
;;; provider gets the bytes; the transcript gets a durable image fact naming
;;; the file, its media type and its pixel size, and a shell whose terminal
;;; speaks the kitty graphics protocol draws the image itself under the card
;;; (tui/ui.lisp KITTY-IMAGE-PLACEMENTS). The fact carries a path, never the
;;; bytes: it is durable history, and megabytes of base64 in the event log
;;; would be paid for on every hydration of the session. The protocol takes
;;; PNG only, so a JPEG, GIF or WebP is transcoded once here, into a
;;; content-addressed file in the cache — the source bytes name it, so the
;;; same image looked at twice transcodes once and a changed file never
;;; shows the old picture.

(defun image-cache-file (octets type)
  ;; The cache's file of extension TYPE for an image, named by its bytes OCTETS.
  (nlk:cache-path (format nil "nodecode/previews/~a.~a" (subseq (nlk::sha256-text octets) 7) type)))

(defun image-preview-png (resolved octets media-type)
  "The PNG file a terminal can be shown for the image at RESOLVED: RESOLVED
itself when its bytes already are a PNG, a cached transcode of it when they
are not, and NIL when no transcoder on this box would make one
(NLT:*PNG-TRANSCODERS*). Answers a namestring."
  (if (equal media-type "image/png")
      (namestring resolved)
      (let ((cached (image-cache-file octets "png")))
        (with-handlers ((error () nil))
          (let ((ready (or (and (equal "image/png"
                                       (image-media-type (nlt:file-octets cached)))
                                cached)
                           (progn (ensure-directories-exist cached)
                                  (nlt:transcode-to-png resolved cached)))))
            (and ready (namestring ready)))))))

;;; The picture an answer names is drawn from its bytes as they were when the
;;; answer was written, whatever the file holds later: the image keeps them
;;; beside the transcodes, named the same way, and the fact names that copy
;;; for whoever draws it.

(defun image-cache-copy (octets media-type)
  "OCTETS, an image's bytes of MEDIA-TYPE, as a file of the cache named by
them, written once. => its pathname, or NIL when it could not be written."
  (let ((copy (image-cache-file octets (subseq media-type (length "image/")))))
    (with-handlers ((error () nil))
      (unless (probe-file copy)
        (ensure-directories-exist copy)
        (alexandria:write-byte-vector-into-file octets copy))
      copy)))

(defun look (path)
  "Show yourself the image file at PATH — PNG, JPEG, GIF or WebP, by its
bytes — and answer a one-line receipt."
  ;; The image is attached to this turn under this call and reaches you at the
  ;; next round, after this call's result; it leaves the window when the turn
  ;; ends while the receipt stays, so look again with another call. A relative
  ;; PATH is against the session's directory. The operator sees it too: the
  ;; transcript draws the file under the call, as the image itself where the
  ;; terminal speaks the kitty graphics protocol. Refuses a file that is not
  ;; an image, one over *IMAGE-MAX-BYTES*, a model models.dev says reads text
  ;; only, and any image once this turn's provider has refused one: a model
  ;; the catalog does not know is tried, and a request it refuses is re-sent
  ;; without the image, a note standing where the image was (turn.lisp
  ;; CALL-PROVIDER).
  (receipt-call "look" path)
  (bind (((octets resolved) (image-file-octets path))
         (media-type (image-media-type octets)))
    (when (null (model-reads-images-p *model* *provider*))
      (error "~a reads text only, by models.dev; (select-model ...) one whose input includes image, then look again"
             *model*))
    (let ((turn (or *current-durable-turn*
                    (error "no turn is running to show ~a in" (namestring resolved)))))
      (when (turn-images-withheld-p (nlk:durable-turn-turn-id turn))
        (error "~a ~a did not accept the request that carried this turn's last image and would not take ~a either; work from the file another way, or (select-model ...) one whose input includes image"
               *provider* *model* (namestring resolved)))
      (attach-turn-images (nlk:durable-turn-turn-id turn)
                          (list (cons media-type
                                      (cl-base64:usb8-array-to-base64-string
                                       octets)))
                          :anchor (json-value *executing-tool-call-id* :text)))
    (let ((fact (display-fact "image" (image-fact resolved octets))))
      (format nil "[image: ~a, ~a~@[~a~], ~:d bytes; shown to you from the next round until this turn ends]"
              (namestring resolved) media-type
              (when-let (width (gethash "width" fact))
                (format nil " ~dx~d" width (gethash "height" fact)))
              (length octets)))))

(defun image-file-octets (path)
  "The bytes of the image file at PATH, resolved as LOOK resolves it, once
they open an image under the ceiling. => (values OCTETS RESOLVED)."
  (let* ((resolved (eval-snippet-path path))
         (octets (handler-case (alexandria:read-file-into-byte-vector resolved)
                   (error (e) (error "cannot read ~a: ~a"
                                     (namestring resolved) e)))))
    (unless (image-media-type octets)
      (error "~a is not an image this verb can show: its bytes open no PNG, JPEG, GIF or WebP"
             (namestring resolved)))
    (when (> (length octets) *image-max-bytes*)
      (error "~a is ~:d bytes, over the ~:d-byte ceiling one image may take; shrink it first"
             (namestring resolved) (length octets) *image-max-bytes*))
    (values octets resolved)))

(defun image-fact (resolved octets &key target &aux (media-type (image-media-type octets)))
  "The durable fact a transcript draws OCTETS, the image file at RESOLVED,
from: what it is, how big, and the files that hold it, never its bytes.
TARGET is the file as an answer wrote it: the fact is then an answer's, \"id\"
its bytes' name, and the bytes are kept (IMAGE-CACHE-COPY) and drawn from.
RESOLVED NIL is a picture that came as bytes with no file -- a prompt's
(PROMPT-IMAGES): kept and named the same way, with no path."
  (bind (((width height) (nlt:image-dimensions octets))
         (kept (or target (null resolved)))
         (copy (and kept (image-cache-copy octets media-type))))
    (json-object "payload_kind" "file"
                 "source" (cond (target "answer") (resolved "look") (t "prompt"))
                 :opt "target" target
                 :opt "id" (and kept (subseq (nlk::sha256-text octets) 7))
                 :opt "path" (and resolved (namestring resolved))
                 "media_type" media-type
                 "bytes" (length octets)
                 :opt "width" width
                 :opt "height" height
                 :opt "copy_path" (and copy (namestring copy))
                 :opt "png_path" (when-let (file (or copy resolved))
                                   (image-preview-png file octets media-type)))))

;;; A picture sent with a prompt reaches the model as the turn's attachment
;;; and leaves with the turn (turn.lisp *TURN-IMAGES*); the operator's own
;;; message keeps it the way an answer keeps the pictures it names: the input
;;; fact carries an image fact for each, and the bytes are kept beside a
;;; LOOK's transcodes. Retention: persisted history, paths and never bytes.

(defun prompt-images (images)
  "The image facts of IMAGES, a prompt's (MEDIA-TYPE . BASE64) attachments,
in order, each one's bytes kept (IMAGE-FACT); NIL when none opens an image.
Never signals."
  (loop for (nil . data) in images
        for octets = (and (stringp data)
                          (ignore-errors (cl-base64:base64-string-to-usb8-array data)))
        for fact = (and octets (image-media-type octets) (ignore-errors (image-fact nil octets)))
        when fact collect fact into facts
        finally (return (and facts (coerce facts 'vector)))))

;;; --- a picture an answer names ---------------------------------------------
;;; LOOK shows the model a file; an answer shows the operator one by naming it,
;;; ![alt](path), where the words about it stand. The transcript cannot open
;;; what the answer names -- a page opens no path -- so the engine reads it
;;; once, as the message completes and the way LOOK reads a file, and the
;;; message's fact carries a LOOK's image fact for each (turn.lisp AGENT-LOOP):
;;; one translation, the same on the live stream and in replay. The bytes are
;;; kept beside a LOOK's transcodes, so the picture an answer drew stays that
;;; picture when the file changes. Nothing is sent to the model, and only a
;;; file is read: a target with a scheme other than file: is a link, as is a
;;; file that is missing, no image or over the ceiling. Retention: persisted
;;; history, paths and never bytes; the bytes are the cache's, as a LOOK's
;;; transcode is.

(defparameter *answer-image-limit* 16
  "How many pictures one answer's fact carries: what a shell holds at once.")

(defun markdown-image-targets (text &aux (fence nil) (targets '()))
  "The targets of the Markdown images in TEXT, ![alt](target) or
![alt](<target>), each once and in order, without their angle brackets; an
image written inside a fence or a code span is code."
  (dolist (line (nlk:lines text) (nreverse targets))
    (let ((head (string-left-trim " " line)))
      (cond (fence (when (uiop:string-prefix-p fence head) (setf fence nil)))
            ((ppcre:scan "^(?:```|~~~)" head) (setf fence (subseq head 0 3)))
            (t (ppcre:do-register-groups (bracketed bare)
                   ("!\\[[^\\]]*\\]\\((?:<([^<>]+)>|([^<)\\s][^)\\s]*))\\)"
                    (ppcre:regex-replace-all "`[^`]*`" line ""))
                 (pushnew (or bracketed bare) targets :test #'equal)))))))

(defun answer-images (text)
  "The image facts of the files TEXT, a completed answer, names as Markdown
images, each read as LOOK reads a file; NIL when it names none that is an
image. Never signals."
  (when-let (targets (and (search "](" text) (markdown-image-targets text)))
    (loop for target in targets
          ;; A file: URL is its path, a name with no scheme is itself, and
          ;; anything else is somewhere this never reaches.
          for path = (cond ((ppcre:scan "^(?i)file:" target)
                            (ppcre:regex-replace "^(?i)file:(?://(?:localhost)?(?=/))?" target ""))
                           ((not (ppcre:scan "^[A-Za-z][A-Za-z0-9+.-]+:" target)) target))
          for fact = (ignore-errors
                      (when path
                        (multiple-value-bind (octets resolved) (image-file-octets path)
                          (image-fact resolved octets :target target))))
          when fact collect fact into facts
          until (= (length facts) *answer-image-limit*)
          finally (return (and facts (coerce facts 'vector))))))

;;; --- a snippet's forms -----------------------------------------------------
;;; The toplevel forms of an eval snippet, read the way the tool reads them: the
;;; parens a multi-line snippet leaves open completed, the forms read in the
;;; evolved package, a leading ;; note passing as the comment it is. The eval
;;; lane asks it how many forms a snippet is, and the reflection cell reads a
;;; turn's snippets through it.

(defun snippet-forms (text &aux (*package* (find-package '#:nodecode.evolved)))
  "The toplevel forms of the snippet TEXT as the eval tool reads them, or NIL
when it does not read -- then eval reports the reader's verdict and runs
nothing, which is no fence's to repeat."
  (with-handlers ((error () nil) (storage-condition () nil))
    (when-let (completed (complete-form-text text)) (mapcar #'car (read-snippet-forms completed)))))

(register-tool
 "eval"
 (list :description
       (concatenate 'string
                    "Evaluate Common Lisp forms in the live image and return their printed values; only the last value prints in full. (sh \"ls -la\") runs a shell command and returns its output. (edit path ($ \"old\") ($ \"new\")) with strings {\"old\": \"<the text now in the file>\", \"new\": \"<its replacement>\"} replaces the one occurrence of old in the file; (write-file path ($ \"text\")) with strings {\"text\": \"<the whole file>\"} writes a whole file, parents created. $ takes an entry's name, never the text itself. (look path) shows you an image file. In a Lisp string a backslash only escapes the next character (\"\\n\" reads as n), so text dense with backslashes, double quotes or newlines goes in the strings object, whichever verb takes it; a shell command carrying a heredoc goes there whole: strings {\"script\": \"...\"} with (sh ($ \"script\")). A snippet's opening ;; line is its note, kept past eviction: write what you would resume from. When you use eval for work, begin the form with one concise leading ;; progress comment naming what you are about to do; this comment becomes the live activity label in channel cards and the TUI."
                    #+linux " A snippet's last form, when it only reads - no verb, file, process, socket, assignment or definition - is watched: it runs again after each later snippet and comes back as one line only when its value changes."
                    " (help :eval) has the rest.")
       :parameters
       (make-json-object
        "type" "object"
        "properties" (make-json-object
                      "form" (make-json-object
                              "type" "string"
                              "description" "One or more Common Lisp forms, evaluated in order. Multi-line welcome: parens a multi-line snippet leaves open are closed by its indentation; a one-line snippet must close its own.")
                      "yield_time_ms" (make-json-object
                                       "type" "integer"
                                       "description" "Milliseconds to wait before the form continues in the background (never killed; the harness reads its exit to you when it lands). Default 10000, max 600000; also bounds an (eval-await N) park.")
                      "strings" (make-json-object
                                 "type" "object"
                                 "additionalProperties" (make-json-object
                                                         "type" "string")
                                 "description" "Named literal texts sent beside the form, as {\"old\": \"...\"}: the form reads each by its key, ($ \"old\"), exactly as sent, with only JSON escaping. A key is a short name you choose, never the text."))
        "required" (vector "form")))
 (lambda (arguments)
   (with-json ((form :any "form")
               (yield :integer "yield_time_ms")
               (strings :object "strings"))
       arguments
     (if (stringp form)
         (background-capable-eval form yield strings)
         ;; Echo what actually decoded: a model that sees only
         ;; "requires form" retries the identical call, but one
         ;; that sees its arguments arrived as {} can rewrite.
         (failure (format nil "ERROR: eval requires a \"form\" string ~
                               argument; received ~a"
                          (if (hash-table-p arguments)
                              (encode-json-object arguments)
                              (format nil "~s" arguments))))))))

;;; --- band surface plan (src/tui/render_plan.zig background cluster) ---------
;;; Pure material for the status-bar band renderer (ui.lisp) and its tests:
;;; budget, newest-first ordering with the overflow collapse, and the
;;; frozen-head/marker/live-tail output window.
;;; All display-line semantics reuse the preview counters the reducer keeps.

(defconstant +background-max-output-lines+ 5
  "render_plan.zig max_background_process_output_lines.")

;;; The glyph is an activity odometer (a stalled process visibly freezes), not
;;; a wall-clock spinner: the index is ACTIVITY-EVENTS mod 8.
(defparameter +background-odometer-frames+
  #("⠁" "⠂" "⠄" "⡀" "⢀" "⠠" "⠐" "⠈")
  "render_plan.zig activity odometer frames, verbatim order.")

(defun background-odometer-mark (activity-events)
  (aref +background-odometer-frames+ (mod (max 0 activity-events) 8)))

(defun bg-row-written-lines (row)
  "The lines ROW's exec has written so far: the complete ones, and the one
still open."
  (+ (bg-band-row-output-seen-lines row) (if (bg-band-row-output-line-dirty row) 1 0)))

(defun background-output-window (row spare &optional (cap +background-max-output-lines+))
  "The output window shape for ROW at SPARE rows, its tail at most CAP lines
(render_plan.zig backgroundOutputWindow), or NIL without output."
  ;; => plist
  ;;   (:head LINES :omitted N :tail LINES :collapse-p BOOL)
  ;; COLLAPSE-P marks the spare==1 shape: the omission collapses into the single
  ;; tail row's gutter as ⋮, uncounted.
  (let* ((total (bg-row-written-lines row))
         ;; display lines are the non-empty ones (model/background.zig display-line counting)
         (kept (remove "" (nlk:lines (octets-to-text (bg-band-row-output-tail row))) :test #'equal))
         ;; a tail that dropped earlier output was cut by bytes, partway into
         ;; its first line: the exec view reads every kept line (KEPT-TAIL-TEXT)
         (tail-lines (last (if (> total (length kept)) (rest kept) kept) cap)))
    (when (and (plusp total) (plusp spare) tail-lines)
      (cond
        ;; Everything fits and the tail holds the whole stream.
        ((and (<= total spare) (>= (length tail-lines) total))
         (list :head '() :omitted 0 :collapse-p nil
               :tail (last tail-lines total)))
        ((= spare 1)
         (list :head '() :omitted (max 0 (1- total))
               :collapse-p t :tail (last tail-lines 1)))
        (t
         (let* ((head-lines (remove "" (butlast (uiop:split-string   ; newline-closed lines only
                                                 (octets-to-text (bg-band-row-output-head row))
                                                 :separator '(#\Newline)))
                                    :test #'equal))
                (head-take (max 0 (min (length head-lines) (- spare 3) 2)))
                (tail-take (max 0 (min (length tail-lines) (- spare head-take 1)))))
           (list :head (subseq head-lines 0 head-take)
                 :omitted (max 0 (- total head-take tail-take)) :collapse-p nil
                 :tail (last tail-lines tail-take))))))))

(defun background-kept-window (row)
  "All ROW's exec retained as an output window: two head lines, the clause,
up to 400 lines of tail -- what the exec view and an open band exec show."
  (background-output-window row 403 400))

;;; --- tool-result display projection (protocol/background_exec.zig) ----------

(defun shell-result-display-text (text &aux (parsed (ignore-errors (nlk:decode-json (or text "")))))
  "parseOutputEnvelope: the display body for a run/write_stdin result —
foreground {stdout,stderr,exit_code} shows stdout + newline + stderr;
background {status,output} shows the output stream; anything that is not an
envelope returns NIL and renders unchanged."
  (when (hash-table-p parsed)
    (with-json ((stdout :string "stdout")
                (err :string "stderr" :default "")
                (status :string "status")
                (output :string "output"))
        parsed
      (cond
        ((and stdout (nth-value 1 (gethash "exit_code" parsed)))
         (format nil "~{~a~^~%~}" (remove "" (list stdout err) :test #'string=)))
        ((and status output
              (or (equal status "running") (equal status "exited")))
         output)))))

(defun eval-string-result-display-text (text &aux (trimmed (nlk:trimmed (or text ""))))
  "The display body for an eval result that is exactly one printed
string literal: the operator reads the text the form produced, not its
escaped READ syntax with a lone closing-quote row — the same
person-over-protocol rule as SHELL-RESULT-DISPLAY-TEXT above, for the
organism's own snippet."
  ;; Any other shape — bare values, printed output before the value, a
  ;; truncation tail, the empty literal — returns NIL and renders unchanged.
  (when (and (> (length trimmed) 1)
             (char= #\" (char trimmed 0)))
    (ignore-errors
     (multiple-value-bind (value end)
         (let ((*read-eval* nil))
           (read-from-string trimmed))
       (when (and (stringp value)
                  (plusp (length value))
                  (eql end (length trimmed)))
         value)))))

(defun tool-result-shown (tool text metadata &aux (read (json-value metadata :object "background_read")))
  "The body a TOOL call's card shows the operator for its result TEXT and
display METADATA: a background job's read as what the job wrote
(NOTE-BACKGROUND-READ), a result envelope as its stream, an eval result
without the harness's notes, anything else as it is."
  ;; One reading for every surface: the shell's card and the page's.
  (if read
      (or (json-value read :string "text") "")
      (or (shell-result-display-text text) (tool-result-display-text tool text) text)))

(defun tool-result-display-text (tool text)
  "The display body for a TOOL result that carries notes for the model, or
NIL: an eval result's [text: ...] header's text alone, then a literal as
EVAL-STRING-RESULT-DISPLAY-TEXT reads it."
  ;; A fresh box's first turn read `[text: 1 line, 5 characters]' over a
  ;; command's output (the drive).
  (when (equal tool "eval")
    (let ((body (ppcre:regex-replace-all "(?m)^\\[text: [\\d,]+ lines?, [\\d,]+ characters\\]\\n"
                                         (or text "") "")))
      (or (eval-string-result-display-text body) body))))
