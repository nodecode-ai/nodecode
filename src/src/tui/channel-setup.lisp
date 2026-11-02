;;;; channel-setup.lisp --- /gateway setup on the shell: a channel set up step by step on a sheet. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; A channel set up on a sheet in the middle of the frame, over the
;;; organism's own config surface -- the calls the web page's Control >
;;; Channels and /settings make: the channel's cell switched on (POST
;;; /api/gateway/cells), the pasted token kept in the section's 0600 file
;;; (POST /api/gateway/config {secret}) and checked against the platform
;;; (/config/probe), a wait while the platform has nothing to pick from (a bot
;;; in no server yet), each field picked from a checklist of what the token
;;; sees (/config/probe {field}) and written (/config {path, value}), the
;;; channels started again (/api/channels?op=restart). A finished step stands
;;; as one line, and only the open one is drawn whole. Every call runs off the
;;; loop (SHEET-CALL); a wait asks again every few seconds until there is
;;; something to pick, or the sheet is closed. The sheet reads and writes no
;;; file itself; every refusal is the organism's, in its words.

;;; --- the choices ----------------------------------------------------------------

(defun gateway-setup-choice-name (label)
  "What a choice's LABEL calls it: the label up to its first two spaces --
the `#general' of `#general  Home', the `@mods' of `@mods  Home'."
  (subseq label 0 (or (search "  " label) (length label))))

(defun gateway-setup-named (text choices)
  "The id of the first of CHOICES ((ID . LABEL) ...) whose name is TEXT,
case aside and a leading # or @ optional on either side, or NIL."
  (flet ((bare (name) (string-left-trim "#@" (string-trim '(#\Space #\Tab) name))))
    (let ((wanted (bare text)))
      (and (plusp (length wanted))
           (car (find-if (lambda (choice)
                           (string-equal wanted (bare (gateway-setup-choice-name (cdr choice)))))
                         choices))))))

(defun gateway-setup-picks (reply choices)
  "The ids REPLY names, each once, in its order: `all' (or *) every one of
CHOICES ((ID . LABEL) ...); else each comma-separated entry a choice's name
(GATEWAY-SETUP-NAMED), or words each a 1-based number into CHOICES, a range
of them (2-5), a name, or an id as typed."
  (flet ((number-in (text)
           (and (plusp (length text)) (every #'digit-char-p text)
                (let ((number (parse-integer text)))
                  (and (<= 1 number (length choices)) number)))))
    (if (member (string-trim '(#\Space #\Tab) reply) '("all" "*") :test #'string-equal)
        (mapcar #'car choices)
        (remove-duplicates
         (loop for entry in (uiop:split-string reply :separator '(#\,))
               for named = (gateway-setup-named entry choices)
               if named collect named
                 else append
                      (loop for word in (uiop:split-string entry :separator '(#\Space #\Tab))
                            for dash = (position #\- word :start (min 1 (length word)))
                            for from = (and dash (number-in (subseq word 0 dash)))
                            for to = (and dash (number-in (subseq word (1+ dash))))
                            for number = (number-in word)
                            unless (string= word "")
                              append (cond ((and from to (<= from to))
                                            (mapcar #'car (subseq choices (1- from) to)))
                                           (number (list (car (nth (1- number) choices))))
                                           (t (list (or (gateway-setup-named word choices) word))))))
         :test #'equal :from-end t))))

(defun gateway-setup-names (ids choices &key (shown 3))
  "IDS as their names among CHOICES ((ID . LABEL) ...), the first SHOWN of
them and how many more, `all N' when they are every one of more than SHOWN."
  (let ((names (mapcar (lambda (id)
                         (let ((choice (assoc id choices :test #'equal)))
                           (if choice (gateway-setup-choice-name (cdr choice)) id)))
                       ids)))
    (format nil "~:[~*~;all ~d: ~]~{~a~^, ~}~@[, +~d more~]"
            (and (> (length ids) shown) (= (length ids) (length choices))) (length ids)
            (subseq names 0 (min shown (length names)))
            (and (> (length names) shown) (- (length names) shown)))))

(defun setup-pick-rows (choices picked cursor &key (rows 20))
  "The checklist over CHOICES ((ID . LABEL) ...) as rows to draw, top down:
(:ALL POINTED TICKED COUNT), (:HEAD SERVER), (:CHOICE POINTED TICKED NAME
AFTER NESTED INDEX), (:MORE COUNT) -- the ids in PICKED ticked, the CURSOR
row (0, the all row) pointed at, at most ROWS choices shown around it. A
label's tail (`#general  Home') heads a group when two or more share it, a
server's channels, them NESTED under it; a lone one rides AFTER its name."
  (let* ((count (length choices))
         (top (max 0 (min (- cursor (floor rows 2)) (- count rows))))
         (end (min count (+ top rows)))
         (tails (mapcar (lambda (choice)
                          (let ((cut (search "  " (cdr choice))))
                            (and cut (subseq (cdr choice) (+ cut 2)))))
                        choices))
         (out (list (list :all (zerop cursor)
                          (and choices (every (lambda (choice) (member (car choice) picked :test #'equal)) choices))
                          count))))
    (when (plusp top) (push (list :more top) out))
    (loop with group = nil
          for index from top below end
          for (id . label) = (nth index choices)
          for tail = (nth index tails)
          for heading = (and tail (< 1 (count tail tails :test #'equal)))
          do (when (and heading (not (equal tail group)))
               (setf group tail)
               (push (list :head tail) out))
             (push (list :choice (= cursor (1+ index)) (and (member id picked :test #'equal) t)
                         (gateway-setup-choice-name label) (and (not heading) tail) heading index)
                   out))
    (when (< end count) (push (list :more (- count end)) out))
    (nreverse out)))

(defun setup-pick-toggle (choices picked cursor)
  "PICKED with the CURSOR row of the checklist over CHOICES flipped: the all
row (0) ticks every one, or none when every one is ticked."
  (if (zerop cursor)
      (if (every (lambda (choice) (member (car choice) picked :test #'equal)) choices)
          '()
          (mapcar #'car choices))
      (let ((id (car (nth (1- cursor) choices))))
        (if (member id picked :test #'equal)
            (remove id picked :test #'equal)
            (cons id picked)))))

(defun gateway-setup-lane (call id &key (seconds 30))
  "ID's row of GET /api/channels once it is running and connected, or has
settled anywhere else, after at most SECONDS."
  (loop with deadline = (+ (get-universal-time) seconds)
        for row = (find id (json-array (funcall call :get "/api/channels" nil) "channels")
                        :key (lambda (row) (json-value row :string "id")) :test #'equal)
        for state = (and row (json-value row :string "state"))
        when (or (and (equal state "running") (json-value row :boolean "connected"))
                 (member state '("refused" "failed" "unconfigured" "stopped" "done") :test #'equal)
                 (>= (get-universal-time) deadline))
          return row
        do (sleep 1)))

(defstruct (channel-setup-sheet (:include sheet-dialog))
  "/gateway setup: CHANNEL, the id set up (NIL until picked from CELLS, the
channel cells on offer as ((ID . ROW) ...), AT the list's cursor); CALL,
(METHOD PATH BODY) => the organism's decoded answer; DONE, the folded steps
((TEXT . DONE-P) ...) oldest first; STEP, the open one; the SECTION as the
config route draws it and its PATH; the FIELDS still to ask, the FIELD
asked, its CHOICES, the DEFAULTS an unset one takes, the ids PICKED and the
CURSOR; INPUT typed or pasted; CHECK, the check's lines, and SUMMARY, its
one line; WAIT, what the platform said to do first, and WAITED once it was
done; ROUND, the pass over the fields; SAID, a line under the step, (TEXT
. TONE)."
  (channel nil) (cells '()) (at 0) (call nil) (done '()) (step :loading)
  (section nil) (path nil) (fields '()) (field nil) (choices '()) (defaults '()) (picked '())
  (cursor 0) (typed "" :type string) (check '()) (summary nil) (wait nil) (waited nil)
  (round 0) (said nil))

(nlk:access (setup channel-setup-sheet))

(defparameter *channel-setup-poll-seconds* 3
  "How long the sheet waits between asks while the platform has nothing to
pick from yet.")

(defparameter +channel-setup-size+ '(100 30)
  "The columns and rows the sheet asks for.")

;;; --- the steps ------------------------------------------------------------------

(defun channel-setup-say (setup text &optional (tone :danger))
  "Say TEXT under the open step, in TONE."
  (setf setup.said (cons text tone)))

(defun channel-setup-fold (setup text &optional (done t))
  "Fold the open step into one line: TEXT, a check before it when DONE."
  (setf setup.done (append setup.done (list (cons text done)))
        setup.said nil))

(defun channel-setup-ask (model setup thunk then saying)
  "Run THUNK off the loop, SAYING so on the key row, and THEN (MODEL
ANSWER) with what it answered; a refusal, or a call that failed, is said
under the step instead, in the organism's words."
  (sheet-call model setup
              (lambda ()
                (handler-case (funcall thunk)
                  (gateway-http-error (condition)
                    (list :error (or (ignore-errors (gateway-http-error-detail condition))
                                     (princ-to-string condition))))))
              (lambda (model answer)
                (if-let (failure (off-loop-failure answer))
                  (channel-setup-say setup failure)
                  (funcall then model answer)))
              saying))

(defun channel-setup-call (setup method path &optional body)
  "One call to the organism's control surface, through SETUP's CALL."
  (funcall setup.call method path body))

(defun channel-setup-label (setup)
  "The channel's name as the sheet says it."
  (if setup.channel (settings-label setup.channel) "a channel"))

(defun channel-setup-field-label (field)
  "FIELD's name as a folded line says it: allowed_channels is allowed channels."
  (substitute #\Space #\_ (json-value field :string "name")))

(defun channel-setup-id (named)
  "The channel NAMED names: discord for discord, channel-discord or nodecode-channel-discord."
  (string-downcase (reduce (lambda (word prefix)
                             (if (uiop:string-prefix-p prefix word) (subseq word (length prefix)) word))
                           '("nodecode-" "channel-") :initial-value named)))

(defun channel-setup-start (model setup)
  "Read the channel cells on offer, then set up the one named, or ask which."
  (channel-setup-ask
   model setup
   (lambda ()
     ;; Opened on a shell's first frame (`nodecode gateway setup'), the organism
     ;; may still be coming up -- no endpoint published (a 503), or one not
     ;; answering yet: its first read waits for it, up to a minute; any other
     ;; refusal is said at once.
     (loop with deadline = (+ (get-universal-time) 60)
           do (handler-case (return (channel-setup-call setup :get "/api/gateway/cells"))
                (gateway-http-error (condition)
                  (if (or (/= 503 (gateway-http-error-status condition)) (>= (get-universal-time) deadline))
                      (error condition)
                      (sleep 0.5)))
                (error (condition)
                  (if (>= (get-universal-time) deadline) (error condition) (sleep 0.5))))))
   (lambda (model answer)
     (setf setup.cells (loop for row across (or (json-array answer "cells") #())
                             for name = (json-value row :string "name")
                             when (and name (uiop:string-prefix-p "nodecode-channel-" name)
                                       (string/= name "nodecode-channel-kit"))
                               collect (cons (subseq name (length "nodecode-channel-")) row)))
     (cond ((null setup.cells)
            (setf setup.step :done)
            (channel-setup-say setup "No channel adapter is on offer here."))
           ((and setup.channel (assoc setup.channel setup.cells :test #'string=))
            (channel-setup-install model setup))
           (t
            (when setup.channel
              (channel-setup-say setup (format nil "No channel ~a here; pick one." setup.channel) :system)
              (setf setup.channel nil))
            (setf setup.step :channel setup.at 0))))
   "Reading the channels…"))

(defun channel-setup-install (model setup &aux (row (cdr (assoc setup.channel setup.cells :test #'string=)))
                                              (cell (format nil "nodecode-channel-~a" setup.channel)))
  "Switch the channel's cell on when it is off, then read its section."
  (if (json-value row :boolean "on")
      (channel-setup-read model setup)
      (progn
        (setf setup.step :loading)
        (channel-setup-ask
         model setup
         (lambda () (channel-setup-call setup :post "/api/gateway/cells"
                                        (json-object "name" cell "on" t "seconds" nlk:*cell-switch-seconds*)))
         (lambda (model answer)
           (channel-setup-fold setup (first-line (or (json-value answer :string "text") (format nil "~a is on" cell))))
           (channel-setup-read model setup))
         (format nil "Switching ~a on…" cell)))))

(defun channel-setup-read (model setup &optional then)
  "Read the channel's section off the config route, then THEN (MODEL SETUP),
or without one the token step (the fields, for a section with no secret)."
  (channel-setup-ask
   model setup
   (lambda () (channel-setup-call setup :get "/api/gateway/config"))
   (lambda (model answer)
     (let ((section (find (format nil "channels.~a" setup.channel) (or (json-array answer "sections") #())
                          :key (lambda (section) (json-value section :string "key")) :test #'equal)))
       (if (null section)
           (progn (setf setup.step :done)
                  (channel-setup-say setup (format nil "nodecode-channel-~a declares no channels.~a section to set up."
                                                   setup.channel setup.channel)))
           (progn (setf setup.section section
                        setup.path (json-value section :array "path"))
                  (cond (then (funcall then model setup))
                        ((json-value section :object "secret") (setf setup.step :token setup.typed ""))
                        (t (channel-setup-fields model setup)))))))
   "Reading the setup…"))

(defun channel-setup-secret (setup)
  "The section's secret pair as (NOUN ENV FILE SET), SET the member that
names the secret now, or NIL."
  (when-let (secret (json-value setup.section :object "secret"))
    (let ((env (json-value secret :string "env"))
          (file (json-value secret :string "file")))
      (list (or (json-value secret :string "noun") "secret") env file
            (loop for field across (or (json-array setup.section "fields") #())
                  for name = (json-value field :string "name")
                  when (and (member name (list env file) :test #'equal) (json-value field :boolean "set"))
                    return name)))))

(defun channel-setup-token (model setup &aux (text (string-trim '(#\Space #\Tab #\Return #\Newline) setup.typed)))
  "Keep what was pasted as the section's secret -- a token in its 0600
file, or $NAME, the environment variable that holds it -- then check it.
Nothing pasted keeps the one set."
  (destructuring-bind (noun env file set) (channel-setup-secret setup)
    (flet ((then-check (model answer)
             (declare (ignore answer))
             (setf setup.typed "")
             (channel-setup-read model setup #'channel-setup-check)))
      (cond ((string= text "")
             (if set
                 (channel-setup-check model setup)
                 (channel-setup-say setup (format nil "Paste the ~a first." noun) :system)))
            ((char= #\$ (char text 0))
             (let ((name (subseq text 1)))
               (cond ((equal set file)
                      (channel-setup-say setup (format nil "The ~a is kept in a file now: paste the ~a itself to replace it." noun noun) :system))
                     ((not (and (plusp (length name)) (every (lambda (char) (or (alphanumericp char) (char= char #\_))) name)))
                      (channel-setup-say setup (format nil "~s is not an environment variable's name." name)))
                     (t (channel-setup-ask model setup
                                           (lambda () (channel-setup-call setup :post "/api/gateway/config"
                                                                          (json-object "path" (concatenate 'vector setup.path (vector env))
                                                                                       "value" name)))
                                           #'then-check "Keeping it…")))))
            (t (channel-setup-ask model setup
                                  (lambda () (channel-setup-call setup :post "/api/gateway/config"
                                                                 (json-object "path" setup.path "secret" text)))
                                  #'then-check "Keeping it…"))))))

(defun channel-setup-check (model setup)
  "Ask the section's check what the token sees, and show it: Enter goes on
with that token. A section with no check goes straight to its fields."
  (if (not (json-value setup.section :boolean "check"))
      (progn (channel-setup-fold setup (format nil "~a kept" (first (channel-setup-secret setup))))
             (channel-setup-fields model setup))
      (channel-setup-ask
       model setup
       (lambda () (channel-setup-call setup :post "/api/gateway/config/probe" (json-object "path" setup.path)))
       (lambda (model answer)
         (declare (ignore model))
         (let ((text (or (json-value answer :string "text") "")))
           (setf setup.check (uiop:split-string text :separator '(#\Newline))
                 setup.summary (or (json-value answer :string "summary") (first-line text))
                 setup.step :check)))
       "Checking it…")))

(defun channel-setup-asked-fields (section)
  "The fields of SECTION the setup asks for, in its order: the essential
ones and the owner, the secret's own pair aside."
  (let ((pair (let ((secret (json-value section :object "secret")))
                (and secret (list (json-value secret :string "env") (json-value secret :string "file"))))))
    (loop for field across (or (json-array section "fields") #())
          for name = (json-value field :string "name")
          when (and (not (member name pair :test #'equal))
                    (or (json-value field :boolean "essential") (equal name "owner")))
            collect field)))

(defun channel-setup-fields (model setup)
  "Ask every field the section needs, over the section as it reads now."
  (incf setup.round)
  (channel-setup-read model setup
                      (lambda (model setup)
                        (setf setup.fields (channel-setup-asked-fields setup.section))
                        (channel-setup-next model setup))))

(defun channel-setup-next (model setup)
  "Ask the next field: from a checklist when the platform offers choices,
as typed ids otherwise; past the last one, see whether the section is
complete."
  (let ((field (pop setup.fields)))
    (setf setup.field field setup.typed "")
    (cond ((null field) (channel-setup-round-end model setup))
          ((json-value field :boolean "choices") (channel-setup-offer model setup))
          (t (setf setup.step :input)))))

(defun channel-setup-current (field)
  "The ids FIELD names now, a list field's value; NIL when it is unset."
  (and (json-value field :boolean "set")
       (coerce (or (json-array field "value") #()) 'list)))

(defun channel-setup-offer (model setup &key again)
  "Ask the platform what the open field may name. While it says to wait,
show what to do and ask AGAIN every *CHANNEL-SETUP-POLL-SECONDS*; once it
offers choices, the checklist, ticked as the field stands or with its
default."
  (let ((name (json-value setup.field :string "name"))
        (field setup.field))
    (channel-setup-ask
     model setup
     (lambda ()
       (when again (sleep *channel-setup-poll-seconds*))
       (channel-setup-call setup :post "/api/gateway/config/probe" (json-object "path" setup.path "field" name)))
     (lambda (model answer)
       (let ((choices (map 'list (lambda (choice) (cons (json-value choice :string "id")
                                                         (json-value choice :string "label")))
                           (or (json-array answer "choices") #())))
             (wait (json-value answer :string "wait")))
         (cond ((and (null choices) wait)
                (setf setup.wait wait setup.step :wait)
                (channel-setup-offer model setup :again t))
               (t
                (when (eq setup.step :wait)
                  (setf setup.waited t)
                  (channel-setup-fold setup (or (json-value answer :string "found") "there now")))
                (let ((defaults (coerce (or (json-array answer "default") #()) 'list)))
                  (setf setup.choices choices setup.defaults defaults setup.cursor 0
                        setup.picked (or (channel-setup-current field) defaults)
                        setup.step (if choices :pick :input)))))))
     (if again "Waiting…" "Asking…"))))

(defun channel-setup-write (model setup value summary)
  "Write VALUE to the open field, then fold it into SUMMARY and ask the next."
  (let ((name (json-value setup.field :string "name")))
    (channel-setup-ask
     model setup
     (lambda () (channel-setup-call setup :post "/api/gateway/config"
                                    (json-object "path" (concatenate 'vector setup.path (vector name)) "value" value)))
     (lambda (model answer)
       (declare (ignore answer))
       (channel-setup-fold setup summary)
       (channel-setup-next model setup))
     "Saving…")))

(defun channel-setup-keep (model setup)
  "Leave the open field as it stands, folded into what it says."
  (let* ((field setup.field)
         (now (and (json-value field :boolean "set") (json-value field :string "text"))))
    (channel-setup-fold setup (format nil "~a: ~a" (channel-setup-field-label field) (or now "none")) (and now t))
    (channel-setup-next model setup)))

(defun channel-setup-pick (model setup)
  "Write the ticked ids to the open field, in the choices' order, the ids
it named that the platform did not offer kept; nothing changed keeps it."
  (let* ((field setup.field)
         (offered (mapcar #'car setup.choices))
         (picks (append (remove-if-not (lambda (id) (member id setup.picked :test #'equal)) offered)
                        (remove-if (lambda (id) (member id offered :test #'equal)) setup.picked)))
         (current (channel-setup-current field)))
    (if (or (equal picks current) (and (null picks) (null current)))
        (channel-setup-keep model setup)
        (channel-setup-write model setup (coerce picks 'vector)
                             (format nil "~a: ~a" (channel-setup-field-label field)
                                     (if picks (gateway-setup-names picks setup.choices) "none"))))))

(defun channel-setup-typed (model setup &aux (text (string-trim '(#\Space #\Tab) setup.typed)))
  "Write what was typed to the open field: ids, comma-separated, for a list."
  (if (string= text "")
      (channel-setup-keep model setup)
      (let ((list (equal "list" (json-value setup.field :string "type"))))
        (channel-setup-write model setup
                             (if list (coerce (gateway-setup-picks text nil) 'vector) text)
                             (format nil "~a: ~a" (channel-setup-field-label setup.field) text)))))

(defun channel-setup-round-end (model setup)
  "Past the last field: a complete section starts its channel (after a wait,
with what the token sees now folded in first); one still needing something
is asked again, at most three passes."
  (channel-setup-read
   model setup
   (lambda (model setup)
     (let ((needs (json-value setup.section :string "needs")))
       (cond ((equal needs "complete")
              (if (and setup.waited (json-value setup.section :boolean "check"))
                  (channel-setup-ask
                   model setup
                   (lambda () (channel-setup-call setup :post "/api/gateway/config/probe" (json-object "path" setup.path)))
                   (lambda (model answer)
                     (channel-setup-fold setup (or (json-value answer :string "summary")
                                                   (first-line (or (json-value answer :string "text") ""))))
                     (channel-setup-launch model setup))
                   "Checking it again…")
                  (channel-setup-launch model setup)))
             ((< setup.round 3)
              (channel-setup-fold setup (format nil "~a still needs ~a" (channel-setup-label setup) needs) nil)
              (channel-setup-fields model setup))
             (t (channel-setup-fold setup (format nil "~a is not started: ~a" (channel-setup-label setup) needs) nil)
                (setf setup.step :done)))))))

(defun channel-setup-launch (model setup)
  "Start the channels again and wait for this one to say how it stands."
  (setf setup.step :start)
  (channel-setup-ask
   model setup
   (lambda ()
     (channel-setup-call setup :post "/api/channels?op=restart")
     (gateway-setup-lane setup.call setup.channel))
   (lambda (model lane)
     (declare (ignore model))
     (let ((state (and lane (json-value lane :string "state")))
           (detail (and lane (or (json-value lane :string "detail") (json-value lane :string "last_rejection")))))
       (if (and (equal state "running") (json-value lane :boolean "connected"))
           (channel-setup-fold setup (format nil "~a is connected~@[: ~a~]" setup.channel detail))
           (channel-setup-fold setup (format nil "~a did not connect: ~a~@[ - ~a~]" setup.channel (or state "no lane") detail) nil))
       (setf setup.step :done)))
   "Starting the channels…"))

(defun channel-setup-invite (setup)
  "The first link in what the platform said to do while waiting, or NIL."
  (when-let (wait setup.wait)
    (let ((start (search "https://" wait)))
      (and start (subseq wait start (position-if (lambda (char) (member char '(#\Space #\Newline #\Tab))) wait :start start))))))

(defun channel-setup-copy (model setup)
  "Put the invite link on the clipboard, and say how that went."
  (let ((link (channel-setup-invite setup))
        (program model.program))
    (cond ((null link))
          ((null program) (sheet-flash setup "No terminal to copy to" :danger))
          (t (nlt:write-clipboard program link)
             (sheet-flash setup "Link sent to the clipboard" :success)))))

(defun channel-setup-close (model setup)
  "Close the sheet: what was saved stays saved; a finished setup says how
it ended in the status line."
  (let ((last (car (last setup.done))))
    (dismiss-dialog model nil)
    (when (and last (eq setup.step :done))
      (show-status model (car last) :error-p (not (cdr last))))))

;;; --- the keys -------------------------------------------------------------------

(defun channel-setup-key (model setup key &key ctrl alt shift)
  "Every key the shell's chords leave the open sheet, by its step."
  (declare (ignore shift))
  (let ((busy setup.busy))
    (if (eq key :escape)
        (case setup.step
          ;; Esc on a field keeps it as it stands; anywhere else it closes.
          ((:pick :input) (unless busy (channel-setup-keep model setup)))
          (:check (setf setup.step :token setup.typed ""))
          (t (channel-setup-close model setup)))
        (case setup.step
          (:channel
           (cond ((member key '(:up :down))
                  (setf setup.at (max 0 (min (1- (length setup.cells)) (+ setup.at (if (eq key :up) -1 1))))))
                 ((and (eq key :enter) (not busy) setup.cells)
                  (setf setup.channel (car (nth setup.at setup.cells)) setup.said nil)
                  (channel-setup-fold setup (format nil "channel: ~a" setup.channel))
                  (channel-setup-install model setup))))
          ((:token :input)
           (cond ((eq key :enter)
                  (unless busy
                    (if (eq setup.step :token) (channel-setup-token model setup) (channel-setup-typed model setup))))
                 (t (when-let (edited (edit-text-field setup.typed key :ctrl ctrl :alt alt))
                      (setf setup.typed edited)))))
          (:check
           (cond ((and (eq key :enter) (not busy))
                  (channel-setup-fold setup (or setup.summary "token kept"))
                  (channel-setup-fields model setup))
                 ((eql key #\e) (setf setup.step :token setup.typed ""))))
          (:wait (when (eql key #\c) (channel-setup-copy model setup)))
          (:pick
           (let ((count (length setup.choices)))
             (cond ((or (eq key :up) (eql key #\k)) (setf setup.cursor (max 0 (1- setup.cursor))))
                   ((or (eq key :down) (eql key #\j)) (setf setup.cursor (min count (1+ setup.cursor))))
                   ((member key '(:page-up :page-down))
                    (setf setup.cursor (max 0 (min count (+ setup.cursor (if (eq key :page-up) -10 10))))))
                   ((eql key #\Space) (setf setup.picked (setup-pick-toggle setup.choices setup.picked setup.cursor)))
                   ((member key '(#\a #\A)) (setf setup.picked (setup-pick-toggle setup.choices setup.picked 0)))
                   ((and (eq key :enter) (not busy)) (channel-setup-pick model setup)))))
          ((:done :loading :start)
           (when (and (eq key :enter) (eq setup.step :done)) (channel-setup-close model setup)))))
    t))

(defun channel-setup-paste (model setup text)
  "A paste lands in the open field, one line of it."
  (declare (ignore model))
  (when (member setup.step '(:token :input))
    (setf setup.typed (concatenate 'string setup.typed
                                   (remove-if (lambda (char) (member char '(#\Return #\Newline))) text)))))

;;; --- painting -------------------------------------------------------------------

(defun channel-setup-wrap (text width &key (indent 0))
  "TEXT as lines at most WIDTH cells wide, broken at spaces, a word longer
than a line (a link) cut where it must; lines after the first INDENT cells in."
  (let ((lines '()) (line ""))
    (flet ((space-left () (- width (if lines indent 0))))
      (dolist (piece (uiop:split-string text :separator '(#\Space)))
        (let ((word piece))
          (loop while (> (text-display-width word) (space-left))
                do (let ((cut (if (string= line "") (space-left) 0)))
                     (if (zerop cut)
                         (progn (push line lines) (setf line ""))
                         (let ((head (take-display-prefix word cut)))
                           (push head lines)
                           (setf word (subseq word (length head)))))))
          (cond ((string= line "") (setf line word))
                ((<= (+ (text-display-width line) 1 (text-display-width word)) (space-left))
                 (setf line (concatenate 'string line " " word)))
                (t (push line lines) (setf line word)))))
      (push line lines))
    (let ((lines (nreverse lines)))
      (cons (first lines)
            (mapcar (lambda (line) (concatenate 'string (make-string indent :initial-element #\Space) line))
                    (rest lines))))))

(defun channel-setup-keys (setup)
  "The key row of SETUP's open step: (KEY WORD) ..."
  (case setup.step
    (:channel '(("↑↓" "move") ("enter" "set it up") ("esc" "close")))
    (:token '(("enter" "check it") ("esc" "close")))
    (:check '(("enter" "go on") ("e" "another token") ("esc" "close")))
    (:wait (if (channel-setup-invite setup) '(("c" "copy the link") ("esc" "close")) '(("esc" "close"))))
    (:pick '(("↑↓" "move") ("space" "pick") ("a" "all") ("enter" "save") ("esc" "keep it")))
    (:input '(("enter" "save") ("esc" "keep it")))
    (:done '(("enter" "close")))
    (t '(("esc" "close")))))

(defun paint-channel-setup-field (canvas x row width text placeholder ground &key masked)
  "An input field WIDTH wide from X on ROW: TEXT (dots when MASKED), else
PLACEHOLDER quiet. => the caret's cell."
  (let ((field (sheet-ground :composer)))
    (canvas-fill canvas x row width 1 (sheet-ink nil :ground field))
    (if (plusp (length text))
        (let ((shown (if masked (make-string (min (length text) (- width 3)) :initial-element #\•) (clip-tail text (- width 3)))))
          (cons (canvas-put canvas (1+ x) row shown (sheet-ink :composer :ground field)) row))
        (progn (canvas-put canvas (1+ x) row (clip-text-to-width placeholder (- width 3)) (sheet-ink :muted :ground field))
               (cons (1+ x) row)))))

(defun paint-channel-setup-list (setup canvas row bottom px pw ground)
  "The open field's checklist from ROW, no lower than BOTTOM: a click on a
row ticks it."
  (let ((rows (setup-pick-rows setup.choices setup.picked setup.cursor
                               :rows (max 3 (- bottom row 3)))))
    (flet ((box (pointed ticked indent text after cursor)
             (let ((at (+ px indent)))
               (when pointed (canvas-put canvas (- at 2) row "›" (sheet-ink :own-model :ground ground :bold t)))
               (setf at (canvas-put canvas at row (if ticked "[x]" "[ ]") (sheet-ink (if ticked :success :muted) :ground ground :bold ticked)))
               (setf at (canvas-put canvas (1+ at) row text (sheet-ink (if pointed :foreground :system) :ground ground :bold pointed) (+ px pw)))
               (when after (canvas-put canvas (+ at 2) row after (sheet-ink :muted :ground ground) (+ px pw)))
               (let ((cursor cursor))
                 (sheet-hit setup row px (+ px pw)
                            (lambda (model setup)
                              (declare (ignore model))
                              (setf setup.cursor cursor
                                    setup.picked (setup-pick-toggle setup.choices setup.picked cursor))
                              t))))))
      (loop for (kind . data) in rows
            while (< row bottom)
            do (ecase kind
                 (:all (destructuring-bind (pointed ticked count) data
                         (box pointed ticked 2 (format nil "all ~d" count) nil 0)))
                 (:head (canvas-put canvas (+ px 2) row (first data) (sheet-ink :muted :ground ground) (+ px pw)))
                 (:more (canvas-put canvas (+ px 4) row (format nil "… ~d more" (first data)) (sheet-ink :muted :ground ground)))
                 (:choice (destructuring-bind (pointed ticked name after nested index) data
                            (box pointed ticked (if nested 4 2) name after (1+ index)))))
               (incf row)))
    row))

(defun paint-channel-setup-step (setup canvas row bottom px pw ground)
  "The open step from ROW, no lower than BOTTOM. => the caret's cell, or NIL."
  (labels ((say (text &key (tone :system) (indent 0) bold)
             (dolist (line (channel-setup-wrap text (- pw indent) :indent 0))
               (when (< row bottom)
                 (canvas-put canvas (+ px indent) row line (sheet-ink tone :ground ground :bold bold) (+ px pw))
                 (incf row))))
           (gap () (incf row)))
    (case setup.step
      (:loading (say "…" :tone :muted) nil)
      (:channel
       (say "Which channel?" :tone :foreground :bold t)
       (gap)
       (loop for (id . cell) in setup.cells
             for index from 0
             do (when (< row bottom)
                  (let ((pointed (= index setup.at)))
                    (when pointed (canvas-put canvas px row "›" (sheet-ink :own-model :ground ground :bold t)))
                    (let ((at (canvas-put canvas (+ px 2) row (settings-label id) (sheet-ink (if pointed :foreground :system) :ground ground :bold pointed))))
                      (canvas-put canvas (+ at 2) row (if (json-value cell :boolean "on") "on" "not installed") (sheet-ink :muted :ground ground)))
                    (incf row))))
       nil)
      (:token
       (destructuring-bind (noun env file set) (channel-setup-secret setup)
         (declare (ignore env file))
         (let ((steps (loop for step in (uiop:split-string (or (json-value setup.section :string "guide") "") :separator '(#\;))
                            for words = (string-trim '(#\Space) step)
                            unless (string= words "") collect words)))
           (when steps
             (say (format nil "On ~a's side first:" (channel-setup-label setup)) :tone :foreground :bold t)
             (loop for step in steps
                   for number from 1
                   do (let ((lines (channel-setup-wrap step (- pw 5))))
                        (when (< row bottom)
                          (canvas-put canvas (+ px 1) row (format nil "~d" number) (sheet-ink :own-model :ground ground :bold t)))
                        (dolist (line lines)
                          (when (< row bottom)
                            (canvas-put canvas (+ px 4) row line (sheet-ink :system :ground ground) (+ px pw))
                            (incf row)))))
             (gap))
           (say (string-capitalize noun :end 1) :tone :foreground :bold t)
           (prog1 (paint-channel-setup-field canvas px row (min pw 72) setup.typed
                                             (if set
                                                 (format nil "set: Enter keeps it, or paste a new one")
                                                 (format nil "paste it here, or $NAME; it is never shown"))
                                             ground :masked t)
             (incf row)))))
      (:check
       (say "What the token sees:" :tone :foreground :bold t)
       (dolist (line setup.check) (say line :indent 2))
       nil)
      (:wait
       (say (substitute #\Space #\Newline (or setup.wait "")) :tone :foreground)
       (gap)
       (say (format nil "Waiting: asking again every ~d s." *channel-setup-poll-seconds*) :tone :muted)
       nil)
      (:pick
       (say (format nil "~a: ~a" (channel-setup-field-label setup.field) (or (json-value setup.field :string "doc") ""))
            :tone :foreground :bold t)
       (setf row (paint-channel-setup-list setup canvas row bottom px pw ground))
       nil)
      (:input
       (say (format nil "~a: ~a" (channel-setup-field-label setup.field) (or (json-value setup.field :string "doc") ""))
            :tone :foreground :bold t)
       (prog1 (paint-channel-setup-field canvas px row (min pw 72) setup.typed
                                         (if (equal "list" (json-value setup.field :string "type"))
                                             "ids, comma-separated; Enter keeps it"
                                             "Enter keeps it")
                                         ground)
         (incf row)))
      (:start (say "Starting the channels…" :tone :muted) nil)
      (:done
       (when setup.channel
         (say (format nil "Change it any time: /gateway setup ~a, or /settings › Channels." setup.channel) :tone :muted))
       nil))))

(defun paint-channel-setup (setup canvas edges)
  "The sheet: its title, a line for each finished step, the open step
whole, what it said, and its keys. => the caret's cell, or NIL."
  (with-stacked-sheet (canvas edges)
    (let* ((ground pane)
           (row header-row)
           (bottom (- hint-row 2)))
      (canvas-put canvas px row (format nil "Set up ~a" (channel-setup-label setup))
                  (sheet-ink :foreground :ground ground :bold t))
      (incf row 2)
      ;; The newest folded lines that leave the open step room.
      (let ((room (max 1 (- bottom row 8))))
        (dolist (line (last setup.done room))
          (let ((at (canvas-put canvas px row (if (cdr line) "✓" "-")
                                (sheet-ink (if (cdr line) :success :muted) :ground ground :bold t))))
            (canvas-put canvas (1+ at) row (clip-text-to-width (car line) (- (+ px pw) at 1))
                        (sheet-ink (if (cdr line) :system :muted) :ground ground) (+ px pw)))
          (incf row)))
      (when setup.done (incf row))
      (let ((caret (paint-channel-setup-step setup canvas row (if setup.said (1- bottom) bottom) px pw ground)))
        (when setup.said
          (canvas-put canvas px (1- bottom) (clip-text-to-width (car setup.said) pw)
                      (sheet-ink (cdr setup.said) :ground ground)))
        (paint-keys setup canvas hint-row px (+ px pw) (channel-setup-keys setup) setup.flash ground)
        caret))))

(defun channel-setup-paint (model setup width height)
  "The sheet for a WIDTH x HEIGHT frame: one pane, wide or narrow."
  (paint-sheet model setup width height +channel-setup-size+ #'paint-channel-setup #'paint-channel-setup))

;;; --- opening it -----------------------------------------------------------------

(defun open-channel-setup (model &optional channel &key call)
  "/gateway setup [CHANNEL]: the sheet at once over the shell,
its first read off the loop. CALL stands in for the organism's control
surface (a test's fake). => NIL."
  (let ((setup (make-channel-setup-sheet
                :kind :channel-setup :title "Set up"
                :paint #'channel-setup-paint :key #'channel-setup-key
                :paste #'channel-setup-paste :click #'sheet-click
                :channel (and channel (channel-setup-id channel))
                :call (or call
                          (lambda (method path body)
                            (gateway-model-http-call model method path (and body (encode-json-object body))
                                                     :read-timeout (if (equal path "/api/gateway/cells")
                                                                       nlk:*cell-switch-seconds*
                                                                       60)))))))
    (setf (chat-model-dialog model) setup)
    (channel-setup-start model setup)
    nil))
