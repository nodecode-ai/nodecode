;;;; composer-test.lisp --- U2 gates: multiline composer + prompt history.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. Alt+Enter / Ctrl+J insert a newline; the box grows and the
;;;;      transcript viewport yields exactly the extra rows; the frame keeps
;;;;      its height.
;;;;   2. Plain Enter still submits (and never inserts a newline).
;;;;   3. Up at the end of the text recalls newest-first; repeated Up walks
;;;;      older; Down walks back and finally restores the stashed draft.
;;;;   4. The draft prefix filters which entries the walk visits.
;;;;   5. Editing ends a history walk.
;;;;   6. Up away from the text end never recalls (cursor keys keep editing).
;;;;   7. Submitted prompts persist newest-first across models via the
;;;;      history file; the runner default (NIL path) disables persistence.
;;;;   10. Text longer than one row wraps at the composer's own box width;
;;;;      the box grows, no frame row spills, and the caret stays inside
;;;;      the box (Up/Down walk visual rows, not the logical line).
;;;;   11. The $/@ catalogs are walked on a thread of their own: a
;;;;      trigger answers from whatever has landed and never waits on the
;;;;      walk, one walk lands once through the results table, and a walk
;;;;      that failed stays unloaded and names itself in the notice lane.
;;;;   12. The walk's answer is kept across processes: the boot
;;;;      installs the cached catalogs before the first frame, so the first
;;;;      `$' paints rows without waiting on the walk at all.
;;;;
;;;; Loads after ui-test: reuses MAKE-FRAME-MODEL.

(in-package #:nodecode.test)

(nlk:access (cursor nlt::cursor))

(defun press-wire (model &rest bytes)
  "Press the key a terminal sends as BYTES, decoded by the reader."
  (nle:update model (apply #'key-from bytes))
  model)

(defun is-composer (model expected why)
  "Assert MODEL's composer holds EXPECTED, under WHY."
  (is (equal expected (composer-value model)) why))

(defmacro with-parked-cursor ((model &optional (row (gensym "ROW"))) &body body)
  "Compose MODEL's frame, then run BODY with PARK the cursor it parked and
ROW, when named, the frame row it parked on."
  (let ((rows (gensym "ROWS")))
    `(let* ((,rows (nle::compose-frame ,model))
            (park (nle::chat-model-parked-cursor ,model))
            (,row (and (consp park) (nth (cdr park) ,rows))))
       (declare (ignorable ,row))
       ,@body)))

(defun expire-status-news (model)
  "Age MODEL's composer status past a news line's life, then tick as the clock does."
  (when model.composer-status
    (decf (nle::composer-status-born model.composer-status) (* 6 internal-time-units-per-second)))
  (nle:update model (nlt:make-tick-msg)))

(defun is-news-in-composer (model width status &optional (above status))
  "Assert STATUS stands as news in MODEL's composer row at WIDTH and no row above it carries
ABOVE, then expires into the placeholder with the band as it was."
  (is (equal status (nle::status-said model)))
  (is (search (nle::clip-text-to-width status (nle::composer-text-width width))
              (nth 19 (nle::compose-frame model))))
  (is (not (rows-carry-p above (subseq (nle::compose-frame model) 0 18))))
  (let ((lines (copy-list (nle::band-window-lines model.band))))
    (expire-status-news model)
    (nlt:view model)
    (is (null model.composer-status))
    (is (search "Ready" (nth 19 (nle::compose-frame model))))
    (is (equal lines (nle::band-window-lines model.band)))))

(defun open-openai-connect (model)
  "Open the /models wizard on MODEL over a one-provider list, openai."
  (nle::open-connect-dialog model (lambda () (picker-rows '("openai" :value "openai"))) nil nil))

;;; --- claim 1: newline keys grow the box, the band yields ------------------

(deftest composer-newline-grows-the-box (with-frame-model (model 80 30 :session-id "composer-grow"))
  (type-string model "first")
  ;; The wire bytes, not hand-built events: Alt+Enter is ESC CR and
  ;; Ctrl+J is a bare LF. Both used to decode into something else
  ;; entirely (a lone Escape, and a plain Enter that submitted).
  (press-wire model 27 13)
  (type-string model "second")
  (is-composer model (format nil "first~%second") "Alt+Enter inserts a newline")
  (press-wire model 10)
  (type-string model "third")
  (is-composer model (format nil "first~%second~%third") "Ctrl+J inserts a newline")
  (is (null model.turn-in-flight))
  (let ((rows (nle::compose-frame model)))
    (is (= 30 (length rows)))
    ;; 30-row stack with a 3-row composer: chrome 0-2, band 3-20,
    ;; gap 21, pad 22, text 23-25, pad 26, gap 27, footer 28, blank 29.
    (is (search "first" (nth 23 rows)))
    (is (search "third" (nth 25 rows))))
  (is (= 3 (nle::composer-visible-rows model)))
  (is (= (- 30 (nle::shell-overhead 30 3))
         (nle::band-window-height model.band))))

;;; --- claim 2: plain Enter submits ------------------------------------------

(deftests composer (session-id key)
  ;; The shell has a link: one that cannot carry the prompt is refused
  ;; before any send.
  (with-recording-link (link)
    (with-frame-model (model 80 24 :session-id session-id :ws link :typed "hello organism")
      key
      (is (nle::chat-model-turn-in-flight model))
      (is (equal '("start_turn") (link-commands link)) "the start left over the link")
      (is-composer model "" "submit resets the composer")))
  (plain-enter-still-submits "composer-submit" (press model :enter))
  ;; The same claim through the bytes a terminal actually sends. The keymap
  ;; binds "enter" and "tab" bare, the way the Zig TUI's decoder reports
  ;; them, and the core's reader reports them bare too. When the two once
  ;; drifted apart (a reader tagging every C0 byte with ctrl), Enter matched
  ;; no action at all and the composer swallowed it — green suites and a
  ;; dead composer.
  (carriage-return-submits "composer-cr" (press-wire model 13)))

(deftest composer-horizontal-tab-accepts-the-suggestion ()
  (with-frame-model (model 80 24 :session-id "composer-ht" :typed "/th")
    (press-wire model 9)
    (is-composer model "/themes " "a bare HT completes the ghost")
    (is (null model.turn-in-flight))))

;;; The decoder contract itself, at the byte level. Every keymap assertion
;;; in this suite is only worth what the reader's decode is worth: a
;;; decoder that reports two different keys under one code makes the
;;; keymap unable to tell them apart, whatever the chord table says.

(deftest newline-and-submit-bytes-decode-as-distinct-keys (let ((keymap (nle::resolve-keybinds '()))
                                                                (lf (key-from 10))
                                                                (cr (key-from 13))
                                                                (alt-enter (key-from 27 13))))
  (flet ((matches (action msg)
           (nle::keybind-matches-p keymap action (nlt:key-event-code msg) (nlt:key-event-mod msg))))
    ;; Raw LF is the Ctrl+J chord: in raw mode ICRNL is off, so Enter
    ;; sends CR and nothing else produces LF (vaxis parseGround decodes
    ;; 0x0a to `j` with ctrl for the same reason).
    (is (matches :insert-newline lf))
    (is (not (matches :submit lf)))
    ;; CR is Enter with no modifier: submit, not a newline.
    (is (matches :submit cr))
    (is (not (matches :insert-newline cr)))
    ;; ESC CR is the legacy Alt+Enter encoding. Decoding it as an
    ;; unknown escape sequence turned the newline chord into a cancel.
    (is (matches :insert-newline alt-enter))
    ;; Shift+Enter, the first insert-newline default, has no legacy
    ;; encoding at all: a terminal sends a bare CR for it unless the
    ;; kitty keyboard protocol is on, where it arrives as CSI 13;2u.
    (is-let* ((shift-enter (key-from 27 91 49 51 59 50 117)))
      ((matches :insert-newline shift-enter) is) ((matches :submit shift-enter) not))))

(deftest shell-view-asks-for-kitty-key-disambiguation ()
  (with-frame-model (model 80 24 :session-id "composer-kitty-keys")
    ;; Without this the terminal never sends CSI 13;2u and the default
    ;; Shift+Enter binding is dead on every surface.
    (is (nlt:view-state-keyboard-enhancements (nle::shell-view model)))))

;;; --- the readline editing chords -------------------------------------------
;;;
;;; The Zig composer answers the unix editing set (runtime.zig
;;; isCursorStartKey/isCursorLeftKey/isDeleteAtCursorKey/...); the lisp
;;; composer routes the same chords through the editor.

(deftest composer-answers-the-readline-editing-chords ()
  (with-frame-model (model 80 24 :session-id "composer-readline" :typed "alpha beta")
    (press-wire model 1)                    ; Ctrl+A: line start
    (press-wire model 6)                    ; Ctrl+F: one char right
    (press-wire model 4)                    ; Ctrl+D: delete at cursor
    (is-composer model "apha beta" "Ctrl+A/Ctrl+F/Ctrl+D move and delete like readline")
    (press-wire model 5)                    ; Ctrl+E: line end
    (press-wire model 2)                    ; Ctrl+B: one char left
    (press-wire model 127)                  ; DEL: delete before cursor
    (is-composer model "apha bea" "Ctrl+E/Ctrl+B walk to the end and back")
    (press-wire model 5)                    ; Ctrl+E: line end
    (press-wire model 21)                   ; Ctrl+U: kill to line start
    (is-composer model "" "Ctrl+U clears the line")
    (type-string model "one two")
    (press-wire model 23)                   ; Ctrl+W: kill word back
    (is-composer model "one " "Ctrl+W deletes the word before the cursor")
    (press-wire model 1)
    (press-wire model 11)                   ; Ctrl+K: kill to line end
    (is-composer model "" "Ctrl+K clears from the cursor to the end")
    ;; An unbound ctrl chord is a gesture, not text: it must never type
    ;; its base letter into the box.
    (press-wire model 7)                    ; Ctrl+G: unbound
    (is-composer model "" "an unbound ctrl chord types nothing")))

(deftest ctrl-d-exits-only-on-an-empty-composer ()
  (with-frame-model (model 80 24 :session-id "composer-ctrl-d" :typed "draft")
    (press-wire model 1)                    ; Ctrl+A: cursor to line start
    (let ((cmd (nth-value 1 (nle:update model (key-from 4)))))
      (is (null cmd) "Ctrl+D with text in the box never quits"))
    (is-composer model "raft" "Ctrl+D with text deletes at the cursor instead")
    (nle::editor-reset model.editor)
    (let ((cmd (nth-value 1 (nle:update model (key-from 4)))))
      (is (and cmd (nlt:quit-msg-p (funcall cmd)))))))

;;; --- claims 3-6: history navigation ----------------------------------------

(deftests composer-history (session-id prompts draft steps)
  ;; Each case is the prompts submitted before, the draft typed, and the walk:
  ;; a key, the composer text it leaves, why, and whether a walk is then active.
  (with-frame-model (model 80 24 :session-id session-id)
    (dolist (prompt prompts) (nle::note-submitted-prompt model prompt))
    (type-string model draft)
    (loop for (key expected why walking) in steps
          do (press model key)
             (is-composer model expected why)
             (when walking (is model.history-index "the walk is active")))
    (is (null model.history-index)))
  (recalls-newest-first "composer-history" '("oldest prompt" "newest prompt") ""
   '((:up "newest prompt" "Up recalls the newest entry")
     (:up "oldest prompt" "Up again walks older")
     (:up "oldest prompt" "Up past the oldest entry holds")
     (:down "newest prompt" "Down walks newer")
     (:down "" "Down past the newest match restores the empty draft")))
  (prefix-filters-the-walk "composer-prefix" '("help me" "other thing") "he"
   '((:up "help me" "the draft prefix filters which entries the walk visits")
     (:down "he" "Down restores the typed draft")))
  (editing-ends-the-walk "composer-edit-reset" '("recalled entry") ""
   '((:up "recalled entry" "Up recalls the entry" t)
     (#\! "recalled entry!" "the edit lands on the recalled text")))
  (requires-cursor-at-end "composer-mid" '("an entry") "draft"
   '((:left "draft" "Left moves the caret off the text end")
     (:up "draft" "Up away from the text end never recalls"))))

;;; --- claim 7: persistence ---------------------------------------------------

(deftest composer-history-persists-across-models (with-temp-store ())
  (with-temp-file (nle::*composer-history-path* :type "history")
    (let ((first-model (make-frame-model
                        80 24 :session-id "composer-persist-a")))
      (nle::note-submitted-prompt first-model "durable one")
      (nle::note-submitted-prompt first-model "durable two"))
    (let ((second-model (make-frame-model
                         80 24 :session-id "composer-persist-b")))
      (press second-model :up)
      (is-composer second-model "durable two" "a fresh model recalls the persisted newest entry")
      (press second-model :up)
      (is-composer second-model "durable one" "and the persisted older entry"))))

;;; --- ghost text, $/@ triggers, and paste (input/chrome ladder) -------------

(deftest composer-ghost-completes-the-top-slash-match ()
  (with-frame-model (model 80 24 :session-id "ghost" :typed "/th")
    (is (equal "emes" (nle::composer-ghost-remainder model)))
    ;; "/th" matches two commands, so the dropdown panel below the
    ;; composer takes three rows and the composer text row sits at 16.
    (is (search "emes" (nth 16 (nle::compose-frame model))))
    (press model :left)
    (is (null (nle::composer-ghost-remainder model)))
    (press model :right :tab)
    (is-composer model "/themes " "the accept gesture completes the ghost")
    (is (null (nle::composer-ghost-remainder model)))))

(deftest composer-file-trigger-completes-workspace-paths ()
  (with-temp-workspace (dir "README.md" "src/main.lisp" ".hidden"
                            "node_modules/x.js")
    (with-frame-model (model 80 24 :session-id "at-files")
      (is (equal '("README.md" "src/main.lisp") (nle::workspace-file-catalog dir)))
      (setf model.file-catalog (nle::workspace-file-catalog dir) model.file-catalog-loaded t)
      (type-string model "@sr")
      (let ((suggestions (nle::token-suggestions-for model)))
        (is (= 1 (length suggestions)) "the @ prefix filters paths")
        (is (equal "src/main.lisp"
                   (nle::composer-suggestion-text (first suggestions)))))
      (press model :tab)
      (is-composer model "@src/main.lisp " "Tab completes the path plus a trailing space")
      (type-string model "@RE" :enter)
      (is-composer model "@src/main.lisp @README.md " "Enter over the dropdown completes instead of submitting")
      (is (null model.turn-in-flight)))))

(deftest skill-frontmatter-reads-a-block-scalar-description ()
  ;; Another harness writes long descriptions as YAML block scalars; the
  ;; skills and import cells read one parser, and it must answer the
  ;; joined lines, never the bare `|' — which once listed a skill as
  ;; `computer-use — |'.
  (multiple-value-bind (name description)
      (nle::parse-skill-frontmatter
       (format nil "---~%name: computer-use~%description: |~%  Drive the desktop in the background,~%  clicking and typing.~%~%  Never steal focus.~%platforms: [linux]~%---~%body~%"))
    (is (equal "computer-use" name))
    (is (equal "Drive the desktop in the background, clicking and typing. Never steal focus." description)))
  (is-values (name description)
      (nle::parse-skill-frontmatter
       (format nil "---~%name: folded~%description: >-~%  one~%  two~%---~%"))
    (name "folded") (description "one two" "a folded scalar with a chomping sign too"))
  (is (equal "plain" (nth-value 1 (nle::parse-skill-frontmatter
                                    (format nil "---~%name: x~%description: plain~%---~%"))))))

(deftest composer-frontmatter-is-the-block-between-its-fences ()
  ;; Read as far as the closer, never past it; a fence is `---' trimmed.
  (flet ((fenced (&rest lines) (car (nle::frontmatter-block (format nil "~{~a~^~%~}" lines)))))
    (is (equal '("name: a" "") (fenced "---" "name: a" "" "---" "body" "---")))
    (is (equal '("name: a") (fenced (format nil "---~c" #\Return) (format nil "name: a~c" #\Return) " --- ")))
    (is (null (fenced "---" "---")) "an empty block")
    (is (null (fenced "---" "name: a" "")) "no closer")
    (is (null (fenced "name: a" "---" "b" "---")) "no opener")
    (is (null (fenced "---")))
    (is (null (nle::frontmatter-block "")))
    (is (null (nle::frontmatter-block nil)))))

(deftest composer-paste-collapses-past-the-bounds ()
  (with-frame-model (model 80 24 :session-id "paste")
    (nle:update model (nlt:make-paste-msg :text "small paste"))
    (is-composer model "small paste" "a small paste inserts verbatim")
    (is (null model.pastes) "nothing is stashed")
    (nle::editor-reset model.editor)
    (nle:update model (nlt:make-paste-msg
                       :text (format nil "one~ctwo~%three" #\Return)))
    (is-composer model (format nil "one~%two~%three") "CR and CRLF normalize to LF")
    (nle::editor-reset model.editor)
    (let ((big (make-string 1500 :initial-element #\x)))
      (nle:update model (nlt:make-paste-msg :text big))
      (is-composer model "[Paste #1] " "a paste past 1000 chars collapses to the marker")
      (is (equal big (cdr (first model.pastes))))
      (type-string model "explain")
      (is (equal (concatenate 'string big " explain")
                 (nle::expand-composer-pastes
                  model (composer-value model)))))
    (nle::editor-reset model.editor)
    (nle::clear-composer-attachments model)
    (nle:update model (nlt:make-paste-msg
                       :text (format nil "~{line ~d~^~%~}"
                                     (list 1 2 3 4 5 6 7 8 9 10 11 12))))
    (is-composer model "[Paste #2] " "a paste past 10 lines collapses too; the counter advances")))

;;; A dialog's field takes a paste the way it takes typing. The dialog branch
;;; used to drop every paste, so the setup wizard's "Paste the provider API
;;; key." step ignored the terminal's own paste — over ssh the one paste that
;;; reaches this process at all — and a key could only be typed out by hand.

(defun paste-wire (model text)
  "Paste TEXT as a terminal in bracketed-paste mode sends it, decoded by the
reader: ESC [ 200 ~, the text, ESC [ 201 ~."
  (nle:update model (key-from-string (concatenate 'string (csi "200~") text (csi "201~")))))

(deftest dialog-paste-lands-where-typing-would ()
  (with-frame-model (model 80 30 :session-id "dialog-paste")
    (open-openai-connect model)
    (let ((dialog model.dialog))
      (paste-wire model "open")
      (is (equal "open" dialog.search))
      (nle::connect-enter-step dialog :api-key)
      (paste-wire model (format nil "  sk-live-123~%"))
      (is-shape dialog (.input "sk-live-123") (nle::connect-dialog-step eq :api-key))
      (setf dialog.input "")
      (paste-wire model (format nil "sk-~%live~c~%456" #\Return))
      (is (equal "sk-live456" dialog.input))
      (nle::connect-enter-step dialog :result)
      (paste-wire model "zzz")
      (is (equal "open" dialog.search)))
    (is-composer model "" "the composer under the dialog never saw a paste")))

(deftest dialog-paste-never-walks-the-key-ladder ()
  (with-frame-model (model 80 30 :session-id "dialog-paste-list")
    (let ((picked nil))
      (nle::open-picker-dialog
       model "Profile"
       (picker-rows '("Minimal" :value :minimal) '("Everything" :value :everything))
       (lambda (model value) (declare (ignore model)) (setf picked value) nil))
      (let ((dialog model.dialog))
        (paste-wire model (format nil "x~%"))
        (is (eq dialog model.dialog))
        (is (null picked) "the pasted newline selected nothing")
        (is (equal "" dialog.search))))
    (is-composer model "" "nor does the composer")))

(deftest dialog-ctrl-v-pastes-the-clipboard-text ()
  (with-frame-model (model 80 30 :session-id "dialog-clipboard")
    (open-openai-connect model)
    (let ((dialog model.dialog))
      (nle::connect-enter-step dialog :api-key)
      #-win32
      (let ((nle::*clipboard-paste-commands* '(("printf" "sk-from-clipboard\\n"))))
        (press-wire model 22)
        (is (equal "sk-from-clipboard" (nle::picker-dialog-input dialog))))
      (setf dialog.input "")
      (let ((nle::*clipboard-paste-commands* '()))
        (press-wire model 22)
        (is (equal "" dialog.input))
        (is (row-carrying "paste with your terminal (Ctrl+Shift+V)" (nle::compose-frame model)))))))

;;; An image paste on Windows. Windows Terminal keeps Ctrl+V and Ctrl+Shift+V
;;; for its own paste, and with nothing but an image on the clipboard that
;;; paste is empty: ESC [ 200 ~ then ESC [ 201 ~, nothing between. Ctrl+Alt+V
;;; comes through as ESC ^V, and the console host passes Ctrl+V on as ^V. Each
;;; asks the clipboard for its image -- which Windows answers through
;;; PowerShell, the one reader every Windows box has.

(deftest an-empty-terminal-paste-stages-the-clipboard-image ()
  (with-frame-model (model 80 30 :session-id "empty-paste-image")
    (with-stubbed-fdefinition (nle::read-clipboard-image-base64 ()
                                (values "image/png" "iVBORw0KGgo="))
      (paste-wire model "")
      (is-composer model "[Image #1] " "the empty paste staged the clipboard's image")
      (is (equal '(("image/png" . "iVBORw0KGgo=")) (nle::chat-model-pending-images model)) "the image rides the next submit")
      (press-wire model 27 22)
      (is-composer model "[Image #1] [Image #2] " "Ctrl+Alt+V, as ESC ^V, stages the next one")
      (press-wire model 22)
      (is-composer model "[Image #1] [Image #2] [Image #3] " "and Ctrl+V, as the console host passes it")
      (is (= 3 (length (nle::chat-model-pending-images model))) "three images staged"))))

(deftest an-empty-terminal-paste-without-an-image-types-nothing ()
  (with-frame-model (model 80 30 :session-id "empty-paste-none")
    (with-stubbed-fdefinition (nle::read-clipboard-image-base64 () nil)
      (paste-wire model "")
      (is-composer model "" "nothing to stage, nothing typed")
      (is (null (nle::chat-model-pending-images model)) "no image staged"))))

(deftest a-text-paste-never-asks-for-the-clipboard-image ()
  (with-frame-model (model 80 30 :session-id "text-paste-no-probe")
    (let ((asked 0))
      (with-stubbed-fdefinition (nle::read-clipboard-image-base64 ()
                                  (incf asked)
                                  (values "image/png" "iVBORw0KGgo="))
        (paste-wire model "hello")
        (is-composer model "hello" "the text landed as typed")
        (is (zerop asked) "the clipboard was never asked for an image")))))

(deftest a-pasted-image-path-stages-the-file ()
  ;; Windows Terminal pastes a snip (Win+Shift+S, PrintScreen) as the path of
  ;; the PNG the snipping tool left; a file dragged onto a terminal types its path.
  (uiop:with-temporary-file (:pathname png :type "png" :direction :output :element-type '(unsigned-byte 8) :stream out)
    (write-sequence (coerce #(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A 0 0 0 13) '(vector (unsigned-byte 8))) out)
    (finish-output out)
    (let ((path (uiop:native-namestring png)))
      (with-frame-model (model 80 30 :session-id "pasted-image-path")
        (paste-wire model path)
        (is-composer model "[Image #1] " "the path became the image")
        (is (equal "image/png" (car (first (nle::chat-model-pending-images model)))) "typed by its bytes")
        (paste-wire model (format nil "\"~a\"~%" path))
        (is-composer model "[Image #1] [Image #2] " "a quoted path, with the newline a terminal adds, too")))))

(deftest a-pasted-path-that-is-no-image-stays-text ()
  (uiop:with-temporary-file (:pathname fake :type "png" :direction :output :stream out)
    (write-string "not a png" out)
    (finish-output out)
    (let ((path (uiop:native-namestring fake)))
      (with-frame-model (model 80 30 :session-id "pasted-path-text")
        (paste-wire model path)
        (is-composer model path "a .png whose bytes are no PNG stays its path")
        (is (null (nle::chat-model-pending-images model)) "nothing staged"))))
  (with-frame-model (model 80 30 :session-id "pasted-path-missing")
    (paste-wire model "/no/such/dir/shot.png")
    (is-composer model "/no/such/dir/shot.png" "a missing file stays its path"))
  (with-frame-model (model 80 30 :session-id "pasted-path-prose")
    (paste-wire model "see notes.txt")
    (is-composer model "see notes.txt" "text that names no image stays text")))

(deftest windows-reads-the-clipboard-image-through-powershell ()
  (let* ((argv (nle::windows-clipboard-image-argv))
         (script (sb-ext:octets-to-string (cl-base64:base64-string-to-usb8-array (car (last argv))) :external-format :utf-16le)))
    (is (equal '("powershell.exe" "-NoProfile" "-NonInteractive" "-STA" "-EncodedCommand") (butlast argv)) "Windows PowerShell, no profile, single-threaded for the clipboard")
    (is (equal nle::*windows-clipboard-image* script) "the encoded command is the reader script, as UTF-16LE base64"))
  (is (equal '("image/png" "iVBORw0KGgo=") (multiple-value-list (nle::clipboard-image-line "image/png iVBORw0KGgo="))) "the reader's line splits into its media type and its base64")
  (is (equal '("image/jpeg" "/9j/") (multiple-value-list (nle::clipboard-image-line "image/jpeg /9j/"))) "a copied JPEG file keeps its own type")
  (is (null (nle::clipboard-image-line nil)) "no output, no image")
  (is (null (nle::clipboard-image-line "image/png ")) "a type with no payload is no image")
  (is (null (nle::clipboard-image-line "text/plain hello")) "only an image type is taken"))

(deftest connect-wizard-close-runs-the-chained-continuation ()
  ;; Esc that closes the /models wizard runs the continuation a caller
  ;; chained it with. The setup walk chains it: a close that ran nothing
  ;; stranded the walk -- no model stage, no cells, no closing block.
  (with-frame-model (model 80 30 :session-id "connect-close")
    (let ((closes 0))
      (flet ((open-wizard ()
               (open-openai-connect model)
               (let ((dialog model.dialog))
                 (setf dialog.on-cancel (lambda (model) (declare (ignore model)) (incf closes)))
                 dialog)))
        (nle::connect-dialog-key model (open-wizard) :escape)
        (is (null model.dialog) "Esc closes the wizard")
        (is (= 1 closes) "and the close runs the continuation")
        (let ((dialog (open-wizard)))
          (nle::connect-enter-step dialog :result)
          (nle::connect-dialog-key model dialog :escape)
          (is (eq dialog model.dialog) "a failed save is not a close")
          (is (eq :api-key (nle::connect-dialog-step dialog)) "Esc returns to the key")
          (is (= 1 closes) "and runs nothing"))))))

(deftest connect-wizard-a-saved-key-returns-to-the-list-and-enter-goes-on (let ((saved nil)
                                                                                 (continued 0)))
  ;; A saved key returns to the provider list, asked for anew so the
  ;; provider reads connected under a Suggested heading and is selected
  ;; past it: one open connects every provider the operator has a key for,
  ;; then Enter on a connected one opens what the caller chained. A search that
  ;; clears seats past the heading again. The save answers off the loop,
  ;; where no key's update repaints the band, so the key's line is painted at
  ;; once.
  (with-frame-model (model 100 30 :session-id "connect-saved-line")
    (let ((answer nil))
      (nle::open-connect-dialog
       model (lambda ()
               (append (when saved (list (nle::make-picker-row :label "Suggested" :heading t)))
                       (list (nle::make-picker-row :label "OpenAI"
                                                   :value (list :provider "openai" :connected saved)))))
       (nlk:lambda-ignoring (model provider-id secret done)
         (setf answer (lambda ()
                        (setf saved t)
                        (funcall done model "OpenAI key works, saved to auth.json" nil))))
       (nlk:lambda-ignoring (model provider)
         (when (equal "openai" provider) (incf continued))))
      (dialog-keys model #\o #\p :enter #\s #\k :enter)
      (nle::render-state model)         ; the key's own update tail
      (funcall answer)
      (let ((dialog model.dialog))
        (is (eq :provider (nle::connect-dialog-step dialog)) "the list is back")
        (is (equal '("Suggested" "OpenAI") (row-labels dialog)) "asked for anew")
        (is (equal "" (nle::connect-dialog-search dialog)) "the search cleared")
        (is (= 1 (nle::connect-dialog-selection dialog)) "resting on OpenAI, past the heading")
        (dialog-keys model #\o :backspace)
        (is (= 1 (nle::connect-dialog-selection dialog)) "and again once a search clears"))
      (is (rows-carry-p "OpenAI key works, saved to auth.json" (nle::compose-frame model)))
      (is (zerop continued))
      (dialog-keys model :enter)
      (is (null model.dialog))
      (is (= 1 continued) "Enter on it runs what the caller chained, on its provider"))))

(deftest composer-newline-override-displaces-the-defaults ()
  (with-frame-model (model 80 24 :session-id "newline-override")
    (setf model.keymap
          (nle::resolve-keybinds '((:insert-newline . "ctrl+n"))))
    (type-string model "a")
    (press model #\n :mod nlt:+mod-ctrl+)
    (type-string model "b")
    (is-composer model (format nil "a~%b") "the configured chord inserts the newline")
    (press model :enter :mod nlt:+mod-alt+)
    (is-composer model (format nil "a~%b") "the displaced Alt+Enter default no longer inserts")))

;;; --- claim 8: the hardware cursor parks on the composer caret --------------
;;;
;;; Zig parks vaxis's real terminal cursor at the composer caret cell
;;; (composer.zig cursor_visible); the lisp shell mirrors it: the editor
;;; paints no bracket caret, COMPOSE-FRAME records the
;;; caret's frame cell, and SHELL-VIEW hands it to the renderer as the
;;; declarative cursor. A docked dialog panel replaces the composer, so the
;;; cursor hides (?25l) exactly while no caret cell exists.

(deftest composer-parks-the-hardware-cursor ()
  (with-frame-model (model 80 30 :session-id "composer-cursor")
    ;; Empty composer: the caret parks at the text inset, over the hint --
    ;; the column a sent prompt's words start at, where a run's title stands.
    (with-parked-cursor (model)
      (is (consp park) "an idle frame parks the cursor")
      (is (= (+ 2 (nle::composer-text-inset)) (car park)) "the empty composer parks at the text inset")
      (is (= 5 (car park)) "on the prose column"))
    (type-string model "first")
    (with-parked-cursor (model row)
      (is (= 10 (car park)) "the caret column is the inset + text width")
      (is (search "first" row) "the caret row is the composer text row")
      (is (null (search "[ ]" row)) "no bracket caret paints in the composer"))
    ;; A newline moves the caret to the box's second text row.
    (press model :enter :mod nlt:+mod-alt+)
    (type-string model "second")
    (with-parked-cursor (model row)
      (is (= 11 (car park)) "the caret tracks the second line's width")
      (is (search "second" row) "the caret row follows the newline"))
    ;; The view-state carries the park as the declarative cursor.
    (let ((cursor (nlt:view-state-cursor (nle::shell-view model))) (park model.parked-cursor))
      (is cursor "the view-state parks the hardware cursor")
      (is-shape cursor (.x = (car park)) (.y = (cdr park))))))

(deftest composer-cursor-hides-under-a-docked-dialog ()
  (with-frame-model (model 80 30 :session-id "composer-cursor-dlg")
    (nle::open-picker-dialog model "Pick" (picker-rows '("x" :value :x)) nil)
    (nle::compose-frame model)
    (is (null model.parked-cursor))
    (is (null (nlt:view-state-cursor (nle::shell-view model))))))

;;; --- claim 9: a docked text field takes the hardware cursor with it --------
;;;
;;; Zig turns the cursor off under any docked panel and the panels that own a
;;; text field put it back on their own row (dockedDialogSearchFocus,
;;; connectDialogInputFocus). The lisp shell parks the same cell, so an input
;;; that is not the composer still shows a real terminal caret.

(deftest dialog-search-row-parks-the-hardware-cursor ()
  (with-frame-model (model 80 30 :session-id "dialog-cursor")
    (nle::open-picker-dialog
     model "Extensions" (picker-rows '("alpha" :value :alpha)) nil :searchable t)
    (with-parked-cursor (model row)
      (is-shape park (consp is "a searchable panel parks the cursor") (car = 4))
      (is (search "Type to search" row) "and that row is the search row, over its placeholder"))
    (dialog-keys model #\a #\l)
    (with-parked-cursor (model row)
      (is (= 6 (car park)) "the caret column follows the typed query")
      (is (search "al" row) "the query paints on the parked row")
      (is (null (search "▏" row)) "no caret glyph stands in for the real cursor")
      (let ((cursor (nlt:view-state-cursor (nle::shell-view model))))
        (is cursor "the view-state carries the dialog cursor")
        (is (= (car park) (nlt:cursor-x cursor)) "column matches the park")
        (is (= (cdr park) (nlt:cursor-y cursor)) "row matches the park")))))

(deftest connect-wizard-input-parks-the-hardware-cursor ()
  (with-frame-model (model 80 30 :session-id "connect-cursor")
    (open-openai-connect model)
    ;; The provider step is the searchable one: it focuses the search row.
    (with-parked-cursor (model row)
      (is (= 4 (car park)) "the provider step parks on the search row")
      (is (search "Search providers" row) "over the provider-search placeholder"))
    ;; The field steps park past the label and the value instead. The steps
    ;; move through the flow's own seam, which is also what repaints the
    ;; panel chrome the cursor arithmetic reads.
    (let ((dialog model.dialog))
      (nle::connect-enter-step dialog :custom-provider)
      (setf dialog.input "acme"))
    (with-parked-cursor (model row)
      ;; content column 4 + "Provider id" (11) + the two-space gap + "acme".
      (is (= 21 (car park)) "the caret sits past the typed field value")
      (is (search "Provider id  acme" row) "on the field row itself"))
    ;; The secret step parks past the mask, never past the plaintext.
    (let ((dialog model.dialog))
      (nle::connect-enter-step dialog :api-key)
      (setf dialog.input "sk-secret"))
    (with-parked-cursor (model)
      ;; content column 4 + "API key" (7) + the gap + 9 mask bullets.
      (is (= 22 (car park)) "the caret follows the mask width"))
    ;; The result step owns no field, so the cursor goes dark again.
    (nle::connect-enter-step model.dialog :result)
    (nle::compose-frame model)
    (is (null model.parked-cursor))))

;;; --- claim 10: the composer text wraps at the box boundary -----------------
;;;
;;; The composer holds live, unstyled text whose caret has to map to a visual
;;; cell, so the widget owns the break — it runs BREAK-FRAGMENTS, the one
;;; word-aware line-break authority, over the raw text at
;;; boxGeometry.text_width for exactly that reason. With the lisp widget's
;;; soft wrap off, a prompt longer than one row painted as a single
;;; over-wide frame row: it ran past the box's right edge and the terminal
;;; wrapped the tail to column 0, outside the box entirely. The wrap width
;;; is the composer's own (COMPOSER-TEXT-WIDTH), not the transcript's.

(deftest composer-wraps-long-text-inside-the-box ()
  (with-frame-model (model 80 30 :session-id "composer-wrap")
    (let* ((text-width (nle::composer-text-width 80))
           (head (make-string text-width :initial-element #\a))
           (tail "spill"))
      (is (= text-width
             (nle::editor-width model.editor)))
      (type-string model (concatenate 'string head tail))
      (let ((rows (nle::compose-frame model)))
        (is (= 30 (length rows)) "a wrapped composer keeps the frame height")
        (is (every (lambda (row) (= 80 (nle::text-display-width row))) rows))
        (is (= 2 (nle::composer-visible-rows model)))
        ;; The last composer text row is height - 5; a two-row box occupies
        ;; the two rows ending there.
        (is (search head (nth 24 rows)))
        (is (search tail (nth 25 rows)))
        (is (null (search tail (nth 24 rows))))))))

(deftest composer-moves-a-word-the-row-cannot-hold-to-the-next-row ()
  ;; The word-whole rule at the composer seam: a row that already holds text
  ;; and a space ends there, and the word past it lands whole on the row
  ;; below — never divided between the two (the hard-break rule put as much
  ;; of the word on the row above as fit).
  (with-frame-model (model 80 30 :session-id "composer-word-wrap")
    (let* ((text-width (nle::composer-text-width 80))
           (word "endtoendword")
           (lead (make-string (- text-width 12) :initial-element #\a)))
      (type-string model (concatenate 'string lead " " word))
      (let ((rows (nle::compose-frame model)))
        (is (= 30 (length rows)) "the frame still composes")
        (is (= 2 (nle::composer-visible-rows model)))
        (is (= 1 (count-if (lambda (row) (search word row)) rows)))
        (is (every (lambda (row)
                     (or (search word row)
                         (null (search (subseq word 0 5) row))))
                   rows))))))

(deftest composer-caret-stays-inside-the-wrapped-box ()
  (with-frame-model (model 80 30 :session-id "composer-wrap-caret")
    (let* ((text-width (nle::composer-text-width 80))
           (box-right (1- (+ 2 (nle::composer-box-width 80)))))
      ;; A row filled to the last text column: the caret parks one column
      ;; inside the box, and the box's last column stays whole as padding
      ;; (Zig text_right_padding), so neither the text nor the caret ever
      ;; touches the box's right edge — the right padding the operator asks for.
      (type-string model (make-string text-width :initial-element #\a))
      (with-parked-cursor (model)
        (is (= 1 (nle::composer-visible-rows model)))
        (is (= (+ 2 (nle::composer-text-inset) text-width) (car park)))
        (is (= (1- box-right) (car park)) "one column short of the box's right edge")
        (is (= 25 (cdr park)) "on the box's only text row"))
      ;; One more character wraps: the caret follows to the next visual row.
      (type-string model "b")
      (with-parked-cursor (model)
        (is (= 2 (nle::composer-visible-rows model)) "the box grew a row")
        (is (= 6 (car park)) "the caret sits past the wrapped character")
        (is (= 25 (cdr park)) "on the new bottom text row"))
      ;; Up/Down walk visual rows inside the wrapped line, not over the whole
      ;; logical line — history declines here, so the keys reach the widget.
      (press model :up)
      (with-parked-cursor (model)
        (is (= 24 (cdr park)) "Up lands on the wrapped row above")
        (is (= 6 (car park)) "keeping its offset within the row"))
      (press model :down)
      (with-parked-cursor (model)
        (is (= 25 (cdr park)) "Down returns to the wrapped row below")
        (is (= 6 (car park)) "at the same offset")))))

(deftest composer-catalogs-walk-off-the-loop-thread ()
  ;; The @ catalog is a filesystem walk. READ-COMPOSER-CATALOGS is the
  ;; walk the preload thread runs, and a walk that signals is carried as text
  ;; rather than swallowed into a silently empty dropdown.
  (with-temp-workspace (dir "README.md" ".git/HEAD")
    (let ((result (nle::read-composer-catalogs dir)))
      (is (getf result :files-loaded) "the file walk answered")
      (is (member "README.md" (getf result :files) :test #'equal))
      (is (null (getf result :failures)) "and it did not fail"))))

(deftest composer-catalogs-land-in-the-tick-and-a-failure-retries ()
  ;; The walk thread leaves its plist in the results table; the loop's half is
  ;; LAND-COMPOSER-CATALOG-WALKS, which the tick branch calls before its render
  ;; decision. Install an answer, keep a failed walk unloaded so the next
  ;; trigger retries it, and put the failure up as an error toast — silence is
  ;; what the old (ignore-errors ...) left behind.
  (with-frame-model (model 80 24 :session-id "land-catalogs")
    (is (null (nle::land-composer-catalog-walks model)))
    (setf (gethash model nle::*composer-catalog-results*)
          (list :files-loaded nil :failures '("file catalog unavailable — fixture")))
    (is-shape model (nle::land-composer-catalog-walks is "a failed walk still lands")
      (.file-catalog null) (.file-catalog-loaded null))
    (is (find "file catalog unavailable" model.toasts :key #'nle::toast-text :test #'search))
    ;; A walk that answered installs, and drains the table so one walk never
    ;; lands twice.
    (setf (gethash model nle::*composer-catalog-results*)
          (list :files '("README.md") :files-loaded t :failures nil))
    (is (nle::land-composer-catalog-walks model) "the landed walk installs")
    (is-shape model (.file-catalog '("README.md")) (.file-catalog-loaded is "and its loaded flag"))
    (is (null (gethash model nle::*composer-catalog-results*)))))

(deftest composer-trigger-asks-for-the-walk-rather-than-paying-it ()
  ;; The defect in a unit: with nothing loaded, a trigger used to walk the
  ;; filesystem inside the keystroke's own frame. Now it answers from whatever
  ;; has landed and asks for the walk; a model with no program has no frame to
  ;; paint, so nothing starts here.
  (with-frame-model (model 80 24 :session-id "t111-lazy" :typed "@t111")
    (is-shape model (nle::token-suggestions-for null) (.file-catalog null)
      (.file-catalog-loaded null) (nle::start-composer-catalog-walk null))
    (is (null (gethash model nle::*composer-catalog-walks*)))))

(deftest composer-catalogs-boot-from-the-last-walks-cache ()
  ;; The walk's answer is kept across processes, keyed by the workspace
  ;; the shell runs in, and the boot installs it before the first frame — so
  ;; the first `@' paints rows without waiting on the walk (seconds while a
  ;; fresh shell boots an organism beside itself).
  (let ((nle::*composer-catalog-cache-path* (temp-path "t111-cache" "sexp")))
    (is (null (nle::read-composer-catalog-cache (uiop:getcwd))))
    (let ((result (list :files '("README.md") :files-loaded t
                        :failures '("file catalog unavailable — fixture"))))
      (nle::write-composer-catalog-cache (uiop:getcwd) result)
      (is-present (back (nle::read-composer-catalog-cache (uiop:getcwd))) "the answer reads back"
        (is-shape back (:files '("README.md")) (:files-loaded is) (:failures null)))
      ;; A failed walk is not remembered as an answer: the next boot retries it.
      (nle::write-composer-catalog-cache (uiop:getcwd) (list :files-loaded nil :failures nil))
      (is (null (getf (nle::read-composer-catalog-cache (uiop:getcwd)) :files-loaded)))
      ;; And the boot installs what the cache holds: the frame that paints
      ;; first already has the rows, no trigger having asked for anything.
      (nle::write-composer-catalog-cache (uiop:getcwd) result)
      (with-frame-model (model 80 24 :session-id "cache-boot")
        (is (nle::chat-model-file-catalog-loaded model))
        (type-string model "@REA")
        (is (= 1 (length (nle::token-suggestions-for model))))))))

(deftest composer-counts-the-lines-its-box-hides ()
  ;; Without this, a paste taller than the box would show its last rows with
  ;; nothing saying the rest is there. A paste under the collapse bounds lands
  ;; as typed text, so it scrolls inside the box like any; the pad row above
  ;; the text counts the lines that start out of view, the one below those that
  ;; end out of view.
  (with-frame-model (model 80 24 :session-id "composer-hidden")
    (nle::attach-composer-paste model (format nil "~{line ~2,'0d~^~%~}"
                                              (loop for i from 1 to 10 collect i)))
    (let ((rows (nle::compose-frame model)))
      (is (= 7 (nle::composer-visible-rows model)) "the box stops at its cap")
      (is (rows-carry-p "+3 lines above" rows) "the three out of view above are counted")
      (is (rows-carry-p "line 04" rows) "and the first line in view is the fourth")
      (is (not (rows-carry-p "line 03" rows)))
      (is (not (rows-carry-p "lines below" rows)) "the caret's end is in view"))
    (setf (nle::editor-row model.editor) 0
          (nle::editor-col model.editor) 0)
    (let ((rows (nle::compose-frame model)))
      (is (rows-carry-p "+3 lines below" rows) "at the top the count moves under the text")
      (is (not (rows-carry-p "lines above" rows))))))


(deftest session-switch-status-is-transient-composer-only ()
  ;; The sessions picker's Enter runs the switch owner; the switch says so in
  ;; the composer status, over whatever stood there, with no extra rows or
  ;; viewport movement.
  (with-stubbed-fdefinitions ((nle::send-subscribe (ws ids))
                             (nle::refresh-gateway-model-facts (model)))
    (dolist (width '(40 80))
      (with-frame-model (model width 24 :session-id "switch-from")
        (let* ((state (nle::initial-chat-state))
               (row (nle::make-transcript-row :kind :text :role "user"
                                              :text "destination history")))
          (nle::append-tail-row state row)
          (setf model.transcript-preview (cons "switch-to" state))
          (nle::show-status model "Switched to previous-session")
          (nle::open-picker-dialog model "Sessions" (picker-rows '("switch-to" :value "switch-to"))
                                   #'nle::attach-picked-session :kind :sessions)
          (press model :enter)
          (is-shape model (nle::chat-model-session-id "switch-to") (.dialog null))
          (is (equal (list row) (nle::transcript-rows model.state)))
          (is-news-in-composer model width "Switched to switch-to" "Switched")
          (nle::show-status model "Switched to switch-to")
          (type-string model "my draft")
          (is (equal "my draft" (composer-value model)))
          (is (search "my draft" (nth 19 (nle::compose-frame model))))
          (decf (nle::composer-status-born model.composer-status) (* 6 internal-time-units-per-second))
          (nle:update model (nlt:make-tick-msg))
          (nlt:view model)
          (is (null model.composer-status) "expiry also clears a hint hidden by a draft")
          (is (equal "my draft" (composer-value model))))))))

(deftest session-attach-and-fork-status-is-transient ()
  (with-stubbed-fdefinitions ((nle::send-subscribe (ws ids))
                             (nle::refresh-gateway-model-facts (model))
                             (nle::gateway-model-http-call (model method path body)
                               (nlk:json-object "session_id" "fork-destination")))
    (with-frame-model (model 80 24 :session-id "fork-parent")
      (is (null (nle::attach-picked-session model "fork-parent")))
      (is (equal "Already attached to this session" (nle::status-said model)))
      ;; Submit the actual slash: its return goes through SUBMIT-SLASH too.
      (type-string model "/fork" :enter)
      (is-shape model (nle::chat-model-session-id "fork-destination")
        (nle::home-screen-p is) (nle::status-said "Forked from fork-parent"))
      (is (search "Forked from fork-parent" (nth 19 (nle::compose-frame model)))))))

(deftest composer-ctrl-u-removes-empty-lines-on-the-wire ()
  (with-frame-model (model 80 30 :session-id "composer-empty-ctrl-u" :typed "above")
    (press-wire model 10)
    (press-wire model 10)
    (type-string model "below")
    (press-wire model 1)
    (press-wire model 2) ; Back across the newline to the middle empty line.
    (nle::compose-frame model)
    (is (= 3 (nle::composer-visible-rows model)))
    (let ((before (nle::band-window-height model.band)))
      (press-wire model 21)
      (is-composer model (format nil "above~%below") "raw Ctrl+U removes only the empty middle line")
      (is-shape model.editor (nle::editor-row = 0) (nle::editor-col = 5))
      (nle::compose-frame model)
      (is (= 2 (nle::composer-visible-rows model)))
      (is (= (1+ before) (nle::band-window-height model.band)) "the transcript gains the removed composer row"))
    (type-string model "!")
    (is-composer model (format nil "above!~%below") "the next typed character lands on the adjacent previous line")
    (is (null model.turn-in-flight) "Ctrl+U never submits a prompt")))

(deftest theme-change-status-is-transient-composer-only (let ((engaged nil) (saved nil)))
  (with-stubbed-fdefinitions
      ((nle::engage-theme (&rest args) (setf engaged (getf args :name)))
       (nle::save-theme-selection (name) (setf saved name)))
    (dolist (width '(40 80))
      (with-frame-model (model width 24 :session-id "theme-status"
                         :paint ((nle::ingest-message-fact "user" "history stays here")))
        (let ((rows (copy-list (nle::transcript-rows model.state))))
          (type-string model "/themes Tungstene" :enter)
          (is (equal "Tungstene" engaged))
          (is (equal "Tungstene" saved))
          (is (equal rows (nle::transcript-rows model.state)))
          (is-news-in-composer model width "Changed to Tungstene theme")
          (is (equal rows (nle::transcript-rows model.state))))))))

(deftest theme-picker-status-preserves-draft-and-cancel-is-silent (let ((saved nil)))
  (with-stubbed-fdefinitions
      ((nle::engage-theme (&rest args))
       (nle::save-theme-selection (name) (setf saved name)))
    (with-frame-model (model 80 24 :session-id "theme-picker-status" :typed "my draft")
      (is (null (nle::slash-themes-op model "")))
      (press model :escape)
      (is (null saved))
      (is-shape model (.composer-status null))
      (nle::slash-themes-op model "")
      (let ((dialog model.dialog))
        (setf dialog.selection
              (position "Tungstene" dialog.rows :key #'nle::picker-row-value :test #'equal)))
      (press model :enter)
      (is (null model.dialog))
      (is (equal "Tungstene" saved))
      (is-shape model (nle::status-said "Changed to Tungstene theme")
        (composer-value "my draft"))
      (is (search "my draft" (nth 19 (nle::compose-frame model))))
      (is (not (rows-carry-p "Changed to Tungstene theme" (nle::compose-frame model))))
      (expire-status-news model)
      (nlt:view model)
      (is-shape model (.composer-status null) (composer-value "my draft")))))

(deftest viewport-switch-status-is-transient-composer-only (dolist (width '(40 80)))
  (dolist (action '(:keyboard :slash))
    (with-frame-model (model width 24 :session-id "viewport-status"
                       :paint ((nle::ingest-message-fact "user" "history stays here")))
      (let ((rows (copy-list (nle::transcript-rows model.state))))
        (dolist (mode '(:developer :standard))
          (ecase action
            (:keyboard (press model #\o :mod nlt:+mod-ctrl+))
            (:slash
             (type-string model (format nil "/details ~a" (string-downcase mode)))
             (press model :enter)))
          (let ((status (format nil "Switched to ~:(~a~) viewport" mode)))
            (is (eq mode model.tool-detail))
            (is (equal rows (nle::transcript-rows model.state)))
            (is-news-in-composer model width status)))))))

(deftest viewport-status-preserves-draft-and-query-errors ()
  (with-frame-model (model 80 24 :session-id "viewport-status-draft" :typed "my draft")
    (press model #\o :mod nlt:+mod-ctrl+)
    (is-shape model (.tool-detail eq :developer)
      (nle::status-said "Switched to Developer viewport") (composer-value "my draft"))
    (is (search "my draft" (nth 19 (nle::compose-frame model))))
    (is (not (rows-carry-p "Switched to" (nle::compose-frame model))))
    (expire-status-news model)
    (nlt:view model)
    (is (null model.composer-status) "a draft does not prevent expiry")
    (is (equal "my draft" (composer-value model)))
    (is (equal "viewport Developer" (nle::slash-details-op model "")))
    (is (null model.composer-status) "querying does not announce a switch")
    (is (search "usage: details" (nle::slash-details-op model "expanded")))
    (is (eq :developer model.tool-detail))
    (is (null model.composer-status) "invalid input does not announce a switch")))
