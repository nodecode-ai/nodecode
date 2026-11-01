;;;; display.lisp --- display policy: logical lines, fades, clickable targets. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- display policy: logical lines ----------------------------------------
;;; Row semantics to renderable rows in two steps, mirroring the Zig layers:
;;; a row becomes LOGICAL LINES (text + styled spans + gutter role), and the
;;; layout pass wraps each logical line at the frame width through the one
;;; line-break authority (theme.lisp BREAK-FRAGMENTS), painting gutters and
;;; hanging indents exactly as src/tui/body_layout.zig and transcript.zig do.

(defstruct (mspan (:constructor mspan (start end &key tone bold italic dim
                                            underline strike hidden link ground glint)))
  "A styled span over a logical line, offsets in characters."
  ;; HIDDEN spans are markdown marker bytes that stay in the text but never
  ;; paint, nor count where the line breaks. LINK, when set, is the URI the
  ;; painted span wraps in an OSC-8 hyperlink and a click on it opens
  ;; (OPEN-TARGET): an http(s) page, or a file: target naming a file or a
  ;; folder. GROUND is a tone whose background the span stands on, a strong
  ;; run's chip; GLINT, the internal time it arrived at when a glint crosses
  ;; it then (GLINT-OVERLAY).
  start end tone bold italic dim underline strike hidden link ground glint)

(nlk:access (moved mspan) (span mspan))

(defmacro copy-with (type object &rest slots &aux (copy (gensym "COPY")))
  "A copy of OBJECT, a TYPE structure, with SLOTS -- slot keywords and value
forms, evaluated in order -- stored on the copy. => the copy."
  ;; The painted lines and spans are shared across frames, so a variant of
  ;; one is a copy carrying the change, never the shared one written over.
  `(let ((,copy (,(alexandria:symbolicate 'copy- type) ,object)))
     (setf ,@(loop for (slot value) on slots by #'cddr
                   collect `(,(alexandria:symbolicate type '- slot) ,copy) collect value))
     ,copy))

(defun shifted-span (span offset)
  "A copy of SPAN moved OFFSET characters along its line."
  (copy-with mspan span :start (+ offset span.start) :end (+ offset span.end)))

(defstruct lline
  "One logical line ready for layout: body text, base style, spans, gutter."
  (text "" :type string)
  (tone :foreground)
  (bold nil) (italic nil) (dim nil) (underline nil)
  (spans '())
  (role :prose)
  (gutter-tone nil)
  (gutter-italic nil)
  (hang 0)
  ;; A SEG, or a list of them, each continuation row paints at the head of
  ;; its hang, over as many of the hang's blanks as it is wide: a quote's
  ;; bars run down every row the line wraps onto, a code block's edge.
  (hang-mark nil)
  ;; What the line's rows stand on, (TONE FROM TO FADE): TONE's background
  ;; from column FROM below TO -- an integer the first row's, :TEXT two past
  ;; each row's words, :EDGE the text's right edge -- then FADE cells of it
  ;; going out into the canvas (GROUND-SEGS).
  (ground nil)
  ;; A SEG standing at the right end of the line's first row, which wraps
  ;; short of it by a two-column gap: a message's clock (LLINE-ASIDE-SHOWN).
  (aside nil)
  (fill nil)
  ;; Painted under a shadow: every glyph of the row — text, spans, gutter —
  ;; at *SHADOW-PERCENT* of the light, the canvas beneath them untouched
  ;; (SHADOW-STYLE). The Zig cell-level shadow pass, carried on the lline so
  ;; LLINE-RENDER keys on it.
  (shadow nil)
  (rule nil)
  ;; A table's row: its columns are laid out to the row, so a row cut short
  ;; for an aside would break them (STAMP-TRANSCRIPT-LLINES).
  (grid nil)
  ;; On the first row of an inline image band: the image fact drawn over it
  ;; (IMAGE-BAND-LLINES), which is how the placement finds the band.
  (image nil))

(nlk:access (b lline) (fresh lline) (line lline) (title lline))

;;; Developer preserves grouped routine calls and output previews. Standard
;;; keeps live calls as name-plus-comment disclosures. At a terminal fact,
;;; its tool/thought trail folds to one clickable `Ran N commands' summary.
;;; Both keep thoughts folded; there is no global open-all mode.
(defparameter *tool-detail* :standard
  "Render-scope viewport mode, :DEVELOPER or :STANDARD.")

;;; RENDER-STATE binds it from the model's EXPANDED-TOOL-CALL slot (the Zig
;;; expandedToolCallId accordion). It opens what it names whatever the detail
;;; level.
(defvar *expanded-tool-call* nil
  "Render-scope accordion state: the expand key of the one clicked-open
card, group or thought, or NIL.")

;;; --- fades -------------------------------------------------------------
;;; A place on the screen that shows one thing at a time -- a turn's words
;;; under its run, the finished thought under the live bar -- hands it to a
;;; newer thing by a fade: what stood there goes out into the canvas, then
;;; the newcomer comes up out of it. The light is the frame's paint
;;; (FADE-OVERLAY), cell by cell over the rows the frame shows, so no row
;;; is laid out again while it moves; the swap, once the old thing has gone
;;; out, is a render (FADE-DUE-P).

(defvar *fades* nil
  "T while rows lay out for a shell painting on a terminal (WITH-RENDER-SCOPE):
places fade there, and nowhere else -- a test or the image's warm-up lays
out what the state holds now.")

(defstruct (fade (:constructor make-fade (shown out in &aux (at (get-internal-real-time)))))
  "One place's fade: SHOWN the thing that holds it, LEAVING what stood there
before it, going out from AT from FROM percent of its light, at the pace
that takes all of it out in OUT milliseconds, SHOWN then coming up over IN
-- all at once, or with a STEP, its TOKENS one after another, each over IN,
a STEP behind the one before it: in reading order, or in ORDER's, a vector
giving each token the slot it comes up in."
  shown (leaving nil) (from 100) at out in (step 0) (tokens 1) (order nil))

(nlk:access (fade fade))

(defun fade-fall (fade)
  "Milliseconds what is leaving FADE's place takes to go out: OUT from all
of its light, less from less, so a thing overtaken before it was up goes at
once and its newcomer never waits on the dark."
  (if fade.leaving (ceiling (* fade.out fade.from) 100) 0))

(defun fade-on (fade &optional (now (get-internal-real-time)))
  "What FADE's place shows at NOW: what is leaving, while it goes out, then
what holds it."
  (if (and fade.leaving (< (since-ms fade.at now) (fade-fall fade))) fade.leaving fade.shown))

(defun fade-rise (fade)
  "Milliseconds what holds FADE's place takes to come all the way up."
  (+ fade.in (* fade.step (max 0 (1- fade.tokens)))))

(defun fade-light (fade item &optional (now (get-internal-real-time)) token)
  "How much of its light, in percent, ITEM stands at in FADE's place at NOW:
the whole of it, or its TOKENth token when its tokens come up one by one."
  (let ((ms (since-ms fade.at now)))
    (cond ((and fade.leaving (equal item fade.leaving))
           (max 0 (- fade.from (floor (* 100 ms) (max 1 fade.out)))))
          (t (let ((up (max 0 (- ms (fade-fall fade)))))
               (min 100 (if token
                            (floor (* 100 (max 0 (- up (* (if fade.order
                                                              (aref fade.order (mod token (length fade.order)))
                                                              token)
                                                          fade.step))))
                                   (max 1 fade.in))
                            (floor (* 100 up) (max 1 (fade-rise fade))))))))))

(defun fade-settled-p (fade &optional (now (get-internal-real-time)))
  "Whether FADE's place stands still at NOW: what holds it is all the way up."
  (>= (since-ms fade.at now) (+ (fade-fall fade) (fade-rise fade))))

(defun fade-to (fade item &optional (now (get-internal-real-time)))
  "Hand FADE's place to ITEM at NOW: what it shows goes out first, from the
light it had come up to; what is already going out keeps going, and ITEM
waits behind it. => FADE."
  (unless (equal item fade.shown)
    (let ((on (fade-on fade now)))
      (unless (and fade.leaving (equal on fade.leaving))
        (setf fade.from (fade-light fade on now) fade.leaving on fade.at now))
      (setf fade.shown item)))
  fade)

(nlk:define-side-table fade-line (:weakness :key)
  "A painted transcript row -> (FADE . ITEM): the place it stands in and what
of that place it paints, so the frame's FADE-OVERLAY finds it on screen.")

(defun mark-fading-rows (rows fade item)
  "ROWS, the painted rows of ITEM in FADE's place, marked for the overlay. => ROWS."
  (dolist (row rows rows)
    (setf (fade-line row) (cons fade item))))

(defun tool-row-expand-key (row)
  "Accordion identity for a tool card: the durable call id once the row
is hydrated, else the row object itself (a streaming row the wire has
not stamped yet). A thought has no id: its key is the row."
  (or (transcript-row-tool-call-id row) row))

(defun open-key (&optional (key *expanded-tool-call*))
  "What accordion KEY opens: a run's call, or one of its files, clicked open
under the run's shut title is keyed (:FOLD KEY), which opens KEY and leaves
the run shut (TOOL-GROUP-OPEN-P)."
  (if (and (consp key) (eq :fold (first key))) (second key) key))

(defun detail-open-p (key)
  "Whether the one clicked-open card, group or thought is KEY."
  (and key (equal (open-key) key)))

;;; RENDER-STATE binds it from the model; the hovered block paints on the
;;; :hover background (the Zig setHoveredToolCall/UserPrompt states).
(defvar *hover-row* nil
  "Render-scope hover state: the TRANSCRIPT-ROW under the pointer, or NIL.")

;;; --- clickable targets --------------------------------------------------------
;;; What a click on the transcript opens, in one currency: a URI string on the
;;; span (MSPAN-LINK), which is also exactly what the OSC-8 wrap hands the
;;; terminal — so a terminal that opens hyperlinks itself opens these without
;;; asking this program at all. Two schemes reach the opener: http(s), a page,
;;; and file:, a file or a folder. A span carrying neither never arms.
;;;
;;; Two kinds of target, and they are not built the same way. A FACT names its
;;; own address — the file a call read, the file a diff changed, the picture a
;;; LOOK named — and becomes a target directly. PROSE only ever looks like one,
;;; so it passes PATH-SHAPED-P first, which is deliberately narrow: an anchor
;;; a path alone has, or a relative path whose last segment carries an
;;; extension.
;;;
;;; Nothing here touches the filesystem. A golden frame is the same frame on
;;; every box, and whether the path is still there is the click's question
;;; (OPEN-TARGET), asked once, at the moment it is asked.

(defun percent-encode (text)
  "TEXT's UTF-8 octets as a URI path: each octet written %XX but RFC 3986's
unreserved set and the three a path carries as themselves -- the separator,
the drive colon and @."
  ;; Every other octet percent-encodes, so a space, or a name in any script,
  ;; survives both this program's parser and the terminal's. The scanners are
  ;; built once: a pattern handed over as data is compiled at every call, three
  ;; a path, and every tool card names one.
  (ppcre:regex-replace-all (load-time-value (ppcre:create-scanner "%(?:2F|3A|40)"))
                           (quri:url-encode text :encoding :utf-8)
                           #'percent-decode :simple-calls t))

(defun percent-decode (text)
  "The string TEXT's %XX escapes decode to, the octets read back as UTF-8."
  ;; A run of escapes is one character's octets or several whole ones — an
  ;; escape never shares a character with a literal — so each run decodes alone.
  (ppcre:regex-replace-all "(?:%[0-9A-Fa-f]{2})+" text
                           (lambda (run) (quri:url-decode run :encoding :utf-8)) :simple-calls t))

;;; A launch fact like the footer's label, and dropped by the same save hook,
;;; so no dumped image carries the build box's directory into an operator's
;;; session.
(defvar *workspace-directory* nil
  "The shell's own directory, memoized: what a relative path in the
transcript resolves against.")

(defun workspace-directory ()
  "The shell's directory, without its trailing separator."
  (or *workspace-directory*
      (setf *workspace-directory*
            (string-right-trim "/\\" (uiop:native-namestring (uiop:getcwd))))))

(defun absolute-path-p (path &aux (len (length path)))
  "Whether PATH names its own root already: a leading separator, or a drive
letter and a colon (the Windows shape)."
  (or (and (plusp len) (find (char path 0) "/\\"))
      (and (> len 2) (alpha-char-p (char path 0)) (char= #\: (char path 1))
           (find (char path 2) "/\\"))))

(defun absolute-path (path)
  "PATH as an absolute native path: one that names its own root untouched,
`~' against the operator's home, anything else against the shell's own
directory. NIL for nothing to resolve."
  (let ((path (string-trim '(#\Space #\Tab) (or path ""))))
    (cond ((zerop (length path)) nil)
          ((absolute-path-p path) path)
          ((or (string= "~" path) (uiop:string-prefix-p "~/" path))
           (let ((home (ignore-errors (uiop:native-namestring (user-homedir-pathname)))))
             (and home (plusp (length home))
                  (concatenate 'string (string-right-trim "/\\" home) (subseq path 1)))))
          (t (concatenate 'string (workspace-directory) "/" path)))))

(defun file-uri (path &optional place)
  "PATH as a file: URI — the currency a path travels in from the span that
names it to the click that opens it, and what a terminal that follows OSC-8
hyperlinks itself is handed. PLACE, a line cited in the file, rides as the
fragment."
  ;; A numeric fragment is the line, the shape kitty's hyperlinked grep
  ;; writes and its open actions read; the opener itself drops it.
  (when-let (absolute (absolute-path path))
    (let ((slashed (substitute #\/ #\\ absolute)))
      (concatenate 'string "file://"
                   (percent-encode (if (char= #\/ (char slashed 0))
                                       slashed
                                       (concatenate 'string "/" slashed)))
                   (if (plusp (length place)) (concatenate 'string "#" (percent-encode place)) "")))))

(defun cited-place (text)
  "TEXT, a path an answer may cite a place in, as (values PATH PLACE): a
`#' opens the place as written (`x.lisp#L12'), and a trailing `:12',
`:12:5' or `:10-20' cites line 12, 12 or 10 (PLACE \"12\"). PLACE is NIL for
a bare path."
  ;; The place is where in the file, never part of its name: left on the
  ;; name, `ui.lisp:1269' opened nothing and the click said it was gone.
  (if-let (hash (position #\# text))
    (values (subseq text 0 hash) (subseq text (1+ hash)))
    (multiple-value-bind (start end starts ends)
        (ppcre:scan (load-time-value (ppcre:create-scanner ":(\\d+)(?:[:-]\\d+)?\\z")) text)
      (declare (ignore end))
      (if start
          (values (subseq text 0 start) (subseq text (aref starts 0) (aref ends 0)))
          (values text nil)))))

(defun path-link-spans (path)
  "The link span over PATH for a line that opens with that fact's path, or NIL."
  (when-let (uri (and path (file-uri path)))
    (list (mspan 0 (length path) :link uri))))

(defun file-uri-path (uri)
  "The native path a file: URI names, or NIL when URI is not one."
  (when (and (stringp uri) (uiop:string-prefix-p "file://" uri))
    (let ((path (percent-decode (subseq uri (length "file://") (position #\# uri)))))
      #+win32 (substitute #\\ #\/
                          (if (and (> (length path) 2) (char= #\/ (char path 0))
                                   (char= #\: (char path 2)))
                              (subseq path 1)
                              path))
      #-win32 path)))

(defun web-url-p (text)
  "Whether TEXT is an http(s) address — the only scheme a model may write
that a click hands to the operating system."
  (and (stringp text) (ppcre:scan "^(?i)https?://" text) t))

(defun openable-target-p (uri)
  "Whether a click on URI opens anything: a page or a file, and nothing
else."
  ;; A markdown link's target is the model's to write, so a scheme this does
  ;; not name never arms.
  (or (web-url-p uri)
      (and (stringp uri) (uiop:string-prefix-p "file://" uri))))

(defun span-target-uri (target)
  "The URI for a fact's TARGET — (:PATH . STRING) or (:URL . STRING), the
shape tool-summary.lisp hands a title — or NIL."
  ;; A fact names its own address, so a path here is taken at its word; only
  ;; prose has to look like one first.
  (when (consp target)
    (case (car target)
      (:path (file-uri (cdr target)))
      (:url (and (web-url-p (cdr target)) (cdr target))))))

(defun path-shaped-p (text &key in-code)
  "Whether TEXT reads as a path rather than as prose: no whitespace and no
shell metacharacter in it, no scheme, not a run of digits and separators (a
date, a fraction), and either an anchor a path alone has (`/', `~/', `./',
`../', a drive letter) or a relative path whose last segment carries a name
and an extension."
  ;; `src/tui/ui.lisp' is one and `and/or' is not, which is the whole point of
  ;; the rule.
  ;;
  ;; IN-CODE drops the extension requirement for a run the answer put in
  ;; backticks. Inside code marks, `src/tui' is a folder and nothing else, and
  ;; the marks are the author's own — the middle rung between a fact, which
  ;; names its own address, and bare prose, where only an anchor will do.
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (and (> (length text) 1)
         (not (search "://" text))
         (not (ppcre:scan "[ \\t\\n\\r*?\\[\\]{}|<>\"'`$;&=]" text))
         (find-if #'alpha-char-p text)
         (or (absolute-path-p text)
             (ppcre:scan "^(?:~|\\.\\.?)/" text)
             ;; A relative path names a file (a name, a dot, an extension),
             ;; ends at a folder, or stands inside code marks. Without that
             ;; `and/or' would paint as something to click.
             (and (find #\/ text)
                  (or in-code (ppcre:scan "/(?:[^./]+\\.[^/]+)?\\z" text))))
         t)))

(defun path-target-uri (text &key in-code)
  "The file: URI for TEXT when TEXT reads as a path, else NIL — what a run
of prose or of inline code carries as its target."
  (and (path-shaped-p text :in-code in-code)
       (multiple-value-bind (path place) (cited-place text)
         (file-uri path place))))

(defun markdown-target-uri (destination)
  "An explicit Markdown DESTINATION as an openable URI, or NIL."
  ;; Unlike a bare prose run, an explicit destination names its target:
  ;; report.md and paths with spaces need no PATH-SHAPED-P heuristic.
  ;; Normalize once here so OSC8 and the mouse read exactly the same URI.
  ;; A literal U+FEFF at the Markdown boundary is an invisible formatting
  ;; mark, not a relative-path prefix. Trim before wrapper/scheme/root checks,
  ;; and again inside angle wrappers. Do NOT trim after percent decoding:
  ;; %EF%BB%BF and an interior U+FEFF can name real filename characters.
  (let* ((boundary (list #\Space #\Tab #\Newline #\Return (code-char #xfeff)))
         (target (string-trim boundary (or destination "")))
         (length (length target)))
    (when (and (> length 1) (char= #\< (char target 0))
               (char= #\> (char target (1- length))))
      (setf target (string-trim boundary (subseq target 1 (1- length)))))
    (cond ((openable-target-p target) target)
          ((or (zerop (length target)) (char= #\# (char target 0))
               (and (ppcre:scan "^[A-Za-z][A-Za-z0-9+.-]*:" target)
                    (not (absolute-path-p target))))
           nil)
          ;; Bad percent-encoded UTF-8 is inert, not a renderer error.
          ;; A literal `#' is the fragment; an encoded one (%23) is the name's.
          (t (multiple-value-bind (path place) (cited-place target)
               (ignore-errors (file-uri (percent-decode path) place)))))))

(defun bare-run-end (text pos floor)
  "The end of the bare run opening at POS in TEXT: whitespace, a quote, an
angle bracket or a backquote closes it, less what a sentence puts after a
run — a trailing . , : ; ! ? or ' and a closing ), ] or } the run itself did
not open."
  ;; Never trimmed past FLOOR, the first index that could still end a run
  ;; worth having.
  (let ((end (or (ppcre:scan "[ \\t\\n\\r\"<>`]" text :start pos) (length text))))
    (loop while (> end floor)
          do (let* ((ch (char text (1- end)))
                    (opener (case ch (#\) #\() (#\] #\[) (#\} #\{))))
               (if (or (find ch ".,:;!?'")
                       (and opener
                            (> (count ch text :start pos :end end)
                               (count opener text :start pos :end end))))
                   (decf end)
                   (return))))
    end))

(defun bare-path-end (text pos start)
  "The end of the bare path anchored at POS in TEXT, or NIL when none opens
there."
  ;; Only an anchor a path alone has opens one — `/', `~/', `./', `../' — and
  ;; only at a word's start, so a slash inside a word, a date or a fraction is
  ;; never a path. The run ends where BARE-RUN-END says it does.
  (when (and (or (= pos start) (not (alphanumericp (char text (1- pos)))))
             (ppcre:scan "^(?:~|\\.\\.?)?/" text :start pos))
    (let ((end (bare-run-end text pos (1+ pos))))
      (and (> end (1+ pos))
           (path-shaped-p (subseq text pos end))
           end))))

