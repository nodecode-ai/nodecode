;;;; markdown.lisp --- markdown classification: the Zig render_plan.zig subset. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; Fences, headings, thematic breaks, list markers, blockquotes, and the
;;; inline pass (code/strong/emphasis/strike). A list's marker bytes stay in
;;; the text, in the keyword tone; the other blocks draw what their markers
;;; say (a heading its text alone, a quote a bar per level, a task its
;;; box); inline markers are hidden, and a fence's marker lines are not
;;; drawn at all. Fence bodies carry syntax-token spans from the theme.lisp
;;; tokenizer; a fence's language is what its body is tokenized as (```mermaid
;;; included — it renders as the plain code block).
;;; The block markers are read by anchored CL-PPCRE patterns, compiled once.

(defun fence-open-info (line)
  "When LINE opens a fenced code block: (marker-char run-length language
indent), else NIL."
  ;; The language is the info's first word, a `{...' attribute read from past
  ;; its first dot and a leading `.' dropped (`{.python}', `.python'). INDENT
  ;; is the spaces before the marker run.
  (ppcre:register-groups-bind (spaces run info) ("^( {0,3})(`{3,}|~{3,})(.*)" line)
    (let ((info (ppcre:regex-replace "^(?:\\{[^.]*\\.|\\.)" (nlk:trimmed info) "")))
      (list (char run 0) (length run) (ppcre:scan-to-strings "^[^ \\t\\r\\n\\f,}\\[\\]]*" info)
            (length spaces)))))

(defun fence-close-p (line fence)
  "Close requires the same marker char, a run at least as long, blank rest."
  (ppcre:register-groups-bind (run) ("^ {0,3}(`+|~+) *\\z" line)
    (and (char= (char run 0) (first fence)) (>= (length run) (second fence)))))

(defun heading-marker-end (line)
  "When LINE is an ATX heading: (values marker-end level), else NIL."
  ;; The marker is the #-run plus its trailing space; the heading's text
  ;; starts past it.
  (multiple-value-bind (start end) (ppcre:scan "^#{1,6}(?: |\\z)" line)
    (and start (values end (count #\# line :end end)))))

(defun list-marker-end (line)
  "Character end of a list-marker prefix (≤8 space indent, bullet or 1-9
digits + ./) + space, optional task box), else NIL. Becomes the hang indent.
Second value: whether the item is a task, :done for `[x]' and :open for `[ ]'."
  ;; ASCII digits, as the Zig renderer and CommonMark read an ordered marker.
  (multiple-value-bind (start end boxes) (ppcre:scan "^ {0,8}(?:[-*+]|\\d{1,9}[.)]) (\\[[ xX]\\] )?" line)
    (and start (values end (let ((box (aref boxes 0)))
                             (and box (if (char= #\Space (char line (1+ box))) :open :done)))))))

(defun blockquote-prefix-end (line)
  "End of a blockquote prefix: runs of > each optionally followed by a space."
  (nth-value 1 (ppcre:scan "^ {0,3}(?:> ?)+" line)))

(defun bare-url-end (text pos start)
  "The end of the bare http(s) URL at POS in TEXT, or NIL when none opens
there."
  ;; The scheme must open a word — POS is START, or the character before it is
  ;; not alphanumeric — and the URL runs to where BARE-RUN-END says it does,
  ;; never trimmed back into its own scheme.
  (let ((scheme-end (nth-value 1 (ppcre:scan "^(?i)https?://" text :start pos))))
    (when (and scheme-end
               (or (= pos start)
                   (not (alphanumericp (char text (1- pos))))))
      (let ((end (bare-run-end text pos scheme-end)))
        (and (> end scheme-end) end)))))

(defun inline-color-open (text pos)
  "End and RGB of a strict foreground-only color opener at POS, else NIL."
  (multiple-value-bind (match end starts ends)
      (ppcre:scan "^<span style=\"color: *(#[0-9a-fA-F]{6})\">" text :start pos)
    (when match
      (let ((hex (subseq text (aref starts 0) (aref ends 0))))
        (values end (loop for at in '(1 3 5)
                          collect (parse-integer hex :start at :end (+ at 2) :radix 16)))))))

(defun inline-color-run (text pos &aux (depth 1))
  "A complete single-line color region: (body-start body-end end RGB), else NIL.
Nested regions balance; escapes and inline code cannot close a region."
  ;; The body reads as tokens -- an escape, a code span, a closer, an opener,
  ;; any one character -- so a closer inside the first two never counts.
  (multiple-value-bind (body-start rgb) (inline-color-open text pos)
    (when body-start
      (ppcre:do-matches (start end
                         "(?s)\\\\.?|`[^`]+`|</span>|<span style=\"color: *#[0-9a-fA-F]{6}\">|."
                         text nil :start body-start)
        (cond ((prefix-at-p text start "</span>")
               (when (zerop (decf depth))
                 (return-from inline-color-run (list body-start start end rgb))))
              ((and (char= #\< (char text start)) (> end (1+ start)) (> (incf depth) 16))
               (return-from inline-color-run nil)))))))

(defun compact-inline-text (text spans &aux (hidden (remove-if-not #'mspan-hidden spans)))
  "TEXT and SPANS with hidden markers removed before measuring or attaching metadata."
  (flet ((shift (offset)
           (- offset (loop for span in hidden sum (max 0 (- (min offset span.end) span.start))))))
    (values
     (with-output-to-string (out)
       (loop for from = 0 then (mspan-end span)
             for span in hidden
             do (write-string text out :start from :end (mspan-start span))
             finally (write-string text out :start from)))
     (loop for span in spans
           unless (mspan-hidden span)
             collect (copy-with mspan span :start (shift span.start) :end (shift span.end))))))

(defun code-spans (text start end &aux (body (subseq text start end))
                                       (quiet (make-array (length body) :element-type 'bit
                                                                        :initial-element 0)))
  "The spans over the inline code run [START,END) of TEXT, two tones of the
cool hue: an id all quiet, a path's folders quiet under its last segment,
a name bright but for its package and its punctuation. Each carries the
file the run names, when it names one (PATH-TARGET-URI)."
  ;; The bright part is what a reader looks for -- the file, the function --
  ;; and the rest stands back.
  (let ((tone (cond ((ppcre:scan "^(?:[0-9a-f]{7,40}|\\d{3,}|[0-9A-Z]{8,}(?:/[0-9a-f]+…?)?)\\z" body)
                     (fill quiet 1)
                     :inline-code)
                    ((and (not (find-if #'blank-char-p body))
                          (or (ppcre:scan "^(?:~|\\.\\.?/)|[^/]/" body)
                              (ppcre:scan "\\.(?:lisp|asd|py|json|jsonc|md|js|ts|sh|txt|toml|ya?ml|rs|go|zig|c|h|html|css)\\z"
                                          body)))
                     (let* ((last (if (alexandria:ends-with #\/ body) (1- (length body)) (length body)))
                            (cut (position #\/ body :end last :from-end t)))
                       (when cut (fill quiet 1 :end (1+ cut)))
                       (when (< last (length body)) (setf (sbit quiet last) 1)))
                     :code-path)
                    (t (fill quiet 1 :end (or (let ((at (search "::" body :from-end t))) (and at (+ at 2)))
                                              (and (ppcre:scan "^[a-z][a-z0-9.-]*:[^:\\s]" body)
                                                   (1+ (position #\: body)))
                                              0))
                       (loop for i from 0 below (length body)
                             when (find (char body i) "*+{},…") do (setf (sbit quiet i) 1))
                       :inline-code)))
        (link (path-target-uri body :in-code t)))
    (loop with spans = '()
          for i from 0 below (length body)
          for piece = (if (= 1 (sbit quiet i)) :code-quiet tone)
          do (if (and spans (eq piece (mspan-tone (first spans))))
                 (setf (mspan-end (first spans)) (+ start i 1))
                 (push (mspan (+ start i) (+ start i 1) :tone piece :link link) spans))
          finally (return (nreverse spans)))))

(defun inline-spans (text start &optional (color-depth 0) &aux (spans '())
                                     (pos start)
                                     (len (length text)))
  "Inline markdown spans from START: `code`, **strong**, *em* and _em_,
***both***, ~~strike~~, backslash escapes, [text](url) links, bare http(s)
URLs, paths and strict <span style=\"color:#RRGGBB\"> regions."
  ;; Markers hide; code and strong take their palette tones; emphasis is
  ;; italic only, and _ never toggles intra-word (Zig). A link paints in the
  ;; link tone, underlined, and carries its target (MSPAN-LINK) for the OSC-8
  ;; wrap and the click that opens it; a path carries the same target and no
  ;; styling of its own, since a page is a place to go and a path is just the
  ;; file being talked about.
  ;; A backslash before ASCII punctuation (CommonMark) hides and the character
  ;; after it prints as itself, never opening or closing a mark: \* \_ \` \|
  ;; \\ \# read as the one character. Without this, the backslash would print
  ;; and the mark would style what follows. Inside `code' a backslash is
  ;; literal: a code run's text is kept whole (CODE-SPANS).
  (labels ((escape-p (at end)
             (and (< (1+ at) end)
                  (char= (char text at) #\\)
                  (find (char text (1+ at)) "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")))
           (paired (open-len close-pos close-len &rest style)
             ;; The run's STYLE is laid in pieces around the backslashes of
             ;; its own escapes, which hide.
             (push (mspan pos (+ pos open-len) :hidden t) spans)
             (let* ((piece (+ pos open-len))
                    (inner (and (< color-depth 16)
                                (search "<span style=" text :start2 piece :end2 close-pos)
                                (inline-spans (subseq text piece close-pos) 0 (1+ color-depth))))
                    (colored (some (lambda (span) (visual-style-p (mspan-tone span))) inner)))
               (cond (colored
                      (lay inner piece close-pos style
                           (lambda (moved)
                             (setf moved.tone (or moved.tone (getf style :tone))
                                   moved.ground (or moved.ground (getf style :ground))
                                   moved.bold (or moved.bold (getf style :bold))
                                   moved.italic (or moved.italic (getf style :italic))
                                   moved.strike (or moved.strike (getf style :strike))
                                   moved.underline (or moved.underline (getf style :underline))
                                   moved.link (or moved.link (getf style :link))))))
                     (t (loop for at from piece below close-pos
                              when (and (>= at piece) (escape-p at close-pos))
                                do (when (< piece at)
                                     (push (apply #'mspan piece at style) spans))
                                   (push (mspan at (1+ at) :hidden t) spans)
                                   (setf piece (+ at 2)))
                        (when (< piece close-pos)
                          (push (apply #'mspan piece close-pos style) spans)))))
             (push (mspan close-pos (+ close-pos close-len) :hidden t) spans)
             (setf pos (+ close-pos close-len)))
           (colored (run)
             (destructuring-bind (body-start body-end end rgb) run
               (let ((tone (vstyle :fg rgb)))
                 (push (mspan pos body-start :hidden t :tone tone) spans)
                 (lay (inline-spans (subseq text body-start body-end) 0 (1+ color-depth))
                      body-start body-end (list :tone tone)
                      (lambda (moved) (unless (visual-style-p moved.tone) (setf moved.tone tone))))
                 (push (mspan body-end end :hidden t) spans)
                 (setf pos end))))
           (lay (inner base end style adopt &aux (at 0))
             ;; INNER's spans moved on to BASE, ADOPT styling each one that is
             ;; not hidden, and the gaps between them up to END laid in STYLE.
             (dolist (span inner)
               (when (< at span.start)
                 (push (apply #'mspan (+ base at) (+ base span.start) style) spans))
               (let ((moved (shifted-span span base)))
                 (unless moved.hidden (funcall adopt moved))
                 (push moved spans))
               (setf at span.end))
             (when (< (+ base at) end)
               (push (apply #'mspan (+ base at) end style) spans)))
           (fenced (marker &rest style)
             ;; MARKER, then a non-empty run up to the next MARKER no backslash
             ;; escapes; else one char on.
             (let ((close (loop for at = (search marker text :start2 (+ pos (length marker)))
                                  then (search marker text :start2 (1+ at))
                                while (and at (char= (char text (1- at)) #\\))
                                finally (return at))))
               (if (and close (> close (+ pos (length marker))))
                   (apply #'paired (length marker) close (length marker) style)
                   (incf pos)))))
    (loop while (< pos len)
          do (let* ((ch (char text pos))
                    (color-run (and (< color-depth 16) (char= ch #\<)
                                    (inline-color-run text pos))))
               (cond
                 ((escape-p pos len)
                  (push (mspan pos (1+ pos) :hidden t) spans)
                  (incf pos 2))
                 (color-run (colored color-run))
                 ((char= ch #\`)
                  ;; Inline code is where an answer names a file —
                  ;; `src/tui/ui.lisp' — so the run carries that path as
                  ;; its target when it reads as one (CODE-SPANS). The
                  ;; span a reader already sees as a file is the span a
                  ;; click opens; no second scan, no second rule.
                  (let ((close (position #\` text :start (1+ pos))))
                    (cond ((and close (> close (1+ pos)))
                           (push (mspan pos (1+ pos) :hidden t) spans)
                           (dolist (span (code-spans text (1+ pos) close))
                             (push span spans))
                           (push (mspan close (1+ close) :hidden t) spans)
                           (setf pos (1+ close)))
                          (t (incf pos)))))
                 ;; ***both*** is strong and emphasis at once; without this
                 ;; it would read as ** and leave a stray * on each side of
                 ;; a bold run. A strong run stands on a chip.
                 ((and (prefix-at-p text pos "***")
                       (search "***" text :start2 (+ pos 3)))
                  (fenced "***" :tone :inline-strong :bold t :italic t :ground :chip))
                 ((prefix-at-p text pos "**")
                  (fenced "**" :tone :inline-strong :bold t :ground :chip))
                 ((char= ch #\*) (fenced "*" :italic t))
                 ;; Struck through, the web page's <del>; without this, the
                 ;; tildes would print.
                 ((prefix-at-p text pos "~~") (fenced "~~" :strike t))
                 ((and (char= ch #\_)
                       (or (= pos start)
                           (not (alphanumericp (char text (1- pos))))))
                  (let ((close (position #\_ text :start (1+ pos))))
                    (if (and close (> close (1+ pos))
                             (or (= (1+ close) len)
                                 (not (alphanumericp (char text (1+ close))))))
                        (paired 1 close 1 :italic t)
                        (incf pos))))
                 ((char= ch #\[)
                  ;; [text](url): the text underlines in the link tone,
                  ;; carries the url as an OSC-8 hyperlink (zero width —
                  ;; the layout math never sees it), and the raw url hides.
                  (multiple-value-bind (match end url)
                      (ppcre:scan "^\\[[^\\]]+\\]\\(([^)]*)\\)" text :start pos)
                    (if match
                        (let ((close (- (aref url 0) 2)))
                          ;; An image, ![alt](url), reads as its alt text.
                          (when (and (> pos start) (char= #\! (char text (1- pos))))
                            (push (mspan (1- pos) pos :hidden t) spans))
                          (paired 1 close (- end close)
                                  :tone :link :underline t
                                  :link (markdown-target-uri
                                         (subseq text (aref url 0) (1- end)))))
                        (incf pos))))
                 (t
                  (let ((end (and (char-equal ch #\h)
                                  (bare-url-end text pos start)))
                        (path-end (and (find ch "/~.") (bare-path-end text pos start))))
                    (cond (end
                           (push (mspan pos end :tone :link :underline t
                                        :link (subseq text pos end))
                                 spans)
                           (setf pos end))
                          ;; A bare path keeps the prose's own colour: a
                          ;; transcript naming files everywhere would read
                          ;; as a link farm if each one underlined. What
                          ;; marks it is the pointer (*HOVER-TARGET*).
                          (path-end
                           (when-let (uri (path-target-uri (subseq text pos path-end)))
                             (push (mspan pos path-end :link uri) spans))
                           (setf pos path-end))
                          (t (incf pos)))))))))
  (nreverse spans))

(defun token-spans (tokens)
  "Styled spans for syntax TOKENS."
  ;; Plain runs carry no span, so they paint in the line's base style; a
  ;; dimmed base dims the token colors through the span pipeline exactly as
  ;; the Zig dimColor pass does.
  (loop for token in tokens
        for tone = (token-tone token.kind)
        when tone
          collect (mspan token.start token.end
                         :tone tone)))

(defun command-token-spans (command)
  "Syntax spans for an executable command."
  ;; Both completed tool headers and the live background band use this pass so
  ;; a call keeps the same syntax colors when it moves between surfaces.
  (token-spans (tokenize-shell command)))

(defun table-row-cells (line &aux (text (string-trim " " line)) (cells (list '())) (closed nil))
  "Split one | row into trimmed cell strings, outer pipes stripped. A pipe
escaped as \\| or standing inside a `code span' stays in its cell, the
escape's backslash dropped (GFM, which drops it inside code too)."
  ;; Without this, a | inside an escaped pair or a closed code span would be
  ;; read as a cell boundary and push the row's last cells off the grid.
  ;; Any other escaped pair passes on whole for the inline pass, so an escaped
  ;; backtick never opens a code span here either. The row reads as tokens --
  ;; an escaped pair, a closed code span, a run of plain text, any one
  ;; character -- and only a bare | token parts cells.
  (ppcre:do-matches-as-strings (token "\\\\.|`[^`]*`|[^\\\\`|]+|." text)
    (if (setf closed (string= token "|"))
        (push '() cells)
        (push token (first cells))))
  ;; The outer pipes open and close the row rather than part cells: a leading
  ;; one leaves an empty first cell, a closing one an empty last. An empty row
  ;; still has its one empty cell.
  (when closed
    (pop cells))
  (let ((cells (nreverse cells)))
    (when (and (plusp (length text)) (char= (char text 0) #\|))
      (pop cells))
    (mapcar (lambda (tokens)
              (string-trim " " (ppcre:regex-replace-all
                                "\\\\\\|" (apply #'concatenate 'string (reverse tokens)) "|")))
            (or cells (list '())))))

(defun table-cell (cell)
  "CELL as the grid paints it: (TEXT . SPANS), hidden markers removed."
  (multiple-value-bind (text spans) (compact-inline-text cell (inline-spans cell 0))
    (cons text spans)))

(defparameter *min-table-key-value-width* 8
  "Minimum value-column budget for transposed key/value table records (Zig
min_table_key_value_width).")

(defun table-column-widths (all-rows &optional columns)
  "The display width of each markdown table column over ALL-ROWS."
  ;; COLUMNS caps how many columns are measured — the rendered grid keeps only
  ;; the header's columns — and defaults to the widest row's.
  (loop for column below (or columns (reduce #'max all-rows :key #'length))
        collect (loop for cells in all-rows
                      maximize (text-display-width (or (nth column cells) "")))))

(defun pack-lines (items width &optional (joiner " ") &aux (lines '()) (current nil))
  "ITEMS joined by JOINER into lines of at most WIDTH display columns,
greedily and in order; an item wider than WIDTH stands alone on its line."
  ;; The Zig emitMarkdownTableKeyValueRow tokenize-and-fill, and the home
  ;; screen's cell lane: NIL for no items.
  (dolist (item items)
    (let ((candidate (and current (concatenate 'string current joiner item))))
      (if (and candidate (> (text-display-width candidate) width))
          (setf lines (cons current lines) current item)
          (setf current (or candidate item)))))
  (nreverse (if current (cons current lines) lines)))

(defun table-separator-p (line)
  "Whether LINE is the separator row under a table's header: only | - : and
spaces, a | and a - among them, three characters or more between its outer
spaces."
  (ppcre:scan "^(?=.*\\|)(?=.*-) *[|:-][|:\\- ]+[|:-] *\\z" line))

(defun settled-line-count (lines &aux (count (length lines)))
  "How many of LINES, the lines of a message still arriving, are drawn yet:
all but the pipe rows it ends on when no separator has made them a table
yet, or, once one has, all but the last row."
  ;; A table lays its columns out over every row it holds, so a row drawn
  ;; while its cells arrived widened them token by token and the whole table
  ;; jumped. Holding back what is still arriving lays the table
  ;; out once per row: a row lands whole when the line after it begins, and a
  ;; header waits for its separator instead of showing first as a line of
  ;; pipes. Only a line opening with a pipe waits, so prose never stalls; the
  ;; separator may stand just above those lines, written without an outer
  ;; pipe.
  (let* ((run (1+ (or (position-if-not (lambda (line) (ppcre:scan "^ {0,3}\\|" line))
                                       lines :from-end t)
                      -1)))
         (separator (position-if #'table-separator-p lines :start (max 0 (1- run)))))
    (cond ((= run count) count)
          ((null separator) run)
          ((= separator (1- count)) count)
          (t (1- count)))))

(defun table-alignments (separator-line)
  "Each column's alignment as SEPARATOR-LINE, the |---| row under a table's
header, says it: :CENTER for a colon at both ends of its cell, :RIGHT for one
at the end, else :LEFT (GFM, md.js's ALIGNMENTS)."
  (mapcar (lambda (cell)
            (cond ((ppcre:scan "^:.*:\\z" cell) :center)
                  ((ppcre:scan ":\\z" cell) :right)
                  (t :left)))
          (table-row-cells separator-line)))

(defun markdown-table-llines (header-line separator-line row-lines
                              &key content-width)
  "An aligned markdown table: padded columns behind │ separators, the
header bold on a heading's band, each column standing left, right or
centred as SEPARATOR-LINE says."
  ;; When CONTENT-WIDTH is supplied and the padded grid overflows it (the Zig
  ;; markdownTableColumnsForBlock gate), the table transposes to key/value
  ;; records instead.
  ;; A cell is (TEXT . SPANS) (TABLE-CELL); ALL is every row's texts.
  ;; Without this, the alignment row's colons would be read as nothing, so a
  ;; column of figures marked --: would stand ragged at the left like prose.
  (let* ((header (mapcar #'table-cell (table-row-cells header-line)))
         (rows (mapcar (lambda (line) (mapcar #'table-cell (table-row-cells line)))
                       row-lines))
         (all (mapcar (lambda (cells) (mapcar #'car cells)) (cons header rows))))
    (if (and content-width
             (not (and (every (lambda (cells) (<= (length cells) 12)) all)
                       (let ((widths (table-column-widths all)))
                         (<= (+ 1 (reduce #'+ widths) (* 3 (length widths)))
                             content-width)))))
        ;; Key/value records (Zig emitMarkdownTableKeyValueRow), a dim rule
        ;; between; their values are the cells' texts, unstyled.
        (let* ((label-width (loop for label in (first all) maximize (text-display-width label)))
               (value-indent (min (+ label-width 3)
                                  (max 0 (- content-width
                                            *min-table-key-value-width*))))
               (value-width (max (- content-width value-indent)
                                 *min-table-key-value-width*)))
          (loop for cells in (rest all)
                for first-record = t then nil
                unless first-record
                  collect (make-lline :text "" :tone :markdown-line :rule t)
                nconc (loop for label in (first all)
                            for i from 0
                            for words = (split-words (nth i cells))
                            nconc (loop for fragment in (or (pack-lines words value-width)
                                                            (list ""))
                                        for labeled = t then nil
                                        for lead = (if labeled (concatenate 'string " " label) "")
                                        for pad = (max 0 (- value-indent (text-display-width lead)))
                                        for text = (format nil "~a~va~a" lead pad "" fragment)
                                        for bold = (and labeled (mspan 1 (length lead) :bold t))
                                        collect (make-lline :text (string-right-trim " " text)
                                                            :spans (and bold (list bold)))))))
        (let ((widths (table-column-widths all (length header)))
              (alignments (table-alignments separator-line)))
          ;; One grid row: each column's cell padded to its width with FILL on
          ;; the side its alignment leaves open (both, split, for a centred
          ;; one), the columns joined by JOINT in the lines' tone and each
          ;; cell's spans moved to where its text lands.
          (flet ((grid-lline (cells joint fill &rest style &aux (spans '()))
                   (apply #'make-lline
                          :text (with-output-to-string (out)
                                  (loop for width in widths
                                        for column from 0
                                        for (text . cell-spans) = (or (pop cells) '(""))
                                        for pad = (- width (text-display-width text))
                                        for before = (case (nth column alignments)
                                                       (:right pad)
                                                       (:center (floor pad 2))
                                                       (t 0))
                                        for first = t then nil
                                        do (unless first
                                             (push (mspan (file-position out)
                                                          (+ (file-position out) (length joint))
                                                          :tone :markdown-line)
                                                   spans)
                                             (write-string joint out))
                                           (format out "~v,,,v@a" before fill "")
                                           ;; The stream's position is where the cell's text lands.
                                           (dolist (span cell-spans)
                                             (push (shifted-span span (file-position out)) spans))
                                           (format out "~a~v,,,v@a" text (- pad before) fill "")))
                          :spans (nreverse spans)
                          :grid t
                          style)))
            ;; The header stands on a heading's band, which says header where
            ;; a ─┼─ rule under it would.
            (let ((head (grid-lline header " │ " #\Space :tone :heading :bold t)))
              (setf (lline-ground head) (list :heading-band 3 (+ 3 (text-display-width (lline-text head))) nil))
              (cons head (mapcar (lambda (cells)
                                   (grid-lline cells " │ " #\Space))
                                 rows))))))))

(defun lead-spans (line spans)
  "SPANS, the inline spans of paragraph LINE, with the strong run LINE
opens on made its lead -- the heading's tone, on no chip -- and the lead's
width as a second value, NIL when LINE opens on no strong run."
  (values spans
          (when (and (prefix-at-p line 0 "**") (not (prefix-at-p line 0 "***")))
            (loop with width = 0
                  for at = 0 then span.end
                  for span in spans
                  while (and (= span.start at) (or span.hidden (eq span.tone :inline-strong)))
                  unless span.hidden
                    do (setf span.tone :heading span.ground nil)
                       (incf width (text-display-width (subseq line span.start span.end)))
                  finally (return (and (plusp width) width))))))

(defun markdown-llines (text &key shadow width arriving)
  "Classify TEXT's logical lines as the Zig transcript does for assistant and
reasoning bodies."
  ;; Every body word-wraps — a word the row cannot hold moves
  ;; whole to the next row — a fence skips the inline pass and carries
  ;; syntax-token spans for the fence's language. Pipe tables
  ;; (a | header over a |---| separator) render aligned; ARRIVING says TEXT
  ;; is a message still streaming, whose table rows wait until they are
  ;; whole (SETTLED-LINE-COUNT). SHADOW marks
  ;; every lline for the shadow (the reasoning block): the same lines in the same
  ;; tones, the text painted at less light on the same canvas.
  ;; Second and third values: the character offset of the last top-level blank
  ;; line (the incremental-reparse checkpoint — nothing before it can re-render
  ;; differently once more text is appended, because no construct in this
  ;; dialect looks across a blank line outside a fence) and the count of llines
  ;; emitted before that line; both NIL without a usable checkpoint.
  ;; Empty TEXT classifies to no llines at all, SPLIT-LINES' policy: a body with
  ;; no content contributes nothing, not a blank line the block padding would
  ;; then pad on both sides.
  (let* ((fence nil)
         (slab-ended nil)
         (lines (coerce (split-lines text) 'vector))
         (line-count (if arriving (settled-line-count lines) (length lines)))
         (index 0)
         (result '())
         (cp-line nil)
         (cp-tail nil)
         ;; The prose body width (Zig transcriptBodyWidthForLine: the frame minus
         ;; the 3-column prose indent and the right padding column).
         (content-width (and width (max 1 (- width (nth-value 2 (body-gutter :prose))
                                              *transcript-right-padding*)))))
   (flet ((emit (&rest lline-args)
            (push (apply #'make-lline lline-args) result))
          (slab (&rest lline-args)
            ;; A code block's row: on the slab from its edge to the text's
            ;; right one, a wrapped line going on under the edge and a ↦.
            (push (apply #'make-lline :role :code :gutter-tone :markdown-bar :ground '(:slab 3 :edge nil)
                         :hang 2 :hang-mark (list (make-seg :text "▌" :tone :markdown-bar)
                                                  (make-seg :text "↦" :tone :muted))
                         lline-args)
                  result))
          (gap ()
            ;; A code block stands a blank row off whatever touches it --
            ;; prose, a heading, a list, another block -- without a blank
            ;; line between them, its ground would meet the line above it
            ;; edge to edge. A blank already there is the gap, and a block
            ;; opening the body has the body's padding.
            (let ((last (first result)))
              (when (and last (or (lline-rule last) (not (eq :prose (lline-role last)))
                                  (position-if-not #'blank-char-p (lline-text last))))
                (push (make-lline :text "") result))))
          (prose (text start &aux (spans (inline-spans text start)) (at (1- (length text))))
            ;; The inline pass, and CommonMark's hard break: an unescaped
            ;; backslash closing a line the paragraph goes on after hides, the
            ;; row ending there anyway. A paragraph's last line keeps its
            ;; backslash, as CommonMark has it.
            (if (and (>= at start)
                     (< index line-count)
                     (position-if-not #'blank-char-p (aref lines index))
                     (oddp (- at (or (position #\\ text :test-not #'char= :from-end t) -1)))
                     (notany (lambda (span) (> span.end at)) spans))
                (append spans (list (mspan at (1+ at) :hidden t)))
                spans)))
    (loop while (< index line-count)
     do
     (let ((line (aref lines index)))
       ;; The line after a code block's closing fence stands off it (GAP).
       (when (and (shiftf slab-ended nil) (position-if-not #'blank-char-p line))
         (gap))
       ;; Checkpoint: a blank line reached by this loop with no open fence,
       ;; and fence bodies never get here with FENCE set, so a blank recorded
       ;; here is a true top-level block boundary.
       (when (and (not fence) (zerop (length line)))
         (setf cp-line index
               cp-tail result))
       (incf index)
       (if (and (not fence)
                (find #\| line)
                (< index line-count)
                (table-separator-p (aref lines index))
                (>= (length (table-row-cells line)) 2))
           ;; A table block: header, separator, then every following | row.
           (let ((separator (aref lines index))
                 (rows (loop for i from (1+ index) below line-count
                             while (find #\| (aref lines i)) collect (aref lines i))))
             (incf index (1+ (length rows)))
             (setf result (revappend (markdown-table-llines line separator rows
                                                             :content-width content-width)
                                     result)))
        (cond
         ;; A fence's body stands on a slab, a ground a step off the canvas
         ;; with an edge in the gutter, its first row the language and its
         ;; last a blank: with only an indent saying code, a block would read
         ;; as prose. A ```diff body takes the diff tones by its lines' first
         ;; column past the fence's own indent.
         ((and fence (not (fence-close-p line fence)))
          (bind (((_ _ language indent) (values-list fence))
                 (mark (subseq line (min indent (or (position #\Space line :test-not #'char=)
                                                    (length line))))))
            (if (member language '("diff" "patch") :test #'string-equal)
                (slab :text line
                      :tone (cond ((some (lambda (head) (prefix-at-p mark 0 head))
                                         '("@@" "+++" "---"))
                                   :muted)
                                  ((prefix-at-p mark 0 "+") :diff-added)
                                  ((prefix-at-p mark 0 "-") :diff-removed)
                                  (t :foreground)))
                (slab :text line :spans (token-spans (tokenize-line line language))))))
         ;; The markers hide (the literal-code rule): a closing line clears
         ;; FENCE and is the slab's blank last row, an opening line sets it
         ;; and is its first, the language quiet at the fence's indent; each
         ;; stands a blank row off what touches the block (GAP).
         ((shiftf fence nil) (slab :text "") (setf slab-ended t))
         ((setf fence (fence-open-info line))
          (gap)
          (bind (((_ _ language indent) (values-list fence)))
            (slab :text (if (plusp (length language)) (format nil "~va~a" indent "" language) "")
                  :tone :muted)))
         ;; A thematic break: at most 3 spaces indent, then 3+ of one of -*_ and spaces.
         ((ppcre:scan "^ {0,3}([-*_])(?: *\\1){2,} *\\z" line)
          (emit :text line :tone :markdown-line :rule t))
         ;; A heading draws its text alone, its #s hidden (a closing run
         ;; too). # and ## stand on a band of the cool hue from the gutter,
         ;; solid under their words and going out into the canvas after them,
         ;; # on the deeper band and underlined; ### and deeper are their
         ;; words alone in the cool hue, ### bold. Every level drew bold behind
         ;; its hashes, and in the prose's colour until the D board.
         ;; A thought's stand on no band.
         ((heading-marker-end line)
          (multiple-value-bind (marker-end level) (heading-marker-end line)
            (let ((text (ppcre:regex-replace "(?:^| +)#+ *\\z" (subseq line marker-end) ""))
                  (banded (and (< level 3) (not shadow))))
              (emit :text text :tone (if banded :heading :subheading)
                    :bold (< level 4) :underline (= level 1)
                    :role (if banded :heading :prose) :gutter-tone :subheading
                    :hang (if banded 2 0) :hang-mark (and banded (make-seg :text "▌" :tone :subheading))
                    :ground (and banded (list (if (= level 1) :title-band :heading-band) 1 :text 36))
                    :spans (inline-spans text 0)))))
         ((list-marker-end line)
          (multiple-value-bind (end task) (list-marker-end line)
            (let ((indent (position #\Space line :test-not #'char=)))
              (cond
                ;; A task draws a box where its marker stood, □ open in the
                ;; marks' tone and ▣ done in the lines', its text dim: the web
                ;; page's checkbox, Codex's ☐ ☑ being no glyphs of Noto Sans
                ;; Mono. Without this, a task would draw `- [x]' as typed.
                (task
                 (let* ((done (eq task :done))
                        (head (format nil "~va~a " indent "" (if done "▣" "□")))
                        (text (concatenate 'string head (subseq line end))))
                   (emit :text text :dim done
                         :hang (min (length head) *max-markdown-hang*)
                         :spans (cons (mspan indent (1+ indent) :tone (if done :markdown-line :list-mark)
                                                                :bold (not done))
                                      (prose text (length head))))))
                ;; A number stands right-aligned to the widest of its list
                ;; (the :ORDINAL inset, set below), so a list reaching 10
                ;; keeps its words in one column.
                ((digit-char-p (char line indent))
                 (emit :text line :role (cons :ordinal 0)
                       :hang (min end *max-markdown-hang*)
                       :spans (cons (mspan indent (position #\Space line :start indent) :tone :list-mark)
                                    (prose line end))))
                ;; A bullet is a dot where its marker stood, hollow nested.
                (t (let ((text (concatenate 'string (subseq line 0 indent) (if (< indent 2) "•" "◦")
                                            (subseq line (1+ indent)))))
                     (emit :text text :hang (min end *max-markdown-hang*)
                           :spans (cons (mspan indent (1+ indent) :tone :list-mark)
                                        (prose text end)))))))))
         ;; A quote draws a bar per level in place of its > marks and its
         ;; text in the muted tone, the web page's border and colour; the
         ;; bars run down every row a long line wraps onto.
         ((blockquote-prefix-end line)
          (let* ((end (blockquote-prefix-end line))
                 (bars (apply #'concatenate 'string
                              (make-list (count #\> line :end end) :initial-element "▌ ")))
                 (text (concatenate 'string bars (subseq line end))))
            (emit :text text :tone :muted
                  :hang (min (length bars) *max-markdown-hang*)
                  :hang-mark (make-seg :text bars :tone :markdown-bar)
                  :spans (cons (mspan 0 (length bars) :tone :markdown-bar)
                               (prose text (length bars))))))
         ;; A paragraph that opens on a strong run leads with it: the run
         ;; in the heading's tone on a heading's band (LEAD-SPANS).
         (t
          (multiple-value-bind (spans lead) (if shadow (prose line 0) (lead-spans line (prose line 0)))
            (if lead
                (emit :text line :role :heading :hang 2 :gutter-tone :subheading
                      :ground (list :heading-band 1 (+ 3 lead 2) 36) :spans spans)
                (emit :text line :spans spans)))))))))
   ;; Explicit colors carry long hidden tags. Finalize their visible text
   ;; before wrapping, memoization and EQ-keyed link/copy metadata. Other
   ;; Markdown keeps its established representation.
   (dolist (line result)
     (when (some (lambda (span) (visual-style-p (mspan-tone span))) (lline-spans line))
       (multiple-value-bind (text spans) (compact-inline-text (lline-text line) (lline-spans line))
         (setf (lline-text line) text (lline-spans line) spans))))
   (when shadow
     (dolist (line result)
       (setf line.shadow t)
       (dolist (span (lline-spans line))
         (when (eq (mspan-tone span) :inline-strong)
           (setf (mspan-tone span) :thought-strong
                 (mspan-ground span) nil)))))
   ;; A run of numbered items takes the widest number's columns at each
   ;; depth, every number right-aligned in them: its last digit and dot
   ;; stand where a bullet would, the words a column past.
   (loop with run = '()
         for line in (append result (list nil))
         for (role . nil) = (and line (consp line.role) line.role)
         do (if (eq role :ordinal)
                (let* ((text line.text)
                       (indent (position #\Space text :test-not #'char=)))
                  (push (list line indent (- (position #\Space text :start indent) indent)) run))
                (loop for (numbered indent width) in run
                      do (setf (lline-role numbered)
                               (cons :ordinal (- (loop for (nil at most) in run
                                                       when (= at indent) maximize most)
                                                 width)))
                      finally (setf run '()))))
   ;; CP-TAIL is the result list as it stood on reaching the checkpoint
   ;; line, so its length IS the head lline count.
   (let ((cp-count (and cp-line (length cp-tail))))
     (values (nreverse result)
             (and cp-line (loop for i below cp-line sum (1+ (length (aref lines i)))))
             cp-count))))

;;; --- layout: logical lines to painted rows ---------------------------------