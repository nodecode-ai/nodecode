;;;; cards.lisp --- workspace diff documents and image-result cards. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; The line-diff document over a tool call's workspace bracket. The producer
;;; (snapshot.lisp WORKSPACE-DIFF-DOCUMENT) diffs the call's pre/post capture
;;; trees and ships bounded per-file unified patches in the result's display
;;; metadata; this projection parses them back into the edit_diff.zig
;;; document shape — right-aligned numbered gutters, removed-before-added
;;; hunks (git's own ordering), word-level emphasis on index-paired lines,
;;; "..." elision rows between hunks, per-file path headers carrying the
;;; " +N -M" stats. Both the live stream and replay read the same fact, so
;;; the document renders identically on both paths.

(defstruct (diff-line (:constructor diff-line (sign number text)))
  "One diff document line: SIGN is :added, :removed, :context, or :meta;
NUMBER the file line number (NIL renders a blank number column); RANGES
the word-level emphasis (START . END) char ranges into TEXT, ascending."
  sign number (text "" :type string) (ranges '()))

(nlk:access (a diff-line) (r diff-line))

(defun diff-word-tokens (line &aux (tokens '()))
  "LINE split into alternating whitespace and word runs as (START END
WORD-P) triples — a space is ' ' or tab only (Zig tokenizeWords)."
  ;; NIL past the token cap: the pair keeps whole-line coloring.
  (ppcre:do-matches (start end "[ \\t]+|[^ \\t]+" line (nreverse tokens))
    (when (>= (length tokens) 128)      ; the Zig max_word_tokens cap
      (return-from diff-word-tokens nil))
    (push (list start end (not (member (char line start) '(#\Space #\Tab)))) tokens)))

(defun diff-word-ranges (line counterpart &aux (tokens-a (diff-word-tokens line))
                                               (tokens-b (diff-word-tokens counterpart)))
  "Word-level change ranges of LINE against its index-paired COUNTERPART:
(START . END) char ranges with byte-adjacent unmatched tokens merged into
one emphasis span."
  ;; NIL when the pair shares no common word token — the Zig
  ;; appendWordHighlights similarity gate, so fully rewritten lines keep plain
  ;; whole-line coloring instead of an all-emphasis smear.
  (when (and tokens-a tokens-b)
    (let* ((va (coerce tokens-a 'vector))
           (vb (coerce tokens-b 'vector))
           (token= (lambda (ta tb)
                     (string= line counterpart
                              :start1 (first ta) :end1 (second ta)
                              :start2 (first tb) :end2 (second tb))))
           (n (length va))
           (m (length vb))
           ;; Suffix-LCS, the Zig edit_diff shape: (aref table i j) is VA[i..] vs VB[j..].
           (table (make-array (list (1+ n) (1+ m)) :initial-element 0))
           (ranges '())
           (matched-word nil)
           (i 0)
           (j 0))
      (loop for i from (1- n) downto 0
            do (loop for j from (1- m) downto 0
                     do (setf (aref table i j)
                              (if (funcall token= (aref va i) (aref vb j))
                                  (1+ (aref table (1+ i) (1+ j)))
                                  (max (aref table (1+ i) j)
                                       (aref table i (1+ j)))))))
      (flet ((collect (token)
               (if (and ranges (= (cdr (first ranges)) (first token)))
                   (setf (cdr (first ranges)) (second token))
                   (push (cons (first token) (second token)) ranges))))
        (loop while (and (< i n) (< j m))
              do (cond ((funcall token= (aref va i) (aref vb j))
                        (when (third (aref va i)) (setf matched-word t))
                        (incf i)
                        (incf j))
                       ((>= (aref table (1+ i) j) (aref table i (1+ j)))
                        (collect (aref va i))
                        (incf i))
                       (t (incf j))))
        (loop for rest from i below n do (collect (aref va rest)))
        (when (and matched-word ranges)
          (nreverse ranges))))))

(defun parse-unified-patch (patch &aux (result '())
                                       (pending-removed '())
                                       (pending-added '())
                                       (old-num 0)
                                       (new-num 0)
                                       (in-hunk nil)
                                       (first-hunk t))
  "One \"diff --git\" section's unified patch text as diff document lines."
  ;; File headers before the first hunk drop; later hunks separate with a
  ;; "..." elision row; each hunk's contiguous removed-then-added run is
  ;; index-paired for word emphasis (the retired hunk-flush pairing). Removed
  ;; and context lines number old-side, added lines new-side.
  (labels ((flush (&aux (removed (nreverse pending-removed))
                        (added (nreverse pending-added)))
             (setf pending-removed '() pending-added '())
             (loop for r in removed
                   for a in added
                   do (setf r.ranges
                            (diff-word-ranges r.text
                                              (diff-line-text a))
                            a.ranges
                            (diff-word-ranges (diff-line-text a)
                                              r.text)))
             (setf result (revappend added (revappend removed result)))))
    (dolist (line (or (split-lines patch) '()))
      ;; A "@@ -A[,B] +C[,D] @@" hunk header: OLD-START only when both numbers read.
      (let* ((plus (and (>= (length line) 4) (string= "@@ -" line :end2 4)
                        (position #\+ line)))
             (new-start (and plus (parse-integer line :start (1+ plus) :junk-allowed t)))
             (old-start (and new-start (parse-integer line :start 4 :junk-allowed t))))
        (cond
          (old-start
           (flush)
           (unless first-hunk
             (push (diff-line :context nil "...") result))
           (setf first-hunk nil
                 in-hunk t
                 old-num old-start
                 new-num new-start))
          ((not in-hunk))
          ((zerop (length line)))
          (t
           (case (char line 0)
             (#\Space
              (flush)
              (push (diff-line :context old-num (subseq line 1)) result)
              (incf old-num)
              (incf new-num))
             (#\-
              ;; A removal after additions opens a new change run; keep
              ;; the pairing per run rather than smearing across runs.
              (when pending-added (flush))
              (push (diff-line :removed old-num (subseq line 1))
                    pending-removed)
              (incf old-num))
             (#\+
              (push (diff-line :added new-num (subseq line 1))
                    pending-added)
              (incf new-num))
             ;; "\ No newline at end of file" annotates, never renders.
             (#\\ nil)
             (t (flush)
                (setf in-hunk nil)))))))
    (flush)
    (nreverse result)))

(defun diff-body-lline (line gutter-width)
  "One diff document line as a logical line (transcript.zig
appendDiffBodySegments): added/removed rows tint the full row background
and keep the normal foreground for content — diff meaning rides on the bg
— with the gutter alone in the diff foreground tone and word-level ranges
on the emphasis background; context rows are the dim foreground base; meta
rows dim muted."
  (let* ((prefix-len (if (plusp gutter-width) (+ gutter-width 3) 2))
         (sign (diff-line-sign line))
         (added (eq sign :added)))
    (if (eq sign :meta)
        (make-lline :text (diff-line-text line) :tone :muted :dim t
                    :role :tool-cont)
        (make-lline
         ;; The Zig DocumentBuilder.appendPrefixed gutter: number column, sign, space.
         :text (concatenate 'string
                            (if (plusp gutter-width)
                                (format nil "~v@a " gutter-width (or (diff-line-number line) ""))
                                "")
                            (ecase sign (:added "+ ") (:removed "- ") (:context "  "))
                            (diff-line-text line))
         :tone :foreground :role :tool-cont
         :dim (eq sign :context)
         :fill (case sign (:added :diff-added-line) (:removed :diff-removed-line))
         :spans (unless (eq sign :context)
                  (cons (mspan 0 prefix-len
                               :tone (if added :diff-added :diff-removed))
                        (loop for (start . end) in (diff-line-ranges line)
                              collect (mspan (+ prefix-len start)
                                             (+ prefix-len end)
                                             :tone (if added
                                                       :diff-added-word
                                                       :diff-removed-word)))))))))

(defun append-row-diff-stats (lline row &aux (files (json-array row.metadata "workspace_diff" "files")))
  "The Zig \" +N -M\" header stats over the lines every file call ROW changed
added and removed, onto LLINE -- nothing for a call that changed none."
  (when (plusp (length files))
    (loop for file across files
          sum (or (json-value file :integer "added") 0) into added
          sum (or (json-value file :integer "removed") 0) into removed
          finally (append-lline-run lline (format nil " +~d" added) :tone :diff-added)
                  (append-lline-run lline (format nil " -~d" removed) :tone :diff-removed))))

(defparameter *diff-preview-lines* 12
  "How many lines of a changed file's diff stand under its header before
the header is clicked open: an edit reads as an edit at a glance, and a
long one does not bury the turn.")

(nlk:define-side-table parsed-patch (:weakness :key)
  "Weak memo: a diff document file -> PARSE-UNIFIED-PATCH of its patch, so
a run listing its edits on every frame parses each patch once.")

(defun file-diff-llines (file key &aux (open (detail-open-p key)))
  "The diff of FILE, a diff document file, as lines under its header: all
of it while the header (KEY) is clicked open, else its first
*DIFF-PREVIEW-LINES* and a line counting the rest, which opens it too."
  (let* ((patch (json-value file :string "patch"))
         (entries (and patch (memoized (parsed-patch file) (patch) (parse-unified-patch patch))))
         (maximum (reduce #'max entries :key (lambda (entry) (or (diff-line-number entry) 0)) :initial-value 0))
         (gutter (if (plusp maximum) (length (format nil "~d" maximum)) 0))
         (shown (if open entries (subseq entries 0 (min (length entries) *diff-preview-lines*))))
         (hidden (- (length entries) (length shown))))
    (append (mapcar (lambda (entry) (diff-body-lline entry gutter)) shown)
            (when (plusp hidden)
              (let ((more (make-lline :text (make-string (if (plusp gutter) (+ gutter 3) 2)
                                                         :initial-element #\Space)
                                      :tone :muted :role :tool-cont)))
                (append-lline-run more (format nil "… +~d line~:p" hidden) :tone :muted)
                (dolist (span (lline-spans more)) (setf (mspan-link span) key))
                (list more))))))

(defun workspace-diff-llines (row)
  "File disclosures immediately below their tool, each showing the head of
its diff and opening its own full diff (FILE-DIFF-LLINES)."
  (when-let (document (json-value row.metadata :object "workspace_diff"))
    (let ((truncated (or (json-value document :integer "truncated") 0)))
      (append
       (loop for file across (json-array document "files")
             when (hash-table-p file)
               nconc (let* ((path (or (json-value file :string "path") ""))
                            (key (list :diff (tool-row-expand-key row) path))
                            (added (format nil "+~d" (or (json-value file :integer "added") 0)))
                            (removed (format nil "-~d" (or (json-value file :integer "removed") 0)))
                            ;; The counts right-align against the content rail,
                            ;; so every path starts on it, under the words of the
                            ;; call that wrote it.
                            (hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
                            (header (make-lline :text (make-string (max 0 (- hang (length added) (length removed) 2))
                                                                   :initial-element #\Space)
                                                :tone :muted :hang hang)))
                       (append-lline-run header added :tone :diff-added)
                       (append-lline-run header (format nil " ~a" removed) :tone :diff-removed)
                       (append-lline-run header (format nil " ~a~a" path (if (json-value file :boolean "binary") " (binary)" "")) :tone :muted)
                       ;; All glyphs on the header toggle this file, never the
                       ;; OS opener or another file in the same Eval.
                       (dolist (span (lline-spans header)) (setf (mspan-link span) key))
                       (cons header (file-diff-llines file key))))
       (when (plusp truncated)
         (list (make-lline :text (format nil "... +~d more file~:p" truncated)
                           :tone :muted :dim t :role :tool-cont)))))))

;;; --- image-result cards -----------------------------------------------------
;;; The tool_result display fact (metadata envelope) the Zig transcript draws
;;; as a card: a generated image (writeTranscriptToolHeader's image arm plus
;;; the kitty preview band). It derives from the SAME durable fact on the live
;;; stream and in replay, so the card renders identically on both paths.

(defun image-fact-plist (image &optional text)
  "IMAGE, an image display fact, as (:payload-kind :source :path :target
:media-type :width :height :bytes :png-path), payload kind inferred from
TEXT, the result beside it, when the fact omits it (the Zig
inferredImagePayloadKind chain)."
  (list* :payload-kind (or (json-value image :text "payload_kind") (inferred-image-payload-kind text))
         :source (or (json-value image :text "source") "inferred")
         (append (json-members image :text "path" "target" "media_type")
                 (json-members image :integer "width" "height" "bytes")
                 (json-members image :text "png_path"))))

(defun row-image-plist (row)
  "The image display fact on the tool ROW, or NIL (IMAGE-FACT-PLIST)."
  ;; Two shapes reach here and both draw one card. A payload fact carries the
  ;; image in the result text as base64, which is what a provider-executed
  ;; image generator returns. A file fact — a LOOK's (exec.lisp) — names the
  ;; file instead: its path, media type and pixel size, plus the PNG a
  ;; terminal can be given, which is the file itself or a cached transcode of
  ;; it. The fact never carries bytes, so the preview is read from the named
  ;; path at the moment it is drawn.
  (when (eq :tool-call row.kind)
    (when-let (image (json-value row.metadata :object "image"))
      (image-fact-plist image row.text))))

(defun image-preview-payload (image row)
  "What the terminal would be given for the IMAGE fact on ROW: the PNG file
it names, or the base64 in the row's result text."
  ;; NIL when the fact names no PNG the protocol can carry — a JPEG on a box
  ;; with no transcoder — which is also what keeps the card from reserving a
  ;; band nothing can fill.
  (or (getf image :png-path) (and (null (getf image :path)) (inline-image-base64 row.text))))

(defun image-card-name (image row)
  "What the card calls the image: the base name of the file the fact names —
what a person reads to know which picture this is — and the tool that
produced it for a fact that carries bytes and no path."
  ;; Cut at the last separator by hand rather than through a pathname: a path
  ;; is a string here, and a `*' or a `[' in one is a character, never a
  ;; wildcard.
  (let* ((path (getf image :path))
         (cut (and path (position-if (lambda (ch) (find ch "/\\")) path :from-end t)))
         (name (and path (subseq path (if cut (1+ cut) 0)))))
    (or (and name (plusp (length name)) name) path (json-value row.tool-name :text) "image")))

(defun image-card-title-lline (image row)
  "The card's title, in the shape every other tool card's has: the :IMAGE
family's own verb (activity.lisp *ACTIVITY-FAMILIES* — `Viewed', `Viewing'
while the call runs) over the image's name, then the status word a card ends
with when it is not simply complete."
  ;; The whole path this is the name of lives in the caption under the
  ;; picture.
  (let ((title (title-phrases-lline
                (list (list (getf (activity-family-plist :image)
                                  (if (equal "running" row.tool-status) :present :past))
                            (image-card-name image row)
                            nil
                            ;; The name reads as the picture, so the name is
                            ;; what a click on the picture lands on.
                            (when-let (path (getf image :path)) (cons :path path)))))))
    (append-title-detail title (tool-note row))
    (append-status-text title row.tool-status (tool-status-text row.tool-status))))

(defun image-card-detail (image)
  "The card's caption, under the picture: the whole path, then what the image
is and how big — `/home/me/shot.png · image/png · 1920x1080 · 412.0 KB' —
and the payload wording for a fact that carries its bytes in the result text
instead."
  ;; A file the graphics protocol cannot carry says so where the size would
  ;; be, which is why no band is reserved under it.
  (if (getf image :path)
      (let ((width (getf image :width))
            (height (getf image :height))
            (bytes (getf image :bytes)))
        (format nil "~{~a~^ · ~}"
                (remove nil
                        (list (getf image :path)
                              (or (getf image :media-type) "image")
                              (and width height (format nil "~dx~d" width height))
                              (and bytes (size-text bytes))
                              (unless (getf image :png-path)
                                "no inline preview: the graphics protocol carries PNG and no transcoder is installed")))))
      (or (cdr (assoc (getf image :payload-kind)
                      '(("base64" . "base64 image payload")
                        ("data_url" . "image data URL")
                        ("url" . "image URL"))
                      :test #'equal))
          "image result")))

(defun inferred-image-payload-kind (text)
  "The Zig inferredImagePayloadKind: data URL, URL, base64, else unknown."
  (let ((text (or text "")))
    (cond ((uiop:string-prefix-p "data:image/" text) "data_url")
          ((web-url-p text) "url")
          ((inline-image-base64 text) "base64")
          (t "unknown"))))

(defun inline-image-base64 (payload)
  "The bare base64 body of an inline PNG payload, or NIL when PAYLOAD is
not inline-renderable (the Zig normalizeInlineImagePayload gate): a bare
base64 string or a data:image/png;base64, URL, base64-alphabet only,
bounded at 8MB."
  ;; The URL's head is everything before its first comma: it names PNG and
  ;; says base64, and the body after it is at least eight alphabet characters.
  ;; A head that says anything else stays on, and no body carries a colon.
  (let* ((text (nlk:trimmed (or payload "")))
         (body (ppcre:regex-replace "^data:image/png[^,]*;base64[^,]*," text "")))
    (and (<= (length text) (* 8 1024 1024))
         (ppcre:scan "^[A-Za-z0-9+/=]{8,}\\z" body)
         body)))

;;; RENDER-STATE binds it from the model's KITTY-GRAPHICS flag.
(defvar *kitty-images-enabled* nil
  "Render-scope switch: T reserves an inline preview band under image
cards (the terminal answered the kitty graphics query).")

(defparameter *image-preview-rows* 8
  "Rows the inline kitty preview band reserves under an image card whose
fact never measured the image (the Zig image_preview_rows viewport input).")

;;; An image inside a transcript is a glance at what the organism saw, never a
;;; page of it.
(defparameter *image-preview-max-rows* 16
  "Ceiling on a measured preview band.")

(defun image-preview-rows (image width &aux (image-width (getf image :width))
                                            (image-height (getf image :height))
                                            (columns (max 8 (- width 4))))
  "Rows the inline preview band reserves for the IMAGE fact at frame WIDTH."
  ;; A fact that measured the file gets the image's own aspect at the nominal
  ;; cell aspect, never taller than the image's pixels ask for, never taller
  ;; than *IMAGE-PREVIEW-MAX-ROWS*, and never so tall that the columns kitty
  ;; derives would run past the frame. Deterministic from the fact alone,
  ;; which is what lets the reserved band and the placement agree without
  ;; sharing state — and lets every attached shell reserve exactly what it
  ;; places.
  ;;
  ;; The terminal is never asked: kitty is given a row count and derives the
  ;; columns from the image's own aspect ratio, so the guess only has to be close
  ;; enough to keep an image off its own scale. A cell is nominally 18 pixels tall
  ;; (near enough) and 2 times taller than wide (near enough), which is what
  ;; keeps a wide image inside the frame.
  (if (and (integerp image-width) (plusp image-width)
           (integerp image-height) (plusp image-height))
      (max 1 (min *image-preview-max-rows*
                  (ceiling image-height 18)
                  (floor (* columns image-height)
                         (* 2 image-width))))
      *image-preview-rows*))

(defun image-band-llines (image row width)
  "What stands under the line naming IMAGE, a fact of ROW's: the band a
terminal that speaks the kitty graphics protocol draws it over, its first row
carrying the fact for the placement, then the caption -- alone where the
terminal has no graphics or the fact names no PNG it could be given."
  (append
   (when (and *kitty-images-enabled* (image-preview-payload image row))
     (loop for fact = image then nil
           repeat (image-preview-rows image width)
           collect (make-lline :text "" :role :image-slot :image fact)))
   ;; The caption opens with the whole path, which is the other half of the
   ;; click: the name above says which picture, this says where it is, and
   ;; both open it.
   (list (make-lline :text (image-card-detail image)
                     :tone :foreground :dim t :role :tool
                     :gutter-tone :muted
                     :spans (path-link-spans (getf image :path))))))

(defun answer-llines (row llines width)
  "LLINES, the prose of the assistant text ROW, with each picture its facts
name drawn under the line that names it (IMAGE-BAND-LLINES), and one no line
names -- a table's cell -- under the last."
  ;; The fact decides what is a picture; the line only says where it stands,
  ;; read by the rule the engine found the target with (MARKDOWN-IMAGE-TARGETS).
  ;; LLINES is the parse cache's and is not touched.
  ;; IMAGES: the file facts of the pictures the answer names (exec.lisp
  ;; ANSWER-IMAGES), in its order, each as IMAGE-FACT-PLIST shapes one.
  (let ((images (loop for image across (json-array row.metadata "images")
                      when (hash-table-p image) collect (image-fact-plist image))))
    (flet ((band (image) (image-band-llines image row width)))
      (if (null images)
          llines
          (append (loop for line in llines
                        collect line
                        unless (eq :code line.role)
                          append (loop for target in (markdown-image-targets line.text)
                                       for image = (find target images :test #'equal
                                                                       :key (lambda (image) (getf image :target)))
                                       when image
                                         do (setf images (remove image images))
                                         and append (band image)))
                  (mapcan #'band images))))))

(defstruct md-cache
  "One row's previous markdown parse plus its incremental checkpoint: the
trimmed-relative character offset of the last top-level blank line and the
count of llines emitted before it."
  ;; RAW is the text object the parse read — streaming rows grow their text in
  ;; place through the append buffer, so the same object with a larger fill
  ;; pointer means everything before the checkpoint re-parses to the same
  ;; llines by construction and the parse can restart at the checkpoint.
  raw len shadow width llines cp-offset cp-count)

(nlk:access (cache md-cache))

(nlk:define-side-table row-md-cache (:weakness :key)
  "Weak per-row memo for ROW-MARKDOWN-LLINES: row -> MD-CACHE.")

(defun row-markdown-llines (row raw &key shadow width)
  "MARKDOWN-LLINES over the outer-trimmed RAW (the TRIM-BLOCK-TEXT policy,
applied by bounds so an unchanged prefix is never re-copied), incremental
across streaming appends: when RAW is the same append buffer the cache
already parsed and a top-level blank-line checkpoint exists, the head
llines are reused as-is and only the text from the checkpoint on
re-parses."
  ;; The head llines stay EQ-stable, which is what feeds the LLINE-RENDER memo
  ;; during streaming; the cached llines are shared across frames, so callers
  ;; must treat them as immutable (copy before dimming or hover-painting).
  (let* ((cache (row-md-cache row))
         (start (position-if-not #'blank-char-p raw))
         (end (if start
                  (1+ (position-if-not #'blank-char-p raw :from-end t))
                  0))
         (usable
           (and cache start
                ;; Same buffer object, append-only growth: the parsed prefix
                ;; is intact without a character compare.
                (eq cache.raw raw)
                (>= (length raw) cache.len)
                (eql cache.shadow shadow)
                (eql cache.width width)
                cache.cp-offset
                (plusp cache.cp-offset)
                (< (+ start cache.cp-offset) end))))
    (multiple-value-bind (llines cp-offset cp-count)
        (flet ((parse (text)
                 ;; Kept for as long as the row is projected, so kept narrow:
                 ;; a line cut from the streaming buffer is four bytes a
                 ;; character. Only what parses fresh; a head line reused is
                 ;; narrowed already.
                 (multiple-value-bind (llines cp count)
                     (markdown-llines text :shadow shadow :width width
                                           ;; A row no fact owns yet, and no failure
                                           ;; sealed, is a stream still arriving.
                                           :arriving (not (or row.hydrated row.voided)))
                   (dolist (line llines)
                     (setf line.text (nlk:narrow-string line.text)))
                   (values llines cp count))))
          (if usable
              (bind ((off cache.cp-offset) (head-count cache.cp-count)
                     ((tail tail-cp tail-count) (parse (subseq raw (+ start off) end))))
                (values (nconc (subseq cache.llines 0 head-count)
                               tail)
                        ;; No blank line in the tail yet: the old checkpoint
                        ;; still bounds the stable prefix of the new text.
                        (if tail-cp (+ off tail-cp) off)
                        (if tail-cp (+ head-count tail-count) head-count)))
              (parse (if start (subseq raw start end) ""))))
      (setf (row-md-cache row)
            (make-md-cache :raw raw :len (length raw)
                           :shadow shadow :width width
                           :llines llines
                           :cp-offset cp-offset :cp-count cp-count))
      (unless shadow
        (stamp-glints row llines))
      llines)))

(nlk:define-side-table row-glint-stamps (:weakness :key)
  "A message row -> the internal times its chips arrived at, in their
order: NIL for one the row held when this shell first laid it out.")

(defun stamp-glints (row llines &aux (stamps (or (row-glint-stamps row)
                                                 (setf (row-glint-stamps row)
                                                       (make-array 4 :adjustable t :fill-pointer 0))))
                                     (count 0))
  "Give each chip of message ROW's LLINES the time it arrived (MSPAN-GLINT):
one first laid out while ROW is arriving arrived then, and a glint crosses
it once; one a finished row held, never."
  ;; A chip is known by its place among ROW's chips, which a stream only ever
  ;; adds to, so every parse of a growing row finds each chip's own time.
  (let ((now (and (not (or row.hydrated row.voided)) (get-internal-real-time))))
    (dolist (line llines)
      (dolist (span (lline-spans line))
        (when (eq :chip (mspan-ground span))
          (when (>= count (length stamps))
            (vector-push-extend now stamps))
          (setf (mspan-glint span) (aref stamps count))
          (incf count))))))

(defparameter *thinking-tail-rows* 4
  "How many wrapped rows of a thought still streaming a folded transcript
shows under its `Thinking…' line: enough to follow the sentence it is on,
not the minutes of it — with the line above, the height of a collapsed tool
card's output window (five rows).")

(defun thinking-p (row)
  "Whether ROW is a thought still arriving: streamed, not yet confirmed or
voided, its clock still running."
  (and (not row.hydrated) (not row.voided) (null row.thought-ms)))

(defun row-notice-summary (row)
  "A successful system notice's explicit disclosure title metadata, or NIL."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (not (transcript-row-error-p row))
       (json-value (transcript-row-metadata row) :object "summary")))

(defun row-notice-label (row)
  "A successful system notice's label metadata, the word its row stands
under in the label lane, or NIL."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (not (transcript-row-error-p row))
       (json-value (transcript-row-metadata row) :text "label")))

(defun row-notice-bare-p (row)
  "Whether ROW is a notice that stands as its words alone, no band and no
mark: its metadata says `bare'."
  (and (transcript-row-p row) (eq :system-notice (transcript-row-kind row))
       (json-value (transcript-row-metadata row) :boolean "bare")))

(defun row-llines (row width)
  "The logical lines for ROW under the Zig row-chrome table."
  (unless (redundant-finish-divider-p row)
    (let ((lines
           (ecase row.kind
             (:text
              (cond ((equal row.role "assistant")
                     ;; Raw text: ROW-MARKDOWN-LLINES applies the outer trim by
                     ;; bounds so a streaming frame never re-copies the whole body.
                     (answer-llines row (row-markdown-llines row row.text :width width) width))
                    ((equal row.role "user")
                     (mapcar (lambda (line)
                               (make-lline :text line :tone :user-block :fill :user-block))
                             (or (split-lines
                                  (trim-block-text row.text))
                                 (list ""))))
                    (t (system-notice-llines
                        (trim-block-text row.text) nil))))
             (:reasoning
              ;; Open: the prose under a shadow, no WIDTH. Folded: one line, a tail while it streams.
              ;; A folded thought that has finished is one line, so its prose is never laid out.
              (let* ((open (detail-open-p row))
                     (thinking (thinking-p row))
                     (prose (and (or open (and thinking (eq *tool-detail* :developer))) (row-markdown-llines row row.text :shadow t))))
                (cond ((not (position-if-not #'blank-char-p (or row.text ""))) nil)
                      (open prose)
                      ((eq *tool-detail* :standard)
                       ;; Parse only the displayed line, not the full closed thought.
                       ;; Markdown owns its text/style; the Think prefix only shifts
                       ;; copied spans, before the renderer attaches link metadata.
                       ;; BODY: the one line a folded thought shows, as Markdown: the newest
                       ;; line while it streams, its first once it is done; NIL for a
                       ;; marker-only line (the text is not blank here).
                       (let* ((raw row.text)
                              (end (1+ (position-if-not #'blank-char-p raw :from-end t)))
                              (start (if thinking
                                         (1+ (or (position #\Newline raw :end end :from-end t) -1))
                                         (position-if-not #'blank-char-p raw)))
                              (stop (if thinking end (position #\Newline raw :start start)))
                              ;; A marker-only excerpt has no Markdown body to preview.
                              (body (or (first (markdown-llines (subseq raw start stop) :shadow t))
                                        (make-lline :tone :muted :italic t)))
                              (prefix (lline-text (append-title-detail (title-phrases-lline (list (list "Think" "" nil nil))) ""))))
                         (list (copy-with lline body
                                 :text (concatenate 'string prefix
                                                    (if (lline-rule body) "─" (lline-text body)))
                                 :role :prose :hang (text-display-width prefix)
                                 :hang-mark nil :rule nil
                                 :spans (list* (mspan 0 5 :tone :thought :bold t)
                                               (mspan 5 (length prefix) :tone :muted :italic t)
                                               (mapcar (lambda (span)
                                                         (shifted-span span (length prefix)))
                                                       (lline-spans body)))))))
                      (t (cons (make-lline :text (if thinking
                                                     "Thinking…"
                                                     (let ((ms row.thought-ms) (tokens row.reasoning-tokens))
                                                       (format nil "Thought~@[ for ~a~]~@[ · ~a tokens~]"
                                                               (and ms (format-turn-elapsed ms))
                                                               (and tokens (plusp tokens)
                                                                    (format-token-count tokens)))))
                                           :tone :muted :italic t
                                           :spans (list (mspan 0 (if thinking 9 7) :tone :thought :bold t)))
                               ;; The last rows of a thought still streaming, blank lines skipped.
                               (and thinking
                                    (llines-window (remove-if (lambda (line)
                                                                (zerop (length (string-trim '(#\Space #\Tab)
                                                                                            line.text))))
                                                              prose)
                                                   width *thinking-tail-rows* :from-end t)))))))
             (:tool-call
              (cond
                ;; File disclosures belong to the Eval that changed them. Their
                ;; headers always show; each file alone owns its expanded diff.
                ((row-edited-p row)
                 (cons (tool-summary-lline row)
                       (append (workspace-diff-llines row)
                               (when (and (run-member-open-p row)
                                          (or row.error-p (row-load-errors row)
                                              (member row.tool-status '("stopped" "exited" "background") :test #'equal)))
                                 (tool-output-llines row width))
                               (when-let (image (row-image-plist row))
                                 (cons (image-card-title-lline image row) (image-band-llines image row width))))))
                ((and (eq *tool-detail* :standard) (not (detail-open-p (tool-row-expand-key row))))
                 (cons (tool-summary-lline row)
                       (fork-tree-llines row width :indent (- *transcript-detail-column*
                                                              (nth-value 2 (body-gutter :prose))))))
                ((when-let (image (row-image-plist row))
                   (cons (image-card-title-lline image row) (image-band-llines image row width))))
                (t
                 (append (if (eq *tool-detail* :standard)
                             (cons (tool-summary-lline row) (tool-command-llines row))
                             (tool-header-llines row))
                         (fork-tree-llines row width :indent 2)
                         (tool-output-llines row width)))))
             (:system-notice
              (if-let (summary (row-notice-summary row))
                (let ((title (title-phrases-lline (list (list (json-value summary :string "name") "" nil nil))
                                                  :verb-tone :success)))
                  (append-title-detail title (json-value summary :text "comment"))
                  (cons title (when (detail-open-p row)
                                (system-notice-llines (trim-block-text row.text) nil))))
                (let ((text (trim-block-text row.text)))
                  (if-let (label (row-notice-label row))
                    ;; A labelled notice's line: the label bold in the label lane where
                    ;; Eval and Think stand, the text on the content rail, on the canvas.
                    ;; Without this, the ■ and its words would sit between the rails,
                    ;; off the canvas, so the band would read as blank rows.
                    (let* ((hang (- *transcript-detail-column* (nth-value 2 (body-gutter :prose))))
                           (line (make-lline :text "" :tone :system :hang hang))
                           (gap (make-string (max 1 (- hang (text-display-width label)))
                                             :initial-element #\Space)))
                      (append-lline-run line label :bold t)
                      (append-lline-run line gap)
                      (list (append-lline-run line (nlk:one-line text))))
                    ;; A bare notice's lines: its words in the system tone on the canvas, on
                    ;; the label lane where a notice's words stand, with no mark in the
                    ;; gutter. Without this, a bare notice would read as a box the system
                    ;; put up rather than the organism speaking.
                    (if (row-notice-bare-p row)
                        (loop for line in (or (split-lines text) (list ""))
                              collect (make-lline :text line :tone :system :italic t
                                                  :role :system-cont
                                                  :spans (notice-url-spans line)))
                        (system-notice-llines text row.error-p))))))
             ;; The two divider rows are one row shape; the evicted side of the fold
             ;; seam is the dimmed one.
             ;; A wrapped row goes on under its words, past the gutter's mark.
             ((:finish-divider :eviction-divider)
              (list (make-lline :text (trim-block-text row.text)
                                :tone :muted :role :status :hang 2
                                :dim (eq :eviction-divider row.kind)))))))
      ;; Both speakers' words start on the prose gutter, where a run's title
      ;; and the live line's words stand, the clock at the first row's right
      ;; end.
      (if (message-timestamp-row-p row)
          (stamp-transcript-llines lines (transcript-clock row) width)
          lines))))
(defun row-llines-with-eviction (row width)
  "ROW-LLINES with the evicted-region dimming applied on top: every line of
a row a context eviction folded away renders dim, whatever its kind, and so
does a voided attempt's row — kept, but not the answer."
  ;; Dims copies: the markdown llines are shared with the per-row parse cache,
  ;; and a mutation there would outlive the eviction that asked for it. A
  ;; voided row whose seal label has landed closes with that label as a status
  ;; line. The model's words keep the foreground however much work followed
  ;; them: greyed, every message but a turn's answer read as spent.
  (let* ((lines (row-llines row width))
         (voided row.voided)
         (lines (if (or row.evicted-p voided)
                    (mapcar (lambda (line) (copy-with lline line :dim t)) lines)
                    lines)))
    (if (stringp voided)
        (append lines
                (list (make-lline :text voided :tone :muted :role :status
                                  :dim t)))
        lines)))

(defun row-block-fill (item)
  "The background tone ITEM's block paints behind its body and its padding
rows, NIL when the block sits on the canvas."
  ;; ITEM is a projection item: a TRANSCRIPT-ROW, or a TOOL-GROUP, whose
  ;; folded run is a tool block and sits on the canvas like one (the
  ;; viewport's pad arm reads the last line's owner, and that owner is the
  ;; group while the model works through routine calls). The Zig
  ;; Filled transcript blocks: system notices and error diagnostics fill;
  ;; user prompts keep their own fill; assistant prose, reasoning and tool
  ;; blocks stay on canvas (reasoning's shadow is over text, not a fill).
  (when (transcript-row-p item)
    (cond ((or (row-notice-summary item) (row-notice-label item) (row-notice-bare-p item)) nil)
          ((eq item.kind :system-notice) (if item.error-p :diagnostic-block :system-block))
          ((or (not (eq item.kind :text)) (equal item.role "assistant")) nil)
          ((equal item.role "user") :user-block)
          (t :system-block))))

;;; Weak keys, so rows a fold or settle drops release their blocks with them.
(nlk:define-side-table row-display-cache (:weakness :key)
  "Weak per-row memo for ROW-DISPLAY-BLOCK: row -> its render key and block.")

(defun row-block-padding-p (row &optional (mode *tool-detail*))
  "Whether ROW owns padding: thoughts, compact notices and Standard tools own none."
  (not (or (redundant-finish-divider-p row)
           (row-notice-summary row)
           (and (transcript-row-p row)
                (or (eq :reasoning row.kind) (and (eq mode :standard) (eq :tool-call row.kind)))))))

(defun row-hover-llines (row llines)
  "Hover paint for ROW, inline or nested: Standard disclosure titles, otherwise block surfaces."
  (cond
    ((not (eq row *hover-row*)) llines)
    ((and (eq :text row.kind) (equal "user" row.role))
     ;; The prompt keeps its block fill, including both padding rows.
     (mapcar (lambda (line)
               (copy-with lline line :fill :user-block-hover
                          :tone (if (eq :user-block line.tone) :user-block-hover line.tone)))
             llines))
    ((and (eq *tool-detail* :standard)
          (or (member row.kind '(:tool-call :reasoning)) (row-notice-summary row)))
     ;; An open thought is prose only: it has no title to accent.
     (if (and llines (not (and (eq row.kind :reasoning) (detail-open-p row))))
         (cons (disclosure-hover-lline (first llines)) (rest llines))
         llines))
    (t
     ;; Copy-on-hover: the stabilized llines and interned pads are shared.
     (mapcar (lambda (line) (copy-with lline line :fill (or line.fill :hover))) llines))))

(defun row-display-block (row &key (width 80))
  "Render one TRANSCRIPT-ROW into a DISPLAY-BLOCK of painted rows, the
block's padding included when its viewport policy owns any."
  ;; Memoized per row, so a streaming frame re-renders only the rows whose
  ;; content or modifiers moved and full re-projection cost tracks the delta,
  ;; not the transcript. Consumers already must not mutate a block's lines —
  ;; the projection cache shares them across animation ticks — and the memo
  ;; extends that same discipline across full renders.
  ;;
  ;; The fields are everything the block is a function of: the row fields the
  ;; display policy reads (reducers replace field values rather than mutating
  ;; them in place, so object identity is the change signal), the render width,
  ;; the theme, and the render modifiers. Hover folds to a per-row boolean so
  ;; pointer motion invalidates the two rows it crosses, not the whole
  ;; transcript; streaming appends grow text in place, so the text revision is
  ;; the change signal identity alone cannot carry.
  (when (bg-band-row-p row)
    (return-from row-display-block
      (multiple-value-bind (lines plains refs) (background-band-lines row width)
        (make-display-block :retention :live-only :kind :tool-call :row row
                            :lines lines :plain-lines plains :refs refs))))
  (memoized (row-display-cache row)
    (width *palette* *color-profile* *kitty-images-enabled*
     *tool-detail* *expanded-tool-call*
     (eq row *hover-row*)
     row.kind row.role
     row.retention row.text
     row.tool-name row.tool-args
     row.tool-status row.tool-exit-code
     (transcript-row-tool-call-id row) row.metadata (row-load-errors row)
     row.hydrated row.error-p
     row.evicted-p row.voided
     row.thought-ms row.reasoning-tokens
     row.text-rev)
    (let* ((fill (row-block-fill row))
           (body (row-body-llines row width))
           ;; The block's own padding rows. Padding is a property of the block, not of the
           ;; gap between blocks: the Zig walk emits one block_padding line above and one
           ;; below every block unconditionally, so two adjacent blocks are separated by
           ;; both of their pads and the rhythm never collapses to a single row.
           ;;
           ;; Padding belongs to a body, so an empty one pads to nothing: a row whose
           ;; content renders to no logical lines -- a provider round that reported
           ;; whitespace-only `reasoning_content', a text row still waiting on its first
           ;; delta -- is not a block and must not widen the gap between the blocks
           ;; around it. Nor is a body of blank lines: without this, one empty line
           ;; padded on both sides would draw three blank rows where the rhythm keeps one.
           (llines (cond ((not (row-block-padding-p row)) body)
                         ((some (lambda (line)
                                  (or (lline-image line) (lline-rule line)
                                      (position-if-not #'blank-char-p (lline-text line))))
                                body)
                          (append (list (block-pad-lline fill)) body
                                  (list (block-pad-lline fill)))))))
      (setf llines (row-hover-llines row llines))
      (multiple-value-bind (lines plains) (llines-rows llines width)
        (make-display-block :retention row.retention
                            :kind row.kind
                            :row row
                            :fill fill
                            :lines lines
                            :plain-lines plains)))))

(defun lline-content-equal (a b)
  "Field-wise LLINE equality: two logical lines that lay out and paint
identically, so one may substitute for the other in the render caches."
  ;; EQUALP walks every slot of both lines and of each span, but folds case,
  ;; so the two strings a row shows as written -- its text and each span's
  ;; link target -- are compared exactly as well.
  (or (eq a b)
      (and (string= (lline-text a) b.text)
           (equalp a b)
           (every (lambda (x y) (equal (mspan-link x) (mspan-link y))) (lline-spans a) b.spans))))

;;; Row chrome rebuilds every logical line from source text on each re-render;
;;; during streaming the tail row re-renders every frame while all but its
;;; last line come out identical, so keeping the unchanged ones EQ-stable is
;;; what lets LLINE-RENDER skip their wrap+paint.
(nlk:define-side-table row-llines-cache (:weakness :key)
  "Weak per-row memo for ROW-BODY-LLINES: row -> the lline list of the
row's previous render.")

(nlk:define-side-table row-image-bands (:weakness :key)
  "Weak memo: a row -> ((OFFSET . IMAGE) ...), the inline image bands of its
last render: the painted row each opens on, counted from the row's first, and
the fact drawn there.")

(defun row-body-llines (row width)
  "ROW's logical lines at WIDTH, EQ-stable across renders (the ROW-LLINES-CACHE),
its inline image bands noted for the placement (IMAGE-BAND-AT)."
  ;; Each fresh lline is replaced by ROW's previous content-identical lline at the
  ;; same position, so unchanged lines stay EQ across re-renders and hit the
  ;; LLINE-RENDER memo. The merged list becomes the new cache entry. Positional
  ;; lockstep is the right shape: streaming appends only extend or add trailing
  ;; lines, and any reflow (tables re-align, a fence closes) changes the affected
  ;; lines' content, which is exactly what the equality probe detects.
  (let ((llines (setf (row-llines-cache row)
                      (loop with prev = (row-llines-cache row)
                            for new in (row-llines-with-eviction row width)
                            for old = (pop prev)
                            collect (if (and old (lline-content-equal new old)) old new)))))
    (when (or (find-if #'lline-image llines) (row-image-bands row))
      (setf (row-image-bands row)
            (loop with at = 0
                  for line in llines
                  when line.image collect (cons at line.image)
                  do (incf at (length (lline-render line width))))))
    llines))

(defun visible-transcript-state (model &aux (preview (chat-model-transcript-preview model)))
  "The state the transcript projects this frame: an open dialog's preview
while one is armed, else MODEL's live folded state (the Zig
sessionsDialogPreview substitution at the render-plan seam)."
  ;; The preview never replaces the live state — it substitutes here only, and
  ;; only while a dialog is open, so closing the dialog restores the live
  ;; transcript.
  (if (and preview (chat-model-dialog model))
      (cdr preview)
      (chat-model-state model)))

;;; --- the group pass ----------------------------------------------------------