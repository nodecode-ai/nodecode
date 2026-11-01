;;;; view.lisp --- the view method, toasts, the ember. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- view method --------------------------------------------------------

(defun image-payload-base64 (payload)
  "The base64 PNG PAYLOAD stands for: its own body when PAYLOAD is an
inline image payload (a bare base64 string or a data:image/png URL), and
otherwise the bytes of the PNG file at that path."
  ;; NIL when it is neither, which is what marks a payload failed so nothing
  ;; retries it.
  (or (inline-image-base64 payload)
      (let ((octets (and (stringp payload) (plusp (length payload))
                         ;; Ceiling on a PNG file read for an inline preview: the
                         ;; whole of it goes to the terminal as base64, once per
                         ;; shell, so a file past 8 MiB simply shows no preview.
                         (nlt:file-octets payload (* 8 1024 1024)))))
        (when (equal "image/png" (nlt:image-media-type octets))
          (cl-base64:usb8-array-to-base64-string octets)))))

(defun ensure-kitty-image (model key payload &aux (cache (chat-model-kitty-image-cache model))
                                                  (cached (gethash key cache)))
  "The transmitted NLT:TERMINAL-IMAGE for PAYLOAD under KEY, transmitting
once through the program terminal and caching (the Zig AssetCache): NIL
when the payload names no PNG this can carry, transmission failed before,
the cache is full, or no terminal stream exists yet."
  ;; KEY is the call the image belongs to, so a second look at a file that
  ;; changed under it is a second transmission rather than the first one's
  ;; picture.
  (cond
    ((nlt:terminal-image-p cached) cached)
    ((or (eq cached :failed) (>= (hash-table-count cache) 16)) nil)   ; Zig max_cached_images
    (t
     (let* ((base64 (image-payload-base64 payload))
            (program (chat-model-program model))
            (stream (and program program.tty-stream)))
       ;; No stream yet is no verdict: the next frame asks again.
       (when (or stream (null base64))
         (let ((image (and base64 (ignore-errors (nlt:transmit-image base64 :stream stream)))))
           (setf (gethash key cache) (or image :failed))
           image))))))

(defun image-band-at (refs plains index &aux (ref (aref refs index)) (start index))
  "The image fact whose inline band opens on lane row INDEX, or NIL: a band
of the row that owns that lane row, as its last render noted them
(ROW-BODY-LLINES)."
  ;; A block paints as [pad] body [pad], every row of it under the same ref,
  ;; and a member card of an open group as its body alone: a blank first row is
  ;; the block's own padding, which no offset counts. Read off the whole
  ;; projection rather than the visible window: a row scrolled halfway out of
  ;; the band must answer where its image STARTS.
  (loop while (and (plusp start) (eq ref (aref refs (1- start))))
        do (decf start))
  (when (zerop (length (string-trim '(#\Space) (aref plains start))))
    (incf start))
  (cdr (assoc (- index start) (row-image-bands ref))))

(defun kitty-image-placements (model)
  "Declarative kitty placements for the pictures visible in the transcript
band, an image card's and an answer's alike: each anchors its transmitted
image at the first row of its own reserved band (IMAGE-BAND-AT), at the body
column."
  ;; Placement identity (image, id, x, y, rows) is deterministic, so unchanged
  ;; frames emit no kitty traffic — the renderer's placement-diff contract. A
  ;; band that begins above the viewport anchors nothing: the protocol
  ;; places a whole image at a cell, and showing part of one is a crop this
  ;; does not ask for. An image draws over the cells, a sheet's window
  ;; too, so none is placed while one is open.
  (when (and (chat-model-kitty-graphics model)
             (not (sheet-dialog-p (chat-model-dialog model))))
    (bind (((vp-height _ _ top-pad) (transcript-band-metrics model))
           (window (chat-model-band model))
           (refs (chat-model-line-rows model))
           (plains (chat-model-line-plain model))
           (width (projection-width model))
           ;; The band's two-column inset plus the body column, so the
           ;; picture starts where the text naming it does.
           (x (+ 2 (nth-value 2 (body-gutter :image-slot))))
           (counts (make-hash-table :test #'eql)))
      (loop for row from 0 below (min vp-height (length refs) (length plains))
            for index = (band-window-row-index window row)
            for ref = (and index (< index (min (length refs) (length plains))) (aref refs index))
            for fact = (and ref (transcript-row-p ref) (row-image-bands ref)
                            (image-band-at refs plains index))
            for payload = (and fact (image-preview-payload fact ref))
            ;; A card's own call owns the transmission: two
            ;; looks at one path are two images. An answer's
            ;; picture is a file named by its bytes.
            for image = (and payload
                             (ensure-kitty-image
                              model
                              (or ref.tool-call-id payload)
                              payload))
            for top = (+ 3 top-pad row)
            for rows = (and image (image-preview-rows fact width))
            ;; An image draws over the cells, so one sharing a row
            ;; with a toast would cover it: it waits for the toast
            ;; to go. A placement says no width, so a shared row is
            ;; the whole test.
            when (and image (not (toasts-cover-rows-p model top rows)))
              collect (nlt:make-image-placement
                       image
                       :x x
                       :y top
                       :rows rows
                       :placement-id
                       (incf (gethash image.id counts 0)))))))

(defun shell-window-title ()
  "The name this shell's window wears: the profile, then the workspace it
is standing in — `default · ~/src/thing'."
  ;; The same two facts the footer's right lane leads with, composed once more
  ;; for the one surface an operator reads without looking inside the frame.
  ;; Control characters are dropped: a title rides an escape sequence and a
  ;; stray one would end it early.
  (let* ((parts (remove nil (list (nlk:profile-label) (workspace-footer-label))))
         (text (format nil "~{~a~^ · ~}" (remove "" parts :test #'string=))))
    (when (plusp (length text))
      (remove-if (lambda (character) (< (char-code character) 32)) text))))

;;; --- toasts -----------------------------------------------------------------
;;; What the system says without being asked — an organism notice, the board
;;; handed to a shell as it attaches, this shell's own caught errors, the
;;; build and update lines — floats top right over the frame for a while: the
;;; web page's toast() (cells/web/page/app.js) in cells. News leaves after
;;; five seconds and waits while the pointer rests on it; an error, or a line
;;; naming something to do, stays until it is closed; the same words again
;;; count up (×N) on the one on screen and bring it back to the top; a keyed
;;; notice replaces the toast under its key, and toasts once in this shell
;;; for what it says; past three, the oldest news leaves first, then the
;;; oldest error. Live-only: never a transcript row and never replayed —
;;; the board (/cells) and NLK:NOTICE-LOG keep what was said. The frame's
;;; overlay paints them over the laid-out cells (NLT:PAINT-ROWS), so the
;;; transcript under a toast keeps its rows, its copy lane and its hit test.
;;; No line borders: a toast is its ground, two columns of it either side of
;;; the words, and a half-row edge of it above and below.

(nlk:define-section ("tui")
  (:guide "Terminal display preferences.")
  ("notification_popups" :boolean :default :false
                         :doc "Show notification popups in the top-right corner. Off by default; notices remain available in /cells."))

(defun notification-popups-p (&aux (section (nlk:find-section '("tui"))))
  "Whether this shell shows notification popups; the declared default is Off."
  (eq t (getf (nlk:section-settings section (nlk:section-table (read-shared-config) '("tui")))
              :notification-popups)))

(defparameter *toast-news-ms* 5000
  "How long a toast that does not stay is up, the web page's five seconds.")

(defparameter *toasts-kept* 3
  "How many toasts are up at once; a narrow window shows the newest two.")

(defparameter *toast-width* 44
  "Columns a toast takes on a window 60 or more wide.")

(defparameter *toast-text-lines* 4
  "Lines of words a toast shows; more are cut with an ellipsis.")

(defstruct (toast (:constructor make-toast (text &key error-p (stays error-p) key foot)))
  "One notice up on the frame."
  ;; STAYS: only a close takes it down. FOOT: words under TEXT that the cut
  ;; at *TOAST-TEXT-LINES* never takes — where the rest is — or NIL. BORN:
  ;; when its five seconds began — a repeat and the pointer leaving it start
  ;; them again. HELD: how many milliseconds of them had run when the pointer
  ;; came onto it, NIL while the pointer is elsewhere: its countdown stands
  ;; still there.
  text error-p stays key foot (count 1) (born (get-internal-real-time)) held)

(defstruct (placed-toast (:constructor place-toast (toast x y width rows margin strip)))
  "Where the last frame painted one toast: its box's first column X, its
first row Y, the box's WIDTH, and the ROWS painted there, each led by the
blank column left of the box when it has a MARGIN, with the countdown STRIP
those rows drew (TOAST-STRIP)."
  toast x y width rows margin strip)

(nlk:access (place placed-toast))

(defun toast (model text &key error-p stays key foot)
  "Put TEXT up as a toast on MODEL's frame: an error, or one that STAYS, until
it is closed, anything else for five seconds. KEY names a standing notice:
its toast replaces the one up under KEY. FOOT is said under TEXT, whole
however much of TEXT the toast cuts. => whether the toasts moved."
  (let ((text (string-right-trim '(#\Space #\Tab #\Return #\Newline) (or text "")))
        (seen (chat-model-toasts-seen model)))
    ;; A keyed notice toasts once for what it says: the board is handed to
    ;; every shell that attaches, and a reconnect is not news.
    (when (and (plusp (length text))
               (not (and key (equal text (gethash key seen)))))
      (when key (setf (gethash key seen) text))
      (let* ((toasts (chat-model-toasts model))
             (same (find text toasts :key #'toast-text :test #'string=)))
        (cond (same
               (incf (toast-count same))
               (setf (toast-born same) (get-internal-real-time)
                     toasts (cons same (remove same toasts)))
               ;; Under the pointer its countdown stands still, full again.
               (when (toast-held same) (setf (toast-held same) 0)))
              (t
               (setf toasts (cons (make-toast text :error-p error-p :stays (or stays error-p)
                                              :key key :foot foot)
                                  (if key (remove key toasts :key #'toast-key :test #'equal) toasts)))
               ;; Past the room the oldest news leaves first, then the oldest
               ;; that stays; the new one never.
               (loop while (> (length toasts) *toasts-kept*)
                     do (setf toasts (remove (or (find-if-not #'toast-stays (rest toasts) :from-end t)
                                                 (car (last toasts)))
                                             toasts)))))
        (setf (chat-model-toasts model) toasts)
        t))))

(defun close-toast (model toast)
  "Take TOAST down. => whether it was up."
  (when (member toast (chat-model-toasts model))
    (setf (chat-model-toasts model) (remove toast (chat-model-toasts model)))
    (when (eq toast (chat-model-toast-hover model))
      (setf (chat-model-toast-hover model) nil))
    t))

(defun expire-toasts (model &aux (now (get-internal-real-time)))
  "Take down the news whose five seconds ran out. => whether any left."
  (flet ((expired-p (toast)
           (and (not (toast-stays toast))
                (not (eq toast (chat-model-toast-hover model)))
                (>= (since-ms (toast-born toast) now) *toast-news-ms*))))
    (when (some #'expired-p (chat-model-toasts model))
      (setf (chat-model-toasts model) (remove-if #'expired-p (chat-model-toasts model)))
      t)))

(defun hover-toast (model toast &aux (was (chat-model-toast-hover model)))
  "Rest the pointer on TOAST, NIL for none. => whether that moved."
  ;; The toast the pointer leaves starts its five seconds again, as the web
  ;; page's does on pointerleave; the one it comes onto holds its countdown
  ;; where it stands.
  (unless (eq toast was)
    (when was (setf (toast-born was) (get-internal-real-time)
                    (toast-held was) nil))
    (when toast (setf (toast-held toast) (since-ms (toast-born toast))))
    (setf (chat-model-toast-hover model) toast)
    t))

(defparameter *toast-strip-steps* 8
  "Steps a countdown strip takes across one column: its last cell fades from
the strip's shade to the ground in this many.")

(defun toast-strip (toast width now)
  "Columns of TOAST's WIDTH-wide bottom edge its countdown spans at NOW, to
a *TOAST-STRIP-STEPS*th of one: all of them as its five seconds begin, none
as they end, standing still while the pointer holds it; NIL for a toast
that stays."
  ;; A cell cannot be cut: the strip's tip would need three colours in one —
  ;; the strip, the ground, and the cell behind the edge. The part of a
  ;; column left is its last cell's shade instead (TOAST-ROWS), so the strip
  ;; slides rather than stepping a whole column nine times a second.
  (unless (toast-stays toast)
    (let ((ran (or (toast-held toast) (since-ms (toast-born toast) now))))
      (max 0 (min width (/ (ceiling (* width *toast-strip-steps* (- *toast-news-ms* ran))
                                    *toast-news-ms*)
                           *toast-strip-steps*))))))

(defun toast-strips-moved-p (model &aux (now (get-internal-real-time)))
  "Whether a countdown strip the last frame painted has moved a step since."
  (some (lambda (place) (not (eql place.strip (toast-strip place.toast place.width now))))
        (chat-model-toast-layout model)))

(defun toast-text-lines (toast width &aux (count (toast-count toast)))
  "TOAST's words broken to WIDTH columns, at most *TOAST-TEXT-LINES* lines, the
last of its text cut with … when there were more, then its foot whole, and
its ×N when it was said again. => (values LINES TAIL), TAIL to follow the
last line's words."
  (flet ((broken (text)
           (loop for line in (split-lines text) nconc (wrapped-lines line width)))
         (cut (line room)
           (concatenate 'string (take-display-prefix line (1- room)) "…")))
    (let* ((foot (broken (toast-foot toast)))
           (room (max 1 (- *toast-text-lines* (length foot))))
           (lines (broken (toast-text toast)))
           (tail (and (> count 1) (format nil "×~d" count))))
      (setf lines (append (cap-lines lines room width) foot))
      (let ((last-line (car (last lines))))
        (cond ((null tail) (values lines nil))
              ((<= (+ (text-display-width last-line) 1 (length tail)) width)
               (values lines (format nil " ~a" tail)))
              ((< (length lines) *toast-text-lines*)
               (values (append lines (list "")) tail))
              (t
               (setf (car (last lines)) (cut last-line (- width 1 (length tail))))
               (values lines (format nil " ~a" tail))))))))

(defun toast-rows (toast width &key hovered margin strip)
  "TOAST painted WIDTH columns wide: a half-row edge of its ground above (▄)
and below (▀), whose other half shows the cell behind it, and between them
its words with two columns of ground either side and its × on the first
row. The bottom edge's first STRIP columns are its countdown (TOAST-STRIP),
in a shade between the ground and the ×'s, a row of ground under the words.
HOVERED lifts the ground a step and lights the ×; MARGIN leads every row
with one blank canvas cell."
  ;; Under the :none profile there is no ground to draw an edge in, so the
  ;; edges are blank rows, the words sit on blanks, and no strip shows. The
  ;; row of ground above the strip is its own: right under the words it read
  ;; as underlining them. A toast with no strip keeps its three-part shape.
  (let* ((tone (if hovered :toast-hover :toast))
         (ground (unless (eq (effective-color-profile) :none)
                   (visual-style-bg (style-for-tone tone))))
         (lit (if (and ground strip) strip 0))
         (shade (and (plusp lit)
                     (blend-rgb (visual-style-fg (dim-style (style-for-tone tone))) ground 40)))
         (lead (if margin (blank-frame-row 1 (canvas-fill-bg)) ""))
         (text-width (- width 7)))
    (flet ((edge (glyph &optional (lit 0))
             ;; LIT whole cells in the shade, the part of one left as its
             ;; share of the shade over the ground, the rest ground.
             (multiple-value-bind (whole part) (floor lit)
               (concatenate
                'string lead
                (if ground
                    (concatenate
                     'string
                     (styled (make-string whole :initial-element glyph) (vstyle :fg shade))
                     (if (plusp part)
                         (styled (string glyph)
                                 (vstyle :fg (blend-rgb shade ground (round (* 100 part)))))
                         "")
                     (styled (make-string (- width whole (if (plusp part) 1 0)) :initial-element glyph)
                             (vstyle :fg ground)))
                    (make-string width :initial-element #\Space))))))
      (multiple-value-bind (lines tail) (toast-text-lines toast text-width)
        (append
         (list (edge #\▄))
         (loop for (line . more) on lines
               for first = t then nil
               for count = (and (null more) tail)
               collect (let ((used (+ 2 (text-display-width line)
                                      (if count (text-display-width count) 0))))
                         (concatenate
                          'string lead
                          (render-row
                           nil
                           (append (list (make-seg :text "  " :tone tone))
                                   (fragment-segs
                                    (make-lline :text line :spans (notice-url-spans line)
                                                :tone (if (toast-error-p toast) :toast-danger tone))
                                    0 (length line))
                                   (and count (list (make-seg :text count :tone tone :dim t)))
                                   (list (make-seg :text (make-string (max 0 (- width 3 used))
                                                                      :initial-element #\Space)
                                                   :tone tone)
                                         (make-seg :text (if first "×" " ") :tone tone
                                                   :dim (not hovered))
                                         (make-seg :text "  " :tone tone)))
                           ground width))))
         (and ground strip (list (concatenate 'string lead (blank-frame-row width ground))))
         (list (edge #\▀ lit)))))))

(defun place-toasts (model width floor)
  "Where MODEL's toasts paint on a WIDTH-column frame, newest on top from
row 3 down to row FLOOR, which none reaches: *TOAST-WIDTH* columns ending
where the transcript's text does, one blank column short of the scroll rail,
with a blank canvas column to their left, or from the frame's two-column
gutter to that same edge below 60 columns, three at most (two there). => a
list of PLACED-TOAST."
  ;; Visibility only: notice data and direct command answers keep their owners.
  ;; Dropping the layout also releases hit tests, image occlusion and countdown paint.
  (when (and (chat-model-toasts model) (not (notification-popups-p)))
    (hover-toast model nil)
    (return-from place-toasts nil))
  ;; A toast that would reach FLOOR is not placed, nor any older one: the
  ;; stack never covers the composer or the rows under a docked panel. Its
  ;; right edge is the message boxes' own, so the blank column before the
  ;; rail keeps it from reading as one block with the thumb.
  (let* ((wide (>= width 60))
         (right (+ 2 (transcript-text-width width)))
         (box (if wide (min *toast-width* (- right 5)) (- right 2)))
         (x (if wide (- right box) 2))
         (y 3)
         (now (get-internal-real-time)))
    ;; Under 12 columns there is no room for words between the pads and the ×.
    (when (>= box 12)
      (loop for toast in (chat-model-toasts model)
            for shown below (if wide *toasts-kept* 2)
            for strip = (toast-strip toast box now)
            for rows = (toast-rows toast box :hovered (eq toast (chat-model-toast-hover model))
                                             :margin wide :strip strip)
            while (<= (+ y (length rows)) floor)
            collect (place-toast toast x y box rows wide strip)
            do (incf y (length rows))))))

(defun toast-at (model x y &aux (column (1- x)) (row (1- y)))
  "The toast the last frame painted under mouse X, Y (1-based), or NIL, and
:CLOSE as a second value when the cell is its × (or a column either side of
it)."
  (dolist (place (chat-model-toast-layout model))
    (let ((right (+ place.x place.width)))
      (when (and (<= place.x column (1- right))
                 (<= place.y row (+ place.y (length place.rows) -1)))
        (return (values place.toast
                        (and (= row (1+ place.y)) (<= (- right 4) column (- right 2)) :close)))))))

(defun toasts-cover-rows-p (model top count)
  "Whether a toast the last frame placed covers any of the COUNT rows from
row TOP."
  (some (lambda (place)
          (and (< place.y (+ top (or count 1)))
               (< top (+ place.y (length place.rows)))))
        (chat-model-toast-layout model)))

(defun toasts-overlay (model &aux (layout (chat-model-toast-layout model)))
  "The toasts COMPOSE-FRAME placed, as a paint over the frame's cells, or NIL
while none is up."
  (when layout
    (lambda (buffer)
      (dolist (place layout)
        (nlt:paint-rows buffer place.rows
                        :x (if place.margin (1- place.x) place.x)
                        :y place.y)))))

(defun tooltip-rows (texts width &aux (bg (visual-style-bg (style-for-tone :toast))))
  "TEXTS painted as a WIDTH-column tooltip on the toast ground: each wrapped
inside two-column margins, between a blank row above and one below."
  (append (list (blank-frame-row width bg))
          (loop for text in texts
                nconc (loop for line in (wrapped-lines text (max 1 (- width 4)))
                            collect (render-row nil (list (make-seg :text (format nil "  ~a  " line)
                                                                    :tone :foreground))
                                                bg width)))
          (list (blank-frame-row width bg))))

(defun status-hint-overlay (model)
  "Keyboard affordances over the active status verb, never inline."
  (when (and (chat-model-turn-in-flight model)
             (not (chat-model-dialog model))
             (not (chat-model-selection-anchor model))
             (eq :status-hint (chat-model-hover-target model)))
    (let* ((rows (frame-lane-rows (frame-lanes model)))
           (index (position-if (lambda (row)
                                 (find :status-hint (plain-row-links row) :key #'third))
                               rows))
           (hint (live-status-hint model)))
      (when (and index (plusp (length hint)))
        (let* ((row (aref rows index))
               (range (find :status-hint (plain-row-links row) :key #'third))
               (column (text-display-width (subseq (plain-frame-row row) 0 (first range))))
               (width (min (frame-width model) (+ 4 (text-display-width hint))))
               (tooltip (tooltip-rows (list hint) width))
               (x (max 0 (min column (- (frame-width model) width))))
               (y (max 0 (- index (length tooltip)))))
          (lambda (buffer) (nlt:paint-rows buffer tooltip :x x :y y)))))))

;;; --- the ember ---------------------------------------------------------------
;;; A running call's listed line burns amber (RUN-TAIL-LLINE): in grey it
;;; would read as a command that had stopped. The ember keeps
;;; it reading alive, and it says what the command does rather than sweeping
;;; on a loop: each line its exec writes flares the words white-hot -- toward
;;; the title's ink, the theme's strongest, so a light theme flares dark --
;;; cooling over +EMBER-FLARE-MS+, and while it is quiet it
;;; breathes on +EMBER-BREATH-MS+ -- a command that prints once a second
;;; beats once a second, and a silent one never reads as frozen. Heat is
;;; paint, never projection: the frame's overlay warms the cells the band laid
;;; out, so a tick that moves only the heat re-projects nothing.

;;; The fades' paint: where their rows stand on screen, and their light.

(nlk:define-side-table fade-rows (:weakness :key)
  "A model -> the rows its last frame showed in a place still fading,
(Y X0 X1 FADE ITEM) each: the screen row, the cells from X0 below X1 its
words span, the place and what of it the row paints.")

(nlk:define-side-table fade-painted (:weakness :key)
  "A model -> T once its last frame's fade overlay painted every fading row
all the way up.")

(defun fade-moved-p (model &aux (now (get-internal-real-time)))
  "Whether a fading row on screen is still moving, or stood still since the
last frame painted it lower: what a tick that moved nothing else repaints
for."
  (and (fade-rows model)
       (or (notevery (lambda (row) (fade-settled-p (fourth row) now)) (fade-rows model))
           (not (fade-painted model)))))

(defun fade-due-p (model &aux (now (get-internal-real-time)))
  "Whether a row on screen is one whose place has gone over to what holds it:
the thing that left is out, and a render puts the newcomer where it stood."
  (some (lambda (row &aux (fade (fourth row)))
          (and (not (equal (fifth row) (fade-on fade now)))
               (equal (fifth row) fade.leaving)))
        (fade-rows model)))

(defun sgr-ink-rgb (sgr)
  "The rgb a cell's foreground SGR text names -- `38;2;R;G;B', `38;5;N', or a
basic 30-37/90-97 -- or NIL for the terminal's default."
  (if-let (code (and sgr (not (find #\; sgr)) (parse-integer sgr :junk-allowed t)))
    (nth (- code (if (< code 90) 30 82)) *ansi16-palette*)
    (sgr-rgb sgr)))

(defun fade-overlay (model &aux (rows (fade-rows model)) (profile (effective-color-profile)))
  "The fading rows' light as a paint over the frame's cells, or NIL while
none shows: a cell's ink blended toward the ground under it, the canvas the
terminal shows (CANVAS-RGB) under a cell with none; over a canvas the
terminal has not said, such a cell stands as painted. A place whose tokens
come up one by one lights each token on its own, counted in reading order
down its rows."
  (when (and rows (not (eq profile :none)))
    (let* ((now (get-internal-real-time))
           (settled (every (lambda (row) (fade-settled-p (fourth row) now)) rows))
           (ink (visual-style-fg (style-for-tone :foreground)))
           (canvas (canvas-rgb))
           ;; The parser hands every cell of one colour the same string, so a
           ;; frame blends each colour pair at a light once.
           (blends (make-hash-table :test 'equal))
           ;; Each place's token count so far, across its rows.
           (counts (make-hash-table :test 'eq)))
      (setf (fade-painted model) settled)
      (unless settled
        (lambda (buffer &aux (height (nlt:screen-buffer-height buffer)) (width (nlt:screen-buffer-width buffer)))
          (flet ((paint (cell light)
                   (when (< light 100)
                     (let ((key (list (nlt:cell-fg cell) (nlt:cell-bg cell) light)))
                       (when-let (sgr (alexandria:ensure-gethash
                                       key blends
                                       (let ((ground (or (sgr-rgb (second key)) canvas)))
                                         (and ground
                                              (color-sgr (blend-rgb (or (sgr-ink-rgb (first key)) ink) ground light)
                                                         t profile)))))
                         (setf (nlt:cell-fg cell) sgr))))))
            (loop for (y x0 x1 fade item) in rows
                  when (< y height)
                    do (if (and (plusp (fade-step fade)) (equal item (fade-on fade now))
                                (not (equal item (fade-leaving fade))))
                           ;; A token starts on a glyph after a blank; the
                           ;; second cell of a wide glyph is neither.
                           (loop with token = (gethash fade counts -1)
                                 with blank = t
                                 for x from x0 below (min x1 width)
                                 for cell = (nlt:screen-buffer-ref buffer x y)
                                 for space = (and (char= #\Space (nlt:cell-char cell)) (plusp (nlt:cell-width cell)))
                                 do (when (and blank (not space)) (incf token))
                                    (setf blank space)
                                    (unless space (paint cell (fade-light fade item now (max 0 token))))
                                 finally (setf (gethash fade counts) token))
                           (loop with light = (fade-light fade item now)
                                 for x from x0 below (min x1 width)
                                 do (paint (nlt:screen-buffer-ref buffer x y) light))))))))))

(defparameter +ember-flare-ms+ 700
  "Milliseconds a flare takes to cool back to the line's amber.")

(defparameter +ember-breath-ms+ 2400
  "Milliseconds one breath of a quiet running line takes.")

(defparameter +ember-breath-heat+ 3/10
  "How hot a breath runs at its peak, a flare's being 1.")

(nlk:define-side-table ember-stamps (:weakness :key)
  "A running call -> (LINES . AT): the lines its exec had written when this
shell last saw the count move, and the internal time it did.")

(nlk:define-side-table ember-rows (:weakness :key)
  "A model -> the running listed lines its last frame showed, (Y X0 X1 CALL)
each: the screen row, the cells from X0 below X1 its words span, the call.")

(nlk:define-side-table ember-painted (:weakness :key)
  "A model -> the heats its last frame's ember painted.")

(defun ember-heat (call backgrounds &optional (now (get-internal-real-time)))
  "How hot CALL's listed line burns at NOW, in twentieths: 0 its amber, 20
white-hot. A flare when it is first seen and whenever its exec among
BACKGROUNDS writes a line, then a breath while it is quiet."
  (let* ((exec (transcript-background-row call backgrounds))
         (lines (if exec (bg-row-written-lines exec) 0))
         (stamp (ember-stamps call)))
    (unless (eql lines (car stamp))
      (setf stamp (setf (ember-stamps call) (cons lines now))))
    (let ((quiet (since-ms (cdr stamp) now)))
      (round (* 20 (if (< quiet +ember-flare-ms+)
                       (expt (- 1 (/ quiet +ember-flare-ms+)) 2)
                       (* +ember-breath-heat+
                          (/ (- 1 (cos (/ (* 2 pi (- quiet +ember-flare-ms+)) +ember-breath-ms+)))
                             2))))))))

(defun ember-heats (model &optional (now (get-internal-real-time)))
  "The heat of each running line MODEL's last frame showed, at NOW."
  (let ((backgrounds (state-background-rows (visible-transcript-state model))))
    (mapcar (lambda (row) (ember-heat (fourth row) backgrounds now)) (ember-rows model))))

(defun ember-moved-p (model)
  "Whether a running line on screen burns hotter or cooler than the last
frame painted it: what a tick that moved nothing else repaints for."
  (and (ember-rows model) (not (equal (ember-heats model) (ember-painted model)))))

(defun ember-overlay (model &aux (rows (ember-rows model)) (profile (effective-color-profile)))
  "The running lines' heat as a paint over the frame's cells, or NIL while
none shows: a warm cell's foreground is the line's amber blended toward its
white-hot, the title's ink with a quarter of the amber left in it."
  (when (and rows (not (eq profile :none)))
    (let* ((heats (setf (ember-painted model) (ember-heats model)))
           (amber (visual-style-fg (style-for-tone :tool)))
           (core (blend-rgb (visual-style-fg (style-for-tone :title)) amber 75)))
      (lambda (buffer)
        (loop for (y x0 x1) in rows
              for heat in heats
              when (and (plusp heat) (< y (nlt:screen-buffer-height buffer)))
                do (loop with fg = (color-sgr (blend-rgb core amber (* 5 heat)) t profile)
                         for x from x0 below (min x1 (nlt:screen-buffer-width buffer))
                         do (setf (nlt:cell-fg (nlt:screen-buffer-ref buffer x y)) fg)))))))

(nlk:define-side-table glint-rows (:weakness :key)
  "A model -> the chips its last frame showed a glint still crossing,
(Y X0 X1 AT) each: the screen row, the cells from X0 below X1, the time the
chip arrived.")

(defun glint-overlay (model &aux (rows (glint-rows model)))
  "The glints crossing chips that have just arrived as a paint over the
frame's cells, or NIL while none crosses: a band of the chip's ink laid into
its ground, eased in from the left and out past the right -- the home mark's
shine, on what an answer found. Truecolor only: quantized, the band jumps."
  (when (and rows (eq (effective-color-profile) :truecolor))
    (let ((now (get-internal-real-time))
          (glow (blend-rgb (visual-style-fg (style-for-tone :inline-strong))
                           (visual-style-bg (style-for-tone :chip)) 55)))
      (lambda (buffer)
        (loop for (y x0 x1 at) in rows
              for s = (min 1 (/ (since-ms at now) +glint-ms+))
              for half = (max 1.5d0 (* 0.45d0 (- x1 x0)))
              for centre = (+ x0 (- half) (* (/ (- 1 (cos (* pi s))) 2) (+ (- x1 x0) half half)))
              when (< y (nlt:screen-buffer-height buffer))
                do (loop for x from x0 below (min x1 (nlt:screen-buffer-width buffer))
                         for heat = (- 1 (/ (abs (- (+ x 1/2) centre)) half))
                         for cell = (nlt:screen-buffer-ref buffer x y)
                         when (plusp heat)
                           do (setf (nlt:cell-bg cell)
                                    (color-sgr (blend-rgb glow (or (sgr-rgb (nlt:cell-bg cell)) glow)
                                                          (round (* 100 heat)))
                                               nil :truecolor))))))))

(defun frame-overlay (model &aux (overlays (remove nil (mapcar (lambda (overlay) (funcall overlay model))
                                                               '(fade-overlay ember-overlay glint-overlay
                                                                 status-hint-overlay toasts-overlay selection-overlay
                                                                 message-timestamp-overlay sheet-overlay)))))
  "The frame's overlays: the fading places' light, the running lines' ember,
the arriving chips' glints, verb hint, toasts, then the drag selection wash,
the message timestamps, and an open sheet's window over them all."
  (when overlays
    (lambda (buffer) (dolist (overlay overlays) (funcall overlay buffer)))))

(defun shell-view (model)
  "The composed frame as the tui view — the named body VIEW delegates to,
so a mutation (or a test) can redefine and faithfully restore the shell."
  ;; COMPOSE-FRAME settles PARKED-CURSOR, so it must run before the slot
  ;; is read into the view-state below. The rows go to the renderer as the
  ;; list they are: no join, no re-split.
  (nlt:make-view (compose-frame model)
                 :alt-screen t
                 ;; Any-event tracking: hover states need pure motion
                 ;; (:all-motion = 1003h+1006h).
                 :mouse-mode :all-motion
                 ;; Kitty keyboard, disambiguate-only (CSI >1u) — the
                 ;; flag the Zig TUI's vaxis init also carries. Legacy
                 ;; encodings cannot express Shift+Enter at all: the
                 ;; terminal sends a bare CR for it, so the default
                 ;; insert-newline chord was unreachable without this.
                 ;; With disambiguation on, modified and functional keys
                 ;; arrive as CSI-u (Shift+Enter = CSI 13;2u, Ctrl+J =
                 ;; CSI 106;5u, Escape = CSI 27u) while unmodified text
                 ;; keys keep arriving as plain UTF-8, so typing —
                 ;; capitals included — is untouched. Terminals without
                 ;; the protocol ignore the sequence and keep the legacy
                 ;; decoding, which now reads Ctrl+J correctly too.
                 :keyboard-enhancements t
                 ;; The hardware cursor parked on the composer caret cell,
                 ;; or hidden (?25l) while no cell owns it — the Zig
                 ;; vaxis cursor_visible contract. Steady block, matching
                 ;; the Zig TUI's cursor.
                 :cursor (when-let (park (chat-model-parked-cursor model))
                           (nlt:make-cursor :x (car park) :y (cdr park)))
                 ;; Inline kitty previews for visible image cards; empty
                 ;; (and therefore inert) when the terminal never answered
                 ;; the graphics query.
                 :images (kitty-image-placements model)
                 ;; The window wears what the footer's right lane says, so
                 ;; two shells are told apart by the window manager, the
                 ;; tab bar and the task switcher — where an operator with
                 ;; two of them open actually looks. Live-only: written to
                 ;; the terminal, never to the store, and given back on
                 ;; the way out.
                 :title (shell-window-title)
                 ;; The toasts and the drag selection's wash, over the cells
                 ;; these rows paint.
                 :overlay (frame-overlay model)))

(defmethod view ((model chat-model))
  (shell-view model))

