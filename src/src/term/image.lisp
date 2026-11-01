;;;; image.lisp --- kitty graphics: transmit, place, delete.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;; Derived from cl-tuition's image.lisp (see NOTICE), the nodecode fork's
;;;; own work: an image is transmitted once (direct base64, chunked at
;;;; 4096 bytes as the protocol requires), placed at cell positions by the
;;;; view-state, and the renderer diffs successive placement lists so an
;;;; unchanged placement survives a frame untouched. Capability detection
;;;; is the vaxis heuristic: an APC query the decoder surfaces as a
;;;; KITTY-GRAPHICS-MSG; silence means no support.

(in-package #:nodecode.term)

(defvar *kitty-image-id-counter* 0
  "Monotonic kitty image ids.")

(defconstant +kitty-chunk-size+ 4096
  "Base64 payload bytes per kitty graphics escape.")

(nlk:define-record (terminal-image (:constructor make-terminal-image)
                                   (:export :constructor :predicate id width height))
  "An image the terminal holds under ID."
  (id 0 :type fixnum)
  (width nil :type (or null fixnum))
  (height nil :type (or null fixnum)))

(nlk:access (image terminal-image))

;;; --- what image bytes are ------------------------------------------------
;;; One authority on the shape of image bytes, and it reads the bytes, never
;;; a name: a .png that is not a PNG reaches no provider and no terminal as
;;; one. LOOK admits a file by the media type (exec.lisp), the transcript's
;;; image card sizes its inline preview band by the dimensions, and the
;;; carrier below coerces by both.

(defun %unsigned (bytes index count &optional little)
  "The COUNT bytes of BYTES at INDEX as one unsigned integer, high byte first
or, with LITTLE, low byte first."
  (loop for i below count sum (ash (aref bytes (+ index i)) (* 8 (if little i (- count i 1))))))

(defun image-media-type (octets)
  "The media type OCTETS declare in their leading bytes — image/png,
image/jpeg, image/gif, image/webp — or NIL."
  ;; The file's own bytes, never its name: a .png that is not a PNG reaches no
  ;; provider as one.
  (when (and (vectorp octets) (not (stringp octets)))
    (flet ((lead (&rest bytes)
             (and (>= (length octets) (length bytes))
                  (loop for byte in bytes
                        for index from 0
                        always (or (null byte) (eql byte (aref octets index)))))))
      (cond ((lead #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A) "image/png")
            ((lead #xFF #xD8 #xFF) "image/jpeg")
            ((lead #x47 #x49 #x46 #x38) "image/gif")
            ((lead #x52 #x49 #x46 #x46 nil nil nil nil #x57 #x45 #x42 #x50)
             "image/webp")))))

(defun png-dimensions (octets &aux (bytes (and (typep octets 'sequence) (coerce octets 'vector))))
  "(values WIDTH HEIGHT) from the IHDR chunk of PNG OCTETS, or NIL."
  ;; IMAGE-MEDIA-TYPE is the one reader of the signature.
  (when (and (equal (image-media-type bytes) "image/png") (>= (length bytes) 24)
             (every #'= (map 'vector #'char-code "IHDR") (subseq bytes 12 16)))
    (let ((width (%unsigned bytes 16 4)) (height (%unsigned bytes 20 4)))
      (when (and (plusp width) (plusp height))
        (values width height)))))

(defun %jpeg-dimensions (bytes &aux (index 2)
                                    (end (length bytes)))
  "(values WIDTH HEIGHT) from the first frame header of JPEG BYTES: segments
are walked by their own declared lengths until a start-of-frame marker,
which carries the height before the width."
  (loop
    (when (>= (+ index 9) end) (return nil))
    (let ((marker (aref bytes (1+ index))))
      (cond ((/= #xFF (aref bytes index)) (return nil))
            ;; A run of #xFF before the marker byte is fill, not a segment.
            ((= #xFF marker) (incf index))
            ;; SOF0, SOF1, SOF2 — baseline, extended, progressive.
            ((<= #xC0 marker #xC2)
             (return (values (%unsigned bytes (+ index 7) 2)
                             (%unsigned bytes (+ index 5) 2))))
            ;; The restart markers, SOI, EOI and TEM stand alone: no length.
            ((or (<= #xD0 marker #xD9) (= marker #x01)) (incf index 2))
            (t (let ((length (%unsigned bytes (+ index 2) 2)))
                 (when (< length 2) (return nil))
                 (incf index (+ 2 length))))))))

(defun %gif-dimensions (bytes)
  "(values WIDTH HEIGHT) from the logical screen descriptor of GIF BYTES."
  (when (>= (length bytes) 10)
    (values (%unsigned bytes 6 2 t) (%unsigned bytes 8 2 t))))

(defun %webp-dimensions (bytes)
  "(values WIDTH HEIGHT) from the first chunk of WebP BYTES: lossy (VP8 ),
lossless (VP8L) and extended (VP8X) each declare the size their own way."
  (when (>= (length bytes) 30)
    (nlk:dispatch (map 'string #'code-char (subseq bytes 12 16)) string=
      ("VP8 " (values (logand (%unsigned bytes 26 2 t) #x3FFF)
                      (logand (%unsigned bytes 28 2 t) #x3FFF)))
      ("VP8L" (let ((bits (%unsigned bytes 21 4 t)))
                (values (1+ (logand bits #x3FFF))
                        (1+ (logand (ash bits -14) #x3FFF)))))
      ("VP8X" (values (1+ (%unsigned bytes 24 3 t))
                      (1+ (%unsigned bytes 27 3 t)))))))

(defun image-dimensions (octets &aux (bytes (and (typep octets 'sequence) (coerce octets 'vector))))
  "(values WIDTH HEIGHT) in pixels for PNG, JPEG, GIF or WebP OCTETS, read
from the format's own header, or NIL when the bytes open none of them or
the header does not carry it."
  (let ((media (and bytes (image-media-type bytes))))
    (cond ((equal media "image/png") (png-dimensions bytes))
          ((equal media "image/jpeg") (%jpeg-dimensions bytes))
          ((equal media "image/gif") (%gif-dimensions bytes))
          ((equal media "image/webp") (%webp-dimensions bytes)))))

;;; --- what the graphics protocol can carry --------------------------------
;;; The kitty protocol takes PNG and raw pixel buffers, nothing else, so a
;;; JPEG, GIF or WebP reaches the terminal only through a transcoder. Named
;;; here, beside the transmitter whose format requirement it serves, so the
;;; producer of a preview (exec.lisp IMAGE-PREVIEW-PNG) and the shell that
;;; draws it agree on one answer.

;;; Data, so an operator's layer can name another. None of them is a
;;; dependency — with none installed a non-PNG image simply shows no inline
;;; preview, and the card says so.
;;; An animation is its first frame: one PNG is wanted, and ImageMagick
;;; writes a numbered file per frame unless one is picked, ffmpeg refuses
;;; to. None of them is told to turn the picture upright: an EXIF
;;; orientation is the reader's to apply (engine/image-fit.lisp FIT-IMAGE),
;;; once.
(defparameter *png-transcoders*
  '(("magick" :first-frame :target)
    ("convert" :first-frame :target)
    ("ffmpeg" "-v" "error" "-y" "-noautorotate" "-i" :source "-frames:v" "1" :target)
    ("sips" "-s" "format" "png" :source "--out" :target))
  "The programs tried, in order, to turn an image file into a PNG: each a
command whose :SOURCE is the file read, :FIRST-FRAME that file's first
frame as ImageMagick names it, and :TARGET the .png written. sips is
macOS's own.")

(defun transcode-to-png (source target)
  "Write the image file SOURCE to TARGET as a PNG through the first of
*PNG-TRANSCODERS* that runs and leaves PNG bytes there."
  ;; => TARGET on
  ;; success, NIL when every transcoder is missing or refused the file.
  (dolist (command *png-transcoders*)
    (let ((argv (sublis `((:source . ,(namestring source)) (:target . ,(namestring target))
                          (:first-frame . ,(format nil "~a[0]" (namestring source))))
                        command)))
      (when (and (nlk:exits-zero-p argv)
                 (equal "image/png" (image-media-type (file-octets target))))
        (return target)))))


;;; --- transmission --------------------------------------------------------

(defun transmit-image (encoded &key stream)
  "Transmit ENCODED, a base64 PNG, to the terminal under a fresh id; answers
a TERMINAL-IMAGE carrying the pixel dimensions its PNG header declares."
  (nlk:bind (((width height)
              (png-dimensions (ignore-errors (cl-base64:base64-string-to-usb8-array encoded))))
             (id (incf *kitty-image-id-counter*))
             (len (length encoded))
             (dims (if width (format nil ",s=~D,v=~D" width height) "")))
    ;; The first chunk carries the keys (and m=1 when more follow); the rest m= alone.
    (loop for start from 0 below (max len 1) by +kitty-chunk-size+
          for end = (min (+ start +kitty-chunk-size+) len)
          do (if (zerop start)
                 (format stream "~C_Gf=100~A,i=~D~:[~;,m=1~];~A~C\\" #\Escape dims id
                         (< end len) (subseq encoded 0 end) #\Escape)
                 (format stream "~C_Gm=~D;~A~C\\" #\Escape (if (= end len) 0 1)
                         (subseq encoded start end) #\Escape)))
    (force-output stream)
    (make-terminal-image :id id :width width :height height)))

;;; --- placement -----------------------------------------------------------

;; Placement ids are namespaced per image: distinct ids show one image in several places.
(nlk:define-record (image-placement (:constructor make-image-placement
                                         (image &key (x 0) (y 0) rows (placement-id 1)))
                                    (:export :constructor image placement-id x y rows))
  "A transmitted image shown at a cell position."
  (image nil :type (or null terminal-image))
  (placement-id 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (rows nil :type (or null fixnum)))

(nlk:access (p image-placement))

(defun place-image (image x y &key rows placement-id stream)
  "Show IMAGE at cell X, Y: cursor there, then a=p with the geometry it was
given — each key omitted rather than defaulted when its argument is NIL —
and C=1 so the cursor does not move with the image."
  (format stream
          "~C[~D;~DH~C_Ga=p,i=~D~@[,p=~D~]~@[,r=~D~],C=1~C\\"
          #\Escape (1+ y) (1+ x) #\Escape image.id placement-id rows #\Escape)
  (force-output stream))

(defun delete-image-placements (image &key placement-id stream)
  "Remove IMAGE's placements, or just PLACEMENT-ID, keeping the data."
  (format stream "~C_Ga=d,d=i,i=~D~@[,p=~D~]~C\\" #\Escape image.id placement-id #\Escape)
  (force-output stream))

(define-escape (clear-kitty-images :flush t) ("_Ga=d" "\\")
  "Delete every image and placement the terminal holds.")

;;; --- capability query ----------------------------------------------------

(define-escape (request-kitty-graphics :flush t) ("_Gi=1,s=1,v=1,a=q,t=d,f=24;AAAA" "\\")
  ;; A supporting terminal answers an APC the decoder turns into a
  ;; KITTY-GRAPHICS-MSG, and silence means no support.
  ;;
  ;; The pixel is load-bearing. a=q means `try to load this image and answer
  ;; OK or the error', so a query carrying nothing to load is an error, not a
  ;; capability answer: kitty 0.47.1 replies `EINVAL:Zero width/height not
  ;; allowed' to a bare i=1,a=q. The reply is an APC either way, so the
  ;; query would look answered while OK-P stayed false, and every terminal
  ;; would read as having no graphics, so no inline preview would appear.
  ;; Nothing is stored: a=q neither replaces the image under this id nor
  ;; keeps what it loaded.
  "Ask whether the terminal speaks kitty graphics: the query action over a
single RGB pixel (i=1,s=1,v=1,a=q,t=d,f=24 with the pixel as the payload).")

;;; --- the renderer seam ---------------------------------------------------

(defun %render-image-placements (old new stream &key force)
  "Delete the placements in OLD that NEW dropped and place the ones NEW
added; with FORCE, delete and re-place everything (after a full paint)."
  ;; Answers NEW. A placement is its values: EQUALP reads through to the image.
  (dolist (p (if force old (set-difference old new :test #'equalp)))
    (delete-image-placements p.image :placement-id p.placement-id :stream stream))
  (dolist (p (if force new (set-difference new old :test #'equalp)))
    (place-image p.image p.x p.y :rows p.rows :placement-id p.placement-id :stream stream))
  new)
