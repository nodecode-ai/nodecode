;;;; image-test.lisp --- kitty graphics.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from the kitty cases of cl-tuition's tests/test-terminal-caps.lisp
;;;; (Copyright (C) 2025 Anthony Green, MIT).

(in-package #:nodecode.test)

(nlk:access (image nlt::terminal-image) (p1 nlt::image-placement) (p2 nlt::image-placement))

(defparameter +png-1x1-base64+
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII=")

(deftest png-dimensions-read-the-ihdr-chunk ()
  (multiple-value-bind (w h)
      (nlt:png-dimensions (cl-base64:base64-string-to-usb8-array +png-1x1-base64+))
    (is (and (= 1 w) (= 1 h)) "a 1x1 PNG"))
  (is (null (nlt:png-dimensions (make-array 30 :initial-element 0))) "not a PNG"))

(defparameter +jpeg-600x300+
  ;; SOI, then an SOF0 whose frame header carries the height before the width.
  (octets #xFF #xD8 #xFF #xC0 #x00 #x11 #x08 #x01 #x2C #x02 #x58 #x03
          #x01 #x11 #x00 #x02))

(defparameter +gif-32x16+
  (octets #x47 #x49 #x46 #x38 #x39 #x61 #x20 #x00 #x10 #x00))

(defparameter +webp-64x48+
  (concatenate '(vector (unsigned-byte 8))
               (map 'vector #'char-code "RIFF")
               (octets 0 0 0 0)
               (map 'vector #'char-code "WEBPVP8 ")
               (make-array 10 :initial-element 0)
               (octets #x40 #x00 #x30 #x00)))

(deftest image-bytes-say-what-they-are-and-how-big ()
  ;; The bytes decide, never the name: one authority the LOOK verb, the
  ;; channel adapters and the terminal carrier all read.
  (is-each (nlt:image-media-type)
    ((cl-base64:base64-string-to-usb8-array +png-1x1-base64+) "image/png" "a PNG")
    (+jpeg-600x300+ "image/jpeg" "a JPEG")
    (+gif-32x16+ "image/gif" "a GIF")
    (+webp-64x48+ "image/webp" "a WebP")
    ((octets 1 2 3 4 5 6 7 8 9 10 11 12) nil "no signature, no media type")
    ("iVBORw0KGgo" nil "a string is not bytes"))
  (flet ((size (octets)
           (multiple-value-bind (w h) (nlt:image-dimensions octets)
             (and w (list w h)))))
    (is-each (size)
      ((cl-base64:base64-string-to-usb8-array +png-1x1-base64+) '(1 1) "the IHDR chunk")
      (+jpeg-600x300+ '(600 300) "the SOF frame header, height first")
      (+gif-32x16+ '(32 16) "the logical screen descriptor")
      (+webp-64x48+ '(64 48) "the VP8 chunk")
      ((octets 1 2 3 4 5 6 7 8 9 10 11 12) nil "nothing to measure"))))

(deftest a-png-carries-itself-and-anything-else-needs-a-transcoder ()
  ;; The graphics protocol takes PNG and raw pixels, nothing else, so the
  ;; transcoder list is the whole of what makes a JPEG showable — and with
  ;; none of them present the answer is NIL, never a half-written file.
  (with-temp-file (source :type "jpg")
    (with-temp-file (target :type "png")
      (alexandria:write-byte-vector-into-file +jpeg-600x300+ source)
      (is (equal (coerce +jpeg-600x300+ 'list)
                 (coerce (nlt:file-octets source) 'list)))
      (is (null (nlt:file-octets source 4)) "and refuses one past the bound")
      (is (null (nlt:file-octets (temp-path "carry-absent" "png"))))
      (let ((nlt:*png-transcoders* '(("nodecode-no-such-transcoder" :source :target))))
        (is (null (nlt:transcode-to-png source target))))
      (let ((nlt:*png-transcoders*
              (list (list "sh" "-c"
                          (format nil "cat ~a > \"$1\"" (namestring source))
                          "sh" :target))))
        (is (null (nlt:transcode-to-png source target)))))))

(deftest transmit-sends-one-apc-for-a-small-image ()
  (let* ((out (make-string-output-stream))
         (image (nlt:transmit-image +png-1x1-base64+ :stream out))
         (written (get-output-stream-string out)))
    (is (nlt:terminal-image-p image) "a terminal image comes back")
    (is (and (= 1 image.width) (= 1 image.height)))
    (is-table (in) (search in written) ("_Gf=100" "PNG is format 100")
      ((format nil ",i=~D;" image.id) "under its id") (+png-1x1-base64+ "carrying the payload"))
    (is (null (search ",m=1;" written)) "in one piece")))

(deftest transmit-chunks-a-large-payload ()
  (is-carrying (written (with-output-to-string (out)
                          (nlt:transmit-image (make-string 5000 :initial-element #\A) :stream out)))
    (",m=1;" "the first chunk says more follows") ("_Gm=0;" "the last chunk says it is the last")))

(deftest place-and-delete-name-the-image-and-the-geometry ()
  (let ((image (nlt:transmit-image +png-1x1-base64+ :stream (make-string-output-stream))))
    (let ((written (with-output-to-string (out)
                     (nlt:place-image image 4 2 :rows 3 :placement-id 7 :stream out))))
      (is (search (csi "3;5H") written) "the cursor goes to the cell, 1-based")
      (is (search (format nil "_Ga=p,i=~D,p=7,r=3,C=1" image.id) written)))
    (let ((written (with-output-to-string (out)
                     (nlt:place-image image 4 2 :rows 3 :placement-id 7 :crop-y 10 :crop-height 30 :stream out))))
      (is (search (format nil "_Ga=p,i=~D,p=7,r=3,y=10,h=30,C=1" image.id) written) "a cut placement names its source rectangle's pixel rows"))))

(deftest the-graphics-query-loads-a-pixel-so-the-answer-can-be-ok ()
  ;; a=q asks the terminal to LOAD an image and answer OK or the error, so
  ;; the query must carry one: kitty answers a payload-less query
  ;; `EINVAL:Zero width/height not allowed', which is an APC that is not OK.
  (is (search "_Gi=1,s=1,v=1,a=q,t=d,f=24;AAAA"
              (with-output-to-string (out) (nlt:request-kitty-graphics out nil))))
  (is (string= "" (with-output-to-string (out) (nlt:request-kitty-graphics out :tmux))) "tmux would take the bare APC as the pane title")
  (is (string= "" (with-output-to-string (out) (nlt:request-kitty-graphics out :screen))) "screen would take it as its hardstatus")
  (let ((msg (funcall (nlt:terminal-io-cmd #'nlt:request-kitty-graphics))))
    (is (nlt:terminal-io-msg-p msg) "the command asks the loop thread to write it")))

(deftest placement-diff-deletes-the-dropped-and-places-the-added ()
  (let* ((image (nlt:transmit-image +png-1x1-base64+ :stream (make-string-output-stream)))
         (p1 (nlt:make-image-placement image :x 0 :y 0 :rows 2))
         (p2 (nlt:make-image-placement image :x 5 :y 1 :rows 2)))
    (flet ((painted (old new &rest keys)
             (with-output-to-string (out)
               (apply #'nlt::%render-image-placements old new out keys))))
      (is (string= "" (painted (list p1) (list p1))) "an unchanged list emits nothing")
      (let ((written (painted (list p1) (list p2))))
        (is (search (format nil "_Ga=d,d=i,i=~D,p=~D" image.id p1.placement-id) written))
        (is (search (format nil "_Ga=p,i=~D,p=~D" image.id p2.placement-id) written)))
      (is (search "_Ga=p" (painted (list p2) (list p2) :force t))))))
