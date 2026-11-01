;;;; webp-test.lisp --- WebP read here, pixel for pixel as libwebp reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is WEBP-DECODE's contract: each of the format's forms
;;;; -- lossy, lossy under an ALPH chunk, lossless with and without alpha, a
;;;; palette, an animation's first frame -- decodes to exactly the pixels
;;;; libwebp gives (each fixture was written and read back by PIL over libwebp,
;;;; the pixels kept as their SHA-256); a file cut short, bytes that open no
;;;; WebP and a canvas past *WEBP-MAX-PIXELS* are refused as WEBP-MALFORMED,
;;;; and through WEBP-PIXELS as IMAGE-UNFIT; and FIT-IMAGE shrinks a WebP past
;;;; 2000 px a side with no transcoder on the box.

(in-package #:nodecode.test)

(defparameter *webp-fixtures*
  '(("lossy" 37 23 3 "4504aa75f8cb7d94267c86b1467876e967ef3a2a9ab0382579b6922a03317c08"
     "UklGRgoBAABXRUJQVlA4IP4AAACQCACdASolABcAPoU2lEelIyIhNVgMAKAQiWwAnTKEd6ex+YJSf7HrCJUIWAj80N7/pxkJvNUhwctL3fObO67j66xXRcqUItpQ/q33zgD+Zgy/LFZdj/LTkaZKX4AdG/T9+QTqvozP2Su14Blx1HScICbcQt33rhSS40comqFMD4XP6gbB2pFv79hu1CTn0FCWix1Rd4i18oieAqF56KA89Cuw5OA3xlCLTMmvXB1wmpseegGqaaL+joijQpUEuPLE6AkjFq9XtlOAi/YTprQGCj8GFUBA2N1+ANJcuZIlD+cJyp48PXdpr3gsjSeAFGwRx9gZQBlJwviq8AAAAA==")
    ("lossy-alpha" 19 13 4 "34cb4a82bc34875dbd32f377d1934f61b5da270c33090ba5e71f7a2fd94f71af"
     "UklGRiABAABXRUJQVlA4WAoAAAAQAAAAEgAADAAAQUxQSJwAAAABuSZE9D8McGvbWha9z3AnJ6QVGqAbYiIKowMWkbu7zMwn418L70ZMgMK2bZCOIY9Q5o0y5W5loK+HRzaejIqaOsIdmiQEnmN9gOUFRFIaCLcpEvkJCxwQQTGauuZaBBJ4FgCHiaRAc6oUCP9XDV7J7x3A3ndIimYEOTFGM0CSYgxrLEUZgyE5McZg4CXGMBS+ijFQpISjGANFSgBWUDggXgAAADAEAJ0BKhMADQA+tUieSackIqEwCADgFolsAJ0yhHAiwADt/tE72R2SWgD+xCetww5sscHWtK6Kv5IoWJ3GCJRh0h5xu8iGYXOYbz1cjZEnQbhu1iA0QF9y+DUAAAA=")
    ("lossless" 21 11 3 "21a08b0cb525a1a82d3333614c574db9e91717410243520f6459106fd84d7df7"
     "UklGRioDAABXRUJQVlA4TB4DAAAvFIACADcHybbdtg0ANtvpve1/M1lBxsmo95CQQOBvA24kSYqtgd25x/y0T/6b8Y34On+JaWkAbgCwbRsA7MXpW07Ilv+3XJAf3Hu3WUDMfzzc/0ZHglZyZ7qxgAF1AiIaieeAWUlzwV5o1sT28qxJJraUsGvcK9gR/kUpDsdFd0Gj4u4Mccg90WwmTUqUEbQA4m9CP4RkLSYqeIgJnis0rGjIuwEyLIxVz7wNg8EfOCmUkogWsizIyk/zH/BQvIi5qhSr0wDHKkQrt0ZRa03Zy2jGp5MFq8Byu9HZQ1kBExSLITIDgZjQNMOL0TsiBwzWobI8GsCNFoT1BdtDXJBNCBMvpibg1d+PgiZQvTetuCGdoYZHeeEcrS48bW0Z7FTbMoBr+1xghlQE93fbX4cHEk/woTkHAtESzMMKY4+nQVf/9mUvydPml1QszlCwguCoDJL6ieeLEg/2YTzbl/YTyNvDk+zNv5UiR7LqDlEQFoZNyoIqgw8b0moipBaJlumpKbJigJUyPCtspI6Idaw2ja1+YwPpht0SIXTkzDAVnjTgQJLIUuSeja1oi6ENaxuI0AP+CFAGef0pO2hRgVdt+/Rym6F4KZIfI66Botbz8BUsmRO/zRJsRcNYxLqb9uVw2TAJ3bCrO+NkObVPd4SggjV/Tv4OSsWVuPkjU1OFRXR5NdzVK4bCoMsrI3PFiSgyq2zv6dt8ze5pLVfGIX70DBADACCYzLazbdu2bdu2bdu2bdu2uymi/7F/eKLs7RcQmIYi4CkTAODQwHPz997BsY+Xb3JKajoGNzkOfh/9ePpq291SUPTw9HcJnyNhydRp0PBbQ3/r6ZGKqnySu1NQKHKGCJkaef0cbGrf2VSSkrGycIsIy55Fw6CMsZfa6pbtDXU5aUtzM1fnkAwEnMZEb6yo7Dhb15G1TjQyjVxcyMrMzWN5rKvq67zW1FATF0swEYxaCs5hz2dlLq/v7b5c0dK2FY0XMuaP5p3n5mK7f+iq6bm6WLOXlNCNixUw5HMs4OS4uy37+b85X1410NMXsRGOsXPgKSwqLin9/QM=")
    ("lossless-alpha" 17 9 4 "f0123a5e0227e5367428c2d28c38df996fea81744fa2806470146b4ec2550e3d"
     "UklGRoQCAABXRUJQVlA4THcCAAAvEAACEEfkIJIkRRo4ZjDx/vW8iOW7WxobjiPbVqKnuEMS5B/Q7GaHO3xx2EaSIs0N7M7tM0MQn38K/3kwMyzCEQCAQJRt27ZtrBxr7AetPaAHtPWGnqCZ09m+bDv+vwUweWIzurFV3JNpEhuG83AWfH5Lj31QJFO8k0EE0qsEW0Y+CFIA5b9eVXW2LB96el2VdlDvJTS5i6Y5nSOpY1VGIPafw8+azA8xuoIRkUnjDpc8oMkqjLERxIPxVaAFE5DMF7Iq79qNtwa7dI4fxD+3j3TjA/+Tj8UhouuAP0SQhIY0EtIMYkgcfXv1pp3svRx6ErRYyWXzKUqyq73BmG5j2mFYU6eeqS+cc2XLe/aliwZCHOlBgxcGb8+BIwKvCJIbgUIK2MEeOTLjNk0V4qrLAv4avOGPDy8S/aIyVotTk+NE5keEwGy4XAl78XRZ/c8pfFCn/RPfSPghOFLdofAXAAhvb8cRwbM16xiknVE4xiN+H+QtsMQP22dOV/+hiNNpZ4nyXeV4d7h49EomTlfAS5/4x1isHDfAQltCvbUmrT5SPPl/niCLP/K8Zn/1fmzy/JkGvWtLf8XXJN+xv0bbHK0cFCCz1NfPN+scu7z9PH1s5RcGcV52CB6x7yh3gC1Jsmlb2LFt27Zt2/a1bWMi+h8AsL93dnev06g/cSj4pQDA7vj8VG/QfiuwaAT5HwCyJ/XbG6Pz50sJeCR1vTo8SDWur0wOuwqkGMLfdpHLHFVrFw8umxBE8g/icpNPJ6KhwOWjFSRiGY0+GxaKlVI76PeaLQL+G4kxmQ+SsXK41fSB+4X3zmFOx6N+vBfpdMHz+vzEZbMAAA==")
    ("palette" 23 9 3 "a130ce08bcc37c64e9c1303a6565551bb094f97e98e7480d97ea346015b49715"
     "UklGRlYAAABXRUJQVlA4TEkAAAAvFgACAB8wUAM1UPMfeHdgENtWIiSAE4Ea8CNg1Yggu9U2gmuIiP6rTduAGVP0CLQTmguzf1NZ2JvqSt/0oRP9EP8h/UPmQ2ABAA==")
    ("animation" 24 16 3 "21a731d7a1f7532b6e55943de6a6ae91659a08df04c0689c309d2ed651481f61"
     "UklGRiQBAABXRUJQVlA4WAoAAAACAAAAFwAADwAAQU5JTQYAAAAAAAAAAABBTk1GogAAAAAAAAAAABcAAA8AADIAAAJWUDggigAAABAEAJ0BKhgAEAAucU6nU6IlpaWFgHBLYqa8BIcm/x14Ks2w2REgAbxgAP7wwZtee1/JPTIS+Bdmo/X1QI7HsgwAVMmjO05ESTUKG7/9rcogCeZDxj6fVBQ61e3f4///Bu3/6XO98v/63M+i16/LODVpcg8BpjA3n9LJDCHl9l1xPpsZqmAMpFgAAEFOTUZOAAAAAAAAAAAAFwAADwAAMgAAAFZQOCA2AAAA1AIAnQEqGAAQAD6dPppIgwCAAAE4lAFqLQD8AAEL4AD+lxf/CrcMQzVf/OwWqs33pO15QAAA")
)
  "Each fixture's name, width, height, channels, the SHA-256 of the pixels
libwebp decodes it to, and the file as base64.")

(defparameter *wide-webp*
  "UklGRigAAABXRUJQVlA4TBsAAAAvX8kDAAdQraIUtP8BBWkbMPUvfzei/8n91y8A"
  "A 2400 x 16 lossless WebP of one flat color, as base64.")

(defun webp-fixture (name)
  (cl-base64:base64-string-to-usb8-array (sixth (assoc name *webp-fixtures* :test #'string=))))

(deftest webp-decodes-every-form-to-libwebps-pixels ()
  (loop for (name width height channels digest) in *webp-fixtures*
        do (multiple-value-bind (pixels w h c) (nle::webp-decode (webp-fixture name))
             (is (and (eql w width) (eql h height) (eql c channels))
                 (format nil "~a is ~dx~d of ~d channels, not ~dx~d of ~d" name width height channels w h c))
             (is (string= digest (subseq (nlk::sha256-text pixels) 7))
                 (format nil "~a decodes to the pixels libwebp gives" name)))))

(deftest webp-pixels-hands-fit-image-its-pixels ()
  (is-present (pixels (nle::webp-pixels (webp-fixture "lossy-alpha"))) "the PIXELS of a lossy WebP with alpha"
    (is (nle::pixels-p pixels) "a PIXELS")
    (is (equal (list 19 13 4) (list (nle::pixels-width pixels) (nle::pixels-height pixels) (nle::pixels-channels pixels)))
        "19 x 13, RGBA")))

(deftest webp-refuses-what-it-cannot-read ()
  (let ((lossy (webp-fixture "lossy")))
    (signals-error nle::webp-malformed (nle::webp-decode (subseq lossy 0 60)))
    (signals-error nle::webp-malformed (nle::webp-decode (subseq (webp-fixture "lossless") 0 200)))
    (signals-error nle::webp-malformed
      (nle::webp-decode (map '(vector (unsigned-byte 8)) #'char-code "RIFF\0\0\0\0WEBPJUNK\0\0\0\0\0\0\0\0")))
    (signals-error nle::image-unfit (nle::webp-pixels (subseq lossy 0 60)))
    (let ((nle::*webp-max-pixels* 100))
      (is (search "past the 100 pixels" (refusal-text nle::webp-malformed (nle::webp-decode lossy)))
          "a canvas past the ceiling is refused from its header"))))

(deftest fit-image-shrinks-a-webp-with-no-transcoder ()
  (let ((nlt:*png-transcoders* '()))
    (multiple-value-bind (octets media-type width height)
        (nle::fit-image (cl-base64:base64-string-to-usb8-array *wide-webp*))
      (is (equal (list 2000 13) (list width height)) "2400 x 16 shrunk to 2000 x 13")
      (is (member media-type '("image/png" "image/jpeg") :test #'equal) "re-encoded as PNG or JPEG")
      (is (equal (list 2000 13) (multiple-value-list (nlt:image-dimensions octets)))
          "the bytes sent are that size"))))
