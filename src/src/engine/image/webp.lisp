;;;; webp.lisp --- WebP read here: lossy, lossless, alpha, an animation's first frame.
;;;;
;;;; SPDX-License-Identifier: MIT AND BSD-3-Clause
;;;;
;;;; FIT-IMAGE (image-fit.lisp) shrinks a picture once it has its pixels,
;;;; and no Lisp library reads WebP, the format a browser saves a picture
;;;; off the web as: without this file such a picture needs ImageMagick or
;;;; ffmpeg on the box. WEBP-PIXELS is the decoder *IMAGE-DECODERS* names
;;;; for image/webp; WEBP-DECODE beneath it reads all three of the format's
;;;; forms:
;;;;
;;;;   VP8   lossy: one VP8 keyframe (RFC 6386) -- the boolean entropy
;;;;         decoder, intra prediction, the inverse DCT and WHT, the loop
;;;;         filter -- then YUV 4:2:0 to RGB;
;;;;   VP8L  lossless (RFC 9649): prefix codes, LZ77 backward references,
;;;;         the color cache and the four transforms;
;;;;   VP8X  the extended container: the ALPH chunk's alpha under a lossy
;;;;         picture, and of an animation (ANIM, ANMF) its first frame.
;;;;
;;;; Ported from Go's golang.org/x/image v0.47.0, packages vp8, vp8l and webp
;;;; (https://pkg.go.dev/golang.org/x/image), under the license at the end of
;;;; this file. Where Go and libwebp part (https://chromium.googlesource.com/
;;;; webm/libwebp), the decoder browsers and PIL run, this follows libwebp,
;;;; so a picture decodes to the pixels everyone else sees: its YUV-to-RGB
;;;; arithmetic and "fancy" chroma upsampling, its refusal of a prefix code
;;;; that is not complete, its canonical codes for the simple code form, its
;;;; predictors 14 and 15, and an animation's first frame on a transparent
;;;; canvas.

(in-package #:nodecode.evolved)

;;; --- refusals and buffers ----------------------------------------------------

(define-error webp-malformed (error) (reason)
  (:report "~a" reason)
  (:documentation
   "Bytes WEBP-DECODE cannot read as a WebP, REASON saying why."))

(defun %webp-fail (control &rest arguments)
  (error 'webp-malformed :reason (apply #'format nil control arguments)))

(defparameter *webp-max-pixels* (* 8000 8000)
  "The most pixels WEBP-DECODE builds a picture of. A larger canvas is
refused from its header, before a byte of it is allocated: the format
allows 16384 x 16384, whose planes alone would take a gigabyte of heap.")

(defun %webp-check-size (width height)
  (when (> (* width height) *webp-max-pixels*)
    (%webp-fail "a WebP of ~:dx~:d, past the ~:d pixels one is decoded at"
                width height *webp-max-pixels*)))

(declaim (inline %webp-octets))
(defun %webp-octets (length)
  (make-array length :element-type '(unsigned-byte 8) :initial-element 0))

(defun %webp-le (data at &optional (bytes 3))
  "The BYTES-byte little-endian number in DATA at AT."
  (loop for k below bytes sum (ash (aref data (+ at k)) (* 8 k))))

(defmacro %webp-table (name element-type documentation &rest values)
  "Define NAME, a global vector of ELEMENT-TYPE holding VALUES."
  `(progn
     (declaim (type (simple-array ,element-type (,(length values))) ,name))
     (sb-ext:defglobal ,name
         (make-array ,(length values) :element-type ',element-type
                                      :initial-contents ',values)
       ,documentation)))

;;; --- tables ----------------------------------------------------------------------

(%webp-table *vp8-zigzag* (unsigned-byte 8)
  "The raster position of each coefficient of a 4x4 block, in coding order."
  0 1 4 8 5 2 3 6 9 12 13 10 7 11 14 15)

(%webp-table *vp8-bands* (unsigned-byte 8)
  "The probability band of each coefficient position, a sentinel last."
  0 1 2 3 6 4 5 6 6 6 6 6 6 6 6 7 0)

(%webp-table *vp8l-code-length-order* (unsigned-byte 8)
  "The order the code length code's own lengths are stored in."
  17 18 0 1 2 3 4 5 16 6 7 8 9 10 11 12 13 14 15)

(%webp-table *vp8-dc-quant* (unsigned-byte 16)
  "The DC dequantization factor of each quantizer index."
  4 5 6 7 8 9 10 10 11 12 13 14 15 16 17 17
  18 19 20 20 21 21 22 22 23 23 24 25 25 26 27 28
  29 30 31 32 33 34 35 36 37 37 38 39 40 41 42 43
  44 45 46 46 47 48 49 50 51 52 53 54 55 56 57 58
  59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
  75 76 76 77 78 79 80 81 82 83 84 85 86 87 88 89
  91 93 95 96 98 100 101 102 104 106 108 110 112 114 116 118
  122 124 126 128 130 132 134 136 138 140 143 145 148 151 154 157)

(%webp-table *vp8-ac-quant* (unsigned-byte 16)
  "The AC dequantization factor of each quantizer index."
  4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
  20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
  36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51
  52 53 54 55 56 57 58 60 62 64 66 68 70 72 74 76
  78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108
  110 112 114 116 119 122 125 128 131 134 137 140 143 146 149 152
  155 158 161 164 167 170 173 177 181 185 189 193 197 201 205 209
  213 217 221 225 229 234 239 245 249 254 259 264 269 274 279 284)

(%webp-table *vp8-cat3456* (unsigned-byte 8)
  "The extra-bit probabilities of DCT_CAT3..6, twelve a category, zero-ended."
  173 148 140 0 0 0 0 0 0 0 0 0
  176 155 140 135 0 0 0 0 0 0 0 0
  180 157 141 134 130 0 0 0 0 0 0 0
  254 254 243 230 196 177 153 140 133 130 129 0)

(%webp-table *vp8-coefficient-update-probabilities* (unsigned-byte 8)
  "The probability that a frame updates each coefficient probability, [plane][band][context][token]."
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  176 246 255 255 255 255 255 255 255 255 255
  223 241 252 255 255 255 255 255 255 255 255
  249 253 253 255 255 255 255 255 255 255 255
  255 244 252 255 255 255 255 255 255 255 255
  234 254 254 255 255 255 255 255 255 255 255
  253 255 255 255 255 255 255 255 255 255 255
  255 246 254 255 255 255 255 255 255 255 255
  239 253 254 255 255 255 255 255 255 255 255
  254 255 254 255 255 255 255 255 255 255 255
  255 248 254 255 255 255 255 255 255 255 255
  251 255 254 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 253 254 255 255 255 255 255 255 255 255
  251 254 254 255 255 255 255 255 255 255 255
  254 255 254 255 255 255 255 255 255 255 255
  255 254 253 255 254 255 255 255 255 255 255
  250 255 254 255 254 255 255 255 255 255 255
  254 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  217 255 255 255 255 255 255 255 255 255 255
  225 252 241 253 255 255 254 255 255 255 255
  234 250 241 250 253 255 253 254 255 255 255
  255 254 255 255 255 255 255 255 255 255 255
  223 254 254 255 255 255 255 255 255 255 255
  238 253 254 254 255 255 255 255 255 255 255
  255 248 254 255 255 255 255 255 255 255 255
  249 254 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 253 255 255 255 255 255 255 255 255 255
  247 254 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 253 254 255 255 255 255 255 255 255 255
  252 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 254 254 255 255 255 255 255 255 255 255
  253 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 254 253 255 255 255 255 255 255 255 255
  250 255 255 255 255 255 255 255 255 255 255
  254 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  186 251 250 255 255 255 255 255 255 255 255
  234 251 244 254 255 255 255 255 255 255 255
  251 251 243 253 254 255 254 255 255 255 255
  255 253 254 255 255 255 255 255 255 255 255
  236 253 254 255 255 255 255 255 255 255 255
  251 253 253 254 254 255 255 255 255 255 255
  255 254 254 255 255 255 255 255 255 255 255
  254 254 254 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 254 255 255 255 255 255 255 255 255 255
  254 254 255 255 255 255 255 255 255 255 255
  254 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  254 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  248 255 255 255 255 255 255 255 255 255 255
  250 254 252 254 255 255 255 255 255 255 255
  248 254 249 253 255 255 255 255 255 255 255
  255 253 253 255 255 255 255 255 255 255 255
  246 253 253 255 255 255 255 255 255 255 255
  252 254 251 254 254 255 255 255 255 255 255
  255 254 252 255 255 255 255 255 255 255 255
  248 254 253 255 255 255 255 255 255 255 255
  253 255 254 254 255 255 255 255 255 255 255
  255 251 254 255 255 255 255 255 255 255 255
  245 251 254 255 255 255 255 255 255 255 255
  253 253 254 255 255 255 255 255 255 255 255
  255 251 253 255 255 255 255 255 255 255 255
  252 253 254 255 255 255 255 255 255 255 255
  255 254 255 255 255 255 255 255 255 255 255
  255 252 255 255 255 255 255 255 255 255 255
  249 255 254 255 255 255 255 255 255 255 255
  255 255 254 255 255 255 255 255 255 255 255
  255 255 253 255 255 255 255 255 255 255 255
  250 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255
  254 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255)

(%webp-table *vp8-default-coefficient-probabilities* (unsigned-byte 8)
  "The coefficient probabilities a keyframe starts from, [plane][band][context][token]."
  128 128 128 128 128 128 128 128 128 128 128
  128 128 128 128 128 128 128 128 128 128 128
  128 128 128 128 128 128 128 128 128 128 128
  253 136 254 255 228 219 128 128 128 128 128
  189 129 242 255 227 213 255 219 128 128 128
  106 126 227 252 214 209 255 255 128 128 128
  1 98 248 255 236 226 255 255 128 128 128
  181 133 238 254 221 234 255 154 128 128 128
  78 134 202 247 198 180 255 219 128 128 128
  1 185 249 255 243 255 128 128 128 128 128
  184 150 247 255 236 224 128 128 128 128 128
  77 110 216 255 236 230 128 128 128 128 128
  1 101 251 255 241 255 128 128 128 128 128
  170 139 241 252 236 209 255 255 128 128 128
  37 116 196 243 228 255 255 255 128 128 128
  1 204 254 255 245 255 128 128 128 128 128
  207 160 250 255 238 128 128 128 128 128 128
  102 103 231 255 211 171 128 128 128 128 128
  1 152 252 255 240 255 128 128 128 128 128
  177 135 243 255 234 225 128 128 128 128 128
  80 129 211 255 194 224 128 128 128 128 128
  1 1 255 128 128 128 128 128 128 128 128
  246 1 255 128 128 128 128 128 128 128 128
  255 128 128 128 128 128 128 128 128 128 128
  198 35 237 223 193 187 162 160 145 155 62
  131 45 198 221 172 176 220 157 252 221 1
  68 47 146 208 149 167 221 162 255 223 128
  1 149 241 255 221 224 255 255 128 128 128
  184 141 234 253 222 220 255 199 128 128 128
  81 99 181 242 176 190 249 202 255 255 128
  1 129 232 253 214 197 242 196 255 255 128
  99 121 210 250 201 198 255 202 128 128 128
  23 91 163 242 170 187 247 210 255 255 128
  1 200 246 255 234 255 128 128 128 128 128
  109 178 241 255 231 245 255 255 128 128 128
  44 130 201 253 205 192 255 255 128 128 128
  1 132 239 251 219 209 255 165 128 128 128
  94 136 225 251 218 190 255 255 128 128 128
  22 100 174 245 186 161 255 199 128 128 128
  1 182 249 255 232 235 128 128 128 128 128
  124 143 241 255 227 234 128 128 128 128 128
  35 77 181 251 193 211 255 205 128 128 128
  1 157 247 255 236 231 255 255 128 128 128
  121 141 235 255 225 227 255 255 128 128 128
  45 99 188 251 195 217 255 224 128 128 128
  1 1 251 255 213 255 128 128 128 128 128
  203 1 248 255 255 128 128 128 128 128 128
  137 1 177 255 224 255 128 128 128 128 128
  253 9 248 251 207 208 255 192 128 128 128
  175 13 224 243 193 185 249 198 255 255 128
  73 17 171 221 161 179 236 167 255 234 128
  1 95 247 253 212 183 255 255 128 128 128
  239 90 244 250 211 209 255 255 128 128 128
  155 77 195 248 188 195 255 255 128 128 128
  1 24 239 251 218 219 255 205 128 128 128
  201 51 219 255 196 186 128 128 128 128 128
  69 46 190 239 201 218 255 228 128 128 128
  1 191 251 255 255 128 128 128 128 128 128
  223 165 249 255 213 255 128 128 128 128 128
  141 124 248 255 255 128 128 128 128 128 128
  1 16 248 255 255 128 128 128 128 128 128
  190 36 230 255 236 255 128 128 128 128 128
  149 1 255 128 128 128 128 128 128 128 128
  1 226 255 128 128 128 128 128 128 128 128
  247 192 255 128 128 128 128 128 128 128 128
  240 128 255 128 128 128 128 128 128 128 128
  1 134 252 255 255 128 128 128 128 128 128
  213 62 250 255 255 128 128 128 128 128 128
  55 93 255 128 128 128 128 128 128 128 128
  128 128 128 128 128 128 128 128 128 128 128
  128 128 128 128 128 128 128 128 128 128 128
  128 128 128 128 128 128 128 128 128 128 128
  202 24 213 235 186 191 220 160 240 175 255
  126 38 182 232 169 184 228 174 255 187 128
  61 46 138 219 151 178 240 170 255 216 128
  1 112 230 250 199 191 247 159 255 255 128
  166 109 228 252 211 215 255 174 128 128 128
  39 77 162 232 172 180 245 178 255 255 128
  1 52 220 246 198 199 249 220 255 255 128
  124 74 191 243 183 193 250 221 255 255 128
  24 71 130 219 154 170 243 182 255 255 128
  1 182 225 249 219 240 255 224 128 128 128
  149 150 226 252 216 205 255 171 128 128 128
  28 108 170 242 183 194 254 223 255 255 128
  1 81 230 252 204 203 255 192 128 128 128
  123 102 209 247 188 196 255 233 128 128 128
  20 95 153 243 164 173 255 203 128 128 128
  1 222 248 255 216 213 128 128 128 128 128
  168 175 246 252 235 205 255 255 128 128 128
  47 116 215 255 211 212 255 255 128 128 128
  1 121 236 253 212 214 255 255 128 128 128
  141 84 213 252 201 202 255 219 128 128 128
  42 80 160 240 162 185 255 205 128 128 128
  1 1 255 128 128 128 128 128 128 128 128
  244 1 255 128 128 128 128 128 128 128 128
  238 1 255 128 128 128 128 128 128 128 128)

(%webp-table *vp8-bmode-probabilities* (unsigned-byte 8)
  "The probabilities of a 4x4 subblock's intra mode, [above mode][left mode][tree node]."
  231 120 48 89 115 113 120 152 112
  152 179 64 126 170 118 46 70 95
  175 69 143 80 85 82 72 155 103
  56 58 10 171 218 189 17 13 152
  114 26 17 163 44 195 21 10 173
  121 24 80 195 26 62 44 64 85
  144 71 10 38 171 213 144 34 26
  170 46 55 19 136 160 33 206 71
  63 20 8 114 114 208 12 9 226
  81 40 11 96 182 84 29 16 36
  134 183 89 137 98 101 106 165 148
  72 187 100 130 157 111 32 75 80
  66 102 167 99 74 62 40 234 128
  41 53 9 178 241 141 26 8 107
  74 43 26 146 73 166 49 23 157
  65 38 105 160 51 52 31 115 128
  104 79 12 27 217 255 87 17 7
  87 68 71 44 114 51 15 186 23
  47 41 14 110 182 183 21 17 194
  66 45 25 102 197 189 23 18 22
  88 88 147 150 42 46 45 196 205
  43 97 183 117 85 38 35 179 61
  39 53 200 87 26 21 43 232 171
  56 34 51 104 114 102 29 93 77
  39 28 85 171 58 165 90 98 64
  34 22 116 206 23 34 43 166 73
  107 54 32 26 51 1 81 43 31
  68 25 106 22 64 171 36 225 114
  34 19 21 102 132 188 16 76 124
  62 18 78 95 85 57 50 48 51
  193 101 35 159 215 111 89 46 111
  60 148 31 172 219 228 21 18 111
  112 113 77 85 179 255 38 120 114
  40 42 1 196 245 209 10 25 109
  88 43 29 140 166 213 37 43 154
  61 63 30 155 67 45 68 1 209
  100 80 8 43 154 1 51 26 71
  142 78 78 16 255 128 34 197 171
  41 40 5 102 211 183 4 1 221
  51 50 17 168 209 192 23 25 82
  138 31 36 171 27 166 38 44 229
  67 87 58 169 82 115 26 59 179
  63 59 90 180 59 166 93 73 154
  40 40 21 116 143 209 34 39 175
  47 15 16 183 34 223 49 45 183
  46 17 33 183 6 98 15 32 183
  57 46 22 24 128 1 54 17 37
  65 32 73 115 28 128 23 128 205
  40 3 9 115 51 192 18 6 223
  87 37 9 115 59 77 64 21 47
  104 55 44 218 9 54 53 130 226
  64 90 70 205 40 41 23 26 57
  54 57 112 184 5 41 38 166 213
  30 34 26 133 152 116 10 32 134
  39 19 53 221 26 114 32 73 255
  31 9 65 234 2 15 1 118 73
  75 32 12 51 192 255 160 43 51
  88 31 35 67 102 85 55 186 85
  56 21 23 111 59 205 45 37 192
  55 38 70 124 73 102 1 34 98
  125 98 42 88 104 85 117 175 82
  95 84 53 89 128 100 113 101 45
  75 79 123 47 51 128 81 171 1
  57 17 5 71 102 57 53 41 49
  38 33 13 121 57 73 26 1 85
  41 10 67 138 77 110 90 47 114
  115 21 2 10 102 255 166 23 6
  101 29 16 10 85 128 101 196 26
  57 18 10 102 102 213 34 20 43
  117 20 15 36 163 128 68 1 26
  102 61 71 37 34 53 31 243 192
  69 60 71 38 73 119 28 222 37
  68 45 128 34 1 47 11 245 171
  62 17 19 70 146 85 55 62 70
  37 43 37 154 100 163 85 160 1
  63 9 92 136 28 64 32 201 85
  75 15 9 9 64 255 184 119 16
  86 6 28 5 64 255 25 248 1
  56 8 17 132 137 255 55 116 128
  58 15 20 82 135 57 26 121 40
  164 50 31 137 154 133 25 35 218
  51 103 44 131 131 123 31 6 158
  86 40 64 135 148 224 45 183 128
  22 26 17 131 240 154 14 1 209
  45 16 21 91 64 222 7 1 197
  56 21 39 155 60 138 23 102 213
  83 12 13 54 192 255 68 47 28
  85 26 85 85 128 128 32 146 171
  18 11 7 63 144 171 4 4 246
  35 27 10 146 174 171 12 26 128
  190 80 35 99 180 80 126 54 45
  85 126 47 87 176 51 41 20 32
  101 75 128 139 118 146 116 128 85
  56 41 15 176 236 85 37 9 62
  71 30 17 119 118 255 17 18 138
  101 38 60 138 55 70 43 26 142
  146 36 19 30 171 255 97 27 20
  138 45 61 62 219 1 81 188 64
  32 41 20 117 151 142 20 21 163
  112 19 12 61 195 128 48 4 24)

(%webp-table *vp8l-distance-map* (unsigned-byte 8)
  "The 120 short distance codes, each a (dy << 4) | (8 - dx) pair."
  24 7 23 25 40 6 39 41 22 26
  38 42 56 5 55 57 21 27 54 58
  37 43 72 4 71 73 20 28 53 59
  70 74 36 44 88 69 75 52 60 3
  87 89 19 29 86 90 35 45 68 76
  85 91 51 61 104 2 103 105 18 30
  102 106 34 46 84 92 67 77 101 107
  50 62 120 1 119 121 83 93 17 31
  100 108 66 78 118 122 33 47 117 123
  49 63 99 109 82 94 0 116 124 65
  79 16 32 98 110 48 115 125 81 95
  64 114 126 97 111 80 113 127 96 112)

(declaim (type (simple-array (unsigned-byte 8) (256)) *webp-reversed-bytes*))
(sb-ext:defglobal *webp-reversed-bytes*
    (let ((table (%webp-octets 256)))
      (dotimes (byte 256 table)
        (setf (aref table byte)
              (loop for bit below 8 sum (if (logbitp bit byte) (ash 1 (- 7 bit)) 0)))))
  "Each byte with its bits in the opposite order.")

;;; --- VP8: the boolean entropy decoder ------------------------------------------
;;; A partition is read one bool at a time, each at a probability out of 256
;;; that it is 0. VALUE holds the next 16 bits of the arithmetic code, RANGE
;;; the width of the current interval less one.

(defstruct (vp8-bits (:constructor %make-vp8-bits (buffer position end))
                     (:copier nil) (:predicate nil))
  (buffer (%webp-octets 0) :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)
  (end 0 :type fixnum)
  (range 254 :type (unsigned-byte 8))
  (value 0 :type (unsigned-byte 32))
  (count 0 :type fixnum)
  (eof nil :type boolean))

(declaim (inline vp8-read-bit))
(defun vp8-read-bit (bits probability)
  "The next bool BITS decodes at PROBABILITY (of 256) that it is 0: T or NIL.
Past the partition's end it answers NIL and marks the partition cut short."
  (declare (type vp8-bits bits) (type (unsigned-byte 8) probability)
           (optimize speed (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (< (vp8-bits-count bits) 8)
    (let ((position (vp8-bits-position bits)))
      (when (>= position (vp8-bits-end bits))
        (setf (vp8-bits-eof bits) t)
        (return-from vp8-read-bit nil))
      (setf (vp8-bits-value bits)
            (logior (vp8-bits-value bits)
                    (the (unsigned-byte 16)
                         (ash (aref (vp8-bits-buffer bits) position) (- 8 (vp8-bits-count bits)))))
            (vp8-bits-position bits) (1+ position)
            (vp8-bits-count bits) (+ (vp8-bits-count bits) 8))))
  (let* ((range (vp8-bits-range bits))
         (split (1+ (ash (* range probability) -8)))
         (value (vp8-bits-value bits))
         (bit (>= value (ash split 8))))
    (declare (type (unsigned-byte 8) range) (type (integer 1 256) split)
             (type (unsigned-byte 32) value))
    (if bit
        (setf range (- range split) value (- value (ash split 8)))
        (setf range (1- split)))
    (when (< range 127)
      (let ((shift (- 8 (integer-length (1+ range)))))
        (declare (type (integer 1 7) shift))
        (setf range (1- (ash (1+ range) shift))
              value (ash value shift))
        (decf (vp8-bits-count bits) shift)))
    (setf (vp8-bits-range bits) range
          (vp8-bits-value bits) value)
    bit))

(defun vp8-read-literal (bits count &optional (probability 128))
  "The COUNT-bit unsigned number BITS decodes next, high bit first."
  (declare (type vp8-bits bits) (type (integer 0 16) count)
           (type (unsigned-byte 8) probability) (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((value 0))
    (declare (type (unsigned-byte 16) value))
    (dotimes (i count value)
      (setf value (logior (ash value 1) (if (vp8-read-bit bits probability) 1 0))))))

(defun vp8-read-optional-signed (bits count)
  "A COUNT-bit magnitude and its sign, read when a flag says one follows; else 0."
  (declare (type vp8-bits bits))
  (if (vp8-read-bit bits 128)
      (let ((magnitude (vp8-read-literal bits count)))
        (if (vp8-read-bit bits 128) (- magnitude) magnitude))
      0))

;;; --- VP8: a keyframe -----------------------------------------------------------------

(declaim (inline %clip8 %wrap16))
(defun %clip8 (value)
  (declare (type fixnum value))
  (cond ((< value 0) 0) ((> value 255) 255) (t value)))

(defun %wrap16 (value)
  ;; The int16 a coefficient is kept as, wrapping like libwebp's.
  (declare (type fixnum value))
  (- (logand (+ value 32768) 65535) 32768))

(defun vp8-decode (data start end)
  "Decode the VP8 keyframe in DATA from START below END. => (values Y U V
WIDTH HEIGHT Y-STRIDE UV-STRIDE): the luma plane at full size and the two
chroma planes at half, each padded out to whole 16x16 macroblocks and
already through the loop filter."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start end)
           (optimize speed (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (< (- end start) 10)
    (%webp-fail "a VP8 frame shorter than its 10-byte header"))
  (let ((tag (%webp-le data start)))
    (when (logbitp 0 tag)
      (%webp-fail "a VP8 frame that is no keyframe"))
    (unless (and (= (aref data (+ start 3)) #x9d) (= (aref data (+ start 4)) #x01)
                 (= (aref data (+ start 5)) #x2a))
      (%webp-fail "a VP8 keyframe without its start code"))
    (let* ((width (logand (%webp-le data (+ start 6) 2) #x3fff))
           (height (logand (%webp-le data (+ start 8) 2) #x3fff))
           (first-start (+ start 10))
           (first-end (+ first-start (ash tag -5))))
      (declare (type fixnum width height first-start first-end))
      (when (or (zerop width) (zerop height))
        (%webp-fail "a VP8 frame of no pixels"))
      (when (> first-end end)
        (%webp-fail "a VP8 first partition that runs past its frame"))
      (%webp-check-size width height)
      (let* ((mbw (ash (+ width 15) -4))
             (mbh (ash (+ height 15) -4))
             (y-stride (* 16 mbw))
             (uv-stride (* 8 mbw))
             (yp (%webp-octets (* y-stride 16 mbh)))
             (up (%webp-octets (* uv-stride 8 mbh)))
             (vp (%webp-octets (* uv-stride 8 mbh)))
             (fp (%make-vp8-bits data first-start first-end))
             ;; The segment header.
             (use-segment nil) (update-map nil) (relative-delta nil)
             (segment-quantizer (make-array 4 :element-type 'fixnum :initial-element 0))
             (segment-filter (make-array 4 :element-type 'fixnum :initial-element 0))
             (segment-probability (make-array 3 :element-type '(unsigned-byte 8) :initial-element 255))
             ;; The filter header.
             (filter-simple nil) (filter-level 0) (sharpness 0) (use-lf-delta nil)
             (ref-delta 0) (mode-delta 0)
             ;; Dequantization, six factors a segment: y1 dc ac, y2 dc ac, uv dc ac.
             (quant (make-array 24 :element-type 'fixnum :initial-element 0))
             (probabilities (copy-seq *vp8-default-coefficient-probabilities*))
             (use-skip nil) (skip-probability 0)
             (partitions nil) (partition-mask 0)
             ;; Per macroblock: the work area, a 26 x 32 window of the frame
             ;; holding the block being built and the edge pixels it predicts
             ;; from (luma at rows 1-16, columns 8-23; blue chroma at rows
             ;; 18-25, columns 8-15; red chroma at rows 18-25, columns 24-31).
             (ybr (%webp-octets (* 26 32)))
             (coeff (make-array 400 :element-type '(signed-byte 16) :initial-element 0))
             (scratch (make-array 16 :element-type 'fixnum :initial-element 0))
             (up-pred (%webp-octets (* 4 mbw)))
             (up-nz (%webp-octets mbw))
             (up-nz-y16 (%webp-octets mbw))
             (left-pred (%webp-octets 4))
             (left-nz 0) (left-nz-y16 0)
             (pred-y4 (%webp-octets 16))
             (use-pred-y16 nil) (pred-y16 0) (pred-c8 0)
             (nz-dc-mask 0) (nz-ac-mask 0)
             (segment 0)
             ;; Loop filter strengths: per segment and 16x16-or-4x4, the edge
             ;; limit, the interior limit and the high edge variance threshold.
             (filter-params (make-array 24 :element-type 'fixnum :initial-element 0))
             (mb-filter (%webp-octets (* 4 mbw mbh))))
        (declare (type fixnum mbw mbh y-stride uv-stride filter-level sharpness ref-delta mode-delta
                       skip-probability partition-mask left-nz left-nz-y16 pred-y16 pred-c8 segment)
                 (type (unsigned-byte 32) nz-dc-mask nz-ac-mask)
                 (type (simple-array (unsigned-byte 8) (*)) yp up vp ybr up-pred up-nz up-nz-y16
                       left-pred pred-y4 mb-filter segment-probability)
                 (type (simple-array (unsigned-byte 8) (1056)) probabilities)
                 (type (simple-array fixnum (*)) segment-quantizer segment-filter quant
                       filter-params scratch)
                 (type (simple-array (signed-byte 16) (400)) coeff))
        (macrolet ((at (row column) `(aref ybr (+ (* 32 ,row) ,column)))
                   (flag (probability) `(vp8-read-bit fp ,probability))
                   (over (y x size value)
                     ;; Each pixel of the SIZE square at Y X set to VALUE, I and J its column and row in it.
                     `(dotimes (j ,size) (dotimes (i ,size) (setf (at (+ ,y j) (+ ,x i)) ,value)))))
          (labels
              ;; --- prediction ---
              ((fill-block (y x size value)
                 (declare (type fixnum y x size value))
                 (over y x size value))
               (average-top (y x size)
                 (declare (type fixnum y x size))
                 (let ((sum 0)) (declare (type fixnum sum))
                   (dotimes (i size sum) (incf sum (at (1- y) (+ x i))))))
               (average-left (y x size)
                 (declare (type fixnum y x size))
                 (let ((sum 0)) (declare (type fixnum sum))
                   (dotimes (j size sum) (incf sum (at (+ y j) (1- x))))))
               (predict-tm (y x size)
                 (declare (type fixnum y x size))
                 (let ((corner (at (1- y) (1- x))))
                   (over y x size (%clip8 (- (+ (at (+ y j) (1- x)) (at (1- y) (+ x i))) corner)))))
               (predict-large (mode y x size)
                 ;; The 16x16 luma and 8x8 chroma predictors; modes past HU
                 ;; are DC with an edge missing: 10 the top, 11 the left, 12 both.
                 (declare (type fixnum mode y x size))
                 (let ((shift (if (= size 16) 5 4)))
                   (case mode
                     (0 (fill-block y x size (ash (+ (average-top y x size) (average-left y x size) size) (- shift))))
                     (1 (predict-tm y x size))
                     (2 (over y x size (at (1- y) (+ x i))))
                     (3 (over y x size (at (+ y j) (1- x))))
                     (10 (fill-block y x size (ash (+ (average-left y x size) (ash size -1)) (- 1 shift))))
                     (11 (fill-block y x size (ash (+ (average-top y x size) (ash size -1)) (- 1 shift))))
                     (t (fill-block y x size #x80)))))
               (predict4 (mode y x)
                 (declare (type fixnum mode y x))
                 (case mode
                   (0 (fill-block y x 4 (ash (+ (average-top y x 4) (average-left y x 4) 4) -3)))
                   (1 (predict-tm y x 4))
                   (t
                    (let* ((a (at (1- y) (1- x)))
                           (b (at (1- y) x)) (c (at (1- y) (+ x 1))) (d (at (1- y) (+ x 2)))
                           (e (at (1- y) (+ x 3))) (f (at (1- y) (+ x 4))) (g (at (1- y) (+ x 5)))
                           (h (at (1- y) (+ x 6))) (h2 (at (1- y) (+ x 7)))
                           (p (at y (1- x))) (q (at (+ y 1) (1- x)))
                           (r (at (+ y 2) (1- x))) (s (at (+ y 3) (1- x))))
                      (declare (type fixnum a b c d e f g h h2 p q r s))
                      (macrolet ((avg3 (u v w) `(ash (+ ,u (* 2 ,v) ,w 2) -2))
                                 (avg2 (u v) `(ash (+ ,u ,v 1) -1))
                                 (rows (&rest values)
                                   `(progn ,@(loop for value in values for k from 0
                                                   collect `(setf (at (+ y ,(floor k 4)) (+ x ,(mod k 4))) ,value)))))
                        (case mode
                          (2 (let ((abc (avg3 a b c)) (bcd (avg3 b c d)) (cde (avg3 c d e)) (def (avg3 d e f)))
                               (rows abc bcd cde def abc bcd cde def abc bcd cde def abc bcd cde def)))
                          (3 (let ((ssr (avg3 s s r)) (srq (avg3 s r q)) (rqp (avg3 r q p)) (apq (avg3 a p q)))
                               (rows apq apq apq apq rqp rqp rqp rqp srq srq srq srq ssr ssr ssr ssr)))
                          (4 (let ((srq (avg3 s r q)) (rqp (avg3 r q p)) (qpa (avg3 q p a)) (pab (avg3 p a b))
                                   (abc (avg3 a b c)) (bcd (avg3 b c d)) (cde (avg3 c d e)))
                               (rows pab abc bcd cde qpa pab abc bcd rqp qpa pab abc srq rqp qpa pab)))
                          (5 (let ((ab (avg2 a b)) (bc (avg2 b c)) (cd (avg2 c d)) (de (avg2 d e))
                                   (rqp (avg3 r q p)) (qpa (avg3 q p a)) (pab (avg3 p a b))
                                   (abc (avg3 a b c)) (bcd (avg3 b c d)) (cde (avg3 c d e)))
                               (rows ab bc cd de pab abc bcd cde qpa ab bc cd rqp pab abc bcd)))
                          (6 (let ((abc (avg3 b c d)) (bcd (avg3 c d e)) (cde (avg3 d e f)) (def (avg3 e f g))
                                   (efg (avg3 f g h)) (fgh (avg3 g h h2)) (ghh (avg3 h h2 h2)))
                               (rows abc bcd cde def bcd cde def efg cde def efg fgh def efg fgh ghh)))
                          (7 (let ((ab (avg2 b c)) (bc (avg2 c d)) (cd (avg2 d e)) (de (avg2 e f))
                                   (abc (avg3 b c d)) (bcd (avg3 c d e)) (cde (avg3 d e f)) (def (avg3 e f g))
                                   (efg (avg3 f g h)) (fgh (avg3 g h h2)))
                               (rows ab bc cd de abc bcd cde def bc cd de efg bcd cde def fgh)))
                          (8 (let ((sr (avg2 s r)) (rq (avg2 r q)) (qp (avg2 q p)) (pa (avg2 p a))
                                   (srq (avg3 s r q)) (rqp (avg3 r q p)) (qpa (avg3 q p a)) (pab (avg3 p a b))
                                   (abc (avg3 a b c)) (bcd (avg3 b c d)))
                               (rows pa pab abc bcd qp qpa pa pab rq rqp qp qpa sr srq rq rqp)))
                          (t (let ((pq (avg2 p q)) (qr (avg2 q r)) (rs (avg2 r s))
                                   (pqr (avg3 p q r)) (qrs (avg3 q r s)) (rss (avg3 r s s)))
                               (rows pq pqr qr qrs qr qrs rs rss rs rss s s s s s s)))))))))
               (top-left-mode (mbx mby mode)
                 (declare (type fixnum mbx mby mode))
                 (cond ((/= mode 0) mode)
                       ((zerop mbx) (if (zerop mby) 12 11))
                       ((zerop mby) 10)
                       (t 0)))
               ;; --- the inverse transforms ---
               (mul1 (v) (declare (type fixnum v)) (+ v (ash (* v 20091) -16)))
               (mul2 (v) (declare (type fixnum v)) (ash (* v 35468) -16))
               (inverse-dct4 (y x base)
                 (declare (type fixnum y x base))
                 (flet ((butterfly (c0 c4 c8 c12)
                          ;; The 1-D inverse transform of four taps: => its four outputs.
                          (declare (type fixnum c0 c4 c8 c12))
                          (let ((a (+ c0 c8)) (b (- c0 c8)) (c (- (mul2 c4) (mul1 c12))) (d (+ (mul1 c4) (mul2 c12))))
                            (values (+ a d) (+ b c) (- b c) (- a d)))))
                   (declare (inline butterfly))
                   (dotimes (i 4)
                     (let ((k (* 4 i)))
                       (setf (values (aref scratch k) (aref scratch (+ k 1)) (aref scratch (+ k 2)) (aref scratch (+ k 3)))
                             (butterfly (aref coeff (+ base i)) (aref coeff (+ base 4 i))
                                        (aref coeff (+ base 8 i)) (aref coeff (+ base 12 i))))))
                   (dotimes (j 4)
                     (multiple-value-bind (v0 v1 v2 v3)
                         (butterfly (+ (aref scratch j) 4) (aref scratch (+ 4 j)) (aref scratch (+ 8 j)) (aref scratch (+ 12 j)))
                       (macrolet ((add (i v) `(setf (at (+ y j) (+ x ,i)) (%clip8 (+ (at (+ y j) (+ x ,i)) (ash ,v -3))))))
                         (add 0 v0) (add 1 v1) (add 2 v2) (add 3 v3))))))
               (inverse-dct4-dc (y x base)
                 (declare (type fixnum y x base))
                 (let ((dc (ash (+ (aref coeff base) 4) -3)))
                   (over y x 4 (%clip8 (+ (at (+ y j) (+ x i)) dc)))))
               (inverse-dct8 (y x base dc-only)
                 (declare (type fixnum y x base))
                 (loop for (dy dx) in '((0 0) (0 4) (4 0) (4 4))
                       for b of-type fixnum from base by 16
                       do (if dc-only
                              (inverse-dct4-dc (+ y dy) (+ x dx) b)
                              (inverse-dct4 (+ y dy) (+ x dx) b))))
               (inverse-wht16 ()
                 (dotimes (i 4)
                   (let ((a0 (+ (aref coeff (+ 384 i)) (aref coeff (+ 396 i))))
                         (a1 (+ (aref coeff (+ 388 i)) (aref coeff (+ 392 i))))
                         (a2 (- (aref coeff (+ 388 i)) (aref coeff (+ 392 i))))
                         (a3 (- (aref coeff (+ 384 i)) (aref coeff (+ 396 i)))))
                     (setf (aref scratch i) (+ a0 a1) (aref scratch (+ 8 i)) (- a0 a1)
                           (aref scratch (+ 4 i)) (+ a3 a2) (aref scratch (+ 12 i)) (- a3 a2))))
                 (dotimes (i 4)
                   (let* ((k (* 4 i))
                          (dc (+ (aref scratch k) 3))
                          (a0 (+ dc (aref scratch (+ k 3))))
                          (a1 (+ (aref scratch (+ k 1)) (aref scratch (+ k 2))))
                          (a2 (- (aref scratch (+ k 1)) (aref scratch (+ k 2))))
                          (a3 (- dc (aref scratch (+ k 3))))
                          (out (* 64 i)))
                     (setf (aref coeff out) (%wrap16 (ash (+ a0 a1) -3))
                           (aref coeff (+ out 16)) (%wrap16 (ash (+ a3 a2) -3))
                           (aref coeff (+ out 32)) (%wrap16 (ash (- a0 a1) -3))
                           (aref coeff (+ out 48)) (%wrap16 (ash (- a3 a2) -3))))))
               ;; --- the macroblock's modes and residuals ---
               (parse-pred-y16 (mbx)
                 (declare (type fixnum mbx))
                 (let ((mode (if (not (flag 156))
                                 (if (not (flag 163)) 0 2)
                                 (if (not (flag 128)) 3 1))))
                   (dotimes (i 4)
                     (setf (aref up-pred (+ (* 4 mbx) i)) mode (aref left-pred i) mode))
                   (setf pred-y16 mode)))
               (parse-pred-y4 (mbx)
                 (declare (type fixnum mbx))
                 (dotimes (j 4)
                   (let ((mode (aref left-pred j)))
                     (dotimes (i 4)
                       (let ((p (* 9 (+ (* 10 (aref up-pred (+ (* 4 mbx) i))) mode))))
                         (flet ((b (k) (flag (aref *vp8-bmode-probabilities* (+ p k)))))
                           (setf mode (cond ((not (b 0)) 0)
                                            ((not (b 1)) 1)
                                            ((not (b 2)) 2)
                                            ((not (b 3)) (cond ((not (b 4)) 3) ((not (b 5)) 4) (t 5)))
                                            ((not (b 6)) 6)
                                            ((not (b 7)) 7)
                                            ((not (b 8)) 8)
                                            (t 9))))
                         (setf (aref pred-y4 (+ (* 4 j) i)) mode
                               (aref up-pred (+ (* 4 mbx) i)) mode)))
                     (setf (aref left-pred j) mode))))
               (parse-pred-c8 ()
                 (setf pred-c8 (cond ((not (flag 142)) 0)
                                     ((not (flag 114)) 2)
                                     ((not (flag 183)) 3)
                                     (t 1))))
               (probability-index (plane band context)
                 (declare (type fixnum plane band context))
                 (* 11 (+ (* 3 (+ (* 8 plane) band)) context)))
               (parse-block (r plane context dc-factor ac-factor skip-first base)
                 ;; One 4x4 block's coefficients, dequantized into COEFF at
                 ;; BASE: 1 when any token comes before its end of block, else 0.
                 (declare (type vp8-bits r) (type fixnum plane context dc-factor ac-factor base))
                 (let* ((n (if skip-first 1 0))
                        (p (probability-index plane (aref *vp8-bands* n) context)))
                   (declare (type fixnum n p))
                   (if (not (vp8-read-bit r (aref probabilities p)))
                       0
                       (loop
                         (when (= n 16) (return 1))
                         (incf n)
                         (if (not (vp8-read-bit r (aref probabilities (+ p 1))))
                             (setf p (probability-index plane (aref *vp8-bands* n) 0))
                             (let ((v 0))
                               (declare (type fixnum v))
                               (if (not (vp8-read-bit r (aref probabilities (+ p 2))))
                                   (setf v 1
                                         p (probability-index plane (aref *vp8-bands* n) 1))
                                   (progn
                                     (setf v (cond
                                               ((not (vp8-read-bit r (aref probabilities (+ p 3))))
                                                (if (not (vp8-read-bit r (aref probabilities (+ p 4))))
                                                    2
                                                    (+ 3 (vp8-read-literal r 1 (aref probabilities (+ p 5))))))
                                               ((not (vp8-read-bit r (aref probabilities (+ p 6))))
                                                (if (not (vp8-read-bit r (aref probabilities (+ p 7))))
                                                    (+ 5 (vp8-read-literal r 1 159))
                                                    (+ 7 (* 2 (vp8-read-literal r 1 165))
                                                       (vp8-read-literal r 1 145))))
                                               (t
                                                (let* ((b1 (vp8-read-literal r 1 (aref probabilities (+ p 8))))
                                                       (b0 (vp8-read-literal r 1 (aref probabilities (+ p 9 b1))))
                                                       (category (+ (* 2 b1) b0))
                                                       (extra 0))
                                                  (declare (type fixnum extra))
                                                  (loop for k of-type fixnum from (* 12 category)
                                                        for probability = (aref *vp8-cat3456* k)
                                                        until (zerop probability)
                                                        do (setf extra (+ (* 2 extra)
                                                                          (vp8-read-literal r 1 probability))))
                                                  (+ extra 3 (ash 8 category))))))
                                     (setf p (probability-index plane (aref *vp8-bands* n) 2))))
                               (let* ((z (aref *vp8-zigzag* (1- n)))
                                      (c (* v (if (> z 0) ac-factor dc-factor))))
                                 (declare (type fixnum c))
                                 (when (vp8-read-bit r 128) (setf c (- c)))
                                 (setf (aref coeff (+ base z)) (%wrap16 c))
                                 (when (or (= n 16) (not (vp8-read-bit r (aref probabilities p))))
                                   (return 1)))))))))
               (parse-residuals (mbx r)
                 ;; Every coefficient of the macroblock: T when there were none.
                 (declare (type fixnum mbx) (type vp8-bits r))
                 (let ((q (* 6 segment)) (plane 3) (nz-dc 0) (nz-ac 0) (base 0) (bit 0))
                   (declare (type fixnum q plane base bit) (type (unsigned-byte 32) nz-dc nz-ac))
                   (when use-pred-y16
                     (let ((nz (parse-block r 1 (+ left-nz-y16 (aref up-nz-y16 mbx))
                                            (aref quant (+ q 2)) (aref quant (+ q 3)) nil 384)))
                       (setf left-nz-y16 nz (aref up-nz-y16 mbx) nz)
                       (inverse-wht16)
                       (setf plane 0)))
                   (flet ((blocks (size at plane dc ac skip-first)
                            ;; SIZE x SIZE blocks, whose nonzero flags beside and above
                            ;; them are LEFT-NZ's and UP-NZ's SIZE bits from AT.
                            (declare (type (integer 2 4) size) (type (integer 0 6) at) (type fixnum plane dc ac))
                            (let ((left (ldb (byte size at) left-nz)) (up (ldb (byte size at) (aref up-nz mbx))))
                              (declare (type (unsigned-byte 4) left up))
                              (dotimes (y size)
                                (let ((nz (ldb (byte 1 y) left)))
                                  (dotimes (x size)
                                    (setf nz (parse-block r plane (+ nz (ldb (byte 1 x) up)) dc ac skip-first base)
                                          up (dpb nz (byte 1 x) up))
                                    (when (= nz 1) (setf nz-ac (logior nz-ac (ash 1 bit))))
                                    (when (/= 0 (aref coeff base)) (setf nz-dc (logior nz-dc (ash 1 bit))))
                                    (incf base 16) (incf bit))
                                  (setf left (dpb nz (byte 1 y) left))))
                              (setf left-nz (dpb left (byte size at) left-nz)
                                    (aref up-nz mbx) (dpb up (byte size at) (aref up-nz mbx))))))
                     (blocks 4 0 plane (aref quant q) (aref quant (+ q 1)) use-pred-y16)
                     (blocks 2 4 2 (aref quant (+ q 4)) (aref quant (+ q 5)) nil)
                     (blocks 2 6 2 (aref quant (+ q 4)) (aref quant (+ q 5)) nil))
                   (setf nz-dc-mask nz-dc nz-ac-mask nz-ac)
                   (and (zerop nz-dc) (zerop nz-ac))))
               ;; --- building the macroblock ---
               (prepare (mbx mby)
                 (declare (type fixnum mbx mby))
                 (if (zerop mbx)
                     (progn (loop for y from 0 below 17 do (setf (at y 7) #x81))
                            (loop for y from 17 below 26 do (setf (at y 7) #x81 (at y 23) #x81)))
                     (progn (loop for y from 0 below 17 do (setf (at y 7) (at y 23)))
                            (loop for y from 17 below 26 do (setf (at y 7) (at y 15) (at y 23) (at y 31)))))
                 (if (zerop mby)
                     (progn (loop for x from 7 below 28 do (setf (at 0 x) #x7f))
                            (loop for x from 7 below 16 do (setf (at 17 x) #x7f))
                            (loop for x from 23 below 32 do (setf (at 17 x) #x7f)))
                     (let ((y-row (+ (* (1- (* 16 mby)) y-stride) (* 16 mbx)))
                           (c-row (+ (* (1- (* 8 mby)) uv-stride) (* 8 mbx))))
                       (dotimes (i 16) (setf (at 0 (+ 8 i)) (aref yp (+ y-row i))))
                       (dotimes (i 8)
                         (setf (at 17 (+ 8 i)) (aref up (+ c-row i))
                               (at 17 (+ 24 i)) (aref vp (+ c-row i))))
                       (loop for i from 16 below 20
                             do (setf (at 0 (+ 8 i))
                                      (aref yp (+ y-row (if (= mbx (1- mbw)) 15 i)))))))
                 (loop for y from 4 below 16 by 4
                       do (loop for x from 24 below 28 do (setf (at y x) (at 0 x)))))
               (build (mbx mby)
                 (declare (type fixnum mbx mby))
                 (when use-pred-y16
                   (predict-large (top-left-mode mbx mby pred-y16) 1 8 16))
                 (dotimes (j 4)
                   (dotimes (i 4)
                     (let ((n (+ (* 4 j) i)) (y (+ (* 4 j) 1)) (x (+ (* 4 i) 8)))
                       (unless use-pred-y16 (predict4 (aref pred-y4 n) y x))
                       (cond ((logbitp n nz-ac-mask) (inverse-dct4 y x (* 16 n)))
                             ((logbitp n nz-dc-mask) (inverse-dct4-dc y x (* 16 n)))))))
                 (loop with mode = (top-left-mode mbx mby pred-c8)
                       for (x base blocks) in '((8 256 #x0f0000) (24 320 #xf00000))
                       do (predict-large mode 18 x 8)
                          (when (logtest (logior nz-ac-mask nz-dc-mask) blocks)
                            (inverse-dct8 18 x base (not (logtest nz-ac-mask blocks)))))
                 (dotimes (y 16)
                   (replace yp ybr :start1 (+ (* (+ (* 16 mby) y) y-stride) (* 16 mbx))
                                   :start2 (+ (* 32 (+ 1 y)) 8) :end2 (+ (* 32 (+ 1 y)) 24)))
                 (dotimes (y 8)
                   (let ((row (+ (* (+ (* 8 mby) y) uv-stride) (* 8 mbx))))
                     (replace up ybr :start1 row :start2 (+ (* 32 (+ 18 y)) 8) :end2 (+ (* 32 (+ 18 y)) 16))
                     (replace vp ybr :start1 row :start2 (+ (* 32 (+ 18 y)) 24) :end2 (+ (* 32 (+ 18 y)) 32)))))
               ;; --- the loop filter ---
               (clamp15 (v) (declare (type fixnum v)) (max -16 (min 15 v)))
               (clamp127 (v) (declare (type fixnum v)) (max -128 (min 127 v)))
               (filter (pix n level ilevel hlevel index istep jstep inner)
                 ;; N points from INDEX by ISTEP, each across the edge JSTEP
                 ;; before it: the simple filter, else the normal filter's
                 ;; 4-tap INNER edge or 6-tap macroblock edge.
                 (declare (type (simple-array (unsigned-byte 8) (*)) pix)
                          (type fixnum n level ilevel hlevel index istep jstep))
                 (loop repeat n
                       for at of-type fixnum from index by istep
                       do (macrolet ((px (k) `(aref pix (+ at (* ,k jstep)))))
                            (let ((p3 (px -4)) (p2 (px -3)) (p1 (px -2)) (p0 (px -1))
                                  (q0 (px 0)) (q1 (px 1)) (q2 (px 2)) (q3 (px 3)))
                              (declare (type fixnum p3 p2 p1 p0 q0 q1 q2 q3))
                              (flet ((adjust (a)
                                       ;; P0 and Q0 moved toward each other by A; => Q0's step.
                                       (let ((a1 (clamp15 (ash (+ a 4) -3))) (a2 (clamp15 (ash (+ a 3) -3))))
                                         (setf (px -1) (%clip8 (+ p0 a2)) (px 0) (%clip8 (- q0 a1)))
                                         a1)))
                                (declare (inline adjust))
                                (unless (or (> (+ (* 2 (abs (- p0 q0))) (ash (abs (- p1 q1)) -1)) level)
                                            (and (not filter-simple)
                                                 (or (> (abs (- p3 p2)) ilevel) (> (abs (- p2 p1)) ilevel)
                                                     (> (abs (- p1 p0)) ilevel) (> (abs (- q1 q0)) ilevel)
                                                     (> (abs (- q2 q1)) ilevel) (> (abs (- q3 q2)) ilevel))))
                                  (cond
                                    ((or filter-simple (> (abs (- p1 p0)) hlevel) (> (abs (- q1 q0)) hlevel))
                                     (adjust (+ (* 3 (- q0 p0)) (clamp127 (- p1 q1)))))
                                    (inner
                                     (let ((a3 (ash (+ (adjust (* 3 (- q0 p0))) 1) -1)))
                                       (setf (px -2) (%clip8 (+ p1 a3)) (px 1) (%clip8 (- q1 a3)))))
                                    (t
                                     ;; The taps 1, 2 and 3 from the edge move by 27, 18 and 9 128ths of A.
                                     (let* ((a (clamp127 (+ (* 3 (- q0 p0)) (clamp127 (- p1 q1)))))
                                            (d1 (ash (+ (* 27 a) 63) -7)) (d2 (ash (+ (* 18 a) 63) -7)) (d3 (ash (+ (* 9 a) 63) -7)))
                                       (setf (px -3) (%clip8 (+ p2 d3)) (px -2) (%clip8 (+ p1 d2)) (px -1) (%clip8 (+ p0 d1))
                                             (px 0) (%clip8 (- q0 d1)) (px 1) (%clip8 (- q1 d2)) (px 2) (%clip8 (- q2 d3))))))))))))
               (loop-filter ()
                 (dotimes (mby mbh)
                   (dotimes (mbx mbw)
                     (let* ((m (* 4 (+ (* mbw mby) mbx)))
                            (level (aref mb-filter m))
                            (ilevel (aref mb-filter (+ m 1)))
                            (hlevel (aref mb-filter (+ m 2)))
                            (inner (= 1 (aref mb-filter (+ m 3))))
                            (yi (* 16 (+ (* mby y-stride) mbx)))
                            (ci (* 8 (+ (* mby uv-stride) mbx))))
                       (declare (type fixnum level ilevel hlevel yi ci))
                       (unless (zerop level)
                         ;; The left edge and the inner columns, then the top edge and the inner rows;
                         ;; chroma, under the normal filter only, has one inner edge at 4.
                         (dolist (across-columns '(t nil))
                           (let ((ys (if across-columns y-stride 1)) (yj (if across-columns 1 y-stride))
                                 (cs (if across-columns uv-stride 1)) (cj (if across-columns 1 uv-stride)))
                             (declare (type fixnum ys yj cs cj))
                             (flet ((edge (level k inner)
                                      (declare (type fixnum level k))
                                      (filter yp 16 level ilevel hlevel (+ yi (* k yj)) ys yj inner)
                                      (unless (or filter-simple (> k 4))
                                        (filter up 8 level ilevel hlevel (+ ci (* k cj)) cs cj inner)
                                        (filter vp 8 level ilevel hlevel (+ ci (* k cj)) cs cj inner))))
                               (when (> (if across-columns mbx mby) 0) (edge (+ level 4) 0 nil))
                               (when inner (loop for k from 4 below 16 by 4 do (edge level k t))))))))))))
            ;; --- the frame header ---
            (flag 128)                   ; color space
            (flag 128)                   ; clamping type
            (setf use-segment (flag 128))
            (when use-segment
              (setf update-map (flag 128))
              (when (flag 128)
                (setf relative-delta (not (flag 128)))
                (dotimes (i 4) (setf (aref segment-quantizer i) (vp8-read-optional-signed fp 7)))
                (dotimes (i 4) (setf (aref segment-filter i) (vp8-read-optional-signed fp 6))))
              (when update-map
                (dotimes (i 3)
                  (setf (aref segment-probability i) (if (flag 128) (vp8-read-literal fp 8) 255)))))
            (setf filter-simple (flag 128)
                  filter-level (vp8-read-literal fp 6)
                  sharpness (vp8-read-literal fp 3)
                  use-lf-delta (flag 128))
            (when (and use-lf-delta (flag 128))
              ;; Of the deltas, a keyframe's filter takes the first of each:
              ;; the intra frame's, and the 4x4 prediction mode's.
              (dotimes (i 4) (let ((v (vp8-read-optional-signed fp 6))) (when (zerop i) (setf ref-delta v))))
              (dotimes (i 4) (let ((v (vp8-read-optional-signed fp 6))) (when (zerop i) (setf mode-delta v)))))
            (let* ((count (ash 1 (vp8-read-literal fp 2)))
                   (sizes first-end)
                   (at (+ sizes (* 3 (1- count)))))
              (declare (type fixnum count sizes at))
              (when (> at end) (%webp-fail "VP8 partition sizes that run past the frame"))
              (setf partitions (make-array count) partition-mask (1- count))
              (dotimes (i (1- count))
                (let ((size (%webp-le data (+ sizes (* 3 i)))))
                  (when (> (+ at size) end) (%webp-fail "a VP8 partition that runs past the frame"))
                  (setf (svref partitions i) (%make-vp8-bits data at (+ at size)))
                  (incf at size)))
              (setf (svref partitions (1- count)) (%make-vp8-bits data at end)))
            (let ((base (vp8-read-literal fp 7))
                  (y1-dc (vp8-read-optional-signed fp 4))
                  (y2-dc (vp8-read-optional-signed fp 4))
                  (y2-ac (vp8-read-optional-signed fp 4))
                  (uv-dc (vp8-read-optional-signed fp 4))
                  (uv-ac (vp8-read-optional-signed fp 4)))
              (dotimes (s 4)
                (let ((q (cond ((not use-segment) base)
                               (relative-delta (+ base (aref segment-quantizer s)))
                               (t (aref segment-quantizer s))))
                      (k (* 6 s)))
                  (flet ((dc (delta &optional (top 127)) (aref *vp8-dc-quant* (max 0 (min top (+ q delta)))))
                         (ac (delta) (aref *vp8-ac-quant* (max 0 (min 127 (+ q delta))))))
                    (setf (aref quant k) (dc y1-dc)
                          (aref quant (+ k 1)) (ac 0)
                          (aref quant (+ k 2)) (* 2 (dc y2-dc))
                          (aref quant (+ k 3)) (max 8 (floor (* 155 (ac y2-ac)) 100))
                          (aref quant (+ k 4)) (dc uv-dc 117)
                          (aref quant (+ k 5)) (ac uv-ac))))))
            (flag 128)                   ; refresh entropy probabilities: one frame keeps none
            (dotimes (i 1056)
              (when (flag (aref *vp8-coefficient-update-probabilities* i))
                (setf (aref probabilities i) (vp8-read-literal fp 8))))
            (setf use-skip (flag 128))
            (when use-skip (setf skip-probability (vp8-read-literal fp 8)))
            (when (vp8-bits-eof fp) (%webp-fail "a VP8 frame header cut short"))
            (unless (zerop filter-level)
              (dotimes (s 4)
                (let ((base (if use-segment
                                (+ (aref segment-filter s) (if relative-delta filter-level 0))
                                filter-level)))
                  (dotimes (j 2)
                    (let ((level base) (k (* 3 (+ (* 2 s) j))))
                      (declare (type fixnum level k))
                      (when use-lf-delta
                        (incf level ref-delta)
                        (when (= j 1) (incf level mode-delta)))
                      (setf level (max 0 (min 63 level)))
                      (when (> level 0)
                        (let ((ilevel level))
                          (declare (type fixnum ilevel))
                          (when (> sharpness 0)
                            (setf ilevel (min (ash ilevel (if (> sharpness 4) -2 -1)) (- 9 sharpness))))
                          (setf ilevel (max ilevel 1))
                          (setf (aref filter-params k) (+ (* 2 level) ilevel)
                                (aref filter-params (+ k 1)) ilevel
                                (aref filter-params (+ k 2)) (cond ((>= level 40) 2) ((>= level 15) 1) (t 0))))))))))
            ;; --- the macroblocks ---
            (dotimes (mby mbh)
              (fill left-pred 0)
              (setf left-nz 0 left-nz-y16 0)
              (let ((r (svref partitions (logand mby partition-mask))))
                (dotimes (mbx mbw)
                  (when update-map
                    (setf segment (if (not (flag (aref segment-probability 0)))
                                      (vp8-read-literal fp 1 (aref segment-probability 1))
                                      (+ 2 (vp8-read-literal fp 1 (aref segment-probability 2))))))
                  (let ((skip (and use-skip (flag skip-probability))))
                    (fill coeff 0)
                    (prepare mbx mby)
                    (setf use-pred-y16 (flag 145))
                    (if use-pred-y16 (parse-pred-y16 mbx) (parse-pred-y4 mbx))
                    (parse-pred-c8)
                    (if (not skip)
                        (setf skip (parse-residuals mbx r))
                        (progn
                          (when use-pred-y16 (setf left-nz-y16 0 (aref up-nz-y16 mbx) 0))
                          (setf left-nz 0 (aref up-nz mbx) 0 nz-dc-mask 0 nz-ac-mask 0)))
                    (build mbx mby)
                    (let ((k (* 3 (+ (* 2 segment) (if use-pred-y16 0 1))))
                          (m (* 4 (+ (* mbw mby) mbx))))
                      (setf (aref mb-filter m) (aref filter-params k)
                            (aref mb-filter (+ m 1)) (aref filter-params (+ k 1))
                            (aref mb-filter (+ m 2)) (aref filter-params (+ k 2))
                            (aref mb-filter (+ m 3)) (if (or (not use-pred-y16) (not skip)) 1 0)))))))
            (when (vp8-bits-eof fp) (%webp-fail "a VP8 first partition cut short"))
            (loop for r across partitions
                  when (vp8-bits-eof r) do (%webp-fail "a VP8 token partition cut short"))
            (unless (zerop filter-level) (loop-filter))
            (values yp up vp width height y-stride uv-stride)))))))

;;; --- VP8: YUV 4:2:0 to RGB, as libwebp converts -------------------------------
;;; libwebp's "fancy upsampling" (src/dsp/upsampling.c): each chroma sample
;;; is spread over the 2x2 luma pixels it covers by weights 9-3-3-1 toward its
;;; neighbours, then BT.601 at limited range in 14-bit fixed point
;;; (src/dsp/yuv.h).

(declaim (inline %yuv-clip))
(defun %yuv-clip (v)
  (declare (type fixnum v))
  (if (<= 0 v 16383) (ash v -6) (if (< v 0) 0 255)))

(defun webp-yuv-to-rgb (yp up vp width height y-stride uv-stride channels)
  "The picture in planes YP, UP and VP as WIDTH x HEIGHT pixels of CHANNELS
samples, R G B and, with 4, an alpha of 255."
  (declare (type (simple-array (unsigned-byte 8) (*)) yp up vp)
           (type fixnum width height y-stride uv-stride) (type (integer 3 4) channels)
           (optimize speed (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((out (%webp-octets (* width height channels))))
    (when (= channels 4) (fill out 255))
    (labels ((put (y u v at)
               (declare (type fixnum y u v at))
               (let ((luma (ash (* y 19077) -8)))
                 (setf (aref out at) (%yuv-clip (+ luma (ash (* v 26149) -8) -14234))
                       (aref out (+ at 1)) (%yuv-clip (+ luma (- (ash (* u 6419) -8)) (- (ash (* v 13320) -8)) 8708))
                       (aref out (+ at 2)) (%yuv-clip (+ luma (ash (* u 33050) -8) -17685)))))
             (pair (top-y bottom-y top-uv cur-uv top-out bottom-out)
               ;; Luma rows TOP-Y and BOTTOM-Y (NIL for none) between chroma
               ;; rows TOP-UV and CUR-UV, written at TOP-OUT and BOTTOM-OUT.
               (declare (type fixnum top-y top-uv cur-uv top-out))
               (let ((tl-u (aref up top-uv)) (tl-v (aref vp top-uv))
                     (l-u (aref up cur-uv)) (l-v (aref vp cur-uv)))
                 (declare (type fixnum tl-u tl-v l-u l-v))
                 (flet ((edge (x)
                          ;; Column X, at an edge: its one chroma column, weighted 3-1 between the rows.
                          (declare (type fixnum x))
                          (put (aref yp (+ top-y x)) (ash (+ (* 3 tl-u) l-u 2) -2) (ash (+ (* 3 tl-v) l-v 2) -2)
                               (+ top-out (* channels x)))
                          (when bottom-y
                            (put (aref yp (+ bottom-y x)) (ash (+ (* 3 l-u) tl-u 2) -2) (ash (+ (* 3 l-v) tl-v 2) -2)
                                 (+ bottom-out (* channels x))))))
                 (edge 0)
                 (loop for x of-type fixnum from 1 to (ash (1- width) -1)
                       do (let* ((t-u (aref up (+ top-uv x))) (t-v (aref vp (+ top-uv x)))
                                 (u (aref up (+ cur-uv x))) (v (aref vp (+ cur-uv x)))
                                 (avg-u (+ tl-u t-u l-u u 8)) (avg-v (+ tl-v t-v l-v v 8))
                                 (d12-u (ash (+ avg-u (* 2 (+ t-u l-u))) -3))
                                 (d12-v (ash (+ avg-v (* 2 (+ t-v l-v))) -3))
                                 (d03-u (ash (+ avg-u (* 2 (+ tl-u u))) -3))
                                 (d03-v (ash (+ avg-v (* 2 (+ tl-v v))) -3))
                                 (odd (1- (* 2 x))) (even (* 2 x)))
                            (declare (type fixnum t-u t-v u v avg-u avg-v d12-u d12-v d03-u d03-v odd even))
                            (put (aref yp (+ top-y odd)) (ash (+ d12-u tl-u) -1) (ash (+ d12-v tl-v) -1)
                                 (+ top-out (* channels odd)))
                            (put (aref yp (+ top-y even)) (ash (+ d03-u t-u) -1) (ash (+ d03-v t-v) -1)
                                 (+ top-out (* channels even)))
                            (when bottom-y
                              (put (aref yp (+ bottom-y odd)) (ash (+ d03-u l-u) -1) (ash (+ d03-v l-v) -1)
                                   (+ bottom-out (* channels odd)))
                              (put (aref yp (+ bottom-y even)) (ash (+ d12-u u) -1) (ash (+ d12-v v) -1)
                                   (+ bottom-out (* channels even))))
                            (setf tl-u t-u tl-v t-v l-u u l-v v)))
                 (when (evenp width) (edge (1- width)))))))
      (let ((row-out (* width channels)))
        (pair 0 nil 0 0 0 0)
        (loop for k of-type fixnum from 1 to (ash (1- height) -1)
              do (pair (* y-stride (1- (* 2 k))) (* y-stride (* 2 k))
                       (* uv-stride (1- k)) (* uv-stride k)
                       (* row-out (1- (* 2 k))) (* row-out (* 2 k))))
        (when (evenp height)
          (let ((k (1- (ash height -1))))
            (pair (* y-stride (1- height)) nil (* uv-stride k) (* uv-stride k) (* row-out (1- height)) 0))))
      out)))

;;; --- VP8L: bits, least significant first --------------------------------------

(defstruct (vp8l-bits (:constructor %make-vp8l-bits (buffer position end))
                      (:copier nil) (:predicate nil))
  (buffer (%webp-octets 0) :type (simple-array (unsigned-byte 8) (*)))
  (position 0 :type fixnum)
  (end 0 :type fixnum)
  (bits 0 :type (unsigned-byte 32))
  (count 0 :type (integer 0 32)))

(declaim (inline %vp8l-pull %vp8l-drop))
(defun %vp8l-pull (r)
  "R's next byte shifted in above the bits it holds: NIL, R unchanged, at its end."
  (declare (type vp8l-bits r) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((at (vp8l-bits-position r)))
    (when (< at (vp8l-bits-end r))
      (setf (vp8l-bits-bits r) (logior (vp8l-bits-bits r)
                                       (the (unsigned-byte 32)
                                            (ash (aref (vp8l-bits-buffer r) at) (vp8l-bits-count r))))
            (vp8l-bits-position r) (1+ at)
            (vp8l-bits-count r) (+ (vp8l-bits-count r) 8)))))

(defun %vp8l-drop (r count)
  (declare (type vp8l-bits r) (type (integer 0 32) count))
  (setf (vp8l-bits-bits r) (ash (vp8l-bits-bits r) (- count))
        (vp8l-bits-count r) (- (vp8l-bits-count r) count)))

(defun vp8l-read (r count)
  "The next COUNT bits of R, least significant first, as a number."
  (declare (type vp8l-bits r) (type (integer 0 24) count) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop while (< (vp8l-bits-count r) count)
        do (unless (%vp8l-pull r) (%webp-fail "a VP8L stream that ends early")))
  (prog1 (logand (vp8l-bits-bits r) (1- (ash 1 count)))
    (%vp8l-drop r count)))

;;; --- VP8L: prefix codes --------------------------------------------------------
;;; A code is a binary tree of CHILDREN (-1 at a leaf, whose symbol is in
;;; SYMBOLS) with a 7-bit table in front: an entry is the symbol and its
;;; length plus one for a code that short, else the tree node 7 bits reach.

(defstruct (vp8l-code (:constructor %make-vp8l-code (children symbols))
                      (:copier nil) (:predicate nil))
  (table (make-array 128 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (128)))
  (children (make-array 1 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (*)))
  (symbols (make-array 1 :element-type 'fixnum :initial-element 0) :type (simple-array fixnum (*)))
  (used 1 :type fixnum))

(defun %vp8l-insert (code symbol bits length)
  (declare (type vp8l-code code) (type fixnum symbol bits length) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((table (vp8l-code-table code))
        (children (vp8l-code-children code))
        (base 0))
    (declare (type fixnum base))
    (if (> length 7)
        (setf base (ash (aref *webp-reversed-bytes* (logand (ash bits (- 7 length)) #xff)) -1))
        (progn
          (setf base (ash (aref *webp-reversed-bytes* (logand bits #xff)) (- length 8)))
          (dotimes (i (ash 1 (- 7 length)))
            (setf (aref table (logior base (ash i length))) (logior (ash symbol 8) (1+ length))))))
    (let ((n 0) (jump 7))
      (declare (type fixnum n jump))
      (loop while (> length 0)
            do (decf length)
               (case (aref children n)
                 (-1 (%webp-fail "a VP8L prefix code with one code inside another"))
                 (0 (let ((used (vp8l-code-used code)))
                      (when (> (+ used 2) (length children))
                        (%webp-fail "a VP8L prefix code with more codes than symbols"))
                      (setf (aref children n) used
                            (vp8l-code-used code) (+ used 2)))))
               (setf n (+ (aref children n) (logand 1 (ash bits (- length)))))
               (decf jump)
               (when (and (zerop jump) (zerop (aref table base)))
                 (setf (aref table base) (ash n 8))))
      (case (aref children n)
        (-1 nil)
        (0 (setf (aref children n) -1))
        (t (%webp-fail "a VP8L prefix code with one code inside another")))
      (setf (aref (vp8l-code-symbols code) n) symbol))))

(defun vp8l-build-code (lengths)
  "The prefix code whose symbols have code LENGTHS, as libwebp builds it: one
symbol alone takes no bits, and more must fill the code space exactly."
  (declare (type (simple-array fixnum (*)) lengths) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((count 0) (last 0) (kraft 0))
    (declare (type fixnum count last kraft))
    (dotimes (s (length lengths))
      (let ((l (aref lengths s)))
        (unless (zerop l)
          (incf count) (setf last s)
          (incf kraft (ash 1 (- 15 l))))))
    (when (zerop count) (%webp-fail "a VP8L prefix code of no symbols"))
    (let* ((size (1- (* 2 count)))
           (code (%make-vp8l-code (make-array size :element-type 'fixnum :initial-element 0)
                                  (make-array size :element-type 'fixnum :initial-element 0))))
      (if (= count 1)
          (%vp8l-insert code last 0 0)
          (let ((histogram (make-array 16 :element-type 'fixnum :initial-element 0))
                (next (make-array 16 :element-type 'fixnum :initial-element 0)))
            (unless (= kraft (ash 1 15))
              (%webp-fail "a VP8L prefix code that is not complete"))
            (loop for l across lengths unless (zerop l) do (incf (aref histogram l)))
            (let ((c 0))
              (declare (type fixnum c))
              (loop for l from 1 to 15
                    do (setf c (ash (+ c (aref histogram (1- l))) 1)
                             (aref next l) c)))
            (dotimes (s (length lengths))
              (let ((l (aref lengths s)))
                (unless (zerop l)
                  (%vp8l-insert code s (aref next l) l)
                  (incf (aref next l)))))))
      code)))

(defun vp8l-next (code r)
  "The symbol CODE decodes next from R."
  (declare (type vp8l-code code) (type vp8l-bits r) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((n 0))
    (declare (type fixnum n))
    ;; The table, while 7 bits are at hand; then the tree, a bit at a time.
    (when (or (>= (vp8l-bits-count r) 7) (%vp8l-pull r))
      (let* ((entry (aref (vp8l-code-table code) (logand (vp8l-bits-bits r) 127)))
             (length (logand entry #xff)))
        (unless (zerop length)
          (%vp8l-drop r (1- length))
          (return-from vp8l-next (ash entry -8)))
        (setf n (ash entry -8))
        (%vp8l-drop r 7)))
    (let ((children (vp8l-code-children code)))
      (loop until (= (aref children n) -1)
            do (when (and (zerop (vp8l-bits-count r)) (not (%vp8l-pull r)))
                 (%webp-fail "a VP8L stream that ends early"))
               (setf n (+ (aref children n) (logand (vp8l-bits-bits r) 1)))
               (%vp8l-drop r 1)))
    (aref (vp8l-code-symbols code) n)))

(defun vp8l-read-code (r alphabet-size)
  "The prefix code over ALPHABET-SIZE symbols stored next in R."
  (declare (type vp8l-bits r) (type fixnum alphabet-size))
  (let ((lengths (make-array alphabet-size :element-type 'fixnum :initial-element 0)))
    (if (= 1 (vp8l-read r 1))
        (let* ((count (1+ (vp8l-read r 1)))
               (first (vp8l-read r (if (= 1 (vp8l-read r 1)) 8 1))))
          (when (>= first alphabet-size) (%webp-fail "a VP8L code for a symbol past its alphabet"))
          (setf (aref lengths first) 1)
          (when (= count 2)
            (let ((second (vp8l-read r 8)))
              (when (>= second alphabet-size) (%webp-fail "a VP8L code for a symbol past its alphabet"))
              (setf (aref lengths second) 1))))
        (let ((codes (+ 4 (vp8l-read r 4)))
              (length-lengths (make-array 19 :element-type 'fixnum :initial-element 0)))
          (when (> codes 19) (%webp-fail "a VP8L code length code of ~d lengths" codes))
          (dotimes (i codes)
            (setf (aref length-lengths (aref *vp8l-code-length-order* i)) (vp8l-read r 3)))
          (let ((length-code (vp8l-build-code length-lengths))
                (limit alphabet-size))
            (when (= 1 (vp8l-read r 1))
              (setf limit (+ 2 (vp8l-read r (+ 2 (* 2 (vp8l-read r 3))))))
              (when (> limit alphabet-size) (%webp-fail "a VP8L code with more lengths than symbols")))
            (let ((symbol 0) (previous 8))
              (loop while (and (< symbol alphabet-size) (> limit 0))
                    do (decf limit)
                       (let ((length (vp8l-next length-code r)))
                         (if (< length 16)
                             (progn (setf (aref lengths symbol) length)
                                    (incf symbol)
                                    (unless (zerop length) (setf previous length)))
                             (let* ((k (- length 16))
                                    (repeat (+ (vp8l-read r (aref #(2 3 7) k)) (aref #(3 3 11) k)))
                                    (value (if (= length 16) previous 0)))
                               (when (> (+ symbol repeat) alphabet-size)
                                 (%webp-fail "a VP8L code with more lengths than symbols"))
                               (loop repeat repeat
                                     do (setf (aref lengths symbol) value)
                                        (incf symbol))))))))))
    (vp8l-build-code lengths)))

;;; --- VP8L: an entropy-coded image ------------------------------------------------

(declaim (inline %vp8l-tiles))
(defun %vp8l-tiles (size bits)
  (declare (type fixnum size) (type (integer 0 9) bits))
  (ash (+ size (1- (ash 1 bits))) (- bits)))

(defun %vp8l-prefixed (r symbol)
  ;; A length or distance: SYMBOL, and extra bits read after it.
  (declare (type vp8l-bits r) (type fixnum symbol))
  (if (< symbol 4)
      (1+ symbol)
      (let ((extra (ash (- symbol 2) -1)))
        (+ (ash (+ 2 (logand symbol 1)) extra) (vp8l-read r extra) 1))))

(defun %vp8l-distance (width code)
  (declare (type fixnum width code))
  (if (> code 120)
      (- code 120)
      (let ((d (aref *vp8l-distance-map* (1- code))))
        (max 1 (+ (* (ash d -4) width) (- 8 (logand d #xf)))))))

(defun vp8l-decode-pixels (r width height top-level)
  "The WIDTH x HEIGHT image coded next in R, 4 bytes R G B A a pixel. Only
the TOP-LEVEL image may switch prefix codes by region."
  (declare (type vp8l-bits r) (type fixnum width height)
           (optimize speed (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((cache nil) (cache-shift 0) (cache-bits 0))
    (declare (type fixnum cache-shift cache-bits))
    (when (= 1 (vp8l-read r 1))
      (setf cache-bits (vp8l-read r 4))
      (unless (<= 1 cache-bits 11) (%webp-fail "a VP8L color cache of ~d bits" cache-bits))
      (setf cache (make-array (ash 1 cache-bits) :element-type '(unsigned-byte 32) :initial-element 0)
            cache-shift (- 32 cache-bits)))
    (let ((meta-bits 0) (meta nil) (group-count 1))
      (declare (type fixnum meta-bits group-count))
      (when (and top-level (= 1 (vp8l-read r 1)))
        (setf meta-bits (+ 2 (vp8l-read r 3))
              meta (vp8l-decode-pixels r (%vp8l-tiles width meta-bits) (%vp8l-tiles height meta-bits) nil))
        (let ((most 0))
          (declare (type fixnum most))
          (loop for p of-type fixnum from 0 below (length (the (simple-array (unsigned-byte 8) (*)) meta)) by 4
                do (setf most (max most (logior (ash (aref meta p) 8) (aref meta (1+ p))))))
          ;; libwebp's encoder writes at most 2600 groups (MAX_HUFF_IMAGE_SIZE).
          (when (>= most 2600) (%webp-fail "a VP8L image of ~:d prefix code groups" (1+ most)))
          (setf group-count (1+ most))))
      (let ((groups (make-array group-count)))
        (dotimes (g group-count)
          (setf (svref groups g)
                (vector (vp8l-read-code r (+ 280 (if cache (ash 1 cache-bits) 0)))
                        (vp8l-read-code r 256) (vp8l-read-code r 256) (vp8l-read-code r 256)
                        (vp8l-read-code r 40))))
        (let* ((pix (%webp-octets (* 4 width height)))
               (total (length pix))
               (meta-mask (if (zerop meta-bits) 0 (1- (ash 1 meta-bits))))
               (tiles-per-row (if (zerop meta-bits) 0 (%vp8l-tiles width meta-bits)))
               (p 0) (cached 0) (x 0) (y 0)
               (group (svref groups 0))
               (lookup (/= meta-mask 0)))
          (declare (type fixnum total meta-mask tiles-per-row p cached x y) (type simple-vector group))
          (loop while (< p total)
                do (when lookup
                     (let ((i (* 4 (+ (* tiles-per-row (ash y (- meta-bits))) (ash x (- meta-bits))))))
                       (declare (type (simple-array (unsigned-byte 8) (*)) meta))
                       (setf group (svref groups (logior (ash (aref meta i) 8) (aref meta (1+ i)))))))
                   (let ((green (vp8l-next (svref group 0) r)))
                     (declare (type fixnum green))
                     (cond
                       ((< green 256)
                        (setf (aref pix p) (vp8l-next (svref group 1) r)
                              (aref pix (+ p 1)) green
                              (aref pix (+ p 2)) (vp8l-next (svref group 2) r)
                              (aref pix (+ p 3)) (vp8l-next (svref group 3) r)))
                       ((< green 280)
                        (let* ((length (%vp8l-prefixed r (- green 256)))
                               (distance (%vp8l-distance width (%vp8l-prefixed r (vp8l-next (svref group 4) r))))
                               (end (+ p (* 4 length)))
                               (q (- p (* 4 distance))))
                          (declare (type fixnum length distance end q))
                          (when (or (< q 0) (> end total))
                            (%webp-fail "a VP8L backward reference outside the picture"))
                          (loop while (< p end)
                                do (setf (aref pix p) (aref pix q))
                                   (incf p) (incf q))
                          (incf x length)
                          (loop while (>= x width) do (decf x width) (incf y))
                          (setf lookup (/= meta-mask 0))))
                       (t
                        (unless cache (%webp-fail "a VP8L color cache symbol with no cache"))
                        (let ((cache cache))
                          (declare (type (simple-array (unsigned-byte 32) (*)) cache))
                          (loop while (< cached p)
                                do (let ((argb (logior (ash (aref pix (+ cached 3)) 24) (ash (aref pix cached) 16)
                                                       (ash (aref pix (+ cached 1)) 8) (aref pix (+ cached 2)))))
                                     (declare (type (unsigned-byte 32) argb))
                                     (setf (aref cache (ash (logand (* argb #x1e35a7bd) #xffffffff) (- cache-shift)))
                                           argb))
                                   (incf cached 4))
                          (let ((index (- green 280)))
                            (when (>= index (length cache)) (%webp-fail "a VP8L color cache index past the cache"))
                            (let ((argb (aref cache index)))
                              (setf (aref pix p) (ldb (byte 8 16) argb)
                                    (aref pix (+ p 1)) (ldb (byte 8 8) argb)
                                    (aref pix (+ p 2)) (ldb (byte 8 0) argb)
                                    (aref pix (+ p 3)) (ldb (byte 8 24) argb)))))))
                     ;; A literal or a cached color: one pixel on.
                     (when (or (< green 256) (>= green 280))
                       (incf p 4)
                       (incf x)
                       (when (= x width) (setf x 0) (incf y))
                       (setf lookup (and (/= meta-mask 0) (zerop (logand x meta-mask)))))))
          pix)))))

;;; --- VP8L: the transforms ---------------------------------------------------------

(defun %vp8l-unpredict (pix width height bits modes)
  ;; The predictor transform: each pixel was stored less a prediction from its
  ;; neighbours, by the mode of its tile.
  (declare (type (simple-array (unsigned-byte 8) (*)) pix modes) (type fixnum width height)
           (type (integer 0 9) bits) (optimize speed (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (or (zerop width) (zerop height)) (return-from %vp8l-unpredict pix))
  (macrolet ((add (k value) `(setf (aref pix (+ p ,k)) (logand (+ (aref pix (+ p ,k)) ,value) 255))))
    (flet ((avg2 (a b) (declare (type fixnum a b)) (ash (+ a b) -1))
           (clamp (v) (declare (type fixnum v)) (max 0 (min 255 v))))
      (declare (inline avg2 clamp))
      (let ((p 0))
        (declare (type fixnum p))
        (add 3 255)
        (loop for q of-type fixnum from 4 below (* 4 width)
              do (setf (aref pix q) (logand (+ (aref pix q) (aref pix (- q 4))) 255)))
        (setf p (* 4 width))
        (let ((tiles-per-row (%vp8l-tiles width bits)) (mask (1- (ash 1 bits))) (top 0))
          (declare (type fixnum tiles-per-row mask top))
          (loop for y of-type fixnum from 1 below height
                do (dotimes (k 4) (add k (aref pix (+ top k))))
                   (incf p 4) (incf top 4)
                   (let* ((q (* 4 (ash y (- bits)) tiles-per-row))
                          (mode (logand (aref modes (1+ q)) #x0f)))
                     (declare (type fixnum q mode))
                     (incf q 4)
                     (loop for x of-type fixnum from 1 below width
                           do (when (zerop (logand x mask))
                                (setf mode (logand (aref modes (1+ q)) #x0f))
                                (incf q 4))
                              (macrolet ((each (form)
                                           ;; FORM per channel K, L/T/TR/TL the neighbours' bytes.
                                           `(dotimes (k 4)
                                              (let ((l (aref pix (+ p k -4))) (tt (aref pix (+ top k)))
                                                    (tr (aref pix (+ top k 4))) (tl (aref pix (+ top k -4))))
                                                (declare (type fixnum l tt tr tl) (ignorable l tt tr tl))
                                                (add k ,form)))))
                                (case mode
                                  (1 (each l))
                                  (2 (each tt))
                                  (3 (each tr))
                                  (4 (each tl))
                                  (5 (each (avg2 (avg2 l tr) tt)))
                                  (6 (each (avg2 l tl)))
                                  (7 (each (avg2 l tt)))
                                  (8 (each (avg2 tl tt)))
                                  (9 (each (avg2 tt tr)))
                                  (10 (each (avg2 (avg2 l tl) (avg2 tt tr))))
                                  (11 (let ((sum-l 0) (sum-t 0))
                                        (declare (type fixnum sum-l sum-t))
                                        (dotimes (k 4)
                                          (let ((tl (aref pix (+ top k -4))))
                                            (incf sum-l (abs (- (aref pix (+ top k)) tl)))
                                            (incf sum-t (abs (- (aref pix (+ p k -4)) tl)))))
                                        (if (< sum-l sum-t)
                                            (each l)
                                            (each tt))))
                                  (12 (each (clamp (- (+ l tt) tl))))
                                  (13 (each (let ((a (avg2 l tt))) (clamp (+ a (truncate (- a tl) 2))))))
                                  (t (add 3 255))))
                              (incf p 4) (incf top 4))))))))
  pix)

(defun %vp8l-uncross-color (pix width height bits multipliers)
  (declare (type (simple-array (unsigned-byte 8) (*)) pix multipliers) (type fixnum width height)
           (type (integer 0 9) bits) (optimize speed (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((s8 (b) (declare (type (unsigned-byte 8) b)) (if (> b 127) (- b 256) b)))
    (declare (inline s8))
    (let ((p 0) (tiles-per-row (%vp8l-tiles width bits)) (mask (1- (ash 1 bits))))
      (declare (type fixnum p tiles-per-row mask))
      (dotimes (y height)
        (let ((q (* 4 (ash y (- bits)) tiles-per-row)) (g2r 0) (g2b 0) (r2b 0))
          (declare (type fixnum q g2r g2b r2b))
          (dotimes (x width)
            (when (zerop (logand x mask))
              (setf r2b (s8 (aref multipliers q))
                    g2b (s8 (aref multipliers (+ q 1)))
                    g2r (s8 (aref multipliers (+ q 2))))
              (incf q 4))
            (let* ((green (s8 (aref pix (+ p 1))))
                   (red (logand (+ (aref pix p) (ash (* g2r green) -5)) 255))
                   (blue (logand (+ (aref pix (+ p 2)) (ash (* g2b green) -5) (ash (* r2b (s8 red)) -5)) 255)))
              (setf (aref pix p) red (aref pix (+ p 2)) blue))
            (incf p 4))))))
  pix)

(defun %vp8l-add-green (pix)
  (declare (type (simple-array (unsigned-byte 8) (*)) pix) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop for p of-type fixnum from 0 below (length pix) by 4
        do (let ((green (aref pix (+ p 1))))
             (setf (aref pix p) (logand (+ (aref pix p) green) 255)
                   (aref pix (+ p 2)) (logand (+ (aref pix (+ p 2)) green) 255))))
  pix)

(defun %vp8l-unindex (pix width height bits palette)
  ;; The color indexing transform: each pixel's green is an index into
  ;; PALETTE, 2^BITS of them packed into one when the palette is small.
  (declare (type (simple-array (unsigned-byte 8) (*)) pix palette) (type fixnum width height)
           (type (integer 0 3) bits) (optimize speed (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (zerop bits)
      (loop for p of-type fixnum from 0 below (length pix) by 4
            do (let ((i (* 4 (aref pix (+ p 1)))))
                 (replace pix palette :start1 p :start2 i :end2 (+ i 4)))
            finally (return pix))
      (let* ((out (%webp-octets (* 4 width height)))
             (per-pixel (ash 8 (- bits)))
             (value-mask (1- (ash 1 per-pixel)))
             (x-mask (1- (ash 1 bits)))
             (d 0) (p 0) (v 0))
        (declare (type fixnum per-pixel value-mask x-mask d p v))
        (dotimes (y height)
          (dotimes (x width)
            (when (zerop (logand x x-mask))
              (setf v (aref pix (+ p 1)))
              (incf p 4))
            (let ((i (* 4 (logand v value-mask))))
              (replace out palette :start1 d :start2 i :end2 (+ i 4)))
            (incf d 4)
            (setf v (ash v (- per-pixel)))))
        out)))

(defun vp8l-decode (data start end &optional width height)
  "Decode the VP8L stream in DATA from START below END. With WIDTH and
HEIGHT the stream is headerless, as an ALPH chunk's is. => (values PIXELS
WIDTH HEIGHT ALPHA-P), PIXELS 4 bytes R G B A a pixel, ALPHA-P the header's
word that alpha is used."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start end))
  (let ((r (%make-vp8l-bits data start end)) (alpha nil))
    (unless width
      (unless (= (vp8l-read r 8) #x2f) (%webp-fail "a VP8L stream without its signature"))
      (setf width (1+ (vp8l-read r 14))
            height (1+ (vp8l-read r 14))
            alpha (= 1 (vp8l-read r 1)))
      (unless (zerop (vp8l-read r 3)) (%webp-fail "a VP8L stream of a version past 0")))
    (%webp-check-size width height)
    (let ((transforms '()) (seen 0) (coded-width width))
      (declare (type fixnum seen coded-width))
      (loop while (= 1 (vp8l-read r 1))
            do (let ((type (vp8l-read r 2)))
                 (when (logbitp type seen) (%webp-fail "a VP8L transform used twice"))
                 (setf seen (logior seen (ash 1 type)))
                 (ecase type
                   ((0 1)
                    (let ((bits (+ 2 (vp8l-read r 3))))
                      (push (list type coded-width bits
                                  (vp8l-decode-pixels r (%vp8l-tiles coded-width bits) (%vp8l-tiles height bits) nil))
                            transforms)))
                   (2 (push (list 2 coded-width 0 nil) transforms))
                   (3 (let* ((colors (1+ (vp8l-read r 8)))
                             (bits (cond ((<= colors 2) 3) ((<= colors 4) 2) ((<= colors 16) 1) (t 0)))
                             (palette (%webp-octets 1024)))
                        (replace palette (vp8l-decode-pixels r colors 1 nil))
                        (loop for p from 4 below (* 4 colors)
                              do (setf (aref palette p) (logand (+ (aref palette p) (aref palette (- p 4))) 255)))
                        (push (list 3 coded-width bits palette) transforms)
                        (setf coded-width (%vp8l-tiles coded-width bits)))))))
      (let ((pix (vp8l-decode-pixels r coded-width height t)))
        (loop for (type transform-width bits data) in transforms
              do (setf pix (ecase type
                             (0 (%vp8l-unpredict pix transform-width height bits data))
                             (1 (%vp8l-uncross-color pix transform-width height bits data))
                             (2 (%vp8l-add-green pix))
                             (3 (%vp8l-unindex pix transform-width height bits data)))))
        (values pix width height alpha)))))

;;; --- the container -------------------------------------------------------------------

(defun %webp-alpha (data start end width height)
  "The WIDTH x HEIGHT alpha plane of the ALPH chunk in DATA from START below END."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start end width height))
  (when (>= start end) (%webp-fail "an empty ALPH chunk"))
  (let* ((header (aref data start))
         (size (* width height))
         (alpha (case (logand header 3)
                  (0 (when (< (- end start 1) size) (%webp-fail "an ALPH chunk shorter than its picture"))
                     (subseq data (1+ start) (+ start 1 size)))
                  (1 (let ((pix (vp8l-decode data (1+ start) end width height))
                           (plane (%webp-octets size)))
                       (dotimes (i size plane) (setf (aref plane i) (aref pix (1+ (* 4 i)))))))
                  (t (%webp-fail "an ALPH chunk compressed by method ~d" (logand header 3))))))
    (declare (type (simple-array (unsigned-byte 8) (*)) alpha))
    (flet ((add (i value) (setf (aref alpha i) (logand (+ (aref alpha i) value) 255))))
      (let ((filter (logand (ash header -2) 3)))
        (unless (zerop filter)
          (loop for i from 1 below width do (add i (aref alpha (1- i))))
          (loop for row from width below size by width
                do (add row (aref alpha (- row width)))
                   (loop for i from (1+ row) below (+ row width)
                         do (add i (ecase filter
                                     (1 (aref alpha (1- i)))
                                     (2 (aref alpha (- i width)))
                                     (3 (max 0 (min 255 (- (+ (aref alpha (1- i)) (aref alpha (- i width)))
                                                            (aref alpha (- i width 1)))))))))))))
    alpha))

(defun %webp-chunks (data start end)
  "The RIFF chunks in DATA from START below END: a list of (FOURCC START END)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum start end))
  (loop with at = start
        while (<= (+ at 8) end)
        collect (let ((size (%webp-le data (+ at 4) 4)))
                  (prog1 (list (map 'string #'code-char (subseq data at (+ at 4)))
                               (+ at 8) (min end (+ at 8 size)))
                    (setf at (+ at 8 size (logand size 1)))))))

(defun %webp-frame (data chunks width height)
  "The picture of CHUNKS (an optional ALPH, then VP8 or VP8L) of DATA, which
must be WIDTH x HEIGHT: 4 bytes R G B A a pixel."
  (let ((alpha nil))
    (loop for (id start end) in chunks
          do (cond
               ((string= id "ALPH") (setf alpha (list start end)))
               ((string= id "VP8 ")
                (multiple-value-bind (yp up vp w h y-stride uv-stride) (vp8-decode data start end)
                  (unless (and (= w width) (= h height))
                    (%webp-fail "a VP8 frame of ~dx~d in a canvas of ~dx~d" w h width height))
                  (let ((pix (webp-yuv-to-rgb yp up vp w h y-stride uv-stride 4)))
                    (when alpha
                      (let ((plane (%webp-alpha data (first alpha) (second alpha) w h)))
                        (dotimes (i (* w h)) (setf (aref pix (+ 3 (* 4 i))) (aref plane i)))))
                    (return-from %webp-frame pix))))
               ((string= id "VP8L")
                (multiple-value-bind (pix w h) (vp8l-decode data start end)
                  (unless (and (= w width) (= h height))
                    (%webp-fail "a VP8L picture of ~dx~d in a canvas of ~dx~d" w h width height))
                  (return-from %webp-frame pix)))))
    (%webp-fail "a WebP frame with no VP8 or VP8L picture")))

(defun %webp-picture (pix width height alpha)
  "PIX, 4 bytes a pixel, as WEBP-DECODE answers it: whole with ALPHA, else R G B."
  (if alpha
      (values pix width height 4)
      (let ((out (%webp-octets (* 3 width height))))
        (loop for p from 0 below (length pix) by 4
              for o from 0 by 3
              do (replace out pix :start1 o :start2 p :end2 (+ p 3)))
        (values out width height 3))))

(defun webp-decode (octets)
  "The picture in OCTETS, a WebP file's bytes: (values PIXELS WIDTH HEIGHT
CHANNELS), PIXELS row-major octets of CHANNELS samples a pixel -- 3, R G B,
or 4, R G B and straight alpha when the file says it has alpha. Of an
animation, its first frame on the canvas. Signals WEBP-MALFORMED for bytes
it cannot read and for a canvas past *WEBP-MAX-PIXELS*."
  (let* ((data (coerce octets '(simple-array (unsigned-byte 8) (*))))
         (length (length data)))
    (unless (and (>= length 20)
                 (string= "RIFF" (map 'string #'code-char (subseq data 0 4)))
                 (string= "WEBP" (map 'string #'code-char (subseq data 8 12))))
      (%webp-fail "bytes that open no RIFF WEBP file"))
    (let* ((end (min length (+ 8 (%webp-le data 4 4))))
           (chunks (%webp-chunks data 12 end)))
      (unless chunks (%webp-fail "a WebP file of no chunks"))
      (destructuring-bind (id start chunk-end) (first chunks)
        (cond
          ((string= id "VP8 ")
           (multiple-value-bind (yp up vp w h y-stride uv-stride) (vp8-decode data start chunk-end)
             (values (webp-yuv-to-rgb yp up vp w h y-stride uv-stride 3) w h 3)))
          ((string= id "VP8L")
           (multiple-value-bind (pix w h alpha) (vp8l-decode data start chunk-end)
             (%webp-picture pix w h alpha)))
          ((string= id "VP8X")
           (when (< (- chunk-end start) 10) (%webp-fail "a VP8X chunk shorter than 10 bytes"))
           (let* ((flags (aref data start))
                  (alpha-flag (logbitp 4 flags))
                  (width (1+ (%webp-le data (+ start 4))))
                  (height (1+ (%webp-le data (+ start 7))))
                  (rest (rest chunks)))
             (%webp-check-size width height)
             (if (logbitp 1 flags)
                 (let ((frame (find "ANMF" rest :key #'first :test #'string=)))
                   (unless frame (%webp-fail "an animated WebP with no frame"))
                   (destructuring-bind (fstart fend) (rest frame)
                     (when (< (- fend fstart) 16) (%webp-fail "an ANMF chunk shorter than its header"))
                     (let* ((x (* 2 (%webp-le data fstart)))
                            (y (* 2 (%webp-le data (+ fstart 3))))
                            (fw (1+ (%webp-le data (+ fstart 6))))
                            (fh (1+ (%webp-le data (+ fstart 9)))))
                       (when (or (> (+ x fw) width) (> (+ y fh) height))
                         (%webp-fail "an animation frame outside its canvas"))
                       (let ((pix (%webp-frame data (%webp-chunks data (+ fstart 16) fend) fw fh))
                             (canvas (%webp-octets (* 4 width height))))
                         (dotimes (row fh)
                           (replace canvas pix :start1 (* 4 (+ (* (+ y row) width) x))
                                               :start2 (* 4 row fw) :end2 (* 4 (1+ row) fw)))
                         (%webp-picture canvas width height alpha-flag)))))
                 ;; libwebp's has_alpha: the flag, or an ALPH chunk without it.
                 (%webp-picture (%webp-frame data rest width height) width height
                                (or alpha-flag (find "ALPH" rest :key #'first :test #'string=))))))
          (t (%webp-fail "a WebP whose first chunk is ~s, not VP8, VP8L or VP8X" id)))))))

(defun webp-pixels (octets)
  "OCTETS, a WebP, decoded to PIXELS for FIT-IMAGE: *IMAGE-DECODERS*'s
image/webp. Signals IMAGE-UNFIT, saying why, for bytes it cannot read --
and for any other error inside the decoder, so a file no one foresaw costs
the picture, never the turn."
  (handler-case (multiple-value-bind (data width height channels) (webp-decode octets)
                  (%make-pixels width height channels data))
    (webp-malformed (e)
      (error 'image-unfit :reason (format nil "the WebP does not decode here: ~a" (webp-malformed-reason e))))
    (error (e)
      (error 'image-unfit :reason (format nil "the WebP does not decode here: ~a" e)))))

;;;; --- Go's license, for the parts ported from golang.org/x/image ------------
;;;;
;;;; Copyright 2009 The Go Authors.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:
;;;;
;;;;    * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;    * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;    * Neither the name of Google LLC nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
