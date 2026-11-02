;;;; json.lisp --- the decoded-wire-value waist.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every surface in this image reads foreign JSON: provider SSE frames,
;;;; gateway envelopes, auth.json, the models.dev catalog, config.jsonc. shasht
;;;; hands back whatever Lisp type the peer's bytes decoded to, and three of its
;;;; decodings are traps:
;;;;
;;;;   JSON null  -> :NULL   a truthy keyword, so (or value "") passes it into
;;;;                         string-typed slots and the crash lands elsewhere
;;;;   JSON false -> NIL     indistinguishable from an absent key by value
;;;;   JSON true  -> T
;;;;
;;;; Before this file every reader re-derived that table inline. The shapes
;;;; recur across provider, client, gateway, ui, events, exec, lifecycle,
;;;; checkpoint, turn, theme, identity and store:
;;;;
;;;;   (and (hash-table-p object) (gethash "k" object))
;;;;   (let ((v (gethash "k" object))) (and (integerp v) v))
;;;;   (eq t (gethash "k" object))
;;;;   (flet ((int-field (key) ...)) ...)          ; the same probe, re-fletted
;;;;
;;;; plus four competing coercion authorities (WIRE-STRING,
;;;; WIRE-NONEMPTY-STRING, WIRE-OPTIONAL-STRING, JSON-INTEGER) that each knew
;;;; one third of the table. This file is the one authority: JSON-VALUE decides
;;;; what a decoded value means, and WITH-JSON is the binding form that makes
;;;; the decision unavoidable at the 600-odd sites that bind a wire field to a
;;;; local.
;;;;
;;;; The same file owns the encode side: JSON-OBJECT (literal keys, checked at
;;;; macroexpansion, with conditional fields) and MAKE-JSON-OBJECT (runtime
;;;; pairs) are the two ways a wire object is built, so no producer hand-rolls
;;;; MAKE-HASH-TABLE + SETF GETHASH chains and the object representation is a
;;;; fact of this file alone.
;;;;
;;;; And the read side: DECODE-JSON is the one shasht read seam, the way
;;;; ENCODE-JSON-OBJECT is the write seam. shasht reads correctly and
;;;; loosely — every string an adjustable 32-slot buffer behind an array
;;;; header, every array the same, every object a default-sized table, every
;;;; key a fresh copy — and a document the organism keeps costs it twenty
;;;; times its text: the models.dev catalog, 3.8 MB of JSON, held 66 MB of
;;;; heap. DECODE-JSON hands back the same shapes held tightly, so what a
;;;; reader keeps costs what it says.
;;;;
;;;; This is the lenient dialect, for data a peer produced: a value of the
;;;; wrong type reads as NIL. PROTOCOL.LISP owns the strict dialect for the
;;;; gateway envelope contract, where a wrong type is a refusal
;;;; (REQUIRED-STRING and friends). Both dialects are deliberate; a reader
;;;; picks one by which side of the trust boundary it stands on.

(in-package #:nodecode.kernel)

;; EVAL-WHEN because WITH-JSON consults this list while expanding, which is
;; compile time for every file that uses it -- including, one day, this one.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; WITH-JSON validates against this list at macroexpansion time, so a typo
  ;; in a spec is a compile error rather than a field that silently reads NIL
  ;; forever.
  (defparameter +json-types+
    '(:any :string :text :integer :number :boolean :object :array)
    "The coercions JSON-VALUE offers."))

;;; --- reading: the one JSON reader ----------------------------------------
;;;
;;; shasht reads correctly and loosely. Every string arrives as a 32-slot
;;; adjustable buffer behind an array header, every array the same, every
;;; object a default-sized table, and the whole loose tree is live at once
;;; before anything can be tightened: the models.dev catalog, 3.8 MB of
;;; text, cost 66 MB of heap to decode and 17 MB to keep, and the 66 MB
;;; stood in the process's peak. A reader that builds the tight shapes as
;;; it goes never holds the loose one, so the peak is the result and the
;;; garbage is one string at a time.
;;;
;;; The shapes are shasht's, because the image already reads them: objects
;;; are EQUAL hash tables, arrays are vectors, true is T, false is NIL and
;;; null is :NULL. What changed is what they cost. Tight means exactly
;;; sized: a string is simple and no longer than its characters, narrowed
;;; to a base string when every character fits one (most wire text does,
;;; and the wide form costs four bytes a character); an array is a simple
;;; vector of its length; an object is a table of its count. The reader
;;; walks UTF-8 octets, the form a document arrives and is kept in, so a
;;; file read for it is read as bytes and never widened. Writing is still
;;; shasht's — ENCODE-JSON-OBJECT below, and ENCODE-JSON-OCTETS for a body
;;; that leaves the process — and this dialect is fixed rather than read
;;; from shasht's specials: one dialect is the point.

(define-error json-refusal (error)
    (detail position)
  (:documentation
   "JSON text this reader cannot read, and the byte it stopped at.")
  (:report "malformed JSON at byte ~a: ~a" position detail)
  (:export))

;;; Decoded values are never written to, so one string can stand for every
;;; equal occurrence -- in every document, not only the one it was read in:
;;; a session's events, a ledger's lines and a room's messages repeat the
;;; same keys and values each, and a decoder that shared them per document
;;; kept one copy per document of each.
(defparameter +json-shared-string-length+ 64
  "Strings this long or shorter are shared across every decoded document: the
keys, and the small enumerated values — \"text\", \"assistant\", \"medium\" —
that documents repeat by the thousand.")

(defparameter +json-shared-string-count+ 32768
  "How many strings the decoder shares before it starts over: what sharing
pins stays bounded, and a string read after a start-over is shared again
from its next occurrence.")

(defvar *json-shared-strings* (make-hash-table :test #'equal :synchronized t)
  "The short strings decoded so far, each the one every equal string a later
document decodes is read as (JSON-SHARED-STRING).")

(defun json-shared-string (string &aux (table *json-shared-strings*))
  "The string the decoder shares EQUAL to STRING, else STRING, shared from
now on; the table starts over once it holds +JSON-SHARED-STRING-COUNT+."
  ;; Two readers missing at once each share their own; the second stands,
  ;; and the first is one more copy, as every string was before.
  (or (gethash string table)
      (progn
        (when (>= (hash-table-count table) +json-shared-string-count+)
          (setf table (make-hash-table :test #'equal :synchronized t)
                *json-shared-strings* table))
        (setf (gethash string table) string))))

(defparameter +json-max-exponent+ 100000
  "The largest exponent a number may spell: far past any float (1e308), and
small enough that raising ten to it is instant.")

(defparameter +json-max-digits+ 4300
  "The most digits a number's whole part, fraction or exponent may spell:
Python's own cap on a decimal integer. Folding them costs the square of their
count, and a megabyte of them held the reader half a minute.")

;;; The reader descends, so depth is stack; wire JSON nests single digits deep
;;; and a document that nests hundreds is a peer's accident or a probe.
(defparameter +json-max-depth+ 256
  "How deep arrays and objects may nest before the reader refuses.")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline decode-utf-8))
(defun decode-utf-8 (octets from to out start malformed)
  "Bytes FROM..TO of OCTETS as the characters they spell, into OUT from
START; => where those characters end. Only well-formed UTF-8 reads: an
overlong form, a surrogate or a code past #x10FFFF calls MALFORMED, which
does not return."
  (declare (type octets octets)
           (type fixnum from to start)
           (type (simple-array character (*)) out)
           (type function malformed)
           (optimize speed))
  (let ((at from)
        (into start))
    (declare (type fixnum at into))
    (loop while (< at to)
          do (let* ((lead (aref octets at))
                    (width (cond ((< lead #x80) 1)
                                 ((<= #xc2 lead #xdf) 2)
                                 ((<= #xe0 lead #xef) 3)
                                 ((<= #xf0 lead #xf4) 4)
                                 (t (funcall malformed))))
                    (code (if (= width 1) lead (logand lead (ash #x7f (- width))))))
               (declare (type (integer 1 4) width)
                        (type (unsigned-byte 21) code))
               (loop for next of-type fixnum from (1+ at) below (+ at width)
                     for byte = (if (< next to) (aref octets next) 0)
                     do (unless (<= #x80 byte #xbf) (funcall malformed))
                        (setf code (logior (ash code 6) (logand byte #x3f))))
               (when (case width
                       (3 (or (< code #x800) (<= #xd800 code #xdfff)))
                       (4 (not (<= #x10000 code #x10ffff))))
                 (funcall malformed))
               (setf (schar out into) (code-char code))
               (incf at width)
               (incf into)))
    into))

(defun decode-json (source &key whole keep revive)
  "SOURCE — JSON as UTF-8 octets, as a string, or as a character stream — as
a Lisp value: objects as EQUAL hash tables, arrays as simple vectors,
strings as simple strings, true T, false NIL, null :NULL."
  ;; WHOLE demands that SOURCE be exactly one value with nothing after it.
  ;; Refuses with JSON-REFUSAL on text that is not JSON, a string that is not
  ;; UTF-8 included.
  ;;
  ;; KEEP, when given, is asked of every object member with its key path,
  ;; innermost key first (an array's elements share their array's path): a
  ;; member it refuses is left out, its value passed over without a string or
  ;; table built for it, and read only as far as finding where it ends. A
  ;; reader that wants a few fields of a large document pays for those:
  ;; Without KEEP, it would still build a table for every member of the whole
  ;; document, each one garbage once it is compacted.
  ;;
  ;; REVIVE, when given, is JSON.parse's reviver: called with the same path
  ;; and each kept member's value as its value closes, innermost first, and
  ;; its answer stands in the value's place. A reader that keeps a document
  ;; as its own records builds each as its object closes, so the tables it
  ;; reads them from die young, one at a time, instead of all living until
  ;; the whole document is read -- long enough to be promoted, and to hold
  ;; the heap at their size until the next full walk.
  ;;
  ;; Every reader in the image comes through here, so what a decoded value
  ;; costs is a fact of this file alone. The reader walks bytes: JSON's
  ;; structure is ASCII, so a document is read in the form it crosses the wire
  ;; and sits on disk in, and only a string's own bytes are ever decoded —
  ;; straight into a base string when they are all ASCII, one byte a character
  ;; where the wide form costs four.
  ;; A stream is read whole because every reader in this image reads one
  ;; value from a stream it opened for that value: a framed protocol hands
  ;; this file the frame, not the socket.
  (let* ((octets (etypecase source
                   (octets source)
                   ((vector (unsigned-byte 8)) (coerce source 'octets))
                   (string (utf-8-octets source))
                   (stream (utf-8-octets (uiop:slurp-stream-string source)))))
         (end (length octets))
         (index 0)
         ;; One value stack and one character buffer for the whole document,
         ;; each grown by doubling. An array's elements and an object's pairs
         ;; land on the stack until their extent is known, which is what lets
         ;; the result be exactly sized; a string with escapes is rebuilt in
         ;; the buffer. Both are this call's own and neither outlives it.
         (stack (make-array 64 :adjustable t :fill-pointer 0))
         (buffer (make-string 64))
         (filled 0))
    (declare (type octets octets)
             (type fixnum index end filled)
             (type (simple-array character (*)) buffer))
    (labels
        ((refuse (control &rest arguments)
           (error 'json-refusal :position index
                                :detail (apply #'format nil control arguments)))
         (peek ()
           (when (< index end) (aref octets index)))
         (skip-space ()
           (loop while (and (< index end)
                            (member (aref octets index)
                                    '#.(mapcar #'char-code '(#\Space #\Tab #\Newline #\Return))))
                 do (incf index)))
         (share (string)
           (if (<= (length string) +json-shared-string-length+)
               (json-shared-string string)
               string))
         (decode-bytes (from to narrow)
           "FROM..TO of OCTETS as a fresh exactly-sized string: a base string
straight from the bytes when NARROW says they are all ASCII, else the
characters they spell."
           (if narrow
               (let ((out (make-string (- to from) :element-type 'base-char)))
                 (loop for i of-type fixnum from from below to
                       for j of-type fixnum from 0
                       do (setf (schar out j) (code-char (aref octets i))))
                 out)
               (progn (buffer-run from to)
                      (subseq buffer 0 filled))))
         (buffer-run (from to)
           "Bytes FROM..TO of OCTETS onto the end of BUFFER, as the characters
they spell, with room left for the escape that follows them."
           (let ((need (+ filled (- to from) 1)))
             (when (> need (length buffer))
               (setf buffer (replace (make-string (* 2 need)) buffer :end2 filled))))
           (setf filled (decode-utf-8 octets from to buffer filled
                                      (lambda () (refuse "a string that is not UTF-8")))))
         (tighten (narrow)
           "The rebuilt string in BUFFER as its own exactly-sized string."
           (share (if narrow
                      (replace (make-string filled :element-type 'base-char) buffer)
                      (subseq buffer 0 filled))))
         (read-hex4 (&aux (code 0))
           (loop repeat 4
                 for value = (and (< index end)
                                  (digit-char-p (code-char (aref octets index)) 16))
                 do (unless value (refuse "an escape wants four hex digits"))
                    (setf code (logior (ash code 4) value))
                    (incf index))
           code)
         (read-code (&aux (code (read-hex4)))
           ;; \\uXXXX, and the pair a character outside the basic plane is
           ;; spelled with. A surrogate stands only in a leading-then-trailing
           ;; pair: a lone one, or a leading one followed by anything else, is
           ;; a refusal rather than a character that was never written.
           (cond
             ((<= #xd800 code #xdbff)
              (unless (and (< (1+ index) end)
                           (= (aref octets index) #.(char-code #\\))
                           (= (aref octets (1+ index)) #.(char-code #\u)))
                (refuse "a leading surrogate without its pair"))
              (incf index 2)
              (let ((tail (read-hex4)))
                (unless (<= #xdc00 tail #xdfff)
                  (refuse "a leading surrogate followed by something that is not its pair"))
                (code-char (+ #x10000 (ash (- code #xd800) 10) (- tail #xdc00)))))
             ((<= #xdc00 code #xdfff)
              (refuse "a trailing surrogate without its leading one"))
             (t (code-char code))))
         (read-string ()
           ;; INDEX is at the opening quote. The common string carries no
           ;; escape, and is one copy straight out of the source bytes. A
           ;; string with escapes is rebuilt in the buffer: each run between
           ;; escapes copied whole, each escape as the character it spells.
           (let ((from (incf index))
                 (narrow t))
             (declare (type fixnum from))
             (setf filled 0)
             (loop
               ;; Past the plain ASCII run to the byte that ends it.
               (let ((at index))
                 (declare (type fixnum at))
                 (loop while (and (< at end)
                                  (let ((byte (aref octets at)))
                                    (and (< #.(1- (char-code #\Space)) byte #x80)
                                         (/= byte #.(char-code #\"))
                                         (/= byte #.(char-code #\\)))))
                       do (incf at))
                 (setf index at))
               (when (>= index end) (refuse "a string never closes"))
               (let ((byte (aref octets index)))
                 (cond
                   ((= byte #.(char-code #\"))
                    (incf index)
                    (return (if (zerop filled)
                                (share (decode-bytes from (1- index) narrow))
                                (progn (buffer-run from (1- index))
                                       (tighten narrow)))))
                   ((= byte #.(char-code #\\))
                    (buffer-run from index)
                    (incf index)
                    (when (>= index end) (refuse "a string ends inside an escape"))
                    (let* ((escape (aref octets index))
                           (short (position escape #.(sb-ext:string-to-octets "\"\\/bfnrt")))
                           (decoded (progn
                                      (incf index)
                                      (cond (short
                                             (code-char (svref #(34 92 47 8 12 10 13 9) short)))
                                            ((= escape #.(char-code #\u)) (read-code))
                                            (t (refuse "~s is not an escape"
                                                       (code-char escape)))))))
                      (unless (typep decoded 'base-char) (setf narrow nil))
                      (setf (schar buffer filled) decoded)
                      (incf filled))
                    (setf from index))
                   ((< byte #.(char-code #\Space))
                    (refuse "a control character inside a string"))
                   (t
                    (setf narrow nil)
                    (incf index)))))))
         (read-digits (accumulator refusal &optional limit &aux (start index))
           "Fold the digits at INDEX into ACCUMULATOR; => the value, and a
REFUSAL when there were none, and a refusal when LIMIT is given and the value
passes it, or past +JSON-MAX-DIGITS+ of them."
           (loop for value = (and (< index end)
                                  (let ((byte (aref octets index)))
                                    (and (<= #.(char-code #\0) byte #.(char-code #\9))
                                         (- byte #.(char-code #\0)))))
                 while value
                 do (when (= (- index start) +json-max-digits+)
                      (refuse "a number with too many digits"))
                    (setf accumulator (+ (* 10 accumulator) value))
                    (when (and limit (> accumulator limit)) (refuse "an exponent out of range"))
                    (incf index))
           (when (= index start) (refuse refusal))
           accumulator)
         (read-number (&aux (negative (when (eql (peek) #.(char-code #\-)) (incf index) t))
                            (leading-zero (and (eql (peek) #.(char-code #\0))
                                               (< (1+ index) end)
                                               (<= #.(char-code #\0) (aref octets (1+ index))
                                                   #.(char-code #\9))))
                            (mantissa (progn (when leading-zero (refuse "a number with a leading zero"))
                                             (read-digits 0 "a number without digits")))
                            (scale 0)
                            (exponent 0)
                            (fractional nil))
           (when (eql (peek) #.(char-code #\.))
             (setf fractional t)
             (incf index)
             (let ((before index))
               (setf mantissa (read-digits mantissa "a fraction without digits")
                     scale (- before index))))
           (when (member (peek) '(#.(char-code #\e) #.(char-code #\E)))
             (setf fractional t)
             (incf index)
             (let ((negative-exponent (case (peek)
                                        (#.(char-code #\+) (incf index) nil)
                                        (#.(char-code #\-) (incf index) t))))
               ;; Stopped at the limit as it is read: ten to an exponent of
               ;; thousands of digits would cost the decoder its time for
               ;; nothing, since no float is that far out.
               (let ((value (read-digits 0 "an exponent without digits" +json-max-exponent+)))
                 (setf exponent (if negative-exponent (- value) value)))))
           (let ((value (if negative (- mantissa) mantissa)))
             (if fractional
                 ;; shasht's own arithmetic, so a float reads the same
                 ;; here as it did there: the digits as an integer, scaled.
                 (handler-case (* value (expt (coerce 10 *read-default-float-format*)
                                              (+ scale exponent)))
                   ;; Past the largest float: a refusal, not a raw arithmetic error.
                   (arithmetic-error () (refuse "a number out of range")))
                 value)))
         (expect-word (word value &aux (stop (+ index (length word))))
           (unless (and (<= stop end)
                        (loop for character across word
                              for i from index
                              always (= (aref octets i) (char-code character))))
             (refuse "~a expected" word))
           (setf index stop)
           value)
         (skip-string ()
           ;; INDEX is at the opening quote; past the closing one.
           (incf index)
           (loop (when (>= index end) (refuse "a string never closes"))
                 (let ((byte (aref octets index)))
                   (incf index)
                   (cond ((= byte #.(char-code #\")) (return))
                         ((= byte #.(char-code #\\)) (incf index))))))
         (skip-value (&aux (nesting 0))
           "Past the value at INDEX, building nothing."
           (declare (type fixnum nesting))
           (skip-space)
           (loop
             (when (>= index end) (refuse "the text ends inside a value"))
             (let ((byte (aref octets index)))
               (cond ((= byte #.(char-code #\")) (skip-string) (when (zerop nesting) (return)))
                     ((member byte '(#.(char-code #\{) #.(char-code #\[))) (incf nesting) (incf index))
                     ((member byte '(#.(char-code #\}) #.(char-code #\])))
                      ;; at nesting 0 it closes the container around the value
                      (when (zerop nesting) (return))
                      (decf nesting)
                      (incf index)
                      (when (zerop nesting) (return)))
                     ((and (zerop nesting)
                           (member byte '#.(mapcar #'char-code '(#\, #\Space #\Tab #\Newline #\Return))))
                      (return))
                     (t (incf index))))))
         (read-container (depth close path &aux (mark (fill-pointer stack))
                                                (object (= close #.(char-code #\}))))
           ;; The array or, when CLOSE is }, the object whose opener INDEX is
           ;; past: its elements, or its keys and values in turn, stacked until
           ;; it closes.
           (skip-space)
           (if (eql (peek) close)
               (incf index)
               (loop
                 (if object
                     (let ((key (progn
                                  (skip-space)
                                  (unless (eql (peek) #.(char-code #\")) (refuse "an object wants a key"))
                                  (read-string))))
                       (skip-space)
                       (unless (eql (peek) #.(char-code #\:)) (refuse "a key wants :"))
                       (incf index)
                       (let ((inner (if (or keep revive) (cons key path) path)))
                         (if (and keep (not (funcall keep inner)))
                             (skip-value)
                             (let ((value (read-value (1+ depth) inner)))
                               (vector-push-extend key stack)
                               (vector-push-extend (if revive (funcall revive inner value) value)
                                                   stack)))))
                     (vector-push-extend (read-value (1+ depth) path) stack))
                 (skip-space)
                 (cond ((eql (peek) #.(char-code #\,)) (incf index))
                       ((eql (peek) close) (incf index) (return))
                       (t (refuse (if object "an object wants , or }" "an array wants , or ]"))))))
           (prog1 (if object
                      (let ((table (make-hash-table
                                    :test #'equal
                                    :size (max 1 (ash (- (fill-pointer stack) mark) -1)))))
                        (loop for pair from mark below (fill-pointer stack) by 2
                              do (setf (gethash (aref stack pair) table)
                                       (aref stack (1+ pair))))
                        table)
                      (subseq stack mark))
             (setf (fill-pointer stack) mark)))
         (read-value (depth path)
           (when (> depth +json-max-depth+)
             (refuse "nested deeper than ~d" +json-max-depth+))
           (skip-space)
           (let ((byte (peek)))
             (case byte
               ((nil) (refuse "the text ends where a value belongs"))
               (#.(char-code #\{) (incf index) (read-container depth #.(char-code #\}) path))
               (#.(char-code #\[) (incf index) (read-container depth #.(char-code #\]) path))
               (#.(char-code #\") (read-string))
               (#.(char-code #\t) (expect-word "true" t))
               (#.(char-code #\f) (expect-word "false" nil))
               (#.(char-code #\n) (expect-word "null" :null))
               (t (if (or (= byte #.(char-code #\-))
                          (<= #.(char-code #\0) byte #.(char-code #\9)))
                      (read-number)
                      (refuse "~s begins no value" (code-char byte))))))))
      (let ((value (read-value 0 '())))
        (when whole
          (skip-space)
          (when (< index end) (refuse "a second value follows the first")))
        value))))

(defun json-value (object type &rest path &aux (value object)
                                               (present t))
  "One typed, NIL-safe read of decoded JSON."
  ;; Returns (values VALUE PRESENT).
  ;;
  ;; OBJECT is any decoded value. PATH is the string keys walked from OBJECT;
  ;; a step through anything that is not a hash table stops the walk, so a
  ;; missing intermediate object reads exactly like a missing leaf and no
  ;; caller needs its own HASH-TABLE-P guard between levels. With no PATH,
  ;; OBJECT itself is the value, which is how a value already in hand gets
  ;; coerced.
  ;;
  ;; TYPE selects the coercion. A value of the wrong type reads as NIL:
  ;;
  ;;   :any      the value, with shasht's :NULL folded to NIL
  ;;   :string   the value when it is a string
  ;;   :text     the value when it is a non-empty string
  ;;   :integer  the value when it is an integer
  ;;   :number   the value when it is a real
  ;;   :boolean  T for JSON true only; false, null, absent and every other
  ;;             decoding read NIL
  ;;   :object   the value when it is a hash table
  ;;   :array    the value when it is a JSON array (a vector that is not a string)
  ;;
  ;; PRESENT is T when the final key existed on its object -- the GETHASH
  ;; second value, carried through the walk -- so a caller can tell an absent
  ;; key from a present one whose value coerced to NIL.
  ;;
  ;; PRESENT does not separate JSON null from JSON false: every coercion here
  ;; answers NIL for both, deliberately. The two routes that must tell them
  ;; apart are strict parsers -- the evict percent and the checkpoint
  ;; restore-files scope, which refuse a wrong-typed field rather than
  ;; defaulting it -- and they read GETHASH's own two values directly.
  ;; Strictness is PROTOCOL.LISP's dialect; this waist stays lenient.
  (dolist (key path)
    (unless (hash-table-p value)
      (setf value nil
            present nil)
      (return))
    (multiple-value-setq (value present) (gethash key value)))
  (values
   (case type
     (:any (if (eq value :null) nil value))
     (:string (and (stringp value) value))
     (:text (and (stringp value) (plusp (length value)) value))
     (:integer (and (integerp value) value))
     (:number (and (realp value) value))
     (:boolean (eq value t))
     (:object (and (hash-table-p value) value))
     (:array (and (vectorp value) (not (stringp value)) value))
     (otherwise (error "JSON-VALUE: unknown type ~s (want one of ~{~s~^ ~})"
                       type +json-types+)))
   present))

(defun json-array (object &rest path)
  "The array at PATH in OBJECT, or an empty vector, so a LOOP ACROSS needs no guard."
  ;; (LOOP FOR x ACROSS (JSON-VALUE o :array "k")) walks NIL and signals, so
  ;; every caller wrapped the read in (OR ... #()) and half of them wrapped
  ;; onto a second line to do it.
  (or (apply #'json-value object :array path) #()))

(defun wire-symbol (key &optional (package :keyword))
  "A snake_case wire KEY as the symbol it names in PACKAGE: max_results is MAX-RESULTS."
  (intern (substitute #\- #\_ (string-upcase key)) package))

(defun json-members (object type &rest keys)
  "OBJECT's members KEYS, each read as TYPE (JSON-VALUE), as a plist under each
KEY's kebab-case keyword: \"media_type\" is :MEDIA-TYPE. JSON-OBJECT's :GETF
read back."
  (loop for key in keys
        collect (wire-symbol key)
        collect (json-value object type key)))

(defmacro with-json (specs object &body body &aux (source (gensym "JSON")))
  "LET* over decoded JSON OBJECT, with wire reads written declaratively."
  ;; A SPEC is either a wire read or an ordinary binding, and the two
  ;; interleave freely -- which is the shape the call sites actually have,
  ;; reads and the values derived from them alternating in one sequential
  ;; chain:
  ;;
  ;;   wire read         (VAR TYPE KEY... [:DEFAULT FORM])
  ;;   ordinary binding  VAR | (VAR) | (VAR FORM)
  ;;
  ;; TYPE and the keys are JSON-VALUE's arguments; :DEFAULT supplies the value
  ;; used when the read is NIL. TYPE must be one of the eight +JSON-TYPES+
  ;; keywords and must be followed by at least one string-literal key, so an
  ;; ordinary binding is never mistaken for a read.
  ;;
  ;;   (with-json ((call-id      :string  "tool_call_id")
  ;;               (command      :string  "command" :default "")
  ;;               (backgrounded :boolean "backgrounded")
  ;;               (status       :string  "background_shell" "status")
  ;;               (live-p       (and status (string= status "running"))))
  ;;       process
  ;;     ...)
  ;;
  ;; OBJECT is evaluated once, before any binding. Bindings are sequential, so
  ;; a later spec may name an earlier VAR. Presence is not bindable here --
  ;; the two strict parsers that must tell JSON null from JSON false read
  ;; GETHASH's own two values directly, as JSON-VALUE's docstring records.
  (flet ((binding (spec)
           (cond
             ;; A wire read always has a key, so (VAR :STRING) is an ordinary binding.
             ((and (consp spec) (>= (length spec) 3) (member (second spec) +json-types+))
              (destructuring-bind (var type &rest tail) spec
                (unless (and var (symbolp var) (not (keywordp var)))
                  (error "WITH-JSON: ~s is not a variable name in ~s" var spec))
                (let* ((mark (position :default tail))
                       (path (if mark (subseq tail 0 mark) tail))
                       (default (when mark
                                  (unless (= (length tail) (+ mark 2))
                                    (error "WITH-JSON: :DEFAULT takes exactly one form in ~s"
                                           spec))
                                  (nth (1+ mark) tail))))
                  ;; Literal keys only. A dynamic key is a lookup, not a wire field,
                  ;; and belongs in a direct JSON-VALUE call; keeping the macro literal
                  ;; means every wire key a file reads is greppable as a string inside
                  ;; a WITH-JSON form.
                  (unless (and path (every #'stringp path))
                    (error "WITH-JSON: ~s needs at least one string-literal key" spec))
                  ;; :DEFAULT substitutes on a NIL read, and JSON false reads NIL, so
                  ;; a defaulted :BOOLEAN could never answer false. Refuse it rather
                  ;; than expand into a form that cannot mean what it says.
                  (when (and mark (eq type :boolean))
                    (error "WITH-JSON: :DEFAULT is meaningless on :BOOLEAN in ~s" spec))
                  (let ((read `(json-value ,source ,type ,@path)))
                    (list var (if mark `(or ,read ,default) read))))))
             ;; A wire type with no key can only be a mistake: (VAR :INTEGER) reads
             ;; nothing, and binding the bare keyword is never what the author meant.
             ((and (consp spec) (= (length spec) 2) (member (second spec) +json-types+))
              (error "WITH-JSON: ~s names a wire type but no key" spec))
             ((and (symbolp spec) spec (not (keywordp spec))) (list spec nil))
             ((and (consp spec) (<= (length spec) 2)
                   (symbolp (first spec)) (first spec) (not (keywordp (first spec))))
              spec)
             (t (error "WITH-JSON: ~s is neither a wire read (VAR TYPE KEY...) nor an ~
ordinary binding (VAR FORM)" spec)))))
    `(let ((,source ,object))
       (declare (ignorable ,source))
       (let* ,(mapcar #'binding specs)
         ,@body))))

;;; Precedence rather than preference because an endpoint may send two of them
;;; holding the same text, and appending both would double the trace.
(defparameter +reasoning-wire-fields+
  '("reasoning_content" "reasoning" "reasoning_text")
  "The keys an OpenAI-compatible endpoint may carry a thinking trace under,
in precedence order: llama.cpp streams `reasoning_content`, vLLM streams
`reasoning`, and some endpoints `reasoning_text`.")

(defun wire-reasoning (object &aux (first-field nil))
  "The thinking trace OBJECT carries, under whichever key the endpoint used."
  ;; Returns (values TEXT PRESENT FIELD). TEXT is the first NON-EMPTY string
  ;; found under +REASONING-WIRE-FIELDS+, so an endpoint that spells the same
  ;; trace twice contributes it once. PRESENT is true when any of those keys
  ;; held a string AT ALL, empty included -- the chat lane replays the key's
  ;; presence rather than its text, so an empty reasoning chunk still has to
  ;; register. FIELD names the key TEXT came from.
  ;;
  ;; (values NIL NIL NIL) when OBJECT carries no thinking at all.
  (dolist (field +reasoning-wire-fields+)
    (when-let (value (json-value object :string field))
      (when (plusp (length value))
        (return-from wire-reasoning (values value t field)))
      (setf first-field (or first-field field))))
  (values (and first-field "") (and first-field t) first-field))

;;; --- the JSONC dialect ------------------------------------------------------
;;; Three primitives below hold this dialect's entire string/escape/comment
;;; state machine, and everything else in this file -- NORMALIZE-JSONC, and the
;;; edit side after it -- is a driver of them. That is the invariant the old
;;; config writer broke: it grew a second quote scanner over in provider.lisp,
;;; stayed comment-blind, and found an object's close brace with a bare
;;; POSITION. A splice that guesses wrong lands inside an operator's api_key.

(define-refusal jsonc-refusal jsonc-refuse "malformed JSONC document: ~a"
  "A JSONC document is malformed in a way no reader or writer may guess at." :export t)

(defun jsonc-space-p (character)
  "The whitespace set of this dialect, #\\Page included (config.zig
normalizeJsonc)."
  ;; Note shasht itself does not accept #\Page, so it survives normalization
  ;; only as the gap between tokens it already was.
  (and (member character '(#\Space #\Tab #\Newline #\Return #\Page)) t))

(defun jsonc-string-end (source index &optional (limit (length source)))
  "INDEX names a string's opening #\\\"; => the index just past its closer."
  ;; A backslash escapes the next character, so an escaped quote and a value
  ;; ending in a literal backslash both land where the reader lands. An
  ;; unterminated string is a refusal: the %JSONC-QUOTED-END this replaces
  ;; answered NIL and let its caller splice on regardless.
  (loop with i = (1+ index)
        while (< i limit)
        do (let ((character (char source i)))
             (cond ((char= character #\\) (incf i 2))
                   ((char= character #\") (return-from jsonc-string-end (1+ i)))
                   (t (incf i))))
        finally (jsonc-refuse "unterminated string at ~a" index)))

(defun jsonc-comment-end (source index &optional (limit (length source)))
  "The index just past the comment opening at INDEX, or NIL when INDEX opens
none."
  ;; A // comment ends BEFORE its newline, so the caller routes that newline
  ;; as the whitespace it is. An unterminated block comment is a refusal, the
  ;; rule the normalizer already enforced.
  (when (and (< (1+ index) limit) (char= (char source index) #\/))
    (case (char source (1+ index))
      (#\/ (or (position #\Newline source :start (+ index 2) :end limit) limit))
      (#\* (+ 2 (or (search "*/" source :start2 (+ index 2) :end2 limit)
                    (jsonc-refuse "unterminated block comment at ~a" index)))))))

(defun normalize-jsonc (source)
  "SOURCE as strict JSON: // and /* */ comments stripped and trailing commas
dropped, both string-aware (config.zig normalizeJsonc)."
  ;; One scanner for every JSONC document this image reads -- the operator
  ;; config and the themes documents -- because a second copy is a second
  ;; string/escape state machine to keep in step. Those rules live in
  ;; JSONC-STRING-END and JSONC-COMMENT-END above; what stays here is the
  ;; transduction only, which is why this function has no in-string state of
  ;; its own and copies each string as one span.
  ;;
  ;; The comma is decided against the normalized text rather than against
  ;; SOURCE: a comma is held back until the next significant character
  ;; arrives, so a comment sitting between a trailing comma and its closer
  ;; still lets the comma go. #\Page counts as whitespace while a comma is
  ;; held.
  (with-output-to-string (out)
    (let ((i 0)
          (n (length source))
          (comma-pending nil)
          (gap (make-string-output-stream)))
      (flet ((flush (closed)
               ;; A held comma rides out unless the container closed on it;
               ;; the whitespace it was holding always does.
               (when comma-pending
                 (unless closed (write-char #\, out))
                 (write-string (get-output-stream-string gap) out)
                 (setf comma-pending nil))))
        (loop while (< i n)
              for character = (char source i)
              for comment = (jsonc-comment-end source i n)
              do (let ((next (1+ i)))
                   (cond
                     (comment (setf next comment))
                     ((jsonc-space-p character)
                      (write-char character (if comma-pending gap out)))
                     ((char= character #\,)
                      (flush nil)
                      (setf comma-pending t))
                     (t
                      (flush (or (char= character #\}) (char= character #\])))
                      (when (char= character #\") (setf next (jsonc-string-end source i n)))
                      (write-string source out :start i :end next)))
                   (setf i next)))
        ;; Nothing significant followed: the comma is data, not punctuation,
        ;; and the parser downstream is the one that gets to refuse it.
        (flush nil)))))

;;; --- the JSONC edit side ----------------------------------------------------
;;; Setting one member of an operator-owned document, every other byte
;;; unchanged. Decode-mutate-re-encode is what the Zig sibling does
;;; (config.zig saveConfigMutationAtPath) and it cannot be copied here on two
;;; counts. It normalizes first, so it deletes every comment in the operator's
;;; file; and shasht decodes objects into hash tables, so a round trip would
;;; also scramble their key order -- while its ordered :alist format collapses
;;; {} and false both onto NIL, which would rewrite an empty provider block as
;;; `false`. So: span arithmetic and a splice. What a string or a comment IS
;;; stays the property of the three primitives above; nothing here re-decides
;;; it.

;;; Hardcoded rather than inferred from the document: an object that has a
;;; member copies that member's indent, so this only ever fires on an object
;;; with none, and guessing a step has a corruption-shaped downside where
;;; being conventional does not.
(defparameter +jsonc-indent-step+ "  "
  "One nesting step for synthesized JSONC.")

(defun jsonc-trivia-end (source index &optional (limit (length source)))
  "The first index in [INDEX,LIMIT) holding a significant character; whitespace
and both comment forms are stepped over, LIMIT when none remains."
  ;; Holds no state of its own: it is a loop over JSONC-SPACE-P and
  ;; JSONC-COMMENT-END.
  (loop with i = index
        while (< i limit)
        do (setf i (or (jsonc-comment-end source i limit)
                       (if (jsonc-space-p (char source i)) (1+ i) (return i))))
        finally (return limit)))

(defun jsonc-value-end (source index &optional (limit (length source)))
  "The index just past the complete value whose first significant character is
at INDEX."
  ;; Containers close by depth with strings and comments stepped over whole,
  ;; which is why GENERAL's close brace is GENERAL's own and not the one
  ;; closing a nested CONTEXT_WINDOW_POLICY -- the defect a bare POSITION
  ;; search carried.
  (let ((character (char source index)))
    (cond
      ((char= character #\") (jsonc-string-end source index limit))
      ((or (char= character #\{) (char= character #\[))
       (loop with depth = 0
             with i = index
             while (< i limit)
             do (let ((inner (char source i)))
                  ;; A comment or a string is stepped over whole, so a brace
                  ;; inside one is never counted.
                  (setf i (or (jsonc-comment-end source i limit)
                              (and (char= inner #\") (jsonc-string-end source i limit))
                              (1+ i)))
                  (case inner
                    ((#\{ #\[) (incf depth))
                    ((#\} #\]) (when (zerop (decf depth))
                                 (return-from jsonc-value-end i)))))
             finally (jsonc-refuse "unbalanced container opened at ~a" index)))
      (t
       ;; A number, true, false or null: it ends where the next punctuation,
       ;; whitespace or comment begins.
       (loop with i = index
             while (and (< i limit)
                        (not (find (char source i) ",}]"))
                        (not (jsonc-space-p (char source i)))
                        (not (jsonc-comment-end source i limit)))
             do (incf i)
             finally (return i))))))

(defun jsonc-members (source open close)
  "A (KEY-START KEY-END VALUE-START VALUE-END) list for each DIRECT member of
the object whose #\\{ is at OPEN and whose matching #\\} is at CLOSE, in
document order; nested containers are stepped over, never entered."
  ;; The last member's VALUE-END is the index just past its value; an object
  ;; with no members answers NIL.
  ;;
  ;; The one place in this image that knows what a JSON member looks like:
  ;; JSONC-MEMBER-BOUNDS, JSONC-MEMBER-INDENT, and the insertion and removal
  ;; in JSONC-MERGE-EDITS and JSONC-PATH-EDITS are all readers of it.
  (loop with i = (jsonc-trivia-end source (1+ open) close)
        while (and (< i close) (char= (char source i) #\"))
        collect (let* ((key-start i)
                       (key-end (jsonc-string-end source i close))
                       (colon (jsonc-trivia-end source key-end close)))
                  (unless (and (< colon close) (char= (char source colon) #\:))
                    (jsonc-refuse "member at ~a carries no colon" key-start))
                  (let* ((value-start (jsonc-trivia-end source (1+ colon) close))
                         (value-end (jsonc-value-end source value-start close)))
                    (setf i (jsonc-trivia-end source value-end close))
                    (list key-start key-end value-start value-end)))
        while (and (< i close) (char= (char source i) #\,))
        do (setf i (jsonc-trivia-end source (1+ i) close))))

(defun jsonc-member-bounds (source key open close)
  "=> (values VALUE-START VALUE-END KEY-START) for KEY's direct member of the
object at [OPEN,CLOSE], or NIL."
  ;; KEY is matched against the raw text between the quotes: this writer never
  ;; emits an escaped key and the config schema carries none, so decoding one
  ;; here would buy nothing.
  (when-let (found (find-if (lambda (member)
                              (string= key source :start2 (1+ (first member))
                                                  :end2 (1- (second member))))
                            (jsonc-members source open close)))
    (values (third found) (fourth found) (first found))))

(defun jsonc-string-leaves (source &aux (leaves '()))
  "Every string value in the JSONC document SOURCE as (PATH START END), in
document order: PATH the member keys and array indices that reach it from
the root, START and END its literal's bounds, quotes included."
  ;; A reader of the members like the rest, and the one that also walks an
  ;; array's elements: the raw config editor finds each secret's literal by
  ;; it, to read it out and to put it back byte for byte.
  (labels ((walk (start path &aux (end (jsonc-value-end source start)))
             (case (char source start)
               (#\" (push (list (reverse path) start end) leaves))
               (#\{ (loop for (key-start key-end value-start) in (jsonc-members source start (1- end))
                          do (walk value-start
                                   (cons (subseq source (1+ key-start) (1- key-end)) path))))
               ;; An element, then past the comma after it to the next one.
               (#\[ (loop for index from 0
                          for at = (jsonc-trivia-end source (1+ start) (1- end))
                            then (jsonc-trivia-end
                                  source (1+ (jsonc-trivia-end source (jsonc-value-end source at)
                                                               (1- end)))
                                  (1- end))
                          while (< at (1- end))
                          do (walk at (cons index path)))))))
    (let ((root (jsonc-trivia-end source 0)))
      (when (< root (length source))
        (walk root '()))))
  (nreverse leaves))

(defun jsonc-line-indent (source index)
  "The whitespace opening the line INDEX sits on."
  (let* ((newline (position #\Newline source :end index :from-end t))
         (start (if newline (1+ newline) 0))
         (text (position-if-not (lambda (character) (member character '(#\Space #\Tab)))
                                source :start start :end index)))
    (subseq source start (or text index))))

(defun jsonc-member-indent (source open close)
  "The indent a new direct member of the object at [OPEN,CLOSE] should carry:
the last member's own line indent, or -- for an object with no members -- the
object's line indent plus one step."
  ;; NIL when the object keeps its members on one line, the inline style,
  ;; which inserts inline.
  (if-let (last-key (first (car (last (jsonc-members source open close)))))
    (and (position #\Newline source :start open :end last-key)
         (jsonc-line-indent source last-key))
    ;; No member to copy from, so the question is only whether this
    ;; document writes objects across lines at all: a newline inside the
    ;; braces answers it for a root written as {\n}, and one before them
    ;; answers it for a nested {} sitting on its own line.
    (and (or (position #\Newline source :start open :end close)
             (position #\Newline source :end open))
         (concatenate 'string (jsonc-line-indent source open)
                      +jsonc-indent-step+))))

(defun json-string-literal (text)
  "TEXT as a JSON string literal, through the one shasht write seam."
  (with-output-to-string (out) (shasht:write-json-string text out)))

;;; --- the value model --------------------------------------------------------
;;; What a caller may write is a Lisp value in the eval snippet's convention --
;;; the one NCK:CALL answers with and the mcp cell's arguments use -- read
;;; once into a small tree so the renderer and the comparison below never
;;; re-decide what T or a keyword plist means: (:OBJECT . ((KEY . TREE) ...))
;;; keeps the caller's member order, (:ARRAY . (TREE ...)), and a scalar is a
;;; string, an integer, a float, T, :FALSE or :NULL.

(defun jsonc-object-key (key)
  "KEY as the string an object member carries: a string as it is, a symbol
downcased -- the keyword-plist convention (:bot_token_env is bot_token_env)."
  (cond ((stringp key) key)
        ((symbolp key) (string-downcase (symbol-name key)))
        (t (jsonc-refuse "~s is not an object key" key))))

(defun jsonc-writable (value)
  "VALUE as the tree the writer renders, or a refusal naming what it cannot
write."
  ;; NIL is refused rather than guessed at: it reads as false, null and the
  ;; empty list at once, and each of those has its own spelling -- :FALSE,
  ;; :NULL, #() -- so a caller says which.
  (cond
    ((or (stringp value) (integerp value) (floatp value) (member value '(t :false :null)))
     value)
    ((null value)
     (jsonc-refuse "NIL is ambiguous: write :false, :null, or #() for an empty array"))
    ((hash-table-p value)
     (cons :object (loop for key being the hash-keys of value using (hash-value inner)
                         collect (cons (jsonc-object-key key) (jsonc-writable inner)))))
    ((and (consp value) (keywordp (car value)))
     (cons :object
           (loop for (key inner) on value by #'cddr
                 collect (cons (jsonc-object-key key) (jsonc-writable inner)))))
    ((consp value) (cons :array (mapcar #'jsonc-writable value)))
    ((vectorp value) (cons :array (map 'list #'jsonc-writable value)))
    (t (jsonc-refuse "~s is not a JSON value" value))))

(defun jsonc-tree-kind (tree)
  "One of :OBJECT, :ARRAY, or :SCALAR for a writable tree."
  (or (and (consp tree) (find (car tree) '(:object :array))) :scalar))

(defun jsonc-tree-equal-p (tree decoded)
  "Whether DECODED -- shasht's reading of a member's current text -- already
says TREE, so an idempotent write can leave the member alone."
  ;; shasht reads false as NIL, null as :NULL, objects as hash tables and
  ;; arrays as vectors.
  (case (jsonc-tree-kind tree)
    (:object
     (and (hash-table-p decoded)
          (= (hash-table-count decoded) (length (cdr tree)))
          (loop for (key . inner) in (cdr tree)
                always (multiple-value-bind (value present) (gethash key decoded)
                         (and present (jsonc-tree-equal-p inner value))))))
    (:array
     (and (vectorp decoded) (not (stringp decoded))
          (= (length decoded) (length (cdr tree)))
          (every #'jsonc-tree-equal-p (cdr tree) (coerce decoded 'list))))
    (t
     (cond ((stringp tree) (and (stringp decoded) (string= tree decoded)))
           ((numberp tree) (and (numberp decoded) (= tree decoded)))
           (t (eq decoded (cdr (assoc tree '((t . t) (:false) (:null . :null))))))))))

;;; --- rendering ----------------------------------------------------------------

(defun render-jsonc-value (tree indent)
  "TREE as JSONC text."
  ;; An object renders one member per line one step in from INDENT with its
  ;; closing brace at INDENT, or on one line when INDENT is NIL; an array
  ;; always renders on one line.
  (case (jsonc-tree-kind tree)
    (:object
     (let ((members (cdr tree)))
       (cond
         ((null members) "{}")
         (indent
          (let ((inner (concatenate 'string indent +jsonc-indent-step+)))
            (format nil "{~{~%~a~a~^,~}~%~a}"
                    (loop for (key . tree) in members
                          collect inner collect (render-jsonc-member key tree inner))
                    indent)))
         (t (format nil "{ ~{~a~^, ~} }"
                    (loop for (key . tree) in members
                          collect (render-jsonc-member key tree nil)))))))
    (:array (format nil "[~{~a~^, ~}]"
                    (loop for inner in (cdr tree) collect (render-jsonc-value inner nil))))
    (t (cond ((stringp tree) (json-string-literal tree))
             ((cdr (assoc tree '((t . "true") (:false . "false") (:null . "null")))))
             (t (encode-json-object tree))))))

(defun render-jsonc-member (key tree indent)
  "One `\"key\": value' member; INDENT is the line the member sits on, so a
nested object's own members sit one step further in."
  (concatenate 'string (json-string-literal key) ": " (render-jsonc-value tree indent)))

;;; --- the path write ------------------------------------------------------------

(defun jsonc-tree-leaves (tree path)
  "The dotted paths a write of TREE at PATH adds: every scalar or array leaf
under an object, or PATH itself."
  (if (eq (jsonc-tree-kind tree) :object)
      (loop for (key . inner) in (cdr tree)
            append (jsonc-tree-leaves inner (append path (list key))))
      (list (section-key path))))

(defun jsonc-nest (path tree)
  "TREE wrapped in one object per key of PATH: (\"a\" \"b\") nests it as
{\"a\": {\"b\": TREE}}."
  (reduce (lambda (key inner) (list :object (cons key inner)))
          path :from-end t :initial-value tree))

(defun jsonc-member-line-start (source index)
  "The index a removal of the member at INDEX should start from: the newline
that opens its line, when nothing but whitespace shares that line, so the
member's own line goes out with it. INDEX itself in the inline style."
  (let ((before (position-if-not (lambda (character) (member character '(#\Space #\Tab)))
                                 source :end index :from-end t)))
    (if (and before (char= (char source before) #\Newline)) before index)))

(defun jsonc-merge-edits (source open close members path)
  "=> (values EDITS ADDED REPLACED) making the object at [OPEN,CLOSE] say every
(KEY . TREE) of MEMBERS: a member already saying its tree is left alone, an
object member merges into an object value, anything else is replaced whole,
and the absent ones are inserted together in the object's own style."
  ;; Never removes a member -- a write is additive and replacing only.
  (let ((edits '()) (added '()) (replaced '()) (missing '()))
    (dolist (member members)
      (destructuring-bind (key . tree) member
        (bind ((leaf (append path (list key)))
               ((value-start value-end) (jsonc-member-bounds source key open close)))
          (cond
            ((null value-start) (push member missing))
            ;; The value already says TREE, read through the config reader's own
            ;; normalizer; a value it cannot decode is not equal, and is replaced.
            ((ignore-errors
              (jsonc-tree-equal-p
               tree (decode-json (normalize-jsonc (subseq source value-start value-end))))))
            ((and (eq (jsonc-tree-kind tree) :object)
                  (char= (char source value-start) #\{))
             (multiple-value-bind (inner-edits inner-added inner-replaced)
                 (jsonc-merge-edits source value-start (1- value-end)
                                    (cdr tree) leaf)
               (setf edits (append inner-edits edits)
                     added (append inner-added added)
                     replaced (append inner-replaced replaced))))
            (t
             ;; An object replacing a leaf follows the enclosing object's
             ;; style: one member per line at this line's indent, or
             ;; inline when the document keeps members on one line.
             (push (list value-start value-end
                         (render-jsonc-value
                          tree (and (eq (jsonc-tree-kind tree) :object)
                                    (jsonc-member-indent source open close)
                                    (jsonc-line-indent source value-start))))
                   edits)
             (push (section-key leaf) replaced))))))
    (when missing
      (let* ((missing (nreverse missing))
             (indent (jsonc-member-indent source open close))
             (rendered (loop for (key . tree) in missing
                             collect (render-jsonc-member key tree indent)))
             ;; Anchored at the last member's value end, a prefix separator: a trailing comma stays.
             (last (fourth (car (last (jsonc-members source open close))))))
        (push (if last
                  (list last last
                        (format nil "~{~a~a~}"
                                (loop for member in rendered
                                      collect (if indent (format nil ",~%~a" indent) ", ")
                                      collect member)))
                  ;; No members: everything between the braces is trivia. Replace from
                  ;; past the last comment in it, so a comment written between them
                  ;; survives and the whitespace around it does not double up.
                  (list (loop with tail = (1+ open)
                              for i = tail then (or comment (1+ i))
                              for comment = (jsonc-comment-end source i close)
                              while (< i close)
                              when comment do (setf tail comment)
                              finally (return tail))
                        close
                        (if indent
                            (format nil "~{~%~a~a~^,~}~%~a"
                                    (loop for member in rendered collect indent collect member)
                                    (jsonc-line-indent source open))
                            (format nil " ~{~a~^, ~} " rendered))))
              edits)
        (loop for (key . tree) in missing
              do (setf added (revappend (jsonc-tree-leaves tree (append path (list key)))
                                        added)))))
    (values edits (nreverse added) (nreverse replaced))))

(defun set-jsonc-path (source path value)
  "SOURCE with VALUE written at PATH -- a list of member keys from the root --
and every other byte unchanged."
  ;;
  ;; => (values NEW-TEXT ADDED REPLACED REMOVED): the document, the dotted paths
  ;; of the members the write added, of those it replaced, and of the one it
  ;; removed. All three empty means SOURCE already said all of it, and NEW-TEXT is
  ;; then SOURCE itself, so an idempotent write can leave the file -- and the
  ;; reader's memo over its mtime -- alone.
  ;;
  ;; VALUE is a Lisp value in the eval snippet's convention: a string, a number,
  ;; T, :FALSE, :NULL, a keyword plist or hash table for an object, a list or
  ;; vector for an array. An object value MERGES into the object at PATH --
  ;; its members are added or replaced one by one, nested objects merging in
  ;; turn, and no member is ever removed; anything else replaces the member at
  ;; PATH whole. Objects missing along PATH are created in the document's own
  ;; indent and comma style; an empty or comment-only SOURCE becomes a minimal
  ;; document.
  ;;
  ;; :REMOVE is the one value that is not a value: it takes the member at PATH
  ;; out of the document, separator and all, and is the only way a member ever
  ;; leaves one. Removing what is not there is what the document already says,
  ;; so it edits nothing -- and it creates none of the objects along PATH on
  ;; the way.
  ;;
  ;; Signals JSONC-REFUSAL rather than guessing at a document it does not
  ;; understand: a root that is not an object, a member along PATH whose value
  ;; is not an object, an unterminated string or block comment, an unbalanced
  ;; container, a NIL value.
  (let* ((tree (if (eq value :remove) :remove (jsonc-writable value)))
         (path (cond ((and (consp path) (every #'stringp path)) path)
                     ((stringp path) (list path))
                     (t (jsonc-refuse "a path is a list of member keys, not ~s" path))))
         (n (length source))
         (root (jsonc-trivia-end source 0 n)))
    (cond
      ((and (= root n) (eq tree :remove))
       (values source '() '() '()))
      ((= root n)
       (values (format nil "{~%~a~a~%}~%"
                       +jsonc-indent-step+
                       (render-jsonc-member (first path)
                                            (jsonc-nest (rest path) tree)
                                            +jsonc-indent-step+))
               (jsonc-tree-leaves tree path)
               '() '()))
      ((char/= (char source root) #\{)
       (jsonc-refuse "root is ~c, not an object" (char source root)))
      (t
       (multiple-value-bind (edits added replaced removed)
           (jsonc-path-edits source root (1- (jsonc-value-end source root n)) path tree '())
         (values (if edits
                     ;; Spliced ascending; an overlap (a splice mid-token) is refused.
                     (with-output-to-string (out)
                       (loop with cursor = 0
                             for (start end text) in (sort edits #'< :key #'first)
                             do (unless (<= cursor start end (length source))
                                  (jsonc-refuse "overlapping edit ~a at cursor ~a"
                                                (list start end text) cursor))
                                (write-string source out :start cursor :end start)
                                (write-string text out)
                                (setf cursor end)
                             finally (write-string source out :start cursor)))
                     source)
                 added replaced removed))))))

(defun jsonc-path-edits (source open close path tree prefix)
  "=> (values EDITS ADDED REPLACED REMOVED) writing TREE at PATH inside the
object at [OPEN,CLOSE]; PREFIX is the dotted path already descended."
  ;; TREE :REMOVE takes the member at PATH out instead, and creates nothing on
  ;; the way.
  (destructuring-bind (key . rest) path
    (bind ((here (append prefix (list key)))
           ((value-start value-end key-start) (jsonc-member-bounds source key open close)))
      (cond
        ((and value-start rest)
         ;; An object stands where the path still descends: recur into it.
         (unless (char= (char source value-start) #\{)
           (jsonc-refuse "~a is not an object" (section-key here)))
         (jsonc-path-edits source value-start (1- value-end) rest tree here))
        ((eq tree :remove)
         ;; A member absent from here down is a removal the document has
         ;; already made: nothing is created on the way to taking it out.
         (if value-start
             ;; By its line when alone on one; a last member takes the comma before it.
             (let* ((members (jsonc-members source open close)) ; document order
                    (position (position key-start members :key #'first))
                    (next (nth (1+ position) members))
                    (previous (and (plusp position) (nth (1- position) members)))
                    (start (jsonc-member-line-start source key-start))
                    (end (cond (next (jsonc-member-line-start source (first next)))
                               ((and (/= start key-start)
                                     (position #\Newline source :start value-end :end close)))
                               (t value-end)))
                    (comma (and (null next) previous
                                (let ((at (jsonc-trivia-end source (fourth previous) close)))
                                  (and (< at close) (char= (char source at) #\,) at)))))
               (values (append (and comma (list (list comma (1+ comma) "")))
                               (list (list start end "")))
                       '() '() (list (section-key here))))
             (values '() '() '() '())))
        (t
         ;; The leaf, or a member absent from here down: either way one
         ;; merge of KEY against this object, which inserts the whole
         ;; remaining nest when the member is absent -- the same insertion
         ;; the leaf case needs, so there is one copy of it, not two. Its
         ;; three values leave REMOVED to read NIL.
         (jsonc-merge-edits source open close
                            (list (cons key (jsonc-nest rest tree)))
                            prefix))))))

;;; --- the encode side --------------------------------------------------------

(defun make-json-object (&rest pairs)
  "Build one JSON object (an EQUAL hash table) from alternating key/value
PAIRS."
  ;; This is the runtime constructor, for keys or pair lists that only exist
  ;; at run time (it is APPLYable); a wire shape with known keys is written
  ;; with JSON-OBJECT instead, which keeps every key greppable and admits
  ;; conditional fields.
  (apply #'copy-json-object nil pairs))

(defun copy-json-object (object &rest pairs &aux (copy (make-hash-table :test #'equal)))
  "A fresh JSON object with OBJECT's members in their order (none for a NIL
OBJECT), then alternating key/value PAIRS set over them: the copy a writer
takes before it re-keys or adds to an object it does not own."
  (when object
    (maphash (lambda (key value) (setf (gethash key copy) value)) object))
  (loop for (key value) on pairs by #'cddr
        do (setf (gethash key copy) value))
  copy)

(defun pretty-json (value)
  "VALUE as indented JSON text; NIL reads as null."
  (if (null value) "null" (shasht:write-json* value :stream nil :pretty t)))

(defun encode-json-object (object &aux (*print-pretty* nil))
  "OBJECT as a JSON string, via the one shasht write seam."
  ;; The dialect is fixed here, not read off the printer: compact, whatever
  ;; *PRINT-PRETTY* says (T in a stock image, and shasht indents by nesting
  ;; depth under it), so a body's bytes are the same from every caller and a
  ;; value encoded on its own is the bytes it is inside any body
  ;; (JSON-VERBATIM).
  (with-output-to-string (out)
    (shasht:write-json object out)))

;;; --- a value already encoded ----------------------------------------------
;;; The body that leaves the process on a round of a turn is mostly the body
;;; that left it the round before: the same history messages, encoded again.
;;; JSON-VERBATIM is one value the encoder does not encode — its UTF-8
;;; octets, laid into the body as they are — so a caller that kept a value's
;;; bytes hands them back in the value's place. Under ENCODE-JSON-OCTETS the
;;; bytes cross without a decode, and a long run of them is not even copied
;;; until the body is whole. Anywhere else (a test printing through shasht) a
;;; verbatim reads as the text its octets spell.

(defstruct (json-verbatim (:constructor json-verbatim (octets)) (:copier nil))
  "One JSON value already encoded: OCTETS, the UTF-8 bytes of its text,
written into a body as they are."
  ;; The value they stood for is not kept — what stands in the body is the
  ;; bytes, and a body assembled from verbatims encodes to the bytes the same
  ;; body encodes to whole.
  (octets (error "octets required") :type octets :read-only t))

(defmethod shasht:print-json-value ((value json-verbatim) stream)
  (write-string (sb-ext:octets-to-string (json-verbatim-octets value) :external-format :utf-8)
                stream)
  value)

;;; --- the octet writer -------------------------------------------------------
;;; ENCODE-JSON-OCTETS writes a body's UTF-8 straight into one buffer. Without
;;; this, the body would go through shasht as a character-at-a-time string
;;; stream, make a generic call per value and fresh delimiter strings per
;;; object, and then be encoded again. The bytes are shasht's, to the byte: the
;;; writer owns what bodies are built of — objects, arrays, strings, fixnums,
;;; T, NIL, :NULL, verbatims — and writes any other value (a float, a bignum,
;;; a keyword) as shasht does.

(defstruct (json-sink (:constructor make-json-sink ()) (:copier nil) (:predicate nil))
  "One ENCODE-JSON-OCTETS in progress."
  (octets (make-array 1024 :element-type '(unsigned-byte 8)) :type octets)
  (fill 0 :type fixnum)
  ;; (AT . OCTETS), newest first: a verbatim of 512 bytes or more, laid in at
  ;; AT when the body is assembled rather than copied through the buffer.
  (splices '() :type list))

(declaim (inline sink-room sink-byte))
(defun sink-room (sink count)
  "SINK's octets, with room for COUNT more past its fill."
  (let ((octets (json-sink-octets sink)) (need (+ (json-sink-fill sink) count)))
    (if (<= need (length octets))
        octets
        (setf (json-sink-octets sink)
              (replace (make-array (max need (* 2 (length octets)))
                                   :element-type '(unsigned-byte 8))
                       octets :end2 (json-sink-fill sink))))))

(defun sink-byte (sink byte)
  (setf (aref (sink-room sink 1) (json-sink-fill sink)) byte)
  (incf (json-sink-fill sink)))

(defun sink-octets (sink octets)
  (declare (type octets octets))
  (replace (sink-room sink (length octets)) octets :start1 (json-sink-fill sink))
  (incf (json-sink-fill sink) (length octets)))

(defun sink-char (sink code &aux (short (position code #(8 9 10 12 13 34 92))))
  "Character CODE of a JSON string, escaped as shasht escapes it."
  (cond (short (sink-byte sink 92) (sink-byte sink (char-code (schar "btnfr\"\\" short))))
        ((or (< code 32) (<= 127 code 159))
         (sink-octets sink (sb-ext:string-to-octets (format nil "\\u~4,'0x" code))))
        ((< code #x80) (sink-byte sink code))
        ;; UTF-8 has no surrogate: refused as the encoder refuses it
        ((<= #xd800 code #xdfff)
         (sink-octets sink (utf-8-octets (string (code-char code)))))
        (t (let ((width (cond ((< code #x800) 2) ((< code #x10000) 3) (t 4))))
             (sink-byte sink (logior (aref #(0 0 #xc0 #xe0 #xf0) width)
                                     (ash code (* -6 (1- width)))))
             (loop for shift from (* 6 (- width 2)) downto 0 by 6
                   do (sink-byte sink (logior #x80 (ldb (byte 6 shift) code))))))))

(defun sink-string (sink text)
  "TEXT as a JSON string."
  (sink-byte sink 34)
  ;; A plain ASCII run is copied a byte at a time with the index in a
  ;; register; the rest, and a string of neither simple kind, is SINK-CHAR's.
  (macrolet ((run (type)
               `(let ((text text) (octets (sink-room sink (length text)))
                      (fill (json-sink-fill sink)))
                  (declare (type ,type text) (type fixnum fill))
                  (loop for char across text
                        for code = (char-code char)
                        do (if (and (< 31 code 127) (/= code 34) (/= code 92)
                                    (< fill (length octets)))
                               (setf (aref octets fill) code fill (1+ fill))
                               (progn (setf (json-sink-fill sink) fill)
                                      (sink-char sink code)
                                      (setf octets (json-sink-octets sink)
                                            fill (json-sink-fill sink)))))
                  (setf (json-sink-fill sink) fill))))
    (typecase text
      ((simple-array character (*)) (run (simple-array character (*))))
      (simple-base-string (run simple-base-string))
      (t (loop for char across text do (sink-char sink (char-code char))))))
  (sink-byte sink 34))

(defun sink-value (sink value)
  "VALUE's JSON onto SINK."
  (flet ((items (sequence &aux (first t))
           (sink-byte sink 91)
           (map nil (lambda (item)
                      (if first (setf first nil) (sink-byte sink 44))
                      (sink-value sink item))
                sequence)
           (sink-byte sink 93))
         (shasht (&aux (*print-pretty* nil))
           (sink-octets sink (utf-8-octets (shasht:write-json value nil)))))
    (typecase value
      (json-verbatim
       (let ((octets (json-verbatim-octets value)))
         (if (< (length octets) 512)
             (sink-octets sink octets)
             (push (cons (json-sink-fill sink) octets) (json-sink-splices sink)))))
      (string (sink-string sink value))
      (hash-table
       (let ((first t))
         (sink-byte sink 123)
         (maphash (lambda (key item)
                    (if first (setf first nil) (sink-byte sink 44))
                    (sink-value sink key) (sink-byte sink 58) (sink-value sink item))
                  value)
         (sink-byte sink 125)))
      (fixnum
       (when (minusp value) (sink-byte sink 45))
       (dolist (digit (nreverse (loop for rest = (abs value) then (floor rest 10)
                                      collect (+ 48 (mod rest 10))
                                      until (< rest 10))))
         (sink-byte sink digit)))
      ((member t nil :null)
       (sink-octets sink (case value
                           ((t) #.(sb-ext:string-to-octets "true"))
                           ((nil) #.(sb-ext:string-to-octets "false"))
                           (t #.(sb-ext:string-to-octets "null")))))
      ;; shasht's tagged lists, (:ARRAY ...) and its kin, are shasht's
      (cons (if (member (car value) '(:array :object-alist :object-plist))
                (shasht)
                (items value)))
      ((and vector (not string)) (items value))
      (t (shasht)))))

(defun encode-json-octets (object &aux (sink (make-json-sink)))
  "OBJECT as the UTF-8 octets of its JSON text: the form a body leaves the
process in."
  ;; A JSON-VERBATIM inside OBJECT contributes its octets untouched; the text
  ;; around them is this call's own and does not outlive it.
  (sink-value sink object)
  (let* ((text (json-sink-octets sink))
         (splices (reverse (json-sink-splices sink)))
         (body (make-array (reduce #'+ splices :key (lambda (splice) (length (cdr splice)))
                                               :initial-value (json-sink-fill sink))
                           :element-type '(unsigned-byte 8)))
         (from 0) (to 0))
    (loop for (at . octets) in (append splices (list (cons (json-sink-fill sink) #())))
          do (replace body text :start1 to :start2 from :end2 at)
             (replace body octets :start1 (+ to (- at from)))
             (setf to (+ to (- at from) (length octets)) from at))
    body))

;;; --- encodings remembered by identity --------------------------------------
;;; What a verbatim stands for has to come from somewhere: JSON-MEMO is the
;;; where. It remembers, per object it is shown, what the object encoded to
;;; under each dialect a caller names — one lane's wire shape of a message
;;; is one dialect, another lane's another — and how long the object's own
;;; JSON text is, the number an estimate sums. Identity is EQ and the table
;;; is weak on it: an object the caller drops takes its memory with it, and
;;; the caller never copies an object it wants remembered.
;;;
;;; Items go two ways round. JSON-MEMO-ITEMS answers an object's items under
;;; a dialect: its verbatims, when every item of the last build was noted;
;;; else the builder's fresh items, each linked to the object at its
;;; position. The seam that encodes the body (ENCODE-JSON-BODY) notes every
;;; fresh item's octets as it writes them, and from then on the object's
;;; items are its verbatims. A body built and never encoded leaves its items
;;; short, and the next build starts them over.

(defstruct (json-memo (:constructor make-json-memo ()) (:copier nil))
  "Encodings remembered by object identity, weakly: LINKS maps an object,
and every item built from it, to its JSON-MEMO-ENTRY — as (ENTRY DIALECT
POSITION) for an item, (ENTRY) for the object itself."
  (links (make-hash-table :test #'eq :weakness :key :synchronized t)
   :type hash-table :read-only t))

(defstruct (json-memo-entry (:constructor make-json-memo-entry ()) (:copier nil))
  "One object as the memo knows it: DIALECTS, (DIALECT . ITEMS) with ITEMS a
simple vector holding a JSON-VERBATIM at every noted position and NIL where
the item is still to be noted; CHARS, the length of the object's own JSON
text, or NIL until measured."
  (dialects '() :type list)
  (chars nil :type (or null (integer 0))))

(access (entry json-memo-entry))

(defun json-memo-entry (memo object)
  "OBJECT's entry in MEMO, made on first sight."
  (first (alexandria:ensure-gethash object (json-memo-links memo) (list (make-json-memo-entry)))))

(defun json-memo-chars (memo object)
  "The length of OBJECT's JSON text, measured once per MEMO: an object shown
again answers the length it measured the first time."
  ;; An array measures through its elements, each remembered on its own, so an
  ;; array rebuilt every round of the same elements costs its new ones.
  (if (and (vectorp object) (not (stringp object)))
      (+ 2 (max 0 (1- (length object)))
         (loop for element across object
               sum (json-memo-chars memo element)))
      (let ((entry (json-memo-entry memo object)))
        (or entry.chars (setf entry.chars (length (encode-json-object object)))))))

(defun json-memo-items (memo objects dialect item-fn)
  "The body array for OBJECTS under DIALECT, a keyword naming one wire
shape: each object's items in turn — its verbatims when every item of its
last build under DIALECT was noted (ENCODE-JSON-BODY), else the fresh list
ITEM-FN answers for it, each item linked to the object at its position so
its note lands. => a simple vector."
  (coerce (loop for object in (coerce objects 'list)
                for entry = (json-memo-entry memo object)
                for noted = (assoc dialect entry.dialects)
                append (if (and noted (notany #'null (cdr noted)))
                           (coerce (cdr noted) 'list)
                           (let ((items (funcall item-fn object)))
                             (setf entry.dialects
                                   (acons dialect (make-array (length items) :initial-element nil)
                                          (remove dialect entry.dialects :key #'car)))
                             (loop for item in items
                                   for position from 0
                                   do (setf (gethash item (json-memo-links memo))
                                            (list entry dialect position)))
                             items)))
          'simple-vector))

(defun json-memo-note (memo item octets &aux (verbatim (json-verbatim octets))
                                             (link (gethash item (json-memo-links memo))))
  "OCTETS are ITEM's encoding: remembered as a verbatim at ITEM's position in
the items its build opened, when ITEM is one MEMO linked as an item; any other
item passes. => the JSON-VERBATIM of OCTETS."
  (when (cdr link)
    (destructuring-bind (entry dialect position) link
      (let ((noted (assoc dialect (json-memo-entry-dialects entry))))
        (when (and noted (< position (length (cdr noted))))
          (setf (svref (cdr noted) position) verbatim)))))
  verbatim)

(defun encode-json-body (body key memo)
  "BODY as octets, the values under KEY assembled through MEMO: a
JSON-VERBATIM among them is written as it is; any other value is encoded
here and noted (JSON-MEMO-NOTE), so the next build of the same items finds
its verbatim."
  ;; BODY is the caller's and is written to: KEY comes back holding the
  ;; verbatims.
  (setf (gethash key body)
        (map 'simple-vector
             (lambda (item)
               (if (json-verbatim-p item)
                   item
                   (json-memo-note memo item (encode-json-octets item))))
             (gethash key body)))
  (encode-json-octets body))

;;; --- the eval snippet's object convention ----------------------------------
;;; Decoded JSON as the model reads and writes it through eval: an
;;; object is a keyword plist (:key value ...), an array a vector, null
;;; :NULL. NCK:CALL answers with it, CONFIG-GET answers with it, CONFIG-SET
;;; and the mcp cell's arguments accept it; one pair of converters here so
;;; every surface spells an object the same way.

(defun json-plist (value)
  "Decoded JSON as a keyword plist: objects become (:key value ...), arrays
stay vectors, scalars pass. Recursive."
  (cond
    ((hash-table-p value)
     (loop for key being the hash-keys of value using (hash-value inner)
           collect (intern (string-upcase key) :keyword)
           collect (json-plist inner)))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'json-plist value))
    (t value)))

(defun plist-json (value)
  "A keyword plist as an EQUAL hash table the wire encoder writes as a JSON
object, keys downcased; vectors are walked, hash tables and scalars pass."
  ;; Recursive. The inverse of JSON-PLIST for snake_case keys.
  (cond
    ((and (consp value) (keywordp (car value)))
     (apply #'make-json-object (loop for (key inner) on value by #'cddr
                                     collect (string-downcase (symbol-name key))
                                     collect (plist-json inner))))
    ((and (vectorp value) (not (stringp value)))
     (map 'vector #'plist-json value))
    (t value)))

(defmacro json-object (&rest specs &aux (object (gensym "OBJECT"))
                                        (forms '()))
  "Build one JSON object (an EQUAL hash table) from literal-key SPECS."
  ;; The construction twin of WITH-JSON: where WITH-JSON is how a wire field
  ;; is read, JSON-OBJECT is how a wire object is built. A spec is one of
  ;;
  ;;   KEY FORM             set KEY to FORM's value, unconditionally
  ;;   :OPT KEY FORM        evaluate FORM once; set KEY only when it is non-NIL
  ;;   :WHEN TEST KEY FORM  when TEST is true, set KEY to FORM's value
  ;;   (:GETF PLIST [:OPT] KEY...)  each KEY to PLIST's member of the same name,
  ;;                        snake_case read as the kebab-case keyword: "turn_id"
  ;;                        is (GETF PLIST :TURN-ID), :OPT as above
  ;;
  ;; KEY is a literal string, checked at macroexpansion — a computed key is a
  ;; lookup table, not a wire field, and belongs in MAKE-JSON-OBJECT or a
  ;; direct SETF of GETHASH. Specs evaluate left to right, tests and values in
  ;; spec order. With no specs the result is an empty object.
  ;;
  ;; :OPT folds NIL to key absence, and shasht writes NIL as JSON false, so a
  ;; false boolean can never ride through :OPT; a deliberate false is written
  ;; with :WHEN (the way navigate_checkpoint sends restore_files).
  (loop while specs
        do (let ((head (pop specs)))
             (flet ((take (what)
                      (when (null specs)
                        (error "JSON-OBJECT: ~s is missing its ~a" head what))
                      (pop specs)))
               (cond
                 ((and (consp head) (eq :getf (first head)))
                  (destructuring-bind (plist &rest keys) (rest head)
                    (let ((opt (and (eq :opt (first keys)) (pop keys))))
                      (dolist (key (reverse keys))
                        (let ((name (wire-symbol key)))
                          (setf specs `(,@(and opt '(:opt)) ,key (getf ,plist ,name) ,@specs)))))))
                 ((stringp head)
                  (push `(setf (gethash ,head ,object) ,(take "value form"))
                        forms))
                 ((member head '(:opt :when))
                  (let* ((test (and (eq head :when) (take "test form")))
                         (key (take "key")))
                    (unless (stringp key)
                      (error "JSON-OBJECT: ~s needs a literal string key, got ~s" head key))
                    (push (if (eq head :when)
                              `(when ,test
                                 (setf (gethash ,key ,object) ,(take "value form")))
                              (let ((value (gensym "VALUE")))
                                `(when-let (,value ,(take "value form"))
                                   (setf (gethash ,key ,object) ,value))))
                          forms)))
                 (t
                  (error "JSON-OBJECT: ~s is neither a literal string key nor ~
                            :OPT/:WHEN — computed keys go through MAKE-JSON-OBJECT"
                         head))))))
  `(let ((,object (make-hash-table :test #'equal)))
     ,@(nreverse forms)
     ,object))
