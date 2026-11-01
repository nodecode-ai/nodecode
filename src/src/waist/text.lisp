;;;; text.lisp --- plain helpers every part reaches for: one line of text,
;;;; a trim, a cut, the lines, the distinct ones, a file's text, a project's
;;;; root, a stamp's time and a time's stamp, a duration's seconds.
;;;; Kernel-owned.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each of these had been written five or six times over, once per
;;;; cell, the same body under a different name. One definition here,
;;;; read by every folder, so a fix lands once and a reader learns one
;;;; name.

(in-package #:nodecode.kernel)

(defun one-line (value &key cap (ellipsis "..."))
  "VALUE as one line: runs of blanks collapsed to one space, trimmed; a
non-string is printed first, NIL is empty."
  ;; With CAP, a longer line is cut to CAP - 1 characters and ELLIPSIS.
  ;; The blanks are cl-ppcre's \s: space, tab, newline, return and page.
  ;; One linear pass instead of allocating a replacement for every blank
  ;; run: history labels alone walk several megabytes of prompts at once.
  (let* ((text (cond ((stringp value) value)
                     ((null value) "")
                     (t (princ-to-string value))))
         (out (make-string (length text) :element-type (array-element-type text)))
         (end 0)
         (blank nil))
    (declare (type string text) (type fixnum end) (optimize (speed 3) (safety 1)))
    (loop for char across text
          do (case char
               ((#\Space #\Tab #\Newline #\Return #\Page)
                (when (plusp end) (setf blank t)))
               (otherwise
                (when blank
                  (setf (char out end) #\Space blank nil)
                  (incf end))
                (setf (char out end) char)
                (incf end))))
    (if (and cap (> end cap)) (uiop:strcat (subseq out 0 (1- cap)) ellipsis) (subseq out 0 end))))

(defun first-line (text &optional limit &aux (end (or (position #\Newline text) (length text))))
  "TEXT up to its first newline, cut to LIMIT characters when LIMIT is given."
  (subseq text 0 (if limit (min end limit) end)))

(defun split-words (text)
  "TEXT split on blank runs (space, tab, CR, LF): its words in order, NIL for
blank or NIL TEXT."
  (remove "" (uiop:split-string (or text "")
                                :separator '(#\Space #\Tab #\Return #\Newline))
          :test #'string=))

(defun trimmed (text)
  "TEXT without its leading and trailing blanks -- space, tab, return,
newline. The one bag: a caller that spells the character list out disagrees
with the next one by a character sooner or later."
  (string-trim '(#\Space #\Tab #\Return #\Newline) text))

(defun narrow-string (string)
  "STRING as a simple-base-string when every character of it is one, else
STRING itself."
  ;; A character string takes four bytes a character and a base string one:
  ;; text kept for long -- painted transcript rows, mostly ASCII -- is kept
  ;; narrow.
  (if (and (not (typep string 'simple-base-string)) (every (lambda (char) (typep char 'base-char)) string))
      (coerce string 'simple-base-string)
      string))

(defun clip (text limit &key (ellipsis "...") disclose)
  "TEXT cut to its first LIMIT characters when longer (a NIL LIMIT never cuts),
followed by ELLIPSIS -- or, with DISCLOSE, by a line counting the cut, DISCLOSE's
own words after it when it is a string."
  (if (and limit (> (length text) limit))
      (if disclose
          (format nil "~a~%... [truncated ~:d chars~@[; ~a~]]"
                  (subseq text 0 limit) (- (length text) limit)
                  (and (stringp disclose) disclose))
          (concatenate 'string (subseq text 0 limit) ellipsis))
      text))

;;; Without this budget, a report would name the value that failed a check in
;;; full, and the model would read all of it for the one line that matters.
(defparameter *condition-report-budget* 2000
  "Characters a condition's report may take when it travels on as text -- a
tool result, a shell notice.")

(defun safe-condition-report (condition)
  "CONDITION's report text, its datum printed at *PRINT-LEVEL* 3
*PRINT-LENGTH* 12 (a rung of the value ladder) and the whole cut to
*CONDITION-REPORT-BUDGET* characters with the count named; or a typed
placeholder when the report itself signals."
  ;; Printing a condition is not
  ;; passive: a form compiled with errors is a SIMPLE-PROGRAM-ERROR whose
  ;; format-control embeds the failing source and the compiler diagnostic
  ;; verbatim with no arguments, so ~A would re-run FORMAT over
  ;; model-authored bytes. A stray tilde in them would signal a fresh
  ;; FORMAT-ERROR from inside a handler arm, past every protection; under
  ;; --disable-debugger that would be whole-image death. The level bound
  ;; is also what keeps a cyclic datum finite: a TYPE-ERROR whose datum is
  ;; a TUI projection memo -- a group whose memo holds the block whose row
  ;; is the group -- would print unbounded by the shell's error handler
  ;; until the heap was gone.
  (let ((report
          (handler-case (let ((*print-level* 3) (*print-length* 12))
                          (princ-to-string condition))
            (serious-condition ()
              (if (and (typep condition 'simple-condition)
                       (stringp (simple-condition-format-control condition))
                       (null (simple-condition-format-arguments condition)))
                  ;; With no arguments the control string IS the message --
                  ;; hand it over as plain text, un-reformatted, tildes and
                  ;; all. Anything else degrades to the type name alone.
                  (simple-condition-format-control condition)
                  (format nil "~s (condition report unprintable)"
                          (type-of condition)))))))
    (clip report *condition-report-budget*
          :ellipsis (format nil " [report elided: ~:D characters in full]" (length report)))))

(defun lines (text &aux (text (or text "")) (lines '()))
  "TEXT's lines, LF or CRLF, the trailing newline's empty tail dropped;
NIL text has none."
  ;; A search for each LF, not a regex: split on `\r?\n'. One CR before an
  ;; LF goes with the LF, a lone CR stays, and every empty line at the end is
  ;; dropped.
  (loop for start = 0 then (1+ newline)
        for newline = (position #\Newline text :start start)
        for end = (or newline (length text))
        do (push (subseq text start (if (and newline (> end start)
                                             (char= #\Return (char text (1- end))))
                                        (1- end)
                                        end))
                 lines)
        while newline)
  (loop while (and lines (zerop (length (first lines)))) do (pop lines))
  (nreverse lines))

(defun distinct (strings)
  "STRINGS without duplicates, first occurrence kept, order held."
  (remove-duplicates strings :test #'equal :from-end t))

(defun string-set (strings &aux (table (make-hash-table :test #'equal)))
  "STRINGS as a hash set: each a key EQUAL finds, mapped to T."
  (dolist (string strings table)
    (setf (gethash string table) t)))

(defun unused-name (base taken-p)
  "BASE, else BASE-2, BASE-3, ...: the first name TAKEN-P answers false for."
  (loop for n from 1
        for name = (if (= n 1) base (format nil "~a-~d" base n))
        unless (funcall taken-p name) return name))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The universal time of 1970-01-01T00:00:00Z.")

(defun unix-now ()
  "Seconds since the Unix epoch, now."
  (- (get-universal-time) +unix-epoch+))

(defun deadline-after (seconds)
  "The internal real time SECONDS (any real) from now: what a bounded wait
compares GET-INTERNAL-REAL-TIME against."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun wait-until (test deadline &optional (pause 0.005))
  "TEST's first true answer, asked every PAUSE seconds until the internal real
time is past DEADLINE (DEADLINE-AFTER's); NIL then."
  (loop thereis (funcall test)
        until (>= (get-internal-real-time) deadline)
        do (sleep pause)))

(defun elapsed-ms (since &optional (now (get-internal-real-time)))
  (round (* 1000 (- now since)) internal-time-units-per-second))

(defun iso-time (unix &key (millis t))
  "UNIX seconds (any real) as UTC ISO-8601, to the millisecond unless MILLIS
is NIL."
  (multiple-value-bind (whole fraction) (floor unix)
    (bind (((s m h d mo y) (decode-universal-time (+ whole +unix-epoch+) 0)))
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~:[~*~;.~3,'0d~]Z"
              y mo d h m s millis (min 999 (floor (* fraction 1000)))))))

(defun size-text (bytes)
  "BYTES as a reader sizes a disk by eye: B, then KB, MB, GB or TB to one
decimal."
  (let ((bytes (or bytes 0)))
    (if (< bytes 1024)
        (format nil "~d B" bytes)
        (loop for unit in '("KB" "MB" "GB" "TB")
              for size = (/ bytes 1024d0) then (/ size 1024)
              when (or (< size 1024) (string= unit "TB"))
                return (format nil "~,1f ~a" size unit)))))

(defun duration-text (seconds &key coarse)
  "SECONDS as the shortest honest reading: 45s, 13m, 2h 13m, 3d 4h -- or,
under COARSE, its largest unit alone: 2h, 3d."
  (bind ((seconds (max 0 (round seconds))) ((days rest) (floor seconds 86400))
         ((hours rest) (floor rest 3600)) (minutes (floor rest 60)))
    (cond ((plusp days) (format nil "~dd~:[ ~dh~;~]" days coarse hours))
          ((plusp hours) (format nil "~dh~:[ ~dm~;~]" hours coarse minutes))
          ((plusp minutes) (format nil "~dm" minutes))
          (t (format nil "~ds" seconds)))))

(defun iso-universal (iso)
  "The universal time an ISO-8601 stamp names -- its date and time, any zone
suffix read as UTC -- or NIL for anything that is not one."
  (and (stringp iso) (>= (length iso) 19)
       (ignore-errors
        ;; second, minute, hour, day, month, year
        (apply #'encode-universal-time
               (append (loop for (start end) in '((17 19) (14 16) (11 13) (8 10) (5 7) (0 4))
                             collect (parse-integer iso :start start :end end))
                       '(0))))))

(defparameter +duration-units+
  '(("s" . 1) ("sec" . 1) ("secs" . 1) ("second" . 1) ("seconds" . 1)
    ("m" . 60) ("min" . 60) ("mins" . 60) ("minute" . 60) ("minutes" . 60)
    ("h" . 3600) ("hr" . 3600) ("hrs" . 3600) ("hour" . 3600) ("hours" . 3600)
    ("d" . 86400) ("day" . 86400) ("days" . 86400)
    ("w" . 604800) ("week" . 604800) ("weeks" . 604800))
  "Unit words and their seconds. A bare unit (\"hour\") counts as one.")

(defun parse-duration (text &aux (total 0)
                                 (consumed 0))
  "Seconds for TEXT, a sum of terms like \"2h\", \"30 min\", \"hour\", \"1h 30m\";
NIL when TEXT is not a duration."
  (ppcre:do-register-groups (count unit) ("(\\d*)\\s*([a-z]+)\\s*" text)
    (let ((seconds (cdr (assoc unit +duration-units+ :test #'string=))))
      (unless seconds (return-from parse-duration nil))
      (incf consumed)
      (incf total (* (if (plusp (length count)) (parse-integer count) 1)
                     seconds))))
  (and (plusp consumed)
       (ppcre:scan "^(?:\\d*\\s*[a-z]+\\s*)+$" text)
       total))

(defun utf-8-octets (text)
  "TEXT as UTF-8 bytes, a lone surrogate refused: what UTF-8-TEXT reads back."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun utf-8-size (text)
  "The bytes TEXT takes in UTF-8, counted without spelling them out: the
length of (UTF-8-OCTETS TEXT) for every TEXT it spells."
  (loop for char across text
        sum (let ((code (char-code char)))
              (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))))

(defun utf-8-text (octets &aux (octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
  "The text OCTETS spell in UTF-8, a fresh string exactly as long: what
SB-EXT:OCTETS-TO-STRING answers, which a malformed sequence is left to."
  ;; Two passes over well-formed UTF-8 -- no overlong form, no surrogate,
  ;; nothing past #x10FFFF: the characters counted, a byte that continues
  ;; none starting one, then written into a string that long. SBCL's own
  ;; decoder grows a buffer and copies it out.
  (let ((end (length octets))
        (count 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type fixnum end count)
             (optimize speed (safety 0)))
    (dotimes (at end)
      (unless (= (logand (aref octets at) #xc0) #x80) (incf count)))
    (let ((text (make-string count)))
      (decode-utf-8 octets 0 end text 0
                    (lambda ()
                      (return-from utf-8-text
                        (sb-ext:octets-to-string octets :external-format :utf-8))))
      text)))

(defun read-text (path &optional limit &aux (octets (file-octets path limit)))
  "The file at PATH as a string, or NIL when it is absent, unreadable, not
UTF-8, or longer than LIMIT bytes."
  ;; One open and one decode: probed, then read through a character stream,
  ;; a file was opened twice and decoded at SBCL's pace (UTF-8-TEXT).
  (and octets (ignore-errors (utf-8-text octets))))

(defun append-json-line (path value)
  "VALUE as one compact JSON line appended to PATH, the file and its parents
made when absent: what a line-oriented ledger reads back (READ-JSON-LINES)."
  ;; The directory is made when the open finds none. Off Windows the line
  ;; is one write to a descriptor opened to append; without this each line
  ;; would make a stream and its buffer.
  (let ((line (concatenate 'string (encode-json-object value) (string #\Newline))))
    #-win32
    (let* ((octets (sb-ext:string-to-octets line :external-format :utf-8))
           (native (sb-ext:native-namestring (merge-pathnames path) :as-file t))
           (fd (flet ((open-end ()
                        (sb-posix:open native (logior sb-posix:o-wronly sb-posix:o-append sb-posix:o-creat)
                                       #o666)))
                 (with-handlers ((sb-posix:syscall-error (condition)
                                   (unless (= (sb-posix:syscall-errno condition) sb-posix:enoent)
                                     (error condition))
                                   (ensure-directories-exist path)
                                   (open-end)))
                   (open-end)))))
      (with-cleanup ((sb-posix:close fd))
        (sb-sys:with-pinned-objects (octets)
          (loop with start = 0
                while (< start (length octets))
                do (incf start (sb-posix:write fd (sb-sys:sap+ (sb-sys:vector-sap octets) start)
                                               (- (length octets) start)))))))
    #+win32
    (flet ((open-end ()
             (open path :direction :output :if-exists :append :if-does-not-exist :create
                        :external-format :utf-8)))
      (with-open-stream (out (with-handlers ((file-error ()
                                               (ensure-directories-exist path)
                                               (open-end)))
                               (open-end)))
        (write-string line out))))
  value)

(defun read-json-lines (path &key (test #'hash-table-p))
  "Every line of PATH that decodes to a value TEST accepts, in file order: a
blank or torn line is skipped, and an absent file has none."
  (loop for line in (lines (read-text path))
        for value = (ignore-errors (decode-json line))
        when (funcall test value) collect value))

(defun project-root (directory)
  "The nearest directory at or above DIRECTORY that holds a .git -- a
repository or a worktree -- as a path without its trailing slash, or NIL."
  (loop for current = (ignore-errors (uiop:ensure-directory-pathname directory))
          then (let ((parent (uiop:pathname-parent-directory-pathname current)))
                 (and parent (not (equal parent current)) parent))
        while current
        do (when (probe-file (concatenate 'string (namestring current) ".git"))
             (return (string-right-trim "/" (namestring current))))))
