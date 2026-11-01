;;;; bloat.lisp --- structural bloat ledger for Common Lisp sources.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; tokei counts the lines; this file says which of them are the same lines
;;;; written twice, and which single collapse removes the most of them.
;;;;
;;;; It reads every top-level form with the same reader that runs the code, and
;;;; digests every subtree by shape: a list keeps the name of its head symbol
;;;; and the shapes of its elements, every leaf becomes one wildcard whose size
;;;; is still counted. So (defun a (x) (f x)) and (defun b (y) (f y)) share a
;;;; digest -- a name, a value or a keyword is not what makes two blocks two
;;;; blocks. Clusters of one digest are candidate definitions, and a cluster's
;;;; weight is what collapsing it removes, in lines: its printed size over the
;;;; file's chars-per-code-line, tokei's own count being the yardstick.
;;;;
;;;; Reads are made with *read-eval* nil, in a throwaway package: no source
;;;; file can run anything at read time, and no symbol here can be clobbered by
;;;; a symbol in a source file. A file that will not read is named and skipped,
;;;; never half-counted.
;;;;
;;;;   (nodecode.bloat:report)                     ; src/ and cells/
;;;;   (nodecode.bloat:report :paths '("src"))     ; the product alone

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage #:nodecode.bloat
  (:use #:common-lisp)
  (:export #:report #:scan #:*src-root*))

(in-package #:nodecode.bloat)

(defparameter *src-root*
  ;; this file sits in src/tools/check/: two levels up is src/
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-parent-directory-pathname
    (make-pathname :name nil :type nil :defaults (or *load-truename* (truename "./")))))
  "The directory holding src/, cells/ and test/ -- the asdf root.")

;;; --- a shape digest ------------------------------------------------------

(defconstant +mask+ #x3FFFFFFFFFFFFFF)

(declaim (inline mix))
(defun mix (a b)
  "Order-sensitive, fixnum-safe blend of two digests."
  (let ((x (logand (logxor a (ash b 19)) +mask+)))
    (logand (logxor x (ash (logand x #x7FFFFFF) 13) (ash x -23)) +mask+)))

(defvar *memo* nil
  "Eq hash: cons -> (digest nodes chars), for one file's walk.")

;;; A list headed by one of them keeps the head name in its shape, so (let
;;; ...) never reads as (when ...) however alike they are. Every other head --
;;; a variable, a keyword, a number, an argument list -- is data, and data is
;;; a wildcard: (defun f (x) ..) and (defun g (y) ..) are the same shape,
;;; which is the whole point of asking.
(defvar *operators* (make-hash-table :test #'equal)
  "Names that count as operators.")

(defun digest-of (form)
  "Values: the shape digest of FORM, its node count, its printed size."
  (if (consp form)
      (let ((hit (gethash form *memo*)))
        (if hit
            (values-list hit)
            (let* ((head (car form)) (tail (cdr form))
                   (hname (and (symbolp head) (symbol-name head))))
              (multiple-value-bind (hd hn hc)
                  (cond ((and hname (or (gethash hname *operators*)
                                        (and (plusp (length hname)) (char= (char hname 0) #\&))))
                         (values (mix 7919 (sxhash hname)) 1 (+ 1 (length hname))))
                        (hname (values 5381 1 1))
                        (t (digest-of head)))
                (let ((d (mix hd 5381))
                      (nodes (1+ hn))
                      (chars (+ hc 1)))
                  (loop for rest on tail
                        do (multiple-value-bind (cd cn cc)
                               (digest-of (if (consp rest) (car rest) rest))
                             (setf d (mix d cd)
                                   nodes (+ nodes cn)
                                   chars (+ chars cc 1))))
                  (unless (consp tail) (incf chars 2))
                  (values-list (setf (gethash form *memo*) (list d nodes chars))))))))
      (values 5381 1 (if (null form) 3 (length (prin1-to-string form))))))

;;; --- clusters ------------------------------------------------------------

(defstruct (blob (:constructor make-blob (digest)))
  digest (count 0) (nodes 0) (chars 0) example file line (sites '()))

(defvar *collect* nil
  "When bound to a list, every subtree of at least *shingle-nodes* nodes
pushes its digest here: a definition's shape reduced to the shapes inside it,
which is what two definitions are compared by when neither is the other.")

;;; Below this every block shares shapes with every other and the comparison
;;; says nothing.
(defparameter *shingle-nodes* 8
  "The smallest subtree that counts as one shingle.")

(defun visit (form file line table min-nodes)
  (multiple-value-bind (d nodes chars) (digest-of form)
    (when (and (consp *collect*) (>= nodes *shingle-nodes*)) (push d (cdr *collect*)))
    (when (>= nodes min-nodes)
      (let ((blob (or (gethash d table) (setf (gethash d table) (make-blob d)))))
        (incf (blob-count blob))
        (setf (blob-nodes blob) nodes (blob-chars blob) chars)
        (unless (blob-example blob)
          (setf (blob-example blob) form (blob-file blob) file (blob-line blob) line))
        (when (< (length (blob-sites blob)) 5)
          (push (format nil "~a:~d" (enough-namestring file *src-root*) line)
                (blob-sites blob))))))
  (when (consp form)
    (loop for tail = form then (cdr tail)
          while (consp tail) do (visit (car tail) file line table min-nodes)
          finally (when tail (visit tail file line table min-nodes)))))

;;; --- reading -------------------------------------------------------------

(defun file-text (path)
  (uiop:read-file-string path :external-format :utf-8))

(defun line-of (newlines pos)
  "The 1-based line of character position POS."
  (1+ (count pos newlines :test #'>)))

(defvar *read-package* (make-package "NODECODE.BLOAT.READ" :use nil)
  "Where source symbols are interned, so they cannot touch this image.")

(defun read-top-forms (text)
  "Values: list of (form start end), and whether the whole file was read."
  (let ((*read-eval* nil)
        (*package* *read-package*)
        (*readtable* (let ((table (copy-readtable nil)))
                       (set-dispatch-macro-character
                        #\# #\. (lambda (stream char argument)
                                  (declare (ignore char argument))
                                  (read stream t nil t)
                                  :bloat-discarded-read-eval)
                        table)
                       table))
        (in (make-string-input-stream text))
        (ok t))
    (values (loop for start = (file-position in)
                  for form = (handler-case (read in nil :eof)
                               (error (e) (setf ok (format nil "~a" e)) :eof))
                  until (eq form :eof)
                  collect (list form start (file-position in)))
            ok)))

;;; --- tokei ---------------------------------------------------------------

(defun tokei-table ()
  "Hash: file namestring -> (code-lines . chars-per-code-line), from tokei."
  (let ((out (make-hash-table :test #'equal))
        (json (uiop:run-program (list "tokei" "--output" "json" (namestring *src-root*))
                                :output :string :ignore-error-status t)))
    ;; Every file is one report under its language; Total's children repeat them.
    (loop for language being the hash-values of (nlk:decode-json json)
          do (loop for node across (gethash "reports" language)
                   for name = (nlk:json-value node :string "name")
                   for code = (nlk:json-value node :integer "stats" "code")
                   when (and name code (plusp code) (probe-file name))
                     do (setf (gethash name out)
                              (cons code (max 1.0 (/ (length (file-text name)) code))))))
    out))

;;; --- the ledger ----------------------------------------------------------

(defun subform-p (small big)
  (or (equal small big)
      (and (consp big) (or (subform-p small (car big)) (subform-p small (cdr big))))))

(defun near-clones (items &key (threshold 0.55) (min-lines 8))
  "Definitions that share most of their shape without being each other: for
each pair, the share of shingles they have in common, over the shingles either
one has."
  ;; A pair at 0.8 is one definition written twice with a different name and a
  ;; different call inside; the collapse is one body, parameterized.
  (let ((index (make-hash-table :test #'eql))
        (counts (make-hash-table :test #'equal))
        (found '()))
    (loop for item in items
          for shingles = (sixth item)
          when (and shingles (>= (fourth item) min-lines))
            do (dolist (s shingles) (push item (gethash s index))))
    (loop for holders being the hash-values of index
          when (<= 2 (length holders) 60)
            do (loop for (a . rest) on holders
                     do (dolist (b rest)
                          (incf (gethash (if (string< (second a) (second b))
                                             (cons a b) (cons b a))
                                         counts 0)))))
    (loop for key being the hash-keys of counts using (hash-value shared)
          for (a . b) = key
          for union-size = (length (union (sixth a) (sixth b)))
          for score = (and (>= shared 3) (plusp union-size) (/ shared union-size))
          when (and score (>= score threshold))
            do (push (list score (fourth a) (fourth b) a b) found))
    (sort found #'> :key (lambda (pair) (* (first pair) (min (second pair) (third pair)))))))

(defun group-lines (members)
  "Lines saved by keeping the largest definition of MEMBERS and calling it."
  (- (reduce #'+ members :key #'fourth) (reduce #'max members :key #'fourth :initial-value 0)))

(defun group-pairs (pairs &aux (cluster-of (make-hash-table :test #'eq))
                               (clusters '()))
  "Union near-duplicate pairs into the clusters they describe: a definition
that shares its shape with three others is one collapse, not three pairs."
  (flet ((attach (item cluster)
           (push item (cdr cluster))
           (setf (gethash item cluster-of) cluster)))
    ;; A pair joins the cluster either side is in, merges two, or opens one.
    (loop for (score nil nil a b) in pairs
          for ca = (gethash a cluster-of)
          for cb = (gethash b cluster-of)
          for cluster = (or ca cb (car (push (list score) clusters)))
          unless (and ca (eq ca cb))
            do (dolist (item (cond ((and ca cb)
                                    (prog1 (cdr cb) (setf clusters (delete cb clusters))))
                                   (ca (list b))
                                   (cb (list a))
                                   (t (list b a))))
                 (attach item cluster))
               (setf (car cluster) (max (car cluster) score))))
  (sort clusters #'> :key (lambda (cluster) (group-lines (cdr cluster)))))

(defun print-form (form limit &aux (*package* *read-package*) (*print-case* :downcase)
                                   (*print-circle* nil) (text (prin1-to-string form)))
  (subseq text 0 (min limit (length text))))

(defun blob-lines (blob table)
  (let ((ratio (or (cdr (gethash (namestring (blob-file blob)) table)) 40.0)))
    (max 1 (round (blob-chars blob) ratio))))

(defun scan (&key (paths '("src" "cells")) (min-nodes 14))
  "Values: blocks, defs, unread, tokei table, file count, code lines."
  (let ((blocks (make-hash-table :test #'eql))
        (defs (make-hash-table :test #'eql))
        (unread '())
        (table (tokei-table))
        (files (loop for path in paths
                     for root = (uiop:ensure-directory-pathname (merge-pathnames path *src-root*))
                     append (sort (uiop:directory* (merge-pathnames "**/*.lisp" root))
                                  #'string< :key #'namestring)))
        (code 0))
    (when (zerop (hash-table-count *operators*))
      ;; A macro or a special operator is FBOUNDP too.
      (do-all-symbols (symbol)
        (when (fboundp symbol) (setf (gethash (symbol-name symbol) *operators*) t))))
    (dolist (file files)
      (let* ((text (file-text file))
             (*memo* (make-hash-table :test #'eq))
             (newlines (loop for i from 0 below (length text)
                             when (char= (char text i) #\Newline) collect i)))
        (incf code (or (car (gethash (namestring file) table)) 0))
        (multiple-value-bind (forms ok) (read-top-forms text)
          (unless (eq ok t)
            (push (format nil "~a -- ~a" (enough-namestring file *src-root*) ok) unread))
          (loop for (form start end) in forms
                for line = (line-of newlines start)
                for head = (and (consp form) (symbolp (car form)) (symbol-name (car form)))
                for definition = (and head (>= (length head) 4) (string= "DEF" head :end2 3) form)
                do (let ((*collect* (when definition (cons t nil))))
                     (visit form file line blocks min-nodes)
                     (when definition
                       (multiple-value-bind (d nodes) (digest-of form)
                         (push (list (second form) (enough-namestring file *src-root*)
                                     line (1+ (- (line-of newlines end) line)) nodes
                                     (remove-duplicates (cdr *collect*)))
                               (gethash d defs)))))))))
    (values blocks defs (nreverse unread) table (length files) code)))

;;; Below this, two forms share a shape because the shape IS the whole thing --
;;; (defparameter +x+ "...") is not code written twice, it is a value; the
;;; group it would join is noise that buries the groups worth reading.
(defparameter *min-def-nodes* 40
  "A definition is only a clone candidate when it is big enough to be one.")

;;; A short form repeats because it is an idiom, not because someone wrote it
;;; twice.
(defparameter *min-block-chars* 200
  "Printed size a repeated block needs before it counts.")

(defun top-blocks (blocks table &key (top 12))
  (let ((list '()) (kept '()))
    (maphash (lambda (d blob)
               (when (and (>= (blob-count blob) 2)
                          (>= (blob-chars blob) *min-block-chars*))
                 (push blob list)))
             blocks)
    (setf list (sort list #'> :key (lambda (b) (* (1- (blob-count b)) (blob-lines b table)))))
    (dolist (b list)
      (unless (find-if (lambda (k) (and (= (blob-count k) (blob-count b))
                                        (> (blob-chars k) (blob-chars b))
                                        (subform-p (blob-example b) (blob-example k))))
                       kept)
        (push b kept)))
    (subseq (nreverse kept) 0 (min top (length kept)))))

(defun report (&key (paths '("src" "cells")) (min-nodes 14) (top 12) (clones 8))
  (multiple-value-bind (blocks defs unread table files code)
      (scan :paths paths :min-nodes min-nodes)
    (let* ((items (reverse (loop for members being the hash-values of defs append members)))
           (pairs (near-clones items))
           (near (group-pairs pairs))
           (near-saved (reduce #'+ near :key (lambda (c) (group-lines (cdr c))) :initial-value 0))
           (clone-pairs (let ((groups (loop for members being the hash-values of defs
                                            when (and (>= (length members) 2)
                                                      (>= (fifth (first members)) *min-def-nodes*))
                                              collect (nreverse members))))
                          (setf groups (sort (nreverse groups) #'> :key #'group-lines))
                          (subseq groups 0 (min 1000 (length groups)))))
           (clone-saved (reduce #'+ clone-pairs :key #'group-lines :initial-value 0))
           (clone-count (reduce #'+ clone-pairs :key #'length :initial-value 0))
           (block-list (top-blocks blocks table :top top))
           (block-saved (reduce #'+ block-list
                                :key (lambda (b) (* (1- (blob-count b))
                                                    (blob-lines b table))))))
      (flet ((sites (members more)
               (append (loop for m in (subseq members 0 (min 4 (length members)))
                             collect (format nil "~a:~d" (second m) (third m)))
                       (when (> (length members) 4)
                         (list (format nil "and ~d ~a" (- (length members) 4) more))))))
        (with-output-to-string (out)
          (format out "BLOAT LEDGER  ~:d code lines (tokei) in ~:d files  [~{~a~^ ~}]~%~@[unread: ~{~a~^, ~}~%~]"
                  code files paths unread)
          (format out "~%A. IDENTICAL DEFINITIONS -- ~d groups, ~d definitions, ~:d lines saved~%"
                  (length clone-pairs) clone-count clone-saved)
          (dolist (members (subseq clone-pairs 0 (min clones (length clone-pairs))))
            (format out "   ~3dx ~4d lines saved  ~a~%        ~{~a~^  ~}~%"
                    (length members) (group-lines members)
                    (print-form (first (first members)) 34)
                    (sites members "more sites")))
          (format out "~%B. REPEATED BLOCKS -- ~d clusters, ~:d lines in the top ~d~%"
                  (length block-list) block-saved (length block-list))
          (loop for blob in block-list
                for i from 1
                for lines = (blob-lines blob table)
                do (format out "   ~2d. ~4d lines ~2dx ~3d   ~a~%       ~a~%"
                           i (* (1- (blob-count blob)) lines) (blob-count blob) lines
                           (print-form (blob-example blob) 110)
                           (format nil "~{~a~^ ~}" (reverse (blob-sites blob)))))
          (format out "~%C. NEAR-DUPLICATE DEFINITIONS -- ~d clusters from ~d pairs, ~:d lines saved~%"
                  (length near) (length pairs) near-saved)
          (dolist (cluster (subseq near 0 (min top (length near))))
            (format out "   ~3dx ~4d lines saved  best ~,2f  ~a~%        ~{~a~^  ~}~%"
                    (1- (length (cdr cluster))) (group-lines (cdr cluster)) (car cluster)
                    (print-form (first (second cluster)) 30)
                    (sites (cdr cluster) "more")))
          (format out "~%RECLAIMABLE  ~:d lines  = ~,1f% of the ~:d scanned (blocks and exact clones only)~%"
                  (+ clone-saved block-saved)
                  (* 100.0 (/ (+ clone-saved block-saved) (max 1 code))) code))))))
