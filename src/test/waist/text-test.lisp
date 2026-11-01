;;;; text-test.lisp --- exact shared whitespace-normalization contract.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(deftest one-line-preserves-its-whitespace-and-cap-contract ()
  (is-table (value expected)
    (string= expected (nlk:one-line value))
    (nil "") (42 "42") ("" "") (" " "")
    ("alpha  beta" "alpha beta")
    ((format nil " ~cα~%β~c~cγ ~c" #\Tab #\Return #\Page #\Tab) "α β γ")
    ;; Neither a vertical tab nor a Unicode non-breaking space is ppcre's \s.
    ((string (code-char 11)) (string (code-char 11)))
    ((string (code-char 160)) (string (code-char 160))))
  (is (string= "alpha" (nlk:one-line " alpha " :cap 5)))
  (is (string= "alph..." (nlk:one-line "alpha beta" :cap 5)))
  (is (string= "alph…" (nlk:one-line "alpha beta" :cap 5 :ellipsis "…")))
  (is (string= "..." (nlk:one-line "ab" :cap 1))))

(deftest one-line-linear-pass-matches-the-existing-regex-contract ()
  (let ((alphabet (list #\a #\λ #\Space #\Tab #\Newline #\Return #\Page
                        (code-char 11) (code-char 160))))
    (labels ((check (prefix remaining)
               (let ((text (coerce prefix 'string)))
                 (is (string= (string-trim " " (cl-ppcre:regex-replace-all "\\s+" text " "))
                              (nlk:one-line text))))
               (when (plusp remaining)
                 (dolist (char alphabet) (check (cons char prefix) (1- remaining))))))
      (check nil 4)))
  (let ((text (make-array 8 :element-type 'character :initial-contents " a  b   "
                           :fill-pointer 7 :adjustable t)))
    (is (string= "a b" (nlk:one-line text))))
  (let ((text (format nil "~{ ~a~%~}" (make-list 20000 :initial-element "word"))))
    (is (string= "word word..." (nlk:one-line text :cap 10)))))
(deftest lines-split-as-the-crlf-pattern-does ()
  ;; LINES searches for each LF instead of matching `\r?\n'; the answer must
  ;; be the pattern's own, edges included.
  (dolist (text (list "" (string #\Newline) "a" (format nil "a~%") (format nil "a~%~%~%")
                      (format nil "~%a~%b") (format nil "a~c~%b~c" #\Return #\Return)
                      (format nil "a~c~c~%b" #\Return #\Return) (format nil "~c~%" #\Return)
                      (format nil "a~%  ~%") (coerce (format nil "x~%y~%") 'base-string)))
    (is (equal (ppcre:split "\\r?\\n" text) (nlk:lines text)) (format nil "~s" text)))
  (is (null (nlk:lines nil))))

(deftest utf-8-text-reads-as-sbcl-decodes ()
  ;; Well-formed bytes are decoded here and the rest left to SBCL's decoder:
  ;; the answer is SBCL's either way, a refusal included.
  (dolist (text (list "" "plain" (format nil "a~%b") "é — ü" "日本語" (string (code-char #x1F600))
                      (coerce (list (code-char #xFEFF) #\a) 'string) (string (code-char #x10FFFF))))
    (let ((decoded (nlk::utf-8-text (sb-ext:string-to-octets text :external-format :utf-8))))
      (is (equal text decoded) (format nil "~s" text))
      (is (typep decoded '(simple-array character (*))))))
  (dolist (codes '((#x80) (#xC0 #xAF) (#xE0 #x80 #xAF) (#xED #xA0 #x80) (#xE2 #x82) (#xF4 #x90 #x80 #x80)
                   (#x61 #xFF) (#xF0 #x9F #x98)))
    (let ((octets (coerce codes '(simple-array (unsigned-byte 8) (*)))))
      (is (equal (handler-case (sb-ext:octets-to-string octets :external-format :utf-8) (error () :refused))
                 (handler-case (nlk::utf-8-text octets) (error () :refused)))
          (format nil "~s" codes)))))

(deftest utf-8-size-counts-the-bytes-utf-8-octets-spells ()
  ;; The rules files' budget counts this (proofs/ContextBudget.lean): one
  ;; byte to four a character, never the octets spelled out.
  (dolist (text (list "" "plain" (format nil "a~%b") "é — ü" "日本語" (string (code-char #x1F600))
                      (coerce (list (code-char #xFEFF) #\a) 'string) (string (code-char #x10FFFF))
                      (coerce "base" 'base-string)))
    (is (= (length (nlk:utf-8-octets text)) (nlk:utf-8-size text)) (format nil "~s" text))))

(deftest read-text-reads-utf-8-within-a-limit (with-temp-workspace (dir))
  (let ((path (write-temp-file (merge-pathnames "utf8.txt" dir) "héllo"))
        (broken (merge-pathnames "broken.txt" dir)))
    (alexandria:write-byte-vector-into-file (coerce '(#x61 #xFF) '(vector (unsigned-byte 8))) broken)
    (is (equal "héllo" (nlk:read-text path)))
    (is (equal "héllo" (nlk:read-text path 6)) "six bytes, at the limit")
    (is (null (nlk:read-text path 5)) "and past it")
    (is (null (nlk:read-text broken)) "not UTF-8")
    (is (null (nlk:read-text (merge-pathnames "absent.txt" dir))))))

(deftest a-ledger-line-makes-its-folder (with-temp-workspace (dir))
  (let ((path (merge-pathnames "deep/er/ledger.jsonl" dir)))
    (nlk:append-json-line path (nlk:json-object "n" 1))
    (nlk:append-json-line path (nlk:json-object "n" 2))
    (is (equal '(1 2) (mapcar (lambda (row) (nlk:json-value row :integer "n"))
                              (nlk:read-json-lines path))))))
