;;;; extract.lisp --- HTML to readable text, cl-ppcre only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; pi-web-access runs a page through Readability and Turndown. The serving
;;;; image carries no HTML parser, and vendoring one for a peripheral whose
;;;; whole point is two functions would be the tail wagging the dog, so this
;;;; is the regex reduction of that pipeline: drop what is never prose
;;;; (script, style, noscript, svg, head, comments), narrow to <main> or
;;;; <article> when the page names one, keep absolute links as `text (url)',
;;;; turn block closers into newlines, strip the rest, decode entities,
;;;; collapse. A JavaScript-rendered shell — little text, many scripts — is
;;;; reported as a third value; FETCH says what can read it.
;;;;
;;;; Every scanner is built once: a 5 MiB page is compiled-regex bound and
;;;; the patterns are constants -- a literal regex is compiled at load time by
;;;; cl-ppcre's own compiler macros, flags written inline as (?i) and (?s).

(in-package #:nodecode-websearch)

(defparameter *scope-scanners*
  (mapcar (lambda (tag)
            (cl-ppcre:create-scanner (format nil "<~a\\b[^>]*>(.*?)</~a\\s*>" tag tag)
                                     :case-insensitive-mode t :single-line-mode t))
          '("main" "article" "body"))
  "Narrowing, in preference order: the first tag a page has wins.")

(defparameter *link-scanner*
  (cl-ppcre:create-scanner
   "<a\\b[^>]*?\\bhref\\s*=\\s*[\"']?(https?://[^\"'\\s>]+)[\"']?[^>]*>(.*?)</a\\s*>"
   :case-insensitive-mode t :single-line-mode t))

(defparameter *block-break-scanner*
  (cl-ppcre:create-scanner
   "<br\\s*/?>|</(p|div|li|h[1-6]|tr|blockquote|section|pre|table|ul|ol|dd|dt)\\s*>"
   :case-insensitive-mode t))

(defparameter *inline-tag-scanner*
  (cl-ppcre:create-scanner
   "</?(a|b|i|u|s|em|strong|span|code|small|sup|sub|abbr|cite|q|mark|time|label|font|wbr)\\b[^>]*>"
   :case-insensitive-mode t)
  "Tags that wrap a run of text inside a line: they vanish, where a block
tag becomes a space, so `<b>M</b>.' stays `M.'.")

(defparameter *tag-scanner*
  (cl-ppcre:create-scanner "<[^>]+>" :single-line-mode t))

(defparameter +named-entities+
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("copy" . "©") ("reg" . "®") ("ndash" . "–") ("mdash" . "—")
    ("hellip" . "…") ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“") ("rdquo" . "”")
    ("laquo" . "«") ("raquo" . "»") ("middot" . "·") ("bull" . "•") ("trade" . "™")))

(defun decode-entities (text)
  "TEXT with the common named and every numeric entity decoded; an unknown
name survives as written."
  (cl-ppcre:regex-replace-all
   "&(#x[0-9a-fA-F]+|#[0-9]+|[a-zA-Z]+);" text
   (lambda (match name)
     (cond ((char= (char name 0) #\#)
            (let ((code (if (char-equal (char name 1) #\x)
                            (parse-integer name :start 2 :radix 16 :junk-allowed t)
                            (parse-integer name :start 1 :junk-allowed t))))
              (if (and code (< 0 code char-code-limit))
                  (string (code-char code))
                  match)))
           (t (or (cdr (assoc name +named-entities+ :test #'string-equal))
                  match))))
   :simple-calls t))

(defun strip-controls (text)
  "TEXT trimmed (newline, return, space) and without C0 controls other than newline
and tab, so the eval snippet's sanitizer has nothing to remove and nothing to announce."
  (remove-if (lambda (char &aux (code (char-code char)))
               (and (< code 32) (not (member code '(9 10)))))
             (string-trim '(#\Newline #\Return #\Space) text)))

(defun extract-text (html)
  "Readable text of HTML."
  ;; Answers (values TEXT TITLE SHELL-P): TITLE NIL when the page has none,
  ;; SHELL-P true when the page looks like a JavaScript-rendered shell — under
  ;; 500 chars of text with more than three scripts.
  (let* ((title (cl-ppcre:register-groups-bind (title)
                    ("(?is)<title[^>]*>(.*?)</title\\s*>" html)
                  (let ((clean (nlk:one-line (decode-entities
                                              (cl-ppcre:regex-replace-all *tag-scanner* title " ")))))
                    (and (plusp (length clean)) clean))))
         (scripts (/ (length (cl-ppcre:all-matches "(?i)<script\\b" html)) 2))
         (text (cl-ppcre:regex-replace-all "(?s)<!--.*?-->" html ""))
         (text (cl-ppcre:regex-replace-all
                "(?is)<(script|style|noscript|svg|head)\\b[^>]*>.*?</\\1\\s*>" text ""))
         ;; The inner HTML of the first of <main>, <article>, <body> it has.
         (text (dolist (scanner *scope-scanners* text)
                 (cl-ppcre:register-groups-bind (inner) (scanner text)
                   (return inner))))
         ;; An anchor as `text (href)'; markup-only text contributes nothing.
         (text (cl-ppcre:regex-replace-all
                *link-scanner* text
                (lambda (match href inner)
                  (declare (ignore match))
                  (let ((label (nlk:one-line (cl-ppcre:regex-replace-all *tag-scanner* inner " "))))
                    (if (plusp (length label))
                        (format nil "~a (~a)" label href)
                        "")))
                :simple-calls t))
         (text (cl-ppcre:regex-replace-all *block-break-scanner* text (string #\Newline)))
         (text (cl-ppcre:regex-replace-all *inline-tag-scanner* text ""))
         (text (cl-ppcre:regex-replace-all *tag-scanner* text " "))
         (text (strip-controls (decode-entities text)))
         ;; Blank runs collapsed, at most one empty line in a row, trimmed.
         (text (cl-ppcre:regex-replace-all "[ \\t]+" text " "))
         (text (cl-ppcre:regex-replace-all "[ \\t]*\\n[ \\t]*" text (string #\Newline)))
         (text (nlk:trimmed (cl-ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%")))))
    ;; Under 500 chars of text with more than 3 scripts: presumed a JavaScript shell.
    (values text title (and (< (length text) 500) (> scripts 3)))))
