;;;; fetch.lisp --- one URL as bounded text, read on by offset.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; pi-web-access stores every fetched body under a responseId and hands the
;;;; model bounded slices of it, so a 200 KB page never lands in the context
;;;; whole. Same idea, one fewer noun: the URL is the id. The extracted text
;;;; of a page lives in an in-memory cache for an hour, and (web:fetch url
;;;; :offset N) slices it without a second download. The slice trailer spells
;;;; the exact next call, so reading on is a copy, not a composition.
;;;;
;;;; Four readers, and a chain. `http' is the local one: the page comes here
;;;; with a browser's header set and is reduced by EXTRACT-TEXT; a PDF goes
;;;; through the converter this box has (pdftotext, else python's pypdf);
;;;; other text types are verbatim; a page past fetch.max_bytes is read to
;;;; the cap and says so. What the local reader cannot read — a host that
;;;; answered 401/403/406/429, a JavaScript shell, a PDF with no converter, a
;;;; document that is not text — goes to the next reader: `firecrawl'
;;;; (api.firecrawl.dev/v2/scrape, keyless, renders JavaScript and parses
;;;; documents, a key only lifts its limits), then `exa' (the keyless MCP
;;;; endpoint's web_fetch_exa). Each reader that failed is a [note] under
;;;; the url, and the note names who answered; only every reader failing is
;;;; an error. Both hosted readers, and `jina' (r.jina.ai, never in the
;;;; chain, an operator's explicit choice), are third parties that then know
;;;; what was read: the note says so, every time. A missing page (404) is
;;;; missing: no reader is asked twice for it.
;;;;
;;;; THREAD RULE: FETCH runs on the turn worker of whichever session called
;;;; it; the cache is the one shared variable and every touch holds its lock.

(in-package #:nodecode-websearch)

(defparameter +page-capacity+ 64)
(defparameter +limit-floor+ 200)
(defparameter +jina-marker+ "Markdown Content:")
(defparameter +readers+ '("http" "firecrawl" "exa" "jina")
  "What :reader and fetch.reader may name.")
(defparameter +chain+ '("http" "firecrawl" "exa")
  "The readers a fetch walks, from the one named (or fetch.reader) on;
jina is never walked to.")
(defparameter +hosted-characters+ 60000
  "How much of a page a hosted reader is asked for: enough for the cache to
answer several slices, bounded so a book does not come down.")
(defparameter +pdf-max-bytes+ 33554432
  "The byte cap a PDF reads under, 32 MiB, when fetch.max_bytes is lower:
a PDF goes to a temporary file and the converter, never into a Lisp
string, and a cut PDF converts to nothing — where a cut HTML page still
reads. An arXiv paper with figures runs 5 to 15 MB.")

(defun pdf-type-p (content-type)
  "T when CONTENT-TYPE names a PDF."
  (and (stringp content-type) (cl:search "application/pdf" (string-downcase content-type)) t))

(nlk:define-record (page (:copier nil) (:predicate nil))
  (url "" :type string)
  (reader "http" :type string)
  (title "" :type string)
  (text "" :type string)
  (notes '() :type list)
  (fetched-at (get-universal-time) :type integer))

(defvar *pages* '()
  "Fetched pages, newest first. A DEFVAR: a reload must not drop the cache.")

(defvar *pages-lock* (bt2:make-lock :name "websearch-pages"))

(nlk:define-error unreadable (error) (why (title :initform nil) (text :initform nil))
  (:report "~a" why)
  (:documentation "A reader could not read the page: WHY says so, and TEXT
holds what little it got (a JavaScript shell's), kept as the last resort
when no later reader answers."))

(defun unreadable (why &key title text)
  (error 'unreadable :why why :title title :text text))

;;; --- what else can read a page -------------------------------------------------

(defun browser-hint ()
  "How a browser reads a page no reader could, or NIL when no browser
cell is loaded: the chrome cell's package is in the image exactly when
its folder is installed."
  (and (find-package "NODECODE-CHROME")
       "the chrome cell reads it as a browser: (chrome:navigate url) then (chrome:snapshot)"))

;;; --- pdf -------------------------------------------------------------------------

(defparameter +pypdf-script+
  "import sys, pypdf
print('\\n'.join((page.extract_text() or '') for page in pypdf.PdfReader(sys.argv[1]).pages))"
  "The python that prints a PDF's text, when pypdf is importable.")

(defun pdf-converter (path)
  "The argv that prints the text of the PDF at PATH on stdout, or NIL when
this box has neither pdftotext (poppler) nor python3 with pypdf."
  (cond ((nlk:exits-zero-p '("sh" "-c" "command -v pdftotext >/dev/null 2>&1"))
         (list "pdftotext" "-layout" path "-"))
        ((nlk:exits-zero-p '("python3" "-c" "import pypdf"))
         (list "python3" "-c" +pypdf-script+ path))
        (t nil)))

(defun convert-pdf (octets)
  "OCTETS, a PDF, as text through the converter this box has."
  ;; => (values
  ;; TEXT NIL), or (values NIL WHY) when there is no converter or it produced
  ;; nothing.
  (uiop:with-temporary-file (:stream stream :pathname path :type "pdf"
                             :element-type '(unsigned-byte 8) :direction :output)
    (write-sequence octets stream)
    :close-stream
    (let ((argv (pdf-converter (uiop:native-namestring path))))
      (if (null argv)
          (values nil "no PDF converter on this box (poppler's pdftotext, or python's pypdf)")
          (let ((text (ignore-errors (nlk:run-bounded argv :error-output nil))))
            (if (and (stringp text) (plusp (length (string-trim '(#\Space #\Newline) text))))
                (values (strip-controls text) nil)
                (values nil (format nil "~a produced no text" (first argv)))))))))

;;; --- the local reader --------------------------------------------------------------

(defun read-http (url settings)
  "URL fetched here with the browser header set and reduced locally."
  ;; =>
  ;; (values TITLE TEXT), or an UNREADABLE naming what the next reader may
  ;; manage: a gate status, a JavaScript shell (its little text kept), a PDF
  ;; this box cannot convert, a document that is not text. A 404 is a plain
  ;; failure: the page is missing.
  (flet ((cap-for (headers)
           ;; The cap depends on what comes: a PDF reads further, it never becomes a string.
           (if (pdf-type-p (and (hash-table-p headers) (gethash "content-type" headers)))
               (max +pdf-max-bytes+ (getf settings :max-bytes))
               (getf settings :max-bytes))))
    (multiple-value-bind (octets status headers truncated)
        (http :get url
              :headers (browser-headers "text/html,application/xhtml+xml,application/xml;q=0.9,application/pdf;q=0.8,text/plain;q=0.8,*/*;q=0.5")
              :timeout 30
              :max-bytes #'cap-for)
      (unless (eql status 200)
        (if (member status '(401 403 406 429))
            (unreadable (format nil "HTTP ~a" status))
            (fail "HTTP ~a fetching ~a" status url)))
      (let* ((cut (and truncated
                       (format nil "[cut at ~:d bytes (fetch.max_bytes): the page is longer]"
                               (cap-for headers))))
             (content-type (and (hash-table-p headers) (gethash "content-type" headers)))
             (type (string-downcase (or content-type "")))
             ;; Untyped: HTML when its first KiB looks like a document.
             (head (string-downcase
                    (nlk:decode-octets (subseq octets 0 (min 1024 (length octets)))))))
        (flet ((done (title text)
                 (values title (if cut (format nil "~a~%~%~a" text cut) text))))
          (cond ((if (null content-type)
                     (or (cl:search "<html" head) (cl:search "<!doctype" head))
                     (or (cl:search "text/html" type) (cl:search "application/xhtml" type)))
                 (multiple-value-bind (text title shell)
                     (extract-text (nlk:decode-octets octets content-type))
                   (if shell
                       (unreadable "JavaScript-rendered, little text reached here"
                                   :title (or title "(untitled)") :text text)
                       (done (or title "(untitled)") text))))
                ((or (pdf-type-p content-type)
                     (and (uiop:string-prefix-p "%pdf-" head)
                          (or (null content-type) (cl:search "octet-stream" type))))
                 (multiple-value-bind (text why) (convert-pdf octets)
                   (if text
                       (done "(pdf)" text)
                       (unreadable (format nil "a PDF, ~a~:[~;, and only its first ~:d bytes came~]"
                                           why truncated (cap-for headers))))))
                ((or (null content-type)
                     (uiop:string-prefix-p "text/" type)
                     (cl-ppcre:scan "application/(json|xml|javascript)|\\+(json|xml)" type))
                 (done "(text)" (string-trim '(#\Newline #\Return #\Space)
                                             (nlk:decode-octets octets content-type))))
                (t (unreadable (format nil "~a, not text" content-type)))))))))

;;; --- the hosted readers -----------------------------------------------------------

(defun read-firecrawl (url settings)
  "URL through Firecrawl's scrape: markdown, JavaScript rendered, documents
parsed."
  ;; => (values TITLE TEXT); the page's own status past 399, or no
  ;; markdown, is a plain failure.
  (let ((key (cdr (assoc "firecrawl" (getf settings :keys) :test #'string=))))
    (let* ((object (decode-json
                    (http-text :post "https://api.firecrawl.dev/v2/scrape"
                               :headers (json-headers key)
                               :content (nlk:json-object "url" url "formats" (vector "markdown"))
                               :timeout 40)))
           (page-status (nlk:json-value object :integer "data" "metadata" "statusCode"))
           (markdown (nlk:json-value object :string "data" "markdown")))
      (when (eq (nlk:json-value object :boolean "success") nil)
        (fail "~a" (or (nlk:json-value object :text "error") "firecrawl request failed")))
      (when (and page-status (>= page-status 400))
        (fail "HTTP ~a at the page" page-status))
      (unless (and (stringp markdown) (plusp (length (string-trim '(#\Space #\Newline) markdown))))
        (fail "no text came back"))
      (values (or (nlk:json-value object :text "data" "metadata" "title") "(untitled)")
              (strip-controls markdown)))))

(defun read-exa (url settings)
  "URL through Exa's keyless web_fetch_exa: the tool's text begins with a
`# Title' line and `URL:' / `Published:' / `Author:' lines, then the page."
  ;; => (values TITLE TEXT).
  (declare (ignore settings))
  (let ((text (exa-mcp-text (exa-mcp "web_fetch_exa"
                                     (nlk:json-object "urls" (vector url)
                                                      "maxCharacters" +hosted-characters+)
                                     40))))
    (unless (plusp (length (string-trim '(#\Space #\Newline) text)))
      (fail "no text came back"))
    (let* ((lines (nlk:lines text))
           (title (let ((first (first lines)))
                    (and first (uiop:string-prefix-p "# " first)
                         (nlk:one-line (subseq first 2)))))
           ;; The header block ends at the first blank line.
           (blank (position "" lines :test #'string=))
           (body-lines (if (and title blank) (nthcdr (1+ blank) lines) lines)))
      (values (or title "(untitled)")
              (strip-controls (format nil "~{~a~^~%~}" body-lines))))))

(defun read-jina (url settings)
  "URL rendered by r.jina.ai: the markdown after the `Markdown Content:'
marker, the title from the leading `Title:' line. => (values TITLE TEXT)."
  (nlk:bind (((octets status _ truncated)
              (http :get (concatenate 'string "https://r.jina.ai/" url)
                    :headers `(("accept" . "text/markdown")
                               ("user-agent" . ,+user-agent+))
                    :timeout 30
                    :max-bytes (getf settings :max-bytes))))
    (unless (eql status 200)
      (fail "HTTP ~a from the jina reader" status))
    (let* ((body (nlk:decode-octets octets))
           (marker (cl:search +jina-marker+ body))
           (title (nlk:one-line (group "(?m)^Title:[ \\t]*(.*)$" body)))
           (text (strip-controls (subseq body (if marker (+ marker (length +jina-marker+)) 0)))))
      (values (if (plusp (length title)) title "(untitled)")
              (if truncated
                  (format nil "~a~%~%[cut at ~:d bytes (fetch.max_bytes): the page is longer]"
                          text (getf settings :max-bytes))
                  text)))))

;;; --- the chain -------------------------------------------------------------------

(defun read-page (url reader settings &aux (chain (if (string= reader "http") +chain+ (list reader)))
                                           (notes '())
                                           (fallback nil))
  "URL as a PAGE, read by READER — or, from `http' on, by the first of the
chain that can: every reader that could not is a note on the page, the
last note naming who answered."
  ;; A JavaScript shell's little text is kept when no later reader does
  ;; better. Every reader failing is one error naming each; a missing page
  ;; never reaches a second reader.
  (dolist (name chain)
    (multiple-value-bind (title text why)
        ;; A reader answers (values TITLE TEXT), so WHY reads NIL.
        (handler-case
            (cond ((string= name "http") (read-http url settings))
                  ((string= name "firecrawl") (read-firecrawl url settings))
                  ((string= name "exa") (read-exa url settings))
                  (t (read-jina url settings)))
          (nlk:turn-cancelled-condition (condition) (error condition))
          (unreadable (condition)
            (when (unreadable-text condition)
              (setf fallback (list (unreadable-title condition) (unreadable-text condition))))
            (values nil nil (unreadable-why condition)))
          ;; A reader's own refusal -- a missing page, a failed request -- and any other error.
          (error (condition)
            (values nil nil (redact (nlk:one-line (princ-to-string condition) :cap 200)))))
      (cond (why
             ;; A missing page is missing; nothing further reads it.
             (when (and (string= name "http") (cl:search "HTTP 404" why))
               (fail "~a" why))
             (setf notes (append notes (list (format nil "~a: ~a" name why)))))
            (t
             (return-from read-page
               (make-page :url url :reader reader :title title :text text
                          :notes (if notes
                                     (append (butlast notes)
                                             (list (format nil "~a; read by ~a" (car (last notes)) name)))
                                     '())))))))
  (cond (fallback
         (make-page :url url :reader reader :title (first fallback) :text (second fallback)
                    :notes (append notes (list (format nil "the local reader's text is what there is~@[; ~a~]"
                                                       (browser-hint))))))
        (t
         (fail "no reader could read ~a: ~{~a~^; ~}~@[; ~a~]" url notes (browser-hint)))))

;;; --- the cache ----------------------------------------------------------------

(defun cached-page (url reader settings)
  "The PAGE at URL as READER reads it: the live cache entry (an expired one
dropped), else read now and cached, the oldest evicted past capacity."
  (unless (stringp url)
    (fail "url must be a string"))
  (unless (member reader +readers+ :test #'equal)
    (fail "unknown reader ~s: one of ~{~a~^, ~}" reader +readers+))
  (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
    (fail "only http(s) URLs are fetched, got ~s" url))
  (flet ((same-p (page) (and (string= page.url url) (string= page.reader reader))))
    (or (bt2:with-lock-held (*pages-lock*)
          (let ((page (find-if #'same-p *pages*)))
            (cond ((null page) nil)
                  ((> (- (get-universal-time) (page-fetched-at page)) 3600)
                   (setf *pages* (remove page *pages*))
                   nil)
                  (t page))))
        (let ((page (read-page url reader settings)))
          (bt2:with-lock-held (*pages-lock*)
            (setf *pages* (cons page (remove-if #'same-p *pages*)))
            (when (> (length *pages*) +page-capacity+)
              (setf *pages* (subseq *pages* 0 +page-capacity+)))
            page)))))

;;; --- the verb ----------------------------------------------------------------

(defun fetch (url &key (offset 0) limit reader)
  "URL as readable text: title, url, then up to LIMIT characters (default
fetch.limit, 6000; at most 7000) starting at OFFSET."
  ;; The extracted page is cached for an hour, so a call with :OFFSET slices
  ;; without re-fetching; a cut result ends with `Showing A-B of N chars;
  ;; (web:fetch "url" :offset B) for the next slice'. READER (default
  ;; fetch.reader, http) is where reading starts: http reads locally — HTML to
  ;; text, a PDF converted, other text verbatim — and what it cannot read goes
  ;; to firecrawl, then exa, each failure a [note] under the url; "firecrawl",
  ;; "exa" or "jina" named reads with that one alone.
  ;; What a transcript calls this call (activity.lisp): a fetch of URL.
  (nle:receipt "calls" (list* :verb "web:fetch" :family "fetch"
                              (and (stringp url) (list :source url))))
  (with-redacted-errors
    (let* ((settings (running-settings))
           (reader (or reader (getf settings :reader)))
           ;; At most 7000: under EVAL's 8000 with room for the header and trailer.
           (limit (min (max (if (integerp limit) limit (getf settings :limit)) +limit-floor+)
                       7000)))
      (unless (and (integerp offset) (>= offset 0))
        (fail "offset must be a non-negative integer"))
      (let* ((page (cached-page url reader settings))
             (text page.text)
             (total (length text))
             (end (min (+ offset limit) total)))
        (when (and (plusp total) (>= offset total))
          (fail "offset ~d is past the end: the page has ~d chars" offset total))
        (format nil "~a~%~a~{~%[~a]~}~%~%~a~:[~;~%~%Showing ~d-~d of ~:d chars; ~
                     (web:fetch ~s :offset ~d) for the next slice~]"
                page.title page.url page.notes
                (if (plusp total) (subseq text offset end) "(no text)")
                (< end total) offset end total page.url end)))))
