;;;; cell.lisp --- the page, served from the gateway's own port.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; /web/ answers the files in page/ and nothing else: a name is one path
;;;; segment of the page's own alphabet, so no request reaches outside the
;;;; folder. The page carries no secret. The operator token arrives in the
;;;; link's fragment (#t=...), which a browser never sends to a server, and the
;;;; page hands it to the gateway in the handshake it already makes; a page of
;;;; another site cannot read it, and cannot open the socket either (the
;;;; gateway refuses a foreign Origin and any Host that is not loopback). The
;;;; page's own Cells switch cannot take the page away: this folder advises
;;;; the question the switch asks first (NLE::CELL-OFF-REFUSAL).

(defpackage #:nodecode-web
  (:use #:cl))

(in-package #:nodecode-web)

(nlk:define-peripheral web :not-running t)

(defparameter *page-directory*
  (asdf:system-relative-pathname "nodecode-web" "page/")
  "Where the page's files are.")

(defparameter *page-types*
  '(("html" . "text/html; charset=utf-8")
    ("js" . "text/javascript; charset=utf-8")
    ("css" . "text/css; charset=utf-8")
    ("svg" . "image/svg+xml")
    ("woff2" . "font/woff2"))
  "The media type of each kind of file the page has.")

(defparameter *page-headers*
  (list :cache-control "no-cache"
        :x-content-type-options "nosniff"
        :referrer-policy "no-referrer"
        :content-security-policy
        (format nil "default-src 'self'; connect-src 'self'; img-src 'self' data: blob:; ~
                     style-src 'self'; script-src 'self'; base-uri 'none'; form-action 'none'; ~
                     frame-ancestors 'none'"))
  "What every file of the page is sent with: never framed, never sniffed,
nothing loaded from anywhere but here.")

(defun page-file (name)
  "The page's file NAME with its media type, or NIL for a name it does not have."
  (when (and (plusp (length name))
             (every (lambda (char) (or (alphanumericp char) (find char "-_."))) name)
             (char/= #\. (char name 0)))
    (let* ((file (probe-file (merge-pathnames name *page-directory*)))
           (type (and file (cdr (assoc (pathname-type file) *page-types* :test #'string-equal)))))
      (and type (cons file type)))))

(defun serve-page (env)
  "GET /web/NAME: the page's file NAME, index.html for none."
  (let* ((name (subseq (getf env :path-info) (length "/web/")))
         (found (page-file (if (string= name "") "index.html" name))))
    (if (and found (member (getf env :request-method) '(:get :head)))
        (list 200 (list* :content-type (cdr found) *page-headers*) (car found))
        (list 404 '(:content-type "text/plain; charset=utf-8") '("not found")))))

(defun redirect-to-page (env)
  "GET /web: the page is a directory; the link's fragment rides the redirect."
  (declare (ignore env))
  (list 302 '(:location "/web/") '()))

(defun keep-the-page (next name &aux (own (nlk:system-cell "nodecode-web")))
  "Why this folder NAME stays on (NLE::CELL-OFF-REFUSAL): the page the
switch is on is served from here, so the page cannot turn it off."
  (if (and own (string= name (nlk:cell-name own)))
      "it serves this page, which would go with it; take it out of the cell folder by hand"
      (funcall next name)))

(nle:define-cell web
  (:section ("web")
    (:guide "nothing to set: `nodecode web' opens the page"))
  (:route "/web/" #'serve-page :public t)
  (:route "/web" #'redirect-to-page :public t)
  (:hook 'nle::cell-off-refusal #'keep-the-page))
