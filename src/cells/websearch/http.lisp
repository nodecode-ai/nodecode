;;;; http.lisp --- the cell's one network seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through HTTP, so a test stubs that one
;;;; name (the notify cell's LAUNCH pattern) and never touches the network.
;;;; The seam answers values, never signals, for an HTTP status: a 4xx/5xx is
;;;; a fact the caller formats, and only transport failure — DNS, TLS, a
;;;; timeout — propagates as an ERROR for WITH-REDACTED-ERRORS to redact.
;;;;
;;;; The fetch path streams: a page is read in 64 KiB chunks under a byte cap
;;;; and a wall clock, because dexador's READ-TIMEOUT is per read and a slow
;;;; drip could otherwise hold the turn worker for minutes. The pool is never
;;;; used: an aborted read on a pooled socket would poison the provider lanes'
;;;; connections (the provider.lisp listing GET takes the same posture).

(in-package #:nodecode-websearch)

(defparameter +user-agent+
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36 nodecode-websearch"
  "Browser-shaped, as pi-web-access sends: a bare library UA is refused by
enough sites to matter.")

;;; A page-shaped request carries them with the user-agent. Without these,
;;; endpoints that gate on fetch metadata would answer with a 202 challenge
;;; page or 403/406 instead of results.
(defparameter +browser-headers+
  '(("accept-language" . "en-US,en;q=0.9")
    ("sec-fetch-dest" . "document")
    ("sec-fetch-mode" . "navigate")
    ("sec-fetch-site" . "none")
    ("sec-fetch-user" . "?1")
    ("sec-ch-ua-mobile" . "?0")
    ("upgrade-insecure-requests" . "1"))
  "The fetch-metadata headers a browser sends with a page navigation.")

(defun browser-headers (accept &optional more)
  "The header alist a page-shaped request carries: the browser user-agent,
ACCEPT, the fetch-metadata set, then MORE (an alist) for the request's own."
  (append `(("user-agent" . ,+user-agent+)
            ("accept" . ,accept))
          +browser-headers+
          more))

(defmacro group (regex text)
  "The first register REGEX captures in TEXT, or NIL: a literal REGEX compiles once."
  `(cl-ppcre:register-groups-bind (value) (,regex ,text) value))

(defun http (method url &rest arguments &key headers &allow-other-keys)
  "The cell's one network call, NLK:HTTP asking for the body unencoded;
tests stub this name."
  (apply #'nlk:http method url :headers (append headers '(("accept-encoding" . "identity")))
         arguments))
