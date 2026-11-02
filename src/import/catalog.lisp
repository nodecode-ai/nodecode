;;;; catalog.lisp --- the credential vocabulary, from models.dev.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every harness that talks to a provider names it the same three ways:
;;;; by an id, by the environment variable it reads the key from, and by an
;;;; endpoint. models.dev publishes all three for every provider it
;;;; carries -- `env' (the variable names), `npm' (the SDK, which names the
;;;; wire) and `api' (the base URL) -- and this organism already keeps that
;;;; file whole under ~/.cache/nodecode/models.dev/api.json, which
;;;; NLE::MODELS-CATALOG-TABLE parses for the /models picker.
;;;;
;;;; So the import needs no provider table of its own. A foreign home that
;;;; says OPENROUTER_API_KEY, or `provider: deepseek', or
;;;; `base_url: https://api.z.ai/api/paas/v4', is answered from the same
;;;; catalog the picker lists from, and the day models.dev adds a provider
;;;; this reads it with nothing recompiled. What the catalog cannot answer
;;;; -- a private router, a local server -- still lands, as an explicit
;;;; providers.<id> entry carrying the endpoint the home named.

(in-package #:nodecode.import)

;;; --- the memo ---------------------------------------------------------------
;;; The catalog is a few megabytes and its provider list is walked once per
;;; plan, not once per fact: the two indexes below are built on first use
;;; and thrown away when the parsed root changes identity.

(defvar *vocabulary* nil
  "(ROOT ENV-TABLE BASE-TABLE) for the catalog root the indexes were built
from; rebuilt when the root is a different object.")

(defvar *providers-memo* nil
  "The catalog's providers without their models (CATALOG-PROVIDERS), as
NLE::READ-JSON-FILE-MEMO keeps a parse.")

(defvar *providers-probe* nil
  "When CATALOG-PROVIDERS last looked at the catalog file.")

(defun catalog-providers ()
  "The models.dev catalog's providers, each without its models: all a home's
read asks of the catalog -- a provider's env, api, npm and name."
  ;; Passed over, the models are most of the file; without passing them
  ;; over, the read would wait on the bulk of the data.
  (nle::read-json-file-memo '*providers-memo* nle::*models-catalog-path*
                            :max-bytes (* 8 1024 1024)
                            :probe '*providers-probe*
                            :probe-seconds nle::*models-catalog-probe-seconds*
                            :decoder (lambda (octets)
                                       (nlk:decode-json
                                        octets
                                        :keep (lambda (path)
                                                (or (/= (length path) 2)
                                                    (and (string/= (first path) "models")
                                                         (nle::catalog-member-p path))))))))

(defun catalog-normalize-base (url)
  "URL as a base for comparison: lowercased, trailing slashes off."
  (and (stringp url)
       (let ((text (string-right-trim "/" (string-downcase (string-trim " " url)))))
         (and (plusp (length text)) text))))

(defun vocabulary (&aux (root (catalog-providers)))
  "(ENV-TABLE BASE-TABLE) over the current catalog; both empty without one."
  (unless (and *vocabulary* (eq (first *vocabulary*) root))
    (let ((env-table (make-hash-table :test #'equal))
          (base-table (make-hash-table :test #'equal)))
      (loop for id being the hash-keys of (or root (make-hash-table)) using (hash-value entry)
            for api = (catalog-normalize-base (nlk:json-value entry :text "api"))
            when (and (stringp id) (hash-table-p entry))
              ;; First provider wins: two providers sharing a name
              ;; (a regional twin) would otherwise flip by hash order.
              do (loop for name across (or (nlk:json-value entry :array "env") #())
                       when (stringp name) do (alexandria:ensure-gethash name env-table id))
                 (when api (alexandria:ensure-gethash api base-table id)))
      (setf *vocabulary* (list root env-table base-table))))
  (rest *vocabulary*))

(defun provider-for-env (name)
  "The models.dev provider whose key NAME holds, or NIL."
  (and (stringp name) (gethash name (first (vocabulary)))))

(defun catalog-entry (id)
  "The catalog's providers.<ID> object without its models, or NIL."
  (nlk:json-value (catalog-providers) :object id))

(defun catalog-sdk (id &aux (entry (catalog-entry id)))
  "The wire ID's lane speaks, from its models.dev npm package, or NIL."
  (and entry (nle::catalog-npm-lane-name (nlk:json-value entry :text "npm"))))

(defun catalog-base (id &aux (entry (catalog-entry id)))
  (and entry (nlk:json-value entry :text "api")))

(defun catalog-model-id (id name &aux (models (nlk:json-value (nle::models-catalog-table) :object id "models")))
  "The id provider ID's catalog serves the model NAME by, or NIL: NAME itself,
or -- NAME one word, as a harness names a line of models (`opus', `sonnet[1m]')
-- the newest model a turn runs on whose id carries that word."
  ;; Release order is the one ranking the catalog carries, read as the setup
  ;; walk's suggestion (NLE::SETUP-SUGGESTED-MODEL) reads it: an undated model
  ;; is never the newest, and a tie goes to the id sorting first.
  (let ((word (ppcre:regex-replace "\\[[^]]*\\]\\z" (string-downcase name) "")))
    (cond ((null models) nil)
          ((gethash name models) name)
          ((and (plusp (length word)) (every #'alpha-char-p word))
           (nle::suggested-model id word)))))

(defun provider-id-text (text)
  "TEXT as a config key: a `custom:' or `openrouter/' prefix dropped,
lowercased, the characters a member name keeps."
  (let* ((raw (string-trim '(#\Space #\Tab) (or text "")))
         (raw (if (uiop:string-prefix-p "custom:" raw) (subseq raw 7) raw))
         (id (string-downcase
              (remove-if-not (lambda (ch) (or (alphanumericp ch) (member ch '(#\- #\_ #\.))))
                             raw))))
    (and (plusp (length id)) id)))
