;;;; support.lisp --- prs test runner, the scripted GitHub, the fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; PR triage tests register into the SAME nodecode.test registry under a
;;;; PRS-CELL- name prefix; RUN-PRS-TESTS runs exactly that slice.
;;;;
;;;; The one network call is NODECODE-PRS::HTTP; WITH-GITHUB stubs that name
;;;; with a table of routes, so no test opens a socket, and records every call
;;;; with its JSON body decoded. The ingress is NODECODE-PRS::SUBMIT-PROMPT,
;;;; stubbed the same way (WITH-SUBMITS), so no test starts a turn. The watch
;;;; thread is never started: a test runs POLL-REPO by hand.

(in-package #:nodecode.test)

(define-test-slice "prs" "PRS-CELL-" :start prs:start-cell)

(defvar *prs-token-file* nil
  "A file holding a test token, made once per image.")

(defun prs-token-file ()
  (or *prs-token-file*
      (setf *prs-token-file*
            (uiop:with-temporary-file (:stream out :pathname file :keep t :prefix "prs-token")
              (write-string "ghp_test_token" out)
              (namestring file)))))

(defmacro with-prs ((&rest pairs) &body body)
  "Run BODY with the cell started (no watch thread) on PAIRS and a token."
  `(let ((nodecode-prs::*watch* nil))
     (with-cell-stop ((prs-start "token_file" (prs-token-file) ,@pairs))
       ,@body)))

;;; --- the scripted GitHub --------------------------------------------------------

(defvar *github-calls* '()
  "(:method :url :headers :body) of every call, newest first.")

(defun github-calls (&optional method (pattern ""))
  "The calls of METHOD (any, when NIL) whose URL matches PATTERN, oldest first."
  (remove-if-not (lambda (call) (and (or (null method) (eq method (getf call :method)))
                                     (ppcre:scan pattern (getf call :url))))
                 (reverse *github-calls*)))

(defun gh-headers (&rest pairs)
  (apply #'nlk:make-json-object pairs))

(defmacro with-github ((&rest routes) &body body)
  "Run BODY with NODECODE-PRS::HTTP answering from ROUTES, each (METHOD
URL-REGEX STATUS BODY [HEADERS]) evaluated, or (:ALL FORM) for a list of
them; the first match wins, and anything else is a 404."
  `(let ((*github-calls* '())
         (routes (append ,@(mapcar (lambda (route)
                                     (if (eq (first route) :all)
                                         (second route)
                                         `(list (list ,@route))))
                                   routes))))
     (with-stubbed-fdefinition (nodecode-prs::http (method url &key headers content &allow-other-keys)
                                 (push (list :method method :url url :headers headers
                                             :body (and content (nlk:decode-json content)))
                                       *github-calls*)
                                 (let ((route (find-if (lambda (route)
                                                         (and (eq (first route) method)
                                                              (ppcre:scan (second route) url)))
                                                       routes)))
                                   (if route
                                       (destructuring-bind (method pattern status body &optional headers) route
                                         (declare (ignore method pattern))
                                         (values (if (stringp body) body (nlk:encode-json-object body))
                                                 status (or headers (gh-headers))))
                                       (values "{\"message\":\"Not Found\"}" 404 (gh-headers)))))
       ,@body)))

;;; --- fixtures ------------------------------------------------------------------------

(defun gh-sha (n &optional (push 0))
  "A 40-hex head sha for PR N after PUSH pushes."
  (string-downcase (format nil "~40,'0x" (+ (* n 4096) push))))

(defun gh-pull (n &key (head (gh-sha n)) (author "alice") draft (labels '()) (state "open")
                       (title (format nil "Fix thing ~d" n)) (created (format nil "2026-09-2~dT10:00:00Z" n)))
  "PR N as GitHub's pulls endpoints answer it."
  (nlk:json-object "number" n "title" title "state" state "draft" draft
                   "user" (nlk:json-object "login" author) "author_association" "CONTRIBUTOR"
                   "head" (nlk:json-object "sha" head "label" (format nil "~a:fix-~d" author n))
                   "base" (nlk:json-object "ref" "main")
                   "labels" (map 'vector (lambda (name) (nlk:json-object "name" name)) labels)
                   "created_at" created "updated_at" created
                   "additions" 3 "deletions" 1 "mergeable_state" "clean"
                   "body" "Fixes the thing."
                   "html_url" (format nil "https://github.com/a/b/pull/~d" n)))

(defun gh-file (name &optional (patch "@@ -1 +1 @@\n-old\n+new"))
  (nlk:json-object "filename" name "status" "modified" "additions" 1 "deletions" 1 "patch" patch))

(defun gh-review (n rank &optional (head (gh-sha n)))
  "A review on PR N carrying the rank marker at HEAD."
  (nlk:json-object "body" (format nil "Reads well.~%~%<!-- nodecode-prs sha=~a rank=~a -->" head rank)
                   "html_url" (format nil "https://github.com/a/b/pull/~d#review" n)))

(defun gh-dossier-routes (n &key (files (vector (gh-file "src/a.lisp"))) (others #()))
  "What a dossier of PR N reads besides the PR itself: its files, its checks,
the open PRs it may overlap (OTHERS) and their files."
  (list (list :get (format nil "/pulls/~d/files" n) 200 files)
        (list :get "/check-runs" 200
              (nlk:json-object "check_runs" (vector (nlk:json-object "name" "lint" "status" "completed"
                                                                     "conclusion" "success"))))
        (list :get "sort=created" 200 others)))

(defvar *prs-submits* '()
  "(:session :prompt :command-id) the submit stub received, newest first.")

(defmacro with-submits ((&key (disposition :started)) &body body)
  "Run BODY with the ingress recording instead of starting turns."
  `(let ((*prs-submits* '()))
     (with-stubbed-fdefinition (nodecode-prs::submit-prompt (session prompt command-id source-id)
                                 (push (list :session session :prompt prompt :command-id command-id)
                                       *prs-submits*)
                                 ,disposition)
       ,@body)))
