;;;; feed.lisp --- the advisories, and where they come from.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The feed is data, not code: a JSON file in this repository on `main',
;;;; which means a fix lands for every installed copy the moment it is
;;;; committed -- an installed cell is a git clone pinned to a commit and
;;;; nothing moves it, so anything baked into this folder would only reach
;;;; users who reinstall. The bundled copy beside this file is the floor: a
;;;; machine with no network, or a fetch that fails, still answers, and
;;;; FEED-STATUS says which copy answered and how old it is, so a stale
;;;; answer is never silent.

(in-package #:nodecode-cyber-security)

(defparameter *feed-url*
  "https://raw.githubusercontent.com/nodecode-ai/cells/main/tools/nodecode-cyber-security/advisories.json"
  "Where the live feed lives: the cells repository's main branch.")

(defparameter *feed-timeout* 15 "One fetch's ceiling, in seconds.")

(defun fetch-feed (url)
  "GET URL => (VALUES BODY STATUS); the cell's one network call."
  (nlk:http "GET" url :timeout *feed-timeout*
                      :headers '(("accept-encoding" . "identity"))))

(defparameter *fetch* 'fetch-feed
  "How the feed is fetched: (funcall *FETCH* URL) => (VALUES BODY STATUS); tests stub it.")

(defparameter *feed* nil "The entries answering now, newest feed load first.")

(defparameter *feed-state* (list :source :none :updated nil :error nil :fetched nil)
  "What answered: (:source :remote|:bundled|:none :updated ISO :error TEXT|NIL :fetched ISO).")

(defun packaged-path ()
  "The advisories.json shipped beside this file."
  (asdf:system-relative-pathname "nodecode-cyber-security" "advisories.json"))

(defun field (entry key &optional default)
  "ENTRY's KEY, a vector of values as a list, or DEFAULT."
  (let ((v (and (hash-table-p entry) (gethash key entry))))
    (if (and v (vectorp v) (not (stringp v))) (coerce v 'list) (or v default))))

(defun read-feed (text)
  "TEXT, the feed's JSON, as (VALUES ENTRIES UPDATED) -- or NIL when it is not a feed."
  (let ((obj (handler-case (shasht:read-json text) (error () nil))))
    (when (hash-table-p obj)
      (let ((entries (gethash "advisories" obj))
            (updated (gethash "updated" obj)))
        (when (and (vectorp entries) (plusp (length entries))
                   (every (lambda (e) (and (hash-table-p e) (gethash "id" e))) entries))
          (values (coerce entries 'list) updated))))))

(defun load-feed (&key (fetch t))
  "Answer the entries: the live feed when it answers, else the bundled copy.
Re-reads every call; the worker calls it on its own clock and reports what
answered. => a list of advisory hash tables."
  (multiple-value-bind (entries updated kind error)
      (if fetch
          (handler-case
              (multiple-value-bind (body status)
                  (funcall *fetch* *feed-url*)
                (if (= status 200)
                    (multiple-value-bind (e u) (read-feed body)
                      (if e (values e u :remote nil)
                          (values nil nil nil (format nil "the feed at ~a is not a feed" *feed-url*))))
                    (values nil nil nil (format nil "the feed answered HTTP ~a" status))))
            (error (e) (values nil nil nil (format nil "~a" e))))
          (values nil nil nil nil))
    (declare (ignore kind))
    (cond (entries
           ;; The live feed answered: it is the authority.
           (setf *feed* entries
                 *feed-state* (list :source :remote :updated updated :error nil
                                    :fetched (local-time-now))))
          ((eq (getf *feed-state* :source) :remote)
           ;; It answered before and not now: keep that copy, say so.
           (setf *feed-state* (list :source :stale :updated (getf *feed-state* :updated)
                                    :error error :fetched (getf *feed-state* :fetched))))
          (t
           ;; Never answered here (or not asked): the copy shipped beside us.
           (multiple-value-bind (bundled bundled-updated)
               (handler-case (read-feed (uiop:read-file-string (packaged-path)))
                 (error () nil))
             (setf *feed* bundled
                   *feed-state* (list :source (if bundled :bundled :none)
                                      :updated bundled-updated :error error
                                      :fetched (local-time-now))))))
    *feed*))

(defun local-time-now ()
  "Now, as an ISO-8601 UTC string."
  (format-timestamp))

(defun format-timestamp ()
  (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h m s)))

(defun advisories (&key (fetch nil))
  "Every advisory the feed carries (loading it first, when FETCH or nothing is loaded)."
  (when (or fetch (null *feed*))
    (load-feed :fetch fetch))
  *feed*)

(defun advisory (id)
  "The one advisory named ID, or NIL."
  (find id (advisories) :test #'string-equal :key (lambda (a) (gethash "id" a))))

(defun refresh ()
  "Re-read the live feed now, and answer what answered."
  (load-feed :fetch t)
  *feed-state*)

(defun feed-status (&optional (state *feed-state*))
  "One line: which copy answered, when it was written, and why if it failed."
  (format nil "feed: ~a, written ~a, checked ~a~@[ (~a)~]"
          (case (getf state :source)
            (:remote "live")
            (:bundled (if (getf state :error) "bundled copy -- live fetch failed" "bundled copy"))
            (:stale "last good copy -- live fetch failed")
            (t "not loaded"))
          (or (getf state :updated) "unknown") (or (getf state :fetched) "never")
          (getf state :error)))
