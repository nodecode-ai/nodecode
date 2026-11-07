;;;; line.lisp --- the line: one WebSocket out to the relay, kept open.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The machine dials; nothing dials it. A socket is the one shape that
;;;; crosses the relay's edge frame by frame -- a streamed request body is
;;;; buffered there whole -- and one socket carries every
;;;; browser's requests at once, so a page's own socket and its fetches cost
;;;; no connection each. The first hello mints this machine's name and a
;;;; secret the relay keeps only the digest of; every later one presents them.
;;;;
;;;; One worker keeps it: dials when the link is on and there is no line,
;;;; after a wait that doubles with each failure up to a minute; pings every
;;;; 30 s, which the relay answers without waking; and drops a line it has
;;;; heard nothing on for 90 s, since a half-open socket reads as open
;;;; forever. A dropped line takes every flow with it (proxy.lisp), and a
;;;; failure stands on the board under one key until a welcome takes it down.
;;;; Why the last dial failed is kept in words -- nothing listens there, the
;;;; name is not found, the relay refused this machine -- never a condition's
;;;; printed self, and until a welcome the link reads as failing.

(in-package #:nodecode-link)

(defparameter *ping-seconds* 30)
(defparameter *silence-seconds* 90)
(defparameter +welcome-seconds+ 10
  "How long a dialled line may take to be welcomed.")

(nlk:define-record (line (:copier nil) (:predicate nil))
  "One socket to the relay."
  (socket nil)
  (state :hello)
  (heard 0)
  (pinged 0)
  (why nil)
  (writes (bt2:make-lock :name "link line writes")))

(nlk:access (line line))

(defvar *line* nil
  "The socket to the relay while there is one. Set under *LOCK*.")

(defvar *worker* nil
  "The worker that keeps the line.")

(defvar *failures* 0
  "Dials that failed since the last welcome.")

(defvar *next-dial* 0
  "The universal time the worker may dial again.")

(defvar *failure* nil
  "Why the last dial failed, in words, until a welcome: while it is there the
link is failing.")

(defvar *line-changed* (bt2:make-condition-variable :name "link line")
  "Notified when a line is welcomed or lost, for a /link waiting on it.")

(defparameter +notice-key+ "nodecode-link")

;;; --- writing -------------------------------------------------------------------------

(defun write-frame (line frame)
  "FRAME -- JSON text, or a data frame's octets -- as one message on LINE."
  ;; Composed and written here rather than through WSD:SEND, whose failure
  ;; path closes the connection from whichever thread wrote -- on TLS that is
  ;; SSL_free under the reader (NLK:SEVER-WEBSOCKET). A write that fails
  ;; signals to the flow that wrote it, and the line is severed once.
  (let ((octets (fast-websocket:compose-frame frame :masking t))
        (stream (wsd:socket line.socket)))
    (bt2:with-lock-held (line.writes)
      (handler-case (progn (write-sequence octets stream) (force-output stream))
        (error (condition)
          (sever line)
          (error "the line refused a write: ~a" condition))))))

(defun send-on-line (frame)
  (let ((line *line*))
    (unless (and line (eq line.state :up))
      (error "the line to the relay is down"))
    (write-frame line frame)))

(defun sever (line)
  (ignore-errors (nlk:sever-websocket line.socket :grace-seconds 1)))

;;; --- reading -------------------------------------------------------------------------

(defun on-welcome (line object)
  (nlk:with-json ((machine :text "machine") (origin :text "origin") (address :text "address")
                  (secret :text "secret"))
      object
    (if secret
        (keep "machine" machine "secret" secret "origin" origin "address" address)
        (keep "origin" origin "address" address))
    (with-link-lock
      (setf line.state :up *failures* 0 *failure* nil)
      (bt2:condition-broadcast *line-changed*))
    (nle:notice nil :key +notice-key+)))

(defun on-refused (line why)
  ;; A name the relay no longer holds, or a secret it will not take, will not
  ;; get better by asking again: this machine mints a fresh name, and the
  ;; operator is told the address moved.
  (when (member why '("unknown machine" "wrong secret") :test #'equal)
    (when (kept "machine")
      (nle:notice (format nil "link: the relay no longer knows ~a (~a); this machine takes a new address"
                          (kept "machine") why)
                  :level :warning))
    (keep "machine" nil "secret" nil "origin" nil "address" nil)
    (setf *next-dial* 0))
  (setf line.why why))

(defun on-line-message (line message)
  (setf line.heard (get-universal-time))
  (if (stringp message)
      (let ((object (ignore-errors (nlk:json-value (nlk:decode-json message) :object))))
        (nlk:with-json ((type :text "t") (id :integer "s")) object
          (cond ((equal type "welcome") (on-welcome line object))
                ((equal type "refused") (on-refused line (nlk:json-value object :string "why")))
                ((not (eq line.state :up)))
                ((equal type "open") (on-open object))
                ((equal type "end") (on-end id))
                ((equal type "reset") (on-reset id)))))
      (when (and (eq line.state :up) (> (length message) 4))
        (on-data message))))

(defun on-line-close (line)
  (let ((was (with-link-lock
               (prog1 line.state
                 (setf line.state :closed)
                 (when (eq *line* line) (setf *line* nil))
                 (bt2:condition-broadcast *line-changed*)))))
    (drop-flows)
    (when (eq was :up)
      ;; A line that was up and dropped is dialled again at once; one that
      ;; never came up waits its turn.
      (setf *next-dial* 0))
    (nlk:worker-poke *worker*)))

;;; --- keeping it ---------------------------------------------------------------------

(defun hello ()
  (let ((machine (kept "machine")) (secret (kept "secret")))
    (nlk:encode-json-object
     (nlk:json-object "t" "hello" "v" 1 :opt "machine" machine :opt "secret" secret))))

(defun line-url ()
  "The relay's line, its query naming this machine -- or, for a mint, what to
call it: the relay picks the machine's Durable Object from the address alone,
before a frame is read. The secret goes only in the hello."
  (let ((machine (kept "machine")))
    (format nil "~a?~a=~a" (setting :relay) (if machine "machine" "hint")
            (quri:url-encode (or machine (string-downcase (or (machine-instance) "machine")))))))

(define-condition line-refused (error)
  ((words :initarg :words :reader line-refused-words))
  (:report (lambda (condition stream) (write-string (line-refused-words condition) stream)))
  (:documentation "A dial the relay answered and did not welcome."))

(defun dial ()
  "Open a line and say hello; => T once it is welcomed."
  (let* ((socket (wsd:make-client (line-url)))
         (line (make-line :socket socket :heard (get-universal-time)
                          :pinged (get-universal-time))))
    (wsd:on :message socket (lambda (message) (on-line-message line message)))
    (wsd:on :close socket (lambda (&key code reason)
                            (declare (ignore code reason))
                            (on-line-close line)))
    (wsd:start-connection socket)
    (with-link-lock (setf *line* line))
    (write-frame line (hello))
    (let ((deadline (+ (get-universal-time) +welcome-seconds+)))
      (with-link-lock
        (loop until (or (member line.state '(:up :closed)) (>= (get-universal-time) deadline))
              do (bt2:condition-wait *line-changed* *lock* :timeout 1))))
    (unless (eq line.state :up)
      (sever line)
      (error 'line-refused :words (cond (line.why (format nil "the relay refused this machine: ~a" line.why))
                                        ((eq line.state :closed) "the relay closed the line before welcoming this machine")
                                        (t (format nil "the relay did not welcome this machine in ~d s"
                                                   +welcome-seconds+)))))
    t))

(defun failure-words (condition)
  "Why a dial failed, as the operator reads it."
  (typecase condition
    (line-refused (line-refused-words condition))
    ;; websocket-driver refuses a handshake it cannot take with a SIMPLE-ERROR
    ;; of its own words: something answered at the relay's address, and not
    ;; with a line.
    (simple-error "the relay's address answered, but not with a line")
    (t (nle:transport-failure-label condition (setting :relay)))))

(defun note-failure (condition)
  (let ((wait (min 60 (expt 2 (min 6 (incf *failures*))))))
    (setf *next-dial* (+ (get-universal-time) wait)
          *failure* (failure-words condition))
    ;; The first failure is often the network coming back; say it from the second.
    (when (>= *failures* 2)
      (nle:notice (format nil "link: no line to the relay at ~a — ~a; trying again in ~d s"
                          (setting :relay) *failure* wait)
                  :level :warning :key +notice-key+))))

(defun retry-now ()
  "Dial again at once, the wait the failures earned let go."
  (setf *next-dial* 0)
  (nlk:worker-poke *worker*))

;;; --- the account ------------------------------------------------------------------------
;;; The account's Machines page shows this machine's address once the machine tells it its
;;; name here (NLE:MACHINE-UPDATE, with the secret its link left in auth.json). A lap with the
;;; line up tells it whenever the name is not the one last told, or +REPORT-SECONDS+ have gone
;;; since: a home linked while the line was already up is told within a lap, a new name at once.

(defparameter +report-seconds+ 3600)
(defparameter +account-notice-key+ "nodecode-link-account")

(defvar *reported* nil
  "(NAME . WHEN) the account was last told, or NIL.")

(defvar *report-after* 0
  "No report before this universal time: a refused one waits ten minutes.")

(defun report-address (now &aux (machine (kept "machine")))
  "Tell the account this machine's name at the relay, when it was not told it lately."
  (when (and machine (>= now *report-after*)
             (not (and (equal machine (car *reported*))
                       (< (- now (cdr *reported*)) +report-seconds+))))
    (handler-case
        ;; By name: the gateway may run a binary older than this folder (an installed release
        ;; loads the home's cells), and a reader error would refuse the whole link; that binary
        ;; is named instead. NIL: this home was never linked, so there is no one to tell yet.
        (when (funcall (or (find-symbol "MACHINE-UPDATE" :nodecode.evolved)
                           (error "this nodecode is older than the link cell; update it"))
                       (nlk:make-json-object "relay" machine))
          (setf *reported* (cons machine now))
          (nle:notice nil :key +account-notice-key+))
      (error (condition)
        (setf *report-after* (+ now 600))
        (nle:notice (format nil "link: nodecode.ai was not told this machine's address: ~a" condition)
                    :level :warning :key +account-notice-key+)))))

(defun keep-line ()
  "One lap of the worker: dial, ping, or drop what went quiet; then tell the account."
  (let ((line *line*)
        (now (get-universal-time)))
    (cond
      ((not (kept "on"))
       (when line (sever line)))
      ((null line)
       (when (>= now *next-dial*)
         (handler-case (dial)
           (error (condition) (note-failure condition)))))
      ((> (- now line.heard) *silence-seconds*)
       (sever line))
      ((>= (- now line.pinged) *ping-seconds*)
       (setf line.pinged now)
       (ignore-errors (send-on-line "{\"t\":\"ping\"}"))))
    (when (and *line* (eq (line-state *line*) :up))
      (report-address now))))

(defun await-line (seconds)
  "Wait up to SECONDS for a welcomed line => T when there is one."
  (let ((deadline (+ (get-universal-time) seconds)))
    (with-link-lock
      (loop
        (when (and *line* (eq (line-state *line*) :up)) (return t))
        (when (>= (get-universal-time) deadline) (return nil))
        (bt2:condition-wait *line-changed* *lock* :timeout 1)))))

(defun start-line ()
  (unless *worker*
    (setf *worker* (nlk:worker-start "link-line" #'keep-line :wake 5 :prime t))))

(defun stop-line ()
  (setf *worker* (nlk:worker-stop *worker*))
  (nlk:when-let (line *line*) (sever line))
  (drop-flows))
