;;;; pair.lisp --- who is let in: the browsers the operator allowed, and the
;;;; one that is asking.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A browser is allowed by the operator AT THE MACHINE: it asks, the relay's
;;;; page shows it a 6-digit code, the operator's shell shows the same code,
;;;; and `/link allow CODE' lets it in. It then holds a cookie of 32 random
;;;; bytes for this machine's origin alone (__Host-, Secure, HttpOnly), and
;;;; the machine keeps the cookie's SHA-256 and nothing that could be replayed.
;;;; One ask waits at a time, for two minutes; an address found or guessed
;;;; can ask, and that is all it can do.
;;;;
;;;; What the asking browser says about itself is untrusted, and the ask is
;;;; shown to the operator: its user agent is read down to a browser and a
;;;; system from a short list and never echoed, and the place is the relay's
;;;; reading of its address, kept to letters and punctuation. The ask is said
;;;; to the shells attached now and is never put on the standing board, which
;;;; is the model's prompt -- a code there is a code a model could type.

(in-package #:nodecode-link)

(defparameter +cookie+ "__Host-link"
  "The cookie an allowed browser holds.")

(defparameter *ask-seconds* 120
  "How long an ask waits for the operator.")

(defparameter +asks-per-address+ 3
  "How many asks one address may make in +ASK-WINDOW+ seconds.")

(defparameter +ask-window+ 600)

(defun hex (octets)
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun digest (text)
  "TEXT's SHA-256, in hex."
  (hex (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets text :external-format :utf-8))))

(defun now () (get-universal-time))

;;; --- the browsers let in --------------------------------------------------------

(defun browsers ()
  "The allowed browsers, each a hash table: id, hash, agent, place, allowed."
  (coerce (or (kept "browsers") #()) 'list))

(defun cookie-value (header)
  "The value of +COOKIE+ in a Cookie HEADER, or NIL."
  (loop for part in (uiop:split-string (or header "") :separator ";")
        for pair = (string-trim " " part)
        when (uiop:string-prefix-p (format nil "~a=" +cookie+) pair)
          return (subseq pair (1+ (length +cookie+)))))

(defun allowed-browser (cookie-header)
  "The allowed browser whose cookie COOKIE-HEADER carries, or NIL."
  (nlk:when-let (cookie (cookie-value cookie-header))
    (let ((hash (digest cookie)))
      (find hash (browsers) :key (lambda (browser) (gethash "hash" browser)) :test #'equal))))

(defun allow-browser (agent place)
  "Let a browser in: keep its cookie's digest => the cookie it is to hold."
  (let ((cookie (hex (nlk:random-bytes 32))))
    (with-link-lock
      (setf (gethash "browsers" *state*)
            (concatenate 'vector (coerce (or (gethash "browsers" *state*) #()) 'vector)
                         (vector (nlk:make-json-object
                                  "id" (subseq (hex (nlk:random-bytes 4)) 0 6)
                                  "hash" (digest cookie) "agent" agent "place" place
                                  "allowed" (nlk:iso-time (nlk:unix-now) :millis nil)))))
      (write-state))
    cookie))

(defun remove-browser (id)
  "Stop letting the browser ID in => T when there was one."
  (with-link-lock
    (let* ((all (coerce (or (gethash "browsers" *state*) #()) 'list))
           (left (remove id all :key (lambda (browser) (gethash "id" browser)) :test #'equal)))
      (when (< (length left) (length all))
        (setf (gethash "browsers" *state*) (coerce left 'vector))
        (write-state)
        t))))

;;; --- what a browser says it is --------------------------------------------------

(defparameter +agents+
  '(("Edg/" . "Edge") ("OPR/" . "Opera") ("SamsungBrowser" . "Samsung Internet")
    ("Firefox/" . "Firefox") ("FxiOS" . "Firefox") ("CriOS" . "Chrome")
    ("Chrome/" . "Chrome") ("Safari/" . "Safari"))
  "User-agent marks, most specific first, and the browser each one names.")

(defparameter +systems+
  '(("iPhone" . "iPhone") ("iPad" . "iPad") ("Android" . "Android") ("CrOS" . "ChromeOS")
    ("Mac OS X" . "Mac") ("Windows" . "Windows") ("Linux" . "Linux"))
  "User-agent marks for the system, most specific first.")

(defun agent-name (user-agent)
  "USER-AGENT as a browser on a system from the two short lists, never its text."
  (flet ((pick (table)
           (cdr (find-if (lambda (entry) (search (car entry) (or user-agent ""))) table))))
    (format nil "~a on ~a" (or (pick +agents+) "a browser") (or (pick +systems+) "an unknown system"))))

(defun place-name (place)
  "PLACE, the relay's reading of the address, kept to letters, digits, spaces
and a little punctuation, forty characters at most."
  (let ((kept (remove-if-not (lambda (char) (or (alpha-char-p char) (digit-char-p char)
                                                (find char " ,.-'")))
                             (or place ""))))
    (subseq kept 0 (min 40 (length kept)))))

;;; --- the one ask ------------------------------------------------------------------
;;; A plist, because it lives a few minutes and a reload must not orphan it:
;;; :id (what the asking page waits on), :code, :agent, :place, :ip,
;;; :expires, :state (:waiting, :allowed, :denied), :cookie once allowed.

(defvar *ask* nil
  "The ask waiting for the operator, or the last one answered. Guarded by *LOCK*.")

(defvar *asked* '()
  "(IP . TIME) for every ask in the window, newest first. Guarded by *LOCK*.")

(defvar *ask-changed* (bt2:make-condition-variable :name "link ask")
  "Notified whenever the ask is answered, so a waiting page hears at once.")

(defun ask-live-p (ask)
  (and ask (eq (getf ask :state) :waiting) (< (now) (getf ask :expires))))

(defun live-ask ()
  "A copy of the ask waiting for the operator, or NIL."
  (with-link-lock (and (ask-live-p *ask*) (copy-list *ask*))))

(defun code-text (code)
  "A code as it is read aloud: 482 913."
  (format nil "~a ~a" (subseq code 0 3) (subseq code 3)))

(defun start-ask (ip agent place)
  "Open an ask for a browser at IP => the ask, or a refusal as a string."
  (with-link-lock
    (let ((now (now)))
      (setf *asked* (remove-if (lambda (entry) (> (- now (cdr entry)) +ask-window+)) *asked*))
      (cond ((ask-live-p *ask*)
             (format nil "another browser is asking; try again in ~d s" (- (getf *ask* :expires) now)))
            ((>= (count ip *asked* :key #'car :test #'equal) +asks-per-address+)
             "too many asks from this address; try again later")
            (t
             (push (cons ip now) *asked*)
             (let ((code (format nil "~6,'0d" (mod (reduce (lambda (a b) (+ (* a 256) b))
                                                           (nlk:random-bytes 4))
                                                   1000000))))
               (setf *ask* (list :id (hex (nlk:random-bytes 16)) :code code
                                 :agent agent :place place :ip ip
                                 :expires (+ now *ask-seconds*) :state :waiting))))))))

(defun answer-ask (state &optional code)
  "Answer the waiting ask with STATE, :ALLOWED or :DENIED; allowing takes its
CODE, which must be the one the asking page shows. => the ask, or NIL."
  (let ((ask (with-link-lock (and (ask-live-p *ask*) *ask*))))
    (when (and ask (or (eq state :denied)
                       (equal (remove #\Space (or code "")) (getf ask :code))))
      ;; The cookie is minted here, in the shell's answer, and handed to the
      ;; page by its next wait: nothing else ever holds it.
      (let ((cookie (and (eq state :allowed) (allow-browser (getf ask :agent) (getf ask :place)))))
        (with-link-lock
          (when (eq ask *ask*)
            (setf (getf *ask* :state) state
                  (getf *ask* :cookie) cookie)
            (bt2:condition-broadcast *ask-changed*)
            *ask*))))))

(defun await-ask (id seconds)
  "Wait up to SECONDS for the ask ID to be answered => :ALLOWED (and the
cookie), :DENIED, :EXPIRED or :WAITING."
  (let ((deadline (+ (now) seconds)))
    (with-link-lock
      (loop
        (let ((ask *ask*))
          (cond ((not (equal id (getf ask :id))) (return :expired))
                ((eq (getf ask :state) :allowed)
                 ;; Handed over once: a second wait on the same id gets nothing.
                 (return (values :allowed (shiftf (getf *ask* :cookie) nil))))
                ((eq (getf ask :state) :denied) (return :denied))
                ((>= (now) (getf ask :expires)) (return :expired))
                ((>= (now) deadline) (return :waiting))))
        (bt2:condition-wait *ask-changed* *lock*
                            :timeout (max 1 (min (- deadline (now)) (- (getf *ask* :expires) (now)))))))))
