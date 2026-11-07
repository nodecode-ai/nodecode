;;;; package.lisp --- the LINK package: what it keeps on disk, and how a frame
;;;; leaves on the line.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-link
  (:use #:cl)
  (:export #:status))

(in-package #:nodecode-link)

;;; The lock is over the state below and the line's table of streams, whichever
;;; thread asks: the line's reader, a stream's worker, a shell's /link.
(nlk:define-peripheral link :not-running t :lock "nodecode-link")

;;; --- what is kept ----------------------------------------------------------------
;;; One file, link/state.json under the home, 0600: whether the operator
;;; turned the link on, this machine's name at the relay and the secret that
;;; proves it, the address the relay said it answers at, and the browsers let
;;; in -- each by the SHA-256 of its cookie, never the cookie. It is a state
;;; entry of the home (organism/profile.lisp): a profile export, a clone and
;;; a backup all leave it where it is, since the secret is this machine's
;;; alone.

(defvar *state* nil
  "The kept state as a hash table while the cell runs. Guarded by *LOCK*.")

;;; Read once, at the start, on the thread that starts the cell: the line's
;;; reader and every flow's worker write the state too, and a home bound on
;;; the starting thread is not bound on theirs.
(defvar *state-file* nil
  "Where the state is kept, fixed when the cell starts.")

(defun state-file () (or *state-file* (nlk:home "link/state.json")))

(defun read-state ()
  "The kept state, or a fresh one when there is none yet."
  (or (ignore-errors (nlk:json-value (nlk:decode-json (nlk:read-text (state-file))) :object))
      (nlk:make-json-object "browsers" (vector))))

(defun write-state ()
  "Keep *STATE* (the caller holds the lock)."
  (nlk:write-file-atomically (state-file) (nlk:encode-json-object *state*)
                             :mode #o600 :directory-mode #o700))

(defun kept (key)
  (with-link-lock (and *state* (gethash key *state*))))

(defun keep (&rest pairs)
  "Set each KEY VALUE of PAIRS in the kept state and write it once."
  (with-link-lock
    (loop for (key value) on pairs by #'cddr
          do (if value (setf (gethash key *state*) value) (remhash key *state*)))
    (write-state)))

;;; --- the wire ----------------------------------------------------------------------
;;; Control frames are one JSON object each, as text; data frames are binary:
;;; u32 stream, u8 flags, payload of at most +PAYLOAD-MAX+ bytes. A WebSocket
;;; message longer than that goes as several frames, each but the last
;;; flagged MORE. The relay's edge takes one socket message of a megabyte at
;;; most, and a gateway frame can be longer than that.

(defconstant +payload-max+ 262144)
(defconstant +text+ 1)
(defconstant +more+ 2)

(defun data-frame (stream flags octets &key (start 0) (end (length octets)))
  "The data frame carrying OCTETS[START,END) on STREAM."
  (let ((frame (make-array (+ 5 (- end start)) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) (ldb (byte 8 24) stream) (aref frame 1) (ldb (byte 8 16) stream)
          (aref frame 2) (ldb (byte 8 8) stream) (aref frame 3) (ldb (byte 8 0) stream)
          (aref frame 4) flags)
    (replace frame octets :start1 5 :start2 start :end2 end)
    frame))

(defun frame-stream (frame)
  (logior (ash (aref frame 0) 24) (ash (aref frame 1) 16) (ash (aref frame 2) 8) (aref frame 3)))

(defun frame-flags (frame) (aref frame 4))

(defun frame-payload (frame) (subseq frame 5))

;;; A variable because a test reads what the line would have carried, and
;;; because it is read on every stream's worker thread (set, never bound).
(defvar *send* 'send-on-line
  "How a frame leaves: a function of one frame, a control object's JSON text
or a data frame's octets. The default writes it on the line (line.lisp).")

(defun send-control (type &rest pairs)
  "Send the control frame of TYPE with the JSON members PAIRS."
  (funcall *send* (nlk:encode-json-object (apply #'nlk:make-json-object "t" type pairs))))

(defun send-data (stream octets &key text)
  "Send OCTETS on STREAM: one HTTP body piece, or one whole WebSocket message
cut into frames of +PAYLOAD-MAX+ at most, all but the last flagged MORE."
  (let ((length (length octets)))
    (loop for start = 0 then end
          for end = (min length (+ start +payload-max+))
          do (funcall *send* (data-frame stream (logior (if text +text+ 0)
                                                        (if (< end length) +more+ 0))
                                         octets :start start :end end))
          while (< end length))))
