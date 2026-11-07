;;;; dave.lisp --- libdave, the only door to Discord voice media.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Discord refuses a voice client that does not speak DAVE: identify with
;;;; max_dave_protocol_version 0 and the voice gateway closes with 4017,
;;;; "E2EE/DAVE protocol required". There is no downgrade and no
;;;; presence-only corner. DAVE's key agreement is MLS (RFC 9420) under an
;;;; external sender, which is not a thing to reimplement: Discord publishes
;;;; libdave under MIT with a plain extern-C ABI and prebuilt, self-contained
;;;; shared objects for the five desktop targets, so this file is a binding
;;;; and nothing more.
;;;;
;;;; INSTALL-DAVE fetches the pinned archive into the cache the way the kit
;;;; fetches its speech engines; LOAD-DAVE opens it once per image. Neither
;;;; runs at load time: an organism with no voice channel configured never
;;;; touches the library, and one that is asked to join says plainly that the
;;;; library is missing and how to get it.
;;;;
;;;; One non-obvious call: DAVE-SESSION-CREATE passes NO authSessionId. That
;;;; string names a PERSISTED MLS signature key, and the shipped build is
;;;; compiled PERSISTENT_KEYS=OFF, so its GetPersistedKeyPair is a stub that
;;;; answers nothing and Session::InitLeafNode aborts. With the id absent
;;;; libdave takes the other branch and generates the signature key in
;;;; memory, which is the right lifetime anyway: the key belongs to one
;;;; call, not to the machine.

(in-package #:nodecode-channel-discord)

(nlk:define-startup-parameter *dave-directory*
    (nlk:cache-path "nodecode/libdave/")
  "Where INSTALL-DAVE puts libdave. Tests bind a scratch folder.")

;;; Each archive carries lib/ and include/ and links boringssl and mlspp
;;; statically, so nothing but a C++ runtime is asked of the machine.
(defparameter +dave-builds+
  '(((:linux :x64) "libdave-Linux-X64-boringssl"
     "cd77724d3fa90359f6430c294fcad7b8f7e248ec8e16db9031d7b6f622eb3451" "libdave.so")
    ((:linux :arm64) "libdave-Linux-ARM64-boringssl"
     "2c4396825d1d777b2f37e21dbd12fc9b4a73cb90f7cecfd9d30b2ce5b24d9a9b" "libdave.so")
    ((:darwin :arm64) "libdave-macOS-ARM64-boringssl"
     "3158ddc2af8e4def0f4c5a70c21ec6756e370f5b9191485c24c5f2fc5efa9749" "libdave.dylib")
    ((:darwin :x64) "libdave-macOS-X64-boringssl"
     "30c268218c8de74c72c12fc39a8b8d2de5c979a188bb65b23fef0da63a1b2655" "libdave.dylib")
    ((:win32 :x64) "libdave-Windows-X64-boringssl"
     "f4cade98328fce51ea6e17fcc3c7bd39447999a554dbf383f7f63b89f0bbd714" "dave.dll"))
  "((PLATFORM ARCHITECTURE) ARCHIVE SHA256 LIBRARY) of libdave v1.2.0/cpp.")

(defparameter +dave-url+
  "https://github.com/discord/libdave/releases/download/v1.2.0%2Fcpp/~a.zip")

(defun dave-library-path ()
  "Where this machine's libdave is once installed, or NIL when no build
exists for it. The path is answered whether or not the file is there."
  (nlk:bind (((archive _ library) (nck:machine-build +dave-builds+)))
    (when archive
      (merge-pathnames (format nil "~a/lib/~a" archive library) *dave-directory*))))

(defun dave-installed-p (&aux (path (dave-library-path)))
  (and path (probe-file path) t))

(defun install-dave ()
  "Fetch libdave into *DAVE-DIRECTORY* (about 4 MB), checked against its
pinned digest, once."
  ;; => a text naming where it is. Signals when this
  ;; machine has no build or the fetch fails.
  (multiple-value-bind (archive sha256) (nck:machine-build +dave-builds+)
    (unless archive
      (error "Discord's voice library is not built for ~(~a ~a~), and Discord ~
              refuses voice without it"
             (nlk:platform) (nlk:architecture)))
    (nck:install-pinned-archive archive sha256 +dave-url+ *dave-directory*
                                :kind :zip :into archive)
    (format nil "libdave is installed under ~a: Discord's DAVE/MLS ~
                 implementation (MIT), which its voice gateway requires"
            (uiop:native-namestring *dave-directory*))))

;;; Opening twice is harmless but the flag keeps the join path from stat-ing
;;; the cache on every utterance.
(defvar *dave-loaded* nil
  "T once this image has opened libdave.")

(defun load-dave ()
  "Open libdave into this image, installing it first when it is absent."
  ;; Signals with the honest reason when it cannot be had.
  (or *dave-loaded*
      (progn
        (unless (dave-installed-p) (install-dave))
        (unless (dave-installed-p)
          (error "libdave is not at ~a after installing it" (dave-library-path)))
        (sb-alien:load-shared-object (uiop:native-namestring (dave-library-path)))
        (setf *dave-loaded* t))))

;;; --- the C ABI -----------------------------------------------------------------
;;;
;;; Declared at compile time, called only after LOAD-DAVE: SB-ALIEN resolves
;;; a routine's symbol on the first call, so an image that never joins a
;;; voice channel never needs the library present.

(defmacro define-dave-routines (&body rows)
  "One SB-ALIEN:DEFINE-ALIEN-ROUTINE per ROW, (C-NAME LISP-NAME RESULT (ARG TYPE)...), each
TYPE one of :sap :string :size :u16 :u32 :int :bool :void."
  (flet ((alien (type)
           (ecase type
             (:sap 'sb-alien:system-area-pointer) (:string 'sb-alien:c-string)
             (:size 'sb-alien:unsigned-long) (:u16 'sb-alien:unsigned-short)
             (:u32 'sb-alien:unsigned-int) (:int 'sb-alien:int)
             ;; (boolean 8), not the default 32: a C bool is one octet, and
             ;; reading four of them reads whatever is beside it.
             (:bool '(sb-alien:boolean 8)) (:void 'sb-alien:void))))
    `(progn
       ,@(loop for (c-name name result . arguments) in rows
               collect `(sb-alien:define-alien-routine (,c-name ,name) ,(alien result)
                          ,@(loop for (argument type) in arguments
                                  collect (list argument (alien type))))))))

(define-dave-routines
  ("daveFree" %dave-free :void (pointer :sap))
  ("daveSessionCreate" %dave-session-create :sap
   (context :sap) (auth-session-id :string) (callback :sap) (user-data :sap))
  ("daveSessionDestroy" dave-session-destroy :void (session :sap))
  ("daveSessionInit" %dave-session-init :void
   (session :sap) (version :u16) (group-id :size) (self-user-id :string))
  ("daveSessionSetExternalSender" %dave-set-external-sender :void
   (session :sap) (bytes :sap) (length :size))
  ("daveSessionGetMarshalledKeyPackage" %dave-key-package :void
   (session :sap) (out :sap) (length :sap))
  ("daveSessionProcessProposals" %dave-process-proposals :void (session :sap) (proposals :sap)
   (length :size) (user-ids :sap) (user-ids-length :size) (out :sap) (out-length :sap))
  ("daveSessionProcessCommit" %dave-process-commit :sap (session :sap) (commit :sap) (length :size))
  ("daveSessionProcessWelcome" %dave-process-welcome :sap
   (session :sap) (welcome :sap) (length :size) (user-ids :sap) (user-ids-length :size))
  ("daveSessionGetKeyRatchet" %dave-key-ratchet :sap (session :sap) (user-id :string))
  ("daveCommitResultIsFailed" dave-commit-failed-p :bool (result :sap))
  ("daveCommitResultDestroy" dave-commit-destroy :void (result :sap))
  ("daveWelcomeResultDestroy" dave-welcome-destroy :void (result :sap))
  ("daveKeyRatchetDestroy" dave-ratchet-destroy :void (ratchet :sap))
  ("daveEncryptorCreate" dave-encryptor-create :sap)
  ("daveEncryptorDestroy" dave-encryptor-destroy :void (encryptor :sap))
  ("daveEncryptorSetKeyRatchet" dave-encryptor-set-ratchet :void (encryptor :sap) (ratchet :sap))
  ("daveEncryptorAssignSsrcToCodec" dave-encryptor-assign-codec :void
   (encryptor :sap) (ssrc :u32) (codec :int))
  ("daveEncryptorSetPassthroughMode" dave-encryptor-passthrough :void
   (encryptor :sap) (passthrough :bool))
  ("daveEncryptorGetMaxCiphertextByteSize" %dave-max-ciphertext :size
   (encryptor :sap) (media-type :int) (frame-size :size))
  ("daveEncryptorEncrypt" %dave-encrypt :int (encryptor :sap) (media-type :int) (ssrc :u32)
   (frame :sap) (frame-length :size) (out :sap) (out-capacity :size) (written :sap))
  ("daveDecryptorCreate" dave-decryptor-create :sap)
  ("daveDecryptorDestroy" dave-decryptor-destroy :void (decryptor :sap))
  ("daveDecryptorTransitionToKeyRatchet" dave-decryptor-arm :void
   (decryptor :sap) (ratchet :sap) (expiry :int))
  ("daveDecryptorGetMaxPlaintextByteSize" %dave-max-plaintext :size
   (decryptor :sap) (media-type :int) (frame-size :size))
  ("daveDecryptorDecrypt" %dave-decrypt :int (decryptor :sap) (media-type :int) (frame :sap)
   (frame-length :size) (out :sap) (out-capacity :size) (written :sap)))

;;; --- octets across the boundary -------------------------------------------------

(defparameter +dave-codec-opus+ 1)
(defparameter +dave-media-audio+ 0)

(defun null-handle-p (sap) (zerop (sb-sys:sap-int sap)))

(defmacro with-alien-octets ((pointer octets) &body body &aux (bytes (gensym "BYTES")))
  "POINTER is a system area pointer to OCTETS, pinned in place for BODY."
  ;; A simple octet vector is its own storage, so what C writes there lands in
  ;; OCTETS; any other sequence is copied into one first.
  `(let ((,bytes (coerce ,octets '(simple-array (unsigned-byte 8) (*)))))
     (sb-sys:with-pinned-objects (,bytes)
       (let ((,pointer (sb-sys:vector-sap ,bytes)))
         ,@body))))

(defun sap-octets (sap length &aux (octets (make-array length :element-type '(unsigned-byte 8))))
  (dotimes (index length octets)
    (setf (aref octets index) (sb-sys:sap-ref-8 sap index))))

(defmacro with-alien-out-buffer ((pointer length) &body body)
  "A uint8_t** and a size_t* valid for BODY."
  ;; The form answers the octets libdave wrote, or NIL when it wrote none; the
  ;; buffer it allocated is freed with its own allocator.
  (let ((out (gensym "OUT")) (len (gensym "LEN")))
    `(sb-alien:with-alien ((,out sb-alien:system-area-pointer (sb-sys:int-sap 0))
                           (,len (sb-alien:unsigned 64) 0))
       (let ((,pointer (sb-alien:alien-sap (sb-alien:addr ,out)))
             (,length (sb-alien:alien-sap (sb-alien:addr ,len))))
         ,@body)
       (when (and (plusp ,len) (not (null-handle-p ,out)))
         (prog1 (sap-octets ,out ,len) (%dave-free ,out))))))

(defmacro with-alien-strings ((pointer strings) &body body)
  "POINTER is a const char** over STRINGS for BODY."
  ;; The pointer array is a pinned vector of the C strings' addresses.
  (let ((aliens (gensym "ALIENS")) (array (gensym "ARRAY")))
    `(let* ((,aliens (mapcar (lambda (string) (sb-alien:make-alien-string (dave-text string)))
                             ,strings))
            (,array (map '(simple-array (unsigned-byte 64) (*))
                         (lambda (alien) (sb-sys:sap-int (sb-alien:alien-sap alien))) ,aliens)))
       (nlk:with-cleanup ((mapc #'sb-alien:free-alien ,aliens))
         (sb-sys:with-pinned-objects (,array)
           (let ((,pointer (sb-sys:vector-sap ,array)))
             ,@body))))))

(defun dave-text (string)
  "STRING as a SIMPLE-STRING: SB-ALIEN's c-string binding refuses anything
else, and every id here came out of a decoded JSON object."
  (coerce string 'simple-string))

;;; --- the session, as this adapter uses it ----------------------------------------

(defun dave-session-create ()
  "A DAVE session with an in-memory signature key."
  ;; See the file header for why no authSessionId is passed.
  (%dave-session-create (sb-sys:int-sap 0) nil (sb-sys:int-sap 0) (sb-sys:int-sap 0)))

(defun dave-session-init (session version group-id self-user-id)
  (%dave-session-init session version group-id (dave-text self-user-id)))

(defun dave-session-set-external-sender (session octets)
  (with-alien-octets (pointer octets)
    (%dave-set-external-sender session pointer (length octets))))

(defun dave-session-key-package (session)
  (with-alien-out-buffer (out length) (%dave-key-package session out length)))

(defun dave-session-process-proposals (session octets user-ids)
  "The commit/welcome libdave answers with, or NIL when the proposals ask
nothing of this member."
  (with-alien-octets (proposals octets)
    (with-alien-strings (ids user-ids)
      (with-alien-out-buffer (out length)
        (%dave-process-proposals session proposals (length octets)
                                 ids (length user-ids) out length)))))

(defun dave-session-process-commit (session octets)
  (with-alien-octets (commit octets)
    (%dave-process-commit session commit (length octets))))

(defun dave-session-process-welcome (session octets user-ids)
  (with-alien-octets (welcome octets)
    (with-alien-strings (ids user-ids)
      (%dave-process-welcome session welcome (length octets) ids (length user-ids)))))

(defun dave-session-key-ratchet (session user-id)
  "That member's key ratchet, or NIL while they are not in the group."
  (let ((ratchet (%dave-key-ratchet session (dave-text user-id))))
    (unless (null-handle-p ratchet) ratchet)))

(defun dave-frame (routine capacity frame &rest leading
                   &aux (output (make-array (max 1 capacity) :element-type '(unsigned-byte 8))))
  "FRAME through libdave's ROUTINE, called with LEADING, the frame and its
length, a CAPACITY-byte output and a count out: the octets it wrote, or
(values NIL CODE) when it refuses."
  (with-alien-octets (input frame)
    (with-alien-octets (out output)
      (sb-alien:with-alien ((written (sb-alien:unsigned 64) 0))
        (let ((code (apply routine (append leading (list input (length frame) out capacity
                                                         (sb-alien:alien-sap
                                                          (sb-alien:addr written)))))))
          (if (zerop code) (subseq output 0 written) (values nil code)))))))

(defun dave-encrypt (encryptor ssrc frame)
  "FRAME encrypted for the group, or (values NIL CODE) when libdave refuses."
  (dave-frame #'%dave-encrypt (%dave-max-ciphertext encryptor +dave-media-audio+ (length frame))
              frame encryptor +dave-media-audio+ ssrc))

(defun dave-decrypt (decryptor frame)
  "FRAME opened, or (values NIL CODE) when it is not for us."
  (dave-frame #'%dave-decrypt (%dave-max-plaintext decryptor +dave-media-audio+ (length frame))
              frame decryptor +dave-media-audio+))
