;;;; workspace-journal.lisp --- whether a workspace moved, from the kernel's word
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A workspace capture asks its workspace's journal whether anything in it
;;;; moved since the capture before. An unmoved workspace keeps that capture's
;;;; tree and no git starts: on Windows a git start costs some 9 ms and its
;;;; walk 4 more, and a tool step brackets its call with two captures, where
;;;; Linux pays 1-2 ms for both (T-442).
;;;;
;;;; The journal is the directory change feed Windows keeps for a handle:
;;;; ReadDirectoryChangesW over the whole tree, its completion queued on a
;;;; port of its own and polled -- never waited on -- by whichever thread
;;;; captures. The kernel queues a completion inside the file operation that
;;;; reports it, so a write whose call (or whose process) has returned is in
;;;; the queue before the capture after it looks; a port makes the read
;;;; independent of the thread that issued it, which may exit. What the feed
;;;; cannot vouch for -- an overflowed buffer, a failed read, a read it could
;;;; not re-arm -- counts as a change, and a journal that cannot re-arm is
;;;; given up: its workspace captures by walking again, as before.
;;;;
;;;; Changes under the workspace's own .git, and under the home's snapshot
;;;; storage when that sits inside it, are not the workspace's content: git
;;;; refreshes its index on every status, and a capture's own objects land in
;;;; the storage. Everything else counts, ignored files included: a change
;;;; the walk would leave out costs one walk, never a wrong tree.
;;;;
;;;; Elsewhere there is no journal (WORKSPACE-JOURNAL-EPOCH answers NIL) and
;;;; every capture walks: a walk there is a millisecond or two.

(in-package #:nodecode.kernel)

#+win32
(progn
  (sb-alien:define-alien-routine ("CreateFileW" %journal-open-directory) sb-win32::handle
    (name sb-win32::system-string) (access sb-win32::dword) (share sb-win32::dword)
    (security (* t)) (disposition sb-win32::dword) (attributes sb-win32::dword)
    (template sb-win32::handle))
  (sb-alien:define-alien-routine ("ReadDirectoryChangesW" %journal-read) sb-alien:int
    (handle sb-win32::handle) (buffer sb-sys:system-area-pointer) (length sb-win32::dword)
    (subtree sb-alien:int) (filter sb-win32::dword) (returned sb-sys:system-area-pointer)
    (overlapped sb-sys:system-area-pointer) (routine sb-sys:system-area-pointer))
  (sb-alien:define-alien-routine ("CreateIoCompletionPort" %journal-port) sb-win32::handle
    (file sb-win32::handle) (existing sb-win32::handle) (key (sb-alien:unsigned 64))
    (threads sb-win32::dword))
  (sb-alien:define-alien-routine ("GetQueuedCompletionStatus" %journal-dequeue) sb-alien:int
    (port sb-win32::handle) (bytes sb-sys:system-area-pointer) (key sb-sys:system-area-pointer)
    (overlapped sb-sys:system-area-pointer) (milliseconds sb-win32::dword))
  (sb-alien:define-alien-routine ("GetCurrentProcessId" %journal-process-id) sb-win32::dword)
  (sb-alien:define-alien-routine ("QueryPerformanceCounter" %journal-counter) sb-alien:int
    (count (* (sb-alien:unsigned 64)))))

(defconstant +journal-buffer-bytes+ 65536
  "One read's buffer: 64 KB, the most a network share takes.")

(defconstant +journal-filter+ #x5F
  "FILE_NOTIFY_CHANGE_ FILE_NAME DIR_NAME ATTRIBUTES SIZE LAST_WRITE CREATION.")

(defstruct (workspace-journal (:conc-name wsj-))
  "One workspace's change feed: its directory handle, the port its reads
complete on, the read's buffer and OVERLAPPED, and EPOCH, counted up at each
harvest that found a change."
  root ignored handle port buffer overlapped scratch
  (epoch 0) (lock (bt2:make-lock :name "workspace-journal")) process)

(define-side-table workspace-journals (:test #'equal :synchronized t)
  "Workspace root namestring -> its WORKSPACE-JOURNAL, or :NONE when it has
none. Live-only: the handles are this process's, dropped before a dump.")

(defvar *workspace-journals-lock* (bt2:make-lock :name "workspace-journals")
  "Held while a workspace's journal is looked up or opened: one per root.")

(defun drop-workspace-journals ()
  "Drop every journal: a dumped image must not inherit this process's handles."
  (clrhash *workspace-journals*))

(pushnew 'drop-workspace-journals sb-ext:*save-hooks*)

#+win32
(progn
  (defun journal-arm (journal)
    "Issue JOURNAL's next read. => true, or NIL when the read was refused."
    (let ((overlapped (wsj-overlapped journal)))
      (dotimes (i 32) (setf (sb-sys:sap-ref-8 overlapped i) 0))
      (/= 0 (%journal-read (wsj-handle journal) (wsj-buffer journal)
                           +journal-buffer-bytes+ 1 +journal-filter+
                           (sb-sys:int-sap 0) overlapped (sb-sys:int-sap 0)))))

  (defun journal-names (journal bytes)
    "The relative paths the BYTES of JOURNAL's buffer name."
    (let ((buffer (wsj-buffer journal)) (at 0) (names '()))
      (loop
        (let* ((next (sb-sys:sap-ref-32 buffer at))
               (length (min (sb-sys:sap-ref-32 buffer (+ at 8)) (- bytes at 12)))
               (octets (make-array (max 0 length) :element-type '(unsigned-byte 8))))
          (dotimes (i (length octets))
            (setf (aref octets i) (sb-sys:sap-ref-8 buffer (+ at 12 i))))
          (push (sb-ext:octets-to-string octets :external-format :utf-16le) names)
          (when (or (zerop next) (>= (+ at next) bytes))
            (return names))
          (incf at next)))))

  (defun open-journal (root ignored)
    "A journal armed over ROOT, or NIL when Windows gives none."
    (let ((handle (%journal-open-directory
                   (string-right-trim "\\/" (uiop:native-namestring root))
                   #x1                 ; FILE_LIST_DIRECTORY
                   7                   ; share read, write, delete
                   nil 3               ; OPEN_EXISTING
                   (logior #x02000000  ; FILE_FLAG_BACKUP_SEMANTICS
                           #x40000000) ; FILE_FLAG_OVERLAPPED
                   0)))
      (unless (= handle sb-win32::invalid-handle)
        (let ((port (%journal-port handle 0 1 1)))
          (if (zerop port)
              (progn (sb-win32::close-handle handle) nil)
              (flet ((foreign-block (bytes)
                       (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) bytes))))
                (let ((journal (make-workspace-journal
                                :root root :ignored ignored :handle handle :port port
                                :buffer (foreign-block +journal-buffer-bytes+)
                                :overlapped (foreign-block 32) :scratch (foreign-block 24)
                                :process (%journal-process-id))))
                  (if (journal-arm journal)
                      journal
                      (progn (close-journal journal) nil)))))))))

  (defun close-journal (journal)
    (ignore-errors (sb-win32::close-handle (wsj-handle journal)))
    (ignore-errors (sb-win32::close-handle (wsj-port journal))))

  (defun journal-ignored-p (journal name)
    "Whether NAME, a path relative to JOURNAL's root, lies under a folder whose
changes are not the workspace's content."
    (some (lambda (prefix)
            (and (>= (length name) (length prefix))
                 (string-equal prefix name :end2 (length prefix))
                 (or (= (length name) (length prefix))
                     (char= #\\ (char name (length prefix))))))
          (wsj-ignored journal)))

  (defun harvest-journal (journal)
    "Drain JOURNAL's completed reads, counting its epoch up when one named a
change that counts or could not say. => the epoch, or NIL once the journal
has failed for good."
    (let ((scratch (wsj-scratch journal)) (moved nil))
      (loop repeat 64
            do (dotimes (i 24) (setf (sb-sys:sap-ref-8 scratch i) 0))
               (let ((done (%journal-dequeue (wsj-port journal) scratch
                                             (sb-sys:sap+ scratch 8) (sb-sys:sap+ scratch 16) 0))
                     (bytes (sb-sys:sap-ref-32 scratch 0)))
                 (when (and (zerop done) (zerop (sb-sys:sap-ref-64 scratch 16)))
                   (return))
                 ;; A failed read or an overflow (no bytes) says nothing of
                 ;; what moved: it counts as a move.
                 (when (or (zerop done) (zerop bytes)
                           (notevery (lambda (name) (journal-ignored-p journal name))
                                     (journal-names journal bytes)))
                   (setf moved t))
                 (unless (journal-arm journal)
                   (return-from harvest-journal nil)))
            finally (setf moved t))
      (when moved (incf (wsj-epoch journal)))
      (wsj-epoch journal))))

(defun workspace-journal-epoch (root &key ignored)
  "The change epoch of the workspace at ROOT, a directory namestring: equal
epochs at two moments say nothing in it moved between them, IGNORED -- paths
relative to ROOT, a backslash between folders, or a function answering them,
called once when the journal opens -- aside. NIL where no journal can say,
and always off Windows."
  #-win32 (declare (ignore root ignored))
  #-win32 nil
  #+win32
  (let ((journal (root-journal root ignored)))
    (when (workspace-journal-p journal)
      (bt2:with-lock-held ((wsj-lock journal))
        (or (harvest-journal journal)
            (progn (close-journal journal)
                   (setf (workspace-journals root) :none)
                   nil))))))

(defun journal-clock ()
  "A count every process on this machine reads on one scale, rising: what
orders one process's arming of a journal against another's walk of the same
workspace (ADOPT-LAUNCH-TREE). NIL where no journal keeps a workspace."
  ;; QueryPerformanceCounter: one counter for every process since boot, never
  ;; stepped the way the wall clock is.
  #-win32 nil
  #+win32
  (sb-alien:with-alien ((count (sb-alien:unsigned 64)))
    (and (/= 0 (%journal-counter (sb-alien:addr count))) count)))

#+win32
(defun root-journal (root ignored)
  "ROOT's journal, opened on first use, or :NONE when Windows gives none."
  (bt2:with-lock-held (*workspace-journals-lock*)
    (let ((journal (workspace-journals root)))
      ;; A journal another process made -- an image restored from a dump that
      ;; outlived its save hook -- holds handles that are not this process's.
      (when (and (workspace-journal-p journal)
                 (/= (wsj-process journal) (%journal-process-id)))
        (setf journal nil))
      (or journal
          (setf (workspace-journals root)
                (or (open-journal root (list* ".git" (if (functionp ignored)
                                                         (funcall ignored)
                                                         ignored)))
                    :none))))))
