;;;; images.lisp --- a saved heap: this organism frozen whole, kept on this machine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The folder is the self a person shares: cells under git, a generation
;;;; a manifest of it (generation.lisp), the warm image a cache built from it
;;;; (launch.lisp). A saved heap is the third kind beside them: the running
;;;; organism's own heap as it stands -- values, closures, a definition no
;;;; file holds -- frozen by `/snapshot' and booted again later in its
;;;; place. It adds to the folder and replaces nothing: the folder still
;;;; loads, keeps and shares exactly as before, and a launch with no restore
;;;; asked boots it as it always has.
;;;;
;;;; IMAGES-DIRECTORY/NAME/ (~/.nodecode/snapshots/NAME/) holds one heap: `nodecode', the executable, and
;;;; manifest.json -- the build that saved it, the generation and folder key
;;;; it stood at, when, how big. A heap holds whatever the organism held,
;;;; the secrets it read among them, so it never leaves this machine: the
;;;; directories are 0700, the manifest 0600, the home table marks snapshots/
;;;; as carried by nothing (profile.lisp +HOME-ENTRIES+: no clone, archive or
;;;; backup takes it), and nothing here sends one anywhere.
;;;;
;;;; A restore is one-shot. `nodecode snapshots restore NAME' leaves the
;;;; pointer snapshots/restore naming the heap, and the next launch that would
;;;; own the organism -- a serve, the gateway's unit, a shell that finds none
;;;; running -- takes the pointer and execs the heap in its own place before
;;;; any folder loads (HAND-OVER-TO-SAVED-IMAGE), the way a warm image's
;;;; pointer is followed. The pointer is gone before the exec, so a heap that
;;;; dies boots nothing twice: the launch after it loads the folder. An
;;;; organism already running is asked to exit once idle for it, as a
;;;; rollback is (gateway.lisp RESTART-WHEN-IDLE).
;;;;
;;;; A heap boots only on the build that saved it. Its definitions are that
;;;; build's, while the release the home runs may have moved since: booting
;;;; it would take the home back a release without anyone saying so. A heap
;;;; of another build is refused, naming both, and `nodecode update LABEL'
;;;; holds the home at that build first.

(in-package #:nodecode.evolved)

(defparameter +image-manifest-format+ 1
  "The saved-heap manifest's format; a reader passes over one of another.")

;;; --- where the heaps live ----------------------------------------------------

(defun images-directory ()
  "~/.nodecode/snapshots/: one directory a snapshot, and the restore pointer.
Read off the home now, so a profile and a test's scratch home both apply."
  (home "snapshots/"))

(defun image-name-refusal (name)
  "Why NAME cannot name a saved heap, or NIL: 1 to 64 ASCII letters, digits,
`.', `_' and `-', starting with a letter or digit -- a directory name on
every platform, and never the pointer's."
  (cond ((not (and (stringp name) (plusp (length name)))) "a snapshot needs a NAME")
        ((> (length name) 64) (format nil "~s is longer than 64 characters" name))
        ((not (or (digit-char-p (char name 0)) (char<= #\a (char-downcase (char name 0)) #\z)))
         (format nil "~s: a NAME starts with a letter or a digit" name))
        ((notevery (lambda (c) (or (digit-char-p c) (char<= #\a (char-downcase c) #\z) (find c "._-")))
                   name)
         (format nil "~s: a NAME is letters, digits, `.', `_' and `-'" name))
        ((string-equal name "restore") "`restore' names the pointer, not a snapshot")))

(defun saved-image-directory (name)
  (merge-pathnames (format nil "~a/" name) (images-directory)))

(defun saved-image-path (name)
  "The executable heap NAME boots from."
  (merge-pathnames "nodecode" (saved-image-directory name)))

(defun saved-image-dump-path (name)
  "Where a save writes NAME's heap before it is sealed: a save that dies
half-way never stands where a restore looks."
  (merge-pathnames "nodecode.saving" (saved-image-directory name)))

(defun saved-image-manifest-path (name)
  (merge-pathnames "manifest.json" (saved-image-directory name)))

(defun restore-pointer-path ()
  "The one-shot pointer `restore' leaves for the next owning launch."
  (merge-pathnames "restore" (images-directory)))

(defun ensure-private-directory (directory)
  "DIRECTORY made when missing and kept 0700 either way: it holds heaps
with this machine's secrets in them."
  (ensure-directories-exist directory :mode #o700)
  (nlk:set-file-mode directory #o700)
  directory)

;;; --- a save: the dump's path, then the seal ----------------------------------

(defun prepare-saved-image (name)
  "Make NAME's directory, private, and answer the path a save dumps the heap
to (SAVED-IMAGE-DUMP-PATH) as a native namestring; a half-written dump an
earlier save left is removed. Signals the name's refusal."
  (when-let (refusal (image-name-refusal name)) (error "~a" refusal))
  (ensure-private-directory (images-directory))
  (ensure-private-directory (saved-image-directory name))
  (let ((dump (saved-image-dump-path name)))
    (when (probe-file dump) (delete-file dump))
    (uiop:native-namestring dump)))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in)))

(defun seal-saved-image (name &key folder-key generation (build (effective-version)))
  "The save's last step, in the process that asked for it once the dump has
returned: the heap at SAVED-IMAGE-DUMP-PATH checked whole, made private and
moved where a restore looks, and its manifest written -- BUILD the build
whose definitions it holds, GENERATION and FOLDER-KEY what the folder stood
at. => the manifest, as written. Signals when the dump is missing or cut
short, leaving any heap NAME already had in place."
  (when-let (refusal (image-name-refusal name)) (error "~a" refusal))
  (let ((dump (saved-image-dump-path name))
        (image (saved-image-path name)))
    (unless (nlk::image-complete-p dump)
      (error "the heap saved as ~a is ~:[missing~;incomplete~]: nothing replaced"
             name (probe-file dump)))
    (nlk:set-file-mode dump #o700)
    (nlk::replace-file dump image)
    (let ((manifest (json-object "format" +image-manifest-format+
                                 "name" name
                                 "saved_at" (iso-now)
                                 "build" build
                                 "stamp" nlk::*image-stamp*
                                 "platform" (release-platform)
                                 "generation" (or generation :null)
                                 "folder_key" (or folder-key :null)
                                 "bytes" (file-octets image))))
      (write-file-atomically (saved-image-manifest-path name) (pretty-json manifest)
                             :mode #o600)
      manifest)))

;;; --- what is saved -----------------------------------------------------------

(defun saved-image-manifest (name)
  "NAME's manifest, or NIL when it has none this reader can read."
  (let ((manifest (ignore-errors (nlk::json-object-file (saved-image-manifest-path name)))))
    (and manifest (eql +image-manifest-format+ (json-value manifest :integer "format"))
         manifest)))

(defun saved-image-names ()
  "Every directory under IMAGES-DIRECTORY a NAME could name, sorted."
  (sort (loop for directory in (directory (merge-pathnames "*/" (images-directory)))
              for name = (car (last (pathname-directory directory)))
              when (and (stringp name) (null (image-name-refusal name))) collect name)
        #'string<))

(defun restore-pointer ()
  "The NAME the restore pointer holds, or NIL."
  (let ((path (restore-pointer-path)))
    (and (probe-file path)
         (let ((name (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (uiop:read-file-string path))))
           (and (plusp (length name)) name)))))

(defun saved-image-refusal (name &key (build (effective-version)))
  "Why heap NAME cannot boot in place of BUILD, the build this launch runs,
or NIL when it can."
  (let ((image (saved-image-path name))
        (manifest (saved-image-manifest name)))
    (or (image-name-refusal name)
        (cond ((not (probe-file image))
               (format nil "no snapshot named ~a is kept here (nodecode snapshots lists them)" name))
              ((not (nlk::image-complete-p image))
               (format nil "snapshot ~a is incomplete: taking it never finished" name))
              ((null manifest)
               (format nil "snapshot ~a has no manifest, so nothing says which build took it" name))
              ((not (equal (version-label (or (json-value manifest :string "build") ""))
                           (version-label build)))
               (let ((saved (json-value manifest :string "build")))
                 (format nil "snapshot ~a was taken by ~a and this home runs ~a: `nodecode update ~a' ~
                              holds the home at that build, then the restore boots"
                         name saved build (version-label saved))))))))

(defun megabytes (octets) (round octets (* 1024 1024)))

(defun saved-images-text (&aux (names (saved-image-names)) (pending (restore-pointer)))
  "The saved heaps in words, one line each: name, when, size, build, the
generation it stood at, and which one the next launch boots."
  (with-output-to-string (out)
    (if (null names)
        (format out "no snapshot in this home yet: /snapshot in a shell, or nodecode snapshot~%")
        (dolist (name names)
          (let ((manifest (saved-image-manifest name))
                (refusal (saved-image-refusal name)))
            (if manifest
                (format out "~a  ~a  ~d MB  ~a~@[  generation ~d~]~:[~;  <- the next launch boots it~]~@[  (~a)~]~%"
                        name (json-value manifest :string "saved_at")
                        (megabytes (or (json-value manifest :integer "bytes") 0))
                        (json-value manifest :string "build")
                        (json-value manifest :integer "generation")
                        (equal name pending)
                        (and refusal (not (search "was taken by" refusal)) refusal))
                (format out "~a  (~a)~%" name refusal)))))
    (when (and pending (not (member pending names :test #'equal)))
      (format out "the restore pointer names ~a, which is not kept here: the next launch runs fresh from files~%"
              pending))
    (format out "~a holds this organism's secrets: it stays on this machine~%"
            (home-abbreviated (namestring (images-directory))))))

(defun delete-saved-image (name)
  "Remove heap NAME, and the restore pointer when it names it. => words."
  (when-let (refusal (image-name-refusal name)) (error "~a" refusal))
  (let ((directory (saved-image-directory name)))
    (unless (probe-file directory)
      (error "no snapshot named ~a is kept here" name))
    ;; The organism running from a heap copies its runtime from that file at
    ;; its next save: the file stays while a process runs it.
    (when-let (pids (nlk::processes-running (saved-image-path name)))
      (error "snapshot ~a is running now (pid~p ~{~d~^, ~}): switch to fresh from files or another snapshot first"
             name (length pids) pids))
    (when (equal (restore-pointer) name)
      (ignore-errors (delete-file (restore-pointer-path))))
    (nlk:remove-tree (truename directory))
    (format nil "snapshot ~a deleted" name)))

(defun rename-saved-image (name new)
  "Keep heap NAME under NEW instead: its directory moved, its manifest and a
restore pointer naming it following. => words. Signals the refusal: either
name refused, NAME not kept, NEW already kept, or NAME running now -- the
organism running from a heap copies its runtime from that file at its next
save, and remembers it by the name it booted under."
  (when-let (refusal (or (image-name-refusal name) (image-name-refusal new))) (error "~a" refusal))
  (let ((directory (saved-image-directory name))
        (target (saved-image-directory new)))
    (unless (probe-file directory)
      (error "no snapshot named ~a is kept here" name))
    (when (equal name new)
      (return-from rename-saved-image (format nil "snapshot ~a keeps its name" name)))
    (when (probe-file target)
      (error "a snapshot named ~a is already kept here" new))
    (when-let (pids (nlk::processes-running (saved-image-path name)))
      (error "snapshot ~a is running now (pid~p ~{~d~^, ~}): switch to fresh from files or another snapshot first"
             name (length pids) pids))
    (flet ((bare (path) (string-right-trim "/\\" (uiop:native-namestring path))))
      (sb-posix:rename (bare directory) (bare target)))
    (let ((manifest (saved-image-manifest new)))
      (when manifest
        (setf (gethash "name" manifest) new)
        (write-file-atomically (saved-image-manifest-path new) (pretty-json manifest) :mode #o600)))
    (when (equal (restore-pointer) name)
      (write-file-atomically (restore-pointer-path) (format nil "~a~%" new) :mode #o600))
    (format nil "snapshot ~a renamed ~a" name new)))

;;; --- a restore: the pointer, and the launch that takes it --------------------

(defun request-image-restore (name)
  "Leave the one-shot pointer to heap NAME for the next launch that owns the
organism, once NAME can boot on this build. => words. Signals the refusal."
  (when-let (refusal (saved-image-refusal name)) (error "~a" refusal))
  (ensure-private-directory (images-directory))
  (write-file-atomically (restore-pointer-path) (format nil "~a~%" name) :mode #o600)
  (format nil "snapshot ~a is the next launch's" name))

(defun hand-over-to-saved-image (&optional (argv sb-ext:*posix-argv*))
  "The launch's restore, before any folder loads: when the restore pointer
names a heap, take the pointer -- one-shot, gone before the exec -- and exec
that heap in this process's place with ARGV. => never when the heap boots;
else words saying why not, or NIL when no restore is asked."
  ;; One stat when no restore is asked: every launch that owns the organism
  ;; passes here. A pointer this launch cannot honour -- another build, a
  ;; heap since deleted -- is said once and taken away, so the launch after
  ;; it does not trip over it again; the folder loads as it always has.
  (when (probe-file (restore-pointer-path))
    (let ((name (restore-pointer)))
      (ignore-errors (delete-file (restore-pointer-path)))
      (let ((refusal (if name (saved-image-refusal name) "the restore pointer names no snapshot")))
        (cond (refusal
               (format *error-output* "~&snapshot: not restored: ~a~%" refusal)
               refusal)
              (t
               (note-boot-phase "exec-saved-image" name)
               (format *error-output* "~&snapshot: booting ~a~%" name)
               (format nil "snapshot ~a: the kernel refused the exec: ~a"
                       name (nlk::exec-program (saved-image-path name) argv))))))))

;;; --- the organism's side: its routes -----------------------------------------

(defun restore-image (name &key (restart (and *gateway-handler* t)))
  "Heap NAME for this home: while this organism serves (RESTART, the
default), it switches to NAME in place once no turn is in flight
(SWITCH-IMAGE); else the restore pointer is left and the next launch boots
it. => words: the model's and the operator's (restore-image NAME)."
  (cond ((and restart *serving*) (switch-image name))
        (t (let ((text (request-image-restore name)))
             (when restart
               (restart-when-idle (format nil "snapshot ~a" name) :reason "a restore"))
             (format nil "~a~:[: the next launch boots it~;; this organism exits once no turn is in flight, ~
                          and the next launch boots it~]" text restart)))))

(define-gateway-route handle-gateway-images-http (env)
  "GET /api/gateway/images: the saved heaps in words (SAVED-IMAGES-TEXT)."
  (:route :get "/api/gateway/images" "images")
  ;; RUNNING names the heap this organism boots from, and when it was saved:
  ;; a shell that asked for a save waits for exactly that answer.
  (json-object "text" (saved-images-text)
               :opt "running" (getf nlk::*saved-image* :name)
               :opt "running_saved" (getf nlk::*saved-image* :saved)))

(define-gateway-route handle-gateway-image-save-http (env)
  "POST /api/gateway/images/save {name}: this organism's heap saved as NAME
(SAVE-IMAGE); the answer carries the sentence the verb prints."
  (:route :post "/api/gateway/images/save" "image-save")
  (let ((name (json-value (gateway-request-json env) :string "name")))
    (require-request (null (image-name-refusal name)) (or (image-name-refusal name) "save takes {\"name\": NAME}"))
    (json-object "text" (funcall 'save-image name))))

(define-gateway-route handle-gateway-image-restore-http (env)
  "POST /api/gateway/images/restore {name}: the pointer to heap NAME left,
this organism exiting once idle for the next launch to boot it."
  (:route :post "/api/gateway/images/restore" "image-restore")
  (let ((name (json-value (gateway-request-json env) :string "name")))
    (require-request (null (image-name-refusal name)) (or (image-name-refusal name) "restore takes {\"name\": NAME}"))
    (json-object "text" (restore-image name))))

(define-gateway-route handle-gateway-image-switch-http (env)
  "POST /api/gateway/images/switch {name}: this organism runs heap NAME in its
own place once idle -- no name, or null: the folder (SWITCH-IMAGE)."
  (:route :post "/api/gateway/images/switch" "image-switch")
  (let ((name (json-value (gateway-request-json env) :string "name")))
    (require-request (or (null name) (null (image-name-refusal name)))
                     (image-name-refusal name))
    (let ((refusal (switch-refusal name)))
      (require-request (null refusal) refusal))
    (json-object "text" (switch-image name))))

;;; --- the command line ---------------------------------------------------------

(defun image-request (port path name)
  "Ask the organism on PORT for PATH about heap NAME. => its sentence."
  (answer-text (gateway-http-session-request port :post path (json-object "name" name)
                                             (gateway-configured-operator-token)
                                             :connect-timeout 3 :read-timeout 600)))

(defun image-main (words)
  "`nodecode snapshot [NAME]' (WORDS (\"save\" [NAME])) takes one of the running
organism, under NAME or its date; `nodecode snapshots' (WORDS the verb's
own: list, restore NAME, delete NAME, rename NAME NEW) lists them, has the
next launch boot one, removes or renames one. Runs before any folder loads."
  (answer-and-quit
    (let ((verb (or (first words) "list"))
          (name (second words))
          (port (organism-port)))
      (unless (cond ((equal verb "list") (<= (length words) 1))
                    ((equal verb "save") (<= (length words) 2))
                    ((equal verb "rename") (= (length words) 3))
                    (t (= (length words) 2)))
        (error (if (equal verb "save")
                   "snapshot takes one NAME at most: nodecode snapshot [NAME]"
                   "snapshots takes restore NAME, delete NAME or rename NAME NEW: nodecode snapshots [restore NAME | delete NAME | rename NAME NEW]; bare, in a terminal, the list to pick from")))
      ;; A snapshot named by nobody is named by its date (DATED-IMAGE-NAME).
      (when (and (equal verb "save") (null name))
        (setf name (funcall 'dated-image-name)))
      (values
       (cond ((equal verb "list") (saved-images-text))
             ((equal verb "save")
              (unless port
                (error "no organism runs in this home to snapshot: start one (nodecode), then take it"))
              (image-request port "/api/gateway/images/save" name))
             ((equal verb "restore")
              (if port
                  (image-request port "/api/gateway/images/restore" name)
                  (restore-image name :restart nil)))
             ((equal verb "delete") (delete-saved-image name))
             ((equal verb "rename") (rename-saved-image name (third words)))
             (t (error "snapshots knows restore, delete and rename, not ~s" verb)))
       0 :output))))

(defun follow-restore-pointer ()
  "The owning launch's restore (HAND-OVER-TO-SAVED-IMAGE), when a restore is
asked and no organism already serves this home: a launch that will attach
leaves the pointer to the one that owns. => as HAND-OVER-TO-SAVED-IMAGE."
  (and (probe-file (restore-pointer-path))
       (null (organism-port))
       (hand-over-to-saved-image)))
