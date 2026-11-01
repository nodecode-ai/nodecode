;;;; paths.lisp --- where the image finds its home and its own source.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every path the organism uses resolves when the process STARTS, never when
;;;; the file LOADS. A DEFPARAMETER whose initform calls USER-HOMEDIR-PATHNAME
;;;; is evaluated once, at build, and the dumped binary would carry the
;;;; builder's answer forever, so a saved core would read the builder's home
;;;; instead of the operator's. DEFINE-STARTUP-PARAMETER is the one shape for
;;;; such a value: a defparameter for the developer image, and a recorded
;;;; initform that SB-EXT:*INIT-HOOKS* re-evaluates the moment a saved core
;;;; starts — before MAIN, before any cell, before any read.
;;;;
;;;; Three roots, one rule each:
;;;;
;;;;   HOME-PATH   ~/<relative>, with $HOME read at call time: the operating
;;;;               system's home, where other harnesses keep theirs
;;;;               (~/.hermes, ~/.codex) and where the cache sits.
;;;;   HOME        <home>/<relative>: the organism's own folder — config,
;;;;               secrets, the store, the layer, memory, skills. One value
;;;;               of it is a PROFILE: ~/.nodecode/ by default, else the
;;;;               profile the command line, $NODECODE_HOME or the sticky
;;;;               file names (LOCATE-HOME). Two homes are two organisms:
;;;;               each has its own layer, its own store, its own gateway.
;;;;   SOURCE-ROOT the shipped source beside the executable
;;;;               (<dir-of-binary>/src/src/), falling back to the ASDF
;;;;               checkout the developer image loaded from. The binary ships
;;;;               WITH its source: it is what the organism reads to inspect
;;;;               itself on any machine, and the base an upgrade is diffed
;;;;               against.

(in-package #:nodecode.kernel)

(defun home-path (relative)
  "~/RELATIVE, resolved against $HOME now — never a cached home."
  ;; The operating system's home, not the organism's: see HOME for that.
  (merge-pathnames relative (user-homedir-pathname)))

(defun account-home ()
  "This account's home as the password database has it — the one systemd
runs a user unit in, whatever $HOME this process was handed — or NIL."
  #-win32 (let ((directory (ignore-errors
                            (sb-posix:passwd-dir (sb-posix:getpwuid (sb-posix:getuid))))))
            (and directory (uiop:ensure-directory-pathname directory)))
  #+win32 nil)

(defun expand-home (text &aux (text (string-trim '(#\Space #\Tab) text)))
  "TEXT, a path as the shell idiom writes it, with a leading `~/' read against
the operator's home, as a pathname."
  (if (uiop:string-prefix-p "~/" text)
      (home-path (subseq text 2))
      (uiop:parse-native-namestring text)))

(defun home-abbreviated (path)
  "PATH with the operator's home written `~': `~/src/thing', bare `~' at
home, the path itself anywhere else. EXPAND-HOME reads it back."
  ;; How a person reads their own paths, which is how a hint about one should
  ;; name it.
  (let ((path (string-right-trim "/" (or path "")))
        (home (string-right-trim "/" (or (ignore-errors
                                          (namestring (user-homedir-pathname)))
                                         ""))))
    (cond ((zerop (length path)) "/")
          ((and (plusp (length home)) (path-within-p path home))
           (concatenate 'string "~" (subseq path (length home))))
          (t path))))

(defun environment-directory (variable)
  "The directory VARIABLE names in this process's environment, or NIL when
it is unset or blank."
  (let ((value (string-trim '(#\Space #\Tab) (or (uiop:getenv variable) ""))))
    (and (plusp (length value))
         (uiop:parse-native-namestring value :ensure-directory t))))

(defun cache-path (relative)
  "$XDG_CACHE_HOME/RELATIVE, else %LOCALAPPDATA%/RELATIVE on Windows, else
~/.cache/RELATIVE, all read now: where the organism keeps what it can
recompute — fetched catalogs, compiled layers — apart from what it must
keep."
  ;; Shared by every profile: a cache entry is keyed by what it was computed
  ;; from, never by which home asked.
  (merge-pathnames relative
                   (or (environment-directory "XDG_CACHE_HOME")
                       #+win32 (environment-directory "LOCALAPPDATA")
                       (home-path ".cache/"))))

;;; Compiled code that embeds this image's macro expansions is keyed on it
;;; (the layer cache): a new build is a new stamp, and nothing compiled
;;; against the old build's expansions is found again.
(defvar *image-stamp*
  (format nil "~36r~36r" (get-universal-time)
          (random (expt 36 4) (make-random-state t)))
  "This image's identity: minted once, when this file loads — at the build
for a dumped binary, at the session's start for a developer image — and
never recomputed at image start, which is what sets it apart from a
startup parameter.")

(defvar *startup-parameters* '()
  "((SYMBOL . INITFORM-THUNK) ...) in definition order: every parameter
RESOLVE-STARTUP-PARAMETERS recomputes when a saved core starts.")

(defmacro define-startup-parameter (name initform &optional documentation)
  "DEFPARAMETER NAME to INITFORM now, and again at every image start."
  ;; Tests LET or SETF the parameter exactly as they would any special; only
  ;; the saved-core restart re-runs INITFORM.
  `(progn
     (defparameter ,name ,initform
       ,@(when documentation (list documentation)))
     (if-let (entry (assoc ',name *startup-parameters*))
       (setf (cdr entry) (lambda () ,initform))
       (alexandria:appendf *startup-parameters* (list (cons ',name (lambda () ,initform)))))
     ',name))

(defun resolve-startup-parameters ()
  "Recompute every DEFINE-STARTUP-PARAMETER value from the live process
environment, in definition order — so *HOME*, defined first, is fresh
before every parameter that reads it through HOME."
  ;; Runs from
  ;; SB-EXT:*INIT-HOOKS* on core start; callable by hand after an environment
  ;; change.
  (loop for (name . thunk) in *startup-parameters*
        do (setf (symbol-value name) (funcall thunk)))
  (length *startup-parameters*))

(pushnew 'resolve-startup-parameters sb-ext:*init-hooks*)

;;; --- the home: one folder, one profile ---------------------------------------
;;; A profile is a home. The default one is ~/.nodecode/ itself; a named one
;;; is ~/.nodecode/profiles/<name>/, laid out exactly the same — config.jsonc,
;;; secrets/, auth.json, SOUL.md, the state entries flat at the root (the
;;; store, the operator token), cells/ (the layer, the knowledge cells and
;;; the installed peripherals). So
;;; nothing migrates when profiles arrive, and every verb that reads or
;;; writes the home goes through HOME and follows the profile for free. Two
;;; profiles are two organisms — each layer is its own git repository of
;;; definitions — and two gateways with nothing to collide on: each store's
;;; .owner sidecar advertises its own ephemeral port.

;;; ADOPT-PROFILE sets it before re-resolving the startup parameters; NIL in
;;; every other process.
(defvar *profile-flag* nil
  "The profile the command line named (`-p NAME'): read by LOCATE-HOME
ahead of the environment.")

(defparameter +reserved-profile-names+
  '("default" "profiles" "nodecode" "test" "tmp" "root" "sudo"
    "serve" "trial" "setup" "import" "profile" "gateway" "resume" "warm" "warm-dump" "web")
  "Names a profile may not be created under: the default home's own name,
the folder the named ones live in, and every word the launch parses as a
verb — a wrapper named `serve' would shadow the verb it wraps.")

(defun profile-name-p (name)
  "Whether NAME may name a profile: [a-z0-9] then up to 63 of [a-z0-9_-] —
a folder name that is also a shell word, an environment value and a unit
name, on every platform."
  (and (stringp name) (ppcre:scan "\\A[a-z0-9][a-z0-9_-]{0,63}\\z" name) t))

(defun default-home ()
  "~/.nodecode/: the default profile's home, resolved now."
  (home-path ".nodecode/"))

;; The two places the ladder reads besides the environment. Startup
;; parameters defined AHEAD of *HOME*, since RESOLVE-STARTUP-PARAMETERS runs
;; in definition order and LOCATE-HOME reads both; the hermetic test posture
;; SETFs them under its scratch home so a profile test never touches
;; ~/.nodecode/profiles/.
(define-startup-parameter *profiles-directory* (home-path ".nodecode/profiles/")
  "Where every named profile's home sits: ~/.nodecode/profiles/<name>/.")

(define-startup-parameter *active-profile-path* (home-path ".nodecode/active-profile")
  "The sticky file: one line naming the profile a launch that names none
boots into. Written by PROFILE-USE, removed for `default'.")

(defun profile-home (name)
  "Where profile NAME lives: the default home for `default', else a folder
of its name under *PROFILES-DIRECTORY*. Nothing is created or checked here."
  (if (string= name "default")
      (default-home)
      (merge-pathnames (format nil "~a/" name) *profiles-directory*)))

(defun sticky-profile ()
  "The name the sticky file holds, or NIL: absent, blank, not a profile
name, `default', or naming a folder that is not there — a deleted
profile's stale pointer falls back to the default home, never to a
refusal at boot."
  (let* ((path *active-profile-path*)
         (text (and (probe-file path) (ignore-errors (uiop:read-file-string path))))
         (name (and text (trimmed text))))
    (and name
         (profile-name-p name)
         (not (string= name "default"))
         (uiop:directory-exists-p (profile-home name))
         name)))

(defun locate-home ()
  "The organism's home, first found wins: the profile the command line
named, $NODECODE_HOME, the profile the sticky file names, ~/.nodecode/."
  (or (and *profile-flag* (profile-home *profile-flag*))
      (environment-directory "NODECODE_HOME")
      (when-let (name (sticky-profile)) (profile-home name))
      (default-home)))

;;; Resolved at process start, before every parameter that reads it; read
;;; through HOME at call time everywhere else. The hermetic test posture SETFs
;;; it to a scratch folder.
(define-startup-parameter *home* (locate-home)
  "The organism's home as a directory pathname: ~/.nodecode/, or the
profile's folder (LOCATE-HOME).")

(defun home (&optional (relative ""))
  "The home, or a path under it: (home) is the directory itself, (home
\"cells/\") a folder in it, (home \"config.jsonc\") a file."
  ;; Reads *HOME* now, so a test's scratch home and a profile both apply.
  (merge-pathnames relative *home*))

(defun profile-name (&optional (home *home*))
  "The name of the profile HOME is: `default' for ~/.nodecode/, a folder's
name under ~/.nodecode/profiles/, else the directory's own name — a custom
$NODECODE_HOME is a profile called what its folder is called."
  (let* ((home (uiop:ensure-directory-pathname home))
         (last (car (last (pathname-directory home)))))
    (cond ((equal (namestring home) (namestring (default-home))) "default")
          ((stringp last) last)
          (t "default"))))

(defun profile-named-p (&optional (home *home*))
  "Whether HOME is a named profile: a folder directly under
*PROFILES-DIRECTORY*."
  ;; The default home and a custom $NODECODE_HOME are not.
  ;;
  ;; A question about layout, and only that: its one caller is the systemd
  ;; unit name (organism/background.lisp), where a named profile is
  ;; nodecode-gateway-NAME.service and the default home is
  ;; nodecode-gateway.service. It used to gate the shells too, which left the
  ;; default home identified by silence; what a shell shows is PROFILE-LABEL's
  ;; answer now, and that one is total over every home.
  (let ((home (uiop:ensure-directory-pathname home)))
    (and (equal (namestring (uiop:pathname-parent-directory-pathname home))
                (namestring *profiles-directory*))
         (stringp (car (last (pathname-directory home))))
         t)))

(defun profile-label (&optional (home *home*))
  "The name a shell shows for the home it is running, for every home the
organism can have."
  ;; PROFILE-NAME answers what a home is called, for unit names and manifests;
  ;; this answers what an operator reads, and the default home has a name to
  ;; say like any other. Total: never NIL, never signals.
  (or (ignore-errors (profile-name home)) "default"))

(defun publish-home ()
  "NODECODE_HOME in this process's environment = *HOME*, so a child — a
serve candidate, a trial, a warm image, a shell the model runs — lands in
this process's home, never in the sticky file's."
  ;; MAIN calls it once the home is settled; the answer is the value
  ;; published.
  (set-environment-variable "NODECODE_HOME"
                            (string-right-trim "/\\" (uiop:native-namestring *home*))))

(defun adopt-profile (name)
  "Make NAME's home this process's: every startup parameter re-resolves
under it. MAIN's `-p NAME', run before the folder loads. => *HOME*."
  (setf *profile-flag* name)
  (resolve-startup-parameters)
  *home*)

;;; The one authority for which binary is the organism — the source root,
;;; trial children, resurrection candidates all consult it — so a process
;;; exec'd from the launch cache still reads and spawns the binary the
;;; operator runs.
(defvar *image-origin* nil
  "The executable this image descends from: NIL in the origin binary and
every developer image; the origin's own path in a warm launch image
(launch.lisp bakes it at dump).")

(defun resolve-runtime (runtime)
  "RUNTIME, the executable's path as the runtime reports it, with every
symbolic link followed, as a pathname."
  ;; On Linux SBCL reads /proc/self/exe and the path is already real; on macOS
  ;; it is the path the process was started through, so a launch by the
  ;; installer's ~/.local/bin/nodecode link would name that folder, where no
  ;; src/ sits beside the binary, and the organism would be blind to itself.
  ;; On Windows the installer's `current' is a junction, which TRUENAME leaves
  ;; standing (os.lisp, REAL-PATH). RUNTIME is a pathname or a native
  ;; namestring — SBCL has reported either over the years. The path itself
  ;; when nothing resolves -- and while this file loads, before os.lisp
  ;; defines REAL-PATH: the startup value is the one that counts.
  (let ((path (if (pathnamep runtime) runtime (uiop:parse-native-namestring runtime))))
    (or (ignore-errors (real-path path)) path)))

(defun origin-pathname ()
  "The organism's executable: the origin this image was dumped from, else
the running runtime itself, links resolved (RESOLVE-RUNTIME)."
  (or *image-origin*
      (and sb-ext:*runtime-pathname* (resolve-runtime sb-ext:*runtime-pathname*))))

(defun organism-binary (&aux (origin (origin-pathname))
                             (binary (and origin (pathname origin)))
                             (name (and binary (pathname-name binary))))
  "The nodecode executable to exec later — a profile's wrapper, the
gateway's unit — as a native namestring, or NIL in a developer image."
  ;; The installer lays a release out as <root>/<version>/nodecode beside a
  ;; <root>/current link to the version it installed last: when this binary
  ;; lives in that layout — a `current' beside its folder that resolves to a
  ;; sibling folder's executable — the answer is the path through the link, so
  ;; whatever execs it after the next update runs the new build. The link need
  ;; not resolve to THIS binary: an update has just moved it to the next one,
  ;; and the unit this process rewrites before restarting itself must name the
  ;; link, not the folder it is leaving. Anything else is the
  ;; executable itself.
  (when (and (stringp name) (uiop:string-prefix-p "nodecode" name))
    (let* ((folder (uiop:pathname-directory-pathname binary))
           (root (uiop:pathname-parent-directory-pathname folder))
           (current (merge-pathnames
                     (make-pathname :directory '(:relative "current")
                                    :name name :type (pathname-type binary))
                     root))
           (through (ignore-errors (truename current)))
           (sibling (and through
                         (equal (namestring (uiop:pathname-parent-directory-pathname
                                             (uiop:pathname-directory-pathname through)))
                                (namestring (ignore-errors (truename root)))))))
      (uiop:native-namestring (if sibling current binary)))))

(defun runtime-directory (&aux (runtime (origin-pathname)))
  "The directory holding the organism's executable — the origin binary
when this process is a warm launch image — or NIL for a bare SBCL."
  (and runtime (uiop:pathname-directory-pathname (pathname runtime))))

;;; NIL when no source travels with this image.
(define-startup-parameter *source-root*
    ;; <runtime-directory>/src/src/ when the binary carries its source, else
    ;; the ASDF checkout this image loaded from, else NIL
    (let ((beside (and (runtime-directory)
                       (merge-pathnames "src/src/" (runtime-directory)))))
      (cond ((and beside (probe-file beside)) (truename beside))
            ((ignore-errors (asdf:find-system "nodecode" nil))
             (ignore-errors
              (truename (asdf:system-relative-pathname "nodecode" "src/"))))))
  "Directory of the shipped source the organism reads itself from.")

;;; A cell that holds an external lease -- a channel's poller or webhook --
;;; reads this at its start and stands down, since the operator's live
;;; organism keeps it. The executable's MAIN sets it before the folder loads
;;; and publishes it as NODECODE_EPHEMERAL=1, so every process it starts --
;;; the organism a shell runs in a process of its own above all -- is a
;;; throwaway too (PUBLISH-EPHEMERAL). The store only: config, the layer and
;;; every other home file are still the profile's. A scratch profile
;;; (NODECODE_HOME=$(mktemp -d)) is the whole-organism isolation a proof
;;; wants.
(define-startup-parameter *ephemeral* (equal "1" (uiop:getenv "NODECODE_EPHEMERAL"))
  "True when this organism runs on a throwaway store of its own (the
shell's `--ephemeral': a fresh temporary database, its own gateway, no
claim on anything outside the process).")

(defun publish-ephemeral ()
  "Make this process a throwaway (*EPHEMERAL*), and every process it starts
with it."
  (setf *ephemeral* t)
  (set-environment-variable "NODECODE_EPHEMERAL" "1"))

;;; The floor: generation 0, the binary alone. Set by `--pristine' by hand,
;;; and by a launch that finds the self before it died before serving
;;; (generation.lisp DECIDE-PRISTINE); every process this one starts inherits
;;; it, so a shell's serve runs pristine beside it.
(define-startup-parameter *pristine* (equal "1" (uiop:getenv "NODECODE_PRISTINE"))
  "True when this organism runs generation 0: no folder loaded, no image
followed, nothing kept.")

(defun publish-pristine ()
  "Make this process pristine (*PRISTINE*), and every process it starts with it."
  (setf *pristine* t)
  (set-environment-variable "NODECODE_PRISTINE" "1"))

;;; Every subdirectory carrying an .asd at its top level is a cell; the
;;; organism's own layer is written here too. NIL disables both — nothing
;;; loads, nothing is recorded — the hermetic test posture; scribe tests bind
;;; a scratch tree.
(define-startup-parameter *cells-directory* (home "cells/")
  "The one folder, under the home.")
