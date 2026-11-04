;;;; sandbox-macos.lisp --- `nodecode --sandbox' on macOS: the organism under
;;;; Seatbelt, the shell outside it. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; macOS has no namespaces and no bubblewrap. It has Seatbelt, the kernel
;;;; sandbox every App Store app runs in: /usr/bin/sandbox-exec applies a
;;;; profile written in SBPL to a program and every process it starts, and no
;;;; process inside can lift it. The launcher writes that profile itself, at
;;;; each launch, from the same policy the Linux jail reads
;;;; (SANDBOX-SEATBELT-PROFILE) -- no srt, no Node, nothing to install -- and
;;;; runs the serve under it (SANDBOX-SEATBELT-WRAP).
;;;;
;;;; In SBPL the last rule that matches decides, so the profile goes from broad
;;;; to narrow: everything denied; the system read; every account's home, the
;;;; host's temporary directories and the mounted volumes read-denied; the
;;;; folder, the organism's home, the binary's tree and what the operator
;;;; grants read back, and the folder and home written; then the secrets, other
;;;; agents' credential stores and the policy's denials refused again, and what
;;;; the launcher reads write-denied, last. The jail's own temporary directory,
;;;; cache and runtime directory, under the home's sandbox/, are its to write.
;;;;
;;;; There is no network namespace to give the jail a loopback of its own: the
;;;; jailed gateway binds its port on the host's loopback itself -- the one
;;;; port it may bind or be reached on -- so the shell attaches to it as it
;;;; would to any gateway, with no bridge. Its only other way out is the
;;;; launcher's egress proxy on loopback; DNS, every other address and every
;;;; unix socket but the system log's are refused.

(in-package #:nodecode.evolved)

(defparameter +seatbelt-program+ "/usr/bin/sandbox-exec"
  "Seatbelt's launcher, part of every macOS install.")

(defparameter +seatbelt-profile-name+ "seatbelt.sb"
  "The generated profile's name under <home>/sandbox/: written by the
launcher at each launch, write-denied inside the jail.")

(defparameter +seatbelt-denied-roots+
  '("/Users" "/private/tmp" "/private/var/tmp" "/private/var/folders" "/Volumes" "/cores")
  "What a jailed organism on macOS reads only where the policy gives it back:
every account's home (its ~/Library -- keychains, mail, cookies -- with it),
the host's temporary directories, whose files and sockets are other
programs', and the mounted volumes.")

(defun seatbelt-string (text)
  "TEXT as an SBPL string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          do (when (member char '(#\" #\\)) (write-char #\\ out))
             (write-char char out))
    (write-char #\" out)))

(defun seatbelt-path (path)
  "PATH as Seatbelt matches it: the real path, symlinks resolved (/tmp is
/private/tmp on macOS), with no trailing slash. A path that does not exist
yet keeps its own name under its nearest present ancestor's real path, so a
rule can name a file the jail is about to create."
  (let ((name (string-right-trim "/" (uiop:native-namestring path))))
    (labels ((real (name)
               (cond ((string= name "") "/")
                     ((ignore-errors (probe-file name))
                      (string-right-trim "/" (uiop:native-namestring (truename name))))
                     (t (let ((cut (position #\/ name :from-end t)))
                          (if (or (null cut) (zerop cut))
                              name
                              (let ((parent (real (subseq name 0 cut))))
                                (format nil "~a~a" (string-right-trim "/" parent) (subseq name cut)))))))))
      (let ((real (real name)))
        (if (string= real "") "/" real)))))

(defun seatbelt-regex-quote (text)
  "TEXT with every character a regex gives a meaning escaped, for an SBPL
#\"...\" literal."
  (with-output-to-string (out)
    (loop for char across text
          do (when (find char ".[]()*+?{}|^$\\\"") (write-char #\\ out))
             (write-char char out))))

(defun seatbelt-ancestors (path)
  "Every directory above PATH, \"/\" excluded: what a jail must be able to
stat -- never list or read -- for a path under them to resolve."
  (loop for cut = (position #\/ path :start 1) then (position #\/ path :start (1+ cut))
        while cut
        collect (subseq path 0 cut)))

(defun seatbelt-rule (out action operations paths &key (filter "subpath"))
  "Write one SBPL rule to OUT: ACTION (allow or deny) of OPERATIONS on each
of PATHS by FILTER; nothing when PATHS is empty."
  (when paths
    (format out "(~a ~{~a~^ ~}~{~%  (~a ~a)~})~%" action operations
            (loop for path in paths append (list filter (seatbelt-string path))))))

(defun seatbelt-inside-p (entry area)
  "Whether the path ENTRY is AREA or lies under it (both as SEATBELT-PATH
names them)."
  (or (string= entry area)
      (and (> (length entry) (length area))
           (string= area entry :end2 (length area))
           (char= #\/ (char entry (length area))))))

(defun sandbox-seatbelt-profile (&key home folder binary policy port proxy-port store
                                      run cache temporary (user-home (user-homedir-pathname))
                                      (path #'seatbelt-path) (exists #'probe-file))
  "The SBPL profile, as a string, that jails HOME's organism working in
FOLDER on macOS: BINARY is the executable it runs (its tree read, never
written); POLICY the operator's additions (SANDBOX-POLICY); PORT the
loopback port its gateway binds and is reached on; PROXY-PORT the egress
proxy's, or NIL for a jail with no way out at all; RUN, CACHE and TEMPORARY
the directories under HOME's sandbox/ that are the jail's own; STORE the
store the launcher named: its directory is the jail's to read and write
wherever it lies, unless that is the account's home or above it, where only
the store's own files (its sidecars, the write probe and the temporary
files nodecode makes beside it) are; USER-HOME
the account's home, whose Library/ is refused again even when the folder is
the home itself, unless the folder lies inside it. PATH turns a
name into the one Seatbelt matches (SEATBELT-PATH); EXISTS decides which
entries of +SANDBOX-HOME-READ+ are named."
  (let* ((home-dir (uiop:ensure-directory-pathname home))
         (sandbox-dir (merge-pathnames "sandbox/" home-dir))
         (binary-dir (and binary (uiop:pathname-directory-pathname binary)))
         (paths (lambda (items)
                  (remove-duplicates (mapcar path (remove nil items)) :test #'string= :from-end t)))
         (expand (lambda (items) (mapcar #'sandbox-expand items)))
         (own (funcall paths (list run cache temporary)))
         (store (and store (funcall path store)))
         (store-dir (and store (subseq store 0 (max 1 (position #\/ store :from-end t)))))
         (account (funcall path user-home))
         ;; a store in a directory of its own gets the directory; one in the
         ;; account's home or above gets its files only
         (store-whole (and store-dir (not (seatbelt-inside-p account store-dir))))
         (read-back (funcall paths
                             (append (list folder home binary-dir)
                                     (remove-if-not (lambda (entry) (ignore-errors (funcall exists entry)))
                                                    (funcall expand +sandbox-home-read+))
                                     (funcall expand (getf policy :allow-read))
                                     (funcall expand (getf policy :allow-write))
                                     (and store-whole (list store-dir)))))
         (write-back (funcall paths (append (list folder home)
                                            (funcall expand (getf policy :allow-write))
                                            (and store-whole (list store-dir)))))
         ;; ~/Library holds the keychains, mail, cookies and every app's
         ;; tokens: a folder at or above the home must not read it back.
         (library (funcall path (merge-pathnames "Library/" (uiop:ensure-directory-pathname user-home))))
         (refused (funcall paths
                           (append (unless (some (lambda (entry) (seatbelt-inside-p entry library)) read-back)
                                     (list library))
                                   (funcall expand +sandbox-default-deny-read+)
                                   (mapcar (lambda (relative) (sandbox-home-path home relative))
                                           '("auth.json" "operator-token" "backups/" "link/" "secrets/"))
                                   (funcall expand (getf policy :deny-read)))))
         (write-denied (funcall paths
                                (list sandbox-dir
                                      (sandbox-home-path home "sandbox.jsonc")
                                      (sandbox-home-path home "config.jsonc")
                                      binary-dir binary))))
    (with-output-to-string (out)
      (format out ";; nodecode --sandbox on macOS: written by the launcher at every launch.~%")
      (format out ";; The last rule that matches decides, so the rules run broad to narrow.~%")
      (format out "(version 1)~%(deny default)~%(import \"system.sb\")~%~%")
      (format out ";; processes: its own children, signalled and inspected only inside the jail~%")
      (format out "(allow process-fork)~%(allow process-exec)~%")
      (format out "(allow signal (target same-sandbox))~%(allow process-info* (target same-sandbox))~%")
      (format out "(allow pseudo-tty)~%")
      (format out "(allow file-read* file-write* file-ioctl~%  (literal \"/dev/tty\") (literal \"/dev/ptmx\") (regex #\"^/dev/ttys[0-9]+$\"))~%~%")
      (format out ";; files: the system read; every home, the host's temporary directories and the volumes not~%")
      (format out "(allow file-read* file-map-executable (subpath \"/\"))~%")
      (seatbelt-rule out "deny" '("file-read*") (funcall paths +seatbelt-denied-roots+))
      (format out ";; the folder, the home and what the operator grants, read back; their parents only stat'ed~%")
      (seatbelt-rule out "allow" '("file-read-metadata")
                     (remove-duplicates (loop for entry in (append read-back (and store (list store)))
                                              append (seatbelt-ancestors entry))
                                        :test #'string= :from-end t)
                     :filter "literal")
      (seatbelt-rule out "allow" '("file-read*") read-back)
      (seatbelt-rule out "allow" '("file-write*") write-back)
      (when (and store (not store-whole))
        (format out ";; the store in the account's home: its own files, nothing else of the home~%")
        (format out "(allow file-read* file-write* (regex #\"^~a/(~a([-.][^/]*)?|\\.nodecode-[^/]*)$\"))~%"
                (if (string= store-dir "/") "" (seatbelt-regex-quote store-dir))
                (seatbelt-regex-quote (subseq store (1+ (position #\/ store :from-end t))))))
      (format out ";; refused again wherever they sit: secrets, other agents' credentials, the policy's denials~%")
      (seatbelt-rule out "deny" '("file-read*" "file-write*") refused)
      (format out ";; what the launcher reads and what runs the jail, never written from inside it~%")
      (seatbelt-rule out "deny" '("file-write*") write-denied)
      (format out ";; the jail's own temporary directory, cache and runtime directory~%")
      (seatbelt-rule out "allow" '("file-read*" "file-write*") own)
      (format out "~%;; network: the gateway's own loopback port, and the launcher's proxy~%")
      (when port
        (format out "(allow network-bind network-inbound (local ip \"localhost:~d\"))~%" port)
        (format out "(allow network-outbound (remote ip \"localhost:~d\"))~%" port))
      (when proxy-port
        (format out "(allow network-outbound (remote ip \"localhost:~d\"))~%" proxy-port)))))

(defun seatbelt-proxy-variables (proxy-port)
  "The environment that sends a jailed program's web traffic to the egress
proxy on loopback PROXY-PORT, in both spellings programs read; NIL with no
proxy."
  (when proxy-port
    (let ((url (format nil "http://127.0.0.1:~d" proxy-port)))
      (append (loop for name in '("HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY")
                    collect (format nil "~a=~a" name url)
                    collect (format nil "~(~a~)=~a" name url))
              (list "NO_PROXY=localhost,127.0.0.1" "no_proxy=localhost,127.0.0.1")))))

(defun sandbox-seatbelt-wrap (argv &key profile port run cache temporary proxy-port directory)
  "ARGV, the serve's own, as the command line that runs it under Seatbelt:
env sets the gateway port it binds, the runtime directory, cache and
temporary directory that are the jail's own, the proxy, and takes the
shell's pristine marker away (the shell stands on the binary alone; the
organism loads its folder); sandbox-exec applies PROFILE to it and to every
process it starts. DIRECTORY, when given, is where it runs: a jail with no
work folder starts in its home, never in a directory it cannot read."
  (flet ((directory-word (name directory)
           (format nil "~a=~a" name (string-right-trim "/" (uiop:native-namestring directory)))))
    (append (list* "env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1"
                   (format nil "NODECODE_GATEWAY_PORT=~d" port)
                   (append (and run (list (directory-word "XDG_RUNTIME_DIR" run)))
                           (and cache (list (directory-word "XDG_CACHE_HOME" cache)))
                           (and temporary (list (directory-word "TMPDIR" temporary)))
                           (seatbelt-proxy-variables proxy-port)
                           (list +seatbelt-program+ "-f" (uiop:native-namestring profile))
                           ;; `sh -c SCRIPT DIR ARGV...' binds DIR to $0, the argv to "$@"
                           (and directory
                                (list "/bin/sh" "-c" "cd \"$0\" && exec \"$@\""
                                      (string-right-trim "/" (uiop:native-namestring directory))))))
            argv)))

(defun sandbox-seatbelt-requirements ()
  "Why Seatbelt cannot jail an organism here, as one sentence, or NIL."
  (unless (probe-file +seatbelt-program+)
    ;; Every macOS has it; a missing one is a damaged install, said by name.
    (format nil "--sandbox needs ~a, which every macOS install has: this one is missing it"
            +seatbelt-program+)))

(defun sandbox-seatbelt-preflight (&key serve store)
  "SANDBOX-LAUNCH-PREFLIGHT's macOS half, once SANDBOX-REQUIREMENTS let it
run: write HOME's Seatbelt profile under <home>/sandbox/ around the launch's
work folder (none for a SERVE, SANDBOX-WORK-FOLDER) and the STORE it
serves, pick the loopback port the jailed gateway binds, set *SANDBOX-PLAN*
for FRONT-ORGANISM and answer NIL."
  (let* ((home (nlk:home))
         (sandbox-dir (merge-pathnames "sandbox/" (uiop:ensure-directory-pathname home)))
         (run (merge-pathnames "run/" sandbox-dir))
         (cache (merge-pathnames "cache/" sandbox-dir))
         (temporary (sandbox-jail-temporary-directory home))
         (profile (merge-pathnames +seatbelt-profile-name+ sandbox-dir))
         (port (local-port 0))
         (folder (sandbox-work-folder (uiop:getcwd) :serve serve)))
    ;; The store's directory is made here: the jail may write the store and
    ;; its sidecars, never make a directory for them.
    (when store (ensure-directories-exist store))
    ;; The launcher's files exist before the profile that write-denies them,
    ;; and the mark goes down before anything runs in the jail.
    (sandbox-ensure-launcher-files home)
    (sandbox-mark home)
    ;; The jailed gateway serves on the home's own operator token: shells
    ;; outside read it from the file, the jail -- where the file is refused --
    ;; is handed it in its environment.
    (nlk::set-environment-variable "NODECODE_GATEWAY_OPERATOR_TOKEN" (sandbox-operator-token))
    (dolist (directory (list run cache temporary))
      (ensure-directories-exist directory)
      (ignore-errors (sb-posix:chmod (uiop:native-namestring directory) #o700)))
    (nlk::write-file-atomically
     profile
     (sandbox-seatbelt-profile :home home :folder folder :store store
                               :binary (ignore-errors (truename (nlk::origin-pathname)))
                               :policy (sandbox-policy home)
                               :port port :run run :cache cache :temporary temporary)
     :mode #o600 :directory-mode #o700)
    (setf *sandbox-plan* (list :seatbelt profile :port port :run run :cache cache
                               :temporary temporary
                               ;; no folder of its own: it starts in its home
                               :directory (unless folder home))
          *sandboxed-shell* t)
    nil))
