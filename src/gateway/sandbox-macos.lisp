;;;; sandbox-macos.lisp --- `nodecode --sandbox' on macOS: the organism under
;;;; Seatbelt, the shell outside it. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; macOS has no namespaces and no bubblewrap. It has Seatbelt, the kernel
;;;; sandbox every App Store app runs in: /usr/bin/sandbox-exec applies a
;;;; profile written in SBPL to a program and to every process it starts, and no
;;;; process inside can lift it. The launcher writes that profile itself at each
;;;; launch, from the plan the Linux jail is built from (SANDBOX-SEATBELT-PROFILE)
;;;; -- nothing to install -- and runs the serve under it (SANDBOX-SEATBELT-ARGV).
;;;;
;;;; In SBPL the last rule that matches decides, so the profile goes from broad
;;;; to narrow: everything denied; the system read; every account's home, the
;;;; host's temporary directories and the mounted volumes read-denied; the
;;;; folder, the organism's home, the binary's tree and what the operator grants
;;;; read back, and the folder and home written; then the keys, other agents'
;;;; credential stores and the policy's denials refused again, and what the
;;;; launcher reads write-denied. The jail's own temporary directory, cache and
;;;; runtime directory, under the home's sandbox/, are its to write.
;;;;
;;;; Seatbelt refuses a path; it cannot put another file in its place, as bwrap
;;;; mounts a key's stand-in over the key's file. So the real auth.json and
;;;; secrets/ -- and config.jsonc, when it holds a key -- are refused, and the
;;;; jail is told where their stand-ins are (NODECODE_SANDBOX_STAND_INS,
;;;; NLK:JAIL-STAND-IN): its credential store, settings and secrets directory
;;;; are read from there, and the broker outside adds the keys as on Linux.
;;;;
;;;; There is no network namespace to give the jail a loopback of its own. The
;;;; jailed gateway binds its port on the host's loopback itself -- the one port
;;;; it may bind or be reached on -- so the shell attaches to it as to any
;;;; gateway, with no bridge; its one way out is the egress proxy the launcher
;;;; answers on another port of that loopback (sandbox-egress.lisp). DNS, every
;;;; other address and port, and every unix socket but the system log's are
;;;; refused. The proxy's port is on the host's loopback, where any program of
;;;; the machine -- a web page through DNS rebinding among them -- could reach
;;;; the broker that adds the keys: so each launch mints a token, the jail is
;;;; handed it, and the proxy answers only a request that carries it
;;;; (SANDBOX-EGRESS-ADMITTED).

(in-package #:nodecode.evolved)

(defparameter +seatbelt-program+ "/usr/bin/sandbox-exec"
  "Seatbelt's launcher, part of every macOS install.")

(defparameter +seatbelt-profile-name+ "seatbelt.sb"
  "The generated profile's name under <home>/sandbox/: written by the
launcher at each launch, write-denied inside the jail.")

(defparameter +seatbelt-denied-roots+
  '("/Users" "/private/tmp" "/private/var/tmp" "/private/var/folders" "/Volumes" "/cores")
  "What a jailed organism on macOS reads only where the plan gives it back:
every account's home (its ~/Library -- keychains, mail, cookies -- with it),
the host's temporary directories, whose files and sockets are other
programs', and the mounted volumes.")

(defparameter +seatbelt-system-trees+
  '("/usr" "/bin" "/sbin" "/opt" "/System" "/Library" "/Applications" "/nix")
  "The system's trees, which the jail reads: where its PATH may point besides
the folder, the home and the binary's tree.")

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

(defun sandbox-seatbelt-profile (&key home folder binary store policy hidden stand-ins
                                      port egress-port run cache temporary
                                      (user-home (user-homedir-pathname))
                                      (path #'seatbelt-path) (exists #'probe-file))
  "The SBPL profile, as a string, that jails HOME's organism working in
FOLDER (NIL for none) on macOS. BINARY is the executable it runs (its tree
read, never written); POLICY the operator's additions (SANDBOX-POLICY);
HIDDEN the home's paths it never reads (the plan's :HIDDEN: the keys, the
tokens, the backups) and STAND-INS, an alist (REAL . STAND-IN), the files
read in their place: each REAL is refused, each STAND-IN reads through the
home. PORT is the loopback port its gateway binds and is reached on,
EGRESS-PORT the egress proxy's, or NIL for a jail with no way out at all;
RUN, CACHE and TEMPORARY the directories under HOME's sandbox/ that are the
jail's own. STORE is the store the launcher named: its directory is the
jail's to read and write wherever it lies, unless that is the account's home
or above it, where only the store's own files (its sidecars, the write probe
and the temporary files nodecode makes beside it) are. USER-HOME is the
account's home, whose Library/ is refused again even when the folder is the
home itself, unless the folder lies inside it. PATH turns a name into the
one Seatbelt matches (SEATBELT-PATH); EXISTS decides which entries of
+SANDBOX-HOME-READ+ are named."
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
                                   hidden
                                   (mapcar #'car stand-ins)
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
      (format out ";; refused again wherever they sit: the keys, other agents' credentials, the policy's denials~%")
      (seatbelt-rule out "deny" '("file-read*" "file-write*") refused)
      (format out ";; what the launcher reads and what runs the jail, never written from inside it~%")
      (seatbelt-rule out "deny" '("file-write*") write-denied)
      (format out ";; the jail's own temporary directory, cache and runtime directory~%")
      (seatbelt-rule out "allow" '("file-read*" "file-write*") own)
      (format out "~%;; network: the gateway's own loopback port, and the launcher's egress proxy~%")
      (when port
        (format out "(allow network-bind network-inbound (local ip \"localhost:~d\"))~%" port)
        (format out "(allow network-outbound (remote ip \"localhost:~d\"))~%" port))
      (when egress-port
        (format out "(allow network-outbound (remote ip \"localhost:~d\"))~%" egress-port)))))

(defun sandbox-seatbelt-argv (argv &key profile home folder port egress-port token run cache temporary
                                        stand-ins jail-environment read-trees
                                        (environment (sb-ext:posix-environ))
                                        (homes (sandbox-user-homes)))
  "ARGV, the serve's own, as the command line that runs it under Seatbelt.
env unsets each variable of ENVIRONMENT, the launcher's, the jail does not
keep (SANDBOX-ENVIRONMENT-KEPT-P) and sets the jail's own: its PATH (the
entries in the system's trees and READ-TREES), the gateway's PORT, the
EGRESS-PORT with the proxy variables that name it and its TOKEN, RUN, CACHE
and TEMPORARY as its runtime directory, cache and TMPDIR, STAND-INS (an alist
(REAL . STAND-IN)) as NODECODE_SANDBOX_STAND_INS, and JAIL-ENVIRONMENT,
NAME VALUE ..., last. sandbox-exec then applies PROFILE to the serve and to
every process it starts, which runs in the work FOLDER
(SANDBOX-WORK-FOLDER) or, with none, in HOME -- never in a directory the
jail cannot read."
  (let* ((folder (and folder (sandbox-work-folder folder homes)))
         (variables (mapcar (lambda (entry)
                              (let ((at (position #\= entry)))
                                (if at (cons (subseq entry 0 at) (subseq entry (1+ at))) (cons entry ""))))
                            environment))
         (out '()))
    (labels ((emit (&rest words) (dolist (word words) (push word out)))
             (setenv (name value) (emit (format nil "~a=~a" name value))))
      (emit "/usr/bin/env")
      (loop for (name . nil) in variables
            unless (or (sandbox-environment-kept-p name) (string= name ""))
              do (emit "-u" name))
      (setenv "PATH" (sandbox-jail-path (cdr (assoc "PATH" variables :test #'string=))
                                        (append +seatbelt-system-trees+ (remove nil read-trees))))
      (setenv "NODECODE_SANDBOXED" "1")
      (setenv "NODECODE_GATEWAY_PORT" port)
      (when egress-port
        (setenv "NODECODE_SANDBOX_EGRESS_PORT" egress-port)
        (when token (setenv "NODECODE_SANDBOX_EGRESS_TOKEN" token))
        (loop for (name value) on (sandbox-proxy-environment egress-port token) by #'cddr
              do (setenv name value)))
      (when run (setenv "XDG_RUNTIME_DIR" (sandbox-directory-name run)))
      (when cache (setenv "XDG_CACHE_HOME" (sandbox-directory-name cache)))
      (when temporary (setenv "TMPDIR" (sandbox-directory-name temporary)))
      (when stand-ins
        (setenv "NODECODE_SANDBOX_STAND_INS"
                (format nil "~{~a~^~%~}"
                        (loop for (real . stand-in) in stand-ins
                              collect (format nil "~a~c~a" real #\Tab stand-in)))))
      (loop for (name value) on jail-environment by #'cddr do (setenv name value))
      (emit +seatbelt-program+ "-f" (uiop:native-namestring profile)
            ;; `sh -c SCRIPT DIR ARGV...' binds DIR to $0, the argv to "$@"
            "/bin/sh" "-c" "cd \"$0\" && exec \"$@\"" (or folder (sandbox-directory-name home)))
      (append (nreverse out) argv))))

(defun sandbox-seatbelt-requirements ()
  "Why Seatbelt cannot jail an organism here, as one sentence, or NIL."
  (unless (probe-file +seatbelt-program+)
    ;; Every macOS has it; a missing one is a damaged install, said by name.
    (format nil "--sandbox needs ~a, which every macOS install has: this one is missing it"
            +seatbelt-program+)))

(defun sandbox-seatbelt-stand-ins (home stand-ins)
  "STAND-INS (SANDBOX-STAND-INS) and the one Seatbelt needs beside them: the
home's secrets/ read as an empty directory of its own under
<HOME>/sandbox/stand-in/, where bwrap would mount an empty one over it."
  (let ((empty (merge-pathnames "sandbox/stand-in/secrets/" (uiop:ensure-directory-pathname home))))
    (ensure-directories-exist empty)
    ;; it stays empty: a file left there by hand is no stand-in of a key
    (dolist (file (ignore-errors (directory (merge-pathnames "*.*" empty))))
      (ignore-errors (delete-file file)))
    (append stand-ins
            (list (cons (sandbox-directory-name (sandbox-home-path home "secrets/"))
                        (sandbox-directory-name empty))))))

(defun sandbox-seatbelt-listener ()
  "A listening socket on the host's loopback, on a port the system picks:
where the launcher answers a Seatbelt jail's way out."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (ignore-errors (sb-bsd-sockets:socket-close listener)))))
      (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
      (sb-bsd-sockets:socket-listen listener 128))
    listener))

(defun sandbox-seatbelt-plan (plan)
  "PLAN, SANDBOX-LAUNCH-PLAN's, as a Seatbelt jail runs it: its way out a
listener on the host's loopback (:EGRESS-LISTENER, :EGRESS-PORT) whose
egress answers only the launch's token (:EGRESS's :TOKEN), the secrets
directory's stand-in added, and the profile written under the home's
sandbox/ (:PROFILE)."
  (let* ((home (getf plan :home))
         (listener (sandbox-seatbelt-listener))
         (egress-port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
         (token (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 24) 'list)))
         (stand-ins (sandbox-seatbelt-stand-ins home (getf plan :stand-ins)))
         (profile (merge-pathnames (format nil "sandbox/~a" +seatbelt-profile-name+)
                                   (uiop:ensure-directory-pathname home)))
         (folder (sandbox-work-folder (getf plan :folder))))
    (nlk::write-file-atomically
     profile
     (sandbox-seatbelt-profile :home home :folder folder :binary (getf plan :binary)
                               :store (getf plan :store) :policy (getf plan :policy)
                               :hidden (getf plan :hidden) :stand-ins stand-ins
                               :port (getf plan :port) :egress-port egress-port
                               :run (getf plan :run) :cache (getf plan :cache)
                               :temporary (getf plan :temporary))
     :mode #o600 :directory-mode #o700)
    (list* :profile (uiop:native-namestring profile)
           :egress-listener listener
           :egress-port egress-port
           :stand-ins stand-ins
           :egress (list* :token token (getf plan :egress))
           plan)))

(defvar *sandbox-seatbelt-egress* nil
  "The thread answering this shell's Seatbelt jail on the way out, once one
is started: one listener serves every jail the shell starts, again after an
update.")

(defun sandbox-seatbelt-egress (plan)
  "Answer the way out of PLAN's jail (SANDBOX-SEATBELT-PLAN) on its listener,
once for this shell."
  (unless *sandbox-seatbelt-egress*
    (setf *sandbox-seatbelt-egress*
          (sandbox-egress-start :listener (getf plan :egress-listener) :egress (getf plan :egress)))))

#+darwin
(defun sandbox-seatbelt-on-p ()
  "Whether a Seatbelt profile is on this process: sandbox_check(3) of its own
pid with no operation named."
  (ignore-errors
   (plusp (sb-alien:alien-funcall
           (sb-alien:extern-alien "sandbox_check" (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int))
           (sb-posix:getpid) nil 0))))

#+darwin
(defun sandbox-seatbelt-enter ()
  "The jailed organism's first act on macOS, before anything of its folder
loads: one told it is jailed that finds no Seatbelt on itself stops here (exit
78) -- it would read and reach all the account can."
  (unless (sandbox-seatbelt-on-p)
    (format *error-output* "nodecode: --sandbox found no Seatbelt profile on the jail, so it could read and reach all this account can; it does not run without one~%")
    (finish-output *error-output*)
    (sb-ext:exit :code 78 :abort t)))
