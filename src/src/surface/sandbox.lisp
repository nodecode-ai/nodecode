;;;; sandbox.lisp --- `nodecode --sandbox': the organism in an OS jail, the
;;;; shell outside it. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The jail is Anthropic's sandbox-runtime (`srt': bubblewrap, seccomp and a
;;;; domain-allowlisting proxy on Linux). The shell cannot run inside it --
;;;; bubblewrap gives the jail no controlling terminal -- so a sandboxed
;;;; launch keeps the shell on the host, pristine (the binary alone, so no
;;;; code the jailed organism writes into the layer runs outside the jail),
;;;; and fronts a `serve' that srt wraps (FRONT-ORGANISM). The jail has its own
;;;; network namespace: the gateway inside it binds a port the launcher chose
;;;; (NODECODE_GATEWAY_PORT), a socat inside publishes it on a unix socket
;;;; under the home, and a socat on the host listens on the same loopback
;;;; port and forwards into the socket -- so the shell's ordinary attach, by
;;;; the port the store's holder sidecar names, reaches the jailed gateway.
;;;; Every secret file under secrets/ (providers.<id>.api_key, Fork B's
;;;; convention) is masked: inside the jail the file reads as a sentinel, and
;;;; the proxy swaps the real key back only on egress to that provider's host.

(in-package #:nodecode.evolved)

(defvar *sandbox-bridge* nil
  "The host-side socat bridging the chosen loopback port into the jail's
socket: (:pid N :process P), while it runs.")

(defparameter +sandbox-default-domains+
  '("nodecode.ai" "api.nodecode.ai" "models.dev" "github.com" "api.github.com"
    "codeload.github.com" "objects.githubusercontent.com" "raw.githubusercontent.com")
  "The hosts every jailed organism may reach: its own account and hub, the
models catalog, and the cells repository.")

(defparameter +sandbox-default-deny-read+
  '("~/.ssh" "~/.gnupg" "~/.aws" "~/.azure" "~/.config/gcloud" "~/.kube" "~/.docker"
    "~/.netrc" "~/.git-credentials" "~/.claude" "~/.codex" "~/.cursor" "~/.config/gh"
    "/var/run/docker.sock" "/run/docker.sock" "/tmp/.X11-unix")
  "What a jailed organism never reads: other agents' homes and credential
stores, and the sockets that are a way out of a jail (docker, X11).")

(defparameter +sandbox-settings-name+ "srt-settings.json"
  "The generated srt settings file's name under <home>/sandbox/.")

(defun sandbox-home-path (home relative)
  "RELATIVE under HOME as a native namestring."
  (uiop:native-namestring (merge-pathnames relative (uiop:ensure-directory-pathname home))))

(defun sandbox-expand (entry)
  "ENTRY with a leading ~/ expanded to the user's home directory."
  (if (and (>= (length entry) 2) (string= "~/" (subseq entry 0 2)))
      (uiop:native-namestring (merge-pathnames (subseq entry 2) (user-homedir-pathname)))
      entry))

(defun sandbox-run-directory ()
  "The user's runtime directory, whose sockets (dbus, the ssh agent) a jail
must not reach: XDG_RUNTIME_DIR, else /run/user/<uid>."
  (or (nlk:trimmed (or (uiop:getenv "XDG_RUNTIME_DIR") ""))
      #-win32 (format nil "/run/user/~d" (sb-posix:getuid))
      #+win32 nil))

(defun sandbox-policy (home)
  "The operator's additions from <HOME>/sandbox.jsonc as a plist --
(:allowed-domains LIST :deny-read LIST :allow-read LIST :allow-write LIST) -- or NIL when
there is no file; a file that does not parse is an error naming it."
  (let ((path (merge-pathnames "sandbox.jsonc" (uiop:ensure-directory-pathname home))))
    (when (probe-file path)
      (let ((object (handler-case (nlk:decode-json (nlk::normalize-jsonc (uiop:read-file-string path)))
                      (error (e) (error "~a does not parse: ~a" (namestring path) e)))))
        (flet ((strings (key) (coerce (nlk:json-array object key) 'list)))
          (list :allowed-domains (strings "allowed_domains")
                :deny-read (strings "deny_read")
                :allow-read (strings "allow_read")
                :allow-write (strings "allow_write")))))))

(defparameter +sandbox-home-read+
  '("~/.gitconfig" "~/.config/git/config" "~/.config/git/ignore")
  "What of the user's home a jailed organism still reads besides its folder,
its own home and its cache: git's identity and ignores, never its credentials.")

(defun sandbox-hidden-files (home &optional masked)
  "The files of HOME a jail must not read: the store of keys and sign-in
tokens, the gateway's token, the backups (each one this home whole), the link
secret, and every file under secrets/ that is not MASKED (a list of paths)."
  (let* ((dir (uiop:ensure-directory-pathname home))
         (masked (mapcar #'uiop:native-namestring masked))
         (secrets (ignore-errors (directory (merge-pathnames "secrets/*.*" dir)))))
    (append (mapcar (lambda (relative) (sandbox-home-path home relative))
                    '("auth.json" "operator-token" "backups/" "link/state.json"))
            (loop for path in secrets
                  for name = (uiop:native-namestring path)
                  unless (member name masked :test #'string=) collect name))))

(defun sandbox-settings (&key home folder binary srt-dir policy secret-files provider-hosts
                              hidden (deny-home t)
                              (exists #'probe-file) (run-directory (sandbox-run-directory)))
  "The srt settings object for a jail around HOME's organism working in
FOLDER: a JSON object ready for NLK:ENCODE-JSON-OBJECT. BINARY is the
executable the jail runs and SRT-DIR where srt is installed (both
write-denied, so nothing inside can rewrite what runs it, and read-allowed);
POLICY the operator's additions (SANDBOX-POLICY); SECRET-FILES an alist of
(PATH . HOST), each masked and injected only toward HOST; PROVIDER-HOSTS the
hosts the configured providers are served at; HIDDEN the files of HOME it
must not read (SANDBOX-HIDDEN-FILES). With DENY-HOME the user's whole home
directory is read-denied and only the folder, HOME, the cache, the binary's
and srt's trees and +SANDBOX-HOME-READ+ read back. EXISTS decides which
entries are mounted (srt refuses a path that is not there)."
  (let* ((home-dir (uiop:native-namestring (uiop:ensure-directory-pathname home)))
         (folder-dir (uiop:native-namestring (uiop:ensure-directory-pathname folder)))
         (cache-dir (uiop:native-namestring (merge-pathnames ".cache/nodecode/" (user-homedir-pathname))))
         (binary-dir (and binary (uiop:native-namestring (uiop:pathname-directory-pathname binary))))
         (string-list (lambda (items) (coerce (remove-duplicates (remove nil items) :test #'string= :from-end t) 'vector)))
         (present (lambda (paths) (remove-if-not (lambda (path) (and path (ignore-errors (funcall exists path)))) paths)))
         (domains (funcall string-list (append provider-hosts +sandbox-default-domains+
                                               (getf policy :allowed-domains))))
         (deny-read (funcall string-list
                             (funcall present
                                      (append (and deny-home (list (uiop:native-namestring (user-homedir-pathname))))
                                              (mapcar #'sandbox-expand +sandbox-default-deny-read+)
                                              (and run-directory (list run-directory))
                                              hidden
                                              (mapcar #'sandbox-expand (getf policy :deny-read))))))
         (allow-read (funcall string-list
                              (and deny-home
                                   (funcall present
                                            (append (list folder-dir home-dir cache-dir binary-dir
                                                          (and srt-dir (uiop:native-namestring srt-dir)))
                                                    (mapcar #'sandbox-expand +sandbox-home-read+)
                                                    (mapcar #'sandbox-expand (getf policy :allow-read))
                                                    (mapcar #'sandbox-expand (getf policy :allow-write)))))))
         (allow-write (funcall string-list
                               (append (list folder-dir home-dir cache-dir "/tmp")
                                       (mapcar #'sandbox-expand (getf policy :allow-write)))))
         (deny-write (funcall string-list
                              (list (sandbox-home-path home "sandbox.jsonc")
                                    (sandbox-home-path home (format nil "sandbox/~a" +sandbox-settings-name+))
                                    (sandbox-home-path home "secrets/")
                                    (and binary (uiop:native-namestring binary))
                                    (and srt-dir (uiop:native-namestring srt-dir)))))
         (masks (loop for (path . host) in secret-files
                      ;; Only the key's own span is masked: a whole-file mask
                      ;; carries the file's trailing newline back into the
                      ;; header it is substituted into, which the proxy then
                      ;; refuses to forward. No key found: the file is denied.
                      collect (nlk:make-json-object "path" (uiop:native-namestring path)
                                                    "mode" "mask"
                                                    "extract" "^\\s*(\\S+)"
                                                    "onExtractNoMatch" "deny"
                                                    "injectHosts" (vector host))))
         (network (nlk:make-json-object "allowedDomains" domains
                                        "deniedDomains" (vector)
                                        ;; the bridge socket: seccomp's unix-socket block is
                                        ;; off, which is why the socket directories above
                                        ;; are read-denied instead
                                        "allowAllUnixSockets" t)))
    (when masks
      (setf (gethash "tlsTerminate" network) (nlk:make-json-object)))
    (nlk:make-json-object
     "network" network
     "filesystem" (nlk:make-json-object "denyRead" deny-read
                                        "allowRead" allow-read
                                        "allowWrite" allow-write
                                        "denyWrite" deny-write)
     "credentials" (nlk:make-json-object "files" (coerce masks 'vector)))))

(defun sandbox-provider-hosts ()
  "The hosts the configured providers are served at, loopback and other
machines of the operator's own left out (they are not reachable from the
jail anyway)."
  (let ((hosts '()))
    (dolist (id (ignore-errors (union (configured-provider-inventory) (listable-provider-ids) :test #'string-equal)))
      (let* ((base (ignore-errors (provider-base-url id)))
             (host (and base (ignore-errors (quri:uri-host (quri:uri base))))))
        (when (and host (plusp (length host)) (not (local-endpoint-p base)))
          (pushnew host hosts :test #'string-equal))))
    (sort hosts #'string<)))

(defun sandbox-secret-files ()
  "An alist of (PATH . HOST): every providers.<id>.api_key under the secrets
directory whose provider has a host to inject it toward."
  (loop for path in (ignore-errors (directory (merge-pathnames "providers.*.api_key" *secrets-directory*)))
        for name = (file-namestring path)
        for id = (and (> (length name) (length "providers..api_key"))
                      (subseq name (length "providers.") (- (length name) (length ".api_key"))))
        for base = (and id (ignore-errors (provider-base-url id)))
        for host = (and base (ignore-errors (quri:uri-host (quri:uri base))))
        when (and host (plusp (length host)))
          collect (cons path host)))

(defun srt-program ()
  "The srt executable: $NODECODE_SRT, else `srt' on PATH; NIL when neither."
  (let ((named (nlk:trimmed (or (uiop:getenv "NODECODE_SRT") ""))))
    (cond ((plusp (length named)) (and (probe-file named) named))
          (t (nlk::executable-on-path "srt")))))

(defun sandbox-unsplit-keys (&optional (auth-path *auth-file-path*))
  "The provider ids auth.json still holds an API key for: keys a jail could
only be given whole, so --sandbox asks for `nodecode secrets split' first."
  (let ((auth (ignore-errors (read-auth-file auth-path))))
    (and (hash-table-p auth)
         (let ((keys (json-value auth :object "api_keys")))
           (and (hash-table-p keys) (sort (alexandria:hash-table-keys keys) #'string<))))))

(defun sandbox-srt-tree (srt)
  "The installed srt package SRT runs from -- its bin link resolved and two
levels up from dist/cli.js -- which the jail reads (its seccomp helper runs
inside) and never writes; NIL when it cannot be named."
  (ignore-errors
   (let ((real (truename srt)))
     (loop for dir = (uiop:pathname-directory-pathname real) then (uiop:pathname-parent-directory-pathname dir)
           repeat 4
           when (probe-file (merge-pathnames "package.json" dir)) return dir
           finally (return (uiop:pathname-directory-pathname real))))))

(defun sandbox-requirements (store-path)
  "Why `--sandbox' cannot run here, as one sentence, or NIL when it can."
  (cond #-linux
        (t "--sandbox runs on Linux for now; macOS and Windows jails are not yet wired")
        ((null (srt-program))
         "--sandbox needs srt, Anthropic's sandbox runtime: npm install -g @anthropic-ai/sandbox-runtime (or set NODECODE_SRT to it)")
        ((null (nlk::executable-on-path "bwrap"))
         "--sandbox needs bubblewrap: install bwrap")
        ((null (nlk::executable-on-path "socat"))
         "--sandbox needs socat")
        ((ignore-errors (gateway-supervises-p store-path))
         "the background gateway serves this home unsandboxed: `nodecode gateway off' first, or use -p NAME for another home")
        ((ignore-errors (nlk::store-lease-held-p store-path))
         "an organism already runs this home unsandboxed: quit its shells or `nodecode gateway off', or use -p NAME for another home")
        ((sandbox-unsplit-keys)
         (format nil "auth.json still holds the API key~p of ~{~a~^, ~}: `nodecode secrets split' moves each into a file of its own, which the jail sees only as a stand-in"
                 (length (sandbox-unsplit-keys)) (sandbox-unsplit-keys)))))

(defun sandbox-launch-preflight (store-path)
  "Prepare a sandboxed launch of STORE-PATH's home: refuse with a sentence
when it cannot run (SANDBOX-REQUIREMENTS), else write the srt settings under
<home>/sandbox/, pick the loopback port the jailed gateway binds, set
*SANDBOX-PLAN* for FRONT-ORGANISM and answer NIL."
  (let ((store (merge-pathnames (or store-path nlk::*default-store-path*))))
    (or (sandbox-requirements store)
        (let* ((home (nlk:home))
               (srt (srt-program))
               (run (merge-pathnames "sandbox/run/" (uiop:ensure-directory-pathname home)))
               (settings (merge-pathnames +sandbox-settings-name+
                                          (merge-pathnames "sandbox/" (uiop:ensure-directory-pathname home))))
               (socket (merge-pathnames "gateway.sock" run))
               (port (local-port 0))
               (secret-files (sandbox-secret-files))
               (object (sandbox-settings
                        :home home :folder (uiop:getcwd)
                        :binary (ignore-errors (truename (nlk::origin-pathname)))
                        :srt-dir (sandbox-srt-tree srt)
                        :policy (sandbox-policy home)
                        :secret-files secret-files
                        :hidden (sandbox-hidden-files home (mapcar #'car secret-files))
                        :provider-hosts (sandbox-provider-hosts))))
          ;; The jailed gateway and this shell share a token minted for this
          ;; launch: the home's operator-token is read-denied in the jail.
          (nlk::set-environment-variable "NODECODE_GATEWAY_OPERATOR_TOKEN"
                                         (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 32) 'list)))
          (ensure-directories-exist run)
          #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring run) #o700))
          (nlk::write-file-atomically settings (nlk:encode-json-object object) :mode #o600 :directory-mode #o700)
          (setf *sandbox-plan* (list :srt srt :settings settings :socket socket :port port :run run)
                *sandboxed-shell* t)
          nil))))

(defun sandbox-wrap (argv &key srt settings socket port run)
  "ARGV, the serve's own, as the command line that runs it jailed: env sets
the gateway port the jail binds and a runtime directory it may write and
takes the shell's pristine marker away (the shell stands on the binary
alone; the organism loads its folder), srt jails the rest, and a socat
beside the serve publishes the jail's loopback PORT on the host-visible
unix SOCKET."
  ;; `sh -c SCRIPT A B ...' binds A to $0 and the rest to $1...; the script
  ;; shifts the port off and execs the serve argv whole, so a word in it with
  ;; a space (a store path) stays one word. srt's `--' is what keeps the
  ;; words apart: without it srt joins them into one string for bash.
  (append (list "env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1"
                (format nil "NODECODE_GATEWAY_PORT=~d" port)
                (format nil "XDG_RUNTIME_DIR=~a" (string-right-trim "/" (uiop:native-namestring run)))
                srt "--settings" (uiop:native-namestring settings) "--" "sh" "-c"
                "socat UNIX-LISTEN:\"$0\",fork,unlink-early TCP:127.0.0.1:\"$1\" & shift; exec \"$@\""
                (uiop:native-namestring socket) (princ-to-string port))
          argv))

(defun sandbox-wrap-command (command)
  "COMMAND, a serve argv, as this shell spawns it: unchanged outside a
sandboxed shell; inside one, jailed (SANDBOX-WRAP) with the host-side bridge
running on the plan's port."
  (let ((plan *sandbox-plan*))
    (if (and plan command)
        (progn
          (unless *sandbox-bridge*
            (start-sandbox-bridge (getf plan :port) (getf plan :socket)))
          (sandbox-wrap command :srt (getf plan :srt) :settings (getf plan :settings)
                                :socket (getf plan :socket) :port (getf plan :port)
                                :run (getf plan :run)))
        command)))

(defun start-sandbox-bridge (port socket)
  "Start the host-side socat that listens on loopback PORT and forwards each
connection into the jail's SOCKET; kept in *SANDBOX-BRIDGE* to stop later."
  (stop-sandbox-bridge)
  (bind (((pid _ _ process _)
          (nlk:spawn-program (list "socat"
                                   (format nil "TCP-LISTEN:~d,bind=127.0.0.1,fork,reuseaddr" port)
                                   (format nil "UNIX-CONNECT:~a" (uiop:native-namestring socket)))
                             :output nil :error-output nil :input :null)))
    (setf *sandbox-bridge* (list :pid pid :process process))
    pid))

(defun stop-sandbox-bridge ()
  "End the host-side bridge, when one runs."
  (when-let (bridge (shiftf *sandbox-bridge* nil))
    (ignore-errors (nlk:kill-tree (getf bridge :pid)))
    (ignore-errors (sb-ext:process-wait (getf bridge :process)))
    (ignore-errors (sb-ext:process-close (getf bridge :process)))))
