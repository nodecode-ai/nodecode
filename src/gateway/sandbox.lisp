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
;;;; (NODECODE_GATEWAY_PORT), the shell listens on that same loopback port on
;;;; the host and hands the listening socket to the jail as its stdout, and a
;;;; socat inside accepts on it and forwards each connection to the gateway --
;;;; so the shell's ordinary attach, by the port the store's holder sidecar
;;;; names, reaches the jailed gateway. The jail never makes a unix socket of
;;;; its own: srt's seccomp filter stays on and refuses every one, and the
;;;; jail's /tmp is a private one, so no socket of the host's -- a tmux
;;;; server's, an agent's, a desktop's -- is a way out of it.
;;;; Every secret file under secrets/ (providers.<id>.api_key, Fork B's
;;;; convention) is masked: inside the jail the file reads as a sentinel, and
;;;; the proxy swaps the real key back only on egress to that provider's host.
;;;;
;;;; What the jail writes must not run outside it later. The home's layer and
;;;; cells are the jail's to write, so a home that has run sandboxed carries a
;;;; mark (<home>/sandbox/jailed.json, written outside the jail and
;;;; write-denied inside it): a launch that would load the folder unjailed
;;;; refuses until `nodecode sandbox review' has been read and `release --yes'
;;;; lifts it. What the launcher itself reads -- config.jsonc (the providers,
;;;; hence the hosts a real key is injected toward), sandbox.jsonc, secrets/ --
;;;; is write-denied in the jail, and the jail keeps a cache of its own, so the
;;;; catalog and the warm images every launch on this machine reads are not
;;;; its to write.

(in-package #:nodecode.evolved)

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

(defun sandbox-srt-temporary-directory (home)
  "Where srt keeps what it makes for one jail of HOME -- its proxy sockets and
the stand-ins it masks secrets with: srt's TMPDIR, write-denied inside the
jail. Not /tmp, which the jail sees as an empty one of its own and where
srt's sockets would be hidden from it with the rest."
  (merge-pathnames "sandbox/srt/" (uiop:ensure-directory-pathname home)))

(defun sandbox-jail-temporary-directory (home)
  "The temporary directory srt hands the jail as TMPDIR (CLAUDE_CODE_TMPDIR):
under HOME, the jail's own, never shared with another sandbox on the host."
  (merge-pathnames "sandbox/tmp/" (uiop:ensure-directory-pathname home)))

(defparameter +sandbox-socket-path-room+ 72
  "How long srt's temporary directory's name may be: srt names its proxy
sockets claude-socks-<16 hex>.sock under it, and a unix socket's whole path
fits in 107 bytes.")

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
         (binary-dir (and binary (uiop:native-namestring (uiop:pathname-directory-pathname binary))))
         (string-list (lambda (items) (coerce (remove-duplicates (remove nil items) :test #'string= :from-end t) 'vector)))
         (present (lambda (paths) (remove-if-not (lambda (path) (and path (ignore-errors (funcall exists path)))) paths)))
         (domains (funcall string-list (append provider-hosts +sandbox-default-domains+
                                               (getf policy :allowed-domains))))
         (deny-read (funcall string-list
                             (funcall present
                                      (append (and deny-home (list (uiop:native-namestring (user-homedir-pathname))))
                                              ;; the host's /tmp, whose sockets (tmux, agents, a
                                              ;; desktop's input) are ways out: the jail's /tmp is
                                              ;; an empty one of its own
                                              (list "/tmp")
                                              (mapcar #'sandbox-expand +sandbox-default-deny-read+)
                                              (and run-directory (list run-directory))
                                              hidden
                                              (mapcar #'sandbox-expand (getf policy :deny-read))))))
         (allow-read (funcall string-list
                              (and deny-home
                                   (funcall present
                                            (append (list folder-dir home-dir binary-dir
                                                          (and srt-dir (uiop:native-namestring srt-dir)))
                                                    (mapcar #'sandbox-expand +sandbox-home-read+)
                                                    (mapcar #'sandbox-expand (getf policy :allow-read))
                                                    (mapcar #'sandbox-expand (getf policy :allow-write)))))))
         (allow-write (funcall string-list
                               (append (list folder-dir home-dir)
                                       (mapcar #'sandbox-expand (getf policy :allow-write)))))
         (deny-write (funcall string-list
                              (list (sandbox-home-path home "sandbox.jsonc")
                                    (sandbox-home-path home "config.jsonc")
                                    (sandbox-home-path home (format nil "sandbox/~a" +sandbox-settings-name+))
                                    (sandbox-home-path home (format nil "sandbox/~a" +sandbox-mark-name+))
                                    ;; srt's own temporary directory: its proxy sockets
                                    ;; and the stand-ins it masks secrets with
                                    (uiop:native-namestring (sandbox-srt-temporary-directory home))
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
         ;; No allowAllUnixSockets: it would turn srt's seccomp filter off, and
         ;; with it the jail could reach every unix socket it can see.
         (network (nlk:make-json-object "allowedDomains" domains
                                        "deniedDomains" (vector))))
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

(defun sandbox-ensure-launcher-files (home)
  "Make HOME's sandbox.jsonc and config.jsonc, empty, when either is absent:
the launcher reads both (the policy; the providers, hence the hosts a real
key is injected toward), so the jail must never be the one to create them.
srt write-denies an absent path by binding a placeholder over it, which the
jailed organism could not read its config through. => the files made."
  (loop for name in '("sandbox.jsonc" "config.jsonc")
        for path = (merge-pathnames name (uiop:ensure-directory-pathname home))
        unless (probe-file path)
          do (nlk::write-file-atomically path (format nil "{}~%") :mode #o600 :directory-mode #o700)
          and collect path))

(defun sandbox-unsplit-keys (&optional (auth-path *auth-file-path*))
  "The provider ids auth.json still holds an API key for: keys a jail could
only be given whole, so --sandbox asks for `nodecode secrets split' first."
  (let ((auth (ignore-errors (read-auth-file auth-path))))
    (and (hash-table-p auth)
         (let ((keys (json-value auth :object "api_keys")))
           (and (hash-table-p keys) (sort (alexandria:hash-table-keys keys) #'string<))))))

;;; --- the mark: a home that has run sandboxed ----------------------------------

(defun sandboxed-process-p ()
  "Whether this process runs inside a --sandbox jail: NODECODE_SANDBOXED set
and not blank (the launcher's wrap sets it to 1)."
  (plusp (length (nlk:trimmed (or (uiop:getenv "NODECODE_SANDBOXED") "")))))

(defparameter +sandbox-mark-name+ "jailed.json"
  "The mark's name under <home>/sandbox/: written outside the jail at a
home's first sandboxed launch, write-denied inside it.")

(defun sandbox-mark-path (home)
  "HOME's mark: present once the home has run sandboxed."
  (merge-pathnames (format nil "sandbox/~a" +sandbox-mark-name+) (uiop:ensure-directory-pathname home)))

(defun sandbox-cell-heads (home)
  "An alist (NAME . HEAD) for every folder under HOME's cells/: its git HEAD,
or NIL for a folder with no repository."
  (loop for dir in (ignore-errors (uiop:subdirectories (merge-pathnames "cells/" (uiop:ensure-directory-pathname home))))
        for name = (car (last (pathname-directory dir)))
        collect (cons name
                      (and (probe-file (merge-pathnames ".git/" dir))
                           (let ((out (ignore-errors
                                       (uiop:run-program (list "git" "-C" (uiop:native-namestring dir) "rev-parse" "HEAD")
                                                         :output :string :ignore-error-status t))))
                             (and out (= 40 (length (nlk:trimmed out))) (nlk:trimmed out)))))))

(defun sandbox-mark (home)
  "Write HOME's mark unless it has one: when, and what its cells/ held -- each
folder and its HEAD -- so a review can name what the jail added since. The
first mark stands until a release: a later sandboxed launch never moves it."
  (let ((path (sandbox-mark-path home)))
    (unless (probe-file path)
      (let ((heads (sandbox-cell-heads home)))
        (nlk::write-file-atomically
         path
         (nlk:encode-json-object
          (nlk:make-json-object "since" (nlk::iso-now)
                                "cells" (let ((object (nlk:make-json-object)))
                                          (loop for (name . head) in heads
                                                do (setf (gethash name object) (or head :null)))
                                          object)))
         :mode #o600 :directory-mode #o700)))
    path))

(defun sandbox-mark-object (home)
  "HOME's mark as read, or NIL when it has none."
  (let ((path (sandbox-mark-path home)))
    (and (probe-file path) (ignore-errors (nlk:decode-json (uiop:read-file-string path))))))

(defun sandbox-jailed-refusal (home &key asked sandboxed loads)
  "Why a launch of HOME that LOADS its folder must not run outside the jail,
or NIL: HOME carries the mark and the launch neither ASKED for --sandbox nor
runs SANDBOXED already."
  (when (and loads (not asked) (not sandboxed))
    (when-let (mark (sandbox-mark-object home))
      (format nil "this home has run under --sandbox since ~a, and what ran in the jail could have written code this launch would run outside it. `nodecode --sandbox' runs it jailed; `nodecode sandbox review' shows what changed, and `nodecode sandbox release --yes' lifts this once you have read it."
              (or (ignore-errors (json-value mark :text "since")) "an unknown time")))))

(defun sandbox-review-text (home)
  "What HOME's cells/ gained since its mark: each new folder, and each
folder's commits past the HEAD the mark recorded, with the files they touch."
  (let ((mark (sandbox-mark-object home)))
    (if (null mark)
        "this home has not run under --sandbox: nothing to review"
        (let* ((recorded (or (json-value mark :object "cells") (nlk:make-json-object)))
               (cells (merge-pathnames "cells/" (uiop:ensure-directory-pathname home)))
               (lines (list (format nil "under --sandbox since ~a. Written in the jail since then:"
                                    (or (json-value mark :text "since") "?"))))
               (changed 0))
          (flet ((git (dir &rest args)
                   (nlk:trimmed (or (ignore-errors
                                     (uiop:run-program (list* "git" "-C" (uiop:native-namestring dir) args)
                                                       :output :string :ignore-error-status t))
                                    ""))))
            (loop for (name . head) in (sandbox-cell-heads home)
                  for dir = (merge-pathnames (format nil "~a/" name) cells)
                  for (before present) = (multiple-value-list (gethash name recorded))
                  for before-head = (and (stringp before) before)
                  do (cond ((not present)
                            (incf changed)
                            (push (format nil "- cells/~a: a new folder~@[, ~a commit~:p~]" name
                                          (and head (ignore-errors (parse-integer (git dir "rev-list" "--count" "HEAD")))))
                                  lines)
                            (when head (push (git dir "log" "--oneline" "-n" "20") lines)))
                           ((and head before-head (string/= head before-head))
                            (incf changed)
                            (push (format nil "- cells/~a: ~a commit~:p since the mark" name
                                          (or (ignore-errors (parse-integer (git dir "rev-list" "--count" (format nil "~a..HEAD" before-head)))) "?"))
                                  lines)
                            (push (git dir "log" "--oneline" "-n" "20" (format nil "~a..HEAD" before-head)) lines)
                            (push (git dir "diff" "--stat" before-head "HEAD") lines)))
                     (when present
                       ;; a file written since the mark loads whether or not
                       ;; a commit carries it: a plain folder, or an edit the
                       ;; jail left uncommitted
                       (let ((newer (nlk:trimmed
                                     (or (ignore-errors
                                          (uiop:run-program
                                           (list "find" (uiop:native-namestring dir) "-path" "*/.git" "-prune" "-o"
                                                 "-type" "f" "-newer" (uiop:native-namestring (sandbox-mark-path home)) "-print")
                                           :output :string :ignore-error-status t))
                                         ""))))
                         (when (plusp (length newer))
                           (incf changed)
                           (push (format nil "- cells/~a: files written since the mark:" name) lines)
                           (push newer lines))))))
          (when (zerop changed) (push "- nothing: cells/ is as the mark found it" lines))
          (format nil "~{~a~^~%~}" (reverse (remove "" lines :test #'string=)))))))

(defun sandbox-answer (words)
  "The `sandbox' command line as (values TEXT EXIT-CODE STREAM): `review'
prints what the jail wrote since the mark; `release --yes' lifts the mark
(and drops the hub cache the jail could also write), never from inside the
jail."
  (let ((home (nlk:home)))
    (cond ((equal words '("review")) (values (sandbox-review-text home) 0 :output))
          ((equal (first words) "release")
           (cond ((sandboxed-process-p)
                  (values "the mark is lifted from outside the jail, never from inside it" 1 :error))
                 ((null (sandbox-mark-object home))
                  (values "this home has not run under --sandbox: nothing to release" 0 :output))
                 ((not (equal (rest words) '("--yes")))
                  (values "release lets this home launch outside the jail again, running what the jail wrote: read `nodecode sandbox review' first, then `nodecode sandbox release --yes'" 2 :error))
                 (t (delete-file (sandbox-mark-path home))
                    (ignore-errors (uiop:delete-directory-tree
                                    (merge-pathnames "hub/" (uiop:ensure-directory-pathname home))
                                    :validate t :if-does-not-exist :ignore))
                    (values "released: this home launches outside the jail again (the hub cache is dropped and fetched afresh)" 0 :output))))
          (t (values "nodecode sandbox review | release [--yes]" 2 :error)))))

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
        ((not (socat-accepts-a-descriptor-p))
         "--sandbox needs socat 1.7.4 or later: the jail takes its one way in as a listening socket (socat's ACCEPT-FD)")
        ((> (length (uiop:native-namestring (sandbox-srt-temporary-directory (nlk:home))))
            +sandbox-socket-path-room+)
         (format nil "this home's path is too long for srt's sockets under it (~a): use -p NAME for a home with a shorter one"
                 (uiop:native-namestring (sandbox-srt-temporary-directory (nlk:home)))))
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
               (cache (merge-pathnames "sandbox/cache/" (uiop:ensure-directory-pathname home)))
               ;; An absent policy or config file would be the jail's to
               ;; create, and the next launch would read it: each is made here,
               ;; empty, and the jail sees it write-denied. The mark goes down
               ;; before the settings that deny writing it.
               (_ (progn (sandbox-ensure-launcher-files home)
                         (sandbox-mark home)))
               (settings (merge-pathnames +sandbox-settings-name+
                                          (merge-pathnames "sandbox/" (uiop:ensure-directory-pathname home))))
               (srt-temporary (sandbox-srt-temporary-directory home))
               (jail-temporary (sandbox-jail-temporary-directory home))
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
          (declare (ignore _))
          ;; The jailed gateway and this shell share a token minted for this
          ;; launch: the home's operator-token is read-denied in the jail.
          (nlk::set-environment-variable "NODECODE_GATEWAY_OPERATOR_TOKEN"
                                         (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 32) 'list)))
          (dolist (directory (list run cache srt-temporary jail-temporary))
            (ensure-directories-exist directory)
            #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring directory) #o700)))
          (nlk::write-file-atomically settings (nlk:encode-json-object object) :mode #o600 :directory-mode #o700)
          (setf *sandbox-plan* (list :srt srt :settings settings :port port :run run :cache cache
                                     :srt-temporary srt-temporary :jail-temporary jail-temporary)
                *sandboxed-shell* t)
          nil))))

(defparameter +sandbox-jail-script+
  "exec 3>&1 1>&2
if ! grep -q '^Seccomp:[[:space:]]*2' /proc/self/status; then
  echo 'nodecode: --sandbox found no seccomp filter on the jail, so it could reach the unix sockets of the host; it does not run without one (srt applies it on x64 and arm64)' >&2
  exit 78
fi
socat ACCEPT-FD:3,fork TCP4:127.0.0.1:\"$0\" &
exec 3>&- \"$@\""
  "What runs first inside the jail, under srt's seccomp filter: the listening
socket the shell handed in on stdout moves to descriptor 3 and stdout goes
back to the log; a jail without the filter (an architecture srt has no
helper for, a host that refuses it) stops here, before anything of the
folder loads; a socat accepts each connection on the socket and forwards it
to the gateway's port inside the jail; the serve runs with the socket
closed. $0 is the port, the rest the serve argv.")

(defun sandbox-wrap (argv &key srt settings port run cache srt-temporary jail-temporary)
  "ARGV, the serve's own, as the command line that runs it jailed: env sets
the gateway port the jail binds, a runtime directory and a cache it may
write, srt's own temporary directory (SRT-TEMPORARY) and the one it hands
the jail (JAIL-TEMPORARY), and takes the shell's pristine marker away (the
shell stands on the binary alone; the organism loads its folder); srt jails
the rest, and +SANDBOX-JAIL-SCRIPT+ runs first inside it. The serve's stdout
must be the host-side listener on PORT (CALL-WITH-SANDBOX-STDOUT)."
  ;; `sh -c SCRIPT A B ...' binds A to $0 and the rest to $1...: the port is
  ;; $0 and the serve argv is "$@" whole, so a word in it with a space (a
  ;; store path) stays one word. srt's `--' is what keeps the words apart:
  ;; without it srt joins them into one string for bash.
  (flet ((directory-word (name directory)
           (format nil "~a=~a" name (string-right-trim "/" (uiop:native-namestring directory)))))
    (append (list* "env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1"
                   (format nil "NODECODE_GATEWAY_PORT=~d" port)
                   (directory-word "XDG_RUNTIME_DIR" run)
                   (directory-word "XDG_CACHE_HOME"
                                   (or cache (merge-pathnames "cache/" (uiop:pathname-parent-directory-pathname run))))
                   (append (and srt-temporary (list (directory-word "TMPDIR" srt-temporary)))
                           (and jail-temporary (list (directory-word "CLAUDE_CODE_TMPDIR" jail-temporary)))
                           (list srt "--settings" (uiop:native-namestring settings) "--" "sh" "-c"
                                 +sandbox-jail-script+ (princ-to-string port))))
            argv)))

(defun sandbox-wrap-command (command)
  "COMMAND, a serve argv, as this shell spawns it: unchanged outside a
sandboxed shell; inside one, jailed (SANDBOX-WRAP), to be spawned with
the plan's listener as its stdout (CALL-WITH-SANDBOX-STDOUT)."
  (let ((plan *sandbox-plan*))
    (if (and plan command)
        (sandbox-wrap command :srt (getf plan :srt) :settings (getf plan :settings)
                              :port (getf plan :port) :run (getf plan :run) :cache (getf plan :cache)
                              :srt-temporary (getf plan :srt-temporary)
                              :jail-temporary (getf plan :jail-temporary))
        command)))

(defun sandbox-listener (port)
  "A socket listening on the host's loopback PORT: the jail's one way in,
which its socat accepts on (+SANDBOX-JAIL-SCRIPT+)."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (listening nil))
    (unwind-protect
         (progn (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
                (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
                (sb-bsd-sockets:socket-listen socket 64)
                (setf listening t)
                socket)
      (unless listening (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun call-with-sandbox-stdout (function)
  "Call FUNCTION with the descriptor a jailed serve's stdout must be -- a
listener on the plan's loopback port (SANDBOX-LISTENER) -- or with NIL
outside a sandboxed shell, and answer what it answers. This shell's own
copy of the listener closes once FUNCTION returns: the jail alone holds it,
so the port refuses once the jail is gone."
  (let ((plan *sandbox-plan*))
    (if (null plan)
        (funcall function nil)
        (let ((listener (sandbox-listener (getf plan :port))))
          (unwind-protect (funcall function (sb-bsd-sockets:socket-file-descriptor listener))
            (ignore-errors (sb-bsd-sockets:socket-close listener)))))))

(defun socat-accepts-a-descriptor-p ()
  "Whether the socat on PATH takes a listening socket it is handed (its
ACCEPT-FD address, socat 1.7.4 and later)."
  (search "ACCEPT-FD" (or (ignore-errors (uiop:run-program (list "socat" "-h") :output :string
                                                                                  :error-output nil
                                                                                  :ignore-error-status t))
                          "")))
