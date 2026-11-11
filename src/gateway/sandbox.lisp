;;;; sandbox.lisp --- `nodecode --sandbox': the organism in an OS jail, the
;;;; shell outside it. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; nodecode jails the organism itself, with no runtime of anyone else's:
;;;; on Linux bubblewrap and a seccomp filter built here (sandbox-linux.lisp),
;;;; on macOS Seatbelt, under a profile written here (sandbox-macos.lisp). What
;;;; follows is the Linux jail's shape; macOS keeps it where it can and says
;;;; where it cannot at the head of its own file.
;;;; The shell cannot run inside the jail -- the jail has no controlling
;;;; terminal -- so a sandboxed launch keeps the shell on the host, pristine
;;;; (the binary alone, so no code the jailed organism writes into the layer
;;;; runs outside the jail), and fronts a `serve' it runs jailed
;;;; (FRONT-ORGANISM). The jail has its own network namespace: the gateway
;;;; inside it binds a port the launcher chose (NODECODE_GATEWAY_PORT) on the
;;;; jail's loopback, the shell listens on that same port on the host's and
;;;; hands the listening socket in, and the jailed organism carries each
;;;; connection on it to its gateway -- so the shell's ordinary attach, by
;;;; the port the store's holder sidecar names, reaches the jailed gateway.
;;;; The way out is the egress proxy, outside the jail: the jail hands the
;;;; launcher a listener on its own loopback and the launcher answers on it.
;;;; The home's keys (auth.json, secrets/) are not in the jail at all; the
;;;; proxy outside adds them on a provider's own calls. The jail makes no
;;;; unix socket, and its /tmp is a private one, so no socket of the host's --
;;;; a tmux server's, an agent's, a desktop's -- is a way out of it.
;;;;
;;;; What the jail writes must not run outside it later. The home's layer and
;;;; cells are the jail's to write, so a home that has run sandboxed carries a
;;;; mark (<home>/sandbox/jailed.json, written outside the jail and
;;;; read-only inside it): a launch that would load the folder unjailed
;;;; refuses until `nodecode sandbox review' has been read and `release --yes'
;;;; lifts it. What the launcher itself reads -- config.jsonc (the providers,
;;;; hence the hosts a real key is added toward), sandbox.jsonc -- is
;;;; read-only in the jail, and the jail keeps a cache of its own, so the
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

(defun sandbox-home-path (home relative)
  "RELATIVE under HOME as a native namestring."
  (uiop:native-namestring (merge-pathnames relative (uiop:ensure-directory-pathname home))))

(defun sandbox-jail-temporary-directory (home)
  "The jail's TMPDIR: under HOME, on disk, the jail's own, never shared
with another sandbox on the host."
  (merge-pathnames "sandbox/tmp/" (uiop:ensure-directory-pathname home)))

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
(:allowed-domains LIST :deny-read LIST :allow-read LIST :allow-write LIST
:network WORD) -- or NIL when there is no file; a file that does not parse is
an error naming it. :NETWORK is \"internet\" when the jail may reach every
public host, else NIL: only the hosts allowed."
  (let ((path (merge-pathnames "sandbox.jsonc" (uiop:ensure-directory-pathname home))))
    (when (probe-file path)
      (let ((object (handler-case (nlk:decode-json (nlk::normalize-jsonc (uiop:read-file-string path)))
                      (error (e) (error "~a does not parse: ~a" (namestring path) e)))))
        (flet ((strings (key) (coerce (nlk:json-array object key) 'list)))
          (list :allowed-domains (strings "allowed_domains")
                :deny-read (strings "deny_read")
                :allow-read (strings "allow_read")
                :allow-write (strings "allow_write")
                :network (let ((word (ignore-errors (json-value object :text "network"))))
                           (and word (string-equal (nlk:trimmed word) "internet") "internet"))))))))

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

(defun sandbox-user-homes ()
  "The user's home directories the jail read-denies whole: the one $HOME
names, and the account's own (its passwd entry) when $HOME names another."
  ;; A $HOME pointed elsewhere -- a wrapper, sudo, a CI user -- would
  ;; otherwise leave the account's real home, ~/.ssh with it, readable.
  (remove-duplicates
   (remove nil (list (uiop:native-namestring (user-homedir-pathname))
                     #-win32
                     (ignore-errors
                      (uiop:native-namestring
                       (uiop:ensure-directory-pathname
                        (sb-posix:passwd-dir (sb-posix:getpwuid (sb-posix:getuid))))))))
   :test #'string= :from-end t))

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

(defun sandbox-ensure-launcher-files (home)
  "Make HOME's sandbox.jsonc and config.jsonc, empty, when either is absent:
the launcher reads both (the policy; the providers, hence the hosts a real
key is added toward), so the jail must never be the one to create them:
the jail sees each read-only, which a path that is not there cannot be.
=> the files made."
  (loop for name in '("sandbox.jsonc" "config.jsonc")
        for path = (merge-pathnames name (uiop:ensure-directory-pathname home))
        unless (probe-file path)
          do (nlk::write-file-atomically path (format nil "{}~%") :mode #o600 :directory-mode #o700)
          and collect path))

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

(defun sandbox-listener (port)
  "A socket listening on the host's loopback PORT: the jail's one way in,
which the jailed organism accepts on (SANDBOX-BRIDGE-INBOUND)."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (listening nil))
    (unwind-protect
         (progn (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
                (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
                (sb-bsd-sockets:socket-listen socket 64)
                (setf listening t)
                socket)
      (unless listening (ignore-errors (sb-bsd-sockets:socket-close socket))))))


(defun sandbox-requirements (store-path)
  "Why `--sandbox' cannot run here, as one sentence, or NIL when it can."
  (cond #-(or linux darwin)
        (t "--sandbox runs on Linux and macOS for now; on Windows, run nodecode inside WSL2, where the Linux jail works")
        #+linux
        ((sandbox-bwrap-requirements))
        #+darwin
        ((sandbox-seatbelt-requirements))
        ((ignore-errors (gateway-supervises-p store-path))
         "the background gateway serves this home unsandboxed: `nodecode gateway off' first, or use -p NAME for another home")
        ((ignore-errors (nlk::store-lease-held-p store-path))
         "an organism already runs this home unsandboxed: quit its shells or `nodecode gateway off', or use -p NAME for another home")))

(defun sandbox-egress-hosts (home)
  "The hosts a jailed organism of HOME may reach through the proxy: the
configured providers', nodecode's own and the operator's additions."
  (remove-duplicates (append (sandbox-provider-hosts) +sandbox-default-domains+
                             (getf (sandbox-policy home) :allowed-domains))
                     :test #'string-equal :from-end t))

;;; --- the keys outside, the stand-ins inside ----------------------------------------
;;; The jail reads no key. Where the home keeps one -- auth.json, secrets/,
;;; config.jsonc, the launcher's environment -- the jail reads a stand-in
;;; (SANDBOX-KEY-STAND-IN) in the same place, so its providers list and
;;; resolve as they would outside; the broker outside swaps a stand-in for the
;;; key (sandbox-egress.lisp), toward that provider's own address alone.

(defun sandbox-key-candidates (&optional (environment (sb-ext:posix-environ)))
  "The providers a key may be kept for outside the jail: the inventory's
(auth.json, secrets/, config.jsonc), the listable ones, and each one an
<ID>_API_KEY of ENVIRONMENT names, anthropic for ANTHROPIC_AUTH_TOKEN."
  (remove-duplicates
   (append (ignore-errors (configured-provider-inventory))
           (ignore-errors (listable-provider-ids))
           (loop for entry in environment
                 for at = (position #\= entry)
                 for name = (and at (subseq entry 0 at))
                 when (and name (> (length name) (length "_API_KEY")) (uiop:string-suffix-p name "_API_KEY")
                           (plusp (length (nlk:trimmed (subseq entry (1+ at))))))
                   collect (substitute #\- #\_ (string-downcase (subseq name 0 (- (length name) (length "_API_KEY"))))))
           (and (find-if (lambda (entry) (uiop:string-prefix-p "ANTHROPIC_AUTH_TOKEN=" entry)) environment)
                '("anthropic")))
   :test #'string-equal :from-end t))

(defun sandbox-provider-table (&optional (ids (sandbox-key-candidates)))
  "Each of IDS the launcher knows an address for, as a plist: :ID, :ORIGINS
the origins its endpoint and base are served at (NLK:URL-ORIGIN), :KEYED
true when a key is kept for it outside the jail."
  (let ((auth (ignore-errors (read-auth-file *auth-file-path*))))
    (loop for id in ids
          for origins = (remove-duplicates
                         (remove nil (list (ignore-errors (nlk:url-origin (provider-endpoint id)))
                                           (ignore-errors (nlk:url-origin (provider-base-url id)))))
                         :test #'string= :from-end t)
          when origins
            collect (list :id id :origins origins :keyed (and (sandbox-key-kept-p id auth) t)))))

(defun sandbox-key-kept-p (id auth)
  "Whether a key is kept for the provider ID outside the jail: its chain
answers one from config.jsonc, secrets/, AUTH (the parsed auth.json) or the
environment -- each tier asked on its own too, so a provider whose lane is
not known yet (a catalog not loaded, a cell's) is not missed."
  (let ((credential (ignore-errors (resolve-provider-credential id :probe t))))
    (or (and credential (member (credential-source credential) '(:config :secret-file :api-key :env)))
        (ignore-errors (provider-secret-key id :probe t))
        (and auth (auth-api-key auth id))
        (provider-env-key id)
        (ignore-errors (trimmed-config-string (configured-provider-entry id) "api_key")))))

(defun sandbox-stand-in-auth (auth providers)
  "The auth.json the jail reads in place of AUTH, the parsed real one: each
keyed one of PROVIDERS (SANDBOX-PROVIDER-TABLE) under api_keys, its key a
stand-in and the vars of its address kept; nothing else of AUTH (a machine's
link, a sign-in). NIL when no provider is keyed."
  (let ((keys (nlk:make-json-object)))
    (dolist (provider providers)
      (when (getf provider :keyed)
        (let* ((id (getf provider :id))
               (entry (nlk:make-json-object "key" (sandbox-key-stand-in id)))
               (vars (ignore-errors (json-value auth :object "api_keys" id "vars"))))
          (when (hash-table-p vars) (setf (gethash "vars" entry) vars))
          (setf (gethash id keys) entry))))
    (when (plusp (hash-table-count keys))
      (nlk:encode-json-object (nlk:make-json-object "api_keys" keys)))))

(defun sandbox-stand-in-config (text)
  "TEXT, a config.jsonc, as the jail reads it when a provider's api_key is
in it -- each such key a stand-in -- or NIL when none is."
  (let* ((config (ignore-errors (nlk:decode-json (nlk::normalize-jsonc text))))
         (providers (and (hash-table-p config) (gethash "providers" config)))
         (swapped nil))
    (when (hash-table-p providers)
      (maphash (lambda (id entry)
                 (when (and (hash-table-p entry) (stringp (gethash "api_key" entry))
                            (plusp (length (nlk:trimmed (gethash "api_key" entry)))))
                   (setf (gethash "api_key" entry) (sandbox-key-stand-in id)
                         swapped t)))
               providers))
    (and swapped (nlk:encode-json-object config))))

(defun sandbox-stand-ins (home providers)
  "Write the files the jail of HOME reads in place of the real ones under
<HOME>/sandbox/stand-in/ and answer them as an alist (REAL . STAND-IN), native
names: auth.json when it holds a key, config.jsonc when a provider's api_key
is in it."
  (let* ((dir (merge-pathnames "sandbox/stand-in/" (uiop:ensure-directory-pathname home)))
         (auth-path (merge-pathnames "auth.json" (uiop:ensure-directory-pathname home)))
         (config-path (merge-pathnames "config.jsonc" (uiop:ensure-directory-pathname home)))
         (auth (and (probe-file auth-path) (ignore-errors (read-auth-file auth-path))))
         (auth-text (and (probe-file auth-path) (sandbox-stand-in-auth auth providers)))
         (config-text (and (probe-file config-path)
                           (sandbox-stand-in-config (uiop:read-file-string config-path))))
         (pairs '()))
    (loop for (real text name) in (list (list auth-path auth-text "auth.json")
                                        (list config-path config-text "config.jsonc"))
          for path = (merge-pathnames name dir)
          do (if text
                 (progn (nlk::write-file-atomically path text :mode #o600 :directory-mode #o700)
                        (push (cons (uiop:native-namestring real) (uiop:native-namestring path)) pairs))
                 (ignore-errors (delete-file path))))
    (nreverse pairs)))

(defun sandbox-jail-environment (providers)
  "The variables the jail gets for PROVIDERS (SANDBOX-PROVIDER-TABLE), as
NAME VALUE ...: each keyed provider's own <ID>_API_KEY, its stand-in (the
key of the environment, or of secrets/, is in no file the jail reads), and
NODECODE_SANDBOX_HOST_ORIGINS, the origins on this host or its networks the
broker reaches for it (NLK:JAIL-BROKERED-URL)."
  (let ((local (remove-duplicates
                (loop for provider in providers
                      append (remove-if-not #'local-endpoint-p (getf provider :origins)))
                :test #'string=)))
    (append (loop for provider in providers
                  when (getf provider :keyed)
                    append (list (format nil "~a_API_KEY" (substitute #\_ #\- (string-upcase (getf provider :id))))
                                 (sandbox-key-stand-in (getf provider :id))))
            (and local (list "NODECODE_SANDBOX_HOST_ORIGINS" (format nil "~{~a~^,~}" local))))))

(defun sandbox-egress-plan (home providers)
  "What the egress outside HOME's jail answers under (SANDBOX-EGRESS-START):
:HOSTS, :INTERNET, :PROVIDERS and :LOG."
  (list :hosts (sandbox-egress-hosts home)
        :internet (and (getf (sandbox-policy home) :network) t)
        :providers providers
        :log (uiop:native-namestring (merge-pathnames "sandbox/egress.log" (uiop:ensure-directory-pathname home)))))

(defun sandbox-launch-preflight (store-path)
  "Prepare a sandboxed launch of STORE-PATH's home: refuse with a sentence
when it cannot run (SANDBOX-REQUIREMENTS), else make what the jail needs
under <home>/sandbox/, pick the loopback port the jailed gateway binds, set
*SANDBOX-PLAN* for FRONT-ORGANISM and answer NIL."
  (let ((store (merge-pathnames (or store-path nlk::*default-store-path*))))
    (or (sandbox-requirements store)
        (let* ((home (nlk:home))
               (sandbox (merge-pathnames "sandbox/" (uiop:ensure-directory-pathname home)))
               (run (merge-pathnames "run/" sandbox))
               (cache (merge-pathnames "cache/" sandbox))
               (temporary (sandbox-jail-temporary-directory home))
               (port (local-port 0)))
          ;; An absent policy, config or secrets/ would be the jail's to
          ;; create, and the next launch would read it: each is made here,
          ;; and the jail sees it read-only or not at all. The mark goes down
          ;; before the jail that may not write it.
          (sandbox-ensure-launcher-files home)
          (sandbox-mark home)
          ;; The jailed gateway and this shell share a token minted for this
          ;; launch: the home's operator-token is hidden in the jail.
          (nlk::set-environment-variable "NODECODE_GATEWAY_OPERATOR_TOKEN"
                                         (format nil "~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 32) 'list)))
          (dolist (directory (list run cache temporary (merge-pathnames "secrets/" (uiop:ensure-directory-pathname home))))
            (ensure-directories-exist directory)
            #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring directory) #o700)))
          (setf *sandbox-plan*
                (let ((plan (sandbox-launch-plan home store run cache temporary port)))
                  ;; Seatbelt's jail shares the host's loopback: its way out
                  ;; is a listener here, and its profile is written now
                  #+darwin (sandbox-seatbelt-plan plan)
                  #-darwin plan)
                *sandboxed-shell* t)
          nil))))

(defun sandbox-launch-plan (home store run cache temporary port)
  "The plan of HOME's jail FRONT-ORGANISM runs (*SANDBOX-PLAN*): what it
mounts, the ports in and out, the stand-ins and the egress it answers under."
  (let* ((providers (sandbox-provider-table))
         (log (merge-pathnames "sandbox/egress.log" (uiop:ensure-directory-pathname home))))
    ;; the egress log is the launcher's: made here, read-only in the jail
    (unless (probe-file log)
      (nlk::write-file-atomically log "" :mode #o600 :directory-mode #o700))
    (list :home (uiop:native-namestring (uiop:ensure-directory-pathname home))
          :folder (uiop:getcwd)
          :binary (uiop:native-namestring (truename (nlk::origin-pathname)))
          :store (uiop:native-namestring store)
          :policy (sandbox-policy home)
          :hidden (cons (sandbox-home-path home "secrets/") (sandbox-hidden-files home))
          :stand-ins (sandbox-stand-ins home providers)
          :jail-environment (sandbox-jail-environment providers)
          :run run :cache cache :temporary temporary
          :port port
          ;; the jail's own loopback is empty: any port but the gateway's
          :egress-port (if (= port 3128) 3129 3128)
          :egress (sandbox-egress-plan home providers)
          #+linux :disable-userns
          #+linux (sandbox-bwrap-disables-userns-p
                   (uiop:native-namestring (nlk::executable-on-path "bwrap"))))))

(defun sandbox-wrap-command (command)
  "COMMAND, a serve argv, as this shell spawns it: unchanged outside a
sandboxed shell; inside one, jailed (SANDBOX-BWRAP-ARGV), its program the
binary the jail mounts, to be spawned with the plan's descriptors
(CALL-WITH-SANDBOX-DESCRIPTORS)."
  (let ((plan *sandbox-plan*))
    (if (and plan command)
        #+darwin
        (sandbox-seatbelt-argv (cons (getf plan :binary) (rest command))
                               :profile (getf plan :profile)
                               :home (getf plan :home) :folder (getf plan :folder)
                               :port (getf plan :port) :egress-port (getf plan :egress-port)
                               :token (getf (getf plan :egress) :token)
                               :run (getf plan :run) :cache (getf plan :cache)
                               :temporary (getf plan :temporary)
                               :stand-ins (getf plan :stand-ins)
                               :jail-environment (getf plan :jail-environment)
                               :read-trees (list (getf plan :folder) (getf plan :home)
                                                 (uiop:native-namestring
                                                  (uiop:pathname-directory-pathname (getf plan :binary)))))
        #-darwin
        (sandbox-bwrap-argv (cons (getf plan :binary) (rest command))
                            :bwrap (uiop:native-namestring (nlk::executable-on-path "bwrap"))
                            :home (getf plan :home) :folder (getf plan :folder)
                            :binary (getf plan :binary) :store (getf plan :store)
                            :policy (getf plan :policy) :hidden (getf plan :hidden)
                            :run (getf plan :run) :cache (getf plan :cache)
                            :temporary (getf plan :temporary)
                            :port (getf plan :port) :egress-port (getf plan :egress-port)
                            :stand-ins (getf plan :stand-ins)
                            :jail-environment (getf plan :jail-environment)
                            :disable-userns (getf plan :disable-userns))
        command)))

(defun call-with-sandbox-descriptors (function)
  "Call FUNCTION with the descriptors a jailed serve is spawned with -- the
listener on the plan's loopback port (its way in), the jail's end of a
socketpair (its way out) and the seccomp program, to be its 3, 4 and 5 --
or with NIL outside a sandboxed shell, and answer what it answers. Once
FUNCTION has spawned the jail, this shell's copies close -- the jail alone
holds the listener, so the port refuses once the jail is gone -- and its end
of the socketpair waits for the jail's egress listener
(SANDBOX-RECEIVE-EGRESS)."
  (let ((plan *sandbox-plan*))
    (if (null plan)
        (funcall function nil)
        #-(or linux darwin) (error "no jail is wired on this system")
        ;; Seatbelt's jail binds its own port and dials the launcher's
        ;; listener: it is handed no descriptor
        #+darwin (progn (sandbox-seatbelt-egress plan) (funcall function nil))
        #+linux
        (let ((listener (sandbox-listener (getf plan :port)))
              (outside nil) (inside nil) (seccomp nil) (spawned nil))
          (unwind-protect
               (progn
                 (setf (values outside inside) (nlk::socket-pair)
                       seccomp (sandbox-seccomp-descriptor))
                 (multiple-value-prog1
                     (funcall function (list (sb-bsd-sockets:socket-file-descriptor listener) inside seccomp))
                   (setf spawned t)
                   (sandbox-receive-egress outside (getf plan :egress))))
            (ignore-errors (sb-bsd-sockets:socket-close listener))
            (dolist (descriptor (list inside seccomp (and (not spawned) outside)))
              (when descriptor (ignore-errors (sb-posix:close descriptor)))))))))
