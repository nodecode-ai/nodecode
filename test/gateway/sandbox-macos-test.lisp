;;;; sandbox-macos-test.lisp --- `nodecode --sandbox' on macOS: the Seatbelt
;;;; profile and its order, the command line, the stand-ins read by name, the
;;;; egress that answers its own jail alone and, on a Mac, the jail itself.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun seatbelt-test-path (path)
  "PATH as written, no trailing slash: the profile's names with no file
system asked."
  (string-right-trim "/" (uiop:native-namestring path)))

(defun seatbelt-test-profile (&key policy (egress-port 47001) (folder "/Users/u/proj/") stand-ins)
  "The profile for a fixed home, folder and binary, paths taken as written."
  (nle::sandbox-seatbelt-profile :home "/Users/u/.nodecode/" :folder folder :user-home "/Users/u/"
                                 :binary "/opt/nc/nodecode" :policy policy
                                 :hidden '("/Users/u/.nodecode/secrets/" "/Users/u/.nodecode/auth.json"
                                           "/Users/u/.nodecode/operator-token" "/Users/u/.nodecode/backups/"
                                           "/Users/u/.nodecode/link/state.json")
                                 :stand-ins stand-ins
                                 :port 47002 :egress-port egress-port
                                 :run "/Users/u/.nodecode/sandbox/run/"
                                 :cache "/Users/u/.nodecode/sandbox/cache/"
                                 :temporary "/Users/u/.nodecode/sandbox/tmp/"
                                 :path #'seatbelt-test-path
                                 :exists (constantly nil)))

(defun seatbelt-test-count (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (loop for start = 0 then (+ found (length needle))
        for found = (search needle haystack :start2 start)
        while found count t))

(deftest sandbox-seatbelt-profile-denies-by-default ()
  (let ((profile (seatbelt-test-profile)))
    (is (search (format nil "(version 1)~%(deny default)") profile) "a deny-default profile: what no rule allows is refused")
    (is (not (search "(allow default" profile)) "and nothing turns the default back to allow")
    (is (search "(allow signal (target same-sandbox))" profile) "the jail signals only its own processes")
    (is (not (search "(allow mach" profile)) "no system service beyond the system base is opened")))

#-win32
(deftest sandbox-seatbelt-profile-runs-broad-to-narrow ()
  (let* ((profile (seatbelt-test-profile
                   :policy '(:deny-read ("/Users/u/proj/private"))
                   :stand-ins '(("/Users/u/.nodecode/config.jsonc" . "/Users/u/.nodecode/sandbox/stand-in/config.jsonc"))))
         (system (search "(allow file-read* file-map-executable (subpath \"/\"))" profile))
         (homes (search "(deny file-read*" profile))
         (folder (search (format nil "(allow file-read*~%  (subpath \"/Users/u/proj\")") profile))
         (written (search "(allow file-write*" profile))
         (refused (search "(deny file-read* file-write*" profile))
         (kept (search "(deny file-write*" profile))
         (own (search (format nil "(allow file-read* file-write*~%  (subpath \"/Users/u/.nodecode/sandbox/run\")") profile)))
    (is (and system homes folder written refused kept own) "every layer of the file rules is there")
    (is (and system homes folder written refused kept own
             (< system homes folder written refused kept own)) "the system, then the homes denied, the folder read back and written, the keys refused, the launcher's files kept, the jail's own directories last: the last matching rule decides")
    (is (search "(subpath \"/Users\")" (subseq profile homes folder)) "every account's home is read-denied")
    (is (search "(subpath \"/private/var/folders\")" (subseq profile homes folder)) "and the host's per-user temporary directories")
    (let ((refusals (subseq profile refused kept)))
      (is (search "(subpath \"/Users/u/.nodecode/secrets\")" refusals) "the home's secrets are refused after the home is read back")
      (is (search "(subpath \"/Users/u/.nodecode/auth.json\")" refusals) "so is its store of keys and sign-in tokens")
      (is (search "(subpath \"/Users/u/.nodecode/operator-token\")" refusals) "and the gateway's token")
      (is (search "(subpath \"/Users/u/.nodecode/config.jsonc\")" refusals) "a file a stand-in is read in place of is refused itself")
      (is (not (search "stand-in/config.jsonc" refusals)) "while its stand-in reads through the home")
      (is (search (format nil "(subpath ~s)" (nle::sandbox-expand "~/.ssh")) refusals) "~/.ssh is refused even when the folder is the home it sits in")
      (is (search "(subpath \"/Users/u/proj/private\")" refusals) "a path the policy denies is refused inside the folder"))
    (is (search "(subpath \"/Users/u/.nodecode/sandbox.jsonc\")" (subseq profile kept own)) "what the launcher reads is write-denied")
    (is (search "(subpath \"/Users/u/.nodecode/sandbox\")" (subseq profile kept own)) "the profile, the mark, the stand-ins and the egress log under sandbox/ are not the jail's to write")
    (is (search "(subpath \"/opt/nc/nodecode\")" (subseq profile kept own)) "nor the binary that runs the jail")
    (is (search "(literal \"/Users/u\")" profile) "the folder's parents can be stat'ed, so its path resolves")
    (is (not (search "(subpath \"/Users/u\")" (subseq profile folder))) "but the account's home itself is never read back")))

#-win32
(deftest sandbox-seatbelt-profile-keeps-library-out-of-a-home-folder ()
  (let* ((profile (seatbelt-test-profile :folder "/Users/u/"))
         (refused (search "(deny file-read* file-write*" profile)))
    (is (and refused (search "(subpath \"/Users/u/Library\")" (subseq profile refused))) "a folder that is the account's home does not read back its Library: keychains, mail, cookies, app tokens"))
  (let ((profile (seatbelt-test-profile :folder "/Users/u/Library/Mobile Documents/proj/")))
    (is (not (search "(subpath \"/Users/u/Library\")" profile)) "a folder inside ~/Library (iCloud Drive) is not refused with it")
    (is (search "(subpath \"/Users/u/Library/Mobile Documents/proj\")" profile) "it is read back on its own")))

(deftest sandbox-seatbelt-profile-opens-only-two-ports ()
  (let ((profile (seatbelt-test-profile)))
    (is (search "(allow network-bind network-inbound (local ip \"localhost:47002\"))" profile) "the gateway binds and is reached on its own loopback port")
    (is (search "(allow network-outbound (remote ip \"localhost:47001\"))" profile) "the one way out is the launcher's egress")
    (is (= 2 (seatbelt-test-count "(allow network-outbound" profile)) "and its own port; no other outbound rule")
    (is (= 1 (seatbelt-test-count "(allow network-bind" profile)) "and no other port to bind")
    (is (not (search "(remote ip \"*" profile)) "no address outside loopback"))
  (let ((profile (seatbelt-test-profile :egress-port nil)))
    (is (= 1 (seatbelt-test-count "(allow network-outbound" profile)) "with no egress the jail reaches nothing but its own gateway")))

(deftest sandbox-seatbelt-strings-are-escaped ()
  (is (string= "\"/a \\\"b\\\\c\"" (nle::seatbelt-string "/a \"b\\c")) "a quote or backslash in a path cannot end the string early"))

#-win32
(deftest sandbox-seatbelt-path-resolves-what-is-not-there-yet ()
  (let* ((root (uiop:ensure-directory-pathname (temp-path "seatbelt")))
         (real (merge-pathnames "real/" root))
         (link (string-right-trim "/" (uiop:native-namestring (merge-pathnames "link" root)))))
    (unwind-protect
         (progn
           (ensure-directories-exist real)
           (sb-posix:symlink (uiop:native-namestring real) link)
           (let ((resolved (string-right-trim "/" (uiop:native-namestring (truename real)))))
             (is (string= resolved (nle::seatbelt-path (format nil "~a/" link))) "a symlinked directory is named by its real path, with no trailing slash")
             (is (string= (format nil "~a/absent.txt" resolved)
                          (nle::seatbelt-path (format nil "~a/absent.txt" link))) "a file not there yet keeps its name under its parent's real path")))
      (ignore-errors (delete-file link))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun seatbelt-test-store-profile (store)
  "A folderless jail's profile around STORE, paths taken as written."
  (nle::sandbox-seatbelt-profile :home "/Users/u/.nodecode/" :folder nil :user-home "/Users/u/"
                                 :hidden '("/Users/u/.nodecode/secrets/") :store store :port 47002
                                 :path #'seatbelt-test-path :exists (constantly nil)))

#-win32
(deftest sandbox-seatbelt-profile-gives-the-jail-its-store-wherever-it-lies ()
  (let* ((profile (seatbelt-test-store-profile "/Users/Shared/run/smoke/history.db"))
         (refused (search "(deny file-read* file-write*" profile))
         (written (search "(allow file-write*" profile)))
    (is (and written refused (search "(subpath \"/Users/Shared/run/smoke\")" (subseq profile written refused))) "a store in a directory of its own: the directory is written, before the refusals, which still win inside it")
    (is (search "(literal \"/Users/Shared/run\")" profile) "its parents can be stat'ed, so the path resolves"))
  (let ((profile (seatbelt-test-store-profile "/Users/u/history.db")))
    (is (not (search "(subpath \"/Users/u\")" profile)) "a store in the account's home never opens the home")
    (is (search "(allow file-read* file-write* (regex #\"^/Users/u/(history\\.db([-.][^/]*)?|\\.nodecode-[^/]*)$\"))" profile) "only its own files: sidecars, the write probe, nodecode's temporary files"))
  (let ((profile (seatbelt-test-store-profile "/Users/u/.nodecode/history.db")))
    (is (not (search "(regex #\"^/Users" profile)) "the default store, in the home, needs no rule of its own")
    (is (search "(subpath \"/Users/u/.nodecode/secrets\")" (subseq profile (search "(deny file-read* file-write*" profile))) "and the home's secrets stay refused")))

(defun seatbelt-test-argv (&key (folder "/Users/u/proj/") (environment '("PATH=/opt/homebrew/bin:/Users/u/.local/bin:/usr/bin" "HOME=/Users/u" "TERM=xterm" "OPENAI_API_KEY=sk-real" "SSH_AUTH_SOCK=/private/tmp/agent" "NODECODE_PRISTINE=1" "NODECODE_HOME=/Users/u/.nodecode")))
  "The command line of a jail around a fixed plan."
  (nle::sandbox-seatbelt-argv '("/opt/nc/nodecode" "serve" "--store" "/Users/u/a b/store.db")
                              :profile "/Users/u/.nodecode/sandbox/seatbelt.sb"
                              :home "/Users/u/.nodecode/" :folder folder :homes '("/Users/u/")
                              :port 47002 :egress-port 47001 :token "t0k"
                              :run "/Users/u/.nodecode/sandbox/run/" :cache "/Users/u/.nodecode/sandbox/cache/"
                              :temporary "/Users/u/.nodecode/sandbox/tmp/"
                              :stand-ins '(("/Users/u/.nodecode/auth.json" . "/Users/u/.nodecode/sandbox/stand-in/auth.json")
                                           ("/Users/u/.nodecode/secrets" . "/Users/u/.nodecode/sandbox/stand-in/secrets"))
                              :jail-environment '("OPENAI_API_KEY" "nodecode-sandbox-stand-in-openai")
                              :read-trees '("/Users/u/proj" "/Users/u/.nodecode" "/opt/nc")
                              :environment environment))

#-win32
(deftest sandbox-seatbelt-argv-runs-the-serve-under-sandbox-exec ()
  (let* ((argv (seatbelt-test-argv))
         (exec (position "/usr/bin/sandbox-exec" argv :test #'string=))
         (words (subseq argv 0 exec)))
    (is (string= "/usr/bin/env" (first argv)) "env sets the jail's environment")
    (flet ((unset-p (name) (search (list "-u" name) words :test #'string=))
           (set-p (word) (member word words :test #'string=)))
      (is (and (unset-p "OPENAI_API_KEY") (unset-p "SSH_AUTH_SOCK") (unset-p "NODECODE_PRISTINE")) "a key, an agent's socket and the shell's pristine marker are unset in it")
      (is (not (or (unset-p "HOME") (unset-p "TERM") (unset-p "NODECODE_HOME"))) "what the jail keeps rides in")
      (is (set-p "PATH=/opt/homebrew/bin:/usr/bin:/usr/local/bin:/bin:/usr/local/sbin:/usr/sbin") "its PATH keeps the system's trees, never a directory of the account's home it cannot read")
      (is (set-p "NODECODE_SANDBOXED=1") "it knows it is jailed")
      (is (set-p "NODECODE_GATEWAY_PORT=47002") "and the port its gateway binds")
      (is (set-p "NODECODE_SANDBOX_EGRESS_PORT=47001") "and its way out")
      (is (set-p "NODECODE_SANDBOX_EGRESS_TOKEN=t0k") "with the token that way out answers")
      (is (set-p "HTTPS_PROXY=http://nodecode:t0k@127.0.0.1:47001") "web traffic goes to the proxy, the token its password")
      (is (set-p "https_proxy=http://nodecode:t0k@127.0.0.1:47001") "in the spelling curl reads too")
      (is (set-p "TMPDIR=/Users/u/.nodecode/sandbox/tmp") "its temporary directory is its own")
      (is (set-p (format nil "NODECODE_SANDBOX_STAND_INS=/Users/u/.nodecode/auth.json~c/Users/u/.nodecode/sandbox/stand-in/auth.json~%/Users/u/.nodecode/secrets~c/Users/u/.nodecode/sandbox/stand-in/secrets" #\Tab #\Tab)) "it is told where its stand-ins are")
      (is (> (position "OPENAI_API_KEY=nodecode-sandbox-stand-in-openai" words :test #'string=)
             (position "OPENAI_API_KEY" words :test #'string=)) "a key's variable is the stand-in, set after the real one is unset"))
    (is (equal '("/usr/bin/sandbox-exec" "-f" "/Users/u/.nodecode/sandbox/seatbelt.sb"
                 "/bin/sh" "-c" "cd \"$0\" && exec \"$@\"" "/Users/u/proj"
                 "/opt/nc/nodecode" "serve" "--store" "/Users/u/a b/store.db")
               (subseq argv exec)) "sandbox-exec applies the profile; the serve runs in the folder, its argv word for word")))

#-win32
(deftest sandbox-seatbelt-argv-runs-a-folderless-jail-in-its-home ()
  (let* ((argv (seatbelt-test-argv :folder "/Users/u/"))
         (exec (position "/usr/bin/sandbox-exec" argv :test #'string=)))
    (is (string= "/Users/u/.nodecode" (nth (+ exec 6) argv)) "a folder that holds the account's home is no work folder: the jail starts in its home, never in a directory it cannot read")))

#-win32
(deftest sandbox-seatbelt-stand-ins-add-an-empty-secrets-directory ()
  (let* ((home (uiop:ensure-directory-pathname (temp-path "seatbelt-home")))
         (empty (merge-pathnames "sandbox/stand-in/secrets/" home)))
    (unwind-protect
         (progn
           (ensure-directories-exist empty)
           (alexandria:write-string-into-file "left by hand" (merge-pathnames "providers.x.api_key" empty))
           (let ((pairs (nle::sandbox-seatbelt-stand-ins home '(("/h/auth.json" . "/h/sandbox/stand-in/auth.json")))))
             (is (equal '("/h/auth.json" . "/h/sandbox/stand-in/auth.json") (first pairs)) "the stand-ins written keep their place")
             (is (equal (cons (nle::sandbox-directory-name (merge-pathnames "secrets/" home))
                              (nle::sandbox-directory-name empty))
                        (second pairs)) "the home's secrets/ is read as the stand-in directory")
             (is (null (directory (merge-pathnames "*.*" empty))) "which is empty: nothing in it stands in for a key")))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(defmacro with-seatbelt-test-environment ((&rest bindings) &body body)
  "Run BODY with each (NAME VALUE) of BINDINGS in the environment, the old
values put back after (an empty one where there was none)."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (mapcar (lambda (name) (cons name (uiop:getenv name))) ',(mapcar #'first bindings))))
       (unwind-protect
            (progn ,@(loop for (name value) in bindings
                           collect `(nlk::set-environment-variable ,name ,value))
                   ,@body)
         (loop for (name . value) in ,saved
               do (nlk::set-environment-variable name (or value "")))))))

#-win32
(deftest sandbox-seatbelt-stand-ins-are-read-by-name-in-the-jail ()
  (let ((map (format nil "/h/auth.json~c/h/sandbox/stand-in/auth.json~%/h/secrets~c/h/sandbox/stand-in/secrets" #\Tab #\Tab)))
    (with-seatbelt-test-environment (("NODECODE_SANDBOXED" "1") ("NODECODE_SANDBOX_STAND_INS" map))
      (is (equal #p"/h/sandbox/stand-in/auth.json" (nlk:jail-stand-in #p"/h/auth.json")) "a jailed process reads a file's stand-in in its place")
      (is (equal #p"/h/sandbox/stand-in/secrets/" (nlk:jail-stand-in #p"/h/secrets/")) "a directory's, as a directory")
      (is (equal #p"/h/config.jsonc" (nlk:jail-stand-in #p"/h/config.jsonc")) "a file with no stand-in is its own"))
    (with-seatbelt-test-environment (("NODECODE_SANDBOXED" "") ("NODECODE_SANDBOX_STAND_INS" map))
      (is (equal #p"/h/auth.json" (nlk:jail-stand-in #p"/h/auth.json")) "outside a jail the map is never read"))))

(deftest sandbox-seatbelt-broker-path-carries-the-token ()
  (with-seatbelt-test-environment (("NODECODE_SANDBOX_EGRESS_PORT" "47001") ("NODECODE_SANDBOX_EGRESS_TOKEN" "t0k")
                                   ("NODECODE_SANDBOX_HOST_ORIGINS" ""))
    (let ((url (nlk:jail-brokered-url "https://api.openai.com/v1/chat/completions"
                                      (list (cons "Authorization" (format nil "Bearer ~a" (nle::sandbox-key-stand-in "openai"))))))
          (egress (list :token "t0k")))
      (is (string= "http://127.0.0.1:47001/.nodecode-broker/t0k/https/api.openai.com:443/v1/chat/completions" url) "a jail on the host's loopback names its token first")
      (let ((target (nle::sandbox-egress-admitted (subseq url (length "http://127.0.0.1:47001")) '() egress)))
        (is (string= "/.nodecode-broker/https/api.openai.com:443/v1/chat/completions" target) "the egress takes it out")
        (is (equal "https://api.openai.com:443/v1/chat/completions" (nle::sandbox-broker-target target)) "and reads the address back")))))

(deftest sandbox-seatbelt-egress-admits-its-own-jail-alone ()
  (let ((egress (list :token "t0k"))
        (basic (lambda (credentials)
                 (list (cons "Proxy-Authorization" (format nil "Basic ~a" (cl-base64:string-to-base64-string credentials)))))))
    (is (string= "/.nodecode-broker/https/a:443/x" (nle::sandbox-egress-admitted "/.nodecode-broker/https/a:443/x" '() '(:hosts nil))) "an egress with no token -- a jail's own loopback -- answers every request")
    (is (null (nle::sandbox-egress-admitted "/.nodecode-broker/https/a:443/x" '() egress)) "with one, a broker path without it is refused")
    (is (null (nle::sandbox-egress-admitted "/.nodecode-broker/t0x/https/a:443/x" '() egress)) "and one with another")
    (is (string= "example.com:443" (nle::sandbox-egress-admitted "example.com:443" (funcall basic "nodecode:t0k") egress)) "a tunnel whose Proxy-Authorization carries the token is admitted")
    (is (null (nle::sandbox-egress-admitted "example.com:443" (funcall basic "nodecode:other") egress)) "another password is not")
    (is (null (nle::sandbox-egress-admitted "example.com:443" '() egress)) "nor no Proxy-Authorization at all")
    (is (null (nle::sandbox-egress-admitted "http://example.com/" '(("Proxy-Authorization" . "Bearer t0k")) egress)) "only Basic credentials are read")))

(deftest sandbox-seatbelt-egress-refuses-a-request-with-no-token ()
  (let ((home (uiop:ensure-directory-pathname (temp-path "seatbelt-egress"))))
    (ensure-directories-exist home)
    (multiple-value-bind (listener port) (egress-test-listener)
      (unwind-protect
           (let ((log (uiop:native-namestring (merge-pathnames "egress.log" home))))
             (nle::sandbox-egress-start :listener listener
                                        :egress (list :token "t0k" :hosts '("localhost") :log log :providers '()))
             (let ((answer (egress-test-exchange
                            port (format nil "CONNECT example.com:443 HTTP/1.1~c~c~c~c" #\Return #\Newline #\Return #\Newline))))
               (is (uiop:string-prefix-p "HTTP/1.1 407" answer) "a tunnel asked of the host's loopback with no token is refused before any verdict"))
             (let ((answer (egress-test-exchange
                            port (format nil "GET /.nodecode-broker/https/api.openai.com:443/v1/models HTTP/1.1~c~cHost: rebound.example~c~c~c~c"
                                         #\Return #\Newline #\Return #\Newline #\Return #\Newline))))
               (is (uiop:string-prefix-p "HTTP/1.1 407" answer) "so is a broker call from a page that reached the port by its name"))
             (let ((answer (egress-test-exchange
                            port (format nil "CONNECT example.com:443 HTTP/1.1~c~cProxy-Authorization: Basic ~a~c~c~c~c"
                                         #\Return #\Newline (cl-base64:string-to-base64-string "nodecode:t0k")
                                         #\Return #\Newline #\Return #\Newline))))
               (is (and (uiop:string-prefix-p "HTTP/1.1 403" answer) (search "not a host this jail may reach" answer)) "with the token the request reaches the verdict, which still refuses a host not allowed"))
             (is (search "refuse CONNECT: no token" (uiop:read-file-string log)) "each refusal is logged"))
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))))

#+darwin
(deftest sandbox-seatbelt-plan-wraps-and-hands-in-no-descriptor ()
  (let ((nle::*sandbox-plan* (list :profile "/h/.nodecode/sandbox/seatbelt.sb" :port 47002 :egress-port 47001
                                   :home "/h/.nodecode/" :folder "/h/proj/" :binary "/opt/nc/nodecode"
                                   :egress (list :token "t0k")
                                   :run "/h/.nodecode/sandbox/run/" :cache "/h/.nodecode/sandbox/cache/"
                                   :temporary "/h/.nodecode/sandbox/tmp/")))
    (is (member "/usr/bin/sandbox-exec" (nle::sandbox-wrap-command '("/opt/nc/nodecode" "serve")) :test #'string=) "a macOS plan wraps the serve in sandbox-exec")))

#+darwin
(deftest sandbox-seatbelt-jails-a-real-process ()
  (let* ((root (uiop:ensure-directory-pathname
                (format nil "/Users/Shared/nodecode-seatbelt-test-~d" (random 1000000))))
         (home (merge-pathnames "home/" root))
         (folder (merge-pathnames "proj/" root))
         (profile (merge-pathnames "seatbelt.sb" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "secrets/" home))
           (ensure-directories-exist folder)
           (alexandria:write-string-into-file "work" (merge-pathnames "work.txt" folder))
           (alexandria:write-string-into-file "sk-test" (merge-pathnames "secrets/providers.x.api_key" home))
           (alexandria:write-string-into-file
            (nle::sandbox-seatbelt-profile :home home :folder folder :binary "/bin/cat" :port nil
                                           :hidden (list (uiop:native-namestring (merge-pathnames "secrets/" home)))
                                           :temporary (merge-pathnames "sandbox/tmp/" home))
            profile)
           (flet ((jailed (&rest argv)
                    (nth-value 2 (uiop:run-program (list* "/usr/bin/sandbox-exec" "-f" (uiop:native-namestring profile) argv)
                                                   :output nil :error-output nil :ignore-error-status t))))
             (is (zerop (jailed "/bin/cat" (uiop:native-namestring (merge-pathnames "work.txt" folder)))) "the folder reads inside the jail")
             (is (not (zerop (jailed "/bin/cat" (uiop:native-namestring (merge-pathnames "secrets/providers.x.api_key" home))))) "the home's secrets do not")
             (is (not (zerop (jailed "/bin/ls" (uiop:native-namestring (user-homedir-pathname))))) "nor the account's home")
             (is (not (zerop (jailed "/bin/sh" "-c" "echo x > /tmp/nodecode-seatbelt-escape"))) "the host's /tmp is not the jail's to write")
             (is (not (zerop (jailed "/usr/bin/curl" "-sS" "-m" "5" "http://1.1.1.1"))) "and no address answers")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
