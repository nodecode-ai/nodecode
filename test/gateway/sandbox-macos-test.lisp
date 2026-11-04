;;;; sandbox-macos-test.lisp --- `nodecode --sandbox' on macOS: the Seatbelt
;;;; profile, its order, the wrap and, on a Mac, the jail itself.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun seatbelt-test-profile (&key policy (proxy-port 47001) (folder "/Users/u/proj/"))
  "The profile for a fixed home, folder and binary, paths taken as written."
  (nle::sandbox-seatbelt-profile :home "/Users/u/.nodecode/" :folder folder :user-home "/Users/u/"
                                 :binary "/opt/nc/nodecode" :policy policy
                                 :port 47002 :proxy-port proxy-port
                                 :run "/Users/u/.nodecode/sandbox/run/"
                                 :cache "/Users/u/.nodecode/sandbox/cache/"
                                 :temporary "/Users/u/.nodecode/sandbox/tmp/"
                                 :path (lambda (path) (string-right-trim "/" (uiop:native-namestring path)))
                                 :exists (constantly nil)))

(defun seatbelt-test-position (profile text)
  "Where TEXT first appears in PROFILE, or NIL."
  (search text profile))

(deftest sandbox-seatbelt-profile-denies-by-default ()
  (let ((profile (seatbelt-test-profile)))
    (is (search (format nil "(version 1)~%(deny default)") profile) "a deny-default profile: what no rule allows is refused")
    (is (not (search "(allow default" profile)) "and nothing turns the default back to allow")
    (is (search "(allow signal (target same-sandbox))" profile) "the jail signals only its own processes")
    (is (not (search "(allow mach" profile)) "no system service beyond the system base is opened")))

(deftest sandbox-seatbelt-profile-runs-broad-to-narrow ()
  (let* ((profile (seatbelt-test-profile :policy '(:deny-read ("/Users/u/proj/private"))))
         (system (seatbelt-test-position profile "(allow file-read* file-map-executable (subpath \"/\"))"))
         (homes (seatbelt-test-position profile "(deny file-read*"))
         (folder (seatbelt-test-position profile "(allow file-read*
  (subpath \"/Users/u/proj\")"))
         (written (seatbelt-test-position profile "(allow file-write*"))
         (refused (seatbelt-test-position profile "(deny file-read* file-write*"))
         (kept (seatbelt-test-position profile "(deny file-write*"))
         (own (seatbelt-test-position profile "(allow file-read* file-write*
  (subpath \"/Users/u/.nodecode/sandbox/run\")")))
    (is (and system homes folder written refused kept own) "every layer of the file rules is there")
    (is (and system homes folder written refused kept own
             (< system homes folder written refused kept own)) "the system, then the homes denied, the folder read back and written, the secrets refused, the launcher's files kept, the jail's own directories last: the last matching rule decides")
    (is (search "(subpath \"/Users\")" (subseq profile homes folder)) "every account's home is read-denied")
    (is (search "(subpath \"/private/var/folders\")" (subseq profile homes folder)) "and the host's per-user temporary directories")
    (is (search "(subpath \"/Users/u/.nodecode/secrets\")" (subseq profile refused kept)) "the home's secrets are refused after the home is read back")
    (is (search "(subpath \"/Users/u/.nodecode/auth.json\")" (subseq profile refused kept)) "so is its store of keys and sign-in tokens")
    (is (search (format nil "(subpath ~s)" (nle::sandbox-expand "~/.ssh")) (subseq profile refused kept)) "~/.ssh is refused even when the folder is the home it sits in")
    (is (search "(subpath \"/Users/u/proj/private\")" (subseq profile refused kept)) "a path the policy denies is refused inside the folder")
    (is (search "(subpath \"/Users/u/.nodecode/config.jsonc\")" (subseq profile kept own)) "what the launcher reads is write-denied")
    (is (search "(subpath \"/Users/u/.nodecode/sandbox\")" (subseq profile kept own)) "the profile and the mark under sandbox/ are not the jail's to write")
    (is (search "(subpath \"/opt/nc/nodecode\")" (subseq profile kept own)) "nor the binary that runs the jail")
    (is (search "(literal \"/Users/u\")" profile) "the folder's parents can be stat'ed, so its path resolves")
    (is (not (search "(subpath \"/Users/u\")" (subseq profile folder))) "but the account's home itself is never read back")))

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
    (is (search "(allow network-outbound (remote ip \"localhost:47001\"))" profile) "the one way out is the launcher's proxy")
    (is (= 2 (count-matches "(allow network-outbound" profile)) "and its own port; no other outbound rule")
    (is (not (search "(remote ip \"*" profile)) "no address outside loopback"))
  (let ((profile (seatbelt-test-profile :proxy-port nil)))
    (is (= 1 (count-matches "(allow network-outbound" profile)) "with no proxy the jail reaches nothing but its own gateway")))

(defun count-matches (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK."
  (loop for start = 0 then (+ found (length needle))
        for found = (search needle haystack :start2 start)
        while found count t))

(deftest sandbox-seatbelt-strings-are-escaped ()
  (is (string= "\"/a \\\"b\\\\c\"" (nle::seatbelt-string "/a \"b\\c")) "a quote or backslash in a path cannot end the string early"))

(deftest sandbox-seatbelt-path-resolves-what-is-not-there-yet ()
  (let* ((root (uiop:ensure-directory-pathname
                (format nil "~a/seatbelt-~d" (string-right-trim "/" (uiop:native-namestring (uiop:temporary-directory)))
                        (random 1000000))))
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

(deftest sandbox-seatbelt-wrap-runs-the-serve-under-sandbox-exec ()
  (let ((argv (nle::sandbox-seatbelt-wrap '("/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
                                          :profile "/h/.nodecode/sandbox/seatbelt.sb" :port 47002
                                          :run "/h/.nodecode/sandbox/run/" :cache "/h/.nodecode/sandbox/cache/"
                                          :temporary "/h/.nodecode/sandbox/tmp/" :proxy-port 47001)))
    (is (equal '("env" "-u" "NODECODE_PRISTINE" "NODECODE_SANDBOXED=1" "NODECODE_GATEWAY_PORT=47002"
                 "XDG_RUNTIME_DIR=/h/.nodecode/sandbox/run" "XDG_CACHE_HOME=/h/.nodecode/sandbox/cache"
                 "TMPDIR=/h/.nodecode/sandbox/tmp")
               (subseq argv 0 8)) "env: the port, and the jail's own runtime, cache and temporary directories")
    (is (member "HTTPS_PROXY=http://127.0.0.1:47001" argv :test #'string=) "web traffic goes to the proxy")
    (is (member "https_proxy=http://127.0.0.1:47001" argv :test #'string=) "in the spelling curl reads too")
    (is (equal '("/usr/bin/sandbox-exec" "-f" "/h/.nodecode/sandbox/seatbelt.sb"
                 "/opt/nc/nodecode" "serve" "--store" "/h/a b/store.db")
               (subseq argv (position "/usr/bin/sandbox-exec" argv :test #'string=))) "sandbox-exec applies the profile to the serve argv, word for word")))

(deftest sandbox-seatbelt-profile-gives-the-jail-its-store-wherever-it-lies ()
  (let ((profile (nle::sandbox-seatbelt-profile :home "/Users/u/.nodecode/" :folder nil :user-home "/Users/u/"
                                                :store "/Users/Shared/run/smoke/history.db" :port 47002
                                                :path (lambda (path) (string-right-trim "/" (uiop:native-namestring path)))
                                                :exists (constantly nil))))
    (is (search "(allow file-read* file-write* (regex #\"^/Users/Shared/run/smoke/history\\.db([-.][^/]*)?$\"))" profile) "the store and its -wal, -shm, .lock and .owner sidecars, and nothing else of its directory")
    (is (search "(literal \"/Users/Shared/run/smoke\")" profile) "its directory can be stat'ed, so the path resolves")
    (is (not (search "(subpath \"/Users/Shared/run/smoke\")" profile)) "but is never read or written whole")))

(deftest sandbox-seatbelt-wrap-runs-a-folderless-jail-in-its-home ()
  (let ((argv (nle::sandbox-seatbelt-wrap '("/opt/nc/nodecode" "serve") :profile "/h/sandbox/seatbelt.sb" :port 47002
                                          :directory "/h/")))
    (is (equal '("/usr/bin/sandbox-exec" "-f" "/h/sandbox/seatbelt.sb" "/bin/sh" "-c" "cd \"$0\" && exec \"$@\"" "/h" "/opt/nc/nodecode" "serve") (subseq argv (position "/usr/bin/sandbox-exec" argv :test #'string=))) "it starts in its home, never in a directory the jail cannot read, then execs the serve argv whole")))

(deftest sandbox-seatbelt-plan-wraps-and-hands-in-no-listener ()
  (let ((nle::*sandbox-plan* (list :seatbelt "/h/.nodecode/sandbox/seatbelt.sb" :port 47002
                                   :run "/h/.nodecode/sandbox/run/" :cache "/h/.nodecode/sandbox/cache/"
                                   :temporary "/h/.nodecode/sandbox/tmp/")))
    (is (member "/usr/bin/sandbox-exec" (nle::sandbox-wrap-command '("/opt/nc/nodecode" "serve")) :test #'string=) "a Seatbelt plan wraps the serve in sandbox-exec, not srt")
    (is (null (nle::call-with-sandbox-stdout #'identity)) "and hands it no listener: the jailed gateway binds its own loopback port")))

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
