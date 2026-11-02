;;;; launch-test.lisp --- the executable's command line: the tables, help,
;;;; the nearest word, -p anywhere, --version, and the profile command's
;;;; answer with its question on a terminal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The parser is a pure function of its words; help and the version exit
;;;; in MAIN, so they are asserted on the plan and on LAUNCH-HELP-TEXT, the
;;;; text MAIN prints. PROFILE-ANSWER is MAIN's profile arm without the exit, its
;;;; terminal replaced by a function the test answers through.

(in-package #:nodecode.test)

(deftest launch-help-is-a-command-and-a-flag ()
  (dolist (line '(("--help") ("-h") ("help")))
    (is (equal '(:help nil) (second (apply #'launch-plan line)))
        (format nil "~{~a~^ ~} asks for the whole help" line)))
  (is-table (line expected) (equal 'expected (second (apply #'launch-plan 'line)))
    (("help" "profile") (:help "profile") "help COMMAND")
    (("help" "profile" "create") (:help "profile" "create") "help profile VERB")
    (("help" "profiles") (:help "profile") "an alias names its command")
    (("serve" "--help") (:help "serve") "COMMAND --help")
    (("--help" "profile") (:help "profile") "--help ahead of the command")
    (("resume" "--help") (:help "resume")
     "a command's help is never read as its argument")
    (("import" "--help") (:help "import") "nor as import's source")
    (("profile" "--help") (:profile ("--help"))
     "the profile command reads its own --help")
    (("--version") (:version) "--version")
    (("--help" "--version") (:help nil) "help outranks the version"))
  (is (search "Did you mean \"profile\"?" (launch-refusal "help" "prfile"))))

(deftest launch-help-text-comes-from-the-tables ()
  (let ((text (nle::launch-help-text)))
    (is (uiop:string-prefix-p "usage: nodecode [-p NAME] [COMMAND] [OPTIONS]" text))
    (dolist (verb nle::*launch-verbs*)
      (if verb.internal
          (is (null (search (format nil "~%  ~a " verb.name) text))
              (format nil "internal ~a is not listed" verb.name))
          (is (search verb.summary text) (format nil "~a is listed with its summary" verb.name))))
    (is (search "-p, --profile NAME" text) "an option shows its short spelling")
    (is (search "profile [VERB]" text) "a label replaces the name and arguments"))
  (is-table (usage topic) (uiop:string-prefix-p usage (apply #'nle::launch-help-text topic))
    ("usage: nodecode serve [--store PATH]" '("serve") "one command's help leads with its usage")
    ("usage: nodecode profile [VERB]" '("profile")
     "the profile command's help is the profile verbs'")
    ("usage: nodecode profile create NAME" '("profile" ("create")) "and one verb's")
    ("usage: nodecode --store PATH" '("--store") "an option has its own help too"))
  (is-carrying (refusal (launch-refusal "import" "not-a-world"))
    "nodecode import [WORLD] [--source PATH]"
    "is not a world this build reads"))

(deftest launch-aliases-suggestions-and-short-refusals ()
  (is (equal '(:profile ()) (second (launch-plan "profiles"))) "profiles is profile")
  (is (equal '(:profile ("create" "x")) (second (launch-plan "profiles" "create" "x"))))
  (is (equal "no command \"prfile\". Did you mean \"profile\"?" (launch-refusal "prfile" "list")))
  (is (equal "no option \"--stor\". Did you mean \"--store\"?" (launch-refusal "--stor" "/tmp/x")))
  (is (equal "no command \"trail\"." (launch-refusal "trail")))
  (is (equal "no command \"zzz\"." (launch-refusal "zzz")) "nothing close, no guess")
  (is (equal (format nil "nodecode: no command \"zzz\".~%Run \"nodecode --help\" to see every command.~%")
             (nle::usage-refusal-text "no command \"zzz\"."))))

(deftest launch-add-takes-the-rest-of-the-line ()
  (is (equal '(:add ("nc://cline" "--yes")) (second (launch-plan "add" "nc://cline" "--yes"))) "the rest is add's")
  (is (search "add is a command of its own" (launch-refusal "--ephemeral" "add" "nc://cline")))
  (is (uiop:string-prefix-p "usage: nodecode add nc://NAME [--yes]" (nle::launch-help-text "add"))))

(deftest launch-profile-flag-reads-anywhere ()
  (is (equal "coder" (seventh (launch-plan "-p" "coder"))) "ahead of the command")
  (let ((plan (launch-plan "profile" "list" "-p" "coder")))
    (is (equal '(:profile ("list")) (second plan)) "after it, taken out of the profile words")
    (is (equal "coder" (seventh plan)) "and still the launch's"))
  (is (equal "coder" (seventh (launch-plan "serve" "--profile" "coder"))) "the long spelling")
  (is (search "second profile" (launch-refusal "-p" "a" "-p" "b")) "one per launch")
  (is (search "needs a NAME" (launch-refusal "serve" "-p")) "a missing name")
  (is (search "not a profile name" (launch-refusal "-p" "Nope")) "a name no profile could have"))

(deftest launch-terminal-answer-asks-only-a-terminal ()
  #-win32
  (let ((out (make-string-output-stream)))
    (is (equal "y" (nle::terminal-answer "Delete it? [y/N]"
                                        :input (make-string-input-stream (format nil "y~%"))
                                        :output out :interactive t)))
    (is (equal "Delete it? [y/N] " (get-output-stream-string out))))
  (is (null (nle::terminal-answer "Delete it? [y/N]"
                                  :input (make-string-input-stream "y")
                                  :output (make-broadcast-stream) :interactive nil)))
  (is (equal '(t t nil nil) (mapcar #'nle::yes-answer-p '("y" " YES " "" "n"))) "Enter is no"))

(deftest launch-profile-answer-asks-before-a-delete ()
  (nlk:profile-create "coder")
  (is-values (text code stream) (nle::profile-answer '("delete" "coder") :ask (constantly nil))
    (code = 1 "no terminal: refused") (stream eq :error)
    ((search "\"nodecode profile delete coder --yes\" means it" text) is "saying how to answer"))
  (is-values (text code) (nle::profile-answer '("delete" "coder") :ask (constantly "n"))
    (code = 1 "a no keeps it") (text "nothing was deleted"))
  (is (nlk:find-profile "coder") "still there")
  (let ((asked nil))
    (is-values (text code stream)
        (nle::profile-answer '("delete" "coder") :ask (lambda (question) (setf asked question) "y"))
      ((search "[y/N]" asked) is "the question names the stakes") (code = 0) (stream eq :output)
      (text "deleted coder")))
  (is (null (nlk:find-profile "coder")) "a yes deleted it")
  (multiple-value-bind (text code)
      (nle::profile-answer '("delete" "ghost")
                           :ask (lambda (question) (declare (ignore question)) (error "asked")))
    (is (= 1 code) "a missing profile is refused before anything is asked")
    (is (search "no profile \"ghost\"" text)))
  (is-values (text code) (nle::profile-answer '("lsit"))
    (code = 2 "a mistyped verb is a bad invocation") ((search "Did you mean \"list\"?" text) is)
    ((search "Run \"nodecode profile help\" to see the verbs." text) is))
  (is-values (text code) (nle::profile-answer '("--help"))
    (code = 0) ((uiop:string-prefix-p "usage: nodecode profile [VERB]" text) is)))

;;; --- the commands that need a terminal -------------------------------------

(deftest launch-table-says-which-commands-need-a-terminal ()
  (is (equal '("setup" "import" "resume") (loop for verb in nle::*launch-verbs* when verb.terminal collect verb.name)) "only the commands that open the shell")
  ;; The declaration and the parser cannot drift: a command's line launches
  ;; the shell exactly when its table entry says it draws on the terminal.
  (dolist (verb nle::*launch-verbs*)
    (let* ((extra (cdr (assoc verb.name '(("resume" "s-1") ("warm-dump" "/x") ("machine" "link")) :test #'string=)))
           (plan (apply #'launch-plan verb.name extra)))
      (is (eq (and verb.terminal t) (and (null (first plan)) (null (second plan)))) (format nil "~a: the table and the parser agree" verb.name)))))

(deftest a-launch-that-needs-a-terminal-refuses-where-there-is-none ()
  (let ((nlt::*terminal-device* "/nonexistent/tty")
        (refusal "nodecode ~@[~a ~]needs a terminal (no controlling tty); run it in one."))
    (is-table (child serve args expected) (equal expected (nle::terminal-refusal 'child 'serve 'args))
      (nil nil ("import") (format nil refusal "import") "import")
      (nil nil ("import" "--yes" "--dry-run") (format nil refusal "import")
       "whatever import is given")
      (nil nil ("setup") (format nil refusal "setup") "setup")
      (nil nil ("resume" "s-1") (format nil refusal "resume") "resume")
      (nil nil ("--ephemeral" "setup") (format nil refusal "setup") "options ahead of the command")
      (nil nil () (format nil refusal nil) "the bare launch")
      (nil nil ("--store" "x.db") (format nil refusal nil) "the bare launch on a named store")
      (nil t ("serve") nil "a headless owner draws nothing")
      (nil :wait ("gateway" "run") nil "nor does the gateway")
      ((:web ()) nil ("web") nil "web answers on stdout")
      ((:profile ("list")) nil ("profile" "list") nil "profile answers on stdout")
      ((:help nil) nil ("help") nil "help answers on stdout")
      ((:version) nil ("--version") nil "the version answers on stdout")))
  #-win32
  (let ((nlt::*terminal-device* "/dev/null"))
    (is (null (nle::terminal-refusal nil nil '("import"))) "a terminal that opens is no refusal")
    (is (null (nle::terminal-refusal nil nil '())))))

;;; --- the home, asked once before anything boots ---------------------------------
;;; A launch over a home it cannot use used to open the wizard over a dead
;;; organism and walk on; HOME-REFUSAL says one sentence instead, and MAIN
;;; prints it and exits before a shell paints or anything is made.

(defmacro is-refusal ((form &rest paths) sentence)
  "Assert FORM answers SENTENCE, each ~a of it one of PATHS written from the home."
  `(is (equal (format nil ,sentence ,@(mapcar (lambda (path) `(nle::home-abbreviated ,path)) paths))
              ,form)))

(deftest launch-home-refusal-lets-a-usable-home-through ()
  (is (null (nle::home-refusal nil)) "a home not made yet is made at boot")
  (ensure-directories-exist nlk::*home*)
  (is (null (nle::home-refusal nil)) "a home that is there")
  (is (null (nle::home-refusal (merge-pathnames "later/x.db" nlk::*home*))) "a store whose folder is made at boot")
  (is (null (uiop:directory-files nlk::*home*)) "the probe leaves nothing behind")
  (is (null (uiop:subdirectories nlk::*home*)) "nor a folder")
  (dolist (text (list (format nil "{ // kept~%  \"general\": { \"theme\": \"x\" } }") "" (format nil "// nothing yet~%")))
    (with-temp-file (nle::*shared-config-path* :setf t :type "jsonc" :contents text)
      (is (null (nle::home-refusal nil)) (format nil "a config that reads or is blank: ~s" text)))))

(deftest launch-home-refusal-names-a-config-that-does-not-parse ()
  (flet ((refusal (text)
           (with-temp-file (nle::*shared-config-path* :setf t :type "jsonc" :contents text)
             (values (nle::home-refusal nil) nle::*shared-config-path*))))
    (multiple-value-bind (text path) (refusal "{ \"general\": { \"default_model\": { \"provider\": \"a\" ")
      (is-carrying text
        (is (search (nle::home-abbreviated (uiop:native-namestring path)) text) "the file is named")
        ".jsonc is not valid JSONC (byte "
        "; fix it or move it aside, then run nodecode setup"
        (:absent "SB-" "no condition text")
        (:absent "#<" "no object")))
    (is-carrying (text (refusal "[1]"))
      "cannot be read (JSON root is not an object)"
      "then run nodecode setup")))

(deftest launch-home-refusal-says-what-stops-the-write ()
  (ensure-directories-exist nlk::*home*)
  (let ((home (nle::home-abbreviated (string-right-trim "/" (namestring nlk::*home*)))))
    (flet ((refusal (errno &optional store-path)
             (with-stubbed-fdefinition (nle::folder-write-errno (folder) errno)
               (nle::home-refusal store-path))))
      ;; EROFS, EACCES and ENOSPC by number: the values every platform's C runtime agrees on
      (is-carrying (text (refusal 30))
        (is (search (format nil "~a is read-only; nodecode needs to write there" home) text) "the home is named")
        "set NODECODE_HOME to another folder"
        (:absent "SB-" "no condition text"))
      (is-carrying (text (refusal 13)) "is not writable by this account")
      (is-carrying (text (refusal 28)) "is not writable (no space left on device)"))
    ;; the store's folder is judged apart from the home, by the nearest folder that exists
    (with-stubbed-fdefinition (nle::folder-write-errno (folder)
                               (unless (equal (namestring folder) (namestring nlk::*home*))
                                 30))
      (is-carrying (text (nle::home-refusal
                          (merge-pathnames "no-such/deeper/x.db" (temp-path "store-side"))))
        "/no-such/deeper does not exist and "
        " is read-only, so nodecode cannot make it; make "
        "name a store elsewhere with --store PATH"
        (:absent "SB-")))))

#-win32
(deftest launch-home-refusal-names-what-stands-where-the-home-belongs ()
  ;; a file, or a link to a folder that is not there: no mkdir gets past either,
  ;; and the boot used to die on it behind the setup walk (ip-03)
  (with-temp-directory (dir "home-shape")
    (let* ((home (merge-pathnames "home/" dir))
           (native (string-right-trim "/" (namestring home))))
      (flet ((refusal () (let ((nlk::*home* home)) (nle::home-refusal nil))))
        (write-temp-file native "x")
        (is-refusal ((refusal) native)
          "~a is a file, not a folder; move it aside or set NODECODE_HOME to another folder")
        (delete-file native)
        (dolist (target (list (namestring (merge-pathnames "gone" dir)) "/nonexistent/deep/dir"))
          (nlk:make-symlink target native)
          (is-refusal ((refusal) native target)
            "~a links to ~a, which does not exist; create that folder or remove the ~
             link, or set NODECODE_HOME to another folder")
          (is (not (probe-file target)) "nothing is made where the link points"))
        ;; a link that is there but cannot be followed, a loop: not called
        ;; missing, and the system's words say why
        (let ((other (namestring (merge-pathnames "loop" dir))))
          (nlk:make-symlink native other)
          (nlk:make-symlink other native)
          (is-carrying (text (refusal))
            (is (search (format nil "~a links to ~a, which nodecode cannot reach ("
                                (nle::home-abbreviated native) (nle::home-abbreviated other))
                        text))
            "); fix the link or remove it, or set NODECODE_HOME to another folder"
            (:absent "does not exist"))
          (sb-posix:unlink native)
          (sb-posix:unlink other))
        ;; a store whose folder would be made under a file
        (write-temp-file native "x")
        (is-carrying (text (nle::home-refusal (merge-pathnames "deeper/x.db" home)))
          "/deeper does not exist and "
          " is a file, not a folder, so nodecode cannot make it; move "
          " aside or name a store elsewhere with --store PATH")))
    ;; the probes leave nothing behind: the one file is the test's own
    (is (equal '("home") (mapcar #'file-namestring (uiop:directory-files dir))))
    (is (null (uiop:subdirectories dir)))))

(deftest launch-home-refusal-names-a-store-it-cannot-open ()
  ;; --store naming a folder or a file this account cannot write, and the home's
  ;; own store a folder: SQLite refused each at the boot, behind the walk (ip-03)
  (ensure-directories-exist nlk::*home*)
  (with-temp-directory (dir "store-shape")
    (let ((folder (string-right-trim "/" (namestring dir))))
      (is-refusal ((nle::home-refusal folder) folder)
        "~a is a folder, not a store file; name a file, such as --store ~:*~a/history.db"))
    (let ((store (namestring (merge-pathnames "x.db" dir))))
      (is (null (nle::home-refusal store)) "a store not made yet is made at boot")
      (write-temp-file store)
      (is (null (nle::home-refusal store)) "an empty file is a store SQLite makes")
      #-win32
      (let ((link (namestring (merge-pathnames "link.db" dir))))
        (sb-posix:symlink (namestring (merge-pathnames "later.db" dir)) link)
        (is (null (nle::home-refusal link)) "a link to a store not made yet: the boot makes it there"))
      #-win32
      (unless (zerop (sb-posix:getuid))
        (sb-posix:chmod store #o444)
        (is-refusal ((nle::home-refusal store) store)
          "~a is not writable by this account; nodecode needs to write to it, so make it ~
           writable or name a store elsewhere with --store PATH"))))
  (let ((own (namestring nlk::*default-store-path*)))
    ;; 8 KB of garbage where the home's store belongs, or one byte: what a crash
    ;; or a full disk leaves, refused before the boot runs over it (CB-102)
    (dolist (bytes (list (apply #'octets (make-list 8000 :initial-element 171)) (octets 10)))
      (alexandria:write-byte-vector-into-file bytes own :if-exists :supersede)
      (nlk:with-cleanup ((delete-file own))
        (is-refusal ((nle::home-refusal nil) own)
          "~a is not a nodecode store (not an SQLite database); move it aside and run ~
           nodecode again")))
    ;; a store SQLite wrote, in the write-ahead log the organism keeps it in, is
    ;; let through: every launch after the first reads one. The check reads the
    ;; header only; a damaged page past it is the boot's to name
    ;; (STORE-UNREADABLE, same sentence)
    (sqlite:with-open-database (db own)
      (sqlite:execute-single db "pragma journal_mode = wal")
      (sqlite:execute-non-query db "create table t (x)")
      (sqlite:execute-non-query db "insert into t values (1)"))
    (nlk:with-cleanup ((delete-file own))
      (is (null (nle::home-refusal nil))))
    (ensure-directories-exist (uiop:ensure-directory-pathname own))
    (nlk:with-cleanup ((uiop:delete-empty-directory (uiop:ensure-directory-pathname own)))
      (is-refusal ((nle::home-refusal nil) own)
        "~a is a folder, not a store file; move it aside and run nodecode again"))))

(deftest launch-store-refusal-opens-nothing-that-is-no-store-file ()
  ;; a pipe at the store's path: the header's read waited on it for ever,
  ;; before anything painted; and a --store path SBCL reads as a pattern, whose
  ;; refusal printed #<SB-IMPL::PATTERN ...>
  (ensure-directories-exist nlk::*home*)
  #-win32
  (with-temp-directory (dir "store-pipe")
    (let ((pipe (namestring (merge-pathnames "pipe.db" dir))))
      (sb-posix:mkfifo pipe #o600)
      (is-refusal ((nle::home-refusal pipe) pipe)
        "~a is a device or a pipe, not a store file; name a file with --store PATH")))
  #-win32
  (let ((own (namestring nlk::*default-store-path*)))
    (sb-posix:mkfifo own #o600)
    (nlk:with-cleanup ((delete-file own))
      (is-refusal ((nle::home-refusal nil) own)
        "~a is a device or a pipe, not a store file; move it aside and run nodecode again")))
  (dolist (named '("a*b.db" "a?b.db" "a[1].db" "a[b.db"))
    (is-carrying (text (nle::home-refusal named))
      (:= (format nil "--store ~a: nodecode cannot use a store path with *, ? or [ in it; name one without them"
                  named))))
  (is-carrying (text (nle::home-refusal (concatenate 'string (namestring (user-homedir-pathname)) "st*re.db")))
    "--store ~/st*re.db: "))

(deftest launch-a-store-sqlite-refuses-is-named-from-home ()
  ;; the boot's sentence for a store SQLite refuses is the preflight's, with
  ;; the path written from `~': the toast cut the full path's `what to do' off
  (let ((store (namestring (merge-pathnames ".nodecode/history.db" (user-homedir-pathname)))))
    (is-carrying (text (nle::plain-error-text (make-condition 'nlk::store-unreadable :path store)))
      (:= "~/.nodecode/history.db is not a nodecode store (not an SQLite database); move it aside and run nodecode again"))
    (is-carrying (text (nle::plain-error-text
                        (make-condition 'nlk::store-unreadable :path store
                                                               :reason "database disk image is malformed")))
      (:= "~/.nodecode/history.db is not a nodecode store (database disk image is malformed); move it aside and run nodecode again"))))

(deftest launch-write-probe-leaves-nothing-and-sees-a-refusal ()
  (with-temp-directory (dir "write-probe")
    (is (null (nle::folder-write-errno dir)))
    (is (null (uiop:directory-files dir)) "the probe file is gone")
    ;; root writes anywhere, so a folder that refuses is a non-root gate
    #-win32
    (unless (zerop (sb-posix:getuid))
      (sb-posix:chmod (namestring dir) #o500)
      (nlk:with-cleanup ((sb-posix:chmod (namestring dir) #o700))
        (is (eql 13 (nle::folder-write-errno dir)))
        (is-carrying (text (nle::folder-refusal dir "or hint"))
          "is not writable by this account; nodecode needs to write there, so make it writable or hint")))))

(deftest plain-error-text-says-what-the-system-said ()
  (let ((condition (signals-error sb-posix:syscall-error (sb-posix:mkdir "/no-such-dir/x" #o700))))
    ;; the system's words, not the condition, and words for the operator
    (is (equal '("no such file or directory" t) (multiple-value-list (nle::plain-error-text condition)))))
  ;; A report is the condition's own, which may be Lisp's: no words for the operator.
  (is (equal '("the store is a lie" nil)
             (multiple-value-list
              (nle::plain-error-text (make-condition 'simple-error :format-control "the store is a lie"))))))

