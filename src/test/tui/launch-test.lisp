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

;;; `nodecode gateway setup': GATEWAY-SETUP-TEXT over a fake control
;;; surface, the organism's routes as the walk calls them; its terminal is the
;;; ASK functions. The fake installs the cell, keeps the secret, refuses one
;;; token at the check and offers two channels.

(defun setup-fake-object (&rest members)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on members by #'cddr do (setf (gethash key object) value))
    object))

(defun setup-fake-organism (&key offer &aux (calls '()) installed secret chosen restarted)
  "A control surface for the walk: (values CALL CALLS), CALLS answering every
call made so far, oldest first, as (METHOD PATH BODY-JSON). OFFER, when
given, answers a field's choices probe from the field's name, and the owner
field offers choices too."
  (labels ((field (name type &rest more)
             (apply #'setup-fake-object "name" name "type" type "doc" (format nil "~a doc" name) more))
           (section ()
             (setup-fake-object
              "key" "channels.discord" "path" (vector "channels" "discord")
              "guide" "make an app; copy the token" "check" t
              "needs" (if (and secret chosen) "complete" "still needed: bot token, allowed_channels or allowed_users")
              "secret" (setup-fake-object "noun" "bot token" "env" "bot_token_env" "file" "bot_token_file")
              "fields" (vector (field "bot_token_env" "env")
                               (field "bot_token_file" "path" "set" (and secret t) "essential" (and secret t))
                               (field "allowed_channels" "list" "essential" t "choices" t "set" (and chosen t))
                               (field "allowed_users" "list" "essential" t)
                               (field "owner" "list" "choices" (and offer t))
                               (field "pairing" "boolean" "default" t))))
           (call (method path body)
             (push (list method path (and body (nlk:encode-json-object body))) calls)
             (cond ((equal path "/api/gateway/cells")
                    (when (eq method :post) (setf installed t))
                    (setup-fake-object "text" (and (eq method :post) "nodecode-channel-discord is on")
                                       "cells" (vector (setup-fake-object "name" "nodecode-channel-discord" "on" installed)
                                                       (setup-fake-object "name" "nodecode-channel-kit" "on" t)
                                                       (setup-fake-object "name" "nodecode-channel-telegram" "on" t))))
                   ((equal path "/api/channels")
                    (setup-fake-object "channels" (vector (setup-fake-object "id" "telegram" "state" "running" "connected" t)
                                                          (setup-fake-object "id" "discord" "state" (if restarted "running" "unconfigured")
                                                                             "connected" restarted "detail" "as nodebot"))))
                   ((equal path "/api/channels?op=restart") (setf restarted t) (setup-fake-object "channels" #()))
                   ((equal path "/api/gateway/config")
                    (when (eq method :post)
                      (cond ((gethash "secret" body) (setf secret (gethash "secret" body)))
                            ((equal "allowed_channels" (aref (gethash "path" body) 2))
                             (setf chosen (coerce (gethash "value" body) 'list)))))
                    (setup-fake-object "said" (and (eq method :post) "saved it")
                                       "sections" (if installed (vector (section)) #())))
                   ((equal path "/api/gateway/config/probe")
                    (cond ((and offer (gethash "field" body)) (funcall offer (gethash "field" body)))
                          ((gethash "field" body)
                           (setup-fake-object "choices" (vector (setup-fake-object "id" "111" "label" "#general (Home)")
                                                                (setup-fake-object "id" "222" "label" "#bots (Home)"))))
                          ((equal secret "good-token") (setup-fake-object "text" "token: ok — bot nodebot"))
                          ((equal secret "bad-token") (setup-fake-object "text" "token: unauthorized (401): the token is wrong"))
                          (t (error 'nle::gateway-http-error :status 400 :code "refused"
                                                            :detail "channels.discord is unreadable"))))
                   (t (error "the fake organism has no ~a ~a" method path)))))
    (values #'call (lambda () (reverse calls)))))

(defun setup-walk (words answers secrets)
  "GATEWAY-SETUP-TEXT over a fresh fake, ANSWERS and SECRETS the lines typed:
(values TEXT CODE STREAM NARRATION CALLS)."
  (multiple-value-bind (call calls) (setup-fake-organism)
    (let ((out (make-string-output-stream)))
      (multiple-value-bind (text code stream)
          (nle::gateway-setup-text words :call call :keep-running nil :out out
                                         :ask (lambda (question) (declare (ignore question)) (pop answers))
                                         :ask-secret (lambda (question) (declare (ignore question)) (pop secrets)))
        (values text code stream (get-output-stream-string out) (funcall calls))))))

(deftest launch-gateway-setup-picks-by-number-range-name-or-all ()
  (let ((choices '(("111" . "#general  Home") ("222" . "#bots  Home") ("333" . "#ops  Away") ("444" . "@mods  Home"))))
    (is (equal '("111" "222" "333" "444") (nle::gateway-setup-picks "all" choices)) "all picks every one")
    (is (equal '("111" "222" "333" "444") (nle::gateway-setup-picks " * " choices)))
    (is (equal '("222" "333") (nle::gateway-setup-picks "2-3" choices)) "a range of numbers")
    (is (equal '("111" "333") (nle::gateway-setup-picks "general, #ops" choices)) "names, the # optional")
    (is (equal '("444" "222") (nle::gateway-setup-picks "@mods 2 2" choices)) "each once, in the order named")
    (is (equal '("999" "5") (nle::gateway-setup-picks "999 5" choices)) "an id, or a number past the list, as typed")
    (is (equal '("2") (nle::gateway-setup-picks "2" nil)) "no choices: words as typed")))

(deftest launch-gateway-setup-waits-for-a-server-then-takes-all-and-the-owner-default ()
  ;; The token's bot is in no server for two asks: the walk says how to add it
  ;; and waits; then `all' picks every channel, and an Enter at the unset
  ;; owner takes the default the choices name.
  (let ((asks 0) (questions '()) (answers (list "" "all" "" "")))
    (multiple-value-bind (call calls)
        (setup-fake-organism
         :offer (lambda (field)
                  (cond ((equal field "owner")
                         (setup-fake-object "choices" (vector (setup-fake-object "id" "77" "label" "peas  made the app")
                                                              (setup-fake-object "id" "88" "label" "ada  Home"))
                                            "default" (vector "77")))
                        ((< (incf asks) 3)
                         (setup-fake-object "choices" #() "wait" "nodebot is in no server yet. Add it to one: open the link"))
                        (t (setup-fake-object "choices" (vector (setup-fake-object "id" "111" "label" "#general  Home")
                                                                (setup-fake-object "id" "222" "label" "#bots  Home")))))))
      (let ((out (make-string-output-stream))
            (nle::*gateway-setup-poll-seconds* 0))
        (multiple-value-bind (text code)
            (nle::gateway-setup-text '("discord") :call call :keep-running nil :out out
                                     :ask (lambda (question) (push question questions) (pop answers))
                                     :ask-secret (constantly "good-token"))
          (let ((said (get-output-stream-string out))
                (made (funcall calls)))
            (is (= 0 code)) (is (search "discord is connected" text))
            (is-carrying (text said)
              "nodebot is in no server yet. Add it to one: open the link" "Ctrl+C stops, and what is saved stays"
              (" done." "the wait ends when there is something to pick")
              ("What the token sees now:" "after a wait, the check again, before the channels start")
              ("#general  Home" "a choice by its name") (:absent "111" "never its id")
              ("  1  make an app" "the guide's steps are numbered"))
            (is (= 3 asks) "asked again until the server was there")
            (flet ((wrote (json) (member json made :key #'third :test #'equal)))
              (is (wrote "{\"path\":[\"channels\",\"discord\",\"allowed_channels\"],\"value\":[\"111\",\"222\"]}") "all wrote every channel")
              (is (wrote "{\"path\":[\"channels\",\"discord\",\"owner\"],\"value\":[\"77\"]}") "an Enter at the unset owner took its default"))
            (let ((asked "Numbers, a range like 2-5, all, or names, comma-separated; Enter takes peas:"))
              (is (member asked questions :test #'equal) "the question names what an Enter takes"))))))))

(deftest launch-gateway-setup-walks-a-channel-to-connected ()
  (multiple-value-bind (text code stream said calls) (setup-walk '() '("1" "n" "" "2" "" "") '("refused-token" "bad-token" "good-token"))
    (is (= 0 code)) (is (eq :output stream))
    (is (search "discord is connected: as nodebot" text))
    (is (search "/channels pair CODE" text) "pairing is on by default, so the answer says how to let someone in")
    (is (search "The check refused it: channels.discord is unreadable" said) "a refused check is said in the organism's words")
    (is (search "token: unauthorized (401)" said) "what a wrong token sees is shown, for the operator to decline")
    (is (null (search "checks out" said)) "the report is never called a pass")
    (is (null (or (search "good-token" said) (search "good-token" text))) "the token is never echoed")
    (flet ((made (method path &optional body)
             (member (list method path body) calls :test #'equal)))
      (is (made :post "/api/gateway/cells" "{\"name\":\"nodecode-channel-discord\",\"on\":true,\"seconds\":600}") "the cell is switched on")
      (is (made :post "/api/gateway/config" "{\"path\":[\"channels\",\"discord\"],\"secret\":\"good-token\"}") "the second paste is kept by the organism, by reference")
      (is (made :post "/api/gateway/config" "{\"path\":[\"channels\",\"discord\",\"allowed_channels\"],\"value\":[\"222\"]}") "a number picks the id the token's choices offered")
      (is (< (position '(:post "/api/gateway/config" "{\"path\":[\"channels\",\"discord\",\"allowed_channels\"],\"value\":[\"222\"]}")
                       calls :test #'equal)
             (position '(:post "/api/channels?op=restart" nil) calls :test #'equal)) "the channels start again after the section is complete")))
  (is-values (text code stream) (setup-walk '() '() '())
    (code = 1 "no line to read stops the walk") (stream eq :error) ((search "setup stopped" text) is))
  (is-values (text code) (setup-walk '("irc") '() '())
    (code = 2 "a channel not on offer is a bad invocation") ((search "no channel irc here; there are discord, telegram" text) is))
  (is (search "setup" (nlk:gateway-text "help" :command "nodecode gateway")) "help lists setup")
  (is (search "command line's own"
              (handler-case (nlk:gateway-text "setup" :command "/gateway")
                (nlk:gateway-usage-error (refusal) (princ-to-string refusal)))) "/gateway setup points at the terminal"))
