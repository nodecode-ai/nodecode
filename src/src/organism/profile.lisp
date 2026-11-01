;;;; profile.lisp --- profiles: several homes, one binary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A profile is a home (paths.lisp): ~/.nodecode/ is the default one and
;;;; ~/.nodecode/profiles/<name>/ a named one, each laid out the same, each
;;;; its own organism — its own config and secrets, its own store and
;;;; gateway, its own layer of definitions, its own store. This
;;;; file is the lifecycle: list, create (blank; or a clone of another
;;;; profile's settings; or of the whole organism, layer and all), delete,
;;;; make one the profile a bare launch boots, export and import as an
;;;; archive, install and update one from a git remote (a distribution). The
;;;; shells — `nodecode profile ...', /profile, a channel's slash, the model's
;;;; eval — are PROFILE-COMMAND over these verbs: one text in, one text out;
;;;; the web page's Profiles tab calls the verbs themselves, over the gateway's
;;;; routes (surface/control.lisp).
;;;;
;;;; What a home holds is +HOME-ENTRIES+, one row per entry: who owns it,
;;;; whether a clone carries it, whether an archive may ship it. Every verb
;;;; here reads that table and none carries a second copy of the answer, so
;;;; adding an entry to a home is adding a row.
;;;;
;;;; Kernel, and locked: a verb that removes a whole home is not material the
;;;; organism rewrites in itself. Every verb answers data; PROFILE-REPORT,
;;;; PROFILE-CREATED-TEXT and PROFILE-COMMAND render. Every refusal is PROFILE-ERROR with the whole
;;;; report as its text. Nothing here changes THIS process's home: a profile
;;;; is chosen at launch (`-p NAME', $NODECODE_HOME, the sticky file),
;;;; PROFILE-USE only moves where the next launch goes, and PROFILE-SWITCH
;;;; answers a plan — the binary and the argv — that replaces the process
;;;; rather than rebinding it. The shell performs that plan at its own
;;;; teardown seam; this file never touches the terminal or the store.

(in-package #:nodecode.kernel)

(define-refusal profile-error profile-fail "~a"
  "A profile verb refused; DETAIL is the whole report." :export t)

;;; NIL writes none: the hermetic test posture, and Windows, where the
;;; installer owns PATH.
(define-startup-parameter *wrapper-directory*
  #-win32 (home-path ".local/bin/")
  #+win32 nil
  "Where PROFILE-CREATE writes a profile's wrapper command — `coder' for
`nodecode -p coder' — on POSIX.")

(defparameter +wrapper-marker+ "# nodecode profile wrapper: "
  "The line a wrapper carries, so PROFILE-DELETE knows the file is ours.")

;;; A row is data: ENTRY (the name at the home's top level, a trailing slash
;;; where the code that writes it uses one), CARRIED (a clone copies it),
;;; SHARED (a `:settings' clone links it instead of copying: the
;;; credentials are the operator's, never the home's, so a second home reads
;;; the one copy -- and so is the knowledge cell, the memories and skills,
;;; which the clone links as a cell folder),
;;; SHIPPED (an export or a distribution may carry it), WHOSE (:SETTINGS what
;;; an operator built, :MANIFEST what names the profile, :STATE what this home
;;; made for itself, :DEFAULT what only the default home has) and what it is,
;;; in words.
;;;
;;; SHARED is the answer to "should a new profile inherit the memory?": not
;;; inherit — read the same folder. Memories and skills are keyed to the
;;; operator and to the project, so a copy of them is one correction kept in N
;;; places, and a store two processes write is two writers compounding state
;;; neither of them authored (Hermes, docs/user-guide/profiles: "give every
;;; agent its own profile ... agents that need shared memory should use an
;;; external memory provider instead"). Secrets and credentials are SHARED for
;;; the sharper reason: a copy of a credential is one credential with two
;;; owners. A `:all' clone copies even these — a fork of the organism wants
;;; its own history (PROFILE-CREATE).
;;;
;;; Sharing means two processes write one folder, so every write to a shared
;;; folder takes its lock, whose path resolves the link and so is one file for
;;; every home (os.lisp FOLDER-LOCK-PATH, WITH-FOLDER-LOCK).
;;;
;;; AUTH.JSON and SECRETS are shared by a clone, never shipped in an archive.
;;; Two columns on one row is exactly why four hand-kept lists drifted — they
;;; were answering different questions about the same entry in different
;;; places. The precedent is *AGENT-WORLDS* in waist/worlds.lisp: adding an
;;; entry to a home is adding a row, and a row carries no function.
(defparameter +home-entries+
  ;; ENTRY                CARRIED SHARED SHIPPED WHOSE      WHAT
  '(("config.jsonc"       t       nil    t       :settings  "the operator's settings")
    ("SOUL.md"            t       nil    t       :settings  "who this organism is")
    ("AGENTS.md"          t       nil    t       :settings  "the operator's rules for every session")
    ("themes.json"        t       nil    t       :settings  "the operator's themes")
    ("cells"             t       nil    t       :settings  "the cell folders")
    ("profile.jsonc"      nil     nil    t       :manifest  "what this profile is")
    ("history.db*"        nil     nil    nil     :state     "the store and its sidecars")
    (".locks/"            nil     nil    nil     :state     "the shared folders' write locks")
    ("composer-history"   nil     nil    nil     :state     "what was typed")
    ("usage.jsonl"        nil     nil    nil     :state     "the use ledger the index ranks by")
    ("theme-selection"    nil     nil    nil     :state     "the theme in use")
    ("keybinds.json"      nil     nil    nil     :state     "the chords as bound")
    ("operator-token"     nil     nil    nil     :state     "this home's gateway token")
    ("generations/"       nil     nil    nil     :state     "the sealed generations, a manifest each, and a restore's markers")
    ("release-pin"        nil     nil    nil     :state     "the release `nodecode update VERSION' holds this home at")
    ("distribution.json"  nil     nil    nil     :state     "where this home was installed from")
    ("gateway-off/"       nil     nil    nil     :state     "the gateway's off switch")
    ("hermes/"            nil     nil    nil     :state     "the import marker")
    ("mcp/"               nil     nil    nil     :state     "the MCP server logs")
    ("secrets"            t       t      nil     :settings  "the pasted secrets")
    ("auth.json"          t       t      nil     :settings  "the provider credentials")
    ("link"               nil     nil    nil     :state     "this machine's name at its relay, and the browsers let in")
    ("backups"            nil     nil    nil     :state     "the backups, each this home whole, keys and all")
    ("profiles"           nil     nil    nil     :default   "the named homes, under the default one")
    ("active-profile"     nil     nil    nil     :default   "the sticky file, under the default one"))
  "Every entry a home holds, one row each, and it is the only place a
question about one is answered.")

(define-error profile-usage-error (profile-error)
    ()
  ;; DETAIL ends by pointing at the help; a command line exits 2 on it, the
  ;; code for a bad invocation.
  (:documentation "A profile request its verbs cannot read: an unknown
verb, a word missing or left over.")
  (:export))

(define-error profile-needs-yes (profile-error)
    (question)
  ;; DETAIL is the refusal a shell with no terminal shows, saying how to
  ;; answer on the line with --yes; QUESTION is what a shell with a terminal
  ;; asks instead, before it runs the request again with --yes.
  (:documentation "A profile request that waits on the operator's yes.")
  (:export))

;;; The one table all three read.
(defparameter *profile-verbs*
  (list
   (make-verb "list" "every profile, the one a bare launch boots, a live one's gateway"
              :detail "One line each: * marks this process's own profile, then its home, the pid and port of an organism serving it, `boots by default' for the one a launch that names none takes, and its description.")
   (make-verb "use" "boot NAME when a launch names no profile" :arguments "NAME"
              :detail "Writes ~/.nodecode/active-profile; `use default' removes it. The process that runs it stays where it is: a profile is chosen when an organism starts.")
   (make-verb "switch" "run NAME here, now: this shell comes back in that home"
              :arguments "NAME"
              :detail "The shell gives the terminal back, closes its store and relaunches itself under `-p NAME' in the same window, keeping its pid and its place in the shell's job table. Where `use' moves the next bare launch, `switch' moves this one. Refused for the profile already running, and in a developer image, which has no installed binary to relaunch.")
   (make-verb "create" "a new home sharing your memories, skills and credentials: --blank for an empty one, --clone-all for a fork that copies them"
              :label "create NAME"
              :arguments "NAME [--blank | --clone-all] [--from NAME] [--description TEXT]"
              :detail "Carries config.jsonc, auth.json, secrets/, SOUL.md, AGENTS.md, themes and the installed cells, the knowledge cell of your memories and skills linked: the same abilities and the same memory over a fresh store and a fresh layer, because a second profile almost always wants the first one's abilities. --blank makes an empty home instead and the setup walk runs at its first launch. --clone-all carries your own cell folders too, and the layer with its git history: a fork of the organism. --from NAME clones that profile instead of this one. Never the store. On Linux and macOS a command named NAME is written into ~/.local/bin.")
   (make-verb "delete" "remove NAME's whole home: store, layer and secrets"
              :arguments "NAME [--yes]"
              :detail "Asks first on a terminal; --yes answers for a script. Refused for the default profile, for the profile this process runs, and while an organism serves it.")
   (make-verb "export" "NAME's home as a tar.gz, without its store, secrets and credentials"
              :arguments "NAME PATH [--whole]"
              :detail "The settings, SOUL.md, AGENTS.md, themes and the cell folders, a knowledge cell among them: what the home may hand on. --whole is the home to come back to: its secrets, its credentials and a copy of its store taken while it runs, beside the rest, readable by you alone. `import' brings either back as a profile.")
   (make-verb "import" "a profile from an archive export wrote"
              :label "import PATH" :arguments "PATH [--name NAME]"
              :detail "Named by the archive's profile.jsonc unless --name says otherwise; a name already taken is refused.")
   (make-verb "install" "a profile from a git repository: a distribution"
              :label "install URL" :arguments "URL [--name NAME]"
              :detail "A home laid out like any other, with a profile.jsonc naming it, cloned shallow. A repository carrying a symbolic link is refused. What the organism keeps for itself, its memory and its store, stays out of the repository.")
   (make-verb "update" "pull NAME's distribution" :arguments "NAME"
              :detail "git pull --ff-only --autostash in its home: the distribution's files move, your edits ride over the top, the cells and the store are never touched.")
   (make-verb "help" "this text, or one verb's" :arguments "[VERB]"))
  "The profile verbs, in help's order: what PROFILE-COMMAND reads, what
its help prints, what a mistyped verb is matched against.")

;;; --- the record ----------------------------------------------------------------

(define-record (profile-record (:copier nil) (:export :predicate :readers))
  "One profile as (PROFILES) reports it: NAME, its HOME, the manifest's
DESCRIPTION and SOURCE (the profile it was cloned from, or the remote it
was installed from), CREATED-AT; CURRENT-P for this process's own home,
STICKY-P for the one a launch that names none boots into; PID and PORT of
a live owner of its store, else NIL."
  (name "" :type string)
  (home nil)
  (description nil)
  (source nil)
  (created-at nil)
  (current-p nil)
  (sticky-p nil)
  (pid nil)
  (port nil))

(access (source-record profile-record))

;;; --- the manifest ----------------------------------------------------------------

(defun json-object-file (path)
  "The JSONC file at PATH as a hash table, or NIL: absent, unreadable, or
not an object."
  (let ((object (ignore-errors (nlk:decode-json (normalize-jsonc (read-text path))))))
    (and (hash-table-p object) object)))

(defun read-manifest (home)
  "HOME's profile.jsonc as a hash table, or NIL (JSON-OBJECT-FILE)."
  (json-object-file (merge-pathnames "profile.jsonc" home)))

(defun update-json-file (path &rest pairs)
  "The JSON object file at PATH with PAIRS (key value ...) laid over what it
holds, a NIL value taking its member off, written back. => the object."
  (let ((object (or (json-object-file path) (make-hash-table :test #'equal))))
    (loop for (key value) on pairs by #'cddr
          do (if (null value)
                 (remhash key object)
                 (setf (gethash key object) value)))
    (with-output-file (out path)
      (write-line (encode-json-object object) out))
    object))

(defun write-manifest (home &rest pairs)
  "HOME's profile.jsonc with PAIRS (key value ...) laid over what it holds;
a NIL value removes the member. => the object written."
  (apply #'update-json-file (merge-pathnames "profile.jsonc" home) pairs))

(defun manifest-list (home key)
  "KEY's list out of HOME's manifest — what PROFILE-CREATE recorded — or NIL
for a home whose manifest does not say."
  (loop for name across (json-array (read-manifest home) key)
        when (stringp name) collect name))

;;; --- the roster -------------------------------------------------------------------

(defun profile-facts (name home)
  (let ((manifest (read-manifest home))
        (stamp (json-object-file (merge-pathnames "distribution.json" home)))
        (sticky (sticky-profile)))
    (multiple-value-bind (verdict pid port) (store-death-verdict (home-store-path home))
      (make-profile-record
       :name name
       :home (uiop:ensure-directory-pathname home)
       :description (json-value manifest :string "description")
       :source (or (json-value stamp :string "source")
                   (json-value manifest :string "source"))
       :created-at (json-value manifest :string "created_at")
       :current-p (equal (namestring (uiop:ensure-directory-pathname home))
                     (namestring (uiop:ensure-directory-pathname *home*)))
       :sticky-p (if (string= name "default") (null sticky) (equal name sticky))
       :pid (and (eq verdict :live) pid)
       :port (and (eq verdict :live) port)))))

(defun profiles ()
  "Every profile as a PROFILE-RECORD: the default home first, then each
named one — which is this process's own, which a bare launch boots, which
has a live gateway. A named profile exists when its folder does."
  (cons (profile-facts "default" (default-home))
        (let ((root *profiles-directory*))
          (when (and root (uiop:directory-exists-p root))
            (loop for home in (sort (uiop:subdirectories root) #'string< :key #'folder-name)
                  for name = (folder-name home)
                  when (profile-name-p name)
                    collect (profile-facts name home))))))

(defun check-profile-name (name)
  (unless (profile-name-p name)
    (profile-fail "~s is not a profile name: a-z, 0-9, _ and -, 64 at most, ~
                   starting with a letter or digit" name))
  name)

(defun find-profile (name)
  "NAME's record, or NIL when no such profile exists."
  (check-profile-name name)
  (cond ((string= name "default") (profile-facts "default" (default-home)))
        ((uiop:directory-exists-p (profile-home name))
         (profile-facts name (profile-home name)))))

(defun require-profile (name)
  "NAME's record, or a refusal naming the nearest profile there is."
  (or (find-profile name)
      (profile-fail "~a" (unknown-word-text
                          "profile" name
                          (mapcar (lambda (record) (make-verb (profile-record-name record) ""))
                                  (profiles))))))

;;; --- the wrapper -----------------------------------------------------------------

(defun wrapper-path (name)
  (and *wrapper-directory* (merge-pathnames name *wrapper-directory*)))

(defun write-wrapper (name &aux (path (wrapper-path name))
                                (binary (organism-binary)))
  "The wrapper command for NAME: `#!/bin/sh', the marker, an exec of the
organism's binary with `-p NAME'."
  ;; => its path, or NIL when there is nowhere
  ;; to write it or no binary to exec.
  (when (and path binary
             ;; A command of the operator's own under that name stays.
             (or (not (probe-file path)) (wrapper-owned-p path name)))
    (with-output-file (out path)
      (format out "#!/bin/sh~%~a~a~%exec ~s -p ~a \"$@\"~%" +wrapper-marker+ name binary name))
    #-win32 (ignore-errors (sb-posix:chmod (uiop:native-namestring path) #o755))
    path))

(defun wrapper-owned-p (path name &aux (text (and path (read-text path))))
  (and text
       (search (format nil "~a~a~%" +wrapper-marker+ name) text)
       t))

;;; --- create --------------------------------------------------------------------------

(defun drop-cloned-channels (home &aux (path (merge-pathnames "config.jsonc" home)))
  "Take the `channels' member out of the config.jsonc a clone just copied
into HOME, and answer the member's name when there was one to take out."
  ;; A bot token belongs to one profile: two homes polling one Telegram token
  ;; answer each other's 409 (channels/telegram/adapter.lisp) and a Discord bot
  ;; cannot hold two gateways at once. Hermes strips every channel key from a
  ;; clone for the same reason — "a bot can only belong to one profile". The
  ;; new home configures its own bot (/settings, or \"set up discord\" in its
  ;; shell) or runs with no channel at all, which is the honest default: a
  ;; second home that inherits the first one's bot is a home whose messages
  ;; arrive wherever the first one is not.
  ;;
  ;; The write is the config writer's (NLK:SET-JSONC-PATH), so every comment
  ;; and every other byte of the operator's document is kept and only the
  ;; member goes.
  (when-let (text (read-text path))
    (bind (((new-text _ _ removed)
            (ignore-errors (set-jsonc-path text '("channels") :remove))))
      (when removed
        (write-file-atomically path new-text :mode :private)
        (first removed)))))

(defun git-clone (from to &key depth)
  "git clone FROM into TO. => TO; a failure is a refusal naming git's words."
  (run-checked (append (list *git-program* "clone" "--quiet")
                       (and depth (list "--depth" (princ-to-string depth)))
                       (list (if (pathnamep from) (uiop:native-namestring from) from)
                             (string-right-trim "/\\" (uiop:native-namestring to))))
               (lambda (err) (profile-fail "git clone of ~a failed: ~a" from err)))
  to)

(defun profile-create (name &key (clone :settings) from description &aux carried shared)
  "A new profile NAME under *PROFILES-DIRECTORY*."
  ;; CLONE says what it starts with, and :SETTINGS is the default because a
  ;; second profile almost always wants the first one's abilities:
  ;; config.jsonc (without its channels — DROP-CLONED-CHANNELS), SOUL.md,
  ;; themes and the installed peripherals (shipped and hub cells alike, so
  ;; the copied config's providers come along), plus the rows +HOME-ENTRIES+ marks
  ;; SHARED — secrets/ and auth.json — and the knowledge cells, which are LINKED
  ;; rather than copied: they belong to the operator and to the project, not
  ;; to a home, so the new profile reads the one copy and the two homes'
  ;; writers take that folder's lock. A fresh store, a fresh layer. :ALL is a
  ;; fork of the organism: the operator's own cell folders and the layer
  ;; with its git history, and even the shared rows copied, because a fork
  ;; wants its own history — the channels are left behind in both, since a bot
  ;; token belongs to one home. :BLANK is an empty home, and the setup walk
  ;; runs at its first launch. Never the store. FROM
  ;; names the source profile (default: this process's own home).
  ;;
  ;; The home is built under *PROFILES-DIRECTORY* in a dot-named staging
  ;; directory and published with one rename, so nothing that walks the
  ;; profiles folder ever sees a half-made home (the shape Hermes uses, for
  ;; the same reason: a gateway rescans profiles as they appear). Writes
  ;; profile.jsonc — which names what was carried, shared and dropped — and,
  ;; on POSIX under the organism's binary, the wrapper command NAME. => the
  ;; new home. Refuses a reserved name, an existing profile, a source that
  ;; does not exist.
  (check-profile-name name)
  (when (member name +reserved-profile-names+ :test #'string=)
    (profile-fail "~a is reserved: the default home, its folder, or a launch verb" name))
  (unless (member clone '(:blank :settings :all))
    (profile-fail "clone is :blank, :settings or :all, not ~s" clone))
  ;; From here down NIL is the blank home, the way the copy loops read it.
  (when (eq clone :blank) (setf clone nil))
  (let ((home (profile-home name)))
    (when (or (uiop:directory-exists-p home)
              (probe-file (string-right-trim "/" (namestring home))))
      (profile-fail "a profile named ~a already exists at ~a" name (namestring home)))
    (let* ((source-record (and from (require-profile from)))
           (source (cond (source-record source-record.home)
                         (clone *home*)))
           ;; The home is BUILT here and published with one rename: a shell or
           ;; gateway walking profiles/ never sees a half-made home, and a
           ;; create that dies leaves only this staging tree, which the cleanup
           ;; takes. The leading dot keeps it out of the roster as well — the
           ;; scan takes a folder whose name is a profile name, and
           ;; `.creating-NAME-…' is not one — and `.locks/' beside it holds the
           ;; running homes' write locks (os.lisp FOLDER-LOCK-PATH).
           (staging (merge-pathnames
                     (format nil ".creating-~a-~36r/" name (random (expt 36 6) (make-random-state t)))
                     *profiles-directory*)))
      (ensure-directories-exist (merge-pathnames "cells/" staging))
      (with-cleanup ((ignore-errors (remove-tree staging)))
        (when clone
          ;; The settings, and the shared rows by reference: never a state
          ;; entry.
          ;; CELLS is walked folder by folder below rather than copied whole (a
          ;; shipped install, a symbolic link and the operator's own each move
          ;; differently). Whether a row names a file or a folder is the
          ;; filesystem's to answer at the copy, not a second list's to remember.
          (loop for (entry carries shares) in +home-entries+
                when (and carries (string/= "cells" entry))
                  do (let* ((folder (uiop:directory-exists-p (merge-pathnames entry source)))
                            (path (or folder (merge-pathnames entry source)))
                            (copy (merge-pathnames (if folder (format nil "~a/" entry) entry)
                                                   staging))
                            ;; A fork copies even these: it is an organism of its own.
                            ;; A file shares as readily as a folder: auth.json is one.
                            (by-reference (and (eq clone :settings) shares (probe-file path))))
                       (cond
                         ;; Linked: the new home's secrets/ IS this home's. Two homes, one
                         ;; folder, one writer at a time — the folder's lock (os.lisp
                         ;; WITH-FOLDER-LOCK) is what a writer there takes.
                         ((and by-reference (make-symlink path copy))
                          (push entry shared))
                         ;; Copied: everything else, a `:all' fork of the shared rows, and
                         ;; Windows, where MAKE-SYMLINK answers NIL and the manifest then
                         ;; says the entry was carried rather than shared.
                         (folder
                          (copy-tree-preserving path copy)
                          (push entry carried))
                         ((probe-file path)
                          (uiop:copy-file path copy)
                          (push entry carried)))))
          ;; A knowledge cell is shared: linked, one folder for both homes,
          ;; written under its folder lock (scribe.lisp).
          ;; Whatever the installer put there is carried: a shipped link as a
          ;; link, and a copy with its marker -- a shipped copy, or a hub clone
          ;; (`nodecode add nc://claude-code') -- as a copy, marker and all
          ;; (INSTALLER-PUT-P). A hub clone left behind is a provider or a
          ;; tool the copied config.jsonc still names: a profile cloned from a
          ;; home on claude-code answered its first turn `provider claude-code
          ;; is not one this box knows'.
          (loop with cells = (merge-pathnames "cells/" source)
                for entry in (and (uiop:directory-exists-p cells) (uiop:subdirectories cells))
                for name = (car (last (pathname-directory entry)))
                for destination = (merge-pathnames (format nil "cells/~a/" name) staging)
                for link = (symlink-target entry)
                for how = (cond (link (make-symlink link destination) :carried)
                                ((installer-put-p name cells)
                                 (copy-tree-preserving entry destination) :carried)
                                ((and (eq clone :settings) (knowledge-folder-p entry))
                                 ;; Windows, where MAKE-SYMLINK answers NIL, copies
                                 (if (make-symlink (truename entry) destination)
                                     :shared
                                     (progn (copy-tree-preserving entry destination) :carried)))
                                ((not (eq clone :all)) nil)
                                ((uiop:directory-exists-p (merge-pathnames ".git/" entry))
                                 (git-clone entry destination) :carried)
                                (t (copy-tree-preserving entry destination) :carried))
                do (case how
                     (:shared (push (format nil "cells/~a" name) shared))
                     (:carried (push (format nil "cells/~a" name) carried)))))
        ;; A bot token belongs to one home, so the operator's config is copied
        ;; without its channels (DROP-CLONED-CHANNELS) and the manifest says
        ;; which members were left behind.
        (write-manifest staging
                        "name" name
                        "description" (or description "")
                        "created_at" (iso-now)
                        "source" (cond (source-record source-record.name) (clone (profile-name)))
                        "clone" (string-downcase (symbol-name (or clone :none)))
                        "carried" (coerce (reverse carried) 'vector)
                        "shared" (coerce (nreverse shared) 'vector)
                        "dropped" (remove nil (vector (drop-cloned-channels staging))))
        ;; One rename publishes it: every byte above was written where nothing
        ;; walks, and a reader finds the home whole or not at all.
        (replace-file staging home))
      (write-wrapper name)
      home)))

(defun profile-created-text (name home clone from)
  "What a create of NAME at HOME, cloned as CLONE from FROM (NIL: this
process's own home), says in every shell: where the home is, what it came
from and shares, the channels it left behind, and how to run it -- the
wrapper too when one was written, and whether its folder is on PATH."
  (let ((wrapped (wrapper-owned-p (wrapper-path name) name)))
    (format nil "created ~a at ~a~@[, ~a~]~@[; no ~{~a~^, ~} (a bot token belongs to one home)~]; `nodecode -p ~a' runs it~:[~;, and so does `~a'~]~@[ (~a is not on your PATH)~]"
            name (string-right-trim "/" (namestring home))
            (and (not (eq clone :blank))
                 (format nil "~:[the settings of ~a~;a fork of ~a~]~@[, sharing ~{~a~^, ~}~]"
                         (eq clone :all) (or from (profile-name))
                         (manifest-list home "shared")))
            (manifest-list home "dropped")
            name wrapped name
            (and wrapped
                 (not (let ((wanted (string-right-trim
                                     "/\\" (uiop:native-namestring *wrapper-directory*))))
                        (and (plusp (length wanted))
                             (some (lambda (entry)
                                     (string= (string-right-trim "/\\" entry) wanted))
                                   (uiop:split-string (or (uiop:getenv "PATH") "")
                                                      :separator #+win32 ";" #-win32 ":")))))
                 (string-right-trim "/" (namestring *wrapper-directory*))))))

;;; --- delete, use ---------------------------------------------------------------------

(defun deletable-profile (name &aux (record (require-profile name)))
  "NAME's record when it may be deleted, else a refusal saying why not:
the default profile, this process's own, one whose store has a live owner
(stop that organism first)."
  (when (string= name "default")
    (profile-fail "the default profile is ~a itself: not deleted, emptied by hand"
                  (namestring (default-home))))
  (when (profile-record-current-p record)
    (profile-fail "~a is this process's own profile; delete it from another" name))
  (when (profile-record-pid record)
    (profile-fail "~a is served by pid ~d~@[ on port ~d~]; ~
                     ~:[stop that organism first~;that is its gateway — ~
                     `nodecode -p ~a gateway off' stops it~]"
                  name (profile-record-pid record) (profile-record-port record)
                  (gateway-process-p (profile-record-pid record)) name))
  record)

(defun profile-delete (name &aux (record (deletable-profile name))
                                 (sticky (equal name (sticky-profile))))
  "Remove profile NAME: its whole home — store, layer, secrets —
its gateway's unit, its wrapper when ours, and the sticky file when it
named it. Refused as DELETABLE-PROFILE says. => T."
  ;; A unit left behind would restart a home that is gone, five seconds
  ;; after every exit, for good.
  (gateway-off :home (profile-record-home record) :mark nil)
  (remove-tree (profile-record-home record))
  (let ((wrapper (wrapper-path name)))
    (when (wrapper-owned-p wrapper name)
      (delete-file wrapper)))
  (when sticky (delete-file *active-profile-path*))
  t)

(defun profile-use (name)
  "Make NAME the profile a launch that names none boots into: the sticky
file names it, and is removed for `default'."
  ;; This process stays where it is. => NAME.
  (require-profile name)
  (if (string= name "default")
      (when (probe-file *active-profile-path*) (delete-file *active-profile-path*))
      (with-output-file (out *active-profile-path*) (write-line name out)))
  name)

;;; --- switch ------------------------------------------------------------------
;;; A home is a store, a gateway, a token and a layer of live definitions in
;;; the image: one process cannot hold two, so moving to another home is a
;;; new image, not a rebind. EXEC-PROGRAM makes that read as a switch rather
;;; than a restart — the pid, the terminal, the shell job and the window all
;;; survive and only the image changes — which is why this verb answers a
;;; plan and lets the shell perform it at its own teardown seam. Nothing here
;;; changes THIS process's home; the process is replaced.

(defun profile-switch (name &optional (argv sb-ext:*posix-argv*))
  "The plan that moves a shell to profile NAME: => (values BINARY ARGV),
ARGV being BINARY, `-p NAME', and every word this process was launched
with but the profile flag it already carried."
  ;; Pure — it reads the roster and ORGANISM-BINARY and touches nothing. The
  ;; caller performs it with EXEC-PROGRAM once its terminal is restored and
  ;; its store is closed.
  ;;
  ;; Refuses three ways, each a PROFILE-ERROR: a profile that is not there
  ;; (naming the nearest), the profile this process is already running, and a
  ;; developer image, which has no binary on disk to exec.
  (when (profile-record-current-p (require-profile name))
    (profile-fail "this shell is already running ~a" name))
  (let ((binary (organism-binary)))
    (unless binary
      (profile-fail "this image was not started from an installed binary, ~
                     so it cannot re-exec; run `nodecode -p ~a' in a new ~
                     shell, or `profile use ~a' to make it the default"
                    name name))
    ;; Any `-p NAME' or `--profile NAME' pair goes, wherever it sits -- the rule
    ;; PARSE-LAUNCH-ARGUMENTS reads them by.
    ;; What is left is every other word this process was started with, so a
    ;; switch keeps the session, the store path and the verb the operator
    ;; launched under.
    (values binary (list* binary "-p" name
                          (rest (loop while argv
                                      for word = (pop argv)
                                      if (member word '("-p" "--profile") :test #'string=)
                                        do (pop argv)
                                      else collect word))))))

;;; LAUNCH.LISP installs the one that execs immediately — a command line, a
;;; channel lane and the model's eval have no terminal state to give back. A
;;; TUI shell rebinds it for the length of its run to park the plan instead,
;;; and execs it once the program loop has restored the terminal and the store
;;; has closed: an exec keeps open file descriptors, so a store lease still
;;; held would be inherited into a deadlock against itself.
(defvar *profile-switch-performer* nil
  "How a switch plan is performed, so this file decides what a switch means
and never when: a function of (NAME BINARY ARGV) answering the text the
caller shows.")

;;; The environment is the one channel an exec keeps, and one word is enough:
;;; the successor consumes it (unsetting it) so nothing it spawns inherits the
;;; claim.
(defparameter +handover-variable+ "NODECODE_HANDOVER"
  "The environment word a switch's successor reads: this image took a
terminal another image was running — the screen is live, not blank.")

;;; A special and not a closure's local, for that reason alone: the parker and
;;; the unwinds that read it are three files apart. A shell either execs or
;;; exits once this is set, so nothing clears it.
(defvar *profile-switch-parked* nil
  "The plan a shell parked — (NAME BINARY ARGV) — while the images that must
behave differently on the way out can still see it: the terminal keeps its
screen for the successor, and a shell that owns a gateway skips the graceful
stop it would otherwise pay seconds for.")

;;; --- export, import ------------------------------------------------------------------

(defun staging-directory (tag)
  (merge-pathnames (format nil "nodecode/profile-~a/~d-~d/" tag (get-universal-time)
                           (random (expt 36 6) (make-random-state t)))
                   (cache-path "")))

(defun export-home (home path &key whole (store (home-store-path home)) (step (constantly nil)))
  "HOME as a gzipped tar at PATH: the rows of +HOME-ENTRIES+ an archive may
ship, or with WHOLE the home to come back to -- every row a clone carries,
the secrets and credentials among them, and the store at STORE as SQLite
copies it while its writer goes on. STEP is called with what it is doing as
it goes. => PATH as written, and the entries it carries."
  ;; One folder in the tar, so PROFILE-IMPORT brings either back as a home of
  ;; its own. What rides is the table's answer and nothing else's: an entry
  ;; no row claims stays behind (a team's worktrees, gigabytes of them, sat
  ;; in a home beside the store), and so do the default home's profiles/ and
  ;; sticky file, which no row ships.
  ;;
  ;; A row that is a symbolic link -- a shared row, a link into the home this
  ;; one came from (PROFILE-CREATE) -- rides by its content, because a path
  ;; into another home is nothing on the machine the archive reaches, and so
  ;; does a knowledge cell linked under cells/, the memories and skills a
  ;; sharing profile reads. Any other link inside a row stays a link: the
  ;; shipped cells under cells/ point into the source tree the target machine
  ;; has of its own.
  ;;
  ;; Passes, then: the snapshot and the rows as they are, then the linked
  ;; rows by content (`-h'), then gzip -- tar cannot follow one link and keep
  ;; another, nor add to what it has compressed. tar is GNU tar on Linux and
  ;; bsdtar on macOS and Windows; both read these flags and PROFILE-IMPORT's.
  ;; The tree is staged beside PATH, closed to everyone else (a whole one
  ;; holds the keys), so the archive lands whole by one rename.
  (let* ((home (uiop:ensure-directory-pathname home))
         (folder (car (last (pathname-directory home))))
         (parent (uiop:native-namestring (uiop:pathname-parent-directory-pathname home)))
         (path (merge-pathnames path (uiop:getcwd)))
         (entries (loop for (entry carried nil shipped) in +home-entries+
                        when (and (or shipped (and whole carried))
                                  (probe-file (merge-pathnames entry home)))
                          collect entry))
         (knowledge (loop for directory in (and (member "cells" entries :test #'string=)
                                                (uiop:subdirectories (merge-pathnames "cells/" home)))
                          when (and (symlink-target directory) (knowledge-folder-p directory))
                            collect (format nil "cells/~a" (folder-name directory))))
         (linked (append (remove-if-not (lambda (entry) (symlink-target (merge-pathnames entry home)))
                                        entries)
                         knowledge))
         (plain (remove-if (lambda (entry) (member entry linked :test #'string=)) entries))
         (staging (uiop:merge-pathnames* (format nil ".~a.making/" (file-namestring path))
                                         (uiop:pathname-directory-pathname path)))
         (snapshot (merge-pathnames (format nil "~a/history.db" folder) staging))
         (raw (uiop:native-namestring (merge-pathnames "home.tar" staging))))
    (flet ((under (names) (mapcar (lambda (entry) (format nil "~a/~a" folder entry)) names)))
      (ensure-directories-exist snapshot)
      #-win32 (sb-posix:chmod (uiop:native-namestring staging) #o700)
      (with-cleanup ((ignore-errors (remove-tree staging)))
        (when (and whole (probe-file store))
          (funcall step "copying the store")
          (snapshot-store store snapshot))
        (let* ((stored (probe-file snapshot))
               (passes (remove nil (list (append (loop for cell in knowledge
                                                       collect (format nil "--exclude=~a/~a" folder cell))
                                                 (and stored (list "-C" (uiop:native-namestring staging)
                                                                   (format nil "~a/history.db" folder)))
                                                 (and plain (list* "-C" parent (under plain))))
                                         (and linked (list* "-h" "-C" parent (under linked)))))))
          (unless passes
            (profile-fail "~a holds nothing an archive carries" (namestring home)))
          (funcall step "packing")
          (loop for pass in passes
                for verb = "-cf" then "-rf"
                do (run-checked (list* "tar" verb raw pass) (lambda (err) (profile-fail "tar failed: ~a" err))))
          (funcall step "compressing")
          (run-checked (list "gzip" "-f" raw) (lambda (err) (profile-fail "gzip failed: ~a" err)))
          (let ((packed (format nil "~a.gz" raw)))
            #-win32 (when whole (sb-posix:chmod packed #o600))
            (replace-file packed path))
          (values path (if stored (cons "history.db" entries) entries)))))))

(defun profile-export (name path &key whole)
  "NAME's home as a gzipped tar at PATH (EXPORT-HOME): what it may hand on,
without its store, secrets and credentials, or with WHOLE all of them.
=> PATH as written."
  (values (export-home (profile-record-home (require-profile name)) path :whole whole)))

(defun adopt-staged-home (folder name &rest stamp)
  "The staged home FOLDER becomes profile NAME: copied whole under
*PROFILES-DIRECTORY*, its manifest stamped with STAMP pairs, its wrapper
written. => NAME."
  (check-profile-name name)
  (when (member name +reserved-profile-names+ :test #'string=)
    (profile-fail "~a is reserved; --name NAME picks another" name))
  (when (uiop:directory-exists-p (profile-home name))
    (profile-fail "a profile named ~a already exists; --name NAME picks another" name))
  (let ((home (profile-home name)))
    (copy-tree-preserving folder home)
    (ensure-directories-exist home)
    (apply #'write-manifest home "name" name stamp)
    (write-wrapper name)
    name))

(defun profile-import (path &key name)
  "A profile from the archive PROFILE-EXPORT wrote at PATH, as NAME
(default: the archive's manifest name, else its folder's)."
  ;; => the name.
  ;; Refuses an existing name and a reserved one.
  (let ((path (merge-pathnames path (uiop:getcwd))))
    (unless (probe-file path) (profile-fail "no archive at ~a" (namestring path)))
    (let ((staging (staging-directory "import")))
      (ensure-directories-exist staging)
      (with-cleanup ((ignore-errors (remove-tree staging)))
        (run-checked (list "tar" "-xzf" (uiop:native-namestring path)
                           "-C" (uiop:native-namestring staging))
                     (lambda (err) (profile-fail "tar failed: ~a" err)))
        (let* ((folders (uiop:subdirectories staging))
               (folder (if (= (length folders) 1)
                           (first folders)
                           (profile-fail "the archive holds ~d top-level folders, not one"
                                         (length folders))))
               (name (or name
                         (json-value (read-manifest folder) :string "name")
                         (string-left-trim "." (car (last (pathname-directory folder)))))))
          (adopt-staged-home folder name
                             "imported_at" (iso-now)
                             "imported_from" (namestring path)))))))

;;; --- distributions: a profile from a git remote ----------------------------------------

(defun profile-install (url &key name &aux (staging (staging-directory "install")))
  "A profile from the git repository at URL — a distribution: a home laid
out like any other, with a profile.jsonc naming it — cloned shallow, as
NAME (default: the manifest's name, else the repository's)."
  ;; A repository carrying a symbolic link is refused. The remote and the
  ;; install time are stamped beside the store, never into the manifest, so an
  ;; update finds it clean; what the organism keeps for itself is excluded
  ;; from git. => the name.
  (ensure-directories-exist staging)
  (with-cleanup ((ignore-errors (remove-tree staging)))
    (let ((repository (merge-pathnames "repository/" staging)))
      (git-clone url repository :depth 1)
      (when-let (links (repository-links repository))
        (profile-fail "~a carries symbolic link~p (~{~a~^, ~}); a distribution may not"
                      url (length links) links))
      (let ((name (or name
                      (json-value (read-manifest repository) :string "name")
                      (let ((last (car (last (ppcre:split "/" (string-right-trim "/" url))))))
                        (ppcre:regex-replace "\\.git$" last "")))))
        (adopt-staged-home repository name)
        (let ((home (profile-home name)))
          ;; Out of the repository: the private entries, the use ledger among
          ;; them, then what the organism keeps for itself -- the cell
          ;; folders it installs and writes, its knowledge cells among them.
          ;; Those are carried by a clone, and still must not be contended
          ;; for by a pull.
          (when (uiop:directory-exists-p (merge-pathnames ".git/" home))
            (with-output-file (out (merge-pathnames ".git/info/exclude" home))
              (format out "# nodecode: what a profile keeps for itself~%")
              (loop for (entry nil nil shipped whose) in +home-entries+
                    unless (or shipped (eq :default whose)) do (format out "/~a~%" entry))
              (format out "/cells~%")))
          (with-output-file (out (merge-pathnames "distribution.json" home))
            (write-line (encode-json-object (json-object "source" url "installed_at" (iso-now)))
                        out)))
        name))))

(defun profile-update (name)
  "Pull NAME's distribution: `git pull --ff-only --autostash' in its home,
so its owned paths move and an operator's edits ride over the top; what
the organism keeps — its cells, the store, secrets — is excluded from the
repository and untouched."
  ;; => git's last line. Refuses a profile that was
  ;; not installed from a distribution.
  (let* ((home (profile-record-home (require-profile name)))
         (stamp (json-object-file (merge-pathnames "distribution.json" home))))
    (unless (and stamp (uiop:directory-exists-p (merge-pathnames ".git/" home)))
      (profile-fail "~a was not installed from a distribution; nothing to pull" name))
    (let ((out (run-checked (list *git-program* "-C" (uiop:native-namestring home)
                                  "pull" "--ff-only" "--autostash" "--quiet")
                            (lambda (err)
                              (profile-fail "git pull in ~a failed: ~a" name err)))))
      (or (car (last (remove "" (lines out) :test #'string=))) "up to date"))))

;;; --- the shells' text --------------------------------------------------------------------

(defparameter +unclaimed-entries-named+ 6
  "How many unclaimed entries a roster line names before it counts the
rest: enough to act on, few enough that the roster stays a roster.")

(defun home-entry-names (home)
  "The top-level names HOME holds, sorted, a folder's name with a trailing
slash so it reads as one; none for a home it cannot read."
  (sort (append (mapcar (lambda (path) (format nil "~a/" (folder-name path)))
                        (ignore-errors (uiop:subdirectories home)))
                (mapcar #'file-namestring (ignore-errors (uiop:directory-files home))))
        #'string<))

(defun unclaimed-entries (home)
  "The top-level names HOME holds that no +HOME-ENTRIES+ row claims,
sorted, a folder's name with a trailing slash so it reads as one."
  ;; What a home has accumulated that the product which made it does not
  ;; recognise: dead folders from an older layout, a cell's droppings, a
  ;; backup an operator left. Read-only, and tolerant — a home it cannot read
  ;; answers nothing rather than refusing.
  (flet ((claimed-p (entry-name &aux (name (string-right-trim "/" entry-name)))
           ;; A row spelled with a trailing * claims everything beside it:
           ;; one history.db* row is the store and every sidecar it opens.
           (loop for (entry) in +home-entries+
                 for stem = (string-right-trim "*/" entry)
                 thereis (if (uiop:string-suffix-p entry "*")
                             (uiop:string-prefix-p stem name)
                             (string= stem name)))))
    (remove-if #'claimed-p (home-entry-names home))))

(defun unclaimed-entries-text (home)
  "One line's worth of what HOME carries that no row claims, or NIL when
it carries nothing unclaimed."
  ;; Names +UNCLAIMED-ENTRIES-NAMED+ of them and counts the rest.
  (when-let (entries (unclaimed-entries home))
    (let ((named (subseq entries 0 (min (length entries) +unclaimed-entries-named+)))
          (rest (max 0 (- (length entries) +unclaimed-entries-named+))))
      (format nil "~{~a~^, ~}~[~:;, and ~:*~d more~]" named rest))))

(defun profile-report (&optional (records (profiles)))
  "The roster as text, one line per profile: a star for this process's
own, the name, the home, a live gateway's pid and port, `boots by default'
for the one a bare launch takes, the description."
  ;; Under a profile whose home carries entries no +HOME-ENTRIES+ row claims,
  ;; one more line naming them — the roster is the only place a home can say
  ;; what it has accumulated. It says it; nothing here moves, hides or removes
  ;; anything, and no verb was added that would.
  (with-output-to-string (out)
    (dolist (record records)
      (format out "~:[ ~;*~] ~a  ~a~@[  serving pid ~d~]~@[ port ~d~]~:[~;  boots by default~]~@[  — ~a~]~%"
              (profile-record-current-p record)
              (profile-record-name record)
              (string-right-trim "/" (namestring (profile-record-home record)))
              (profile-record-pid record)
              (profile-record-port record)
              (profile-record-sticky-p record)
              (let ((description (profile-record-description record)))
                (and description (plusp (length description)) description)))
      (when-let (shared (manifest-list (profile-record-home record) "shared"))
        (format out "      shares ~{~a~^, ~}~@[ with ~a~]~%"
                shared (profile-record-source record)))
      (when-let (unclaimed (unclaimed-entries-text (profile-record-home record)))
        (format out "      also carrying ~a~%" unclaimed)))))

(defun profile-command (args &key (command "profile"))
  "One profile request as text: the shells' one entry, `nodecode profile
...', /profile, a channel's /profile, the model's eval."
  ;; ARGS is the words
  ;; after the command, one string or a list; COMMAND is how the calling shell
  ;; spells the command ("nodecode profile", "/profile"), which help and a
  ;; refusal quote back. The verbs are *PROFILE-VERBS*; with none, the roster.
  ;; `help [VERB]', or --help or -h anywhere, answers help instead of acting.
  ;; => the answer. A refusal is PROFILE-ERROR: PROFILE-USAGE-ERROR for words
  ;; the verbs cannot read, pointing at the help; PROFILE-NEEDS-YES for a
  ;; delete nobody said yes to, which a shell with a terminal asks instead.
  (with-verbs (args *profile-verbs* :refusal profile-usage-error
               :noun "profile verb" :default "list" :command command
               :usage "usage: ~a [VERB] [ARGUMENTS]~%~%A profile is a home of its own — its config, secrets, store, layer and gateway — and a new one reads your memories, skills and credentials where they are rather than copying them; `--clone-all' forks those too. `nodecode -p NAME' runs one; with no verb, ~a lists them.~%~%verbs~%~a")
    (labels ((flag (name)
               (when (member name words :test #'string=)
                 (setf words (remove name words :test #'string= :count 1))
                 t))
             (option (name)
               (when-let (tail (member name words :test #'string=))
                 (let ((value (second tail)))
                   (unless value (usage-fail verb "~a needs a value." name))
                   (setf words (append (ldiff words tail) (cddr tail)))
                   value)))
             (positional (what)
               (or (pop words) (usage-fail verb "~a needs a ~a." verb what)))
             (last-positional (what) (prog1 (positional what) (nothing-more))))
      (dispatch verb string=
        ("list" (nothing-more)
                (profile-report))
        ("use" (let ((name (last-positional "NAME")))
                 (profile-use name)
                 (format nil "~a boots by default from the next launch on; this process stays on ~a"
                         name (profile-name))))
        ("switch" (let ((name (last-positional "NAME")))
                    (multiple-value-bind (binary argv) (profile-switch name)
                      (unless *profile-switch-performer*
                        (profile-fail "nothing in this image performs a switch; ~
                              `nodecode -p ~a' runs ~:*~a in a new shell"
                                      name))
                      (funcall *profile-switch-performer* name binary argv))))
        ("create" (let* ((all (flag "--clone-all"))
                         (blank (flag "--blank"))
                         (from (option "--from"))
                         (description (option "--description"))
                         (name (last-positional "NAME")))
                    (when (and all blank)
                      (usage-fail verb "--blank and --clone-all ask for opposite homes."))
                    (let ((clone (cond (all :all) (blank :blank) (t :settings))))
                      (profile-created-text
                       name (profile-create name :clone clone :from from :description description)
                       clone from))))
        ("delete" (let* ((yes (flag "--yes"))
                         (name (last-positional "NAME")))
                    ;; Refused before anything is asked: a question about a
                    ;; profile that cannot be deleted would be answered for nothing.
                    (deletable-profile name)
                    (unless yes
                      (error 'profile-needs-yes
                             :detail (format nil "delete removes ~a's whole home: its store, layer and secrets. \"~a delete ~a --yes\" means it."
                                             name command name)
                             :question (format nil "Delete ~a's whole home, with its store, layer and secrets? [y/N]"
                                               name)))
                    (profile-delete name)
                    (format nil "deleted ~a" name)))
        ("export" (let* ((whole (flag "--whole"))
                         (name (positional "NAME"))
                         (path (last-positional "PATH")))
                    (format nil "exported ~a to ~a~:[~;, its store and keys with it: keep it where you keep passwords~]"
                            name (profile-export name path :whole whole) whole)))
        ("import" (let* ((name (option "--name"))
                         (path (last-positional "PATH")))
                    (let ((name (profile-import path :name name)))
                      (format nil "imported ~a from ~a; `nodecode -p ~a' runs it" name path name))))
        ("install" (let* ((name (option "--name"))
                          (url (last-positional "URL")))
                     (let ((name (profile-install url :name name)))
                       (format nil "installed ~a from ~a; `nodecode -p ~a' runs it, `profile update ~a' pulls it"
                               name url name name))))
        ("update" (let ((name (last-positional "NAME")))
                    (format nil "~a: ~a" name (profile-update name))))
        (t (error "the profile verb ~a has no branch in PROFILE-COMMAND" verb))))))
