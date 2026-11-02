;;;; profile-test.lisp --- profiles: several homes, one binary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hermetic posture points the profiles folder, the sticky file and
;;;; the wrapper folder under a scratch home of the test's own, so every
;;;; verb here creates and removes homes nowhere near ~/.nodecode/. The
;;;; resolution ladder (which home a launch boots) is organism/cells-test's;
;;;; these gates are the lifecycle: create, clone, use, delete, export and
;;;; import, install and update, and the one text the shells speak.

(in-package #:nodecode.test)

(nlk:access (verb nlk::verb))

(defun profile-test-structure-call (calls)
  "The tar pass that carries a home's rows as they are among CALLS, newest
first: the one that creates the archive."
  (find-if (lambda (argv) (and (equal "tar" (first argv)) (equal "-cf" (second argv)))) calls))

(defun profile-test-members (argv folder)
  "The members ARGV names under FOLDER, in order."
  (remove-if-not (lambda (word) (uiop:string-prefix-p (format nil "~a/" folder) word)) argv))

(defun profile-test-staging-trees ()
  "What the profiles folder holds that is not a profile: a half-made home a
create left behind, which the dot-named staging directory and the cleanup
exist to prevent."
  (remove-if-not (lambda (name) (uiop:string-prefix-p "." name))
                 (mapcar #'nlk::folder-name (uiop:subdirectories nlk:*profiles-directory*))))

(deftest profile-create-blank-then-list ()
  (let ((home (nlk:profile-create "coder" :clone :blank :description "for code")))
    (is (uiop:directory-exists-p home) "the home exists")
    (is (search "/profiles/coder/" (namestring home)) "under the profiles folder")
    (let ((record (nlk:find-profile "coder")))
      (is record "the profile is found")
      (is (equal "for code" (nlk:profile-record-description record)))
      (is (not (nlk:profile-record-current-p record)) "it is not this process's home")
      (is (not (nlk:profile-record-sticky-p record)) "no sticky file names it")
      (is (null (nlk:profile-record-pid record)) "nothing serves it"))
    (let ((names (mapcar #'nlk:profile-record-name (nlk:profiles))))
      (is (equal "default" (first names)) "the default comes first")
      (is (member "coder" names :test #'string=) "the new one is listed"))
    (is (nlk:profile-record-sticky-p (nlk:find-profile "default")))
    (is (signals-error nlk:profile-error (nlk:profile-create "coder")) "an existing name refuses")
    (is (signals-error nlk:profile-error (nlk:profile-create "default")) "the default name refuses")
    (is (signals-error nlk:profile-error (nlk:profile-create "serve")) "a launch verb refuses")
    (is (signals-error nlk:profile-error (nlk:profile-create "Coder")) "a bad name refuses")
    (is (null (nlk:find-profile "writer")) "an unknown profile is NIL")))

(deftest profile-use-moves-the-sticky-file ()
  (nlk:profile-create "coder")
  (is (equal "coder" (nlk:profile-use "coder")))
  (is (equal "coder" (nlk:sticky-profile)) "the sticky file names it")
  (is (nlk:profile-record-sticky-p (nlk:find-profile "coder")) "and the record says so")
  (is (not (nlk:profile-record-sticky-p (nlk:find-profile "default"))))
  (nlk:profile-use "default")
  (is (null (probe-file nlk:*active-profile-path*)) "default removes the file")
  (is (signals-error nlk:profile-error (nlk:profile-use "writer")) "an unknown profile refuses"))

(deftest profile-delete-removes-the-home ()
  (nlk:profile-create "coder")
  (nlk:profile-use "coder")
  (is (signals-error nlk:profile-error (nlk:profile-delete "default")) "the default is never deleted")
  (is (nlk:profile-delete "coder"))
  (is (null (uiop:directory-exists-p (nlk:profile-home "coder"))) "the folder is gone")
  (is (null (probe-file nlk:*active-profile-path*)) "the sticky file that named it is gone")
  (is (null (nlk:find-profile "coder")) "and so is the profile")
  (is (signals-error nlk:profile-error (nlk:profile-delete "coder")) "deleting twice refuses"))

(deftest profile-clone-carries-settings-then-everything ()
  (with-temp-workspace (target "guard/nodecode-guard.asd")
    (profile-test-seed-every-entry nlk:*home*)
    (let ((shipped (string-right-trim "/" (namestring (merge-pathnames "guard/" target)))))
      (nlk:make-symlink shipped (merge-pathnames "cells/guard/" nlk:*home*))
      ;; A layer: the organism's own git repository under cells/.
      (let ((layer (merge-pathnames "cells/tester/" nlk:*home*)))
        (write-temp-file (merge-pathnames "tester.asd" layer) "(defsystem \"tester\")")
        (is (nlk:git-commit-all layer "one")))
      (let ((home (nlk:profile-create "coder" :clone :settings)))
        (is (equal shipped (nlk:symlink-target (merge-pathnames "cells/guard/" home))))
        (is (null (uiop:directory-exists-p (merge-pathnames "cells/own/" home))))
        (is (null (uiop:directory-exists-p (merge-pathnames "cells/tester/" home))))
        (is (equal "settings" (gethash "clone" (nlk::read-manifest home)))))
      (let ((home (nlk:profile-create "fork" :clone :all)))
        (is (uiop:directory-exists-p (merge-pathnames "cells/own/" home)))
        (is (uiop:directory-exists-p (merge-pathnames "cells/tester/.git/" home)))
        (is (probe-file (merge-pathnames "cells/tester/tester.asd" home)) "with its files")
        (is (null (probe-file (merge-pathnames "history.db" home))) "still never the store"))
      (let ((home (nlk:profile-create "twin" :from "coder")))
        (is (probe-file (merge-pathnames "SOUL.md" home)) "--from NAME clones that profile's settings")
        (is (equal "coder" (nlk:profile-record-source (nlk:find-profile "twin")))))
      (is (signals-error nlk:profile-error (nlk:profile-create "other" :from "nobody"))))))

(deftest a-clone-carries-the-cells-the-hub-installed ()
  ;; Peas, 2026-11-01: `nodecode profile create insight' cloned a home whose
  ;; config.jsonc runs on claude-code, and left behind nodecode-claude-code,
  ;; a hub clone (`nodecode add nc://claude-code') carrying a .hub marker and
  ;; no .shipped one, so the new profile's first turn failed `provider
  ;; claude-code is not one this box knows'. A hub clone is the installer's
  ;; as much as a shipped copy is (INSTALLER-PUT-P): it comes along, marker
  ;; and all, so an update still knows its pin. The operator's own folder
  ;; still stays home, as before.
  (profile-test-seed-every-entry nlk:*home*)
  (let ((hub (merge-pathnames "cells/nodecode-claude-code/" nlk:*home*))
        (pin (format nil "https://github.com/nodecode-ai/cells~%38148e7~%providers/nodecode-claude-code~%")))
    (write-temp-file (merge-pathnames "nodecode-claude-code.asd" hub) "(defsystem \"nodecode-claude-code\")")
    (write-temp-file (merge-pathnames nlk::+hub-marker+ hub) pin)
    (let* ((home (nlk:profile-create "insight"))
           (copy (merge-pathnames "cells/nodecode-claude-code/" home)))
      (is (probe-file (merge-pathnames "nodecode-claude-code.asd" copy)) "the hub clone is carried")
      (is (null (nlk:symlink-target copy)) "as a copy of its own, not a link into the source home")
      ;; with its pin, so an update still knows where it came from
      (is (equal pin (uiop:read-file-string (merge-pathnames nlk::+hub-marker+ copy))))
      (is (equal '("https://github.com/nodecode-ai/cells" "38148e7" "providers/nodecode-claude-code")
                 (nlk::hub-pin "nodecode-claude-code" (merge-pathnames "cells/" home))))
      (is (find "cells/nodecode-claude-code" (gethash "carried" (nlk::read-manifest home))
                :test #'equal))
      ;; the operator's own folder still stays home
      (is (null (uiop:directory-exists-p (merge-pathnames "cells/own/" home)))))))

(deftest a-settings-clone-carries-the-folder-a-pinned-provider-needs ()
  ;; The operator's own folder stays home, but for one serving a provider
  ;; the copied config pins: the clone's config names that provider too, and
  ;; without the folder its first turn is refused. Every pin counts, and a
  ;; folder serving a provider nothing pins still stays.
  (write-temp-file (merge-pathnames "cells/my-lane/my-lane.asd" nlk:*home*)
                   "(defsystem \"my-lane\" :properties ((:serves (:provider \"my-lane\"))))")
  (write-temp-file (merge-pathnames "cells/judge/judge.asd" nlk:*home*)
                   "(defsystem \"judge\" :properties ((:serves (:provider \"judge\"))))")
  (write-temp-file (merge-pathnames "cells/spare/spare.asd" nlk:*home*)
                   "(defsystem \"spare\" :properties ((:serves (:provider \"spare\"))))")
  (write-temp-file (merge-pathnames "config.jsonc" nlk:*home*)
                   "{ // pins
  \"general\": { \"default_model\": { \"provider\": \"my-lane\", \"model\": \"m\" },
               \"decider_model\": { \"provider\": \"judge\", \"model\": \"d\" } } }")
  (is (equal '("my-lane" "judge")
             (nlk::config-pinned-providers (merge-pathnames "config.jsonc" nlk:*home*))))
  (let* ((home (nlk:profile-create "coder"))
         (cells (merge-pathnames "cells/" home))
         (rode (sort (remove-if-not (lambda (entry) (uiop:string-prefix-p "cells/" entry))
                                    (nlk::manifest-list home "shared"))
                     #'string<)))
    (is (probe-file (merge-pathnames "my-lane/my-lane.asd" cells)) "the default model's folder rides")
    (is (nlk:symlink-target (merge-pathnames "my-lane" cells)) "linked: one folder for both homes")
    (is (probe-file (merge-pathnames "judge/judge.asd" cells)) "and the decider's")
    (is (null (uiop:directory-exists-p (merge-pathnames "spare/" cells))) "a provider nothing pins stays")
    (is (equal '("cells/judge" "cells/my-lane") rode) "and the manifest names them shared")
    (is (nlk:profile-delete "coder"))
    (is (probe-file (merge-pathnames "cells/my-lane/my-lane.asd" nlk:*home*)) "deleting the clone keeps the operator's folder"))
  (let ((home (nlk:profile-create "empty" :clone :blank)))
    (is (null (uiop:directory-exists-p (merge-pathnames "cells/my-lane/" home))) "a blank home takes none")))

(deftest profile-wrapper-is-written-and-removed-with-the-profile (with-temp-trees ((bin "bin")))
  (setf nlk:*wrapper-directory* bin)
  (with-stubbed-fdefinition (nlk::organism-binary () "/opt/nodecode/nodecode")
    (write-temp-file (merge-pathnames "mine" bin) (format nil "#!/bin/sh~%echo mine~%"))
    (nlk:profile-create "coder")
    (nlk:profile-create "mine")
    (let ((wrapper (merge-pathnames "coder" bin)))
      (is (probe-file wrapper) "the wrapper exists")
      (is-carrying (text (uiop:read-file-string wrapper))
        ("#!/bin/sh" "a shell script")
        ("nodecode profile wrapper: coder" "carrying the marker")
        ("exec \"/opt/nodecode/nodecode\" -p coder \"$@\"" "that execs the binary with -p")))
    (is (equal (format nil "#!/bin/sh~%echo mine~%")
               (uiop:read-file-string (merge-pathnames "mine" bin))))
    (nlk:profile-delete "coder")
    (nlk:profile-delete "mine")
    (is (null (probe-file (merge-pathnames "coder" bin))))
    (is (probe-file (merge-pathnames "mine" bin)))))

(deftest profile-command-is-the-shells-text ()
  (let ((answer (nlk:profile-command "create coder --description for-code")))
    (is (search "created coder at" answer) "create answers where")
    (is (search "nodecode -p coder" answer) "and how to run it"))
  (is (search "coder" (nlk:profile-command "list")) "list names it")
  (is (search "coder" (nlk:profile-command "")) "no verb lists")
  (is (search "boots by default" (nlk:profile-command "use coder")) "use says what changed")
  (is (search "for-code" (nlk:profile-command (list "list"))) "a word list works as the string does")
  (is (signals-error nlk:profile-error (nlk:profile-command "delete coder")) "delete needs --yes")
  (is (equal "deleted coder" (nlk:profile-command "delete coder --yes")))
  (is (signals-error nlk:profile-error (nlk:profile-command "frobnicate")) "an unknown verb refuses"))

(deftest profile-export-and-import-round-trip ()
  (profile-test-seed-every-entry (nlk:profile-create "coder"))
  (with-temp-file (archive :type "tar.gz")
    (let ((written (nlk:profile-export "coder" archive)))
      (is (probe-file written) "the archive exists")
      (is (signals-error nlk:profile-error (nlk:profile-import written)))
      (is (equal "restored" (nlk:profile-import written :name "restored")))
      (let ((home (nlk:profile-home "restored")))
        (is (probe-file (merge-pathnames "SOUL.md" home)) "settings ride")
        (is (probe-file (merge-pathnames "cells/own/own.asd" home)) "the cells ride")
        (is (null (probe-file (merge-pathnames "history.db" home))) "the store never leaves")
        (is (null (probe-file (merge-pathnames "link/state.json" home))) "nor this machine's link")
        ;; A row spelled as a folder, `mcp/', went through tar's exclude
        ;; unmatched and rode along.
        (is (null (probe-file (merge-pathnames "mcp/log.txt" home))) "nor a state folder")
        (is (null (probe-file (merge-pathnames "auth.json" home))) "nor the credentials")
        (is (equal "restored" (gethash "name" (nlk::read-manifest home)))))))
  (with-temp-file (whole :type "tar.gz")
    (is (search "keep it where you keep passwords"
                (nlk:profile-command (list "export" "coder" (namestring whole) "--whole"))) "--whole says what it carries")
    (is (equal "kept" (nlk:profile-import whole :name "kept")))
    (is (probe-file (merge-pathnames "auth.json" (nlk:profile-home "kept"))) "a whole one carries the keys")
    (is (probe-file (merge-pathnames "secrets/token" (nlk:profile-home "kept"))) "and the secrets")))

(deftest profile-install-and-update-from-a-distribution (with-temp-workspace (remote "SOUL.md"))
  (write-temp-file (merge-pathnames "profile.jsonc" remote)
                   "{\"name\": \"dist\", \"description\": \"a distribution\"}")
  (is (nlk:git-commit-all remote "one"))
  (is (equal "dist" (nlk:profile-install (namestring remote))) "named by its manifest")
  (let ((home (nlk:profile-home "dist")))
    (is (probe-file (merge-pathnames "SOUL.md" home)) "the files are there")
    (is (uiop:directory-exists-p (merge-pathnames ".git/" home)) "as a clone")
    (is (probe-file (merge-pathnames ".git/info/exclude" home)) "with the private paths excluded")
    (is (equal (namestring remote) (nlk:profile-record-source (nlk:find-profile "dist"))))
    (is (equal "a distribution" (nlk:profile-record-description (nlk:find-profile "dist"))))
    (is (stringp (nlk:profile-update "dist")) "update pulls")
    (is (signals-error nlk:profile-error (nlk:profile-command "install"))))
  (is (signals-error nlk:profile-error (nlk:profile-update "default")))
  (is (signals-error nlk:profile-error (nlk:profile-install (namestring remote)))))

(deftest profile-help-and-refusals-come-from-the-verbs ()
  (let ((help (nlk:profile-command "help")))
    (is (uiop:string-prefix-p "usage: profile [VERB] [ARGUMENTS]" help))
    (dolist (verb nlk:*profile-verbs*)
      (is (search verb.summary help) (format nil "~a is listed" verb.name))))
  (is (uiop:string-prefix-p "usage: /profile [VERB]" (nlk:profile-command "--help" :command "/profile")))
  (is (equal (nlk:profile-command "help create") (nlk:profile-command "create --help")))
  (is (uiop:string-prefix-p "usage: profile create NAME [--blank | --clone-all]"
                            (nlk:profile-command "help create")))
  (is (uiop:string-prefix-p "usage: profile delete NAME" (nlk:profile-command "delete -h")))
  (let ((refusal (refusal-text nlk:profile-usage-error (nlk:profile-command "lsit" :command "/profile"))))
    (is (search "no profile verb \"lsit\". Did you mean \"list\"?" refusal))
    (is (search "Run \"/profile help\" to see the verbs." refusal)))
  (is (search "Run \"profile help create\" to see its usage."
              (refusal-text nlk:profile-usage-error (nlk:profile-command "create"))))
  (is (signals-error nlk:profile-usage-error (nlk:profile-command "list extra")) "a word left over")
  (is (signals-error nlk:profile-usage-error (nlk:profile-command "help nothing"))))

(deftest profile-delete-waits-on-a-yes ()
  (nlk:profile-create "coder")
  (let ((pending (signals-error nlk:profile-needs-yes
                   (nlk:profile-command "delete coder" :command "nodecode profile"))))
    (is (search "[y/N]" (nlk:profile-needs-yes-question pending)) "a question a terminal can ask")
    (is (search "\"nodecode profile delete coder --yes\" means it" (princ-to-string pending))))
  (is (nlk:find-profile "coder") "nothing deleted")
  (let ((refusal (signals-error nlk:profile-error (nlk:profile-command "delete ghost"))))
    (is (search "no profile \"ghost\"" (princ-to-string refusal)))
    (is (not (typep refusal 'nlk:profile-needs-yes))))
  (is (search "Did you mean \"coder\"?" (refusal-text nlk:profile-error (nlk:profile-command "delete codr"))))
  (is (equal "deleted coder" (nlk:profile-command "delete coder --yes"))))

;;; --- what a home holds: the parity pins ------------------------------------
;;; Three answers about a home's entries live in three separate literals
;;; today (+PRIVATE-ENTRIES+, PROFILE-CREATE's inline clone list,
;;; PROFILE-EXPORT's excludes) and a fourth inside PROFILE-INSTALL. These
;;; gates state each answer as a literal expected value, so a refactor that
;;; promotes them to one table has to reproduce today's bytes rather than
;;; recompute them from the code under test.

(defun profile-test-seed-every-entry (home)
  "A home holding one of everything a clone might be asked to carry and
one of everything it must refuse: settings, secrets, state, the store."
  (dolist (file '("config.jsonc" "auth.json" "SOUL.md" "themes.json"
                  "history.db" "history.db-wal" "composer-history"
                  "theme-selection" "keybinds.json" "operator-token"
                  "distribution.json" "secrets/token" "usage.jsonl"
                  "link/state.json" "mcp/log.txt" "hermes/marker"
                  "gateway-off/flag" "cells/own/own.asd"))
    (write-temp-file (merge-pathnames file home) "x"))
  home)

(deftest pin-what-a-clone-carries ()
  ;; The literal set a clone carries at HEAD — copied, or linked where the
  ;; table says SHARED. cells/ is always made, with or without a clone, so
  ;; it stands in both columns below.
  (profile-test-seed-every-entry nlk:*home*)
  ;; The operator's rules for every session: a profile has its own copy.
  (write-temp-file (merge-pathnames "AGENTS.md" nlk:*home*) "x")
  (let ((cloned (nlk:profile-create "cloned")))
    (is (equal '("cells/" "profile.jsonc")
               (nlk::home-entry-names (nlk:profile-create "blank" :clone :blank))))
    (is (equal '("AGENTS.md" "SOUL.md" "auth.json" "cells/" "config.jsonc"
                 "profile.jsonc" "secrets/" "themes.json")
               (nlk::home-entry-names cloned)))
    (dolist (shared '("secrets" "auth.json"))
      (is (nlk:symlink-target (merge-pathnames shared cloned))
          (format nil "~a is shared by reference, not copied" shared)))
    (dolist (copied '("config.jsonc" "SOUL.md" "AGENTS.md" "themes.json"))
      (is (null (nlk:symlink-target (merge-pathnames copied cloned)))
          (format nil "~a is a copy of its own" copied)))
    (dolist (never '("history.db" "history.db-wal" "composer-history"
                     "theme-selection" "keybinds.json" "operator-token"
                     "distribution.json" "usage.jsonl" "link/" "mcp/" "hermes/" "gateway-off/"))
      (is (not (member never (nlk::home-entry-names cloned) :test #'string=))
          (format nil "~a is never cloned" never)))))

(defun profile-test-export-calls (what &aux (calls '()))
  "The argv lists an export of WHAT -- a profile's name, or a home -- hands
RUN-CHECKED, newest first: nothing runs and nothing is moved."
  (with-stubbed-fdefinitions ((nlk::run-checked (argv-in refuse &key seconds)
                               (push argv-in calls)
                               "")
                              (nlk:replace-file (from to)
                                to))
    (if (stringp what)
        (nlk:profile-export what (namestring (temp-path what "tar.gz")))
        (nlk:export-home what (namestring (temp-path "home" "tar.gz")))))
  calls)

(deftest pin-what-an-export-carries ()
  ;; The exact members PROFILE-EXPORT hands the pass that makes the archive,
  ;; for a named profile and for the home it came from: the rows the table
  ;; ships, in its order, a linked row held back for the content pass.
  ;; Nothing the table keeps out and nothing it does not claim, so the default
  ;; home's profiles folder and sticky file stay home with no word of their own.
  (profile-test-seed-every-entry nlk:*home*)
  (write-temp-file (merge-pathnames "team/worktree/big" nlk:*home*) "no row claims it")
  (write-temp-file (merge-pathnames "active-profile" nlk:*home*) "coder")
  (nlk:profile-create "coder")
  (let ((coder (profile-test-members (profile-test-structure-call (profile-test-export-calls "coder")) "coder"))
        (shipped '("coder/config.jsonc" "coder/SOUL.md" "coder/themes.json" "coder/cells" "coder/profile.jsonc")))
    (is (equal shipped coder) "the shipped rows"))
  (let* ((folder (nlk::folder-name nlk:*home*))
         (members (profile-test-members (profile-test-structure-call (profile-test-export-calls nlk:*home*)) folder))
         (expected (mapcar (lambda (entry) (format nil "~a/~a" folder entry))
                           '("config.jsonc" "SOUL.md" "themes.json" "cells"))))
    (is (equal expected members) "no store, keys, state, profiles, sticky file, or a folder no row claims")))

(defun profile-test-seed-knowledge (home)
  "A knowledge cell under HOME's cells/, holding one memory."
  (write-temp-file (merge-pathnames "cells/tester-memory/tester-memory.asd" home)
                   "(defsystem \"tester-memory\" :class nodecode.kernel:layer :properties ((:knowledge . t)) :components ())")
  (write-temp-file (merge-pathnames "cells/tester-memory/kept.lisp" home) "carried by content"))

#-win32
(deftest an-export-carries-a-linked-knowledge-cell-by-its-content (with-temp-workspace (out))
  ;; A sharing profile's knowledge cell is a link into another home, and a
  ;; link is nothing on the machine the archive reaches: the archive carries
  ;; the memories themselves, in a second pass that dereferences exactly those.
  (profile-test-seed-every-entry nlk:*home*)
  (profile-test-seed-knowledge nlk:*home*)
  (nlk:profile-create "sharing")
  (let* ((calls (profile-test-export-calls "sharing"))
         (content (find-if (lambda (argv) (member "-h" argv :test #'string=)) calls))
         (structure (profile-test-structure-call calls)))
    (is (equal '("sharing/cells/tester-memory") (last content 1)) "a content pass, dereferenced")
    (is (member "--exclude=sharing/cells/tester-memory" structure :test #'string=) "and out of the structure pass")
    (is (member "sharing/cells" structure :test #'string=) "the other cells as they are"))
  (let ((archive (namestring (temp-path "sharing-real" "tar.gz"))))
    (is (equal archive (namestring (nlk:profile-export "sharing" archive))) "the export answers its path")
    (nlk:bind (((_ _ status)
                (nlk:run-bounded (list "tar" "-xzf" archive "-C" (namestring out)) :seconds 60)))
      (is (eql 0 status) "the archive extracts")
      (let ((cell (merge-pathnames "sharing/cells/tester-memory/" out)))
        (is (equal "carried by content" (uiop:read-file-string (merge-pathnames "kept.lisp" cell))))
        (is (null (nlk:symlink-target (merge-pathnames "sharing/cells/tester-memory" out))) "a directory, not a link")
        (is (null (probe-file (merge-pathnames "sharing/secrets/token" out))) "no secrets ride along")))))

(deftest pin-what-a-distribution-keeps-out-of-git (with-temp-workspace (remote "SOUL.md"))
  ;; The exact .git/info/exclude body PROFILE-INSTALL writes: the private
  ;; entries, then what the organism keeps for itself.
  (write-temp-file (merge-pathnames "profile.jsonc" remote) "{\"name\": \"dist\"}")
  (is (nlk:git-commit-all remote "one"))
  (nlk:profile-install (namestring remote))
  (is (equal '("# nodecode: what a profile keeps for itself"
               "/history.db*" "/.locks/" "/composer-history" "/usage.jsonl" "/theme-selection"
               "/keybinds.json" "/operator-token" "/generations/" "/release-pin" "/distribution.json"
               "/gateway-off/" "/hermes/" "/mcp/" "/secrets" "/auth.json"
               "/link" "/backups" "/cells" "")
             (uiop:split-string
              (nlk:read-text (merge-pathnames ".git/info/exclude" (nlk:profile-home "dist")))
              :separator (string #\Newline)))))

(deftest the-roster-names-entries-no-row-claims ()
  ;; A home that has accumulated junk says so, in the one place a home is
  ;; ever printed. The report names; nothing here moves or removes.
  (profile-test-seed-every-entry nlk:*home*)
  (nlk:profile-create "clean" :clone :settings)
  (is (null (nlk::unclaimed-entries (nlk:profile-home "clean"))))
  (is (null (search "also carrying" (nlk::profile-report (list (nlk:find-profile "clean"))))))
  ;; What the operator's own ~/.nodecode has accumulated, in miniature.
  (let ((home (nlk:profile-home "clean")))
    (dolist (entry '("discord.token" "agents/one.json" "shell-snapshots/a.txt" "history.db-wal"
                     "usage.jsonl"))
      (write-temp-file (merge-pathnames entry home) "x"))
    (is (equal '("agents/" "discord.token" "shell-snapshots/")
               (nlk::unclaimed-entries home)))
    (let ((line (nlk::profile-report (list (nlk:find-profile "clean")))))
      (is (search "also carrying agents/, discord.token, shell-snapshots/" line))
      (is (search "clean" line) "beside the line it belongs to"))))

(deftest the-roster-counts-what-it-has-no-room-to-name ()
  (nlk:profile-create "messy")
  (let ((home (nlk:profile-home "messy")))
    (dotimes (i 9)
      (write-temp-file (merge-pathnames (format nil "junk-~2,'0d" i) home) "x"))
    (let ((text (nlk::unclaimed-entries-text home)))
      (is-carrying text ("junk-00" "the first is named") ("junk-05" "and the sixth"))
      (is-carrying text (:absent "junk-06" "the seventh is not")
                   (", and 3 more" "it is counted")))))

(deftest the-roster-holds-against-a-home-shaped-like-the-operators ()
  ;; A fixture built from ~/.nodecode: what the table claims, and what
  ;; layout changes and cell droppings leave beside it. The gate is both
  ;; directions - every dead entry named, every live one silent.
  (nlk:profile-create "operator")
  (let ((home (nlk:profile-home "operator"))
        (dead '("agents/" "gateway/" "shell-snapshots/" "chrome-screenshots/"
                "news-diff/" "tickets/" "import/" "cells.disabled/"
                "discord.token" "telegram.token" "TICKETS.md" "timeline.txt"
                "box-watch.state" "discord-archive.db" "config.jsonc.bak"
                "config.jsonc.bak-theme"
                "cells-backup-20260907T192039Z.tar.gz"))
        (live '("config.jsonc" "auth.json" "SOUL.md" "themes.json"
                "composer-history" "theme-selection"
                "history.db" "history.db-wal" "history.db-shm"
                "history.db.lock" "history.db.owner" "history.db.serve.log"
                "cells/" "usage.jsonl" "secrets/" "link/" "mcp/"
                "profiles/")))
    ;; Every entry the two lists name is laid down, a folder holding a file.
    (dolist (entry (append dead live))
      (let ((file (if (uiop:string-suffix-p entry "/") (format nil "~akeep" entry) entry)))
        (write-temp-file (merge-pathnames file home) "x")))
    (let ((unclaimed (nlk::unclaimed-entries home)))
      (dolist (entry dead)
        (is (member entry unclaimed :test #'string=)
            (format nil "~a is named" entry)))
      (dolist (entry (cons "profile.jsonc" live))
        (is (not (member entry unclaimed :test #'string=))
            (format nil "~a is not" entry))))))

;;; --- switch: one verb decides, the shell performs ---------------------------

(deftest profile-switch-answers-a-plan-and-refuses-three-ways ()
  (with-stubbed-fdefinition (nlk:organism-binary () "/opt/nodecode/current/nodecode")
    (nlk:profile-create "coder")
    (is-values (binary argv) (nlk:profile-switch "coder" '("nodecode" "resume" "s-ABC"))
      (binary "/opt/nodecode/current/nodecode" "the plan names the installed binary")
      (argv '("/opt/nodecode/current/nodecode" "-p" "coder" "resume" "s-ABC")))
    (is-table (argv) (equal '("/opt/nodecode/current/nodecode" "-p" "coder" "resume" "s-ABC")
                            (nth-value 1 (nlk:profile-switch "coder" argv)))
      ('("nodecode" "-p" "writer" "resume" "s-ABC"))
      ('("nodecode" "resume" "s-ABC" "--profile" "writer")))
    (is (= 1 (count "-p" (nth-value 1 (nlk:profile-switch "coder" '("nodecode" "-p" "writer")))
                    :test #'string=)))
    ;; What the launch asked of its first session was sent there: the other
    ;; profile's fresh session would only run the prompt again.
    (is (equal '("/opt/nodecode/current/nodecode" "-p" "coder" "--ephemeral")
               (nth-value 1 (nlk:profile-switch "coder" '("nodecode" "--model" "m" "--ephemeral" "--prompt-file" "f"
                                                       "--" "-p" "x")))))
    ;; The three refusals.
    (let ((unknown (refusal-text nlk:profile-error (nlk:profile-switch "codr"))))
      (is (search "no profile \"codr\"" unknown) "an unknown profile refuses")
      (is (search "Did you mean \"coder\"?" unknown) "naming the nearest"))
    ;; The hermetic posture puts *HOME* in a scratch folder that is no
     ;; profile's, so a home has to be stood in before "already there"
     ;; can be asked at all.
    (with-saved-globals ((nodecode.kernel::*home* (nlk:profile-home "coder")))
      (is (nlk:profile-record-current-p (nlk:find-profile "coder")) "this shell runs coder")
      (is (search "already running coder"
                  (refusal-text nlk:profile-error (nlk:profile-switch "coder")))))))

(deftest profile-switch-refuses-a-developer-image ()
  (with-stubbed-fdefinition (nlk:organism-binary () nil)
    (nlk:profile-create "coder")
    (let ((refusal (refusal-text nlk:profile-error (nlk:profile-switch "coder"))))
      (is (search "not started from an installed binary" refusal) "it says why")
      (is (search "nodecode -p coder" refusal) "and what to run instead")
      (is (search "profile use coder" refusal) "and the other way there"))))

(deftest profile-switch-is-a-verb-the-shells-reach ()
  (is (find "switch" nlk:*profile-verbs* :key #'nlk:verb-name :test #'string=))
  (is (search "switch" (nlk:profile-command "help")) "help lists it")
  (is (uiop:string-prefix-p "usage: profile switch NAME" (nlk:profile-command "help switch")))
  (with-stubbed-fdefinition (nlk:organism-binary () "/opt/nodecode/current/nodecode")
    (nlk:profile-create "coder")
    (let ((performed '()))
      (let ((nlk:*profile-switch-performer*
              (lambda (name binary argv)
                (setf performed (list name binary argv))
                (format nil "parked ~a" name))))
        (is (equal "parked coder" (nlk:profile-command "switch coder")))
        (is-shape performed (first "coder") (second "/opt/nodecode/current/nodecode"))
        (is (member "-p" (third performed) :test #'string=) "with the plan's argv"))
      (is (signals-error nlk:profile-usage-error (nlk:profile-command "switch")))
      (is (signals-error nlk:profile-usage-error (nlk:profile-command "switch coder extra"))))))

(deftest profile-use-is-untouched-by-the-switch-verb ()
  ;; `use' is the next bare launch; `switch' is now. Both survive, and
  ;; use's wording is what it was.
  (nlk:profile-create "coder")
  (is (equal (format nil "coder boots by default from the next launch on; this process stays on ~a"
                     (nlk:profile-name))
             (nlk:profile-command "use coder")))
  (is (equal "coder" (nlk:sticky-profile)) "and the sticky file moved"))

(deftest create-carries-the-settings-and-blank-is-the-opt-in ()
  ;; A second profile almost always wants the first one's abilities, so
  ;; the common case stopped being the flagged one. --clone is gone with
  ;; no shim: it names nothing now.
  (is (equal "settings" (gethash "clone" (nlk::read-manifest (nlk:profile-create "carried")))))
  (is (equal "none"
             (gethash "clone" (nlk::read-manifest (nlk:profile-create "empty" :clone :blank)))))
  (is (signals-error nlk:profile-error (nlk:profile-create "bad" :clone :nonsense))))

#-win32
(deftest create-shares-what-belongs-to-the-operator-rather-than-the-home ()
  ;; The answer to "should a new profile inherit the memory?": not inherit —
  ;; read the same folder. One correction, one copy, and the folder's lock
  ;; (a-folder-lock-* in os-test) is what keeps two writers apart.
  (profile-test-seed-every-entry nlk:*home*)
  (profile-test-seed-knowledge nlk:*home*)
  (let* ((source nlk:*home*)
         (home (nlk:profile-create "sharing")))
    (dolist (entry '("secrets" "auth.json" "cells/tester-memory"))
      (is (equal (namestring (truename (merge-pathnames entry source)))
                 (namestring (truename (merge-pathnames entry home))))
          (format nil "~a in the new home is this home's, through a link" entry)))
    (is (equal '("secrets" "auth.json" "cells/tester-memory")
               (nlk::manifest-list home "shared")) "the manifest names them")
    (write-temp-file (merge-pathnames "cells/tester-memory/later.lisp" source) "kept here")
    (is (equal "kept here"
               (uiop:read-file-string (merge-pathnames "cells/tester-memory/later.lisp" home))) "read through the link")
    ;; A keystroke's worth of the operator-facing text.
    (is (search "sharing secrets, auth.json, cells/tester-memory"
                (nlk:profile-command "create twin")) "the create answer says it")
    (is (search "shares secrets, auth.json, cells/tester-memory"
                (nlk:profile-command "list")) "and the roster says so too"))
  ;; A fork is an organism of its own: it copies even the shared rows.
  (let ((fork (nlk:profile-create "fork" :clone :all)))
    (is (null (nlk:symlink-target (merge-pathnames "secrets/" fork))) "a fork owns its secrets")
    (is (null (nlk:symlink-target (merge-pathnames "cells/tester-memory/" fork))) "and its memory")
    (is (equal '() (nlk::manifest-list fork "shared")) "and its manifest shares nothing")))

(deftest create-publishes-with-one-rename-and-leaves-no-half-home ()
  ;; The home is built in a dot-named staging tree and published with one
  ;; rename, so a shell or gateway walking profiles/ never sees a half-made
  ;; home. The dot also keeps it out of the roster, whose scan takes a folder
  ;; whose name is a profile name.
  (is (not (nlk:profile-name-p ".creating-coder-1k2j")) "a staging tree is not a profile name")
  (profile-test-seed-every-entry nlk:*home*)
  (nlk:profile-create "sudden")
  (is (equal '() (profile-test-staging-trees)) "a published home leaves nothing beside it")
  ;; A create that dies in the middle leaves nothing at all: this refusal is
  ;; raised after the staging tree was made.
  (is (signals-error nlk:profile-error (nlk:profile-create "half" :from "no-such-home")))
  (is (null (uiop:directory-exists-p (nlk:profile-home "half"))) "no home")
  (is (equal '() (profile-test-staging-trees)) "and no staging tree"))

(deftest a-home-never-holds-another-home-s-bot-token ()
  ;; A bot token belongs to one profile: two homes polling one Telegram token
  ;; answer each other's 409, and one Discord bot cannot hold two gateways.
  ;; The channels are left behind, and the manifest and the answer both say so.
  (write-temp-file (merge-pathnames "config.jsonc" nlk:*home*)
                   (format nil "{~%  // the operator's own words~%  \"general\": {\"theme\": \"dark\"},~%  \"channels\": {\"telegram\": {\"bot_token_env\": \"TELEGRAM_BOT_TOKEN\"}}~%}~%"))
  (let ((home (nlk:profile-create "quiet")))
    (let ((text (uiop:read-file-string (merge-pathnames "config.jsonc" home))))
      (is (null (search "channels" text)) "the channels member is gone")
      (is (search "the operator's own words" text) "the comment stays")
      (is (search "\"theme\": \"dark\"" text) "and so does the setting beside it"))
    (is (equal '("channels") (nlk::manifest-list home "dropped")) "the manifest names the drop")
    (is (search "a bot token belongs to one home" (nlk:profile-command "create silent")) "said in the answer"))
  ;; A fork drops them too: a second home is a second poller whatever it is.
  (let ((fork (nlk:profile-create "loud" :clone :all)))
    (is (null (search "channels" (uiop:read-file-string (merge-pathnames "config.jsonc" fork)))) "a fork too"))
  ;; A home made from a config that names no channels has nothing to say.
  (write-temp-file (merge-pathnames "config.jsonc" nlk:*home*) "{\"general\": {}}")
  (let ((home (nlk:profile-create "plain")))
    (is (equal '() (nlk::manifest-list home "dropped")) "nothing dropped, nothing claimed")
    (is (null (search "a bot token belongs to one home" (nlk:profile-command "create plainer"))) "quiet")))

(deftest the-create-verb-reads-blank-and-no-longer-reads-clone ()
  (profile-test-seed-every-entry nlk:*home*)
  (let ((answer (nlk:profile-command "create carried")))
    (is (search "the settings of" answer) "the answer says what came along"))
  (is (probe-file (merge-pathnames "config.jsonc" (nlk:profile-home "carried"))))
  (nlk:profile-command "create empty --blank")
  (is (null (probe-file (merge-pathnames "config.jsonc" (nlk:profile-home "empty")))))
  (is (signals-error nlk:profile-usage-error (nlk:profile-command "create gone --clone")))
  (is (signals-error nlk:profile-usage-error (nlk:profile-command "create both --blank --clone-all")))
  (let ((help (nlk:profile-command "help create")))
    (is (search "--blank" help) "the help offers --blank")
    (is (null (search "--clone " help)) "and no longer offers --clone")
    (is (search "--clone-all" help) "--clone-all keeps its exact meaning")))

(deftest a-parked-switch-is-performed-only-after-the-shell-let-go (let ((order '())))
  ;; The ordering is the contract both shells run inside: the program loop
  ;; returns (its own unwind gave the terminal back), the store is released,
  ;; and only then is the process replaced. An exec keeps open file
  ;; descriptors, so a lease still held would be inherited into the new
  ;; image and deadlock it against itself.
  (with-stubbed-fdefinition (nlk:close-store () (push :store-released order) nil)
    (with-saved-globals ((nodecode.kernel::*profile-switch-performer*
                          (nlk:lambda-ignoring (name binary argv)
                            (push (list :exec name) order)
                            nil)))
      (let* ((answer nil)
             (program (nlt:make-program nil))
             (returned (progn
                         (setf (nlt:program-running program) t)
                         (nle::call-with-profile-switch
                          program
                          (lambda ()
                            (setf answer (funcall nodecode.kernel::*profile-switch-performer*
                                                  "coder" "/opt/nodecode"
                                                  '("/opt/nodecode" "-p" "coder")))
                            (push (if (some #'nlt:quit-msg-p
                                            (nlt::mailbox-drain
                                             (nlt::program-mailbox program) :timeout 1))
                                      :loop-asked-to-quit
                                      :no-quit)
                                  order)
                            (push :loop-returned order)
                            :the-shells-value)))))
        (is (search "switching to coder" answer))
        (is (member :loop-asked-to-quit order))
        (is (equal '(:loop-asked-to-quit :loop-returned :store-released (:exec "coder"))
                   (reverse order)))
        (is (eq :the-shells-value returned) "the shell's own value survives the seam")))))

(deftest a-shell-with-nothing-parked-exits-exactly-as-before (let ((touched '())))
  (with-stubbed-fdefinition (nlk:close-store () (push :store-released touched) nil)
    (with-saved-globals ((nodecode.kernel::*profile-switch-performer*
                          (nlk:lambda-ignoring (name binary argv)
                            (push :exec touched)
                            nil)))
      (is (eq :ordinary-exit
              (nle::call-with-profile-switch (nlt:make-program nil)
                                             (lambda () :ordinary-exit))))
      (is (null touched) "nothing was closed and nothing was exec'd"))))
