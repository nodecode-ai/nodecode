;;;; import-test.lisp --- the plan over a box, dry and applied.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (item nik::item) (stop nik::item))

(deftest import-clock-reads-seconds-as-utc ()
  (is-table (in want) (equal want (nlk:iso-time in)) (0 "1970-01-01T00:00:00.000Z")
    (1782252307 "2026-06-23T22:05:07.000Z") (1782252307.25d0 "2026-06-23T22:05:07.250Z"))
  (is (equal "2026-06-23T22:05:07.000Z" (nik::iso-from-any "2026-06-23T22:05:07")))
  (is (equal "2026-06-23T22:05:07.000Z" (nik::iso-from-any 1782252307000)))
  (is (null (nik::iso-from-any "soon"))))

(deftest import-plan-reads-a-home-dry (with-import-runtime (home box :hermes))
  ;; A dry run names every piece and writes nothing: the config stays as it
  ;; was, no auth.json, no SOUL.md, no skills folder — and the keys and
  ;; tokens the home carries are in the plan like everything else, never
  ;; shown.
  (let ((plan (hermes-plan home)))
    (is (equal '("auth.json api_keys.openrouter" "auth.json api_keys.subrouter-1"
                 "general.default_model" "providers.subrouter-1")
               (sort (mapcar #'nik:item-destination (import-items plan "providers")) #'string<)))
    (is (every (lambda (status) (equal status "imported")) (import-statuses plan "providers")))
    (is (null (import-item plan "providers" "providers.openrouter")))
    (is (search "gpt-5.5 on subrouter-1"
                (nik:item-reason (import-item plan "providers" "general.default_model"))))
    (is (equal '("imported") (import-statuses plan "instructions")) "one persona")
    (is (equal '("imported" "imported" "imported") (import-statuses plan "memory")))
    (is (equal '("imported") (import-statuses plan "skills")) "one skill")
    (is (null (import-item plan "skills" ".archive")) "a hidden folder is not walked")
    (is (equal '("imported") (import-statuses plan "profiles")))
    (is (search "nodecode -p coder import hermes --source"
                (nik:item-reason (import-item plan "profiles" "profiles/coder"))))
    (is (equal '("imported" "imported" "skipped" "skipped")
               (sort (copy-list (import-statuses plan "mcp")) #'string<)) "two servers and the secrets left out of them")
    (is (search "cwd dropped" (nik:item-reason (import-item plan "mcp" "github"))))
    (is (equal '("imported" "imported") (import-statuses plan "channels")) "telegram and discord")
    (is (search "token in a 0600 file"
                (nik:item-reason (import-item plan "channels" "channels.telegram"))))
    (is (equal '("imported" "imported" "skipped") (import-statuses plan "cron")))
    (is (= 2 (count "imported" (import-statuses plan "sessions") :test #'string=)))
    (let ((report (nik:report-text plan)))
      (is (search "dry run" report))
      (is-table (in) (not (search in report)) (+fixture-key+ "no key is ever printed")
        (+fixture-token+ "and no bot token") ("FAKE-not-a-token-github" "nor an MCP server's env")))
    (is (null (probe-file (merge-pathnames "auth.json" home))) "nothing written")
    (is (null (probe-file (merge-pathnames "SOUL.md" home))))
    (is (null (uiop:directory-exists-p (nlk:knowledge-directory))) "no knowledge cell either")))

(deftest import-apply-lands-each-piece-through-its-own-seam (with-import-runtime (home box :hermes))
  ;; Every piece but the cron jobs, which land through the cron folder and
  ;; are its lane's to prove (CRON-CELL-LANDS-AN-IMPORTED-JOB).
  (with-temp-store ()
    (let ((plan (nik:apply-plan (hermes-plan home :without "cron"))))
      (is (zerop (cdr (assoc "error" (nik::summary-counts plan) :test #'string=))))
      (is-shape (import-config)
        ((:text "providers" "subrouter-1" "base_url") "https://ai.example.test/v1")
        ((:text "providers" "subrouter-1" "sdk") "openai-completions")
        ((:text "general" "default_model" "provider") "subrouter-1")
        ((:text "mcp" "servers" "github" "command") "npx")
        ((:text "mcp" "servers" "github" "env" "LOG_LEVEL") "info")
        ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone"))
        ((:any "channels" "telegram" "reactions") eq t))
      (let ((auth (import-auth)))
        (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "subrouter-1" "key")))
        (is (equal +fixture-key-2+ (nlk:json-value auth :text "api_keys" "openrouter" "key"))))
      ;; Under the heading that names the home it came from, as each home's rules land.
      (is (equal (format nil "## hermes — SOUL.md~%~%Speak plainly. Never flatter.~%")
                 (uiop:read-file-string (merge-pathnames "SOUL.md" home))))
      (is-present (skill (kept-text "notes")) "the skill is a definition in the knowledge cell"
        (is (search "(define-skill notes" skill))
        (is (search ":origin \"hermes\"" skill) "the world it came from, which the index ranks by")
        (is (search ";; support files not carried: scripts/run.py (11 bytes)" skill) "a script is named, not carried"))
      (let ((secret (merge-pathnames "secrets/channels.telegram.token_file" home)))
        (is (probe-file secret) "the bot token is a file of its own")
        (is (equal +fixture-token+ (string-trim '(#\Newline) (uiop:read-file-string secret)))))
      (is (member "nodecode-channel-telegram" *import-refreshed* :test #'equal))
      (is (= 3 (length (directory (merge-pathnames "tester-memory/hermes-*.lisp" nlk::*cells-directory*)))))
      (is (search "— session hermes import"
                  (nlk::git-in-layer (nlk:knowledge-directory) "log" "--format=%s" "-1")) "kept under the import's name")
      (let ((subjects (uiop:split-string (nlk::git-in-layer (nlk:knowledge-directory) "log" "--format=%s")
                                         :separator '(#\Newline))))
        (is (= 1 (count-if (lambda (line) (search "— session hermes import" line)) subjects)) "the memories and the skill are one commit, not one each"))
      (is (nlk:session-exists-p "hermes-20260623_150503_55c2a9"))
      (is (probe-file (nik::plan-report-path plan)) "the report is on disk"))))

(deftest import-leaves-an-mcp-servers-secrets-where-they-are (with-import-runtime (home box))
  ;; The mcp folder reads a server's env and headers from config.jsonc as
  ;; written, a file anyone on the box can read, and takes no reference to a
  ;; 0600 file: a secret in either has no safe place to go. It stays in its
  ;; home, the report names it, and what is not a secret comes over.
  (make-hermes-fixture box)
  (let* ((plan (hermes-plan home :only "mcp"))
         (left (remove "skipped" (import-items plan "mcp") :key #'nik:item-status :test-not #'string=))
         (github (find "mcp.servers.github" left :key #'nik:item-destination :test #'equal))
         (docs (find "mcp.servers.docs" left :key #'nik:item-destination :test #'equal)))
    (is (search "env GITHUB_PERSONAL_ACCESS_TOKEN" (nik:item-reason github)) "the variable, by name")
    (is (search "headers Authorization" (nik:item-reason docs)) "the header, by name")
    (is (not (search "LOG_LEVEL" (nik:item-reason github))) "and only the secrets")
    (is (search "config.jsonc" (nik:item-reason github)) "and where to add it by hand")
    ;; That path written as the operator writes it, from ~. A path under the
    ;; home that names nothing, read by this plan alone.
    (let* ((nle::*shared-config-path* (merge-pathnames ".nl-import-absent/config.jsonc" (user-homedir-pathname)))
           (nle::*shared-config-memo* nil)
           (reason (nik:item-reason (find "mcp.servers.github"
                                          (remove "skipped" (import-items (hermes-plan home :only "mcp") "mcp")
                                                  :key #'nik:item-status :test-not #'string=)
                                          :key #'nik:item-destination :test #'equal))))
      (is-carrying (text reason)
        "in ~/.nl-import-absent/config.jsonc yourself"
        (:absent (namestring (user-homedir-pathname)))))
    (nik:apply-plan plan)
    (is-shape (import-config)
      ((:text "mcp" "servers" "github" "command") "npx")
      ((:text "mcp" "servers" "github" "env" "LOG_LEVEL") "info")
      ((:text "mcp" "servers" "docs" "url") "https://docs.example.test/mcp")
      ((:text "mcp" "servers" "docs" "headers" "X-Client") "hermes"))
    (let ((config (uiop:read-file-string nle::*shared-config-path*))
          (report (uiop:read-file-string (nik::plan-report-path plan))))
      (dolist (secret '("FAKE-not-a-token-github" "FAKE-not-a-token-docs"))
        (is (not (search secret config)) "no secret in the shared config")
        (is (not (search secret report)) "or in the report")))))

(deftest import-lands-an-mcp-server-off-and-says-what-it-would-run (with-import-runtime (home box))
  ;; vr-116: every imported server started at the next launch, before any
  ;; task, and npx and uvx fetched and ran their packages unasked. A server
  ;; lands off; the report says what it would run, where that comes from, and
  ;; how the operator starts it.
  (make-hermes-fixture box)
  (let* ((plan (hermes-plan home :only "mcp"))
         (github (import-item plan "mcp" "mcp.servers.github"))
         (docs (import-item plan "mcp" "mcp.servers.docs")))
    (is (search "npx -y @modelcontextprotocol/server-github (fetched from npm); lands off: /mcp on github starts it"
                (nik:item-reason github)))
    (is (search "https://docs.example.test/mcp; lands off: /mcp on docs starts it" (nik:item-reason docs)))
    (nik:apply-plan plan)
    (dolist (name '("github" "docs"))
      (let ((entry (nlk:json-value (import-config) :object "mcp" "servers" name)))
        (is (and entry (not (nlk:config-boolean entry "enabled" t))) name))))
  ;; A secret rides a server's arguments as often as its env: the report says
  ;; the command up to the package a runner fetches, masked, and counts the
  ;; rest. Windows spells a runner `npx.cmd' or `cmd /c npx'.
  (dolist (case '(("npx" #("-y" "mcp-remote" "https://mcp.example/sse" "--header" "Authorization: Bearer sk-live-SECRET123")
                   "npx -y mcp-remote and 3 more arguments (fetched from npm)")
                  ("docker" #("run" "-i" "--rm" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN=ghp_SECRET0123456789" "ghcr.io/github/github-mcp-server")
                   "docker run and 5 more arguments (fetched from a container registry)")
                  ("mcp-server-postgres" #("postgresql://admin:SECRETPW@db.internal/prod")
                   "mcp-server-postgres with 1 argument")
                  ("uvx" #("--from" "git+https://bot:SECRETPW@git.example/mcp.git" "mcp-git")
                   "uvx --from git+https://[set]@git.example/mcp.git and 1 more argument (fetched from PyPI)")
                  ("uvx" #("--api-key" "SECRET-sk-proj-0123456789" "--token=SECRET" "pkg")
                   "uvx --api-key [set] and 2 more arguments (fetched from PyPI)")
                  ("uvx" #("--token=SECRET" "-y" "pkg" "sk-proj-SECRET0123456789")
                   "uvx --token=[set] -y pkg and 1 more argument (fetched from PyPI)")
                  ("cmd" #("/c" "npx" "-y" "@upstash/context7-mcp") "cmd /c npx -y @upstash/context7-mcp (fetched from npm)")
                  ("C:\\nodejs\\npx.cmd" #("-y" "@upstash/context7-mcp")
                   "C:\\nodejs\\npx.cmd -y @upstash/context7-mcp (fetched from npm)")))
    (destructuring-bind (command args line) case
      (is (equal line (nik::server-line (nlk:json-object "command" command "args" args))) line)))
  ;; a url's password and a secret in its query
  (is (equal "https://[set]@mcp.example/sse?token=[set]"
             (nik::server-line (nlk:json-object "url" "https://bot:SECRETPW@mcp.example/sse?token=SECRET")))))

(deftest import-plan-leaves-a-kind-out-and-says-it-is-waiting (with-import-runtime (home box))
  ;; The first frame brings everything but the conversations. The plan says
  ;; so in its answer, so the closing line can tell the operator where they
  ;; went instead of leaving them to wonder.
  (make-hermes-fixture box)
  (let ((plan (hermes-plan home :without "sessions")))
    (is (null (import-items plan "sessions")) "no conversation is read")
    (is (import-items plan "providers") "and everything else still is")
    (is (eq t (gethash "sessions_left" (nik:plan-json plan))) "the answer says they are waiting"))
  (let ((value (nik:plan-json (hermes-plan home :only "providers,sessions" :without "sessions"))))
    (is (eq t (gethash "sessions_left" value)) "WITHOUT outranks ONLY"))
  (is (eq :false (gethash "sessions_left" (nik:plan-json (hermes-plan home))))))

(deftest import-reads-a-world-nobody-here-has (with-import-runtime (home box :openclaw))
  ;; Openclaw, written from its own docs: an auth profile keyed
  ;; `<provider>:<label>', an OAuth profile beside it, a custom provider, a
  ;; bot token, a model pick, a workspace of instructions and a memory day
  ;; file, and a skill. Not one line of this folder knows the word
  ;; `openclaw' — it is a row in a table, and everything else is a shape.
  (let ((facts (home-facts-of "openclaw")))
    (is-table (in want) (equal want (fact-ids facts in)) (:provider '("acmerouter" "anthropic"))
      (:oauth '("anthropic") "the grant is named and left") (:mcp '("weather"))
      (:skill '("weather-brief")) (:instructions '("AGENTS" "IDENTITY" "SOUL")))
    (is (= 1 (length (fact-ids facts :memory))) "and its memory day file")
    (is (equal '("telegram") (fact-ids facts :channel)))
    (let ((model (first (remove :model facts :key #'nik:fact-kind :test-not #'eq))))
      (is-shape (nik:fact-value model) (:provider "anthropic") (:model "claude-opus-5")))))

(deftest import-reads-command-code-and-windsurf (with-import-runtime (home box))
  ;; Two more worlds nobody here has: one that keeps a single key in
  ;; auth.json, and one whose whole contribution is a folder of remembered
  ;; entries -- and its global_rules.md among them, which is its rules for
  ;; every session, not one more thing remembered (an-116).
  (make-commandcode-fixture box)
  (make-windsurf-fixture box)
  (is (equal '("commandcode") (fact-ids (home-facts-of "commandcode") :provider)))
  (let ((facts (home-facts-of "windsurf")))
    (is (= 1 (length (fact-ids facts :memory))) "the dated memory file")
    (is (equal '("GLOBAL_RULES") (fact-ids facts :instructions)) "the rules are instructions")))

(deftest import-lands-rules-in-agents-md-and-a-persona-in-soul-md (with-import-runtime (home box))
  ;; am-101: a harness's rules for every session -- Claude Code's CLAUDE.md,
  ;; Codex's AGENTS.md, Windsurf's global_rules.md -- land in this home's
  ;; AGENTS.md, which every session reads; a persona (SOUL.md, IDENTITY.md)
  ;; in its SOUL.md, the channels' voice. Both went to SOUL.md, which no
  ;; session in a terminal reads, while the report said imported.
  (import-write box ".claude/settings.json" "{}")
  (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use just.~%"))
  (make-codex-fixture box)
  (make-windsurf-fixture box)
  (make-openclaw-fixture box)
  (let* ((rules (namestring (merge-pathnames "AGENTS.md" home)))
         (soul (namestring (merge-pathnames "SOUL.md" home)))
         (plan (nik:make-plan :only "instructions"))
         (items (import-items plan "instructions")))
    ;; One item a home's file, the rules ahead of the persona.
    (is (equal (list rules soul) (nlk:distinct (mapcar #'nik:item-destination items))))
    (is (every (lambda (item) (equal "imported" (nik:item-status item))) items))
    ;; the report names AGENTS.md, written from the home
    (is (search (format nil "→ ~a —" (nlk:home-abbreviated rules)) (nik:report-text plan)))
    ;; xa-201: the persona was reported as imported instructions, and no
    ;; terminal session reads SOUL.md; its line says who does.
    (is-carrying (text (nik:item-reason (find soul items :key #'nik:item-destination :test #'equal)))
      "the voice your chat bots speak in" "a terminal session does not read SOUL.md")
    (nik:apply-plan plan)
    (is-carrying (text (uiop:read-file-string rules))
      "Use just." "Small diffs." "Always run the linter" "Answer in one paragraph."
      "## claude — CLAUDE.md" (:absent "Dry, exact" "no persona among the rules"))
    (is-carrying (text (uiop:read-file-string soul))
      "Dry, exact" "second pair of hands" (:absent "Use just." "no rules in the voice")))
  ;; An AGENTS.md already here is the operator's: it stays, first, and the
  ;; rules land after it, each under its own heading.
  (with-temp-directory (fresh "import-home-2")
    (import-write fresh "AGENTS.md" "mine")
    (let ((plan (nik:make-plan :settings (list :home fresh) :only "instructions")))
      (is (every (lambda (status) (equal status "imported")) (import-statuses plan "instructions")))
      (nik:apply-plan plan))
    (is-carrying (text (uiop:read-file-string (merge-pathnames "AGENTS.md" fresh)))
      (is (uiop:string-prefix-p "mine" text) "what was here stays, first")
      "## codex — AGENTS.md" "Small diffs.")))

(deftest import-keeps-the-rules-within-what-a-session-reads (with-import-runtime (home box))
  ;; xh-201, xn-202: a session refuses the home's AGENTS.md whole past the
  ;; core's limit (context.lisp OPERATOR-RULES-SECTION), and the import joined
  ;; every home's rules into it unsized and said imported, so no session read
  ;; any of them. What fits lands; a file that would take AGENTS.md past the
  ;; limit is left out, named, with its size and what to do.
  (let ((limit nle::+instructions-byte-limit+))
    (import-write box ".claude/settings.json" "{}")
    (import-write box ".claude/CLAUDE.md"
                  (format nil "# Claude rules~%~%~a~%" (make-string (floor limit 2) :initial-element #\c)))
    (make-codex-fixture box)
    (import-write box ".codex/AGENTS.md"
                  (format nil "# Codex rules~%~%~a~%" (make-string (floor limit 2) :initial-element #\x)))
    (let* ((plan (nik:make-plan :source '("claude" "codex") :settings (list :home (nlk:home))
                                :only "instructions"))
           (items (import-items plan "instructions"))
           (kept (find "imported" items :key #'nik:item-status :test #'equal))
           (left (find "skipped" items :key #'nik:item-status :test #'equal)))
      (is (= 2 (length items)) "each fits alone, the two together do not")
      (is-present kept "one lands"
        (is-carrying (text (nik:item-reason kept))
          "with it AGENTS.md is"
          (is (search (format nil "of the ~a a session reads" (nlk:size-text limit)) text))))
      (is-present left "the other is left out, and said"
        (is-carrying (text (nik:item-reason left))
          "left out: with it" "AGENTS.md would be"
          (is (search (format nil "past the ~a a session reads" (nlk:size-text limit)) text))
          ;; The file to trim, by its path, and the command that tries again.
          (is (search (format nil "/~a or what is already there, and run `nodecode import ~a'"
                              (nik:item-source left) (nik::item-world left))
                      text))))
      (nik:apply-plan plan)
      (is (<= (nlk:utf-8-size (uiop:read-file-string (nlk:home "AGENTS.md"))) limit))
      (is-present (text (cdr (nle::operator-rules-section))) "and a session reads what landed"
        (is (search (format nil "## ~a — ~a" (nik::item-world kept) (nik:item-source kept)) text)))))
  ;; A file past the limit on its own says that.
  (import-write box ".claude/CLAUDE.md" (make-string (* 2 nle::+instructions-byte-limit+) :initial-element #\c))
  (let ((item (first (import-items (nik:make-plan :source "claude" :only "instructions")
                                   "instructions"))))
    (is (equal "skipped" (nik:item-status item)))
    (is-carrying (text (nik:item-reason item))
      (is (search (format nil ".claude/CLAUDE.md is ~a" (nlk:size-text (* 2 nle::+instructions-byte-limit+))) text))
      "trim it and run `nodecode import claude'")
    (is (null (probe-file (merge-pathnames "AGENTS.md" home))) "nothing written for it"))
  ;; Sized as a session sizes it, in UTF-8 bytes: under the limit in
  ;; characters but over it in bytes, a file would land where no session
  ;; reads it.
  (let ((characters (floor nle::+instructions-byte-limit+ 2)))
    (import-write box ".claude/CLAUDE.md" (make-string characters :initial-element (code-char #x65E5)))
    (let ((item (first (import-items (nik:make-plan :source "claude" :only "instructions")
                                     "instructions"))))
      (is (equal "skipped" (nik:item-status item)) "32 Ki characters of CJK are 96 KiB")
      (is (search (format nil ".claude/CLAUDE.md is ~a" (nlk:size-text (* 3 characters))) (nik:item-reason item)))
      (is (null (probe-file (merge-pathnames "AGENTS.md" home))) "nothing written for it")))
  ;; A persona is held to what a chat bot reads (channels/kit/soul.lisp):
  ;; past it, no bot speaks in it, so it is not reported as their voice.
  (import-write box ".hermes/SOUL.md" (make-string (* 2 nlk::+max-harness-bytes+) :initial-element #\s))
  (let ((item (first (import-items (nik:make-plan :source "hermes" :only "instructions")
                                   "instructions"))))
    (is (equal "skipped" (nik:item-status item)))
    (is-carrying (text (nik:item-reason item))
      ".hermes/SOUL.md is" (:absent "the voice your chat bots speak in" "not called their voice")
      (is (search (format nil "past the ~a a chat bot reads" (nlk:size-text nlk::+max-harness-bytes+)) text)))
    (is (null (probe-file (merge-pathnames "SOUL.md" home))) "nothing written for it"))
  ;; A home read at a path is no world the import verb takes by name: it is
  ;; named again by its path.
  (import-write box "backup/.claude/CLAUDE.md"
                (make-string (* 2 nle::+instructions-byte-limit+) :initial-element #\c))
  (is (search "run `nodecode import --source "
              (nik:item-reason
               (first (import-items (nik:make-plan :source (namestring (merge-pathnames "backup/.claude/" box))
                                                   :only "instructions")
                                    "instructions"))))))

(deftest import-adds-a-later-homes-rules-under-their-own-heading (with-import-runtime (home box))
  ;; xa-205: a later import found AGENTS.md here and could only leave its
  ;; rules out or, under --overwrite, replace the file whole -- the first
  ;; home's rules gone without a word. Each home's rules are a section under
  ;; its own heading: a later import adds its section after what is here, a
  ;; re-run finds them already there, and --overwrite replaces that one
  ;; section, and says so.
  (make-codex-fixture box)
  (flet ((run (source &rest keys)
           (let ((plan (apply #'nik:make-plan :source source :only "instructions" keys)))
             (nik:apply-plan plan)
             (first (import-items plan "instructions"))))
         (rules () (uiop:read-file-string (merge-pathnames "AGENTS.md" home))))
    (is (search "added under `## codex — AGENTS.md'" (nik:item-reason (run "codex"))))
    ;; A home read at a path lands under its path, as the operator writes
    ;; it, a section of its own too.
    (let ((backup (merge-pathnames "backup/.claude/" box)))
      (import-write backup "CLAUDE.md" (format nil "Path rules.~%"))
      (is (search (format nil "added under `## ~a — CLAUDE.md'"
                          (nlk:home-abbreviated (uiop:native-namestring backup)))
                  (nik:item-reason (run (namestring backup)))))
      ;; Under the operator's home that path reads `~/...', a heading still.
      (is (nik::rules-heading-p "## ~/backup/.claude — CLAUDE.md")))
    (import-write box ".claude/settings.json" "{}")
    ;; A heading of the rules' own that reads like a world's is no section.
    (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use just.~%~%## Claude — style~%~%Short lines.~%"))
    (let ((item (run "claude")))
      (is (equal "imported" (nik:item-status item)) "a later home's rules are added, not a conflict")
      (is (search "added under `## claude — CLAUDE.md'" (nik:item-reason item))))
    (is-carrying (text (rules)) "Small diffs." "Use just." "Path rules.")
    (is (equal "skipped" (nik:item-status (run "claude"))) "a re-run finds its rules already here")
    ;; The section after the one --overwrite replaces is another home's, and stays.
    (import-write box ".codex/AGENTS.md" (format nil "# Codex rules~%~%Big diffs.~%"))
    (is (search "replaced its own section `## codex — AGENTS.md'"
                (nik:item-reason (run "codex" :overwrite t))))
    (is-carrying (text (rules))
      "Big diffs." ("Path rules." "the home read at a path keeps its section") "Use just."
      (:absent "Small diffs."))
    (import-write home "AGENTS.md" (ppcre:regex-replace-all "\\n" (rules) (format nil "~c~%" #\Return)))
    (is (equal "skipped" (nik:item-status (run "claude"))) "and in a file saved with CRLF endings")
    (import-write box ".claude/CLAUDE.md" (format nil "# House rules~%~%Use make.~%"))
    (let ((item (run "claude")))
      (is (equal "conflict" (nik:item-status item)) "changed rules wait for --overwrite")
      (is (search "--overwrite replaces that section alone" (nik:item-reason item))))
    (is (search "replaced its own section `## claude — CLAUDE.md'"
                (nik:item-reason (run "claude" :overwrite t))))
    (is-carrying (text (rules))
      "Big diffs." "Path rules." "Use make."
      (:absent "Use just." "the old section is gone") (:absent "Short lines." "all of it")
      ;; In the order they came.
      (is (< (search "## codex" text) (search "Path rules." text) (search "## claude" text))))))

(deftest import-reads-a-home-written-in-toml (with-import-runtime (home box :codex))
  (let ((facts (home-facts-of "codex")))
    (is (equal '("acmeprivate")
               (remove-duplicates (fact-ids facts :provider) :test #'equal)))
    (is (equal '("docs") (fact-ids facts :mcp)))
    (is (equal '("AGENTS") (fact-ids facts :instructions)))
    (nik:apply-plan (nik:make-plan :source "codex"))
    (is-shape (import-config)
      ((:text "providers" "acmeprivate" "base_url") "https://private.example.test/v1")
      ((:text "general" "default_model" "provider") "acmeprivate"))
    (is (equal +fixture-key+
               (nlk:json-value (import-auth) :text "api_keys" "acmeprivate" "key")))))

(deftest import-merges-every-home-on-the-box-newest-first (with-import-runtime (home box))
  ;; A real box carries several homes and the useful facts are spread over
  ;; them. Naming no world reads all of them; a provider the newest world
  ;; names wins, and everything else is a union.
  (make-commandcode-fixture box)
  (make-openclaw-fixture box)
  (let ((plan (nik:make-plan)))
    (is (< 1 (length (nik::plan-homes plan))) "both homes are read")
    (let ((keys (remove-if-not (lambda (item)
                                 (search "auth.json api_keys." (or item.destination "")))
                               (import-items plan "providers"))))
      (is (= 3 (length keys)))
      (dolist (id '("acmerouter" "anthropic" "commandcode"))
        (is (find-if (lambda (item) (search id item.destination)) keys)
            (format nil "~a's key lands" id))))
    (is (search "Openclaw" (nik::plan-source-line plan)))))

(deftest import-never-clobbers-a-fact-already-here (with-import-runtime (home box :hermes))
  (write-temp-file nle::*shared-config-path*
                   "{\"providers\": {\"subrouter-1\": {\"sdk\": \"anthropic\"}},
                     \"general\": {\"default_model\": {\"provider\": \"x\", \"model\": \"y\"}},
                     \"mcp\": {\"servers\": {\"github\": {\"command\": \"mine\"}}},
                     \"channels\": {\"telegram\": {\"enabled\": true}}}")
  (import-write home "SOUL.md" "mine")
  (let ((plan (hermes-plan home)))
    (is-table (kind needle) (equal "conflict" (nik:item-status (import-item plan kind needle)))
      ("providers" "providers.subrouter-1") ("providers" "general.default_model")
      ("mcp" "github") ("channels" "channels.telegram"))
    ;; A persona here is the operator's voice, and a bot speaks in one.
    (is (equal "conflict" (nik:item-status (first (import-items plan "instructions")))))
    (is (search "--overwrite puts this one in its place"
                (nik:item-reason (first (import-items plan "instructions")))))
    (is (search "--overwrite" (nik:item-reason (import-item plan "mcp" "github")))))
  (let ((plan (hermes-plan home :overwrite t)))
    (is (equal "imported" (nik:item-status (import-item plan "mcp" "github"))))
    (is (search "in place of your own" (nik:item-reason (first (import-items plan "instructions")))))))

(deftest import-takes-a-running-gateway-over-by-default (with-import-runtime (home box :hermes))
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
  (let* ((plan (hermes-plan home))
         (stop (find "hermes-gateway.service" (import-items plan "channels")
                     :key #'nik:item-source :test #'equal)))
    (is stop "the takeover is an item of its own")
    (is (search "systemctl --user enable --now hermes-gateway.service"
                stop.reason))
    (is (eq stop (find-if (lambda (item) (equal "channels" item.kind)) (nik:plan-items plan))))
    (nik:apply-plan plan)
    (is (= 1 *import-stopped*) "the unit is stopped once")
    (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled"))))
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active")
        *import-stopped* 0)
  (nik:apply-plan (hermes-plan home :overwrite t :takeover nil))
  (is (zerop *import-stopped*) "--keep-running stops nothing")
  (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram")
                               "enabled" t)))
  (is (member "telegram" (nik::held-platforms (list :home home))
              :test #'equal)))

(deftest import-apply-over-the-wire-with-keep-running-stops-nothing (with-import-runtime (home box))
  ;; The first frame applies with keep_running set: a live foreign gateway is
  ;; never stopped or disabled, its bot lands off and held for the watch to hand
  ;; over, so one token is never polled by two gateways.
  (make-hermes-fixture box)
  (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
  (let ((value (nik::run-method "apply" (nlk:json-object "source" "hermes" "keep_running" t))))
    (is (zerop *import-stopped*) "no stop, so no disable after it")
    (is (equal "hermes-gateway.service" (gethash "takeover" value)) "it still names the unit that keeps running")
    (is (find "channels.telegram" (gethash "bots" value) :test #'equal) "and the bots it polls")
    (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram") "enabled" t)) "the bot is off while the other gateway polls its token")
    (is (member "telegram" (nik::held-platforms (list :home home)) :test #'equal))
    ;; The answer carries the report and nothing posts it: the wizard writes
    ;; it into its own transcript. Posted, it was a toast cut at four rows
    ;; (ip-01).
    (is (search "[providers]" (gethash "report" value)))
    (is (notany (lambda (entry) (search "[providers]" (first entry))) (nlk:notice-log)))))

(deftest import-holds-an-openclaw-bot-while-its-own-gateway-runs (with-import-runtime (home box))
  ;; Without the hold, an Openclaw home's Telegram bot would poll beside
  ;; `openclaw gateway', and Telegram answers two pollers on one token with 409.
  ;; The probe answers only for the unit it is asked about, as the box does.
  (make-openclaw-fixture box)
  (let ((nik::*running-probe*
          (lambda (units)
            (and (member "openclaw-gateway.service" units :test #'equal) *import-running*))))
    (setf *import-running* (list "pid 4242: openclaw gateway --port 18789"))
    (let ((plan (nik:make-plan :source "openclaw" :takeover nil)))
      (nik:apply-plan plan)
      (is (zerop *import-stopped*))
      ;; Off while the other gateway polls its token, held so it comes on once
      ;; that gateway stops, and the report names the process that holds it.
      (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram")
                                   "enabled" t)))
      (is (member "telegram" (nik::held-platforms (list :home home)) :test #'equal))
      (is (search "another gateway: running — pid 4242: openclaw gateway --port 18789"
                  (nik::report-text plan))))
    ;; `nodecode import openclaw' with its unit up takes the bot over, as it does Hermes's.
    (setf *import-running* (list "systemd user unit openclaw-gateway.service is active"))
    (let ((plan (nik:make-plan :source "openclaw" :overwrite t)))
      (is (equal "openclaw-gateway.service" (gethash "takeover" (nik::plan-json plan))))
      (nik:apply-plan plan)
      (is (= 1 *import-stopped*) "the unit is stopped once")
      (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled")))
      (is (null (nik::held-platforms (list :home home)))))))

(deftest import-hands-a-held-bot-over-once-the-other-gateway-stops (with-import-runtime (home box))
  (let ((settings (list :home home)))
    (write-temp-file nle::*shared-config-path*
                     "{\"channels\": {\"telegram\": {\"enabled\": false, \"allowed_users\": [\"1\"]}}}")
    (nik::write-held settings '("telegram"))
    (setf *import-running* (list "systemd user unit hermes-gateway.service is active"))
    (is (eq :waiting (nik::watch-step settings)) "while it runs, nothing moves")
    (setf *import-running* '())
    (is (eq :gone (nik::watch-step settings)) "one look with it gone is not a stop")
    (is (eq :handed (nik::watch-step settings)) "two in a row is")
    (is (eq t (nlk:json-value (import-config) :any "channels" "telegram" "enabled")))
    (is (null (nik::held-platforms settings)) "and the held record is spent")
    (is (eq :idle (nik::watch-step settings)) "nothing held, nothing to do")))

(deftest import-knows-a-gateway-by-its-command-not-a-word-in-it ()
  ;; xh-202: any command line carrying the program's name and `gateway' held a
  ;; bot off, so a terminal tailing the unit's log was the gateway. The command
  ;; decides: the title the gateway gives itself, or the program -- itself, the
  ;; script, module or code an interpreter runs, or the name the kernel keeps
  ;; -- whose first word is its verb. A string is a line as `pgrep' joins it,
  ;; a list the words /proc holds.
  (let* ((code (concatenate 'string "import os, sys, runpy; os.environ.pop('PYTHONHOME', None); "
                            "sys.path.insert(0, '/home/op/.hermes/hermes-agent'); import hermes_bootstrap; "
                            "runpy.run_module('hermes_cli.main', run_name='__main__', alter_sys=True)"))
         ;; Hermes' store launcher (hermes_cli/_launchers.py runtime_command).
         (store (list "/home/op/.hermes/store/bin/python3" "-I" "-c" code "gateway" "run" "--replace"))
         (chat (list "/home/op/.hermes/store/bin/python3" "-I" "-c" code "chat" "-q" "is the gateway up")))
    (flet ((gateway-p (argv unit &optional comm)
             (nik::gateway-command-p argv (format nil "~a-gateway.service" unit) comm)))
      (dolist (argv '("openclaw gateway --port 18789"
                      "openclaw --profile work gateway run"
                      "node /usr/local/bin/openclaw gateway --port 18789"
                      "/usr/bin/node --max-old-space-size=4096 /usr/lib/node_modules/openclaw/dist/index.js gateway"
                      "openclaw-gateway"))
        (is (gateway-p argv "openclaw") argv))
      ;; The title, as the 15 characters of it the kernel keeps.
      (is (gateway-p '("openclaw") "openclaw" "openclaw-gatewa"))
      (dolist (argv (list "/home/op/.hermes/hermes-agent/venv/bin/python -m hermes_cli.main gateway run"
                          "python -X dev -m hermes_cli.main gateway run"
                          "python3 /home/op/.local/bin/hermes gateway run --replace"
                          "hermes -p work gateway"
                          "python3 /opt/hermes-agent/gateway/run.py"
                          "hermes-gateway"
                          store
                          (format nil "~{~a~^ ~}" store)))
        (is (gateway-p argv "hermes") argv))
      ;; The program by the name it gives itself, whatever code launched it.
      (is (gateway-p (list "/usr/bin/python3" "-I" "-c" "exec(a_launcher_this_reader_does_not_know)" "gateway" "run")
                     "hermes" "hermes"))
      ;; Each as if the kernel named it `hermes', as a Hermes chat is named: the name alone is no gateway.
      (dolist (argv (list "journalctl --user -fu openclaw-gateway"
                          "journalctl --user -fu hermes-gateway.service"
                          "tail -f /home/op/.openclaw/logs/gateway.log"
                          "vim /home/op/.hermes/gateway.json"
                          "bash drive.sh openclaw gateway"
                          "openclaw logs --follow"
                          "openclaw gateway status"
                          "hermes chat -q restart the gateway please"
                          "/home/op/.hermes/hermes-agent/venv/bin/python -m hermes_cli.main chat"
                          chat
                          (format nil "~{~a~^ ~}" chat)))
        (is (notany (lambda (unit) (gateway-p argv unit "hermes")) '("openclaw" "hermes"))
            argv)))))

(defun import-report-words (home)
  "The words that say the report is a file under HOME's import folder."
  (format nil "the report is in ~a/report-"
          (nlk:home-abbreviated (namestring (merge-pathnames "import/" home)))))

(deftest import-slash-previews-then-applies (with-import-runtime (home box :commandcode))
  (let ((line (nik::run-slash "commandcode")))
    (is-carrying line ("(dry run)" "a bare /import previews") "--yes applies")
    (is (null (probe-file (merge-pathnames "auth.json" home))) "and writes nothing")
    ;; The notice is the summary and where the report is, never the report
    ;; cut at a toast's four rows, and the hint that stays says where too,
    ;; the home written `~' (ip-01).
    (is-carrying (text (first (first (nlk:notice-log))))
      "(dry run)" (:absent "[providers]")
      (is (search (import-report-words home) text) text))
    (is (search (import-report-words home) line) line))
  (let ((line (nik::run-slash "commandcode --yes")))
    (is (search "imported" line))
    (is (equal +fixture-key+ (nlk:json-value (import-auth) :text "api_keys" "commandcode" "key")))
    (is-carrying (text (first (first (nlk:notice-log))))
      " imported · " (:absent "(dry run)")
      (is (search (import-report-words home) text) text))
    (is (search (import-report-words home) line) line))
  (is (search "not a world this build knows"
              (princ-to-string (signals-error nik:import-error (nik::run-slash "nosuchworld"))))))

(defmacro with-complete-stub ((&body stub) &body body)
  `(with-stubbed-fdefinitions ((nle:complete (system user &key session-id max-tokens fallback-p)
                                ,@stub))
     ,@body))

(deftest import-verify-asks-the-target-once-and-keeps-the-key-out-of-the-refusal ()
  (with-complete-stub ((is (null fallback-p) "no fallback: the imported target alone")
                       "ok")
    (multiple-value-bind (ok reason) (nik::verify-target)
      (is (eq t ok) "an answer is an answer")
      (is (null reason))))
  ;; A silent answer is an answer: the provider returns an empty assistant
  ;; message. Without this, the empty response would be retried while the
  ;; walk waits.
  (with-complete-stub ((is (eql 0 nle::*max-stream-retries*) "one attempt")
                       (error 'nle::provider-empty-response
                              :detail "provider returned an empty assistant message"))
    (is (eq t (nik::verify-target)) "a silent answer is an answer"))
  ;; The key the check sent is out of the words, whole and in part; a model
  ;; id is not a key, and stays.
  (let ((nle::*api-key* "FAKEnotakey0123456789abcdef"))
    (with-complete-stub ((error 'nle::provider-error
                                :status 401 :provider "openrouter" :model "anthropic/claude-sonnet-4.5"
                                :detail "{\"error\":{\"message\":\"Incorrect API key provided: FAKEnotakey0123456789abcdef for anthropic/claude-sonnet-4.5\"}}"))
      (multiple-value-bind (ok reason code status) (nik::verify-target)
        (is (null ok) "a refusal is a refusal")
        (is (equal "auth" code))
        (is (eql 401 status))
        (is-carrying (text reason)
          "Incorrect API key provided: [redacted]" "anthropic/claude-sonnet-4.5"
          (:absent "FAKEnotakey") (:absent "provider request failed") (:absent "/connect"))))
    (with-complete-stub ((error "rejected FAKEnotakey012 (claude-opus-5-5)"))
      (is-carrying (text (nth-value 1 (nik::verify-target)))
        "rejected [redacted] (claude-opus-5-5)" (:absent "FAKEnotakey"))))
  ;; A !command's key is what it prints, which its text does not hold:
  ;; without this, a refusal quoting the key the check sends would keep
  ;; it.
  (let ((nle::*api-key* "!printf %s%s KEYhead0 tail0123456789"))
    (with-complete-stub ((error 'nle::provider-error
                                :status 401
                                :detail "{\"error\":{\"message\":\"Invalid key KEYhead0tail0123456789\"}}"))
      (is-carrying (text (nth-value 1 (nik::verify-target)))
        "Invalid key [redacted]" (:absent "KEYhead0tail") (:absent "tail0123456789"))))
  (with-complete-stub ((error 'nle::provider-connection-refused
                              :detail "nothing listens at 127.0.0.1:1 — is the server running?"))
    (multiple-value-bind (ok reason code) (nik::verify-target)
      (is (null ok))
      (is (search "nothing listens at 127.0.0.1:1" reason))
      (is (equal "connection_refused" code) "a refusal of the port is named by its class, not its words"))))

(deftest import-scan-answers-the-pasteable-report (with-import-runtime (home box :hermes))
  (let ((value (nik::run-method "scan" (nlk:json-object "source" "hermes"))))
    (is (hash-table-p value))
    (is (plusp (gethash "imported" (gethash "summary" value))))
    (is (equalp #("hermes") (gethash "worlds" value)))
    (is (not (search +fixture-key+ (gethash "report" value))))
    (is (vectorp (gethash "unmapped" value)))))

(deftest import-never-repoints-a-built-in-provider (with-import-runtime (home box :qwen))
  ;; Qwen Code lists every OpenAI-compatible endpoint under
  ;; modelProviders.openai: the wire's name, not a vendor's. Read as a
  ;; provider it repointed the built-in OpenAI at the first endpoint it met
  ;; and filed the endpoint's key under the variable's name. Each endpoint
  ;; is a provider of its own, with its key, and OpenAI is left as it was.
  (let ((plan (nik:make-plan :source "qwen")))
    (dolist (item (import-items plan "providers"))
      (is (not (search "openai" (or (nik:item-destination item) ""))) "nothing is written for OpenAI"))
    (nik:apply-plan plan)
    (is (null (nth-value 1 (nlk:json-value (import-config) :any "providers" "openai"))))
    (is-shape (import-config)
      ((:text "providers" "dashscope-aliyuncs" "base_url") "https://dashscope.aliyuncs.com/compatible-mode/v1")
      ((:text "providers" "dashscope-aliyuncs" "sdk") "openai-completions")
      ((:text "providers" "localhost-11434" "base_url") "http://localhost:11434/v1"))
    (let ((auth (import-auth)))
      (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "dashscope-aliyuncs" "key")))
      (is (equal "ollama-local-1" (nlk:json-value auth :text "api_keys" "localhost-11434" "key")))
      (dolist (id '("openai" "dashscope" "ollama"))
        (is (null (nth-value 1 (nlk:json-value auth :any "api_keys" id)))
            (format nil "no key is filed under ~a" id))))))

(deftest import-gives-a-key-to-the-endpoint-that-names-its-variable (with-import-runtime (home box))
  ;; OPENAI_API_KEY is OpenAI's variable, and a Qwen home that names it for
  ;; another endpoint means that endpoint's key: it lands there, and OpenAI,
  ;; which the home never gave a key, keeps none.
  (make-qwen-fixture box :key-name "OPENAI_API_KEY")
  (nik:apply-plan (nik:make-plan :source "qwen"))
  (let ((auth (import-auth)))
    (is (equal +fixture-key+ (nlk:json-value auth :text "api_keys" "dashscope-aliyuncs" "key")))
    (is (null (nth-value 1 (nlk:json-value auth :any "api_keys" "openai"))))))

(deftest import-follows-a-moved-endpoint-with-the-model-that-named-it (with-import-runtime (home box))
  ;; A home that points its `openai' at a proxy and picks `openai/gpt-5.5'
  ;; means the proxy: the endpoint lands under its own id and the pick goes
  ;; with it, so the default model is the one that worked there.
  (import-write box ".config/opencode/opencode.json"
                (format nil "{\"model\": \"openai/gpt-5.5\", \"provider\": {\"openai\": {\"options\": {\"baseURL\": \"https://llm.corp.example.test/v1\", \"apiKey\": \"~a\"}}}}"
                        +fixture-key+))
  (let ((plan (nik:make-plan :source "opencode")))
    (is (search "gpt-5.5 on llm-corp-example"
                (nik:item-reason (import-item plan "providers" "general.default_model"))))
    (is (import-item plan "providers" "providers.llm-corp-example"))
    (is (null (import-item plan "providers" "providers.openai")))))

(deftest import-files-an-endpoint-named-by-its-key-under-its-host (with-import-runtime (home box))
  ;; An OpenCode home named a provider by its own key, and the key landed as
  ;; a provider id the lists showed. The endpoint lands under its
  ;; host's words with its key and the model that named it, and no line the
  ;; plan says carries the key.
  (let ((named "sk-sel-0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f"))
    (import-write box ".config/opencode/opencode.json"
                  (format nil "{\"model\": \"~a/kimi-k2.7-code\", \"provider\": {\"~a\": {\"npm\": \"@ai-sdk/openai-compatible\", \"models\": {\"kimi-k2.7-code\": {}}, \"options\": {\"baseURL\": \"https://api.select.example.test/v1\", \"apiKey\": \"~a\"}}}}"
                          named named named))
    (let ((plan (nik:make-plan :source "opencode")))
      (is (not (search named (with-output-to-string (out) (shasht:write-json (nik:plan-json plan) out)))) "the plan never says the key")
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:text "providers" "select-example" "base_url") "https://api.select.example.test/v1")
        ((:text "general" "default_model" "provider") "select-example"))
      (is (null (nth-value 1 (nlk:json-value (import-config) :any "providers" named))))
      (let ((auth (import-auth)))
        (is (equal named (nlk:json-value auth :text "api_keys" "select-example" "key")))
        (is (null (nth-value 1 (nlk:json-value auth :any "api_keys" named))) "no key is filed under itself")))))

(deftest import-places-a-bare-model-on-the-one-provider-that-serves-it (with-import-runtime (home box))
  ;; am-04: Claude Code writes `opus' beside the Anthropic key it keeps, Codex
  ;; `gpt-5.6' beside its OpenAI key. Each was dropped as naming no provider,
  ;; `which no provider here can reach', under the very line that landed the key.
  (flet ((pick (world)
           (import-item (nik:make-plan :source world :only "providers")
                        "providers" "general.default_model")))
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opus\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
    ;; A line of models by its word: the newest the catalog lists, and the report says what it read.
    (is (equal "claude-opus-5-5 on anthropic, for Claude Code's opus" (nik:item-reason (pick "claude"))))
    (import-write box ".codex/config.toml" (format nil "model = \"gpt-5.6\"~%"))
    (import-write box ".codex/auth.json" (format nil "{\"OPENAI_API_KEY\": \"~a\"}" +fixture-key-2+))
    (let ((plan (nik:make-plan :source "codex" :only "providers")))
      (is (equal "gpt-5.6 on openai, the one imported provider that serves it"
                 (nik:item-reason (import-item plan "providers" "general.default_model"))))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:text "general" "default_model" "provider") "openai")
        ((:text "general" "default_model" "model") "gpt-5.6")))
    ;; What no imported key serves is said so, never as a provider it names.
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opusplan\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
    (let ((item (pick "claude")))
      (is (equal "skipped" (nik:item-status item)))
      (is (equal "Claude Code's opusplan names no provider, and no key imported here serves it: pick a model in /models"
                 (nik:item-reason item))))
    ;; Two imported providers that both serve it: neither is guessed.
    (import-write box ".claude/settings.json"
                  (format nil "{\"model\": \"opus\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\", \"OPENROUTER_API_KEY\": \"~a\"}}"
                          +fixture-key+ +fixture-key-2+))
    (is (search "each serve it: pick one in /models" (nik:item-reason (pick "claude"))))))

;;; --- the model a home ran ---------------------------------------------------

(deftest import-tally-scores-a-day-of-work-halved-every-fortnight ()
  ;; A model scores its share of each day's tokens, halved for every 14 days
  ;; back: four fifths of today and all of yesterday, against a fifth of
  ;; today and all of a day a fortnight back. A session of no tokens and a
  ;; day past the window count nothing, and the effort is the one most of a
  ;; model's tokens ran at.
  (let* ((now 1792000000) (day 86400) (nik::*tally-now* now)
         (scores (nik::tally-scores
                  (list (list now "p" "a" 400 "high") (list now "p" "b" 100 "low")
                        (list (- now (* 14 day)) "p" "b" 50 "xhigh")
                        (list (- now day) "p" "a" 10 "xhigh")
                        (list now "p" "c" 0 nil)
                        (list (- now (* 91 day)) "p" "d" 1000 nil)))))
    (is (equal '("a" "b") (mapcar (lambda (entry) (getf entry :model)) scores)))
    (is (< (abs (- (+ 0.8d0 (expt 0.5d0 1/14)) (getf (first scores) :score))) 1d-9))
    (is (< (abs (- 0.7d0 (getf (second scores) :score))) 1d-9))
    (is (equal '(2 150 "low") (list (getf (second scores) :days) (getf (second scores) :tokens)
                                    (getf (second scores) :effort))))
    (is (equal "high" (getf (first scores) :effort)))))

(deftest import-sets-the-model-a-home-ran-on-a-key-that-comes-over (with-import-runtime (home box))
  ;; A Codex config named relay-small while its threads ran relay-large, and
  ;; Claude Code ran claude-opus-5-5 harder than both on a Max plan, a
  ;; sign-in that stays behind. The default is what ran, on a
  ;; key that came over, at the effort it ran at -- relay-large -- until
  ;; Claude Code keeps a key of its own; the model it passed over is said,
  ;; with the key here that serves it.
  (let* ((now (- (get-universal-time) nlk:+unix-epoch+))
         (nik::*tally-now* now)
         (last (subseq (nlk:iso-time (- now 86400)) 0 10)))
    (import-write box ".codex/config.toml"
                  (format nil "model = \"relay-small\"~%model_provider = \"relay\"~%~%[model_providers.relay]~%base_url = \"https://relay.example.test/v1\"~%key = \"~a\"~%"
                          +fixture-key-2+))
    (import-write box ".codex/auth.json" (format nil "{\"OPENROUTER_API_KEY\": \"~a\"}" +fixture-key+))
    (let ((db (sqlite:connect (namestring (merge-pathnames ".codex/state_5.sqlite" box)))))
      (nlk:with-cleanup ((sqlite:disconnect db))
        (sqlite:execute-non-query
         db "create table threads (id text primary key, model_provider text not null, model text, tokens_used integer not null default 0, updated_at integer not null, reasoning_effort text)")
        (loop for (id model tokens days effort) in '(("t1" "relay-large" 9000 1 "xhigh") ("t2" "relay-large" 7000 3 "high")
                                                     ("t3" "relay-small" 500 2 "low") ("t4" "relay-small" 0 0 nil))
              do (sqlite:execute-non-query db "insert into threads values (?, 'relay', ?, ?, ?, ?)"
                                           id model tokens (- now (* days 86400)) effort))))
    (import-write box ".claude.json" "{}")
    (import-write box ".claude/stats-cache.json"
                  (format nil "{\"dailyModelTokens\": [~{{\"date\": \"~a\", \"tokensByModel\": {\"claude-opus-5-5\": 900000000}}~^, ~}]}"
                          (loop for days below 3 collect (subseq (nlk:iso-time (- now (* days 86400))) 0 10))))
    (import-write box ".claude/settings.json" "{\"model\": \"opus\"}")
    (flet ((plan () (nik:make-plan :settings (list :home home) :only "providers"))
           (reasons (plan)
             (mapcar #'nik:item-reason
                     (remove "general.default_model" (import-items plan "providers")
                             :key #'nik:item-destination :test-not #'equal))))
      (let ((plan (plan)))
        (is (equal (list (format nil "relay-large on relay at xhigh effort, the model Codex ran most lately: 16k tokens over 2 days of the last 90, last on ~a" last)
                         "claude-opus-5-5 is what Claude Code ran most lately (2.7B tokens over 3 days), on a key that did not come over; openrouter as anthropic/claude-opus-5-5 serves it, billed per token: /models picks it")
                   (reasons plan)))
        (nik:apply-plan plan)
        (is-shape (import-config)
          ((:text "general" "default_model" "provider") "relay")
          ((:text "general" "default_model" "model") "relay-large")
          ((:text "general" "default_model" "effort") "xhigh"))
        ;; this organism fresh again, so the next plans set the default
        (write-temp-file nle::*shared-config-path* "{}")
        (setf nle::*shared-config-memo* nil)
        (uiop:delete-file-if-exists nle::*auth-file-path*))
      ;; A plugin's template is no key at all.
      (import-write box ".claude/plugins/kit/.env.example" "ANTHROPIC_API_KEY=sk-ant-FAKE-not-a-key-example")
      (is (search "relay-large on relay" (first (reasons (plan)))))
      (import-write box ".claude/settings.json"
                    (format nil "{\"model\": \"opus\", \"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
      (let ((said (reasons (plan))))
        (is (search "claude-opus-5-5 on anthropic, the model Claude Code ran most lately: 2.7B tokens over 3 days" (first said)))
        (is (null (rest said)) "nothing was passed over"))
      ;; Signed in as well, the ledger is the sign-in's: it stays behind.
      (import-write box ".claude/.credentials.json"
                    (format nil "{\"claudeAiOauth\": {\"accessToken\": \"~a\", \"refreshToken\": \"~a\", \"expiresAt\": 1}}"
                            +fixture-token+ +fixture-refresh+))
      (let ((said (reasons (plan))))
        (is (search "relay-large on relay" (first said)))
        (is (search "on a sign-in that stays there" (second said)))))))

(deftest import-finds-a-key-its-endpoint-by-the-domain-its-name-says (with-import-runtime (home box))
  ;; Without this, a backup key would land as a provider with nowhere to
  ;; send a request. A backup is left alone; a live key the home names no
  ;; endpoint for takes the one another home dials on the domain its name
  ;; says, and when homes dial that domain different ways, none is guessed.
  (import-write box ".hermes/config.yaml" (format nil "toolsets: []~%"))
  (import-write box ".hermes/.env" (format nil "ACMEROUTER_SMART_API_KEY=~a~%" +fixture-key+))
  (import-write box ".hermes/.env.bak-0612" (format nil "ACMEROUTER_OLD_API_KEY=~a~%" +fixture-key-2+))
  (import-write box ".hermes/.env.pre-email-disable" (format nil "ACMEROUTER_PRE_API_KEY=~a~%" +fixture-key-2+))
  (import-write box ".codex/config.toml"
                (format nil "[model_providers.cheap]~%base_url = \"https://api.acmerouter.example/v1\"~%key = \"~a\"~%"
                        +fixture-key-2+))
  (flet ((plan () (nik:make-plan :settings (list :home home) :only "providers")))
    (let ((plan (plan)))
      (is (null (import-item plan "providers" "acmerouter-old")) "a backup's key stays where it is")
      (is (null (import-item plan "providers" "acmerouter-pre")))
      (is-present (item (import-item plan "providers" "providers.acmerouter-smart")) "the live key's endpoint"
        (is (equal "imported" (nik:item-status item)))
        (is (search "endpoint https://api.acmerouter.example/v1 from Codex's cheap, on the domain ACMEROUTER_SMART_API_KEY names"
                    (nik:item-reason item))))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:text "providers" "acmerouter-smart" "base_url") "https://api.acmerouter.example/v1")
        ((:text "providers" "acmerouter-smart" "sdk") "openai-completions")))
    (write-temp-file nle::*shared-config-path* "{}")
    (setf nle::*shared-config-memo* nil)
    ;; Dialed another way as well: both named, neither taken.
    (import-write box ".pi/agent/settings.json" "{}")
    (import-write box ".pi/agent/models.json"
                  (format nil "{\"providers\": {\"kiro\": {\"baseUrl\": \"https://acmerouter.example/anthropic\", \"apiKey\": \"~a\"}}}"
                          +fixture-key-2+))
    (is-present (item (import-item (plan) "providers" "providers.acmerouter-smart")) "the key's entry"
      (is (equal "skipped" (nik:item-status item)))
      (is-carrying (text (nik:item-reason item))
        "Codex's cheap (https://api.acmerouter.example/v1, openai-completions)"
        "pi's kiro (https://acmerouter.example/anthropic, anthropic)"
        "dial its domain different ways" "add providers.acmerouter-smart.base_url"))))

(deftest import-keeps-one-bot-per-token (with-import-runtime (home box :hermes))
  ;; Two harnesses' Telegram bots are two bots when their tokens differ, and
  ;; one section holds one: the newest home's is kept, the other is named and
  ;; left out, and an allowlist or an owner of one is never the other's.
  (make-openclaw-fixture box :token "FAKE-not-a-token-openclaw-987654"
                             :discord "FAKE-not-a-token-openclaw-discord")
  (flet ((items (plan platform)
           (remove-if-not (lambda (item) (equal (format nil "channels.~a" platform) (nik:item-destination item)))
                          (import-items plan "channels"))))
    (let ((plan (nik:make-plan :only "channels")))
      (dolist (platform '("telegram" "discord"))
        (is (equal '("conflict" "imported") (sort (mapcar #'nik:item-status (items plan platform)) #'string<))
          (format nil "one ~a section, one bot named and left out" platform)))
      (is (find "hermes and openclaw both have a Telegram bot; kept hermes, left openclaw's out"
                (items plan "telegram") :key #'nik:item-reason :test #'equal))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone"))
        ((:any "channels" "telegram" "owner") equalp #("8071918233" "someone"))
        ((:any "channels" "discord" "allowed_users") equalp #("42"))
        ((:any "channels" "discord" "owner") equalp #("42")))
      (is (equal +fixture-token+
                 (string-trim '(#\Newline)
                              (uiop:read-file-string (merge-pathnames "secrets/channels.telegram.token_file" home)))) "the token is the kept bot's")
      (is (not (search "9001" (uiop:read-file-string nle::*shared-config-path*))) "and nobody from the other bot is on it"))
    ;; A section already here outranks both, and neither is named as kept.
    (write-temp-file nle::*shared-config-path* "{\"channels\": {\"telegram\": {\"enabled\": true}}}")
    (let ((plan (nik:make-plan :only "channels")))
      (is (equal '("conflict" "conflict") (mapcar #'nik:item-status (items plan "telegram"))))
      (is (every (lambda (item) (search "section already here" (nik:item-reason item)))
                 (items plan "telegram"))))))

(deftest import-merges-a-bot-two-homes-share (with-import-runtime (home box :hermes :openclaw))
  ;; The same token is the same bot, whichever harness holds it: one section,
  ;; and its allowlist and owners are the union of both homes' -- each
  ;; already trusted those people with that very bot.
  (let ((plan (nik:make-plan :only "channels")))
    (is (equal '("imported")
               (mapcar #'nik:item-status
                       (remove-if-not (lambda (item) (equal "channels.telegram" (nik:item-destination item)))
                                      (import-items plan "channels")))))
    (nik:apply-plan plan)
    (is-shape (import-config)
      ((:any "channels" "telegram" "allowed_users") equalp #("8071918233" "someone" "9001"))
      ((:any "channels" "telegram" "owner") equalp #("8071918233" "someone" "9001")))))

(deftest import-reads-an-openclaw-bot-as-its-docs-write-it (with-import-runtime (home box))
  ;; xn-201: Openclaw keeps a bot under channels.<platform>, its token in the
  ;; file `tokenFile' names (which wins) or inline, and who may talk as
  ;; `allowFrom' (direct messages) and `groupAllowFrom' (senders in the groups
  ;; it lists), user ids a `tg:' may lead (docs/channels/telegram/access-control.md).
  ;; Only `allowedUsers' was read, so the bot was dropped as having no allowlist.
  (flet ((plan-item (telegram)
           ;; TELEGRAM is the members after the bot token in openclaw.json's telegram section.
           (import-write box ".openclaw/openclaw.json"
                         (format nil "{\"channels\": {\"telegram\": {\"botToken\": \"~a\", ~a}}}"
                                 +fixture-token+ telegram))
           (import-item (nik:make-plan :source "openclaw" :only "channels" :overwrite t)
                        "channels" "channels.telegram")))
    (import-write box ".openclaw/credentials/telegram.token" (format nil "~a~%" +fixture-token+))
    (import-write box ".openclaw/openclaw.json"
                  "{\"channels\": {\"telegram\": {\"enabled\": true, \"dmPolicy\": \"allowlist\",
  \"botToken\": \"123:inline\", \"tokenFile\": \"~/.openclaw/credentials/telegram.token\",
  \"allowFrom\": [\"tg:9001\", \"accessGroup:operators\"], \"groupAllowFrom\": [\"9003\", \"9001\"]}}}")
    (let ((plan (nik:make-plan :source "openclaw" :only "channels")))
      ;; A sender Openclaw lets in only in a group is not let in here, where a listed user talks anywhere.
      (is (search "1 user, 0 chats, token in a 0600 file; groupAllowFrom left out"
                  (nik:item-reason (import-item plan "channels" "channels.telegram"))))
      (nik:apply-plan plan)
      (is-shape (import-config)
        ((:any "channels" "telegram" "allowed_users") equalp #("9001"))
        ((:any "channels" "telegram" "owner") equalp #("9001"))
        ((:any "channels" "telegram" "enabled") eq t))
      ;; The file's token, not the inline one, lands where an inline one does: a 0600 file of this home's.
      (is (equal +fixture-token+
                 (nlk:trimmed (uiop:read-file-string
                               (merge-pathnames "secrets/channels.telegram.token_file" home))))))
    ;; `*' lets anybody in, which is no allowlist: still refused.
    (let ((item (plan-item "\"dmPolicy\": \"open\", \"allowFrom\": [\"*\"]")))
      (is (equal "error" (nik:item-status item)))
      (is (search "no allowlist in the home" (nik:item-reason item))))
    ;; With direct messages off there, allowFrom names only group senders: no allowlist either.
    (let ((item (plan-item "\"dmPolicy\": \"disabled\", \"allowFrom\": [\"9001\"]")))
      (is (equal "error" (nik:item-status item)))
      (is (search "allowFrom left out" (nik:item-reason item))))
    ;; A token file that does not read, a link among them, gives no token, and its path is named.
    (sb-posix:symlink (namestring (merge-pathnames ".openclaw/credentials/telegram.token" box))
                      (namestring (merge-pathnames ".openclaw/credentials/linked.token" box)))
    (dolist (path '("~/.openclaw/credentials/missing.token" "~/.openclaw/credentials/linked.token"))
      (let ((item (plan-item (format nil "\"tokenFile\": \"~a\", \"allowFrom\": [\"9001\"]" path))))
        (is (equal "skipped" (nik:item-status item)) path)
        (is (search (file-namestring path) (nik:item-reason item)) path))))
  ;; A bot switched off there lands off, and is not held for the watch to turn on.
  (import-write box ".openclaw/openclaw.json"
                (format nil "{\"channels\": {\"telegram\": {\"enabled\": false, \"botToken\": \"~a\", \"allowFrom\": [\"9001\"]}}}"
                        +fixture-token+))
  (let* ((nik::*running-probe* (lambda (units) (declare (ignore units)) (list "pid 4242: openclaw gateway")))
         (plan (nik:make-plan :source "openclaw" :only "channels" :overwrite t :takeover nil)))
    (is (search "off in openclaw, so it lands off"
                (nik:item-reason (import-item plan "channels" "channels.telegram"))))
    (nik:apply-plan plan)
    (is (not (nlk:config-boolean (nlk:json-value (import-config) :any "channels" "telegram") "enabled" t)))
    (is (null (nik::held-platforms (list :home home))))))
;;; --- memories and skills: definitions -----------------------------------------------

(deftest import-nodecode-world-converts-both-carriers-and-seeds-the-ledger (with-import-runtime (home box))
  (make-nodecode-fixture box)
  (let ((plan (nik:make-plan :source "nodecode")))
    (is (equal '("conflict" "imported" "imported" "imported")
               (sort (copy-list (import-statuses plan "memory")) #'string<)) "a second memory of one name is named")
    (is (search "a memory named quiet-output came from quiet-output.md first"
                (nik:item-reason (find "conflict" (import-items plan "memory") :key #'nik:item-status :test #'equal))))
    (is (equal '("imported") (import-statuses plan "skills")) "neither the archived skill nor a proposed one")
    (is (equal "Split red tests from the baseline. (not carried: scripts/run.sh (7 bytes))"
               (nik:item-reason (import-item plan "skills" "red-baseline"))) "a skill's line is its description")
    (is (equal '("imported") (import-statuses plan "usage")))
    (nik:apply-plan plan))
  (is-present (memory (kept-text "quiet-output")) "a memory is a define-memory"
    (is-carrying memory "(define-memory quiet-output"
      ("\"The operator wants quiet output, said \\\"twice\\\".\"" "the description escaped")
      ":type :feedback" ":sources (\"s-ONE\" \"s-TWO\")"
      ";; created 2026-09-01T10:00:00.000Z · updated 2026-09-20T10:00:00.000Z"
      ";; Said on 2026-09-01; a log line counts as noise."))
  (is (search ":project \"/tmp/proj\"" (kept-text "repo-builds")) "a project's memory keeps its root")
  (is-present (skill (kept-text "red-baseline")) "a skill is a prose define-skill"
    (is-carrying skill ";; origin: reflection" ";; --- references/notes.md ---"
      ";; Read the FAIL lines." "support files not carried: scripts/run.sh (7 bytes)"))
  (is (null (kept-text "old")))
  (is (null (kept-text "maybe")))
  (flet ((uses (name)
           (loop for line in (nlk:knowledge-ledger-lines)
                 when (equal name (nlk:json-value line :string "name"))
                   collect (list (nlk:json-value line :string "kind") (nlk:json-value line :string "at")))))
    (is (find '("keep" "2026-09-20T10:00:00.000Z") (uses "quiet-output") :test #'equal) "kept at its own date")
    (is (find "view" (uses "quiet-output") :key #'first :test #'equal) "listed hot, so viewed now")
    (is (find '("helped" "2026-09-22T10:00:00.000Z") (uses "quiet-output") :test #'equal) "and its sighting")
    (is (not (find "view" (uses "cold-fact") :key #'first :test #'equal)) "a cold one is not viewed")
    (is (find '("view" "2026-09-23T10:00:00.000Z") (uses "red-baseline") :test #'equal) "the library's ledger")
    (is (not (find "keep" (uses "red-baseline") :key #'first :test #'equal)) "a skill the ledger knows is not kept again"))
  (is (equal '("skipped") (import-statuses (nik:make-plan :source "nodecode" :only "usage") "usage")) "the ledger comes over once")
  (is (equal "skipped" (nik:item-status (import-item (nik:make-plan :source "nodecode" :only "memory") "memory" "quiet-output")))))

(deftest import-nodecode-world-unpins-the-retired-indexes (with-import-runtime (home box :nodecode))
  ;; Every session the retired cells pinned keeps its rows until they are taken
  ;; out, teaching verbs that are gone beside the index that replaced them.
  (with-temp-store ()
    (nlk:create-session :id "s-old" :cwd "/tmp")
    (nlk:set-harness-section "s-old" "memory-index" "(memory:read \"name\") opens one.")
    (nlk:set-harness-section "s-old" "skills-index" "(skills:view \"name\") before following one.")
    (nlk:set-harness-section "s-old" "soul" "be kind")
    (let ((plan (nik:make-plan :source "nodecode" :only "standing")))
      (is (equal "2 pins of memory-index and skills-index on 1 session, unpinned"
                 (nik:item-reason (first (import-items plan "standing")))))
      (nik:apply-plan plan))
    (is (equal '(("soul" . "be kind")) (nlk:list-harness-sections "s-old")) "the retired pins go, the rest stay")
    (is (equal '("skipped") (import-statuses (nik:make-plan :source "nodecode" :only "standing") "standing")))))

(deftest import-claude-memory-reads-frontmatter-and-resolves-its-project (with-import-runtime (home box))
  (import-write box ".claude.json" "{\"projects\": {\"/tmp/proj.x\": {}}}")
  (import-write box ".claude/settings.json" "{}")
  (import-write box ".claude/projects/-tmp-proj-x/memory/MEMORY.md"
                (format nil "- [grok-reasons](grok-reasons.md) — grok bills reasoning~%"))
  (import-write box ".claude/projects/-tmp-proj-x/memory/grok-reasons.md"
                (format nil "---~%name: grok-reasons~%description: grok bills reasoning it never shows~%metadata:~%  type: reference~%---~%~%Seen on both lanes.~%"))
  (let ((facts (home-facts-of "claude")))
    (is-present (value (nik::fact-value (find "grok-reasons" (remove :memory facts :key #'nik:fact-kind :test-not #'eq)
                                              :key #'nik:fact-id :test #'equal))) "one fact"
      (is (equal "reference" (getf value :type)))
      (is (equal "/tmp/proj.x" (getf value :project)) "its folder resolves through ~/.claude.json")
      (is (equal "grok bills reasoning it never shows" (getf value :description))))
    (is (equal '("grok-reasons") (fact-ids facts :hot)) "its MEMORY.md is the hot index")))

(deftest import-definition-text-reads-back-whole (with-import-runtime (home box))
  (let ((description "A \"quoted\" path C:\\x; not a comment")
        (lines (list "first (with parens)" "" "a ;; second comment" "\"quoted\" too")))
    (multiple-value-bind (text bare)
        (nik::definition-text "define-memory" "kn-round" description lines
                              :type :lesson :sources '("s-1"))
      (flet ((read-in-evolved (text)
               (let ((*package* (find-package '#:nodecode.evolved)) (*read-eval* nil))
                 (read-from-string text))))
        (let ((form (read-in-evolved text)))
          (is (equal description (third form)) "the description reads back as written")
          (is (equal '(:type :lesson :sources ("s-1")) (cdddr form)) "the options, and the prose is comments")
          (is (equal form (read-in-evolved bare)) "without its prose, the same form")))
      ;; Kept as a member of its run's PROGN: the scribe files the member's own span, comments whole.
      (let ((item (nik::make-item :kind "memory" :status "imported" :destination "kn-round"
                                  :world "test" :keep (list text))))
        (nik::keep-definitions (list (cons item (nik::evaluate-definition text))))
        (is (equal "imported" (nik::item-status item))))
      (is (equal (format nil "~{~a~^~%~}" lines) (nlk::knowledge-prose (nlk:knowledge-entry-named "kn-round" nil)))))))

(deftest import-progress-follows-actions-and-persisted-keeps (with-import-runtime (home box))
  (let* ((written nil) (events '())
         (item (nik::make-item :kind "providers" :status "imported" :destination "fixture"
                               :action (lambda () (setf written t))))
         (bad (nik::make-item :kind "memory" :status "imported" :destination "bad"
                              :action (lambda () (error "fixture failure"))))
         (keep (nik::make-item :kind "memory" :status "imported" :destination "progress-note" :world "fixture"
                               :keep (list "(define-memory progress-note \"fixture\" :type :reference)" :seed nil)))
         (plan (nik::%make-plan :settings (list :home home) :items (list item keep bad))))
    (nik:apply-plan plan :progress
      (lambda (phase current)
        (when (eq current item)
          (is (eq written (equal phase "finished")) "completion follows the real writer"))
        (when (and (eq current keep) (equal phase "finished"))
          (is (probe-file (merge-pathnames "cells/tester-memory/progress-note.lisp" home)) "kept means persisted, not merely evaluated"))
        (push (list phase (nik:item-destination current) (nik:item-status current)) events)))
    (is (equal '(("importing" "fixture" "imported") ("finished" "fixture" "imported")
                 ("importing" "progress-note" "imported") ("finished" "progress-note" "imported")
                 ("importing" "bad" "imported") ("finished" "bad" "error"))
               (nreverse events)))
    (is (equal "error" (nik:item-status bad)))))

(deftest import-route-streams-actual-outcomes-before-the-terminal-report (with-import-runtime (home box))
  (import-write box ".claude/CLAUDE.md" "The fixture imports real instructions.")
  (let* ((request (nlk:encode-json-object (nlk:json-object "method" "apply" "source" "claude"
                                                       "only" "instructions" "keep_running" t "stream" t)))
         (reply (nik::import-route (list :content-length (length request) :content-type "application/json"
                                        :raw-body (make-string-input-stream request))))
         (events '()) (closed nil))
    (is (functionp reply))
    (funcall reply
             (lambda (response)
               (is (= 200 (first response)))
               (is (equal "text/event-stream" (getf (second response) :content-type)))
               (is (= 2 (length response)) "the Clack responder must return a writer")
               (lambda (chunk &key close)
                 (if close
                     (setf closed t)
                     (nlk:read-events (make-string-input-stream chunk)
                                      (lambda (data)
                                        (let ((event (nlk:decode-json data)))
                                          (when (equal "finished" (gethash "phase" event))
                                            (is (probe-file (merge-pathnames "AGENTS.md" home))))
                                          (push event events))
                                        nil))))))
    (setf events (nreverse events))
    (is closed)
    (is (equal '("item" "item" "result") (mapcar (lambda (event) (gethash "type" event)) events)))
    (is (equal '("importing" "finished") (mapcar (lambda (event) (gethash "phase" event)) (butlast events))))
    (is (equal "imported" (nlk:json-value (second events) :string "item" "status")))
    (is (eq t (nlk:json-value (car (last events)) :any "result" "applied")))))

(deftest a-claude-session-import-takes-the-conversation-not-its-subagents (with-import-runtime (home box))
  ;; T-349: Claude Code keeps a session's subagent transcripts under
  ;; <session>/subagents/, and every line of them names the PARENT's
  ;; sessionId and isSidechain: read as sessions, they took the parent's id,
  ;; collided, and crowded the budget. A main transcript can carry sidechain
  ;; lines inline too, and it opens with queue-operation lines that name no
  ;; folder: the conversation's cwd is on the first line that has one.
  (let* ((id "0a1b2c3d-1111-2222-3333-444455556666")
         (project ".claude/projects/-work-app/"))
    (flet ((line (role text &key sidechain queue)
             (nlk:encode-json-object
              (if queue
                  (nlk:json-object "type" "queue-operation" "operation" queue "sessionId" id)
                  (nlk:json-object "type" role "isSidechain" (if sidechain t :false)
                                   "cwd" "/work/app" "sessionId" id
                                   "timestamp" "2026-10-22T10:00:00Z"
                                   "message" (nlk:json-object "role" role "content" text)))))
           (transcript (&rest lines) (format nil "~{~a~%~}" lines)))
      (import-write box ".claude/settings.json" "{}")
      (import-write box (format nil "~a~a.jsonl" project id)
                    (transcript (line nil nil :queue "enqueue") (line nil nil :queue "dequeue")
                                (line "user" "fix the build")
                                (line "assistant" "fixed: a missing import")
                                (line "user" "SUBAGENT BRIEF" :sidechain t)
                                (line "assistant" "SUBAGENT ANSWER" :sidechain t)))
      (dolist (sub '("subagents/agent-a1.jsonl" "subagents/workflows/wf_1/agent-a2.jsonl"))
        (import-write box (format nil "~a~a/~a" project id sub)
                      (transcript (line "user" "SUBAGENT BRIEF" :sidechain t)
                                  (line "assistant" "SUBAGENT ANSWER" :sidechain t)))))
    (let ((plan (nik:make-plan :source "claude" :settings (list :home home) :only "sessions")))
      (is (equal '("imported") (import-statuses plan "sessions")) "one conversation, none of its subagents")
      (is (search (format nil "~a.jsonl" id) (nik:item-source (first (import-items plan "sessions"))))))
    (let* ((facts (nik::session-facts (nik::open-home "claude")))
           (record (getf (nik::fact-value (first facts)) :record)))
      (is (= 1 (length facts)))
      (is (equal "/work/app" (string-right-trim "/" (nik::record-cwd record))) "its own folder, not the launch's")
      (is (equal '("fix the build") (mapcar (lambda (exchange) (getf exchange :input)) (nik::record-exchanges record))) "the inline sidechain pair is no exchange of it"))))

(deftest a-streamed-import-pings-while-it-works (let ((writes '()) (closed nil) (lock (bt2:make-lock :name "t"))))
  ;; T-350: a plan of a large home read in silence past the shell's 30 s
  ;; read. The stream says it is working every *STREAM-PING-SECONDS* -- a
  ;; comment NLK:READ-EVENTS skips -- ends on the result, and writes nothing
  ;; once closed.
  (flet ((responder (status)
           (declare (ignore status))
           (lambda (text &key close)
             (bt2:with-lock-held (lock)
               (when closed (push :after-close writes))
               (if close (setf closed t) (push text writes))))))
    (let ((nik::*stream-ping-seconds* 0.05))
      (nik::stream-import #'responder
                          (lambda (progress)
                            (declare (ignore progress))
                            (sleep 0.4)
                            (nlk:json-object "summary" "read")))))
  (let ((writes (reverse writes)))
    (is closed)
    (is (not (member :after-close writes)) "nothing after the close")
    (is (<= 3 (count-if (lambda (text) (and (stringp text) (search ": working" text))) writes)) "pinged while it read")
    (is (search "\"type\":\"result\"" (car (last writes))) "the result closes it")
    (is (search "\"summary\":\"read\"" (car (last writes))))))
