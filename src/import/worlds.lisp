;;;; worlds.lisp --- where the other coding agents keep their homes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A table of the harnesses an operator may be arriving from, and a
;;;; stat-only probe over it. Nothing here reads a file, opens a socket or
;;;; writes anything: DETECT-AGENT-WORLDS is one PROBE-FILE per evidence
;;;; path and one FILE-WRITE-DATE per world found — about a millisecond
;;;; over the whole table — which is what makes it safe on a first frame,
;;;; before any decision has been made and before any folder has loaded.
;;;;
;;;; It is here rather than in the import folder on purpose. The first
;;;; frame has to decide whether there is a past to bring over BEFORE it
;;;; loads anything: a blank box must reach the cold wizard without paying
;;;; for an import folder's compile, and without ever seeing a word of
;;;; migration vocabulary. The readers stay in the folder; only the
;;;; question `is any of this here' lives in the core, and this is the one
;;;; authority for it — the folder reads this table rather than carrying a
;;;; second copy.
;;;;
;;;; The core already knows this much about the outside world:
;;;; *INSTRUCTIONS-FILE-NAMES* names AGENTS.md and CLAUDE.md. This is the
;;;; same kind of knowledge, written down once.
;;;;
;;;; A row is DATA. Adding a world is adding a row; it carries no function,
;;;; no reader and no special case, and the import folder's lint asserts
;;;; that it never will.

(in-package #:nodecode.kernel)

(define-record (agent-world (:copier nil) (:export :predicate :readers)
                            (:constructor make-agent-world
                                (&key name label roots evidence (last-used (first evidence))
                                      skip enter retire tally takeover-unit)))
  "One harness's home, as data."
  ;; NAME is what `/import <name>' says and what its marker is filed under;
  ;; LABEL what a report calls it; ROOTS the paths under $HOME it keeps,
  ;; directories and files both, the first one the one a report names;
  ;; EVIDENCE the paths whose presence says this world is on the box;
  ;; LAST-USED the path whose write time dates it, so the newest world wins a
  ;; tie -- the first evidence path unless a row names another; SKIP
  ;; directory names not worth entering, beyond the ones every world skips;
  ;; ENTER the hidden ones worth entering, which every world skips; RETIRE
  ;; the standing sections the world's carriers pinned on this organism's
  ;; own sessions, which a conversion unpins (only this organism's own
  ;; retired carriers ever pinned any); TALLY the paths, a name maybe wild,
  ;; where the world records which models it ran and how much (import
  ;; tally.lisp reads them by shape);
  ;; TAKEOVER-UNIT the systemd user unit that must stop before this organism
  ;; can answer on that harness's bots, which every world whose home can carry
  ;; a bot names: its program, the unit's name up to its first `-' or `.', is
  ;; also what a running gateway's command line is found by, unit or none.
  (name "" :type string)
  (label "" :type string)
  (roots '() :type list)
  (evidence '() :type list)
  (last-used nil)
  (skip '() :type list)
  (enter '() :type list)
  (retire '() :type list)
  (tally '() :type list)
  (takeover-unit nil))

(access (world agent-world))

(defparameter *agent-worlds*
  (list
   (make-agent-world :name "hermes" :label "Hermes Agent"
                     :roots '(".hermes/")
                     :evidence '(".hermes/config.yaml" ".hermes/state.db")
                     :last-used ".hermes/state.db"
                     :skip '("hermes-agent" "audio_cache" "image_cache" "hooks")
                     :tally '(".hermes/state.db")
                     :takeover-unit "hermes-gateway.service")
   (make-agent-world :name "codex" :label "Codex"
                     :roots '(".codex/")
                     :evidence '(".codex/config.toml" ".codex/auth.json")
                     :last-used ".codex/history.jsonl"
                     :skip '("attachments" "handoffs" "archive" "decisions")
                     :tally '(".codex/state_*.sqlite"))
   (make-agent-world :name "claude" :label "Claude Code"
                     :roots '(".claude/" ".claude.json")
                     :evidence '(".claude.json" ".claude/settings.json")
                     ;; subagents/: a session's subagent transcripts, its sidechains, never a conversation
                     :skip '("shell-snapshots" "statsig" "ide" "file-history" "todos" "subagents")
                     :tally '(".claude/stats-cache.json"))
   (make-agent-world :name "opencode" :label "OpenCode"
                     :roots '(".config/opencode/" ".local/share/opencode/")
                     :evidence '(".config/opencode/opencode.json"
                                 ".config/opencode/opencode.jsonc"
                                 ".local/share/opencode/auth.json")
                     :last-used ".local/share/opencode/opencode.db"
                     :skip '("repos" "node_modules")
                     :tally '(".local/share/opencode/opencode.db"))
   (make-agent-world :name "cursor" :label "Cursor CLI"
                     :roots '(".cursor/")
                     :evidence '(".cursor/cli-config.json" ".cursor/mcp.json")
                     :skip '("ai-tracking" "interpreter-notebooks"))
   (make-agent-world :name "gemini" :label "Gemini CLI"
                     :roots '(".gemini/")
                     :evidence '(".gemini/settings.json")
                     :last-used ".gemini/state.json"
                     :skip '("antigravity"))
   (make-agent-world :name "qwen" :label "Qwen Code"
                     :roots '(".qwen/")
                     :evidence '(".qwen/settings.json"))
   (make-agent-world :name "pi" :label "pi"
                     :roots '(".pi/agent/")
                     :evidence '(".pi/agent/settings.json" ".pi/agent/auth.json"))
   (make-agent-world :name "crush" :label "Crush"
                     :roots '(".crush/" ".config/crush/")
                     :evidence '(".crush/crush.db" ".config/crush/crush.json"))
   (make-agent-world :name "cline" :label "Cline"
                     :roots '(".cline/")
                     :evidence '(".cline/data/" ".cline/cron/")
                     :skip '("locks"))
   (make-agent-world :name "windsurf" :label "Windsurf"
                     :roots '(".codeium/windsurf/")
                     :evidence '(".codeium/windsurf/memories/")
                     :skip '("brain" "cascade" "code_tracker" "codemaps" "context_state"
                             "database" "implicit" "windsurf" "windsurf.bak"))
   (make-agent-world :name "kiro" :label "Kiro"
                     :roots '(".kiro/")
                     :evidence '(".kiro/settings/" ".kiro/agents/")
                     :skip '("powers"))
   (make-agent-world :name "dotagents" :label "dotagents"
                     :roots '(".agents/")
                     :evidence '(".agents/agents.md" ".agents/models.json")
                     :last-used ".agents/models.json"
                     :skip '(".backups" "layouts"))
   ;; `openclaw gateway --port 18789' under openclaw-gateway.service (its
   ;; src/daemon/constants.ts, docs/gateway/index.md); the process calls itself
   ;; `openclaw-gateway' once it runs (src/cli/gateway-cli/run-loop.ts).
   (make-agent-world :name "openclaw" :label "Openclaw"
                     :roots '(".openclaw/")
                     :evidence '(".openclaw/openclaw.json" ".openclaw/workspace/")
                     :takeover-unit "openclaw-gateway.service")
   (make-agent-world :name "commandcode" :label "Command Code"
                     :roots '(".commandcode/")
                     :evidence '(".commandcode/auth.json" ".commandcode/config.json"))
   ;; This organism's own retired carriers: the memory folders and the
   ;; SKILL.md library from before knowledge cells, with their use ledgers.
   ;; Once they are converted and removed the evidence is gone, and so is
   ;; the row's offer.
   (make-agent-world :name "nodecode" :label "Nodecode (retired memory/ and skills/)"
                     :roots '(".nodecode/memory/" ".nodecode/skills/")
                     :evidence '(".nodecode/memory/MEMORY.md" ".nodecode/skills/.skills/usage.jsonl")
                     :skip '("proposals" "proposed")
                     :enter '(".skills" ".experience")
                     :retire '("memory-index" "skills-index")))
  "Every world this build knows where to look for. Data only.")

(defun find-agent-world (name)
  "The world NAME names, or NIL. Matched without case."
  (and (stringp name)
       (find (string-trim " " name) *agent-worlds*
             :key #'agent-world-name :test #'string-equal)))

(defun agent-world-names ()
  (mapcar #'agent-world-name *agent-worlds*))

(define-record (detected-world (:copier nil) (:export :readers))
  "One world found on the box: the WORLD, the ROOT a report names, and
LAST-USED, the universal time its last-used path was written, or NIL."
  world root last-used)

;;; A test binds it to a fixture home, which is how a world nobody on this box
;;; has is still proved.
(define-startup-parameter *agent-home* (user-homedir-pathname)
  "Where the other harnesses' homes are looked for: the operating system's
home, not this profile's — a foreign harness keeps one home whichever
profile of this organism is running.")

(defun agent-home-path (relative)
  "RELATIVE under *AGENT-HOME* as a pathname, a directory when it ends in
`/'."
  (merge-pathnames relative *agent-home*))

(defun agent-path-present-p (relative)
  "Whether the path exists. One stat: nothing is opened, nothing is read."
  (and (probe-file (agent-home-path relative)) t))

(defun detect-agent-worlds (&key (worlds *agent-worlds*))
  "Every world of WORLDS whose evidence is on this box, most recently used
first."
  ;; A bounded local probe: one PROBE-FILE per evidence path and one
  ;; FILE-WRITE-DATE per world found, no file opened, nothing written, no
  ;; network.
  (sort (loop for world in worlds
              for root = (and (some #'agent-path-present-p world.evidence)
                              (find-if #'agent-path-present-p world.roots))
              for file = (and root world.last-used (probe-file (agent-home-path world.last-used)))
              when root
                collect (make-detected-world
                         :world world
                         :root (agent-home-path root)
                         :last-used (and file (ignore-errors (file-write-date file)))))
        (lambda (x y) (and x (or (null y) (> x y)))) :key #'detected-world-last-used))
