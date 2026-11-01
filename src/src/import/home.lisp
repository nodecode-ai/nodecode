;;;; home.lisp --- which files to look at, and what the walk makes of them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The manifest (world.lisp) says WHERE a home is; the shapes
;;;; (shape.lisp) say what a file HOLDS. This is the third and last piece:
;;;; the conventions that say which of a home's files are worth opening,
;;;; and it is conventions rather than a per-world file list on purpose —
;;;; a table of paths per world is a reader per world wearing different
;;;; clothes, and it misses exactly what a world table always misses. On
;;;; the box this was written against, a convention walk found per-project
;;;; MCP servers inside ~/.claude.json, provider keys in pi's models.json,
;;;; Cline's cron sqlite and Windsurf's memories — none of which anybody
;;;; would have written a row for.
;;;;
;;;; The conventions:
;;;;
;;;;   a config file     .json .jsonc .json5 .yaml .yml .toml, or an
;;;;                     environment file — read whole, classified whole
;;;;   a skill           a directory holding SKILL.md, under a directory
;;;;                     whose name starts with `skills', or a root: a
;;;;                     repository's own skill, or a skill's folder named
;;;;   instructions      markdown named the way the ecosystem names it
;;;;                     (AGENTS, CLAUDE, GEMINI, SOUL, IDENTITY, ...)
;;;;   a memory entry    a .md under a directory named memory or memories,
;;;;                     or one of the named memory files: one named entry
;;;;                     when its frontmatter names it, else split on its
;;;;                     own separator lines; a MEMORY.md of links is the
;;;;                     home's hot index instead
;;;;   a use ledger      usage.jsonl and sightings.jsonl, line by line
;;;;   a conversation    transcripts.lisp, given the roots
;;;;
;;;; and the bounds, because this runs on a first frame: a depth, a file
;;;; count, a byte cap per file, and the directories nobody's settings live
;;;; in. Everything read and skipped is counted, and what was seen and not
;;;; understood comes back as the UNMAPPED list — the backlog, and the
;;;; pasteable half of a bug report.

(in-package #:nodecode.import)

(nlk:access (named nlk::agent-world) (world nlk::agent-world))

;;; --- the bounds -------------------------------------------------------------

;;; Five reaches workspace/skills/<name>/SKILL.md and stops.
(defparameter +walk-file-depth+ 5
  "How far below a root the walk goes.")

(defparameter +text-byte-cap+ (* 512 1024)
  "The largest instruction or memory file the walk reads.")

(defparameter +skip-directories+
  '("node_modules" ".git" ".venv" "venv" "__pycache__" "cache" "caches" "tmp" ".tmp"
    "temp" "logs" "log" "bin" "dist" "build" "backups" "backup" "snapshot" "snapshots"
    "tool-output" "trash" "debug" "coverage" ".cache" "target" "vendor" ".github"
    ".husky" "fixtures" "testdata" "extensions" "node_modules" "site-packages")
  "Directory names no harness keeps settings in, skipped in every world.")

(defparameter +memory-directories+ '("memory" "memories")
  "Directory names whose markdown files are each one remembered entry.")

(defparameter +transcript-directories+
  '("sessions" "projects" "chats" "history" "storage" "conversations" "threads" "rollouts")
  "Directory names a harness keeps its conversations in: crossed, never read.")

;;; A home's own AGENTS.md sits at its root, or one directory in (a
;;; workspace); deeper than that the file belongs to something else — a
;;; skill's template, a plugin checkout's README, one subagent's brief — and
;;; folding those into the operator's own persona is how an import turns 43k
;;; characters of other people's text into this organism's standing
;;; instructions.
(defparameter +instruction-depth+ 1
  "How far below a root standing instructions are looked for.")

;;; Two kinds of standing text, by what a harness means by the name: its
;;; rules for every session (Claude Code's CLAUDE.md, Codex's AGENTS.md,
;;; Windsurf's global_rules.md), and a persona, the agent's voice (Hermes's
;;; and Openclaw's SOUL.md, Openclaw's IDENTITY.md). They land apart
;;; (plan.lisp PLAN-INSTRUCTIONS).
(defparameter +instruction-names+
  '("AGENTS" "AGENT" "CLAUDE" "GEMINI" "QWEN" "SYSTEM-PROMPT" "SYSTEM_PROMPT" "GLOBAL_RULES"
    "GLOBAL-RULES" "RULES" "INSTRUCTIONS")
  "The names a harness gives its rules for every session.")

(defparameter +persona-names+ '("SOUL" "IDENTITY" "PERSONA")
  "The names a harness gives the agent's persona.")

;;; The manifest table itself is the core's (NLK:*AGENT-WORLDS*, waist/worlds.lisp):
;;; the first frame has to know whether a past exists before any folder
;;; loads, so the question lives there and the readers live here.

;;; --- the home ---------------------------------------------------------------

(nlk:define-record (home (:copier nil))
  "One foreign home as the walk read it."
  ;; WORLD is its manifest, or NIL for a bare path; NAME what the report calls
  ;; it; ROOTS the directories and files walked; FACTS everything the shapes
  ;; made of them; UNMAPPED the files seen and not understood, relative to the
  ;; first root; NOTES one line per file that would not parse.
  world name (roots '()) (facts '()) (unmapped '()) (notes '()))

(defun home-world-name (home) (nlk:if-let (world home.world) world.name home.name))

(defun open-home (source)
  "SOURCE — a world name, a path, a repository (PARSE-SOURCE), or a WORLD —
as a HOME with its roots resolved and nothing read yet; a repository is
fetched at its commit first (SOURCE-HOME)."
  (let* ((world (cond ((nlk:agent-world-p source) source)
                      ((and (stringp source) (nlk:find-agent-world source)))))
         (repository (and (null world) (parse-source source)))
         (path (cond ((or world repository) nil)
                     ((pathnamep source) source)
                     ((stringp source) (nlk:expand-home source))
                     (t (fail "an import source is a world name (~{~a~^, ~}) or a path"
                              (nlk:agent-world-names))))))
    (cond
      (world
       (let ((roots (loop for relative in (nlk:agent-world-roots world)
                          when (probe-file (nlk:agent-home-path relative)) collect it)))
         (make-home :world world :name world.label
                    :roots (or roots
                               (fail "no ~a home on this box: looked for ~{~a~^, ~} under ~a"
                                     world.label world.roots (user-homedir-pathname))))))
      (repository (source-home repository))
      (t
       (let* ((file (or (probe-file path) (fail "no home at ~a" (namestring path))))
              ;; A path inside a world this build knows is still that world: the
              ;; manifest's skip list and takeover apply to a home read by path.
              (named (find-if (lambda (world)
                                (some (lambda (relative)
                                        (let ((root (probe-file (nlk:agent-home-path relative))))
                                          (and root (equal (namestring root) (namestring file)))))
                                      world.roots))
                              nlk:*agent-worlds*)))
         (make-home :world named
                    :name (if named named.label (namestring file))
                    :roots (list file)))))))

;;; --- the walk ---------------------------------------------------------------

(defun hidden-name-p (name)
  (and (stringp name) (plusp (length name)) (char= (char name 0) #\.)))

(defun skip-directory-p (home name &optional (hidden t) &aux (lower (string-downcase name)))
  "Whether a directory is not worth entering."
  ;; A hidden one never is, unless HIDDEN is NIL or the home's world names it
  ;; (ENTER): inside a home they are archives, caches and the tool's own
  ;; bookkeeping, and the homes that are themselves hidden are reached as
  ;; roots, not walked into.
  (or (and hidden (hidden-name-p name)
           (not (and home.world (member name (nlk:agent-world-enter home.world) :test #'string=))))
      (member lower +skip-directories+ :test #'string=)
      (and home.world (member name (nlk:agent-world-skip home.world) :test #'string-equal))))

;;; --- convention: a config file ----------------------------------------------

(defun environment-file-p (name)
  "Whether a file NAME is an environment file's: `.env', `.env.local', `x.env',
never the template one ships with: `.env.example', `.env.dist'."
  ;; A Claude Code plugin's .env.example said `ANTHROPIC_API_KEY=sk-ant-...',
  ;; and that placeholder landed as the operator's Anthropic key.
  (and (or (uiop:string-prefix-p ".env." name)
           (uiop:string-suffix-p name ".env"))
       (not (uiop:string-suffix-p name ".dist"))
       (notany (lambda (word) (cl:search word name :test #'char-equal))
               '("example" "sample" "template"))))

(defun backup-file-p (name &aux (words (ppcre:split "[.\\-_~ ]+" (string-downcase name))))
  "Whether a file NAME says it is a copy kept aside: `.env.bak-0612',
`.env.backup.1780870386', `config.old.json', and for an environment file
`.env.pre-email-disable'."
  ;; What the operator moved out of the live file: five subrouter
  ;; keys with no endpoint and a stale OpenRouter key all came from Hermes's
  ;; .env backups, and its .env held none of them.
  (or (intersection words '("bak" "backup" "backups" "old" "orig" "save" "saved") :test #'string=)
      (and (environment-file-p name) (member "pre" words :test #'string=))))

(defun cache-file-p (name)
  "Whether a file NAME says it is a cache: `plugin-catalog-cache.json', `models_cache.json'."
  ;; What a harness fetched and keeps, never what an operator set.
  (cl:search "cache" name :test #'char-equal))

;;; --- convention: a skill ----------------------------------------------------

(defun skill-support (directory &aux (inlined '()) (named '()))
  "=> (values LINES NAMED): the small text files beside a skill's SKILL.md
as ;; sections the definition carries, and every other file named with its
size, hidden files and __pycache__ left out."
  ;; Listed as the home walk lists (DIRECTORY-ENTRIES): through UIOP, each of
  ;; a library's skill folders makes and resolves a pathname a name, the
  ;; bulk of the skills plan.
  (labels ((walk (folder trail)
             ;; FOLDER native and `/'-ended, TRAIL where it sits under DIRECTORY
             (multiple-value-bind (files subdirectories) (directory-entries folder)
               (dolist (name files)
                 (let* ((relative (concatenate 'string trail name))
                        (native (concatenate 'string folder name))
                        (bytes (or (ignore-errors (sb-posix:stat-size (sb-posix:stat native))) 0)))
                   (unless (or (hidden-name-p name) (string-equal "SKILL.md" relative))
                     (if (and (member (name-type name) '("md" "txt") :test #'equal) (<= bytes 8192))
                         (push (cons (format nil "--- ~a ---" relative)
                                     (nlk:lines (nlk:read-text (uiop:parse-native-namestring native))))
                               inlined)
                         (push (format nil "~a (~d bytes)" relative bytes) named)))))
               (dolist (name subdirectories)
                 (unless (or (hidden-name-p name) (string= "__pycache__" name))
                   (walk (concatenate 'string folder name "/") (concatenate 'string trail name "/")))))))
    (walk (uiop:native-namestring (uiop:ensure-directory-pathname directory)) ""))
  (values (append (loop for section in (reverse inlined) append section)
                  ;; and where they sit, so the procedure can read and run them there
                  (and named (list (format nil "support files not carried: ~{~a~^, ~}"
                                           (reverse named))
                                   (format nil "they sit in ~a"
                                           (uiop:native-namestring
                                            (uiop:ensure-directory-pathname directory))))))
          (reverse named)))

(defun skill-fact (file relative world)
  (let* ((directory (uiop:pathname-directory-pathname file))
         (text (nlk:read-text file +text-byte-cap+))
         (declared (and text (ignore-errors (nle::parse-skill-frontmatter text))))
         (slug (string-downcase
                (substitute #\- #\Space
                            (string-trim '(#\Space #\Tab) (or declared (nlk:folder-name directory))))))
         (name (if (and (plusp (length slug))
                        (every (lambda (ch) (or (alphanumericp ch) (member ch '(#\- #\_ #\.)))) slug))
                   slug
                   (nlk:folder-name directory))))
    (make-fact :kind :skill :id name :world world :source relative
               :value (list :name name :directory directory :text text
                            :summary (and text (one-line-of text 80))
                            ;; read with the skill, on its lane (READ-HOMES)
                            :support (multiple-value-list (skill-support directory))))))

;;; --- convention: instructions and memory ------------------------------------

(defparameter +markdown-types+ '("md" "markdown" "txt")
  "The types a file of prose carries.")

(defun markdown-base (file)
  (and (member (string-downcase (or (pathname-type file) "")) +markdown-types+ :test #'string=)
       (string-upcase (pathname-name file))))

(defun one-line-of (text limit &aux (text (or text "")))
  "The first non-empty line of TEXT, whitespace folded, cut to LIMIT."
  ;; Line by line, as far as that line: split whole, every skill and memory
  ;; was split into all its lines for its first.
  (nlk:one-line (loop for start = 0 then (1+ end)
                      for end = (or (position #\Newline text :start start) (length text))
                      for line = (subseq text start end)
                      when (plusp (length (string-trim '(#\Space #\Tab #\Return #\-) line)))
                        return line
                      while (< end (length text)))
                :cap limit :ellipsis "…"))

;;; --- the read ---------------------------------------------------------------

(defun holds-p (part text &aux (first (char part 0)))
  "Whether TEXT holds the string PART."
  ;; From each place its first character is: CL:SEARCH walks every character
  ;; through a generic accessor.
  (loop for at = (position first text) then (position first text :start (1+ at))
        while at
          thereis (string= part text :start2 at :end2 (min (length text) (+ at (length part))))))

(defun note-unmapped (home relative why) (push (format nil "~a — ~a" relative why) home.unmapped))

(defun read-config-facts (home file relative)
  ;; A settings file is kilobytes: past 2 MB it is a log or a cache wearing a .json name.
  (let* ((octets (nlt:file-octets file (* 2 1024 1024)))
         (type (string-downcase (or (pathname-type file) "")))
         (tree (and octets
                    (handler-case
                        (flet ((text () (sb-ext:octets-to-string octets :external-format :utf-8)))
                          (cond ((environment-file-p (file-namestring file))
                                 (alexandria:alist-hash-table (read-env-file (text)) :test #'equal))
                                ((string= type "toml") (read-toml (text)))
                                ((member type '("yaml" "yml") :test #'string=) (read-yaml (text)))
                                ;; JSON from its octets, whole; JSONC too, which half the homes
                                ;; on a real box write as `.json'. Octets that read as JSON
                                ;; whole are their own normal form: a cache of half a megabyte
                                ;; made text, normalized and made octets again was 20 MB consed.
                                (t (or (ignore-errors (nlk:decode-json octets :whole t))
                                       (nlk:decode-json (nlk::normalize-jsonc (text)))))))
                      (error (condition)
                        (push (format nil "~a did not read: ~a" relative condition) home.notes)
                        nil)))))
    (cond ((null octets)
           (note-unmapped home relative
                          (format nil "~d bytes, past the cap" (or (nlk:file-bytes file) 0)))
           '())
          ((null tree) '())
          (t (let ((facts (with-plugin-root (classify-tree tree :source relative :world (home-world-name home))
                                            file)))
               (unless facts (note-unmapped home relative "nothing this build reads"))
               facts)))))

(defun with-plugin-root (facts file &aux (needle "${CLAUDE_PLUGIN_ROOT}"))
  "FACTS, ${CLAUDE_PLUGIN_ROOT} in each MCP server's command, args and env
made the folder FILE sits in: a Claude Code plugin's .mcp.json names its own
files that way, and the mcp folder expands no variable."
  (let ((root (string-right-trim "/" (uiop:native-namestring (uiop:pathname-directory-pathname file)))))
    (flet ((expanded (value)
             (if (and (stringp value) (cl:search needle value))
                 (with-output-to-string (out)
                   (loop with start = 0
                         for at = (cl:search needle value :start2 start)
                         do (write-string value out :start start :end (or at (length value)))
                         while at
                         do (write-string root out)
                            (setf start (+ at (length needle)))))
                 value)))
      (dolist (fact facts facts)
        (when (eq (fact-kind fact) :mcp)
          (let ((server (getf (fact-value fact) :object)))
            (multiple-value-bind (command found) (gethash "command" server)
              (when found (setf (gethash "command" server) (expanded command))))
            (multiple-value-bind (args found) (gethash "args" server)
              (when (and found (vectorp args)) (setf (gethash "args" server) (map 'vector #'expanded args))))
            (multiple-value-bind (env found) (gethash "env" server)
              (when (and found (hash-table-p env))
                (maphash (lambda (key value) (setf (gethash key env) (expanded value))) env)))))))))

(defun frontmatter (text &aux (block (nle::frontmatter-block text)))
  "=> (values FIELDS BODY): TEXT's leading `---' block as ((KEY . VALUE)
...), a key indented under a bare `key:' line read as `key.child', a quoted
value unquoted, a block scalar's lines joined, and the text after the block;
NIL and TEXT when it has none."
  (if (null block)
      (values nil text)
      (let ((section nil) (fields '()) (lines (car block)))
        (flet ((blank-p (line) (string= "" (string-trim '(#\Space #\Tab #\Return) line)))
               (indentation (line)
                 (or (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) line) 0)))
          (loop while lines
                do (ppcre:register-groups-bind (indent key value)
                       ("^([ \\t]*)([A-Za-z0-9_.-]+):[ \\t]*(.*?)[ \\t\\r]*$" (pop lines))
                     (let ((value (ppcre:regex-replace "^([\"'])(.*)\\1$" value "\\2")))
                       ;; A block scalar -- `>' folded, `|' literal -- is the lines indented
                       ;; under its key: a published skill's description is often one.
                       (when (ppcre:scan "^[>|][+-]?[0-9]?$" value)
                         (let* ((folded (char= #\> (char value 0)))
                                (body (loop while (and lines (or (blank-p (first lines))
                                                                 (> (indentation (first lines)) (length indent))))
                                            collect (string-trim '(#\Space #\Tab #\Return) (pop lines)))))
                           (setf value (string-trim '(#\Newline)
                                                    (format nil (if folded "~{~a~^ ~}" "~{~a~^~%~}")
                                                            (if folded (remove "" body :test #'string=) body))))))
                       (cond ((plusp (length indent))
                              (when section (push (cons (format nil "~a.~a" section key) value) fields)))
                             ((string= value "") (setf section key))
                             (t (setf section nil) (push (cons key value) fields)))))))
        ;; The body's lines as NLK:LINES gives them, joined: CRLF made LF and
        ;; the trailing blank lines off.
        (values (nreverse fields)
                (let ((body (cdr block)))
                  (if (find #\Return body)
                      (format nil "~{~a~^~%~}" (nlk:lines body))
                      (string-right-trim '(#\Newline) body)))))))

(defun claude-projects (&aux (config (nlk::json-object-file (nlk:agent-home-path ".claude.json"))))
  "The project roots Claude Code's ~/.claude.json names, or NIL."
  (and config (hash-table-p (gethash "projects" config))
       (alexandria:hash-table-keys (gethash "projects" config))))

(defvar *project-roots* nil
  "Claude Code's project roots by munged spelling, read once per walk (READ-HOMES binds it).")

(defun project-roots (&aux (table (make-hash-table :test 'equal)))
  "Claude Code's project roots, by the munged spelling its projects/ folders take."
  (dolist (root (claude-projects) table)
    (alexandria:ensure-gethash (string-downcase (ppcre:regex-replace-all "[^A-Za-z0-9]" root "-"))
                               table root)))

(defun trail-project (trail &aux (munged (second (member "projects" trail :test #'string=))))
  "The project a memory under TRAIL is about when it sits in a folder named
for one -- Claude Code's projects/<root, munged>/memory/: the root
~/.claude.json names whose munged spelling is that folder's, else NIL."
  ;; Read per memory, ~/.claude.json (200 KB on a working box) was decoded
  ;; once for each of 1,100 entries: ten seconds of a first frame's import.
  (and munged
       (values (gethash (string-downcase munged)
                        (or *project-roots* (setf *project-roots* (project-roots)))))))

(defun memory-file-fact (home relative trail fields body)
  "One remembered entry from a memory file whose frontmatter names it: its
name, description, type, scope and dates, and its body."
  (flet ((field (key) (cdr (assoc key fields :test #'string-equal))))
    (let ((scope (field "scope"))
          (body (nlk:trimmed body)))
      (make-fact :kind :memory :id (field "name") :world (home-world-name home) :source relative
                 :value (list :name (field "name")
                              :description (or (field "description")
                                               (field "metadata.short-description")
                                               (one-line-of body 100))
                              :type (or (field "type") (field "metadata.type"))
                              :project (if (and scope (uiop:string-prefix-p "project " scope))
                                           (nlk:trimmed (subseq scope 8))
                                           (trail-project trail))
                              :created (field "created") :updated (field "updated")
                              :sources (and (field "sources")
                                            (remove "" (mapcar #'nlk:trimmed
                                                               (uiop:split-string
                                                                (string-trim "[]" (field "sources"))
                                                                :separator ","))
                                                    :test #'string=))
                              :text body)))))

(defun hot-index-facts (home relative text &aux (facts '()))
  "A MEMORY.md of links, `- [name](name.md) — description', as one :HOT
fact per name it lists: the home's hot index, which the import keeps as a
view of each."
  (ppcre:do-register-groups (name) ("(?m)^\\s*-\\s*\\[([^\\]]+)\\]\\([^)]*\\.md\\)" text)
    (push (make-fact :kind :hot :id name :world (home-world-name home) :source relative) facts))
  (nreverse facts))

(defun usage-facts (home file relative)
  "The use a ledger FILE records, as :USAGE facts: a skills library's
usage.jsonl line by line, an experience ledger's helped and harm sightings
by the memory they name and its tool sightings as calls."
  (let ((sightings (string= "sightings.jsonl" (file-namestring file))))
    (loop for line in (nlk:read-json-lines file)
          for index from 0
          for kind = (nlk:json-value line :string "kind")
          for (name use) = (cond ((not sightings) (list (nlk:json-value line :string "name") kind))
                                 ((member kind '("helped" "harm") :test #'equal)
                                  (list (nlk:json-value line :string "line") kind))
                                 ((equal kind "tool") (list (nlk:json-value line :string "tool") "call")))
          when (and name use)
            collect (make-fact :kind :usage :id (format nil "~a#~d" relative index)
                               :world (home-world-name home) :source relative
                               :value (list :name name :kind use
                                            :at (nlk:json-value line :string "at")
                                            :session (nlk:json-value line :string "session"))))))

(defun read-text-facts (home file relative trail)
  "The instruction and memory facts a markdown file yields."
  ;; Instructions are the home's OWN — at its root or one directory in, never
  ;; inside somebody else's checkout; a remembered entry can sit as deep as
  ;; its home files it, which for one harness is projects/<project>/memory/.
  ;; A named one outranks the folder it sits in: Windsurf keeps its
  ;; global_rules.md among its memories.
  (let* ((base (markdown-base file))
         (named (and base
                     (or (member base +instruction-names+ :test #'string=)
                         (member base +persona-names+ :test #'string=))
                     (<= (length trail) +instruction-depth+)
                     (not (intersection trail '("skills" "plugins" "marketplaces" "agents"
                                                "subagents" "templates" "commands" "hooks"
                                                "examples" "docs" "extensions" "powers" "recipes")
                                        :test #'string=))))
         (memory (and base (not named)
                      (or (intersection trail +memory-directories+ :test #'string=)
                          (and (member base '("MEMORY" "MEMORIES" "USER") :test #'string=)
                               (<= (length trail) +instruction-depth+)))))
         (wanted (or memory named))
         (text (and wanted (nlk:read-text file +text-byte-cap+))))
    (cond
      ((not wanted) nil)
      ((null text)
       (note-unmapped home relative "past the text cap")
       '())
      ;; A harness's own seed: nothing left once its comments and headings are
      ;; out. Each pattern runs where its mark is: run on every file.
      ((not (ppcre:scan "(?m)^[ \\t\\r]*[^# \\t\\r\\n]"
                        (if (holds-p "<!--" text)
                            (ppcre:regex-replace-all "(?s)<!--.*?(?:-->|\\z)" text "")
                            text)))
       '())
      ;; A home's hot index: links to its memory files, not memories of its own.
      ((and memory (string= base "MEMORY") (hot-index-facts home relative text)))
      (memory
       (multiple-value-bind (fields body) (frontmatter text)
         (if (assoc "name" fields :test #'string-equal)
             ;; A memory file that names itself is one entry, under that name.
             (list (memory-file-fact home relative trail fields body))
             ;; One entry per run of lines between `§' separator lines, line-end CRs off.
             (let ((type (if (string= base "USER") "user" "project")))
               (loop for piece in (if (find #\§ text)
                                      (ppcre:split "(?m)^[ \\t\\r]*§[ \\t\\r]*$" text)
                                      (list text))
                     for entry = (nlk:trimmed (if (find #\Return piece)
                                                  (ppcre:regex-replace-all "\\r+(?=\\n|\\z)" piece "")
                                                  piece))
                     when (plusp (length entry))
                       collect (make-fact :kind :memory
                                          :id (format nil "~a-~a" (home-world-name home)
                                                      (nlk:short-digest entry 10))
                                          :world (home-world-name home) :source relative
                                          :value (list :text entry :type type)))))))
      (t
       (list (make-fact :kind :instructions :id base
                        :world (home-world-name home) :source relative
                        :value (list :text text :hash (nlk:short-digest text 10)
                                     :persona (and (member base +persona-names+ :test #'string=)
                                                   t))))))))

(defun name-type (name &aux (dot (position #\. name :from-end t)))
  "The type a file NAME's pathname reads: what follows its last dot, NIL when
no dot follows its first character (`.env')."
  (and dot (plusp dot) (subseq name (1+ dot))))

(defun directory-entries (directory &aux (files '()) (subdirectories '())
                                         (stat (make-instance 'sb-posix:stat)))
  "The native DIRECTORY's entries, `/'-ended, as (values FILES SUBDIRECTORIES):
names, a link counted as what it reaches and one that reaches nothing left
out, in UIOP:DIRECTORY-FILES's and UIOP:SUBDIRECTORIES's order."
  ;; One readdir, and a stat only for the names the listing leaves open -- a
  ;; link, or a file system that does not say. Through UIOP each directory
  ;; would be listed twice and each entry would make a pathname and resolve.
  ;; With a stat for every name, the walk would pay for the transcripts in
  ;; folders it only crosses.
  (multiple-value-bind (names kinds) (nlk::directory-names directory)
    (dolist (name names)
      (case (pop kinds)
        (:directory (push name subdirectories))
        ((:file :other) (push name files))
        (t (when (ignore-errors (sb-posix:stat (concatenate 'string directory name) stat))
             (if (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) sb-posix:s-ifdir)
                 (push name subdirectories)
                 (push name files)))))))
  ;; SBCL's DIRECTORY sorts by namestring: a subdirectory's ends in `/'.
  (values (sort files #'string<)
          (sort subdirectories #'string< :key (lambda (name) (concatenate 'string name "/")))))

(defun read-in (home job &aux (scratch (make-home :world home.world :name home.name)))
  "What JOB, a function of a home it notes into, reads of HOME, as (FACTS
UNMAPPED NOTES), its notes newest first: noted apart, jobs on several
threads write nothing over each other and their notes keep the walk's order."
  (list (funcall job scratch) (home-unmapped scratch) (home-notes scratch)))

;; A home's read is in three parts, so that homes and files read side by
;; side where nothing they read binds them: each home's walk (WALK-HOME) on
;; a lane of its own; then every prose file of every home -- instructions,
;; memories, skills, ledgers -- eight at a time; and beside those the
;; settings files, read here, under this call's bindings, since what a
;; setting means is the catalog's and the box's.

(defun walk-home (home text &aux (visits '()) (budget 6000) (projects nil)
                                 (first-root (uiop:ensure-directory-pathname (first home.roots))))
  "The files of HOME a read wants, in walk order, as (KIND . JOB): JOB a
function of the home it notes into, KIND :SETTINGS for one read under the
caller's bindings, else :PROSE. The second value says whether a memory sits
under a projects/ folder, which reads Claude Code's project roots."
  ;; TEXT nil leaves its prose -- instructions, memory, skills, ledgers -- unread.
  (labels ((visit (path name trail &optional relative
                   &aux (type (string-downcase (or (name-type name) ""))))
             ;; PATH native, RELATIVE its name in the report when not FIRST-ROOT's;
             ;; a pathname is made only for a file that is read
             (macrolet ((job (kind form)
                          `(let* ((file (uiop:parse-native-namestring path))
                                  (relative (or relative (enough-namestring file first-root))))
                             (push (cons ,kind (lambda (home) ,form)) visits))))
               (cond
                 ((and text (string-equal "SKILL.md" name)
                       ;; a library's skill, or the one a root is
                       (or (null trail) (some (lambda (name) (uiop:string-prefix-p "skills" name)) trail)))
                  (job :prose (list (skill-fact file relative (home-world-name home)))))
                 ((and (not (member (string-downcase name)
                                    '("package.json" "package-lock.json" "bun.lock" "bun.lockb"
                                      "yarn.lock" "tsconfig.json" "jsconfig.json" "composer.lock"
                                      "pnpm-lock.yaml")
                                    :test #'string=))
                       (not (cache-file-p name))
                       (not (backup-file-p name))
                       (or (environment-file-p name)
                           (member type '("json" "jsonc" "json5" "yaml" "yml" "toml") :test #'string=)))
                  (job :settings (read-config-facts home file relative)))
                 ((and text (member type +markdown-types+ :test #'string=))
                  (when (member "projects" trail :test #'string=) (setf projects t))
                  (job :prose (read-text-facts home file relative trail)))
                 ((and text (member name '("usage.jsonl" "sightings.jsonl") :test #'string=))
                  (job :prose (usage-facts home file relative))))))
           ;; A conversation directory is crossed, never read (no settings) nor skipped (memory lives in one).
           (walk (directory trail depth crossing)
             (when (and (plusp budget) (<= depth +walk-file-depth+))
               (multiple-value-bind (files subdirectories) (directory-entries directory)
                 (unless crossing
                   (dolist (name files)
                     (when (plusp budget)
                       (decf budget)
                       (visit (concatenate 'string directory name) name trail))))
                 (dolist (name subdirectories)
                   (let* ((lower (string-downcase name))
                          (into (and (not (member lower +memory-directories+ :test #'equal))
                                     ;; a memory folder's projects/ holds memories, not transcripts
                                     (or (and (member lower +transcript-directories+ :test #'equal)
                                              (not (intersection trail +memory-directories+
                                                                 :test #'equal)))
                                         crossing))))
                     ;; crossed only for the memory folders it holds, prose
                     (unless (or (skip-directory-p home name) (and into (not text)))
                       (walk (concatenate 'string directory name "/") (append trail (list lower))
                             (1+ depth) into))))))))
    ;; A root named for what it holds -- a memory folder, a skills library --
    ;; is read as what its name says, as a folder of that name inside one is.
    (dolist (root home.roots)
      (if (uiop:directory-pathname-p root)
          (let ((name (string-downcase (nlk:folder-name root))))
            (walk (uiop:native-namestring root)
                  (and (member name (cons "skills" +memory-directories+) :test #'string=)
                       (list name))
                  0 nil))
          (visit (uiop:native-namestring root) (file-namestring root) '() (file-namestring root)))))
  (values (nreverse visits) projects))

(defun read-homes (homes &key (sessions t) (text t) tally)
  "Every one of HOMES read into facts, each HOME filled in as it goes; TEXT
nil leaves their prose unread, TALLY reads which models each ran (TALLY-FACTS).
A home whose read fails keeps the failure as a note, and no facts."
  ;; SESSIONS nil leaves the conversations alone — what the first frame wants,
  ;; since a transcript store is gigabytes and a provider key is bytes. TEXT
  ;; nil is a plan of keys and config alone: the setup walk's first, which
  ;; skipped past nothing but the 1,800 files it never wanted.
  (flet ((guarded (function)
           (lambda (&rest arguments)
             (handler-case (apply function arguments) (error (condition) condition)))))
    (let* ((walks (nlk:parallel-map (guarded (lambda (home) (multiple-value-list (walk-home home text))))
                                    homes))
           ;; each home's project roots, read here where the box is bound
           (roots (mapcar (lambda (walk) (and (consp walk) (second walk) (project-roots))) walks))
           ;; (HOME ROOTS . JOB) for each job of WANTED kind the walks found
           (jobs (lambda (wanted)
                   (loop for home in homes
                         for walk in walks
                         for roots* in roots
                         when (consp walk)
                           append (loop for (kind . function) in (first walk)
                                        when (eq kind wanted)
                                          collect (list* home roots* function)))))
           (prose (nlk:spawn "import prose"
                    (nlk:parallel-map (guarded (lambda (job)
                                                 (destructuring-bind (home roots . function) job
                                                   (let ((*project-roots* roots))
                                                     (read-in home function)))))
                                      (funcall jobs :prose))))
           (settings (let ((*normal-names* (make-hash-table :test #'equal)))
                       (loop for (home nil . function) in (funcall jobs :settings)
                             collect (funcall (guarded #'read-in) home function))))
           ;; here too, beside the prose: a few indexed queries a home
           (tallies (mapcar (lambda (home) (and tally (tally-facts home))) homes))
           (prose (bt2:join-thread prose)))
      (loop for home in homes
            for walk in walks
            for (tallied noted) in tallies
            do (let ((reads (and (consp walk)
                                 (loop for (kind) in (first walk)
                                       collect (if (eq kind :prose) (pop prose) (pop settings))))))
                 (nlk:if-let (failure (if (consp walk) (find-if-not #'listp reads) walk))
                   (push (princ-to-string failure) home.notes)
                   (let ((facts '()))
                     (loop for (found unmapped notes) in reads
                           do (setf home.unmapped (append unmapped home.unmapped)
                                    home.notes (append notes home.notes)
                                    facts (revappend found facts)))
                     (setf facts (append (nreverse facts) tallied)
                           home.notes (append noted home.notes))
                     (when sessions
                       (setf facts (append facts (session-facts home))))
                     (setf home.facts facts
                           home.unmapped (nreverse home.unmapped)
                           home.notes (nreverse home.notes)))))))))
