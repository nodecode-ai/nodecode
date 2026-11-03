;;;; trust.lisp --- what a folder may hand a session beyond words. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A folder's AGENTS.md is read wherever a session stands (context.lisp
;;;; INSTRUCTIONS-FILES): words that add, which every harness reads without
;;;; asking. What a folder carries beyond words waits for the operator:
;;;; skills under .agents/skills/ or .claude/skills/, prose the model is told
;;;; to follow, often with scripts beside it; cells under .nodecode/cells/,
;;;; Lisp the one live image would load for every session; and MCP servers in
;;;; .mcp.json, processes started on this machine. PROJECT-RESOURCES finds
;;;; them on the walk AGENTS.md takes, from the session's folder up to /.
;;;;
;;;; A decision is the folder's own, never a parent's: the folder that carries
;;;; the resources is trusted or not by name. A decision for ~ that vouched
;;;; for every folder under it would trust every repository the operator ever
;;;; clones; this way a clone's .agents/skills/ is asked about the first time
;;;; a session stands in it, whatever was said about the folders above.
;;;;
;;;; Decisions live in the home's trust.json, {"folders": {"/abs/dir/":
;;;; "trust" | "deny"}}, one per profile, written whole and atomically.
;;;; "This session only" lives in the image and goes with it.
;;;;
;;;; The ask (ASK-FOLDER-TRUST) is put once per session, at its turn's
;;;; request build, to the person watching that session, through
;;;; ASK-OPERATOR: the web page, the desktop app and the terminal shell each
;;;; draw it. Nobody watching, or no answer before it expires, is "Not now":
;;;; the session goes on without what waits, nothing is saved, and the next
;;;; session asks again. /trust shows the decisions and changes them.
;;;;
;;;; What trust turns on today: a trusted folder's skills ride the session's
;;;; `project-skills' section, one line each, the file read on demand. Cells
;;;; and MCP servers are found and shown, and wait for their own wiring: the
;;;; ask names only what an answer turns on.

(in-package #:nodecode.evolved)

(defparameter *project-skill-folders* '(".agents/skills/" ".claude/skills/")
  "Where a folder keeps skills, each a subfolder holding SKILL.md: the open
Agent Skills location, then Claude Code's.")

(defparameter *project-cells-folder* ".nodecode/cells/"
  "Where a folder keeps cells, each a subfolder holding an .asd.")

(defparameter *project-mcp-file* ".mcp.json"
  "The file a folder names its MCP servers in, under \"mcpServers\".")

(defparameter *trust-asked-kinds* '(:skill)
  "The kinds of resource a trust decision turns on today, and so the ones
the ask names. A kind joins when trust has something to do with it.")

(defparameter +trust-choices+ '("Trust this folder" "This session only" "Not now")
  "The answers the ask offers, in the order the person reads them.")

(defvar *trust-file* nil
  "The decisions' file, or NIL for the home's trust.json.")

(defvar *trust-lock* (bt2:make-lock :name "folder-trust"))

(defvar *session-trust* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the trust keys trusted for that session alone.")

(defvar *trust-asked* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the trust keys its session was already asked about.")

(defun trust-file ()
  (or *trust-file* (nlk:home "trust.json")))

(defun trust-key (directory)
  "DIRECTORY as trust.json files it: its native name, links resolved, with a
trailing slash."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (uiop:native-namestring (or (ignore-errors (truename directory)) directory))))

;;; --- the decisions --------------------------------------------------------

(defun read-trust-decisions ()
  "The saved decisions, an EQUAL table of trust key -> :TRUST or :DENY;
empty when the file is missing, unreadable or not the shape it should be."
  (let ((table (make-hash-table :test #'equal))
        (folders (ignore-errors
                  (json-value (decode-json (nlk:read-text (trust-file))) :any "folders"))))
    (when (hash-table-p folders)
      (maphash (lambda (key value)
                 (let ((decision (cond ((equal value "trust") :trust)
                                       ((equal value "deny") :deny))))
                   (when (and decision (stringp key))
                     (setf (gethash key table) decision))))
               folders))
    table))

(defun save-trust-decisions (directories decision)
  "Save DECISION -- :TRUST, :DENY, or NIL to forget -- for each of
DIRECTORIES, the rest of the file kept. => the trust keys written."
  (check-type decision (member :trust :deny nil))
  (bt2:with-lock-held (*trust-lock*)
    (let ((table (read-trust-decisions))
          (keys (mapcar #'trust-key directories)))
      (dolist (key keys)
        (if decision (setf (gethash key table) decision) (remhash key table)))
      (let ((folders (make-hash-table :test #'equal)))
        (maphash (lambda (key value) (setf (gethash key folders) (string-downcase value))) table)
        (nlk:write-file-atomically (trust-file)
                                   (format nil "~a~%" (encode-json-object (json-object "folders" folders)))
                                   :mode #o600))
      keys)))

(defun trust-session (session-id directories)
  "Trust DIRECTORIES for SESSION-ID alone. => their trust keys."
  (let ((keys (mapcar #'trust-key directories)))
    (setf (gethash session-id *session-trust*)
          (union keys (gethash session-id *session-trust*) :test #'string=))
    keys))

(defun folder-decision (directory &optional session-id)
  "DIRECTORY's standing: :TRUST or :DENY as saved, :SESSION when trusted
for SESSION-ID alone, or NIL when nothing is decided."
  (let ((key (trust-key directory)))
    (or (gethash key (read-trust-decisions))
        (and session-id
             (member key (gethash session-id *session-trust*) :test #'string=)
             :session))))

;;; --- what a folder carries ------------------------------------------------

(defun nodecode-home-folder-p (directory)
  "Whether DIRECTORY, a .nodecode/ folder, is a nodecode home -- this
image's, or one that holds a config.jsonc or profiles/ -- whose cells are
the operator's own, never a project's."
  (or (uiop:file-exists-p (merge-pathnames "config.jsonc" directory))
      (uiop:directory-exists-p (merge-pathnames "profiles/" directory))
      (equal (ignore-errors (truename directory)) (ignore-errors (truename (nlk:home))))))

(defun subfolders (directory)
  (and (uiop:directory-exists-p directory)
       (sort (uiop:subdirectories directory) #'string< :key #'namestring)))

(defun folder-skills (directory)
  "The skills DIRECTORY keeps: (:SKILL NAME DESCRIPTION FILE) for each
subfolder of a skills folder whose SKILL.md names itself and says what it
is for."
  (loop for place in *project-skill-folders*
        nconc (loop for skill in (subfolders (merge-pathnames place directory))
                    for file = (uiop:file-exists-p (merge-pathnames "SKILL.md" skill))
                    for text = (and file (ignore-errors (nlk:read-text file)))
                    for (name description) = (and text (multiple-value-list (parse-skill-frontmatter text)))
                    when name collect (list :skill name description file))))

(defun folder-cells (directory &aux (home (merge-pathnames ".nodecode/" directory)))
  "The cells DIRECTORY keeps: (:CELL NAME FOLDER) for each subfolder of its
.nodecode/cells/ that holds an .asd -- none when that .nodecode/ is a home."
  (unless (and (uiop:directory-exists-p home) (nodecode-home-folder-p home))
    (loop for cell in (subfolders (merge-pathnames *project-cells-folder* directory))
          when (uiop:directory-files cell "*.asd")
            collect (list :cell (car (last (pathname-directory cell))) cell))))

(defun folder-mcp-servers (directory)
  "The MCP servers DIRECTORY names: (:MCP NAME FILE) for each key of its
.mcp.json's \"mcpServers\"."
  (let* ((file (uiop:file-exists-p (merge-pathnames *project-mcp-file* directory)))
         (servers (and file (ignore-errors
                             (json-value (decode-json (nlk:read-text file)) :any "mcpServers")))))
    (when (hash-table-p servers)
      (sort (loop for name being the hash-keys of servers
                  when (stringp name) collect (list :mcp name file))
            #'string< :key #'second))))

(defun folder-resources (directory)
  "What DIRECTORY itself carries beyond words, skills first."
  (append (folder-skills directory) (folder-cells directory) (folder-mcp-servers directory)))

(defun project-resources (cwd)
  "What the folders from CWD up to / carry beyond words, nearest folder
first: a list of (DIRECTORY . ITEMS), ITEMS as FOLDER-RESOURCES answers,
for each folder that carries any -- the walk INSTRUCTIONS-FILES takes."
  (let ((directory (and cwd (ignore-errors (uiop:ensure-directory-pathname cwd))))
        (found '()))
    (loop repeat 64
          while directory
          do (let ((items (ignore-errors (folder-resources directory)))
                   (parent (uiop:pathname-parent-directory-pathname directory)))
               (when items (push (cons directory items) found))
               (setf directory (and (not (equal parent directory)) parent))))
    (nreverse found)))

(defun asked-resources (cwd)
  "PROJECT-RESOURCES of CWD kept to the kinds trust turns on today, a folder
with none of them left out."
  (loop for (directory . items) in (project-resources cwd)
        for kept = (remove-if-not (lambda (item) (member (first item) *trust-asked-kinds*)) items)
        when kept collect (cons directory kept)))

(defun trusted-resources (session-id cwd)
  "The items of CWD's walk whose folder is trusted, saved or for SESSION-ID
alone, nearest folder first."
  (loop for (directory . items) in (project-resources cwd)
        when (member (folder-decision directory session-id) '(:trust :session))
          append items))

;;; --- what a session reads -------------------------------------------------

(defun project-skills-section (session-id cwd)
  "The `project-skills' section, (NAME . TEXT): one line for each skill a
trusted folder of CWD's walk keeps, or NIL for none. The nearest folder's
skill wins a name two folders share, and the operator's own skill or memory
of that name wins over both: a repository never stands in for one of yours."
  (let ((skills (remove-duplicates
                 (remove-if-not (lambda (item) (eq (first item) :skill))
                                (trusted-resources session-id cwd))
                 :key #'second :test #'string-equal :from-end t)))
    (setf skills (remove-if (lambda (skill) (ignore-errors (knowledge-entry-named (second skill) session-id)))
                            skills))
    (when skills
      (cons "project-skills"
            (format nil "Skills the folders of this session's project carry, trusted by the operator. ~
                         Read a skill's file before you follow it; one of your own by the same name wins.~%~
                         ~{- ~a~%~}"
                    (loop for (nil name description file) in skills
                          collect (format nil "~a: ~a (~a)" name description
                                          (home-abbreviated (uiop:native-namestring file)))))))))

;;; --- the ask --------------------------------------------------------------

(defun resource-summary (items)
  "ITEMS as one line a person reads: \"skills a, b · cells c · MCP d\"."
  (format nil "~{~a~^ · ~}"
          (loop for (kind label) in '((:skill "skills") (:cell "cells") (:mcp "MCP"))
                for names = (loop for item in items when (eq (first item) kind) collect (second item))
                when names collect (format nil "~a ~{~a~^, ~}" label names))))

(defun trust-question (folders)
  "=> (values TITLE DETAIL): the ask for FOLDERS, (DIRECTORY . ITEMS) each."
  (values (if (rest folders)
              (format nil "~d folders here carry skills that need your OK" (length folders))
              (format nil "~a carries skills that need your OK"
                      (home-abbreviated (uiop:native-namestring (car (first folders))))))
          (format nil "~{~a~%~}AGENTS.md is read either way. No answer counts as Not now: ~
                       this session goes on without them."
                  (loop for (directory . items) in folders
                        collect (format nil "~a: ~a" (home-abbreviated (uiop:native-namestring directory))
                                        (resource-summary items))))))

(defun record-trust-answer (session-id directories answer)
  "Act on ANSWER, one of +TRUST-CHOICES+ or NIL, for DIRECTORIES.
=> :TRUST, :SESSION, or NIL when nothing changed."
  (cond ((equal answer (first +trust-choices+)) (save-trust-decisions directories :trust) :trust)
        ((equal answer (second +trust-choices+)) (trust-session session-id directories) :session)))

(defun ask-folder-trust (session-id cwd)
  "Ask the person watching SESSION-ID, once per session, about the folders
of CWD's walk that carry what trust turns on and stand undecided; act on the
answer. => what RECORD-TRUST-ANSWER answered, or NIL when nothing was asked."
  (let* ((asked (gethash session-id *trust-asked*))
         (open (remove-if (lambda (entry)
                            (or (folder-decision (car entry) session-id)
                                (member (trust-key (car entry)) asked :test #'string=)))
                          (asked-resources cwd))))
    (when (and open (fboundp 'ask-operator))
      ;; Asked before the answer: a second round of this turn, or a turn the
      ;; person started while this one waits, does not ask again.
      (setf (gethash session-id *trust-asked*)
            (append asked (mapcar (lambda (entry) (trust-key (car entry))) open)))
      (multiple-value-bind (title detail) (trust-question open)
        (record-trust-answer session-id (mapcar #'car open)
                             (funcall 'ask-operator title :choices +trust-choices+
                                                          :detail detail :session-id session-id))))))

;;; --- /trust ---------------------------------------------------------------

(defun trust-report (session-id cwd)
  "What /trust shows: each folder of CWD's walk that carries anything, its
standing, and what it carries."
  (let ((folders (project-resources cwd)))
    (if (null folders)
        (format nil "No folder from ~a up carries skills, cells or MCP servers. AGENTS.md is read without trust."
                (home-abbreviated (uiop:native-namestring (uiop:ensure-directory-pathname cwd))))
        (format nil "~{~a~^~%~}~%~%/trust yes saves trust for these folders · /trust session trusts them for this session · ~
                     /trust no stops the ask · /trust forget clears the saved answer"
                (loop for (directory . items) in folders
                      collect (format nil "~a  ~a~%  ~a"
                                      (home-abbreviated (uiop:native-namestring directory))
                                      (case (folder-decision directory session-id)
                                        (:trust "trusted")
                                        (:session "trusted for this session")
                                        (:deny "not trusted")
                                        (t "not decided"))
                                      (resource-summary items)))))))

(defun headless-trust (args session-id &aux (word (string-downcase (string-trim " " args))))
  "/trust [yes|session|no|forget] for SESSION-ID's folder: the report, or
the decision for every folder of its walk that carries anything."
  (let* ((session (and session-id (nlk:store-open-p) (ignore-errors (nlk:find-session session-id))))
         (cwd (and session (nlk:durable-session-cwd session))))
    (if (null cwd)
        "/trust needs a session with a folder"
        (let ((directories (mapcar #'car (project-resources cwd))))
          (cond ((equal word "") (trust-report session-id cwd))
                ((null directories) (trust-report session-id cwd))
                ((member word '("yes" "trust") :test #'string=)
                 (save-trust-decisions directories :trust)
                 (format nil "Trusted: ~{~a~^, ~}. The next turn reads their skills."
                         (mapcar (lambda (d) (home-abbreviated (uiop:native-namestring d))) directories)))
                ((string= word "session")
                 (trust-session session-id directories)
                 "Trusted for this session. The next turn reads their skills.")
                ((member word '("no" "deny") :test #'string=)
                 (save-trust-decisions directories :deny)
                 "Not trusted, and not asked again. /trust forget asks again.")
                ((string= word "forget")
                 (save-trust-decisions directories nil)
                 (remhash session-id *session-trust*)
                 (remhash session-id *trust-asked*)
                 "Forgotten: the next turn asks again.")
                (t "/trust takes yes, session, no or forget"))))))
