;;;; source-test.lisp --- a repository as an import source
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A git repository on the box, shaped as a Claude Code plugin, stands for
;;;; one on GitHub: the fetch, the walk, the plan and the removal all run on
;;;; it, and no gate reaches the network.

(in-package #:nodecode.test)

(defun fixture-git (directory &rest arguments)
  "git ARGUMENTS in DIRECTORY for a fixture, the operator's git config left
out. => its output, trimmed; a failure fails the gate."
  (multiple-value-bind (out err status)
      (nlk::run-bounded (list* "git" "-C" (uiop:native-namestring directory)
                               "-c" "user.name=fixture" "-c" "user.email=fixture@example.invalid"
                               "-c" "commit.gpgsign=false" "-c" "tag.gpgsign=false" arguments)
                        :seconds 60
                        :environment '("GIT_TERMINAL_PROMPT=0" "GIT_CONFIG_GLOBAL=/dev/null"
                                       "GIT_CONFIG_NOSYSTEM=1"))
    (unless (eql status 0)
      (error "fixture git ~a failed: ~a" (first arguments) err))
    (string-trim '(#\Newline #\Space) (or out ""))))

(defun make-source-fixture (box &aux (repo (merge-pathnames "plugin-repo/" (uiop:ensure-directory-pathname box))))
  "A repository in BOX shaped as a Claude Code plugin -- a skill with a script
beside it, a skill at its root, an .mcp.json naming its own files, and the
maintainers' CLAUDE.md and memory -- committed as v1 and tagged, then once
more. => (values URL V1 HEAD), URL its file:// address."
  (import-write repo "skills/probe-audit/SKILL.md"
                (format nil "---~%name: probe-audit~%description: >-~%  Audit a probe~%  with a fresh verifier.~%---~%Run scripts/check.cjs on each finding.~%"))
  (import-write repo "skills/probe-audit/scripts/check.cjs" (format nil "console.log('ok')~%"))
  (import-write repo "SKILL.md"
                (format nil "---~%name: root-skill~%description: The first root skill.~%---~%Say where it sits.~%"))
  (import-write repo ".mcp.json"
                "{\"mcpServers\": {\"probe-srv\": {\"command\": \"git\", \"args\": [\"${CLAUDE_PLUGIN_ROOT}/bin/serve.sh\", \"--quiet\"]}}}")
  (import-write repo "CLAUDE.md" (format nil "Maintainers: run make test before each push.~%"))
  (import-write repo "memory/notes.md" (format nil "The maintainers prefer tabs.~%"))
  (fixture-git repo "init" "--quiet" "--initial-branch=main")
  (fixture-git repo "add" "--all")
  (fixture-git repo "commit" "--quiet" "--message" "v1")
  (fixture-git repo "tag" "v1")
  (let ((v1 (fixture-git repo "rev-parse" "HEAD")))
    (import-write repo "SKILL.md"
                  (format nil "---~%name: root-skill~%description: The second root skill.~%---~%Say where it sits.~%"))
    (fixture-git repo "commit" "--quiet" "--all" "--message" "v2")
    (values (format nil "file://~a" (string-right-trim "/\\" (uiop:native-namestring repo)))
            v1
            (fixture-git repo "rev-parse" "HEAD"))))

(defun source-label (box commit) "The label the fixture repository in BOX is kept under at COMMIT."
  (format nil "local/~a/plugin-repo@~a" (nlk:folder-name box) (subseq commit 0 12)))

(deftest import-source-names-a-repository-the-ways-people-paste-one ()
  (is (equal '(:url "https://github.com/cloudflare/security-audit-skill"
               :name "github.com/cloudflare/security-audit-skill" :ref nil :path nil)
             (nik::parse-source "cloudflare/security-audit-skill")) "OWNER/REPO is GitHub's")
  (is (equal '(:url "https://github.com/GoogleChromeLabs/webmcp-tools"
               :name "github.com/GoogleChromeLabs/webmcp-tools" :ref "main" :path "webmcp-skills")
             (nik::parse-source "https://github.com/GoogleChromeLabs/webmcp-tools/tree/main/webmcp-skills")) "a tree URL names its ref and its folder")
  (is (equal '(:url "https://github.com/o/r" :name "github.com/o/r" :ref "v1.2" :path "skills/x")
             (nik::parse-source "github:o/r/skills/x#v1.2")) "#REF pins a branch, a tag or a commit")
  (is (equal "https://gitlab.com/o/r" (getf (nik::parse-source "https://gitlab.com/o/r.git") :url)) "another host's URL, its .git dropped")
  (is (equal '(:url "file:///C:/Users/me/repo" :name "local/me/repo" :ref nil :path nil)
             (nik::parse-source "file:///C:/Users/me/repo")) "a folder here is a file:// URL")
  #+win32
  (is (equal "local/me/repo" (getf (nik::parse-source "file://C:\\Users\\me\\repo") :name)) "or a Windows path")
  (is (null (nik::parse-source "claude")) "a world's name is no repository")
  (is (null (nik::parse-source "/tmp")) "nor is a path")
  (is (null (nik::parse-source "./o/r")) "nor a relative one")
  (signals-error nik:import-error (nik::parse-source "https://example.com/only-one")))

(deftest import-source-brings-skills-and-servers-alone (with-import-runtime (home box))
  (multiple-value-bind (url v1 head) (make-source-fixture box)
    (declare (ignore v1))
    (let* ((plan (nik:make-plan :source url))
           (label (source-label box head))
           (folder (merge-pathnames (format nil "sources/~a/" label) home))
           (root (string-right-trim "/" (uiop:native-namestring folder))))
      (is (equal '("mcp" "skills")
                 (sort (remove-duplicates (mapcar #'nik:item-kind (nik:plan-items plan)) :test #'string=)
                       #'string<)) "a repository brings skills and servers, never its maintainers' instructions or memory")
      (is (equal '("imported" "imported") (import-statuses plan "skills")) "the library's skill and the root's")
      (is (equal '("imported") (import-statuses plan "mcp")))
      (is (probe-file (merge-pathnames "skills/probe-audit/scripts/check.cjs" folder)) "the folder is kept at its commit, scripts and all")
      (is (null (probe-file (merge-pathnames ".git/" folder))) "without its .git")
      (nik:apply-plan plan)
      (is (null (kept-text "probe-audit")) "no skill is copied")
      (is-present (skill (nlk:knowledge-entry-named "probe-audit" nil)) "the skill is linked"
        (is (equal (truename (merge-pathnames "skills/probe-audit/SKILL.md" folder)) (nlk:knowledge-entry-path skill)) "read where the fetch keeps it, its script beside it")
        (is (equal "Audit a probe with a fresh verifier." (nlk:knowledge-entry-description skill)) "a folded block scalar is its description"))
      (is (equal "The second root skill." (nlk:knowledge-entry-description (nlk:knowledge-entry-named "root-skill" nil))) "no ref is the default branch's head")
      (let ((server (nlk:json-value (import-config) :object "mcp" "servers" "probe-srv")))
        (is (not (nlk:config-boolean server "enabled" t)) "its server lands off")
        (is (equalp (vector (format nil "~a/bin/serve.sh" root) "--quiet") (gethash "args" server)) "${CLAUDE_PLUGIN_ROOT} is the folder it sits in"))
      (is (equal (format nil "~a: 2 skills (probe-audit, root-skill), 1 server (probe-srv)" label)
                 (nik:sources))))))

(deftest import-source-ref-pins-a-commit (with-import-runtime (home box))
  (multiple-value-bind (url v1) (make-source-fixture box)
    (let ((plan (nik:make-plan :source (format nil "~a#v1" url) :only "skills")))
      (is (equal '("skills") (remove-duplicates (mapcar #'nik:item-kind (nik:plan-items plan)) :test #'string=)))
      (nik:apply-plan plan)
      (is-present (skill (nlk:knowledge-entry-named "root-skill" nil)) "the tag's root skill is imported"
        (is (equal "The first root skill." (nlk:knowledge-entry-description skill)) "the tag's commit, not the head")
        (is (uiop:subpathp (nlk:knowledge-entry-path skill)
                           (truename (merge-pathnames (format nil "sources/~a/" (source-label box v1)) home))))))
    (signals-error nik:import-error (nik:make-plan :source url :only "instructions"))))

(deftest import-source-removal-takes-back-what-it-brought (with-import-runtime (home box))
  (multiple-value-bind (url v1 head) (make-source-fixture box)
    (declare (ignore v1))
    (let ((label (source-label box head)))
      (nik:apply-plan (nik:make-plan :source url))
      (is (equal (format nil "~a removed: 2 skills (probe-audit, root-skill), 1 server (probe-srv), and its folder" label)
                 (nik:remove-source label)))
      (is (null (nlk:knowledge-entry-named "probe-audit" nil)) "its skills are gone")
      (is (null (nlk:knowledge-entry-named "root-skill" nil)))
      (is (null (directory (merge-pathnames "cells/linked-skills/*/" home) :resolve-symlinks nil)) "and their links")
      (is (null (nlk:json-value (import-config) :object "mcp" "servers" "probe-srv")) "its server is gone")
      (is (null (probe-file (merge-pathnames (format nil "sources/~a/" label) home))) "and its folder")
      (is (cl:search "no repository source here" (nik:sources)))
      ;; a server of the same name configured otherwise is not the source's to take
      (nik:apply-plan (nik:make-plan :source url))
      (nle:config-set '("mcp" "servers" "probe-srv" "command") "bash")
      (is (cl:search "0 servers" (nik:remove-source label)))
      (is (equal "bash" (nlk:json-value (import-config) :string "mcp" "servers" "probe-srv" "command"))))
    (signals-error nik:import-error (nik:remove-source "github.com/no/such@000000000000"))))

(deftest import-source-removal-forgets-an-older-builds-copies (with-import-runtime (home box))
  ;; An older build converted a source's skills into definitions carrying its
  ;; label as their :origin; taking the source back forgets them too.
  (multiple-value-bind (url v1 head) (make-source-fixture box)
    (declare (ignore v1))
    (let ((label (source-label box head)))
      (let ((nik::*link-skills* nil))
        (nik:apply-plan (nik:make-plan :source url :only "skills")))
      (is (kept-text "probe-audit") "an older build's copy")
      (is (cl:search "2 skills" (nik:remove-source label)))
      (is (null (kept-text "probe-audit")) "forgotten")
      (is (null (kept-text "root-skill"))))))
