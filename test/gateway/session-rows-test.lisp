;;;; session-rows-test.lisp --- what a session's row in a list says of it.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun session-row (id)
  "ID's entry in the session directory."
  (find id (nlk:list-sessions) :key (lambda (row) (getf row :id)) :test #'equal))

(defun seed-provider-turn (id input &key provider model answer)
  "One completed turn on ID: INPUT, a provider request naming PROVIDER and
MODEL when PROVIDER is given, and ANSWER when it is given."
  (ensure-durable-session id)
  (let ((turn (nlk:admit-turn id (nlk::make-durable-id "command") input)))
    (when provider
      (nlk:record-turn-provider-request turn :provider provider :model model))
    (when answer
      (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" answer)))
    (nlk:complete-turn turn nil)
    turn))

(deftest the-directory-keeps-each-sessions-provider-and-newest-answer (with-temp-store ())
  ;; A session's provider and model are the ones its newest provider request
  ;; named; its answer is its newest answer's first line that says something,
  ;; bold and code marks dropped and a heading, quote or list mark trimmed,
  ;; bounded to 160 characters. An answer with no text (a round of tool calls
  ;; alone) leaves the one before it. The fill derives the same from the log,
  ;; and an open rebuilds a directory written before the columns.
  (seed-provider-turn "sr-claude" "fix it" :provider "claude-code" :model "claude-opus-5-5"
                      :answer (format nil "~%  ## **Done.** PR `#12` is green~%the rest"))
  (seed-provider-turn "sr-two" "first" :provider "openai-codex" :model "gpt-6" :answer "- first answer")
  (seed-provider-turn "sr-two" "second" :provider "deepseek" :model "deepseek-v4" :answer "")
  (seed-provider-turn "sr-long" "long" :provider "local" :model "qwen"
                      :answer (make-string 200 :initial-element #\x))
  (seed-provider-turn "sr-quiet" "quiet")
  (flet ((said () (mapcar (lambda (id &aux (row (session-row id)))
                            (list (getf row :provider) (getf row :model) (getf row :answer)))
                          '("sr-claude" "sr-two" "sr-long" "sr-quiet"))))
    (let ((kept (list '("claude-code" "claude-opus-5-5" "Done. PR #12 is green")
                      '("deepseek" "deepseek-v4" "first answer")
                      (list "local" "qwen" (make-string 160 :initial-element #\x))
                      '(nil nil nil)))
          (handle (nlk::require-open-store)))
      (is (equal kept (said)) "each session's newest provider and model, and its newest answer's line")
      (sqlite:execute-non-query handle "delete from session_directory")
      (nlk::apply-schema handle)
      (is (equal kept (said)) "the fill derives the same from the log")
      ;; A directory an older build wrote: none of the three columns, and a
      ;; trigger that keeps none of them.
      (sqlite:execute-non-query handle "drop trigger session_directory_follows_the_log")
      (sqlite:execute-non-query handle "create trigger session_directory_follows_the_log
                                          after insert on events begin select 1; end")
      (sqlite:execute-non-query handle "drop table session_directory")
      (sqlite:execute-non-query handle "create table session_directory (
                                          session_id text primary key, created_at text, cwd text,
                                          parent, anchor_turn_id, title text, prompt text,
                                          updated_at text not null,
                                          message_count integer not null default 0,
                                          source text, turn_id text, turn_kind text,
                                          waits text, waits_turn text) without rowid")
      (nlk::apply-schema handle)
      (is (search "answer text" (sqlite:execute-single handle "select sql from sqlite_master where name = 'session_directory'")) "the open rebuilds it with the columns")
      (is (equal kept (said)) "filled again from the log")
      (seed-provider-turn "sr-quiet" "now" :provider "anthropic" :model "claude-haiku" :answer "Merged.")
      (is (equal '("anthropic" "claude-haiku" "Merged.") (car (last (said)))) "and the trigger with it keeps them"))))

(deftest a-sessions-row-names-its-folders-branch (with-temp-directory (dir "session-rows"))
  ;; The branch is read from the checkout's HEAD (FOLDER-BRANCH, the PR
  ;; lookup's reader), the nearest checkout up from the folder as git looks; a
  ;; linked worktree's .git is a file that names its git dir; a detached HEAD
  ;; and a folder outside every checkout have none. The row carries it beside the provider, the model and the answer,
  ;; and null for each one a session lacks.
  (flet ((git (cwd &rest args)
           (uiop:run-program (list* "git" "-C" (namestring cwd) "-c" "user.name=T" "-c" "user.email=t@t" args)
                             :output :string :error-output :string)))
    (let ((repo (merge-pathnames "repo/" dir))
          (deep (merge-pathnames "repo/src/deep/" dir))
          (tree (merge-pathnames "repo-wt/" dir))
          (plain (merge-pathnames "plain/" dir)))
      (ensure-directories-exist deep)
      (ensure-directories-exist plain)
      (git repo "init" "-q" "-b" "main")
      (git repo "commit" "-q" "--allow-empty" "-m" "first")
      (git repo "worktree" "add" "-q" "-b" "nc/feature" (namestring tree))
      (is-table (folder want) (equal want (nle::folder-branch (namestring folder)))
        (repo "main" "a checkout's own branch")
        (deep "main" "from a folder inside it")
        (tree "nc/feature" "a linked worktree's, through its .git file")
        (plain nil "none outside a checkout")
        ("" nil "none for a session with no folder"))
      (let ((row (nle::gateway-session-entry-json
                  (list :id "sr-row" :title "t" :working-directory (namestring tree)
                        :provider "claude-code" :model "claude-opus-5-5" :answer "Done.")))
            (bare (nle::gateway-session-entry-json (list :id "sr-bare" :title "" :working-directory (namestring plain)))))
        (is-shape row ("provider" "claude-code") ("model" "claude-opus-5-5")
          ("last_answer" "Done.") ("branch" "nc/feature"))
        (is-shape bare ("provider" eq :null) ("model" eq :null) ("last_answer" eq :null) ("branch" eq :null)))
      (git repo "checkout" "-q" "--detach")
      (is (null (nle::folder-branch (namestring repo))) "none on a detached HEAD"))))
