;;;; open-effect-test.lisp --- gates for the open effect a command's answer carries (#199).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. The terminal this shell sits in is told by its own variable, the
;;;;      innermost first, and answers its own new-tab verb with ARGV whole.
;;;;   2. No variable it knows, or only empty ones: no command.
;;;;   3. A session opens as `nodecode resume ID [-- PROMPT]'.
;;;;   4. What cannot be opened says the effect's own fallback, else the line
;;;;      that opens it by hand, quoted for a shell.

(in-package #:nodecode.test)

(defun tab-command-under (variables &optional (argv '("nodecode" "resume" "s1")))
  "TERMINAL-TAB-COMMAND for ARGV under VARIABLES, a plist of name and value
standing in for the environment, from /w as the directory."
  (nle::terminal-tab-command argv :directory "/w"
                                  :getenv (lambda (name) (loop for (key value) on variables by (function cddr) when (equal key name) return value))))

(deftest open-effect-terminal-verbs ()
  (let ((argv '("nodecode" "resume" "s1")))
    (is (equal (list* "tmux" "new-window" "-c" "/w" "-n" "nodecode" "--" argv)
               (tab-command-under (list "TMUX" "/tmp/tmux-1/default,1,0"))) "under tmux a new window runs ARGV in this shell's folder")
    (is (equal (list* "zellij" "run" "--cwd" "/w" "--name" "nodecode" "--" argv)
               (tab-command-under (list "ZELLIJ" "0"))) "under zellij a new pane runs it")
    (is (equal (list* "screen" "-X" "screen" "-t" "nodecode" argv)
               (tab-command-under (list "STY" "123.pts-1"))) "under GNU screen a new window runs it")
    (is (equal (list* "wezterm" "cli" "spawn" "--cwd" "/w" "--" argv)
               (tab-command-under (list "WEZTERM_PANE" "3"))) "in WezTerm a new tab runs it")
    (is (equal (list* "kitty" "@" "--to" "unix:/tmp/k" "launch" "--type=tab"
                      "--cwd" "/w" "--tab-title" "nodecode" argv)
               (tab-command-under (list "KITTY_LISTEN_ON" "unix:/tmp/k"))) "in kitty with remote control a new tab runs it, through its socket")
    (is (equal (list* "wt" "-w" "0" "new-tab" "-d" "/w" "--title" "nodecode" argv)
               (tab-command-under (list "WT_SESSION" "abc"))) "in Windows Terminal a new tab of this window runs it")
    (multiple-value-bind (command detach) (tab-command-under (list "TERM_PROGRAM" "ghostty"))
      (is (equal argv (last command 3)) "in Ghostty ARGV closes the command whole")
      (is (member "-e" command :test #'equal) "Ghostty runs it through -e")
      #-darwin (is detach "on Linux Ghostty's window lives as long as the program: detached"))))

(deftest open-effect-innermost-terminal-wins ()
  (is (equal "tmux" (first (tab-command-under (list "TMUX" "/t,1,0" "KITTY_LISTEN_ON" "unix:/k"
                                                    "TERM_PROGRAM" "ghostty")))) "a tmux inside kitty or Ghostty opens a tmux window")
  (is (null (tab-command-under (list "KITTY_WINDOW_ID" "1"))) "kitty without a remote-control socket has no verb a detached program reaches")
  (is (null (tab-command-under (list "TMUX" "" "TERM_PROGRAM" "xterm"))) "an empty variable is unset, and an unknown terminal answers nothing")
  (is (null (tab-command-under nil)) "an ssh session with no multiplexer answers nothing"))

(deftest open-effect-session-line ()
  (let ((argv (nle::open-session-argv "s-42" "look at this")))
    (is (equal '("resume" "s-42" "--" "look at this") (last argv 4)) "a session opens as nodecode resume ID -- PROMPT, the prompt one word")
    (is (member "nodecode" argv :test #'equal) "the command is nodecode's own"))
  (is (equal '("resume" "s-42") (last (nle::open-session-argv "s-42" nil) 2)) "with no prompt nothing follows the id"))

(deftest open-effect-fallback-says-the-line ()
  (flet ((fallback (effect) (nle::json-value (nle::answer-effect-json effect) :string "fallback")))
    (let ((line (fallback '(:open :session "s-42" :prompt "hi there"))))
      (is (search "resume s-42 -- 'hi there'`" line) "a session's fallback is this home's shell line, its prompt quoted")
      (is (search (nle::shell-word (first (nle::shell-resume-argv "s-42"))) line) "the line is the one a shell of this home resumes with"))
    (is (equal "say this" (fallback '(:open :url "https://x.test" :fallback "say this"))) "the effect's own fallback wins")
    (is (equal "https://x.test" (fallback '(:open :url "https://x.test"))) "a page's fallback is the page")))
