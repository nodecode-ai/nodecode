;;;; open-effect.lisp --- the open effect a command's answer carries: a session in a new tab of this shell's own terminal. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; A cell's command runs in the gateway, which does not know the person's
;;; terminal; the shell that ran the command does. So the answer names the
;;; session -- (:open SESSION-ID :prompt PROMPT) -- and this shell opens it
;;; where it runs, as `nodecode resume ID -- PROMPT' in a new tab of the
;;; terminal it sits in, over ssh and inside tmux alike. A shell under no
;;; terminal it can drive does not name :open among the effects it performs
;;; (SHELL-EFFECT-NAMES), so the command words its text for that: the line
;;; that opens the session by hand.

(defun terminal-tab-command (argv &key (directory (uiop:native-namestring (uiop:getcwd)))
                                       (title "nodecode") (getenv #'uiop:getenv))
  "The command that runs ARGV in a new tab of the terminal this shell runs
in -- a window or a pane where that terminal opens no tab from outside --
and whether it must run detached, as (values COMMAND DETACH), or NIL when
the environment GETENV reads names no terminal this shell can drive. The
innermost wins: a tmux inside kitty opens a tmux window."
  ;; Each is the terminal's own remote-control verb, keyed on the variable
  ;; that terminal sets in its children -- the only witness a program has
  ;; of the terminal it runs in. Over ssh none of a desktop terminal's
  ;; variables cross, so a multiplexer on the far side is what answers.
  ;; Ghostty on Linux has no such verb: `ghostty -e' is a new window that
  ;; lives as long as the program, hence detached.
  (flet ((env (name &aux (value (funcall getenv name)))
           (and value (plusp (length value)) value)))
    (cond ((env "TMUX") (list* "tmux" "new-window" "-c" directory "-n" title "--" argv))
          ((env "ZELLIJ") (list* "zellij" "run" "--cwd" directory "--name" title "--" argv))
          ((env "STY") (list* "screen" "-X" "screen" "-t" title argv))
          ((env "WEZTERM_PANE") (list* "wezterm" "cli" "spawn" "--cwd" directory "--" argv))
          ((env "KITTY_LISTEN_ON")
           (list* "kitty" "@" "--to" (env "KITTY_LISTEN_ON") "launch" "--type=tab"
                  "--cwd" directory "--tab-title" title argv))
          ((env "WT_SESSION") (list* "wt" "-w" "0" "new-tab" "-d" directory "--title" title argv))
          ((equal (env "TERM_PROGRAM") "ghostty")
           #+darwin (list* "open" "-na" "Ghostty" "--args"
                           (format nil "--working-directory=~a" directory) "-e" argv)
           #-darwin (values (list* "ghostty" (format nil "--working-directory=~a" directory)
                                   "-e" argv)
                            t)))))

(defun shell-effect-names (&key (getenv #'uiop:getenv))
  "The effects this shell performs, as POST /api/gateway/slash names them:
the clipboard and the draft always, and :open where the terminal GETENV
reads is one this shell can open a tab in (TERMINAL-TAB-COMMAND)."
  (if (terminal-tab-command '("nodecode") :directory "/" :getenv getenv)
      #("clipboard" "draft" "open")
      #("clipboard" "draft")))

(defun shell-word (word)
  "WORD as one POSIX shell word: as it is when nothing in it is special,
else single-quoted, a quote inside it closed, escaped and reopened."
  (if (and (plusp (length word))
           (every (lambda (char) (or (alphanumericp char) (find char "-_./:=@%+,"))) word))
      word
      (with-output-to-string (out)
        (write-char #\' out)
        (loop for char across word
              do (if (char= char #\') (write-string "'\\''" out) (write-char char out)))
        (write-char #\' out))))

(defun open-session-tab (session prompt)
  "Open SESSION in a new tab of this shell's terminal, PROMPT its first
prompt => the notice that says where, or (values WHY T) when it could not.
The terminal's verb runs to its exit, five seconds at most, so `opened' is
said only once the terminal took it."
  (multiple-value-bind (command detach) (terminal-tab-command (open-session-argv session prompt))
    (flet ((by-hand (why)
             (values (format nil "Open it in a terminal: `~{~a~^ ~}`~@[ (~a)~]"
                             (mapcar #'shell-word (open-session-argv session prompt))
                             (and (plusp (length why)) why))
                     t)))
      (cond ((null command) (by-hand nil))
            (detach (if (nlk:spawn-detached command (nlk:cache-path "nodecode/open-session.log"))
                        (format nil "Opened ~a in a new ~a window" session (first command))
                        (by-hand nil)))
            (t (multiple-value-bind (output errors status)
                   (ignore-errors (nlk:run-bounded command :seconds 5))
                 (declare (ignore output))
                 (if (eql 0 status)
                     (format nil "Opened ~a in a new ~a ~a" session (first command)
                             (if (member (first command) '("tmux" "screen") :test #'equal) "window" "tab"))
                     (by-hand (and errors (string-trim '(#\Newline #\Space) errors))))))))))

(defun perform-open-effect (model session prompt)
  "Open SESSION, PROMPT its first prompt, in a new tab of MODEL's terminal,
saying in the composer status what became of it."
  ;; Off the loop thread: a terminal's verb may take its time, and the
  ;; outcome lands in the composer's status once known.
  (if-let (program (chat-model-program model))
    (spawn "nodecode-open-effect"
      (multiple-value-bind (text error-p) (open-session-tab session prompt)
        (nlt:send program (make-instance 'system-notice-msg :text text :error-p error-p :answer t))))
    (multiple-value-bind (text error-p) (open-session-tab session prompt)
      (show-status model text :error-p error-p))))
