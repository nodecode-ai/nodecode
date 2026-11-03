;;;; open-effect.lisp --- the open effect a command's answer carries: a session in a new tab of this shell's own terminal, a page or a file through this machine's opener. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; A cell's command runs in the gateway, which knows neither the person's
;;; terminal nor their desktop; the shell that ran the command knows both.
;;; So the answer names what to open -- (:open :session ID :prompt P),
;;; (:open :url U), (:open :file F) -- and this shell opens it where it
;;; runs: a session as `nodecode resume ID -- P' in a new tab of the
;;; terminal it sits in, over ssh and inside tmux alike; a page or a file
;;; through this machine's own opener (OPEN-TARGET). What it cannot open it
;;; says with the effect's fallback, the line that opens it by hand.

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

(defun open-session-tab (session prompt)
  "Open SESSION in a new tab of this shell's terminal, PROMPT its first
prompt => the notice that says where, or (values WHY T) when it could not:
WHY the terminal's own complaint, or NIL. The terminal's verb runs to its
exit, five seconds at most, so `opened' is said only once the terminal took
it."
  (multiple-value-bind (command detach) (terminal-tab-command (open-session-argv session prompt))
    (cond ((null command) (values nil t))
          (detach (if (nlk:spawn-detached command (nlk:cache-path "nodecode/open-url.log"))
                      (format nil "Opened ~a in a new ~a window" session (first command))
                      (values nil t)))
          (t (multiple-value-bind (output errors status)
                 (ignore-errors (nlk:run-bounded command :seconds 5))
               (declare (ignore output))
               (if (eql 0 status)
                   (format nil "Opened ~a in a new ~a ~a" session (first command)
                           (if (member (first command) '("tmux" "screen") :test #'equal) "window" "tab"))
                   (values (and errors (string-trim '(#\Newline #\Space) errors)) t)))))))

(defun open-effect-outcome (effect)
  "Open what EFFECT names on this machine => the notice saying so, or
(values TEXT T) saying the effect's fallback -- the line that opens it by
hand -- with why, when known."
  (let ((session (json-value effect :text "session"))
        (target (or (and (json-value effect :text "file") (file-uri (json-value effect :text "file")))
                    (json-value effect :text "url"))))
    (multiple-value-bind (text error-p)
        (cond (session (open-session-tab session (json-value effect :text "prompt")))
              ((and target (eq t (open-target target)))
               (format nil "Opened ~a" (or (json-value effect :text "file") target)))
              (t (values nil t)))
      (if error-p
          (values (format nil "~a~@[ (~a)~]" (or (json-value effect :text "fallback") "Nothing here opens it")
                          (and (plusp (length text)) text))
                  t)
          text))))

(define-answer-effect "open" (model effect)
  ;; Off the loop thread: a terminal's verb may take its time, and the
  ;; outcome lands in the composer's status once known.
  (if-let (program (chat-model-program model))
    (spawn "nodecode-open-effect"
      (multiple-value-bind (text error-p) (open-effect-outcome effect)
        (nlt:send program (make-instance 'system-notice-msg :text text :error-p error-p :answer t))))
    (multiple-value-bind (text error-p) (open-effect-outcome effect)
      (show-status model text :error-p error-p))))
