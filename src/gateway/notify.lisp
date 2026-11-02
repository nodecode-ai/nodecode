;;;; notify.lisp --- a desktop notice when a turn ends. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A second reader of the frames PUBLISH-EVENT lets out (gateway.lisp),
;;;; beside the activity map: a turn.completed or turn.failed
;;;; fact spawns the command the `notify' section names, detached, never
;;;; waited on and never handed stdio (the serving image's fds are severed
;;;; while a TUI owns the terminal). It runs on the publishing thread with no
;;;; lock held, and nothing it does can fail the publish: a command that
;;;; cannot start is said once an image, not after every turn.
;;;;
;;;; SCOPE: PUBLISH-EVENT answers whether any subscribed peer -- a TUI --
;;;; received the frame, the one origin signal a frame carries (provenance is
;;;; durable-log-only). The default `attached' notifies only then, which keeps
;;;; Discord and Telegram turns quiet: those sessions have no peer. `all' is
;;;; the escape hatch for a turn that finishes after the TUI closed. A
;;;; cancelled turn is silent by design: the operator pressed Esc.
;;;;
;;;; The section's presence turns it on, the rule every section shares:
;;;;   "notify": {"command": ["notify-send", "--app-name=Nodecode",
;;;;                          "{title}", "{body}"],
;;;;              "scope": "attached"}
;;;; `{title}' and `{body}' are replaced inside every argv word, so a tool
;;;; that wants `--message={body}' works as well as one taking positionals.
;;;; With no command, Linux gets notify-send; Windows a toast through Windows
;;;; PowerShell, the text inside its script as base64 -- a failed turn's
;;;; detail is the provider's text, and spliced in as it is it would run as
;;;; script; Linux under WSL, whose desktop is Windows', the same toast
;;;; through the powershell.exe WSL puts on PATH; a Mac osascript's `display
;;;; notification', the title and body the run handler's arguments, for the
;;;; same reason.

(in-package #:nodecode.evolved)

(nlk:define-section ("notify")
  (:guide "A desktop notice when a turn ends; the section's presence turns it on. command is the argv to spawn, {title} and {body} filled into every word")
  ("command" :list :doc "the argv; notify-send under the app name when unset, a toast on Windows and WSL")
  ("scope" :choice :options '("attached" "all") :default "attached"
           :doc "attached pops only when a shell received the frame; all always"))

;;; A toast from Windows PowerShell's own app id: Windows drops one from an
;;; id no installed app registered. The title and body arrive as base64 of
;;; their UTF-8, which is inert inside the script and ASCII on the command
;;; line (a child's environment and argv both lose anything past Latin-1 on
;;; the way there). No -WindowStyle: the child shares the terminal's console;
;;; without this, hiding it would hide Windows Terminal after every turn.
(defparameter +notify-windows-toast+
  "$d = { param($s) [Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($s)) }
$m = [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime]
$x = $m::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02)
$t = $x.GetElementsByTagName('text')
[void]$t.Item(0).AppendChild($x.CreateTextNode((& $d '~a')))
[void]$t.Item(1).AppendChild($x.CreateTextNode((& $d '~a')))
$m::CreateToastNotifier('{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}\\WindowsPowerShell\\v1.0\\powershell.exe').Show([Windows.UI.Notifications.ToastNotification]::new($x))")

(defvar *notified-turn* nil
  "The turn the last notice popped for: a republished page must not pop
twice for one turn.")

(defvar *notify-failure-said* nil
  "Whether this image has said a notice that could not start: once is enough.")

(defun notify-base64 (text &optional (external-format :utf-8))
  (cl-base64:usb8-array-to-base64-string (sb-ext:string-to-octets text :external-format external-format)))

(defun notify-host (&optional (platform (nlk:platform)) (release (software-version)))
  "PLATFORM, but :WSL for Linux under Windows' subsystem -- its kernel's
RELEASE names Microsoft -- where the desktop is Windows' and no notify-send is."
  (if (and (eq platform :linux) (search "microsoft" release :test #'char-equal))
      :wsl
      platform))

(defun notify-default-command (body &optional (host (notify-host)))
  "The argv an operator who named none gets for BODY: notify-send's
positional SUMMARY then BODY under the app name, on Windows and under WSL the
toast, on a Mac osascript's notice."
  (case host
    ((:windows :wsl)
     (list "powershell.exe" "-NoProfile" "-NonInteractive" "-EncodedCommand"
           (notify-base64 (format nil +notify-windows-toast+
                                  (notify-base64 "Nodecode") (notify-base64 body))
                          :utf-16le)))
    (:darwin
     '("osascript" "-e" "on run argv"
       "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
       "-e" "end run" "{title}" "{body}"))
    (t '("notify-send" "--app-name=Nodecode" "{title}" "{body}"))))

(defun notify-launch (argv)
  "Spawn ARGV with no stdio and never wait on it: a thread of its own reaps it."
  ;; The one process spawn here, so a test stubs this name.
  (nlk:bind (((pid _ _ process)
              (nlk:spawn-program argv :output nil :error-output nil)))
    (nlk:spawn "notify-reap" (nlk:wait-for-child pid process))))

(defun notify-turn-end (op delivered)
  "Pop the desktop notice for OP, a frame PUBLISH-EVENT let out, when it is a
turn's end the `notify' section asks for; DELIVERED is whether a shell
received it."
  (nlk:with-handlers ((error (condition)
                        (unless *notify-failure-said*
                          (setf *notify-failure-said* t)
                          (notice (format nil "notify: ~a" condition) :level :warning))))
    (nlk:bind (((type payload _ turn-id) (nlk:frame-fact op)))
      (when (member type '("turn.completed" "turn.failed") :test #'equal)
        (let* ((table (nlk:section-table (read-shared-config) '("notify")))
               (settings (and table (nlk:section-enabled-p table)
                              (nlk:section-settings (nlk:find-section '("notify")) table))))
          (when (and settings
                     (or delivered (equal (getf settings :scope) "all"))
                     (not (equal turn-id *notified-turn*)))
            (setf *notified-turn* turn-id)
            (let* ((session-id (getf op :session-id))
                   ;; The session's directory name, else its id.
                   (name (nlk:when-let (cwd (nlk:find-session-cwd session-id)) (nlk:folder-name cwd)))
                   (label (if (stringp name) name session-id))
                   (detail (or (nlk:json-value payload :string "detail") ""))
                   (body (if (equal type "turn.failed")
                             (format nil "Turn failed — ~a: ~a" label (nlk:first-line detail 120))
                             (format nil "Ready for input — ~a" label))))
              ;; Every argv word with {title} and {body} filled in.
              (notify-launch (mapcar (lambda (word)
                                       (uiop:frob-substrings
                                        (uiop:frob-substrings word '("{title}") "Nodecode")
                                        '("{body}") body))
                                     (or (getf settings :command)
                                         (notify-default-command body)))))))))))
