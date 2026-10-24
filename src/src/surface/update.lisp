;;;; update.lisp --- a shipped release reaches the running organism.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The organism's own update, in its own process. The
;;;; installer (src/tools/install.sh, shipped beside the binary) owns the
;;;; layout: <release-directory>/<version>/ beside a `current' link. What
;;;; runs here is the policy around it:
;;;;
;;;;   find      the channel pointer names a release label (LATEST-RELEASE)
;;;;   stage     the installer lays it out beside this build; `current'
;;;;             does not move and nothing restarts (STAGE-RELEASE)
;;;;   classify  CLASSIFY-UPGRADE over the core tree and the shipped cells
;;;;             tree says hot or restart, definition by definition
;;;;   apply     CONVERGE evaluates every changed definition a live image
;;;;             can take into this one under a lowered floor, all or
;;;;             nothing; `current' moves; every attached shell folds the
;;;;             same tree (ORGANISM.UPGRADED). Boot state the release holds
;;;;             (a changed defstruct, a package) lands at the next launch:
;;;;             a shell's organism when the operator reopens it, the
;;;;             gateway its unit runs by exiting once no turn is in flight,
;;;;             for the unit to bring it back on `current' (RESTART-WHEN-IDLE).
;;;;
;;;; Who triggers: the poller (config `update': auto applies, ask stages and
;;;; says so, off never looks), `nodecode update' from a shell, and the
;;;; route the verb calls. One lock: an update runs whole or not at all.

(in-package #:nodecode.evolved)

;;; --- where releases live ----------------------------------------------------

(defun installed-release-root ()
  "The folder above this binary when a `current' link sits there -- the
installer's layout this binary runs from -- else NIL."
  (let* ((runtime (nlk:runtime-directory))
         (parent (and runtime (uiop:pathname-parent-directory-pathname runtime))))
    (and parent (probe-file (merge-pathnames "current" parent)) parent)))

;;; The layout keeps the host it was installed from (install.sh's base-url):
;;; a build installed from a mirror or a folder would otherwise read the
;;; project's pointer when its launch no longer carries the variable, and move
;;; itself to that release.
(nlk:define-startup-parameter *release-url*
    (let ((env (or (uiop:getenv "NODECODE_BASE_URL")
                   (when-let (root (installed-release-root))
                     (ignore-errors (uiop:read-file-line (merge-pathnames "base-url" root)))))))
      (if (and env (plusp (length env)))
          (string-right-trim "/" env)
          "https://nodecode.ai/dl"))
  "Where releases are fetched from: the tarballs, their sha256 sidecars and
one pointer per channel and platform. The installer's NODECODE_BASE_URL, else
the one the layout this binary runs from was installed with.")

;;; A build from a checkout is not a release: the channel's pointer names
;;; something else, often older. Without this check the poller would take
;;; that pointer for an update -- staging stable into the home and
;;; hot-loading its source over the newer image, whose failed compile would
;;; print `; caught 1 fatal ERROR condition' into the operator's transcript.
(nlk:define-startup-parameter *installed-build* (and (installed-release-root) t)
  "Whether this binary runs from the installer's layout, which is what makes
automatic updates its default (UPDATE-SETTINGS).")

;;; The installer's layout this binary runs from, else the installer's
;;; default under the home.
(nlk:define-startup-parameter *release-directory*
    (or (installed-release-root) (merge-pathnames ".local/share/nodecode/" (user-homedir-pathname)))
  "Where the installer lays versions out: <here>/<version>/nodecode beside a
`current' link.")

;;; install.ps1 on Windows, whose releases are zips under a `current' junction.
(nlk:define-startup-parameter *installer-path*
    (let ((runtime (nlk:runtime-directory))
          (name (if (eq (nlk:platform) :windows) "install.ps1" "install.sh")))
      (and runtime
           (or (probe-file (merge-pathnames name runtime))
               (probe-file (merge-pathnames (format nil "src/tools/~a" name) runtime)))))
  "The installer this build carries: install.sh (install.ps1 on Windows)
beside the binary in a release, under src/tools/ in a checkout. NIL when
neither is there.")

(defun release-platform ()
  "ARCH-OS as a release names itself: x86_64-linux, arm64-darwin. The names
pack.sh gives a tarball, from the same two reads the kernel makes of itself."
  (format nil "~a-~a"
          (ecase (nlk:architecture) (:x64 "x86_64") (:arm64 "arm64"))
          (case (nlk:platform) (:darwin "darwin") (:windows "windows") (t "linux"))))

(defun version-label (version)
  "The release label in VERSION: what follows `+' in 0.0.1+e4cfdd109, or
VERSION itself."
  (if-let (plus (position #\+ version)) (subseq version (1+ plus)) version))

(defun effective-label ()
  "The release label this image's live definitions are at: the hot upgrade's
when one moved them, else the version folder this binary runs from when it
lives in the installer's layout -- <release-directory>/<label>/nodecode
beside a `current' link, where the folder is the release's identity and the
stamp inside the binary agrees with it for every published build -- else
the build's own stamp."
  (or nlk::*effective-version*
      (and (installed-release-root) (car (last (pathname-directory (nlk:runtime-directory)))))
      (version-label *user-agent-version*)))

(defun effective-version (&aux (label (effective-label)))
  "The version this image reports for itself -- the connect ack, the identity
route, the skew notice: the build's own until a hot upgrade moves the
definitions past it, then the build's number with the new label."
  (if (equal label (version-label *user-agent-version*))
      *user-agent-version*
      (format nil "~a+~a"
              (subseq *user-agent-version* 0 (position #\+ *user-agent-version*))
              label)))

(defun release-version-directory (version)
  (merge-pathnames (format nil "~a/" version) *release-directory*))

;;; --- the release host ------------------------------------------------------

(defun release-host-p (url)
  "Whether URL may serve releases: https://, or a folder as file://. Over
plain http anyone on the path could hand over another pointer, another
tarball and the sha256 sidecar that matches it; install.sh and install.ps1
refuse such a host by this same rule."
  (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "file://" url)))

(defun release-label-p (label)
  "Whether LABEL can name a release: 1 to 63 of ASCII letters, digits and
._-+, never starting with a dot. A label becomes a folder under the release
directory that a reinstall replaces whole, so `.' or `..' would name that
directory or its parent. install.sh and install.ps1 hold the same rule."
  (and (stringp label) (< 0 (length label) 64) (char/= (char label 0) #\.)
       (every (lambda (c) (and (< (char-code c) 128) (or (alphanumericp c) (find c "._-+"))))
              label)))

(defun release-fetch (url &key (timeout 20))
  "GET URL as text => the body, or (values NIL REASON). A file:// URL, a
release host that is a folder as the installer takes one, reads the file;
a host RELEASE-HOST-P refuses is not asked."
  (multiple-value-bind (body reason)
      (cond ((uiop:string-prefix-p "file://" url)
             (or (ignore-errors (uiop:read-file-string (subseq url 7))) (values nil "no such file")))
            ((release-host-p url) (http-fetch url :timeout timeout))
            (t (values nil "a release host is https:// or a folder (file://)")))
    (if body (nlk:body-text body) (values nil reason))))

(defvar *release-fetch* 'release-fetch
  "The GET the release host is asked with, a function of a URL; a test binds
a stub.")

(defun latest-release (&key (channel "stable"))
  "The release label CHANNEL's pointer names for this platform, or (values
NIL REASON)."
  (bind ((url (format nil "~a/~a-~a" (release-url) channel (release-platform)))
         ((text reason) (funcall *release-fetch* url))
         (version (and (stringp text)
                       (nlk:trimmed text))))
    (if (release-label-p version)
        version
        (values nil (format nil "~a: ~a" url (or reason "names no version"))))))

;;; --- the installer ---------------------------------------------------------

(defun last-line (text &aux (bare (string-trim '(#\Newline #\Return #\Space) text)))
  ;; Return too: install.ps1's lines end in CRLF; without trimming it, the
  ;; last line would read as a bare CR and say nothing of why staging failed.
  (string-trim '(#\Return) (subseq bare (1+ (or (position #\Newline bare :from-end t) -1)))))

(defun run-installer (mode version doing &aux (installer *installer-path*))
  "Run the shipped installer in MODE -- \"stage\" or \"link\" -- for VERSION
against *RELEASE-DIRECTORY* and *RELEASE-URL*. => T, or a refusal: DOING it failed."
  (unless (and installer (probe-file installer))
    (nlk::upgrade-fail "no installer travels with this build, so it cannot stage a release"))
  (multiple-value-bind (out err code)
      (run-bounded
       ;; No -NoModifyPath for install.ps1: its link moves the user PATH's
       ;; entry to the version it links, which an elevated shell needs.
       (if (eq (nlk:platform) :windows)
           (list "powershell.exe" "-NoProfile" "-NonInteractive" "-ExecutionPolicy" "Bypass"
                 "-File" (uiop:native-namestring installer))
           (list "sh" (uiop:native-namestring installer) "--no-modify-path"))
       :environment
       (list* (format nil "NODECODE_STAGE=~a" mode)
              (format nil "NODECODE_VERSION=~a" version)
              (format nil "NODECODE_BASE_URL=~a" (release-url))
              ;; where the versions lie: install.sh's name for it, install.ps1's
              (format nil "~a=~a"
                      (if (eq (nlk:platform) :windows) "NODECODE_INSTALL_DIR" "NODECODE_HOME")
                      (string-right-trim "/\\" (uiop:native-namestring *release-directory*)))
              ;; A PSModulePath inherited from PowerShell 7 names its own module
              ;; folders first, and the Windows PowerShell 5.1 this runs cannot
              ;; load its Microsoft.PowerShell.Utility from them: `Get-FileHash'
              ;; is not found, and every staging fails at the sha256. Left
              ;; empty, 5.1 reads its own.
              (and (eq (nlk:platform) :windows) (list "PSModulePath=")))
       ;; a fetch of a whole release over a slow link, and no longer
       :seconds 1800)
    (or (eql code 0)
        (nlk::upgrade-fail
         "~a ~a failed: ~a" doing version
         (last-line (format nil "~a~@[~%~a~]~:[~;~%the installer ran past 30 minutes and was killed~]"
                            out (and (plusp (length err)) err) (eq code :timeout)))))))

(defun stage-release (version)
  "Lay VERSION out beside this build through the installer: fetched,
verified, probed where it lies; `current' untouched. => its directory."
  (run-installer "stage" version "staging")
  (release-version-directory version))

(defun link-release (version)
  "Point `current' at VERSION, laid out earlier: what every launch after
this runs. Nothing restarts."
  (run-installer "link" version "linking"))

;;; --- the plans -------------------------------------------------------------

(defun release-plans-at (directory &aux (core-new (merge-pathnames "src/src/" directory))
                                        (cells-new (merge-pathnames "src/cells/" directory))
                                        (core-old nlk:*source-root*)
                                        (cells-old (ignore-errors (nlk::shipped-cells-directory))))
  "What the release laid out at DIRECTORY means for this image: the core
tree against this image's own, and the shipped cells tree when both sides
carry one. => ((PLAN . ROOT) ...), the core first."
  (unless core-old
    (nlk::upgrade-fail "this image carries no source tree of its own to compare against"))
  (unless (probe-file core-new)
    (nlk::upgrade-fail "no source tree at ~a" (uiop:native-namestring core-new)))
  (append (list (cons (nlk::classify-upgrade core-old core-new) core-new))
          (when (and cells-old (probe-file cells-old) (probe-file cells-new))
            (list (cons (nlk::classify-upgrade cells-old cells-new) cells-new)))))

(defun plans-verdict (plans)
  (let ((verdicts (mapcar (lambda (pair) (getf (car pair) :verdict)) plans)))
    (or (find :restart verdicts) (find :hot verdicts) :same)))

(defun plans-held (plans)
  (loop for (plan . nil) in plans append (getf plan :held)))

(defun held-text (held)
  (format nil "~a — ~a~@[ +~d more~]"
          (caar held) (cdar held) (and (rest held) (length (rest held)))))

;;; --- the organism's update -------------------------------------------------

(defvar *upgrade-lock* (bt2:make-lock :name "upgrade")
  "One update at a time, whole.")

(defvar *refused-release* nil
  "The release this image refused live and linked for its next launch, or
NIL: the poller leaves it be rather than refuse it again at every look.")

;;; dexador's Windows backend (WinHTTP) reads a whole body before it answers
;;; -- "This obviously isn't streaming", its own note -- and a feed never
;;; ends: every connection through it would time out. There the feed rides a
;;; socket of the image's own (FEED-OPEN-SOCKET), which knows nothing of the
;;; system's proxy, so the poller keeps a short fallback.
(defun feed-streams-p ()
  "Whether dexador can hold the release feed open itself."
  (not (eq dex:*dexador-backend* :winhttp)))

(nlk:define-section ("update")
  (:guide "How this organism hears of a release and takes it: auto applies one as it lands -- live where it can go live, else from the next restart; ask stages it and says so, for `nodecode update' to apply; off never looks.")
  ("mode" :choice :options '("auto" "ask" "off")
          :doc "auto, ask or off; unset, auto on an installed build and off on a checkout's")
  ("channel" :string :default "stable" :doc "the release channel followed")
  ("check_minutes" :integer :min 1
                   :doc "minutes between looks at the channel; unset, 360 where a release feed can say the moment one lands, else 30")
  ("url" :string :doc "the release host, a mirror of your own; unset, the one this build was installed from")
  ("feed" :string :doc "where the release feed lives, or off for none; unset, beside the release host, none for a folder"))

(defun update-settings ()
  "=> (values MODE CHANNEL MINUTES URL FEED) from the shared config's
`update' section as its declaration reads it: auto (off for a build
outside the installer's layout), stable, 360 -- 30 where no feed can run --
the release host and the feed's host unless it says otherwise."
  ;; FEED names where the release feed lives -- `off' holds none -- and
  ;; defaults to the release host, beside whose /dl path the feed route sits
  ;; (FEED-URL). Six hours is the cadence of a box the feed tells the moment
  ;; a release lands; a box it cannot reach hears only the poller, and at six
  ;; hours an open shell on Windows sat a working day behind every release.
  (let* ((settings (nlk:section-settings (nlk:find-section '("update"))
                                         (ignore-errors (json-value (read-shared-config) :object "update"))))
         (url (string-right-trim "/" (or (getf settings :url) *release-url*)))
         (feed (string-right-trim "/" (or (getf settings :feed) "")))
         ;; a host that is a folder holds no feed: one read as a socket
         ;; would print SB-BSD-SOCKETS::ADDRESS (NIL NIL)
         (feed (cond ((plusp (length feed)) feed) ((uiop:string-prefix-p "https://" url) url) (t "off"))))
    (values (or (getf settings :mode) (if *installed-build* "auto" "off"))
            (getf settings :channel)
            (or (getf settings :check-minutes) (if (and (feed-streams-p) (string/= feed "off")) 360 30))
            url feed)))

(defun release-url ()
  "Where this organism fetches releases from: the config's `update.url'
when set -- a mirror of the operator's own -- else *RELEASE-URL*."
  (nth-value 3 (update-settings)))

(defun update-note (level &optional control &rest args)
  "The update's standing note at LEVEL: CONTROL formatted with ARGS, or none. => NIL."
  (nlk:note-standing "update" (and control (apply #'format nil control args)) level)
  nil)

(defun update-answer-text (result &aux (to (getf result :to)) (line (getf result :line)))
  "RESULT (UPGRADE-ORGANISM's) as the sentence the verb prints."
  (ecase (getf result :applied)
    (:current (format nil "already at ~a" to))
    (:checked (format nil "~a is staged, nothing runs it yet: ~a" to line))
    (:staged (format nil "~a is staged: ~a · `nodecode update' applies it" to line))
    ((:live :hot) line)))

(defun release-target (version &aux (channel (nth-value 1 (update-settings))))
  "The release an update moves to: VERSION, else the configured channel's
latest; refuses (UPGRADE-REFUSAL) when neither names one, or VERSION is no
release label (RELEASE-LABEL-P)."
  (when (and version (not (release-label-p version)))
    (nlk::upgrade-fail "~a is not a release label" version))
  (or version
      (multiple-value-bind (latest reason) (latest-release :channel channel)
        (or latest
            (nlk::upgrade-fail "no ~a release named for ~a: ~a"
                               channel (release-platform) reason)))))

(defun upgrade-organism (&key version check apply idle hold)
  "This organism's update, whole: VERSION -- the channel's latest when NIL
-- staged beside this build, classified, and under APPLY carried out: hot,
in this image and every attached shell, while no turn can begin -- under
IDLE once none runs -- or a restart at the next idle moment. Applied under
HOLD, VERSION is the operator's own pick and holds this home there."
  ;; Not under APPLY, it is staged and said (the poller in `ask' mode); under
  ;; CHECK, classified only. => a plist: :from :to :verdict :line :held and
  ;; :applied, one of :current :checked :staged :live :hot. Refuses
  ;; (UPGRADE-REFUSAL) when the release cannot be named or staged; one this
  ;; image will not evaluate is linked for its next boot. The image is as it was.
  (bt2:with-lock-held (*upgrade-lock*)
    (bind ((from (effective-label)) (to (release-target version)))
      ;; An update asked for is the operator's choice of what runs: one that
      ;; names its release holds this home there -- the way back after an
      ;; update, and the one hold there is -- and one that names none lifts it.
      (when (and apply (not check)) (if hold (hold-release to) (lift-hold)))
      (when (string= to from)
        (return-from upgrade-organism
          (list :from from :to to :verdict :same :line "" :applied :current)))
      (let* ((plans (release-plans-at (stage-release to)))
             (verdict (plans-verdict plans))
             (line (format nil "~{~a: ~a~^; ~}"
                           (loop for (plan) in plans for label in '("core" "cells")
                                 collect label collect (nlk::upgrade-plan-line plan))))
             (held (plans-held plans))
             (result (list :from from :to to :verdict verdict :line line :held held)))
        (cond
          (check (append result (list :applied :checked)))
          ((not apply)
           (update-note :info "update ~a is staged: ~a · `nodecode update' applies it" to line)
           (append result (list :applied :staged)))
          (t
           ;; A release that holds boot state (:RESTART) still reaches this
           ;; image, gateway or not: every definition a live image can take
           ;; goes live here, now, `current' moves so the next launch runs
           ;; the whole new build, and the boot state it held applies at
           ;; that build's own boot -- which the gateway a unit runs makes
           ;; for itself once idle (RESTART-WHEN-IDLE). A hot release holds
           ;; nothing, so its standing note clears. What the classifier sent
           ;; hot and this image refused is the same: the image is as it
           ;; was and the whole release waits for that boot. Refused and left
           ;; there, a release would strand the image on the build before it:
           ;; every release staged, none run.
           (multiple-value-bind (count files refused displaced)
               (converge-between-turns plans to (eq verdict :restart) idle)
             (link-release to)
             (when refused (setf *refused-release* to))
             ;; An exit brings this process back on `current' when it serves `gateway run'
             ;; under a unit nodecode wrote.
             (let* ((restarts (and (or held refused) *gateway-run*
                                   (ignore-errors (nlk:gateway-installed-p))))
                    (launch (if restarts
                                "the gateway's restart, once no turn is running"
                                "the next launch"))
                    (text (cond (refused
                                 (format nil "~a runs at ~a: this image refused it live (~a)"
                                         to launch refused))
                                ((not held)
                                 (format nil "~a is live: ~d definition~:p across ~d file~:p (~a)"
                                         to count files line))
                                ((plusp count)
                                 (format nil "~a is live: ~d definition~:p across ~d file~:p; boot state waits for ~a (~a)"
                                         to count files launch (held-text held)))
                                (t
                                 (format nil "~a runs at ~a: no definition of it can go live here (~a)"
                                         to launch (held-text held)))))
                    (text (if displaced (format nil "~a; ~a" text (displaced-text displaced)) text)))
               (nlk:note-standing "update" (and (or held refused) text) :info)
               (ignore-errors
                (nlk::record-upgrade +gateway-session-id+ :from from :to to
                                     :mode (if refused "restart" "hot") :line line
                                     :held (if refused (list (cons "refused live" refused)) held)))
               ;; A shell folds what went live here; nothing did, and the
               ;; release is said at the boot that runs it. The model words
               ;; it off this lock.
               (unless refused
                 (announce-upgraded (cdr (first plans)) to text)
                 (when-let (note (ignore-errors (take-release from)))
                   (spawn "release-note" (say-note note))))
               ;; An upgrade lands definitions, not threads: a release that
               ;; lets the feed run where the build it replaced could not
               ;; (Windows before f286ec975) runs it now, not at the next
               ;; look, half an hour on.
               (ignore-errors (start-release-feed))
               (when restarts
                 (restart-when-idle to))
               (list* :applied (if (or held refused) :live :hot) :line text result)))))))))

;;; --- the restart a held release waits for ----------------------------------
;;;
;;; A shell's organism runs the held boot state at its next launch, which the
;;; operator makes by reopening it. The gateway its unit runs has no operator
;;; to: without this, it would name the new build and run the old boot state
;;; until the next login, reboot or crash. It exits once no turn is in flight
;;; instead (RESTART-WHEN-IDLE, the gateway's one exit, gateway.lisp), and the
;;; unit -- systemd's Restart=always, launchd's KeepAlive through the
;;; launcher -- starts it again on `current', which already names the new
;;; build.

;;; --- the hold --------------------------------------------------------------
;;;
;;; The one thing that keeps the poller off the channel is the operator naming
;;; a release: `nodecode update VERSION' holds this home there -- the way back
;;; after an update -- until `nodecode update' asks for the channel again. A
;;; restore holds nothing: it moves the operator's side alone, and the poller
;;; carries the kernel's forward (generation.lisp RESTORE-STEPS).

(defun hold-path ()
  "Where the release `nodecode update VERSION' holds this home at is kept."
  (nlk:home "release-pin"))

(defun hold-release (version)
  "Hold this home at release VERSION until `nodecode update' lifts it."
  (nlk::write-json-file (hold-path) "version" version "at" (nlk:iso-now)))

(defun held-release ()
  "The release this home is held at, or NIL."
  (when-let (hold (nlk::json-object-file (hold-path)))
    (json-value hold :string "version")))

(defun lift-hold (&aux (path (hold-path)))
  "Lift the hold. => whether one stood."
  (when (probe-file path)
    (delete-file path)
    t))

;;; --- going live between turns ------------------------------------------------
;;;
;;; A hot update evaluates a release's definitions one by one, and a turn that
;;; began among them would run on half of one build and half of another; so
;;; the move goes live while no turn can begin, and the poller's waits for a
;;; moment none runs; a release that leaves a provider this image served
;;; unresolved is undone whole and runs at the next boot instead.

(defun call-with-turns-held (function &key idle)
  "FUNCTION's values, called while no turn can begin: every admission writes
its start under the store's lock (lifecycle.lisp ADMIT-COMMAND), so holding
that lock holds them, and a turn already running waits at its next write.
Under IDLE, only once no turn runs at all, looked at again every
*IDLE-POLL-SECONDS*. The cell loader's lock comes first, the order a load
that records facts takes the two in."
  (loop (bt2:with-recursive-lock-held (nlk::*cell-load-lock*)
          (bt2:with-recursive-lock-held (nlk::*store-lock*)
            (unless (and idle (nlk:any-active-turn-p))
              (return (funcall function)))))
        (sleep *idle-poll-seconds*)))

(defun resolving-providers ()
  "The providers the shared config names whose lane this image resolves."
  (let ((providers (ignore-errors (json-value (read-shared-config) :object "providers"))))
    (when (hash-table-p providers)
      (loop for name being the hash-keys of providers
            when (ignore-errors (find-provider-lane name)) collect name))))

(defun providers-lost (serving)
  "Why this image no longer serves every provider SERVING named, or NIL."
  (when-let (lost (sort (set-difference serving (resolving-providers) :test #'string=) #'string<))
    (format nil "provider~p ~{~a~^, ~} no longer resolve~:[s~;~] here" (length lost) lost (rest lost))))

(defun stale-overrides ()
  "The layer's overrides whose shipped definition changed since they were
recorded, by file name: what its loader skips at boot (scribe.lisp
STALE-LAYER-FILE), the shipped definition standing in their place."
  (let ((directory (ignore-errors (nlk:layer-directory))))
    (when (and directory (probe-file directory))
      (loop for path in (directory (merge-pathnames "*.lisp" directory))
            when (ignore-errors (nlk::stale-layer-file path)) collect (pathname-name path)))))

(defun displaced-text (names)
  "The layer's overrides NAMES a release now stands over, as its line says them."
  (format nil "your override~p of ~{~a~^, ~} stand~:[s~;~] down: this release changed what ~
               ~:[it replaces~;they replace~], so its definition~:[~;s~] run~:[s~;~] until ~
               ~:[it is~;they are~] recorded again"
          (length names) names (rest names) (rest names) (rest names) (rest names) (rest names)))

(defun converge-between-turns (plans to allow-held idle)
  "NLK::CONVERGE of PLANS onto release TO while no turn can begin, under IDLE
once none runs (CALL-WITH-TURNS-HELD), undone whole when a provider this
image served stops resolving. => (values COUNT FILES REFUSED DISPLACED):
REFUSED why this image would not take it live, or NIL; DISPLACED the
layer's overrides the release now stands over."
  (let ((serving (resolving-providers))
        (stale (stale-overrides)))
    (when (and idle (nlk:any-active-turn-p))
      (update-note :info "~a is staged; it goes live the first moment no turn is running" to))
    (handler-case
        (multiple-value-bind (count files)
            (call-with-turns-held
             (lambda ()
               (nlk::converge plans :version to :source-root (cdr (first plans))
                                    :allow-held allow-held
                                    :check (lambda () (providers-lost serving))))
             :idle idle)
          (values count files nil (set-difference (stale-overrides) stale :test #'string=)))
      (nlk:upgrade-refusal (refusal) (values 0 0 (nlk:upgrade-refusal-detail refusal) nil)))))

;;; --- the poller ------------------------------------------------------------

(defvar *update-poller* nil)

(defun check-for-update ()
  "One look at the channel: nothing while the pointer names this image's own
label, or the release this image refused live and linked for its next boot;
else the update, applied in `auto' mode, staged and said in `ask' mode."
  ;; Also where the feed comes up: an upgrade lands definitions, not threads,
  ;; so a running gateway that began before there was a reader to start gets
  ;; one at the next look.
  (start-release-feed)
  (bind (((mode channel) (update-settings)) ((latest reason) (latest-release :channel channel)))
    ;; `nodecode update VERSION' holds this home at VERSION (UPGRADE-ORGANISM
    ;; :HOLD): the channel is left alone until `nodecode update' lifts it, and
    ;; the look still says what landed meanwhile, so a hold never hides one.
    (when-let (held (held-release))
      (update-note :info "release ~a is held by `nodecode update ~a'~@[; ~a is out~]; ~
                          `nodecode update' lifts the hold"
                   held held (and latest (string/= latest held) latest))
      (return-from check-for-update nil))
    (let ((way nil))
      (cond ((null latest)
             (update-note :warning "the ~a channel did not answer: ~a" channel reason))
            ((string= latest (effective-label)) (update-note :info))
            ((equal latest *refused-release*) nil)
            ;; Only forward: a channel naming an older build -- or one behind a
            ;; build no channel published, a checkout's own -- is said, never
            ;; taken.
            ((member (setf way (release-direction (effective-label) latest)) '(:backward :local))
             (update-note :info "~a is out; this build, ~a, is ~:[newer~;one no channel published~], ~
                                 so it stays: `nodecode update' takes ~a"
                          latest (effective-label) (eq way :local) latest))
            (t (handler-case (upgrade-organism :version latest :apply (string= mode "auto") :idle t)
                 (nlk:upgrade-refusal (condition) (update-note :warning "~a" condition))))))))

;;; --- the release feed -------------------------------------------------------
;;;
;;; The pointer is a file on the release host and everything above reads it
;;; when it looks. The feed is the other direction: one held connection to
;;; the host's feed route -- /api/release/feed, beside its /dl path -- which
;;; writes the label down the wire the second it moves. A label the feed
;;; names runs CHECK-FOR-UPDATE: the same whole policy the poller runs, at
;;; the moment instead of in six hours. The poller stays, and covers what
;;; the feed cannot: a host that serves no feed, a config that turned it
;;; off, and a connection that never lands.

(defvar *release-stream* nil
  "The release-feed reader, an NLK:WORKER, while it runs, or NIL.")

(defvar *release-stream-stopping* nil
  "T while the reader is being stopped.")

;;; A reconnect replays the pointer's own label; comparing against this is
;;; what keeps one release one poke, and NIL at start is what makes the first
;;; label a check even when nothing moved.
(defvar *release-stream-seen* nil
  "The label the feed last named.")

(defvar *feed-stream* nil
  "The open feed stream while a session reads it: what a stop closes to end
a read blocked on a quiet connection.")

(defvar *feed-open* 'feed-open
  "How one feed session is opened: a function of a URL answering a stream as
its first value, or (values NIL REASON). A test binds a stub.")

(defvar *release-poke* 'poke-update-check
  "What a label the feed names runs, a function of that label: the update
check, whole. A test binds a recorder.")

;;; The feed pings every twenty, so only a stalled socket ever reaches this.
(defparameter *feed-read-timeout* 60
  "Seconds the feed may go without a line -- or, opening, without its
headers -- before the connection is called dead.")

(defparameter *feed-retry-seconds* 5
  "Seconds between reconnects after the feed ends, doubling while it keeps
failing and capped at a minute.")

;;; Without this, a failure would say at every boot, a box's gateway
;;; serving before its DHCP lease, and at each drop of its line, each the
;;; condition's printed self (`Condition USOCKET:NS-TRY-AGAIN-ERROR was
;;; signalled.').
(defparameter *feed-outage-seconds* 60
  "How long the feed keeps failing before the reader says so: a box that
boots ahead of its network and a line that drops for a moment say nothing.")

(defun feed-failure-text (condition url)
  "Why a connection to the feed at URL failed, in words: a failure below HTTP
as TRANSPORT-FAILURE-LABEL names it, anything else as it says itself."
  (if (transport-failure-row condition)
      (transport-failure-label condition url)
      (princ-to-string condition)))

(defun feed-url (host channel &key (cache-buster (get-universal-time)))
  "The feed's URL for a release host at HOST and CHANNEL: the feed route
beside its /dl path, naming the pointer this platform reads."
  ;; The cache-buster is not decoration. A 404 at this path -- the web app
  ;; mid-recreate, the route not yet deployed -- is cacheable at the edge, and
  ;; a box that asked the same URL again would keep getting the old answer
  ;; back. A fresh one per connection costs a cache miss that was always going
  ;; to be a miss, and a successful answer is a stream no cache could hold
  ;; anyway.
  (format nil "~a/api/release/feed?pointer=~a-~a&cb=~a"
          (ppcre:regex-replace "(.)/dl\\z" (string-right-trim "/" host) "\\1")
          channel (release-platform) cache-buster))

(defun feed-open-socket (url &aux (uri (quri:uri url)) (host (quri:uri-host uri))
                                   (tls (equal "https" (quri:uri-scheme uri))))
  "GET URL over a socket of this image's own and answer its body as an octet
stream, chunked transfer undone; signals when the answer is not a 200."
  ;; TLS is cl+ssl's, which on Windows checks no certificate: it has no store
  ;; to check one against (cl-plus-ssl#54). What this stream can say is a
  ;; label, and a label only makes the box look: the pointer is then read
  ;; over the system's own TLS and the release fetched and checked against
  ;; its sha256 by the installer, so a forged label buys a look, never an
  ;; install.
  (let* ((socket (usocket:socket-connect host (or (quri:uri-port uri) (if tls 443 80))
                                         :element-type '(unsigned-byte 8) :timeout 10))
         (stream (usocket:socket-stream socket)))
    (with-handlers ((error (condition) (ignore-errors (usocket:socket-close socket)) (error condition)))
      (when tls
        (setf stream (cl+ssl:make-ssl-client-stream stream :hostname host)))
      (write-sequence
       (sb-ext:string-to-octets
        (format nil "GET ~a~@[?~a~] HTTP/1.1~C~CHost: ~a~C~CAccept: text/event-stream~C~CUser-Agent: nodecode~C~CConnection: close~C~C~C~C"
                (or (quri:uri-path uri) "/") (quri:uri-query uri) #\Return #\Newline host
                #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline
                #\Return #\Newline)
        :external-format :latin-1)
       stream)
      (finish-output stream)
      ;; The status line and the headers, a line at a time, to the blank one.
      (flet ((header-line ()
               (map 'string #'code-char (loop for byte = (read-byte stream nil nil)
                                              until (or (null byte) (= byte 10))
                                              unless (= byte 13) collect byte))))
        (let ((status (header-line)))
          (unless (search " 200" status)
            (error "the feed answered ~a" (nlk:trimmed status)))
          ;; Every header is read, to the blank line, whichever says chunked.
          (if (plusp (loop for line = (string-downcase (header-line))
                           until (zerop (length line))
                           count (and (uiop:string-prefix-p "transfer-encoding:" line)
                                      (search "chunked" line))))
              (let ((body (chunga:make-chunked-stream stream)))
                (setf (chunga:chunked-stream-input-chunking-p body) t)
                body)
              stream))))))

(defun feed-open (url)
  "Open URL as an event stream => the stream, or (values NIL REASON)."
  ;; Binary on purpose. dexador's character stream reads ahead for a fill that
  ;; a quiet feed -- one label, then twenty seconds of ping -- never gives it,
  ;; and the reader sits on octets the socket already had. The octets stream
  ;; answers with each one as it lands. Where dexador cannot stream at all
  ;; (FEED-STREAMS-P) the image's own socket carries it.
  ;;
  ;; The headers are read under a deadline for the reason READ-EVENTS reads
  ;; each line under one: over TLS, dexador's :read-timeout bounds nothing.
  (with-handlers ((sb-sys:deadline-timeout ()
                    (values nil (format nil "no answer in ~a seconds" *feed-read-timeout*)))
                  (error (condition) (values nil (feed-failure-text condition url))))
    (values (nth-value 0 (sb-sys:with-deadline (:seconds *feed-read-timeout*)
                           (if (feed-streams-p)
                               (dex:get url :want-stream t :force-binary t
                                            :connect-timeout 10
                                            :use-connection-pool nil)
                               (feed-open-socket url))))
            nil)))

(defun feed-note-label (payload &optional (poke *release-poke*) &aux (label (nlk:trimmed payload)))
  "One event's payload: the label it names, when it is not the one already
seen, is a poke at the update check."
  (when (and (plusp (length label)) (not (equal label *release-stream-seen*)))
    (setf *release-stream-seen* label)
    (funcall poke label)))

;;; The check itself is cheap and idempotent; the pause is what keeps a broken
;;; release from being asked about in a loop. An empty list holds one attempt.
(defparameter *poke-retry-seconds* '(30 60)
  "How long a poke waits before asking again, when the check did not land
the label the feed named -- a tarball the edge is not serving yet, a staging
that failed.")

(defun poke-update-check (label)
  "A label the feed named: the update check, whole."
  ;; The check reads the pointer itself, so a label two publishes stale still
  ;; lands on the latest.
  ;;
  ;; When the check does not land it -- the release is not servable yet, or
  ;; staging failed -- the same check runs again after a pause, so a box that
  ;; heard about a release before it could fetch it does not sit on the older
  ;; build until the next publish.
  (declare (ignore label))
  (check-for-update)
  (loop for wait in *poke-retry-seconds*
        until (or (equal *release-stream-seen* (effective-label))
                  *release-stream-stopping*)
        do (sleep wait)
           (unless *release-stream-stopping*
             (check-for-update))))

(defun release-feed-session (url &key open poke)
  "One connection's whole life: open URL, read every event, close."
  ;; => (values
  ;; OK REASON); OK is NIL when the feed could not be opened or ended in error.
  ;; OPEN and POKE default to the current seams, so a caller that wants the ones
  ;; it began with passes them.
  (bind ((open (or open *feed-open*)) (poke (or poke *release-poke*))
         ((stream reason) (funcall open url)))
    (if (null stream)
        (values nil reason)
        (with-cleanup ((ignore-errors (close stream)) (setf *feed-stream* nil))
          ;; The host ends a held feed when it will, and over TLS that comes
          ;; as END-OF-FILE out of the read -- cl+ssl's, for a close with no
          ;; goodbye -- not as the stream ending: a session that ended, and
          ;; the reader is back in five seconds. Without this handler, a
          ;; normal end would be taken for a failure and shown as a red toast
          ;; in the operator's shell.
          (with-handlers ((end-of-file () (values t nil))
                          (error (condition) (values nil (feed-failure-text condition url))))
            (setf *feed-stream* stream)
            ;; Octets one at a time: FEED-OPEN says why.
            (nlk:read-events stream (lambda (payload) (feed-note-label payload poke) nil)
                             :idle *feed-read-timeout*)
            (values t nil))))))

(defun start-release-feed ()
  "Start the reader unless one runs, the update mode is off, the config
turned the feed off, or this build's HTTP cannot stream. One per home: the
gateway's, beside the poller."
  ;; The reader holds this home's feed open: connect, read, reconnect -- the
  ;; delay doubling five to sixty seconds while it keeps failing, and starting
  ;; over at five the next time a connection lands. The URL, the opener and
  ;; the poke are read once, when the reader starts: a thread that outlived
  ;; whatever rebound them keeps the seams it began with. An outage is said
  ;; once, when it has lasted *FEED-OUTAGE-SECONDS*, and not again until a
  ;; connection lands: in a shell that owns its organism each saying is a
  ;; toast, and a reason names the connection's own socket, so no two read
  ;; alike.
  (bind (((mode _ _ _ feed) (update-settings)))
    (unless (or (string= mode "off") (string= feed "off") *release-stream*)
      (let ((open *feed-open*) (poke *release-poke*) (delay *feed-retry-seconds*)
            (outage (* *feed-outage-seconds* internal-time-units-per-second))
            (since nil) (said nil))
        (setf *release-stream-seen* nil
              *release-stream-stopping* nil
              *release-stream*
              (worker-start
               "release-feed"
               (lambda ()
                 (multiple-value-bind (ok reason)
                     ;; An error here -- a config that will not read, a URL
                     ;; that will not parse -- is a failed connection.
                     (handler-case
                         (bind (((_ channel _ _ feed) (update-settings)))
                           (release-feed-session (feed-url feed channel) :open open :poke poke))
                       (error (condition) (values nil (feed-failure-text condition nil))))
                   (if ok
                       (setf since nil said nil)
                       (let ((now (get-internal-real-time)))
                         (unless since (setf since now))
                         ;; A session that ended well carries no reason.
                         (when (and reason (not said) (not *release-stream-stopping*)
                                    (>= (- now since) outage))
                           (setf said t)
                           (ignore-errors
                            (format *error-output* "~&release feed: ~a~%" reason)))))
                   (setf delay (if ok *feed-retry-seconds* (min 60 (* 2 delay))))))
               :wake (lambda () delay) :prime t))))))

(defun stop-release-feed ()
  "End the reader: close the stream a read is blocked on, set the latch and
wake its wait, and give the thread a moment to leave."
  (when *release-stream*
    (setf *release-stream-stopping* t)
    (ignore-errors (close *feed-stream*))
    (setf *release-stream* (worker-stop *release-stream*) *release-stream-stopping* nil)))

;;; The cadence is six hours, and a box that booted on an older build should
;;; not wait one to hear about the current release -- which is also the whole
;;; path when the feed never lands.
(defparameter *update-poller-first-look-seconds* 30
  "How long after the gateway starts the poller takes its first look at the
channel.")

(defun start-update-poller ()
  "Look at the channel every `check_minutes' unless the config says off,
and hold the release feed while it is on. START-GATEWAY's."
  (bind (((mode _ minutes) (update-settings))
         (wait *update-poller-first-look-seconds*))
    (unless (string= mode "off")
      (start-release-feed)
      (unless *update-poller*
        (setf *update-poller*
              (worker-start "update-poller"
                            (lambda ()
                              (ignore-errors (check-for-update))
                              (setf wait (* 60 minutes)))
                            :wake (lambda () wait)))))))

(defun stop-update-poller ()
  (setf *update-poller* (worker-stop *update-poller* :timeout nil))
  (stop-release-feed))

;;; --- the note a new release says --------------------------------------------
;;;
;;; A release that reached this home is news to everyone who uses it: the
;;; operator at a shell, the people in a room, the model itself. Without this,
;;; a release would say nothing: a hot update would clear the board, and a
;;; gateway whose unit restarted would come back with only "Back." The organism
;;; says each release once, the first time it runs it: the version, and what
;;; changed since the release this home ran before, read from the list the
;;; publisher writes beside the archives (publish.sh: changes-<label>, a line
;;; per commit newest first, its head when it changed what ships, its hash
;;; alone when it did not). The note stands on the board under `release',
;;; where the model reads it once per conversation, and the model tells it in
;;; its own words (gateway.lisp SAY-NOTE) -- with the boot's note of how it
;;; came back, at the boot that runs it; alone, after a hot update -- to
;;; every attached shell and a channel cell's home room. Which release this
;;; home last said is kept in the gateway's own state, so a restart onto the
;;; same build says nothing.

(defparameter +release-key+ "release"
  "The gateway state key the release this home last said is kept under, and
the board key its note stands under.")

(defparameter *release-note-changes* 12
  "How many changes a release note names; the rest are counted.")

(defvar *release-list* nil
  "The last list the release host answered, (LABEL . LINES): a list never
changes once published, so a release's is fetched once a run.")

(defun release-list (to &aux (kept *release-list*))
  "The release host's changes-TO list: (LABEL . HEAD) a commit, newest first,
HEAD NIL for one that changed nothing that ships; NIL when the host has no
list for TO."
  (if (equal (car kept) to)
      (cdr kept)
      (let ((text (ignore-errors (funcall *release-fetch* (format nil "~a/changes-~a" (release-url) to)))))
        (when (stringp text)
          (let ((lines (loop for line in (uiop:split-string text :separator '(#\Newline #\Return))
                             for space = (position #\Space line)
                             for label = (nlk:trimmed (subseq line 0 space))
                             for head = (and space (nlk:trimmed (subseq line space)))
                             when (plusp (length label))
                               collect (cons label (and (plusp (length head)) head)))))
            (setf *release-list* (cons to lines))
            lines)))))

(defun release-direction (from to)
  "Which way moving from release FROM to release TO goes, read off the release
host's changes lists (publish.sh: the 200 commits up to a release, newest
first): :FORWARD when FROM is on TO's list, :BACKWARD when TO is on FROM's,
:LOCAL when the host has no list for FROM -- a build no channel published, a
checkout's own -- and :UNKNOWN when nothing says: TO has no list, or the two
are further apart than a list reaches."
  (let ((onto (release-list to)))
    (cond ((null onto) :unknown)
          ((find from onto :key #'car :test #'equal) :forward)
          (t (let ((back (release-list from)))
               (cond ((null back) :local)
                     ((find to back :key #'car :test #'equal) :backward)
                     (t :unknown)))))))

(defun release-changes (from to)
  "What changed from release FROM to release TO: the heads the release host's
changes-TO list carries above FROM, newest first; all of them when FROM is not
on it. => (values HEADS MORE), MORE the count past *RELEASE-NOTE-CHANGES*; NIL
when the host has no list for TO."
  (when-let (heads (loop for (label . head) in (release-list to)
                         until (equal label from)
                         when head collect head))
    (values (subseq heads 0 (min (length heads) *release-note-changes*))
            (max 0 (- (length heads) *release-note-changes*)))))

(defun release-note (from to)
  "What release TO says to a home that ran FROM before it: the version, then
what changed, a line each."
  (multiple-value-bind (heads more) (release-changes from to)
    (format nil "nodecode updated to ~a (from ~a)~{~%- ~a~}~@[~%- and ~d more~]~
                 ~@[~%`nodecode update ~a' goes back to it and holds there~]"
            (effective-version) from heads (and more (plusp more) more)
            (and (probe-file (release-version-directory from)) from))))

(defun said-release (&optional (key "label"))
  "The release this home last said, or NIL; with KEY \"from\", the release
it ran before that one, when it said what changed since."
  (ignore-errors
   (json-value (nlk:decode-json (nlk:session-state-get "gateway" +release-key+)) :string key)))

(defun take-release (&optional from &aux (to (effective-label)))
  "The note on the release this organism runs, for the model to say, once:
the first time it runs one other than the release this home last said, or
than FROM, the one an update just moved it from; it stands on the board from
then. A home's first release is kept, not said, and an update mode of off
says nothing. The caller holds *UPGRADE-LOCK*. => the note, or NIL."
  (when (and (nlk:store-open-p) (string/= "off" (update-settings)))
    (bind ((said (said-release)) (from (or from said)))
      (unless (equal said to)
        (let ((before (and from (string/= from to) from)))
          (nlk:session-state-put "gateway" +release-key+
                                 (encode-json-object (json-object "label" to :opt "from" before)))
          (when before
            ;; The board is the model's: no shell draws a :quiet entry, and
            ;; the model's words of it reach them instead, said once.
            (notice (release-note before to) :key +release-key+ :level :quiet)))))))

(defun boot-release ()
  "TAKE-RELEASE for the boot that runs it."
  (bt2:with-lock-held (*upgrade-lock*) (take-release)))

(defun release-json (&aux (to (effective-label)))
  "The release this organism runs and what it changed, as a shell's Latest
changes reads it: its LABEL, the release this home ran before it (FROM) once
it said what changed since, and every change since then -- every one on the
release host's list when it knows no FROM -- newest first; LISTED false when
the host has no list for LABEL."
  (let ((from (and (equal to (said-release)) (said-release "from")))
        (lines (release-list to)))
    (json-object "label" to
                 :opt "from" from
                 "listed" (if lines t :false)
                 "changes" (coerce (loop for (label . head) in lines
                                         until (equal label from)
                                         when head collect (json-object "label" label "head" head))
                                   'vector))))

(define-gateway-route handle-gateway-release-http (env)
  (:route :get "/api/gateway/release" "release")
  (release-json))

;;; --- the frame to shells, and a shell's fold -------------------------------

(defun announce-upgraded (root version line)
  "Every attached shell is told the organism's image now
reads itself from ROOT at VERSION: one organism.upgraded frame; each folds
it by converging its own image on the same tree (CONVERGE-SHELL)."
  ;; => how
  ;; many were told.
  (or (ignore-errors
       (nth-value 1
                  (broadcast-organism
                   "organism.upgraded"
                   (json-object "root" (namestring root) "version" version "line" line))))
      0))

(defun converge-shell (root version)
  "An attached shell's fold of organism.upgraded: this image classifies its
own tree against ROOT, the organism's new self-view, and converges when it
can. => the line the shell shows, or NIL when there is nothing to say."
  (with-handlers ((error (condition)
                    (format nil "this shell could not converge: ~a" condition)))
    (let ((own nlk:*source-root*)
          (root (truename root)))
      (when (and own (not (equal (namestring (truename own)) (namestring root))))
        (bind ((plans (release-plans-at
                       (uiop:pathname-parent-directory-pathname
                        (uiop:pathname-parent-directory-pathname root))))
               (verdict (plans-verdict plans))
               ((count files) (nlk::converge plans :version version :source-root root
                                                   :allow-held (eq verdict :restart))))
          (ecase verdict
            (:same "same definitions; now reading itself from the new tree")
            (:hot (format nil "~d definition~:p across ~d file~:p now live" count files))
            (:restart
             (format nil "~d definition~:p across ~d file~:p now live; boot state waits for the next launch (~a)"
                     count files (held-text (plans-held plans))))))))))

;;; --- the verb --------------------------------------------------------------

(defun update-locally (version check &aux (result (upgrade-organism :version version :check t)))
  "`nodecode update' with no organism serving: stage, classify, link."
  ;; The next launch runs the release.
  (cond ((or check (eq :current (getf result :applied))) (update-answer-text result))
        (t (link-release (getf result :to))
           ;; the hold the organism's own update takes (UPGRADE-ORGANISM :HOLD)
           (if version (hold-release (getf result :to)) (lift-hold))
           (format nil "~a is current; no organism is running here, so the next launch runs it (~a)"
                   (getf result :to) (getf result :line)))))

(defun update-text (words)
  "`nodecode update [VERSION] [--check]': the organism serving this home
moves to VERSION, the channel's latest by default -- through its route
when one serves, else by staging and linking here for the next launch."
  ;; --check stages and classifies only. => the answer.
  (if (intersection '("--help" "-h" "help") words :test #'string=)
      (nlk:verb-help-text (nlk:find-verb "update" *launch-verbs*) :command "nodecode")
      (let* ((check (member "--check" words :test #'string=))
             (version (find-if-not (lambda (word) (uiop:string-prefix-p "--" word)) words))
             (port (organism-port)))
        (if port
            (let* ((answer (gateway-http-session-request
                            port :post "/api/gateway/update"
                            (json-object "version" (or version :null) "check" (and check t))
                            (gateway-configured-operator-token)
                            :connect-timeout 3 :read-timeout 900))
                   (text (answer-text answer))
                   (organism (json-value answer :string "to"))
                   (binary (effective-label)))
              ;; This answer is the organism's and `--version' the binary's, so a
              ;; checkout's ./nodecode, which never updates, read as a contradiction:
              ;; `already at ...' beside `nodecode ...'.
              ;; When the two differ, each line says whose it is.
              (if (and organism (not (equal organism binary)))
                  (format nil "this home's organism: ~a~%this binary: ~a~:[~;, a checkout build (`just release' rebuilds it)~]"
                          text binary (not *installed-build*))
                  text))
            (update-locally version check)))))

;;; --- the route -------------------------------------------------------------

(define-gateway-route handle-gateway-update-http (env)
  "POST /api/gateway/update {version?, check?}: the organism's update,
applied; the answer carries what happened and the sentence the verb prints."
  (:route :post "/api/gateway/update" "update")
  (let* ((body (gateway-request-json env))
         (version (json-value body :string "version"))
         (result (upgrade-organism :version version :hold (and version t)
                                   :check (eq t (json-value body :any "check")) :apply t)))
    (json-object (:getf result "from" "to")
                 "verdict" (string-downcase (symbol-name (getf result :verdict)))
                 "applied" (string-downcase (symbol-name (getf result :applied)))
                 "text" (update-answer-text result)
                 "held" (map 'vector (lambda (entry) (format nil "~a — ~a" (car entry) (cdr entry)))
                             (getf result :held)))))
