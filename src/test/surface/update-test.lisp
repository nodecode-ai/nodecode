;;;; update-test.lisp --- a shipped release reaches the running organism (update.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A release host on disk: tarballs the real installer fetches over
;;;; file://, checks against their sha256 and lays out beside a scratch
;;;; `current'. The organism under test is the gateway fixture, its
;;;; self-view moved to a scratch tree so what converges is the fixture's
;;;; own function and nothing of this image. The route handler runs on the
;;;; server thread, so the seams it reads are set globally, and put back.

(in-package #:nodecode.test)

(defun write-release-tarball (dist version files &aux (name (format nil "nodecode-~a-~a" version (nle::release-platform)))
                                                      (work (temp-path "release-work"))
                                                      (tree (merge-pathnames (format nil "~a/" name) work)))
  "nodecode-VERSION-<platform>.tar.gz under DIST with its sha256 sidecar: an
executable `nodecode' answering --help with the usage line, and FILES
((RELATIVE . TEXT) ...) under it."
  (write-upgrade-tree tree (cons (cons "nodecode" "#!/bin/sh
echo 'usage: nodecode [-p NAME] [COMMAND] [OPTIONS]'
") files))
  (sb-posix:chmod (namestring (merge-pathnames "nodecode" tree)) #o755)
  (ensure-directories-exist dist)
  (let ((tarball (uiop:native-namestring (merge-pathnames (format nil "~a.tar.gz" name) dist))))
    (uiop:run-program (list "tar" "-czf" tarball "-C" (uiop:native-namestring work) name))
    (let ((digest (uiop:run-program (list "sha256sum" tarball) :output :string)))
      (write-temp-file (merge-pathnames (format nil "~a.tar.gz.sha256" name) dist)
                       (format nil "~a  ~a.tar.gz~%" (subseq digest 0 64) name))))
  (ignore-errors (uiop:delete-directory-tree work :validate t))
  name)

(defun upg-source (answer slots)
  "The fixture's src/src/a.lisp: UPG-ROUTE-F answering ANSWER, UPG-ROUTE-S holding SLOTS."
  (format nil "(in-package #:nodecode.evolved)~%(defun upg-route-f () ~d)~%~
               (defstruct upg-route-s ~a)~%" answer slots))

(defparameter *update-old-a* (upg-source 1 "a"))

(defun release-files (a-text)
  (list (cons "src/src/a.lisp" a-text)
        (cons "src/nodecode.asd" "(defsystem \"upg\" :depends-on (\"uiop\") :components ((:file \"a\")))")))

(defmacro on-url (&body body)
  "A function of a URL, which BODY does not read: a stand-in release host or feed."
  `(lambda (url) (declare (ignore url)) ,@body))

(defmacro with-release-host ((dist releases) &body body)
  "A release host at DIST (file://) carrying RELEASES ((VERSION . FILES)
...), the installer's layout under a scratch release directory, and the
repo's own installer, for BODY."
  `(with-temp-trees ((,dist "dist") (releases-dir "releases"))
     (loop for (version . files) in ,releases
           do (write-release-tarball ,dist version files))
     (with-saved-globals ((nle::*release-url*
                           (format nil "file://~a" (string-right-trim "/" (namestring ,dist))))
                          (nle::*release-fetch* 'nle::release-fetch)
                          (nle::*release-directory* releases-dir)
                          (nle::*installer-path*
                           (asdf:system-relative-pathname "nodecode" "tools/install.sh")))
       ,@body)))

(deftest update-installer-stages-then-links ()
  (with-release-host (dist (list (cons "tv1" (release-files *update-old-a*))))
    (let ((directory (nle::stage-release "tv1")))
      (is (probe-file (merge-pathnames "nodecode" directory)) "the version is laid out")
      (is (probe-file (merge-pathnames "src/src/a.lisp" directory)) "with its source tree")
      (is (null (probe-file (merge-pathnames "current" nle::*release-directory*)))))
    (is (nle::link-release "tv1"))
    (is (equal (namestring (truename (merge-pathnames "tv1/" nle::*release-directory*)))
               (namestring (truename (merge-pathnames "current" nle::*release-directory*)))))
    (is-carrying (text (refusal-text nlk:upgrade-refusal (nle::stage-release "no-such")))
      ("no-such" "a release the host lacks is refused by name"))))

(deftest update-installer-failure-reads-its-last-line-past-crlf ()
  ;; install.ps1 ends its lines in CRLF: the reason is the last line's text,
  ;; never the bare CR after it.
  (is (equal "sha256 mismatch" (nle::last-line (format nil "staging~C~%sha256 mismatch~C~%" #\Return #\Return))))
  (is (equal "done" (nle::last-line (format nil "a~%done~%")))))

(deftest update-windows-installer-reads-5.1s-own-modules ()
  ;; Started from PowerShell 7, 5.1 found no Get-FileHash and every staging failed.
  (flet ((child-environment (platform &aux environment)
           (with-saved-globals ((nle::*installer-path*
                                 (asdf:system-relative-pathname "nodecode" "tools/install.ps1"))
                                (nle::*release-directory* (temp-path "releases")))
             (with-stubbed-fdefinitions ((nlk:platform () platform)
                                         (nle::run-bounded (argv &key ((:environment handed)) &allow-other-keys)
                                           (declare (ignore argv))
                                           (setf environment handed)
                                           (values "" "" 0)))
               (is (nle::run-installer "stage" "tv1" "staging"))))
           environment))
    (is (member "PSModulePath=" (child-environment :windows) :test #'string=) "the Windows installer's child gets no inherited PSModulePath")
    (is (notany (lambda (entry) (search "PSModulePath" entry)) (child-environment :linux)) "install.sh's child is handed nothing of PowerShell's")))

(deftest update-polls-every-half-hour-where-no-feed-streams ()
  ;; A box the feed cannot reach hears of a release only from the poller.
  (with-saved-globals ((nle::*installed-build* t))
    (is (= 360 (nth-value 2 (nle::update-settings))))
    (let ((dex:*dexador-backend* :winhttp))
      (is (= 30 (nth-value 2 (nle::update-settings)))))))

(deftest update-latest-release-reads-the-channel-pointer ()
  (with-saved-globals ((nle::*release-fetch* (lambda (url) (if (search "beta-" url) "  beta9
" "stable7"))))
    (is (equal "stable7" (nle::latest-release)))
    (is (equal "beta9" (nle::latest-release :channel "beta"))))
  (with-saved-globals ((nle::*release-fetch*
                         (on-url (values nil "HTTP 404"))))
    (is-values (version reason) (nle::latest-release)
      (version null) ((search "HTTP 404" reason) is))))

(deftest update-a-release-label-never-names-a-folder-above-its-own ()
  ;; A label becomes a folder under the release directory that a reinstall
  ;; replaces whole: `.' or `..' named that directory or its parent.
  (with-saved-globals ((nle::*release-fetch* (on-url "..")))
    (is-values (version reason) (nle::latest-release)
      (version null) ((search "names no version" reason) is)))
  (is-carrying (text (refusal-text nlk:upgrade-refusal (nle::release-target "..")))
    (".. is not a release label" "a label the operator names is refused by the same rule"))
  (dolist (label (list "." ".." ".hidden" "../up" "a/b" "a\\b" "" (format nil "t~Cst" (code-char 233))))
    (is (not (nle::release-label-p label)) label))
  (dolist (label '("2ec27b3f4" "tv1" "0.0.1+2ec27b3f4" "beta-9_x"))
    (is (nle::release-label-p label) label)))

(deftest update-a-release-host-is-https-or-a-folder ()
  ;; Over plain http anyone on the path could swap the pointer, the tarball
  ;; and its sidecar; install.sh and install.ps1 refuse such a host the same way.
  (is-values (body reason) (nle::release-fetch "http://127.0.0.1:9/dl/stable-x86_64-linux")
    (body null) ((search "https://" reason) is))
  (is (nle::release-host-p "https://nodecode.ai/dl"))
  (is (nle::release-host-p "file:///srv/releases"))
  (is (not (nle::release-host-p "http://nodecode.ai/dl")))
  (with-saved-globals ((nle::*installed-build* t) (nle::*release-url* "http://mirror.example/dl"))
    (is (equal "off" (nth-value 4 (nle::update-settings))) "an http host gets no feed")))

(deftest update-reads-a-release-host-that-is-a-folder (with-release-host (dist nil))
  ;; The installer takes a file:// host; without this, the organism would read
  ;; it as a socket.
  (write-temp-file (merge-pathnames (format nil "stable-~a" (nle::release-platform)) dist) (format nil "tv9~%"))
  (with-saved-globals ((nle::*installed-build* t))
    (is (equal "tv9" (nle::latest-release)))
    (is (equal "off" (nth-value 4 (nle::update-settings))) "a folder holds no feed")
    (is (= 30 (nth-value 2 (nle::update-settings))) "so the poller looks every half hour")))

(deftest update-settings-default-to-auto-stable ()
  (with-saved-globals ((nle::*installed-build* t))
    (is-values (mode channel minutes url) (nle::update-settings) (mode "auto") (channel "stable")
      (minutes eql 360)
      (url nle::*release-url* "the release host is the parameter's unless the config says")))
  (with-saved-globals ((nle::*installed-build* nil))
    (is (equal "off" (nle::update-settings)) "a build outside the installer's layout -- a checkout's -- never updates itself unasked")))

(defmacro with-organism-tree ((&optional version text) &body body)
  "This image's self-view moved to a scratch tree holding *UPDATE-OLD-A* as
src/src/a.lisp, globally, for BODY; the fixture's function defined as the old
tree says. With VERSION, a release host carrying that release, TEXT its
src/src/a.lisp."
  `(with-release-host (dist ,(and version `(list (cons ,version (release-files ,text)))))
     (with-temp-trees ((old "organism-old"))
       (nlk:with-cleanup ((fmakunbound 'nodecode.evolved::upg-route-f))
         (write-upgrade-tree old (list (cons "src/src/a.lisp" *update-old-a*)))
         (eval '(defun nodecode.evolved::upg-route-f () 1))
         (with-saved-globals ((nlk::*source-root* (truename (merge-pathnames "src/src/" old)))
                              (nlk::*shipped-definitions* nil)
                              (nlk::*effective-version* nil))
           ,@body)))))

(deftest update-route-converges-a-hot-release (with-organism-tree ("tv2" (upg-source 2 "a")))
  (with-temp-gateway (port)
    (is (nlk::record-upgrade nle::+gateway-session-id+ :from "a" :to "b" :mode "hot" :line "probe"))
    (with-gateway-http (port :post "/api/gateway/update"
                             :body "{\"version\": \"tv2\", \"check\": true}" :status 200)
      (is-shape body ((:string "applied") "checked") ((:string "verdict") "hot")))
    (is (eql 1 (funcall 'nodecode.evolved::upg-route-f)) "a check evaluates nothing")
    (with-gateway-http (port :post "/api/gateway/update" :body "{\"version\": \"tv2\"}")
      (is (= 200 status))
      (is (equal "hot" (nlk::json-value body :string "applied")))
      (is (search "tv2 is live: 1 definition" (nlk::json-value body :string "text"))))
    (is (eql 2 (funcall 'nodecode.evolved::upg-route-f)) "the release's definition is live")
    (is (equal "tv2" nlk::*effective-version*))
    (is (equal "tv2" (nle::held-release)) "a release the operator names holds this home there")
    (is (search "/tv2/" (namestring nlk::*source-root*)) "the self-view is the staged tree")
    (is (probe-file (merge-pathnames "current" nle::*release-directory*)) "current moved")
    (with-gateway-http (port :get "/api/gateway/identity")
      (is (uiop:string-suffix-p (nlk::json-value body :string "version") "+tv2"))
      (is (equal nle::*user-agent-version* (nlk::json-value body :string "build"))))
    (is (nlk::events :kind nlk::+kind-organism-upgraded+ :as :exists))
    (with-gateway-http (port :post "/api/gateway/update" :body "{\"version\": \"tv2\"}")
      (is (equal "current" (nlk::json-value body :string "applied"))))))

(deftest update-applied-release-brings-the-feed-up-at-once (let ((started 0)))
  ;; A box whose build had no feed hears the next release in seconds only once
  ;; the reader runs: that is the moment the upgrade lands, not the next look.
  (with-organism-tree ("tv3" (upg-source 2 "a"))
    (with-stubbed-fdefinition (nle::start-release-feed () (incf started))
      (is (eq :hot (getf (nle::upgrade-organism :version "tv3" :apply t) :applied)))))
  (is (= 1 started) "the reader is started as the release goes live"))

(deftest update-route-upgrades-the-live-image-when-the-release-holds-boot-state ()
  ;; A release that holds boot state still reaches the live image: every
  ;; definition a live image can take goes live now -- no restart, no idle
  ;; wait -- `current' moves so the next launch runs the whole new build,
  ;; and the held boot state is named, on the answer and on the record.
  (with-organism-tree ("tv3" (upg-source 2 "a b"))
    (with-temp-gateway (port)
      (with-gateway-http (port :post "/api/gateway/update" :body "{\"version\": \"tv3\"}")
        (is (= 200 status))
        (is (equal "live" (nlk::json-value body :string "applied")))
        (is (search "tv3 is live: 1 definition" (nlk::json-value body :string "text")))
        (is (search "defstruct" (nlk::json-value body :string "text")) "the held reason is named"))
      (is (eql 2 (funcall 'nodecode.evolved::upg-route-f)) "the release's definition is live")
      (is (equal "tv3" nlk::*effective-version*))
      (let ((current (truename (merge-pathnames "current" nle::*release-directory*))))
        (is (equal "tv3" (car (last (pathname-directory current)))) "current names the new build"))
      (let ((record (nlk::events :kind nlk::+kind-organism-upgraded+
                                 :order :newest :as :payload)))
        (is (equal "hot" (gethash "mode" record)))
        (is (plusp (length (gethash "held" record))) "what is held rides the record"))
      (let ((notice (board-line "update")))
        (is (search "boot state waits for the next launch" notice) "the board says what waits"))
      (with-gateway-http (port :post "/api/gateway/update" :body "{\"version\": \"tv3\"}")
        (is (equal "current" (nlk::json-value body :string "applied")))))))

(deftest update-held-release-restarts-a-unit-gateway-once-idle (let ((exited '()) (busy t)))
  ;; The gateway a unit runs has no operator to reopen it: past the release's
  ;; live definitions it waits out the turn in flight, then exits, and the
  ;; unit starts it again on `current' -- the new build, boot state and all.
  (with-organism-tree ("tv4" (upg-source 2 "a b"))
    (with-saved-globals ((nle::*gateway-run* t) (nle::*idle-poll-seconds* 0.05)
                         (nle::*restart-wait* nil)
                         (nle::*restart-exit* (lambda (to) (push to exited))))
      (with-stubbed-fdefinitions ((nlk:gateway-installed-p (&optional home) t)
                                  (nlk:any-active-turn-p () busy)
                                  (nle::start-release-feed () nil))
        (let ((result (nle::upgrade-organism :version "tv4" :apply t)))
          (is (eq :live (getf result :applied)))
          (is-carrying (text (getf result :line))
            "tv4 is live: 1 definition"
            "boot state waits for the gateway's restart"))
        (is (eql 2 (funcall 'nodecode.evolved::upg-route-f)) "the live definitions land first")
        (sleep 0.3)
        (is (null exited) "not while a turn is in flight")
        (setf busy nil)
        (bt2:join-thread nle::*restart-wait*)
        (is (equal '("tv4") exited) "then once, for the new build")))))

(deftest update-refused-release-runs-at-the-restart (let ((exited '()) (asked 0)))
  ;; What the classifier sends hot and this image will not evaluate is boot
  ;; state after all: the image is as it was, `current' moves, the unit's
  ;; gateway restarts onto it once idle, and the poller leaves the release be.
  ;; Refused and left there, a release would strand a box.
  (with-organism-tree ("tv5" (format nil "~a(defmethod upg-route-g ((x upg-no-such-class)) x)~%"
                                     (upg-source 2 "a")))
    (with-saved-globals ((nle::*gateway-run* t) (nle::*idle-poll-seconds* 0.05)
                         (nle::*restart-wait* nil) (nle::*refused-release* nil)
                         (nle::*restart-exit* (lambda (to) (push to exited))))
      (with-stubbed-fdefinitions ((nlk:gateway-installed-p (&optional home) t)
                                  (nlk:any-active-turn-p () nil)
                                  (nle::start-release-feed () nil))
        (let ((result (nle::upgrade-organism :version "tv5" :apply t)))
          (is-shape result (:verdict eq :hot "the classifier sent it hot") (:applied eq :live))
          (is-carrying (text (getf result :line))
            "tv5 runs at the gateway's restart"
            "upg-route-g"))
        (is (eql 1 (funcall 'nodecode.evolved::upg-route-f)) "the image is as it was")
        (is (null nlk::*effective-version*))
        (let ((current (truename (merge-pathnames "current" nle::*release-directory*))))
          (is (equal "tv5" (car (last (pathname-directory current)))) "current names the new build"))
        (is (search "refused it live" (board-line "update")))
        (bt2:join-thread nle::*restart-wait*)
        (is (equal '("tv5") exited) "the unit's gateway restarts onto it")
        (with-saved-globals ((nle::*release-fetch* (on-url "tv5")))
          (with-stubbed-fdefinition (nle::upgrade-organism (&rest args) (incf asked))
            (is (null (nle::check-for-update)))))
        (is (zerop asked) "the poller does not refuse it again")))))

(deftest update-converge-shell-folds-the-new-tree (with-organism-tree ())
  (with-temp-trees ((new "shell-new"))
    (write-upgrade-tree new (list (cons "src/src/a.lisp" (upg-source 5 "a"))))
    (let* ((root (truename (merge-pathnames "src/src/" new)))
           (line (nle::converge-shell (namestring root) "tv5")))
      (is (equal "1 definition across 1 file now live" line))
      (is (eql 5 (funcall 'nodecode.evolved::upg-route-f)))
      (is (equal "tv5" nlk::*effective-version*))
      (is (null (nle::converge-shell (namestring root) "tv5"))))))

(deftest release-platform-is-a-name-pack-sh-gives-a-tarball ()
  ;; Not an echo of the function: the two halves are the arms of pack.sh's
  ;; case statements, so without this a NIL from the CPU read would fail on
  ;; the wire, not here.
  (let* ((platform (nle::release-platform))
         (dash (position #\- platform)))
    (is (member (subseq platform 0 dash) '("x86_64" "arm64") :test #'equal) platform)
    (is (member (subseq platform (1+ dash)) '("linux" "darwin" "windows") :test #'equal) platform)))

(deftest update-feed-url-sits-beside-the-release-host (let ((platform (nle::release-platform))))
  (is (equal (format nil "https://nodecode.ai/api/release/feed?pointer=stable-~a&cb=t1" platform)
             (nle::feed-url "https://nodecode.ai/dl" "stable" :cache-buster "t1")))
  (is (equal (format nil "https://mirror.example/api/release/feed?pointer=beta-~a&cb=t2" platform)
             (nle::feed-url "https://mirror.example/" "beta" :cache-buster "t2")))
  (is (not (equal (nle::feed-url "https://nodecode.ai/dl" "stable" :cache-buster "t1")
                  (nle::feed-url "https://nodecode.ai/dl" "stable" :cache-buster "t2")))))

(defmacro with-idle-feed ((&rest places) &body body)
  "WITH-SAVED-GLOBALS over PLACES with no release-feed reader running or
remembered, in an installed build, whose updates are on by default."
  `(with-saved-globals ((nle::*installed-build* t)
                        (nle::*release-stream* nil) (nle::*release-stream-stopping* nil)
                        (nle::*release-stream-seen* nil) (nle::*feed-stream* nil) ,@places)
     ,@body))

(deftest update-feed-reads-events-and-pokes-once-per-label (let ((poked '())))
  (with-idle-feed ((nle::*release-poke* (lambda (label) (push label poked))))
    (nlk:read-events
     (make-string-input-stream (format nil ": hello~%data: v1~%~%data: v1~%~%data: v2~%~%"))
     (lambda (payload) (nle::feed-note-label payload) nil))
    (is (equal '("v1" "v2") (reverse poked)))
    (is (equal "v2" nle::*release-stream-seen*) "the feed remembers where it is")))

(deftest update-feed-session-opens-reads-and-refuses ()
  (let ((poked '()))
    (with-idle-feed ((nle::*feed-open* (on-url
                                       (values (make-string-input-stream (format nil "data: v7~%~%")) nil)))
                   (nle::*release-poke* (lambda (label) (push label poked))))
      (is-values (ok reason) (nle::release-feed-session "http://feed.test/")
        (ok is "the session read to the end") (reason null "and ended cleanly"))
      (is (equal '("v7") poked) "the label it carried was poked")))
  (with-saved-globals ((nle::*feed-open*
                         (on-url (values nil "HTTP 404"))))
    (is-values (ok reason) (nle::release-feed-session "http://feed.test/")
      (ok null "no stream, no session") (reason "HTTP 404" "the reason travels"))))

(defun serve-one-answer (&rest lines)
  "A listener on 127.0.0.1 that reads one request and writes LINES, each
ended by CRLF, then closes. => (values PORT LISTENER)."
  (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                         :element-type '(unsigned-byte 8)))
        (answer (format nil "~{~a~C~C~}" (mapcan (lambda (l) (list l #\Return #\Newline)) lines))))
    (nlk:spawn "feed-answer"
      (ignore-errors
       (let* ((client (usocket:socket-accept listener))
              (stream (usocket:socket-stream client)))
         (nlk:with-cleanup ((usocket:socket-close client))
           ;; the request, to its blank line
           (loop with tail = 0
                 for byte = (read-byte stream nil nil)
                 while byte
                 do (setf tail (if (member byte '(10 13)) (1+ tail) 0))
                 until (>= tail 4))
           (write-sequence (sb-ext:string-to-octets answer :external-format :latin-1) stream)
           (finish-output stream)))))
    (values (usocket:get-local-port listener) listener)))

(deftest update-feed-socket-reads-a-chunked-stream-and-refuses-a-failure ()
  ;; What carries the feed where dexador cannot stream (Windows): the status,
  ;; the headers, then the chunked body undone into the octets READ-EVENTS
  ;; reads.
  (let* ((event (format nil "data: rel42~%~%"))
         (labels '()))
    (multiple-value-bind (port listener)
        (serve-one-answer "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                          "Transfer-Encoding: chunked" "" (format nil "~x" (length event)) event "0" "")
      (nlk:with-cleanup ((usocket:socket-close listener))
        (let ((stream (nle::feed-open-socket (format nil "http://127.0.0.1:~d/api/release/feed?pointer=x" port))))
          (nlk:with-cleanup ((ignore-errors (close stream)))
            (nlk:read-events stream (lambda (payload) (push (nlk:trimmed payload) labels) nil) :idle 10)))))
    (is (equal '("rel42") labels) "the label, through the chunking"))
  (multiple-value-bind (port listener)
      (serve-one-answer "HTTP/1.1 404 Not Found" "Content-Length: 0" "")
    (nlk:with-cleanup ((usocket:socket-close listener))
      (is-carrying (text (refusal-text error (nle::feed-open-socket (format nil "http://127.0.0.1:~d/feed" port))))
        ("404" "a failed answer is refused with its status")))))

(defmacro with-octet-pipe ((in out) &body body &aux (read-fd (gensym)) (write-fd (gensym)))
  "BODY with IN an octet fd-stream reading what OUT, a character stream,
writes: a socket's shape, where only a deadline bounds a read."
  `(multiple-value-bind (,read-fd ,write-fd) (sb-posix:pipe)
     (let ((,in (sb-sys:make-fd-stream ,read-fd :input t :element-type '(unsigned-byte 8)))
           (,out (sb-sys:make-fd-stream ,write-fd :output t)))
       (nlk:with-cleanup ((ignore-errors (close ,out)) (ignore-errors (close ,in)))
         ,@body))))

(deftest update-feed-session-ends-when-the-connection-goes-silent ()
  "A half-open connection -- one ping, then nothing, the socket never closed
-- ends the session with a reason, so the reader reconnects."
  (with-octet-pipe (in out)
    (format out ": ping~%~%")
    (finish-output out)
    (with-idle-feed ((nle::*feed-read-timeout* 0.3)
                     (nle::*feed-open* (on-url (values in nil)))
                     (nle::*release-poke* (lambda (label) (declare (ignore label)))))
      (is-values (ok reason) (nle::release-feed-session "http://feed.test/")
        (ok null "a silent feed is a failed session")
        ((search "silent" reason) is "and says so")))))

(defclass torn-feed (sb-gray:fundamental-character-input-stream)
  ((text :initarg :text) (at :initform 0))
  (:documentation "A feed its host tears down: TEXT, then END-OF-FILE signalled
out of the read, the way cl+ssl ends a TLS stream closed with no goodbye."))

(defmethod sb-gray:stream-read-char ((stream torn-feed))
  (with-slots (text at) stream
    (if (< at (length text))
        (prog1 (char text at) (incf at))
        (error 'end-of-file :stream stream))))

(deftest update-feed-session-ends-well-when-the-host-closes-it (let ((poked '())))
  "The host ends a held feed when it will: over TLS that is END-OF-FILE out
of the read, and the session ended, with nothing to say. Without this,
each reconnect would show a red toast."
  (with-idle-feed ((nle::*feed-open* (on-url
                                       (values (make-instance 'torn-feed :text (format nil "data: v8~%~%")) nil)))
                   (nle::*release-poke* (lambda (label) (push label poked))))
    (is-values (ok reason) (nle::release-feed-session "http://feed.test/")
      (ok is "the session ended well") (reason null "with nothing to say"))
    (is (equal '("v8") poked) "the label it carried was poked")))

(defmacro running-feed (done)
  "The release feed's reader started, awaited until DONE holds, and stopped."
  `(progn (nle::start-release-feed) (await (:tries 200 :rest 0.05) ,done) (nle::stop-release-feed)))

(deftest update-feed-says-a-failure-once-while-it-lasts (let ((opened 0) (said (make-string-output-stream))))
  "A reason names the connection's own socket, so no two read alike: the
reader says the first and keeps quiet until a connection lands."
  (with-idle-feed ((nle::*feed-retry-seconds* 0.01)
                   (nle::*feed-outage-seconds* 0)
                   (nle::*feed-open* (on-url
                                       (values nil (format nil "refused on socket ~d" (incf opened)))))
                   ((sb-ext:symbol-global-value '*error-output*) said))
    (running-feed (>= opened 4)))
  (is (>= opened 4) "the reader kept trying")
  (is (equal (format nil "release feed: refused on socket 1~%") (get-output-stream-string said))))

(deftest update-feed-says-an-outage-only-once-it-lasts (let ((opened 0) (said (make-string-output-stream))))
  "A failure that heals inside *FEED-OUTAGE-SECONDS* says nothing; one that
lasts is said once, in words."
  (with-idle-feed ((nle::*feed-retry-seconds* 0.01)
                   (nle::*feed-outage-seconds* 30)
                   (nle::*release-poke* (lambda (label) (declare (ignore label))))
                   (nle::*feed-open* (on-url (if (< (incf opened) 4)
                                                 (values nil "host not found at feed.test:443")
                                                 (values (make-string-input-stream (format nil "data: v1~%~%")) nil))))
                   ((sb-ext:symbol-global-value '*error-output*) said))
    (running-feed (>= opened 6)))
  (is (>= opened 6) "three failed opens, then the feed landed")
  (is (equal "" (get-output-stream-string said)) "an outage shorter than the window is not said")
  (is-table (condition words) (search words (nle::feed-failure-text condition "https://feed.test/api/release/feed"))
    ((make-condition 'usocket:ns-try-again-error :host-or-ip "feed.test")
     "host not found at feed.test" "a name the resolver could not answer yet")
    ((make-condition 'usocket:network-unreachable-error)
     "network unreachable" "no route out")
    ((make-condition 'simple-error :format-control "event stream silent for ~a seconds"
                                   :format-arguments '(60))
     "event stream silent for 60 seconds" "a failure above the wire says itself")))

(deftest update-feed-pings-keep-a-quiet-feed-open (let ((events '())))
  "The idle limit is per line: pings closer together than it keep the
connection past it, and the label after them still lands."
  (with-octet-pipe (in out)
    (let ((writer (nlk:spawn "feed-pinger"
                    (dotimes (i 6)
                      (format out ": ping~%~%")
                      (finish-output out)
                      (sleep 0.1))
                    (format out "data: v3~%~%")
                    (close out))))
      (nlk:read-events in (lambda (payload) (push payload events) nil) :idle 0.4)
      (bt2:join-thread writer)
      (is (equal '("v3") events) "six pings over 0.6 s, each inside 0.4 s, then the label"))))

(deftest update-feed-thread-reconnects-and-stops (let ((opened 0) (poked '())))
  (with-idle-feed ((nle::*feed-retry-seconds* 0.05)
                 (nle::*feed-open* (on-url
                                     (incf opened)
                                     (values (make-string-input-stream (format nil "data: v9~%~%")) nil)))
                 (nle::*release-poke* (lambda (label) (push label poked))))
    (running-feed (>= opened 3))
    (is (>= opened 3) "the reader comes back after the feed ends")
    (is (equal '("v9") poked) "one release, one poke, however many reconnects")
    (is (null nle::*release-stream*) "the stop ends the thread")))

(deftest update-poller-looks-soon-after-boot (let ((asked '())))
  "The cadence is six hours; the first look after a boot is not."
  (with-idle-feed ((nle::*update-poller* nil)
                 (nle::*update-poller-first-look-seconds* 0.05)
                 (nle::*release-fetch* (lambda (url)
                                         (push url asked)
                                         (values (nle::effective-label) nil))))
    (nlk:with-cleanup ((nle::stop-update-poller) (nlk:note-standing "update" nil :info))
      (nle::start-update-poller)
      (await (:rest 0.05) asked)
      (is asked "the poller looked without waiting the cadence"))))

(deftest update-check-brings-the-feed-up ()
  "A look at the channel also ensures the reader runs: a hot upgrade lands
new definitions in an image whose gateway began before there was a reader
to start."
  (with-idle-feed ((nle::*feed-retry-seconds* 30)
                 (nle::*feed-open* (constantly nil))
                 (nle::*release-fetch* (constantly (nle::effective-label))))
    (nlk:with-cleanup ((nle::stop-release-feed) (nlk:note-standing "update" nil :info))
      (nle::check-for-update)
      (is (not (null nle::*release-stream*)) "the reader runs after the look")
      (nle::check-for-update)
      (let ((thread (nlk:worker-thread nle::*release-stream*)))
        (is (bt2:thread-alive-p thread) "and the second look does not replace it")))))

(deftest update-poke-retries-a-check-that-did-not-land ()
  "A poke asks once; when the label it named is still ahead of this image, it
asks again after a pause -- and stops the moment the image has caught up."
  (loop for (seen label looks why)
          in `(("ahead" "ahead" 3 "the first look and both retries")
               (,(nle::effective-label) "mine" 1
                "a label this image already wears is one look, no retries"))
        do (let ((asked '()))
             (with-saved-globals ((nle::*release-stream-stopping* nil) (nle::*release-stream* t)
                                  (nle::*poke-retry-seconds* '(0.01 0.01))
                                  (nle::*release-stream-seen* seen)
                                  (nle::*release-fetch* (lambda (url)
                                                          (push url asked)
                                                          (values (nle::effective-label) nil))))
               (nle::poke-update-check label)
               (is (= looks (length asked)) why)
               (is (every (lambda (url) (search "/stable-" url)) asked))
               (nlk:note-standing "update" nil :info)))))

(deftest update-answer-says-whose-label-when-the-binary-differs ()
  ;; `update' answers for the organism serving this home and `--version' for
  ;; the binary typed. A checkout's ./nodecode, which never updates, would
  ;; otherwise read as a contradiction: `already at ...' beside `nodecode
  ;; ...'. One label is one line; two say whose.
  (flet ((answer (binary installed &aux (nle::*installed-build* installed))
           (with-stubbed-fdefinitions
               ((nle::organism-port () 4321)
                (nle::gateway-configured-operator-token () "token")
                (nle::gateway-http-session-request (&rest args)
                  (nlk:json-object "text" "already at tv9" "from" "tv9" "to" "tv9"))
                (nle::effective-label () binary))
             (nle::update-text '("--check")))))
    (is (equal "already at tv9" (answer "tv9" t)))
    (is (equal (format nil "this home's organism: already at tv9~%~
                            this binary: tv1, a checkout build (`just release' rebuilds it)")
               (answer "tv1" nil)))
    (is (equal (format nil "this home's organism: already at tv9~%this binary: tv1")
               (answer "tv1" t)))))

(defun numbered-release-files (number)
  "A release's files whose nodecode.asd says NUMBER, as a release's does."
  (list (cons "src/src/a.lisp" *update-old-a*)
        (cons "src/nodecode.asd"
              (format nil "(defsystem \"upg\" :version \"~a\" :depends-on (\"uiop\") :components ((:file \"a\")))" number))))

(deftest update-a-window-whose-path-predates-the-update-is-told-why ()
  ;; Windows puts the version's own folder on the user PATH, and a window keeps
  ;; the PATH it opened with: after `nodecode update' there, `nodecode' still
  ;; finds the old folder and `--version' says the update never happened --
  ;; "update doesn't work, no error". The answer names both releases, and the
  ;; way to the new one.
  (with-release-host (dist (list (cons "tv1" (numbered-release-files "9.0.1"))
                                 (cons "tv2" (numbered-release-files "9.0.2"))))
    (is (null (nle::current-label)) "nothing installed, nothing current")
    (nle::stage-release "tv1")
    (nle::link-release "tv1")
    (nle::stage-release "tv2")
    (nle::link-release "tv2")
    (is (equal "tv2" (nle::current-label)))
    (is (equal "9.0.2 (tv2)" (nle::release-name "tv2")))
    (is (equal "no-such" (nle::release-name "no-such")) "a release not laid out here keeps its label")
    (with-stubbed-fdefinition (nle::window-release () "tv1")
      (is (equal "this window still runs 9.0.1 (tv1): its PATH is from before the update. Open a new terminal or tab to run 9.0.2 (tv2)"
                 (nle::stale-window-text))))
    (with-stubbed-fdefinition (nle::window-release () "tv2")
      (is (null (nle::stale-window-text)) "a window that runs `current' is told nothing"))
    (with-stubbed-fdefinition (nle::window-release () nil)
      (is (null (nle::stale-window-text)) "nor one whose PATH names no version folder"))))

(deftest update-path-names-a-folder-whatever-its-trailing-separator ()
  (let ((sep (if (eq (nlk:platform) :windows) ";" ":")))
    (is (nle::path-names-directory-p #p"/opt/nc/tv1/" (format nil "/usr/bin~a/opt/nc/tv1~a/bin" sep sep)))
    (is (nle::path-names-directory-p #p"/opt/nc/tv1/" (format nil "/opt/nc/tv1/~a/bin" sep)))
    (is (not (nle::path-names-directory-p #p"/opt/nc/tv1/" (format nil "/opt/nc/tv2~a/opt/nc/current" sep))))
    (is (not (nle::path-names-directory-p #p"/opt/nc/tv1/" nil)))))

(deftest update-locally-takes-the-release-current-names-as-it-lies (with-organism-tree ("tv2" (upg-source 2 "a")))
  ;; A second `nodecode update' in a window that still runs the old release
  ;; used to fetch and lay out the release `current' already named, and say
  ;; "tv2 is current" -- which reads as nothing to do -- beside the classifier's
  ;; line, which says nothing to a person whose next launch runs the build whole.
  (with-stubbed-fdefinitions ((nle::organism-port () nil)
                              (nle::window-release () nil))
    (is (equal "updated to tv2: the next launch runs it" (nle::update-text '("tv2"))))
    (is (equal "tv2" (nle::current-label)))
    (with-stubbed-fdefinition (nle::stage-release (version) (error "~a was staged again" version))
      (is (equal "tv2 is installed already: the next launch runs it" (nle::update-text '("tv2"))))
      (with-stubbed-fdefinition (nle::window-release () "tv1")
        (is (equal (format nil "tv2 is installed already~%~
                                this window still runs tv1: its PATH is from before the update. Open a new terminal or tab to run tv2")
                   (nle::update-text '("tv2"))))))))

;;; --- the note a new release says -------------------------------------------

(defun changes-list (&rest lines)
  "The publisher's changes-<label> text of LINES."
  (format nil "~{~a~%~}" lines))

(deftest update-release-changes-reads-what-came-after-the-release-before (let ((asked '())))
  ;; publish.sh's list, newest first: a head for a commit that touched what
  ;; ships, the label alone for one that did not.
  (with-saved-globals ((nle::*release-fetch*
                        (lambda (url)
                          (push url asked)
                          (changes-list "tv9aaaaaa core: the newest" "tv9bbbbbb"
                                        "tv9cccccc channels: a card" "tv8000000 core: the one before"
                                        "tv7000000 core: older"))))
    (is-values (heads more) (nle::release-changes "tv8000000" "tv9aaaaaa")
      (heads '("core: the newest" "channels: a card") "the heads above the release before")
      (more eql 0))
    (is (search "/changes-tv9aaaaaa" (first asked)) "the list beside the archives")
    (let ((nle::*release-note-changes* 3))
      (is-values (heads more) (nle::release-changes "elsewhere" "tv9aaaaaa")
        (heads '("core: the newest" "channels: a card" "core: the one before")
               "a release before that the list does not reach: every change, capped")
        (more eql 1))))
  (with-saved-globals ((nle::*release-fetch* (on-url (values nil "HTTP 404"))))
    (is (null (nle::release-changes "a" "b")) "a host with no list")))

(deftest update-a-new-release-is-said-once (with-temp-store ())
  ;; If a remote upgrade happens, it is notified. The first release a home
  ;; runs is kept, not said; the next is taken once -- on the board the model
  ;; reads, and as the note the model words for the shells and the channel
  ;; hosts (SAY-NOTE) -- and the boot after it has nothing to tell again.
  (with-saved-globals ((nle::*installed-build* t)
                       (nlk::*effective-version* "tv7")
                       (nle::*release-fetch*
                        (on-url
                          (changes-list "tv8 core: what tv8 changed" "tv7 core: what tv7 changed"))))
    (nlk:with-cleanup ((nlk:note-standing "release" nil :info))
      (is (null (nle::take-release)) "a home's first release is kept, not said")
      (setf nlk::*effective-version* "tv8")
      (is-present (note (nle::boot-release)) "the next release is taken"
        (is-carrying note "nodecode updated to " "+tv8 (from tv7)" "- core: what tv8 changed"
                     (:absent "what tv7 changed" "nothing from the release this home ran before"))
        ;; It stands on the board the model reads.
        (is (equal note (second (find "release" (nlk:standing-notices) :key #'first :test #'string=)))))
      (is (null (nle::take-release)) "taken once")
      (is (null (nle::take-release "tv7")) "the update that moved it does not take it again")))
  (with-saved-globals ((nle::*installed-build* nil) (nlk::*effective-version* "tv9"))
    (is (null (nle::take-release "tv8")) "an update mode of off says nothing")))

(deftest update-the-release-route-says-what-the-release-changed (with-temp-gateway (port))
  ;; A shell's Latest changes (/settings): the release the organism runs,
  ;; the one this home ran before once it said what changed since, and each
  ;; change since, newest first; a commit that changed nothing that ships is
  ;; none, and a host with no list says so.
  (with-saved-globals ((nle::*installed-build* t)
                       (nlk::*effective-version* "tv7")
                       (nle::*release-fetch*
                        (lambda (url)
                          (if (search "changes-tv8" url)
                              (changes-list "tv8 core: what tv8 changed" "tv8b" "tv7 core: what tv7 changed")
                              (values nil "HTTP 404")))))
    (nlk:with-cleanup ((nlk:note-standing "release" nil :info))
      (nle::take-release)
      (with-gateway-http (port :get "/api/gateway/release" :status 200)
        (is-shape body ((:string "label") "tv7") ((:any "listed") eq nil) ((:any "from") null))
        (is (zerop (length (nlk:json-value body :array "changes"))) "a host with no list: no change"))
      (setf nlk::*effective-version* "tv8")
      (nle::boot-release)
      (with-gateway-http (port :get "/api/gateway/release" :status 200)
        (is-shape body ((:string "label") "tv8") ((:string "from") "tv7") ((:any "listed") eq t))
        (is (equal '(("tv8" "core: what tv8 changed"))
                   (map 'list (lambda (change)
                                (list (nlk:json-value change :string "label") (nlk:json-value change :string "head")))
                        (nlk:json-value body :array "changes"))) "the changes since the release before, a head each")))))

(deftest update-a-shell-shows-no-organism-note-on-home ()
  ;; The home screen is the mark, the version and the tip: a note the
  ;; organism says before the chat has begun is not shown there, and it is
  ;; spent, so the same note brought again by a later attach puts no row
  ;; into the chat that begins after.
  (with-saved-globals ((nle::*organism-notes-shown* '()))
    (with-frame-model (model 100 30 :session-id "note-home")
      (feed-organism model "organism.note" "text" "Back. Short stop. Not me.")
      (is (null (nle::transcript-rows model.state)) "no row on a shell whose chat has not begun")
      (let ((frame (nle::compose-frame model)))
        (is (not (rows-carry-p "Not me." frame)) "the home screen does not say it")
        (is (rows-carry-p "█▀▄█" frame) "and stays home"))
      (rendering model (setf model.state (nle::ingest-submitted-prompt model.state "ask")))
      (feed-organism model "organism.note" "text" "Back. Short stop. Not me.")
      (is (not (rows-carry-p "Not me." (nle::compose-frame model))) "the same note at a later attach puts no row into the chat"))))

(deftest update-a-shell-shows-an-organism-note-once-as-its-words-alone ()
  ;; Once the chat has begun the note is a row of the transcript -- a toast's
  ;; four lines would cut it -- its words alone on the canvas, no band and no
  ;; mark; a note that comes again, at every attach while it is fresh, is one
  ;; row.
  (with-saved-globals ((nle::*organism-notes-shown* '()))
    (with-frame-model (model 100 30 :session-id "release-shell")
      (setf model.state (nle::ingest-submitted-prompt model.state "ask"))
      (dotimes (i 2)
        (feed-organism model "organism.note"
                       "text" (format nil "nodecode updated to 0.0.1+tv8 (from tv7)~%- core: what tv8 changed")))
      (let ((rows (remove :system-notice (nle::transcript-rows model.state)
                          :key #'nle::transcript-row-kind :test-not #'eq)))
        (is (= 1 (length rows)) "one row for the note said twice")
        (is (search "- core: what tv8 changed" (nle::transcript-row-text (first rows))))
        (is (null (nle::row-block-fill (first rows))) "no band behind it")
        (is (notany #'nle::lline-fill (nle::row-llines (first rows) 80)) "nor under any of its lines"))
      (let ((line (row-carrying "nodecode updated to 0.0.1+tv8" (nle::compose-frame model))))
        (is (and line (not (search "■" line))) "and no mark before it")))))

(deftest update-poller-leaves-a-held-release-be (let ((asked 0)))
  ;; `nodecode update VERSION' held this home there: the channel is left
  ;; alone and what landed meanwhile is said, until a bare `nodecode update'.
  (with-stubbed-fdefinitions ((nle::start-release-feed () nil)
                              (nle::upgrade-organism (&rest args) (incf asked)))
    (with-saved-globals ((nle::*release-fetch* (on-url "tv9")))
      (nle::hold-release "tv1")
      (is (null (nle::check-for-update)))
      (is (zerop asked) "the poller moves nothing under a hold")
      (is-carrying (text (board-line "update"))
        ("release tv1 is held by `nodecode update tv1'" "the board names the hold")
        ("tv9 is out" "what landed meanwhile") ("lifts the hold" "and the way off it")))))

(deftest update-a-named-release-holds-and-one-unnamed-lifts-it (with-organism-tree ("tv3" (upg-source 2 "a")))
  ;; The operator's own pick of what runs is the one hold there is.
  (with-stubbed-fdefinition (nle::start-release-feed () nil)
    (is (eq :hot (getf (nle::upgrade-organism :version "tv3" :apply t :hold t) :applied)))
    (is (equal "tv3" (nle::held-release)) "a named release holds this home there")
    (nle::upgrade-organism :version "tv3" :check t :apply t)
    (is (equal "tv3" (nle::held-release)) "a check moves no hold")
    (is (eq :current (getf (nle::upgrade-organism :version "tv3" :apply t) :applied)))
    (is (null (nle::held-release)) "an update that names none lifts it")))

(defun changes-host (lists &optional (latest "tv9"))
  "A release host whose pointer names LATEST and whose changes-<label> lists
are LISTS ((LABEL . LINES) ...), any other list a 404."
  (lambda (url)
    (let ((list (find-if (lambda (entry) (search (format nil "changes-~a" (car entry)) url)) lists)))
      (cond (list (apply #'changes-list (cdr list)))
            ((search "changes-" url) (values nil "HTTP 404"))
            (t latest)))))

(deftest update-poller-moves-only-forward ()
  ;; The channel's latest goes live only past this build: this build on its
  ;; changes list, or two releases no list relates. One on this build's list
  ;; -- older -- or one beside a build no channel published, a checkout's
  ;; own, is said and left.
  (loop for (lists applies why says)
          in '(((("tv9" "tv9" "tv5")) t "tv5 is on tv9's list: forward" nil)
               ((("tv9" "tv9") ("tv5" "tv5")) t "two releases no list relates" nil)
               ((("tv9" "tv9" "tv1") ("tv5" "tv5" "tv9")) nil "tv9 is on tv5's list: back" "is newer")
               ((("tv9" "tv9" "tv1")) nil "no list for tv5: no channel published it"
                "one no channel published"))
        do (let ((asked '()))
             (with-stubbed-fdefinitions ((nle::start-release-feed () nil)
                                         (nle::upgrade-organism (&rest args) (push args asked) nil))
               (with-saved-globals ((nlk::*effective-version* "tv5") (nle::*release-list* nil)
                                    (nle::*release-fetch* (changes-host lists)))
                 (nlk:with-cleanup ((nlk:note-standing "update" nil :info))
                   (nle::check-for-update)
                   (is (eq applies (and asked t)) why)
                   (when applies
                     (is (eq t (getf (first asked) :idle)) "and it waits for a moment no turn runs"))
                   (when says
                     (is (search "tv9 is out" (board-line "update")) "the board says what landed")
                     (is (search says (board-line "update")) why))))))))

(deftest update-a-release-that-loses-a-provider-is-undone (with-organism-tree ("tv3" (upg-source 2 "a")))
  ;; Every definition evaluated and a provider this image served no longer
  ;; resolves: the image is put back and the release waits for a boot, which
  ;; loads it whole.
  (let ((looks 0))
    (with-saved-globals ((nle::*refused-release* nil) (nle::*gateway-run* nil))
      (with-stubbed-fdefinitions ((nle::start-release-feed () nil)
                                  (nle::resolving-providers ()
                                    (if (= 1 (incf looks)) '("p1" "p2") '("p1"))))
        (let ((result (nle::upgrade-organism :version "tv3" :apply t)))
          (is (eq :live (getf result :applied)) "refused live")
          (is-carrying (text (getf result :line))
            ("provider p2 no longer resolves here, so the release is undone" "why")
            ("tv3 runs at the next launch" "and when")))
        (is (eql 1 (funcall 'nodecode.evolved::upg-route-f)) "the image is as it was")
        (is (null nlk::*effective-version*))
        (is (equal "tv3" nle::*refused-release*) "the poller leaves it for the boot")))))

(deftest update-the-poller-s-release-goes-live-once-no-turn-runs (with-organism-tree ("tv3" (upg-source 2 "a")))
  (let ((busy t))
    (with-saved-globals ((nle::*idle-poll-seconds* 0.05))
      (with-stubbed-fdefinitions ((nle::start-release-feed () nil)
                                  (nlk:any-active-turn-p () busy))
        (let ((thread (bt2:make-thread
                       (lambda () (nle::upgrade-organism :version "tv3" :apply t :idle t))
                       :name "idle-update")))
          (sleep 0.3)
          (is (eql 1 (funcall 'nodecode.evolved::upg-route-f)) "not while a turn runs")
          (is (search "goes live the first moment no turn is running" (board-line "update")) "the board says it waits")
          (setf busy nil)
          (is (eq :hot (getf (bt2:join-thread thread) :applied)))
          (is (eql 2 (funcall 'nodecode.evolved::upg-route-f)) "then it is live"))))))

(deftest update-holds-every-turn-start-while-it-goes-live (let ((taken :unasked)))
  ;; A turn begins under the store's lock (ADMIT-COMMAND); the move holds
  ;; that lock, so no turn starts across half of one build and half of another.
  (nle::call-with-turns-held
   (lambda ()
     (setf taken (bt2:join-thread
                  (bt2:make-thread
                   (lambda ()
                     (handler-case
                         (let ((mutex (bt2:lock-native-lock nlk::*store-lock*)))
                           (when (sb-thread:grab-mutex mutex :waitp nil)
                             (sb-thread:release-mutex mutex)
                             t))
                       (error (condition) condition)))
                   :name "would-admit")))))
  (is (null taken) "no turn can take the store's lock meanwhile"))

(deftest update-says-which-overrides-the-release-displaced (with-organism-tree ("tv3" (upg-source 2 "a")))
  ;; An override whose shipped definition the release changed stands down, as
  ;; the boot's loader skips it (STALE-LAYER-FILE); the line names it.
  (let ((looks 0))
    (with-stubbed-fdefinitions ((nle::start-release-feed () nil)
                                (nle::stale-overrides () (if (= 1 (incf looks)) '() '("upg-route-f"))))
      (is-carrying (text (getf (nle::upgrade-organism :version "tv3" :apply t) :line))
        ("tv3 is live" "it went live")
        ("your override of upg-route-f stands down" "and names what stood down")))))

(deftest update-the-release-note-says-the-way-back ()
  (with-temp-trees ((releases "releases"))
    (ensure-directories-exist (merge-pathnames "tv7/" releases))
    (with-saved-globals ((nle::*release-directory* releases))
      (is (search "`nodecode update tv7' goes back to it and holds there" (nle::release-note "tv7" "tv8")))
      (is (not (search "goes back" (nle::release-note "tv6" "tv8"))) "not to a release no longer laid out"))))
