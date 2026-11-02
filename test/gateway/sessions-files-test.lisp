;;;; sessions-files-test.lisp --- a session deleted, exported and imported
;;;; over the gateway's sessions routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   F1. DELETE /api/gateway/sessions/<id> takes the session and what hangs
;;;;       under it, says what went, and forgets them: the listing, the
;;;;       roster, the page's rows views. ?dry_run=1 says the same and deletes
;;;;       nothing. An unknown id is a 404.
;;;;   F2. It refuses, naming why, while a turn runs in the tree or a shell
;;;;       holds one of them open; a web page's own rows view is no shell.
;;;;   F3. GET .../export sends the file with a name to save it under; POST
;;;;       /api/gateway/sessions/import brings it back, and the session's
;;;;       timeline reads as it did; a session already here refuses it, and
;;;;       a body that is not a session file is named as such.
;;;;   F4. POST .../title names a session, one line of at most 64
;;;;       characters, and answers the name now in effect, which the
;;;;       directory, the roster and /rename all read; the empty name gives
;;;;       back the first prompt's line. A session standing by has nothing to
;;;;       name. A name the operator gave stops model naming, even one landing
;;;;       while the model thinks; a cleared one lets it name again.
;;;;   F5. GET .../export?format=markdown sends the session as a shell
;;;;       attached to it shows it, to be read: a fork's parent turns above
;;;;       its own, never a turn a rewind left, the prompts and answers, a
;;;;       plan, one line per call naming a spawned agent's session; a turn
;;;;       still running is left out and said to be. Named after the session.
;;;;       /export writes it where the shell runs: in its folder, in a folder
;;;;       named, or at a path named.
;;;;   F6. GET .../files lists the session's own folder, directories first,
;;;;       a dotfile marked hidden, a link that stays inside as what it names
;;;;       and one that leads out as a link; a file is answered as what it
;;;;       is: text as text up to the cap, an image as itself with its type,
;;;;       anything else or larger as a word saying why, and &download=1 sends
;;;;       any file to be saved. POST writes the body as a new file there and
;;;;       refuses a name already there unless asked to replace it, a
;;;;       folder's name always, and a body past the cap; a refused body is
;;;;       still read, so a retry over the same connection stands on its
;;;;       own. Every way out -- a
;;;;       climbing step, an absolute path, a link that leads out, a name with
;;;;       a slash, a NUL or `..' -- is refused before anything is read or
;;;;       written, and so is a session whose folder is gone.
;;;;   F7. The listing says the source that began each session. POST
;;;;       .../prune with a dry run names the old trees and deletes nothing;
;;;;       the delete that follows takes what it named and no more; a pick a
;;;;       shell holds stays, said why; a body that is not a prune is refused.
;;;;   F8. Before a session is, GET /api/gateway/folder names the folder a new
;;;;       one would work in -- the gateway's own when none is named, a typed
;;;;       path resolved as the session route resolves it (~ the home, a
;;;;       relative one under the gateway's folder) -- and says whether it is
;;;;       there, or why not. POST /api/gateway/sessions refuses a folder that
;;;;       is not there in the same words and mints nothing; a fork that names
;;;;       none works where its parent does, there or not.

(in-package #:nodecode.test)

(deftest sessions-route-deletes-a-tree-says-what-went-and-forgets-it (with-temp-gateway (port))
  (seed-transfer-sessions)
  (is-route (port :delete "/api/gateway/sessions/nowhere") 404 "an unknown id" "unknown_session")
  (with-gateway-http (port :delete "/api/gateway/sessions/s-a?dry_run=1" :status 200)
    (is-shape body ("sessions" equalp #("s-a" "s-b" "s-c")) ("turns" = 5)))
  (is (nlk:session-exists-p "s-a") "a dry run deletes nothing")
  ;; A shell on a session under it: the delete waits for it to close.
  (let ((gws (nle::find-or-create-gw-session "s-b"))
        (page (nle::make-rows-view :client :page :session-id "s-b" :link :page-link)))
    (push :shell (nle::gw-session-peers gws))
    (is-route (port :delete "/api/gateway/sessions/s-a") 409 "a shell holds s-b open" "session_open")
    (setf (nle::gw-session-peers gws) (list :page-link))
    (setf (gethash :page nle::*rows-views*) (list page))
    (with-gateway-http (port :delete "/api/gateway/sessions/s-a")
      (is (= 200 status) "a web page's own rows view is no shell")
      (is (equalp #("s-a" "s-b" "s-c") (gethash "sessions" body))))
    (is (null (gethash :page nle::*rows-views*)) "and it is closed"))
  (is-route (port :get "/api/gateway/sessions/s-a") 404)
  (with-gateway-http (port :get "/api/gateway/sessions")
    (is (equalp #() body) "the listing forgot the tree")))

(deftest sessions-route-refuses-a-tree-with-a-running-turn (with-temp-gateway (port))
  (seed-transfer-sessions)
  (nlk:admit-turn "s-b" (nlk::make-durable-id "command") "still going")
  (is-route (port :delete "/api/gateway/sessions/s-a") 409 "a turn runs under it" "turn_running")
  (is-route (port :get "/api/gateway/sessions/s-a/export") 409 "nor is an open turn copied" "turn_running")
  (is (nlk:session-exists-p "s-a")))

(defun post-session-file (port content &optional (type "application/x-ndjson"))
  (gateway-octets port :post "/api/gateway/sessions/import" :content content :type type))

(deftest sessions-route-exports-a-file-and-imports-it-back (with-temp-gateway (port))
  (seed-transfer-sessions)
  (let ((timeline (with-gateway-http (port :get "/api/gateway/sessions/s-a/timeline")
                    (nlk:encode-json-object body))))
    (multiple-value-bind (file status headers) (gateway-octets port :get "/api/gateway/sessions/s-a/export")
      (is (= 200 status))
      (is (equal "application/x-ndjson" (gethash "content-type" headers)))
      (is (search "filename=\"nodecode-s-a.jsonl\"" (gethash "content-disposition" headers)))
      (is-route (port :delete "/api/gateway/sessions/s-a") 200)
      (multiple-value-bind (answer status) (post-session-file port file)
        (is (= 200 status))
        (is (equalp #("s-a" "s-b" "s-c")
                    (gethash "sessions" (nlk:decode-json answer)))))
      (with-gateway-http (port :get "/api/gateway/sessions/s-a/timeline")
        (is (equal timeline (nlk:encode-json-object body)) "the Run view reads as it did"))
      (with-gateway-http (port :get "/api/gateway/sessions/s-a/history" :status 200)
        (is (plusp (length (gethash "frames" body))) "and so does its history"))
      ;; A session already here refuses the file.
      (is (= 409 (nth-value 1 (post-session-file port file))))
      (is (= 415 (nth-value 1 (post-session-file port file "application/json")))))
    (multiple-value-bind (answer status) (post-session-file port (sb-ext:string-to-octets "hello"))
      (is (= 400 status))
      (is (equal "invalid_file" (nlk:json-value (nlk:decode-json answer) :string "error" "code")))))
  (is-route (port :get "/api/gateway/sessions/nowhere/export") 404))

;;; --- F7: prune ----------------------------------------------------------------

(defmacro with-prune ((port &rest fields) &body body)
  `(with-gateway-http (,port :post "/api/gateway/sessions/prune" :status 200
                       :body (nlk:encode-json-object (nlk:json-object ,@fields)))
     ,@body))

(deftest sessions-route-prunes-what-its-dry-run-named-and-says-what-stays (with-temp-gateway (port))
  (seed-old-session "p-cron" 3 :source "cron")
  (seed-old-session "p-old" 2 :source "team")
  (seed-old-session "p-old-kid" 4 :parent "p-old" :source "checkpoint")
  (seed-completed-turn "p-fresh" "today" "fresh")
  (with-gateway-http (port :get "/api/gateway/sessions")
    (flet ((row (entry) (list (gethash "id" entry) (gethash "source" entry))))
      (is (equal '(("p-fresh" "in_process") ("p-old" "team") ("p-old-kid" "checkpoint") ("p-cron" "cron"))
                 (map 'list #'row body)) "every row says the source that began its session")))
  (let ((named nil))
    (with-prune (port "days" 30 "dry_run" t)
      (is-shape body ("dry_run" eq t) ("count" = 3) ("turns" = 3))
      (setf named (gethash "sessions" body))
      (is (equalp #("p-cron" "p-old") named) "the old trees, oldest first, what hangs under one unnamed")
      (is-shape body ("titles" equalp #("old ask" "old ask")) ("kept" equalp #())))
    (is (every #'nlk:session-exists-p '("p-cron" "p-old" "p-old-kid")) "a dry run deletes nothing")
    ;; Old enough by the time the delete comes, but never named: it stays.
    (seed-old-session "p-late" 5 :source "cron")
    (with-prune (port "days" 30 "sessions" named)
      (is-shape body ("dry_run" null) ("count" = 3))
      (is (equalp named (gethash "sessions" body)) "the delete takes what the dry run named"))
    (is (notany #'nlk:session-exists-p '("p-cron" "p-old" "p-old-kid")))
    (is (every #'nlk:session-exists-p '("p-late" "p-fresh")) "and nothing else"))
  ;; A shell on a pick keeps it, and the answer says why.
  (push :shell (nle::gw-session-peers (nle::find-or-create-gw-session "p-late")))
  (with-prune (port "days" 30 "source" "cron")
    (is (equalp #() (gethash "sessions" body)))
    (is-shape (aref (gethash "kept" body) 0)
      ("id" "p-late") ("reason" "a shell has p-late open: close it there first")))
  (is (nlk:session-exists-p "p-late"))
  (is-table (text) (= 400 (gateway-http-status port :post "/api/gateway/sessions/prune"
                                               :token "test-token" :body text))
    ("{}" "days are required")
    ("{\"days\":-1}" "and never fewer than none")
    ("{\"days\":30,\"source\":7}" "a source is a kind")
    ("{\"days\":30,\"sessions\":\"p-late\"}" "the named sessions are a list"))
  (is-route (port :get "/api/gateway/sessions/prune") 405 "a prune is posted"))

;;; --- F4: naming ---------------------------------------------------------------

(defun post-title (port id title)
  "POST TITLE as session ID's name. => (values STATUS BODY)."
  (gateway-http-status port :post (format nil "/api/gateway/sessions/~a/title" id)
                       :token "test-token"
                       :body (nlk:encode-json-object (nlk:json-object "title" title))))

(deftest sessions-route-names-a-session-and-gives-the-prompt-line-back (with-temp-gateway (port))
  (seed-completed-turn "s-name" (format nil "Fix the reconnect backoff~%it never resets") "found it")
  (flet ((listed () (getf (session-row-for "s-name" (nlk:list-sessions)) :title))
         (named (title) (nlk:json-value (nth-value 1 (post-title port "s-name" title)) :string "title")))
    (multiple-value-bind (status body) (post-title port "s-name" (format nil "  Backoff~%  fix "))
      (is (= 200 status))
      (is-shape body ("session_id" "s-name") ("title" "Backoff fix" "one line, trimmed")))
    (is (equal "Backoff fix" (listed)) "the directory reads it")
    (is (= 64 (length (named (make-string 80 :initial-element #\n)))) "a long name keeps its first 64")
    (is (equal "Fix the reconnect backoff" (named "")) "the empty name gives back the prompt's line")
    (is (equal "Fix the reconnect backoff" (listed)))
    (is-table (line answer) (equal answer (nle:slash line :session-id "s-name"))
      ("/rename Pelican work" "renamed: Pelican work" "/rename is the same writer")
      ("/rename default" "renamed: Fix the reconnect backoff" "and default gives the line back"))
    (let ((bare (nle:slash "/rename" :session-id "s-name")))
      (is (search "this session is named \"Fix the reconnect backoff\"" bare) "bare, it says the name")))
  (nlk:standby-session :id "s-fresh")
  (is (= 409 (post-title port "s-fresh" "Too soon")) "a session standing by has nothing to name")
  (is (search "nothing to name yet" (nle:slash "/rename Too soon" :session-id "s-fresh")))
  (is (= 404 (post-title port "nowhere" "x")) "an unknown session")
  (is-route (port :post "/api/gateway/sessions/s-name/title" :body "{\"title\":7}") 400
            "a name is a string" "invalid_request"))

;;; --- F5: Markdown ---------------------------------------------------------------

(defun markdown-export (port id)
  "SESSION ID's Markdown export as PORT's gateway sends it, the clock's and the
checkout's words made plain. => (values TEXT STATUS HEADERS)."
  (multiple-value-bind (body status headers)
      (gateway-octets port :get (format nil "/api/gateway/sessions/~a/export?format=markdown" id))
    (values (ppcre:regex-replace-all
             "\\d{4}-\\d\\d-\\d\\d \\d\\d:\\d\\d"
             (ppcre:regex-replace-all (ppcre:quote-meta-chars
                                       (nlk:durable-session-cwd (nlk:find-session id)))
                                      (sb-ext:octets-to-string body :external-format :utf-8)
                                      "CWD")
             "STAMP")
            status headers)))

(deftest sessions-route-exports-markdown-as-a-shell-shows-the-session (with-temp-gateway (port))
  (seed-transfer-sessions)
  ;; S-B's own second turn: a picture it looked at.
  (with-durable-turn (turn "s-b" :input "look" :complete t)
    (record-tool-use turn "c9" "look" "{\"path\":\"shot.png\"}" "seen"
                     :metadata (nlk:json-object "image" (nlk:json-object "payload_kind" "path")))
    (nlk:record-assistant-message turn (nlk:json-object "role" "assistant" "content" "planned")))
  (multiple-value-bind (text status headers) (markdown-export port "s-b")
    (is (= 200 status))
    (is (equal "text/markdown; charset=utf-8" (gethash "content-type" headers)))
    (is (search "filename=\"fork.md\"" (gethash "content-disposition" headers)) "named after the session")
    ;; The parent's first and third turns above the fork's; its second, which
    ;; a rewind left, nowhere.
    (is (equal (format nil "# fork~%~%- Session: `s-b` · CWD~%- Started STAMP · 4 turns~%~
                            - Forked from \"first\" (`s-a`)~%~%~
                            **You** · STAMP~%~%first~%~%**Nodecode**~%~%- `eval` (+ 1 2)~%~%one~%~%~
                            **You** · STAMP~%~%third~%~%**Nodecode**~%~%three~%~%~
                            **You** · STAMP~%~%fork~%~%**Nodecode**~%~%forked~%~%~
                            **You** · STAMP~%~%look~%~%**Nodecode**~%~%- `look` · image~%~%planned~%")
               text)))
  (nlk:admit-turn "s-a" (nlk::make-durable-id "command") "still going")
  (multiple-value-bind (text status) (markdown-export port "s-a")
    (is (= 200 status) "a running turn is no bar to reading")
    (is (not (search "still going" text)) "and is left out")
    (is (search (format nil "three~%~%_A turn is running: this copy ends before it._~%") text) "saying so"))
  (is-route (port :get "/api/gateway/sessions/s-a/export?format=pdf") 400 "a format it does not write" "invalid_request")
  (is-route (port :get "/api/gateway/sessions/nowhere/export?format=markdown") 404))

(deftest slash-export-writes-the-markdown-where-the-shell-runs (with-temp-gateway (port))
  (seed-transfer-sessions)
  (with-temp-workspace (dir)
    (let ((model (gateway-shell port "s-a" :ui t))
          (nle::*workspace-directory* (string-right-trim "/" (namestring dir))))
      (is (search "nothing to export yet" (nle::execute-slash-command model "export" "")) "a fresh shell has nothing to save")
      (paint-model model (nle::ingest-message-fact "user" "first"))
      (is-table (args path) (equal (format nil "exported to ~a" (merge-pathnames path dir))
                                   (nle::execute-slash-command model "export" args))
        ("" "first.md" "named after the session, in the shell's folder")
        ("notes/" "notes/first.md" "in a folder named")
        ("out.md" "out.md" "at a path named"))
      (let ((file (uiop:read-file-string (merge-pathnames "notes/first.md" dir))))
        (is (uiop:string-prefix-p "# first" file) "the gateway's Markdown")))))

;;; --- F6: the session's own folder -----------------------------------------------

(defparameter +files-route-png+
  (octets #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 6 0 0 0)
  "A PNG's leading bytes: what IMAGE-MEDIA-TYPE reads a picture by.")

(defmacro with-files-route-folder ((port proj away) &body body)
  "WITH-TEMP-GATEWAY over the session s-files, whose folder PROJ holds
README.md, src/main.lisp, .env, shot.png, blob.bin, a link `code' to src/, a
link `out' to the directory AWAY beside it (which holds secret.txt), and a
link `gone' to nothing."
  `(with-temp-trees ((,proj "files-proj") (,away "files-away"))
     (flet ((put (dir name content)
              (nlk:write-file-atomically (merge-pathnames name dir) content))
            (link (target name) (nlk:make-symlink target (merge-pathnames name ,proj))))
       (put ,proj "README.md" (format nil "hello~%world~%"))
       (put ,proj "src/main.lisp" "(print 1)")
       (put ,proj ".env" "KEY=1")
       (put ,proj "shot.png" +files-route-png+)
       (put ,proj "blob.bin" (octets 1 0 2 0 3))
       (put ,away "secret.txt" "not yours")
       (link (merge-pathnames "src/" ,proj) "code")
       (link ,away "out")
       (link (merge-pathnames "nothing-here" ,proj) "gone"))
     (with-gateway-session (,port "s-files" :cwd (namestring ,proj))
       ,@body)))

(defun files-route (port query &key (method :get) content (session "s-files"))
  "SESSION's files route at QUERY. => (values BODY STATUS HEADERS), a JSON
answer decoded."
  (multiple-value-bind (body status headers)
      (gateway-octets port method (format nil "/api/gateway/sessions/~a/files~a" session query)
                      :content content :type (and content "application/octet-stream"))
    (values (if (search "application/json" (gethash "content-type" headers ""))
                (nlk:decode-json body)
                body)
            status headers)))

(defun files-route-refusal (port query &rest keys)
  "The status and the error code the files route answers QUERY with."
  (multiple-value-bind (body status) (apply #'files-route port query keys)
    (list status (nlk:json-value body :string "error" "code"))))

(defun files-route-names (listing)
  "The names a listing's rows carry, in its order."
  (map 'list (lambda (row) (gethash "name" row)) (gethash "entries" listing)))

(deftest files-route-lists-a-folder-and-answers-each-file-as-what-it-is ()
  (with-files-route-folder (port proj away)
    (multiple-value-bind (listing status) (files-route port "")
      (is (= 200 status))
      (is-shape listing ("kind" "directory") ("path" "")
                ("folder" (string-right-trim "/" (namestring (truename proj))) "the folder, links followed"))
      (flet ((row (name) (find name (gethash "entries" listing) :key (lambda (row) (gethash "name" row)) :test #'equal)))
        (is (equal '("code" "src" ".env" "blob.bin" "gone" "out" "README.md" "shot.png") (files-route-names listing)) "directories first, then by name")
        (is-shape (row "code") ("kind" "directory") ("link" "inside" "a link that stays inside reads as what it names"))
        (is-shape (row "out") ("kind" "link") ("link" "outside" "one that leads out is a link"))
        (is-shape (row "gone") ("link" "nowhere"))
        (is-shape (row ".env") ("hidden" t) ("size" 5))
        (is-shape (row "README.md") ("size" 12) ("modified" is "when it last changed"))))
    (is (equal '("main.lisp") (files-route-names (files-route port "?path=code"))) "a folder reached through a link inside")
    (multiple-value-bind (text status headers) (files-route port "?path=README.md")
      (is (= 200 status))
      (is (equal "text/plain; charset=utf-8" (gethash "content-type" headers)) "text as text")
      (is (equal (format nil "hello~%world~%") (sb-ext:octets-to-string text :external-format :utf-8))))
    (multiple-value-bind (bytes status headers) (files-route port "?path=shot.png")
      (is (= 200 status))
      (is (equal "image/png" (gethash "content-type" headers)) "an image as itself, with its type")
      (is (equalp +files-route-png+ bytes)))
    (is-shape (files-route port "?path=blob.bin") ("kind" "file") ("why" "binary") ("size" 5))
    (with-saved-globals ((nle::*folder-text-bytes* 8))
      (is-shape (files-route port "?path=README.md") ("why" "too_large" "text past the cap is not sent")))
    (multiple-value-bind (bytes status headers) (files-route port "?path=src/main.lisp&download=1")
      (is (= 200 status))
      (is (search "attachment; filename=\"main.lisp\"" (gethash "content-disposition" headers)) "a download is saved")
      (is (equal "(print 1)" (sb-ext:octets-to-string bytes :external-format :utf-8))))
    (nlk:standby-session :id "s-standing" :cwd (namestring proj))
    (is (equal "directory" (gethash "kind" (files-route port "" :session "s-standing"))) "a session standing by has its folder too")))

(deftest files-route-says-in-words-when-a-file-cannot-be-read ()
  ;; Without this, a file the gateway may not read would answer SBCL's
  ;; `Error opening #P"...": Permission denied.' with pathname syntax
  ;; and the absolute path, and the page would print it as it came.
  (with-files-route-folder (port proj away)
    (let ((file (uiop:native-namestring (merge-pathnames "README.md" proj))))
      (sb-posix:chmod file 0)
      (nlk:with-cleanup ((sb-posix:chmod file #o644))
        (multiple-value-bind (body status) (files-route port "?path=README.md")
          (is (= 403 status))
          (is-shape body
            ((:string "error" "code") "unreadable")
            ((:string "error" "message") "this file can't be read here (permission denied)"))
          (is (= 403 (nth-value 1
                                (files-route port "?path=README.md&download=1"))) "nor saved"))))))

(deftest files-route-uploads-a-file-and-refuses-a-name-already-there ()
  (with-files-route-folder (port proj away)
    (flet ((upload (query text) (files-route port query :method :post :content (sb-ext:string-to-octets text)))
           (refused (query text) (files-route-refusal port query :method :post :content (sb-ext:string-to-octets text)))
           (on-disk (name) (uiop:read-file-string (merge-pathnames name proj))))
      (multiple-value-bind (listing status) (upload "?path=src&name=notes%20one.txt" "first")
        (is (= 200 status))
        (is (equal '("main.lisp" "notes one.txt") (files-route-names listing)) "the answer is the folder, the file in it"))
      (is (equal "first" (on-disk "src/notes one.txt")))
      (is (equal '(409 "file_exists") (refused "?path=src&name=notes%20one.txt" "second")) "a name already there")
      (is (equal "first" (on-disk "src/notes one.txt")) "is left as it was")
      (is (= 200 (nth-value 1 (upload "?path=src&name=notes%20one.txt&replace=1" "second"))) "unless replace is asked")
      (is (equal "second" (on-disk "src/notes one.txt")))
      (is (equal '(409 "is_folder") (refused "?name=src&replace=1" "x")) "a folder's name, always")
      (with-saved-globals ((nle::*folder-upload-bytes* 4))
        (is (equal '(413 "request_too_large") (refused "?name=big.txt" "too big")) "a body past the cap"))
      (is (not (probe-file (merge-pathnames "big.txt" proj))) "leaves nothing")
      (is (equal '(409 200) (files-route-over-one-connection port '("?name=README.md" "again") '("?name=README.md&replace=1" "again"))) "a refused upload's body is read, so a retry on its connection is a request of its own")
      (is (equal "again" (on-disk "README.md"))))))

(defun files-route-over-one-connection (port &rest requests)
  "The statuses PORT's gateway answers REQUESTS -- each (QUERY BODY), a POST of
BODY to s-files's files route -- sent one after another over one connection,
each answer read whole before the next is sent, as a browser keeping the
connection open sends them."
  (usocket:with-client-socket (socket stream "127.0.0.1" port :element-type 'character)
    (let ((crlf (coerce '(#\Return #\Newline) 'string)))
      (loop for (query body) in requests
            collect (progn
                      (format stream "POST /api/gateway/sessions/s-files/files~a HTTP/1.1~aHost: 127.0.0.1:~d~a~
                                      Authorization: Bearer test-token~aContent-Type: application/octet-stream~a~
                                      Content-Length: ~d~a~a~a"
                              query crlf port crlf crlf crlf (length body) crlf crlf body)
                      (force-output stream)
                      (let ((status (parse-integer (second (uiop:split-string (read-line stream) :separator " "))))
                            (length 0))
                        (loop for line = (string-right-trim '(#\Return) (read-line stream))
                              until (string= line "")
                              when (uiop:string-prefix-p "content-length:" (string-downcase line))
                                do (setf length (parse-integer line :start 15)))
                        (dotimes (i length) (read-char stream))
                        status))))))

(deftest files-route-refuses-every-way-out-of-the-folder (with-files-route-folder (port proj away))
  (let ((body (sb-ext:string-to-octets "x")))
    (is-each (files-route-refusal port)
      ("?path=.." '(403 "outside_folder") "a climbing step")
      ("?path=src/../../files-away" '(403 "outside_folder") "one further down")
      ((format nil "?path=~a" (namestring away)) '(403 "outside_folder") "an absolute path")
      ("?path=out" '(403 "outside_folder") "a link that leads out")
      ("?path=out/secret.txt" '(403 "outside_folder") "a file under it")
      ("?path=out/nothing.txt" '(403 "outside_folder") "or nothing under it: no answer tells what lies outside")
      ("?path=gone" '(404 "not_found") "a link to nothing")
      ("?path=a%00b" '(400 "invalid_path") "a NUL")
      #+win32 ("?path=..%5C..%5Cfiles-away" '(403 "outside_folder") "a climb spelled with \\"))
    (is-each (files-route-refusal port)
      ("?name=a%2Fb.txt" :method :post :content body '(400 "invalid_name") "a name with a slash")
      #+win32 ("?name=..%5Cx.txt" :method :post :content body '(400 "invalid_name") "a name with a \\")
      ("?name=.." :method :post :content body '(400 "invalid_name") "the name ..")
      ("?name=a%00b" :method :post :content body '(400 "invalid_name") "a name with a NUL")
      ("?path=..&name=x.txt" :method :post :content body '(403 "outside_folder") "a folder above")
      ("?path=out&name=x.txt" :method :post :content body '(403 "outside_folder") "a folder through a link out")))
  (is (equal '("secret.txt") (mapcar #'file-namestring (uiop:directory-files away))) "nothing was written outside")
  (is (equal "not yours" (uiop:read-file-string (merge-pathnames "secret.txt" away))) "and what was there is as it was")
  (is-route (port :get "/api/gateway/sessions/nowhere/files") 404 "an unknown session" "unknown_session")
  (nlk:create-session :id "s-gone" :cwd (namestring (merge-pathnames "vanished/" proj)))
  (is-route (port :get "/api/gateway/sessions/s-gone/files") 404 "a folder no longer there" "no_folder"))

(defun folder-check (port path)
  "What PORT's gateway says of PATH as a new session's folder, NIL its own."
  (nlk:decode-json (gateway-octets port :get (format nil "/api/gateway/folder~@[?path=~a~]"
                                                      (and path (quri:url-encode path))))))

(deftest folder-route-names-the-gateways-own-and-checks-one-typed ()
  (with-temp-trees ((proj "folder-proj"))
    (nlk:write-file-atomically (merge-pathnames "notes.txt" proj) "x")
    (with-temp-gateway (port)
      (let ((native (string-right-trim "/" (namestring proj)))
            (here (namestring (uiop:getcwd))))
        (is-shape (folder-check port nil)
          ("path" here "none named: where a session naming none opens") ("exists" t) ("why" :null))
        (is-shape (folder-check port native) ("path" native "a typed folder, as a session would carry it") ("exists" t))
        (is-shape (folder-check port "~") ("path" (namestring (user-homedir-pathname)) "~ is the home"))
        (is-shape (folder-check port (format nil "~a/./sub/.." native))
          ("path" native "a . or .. step is read by the text, as cd reads it"))
        (is-shape (folder-check port (format nil "~a/nothing/here" native))
          ("exists" null "a folder that is not there")
          ("why" (format nil "there is no folder at ~a/nothing/here" native) "says so"))
        (is-shape (folder-check port "relative/dir")
          ("path" (format nil "~arelative/dir" here) "a relative one lies under the gateway's folder")
          ("why" (format nil "there is no folder at ~arelative/dir" here)))
        (is-shape (folder-check port (format nil "~a/notes.txt" native))
          ("why" (format nil "~a/notes.txt is a file, not a folder" native) "a file is no folder"))
        (flet ((open-in (id cwd &optional parent)
                 (gateway-http-status port :post "/api/gateway/sessions" :token "test-token"
                                           :body (nlk:encode-json-object
                                                  (nlk:json-object "session_id" id :opt "cwd" cwd :opt "parent" parent)))))
          (is-values (status body) (open-in "s-typo" "/no/such/folder")
            (status 400 "a session in a folder that is not there is refused")
            ((nlk:json-value body :string "error" "code") "no_folder")
            ((nlk:json-value body :string "error" "message") "there is no folder at /no/such/folder" "in the words the check says"))
          (is (not (nlk:session-exists-p "s-typo")) "and nothing stands by")
          (is (= 200 (open-in "s-there" native)) "one that is there opens")
          (is (equal native (nlk:standby-record-cwd (nlk:standby-session-record "s-there"))) "in it")
          (nlk:create-session :id "s-vanished" :cwd (format nil "~a/vanished" native))
          (is (= 200 (open-in "s-fork" nil "s-vanished")) "a fork naming none opens where its parent does")
          (is (equal (format nil "~a/vanished" native)
                     (nlk:standby-record-cwd (nlk:standby-session-record "s-fork")))))))))
