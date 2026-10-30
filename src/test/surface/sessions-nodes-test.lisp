;;;; sessions-nodes-test.lisp --- /sessions: a session and the nodes under it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;
;;;;   N1. A session has exactly two edges to another session: an inherited
;;;;       fork — a created fact carrying an anchor turn, which is what a
;;;;       parented create and /fork make — and a detached node — a parent
;;;;       with no anchor, which is what CREATE-SESSION :DETACHED records so
;;;;       that a session spawned by another (a team seat) hangs under it
;;;;       while beginning fresh. Both at once is no edge at all and is
;;;;       refused, and the listing reads both fields back per row.
;;;;   N2. The wire names the edge once — SESSION-RELATION, `inherited' /
;;;;       `detached' / absent — so no surface parses an id to learn one.
;;;;   N3. The /sessions picker draws a session's nodes under it, in one
;;;;       branch, the session counting them; a reflection -- the organism's
;;;;       own note on a turn -- is no node there and is not listed, and
;;;;       nothing at all sits above a session.
;;;;   N4. The context line names what is running; the tabs beside it count
;;;;       the sessions, and nothing names a level above a session.
;;;;   N5. The picker opens on the sessions whose tree began in the shell's
;;;;       folder: every row is keyed by its root's folder, so a fork, a frame
;;;;       or a team seat running elsewhere lists with the session it came
;;;;       off, and a folder spelled with its trailing separator is the same
;;;;       folder.
;;;;   N6. A row says what its session waits on the person it answers for:
;;;;       the mark its turn put on the newest answer (ANSWER-WAITS), with the
;;;;       question or null for the answer's own last line, or a newest turn
;;;;       that failed; a mark a later turn followed, a running turn's and a
;;;;       stopped one's wait on nobody.

(in-package #:nodecode.test)

(nlk:access (fork nlk::durable-session) (seat nlk::durable-session))

(deftest sessions-nodes-the-store-holds-both-edges (with-temp-store ())
  ;; N1: an inherited fork freezes the parent's head turn; a detached node
  ;; hangs under the same parent and composes none of it.
  (seed-completed-turn "opener" "a question" "an answer")
  (let ((fork (nlk:create-session :id "fork" :cwd "." :parent "opener"))
        (seat (nlk:create-session :id "seat" :cwd "." :parent "opener" :detached t)))
    (is (equal "opener" fork.parent) "a fork hangs under its parent")
    (is (equal (nlk:session-head-turn-id "opener") fork.anchor-turn-id) "and composes it: the anchor froze that head")
    (is (equal "opener" seat.parent) "a detached node hangs under the same session")
    (is (null seat.anchor-turn-id) "composing nothing of it")
    (is (not (nlk:session-known-turn-p "seat" fork.anchor-turn-id)) "so none of that session's turns are the seat's")
    (is (signals-error nlk:conflicting-session-edge
          (nlk:create-session :id "both" :cwd "." :parent "opener" :detached t
                              :anchor-turn-id fork.anchor-turn-id)) "a detached edge with an anchor is refused")
    ;; A session's source is its first turn's: a reflection's fork is begun by
    ;; the experience cell's own request.
    (nlk:create-session :id "reflection" :cwd "." :parent "opener")
    (nlk:complete-turn (reflection-turn "reflection" "reflection-cmd" "[experience] reflect") nil)
    (flet ((row (id) (session-row-for id (nlk:list-sessions))))
      (is (equal "opener" (getf (row "fork") :parent)) "the listing carries the parent")
      (is (stringp (getf (row "fork") :anchor-turn-id)) "and the anchor that makes it a fork")
      (is (null (getf (row "seat") :anchor-turn-id)) "a detached row carries none")
      ;; The page sets the machine's own notes apart by the source the wire
      ;; names (nle::gateway-session-entry-json), never by a title.
      (is-table (id source)
        (equal source (gethash "source" (nle::gateway-session-entry-json (row id))))
        ("reflection" "experience" "a reflection's row names the source that made it")
        ("opener" "in_process" "and an ask's row names its own")))))

(deftest sessions-say-what-a-session-waits-on (with-temp-store ())
  ;; N6: the turn marks its answer, the listing's row says so.
  (flet ((admit (id) (nlk:admit-turn id (nlk::make-durable-id "command") "ask"))
         (waits (id &aux (json (gethash "waits" (nle::gateway-session-entry-json
                                                 (session-row-for id (nlk:list-sessions))))))
           (if (eq json :null) :null (list (gethash "why" json) (gethash "question" json)))))
    (dolist (id '("w-asks" "w-line" "w-on" "w-failed" "w-stopped" "w-running"))
      (nlk:create-session :id id :cwd "."))
    (let* ((turn (admit "w-asks")) (id (nlk:durable-turn-turn-id turn)))
      (is (equal (list :session-id "w-asks" :turn-id id :waits "Push the pin?")
                 (nle:answer-waits :question " Push the pin? " :session "w-asks" :turn id)) "the mark answers what it marked")
      (nlk:complete-turn turn nil))
    (let ((turn (admit "w-line")))
      (let ((nle::*current-durable-turn* turn))
        (is (equal "w-line" (getf (nle:answer-waits) :session-id)) "a turn marks its own answer by default"))
      (nlk:complete-turn turn nil))
    (let ((turn (admit "w-on")))
      (nle:answer-waits :question "Ship it?" :session "w-on" :turn (nlk:durable-turn-turn-id turn))
      (nlk:complete-turn turn nil))
    (nlk:complete-turn (admit "w-on") nil)
    (nlk:fail-turn (admit "w-failed") (make-condition 'simple-error :format-control "boom"))
    (nlk:cancel-turn (admit "w-stopped") "stopped by the operator")
    (let ((turn (admit "w-running")))
      (nle:answer-waits :question "Which one?" :session "w-running" :turn (nlk:durable-turn-turn-id turn)))
    (is-table (id said)
      (equal said (waits id))
      ("w-asks" '("asked" "Push the pin?") "an answer marked with its question")
      ("w-line" '("asked" :null) "one marked on its own last line")
      ("w-on" :null "a mark a later turn followed")
      ("w-failed" '("failed" nil) "a newest turn that failed")
      ("w-stopped" :null "a turn someone stopped")
      ("w-running" :null "a running turn's mark, until it completes"))
    (is (signals-error error (nle:answer-waits :question "  " :session "w-asks" :turn "t")) "a blank question is refused")
    (is (signals-error error (nle:answer-waits :question (make-string 201 :initial-element #\x)
                                               :session "w-asks" :turn "t")) "and one past 200 characters")
    (is (signals-error error (let ((nle::*current-durable-turn* nil)) (nle:answer-waits))) "no turn, none named: refused")))

(deftest sessions-nodes-the-wire-names-the-edge ()
  ;; N2: the one word every surface draws.
  (is (null (nle::session-relation nil nil)) "a session that began on its own has no edge")
  (is (equal "inherited" (nle::session-relation "opener" "turn-1")) "an anchored edge reads inherited")
  (is (equal "detached" (nle::session-relation "opener" nil)) "a provenance-only edge reads detached")
  (let ((seat (nle::gateway-session-entry-json (list :id "team-z-0" :parent "opener")))
        (fork (nle::gateway-session-entry-json
               (list :id "experience-s-a-turn-1" :parent "s-a" :anchor-turn-id "turn-1")))
        (root (nle::gateway-session-entry-json (list :id "s-a"))))
    (is (equal "detached" (gethash "relation" seat)) "a team seat's row says detached")
    (is (equal "inherited" (gethash "relation" fork)) "a reflection's row says inherited")
    (is (null (gethash "relation" root)) "a root says nothing")
    (is (null (gethash "family" root)) "and no row is grouped above a session")))

(deftest sessions-nodes-a-session-carries-its-own-branch ()
  ;; N3/N4 over a small directory: a shell holding a team seat and a
  ;; reflection, the seat running; a room's lane holding nothing.
  (let* ((reflection (directory-entry "experience-s-a-turn-1" "s-a" :relation "inherited"
                                      :title "a reflection"))
         (lane (directory-entry "discord-100" nil :title "a lane"))
         (entries (list (directory-entry "s-a" nil :title "root shell")
                        (directory-entry "team-z-0" "s-a" :relation "detached"
                                         :title "the seat")
                        reflection
                        lane))
         (fold (progn (setf (gethash "source" reflection) "experience"
                            (gethash "source" lane) "channel")
                      (nle::make-sessions-picker-fold entries)))
         (activity (make-hash-table :test #'equal)))
    (setf (gethash "team-z-0" activity) (iso-ago 30))
    (labels ((rows () (remove-if #'nle::picker-row-heading (nle::sessions-fold-rows fold activity)))
             (labels-of () (mapcar #'nle::picker-row-label (rows)))
             (row (needle) (find needle (rows) :key #'nle::picker-row-label :test #'search)))
      ;; One root row per session and nothing above it; the nodes it holds are
      ;; held back, and the session says how many there are.
      (is (= 2 (length (labels-of))) "one row per session")
      (is (search "1 fork " (session-row-text (row "root shell"))) "the session counts the node it holds, the reflection not among them")
      (is (not (search "fork" (session-row-text (row "a lane")))) "a session holding none says nothing")
      (is (search "channel" (session-row-text (row "a lane"))) "and a room's session says where it came from")
      (is (not (search "channel" (session-row-text (row "root shell")))) "a shell's says nothing")
      (is (null (row "the seat")) "no node is drawn while its session is folded")
      (is (plusp (length (nle::picker-row-mark (row "root shell")))) "a session holding a running node wears the dim spinner")
      ;; Opening the session splices its node in under it; the reflection is
      ;; not listed at all.
      (setf (gethash "s-a" (nle::sessions-fold-open fold)) t)
      (is (equal '("root shell" "╰ the seat" "a lane") (labels-of)) "opening a session draws its nodes")
      (is (= 3 (length (nle::sessions-fold-entries fold))) "the listing the fold keeps holds no reflection")
      ;; N4: the context line says what is running, never a grouping.
      (is (search "1 running" (nle::sessions-dialog-context fold activity)) "the context names what runs")
      (is (equal "" (nle::sessions-dialog-context fold (make-hash-table :test #'equal))) "and nothing while nothing does")
      (is (not (search "famil" (nle::sessions-dialog-context fold activity))) "and no level above a session"))))

(deftest sessions-nodes-a-tree-lists-under-the-folder-it-began-in ()
  ;; N5: a session opened here, the team seat it started in a scratch folder,
  ;; and a session opened elsewhere.
  (let* ((nle::*workspace-directory* "/work/here")
         (entries (list (directory-entry "s-here" nil :age 60 :directory "/work/here/")
                        (directory-entry "seat-here" "s-here" :age 120 :relation "detached"
                                         :directory "/scratch/team/seat/")
                        (directory-entry "s-there" nil :age 600 :directory "/work/there")))
         (fold (nle::make-sessions-picker-fold
                entries (nle::iso-8601-ms "2026-01-01T00:01:00.000Z"))))
    (setf (gethash "s-here" (nle::sessions-fold-open fold)) t)
    (flet ((keys () (mapcar (lambda (row) (cons (nle::picker-row-value row) (nle::picker-row-key row)))
                            (remove-if #'nle::picker-row-heading (nle::sessions-fold-rows fold))))
           (tabs () (mapcar (lambda (tab) (list (getf tab :label) (getf tab :count) (getf tab :value)))
                            (nle::sessions-folder-tabs fold))))
      (is (equal '(("s-here" . "/work/here") ("seat-here" . "/work/here") ("s-there" . "/work/there"))
                 (keys)) "every row is keyed by the folder its tree began in")
      (is (equal '(("This folder" 2 "/work/here") ("All" 3 nil)) (tabs)) "the folder tab counts the tree"))))

(defun session-prefix-text (row &key selected)
  "The rendered row's text, independent of color-profile escape sequences."
  (apply #'concatenate 'string
         (mapcar #'nle::seg-text (nle::dialog-entry-segs row selected))))

(defun session-row-text (row &key selected (room 150))
  "ROW as the /sessions dialog lays it ROOM columns wide (PICKER-ROW-PAINT),
as plain text, its trailing blanks trimmed."
  (string-right-trim " " (apply #'concatenate 'string
                                (mapcar #'nle::seg-text (funcall (nle::picker-row-paint row) selected room)))))

(deftest sessions-row-says-its-age-and-its-name-never-its-prompt ()
  ;; The ledger line: the age, the tree's lane and the name, and at the right
  ;; the forks, the messages and where the session came from -- the right
  ;; giving way first on a narrow row. The first prompt is never drawn.
  (let* ((stamp "2026-10-12T12:34:00.000Z")
         (now (nle::iso-8601-ms "2026-10-12T14:35:00.000Z"))
         (entry (nlk:json-object "id" "s-age" "title" "Work" "updated_at" stamp
                                 "prompt" "fix the flaky test in ci" "message_count" 6
                                 "source" "channel"))
         (walk (list :id "s-age" :entry entry :fork t :lanes '(nil))))
    (flet ((row (&rest options) (apply #'nle::session-picker-row walk now options)))
      (let ((text (session-row-text (row :children 1))))
        (is (search "2h ago  ╰ Work" text) "the age stands before the tree's lane and the name")
        (is (not (search "fix the flaky" text)) "the first prompt is never drawn")
        (is (< (search "Work" text) (search "1 fork" text)
               (search "6 msgs" text) (search "channel" text)) "and the forks, the messages and the source at the right, in that order"))
      (is-shape (row) (nle::picker-row-label "╰ Work" "the raw name keeps its lane")
        (nle::picker-row-detail "" "nor matched")
        (nle::picker-row-value "s-age"))
      (is (equal "Work" (gethash "title" entry)) "the age never becomes part of the saved name")
      (multiple-value-bind (segs ground) (funcall (nle::picker-row-paint (row)) t 150)
        (is (equal "▌" (nle::seg-text (first segs))) "the chosen row wears the edge")
        (is (eq :dialog-accent (nle::seg-tone (first segs))))
        (is (eq :panel-lift ground) "and stands on the lift")
        (is (eq :dialog-accent (nle::seg-tone (find "Work" segs :key #'nle::seg-text :test #'search)))))
      (multiple-value-bind (segs ground) (funcall (nle::picker-row-paint (row)) nil 150)
        (is (null ground) "any other row stands on the panel")
        (is (eq :system (nle::seg-tone (find "Work" segs :key #'nle::seg-text :test #'search)))))
      (let ((narrow (session-row-text (row :children 1) :room 60)))
        (is (not (search "channel" narrow)) "a narrow row gives up its right lane")
        (is (search "Work" narrow) "before the name"))
      (is (search "3d ago" (session-row-text (nle::session-picker-row walk (+ now (* 3 24 60 60 1000)))))))
    ;; A lane no name was given for is named by its first line: its words, or
    ;; the speaker when the cut left none.
    (is-table (title name)
      (equal name (nle::session-spoken title))
      ("peasant (operator) [m1560 u9317]: fix the build" "fix the build" "a room's line names it by its words")
      ("peasant (operator) [m15604912435896034 u93170806531990733" "peasant (operator)" "by its speaker when it holds none")
      ("[m1560 u9317" nil "and by nothing when it holds neither")
      ("Work" "Work" "any other name stands as it is"))
    (setf (gethash "title" entry) nil (gethash "prompt" entry) nil)
    (is (search "2h ago  ╰ s-age" (session-row-text (nle::session-picker-row walk now))) "an unnamed session says its id")
    (setf (gethash "updated_at" entry) nil)
    (is (search "unknown  ╰ s-age" (session-row-text (nle::session-picker-row walk now))) "an absent clock is explicit, never now")))

(deftest sessions-a-day-heads-its-sessions-and-the-folder-tab-keeps-its-own (with-clock-at-local-noon)
  ;; The list breaks at each day its sessions moved on, newest first: a
  ;; heading no key or pointer chooses, over that day's sessions. Under an
  ;; earlier day a row says the clock time, the heading having said the day;
  ;; under today, how long ago. The folder tab keeps a day over its own rows.
  (let* ((nle::*workspace-directory* "/work/here")
         (fold (nle::make-sessions-picker-fold
                (list (directory-entry "s-now" nil :age 60 :directory "/work/here")
                      (directory-entry "s-old" nil :age (* 3 86400) :directory "/work/there"))))
         (rows (nle::sessions-fold-rows fold))
         (match (getf (first (nle::sessions-folder-tabs fold)) :match)))
    (is (equal '(t nil t nil) (mapcar (lambda (row) (and (nle::picker-row-heading row) t)) rows)) "a heading over each day")
    (is (equal "Today" (nle::picker-row-label (first rows))))
    (is (search "Today ─" (session-row-text (first rows))) "a rule runs from it")
    (is (search "1m ago  s-now" (session-row-text (second rows))) "today's row says how long ago")
    (is (ppcre:scan "[0-9]:[0-9][0-9] [AP]M  s-old" (session-row-text (fourth rows))) "an earlier day's the clock time")
    (is (equal '("Today" "s-now")
               (mapcar #'nle::picker-row-label (remove-if-not (lambda (row) (funcall match row "/work/here")) rows))) "the folder tab keeps the day its own session moved on, and no other")))

(deftest sessions-relative-age-is-shared-by-search-and-rename ()
  (with-stubbed-fdefinitions ((nlk::iso-now () "2026-10-12T14:35:00.000Z")
                              (nle::picker-notify-highlight (model dialog &key instant) nil))
    (let* ((entry (nlk:json-object "id" "s-age" "title" "Work" "updated_at" "2026-10-12T12:34:00.000Z"))
           (fold (nle::make-sessions-picker-fold (list entry)))
           (name (first (nle::session-name-picker-rows fold "work" nil))))
      (is (equal "2h ago  Work  name" (session-prefix-text name)) "name search renders the title once after its age")
      (with-frame-model (model 90 25 :session-id "s-age")
        (nle::open-sessions-picker-dialog model fold)
        (let ((dialog (nle::chat-model-dialog model)))
          (setf (nle::picker-dialog-search dialog) "needle")
          (nle::install-session-search
           model "needle" (vector (nlk:json-object "session_id" "s-age" "seq" 7 "title" "Work"
                                                 "occurred_at" "2026-10-11T10:00:00.000Z"
                                                 "snippet" "needle found" "role" "user")))
          (let ((hit (first (nle::picker-dialog-rows dialog))))
            (is (search "2h ago  Work  needle found" (session-prefix-text hit)) "a hit prefixes the session age, not the block age")
            (is (equal '("s-age" . 7) (nle::picker-row-value hit)))
            (nle::install-session-name model dialog "s-age" "Renamed")
            (is (search "2h ago  Renamed  needle found" (session-prefix-text hit)) "renaming preserves the age prefix")))))))

(deftest a-day-heading-comes-back-with-its-first-row-and-rows-keep-off-the-bar ()
  ;; Without this, scrolling back onto the first session would leave Today
  ;; above the window, and a row's right edge would run into the scrollbar.
  (let* ((fold (nle::make-sessions-picker-fold
                (loop for i from 1 to 30
                      collect (directory-entry (format nil "s-~2,'0d" i) nil :age (* 60 i)))))
         (dialog (nle::make-picker-dialog :rows (nle::sessions-fold-rows fold) :kind :sessions)))
    (flet ((window (selection offset)
             (setf (nle::picker-dialog-selection dialog) selection
                   (nle::picker-dialog-view-offset dialog) offset)
             (nth-value 1 (nle::dialog-body-viewport dialog 5))))
      (is (= 0 (window 1 10)) "Up dragging the window onto the first session brings its day with it")
      (is (= 1 (window 1 1)) "a window the wheel put there stays")
      (is (= 2 (window 2 10)) "a row under no heading is the top row as before"))
    (setf (nle::picker-dialog-selection dialog) 1 (nle::picker-dialog-view-offset dialog) 0)
    (let ((rows (nle::docked-dialog-list-rows dialog 100 nil 5)))
      (is (search "─ █" (first rows)) "the heading's rule stops a cell short of the bar")
      ;; 100 columns: 3 of padding, the row to column 93, a blank, the bar at 95.
      (is (equal "msgs  " (subseq (second rows) 90 96)) "a row's right edge stops a cell short of it too")
      (is (every (lambda (row) (= 100 (nle::text-display-width row))) rows) "and every row still spans the panel"))))
