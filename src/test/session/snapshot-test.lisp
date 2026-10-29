;;;; snapshot-test.lisp --- shadow-git workspace snapshot store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims:
;;;;   SN1. The store initializes bare with the pinned config (gc.auto=0).
;;;;   SN2. Capture returns 40-hex tree shas; an unchanged workspace yields
;;;;        the same sha and anchors no second commit.
;;;;   SN3. Restore round-trips modified, deleted, and added paths, strictly
;;;;        per path, and -z parsing survives spaces in names.
;;;;   SN4. A foreign write is skipped and reported, never clobbered; a path
;;;;        the session never wrote restores without a claim.
;;;;   SN5. Home-owned snapshot storage, even inside the work tree, sqlite
;;;;        state and a workspace's own .nodecode/ stay out of capture.
;;;;   SN6. Two sessions share objects but never an index file.
;;;;   SN7. Unavailability degrades: no git means no store, and the durable
;;;;        capture fact still records with an unavailable-reason.
;;;;   SN8. Repack preserves restorability and the interval/lock gates work.
;;;;   SN9. One batched process answers every tree exactly what a process
;;;;        per tree answered, and refuses rather than guess.
;;;;
;;;; Hosts without git run every test body against the degradation contract
;;;; instead of skipping — RUN-ALL has no skip concept, and the degraded
;;;; behavior is itself a contract.

(in-package #:nodecode.test)

(defun %write-file (dir &rest names-and-contents)
  "Write each NAME and CONTENT pair under DIR in order; a NIL CONTENT deletes NAME."
  (loop for (name content) on names-and-contents by #'cddr
        for path = (merge-pathnames name dir)
        do (if content (write-temp-file path content) (delete-file path))))

(defun %read-file (dir name &aux (path (merge-pathnames name dir)))
  (when (probe-file path)
    (uiop:read-file-string path)))

(defun %store-file (store name)
  "NAME inside STORE's shadow git directory."
  (nlk::snapshot-store-file store name))

(defun %anchored-capture (store label &aux (sha (nlk:capture-workspace-tree store)))
  "Capture STORE's workspace tree, anchor it under LABEL, and answer its sha."
  (nlk:anchor-tree store sha label)
  sha)

(defun %claims-at (store tree changes)
  "Each of CHANGES' paths hashed in TREE: the expectations a restore claims."
  (mapcar (lambda (change) (nlk:hash-in-tree store tree (getf change :path))) changes))

(defun %diff-entry (path files)
  "The entry for PATH among a diff document's FILES, or NIL."
  (find path (coerce files 'list) :key (lambda (file) (gethash "path" file)) :test #'equal))

(defmacro with-snapshot-workspace ((store-var dir-var
                                    &key (session-id "snap-test") (tag "snap")) &body body)
  "A fresh workspace directory with an opened snapshot store (or NIL where
git is unavailable), removed afterwards whatever happens."
  `(with-temp-directory (,dir-var ,tag)
     (let ((,store-var (nlk:ensure-snapshot-store (namestring ,dir-var)
                                                  ,session-id)))
       (declare (ignorable ,store-var))
       ,@body)))

(defmacro with-git-workspace ((store-var dir-var
                               &key (session-id "snap-test")) &body body)
  "WITH-SNAPSHOT-WORKSPACE carrying the git-availability gate."
  ;; BODY runs where git is present; where it is not, the header's no-skip
  ;; rule is served by asserting the degradation contract instead. A test
  ;; whose degraded shape is richer than "no store" writes its own gate.
  `(with-snapshot-workspace (,store-var ,dir-var :session-id ,session-id)
     (if (nlk:git-available-p)
         (progn ,@body)
         (is (null ,store-var) "git unavailable: ensure degrades to nil"))))

(defmacro with-captured-turn ((store dir turn session-id) &body body)
  "BODY over a git workspace DIR holding a.txt and its shadow STORE, captures
on, with TURN a turn admitted on SESSION-ID -- a session over DIR -- and
completed once BODY is done."
  `(with-git-workspace (,store ,dir :session-id ,session-id)
     (%write-file ,dir "a.txt" "one")
     (nlk:create-session :id ,session-id :cwd (namestring ,dir))
     (with-workspace-captures (t)
       (with-durable-turn (,turn ,session-id :input "go" :complete t)
         ,@body))))

(deftest snapshot-store-initializes-bare-with-pinned-config (with-git-workspace (store dir))
  (is (nlk:snapshot-store-p store) "store opens")
  (is (probe-file (%store-file store "config")))
  (let ((config (uiop:read-file-string (%store-file store "config"))))
    (is (search "bare = false" config) "core.bare false")
    ;; gc.auto=0 is load-bearing: auto-gc between capture and undo
    ;; could prune a tree the undo needs.
    (is (search "auto = 0" config) "gc.auto pinned to 0")))

(deftest snapshot-capture-returns-tree-shas-and-noop-is-free (with-git-workspace (store dir))
  (%write-file dir "a.txt" "one")
  (let ((first-sha (%anchored-capture store "test anchor")))
    (is (= 40 (length first-sha)) "40-hex tree sha")
    (let ((second-sha (nlk:capture-workspace-tree store)))
      (is (equal first-sha second-sha))
      ;; The tip-tree guard: anchoring the same tree twice must not
      ;; grow the chain.
      (is (stringp (nlk:anchor-tree store second-sha "again"))))
    (%write-file dir "a.txt" "two")
    (is (not (equal first-sha (nlk:capture-workspace-tree store))))))

(defmacro git-asked (form &aux (asked (gensym "ASKED")) (value (gensym "VALUE")))
  "The git subcommands the store asked for while FORM ran, in order, and FORM's value."
  ;; The staging call and the tree call are one pair per capture, and what
  ;; these gates are about is when the second of them, or both, is not made.
  `(let* ((,asked '())
          (,value (with-stubbed-fdefinition (nlk::run-snapshot-git (store arguments &rest keys)
                                              (push (first arguments) ,asked)
                                              (apply original store arguments keys))
                    ,form)))
     (values (reverse ,asked) ,value)))

(deftest a-capture-that-staged-nothing-writes-no-tree (with-git-workspace (store dir))
  ;; One index content is one tree: a capture whose staging left the index
  ;; alone answers the tree already written for it, and the second git call
  ;; of the pair is not made. The identity is the index's content and never
  ;; its stat — git publishes an index by renaming index.lock over it, so
  ;; the inode can be the freed one, a one-for-one edit leaves the size
  ;; alone, and two captures fall inside one second.
  (%write-file dir "a.txt" "one")
  ;; A file written in the same second as the index is racily clean: git
  ;; re-reads it and rewrites the index at every staging until the second
  ;; turns over, and the pair runs while it does. Age the file and the
  ;; settled workspace — what a turn spends most of its calls in — is what
  ;; this gate reads.
  (sb-posix:utimes (namestring (merge-pathnames "a.txt" dir)) 0 0)
  (let ((first-sha (nlk:capture-workspace-tree store)))
    (is (= 40 (length first-sha)) "the first capture writes a tree")
    (nlk:bind (((asked second-sha) (git-asked (nlk:capture-workspace-tree store))))
      (is (equal '("add") asked)
          (format nil "staging alone; no write-tree behind it: ~a" asked))
      (is (equal first-sha second-sha) "and the tree it answers is that tree"))
    ;; Same length, same second, and the index git writes may land on the
    ;; inode the last one freed: only the content tells these apart.
    (%write-file dir "a.txt" "two")
    (nlk:bind (((asked changed) (git-asked (nlk:capture-workspace-tree store))))
      (is (equal '("add" "write-tree") asked)
          (format nil "a staged change writes its tree: ~a" asked))
      (is (not (equal first-sha changed)) "which is a tree of its own"))))

(deftest workspace-diff-document-shapes-modified-added-deleted-binary ()
  ;; The complete diff document between two captured trees: per-file numstat
  ;; plus git-owned unified patch text, binary files flagged and hunkless,
  ;; equal trees folding to NIL.
  (with-git-workspace (store dir)
    (%write-file dir "a.txt" (format nil "one~%two~%three~%") "gone.txt" (format nil "doomed~%"))
    (let ((pre (%anchored-capture store "pre")))
      (%write-file dir "a.txt" (format nil "one~%TWO~%three~%four~%")
                   "new.txt" (format nil "fresh~%") "gone.txt" nil)
      (alexandria:write-byte-vector-into-file
       (coerce #(0 1 2 0 255) '(vector (unsigned-byte 8))) (merge-pathnames "blob.bin" dir))
      (let ((document (nlk:workspace-diff-document store pre (nlk:capture-workspace-tree store))))
        (is (hash-table-p document) "changed trees produce a document")
        (let ((files (gethash "files" document)))
          (is (and (vectorp files) (= 4 (length files))))
          (let ((modified (%diff-entry "a.txt" files)))
            (is (eql 2 (gethash "added" modified)) "numstat added count")
            (is (eql 1 (gethash "removed" modified)) "numstat removed count")
            (let ((patch (gethash "patch" modified)))
              (is (search "@@ -" patch) "the patch carries hunk headers")
              (is (search "+TWO" patch) "the patch carries the change")))
          (let ((binary (%diff-entry "blob.bin" files)))
            (is (eq t (gethash "binary" binary)) "binary files are flagged")
            (is (null (gethash "patch" binary)) "and carry no patch"))
          (is (search "-doomed" (gethash "patch" (%diff-entry "gone.txt" files))))
          (is (search "+fresh" (gethash "patch" (%diff-entry "new.txt" files)))))
        (is (null (gethash "truncated" document)))
        (is (null (nlk:workspace-diff-document store pre pre)))))))

(deftest workspace-diff-document-keeps-entire-patches-and-file-list (with-git-workspace (store dir))
  (%write-file dir "big.txt" (format nil "seed~%"))
  (let ((pre (%anchored-capture store "pre")))
    ;; More than 200 lines and 64 KiB; more than 50 files, with patches
    ;; beyond file ten. A file disclosure must never open to missing text.
    (%write-file dir "big.txt" (format nil "~{~a~%~}tail-marker~%" (make-list 300 :initial-element (make-string 240 :initial-element #\x))))
    (dotimes (i 51) (%write-file dir (format nil "small-~2,'0d.txt" i) (format nil "tiny ~d~%" i)))
    (let* ((document (nlk:workspace-diff-document store pre (nlk:capture-workspace-tree store)))
           (files (gethash "files" document))
           (big (%diff-entry "big.txt" files)))
      (is (= 52 (length files)))
      (is (= 301 (gethash "added" big)))
      (is (search "+tail-marker" (gethash "patch" big)))
      (is (every (lambda (file) (stringp (gethash "patch" file))) files))
      (is (null (gethash "truncated" document))))))

(deftest snapshot-restore-roundtrips-modified-deleted-added (with-git-workspace (store dir))
  (%write-file dir "keep me.txt" "original with spaces" "doomed.txt" "will be deleted")
  (let ((before (%anchored-capture store "before")))
    ;; Mutate: modify one, delete one, add one — then capture, so the
    ;; session CLAIMS the mutated state. Expectations come from that head
    ;; tree, exactly as RESTORE-TO-CHECKPOINT derives them.
    (%write-file dir "keep me.txt" "changed" "doomed.txt" nil
                 "new.txt" "added later")
    (let* ((head (%anchored-capture store "head"))
           (changes (nlk:changed-paths store before)))
      (is (= 3 (length changes)) "three differences detected")
      (multiple-value-bind (restored conflicts)
          (nlk:restore-paths store before changes (%claims-at store head changes))
        (is (null conflicts) "the claimed state restores conflict-free")
        (is (= 3 (length restored)) "all three paths restored"))
      (is (equal "original with spaces" (%read-file dir "keep me.txt")))
      (is (equal "will be deleted" (%read-file dir "doomed.txt")))
      (is (null (%read-file dir "new.txt"))))))

(deftest snapshot-no-claim-restores-only-into-absence (with-git-workspace (store dir))
  ;; EXPECTED NIL = the session recorded the path absent. The only NIL-claim
  ;; state a restore may produce from is absence itself: recreating into
  ;; empty space clobbers nothing, while deleting or overwriting on-disk
  ;; content the session never captured destroys foreign work (the
  ;; unclaimed-:added deletion hole this contract closes).
  (%write-file dir "recreate.txt" "target content" "overwrite.txt" "target content")
  (let ((target (%anchored-capture store "target")))
    ;; Foreign hands: delete one, rewrite one, create one — none of it
    ;; ever captured by this session, so every expectation is NIL.
    (%write-file dir "recreate.txt" nil "overwrite.txt" "foreign content"
                 "foreign-new.txt" "foreign file")
    (let ((changes (nlk:changed-paths store target)))
      (is (= 3 (length changes)) "three differences detected")
      (multiple-value-bind (restored conflicts)
          (nlk:restore-paths store target changes
                             (make-list (length changes)))
        (is (equal '("recreate.txt")
                   (mapcar (lambda (change) (getf change :path)) restored)))
        (is (= 2 (length conflicts)))
        (is (every (lambda (conflict)
                     (eq :foreign-write (getf conflict :reason)))
                   conflicts)))
      (is-table (in want) (equal want (%read-file dir in)) ("recreate.txt" "target content")
        ("overwrite.txt" "foreign content") ("foreign-new.txt" "foreign file")))))

(deftest snapshot-foreign-write-conflict-skips-path (with-git-workspace (store dir))
  (%write-file dir "shared.txt" "session content")
  (let ((session-tree (%anchored-capture store "session state")))
    ;; A foreign writer changes the file after the session's last
    ;; recorded write.
    (%write-file dir "shared.txt" "foreign content")
    (let ((expected (nlk:hash-in-tree store session-tree "shared.txt")))
      (is (stringp expected) "session's recorded hash resolves")
      (multiple-value-bind (restored conflicts)
          (nlk:restore-paths store session-tree (list (list :path "shared.txt" :status :modified))
                             (list expected))
        (is (null restored) "conflicted path not restored")
        (is (= 1 (length conflicts)) "conflict reported")
        (is (eq :foreign-write (getf (first conflicts) :reason))))
      (is (equal "foreign content" (%read-file dir "shared.txt"))))))

(deftest snapshot-excludes-own-store-and-sqlite-patterns (with-git-workspace (store dir))
  (%write-file dir "real.txt" "content" "state.sqlite" "not captured"
               "state.sqlite-wal" "not captured" ".nodecode/state" "not captured")
  (is (not (nlk::path-within-p (nlk:snapshot-store-git-dir store)
                               (string-right-trim "/" (namestring dir)))) "the store lives outside the work tree it captures")
  (let ((sha (nlk:capture-workspace-tree store)))
    (is (stringp (nlk:hash-in-tree store sha "real.txt")))
    (is-table (path) (null (nlk:hash-in-tree store sha path))
      ("state.sqlite" "sqlite file excluded")
      ("state.sqlite-wal" "sqlite sidecar excluded")
      (".nodecode/state" "a workspace's own .nodecode/ excluded"))))

(deftest snapshot-excludes-nested-storage-across-captures (with-temp-directory (dir "snap-nested"))
  (let ((nlk::*home* (merge-pathnames "agent_home/.nodecode/" dir))
        (other-dir (merge-pathnames "other/" dir)))
    (%write-file dir ".gitignore" (format nil "!agent_home/.nodecode/snapshots/**~%")
                 "real.txt" "content" "other/real.txt" "other workspace"
                 "agent_home/.nodecode/notes.txt" "outside snapshot storage"
                 "agent_home-copy/.nodecode/snapshots/keep.txt" "not the storage path")
    (let ((store (nlk:ensure-snapshot-store (namestring dir) "nested"))
          (other (nlk:ensure-snapshot-store (namestring other-dir) "neighbor")))
      (cond ((nlk:git-available-p)
             (%anchored-capture other "neighbor")
             (let ((previous nil))
               (loop for workspace in (list dir (merge-pathnames "other/../" dir) dir) do
                 (setf store (nlk:ensure-snapshot-store (namestring workspace) "nested"))
                 (let* ((sha (%anchored-capture store "nested"))
                        (paths (nlk::run-snapshot-git store (list "ls-tree" "-r" "--name-only" sha))))
                   (is (not (search "agent_home/.nodecode/snapshots/" paths)) "neither this store nor neighboring stores enter the tree")
                   (dolist (path '("real.txt" "other/real.txt" "agent_home/.nodecode/notes.txt"
                                   "agent_home-copy/.nodecode/snapshots/keep.txt"))
                     (is (stringp (nlk:hash-in-tree store sha path)) "workspace files outside snapshot storage remain captured"))
                   (when previous
                     (is (equal previous sha) "unchanged captures do not ingest their own index or objects"))
                   (setf previous sha)))))
            (t (is (and (null store) (null other)) "git unavailable: both stores degrade to nil"))))))

(deftest snapshot-reopen-removes-previously-indexed-storage (with-temp-directory (dir "snap-indexed"))
  (let ((nlk::*home* (merge-pathnames "agent_home/.nodecode/" dir))
        (captured-path "agent_home/.nodecode/snapshots/legacy/objects/blob"))
    (%write-file dir "real.txt" "content" captured-path "previously captured snapshot object")
    (let ((store (nlk:ensure-snapshot-store (namestring dir) "indexed")))
      (cond ((nlk:git-available-p)
             (nlk::run-snapshot-git store (list "add" "-f" "--" captured-path))
             (let ((sha (%anchored-capture store "old index")))
               (is (stringp (nlk:hash-in-tree store sha captured-path)) "the fixture starts with snapshot storage already tracked"))
             (let* ((reopened (nlk:ensure-snapshot-store (namestring dir) "indexed"))
                    (sha (nlk:capture-workspace-tree reopened)))
               (is (null (nlk:hash-in-tree reopened sha captured-path)) "reopening clears captured storage from the existing index")
               (is (stringp (nlk:hash-in-tree reopened sha "real.txt")) "ordinary tracked workspace files stay in the index")
               (is (equal "previously captured snapshot object" (%read-file dir captured-path)) "untracking does not delete snapshot files from disk")
               (is (equal sha (nlk:capture-workspace-tree reopened)) "later captures keep the storage out")))
            (t (is (null store) "git unavailable: ensure degrades to nil"))))))

(deftest a-work-tree-cannot-ship-the-snapshot-store ()
  ;; SN5. A cloned repo may carry .nodecode/workspace_snapshots/ -- the folder
  ;; the store once lived in -- as a whole bare repository whose config names
  ;; an fsmonitor hook and a clean filter its .gitattributes switches on, both
  ;; a program git runs at the first ADD. The store is the home's: nothing the
  ;; work tree ships is read as its configuration, and the capture lands.
  (with-temp-directory (outside "planted-driver")
    (with-temp-directory (dir "planted")
      (let ((planted (merge-pathnames ".nodecode/workspace_snapshots/" dir))
            (driver (merge-pathnames "driver" outside))
            (ran (merge-pathnames "ran" outside)))
        (when (nlk:git-available-p)
          (ensure-directories-exist planted)
          (uiop:run-program (list nlk::*git-program* "init" "--bare" "--quiet"
                                  (namestring planted)))
          (write-temp-file driver (format nil "#!/bin/sh~%echo \"$0 $*\" >> '~a'~%cat~%"
                                          (namestring ran)))
          #-win32 (sb-posix:chmod (namestring driver) #o755)
          (with-open-file (config (merge-pathnames "config" planted)
                                  :direction :output :if-exists :append)
            (format config "[core]~%~cfsmonitor = ~a~%[filter \"planted\"]~%~cclean = ~a~%"
                    #\Tab (namestring driver) #\Tab (namestring driver)))
          (%write-file dir ".gitattributes" (format nil "* filter=planted~%") "a.txt" "one"))
        (let ((store (nlk:ensure-snapshot-store (namestring dir) "planted")))
          (cond ((nlk:git-available-p)
                 (is (nlk:snapshot-store-p store) "a store opens over the work tree")
                 (let ((sha (nlk:capture-workspace-tree store)))
                   (is (stringp (nlk:hash-in-tree store sha "a.txt")) "the capture lands")
                   (is (null (nlk:hash-in-tree store sha ".nodecode/workspace_snapshots/config")) "the planted store is not captured as content either"))
                 (is (null (probe-file ran)) "no hook or filter the work tree shipped ran"))
                (t (is (null store) "git unavailable: ensure degrades to nil"))))))))

(deftest snapshot-per-session-index-isolation ()
  (with-git-workspace (store-a dir :session-id "session-a")
    (let ((store-b (nlk:ensure-snapshot-store (namestring dir) "session-b")))
      (%write-file dir "shared.txt" "same bytes")
      (is (equal (nlk:capture-workspace-tree store-a)
                 (nlk:capture-workspace-tree store-b)) "same workspace, same content address")
      (is (probe-file (%store-file store-a "index-session-a")))
      (is (probe-file (%store-file store-a "index-session-b"))))))

(deftest snapshot-unavailable-degrades-and-still-records-fact (with-temp-store (store))
  (with-snapshot-workspace (snapshot-store dir)
    (nlk:create-session :id "degraded" :cwd (namestring dir))
    (with-durable-turn (turn "degraded" :input "hello" :complete t)
      ;; Force unavailability regardless of the host; re-enable captures
      ;; (the runner switches them off) — the workspace here is a temp dir.
      (with-workspace-captures (t)
        (nlk:bind ((nlk::*git-program* "/nonexistent/git-binary") (nlk::*git-available* :unknown)
                   (result (nlk:checkpoint-capture-boundary
                            turn
                            :boundary-kind "user_message"
                            :anchor-position 1)))
          (is-shape result (consp is "capture returns a result plist")
            (:tree-sha null "no tree sha without git") (:unavailable eq :git-missing))))
      (setf nlk::*git-available* :unknown)
      (let ((captures (nlk:session-workspace-captures "degraded")))
        (is (= 1 (length captures)) "the durable fact recorded anyway")
        (is (null (getf (first captures) :tree-sha)))
        (is (nlk:event-kind-replay-suppressed-p "session.workspace_captured"))))))

(deftest snapshot-repack-preserves-restorability (with-git-workspace (store dir))
  (%write-file dir "file.txt" "version one")
  (let ((oldest (%anchored-capture store "v1")))
    (%write-file dir "file.txt" "version two")
    (%anchored-capture store "v2")
    (%write-file dir "file.txt" "version three")
    (let ((head (%anchored-capture store "v3")))
      (is (nlk:repack-store store) "repack succeeds")
      (is (probe-file (%store-file store "objects/pack/")))
      ;; The oldest tree must still restore byte-identical from the pack.
      ;; Expectations claim the captured head state, as restores do.
      (let ((changes (nlk:changed-paths store oldest)))
        (is (null (nth-value 1 (nlk:restore-paths store oldest changes
                                                  (%claims-at store head changes)))))))
    (is (equal "version one" (%read-file dir "file.txt")))))

(deftest snapshot-repack-lock-contention-skips (with-git-workspace (store dir))
  (let ((lock-path (%store-file store "repack.lock")))
    ;; A fresh lock held by someone else: acquisition must fail.
    (write-temp-file lock-path (format nil "99999~%"))
    (is (null (nlk::acquire-repack-lock store)))
    ;; A stale lock (older than the horizon) is broken and retaken.
    (sb-posix:utimes (namestring lock-path) 0 0)
    (is (nlk::acquire-repack-lock store))
    (ignore-errors (delete-file lock-path))))

(deftest snapshot-restore-to-checkpoint-returns-preimage (with-snapshot-workspace (store dir))
  ;; Restore honors the capture kill switch, so this test opts in
  ;; explicitly — its workspace is an isolated temp directory.
  (with-workspace-captures (t)
   (flet ((restore (target head)
            (nlk:restore-to-checkpoint target head
                                       :workspace (namestring dir) :session-id "snap-test")))
     (cond ((nlk:git-available-p)
            (%write-file dir "work.txt" "checkpoint state")
            (let ((target (%anchored-capture store "target")))
              (%write-file dir "work.txt" "current state")
              ;; The session's last recorded write IS the current state — an
              ;; uncaptured change would be a foreign write by contract.
              (let ((outcome (restore target (%anchored-capture store "head"))))
                (is (eq :restored (getf outcome :status)) "restore completes")
                (let ((preimage (getf outcome :preimage-tree-sha)))
                  (is (and (stringp preimage) (= 40 (length preimage))))
                  (is (not (equal preimage target))))
                (is (equal "checkpoint state" (%read-file dir "work.txt"))))))
           ;; Degradation shape without git.
           (t (let ((outcome (restore "0000000000000000000000000000000000000000" nil)))
                (is (eq :degraded (getf outcome :status)))))))))

(deftest restore-refuses-while-captures-disabled (with-workspace-captures (nil))
  ;; The capture kill switch gates the restore path too: restore's preimage
  ;; capture runs ADD -A exactly like a boundary capture, so with captures
  ;; off nothing may touch the workspace's git state.
  (let ((outcome (nlk:restore-to-checkpoint
                  "0000000000000000000000000000000000000000" nil
                  :workspace "/definitely/not/a/workspace"
                  :session-id "captures-off")))
    (is-shape outcome (:status eq :skipped) (:degradation-reason eq :captures-disabled))))

(deftest anchor-noop-with-warm-tip-cache-spawns-no-git (with-git-workspace (store dir))
  (%write-file dir "a.txt" "one")
  (let* ((sha (nlk:capture-workspace-tree store))
         (tip (nlk:anchor-tree store sha "warm the cache")))
    (nlk:bind (((asked again) (git-asked (nlk:anchor-tree store sha "noop"))))
      (is (equal tip again))
      (is (null asked)))))

(deftest anchor-lost-swap-chains-onto-the-foreign-tip (with-git-workspace (store dir))
  (%write-file dir "a.txt" "one")
  (flet ((git (&rest args) (nlk::git-line store args)))
    (let ((sha1 (%anchored-capture store "ours")))
      ;; A second process advances the ref behind the cache's back.
      (let ((foreign (git "commit-tree" sha1 "-m" "foreign tip")))
        (git "update-ref" nlk::+snapshot-anchor-ref+ foreign)
        (%write-file dir "a.txt" "two")
        (let* ((sha2 (nlk:capture-workspace-tree store))
               (commit (nlk:anchor-tree store sha2 "after foreign move")))
          (is (equal foreign (git "rev-parse" (format nil "~a^" commit)))))))))

(deftest staged-tree-diffs-batch-answers-what-each-tree-answers-alone ()
  ;; The batched read side is a spawn-count optimisation and nothing else, so
  ;; its whole claim is that one process answers exactly what N processes
  ;; answered — with real paths through the framing (a space in a name, a
  ;; deletion, the empty tree) and a refusal rather than a guess when a tree
  ;; is not a sha.
  (with-git-workspace (store dir)
    (%write-file dir "a.txt" "one" "with space.txt" "one")
    (let ((first-tree (nlk:capture-workspace-tree store)))
      (%write-file dir "a.txt" "two" "b.txt" "new")
      (let ((second-tree (nlk:capture-workspace-tree store)))
        (delete-file (merge-pathnames "with space.txt" dir))
        (nlk::run-snapshot-git store (list "add" "-A"))
        (let* ((trees (list first-tree second-tree nlk::+git-empty-tree-sha+))
               (alone (mapcar (lambda (tree)
                                (nlk::staged-changed-paths store tree))
                              trees))
               (batch (nlk::staged-tree-diffs-batched store trees)))
          (is batch "the batch runs")
          (is (equal alone
                     (mapcar (lambda (tree) (gethash tree batch)) trees)))
          (is (null (nlk::staged-tree-diffs-batched store (list "nope"))))
          (is (null (nlk::staged-tree-diffs-batched store '()))))))))

;;; --- the walk that cannot succeed costs one walk -------------------------
;;; Without the gates, a session whose cwd is $HOME would walk every boundary
;;; and fail, leaving capture facts `capture-failed'. Three gates: $HOME is
;;; refused before any git runs, a failure is remembered per index for a
;;; window, and a lock a dead process left is swept.

(deftest capture-failure-is-remembered-per-index (with-temp-store ())
  (with-captured-turn (store dir turn "memo")
    (let ((spawns 0))
      (clrhash nlk::*capture-refusals*)
      (nlk:with-cleanup ((clrhash nlk::*capture-refusals*))
        (with-stubbed-fdefinition
            (nlk::run-snapshot-git (store args &key (with-work-tree t))
             (incf spawns)
             (error 'nlk:snapshot-git-failed
                    :arguments args :exit-status 128
                    :stderr "fatal: adding files failed"
                    :detail "nonzero exit"))
          (let ((first (nlk:checkpoint-capture-boundary
                        turn :boundary-kind "user_message"
                        :anchor-position 1)))
            (is (eq :capture-failed (getf first :unavailable))))
          (let ((walked spawns))
            (is (plusp walked) "the first boundary spawned git")
            (let ((second (nlk:checkpoint-capture-boundary
                           turn :boundary-kind "tool_result"
                           :tool-call-id "c1" :anchor-position 2)))
              (is (eq :capture-failed (getf second :unavailable))))
            (is (= walked spawns) "and spawns no git")
            (is (null (nlk:open-workspace-bracket turn :label "c2")))
            (is (= walked spawns) "without a git of its own")))))
    (is (= 2 (length (nlk:session-workspace-captures "memo"))))))

(deftest home-is-never-a-workspace-to-stage ()
  ;; ADD -A over $HOME writes a blob for every file under it into the shadow
  ;; store: refused before any git runs, with a reason the fact records.
  (cond ((nlk:git-available-p)
         (multiple-value-bind (store reason)
             (nlk:ensure-snapshot-store (namestring (user-homedir-pathname))
                                        "home-session")
           (is (null store) "no store opens over the home directory")
           (is (eq :workspace-too-large reason) "and the reason names it"))
         #-win32
         (multiple-value-bind (store reason)
             (nlk:ensure-snapshot-store "/" "root-session")
           (is (null store) "no store opens over a filesystem root")
           (is (eq :workspace-too-large reason) "with the same reason")))
        (t (is (null (nlk:ensure-snapshot-store (namestring (user-homedir-pathname))
                                                "home-session"))))))

(deftest stale-index-lock-is-swept-on-open (with-git-workspace (store dir :session-id "locked"))
  ;; An index lock left behind would make every capture of its session fail.
  ;; No git of ours holds the lock past its walltime bound, so an older lock
  ;; is a dead process's.
  (let ((lock (concatenate 'string (nlk::snapshot-store-index-path store)
                           ".lock")))
    (write-temp-file lock (format nil "274630~%"))
    (nlk:ensure-snapshot-store (namestring dir) "locked")
    (is (probe-file lock) "a young lock may be a running capture's: left alone")
    #-win32
    (let ((stale (- (nlk:unix-now) nlk::+snapshot-git-timeout-seconds+ 120)))
      (sb-posix:utime lock stale stale)
      (nlk:ensure-snapshot-store (namestring dir) "locked")
      (is (null (probe-file lock)) "a lock older than the git bound is swept")
      (%write-file dir "a.txt" "one")
      (is (= 40 (length (nlk:capture-workspace-tree store)))))))

(deftest deferred-boundary-capture-lands-before-the-turns-next-fact (with-temp-store ())
  ;; DEFER T hands the git walk to a thread of its own and the fact to
  ;; DEFER-TURN-FACT: the round goes on, and the capture is durable — in
  ;; its old place, ahead of the turn's next fact — before that fact is.
  (with-captured-turn (store dir turn "deferred")
    (let ((walker nil))
      (with-stubbed-fdefinition (nlk:capture-workspace-tree (store)
                                 (setf walker (bt2:current-thread))
                                 (funcall original store))
        (is (eq :deferred (nlk:checkpoint-capture-boundary
                           turn :boundary-kind "tool_result"
                           :tool-call-id "c1" :anchor-position 3 :defer t)))
        ;; The turn's next durable fact settles the capture first.
        (nlk:record-tool-call turn "c2" "eval" "{}"))
      (is (and walker (not (eq walker (bt2:current-thread)))))
      (let* ((kinds (mapcar #'nlk:event-kind (session-events "deferred" :created nil)))
             (capture (position "session.workspace_captured" kinds :test #'equal))
             (call (position "turn.tool_call_started" kinds :test #'equal)))
        (is (and capture call (< capture call))))
      (let ((facts (nlk:session-workspace-captures "deferred")))
        (is (= 1 (length facts)) "one capture fact")
        (is (= 40 (length (or (getf (first facts) :tree-sha) ""))))
        (is (equal "c1" (getf (first facts) :tool-call-id)) "for its call"))
      (is (zerop (hash-table-count nlk::*deferred-turn-facts*))))))

(deftest a-boundary-given-its-tree-anchors-without-a-walk (with-temp-store ())
  ;; A tool unit's checkpoint is handed the bracket's post-tree: it anchors
  ;; that sha and records the fact, and never stages or writes a tree of
  ;; its own. With the tip cache warm, a second boundary on the same tree
  ;; spawns no git at all.
  (with-captured-turn (store dir turn "given")
    (let ((tree (nlk:capture-workspace-tree store)))
      (nlk:bind (((asked first) (git-asked (nlk:checkpoint-capture-boundary
                                            turn :boundary-kind "tool_result" :tool-call-id "c1"
                                            :tree-sha tree :anchor-position 3))))
        (is-shape first (:tree-sha tree "the fact carries the given tree") (:unavailable null))
        (is (null (intersection '("add" "write-tree") asked :test #'equal))))
      (let ((asked (git-asked (nlk:checkpoint-capture-boundary
                               turn :boundary-kind "tool_result" :tool-call-id "c2"
                               :tree-sha tree :anchor-position 4))))
        (is (null asked) "a no-op anchor on a warm tip spawns nothing"))
      (let ((facts (nlk:session-workspace-captures "given")))
        (is (= 2 (length facts)))
        (is (every (lambda (fact) (equal tree (getf fact :tree-sha))) facts))))))

(deftest a-write-between-brackets-belongs-to-neither (with-temp-store ())
  ;; Every bracket walks at dispatch: a write landing between two
  ;; brackets — another process's — is neither's.
  (with-captured-turn (store dir turn "bracket")
    (let ((walks 0))
      (with-stubbed-fdefinition (nlk:capture-workspace-tree (store)
                                 (incf walks)
                                 (funcall original store))
        (multiple-value-bind (store-1 pre-1) (nlk:open-workspace-bracket turn :label "c1")
          (is (and store-1 (= 40 (length pre-1))))
          (is (= 1 walks) "the bracket walks its baseline")
          (%write-file dir "a.txt" "two")
          (multiple-value-bind (post-1 document-1)
              (nlk:close-workspace-bracket store-1 pre-1)
            (is (= 2 walks) "and one walk closes it")
            (is (not (equal pre-1 post-1)))
            (is (%diff-entry "a.txt" (gethash "files" document-1)))
            ;; another process writes between the brackets
            (%write-file dir "b.txt" "foreign")
            (multiple-value-bind (store-2 pre-2) (nlk:open-workspace-bracket turn :label "c2")
              (is (= 3 walks) "the next bracket walks at dispatch")
              (is (not (equal post-1 pre-2)) "and opens on the foreign write")
              (%write-file dir "c.txt" "mine")
              (nlk:bind (((_ document-2)
                          (nlk:close-workspace-bracket store-2 pre-2)))
                (let ((files (gethash "files" document-2)))
                  (is (%diff-entry "c.txt" files) "its own write is attributed")
                  (is (null (%diff-entry "b.txt" files))))))
            ;; an unchanged tree closes with no document
            (nlk:bind (((store-3 pre-3) (nlk:open-workspace-bracket turn :label "c3"))
                       ((post-3 document-3) (nlk:close-workspace-bracket store-3 pre-3)))
              (is (equal pre-3 post-3))
              (is (null document-3)))))))))
