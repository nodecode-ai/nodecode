;;;; tool-summary-test.lisp --- proofs for what a tool call did, in words
;;;; (tool-summary.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The shapes are the ones the operator's store actually carries:
;;;; a single (sh ...) — often behind cd X && — the edit and write verbs,
;;;; noted snippets, and snippets that call something no table names. Each table row states the words a card would carry and whether
;;;; the call groups.

(in-package #:nodecode.test)

(nlk:access (activity nle::activity) (noted nle::activity) (static nle::activity))

(defun eval-args (form)
  "Complete eval tool arguments carrying FORM."
  (nlk:encode-json-object (nlk:make-json-object "form" form)))

(defun snippet-activity (form &key metadata error-p)
  (nle::tool-call-activity "eval" form :metadata metadata :error-p error-p))

(defun receipts (&rest calls)
  "A result fact's metadata carrying CALLS, each one receipt's key/value list."
  (nlk:make-json-object "calls" (map 'vector (lambda (call) (apply #'nlk:make-json-object call))
                                     calls)))

(deftest shell-commands-name-what-they-read-search-and-run ()
  (is-table (command expected) (equal 'expected (nle::shell-command-entries command))
    ("cat src/x.lisp" ((:read "src/x.lisp" nil)))
    ("cd /repo && rg -n 'tool-row' src" ((:search "tool-row" nil)))
    ("cd /repo; sed -n '1,80p' src/tui/ui.lisp"
     ((:read "src/tui/ui.lisp" "1-80")))
    ("head -n 40 README.md" ((:read "README.md" "1-40")))
    ("sed -i 's/a/b/' notes.txt" ((:edit "notes.txt" nil)))
    ("ls -la src" ((:list "src" nil)))
    ("ls" ((:list "." nil)))
    ("just lint" ((:command "just lint" nil)))
    ("rg foo src | head -20" ((:search "foo" nil)))
    ("grep -rn -e needle ." ((:search "needle" nil)))
    ("find . -name '*.lisp' -type f" ((:search "*.lisp" nil)))
    ("find . -name '*.fasl' -delete"
     ((:command "find . -name '*.fasl' -delete" nil)))
    ("cat a.txt > b.txt" ((:read "a.txt" nil) (:write "b.txt" nil)))
    ("rg x 2>/dev/null" ((:search "x" nil)))
    ("rg x 2>&1 | tail -5" ((:search "x" nil)))
    ("timeout 60 rg -n x src" ((:search "x" nil)))
    ("FOO=1 git status" ((:command "FOO=1 git status" nil)))
    ("git grep -n pattern" ((:search "pattern" nil)))
    ("bash -lc 'cd x && cat y'" ((:read "y" nil)))
    ("rg \"$(cat list)\" src" ((:command "rg \"$(cat list)\" src" nil)))
    ("sleep 5" ((:wait "5" nil))))
  (let ((entries (nle::shell-command-entries
                  (format nil "cat > out.txt <<'EOF'~%rm -rf /~%just lint~%EOF~%wc -l out.txt"))))
    (is (equal '((:write "out.txt" nil) (:read "out.txt" nil)) entries)
        (format nil "a here-document's body is text, never commands: ~s" entries))))

(deftest a-title-phrase-carries-the-address-it-names ()
  ;; The family table already says what one of a family's sources IS
  ;; (:SOURCE), so the phrase carries the address beside the words and a
  ;; renderer spans the run it just wrote instead of reading a path back out
  ;; of a title it formatted.
  (is-table (phrase snippet) (equal phrase (first (nle::activity-title-phrases (snippet-activity snippet))))
    ('("Read" "src/x.lisp" "1-80" (:path . "src/x.lisp")) "(sh \"sed -n '1,80p' src/x.lisp\")")
    ('("Listed" "src" nil (:path . "src")) "(sh \"ls -la src\")")
    ('("Searched" "'needle'" nil nil) "(sh \"rg -n needle src\")")
    ('("Ran" "just lint" nil nil) "(sh \"just lint\")"))
  ;; A run that counts rather than names promises nothing: a folded card
  ;; cannot deliver a click on a file it is not showing.
  (is (equal '("Read" "3 files" nil nil)
             (first (nle::activity-entries-phrases
                     '((:read "a.lisp" nil) (:read "b.lisp" nil) (:read "c.lisp" nil))))))
  ;; The title cuts what it prints; the target keeps the whole path.
  (let* ((long (format nil "src/~a/deep.lisp" (make-string 80 :initial-element #\x)))
         (phrase (first (nle::activity-entries-phrases (list (list :read long nil))))))
    (is (< (length (second phrase)) (length long)) "the words are cut to fit a title")
    (is (equal (cons :path long) (fourth phrase)) "the address is not"))
  ;; And the joined title still reads exactly as it did.
  (is (equal "Read src/x.lisp (1-80)"
             (nle::activity-title (snippet-activity "(sh \"sed -n '1,80p' src/x.lisp\")"))))
  (is (equal "Read 4 files, Searched 'tool-row'"
             (nle::activity-entries-title
              '((:read "a" nil) (:read "b" nil) (:read "c" nil) (:read "d" nil)
                (:search "tool-row" nil))))))

(deftest snippets-group-only-when-every-operator-is-known ()
  ;; One IS per row: the family, whether it groups, and the title.
  (is-table (form family groupable title)
    (let ((activity (snippet-activity form)))
      (and (eq family activity.family) (eq groupable activity.groupable-p)
           (equal title (nle::activity-title activity))))
    ("(sh \"rg -n foo src\")" :search t "Searched 'foo'")
    ("(let ((out (sh \"cat src/a.lisp\"))) (subseq out 0 200))" :read t "Read src/a.lisp")
    ("(progn (sh \"cat a\") (sh \"cat b\") (sh \"rg -n x\"))" :read t "Read 2 files, Searched 'x'")
    ("(dolist (f (directory \"src/*.lisp\")) (print f))"
     :list t "Listed src/*.lisp")
    ("(with-open-file (in \"notes.txt\") (read-line in))"
     :read t "Read notes.txt")
    ("(eval-status 3)" :poll t "Checked the background job's result")
    ("(sh \"ls\")" :list t "Listed the folder")
    ("(sh \"ls -la ./\")" :list t "Listed the folder")
    ("(edit \"src/a.lisp\" ($ \"old\") ($ \"new\"))" :edit nil "Edited src/a.lisp")
    ("(write-file \"out.md\" ($ \"text\"))" :write nil "Wrote out.md")
    ("(defun tiles (x) (+ x 6))" :define nil "Defined tiles")
    ("(progn (sh \"cat a\") (my-helper))" :eval nil "Read a")
    ("(+ 1 2)" :eval nil "(+ 1 2)")
    ("(setf nle::*system-prompt* \"x\")" :eval nil "(setf nle::*system-prompt* \"x\")")
    ("(let ((x 1)) (incf x) x)" :eval nil "(let ((x 1)) (incf x) x)")
    ("(sh \"cat a\") (sh \"just build\")" :command nil "Read a, Ran just build")))

(deftest a-read-of-a-computed-path-is-still-a-read ()
  ;; The first live DeepSeek snippet of a proof, verbatim: two
  ;; reads through uiop on paths it built, and one grep. A computed path hid
  ;; which file, never that it was read; only a computed command hides what
  ;; ran.
  (let ((activity (snippet-activity (format nil "(let ((base \"/home/me/nodecode/nodecode/\"))~%  (format t \"=== activity.lisp (1-30) ===~~%~~a~~%\" (with-output-to-string (s) (loop for l in (uiop:read-file-lines (concatenate 'string base \"src/src/engine/activity.lisp\")) for i from 1 to 30 do (format s \"~~3d| ~~a~~%\" i l))))~%  (format t \"=== keys.lisp (1-20) ===~~%~~a~~%\" (with-output-to-string (s) (loop for l in (uiop:read-file-lines (concatenate 'string base \"src/src/tui/keys.lisp\")) for i from 1 to 20 do (format s \"~~3d| ~~a~~%\" i l))))~%  (format t \"=== grep ===~~%~~a~~%\" (sh \"grep -rn 'defun group-rows' src/src/tui\")))"))))
    (is (nle::activity-groupable-p activity) "two reads and a search group")
    (is (equal "Read 2 files, Searched 'defun group-rows'" (nle::activity-title activity))
        (nle::activity-title activity)))
  (is (equal "Read src/b.lisp"
             (nle::activity-title (snippet-activity "(uiop:read-file-string (merge-pathnames \"src/b.lisp\" root))"))))
  (is (not (nle::activity-groupable-p (snippet-activity "(sh (format nil \"~a ~a\" tool file))")))))

(deftest a-snippet-note-titles-an-unknown-snippet-and-decides-nothing ()
  (let ((noted (snippet-activity (format nil ";; look at the lanes~%(nck:map-lanes)"))))
    (is (eq :eval noted.family) "an unknown operator stays a snippet")
    (is (not noted.groupable-p) "and never groups, whatever its note says")
    (is (equal "look at the lanes" (nle::activity-title noted)) "the note is its title"))
  (let ((read (snippet-activity (format nil ";; read the config~%(sh \"cat config.json\")"))))
    (is (nle::activity-groupable-p read) "a noted read still groups")
    (is (equal "Read config.json" (nle::activity-title read)))))

(deftest receipts-name-what-a-computed-snippet-did ()
  (let ((form "(dolist (f '(\"a\" \"b\")) (sh (format nil \"cat ~a\" f)))"))
    (let ((static (snippet-activity form)))
      (is-shape static (.groupable-p not) (.family eq :eval "so it stays a snippet")))
    (let ((observed (snippet-activity form :metadata (receipts '("verb" "sh" "source" "cat a")
                                                               '("verb" "sh" "source" "cat b")))))
      (is (nle::activity-groupable-p observed) "the receipts say it only read")
      (is (equal "Read 2 files" (nle::activity-title observed)) "two files")))
  (is (not (nle::activity-groupable-p (snippet-activity "(web:search \"rust async\")"))))
  (let ((activity (snippet-activity "(web:search \"rust async\")"
                                    :metadata (receipts '("verb" "web:search" "family" "search"
                                                          "source" "rust async")))))
    (is (nle::activity-groupable-p activity))
    (is (equal "Searched 'rust async'" (nle::activity-title activity)) "and its source"))
  (is (not (nle::activity-groupable-p
            (snippet-activity "(progn (sh \"cat a\") (x:y))"
                              :metadata (receipts '("verb" "x:y" "family" "teleport"))))))
  (let ((activity (snippet-activity "(sh \"cat src/a.lisp\")"
                                    :metadata (changed-files "src/a.lisp"))))
    (is-shape activity (.family eq :edit) (.groupable-p not "and an edit never groups")))
  (is (not (nle::activity-groupable-p (snippet-activity "(sh \"cat a\")" :error-p t)))))

(defun changed-files (&rest paths)
  "A result fact's metadata whose workspace diff names PATHS."
  (nlk:make-json-object
   "workspace_diff" (nlk:make-json-object
                     "files" (map 'vector (lambda (path) (nlk:make-json-object "path" path)) paths))))

(deftest a-file-the-call-wrote-is-not-also-edited ()
  ;; The workspace diff names every file a call changed, and a file the call's
  ;; own words already write or edit is that entry: without this, a lone
  ;; write-file would be titled `Wrote notes.md, Edited notes.md'.
  (is-each ((lambda (form metadata) (nle::activity-title (snippet-activity form :metadata metadata))))
    ("(write-file \"notes.md\" \"# Notes\")" (changed-files "notes.md") "Wrote notes.md"
     "a write is not an edit too")
    ("(write-file \"./notes.md\" \"x\")" (changed-files "notes.md") "Wrote ./notes.md"
     "spelled behind a ./")
    ("(write-file \"/work/app/notes.md\" \"x\")" (changed-files "notes.md") "Wrote /work/app/notes.md"
     "spelled whole")
    ("(edit \"notes.md\" \"a\" \"b\")" (changed-files "notes.md") "Edited notes.md" "an edit, once")
    ("(sh \"echo x > out.txt\")" (changed-files "out.txt") "Wrote out.txt" "a redirect writes")
    ("(write-file \"notes.md\" \"x\")" (changed-files "notes.md" "other.md")
     "Wrote notes.md, Edited other.md" "a file the call never named is still an edit")))

(deftest shell-tools-take-their-own-words ()
  (let ((shell (nle::tool-call-activity "bash" "rg -n needle")))
    (is-shape shell (nle::activity-title "Searched 'needle'") (nle::activity-groupable-p is)))
  (is (null (nle::tool-call-activity "browser_click" nil))))

(deftest a-streaming-snippet-reads-as-far-as-it-has-come ()
  (is-each ((lambda (form) (nle::activity-bar-verb (snippet-activity form))))
    ("(sh \"sed -n '1,40p' src/tui/ui" "reading src/tui/ui (1-40)" nil)
    ("(let ((x" "evaluating" nil)
    ((format nil ";; check the lanes~%(nck:") "check the lanes" nil)
    ((format nil ";; Patch the guard~%(progn (sleep 4) :done)") "Patch the guard" "the note outranks a wait's own clock"))
  (is-each (nle::tool-activity-verb)
    ("eval" (eval-args "(sh \"cat notes.txt\")") "reading notes.txt"
     "complete arguments read through the reader")
    ("eval" "{\"form\":\";; first line\\n(let" "first line"
     "a streaming prefix decodes its escapes, so the note ends where its line does")
    ("browser_click" "{}" "running browser_click" "a tool with no words keeps its name")
    (nil "running tool" "and a call not yet named is a tool")))

(deftest a-tool-verb-carries-the-kind-of-its-mark ()
  ;; The kind rides beside the verb, so the bar's mark never reads the words.
  (is-each ((lambda (name arguments) (nth-value 1 (nle::tool-activity-verb name arguments))))
    ("eval" (eval-args "(sh \"cat notes.txt\")") :read "a read is a read")
    ("eval" "{\"form\":\";; first line\\n(let" :eval "a snippet with no words of its own evaluates")
    ("browser_click" "{}" :default "a tool with no words keeps the pulse")
    (nil nil :default "and so does a call not yet named")))

(deftest a-tool-call-stamps-its-verbs-on-its-result-fact (with-session-store ("act-rec"))
  ;; The durable seam: a snippet whose read is nested inside a binding still
  ;; lands the call on the result fact, and the fold carries it to the row
  ;; the transcript classifies — live and replay read the same fact.
  (with-durable-turn (turn "act-rec" :command-id "act-cmd" :input "go" :current t)
    (nle::execute-tool-call
     (wire-tool-call "call-nested" "eval"
                     (eval-args "(let ((out (sh \"rg -n defun src/engine/activity.lisp\"))) (length out))"))
     (compiled-context '()))
    (is-present (calls (nlk:json-value (nlk::tool-result-event-metadata
                                        (session-event "act-rec" :type 'nlk:tool-result-event))
                                       :array "calls"))
      "the result fact carries the call receipts"
      (is (equal "sh" (gethash "verb" (aref calls 0))) "the verb")
      (is (equal "rg -n defun src/engine/activity.lisp"
                 (gethash "source" (aref calls 0)))))
    (let* ((row (find :tool-call (nle::transcript-rows
                                  (nle:fold-log :session-id "act-rec"))
                      :key #'nle::transcript-row-kind))
           (activity (nle::tool-call-activity
                      "eval"
                      (nlk:tool-command-string row.tool-args)
                      :metadata row.metadata)))
      (is (nle::activity-groupable-p activity) "the folded row groups as a search")
      (is (equal "Searched 'defun'" (nle::activity-title activity))
          (nle::activity-title activity)))))

(deftest a-receipt-outside-a-tool-call-records-nothing ()
  (let ((nle::*executing-tool-call-id* nil)
        (nle::*tool-result-metadata* nil))
    (is (equal '(:verb "x") (nle:receipt "calls" '(:verb "x"))) "it answers its value")
    (is (null nle::*tool-result-metadata*) "and writes no envelope"))
  (let ((nle::*executing-tool-call-id* "c1")
        (nle::*tool-result-metadata* nil)
        (nle::*receipt-limit* 2))
    (dotimes (i 5) (nle:receipt "calls" (list :verb "sh" :source (format nil "cat ~d" i))))
    (is (= 2 (length (gethash "calls" nle::*tool-result-metadata*))))))

(deftest a-command-cut-at-a-backslash-still-ends-inside-itself ()
  ;; A receipt source is cut to *RECEIPT-SOURCE-LIMIT* blind to what it lands
  ;; on, so a source can end mid-continuation. A trailing backslash escapes
  ;; nothing, and the scanner's offsets are what every reader slices by:
  ;; without this, an END one past the command would throw SUBSEQ out of bounds
  ;; out of SHELL-COMMAND-ENTRIES and kill every frame of the session.
  (let ((cut (concatenate 'string (make-string 999 :initial-element #\a) "\\"))
        (escaped (concatenate 'string (make-string 999 :initial-element #\a) "\\| tail -25"))
        (piped (concatenate 'string (make-string 500 :initial-element #\a) " | python3 gen.py")))
    (flet ((stage-end (command)
             (loop for p in (nle::shell-pipelines command)
                   maximize (loop for st in p maximize (nle::shell-command-end st)))))
      (dolist (command (list cut escaped piped))
        (is (= (length command) (stage-end command)) "every stage ends inside its command"))
      (is (= 1 (length (nle::shell-command-entries cut))) "the cut command names one call")
      (is (= 1 (length (nle::shell-command-entries escaped))) "an escaped pipe is no split")
      (is (= 2 (length (nle::shell-command-entries piped))) "a real pipe still splits"))))

(deftest a-compound-command-is-one-command ()
  ;; for, while and until ... done, if ... fi, case ... esac, { ... } and
  ;; ( ... ) are one command each, their operators inside it. Without this,
  ;; `for f in *; do echo $f; done' would read as three commands. What
  ;; follows a compound splits as ever, a reserved word anywhere but a
  ;; command's name is a word, and a $( ) keeps its operators.
  (is-table (command expected) (equal 'expected (nle::shell-command-entries command))
    ("for f in *; do echo $f; done" ((:command "for f in *; do echo $f; done" nil)))
    ("for i in $(seq 1 3); do echo $i; sleep 1; done; echo ok >&2; exit 2"
     ((:command "for i in $(seq 1 3); do echo $i; sleep 1; done" nil)))
    ("while read l; do cat $l; done < list"
     ((:command "while read l; do cat $l; done < list" nil) (:read "list" nil)))
    ("if [ -f a ]; then cat a; fi; ls" ((:command "if [ -f a ]; then cat a; fi" nil) (:list "." nil)))
    ("case $x in a) echo a;; *) rm b;; esac" ((:command "case $x in a) echo a;; *) rm b;; esac" nil)))
    ("{ echo a; echo b; } > out.txt" ((:command "{ echo a; echo b; } > out.txt" nil) (:write "out.txt" nil)))
    ("(cd src && rg -n foo)" ((:command "(cd src && rg -n foo)" nil)))
    ("for f in *; do if [ -d $f ]; then ls $f; fi; done | sort"
     ((:command "for f in *; do if [ -d $f ]; then ls $f; fi; done | sort" nil)))
    ("echo done; cat a" ((:read "a" nil)))
    ("echo $(date; uptime) && ls" ((:command "echo $(date; uptime)" nil) (:list "." nil))))
  (is (equal "Ran for f in *; do echo $f; done"
             (nle::activity-title (snippet-activity "(sh \"for f in *; do echo $f; done\")")))))

(deftest a-wait-names-its-unit ()
  ;; A wait of any plain number is seconds; `(sleep 0.5)' would be titled
  ;; `Waited 1 pause', its number no source, and a shell's `sleep 0.5'
  ;; would be titled `Waited 0.5'. A unit the command spells stays as spelled.
  (is-each ((lambda (form) (nle::activity-title (snippet-activity form))))
    ("(sleep 0.5)" "Waited 0.5s" "a snippet's decimal is its source")
    ("(sleep 2)" "Waited 2s" "and a whole number")
    ("(sh \"sleep 0.5\")" "Waited 0.5s" "a shell's decimal")
    ("(sh \"sleep .5\")" "Waited .5s" "with no leading digit")
    ("(sh \"sleep 1m\")" "Waited 1m" "a spelled unit")))

(deftest a-pipeline-is-named-whole-and-waits-say-their-time ()
  ;; A command is named by its whole pipeline, as typed; several waits say the
  ;; time they took together. Without this, a pipeline would be titled by its
  ;; first stage, and two sleeps would be titled `Waited 2 pauses'.
  (is-table (command expected) (equal 'expected (nle::shell-command-entries command))
    ("seq 1 300 | awk '{print $1}'" ((:command "seq 1 300 | awk '{print $1}'" nil)))
    ("cd x && make 2>&1 | tail -5" ((:command "make 2>&1 | tail -5" nil)))
    ("cat a | python3 x.py" ((:read "a" nil) (:command "cat a | python3 x.py" nil)))
    ("sleep 1; sleep 1" ((:wait "1" nil) (:wait "1" nil))))
  (is-each ((lambda (form) (nle::activity-title (snippet-activity form))))
    ("(sh \"seq 1 300 | awk '{print $1}'\")" "Ran seq 1 300 | awk '{print $1}'"
     "a pipeline by its whole line")
    ("(sh \"curl -s x | python3 -m json.tool\")" "Ran curl -s x | python3 -m json.tool"
     "two commands piped are one")
    ("(sh \"sleep 1; sleep 0.5\")" "Waited 1.5s" "the time two waits took")
    ("(sh \"sleep 1; sleep 1\")" "Waited 2s" "two pauses of a second are two seconds")
    ("(sh \"sleep 30; sleep 45\")" "Waited 1m" "past a minute, the organism's own reading")
    ("(sh \"sleep $N; sleep 2\")" "Waited twice" "a time no snippet spelled counts instead"))
  ;; A group of two such calls, as a shell runs their entries together; one
  ;; file read twice is still one file.
  (is (equal "Waited 2s"
             (nle::activity-entries-title
              (nle::distinct-entries (append (nle::activity-entries (snippet-activity "(sh \"sleep 1\")"))
                                             (nle::activity-entries (snippet-activity "(sh \"sleep 1\")")))))))
  (is (equal "Read a" (nle::activity-entries-title
                       (nle::distinct-entries '((:read "a" nil) (:read "a" nil)))))))
