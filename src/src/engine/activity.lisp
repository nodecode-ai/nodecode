;;;; activity.lisp --- what a tool call did, said in words. Evolved package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model's whole surface is two tools, and nearly every call is an
;;;; `eval' snippet of free Lisp. A transcript that prints the snippet makes the
;;;; operator read Lisp to learn that the organism read a file; this module
;;;; turns a call into the words a person would use — `Read src/tui/ui.lisp',
;;;; `Searched 'tool-row'', `Ran just lint' — and says whether the call is
;;;; routine enough to fold into a group with its neighbours.
;;;;
;;;; Two sources, observed before inferred:
;;;;
;;;;   receipts   the verbs a snippet calls stamp what they were asked to do on
;;;;              the call's result fact, under the metadata's "calls" key
;;;;              (RECEIPT, turn.lisp): {"verb": "sh", "source": "rg -n x"}.
;;;;              Nesting and computed arguments do not matter — the stamp
;;;;              happens inside the verb.
;;;;   the snippet   read into a light tree (never interned, never evaluated)
;;;;              and walked for its operators, allowlist only: a head in
;;;;              *ACTIVITY-HEADS* is a call, a pure Common Lisp operator is
;;;;              transparent, and any other operator makes the snippet unknown
;;;;              — it may have done anything, so it never groups. This is
;;;;              what a streaming snippet, a backgrounded snippet and an old log
;;;;              have.
;;;;
;;;; The `;;' note a snippet opens with titles an unknown snippet, and never decides
;;;; a family or whether a snippet groups: a model's claim about its own call is
;;;; not evidence of what the call did. Everything here is classification at
;;;; consumption — the fact stays what it was, and replay classifies exactly
;;;; as the live stream does.

(in-package #:nodecode.evolved)

;;; --- families ----------------------------------------------------------------

;;; An address rides the title as a target a shell can open
;;; (ACTIVITY-SOURCE-TARGET), so a renderer never has to read a path back out
;;; of words it formatted. The order is the severity order a card's family is
;;; picked by, most severe last: a snippet that read a file and ran a command is
;;; a command.
(defparameter *activity-families*
  '((:read    :past "Read"      :present "Reading"    :one "file"       :many "files"       :group t :source :path)
    (:search  :past "Searched"  :present "Searching"  :one "pattern"    :many "patterns"    :group t :source :pattern)
    (:list    :past "Listed"    :present "Listing"    :one "directory"  :many "directories" :group t :source :path)
    (:fetch   :past "Fetched"   :present "Fetching"   :one "page"       :many "pages"       :group t :source :url)
    (:recall  :past "Recalled"  :present "Recalling"  :one "entry"      :many "entries"     :group t :source :pattern)
    (:poll    :past "Checked"   :present "Checking"   :one "background job" :many "background jobs" :group t)
    (:wait    :past "Waited"    :present "Waiting"                                          :group t)
    (:edit    :past "Edited"    :present "Editing"    :one "file"       :many "files"       :source :path)
    (:write   :past "Wrote"     :present "Writing"    :one "file"       :many "files"       :source :path)
    (:image   :past "Viewed"    :present "Viewing"    :one "image"      :many "images"      :source :path)
    (:define  :past "Defined"   :present "Defining"   :one "definition" :many "definitions")
    (:command :past "Ran"       :present "Running"    :one "command"    :many "commands")
    (:eval    :past "Evaluated" :present "Evaluating" :one "snippet"    :many "snippets"))
  "Every family a call can land in, as (FAMILY . PLIST): the verb in both
tenses, the noun a count takes (a wait has none: several say the time they
took, WAITS-TEXT), whether the family is routine enough to group
(:GROUP — reads, searches, listings, fetches, recalls, checks and waits;
anything that changes a file, the image or the world stays a card of its
own), and what one of its sources IS (:SOURCE — a :PATH, a :URL, or a
:PATTERN, the kind that prints quoted; a family with no key names something
that is not an address at all).")

(defun activity-family-plist (family)
  (cdr (assoc family *activity-families*)))

;;; A head whose family is :DEFINE takes its arguments as data, as a
;;; definition does.
;;;
;;; Data, not a registry, and the core's own: a cell names its verbs from
;;; inside them instead, stamping each call with RECEIPT and the family it is
;;; ({:verb "web:search" :family "search" :source ...}) — a folder that
;;; changed this value at load would read to the loader as an override.
(defparameter *activity-heads*
  ;; written (FAMILY SOURCE NAME...), one row per family and source
  (loop for (family source . names)
          in '((:shell 0 "sh")
               (:edit 0 "edit")
               (:write 0 "write-file")
               (:define 0 "patch")
               (:image 0 "look")
               (:poll 0 "eval-status" "eval-await")
               (:poll nil "list-evals")
               (:wait 0 "sleep")
               (:recall :match "recall-log")
               (:recall 0 "recall-entry")
               (:read 0 "uiop:read-file-string" "uiop:read-file-lines" "uiop:read-file-line"
                "uiop:read-file-form" "uiop:read-file-forms")
               (:list 0 "directory" "uiop:directory-files" "uiop:subdirectories" "probe-file"
                "uiop:file-exists-p" "uiop:directory-exists-p"))
        append (loop for name in names collect (list name family source)))
  "The operators a snippet calls that say what the snippet does, as (NAME FAMILY
SOURCE): NAME as a snippet spells it, lower case, with a package prefix unless
the operator is Common Lisp's or this package's own; FAMILY one of
*ACTIVITY-FAMILIES*, or :SHELL for a command string classified by its first
word; SOURCE the argument naming what the call is about — an index into the
arguments, a keyword whose value it is, or NIL.")

;;; --- what a transcript names of a shell command -----------------------------
;;; The grammar — SHELL-PIPELINES, SHELL-STRIP-PREFIX, the SHELL-COMMAND
;;; struct — is shell.lisp's; what follows is the transcript's reading of
;;; it: which programs read, search, list or wait, and what a card names of
;;; each.

;;; Every program not named here, or in *SHELL-TRANSPARENT-PROGRAMS*, is a
;;; command — git, just, python3 and the rest stay a card of their own.
(defparameter *shell-programs*
  '((:read "cat" "head" "tail" "sed" "nl" "less" "more" "bat" "wc" "file" "stat"
     "xxd" "od" "jq" "diff" "cmp" "md5sum" "sha256sum")
    (:search "rg" "grep" "egrep" "fgrep" "ag" "ack" "find" "fd" "locate")
    (:list "ls" "tree" "eza" "exa" "du")
    (:wait "sleep"))
  "The programs whose runs group: what they read, search, list, or wait on.")

(defparameter *shell-transparent-programs*
  '("cd" "echo" "printf" "true" "false" ":" "test" "[" "export" "set" "unset"
    "which" "type" "command" "pwd" "exit" "local" "shift" "read" "date"
    "basename" "dirname" "realpath" "readlink")
  "Programs that neither read nor change anything a transcript should name:
a directory change, an echo, a test.")

(defparameter *shell-filters*
  '("sort" "uniq" "cut" "tr" "awk" "column" "fold" "fmt" "rev" "tac" "paste"
    "comm" "xargs" "cat" "head" "tail" "sed" "nl" "wc" "grep" "egrep" "fgrep"
    "rg" "jq" "less" "more")
  "Programs that, after a pipe, only shape what the pipeline already read.")

(defparameter *shell-flag-values*
  '(("rg" "-e" "--regexp" "-f" "--file" "-g" "--glob" "--iglob" "-t" "--type"
     "-T" "--type-not" "-A" "-B" "-C" "--after-context" "--before-context"
     "--context" "-m" "--max-count" "-j" "--threads" "-M" "--max-columns"
     "--color" "--colors" "--sort" "--sortr" "-r" "--replace" "-d" "--max-depth"
     "--max-filesize" "--pre" "--pre-glob" "--type-add" "-E" "--encoding")
    ("grep" "-e" "--regexp" "-f" "--file" "-A" "-B" "-C" "-m" "--max-count"
     "-d" "-D" "--label" "--color")
    ("egrep" "-e" "-f" "-A" "-B" "-C" "-m") ("fgrep" "-e" "-f" "-A" "-B" "-C" "-m")
    ("ag" "-G" "-A" "-B" "-C" "-m") ("ack" "-A" "-B" "-C" "-m")
    ("head" "-n" "-c" "--lines" "--bytes") ("tail" "-n" "-c" "--lines" "--bytes")
    ("sed" "-e" "--expression" "-f" "--file" "-l")
    ("fd" "-e" "--extension" "-t" "--type" "-d" "--max-depth" "-E" "--exclude"
     "-c" "--color" "-j" "--threads" "-S" "--size")
    ("nl" "-b" "-s" "-w" "-v" "-i") ("wc" "--files0-from")
    ("tree" "-L" "-I" "-P") ("eza" "-L" "--level" "-I" "--ignore-glob")
    ("exa" "-L" "--level" "-I") ("ls" "-I" "--ignore" "-w" "--width")
    ("du" "-d" "--max-depth") ("xxd" "-l" "-s" "-c") ("od" "-N" "-j" "-t")
    ("jq" "--arg" "--argjson" "--slurpfile" "--rawfile" "-L"))
  "The flags, per program, whose value is the next word — so the value is
never mistaken for a path or a pattern.")

(defun shell-positional-args (program args &aux (flags-done nil))
  "ARGS without their flags and the values those flags take."
  ;; A bare -- ends the flags.
  (loop with valued = (cdr (assoc program *shell-flag-values* :test #'string=))
        for word = (pop args)
        while word
        if flags-done collect word
        else if (string= word "--") do (setf flags-done t)
        else if (shell-flag-p word)
               do (when (and (member word valued :test #'string=) (not (find #\= word)))
                    (pop args))
        else collect word))

(defun shell-flag-value (args &rest flags)
  "The value after the first of FLAGS in ARGS, as the next word or after an =."
  (loop for (word . rest) on args
        do (dolist (flag flags)
             (cond ((string= word flag) (return-from shell-flag-value (first rest)))
                   ((and (> (length word) (1+ (length flag)))
                         (uiop:string-prefix-p (format nil "~a=" flag) word))
                    (return-from shell-flag-value (subseq word (1+ (length flag)))))))))

(defun shell-program-entries (program args text)
  "The entries one simple command contributes: PROGRAM with ARGS, TEXT its
source span. See SHELL-COMMAND-ENTRIES."
  (let ((family (car (find-if (lambda (row) (member program (cdr row) :test #'string=))
                              *shell-programs*)))
        (positional (shell-positional-args program args)))
    (flet ((each (family paths &optional detail)
             (mapcar (lambda (path) (list family path detail)) paths))
           (digits-p (word) (and (plusp (length word)) (every #'digit-char-p word))))
      (cond
        ((member program *shell-transparent-programs* :test #'string=) '())
        ((member program '("sh" "bash" "zsh" "dash") :test #'string=)
         (if-let (script (loop for (word . rest) on args
                               when (and (shell-flag-p word) (find #\c word))
                                 return (first rest)))
           (shell-command-entries script) (each :command (list text))))
        ((and (string= program "git") (equal (first positional) "grep"))
         (each :search (list (first (shell-positional-args
                                     "grep" (rest (member "grep" args :test #'string=)))))))
        ((null family) (each :command (list text)))
        ((string= program "sed")
         (let* ((in-place (loop for word in args
                                thereis (or (uiop:string-prefix-p "--in-place" word)
                                            (and (shell-flag-p word)
                                                 (not (uiop:string-prefix-p "--" word))
                                                 (find #\i word)))))
                (script (shell-flag-value args "-e" "--expression"))
                (files (if (or script (shell-flag-value args "-f" "--file"))
                           positional
                           (rest positional)))
                (script (string-trim '(#\Space #\Tab) (or script (first positional) ""))))
           (if in-place
               (each :edit files)
               ;; the line range a print script reads: `12,80p' is "12-80", `5p' is "5"
               (each :read files (and (= 1 (length files))
                                      (ppcre:register-groups-bind (from to)
                                          ("^([0-9]+)(?:,([0-9]+))?p\\z" script)
                                        (format nil "~a~@[-~a~]" from to)))))))
        ((member program '("head" "tail") :test #'string=)
         (let ((count (string-left-trim
                       "+-" (or (shell-flag-value args "-n" "--lines")
                                (loop for word in args
                                      thereis (and (shell-flag-p word) (digits-p (subseq word 1))
                                                   (subseq word 1)))
                                ""))))
           (each :read positional
                 (and (digits-p count)
                      (format nil (if (string= program "head") "1-~a" "last ~a") count)))))
        ((string= program "jq") (each :read (rest positional)))
        ((eq family :read) (each :read positional))
        ((string= program "find")
         (if (intersection args '("-delete" "-exec" "-execdir" "-ok" "-fprint") :test #'string=)
             (each :command (list text))
             (each :search (list (or (shell-flag-value args "-name" "-iname" "-path" "-ipath"
                                                       "-regex" "-iregex")
                                     (and args (not (shell-flag-p (first args))) (first args))
                                     ".")))))
        ((string= program "fd")
         (if (intersection args '("-x" "--exec" "-X" "--exec-batch") :test #'string=)
             (each :command (list text))
             (each :search (list (first positional)))))
        ((eq family :search)
         (each :search (list (or (shell-flag-value args "-e" "--regexp") (first positional)))))
        ((eq family :list) (each :list (or positional (list "."))))
        ((eq family :wait) (each :wait (list (first positional))))))))

(defun shell-command-entries (command)
  "The entries COMMAND — a shell command string — contributes, in order, as
(FAMILY SOURCE DETAIL): what each simple command reads, searches, lists or
waits on, a command for anything else, a write for every redirect into a
file."
  ;; A pipe's later stages that only shape the output (sort, head, grep) add
  ;; nothing; a command substitution makes its command a command whatever the
  ;; program, since it ran something the split cannot see. A command is named
  ;; by its whole pipeline as it was typed, the way Claude Code and Codex
  ;; print the command line: not by the first stage alone.
  (loop with command = (or command "")
        for pipeline in (shell-pipelines command)
        for text = (string-trim '(#\Space #\Tab #\Newline)
                                (subseq command (shell-command-start (first pipeline))
                                        (shell-command-end (car (last pipeline)))))
        nconc (loop for stage in pipeline
                    for filters = nil then *shell-filters*
                    for words = (shell-strip-prefix stage.words)
                    for program = (and words (shell-program-name (first words)))
                    nconc (cond ((shell-command-substitution stage) `((:command ,text nil)))
                                ((or (null program) (member program filters :test #'string=)) '())
                                (t (shell-program-entries program (rest words) text)))
                    nconc (loop for (op . target) in (shell-command-redirects stage)
                                if (and (string= op "<") (not filters))
                                  collect `(:read ,target nil)
                                else if (and (member op '(">" ">>" ">|" "&>" "<>") :test #'string=)
                                             (string/= target "/dev/null"))
                                       collect `(:write ,target nil)))))

;;; --- the snippet ------------------------------------------------------------
;;; A snippet read into a light tree: lists, strings, and tokens as the text
;;; spells them. Nothing is interned and nothing evaluates, so a package this
;;; image lacks, a #. and a half-streamed snippet are all just text.

(defun snippet-operator-name (token)
  "TOKEN as an operator name the tables spell: lower case, bars dropped, a
Common Lisp or evolved-package prefix removed, ppcre spelled cl-ppcre."
  ;; NIL for a keyword or a token that is no symbol.
  (when (and (stringp token) (plusp (length token)))
    (let ((name (string-downcase (remove #\| token))))
      (cond ((or (member (char name 0) '(#\: #\#)) (digit-char-p (char name 0))) nil)
            ((not (find #\: name)) name)
            (t (ppcre:register-groups-bind (package symbol) ("(?s)\\A([^:]*):+(.*)\\z" name)
                 (cond ((member package '("cl" "common-lisp" "cl-user" "common-lisp-user"
                                          "nle" "nodecode.evolved")
                                :test #'string=)
                        symbol)
                       ((string= package "ppcre") (format nil "cl-ppcre:~a" symbol))
                       (t (format nil "~a:~a" package symbol)))))))))

;;; The assignments and IN-PACKAGE and DECLARE are not here: SNIPPET-CALLS walks
;;; them before this table is read, an assignment transparent only on a place
;;; that names no special variable and no other package's symbol — anything
;;; else changed the running image.
(defparameter *snippet-transparent-operators*
  '("progn" "prog1" "prog2" "when" "unless" "if" "and" "or" "not" "values"
    "multiple-value-list" "nth-value" "list" "list*" "cons" "car" "cdr" "caar"
    "cadr" "cdar" "cddr" "caddr" "cdddr" "first" "second" "third" "fourth"
    "fifth" "sixth" "seventh" "eighth" "ninth" "tenth" "rest" "last" "butlast"
    "nth" "nthcdr" "elt" "aref" "svref" "char" "schar" "subseq" "copy-seq"
    "copy-list" "copy-tree" "reverse" "nreverse" "append" "nconc" "length"
    "list-length" "gethash" "getf" "assoc" "assoc-if" "rassoc" "member"
    "member-if" "find" "find-if" "find-if-not" "position" "position-if" "count"
    "count-if" "remove" "remove-if" "remove-if-not" "delete" "delete-if"
    "remove-duplicates" "delete-duplicates" "substitute" "substitute-if"
    "mapcar" "mapc" "mapcan" "maplist" "mapl" "map" "reduce" "sort"
    "stable-sort" "every" "some" "notany" "notevery" "search" "mismatch"
    "concatenate" "intersection" "union" "set-difference" "adjoin" "subst"
    "string" "string=" "string/=" "string<" "string>" "string-equal"
    "string-lessp" "string-upcase" "string-downcase" "string-capitalize"
    "string-trim" "string-left-trim" "string-right-trim" "parse-integer"
    "char-code" "code-char" "char=" "char-equal" "char-upcase" "char-downcase"
    "digit-char-p" "alpha-char-p" "alphanumericp" "upper-case-p" "lower-case-p"
    "format" "print" "prin1" "princ" "pprint" "terpri" "fresh-line" "write"
    "write-string" "write-line" "write-char" "princ-to-string"
    "prin1-to-string" "write-to-string" "finish-output" "force-output"
    "equal" "eq" "eql" "equalp" "=" "/=" "<" ">" "<=" ">=" "+" "-" "*" "/"
    "1+" "1-" "max" "min" "abs" "floor" "ceiling" "round" "truncate" "mod"
    "rem" "expt" "sqrt" "log" "exp" "float" "zerop" "plusp" "minusp" "evenp"
    "oddp" "random" "null" "atom" "consp" "listp" "stringp" "symbolp"
    "keywordp" "numberp" "integerp" "floatp" "characterp" "vectorp" "arrayp"
    "hash-table-p" "functionp" "typep" "type-of" "coerce" "identity"
    "constantly" "complement" "make-hash-table" "hash-table-count" "maphash"
    "make-array" "make-string" "make-list" "vector" "array-dimension"
    "array-dimensions" "fill-pointer" "funcall" "apply" "return" "error"
    "warn" "assert" "check-type" "ignore-errors" "unwind-protect" "time"
    "get-universal-time" "get-internal-real-time" "decode-universal-time"
    "symbol-name" "find-package" "find-symbol" "boundp" "fboundp" "describe"
    "documentation" "apropos" "apropos-list" "macroexpand" "macroexpand-1"
    "make-string-output-stream" "get-output-stream-string" "read-line"
    "read-char" "namestring" "pathname" "pathname-name" "pathname-type"
    "merge-pathnames" "enough-namestring" "file-namestring" "make-pathname"
    "uiop:split-string" "uiop:string-prefix-p" "uiop:string-suffix-p"
    "uiop:strcat" "uiop:getenv" "uiop:native-namestring" "uiop:unix-namestring"
    "uiop:merge-pathnames*" "uiop:ensure-directory-pathname" "uiop:subpathp"
    "cl-ppcre:scan" "cl-ppcre:scan-to-strings" "cl-ppcre:all-matches"
    "cl-ppcre:all-matches-as-strings" "cl-ppcre:regex-replace"
    "cl-ppcre:regex-replace-all" "cl-ppcre:split" "$" "help")
  "Common Lisp and snippet operators that only compute: a snippet made of these and
*ACTIVITY-HEADS* did nothing but what its heads say.")

(defparameter *snippet-definition-operators*
  '("defun" "defmacro" "defvar" "defparameter" "defconstant" "defgeneric"
    "defmethod" "defstruct" "defclass" "deftype" "define-condition"
    "define-modify-macro" "define-hook")
  "Operators that define: the snippet changed the running image by name.")

(defun snippet-calls (text)
  "The calls TEXT — a snippet — makes, read statically."
  ;; => (values CALLS UNKNOWN), CALLS as (VERB . SOURCE) in order with SOURCE the
  ;; literal text the head's source argument spells, (:DYNAMIC . GUESS) when the
  ;; argument is computed (GUESS: its last string literal), or NIL when the head names
  ;; none; UNKNOWN the operators neither *ACTIVITY-HEADS* nor
  ;; *SNIPPET-TRANSPARENT-OPERATORS* names, each once.
  (let* ((calls '())
         (unknown '())
         (locals '())
         (text (or text ""))
         (pos 0)
         (len (length text)))
    (labels ((ch (i) (and (< i len) (char text i)))
             (terminator-p (c) (or (member c nlk::+scribe-whitespace+) (find c "()\"'`,;")))
             ;; past whitespace, ; comments and nested #| |# comments, as the scribe skips them
             (skip () (setf pos (nlk::scribe-skip-blank text pos len)))
             (token (&optional (from pos) &aux (start (setf pos from)))
               (loop for c = (ch pos) while c
                     do (cond ((char= c #\\) (incf pos 2))
                              ((char= c #\|)
                               (setf pos (if-let (close (position #\| text :start (1+ pos)))
                                           (1+ close) len)))
                              ((terminator-p c) (return))
                              (t (incf pos))))
               (setf pos (min pos len))
               (subseq text start pos))
             (next ()
               (skip)
               (unless (or (>= pos len) (char= (char text pos) #\)))
                 (node)))
             (node ()
               (case (char text pos)
                 (#\(
                  (incf pos)
                  (cons :list (loop do (skip)
                                    until (>= pos len)
                                    until (and (char= (char text pos) #\)) (incf pos))
                                    collect (node))))
                 (#\"
                  (incf pos)
                  (let ((out (make-string-output-stream)))
                    (loop for d = (ch pos) while d
                          do (cond ((char= d #\\)
                                    (when (ch (1+ pos)) (write-char (ch (1+ pos)) out))
                                    (incf pos 2))
                                   ((char= d #\") (incf pos) (return))
                                   (t (write-char d out) (incf pos))))
                    (setf pos (min pos len))
                    (list :string (get-output-stream-string out))))
                 (#\' (incf pos) (list :quote (next)))
                 (#\` (incf pos) (list :quasi (next)))
                 (#\, (incf pos)
                  (when (member (ch pos) '(#\@ #\.)) (incf pos))
                  (list :unquote (next)))
                 (#\#
                  (case (ch (1+ pos))
                    (#\' (incf pos 2) (list :function (next)))
                    (#\( (incf pos) (list :quote (node)))
                    (#\\ (let ((start pos))
                           (setf pos (min len (+ pos 3)))
                           (token pos)
                           (subseq text start pos)))
                    (#\. (incf pos 2) (list :read-eval (next)))
                    ((#\+ #\-) (incf pos 2) (next) (or (next) ""))
                    ((#\p #\P) (incf pos 2) (or (next) ""))
                    (t (token))))
                 (t (token))))
             (guess (node)
               ;; a computed argument's likeliest subject: its last string literal no format control
               (when (consp node)
                 (if (eq :string (first node))
                     (let ((text (second node)))
                       (and (plusp (length text)) (not (find #\~ text)) text))
                     (some #'guess (reverse (rest node))))))
             (call (verb source) (push (cons verb source) calls))
             (unknown (name) (pushnew name unknown :test #'string=))
             (forms (nodes) (mapc #'walk nodes))
             (list-args (node) (and (consp node) (eq :list (first node)) (rest node)))
             (keyword-tail (key nodes)
               ;; NODES from the token spelling KEY on, case-insensitively.
               (member key nodes :test (lambda (key node)
                                         (and (stringp node) (string-equal key node)))))
             (special-place-p (node)
               ;; A place that names a special or a foreign symbol.
               (cond ((stringp node)
                      (let ((name (snippet-operator-name node)))
                        (and name (or (ppcre:scan "(?s)\\A\\*.*\\*\\z" name) (find #\: name)))))
                     ((and (consp node) (eq :list (first node)))
                      (or (member (snippet-operator-name (second node))
                                  '("symbol-function" "fdefinition" "symbol-value"
                                    "macro-function" "find-class")
                                  :test #'equal)
                          (some #'special-place-p (rest node))))))
             (source-of (node)
               ;; the literal text NODE spells — a string, a keyword or number token —
               ;; else a guess at what the computed argument names
               (and node (or (cond ((and (consp node) (eq :string (first node))) (second node))
                                   ((and (stringp node) (plusp (length node))
                                         (or (char= #\: (char node 0)) (number-text-p node)))
                                    node))
                             (cons :dynamic (guess node)))))
             (walk (node)
               (when (consp node)
                 (case (first node)
                   ;; a ((lambda ...) args) head walks like any other form in the list
                   (:list (if (stringp (second node))
                              (operator (snippet-operator-name (second node)) (cddr node) nil)
                              (forms (rest node))))
                   (:function (if (stringp (second node))
                                  (operator (snippet-operator-name (second node)) '() t)
                                  (walk (second node))))
                   (:quasi (walk-quasi (second node)))
                   (:unquote (walk (second node)))
                   (:read-eval (unknown "#.")))))
             (walk-quasi (node)
               (when (consp node)
                 (case (first node)
                   (:unquote (walk (second node)))
                   (:list (mapc #'walk-quasi (rest node))))))
             (operator (name args reference-p)
               (let ((row (and name (assoc name *activity-heads* :test #'string=))))
                 (macrolet ((named (&rest names) `(member name ',names :test #'string=)))
                   (cond
                     ((or (null name) (equal name "loop") (member name locals :test #'string=))
                      (forms args))
                     (row
                      (destructuring-bind (family spec) (rest row)
                        ;; #'sh handed to a mapper runs on arguments this walk
                        ;; never sees: its source is computed.
                        (call name (cond (reference-p (and spec (list :dynamic)))
                                         ((integerp spec) (source-of (nth spec args)))
                                         ((keywordp spec)
                                          (source-of (second (keyword-tail
                                                              (format nil ":~(~a~)" spec) args))))))
                        (unless (eq family :define)
                          (forms args))))
                     ((member name *snippet-definition-operators* :test #'string=)
                      (call name (and (stringp (first args)) (first args))))
                     ((equal name "with-open-file")
                      (let* ((spec (list-args (first args)))
                             (direction (keyword-tail ":direction" spec)))
                        (call (if (member (second direction) '(":output" ":io") :test #'equalp)
                                  "with-open-file/output"
                                  "with-open-file")
                              (source-of (second spec)))
                        (mapc #'walk (cddr spec))
                        (walk (second spec))
                        (forms (rest args))))
                     ((named "let" "let*" "symbol-macrolet" "handler-bind")
                      (dolist (binding (list-args (first args)))
                        (forms (rest (list-args binding))))
                      (forms (rest args)))
                     ((named "flet" "labels" "macrolet")
                      (let ((definitions (mapcar #'list-args (list-args (first args)))))
                        (dolist (definition definitions)
                          (push (snippet-operator-name (first definition)) locals))
                        (dolist (definition definitions)
                          (forms (cddr definition)))
                        (forms (rest args))))
                     ((named "lambda" "multiple-value-bind" "destructuring-bind"
                             "block" "return-from" "catch" "the")
                      (forms (rest args)))
                     ((named "dolist" "dotimes" "with-output-to-string" "with-input-from-string")
                      (forms (rest (list-args (first args))))
                      (forms (rest args)))
                     ((named "do" "do*")
                      (dolist (spec (list-args (first args)))
                        (forms (rest (list-args spec))))
                      (forms (list-args (second args)))
                      (forms (cddr args)))
                     ((equal name "cond")
                      (dolist (clause args) (forms (list-args clause))))
                     ((named "case" "ecase" "ccase" "typecase" "etypecase" "handler-case")
                      (walk (first args))
                      (dolist (clause (rest args))
                        ;; a handler-case clause's lambda list is not a form
                        (forms (funcall (if (equal name "handler-case") #'cddr #'rest)
                                        (list-args clause)))))
                     ((named "quote" "declare" "in-package"))
                     ((named "setf" "setq" "psetf" "incf" "decf" "push" "pop" "pushnew" "remhash")
                      (when (some #'special-place-p
                                  (cond ((named "setf" "setq" "psetf")
                                         (loop for (place) on args by #'cddr collect place))
                                        ((named "push" "pushnew" "remhash")
                                         (list (second args)))
                                        (t (list (first args)))))
                        (unknown name))
                      (forms args))
                     (t (unless (member name *snippet-transparent-operators* :test #'string=)
                          (unknown name))
                        (forms args)))))))
      (forms (loop do (skip)
                   until (>= pos len)
                   if (char= (char text pos) #\)) do (incf pos)
                     else collect (node)))
      (values (nreverse calls) (nreverse unknown)))))

;;; --- a call's entries ------------------------------------------------------

(defun receipt-calls (metadata)
  "The calls the verbs a tool call ran stamped on its result METADATA, as
(VERB . SOURCE), in the order they ran."
  (loop for stamp across (json-array metadata "calls")
        for verb = (json-value stamp :text "verb")
        for source = (json-value stamp :any "source")
        when verb
          collect (cons verb (cond ((stringp source) source)
                                   ((numberp source) (princ-to-string source))))))

;;; --- the activity ------------------------------------------------------------

(define-record (activity (:export entries groupable-p))
  "What one tool call did."
  ;; FAMILY is the card's family — the most severe of
  ;; its entries', :EVAL for a snippet that ran something no table names. ENTRIES are
  ;; (FAMILY SOURCE DETAIL), distinct, in order. NOTE is the snippet's opening
  ;; comment, HEAD its first line of code. GROUPABLE-P is true when every entry is
  ;; routine and nothing unknown or failed rode along.
  (family :eval)
  (entries '())
  (note nil)
  (head nil)
  (groupable-p nil))

(defun snippet-head-line (text)
  "TEXT's first line that is code — not blank, not a comment."
  (loop for raw in (nlk:lines text)
        for line = (string-trim '(#\Space #\Tab #\Return) raw)
        when (and (plusp (length line)) (char/= (char line 0) #\;))
          return line))

(defun distinct-entries (entries)
  "ENTRIES in order without one an earlier entry already names -- its
family and source -- but for waits: two reads of one file are one file
read, two pauses of a second are two seconds waited."
  ;; One call's entries, and a group's calls' entries run together.
  (remove-duplicates entries
                     :test (lambda (a b)
                             (and (not (eq :wait (first a)))
                                  (equal (subseq a 0 2) (subseq b 0 2))))
                     :from-end t))

(defun activity-from-entries (entries &key unknown-p note head error-p)
  "The ACTIVITY ENTRIES make: distinct entries, the family by severity, and
whether it groups."
  (let* ((entries (distinct-entries entries))
         (family (or (loop for (family) in (reverse *activity-families*)
                           when (and (not (getf (activity-family-plist family) :group))
                                     (not (eq family :eval))
                                     (assoc family entries))
                             return family)
                     (and unknown-p :eval)
                     (first (first entries))
                     :eval)))
    (make-activity :family family
                   :entries entries
                   :note note
                   :head head
                   :groupable-p (and entries
                                     (not unknown-p)
                                     (not error-p)
                                     (loop for (family) in entries
                                           always (getf (activity-family-plist family) :group))))))

(defun activity-same-file-p (source path)
  "Whether SOURCE, a file as a call spelled it, is PATH, a file the workspace
diff names from the session's directory: the same, behind a ./, or whole."
  (and (stringp source)
       (let ((source (if (uiop:string-prefix-p "./" source) (subseq source 2) source)))
         (or (string= source path)
             (uiop:string-suffix-p source (concatenate 'string "/" path))))))

(defun tool-call-activity (name command &key metadata error-p)
  "The ACTIVITY of one tool call NAME."
  ;; COMMAND is the call's executable text — the eval snippet, or a shell tool's
  ;; command — complete or still streaming. METADATA is the result's display
  ;; envelope once it
  ;; exists: its receipts and its workspace diff outrank what the snippet's text
  ;; suggests, and a changed file makes the call an edit whatever it looked
  ;; like. NIL for a tool this module has no words for, and for a call whose
  ;; arguments have not yet said what it runs — no snippet, no command —
  ;; which keeps its raw shape until they do.
  (flet ((with-diff (entries)
           ;; A changed file the call's own words already write or edit is
           ;; that entry, not a second one: without this, the same file would
           ;; be listed again as an edit.
           (append entries
                   (loop for file across (json-array metadata "workspace_diff" "files")
                         for path = (json-value file :text "path")
                         when (and path (loop for (family source) in entries
                                              never (and (member family '(:write :edit))
                                                         (activity-same-file-p source path))))
                           collect (list :edit path nil)))))
    (cond
      ((null command) nil)
      ((equal name "eval")
       (multiple-value-bind (static unknown) (snippet-calls command)
         (bind ((receipts (receipt-calls metadata))
                ;; (VERB . FAMILY) a cell's verb named for itself; an unknown family drops
                (families (loop for stamp across (json-array metadata "calls")
                                for verb = (json-value stamp :text "verb")
                                for word = (json-value stamp :text "family")
                                for family = (car (assoc (find-symbol (string-upcase word) :keyword)
                                                         *activity-families*))
                                when (and verb family)
                                  collect (cons verb family)))
                ;; A receipt vouches for its verb only when the verb is
                ;; named somewhere: a row, or a family it gave itself.
                (verbs (remove-if-not (lambda (verb)
                                        (or (assoc verb *activity-heads* :test #'string=)
                                            (assoc verb families :test #'string=)))
                                      (mapcar #'car receipts)))
                ;; A computed argument hides only a command: a head that
                ;; reads, lists or waits does that whatever it is handed, so
                ;; its call stands on its guessed source. A computed shell
                ;; command could be anything until a receipt says what ran.
                ((dynamic kept)
                 (loop for (verb . source) in static
                       for guessed = (and (consp source) (eq :dynamic (car source)))
                       if (and guessed
                               (eq :shell (second (assoc verb *activity-heads* :test #'string=))))
                         collect verb into dynamic
                       else collect (cons verb (if guessed (cdr source) source)) into kept
                       finally (return (values dynamic kept))))
                (calls (nlk:distinct (append receipts kept))))
           (activity-from-entries
            (with-diff (loop for (verb . text) in calls
                             for source = (and (stringp text) text)
                             for kind = (cond ((member verb *snippet-definition-operators* :test #'string=)
                                               :define)
                                              ((equal verb "with-open-file") :read)
                                              ((equal verb "with-open-file/output") :write)
                                              ((second (assoc verb *activity-heads* :test #'string=)))
                                              (t (cdr (assoc verb families :test #'string=))))
                             append (cond ((not (eq kind :shell))
                                           (and kind (list (list kind source nil))))
                                          (source (shell-command-entries source))
                                          (t (list (list :command nil nil))))))
            :unknown-p (or (set-difference unknown verbs :test #'string=)
                           (set-difference dynamic verbs :test #'string=))
            :note (snippet-note (or command ""))
            :head (snippet-head-line command)
            :error-p error-p))))
      ;; tool names whose command string is a shell command, not a snippet
      ((member name '("snippet" "run" "bash" "shell" "sh" "exec_command" "evolved_shell") :test #'equal)
       (activity-from-entries
        (with-diff (shell-command-entries (or command "")))
        :head (snippet-head-line command)
        :error-p error-p)))))

(defun tool-argument-text (arguments keys)
  "The string value of the first of KEYS that tool ARGUMENTS — the call's
JSON, complete or still streaming — names: the complete object through the
reader, a streaming prefix through the probe the band preview uses."
  (when (and (stringp arguments) (plusp (length arguments)))
    (let* ((trimmed (string-right-trim '(#\Space #\Newline #\Return #\Tab) arguments))
           ;; Only a text that could be whole goes to the reader: a streaming
           ;; prefix would signal on every delta.
           (root (and (uiop:string-suffix-p trimmed "}")
                      (ignore-errors (nlk:decode-json arguments)))))
      (if (hash-table-p root)
          (loop for key in keys thereis (json-value root :string key))
          (partial-json-string arguments keys)))))

(defun tool-arguments-activity (name arguments &key metadata error-p)
  "TOOL-CALL-ACTIVITY over a call's raw ARGUMENTS JSON, complete or
streaming: the command under +TOOL-COMMAND-KEYS+."
  (tool-call-activity name (tool-argument-text arguments +tool-command-keys+)
                      :metadata metadata :error-p error-p))

(defun tool-activity-verb (name &optional arguments)
  "The verb a channel's digest names a tool call by while it
streams and runs: what the call is doing in words when this module has them
(`reading src/tui/ui.lisp', `evaluating'), else `running NAME' — `running
tool' before the provider has named the call. => (values VERB KIND): KIND
the call's family, the mark the live bar wears beside the tool's own verb,
or :DEFAULT for a call with no words."
  (let* ((named (and (stringp name) (plusp (length name))))
         (activity (and named (tool-arguments-activity name (or arguments "")))))
    (if activity
        (values (activity-bar-verb activity) activity.family)
        (values (format nil "running ~a" (if named name "tool")) :default))))

;;; --- words -------------------------------------------------------------------

(defparameter *activity-source-limit* 60
  "How many characters of one source a title prints before it cuts.")

(defun activity-source-text (family source)
  "SOURCE as a title prints it for FAMILY: one line, cut past
*ACTIVITY-SOURCE-LIMIT*, quoted for a pattern family."
  ;; A wait of any plain number is seconds, 0.5 as much as 5: `Waited 0.5'
  ;; named no unit. One that spells its own (sleep 1m) keeps it.
  (let ((cut (stub-clip (substitute #\Space #\Newline
                                    (string-trim '(#\Space #\Newline) source))
                        *activity-source-limit*)))
    (cond ((eq :pattern (getf (activity-family-plist family) :source))
           (format nil "'~a'" cut))
          ((and (eq family :wait) (number-text-p cut)) (format nil "~as" cut))
          ;; An evaluation's number is the model's handle: the operator reads
          ;; the job it names, as the band shows it.
          ((and (eq family :poll) (plusp (length cut)) (every #'digit-char-p cut))
           "the background job's result")
          ;; `Listed .' read as punctuation.
          ((and (eq family :list) (member cut '("." "./") :test #'string=)) "the folder")
          (t cut))))

(defun number-text-p (text)
  "Whether TEXT spells a plain number, as a snippet's number token or a shell's
sleep argument does: digits with at most one point, 12, 0.5 or .5."
  (and (ppcre:scan "\\A(?:\\d+\\.?\\d*|\\.\\d+)\\z" text) t))

(defun waits-text (sources)
  "Several waits in words, SOURCES how each snippet spelled its time: the time
they took together when every one says it -- a plain number is seconds, as
sleep reads it, 1m and 2h their units -- else how many there were."
  ;; Without this, `Waited 2 pauses' would leave the sum to the reader.
  (let ((seconds (loop for source in sources
                       collect (cond ((null source) nil)
                                     ((number-text-p source)
                                      (let ((point (position #\. source)))
                                        (/ (parse-integer (remove #\. source))
                                           (expt 10 (if point (- (length source) point 1) 0)))))
                                     (t (nlk:parse-duration source))))))
    (if (every #'identity seconds)
        (let ((total (reduce #'+ seconds)))
          (cond ((>= total 60) (nlk:duration-text total))
                ((integerp total) (format nil "~ds" total))
                (t (format nil "~as" (string-right-trim "." (string-right-trim "0" (format nil "~,2f" total)))))))
        (case (length sources) (1 "") (2 "twice") (t (format nil "~d times" (length sources)))))))

(defun activity-source-target (family source)
  "What a click on SOURCE opens for FAMILY — (:PATH . SOURCE) for the
families whose sources are files and folders, (:URL . SOURCE) for the one
that fetches, NIL where a source is a pattern, a count or code."
  ;; The whole source, never the cut a title prints: the words belong to the
  ;; title, the address to the fact.
  (let ((kind (getf (activity-family-plist family) :source)))
    (when (and (member kind '(:path :url))
               (stringp source)
               (plusp (length (string-trim '(#\Space #\Tab #\Newline) source))))
      (cons kind source))))

(defun activity-entries-phrases (entries &key running)
  "ENTRIES as phrases, one per family in the order the families first
appear: (VERB TEXT DETAIL TARGET) — the verb, what it acted on as a title
prints it, the parenthesised aside a single call adds, and what a click on
TEXT opens (ACTIVITY-SOURCE-TARGET) or NIL."
  ;; A phrase that counts rather than names (`3 files') carries no target, so
  ;; a folded run never promises a click it cannot deliver.
  (mapcar (lambda (family &aux (entries (remove family entries :key #'first :test-not #'eq))
                               (plist (activity-family-plist family))
                               (sources (nlk:distinct (remove nil (mapcar #'second entries))))
                               (count (if (eq family :wait)
                                          (length entries)
                                          (+ (length sources) (count nil entries :key #'second))))
                               (source (and (= 1 count) sources (first sources))))
            ;; ("Read" "src/x.lisp" "1-80" (:PATH . "src/x.lisp")) for one
            ;; source, ("Read" "3 files" NIL NIL) for several.
            (list (getf plist (if running :present :past))
                  (cond (source (activity-source-text family source))
                        ((eq family :wait) (waits-text (mapcar #'second entries)))
                        (t (format nil "~d ~a" count (getf plist (if (= 1 count) :one :many)))))
                  (and source (= 1 (length entries)) (third (first entries)))
                  (and source (activity-source-target family source))))
          (remove-duplicates (mapcar #'first entries) :from-end t)))

(defun activity-code-title-p (activity)
  "Whether ACTIVITY is titled by its first line of code: a snippet that ran
something unknown, with no note and nothing known to say."
  (and (eq :eval activity.family) (null activity.note) (null activity.entries) activity.head t))

(defun activity-title-phrases (activity &key running &aux (entries activity.entries)
                                                          (eval-p (eq :eval activity.family)))
  "ACTIVITY's title as phrases, (VERB TEXT DETAIL TARGET) each, for a
renderer that paints the verbs apart and opens what a phrase names."
  ;; A snippet that ran something unknown is titled by its note, else by what its
  ;; known calls did, else by its first line of code — one phrase with no
  ;; verb; every other call by its entries. RUNNING puts the verbs in the
  ;; present tense.
  (cond ((and eval-p (activity-note activity))
         (list (list nil activity.note nil nil)))
        ((or entries (not eval-p)) (activity-entries-phrases entries :running running))
        ((activity-head activity) (list (list nil activity.head nil nil)))
        (t (list (list (getf (activity-family-plist :eval) (if running :present :past))
                       "" nil nil)))))

(defun activity-phrases-text (phrases)
  "PHRASES joined as a title reads: `Read 4 files, Searched 'tool-row''."
  (format nil "~{~a~^, ~}"
          (loop for (verb text detail) in phrases
                for rest = (format nil "~a~@[ (~a)~]" text detail)
                collect (cond ((null verb) rest)
                              ((zerop (length rest)) verb)
                              (t (format nil "~a ~a" verb rest))))))

(defun activity-entries-title (entries &key running)
  "ENTRIES in words: `Read 4 files, Searched 'tool-row''. NIL for none."
  (when entries
    (activity-phrases-text (activity-entries-phrases entries :running running))))

(defun activity-title (activity &key running)
  "ACTIVITY in words — the card's title (ACTIVITY-TITLE-PHRASES joined)."
  (activity-phrases-text (activity-title-phrases activity :running running)))

(defun activity-bar-verb (activity)
  "ACTIVITY as a running call's verb: the call's own note as the model wrote
it, `check the lanes', or else its words, lower case and present tense:
`reading src/tui/ui.lisp', `evaluating'."
  ;; The note wins whatever the family.
  (let ((text (or activity.note
                  (string-downcase (or (activity-entries-title activity.entries :running t)
                                       "evaluating")
                                   :end 1))))
    (stub-clip (substitute #\Space #\Newline text) 48)))

