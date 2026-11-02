;;;; verbs.lisp --- a command line's words: one table, every reader.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command line is a small language: its verbs and options, the other
;;;; spellings each answers to, what each takes and what it is for. Written
;;;; as the branches of a parser, that language had nothing to read it back:
;;;; `nodecode profiles --help' answered `unknown argument' and a usage line
;;;; kept by hand beside the parser, which no --help printed and no typo
;;;; found its way through. Here it is a table of VERBs, and every reader
;;;; derives from the one table: the parser resolves a word and its aliases
;;;; (FIND-VERB), help renders the public entries (VERBS-TEXT,
;;;; VERB-HELP-TEXT), a refusal names the nearest spelling
;;;; (UNKNOWN-WORD-TEXT). The slash catalog (tui/slash.lisp) is the same idea
;;;; for the composer; this one is the kernel's, for the executable's launch
;;;; (tui/ui.lisp) and the profile verbs under it (organism/profile.lisp).

(in-package #:nodecode.kernel)

(define-record (verb (:constructor make-verb
                         (name summary &key aliases (arguments "") label detail internal terminal))
                     (:copier nil) (:export t))
  "One word a command line reads: a verb, or an option spelled with its
dashes."
  ;; NAME is the spelling help shows; ALIASES the others the parser also
  ;; accepts (`profiles' for `profile', `-h' for `--help'); ARGUMENTS what
  ;; follows it, as its usage line writes them; LABEL the left column of a
  ;; list when that is not NAME and ARGUMENTS; SUMMARY one line; DETAIL the
  ;; paragraph its own help adds; INTERNAL for a verb the organism spawns for
  ;; itself, which help never lists and no suggestion offers; TERMINAL for a
  ;; verb that draws on the controlling terminal, which a launch refuses to
  ;; start where the process has none.
  (name "" :type string)
  (summary "" :type string)
  (aliases '() :type list)
  (arguments "" :type string)
  (label nil :type (or null string))
  (detail nil :type (or null string))
  (internal nil)
  (terminal nil))

(defun find-verb (word verbs)
  "The entry of VERBS that WORD names, by its name or an alias, or NIL."
  ;; Exact and case-sensitive, like the shells that pass the word in; an
  ;; internal entry is found like any other.
  (and (stringp word)
       (find-if (lambda (verb)
                  (or (string= word verb.name) (member word verb.aliases :test #'string=)))
                verbs)))

(defun help-flag-p (word)
  "Whether WORD asks for help the way every command line does: --help, -h."
  (and (stringp word) (member word '("--help" "-h") :test #'string=) t))

;;; --- the nearest spelling ---------------------------------------------------------

(defun edit-distance (a b)
  "How many single-character edits turn A into B: an insertion, a deletion,
a substitution, or two neighbours swapped each count one (the optimal
string alignment distance), so `porfile' is one step from `profile'."
  (let ((d (make-array (list (1+ (length a)) (1+ (length b))) :element-type 'fixnum)))
    (dotimes (i (1+ (length a)) (aref d (length a) (length b)))
      (dotimes (j (1+ (length b)))
        (setf (aref d i j)
              (if (zerop (min i j))
                  (max i j)
                  (min (1+ (aref d (1- i) j))
                       (1+ (aref d i (1- j)))
                       (+ (aref d (1- i) (1- j)) (if (char= (char a (1- i)) (char b (1- j))) 0 1))
                       (if (and (> i 1) (> j 1)
                                (char= (char a (1- i)) (char b (- j 2)))
                                (char= (char a (- i 2)) (char b (1- j))))
                           (1+ (aref d (- i 2) (- j 2)))
                           (+ i j)))))))))

(defun nearest-words (word candidates &key (limit 3))
  "The CANDIDATES a mistyped WORD most likely meant, in CANDIDATES' order,
at most LIMIT: those at the smallest edit distance, when that is within a
third of WORD's length (one edit at least); else the one candidate WORD
begins, when exactly one does."
  ;; NIL when nothing is close: a guess that far off misleads more than it
  ;; helps.
  (when (and (stringp word) (plusp (length word)))
    (let* ((candidates (remove-duplicates candidates :test #'string= :from-end t))
           (allowed (max 1 (floor (length word) 3)))
           (scored (loop for candidate in candidates
                         for distance = (edit-distance word candidate)
                         when (<= distance allowed)
                           collect (cons distance candidate))))
      (if scored
          (loop with best = (reduce #'min scored :key #'car) and count = 0
                for (distance . candidate) in scored
                when (and (= distance best) (<= (incf count) limit)) collect candidate)
          (let ((begun (remove-if-not (lambda (candidate)
                                        (and (>= (length word) 2)
                                             (> (length candidate) (length word))
                                             (string= word candidate :end2 (length word))))
                                      candidates)))
            (and (= 1 (length begun)) begun))))))

(defun unknown-word-text (kind word verbs)
  "What a refusal of WORD says, in one line: `no KIND \"WORD\".', then `Did
you mean \"X\"?' when the public entries of VERBS hold a spelling close to
it."
  (let* ((public (remove-if #'verb-internal verbs))
         (near (nearest-words word (append (mapcar #'verb-name public)
                                           (loop for verb in public append verb.aliases)))))
    (format nil "no ~a ~s.~@[ Did you mean ~a?~]" kind word
            (and near (format nil "~{~s~#[~; or ~:;, ~]~}" near)))))

(defmacro with-verbs ((args verbs &key refusal usage noun default command) &body body)
  "Read ARGS -- the words after a command, one string or a list -- against
VERBS and run BODY for the verb they name (DEFAULT with none), with WORDS
bound to what follows it, VERB to its name, USAGE-FAIL to the refusal (a
REFUSAL whose detail points at the help) and NOTHING-MORE to the one for
words left over."
  ;; `help [VERB]', or --help or -h anywhere, answers HELP instead: VERB's own
  ;; (VERB-HELP-TEXT), or USAGE, a FORMAT control over COMMAND twice and the
  ;; VERBS-TEXT; a word no entry names is refused as NOUN. COMMAND is how the
  ;; calling shell spells the command.
  `(let* ((words (if (listp ,args)
                     (copy-list ,args)
                     (remove "" (uiop:split-string ,args :separator " ") :test #'string=)))
          (entry (and words (find-verb (first words) ,verbs)))
          (verb (if entry (verb-name entry) ,default)))
     (labels ((help (about)
                (if about
                    (verb-help-text about :command ,command)
                    (format nil ,usage ,command ,command (verbs-text ,verbs))))
              (usage-fail (about control &rest arguments)
                (error ',refusal
                       :detail (format nil "~? Run \"~a help~@[ ~a~]\" to see ~:[the verbs~;its usage~]."
                                       control arguments ,command about about)))
              (nothing-more ()
                (when words
                  (usage-fail verb "~a does not take ~{~s~^ ~}." verb words))))
       (cond
         ((and entry (string= verb "help"))
          (let* ((topic (second words))
                 (about (and topic (find-verb topic ,verbs))))
            (cond ((or (null topic) about) (help about))
                  (t (usage-fail nil "~a" (unknown-word-text ,noun topic ,verbs))))))
         ((some #'help-flag-p words)
          (help entry))
         ((and words (null entry))
          (usage-fail nil "~a" (unknown-word-text ,noun (first words) ,verbs)))
         (t
          (pop words)
          ,@body)))))

;;; --- help ---------------------------------------------------------------------------

(defun verbs-text (verbs &key (indent 2) (column 26))
  "The public entries of VERBS, one line each: the left column indented by
INDENT, then the summary starting at COLUMN, or two spaces after a left
column that reaches it."
  (with-output-to-string (out)
    (dolist (verb (remove-if #'verb-internal verbs))
      (let ((left (format nil "~va~a" indent ""
                          (or verb.label
                              (string-right-trim " " (format nil "~a ~a" verb.name
                                                             verb.arguments))))))
        (format out "~a~va~a~%" left (max 2 (- column (length left))) "" verb.summary)))))

(defun verb-help-text (verb &key (command ""))
  "One entry's own help: its usage line under COMMAND (\"nodecode\",
\"nodecode profile\", \"/profile\"), its summary as a sentence, its detail."
  (let ((usage (string-right-trim
                " " (format nil "~@[~a ~]~a ~a"
                            (and (plusp (length command)) command)
                            verb.name verb.arguments)))
        (summary verb.summary))
    ;; The summary as a sentence: its first letter up, a full stop at the end.
    (when (plusp (length summary))
      (setf summary (string-upcase summary :end 1))
      (unless (find (char summary (1- (length summary))) ".?!")
        (setf summary (concatenate 'string summary "."))))
    (format nil "usage: ~a~%~%~a~@[~%~%~a~]~%" usage summary verb.detail)))
