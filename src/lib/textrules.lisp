;;;; textrules.lisp --- compiled deny/allow pattern rules over one text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure by construction: rule data in, a refusal string or NIL out. No I/O,
;;;; no threads, no policy about WHAT text is scanned — the caller owns that.
;;;; Relocated from the guard cell's policy engine, vocabulary stripped;
;;;; the behaviors below were each proven against a concrete failure there.
;;;;
;;;;   1. MATCHES ARE POSITIONS, NOT SUBSTRING HITS. `(format t ...)' contains
;;;;      the substring "rm", so a SEARCH-based matcher refuses every
;;;;      formatted output in a tree. cl-ppcre with \b in the caller's
;;;;      patterns is the floor, and positions are what span-scoping needs.
;;;;
;;;;   2. ALLOW OVERRIDES ARE SPAN-SCOPED, NOT WHOLE-TEXT. A whole-text
;;;;      override is a composable bypass: text that matches the allow
;;;;      pattern SOMEWHERE would exempt a real hit sitting next to it. An
;;;;      allow match exempts only the region it covers; any deny hit outside
;;;;      every allow span still refuses.
;;;;
;;;;   3. GATED RULES FIRE ONLY WITH THE GATE. A rule marked :GATED T matches
;;;;      only when the ruleset's :GATE pattern also matches the text — the
;;;;      generic form of the guard's spawn gate, where shell rules fire only
;;;;      when the same text names a process-spawning operator. The gate is
;;;;      caller data, never built in.
;;;;
;;;; A bad pattern is skipped with one warning, never fatal: one unusable
;;;; rule must not take the ruleset down with it. A gated rule in a ruleset
;;;; with no gate is skipped the same way — firing it ungated would flip the
;;;; direction the gate exists to protect.

(in-package #:nodecode.kernel)

(defstruct (text-rule (:copier nil))
  "One compiled deny rule."
  ;; SCANNER is what matches; ID and REASON are what the refusal says, and the
  ;; pattern source is spent compiling the scanner.
  (id (error "id required") :type string :read-only t)
  (scanner (error "scanner required") :read-only t)
  (reason "" :type string :read-only t)
  (gated nil :type boolean :read-only t))

(access (rule text-rule))

(defstruct (text-rules (:copier nil))
  "A compiled ruleset."
  ;; ALLOW-SCANNERS exempt the regions they match, never the whole text;
  ;; GATE-SCANNER arms the :GATED rules, or NIL when none was given.
  (rules '() :type list :read-only t)
  (allow-scanners '() :type list :read-only t)
  (gate-scanner nil :read-only t))

(access (ruleset text-rules))

(defun compile-text-scanner (pattern label)
  "A case-insensitive scanner, or NIL plus one warning on a bad pattern."
  (handler-case (ppcre:create-scanner pattern :case-insensitive-mode t)
    (error (condition)
      (warn "~a skipped: bad pattern ~s: ~a"
            label pattern (princ-to-string condition))
      nil)))

(defun compile-text-rules (&key rules allow gate)
  "Compile RULES (plists of :ID :PATTERN :REASON :GATED), ALLOW patterns, and
the GATE pattern into a TEXT-RULES. Bad patterns are skipped with a warning."
  (let ((gate-scanner (and (stringp gate)
                           (compile-text-scanner gate "gate pattern"))))
    (make-text-rules
     :rules (loop for spec in rules
                  for index from 0
                  for id = (or (getf spec :id) (format nil "rule-~a" index))
                  for pattern = (getf spec :pattern)
                  for gated = (and (getf spec :gated) t)
                  ;; WARN answers NIL, so a skipped rule collects nothing.
                  for rule = (cond ((not (stringp pattern))
                                    (warn "text rule ~a skipped: no pattern" id))
                                   ((and gated (null gate-scanner))
                                    (warn "text rule ~a skipped: gated, but the ruleset has no gate"
                                          id))
                                   (t (when-let (scanner (compile-text-scanner
                                                          pattern (format nil "text rule ~a" id)))
                                        (make-text-rule :id id
                                                        :scanner scanner
                                                        :reason (or (getf spec :reason) id)
                                                        :gated gated))))
                  when rule collect rule)
     :allow-scanners (loop for pattern in allow
                           for index from 0
                           for scanner = (if (stringp pattern)
                                             (compile-text-scanner
                                              pattern (format nil "allow pattern ~a" index))
                                             (warn "allow pattern ~a skipped: not a string" index))
                           when scanner collect scanner)
     :gate-scanner gate-scanner)))

;;; --- spans ---------------------------------------------------------------

;;; --- the refusal ---------------------------------------------------------

(defun text-rules-refusal (ruleset text)
  "The refusal string the first matching rule produces, or NIL when TEXT is
allowed."
  ;; A non-string TEXT abstains: the caller knows what a missing text means,
  ;; and better than we could. The excerpt rides a FORMAT argument, never a
  ;; control string, so foreign bytes in it cannot re-enter FORMAT.
  (when (and (text-rules-p ruleset) (stringp text))
    (let* ((spans (loop for scanner in (text-rules-allow-scanners ruleset) ; START END ...
                        append (ppcre:all-matches scanner text)))
           (gate ruleset.gate-scanner)
           (armed (and gate (ppcre:scan gate text) t)))
      (dolist (rule ruleset.rules nil)
        (when (or armed (not rule.gated))
          ;; the rule's first match no allow span covers
          (when-let (hit (ppcre:do-matches (start end rule.scanner text nil)
                           (unless (loop for (allow-start allow-end) on spans by #'cddr
                                         thereis (and (<= allow-start start) (<= end allow-end)))
                             (return (cons start end)))))
            ;; ~s would print a base string readably, as a tool call binds it:
            ;; #A((4) BASE-CHAR . "sudo").
            (return (format nil "rule ~a (~a) matched \"~a\""
                            rule.id
                            rule.reason
                            ;; The excerpt is capped at 80: enough to recognize the hit.
                            (clip (subseq text (car hit) (cdr hit)) 80)))))))))
