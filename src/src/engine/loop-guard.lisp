;;;; loop-guard.lisp --- a model repeating itself is a failed attempt, not an answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Some models fall into a degenerate loop mid-generation: Gemini, DeepSeek
;;;; and Grok re-emit one paragraph with cosmetic drift ("Confirming safety",
;;;; "Verifying completion", ...) until the output budget is gone, and a model
;;;; can stutter one exact passage back to back. The idle watchdog never fires
;;;; -- the stream is lively -- so without a guard the round streams the loop
;;;; until the output limit and records it as the answer.
;;;;
;;;; The guard reads what one attempt streams, as CALL-PROVIDER's observer
;;;; sees it, and on a loop fails the attempt with PROVIDER-REASONING-LOOP: a
;;;; contract failure the retry ladder re-runs and never continues, so the
;;;; loop is sealed as a voided attempt and never reaches history, and a spent
;;;; budget hands the round to the failover chain as a broken answer.
;;;;
;;;; Three shapes, each a port of oh-my-pi's thinking-loop guard
;;;; (packages/ai/src/utils/thinking-loop.ts at 18.14.2, MIT, Copyright (c)
;;;; 2025 Mario Zechner, 2025-2026 Can Bölük, 2026 Stencil Labs, Inc.), with
;;;; its thresholds as it calibrated them on real reasoning (13.5k non-loop
;;;; thinking blocks for the paragraph check, 536k for the wording check):
;;;;   1. an exact cycle at the end of the stream -- one unit repeated back to
;;;;      back -- on every guarded model;
;;;;   2. near-duplicate paragraphs, word-trigram Jaccard over the recent ones;
;;;;   3. a stall: paragraphs that reshuffle recent wording and name nothing
;;;;      new (no new path, identifier or code span).
;;;; 2 and 3 judge prose only, and run at the :SEMANTIC level alone.
;;;;
;;;; The level is the model's (LOOP-GUARD-LEVEL): :OFF for Claude, whose
;;;; summarized thinking restates one status line while hidden work runs and
;;;; which has no runaway to guard; :SEMANTIC for Gemini, DeepSeek and Grok;
;;;; :EXACT for every other model. providers.<id>.loop_guard ("off", "exact",
;;;; "semantic", or false) outranks the family.

(in-package #:nodecode.evolved)

;;; --- thresholds: oh-my-pi's, unchanged -------------------------------------

(defparameter +loop-tail-window+ 4096
  "Characters of the stream's tail the exact-cycle check scans.")
(defparameter +loop-max-unit+ 1024
  "The longest exact cycle considered, in characters.")
(defparameter +loop-check-stride+ 128
  "New characters between exact scans; a delta this long is scanned at once.")
(defparameter +loop-short-unit+ 60
  "A cycle this short or shorter needs four repeats; a longer one, three.")
(defparameter +loop-short-repeated-chars+ 180
  "What a short one-line cycle must cover to count as a loop.")
(defparameter +loop-long-repeated-chars+ 1024
  "What a long cycle, or a cycle spanning a line break, must cover: a cycle
that repeats whole lines needs the long floor, since a summarizer restating a
status line is ordinary.")
(defparameter +loop-segment-cap+ 700
  "A paragraph with no blank line past this many characters is cut here, so a
wall of text still segments.")
(defparameter +loop-segment-floor+ 60
  "Normalized characters below which a paragraph is too short to judge.")
(defparameter +loop-segment-window+ 16
  "Recent paragraphs a new one is compared against.")
(defparameter +loop-segment-similarity+ 4/5
  "Word-trigram Jaccard at or above which two paragraphs are near-duplicates.
Exact: a float 0.8 compares as the rational a hair above 4/5, which a
Jaccard of exactly 4/5 would miss.")
(defparameter +loop-segment-warmup+ 8
  "Paragraphs judged before either paragraph check may fire.")
(defparameter +loop-segment-cluster+ 4
  "Near-duplicates (the new paragraph among them) that make a loop.")
(defparameter +loop-novelty-window+ 8
  "Recent paragraphs whose pooled words are the novelty baseline.")
(defparameter +loop-novelty-floor+ 1/5
  "Novelty -- the share of a paragraph's words its recent ones never used -- at
or below which it recycles recent wording.")
(defparameter +loop-stall-run+ 8
  "Recycling paragraphs in a row, none naming anything new, that make a loop.
One above the longest such run oh-my-pi found in 536k legitimate blocks.")

;;; --- what a paragraph is, read the way the heuristics read it ---------------

(defparameter +loop-code-line+
  (ppcre:create-scanner
   "^(?:[ \\t]*```.*|(?: {2,}|\\t)(?![ \\t]*(?:[-*+]|\\d+[.)])[ \\t]).*|.*[{}\\[\\](;,>][ \\t]*)$"
   :multi-line-mode t)
  "A whole line shaped like code: a fence, an indented line that is not a
list item, or one ending in a block, statement or markup delimiter. Code
repeats one skeleton with different literals, so the paragraph checks drop it.
Read per line, never by fence state: a summary often ends inside an open fence.")

(defparameter +loop-heading-line+
  (ppcre:create-scanner "^[ \\t]*#{1,6}[ \\t].*$" :multi-line-mode t)
  "A markdown heading: a summarizer's per-thought title, not reasoning.")

(defparameter +loop-title-line+
  (ppcre:create-scanner "^[ \\t]*\\*{2,3}.+?\\*{2,3}[ \\t]*$" :multi-line-mode t)
  "A whole-line bold title, \"**Maintaining Momentum**\": its ever-changing
wording would mask a loop by inflating novelty.")

(defparameter +loop-paragraph-break+ (ppcre:create-scanner "\\n\\s*\\n")
  "What ends a paragraph: a blank line.")

(defparameter +loop-anchor+
  (ppcre:create-scanner
   "`[^`]+`|\\b\\w{2,}\\.[a-zA-Z]\\w{0,4}\\b|[\\w-]+(?:/[\\w-]+){2,}|\\b\\w+_\\w+\\b|\\b[a-z]+[A-Z]\\w*\\b|\\b[A-Z][a-z]+[A-Z]\\w*\\b")
  "A concrete reference the model reasons about: a code span, a file name or
dotted member, a path of three parts or more, a snake, camel or Pascal
identifier. A paragraph naming a NEW one is progress, which spares per-file
work; bare numbers and abbreviations (\"Step 2\", \"i.e.\") anchor nothing.")

(defun loop-pictographic-p (char &aux (code (char-code char)))
  "Whether CHAR is an emoji or another Extended_Pictographic sign."
  (or (<= #x1F000 code #x1FAFF) (<= #x1FC00 code #x1FFFD) (<= #x2600 code #x27BF)
      (<= #x2194 code #x2199) (<= #x23E9 code #x23F3) (<= #x23F8 code #x23FA)
      (<= #x25FB code #x25FE) (<= #x2B05 code #x2B07)
      (member code '(#xA9 #xAE #x203C #x2049 #x2122 #x2139 #x21A9 #x21AA #x231A #x231B
                     #x2328 #x23CF #x24C2 #x25AA #x25AB #x25B6 #x25C0 #x2934 #x2935
                     #x2B1B #x2B1C #x2B50 #x2B55 #x3030 #x303D #x3297 #x3299))))

(defun loop-blank-p (char)
  "Whether CHAR is whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #\No-break_space)))

(defun exact-suffix-cycle (text)
  "The exact cycle TEXT ends in, as (values UNIT COUNT), or NIL. A unit holding
a letter or an emoji counts at the short or long floor; one holding neither
(the `?!?!' a stuck model emits) only past a kilobyte; one of digits and
whitespace alone never: padding and zero-filled dumps are ordinary output."
  ;; A Z-array over TEXT reversed (indexed from the end, never copied) holds
  ;; every suffix period in one linear pass: Z[i] is how far TEXT's end agrees
  ;; with itself shifted by I, so a unit of length I repeats 1 + Z[i]/I times.
  (let ((n (length text)))
    (when (< n +loop-short-repeated-chars+)
      (return-from exact-suffix-cycle nil))
    (let* ((last (1- n))
           (max-unit (min +loop-max-unit+ (floor n 3)))
           (z (make-array (1+ max-unit) :element-type 'fixnum :initial-element 0))
           (left 0)
           (right 0))
      (loop for i from 1 to max-unit
            do (let ((zi (if (<= i right) (min (1+ (- right i)) (aref z (- i left))) 0)))
                 (loop while (and (< (+ i zi) n)
                                  (char= (char text (- last zi)) (char text (- last i zi))))
                       do (incf zi))
                 (setf (aref z i) zi)
                 (when (> (+ i zi -1) right)
                   (setf left i
                         right (+ i zi -1)))))
      (loop for len from 2 to max-unit
            for repeats = (1+ (floor (aref z len) len))
            when (>= repeats (if (<= len +loop-short-unit+) 4 3))
              do (let ((unit (subseq text (- n len))))
                   (cond ((some (lambda (c) (or (alpha-char-p c) (loop-pictographic-p c))) unit)
                          (when (>= (* len repeats)
                                    (if (and (<= len +loop-short-unit+) (not (find #\Newline unit)))
                                        +loop-short-repeated-chars+
                                        +loop-long-repeated-chars+))
                            (return-from exact-suffix-cycle (values unit repeats))))
                         ((every (lambda (c) (or (digit-char-p c) (loop-blank-p c))) unit))
                         ((>= (* len repeats) +loop-long-repeated-chars+)
                          (return-from exact-suffix-cycle (values unit repeats))))))
      nil)))

(defun normalize-loop-segment (segment)
  "SEGMENT as lowercase words of letters and digits, one space apart, the
words that are only digits dropped."
  (let ((words (ppcre:split " +" (map 'string (lambda (c) (if (or (char<= #\a c #\z) (char<= #\0 c #\9)) c #\Space))
                                       (string-downcase segment)))))
    (format nil "~{~a~^ ~}" (remove-if-not (lambda (word) (find-if #'alpha-char-p word)) words))))

(defun loop-word-set (words)
  "WORDS as a set."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (word words set) (setf (gethash word set) t))))

(defun trigram-set (normalized)
  "The word-trigram shingles of NORMALIZED, a set; the words joined when there
are fewer than three."
  (let ((words (coerce (ppcre:split " " normalized) 'vector)))
    (if (< (length words) 3)
        (loop-word-set (and (plusp (length words)) (list normalized)))
        (loop-word-set (loop for i from 0 to (- (length words) 3)
                             collect (format nil "~a ~a ~a" (aref words i) (aref words (+ i 1))
                                             (aref words (+ i 2))))))))

(defun loop-jaccard (a b)
  "The Jaccard similarity of sets A and B."
  (if (or (zerop (hash-table-count a)) (zerop (hash-table-count b)))
      0
      (let ((shared (loop for key being the hash-keys of (if (< (hash-table-count a) (hash-table-count b)) a b)
                          count (gethash key (if (< (hash-table-count a) (hash-table-count b)) b a)))))
        (/ shared (- (+ (hash-table-count a) (hash-table-count b)) shared)))))

;;; --- the detector: one per watched stream of text ---------------------------

(defstruct (loop-detector (:constructor make-loop-detector (semantic-p)))
  "What one stream of text has said so far, as the loop checks need it."
  (semantic-p nil :read-only t)
  ;; the exact check: the tail, and characters since its last scan
  (tail "")
  (unscanned 0)
  ;; the paragraph checks: what is not yet a whole paragraph, the recent
  ;; paragraphs' trigrams, words and anchors, how many were judged, and the
  ;; recycling run
  (pending "")
  (trigrams '())
  (words '())
  (anchors '())
  (judged 0)
  (stall 0))

(defun exact-cycle-detail (unit count)
  "The words a detected exact cycle is reported in."
  (format nil "repeated one ~d-character passage ~d times back to back" (length unit) count))

(defun loop-detector-push (detector delta)
  "Feed DELTA, the next streamed text, to DETECTOR: the loop it completes, in
words, or NIL."
  (when (zerop (length delta))
    (return-from loop-detector-push nil))
  (let ((tail (concatenate 'string (loop-detector-tail detector) delta)))
    (setf (loop-detector-tail detector)
          (if (> (length tail) +loop-tail-window+) (subseq tail (- (length tail) +loop-tail-window+)) tail)))
  ;; at a bounded cadence: a scan per token-sized delta would be quadratic
  (when (or (>= (incf (loop-detector-unscanned detector) (length delta)) +loop-check-stride+)
            (>= (length delta) +loop-check-stride+))
    (setf (loop-detector-unscanned detector) 0)
    (multiple-value-bind (unit count) (exact-suffix-cycle (loop-detector-tail detector))
      (when unit
        (return-from loop-detector-push (exact-cycle-detail unit count)))))
  (when (loop-detector-semantic-p detector)
    (setf (loop-detector-pending detector) (concatenate 'string (loop-detector-pending detector) delta))
    (loop
      (let* ((pending (loop-detector-pending detector))
             (paragraph (multiple-value-bind (start end) (ppcre:scan +loop-paragraph-break+ pending)
                          (cond (start
                                 (setf (loop-detector-pending detector) (subseq pending end))
                                 (subseq pending 0 start))
                                ((> (length pending) +loop-segment-cap+)
                                 (setf (loop-detector-pending detector) (subseq pending +loop-segment-cap+))
                                 (subseq pending 0 +loop-segment-cap+))))))
        (unless paragraph
          (return nil))
        (when-let (hit (judge-loop-paragraphs detector paragraph))
          (return hit))))))

(defun loop-detector-flush (detector)
  "Judge what DETECTOR still holds at the end of its stream: the exact check
once more, whatever its cadence, and the last paragraph, which may be the one
that completes a cluster. The loop, in words, or NIL."
  (multiple-value-bind (unit count) (exact-suffix-cycle (loop-detector-tail detector))
    (when unit
      (return-from loop-detector-flush (exact-cycle-detail unit count))))
  (when (and (loop-detector-semantic-p detector) (plusp (length (loop-detector-pending detector))))
    (let ((rest (loop-detector-pending detector)))
      (setf (loop-detector-pending detector) "")
      (judge-loop-paragraphs detector rest))))

(defun judge-loop-paragraphs (detector text)
  "Judge TEXT, one paragraph, in pieces of at most +LOOP-SEGMENT-CAP+ so each
stays comparable: the first loop one completes, in words, or NIL."
  (loop for start from 0 below (length text) by +loop-segment-cap+
        thereis (judge-loop-segment detector (subseq text start (min (length text) (+ start +loop-segment-cap+))))))

(defun judge-loop-segment (detector raw)
  "Fold RAW, one paragraph, into DETECTOR's recent ones: the loop it
completes, in words, or NIL."
  (let* ((segment (ppcre:regex-replace-all
                   +loop-title-line+
                   (ppcre:regex-replace-all +loop-heading-line+ (ppcre:regex-replace-all +loop-code-line+ raw "") "")
                   ""))
         (normalized (normalize-loop-segment segment)))
    (when (< (length normalized) +loop-segment-floor+)
      (return-from judge-loop-segment nil))
    (let* ((trigrams (trigram-set normalized))
           ;; near-duplicates: the same paragraph with cosmetic drift
           (cluster (1+ (count-if (lambda (earlier) (>= (loop-jaccard trigrams earlier) +loop-segment-similarity+))
                                  (loop-detector-trigrams detector))))
           (words (loop-word-set (ppcre:split " " normalized)))
           ;; a stall: recent wording reshuffled, nothing new named
           (unseen (loop for word being the hash-keys of words
                         count (notany (lambda (earlier) (gethash word earlier)) (loop-detector-words detector))))
           (novelty (if (loop-detector-words detector) (/ unseen (hash-table-count words)) 1))
           (anchors (loop-word-set (mapcar (lambda (anchor) (string-downcase (remove #\` anchor)))
                                           (ppcre:all-matches-as-strings +loop-anchor+ segment))))
           (new-anchor-p (loop for anchor being the hash-keys of anchors
                               thereis (notany (lambda (earlier) (gethash anchor earlier))
                                               (loop-detector-anchors detector)))))
      (setf (loop-detector-stall detector)
            (if (and (<= novelty +loop-novelty-floor+) (not new-anchor-p)) (1+ (loop-detector-stall detector)) 0))
      (flet ((remember (list item limit)
               (let ((kept (append list (list item))))
                 (if (> (length kept) limit) (rest kept) kept))))
        (setf (loop-detector-trigrams detector) (remember (loop-detector-trigrams detector) trigrams +loop-segment-window+)
              (loop-detector-words detector) (remember (loop-detector-words detector) words +loop-novelty-window+)
              (loop-detector-anchors detector) (remember (loop-detector-anchors detector) anchors +loop-novelty-window+)))
      (when (>= (incf (loop-detector-judged detector)) +loop-segment-warmup+)
        (cond ((>= cluster +loop-segment-cluster+)
               (format nil "wrote ~d near-identical paragraphs within its last ~d" cluster +loop-segment-window+))
              ((>= (loop-detector-stall detector) +loop-stall-run+)
               (format nil "wrote ~d paragraphs in a row that only reshuffle its recent wording"
                       (loop-detector-stall detector))))))))

;;; --- the level a model is guarded at ----------------------------------------

(defun loop-guard-level (model &optional level)
  "How much of the loop guard watches MODEL's attempts: :OFF, :EXACT (exact
cycles) or :SEMANTIC (exact cycles plus the paragraph checks). LEVEL, the
operator's providers.<id>.loop_guard (CONFIGURED-LOOP-GUARD-LEVEL), outranks
the model's family."
  (let ((id (string-downcase (or model ""))))
    (or level
        (cond ((search "claude" id) :off)
              ((some (lambda (family) (search family id)) '("gemini" "deepseek" "grok")) :semantic)
              (t :exact)))))

;;; --- the guard: one per attempt ---------------------------------------------

(defstruct (loop-guard (:constructor %make-loop-guard (reasoning text)))
  "One attempt's watch: a detector over its reasoning and one over its text,
and which of them still listens."
  (reasoning nil :read-only t)
  (text nil :read-only t)
  ;; Reasoning is watched until the answer's text starts: a model that writes
  ;; its answer is past its thinking. Grok keeps reasoning after a tool call
  ;; opens, and that reasoning is still watched.
  (reasoning-p t)
  (answering-p nil)
  ;; Text is watched until a tool call opens: what follows is arguments.
  (text-p t))

(defun make-loop-guard (semantic-p)
  "A fresh guard, its two detectors at SEMANTIC-P."
  (%make-loop-guard (make-loop-detector semantic-p) (make-loop-detector semantic-p)))

(defun attempt-loop-guard (config)
  "The loop guard for an attempt under CONFIG, an effective provider config,
or NIL when its model is not guarded."
  (let ((level (loop-guard-level (effective-provider-config-model config) (config-loop-guard-level config))))
    (unless (eq level :off)
      (make-loop-guard (eq level :semantic)))))

(defun loop-guard-observe (guard part)
  "Feed PART, the attempt's next stream part, to GUARD, and fail the attempt
with PROVIDER-REASONING-LOOP when what it streamed has started to loop."
  (flet ((push-text (detector text) (and text (loop-detector-push detector text))))
    (let* ((reasoning (loop-guard-reasoning guard))
           (text (loop-guard-text guard))
           (detail
             (case (stream-part-kind part)
               (:reasoning-delta
                (unless (loop-guard-answering-p guard)
                  (setf (loop-guard-reasoning-p guard) t)
                  (when-let (loop (push-text reasoning (stream-part-text part)))
                    (format nil "its reasoning ~a" loop))))
               (:reasoning-end
                (when-let (loop (and (loop-guard-reasoning-p guard) (loop-detector-flush reasoning)))
                  (format nil "its reasoning ~a" loop)))
               (:text-delta
                (when (plusp (length (stream-part-text part)))
                  (setf (loop-guard-reasoning-p guard) nil
                        (loop-guard-answering-p guard) t))
                (when-let (loop (and (loop-guard-text-p guard) (push-text text (stream-part-text part))))
                  (format nil "its answer ~a" loop)))
               ((:tool-input-start :tool-input-delta :tool-call)
                (setf (loop-guard-text-p guard) nil))
               (:finish
                (or (when-let (loop (and (loop-guard-reasoning-p guard) (loop-detector-flush reasoning)))
                      (format nil "its reasoning ~a" loop))
                    (when-let (loop (and (loop-guard-text-p guard) (loop-detector-flush text)))
                      (format nil "its answer ~a" loop)))))))
      (when (stringp detail)
        (error 'provider-reasoning-loop
               :detail (format nil "the model looped: ~a" detail))))))
