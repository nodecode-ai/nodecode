;;;; composer-highlight-test.lisp --- a lane's tokens, painted in the composer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. A token a cell's lane opens paints in one of two tones: recognised
;;;;      when the lane's rows in hand name it, unresolved when they do not.
;;;;   2. The sigil opens a span only at a word start, as it opens the lane;
;;;;      trailing sentence punctuation is not part of the name.
;;;;   3. Nothing paints before a lane's rows are in hand, the file lane's @
;;;;      never paints, and a slash command's text is the command's.
;;;;   4. A lane whose token is in the text but whose rows are not in hand
;;;;      is wanted: the paint asks for its rows off the keystroke path, once
;;;;      until something lands.
;;;;   5. Without lanes the composer paints as it always did.

(in-package #:nodecode.test)

(defparameter *highlight-rows*
  (list (list "session" (cons "right" "teachingco") (cons "left" "pult")
              (cons "session:s-AB12" "soma"))))

(deftest composer-highlight-two-tones ()
  (with-lanes (("session" "#"))
    (is (equal '((4 10 :recognised) (15 22 :unresolved))
               (nle::composer-lane-spans "ask #right and #nobody x#right" *highlight-rows*)))
    (is (equal '((0 6 :recognised)) (nle::composer-lane-spans "#Right, go" *highlight-rows*)) "case-blind, and the comma is prose")
    (is (equal '((3 18 :recognised)) (nle::composer-lane-spans "to #session:s-AB12." *highlight-rows*)) "a colon inside the name is the name's")
    (is (null (nle::composer-lane-spans "x#right costs$5 #" *highlight-rows*)) "inside a word, or a bare sigil, is text")))

(deftest composer-highlight-needs-rows-and-leaves-other-lanes ()
  (with-lanes (("session" "#"))
    (is (null (nle::composer-lane-spans "ask #right" '())) "no rows in hand, no paint")
    (is (null (nle::composer-lane-spans "see @README.md" *highlight-rows*)) "the file lane never paints")
    (is (null (nle::composer-lane-spans "/ask #right" *highlight-rows*)) "a command's text is the command's"))
  (with-lanes ()
    (is (null (nle::composer-lane-spans "ask #right" *highlight-rows*)) "no lane, no paint")))

(deftest composer-highlight-wants-missing-rows ()
  (with-lanes (("session" "#") ("domain" "%"))
    (is (equal '("session" "domain")
               (nle::composer-lanes-wanting-rows "ask #right in %ops" '())))
    (is (equal '("domain")
               (nle::composer-lanes-wanting-rows "ask #right in %ops" *highlight-rows*)))
    (is (null (nle::composer-lanes-wanting-rows "x#right @a" '())))))

(deftest composer-highlight-paints-the-composer ()
  (with-lanes (("session" "#"))
    (with-frame-model (model 80 30 :session-id "lane-paint")
      (let ((nle:*color-profile* :truecolor))
        (setf (nle::chat-model-lane-rows model) *highlight-rows*)
        (type-string model "ask #right and #nobody")
        (let ((rows (apply #'concatenate 'string (nle::composer-rows model 80 30 nil)))
              (known (nle::seg-render (nle::make-seg :text "#right" :tone :lane-recognised) nil))
              (unknown (nle::seg-render (nle::make-seg :text "#nobody" :tone :lane-unresolved) nil)))
          (is (not (equal known unknown)) "the two tones differ")
          (is (search "#right" rows))
          (is (not (equalp (nle::style-for-tone :lane-recognised) (nle::style-for-tone :user-block))))
          (is (not (equalp (nle::style-for-tone :lane-unresolved) (nle::style-for-tone :user-block))))
          (is (= 2 (length (apply #'append (nle::composer-visible-lane-spans model)))))
          ;; on screen: the a of ask, the # of #right, the # of #nobody
          (let* ((lines (nle::composer-rows model 80 30 nil))
                 (cells (nlt::screen-buffer-cells (nlt:parse-styled-string lines 80 (length lines))))
                 (plain (map 'string (lambda (cell) (or (nlt::cell-char cell) #\Space)) cells))
                 (ask (search "ask #" plain))
                 (fg (lambda (at) (nlt::cell-fg (aref cells at)))))
            (is ask "the typed row is on screen")
            (let ((plain-fg (funcall fg ask))
                  (known-fg (funcall fg (+ ask 4)))
                  (unknown-fg (funcall fg (+ ask 15))))
              (is (not (equal plain-fg known-fg)) "#right is not body text")
              (is (not (equal plain-fg unknown-fg)) "#nobody is not body text")
              (is (not (equal known-fg unknown-fg)) "recognised and unresolved differ")
              (is (equal plain-fg (funcall fg (+ ask 11))) "the prose between is body text"))))))))

(deftest composer-highlight-asks-once-until-landing ()
  (with-lanes (("session" "#"))
    (with-frame-model (model 80 30 :session-id "lane-want")
      (let ((asked '()))
        (with-stubbed-fdefinition (nle::ask-lane-rows (m lane) (declare (ignore m)) (push lane asked) t)
          ;; the token behind the caret: the dropdown does not ask for it
          (type-string model "ask #right ")
          (nle::composer-rows model 80 30 nil)
          (nle::composer-rows model 80 30 nil)
          (is (equal '("session") asked) "one ask across frames")
          (nle::hand-over-lane-rows model "session" (list (cons "right" "teachingco")))
          (nle::land-composer-catalog-walks model)
          (is (null (nle::chat-model-lane-rows-wanted model)) "landing clears the mark")
          (nle::composer-rows model 80 30 nil)
          (is (equal '("session") asked) "rows in hand: no further ask"))))))

(deftest composer-highlight-without-lanes-paints-as-before ()
  (with-lanes ()
    (with-frame-model (model 80 30 :session-id "lane-none")
      (let ((nle:*color-profile* :truecolor))
        (type-string model "ask #right @README.md")
        (is (every #'null (nle::composer-visible-lane-spans model)))
        (is (null (nle::chat-model-lane-rows-wanted model)))))))

(deftest composer-highlight-follows-wrapped-rows ()
  (with-lanes (("session" "#"))
    (with-frame-model (model 24 30 :session-id "lane-wrap")
      (setf (nle::chat-model-lane-rows model) *highlight-rows*)
      (type-string model "a long line that wraps before #right and x#left")
      (let* ((lines (nle::editor-visible-lines (nle::chat-model-editor model)))
             (spans (nle::composer-visible-lane-spans model))
             (painted (loop for line in lines for row in spans
                            nconc (loop for (from to state) in row
                                        collect (list (subseq line from to) state)))))
        (is (< 1 (count-if (lambda (line) (plusp (length line))) lines)) "the text wraps")
        (is (null (first spans)) "the first row holds no token")
        (is (equal '(("#right" :recognised)) painted) "the span stands on the row its token wrapped to; x#left stays text")))))
