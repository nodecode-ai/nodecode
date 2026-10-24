;;;; chrome-contrast-test.lisp --- actual chrome surfaces, including overlays.
(in-package #:nodecode.test)

(defun chrome-text-contrast-failures (buffer)
  "Unreadable alphanumeric cells as (X Y CHAR RATIO), and known-text count."
  (let ((checked 0) (bad nil))
    (dotimes (y (nlt:screen-buffer-height buffer))
      (dotimes (x (nlt:screen-buffer-width buffer))
        (let* ((cell (nlt:screen-buffer-ref buffer x y))
               (fg (painted-cell-rgb (nlt:cell-fg cell)))
               (bg (or (painted-cell-rgb (nlt:cell-bg cell)) (nle::canvas-rgb))))
          (when (and (alphanumericp (nlt:cell-char cell)) fg bg)
            (incf checked)
            (let ((ratio (nle::contrast-ratio fg bg)))
              (when (< ratio 4.5d0)
                (push (list x y (nlt:cell-char cell) ratio) bad)))))))
    (values (nreverse bad) checked)))

(defmacro with-chrome-contrast-themes (&body body)
  "Exercise opaque bundled surfaces under both extended-color profiles."
  `(dolist (name (nle:theme-catalog :user nil))
     (let ((nle::*palette*
             (theme-palette name :user
               (format nil "{\"themes\":[{\"name\":~s,\"transparent-background\":false}]}" name))))
       (dolist (nle::*color-profile* '(:truecolor :ansi256)) ,@body))))

(defun assert-chrome-text-readable (rows width label)
  "Parse ROWS with the production parser and assert known text contrast."
  (multiple-value-bind (bad checked)
      (chrome-text-contrast-failures (nlt:parse-styled-string rows width (length rows)))
    (is (plusp checked) (format nil "~a contains known-color text" label))
    (is (null bad) (format nil "~a ~a: ~a ~s" label nle::*color-profile*
                           (length bad) (subseq bad 0 (min 6 (length bad)))))))

(deftest chrome-contrast-header-footer-and-composer ()
  (with-chrome-contrast-themes
    (with-frame-model (model 100 24 :session-id "chrome-contrast" :tool-detail :developer)
      (with-pinned-window ("chrome-model" 16000)
        (fold-usage model :input 13000 :output 100 :model "chrome-model")
        (let ((bg (nle::canvas-fill-bg)))
          (assert-chrome-text-readable
            (list (nle::bilateral-frame-row "Session title" :session-title
                                           (nle::header-status-text model) 100 bg
                                           nil (nle::header-status-segs model))
                  (nle::footer-frame-row model 100 bg))
            100 "header/footer")
          (assert-chrome-text-readable (nle::composer-rows model 100 24 bg) 100 "placeholder")
          (type-string model "Typed draft")
          (assert-chrome-text-readable (nle::composer-rows model 100 24 bg) 100 "typed")
          (mark-turn-in-flight model)
          (assert-chrome-text-readable (nle::composer-rows model 100 24 bg) 100 "steering"))))))

(deftest chrome-contrast-dialog-selection-and-notices ()
  (with-chrome-contrast-themes
    (with-frame-model (model 100 30 :session-id "chrome-dialog")
      (nle::open-picker-dialog model "Picker title"
        (list (nle::make-picker-row :label "Selected row" :detail "Selected detail")
              (nle::make-picker-row :label "Ordinary row" :detail "Ordinary detail"))
        (lambda (&rest args) (declare (ignore args)))
        :subtitle "Picker subtitle" :searchable t)
      (let* ((dialog (nle::chat-model-dialog model))
             (surface (nle::visual-style-bg (nle:style-for-tone :section))))
        (assert-chrome-text-readable (nle::docked-dialog-header-rows dialog 100 surface 4)
                                    100 "dialog header/search")
        (assert-chrome-text-readable (nle::docked-dialog-list-rows dialog 100 surface 2)
                                    100 "dialog selected/unselected"))
      (dolist (error-p '(nil t))
        (let ((toast (nle::make-toast "Notice text" :error-p error-p)))
          (dolist (hovered '(nil t))
            (assert-chrome-text-readable (nle::toast-rows toast 40 :hovered hovered)
                                        40 "toast normal/error/hover")))))))

(deftest chrome-contrast-after-transcript-selection-wash ()
  ;; SEG-RENDER's check is before this background-mutating overlay. A text
  ;; color at 4.5 on the canvas need not pass on the blended selection.
  (with-chrome-contrast-themes
    (with-frame-model (model 80 24 :session-id "chrome-selection")
      (setf (nle::chat-model-selection model) '((3 . 2) . (3 . 30)))
      (dolist (tone '(:muted :code-quiet :foreground :link :danger))
        (let* ((rows (list "" "" ""
                           (concatenate 'string "  "
                             (nle::render-row nil (list (nle::make-seg :text "Selected text" :tone tone))
                                              (nle::canvas-fill-bg) 40))))
               (buffer (washed-frame model rows))
               (bad (chrome-text-contrast-failures buffer)))
          (is (null bad) (format nil "selection ~a ~a: ~s" tone nle::*color-profile* bad)))))))

(deftest chrome-contrast-selection-preserves-content-and-nontext ()
  (dolist (nle::*color-profile* '(:truecolor :ansi256 :ansi :none))
    (with-frame-model (model 80 24 :session-id "chrome-boundary")
      (setf (nle::chat-model-selection model) '((3 . 2) . (3 . 30)))
      (let* ((text (nle::styled "A.日─" (nle::vstyle :fg '(100 100 100) :bg '(100 100 100)
                                                  :bold t :italic t :underline t :strike t :dim t)))
             (rows (list "" "" "" (concatenate 'string "  " +osc8-open+ text +osc8-close+
                                                  (format nil "~c[31mB~c[0m" #\Escape #\Escape))))
             (before (nlt:parse-styled-string rows 80 24))
             (after (washed-frame model rows)))
        (dolist (column '(2 3 4 5 6 7))
          (let ((old (nlt:screen-buffer-ref before column 3))
                (new (nlt:screen-buffer-ref after column 3)))
            (is (char= (nlt:cell-char old) (nlt:cell-char new)) "selection keeps glyphs and wide continuations")
            (is (equal (nlt:cell-link old) (nlt:cell-link new)) "selection keeps OSC8 links")
            (is (= (logandc2 (nlt:cell-attrs old) nlt::+attr-faint+)
                   (logandc2 (nlt:cell-attrs new) nlt::+attr-faint+)) "non-dim attributes survive")
            (cond ((or (member nle::*color-profile* '(:ansi :none))
                       (member column '(5 6 7)))
                   (is (equal (nlt:cell-fg old) (nlt:cell-fg new)) "basic/default ink and line decoration stay unchanged"))
                  (t (is (not (logtest nlt::+attr-faint+ (nlt:cell-attrs new))) "selected text uses explicit readable ink")
                     (is (>= (nle::contrast-ratio (painted-cell-rgb (nlt:cell-fg new))
                                                 (painted-cell-rgb (nlt:cell-bg new))) 4.5d0) "selected text includes punctuation and wide glyphs")))))))))

(deftest chrome-contrast-selection-leaves-reverse-video-ink ()
  (let ((nle::*color-profile* :truecolor))
    (with-frame-model (model 80 24 :session-id "chrome-reverse")
      (setf (nle::chat-model-selection model) '((3 . 2) . (3 . 3)))
      (let* ((rows (list "" "" "" (format nil "  ~c[38;2;100;100;100;48;2;100;100;100;7mR~c[0m"
                                        #\Escape #\Escape)))
             (before (nlt:parse-styled-string rows 80 24))
             (after (washed-frame model rows))
             (old (nlt:screen-buffer-ref before 2 3))
             (new (nlt:screen-buffer-ref after 2 3)))
        (is (logtest nlt::+attr-reverse+ (nlt:cell-attrs old)) "fixture is reverse-video")
        (is (equal (nlt:cell-fg old) (nlt:cell-fg new)) "reverse ink is not guessed")
        (is (= (nlt:cell-attrs old) (nlt:cell-attrs new)) "reverse attributes stay intact")))))
