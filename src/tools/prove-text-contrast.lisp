;;;; prove-text-contrast.lisp --- reproducible composed-text evidence, not a UI.
;;;; Run from src/ after bootstrapping dependencies; see contrast-proof.md.
;;;; sbcl --dynamic-space-size 4096 --load PATH/quicklisp/setup.lisp --script tools/prove-text-contrast.lisp [output-prefix]
;;;; Loads tests for their terminal-cell decoder; never starts a shell or provider.
(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "nodecode/test")

(in-package #:nodecode.test)

(defun contrast-proof-rgb-text (rgb)
  "Serialize RGB as comma-separated decimal channels."
  (format nil "~{~d~^,~}" rgb))

(defun contrast-proof-cell (paint)
  "Decode the first cell the terminal parser sees in PAINT."
  (nlt:screen-buffer-ref (nlt:parse-styled-string (list paint) 1 1) 0 0))

(defun contrast-proof-time (label thunk count)
  "Measure COUNT renderer calls, after a full GC, including allocation."
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time))
        (bytes (sb-ext:get-bytes-consed)))
    (dotimes (i count) (funcall thunk i))
    (let ((seconds (/ (- (get-internal-real-time) start)
                      (float internal-time-units-per-second 1d0))))
      (format t "~&BENCH ~a calls=~d seconds=~,6f us/call=~,3f bytes/call=~,1f~%"
              label count seconds (* 1d6 (/ seconds count))
              (/ (- (sb-ext:get-bytes-consed) bytes) (float count 1d0))))))

(defun contrast-proof-run (prefix)
  "Export the full rendered-cell matrix and compare baseline, warm and cold work."
  (let ((original (symbol-function 'nle::readable-text-style))
        (cases '()))
    (dolist (name (nle:theme-catalog :user nil))
      (let ((palette (theme-palette name)))
        (dolist (profile '(:truecolor :ansi256))
          (dolist (ground '(nil :title-band :slab :composer :user-block))
            (dolist (tone '(:foreground :muted :thought-strong :code-quiet :inline-code :link))
              (dolist (shadow '(nil t))
                (push (list name palette profile ground tone shadow) cases)))))))
    (setf cases (coerce (nreverse cases) 'vector))
    (labels ((paint (case)
               (destructuring-bind (name palette profile ground tone shadow) case
                 (declare (ignore name))
                 (let ((nle::*palette* palette)
                       (nle::*color-profile* profile)
                       ;; Transparent Tungstene uses a stated dark terminal canvas.
                       (nle::*terminal-canvas* '(28 31 38)))
                   (nle::seg-render
                    (nle::make-seg :text "Readable prose / muted detail / code and links" :tone tone)
                    (and ground (nle::visual-style-bg (nle:style-for-tone ground)))
                    :force-bg t :shadow shadow))))
             (call-at (i) (paint (aref cases (mod i (length cases))))))
      (unwind-protect
           (progn
             (with-open-file (out (concatenate 'string prefix "-cells.tsv")
                                  :direction :output :if-exists :supersede)
               (format out "theme~cprofile~cground~ctone~cshadow~cbefore_fg~cafter_fg~cbg~cbefore_ratio~cafter_ratio~%"
                       #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab)
               (loop for case across cases
                     do (setf (symbol-function 'nle::readable-text-style) #'identity)
                        (let* ((before (contrast-proof-cell (paint case)))
                               (before-fg (painted-cell-rgb (nlt:cell-fg before))))
                          (setf (symbol-function 'nle::readable-text-style) original)
                          (let* ((after (contrast-proof-cell (paint case)))
                                 (after-fg (painted-cell-rgb (nlt:cell-fg after)))
                                 (bg (or (painted-cell-rgb (nlt:cell-bg after))
                                         (let ((nle::*palette* (second case)))
                                           (or (nle::canvas-rgb) '(28 31 38))))))
                            (format out "~a~c~a~c~a~c~a~c~a~c~a~c~a~c~a~c~,6f~c~,6f~%"
                                    (first case) #\Tab (third case) #\Tab (fourth case) #\Tab
                                    (fifth case) #\Tab (sixth case) #\Tab
                                    (contrast-proof-rgb-text before-fg) #\Tab
                                    (contrast-proof-rgb-text after-fg) #\Tab
                                    (contrast-proof-rgb-text bg) #\Tab
                                    (nle::contrast-ratio before-fg bg) #\Tab
                                    (nle::contrast-ratio after-fg bg))))))
             ;; Repeated complete workloads, not just the correction helper.
             (dotimes (round 3)
               (format t "~&ROUND ~d~%" (1+ round))
               (setf (symbol-function 'nle::readable-text-style) #'identity)
               (dotimes (i 960) (call-at i))
               (contrast-proof-time "baseline/matrix" #'call-at 960000)
               (setf (symbol-function 'nle::readable-text-style) original)
               (clrhash nle::*text-contrast-cache*)
               (dotimes (i 960) (call-at i))
               (contrast-proof-time "corrected/warm-matrix" #'call-at 960000))
             ;; Deliberately pessimistic: no cache reuse, including cache-clear cost.
             (contrast-proof-time "corrected/cold-matrix"
                                  (lambda (i) (clrhash nle::*text-contrast-cache*) (call-at i)) 48000)
             ;; Arbitrary inline colors exceed the cache cap, exercising churn.
             (let ((nle::*palette* (nle::default-palette))
                   (nle::*color-profile* :ansi256))
               (contrast-proof-time
                "corrected/unique-inline-256"
                (lambda (i)
                  (nle::seg-render
                   (nle::make-seg :text "Arbitrary inline color"
                                 :tone (nle::vstyle :fg (list (ldb (byte 8 0) i)
                                                            (ldb (byte 8 8) i)
                                                            (ldb (byte 8 16) i))))
                   '(61 65 74)))
                48000))
             (format t "~&MATRIX ~d cases; output ~a-cells.tsv~%" (length cases) prefix))
        (setf (symbol-function 'nle::readable-text-style) original)))))

(contrast-proof-run (or (first (uiop:command-line-arguments)) "contrast-proof"))
