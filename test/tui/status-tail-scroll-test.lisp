;;;; status-tail-scroll-test.lisp --- pinned chrome must remain scroll-reachable.
;;;; SPDX-License-Identifier: MIT
(in-package #:nodecode.test)

(defun status-tail-window (height count prompt-start prompt-rows)
  "Numbered projected rows plus live status/exec tail at a pin boundary."
  (let* ((window (nle::make-band-window :height height))
         (base (loop for i below (- count 4) collect (format nil "row ~d" i)))
         (tail (list "status" "exec" "exec output" "")))
    (setf (nle::band-prompt-spans base)
          (append (when (plusp prompt-start) (list (cons 0 3)))
                  (list (cons prompt-start (+ prompt-start prompt-rows)))))
    (nle::band-window-set-lines window base tail)
    window))

(defun status-tail-indices (window)
  "The same content indices paint and pointer mapping expose."
  (loop for row below (length (nle::band-window-visible window))
        for index = (nle::band-window-row-index window row)
        when index collect index))

(deftest pinned-scroll-range-includes-the-entire-live-tail (dolist (height '(12 16 31 43)))
  (dolist (prompt-start '(0 5))
    (dolist (prompt-rows '(3 4 9))
      (loop for count from (max (+ prompt-start prompt-rows 4) (- height 3))
                      to (+ height 12)
            for window = (status-tail-window height count prompt-start prompt-rows)
            do (nle::band-window-goto-bottom window)
               (let ((shown (status-tail-indices window)))
                 (is (= (1- count) (car (last shown))) "bottom includes the closing pad, not an unreachable status/exec tail")
                 (is (every (lambda (i) (member i shown))
                            (loop for i from (- count 4) below count collect i)) "status, exec and output are all reachable at bottom"))
               (nle::band-window-goto-top window)
               (is (member (if (zerop prompt-start) prompt-rows 3)
                           (status-tail-indices window)) "Home still reaches the answer lead")
               (let ((seen nil))
                 (loop for offset from 0 to (nle::band-window-max-offset window)
                       do (setf (nle::band-window-offset window) offset)
                          (let ((visible (nle::band-window-visible window)))
                            (is (<= (length visible) height) "the pin never overflows the viewport")
                            (loop for text in visible for row from 0
                                  for i = (nle::band-window-row-index window row)
                                  when i
                                    do (pushnew i seen)
                                       (is (equal text (first (nle::band-window-range window i (1+ i)))) "pointer and paint agree across the projection/chrome join"))))
                 (is (every (lambda (i) (member i seen))
                            (loop for i from (+ prompt-start prompt-rows) below count collect i)) "scrolling never skips response or live-tail rows"))))))

(deftest status-and-execs-survive-every-bottom-scroll-gesture ()
  (dolist (mode '(:standard :developer))
    (dolist (running '(t nil))
      (dolist (gesture '(:end :page-down :wheel :scrollbar))
        (with-frame-model (model 100 50 :session-id "status-tail-scroll" :tool-detail mode
                           :paint ((exchange "status-tail prompt" (numbered-body "response" 12))))
          (setf (nle::state-background-rows (nle::chat-model-state model))
                (list (band-row :command "status-tail-exec" :shell-id 41
                                :tail (format nil "output one~%output two~%") :seen 2)))
          (when running (mark-turn-in-flight model))
          (nle::render-state model)
          (let* ((window (nle::chat-model-band model))
                 (count (nle::band-window-count window)))
            ;; Exactly at the old branch's cliff: raw scroll bottom is the
            ;; prompt end, so its fixed body slice omitted live chrome.
            (nle:update model
              (nlt:make-window-size-msg
                :width 100
                :height (+ count (nle::shell-overhead 50 (nle::composer-visible-rows model))
                           (- (cdr (car (last (nle::band-window-prompt-spans window))))))))
            (press model :home)
            (ecase gesture
              (:end (press model :end))
              (:page-down (dotimes (i 10) (press model :page-down)))
              (:wheel (dotimes (i 20) (mouse-update model #'nlt:make-mouse-wheel-msg 8 8 :direction :down)))
              (:scrollbar (click-at model 100 (+ 3 (nle::band-window-height window)))))
            (let ((indices (status-tail-indices window))
                  (refs (nle::chat-model-line-rows model)))
              (is (= (1- (nle::band-window-count window)) (car (last indices))) "all gestures reach the real composed tail")
              (let* ((index (position-if #'nle::bg-band-row-p refs))
                     (y (frame-row-carrying model "status-tail-exec")))
                (is (< index (nle::band-window-count window)) "a background call is history, listed where it ran")
                (is (eql index (nle::frame-transcript-position model 8 y)) "paint and hit testing share its row"))
              (when running
                (is (search "waiting" (aref (nle::frame-plain-rows model) (+ 2 (nle::band-window-height window)))) "the live bar is the tail's last row, one canvas row over the composer"))
              (is (some (lambda (row) (search "status-tail-exec" row)) (nle::compose-frame model)) "the real frame paints the exec command")
              (is (nle::chat-model-follow-bottom model) "bottom re-pins follow"))
            (press model :home)
            (is (rows-carry-p "response 0" (remove-if-not #'stringp (band-row-texts model))) "Home still reaches the first response after visiting live chrome")))))))


(deftest live-status-rides-the-transcripts-tail (dolist (mode '(:standard :developer)))
  ;; The bar is the transcript's last row, one canvas row over the composer:
  ;; it scrolls with history, the scroll gutter runs through it, and pointer
  ;; and paint map it as the band's own row.
  (dolist (height '(10 24 40))
    (dolist (width '(24 80 130))
      (with-frame-model (model width height :session-id "status-dock" :tool-detail mode)
        (paint-model model
          (exchange "Dock the live bar" (numbered-body "dock-response" 60)))
        (nle::render-state model)
        (let ((idle (nle::band-window-count (nle::chat-model-band model))))
          (mark-turn-in-flight model :render t)
          (let* ((window (nle::chat-model-band model))
                 (count (nle::band-window-count window))
                 (foot (+ 2 (nle::band-window-height window))))
            (is (= (+ idle (length (first (nle::status-blocks model)))) count) "the bar's rows are the transcript's tail")
            (dolist (gesture '(:home :page-up :wheel :end :scrollbar))
              (ecase gesture
                (:home (press model :home))
                (:page-up (press model :page-up))
                (:wheel (mouse-update model #'nlt:make-mouse-wheel-msg 8 5 :direction :up))
                (:end (press model :end))
                (:scrollbar (click-at model width (1+ foot))))
              (let ((rows (mapcar #'nle::plain-frame-row (nle::compose-frame model))))
                (is (= height (length rows)) "the bar never moves the composer")
                (is (zerop (length (string-trim '(#\Space) (nth (1+ foot) rows)))) "one canvas row over the composer")
                (is (every (lambda (row) (= width (nle::text-display-width row))) rows) "every row fits the frame")
                (if (member gesture '(:end :scrollbar))
                    (progn
                      (is (search "waiting" (nth foot rows)) "at the tail the bar is the band's last row")
                      (is (= 1 (count-if (lambda (row) (search "waiting" row)) rows)) "one bar")
                      (is (eql #\▐ (char (nth foot rows) (- width 1))) "the scroll rail runs through it")
                      (let ((index (nle::frame-transcript-position model 4 (1+ foot))))
                        (is (eq :status (aref (nle::chat-model-line-rows model) index)) "pointer and paint share the bar's row")))
                    (is (not (rows-carry-p "waiting" rows)) "scrolled up, the bar is out of view with the tail"))))
            ;; Clock-only paints neither grow nor scroll the transcript.
            (decf (nle::chat-model-turn-started-at model) internal-time-units-per-second)
            (nle::render-status-frame model)
            (is (= count (nle::band-window-count window)) "the status clock never changes transcript geometry")
            (rendering model (setf (nle::chat-model-turn-in-flight model) nil))
            (is (= idle (nle::band-window-count window)) "the bar's rows leave with the turn")))))))
