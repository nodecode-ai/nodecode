;;;; disclosure-anchor-test.lisp --- clicks retain context; scroll pins the owning output title.
(in-package :nodecode.test)

(defun disclosure-anchor-state (&key (kind :tool-call) (prompt t) (prefix 0))
  (let ((state (when prompt (nle::ingest-message-fact nil "user" "Anchor request" :turn-id "anchor"))))
    (when (plusp prefix)
      (setf state (nle::ingest-message-fact state "assistant" (numbered-body "before" prefix) :turn-id "anchor")))
    (ecase kind
      (:tool-call
       (setf state (nle::ingest-tool-call-fact state "anchor-tool" "probe" "{}" :turn-id "anchor")
             state (nle::ingest-tool-result-fact state "anchor-tool" "probe" (numbered-body "output" 120) :turn-id "anchor")))
      (:system-notice
       (setf state (nle::ingest-system-notice state (numbered-body "receipt" 120) :turn-id "anchor"
                       :metadata (nlk:json-object "summary" (nlk:json-object "name" "Mutate" "comment" "Anchor receipt"))))))
    state))

(defun open-live-run (model kind)
  "Standard folds a live turn's work into a run, which stands folded until it
is opened to its calls and receipts: open it. Answers the key a closed member
falls back to, NIL where nothing folds."
  (when (and (member kind '(:tool-call :system-notice)) (eq :standard (nle::chat-model-tool-detail model)))
    (let ((run (if (eq kind :tool-call) '(:process "anchor-tool") '(:process "anchor"))))
      (rendering model (setf (nle::chat-model-expanded-tool-call model) run))
      ;; Bring the listed member into view.
      (nle::scroll-transcript model :end)
      run)))

(deftest disclosure-expansion-retains-clicked-title-screen-row ()
  (dolist (mode '(:standard :developer))
    (dolist (kind '(:tool-call :system-notice))
      (dolist (prompt '(t nil))
        (with-frame-model (model 90 30 :session-id "disclosure-anchor" :tool-detail mode
                           :state (disclosure-anchor-state :kind kind :prompt prompt :prefix (if prompt 4 0)))
          (let* ((run (open-live-run model kind))
                 (needle (if (eq kind :tool-call) "probe" "Anchor receipt"))
                 (y (frame-row-carrying model needle))
                 (owner (nle::transcript-item-at-line model (nle::frame-transcript-position model 6 y))))
            (is owner)
            (click-at model 6 y)
            (is (= y (frame-row-carrying model needle)) "long output opens BELOW the clicked title at its original screen row")
            (is (not (nle::chat-model-follow-bottom model)) "expansion is reading intent, not tail-follow")
            (is (eq owner (nle::transcript-item-at-line model (nle::frame-transcript-position model 6 y))) "the same painted cell still targets the same disclosure")
            (when prompt
              (is (rows-carry-p (if (eq kind :tool-call) "output 0" "receipt 0") (remove-if-not #'stringp (band-row-texts model))) "available canvas below the cursor shows the start of output"))
            (is (not (rows-carry-p (if (eq kind :tool-call) "output 119" "receipt 119") (nle::frame-plain-rows model))) "expansion never replaces the viewport with the tail")
            (click-at model 6 y)
            (is (equal run (nle::chat-model-expanded-tool-call model)) "a closed call falls back to its run")
            (is (= y (frame-row-carrying model needle)) "collapsing at the same cursor retains the title too")))))))

(deftest expanded-tool-title-pins-with-pointer-copy-and-boundary-parity ()
  ;; Developer pins the open call's own title over its output. Standard nests
  ;; the call in its live run, and an open run pins its own header over all it
  ;; holds (REFS-DISCLOSURE-SPANS).
  (dolist (mode '(:standard :developer))
    (with-frame-model (model 90 30 :session-id "tool-title-pin" :tool-detail mode
                       :state (disclosure-anchor-state))
      (let* ((title (if (open-live-run model :tool-call) "Ran 1 command" "probe"))
             (window (progn (click-at model 6 (frame-row-carrying model "probe")) (nle::chat-model-band model)))
             (header (row-index-of title (nle::chat-model-line-plain model) :required t))
             (owner (nle::transcript-item-at-line model header)))
        (dolist (offset '(12 40 80))
          (setf (nle::band-window-offset window) offset)
          (let ((y (frame-row-carrying model title)))
            (is y "the owning title remains visible inside its output")
            (is (= 1 (count-if (lambda (row) (and (stringp row) (search title row))) (band-row-texts model))) "no duplicate inline title")
            (is (eq owner (nle::transcript-item-at-line model (nle::frame-transcript-position model 6 y))))
            (setf (nle::chat-model-selection model) (cons (cons (1- y) 0) (cons (1- y) 80)))
            (is (search title (nle::frame-selection-text model)))
            (setf (nle::chat-model-selection model) nil))
          (is (<= (length (nle::band-window-visible window)) (nle::band-window-height window))))
        (click-at model 6 (frame-row-carrying model title))
        (is (null (nle::chat-model-expanded-tool-call model)) "the pinned title is a real collapse control")
        (is (not (rows-carry-p "output 80" (coerce (nle::chat-model-line-plain model) 'list))))))))

(deftest background-output-expansion-retains-click-and-pins-command ()
  (dolist (mode '(:standard :developer))
    (with-frame-model (model 90 30 :session-id "background-anchor" :tool-detail mode)
      (setf (nle::chat-model-state model)
            (nle::ingest-message-fact nil "user" "Background anchor" :turn-id "background"))
      (let ((row (band-row :command "anchor-running-command" :shell-id 93
                           :head (format nil "exec-start~%")
                           :tail (numbered-body "exec-output" 120) :seen 121)))
        (setf (nle::state-background-rows (nle::chat-model-state model)) (list row))
        (nle::render-state model)
        (let ((y (frame-row-carrying model "anchor-running-command")))
          (click-at model 12 y)
          (is (= y (frame-row-carrying model "anchor-running-command")))
          (is (not (nle::chat-model-follow-bottom model)))
          (dolist (offset '(15 40 80))
            (setf (nle::band-window-offset (nle::chat-model-band model)) offset)
            (let ((pin-y (frame-row-carrying model "anchor-running-command")))
              (is (eq row (nle::band-exec-row-at model (nle::frame-transcript-position model 12 pin-y))))
              (is (= 1 (count-if (lambda (text) (and (stringp text) (search "anchor-running-command" text))) (band-row-texts model))))))
          (click-at model 12 (frame-row-carrying model "anchor-running-command"))
          (is (null (nle::chat-model-expanded-tool-call model))))))))

(deftest tool-title-pin-ends-at-output-boundary-and-keeps-final-rows-reachable ()
  (dolist (mode '(:standard :developer))
    (with-frame-model (model 90 30 :session-id "anchor-boundary" :tool-detail mode
                       :state (disclosure-anchor-state))
      (open-live-run model :tool-call)
      (click-at model 6 (frame-row-carrying model "probe"))
      (setf (nle::chat-model-state model)
            (nle::ingest-message-fact (nle::chat-model-state model) "assistant" (numbered-body "after-output" 35) :turn-id "anchor"))
      (nle::render-state model)
      (let ((span (first (nle::band-disclosure-spans (nle::band-window-lines model.band)))))
        (is-present span "the open output stands as a disclosure span"
          (is (loop for offset from (first span) below (second span)
                    thereis (progn
                              (setf (nle::band-window-offset model.band) offset)
                              (rows-carry-p "output 119" (remove-if-not #'stringp (band-row-texts model))))) "final output remains reachable")
          (setf (nle::band-window-offset model.band) (second span))
          (is (not (rows-carry-p "probe" (remove-if-not #'stringp (band-row-texts model)))) "the title belongs only to its own output")
          (nle::scroll-transcript model :end)
          (is (rows-carry-p "after-output 34" (remove-if-not #'stringp (band-row-texts model)))))))))
