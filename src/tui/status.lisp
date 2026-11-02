;;;; status.lisp --- right now and needs attention, over existing owners.
;;;; SPDX-License-Identifier: MIT
;;;; Runtime, health, notices and activity are live-only snapshots. Usage is
;;;; persisted-history, read separately; no diagnostic probes or polling here.
(in-package #:nodecode.evolved)

(defun status-error (value)
  "A partial read's error, whether local or received over HTTP."
  (or (off-loop-failure value) (usage-field value "error")))

(defun status-json (&optional session-id)
  "The serving organism's cheap overview; each section can fail independently."
  (macrolet ((read-section (form)
               `(handler-case ,form
                  (error (condition) (json-object "error" (plain-error-text condition))))))
    (json-object
     "as_of" (- (get-universal-time) +unix-epoch+)
     "identity" (read-section (gateway-identity-json))
     "mode" (read-section (string-downcase (serving-mode (nlk:gateway-state))))
     "health" (read-section (health-facts-json (health-facts)))
     "notices" (read-section (usage-field (standing-notices-payload) "notices"))
     "activity" (read-section (session-activity-payload
                               (bt2:with-lock-held (*session-activity-lock*)
                                 (held-session-activity))))
     :opt "target" (and session-id (read-section (gateway-target-json :session-id session-id))))))

(define-gateway-route handle-gateway-overview-http (env)
  "GET /api/gateway/overview — passive runtime facts, not /doctor or analytics."
  (:route :get "/api/gateway/overview" "status")
  (status-json (cdr (assoc "session_id" (gateway-query-parameters env) :test #'string=))))

(defun status-lines (snapshot usage &key usage-at (now (- (get-universal-time) +unix-epoch+)))
  "One semantic overview for the panel and headless text; absent is never zero."
  (let* ((rows '())
         (identity (usage-field snapshot "identity"))
         (health (usage-field snapshot "health"))
         (notices (usage-field snapshot "notices"))
         (activity (usage-field snapshot "activity"))
         (stamp (usage-field snapshot "as_of"))
         (stream (usage-field snapshot "stream")))
    (labels ((row (label detail &key (tone :muted) action)
               (push (list :label (one-line label) :detail (one-line detail :cap 200 :ellipsis "…")
                           :tone tone :action action) rows))
             (section (label detail &key action)
               (when rows (row "" ""))
               (row label detail :tone :accent :action action))
             (unavailable (label value)
               (row label (format nil "unavailable~@[ — ~a~]" (status-error value)) :tone :warn))
             (problem-p (word) (member word '("warn" "warning" "danger" "error") :test #'equal))
             (tone (word) (cond ((member word '("danger" "error") :test #'equal) :danger)
                               ((problem-p word) :warn) (t :muted)))
             (fact-action (fact)
               (list :command (cond ((member (usage-field fact "key") '("cell" "cells") :test #'equal) "cells")
                                    ((equal (usage-field fact "key") "model") "models")
                                    (t "doctor"))))
             (fact-row (fact)
               (row (usage-field fact "label")
                    (format nil "~a~@[ — ~a~]"
                            (one-line (usage-field fact "detail") :cap 120 :ellipsis "…")
                            (and (usage-field fact "remedy")
                                 (one-line (usage-field fact "remedy") :cap 70 :ellipsis "…")))
                    :tone (tone (usage-field fact "tone")) :action (fact-action fact)))
             (today (label detail)
               (row label detail :action '(:command "usage" "today"))))
      (section "Needs attention" "known runtime facts · no diagnostic probes")
      (cond
        ((null snapshot) (row "Runtime" "Reading the gateway…"))
        ((status-error snapshot) (unavailable "Gateway" snapshot))
        (t
         (if (vectorp health)
             (loop for fact across health when (problem-p (usage-field fact "tone")) do (fact-row fact))
             (unavailable "Health" health))
         (if (vectorp notices)
             (loop for notice across notices
                   when (problem-p (usage-field notice "level"))
                     do (row (usage-field notice "key") (usage-field notice "text")
                             :tone (tone (usage-field notice "level")) :action '(:command "cells")))
             (unavailable "Standing notices" notices))
         ;; Only the section's heading so far: no problem fact or notice, and
         ;; neither read unavailable.
         (when (and (or (null stream) (equal stream "connected")) (null (cdr rows)))
           (row "No reported warnings" "in the reported health facts and standing notices"))))
      (when (and stream (not (equal stream "connected")))
        (row "Live updates unavailable"
             (format nil "~a · snapshot sections may be stale" stream) :tone :warn))
      (section "Runtime" (if (realp stamp) (format nil "snapshot ~a" (iso-time stamp)) "snapshot unavailable"))
      (cond
        ((not (hash-table-p identity)) (unavailable "Identity" snapshot))
        ((status-error identity) (unavailable "Identity" identity))
        (t
         (row "Version" (or (usage-field identity "version") "unavailable"))
         (when-let (build (usage-field identity "build"))
           (unless (equal build (usage-field identity "version")) (row "Binary build" build)))
         (row "Profile" (or (usage-field identity "profile") "unavailable"))
         (let ((start (usage-field identity "started_at")) (mode (usage-field snapshot "mode")))
           (row "Serving" (format nil "~a · uptime ~a"
                                  (if (stringp mode) mode "unavailable")
                                  (if (realp start) (duration-text (max 0 (- now start))) "unavailable"))))))
      (loop for fact across (json-array snapshot "health")
            when (equal (usage-field fact "key") "model")
              do (row "Default model" (usage-field fact "detail")
                      :tone (tone (usage-field fact "tone")) :action '(:command "models"))
            when (equal (usage-field fact "key") "sessions")
              do (row "Saved sessions" (usage-field fact "detail") :action '(:command "sessions")))
      (let* ((target (usage-field snapshot "target"))
             (pin (usage-field target "pin")) (default (usage-field target "default")))
        (when (and (hash-table-p pin)
                   (or (not (equal (usage-field target "model") (usage-field default "model")))
                       (not (equal (usage-field target "provider") (usage-field default "provider")))))
          (row "This session's model" (format nil "~a on ~a" (usage-field target "model")
                                              (usage-field target "provider"))
               :action '(:command "models"))))
      (section "Connections & cells" "standing board · cell-owned status")
      (loop for fact across (json-array snapshot "health")
            unless (member (usage-field fact "key") '("model" "sessions" "gateway" "requests" "cell") :test #'equal)
              do (fact-row fact))
      (loop for notice across (json-array snapshot "notices")
            unless (problem-p (usage-field notice "level"))
              do (row (usage-field notice "key") (usage-field notice "text") :action '(:command "cells")))
      (section "Work now" (format nil "live set · elapsed shown ~a" (iso-time now)) :action '(:command "sessions"))
      (let ((sessions (usage-field activity "sessions")))
        (cond
          ((or (null activity) (status-error activity)) (unavailable "Activity" (or activity snapshot)))
          ((not (vectorp sessions)) (unavailable "Activity" activity))
          (t
           (row "Active sessions" (princ-to-string (length sessions)) :action '(:command "sessions"))
           (loop for session across sessions
                 for started = (ignore-errors (iso-8601-ms (usage-field session "started_at")))
                 do (row (usage-field session "id")
                         (if (realp started) (format nil "running ~a" (duration-text (max 0 (- now (/ started 1000)))))
                             "elapsed unavailable")
                         :action (list :session (usage-field session "id")))))))
      (section "Today · this store" (format nil "API-equivalent estimates~@[ · read ~a~]"
                                            (and usage-at (iso-time usage-at)))
           :action '(:command "usage" "today"))
      (cond
        ((null usage) (row "Usage" "Adding it up…"))
        ((status-error usage) (unavailable "Usage" usage))
        (t
         (let* ((totals (usage-field usage "totals")) (cost (usage-field totals "cost_usd"))
                (unpriced (let ((n (usage-field totals "unpriced"))) (and (realp n) (plusp n) n))))
           (today "Estimated spend"
                  (format nil "~a~@[ · ~d unpriced calls~]"
                          (if (realp cost)
                              (format nil "~:[~;at least ~]~a" unpriced (usd-text cost))
                              "unavailable")
                          unpriced))
           (today "Calls / turns / failed"
                  (format nil "~{~a~^ / ~}"
                          (loop for key in '("calls" "turns" "failed_turns")
                                collect (or (usage-field totals key) "unavailable"))))
           (today "Cache / TTFT p50 / p90"
                  (format nil "~a / ~a / ~a" (share-text (usage-field totals "cached_share"))
                          (ms-text (usage-field usage "wait_p50_ms")) (ms-text (usage-field usage "wait_p90_ms")))))))
      (row "Refresh snapshots" "Enter · activity and notices update from the runtime stream" :action :refresh)
      (nreverse rows))))

(defun status-report-text (snapshot usage &key usage-at (now (- (get-universal-time) +unix-epoch+)))
  "The same overview without a terminal or a session."
  (with-output-to-string (out)
    (write-line "Nodecode status" out)
    (dolist (row (status-lines snapshot usage :usage-at usage-at :now now))
      (cond ((eq :refresh (getf row :action)))
            ((equal "" (getf row :label)) (terpri out))
            (t (format out "~a~a  ~a~%"
                       (if (member (getf row :tone) '(:warn :danger)) "! " "")
                       (getf row :label) (getf row :detail)))))))

(defun headless-status (args session-id)
  "Read-only /status on the serving organism, including today's usage."
  (or (slash-no-args-guard "status" args)
      (let ((snapshot (status-json session-id))
            (usage (handler-case (usage-json "today")
                     (error (condition) (list :error (plain-error-text condition))))))
        (status-report-text snapshot usage :usage-at (- (get-universal-time) +unix-epoch+)))))

;;; The dialog's retained snapshots, not another runtime authority. Never used
;;; after close; keys are weak. Stream overlays keep replies older than a frame
;;; from overwriting it. Existing model activity remains the one live map.
(defvar *status-panels* (make-hash-table :test #'eq :weakness :key))

(defun status-panel-rows (state)
  "Format one dialog snapshot with the shared semantic rows."
  (mapcar (lambda (row &aux (tone (getf row :tone)))
            (make-picker-row :label (getf row :label) :detail (getf row :detail)
                             :value (getf row :action) :heading (null (getf row :action))
                             :mark (if (member tone '(:warn :danger)) "!" "")
                             :mark-tone tone))
          (status-lines (getf state :snapshot) (getf state :usage) :usage-at (getf state :usage-at))))

(defun refresh-status-dialog (model &key board activity &aux (dialog model.dialog))
  "Repaint an open status panel from a pushed fact; no I/O or idle timer."
  (when (and (picker-dialog-p dialog) (eq :status (picker-dialog-kind dialog)))
    (let* ((state (gethash dialog *status-panels*))
           (snapshot (getf state :snapshot)))
      (when activity (setf (getf state :activity-seen) t))
      (when board
        (setf (getf state :board-seen) t (getf state :board) (usage-field board "notices")))
      (when (hash-table-p snapshot)
        (setf (gethash "stream" snapshot) (string-downcase (gateway-model-sync-lifecycle model)))
        (when (getf state :activity-seen)
          (setf (gethash "activity" snapshot)
                (session-activity-payload
                 (loop for id being the hash-keys of (session-activity-table model)
                         using (hash-value started) collect (cons id started)))))
        (when (getf state :board-seen)
          (setf (gethash "notices" snapshot) (getf state :board))))
      (let* ((selected (nth (picker-dialog-selection dialog) (picker-visible-rows dialog)))
             (key (and selected (picker-row-label selected)))
             (rows (status-panel-rows state)))
        (setf (picker-dialog-rows dialog) rows
              (picker-dialog-selection dialog)
              (or (let ((index (and key (position key rows :key #'picker-row-label :test #'equal))))
                    (and index (picker-row-value (nth index rows)) index))
                  (position-if #'picker-row-value rows) 0)))
      t)))

(defun install-status-read (model dialog section value
                            &aux (state (gethash dialog *status-panels*)))
  "A read belongs only to the exact panel that dispatched it."
  (cond ((not (eq model.dialog dialog)) :quiet)
        (t (setf (getf state section) value)
           (when (eq section :usage)
             (setf (getf state :usage-at) (- (get-universal-time) +unix-epoch+)))
           (refresh-status-dialog model)
           nil)))

(defun status-dialog-pick (model value)
  "Enter drills in or explicitly refreshes snapshots; it never mutates state."
  (case (and (consp value) (first value))
    (:command (execute-slash-command model (second value) (or (third value) "")))
    (:session (switch-gateway-session model (second value)))
    ;; :REFRESH asks for this explicitly.
    ;; Headers are not a command. Keep the overview rather than dismissing it.
    (t (funcall 'slash-status-op model))))

(defun slash-status-op (model)
  "Open the overview immediately, then read health and today's usage off-loop."
  (let ((state (list :snapshot nil :usage nil :usage-at nil :activity-seen nil
                     :board nil :board-seen nil)))
    (open-picker-dialog model "Status" (status-panel-rows state) #'status-dialog-pick
                        :kind :status :context "serving organism" :option-marker t
                        :hint "enter details / refresh  esc close  ↑↓ navigate")
    (let ((dialog model.dialog)
          (path (format nil "/api/gateway/overview~@[?session_id=~a~]" model.session-id)))
      (setf (gethash dialog *status-panels*) state)
      (off-loop model (lambda () (gateway-model-http-call model :get path nil))
                (lambda (model value)
                  (cond ((eq :quiet (install-status-read model dialog :snapshot value)) :quiet)
                        (t (off-loop model
                                     (lambda () (gateway-model-http-call model :get "/api/gateway/usage?range=today" nil))
                                     (lambda (model value) (install-status-read model dialog :usage value)))
                           nil))))))
  nil)
