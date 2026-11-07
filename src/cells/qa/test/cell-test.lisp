;;;; cell-test.lisp --- lifecycle, the tool, the notes, /qa.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the config surface (a
;;;; weekly share starts the sender, never and unanswered run the local half
;;;; only, a veto installs nothing, a wrong share is the loader's loud
;;;; refusal), the tool through the real dispatch seam (a bounded note, never
;;;; a failure), that a note's text leaves only by /qa push and rides
;;;; without its session or time, and that /qa show prints the page
;;;; as it would go.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "qa"
  (:fixture with-stubbed-collector)
  (:config "share" "weekly" "url" "https://collector.test/")
  (:command "qa")
  (:running (is (nle::find-tool "report_issue") "the tool is registered")
            ;; the tool's own description is the whole manual
            (is (null (assoc :qa nle::*help-topics*)))
            (is (equal "weekly" (nodecode-qa::setting :share)))
            (is (equal "https://collector.test" (nodecode-qa::setting :url)))
            (is nodecode-qa::*worker* "the sender runs")
            (is (null *qa-posts*) "and sends nothing on a home with no store"))
  (:stopped (is (null (nle::find-tool "report_issue")) "stop unregisters the tool")
            (is (null nodecode-qa::*worker*) "and the sender is gone")
            (is (null nodecode-qa::*stop*) "and the scope is spent"))
  (:refused ("share" "sometimes")))

(deftest qa-cell-never-and-unanswered-run-the-local-half-only (with-saved-globals (nle:*tools*))
  (with-cell-stop ((qa-start "share" "never"))
    (is (nle::find-tool "report_issue") "the tool is there whatever share says")
    (is (null nodecode-qa::*worker*) "no sender")
    (is (equal "never" (nodecode-qa::setting :share))))
  (with-cell-stop ((qa-start))
    (is (nle::find-tool "report_issue"))
    (is (null (nodecode-qa::setting :share)) "unanswered")
    (is (null nodecode-qa::*worker*))
    (is (equal (nodecode-qa::collector-root) (nodecode-qa::setting :url))))
  (with-cell-stop ((qa-start "enabled" nil))
    (is (null (nle::find-tool "report_issue")) "vetoed: not even the tool"))
  (is (signals-error nlk:config-refusal (qa-start "share" "sometimes")))
  ;; A collector that is not a web address would fail every send, silently: refused at the start.
  (is (search "the collector is a web address"
              (handler-case (progn (qa-start "url" "not a url at all") "")
                (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))))
  (is (null (nle::find-tool "report_issue")) "and the refused start installed nothing")
  (is (null (nle::find-registered-command "qa"))))

(deftest qa-cell-the-collector-sits-beside-the-release-host ()
  (is-table (release expected) (equal expected
                                      (let ((nle::*release-url* release))
                                        (nodecode-qa::collector-root)))
    ("https://preview.nodecode.ai/dl" "https://preview.nodecode.ai")
    ("https://mirror.example/releases/dl/" "https://mirror.example/releases")
    ("https://mirror.example" "https://mirror.example")))

;;; --- the tool ------------------------------------------------------------------

(deftest qa-cell-report-issue-keeps-a-bounded-note-and-never-fails (with-session-store ("s1"))
  (with-qa (:share :never)
    (let ((result (execute-wire-call
                   "call-1" "report_issue"
                   "{\"tool\":\"eval\",\"symptom\":\"timeout\",\"note\":\"(sh \\\"sleep 100\\\") never came back\"}")))
      (is (equal "tool" (gethash "role" result)))
      (is (search "noted" (princ-to-string (nle::message-content result)))))
    (let ((notes (nodecode-qa::read-notes)))
      (is (= 1 (length notes)))
      (is (equal "eval" (gethash "tool" (first notes))))
      (is (equal "timeout" (gethash "symptom" (first notes))))
      (is (search "sleep 100" (gethash "note" (first notes))))
      (is (uiop:string-prefix-p "note-" (gethash "id" (first notes)))))
    ;; A long note is clipped.
    (execute-wire-call "call-2" "report_issue"
                       (format nil "{\"tool\":\"look\",\"symptom\":\"other\",\"note\":~s}"
                               (make-string 400 :initial-element #\x)))
    (let ((newest (first (nodecode-qa::read-notes))))
      (is (equal "look" (gethash "tool" newest)))
      (is (= 280 (length (gethash "note" newest)))))
    ;; And a note the model left out is the declaration's default, not a
    ;; value the body invented.
    (execute-wire-call "call-3" "report_issue" "{\"tool\":\"sh\",\"symptom\":\"refused\"}")
    (is (equal "" (gethash "note" (first (nodecode-qa::read-notes)))))
    ;; Bounded: the oldest past 200 are dropped.
    (dotimes (i 210)
      (nodecode-qa::keep-note "eval" "other" (format nil "note ~d" i)))
    (is (= 200 (length (nodecode-qa::read-notes))))
    (is (equal "note 209" (gethash "note" (first (nodecode-qa::read-notes)))))
    (is (search "200 notes kept on this machine, 200 unpushed" (qa:notes)))))

(deftest qa-cell-report-issue-is-held-to-the-shape-it-advertises (with-session-store ("s1"))
  ;; The declaration this tool was migrated onto closes a gap that shipped:
  ;; the schema said `required: tool, symptom' with an enum on the symptom,
  ;; and the body defaulted the two to "unknown"/"other" and ignored the enum,
  ;; so a malformed report was filed instead of repaired. One declaration is
  ;; both halves now.
  (with-qa (:share :never)
    (let* ((entry (nle::find-tool "report_issue"))
           (contract (second entry)))
      (is (nlk:contract-p contract) "the tool is registered against its declaration")
      (is (equalp (nlk:contract-schema contract)
                  (nlk:json-value (aref (nle::tools-payload (list entry)) 0)
                                  :object "function" "parameters"))))
    (loop for (arguments needle) in '(("{\"symptom\":\"timeout\"}" "tool must be present")
                                      ("{\"tool\":\"eval\"}" "symptom must be present")
                                      ("{\"tool\":\"eval\",\"symptom\":\"meh\"}" "one of timeout")
                                      ("{\"tool\":\"eval\",\"symptom\":\"timeout\",\"note\":7}"
                                       "note must be a non-empty string"))
          do (is-carrying (text (princ-to-string
                                 (nle::message-content
                                  (execute-wire-call "call-bad" "report_issue" arguments))))
               ("contract_refused" arguments)
               (is (search needle text) needle)))
    (is (null (nodecode-qa::read-notes)))))

;;; --- the notes leave by hand ---------------------------------------------------

(deftest qa-cell-notes-leave-only-by-hand-and-without-their-session (with-session-store ("s1"))
  (with-stubbed-collector
    (with-qa (:share :weekly)
      (nodecode-qa::keep-note "eval" "timeout" "the snippet never came back")
      (is (search "only by /qa push" (refusal-text qa:qa-error (qa:push-notes))))
      (is (null *qa-posts*))
      (is (search "pushed 1 note to https://collector.test/api/qa/notes" (qa-slash "push")))
      (is (equal "https://collector.test/api/qa/notes" (car (first *qa-posts*))))
      (let* ((body (qa-post-body))
             (notes (gethash "notes" body)))
        (is (equal "nodecode/notes/1" (gethash "report" body)))
        (is (= 1 (length notes)))
        (is (equal "the snippet never came back" (gethash "note" (aref notes 0))))
        (is (equal "eval" (gethash "tool" (aref notes 0))))
        (is (null (nth-value 1 (gethash "session" (aref notes 0)))) "no session id rides")
        (is (null (nth-value 1 (gethash "at" (aref notes 0)))) "no time rides")
        (is (null (nth-value 1 (gethash "id" (aref notes 0)))) "no id rides"))
      (is (nodecode-qa::pushed-p (first (nodecode-qa::read-notes))) "marked")
      (is (search "1 note kept on this machine, 0 unpushed" (qa:notes)))
      (is (search "no unpushed notes" (qa-slash "push")) "pushed once")
      (is (search "report_issue" (qa-slash "push")) "and says where a note comes from")
      (is (= 1 (length *qa-posts*)))
      ;; The weekly page never carries the text, pushed or not.
      (is (not (search "never came back" (nlk:encode-json-object (page-now)))))
      (is (search "cleared 1 note" (nth-value 0 (nodecode-qa::run-slash "clear" "s1"))))
      (is (null (nodecode-qa::read-notes))))))

;;; --- /qa ----------------------------------------------------------------

(deftest qa-cell-show-prints-the-page-as-it-would-go (with-session-store ("s1"))
  (with-stubbed-collector
    (with-qa (:share :weekly)
      (nodecode-qa::ensure-cursor)
      (record-a-week (nlk:admit-turn "s1" "c1" "hi"))
      (let* ((answer (qa:show))
             (lines (nlk:lines answer))
             (page (nlk:decode-json (second lines))))
        (is (search "as it would go to https://collector.test/api/qa" (first lines)))
        (is (= 2 (length lines)) "the line, then the page")
        (is (equal "nodecode/weekly/1" (gethash "report" page)))
        (is (= 32 (length (gethash "nonce" page))))
        (is (= 2 (length (gethash "rounds" page))))
        (is (search (format nil "~d bytes" (length (second lines))) (first lines))))
      (let ((line (qa-slash "")))
        (is (search "sharing weekly with https://collector.test, next page in " line))
        (is (a-week-away-p line) "the next page is a week away"))
      (is (search "the next page" (qa-slash "show")))
      (is (search "unknown subcommand bogus" (qa-slash "bogus")))
      (is (null *qa-posts*) "show and status send nothing"))))

;;; --- the post --------------------------------------------------------------

(deftest qa-cell-a-refusal-names-what-the-collector-said ()
  ;; The collector's 400 names the first thing wrong in its body; /qa printed
  ;; only `HTTP 400' and left the operator nothing to act on.
  (with-stubbed-fdefinitions
      ((nlk:http (method url &key headers content timeout connect-timeout max-bytes binary stream pool proxy)
         (declare (ignore method url headers content timeout connect-timeout max-bytes binary stream pool proxy))
         (values "{\"error\":\"rounds.0.ttft_ms: Invalid input\"}" 400)))
    (multiple-value-bind (ok reason) (nodecode-qa::post-json "https://collector.test/api/qa" "{}")
      (is (null ok))
      (is (equal "HTTP 400: rounds.0.ttft_ms: Invalid input" reason) "the status and the collector's sentence")))
  (with-stubbed-fdefinitions
      ((nlk:http (method url &key headers content timeout connect-timeout max-bytes binary stream pool proxy)
         (declare (ignore method url headers content timeout connect-timeout max-bytes binary stream pool proxy))
         (values "<html>bad gateway</html>" 502)))
    (is (equal "HTTP 502" (nth-value 1 (nodecode-qa::post-json "https://collector.test/api/qa" "{}"))) "a body with no sentence, the status alone"))
  (with-stubbed-fdefinitions
      ((nlk:http (method url &key headers content timeout connect-timeout max-bytes binary stream pool proxy)
         (declare (ignore method url headers content timeout connect-timeout max-bytes binary stream pool proxy))
         (values nil 204)))
    (is (eq t (nodecode-qa::post-json "https://collector.test/api/qa" "{}")) "a 204 is taken")))
