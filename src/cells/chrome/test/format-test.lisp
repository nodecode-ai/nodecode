;;;; format-test.lisp --- the snapshot/inspect/tabs formatters over a fixture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; fixtures/snapshot.json is shaped exactly as snapshot_injected.js's
;;;; snapshotPage builds it (every key, the diff envelope, redaction flags),
;;;; large enough to cross the display caps so the "N more" and clip paths
;;;; are exercised.

(in-package #:nodecode.test)

(defun chrome-fixture (name)
  (shasht:read-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-chrome"
                                   (format nil "test/fixtures/~a" name)))))

(defun with-mode (snapshot mode &aux (copy (nle::copy-json-value snapshot)))
  (setf (gethash "mode" copy) mode)
  copy)

(deftest chrome-cell-format-snapshot-ports-every-section ()
  (let* ((snapshot (chrome-fixture "snapshot.json"))
         (text (chrome::format-snapshot snapshot)))
    (is (uiop:string-prefix-p "# Chrome snapshot (auto)" text))
    (is (search "viewport=1440x900 scroll=0,480" text))
    (is (search "focused: el-4 textbox Add a comment" text))
    (is (search "## Hints" text))
    (is (search "## Changed since last snapshot" text))
    (is (search "- text changed" text))
    (is (search "- focus:  -> el-4" text) "a NIL before reads as empty")
    (is (search "- added el-7 checkbox Viewed" text))
    (is (search "- updated el-3 Squash and merge" text))
    (is (search "## Matches for \"merge button\"" text))
    (is (search "- el-2 button Merge pull request @ 842,512 168x32" text))
    (is (search "- el-40 region Merge box headings=Merge" text))
    (is (search "- el-52 text 2 approvals @ 1,2 3x4" text))
    (is (search "## Layout / context" text))
    (is (search "- el-41 form Leave a comment @ 100,680 1000x200" text))
    (is (search "actions: el-2 Merge pull request; el-3 disabled Squash and merge" text))
    (is (search "## Forms" text))
    (is (search "- el-8 textbox required invalid Password value=[redacted] @ 120,900 300x32" text))
    (is (search "value=Looks \"good\" to me" text))
    (is (search "- el-5 submit/action Comment @ 760,800 90x32" text))
    (is (search "## Visible actions" text))
    (is (search "- el-2 button Merge pull request in el-40 Merge box @ 842,512 168x32" text))
    (is (search "- el-3 button [disabled] Squash and merge" text))
    (is (search "- el-5 button [occluded-by-div] Comment" text))
    (is (search "- ... 9 more; retry with :max-elements or :mode :interactive" text))
    (is (search "## Text snippets" text))
    (is (search "- el-51 This PR makes the port probe deterministic. It also removes a sleep." text))
    (is (search "- ... page text truncated" text))
    (is (search "Tip: (chrome:snapshot :query" text))
    (is (not (search "## Page map" text)) "page map only in pageMap mode")
    (is (< (length text) 6000))))

(deftest chrome-cell-format-snapshot-modes-select-sections ()
  (let ((snapshot (chrome-fixture "snapshot.json")))
    (let ((interactive (chrome::format-snapshot (with-mode snapshot "interactive"))))
      (is (not (search "## Text snippets" interactive)))
      (is (not (search "more; retry" interactive)) "interactive shows up to 60 elements"))
    (let ((page-map (chrome::format-snapshot (with-mode snapshot "pageMap"))))
      (is (search "## Page map" page-map))
      (is (search "- el-40 region: Merge box" page-map))
      (is (search "  - el-2 button Merge pull request" page-map))
      (is (search "- el-60 h1 Fix flaky bridge test #42" page-map))
      (is (not (search "## Visible actions" page-map))))
    (let ((changes (chrome::format-snapshot (with-mode snapshot "changes"))))
      (is (search "## Changed since last snapshot" changes))
      (is (not (search "## Layout" changes))))
    (let ((full (chrome::format-snapshot (with-mode snapshot "full") :limit 7000)))
      (is (uiop:string-prefix-p "{" full) "full mode is JSON")
      (is (search "\"mode\": \"full\"" full)))
    (let ((first (nle::copy-json-value snapshot)))
      (setf (gethash "diff" first) (nlk:json-object "firstSnapshot" t))
      (is (not (search "## Changed" (chrome::format-snapshot first)))))))

(deftest chrome-cell-format-clips-and-tolerates-shape-drift ()
  (let ((snapshot (chrome-fixture "snapshot.json")))
    (let ((clipped (chrome::format-snapshot snapshot :limit 500)))
      (is (<= (length clipped) 620))
      (is (search "[truncated" clipped))
      (is (search "raise :limit" clipped)))
    ;; Unknown keys are ignored; a missing section is simply absent.
    (let ((odd (nlk:json-object "title" "x" "mode" "auto" "novelKey" 1
                                "elements" (vector (nlk:json-object "uid" "el-9" "future" t)))))
      (is (search "- el-9 element  @ ?" (chrome::format-snapshot odd))))
    (is (equal "null" (chrome::format-snapshot nil)) "a non-object falls back to JSON")
    (is (equal "\"text\"" (chrome::format-snapshot "text")))))

(deftest chrome-cell-format-inspect-tabs-and-action-summary ()
  (let ((inspect (nlk:json-object
                  "target" (nlk:json-object "uid" "el-2" "role" "button" "label" "Merge pull request"
                                            "selector" "button.merge"
                                            "rect" (nlk:json-object "x" 1 "y" 2 "width" 3 "height" 4))
                  "clickSuggestion" (nlk:json-object "uid" "el-2" "x" 2.5 "y" 4)
                  "nearbyText" (vector (nlk:json-object "uid" "el-50" "text" "2 approvals"))
                  "formContext" (nlk:json-object
                                 "fields" (vector (nlk:json-object "uid" "el-4" "role" "textbox"
                                                                   "label" "Comment" "value" "hi"))
                                 "actions" (vector (nlk:json-object "uid" "el-5" "label" "Comment")))
                  "nearbyActions" (vector (nlk:json-object "uid" "el-3" "role" "button"
                                                           "label" "Squash" "disabled" t))
                  "ancestors" (vector (nlk:json-object "uid" "el-40" "tag" "section" "label" "Merge box")))))
    (let ((text (chrome::format-inspect inspect)))
      (is (uiop:string-prefix-p "# Chrome inspect el-2" text))
      (is (search "button Merge pull request" text))
      (is (search "selector: button.merge" text))
      (is (search "suggested click: (chrome:click \"el-2\") or :x 2.5 :y 4" text))
      (is (search "- el-4 textbox Comment value=hi" text))
      (is (search "- el-5 action Comment" text))
      (is (search "- el-3 button [disabled] Squash @ ?" text))
      (is (search "- el-40 section Merge box" text))))
  (let ((tabs (vector (nlk:json-object "id" 7 "active" t "title" "Pull Request #42" "url" "https://x/42"
                                       "group" (nlk:json-object "title" "Nodecode: s-1"))
                      (nlk:json-object "id" 9 "active" :false "title" "" "url" "about:blank"))))
    (let ((text (chrome::format-tabs tabs)))
      (is (search "- 7* [Nodecode: s-1] Pull Request #42  https://x/42" text))
      (is (search "- 9 (untitled)  about:blank" text)))
    (is (equal "(no tabs)" (chrome::format-tabs (vector)))))
  (let ((result (nlk:json-object "input" "chrome" "pageMutated" nil "elementVisible" nil
                                 "occludedBy" (nlk:json-object "tag" "div" "id" "toast")
                                 "valueMatches" nil)))
    (let ((summary (chrome::summarize-action result)))
      (is (search "no coarse DOM change detected" summary))
      (is (search "element NOT visible" summary))
      (is (search "occluded by <div#toast>" summary))
      (is (search "input value did not stick" summary)))
    (is (null (chrome::summarize-action (nlk:json-object "input" "chrome" "x" 1))))
    (is (equal "page-level events, not trusted input (hidden page)"
               (chrome::summarize-action (nlk:json-object "input" "dom" "reason" "hidden page"))))
    (is (equal "Clicked el-2" (chrome::format-action "Clicked" "el-2" (nlk:json-object "input" "chrome"))))
    (let ((with-snapshot (chrome::format-action
                          "Clicked" "el-2"
                          (nlk:json-object "result" result
                                           "snapshot" (nlk:json-object "title" "After" "mode" "auto")))))
      (is (uiop:string-prefix-p "Clicked el-2 - no coarse DOM change" with-snapshot))
      (is (search "# Chrome snapshot (auto)" with-snapshot))
      (is (search "After" with-snapshot)))))
