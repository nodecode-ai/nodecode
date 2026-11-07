;;;; format.lisp --- pure text formatters for what the extension returns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ports of pi-chrome's formatChromeSnapshot / formatChromeInspect /
;;;; summarizeActionResult (index.ts), section by section, with the same line
;;;; grammar — `- <uid> <role|tag>[ [flags]] <label>[ in <ctx>] @ <x>,<y> <w>x<h>`
;;;; — because uids and rects are what click and the x,y fallback consume.
;;;; Only the budgets are tighter: pi-chrome may spend 30000 chars on a tool
;;;; result, EVAL clamps at 8000, so every formatter clips at a LIMIT
;;;; (6000 default) and says so, pointing at the zoom knobs.
;;;;
;;;; Every field is read through NLK:JSON-VALUE: absent, null, false and the
;;;; wrong type all read NIL, so a snapshot from a newer or older extension
;;;; formats with its unknown keys ignored rather than signalling. Anything
;;;; that is not an object at all falls back to clipped JSON.

(in-package #:nodecode-chrome)

(defparameter *default-limit* 6000
  "Default character cap on a formatted result, under EVAL's 8000.")

(defun clip (text &optional (limit *default-limit*))
  "TEXT cut at LIMIT with the cut disclosed and the zoom knobs named."
  (nlk:clip text limit :disclose "zoom with :mode/:query/:near-uid or raise :limit"))

(defun compact-line (value &optional (max 120))
  "VALUE as one line: runs of whitespace collapsed, trimmed, cut at MAX."
  (nlk:one-line value :cap max))

(defun rect-text (rect)
  (if (hash-table-p rect)
      (format nil "~a,~a ~ax~a" (jv rect :number "x") (jv rect :number "y")
              (jv rect :number "width") (jv rect :number "height"))
      "?"))

(defun jv (object type &rest path)
  (apply #'nlk:json-value object type path))

(defun jarray (object &rest path)
  "The JSON array at PATH as a list, or NIL."
  (coerce (apply #'nlk:json-array object path) 'list))

(defun take (list n)
  (subseq list 0 (min n (length list))))

(defun label-of (el &rest keys)
  "First non-empty string among KEYS of EL."
  (loop for key in keys
        for value = (jv el :text key)
        when value return value))

(defun flag-text (el &aux (flags '()))
  (when (jv el :boolean "disabled") (push "disabled" flags))
  (nlk:when-let (occluded (jv el :object "occluded"))
    (push (format nil "occluded-by-~a" (or (jv occluded :text "tag") "?")) flags))
  (and flags (format nil " [~{~a~^,~}]" (nreverse flags))))

(defun element-line (el &aux (context (jv el :object "context")))
  (format nil "- ~a ~a~@[~a~] ~a~@[ in ~a~] @ ~a"
          (jv el :text "uid")
          (or (label-of el "role" "tag") "element")
          (flag-text el)
          (compact-line (label-of el "label" "selector"))
          (and context (jv context :text "label")
               (format nil "~a ~a" (jv context :text "uid")
                       (compact-line (jv context :text "label") 60)))
          (rect-text (jv el :object "rect"))))

(defmacro with-report ((value limit) &body body)
  "BODY's LINE, SECTION and LISTING calls as one text clipped at LIMIT; a
VALUE that is not an object reads as its clipped JSON."
  `(if (hash-table-p ,value)
       (let ((lines '()))
         (labels ((line (fmt &rest args) (push (apply #'format nil fmt args) lines))
                  (section (title) (push (format nil "~%## ~a" title) lines))
                  (listing (title items cap row)
                    (when items
                      (section title)
                      (dolist (item (take items cap)) (funcall row item)))))
           ,@body
           (clip (format nil "~{~a~^~%~}" (nreverse lines)) ,limit)))
       (clip (nlk:pretty-json ,value) ,limit)))

(defun format-snapshot (snapshot &key (limit *default-limit*))
  "pi-chrome's formatChromeSnapshot with tighter budgets; :full is JSON."
  (with-report (snapshot limit)
    (let ((mode (or (jv snapshot :text "mode") "")))
      (when (string= mode "full")
        (return-from format-snapshot (clip (nlk:pretty-json snapshot) (max limit 7000))))
      (line "# Chrome snapshot~@[ (~a)~]" (and (plusp (length mode)) mode))
      (line "~a" (or (jv snapshot :text "title") "(untitled)"))
      (when (jv snapshot :text "url") (line "~a" (jv snapshot :text "url")))
      (nlk:when-let (viewport (jv snapshot :object "viewport"))
        (line "viewport=~ax~a scroll=~a,~a"
              (jv viewport :number "width") (jv viewport :number "height")
              (or (jv viewport :number "scrollX") 0)
              (or (jv viewport :number "scrollY") 0)))
      (nlk:when-let (modal (jv snapshot :object "summary" "modal"))
        (line "modal: ~a ~a" (jv modal :text "uid") (compact-line (jv modal :text "label"))))
      (nlk:when-let (focused (jv snapshot :object "summary" "focused"))
        (line "focused: ~a ~a ~a" (jv focused :text "uid")
              (or (jv focused :text "role") "") (compact-line (jv focused :text "label"))))
      (listing "Hints" (jarray snapshot "summary" "hints") 6
               (lambda (hint) (line "- ~a" (compact-line hint 200))))
      ;; Changes since the previous snapshot of this page.
      (let ((diff (jv snapshot :object "diff")))
        (when (and diff (not (jv diff :boolean "firstSnapshot")))
          (listing "Changed since last snapshot"
                   (append
                    (mapcar (lambda (c)
                              (if (equal (jv c :text "kind") "textChanged")
                                  "text changed"
                                  (format nil "~a: ~a -> ~a" (jv c :text "kind")
                                          (compact-line (jv c :any "before") 50)
                                          (compact-line (jv c :any "after") 50))))
                            (jarray diff "changes"))
                    (mapcar (lambda (e)
                              (format nil "added ~a ~a ~a" (jv e :text "uid")
                                      (or (jv e :text "role") "")
                                      (compact-line (jv e :text "label"))))
                            (take (jarray diff "added") 4))
                    (mapcar (lambda (u)
                              (format nil "updated ~a ~a" (jv u :text "uid")
                                      (compact-line (or (jv u :text "after" "label")
                                                        (jv u :text "before" "label")))))
                            (take (jarray diff "updated") 4)))
                   10 (lambda (item) (line "- ~a" item)))))
      ;; Query matches.
      (listing (format nil "Matches for ~s" (or (jv snapshot :text "query") ""))
               (jarray snapshot "matches") 12
               (lambda (m &aux (kind (jv m :text "kind")))
                 (cond
                   ((equal kind "text")
                    (line "- ~a text ~a @ ~a" (jv m :text "uid")
                          (compact-line (jv m :text "text")) (rect-text (jv m :object "rect"))))
                   ((equal kind "region")
                    (line "- ~a region ~a headings=~{~a~^ | ~}" (jv m :text "uid")
                          (compact-line (jv m :text "label"))
                          (mapcar (lambda (h) (compact-line h 50)) (jarray m "headings"))))
                   (t
                    (line "- ~a ~a~:[~; disabled~] ~a @ ~a" (jv m :text "uid")
                          (or (label-of m "role" "tag") "element")
                          (jv m :boolean "disabled")
                          (compact-line (label-of m "label" "selector"))
                          (rect-text (jv m :object "rect")))))))
      ;; Page map.
      (let ((page-map (jv snapshot :object "pageMap")))
        (when (and (string= mode "pageMap") page-map)
          (section "Page map")
          (dolist (region (take (jarray page-map "regions") 18))
            (line "- ~a ~a: ~a" (jv region :text "uid") (jv region :text "kind")
                  (compact-line (jv region :text "label")))
            (dolist (action (take (jarray region "actions") 5))
              (line "  - ~a ~a~:[~; disabled~] ~a" (jv action :text "uid")
                    (or (jv action :text "role") "") (jv action :boolean "disabled")
                    (compact-line (jv action :text "label")))))
          (nlk:when-let (headings (jarray page-map "headings"))
            (line "~%Headings:")
            (dolist (h (take headings 20))
              (line "- ~a h~a ~a" (jv h :text "uid") (or (jv h :number "level") "")
                    (compact-line (jv h :text "text")))))))
      ;; Layout / context.
      (let ((layout (jarray snapshot "layout")))
        (when (and layout (not (string= mode "changes")))
          (section "Layout / context")
          (dolist (sec (take layout (if (string= mode "pageMap") 18 6)))
            (line "- ~a~@[ ~a~] ~a @ ~a" (jv sec :text "uid") (label-of sec "role" "tag")
                  (compact-line (or (label-of sec "label" "text") "(unnamed section)") 110)
                  (rect-text (jv sec :object "rect")))
            (let ((fields (mapcar (lambda (f) (format nil "~a ~a" (jv f :text "uid")
                                                      (compact-line (label-of f "label" "role") 40)))
                                  (take (jarray sec "fields") 4)))
                  (actions (mapcar (lambda (a) (format nil "~a~:[~; disabled~] ~a" (jv a :text "uid")
                                                       (jv a :boolean "disabled")
                                                       (compact-line (label-of a "label" "role") 40)))
                                   (take (jarray sec "actions") 5))))
              (when fields (line "  fields: ~{~a~^; ~}" fields))
              (when actions (line "  actions: ~{~a~^; ~}" actions))))))
      ;; Forms.
      (let ((fields (jarray snapshot "forms" "fields"))
            (submits (jarray snapshot "forms" "submits")))
        (when (and (or (string= mode "forms") fields)
                   (not (string= mode "pageMap"))
                   (or fields submits))
          (section "Forms")
          (dolist (field (take fields (if (string= mode "forms") 40 10)))
            (line "- ~a ~a~:[~; required~]~:[~; invalid~]~:[~; disabled~] ~a~@[ value=~a~]~:[~; value=[redacted]~] @ ~a"
                  (jv field :text "uid") (or (label-of field "role" "tag") "field")
                  (jv field :boolean "required") (jv field :boolean "invalid")
                  (jv field :boolean "disabled")
                  (compact-line (label-of field "label" "selector") 90)
                  (and (jv field :text "value") (compact-line (jv field :text "value") 50))
                  (and (null (jv field :text "value")) (jv field :boolean "valueRedacted"))
                  (rect-text (jv field :object "rect"))))
          (dolist (submit (take submits 8))
            (line "- ~a submit/action~:[~; disabled~] ~a @ ~a" (jv submit :text "uid")
                  (jv submit :boolean "disabled")
                  (compact-line (label-of submit "label" "selector"))
                  (rect-text (jv submit :object "rect"))))))
      ;; Visible actions.
      (let ((elements (and (not (string= mode "pageMap")) (jarray snapshot "elements")))
            (cap (if (string= mode "interactive") 60 25)))
        (listing "Visible actions" elements cap (lambda (el) (line "~a" (element-line el))))
        (when (> (length elements) cap)
          (line "- ... ~d more; retry with :max-elements or :mode :interactive"
                (- (length elements) cap))))
      ;; Text snippets.
      (let ((snippets (and (or (string= mode "text") (string= mode "auto"))
                           (jarray snapshot "textSnippets"))))
        (listing "Text snippets" snippets (if (string= mode "text") 40 12)
                 (lambda (snip)
                   (line "- ~a ~a" (jv snip :text "uid")
                         (compact-line (jv snip :text "text") (if (string= mode "text") 240 140)))))
        (when (and snippets (jv snapshot :boolean "textTruncated"))
          (line "- ... page text truncated; retry with :mode :text or :max-text-chars")))
      (line "~%Tip: (chrome:snapshot :query \"...\" :mode :interactive|:forms|:page-map|:text|:changes|:full) or :near-uid to zoom in."))))

(defun format-inspect (inspect &key (limit *default-limit*))
  "pi-chrome's formatChromeInspect."
  (with-report (inspect limit)
    (let ((target (or (jv inspect :object "target") (nlk:make-json-object))))
      (line "# Chrome inspect~@[ ~a~]" (jv target :text "uid"))
      (line "~a~@[~a~] ~a" (or (label-of target "role" "tag") "element")
            (flag-text target) (compact-line (label-of target "label" "selector")))
      (when (jv target :text "selector") (line "selector: ~a" (jv target :text "selector")))
      (when (jv target :object "rect") (line "rect: ~a" (rect-text (jv target :object "rect"))))
      (nlk:when-let (suggestion (jv inspect :object "clickSuggestion"))
        (line "suggested click: (chrome:click ~s) or :x ~a :y ~a"
              (jv suggestion :text "uid") (jv suggestion :number "x")
              (jv suggestion :number "y")))
      (listing "Nearby text" (jarray inspect "nearbyText") 12
               (lambda (item)
                 (line "- ~a ~a" (jv item :text "uid") (compact-line (jv item :text "text") 180))))
      (nlk:when-let (form (jv inspect :object "formContext"))
        (section "Form context")
        (dolist (field (take (jarray form "fields") 20))
          (line "- ~a ~a~:[~; disabled~] ~a~@[ value=~a~]~:[~; value=[redacted]~]"
                (jv field :text "uid") (or (label-of field "role" "tag") "field")
                (jv field :boolean "disabled")
                (compact-line (label-of field "label" "selector"))
                (and (jv field :text "value") (compact-line (jv field :text "value") 60))
                (and (null (jv field :text "value")) (jv field :boolean "valueRedacted"))))
        (dolist (action (take (jarray form "actions") 10))
          (line "- ~a action~:[~; disabled~] ~a" (jv action :text "uid")
                (jv action :boolean "disabled")
                (compact-line (label-of action "label" "selector")))))
      (listing "Nearby actions" (jarray inspect "nearbyActions") 18
               (lambda (a) (line "~a" (element-line a))))
      (listing "Ancestors" (jarray inspect "ancestors") 6
               (lambda (a)
                 (line "- ~a ~a ~a" (jv a :text "uid") (or (label-of a "role" "tag") "element")
                       (compact-line (label-of a "label" "selector") 120)))))))

(defun format-tabs (tabs)
  "One line per tab: id, active marker, group, title, url."
  (if (and (vectorp tabs) (not (stringp tabs)))
      (if (zerop (length tabs))
          "(no tabs)"
          (format nil "~{~a~^~%~}"
                  (loop for tab across tabs
                        collect (format nil "- ~a~:[~;*~]~@[ [~a]~] ~a  ~a"
                                        (jv tab :any "id")
                                        (jv tab :boolean "active")
                                        (jv tab :text "group" "title")
                                        (compact-line (or (jv tab :text "title") "(untitled)") 80)
                                        (compact-line (or (jv tab :text "url") "") 120)))))
      (clip (nlk:pretty-json tabs))))

(defun summarize-action (result)
  "pi-chrome's summarizeActionResult: the soft hints an action's result
carries, or NIL. pageMutated=false is a coarse heuristic, never a verdict."
  (when (hash-table-p result)
    (flet ((explicit-false-p (key)
             (multiple-value-bind (value present) (gethash key result)
               (and present (null value)))))
      (let ((parts '()))
        ;; A hidden page, or a failed trusted path, gets page-level events: some sites ignore
        ;; those, so the model hears which it got.
        (when (member (jv result :text "input") '("dom" "dom-fallback") :test #'equal)
          (push (format nil "page-level events, not trusted input~@[ (~a)~]"
                        (nlk:when-let (reason (jv result :text "reason")) (clip reason 160)))
                parts))
        (when (explicit-false-p "pageMutated")
          (push "no coarse DOM change detected (may still have taken effect; verify with :snapshot t)" parts))
        (when (jv result :boolean "defaultPrevented") (push "defaultPrevented=true" parts))
        (when (explicit-false-p "elementVisible") (push "element NOT visible" parts))
        (nlk:when-let (occluded (jv result :object "occludedBy"))
          (push (format nil "occluded by <~a~@[#~a~]>" (or (jv occluded :text "tag") "?")
                        (jv occluded :text "id"))
                parts))
        (when (explicit-false-p "valueMatches") (push "input value did not stick" parts))
        (when (jv result :any "autoplayHint") (push "autoplay-gated affordance" parts))
        (and parts (format nil "~{~a~^; ~}" (nreverse parts)))))))

(defun format-action (verb target raw &key (limit *default-limit*))
  "\"<verb> <target> - <hints>\" plus the included snapshot when RAW is the
{result, snapshot} envelope an :include-snapshot call returns."
  (let* ((snapshot (and (hash-table-p raw) (jv raw :object "snapshot")))
         (head (format nil "~a ~a~@[ - ~a~]" verb target
                       (summarize-action (if snapshot (jv raw :any "result") raw)))))
    (if snapshot
        (clip (format nil "~a~%~%~a" head (format-snapshot snapshot :limit limit)) limit)
        head)))
