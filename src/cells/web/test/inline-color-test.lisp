;;;; inline-color-test.lisp --- safe foreground colors match the TUI contract.
(in-package #:nodecode.test)

(deftest web-cell-inline-colors-follow-shared-goldens-and-csp ()
  (let* ((runner (asdf:system-relative-pathname "nodecode" "test/tui/inline-color-web.mjs"))
         (fixture (asdf:system-relative-pathname "nodecode" "src/tui/inline-colors.json"))
         (module (merge-pathnames "md.js" nodecode-web::*page-directory*))
         (output (uiop:run-program (list "node" (namestring runner) (namestring module) (namestring fixture)) :output :string :error-output :output)))
    (is (search (format nil "WEB-COLOR-GOLDENS ~d passed" (length (gethash "cases" (nlk:decode-json (uiop:read-file-string fixture))))) output) "web and TUI read one machine-readable color contract")
    (is (search "span.style.color = span.dataset.color" (web-page-text "app.js")) "validated data-color paints via the style property")
    (is (search ".prose .inline-color :is(strong, em, a, code) { color: inherit; }" (web-page-text "app.css")) "nested marks keep the selected foreground, not their default accent")
    (is (search "--thought-strong: #4D6174;" (web-page-text "app.css")) "root reasoning tone matches the TUI shadow")
    (is (search ".prose.thought strong { color: var(--thought-strong); }" (web-page-text "app.css")) "thought strong uses the same effective shadowed Ice blue as the TUI")
    (is (search "style-src 'self'" (uiop:read-file-string (asdf:system-relative-pathname "nodecode-web" "cell.lisp"))) "no unsafe-inline permission is added")))
