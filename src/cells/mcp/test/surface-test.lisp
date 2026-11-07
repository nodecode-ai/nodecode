;;;; surface-test.lisp --- argument folding, value mapping, result rendering.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun mcp-schema-of (&rest property-names &aux (properties (nlk:make-json-object)))
  (dolist (name property-names)
    (setf (gethash name properties) (nlk:json-object "type" "string")))
  (nlk:json-object "type" "object" "properties" properties))

(deftest mcp-cell-surface-resolves-keys ()
  (let ((schema (mcp-schema-of "per_page" "includeSnapshot" "plain")))
    (is (equal "per_page" (mcp::resolve-key :per-page schema)) "kebab folds to snake")
    (is (equal "includeSnapshot" (mcp::resolve-key :include-snapshot schema)))
    (is (equal "plain" (mcp::resolve-key :plain schema)) "a plain name matches itself")
    (is (equal "foo_bar" (mcp::resolve-key :foo-bar schema)))
    (is (equal "Exact-Name" (mcp::resolve-key "Exact-Name" schema)) "a string passes verbatim")
    (is (equal "perPage" (mcp::resolve-key :per-page (mcp-schema-of "perPage"))))
    (is (signals-error mcp:mcp-error
          (mcp::resolve-key :perpage (mcp-schema-of "per_page" "perPage"))))
    (is (equal "per_page" (mcp::resolve-key :per_page (mcp-schema-of "per_page" "perPage"))))))

(deftest mcp-cell-surface-wire-values ()
  (multiple-value-bind (arguments timeout limit)
      (mcp::build-arguments (mcp-schema-of "a")
                            (list :a "s" :n 3 :yes t :no :false :none nil
                                  :list '(1 "two" :three) :vec #(1)
                                  :nested '(:inner-key 1 :deep (:x t))
                                  :timeout 5 :limit 100))
    (is-shape arguments ("a" "s" "strings ride") ("n" = 3 "numbers ride") ("yes" eq t "t is true")
      ("no" eq :false ":false is false"))
    (is (not (nth-value 1 (gethash "none" arguments))) "nil is absent")
    (is (equalp #(1 "two" "three") (gethash "list" arguments)) "a list is an array, keywords lowercase")
    (is (equalp #(1) (gethash "vec" arguments)) "a vector is an array")
    (let ((nested (gethash "nested" arguments)))
      (is (hash-table-p nested) "a keyword plist is an object")
      (is (= 1 (gethash "inner_key" nested)) "with snake_case keys")
      (is (eq t (gethash "x" (gethash "deep" nested))) "recursively"))
    (is (= 5 timeout) ":timeout is consumed")
    (is (= 100 limit) ":limit is consumed")
    (is (not (nth-value 1 (gethash "timeout" arguments))) "and never sent")
    (let ((text (nlk:encode-json-object arguments)))
      (is (search "\"no\":false" text) "false encodes as false")
      (is (search "\"yes\":true" text) "true encodes as true")))
  (is (= mcp::*max-timeout-seconds*
         (nth-value 1 (mcp::build-arguments nil (list :timeout 100000)))))
  (is (signals-error mcp:mcp-error (mcp::build-arguments nil (list :a)))))

(deftest mcp-cell-surface-renders-every-block-kind ()
  (let ((result (cell-json "{\"content\":[
     {\"type\":\"text\",\"text\":\"hello\"},
     {\"type\":\"image\",\"data\":\"AAAABBBB\",\"mimeType\":\"image/png\"},
     {\"type\":\"audio\",\"data\":\"CC\",\"mimeType\":\"audio/wav\"},
     {\"type\":\"resource_link\",\"uri\":\"file:///x\"},
     {\"type\":\"resource\",\"resource\":{\"uri\":\"file:///y\",\"text\":\"y body\"}},
     {\"type\":\"resource\",\"resource\":{\"uri\":\"file:///z\",\"mimeType\":\"application/octet-stream\",\"blob\":\"ZZZZ\"}},
     {\"type\":\"mystery\"}]}")))
    (is (equal (format nil "hello~%[mcp image content: image/png, 8 base64 bytes]~%[mcp audio content: audio/wav, 2 base64 bytes]~%[mcp resource link: file:///x]~%[mcp resource file:///y]~%y body~%[mcp resource file:///z: application/octet-stream, 4 base64 bytes]~%[mcp mystery content]")
               (mcp::render-result result))))
  (is (search "\"answer\": 42"
              (mcp::render-result (cell-json "{\"content\":[],\"structuredContent\":{\"answer\":42}}"))))
  (is (search "\"other\"" (mcp::render-result (cell-json "{\"other\":1}"))))
  (is (mcp::result-error-p (cell-json "{\"isError\":true}")) "isError is read")
  (is (mcp::result-error-p (cell-json "{\"is_error\":true}")) "in both spellings")
  (is (not (mcp::result-error-p (cell-json "{\"isError\":false}"))) "false is not an error"))

(deftest mcp-cell-surface-clip-discloses ()
  (let ((text (mcp::clip (make-string 100 :initial-element #\x) 40)))
    (is (= 40 (position #\Newline text)) "cut at the limit")
    (is (search "truncated 60 chars; raise :limit" text) "the cut is disclosed"))
  (is (equal "short" (mcp::clip "short" 40)) "under the limit is untouched"))

(deftest mcp-cell-surface-argument-text-and-tool-line ()
  (let ((tool (list :name "read_file"
                    :description (format nil "Read   a~%file  from disk")
                    :schema (cell-json "{\"properties\":{\"path\":{\"type\":\"string\"},\"limit\":{\"type\":\"integer\"}},\"required\":[\"path\"]}"))))
    (is (equal ":path* :limit" (mcp::argument-text tool)) "required arguments are starred")
    (is (equal "(mcp:files/read-file :path* :limit) - Read a file from disk"
               (mcp::tool-line "files" tool)))))
