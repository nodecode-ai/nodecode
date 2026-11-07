;;;; wrappers.lisp --- one Lisp function per remote tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (mcp:files/read-file :path "/etc/hosts") is the shape the model
;;;; writes: a real function in the MCP package, named SERVER/TOOL with both
;;;; halves folded to kebab-case (read_file and readFile both become
;;;; read-file; a fold collision keeps the later tool's raw lowercase name),
;;;; compiled with a real &key lambda list from the tool's inputSchema so
;;;; DESCRIBE shows the arguments, its description as the docstring, and
;;;; exported. The body is one APPLY of CALL, the chokepoint, which is also
;;;; the escape hatch for a name that folded away.
;;;;
;;;; Generated at connect time and after every tools/list, outside the eval
;;;; seam: the scribe records nothing, EVOLVED sees no collision, and a
;;;; tool that vanishes from a fresh list is FMAKUNBOUND. Symbols stay
;;;; exported once seen, because a layer file the model keeps may read
;;;; mcp:files/read-file at boot before any server has connected — the
;;;; primer still tells it to prefer (mcp:call ...) inside definitions it
;;;; keeps, since the function itself exists only while the server does.

(in-package #:nodecode-mcp)

(defun tool-function-name (server-name tool-name)
  "The symbol name for SERVER-NAME's TOOL-NAME, collision-free or not."
  (string-upcase (format nil "~a/~a" (kebab server-name) (kebab tool-name))))

(defun unique-function-name (server-name tool-name taken)
  "TOOL-FUNCTION-NAME unless TAKEN holds it, then the raw lowercase tool
name (non-symbol characters as dashes), then a numbered suffix."
  (let ((candidate (tool-function-name server-name tool-name)))
    (when (gethash candidate taken)
      (setf candidate
            (string-upcase
             (format nil "~a/~a" (kebab server-name)
                     (substitute-if-not #\- (lambda (char) (or (alphanumericp char) (find char "-_")))
                                        (string-downcase tool-name)))))
      (setf candidate (nlk:unused-name candidate (lambda (name) (gethash name taken)))))
    (setf (gethash candidate taken) t)
    candidate))

(defun define-tool-function (server-name tool symbol)
  "Compile and install SYMBOL as the function calling TOOL on SERVER-NAME."
  (let* ((schema (getf tool :schema))
         (required (schema-required schema))
         (properties (schema-properties schema))
         (pairs (remove-duplicates
                 (loop for name in properties
                       for variable = (intern (string-upcase (kebab name)) :nodecode-mcp)
                       ;; A property named t or nil gets no parameter.
                       when (and (not (constantp variable))
                                 (not (eq (symbol-package variable) (find-package :cl)))
                                 (not (member variable '(timeout limit))))
                         collect (cons (intern (symbol-name variable) :keyword) variable))
                 :key #'cdr :from-end t))
         (rest (gensym "ARGS"))
         (lambda-form
           `(lambda (&rest ,rest &key ,@(mapcar (lambda (pair) (list (list (car pair) (cdr pair)))) pairs)
                                        timeout limit &allow-other-keys)
              (declare (ignore ,@(mapcar #'cdr pairs) timeout limit))
              (apply #'call ,server-name ,(getf tool :name) ,rest))))
    (setf (fdefinition symbol) (compile nil lambda-form))
    (setf (documentation symbol 'function)
          (with-output-to-string (out)
            (let ((description (string-trim '(#\Space #\Newline) (or (getf tool :description) ""))))
              (when (plusp (length description))
                (format out "~a~%~%" description)))
            (format out "(mcp:~(~a~)~@[ ~a~])~%" symbol
                    (let ((text (argument-text tool))) (and (plusp (length text)) text)))
            (flet ((listing (label names)
                     (when names
                       (format out "~a: ~{~a~^, ~}~%" label
                               (loop for name in names
                                     for type = (nlk:json-value schema :string "properties" name "type")
                                     collect (format nil "~a~@[ (~a)~]" name type)))))
                   (required-p (name)
                     (member name required :test #'string=)))
              (listing "required" (remove-if-not #'required-p properties))
              (listing "optional" (remove-if #'required-p properties)))
            (format out "MCP server ~a, tool ~a. (mcp:schema ~s ~s) prints the full schema."
                    server-name (getf tool :name) server-name (getf tool :name))))
    (export symbol :nodecode-mcp)
    symbol))

(defun sync-tool-functions (server &aux (server-name (server-name server))
                                        (taken (make-hash-table :test #'equal))
                                        (symbols '())
                                        (tools '()))
  "Bring the generated functions in line with SERVER's catalog: define one
per tool (noting its symbol on the tool plist), forget the ones whose
tools are gone."
  (dolist (tool server.tools)
    (let ((symbol (intern (unique-function-name server-name (getf tool :name) taken)
                          :nodecode-mcp)))
      (nlk:with-handlers ((error (condition)
                            (warn "mcp ~a: cannot define ~a: ~a" server-name symbol condition)))
        (define-tool-function server-name tool symbol)
        (push symbol symbols))
      (push (list* :symbol symbol (alexandria:remove-from-plist tool :symbol)) tools)))
  (dolist (old server.symbols)
    (unless (member old symbols)
      (fmakunbound old)))
  (bt2:with-lock-held ((server-state-lock server))
    (setf server.tools (nreverse tools)
          server.symbols (nreverse symbols)))
  server.symbols)

(defun undefine-tool-functions (server)
  "Forget every function generated for SERVER; the symbols stay exported."
  (dolist (symbol server.symbols)
    (when (fboundp symbol) (fmakunbound symbol)))
  (bt2:with-lock-held ((server-state-lock server))
    (setf server.symbols '()))
  t)
