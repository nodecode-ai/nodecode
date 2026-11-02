;;; test-subset.lisp --- the core suite's tests whose names contain any of
;;; the comma-separated NEEDLES (case-insensitive), in a scratch image:
;;;   cd src && NEEDLES=capture,checkpoint sbcl --dynamic-space-size 4096 \
;;;     --control-stack-size 8MB --noinform --non-interactive \
;;;     --load tools/check/test-subset.lisp
;;; The narrowest core lane when a change has an owner with a name; `just
;;; test' stays the gate. Exit 1 on any failure, like RUN-ALL.
(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :nodecode/test)
(defparameter *needles*
  (uiop:split-string (or (uiop:getenv "NEEDLES") "capture") :separator ","))
(let* ((entries (remove-if-not
                 (lambda (entry &aux (name (string-downcase (symbol-name (car entry)))))
                   (some (lambda (needle) (search (string-downcase needle) name)) *needles*))
                 nodecode.test::*tests*))
       (failed (nodecode.test::run-tests entries)))
  (format t "~&~%subset: ~d test~:p, ~d failed~%" (length entries) (length failed))
  (finish-output)
  (uiop:quit (if failed 1 0)))
