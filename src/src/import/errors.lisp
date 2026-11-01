;;;; errors.lisp --- the import's refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every refusal is IMPORT-ERROR, which the eval snippet renders as
;;;; "ERROR: IMPORT-ERROR: <detail>", and every exported verb answers a
;;;; string, which the eval snippet prints as text.

(in-package #:nodecode.import)

(nlk:define-peripheral import)

(nlk:define-error yaml-unreadable (import-error) ()
  ;; The reader recovers per top-level section — damage in one subtree must
  ;; not lose the rest of somebody's config — but recovery is only honest
  ;; where the reader still knows what it is looking at. An anchor, an alias,
  ;; a tag or a directive means values elsewhere in the document may depend on
  ;; a construct this reader does not follow, so reading on would be guessing
  ;; at other people's settings. Those are refused by name, whole, the way
  ;; they always were.
  (:documentation "A YAML refusal that costs the WHOLE document rather than
one of its sections.") (:signal fail-document))
