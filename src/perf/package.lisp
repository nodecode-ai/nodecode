;;;; package.lisp --- packages for the scaling gate (src/perf/).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The gate times a handful of hot paths at sizes a person reaches and fits
;;;; how their cost grows with the size of what they hold. It stands on the
;;;; test system's own fixtures (the shell over a temp store, the seeded
;;;; turn, the wire fold), so it loads after nodecode/test and reuses them
;;;; rather than growing a second set.

(in-package #:cl-user)

(defpackage #:nodecode.perf
  (:documentation
   "Measured constants: the timing core the scaling gate shares.")
  (:use #:cl)
  (:shadow #:time)
  (:export #:scaling-contract #:make-scaling-contract #:scaling-contract-kind
           #:scaling-contract-bound #:scaling-contract-cpu-cost
           #:scaling-contract-bytes-cost #:scaling-contract-unit
           #:scaling-contract-floor #:scaling-contract-known
           #:run-scaling-fn #:seconds-per-call #:gc-flush
           #:point #:make-point #:point-size #:point-cpu #:point-bytes
           #:point-calls #:exponent #:verdict))

(defpackage #:nodecode.perf.scaling
  (:documentation
   "The scaling gate: each hot path's cost measured at n, 4n, 16n and fitted
as a log-log slope against the bound its contract states.")
  (:use #:cl #:nodecode.perf)
  (:import-from #:nodecode.test
                #:deftest #:with-temp-store #:with-frame-model #:make-frame-model
                #:paint-model #:fold-model #:exchange #:press #:type-string
                #:seed-completed-turn #:wire-fold)
  (:export #:run-scaling #:scaling-cases))
