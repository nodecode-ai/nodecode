;;;; package.lisp --- the BOARD package: two verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BOARD so an eval form reads (board:post ...).
;;;; Every exported verb answers a STRING and every refusal is BOARD-ERROR,
;;;; rendered by the eval snippet as "ERROR: BOARD-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `board' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :hops).

(defpackage #:nodecode-board
  (:use #:cl)
  (:nicknames #:board)
  (:export
   ;; verbs, every one a string
   #:join #:post
   ;; data, so a layer can reword it
   #:*framing* #:*wake-input*
   ;; the one condition
   #:board-error))

(in-package #:nodecode-board)

(nlk:define-peripheral board :not-running t :lock "board")
