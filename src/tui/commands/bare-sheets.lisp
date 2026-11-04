;;;; bare-sheets.lisp --- what a command with a sheet answers typed alone where none opens.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.evolved)

;;; --- the sheets' commands, without a shell ------------------------------------
;;;
;;; A shell opens /models, /usage, /cells and /profile as sheets of their own,
;;; each reaching every line its command reads; a room and the web page draw
;;; no sheet, so there each answers, typed alone, the dialog its sheet would
;;; be (DEFINE-BARE-DIALOG, bare.lisp): rows that run its lines where it ran,
;;; rows that ask for the one line of text a pick cannot name. A shell's
;;; MODEL makes each answer NIL, and the sheet opens. /import answers its
;;; dialog on a shell too: the walk it opens starts from a plan, and the
;;; choice of what to read comes before one. /context and /generations answer
;;; their dialogs from their headless arms already (context-view.lisp,
;;; generations.lisp).

(defparameter +room-model-picks+ 25
  "The most models one provider's row offers as picks: what a room's menu holds.")

(defparameter +room-frequent-models+ 8
  "The most models /models' Frequently used rows name without a shell.")

(defun room-frequent-models ()
  "The models past operator turns ran most, as (PROVIDER MODEL) pairs, at most
+ROOM-FREQUENT-MODELS+; NIL where the store says none."
  (let ((rows (ignore-errors (frequent-models-json))))
    (loop for row across (or rows #())
          for provider = (json-value row :string "provider")
          for id = (json-value row :string "model")
          when (and provider id) collect (list provider id) into pairs
          finally (return (subseq pairs 0 (min +room-frequent-models+ (length pairs)))))))

(defun room-model-rows (key head pairs in-use)
  "PAIRS, (PROVIDER MODEL) lists, as rows keyed KEY whose Enter runs HEAD with
the pair after it, IN-USE's pair marked."
  (loop for (provider id) in (remove-duplicates pairs :test #'equalp :from-end t)
        for here = (equalp (list provider id) in-use)
        collect (list* :key key
                       (run-row id (format nil "~a ~a ~a" head provider id)
                                :detail (format nil "~:[~;in use · ~]~a" here provider)
                                :mark (if here "●" "") :mark-tone :accent))))

(defun room-provider-rows (key head)
  "One row keyed KEY a provider the catalog lists models for here, its models
the picks it offers, each running HEAD with the provider and the model after
it; the row's own Enter says that provider's ids."
  (let ((providers (ignore-errors (models)))
        (rows (ignore-errors (models-catalog-picker-rows))))
    (loop for (provider . count) in providers
          for ids = (loop for row in rows
                          for value = (picker-row-value row)
                          when (and (consp value) (stringp (first value))
                                    (string-equal (first value) provider))
                            collect (second value))
          when (plusp count)
          collect (list :key key :label provider
                        :detail (format nil "~d model~:p" count)
                        :run (list :enter (format nil "/models list ~a" provider))
                        :options (loop for id in (subseq ids 0 (min +room-model-picks+ (length ids)))
                                       collect (run-row id (format nil "~a ~a ~a" head provider id)))))))

(defun models-room-dialog (session-id)
  "/models typed alone in a room or the web page: Main, the model SESSION-ID
runs on and new sessions start on; Auxiliary, the model naming, recaps and
summaries run on; Decider, the model judgments run on -- the shell's tabs,
each the frequently used models, every provider's own, and a field for any
other."
  (bind (((provider id) (gateway-target session-id))
         ((aux-provider aux-id _ aux-configured) (auxiliary-model))
         (decider (decider-model))
         (frequent (room-frequent-models))
         (main-in (list provider id))
         (aux-in (and aux-configured (list aux-provider aux-id))))
    (list-dialog
     :title "Models"
     :summary (format nil "Main ~a/~a · auxiliary ~:[same as main~;~:*~a~] · decider ~:[none~;~:*~a~]"
                      provider id (and aux-in (format nil "~{~a/~a~}" aux-in)) decider)
     :searchable t :search-placeholder "Search models"
     :tabs (list (list :label "Main" :value "main")
                 (list :label "Auxiliary" :value "aux")
                 (list :label "Decider" :value "decider"))
     :rows (append
            (room-model-rows "main" "/models" (cons main-in frequent) main-in)
            (room-provider-rows "main" "/models")
            (list (list* :key "main"
                         (ask-row "Another model" "Model" "/models"
                                  :detail "any model a provider here serves"
                                  :placeholder "its id, or its provider and id" :verb "switch")))
            (list (list* :key "aux"
                         (run-row "Same as main" "/models aux default"
                                  :detail (format nil "~:[~;in use · ~]each session's own model" (null aux-in))
                                  :mark (if aux-in "" "●") :mark-tone :accent)))
            (room-model-rows "aux" "/models aux" (append (and aux-in (list aux-in)) frequent) aux-in)
            (room-provider-rows "aux" "/models aux")
            (list (list* :key "aux"
                         (ask-row "Another model" "Model" "/models aux"
                                  :detail "any model a provider here serves"
                                  :placeholder "its id, or its provider and id" :verb "switch")))
            (loop for row in (decider-choices)
                  for here = (equalp (getf row :id) decider)
                  collect (list* :key "decider"
                                 (run-row (getf row :name) (format nil "/models decider ~a" (getf row :id))
                                          :detail (format nil "~:[~;in use · ~]~a · ~a" here (getf row :model)
                                                          (decider-address (getf row :base)))
                                          :mark (if here "●" "") :mark-tone :accent)))
            (list (list* :key "decider"
                         (run-row "None" "/models decider none"
                                  :detail (format nil "~:[~;in use · ~]nothing decides" (null decider))
                                  :mark (if decider "" "●") :mark-tone :accent)))))))

(define-bare-dialog "models" (session-id model)
  (unless model (models-room-dialog session-id)))

(define-bare-dialog "usage" (session-id model)
  ;; A shell's /usage is its own sheet, the four ranges its tabs.
  (unless model
    (list-dialog :title "Usage"
                 :summary "What every session spent at catalog prices: calls, the cache, the waits, by day, model, session and folder"
                 :rows (loop for (label word) in '(("Today" "today") ("The last 7 days" "7d")
                                                  ("The last 30 days" "30d") ("All of it" "all"))
                             collect (run-row label (format nil "/usage ~a" word))))))

(defun cell-report-detail (cell)
  "What /cells' report says of CELL, after its name: its kind, its
definitions, whether it started, what failed."
  (let ((text (with-output-to-string (out) (nlk:describe-cells (list cell) out)))
        (lead (format nil "cell ~a: " (nlk:cell-name cell))))
    (format nil "~{~a~^ · ~}"
            (loop for line in (uiop:split-string (string-trim '(#\Newline) text) :separator '(#\Newline))
                  for trimmed = (nlk:trimmed line)
                  when (plusp (length trimmed))
                    collect (if (uiop:string-prefix-p lead trimmed) (subseq trimmed (length lead)) trimmed)))))

(define-bare-dialog "cells" (session-id model)
  ;; A shell's /cells is its sheet. Here the report is the dialog's rows, a
  ;; cell each and the standing notices after them, and the row that makes
  ;; one: a room fills no composer, so it says what to tell the agent.
  (unless model
    (let ((cells (nlk:cells)))
      (list-dialog :title "Cells"
                   :summary (render-cells-summary cells)
                   :searchable t :search-placeholder "Search cells"
                   :empty-label (render-cells nil)
                   :rows (append
                          (loop for cell in cells
                                collect (list :label (nlk:cell-name cell) :detail (cell-report-detail cell)))
                          (loop for (nil text) in (nlk:standing-notices)
                                collect (list :label "notice" :detail text))
                          (list (run-row "+ New cell" "/cells new"
                                         :detail "made by talking: the agent asks what it still needs")))))))

(defun render-cells-summary (cells)
  "How many CELLS there are and how many failed, for the line under /cells'
title."
  (let ((failed (count-if #'nlk:cell-failures cells)))
    (format nil "~d cell~:p~:[~*~; · ~d failed~]" (length cells) (plusp failed) failed)))

;;; --- /profile ----------------------------------------------------------------

(defun profile-git-home-p (record)
  "Whether RECORD's home is a git checkout, a distribution `update' pulls."
  (and (nlk:profile-record-home record)
       (uiop:directory-exists-p (merge-pathnames ".git/" (uiop:ensure-directory-pathname
                                                         (nlk:profile-record-home record))))
       t))

(defun profile-row-facts (record)
  "What a row says of RECORD's home after its name: whether it is this
process's, whether a launch naming none boots it, its description."
  (format nil "~{~a~^ · ~}"
          (remove-if (lambda (fact) (zerop (length (or fact ""))))
                     (list (and (nlk:profile-record-current-p record) "this one")
                           (and (nlk:profile-record-sticky-p record) "boots by default")
                           (nlk:profile-record-description record)))))

(defun profile-record-named (name)
  "The profile record NAME names, or NIL."
  (find name (nlk:profiles) :key #'nlk:profile-record-name :test #'string=))

(defun profile-word-names-home-p (word)
  "Whether WORD, alone after /profile, names a home rather than a verb: the
line a row of bare /profile's dialog runs to open what that home takes."
  (and (not (nlk:find-verb word nlk:*profile-verbs*))
       (profile-record-named word)
       t))

(defun profile-home-dialog (record &aux (name (nlk:profile-record-name record)))
  "What can be done to RECORD's home without a terminal, the dialog its row
in /profile opens: booted by default, exported, cloned, pulled, deleted.
Switching relaunches a terminal, which a room has not: a shell's roster
does it."
  (list-dialog
   :title name :summary (profile-row-facts record)
   :rows (append
          (list (if (nlk:profile-record-sticky-p record)
                    (run-row "Stop booting it by default" "/profile use default"
                             :detail "a launch that names no profile takes the default home")
                    (run-row "Boot it by default" (format nil "/profile use ~a" name)
                             :detail "from the next launch that names no profile"))
                (ask-row "Export its home" "Archive path" (format nil "/profile export ~a" name)
                         :detail "settings and cells, without its store and keys"
                         :placeholder "where the tar.gz goes" :verb "export")
                (ask-row "Export it whole" "Archive path" (format nil "/profile export ~a --whole" name)
                         :detail "its keys and a copy of its store with it"
                         :placeholder "where the tar.gz goes" :verb "export")
                (ask-row "New profile from it" "Name" (format nil "/profile create --from ~a" name)
                         :detail "a clone of this home rather than the one running"
                         :placeholder "a-z, 0-9, - and _" :verb "create"))
          (when (profile-git-home-p record)
            (list (run-row "Pull its distribution" (format nil "/profile update ~a" name)
                           :detail "your edits ride on top")))
          (unless (or (nlk:profile-record-current-p record) (string= name "default"))
            (list (run-row "Delete its whole home" (format nil "/profile delete ~a" name)
                           :detail "store, layer and secrets: asked first"))))))

(defun profile-room-dialog ()
  "/profile typed alone without a terminal: every home, each row opening
what can be done to it (PROFILE-HOME-DIALOG), and the rows that make one."
  (let ((records (nlk:profiles)))
    (list-dialog
     :title "Profiles"
     :summary (format nil "~d home~:p · this one is ~a" (length records) (nlk:profile-name))
     :searchable t :search-placeholder "Search profiles"
     :rows (append
            (loop for record in records
                  for name = (nlk:profile-record-name record)
                  collect (run-row name (format nil "/profile ~a" name)
                                   :detail (profile-row-facts record)
                                   :mark (if (nlk:profile-record-current-p record) "*" " ")
                                   :mark-tone :accent))
            (list (ask-row "+ New profile" "Name" "/profile create"
                           :detail "sharing your memories, skills and credentials"
                           :placeholder "a-z, 0-9, - and _" :verb "create")
                  (ask-row "+ New empty profile" "Name" "/profile create --blank"
                           :detail "nothing carried: the setup walk runs at its first launch"
                           :placeholder "a-z, 0-9, - and _" :verb "create")
                  (ask-row "+ New fork" "Name" "/profile create --clone-all"
                           :detail "your own cells and the layer copied too"
                           :placeholder "a-z, 0-9, - and _" :verb "create")
                  (ask-row "Bring a profile in" "Archive" "/profile import"
                           :detail "from an archive an export wrote"
                           :placeholder "the tar.gz's path" :verb "import")
                  (ask-row "Install a distribution" "Repository" "/profile install"
                           :detail "a home from a git repository"
                           :placeholder "its URL" :verb "install"))))))

(define-bare-dialog "profile" (session-id model)
  (unless model (profile-room-dialog)))

;;; --- /import -----------------------------------------------------------------

(define-bare-dialog "import" (session-id model)
  ;; Beside /import's op, which reads the worlds the walk offers (setup.lisp).
  (import-dialog))
