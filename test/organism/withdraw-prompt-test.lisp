;;;; withdraw-prompt-test.lisp --- the Up peel: taking a queued prompt back.
;;;;
;;;; Claims:
;;;;   1. A withdrawal takes a parked steer out of the newest queue snapshot,
;;;;      hands its text and images back, and leaves the running turn alone —
;;;;      a steer owns no durable fact, so the snapshot is the whole write.
;;;;   2. A follow-up too: its admission fact is queue state no context
;;;;      reads, the snapshot is the queue, and a withdrawn one never runs.
;;;;   3. withdraw_prompt round-trips the wire and, against a live gateway,
;;;;      answers the requester with the prompt's content, images and turn
;;;;      while the durable queue really empties; a second withdrawal of the
;;;;      same id is refused.
;;;;   4. Up peels the newest queued prompt, steer or follow-up, into the
;;;;      composer and takes its row out of the band with it, and leaves Up
;;;;      to prompt history whenever nothing is peelable — including while
;;;;      this composer already holds one — while the band names both
;;;;      gestures: the row Esc would cut, and one dim hint line under the
;;;;      block for the peel (the codex pending-input preview's own edit hint).
;;;;   5. A refused withdrawal (the prompt promoted first) takes its text back
;;;;      out of the composer and says so; Enter on an empty composer drops
;;;;      the hold; submitting ends it.
;;;;   6. A queue snapshot that no longer names a queued prompt clears its
;;;;      band row — how every shell learns of a withdrawal, this one's own
;;;;      peel included.
;;;;
;;;; Loads last: drives lifecycle-test's admission helpers, gateway-test's WS
;;;; fixtures, steer-test's shell factory and ui-test's key helpers.

(in-package #:nodecode.test)

;;; --- seeding a queue the way the stream does -------------------------------

(defun %seed-queue-snapshot (model prompts &key (turn-id "turn-live") (disposition "steer"))
  "Fold one queue snapshot into MODEL as the live stream would: PROMPTS as
(PROMPT-ID . TEXT) conses, each parked under DISPOSITION (NIL a follow-up)."
  (feed-events model.state
    (nlk:json-object
     "type" "session_input_queue_snapshot_updated"
     "turn_id" turn-id
     "snapshot" (nlk:json-object
                 "active_turn" turn-id
                 "pending_prompts"
                 (coerce (loop for (id . text) in prompts
                               collect (nlk:json-object "prompt_id" id
                                                        "content" text
                                                        :opt "disposition" disposition))
                         'vector))))
  model)

(defun %band-prompt-ids (model)
  "The prompt ids of the queued rows in MODEL's band, in band order."
  (mapcar #'nle::transcript-row-prompt-id
          (nle::pending-steer-rows model.state)))

;;; --- 1-2: the durable withdrawal -------------------------------------------

(deftest withdrawing-a-parked-steer-hands-it-back-and-frees-its-seat ()
  (with-session-store ("withdraw-seat")
    (let* ((turn (%admitted-active-turn "withdraw-seat" "wd-busy" "busy prompt"))
           (turn-id turn.turn-id))
      (is (not (nlk:steer-parked-p turn)) "nothing is parked yet")
      (is (eq :steered (%steer "withdraw-seat" "wd-steer" "fix teh typo" turn-id
                               :images (list (cons "image/png" "aGk=")))))
      (is (nlk:steer-parked-p turn) "the steer waits in the queue")
      (is (= 1 (length (%input-facts "withdraw-seat"))) "no input fact of its own")
      (let ((withdrawn (nlk:withdraw-pending-prompt "withdraw-seat" "wd-steer")))
        (is withdrawn "the withdrawal answered")
        (is-shape withdrawn (:content "fix teh typo")
          (:images '(("image/png" . "aGk=")) "images too") (:turn-id turn-id) (:queue-depth eql 0)))
      (is (not (nlk:steer-parked-p turn)) "the turn reads no parked steer now")
      (is (zerop (length (nlk::queue-prompts "withdraw-seat"))) "seat gone")
      (is (nlk:active-turn-p "withdraw-seat") "the running turn is untouched")
      (is (= 1 (length (%input-facts "withdraw-seat"))) "and wrote no fact either")
      (is (null (nlk:withdraw-pending-prompt "withdraw-seat" "wd-steer")) "gone"))))

(deftest a-withdrawn-follow-up-never-runs (with-session-store ("withdraw-follow"))
  ;; A follow-up's admission fact is on the record, but the snapshot is the
  ;; queue: withdrawing it is one snapshot, and nothing promotes it after.
  (let* ((turn (%admitted-active-turn "withdraw-follow" "wf-busy" "busy prompt"))
         (queued (nlk:admit-active-turn-input "withdraw-follow" "wf-follow" "second"
                                              :images (list (cons "image/png" "aGk=")))))
    (is (eq :queued queued.disposition))
    (let ((withdrawn (nlk:withdraw-pending-prompt "withdraw-follow" "wf-follow")))
      (is withdrawn "the withdrawal answered")
      (is-shape withdrawn (:content "second") (:images '(("image/png" . "aGk=")) "images too")
        (:turn-id turn.turn-id) (:queue-depth eql 0)))
    (is (zerop (length (nlk::queue-prompts "withdraw-follow"))) "seat gone")
    (is (null (nlk:withdraw-pending-prompt "withdraw-follow" "wf-follow")) "gone")
    (is (nlk:active-turn-p "withdraw-follow") "the running turn is untouched")
    (nlk:complete-turn turn nil)
    (is (null (nlk::dequeue-next-turn "withdraw-follow")) "nothing is left to run")
    (is (equal '("busy prompt") (%asked "withdraw-follow")) "and no provider context reads it")))

;;; --- 3: the wire, and a live gateway -----------------------------------------

(deftest withdraw-prompt-over-the-wire-empties-the-queue-with-its-content ()
  (with-gateway-session (port "withdraw-wire")
    (flet ((withdrawn ()
             (frames-while (ws port)
               (nle::send-subscribe ws (list "withdraw-wire"))
               (nle::send-withdraw-prompt ws "withdraw-wire" "ww-steer"))))
      (let* ((admission (nlk:admit-active-turn-input "withdraw-wire" "ww-busy" "busy prompt"))
             (turn-id admission.turn-id))
        (nlk:admit-active-turn-input "withdraw-wire" "ww-steer" "fix teh typo"
                                     :disposition :steer :expected-turn-id turn-id
                                     :images (list (cons "image/png" "aGk=")))
        (let* ((result (%command-result (withdrawn) "withdraw_prompt"))
               (response (and result (gethash "response" result)))
               (images (and response
                            (nlk::content-image-attachments (gethash "images" response)))))
          (is-present result "the withdrawal came back as a correlated result"
            (is (equal "accepted" (gethash "status" result)))
            (is-shape response ("content" "fix teh typo") ("turn_id" turn-id))
            (is (equal '(("image/png" . "aGk=")) images) "the images ride along")))
        (is (zerop (length (nlk::queue-prompts "withdraw-wire"))) "queue empty")
        (is (nlk:active-turn-p "withdraw-wire") "the turn runs on")
        ;; A second withdrawal of the same id is refused, never repeated.
        (is-present (result (%command-result (withdrawn) "withdraw_prompt"))
            "the refusal is answered too"
            (is-shape result ("status" "rejected") ("code" "prompt_not_pending")))))))

;;; --- 4-5: the shell's peel ----------------------------------------------------

(defun %peelable-model (session-id &optional (prompts '(("p-steer" . "fix teh typo")))
                         &aux (model (make-steer-model session-id)))
  "A gateway shell mid-turn on SESSION-ID with PROMPTS parked as steers in its band."
  (setf model.turn-in-flight t (nle::gateway-model-active-turn-id model) "turn-live")
  (when prompts (%seed-queue-snapshot model prompts))
  model)

(defmacro with-peel-recording ((model session-id &rest prompts) &body body)
  "Run BODY with MODEL a mid-turn shell holding one parked steer, or PROMPTS
when given, its withdrawal and submissions recorded rather than sent."
  ;; Over a link that could carry them, or a submission is refused before any send.
  (let ((link (gensym "LINK")))
    `(with-temp-store ()
       (with-recording-link (,link)
         (let ((,model (%peelable-model ,session-id ,@prompts))
               (withdrawals '())
               (steers '()))
           (setf (nle::gateway-model-ws ,model) ,link)
           (with-stubbed-fdefinitions
               ((nle::send-withdraw-prompt (ws session-id prompt-id)
                  (push (list session-id prompt-id) withdrawals))
                (nle::send-steer-turn (ws session-id turn-id prompt &key command-id images)
                  (push prompt steers))
                (nle::send-start-turn (ws session-id prompt &key command-id
                                                        correlation-id images target)
                  (push prompt steers)))
             ,@body))))))

(deftest up-peels-a-parked-steer-into-the-composer (with-peel-recording (model "up-peel"))
  (press model :up)
  (is (equal "fix teh typo" (composer-value model)) "the text came home")
  (is (equal '(("up-peel" "p-steer")) withdrawals) "withdraw_prompt went out")
  (is (equal "p-steer" (getf (nle::held-prompt model.state)
                              :prompt-id)))
  (is (null (nle::pending-steer-rows model.state)) "row gone")
  (is (null (nth-value 1 (nle::pending-steer-lines model 100))))
  ;; Up again: the hold keeps Up a cursor key — no second peel, and no
  ;; history walk over the text being edited.
  (press model :up)
  (is (equal "fix teh typo" (composer-value model)))
  (is (= 1 (length withdrawals)) "no second withdrawal")
  ;; Submitting ends the hold and queues the prompt again.
  (press model :enter)
  (is (null (nle::held-prompt model.state)))
  (is (equal '("fix teh typo") steers) "and the prompt is queued again"))

(deftest up-peels-a-queued-follow-up-too (with-peel-recording (model "up-follow"))
  ;; A follow-up waits for the turn to finish rather than cutting it, and
  ;; Up takes it back all the same: the newest queued prompt, whichever.
  ;; The follow-up's own admission fact paints its row, as the fold does.
  (setf model.state (nle::ingest-pending-input-fact model.state "then the docs"
                                                    :prompt-id "p-after" :turn-id "turn-live"))
  (press model :up)
  (is (equal '(("up-follow" "p-after")) withdrawals) "the newest, a follow-up")
  (is (equal "then the docs" (composer-value model)))
  (is (equal '("p-steer") (%band-prompt-ids model)) "the steer stays queued"))

(deftest the-band-hints-how-a-queued-prompt-is-edited (with-temp-store ())
  (let ((model (%peelable-model "peel-hint" '(("p-a" . "first steer") ("p-b" . "second steer"))))
        (row "   ⌎ first steer  — esc dispatches now")
        (hint "     up edits the last queued prompt"))
    (flet ((band () (nth-value 1 (nle::pending-steer-lines model 100))))
      ;; The rows stay rows and the hint is the block's own footer line, at
      ;; the body column, dim: the pending-input preview's shape.
      (is (equal (list row "   ⌎ second steer" hint) (band)))
      ;; One steer: one row, one hint — the footer never doubles as a row.
      (%seed-queue-snapshot model '(("p-a" . "first steer")))
      (is (equal (list row hint) (band)))
      ;; A draft in the composer keeps Up on prompt history, so the hint
      ;; that says otherwise would be a lie and is not painted.
      (nle::replace-composer-text model "a draft")
      (is (equal (list row) (band)))
      ;; Clearing the draft gives Up back to the steer, and the hint with it.
      (nle::editor-reset model.editor)
      (is (equal (list row hint) (band))))))

(deftest a-refused-withdrawal-gives-the-composer-back-and-says-so ()
  (with-peel-recording (model "up-refused")
    (press model :up)
    (is (equal "fix teh typo" (composer-value model)))
    (nle::note-withdraw-prompt-result model "rejected" nil)
    (is (equal "" (composer-value model)) "the text goes home empty-handed")
    (is (null (nle::held-prompt model.state)))
    (is (equal "That prompt already started" (nle::status-said model)))))

(deftest enter-on-an-empty-composer-drops-a-held-prompt (with-peel-recording (model "up-discard"))
  (press model :up)
  (nle::replace-composer-text model "")
  (press model :enter)
  (is-shape model.state (nle::held-prompt null) (nle::pending-steer-rows null "ghost gone"))
  (is (equal "Held prompt dropped" (nle::status-said model)))
  (is (null steers) "nothing was submitted"))

(deftest up-walks-history-when-nothing-is-peelable (with-peel-recording (model "up-history" nil))
  (setf model.prompt-history '("older prompt")
        model.history-loaded t)
  (press model :up)
  (is (equal "older prompt" (composer-value model)) "Up is history again")
  (is (null (nle::held-prompt model.state))))

;;; --- 6: the other shells ------------------------------------------------------

(deftest a-withdrawn-steer-leaves-every-shells-band
    (with-peel-recording (model "prune-rows" '(("p-a" . "first steer") ("p-b" . "second steer"))))
  (is (equal '("p-a" "p-b") (%band-prompt-ids model)))
  ;; Another shell withdrew p-a: the newest snapshot no longer names it,
  ;; and the row leaves this shell's band with it.
  (%seed-queue-snapshot model '(("p-b" . "second steer")))
  (is (equal '("p-b") (%band-prompt-ids model)))
  ;; This shell's own peel takes the newest row out at once: the
  ;; composer holds its text and the band holds nothing.
  (press model :up)
  (is (equal '(("prune-rows" "p-b")) withdrawals))
  (is (equal "second steer" (composer-value model)))
  (is (null (%band-prompt-ids model)) "the row left with the peel")
  (is (null (nth-value 1 (nle::pending-steer-lines model 100))))
  ;; The snapshot that follows the withdrawal changes nothing further.
  (%seed-queue-snapshot model '())
  (is (null (%band-prompt-ids model))))

(deftest a-withdrawn-follow-up-leaves-every-shells-band
    (with-peel-recording (model "prune-follow" nil))
  ;; A follow-up's row is painted by its admission fact, and the queue
  ;; snapshot is the band's authority all the same: one that stops naming it
  ;; is a withdrawal, and the row goes with it.
  (setf model.state (nle::ingest-pending-input-fact model.state "then the docs"
                                                    :prompt-id "p-f" :turn-id "turn-live"))
  (%seed-queue-snapshot model '(("p-f" . "then the docs")) :disposition nil)
  (is (equal '("p-f") (%band-prompt-ids model)) "named, it stays")
  (let ((row (first (nle::pending-steer-rows model.state))))
    (is (not (nle::transcript-row-steer-p row)) "and stays a follow-up"))
  (%seed-queue-snapshot model '())
  (is (null (%band-prompt-ids model)) "unnamed, it goes"))
