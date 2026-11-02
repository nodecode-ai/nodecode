;;;; recall-test.lisp --- the model-facing history read (recall.lisp).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gate claims for the PRO-LONG read contract:
;;;;
;;;;   1. RECALL-LOG scans the conversational kinds SQL-side and returns
;;;;      clipped one-line rows, oldest first, tagged with the :seq the
;;;;      eviction stub cites.
;;;;   2. The session scope defaults to the ambient *scribe-session-id* the
;;;;      eval seam binds; with no scope at all the refusal is decisive.
;;;;   3. Truncation is disclosed, never silent: the newest window returns
;;;;      with a (:matched N :showing M) marker, and :before pages older.
;;;;   4. RECALL-ENTRY returns one body whole — the scan-large-return-small
;;;;      split that keeps recall output from refilling the window eviction
;;;;      just freed.
;;;;   5. The read is a pure log read: a durable eviction floor moves the
;;;;      provider window, not what recall can see.
;;;;   6. Rows are stamped :at from the envelope's occurred_at and a tool
;;;;      result surfaces its fact's :duration-ms — per-tool timing is
;;;;      recoverable at this read, and an unmeasured duration stays
;;;;      absent, never an invented zero.

(in-package #:nodecode.test)

(deftest recall-log-scans-conversational-bodies (with-temp-store ())
  (seed-completed-turn "s-1" "please remember the wombat token"
                       "the wombat token is stored"
                       :tools '(("call-1" "eval"
                                 "(find :wombat *zoo*)" "WOMBAT-3")))
  (let ((rows (nlk:recall-log :session-id "s-1" :match "wombat")))
    (is (= 4 (length rows)))
    (is (apply #'< (mapcar (lambda (row) (getf row :seq)) rows)))
    (is (equal "user" (getf (first rows) :role)) "the input row names its role")
    (is (equal "eval" (getf (second rows) :tool)))
    (is (equal "assistant" (getf (fourth rows) :role)) "the answer closes the scan")
    (is (search "wombat token" (getf (first rows) :text)))))

(deftest recall-rows-carry-wall-time-and-tool-duration (with-session-store ("s-1"))
  ;; The harness-level timing contract: every recall row is stamped :at
  ;; with the envelope's occurred_at, and a tool result surfaces the
  ;; :duration-ms its fact recorded, so per-tool timing is answerable from
  ;; the read the eviction stub advertises — never "seq-tagged but not
  ;; timestamped".
  (with-durable-turn (turn "s-1" :input "time the wombat probe" :complete t)
    (record-tool-use turn "call-1" "eval" "(wombat)" "WOMBAT-3" :duration-ms 5321)
    (record-tool-use turn "call-2" "probe" "{\"wombat\":1}" "wombat unmeasured"))
  (let ((rows (nlk:recall-log :session-id "s-1" :match "wombat")))
    (is (every (lambda (row) (nlk::iso-8601-p (getf row :at))) rows))
    (let* ((results (remove-if-not
                     (lambda (row)
                       (equal nlk::+kind-tool-result+ (getf row :kind)))
                     rows))
           (measured (find-if (lambda (row) (equal "WOMBAT-3" (getf row :text))) results))
           (unmeasured (find-if (lambda (row) (equal "wombat unmeasured" (getf row :text)))
                                results)))
      (is (= 5321 (getf measured :duration-ms)))
      (is (null (getf unmeasured :duration-ms)))
      (is-let* ((entry (nlk:recall-entry (getf measured :seq))))
        ((nlk::iso-8601-p (getf entry :at)) is) ((getf entry :duration-ms) = 5321)))))

(deftest recall-log-defaults-to-the-ambient-session (with-temp-store ())
  (seed-completed-turn "s-1" "alpha wombat" "one")
  (seed-completed-turn "s-2" "beta wombat" "two")
  (let* ((nlk:*scribe-session-id* "s-1")
         (rows (nlk:recall-log :match "wombat")))
    (is (= 1 (length rows)) "only the ambient session's rows return")
    (is (search "alpha" (getf (first rows) :text)) "and they are s-1's"))
  (is (search "beta" (getf (first (nlk:recall-log :match "wombat" :session-id "s-2")) :text)))
  (signals-error nlk:recall-unscoped
    (let ((nlk:*scribe-session-id* nil))
      (nlk:recall-log :match "wombat"))))

(deftest recall-log-clips-and-recall-entry-returns-the-whole-body (with-session-store ("s-1"))
  (let ((long (format nil "~{needle line ~d~%~}" (loop for i below 40 collect i))))
    (seed-completed-turn "s-1" "recall the needle text" long)
    (let* ((rows (nlk:recall-log :session-id "s-1" :match "needle line 3"
                                 :kinds nlk::+kind-assistant-completed+))
           (row (first rows)))
      (is (= 1 (length rows)) "a single-kind scan narrows to the answer")
      (is (= 201 (length (getf row :text))))
      (is (char= #\… (char (getf row :text) 200)) "the cut is disclosed")
      (is (not (find #\Newline (getf row :text))) "scan rows are one line")
      (let ((entry (nlk:recall-entry (getf row :seq))))
        (is-shape entry (:text long "the entry body is verbatim") (:turn (getf row :turn)))))))

(deftest recall-never-returns-the-models-thinking (with-session-store ("s-1"))
  ;; od5 g50t: recalled thinking made the API refuse every later request.
  (let ((turn (nlk:admit-turn "s-1" (nlk::make-durable-id "command") "play on"))
        (thought "quokka-private-thought"))
    (nlk:record-assistant-message
     turn (nlk:json-object "role" "assistant" "content" "the visible quokka answer"
                           "reasoning_content" thought
                           "reasoning_blocks" (vector (nlk:json-object "thinking" thought "signature" "c2lnbmVk"))))
    (nlk:record-assistant-message
     turn (nlk:json-object "role" "assistant" "content" :null "reasoning_content" thought))
    (nlk:complete-turn turn nil)
    (flet ((shows-thought-p (rows) (search thought (prin1-to-string rows))))
      (let ((rows (nlk:recall-log :session-id "s-1" :kinds nlk::+kind-assistant-completed+)))
        (is (= 2 (length rows)) "both answers are recalled")
        (is (not (shows-thought-p rows)) "no row carries the thinking")
        ;; nor a whole entry: the answer of thinking alone gets no raw payload
        (let ((entries (mapcar (lambda (row) (nlk:recall-entry (getf row :seq))) rows)))
          (is (not (shows-thought-p entries)))
          (is (null (getf (second entries) :payload)))))
      ;; a match answers what was said, never the thinking
      (is (null (nlk:recall-log :session-id "s-1" :match "private-thought")))
      (is (= 1 (length (nlk:recall-log :session-id "s-1" :match "visible quokka")))))))

(deftest recall-log-truncates-newest-window-and-pages-older (with-temp-store ())
  (seed-user-inputs "s-1" (loop for i from 1 to 9
                                collect (format nil "pelican memo ~d" i)))
  (let* ((rows (nlk:recall-log :session-id "s-1" :match "pelican" :limit 3))
         (marker (first rows)))
    (is (= 4 (length rows)) "three rows plus the truncation marker")
    (is (= 9 (getf marker :matched)) "the marker discloses the full count")
    (is (= 3 (getf marker :showing)) "and how much of it is showing")
    (is (search "memo 9" (getf (car (last rows)) :text)))
    (is (search "memo 7" (getf (second rows) :text)))
    (let ((older (nlk:recall-log :session-id "s-1" :match "pelican" :limit 3
                                 :before (getf (second rows) :seq))))
      (is (search "memo 6" (getf (car (last older)) :text))))))

(deftest recall-log-narrows-by-kinds-and-seq-bounds (with-temp-store ())
  (seed-completed-turn "s-1" "wombat in" "wombat out"
                       :tools '(("c1" "eval" "(wombat)" "wombat result")))
  (let ((results (nlk:recall-log :session-id "s-1" :match "wombat"
                                 :kinds nlk::+kind-tool-result+)))
    (is (= 1 (length results)) "a bare kind string narrows to that kind")
    (is (equal nlk::+kind-tool-result+ (getf (first results) :kind))))
  (let* ((all (nlk:recall-log :session-id "s-1" :match "wombat"))
         (bounded (nlk:recall-log :session-id "s-1" :match "wombat"
                                  :after (getf (second all) :seq))))
    (is (= (- (length all) 2) (length bounded)))))

(deftest recall-entry-falls-back-to-raw-payload (with-temp-store ())
  (seed-completed-turn "s-1" "in" "out")
  (let* ((completed (session-event "s-1" :kind "turn.completed"))
         (entry (nlk:recall-entry (nlk:event-log-position completed))))
    (is (equal "turn.completed" (getf entry :kind)) "the envelope kind rides")
    (is (null (getf entry :text)) "no conversational body is claimed")
    (is (search "turn-number" (getf entry :payload))))
  (is (null (nlk:recall-entry 999999)) "an absent seq is NIL, not an error"))

(deftest recall-reads-below-the-eviction-floor (with-temp-store ())
  ;; The PRO-LONG claim itself: eviction moves the provider window's floor,
  ;; and recall does not consult it — evicted turns stay readable.
  (seed-completed-turn "s-1" "the launch code is osprey-7" "stored")
  (seed-completed-turn "s-1" "unrelated follow-up" "ok")
  (let ((floor (nlk:event-log-position
                (car (last (session-events "s-1"))))))
    (nlk:advance-history-floor "s-1" floor)
    (let ((rows (nlk:recall-log :session-id "s-1" :match "osprey")))
      (is (= 1 (length rows)) "the evicted input is still recallable")
      (is (search "osprey-7"
                  (getf (nlk:recall-entry (getf (first rows) :seq)) :text))))))

(deftest recall-log-reads-the-chain-a-fork-composes (with-temp-store ())
  ;; A lane's evicted history is its room's: the scan follows the fork chain,
  ;; each ancestor up to the anchor, so the lane reads what its window was
  ;; made of and nothing the room recorded after the fork.
  (seed-completed-turn "room-r" "the launch code is osprey-7" "stored")
  (nlk:create-session :id "lane-r" :parent "room-r")
  (seed-completed-turn "room-r" "a later osprey the lane never saw" "ok")
  (seed-completed-turn "lane-r" "what was the osprey code" "osprey-7")
  (flet ((inputs (session-id)
           (mapcar (lambda (row) (getf row :text))
                   (nlk:recall-log :session-id session-id :match "osprey"
                                   :kinds "turn.input_committed"))))
    (is (equal '("the launch code is osprey-7" "what was the osprey code")
               (inputs "lane-r")))
    ;; the room reads its own, never a lane's
    (is (equal '("the launch code is osprey-7" "a later osprey the lane never saw")
               (inputs "room-r")))))


;;; The claims a search box leans on: it answers every session in one read,
;;; it matches the conversational body rather than the envelope's own JSON
;;; (the raw-payload LIKE it replaces answered every message for the word
;;; `role'), every token is required and literal, a snippet is windowed
;;; around the match, and each row carries the session, its title and the
;;; :seq the block pins at.

(deftest recall-search-scans-every-session-at-once (with-temp-store ())
  (seed-completed-turn "s-1" "remember the pelican token"
                       "stored the pelican token")
  (seed-completed-turn "s-2" "unrelated prompt"
                       "the second session also names a pelican")
  (let ((rows (nlk:recall-search "pelican")))
    (is (= 3 (length rows)) "one row per matching body, across sessions")
    (is (equal '("s-2" "s-1" "s-1")
               (mapcar (lambda (row) (getf row :session-id)) rows)))
    (is (apply #'> (mapcar (lambda (row) (getf row :seq)) rows)))
    (is (equal "assistant" (getf (first rows) :role)))
    (is (equal "user" (getf (third rows) :role)))
    (is (search "pelican" (getf (third rows) :text)))
    (is (search "pelican" (getf (first rows) :text)))))

(deftest recall-search-carries-session-title-and-block-identity (with-temp-store ())
  (seed-completed-turn "s-title" "titled session opening line" "an answer")
  (is-let* ((row (first (nlk:recall-search "opening line")))) ((getf row :session-id) "s-title")
    ((search "titled session" (getf row :title)) is) ((integerp (getf row :seq)) is)
    ((getf row :kind) "turn.input_committed")))

(deftest recall-search-matches-the-body-never-the-envelope (with-temp-store ())
  ;; The whole-payload LIKE this read used to be answered the JSON keys every
  ;; message carries, so searching a word like `assistant' answered the whole
  ;; store. The match runs over the conversational body alone.
  (seed-completed-turn "s-1" "hello there" "general kenobi")
  (is (null (nlk:recall-search "assistant")) "the role in the envelope is not a match")
  (is (null (nlk:recall-search "message")) "nor is the field name")
  (is (null (nlk:recall-search "user")))
  (is (= 1 (length (nlk:recall-search "kenobi"))) "while the words are"))

(deftest recall-search-requires-every-token-and-treats-them-literally (with-temp-store ())
  (seed-completed-turn "s-1" "alpha one" "beta two")
  (is (null (nlk:recall-search "alpha beta")))
  (is (= 1 (length (nlk:recall-search "alpha one"))) "two tokens in one body do")
  (seed-completed-turn "s-1" "100% sure" "under_score stays")
  (is (= 1 (length (nlk:recall-search "100%"))) "a % searches for itself")
  (is (null (nlk:recall-search "1%0")) "and is never a wildcard")
  (is (= 1 (length (nlk:recall-search "under_score"))) "a _ searches for itself")
  (is (null (nlk:recall-search "und_r")) "and is never a wildcard"))

(deftest recall-search-windows-the-snippet-around-the-match (with-temp-store ())
  (let ((body (concatenate 'string (make-string 400 :initial-element #\x)
                           " the needle")))
    (seed-completed-turn "s-1" "a prompt" body)
    (let* ((rows (nlk:recall-search "needle" :lead 32 :window 120))
           (snippet (and rows (getf (first rows) :text))))
      (is (= 1 (length rows)))
      (is (search "needle" snippet) "the match is in the snippet")
      (is (< (length snippet) (length body)) "the body is not")
      (is (char= #\… (char snippet 0))))))

(deftest recall-search-refuses-an-empty-query (with-temp-store ())
  (seed-completed-turn "s-1" "anything at all" "an answer")
  (signals-error nlk:recall-query-empty (nlk:recall-search "   "))
  (signals-error nlk:recall-query-empty (nlk:recall-search "")))
