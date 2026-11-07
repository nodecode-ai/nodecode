;;;; outbound-test.lisp --- chunking, retry schedule, executors, redaction.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest channel-outbound-chunking
    (flet ((texts (text limit) (mapcar #'nck:text-chunk-text (nck:split-text-chunks text limit)))))
  (let ((chunks (nck:split-text-chunks "abcdefgh" 3)))
    (is (= 3 (length chunks)))
    (is (equal '("abc" "def" "gh")
               (mapcar #'nck:text-chunk-text chunks)))
    (is (equal '(1 2 3) (mapcar #'nck:text-chunk-index chunks)))
    (is (equal '(3 3 3) (mapcar #'nck:text-chunk-total chunks))))
  (is (equal '("trimmed") (texts "  trimmed  " 100)))
  ;; Code points, not bytes: two 3-byte CJK chars fit a 2-char limit as one
  ;; chunk each boundary.
  (is (equal '("你好" "世界") (texts "你好世界" 2)))
  ;; A boundary lands on a line break, or a space, before the count runs
  ;; out: the link is never cut, and no character is lost or duplicated.
  (let* ((text (format nil "~{~a~^ ~} → <#1549180480505516105>"
                       (loop repeat 79 collect "word")))
         (texts (texts text 200)))
    (is (< 1 (length texts)))
    (is (every (lambda (chunk-text) (<= (length chunk-text) 200)) texts))
    (is (equal text (apply #'concatenate 'string texts)))
    (is (let ((carrier (find "<#" texts :test #'search)))
          (and carrier (search "<#1549180480505516105>" carrier)))))
  ;; Lines come before words: the window's last line break wins even when
  ;; more characters would fit inside it.
  (let ((text (concatenate 'string (make-string 150 :initial-element #\x)
                           (string #\Newline)
                           (make-string 120 :initial-element #\y))))
    (is (equal '(151 120) (mapcar #'length (texts text 200)))))
  (is (signals-error error (nck:split-text-chunks "   " 10))))

(deftest channel-outbound-chunking-keeps-code-fences-whole
    (flet ((texts (text limit) (mapcar #'nck:text-chunk-text (nck:split-text-chunks text limit)))
           (fences (text) (loop for at = (search "```" text) then (search "```" text :start2 (+ at 3))
                                while at count t))))
  ;; Every message is its own document: a fence one chunk leaves open is
  ;; opened again by the next chunk's first ```, and every later chunk reads
  ;; code as prose and prose as code. A chunk cut inside a fence closes it,
  ;; the next opens it again under the same language, and no line of the text
  ;; is lost or doubled.
  (let* ((code (format nil "~{line ~d of the loop~^~%~}" (loop for n from 1 to 60 collect n)))
         (text (format nil "Here it is:~%~%```lisp~%~a~%```~%~%That is all." code))
         (texts (texts text 400)))
    (is (< 2 (length texts)))
    (is (every (lambda (chunk) (<= (length chunk) 400)) texts))
    (is (every (lambda (chunk) (evenp (fences chunk))) texts) "every chunk's fences pair")
    ;; The next chunk opens the fence again under its language.
    (is (uiop:string-prefix-p (format nil "```lisp~%") (second texts)))
    (is (loop for n from 1 to 60
              always (= 1 (count-if (lambda (chunk) (search (format nil "line ~d of the loop" n) chunk))
                                    texts))))
    (is (search "That is all." (car (last texts)))))
  ;; A block that fits one message whole moves to the next chunk rather than
  ;; being cut in two.
  (let* ((prose (format nil "~{~a~^ ~}" (loop repeat 60 collect "word")))
         (block (format nil "```python~%~{print(~d)~^~%~}~%```" (loop for n from 1 to 20 collect n)))
         (texts (texts (format nil "~a~%~a~%after" prose block) 400)))
    (is (= 2 (length texts)))
    (is (zerop (fences (first texts))) "the first chunk is the prose alone")
    (is (uiop:string-prefix-p "```python" (second texts)))
    (is (= 2 (fences (second texts)))))
  ;; Below the floor the lines that carry a fence would crowd out the text:
  ;; a tiny limit cuts raw.
  (is (equal (list (format nil "```a~%") "bc```") (texts (format nil "```a~%bc```") 5))))

(deftest channel-outbound-chunking-moves-a-cut-table-whole ()
  ;; A table cut across two messages leaves the second its tail alone: pipe
  ;; rows with no header and no rule, nothing a platform draws. A table that
  ;; opened in the chunk and fits one message moves to the next whole, as a
  ;; fence block does.
  (let* ((table (format nil "| a | b |~%|---|---|~%| 1 | 2 |~%| 3 | 4 |"))
         (text (format nil "~a~%~%~a~%~%That is all." (make-string 40 :initial-element #\x) table))
         (texts (mapcar #'nck:text-chunk-text (nck:split-text-chunks text 60))))
    (is (some (lambda (chunk) (search table chunk)) texts) "one message carries the whole table")
    (is (every (lambda (chunk) (or (not (find #\| chunk)) (search "|---|" chunk))) texts) "no message opens on a table's tail")))

(deftest channel-outbound-retry-schedule ()
  ;; Opted-in plans retry 429/5xx with linear backoff, three attempts total.
  (let ((result (scripted-execution
                 (list (nck:make-scripted-response 429)
                       (nck:make-scripted-response 500)
                       (reply 200 "ok" t))
                 :retry t)))
    (is-shape result (nck:execution-ok-p is) (nck:execution-attempts = 3)
      (nck:execution-retry-delays '(250 500))))
  ;; The retry-after header wins over the computed backoff.
  (let* ((headers (nlk:make-json-object "retry-after" "2"))
         (result (scripted-execution
                  (list (nck:make-scripted-response 429 nil headers)
                        (nck:make-scripted-response 200))
                  :retry t)))
    (is-shape result (nck:execution-ok-p is) (nck:execution-retry-delays '(2000))))
  ;; A protocol retry-after hook (Telegram body pacing) outranks both.
  (let ((result (scripted-execution
                 (list (nck:make-scripted-response 429)
                       (nck:make-scripted-response 200))
                 :retry t
                 :retry-after-fn (lambda (status body headers)
                                   (declare (ignore status body headers))
                                   1234))))
    (is (equal '(1234) (nck:execution-retry-delays result))))
  ;; Plans that do not opt in fail on the first 429.
  (let ((result (scripted-execution (list (nck:make-scripted-response 429)))))
    (is-shape result (nck:execution-ok-p not) (nck:execution-attempts = 1)
      (nck:execution-status = 429))))

(deftest channel-outbound-secret-redaction ()
  (is (equal "token [redacted] leaked [redacted]!"
             (nck:redact-text "token sk-123 leaked sk-123!" '("sk-123"))))
  (is (equal "clean" (nck:redact-text "clean" '("sk-123"))))
  (is (equal "x" (nck:redact-text "x" '(nil "")))))

(deftest channel-outbound-delivery-stops-at-first-failure ()
  (let ((executor (scripted-executor (nck:make-scripted-response 200)
                                     (reply 403 "message" "forbidden"))))
    (multiple-value-bind (delivered results error)
        (nck:execute-delivery
         (list (nck:make-request-plan :path "/1" :audit-label "one")
               (nck:make-request-plan :path "/2" :audit-label "two")
               (nck:make-request-plan :path "/3" :audit-label "three"))
         executor)
      (is (not delivered))
      (is (= 2 (length results)) "the third plan never executes")
      (is (stringp error))
      (is (= 2 (length (nck:recording-executor-plans executor)))))))

(deftest channel-outbound-multipart-body-p ()
  (is (not (nck:multipart-body-p nil)))
  (is (not (nck:multipart-body-p (nlk:json-object "content" "hi"))))
  (is (nck:multipart-body-p
       '(("payload_json" . "{}") ("files[0]" . #P"x.png")))))

(deftest channel-outbound-plist-shapes ()
  ;; The model reads a decoded body as a keyword plist and writes one back:
  ;; the two converters are inverses over snake_case keys.
  (let* ((decoded (cell-json
                   "{\"id\": \"1\", \"pinned\": false, \"edited\": null,
                     \"author\": {\"id\": \"2\", \"bot\": true},
                     \"embeds\": [], \"mentions\": [{\"id\": \"3\"}]}"))
         (plist (nck:json-plist decoded)))
    (is (equalp '(:id "1" :pinned nil :edited :null
                  :author (:id "2" :bot t)
                  :embeds #() :mentions #((:id "3")))
                plist))
    (is (equalp decoded (nck:plist-json plist))))
  (is (equal "plain" (nck:json-plist "plain")) "a non-JSON body passes")
  (is (equal '(:a 1) (nck:json-plist (nck:plist-json '(:a 1)))))
  (let ((object (nck:plist-json '(:content "hi"
                                  :allowed_mentions (:parse #())))))
    (is-shape object (hash-table-p is) ("content" "hi" "keys downcase"))
    (is (equal "{\"content\":\"hi\",\"allowed_mentions\":{\"parse\":[]}}"
               (let ((*print-pretty* nil)) (nlk:encode-json-object object)))))
  (let ((passthrough (nlk:json-object "Mixed-Case" 1)))
    (is (eq passthrough (nck:plist-json passthrough)))))

(deftest channel-outbound-call ()
  ;; NCK:CALL is one request in the model's shape: plan in, plist out.
  (let* ((executor (scripted-executor (reply 200 "id" "9" "ok" t)
                                      (reply 403 "message" "Missing Access" "code" 50001)
                                      (nck:make-scripted-response 204 nil)))
         (created (nck:call executor "post" "/channels/1/messages"
                            :body '(:content "hi" :tts nil)
                            :headers '(("x-audit-log-reason" . "why"))
                            :timeout 7 :retry nil)))
    (is (equal '(:status 200 :body (:id "9" :ok t)) created))
    (is-present (plan (first (nck:recording-executor-plans executor))) "the plan the executor saw"
      (is (equal "POST" (nck:request-plan-method plan)) "method upcases")
      (is-plan plan :path "/channels/1/messages" "content" "hi"
                    :headers '(("x-audit-log-reason" . "why")) :timeout 7 :retry nil
                    :label "request"))
    (let ((denied (nck:call executor "GET" "/guilds/1/members")))
      (is-shape denied (:status = 403) (:body '(:message "Missing Access" :code 50001)))
      (is (search "status 403" (getf denied :error))))
    (is (equal '(:status 204 :body nil)
               (nck:call executor "DELETE" "/channels/1/messages/9")))
    ;; A multipart alist and an nlk:json-object both pass through as the plan's body.
    (loop for (path body) in (list (list "/channels/1/messages"
                                         '(("payload_json" . "{}") ("files[0]" . #P"/tmp/x.png")))
                                   (list "/x" (nlk:json-object "content" "raw")))
          do (nck:call executor "POST" path :body body)
             (is (eq body
                     (nck:request-plan-body
                      (car (last (nck:recording-executor-plans executor)))))))
    (let ((before (length (nck:recording-executor-plans executor))))
      (is (search "request body must be"
                  (refusal-text error (nck:call executor "POST" "/x" :body "{}"))))
      (is (= before (length (nck:recording-executor-plans executor))))))
  ;; A transport failure is status 0 with the redacted error, no body.
  (let* ((executor (nck:make-plan-executor
                    :run (lambda (plan)
                           (declare (ignore plan))
                           (nck:make-execution :ok-p nil :status 0
                                               :attempts 1
                                               :error "connection refused"))))
         (result (nck:call executor "GET" "/users/@me")))
    (is (equal '(:status 0 :body nil :error "connection refused") result))))

(deftest channel-outbound-call-reads-the-object-back ()
  ;; A 204 on a reaction is the platform accepting a request for
  ;; whatever id the path carried, not the reaction existing. :READ names
  ;; the object the write changes, and the read's whole answer rides back
  ;; under :STATE — so what is reported is what the platform holds.
  (let* ((message (cell-json
                   "{\"id\": \"2\", \"content\": \"India first\",
                     \"reactions\": [{\"emoji\": {\"name\": \"IN\"},
                                      \"count\": 1, \"me\": true}]}"))
         (executor (scripted-executor (nck:make-scripted-response 204 nil)
                                      (nck:make-scripted-response 200 message)))
         (answer (nck:call executor "PUT"
                           "/channels/1/messages/2/reactions/x/@me"
                           :headers '(("x-audit-log-reason" . "why"))
                           :read "/channels/1/messages/2")))
    (is-shape answer (:status = 204 "the write's own status is untouched") (:body null))
    (is-present (state (getf answer :state)) "the read rides back under :state"
      (is (= 200 (getf state :status)))
      (is (equal "India first" (getf (getf state :body) :content)))
      (is (equalp (vector (list :emoji (list :name "IN") :count 1 :me t))
                  (getf (getf state :body) :reactions))))
    (is-present (plans (nck:recording-executor-plans executor)) "the write, then the read"
      (is (= 2 (length plans)))
      (is-plan (second plans) :method "GET" :path "/channels/1/messages/2" :headers nil))))

(deftest channel-outbound-call-reads-back-only-what-it-must ()
  ;; The read is for the one case nothing else covers — a write the platform
  ;; accepted in silence. A read is already state, a refusal changed nothing,
  ;; an answer carrying the object is the object, and NIL asks for none.
  (labels ((attempt (method responses &key (read "/channels/1/pins"))
             (let ((executor (nck:make-recording-executor
                              :responses responses)))
               (let ((answer (nck:call executor method "/channels/1/pins/2"
                                       :read read)))
                 (values answer
                         (length (nck:recording-executor-plans executor))))))
           (unread (note method responses &rest keys)
             (multiple-value-bind (answer plans) (apply #'attempt method responses keys)
               (is (null (getf answer :state)) note)
               (is (= 1 plans)))))
    (unread "a read is already the state" "GET" (list (nck:make-scripted-response 200 nil)))
    (unread "a refusal left nothing to read" "PUT" (list (reply 403 "code" 50013)))
    (unread "an answer carrying the object needs no second look" "PUT" (list (reply 200 "id" "2")))
    (unread "and NIL reads nothing back" "PUT" (list (nck:make-scripted-response 204 nil)) :read nil)
    ;; A read that itself failed says so. The one thing it must never do is
    ;; go missing, which would leave a silent write reading as a clean one.
    (multiple-value-bind (answer plans)
        (attempt "PUT" (list (nck:make-scripted-response 204 nil)
                             (reply 404 "message" "Unknown Message")))
      (is (= 2 plans))
      (is-present (state (getf answer :state)) "the failed read still rides back"
        (is-shape state (:status = 404) (:body '(:message "Unknown Message")))
        (is (search "status 404" (getf state :error)))))))

(deftest channel-outbound-array-bodies-decode-like-objects ()
  ;; A top-level JSON array (a channel's message list, a guild's channels)
  ;; is a vector in the answer, the shape the docstrings promise — never a
  ;; string the snippet has to find a parser for.
  (let ((array (nck::parse-json-body "[{\"id\":\"1\"},{\"id\":\"2\"}]")))
    (is (and (vectorp array) (not (stringp array)) (= 2 (length array))))
    (is (equal "2" (gethash "id" (aref array 1))) "of decoded objects"))
  (is (hash-table-p (nck::parse-json-body "{\"a\":1}")) "an object still decodes")
  (is (equal "[1," (nck::parse-json-body "[1,")) "text that is not JSON stays text")
  (is (equal "" (nck::parse-json-body "")) "a 204's blank stays blank")
  (is (equal "42" (nck::parse-json-body "42")) "a bare scalar stays its text")
  (is (equal "2" (gethash "id" (aref (nck::parse-json-body
                                      (sb-ext:string-to-octets "[{\"id\":\"2\"}]"))
                                     0))))
  (let ((answer (nck::call-answer
                 (nck:make-execution :ok-p t :status 200 :attempts 1
                                     :body (vector (nlk:json-object "id" "7"))))))
    (is (equalp '(:status 200 :body #((:id "7"))) answer))))
