;;;; cell.lisp --- /link, its panel, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A bare /link shows the link and moves nothing: the asking phone's page
;;;; sends its operator here to compare the code, and a bare word that cut
;;;; the line stranded that phone. /link on keeps the line and answers this
;;;; machine's address; /link off drops it, and nothing reaches the machine
;;;; through the relay at all. Each opens the panel: while the link is on the
;;;; address's QR code, for a phone's camera, then the browser asking now, if
;;;; one is, and every browser let in, Enter on a row handing the composer the
;;;; command that answers it. The switch is kept on disk, so a gateway that
;;;; restarts comes back linked, and turning it on asks for the background
;;;; gateway (NLE:KEEP-RUNNING): the page is reachable while no shell is open,
;;;; which is the point of reaching it from elsewhere. The web page's Link tab
;;;; reads and moves the same through /api/link, the same verbs under it.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `memory':
;;;;   "link": {"relay": "wss://uplink.nodecode.ai/line"}

(in-package #:nodecode-link)

(defun linked-p ()
  (let ((line *line*)) (and line (eq (line-state line) :up))))

(defun address ()
  (or (kept "address") (kept "origin")))

(defun ago (iso)
  (nlk:when-let (at (nlk:iso-universal iso))
    (format nil "~a ago" (nlk:duration-text (max 0 (- (get-universal-time) at)) :coarse t))))

(defvar *address-code* nil
  "(ADDRESS . ROWS): the last address drawn as a QR code (QR-ROWS), kept
since the page's Link tab reads it every few seconds and the address seldom
moves.")

(defun address-code (&aux (address (address)) (kept *address-code*))
  "The address as a QR code while the link is on -- the rows a phone's
camera reads, NIL while there is no address to reach."
  (when (and address (eq (state) :on))
    (if (equal address (car kept))
        (cdr kept)
        (cdr (setf *address-code* (cons address (qr-rows address)))))))

(defun post-address (address)
  "The address in the transcript, on a line of its own: selectable, and one
click from the page."
  ;; Unkeyed: said to the shells attached now and kept out of the board, which
  ;; is the model's prompt.
  (nle:notice (format nil "link: this machine's page, for any browser you allow here:~%~a" address)))

(defun web-note ()
  (unless (nlk:system-cell "nodecode-web")
    "; the web cell is not installed, so the address has no page yet (/cells)"))

(defun turn-on ()
  (keep "on" t)
  (nlk:worker-poke *worker*)
  (nle:keep-running "this machine's page at its link address")
  (if (await-line 5)
      (progn (post-address (address))
             (format nil "link: on · ~a~@[~a~]" (address) (web-note)))
      (format nil "link: on · ~:[the relay isn't answering yet~;no line yet — ~:*~a~]; ~
                   this machine keeps trying~@[~a~]"
              *failure* (web-note))))

(defun turn-off ()
  (keep "on" nil)
  (nlk:when-let (line *line*) (sever line))
  ;; A link turned on again starts its dialling afresh.
  (setf *failures* 0 *failure* nil *next-dial* 0)
  "link: off · nothing reaches this machine through the relay")

(defun state ()
  "Whether the link is :ON (a line the relay welcomed), :CONNECTING (turned on,
no line yet), :FAILING (turned on, and the dials keep failing) or :OFF."
  (cond ((linked-p) :on) ((not (kept "on")) :off) (*failure* :failing) (t :connecting)))

(defun seconds-to-dial ()
  (max 0 (- *next-dial* (get-universal-time))))

(defun failure-text ()
  "Why there is no line, and when it is tried again, or NIL."
  (nlk:when-let (why *failure*)
    (format nil "no line: ~a; trying again in ~d s, /link retry tries now" why (seconds-to-dial))))

(defun ask-text (ask)
  "The browser asking, as a status line says it."
  (format nil "a browser is asking: ~a (~a~@[ in ~a~], ~d s left) — /link allow ~a"
          (code-text (getf ask :code)) (getf ask :agent)
          (and (plusp (length (getf ask :place))) (getf ask :place))
          (max 0 (- (getf ask :expires) (now))) (getf ask :code)))

(defun status ()
  "Whether the link is on, where, who may use it, why it has no line, and the
browser asking now."
  (let ((count (length (browsers))) (ask (live-ask)))
    (format nil "link: ~a~@[ · ~a~] · ~d browser~:p allowed~@[ · ~a~]~@[ · ~a~]"
            (ecase (state) (:on "on") (:connecting "on, connecting") (:failing "on, failing") (:off "off"))
            (address) count (failure-text) (and ask (ask-text ask)))))

(defun retry ()
  "Dial the relay now rather than when the wait is out."
  (cond ((not (kept "on")) "link: off · /link on turns it on")
        ((linked-p) (format nil "link: on · ~a" (address)))
        (t (retry-now)
           (if (await-line 3)
               (format nil "link: on · ~a" (address))
               (format nil "link: still no line~@[ — ~a~]; trying again in ~d s"
                       *failure* (seconds-to-dial))))))

(defun allow (typed &aux (code (remove #\Space (or typed ""))))
  "Allow the browser asking, by the code TYPED as either screen shows it: 482
913 or 482913."
  (let ((ask (answer-ask :allowed code)))
    (cond (ask (format nil "link: allowed ~a~@[ in ~a~]" (getf ask :agent)
                       (and (plusp (length (getf ask :place))) (getf ask :place))))
          ((zerop (length code)) "link: allow takes the code the browser shows: /link allow 482 913")
          ((live-ask) (format nil "link: a browser is asking, but not with ~a — check the code on its screen"
                              (if (= 6 (length code)) (code-text code) code)))
          (t "link: no browser is asking"))))

(defun deny ()
  (if (answer-ask :denied) "link: denied" "link: no browser is asking"))

(defun remove-one (id)
  (if (and id (remove-browser id))
      (progn (cut-browser id)
             (format nil "link: ~a can no longer open this machine" id))
      (format nil "link: no allowed browser ~a; /link lists them" (or id ""))))

(defun panel ()
  "The dialog /link answers with: the ask waiting, then every allowed browser."
  (let* ((ask (live-ask))
         (rows (append
                (when ask
                  (list (list :key "asking" :mark "• " :mark-tone :accent
                              :label (format nil "~a  ~a" (code-text (getf ask :code)) (getf ask :agent))
                              :detail (format nil "~@[~a · ~]asking, ~d s left"
                                              (and (plusp (length (getf ask :place))) (getf ask :place))
                                              (- (getf ask :expires) (get-universal-time)))
                              :value (format nil "/link allow ~a" (getf ask :code)))))
                (mapcar (lambda (browser)
                          (nlk:with-json ((id :string "id") (agent :string "agent")
                                          (place :string "place") (allowed :string "allowed"))
                              browser
                            (list :key "allowed" :mark "  " :mark-tone :muted
                                  :label agent
                                  :detail (format nil "~@[~a · ~]allowed ~a · ~a"
                                                  (and (plusp (length place)) place)
                                                  (or (ago allowed) "") id)
                                  :value (format nil "/link remove ~a" id))))
                        (browsers)))))
    (nle:list-dialog
     :title "Link"
     :context (ecase (state)
                (:on (format nil "on · ~a" (address)))
                (:connecting "on · connecting")
                (:failing (format nil "on · ~a" (failure-text)))
                (:off "off"))
     :picture (nlk:when-let (code (address-code))
                (list :rows code :label "The address's QR code"))
     :rows rows
     :empty-label (cond ((eq (state) :on)
                         "No browser allowed yet · scan the code with a phone, ask there, allow it here")
                        ((kept "on") "No browser allowed yet · open the address and allow it here")
                        (t "Off · /link on turns it on"))
     :hint "esc close  enter allow or remove  /link on or /link off switches it")))

(defparameter +usage+
  "/link [status | on | off | retry | allow CODE | deny | remove ID]")

(defun run-slash (args session-id)
  "(values TEXT DIALOG) for one /link."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words args))
         (head (first words)))
    (flet ((shown (text) (values text (panel))))
      (cond
        ((or (null head) (string= head "status")) (shown (status)))
        ((string= head "on") (shown (turn-on)))
        ((string= head "off") (shown (turn-off)))
        ((string= head "retry") (shown (retry)))
        ;; The code as the screens group it, 482 913, is two words.
        ((string= head "allow") (allow (format nil "~{~a~}" (rest words))))
        ((string= head "deny") (deny))
        ((string= head "remove") (remove-one (second words)))
        (t (format nil "link: unknown subcommand ~a; usage ~a" head +usage+))))))

;;; --- the page's route -----------------------------------------------------------

(defun link-json (&key here text)
  "What PANEL shows, as the operator's page reads it: the state, the address
and, while the link is on, its QR code, the relay, why the dials keep failing
while they do (the words, the tries, the seconds to the next), the browser
asking with its code as it reads aloud and the seconds it has left, every
browser allowed -- never its cookie's digest -- and HERE, the id of the allowed
browser the request came through when it came down the line; TEXT is what a
verb just said."
  (let ((ask (live-ask)))
    (nlk:json-object
     "state" (string-downcase (state))
     :opt "address" (address)
     :opt "qr" (nlk:when-let (code (address-code)) (coerce code 'vector))
     "relay" (setting :relay)
     :opt "failure" (nlk:when-let (why *failure*)
                      (nlk:json-object "why" why "tries" *failures* "next_try_seconds" (seconds-to-dial)))
     :opt "ask" (and ask (nlk:json-object "code" (code-text (getf ask :code)) "agent" (getf ask :agent)
                                          "place" (getf ask :place)
                                          "seconds_left" (max 0 (- (getf ask :expires) (now)))))
     "browsers" (map 'vector (lambda (browser)
                               (nlk:with-json ((id :string "id") (agent :string "agent")
                                               (place :string "place") (allowed :string "allowed"))
                                   browser
                                 (nlk:json-object "id" id "agent" agent "place" place "allowed" allowed)))
                     (browsers))
     :opt "here" here
     :opt "text" text)))

(defun link-route (env &aux (query (quri:url-decode-params (or (getf env :query-string) ""))))
  "/api/link: GET answers LINK-JSON; POST ?op=on, off, retry, allow&code=CODE,
deny or remove&id=ID does what /link does, through the same verbs, and answers
LINK-JSON with the verb's line as its text."
  (flet ((param (name) (cdr (assoc name query :test #'string=))))
    (let* ((op (and (eq (getf env :request-method) :post) (param "op")))
           (text (cond ((null op) nil)
                       ((string= op "on") (turn-on))
                       ((string= op "off") (turn-off))
                       ((string= op "retry") (retry))
                       ((string= op "allow") (allow (param "code")))
                       ((string= op "deny") (deny))
                       ((string= op "remove") (remove-one (param "id")))
                       (t (error "link: no op ~s; on, off, retry, allow, deny or remove" op)))))
      (link-json :here (gethash +via+ (getf env :headers)) :text text))))

;;; --- the entry ---------------------------------------------------------------------

(defun install ()
  (setf *state-file* (nlk:home "link/state.json") *failures* 0 *failure* nil *next-dial* 0)
  (with-link-lock (setf *state* (read-state)))
  (start-line)
  (when (kept "on")
    (nle:keep-running "this machine's page at its link address"))
  (nle:on-stop (lambda ()
                 (stop-line)
                 (with-link-lock (setf *state* nil *ask* nil))
                 (setf *state-file* nil))))

(nle:define-cell link
  (:section ("link")
    (:guide "relay is where this machine's line goes; /link on turns it on")
    ("relay" :string :default "wss://uplink.nodecode.ai/line"
             :doc "the relay this machine dials to put its page within reach"))
  (:start #'install)
  (:route "/api/link" #'link-route)
  (:command "link" 'run-slash
            :description "This machine's page from anywhere: /link shows it and the browsers you allowed; on, off, allow, deny, remove"
            :argument-hint "on | off | allow CODE | deny | remove ID | retry"
            :session nil))
