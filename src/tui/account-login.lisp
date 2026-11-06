;;;; account-login.lisp --- this machine signed in to its nodecode.ai account, and the account's meter.
;;;; SPDX-License-Identifier: MIT
;;;; Protocol owners: nodecode-website web/src/app/api/link/{route,[id]/route}.ts (the claim, its
;;;; `login' mode and the /cli page the claim names) and crates/gate (GET /v1/usage).
;;;; `nodecode login' and `nodecode machine link' are plain stdout: no session, store or TUI. Poll
;;;; secrets stay in memory; the granted key has one authority, SAVE-PROVIDER-API-KEY's auth.json.
;;;; Provider wiring uses CONFIG-SET; a sign-in never replaces an existing default model or an
;;;; unrelated provider key. The meter is read by the organism alone (ACCOUNT-USAGE), served at
;;;; /api/gateway/account, and drawn by each shell (*ACCOUNT-METER*).
(in-package #:nodecode.evolved)

(defparameter +account-modes+ '("subscription" "api")
  "What `nodecode login' and /login sign in for: a plan's monthly requests, or API credit paid
by the token. The gate decides who pays each request (the plan while it has requests left, then
the balance); the mode decides what the sign-in asks of the person and what the meter leads with.")

(defun account-mode-word (text &aux (word (string-trim '(#\Space #\Tab #\Return) (or text ""))))
  "The mode TEXT names -- subscription (1, s, sub, plan) or api (2, a, credit, credits) -- or NIL."
  (cond ((member word '("subscription" "1" "s" "sub" "plan") :test #'string-equal) "subscription")
        ((member word '("api" "2" "a" "credit" "credits") :test #'string-equal) "api")))

;;; --- the claim: nodecode login and nodecode machine link -----------------------

(defun account-exchange (method url bearer timeout max-bytes &key body json (host "nodecode.ai"))
  "One bounded exchange with nodecode.ai at URL, BEARER its authorization, JSON a content-type
header, BODY the JSON it sends: (values REPLY STATUS), REPLY NIL when it is no JSON; no POST
retries. HOST is the name a timeout says did not answer."
  (handler-case
      (bind (((bytes status _ truncated)
              (sb-sys:with-deadline (:seconds timeout)
                (nlk:http method url
                          :content (and body (shasht:write-json body nil))
                          :headers (with-user-agent
                                    (append (and json '(("content-type" . "application/json")))
                                            (when bearer (list (cons "authorization" (format nil "Bearer ~a" bearer))))))
                          :timeout timeout :max-bytes max-bytes :binary t))))
        (values (and (not truncated) (ignore-errors (nlk:decode-json bytes))) status))
    (sb-sys:deadline-timeout () (error "~a did not answer within ~ds." host timeout))))

(defun account-login-request (method path body poll timeout)
  "One bounded exchange with the account machine-link API; no POST retries."
  (multiple-value-bind (reply status)
      (account-exchange method (concatenate 'string "https://nodecode.ai" path) poll timeout 8192 :body body :json t)
    (unless (hash-table-p reply)
      (error "nodecode.ai answered with an invalid account-link response (HTTP ~d)." status))
    (values reply status)))

(defun account-login-save (reply)
  "Validate the grant, save its key once -- and the free key beside it when the grant hands one
over (+FREE-PROVIDER+) -- and wire their providers without changing the model."
  (with-json ((provider :string "provider") (key :string "api_key") (base :string "base_url")
              (free :string "free_api_key"))
      reply
    (unless (and (equal provider "nodecode") key (plusp (length key))
                 base (equal "https" (ignore-errors (quri:uri-scheme (quri:uri base))))
                 (ignore-errors (quri:uri-host (quri:uri base))))
      (error "nodecode.ai answered with an invalid account grant; nothing was saved."))
    ;; The key is recoverable in its canonical file even if the config splice fails.
    (save-provider-api-key provider key)
    (config-set (list "providers" provider) (list :sdk "openai-completions" :base_url base))
    ;; A pre-existing inline key would outrank auth.json's newly granted key.
    (when (json-value (read-shared-config) :any "providers" provider "api_key")
      (config-set (list "providers" provider "api_key") :remove))
    ;; Allow with a free key on nodecode.ai's /cli hands over the free key beside the account's:
    ;; a provider of its own, so /models offers Cyber v1 free and paid. A sign-in without one
    ;; takes back the free key an earlier one saved.
    (if (and free (plusp (length free)))
        (progn
          (save-provider-api-key +free-provider+ free)
          (config-set (list "providers" +free-provider+) (list :sdk "openai-completions" :base_url base)))
        (account-forget-free-key))
    provider))

(defun account-forget-free-key ()
  "Take the free key (+FREE-PROVIDER+) out of auth.json and its provider out of the config.
=> whether there was either."
  (let ((key (account-credential +free-provider+))
        (wired (json-value (read-shared-config) :object "providers" +free-provider+)))
    (when key (save-provider-api-key +free-provider+ nil))
    (when wired (config-set (list "providers" +free-provider+) :remove))
    (and (or key wired) t)))

(defun account-grant-provider (reply model)
  "The provider a sign-in's default MODEL runs on: the free key's when REPLY hands one over and
names MODEL itself (the free model), else the account key's."
  (if (and (plusp (length (or (json-value reply :string "free_api_key") "")))
           (equal model (json-value reply :string "model")))
      +free-provider+
      (json-value reply :string "provider")))

(defun account-login-preflight ()
  "Refuse before a one-time code is spent when the local stores could not take the key."
  (when-let (reason (nth-value 1 (read-shared-config)))
    (error "Fix the shared config before linking this machine: ~a" reason))
  (unless *shared-config-path* (error "Account login needs a writable shared config path."))
  (read-auth-file *auth-file-path*))

(defun account-checked (request method path body poll timeout)
  "REQUEST's reply to one exchange, refused unless it is a JSON object answered 200."
  (multiple-value-bind (reply status) (funcall request method path body poll timeout)
    (unless (hash-table-p reply) (error "nodecode.ai answered with an invalid account-link response."))
    (unless (= status 200)
      (error "Account login: ~a (HTTP ~d)."
             (or (json-value reply :string "error") "nodecode.ai refused the request") status))
    reply))

(defun account-claim (code mode request)
  "Claim CODE for this machine -- or, with none, be given one -- for MODE's sign-in when MODE is
one of +ACCOUNT-MODES+: the server's checked answer as (:id :poll :code :confirm :life :every).
CONFIRM is the page the person opens, the server's to name: the Machines page for a code, the
sign-in page with the code already in it for a mode."
  (with-json ((id :string "request") (poll :text "poll") (shown :text "code")
              (confirm :string "confirm") (life :integer "expires_in") (every :integer "every"))
      (account-checked request :post "/api/link"
                       ;; keys 2: this machine keeps a free key apart from the account's
                       ;; (+FREE-PROVIDER+), so a sign-in with one hands over both.
                       (json-object "code" (or code :null) :opt "login" mode "keys" 2 "host" (machine-instance)
                                    "os" (format nil "~a ~a" (software-type) (software-version))
                                    "arch" (machine-type) "release" (effective-version))
                       nil 30)
    (unless (and id (ppcre:scan "^[a-fA-F0-9-]{36}$" id) poll shown
                 confirm (uiop:string-prefix-p "https://" confirm)
                 life (< 0 life 3601) every (<= 1 every 60))
      (error "nodecode.ai answered with an invalid account claim; nothing was saved."))
    (list :id id :poll poll :code shown :confirm confirm :life life :every every)))

(defun account-clock ()
  "Seconds on the monotonic clock a claim's deadline is kept on."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun account-await (claim &key (request #'account-login-request) (clock #'account-clock)
                                 (wait #'sleep) (again "nodecode login"))
  "Ask after CLAIM until the person decides, answering the linked reply. AGAIN is the command a
dead claim's sentence tells the person to run."
  (let ((deadline (+ (funcall clock) (getf claim :life)))
        (path (concatenate 'string "/api/link/" (getf claim :id))))
    (loop
      (let ((left (- deadline (funcall clock))))
        (unless (plusp left) (error "Account login code expired; run ~a again." again))
        (let* ((reply (account-checked request :get path nil (getf claim :poll) (min 30 left)))
               (state (json-value reply :string "state")))
          (cond
            ((equal state "linked") (return reply))
            ((equal state "waiting") nil)
            ((equal state "refused") (error "Account login was refused on nodecode.ai; nothing was saved."))
            ((equal state "expired") (error "Account login code expired; run ~a again." again))
            ((equal state "gone") (error "Account login key was already collected; run ~a again." again))
            (t (error "nodecode.ai answered with an invalid account-link state; nothing was saved.")))))
      (funcall wait (max 0 (min (getf claim :every) (- deadline (funcall clock))))))))

(defun account-login (code &key mode (output *standard-output*)
                               (request #'account-login-request)
                               (clock #'account-clock)
                               (wait #'sleep)
                               (open #'open-target)
                               (usage-request #'account-usage-request))
  "Sign this machine in: with MODE, in a browser -- the page opened where there is a display,
printed always -- then the account's meter; with CODE or neither and no MODE, the Machines-page
link. Answers the name the machine was let in under."
  (account-login-preflight)
  (let* ((claim (account-claim code mode request))
         (url (getf claim :confirm))
         (again (if mode "nodecode login" "nodecode machine link")))
    (if mode
        ;; Claude Code's sign-in, without its paste: the page already carries the code, and this
        ;; machine asks after the claim, so a browser on another device signs it in the same way.
        (let ((opened (eq t (ignore-errors (funcall open url)))))
          (format output "~&~:[Open this page in a browser, on this machine or any other~;Your browser is open at nodecode.ai~]:~%  ~a~%  Code: ~a (the page shows the same code)~%Sign in with your nodecode.ai account and allow this machine. Waiting (Ctrl+C cancels).~%"
                  opened url (getf claim :code)))
        (format output "~&Link this machine to your account:~%  ~a~%  Code: ~a~%Check the machine shown there and approve it. Waiting for approval (Ctrl+C cancels).~%"
                url (getf claim :code)))
    (finish-output output)
    (let* ((reply (account-await claim :request request :clock clock :wait wait :again again))
           (name (json-value reply :string "name"))
           (saved (home-abbreviated (namestring (pathname *auth-file-path*)))))
      (account-login-save reply)
      (save-machine-link (getf claim :id) (getf claim :poll))
      (let* ((lead (account-default-model reply :request usage-request))
             (provider (account-grant-provider reply lead))
             (free (json-value reply :string "free_api_key")))
        (when lead
          (persist-shared-model-pin "default_model" provider lead))
        (cond
          (mode
           (format output "Signed in~@[ on ~a~]. ~:[Account key~;Account key and free key~] saved to ~a.~%" name free saved)
           (account-print-usage output mode (json-value reply :string "api_key")
                                (json-value reply :string "base_url") usage-request lead provider free))
          (t
           (format output "Linked~@[ as ~a~]. Account key saved to ~a.~%With /link turned on in nodecode, the Machines page shows its address.~%"
                   name saved)
           (account-next-step output lead provider))))
      (finish-output output)
      name)))

(defun account-next-step (output lead &optional (provider +own-provider+))
  "What a sign-in prints last: the default model it chose, LEAD on PROVIDER, or that /models
chooses one."
  (if lead
      (format output "Default model: ~a ~a. Run nodecode to start.~%" provider lead)
      (format output "Run nodecode, then /models to choose a nodecode model.~%")))

(defun account-print-usage (output mode key base request &optional lead (provider +own-provider+) free)
  "The meter a fresh sign-in prints -- FREE, a free key handed over beside KEY, its line too --
what MODE still lacks -- a plan or credit -- and the next step: LEAD, the default model the
sign-in chose on PROVIDER, or /models."
  (handler-case
      (let ((usage (account-usage-read key base :request request)))
        (dolist (line (account-usage-lines usage)) (format output "~a~%" line))
        (when free
          (format output "~a~%" (account-free-line (account-usage-read free base :request request))))
        (cond
          ((and (equal mode "subscription") (not (json-value usage :object "plan")))
           ;; No plan is sold now (nodecode.ai/pricing is per token), so the plan
           ;; a person lacks is not one to buy: the account runs on API credit.
           (format output "This account holds no plan, and plans are not sold now: it runs on API credit, added at https://nodecode.ai/account/top-up~%"))
          ((and (equal mode "api") (not (account-balance-rows usage)))
           (format output "This account has no API credit yet: add some at https://nodecode.ai/account/top-up~%"))))
    (error (condition)
      (format output "The account's meter could not be read yet: ~a~%" condition)))
  (account-next-step output lead provider))

(defun account-ask-mode (&key (ask #'terminal-answer))
  "The mode `nodecode login' signs in for, asked on the terminal; refused where none can ask."
  (loop repeat 3
        for reply = (funcall ask (format nil "Sign in to nodecode.ai for:~%  1  Subscription: your Founders plan's requests (Lite, Base or Pro)~%  2  API credits: pay per token from your balance~%Choose 1 or 2:"))
        do (cond ((null reply)
                  (error "nodecode login asks what to sign in for and no terminal can answer: run nodecode login --subscription or nodecode login --api."))
                 ((account-mode-word reply) (return (account-mode-word reply))))
        finally (error "nodecode login takes 1 (subscription) or 2 (API credits).")))

(defun account-login-main (mode)
  "`nodecode login [--subscription | --api]': the browser sign-in, with no store or terminal UI."
  (account-login nil :mode (or mode (account-ask-mode)))
  (uiop:quit 0))

;;; --- the machine's record on the account ----------------------------------------
;;; A link leaves its request id and poll secret in auth.json ("machine_link"): the secret that
;;; proved the machine while it waited proves it after, when the machine tells the account what
;;; changed about it -- a cell its address at a relay, a new release its version.

(defun save-machine-link (id poll)
  "Keep the link this machine was let in by: its request ID and POLL secret, in auth.json."
  (update-auth-file (lambda (auth)
                      (setf (gethash "machine_link" auth) (json-object "request" id "poll" poll)))))

(defun machine-update (facts &key (request #'account-login-request))
  "Tell the account what changed about this machine: FACTS, a JSON object of any of host, os,
arch, release and relay (null clears it), sent with its link's own secret (POST /api/link/<id>).
=> the account's reply, with the machine's name and address, or NIL when this home was never
linked. A refusal signals, naming it."
  (let ((link (json-value (read-auth-file *auth-file-path*) :object "machine_link")))
    (with-json ((id :string "request") (poll :text "poll")) link
      (when (and id poll)
        (account-checked request :post (concatenate 'string "/api/link/" id) facts poll 15)))))

(defun machine-link-main (code)
  "`nodecode machine link [CODE]': the Machines-page link, with no store or terminal UI."
  (account-login code)
  (uiop:quit 0))

;;; --- the meter: what the account has left --------------------------------------

(defun account-usage-request (url key timeout)
  "One bounded GET of the gate's usage with KEY: (values REPLY STATUS), REPLY NIL when it is no JSON."
  (account-exchange :get url key timeout 16384 :host "api.nodecode.ai"))

(defun account-usage-read (key base &key (request #'account-usage-request))
  "The account's usage as the gate at BASE answers it for KEY, or an error saying why not."
  (multiple-value-bind (reply status)
      (funcall request (format nil "~a/usage" (string-right-trim "/" base)) key 10)
    (cond
      ((and (eql status 200) (hash-table-p reply) (equal "usage" (json-value reply :string "object")))
       reply)
      ((member status '(401 403))
       (error "nodecode.ai refused this machine's key (HTTP ~d)~@[: ~a~]. Sign in again with /login or nodecode login."
              status (or (json-value reply :string "error" "message") (json-value reply :string "error"))))
      (t (error "api.nodecode.ai answered its usage with HTTP ~d." status)))))

(defun account-balance-rows (usage)
  "USAGE's balances above zero, as (CURRENCY . MICROS), in the gate's order."
  (loop for row across (or (json-value usage :array "balance") #())
        for currency = (json-value row :string "currency")
        for micros = (json-value row :number "micros")
        when (and currency micros (plusp micros)) collect (cons currency micros)))

(defun account-money (row &aux (currency (string-downcase (car row))))
  "A balance row as the person reads it: EUR 12.40 as €12.40, USD as $."
  (format nil "~a~,2f" (cond ((equal currency "eur") "€") ((equal currency "usd") "$")
                              (t (format nil "~:@(~a~) " currency)))
          (/ (cdr row) 1000000)))

(defun account-count (n &key short)
  "N requests as a count: 41,188, or with SHORT 41.2k from ten thousand up."
  (let ((n (max 0 (floor n))))
    (if (and short (>= n 10000))
        (format nil "~,1fk" (/ (floor n 100) 10))
        (format nil "~:d" n))))

(defun account-ymd (iso)
  "(YEAR MONTH DAY) of the day an ISO timestamp names, or NIL for anything else."
  (and (stringp iso)
       (ppcre:register-groups-bind ((#'parse-integer year month day)) ("^(\\d{4})-(\\d{2})-(\\d{2})" iso)
         (list year month day))))

(defun account-day (iso)
  "The day an ISO timestamp names, as Nov 3; NIL for anything else."
  (destructuring-bind (&optional year month day) (account-ymd iso)
    (declare (ignore year))
    (when (and month (<= 1 month 12))
      (format nil "~a ~d" (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
              day))))

(defun account-plan-tone (left requests)
  "How a plan with LEFT of REQUESTS reads: SUCCESS, TOOL under a tenth left, DANGER at none."
  (cond ((not (plusp left)) :danger)
        ((and requests (plusp requests) (< (/ left requests) 1/10)) :tool)
        (t :success)))

(defun account-plan-name (plan)
  "PLAN's name as the buyer knows it from nodecode.ai: the gate's label (Founders Base), else its
id capitalized."
  ;; The id alone read Founders for Base, Starter for Lite and Plus for Pro (checkout audit,
  ;; 2026-10-22): the gate's plan.name is its id, plan.label the product's name.
  (or (json-value plan :string "label")
      (string-capitalize (or (json-value plan :string "name") "plan"))))

(defun account-lead-model (key base &key (request #'account-usage-request))
  "nodecode.ai's lead chat model as the gate at BASE lists it for KEY -- the first model of kind
chat, in the gate's own order -- or NIL when its listing names none or cannot be read."
  (multiple-value-bind (reply status)
      (ignore-errors (funcall request (format nil "~a/models" (string-right-trim "/" base)) key 10))
    (when (and (eql status 200) (hash-table-p reply))
      (loop for entry across (or (json-value reply :array "data") #())
            when (equal "chat" (json-value entry :string "kind"))
              return (json-value entry :string "id")))))

(defun account-default-model (reply &key (request #'account-usage-request))
  "The model a sign-in makes the default while none is chosen: the one REPLY, the grant, names,
else nodecode.ai's lead chat model (ACCOUNT-LEAD-MODEL); NIL when a default model is already
chosen or nothing names one."
  ;; Signed in with no model chosen, a new buyer met `1 setup issue . run /setup to add a
  ;; model' and a list opening on Anthropic (checkout audit, 2026-10-22). A model chosen before
  ;; stays: a sign-in never replaces it.
  (unless (shared-model-pin)
    (with-json ((model :string "model") (key :string "api_key") (base :string "base_url")) reply
      (or model (and key base (account-lead-model key base :request request))))))

(defun account-meter (usage)
  "The footer's badge for USAGE, as (values LABEL VALUE TONE): the plan's name and its requests
left while the plan pays, the credit once it does, and the tone of what pays -- SUCCESS, TOOL when
the plan runs low or a spent plan's credit pays, DANGER when nothing can pay."
  (let* ((plan (json-value usage :object "plan"))
         (name (and plan (account-plan-name plan)))
         (left (and plan (or (json-value plan :number "left") 0)))
         (requests (and plan (json-value plan :number "requests")))
         (money (first (account-balance-rows usage)))
         (pays (json-value usage :string "pays")))
    (cond
      ;; A free key draws on neither: Cyber v1's free tier, once the account has paid its top-up.
      ((equal pays "free") (values "Free key" "Cyber v1" :success))
      ((equal pays "free_locked") (values "Free key" "top up first" :danger))
      ((and plan (equal pays "plan"))
       (values name (account-count left :short t) (account-plan-tone left requests)))
      ((and money (equal pays "balance"))
       (if plan
           (values name (format nil "credit ~a" (account-money money)) :tool)
           (values "Credit" (account-money money) :success)))
      (plan (values name "0 left" :danger))
      (t (values "nodecode.ai" "no plan or credit" :danger)))))

(defun account-usage-lines (usage)
  "USAGE as the lines /usage and a sign-in print: the plan, the credit, what pays now."
  (let* ((plan (json-value usage :object "plan"))
         (rows (account-balance-rows usage))
         (pays (json-value usage :string "pays")))
    (remove nil
            (list
             (if plan
                 (format nil "~a plan: ~a of ~a requests left~@[, ends ~a~]~@[ · ~a~]"
                         (account-plan-name plan)
                         (account-count (or (json-value plan :number "left") 0))
                         (account-count (or (json-value plan :number "requests") 0))
                         (account-day (json-value plan :string "ends_at"))
                         (when-let (threads (json-value plan :integer "threads"))
                           (format nil "~d of ~d running" (or (json-value plan :integer "running") 0) threads)))
                 "No plan.")
             (format nil "API credit: ~:[none~;~:*~{~a~^, ~}~]" (mapcar #'account-money rows))
             (format nil "Paying with: ~a"
                     (cond ((equal pays "free") "a free key: Cyber v1 from this CLI, 256K context, 2 at once")
                           ((equal pays "free_locked") "nothing yet: a free key works once the account has topped up at https://nodecode.ai/account/top-up")
                           ((equal pays "plan") "the plan")
                           ((equal pays "balance") "API credit")
                           (t "nothing yet: subscribe at https://nodecode.ai/pricing or add credit at https://nodecode.ai/account/top-up")))))))

(defun account-credential (&optional (provider +own-provider+))
  "The saved nodecode.ai key of PROVIDER -- the account's, or the free key's (+FREE-PROVIDER+) --
and the API base it is for, as (values KEY BASE); NIL with none."
  ;; The chain's last tier is the "public" stand-in: a providers.nodecode entry with no key, as
  ;; a sign-out leaves it, is no account.
  (let* ((credential (ignore-errors (resolve-provider-credential provider)))
         (key (and credential (not (eq :public (getf credential :source)))
                   (ignore-errors (credential-key credential)))))
    (when (and (stringp key) (plusp (length key)))
      (values key (or (ignore-errors (provider-base-url provider)) "https://api.nodecode.ai/v1")))))

(defun account-free-line (usage)
  "The line /usage and a sign-in print for the free key's USAGE, beside the account key's lines."
  (format nil "Free key: ~a"
          (if (equal "free_locked" (json-value usage :string "pays"))
              "Cyber v1 at no cost once the account has topped up at https://nodecode.ai/account/top-up"
              "Cyber v1 at no cost from this CLI, 256K context, 2 at once (nodecode-free in /models)")))


(defvar *account-usage* nil
  "The account's meter as this organism last read it: (:usage HT :read-at UT) or (:error TEXT
:read-at UT), NIL before the first read and when this home holds no nodecode.ai key. Live-only.")

(defvar *account-free-usage* nil
  "The free key's meter (+FREE-PROVIDER+) as *ACCOUNT-USAGE* keeps the account key's. Live-only.")

(defun account-usage-state (provider held max-age request)
  "PROVIDER's key's meter: HELD while younger than MAX-AGE seconds, else the gate's answer read
again -- (:usage HT :read-at UT) or (:error TEXT :read-at UT); NIL with no key."
  ;; A failed read is kept as long as a good one, so a gate that is down is asked once a
  ;; MAX-AGE, not once a frame.
  (multiple-value-bind (key base) (account-credential provider)
    (let ((now (get-universal-time)))
      (cond
        ((null key) nil)
        ((and held (< (- now (getf held :read-at)) max-age)) held)
        (t (handler-case (list :usage (account-usage-read key base :request request) :read-at now)
             (error (condition) (list :error (princ-to-string condition) :read-at now))))))))

(defun account-usage (&key (max-age 30) (request #'account-usage-request))
  "*ACCOUNT-USAGE*, read again when older than MAX-AGE seconds; NIL with no nodecode.ai key."
  (setf *account-usage* (account-usage-state +own-provider+ *account-usage* max-age request)))

(defun account-free-usage (&key (max-age 30) (request #'account-usage-request))
  "*ACCOUNT-FREE-USAGE*, read again when older than MAX-AGE seconds; NIL with no free key."
  (setf *account-free-usage* (account-usage-state +free-provider+ *account-free-usage* max-age request)))

(defvar *account-login* nil
  "The organism's sign-in /login started: (:state :waiting | :signed-in | :failed, :mode, :url,
:code, :opened, :text, :at). Live-only; the shell that started it watches it here.")

(defun broadcast-account-changed (account &optional text)
  "Tell every shell attached to this organism that its home's nodecode.ai sign-in moved:
ACCOUNT is \"signed_out\" or \"signed_in\", TEXT the words that say so. => how many were told."
  ;; It rides model_changed, session-less, and not a frame kind of its own: a shell of an older
  ;; build takes a kind it does not know for a permanent transport error, and reads this one as
  ;; what it also is -- the models a turn reaches moved with the key -- and reads its footer
  ;; again. A shell of this build reads `account' as well (LAND-ACCOUNT-CHANGE).
  (nth-value 1 (broadcast-organism "model_changed" (json-object "account" account :opt "text" text))))

(defun account-login-start (mode &key (request #'account-login-request) (open #'open-target))
  "Begin MODE's browser sign-in for this organism's home -- claim, open the page where this
machine has a display, ask after it on a thread of its own -- and answer *ACCOUNT-LOGIN* at once.
A sign-in already waiting is answered again, not claimed twice."
  (unless (member mode +account-modes+ :test #'equal)
    (error "/login signs in for subscription or api, not ~s." mode))
  (cond ((eq :waiting (getf *account-login* :state)) *account-login*)
        (t (account-login-preflight)
         (let* ((claim (account-claim nil mode request))
                (url (getf claim :confirm))
                (opened (eq t (ignore-errors (funcall open url))))
                (login (list :state :waiting :mode mode :url url :code (getf claim :code)
                             :opened opened :text nil :at (get-universal-time))))
           (setf *account-login* login)
           (flet ((settle (state text)
                    (setf *account-login* (list* :state state :text text
                                                 (alexandria:remove-from-plist login :state :text)))))
             (spawn "nodecode-account-login"
               (handler-case
                   (let* ((reply (account-await claim :request request :again "/login"))
                          (name (json-value reply :string "name")))
                     (account-login-save reply)
                     (setf *account-usage* nil *account-free-usage* nil)
                     (let* ((lead (let ((id (account-default-model reply)))
                                    ;; the model a turn runs on from now: a refused pick sets none
                                    (and id (ignore-errors
                                             (select-default-model :provider (account-grant-provider reply id)
                                                                   :model id))
                                         id)))
                            (state (account-usage :max-age 0))
                            (meter (when-let (usage (getf state :usage))
                                     (multiple-value-bind (label value) (account-meter usage)
                                       (format nil "~a ~a" label value))))
                            (text (format nil "Signed in to nodecode.ai~@[ on ~a~]~@[ · ~a~]~@[ · default model ~a~]"
                                          name meter lead)))
                       (settle :signed-in text)
                       ;; every shell on the home reads its footer's meter again
                       (broadcast-account-changed "signed_in" text)
                       (notice text)))
                 (error (condition)
                   (let ((text (format nil "nodecode.ai sign-in: ~a" condition)))
                     (settle :failed text)
                     (notice text :level :warning))))))
           login))))

(defun account-json (&aux (state (account-usage)) (usage (getf state :usage)) (login *account-login*)
                          (free (getf (account-free-usage) :usage)))
  "The account's read model a shell and the web page draw: whether this home is signed in, the
meter's words and tone -- the free key's apart, in `free' -- the lines /usage prints, the gate's
usage as it came, and the sign-in in flight."
  (flet ((meter (usage)
           (multiple-value-bind (label value tone) (if usage (account-meter usage) (values nil nil nil))
             (if label
                 (json-object "label" label "value" value "tone" (string-downcase (symbol-name tone)))
                 :null))))
    (make-json-object
     "signed_in" (if (or state free) t :false)
     "meter" (meter usage)
     "free" (meter free)
     "lines" (coerce (append (cond (usage (account-usage-lines usage))
                                   ((getf state :error) (list (getf state :error))))
                             (and free (list (account-free-line free))))
                     'vector)
     "error" (or (getf state :error) :null)
     "usage" (or usage :null)
     "login" (if login
                 (json-object "state" (string-downcase (symbol-name (getf login :state)))
                              "mode" (getf login :mode) "url" (getf login :url) "code" (getf login :code)
                              "opened" (if (getf login :opened) t :false)
                              "text" (or (getf login :text) :null))
                 :null))))

(define-gateway-route handle-gateway-account-http (env)
  "GET /api/gateway/account -- ACCOUNT-JSON: the account's meter, read through the organism's
cache of the gate's usage, and the sign-in /login has in flight."
  (:route :get "/api/gateway/account" "account")
  ;; ?fresh=1 asks the gate again whatever the cache's age: /usage opened on a top-up or a plan
  ;; bought while this shell is open shows it (checkout audit, 2026-10-22).
  (when (equal "1" (cdr (assoc "fresh" (gateway-query-parameters env) :test #'string=)))
    (account-usage :max-age 0)
    (account-free-usage :max-age 0))
  (account-json))

(define-gateway-route handle-gateway-account-login-http (env)
  "POST /api/gateway/account/login {mode} -- begin the browser sign-in for MODE (subscription or
api) on this organism's home, and answer ACCOUNT-JSON with the page and code in `login'."
  (:route :post "/api/gateway/account/login" "account sign-in")
  (let ((mode (json-value (gateway-request-json env) :string "mode")))
    (unless (member mode +account-modes+ :test #'equal)
      (nlk::protocol-refuse :invalid_request "mode must be \"subscription\" or \"api\""))
    (account-login-start mode)
    (account-json)))

;;; --- signing out: /logout and `nodecode logout' ---------------------------------
;;; The sign-in's mirror: this home forgets the account's key and the link's secret, so no turn,
;;; meter or machine report reaches the account from here. The key itself lives on the account
;;; until the machine is removed on its Machines page; the words say so.

(defun account-logout ()
  "Sign this home out of nodecode.ai: take the account's key out of auth.json (and out of its own
secrets file, and an inline providers.nodecode.api_key), forget the machine link, and drop the
meter and any sign-in in flight. => the words that say what happened, and whether a key or a
link went; signed out already, it says so and writes nothing."
  (let ((key (account-credential))
        (free (account-credential +free-provider+))
        (linked (json-value (read-auth-file *auth-file-path*) :object "machine_link")))
    (setf *account-usage* nil *account-free-usage* nil *account-login* nil)
    (if (not (or key free linked))
        (values "Not signed in to nodecode.ai: nothing to sign out of" nil)
        (progn
          (when key
            (save-provider-api-key "nodecode" nil)
            ;; An inline key outranks auth.json's (ACCOUNT-LOGIN-SAVE takes it out for the same reason).
            (when (json-value (read-shared-config) :any "providers" "nodecode" "api_key")
              (config-set (list "providers" "nodecode" "api_key") :remove)))
          (when free (account-forget-free-key))
          (when linked
            (update-auth-file (lambda (auth) (remhash "machine_link" auth))))
          (values
           (if (account-credential)
               "Signed out here, but a nodecode.ai key still reaches this home from the environment or a config command: remove it there"
               (format nil "Signed out of nodecode.ai; remove this machine at https://nodecode.ai/account/machines to end its key there too~:[~; · the default model needs /login or /models~]"
                       (own-provider-p (ignore-errors (shared-model-pin)))))
           t)))))

(defun account-logout-json (&optional note)
  "Sign this home out in this process (ACCOUNT-LOGOUT) and answer ACCOUNT-JSON, `logout' the
words -- NOTE after them when something went -- and `signed_out' whether a key or a link went."
  (multiple-value-bind (text out) (account-logout)
    (let ((json (account-json)))
      (setf (gethash "logout" json) (if (and out note) (format nil "~a · ~a" text note) text)
            (gethash "signed_out" json) (if out t :false))
      json)))

(define-gateway-route handle-gateway-account-logout-http (env)
  "POST /api/gateway/account/logout -- sign this organism's home out of nodecode.ai
(ACCOUNT-LOGOUT), answer ACCOUNT-JSON with what happened in `logout', and, when a key or a
link went, tell every attached shell (BROADCAST-ACCOUNT-CHANGED): each one drops its meter and
clears its screen, not only the one that asked."
  (:route :post "/api/gateway/account/logout" "account sign-out")
  (let ((json (account-logout-json)))
    (when (eq t (gethash "signed_out" json))
      (broadcast-account-changed "signed_out" (gethash "logout" json)))
    json))

(defun route-missing-p (condition)
  "Whether CONDITION, a GATEWAY-HTTP-ERROR, is an organism saying it has no such route: one of
an older build."
  (and (eql 404 (gateway-http-error-status condition))
       (equal "not_found" (gateway-http-error-code condition))))

(defparameter +older-organism-logout+
  "this home's organism runs an older build with no sign-out, so it and the other open shells keep the account until it restarts on this one"
  "What a sign-out done here says when the organism serving the home predates its route.")

(defun account-logout-through (call)
  "Sign this home out through the organism CALL reaches -- CALL posts its logout route and
answers its ACCOUNT-JSON -- which forgets the key and tells every shell. An organism of a build
before that route (0.1.9) answers 404 unknown route: then this process signs the home out
itself, the same files, and says what still holds the account (+OLDER-ORGANISM-LOGOUT+).
`here' in the answer is true when this process did it."
  (handler-case (funcall call)
    (gateway-http-error (condition)
      (unless (route-missing-p condition) (error condition))
      (let ((json (account-logout-json +older-organism-logout+)))
        (setf (gethash "here" json) t)
        json))))

(defun account-logout-text (&key (port (organism-port)) (token (gateway-configured-operator-token)))
  "`nodecode logout' without its exit: the words of a sign-out through the organism on PORT, the
one serving this home, reached with TOKEN, so every shell open on it is told; with none, or one
that does not answer, here."
  (json-value (if port
                  (handler-case
                      (account-logout-through
                       (lambda ()
                         (gateway-http-session-request port :post "/api/gateway/account/logout" (json-object)
                                                       token :connect-timeout 3 :read-timeout 30)))
                    (error (condition)
                      (account-logout-json (format nil "the organism serving this home did not answer (~a), so its open shells keep the account until it restarts" condition))))
                  (account-logout-json))
              :string "logout"))

(defun account-logout-main ()
  "`nodecode logout': ACCOUNT-LOGOUT-TEXT on stdout, with no store or terminal UI."
  (write-line (account-logout-text))
  (uiop:finish-outputs)
  (uiop:quit 0))

;;; --- the shell: /login and the footer's meter ---------------------------------

(defvar *account-meter* nil
  "This shell's copy of the account read model (ACCOUNT-JSON's meter, the gate's usage and the
lines), set on the loop by REFRESH-ACCOUNT-METER and drawn by the footer and /usage. Live-only.")

(defun account-meter-of (json &aux (meter (json-value json :object "meter")))
  "The plist *ACCOUNT-METER* holds for one ACCOUNT-JSON answer."
  (list :signed-in (eq t (json-value json :boolean "signed_in"))
        :label (and meter (json-value meter :string "label"))
        :value (and meter (json-value meter :string "value"))
        :tone (when-let (tone (and meter (json-value meter :string "tone")))
                (find tone '(:success :tool :danger) :key #'symbol-name :test #'string-equal))
        :free (when-let (free (json-value json :object "free"))
                (list :label (json-value free :string "label") :value (json-value free :string "value")
                      :tone (when-let (tone (json-value free :string "tone"))
                              (find tone '(:success :tool :danger) :key #'symbol-name :test #'string-equal))))
        :usage (json-value json :object "usage")
        :lines (coerce (or (json-value json :array "lines") #()) 'list)
        :login (json-value json :object "login")))

(defun refresh-account-meter (model &key then)
  "Read the account's meter from MODEL's gateway off the loop, put it in the footer, and hand the
answer to THEN as (MODEL JSON) on the loop. A shell with no gateway reads nothing."
  (when (and model.program model.http-port)
    (off-loop model
              (lambda () (gateway-model-http-call model :get "/api/gateway/account" nil))
              (lambda (model value)
                (unless (off-loop-failure value)
                  (setf *account-meter* (account-meter-of value)))
                (when then (funcall then model value))
                (render-state model)))))

(defun account-chip-seg (text tone &key link)
  "TEXT as a chip of TONE: the theme's chip ink on TONE's colour where grounds paint, else bold in
TONE's colour on whatever it stands on."
  (if (grounds-shown-p)
      (make-seg :text (format nil " ~a " text) :bold t :link link
                :tone (vstyle :fg (visual-style-fg (style-for-tone :sheet-chip-on)))
                :ground (vstyle :bg (visual-style-fg (style-for-tone tone))))
      (make-seg :text text :tone tone :bold t :link link)))

(defun account-meter-segs (model)
  "The footer's account badge: the plan's name on a chip, then what is left on a chip of the
meter's tone, both a link to /account -- the free key's (Free key, Cyber v1) while the session
runs on it (+FREE-PROVIDER+); NIL when this home is not signed in to nodecode.ai."
  (let* ((meter (or (and (string-equal +free-provider+
                                       (or (ignore-errors (json-value (footer-identity model) :text)) ""))
                         (getf *account-meter* :free))
                    *account-meter*))
         (value (getf meter :value)))
    (when value
      (let ((label (getf meter :label))
            (tone (or (getf meter :tone) :muted))
            (target (cons :command "account")))
        (if (grounds-shown-p)
            (list (make-seg :text (format nil " ~a " label) :link target :ground :sheet-chip
                            :tone (if (equal target (chat-model-hover-target model)) :disclosure-hover :sheet-chip))
                  (account-chip-seg value tone :link target))
            (list (hover-seg model (format nil "~a ~a" label value) target tone)))))))

;;; /usage draws the account as two slabs side by side, the plan's and the credit's: no rules, a
;;; ground each, the one paying now lifted and badged. Where grounds do not paint (16 colours) the
;;; same rows read as two plain columns.

(defun account-days-left (iso)
  "Whole days from now until the day ISO names begins, never below 0; NIL for anything else."
  (when-let (start (ignore-errors (destructuring-bind (year month day) (account-ymd iso)
                                    (encode-universal-time 0 0 0 day month year 0))))
    (max 0 (ceiling (- start (get-universal-time)) 86400))))

(defun account-bar-segs (share cells tone ground)
  "SHARE (0..1) of CELLS bar cells filled in TONE, the rest a quiet step off GROUND's colour."
  (let* ((on (round (* share cells)))
         (quiet (vstyle :fg (blend-rgb (visual-style-fg (style-for-tone :muted))
                                       (visual-style-bg (style-for-tone ground)) 30))))
    (list (make-seg :text (make-string on :initial-element #\▊) :tone tone)
          (make-seg :text (make-string (- cells on) :initial-element #\▊) :tone quiet))))

(defun account-slabs (usage)
  "USAGE as /usage's two slabs, the plan's then the credit's, each (GROUND LINES): the toast's
second step off the panel for the one that pays now, its first for the other, and its lines as (LEFT RIGHT) segment lists, RIGHT flush to the
slab's far edge."
  (let* ((plan (json-value usage :object "plan"))
         (pays (json-value usage :string "pays"))
         (money (account-balance-rows usage))
         (plan-lit (and plan (equal pays "plan")))
         (credit-lit (equal pays "balance")))
    (flet ((s (text &optional (tone :muted) bold link) (make-seg :text text :tone tone :bold bold :link link))
           (ground (lit) (if lit :toast-hover :toast))
           (only (left) (list left nil))
           (badge (lit) (if lit
                            (list (account-chip-seg "● PAYING" :success))
                            (list (make-seg :text "standby" :tone :muted)))))
      (list
       (list (ground plan-lit)
             (if plan
                 (let* ((left (or (json-value plan :number "left") 0))
                        (requests (or (json-value plan :number "requests") 0))
                        (tone (account-plan-tone left requests))
                        (share (if (plusp requests) (min 1 (max 0 (/ left requests))) 0))
                        (threads (json-value plan :integer "threads"))
                        (running (min (or threads 0) (or (json-value plan :integer "running") 0)))
                        (ends (json-value plan :string "ends_at"))
                        (days (account-days-left ends)))
                   (list (list (list (s (format nil "~a plan" (account-plan-name plan))
                                        :foreground t))
                               (if (or plan-lit (plusp left))
                                   (badge plan-lit)
                                   (list (account-chip-seg "USED UP" :danger))))
                         (only (list (s (account-count left) :foreground t) (s " requests left" :foreground)
                                     (s (format nil "   of ~a" (account-count requests)))))
                         (only (append (account-bar-segs share 26 tone (ground plan-lit))
                                       (list (s (format nil "  ~d%" (round (* 100 share)))))))
                         (only (append (list (s (format nil "~@[ends ~a~]~@[ · ~a~]" (account-day ends)
                                                        (and days (format nil "~d day~:p" days)))))
                                       (when threads
                                         (list (s "     threads ")
                                               (s (make-string running :initial-element #\●) :success)
                                               (s (make-string (- threads running) :initial-element #\○))
                                               (s (format nil " ~d of ~d" running threads))))))))
                 (list (only (list (s "No plan" :foreground t)))
                       (only (list (s "subscribe at nodecode.ai/pricing" :muted nil "https://nodecode.ai/pricing"))))))
       (list (ground credit-lit)
             (list (list (list (s "API credit" :foreground t)) (badge credit-lit))
                   (only (list (if money
                                   (s (format nil "~{~a~^, ~}" (mapcar #'account-money money)) :foreground t)
                                   (s "none"))))
                   (only (list (s (cond (credit-lit "pays for each request")
                                        (plan "pays when the plan runs out")
                                        (t "pays once there is some")))))
                   ;; A click opens the page it names (DOCKED-PANEL-ROW keeps the link).
                   (only (list (s "top up at nodecode.ai/account/top-up" :muted nil
                                  "https://nodecode.ai/account/top-up")))))))))

(defun account-slab-line (line width ground &key (pad 2))
  "LINE, a slab's (LEFT RIGHT), laid WIDTH columns wide on GROUND: PAD columns of pad each side,
LEFT clipped to what RIGHT leaves, RIGHT flush to the far pad and dropped when it does not fit."
  (destructuring-bind (left right) line
    (let* ((inner (max 0 (- width (* 2 pad))))
           (right (and right (<= (+ (segs-display-width left) 1 (segs-display-width right)) inner) right))
           (left (clip-segs left (max 0 (- inner (segs-display-width right))) :mark *dialog-row-overflow-mark*))
           (gap (max 0 (- inner (segs-display-width left) (segs-display-width right)))))
      ;; A docked panel lays every keyword tone on its own surface and keeps only a whole style's
      ;; ground, so each run here is a style carrying the slab's: under 16 colours, where grounds
      ;; do not paint, the runs keep their tones and the slab reads as a plain column.
      (mapcar (lambda (seg &aux (style (style-for-tone (or (seg-tone seg) :foreground)))
                                (ground-bg (and (grounds-shown-p) (visual-style-bg (style-for-tone ground)))))
                (if (or (null ground-bg) (seg-ground seg)
                        (and (visual-style-p (seg-tone seg)) (visual-style-bg style)))
                    seg
                    (let ((copy (copy-seg seg)))
                      (setf (seg-tone copy) (style-with style :bg ground-bg)) copy)))
              (append (list (make-seg :text (make-string (min pad width) :initial-element #\Space) :tone :muted))
                      left
                      (list (make-seg :text (make-string gap :initial-element #\Space) :tone :muted))
                      right
                      (list (make-seg :text (make-string (min pad (max 0 (- width pad))) :initial-element #\Space)
                                      :tone :muted)))))))

(defun account-slab-rows (key)
  "The nodecode.ai account as /account and /usage show it: its heading, then the plan's slab and
the credit's side by side, a picker row per line keyed by KEY (a /usage tab; empty in /account), each
a heading the arrows pass over that paints its line of both. NIL when this home is not signed in
to nodecode.ai."
  (when-let (usage (getf *account-meter* :usage))
    (destructuring-bind ((plan-ground plan-lines) (credit-ground credit-lines)) (account-slabs usage)
      (let* ((depth (+ 2 (max (length plan-lines) (length credit-lines))))
             (blank (list nil nil)))
        (flet ((pad (lines)
                 (append (list blank) lines (make-list (- depth 1 (length lines)) :initial-element blank)))
               (text (line)
                 (string-trim " " (format nil "~{~a~}" (mapcar #'seg-text (append (first line) (second line)))))))
          (append
           (list (make-picker-row :key key :label "nodecode.ai account" :mark "■" :mark-tone :accent
                                  :value (list :account)))
           (loop for plan-line in (pad plan-lines)
                 for credit-line in (pad credit-lines)
                 collect (let ((row (make-picker-row :key key :heading t :value (list :account)
                                                     :label (format nil "~a  ~a" (text plan-line) (text credit-line))))
                               (plan-line plan-line)
                               (credit-line credit-line))
                           (setf (picker-row-paint row)
                                 (lambda-ignoring (selected room)
                                   (let* ((plan-width (min 60 (floor (* 3 (max 0 (- room 7))) 5)))
                                          (credit-width (max 0 (min 44 (- room 7 plan-width)))))
                                     (values (append (list (make-seg :text "    " :tone :muted))
                                                     (account-slab-line plan-line plan-width plan-ground)
                                                     (list (make-seg :text "   " :tone :muted))
                                                     (account-slab-line credit-line credit-width credit-ground))
                                             nil))))
                           row))
           (list (make-picker-row :key key :heading t :label "" :value (list :account)))))))))

(defun account-login-watch (model &key (deadline (+ (get-universal-time) 660)) then)
  "Ask MODEL's gateway after the sign-in it started every few seconds, off the loop, until it is
signed in or failed: the meter lands in the footer the moment the person allows the machine.
THEN, when given, takes the last answer as (MODEL VALUE) on the loop: the account's, or OFF-LOOP's
failure; its sign-in still `waiting' when the deadline passed."
  ;; The organism's notice says how it ended, to every shell; this watch only moves the meter.
  (when (and model.program model.http-port)
    (off-loop model
              (lambda () (sleep 3) (gateway-model-http-call model :get "/api/gateway/account" nil))
              (lambda (model value)
                (unless (off-loop-failure value)
                  (setf *account-meter* (account-meter-of value)))
                (if (and (not (off-loop-failure value))
                         (equal "waiting" (json-value value :string "login" "state"))
                         (< (get-universal-time) deadline))
                    (progn (account-login-watch model :deadline deadline :then then) :quiet)
                    (progn (when then (funcall then model value))
                           (render-state model)))))))

(defun account-login-from-shell (model mode &key then)
  "Start MODE's sign-in on MODE's gateway and say where to allow it. THEN, when given, says it
instead of the notice: it takes two answers as (MODEL VALUE) on the loop, the start's -- the
account, its `login' waiting with the page and the code, or OFF-LOOP's failure, which ends it --
then the one the sign-in ended on (ACCOUNT-LOGIN-WATCH)."
  ;; THEN is the setup walk's (SETUP-SIGN-IN-WAIT): its panel shows the page and the code, and
  ;; goes on once the machine is allowed; the notice under it said the same line again.
  (off-loop model
            (lambda () (gateway-model-http-call model :post "/api/gateway/account/login" (json-object "mode" mode)))
            (lambda (model value)
              (let* ((failure (off-loop-failure value))
                     (login (and (not failure) (json-value value :object "login"))))
                (cond (then (funcall then model value))
                      (failure (show-status model (format nil "login: ~a" failure) :error-p t))
                      (t (show-status model (format nil "~:[Open~;Opened~] ~a · code ~a · sign in and allow this machine there"
                                                    (eq t (json-value login :boolean "opened"))
                                                    (json-value login :string "url") (json-value login :string "code")))))
                (unless failure
                  (account-login-watch model :then then)))
              (render-state model)))
  "login: asking nodecode.ai…")

(defun slash-login-op (model args &key (pick #'account-login-from-shell) (context "") on-cancel
                                  &aux (word (string-trim '(#\Space #\Tab) (or args ""))))
  "/login on a shell: bare, the choice between a subscription and API credits; named, that sign-in.
PICK takes the choice as (MODEL MODE); CONTEXT and ON-CANCEL are the setup walk's, whose step the
choice is (SETUP-STAGE-SIGN-IN): the prompt and /models leave the picker's label empty."
  (cond
    ((plusp (length word))
     (if-let (mode (account-mode-word word))
       (funcall pick model mode)
       (slash-unoffered "login")))
    (t (open-picker-dialog model "Sign in to nodecode.ai"
                           (list (make-picker-row :label "Subscription" :detail "your Founders plan's requests (Lite, Base or Pro)"
                                                  :value "subscription")
                                 (make-picker-row :label "API credits" :detail "pay per token from your balance"
                                                  :value "api"))
                           pick
                           :context context
                           :on-cancel on-cancel
                           :subtitle "Signs in with your nodecode.ai account in the browser; this machine's key is saved here."
                           :hint (if on-cancel "enter choose  esc back  ↑↓ navigate" "enter choose  esc cancel  ↑↓ navigate"))
       nil)))

(defun land-account-change (model account &optional text)
  "This home's nodecode.ai sign-in moved (BROADCAST-ACCOUNT-CHANGED), on MODEL's shell, whichever
shell asked: signed out, the footer's meter goes and the screen clears onto a fresh session with
TEXT in the composer's status; signed in, the meter is read again."
  (cond ((equal account "signed_out")
         (setf *account-meter* nil)
         ;; The fresh session is one control call, made off the loop; the switch lands on it.
         (off-loop model
                   (lambda () (create-fresh-gateway-session model))
                   (lambda (model value)
                     (let ((failure (off-loop-failure value)))
                       (unless failure (switch-gateway-session model value))
                       (show-status model (or text "Signed out of nodecode.ai")))
                     (render-state model))))
        ((equal account "signed_in")
         (refresh-account-meter model))))

(defun slash-logout-op (model args)
  "/logout on a shell: sign this home out of nodecode.ai on its gateway, which tells every shell
open on it -- each drops the footer's meter and clears its screen (LAND-ACCOUNT-CHANGE) -- and
say what happened. It takes no argument."
  (if (plusp (length (string-trim '(#\Space #\Tab) (or args ""))))
      "/logout takes no word: /logout alone signs this machine out of nodecode.ai"
      (progn
        (off-loop model
                  (lambda ()
                    (account-logout-through
                     (lambda () (gateway-model-http-call model :post "/api/gateway/account/logout" (json-object)))))
                  (lambda (model value)
                    (let ((failure (off-loop-failure value)))
                      (cond (failure
                             (show-status model (format nil "logout: ~a" failure) :error-p t))
                            ;; An organism of an older build tells no shell: this one clears itself.
                            ((and (json-value value :boolean "here") (json-value value :boolean "signed_out"))
                             (land-account-change model "signed_out" (json-value value :string "logout")))
                            (t (setf *account-meter* (account-meter-of value))
                               (show-status model (json-value value :string "logout")))))
                    (render-state model)))
        "logout: signing out of nodecode.ai…")))
