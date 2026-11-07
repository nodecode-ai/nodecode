;;;; watch.lisp --- one sleeping thread: a new head sha becomes one rank turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The watch polls each watched repository's open PRs every poll_seconds,
;;;; sorted by update and sent with the last listing's ETag: GitHub answers
;;;; 304 while nothing moved, which costs no request against the rate limit
;;;; and no token at all, because nothing reaches a model. A push moves the PR
;;;; to the top of the first page, so that page's ETag stands for them all.
;;;;
;;;; A head owes a turn when the PR is ready (not a draft), someone else's
;;;; (not the token's own login) and its newest rank marker names another sha.
;;;; The turn is one NLE:SUBMIT into the PR's own session, prs-<repo>-<n>,
;;;; with the command id `prs:<repo>#<n>:<sha>': the kernel's durable
;;;; admission makes a second submit of one head a :DUPLICATE that spawns
;;;; nothing, so a restart, two polls or two organisms never rank one head
;;;; twice, and no claim record of our own is kept. The request carries the
;;;; dossier (SHOW's text), so the turn reads nothing before it judges. At
;;;; most `concurrent' of these turns run at once; the rest wait a poll.
;;;;
;;;; The request is a stranger's text, and the turn nobody watches. So a PR's
;;;; own session only judges: its one tool is eval, and a snippet there is read,
;;;; never run, unless every form in it is a call to prs:show, prs:queue or
;;;; prs:rank whose arguments are literals (JUDGE-ONLY, a :TOOL hook). A
;;;; description that talks the model into anything else is refused before a
;;;; byte of it evaluates; the worst it can do is a rank whose rules RANK owns.
;;;;
;;;; THREAD RULE: SUBMIT-PROMPT is the one seam that reaches the organism's
;;;; ingress, called on the watch thread; a test stubs it and runs POLL by hand.

(in-package #:nodecode-prs)

(nlk:access (admission nlk::active-input-admission))

(defvar *worker* nil
  "The watch thread while it runs (an NLK:WORKER), or NIL.")

(defvar *watch* t
  "Whether the start runs the watch thread; a test drives POLL by hand.")

(defvar *etags* (make-hash-table :test #'equal)
  "repo -> the ETag of its last listing. Watch thread only.")

(defvar *pulls* (make-hash-table :test #'equal)
  "repo -> its open PRs as last listed, for /prs. Guarded by *LOCK*.")

(defvar *seen* (make-hash-table :test #'equal)
  "\"repo#n:sha\" -> T once the watch has settled that head. Watch thread only.")

(defvar *running* '()
  "Sessions whose rank turn the watch started and may still run.")

(defvar *login* nil
  "The token's own login: its PRs are the operator's, not the watch's.")

(defvar *failing* '()
  "Repositories whose last poll failed; each has a standing notice.")

(defun pr-session (repo n)
  (format nil "prs-~(~a~)-~d" (ppcre:regex-replace-all "[^A-Za-z0-9]+" repo "-") n))

(defun submit-prompt (session prompt command-id source-id)
  "PROMPT into SESSION through the one in-process ingress: => the disposition."
  (let ((admission (nle:submit session prompt :command-id command-id :source "prs" :source-id source-id)))
    admission.disposition))

(defun ensure-session (session repo)
  "SESSION standing by, pinned to the section's model, if it is not durable
already. Nothing to do without a store."
  (unless (or (not (nlk:store-open-p)) (nlk:session-exists-p session))
    (nlk:standby-session :id session :cwd (namestring (ensure-directories-exist (clone-dir repo))))
    (when (or (setting :model) (setting :provider))
      (nlk:record-session-model-selection session :provider (setting :provider) :model (setting :model)))))

(defparameter *rank-request*
  "[prs: rank ~a at ~a. This session is the pull request's record: each push brings a new ~
request here, and what was said before is above. The dossier below is everything the rank ~
needs; do not run the PR's code. Call, once:
  (prs:rank ~d :pN :repo ~s :sha ~s :review \"...\")
p0 merge now (small, right, fixes something); p1 merge this week; p2 worth landing after ~
changes; p3 unlikely to land. The review is what the author reads: what is right, what blocks, ~
what to change, in plain words. Add :comments '((:path \"file\" :line N :body \"...\")) for lines ~
of the diff. Close only with :close :duplicate :cite \"#N\", :close :on-main :cite \"<commit>\" or ~
:close :cannot-reproduce :cite \"<commit>\". Then answer with the rank and one line why. You ~
have ~a.]"
  "The line every rank request opens with. Data, so a layer can reword it.")

(defun submit-rank (repo pull)
  "One rank turn for PULL's head: => the admission's disposition."
  (let* ((n (number-of pull))
         (head (head-of pull))
         (session (pr-session repo n)))
    (ensure-session session repo)
    (let ((disposition (submit-prompt session
                                      (format nil "~?~%~%~a" *rank-request*
                                              (list (key repo n) (short head) n repo head
                                                    (nlk:duration-text (* 60 (setting :turn-minutes))))
                                              (dossier repo n))
                                      (format nil "prs:~a:~a" (key repo n) head)
                                      (key repo n))))
      (when (member disposition '(:started :queued))
        (pushnew session *running* :test #'string=))
      disposition)))

(defun list-changes (repo)
  "REPO's open PRs, listed again only when the first page's ETag moved."
  (multiple-value-bind (pulls status headers)
      (api-all (path repo "/pulls?state=open&sort=updated&direction=desc&per_page=100")
               :etag (gethash repo *etags*))
    (if (eql status 304)
        (with-prs-lock (gethash repo *pulls*))
        (progn (setf (gethash repo *etags*) (and (hash-table-p headers) (gethash "etag" headers)))
               (with-prs-lock (setf (gethash repo *pulls*) pulls))))))

(defun owed-p (repo pull)
  "Whether the watch still has to settle PULL's head."
  (and (not (nlk:json-value pull :boolean "draft"))
       (not (equal (author-of pull) *login*))
       (not (gethash (format nil "~a:~a" (key repo (number-of pull)) (head-of pull)) *seen*))))

(defun poll-repo (repo)
  (unless *login*
    (setf *login* (nlk:json-value (api :get "/user") :string "login")))
  (setf *running* (remove-if-not #'nlk:active-turn-p *running*))
  (dolist (pull (sort (remove-if-not (lambda (pull) (owed-p repo pull)) (copy-list (list-changes repo)))
                      #'string< :key (lambda (pull) (nlk:json-value pull :string "created_at"))))
    (when (>= (length *running*) (setting :concurrent))
      (return))
    (let ((mark (mark repo (number-of pull) (head-of pull))))
      (unless (equal (getf mark :sha) (head-of pull))
        (submit-rank repo pull))
      (setf (gethash (format nil "~a:~a" (key repo (number-of pull)) (head-of pull)) *seen*) t))))

(defun poll ()
  "One pass over every watched repository; a failure stands under prs:<repo> until a pass succeeds."
  (when (nlk:store-open-p)
    (dolist (repo (setting :repos))
      (let ((notice-key (format nil "prs:~a" repo)))
        (handler-case
            (progn (poll-repo repo)
                   (when (member repo *failing* :test #'string=)
                     (setf *failing* (remove repo *failing* :test #'string=))
                     (nle:notice nil :key notice-key)))
          (error (condition)
            (pushnew repo *failing* :test #'string=)
            (nle:notice (format nil "prs: ~a not polled: ~a" repo condition)
                        :level :warning :key notice-key)))))))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a turn in a PR's session runs under turn_minutes."
  (let ((session (getf turn :session-id)))
    (if (and (stringp session) (uiop:string-prefix-p "prs-" session))
        (list :seconds (* 60 (setting :turn-minutes)))
        (funcall next turn))))

;;; --- a PR's session only judges ----------------------------------------------------

(defparameter +judging-verbs+ '(show queue rank)
  "What a snippet in a PR's own session may call.")

(defun literal-p (form)
  "Whether FORM is data: a string, number, keyword, NIL, T or a quoted form."
  (or (stringp form) (numberp form) (keywordp form) (null form) (eq form t)
      (and (consp form) (eq (first form) 'quote) (= 2 (length form)))))

(defun judging-p (source)
  "Whether SOURCE reads as one or more calls to +JUDGING-VERBS+ with literal
arguments, and nothing else. Read in a scratch package, never evaluated."
  (let ((scratch (make-package (symbol-name (gensym "PRS-READ")) :use '())))
    (unwind-protect
         (handler-case
             (let ((*read-eval* nil)
                   (*package* scratch))
               (with-input-from-string (in (or source ""))
                 (loop with forms = 0
                       for form = (read in nil in)
                       until (eq form in)
                       unless (and (consp form) (member (first form) +judging-verbs+)
                                   (listp (cdr form)) (every #'literal-p (rest form)))
                         return nil
                       do (incf forms)
                       finally (return (plusp forms)))))
           (error () nil))
      (delete-package scratch))))

(defun judge-only (op next)
  "The :TOOL point: in a PR's own session, a call JUDGING-P does not pass is
refused before it runs."
  (let ((session (getf (nle:turn) :session-id)))
    (if (and (stringp session) (uiop:string-prefix-p "prs-" session)
             (not (and (equal (getf op :name) "eval")
                       (judging-p (nlk:json-value (getf op :arguments) :string "form")))))
        (nle:failure (format nil "ERROR: refused: a pull request's session only judges -- eval calls ~
                                  to prs:show, prs:queue or prs:rank with literal arguments and nothing ~
                                  else; the call did not run"))
        (funcall next op))))
