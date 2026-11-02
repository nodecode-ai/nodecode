;;;; setup-sign-in-test.lisp --- the setup walk signs in to nodecode.ai.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. A box not signed in opens the walk on nodecode.ai's sign-in: the
;;;;      first row, where the list rests, over the providers a key connects.
;;;;   2. Enter on it is /login's choice in the walk's words; Esc returns to
;;;;      the providers. A choice opens a panel that shows the page and the
;;;;      code, and the sign-in landing goes on to nodecode.ai's models.
;;;;   3. A failed sign-in says why over the providers; an answer landing after
;;;;      Esc moves nothing.
;;;;   4. Once a key is saved the row heads what is left to connect, under
;;;;      Suggested, the saved provider still the selection; a home signed in
;;;;      is offered none.
;;;;
;;;; Loads after setup-test: reuses WITH-SETUP-BOX, RUN-SETUP and PAGE-SAID.

(in-package #:nodecode.test)

(defun sign-in-answer (state &rest login)
  "An ACCOUNT-JSON answer as a shell holds it, its sign-in in STATE with LOGIN's fields."
  (nle::make-json-object "signed_in" (if (equal state "signed-in") t :false)
                         "login" (apply #'nle::make-json-object "state" state login)))

(defmacro with-held-sign-in ((mode then) &body body)
  "Run BODY with the walk's sign-in held: MODE and THEN bound to what the walk asked
ACCOUNT-LOGIN-FROM-SHELL, nothing sent to a gateway."
  `(let ((,mode nil) (,then nil))
     (with-stubbed-fdefinition (nle::account-login-from-shell (model asked &key ((:then answer)))
                                 (declare (ignore model))
                                 (setf ,mode asked ,then answer)
                                 "login: asking nodecode.ai…")
       ,@body)))

(deftest setup-walk-opens-on-nodecode-sign-in (with-setup-box ((format nil "{}~%")))
  (with-frame-model (model 100 30 :session-id "setup-sign-in")
    (with-held-sign-in (mode then)
      (run-setup model)
      (let ((dialog model.dialog))
        (is (nle::connect-dialog-p dialog) "the walk opens on the providers")
        (is (equal '("Sign in to nodecode.ai" "" "Or use a provider's key" "Anthropic") (subseq (row-labels dialog) 0 4)) "nodecode.ai's sign-in leads the keys")
        (is (eq :own-model (nle::picker-row-tone (first dialog.rows))) "in our tone")
        (is (= 0 (nle::connect-dialog-selection dialog)) "the list rests on it: Enter signs in")
        (nle::picker-dialog-key model :enter)
        (let ((choice model.dialog))
          (is (equal "Sign in to nodecode.ai" (nle::picker-dialog-title choice)) "Enter asks /login's question")
          (is (equal '("subscription" "api") (mapcar #'nle::picker-row-value (nle::picker-dialog-rows choice))) "a subscription or API credits")
          (is (equal "Step 1. Sign in to nodecode.ai · step 1/3" (nle::picker-dialog-context choice)) "in the walk's words")
          (is (search "esc back" (nle::picker-dialog-hint choice)) "Esc says it goes back"))
        (nle::picker-dialog-key model :escape)
        (is (nle::connect-dialog-p model.dialog) "Esc returns to the providers")
        (is (nle::chat-model-setup-walk model) "and the walk goes on")
        (dialog-keys model :enter :enter)
        (is (equal "subscription" mode) "Enter on Subscription starts that sign-in")
        (let ((panel model.dialog))
          (is (eq :setup-sign-in (nle::picker-dialog-kind panel)) "a panel waits for the browser")
          (is (equal "Step 1. Sign in to nodecode.ai · step 1/3" (nle::picker-dialog-context panel)))
          (is (null (nle::status-said model)) "the panel says it: no notice stands under it")
          (funcall then model (sign-in-answer "waiting" "url" "https://nodecode.ai/link/k3y" "code" "ABCD-1234" "opened" t))
          (is (eq panel model.dialog) "the start keeps the panel")
          (is-carrying (text (page-said model))
            "Opened https://nodecode.ai/link/k3y" "code ABCD-1234" "allow this machine")
          (funcall then model (sign-in-answer "signed-in" "text" "Signed in to nodecode.ai · default model nodecode/foresight-cyber-v1")))
        (is (eq :models (nle::picker-dialog-kind model.dialog)) "signed in, the walk goes on to the models")
        (is (equal "Step 2. Choose model · step 2/3" (nle::picker-dialog-context model.dialog)))
        (is (equal '("nodecode") (mapcar (lambda (tab) (getf tab :value)) (nle::picker-dialog-tabs model.dialog))) "on nodecode.ai's models")
        (is (equal "nodecode" (first (nle::setup-run-providers (nle::chat-model-setup-walk model)))) "the walk counts nodecode.ai as its provider")))
    (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))

(deftest setup-sign-in-failure-says-why-and-a-late-answer-moves-nothing (with-setup-box ((format nil "{}~%")))
  (with-frame-model (model 100 30 :session-id "setup-sign-in-fails")
    (with-held-sign-in (mode then)
      (run-setup model)
      (dialog-keys model :enter :enter)
      (funcall then model (sign-in-answer "waiting" "url" "https://nodecode.ai/link/k3y" "code" "ABCD-1234"))
      (funcall then model (sign-in-answer "failed" "text" "nodecode.ai sign-in: the code expired"))
      (is (nle::connect-dialog-p model.dialog) "a failed sign-in returns to the providers")
      (is (search "the code expired" (or (nle::status-said model) "")) "and says why")
      (dialog-keys model :enter :down :enter)
      (is (equal "api" mode) "API credits starts that sign-in")
      (is (eq :setup-sign-in (nle::picker-dialog-kind model.dialog)))
      (funcall then model (list :error "connection refused"))
      (is (nle::connect-dialog-p model.dialog) "a start nothing answered returns to the providers")
      (is (search "connection refused" (or (nle::status-said model) "")) "and says so")
      (dialog-keys model :enter :enter)
      (nle::picker-dialog-key model :escape)
      (let ((providers model.dialog))
        (is (nle::connect-dialog-p providers) "Esc on the panel returns to the providers")
        (funcall then model (sign-in-answer "signed-in" "text" "Signed in to nodecode.ai"))
        (is (eq providers model.dialog) "a sign-in landing after Esc moves nothing")))
    (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))

(deftest setup-sign-in-row-heads-connect-another-and-leaves-a-signed-in-home ()
  (with-setup-box ((format nil "{}~%"))
    (with-frame-model (model 100 30 :session-id "setup-sign-in-keyed")
      (run-setup model)
      (let ((dialog model.dialog))
        (dialog-keys model :down :enter #\s #\k :enter)
        (is (equal '("Suggested" "Anthropic" "Connect another" "Sign in to nodecode.ai") (subseq (row-labels dialog) 0 4)) "under Suggested, it heads what is left to connect")
        (is (= 1 (nle::connect-dialog-selection dialog)) "the provider just saved is still the selection"))
      (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape))))
  (with-setup-box ((format nil "{\"providers\":{\"nodecode\":{\"sdk\":\"openai-completions\",\"base_url\":\"https://api.nodecode.example/v1\",\"api_key\":\"k\"}}}~%"))
    (with-frame-model (model 100 30 :session-id "setup-sign-in-signed")
      (run-setup model)
      (is (notany (lambda (label) (equal label "Sign in to nodecode.ai")) (row-labels model.dialog)) "a home signed in is offered no sign-in")
      (loop repeat 8 while model.dialog do (nle::picker-dialog-key model :escape)))))
