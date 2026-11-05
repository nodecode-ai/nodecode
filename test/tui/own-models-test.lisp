;;;; own-models-test.lisp --- /models pins nodecode.ai's own models.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Claims:
;;;;   1. A listing's `kind' rides its rows, and a model that is not a chat
;;;;      model is offered no turn.
;;;;   2. Signed in, /models opens on a Nodecode section: each own chat model
;;;;      as nodecode/<model> in the :own-model tone, leading every model
;;;;      list; a search for `nodecode' finds it, and Enter picks it.
;;;;   3. Signed out, the root opens as it always did, and a search for
;;;;      `nodecode' finds the one row that opens /login.
;;;;
;;;; Loads after slash-test: reuses WITH-MODEL-LISTINGS, ENTER-ROW and
;;;; DIALOG-AFTER from there.

(in-package #:nodecode.test)

(defparameter +own-models-config+
  '(:providers (:nodecode (:sdk "openai-completions" :base_url "https://api.nodecode.example/v1" :api_key "k")
                :zeta (:sdk "openai-completions" :base_url "https://zeta.example/v1" :api_key "k")))
  "A shared config signed in to nodecode.ai beside one other keyed provider.")

(defun own-models-cache (&optional (now (get-universal-time)))
  "The provider-models cache +OWN-MODELS-CONFIG+'s providers left at NOW:
nodecode.ai's chat and image model, and zeta's one model."
  `(:nodecode (:fetched_at ,now :base_url "https://api.nodecode.example/v1" :error :null
               :models #((:id "foresight-cyber-v1" :context_window 1048576 :kind "chat")
                         (:id "foresight-image-v1" :kind "image")))
    :zeta (:fetched_at ,now :base_url "https://zeta.example/v1" :error :null
           :models #((:id "a-1" :display "Alpha One")))))

(deftest provider-listing-keeps-a-models-kind ()
  (let ((rows (nle::parse-provider-listing
               "{\"data\":[{\"id\":\"foresight-cyber-v1\",\"kind\":\"chat\"},{\"id\":\"foresight-image-v1\",\"kind\":\"image\"},{\"id\":\"plain\"}]}")))
    (is (equal '(("foresight-cyber-v1" "chat") ("foresight-image-v1" "image") ("plain" nil))
               (mapcar (lambda (row) (list (getf row :id) (getf row :kind))) rows)) "each row keeps the kind its listing names, NIL where it names none")))

(deftest models-root-pins-own-models ()
  (with-model-listings (:config +own-models-config+ :cache (own-models-cache))
    (with-frame-model (model 100 40 :session-id "own-models")
      (let ((picked nil))
        (with-stubbed-fdefinitions ((nle::slash-models-op (model tokens) (setf picked tokens) "selected")
                                    (nle::open-models-reasoning-picker (model) nil))
          (let ((rows (nle::models-catalog-picker-rows)))
            (is (equal '("nodecode" "foresight-cyber-v1") (nle::picker-row-value (first rows))) "our own model leads every model list")
            (is (notany (lambda (row) (equal '("nodecode" "foresight-image-v1") (nle::picker-row-value row))) rows) "an image model is offered no turn")
            (is (find '("zeta" "a-1") rows :key #'nle::picker-row-value :test #'equal) "a listing that names no kind keeps its models"))
          (nle::open-models-provider-picker model)
          (let* ((dialog model.dialog)
                 (rows (nle::picker-visible-rows dialog)))
            (is-shape (first rows) (nle::picker-row-label "Nodecode") (nle::picker-row-heading eq t))
            (is-shape (second rows)
              (nle::picker-row-label "nodecode/foresight-cyber-v1")
              (nle::picker-row-tone eq :own-model)
              (nle::picker-row-value '(:model ("nodecode" "foresight-cyber-v1"))))
            (is (equal '(:model ("nodecode" "foresight-cyber-v1")) (picker-selected-value dialog)) "the list opens on it")
            (dialog-after model "nodecode")
            (is (equal '(("nodecode" "foresight-cyber-v1"))
                       (loop for row in (nle::picker-visible-rows dialog)
                             for pair = (getf (nle::picker-row-value row) :model)
                             when (and (consp pair) (stringp (first pair))) collect pair)) "a search for nodecode finds our model, once")
            (enter-row model "nodecode/foresight-cyber-v1")
            (is (equal '("nodecode" "foresight-cyber-v1") picked) "Enter on it picks it")))))))

(deftest models-root-signs-in-for-own-models ()
  (with-model-listings (:config +listed-providers-config+ :cache (listed-providers-cache))
    (with-frame-model (model 100 40 :session-id "own-models-signed-out")
      (let ((asked nil))
        (with-stubbed-fdefinitions ((nle::slash-login-op (model args) (setf asked (list args)) nil))
          (nle::open-models-provider-picker model)
          (let ((dialog model.dialog))
            (is (notany (lambda (row) (member (nle::picker-row-label row) '("Nodecode" "Sign in to nodecode.ai") :test #'equal))
                        (nle::picker-visible-rows dialog)) "an empty box shows no section of ours")
            (dialog-after model "nodecode")
            (is-present (row (find "Sign in to nodecode.ai" (nle::picker-visible-rows dialog)
                                   :key #'nle::picker-row-label :test #'equal)) "a search for nodecode finds it"
              (is (eq :own-model (nle::picker-row-tone row)) "in our tone"))
            (enter-row model "Sign in to nodecode.ai")
            (is (equal '("") asked) "Enter on it opens /login")))))))
