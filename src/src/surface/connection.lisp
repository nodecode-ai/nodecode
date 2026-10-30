;;;; connection.lisp --- the footer's model name, green or red: who can read a prompt sent to the
;;;; model this shell has chosen, and how it travels there, measured once when the model is chosen.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two halves. What the wire says is measured: one TLS handshake with the endpoint the chosen
;;;; model's lane dials -- the protocol, whether the key exchange is post-quantum, who issued the
;;;; certificate, whether Cloudflare answers in front -- and nothing of the person's is sent. What
;;;; the provider promises is read from *PROVIDER-TERMS*, matched on that endpoint's host: a
;;;; host on no row is unknown, and so unsecure. Only NODECODE's own endpoint reads Private.

(in-package #:nodecode.evolved)

;;; --- what each provider promises ----------------------------------------------
;;; From a survey of each provider's own terms and privacy pages (checked 2026-10). A row's HOSTS
;;; decide who answers (a suffix, or a glob with *); its IDS are the providers whose API those
;;; terms are -- a subscription reaching the same host (a sign-in cell) is not on them, and reads
;;; unknown. STORED is what the terms say happens to a prompt; TRAINS and ZDR their defaults.

(defparameter *provider-terms-checked* "2026-10")

(defparameter *provider-terms*
  '((:name "NODECODE" :own t :hosts ("api.nodecode.ai") :ids ("nodecode") :where "DE → NL" :eu t
     :stored "no" :trains :no :zdr :yes :source "nodecode.ai/privacy")
    (:name "Anthropic" :hosts ("api.anthropic.com") :ids ("anthropic") :where "US"
     :stored "30 days" :trains :no :zdr :enterprise :source "privacy.anthropic.com")
    (:name "OpenAI" :hosts ("api.openai.com") :ids ("openai") :where "US" :eu :option
     :stored "30 days" :trains :no :zdr :request :source "openai.com/policies")
    (:name "Google" :hosts ("generativelanguage.googleapis.com") :ids ("google") :where "US"
     :stored "55 days" :trains :free-tier :zdr :no :source "ai.google.dev/gemini-api/terms")
    (:name "Google Vertex" :hosts ("aiplatform.googleapis.com" "*-aiplatform.googleapis.com")
     :ids ("google-vertex" "google-vertex-anthropic") :where "US" :eu :option
     :stored "24 h cache" :trains :no :zdr :yes :source "cloud.google.com/vertex-ai")
    (:name "Azure OpenAI" :hosts ("openai.azure.com" "services.ai.azure.com" "cognitiveservices.azure.com")
     :ids ("azure" "azure-cognitive-services") :where "US" :eu :option
     :stored "30 days" :trains :no :zdr :request :source "learn.microsoft.com/azure/ai-foundry")
    (:name "AWS Bedrock" :hosts ("bedrock-runtime.*.amazonaws.com") :ids ("amazon-bedrock") :where "US" :eu :option
     :stored "no" :trains :no :zdr :yes :source "aws.amazon.com/bedrock/security-compliance")
    (:name "xAI" :hosts ("api.x.ai") :ids ("xai") :where "US"
     :stored "30 days" :trains :no :zdr :enterprise :source "x.ai/legal")
    (:name "Cohere" :hosts ("api.cohere.com" "api.cohere.ai") :ids ("cohere") :where "CA"
     :stored "30 days" :trains :no :zdr :request :source "cohere.com/privacy")
    (:name "Perplexity" :hosts ("api.perplexity.ai") :ids ("perplexity" "perplexity-agent") :where "US"
     :stored "unknown" :trains :no :zdr :request :source "perplexity.ai/hub/legal")
    (:name "Mistral" :hosts ("api.mistral.ai") :ids ("mistral") :where "FR" :eu t
     :stored "30 days" :trains :free-tier :zdr :request :source "legal.mistral.ai")
    (:name "DeepSeek" :hosts ("api.deepseek.com") :ids ("deepseek") :where "CN"
     :stored "no limit" :trains :unknown :zdr :no :source "cdn.deepseek.com/policies")
    (:name "OpenRouter" :router t :hosts ("openrouter.ai") :ids ("openrouter") :where "US" :eu :option
     :stored "no" :trains :no :zdr :yes :source "openrouter.ai/privacy")
    (:name "Canopy Wave" :hosts ("canopywave.io") :ids ("canopywave") :where "US"
     :stored "no" :trains :no :zdr :yes :source "canopywave.com/zero-data-retention-policy")
    (:name "Nebius" :hosts ("api.tokenfactory.nebius.com" "api.studio.nebius.com" "api.studio.nebius.ai")
     :ids ("nebius") :where "NL" :eu :option
     :stored "yes" :trains :drafts :zdr :yes :source "docs.tokenfactory.nebius.com/legal")
    (:name "Regolo" :hosts ("api.regolo.ai") :ids ("regolo-ai") :where "IT" :eu t
     :stored "no" :trains :no :zdr :yes :source "regolo.ai/privacy")
    (:name "GreenPT" :hosts ("api.greenpt.ai") :ids ("greenpt") :where "NL" :eu t
     :stored "unknown" :trains :no :zdr :unknown :source "greenpt.com/privacy")
    (:name "Tensorix" :hosts ("api.tensorx.ai") :ids ("tensorx") :where "IE" :eu t
     :stored "no" :trains :no :zdr :yes :source "tensorix.ai/privacy")
    (:name "Infercom" :hosts ("api.infercom.ai") :ids ("infercom") :where "LU" :eu t
     :stored "no" :trains :no :zdr :yes :source "infercom.ai/privacy")
    (:name "Berget AI" :hosts ("api.berget.ai") :ids ("berget") :where "SE" :eu t
     :stored "unknown" :trains :unknown :zdr :unknown :source "berget.ai/privacy")
    (:name "evroc" :hosts ("evroc.com") :ids ("evroc") :where "SE" :eu t
     :stored "unknown" :trains :no :zdr :unknown :source "evroc.com/legal")
    (:name "AKI.IO" :hosts ("aki.io") :ids ("aki-io") :where "DE" :eu t
     :stored "unknown" :trains :unknown :zdr :unknown :source "aki.io/processors")
    (:name "Inceptron" :hosts ("api.inceptron.io") :ids ("inceptron") :where "SE" :eu t
     :stored "no" :trains :no :zdr :yes :source "inceptron.io")
    (:name "Lyceum" :hosts ("api.lyceum.technology") :ids ("lyceum") :where "DE" :eu t
     :stored "no" :trains :no :zdr :yes :source "lyceum.technology/trust")
    (:name "Scaleway" :hosts ("api.scaleway.ai") :ids ("scaleway") :where "FR" :eu t
     :stored "no" :trains :no :zdr :yes :source "scaleway.com/legal")
    (:name "OVHcloud" :hosts ("endpoints.kepler.ai.cloud.ovh.net") :ids ("ovhcloud") :where "FR" :eu t
     :stored "no" :trains :no :zdr :yes :source "ovhcloud.com/legal")
    (:name "IONOS" :hosts ("inference.de-txl.ionos.com") :ids ("ionos") :where "DE" :eu t
     :stored "no" :trains :no :zdr :yes :source "ionos.com/terms")
    (:name "STACKIT" :hosts ("model-serving.eu01.onstackit.cloud") :ids ("stackit") :where "DE" :eu t
     :stored "no" :trains :no :zdr :yes :source "stackit.com/privacy")
    (:name "Synthetic" :hosts ("api.synthetic.new") :ids ("synthetic") :where "US"
     :stored "14 days" :trains :no :zdr :unknown :source "synthetic.new/policies")
    (:name "Together AI" :hosts ("api.together.xyz" "api.together.ai") :ids ("togetherai") :where "US"
     :stored "yes" :trains :no :zdr :yes :source "together.ai/privacy")
    (:name "Fireworks" :hosts ("api.fireworks.ai") :ids ("fireworks-ai") :where "US"
     :stored "no" :trains :no :zdr :yes :source "fireworks.ai/privacy-policy")
    (:name "Groq" :hosts ("api.groq.com") :ids ("groq") :where "US" :eu :option
     :stored "up to 30 days" :trains :no :zdr :yes :source "console.groq.com/docs/your-data")
    (:name "Cerebras" :hosts ("api.cerebras.ai") :ids ("cerebras") :where "US"
     :stored "no" :trains :no :zdr :yes :source "cerebras.ai/privacy-policy")
    (:name "SambaNova" :hosts ("api.sambanova.ai") :ids ("sambanova") :where "US"
     :stored "no" :trains :no :zdr :yes :source "sambanova.ai/privacy-policy")
    (:name "DeepInfra" :hosts ("api.deepinfra.com") :ids ("deepinfra") :where "US"
     :stored "no" :trains :no :zdr :yes :source "deepinfra.com/privacy")
    (:name "Replicate" :hosts ("api.replicate.com") :ids ("replicate") :where "US"
     :stored "1 hour" :trains :unknown :zdr :no :source "replicate.com/privacy")
    (:name "Baseten" :hosts ("inference.baseten.co" "api.baseten.co") :ids ("baseten") :where "US" :eu :option
     :stored "no" :trains :no :zdr :yes :source "baseten.co/privacy-policy")
    (:name "Hyperbolic" :hosts ("api.hyperbolic.xyz") :ids ("hyperbolic") :where "US"
     :stored "no" :trains :no :zdr :yes :source "hyperbolic.xyz/privacy")
    (:name "Novita AI" :hosts ("api.novita.ai") :ids ("novita-ai") :where "US"
     :stored "unknown" :trains :no :zdr :yes :source "novita.ai/legal/privacy-policy")
    (:name "Lambda" :hosts ("api.lambdalabs.com" "api.lambda.ai") :ids ("lambda") :where "US"
     :stored "no" :trains :no :zdr :yes :source "lambda.ai/legal/privacy-policy")
    (:name "Featherless" :hosts ("api.featherless.ai") :ids ("featherless") :where "US"
     :stored "no" :trains :no :zdr :yes :source "featherless.ai/privacy")
    (:name "Chutes" :hosts ("chutes.ai") :ids ("chutes") :where "US"
     :stored "no" :trains :no :zdr :yes :source "chutes.ai/privacy")
    (:name "Parasail" :hosts ("api.parasail.io") :ids ("parasail") :where "US"
     :stored "no" :trains :no :zdr :yes :source "parasail.io/legal")
    (:name "Nscale" :hosts ("nscale.com") :ids ("nscale") :where "UK" :eu :option
     :stored "unknown" :trains :unknown :zdr :unknown :source "nscale.com/legal")
    (:name "Cloudflare" :hosts ("api.cloudflare.com") :ids ("cloudflare-workers-ai") :where "US" :eu :option
     :stored "no" :trains :no :zdr :yes :source "developers.cloudflare.com/workers-ai/privacy")
    (:name "Hugging Face" :router t :hosts ("router.huggingface.co" "api-inference.huggingface.co")
     :ids ("huggingface") :where "US"
     :stored "no" :trains :no :zdr :unknown :source "huggingface.co/privacy")
    (:name "Modal" :hosts ("modal.direct" "modal.run") :ids ("modal") :where "US" :eu :option
     :stored "up to 7 days" :trains :no :zdr :yes :source "modal.com/legal/privacy-policy")
    (:name "NVIDIA" :hosts ("integrate.api.nvidia.com") :ids ("nvidia") :where "US"
     :stored "unknown" :trains :unknown :zdr :no :source "build.nvidia.com")
    (:name "Inference.net" :hosts ("inference.net") :ids ("inference") :where "US"
     :stored "no" :trains :no :zdr :yes :source "inference.net/privacy")
    (:name "FriendliAI" :hosts ("api.friendli.ai") :ids ("friendli") :where "KR"
     :stored "no" :trains :no :zdr :yes :source "friendli.ai/privacy-policy")
    (:name "Alibaba Cloud" :hosts ("aliyuncs.com") :ids ("alibaba" "alibaba-cn") :where "CN" :eu :option
     :stored "unknown" :trains :no :zdr :no :source "alibabacloud.com/help/model-studio")
    (:name "Moonshot AI" :hosts ("api.moonshot.ai" "api.moonshot.cn" "api.kimi.com" "api.kimi.ai")
     :ids ("moonshotai" "moonshotai-cn") :where "CN"
     :stored "unknown" :trains :no :zdr :no :source "platform.moonshot.ai/docs")
    (:name "Z.ai" :hosts ("api.z.ai" "open.bigmodel.cn") :ids ("zai" "zhipuai") :where "CN"
     :stored "unknown" :trains :unknown :zdr :no :source "docs.z.ai/legal")
    (:name "MiniMax" :hosts ("api.minimax.io" "api.minimax.cn" "api.minimaxi.com") :ids ("minimax" "minimax-cn")
     :where "CN" :stored "unknown" :trains :unknown :zdr :no :source "minimax.io/platform/protocol")
    (:name "ByteDance" :hosts ("volces.com" "bytepluses.com") :ids ("volcengine" "byteplus") :where "CN"
     :stored "unknown" :trains :no :zdr :unknown :source "docs.byteplus.com/modelark")
    (:name "Baidu" :hosts ("qianfan.baidubce.com") :ids ("qianfan") :where "CN"
     :stored "unknown" :trains :unknown :zdr :no :source "cloud.baidu.com")
    (:name "Tencent" :hosts ("tencentcloudapi.com" "cloud.tencent.com" "tencentmaas.com") :ids ("tencent-tokenhub")
     :where "CN" :stored "unknown" :trains :unknown :zdr :no :source "cloud.tencent.com")
    (:name "StepFun" :hosts ("api.stepfun.com" "api.stepfun.ai") :ids ("stepfun" "stepfun-ai") :where "CN"
     :stored "unknown" :trains :unknown :zdr :no :source "platform.stepfun.com")
    (:name "01.AI" :hosts ("api.lingyiwanwu.com" "api.01.ai") :ids ("01-ai") :where "CN"
     :stored "unknown" :trains :unknown :zdr :no :source "platform.lingyiwanwu.com")
    (:name "SiliconFlow" :hosts ("api.siliconflow.com" "api.siliconflow.cn") :ids ("siliconflow" "siliconflow-cn")
     :where "CN" :stored "unknown" :trains :unknown :zdr :unknown :source "siliconflow.com/privacy")
    (:name "AI21" :hosts ("api.ai21.com") :ids ("ai21") :where "IL"
     :stored "unknown" :trains :unknown :zdr :enterprise :source "ai21.com/privacy-policy")
    (:name "Reka" :hosts ("api.reka.ai") :ids ("reka") :where "US"
     :stored "unknown" :trains :unknown :zdr :unknown :source "reka.ai/privacy")
    (:name "Writer" :hosts ("api.writer.com") :ids ("writer") :where "US"
     :stored "unknown" :trains :no :zdr :enterprise :source "writer.com/legal")
    (:name "Requesty" :router t :hosts ("requesty.ai") :ids ("requesty") :where "UK" :eu :option
     :stored "yes" :trains :no :zdr :request :source "requesty.ai/privacy")
    (:name "Eden AI" :router t :hosts ("api.edenai.run") :ids ("edenai") :where "FR" :eu :option
     :stored "unknown" :trains :no :zdr :unknown :source "edenai.co/privacy-policy")
    (:name "Portkey" :router t :hosts ("api.portkey.ai") :ids ("portkey") :where "US" :eu :option
     :stored "yes" :trains :no :zdr :request :source "portkey.ai/privacy-policy")
    (:name "Vercel" :router t :hosts ("ai-gateway.vercel.sh") :ids ("vercel") :where "US"
     :stored "unknown" :trains :no :zdr :request :source "vercel.com/legal/privacy-policy")
    (:name "Opper" :router t :hosts ("api.opper.ai") :ids ("opper") :where "SE" :eu :option
     :stored "unknown" :trains :no :zdr :unknown :source "opper.ai/privacy")
    (:name "Venice" :hosts ("api.venice.ai") :ids ("venice") :where "US"
     :stored "no" :trains :no :zdr :yes :source "venice.ai/privacy")
    (:name "Poe" :router t :hosts ("api.poe.com") :ids ("poe") :where "US"
     :stored "unknown" :trains :unknown :zdr :no :source "poe.com/privacy"))
  "Each provider's terms as a plist (see the section head); a host on no row is unknown.")

(defun host-matches-p (host pattern)
  "Whether HOST is PATTERN or under it: a suffix on a dot, or a glob whose * is one label's part."
  (let ((host (string-downcase host)))
    (if (find #\* pattern)
        (and (ppcre:scan (format nil "^(?:[^.]+\\.)*~{~a~^[a-z0-9-]+~}$"
                                 (mapcar #'ppcre:quote-meta-chars (uiop:split-string pattern :separator "*")))
                         host)
             t)
        (or (string= host pattern) (uiop:string-suffix-p host (concatenate 'string "." pattern))))))

(defun provider-terms (host)
  "The *PROVIDER-TERMS* row HOST answers for, else NIL: not on the list."
  (and (stringp host)
       (find-if (lambda (row) (some (lambda (pattern) (host-matches-p host pattern)) (getf row :hosts)))
                *provider-terms*)))

;;; --- what the wire says ---------------------------------------------------------

(defparameter *public-certificate-issuers*
  '("let's encrypt" "google trust" "digicert" "sectigo" "comodo" "globalsign" "amazon" "microsoft"
    "godaddy" "starfield" "entrust" "zerossl" "identrust" "cloudflare" "ssl corporation" "certum"
    "asseco" "buypass" "harica" "actalis" "quovadis" "swisssign" "telia" "trustasia" "geant" "apple")
  "Public certificate authorities: an issuer outside them is a certificate this machine was made to
trust -- a company proxy or an antivirus reading the traffic.")

(defun connection-endpoint (provider model)
  "The URL a turn on PROVIDER's MODEL dials (EFFECTIVE-ENDPOINT's rule), else its base: the host
is what the check measures. NIL when nothing names one."
  (or (ignore-errors (lane-endpoint provider (resolve-model-lane provider model)))
      (ignore-errors (provider-endpoint provider))
      (ignore-errors (provider-base-url provider))))

(defun connection-proxy ()
  "The proxy every provider request goes through, else NIL."
  (let ((proxy dex:*default-proxy*))
    (and (stringp proxy) (plusp (length proxy)) proxy)))

(defun issuer-organization (oneline)
  "The O= of an X509_NAME_oneline string, else the whole string."
  (or (ppcre:register-groups-bind (org) ("/O=([^/]+)" oneline) org) oneline))

(defun tls-peer-facts (stream)
  "STREAM's handshake => (:TLS version :GROUP key exchange :ISSUER organization)."
  (let* ((handle (cl+ssl::ssl-stream-handle stream))
         (group-name (cffi:foreign-symbol-pointer "SSL_get0_group_name"))
         (certificate (cl+ssl:ssl-stream-x509-certificate stream)))
    (list :tls (cffi:foreign-funcall "SSL_get_version" :pointer handle :string)
          :group (and group-name (cffi:foreign-funcall-pointer group-name () :pointer handle :string))
          :issuer (unless (cffi:null-pointer-p certificate)
                    (unwind-protect
                         (issuer-organization
                          (cffi:with-foreign-pointer-as-string (buffer 512)
                            (cffi:foreign-funcall "X509_NAME_oneline"
                                                  :pointer (cffi:foreign-funcall "X509_get_issuer_name"
                                                                                 :pointer certificate :pointer)
                                                  :pointer buffer :int 512 :pointer)))
                      (cffi:foreign-funcall "X509_free" :pointer certificate :void))))))

(defun cloudflare-answers-p (stream host)
  "Whether a bare HEAD over STREAM is answered by Cloudflare's edge: it decrypts before the provider."
  (let ((crlf (coerce '(#\Return #\Newline) 'string)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HEAD / HTTP/1.1~aHost: ~a~aUser-Agent: nodecode~aConnection: close~a~a"
                             crlf host crlf crlf crlf crlf)
                     :external-format :latin-1)
                    stream)
    (force-output stream)
    (let ((head (make-array 8192 :element-type '(unsigned-byte 8))) (end 0))
      (loop for byte = (read-byte stream nil)
            while (and byte (< end (length head)))
            do (setf (aref head end) byte) (incf end)
            until (and (>= end 4) (equalp (subseq head (- end 4) end) #(13 10 13 10))))
      (let ((text (string-downcase (sb-ext:octets-to-string head :end end :external-format :latin-1))))
        (and (or (ppcre:scan "(?m)^server:\\s*cloudflare" text) (ppcre:scan "(?m)^cf-ray:" text)) t)))))

(defun tls-handshake-facts (host port timeout)
  "One verified handshake with HOST:PORT => TLS-PEER-FACTS plus :CLOUDFLARE."
  (sb-sys:with-deadline (:seconds timeout)
    (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8) :timeout timeout))
           (stream nil))
      (unwind-protect
           (progn (setf stream (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                                              :hostname host :verify :required))
                  (append (tls-peer-facts stream) (list :cloudflare (cloudflare-answers-p stream host))))
        (when stream (ignore-errors (close stream :abort t)))
        (ignore-errors (usocket:socket-close socket))))))

(defparameter *connection-probe-timeout* 3
  "Seconds one handshake may take before the check gives up on it: a pick is measured off the
loop, and a host that takes longer is one the next try or the next shell measures.")

(defun connection-probe (url &key (timeout *connection-probe-timeout*))
  "Measure the connection URL's requests take, sending nothing of anyone's => a plist: :HOST, :PROXY,
and :LOCAL, :PLAIN (no TLS), the handshake's facts, :HTTPS alone (the handshake could not be read,
the exchange went through) or :ERROR."
  (let* ((uri (ignore-errors (quri:uri url)))
         (host (and uri (quri:uri-host uri)))
         (base (list :host host :proxy (connection-proxy))))
    (cond ((null host) (list* :error "no endpoint" base))
          ((local-endpoint-p url) (list* :local t base))
          ((not (string-equal (quri:uri-scheme uri) "https")) (list* :plain t base))
          (t (handler-case (append (tls-handshake-facts host (or (quri:uri-port uri) 443) timeout) base)
               (serious-condition (condition)
                 ;; Where this build reads no handshake (WinHTTP), the exchange itself still says
                 ;; it went over HTTPS and who answered.
                 (handler-case
                     (multiple-value-bind (body status headers) (http :head url :timeout timeout)
                       (declare (ignore body))
                       (list* :https (and status t)
                              :cloudflare (and (hash-table-p headers)
                                               (or (gethash "cf-ray" headers)
                                                   (search "cloudflare" (or (gethash "server" headers) "")))
                                               t)
                              base))
                   (serious-condition () (list* :error (princ-to-string condition) base)))))))))

;;; --- the verdict ------------------------------------------------------------------

(defun connection-label (level)
  "The verdict in a word or two: the box's first line, and what the footer's colour means."
  (case level
    (:private "Private")
    ((:not-private :unknown) "Not private")
    (:local "Local")
    (:plain "Not encrypted")
    (t "Not checked yet")))

(defun connection-level-tone (level)
  "LEVEL's colour: green where a prompt reaches only us or this machine, red where someone else
can read it, NIL where nothing was measured."
  (case level
    ((:private :local) :success)
    ((:not-private :unknown :plain) :danger)))

(defun public-issuer-p (issuer)
  (some (lambda (name) (search name (string-downcase issuer))) *public-certificate-issuers*))

(defun stored-phrase (stored)
  "What the terms' STORED says, as the start of a line."
  (cond ((equal stored "no") "Not kept")
        ((equal stored "unknown") "Storage unknown")
        ((equal stored "yes") "Kept")
        ((equal stored "no limit") "Kept with no time limit")
        ((search "cache" stored) (format nil "Kept as a ~a" stored))
        (t (format nil "Kept ~a" stored))))

(defun connection-rows (level probe terms plan)
  "The box's lines, one fact each and the problems grouped: (MARK TEXT), MARK :OK measured or our
own, :CLAIM only the provider's word or not known, :NO exposed, NIL the line naming the source.
Training gets a line only when the terms do not say no."
  (let* ((own (getf terms :own))
         (tls (getf probe :tls))
         (issuer (getf probe :issuer))
         (proxy (getf probe :proxy))
         (cloudflare (getf probe :cloudflare))
         (stored (getf terms :stored))
         (wire (cond (tls (format nil "~a~:[~; · PQC~]" (ppcre:regex-replace "^TLSv" tls "TLS ")
                                  (ppcre:scan "(?i)mlkem|kyber" (or (getf probe :group) ""))))
                     ((getf probe :https) "HTTPS"))))
    (if (eq level :local)
        (list (list :ok "Stays on your machine or network"))
        (remove
         nil
         (list
          (cond ((getf probe :plain) (list :no "Not encrypted: anyone on the way can read it"))
                (proxy (list :no (format nil "~@[~a, ~]through proxy ~a" wire proxy)))
                ((null issuer) (list :claim (format nil "~a, interception unchecked" (or wire "Encryption unchecked"))))
                ((public-issuer-p issuer) (list :ok (format nil "~a, not intercepted" wire)))
                (t (list :claim (format nil "~a, certificate from ~a" wire issuer))))
          (list (if (and own (not cloudflare) (not proxy)) :ok :no)
                (format nil "Readable by ~{~a~^, ~}~@[ · ~a~]"
                        (remove nil (list (and cloudflare "Cloudflare")
                                          (if terms (getf terms :name) (getf probe :host))
                                          (and (getf terms :router) "the provider it picks")))
                        (and terms (format nil "~a~@[ · ~a~]" (getf terms :where)
                                           (case (getf terms :eu) ((t) "EU") (:option "EU option"))))))
          (cond ((null terms) (list :no "Not on our list: storage, training and route unknown"))
                (plan (list :claim (format nil "Your ~a plan's terms are not on our list: storage and training unknown"
                                           (getf terms :name))))
                (t (list (cond ((not (equal stored "no")) :no) (own :ok) (t :claim))
                         (format nil "~a, linked to ~a~@[ (~a)~]"
                                 (stored-phrase stored)
                                 (if own "country and billing only" "your account")
                                 (unless own
                                   (case (getf terms :zdr)
                                     (:yes (and (not (equal stored "no")) "ZDR available"))
                                     (:request "ZDR on request")
                                     (:enterprise "ZDR: enterprise only")
                                     (:no "no ZDR")))))))
          (when (and terms (not plan))
            (case (getf terms :trains)
              (:no nil)
              (:free-tier (list :no "Trained on in the free tier"))
              (:drafts (list :no "Used to train draft models"))
              (t (list :claim "Training on your prompts: unknown"))))
          (and terms (not plan)
               (list nil (format nil "~a · ~a" (getf terms :source) *provider-terms-checked*))))))))

(defun connection-verdict (provider probe)
  "What PROBE, the connection PROVIDER's turns take, means => (:LEVEL :LABEL :HOST :NAME :ROWS):
:PRIVATE only for NODECODE's own endpoint reached directly; :NOT-PRIVATE for a provider on the list,
:UNKNOWN for one that is not; :LOCAL, :PLAIN or :UNCHECKED when the wire said so."
  (let* ((terms (provider-terms (getf probe :host)))
         (plan (and terms (not (member provider (getf terms :ids) :test #'equal))))
         (proxy (getf probe :proxy))
         (level (cond ((getf probe :local) :local)
                      ((getf probe :plain) :plain)
                      ((getf probe :error) :unchecked)
                      ((and (getf terms :own) (not proxy) (not (getf probe :cloudflare))) :private)
                      (terms :not-private)
                      (t :unknown))))
    (list :level level :label (connection-label level) :host (getf probe :host)
          :name (or (getf terms :name) (getf probe :host))
          :rows (connection-rows level probe terms plan))))

;;; --- the shell: measured once a pick, remembered a host, the colour, the box ---------
;;; A pick is measured off the loop when its target lands. The gateway names the endpoint: it is
;;; the process whose cells dial it (a sign-in provider's lane is known there, not here). What a
;;; host measured is kept on disk, so the next shell colours the name at once and measures again
;;; behind it. A check that measured nothing is never kept: it tries again a few times, then
;;; leaves the pick unmeasured for the next time its target lands.

(defparameter *connection-retry-delays* '(2 5 15)
  "Seconds before each further try when a check measured nothing: a fresh shell may learn its
endpoint only once the catalog or the gateway's cells have landed.")

(defparameter *connection-memory-days* 7
  "How long a host's measurement on disk stands in for a fresh one.")

(defvar *connection-memory-path* nil
  "Where measurements are kept, else the home's connection-checks.sexp.")

(defvar *connection-memory* :unread
  "URL => (UNIVERSAL-TIME . PROBE) as kept on disk, :UNREAD until first asked.")

(defvar *connection-memory-lock* (sb-thread:make-mutex :name "connection memory"))

(defun connection-memory-file ()
  (or *connection-memory-path* (nlk:home "connection-checks.sexp")))

(defun connection-memory ()
  "The kept measurements, read from disk once."
  (sb-thread:with-recursive-lock (*connection-memory-lock*)
    (when (eq *connection-memory* :unread)
      (setf *connection-memory*
            (let ((table (make-hash-table :test #'equal))
                  (kept (ignore-errors
                         (with-open-file (in (connection-memory-file) :if-does-not-exist nil)
                           (and in (with-standard-io-syntax
                                     (let ((*read-eval* nil) (*package* (find-package :cl-user)))
                                       (read in nil nil))))))))
              (loop for (url time . probe) in (and (listp kept) kept)
                    when (and (stringp url) (integerp time)) do (setf (gethash url table) (cons time probe)))
              table)))
    *connection-memory*))

(defun remembered-probe (url)
  "What URL's host measured within *CONNECTION-MEMORY-DAYS*, with today's proxy, else NIL."
  (when-let (kept (sb-thread:with-recursive-lock (*connection-memory-lock*)
                    (gethash url (connection-memory))))
    (and (< (- (get-universal-time) (car kept)) (* *connection-memory-days* 86400))
         (list* :proxy (connection-proxy) (alexandria:remove-from-plist (cdr kept) :proxy)))))

(defun remember-probe (url probe)
  "Keep PROBE, a measurement that succeeded, as URL's."
  (sb-thread:with-recursive-lock (*connection-memory-lock*)
    (let ((table (connection-memory)))
      (setf (gethash url table) (cons (get-universal-time) (alexandria:remove-from-plist probe :proxy)))
      (ignore-errors
       (nlk:write-file-atomically
        (connection-memory-file)
        (with-standard-io-syntax
          (let ((*package* (find-package :cl-user)) (*print-readably* nil))
            (prin1-to-string (loop for url being the hash-keys of table using (hash-value (time . probe))
                                   collect (list* url time probe))))))))))

(defvar *connection-checks* (make-hash-table :test #'equal :synchronized t)
  "(PROVIDER . MODEL) => that pick's CONNECTION-VERDICT, or :CHECKING while it is measured.")

(defun connection-key (model)
  "(PROVIDER . MODEL-ID) of the target MODEL's footer names, else NIL."
  (multiple-value-bind (raw-provider raw-model state) (footer-identity model)
    (let ((provider (json-value raw-provider :text))
          (model-id (json-value raw-model :text)))
      (and (eq state :configured) provider (cons provider model-id)))))

(defun connection-target-endpoint (model key)
  "The URL KEY's turns dial: what the gateway answered for that target, else this process's own
reading of its lane."
  (let ((facts (gateway-model-facts model)))
    (or (and (equal (getf facts :provider) (car key)) (equal (getf facts :model) (cdr key))
             (getf facts :endpoint))
        (connection-endpoint (car key) (cdr key)))))

(defun measure-connection (model key retry-delays)
  "KEY's verdict from a fresh handshake, trying again after each of RETRY-DELAYS while there is no
endpoint or the handshake fails => NIL when no try measured anything."
  (loop for delay in (cons 0 retry-delays)
        do (when (plusp delay) (sleep delay))
           (let* ((url (connection-target-endpoint model key))
                  (probe (and url (connection-probe url :timeout *connection-probe-timeout*))))
             (when (and probe (not (getf probe :error)))
               (remember-probe url probe)
               (return (connection-verdict (car key) probe))))))

(defun check-connection (model)
  "Measure the connection MODEL's chosen target takes, off the loop, unless this pick has been:
once when a model is chosen, before anyone prompts it. A host measured before colours it at once."
  (when-let (key (connection-key model))
    (unless (gethash key *connection-checks*)
      (let* ((url (connection-target-endpoint model key))
             (kept (and url (remembered-probe url))))
        (setf (gethash key *connection-checks*) (if kept (connection-verdict (car key) kept) :checking)))
      (off-loop model
                ;; Retries wait, so only on the thread OFF-LOOP spawns, never on the loop's own.
                (lambda () (measure-connection model key (and (chat-model-program model)
                                                             *connection-retry-delays*)))
                (lambda (model value)
                  (cond ((and (consp value) (not (off-loop-failure value)))
                         (setf (gethash key *connection-checks*) value))
                        ;; Nothing measured: a kept answer stands, else the pick stays unmeasured.
                        ((eq :checking (gethash key *connection-checks*))
                         (remhash key *connection-checks*)))
                  (render-state model))))))

(defun connection-identity-tone (model)
  "The footer's model name's colour (asked 2026-10-27: the name itself, no knob): green when the
chosen model's connection is private, red when it is not, NIL -- the footer's own -- while it is
measured or when it could not be."
  (let* ((key (connection-key model))
         (check (and key (gethash key *connection-checks*))))
    (and (consp check) (connection-level-tone (getf check :level)))))

(defun connection-dialog-rows (key check)
  "The box: the pick KEY names, whose provider and its verdict, then CHECK's facts a line each with
their mark, then where the terms come from. No blank lines."
  (when (consp check)
    (cons (make-picker-row :heading t :value (list :connection)
                           :tone (connection-level-tone (getf check :level))
                           :label (format nil "~@[~a · ~]~a — ~a" (cdr key) (getf check :name) (getf check :label)))
          (loop for (mark text) in (getf check :rows)
                collect (make-picker-row :heading t :value (list :connection) :label text
                                         :mark (case mark (:ok "✓") (:no "✗") (:claim "?") (t " "))
                                         :mark-tone (case mark (:ok :success) (:no :danger) (:claim :tool) (t :muted))
                                         :tone (and (null mark) :muted))))))

(defun slash-privacy-op (model)
  "Open the box: what the check measured for this pick and what the provider's terms say."
  (let* ((key (connection-key model))
         (check (and key (gethash key *connection-checks*))))
    (open-picker-dialog model "Connection"
                        (connection-dialog-rows key check)
                        (constantly nil)
                        :kind :privacy
                        ;; Nothing here searches or sums up, so the header is the title and one
                        ;; blank: the verdict starts one row under the title, not three.
                        :header-rows 2
                        :empty-label (cond ((eq check :checking) "Checking…")
                                           (key "Not checked yet")
                                           (t "No model chosen yet"))
                        :hint "esc close")
    nil))
