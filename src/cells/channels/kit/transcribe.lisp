;;;; transcribe.lisp --- audio read as the words it says.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A person who speaks to a room instead of typing is still talking to it: a
;;;; voice note reaches the lane as its transcript, read at the place in the
;;;; prompt the message was said (HOST ASK-PROMPT-AND-ATTACHMENTS). This file
;;;; is where audio becomes text — TRANSCRIBE-AUDIO, over one of two engines:
;;;;
;;;;   local  sherpa-onnx's offline recognizer running NVIDIA Parakeet TDT
;;;;          0.6B v3 (int8; 25 European languages, the language found on its
;;;;          own), a child process per recording after ffmpeg decodes it to
;;;;          16 kHz mono. The audio never leaves the machine. Nothing ships
;;;;          with the kit: INSTALL-TRANSCRIBER fetches the pinned engine and
;;;;          model into the cache once, each checked against its digest.
;;;;   http   an OpenAI-compatible POST <base_url>/audio/transcriptions —
;;;;          Groq, OpenAI, Mistral, a self-hosted server — when the
;;;;          transcription section names a base_url, with the operator's key.
;;;;
;;;; The default cap is five minutes.
;;;;
;;;; Every refusal is an error whose message is the honest reason: the ingress
;;;; renders it as the one bracketed note that stands for the recording.

(in-package #:nodecode-channel-kit)

;;; SETF by START-CELL, never LET: the transcription runs on the delivery
;;; pool.
(defvar *transcription* nil
  "The transcription section of the config the kit started with, as a hash
table — NIL when absent, which is the local engine at its defaults.")

(nlk:define-section ("transcription")
  (:guide "Voice messages and audio files the channels receive reach the lane as their transcript; locally by default: (nck:install-transcriber) fetches the engine and its model once (about 510 MB) and needs ffmpeg installed, and the audio never leaves the machine; or name a hosted OpenAI-compatible API with base_url, model and its key — Groq https://api.groq.com/openai/v1 with whisper-large-v3-turbo, OpenAI https://api.openai.com/v1 with gpt-4o-mini-transcribe")
  ("enabled" :boolean :default t
             :doc "transcribe the audio a channel message carries; false leaves a note in its place")
  ("max_seconds" :integer :default 300
                 :doc "the longest recording transcribed; a longer one leaves a note")
  ("base_url" :string
              :doc "an OpenAI-compatible API root ending in /v1; absent, the local transcriber runs")
  ("model" :string :doc "the model base_url serves")
  ("api_key_env" :env :doc "environment variable holding the API key")
  ("api_key_file" :path :doc "file holding the API key"))

(defparameter +transcription-max-seconds+ 300
  "The longest recording transcribed when the section names no max_seconds.")

(defparameter +audio-max-bytes+ (* 25 1024 1024)
  "The most bytes one recording may take: the ceiling hosted transcription
APIs share.")

(defun duration-text (seconds)
  "SECONDS as a person reads a recording's length: 5.5s under a minute, 1:05
past it. NIL for NIL."
  (when (realp seconds)
    (if (< seconds 60)
        (format nil "~,1fs" seconds)
        (multiple-value-bind (minutes rest) (floor (round seconds) 60)
          (format nil "~d:~2,'0d" minutes rest)))))

;;; --- what the bytes are ------------------------------------------------------

(defun audio-container (octets)
  "The container OCTETS open as — \"ogg\" \"wav\" \"flac\" \"mp3\" \"aac\"
\"mp4\" \"webm\" — sniffed from the bytes, never the platform's word; NIL
for bytes no audio file starts with."
  ;; An extension, because the one thing the name is for is the file name
  ;; ffmpeg and a transcription API recognise it by. Video containers count:
  ;; their sound is what is transcribed.
  (flet ((at (offset &rest bytes)
           (and (>= (length octets) (+ offset (length bytes)))
                (loop for byte in bytes
                      for index from offset
                      always (= byte (aref octets index))))))
    (cond ((at 0 #x4F #x67 #x67 #x53) "ogg")
          ((and (at 0 #x52 #x49 #x46 #x46) (at 8 #x57 #x41 #x56 #x45)) "wav")
          ((at 0 #x66 #x4C #x61 #x43) "flac")
          ((at 0 #x49 #x44 #x33) "mp3")
          ;; An MPEG frame sync: layer bits 00 is ADTS AAC, anything else MP3.
          ((and (>= (length octets) 2)
                (= #xFF (aref octets 0))
                (= #xE0 (logand #xE0 (aref octets 1))))
           (if (zerop (logand #x06 (aref octets 1))) "aac" "mp3"))
          ((at 4 #x66 #x74 #x79 #x70) "mp4")
          ((at 0 #x1A #x45 #xDF #xA3) "webm"))))

;;; --- the local engine ----------------------------------------------------------

;;; Tests bind a scratch folder.
(nlk:define-startup-parameter *transcriber-directory*
    (nlk:cache-path "nodecode/transcriber/")
  "Where INSTALL-TRANSCRIBER puts the local engine and its model: the cache,
since both are fetched again from their pins, shared by every profile.")

;;; The archive name is also the folder it opens into, and the digest is
;;; GitHub's for the release asset.
(defparameter +transcriber-engines+
  '(((:linux :x64) "sherpa-onnx-v1.13.8-linux-x64-shared-no-tts"
     "d0f96c8b65c6cd0974fada22737e337de81bc8cd2abbec2e39caf358b1eec5fc")
    ((:linux :arm64) "sherpa-onnx-v1.13.8-linux-aarch64-shared-cpu"
     "4e3734f82bc1379fd91f219f5869c7e9d03b7a4f7561907d8abca4849c51a789")
    ((:darwin :arm64) "sherpa-onnx-v1.13.8-osx-arm64-shared-no-tts"
     "91b96512c4fa1960f8a9ed5360a6c8dda53a4b5015d0590244f14086a234557a")
    ((:darwin :x64) "sherpa-onnx-v1.13.8-osx-x64-shared-no-tts"
     "03fd4cffd98b239d74b9253c270ff637661adb4d51a5a6c9e1f7486e48306db3"))
  "((PLATFORM ARCHITECTURE) ARCHIVE SHA256) per machine the engine is built
for: sherpa-onnx v1.13.8's shared-library release, the smallest carrying the
offline recognizer (18-28 MB).")

(defparameter +transcriber-engine-url+
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/v1.13.8/~a.tar.bz2")

;;; CC-BY-4.0: fetched from upstream on the operator's word, never shipped,
;;; and credited where INSTALL-TRANSCRIBER reports it.
(defparameter +transcriber-model+
  '("sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8"
    "5793d0fd397c5778d2cf2126994d58e9d56b1be7c04d13c7a15bb1b4eafb16bf")
  "(ARCHIVE SHA256) of the model: NVIDIA Parakeet TDT 0.6B v3 exported int8
for sherpa-onnx (487 MB).")

(defparameter +transcriber-model-url+
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/~a.tar.bz2")

(defun local-transcriber (&aux (engine (machine-build +transcriber-engines+))
                               (model (first +transcriber-model+)))
  "(values BINARY MODEL-DIRECTORY) of the local transcriber installed on this
machine, or NIL while either half is missing."
  (when (and engine
             (archive-installed-p engine *transcriber-directory*)
             (archive-installed-p model *transcriber-directory*))
    (values (merge-pathnames (format nil "~a/bin/sherpa-onnx-offline" engine)
                             *transcriber-directory*)
            (merge-pathnames (format nil "~a/" model) *transcriber-directory*))))

(defun install-transcriber ()
  "Fetch the local transcriber into *TRANSCRIBER-DIRECTORY* — the sherpa-onnx
engine built for this machine (about 25 MB) and the Parakeet model (487 MB),
each checked against its pinned digest — once; a half already there is kept."
  ;; => a text naming where it is and what it still needs. Signals when this
  ;; machine has no engine build or a fetch fails. Blocks for the download: a
  ;; minute or two on a fast line.
  (multiple-value-bind (engine engine-sha256) (machine-build +transcriber-engines+)
    (unless engine
      (error "no local transcriber is built for ~(~a ~a~): name a hosted one with transcription.base_url"
             (nlk:platform) (nlk:architecture)))
    (install-pinned-archive engine engine-sha256 +transcriber-engine-url+
                            *transcriber-directory*)
    (destructuring-bind (model model-sha256) +transcriber-model+
      (install-pinned-archive model model-sha256 +transcriber-model-url+
                              *transcriber-directory*))
    (format nil "the local transcriber is installed under ~a: NVIDIA Parakeet TDT ~
                 0.6B v3 (CC-BY-4.0) run by sherpa-onnx (Apache-2.0)~:[; ffmpeg is ~
                 not installed, and it decodes every recording first: install it~;~]"
            (uiop:native-namestring *transcriber-directory*)
            (ffmpeg-present-p))))

(defun last-line (text)
  "TEXT's last non-blank line, or NIL — the line a failing tool says why on."
  (let ((lines (remove-if (lambda (line) (zerop (length (string-trim " " line))))
                          (uiop:split-string (or text "") :separator '(#\Newline)))))
    (and lines (nlk:clip (string-trim " " (car (last lines))) 200))))

(defun sherpa-transcript (output)
  "The text the offline recognizer printed: the \"text\" member of the JSON
object it writes on a line of its own."
  (dolist (line (uiop:split-string (or output "") :separator '(#\Newline))
                (error "the local transcriber printed no transcript"))
    (when (and (plusp (length line)) (char= #\{ (char line 0)))
      (return (string-trim " " (or (nlk:json-value (nlk:decode-json line) :string "text")
                                   ""))))))

;;; Each run takes every core it is given and up to 3 GB for a 5-minute
;;; recording; two at once would take twice that for no sooner answer.
(defvar *local-transcription-lock* (bt2:make-lock :name "local transcription")
  "Held while the local transcriber runs: one recording at a time in this
process.")

(defun transcribe-locally (octets container max-seconds)
  "(values TEXT SECONDS): OCTETS, a CONTAINER recording, decoded by ffmpeg and
read by the local transcriber, one recording at a time."
  ;; Refuses a recording past MAX-SECONDS, found by decoding one second past
  ;; the cap rather than the whole file.
  (multiple-value-bind (binary model) (local-transcriber)
    (unless binary
      (error "no transcriber is installed: (nck:install-transcriber) fetches the ~
              local one once (about 510 MB), or transcription.base_url names a hosted one"))
    (bt2:with-lock-held (*local-transcription-lock*)
      (call-with-scratch-directory
     (lambda (directory)
       (let ((source (write-octets octets (merge-pathnames (format nil "audio.~a" container)
                                                           directory)))
             (wave (merge-pathnames "audio.wav" directory)))
         (multiple-value-bind (out err status)
             (handler-case
                 (nlk:run-bounded (list "ffmpeg" "-nostdin" "-hide_banner" "-loglevel" "error"
                                        "-i" (uiop:native-namestring source)
                                        "-t" (princ-to-string (1+ max-seconds))
                                        "-vn" "-ac" "1" "-ar" "16000" "-c:a" "pcm_s16le"
                                        "-y" (uiop:native-namestring wave))
                                  :seconds 120)
               (error ()
                 (error "ffmpeg is not installed, and the local transcriber decodes with it")))
           (declare (ignore out))
           (unless (and (eql status 0) (probe-file wave))
             (error "its bytes do not decode as audio~@[: ~a~]" (last-line err))))
         ;; 16 kHz mono 16-bit is 32,000 bytes a second after the header.
         (let ((seconds (/ (max 0 (- (nlk:file-bytes wave) 44)) 32000.0)))
           (when (> seconds max-seconds)
             (error "longer than the ~a cap (transcription.max_seconds)"
                    (duration-text max-seconds)))
           (flet ((model-file (name)
                    (uiop:native-namestring (merge-pathnames name model))))
             (multiple-value-bind (out err status)
                 (nlk:run-bounded
                  (list (uiop:native-namestring binary)
                        (format nil "--encoder=~a" (model-file "encoder.int8.onnx"))
                        (format nil "--decoder=~a" (model-file "decoder.int8.onnx"))
                        (format nil "--joiner=~a" (model-file "joiner.int8.onnx"))
                        (format nil "--tokens=~a" (model-file "tokens.txt"))
                        "--model-type=nemo_transducer" "--num-threads=4"
                        (uiop:native-namestring wave))
                  :seconds (+ 60 (ceiling seconds)))
               (unless (eql status 0)
                 (error "the local transcriber ~:[failed~@[: ~a~]~;took too long~]"
                        (eq status :timeout) (last-line err)))
               (values (sherpa-transcript out) seconds))))))))))

;;; --- a hosted engine -----------------------------------------------------------

(defun transcribe-over-http (section octets container)
  "OCTETS, a CONTAINER recording, as the text the OpenAI-compatible API at
SECTION's base_url reads it as: one multipart POST to /audio/transcriptions
carrying the model, the file under its container's name, and the key as a
bearer token when the section names one. The key never reaches a refusal."
  (let* ((base (string-right-trim "/" (config-string section "base_url")))
         (model (or (config-string section "model")
                    (config-error "transcription.model is required beside base_url: the model the API serves")))
         (key (and (or (config-string section "api_key_env")
                       (config-string section "api_key_file"))
                   (resolve-channel-secret section "api_key"))))
    (call-with-scratch-directory
     (lambda (directory)
       (let ((file (write-octets octets (merge-pathnames (format nil "audio.~a" container)
                                                         directory))))
         (multiple-value-bind (body status)
             (nlk:http :post (format nil "~a/audio/transcriptions" base)
                       :headers (and key (list (cons "Authorization" (format nil "Bearer ~a" key))))
                       :content (list (cons "model" model)
                                      (cons "response_format" "json")
                                      (cons "file" file))
                       :timeout 120)
           (when (>= status 400)
             (error "~a answered HTTP ~a~@[: ~a~]" base status
                    (and (stringp body) (plusp (length body)) (nlk:clip (nlk:one-line body) 200))))
           (let ((text (nlk:json-value (handler-case (nlk:decode-json body) (error () nil))
                                       :string "text")))
             (unless text
               (error "~a answered without a transcript" base))
             (string-trim " " text))))))))

;;; --- the seam --------------------------------------------------------------------

(defun transcription-refusal (seconds &aux (section *transcription*))
  "Why a recording SECONDS long (NIL when unknown) is not transcribed here,
before anything is fetched or decoded — or NIL when nothing stands in its way
yet."
  (cond ((not (config-boolean section "enabled" t))
         "transcription is off here (transcription.enabled is false)")
        ((and (realp seconds)
              (> seconds (config-integer section "max_seconds" +transcription-max-seconds+)))
         (format nil "~a long, over the ~a cap (transcription.max_seconds)"
                 (duration-text seconds)
                 (duration-text (config-integer section "max_seconds"
                                                +transcription-max-seconds+))))))

(defun transcribe-audio (octets container &key seconds)
  "(values TEXT SECONDS): OCTETS, a recording in CONTAINER (AUDIO-CONTAINER's
name for it), as the words it says — \"\" when it holds no speech — and its
length when known: decoded, or SECONDS as the platform declared it."
  ;; The engine is the transcription section's: a hosted API when it names a
  ;; base_url, else the local transcriber. Signals with the honest reason for
  ;; every refusal: off, too long, too large, nothing installed, undecodable,
  ;; the engine failing.
  (let ((refusal (transcription-refusal seconds))
        (section *transcription*))
    (when refusal
      (error "~a" refusal))
    (when (> (length octets) +audio-max-bytes+)
      (error "~:d bytes, over the ~:d-byte ceiling one recording may take"
             (length octets) +audio-max-bytes+))
    (if (config-string section "base_url")
        (values (transcribe-over-http section octets container) seconds)
        (transcribe-locally octets container
                            (config-integer section "max_seconds"
                                            +transcription-max-seconds+)))))
