;;;; speech.lisp --- an answer said out loud.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The twin of transcribe.lisp, pointing the other way: SYNTHESIZE-SPEECH
;;;; turns the words a turn answered with into WAV octets, over one of two
;;;; engines, and knows nothing about where they will be played.
;;;;
;;;;   local  sherpa-onnx's offline TTS running a Piper VITS voice. No key,
;;;;          no network, nothing leaves the machine. Nothing ships with the
;;;;          kit: INSTALL-SPEAKER fetches the pinned engine and voice into
;;;;          the cache once, each checked against its digest.
;;;;   http   an OpenAI-compatible POST <base_url>/audio/speech when the
;;;;          speech section names a base_url, with the operator's key.
;;;;
;;;; A refusal is an error whose message is the honest reason. The caller
;;;; decides what a room is told; text stays the answer either way, so a
;;;; voice that cannot speak costs a line, never the turn.

(in-package #:nodecode-channel-kit)

;;; SETF by START-CELL, never LET: synthesis runs on the delivery pool.
(defvar *speech* nil
  "The speech section of the config the kit started with, as a hash table —
NIL when absent, which is the local engine at its defaults.")

(nlk:define-section ("speech")
  (:guide "What a turn answers with, said out loud — the voice channels speak with; locally by default: (nck:install-speaker) fetches the engine and one voice once (about 95 MB) and the words never leave the machine; or name a hosted OpenAI-compatible API with base_url, model and its key — Groq https://api.groq.com/openai/v1 with canopylabs/orpheus-v1-english, OpenAI https://api.openai.com/v1 with gpt-4o-mini-tts")
  ("enabled" :boolean :default t
             :doc "say an answer out loud where a surface can play it; false leaves the answer in text alone")
  ("max_characters" :integer :default 1200
                    :doc "the longest answer spoken; a longer one is cut at a sentence and said with a note that the rest is in text")
  ("speed_percent" :integer :default 100 :min 50
                   :doc "how fast the local voice speaks, as a percentage of its own pace")
  ("base_url" :string
              :doc "an OpenAI-compatible API root ending in /v1; absent, the local voice speaks")
  ("model" :string :doc "the model base_url serves")
  ("voice" :string :doc "the voice base_url should use")
  ("api_key_env" :env :doc "environment variable holding the API key")
  ("api_key_file" :path :doc "file holding the API key"))

;;; Past it the words are cut at a sentence: a voice channel is a
;;; conversation, and a spoken wall of text is unlistenable however patiently
;;; it is rendered.
(defparameter +speech-max-characters+ 1200
  "The longest answer spoken when the section names no max_characters.")

;;; --- the local engine ----------------------------------------------------------

;;; Tests bind a scratch folder.
(nlk:define-startup-parameter *speaker-directory*
    (nlk:cache-path "nodecode/speaker/")
  "Where INSTALL-SPEAKER puts the local engine and its voice: the cache,
since both are fetched again from their pins, shared by every profile.")

(defparameter +speaker-engines+
  '(((:linux :x64) "sherpa-onnx-v1.13.8-linux-x64-shared"
     "c0bdb7907d3a74bba1d55d22bf4d9fa75586cf1530614ebe88a27b9118e015c4")
    ((:linux :arm64) "sherpa-onnx-v1.13.8-linux-aarch64-shared-cpu"
     "4e3734f82bc1379fd91f219f5869c7e9d03b7a4f7561907d8abca4849c51a789")
    ((:darwin :arm64) "sherpa-onnx-v1.13.8-osx-arm64-shared"
     "b10e5c7e2c30ea03de9c442655d14860d9edc475c6251d58a8f5f06e913a1d56")
    ((:darwin :x64) "sherpa-onnx-v1.13.8-osx-x64-shared"
     "54aad64acee9d2d596535a6080d6f22602a720af5460e1d50461b1e1b06bee40"))
  "((PLATFORM ARCHITECTURE) ARCHIVE SHA256) of the sherpa-onnx build to speak
with: the same shared-library release the transcriber uses, minus the
-no-tts trim, so it carries sherpa-onnx-offline-tts (about 28 MB).")

;;; MIT; fetched from upstream on the operator's word, never shipped.
(defparameter +speaker-voice+
  '("vits-piper-en_US-lessac-medium"
    "9e3febfacf0abf4270172d2958bcec246032b7e88efc2720840cc80c93de334e")
  "The voice that speaks by default: Piper's en_US lessac medium, converted
for sherpa-onnx (67 MB).")

(defparameter +speaker-voice-url+
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/~a.tar.bz2")

(defun local-speaker (&aux (engine (machine-build +speaker-engines+))
                           (voice (first +speaker-voice+)))
  "(values BINARY VOICE-DIRECTORY LIBRARY-DIRECTORY) of the local speaker
installed on this machine, or NIL while any part is missing."
  (when (and engine
             (archive-installed-p engine *speaker-directory*)
             (archive-installed-p voice *speaker-directory*))
    (values (merge-pathnames (format nil "~a/bin/sherpa-onnx-offline-tts" engine)
                             *speaker-directory*)
            (merge-pathnames (format nil "~a/" voice) *speaker-directory*)
            (merge-pathnames (format nil "~a/lib/" engine) *speaker-directory*))))

(defun install-speaker ()
  "Fetch the local speaker into *SPEAKER-DIRECTORY* — the sherpa-onnx engine
built for this machine (about 28 MB) and one Piper voice (67 MB), each
checked against its pinned digest — once; a half already there is kept."
  ;; => a text naming where it is. Signals when this machine has no engine build
  ;; or a fetch fails. Blocks for the download.
  (multiple-value-bind (engine engine-sha256) (machine-build +speaker-engines+)
    (unless engine
      (error "no local speaker is built for ~(~a ~a~): name a hosted one with speech.base_url"
             (nlk:platform) (nlk:architecture)))
    ;; The engine is the transcriber's own release, with its TTS half.
    (install-pinned-archive engine engine-sha256 +transcriber-engine-url+ *speaker-directory*)
    (destructuring-bind (voice voice-sha256) +speaker-voice+
      (install-pinned-archive voice voice-sha256 +speaker-voice-url+ *speaker-directory*))
    (format nil "the local speaker is installed under ~a: Piper's en_US lessac ~
                 medium voice (MIT) run by sherpa-onnx (Apache-2.0)"
            (uiop:native-namestring *speaker-directory*))))

;;; --- what is worth saying ------------------------------------------------------

(defparameter +sentence-enders+ '(#\. #\! #\? #\Newline))

(defun spoken-text (text max-characters &aux (clean (speakable-text text)))
  "TEXT as a voice should read it: the markdown a room renders is noise out
loud, and an answer past MAX-CHARACTERS is cut at the last sentence that
fits. => (values SAID CUT-P)."
  (if (<= (length clean) max-characters)
      (values clean nil)
      (let* ((window (subseq clean 0 max-characters))
             (cut (position-if (lambda (c) (member c +sentence-enders+))
                               window :from-end t)))
        (values (string-right-trim '(#\Space #\Newline)
                                   (subseq window 0 (if (and cut (> cut 80)) (1+ cut) nil)))
                t))))

(defun speakable-text (text)
  "TEXT with the marks that are for eyes taken out: fenced code, inline
backticks, emphasis, link syntax and heading hashes."
  ;; What is left is what a person would read aloud. One pass, marks in turn: a
  ;; fenced block, open to the end when unclosed, is not read out but its name
  ;; is; a line's heading hashes and the spaces after them; a backtick; an
  ;; underscore; an asterisk, unless the text ends on it.
  (string-trim '(#\Space #\Newline #\Tab)
               (ppcre:regex-replace-all
                "(?s)```.*?(?:```|\\z)|(?m:^#[# ]*)|`|_|\\*(?!\\z)" text
                (lambda (mark) (if (uiop:string-prefix-p "```" mark) "(code) " ""))
                :simple-calls t)))


;;; --- speaking ------------------------------------------------------------------

(defvar *local-synthesis-lock* (bt2:make-lock :name "nodecode-speaker")
  "One recording at a time: the engine loads its voice per run and two at
once would double a peak the machine was sized for once.")

(defun synthesize-locally (text speed)
  "TEXT as WAV octets, spoken by the installed local voice."
  (multiple-value-bind (binary voice library) (local-speaker)
    (unless binary
      (error "no speaker is installed: (nck:install-speaker) fetches the local ~
              one once (about 95 MB), or speech.base_url names a hosted one"))
    (bt2:with-lock-held (*local-synthesis-lock*)
      (call-with-scratch-directory
       (lambda (directory &aux (wave (merge-pathnames "speech.wav" directory)))
         (flet ((voice-file (name) (uiop:native-namestring
                                    (merge-pathnames name voice))))
           (nlk:bind (((_ err status)
                       (nlk:run-bounded
                        (list (uiop:native-namestring binary)
                              (format nil "--vits-model=~a" (voice-file "en_US-lessac-medium.onnx"))
                              (format nil "--vits-tokens=~a" (voice-file "tokens.txt"))
                              (format nil "--vits-data-dir=~a" (voice-file "espeak-ng-data"))
                              (format nil "--speed=~,2f" speed)
                              "--num-threads=2"
                              (format nil "--output-filename=~a" (uiop:native-namestring wave))
                              text)
                        :seconds 120
                        :environment (list (format nil "LD_LIBRARY_PATH=~a"
                                                   (uiop:native-namestring library))))))
             (unless (and (eql status 0) (probe-file wave))
               (error "the local speaker ~:[failed~@[: ~a~]~;took too long~]"
                      (eq status :timeout) (last-line err)))
             (read-octets wave))))))))

(defun synthesize-over-http (section text)
  "TEXT as the audio the OpenAI-compatible API at SECTION's base_url speaks."
  (let* ((base (string-right-trim "/" (config-string section "base_url" "")))
         (model (or (config-string section "model" nil)
                    (error "speech.base_url is set but speech.model names no model")))
         (voice (config-string section "voice" "alloy"))
         (key (resolve-channel-secret section "api_key")))
    (multiple-value-bind (body status)
        (nlk:http :post (format nil "~a/audio/speech" base)
                  :headers (append (when key
                                     (list (cons "Authorization" (format nil "Bearer ~a" key))))
                                   '(("Content-Type" . "application/json")))
                  :content (nlk:encode-json-object
                            (nlk:json-object "model" model "input" text
                                             "voice" voice "response_format" "wav"))
                  :binary t :connect-timeout 20 :timeout 180 :pool t)
      (when (>= status 400)
        (error "~a refused the words: ~a ~a" base status (nlk:clip (nlk:body-text body) 200)))
      (let ((octets (if (stringp body)
                        (sb-ext:string-to-octets body :external-format :latin-1)
                        body)))
        (unless (equal "wav" (audio-container octets))
          (error "~a answered ~a bytes that are not audio" base (length octets)))
        octets))))

(defun speech-enabled-p (&optional (section *speech*))
  "Whether an answer may be spoken at all."
  ;; An absent section is the local engine at its defaults, which is on; an
  ;; explicit false is off.
  (or (null section) (and (config-boolean section "enabled" t) t)))

(defun synthesize-speech (text &key (section *speech*))
  "(values WAV-OCTETS SAID CUT-P): TEXT spoken, the words that were spoken,
and whether the rest was left in text."
  ;; Signals with an honest reason when no engine can speak — the caller
  ;; reports it once and keeps the text answer, which is the authority either
  ;; way.
  (unless (speech-enabled-p section)
    (error "speech is turned off (speech.enabled)"))
  (let ((words (nlk:trimmed (or text ""))))
    (when (zerop (length words))
      (error "there is nothing to say"))
    (multiple-value-bind (said cut-p)
        (spoken-text words (config-integer section "max_characters"
                                           +speech-max-characters+))
      (when (zerop (length said))
        (error "the answer has no words a voice can read"))
      (values (if (and section (config-string section "base_url" nil))
                  (synthesize-over-http section said)
                  (synthesize-locally said (/ (config-integer section "speed_percent" 100)
                                              100.0)))
              said
              cut-p))))

;;; --- an answer as a voice message ------------------------------------------------
;;; Hermes' /voice on, tts and off (gateway/run_voice.py at 6f7a7991bb): a room
;;; can hear its answers as well as read them, as the platform's own voice
;;; message under the words — Discord's flagged attachment with its waveform,
;;; Telegram's sendVoice. The words stay the answer; the recording is a copy.

(defparameter +voice-replies+
  '(("off" . "answers here are words alone")
    ("on" . "an ask said in a voice message is answered in one too, below the words")
    ("tts" . "every answer here comes with a voice message too, below the words"))
  "What a room's answers carry besides their words, by the word that sets it
(channels.<id>.voice_replies, /voice on, tts, off), with what it means.")

(defparameter +voice-waveform-points+ 256
  "How many bars a voice message draws: Discord's 256, an octet each.")

(defun wav-shape (octets &aux (rate 0) (channels 1) (bits 0) (data 0) (end 0))
  "(values SECONDS WAVEFORM) of WAV OCTETS: how long they play, and
+VOICE-WAVEFORM-POINTS+ octets, each how loud its stretch of the recording
is, the loudest 255. A recording that is not 16-bit PCM draws flat."
  ;; A streamed WAV declares its data as long as it could be (#xFFFFFFFF), so
  ;; the data ends where the octets do.
  (flet ((u16 (at) (logior (aref octets at) (ash (aref octets (1+ at)) 8))))
    (flet ((u32 (at) (logior (u16 at) (ash (u16 (+ at 2)) 16))))
      (loop with at = 12
            while (<= (+ at 8) (length octets))
            do (let ((id (map 'string #'code-char (subseq octets at (+ at 4))))
                     (size (u32 (+ at 4))))
                 (cond ((string= id "fmt ")
                        (setf channels (max 1 (u16 (+ at 10))) rate (u32 (+ at 12))
                              bits (u16 (+ at 22))))
                       ((string= id "data")
                        (setf data (+ at 8) end (min (length octets) (+ at 8 size)))
                        (loop-finish)))
                 (incf at (+ 8 size (logand size 1)))))
      (let* ((block (* channels (max 1 (floor bits 8))))
             (frames (floor (- end data) block))
             (loudness (make-array +voice-waveform-points+ :initial-element 0)))
        (when (and (= bits 16) (plusp frames))
          (dotimes (point +voice-waveform-points+)
            (loop with from = (floor (* point frames) +voice-waveform-points+)
                  with to = (max (1+ from) (floor (* (1+ point) frames) +voice-waveform-points+))
                  for frame from from below (min to frames)
                  for sample = (u16 (+ data (* frame block)))
                  sum (abs (if (>= sample 32768) (- sample 65536) sample)) into total
                  count t into counted
                  finally (setf (aref loudness point) (/ total (max 1 counted))))))
        (let ((peak (reduce #'max loudness)))
          (values (if (plusp rate) (/ frames rate 1.0) 0.0)
                  (map '(vector (unsigned-byte 8))
                       (lambda (level) (if (plusp peak) (round (* 255 level) peak) 0))
                       loudness)))))))

(defun wav-to-ogg-opus (wav target &key (channels 2))
  "WAV octets encoded as an Ogg Opus file at TARGET — 48 kHz, CHANNELS
channels, 20 ms frames, what Discord's voice carries. => TARGET."
  ;; ffmpeg does the encoding and the container does the framing, so no Opus
  ;; implementation is needed here.
  (unless (ffmpeg-present-p)
    (error "ffmpeg is not installed, and a spoken answer is encoded with it"))
  (let ((source (write-octets wav (make-pathname :name "speech" :type "wav" :defaults target))))
    (nlk:bind (((_ err status)
                (nlk:run-bounded (list "ffmpeg" "-nostdin" "-hide_banner" "-loglevel" "error"
                                       "-i" (uiop:native-namestring source)
                                       "-ac" (princ-to-string channels) "-ar" "48000"
                                       "-c:a" "libopus" "-b:a" "64k" "-frame_duration" "20"
                                       "-application" "voip"
                                       "-y" (uiop:native-namestring target))
                                 :seconds 120)))
      (unless (and (eql status 0) (probe-file target))
        (error "the answer did not encode as Opus: ~a" (nlk:one-line err))))
    target))

(defun voice-message (text directory)
  "TEXT said as a voice message in DIRECTORY: (values PATHNAME SECONDS
WAVEFORM CUT-P), the Ogg Opus file, how long it plays, how it looks
(WAV-SHAPE), and whether the rest of TEXT was left in words."
  ;; Signals with SYNTHESIZE-SPEECH's honest reason when nothing can speak.
  (nlk:bind (((wav _ cut-p) (synthesize-speech text))
             ((seconds waveform) (wav-shape wav)))
    (values (wav-to-ogg-opus wav (merge-pathnames "voice-message.ogg" directory) :channels 1)
            seconds waveform cut-p)))
