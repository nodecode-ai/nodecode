;;;; transcribe-test.lisp --- a recording read as the words it says.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No network, no model: the local engine is a stub script standing where
;;;; INSTALL-TRANSCRIBER puts the real one, so the child process, its
;;;; arguments and its output are the real seam, and the fixture recording
;;;; (voice.ogg, one second of Opus tone) goes through the real ffmpeg when
;;;; the machine has one. The hosted engine is a stubbed POST.

(in-package #:nodecode.test)

(defun fixture-recording ()
  "voice.ogg's bytes: one second of a 440 Hz tone, Opus mono 48 kHz, the
shape of a Discord voice message."
  (nck:read-octets (asdf:system-relative-pathname :nodecode-channel-kit
                                                 "test/voice.ogg")))

(defun octets-of (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun install-stub-transcriber (directory output)
  "A local transcriber under DIRECTORY that loads nothing: a script where the
engine binary goes, printing OUTPUT on stdout and writing the arguments it
was given to args.txt beside it, and the two installed markers."
  (let* ((engine (nck::machine-build nck::+transcriber-engines+))
         (model (first nck::+transcriber-model+))
         (binary (merge-pathnames (format nil "~a/bin/sherpa-onnx-offline" engine) directory)))
    (ensure-directories-exist (merge-pathnames (format nil "~a/" model) directory))
    (write-temp-file binary
                     (format nil "#!/bin/sh~%printf '%s\\n' \"$@\" > '~a'~%echo 'loading the model' >&2~%printf '%s\\n' '~a'~%"
                             (uiop:native-namestring (merge-pathnames "args.txt" directory))
                             output))
    #-win32 (sb-posix:chmod (uiop:native-namestring binary) #o755)
    (dolist (archive (list engine model))
      (write-temp-file (nck::installed-marker archive nck:*transcriber-directory*)
                       (format nil "stub~%")))
    binary))

(deftest channel-audio-container-sniffs-the-bytes ()
  ;; What a recording is comes from its bytes, never the platform's word.
  (is (equal "ogg" (nck:audio-container (fixture-recording))))
  (is (equal "wav" (nck:audio-container (octets-of #x52 #x49 #x46 #x46 0 0 0 0
                                                   #x57 #x41 #x56 #x45))))
  (is (null (nck:audio-container (octets-of #x52 #x49 #x46 #x46 0 0 0 0
                                            #x57 #x45 #x42 #x50))))
  (is (equal "flac" (nck:audio-container (octets-of #x66 #x4C #x61 #x43 0))))
  (is (equal "mp3" (nck:audio-container (octets-of #x49 #x44 #x33 4 0))))
  (is (equal "mp3" (nck:audio-container (octets-of #xFF #xFB #x90 0))))
  (is (equal "aac" (nck:audio-container (octets-of #xFF #xF1 #x50 0))))
  (is (equal "mp4" (nck:audio-container (octets-of 0 0 0 #x20 #x66 #x74 #x79 #x70))))
  (is (equal "webm" (nck:audio-container (octets-of #x1A #x45 #xDF #xA3))))
  (is (null (nck:audio-container (octets-of 137 80 78 71 13 10 26 10))) "a PNG")
  (is (null (nck:audio-container (octets-of #x4F #x67))) "too short to say"))

(deftest channel-transcription-refuses-with-the-reason ()
  ;; Every refusal says why, in words the room can read: off, too long, too
  ;; large, nothing installed.
  (let ((nck:*transcriber-directory* (temp-path "transcriber"))
        (recording (fixture-recording)))
    (let ((nck:*transcription* (nlk:json-object "enabled" nil)))
      (is (search "transcription is off"
                  (refusal-text error (nck:transcribe-audio recording "ogg")))))
    (let ((nck:*transcription* nil))
      (is (search "over the 5:00 cap"
                  (refusal-text error (nck:transcribe-audio recording "ogg" :seconds 612))))
      (let ((nck::+audio-max-bytes+ 100))
        (is (search "byte ceiling"
                    (refusal-text error (nck:transcribe-audio recording "ogg")))))
      (is-carrying (refusal (refusal-text error (nck:transcribe-audio recording "ogg")))
        "no transcriber is installed" ("(nck:install-transcriber)" "the note names the way out")
        "transcription.base_url"))))

(deftest channel-transcription-runs-the-local-engine (with-temp-workspace (directory))
  ;; The fixture recording through ffmpeg and the installed engine: the
  ;; engine is handed the model's files and a 16 kHz wav, and its JSON line is
  ;; the transcript. Without ffmpeg on the machine the refusal says so.
  (let ((nck:*transcriber-directory* directory)
        (nck:*transcription* nil)
        (recording (fixture-recording)))
    (when (nck::machine-build nck::+transcriber-engines+)
      (install-stub-transcriber
       directory "{\"lang\": \"\", \"text\": \" hello from the stub \", \"timestamps\": [0.2]}")
      (cond ((nck:ffmpeg-present-p)
             (multiple-value-bind (text seconds) (nck:transcribe-audio recording "ogg")
               (is (equal "hello from the stub" text))
               (is (and (realp seconds) (< 0.9 seconds 1.1)) "the decoded length"))
             (let ((args (uiop:read-file-lines (merge-pathnames "args.txt" directory))))
               (is (member "--model-type=nemo_transducer" args :test #'equal))
               (is (some (lambda (arg) (search "encoder.int8.onnx" arg)) args))
               (is (search ".wav" (car (last args))) "the engine reads the decoded wav"))
             ;; A cap the decoded recording passes is refused after decoding.
             (let ((nck:*transcription* (nlk:json-object "max_seconds" 0)))
               (is (search "longer than"
                           (refusal-text error (nck:transcribe-audio recording "ogg")))))
             ;; Bytes that start like a recording and are not one.
             (is (search "do not decode as audio"
                         (refusal-text error
                           (nck:transcribe-audio
                            (octets-of #x4F #x67 #x67 #x53 1 2 3 4 5 6 7 8) "ogg"))))
             ;; An empty transcript is no speech, not a failure.
             (install-stub-transcriber directory "{\"text\": \"\"}")
             (is (equal "" (nck:transcribe-audio recording "ogg"))))
            (t (is (search "ffmpeg is not installed"
                           (refusal-text error (nck:transcribe-audio recording "ogg")))))))))

(deftest channel-transcription-over-an-openai-compatible-api ()
  ;; base_url names the engine: one multipart POST to /audio/transcriptions
  ;; carrying the model, the recording under its container's name, and the
  ;; key as a bearer token.
  (with-temp-file (key :type "key" :contents (format nil "sk-test~%"))
    (let ((nck:*transcription* (nlk:json-object "base_url" "https://stt.example/v1/"
                                                "model" "whisper-large-v3-turbo"
                                                "api_key_file" key))
          (seen nil))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (setf seen (list* :url url options))
                                 (let ((file (cdr (assoc "file" (getf options :content)
                                                         :test #'equal))))
                                   (is (probe-file file) "the recording is on disk while it is sent"))
                                 (values "{\"text\": \" hola, ¿me oyes? \"}" 200))
        (is-values (text seconds) (nck:transcribe-audio (fixture-recording) "ogg" :seconds 5.5)
          (text "hola, ¿me oyes?")
          (seconds = 5.5 "the declared length stands"))
        (is (equal "https://stt.example/v1/audio/transcriptions" (getf seen :url)))
        (is (equal "Bearer sk-test"
                   (cdr (assoc "Authorization" (getf seen :headers) :test #'equal))))
        (let ((content (getf seen :content)))
          (is (equal "whisper-large-v3-turbo" (cdr (assoc "model" content :test #'equal))))
          (is (equal "ogg" (pathname-type (cdr (assoc "file" content :test #'equal)))))))
      ;; A section naming the API but not the model refuses in its own words.
      (let ((nck:*transcription* (nlk:json-object "base_url" "https://stt.example/v1")))
        (is (search "transcription.model is required"
                    (refusal-text error (nck:transcribe-audio (fixture-recording) "ogg"))))))))

(deftest channel-install-transcriber-refuses-a-wrong-digest (with-temp-workspace (directory))
  ;; A download that is not the pinned archive is refused and nothing of it
  ;; is kept — no archive, no marker.
  (let ((nck:*transcriber-directory* directory))
    (when (nck::machine-build nck::+transcriber-engines+)
      (with-stubbed-fdefinition (nck::download-file (url pathname)
                                 (write-temp-file pathname "not the engine"))
        (is-carrying (refusal (refusal-text error (nck:install-transcriber)))
          "not the pinned" "refused"))
      (is (null (directory (merge-pathnames "*.*" directory))))
      (is (null (nck:local-transcriber))))))

;;; --- an answer as a voice message ----------------------------------------------------

(defun pcm-wav (samples &key (rate 8000) data-size
                &aux (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                                        :fill-pointer 0)))
  "SAMPLES, signed 16-bit, as a mono WAV whose data chunk declares DATA-SIZE
octets, its own size by default."
  (flet ((text (string) (loop for char across string do (vector-push-extend (char-code char) out)))
         (le (value count) (dotimes (index count)
                             (vector-push-extend (ldb (byte 8 (* 8 index)) value) out))))
    (text "RIFF") (le (+ 36 (* 2 (length samples))) 4) (text "WAVE")
    (text "fmt ") (le 16 4) (le 1 2) (le 1 2) (le rate 4) (le (* 2 rate) 4) (le 2 2) (le 16 2)
    (text "data") (le (or data-size (* 2 (length samples))) 4)
    (dolist (sample samples) (le (ldb (byte 16 0) sample) 2)))
  (coerce out '(simple-array (unsigned-byte 8) (*))))

(deftest channel-speech-a-recording-is-drawn-as-it-sounds ()
  ;; A second at 8 kHz, its first half silent and its second loud: it plays a
  ;; second, and its 256 bars are flat until the middle and full after it — a
  ;; streamed WAV, declaring its data as long as it could be, drawn the same.
  (let ((samples (append (make-list 4000 :initial-element 0)
                         (loop repeat 2000 append (list 10000 -10000)))))
    (dolist (data-size (list nil #xFFFFFFFF))
      (is-values (seconds waveform) (nck:wav-shape (pcm-wav samples :data-size data-size))
        ((= seconds 1.0) is)
        ((length waveform) 256)
        ((aref waveform 0) 0) ((aref waveform 127) 0)
        ((aref waveform 128) 255) ((aref waveform 255) 255)))))

(deftest channel-speech-a-voice-message-is-ogg-opus (with-temp-workspace (directory))
  ;; What Discord and Telegram play as a voice message: an Ogg container of
  ;; Opus. Through the real ffmpeg when the machine has one.
  (when (nck:ffmpeg-present-p)
    (let* ((target (nck:wav-to-ogg-opus (pcm-wav (loop repeat 4000 append (list 9000 -9000)))
                                        (merge-pathnames "voice-message.ogg" directory)
                                        :channels 1))
           (octets (nck:read-octets target)))
      (is (equal "OggS" (map 'string #'code-char (subseq octets 0 4))))
      (is (search (map 'vector #'char-code "OpusHead") octets)))))
