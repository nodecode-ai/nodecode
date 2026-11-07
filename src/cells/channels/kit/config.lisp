;;;; config.lisp --- channel secret resolution and the allowlist floor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The typed section accessors (CONFIG-STRING and family) live in the core
;;;; waist now (src/waist/config.lisp) — this file once carried the copy the
;;;; guard cell duplicated, and the waist is the one owner. What remains
;;;; here is channel vocabulary only. Key names are the verbatim Zig channel
;;;; pack keys, so one config configures either implementation.
;;;;
;;;; Secrets are never inline: exactly one of <base>_env / <base>_file names
;;;; where the token lives.

(in-package #:nodecode-channel-kit)

(defun resolve-channel-secret (section base-key &aux (env-key (format nil "~a_env" base-key))
                                                     (file-key (format nil "~a_file" base-key))
                                                     (env-name (config-string section env-key))
                                                     (file-name (config-string section file-key)))
  "The secret named by exactly one of <BASE-KEY>_env / <BASE-KEY>_file."
  ;; Both present is ambiguous and refused; neither present is unconfigured
  ;; and refused; an empty resolution (unset env var, blank file) is refused.
  ;; The returned string is trimmed of surrounding whitespace and newlines.
  (when (and env-name file-name)
    (config-error "exactly one of ~a / ~a may be set, both are"
                  env-key file-key))
  (unless (or env-name file-name)
    (config-error "one of ~a / ~a is required (secrets are never inline)"
                  env-key file-key))
  (let* ((raw (if env-name
                  (uiop:getenv env-name)
                  (handler-case (uiop:read-file-string file-name)
                    (error (condition)
                      (config-error "~a: cannot read ~a: ~a"
                                    file-key file-name condition)))))
         (secret (and (stringp raw)
                      (nlk:trimmed raw))))
    (unless (and secret (plusp (length secret)))
      (config-error "~a names ~a, which is ~:[~;unset or ~]empty"
                    (if env-name env-key file-key) (or env-name file-name) env-name))
    secret))

(defun require-non-empty-allowlist (channel-id &rest named-lists)
  "Fail-closed admission floor: a channel message drives full host authority
(no approval chain exists in this organism by design), so a lane with no
allowlist at all must refuse to start rather than listen to the world."
  ;; NAMED-LISTS is a plist of key-name -> list; at least one must be
  ;; non-empty.
  (unless (loop for (nil list) on named-lists by #'cddr
                  thereis (consp list))
    (config-error
     "channels.~a refuses to start open to the world: populate at least ~
      one of ~{~a~^, ~} (channel messages run with full host authority)"
     channel-id
     (loop for (name nil) on named-lists by #'cddr collect name)))
  t)
