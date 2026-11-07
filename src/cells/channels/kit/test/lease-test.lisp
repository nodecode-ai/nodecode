;;;; lease-test.lisp --- one organism per bot token, across profiles.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lease is a lock file named by the token's hash under a folder
;;;; every profile on the machine shares; a lock is a per-process fact, so
;;;; the held case is a python child holding it, the way the kernel's own
;;;; lock tests do.

(in-package #:nodecode.test)

(deftest channel-token-lease-is-taken-named-released-and-refused-when-held ()
  (with-temp-directory (leases "leases")
    (with-saved-globals (nck::*lease-directory*)
      (setf nck::*lease-directory* (uiop:ensure-directory-pathname leases))
      (is (null (nck::take-channel-lease "telegram" (cell-json "{}"))))
      (with-temp-file (token-file :type "txt" :contents "123456:fixture-token")
        (let* ((section (cell-json (format nil "{\"token_file\": ~s}" token-file)))
               (lease (nck::take-channel-lease "telegram" section)))
          (is (consp lease) "a section with a token takes a lease")
          (is (probe-file (cdr lease)) "the lock file sits under the lease folder")
          (is (search "profile " (uiop:read-file-string (cdr lease))))
          (is (search (format nil "pid ~d" (sb-posix:getpid)) (uiop:read-file-string (cdr lease))))
          (nck::release-channel-lease lease)
          (is (not (nlk:file-locked-p (cdr lease))) "released, nobody holds it")
          ;; Another organism — another process — holds the same token.
          (with-lock-holder ((cdr lease) (format nil "profile other, pid 4242~%"))
            (is-carrying (refusal (refusal-text nlk:config-refusal
                                    (nck::take-channel-lease "telegram" section)))
              "held by profile other, pid 4242" "one organism per token"))
          (let ((again (nck::take-channel-lease "discord" (cell-json (format nil "{\"bot_token_file\": ~s}" token-file)))))
            (is (consp again) "freed with its holder, the discord key resolves the same token")
            (is (equal (namestring (cdr lease)) (namestring (cdr again))))
            (nck::release-channel-lease again)))))))
