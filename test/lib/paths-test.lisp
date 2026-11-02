;;;; paths-test.lisp --- the home: one folder, one profile.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The resolution ladder against a live process environment is covered in
;;;; organism/cells-test (it swaps $HOME and re-resolves every startup
;;;; parameter); these gates pin the pure pieces: what a profile may be
;;;; called, where one lives, and that HOME reads *HOME* at call time —
;;;; the contract every site that moved off a literal `.nodecode/' relies on.

(in-package #:nodecode.test)

(deftest paths-profile-names ()
  (dolist (name '("default" "coder" "a" "x1" "my_profile" "team-2" "0zero"))
    (is (nlk:profile-name-p name) (format nil "~a is a profile name" name)))
  (dolist (name '("" "Coder" "-lead" "_x" "a b" "a/b" "a.b" "über" nil 12))
    (is (not (nlk:profile-name-p name)) (format nil "~s is not a profile name" name)))
  (is (nlk:profile-name-p (make-string 64 :initial-element #\a)) "64 characters fit")
  (is (not (nlk:profile-name-p (make-string 65 :initial-element #\a))) "65 do not")
  (dolist (word '("default" "profiles" "serve" "trial" "setup" "import" "profile" "resume"))
    (is (member word nlk:+reserved-profile-names+ :test #'string=)
        (format nil "~a is reserved: the default home, its folder, or a launch verb" word))))

(deftest paths-profile-homes ()
  (is (equal (namestring (nlk:default-home)) (namestring (nlk:profile-home "default"))))
  (is (equal (namestring (merge-pathnames "coder/" nlk:*profiles-directory*))
             (namestring (nlk:profile-home "coder"))))
  ;; The posture points both under a scratch home; the initforms — what a
  ;; launch resolves — sit inside the default home.
  (flet ((initform (name)
           (namestring (funcall (cdr (assoc name nlk::*startup-parameters*))))))
    (is (uiop:string-suffix-p (initform 'nlk:*profiles-directory*) "/.nodecode/profiles/"))
    (is (uiop:string-suffix-p (initform 'nlk:*active-profile-path*) "/.nodecode/active-profile"))))

(deftest paths-home-reads-the-live-value (let ((scratch (temp-path "home2"))))
  ;; The hermetic posture already points *HOME* at a scratch folder; HOME
  ;; must answer that folder now, and another the moment it changes.
  (is (equal (namestring nlk:*home*) (namestring (nlk:home))) "(home) is the home")
  (is (equal (namestring (merge-pathnames "cells/" nlk:*home*))
             (namestring (nlk:home "cells/"))))
  (with-saved-globals ((nlk:*home* scratch))
    (is (equal (namestring (merge-pathnames "config.jsonc" scratch))
               (namestring (nlk:home "config.jsonc"))))))

(deftest paths-profile-name-of-a-home ()
  (is (equal "default" (nlk:profile-name (nlk:default-home))) "~/.nodecode/ is default")
  (is (equal "coder" (nlk:profile-name (nlk:profile-home "coder"))) "a named folder is its name")
  (is (equal "elsewhere" (nlk:profile-name #p"/tmp/elsewhere/")))
  (is (equal "elsewhere" (nlk:profile-name "/tmp/elsewhere"))))

(deftest paths-profile-label-says-a-name-for-every-home ()
  ;; The display name is total where the layout question is not: the
  ;; default home is named `default', not left silent.
  (is (equal "default" (nlk:profile-label (nlk:default-home))) "the default home has a name")
  (is (equal "coder" (nlk:profile-label (nlk:profile-home "coder"))) "a named profile is its folder")
  (is (equal "elsewhere" (nlk:profile-label #p"/tmp/elsewhere/")) "a custom home is its folder")
  (is (not (nlk:profile-named-p (nlk:default-home))) "the layout question still answers no")
  (is (nlk:profile-named-p (nlk:profile-home "coder")) "and yes for a folder under profiles/")
  (is (not (nlk:profile-named-p #p"/tmp/elsewhere/")) "nor for a custom $NODECODE_HOME")
  ;; The two questions are not the same question, and the docstring that
  ;; said a shell is silent about an unnamed home described a convention
  ;; the chrome no longer follows.
  (let ((manual (format nil "~a~%~a" (documentation 'nlk:profile-named-p 'function)
                        (nlk:definition-prose 'nlk:profile-named-p))))
    (is-carrying manual (:absent "name to say") "PROFILE-LABEL"))
  (dolist (home (list (nlk:default-home) (nlk:profile-home "coder") #p"/tmp/elsewhere/" #p""))
    (let ((label (nlk:profile-label home)))
      (is (and (stringp label) (plusp (length label)))
          (format nil "~a labels as a non-empty string" home)))))

#-win32
(deftest resolve-runtime-follows-the-link-to-the-executable ()
  ;; SBCL on macOS reports the executable by the path the process was
  ;; started through, links unresolved; the installer's ~/.local/bin/nodecode
  ;; is one, and the src/ the organism reads itself from sits beside the
  ;; file the link points at, not beside the link.
  (let* ((root (nlk:home "install/"))
         (binary (merge-pathnames "v1/nodecode" root))
         (link (merge-pathnames "bin/nodecode" root)))
    (write-temp-file binary "#!/bin/sh")
    (ensure-directories-exist link)
    (nlk:make-symlink binary link)
    (is-table (file) (equal (namestring (truename binary))
                            (namestring (nlk::resolve-runtime (uiop:native-namestring file))))
      (link) (binary))
    (let ((missing (uiop:native-namestring (merge-pathnames "gone/nodecode" root))))
      (is (equal missing (namestring (nlk::resolve-runtime missing)))))))
