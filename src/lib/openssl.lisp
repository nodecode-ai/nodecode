;;;; openssl.lisp --- the OpenSSL a Mac build opens: Homebrew's openssl@3, by its own path.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Loaded before cl+ssl (nodecode/openssl, the first of nodecode's
;;;; dependencies), because cl+ssl chooses its libraries as it loads. Left to
;;;; itself on Apple Silicon it opens /opt/homebrew/opt/openssl/lib, the path
;;;; of Homebrew's unversioned `openssl', and that name is OpenSSL 4 now.
;;;; Builds through 0.1.12 opened it there, and since OpenSSL 4 removed
;;;; ASN1_STRING_data, which cl+ssl's certificate check calls, they started
;;;; and then failed their first TLS connection, sign-in included: `The alien
;;;; function "ASN1_STRING_data" is undefined'. bind-asn1-string-get0 (src/lib/os.lisp)
;;;; now reads certificates through ASN1_STRING_get0_data, the name OpenSSL 4
;;;; kept, so a 4.x library no longer breaks a build; this pin is what keeps a
;;;; Homebrew alias move from swapping the library under a shipped build at
;;;; all, so every Mac runs the OpenSSL the release was built and tested with.
;;;;
;;;; SBCL reopens a loaded library at every start of the dumped image, by the
;;;; path it was loaded from on the box that built it. So the path named here
;;;; is the one the release opens on the operator's Mac: openssl@3's own keg
;;;; link, which no other OpenSSL formula moves, and in it the versioned file,
;;;; the install name libssl links its libcrypto by. tools/install/install.sh
;;;; asks for those two files, and tools/release/mac-build.sh refuses to build
;;;; without them.
;;;;
;;;; A cl+ssl already loaded from anywhere else when this file loads is an
;;;; error: that image would carry the other path into its dump.
;;;;
;;;; Everywhere else this file is empty: Linux opens libssl.so.3 by soname,
;;;; Windows ships OpenSSL 3's DLLs beside the executable, and an Intel Mac has
;;;; no build.

(in-package #:cl-user)

#+(and darwin arm64)
(macrolet ((pin (define library path)
             `(let ((loaded (find ',library (cffi:list-foreign-libraries :loaded-only t)
                                  :key #'cffi:foreign-library-name)))
                (when (and loaded
                           (string/= (namestring (cffi:foreign-library-pathname loaded)) ,path))
                  (error "cl+ssl opened ~a before nodecode/openssl could name ~a; ~
                          load nodecode into an image that has not loaded cl+ssl"
                         (cffi:foreign-library-pathname loaded) ,path))
                (,define ,path))))
  (pin cl+ssl/config:define-libcrypto-path cl+ssl/config::libcrypto
       "/opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib")
  (pin cl+ssl/config:define-libssl-path cl+ssl/config::libssl
       "/opt/homebrew/opt/openssl@3/lib/libssl.3.dylib"))
