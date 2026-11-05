;;;; lib/http-backend.lisp -- one HTTP client on every platform
(in-package #:cl-user)

;;; dexador builds only its WinHTTP backend on Windows, and that backend
;;; reads a whole body before it answers ("This obviously isn't streaming",
;;; its own note) and opens a session for every request. A Windows organism
;;; therefore saw a model's reply only once all of it had arrived, and paid
;;; a fresh TCP and TLS handshake on every round of a turn, where Linux
;;; streams each delta and reuses one pooled connection. Here Windows loads
;;; dexador's socket backend as well, the file its system leaves out there,
;;; and makes it the backend: one client on every platform, streamed and
;;; pooled. Its TLS is cl+ssl's, and on Windows the system's own chain engine
;;; verifies the peer (lib/dial.lisp).
#+win32
(unless (find-package "DEXADOR.BACKEND.USOCKET")
  (load (asdf:system-relative-pathname "dexador" "src/backend/usocket.lisp")))

#+win32
(setf dex:*dexador-backend* :usocket)
