;;;; proxy-test.lisp --- the proxy a dial goes through: read from the
;;;; environment at the call, never the build machine's, with loopback and
;;;; NO_PROXY exempt.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each row sets the proxy variables it names -- every other one is unset
;;;; for the row -- and asks NLK:PROXY-FOR about one URL. The variables are
;;;; put back as they were after every row.

(in-package #:nodecode.test)

(defparameter *proxy-variables*
  '("HTTPS_PROXY" "https_proxy" "HTTP_PROXY" "http_proxy"
    "ALL_PROXY" "all_proxy" "NO_PROXY" "no_proxy")
  "Every environment variable NLK:PROXY-FOR reads.")

(defun call-with-proxy-environment (pairs thunk)
  "THUNK with only PAIRS -- NAME VALUE ... -- among *PROXY-VARIABLES* set,
the environment restored afterwards."
  (let ((saved (mapcar (lambda (name) (cons name (uiop:getenv name))) *proxy-variables*)))
    (unwind-protect
         (progn (dolist (name *proxy-variables*) (sb-posix:unsetenv name))
                (loop for (name value) on pairs by #'cddr
                      do (sb-posix:setenv name value 1))
                (funcall thunk))
      (loop for (name . value) in saved
            do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name))))))

(defparameter *proxy-for-rows*
  (let ((p "http://user:pw@proxy.test:3128")
        (q "http://other.test:8080")
        (srt "localhost,127.0.0.1,::1,169.254.0.0/16,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"))
    `(;; loopback is never proxied
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,p) "http://localhost:8000/v1" nil nil "localhost is not proxied")
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,p) "http://127.0.0.1:18777/v1" nil nil "127.0.0.1 is not proxied")
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,p) "http://127.9.9.9/" nil nil "all of 127/8 is loopback")
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,p) "http://[::1]:9/" nil nil "::1 is not proxied")
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,p) "https://api.localhost/" nil nil "a name under localhost is loopback")
      ;; nothing set, or blank
      (() "https://api.openai.com/v1" nil nil "no variable set means no proxy")
      (("HTTPS_PROXY" "   ") "https://api.openai.com/v1" nil nil "a blank variable means no proxy")
      ;; which variable a scheme reads
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,q) "https://api.openai.com/v1" nil ,p "https reads HTTPS_PROXY")
      (("HTTPS_PROXY" ,p "HTTP_PROXY" ,q) "http://plain.test/" nil ,q "http reads HTTP_PROXY")
      (("https_proxy" ,p) "https://api.openai.com/v1" nil ,p "the lower-case spelling is read too")
      (("HTTPS_PROXY" ,p) "wss://gateway.discord.gg/" nil ,p "wss reads HTTPS_PROXY")
      (("ALL_PROXY" ,p) "https://api.openai.com/v1" nil ,p "ALL_PROXY answers https when HTTPS_PROXY is unset")
      (("all_proxy" ,p) "http://plain.test/" nil ,p "all_proxy answers http when HTTP_PROXY is unset")
      (("HTTPS_PROXY" ,p "ALL_PROXY" ,q) "https://api.openai.com/v1" nil ,p "HTTPS_PROXY outranks ALL_PROXY")
      ;; an explicit proxy wins over everything
      (("HTTPS_PROXY" ,p) "https://api.openai.com/v1" "http://explicit.test:9" "http://explicit.test:9" "an explicit proxy wins over the environment")
      (() "http://127.0.0.1:1/" "http://explicit.test:9" "http://explicit.test:9" "an explicit proxy wins even for loopback")
      ;; NO_PROXY: srt's own list, CIDRs included
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://10.1.2.3/" nil nil "10/8 is exempt")
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://192.168.1.1/" nil nil "192.168/16 is exempt")
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://172.31.255.254/" nil nil "the top of 172.16/12 is exempt")
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://172.32.0.1/" nil ,p "just past 172.16/12 is proxied")
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://100.64.1.2/" nil ,p "a tailnet address is proxied")
      (("HTTPS_PROXY" ,p "NO_PROXY" ,srt) "https://openrouter.ai/api/v1" nil ,p "a public name is proxied")
      ;; NO_PROXY names: themselves and every name under them
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com, .internal.test") "https://example.com/" nil nil "a name covers itself")
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com, .internal.test") "https://api.example.com/" nil nil "a name covers the names under it")
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com, .internal.test") "https://notexample.com/" nil ,p "a name does not cover a longer name")
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com, .internal.test") "https://internal.test/" nil nil "a leading dot still covers the name itself")
      (("HTTPS_PROXY" ,p "NO_PROXY" "*.corp.test") "https://a.corp.test/" nil nil "a *. prefix covers the names under it")
      (("HTTPS_PROXY" ,p "no_proxy" "example.com") "https://example.com/" nil nil "the lower-case no_proxy is read")
      (("HTTPS_PROXY" ,p "NO_PROXY" "*") "https://anything.test/" nil nil "a lone star exempts every host")
      ;; a port on an entry limits it
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com:8443") "https://example.com:8443/" nil nil "an entry with a port covers that port")
      (("HTTPS_PROXY" ,p "NO_PROXY" "example.com:8443") "https://example.com/" nil ,p "an entry with a port does not cover another")
      (("HTTPS_PROXY" ,p "NO_PROXY" "10.0.0.0/8") "https://10.200.3.4:8443/x" nil nil "a CIDR covers every port")
      ;; IPv6 literals
      (("HTTP_PROXY" ,p "NO_PROXY" "fd00::1") "http://[fd00::1]:80/" nil nil "a bare IPv6 entry covers that address")
      (("HTTP_PROXY" ,p "NO_PROXY" "[fd00::2]:80") "http://[fd00::2]/" nil nil "a bracketed IPv6 entry with its port covers it")
      (("HTTP_PROXY" ,p "NO_PROXY" "fd00::1") "http://[fd00::9]/" nil ,p "an IPv6 entry covers no other address")))
  "(ENVIRONMENT URL EXPLICIT EXPECTED WHY): what NLK:PROXY-FOR answers for URL
and EXPLICIT with only ENVIRONMENT's proxy variables set.")

(deftest proxy-for-reads-the-environment-at-every-call ()
  (dolist (row *proxy-for-rows*)
    (destructuring-bind (environment url explicit expected why) row
      (is (equal expected
                 (call-with-proxy-environment environment
                                              (lambda () (nlk:proxy-for url explicit))))
          why))))

(deftest proxy-for-sees-a-change-to-the-environment-without-a-restart ()
  (call-with-proxy-environment
   '("HTTPS_PROXY" "http://first.test:1")
   (lambda ()
     (let ((first (nlk:proxy-for "https://api.openai.com/v1")))
       (sb-posix:setenv "HTTPS_PROXY" "http://second.test:2" 1)
       (is (equal '("http://first.test:1" "http://second.test:2")
                  (list first (nlk:proxy-for "https://api.openai.com/v1"))) "the second call reads it")))))

(deftest http-dials-through-no-build-time-proxy ()
  (is (member 'nodecode.kernel::forget-build-proxy sb-ext:*init-hooks*) "an init hook forgets it")
  (let ((dex:*default-proxy* "http://build.test:1"))
    (nodecode.kernel::forget-build-proxy)
    (is (null dex:*default-proxy*) "dexador is left with no proxy of its own")))
