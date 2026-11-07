;;;; openssl-test.lisp --- the OpenSSL the image binds, and the one name 4.0 dropped.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The image loads whichever OpenSSL the box has (os.lisp, BIND-ASN1-STRING-GET0),
;;;; and cl+ssl reads every certificate name through ASN1_STRING_data, which
;;;; OpenSSL 4.0.0 removed. These gates pin the binding and the read that goes
;;;; through it, so a host on either major proves the same thing.

(in-package #:nodecode.test)

(defparameter *certificate-pem*
  "-----BEGIN CERTIFICATE-----
MIIDUTCCAjmgAwIBAgIUKUGok2I+xL60MDvtE3ftAe7oWgwwDQYJKoZIhvcNAQEL
BQAwKzEWMBQGA1UEAwwNbm9kZWNvZGUudGVzdDERMA8GA1UECgwIbm9kZWNvZGUw
HhcNMjYxMTA3MDUwNjU5WhcNMzYxMTA0MDUwNjU5WjArMRYwFAYDVQQDDA1ub2Rl
Y29kZS50ZXN0MREwDwYDVQQKDAhub2RlY29kZTCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAKzzP6oTLUA+oL1Rl6y3d2zU1Kt60b2kMe/SsGJlIuoESZP9
OHeKdmi1zix5/NQ1xljPPSSZSMgg+JvBCEybiOJ7Hu8Kzg3RhNgnGxNUkqffEXH7
1jxDli7sw4CNtgIVhTpuSDxHm7GHItgTTQqlbzadzjWQJaJ/oxuoKxa3weFDVy5y
l3OsF9cR1i3s3qXK94CFH9RZxDVVpYynOHroLIJXuIABE+xdpmIA0YWsJMnFYtrH
7VSst+bNYF0sjkwrjOo0y6KZ7BsSlnm7L7rKxHI2xq2p/2hJvkxsQWw8l8VsKnki
9qOVT0eBp6bJ8ZYbq/JEOH2ABs/OfiZxQqumG6MCAwEAAaNtMGswHQYDVR0OBBYE
FMv2n/WXfGoZSD063nWlRT57hTThMB8GA1UdIwQYMBaAFMv2n/WXfGoZSD063nWl
RT57hTThMA8GA1UdEwEB/wQFMAMBAf8wGAYDVR0RBBEwD4INbm9kZWNvZGUudGVz
dDANBgkqhkiG9w0BAQsFAAOCAQEAFyPy99AaA3FD88hmySpE53WZlKZg4e0x+7Yu
Kc3O2SitTaT39jmGXpLoMckx7S2K4jidUaodR50Pdu9qhHDXlLjiKBrWSLREPH2g
pHpXu2ZijFq3cSVY9QzmbvZQuke3mEXEULP+AcCfzjBYdx1cocmK17nx03mhI1cd
HkC74108vq3XdyyL/sYGMZo/w7Pcyzb5IfZfU20/F6Kj2Bc6jYtc4cTq6so7Noh3
9um3sJN37tMYd2yQVRlzHrJjRDb6JgPO/Mj/63V2ljBGWJqLHFyRArpSsDObFUS4
uIDdOSrJ5oWg4h5XgnObWdMvgiaQGWDYcrklKua9UWYlR+Xu8Q==
-----END CERTIFICATE-----"
  "A self-signed certificate for nodecode.test, good until 2036.")

(deftest asn1-string-data-binds-the-name-openssl-4-kept ()
  ;; ASN1_STRING_data is gone from OpenSSL 4.0.0 and ASN1_STRING_get0_data,
  ;; which returns the same pointer, is in every 1.1.0 and later: the binding
  ;; is what keeps a certificate readable on either major.
  (is (eq :get0 (nlk:bind-asn1-string-get0))))

(deftest a-certificate-name-reads-through-the-binding ()
  ;; The path a verified handshake takes (cl+ssl: CERTIFICATE-SUBJECT-COMMON-NAMES,
  ;; TRY-GET-ASN1-STRING-DATA, ASN1-STRING-BYTES-VECTOR), through the binding
  ;; above and nothing else.
  (nlk:bind-asn1-string-get0)
  (cl+ssl:ensure-initialized)
  (let ((certificate (cl+ssl::x509-cert-from-pem *certificate-pem*)))
    (unwind-protect
         (is (equal '("nodecode.test")
                    (cl+ssl:certificate-subject-common-names certificate)))
      (cl+ssl:x509-free certificate))))
