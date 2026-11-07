;;;; check.lisp --- one machine against the whole feed, and the fix for each hit.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-cyber-security)

(defparameter +rank+ '(:vulnerable 0 :error 1 :skipped 2 :manual 3 :ok 4 :absent 5)
  "How a status sorts in a report: what needs a hand first.")

(defparameter +severity-rank+ '("critical" 0 "high" 1 "medium" 2 "low" 3))

(defun verdict (results)
  "An advisory's status from its checks' RESULTS: the worst one that ran."
  (cond ((null results) :manual)
        ((find :vulnerable results :key (lambda (r) (getf r :status))) :vulnerable)
        ((find :error results :key (lambda (r) (getf r :status))) :error)
        ((find :ok results :key (lambda (r) (getf r :status))) :ok)
        ((find :skipped results :key (lambda (r) (getf r :status))) :skipped)
        (t :absent)))

(defun check (&key dir host (fetch nil))
  "Every advisory against this machine. DIR is a site's folder (a WordPress
root, a Node or PHP project), HOST its public name for the certificate
check; both optional, and a check that needs one is :SKIPPED without it.
=> a list of (:id :title :severity :status :results), worst first."
  (let ((rows (loop for a in (advisories :fetch fetch)
                    for results = (loop for c in (field a "checks")
                                        collect (append (list :label (field c "label"))
                                                        (run-check c :dir dir :host host)))
                    collect (list :id (field a "id") :title (field a "title")
                                  :severity (field a "severity")
                                  :exploited (eq (field a "exploited") t)
                                  :status (verdict results) :results results))))
    (stable-sort rows #'< :key (lambda (r) (+ (* 10 (getf +rank+ (getf r :status)))
                                               (or (second (member (getf r :severity) +severity-rank+
                                                                   :test #'equal))
                                                   9))))))

(defun report (&key dir host (fetch nil) rows)
  "CHECK's answer as text a person reads: what to patch first, one line each."
  (let ((rows (or rows (check :dir dir :host host :fetch fetch))))
    (with-output-to-string (s)
      (format s "~a~%" (feed-status))
      (format s "checked: this machine~@[, site folder ~a~]~@[, host ~a~]~%~%" dir host)
      (dolist (r rows)
        (format s "~a ~a~:[~; (exploited)~] ~a -- ~a~%"
                (case (getf r :status)
                  (:vulnerable "PATCH") (:error "ERROR") (:skipped "SKIP ")
                  (:manual "READ ") (:ok "ok   ") (t "--   "))
                (string-upcase (or (getf r :severity) "")) (getf r :exploited)
                (getf r :id) (getf r :title))
        (dolist (c (getf r :results))
          (when (member (getf c :status) '(:vulnerable :error :skipped :ok))
            (format s "    ~a: ~a~@[ ~a~]~%" (getf c :label) (getf c :why)
                    (and (eq (getf c :status) :vulnerable) (getf c :found)
                         (not (search (getf c :found) (getf c :why)))
                         (getf c :found))))))
      (let ((hits (count :vulnerable rows :key (lambda (r) (getf r :status)))))
        (format s "~%~d to patch~:[~;: (security:show \"ID\") gives each fix~]; READ lines have no machine check -- act if you use that service.~%"
                hits (plusp hits))))))

(defun show (id)
  "One advisory in full, as text: what, who, the fix, the stopgap, the signs, the sources."
  (let ((a (advisory id)))
    (unless a
      (return-from show (format nil "no advisory named ~a; (security:advisories) lists them" id)))
    (with-output-to-string (s)
      (format s "~a~%~a, ~a~:[~;, exploited~]~@[, ~{~a~^ ~}~]~%~%~a~%~%Affected: ~a~%"
              (field a "title") (field a "date") (string-upcase (field a "severity" ""))
              (eq (field a "exploited") t) (field a "cves") (field a "summary") (field a "affected"))
      (when (field a "fixed") (format s "Fixed in: ~{~a~^, ~}~%" (field a "fixed")))
      (loop for (key head) in '(("patch" "Patch") ("mitigation" "Until you can patch")
                                ("iocs" "Signs of compromise") ("sources" "Sources"))
            for items = (field a key)
            when items do (format s "~%~a:~%~{- ~a~%~}" head items)))))
