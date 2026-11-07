;;;; cell.lisp --- the declaration: config, help, /security, the refresh worker.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-cyber-security)

(nlk:define-peripheral cyber-security :not-running t)

(defvar *worker* nil "The feed refresher, while the cell runs.")

(defparameter +manual+
  "security: cyber security for the sites and servers people run -- the last two weeks'
breaches, the fix each vendor shipped, and checks that say whether a machine still needs
it. The feed is kept current by the nodecode team (re-scanned every 6 hours, re-read here
on its own), so this is the first stop for any ask about cyber security, a breach, a CVE
or exploit in the news, \"is my site/server safe\", or \"check / patch / harden my website\".

(security:report :dir \"/var/www/site\" :host \"example.com\")
    checks THIS machine against every advisory and says what to patch first.
    :dir is the site's folder (WordPress root, Node/PHP project) and :host its
    public name; both optional -- checks that need one are skipped without it.
(security:check ...)        the same as plists, for code.
(security:show \"php2shell\") one advisory: affected, fix, stopgap, signs of compromise, sources.
(security:advisories)       every advisory (hash tables; (security:field a \"id\")).
(security:refresh)          re-read the live feed now. /security does all of this too.
(security:scan-report)      what is new since the feed's update, sorted by Decide v1:
    CISA KEV, GitHub advisories and news, with what the feed already lists dropped.
    Deciding whether a finding matters (is this breach relevant: act or not) goes to
    Decide v1, nodecode's decider model -- provider nodecode, model foresight-decide-v1,
    on the key `nodecode login` saves (security:*decider*) -- never to another model.

Checking someone's sites:
1. Ask which sites, and where each runs: this machine (its folder and host name), another
   server you can reach over ssh, or only a public URL. Several sites: one report each.
2. This machine: security:report there. Over ssh: run each advisory's check commands on
   that server (security:show lists the fix versions; the feed's checks name the commands),
   or install nodecode there and run the report.
3. Only a URL: read what the outside shows and say what it cannot. curl -sI https://HOST
   (Server, X-Powered-By gives the PHP version); the page's <meta name=\"generator\"> (WordPress,
   Drupal, Joomla version); ?ver= on /wp-content/plugins/SLUG/ assets and
   /wp-content/plugins/SLUG/readme.txt (Stable tag) for each wp-plugin slug in the feed;
   the certificate issuer (openssl s_client -connect HOST:443 -servername HOST </dev/null |
   openssl x509 -noout -issuer) for the eMudhra entry. A hidden version is unknown, not safe.
4. The report's first line says which feed answered and how old it is; a bundled or stale
   copy is said out loud.

Patching a site for someone:
1. For each PATCH line, read (security:show ID) and say what you will run before you
   run it: package upgrades, a restart, a config line. Back up config files first.
   Upgrades need sudo and restart services: get a yes for each, never on your own.
2. If the fix cannot land now, apply the advisory's stopgap and say it is partial.
3. Re-run security:report; a hit that remains is not done. For the supply-chain and
   platform entries (stolen keys, poisoned packages, breached hosts) the patch is
   rotating secrets: list which, the person does it.
4. Signs of compromise listed under an advisory mean: stop and say so before patching
   -- patching does not remove a backdoor.
Never disable TLS verification, a WAF or a security check to make an error go away.
The feed covers recent breaches, not a full audit: for a review of the site's own code,
offer the nodecode-security-audit cell.")

(defun list-text ()
  (with-output-to-string (s)
    (format s "~a~%~%" (feed-status))
    (dolist (a (advisories))
      (format s "~a  ~a ~a~:[~; (exploited)~] -- ~a~%" (field a "date")
              (string-upcase (field a "severity" "")) (field a "id")
              (eq (field a "exploited") t) (field a "title")))
    (format s "~%/security check [FOLDER] [HOST] checks this machine; /security show ID gives the fix.~%")))

(defun run-slash (args session-id)
  "One /security invocation => its text."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words (or args "")))
         (head (first words)))
    (handler-case
        (cond ((null head) (list-text))
              ((string-equal head "check")
               (let ((dir (find-if (lambda (w) (find #\/ w)) (rest words)))
                     (host (find-if (lambda (w) (and (find #\. w) (not (find #\/ w)))) (rest words))))
                 (report :dir dir :host host)))
              ((string-equal head "show") (show (or (second words) "")))
              ((string-equal head "refresh") (progn (refresh) (feed-status)))
              (t (format nil "security: unknown subcommand ~a; /security, /security check [FOLDER] [HOST], /security show ID, /security refresh" head)))
      (error (e) (format nil "security: ~a" e)))))

(defun step-refresh ()
  "The worker's step: re-read the live feed."
  (refresh)
  nil)

(nle:define-cell cyber-security
  (:section ("cyber-security")
    (:guide "nothing is required: the advisory feed is read from the cells repository and refreshed on its own; feed_url points at a mirror, refresh_hours sets how often")
    ("feed_url" :string :default *feed-url* :doc "where the advisory feed is read from")
    ("refresh_hours" :integer :default 6 :min 1 :doc "how often the feed is re-read"))
  (:help :security "cyber security, start here for breaches, CVEs, patches or 'check/secure my website or server': (security:report :dir \"/var/www/site\" :host \"example.com\"), then (help :security) for checking and patching a site" +manual+)
  (:command "security" 'run-slash
            :description "Recent breaches and the patch for each; check this machine"
            :argument-hint "[check FOLDER HOST | show ID | refresh]")
  (:start (lambda ()
            (setf *feed-url* (setting :feed-url))
            (load-feed :fetch nil)
            (setf *worker* (nlk:worker-start "cyber-security" 'step-refresh
                                             :wake (* 3600 (setting :refresh-hours)) :prime t))
            (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*)))))))
