;;;; scan.lisp --- the six-hourly scan's first pass: collect, then let the decider sort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A full agent reading everything a day publishes costs the same whether
;;;; anything new turned up or not. SCAN gathers the window's items with no
;;;; model at all -- CISA's exploited list, GitHub's reviewed and malware
;;;; advisories, and a news search when this image has one -- drops what the
;;;; feed already holds or an earlier scan already sorted, and asks Decide v1,
;;;; nodecode's decider model (`foresight-decide-v1' on the `nodecode'
;;;; provider, https://api.nodecode.ai/v1), what kind of item each one is.
;;;; Only what it keeps goes on to the agent, which still takes every fact
;;;; from the vendor's own page.
;;;;
;;;; Two kinds skip the decider, because the decider cannot see what decides
;;;; them: an item on CISA's exploited list is always investigated, and a
;;;; malware advisory is kept exactly when its package has real downloads (a
;;;; registry count, no model). When the decider cannot answer, SCAN says so
;;;; and every item goes to the agent, so nothing is skipped quietly.

(in-package #:nodecode-cyber-security)

(defparameter *decider* '(:provider "nodecode" :model "foresight-decide-v1")
  "The model that sorts scan items: nodecode's Decide v1.")

(defparameter *keep-at* 0.5
  "Keep an item when the decider's P(act) is at least this.")

(defparameter *popular-at* 10000
  "A malware advisory is kept when its package had at least this many downloads
last month (RubyGems: in all). Registry mirrors alone give a fresh typosquat
a few hundred to a few thousand, so the bar sits well above that.")

(defparameter *kev-url*
  "https://www.cisa.gov/sites/default/files/feeds/known_exploited_vulnerabilities.json"
  "CISA's Known Exploited Vulnerabilities catalog.")

(defparameter *github-url* "https://api.github.com/advisories"
  "GitHub's global advisory database.")

(defparameter *github-pages* 5 "At most this many pages of 100 per GitHub query.")

(defparameter *news-queries*
  '("actively exploited vulnerability patch"
    "zero-day security update web server CMS plugin"
    "data breach API keys environment variables exposed"
    "supply chain attack malicious npm PyPI package"
    "hosting SaaS platform security incident")
  "The news searches, one per kind of item the feed carries.")

(defparameter *seen-days* 30 "How long a sorted item stays in the seen ledger.")

(defparameter +kind-question+
  "What kind of security item is this, for people who run websites, servers, self-hosted apps, developer machines and hosted services?"
  "The decider's one question about each item.")

(defparameter +kinds+
  '(("act" . "They must patch, upgrade or rotate secrets now: remote code execution, command injection, authentication or authorization bypass, secret or data exposure in server-side software, CMSs and their plugins, web servers, databases, CI/CD, Linux hosts and virtualization, TLS or certificate authorities; a poisoned release of a widely used package; or a hosting or SaaS platform breach that leaks customers' keys.")
    ("minor" . "A crash, denial of service, infinite loop, resource exhaustion or memory bug from a crafted file or request, or a bug in a developer's local tool or shell script, that does not let an outsider take over a server or read its data.")
    ("elsewhere" . "Not theirs to act on: phones, desktop or end-user apps, industrial or embedded devices, opinion or roundup pieces, news with no vulnerability in it, or news about a vulnerability or breach more than two weeks old."))
  "The options of +KIND-QUESTION+; only \"act\" is kept.")

;;; ---------------------------------------------------------------- small helpers

(defun parse-iso (iso)
  "ISO (YYYY-MM-DDTHH:MM:SSZ, or a bare date) as a universal time, UTC."
  (flet ((n (a b) (if (> (length iso) a) (parse-integer iso :start a :end (min b (length iso))) 0)))
    (encode-universal-time (n 17 19) (n 14 16) (n 11 13) (n 8 10) (n 5 7) (n 0 4) 0)))

(defun format-iso (time)
  "TIME, a universal time, as YYYY-MM-DDTHH:MM:SSZ."
  (multiple-value-bind (s m h d mo y) (decode-universal-time time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h m s)))

(defun day (iso) "ISO's date part." (subseq iso 0 (min 10 (length iso))))

(defun clip (text n)
  "TEXT cut to N characters."
  (if (> (length text) n) (concatenate 'string (subseq text 0 n) "...") text))

(defun str (x) "X when it is a string, else NIL." (and (stringp x) x))

(defun json-object (alist)
  "ALIST of (KEY . VALUE) as a JSON object table, nested alists included."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k . v) in alist
          do (setf (gethash k h) (if (and (consp v) (consp (car v))) (json-object v) v)))
    h))

(defun get-json (url)
  "GET URL => (VALUES JSON HEADERS), or signal; through *FETCH*, so tests stub every source."
  (multiple-value-bind (body status headers) (funcall *fetch* url)
    (unless (eql status 200)
      (error "~a answered ~a" url status))
    (values (shasht:read-json body) headers)))

;;; ---------------------------------------------------------------- the decider

(defparameter +same-question+
  "Is this item about one of the advisories already listed below, or about something new?"
  "The decider's second question: what the feed already carries, by meaning, not by link.")

(defun decision-request (text listed)
  "The Decide v1 request about TEXT, as JSON: +KIND-QUESTION+, and when
LISTED ((ID . DESCRIPTION) ...) is not empty, +SAME-QUESTION+ over it."
  (let ((questions (json-object `(("kind" . ,(json-object `(("type" . "choice")
                                                             ("instructions" . ,+kind-question+)
                                                             ("criteria" . ,+kinds+))))))))
    (when listed
      (setf (gethash "same" questions)
            (json-object `(("type" . "choice")
                           ("instructions" . ,+same-question+)
                           ("criteria" . (("new" . "None of these: a different vulnerability, breach or package.")
                                          ,@(subseq listed 0 (min 250 (length listed)))))))))
    (shasht:write-json (json-object `(("state" . ,text) ("questions" . ,questions))) nil)))

(defun decision (answer)
  "ANSWER, Decide v1's JSON text, as (VALUES P-ACT P-NEW), P-NEW 1.0 without
the second question; NIL when it carries no P-ACT."
  (let* ((obj (handler-case (shasht:read-json answer) (error () nil)))
         (answers (and (hash-table-p obj) (or (gethash "answers" obj) obj))))
    (flet ((p (question option)
             (let* ((q (and (hash-table-p answers) (gethash question answers)))
                    (ps (and (hash-table-p q) (gethash "probabilities" q)))
                    (p (and (hash-table-p ps) (gethash option ps))))
               (and (realp p) (float p 1.0)))))
      (let ((act (p "kind" "act")))
        (and act (values act (if (and (hash-table-p answers) (gethash "same" answers)) (or (p "same" "new") 0.0) 1.0)))))))

(defun ask-decider (text listed)
  "Decide v1 on TEXT => (VALUES P-ACT P-NEW); signals when it does not answer.
Runs *DECIDER* through the organism's own side call, so the key is the one
`nodecode login' saved."
  (let ((answer (let ((nle::*auxiliary-model* *decider*))
                  (nle:complete "" (decision-request text listed) :max-tokens 64 :fallback-p nil))))
    (multiple-value-bind (act new) (decision answer)
      (unless act
        (error "Decide v1 answered no probability: ~a" (clip (princ-to-string answer) 200)))
      (values act new))))

(defparameter *decide* 'ask-decider
  "How an item is sorted: (funcall *DECIDE* TEXT LISTED) => (VALUES P-ACT P-NEW); tests stub it.")

;;; ---------------------------------------------------------------- the sources

(defun item (source id title url text &rest more)
  "One scan item: a plist the decider and the agent both read."
  (list* :source source :id id :title title :url url :text text more))

(defun kev-items (since)
  "CISA KEV entries added on or after SINCE's day."
  (loop for v across (gethash "vulnerabilities" (get-json *kev-url*))
        for added = (gethash "dateAdded" v "")
        for cve = (gethash "cveID" v)
        when (string>= (day added) (day since))
          collect (item :kev cve
                        (format nil "~a ~a: ~a" (gethash "vendorProject" v) (gethash "product" v)
                                (gethash "vulnerabilityName" v))
                        (format nil "https://nvd.nist.gov/vuln/detail/~a" cve)
                        (format nil "CISA added ~a (~a ~a, ~a) to its exploited list on ~a: ~a"
                                cve (gethash "vendorProject" v) (gethash "product" v)
                                (gethash "vulnerabilityName" v) added (gethash "shortDescription" v ""))
                        :cves (list cve) :exploited t)))

(defun next-link (headers)
  "The rel=\"next\" URL of a Link header in HEADERS, or NIL."
  (let ((link (and (hash-table-p headers) (str (gethash "link" headers)))))
    (when link
      (loop for part in (uiop:split-string link :separator ",")
            when (search "rel=\"next\"" part)
              return (let ((a (position #\< part)) (b (position #\> part)))
                       (and a b (subseq part (1+ a) b)))))))

(defun github-advisories (query since)
  "Every GitHub advisory matching QUERY published since SINCE's day, following pages."
  (loop with url = (format nil "~a?~a&published=%3E%3D~a&per_page=100" *github-url* query (day since))
        repeat *github-pages*
        while url
        nconc (multiple-value-bind (page headers) (get-json url)
                (setf url (next-link headers))
                (coerce page 'list))))

(defun packages-of (a)
  "Advisory A's packages, each (ECOSYSTEM NAME FIXED-OR-NIL)."
  (loop for v across (or (gethash "vulnerabilities" a) #())
        for p = (gethash "package" v)
        when (hash-table-p p)
          collect (list (gethash "ecosystem" p) (gethash "name" p) (str (gethash "first_patched_version" v)))))

(defun github-item (a kind)
  "Advisory A as one item of KIND, :reviewed or :malware."
  (let ((packages (packages-of a)))
    (item :github (gethash "ghsa_id" a) (gethash "summary" a "") (gethash "html_url" a)
          (format nil "GitHub advisory ~a (~a, ~a~@[, ~a~]): ~a. Packages: ~{~{~a ~a~@[ (fixed in ~a)~]~}~^; ~}. ~a"
                  (gethash "ghsa_id" a) (gethash "type" a) (gethash "severity" a)
                  (str (gethash "cve_id" a)) (gethash "summary" a "") packages
                  (clip (or (str (gethash "description" a)) "") 1200))
          :kind kind :packages packages :ids (list (gethash "ghsa_id" a))
          :cves (let ((c (str (gethash "cve_id" a)))) (and c (list c))))))

(defun github-items (since)
  "GitHub's reviewed critical and high advisories and its malware advisories since SINCE."
  (nconc (loop for severity in '("critical" "high")
               nconc (mapcar (lambda (a) (github-item a :reviewed))
                             (github-advisories (format nil "type=reviewed&severity=~a" severity) since)))
         (mapcar (lambda (a) (github-item a :malware)) (github-advisories "type=malware" since))))

(defparameter *deepseek-search-url* "https://api.deepseek.com/anthropic/v1/messages"
  "DeepSeek's own web search: its Anthropic-format Messages endpoint, asked to use
the native `web_search' server tool (as DeepSeek's harness does).")

(defparameter *deepseek-search-model* "deepseek-v4-flash" "The model that runs DeepSeek's search turn.")

(defun deepseek-key ()
  "This image's DeepSeek API key (provider `deepseek'), or NIL."
  (let ((k (getf (ignore-errors (nle::resolve-provider-credential "deepseek")) :key)))
    (and (stringp k) (plusp (length k)) k)))

(defun deepseek-listed (text)
  "The JSON-object lines of TEXT, DeepSeek's answer, as a table url => (DATE SUMMARY)."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (line (uiop:split-string text :separator '(#\Newline)) h)
      (let* ((line (string-trim '(#\Space #\Tab #\Return #\` #\- #\*) line))
             (o (and (plusp (length line)) (char= (char line 0) #\{)
                     (handler-case (shasht:read-json line) (error () nil))))
             (url (and (hash-table-p o) (str (gethash "url" o)))))
        (when url
          (setf (gethash url h) (list (or (str (gethash "date" o)) "") (or (str (gethash "summary" o)) ""))))))))

(defun deepseek-results (body)
  "BODY, DeepSeek's Messages answer, as ((:url :title :date :text) ...). Only
URLs its web_search_result blocks returned are kept, so nothing it wrote from
memory gets in; when its answer lists results (one JSON object per line:
url, date, summary) only those are kept -- the ones it dated inside the
window -- with its date and summary. Signals when the turn ran no search."
  (let* ((obj (shasht:read-json body))
         (blocks (coerce (or (and (hash-table-p obj) (gethash "content" obj)) #()) 'list))
         (listed (deepseek-listed (format nil "~{~a~^~%~}"
                                          (loop for b in blocks
                                                when (equal (gethash "type" b) "text")
                                                  collect (or (str (gethash "text" b)) "")))))
         (seen (make-hash-table :test 'equal)))
    (unless (some (lambda (b) (equal (gethash "type" b) "web_search_tool_result")) blocks)
      (error "DeepSeek ran no web search for this query"))
    (loop for b in blocks
          when (equal (gethash "type" b) "web_search_tool_result")
            nconc (loop for r across (let ((c (gethash "content" b))) (if (vectorp c) c #()))
                        for url = (str (gethash "url" r))
                        for mine = (and url (gethash url listed))
                        when (and url (equal (gethash "type" r) "web_search_result") (not (gethash url seen))
                                  (or mine (zerop (hash-table-count listed))))
                          collect (progn (setf (gethash url seen) t)
                                         (list :url url :title (or (str (gethash "title" r)) url)
                                               :date (or (and mine (plusp (length (first mine))) (first mine))
                                                         (str (gethash "page_age" r)) "")
                                               :text (or (second mine) "")))))))

(defun deepseek-search (query since)
  "News about QUERY since SINCE through DeepSeek's own web search."
  (let ((key (deepseek-key)))
    (unless key (error "no deepseek key"))
    (multiple-value-bind (body status)
        (nlk:http "POST" *deepseek-search-url*
                  :headers `(("x-api-key" . ,key) ("anthropic-version" . "2023-06-01")
                             ("content-type" . "application/json"))
                  :content (shasht:write-json
                            (json-object
                             `(("model" . ,*deepseek-search-model*)
                               ("max_tokens" . 4096)
                               ("messages" . ,(vector (json-object
                                                       `(("role" . "user")
                                                         ("content" . ,(format nil "Search the web for news published on or after ~a about: ~a. Then answer with one line per result your search returned that was published on or after ~a, each line one JSON object: {\"url\": the result's URL exactly, \"date\": its publication date, \"summary\": one sentence on what happened and what to patch or rotate}. No other text." (day since) query (day since)))))))
                               ("tools" . ,(vector (json-object '(("type" . "web_search_20250305")
                                                                  ("name" . "web_search")
                                                                  ("max_uses" . 3)))))
                               ("thinking" . (("type" . "disabled")))))
                            nil)
                  :timeout 120)
      (unless (eql status 200)
        (error "DeepSeek search answered ~a: ~a" status (clip (princ-to-string body) 200)))
      ;; its own dates are not to be trusted to honour the window: one it wrote
      ;; as a day before SINCE drops the result
      (remove-if (lambda (r) (let ((d (getf r :date)))
                               (and (>= (length d) 10) (digit-char-p (char d 0)) (char= (char d 4) #\-)
                                    (string< (subseq d 0 10) (day since)))))
                 (deepseek-results body)))))

(defun exa-search (query since)
  "News about QUERY since SINCE through Exa's search, when this image has it through MCP."
  (let* ((text (funcall (find-symbol "EXA/WEB-SEARCH-ADVANCED-EXA" "NODECODE-MCP")
                        :query query :category "news" :num-results 10 :text-max-characters 1200
                        :limit 200000 :start-published-date (day since)))
         (obj (handler-case (shasht:read-json text) (error () nil))))
    (unless (hash-table-p obj)
      (error "the news search answered no results list: ~a" (clip (princ-to-string text) 120)))
    (loop for r across (or (gethash "results" obj) #())
          for url = (str (gethash "url" r))
          when url
            collect (list :url url :title (or (str (gethash "title" r)) url)
                          :date (or (str (gethash "publishedDate" r)) "") :text (or (str (gethash "text" r)) "")))))

(defun news-search ()
  "The news search this image has => (VALUES FUNCTION NAME), FUNCTION taking
(QUERY SINCE) and answering ((:url :title :date :text) ...): DeepSeek's own web
search when a deepseek key resolves, else Exa's through MCP, else NIL."
  (cond ((deepseek-key) (values #'deepseek-search "deepseek"))
        ((let ((f (find-symbol "EXA/WEB-SEARCH-ADVANCED-EXA" "NODECODE-MCP"))) (and f (fboundp f)))
         (values #'exa-search "exa"))))

(defparameter *news-search* 'news-search
  "(funcall *NEWS-SEARCH*) => (VALUES FUNCTION NAME) as NEWS-SEARCH, or NIL; tests stub it.")

(defvar *news-engine* nil "The news search the last scan used, by name.")

(defun news-items (since)
  "News since SINCE for each of *NEWS-QUERIES*, or :SKIPPED with no search. A
query that fails is passed over; every query failing signals."
  (multiple-value-bind (search name) (funcall *news-search*)
    (setf *news-engine* name)
    (if (null search)
        :skipped
        (let ((seen (make-hash-table :test 'equal)) (failures '()) (items '()))
          (dolist (query *news-queries*)
            (handler-case
                (dolist (r (funcall search query since))
                  (let ((url (getf r :url)))
                    (unless (gethash url seen)
                      (setf (gethash url seen) t)
                      (push (item :news url (getf r :title) url
                                  (format nil "News, ~a: ~a. ~a" (getf r :date) (getf r :title) (clip (getf r :text) 1200))
                                  :ids (list url))
                            items))))
              (error (e) (push (princ-to-string e) failures))))
          (when (= (length failures) (length *news-queries*))
            (error "~a: every news query failed: ~a" name (first failures)))
          (nreverse items)))))

;;; ---------------------------------------------------------------- malware: a count, not a model

(defun count-at (url path)
  "The number at PATH (keys) in URL's JSON: 0 when URL is 404 (the registry
does not know the package; removed malware usually is), NIL when it cannot be told."
  (multiple-value-bind (body status) (handler-case (funcall *fetch* url) (error () nil))
    (cond ((eql status 404) 0)
          ((eql status 200)
           (let ((v (handler-case (shasht:read-json body) (error () nil))))
             (dolist (k path) (setf v (and (hash-table-p v) (gethash k v))))
             (and (realp v) v))))))

(defun npm-bulk-downloads (names)
  "Last month's downloads of unscoped npm NAMES, 128 per request (npm's bulk
endpoint), as a table name => count; a name the answer leaves out is absent."
  (let ((h (make-hash-table :test 'equal)))
    (loop for start from 0 below (length names) by 128
          for chunk = (subseq names start (min (length names) (+ start 128)))
          do (multiple-value-bind (body status)
                 (handler-case (funcall *fetch* (format nil "https://api.npmjs.org/downloads/point/last-month/~{~a~^,~}" chunk))
                   (error () nil))
               (when (eql status 200)
                 (let ((obj (handler-case (shasht:read-json body) (error () nil))))
                   (when (hash-table-p obj)
                     (if (gethash "downloads" obj)   ; a chunk of one answers flat
                         (setf (gethash (first chunk) h) (gethash "downloads" obj))
                         (maphash (lambda (k v) (setf (gethash k h) (and (hash-table-p v) (gethash "downloads" v 0)))) obj)))))))
    h))

(defun downloads (ecosystem name &optional npm-counts)
  "NAME's recent downloads on ECOSYSTEM's registry (see COUNT-AT); NIL when
the count cannot be told, as for an ecosystem with no count to ask."
  (cond ((string-equal ecosystem "npm")
         (multiple-value-bind (n found) (if npm-counts (gethash name npm-counts) (values nil nil))
           (if found
               (or n 0)
               (count-at (format nil "https://api.npmjs.org/downloads/point/last-month/~a" name) '("downloads")))))
        ((string-equal ecosystem "pip")
         (count-at (format nil "https://pypistats.org/api/packages/~a/recent" (string-downcase name))
                   '("data" "last_month")))
        ((string-equal ecosystem "rubygems")
         (count-at (format nil "https://rubygems.org/api/v1/gems/~a.json" name) '("downloads")))))

(defun popular-p (item &optional npm-counts)
  "Whether malware ITEM poisoned a package people use: one at or over
*POPULAR-AT* downloads, or one whose count cannot be told."
  (some (lambda (p)
          (let ((n (downloads (first p) (second p) npm-counts)))
            (or (null n) (>= n *popular-at*))))
        (or (getf item :packages) '(("unknown" "unknown" nil)))))

;;; ---------------------------------------------------------------- one item per package

(defun package-key (item)
  "The package ITEM is about, for grouping reviewed advisories."
  (let ((p (first (getf item :packages))))
    (and p (format nil "~a ~a" (first p) (second p)))))

(defun group-by-package (items)
  "ITEMS with reviewed advisories of one package folded into one item, critical first."
  (let ((groups (make-hash-table :test 'equal)) (order '()) (rest '()))
    (dolist (i items)
      (let ((k (and (eq (getf i :kind) :reviewed) (package-key i))))
        (if k
            (progn (unless (gethash k groups) (push k order)) (push i (gethash k groups)))
            (push i rest))))
    (append
     (loop for k in (nreverse order)
           for g = (stable-sort (reverse (gethash k groups)) #'>
                                :key (lambda (i) (if (search ", critical" (getf i :text)) 1 0)))
           collect (if (null (rest g))
                       (first g)
                       (list* :title (format nil "~a: ~d advisories (~a)" k (length g) (getf (first g) :title))
                              :text (clip (format nil "~{~a~^~%~%~}" (mapcar (lambda (i) (getf i :text)) g)) 6000)
                              :ids (mapcan (lambda (i) (copy-list (getf i :ids))) g)
                              :cves (mapcan (lambda (i) (copy-list (getf i :cves))) g)
                              :urls (mapcar (lambda (i) (getf i :url)) g)
                              (first g))))
     (nreverse rest))))

;;; ---------------------------------------------------------------- known and seen

(defun known-keys (entries)
  "Every CVE, source URL and GHSA id the feed's ENTRIES already carry."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e entries h)
      (dolist (c (field e "cves")) (setf (gethash (string-upcase c) h) t))
      (dolist (u (field e "sources"))
        (setf (gethash u h) t)
        (let ((g (search "GHSA-" u)))
          (when (and g (<= (+ g 19) (length u)))
            (setf (gethash (string-upcase (subseq u g (+ g 19))) h) t)))))))

(defun cves-in (text)
  "Every CVE id written in TEXT."
  (loop with start = 0
        for s = (search "CVE-" text :start2 start :test #'char-equal)
        while s
        collect (let ((e (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\-))) text :start (+ s 4))
                             (length text))))
                  (setf start e)
                  (string-upcase (subseq text s e)))))

(defun known-p (item keys)
  "Whether the feed already carries ITEM: its URL, an id, a CVE, or (news)
any CVE its text names."
  (or (gethash (getf item :url) keys)
      (some (lambda (k) (gethash (string-upcase k) keys)) (getf item :ids))
      (some (lambda (k) (gethash (string-upcase k) keys)) (getf item :cves))
      (and (eq (getf item :source) :news)
           (some (lambda (k) (gethash k keys)) (cves-in (getf item :text))))))

(defun listed (entries)
  "ENTRIES as ((ID . TITLE: SUMMARY) ...), the options the decider matches news against."
  (mapcar (lambda (e) (cons (gethash "id" e)
                            (clip (format nil "~a: ~a" (gethash "title" e "") (gethash "summary" e "")) 300)))
          entries))

(defun read-seen (path)
  "The seen ledger at PATH: id => ISO time it was sorted."
  (let ((h (make-hash-table :test 'equal)))
    (when (and path (probe-file path))
      (let ((obj (handler-case (shasht:read-json (uiop:read-file-string path)) (error () nil))))
        (when (hash-table-p obj)
          (maphash (lambda (k v) (setf (gethash k h) v)) obj))))
    h))

(defun write-seen (path seen now)
  "Write SEEN to PATH, dropping ids sorted more than *SEEN-DAYS* before NOW."
  (let ((keep (make-hash-table :test 'equal))
        (floor (- now (* *seen-days* 86400))))
    (maphash (lambda (k v) (when (>= (or (ignore-errors (parse-iso v)) now) floor)
                             (setf (gethash k keep) v)))
             seen)
    (ensure-directories-exist path)
    (uiop:with-output-file (out path :if-exists :supersede)
      (shasht:write-json keep out))))

;;; ---------------------------------------------------------------- the scan

(defun scan (&key since seen (entries (advisories)))
  "Collect the items published since SINCE (default: the feed's `updated'
less a day), drop those the feed's ENTRIES carry or the SEEN ledger (a
path; NIL keeps none) already holds, and sort the rest.
=> (:since ISO :decider :ok|:unavailable :error TEXT
    :sources ((SOURCE GOT FRESH) | (SOURCE :skipped) | (SOURCE :failed WHY) ...)
    :keep ITEMS :dropped ((REASON . N) ...) :expiring IDS)
Each kept item carries :why -- :exploited, :popular-malware, :decider (with
:p, its P(act)) or :unsorted. :DECIDER :unavailable means the decider did
not answer: every item it would have sorted is in :KEEP as :unsorted, and
nothing is written to the ledger, so the agent sorts them by hand."
  (let* ((now (get-universal-time))
         (since (or since
                    (let ((u (getf *feed-state* :updated)))
                      (and (stringp u) (format-iso (- (parse-iso u) 86400))))
                    (format-iso (- now (* 14 86400)))))
         (keys (known-keys entries))
         (ledger (read-seen seen))
         (sources '()) (items '()))
    (dolist (source (list (cons :kev #'kev-items) (cons :github #'github-items) (cons :news #'news-items)))
      (let ((got (handler-case (funcall (cdr source) since)
                   (error (e) (push (list (car source) :failed (princ-to-string e)) sources) :failed))))
        (cond ((eq got :failed))
              ((eq got :skipped) (push (list (car source) :skipped) sources))
              (t (let ((fresh (remove-if (lambda (i) (or (known-p i keys)
                                                         (some (lambda (k) (gethash k ledger)) (getf i :ids))
                                                         (gethash (getf i :id) ledger)))
                                         got)))
                   (push (list (car source) (length got) (length fresh)) sources)
                   (setf items (append items fresh)))))))
    (let* ((keep '())
           (dropped (list (cons :minor-or-elsewhere 0) (cons :already-listed 0) (cons :unpopular-malware 0)))
           (decider :ok) (error nil) (sorted '())
           (options (listed entries))
           (npm-counts (npm-bulk-downloads
                        (remove-duplicates
                         (loop for i in items
                               when (eq (getf i :kind) :malware)
                                 nconc (loop for (eco name) in (getf i :packages)
                                             when (and (string-equal eco "npm") (not (find #\/ name)))
                                               collect name))
                         :test #'string=))))
      (dolist (i (group-by-package items))
        (cond ((getf i :exploited) (push (list* :why :exploited i) keep))
              ((eq (getf i :kind) :malware)
               (if (popular-p i npm-counts)
                   (push (list* :why :popular-malware i) keep)
                   (incf (cdr (assoc :unpopular-malware dropped))))
               (push i sorted))
              ((eq decider :unavailable) (push (list* :why :unsorted i) keep))
              (t (handler-case (multiple-value-bind (p new) (funcall *decide* (getf i :text) options)
                                 (cond ((< p *keep-at*) (incf (cdr (assoc :minor-or-elsewhere dropped))))
                                       ((< new *keep-at*) (incf (cdr (assoc :already-listed dropped))))
                                       (t (push (list* :why :decider :p p :new new i) keep)))
                                 (push i sorted))
                   (error (e) (setf decider :unavailable error (princ-to-string e))
                     (push (list* :why :unsorted i) keep))))))
      (when (and seen (eq decider :ok))
        (dolist (i (append sorted keep))
          (dolist (k (or (getf i :ids) (list (getf i :id))))
            (setf (gethash k ledger) (format-iso now))))
        (write-seen seen ledger now))
      (list :since since :decider decider :error error :sources (nreverse sources)
            :keep (nreverse keep) :dropped dropped
            :expiring (expiring entries now)))))

(defun expiring (entries now)
  "The ids of ENTRIES older than 14 days at NOW and not exploited: due to be dropped."
  (loop for e in entries
        for date = (str (gethash "date" e))
        when (and date (not (eq (gethash "exploited" e) t))
                  (< (or (ignore-errors (parse-iso date)) now) (- now (* 14 86400))))
          collect (gethash "id" e)))

(defun scan-report (&rest args &key since seen entries)
  "SCAN, as the text the agent reads: what was asked of whom, what each
source gave, why each item stays, and what was dropped. Same keys as SCAN."
  (declare (ignore since seen entries))
  (let ((r (apply #'scan args)))
    (with-output-to-string (s)
      (format s "scan since ~a; decider ~a~@[ (~a)~]~%" (getf r :since)
              (if (eq (getf r :decider) :ok)
                  (format nil "~a/~a answered" (getf *decider* :provider) (getf *decider* :model))
                  "UNAVAILABLE: sort every :unsorted item by hand")
              (getf r :error))
      (format s "sources: ~{~a~^; ~}~%"
              (mapcar (lambda (x) (destructuring-bind (name a &optional b) x
                                    (case a
                                      (:skipped (format nil "~(~a~) skipped (no search here)" name))
                                      (:failed (format nil "~(~a~) FAILED: ~a" name b))
                                      (t (if (and (eq name :news) *news-engine*)
                                             (format nil "news via ~a ~d, ~d not yet listed or sorted" *news-engine* a b)
                                             (format nil "~(~a~) ~d, ~d not yet listed or sorted" name a b))))))
                      (getf r :sources)))
      (format s "dropped: ~{~(~a~) ~d~^, ~}~%"
              (loop for (k . n) in (getf r :dropped) collect k collect n))
      (format s "expiring (older than 14 days, not exploited): ~:[none~;~:*~{~a~^, ~}~]~%" (getf r :expiring))
      (format s "keep: ~d~%" (length (getf r :keep)))
      (dolist (i (getf r :keep))
        (format s "- [~(~a~)~@[ ~,2f~]] ~a~%  ~{~a~^ ~}~%" (getf i :why) (getf i :p) (getf i :title)
                (or (getf i :urls) (list (getf i :url))))))))
