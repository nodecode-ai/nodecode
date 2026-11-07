;;;; search.lisp --- one verb: a configured provider, else the public floor.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two tiers, plus one engine that only ever answers when named. A provider
;;;; with a key (brave, exa, tavily) answers alone, the named one first; a
;;;; failure or an empty answer advances to the next keyed one. With no keyed
;;;; answer the PUBLIC FLOOR runs: DuckDuckGo's HTML endpoint, Firecrawl's
;;;; keyless search and Exa's keyless MCP endpoint, all asked, their hits
;;;; merged, deduped on a canonical url and ranked by consensus (how many
;;;; engines named the url) then best rank. arXiv is keyless too, but it
;;;; answers papers, not pages: merged into every search it would put a
;;;; preprint beside the page that answers, so it runs only when the ask
;;;; names it. An engine that fails is a [note] above the results, never the
;;;; end of the turn; only every engine failing is an error, and it names
;;;; each failure.
;;;;
;;;; Why a floor and why these three (nc-private#33): a beta's session spent
;;;; 31 searches on three questions because one keyless engine answered with
;;;; page openings the model could not settle on, and each of its six failures
;;;; ended a round. DuckDuckGo's HTML endpoint answers a plain client with
;;;; real snippets once the request carries a browser's fetch-metadata
;;;; headers (a bare user-agent gets its 202 challenge page) but challenges a
;;;; burst from one address; Firecrawl answers keyless with clean
;;;; descriptions; Exa carries dates and finds what the others miss. Mojeek
;;;; and Startpage answer captchas without a real browser and are not a floor
;;;; this cell has.
;;;;
;;;;   brave       GET  api.search.brave.com/res/v1/web/search?q=&count=   X-Subscription-Token
;;;;               web.results[].{title,url,description,page_age}; no answer
;;;;   exa         POST api.exa.ai/answer {query}                          x-api-key
;;;;               {answer, citations[].{title,url,text,publishedDate}}
;;;;   tavily      POST api.tavily.com/search {query,max_results,include_answer:"basic"}   Bearer
;;;;               {answer, results[].{title,url,content,published_date}}
;;;;   duckduckgo  POST html.duckduckgo.com/html/ q=&kl=                  no key
;;;;               result blocks: result__a (title, href, often a //duckduckgo.com/l/?uddg=
;;;;               redirect), result__snippet; result--ad blocks skipped
;;;;   firecrawl   POST api.firecrawl.dev/v2/search {query,limit,sources}  no key (Bearer lifts limits)
;;;;               {success, data.web[].{title,url,description,position}}
;;;;   exa keyless POST mcp.exa.ai/mcp?tools=web_search_exa               no key
;;;;               a JSON-RPC tools/call answered as SSE whose one data: line carries
;;;;               result.content[0].text - `Title: / URL: / Published: / Highlights:'
;;;;               blocks (or raw Exa JSON)
;;;;   arxiv       GET  export.arxiv.org/api/query?search_query=&max_results=   no key
;;;;               an Atom feed, one <entry> per paper: <id> (the abs url),
;;;;               <title>, <summary> (the abstract), <published>; a bare
;;;;               term list is the API's OR (48,541 papers for the two
;;;;               terms a searcher would expect ANDed), so a plain query
;;;;               becomes all:TERM AND all:TERM, while arXiv's own field
;;;;               syntax (ti:, au:, cat:, all:) passes through verbatim
;;;;
;;;; The render is one numbered line per hit — title, snippet, date when
;;;; known — and its url under it: 160 chars of snippet, 3,000 of answer,
;;;; where the page-opening render before it spent 500 and 7,000. A
;;;; provider's own synthesised answer stays on top when it gives one.
;;;;
;;;; Every field is read through NLK:JSON-VALUE: absent, null and the wrong
;;;; type read NIL. The key rides in a header; no url ever carries it.

(in-package #:nodecode-websearch)

;;; brave, exa and tavily take a key and answer alone; duckduckgo, firecrawl
;;; and exa answer without one, as the public floor (a firecrawl key only
;;; lifts its limits); arxiv answers without one when it is named.
(defparameter +provider-names+ '("brave" "exa" "tavily" "duckduckgo" "firecrawl" "arxiv")
  "What :provider and websearch.provider may name.")

(defparameter +keyed-providers+ '("brave" "exa" "tavily"))

(defparameter +floor+ '("duckduckgo" "firecrawl" "exa")
  "The engines the public floor merges, in tiebreak order; the names that
may be configured or asked for without a key.")

;;; arXiv's API returns paper metadata, not pages: as a floor member it would
;;; put a preprint beside every web answer, so it is asked for, never merged.
(defparameter +named-keyless+ '("arxiv")
  "Keyless engines that answer only when named.")

(defparameter +floor-window+ 10
  "How many hits each floor engine is asked for, whatever N the caller
wants: consensus needs breadth, and the merge takes the first N.")

(defparameter +search-limit+ 3000
  "Character cap on a search answer, well under EVAL's 8000: ten hits with
their urls fit, and a second search costs the context little.")
(defparameter +snippet-limit+ 160
  "Character cap on one snippet: a line the model reads, not a page opening.")
(defparameter +title-limit+ 90)

;;; --- hits ----------------------------------------------------------------------

(defstruct (hit (:copier nil) (:predicate nil))
  (title "" :type string)
  (url "" :type string)
  (snippet "" :type string)
  (date nil :type (or null string)))

(nlk:access (hit hit) (kept hit))

(defun iso-date (value)
  "VALUE's leading YYYY-MM-DD, or NIL: Exa's `2026-02-24T00:00:00.000Z', Brave's
page_age, Tavily's published_date all start with one; `N/A' and prose do not."
  (and (stringp value)
       (cl-ppcre:scan "^\\d{4}-\\d{2}-\\d{2}" value)
       (subseq value 0 10)))

(defun make-row (title url snippet date)
  "A HIT from raw provider fields, clipped to the render's limits; NIL
without a url."
  (and (stringp url) (plusp (length url))
       (make-hit :title (nlk:clip (nlk:one-line (if (string= (nlk:one-line title) "") url title))
                                  +title-limit+ :ellipsis "…")
                 :url url
                 :snippet (nlk:clip (nlk:one-line snippet) +snippet-limit+ :ellipsis "…")
                 :date (iso-date date))))

(defun first-hits (n rows &aux (hits (remove nil rows)))
  "The first N of ROWS that are hits: MAKE-ROW's NIL, a row without a url, is not."
  (subseq hits 0 (min n (length hits))))

(defun rows-of (object path title-key url-key snippet-key date-key n)
  "The first N hits under PATH in OBJECT; a row without a url is not a result."
  (first-hits n (loop for row across (apply #'nlk:json-array object path)
                      collect (make-row (nlk:json-value row :text title-key)
                                        (nlk:json-value row :text url-key)
                                        (nlk:json-value row :string snippet-key)
                                        (nlk:json-value row :text date-key)))))

(defun decode-json (text)
  "TEXT as decoded JSON, or a loud failure."
  (handler-case (nlk:decode-json text)
    (error ()
      (fail "answered with something that is not JSON"))))

(defun json-headers (&optional key)
  "The headers a JSON API request carries, KEY its bearer when there is one."
  `(("accept" . "application/json") ("content-type" . "application/json")
    ,@(and key `(("authorization" . ,(concatenate 'string "Bearer " key))))))

(defun http-text (method url &rest keys &key content &allow-other-keys)
  "HTTP's answer as text; anything but a 200 is a loud failure naming the
status and the opening of what came back."
  ;; A JSON object CONTENT goes as its encoding.
  (when (hash-table-p content)
    (setf keys (list* :content (nlk:encode-json-object content) keys)))
  (nlk:bind (((body status) (apply #'http method url keys)) (text (nlk:body-text body)))
    (unless (eql status 200)
      (fail "HTTP ~a: ~a" status (nlk:clip (nlk:one-line text) 300 :disclose t)))
    text))

;;; --- the JSON APIs: the keyed providers, and firecrawl ------------------------

(defun api-search (provider key query n)
  "PROVIDER's one request, with KEY when there is one (firecrawl needs none)."
  ;; => (values ANSWER HITS); a non-200, a body that is not JSON, or
  ;; firecrawl's success:false with an error is a loud failure. Firecrawl's
  ;; data.web rows come in position order.
  (destructuring-bind (method url headers content path snippet date)
      (cond
        ((string= provider "brave")
         (list :get (format nil "https://api.search.brave.com/res/v1/web/search?q=~a&count=~d"
                            (quri:url-encode query :encoding :utf-8) n)
               `(("accept" . "application/json")
                 ("x-subscription-token" . ,key))
               nil '("web" "results") "description" "page_age"))
        ((string= provider "exa")
         (list :post "https://api.exa.ai/answer"
               (append (json-headers) `(("x-api-key" . ,key)))
               (nlk:json-object "query" query)
               '("citations") "text" "publishedDate"))
        ((string= provider "firecrawl")
         (list :post "https://api.firecrawl.dev/v2/search" (json-headers key)
               (nlk:json-object "query" query "limit" n
                                "sources" (vector (nlk:json-object "type" "web")))
               '("data" "web") "description" "date"))
        (t
         (list :post "https://api.tavily.com/search"
               (json-headers key)
               (nlk:json-object "query" query "max_results" n "include_answer" "basic")
               '("results") "content" "published_date")))
    (let ((object (decode-json (http-text method url :headers headers :content content
                                                     :timeout 30))))
      (when (and (string= provider "firecrawl") (null (nlk:json-value object :boolean "success")))
        (nlk:when-let (why (nlk:json-value object :text "error")) (fail "~a" why)))
      (values (and (string/= provider "brave") (nlk:json-value object :text "answer"))
              (rows-of object path "title" "url" snippet date n)))))

;;; --- exa, keyless --------------------------------------------------------------

(defun exa-block-results (text)
  "Hits from the MCP tool's text — `Title: / URL: / Published: / Highlights:'
blocks separated by `---' lines."
  (loop for block in (cl-ppcre:split "(?m)^---[ \\t]*$" text)
        for highlights = (or (group "(?s)Highlights:[ \\t]*\\n?(.*)$" block) "")
        collect (make-row (group "(?m)^Title:[ \\t]*(.*)$" block)
                          (group "(?m)^URL:[ \\t]*(\\S+)" block)
                          (cl-ppcre:regex-replace-all "(?m)^\\.\\.\\.$" highlights " ")
                          (group "(?m)^Published:[ \\t]*(\\S+)" block))))

(defun exa-mcp (tool arguments timeout)
  "One JSON-RPC tools/call to Exa's keyless MCP endpoint."
  ;; TOOL is both the `tools=' query and the tool's name, ARGUMENTS its
  ;; arguments object. => the answer's body text; anything but a 200 is a loud
  ;; failure.
  (http-text :post (format nil "https://mcp.exa.ai/mcp?tools=~a" tool)
             :headers '(("accept" . "application/json, text/event-stream")
                        ("content-type" . "application/json"))
             :content (nlk:json-object "jsonrpc" "2.0"
                                       "id" 1
                                       "method" "tools/call"
                                       "params" (nlk:json-object "name" tool
                                                                 "arguments" arguments))
             :timeout timeout))

(defun exa-mcp-text (text)
  "The tool's text inside the MCP endpoint's SSE TEXT: the last event's
`data:' lines decoded, result.content[0].text."
  ;; A JSON-RPC error or an isError result is a loud failure.
  (let* ((object (decode-json
                  ;; The last SSE event's `data:' lines, joined; no `data:' line: the whole body.
                  (let ((data nil))
                    (with-input-from-string (in text)
                      (nlk:read-events in (lambda (event) (setf data event) nil)))
                    (or data (string-trim '(#\Space #\Newline #\Return) text)))))
         (content (nlk:json-value object :array "result" "content"))
         (first-text (and content (plusp (length content))
                          (nlk:json-value (aref content 0) :string "text")))
         (error-text (or (nlk:json-value object :text "error" "message")
                         (and (nlk:json-value object :boolean "result" "isError") first-text))))
    (when error-text
      (fail "~a" error-text))
    (or first-text "")))

(defun exa-mcp-results (text)
  "Hits from the MCP endpoint's SSE TEXT."
  ;; The tool's text is either result blocks or raw Exa JSON.
  (let* ((inner (exa-mcp-text text))
         (json (and (plusp (length inner)) (char= (char (string-left-trim " " inner) 0) #\{)
                    (ignore-errors (nlk:decode-json inner)))))
      (if (hash-table-p json)
          (loop for row across (nlk:json-array json "results")
                for highlights = (nlk:json-value row :array "highlights")
                collect (make-row (nlk:json-value row :text "title")
                                  (nlk:json-value row :text "url")
                                  (if (and highlights (plusp (length highlights)))
                                      (format nil "~{~a~^ ~}" (coerce highlights 'list))
                                      (or (nlk:json-value row :string "text") ""))
                                  (nlk:json-value row :text "publishedDate")))
          (exa-block-results inner))))

(defun exa-keyless-search (query n)
  "The keyless MCP endpoint's hits for QUERY. => (values NIL HITS)."
  (values nil (first-hits n (exa-mcp-results (exa-mcp "web_search_exa"
                                                      (nlk:json-object "query" query
                                                                       "objective" query
                                                                       "numResults" n)
                                                      30)))))

;;; --- arxiv ---------------------------------------------------------------------

;;; One request per search, well inside the API's courtesy limit of one
;;; request every three seconds.
(defparameter +arxiv-endpoint+ "https://export.arxiv.org/api/query"
  "arXiv's Atom API: keyless metadata - title, abstract, date, the abs url -
for papers.")

(defparameter +arxiv-field-scanner+
  (cl-ppcre:create-scanner "(?i)\\b(?:au|ti|abs|co|jr|cat|rn|all|id|doi):")
  "A query that already writes arXiv's own field syntax (ti:, au:, cat:).")

(defun arxiv-query (query &aux (text (string-trim '(#\Space #\Tab #\Newline) query)))
  "QUERY as the API's search_query."
  ;; One that already names a field is passed verbatim; a plain one becomes
  ;; every term as all:TERM joined with AND, because the API reads a bare term
  ;; list as OR.
  (if (cl-ppcre:scan +arxiv-field-scanner+ text)
      text
      (format nil "~{all:~a~^ AND ~}"
              (or (remove "" (cl-ppcre:split "\\s+" text) :test #'string=) '("")))))

(defparameter *arxiv-entry-scanner*
  (cl-ppcre:create-scanner "(?s)<entry>(.*?)(?:</entry>|\\z)")
  "One Atom entry's body in arXiv's answer feed.")

(defun arxiv-entry (block)
  "The HIT one Atom entry BLOCK carries: the abs url its <id> names, its
<title>, the abstract's opening <summary> and <published>. NIL without an id."
  (let ((id (nlk:one-line (decode-entities (or (group "<id>([^<]*)</id>" block) "")))))
    (when (plusp (length id))
      (make-row
       (decode-entities (or (group "(?s)<title>(.*?)</title>" block) ""))
       (if (uiop:string-prefix-p "http://" id)
           (concatenate 'string "https://" (subseq id 7))
           id)
       (decode-entities (or (group "(?s)<summary>(.*?)</summary>" block) ""))
       (nlk:one-line (group "<published>([^<]*)</published>" block))))))

(defun arxiv-parse (feed n &aux (entries '()))
  "The first N hits on the Atom FEED arXiv answered with, in the API's order."
  (cl-ppcre:do-register-groups (block) (*arxiv-entry-scanner* feed)
    (push (arxiv-entry block) entries))
  (first-hits n (nreverse entries)))

(defun arxiv-search (query n)
  "arXiv's hits for QUERY."
  ;; => (values NIL HITS): keyless paper metadata, at
  ;; most N, in the API's relevance order.
  (let ((feed (http-text
               :get (format nil "~a?search_query=~a&start=0&max_results=~d"
                            +arxiv-endpoint+
                            (quri:url-encode (arxiv-query query) :encoding :utf-8)
                            n)
               :headers `(("user-agent" . ,+user-agent+)
                          ("accept" . "application/atom+xml,application/xml;q=0.9,*/*;q=0.8"))
               :timeout 30)))
    (values nil (arxiv-parse feed n))))

;;; --- duckduckgo ----------------------------------------------------------------

(defparameter *ddg-block-scanner*
  (cl-ppcre:create-scanner "<div\\s+class=\"result\\s+results_links[^\"]*\"" :case-insensitive-mode t)
  "Where one result begins on the HTML endpoint's page.")

(defparameter *ddg-title-scanner*
  (cl-ppcre:create-scanner "<a\\b[^>]*class=\"result__a\"[^>]*>" :case-insensitive-mode t))

(defparameter *ddg-snippet-scanner*
  (cl-ppcre:create-scanner "<a\\b[^>]*class=\"result__snippet\"[^>]*>" :case-insensitive-mode t))

(defun ddg-anchor (scanner block)
  "(values HREF INNER-TEXT) of the anchor SCANNER finds in BLOCK, or NIL."
  (multiple-value-bind (start end) (cl-ppcre:scan scanner block)
    (when start
      (let* ((tag (subseq block start end))
             (close (cl:search "</a" block :start2 end))
             (href (nlk:when-let (value (group "href=\"([^\"]*)\"" tag)) (decode-entities value)))
             (inner (subseq block end (or close (length block)))))
        (values href
                (nlk:one-line (decode-entities
                               (cl-ppcre:regex-replace-all *tag-scanner* inner " "))))))))

(defun ddg-target (href)
  "The page HREF points at: the `uddg' parameter of a //duckduckgo.com/l/
redirect, else HREF itself; NIL unless it is an http(s) url."
  (let ((url (or (cl-ppcre:register-groups-bind (value) ("[?&]uddg=([^&]+)" href)
                   (ignore-errors (quri:url-decode value)))
                 href)))
    (and (stringp url)
         (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
         url)))

(defun duckduckgo-parse (html n)
  "The first N organic hits on the HTML endpoint's page: each result block's
title anchor and snippet anchor, ad blocks skipped."
  (let ((starts (loop for (start) on (cl-ppcre:all-matches *ddg-block-scanner* html) by #'cddr
                      collect start)))
    (loop for (start next) on starts
          for block = (subseq html start (or next (length html)))
          for opener = (subseq block 0 (or (position #\> block) (length block)))
          for hit = (and (not (cl:search "result--ad" opener))
                         (multiple-value-bind (href title) (ddg-anchor *ddg-title-scanner* block)
                           (nlk:when-let (url (and href (ddg-target href)))
                             (make-row title url
                                       (or (nth-value 1 (ddg-anchor *ddg-snippet-scanner* block)) "")
                                       nil))))
          while (< (length results) n)
          when (and hit (not (find hit.url results :key #'hit-url :test #'string=)))
            collect hit into results
          finally (return results))))

(defun duckduckgo-search (query n)
  "The HTML endpoint's hits for QUERY."
  ;; => (values NIL HITS). Anything but a
  ;; 200 is a failure: the endpoint answers a bare client with a 202 challenge
  ;; page, which is why the request carries the browser header set.
  (multiple-value-bind (body status)
      (http :post "https://html.duckduckgo.com/html/"
            :headers (browser-headers "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
                                      '(("content-type" . "application/x-www-form-urlencoded")
                                        ("referer" . "https://html.duckduckgo.com/")))
            :content (format nil "q=~a&kl=us-en" (quri:url-encode query :encoding :utf-8))
            :timeout 20)
    (unless (eql status 200)
      (fail "HTTP ~a~:[~;, its bot challenge~]" status (eql status 202)))
    (values nil (duckduckgo-parse (nlk:body-text body) n))))

;;; --- the merge -------------------------------------------------------------------

(defun canonical-url (url)
  "URL as the key two engines' spellings of one page share: host lowercased,
the fragment gone, a trailing slash gone, utm_* parameters gone."
  ;; An unparseable url is its own key.
  (nlk:with-handlers ((error () url))
    (let* ((uri (quri:uri url))
           (host (string-downcase (or (quri:uri-host uri) "")))
           (path (string-right-trim "/" (or (quri:uri-path uri) "")))
           (query (quri:uri-query uri))
           (kept (and query
                      (remove-if (lambda (pair) (uiop:string-prefix-p "utm_" (car pair)))
                                 (quri:url-decode-params query :lenient t)))))
      (format nil "~a~a~@[?~a~]" host path (and kept (quri:url-encode-params kept))))))

(defstruct (tally (:copier nil) (:predicate nil))
  hit (engines 0 :type fixnum) (best 0 :type fixnum))

(nlk:access (tally tally))

(defun merge-hits (per-engine n &aux (table (make-hash-table :test #'equal))
                                     (tallies '()))
  "The first N of the engines' hit lists merged: one entry per canonical
url, ranked by how many engines named it, then the best rank any gave it,
then the order the engines are listed in."
  ;; The longest snippet and any date survive the merge.
  (dolist (hits per-engine)
    (loop for hit in hits
          for rank from 0
          for tally = (alexandria:ensure-gethash (canonical-url hit.url) table
                        (car (push (make-tally :hit hit :best rank) tallies)))
          for kept = tally.hit
          do (incf tally.engines)
             (setf tally.best (min tally.best rank))
             (when (> (length hit.snippet) (length kept.snippet))
               (setf kept.snippet hit.snippet))
             (unless kept.date
               (setf kept.date hit.date))))
  ;; Stable sorts, the least significant key first, over the first-seen order.
  (setf tallies (stable-sort (nreverse tallies) #'< :key #'tally-best))
  (mapcar #'tally-hit (first-hits n (stable-sort tallies #'> :key #'tally-engines))))

;;; --- the render ----------------------------------------------------------------

(defun render-search (query answer hits notes)
  "The answer text: each note in brackets, ANSWER when a provider gave one,
then one numbered line per hit — title, snippet, date — with its url under
it; `No results' when nothing came back."
  (nlk:clip
   (with-output-to-string (out)
     (dolist (note notes)
       (format out "[~a]~%" note))
     (when notes (terpri out))
     (when (and (stringp answer) (plusp (length (string-trim '(#\Space #\Newline) answer))))
       (format out "~a~%~%" (string-trim '(#\Space #\Newline) answer)))
     (if hits
         (loop for hit in hits
               for i from 1
               do (format out "~:[~%~%~;~]~d. ~a~:[~*~; — ~a~]~@[ (~a)~]~%   ~a"
                          (= i 1) i hit.title
                          (plusp (length hit.snippet)) hit.snippet
                          hit.date hit.url))
         (format out "No results for ~s." query)))
   +search-limit+ :disclose t))

;;; --- the verb --------------------------------------------------------------------

(defun ask (engine query n key)
  "ENGINE's answer for QUERY, KEY its configured key or NIL."
  ;; => (values
  ;; ANSWER HITS), or (values NIL NIL NOTE) when it failed: the failure's
  ;; redacted text, one line. The turn's cancel passes through.
  (handler-case
      (cond ((string= engine "arxiv") (arxiv-search query n))
            ((string= engine "duckduckgo") (duckduckgo-search query n))
            ((and (string= engine "exa") (null key)) (exa-keyless-search query n))
            (t (api-search engine key query n)))
    (nlk:turn-cancelled-condition (condition) (error condition))
    (error (condition)
      (values nil nil (redact (nlk:one-line (princ-to-string condition) :cap 200))))))

(defun search (query &key provider n)
  "Web search for QUERY."
  ;; N results (default websearch.max_results, 5; at most 10). PROVIDER names
  ;; one of brave, exa, tavily, duckduckgo, firecrawl, arxiv to ask first
  ;; (default websearch.provider). A provider with a key answers alone; one
  ;; that fails or answers nothing advances to the next keyed one, then to the
  ;; public floor: duckduckgo, firecrawl and keyless exa, merged. A provider
  ;; named without a key is a note above the floor's results. arxiv takes no
  ;; key and is never merged: named, it answers alone with papers — title,
  ;; abstract, date, the abs url. Returns a string: [notes], the provider's
  ;; answer when it gives one, then one numbered line per result — title,
  ;; snippet, date when known — and its url.
  ;; What a transcript calls this call (activity.lisp): a search, about QUERY.
  (nle:receipt "calls" (list* :verb "web:search" :family "search"
                              (and (stringp query) (list :source query))))
  (with-redacted-errors
    (let* ((settings (running-settings))
           (keys (getf settings :keys))
           (first (cond
                    ((null provider) (getf settings :provider))
                    ((not (stringp provider))
                     (fail "provider must be a string naming one of ~{~a~^, ~}" +provider-names+))
                    ((not (member provider +provider-names+ :test #'string=))
                     (fail "unknown provider ~s: one of ~{~a~^, ~}" provider +provider-names+))
                    (t provider)))
           (n (min (max (if (integerp n) n (getf settings :max-results)) 1) 10))
           (notes '()))
      (unless (and (stringp query) (plusp (length (string-trim " " query))))
        (fail "query must be a non-empty string"))
      (flet ((note (control &rest args)
               (setf notes (append notes (list (apply #'format nil control args))))))
        ;; Tier 0: a keyless engine with no floor place (arxiv) answers
        ;; alone when named; a failure is its [note] and the tiers below
        ;; still run.
        (when (member first +named-keyless+ :test #'string=)
          (multiple-value-bind (answer hits failure) (ask first query n nil)
            (if failure
                (note "~a: ~a" first failure)
                (return-from search (render-search query answer hits notes)))))
        ;; Tier 1: the keyed providers, FIRST first; the first that answers
        ;; is the answer. Naming a keyless engine skips the tier: the ask
        ;; was for the floor.
        (let ((keyed (if (and (member first +floor+ :test #'string=)
                              (not (and (string= first "exa") (assoc "exa" keys :test #'string=))))
                         '()
                         (remove-duplicates (cons first (mapcar #'car keys))
                                            :test #'string= :from-end t))))
          (dolist (engine keyed)
            (let ((key (cdr (assoc engine keys :test #'string=))))
              (cond
                ((not (member engine +keyed-providers+ :test #'string=)))
                ((null key)
                 (note "~a has no api_key (websearch.providers.~a.api_key); answered without it"
                       engine engine))
                (t
                 (multiple-value-bind (answer hits failure) (ask engine query n key)
                   (cond (failure (note "~a: ~a" engine failure))
                         ((or hits (and (stringp answer) (plusp (length answer))))
                          (return-from search (render-search query answer hits notes)))
                         (t (note "~a: no results" engine)))))))))
        ;; Tier 2: the public floor, every engine asked and the hits merged;
        ;; a named floor engine leads the tiebreak.
        (let ((per-engine '()))
          (dolist (engine (if (member first +floor+ :test #'string=)
                              (cons first (remove first +floor+ :test #'string=))
                              +floor+))
            (multiple-value-bind (answer hits failure)
                (ask engine query +floor-window+ (cdr (assoc engine keys :test #'string=)))
              (declare (ignore answer))
              (cond (failure (note "~a: ~a" engine failure))
                    (t (push hits per-engine)))))
          (when (null per-engine)
            (fail "no engine answered ~s: ~{~a~^; ~}" query notes))
          (render-search query nil (merge-hits (nreverse per-engine) n) notes))))))
