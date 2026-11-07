;;;; cell-test.lisp --- the websearch cell, end to end over a scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (page nodecode-websearch::page))

;;; --- lifecycle and config -------------------------------------------------------

(define-cell-lifecycle-tests "websearch"
  (:help :web)
  (:running (is (equal nodecode-websearch::+primer+ (nle:help :web)) "(help :web) is the primer"))
  (:refused ("provider" "nope") ("provider" "tavily") ("max_results" 0)
            ("fetch" (nlk:json-object "reader" "bogus")) ("providers" "x")
            ("providers" (nlk:json-object "brave" (nlk:json-object "api_key" 42)))
            ("fetch" (nlk:json-object "limit" 10)))
  (:idle web:websearch-error
         (web:search "x") (web:fetch "https://a")))

(deftest websearch-cell-picks-the-default-provider ()
  (macrolet ((picks (provider description &rest config-pairs)
               `(with-websearch ,config-pairs
                  (is (equal ,provider
                             (nodecode-websearch::setting :provider))
                      ,description))))
    (picks "tavily" "the only configured provider is the default"
           "providers" (websearch-providers "tavily" "t-key"))
    (picks "brave" "brave first when both are configured and none is named"
           "providers" (websearch-providers "exa" "e-key" "brave" "b-key"))
    (picks "exa" "a named provider wins"
           "providers" (websearch-providers "exa" "e-key" "brave" "b-key") "provider" "exa")
    (picks "duckduckgo" "no key: the public floor, duckduckgo leading" "providers" (nlk:make-json-object))
    (picks "exa" "exa may be named without a key" "providers" (nlk:make-json-object) "provider" "exa")
    (picks "duckduckgo" "duckduckgo may be named, it never has a key"
           "providers" (websearch-providers "brave" "b-key") "provider" "duckduckgo")
    (picks "firecrawl" "firecrawl may be named without a key"
           "providers" (websearch-providers "brave" "b-key") "provider" "firecrawl")
    (picks "arxiv" "arxiv may be named without a key"
           "providers" (nlk:make-json-object) "provider" "arxiv")))

;;; --- search fixtures ----------------------------------------------------------------

(defun brave-body (&rest rows)
  "Brave's JSON for ROWS of (TITLE URL DESCRIPTION &optional PAGE-AGE)."
  (json-body "web" (nlk:make-json-object
                    "results" (coerce (loop for (title url description age) in rows
                                            collect (apply #'nlk:make-json-object
                                                           "title" title "url" url "description" description
                                                           (and age (list "page_age" age))))
                                      'vector))))

(defun exa-sse (text &aux (*print-pretty* nil))
  "The MCP endpoint's answer: one SSE message whose result carries TEXT, the
JSON on one line as the server sends it."
  (format nil "event: message~%data: ~a~%~%"
          (json-body "result" (nlk:make-json-object
                               "content" (vector (nlk:json-object "type" "text" "text" text))))))

(defun exa-blocks (&rest rows)
  "The MCP tool's text for ROWS of (TITLE URL HIGHLIGHTS &optional PUBLISHED)."
  (format nil "~{~a~^~%~%---~%~%~}"
          (loop for (title url highlights published) in rows
                collect (format nil "Title: ~a~%URL: ~a~%Published: ~a~%Author: N/A~%Highlights:~%~a"
                                title url (or published "N/A") highlights))))

(defun ddg-page (&rest rows)
  "The HTML endpoint's page: one result block per ROW of (TITLE HREF SNIPPET
&optional AD-P), the markup the live page has."
  (with-output-to-string (out)
    (format out "<html><body><div id=\"links\" class=\"results\">")
    (dolist (row rows)
      (destructuring-bind (title href snippet &optional ad) row
        (format out "<div class=\"result results_links results_links_deep ~:[~;result--ad ~]web-result \">~
<div class=\"links_main links_deep result__body\"><h2 class=\"result__title\">~
<a rel=\"nofollow\" class=\"result__a\" href=\"~a\">~a</a></h2>~
<div class=\"result__extras\"><div class=\"result__extras__url\"><a class=\"result__url\" href=\"~a\">host</a></div></div>~
<a class=\"result__snippet\" href=\"~a\">~a</a></div></div>"
                ad href title href href snippet)))
    (format out "</div></body></html>")))

(defun arxiv-feed (&rest rows)
  "arXiv's Atom answer for ROWS of (ID TITLE SUMMARY PUBLISHED), the shape
the API sends: a feed title the parser must not take for a paper's, then
one <entry> per paper."
  (with-output-to-string (out)
    (format out "<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>arXiv Query: search_query=all:q&amp;id_list=&amp;start=0</title>
  <opensearch:totalResults>~d</opensearch:totalResults>" (length rows))
    (loop for (id title summary published) in rows
          do (format out "
  <entry>
    <id>http://arxiv.org/abs/~a</id>
    <title>~a</title>
    <link href=\"https://arxiv.org/abs/~a\" rel=\"alternate\" type=\"text/html\"/>
    <summary>~a</summary>
    <published>~a</published>
    <author><name>A. Author</name></author>
  </entry>"
                     id title id summary published))
    (format out "~%</feed>")))

(defun firecrawl-body (&rest rows)
  "Firecrawl's search JSON for ROWS of (TITLE URL DESCRIPTION)."
  (json-body "success" t
             "data" (nlk:make-json-object
                     "web" (coerce (loop for (title url description) in rows
                                         for position from 1
                                         collect (nlk:json-object "title" title "url" url
                                                                  "description" description
                                                                  "position" position))
                                   'vector))))

(defun scrape-body (markdown &key (title "Scraped") (status 200))
  "Firecrawl's scrape JSON: MARKDOWN under data, TITLE and the page's own
STATUS under its metadata."
  (json-body "success" t
             "data" (nlk:make-json-object
                     "markdown" markdown
                     "metadata" (nlk:json-object "title" title "statusCode" status))))

(defun by-url (default &rest pairs)
  "A script answering by URL: the value after the first NEEDLE the call's
url contains, else DEFAULT."
  (lambda (call)
    (loop for (needle value) on pairs by #'cddr
          when (contains-p (http-call-url call) needle) return value
          finally (return default))))

(defun engine-body (&key (brave "") (arxiv "") (ddg (ddg-page)) (firecrawl (firecrawl-body))
                         (exa (exa-sse "nothing parseable")))
  "A body script answering each search engine by url, the floor empty unless named."
  (by-url "" "brave.com" brave "arxiv" arxiv "duckduckgo" ddg "firecrawl" firecrawl "exa.ai" exa))

(defun called (calls needle)
  "The calls in CALLS whose url contains NEEDLE."
  (remove-if-not (lambda (call) (contains-p (http-call-url call) needle)) calls))

;;; --- search: a keyed provider answers alone --------------------------------------------

(deftest websearch-cell-brave-request-and-render (with-websearch ())
  (with-scripted-http (calls :body (brave-body '("One" "https://a/1" "first snippet" "2026-09-01T10:00:00")
                                               '("Two" "https://a/2" "second snippet")
                                               '("Three" "https://a/3" "")))
    (let ((text (web:search "lisp sbcl" :n 3)))
      (is (= 1 (length calls)) "one call: the keyed provider answered, the floor never ran")
      (let ((call (first calls)))
        (is-shape call (http-call-method eq :get "GET")
          (http-call-url "https://api.search.brave.com/res/v1/web/search?q=lisp%20sbcl&count=3"))
        (is (equal "brave-key" (call-header call "x-subscription-token")) "brave header")
        (is (null (http-call-content call)) "no body on GET")
        (is (not (contains-p (http-call-url call) "brave-key")) "key never in the URL"))
      (is (equal (format nil "1. One — first snippet (2026-09-01)~%   https://a/1~%~%~
2. Two — second snippet~%   https://a/2~%~%3. Three~%   https://a/3")
                 text)))))

(deftest websearch-cell-exa-request-and-answer ()
  (with-websearch ("providers" (websearch-providers "brave" "b-key" "exa" "exa-key"))
    (with-scripted-http (calls :body (json-body
                                      "answer" "  Exa says so.  "
                                      "citations" (coerce (loop for i from 1 to 5
                                                                collect (nlk:json-object
                                                                         "title" (format nil "C~d" i)
                                                                         "url" (format nil "https://c/~d" i)
                                                                         "text" "t"
                                                                         "publishedDate" (format nil "2026-01-0~dT00:00:00.000Z" i)))
                                                          'vector)))
      (let ((text (web:search "q" :provider "exa" :n 2)))
        (let ((call (first calls)))
          (is (eq :post (http-call-method call)) "POST")
          (is (equal "https://api.exa.ai/answer" (http-call-url call)) "exa answer URL")
          (is (equal "exa-key" (call-header call "x-api-key")) "exa header")
          (is (equal "q" (nlk:json-value (shasht:read-json (http-call-content call)) :text "query"))))
        (is (= 1 (length calls)) "the named keyed provider answered alone")
        (is (uiop:string-prefix-p (format nil "Exa says so.~%~%1. C1 — t (2026-01-01)~%   https://c/1") text) "answer first, trimmed, then the dated hits")
        (is (not (contains-p text "C3")) "n caps the citations")))))

(deftest websearch-cell-tavily-request-and-answer ()
  (with-websearch ("providers" (websearch-providers "tavily" "tavily-key")
                   "provider" "tavily" "max_results" 4)
    (with-scripted-http (calls :body (json-body
                                      "answer" "Tavily answer"
                                      "results" (vector (nlk:json-object "title" "R" "url" "https://r/1"
                                                                         "content" "c"
                                                                         "published_date" "2025-12-31"))))
      (let ((text (web:search "q")))
        (let* ((call (first calls))
               (body (shasht:read-json (http-call-content call))))
          (is (equal "https://api.tavily.com/search" (http-call-url call)) "tavily URL")
          (is (equal "Bearer tavily-key" (call-header call "authorization")) "bearer header")
          (is (equal "q" (nlk:json-value body :text "query")) "query")
          (is (eql 4 (nlk:json-value body :integer "max_results")) "max_results from config")
          (is (equal "basic" (nlk:json-value body :text "include_answer")) "include_answer basic"))
        (is (equal (format nil "Tavily answer~%~%1. R — c (2025-12-31)~%   https://r/1") text) "answer, then the dated hit")))))

(deftest websearch-cell-render-clips-lines-not-pages (with-websearch ())
  (with-scripted-http (calls :body (brave-body (list "T" "https://a" (format nil "  a~%~%b~C~Cc   d " #\Tab #\Tab))))
    (is (contains-p (web:search "q") (format nil "1. T — a b c d~%   https://a")) "whitespace collapsed to one line"))
  (with-scripted-http (calls :body (apply #'brave-body
                                          (loop for i below 10
                                                collect (list (make-string 200 :initial-element #\t)
                                                              (format nil "https://a/~d" i)
                                                              (make-string 3000 :initial-element #\s)))))
    (let ((text (web:search "q" :n 10)))
      (is (contains-p text (format nil "~%10. ")) "ten hits rendered")
      (is (contains-p text (format nil "~a…~%" (make-string 160 :initial-element #\s))) "a snippet is cut at 160")
      (is (contains-p text (format nil "~a… — " (make-string 90 :initial-element #\t))) "a title is cut at 90")
      (is (<= (length text) 3000) "ten long hits fit the answer cap")
      (is (not (contains-p text "truncated")) "nothing clipped after the fact"))))

(deftest websearch-cell-n-is-clamped-and-defaults (with-websearch ("max_results" 7))
  (with-scripted-http (calls :body (brave-body '("T" "https://a" "s")))
    (web:search "q" :n 50)
    (web:search "q")
    (web:search "q" :n 0)
    (is (contains-p (http-call-url (first calls)) "count=10") "50 clamps to 10")
    (is (contains-p (http-call-url (second calls)) "count=7") "absent n reads max_results")
    (is (contains-p (http-call-url (third calls)) "count=1") "0 clamps to 1")))

;;; --- search: the chain advances -------------------------------------------------------

(deftest websearch-cell-keyed-failure-advances-to-the-floor (with-websearch ())
  (with-scripted-http (calls :status (by-url 200 "brave.com" 401)
                             :body (engine-body :brave "invalid token brave-key"
                                                :ddg (ddg-page '("D" "https://d/" "from duckduckgo"))
                                                :firecrawl (firecrawl-body '("F" "https://f/" "from firecrawl"))
                                                :exa (exa-sse (exa-blocks '("E" "https://e/" "from exa")))))
    (let ((text (web:search "q")))
      (is (equal '("brave.com" "duckduckgo" "firecrawl" "exa.ai")
                 (loop for call in calls
                       collect (find-if (lambda (needle) (contains-p (http-call-url call) needle))
                                        '("brave.com" "duckduckgo" "firecrawl" "exa.ai")))))
      (is (uiop:string-prefix-p "[brave: HTTP 401: invalid token [redacted]]" text) "the failure is a note, redacted")
      (is (not (contains-p text "brave-key")) "the key is absent")
      (is (contains-p text (format nil "1. D — from duckduckgo~%   https://d/")) "the floor answered")
      (is (contains-p text (format nil "2. F — from firecrawl~%   https://f/")))
      (is (contains-p text (format nil "3. E — from exa~%   https://e/")))))
  (with-scripted-http (calls :body (engine-body :brave (brave-body)
                                                :ddg (ddg-page '("D" "https://d/" "d"))
                                                :exa (exa-sse (exa-blocks '("E" "https://e/" "e")))))
    (let ((text (web:search "q")))
      (is (uiop:string-prefix-p "[brave: no results]" text) "an empty keyed answer advances too")
      (is (contains-p text "1. D") "and the floor answers")))
  (with-scripted-http (calls :body (engine-body :brave "<html>oops</html>"
                                                :ddg (ddg-page '("D" "https://d/" "d"))
                                                :exa (exa-sse (exa-blocks '("E" "https://e/" "e")))))
    (is (uiop:string-prefix-p "[brave: answered with something that is not JSON]" (web:search "q")))))

(deftest websearch-cell-every-engine-failing-is-one-error (with-websearch ())
  (with-scripted-http (calls :status 500 :body "down brave-key")
    (let ((condition (signals-error web:websearch-error (web:search "q"))))
      (is (= 4 (length calls)) "brave, duckduckgo, firecrawl and exa were all asked")
      (is (contains-p (princ-to-string condition) "no engine answered \"q\"") "one error")
      (is (contains-p (princ-to-string condition) "brave: HTTP 500") "names each failure")
      (is (contains-p (princ-to-string condition) "duckduckgo: HTTP 500"))
      (is (contains-p (princ-to-string condition) "firecrawl: HTTP 500"))
      (is (contains-p (princ-to-string condition) "exa: HTTP 500"))
      (is (contains-p (princ-to-string condition) "[redacted]") "key redacted")
      (is (not (contains-p (princ-to-string condition) "brave-key")) "key absent")))
  (with-stubbed-fdefinition (nodecode-websearch::http (method url &key headers content timeout max-bytes)
                             (error "dns failed while sending brave-key"))
    (let ((condition (signals-error web:websearch-error (web:search "q"))))
      (is (and condition (not (contains-p (princ-to-string condition) "brave-key"))))
      (is (contains-p (princ-to-string condition) "dns failed") "and keeps its text"))))

(deftest websearch-cell-cancel-passes-through (with-websearch ())
  (with-stubbed-fdefinition (nodecode-websearch::http (method url &key headers content timeout max-bytes)
                             (error 'nlk:turn-cancelled-condition))
    (is (signals-error nlk:turn-cancelled-condition (web:search "q")))))

(deftest websearch-cell-unknown-provider-is-an-error-unkeyed-is-a-note (with-websearch ())
  (with-scripted-http (calls :body (engine-body :brave (brave-body '("B" "https://b/" "b"))
                                                :ddg (ddg-page '("D" "https://d/" "d"))
                                                :firecrawl (firecrawl-body '("F" "https://f/" "f"))
                                                :exa (exa-sse (exa-blocks '("E" "https://e/" "e")))))
    (let ((bing (signals-error web:websearch-error (web:search "q" :provider "bing"))))
      (is (contains-p (princ-to-string bing) "brave, exa, tavily, duckduckgo, firecrawl, arxiv") "unknown names the known")
      (is (null calls) "no call was made"))
    (let ((text (web:search "q" :provider "tavily")))
      (is (equal (format nil "[tavily has no api_key (websearch.providers.tavily.api_key); answered without it]~%~%1. B — b~%   https://b/")
                 text))
      (is (= 1 (length calls)) "brave alone was asked"))
    (let ((text (web:search "q" :provider "duckduckgo")))
      (is (equal (format nil "1. D — d~%   https://d/~%~%2. F — f~%   https://f/~%~%3. E — e~%   https://e/") text))
      (is (= 1 (length (called calls "brave.com"))) "brave was not asked again"))
    (let ((text (web:search "q" :provider "firecrawl")))
      (is (uiop:string-prefix-p "1. F — f" text) "the named floor engine leads the tiebreak")
      (is (contains-p (http-call-url (car (last calls 3))) "firecrawl") "and is asked first"))))

;;; --- search: arxiv ---------------------------------------------------------------------

(deftest websearch-cell-arxiv-request-and-parse (with-websearch (:keyless))
  (with-scripted-http (calls :body (arxiv-feed '("2203.16487v6"
                                                 "Speculative Decoding: Exploiting Speculative Execution"
                                                 "We propose Speculative Decoding (SpecDec) &amp; more."
                                                 "2022-03-30T17:27:09Z")
                                              '("2605.01106v1" "A Second Paper" "Another abstract."
                                                "2026-05-01T09:00:00Z")))
    (let ((text (web:search "speculative decoding" :provider "arxiv")))
      (is (= 1 (length calls)) "one request; no floor engine is asked")
      (let ((url (http-call-url (first calls))))
        (is (eq :get (http-call-method (first calls))) "GET")
        (is (uiop:string-prefix-p "https://export.arxiv.org/api/query?search_query=" url) "the Atom endpoint")
        (is (contains-p url "all%3Aspeculative%20AND%20all%3Adecoding") "a plain query ANDs its terms")
        (is (contains-p url "max_results=5") "the default n"))
      (is (equal (format nil "1. Speculative Decoding: Exploiting Speculative Execution — We propose Speculative Decoding (SpecDec) & more. (2022-03-30)~%   https://arxiv.org/abs/2203.16487v6~%~%2. A Second Paper — Another abstract. (2026-05-01)~%   https://arxiv.org/abs/2605.01106v1")
                 text)))))

(deftest websearch-cell-arxiv-query-shapes ()
  (flet ((shaped (query) (nodecode-websearch::arxiv-query query)))
    (is (equal "all:speculative AND all:decoding" (shaped "speculative decoding")) "a plain query ANDs its terms")
    (is (equal "ti:transformer" (shaped "  ti:transformer ")) "arXiv's own field syntax is verbatim")
    (is (equal "all:diffusion" (shaped "diffusion")) "one term needs no AND")))

(deftest websearch-cell-arxiv-failure-is-a-note-then-the-floor (with-websearch (:keyless))
  (with-scripted-http (calls :status (by-url 200 "arxiv" 503)
                             :body (engine-body :arxiv "rate limited"
                                                :ddg (ddg-page '("D" "https://d/" "d"))
                                                :firecrawl (firecrawl-body '("F" "https://f/" "f"))))
    (let ((text (web:search "q" :provider "arxiv")))
      (is (uiop:string-prefix-p "[arxiv: HTTP 503: rate limited]" text) "the failure is a note")
      (is (contains-p text "1. D — d") "the floor still answered"))))

;;; --- search: the public floor ------------------------------------------------------------

(deftest websearch-cell-duckduckgo-request-and-parse (with-websearch (:keyless))
  (with-scripted-http (calls :body (engine-body :ddg (ddg-page
                                                      '("Understanding the <b>Firefly</b> clock sync"
                                                        "//duckduckgo.com/l/?uddg=https%3A%2F%2Fcloud.google.com%2Fblog%2Ffirefly&amp;rut=abc"
                                                        "Firefly is a <b>clock</b> synchronization &amp; system.")
                                                      '("Sponsored" "https://ads.example/" "buy now" t)
                                                      '("Adobe Firefly" "https://www.adobe.com/products/firefly.html" "Create and edit images.")
                                                      '("Dup" "https://www.adobe.com/products/firefly.html" "again")
                                                      '("Relative" "/no-scheme" "skipped"))))
    (let ((text (web:search "Firefly Google DeepMind" :n 8)))
      (let ((call (first (called calls "duckduckgo"))))
        (is (eq :post (http-call-method call)) "POST")
        (is (equal "https://html.duckduckgo.com/html/" (http-call-url call)) "the HTML endpoint")
        (is (equal "q=Firefly%20Google%20DeepMind&kl=us-en" (http-call-content call)) "the form")
        (is (equal "application/x-www-form-urlencoded" (call-header call "content-type")))
        (is (equal nodecode-websearch::+user-agent+ (call-header call "user-agent")) "browser UA")
        (is (equal "navigate" (call-header call "sec-fetch-mode")) "the fetch-metadata set")
        (is (equal "en-US,en;q=0.9" (call-header call "accept-language")))
        (is (equal "https://html.duckduckgo.com/" (call-header call "referer"))))
      (is (equal (format nil "1. Understanding the Firefly clock sync — Firefly is a clock synchronization & system.~%   https://cloud.google.com/blog/firefly~%~%~
2. Adobe Firefly — Create and edit images.~%   https://www.adobe.com/products/firefly.html")
                 text))))
  (with-scripted-http (calls :status (by-url 200 "duckduckgo" 202)
                             :body (engine-body :ddg "<html>anomaly</html>"
                                                :exa (exa-sse (exa-blocks '("E" "https://e/" "e")))))
    (let ((text (web:search "q")))
      (is (uiop:string-prefix-p "[duckduckgo: HTTP 202, its bot challenge]" text) "the challenge page is a named failure")
      (is (contains-p text "1. E — e") "exa still answers"))))

(deftest websearch-cell-firecrawl-request-and-parse ()
  (with-websearch (:keyless)
    (with-scripted-http (calls :body (engine-body :firecrawl (firecrawl-body '("One" "https://one/" "first")
                                                                             '("Two" "https://two/" "second"))))
      (let ((text (web:search "q" :n 5)))
        (let* ((call (first (called calls "firecrawl")))
               (body (shasht:read-json (http-call-content call))))
          (is (eq :post (http-call-method call)) "POST")
          (is (equal "https://api.firecrawl.dev/v2/search" (http-call-url call)) "the search endpoint")
          (is (null (call-header call "authorization")) "keyless")
          (is (equal "q" (nlk:json-value body :text "query")) "query")
          (is (eql 10 (nlk:json-value body :integer "limit")) "the floor's window of 10")
          (is (equal "web" (nlk:json-value (aref (nlk:json-value body :array "sources") 0) :text "type")) "web sources"))
        (is (equal (format nil "1. One — first~%   https://one/~%~%2. Two — second~%   https://two/") text))))
    (with-scripted-http (calls :body (engine-body :ddg (ddg-page '("D" "https://d/" "d"))
                                                  :firecrawl (json-body "error" "rate limited")))
      (is (uiop:string-prefix-p "[firecrawl: rate limited]" (web:search "q")) "a refused request is a note")))
  (with-websearch ("providers" (websearch-providers "firecrawl" "fc-key"))
    (with-scripted-http (calls :body (engine-body))
      (web:search "q")
      (is (equal "Bearer fc-key" (call-header (first (called calls "firecrawl")) "authorization")))
      (is (= 3 (length calls)) "the floor still runs all three"))))

(deftest websearch-cell-exa-keyless-request-and-blocks (with-websearch (:keyless))
  (with-scripted-http (calls :body (engine-body :exa (exa-sse (exa-blocks
                                                               (list "AP News" "https://apnews.com/" (format nil "Tariffs kicked in Saturday.~%...~%Canada said it would retaliate.") "2026-02-24T00:00:00.000Z")
                                                               '("BBC Home" "https://www.bbc.com/" "Carney calls the tariffs a miscalculation.")
                                                               '("Third" "https://third/" "x")))))
    (let ((text (web:search "top news" :n 2)))
      (let* ((call (first (called calls "exa.ai")))
             (body (shasht:read-json (http-call-content call))))
        (is (eq :post (http-call-method call)) "POST")
        (is (equal "https://mcp.exa.ai/mcp?tools=web_search_exa" (http-call-url call)) "the MCP URL")
        (is (null (call-header call "x-api-key")) "no key header")
        (is (contains-p (call-header call "accept") "text/event-stream") "accepts SSE")
        (is (equal "tools/call" (nlk:json-value body :text "method")) "JSON-RPC tools/call")
        (is (equal "web_search_exa" (nlk:json-value body :text "params" "name")) "the tool")
        (is (equal "top news" (nlk:json-value body :text "params" "arguments" "query")) "query")
        (is (equal "top news" (nlk:json-value body :text "params" "arguments" "objective")) "objective, which the tool requires")
        (is (eql 10 (nlk:json-value body :integer "params" "arguments" "numResults")) "the floor's window of 10, whatever n; the merge takes n"))
      (is (equal (format nil "1. AP News — Tariffs kicked in Saturday. Canada said it would retaliate. (2026-02-24)~%   https://apnews.com/~%~%~
2. BBC Home — Carney calls the tariffs a miscalculation.~%   https://www.bbc.com/")
                 text))))
  (with-scripted-http (calls :body (engine-body
                                     :exa (exa-sse (json-body "results" (vector (nlk:json-object "title" "J" "url" "https://j/" "text" "long text"
                                                                                                 "highlights" (vector "h1" "h2")
                                                                                                 "publishedDate" "2026-03-01T00:00:00.000Z"))))))
    (is (contains-p (web:search "q") (format nil "1. J — h1 h2 (2026-03-01)~%   https://j/")) "raw Exa JSON inside the tool text"))
  (with-scripted-http (calls :body (engine-body :ddg (ddg-page '("D" "https://d/" "d"))
                                                :exa (json-body "jsonrpc" "2.0" "id" 1
                                                                "error" (nlk:json-object "code" -32600 "message" "bad request"))))
    (is (uiop:string-prefix-p "[exa: bad request]" (web:search "q")) "a JSON-RPC error is a note when duckduckgo answered"))
  (with-scripted-http (calls :body (engine-body :exa (json-body "result" (nlk:make-json-object
                                                                          "isError" t
                                                                          "content" (vector (nlk:json-object "type" "text" "text" "tool blew up"))))))
    (is (equal (format nil "[exa: tool blew up]~%~%No results for \"q\".") (web:search "q")))))

(deftest websearch-cell-floor-merges-by-consensus (with-websearch (:keyless))
  (let ((floor (engine-body :ddg (ddg-page '("A" "https://a.example/page" "a from ddg")
                                           '("B" "https://b.example/" "b")
                                           '("C" "https://C.example/doc/" "c short"))
                            :firecrawl (firecrawl-body '("C" "https://c.example/doc" "c from firecrawl")
                                                       '("B" "https://b.example" "b again"))
                            :exa (exa-sse (exa-blocks
                                           '("C" "https://c.example/doc?utm_source=x#top" "c from exa, the longer snippet" "2026-05-05T00:00:00.000Z")
                                           '("D" "https://d.example/" "d")
                                           '("A" "https://a.example/page/" "a"))))))
    (with-scripted-http (calls :body floor)
      (let ((text (web:search "q" :n 10)))
        (is (= 3 (length calls)) "the three floor engines asked")
        (is (notany (lambda (call) (contains-p call.url "arxiv")) calls))
        (is (equal '("C" "A" "B" "D")
                   (loop for line in (nlk:lines text)
                         for title = (cl-ppcre:register-groups-bind (title) ("^\\d+\\. (\\S+)" line) title)
                         when title collect title)))
        (is (contains-p text "C — c from exa, the longer snippet (2026-05-05)") "the longest snippet and the date survive the merge")
        (is (contains-p text (format nil "~%   https://C.example/doc/") ) "the first spelling of the url is the one shown")))
    (with-scripted-http (calls :body floor)
      (is (contains-p (web:search "q" :n 2) (format nil "2. A")) "n caps the merge")
      (is (not (contains-p (web:search "q" :n 2) "3. ")))
      (web:search "q" :n 2 :provider "exa")
      (is (contains-p (http-call-url (car (last calls 3))) "exa.ai") "naming exa asks it first"))))

(deftest websearch-cell-no-results-and-bad-query (with-websearch (:keyless))
  (with-scripted-http (calls :body (engine-body))
    (is (equal "No results for \"q\"." (web:search "q")) "every floor engine empty"))
  (with-scripted-http (calls :body (ddg-page))
    (let ((bad (signals-error web:websearch-error (web:search ""))))
      (is (contains-p (princ-to-string bad) "non-empty") "empty query refused")
      (is (null calls) "before any call"))))

;;; --- extractor -------------------------------------------------------------------

(defun extracted (html)
  (nodecode-websearch::extract-text html))

(deftest websearch-cell-extract-prefers-main-then-article-then-body ()
  (is-each (extracted)
    ("<html><body><nav>menu</nav><main><p>inside main</p></main><footer>f</footer></body></html>"
     "inside main" "main wins over the rest of body")
    ("<body><div>nav</div><ARTICLE class=x><p>the article</p></ARTICLE></body>" "the article"
     "article when no main, case-insensitive")
    ("<html><head><title>t</title></head><body>just body</body></html>"
     "just body" "body when neither")
    ("<p>bare text</p>" "bare text" "the whole thing without a body")))

(deftest websearch-cell-extract-drops-silent-elements ()
  (let ((text (extracted (format nil "<html><head><title>T</title><meta x=y><style>p{color:red}</style></head>~
<body><script>var a = 'script';</script><SCRIPT TYPE=\"module\">~%two~%lines~%</SCRIPT>~
<noscript>enable js</noscript><svg><text>vector</text></svg><!-- a comment -->kept</body></html>"))))
    (is (equal "kept" text) "only the prose survives")))

(deftest websearch-cell-extract-decodes-entities ()
  (is (equal "& < > \" ' © © &zzz;"
             (extracted "&amp; &lt; &gt; &quot; &#39; &nbsp; &#169; &#xA9; &zzz;"))))

(deftest websearch-cell-extract-renders-absolute-links-only ()
  (is (equal "Doc (https://x/y)" (extracted "<a href=\"https://x/y\">Doc</a>")) "absolute link")
  (is (equal "Rel" (extracted "<a href='/rel'>Rel</a>")) "relative: text only")
  (is (equal "M" (extracted "<a href=\"mailto:a@b\">M</a>")) "mailto: text only")
  (is (equal "Bold link (http://h/)" (extracted "<a class=c href=http://h/ id=i><b>Bold</b> link</a>")))
  (is (equal "before after" (extracted "before <a href=\"https://h/\"><img src=logo.png></a> after"))))

(deftest websearch-cell-extract-block-breaks-and-collapse ()
  (is (equal (format nil "a~%b") (extracted "<p>a</p><p>b</p>")) "paragraphs break")
  (is (equal (format nil "x~%y") (extracted "x<br>y")) "br breaks")
  (is (equal "a b" (extracted "<table><tr><td>a</td><td>b</td></tr></table>")) "cells join on one line")
  (is (equal (format nil "one~%~%two")
             (extracted (format nil "  one~%~%~%~%~%<p>two</p>   "))))
  (is (equal "bellgone" (extracted (format nil "bell~Cgone" (code-char 7)))) "controls stripped")
  (is (equal "M." (extracted "<b>M</b>.")) "inline tags leave no space"))

(deftest websearch-cell-extract-title ()
  (is (equal "A & B" (nth-value 1 (extracted "<title> A &amp; B </title>body"))) "decoded, trimmed")
  (is (equal "Split" (nth-value 1 (extracted (format nil "<title~%  >Split</title~%  >body")))) "newlines inside the tags")
  (is (null (nth-value 1 (extracted "<p>no title</p>"))) "absent is NIL"))

(defun js-shell (chars scripts)
  (format nil "<html><body>~a~{~a~}<div id=app></div></body></html>"
          (make-string chars :initial-element #\x)
          (loop repeat scripts collect "<script src=a.js></script>")))

(deftest websearch-cell-extract-flags-a-js-shell ()
  (is (nth-value 2 (extracted (js-shell 300 4))) "little text, many scripts: a shell")
  (is (not (nth-value 2 (extracted (js-shell 300 2)))) "two scripts: not a shell")
  (is (not (nth-value 2 (extracted (js-shell 800 6)))) "enough text: not a shell")
  (is (equal (make-string 300 :initial-element #\x) (extracted (js-shell 300 4))) "the text is the text, no note inside it"))

;;; --- fetch ------------------------------------------------------------------------

(defmacro with-chrome-loaded (&body body)
  "Run BODY with a package standing in for the chrome cell's, deleted on
unwind: the fetch hint keys on the package alone."
  `(let ((package (make-package "NODECODE-CHROME" :use '())))
     (unwind-protect (progn ,@body)
       (delete-package package))))

(defun page-headers (needle type)
  "Response headers by url: TYPE for the page at NEEDLE, JSON for the
hosted readers."
  (by-url (response-headers "content-type" "application/json")
          needle (response-headers "content-type" type)))

(deftest websearch-cell-fetch-html-answers-title-url-text (with-websearch ())
  (with-scripted-http (calls :text "<html><head><title>Example Page</title></head><body><main><h1>Hello</h1><p>World <a href=\"https://x/\">X</a></p></main></body></html>"
                             :type "text/html; charset=utf-8")
    (let ((text (web:fetch "https://example.com/x")))
      (is (equal (format nil "Example Page~%https://example.com/x~%~%Hello~%World X (https://x/)") text))
      (let ((call (first calls)))
        (is (eq :get (http-call-method call)) "GET")
        (is (equal "https://example.com/x" (http-call-url call)) "the URL as given")
        (is (equal nodecode-websearch::+user-agent+ (call-header call "user-agent")) "browser UA")
        (is (equal "navigate" (call-header call "sec-fetch-mode")) "the fetch-metadata set rides on every page request")
        (is (equal "1" (call-header call "upgrade-insecure-requests")))
        (is (contains-p (call-header call "accept") "application/pdf") "a PDF is welcome")
        (is (eql 30 (http-call-timeout call)) "30 s")
        (is (eql 5242880 (funcall (http-call-max-bytes call) (response-headers "content-type" "text/html"))))
        (is (eql 33554432 (funcall (http-call-max-bytes call) (response-headers "content-type" "application/pdf")))))
      (is (= 1 (length calls)) "the local reader answered; no hosted reader was asked"))))

(deftest websearch-cell-fetch-honours-config-limits ()
  (with-websearch ("fetch" (nlk:json-object "max_bytes" 4096 "limit" 400))
    (with-scripted-http (calls :text (make-string 900 :initial-element #\a) :type "text/plain")
      (let ((text (web:fetch "https://example.com/t")))
        (is (eql 4096 (funcall (http-call-max-bytes (first calls)) (response-headers "content-type" "text/plain"))))
        (is (eql 33554432 (funcall (http-call-max-bytes (first calls)) (response-headers "content-type" "application/pdf"))))
        (is (contains-p text "Showing 0-400 of 900 chars") "limit from config")))))

(deftest websearch-cell-fetch-text-is-verbatim-and-sniffs (with-websearch ())
  (with-scripted-http (calls :text "keep <b>this</b> & that" :type "text/plain")
    (is (contains-p (web:fetch "https://e/plain") (format nil "(text)~%https://e/plain~%~%keep <b>this</b> & that"))))
  (with-scripted-http (calls :text "{\"a\": 1}" :type "application/json")
    (is (contains-p (web:fetch "https://e/json") "{\"a\": 1}") "json verbatim"))
  (with-scripted-http (calls :text "<!DOCTYPE html><html><body><p>sniffed</p></body></html>"
                             :headers (response-headers))
    (is (contains-p (web:fetch "https://e/nocontenttype") (format nil "~%~%sniffed")) "no content-type: sniffed as HTML"))
  (with-scripted-http (calls :text "hello" :headers (response-headers))
    (is (contains-p (web:fetch "https://e/nocontenttype2") (format nil "~%~%hello")) "no content-type, not HTML: verbatim")))

(deftest websearch-cell-fetch-reads-to-the-cap-and-a-missing-page-is-missing (with-websearch ())
  (with-scripted-http (calls :text "<html><body><p>the first five megabytes</p></body></html>" :truncated t :type "text/html")
    (let ((text (web:fetch "https://e/big")))
      (is (contains-p text "the first five megabytes") "a page past the cap is read to the cap")
      (is (uiop:string-suffix-p text "[cut at 5,242,880 bytes (fetch.max_bytes): the page is longer]") "and says so")
      (is (= 1 (length calls)) "no other reader was asked")))
  (with-scripted-http (calls :status 404 :text "nope" :type "text/html")
    (let ((text (refusal-text web:websearch-error (web:fetch "https://e/missing"))))
      (is (equal "HTTP 404 fetching https://e/missing" text) "a missing page is missing")
      (is (= 1 (length calls)) "no reader is asked twice for it"))))

(deftest websearch-cell-fetch-escalates-a-gated-page (with-websearch ())
  (with-scripted-http (calls :status (by-url 200 "e/gated" 403)
                             :body (by-url "" "e/gated" (string-octets "forbidden")
                                              "firecrawl" (scrape-body (format nil "# Gated~%~%the page, rendered") :title "Gated Page"))
                             :headers (page-headers "e/gated" "text/html"))
    (let ((text (web:fetch "https://e/gated")))
      (is (equal (format nil "Gated Page~%https://e/gated~%[http: HTTP 403; read by firecrawl]~%~%# Gated~%~%the page, rendered") text))
      (is (= 2 (length calls)) "http, then firecrawl")
      (let ((scrape (second calls)))
        (is (eq :post (http-call-method scrape)) "POST")
        (is (equal "https://api.firecrawl.dev/v2/scrape" (http-call-url scrape)) "the scrape endpoint")
        (is (null (call-header scrape "authorization")) "keyless")
        (let ((body (shasht:read-json (http-call-content scrape))))
          (is (equal "https://e/gated" (nlk:json-value body :text "url")) "the url")
          (is (equalp #("markdown") (nlk:json-value body :array "formats")) "markdown")))))
  (with-scripted-http (calls :status (by-url 200 "e/gated2" 403)
                             :body (by-url "" "e/gated2" (string-octets "forbidden")
                                              "firecrawl" (scrape-body "" :status 404)
                                              "exa.ai" (exa-sse (format nil "# From Exa~%URL: https://e/gated2~%Published: 2026-01-01~%~%the exa text~%more")))
                             :headers (page-headers "e/gated2" "text/html"))
    (let ((text (web:fetch "https://e/gated2")))
      (is (equal (format nil "From Exa~%https://e/gated2~%[http: HTTP 403]~%[firecrawl: HTTP 404 at the page; read by exa]~%~%the exa text~%more") text))
      (let* ((call (third calls))
             (body (shasht:read-json (http-call-content call))))
        (is (equal "https://mcp.exa.ai/mcp?tools=web_fetch_exa" (http-call-url call)) "the fetch tool")
        (is (equal "web_fetch_exa" (nlk:json-value body :text "params" "name")))
        (is (equalp #("https://e/gated2") (nlk:json-value body :array "params" "arguments" "urls")) "the url")
        (is (eql 60000 (nlk:json-value body :integer "params" "arguments" "maxCharacters")) "bounded"))))
  (with-chrome-loaded
    (with-scripted-http (calls :status (by-url 500 "e/gated3" 403)
                               :body (by-url "down" "e/gated3" (string-octets "forbidden"))
                               :headers (page-headers "e/gated3" "text/html"))
      (is (equal "no reader could read https://e/gated3: http: HTTP 403; firecrawl: HTTP 500: down; exa: HTTP 500: down; the chrome cell reads it as a browser: (chrome:navigate url) then (chrome:snapshot)"
                 (refusal-text web:websearch-error (web:fetch "https://e/gated3")))))))

(deftest websearch-cell-fetch-js-shell-escalates-or-keeps-its-text (with-websearch ())
  (with-scripted-http (calls :body (by-url "" "e/app" (string-octets (js-shell 300 4))
                                              "firecrawl" (scrape-body "rendered text" :title "App"))
                             :headers (page-headers "e/app" "text/html"))
    (is (equal (format nil "App~%https://e/app~%[http: JavaScript-rendered, little text reached here; read by firecrawl]~%~%rendered text")
               (web:fetch "https://e/app"))))
  (with-scripted-http (calls :status (by-url 200 "firecrawl" 500 "exa.ai" 500)
                             :body (by-url "down" "e/app2" (string-octets (js-shell 300 4)))
                             :headers (page-headers "e/app2" "text/html"))
    (let ((text (web:fetch "https://e/app2")))
      (is (contains-p text (format nil "[http: JavaScript-rendered, little text reached here]~%[firecrawl: HTTP 500: down]~%[exa: HTTP 500: down]~%[the local reader's text is what there is]~%~%~a"
                                   (make-string 300 :initial-element #\x))))
      (is (not (contains-p text "jina")) "nothing points at the third party")))
  (with-chrome-loaded
    (with-scripted-http (calls :status (by-url 200 "firecrawl" 500 "exa.ai" 500)
                               :body (by-url "down" "e/app3" (string-octets (js-shell 300 4)))
                               :headers (page-headers "e/app3" "text/html"))
      (is (contains-p (web:fetch "https://e/app3") "[the local reader's text is what there is; the chrome cell reads it as a browser: (chrome:navigate url) then (chrome:snapshot)]")))))

(deftest websearch-cell-fetch-converts-a-pdf-or-escalates (with-websearch ())
  (with-stubbed-fdefinition (nodecode-websearch::pdf-converter (path) (list "cat" path))
    (with-scripted-http (calls :text "%PDF-1.4 hello pdf text" :type "application/pdf")
      (let ((text (web:fetch "https://e/paper.pdf")))
        (is (uiop:string-prefix-p (format nil "(pdf)~%https://e/paper.pdf~%~%") text) "a pdf page, read here")
        (is (contains-p text "hello pdf text") "converted through the converter this box has")
        (is (= 1 (length calls)) "no hosted reader asked")))
    (with-scripted-http (calls :text "%PDF-1.4 sniffed pdf" :type "application/octet-stream")
      (is (contains-p (web:fetch "https://e/blob") "sniffed pdf") "octet-stream with a PDF head converts too")))
  (with-stubbed-fdefinition (nodecode-websearch::pdf-converter (path) (and path nil))
    (with-scripted-http (calls :body (by-url "" "e/none.pdf" (string-octets "%PDF-1.4 x")
                                                "firecrawl" (scrape-body "the paper, parsed" :title "Paper"))
                               :headers (page-headers "e/none.pdf" "application/pdf"))
      (is (equal (format nil "Paper~%https://e/none.pdf~%[http: a PDF, no PDF converter on this box (poppler's pdftotext, or python's pypdf); read by firecrawl]~%~%the paper, parsed")
                 (web:fetch "https://e/none.pdf")))))
  (with-stubbed-fdefinition (nodecode-websearch::pdf-converter (path) (and path (list "true")))
    (with-scripted-http (calls :status (by-url 200 "firecrawl" 500 "exa.ai" 500)
                               :body (by-url "down" "e/huge.pdf" (string-octets "%PDF-1.4 x"))
                               :headers (page-headers "e/huge.pdf" "application/pdf")
                               :truncated (by-url nil "e/huge.pdf" t))
      (is (contains-p (refusal-text web:websearch-error (web:fetch "https://e/huge.pdf"))
                      "http: a PDF, true produced no text, and only its first 33,554,432 bytes came; firecrawl: HTTP 500: down; exa: HTTP 500: down")))))

(deftest websearch-cell-fetch-a-document-goes-to-firecrawl (with-websearch ())
  (with-scripted-http (calls :body (by-url "" "e/report.docx" (string-octets "PK...")
                                              "firecrawl" (scrape-body "the report" :title "Report"))
                             :headers (page-headers "e/report.docx" "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
    (is (equal (format nil "Report~%https://e/report.docx~%[http: application/vnd.openxmlformats-officedocument.wordprocessingml.document, not text; read by firecrawl]~%~%the report")
               (web:fetch "https://e/report.docx"))))
  (with-scripted-http (calls :status (by-url 200 "firecrawl" 500 "exa.ai" 500)
                             :body (by-url "down" "e/img.png" (string-octets "PNG..."))
                             :headers (page-headers "e/img.png" "image/png"))
    (is (contains-p (refusal-text web:websearch-error (web:fetch "https://e/img.png"))
                    "http: image/png, not text; firecrawl: HTTP 500: down; exa: HTTP 500: down"))))

(deftest websearch-cell-fetch-named-readers-read-alone ()
  (with-websearch ()
    (with-scripted-http (calls :body (scrape-body "scraped alone" :title "Alone"))
      (let ((text (web:fetch "https://e/x" :reader "firecrawl")))
        (is (equal (format nil "Alone~%https://e/x~%~%scraped alone") text) "no notes when the named reader answers")
        (is (= 1 (length calls)) "one call")
        (is (contains-p (http-call-url (first calls)) "firecrawl") "firecrawl alone")))
    (with-scripted-http (calls :body (exa-sse (format nil "# Exa Alone~%URL: https://e/y~%~%exa alone")))
      (is (equal (format nil "Exa Alone~%https://e/y~%~%exa alone") (web:fetch "https://e/y" :reader "exa")) "exa alone")
      (is (= 1 (length calls))))
    (with-scripted-http (calls :status 500 :body "down")
      (is (equal "no reader could read https://e/z: firecrawl: HTTP 500: down"
                 (refusal-text web:websearch-error (web:fetch "https://e/z" :reader "firecrawl"))))))
  (with-websearch ("providers" (websearch-providers "firecrawl" "fc-key"))
    (with-scripted-http (calls :body (scrape-body "keyed"))
      (web:fetch "https://e/k" :reader "firecrawl")
      (is (equal "Bearer fc-key" (call-header (first calls) "authorization")) "a firecrawl key rides as a bearer"))))

(deftest websearch-cell-fetch-rejects-non-http-urls (with-websearch ())
  (with-scripted-http (calls :text "x")
    (signals-error web:websearch-error (web:fetch "ftp://e/x"))
    (signals-error web:websearch-error (web:fetch "file:///etc/passwd"))
    (signals-error web:websearch-error (web:fetch 42))
    (signals-error web:websearch-error (web:fetch "https://e/x" :reader "curl"))
    (signals-error web:websearch-error (web:fetch "https://e/x" :offset -1))
    (is (null calls) "nothing reached the network")))

(deftest websearch-cell-fetch-slices-from-the-cache (with-websearch ())
  (with-scripted-http (calls :text (make-string 900 :initial-element #\z) :type "text/plain")
    (let ((first (web:fetch "https://e/long" :limit 300))
          (second (web:fetch "https://e/long" :limit 300 :offset 300))
          (third (web:fetch "https://e/long" :limit 300 :offset 600)))
      (is (= 1 (length calls)) "one download for three slices")
      (is (uiop:string-suffix-p first "Showing 0-300 of 900 chars; (web:fetch \"https://e/long\" :offset 300) for the next slice"))
      (is (uiop:string-suffix-p second "(web:fetch \"https://e/long\" :offset 600) for the next slice") "second trailer")
      (is (not (contains-p third "Showing")) "last slice has no trailer")
      (is (= 300 (count #\z third)) "last slice is the tail")
      (is (contains-p (refusal-text web:websearch-error (web:fetch "https://e/long" :offset 900))
                      "past the end")))
    (let ((text (web:fetch "https://e/long" :limit 20000)))
      (is (and (not (contains-p text "Showing")) (= 900 (count #\z text))))
      (is (= 1 (length calls)) "still cached")))
  (with-scripted-http (calls :text (make-string 9000 :initial-element #\y) :type "text/plain")
    (is (contains-p (web:fetch "https://e/huge" :limit 20000) "Showing 0-7000 of 9,000 chars"))))

(deftest websearch-cell-fetch-cache-expires-and-evicts (with-websearch ())
  (with-scripted-http (calls :text "page" :type "text/plain")
    (web:fetch "https://e/0")
    (let ((page (first nodecode-websearch::*pages*)))
      (setf page.fetched-at (- (get-universal-time) 3601)))
    (web:fetch "https://e/0")
    (is (= 2 (length calls)) "an expired entry is fetched again")
    (loop for i from 1 to 64 do (web:fetch (format nil "https://e/~d" i)))
    (is (= 64 (length nodecode-websearch::*pages*)) "capacity holds 64")
    (let ((before (length calls)))
      (web:fetch "https://e/0")
      (is (= (1+ before) (length calls)) "the oldest was evicted and refetches"))))

(deftest websearch-cell-fetch-jina-reader ()
  (with-websearch ()
    (with-scripted-http (calls :text (format nil "Title: Jina Title~%URL Source: https://e/j~%~%Markdown Content:~%# hi~%body text")
                               :type "text/markdown")
      (let ((text (web:fetch "https://e/j" :reader "jina")))
        (is (equal "https://r.jina.ai/https://e/j" (http-call-url (first calls))) "jina URL")
        (is (equal "text/markdown" (call-header (first calls) "accept")) "asks for markdown")
        (is (null (call-header (first calls) "sec-fetch-mode")) "no browser set on the reader's own request")
        (is (equal (format nil "Jina Title~%https://e/j~%~%# hi~%body text") text) "title and the markdown after the marker"))
      (web:fetch "https://e/j")
      (is (= 2 (length calls)) "http and jina are separate cache entries")))
  (with-websearch ("fetch" (nlk:json-object "reader" "jina"))
    (with-scripted-http (calls :text "no marker here" :type "text/plain")
      (let ((text (web:fetch "https://e/k")))
        (is (uiop:string-prefix-p "https://r.jina.ai/" (http-call-url (first calls))) "fetch.reader makes jina the default")
        (is (contains-p text (format nil "(untitled)~%https://e/k~%~%no marker here")) "marker absent: whole body")))))

;;; --- the seam's pure parts -------------------------------------------------------

(deftest websearch-cell-read-capped-keeps-the-first-bytes ()
  (let ((hundred (make-array 100 :element-type '(unsigned-byte 8) :initial-element 7))
        (far (+ (get-internal-real-time) (* 60 internal-time-units-per-second))))
    (multiple-value-bind (octets truncated)
        (nlk::read-capped (flexi-streams:make-in-memory-input-stream hundred) 50 far)
      (is (and (= 50 (length octets)) truncated) "past the cap: the first 50 octets, truncated"))
    (multiple-value-bind (octets truncated)
        (nlk::read-capped (flexi-streams:make-in-memory-input-stream hundred) 200 far)
      (is (and (= 100 (length octets)) (not truncated)) "under the cap: all octets"))
    (multiple-value-bind (octets truncated)
        (nlk::read-capped (flexi-streams:make-in-memory-input-stream hundred) 100 far)
      (is (and (= 100 (length octets)) (not truncated)) "exactly the cap: all octets, not truncated"))))

(deftest websearch-cell-canonical-url-and-browser-headers ()
  (flet ((canon (url) (nodecode-websearch::canonical-url url)))
    (is (equal (canon "https://A.Example/doc/") (canon "http://a.example/doc")) "host case, scheme and a trailing slash do not split a page")
    (is (equal (canon "https://a.example/doc?utm_source=x&id=2#top") (canon "https://a.example/doc?id=2")) "utm_* and the fragment go, other parameters stay")
    (is (not (equal (canon "https://a.example/doc?id=2") (canon "https://a.example/doc?id=3"))) "a different parameter is a different page")
    (is (equal "not a url" (canon "not a url")) "unparseable: its own key"))
  (let ((headers (nodecode-websearch::browser-headers "text/html" '(("referer" . "https://r/")))))
    (is (equal nodecode-websearch::+user-agent+ (cdr (assoc "user-agent" headers :test #'string=))))
    (is (equal "text/html" (cdr (assoc "accept" headers :test #'string=))))
    (is (equal "document" (cdr (assoc "sec-fetch-dest" headers :test #'string=))))
    (is (equal "https://r/" (cdr (assoc "referer" headers :test #'string=))) "the request's own headers ride along")))

;;; --- what a transcript calls these verbs ------------------------------------------

(deftest websearch-cell-verbs-name-themselves-in-the-transcript ()
  ;; A snippet of web:search reads as a search and folds with its neighbours
  ;; (activity.lisp), and each verb stamps its call on the result fact it
  ;; runs under — before anything else, so even a refusal says what was asked.
  (is-verb-receipts (nodecode-websearch::*websearch* (web:search "rust async")
                                                     (web:fetch "https://example.com/a"))
    :receipts (("web:search" . "rust async") ("web:fetch" . "https://example.com/a"))
    :stamped "both calls are on the result fact, in order"
    :source "(progn (web:search \"rust async\") (web:fetch \"https://example.com/a\"))"
    :groups "the receipts make the snippet a routine one"
    :title "Searched 'rust async', Fetched https://example.com/a"))
