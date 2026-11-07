;;;; cell.lisp --- the primer, the settings, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns a vocabulary through the manual: while the cell runs,
;;;; (help :web) answers the primer, and every session's help section, TUI or
;;;; channel lane, carries one line naming the two verbs. Stopping the cell
;;;; takes the topic off. Nothing in the core names this cell.
;;;;
;;;; Config, a sibling top-level key next to `chrome' and `notify':
;;;;   "websearch": {"enabled": true,
;;;;                 "provider": "brave",
;;;;                 "providers": {"brave":     {"api_key": "..."},
;;;;                               "exa":       {"api_key": "..."},
;;;;                               "tavily":    {"api_key": "..."},
;;;;                               "firecrawl": {"api_key": "..."}},
;;;;                 "max_results": 5,
;;;;                 "fetch": {"max_bytes": 5242880, "limit": 6000, "reader": "http"}}
;;;; Keys are inline, as the core `providers' block has them, and every key
;;;; is redacted from any text the model sees. No key at all is the
;;;; zero-config shape: the public floor answers (DuckDuckGo, Firecrawl and
;;;; keyless Exa, merged) and fetch escalates to Firecrawl and Exa keyless;
;;;; a key for brave, exa or tavily is the upgrade, a firecrawl key only
;;;; lifts its limits. fetch.reader: http (the chain), firecrawl, exa, jina.

(in-package #:nodecode-websearch)


;;; --- the primer -----------------------------------------------------------
;;; --- the manual ---------------------------------------------------------------
;;; (help :web) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "Web access is available through the nodecode-websearch cell: two Lisp functions in the
web: package, called through eval. Both return a string.
  (web:search \"query\" &key n provider)   n results (default 5, max 10). Each result is one numbered
      line - title, snippet, date when known - with its url under it. A configured provider
      (:provider \"brave\"|\"exa\"|\"tavily\", or websearch.provider) answers alone and may put its own
      answer on top; with no key the public floor answers: DuckDuckGo, Firecrawl and Exa merged, the
      pages most of them name first. An engine that fails, or a keyed provider named without a
      key, is a [note] above the results, never an error. (:provider \"arxiv\") is keyless and
      searches papers instead of the web - title, abstract, date, the abs url - never merged into
      the floor; web:fetch reads a paper's abs page or PDF once you know its id.
  (web:fetch \"https://...\" &key offset limit reader)   a page as readable text: title, url, then up
      to :limit chars (default 6000, max 7000) from :offset (default 0). A cut result ends with
      \"Showing A-B of N chars; (web:fetch url :offset B) for the next slice\" - call exactly that to
      read on; the page is cached, so it costs no second download. The page is read here first:
      HTML becomes text with links as \"text (url)\", a PDF is converted, other text comes verbatim, a
      page over 5 MB is read to the cap and says so. What cannot be read here - a JavaScript page, a
      host that refused, a document - goes to Firecrawl, then Exa, each failure a [note] under the
      url naming who answered; :reader \"firecrawl\"|\"exa\" reads with that one alone.
Doctrine: search to discover, fetch a url you already know. Read the numbered list before a second
search: when a line already answers, stop - do not fetch it to confirm. A second search is for a new
question, not the same one reworded. Cite urls in your answer. Pass yield_time_ms 60000 on an eval
that fetches: a page that goes to a second reader can take a minute. ERROR: WEBSEARCH-ERROR names
the problem; retry a failing call at most once, with a different url or query."
  "What (help :web) answers while the cell runs.")

;;; --- the entry: the one declaration ---------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: what the `websearch' declaration
derived (VALUES -- the chosen provider and the result count), plus the two
nested objects a flat section cannot carry."
  ;; `providers' is one key per engine; `fetch' the reader and its bounds. The
  ;; chosen provider is checked against the keys here rather than in the
  ;; declaration, because whether a name is usable depends on another member
  ;; -- and with none named, the first configured key leads, else the public
  ;; floor.
  (multiple-value-bind (providers providers-present) (nlk:section-value table "providers")
    (when (and providers-present (not (hash-table-p providers)))
      (nlk:config-error "providers must be an object, got ~s" providers))
    (let* ((keys (loop for name in +provider-names+
                       for entry = (nlk:json-value providers :object name)
                       for key = (and entry (nlk:config-string entry "api_key"))
                       when key collect (cons name key)))
           (chosen (getf values :provider))
           (fetch (nlk:json-value table :object "fetch")))
      (when (and chosen
                 (not (assoc chosen keys :test #'string=))
                 (not (member chosen (append +floor+ +named-keyless+) :test #'string=)))
        (nlk:config-error "provider ~a has no key: set providers.~a.api_key" chosen chosen))
      (list :provider (or chosen (car (first keys)) "duckduckgo")
            :max-results (getf values :max-results)
            :keys keys
            :max-bytes (nlk:config-integer fetch "max_bytes" 5242880 :min 1024)
            :limit (nlk:config-integer fetch "limit" 6000 :min +limit-floor+)
            :reader (nlk:config-enum fetch "reader" "http" +readers+)))))

(nle:define-cell websearch
  (:section ("websearch")
    (:guide "no key at all is the zero-config shape: DuckDuckGo, Firecrawl and keyless Exa answer, merged; a brave, exa or tavily key under providers.<name>.api_key is the upgrade, and a firecrawl key only lifts its limits; fetch.reader picks the reader chain (http walks to firecrawl then exa; firecrawl, exa or jina reads with that one alone) and fetch.max_bytes and fetch.limit bound what one page costs")
    ("provider" :choice :options +provider-names+
                :doc "the engine that answers alone; unset, the first configured key leads, else the public floor")
    ("max_results" :integer :default 5 :min 1
                   :doc "how many results a search answers with by default"))
  ;; providers.<name>.api_key and the fetch object are nested, and the
  ;; provider default depends on which keys are set: the declaration carries
  ;; what it can and this reads the rest.
  (:settings 'read-settings)
  (:help :web "web:search finds pages or papers (arxiv), web:fetch reads a page or a PDF" +primer+))
