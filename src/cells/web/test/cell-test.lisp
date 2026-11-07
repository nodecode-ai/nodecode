;;;; cell-test.lisp --- the page on the gateway's port, and nothing beside it.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun web-get (port path)
  "GET PATH on PORT's gateway, following no redirect => (values BODY STATUS HEADERS)."
  (nlk:http :get (format nil "http://127.0.0.1:~d~a" port path)))

(deftest web-cell-serves-the-page-and-nothing-beside-it (with-temp-gateway (port))
  (with-cell-stop ((web-start))
    (multiple-value-bind (body status headers) (web-get port "/web/")
      (is (= 200 status) "the page answers without the operator token")
      (is (search "text/html" (gethash "content-type" headers)))
      (is (search "<title>Nodecode</title>" body) "it is index.html")
      (is (search "default-src 'self'" (gethash "content-security-policy" headers)) "loads only its own")
      (is (search "frame-ancestors 'none'" (gethash "content-security-policy" headers)) "never framed"))
    (loop for (name type) in '(("app.js" "text/javascript") ("md.js" "text/javascript")
                               ("control.js" "text/javascript") ("theme.js" "text/javascript")
                               ("i18n.js" "text/javascript") ("zh.js" "text/javascript")
                               ("app.css" "text/css") ("mark.svg" "image/svg+xml")
                               ("geist.woff2" "font/woff2") ("martian-mono.woff2" "font/woff2"))
          do (multiple-value-bind (body status headers) (web-get port (format nil "/web/~a" name))
               (declare (ignore body))
               (is (= 200 status) name)
               (is (search type (gethash "content-type" headers)) type)))
    (dolist (outside '("/web/cell.lisp" "/web/../cell.lisp" "/web/.hidden" "/web/test/support.lisp"
                       "/web/nodecode-web.asd"))
      (is (= 404 (raw-gateway-status port outside (format nil "Host: 127.0.0.1:~d" port))) outside))
    (is (= 302 (raw-gateway-status port "/web" (format nil "Host: 127.0.0.1:~d" port))) "/web leads on")
    (funcall (shiftf stop nil))
    (is (= 404 (raw-gateway-status port "/web/" (format nil "Host: 127.0.0.1:~d" port))) "stopped: gone")))

(deftest web-cell-keeps-the-page-it-serves-on
    (with-saved-globals ((nlk::*cells* (list (nlk::make-cell :name "nodecode-web"
                                                                :systems '("nodecode-web"))))))
  ;; The page's own switch cannot take the page away: the folder says why
  ;; through the off question's advice, while it runs and only for itself.
  (is (null (nle::cell-off-refusal "nodecode-web")) "stopped, it says nothing")
  (with-cell-stop ((web-start))
    (is (search "serves this page" (nle::cell-off-refusal "nodecode-web")))
    (is (null (nle::cell-off-refusal "nodecode-other")) "another folder is not its to keep"))
  (is (null (nle::cell-off-refusal "nodecode-web")) "and stopped again, its advice is gone"))

;; The page's colors. app.css draws every color through a token of :root, and
;; the light block (theme.js sets data-theme on the root) redefines every one of
;; them: a literal anywhere else stays dark in light, and a token the light
;; block forgot does too.

(defun web-page-text (name)
  "The page's file NAME, read whole."
  (uiop:read-file-string (merge-pathnames name nodecode-web::*page-directory*)))

(defun web-css-declarations (css)
  "CSS's declarations, comments dropped, each as (SELECTOR . DECLARATION): the
selector of the innermost rule it is written in, both trimmed."
  (let ((text (with-output-to-string (out)
                (loop with at = 0
                      for open = (search "/*" css :start2 at)
                      do (write-string css out :start at :end (or open (length css)))
                         (unless open (return))
                         (setf at (+ 2 (search "*/" css :start2 (+ 2 open)))))))
        (rules '()) (declarations '()) (pending (make-string-output-stream)))
    (flet ((segment () (string-trim '(#\Space #\Tab #\Newline) (get-output-stream-string pending))))
      (loop for char across text
            do (case char
                 (#\{ (push (segment) rules))
                 ((#\; #\}) (let ((declaration (segment)))
                              (when (plusp (length declaration))
                                (push (cons (first rules) declaration) declarations)))
                            (when (char= char #\}) (pop rules)))
                 (t (write-char char pending)))))
    (nreverse declarations)))

(defun web-color-literal-p (text)
  "True when TEXT spells a color itself: a hex color, or rgb(), rgba(), hsl(), hsla()."
  (or (some (lambda (fn) (search fn text)) '("rgb(" "rgba(" "hsl(" "hsla("))
      (loop for hash = (position #\# text) then (position #\# text :start (1+ hash))
            while hash
            thereis (let ((end (or (position-if-not (lambda (char) (digit-char-p char 16)) text :start (1+ hash))
                                   (length text))))
                      (member (- end hash 1) '(3 4 6 8))))))

(defun web-root-colors (declarations selector)
  "The custom properties SELECTOR declares with a color in their value, by name."
  (loop for (rule . declaration) in declarations
        for colon = (position #\: declaration)
        when (and (string= rule selector) colon (eql 0 (search "--" declaration))
                  (web-color-literal-p (subseq declaration colon)))
          collect (subseq declaration 0 colon)))

(deftest web-cell-theme-colors-are-root-tokens ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (roots '(":root" ":root[data-theme=\"light\"]"))
         (dark (web-root-colors declarations (first roots)))
         (light (web-root-colors declarations (second roots))))
    (is (null (loop for (rule . declaration) in declarations
                    when (and (not (member rule roots :test #'string=)) (web-color-literal-p declaration))
                      collect (format nil "~a { ~a }" rule declaration))) "no color outside the root tokens")
    (is (member "--accent" dark :test #'string=) "the dark tokens are read")
    (is (null (set-difference dark light :test #'string=)) "light redefines every color token")
    (is (null (set-difference light dark :test #'string=)) "and declares none dark lacks")))

;; The page's text. Five tones read as words -- primary down to tertiary -- and
;; each holds WCAG AA (4.5:1) on every surface a run of text stands on: the
;; grounds, a row under the pointer, a field lit by typing. --faint and --dim
;; are the tones of a rule, a dot, an underline, and --accent-fill of a lamp:
;; marks, never a color:.

(defun web-css-color (text)
  "TEXT, a #rrggbb or rgba() color, as (R G B ALPHA), the channels 0-255."
  (let ((hash (position #\# text)) (*read-default-float-format* 'double-float))
    (if hash
        (append (loop for at from (1+ hash) by 2 repeat 3 collect (parse-integer text :start at :end (+ at 2) :radix 16)) '(1))
        (mapcar #'read-from-string (cl-ppcre:all-matches-as-strings "[0-9.]+" text)))))

(defun web-contrast (a b)
  "The WCAG contrast ratio of two colors."
  (flet ((luminance (color)
           (loop for channel in color repeat 3 for weight in '(0.2126d0 0.7152d0 0.0722d0)
                 sum (* weight (let ((v (/ channel 255d0)))
                                 (if (<= v 0.03928d0) (/ v 12.92d0) (expt (/ (+ v 0.055d0) 1.055d0) 2.4d0)))))))
    (let ((x (luminance a)) (y (luminance b)))
      (/ (+ (max x y) 0.05d0) (+ (min x y) 0.05d0)))))

(defun web-over (color ground)
  "COLOR, its alpha its fourth channel, laid over the opaque GROUND."
  (append (loop for c in color repeat 3 for g in ground collect (+ (* c (fourth color)) (* g (- 1 (fourth color))))) '(1)))

(defun web-token (declarations light name)
  "The color the theme gives NAME: the light block's when LIGHT and it sets one, else the root's."
  (flet ((declared (selector)
           (loop for (rule . declaration) in declarations
                 when (and (string= rule selector) (eql 0 (search (format nil "~a:" name) declaration)))
                   return (web-css-color (subseq declaration (1+ (length name)))))))
    (or (and light (declared ":root[data-theme=\"light\"]")) (declared ":root"))))

(defun web-surfaces (declarations light)
  "Every surface text stands on, each as (NAME . COLOR): the grounds, and what a row under
the pointer and a field lit by typing lay over the ones they stand on."
  (flet ((token (name) (web-token declarations light name)))
    (append (loop for name in '("--void" "--canvas" "--inset" "--subtle" "--selected" "--stage"
                                "--column-ground" "--slab-subtle" "--slab-inset" "--raised")
                  collect (cons name (token name)))
            (loop for fill in '("--field" "--field-lit" "--hover")
                  append (loop for ground in '("--column-ground" "--stage" "--raised")
                               collect (cons (format nil "~a on ~a" fill ground) (web-over (token fill) (token ground))))))))

(deftest web-cell-text-holds-aa-on-every-surface ()
  (let ((declarations (web-css-declarations (web-page-text "app.css"))) (short '()))
    (dolist (light '(nil t))
      (flet ((token (name) (web-token declarations light name))
             (check (what tone ground)
               (when (< (web-contrast tone ground) 4.5)
                 (push (format nil "~a~:[~; (light)~]" what light) short))))
        (loop for (name . surface) in (web-surfaces declarations light)
              do (dolist (tone '("--primary" "--foreground" "--secondary" "--muted" "--tertiary" "--accent" "--danger-lit"))
                   (check (format nil "~a on ~a" tone name) (token tone) surface)))
        ;; A flag stands on a wash of its own tone (13%), a diff line on its fill.
        (dolist (base '("--stage" "--raised" "--slab-inset" "--slab-subtle"))
          (dolist (tone '("--accent" "--danger-lit"))
            (check (format nil "~a on its wash over ~a" tone base) (token tone)
                   (web-over (append (subseq (token tone) 0 3) '(0.13d0)) (token base))))
          (loop for (tone fill) in '(("--diff-add" "--diff-add-fill") ("--diff-del" "--diff-del-fill"))
                do (check (format nil "~a on ~a over ~a" tone fill base) (token tone) (web-over (token fill) (token base)))))))
    (is (null short) (format nil "text under 4.5:1: ~{~a~^, ~}" short))))

(deftest web-cell-text-tones-step-down ()
  (let ((declarations (web-css-declarations (web-page-text "app.css"))))
    (dolist (light '(nil t))
      (let ((ratios (loop with stage = (web-token declarations light "--stage")
                          for tone in '("--primary" "--foreground" "--secondary" "--muted" "--tertiary")
                          collect (web-contrast (web-token declarations light tone) stage))))
        (is (loop for (above below) on ratios while below always (>= (/ above below) 1.15d0))
            (format nil "each text tone a step below the last~:[~; (light)~]: ~{~,2f~^ ~}" light ratios))))))

(deftest web-cell-rule-tones-are-never-text ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (painted (loop for (rule . declaration) in declarations
                        when (and (eql 0 (search "color:" declaration))
                                  (some (lambda (tone) (search tone declaration)) '("var(--faint)" "var(--dim)" "var(--accent-fill)")))
                          collect (format nil "~a { ~a }" rule declaration)))
         (faded (loop for (rule . declaration) in declarations
                      when (and (or (search ".dimmed" rule) (search ".jl.same" rule)) (search "opacity" declaration))
                        collect rule)))
    (is (null painted) "no color: takes a rule or fill tone")
    (is (null faded) "what is dimmed is dimmed by tone, never by opacity")))

(deftest web-cell-theme-is-set-before-the-first-paint ()
  ;; A module runs after the page is parsed, and the page may paint before it:
  ;; the theme is a classic script in the head, ahead of the stylesheet.
  (let* ((page (web-page-text "index.html"))
         (theme (search "<script src=\"theme.js\"></script>" page)))
    (is theme "theme.js is a classic script, neither module nor deferred")
    (is (and theme (< theme (search "</head>" page)) (< theme (search "app.css" page))) "in the head, before the stylesheet")
    (is (search "data-choice=\"system\"" page) "the switch offers System")))

;; The transcript while a turn runs, and when it ends. What a running turn
;; takes away (an earlier turn's Rewind and Fork, a failed turn's Retry) keeps
;; its room, so nothing above the answer moves when the turn ends; and whether
;; the reader follows the end is theirs, from their own scrolling, re-applied
;; after every change of size: a reader at the end still sees the answer's last
;; lines after the turn ends, and one away from it has the way back, shown only
;; with the transcript.

(deftest web-cell-a-running-turn-keeps-the-transcripts-room ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (busy (remove-if-not (lambda (rule) (search "[data-busy]" (car rule))) declarations)))
    (is busy "app.css has rules for a running turn")
    ;; No rule of a running turn takes a box of the transcript's room away;
    ;; the composer's foot is one row whatever it holds.
    (is (null (loop for (rule . declaration) in busy
                    when (and (not (search ".composer" rule))
                              (cl-ppcre:scan "^(display|height|max-height|margin|padding)" declaration))
                      collect (format nil "~a { ~a }" rule declaration))))
    ;; An earlier turn's Rewind and Fork are hidden in place, and its number stays.
    (is (find '(".app[data-busy] .ask button.rewind" . "visibility: hidden") busy :test #'equal))))

(deftest web-cell-the-reader-at-the-end-stays-there ()
  (let* ((app (web-page-text "app.js"))
         (place (subseq app (search "function placeJump()" app)))
         (place (subseq place 0 (search "}" place))))
    (is (cl-ppcre:scan "new ResizeObserver\\(follow\\)" app) "a change of size re-follows")
    (is (cl-ppcre:scan "settling\\.observe\\(rowsEl\\)" app) "the rows growing, a picture loading")
    (is (cl-ppcre:scan "settling\\.observe\\(ledger\\)" app) "the transcript getting shorter as the box grows")
    ;; Where the reader is comes from their own scrolling, and one distance
    ;; decides both following and the way back.
    (is (cl-ppcre:scan "following = ledger\\.scrollHeight - ledger\\.scrollTop - ledger\\.clientHeight < NEAR_END" app))
    (is (= 1 (length (cl-ppcre:all-matches-as-strings "ledger\\.scrollHeight - ledger\\.scrollTop" app))))
    (is (search "ledger.hidden" place) "the way back shows only with the transcript")
    (is (search "following" place) "and only to a reader away from the end")))

;; The transcript reads down one edge, as the terminal draws it. The operator's
;; line starts where the answer starts; the work's one line marks each call; a
;; turn that ended well keeps its footnote under the pointer -- always on a
;; touch screen -- while a failed or stopped end stays in view, and the
;; context's eviction count (told by its fields, never its words) is such a
;; footnote too; and a reflection's recap is not drawn at all, as the TUI's
;; Standard viewport draws none.
(deftest web-cell-the-transcript-reads-down-one-edge ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (js (web-page-text "app.js"))
         (work (web-js-function js "drawWork"))
         (note (web-js-function js "noteRow")))
    (is (equal "flex-start" (web-css-value css ".ask" "align-items")) "the operator's line on the answer's edge")
    (is (search "callTicks(calls)" work) "the work's line marks each call")
    (is (search "\"failed\"" (web-js-function js "callTicks")) "a failed call's mark is its own")
    (is (search "row.outcome === \"completed\"" work) "the footnote is read off the outcome")
    (is (equal "0" (web-css-value css ".rest.meta" "opacity")) "under the pointer")
    (is (find '(".rest.meta" . "opacity: 1") css :test #'equal) "and always there on a touch screen")
    (is (search "!row.outcome && !row.turn_id" note) "an eviction count is told by its fields")
    (is (search "|| noteRow(row)" work) "and is a footnote")
    (is (search "recorded === \"reflection\") return;" (web-js-function js "renderTranscript")) "no recap in the transcript")
    (is (null (search "drawRecap" js)) "and nothing left that drew one")
    ;; A running turn's newest finished thinking stands under its bar, no
    ;; label, quieter than the work, faded in -- the TUI's live thought;
    ;; words still arriving never show, and an ended turn keeps its thinking
    ;; in its work.
    (is (search "liveThought(work, turn.key)" (subseq work (search "if (live)" work))) "under the running bar")
    (is (search "fold(\"work\", `work-${turn.key}`, false)" work) "the work folded by default, live or done")
    (is (search "!row.live" (web-js-function js "finishedThought")) "only what the model finished")
    (is (equal "var(--tertiary)" (web-css-value css ".prose.live-thought" "color")) "a step quieter than the work")
    (is (search "thought-in" (web-css-value css ".prose.live-thought" "animation")) "faded in")
    (is (null (search "t(\"Think\")" js)) "with no label")))

;; An answer's type. Each heading level is a step of size above the next and a
;; weight above the text; emphasis is weight and slant, never tone alone; running
;; text keeps a measure of about 70 characters while a table or a fence takes
;; the lane; and code is never in the dimmest tone. Geist at the body's 15 px
;; sets 6.94 px a character on average (measured on an answer's paragraph), so
;; the measure's pixels say its characters.

(defparameter *web-geist-average-em* 0.4625d0
  "Geist's average advance per character, in ems, over English prose.")

(defun web-css-value (declarations selector property)
  "What SELECTOR's rule gives PROPERTY, trimmed, or NIL."
  (loop for (rule . declaration) in declarations
        when (and (string= rule selector) (eql 0 (search (format nil "~a:" property) declaration)))
          return (string-trim " " (subseq declaration (1+ (length property))))))

(deftest web-cell-an-answer-reads-as-structure ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (body (web-css-px declarations ".prose" "font-size"))
         (sizes (mapcar (lambda (level) (web-css-px declarations level "font-size"))
                        '(".prose h1" ".prose h2" ".prose h3" ".prose h4, .prose h5, .prose h6")))
         (measure ".prose > :is(p, ul, ol, blockquote, h1, h2, h3, h4, h5, h6)")
         (width (web-css-px declarations measure "max-width")))
    (is (= 15 body))
    (is (and (every #'integerp sizes) (apply #'> sizes) (>= (car (last sizes)) body)) (format nil "each level a step down: ~a" sizes))
    (is (<= 600 (web-css-px declarations ".prose h1, .prose h2, .prose h3, .prose h4, .prose h5, .prose h6" "font-weight")))
    (is (<= 600 (web-css-px declarations ".prose strong" "font-weight")))
    ;; The page refuses synthesized faces and Geist has no italic of its own:
    ;; emphasis asks for the slant back.
    (is (equal "italic" (web-css-value declarations ".prose em" "font-style")))
    (is (equal "style" (web-css-value declarations ".prose em" "font-synthesis")))
    (is (and width (<= 60 (/ width (* body *web-geist-average-em*)) 80)) (format nil "a measure of 60 to 80 characters: ~a px" width))
    (is (notany (lambda (wide) (search wide measure)) '("pre" "table" "div" "figure")) "a fence, a table and a picture keep the lane")
    (dolist (code '(".prose pre" ".prose code"))
      (is (member (web-css-value declarations code "color") '("var(--foreground)" "var(--secondary)") :test #'equal) code))))

;; The page's words. English is the source (i18n.js): every word the page hands
;; t() as a literal and every word its HTML marks has its Chinese in zh.js, a
;; {name} in one is a {name} in the other, and zh.js keeps nothing the page no
;; longer says. A word handed to t() as anything but a literal is a word this
;; cannot see, so a template never is: its values ride {name}.

(defun web-js-string (text start)
  "The JavaScript string literal whose quote opens at START in TEXT, read."
  (let ((out (make-string-output-stream)) (quote (char text start)) (at (1+ start)))
    (loop for char = (char text at)
          do (cond ((char= char quote) (return (get-output-stream-string out)))
                   ((char= char #\\)
                    (let ((next (char text (incf at))))
                      (case next
                        (#\n (write-char #\Newline out))
                        (#\u (write-char (code-char (parse-integer text :start (1+ at) :end (+ at 5) :radix 16)) out)
                         (incf at 4))
                        (t (write-char next out)))))
                   (t (write-char char out)))
             (incf at))))

(defun web-calls (text)
  "Where TEXT calls t(: each position after its parenthesis."
  (loop for at = (search "t(" text) then (search "t(" text :start2 (1+ at))
        while at
        unless (and (plusp at) (let ((before (char text (1- at))))
                                 (or (alphanumericp before) (find before "_$."))))
          collect (+ at 2)))

(defun web-attribute (tag name)
  "The value TAG gives the attribute NAME, or NIL."
  (let ((at (search (format nil " ~a=\"" name) tag)))
    (when at
      (let ((start (+ at (length name) 3)))
        (subseq tag start (position #\" tag :start start))))))

(defun web-html-text (text)
  "TEXT with the HTML entities the page writes read."
  (loop for (entity . char) in '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'") ("&amp;" . "&"))
        do (setf text (cl-ppcre:regex-replace-all entity text char))
        finally (return text)))

(defun web-html-words (page)
  "Every word PAGE marks: the text of an element with data-i18n, and each
attribute its data-i18n-attrs names."
  (loop for open = (position #\< page) then (position #\< page :start close)
        for close = (and open (position #\> page :start open))
        while close
        append (let ((tag (subseq page open close)))
                 (append (when (or (search " data-i18n " tag) (search " data-i18n>" (concatenate 'string tag ">")))
                           (list (web-html-text (string-trim '(#\Space #\Newline)
                                                             (subseq page (1+ close) (position #\< page :start close))))))
                         (loop for name in (uiop:split-string (or (web-attribute tag "data-i18n-attrs") "") :separator " ")
                               for value = (and (plusp (length name)) (web-attribute tag name))
                               when value collect (web-html-text value))))))

(defun web-placeholders (text)
  "The {name}s TEXT carries, sorted."
  (sort (cl-ppcre:all-matches-as-strings "\\{\\w+\\}" text) #'string<))

(deftest web-cell-every-word-the-page-says-has-its-chinese ()
  (let* ((zh (web-page-text "zh.js"))
         (catalog (nlk:decode-json (subseq zh (position #\{ zh :start (search "export const ZH" zh))
                                           (1+ (position #\} zh :from-end t)))
                                   :whole t))
         (scripts (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                        unless (string= (file-namestring file) "zh.js")
                          collect (uiop:read-file-string file)))
         (calls (loop for text in scripts
                      append (loop for at in (web-calls text) collect (cons text at))))
         (words (remove-duplicates
                 (append (web-html-words (web-page-text "index.html"))
                         (loop for (text . at) in calls
                               when (find (char text at) "\"'") collect (web-js-string text at)))
                 :test #'string=)))
    (let ((templates (loop for (text . at) in calls
                           when (char= (char text at) #\`) collect (subseq text at (min (length text) (+ at 40)))))
          (missing (remove-if (lambda (word) (gethash word catalog)) words))
          (stale (loop for word being the hash-keys of catalog
                       unless (member word words :test #'string=) collect word))
          (unmatched (loop for word being the hash-keys of catalog using (hash-value chinese)
                           unless (equal (web-placeholders word) (web-placeholders chinese)) collect word)))
      (is (null templates) "no t() of a template")
      (is (null missing) "every word the page says has its Chinese")
      (is (null stale) "zh.js keeps nothing the page no longer says")
      (is (null unmatched) "a {name} in the English is one in the Chinese"))))

;; The page asks in the page. A browser's own dialog takes any string, in no
;; style of the page's, and checks nothing: the folder a new session opens in
;; was asked that way, and sessions opened in folders that were not there. The
;; opener names its folder before the first message, and asks the gateway of
;; one typed (GET /api/gateway/folder) before it takes it.

(deftest web-cell-asks-in-the-page-and-names-the-folder-first ()
  (let ((dialogs (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                       when (cl-ppcre:scan "\\b(?:window\\.)?(?:prompt|confirm|alert)\\(" (uiop:read-file-string file))
                         collect (file-namestring file))))
    (is (null dialogs) (format nil "no browser dialog: ~{~a~^, ~}" dialogs))
    (is (search "data-i18n>Folder:</span>" (web-page-text "index.html")) "the opener labels its folder in words")
    (is (search "/api/gateway/folder" (web-page-text "app.js")) "and asks the gateway of one typed")))

;; A call's card speaks to the operator: its result is the gateway's reading
;; of it (tool.shown), never the text the model read -- the offer to set a
;; watch, the background handoff's instructions, a stop's compiler report --
;; and a stop reads as whose it was, never `failed'.

(deftest web-cell-a-call-card-says-the-operators-words ()
  (let* ((app (web-page-text "app.js"))
         (start (search "function toolAct(row)" app))
         (end (and start (search (format nil "~%}~%") app :start2 start)))
         (body (and end (subseq app start end))))
    (is body "app.js draws a call's card in toolAct")
    (is (search "tool.shown" body) "its result is the gateway's reading")
    (is (not (search "row.text" body)) "never the text the model read")
    (is (search "status === \"stopped\"" body) "a stop is its own end")
    (dolist (word '("Stopped by you after {time}" "Stopped: the gateway shut down while this ran"
                    "moved to the background" "Its output is over the message box while it runs, and under the turn once it ends."))
      (is (search (format nil "t(~s" word) app) word))))

;; What the page keeps in this browser -- a theme, a draft, what each session
;; showed when it was last on screen -- is a convenience: a private window, a
;; preview or site data turned off makes the storage throw, and the page must
;; still draw. Every call on localStorage or sessionStorage stands inside a try
;; block (safeGet is one).

(defun web-guarded-p (text at)
  "Whether a block opened by `try' encloses position AT of TEXT."
  (loop with depth = 0
        for i from (1- at) downto 0
        do (case (char text i)
             (#\} (incf depth))
             (#\{ (if (plusp depth)
                      (decf depth)
                      (let ((head (string-right-trim '(#\Space #\Tab #\Newline)
                                                     (subseq text (max 0 (- i 12)) i))))
                        (when (and (>= (length head) 3)
                                   (string= "try" head :start2 (- (length head) 3)))
                          (return t))))))))

(defun web-unguarded-storage (text)
  "Each localStorage or sessionStorage call in TEXT, a script of the page, that
no enclosing try block holds, as the line it is on."
  (loop for at in (cl-ppcre:all-matches
                   "(?:local|session)Storage\\.(?:getItem|setItem|removeItem|clear)\\(" text)
          by #'cddr
        unless (web-guarded-p text at)
          collect (string-trim " " (subseq text (1+ (or (position #\Newline text :end at :from-end t) -1))
                                        (or (position #\Newline text :start at) (length text))))))

(deftest web-cell-a-refused-storage-never-breaks-the-page ()
  (let ((unguarded (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                         append (web-unguarded-storage (uiop:read-file-string file)))))
    (is (null unguarded) (format nil "every storage call is inside a try: ~{~a~^; ~}" unguarded)))
  (let ((bare (format nil "function f() {~%  return localStorage.getItem(KEY);~%}")))
    (is (equal '("return localStorage.getItem(KEY);") (web-unguarded-storage bare)) "and one outside is seen")))

;; /help lists the gateway's commands, every shell's, and the page adds the
;; keys its box answers (KEYS in app.js). Every key the box's keydown handler
;; reads is named there, a modifier it reads with Enter too: a key the page
;; answers and /help never names is how Alt+Enter steered unseen.

(defparameter *web-key-names*
  '(("Enter" . "Enter") ("Tab" . "Tab") ("Escape" . "Esc") ("ArrowUp" . "↑") ("ArrowDown" . "↓"))
  "A keydown event's key -> how /help names it.")

(deftest web-cell-help-names-every-key-the-box-answers ()
  (let* ((js (web-page-text "app.js"))
         (at (search "promptEl.addEventListener(\"keydown\"" js))
         (handler (and at (subseq js at (search (format nil "~%});") js :start2 at))))
         (table (let ((start (search "const KEYS = [" js)))
                  (and start (subseq js start (search (format nil "~%];") js :start2 start)))))
         (named (let ((out '()))
                  (cl-ppcre:do-register-groups (label) ("\\[\"([^\"]+)\"," (or table "")) (push label out))
                  out))
         (answered (remove-duplicates
                    (append (let ((out '()))
                              (cl-ppcre:do-register-groups (key) ("event\\.key === \"([^\"]+)\"" (or handler ""))
                                (push (or (cdr (assoc key *web-key-names* :test #'string=)) key) out))
                              out)
                            (and handler (search "event.altKey" handler) '("Alt+Enter"))
                            (and handler (search "event.shiftKey" handler) '("Shift+Enter")))
                    :test #'string=))
         (unnamed (remove-if (lambda (key) (some (lambda (label) (search key label)) named)) answered)))
    (is handler "the box has its keydown handler")
    (is (subsetp '("Enter" "Tab" "Esc" "↑" "Alt+Enter") answered :test #'string=) "the keys the shell's contract names")
    (is (null unnamed) (format nil "/help names every key the box answers: ~{~a~^, ~} unnamed" unnamed))
    (is (search "helpKeys(line)" js) "and /help's answer carries them")))

;; The phone's hit areas. A mark is 28 px and a fold row 13, under the 32 a
;; finger wants at least and the 44 its primary controls want. A block of app.css
;; grows them, and only behind (pointer: coarse), (max-width: 720px): a desktop
;; with a mouse matches neither and is not moved a pixel. This reads that block,
;; from its marker to its end's: every rule of it stands inside the two
;; conditions, and each control the operator named has at least its size, by
;; the property that sets it.

(defparameter *web-hit-marker* "/* --- a finger's hit areas")
(defparameter *web-hit-end* "/* --- end of a finger's hit areas")

(defparameter *web-hit-areas*
  '((44 ".strip .icon" "width" "height")             ; More, the sessions toggle
    (44 ".ask .under .icon" "width" "height")        ; Rewind, Fork
    (44 ".round" "width" "height")                   ; send, stop, attach, the jump pill, a picture's close
    (44 ".chip" "min-height")                        ; the model
    (44 ".row" "min-height")                         ; a session, New session, Dashboard, Control
    (44 ".work > summary" "min-height")              ; the fold rows: Worked,
    (44 ".act > summary" "min-height")               ; a call,
    (44 ".card-diff-more > summary" "min-height")    ; a diff's rest,
    (44 ".exec > summary, .exec-line, .bg-open" "min-height") ; an exec, ended or in the band
    (44 ".exec-stop" "width" "height")
    (44 ".quiet, .solid" "min-height")
    (44 ".pop .choice" "min-height")
    (44 ".history .pick, .cell" "min-height")
    (44 ".resume button" "min-height")
    (44 ".dash-door" "min-height")                   ; a phone's Dashboard: a section's row,
    (44 ".glance-alert" "min-height")               ; what is wrong right now
    (44 ".p-row" "min-height")                      ; a phone's row: a section's, a turn's, a call's
    (44 ".p-all, .p-open" "min-height")             ; Show all, Open the turn
    (44 ".p-banner" "min-height")                   ; a line of trouble
    (44 ".p-back" "min-height")                     ; a turn's page, back to its list
    (44 ".inspector .nav :is(.word, .icon)" "min-width" "height") ; a call's previous, next and close
    (44 ".inspector .tab" "min-height")             ; Summary, Tokens, What was sent, Raw
    (44 ".fs-bar .fs-crumbs button" "min-height")   ; a folder on the way down, on a phone
    (44 ".strip h1 .crumb" "min-height")            ; Control / a tab: back to the list, on a phone
    (44 ".fs-acts .icon" "width" "height")          ; Read again, Upload
    (44 "button.sw" "min-height")                   ; a switch, on a phone
    (44 ".ctl-prof-row" "min-height")               ; a profile, its panel on a tap
    (32 ".icon" "width" "height")                    ; every other mark
    (32 ".looks .icon" "width" "height")             ; the foot's link, look and language
    (32 ".views .view" "min-width" "height")
    (32 ".sw" "min-width" "min-height")
    (32 ".word" "min-width" "min-height")
    (32 ".prose pre .copy" "width" "height")
    (32 ".toast .shut" "width" "height")
    (32 ".attachment .remove" "width" "height")
    (32 ".attach-retry" "min-height")                ; a file that did not go up, sent again
    (32 ".track > button.call" "height")
    (32 ".pill" "width" "height")
    (32 ".cell-link" "min-height")
    (32 ".fs-crumbs button" "min-height"))
  "(MINIMUM SELECTOR PROPERTY...): the block gives SELECTOR each PROPERTY at MINIMUM px or more.")

(defun web-css-preludes (css)
  "What CSS opens each of its top-level blocks with (comments already dropped), in order."
  (let ((depth 0) (start 0) (preludes '()))
    (loop for char across css
          for at from 0
          do (case char
               (#\{ (when (zerop depth)
                      (push (string-trim '(#\Space #\Tab #\Newline) (subseq css start at)) preludes))
                    (incf depth))
               (#\} (decf depth)
                (when (zerop depth) (setf start (1+ at))))))
    (nreverse preludes)))

(defun web-css-kit (declarations text)
  "TEXT with each of the kit's scale tokens (var(--fs-*), var(--r-*),
var(--ls-*)) replaced by what :root gives it."
  (cl-ppcre:regex-replace-all
   "var\\((--(?:fs|r|ls)-[a-z]+)\\)" text
   (lambda (match name)
     (declare (ignore match))
     (or (web-css-value declarations ":root" name) name))
   :simple-calls t))

(defun web-css-px (declarations selector property)
  "The whole pixels SELECTOR's rule gives PROPERTY, a scale token read off
:root, or NIL."
  (loop for (rule . declaration) in declarations
        when (and (string= rule selector)
                  (cl-ppcre:scan (format nil "^~a\\s*:" property) declaration))
          return (let ((value (web-css-kit declarations (subseq declaration (1+ (position #\: declaration))))))
                   (and (cl-ppcre:scan "^\\s*-?[0-9]" value)
                        (parse-integer value :junk-allowed t)))))

(defun web-hit-shortfalls (declarations)
  "What *WEB-HIT-AREAS* asks and the block does not give, one sentence each."
  (loop for (minimum selector . properties) in *web-hit-areas*
        append (loop for property in properties
                     for px = (web-css-px declarations selector property)
                     unless (and px (>= px minimum))
                       collect (format nil "~a ~a is ~a, wants ~d" selector property (or px "unset") minimum))))

(deftest web-cell-the-phones-controls-are-a-fingers-size ()
  (let* ((css (web-page-text "app.css"))
         (at (search *web-hit-marker* css))
         (end (and at (search *web-hit-end* css :start2 at)))
         (hits (and end (subseq css at end)))
         (preludes (and hits (web-css-preludes (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" hits ""))))
         (shortfalls (and hits (web-hit-shortfalls (web-css-declarations hits)))))
    (is at "app.css has the hit-area block")
    (is end "and says where it ends")
    (is (equal preludes '("@media (pointer: coarse), (max-width: 720px)" "@media (max-width: 720px)")) "every rule of it is behind the finger or the phone width: a mouse on a wide window moves not a pixel")
    (is (and hits (null shortfalls)) (format nil "each control the operator named has its size: ~{~a~^; ~}" shortfalls))))

;; A phone's Dashboard is a glance: at the phone width the page
;; is the spend and its days, what is wrong right now, four numbers and a row
;; per section saying how it went, and a row opens its section whole in a
;; sheet -- the same read as a phone's rows (the phone standard, below), a
;; wider window keeping the desktop page exactly as it was. Every section the jump menu names has its
;; row; the sheet closes by its veil, its mark and Escape, and a read landing
;; under it neither throws the reader to its top nor raises it again.
(deftest web-cell-a-phones-dashboard-is-a-glance ()
  (let* ((js (web-page-text "observe.js"))
         (app (web-page-text "app.js"))
         (css (web-css-declarations (web-page-text "app.css")))
         (draw (web-js-function js "drawDashboard"))
         (sheets (subseq js (search "const SHEETS = {" js) (search "function glance(" js)))
         (jumps (let ((out '()))
                  (cl-ppcre:do-register-groups (id) ("\\[\"([a-z]+)\", t\\(" (subseq js (search "const JUMPS" js) (search "const DAYS" js)))
                    (push id out))
                  out)))
    (is (search "root.append(glance(parts, data, activity, " draw) "the page draws the glance")
    (is (search "if (parts.dashSheet) root.append(...dashSheet(" draw) "and the open section's sheet")
    (is (= 7 (length jumps)) "the jump menu's sections are read")
    (dolist (id (append jumps '("hours" "folders" "now")))
      (is (search (format nil "~%  ~a: {" id) sheets) (format nil "~a has its sheet" id)))
    (dolist (builder '("spendSection(data)" "modelsSection(data)" "sessionsSection(data, home)" "foldersSection(data, home)" "callsSection(activity)"))
      (is (search builder draw) (format nil "the desktop page draws ~a" builder)))
    (is (not (search "Section(" sheets)) "and a sheet draws the same read as a phone's rows, never the desktop's tables")
    (is (equal "none" (web-css-value css ".dash-glance, .dash-sheet, .dash-veil" "display")) "wider than a phone, none of it shows")
    (is (equal "block" (web-css-value css ".dash-glance" "display")) "at the phone width the glance shows")
    (is (equal "none" (web-css-value css ".dashboard > :not(.dash-bar, .dash-glance, .dash-sheet, .dash-veil, .obs-note)" "display")) "in place of the desktop page")
    (is (equal "fixed" (web-css-value css ".dash-sheet" "position")) "the sheet rises over it")
    (is (search "openDashSheet(door.dataset.dashSheet)" (web-js-listener app "observe")) "a row opens its sheet")
    (is (search "closeDashSheet()" (web-js-listener app "observe")) "its veil and its mark close it")
    (is (search "event.key !== \"Escape\" || !state.dashSheet" app) "and Escape")
    (is (search "body.scrollTop = read" (web-js-function app "renderObserve")) "a read landing keeps the reader's place")
    (is (search "state.dashEnter = false" (web-js-function app "renderObserve")) "and does not raise it again")))

;; The rest of the page on a phone follows the phone standard's eight rules.
;; Without it, controls would fall under 44 px, lines under 12 px, Files'
;; Size column would be cut, the message box would appear on Run and Files,
;; and a diff in Chat would scroll sideways. A list is rows (observe.js prow,
;; pgroup): a name, at most one line under it, one value, and › or +; a turn
;; is a page under its way back; the inspector rises from the foot; the
;; message box is Chat's alone; no text of the parts under 11 px.
(deftest web-cell-a-phone-reads-every-view-as-rows ()
  (let* ((js (web-page-text "observe.js"))
         (app (web-page-text "app.js"))
         (css (web-css-declarations (web-page-text "app.css")))
         (sizes (loop for (selector . declaration) in css
                      when (and (search ".p-" selector)
                                (or (eql 0 (search "font-size:" declaration)) (eql 0 (search "font:" declaration))))
                        collect (cons selector (let ((px (cl-ppcre:scan-to-strings "(\\d+(?:\\.\\d+)?)px" (web-css-kit css declaration))))
                                                 (and px (read-from-string (string-right-trim "px" px))))))))
    (is (search "export const PHONE = matchMedia(\"(max-width: 720px)\");" js) "the phone's width is one query")
    (is (search "if (ui.phone)" (web-js-function js "drawRun")) "the Run view draws a phone's own column")
    (is (search "phone: PHONE.matches, paged: state.runPaged" app) "when the page is a phone's width")
    (is (search "act: \"run-back\"" (web-js-function js "phoneTurn")) "a turn is a page with its way back")
    (is (search "case \"run-back\": state.runPaged = false;" app) "which goes back to the list")
    (is (search "[data-p-more]" (web-js-listener app "observe")) "a row's + and a group's Show all open in place")
    (is (search "open?.has(`all:${key}`)" (web-js-function js "pgroup")) "a long group shows its first rows, then all of them on a tap")
    (loop for (selector property value) in
          '(("#app:not([data-pane=\"chat\"]) .dock" "display" "none")
            (".sheet.drawer" "height" "88dvh")
            (".card-diff pre.diff" "white-space" "pre-wrap")
            (".fs-row.fs-head" "display" "none"))
          do (is (equal value (web-css-value css selector property)) (format nil "~a { ~a: ~a }" selector property value)))
    (is (and sizes (every (lambda (each) (and (cdr each) (>= (cdr each) 11))) sizes))
        (format nil "no part's text under 11 px: ~{~a~^, ~}" (remove-if (lambda (each) (and (cdr each) (>= (cdr each) 11))) sizes)))))

;; A model or reasoning pick in the chat drawer is its session's own.
;; Without this, a drawer pick would move the default for every session that
;; follows and every scheduled job, and say nothing.
;; Control is the one place the page moves the default; the drawer says where
;; its pick lands and names the default; reasoning has a way back to Default;
;; the chip reads again when a pick lands anywhere and when the chat returns.

(defun web-function-text (text name)
  "The source of TEXT's top-level function NAME, up to the next top-level form."
  (let* ((at (or (search (format nil "~%async function ~a(" name) text)
                 (search (format nil "~%function ~a(" name) text)))
         (end (and at (search (format nil "~%}~%") text :start2 at))))
    (and at end (subseq text at end))))

(deftest web-cell-a-drawer-pick-lands-on-its-session-and-the-default ()
  ;; The chat's pick goes through the session route, which pins the session
  ;; and moves the default new sessions start on (SELECT-SESSION-MODEL);
  ;; Control's Settings moves the default alone.
  (let* ((app (web-page-text "app.js"))
         (html (web-page-text "index.html"))
         (choose (web-function-text app "chooseModel"))
         (submit (web-function-text app "submit"))
         (models (web-function-text app "renderModels")))
    (is (search "api(\"POST\", \"/api/gateway/model\"" (web-page-text "control.js")) "Control moves the default")
    (is (not (search "\"/api/gateway/model\"" app)) "and the chat never does")
    (is (and choose (search "/model`" choose) (search "if (!session)" choose)
             (< (search "if (!session)" choose) (search "api(" choose))) "a pick on the opener waits for the session, one in a session pins it")
    (is (and submit (search "landAim(session)" submit)
             (< (search "landAim(session)" submit) (search "sendPrompt(" submit))) "the opener's pick lands on the new session before its first message")
    (is (and models (search "t(\"This session\")" models) (search "t(\"The new session\")" models)
             (search "becomes the default for new sessions" models)) "the drawer says where a pick lands")
    (is (and models (search "t(\"Default (the model's own)\")" models)
             (search "effort: \"\"" models)) "reasoning's first row is Default, and it clears the pick")
    (is (search "id=\"pickscope-go\"" html) "the drawer says where the default changes")
    (is (search "case \"model_changed\": if (!payload.session_id || payload.session_id === state.current) loadTarget();" app) "the chip reads again when a pick or the default moves anywhere")
    (is (search "if (pane === \"chat\") { schedule(); loadTarget(); }" app) "and when the chat comes back from a board")))

;; A Control form's save. A refused one keeps what was typed and says why under
;; the form, the hand put back on the field the gateway names: control.js's ACT
;; does that for a save that names its form (FORMKEY), and a toast is left for a
;; control that is not a form. This reads every submit handler of the pane's
;; tabs and holds each save it starts to naming its form; the MCP and Skills
;; tabs say their own refusals where the write was made, and are not read. A
;; Retry repeats the same request, so the pane offers one only where the gateway
;; never answered: never beside a refusal.

(defun web-js-literal-end (text start)
  "Where the string literal or template whose quote opens at START in TEXT
ends: past its closing quote."
  (let ((quote (char text start)) (at (1+ start)))
    (loop (let ((char (char text at)))
            (cond ((char= char #\\) (incf at 2))
                  ((char= char quote) (return (1+ at)))
                  (t (incf at)))))))

(defun web-js-close (text open)
  "Where the bracket that opens at OPEN in TEXT closes, literals stepped over."
  (let ((depth 0) (at open))
    (loop (let ((char (char text at)))
            (if (find char "\"'`")
                (setf at (web-js-literal-end text at))
                (progn (cond ((find char "({[") (incf depth))
                             ((and (find char ")}]") (zerop (decf depth))) (return at)))
                       (incf at)))))))

(defun web-js-calls (text name)
  "Each call of the function NAME in TEXT, whole, from its name to its parenthesis."
  (loop for at = (search name text) then (search name text :start2 (1+ at))
        while at
        unless (let ((before (char text (1- at)))) (or (alphanumericp before) (find before "_$.")))
          collect (subseq text at (1+ (web-js-close text (+ at (length name) -1))))))

(deftest web-cell-control-forms-keep-their-refusal-under-them ()
  (dolist (file '("control.js" "settings.js" "access.js" "profiles.js"))
    (let* ((text (web-page-text file))
           (open (position #\{ text :start (search "function submit(" text)))
           (body (subseq text open (1+ (web-js-close text open))))
           (saves (loop for name in '("act(" "jobOp(" "pair(") append (web-js-calls body name)))
           (bare (remove-if (lambda (call) (uiop:string-suffix-p call ", formKey(node))")) saves)))
      (is saves (format nil "~a's submit starts a save" file))
      (is (null bare) (format nil "every save ~a's forms start names its form: ~{~a~^; ~}" file bare))))
  (let ((retries (loop for file in '("control.js" "settings.js" "mcp.js" "index.js" "profiles.js" "access.js")
                       for text = (web-page-text file)
                       append (loop for at = (search "retry:" text) then (search "retry:" text :start2 (1+ at))
                                    while at collect (subseq text at (min (length text) (+ at 30)))))))
    (is retries "the pane offers a Retry somewhere")
    (is (every (lambda (retry) (search "unreached(" retry)) retries) "only where the gateway never answered")))

;; What rides a prompt. A picture sent with it is kept by the gateway with the
;; prompt (exec.lisp PROMPT-IMAGES), so the operator's own line draws it from
;; the row it is sent, live and after a reload alike, and a rewind or a Retry
;; fetches it back by the id that row names: the tab keeps no copy of its own.
;; Any other file waits in the box, its chip saying where it will land, and
;; goes up at Send; the chip's x stops one on its way, and one that did not go
;; up offers Retry and says, where Send is, why the box will not send.

(defun web-js-function (text name)
  "The source of the top-level function NAME in TEXT, from its head to the
brace that closes it at the start of a line, or NIL."
  (let ((at (search (format nil "function ~a(" name) text)))
    (when at
      (subseq text at (+ 2 (search (format nil "~%}~%") text :start2 at))))))

(defun web-js-listener (text element)
  "The source of the click listener TEXT adds to the element ELEMENT."
  (let ((at (search (format nil "$(\"~a\").addEventListener(\"click\"" element) text)))
    (when at
      (subseq text at (+ 3 (search (format nil "~%});~%") text :start2 at))))))

(deftest web-cell-a-picture-sent-stays-in-the-operator-s-line ()
  (let* ((js (web-page-text "app.js"))
         (ask (web-js-function js "drawAsk")))
    (is (search "turn.prompt.metadata?.images" ask) "the operator's line reads the pictures its row names")
    (is (search "figureOf(image, image.id)" ask) "each drawn as a figure fetched by its id, whole on a click")
    (is (search "promptPictures(" (web-js-function js "retry")) "a Retry sends the pictures back with the words")
    (is (search "putBackPictures(" (web-js-function js "navigated")) "a rewind puts them back in the box")
    (is (search "no longer on disk" (web-js-function js "putBackPictures")) "and says so of one it cannot")
    (is (null (search "state.prompted" js)) "the tab keeps no copy of a sent picture")))

(deftest web-cell-a-file-goes-up-at-send-where-its-chip-says ()
  (let* ((js (web-page-text "app.js"))
         (files (web-page-text "files.js"))
         (pick (web-js-function js "addFile"))
         (up (web-js-function js "putUp"))
         (send (web-js-function js "filesUp"))
         (chips (web-js-listener js "attachments"))
         (strays (loop for at = (search "putUp(" js) then (search "putUp(" js :start2 (1+ at))
                       while at
                       unless (loop for part in (list up send chips)
                                    for start = (search part js)
                                    thereis (<= start at (+ start (length part))))
                         collect at)))
    (is (and pick (null (search "putUp(" pick))) "picking a file writes nothing")
    (is (search "placeFiles(" pick) "it reads where the file will land")
    (is (search "placeFiles(files, session, true)" (web-js-function js "submit")) "and again at Send, which holds when the place moved")
    (is (null strays) "a file goes up from Send or its chip's Retry, nowhere else")
    (is (search "entry.stop()" chips) "the x stops a file on its way")
    (is (and (search "signal?.addEventListener(\"abort\"" files) (search "xhr.abort()" files)) "and putFile stops its bytes")
    (is (search "\"attach-retry\"" (web-js-function js "paintFile")) "a file that did not go up offers Retry on its chip")
    (is (search "aria-describedby" (web-js-function js "renderComposer")) "and Send is described by why it will not send")
    (is (search "Retry or remove {name} to send" (web-js-function js "attachWhy")) "in words")))

;; The link outside its tab. A browser asking is a notice that holds, with the
;; code and Allow and Deny, over the others until it is answered or out of
;; time, and the foot's mark shows it at once: every notice reads the link
;; again (the cell's own reading, never the notice's words). A link whose
;; dials keep failing says so with a Try now, and each state reads still, not
;; by motion alone.

(deftest web-cell-a-browser-asking-holds-a-notice-and-the-mark-shows-it ()
  (let* ((app (web-page-text "app.js"))
         (access (web-page-text "access.js"))
         (css (web-page-text "app.css"))
         (receive (web-js-function app "receive"))
         (notice (and receive (search "case \"organism.notice\":" receive))))
    ;; A notice reads the link again.
    (is (and notice (search "readLinkSoon();" receive :start2 notice)
             (< (search "readLinkSoon();" receive :start2 notice) (search "break;" receive :start2 notice))))
    (is (search "control.link.read().then(paintLink)" (web-js-function app "readLinkSoon")))
    (is (search "paintAsk();" (web-js-function app "paintLink")) "a reading paints the ask where the operator is")
    (is (search "control.link.askNotice()" (web-js-function app "paintAsk")))
    ;; A flood of news never takes the ask off the stack.
    (is (search "!each.matches(\".error, .link-ask\")" (web-js-function app "toast")))
    (is (search "function askNotice()" access))
    (is (search "(seen() || c.link?.ask)" access) "an ask is read again while it waits, so one answered elsewhere leaves")
    (is (search "failing: [t(\"Failing.\")" access) "a failing link has its own state")
    (is (search "act: \"link-retry\"" access) "and a Try now")
    ;; Failing is a hollow mark and asking a ringed one: each reads still,
    ;; with motion reduced too.
    (is (cl-ppcre:scan "(?s)\\.link-mark\\[data-state=\"failing\"\\]::after \\{[^}]*border: 1.5px solid var\\(--danger-lit\\)" css))
    (is (cl-ppcre:scan "(?s)\\.link-mark\\[data-asking\\]::after \\{[^}]*border: 1.5px solid var\\(--accent\\)" css))))

;; The slash reply. A command the catalog says acts on no session runs from the
;; opener as it is, and no untitled session is left behind; the panel it
;; answers with is drawn under its line, a row's value one click from the box and
;; a row's line one click from running.

(deftest web-cell-a-sessionless-command-runs-without-a-session-and-shows-its-panel ()
  (let* ((app (web-page-text "app.js"))
         (run (web-js-function app "runSlash"))
         (panel (web-js-function app "slashPanel")))
    (is (search "slashCommand(line)?.session !== false" run) "the catalog's word decides")
    (is (search "(needs ? await newSession(state.folder) : null)" run) "only a command that needs one opens a session")
    (is (search ", \"\", answer?.dialog);" run) "the panel rides the reply")
    (is (search "qrCode(dialog.picture.rows)" panel) "its picture")
    (is (search "pick.dataset.value = row.value" panel) "its rows")
    (is (search "pick.dataset.run = run" panel) "a row's line to run")
    (is (search "if (pick.dataset.run) { runSlash(pick.dataset.run); return; }" app) "runs on a click")
    (is (search "id=\"slash-panel\"" (web-page-text "index.html")))))

;; A phone opens Control on its sections as one list, each saying how it
;; stands, and a section as a page of its own under the way back.
(deftest web-cell-control-on-a-phone-is-a-list-of-its-sections ()
  (let* ((js (web-page-text "control.js"))
         (app (web-page-text "app.js"))
         (css (web-css-declarations (web-page-text "app.css")))
         (groups (subseq js (search "const GROUPS" js) (search "const MARKS" js)))
         (draw (web-js-function js "draw")))
    (is (search "matchMedia(\"(max-width: 720px)\")" js) "at the page's own phone width")
    (dolist (tab '("gateway" "logs" "settings" "cells" "index" "mcp" "profiles" "channels" "link" "jobs"))
      (is (search (format nil "\"~a\"" tab) groups) (format nil "the list reaches ~a" tab)))
    (is (search "if (!phone && !WIDE.matches) {" draw) "a phone draws no strip of tabs, nor a wide window, whose list holds them")
    (is (search "if (atHome()) {" draw) "but the list, until a section is opened")
    (is (search "get tab() { return atHome() ? null : c.tab; }" js) "the list's address names no tab")
    (is (search "control.moves(tab)" (web-js-function app "setPane")) "so Back, or the column's Control, takes a phone to the list")
    (is (equal "54px" (web-css-value css ".ctl-go" "min-height")) "a section's row is a finger tall")
    (is (equal "fixed" (web-css-value css ".ctl-phone .confirm" "position")) "a question rises from the bottom")))

;; A wide window is one skeleton on every page (the desktop audit's variant B,
;; the operator, 2026-10-19: "Land B"). Measured before it at 1440 and 1920 px:
;; three left edges, three content widths (720, 880, 1120), four head layouts,
;; Settings' ten tabs in one flat strip, the column's head reading Sessions 0
;; while the list was still being read. From 1024 px: a rail (the phone's tabs
;; on their side), beside it one list of what is in the place the rail lit (the
;; sessions; the Dashboard's sections; Settings' sections under their groups,
;; saying how each stands), and a page whose head, body and box share one frame.
;; Provider keys (the provider keys variants artifact's A with C's strip; the
;; operator, 2026-10-20: "Land your pick A with C needs fixing strip"). What is
;; wrong comes first, the kit's banner with its Forget, which goes through the
;; keys route by the name the gateway says (a pasted key's masked); the ready
;; keys are one ledger of aligned columns, as wide as it reads; a provider with
;; no key is a chip.
(deftest web-cell-provider-keys-say-what-needs-fixing-first ()
  (let* ((control (web-page-text "control.js"))
         (keys (web-js-function control "drawKeys"))
         (css (web-css-declarations (web-page-text "app.css"))))
    (is (search "root.append(drawKeys(), ...cfgs.blocks());" (web-js-function control "drawSettings")) "Config draws the keys")
    (is (search "keys.append(banner(\"need\", what, why, go));" keys) "what is wrong is the kit's banner")
    (is (< (search "banner(" keys) (search "ctl-keys-ready" keys)) "over the list")
    (is (search "{ provider: d.id, forget: true }" control) "Forget goes through the keys route")
    (is (equal "grid" (web-css-value css ".control-pane:not(.ctl-phone) .ctl-keys-ready > .ctl-row" "display")) "the ready keys are a ledger")
    (is (equal "860px" (web-css-value css ".control-pane:not(.ctl-phone) .ctl-keys-block" "max-width")) "as wide as it reads")
    (is (search "dashed" (web-css-value css ".control-pane:not(.ctl-phone) .ctl-keys-none > .ctl-row" "border")) "a provider with no key is a chip")))

(deftest web-cell-a-wide-window-has-a-rail-a-list-and-a-reading-column ()
  (let* ((html (web-page-text "index.html"))
         (app (web-page-text "app.js"))
         (control (web-page-text "control.js"))
         (text (web-page-text "app.css"))
         (at (search "/* --- a wide window: the rail, its list, a reading column" text))
         (block (and at (subseq text at (search (format nil "~%}~%") text :start2 at))))
         (list (web-js-function app "paintList")))
    (is block "a wide window has its block")
    (when block
      (is (search "@media (min-width: 1024px)" block) "from 1024 px, where the column stops being a sheet")
      (is (search "grid-template-columns: 72px minmax(0, 1fr);" block) "a 72 px rail beside the list")
      (is (search "flex-direction: column; align-items: center; gap: 6px;" block) "the places stand down the rail")
      (is (search "#sessions-tab { display: flex; }" block) "Sessions is one of them, as on a phone")
      (is (search "#app:is([data-pane=\"dashboard\"], [data-pane=\"control\"]) .group { display: none; }" block) "a board's list stands where the sessions were")
      (is (search ".app { --lane: 1040px; }" block) "a board's frame")
      (is (search ".app { --read: 760px; }" block) "a chat's reading column")
      (dolist (rule '(".slab > .rule { display: none; }" ".ledger > .lane, .dock { max-width: var(--read); }"
                      ".ledger .prose > :is(p, ul, ol, blockquote, h1, h2, h3, h4, h5, h6) { max-width: none; }"
                      ".observe .lane { max-width: var(--lane); }"
                      ".slab:has(> .ledger > .opener:not([hidden])) .title-block { visibility: hidden; }"))
        (is (search rule block) (format nil "the wide window holds ~a" (subseq rule 0 (position #\{ rule)))))
      (is (not (search ".opener {" block)) "the opener keeps its centre")
      (is (search ".dashboard .dash-jumps { display: none; }" block) "the Dashboard's sections are the list's, not a menu's"))
    (is (search "id=\"place\"" (subseq html (search "id=\"column\"" html) (search "class=\"slab\"" html))) "the list is the column's")
    (is (search "export const WIDE = matchMedia(\"(min-width: 1024px)\");" control) "control.js names the same width")
    (is (search "const [words, tone] = glance(id);" (subseq control (search "places() {" control))) "each section says how it stands, as the phone's list does")
    (is (search "control.places()" list) "Settings' list is Control's")
    (is (search "JUMPS.map(" list) "the Dashboard's is its sections")
    (is (search "if (board()) setPane(\"chat\");" app) "the rail's Sessions goes back to the session")
    (is (search "$(\"count\").textContent = state.listed ? String(all) : \"\";" app) "no count before the list is read")))

;; A page names its place once and opens with the one head.
;; The strip carries Control / the tab (Control the way back on a
;; phone); the page opens with observe.js's headline, and no section
;; repeats the tab's own name.
(deftest web-cell-a-page-names-its-place-once ()
  (let* ((observe (web-page-text "observe.js"))
         (control (web-page-text "control.js"))
         (access (web-page-text "access.js"))
         (app (web-page-text "app.js"))
         (css (web-page-text "app.css"))
         (place (web-js-function app "paintPlace"))
         (pages (mapcar #'web-page-text '("control.js" "access.js" "index.js" "profiles.js" "mcp.js" "settings.js"))))
    (is (search "export function headline(micro, main, sub)" observe) "one head, shared")
    (is (search "if (micro) node.append" observe) "its label only when it has one")
    (dolist (draw '("drawHome" "drawGateway" "drawJobs"))
      (let ((at (search (format nil "  function ~a(" draw) control)))
        (is (and at (search "headline(" control :start2 at :end2 (search (format nil "~%  }~%") control :start2 at)))
            (format nil "~a opens with it" draw))))
    (is (search "const head = headline(" access) "and Link")
    (is (not (search "el(\"section\", \"headline\")" (concatenate 'string control access))) "no page builds its own")
    (is (not (search "phoneHead" control)) "no second title in the page")
    (is (not (search ".ctl-title" css)) "nor its style")
    (is (search "get label() { return atHome() ? null : LABELS[c.tab]; }" control) "the pane names its open tab")
    (is (search "control.label" place) "the strip says it")
    (is (search "control.back()" place) "its Control goes back to the list")
    (is (search "paintPlace();" (web-js-function app "renderHead")) "on every paint of the strip")
    (is (search "onTab: () => { writeLink(); paintPlace(); }" app) "and on every move between tabs")
    (dolist (label '("Cells" "Index" "Profiles" "Channels" "Scheduled jobs" "MCP servers"))
      (is (notany (lambda (js) (search (format nil "block(t(\"~a\")" label) js)) pages)
          (format nil "no section repeats ~a" label)))))

;; Every Control section on a phone follows the phone standard. One pass over
;; what each tab draws (control.js rise), so a module's tab follows it too: a
;; control a finger's size, words in sentence case, a row with one thing on its
;; right and the rest in its sheet, edits and questions rising from the bottom,
;; on/off a switch, filters segments, a long list searched first, fifty rows at
;; a time.
(deftest web-cell-every-control-section-on-a-phone-follows-the-standard ()
  (let* ((js (web-page-text "control.js"))
         (index (web-page-text "index.js"))
         (access (web-page-text "access.js"))
         (css (web-css-declarations (web-page-text "app.css")))
         (inner (lambda (name)
                  (let ((at (search (format nil "  function ~a(" name) js)))
                    (and at (subseq js at (search (format nil "~%  }~%") js :start2 at))))))
         (draw (funcall inner "draw"))
         (taps (funcall inner "tapRows"))
         (rise (funcall inner "rise")))
    (is (search "if (phone) rise(root);" draw) "every tab, a module's too, passes through the phone's one pass")
    (is (search "/(^|-)(ask|open)(-|$)/" taps) "a row's lone act is its tap only when it opens or asks: a restart or a run waits in the sheet")
    (is (search "const RISES = \".confirm, .ctl-inset, .ctl-rises, .ctl-cfg-section > .ctl-cfg-body\";" js) "a question, an opened row, an opened panel and a section's settings rise")
    (is (search "part.replaceWith(at);" rise) "a form standing in a section is a row that raises it")
    (is (search "const PAGE = 50;" js) "a long list shows fifty rows")
    (is (search "PHONE.matches ? shown.slice(0, s.shown) : shown" index) "and Index draws only those, never its two thousand")
    (is (search "pill(on, on ? t(\"On\") : t(\"Off\")" access) "the link's on and off is a switch")
    (is (search "pill(!paused" js) "and whether a job runs")
    (loop for (selector property value) in
          '((".ctl-phone :is(.quiet, .solid)" "min-height" "44px")
            (".ctl-phone .word" "min-height" "44px")
            (".ctl-phone .icon" "height" "44px")
            (".ctl-phone .pill, .ctl-phone .pill[aria-checked=\"true\"]" "height" "44px")
            (".ctl-phone :is(.quiet, .solid, .word, .sw)" "text-transform" "none")
            (".ctl-phone .ctl-row" "min-height" "54px")
            (".ctl-sheet" "position" "fixed")
            (".ctl-phone .ctl-list > li[hidden]" "display" "none"))
          do (is (equal value (web-css-value css selector property)) (format nil "~a { ~a: ~a }" selector property value)))
    (is (<= 16 (or (web-css-px css ".ctl-phone :is(input.control, select.control, textarea.control)" "font-size") 0)) "a phone's field is never under 16 px, so a tap never zooms the page")
    (is (notany (lambda (each) (and (search "ctl-phone" (car each)) (search "text-transform: uppercase" (cdr each)))) css) "no phone rule writes a control in capitals")))

;; A line that drops says where it broke. Five quick tries pass under the link
;; bar; after them a try every 15 s, and the sheet rises with the hops between
;; the tab and Nodecode -- this device, the relay when the page came down the
;; link, the machine -- each read where it answers (the device's online flag,
;; /healthz, the relay's x-uplink-machine header), Try again at its foot and
;; the command that starts Nodecode when the machine is the break. Try again
;; keeps the ladder's place, so the sheet stays up, and never dials past the gate.

(deftest web-cell-a-dropped-line-says-where-it-broke ()
  (let* ((app (web-page-text "app.js"))
         (html (web-page-text "index.html"))
         (trace (web-js-function app "traceLine"))
         (offline (web-js-function app "renderOffline"))
         (now (web-js-function app "reconnectNow")))
    (is (search "n < QUICK_TRIES ? Math.min(5000, 400 * 2 ** n) : 15000" app) "five quick tries, then one every 15 s")
    (is (search "state.retry > QUICK_TRIES" offline) "the sheet rises once the quick tries are spent")
    (is (search "navigator.onLine" trace))
    (is (search "fetch(\"/healthz\"" trace))
    (is (search "response.headers.get(\"x-uplink-machine\") === \"offline\"" trace) "the relay's offline page names the machine off its line")
    (is (search "location.reload()" trace) "a tab the link no longer lets in still reloads")
    (is (search "state.ws?.readyState !== WebSocket.CLOSED" now) "a try now only between tries, never past the gate")
    (is (not (search "state.retry = 0" now)) "and it keeps the ladder's place")
    (is (search "window.addEventListener(\"online\", reconnectNow)" app) "back online tries at once")
    (dolist (id '("offline" "offline-scrim" "offline-hops" "offline-retry" "offline-cmd" "offline-close"))
      (is (search (format nil "id=\"~a\"" id) html) id))
    (is (search "<code>nodecode gateway on</code>" html))))

;; An answer in the TUI's two hues: # and ## and a bold lead on a band that
;; fades out, bold on a warm chip that glints as it lands, code in two blues
;; by the TUI's CODE-SPANS rule. md.js's HTML is the shared goldens', so the
;; grounds are laid on the answer after it.

(deftest web-cell-an-answer-draws-in-two-hues ()
  (let* ((js (web-page-text "app.js"))
         (draw (web-js-function js "drawWork"))
         (duo (web-js-function js "duotone"))
         (css (web-page-text "app.css"))
         (script (format nil "~a~%for (const s of [\"2ba5acc9b\", \"src/tui/ui.lisp\", \"nle::cell-off-refusal\", \"nodecode gateway on\", \"rooms/nodecode-web/\", \"{a,b}\"]) { const { path, quiet } = codeQuiet(s); console.log((path ? \"path \" : \"code \") + [...s].map((c, i) => (quiet[i] && !quiet[i - 1] ? \"[\" : \"\") + c + (quiet[i] && !quiet[i + 1] ? \"]\" : \"\")).join(\"\")); }" (web-js-function js "codeQuiet")))
         (marks (uiop:run-program (list "node" "-e" script) :output :string)))
    (is (search "duotone(prose);" draw) "an answer is drawn in the two hues")
    (is (search "if (answer.live) glint(prose, turn.key);" draw) "a running answer's new bold runs glint")
    (is (search "\":scope > :is(h1, h2)\"" duo) "# and ## stand on a band")
    (is (search "p.firstChild?.nodeName !== \"STRONG\"" duo) "a paragraph that opens on a bold run leads on one")
    (is (search "Math.max(8, Math.min(36, Math.ceil(band.textContent.length / 2)))" duo) "a band fades over half its words, 8 to 36 cells")
    (is (string= marks (format nil "code [2ba5acc9b]~%path [src/tui/]ui.lisp~%code [nle::]cell-off-refusal~%code nodecode gateway on~%path [rooms/]nodecode-web[/]~%code [{]a[,]b[}]~%")) "code is two blues: an id quiet, a path's folders, a name's package and punctuation")
    (is (search "linear-gradient(90deg, var(--band-ground) calc(100% - var(--tail) + 0.6em), transparent)" css) "a band fades into the page, never a box")
    (is (search ".answer.prose { overflow-x: clip; }" css) "and never passes the answer's edge")
    (is (search ".answer.prose :where(.inline-color strong) { background: none; padding: 0; }" css) "a model's own color keeps no chip")))

;; Sessions on a phone are a page of their own (variant A of the column).
;; Without this, the sessions list would sit below the place rows, most
;; rows wearing the news dot, most rows carrying scheduled jobs, and a
;; room's threads behind a row. The list reads as an inbox: its sources
;; as chips, three bands (running, waiting on you, earlier), a room's
;; threads each a row, the jobs under their own chip, a row its name and
;; where it is held; on a phone the column fills the screen with the
;; search in its top bar and the places as tabs along the foot, New over
;; them.
(deftest web-cell-sessions-on-a-phone-are-a-page-with-tabs ()
  (let* ((html (web-page-text "index.html"))
         (app (web-page-text "app.js"))
         (css (web-page-text "app.css"))
         (render (web-js-function app "renderSessions"))
         (phone (search "/* --- variant A on a phone: Sessions is a page of its own" css)))
    (is (search "id=\"sessions-tab\"" html) "Sessions is a place beside Dashboard and Control")
    (is (search "id=\"sessions-badge\"" html) "its badge counts what waits")
    (is (search "class=\"chips\" id=\"sources\" role=\"radiogroup\"" html) "the sources are chips")
    (is (not (search "<select id=\"source\"" html)) "not a select behind a funnel")
    (is (search "id=\"link-open\"" html) "More reaches Link on a phone")
    (is (search "[[\"ask\", t(\"Needs you\")], [\"run\", t(\"Running\")], [\"done\", t(\"Done\")]]" render) "three bands: needs you, running, done")
    (is (search "sourceOf(entry) !== \"cron\"" render) "the jobs stand under their own chip")
    (is (search "folds(entry.id).every((up) => state.branches.has(up))" render) "a room's threads are rows of their own")
    (is (search "whereOf(entry)" render) "a row says where it is held")
    (is (search "$(\"sessions-badge\").hidden = !asks;" render) "the badge shows only while something waits on you")
    (is phone "a phone has the page")
    (when phone
      (let ((block (subseq css phone)))
        (is (search ".column-head, .group, .group-head { display: contents; }" block) "one bar on top: the mark, the name, More")
        (is (search ".group-head .micro {" block) "the page's name in it")
        (is (search ".finder { grid-area: 2 / 1 / 3 / -1;" block) "the search under it")
        (is (search "position: fixed; left: 0; right: 0; bottom: 0;" block) "the places along the foot")
        (is (search "#new-session {" block) "New over them")
        (is (search "border-radius: 50%; background: var(--accent-fill)" block) "a round +")
        (is (search "#count, .column .gateway { display: none; }" block) "no count beside the name, no footer")))
    (is (search "#sessions-tab, .phone-only { display: none; }" css) "wider, the column stays a column")))

;; Every page on a phone has the Sessions page's bar: three slots -- the way
;; back or the mark, the page's name once, More (the session's Chat, Run and
;; Files heading its acts, the one on screen lit) -- and the tabs along the foot on Dashboard and Control too.
;; The mark is the site's axolotl, drawn once, in the text's own colour.
(deftest web-cell-every-page-on-a-phone-has-one-bar ()
  (let* ((html (web-page-text "index.html"))
         (app (web-page-text "app.js"))
         (css (web-page-text "app.css"))
         (place (web-js-function app "paintPlace"))
         (at (search "/* --- variant A on a phone: one bar on every page" css))
         (block (and at (subseq css at (search (format nil "~%}~%") css :start2 at)))))
    (is (= 1 (length (cl-ppcre:all-matches-as-strings (cl-ppcre:quote-meta-chars "<symbol id=\"axolotl\"") html))) "the animal is drawn once")
    (is (= 2 (length (cl-ppcre:all-matches-as-strings (cl-ppcre:quote-meta-chars "<use href=\"#axolotl\"/>") html))) "and stands in the column's head and in the strip")
    (is (search "fill-opacity=\"0.4\"" html) "in the text's colour, a step an opacity")
    (is (search "id=\"strip-home\"" html) "a top page's mark")
    (is (search "[\"strip-home\", openNew]" app) "which opens a new session, as the column's does")
    (is (search "el(\"span\", \"crumb-word\", t(\"Settings\"))" place) "a Settings tab's crumb keeps its word for a wide strip")
    (is (search "else app.classList.add(\"column-open\");" app) "the Sessions tab opens the Sessions page from a board")
    (is block "a phone has the bar")
    (when block
      (is (search "grid-template-columns: 44px minmax(0, 1fr) 44px;" block) "three slots")
      (is (search ".strip h1 .crumb-word, .strip h1 .sep { display: none; }" block) "a tab named once")
      (is (search ".strip > .views { display: none; }" block) "no switch in the bar: Chat and Run are under More")
      (is (search ".head-acts .view-act { display: none; }" css) "and a phone's alone: wider, the switch holds them")
      (is (search "for (const act of $(\"head-acts\").querySelectorAll(\"[data-view]\"))" app) "the menu's views lit by the pane on screen")
      (is (search "$(\"more\").toggleAttribute(\"data-flagged\", flagged && state.pane !== \"run\");" app) "More carries Run's flag while Run is shut")
      (is (search "const flip = host === $(\"views\") && sw.getAttribute(\"aria-checked\") === \"true\";" app) "wider, a press on the switch's lit side flips it")
      (let ((switch (subseq html (search "id=\"views\"" html) (search "id=\"more\"" html)))
            (acts (subseq html (search "id=\"head-acts\"" html) (search "id=\"status\"" html))))
        (is (and (search "data-view=\"chat\"" switch) (search "data-view=\"run\"" switch)
                 (not (search "data-view=\"files\"" switch))) "the switch holds Chat and Run")
        (is (and (search "id=\"chat-open\" type=\"button\" data-view=\"chat\"" acts)
                 (search "id=\"run-open\" type=\"button\" data-view=\"run\"" acts)
                 (search "id=\"files-open\" type=\"button\" data-view=\"files\"" acts)) "Chat, Run and Files are acts, under More"))
      (is (search "document.querySelectorAll(\".strip [data-view]\")" app) "and still a view an address may name")
      (is (search ":not(.column-open) .railed { visibility: visible; }" block) "the tabs on Dashboard and Control")
      (is (search ":not(.column-open) #new-session { visibility: hidden; }" block) "New stays on Sessions"))
    (is (search ".glance-now { grid-template-columns: minmax(0, 1fr) 10px;" css) "Right now's door: its words and its chevron, no lamp column")
    (is (search "if (lead.dataset) lead.dataset.tone" (web-page-text "observe.js")) "a healthy fact's lead is no mark")))

;; The page draws from one scale.
;; Now :root holds the kit's scale -- six sizes, four corners, two
;; letter-spacings -- and every other rule names one of them. The answer's
;; reading scale (.prose: its headings a step apart, its tables and
;; fences) is the transcript's own and keeps its sizes.
(defun web-off-scale (declarations)
  "Each declaration of DECLARATIONS outside :root that writes a raw size,
corner or letter-spacing instead of naming the kit's scale."
  (loop for (rule . declaration) in declarations
        for colon = (position #\: declaration)
        for property = (and colon (string-trim " " (subseq declaration 0 colon)))
        for value = (and colon (string-trim " " (subseq declaration (1+ colon))))
        when (and property (not (eql 0 (search ":root" rule)))
                  (cond ((member property '("font" "font-size") :test #'string=)
                         (and (not (search ".prose" rule))
                              (cl-ppcre:scan "[0-9]px" (cl-ppcre:regex-replace-all "/\\S+" value ""))))
                        ((string= property "border-radius") (cl-ppcre:scan "[0-9]px" value))
                        ((string= property "letter-spacing")
                         (not (or (member value '("0" "normal" "inherit") :test #'string=)
                                  (eql 0 (search "var(--ls-" value)))))))
          collect (format nil "~a { ~a }" rule declaration)))

(deftest web-cell-the-page-draws-from-one-scale ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (root (loop for (rule . declaration) in css
                     when (string= rule ":root") collect declaration))
         (count (lambda (prefix) (count-if (lambda (d) (eql 0 (search prefix d))) root)))
         (off (web-off-scale css)))
    (is (= 6 (funcall count "--fs-")) "six sizes")
    (is (= 4 (funcall count "--r-")) "four corners")
    (is (= 2 (funcall count "--ls-")) "two letter-spacings")
    (is (equal "15px" (web-css-value css ":root" "--fs-body")) "the body is 15 px")
    (is (null off) (format nil "every rule names the scale: ~{~a~^; ~}" (subseq off 0 (min 8 (length off)))))))

;; Every place opens the same way (step 2 of the standard): under the strip that
;; names it, the one head -- a small label, one sentence of how it stands, a line
;; of detail -- never an icon, a button or a stats line first. Measured before
;; it: four places had the head, two their own, four opened on an (i) and a
;; count, four on nothing, Channels on its Restart button. Each owner draws its
;; head from what it read; Restart channels stands at the page's foot; the
;; dashboard's range follows its head (a phone's under its glance's hero).
(deftest web-cell-every-place-opens-with-its-head ()
  (let* ((control (web-page-text "control.js"))
         (observe (web-page-text "observe.js"))
         (channels (web-js-function control "drawChannels"))
         (dash (web-js-function observe "drawDashboard")))
    (dolist (each '("drawGateway" "drawLogs" "drawSettings" "drawCells" "drawChannels" "drawJobs"))
      (is (search "headline(" (web-js-function control each)) (format nil "~a opens with the head" each)))
    (loop for (file name) in '(("index.js" "draw") ("mcp.js" "head") ("profiles.js" "draw") ("files.js" "head") ("access.js" "draw"))
          do (is (search "headline(" (web-js-function (web-page-text file) name)) (format nil "~a's ~a opens with the head" file name)))
    (is (search "root.append(...head(l), servers(l)" (web-page-text "mcp.js")) "MCP's head stands over its servers")
    (is (not (search "el(\"span\", \"reading\"" (web-js-function control "drawCells"))) "the count moved into the head")
    (is (search "root.append(list, access.pairing(ch), foot);" channels) "Restart channels stands at the foot")
    (is (not (search "block(\"\", infoMark, button(" channels)) "and not over the list")
    (is (search "document.getElementById(\"control-log-state\")" (web-js-function control "drawLog")) "the log's head follows its lines")
    (is (search "root.append(dashHeadline(data, activity, range), ...(phone ? [] : [top])" dash) "the range follows the dashboard's head")
    (is (search "glance(parts, data, activity, phone ? top : null)" dash) "a phone's under its glance's hero")))

;; One button, one filter, one field, one banner (step 3 of the standard).
;; Measured before it: 23 button looks on a phone, the desktop's capitalised
;; mono buttons, four kinds of filter (chips, segmented, underlined capitals, a
;; tab strip), a mono field, and Needs you, the dashboard's alert and a failure
;; each drawn its own way. Now a button is solid, quiet or danger in one shape,
;; sentence case and sans at every width; every filter -- a range, a level, the
;; sources, the Control strip -- is the segmented control on its well; a field
;; is sans; and what wants the reader is the one banner in two tones with its act.
(deftest web-cell-a-button-a-filter-a-field-and-a-banner-are-one-each ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (control (web-page-text "control.js"))
         (observe (web-page-text "observe.js"))
         (value (lambda (selector property) (web-css-value css selector property))))
    (is (equal "500 var(--fs-body)/1 var(--sans)" (funcall value ".quiet, .solid" "font")) "a button is sans at the body's size")
    (is (equal "none" (funcall value ".quiet, .solid" "text-transform")) "never in capitals")
    (is (equal "var(--r-control)" (funcall value ".quiet, .solid" "border-radius")) "one corner")
    (is (notany (lambda (each) (and (cl-ppcre:scan "(^|[ ,])\\.(solid|quiet)\\b" (car each))
                                    (search "uppercase" (cdr each))))
                css) "no rule puts a button back in capitals")
    (is (equal "var(--fail)" (funcall value ".solid.danger" "background")) "danger is the one red")
    (is (search "background: var(--subtle)" (format nil "~{~a~^; ~}" (loop for (rule . d) in css when (string= rule ".scope") collect d))) "every filter stands on the well at every width")
    (is (equal "var(--selected)" (funcall value ".sw[aria-checked=\"true\"]" "background")) "its chosen segment lifted")
    (is (not (funcall value ".sw::after" "content")) "no underline under a chosen word")
    (is (equal "var(--selected)" (funcall value ".chips button[aria-checked=\"true\"]" "background")) "the sources are the same control")
    (is (search "var(--sans)" (funcall value ".control" "font")) "a field is sans")
    (is (search "export function banner(tone, what, why, act)" observe) "one banner")
    (is (search "rows.push(banner(tone, text, detail, go));" (web-js-function control "needsYou")) "Needs you is the banner")
    (is (search "banner(trouble.tone === \"danger\" ? \"fail\" : \"need\"" observe) "so is the dashboard's alert")
    (is (search "banner(\"fail\", plural(failed" (web-js-function control "drawCells")) "and a place's failure")
    (is (equal "var(--fail)" (funcall value ".banner[data-tone=\"fail\"], .banner[data-tone=\"danger\"]" "--tone")) "its failed tone")))

;; Words (step 4 of the standard): capitals belong to the 11 px micro label
;; alone, and every rule that sets them sets it whole (Martian Mono, 500, the
;; micro size and spacing); a word button speaks in sentence case; help, notes
;; and what a page says are sans -- mono is for literal machine text; a head's
;; sentence is the state size at every width; and a fact's field name leaves the
;; screen for its tooltip. Measured before it: 7 capital styles on a phone, help
;; and placeholders in mono, raw keys (started_at) under the Gateway's values.
(deftest web-cell-the-page-speaks-in-words ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (rules (let ((table (make-hash-table :test #'equal)))
                  (loop for (rule . declaration) in css
                        unless (search ".prose" rule)
                          do (push declaration (gethash rule table)))
                  table))
         (capitals (loop for rule being the hash-keys of rules using (hash-value declarations)
                         when (member "text-transform: uppercase" declarations :test #'string=)
                           unless (and (some (lambda (d) (cl-ppcre:scan "^font:\\s*500 var\\(--fs-micro\\)(/[0-9.]+)? var\\(--micro\\)$" d)) declarations)
                                       (member "letter-spacing: var(--ls-micro)" declarations :test #'string=))
                             collect rule))
         (value (lambda (selector property) (web-css-value css selector property))))
    (is (null capitals) (format nil "capitals only in the micro label's style: ~{~a~^, ~}" capitals))
    (is (equal "none" (funcall value ".word" "text-transform")) "a word button in sentence case")
    (is (search "var(--sans)" (funcall value ".word" "font")) "and sans")
    (dolist (help '(".obs-note" ".ctl-notice" ".ctl-sub" ".ctl-cfg-doc" ".toast" ".finder input" ".zoom-bar .hint"))
      (is (notany (lambda (d) (search "var(--mono)" d)) (gethash help rules)) (format nil "~a leaves mono" help)))
    (is (search "var(--fs-state)" (funcall value ".display" "font")) "a head's sentence is the state size")
    (is (search "if (tag) row.title = tag;" (web-js-function (web-page-text "control.js") "fact")) "a field's name is a tooltip")))

;; Colour marks exceptions only (step 5 of the standard): one dot(kind) draws a
;; row's mark -- running red (the operator, 2026-10-21: green done, orange
;; waiting on you, red running), act here the accent, failed the danger red,
;; off a hollow ring -- and a healthy row carries none: a connected channel, a ready server,
;; a loaded cell, a running profile. An on switch is white (the solid tone), so
;; orange keeps its one meaning. Measured before it: orange on connected, on,
;; default and needs you alike; every healthy row of Cells, MCP, Channels,
;; Index and Profiles wore the accent dot.
(deftest web-cell-colour-marks-exceptions-only ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (value (lambda (selector property) (web-css-value css selector property)))
         (observe (web-page-text "observe.js")))
    (is (search "export function dot(kind)" observe) "one mark")
    (is (search "kind ? el(\"i\", `led ${kind}`) : document.createTextNode(\"\")" (web-js-function observe "dot")) "a healthy row carries none")
    (dolist (file '("control.js" "index.js" "mcp.js" "profiles.js" "access.js"))
      (is (not (search "el(\"i\", `led${" (web-page-text file))) (format nil "~a marks rows through dot" file)))
    (is (equal "var(--run)" (funcall value ".led.live" "background")) "running is red")
    (is (equal "var(--live)" (funcall value ".led.ok" "background")) "what answers is green")
    (is (equal "var(--accent-fill)" (funcall value ".led.act" "background")) "act here is the accent")
    (is (equal "var(--fail)" (funcall value ".led.fail" "background")) "failed is red")
    (is (equal "transparent" (funcall value ".led.off" "background")) "off is a hollow ring")
    (is (search "\"fail\" : r.on && r.level === \"started\" ? \"\" : \"off\"" (web-js-function (web-page-text "control.js") "cellRow")) "a loaded cell has no dot, a failed one the red")
    (is (equal "var(--primary)" (funcall value ".pill[aria-checked=\"true\"]" "background")) "an on switch is white")))

;; Every place sits on the kit (step 6 of the standard, its proof sweep at 390
;; and 1440 px in both themes): what the sweep found off it is closed. A phone's
;; Run and its turns open on the one head, not a 32 px title of their own; the
;; column's search is a field at the body's size (a finger's: the title size, so
;; a phone does not zoom it); a running thing's lamp is the green that means
;; running wherever it stands (an answer, the live verb, a command, a session),
;; one that has ended the hollow ring; a hop that answers and a link that is back
;; are green, not the accent. (Running was green until the operator's balls,
;; 2026-10-21: it is the red now, green is done and what answers.)
(deftest web-cell-every-place-sits-on-the-kit ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (observe (web-page-text "observe.js"))
         (app (web-page-text "app.js")))
    (is (not (search "phoneTitle" observe)) "no title of a phone's own")
    (is (not (search ".p-title" (web-page-text "app.css"))) "nor its rules")
    (is (search "[headline(t(\"This session\")" (web-js-function observe "phoneRun")) "a phone's Run opens on the head")
    (is (search "headline(t(\"This session\")" (web-js-function observe "phoneTurn")) "and so does its turn")
    (is (equal "var(--fs-body) var(--sans)" (subseq (web-css-value css ".finder input" "font") 0 26)) "the search is a field at the body's size")
    (is (<= 16 (or (web-css-px css ".finder input" "font-size") 0)) "and under a finger large enough not to zoom")
    (is (search "pulse ? \"led live pulse\" : \"led off\"" (web-js-function app "lamp")) "running is red, ended the ring")
    (is (search "hop === \"ok\" ? \"led ok\"" app) "a hop that answers is green")
    (is (equal "var(--live)" (web-css-value css ".linkbar.back .led" "background")) "a link that is back is green")))

;; Every session wears a ball (the operator, 2026-10-21: "green, orange or red
;; ... like the kanban board", the artifact's B): red and pulsing while a turn
;; runs in it or under it, orange in a halo while its answer waits on you --
;; the gateway's `waits', marked by the turn or a newest turn that failed --
;; green and still once done. The list reads as the board does: Needs you,
;; Running, Done, Done its newest few until Show all; a chip a state lists it
;; alone; the Sessions badge counts what needs you. Measured before it: only a
;; running row had a mark, five pixels of green, and `Waiting on you' meant
;; unread.
(deftest web-cell-every-session-wears-its-ball ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (value (lambda (selector property) (web-css-value css selector property)))
         (app (web-page-text "app.js"))
         (html (web-page-text "index.html"))
         (render (web-js-function app "renderSessions")))
    (is (search "state.running.has(entry.id) || holding.has(entry.id) ? \"run\" : entry.waits ? \"ask\" : \"done\"" render) "what a session is doing: running, waiting on you, done")
    (is (search "button.append(ball(now));" render) "every row wears its ball")
    (is (search "el(\"i\", `ball is-${kind}`)" (web-js-function app "ball")) "one ball, its state its class")
    (is (search "else if (now === \"ask\") meta.append(el(\"span\", \"ses-asks\"" render) "a waiting row says on what")
    (is (search "waits?.why === \"failed\"" app) "or that its last turn failed")
    (is (equal "var(--run)" (funcall value ".ball.is-run" "background")) "running is the red")
    (is (search "ball-beat" (funcall value ".ball.is-run" "animation")) "and pulses")
    (is (equal "var(--accent-fill)" (funcall value ".ball.is-ask" "background")) "waiting on you is the accent")
    (is (search "0 0 0 3px" (funcall value ".ball.is-ask" "box-shadow")) "in a halo")
    (is (equal "var(--live)" (funcall value ".ball.is-done" "background")) "done is the green")
    (is (equal "-1" (funcall value ".sessions .row > .ball" "order")) "the ball leads its row")
    (is (equal "1px solid var(--seam-lit)" (funcall value ".sessions .ses-band:not(:first-child)" "border-top")) "a band below another stands apart under its rule")
    (is (not (search "li.append(ball(key))" render)) "a band head wears no ball: the balls are the rows'")
    (is (search "class=\"chips states\" id=\"states\" role=\"radiogroup\"" html) "a chip a state, the sources' control")
    (is (search "state.doing && state.doing !== key" render) "a chosen state lists its band alone")
    (is (search "at < DONE_ROWS || block.rows.some((entry) => entry.id === state.current)" render) "Done its newest few and the session on screen, until Show all")
    (is (search "$(\"sessions-badge\").textContent = String(asks);" render) "the badge counts what needs you")))

;; The operator, 2026-10-21: "I would like to be able to delete this, add a
;; delete knob or something for a conversation" -- deleting took the Sessions
;; menu's Select, a tick and its trash; a row now carries a trash of its own.
(deftest web-cell-a-session-row-deletes-its-own-session ()
  (let* ((css (web-css-declarations (web-page-text "app.css")))
         (value (lambda (selector property) (web-css-value css selector property)))
         (app (web-page-text "app.js"))
         (render (web-js-function app "renderSessions"))
         (click (web-js-listener app "sessions")))
    (is (search "iconButton(\"trash\", t(\"Delete this session, and every session under it\"), \"icon drop ses-del\")" render) "a stored session's row carries its own trash mark")
    (is (search "if (!state.picking && !entry.standby)" render) "none on a session not yet spoken in, nor in Select, whose ticks delete")
    (is (search "panel.append(...deleteAsk(state.dropping));" render) "what it asked stands under its row, in Select's words")
    (is (search "`${sessionPath(id)}?dry_run=1`" (web-js-function app "askDrop")) "the mark asks the gateway first, a dry run")
    (is (search "deleteAnswered(ask.answers" (web-js-function app "drop")) "Delete deletes what the dry run named")
    (is (search "deleteAnswered(ask.answers" (web-js-function app "deletePicked")) "the one delete Select's ticks take too")
    (is (search "if (mark) { askDrop(mark.dataset.id); return; }" click) "pressing the mark opens no session")
    (is (equal "0" (funcall value ".sessions .ses-del" "opacity")) "the mark waits out of sight")
    (is (equal "1" (funcall value ".sessions li:is(:hover, :focus-within) > .ses-del, .sessions .ses-del[aria-expanded=\"true\"]" "opacity")) "and comes up under the hand or the focus")
    (is (equal "1" (funcall value ".sessions li > .ses-del" "opacity")) "and stands up where there is no hover")
    (is (search "\"Delete this session, and every session under it\":" (web-page-text "zh.js")) "its words in Chinese")))
