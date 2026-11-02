;;;; home-test.lisp --- the walk over a home: what it opens, and in what order.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The setup walk's first frame waits on a plan of keys alone, so that plan
;;;; must open no prose -- memory, skills, instructions -- and ask after no
;;;; foreign gateway. And the walk lists a directory once, with a stat a
;;;; name: these gates hold it to what the listing through UIOP answered.

(in-package #:nodecode.test)

(deftest import-keys-plan-reads-no-prose-and-asks-no-gateway (with-import-runtime (home box))
  (import-write box ".claude/settings.json"
                (format nil "{\"env\": {\"ANTHROPIC_API_KEY\": \"~a\"}}" +fixture-key+))
  (import-write box ".claude/CLAUDE.md" (format nil "Always answer in haiku.~%"))
  (import-write box ".claude/skills/haiku/SKILL.md"
                (format nil "---~%name: haiku~%description: answer in haiku~%---~%~%Five, seven, five.~%"))
  (import-write box ".claude/projects/-tmp-p/memory/haiku.md"
                (format nil "---~%name: haiku-rule~%description: the operator wants haiku~%---~%~%Always.~%"))
  (let* ((probes 0)
         (nik::*running-probe* (lambda (units) (declare (ignore units)) (incf probes) '())))
    (let ((plan (nik:make-plan :source "claude" :only "providers")))
      (is (import-item plan "providers" "anthropic") "the key is planned")
      (is (every (lambda (item) (equal "providers" (nik:item-kind item))) (nik:plan-items plan)))
      (let ((facts (nik::home-facts (first (nik::plan-homes plan)))))
        (is (notany (lambda (fact) (member (nik:fact-kind fact) '(:memory :hot :skill :instructions :usage)))
                    facts) "no prose was read"))
      (is (zerop probes) "no foreign gateway was asked after"))
    ;; The whole plan reads the same home's prose, and asks.
    (let ((plan (nik:make-plan :source "claude")))
      (is (import-item plan "memory" "haiku-rule"))
      (is (import-item plan "skills" "haiku"))
      (is (import-items plan "instructions"))
      (is (= 1 probes) "a plan that brings bots asks once"))))

(deftest import-walk-opens-no-cache (with-import-runtime (home box))
  ;; What a harness fetched and keeps is no setting of the operator's, even
  ;; when it names a key: it is neither read nor listed as unmapped.
  (loop for (path name) in '((".claude/settings.json" "ANTHROPIC_API_KEY")
                             (".claude/plugins/plugin-catalog-cache.json" "OPENROUTER_API_KEY")
                             (".claude/Models_Cache.json" "OPENAI_API_KEY"))
        do (import-write box path (format nil "{\"env\": {\"~a\": \"~a\"}}" name +fixture-key+)))
  (let* ((plan (nik:make-plan :source "claude" :only "providers"))
         (walked (first (nik::plan-homes plan))))
    (is (import-item plan "providers" "anthropic") "the settings file's key is planned")
    (is (not (import-item plan "providers" "openrouter")) "a cache's key is not")
    (is (not (import-item plan "providers" "openai")) "whatever case the name is in")
    (let ((backlog (nik::home-unmapped walked)))
      (is (notany (lambda (line) (search "cache" line :test #'char-equal)) backlog) "and a cache is not the backlog"))))

#-win32
(deftest import-walk-lists-a-directory-as-uiop-did (with-temp-directory (box "walk-box"))
  ;; A link counts as what it reaches, its own path kept; a link to nothing
  ;; is passed over; names sort as SBCL's DIRECTORY sorts namestrings, where
  ;; a subdirectory's ends in `/'.
  (let ((root (uiop:ensure-directory-pathname box)))
    (dolist (path '("elsewhere/x.json" "d/b.json" "d/a.json" "d/abc/k.json" "d/abc-d/k.json"
                    "d/.env"))
      (import-write root path "{}"))
    (loop for (target link) in '(("elsewhere/" "d/linked") ("elsewhere/x.json" "d/linked.json")
                                 ("nowhere.json" "d/gone.json"))
          do (nlk:make-symlink (merge-pathnames target root) (merge-pathnames link root)))
    (let ((directory (merge-pathnames "d/" root)))
      (multiple-value-bind (files subdirectories)
          (nik::directory-entries (uiop:native-namestring directory))
        (is (equal (mapcar #'file-namestring (uiop:directory-files directory)) files))
        (is (equal (mapcar #'nlk:folder-name (uiop:subdirectories directory)) subdirectories))
        (is (equal '(".env" "a.json" "b.json" "linked.json") files))
        (is (equal '("abc-d" "abc" "linked") subdirectories))))))

(deftest import-frontmatter-reads-the-block-alone
    (let ((cr (string #\Return))))
  ;; Only the block is split now; what comes back is what splitting the
  ;; whole text gave: CRLF made LF, the trailing blank lines off.
  (flet ((check (text fields body)
           (multiple-value-bind (got-fields got-body) (nik::frontmatter text)
             (is (equal fields got-fields) (format nil "fields of ~s" text))
             (is (equal body got-body) (format nil "body of ~s" text)))))
    (check (format nil "---~%name: a~%---~%body~%~%") '(("name" . "a")) "body")
    (check (format nil "---~a~%name: a~a~%---~a~%x~a~%y~a~%" cr cr cr cr cr) '(("name" . "a")) (format nil "x~%y"))
    (check (format nil "---~%name: a~%---") '(("name" . "a")) "")
    (check (format nil "---~%metadata:~%  type: user~%---~%~%hi") '(("metadata.type" . "user")) (format nil "~%hi"))
    (let ((open (format nil "---~%name: a~%")))
      (check open nil open))
    (check "no block" nil "no block"))
  (is (equal "hello world" (nik::one-line-of (format nil "~%  ~%-- ~%  hello   world ~%next") 80)))
  (is (equal "" (nik::one-line-of nil 80))))
