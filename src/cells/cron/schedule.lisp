;;;; schedule.lisp --- the schedule grammar and the next-fire arithmetic. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One string in, one SCHEDULE out; one schedule and one instant in, the
;;;; next fire out. No clock is read here except by the two forms that mean
;;;; "from now" ("in 45m", "at 17:30"), and those take the instant as an
;;;; argument so a test can hand them a fixed one. Times are universal-time
;;;; integers throughout; the machine's local zone is the zone of every
;;;; wall-clock field, because that is the zone the operator's watch shows.
;;;;
;;;; The grammar is hermes-agent's cron grammar reduced to what reads
;;;; unambiguously (cron/jobs.py parse_schedule, 2026-09):
;;;;
;;;;   "30m"  "every 2h"  "every hour"  "1h 30m"   recurring interval
;;;;   "in 45m"                                     once, that far from now
;;;;   "at 17:30"  "at 9am"                         once, the next such time
;;;;   "every day at 9am"  "weekdays at 9:30"
;;;;   "every monday 9am"  "every mon,wed at 18:00" recurring, wall clock
;;;;   "0 9 * * 1-5"                                five-field cron
;;;;   "2026-09-10T09:00"  "2026-09-10 09:00"       once, at that time
;;;;
;;;; Cron fields follow Vixie: minute hour day-of-month month day-of-week,
;;;; with lists, ranges, steps, month and weekday names, 7 as Sunday, and
;;;; the rule that when both day fields are restricted a day matching either
;;;; fires. An interval is anchored on the fire it follows, not on the
;;;; instant the ticker got around to it, so "every 2h" keeps its phase.

(in-package #:nodecode-cron)

;;; --- instants ---------------------------------------------------------------

;; The registry stores unix seconds.
(defun universal-from-unix (unix) (+ unix nlk:+unix-epoch+))
(defun unix-from-universal (universal) (- universal nlk:+unix-epoch+))

(defun local-text (universal)
  "UNIVERSAL as local wall time, \"2026-09-07 09:00\"."
  (nlk:bind (((_ minute hour day month year)
              (decode-universal-time universal)))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

;;; --- the schedule -----------------------------------------------------------

(nlk:define-record (schedule (:copier nil))
  "One parsed schedule."
  ;; KIND is :INTERVAL (SECONDS), :CRON (EXPR as written and FIELDS parsed:
  ;; five entries, each T for a bare `*' or a sorted list of integers) or
  ;; :ONCE (AT, a universal time). DISPLAY is what the operator reads back.
  (kind :interval :type keyword)
  (seconds nil :type (or null integer))
  (expr nil :type (or null string))
  (fields nil :type list)
  (at nil :type (or null integer))
  (display "" :type string))

(defparameter +grammar-help+
  ;; Through FORMAT, so the line breaks written here are not in the refusal.
  (format nil "a schedule is one of: an interval \"30m\" / \"every 2h\" / \"every hour\" / \"1h 30m\" ~
(recurring); \"in 45m\" (once, that far from now); \"at 17:30\" (once, the next such time); ~
\"every day at 9am\" / \"weekdays at 9:30\" / \"every monday 9am\" / \"every mon,wed at 18:00\" ~
(recurring, local wall clock); five-field cron \"0 9 * * 1-5\"; or an ISO time ~
\"2026-09-10T09:00\" (once)")
  "The refusal every unparseable schedule carries, so the model can repair it.")

;;; --- durations ----------------------------------------------------------------

(defun duration-text (seconds)
  "SECONDS as the shortest unit words: 7200 -> \"2h\", 5400 -> \"1h 30m\"."
  (nlk:if-let (parts (loop for (unit . size) in '(("d" . 86400) ("h" . 3600) ("m" . 60) ("s" . 1))
                           for count = (floor seconds size)
                           when (plusp count)
                             collect (format nil "~d~a" count unit)
                             and do (decf seconds (* count size))))
    (format nil "~{~a~^ ~}" parts) "0s"))

(defun left-text (universal &optional (now (get-universal-time)))
  "How far NOW is from UNIVERSAL, in relative words: \"in 23h 55m\" ahead,
\"3m ago\" behind, \"now\" at the instant."
  ;; A span of a minute or more reads in whole minutes -- the grain of the
  ;; wall clock it stands beside.
  (let* ((seconds (abs (- universal now)))
         (span (if (>= seconds 60) (- seconds (mod seconds 60)) seconds)))
    (cond ((> universal now) (format nil "in ~a" (duration-text span)))
          ((< universal now) (format nil "~a ago" (duration-text span)))
          (t "now"))))

(defun at-text (universal &optional (now (get-universal-time)))
  "UNIVERSAL as local wall time and how far off it is: \"2026-09-13 20:37 (in 23h 55m)\"."
  (format nil "~a (~a)" (local-text universal) (left-text universal now)))

;;; --- clock times and day words ----------------------------------------------------

(defparameter +clock-regex+ "^(\\d{1,2})(?::(\\d{2}))?\\s*(am|pm)?$")

(defun parse-clock (text)
  "(values HOUR MINUTE) for \"9\", \"9am\", \"9:30\", \"9:30pm\", \"17:30\",
\"noon\", \"midnight\"; NIL when TEXT is not a clock time."
  (cond ((string= text "noon") (values 12 0))
        ((string= text "midnight") (values 0 0))
        (t
         (ppcre:register-groups-bind (hour minute meridiem) (+clock-regex+ text)
           (let ((hour (parse-integer hour))
                 (minute (if minute (parse-integer minute) 0)))
             (when meridiem
               (when (or (zerop hour) (> hour 12))
                 (return-from parse-clock nil))
               (setf hour (+ (mod hour 12) (if (string= meridiem "pm") 12 0))))
             (and (<= 0 hour 23) (<= 0 minute 59)
                  (values hour minute)))))))

(defparameter +weekday-numbers+
  '(("sunday" . 0) ("sun" . 0) ("monday" . 1) ("mon" . 1)
    ("tuesday" . 2) ("tue" . 2) ("tues" . 2) ("wednesday" . 3) ("wed" . 3)
    ("weds" . 3) ("thursday" . 4) ("thu" . 4) ("thur" . 4) ("thurs" . 4)
    ("friday" . 5) ("fri" . 5) ("saturday" . 6) ("sat" . 6)))

(defparameter +month-numbers+
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)))

(defun natural-cron-expr (text)
  "The five-field expression for \"<days> [at] <time>\" -- \"monday 9am\",
\"weekdays at 9:30\", \"mon, wed and fri at 18:00\", \"day at noon\" -- or
NIL when TEXT does not read that way."
  (let* ((tokens (cl:remove "" (ppcre:split "[\\s,]+" text) :test #'string=))
         (word (cdr (assoc (first tokens) '(("day" . "*") ("daily" . "*") ("everyday" . "*")
                                            ("weekday" . "1-5") ("weekdays" . "1-5")
                                            ("weekend" . "0,6") ("weekends" . "0,6"))
                           :test #'equal)))
         (days '())
         (rest (if word (rest tokens) tokens)))
    (unless word
      (loop for day = (assoc (first rest) +weekday-numbers+ :test #'string=)
            while (and rest (or day (string= (first rest) "and")))
            do (when day (pushnew (cdr day) days))
               (setf rest (cdr rest))))
    (when (and (or word days) (not (null rest)))
      (multiple-value-bind (hour minute)
          (parse-clock (format nil "~{~a~^ ~}" (if (string= (first rest) "at") (cdr rest) rest)))
        (when hour
          (format nil "~d ~d * * ~a" minute hour
                  (or word (format nil "~{~d~^,~}" (sort days #'<)))))))))

;;; --- cron fields --------------------------------------------------------------------

(defparameter +field-names+ '("minute" "hour" "day-of-month" "month" "day-of-week"))

(defun field-number (token index)
  "TOKEN as an integer in field INDEX, names allowed where cron allows them:
three-letter months (\"jan\", \"january\") and weekday names or abbreviations."
  (let ((named (case index
                 (3 (and (ppcre:scan "^[a-z]+$" token)
                         (cdr (assoc (subseq token 0 (min 3 (length token)))
                                     +month-numbers+ :test #'string=))))
                 (4 (cdr (assoc token +weekday-numbers+ :test #'string=))))))
    (cond (named named)
          ((ppcre:scan "^\\d+$" token) (parse-integer token))
          (t (fail "cron ~a field: ~s is not a number~:[~; or a name~]"
                   (nth index +field-names+) token (member index '(3 4)))))))

(defun parse-cron-field (text index)
  "T for a bare `*'; else the sorted list of values TEXT names in field
INDEX: lists, ranges, steps (`*/15', `1-5', `1-30/5', `5/10'), names."
  (destructuring-bind (low . high) (nth index '((0 . 59) (0 . 23) (1 . 31) (1 . 12) (0 . 7)))
    (when (string= text "*")
      (return-from parse-cron-field t))
    (let ((values '())
          (field (nth index +field-names+)))
      (dolist (item (ppcre:split "," text))
        (when (string= item "")
          (fail "cron ~a field: empty list item in ~s" field text))
        (ppcre:register-groups-bind (range step) ("^([^/]+)(?:/(\\d+))?$" item)
          (let ((step (if step (parse-integer step) 1)))
            (destructuring-bind (from to)
                (cond ((string= range "*") (list low high))
                      ((ppcre:scan "^[^-]+-[^-]+$" range)
                       (destructuring-bind (a b) (ppcre:split "-" range)
                         (list (field-number a index) (field-number b index))))
                      (t (let ((one (field-number range index)))
                           (list one (if (> step 1) high one)))))
              (when (zerop step)
                (fail "cron ~a field: step 0 in ~s" field item))
              (unless (and (<= low from high) (<= low to high) (<= from to))
                (fail "cron ~a field: ~s is outside ~d-~d" field item low high))
              (loop for value from from to to by step
                    do (pushnew (if (and (= index 4) (= value 7)) 0 value) values)))))
        (unless (ppcre:scan "^[^/]+(?:/\\d+)?$" item)
          (fail "cron ~a field: cannot read ~s" field item)))
      (sort values #'<))))

(defun cron-next (fields after)
  "The first minute strictly after universal time AFTER that FIELDS match,
or NIL inside the horizon."
  ;; Days are stepped at local noon, which no daylight-saving shift can move
  ;; onto another date.
  (destructuring-bind (minutes hours doms months dows) fields
    (nlk:bind (((_ minute-0 hour-0 day-0 month-0 year-0)
                (decode-universal-time (* 60 (1+ (floor after 60))))))
      (loop with first-noon = (encode-universal-time 0 0 12 day-0 month-0 year-0)
            for offset from 0 below 1830 ; five years ahead covers February 29
            for noon = (+ first-noon (* offset 86400))
            do (nlk:bind (((_ _ _ day month year weekday)
                           (decode-universal-time noon)))
                 (when (and (or (eq months t) (member month months))
                            ;; Vixie: both day fields restricted, either matches.
                            (let ((dow (mod (1+ weekday) 7)))
                              (if (and (listp doms) (listp dows))
                                  (or (member day doms) (member dow dows))
                                  (and (or (eq doms t) (member day doms))
                                       (or (eq dows t) (member dow dows))))))
                   (let ((today (zerop offset)))
                     (dolist (hour (if (eq hours t) (alexandria:iota 24) hours))
                       (when (or (not today) (>= hour hour-0))
                         (dolist (minute (if (eq minutes t) (alexandria:iota 60) minutes))
                           (when (or (not today) (> hour hour-0) (>= minute minute-0))
                             (return-from cron-next
                               (encode-universal-time 0 minute hour day month year))))))))))
      nil)))

;;; --- ISO times ----------------------------------------------------------------------

(defparameter +iso-regex+
  "^(\\d{4})-(\\d{2})-(\\d{2})(?:[T ](\\d{2}):(\\d{2})(?::(\\d{2}))?\\s*(Z|[+-]\\d{2}:?\\d{2})?)?$"
  "Date, optional time, optional seconds, optional zone.")

(defun parse-iso (text)
  "Universal time for \"2026-09-10T09:00[:SS][Z|+HH:MM]\" or \"2026-09-10 09:00\";
a bare date is that day's midnight. No zone means local. NIL otherwise."
  (ppcre:register-groups-bind (year month day hour minute second zone) (+iso-regex+ text)
    (let ((tz (cond ((null zone) nil)
                    ((string-equal zone "Z") 0)
                    (t (let ((sign (if (char= (char zone 0) #\-) 1 -1))
                             (digits (cl:remove #\: (subseq zone 1))))
                         ;; ENCODE-UNIVERSAL-TIME's zone is hours WEST.
                         (* sign (+ (parse-integer digits :end 2)
                                    (/ (parse-integer digits :start 2) 60))))))))
      (nlk:with-handlers ((error () nil))
        (apply #'encode-universal-time (if second (parse-integer second) 0)
               (if minute (parse-integer minute) 0)
               (if hour (parse-integer hour) 0)
               (parse-integer day) (parse-integer month)
               (parse-integer year) (and tz (list tz)))))))

;;; --- the parser -----------------------------------------------------------------------

(defun next-clock-instant (hour minute now)
  "The next local instant reading HOUR:MINUTE strictly after NOW."
  (nlk:bind (((_ _ _ day month year) (decode-universal-time now)))
    (let ((today (encode-universal-time 0 minute hour day month year)))
      (if (> today now)
          today
          (multiple-value-bind (s2 mi2 h2 day2 month2 year2)
              (decode-universal-time (+ (encode-universal-time 0 0 12 day month year) 86400))
            (declare (ignore s2 mi2 h2))
            (encode-universal-time 0 minute hour day2 month2 year2))))))

(defun once-schedule (at display)
  (make-schedule :kind :once :at at :display display))

(defun interval-schedule (seconds)
  ;; A job is a model turn: one a second is a bill, not a schedule.
  (when (< seconds 60)
    (fail "an interval under a minute is refused: ~a" (duration-text seconds)))
  (make-schedule :kind :interval :seconds seconds
                 :display (format nil "every ~a" (duration-text seconds))))

(defun cron-schedule (expr display)
  (let ((parts (cl:remove "" (ppcre:split "\\s+" (string-trim " " expr)) :test #'string=)))
    (unless (= (length parts) 5)
      (fail "a cron expression has five fields (minute hour day month weekday), ~s has ~d"
            expr (length parts)))
    (make-schedule :kind :cron :expr expr :display display
                   :fields (loop for part in parts
                                 for index from 0
                                 collect (parse-cron-field part index)))))

(defun parse-schedule (text &key (now (get-universal-time)))
  "TEXT as a SCHEDULE, or a CRON-ERROR carrying the grammar."
  ;; NOW anchors the two relative forms.
  (unless (and (stringp text) (plusp (length (string-trim " " text))))
    (fail "a schedule is a string; ~a" +grammar-help+))
  (let* ((original (string-trim " " text))
         (lower (string-downcase original)))
    (cond
      ((uiop:string-prefix-p "in " lower)
       (let ((seconds (nlk:parse-duration (subseq lower 3))))
         (unless seconds (fail "cannot read the duration in ~s; ~a" original +grammar-help+))
         (once-schedule (+ now seconds) (format nil "once, in ~a" (duration-text seconds)))))
      ((uiop:string-prefix-p "at " lower)
       (multiple-value-bind (hour minute) (parse-clock (string-trim " " (subseq lower 3)))
         (unless hour (fail "cannot read the time in ~s; ~a" original +grammar-help+))
         (let ((at (next-clock-instant hour minute now)))
           (once-schedule at (format nil "once, at ~a" (local-text at))))))
      ((uiop:string-prefix-p "every " lower)
       (let* ((rest (string-trim " " (subseq lower 6)))
              (expr (natural-cron-expr rest))
              (seconds (and (null expr) (nlk:parse-duration rest))))
         (cond (expr (cron-schedule expr original))
               (seconds (interval-schedule seconds))
               (t (fail "cannot read ~s; ~a" original +grammar-help+)))))
      ((nlk:parse-duration lower) (interval-schedule (nlk:parse-duration lower)))
      ((natural-cron-expr lower) (cron-schedule (natural-cron-expr lower) original))
      ((= 5 (length (ppcre:split "\\s+" lower)))
       (cron-schedule lower original))
      ((parse-iso original)
       (let ((at (parse-iso original)))
         (unless (> at now)
           (fail "~s is in the past (local now is ~a)" original (local-text now)))
         (once-schedule at (format nil "once, at ~a" (local-text at)))))
      (t (fail "cannot read ~s; ~a" original +grammar-help+)))))

;;; --- next fire, period, grace ---------------------------------------------------------

(defun next-fire (schedule after &key anchor)
  "The first fire strictly after universal time AFTER, or NIL when the
schedule has none left."
  ;; ANCHOR is the fire an interval counts from (its previous due instant);
  ;; without one the interval counts from AFTER.
  (ecase schedule.kind
    (:once (let ((at schedule.at))
             (and (> at after) at)))
    (:interval (let ((seconds schedule.seconds)
                     (base (or anchor after)))
                 (+ base (* seconds (1+ (floor (- after base) seconds))))))
    (:cron (cron-next schedule.fields after))))

(defparameter +grace-floor-seconds+ 120)

(defun schedule-grace (schedule at)
  "How late the fire due AT may still fire rather than count as missed:
half its period, clamped to two minutes and two hours (hermes' catch-up
rule), so a daily job survives a lunch-long outage and a five-minute job
does not burst-fire after one."
  (nlk:if-let (period (ecase schedule.kind   ; from AT to the fire after it
                        (:once nil)
                        (:interval schedule.seconds)
                        (:cron (let ((next (cron-next schedule.fields at))) (and next (- next at))))))
    (max +grace-floor-seconds+ (min (floor period 2) 7200))
    +grace-floor-seconds+))
