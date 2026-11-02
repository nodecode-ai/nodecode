;;;; gc.lisp --- collection scheduling. One owner for when the organism pauses.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A generational GC decides when to pause on its own, and its choice is
;;;; usually right. It is wrong for this program in one specific way: the
;;;; organism spends most of its wall clock blocked on a provider response, and a
;;;; collection that happens outside that window is a pause an operator can see,
;;;; while a collection that happens inside it is free. So this file does not try
;;;; to collect less. It moves collection into the window that is already dead
;;;; time.
;;;;
;;;; Mechanism: SB-EXT:MAKE-TIMER with :THREAD T runs the callback on a
;;;; normal newly-created thread. The provider request remains blocked on its
;;;; own thread while SBCL stops all threads for the collection, so FORCE-COLLECTION
;;;; never runs from an interrupt handler or with interrupts disabled. The timer
;;;; thread is short-lived and the collection state is serialized below.
;;;;
;;;; The old :THREAD NIL policy interrupted whichever thread happened to be
;;;; running when the signal arrived. That was unsafe for the allocator: a GC
;;;; started in an interrupt context could race a different thread in LISP_ALLOC.
;;;; No permanent worker: the timer thread has no foreign handles and is gone
;;;; before an image is dumped.
;;;;
;;;; Scope: the nursery cap, the forced collection, the trim, and the numbers
;;;; that say what they cost. No heap sizing, no arena work.

(in-package #:nodecode.kernel)

;;; --- the consing budget -------------------------------------------------

;;; SBCL's defaults here are 5% of the dynamic space for the nursery — 410 MB
;;; on this image's 8 GB — and a fifth of that, 82 MB, per generation; chosen
;;; for throughput on batch work. A smaller nursery collects more often and
;;; each collection touches less, which trades a little total throughput for
;;; shorter individual pauses. That is the right trade for a program whose
;;; pauses are visible to a human at a prompt. The per-generation budget is
;;; the other half of the same trade: whatever survives a nursery collection
;;; is promoted, and a generation is only collected — and its freed pages only
;;; given back to the kernel — once it has taken in its budget. At SBCL's 82
;;; MB the organism idles carrying up to that much promoted garbage per
;;; generation; at the nursery's budget it carries at most one more nursery's
;;; worth. Measured on a fresh organism: 99 MB of heap beyond the image, of
;;; which 2.5 MB was live. And a turn's footprint is the nursery plus what the
;;; turn promoted: 30 back-to-back turns peaked at 48
;;; MB of heap under a 16 MB budget and at 27 under 4, for 2.3 ms a nursery
;;; collection -- most of it the scan of older generations' cards, the same
;;; at any size. At 2, with a 4 MB slack (*HEAP-SLACK*), 120
;;; turns held 52-56 MB a minute at most against 58-64 at 4 and 8, for 6.9 s
;;; of collection in place of 3.7: 27 ms a turn bought 6 MB. A parameter, so
;;; the organism can measure GC-RUN-TIME and change it about itself.
(defparameter *consing-budget* (* 2 1024 1024)
  "Bytes the organism's nursery may accumulate between automatic collections,
and bytes any generation, in every process, may take in by promotion before it
is collected in turn.")

;;; The shell's nursery is its own. Its heap is the transcript it paints,
;;; and neither of the organism's dead windows comes to it: it never waits
;;; on a provider, and a streaming turn is never idle. Every collection
;;; lands in a frame. Without this, a shell streaming many turns would
;;; read as frozen while collections ran inside its frames. A nursery
;;; collection costs the scan of the older generations' cards whatever its
;;; size, so a larger nursery is fewer of them: at 2 MB a shell streaming
;;; many turns spends 1.7 s collecting inside its frames, at 8 MB 1.0 s,
;;; and at 16 MB 0.4 s for 7 MB more at the shell's peak; 32 MB is no
;;; faster than 16 and costs 23 MB. The promotion budget stays the
;;; organism's: at 16 MB too the shell can stall longer and hold more.
(defparameter *shell-nursery* (* 16 1024 1024)
  "Bytes a shell's nursery may accumulate between automatic collections.")

(defun cap-nursery (&optional (bytes *consing-budget*))
  "Set the nursery to BYTES and every generation's promotion budget to
*CONSING-BUDGET*, collect a generation as soon as it has taken its budget in,
give freed pages back after every collection, and return the nursery's previous
value."
  ;; Separate from the DEFPARAMETER because setting a policy and applying it
  ;; are two different acts, and the organism may want to apply a value it did
  ;; not store. Generations 0 through 5 are the collectable ones; 6 is the
  ;; image itself.
  ;;
  ;; A budget alone does not bound a generation: SBCL collects one only once
  ;; its average age passes GENERATION-MINIMUM-AGE-BEFORE-GC as well (0.75),
  ;; and a turn's data -- promoted a nursery at a time while it lives, dead
  ;; once the turn ends -- never ages that far. At zero the budget is the
  ;; whole rule. And the runtime hands freed pages back to the kernel only
  ;; after a collection that reached generation 2 (remap_free_pages under
  ;; small_generation_limit, 1); below that, every page a nursery ever filled
  ;; stays resident. At 0 each collection gives back what it freed, one
  ;; madvise per free run, and the next nursery faults its pages in fresh.
  (check-type bytes (integer 1))
  ;; macOS too: darwin-jit hands a free run back by remapping it
  ;; (remap_free_pages) where Linux madvises it.
  #+(or linux darwin) (setf (sb-alien:extern-alien "small_generation_limit" (sb-alien:signed 8)) 0)
  (let ((previous (sb-ext:bytes-consed-between-gcs)))
    ;; SBCL pulls the pending trigger in by the amount the nursery shrinks,
    ;; and the trigger is unsigned: lowering the nursery by more than the
    ;; room left before the next collection underflows it and the setter
    ;; refuses. A collection first puts the trigger a full nursery out.
    (when (and (< bytes previous)
               (< (sb-alien:extern-alien "auto_gc_trigger" sb-alien:unsigned-long)
                  (- previous bytes)))
      (sb-ext:gc))
    (setf (sb-ext:bytes-consed-between-gcs) bytes)
    (loop for generation from 0 to 5
          do (setf (sb-ext:generation-bytes-consed-between-gcs generation) *consing-budget*
                   (sb-ext:generation-minimum-age-before-gc generation) 0d0))
    previous))

;;; Work that conses in bulk and keeps little -- a compile, a cell's load, an
;;; import's read, a catalog's parse -- pays the small nursery once every 2 MB,
;;; and a nursery collection costs the scan of the older generations' cards
;;; whatever its size: the setup walk's import folder loaded in 189 ms, 86 of
;;; them collecting. Such work raises the nursery while it runs.
;;; The nursery is the process's, so the raises count: two threads that each
;;; put back what they found left it at 64 MB for good when they ended in the
;;; order they began.
;;;
;;; A raise lifts the generations' budgets with it and keeps freed pages
;;; mapped: under 2 MB budgets, each nursery collection's survivors made the
;;; next collection walk the older generations too, and every page given back
;;; was faulted in again by the next nursery -- 400 ms of full import,
;;; four times what the same import collected in a plain image.
;;; And the nursery comes back down on a thread of its own, shortly after the
;;; last raise ends: the collection that brings it down copies what the work
;;; left alive, and in the caller's thread it held the import's answer back
;;; 150 ms.
(defvar *nursery-raises* 0
  "How many WITH-NURSERY bodies run now.")

(defvar *nursery-floor* nil
  "The nursery the first WITH-NURSERY found, until it is put back.")

(defvar *nursery-lock* (bt2:make-lock :name "nodecode-nursery"))

(defparameter *nursery-restore-delay* 0.1
  "Seconds after the last WITH-NURSERY body ends before the nursery comes back down.")

(defun thread-timer (function name seconds &rest keys)
  "A timer calling FUNCTION on a thread of its own named NAME, scheduled SECONDS
from now under KEYS, as SB-EXT:SCHEDULE-TIMER takes them. => the timer."
  (let ((timer (sb-ext:make-timer function :thread t :name name)))
    (apply #'sb-ext:schedule-timer timer seconds keys)
    timer))

(defun call-with-nursery (bytes thunk)
  "THUNK's values, the nursery at least BYTES while it runs (WITH-NURSERY)."
  (bt2:with-lock-held (*nursery-lock*)
    (unless *nursery-floor*
      (setf *nursery-floor* (sb-ext:bytes-consed-between-gcs)))
    (incf *nursery-raises*)
    (when (> bytes (sb-ext:bytes-consed-between-gcs))
      (cap-nursery bytes)
      ;; The pending trigger moves only at a collection: raised alone, the
      ;; nursery would still be collected within the old one's 2 MB, copying
      ;; all the work built by then.
      (setf (sb-alien:extern-alien "auto_gc_trigger" sb-alien:unsigned-long)
            (max (sb-alien:extern-alien "auto_gc_trigger" sb-alien:unsigned-long)
                 (+ (sb-kernel:dynamic-usage) bytes)))
      (loop for generation from 0 to 5
            do (setf (sb-ext:generation-bytes-consed-between-gcs generation) bytes))
      #+(or linux darwin) (setf (sb-alien:extern-alien "small_generation_limit" (sb-alien:signed 8)) 1)))
  (unwind-protect (funcall thunk)
    (bt2:with-lock-held (*nursery-lock*)
      (when (zerop (decf *nursery-raises*))
        (thread-timer #'restore-nursery "nodecode-nursery" *nursery-restore-delay*)))))

(defun restore-nursery ()
  "Put back the nursery the first raise found, unless a raise runs again."
  (with-handlers ((error () nil))
    (bt2:with-lock-held (*nursery-lock*)
      (when (and (zerop *nursery-raises*) *nursery-floor*)
        (cap-nursery (shiftf *nursery-floor* nil))))))

(defmacro with-nursery ((bytes) &body body)
  "BODY with the nursery and every generation's budget at least BYTES; shortly
after the last body running ends, the nursery the first one found comes back,
with one collection."
  `(call-with-nursery ,bytes (lambda () ,@body)))

;;; The runtime keeps one mark byte per 1 KB card of the dynamic space --
;;; 8 MB for this image's 8 GB -- and fills every byte at startup, so the
;;; whole table is resident from the first instruction: a fifth of an idle
;;; shell. Only the cards over pages the heap has used are ever read; the
;;; rest can go back. A card that comes back from the kernel reads zero,
;;; which is CARD_MARKED, the value the runtime filled with: the state such
;;; a card was in already.
(defun release-card-marks (&aux (page (sb-posix:getpagesize)))
  "Give the kernel back the collector's card marks for the part of the
dynamic space the heap has not reached. => bytes released, NIL where the
platform has no such call."
  ;; The world is held still for it: a collection rewrites the marks of the
  ;; pages it scans, and one that ran beside the release could find a mark
  ;; it set gone. Where the mark table is indexed from a card other than
  ;; the dynamic space's first, nothing is released. NEXT_FREE_PAGE is read
  ;; through SBCL's own declaration, a 32-bit PAGE-INDEX-T: read as a long
  ;; it carried the word beside it, and where that word is not zero (SBCL
  ;; 2.6.9's Fedora build) the page count came out past the table and
  ;; nothing was ever released.
  #+(and linux (or x86-64 arm64))
  (sb-sys:without-gcing
    (let* ((table (sb-sys:sap-int (sb-alien:extern-alien "gc_card_mark" sb-sys:system-area-pointer)))
           (cards (1+ (sb-alien:extern-alien "gc_card_table_mask" sb-alien:unsigned-long)))
           (used (* (1+ sb-vm:next-free-page)
                    (floor sb-vm:gencgc-page-bytes sb-vm:gencgc-card-bytes)))
           (from (* page (ceiling used page))))
      (when (and (zerop (logand (floor sb-vm:dynamic-space-start sb-vm:gencgc-card-bytes)
                                (1- cards)))
                 (< from cards)
                 (zerop (%madvise (+ table from) (- cards from) 4))) ; MADV_DONTNEED
        (- cards from))))
  #-(and linux (or x86-64 arm64)) (progn page nil))

;;; --- the budget rule: which generation a dead window collects -------------
;;; Both dead windows — provider wait and idle — ask the same question: is
;;; there a generation holding a nursery's worth of promoted garbage? A
;;; long turn answers yes in the oldest one. Each round's request body and
;;; the buffers its streamed reply went through are live for the round and
;;; dead after it, and a round that streams for minutes sees hundreds of
;;; nursery collections, so what it holds climbs to generation 5 before it
;;; dies. Measured on a shell an hour into a cell turn: 219 MB in
;;; generation 5, and not one of its large strings had a referrer.
;;;
;;; SBCL's own cascade will not reach such a generation: it collects an old
;;; generation only once it has taken in three quarters of its size
;;; (GENERATION-MINIMUM-AGE-BEFORE-GC), which a 200 MB generation fed 16 MB
;;; a round never does. And (GC :GEN N) collects generations 0 through N-1
;;; unconditionally but N itself only under that same rule — measured: 55
;;; MB dead in generation 3 survives (GC :GEN 3) and goes with (GC :GEN 4).
;;; COLLECT-GENERATION is where that fact lives.

;;; Zero for one never collected, which is what makes the first dead window of
;;; a launch reclaim whatever boot promoted.
(defvar *generation-baseline* (make-array 7 :initial-element 0)
  "Bytes each generation held just after a dead window last collected it.")

(defun overgrown-generation ()
  "The oldest generation that has taken in its budget's worth since a dead
window last collected it, or NIL."
  ;; Over budget alone would collect a generation of live data every window
  ;; forever; grown by a budget is a nursery's worth of promoted garbage, the
  ;; amount the allocator itself would have collected for had the generation
  ;; been young.
  (loop for generation from 5 downto 1
        for allocated = (sb-ext:generation-bytes-allocated generation)
        when (>= (- allocated (svref *generation-baseline* generation))
                 (sb-ext:generation-bytes-consed-between-gcs generation))
          return generation))

(defvar *forced-heap-at* 0
  "DYNAMIC-USAGE as of the last collection that walked the whole heap -- a
dead window's or the slack rule's -- or as the launch found it.")

(defun collect-generation (generation)
  "Collect generations 0 through GENERATION, GENERATION's own garbage
included, and note their sizes."
  ;; The SBCL fact above: the oldest generation a (GC :GEN n) call reaches
  ;; unconditionally is n-1, so the call names the generation above, and the
  ;; oldest of all takes the full walk.
  (if (>= generation 5)
      (progn (sb-ext:gc :full t) (setf *forced-heap-at* (sb-kernel:dynamic-usage)))
      (sb-ext:gc :gen (1+ generation)))
  (loop for each from 0 to generation
        do (setf (svref *generation-baseline* each) (sb-ext:generation-bytes-allocated each))))

;;; --- collection inside provider wait ------------------------------------

;;; A provider dispatch is dead time, but it is not free time: the old policy
;;; collected on every dispatch, and on this image that was 1,701 full
;;; collections in two hours. A dispatch that has consed less than this has
;;; nothing worth reclaiming and spends the wait on nothing.
(defparameter *collect-min-consed* (* 96 1024 1024)
  "Bytes that must be consed since the last forced collection before the next
one runs.")

;;; Measured on a 590 MB heap carrying ~50 MB/s of UI garbage: generations 0-2
;;; cost 29 ms and reclaimed 61 MB; a full collection that followed cost 326
;;; ms and reclaimed 15 MB more. The full walk is the expensive part and the
;;; young generations hold the garbage, so the routine collection is the
;;; generational one and the full one is reserved for the heap growth test
;;; below.
(defparameter *collect-generation* 2
  "Generations a forced collection reclaims, 0 through this one.")

;;; Above what one turn reads -- its code, the image's constants and the
;;; libraries' -- so a working organism never pays to fault its own turn back
;;; in, and below what a boot leaves behind (55 to 60). What a turn reads is
;;; the file system's to say: 16 to 20 MB from a tmpfs home, 35 to 40 from
;;; one on disk, where the kernel maps every fault's cached neighbours with it
;;; (fault-around; MADV_RANDOM does not stop it).
(defparameter *wide-read-bytes* (* 48 1024 1024)
  "Image pages a working organism may hold before a provider wait or an idle
check gives them back.")

;;; Small enough that it lands inside provider wait even when the model is
;;; fast, and large enough that the request has actually been written to the
;;; socket. Provider round trips on this machine run several hundred
;;; milliseconds to several seconds; the measured full-GC pause is tens of
;;; milliseconds.
(defparameter *collect-delay* 0.05
  "Seconds after dispatch before the forced collection fires.")

;;; Recorded rather than logged: a number can be read back and compared where
;;; a log line cannot.
(defvar *last-gc-pause* nil
  "Wall-clock seconds the most recent forced collection took, or NIL.")

(defvar *forced-collections* 0
  "How many forced collections have completed in this image.")

(defmacro collect-during-wait (&body body &aux (timer (gensym "TIMER")))
  "Run BODY with a collection scheduled to fire shortly after it starts."
  ;; BODY is expected to block — it is the provider request. The timer
  ;; callback runs on a normal thread, so SBCL may stop the world and collect
  ;; without entering through an interrupt handler that has interrupted
  ;; another allocator thread. The callback is serialized with other forced
  ;; collections.
  ;;
  ;; The timer is unscheduled on every exit path: BODY can signal (a provider
  ;; error is ordinary), and a timer that outlives its request would fire into
  ;; the next one.
  `(let ((,timer (thread-timer #'force-collection "nodecode-gc" *collect-delay*)))
     (unwind-protect (progn ,@body)
       (sb-ext:unschedule-timer ,timer))))

(defvar *forced-consed-at* 0
  "SB-EXT:GET-BYTES-CONSED as of the last forced collection.")

;;; Several provider turns may arm timers at once. The allocator already stops
;;; the world during GC, but the policy's threshold reads and bookkeeping also
;;; need one ordinary lock so concurrent timer callbacks cannot both decide to
;;; collect or lose a counter update.
(defvar *forced-collection-lock* (bt2:make-lock :name "nodecode-gc")
  "Serializes forced collection decisions and their counters.")

(defvar *forced-collection-thread* nil
  "The thread that last entered FORCE-COLLECTION, for diagnostics and tests.")

(defun force-collection ()
  "Run one collection and record what it cost."
  ;; The timer's normal thread calls it.
  ;;
  ;; Generational by default: generations 0 through *COLLECT-GENERATION* hold the
  ;; garbage a dispatch produced, and reclaiming them costs about a tenth of the
  ;; full walk. The collection reaches further when an older generation has
  ;; taken in its budget's worth since a dead window last collected it
  ;; (OVERGROWN-GENERATION): what a long turn promotes is found there and
  ;; nowhere else, and the round's own wait is the time to walk for it. A
  ;; dispatch that has consed less than *COLLECT-MIN-CONSED* since the last
  ;; collection skips it all: nothing has accumulated to collect, and the wait
  ;; is better spent not stopping the world.
  ;;
  ;; Errors are swallowed deliberately: the timer callback is auxiliary work,
  ;; and a collection that fails must not take the provider turn with it.
  (bt2:with-lock-held (*forced-collection-lock*)
    (setf *forced-collection-thread* (bt2:current-thread))
    (let ((start (get-internal-real-time)))
      (with-handlers ((error () nil))
        (when (>= (- (sb-ext:get-bytes-consed) *forced-consed-at*)
                  *collect-min-consed*)
          (collect-generation (max *collect-generation* (or (overgrown-generation) 0)))
          ;; A full walk reads every page of the image.
          (release-wide-read)
          (setf *forced-consed-at* (sb-ext:get-bytes-consed))
          (setf *last-gc-pause*
                (/ (- (get-internal-real-time) start)
                   (coerce internal-time-units-per-second 'double-float)))
          (incf *forced-collections*))))))

;;; --- the operator's input ------------------------------------------------
;;; A collection is a stop-the-world pause, and one that lands behind a
;;; keystroke burst stalls the very frame the operator is waiting for.
;;; The shell stamps every key and pointer event, and the collections that
;;; can wait hold off from it: the idle collector for *IDLE-INTERVAL*, a
;;; slack walk for *SLACK-WALK-REST*.
;;; NIL until the first event: internal real time counts from the process's
;;; start, so a 0 here would read as a key pressed at launch, and for the first
;;; *IDLE-INTERVAL* of every process the idle collector would wait on an
;;; operator who never typed.
(defvar *idle-input-at* nil
  "INTERNAL-REAL-TIME of the operator's last key or pointer event, as the
shell stamps it (NOTE-OPERATOR-INPUT), or NIL before the first.")

(defun note-operator-input ()
  "The shell saw a key or pointer event: the collections that can wait hold
off from now."
  (setf *idle-input-at* (get-internal-real-time))
  (values))

;;; --- the slack: garbage a working heap may carry --------------------------
;;; Budgets bound each generation, not the heap. A turn's data is promoted a
;;; nursery at a time while it lives and dies when the turn ends, in whatever
;;; generation it had reached, and five generations each inside its budget.
;;; Neither dead window reaches it -- a back-to-back turn is never idle, and
;;; the provider wait collects every 96 MB consed. So the rule is on the heap
;;; itself: a collection that leaves it more than the slack past what the last
;;; full walk found live is followed by one -- 4 MB, or a quarter of the live
;;; set where that is more, so a gateway holding many sessions walks in
;;; proportion to what it allocates rather than every few megabytes of a heap
;;; ten times the shell's. Not half: a walk that lands on a boot's transient
;;; reads it as live, and the slack it then allows would hold a fresh launch's
;;; first turns.

(defparameter *heap-slack* (* 4 1024 1024)
  "Bytes a collection may leave the heap past the last full walk's live set
before a full walk follows, at the least.")

(defparameter *heap-slack-ratio* 1/4
  "The slack as a share of the live set, where that is more than
*HEAP-SLACK*.")

(defvar *slack-walk-pending* nil
  "True from a slack walk's scheduling until it has run.")

;;; A full walk stops every thread for as long as it takes; without the
;;; rest before it runs, one that lands inside a drag or a scroll would be
;;; a stall the operator watches. The pointer reports every cell it crosses
;;; and the wheel every notch, so a gesture in progress never rests this
;;; long; a turn that was typed and left to run rests at once.
(defparameter *slack-walk-rest* 1/4
  "Seconds without a key or pointer event a slack walk waits for.")

(defun walk-past-slack ()
  "After a collection: schedule a full walk on a thread of its own when the
heap has grown its slack (*HEAP-SLACK*) past *FORCED-HEAP-AT*, once the
operator's input has rested *SLACK-WALK-REST*."
  ;; An after-GC hook runs in whichever thread collected, so the walk goes to
  ;; a timer thread of its own, as the other dead-window collections do, and
  ;; serializes with them.
  (when (and (not *slack-walk-pending*)
             (> (- (sb-kernel:dynamic-usage) *forced-heap-at*)
                (max *heap-slack*
                     (floor (* *heap-slack-ratio*
                               (- *forced-heap-at* (sb-ext:generation-bytes-allocated 6)))))))
    (setf *slack-walk-pending* t)
    (labels ((rest-left ()
               (if *idle-input-at*
                   (max 0 (- *slack-walk-rest*
                             (/ (- (get-internal-real-time) *idle-input-at*)
                                (float internal-time-units-per-second 1d0))))
                   0))
             (walk ()
               (if (plusp (rest-left))
                   (arm)
                   (with-cleanup ((setf *slack-walk-pending* nil))
                     (bt2:with-lock-held (*forced-collection-lock*)
                       (with-handlers ((error () nil))
                         (collect-generation 5))))))
             (arm () (thread-timer #'walk "nodecode-slack-gc" (rest-left))))
      (arm))))

(defun cap-garbage ()
  "Arm the slack rule: every collection is followed by WALK-PAST-SLACK. The
heap as it stands is the first measure of live."
  ;; A launch has not walked: without a baseline its first collection would
  ;; walk the whole heap mid-boot. And a warm image is dumped by a process
  ;; that armed the rule too, so it carries that process's baseline and,
  ;; where a walk was scheduled when the dump unscheduled every timer
  ;; (DISARM-TIMERS), a pending flag nothing would ever clear.
  (setf *forced-heap-at* (sb-kernel:dynamic-usage)
        *slack-walk-pending* nil)
  (pushnew 'walk-past-slack sb-ext:*after-gc-hooks*)
  (values))

;;; --- the trim: what a finished phase leaves resident ----------------------
;;; A collection frees the heap; it does not shrink the process. Two things
;;; stay resident after a phase that read widely — boot, a full walk — and
;;; neither is live data: the heap's freed pages until a generational
;;; collection hands them back, and the pages of the image itself, mapped
;;; from the executable and touched once, which the kernel keeps counted
;;; against the process until told otherwise. The trim asks for both. Timed
;;; where such a phase ends, never on a schedule: the pages come back on
;;; demand, one fault each, and a trim in the middle of work would only
;;; pay that twice.

;;; Recorded for the same reason as *LAST-GC-PAUSE*: a number is scoreable.
(defvar *last-trim* nil
  "(:before :after) resident bytes of the most recent trim, or NIL.")

(defvar *image-releases* 0
  "How many times this image's pages have been given back.")

;;; Below this the release would walk every mapping to reclaim nothing: the
;;; walk costs about 16 ms, and the pages it hands back are refaulted one at a
;;; time by the work that needs them. Above what a release leaves: the pages
;;; an idle process never stops touching -- its libraries' hottest code, the
;;; image's -- come back at once, 3.8 MB on the operator's home,
;;; and a gate at 4 released every quiet stretch for nothing.
(defparameter *idle-release-bytes* (* 8 1024 1024)
  "Image pages this process may hold before a quiet stretch gives them back.")

(defvar *trimmed-resident* nil
  "RESIDENT-BYTES just after RELEASE-IMAGE-PAGES last trimmed the whole
working set -- where the platform trims only that (Windows) -- or NIL.")

(defun image-resident-bytes (&aux (core sb-ext:*core-pathname*))
  "Bytes of this program's own pages — its core file's mappings and its
libraries' code — resident now; where the platform trims only whole working
sets, the bytes the working set has taken back since the last trim; NIL where
the platform says neither."
  (or (and core (mapped-resident-bytes (resolve-runtime core)))
      (and *trimmed-resident*
           (when-let (resident (resident-bytes))
             (max 0 (- resident *trimmed-resident*))))))

(defun release-image-pages (&aux (core sb-ext:*core-pathname*))
  "Give the kernel back the program's own pages — code, constants and the
pseudo-static heap this process maps from its core file, and its libraries'
code — or, where the platform offers only that (Windows), every page of the
working set."
  ;; => bytes of mappings released, T for a working set trimmed, NIL where
  ;; the platform has neither call. Read again as they are touched; nothing
  ;; is lost.
  (prog1 (or (and core (release-mapped-pages (resolve-runtime core)))
             (when (trim-working-set)
               (setf *trimmed-resident* (resident-bytes))
               t))
    (incf *image-releases*)))

(defun release-wide-read ()
  "Give the image's pages back when this process holds more of them than one
turn reads (*WIDE-READ-BYTES*)."
  ;; A launch that goes straight to work keeps what its boot read: the boot
  ;; trim waits for a still screen a streaming turn never shows, and the
  ;; cells start after it. A full walk reads the whole image too.
  (when (when-let (held (image-resident-bytes)) (> held *wide-read-bytes*))
    (release-image-pages)))

(defun trim-memory ()
  "Return what the organism is not using: one generational collection so
the heap's freed pages go back to the kernel, then the image's mapped
pages (on Windows, the working set)."
  ;; => (values resident-before resident-after), NIL where the platform
  ;; does not report residency. Serialized with the forced collections but not
  ;; one of them: those are counted for the pauses they hide, and a trim hides
  ;; inside nothing.
  (bt2:with-lock-held (*forced-collection-lock*)
    (let ((before (resident-bytes)))
      (sb-ext:gc :gen *collect-generation*)
      (release-image-pages)
      (let ((after (resident-bytes)))
        (setf *last-trim* (list :before before :after after))
        (values before after)))))

;;; --- collection inside idle ------------------------------------------------
;;; The collector runs when allocation crosses a budget, so a process that
;;; stops allocating stops collecting. The wizard decodes the catalog, 80 MB
;;; of promoted decode garbage lands in generation 1 over its 16 MB budget,
;;; and it sits there while the operator reads the screen: measured 25 s
;;; after boot as a 190 MB heap whose live set was 78. Idle is the other
;;; dead window beside provider wait, and this timer applies in it the rule
;;; the allocator would apply if allocation continued: a generation that
;;; grew past its budget is collected, and its pages go back to the kernel.
;;; The image's pages go back the same way: a stretch of work reads code
;;; and constants widely — the wizard touched 52 MB of them after the boot
;;; trim — and once the work has stopped, what it touched is given back.
;;; Gated on the pages themselves, not on the work: an idle organism that
;;; has rendered a screen conses nothing while touching tens of megabytes
;;; of code, so consing is the wrong question to ask of it.

;;; Short enough that a screen the operator has stopped touching is trimmed
;;; before they notice the number, long enough that a check costs nothing.
(defparameter *idle-interval* 3
  "Seconds between idle checks.")

;;; A streaming turn conses tens of megabytes a second and is not; a cursor
;;; blinking is.
(defparameter *idle-consing* (* 1024 1024)
  "Bytes consed within one interval below which the organism counts as idle.")

(defvar *idle-timer* nil
  "The repeating timer COLLECT-WHILE-IDLE armed, or NIL.")

(defvar *idle-consed-at* 0
  "SB-EXT:GET-BYTES-CONSED as of the last idle check.")

(defvar *idle-collections* 0
  "How many collections the idle timer has run in this image.")

;;; The generational collection is tens of milliseconds and rides any quiet
;;; moment. A full walk is hundreds — and it is the only thing that finds
;;; garbage a young generation holds on behalf of an older one that is itself
;;; dead: measured on a settled shell, 45 MB in generation 2 of which 26 MB
;;; was live. Worth the walk, but only once the quiet has lasted long enough
;;; that the operator is reading rather than typing.
(defparameter *idle-checks-before-full* 5
  "Consecutive idle checks before the idle collector will walk the whole
heap.")

;;; Any check that finds the organism working zeroes it.
(defvar *idle-checks* 0
  "Consecutive idle checks.")

(defun collect-when-idle ()
  "The idle timer's callback: when the last interval consed little and held
no operator input, collect — the oldest overgrown generation and everything younger, or the whole heap
once the quiet has lasted *IDLE-CHECKS-BEFORE-FULL* checks and the heap has
grown a budget past the last full walk — and on every such stretch give the
image's pages back if it is holding any."
  ;; Errors are swallowed for the reason FORCE-COLLECTION gives.
  (let* ((consed (sb-ext:get-bytes-consed))
         (recent (- consed *idle-consed-at*)))
    (setf *idle-consed-at* consed)
    ;; Work or quiet: a release is no pause, and what a boot or a walk read
    ;; is not what the next turn reads.
    (bt2:with-lock-held (*forced-collection-lock*)
      (with-handlers ((error () nil))
        (release-wide-read)))
    (cond
      ;; Consing is work; so is the operator: a collection is a pause, and
      ;; a pause behind a keystroke is the one they feel.
      ((or (>= recent *idle-consing*)
           ;; An interval that holds the operator's input (NOTE-OPERATOR-INPUT) is not
           ;; quiet whatever it consed.
           (and *idle-input-at*
                (< (- (get-internal-real-time) *idle-input-at*)
                   (* *idle-interval* internal-time-units-per-second))))
       (setf *idle-checks* 0))
      (t
       (incf *idle-checks*)
       (bt2:with-lock-held (*forced-collection-lock*)
         (with-handlers ((error () nil))
           (when-let (generation (if (and (>= *idle-checks* *idle-checks-before-full*)
                                          (> (- (sb-kernel:dynamic-usage) *forced-heap-at*)
                                             *consing-budget*))
                                     5
                                     (overgrown-generation)))
             (collect-generation generation)
             (incf *idle-collections*))
           ;; The nursery the quiet began with is garbage by now, and
           ;; nothing else collects it until allocation resumes. Where the
           ;; runtime gives pages back only after a collection that reached
           ;; generation 2 (off Linux; CAP-NURSERY), the collections that
           ;; empty a nursery on their own keep its pages too: without this,
           ;; an idle shell would keep dead nursery pages in its footprint.
           (when (zerop (mod *idle-checks* *idle-checks-before-full*))
             (when (> (sb-ext:generation-bytes-allocated 0) (floor *consing-budget* 2))
               (collect-generation 1)
               (incf *idle-collections*))
             ;; The mapping walk rides the same quiet stretch the full
             ;; collection waits for, so an organism that pauses between
             ;; keystrokes never pays for either.
             ;; Whether the image holds enough of its own pages to be worth
             ;; a release is asked of the mappings themselves rather than of
             ;; the process's size: a collection frees heap and touches code
             ;; in the same breath, so a process can re-read half its image
             ;; without growing at all.
             (when (when-let (held (image-resident-bytes)) (> held *idle-release-bytes*))
               (release-image-pages)))))))))

(defun collect-while-idle (&optional (interval *idle-interval*))
  "Arm the idle collector: every INTERVAL seconds, on a timer thread of its
own, COLLECT-WHEN-IDLE."
  ;; Idempotent; => the timer. No permanent worker, for the fork-safety reason
  ;; the file header gives — the timer thread lives for one check. Armed means
  ;; scheduled, not merely made: a dump unschedules every timer (DISARM-TIMERS)
  ;; and the image keeps the variable. Without this, a warm launch would find
  ;; the dead timer and arm nothing -- no idle collection would ever run, and an
  ;; idle shell would keep the image pages it had read once.
  (if (and *idle-timer* (sb-ext:timer-scheduled-p *idle-timer*))
      *idle-timer*
      (setf *idle-consed-at* (sb-ext:get-bytes-consed)
            *idle-timer* (thread-timer #'collect-when-idle "nodecode-idle-gc" interval
                                       :repeat-interval interval))))

;;; --- attribution: which collections were visible -------------------------
;;; COLLECT-DURING-WAIT hides pauses inside provider wait; this section
;;; answers whether the hiding works. A span is one unit of work whose stall
;;; an operator would feel — the shells wrap each UI message pass (reduce
;;; plus any paint) — and a collection whose run time advances inside a span
;;; is a pause that landed where it could be seen. *GC-RUN-TIME* alone cannot
;;; say where a pause fell; splitting visible collection time from the hidden
;;; kind is the number the smoothness question actually needs.
;;;
;;; Single-writer by design: spans open and close on the one UI thread, and
;;; the forced-collection timer interrupts that same thread, so plain
;;; INCF/SETF need no lock. Internal-time units throughout, same as
;;; *GC-RUN-TIME*, for the reason GC-RUN-TIME's docstring gives.

(defun fresh-attribution ()
  "An empty attribution window: every count and time zero, no label."
  (list :spans 0 :gc-spans 0 :gc-time 0 :gc-worst 0 :gc-worst-label nil
        :span-time 0 :span-worst 0 :span-worst-label nil :span-worst-cpu 0))

;;; Its :SPAN-TIME, read next to :GC-TIME, separates 'the span was slow' from
;;; 'the span was collected in' — the two stutters need different fixes.
;;; Wall and CPU disagree in exactly one interesting way: a span whose wall
;;; time dwarfs its CPU time was preempted, so the stall was system load, not
;;; this program's work — the two hypotheses need different fixes and
;;; :SPAN-WORST-CPU is the only way to tell them apart from inside the image.
(defvar *attribution* (fresh-attribution)
  "The window since the last reset, a plist.
:SPANS is how many attributed work spans have closed, :GC-SPANS how many of
them a collection ran inside — each one a pause that landed in visible work
rather than a hidden window.
:GC-TIME is the internal-time units of collection that fell inside spans,
:SPAN-TIME the units all spans took, collections included. :GC-WORST is the
largest single-span collection time and :SPAN-WORST the longest single span,
each with the *ATTRIBUTION-LABEL* of the span that set it (:GC-WORST-LABEL,
:SPAN-WORST-LABEL); :SPAN-WORST-CPU is the CPU time of that longest span.")

;;; The shells bind it per message pass — the message's type, refined to the
;;; sync payload kind once the gateway shell has decoded it — and
;;; NOTE-ATTRIBUTED-SPAN snapshots it when a span sets a new worst. A worst
;;; without a label says 'something took 122ms'; the label says which kind of
;;; pass did, which is the difference between a mystery and a code path.
(defvar *attribution-label* nil
  "What the current span is doing, named by its opener.")

;;; EQUAL test because labels are symbols locally and payload-kind strings
;;; from the gateway shell. Vector slots mutate in place, so a span costs no
;;; consing after its label's first appearance.
(defvar *attribution-by-label*
  (make-hash-table :test #'equal)
  "Per-label accumulators: label -> #(count wall-time gc-time).")

(defun note-attributed-span (gc-delta span-delta cpu-delta &aux (window *attribution*))
  "Fold one closed span into the counters."
  ;; GC-DELTA is the *GC-RUN-TIME* advance inside the span, SPAN-DELTA its
  ;; wall duration, CPU-DELTA its run-time duration; internal-time units.
  (incf (getf window :spans))
  (incf (getf window :span-time) (max 0 span-delta))
  (when (> span-delta (getf window :span-worst))
    (setf (getf window :span-worst) span-delta
          (getf window :span-worst-cpu) (max 0 cpu-delta)
          (getf window :span-worst-label) *attribution-label*))
  (let ((tally (alexandria:ensure-gethash *attribution-label* *attribution-by-label*
                                          (vector 0 0 0))))
    (incf (svref tally 0))
    (incf (svref tally 1) (max 0 span-delta))
    (incf (svref tally 2) (max 0 gc-delta)))
  (when (plusp gc-delta)
    (incf (getf window :gc-spans))
    (incf (getf window :gc-time) gc-delta)
    (when (> gc-delta (getf window :gc-worst))
      (setf (getf window :gc-worst) gc-delta
            (getf window :gc-worst-label) *attribution-label*))))

(defmacro with-gc-attribution (&body body &aux (gc-before (gensym "GC-BEFORE"))
                                               (started (gensym "STARTED"))
                                               (cpu-started (gensym "CPU-STARTED")))
  "Run BODY as one attributed span, recording on every exit path."
  ;; UNWIND-PROTECT rather than a values-prog1 because the shells' update
  ;; methods leave through RETURN-FROM on the quit path and through signals on
  ;; provider errors — the spans that stall longest are exactly the ones
  ;; likeliest to exit sideways, and an instrument that drops them lies in the
  ;; direction that matters.
  `(let ((,gc-before sb-ext:*gc-run-time*)
         (,started (get-internal-real-time))
         (,cpu-started (get-internal-run-time)))
     (unwind-protect (progn ,@body)
       (note-attributed-span (- sb-ext:*gc-run-time* ,gc-before)
                             (- (get-internal-real-time) ,started)
                             (- (get-internal-run-time) ,cpu-started)))))

(defun gc-attribution-stats ()
  "A plist snapshot of the attribution window:
(:spans :gc-spans :gc-time :gc-worst :gc-worst-label :span-time :span-worst
:span-worst-label :span-worst-cpu :by-label)."
  ;; Times in internal-time units;
  ;; one call for the same reason as COLLECTION-STATS. :BY-LABEL is an alist of
  ;; (label count wall-time gc-time), heaviest wall time first — fresh lists, so
  ;; a consumer cannot reach the live accumulators through it.
  (append *attribution*
          (list :by-label
                (sort (nreverse (loop for label being the hash-keys of *attribution-by-label*
                                        using (hash-value tally)
                                      collect (list label (svref tally 0) (svref tally 1)
                                                    (svref tally 2))))
                      #'> :key #'third))))

(defun reset-gc-attribution ()
  "Zero the attribution window."
  ;; The instrument answers before/after questions; a window that only ever
  ;; grows cannot.
  (setf *attribution* (fresh-attribution))
  (clrhash *attribution-by-label*)
  (values))

;;; --- the organism's view of its own collection cost ---------------------

(defun gc-run-time ()
  "Total microseconds this image has spent collecting, from SB-EXT:*GC-RUN-TIME*."
  ;;
  ;; Exported so the organism can read its own collection cost.
  ;; Microseconds because that is the unit SBCL keeps it in
  ;; (INTERNAL-TIME-UNITS-PER-SECOND is 1,000,000 here) and converting would invite a
  ;; caller to compare a converted number against a raw one.
  sb-ext:*gc-run-time*)

(defun collection-stats ()
  "A plist of what the organism knows about its own collection behaviour:
(:gc-run-time :last-pause :forced-collections :idle-collections
:image-releases :image-bytes :nursery :heap-bytes :resident-bytes
:last-trim)."
  ;; :RESIDENT-BYTES is the process as the machine counts it and :IMAGE-BYTES
  ;; how much of that is the program's own pages, both NIL where the
  ;; platform does not say.
  ;;
  ;; One call rather than ten accessors because a reader wants a snapshot
  ;; taken at one instant, not ten reads that drift apart. It walks the
  ;; process's mappings to answer :IMAGE-BYTES, so it is a snapshot a surface
  ;; asks for, not a number to read per frame.
  (list :gc-run-time (gc-run-time)
        :last-pause *last-gc-pause*
        :forced-collections *forced-collections*
        :idle-collections *idle-collections*
        :image-releases *image-releases*
        :image-bytes (image-resident-bytes)
        :nursery (sb-ext:bytes-consed-between-gcs)
        :heap-bytes (sb-kernel:dynamic-usage)
        :resident-bytes (resident-bytes)
        :last-trim *last-trim*))
