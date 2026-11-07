;;;; cell.lisp --- the directory, the prompt, what a node has left, the watch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; team@N (arXiv 2609.21032): N identical sessions work one task from one
;;;; directory, and the directory is the whole protocol. Nothing here relays a
;;;; message, assigns a role or runs a controller: a node claims its slot by
;;;; an atomic mkdir, publishes through LOG, and scores through ./verify, the
;;;; one writer of SCORES. The nodes are ORDINARY sessions -- every tool,
;;;; their own transcript, a row on /sessions under the session that opened them.
;;;;
;;;; What the cell owns is six things. The layout (TEAM:OPEN). The seat:
;;;; /team TEXT puts TEXT to the session's standing team, seated through the
;;;; same open at the first /team and the same nodes after it, a spent
;;;; node's seat taken by a fresh one at the next -- a team with no scorer,
;;;; whose product is LOG, best/ and work-K/. Nothing else the operator types
;;;; reaches it: the first real runs seated every input, a status question
;;;; included, and the nodes billed three and a half times what their
;;;; session did. The one paragraph every node reads, byte-identical
;;;; (*PROMPT*, or *UNSCORED* for a team with no scorer). What a node has
;;;; LEFT: advice on NLE:TURN-BUDGET answering the configured budget minus
;;;; the session's summed turn.usage minus the time since the team opened --
;;;; a standing team's clock re-based at each /team -- so a node prompted
;;;; again never starts a fresh token budget: the kernel enforces it,
;;;; refusing tool calls past it. What a node keeps: nothing past the image --
;;;; advice on NLK:RECORD-DEFINITIONS keeps its definitions out of the
;;;; operator's layer. And the parent's side: TEAM:WATCH blocks until SOLVED,
;;;; every node settled, or the deadline, then answers the best -- the
;;;; SCORES maximum, confirmed by one more run of the same ./verify the nodes
;;;; ran. A form that blocks is backgrounded by the eval substrate and its
;;;; exit wakes the session, so the watch is a plain loop and the parent is
;;;; never the one waiting.
;;;;
;;;; Config, a sibling top-level key next to `cron' and `qa':
;;;;   "team": {"nodes": 3, "budget_tokens": 1500000, "budget_seconds": 7200}
;;;; A team is not config: it lives in its directory, written by TEAM:OPEN or
;;;; by /team.

(in-package #:nodecode-team)

;;; --- the manual ---------------------------------------------------------------
;;; (help :team) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter +primer+
  "A team is available through the nodecode-team cell: Lisp functions in the team: package,
called through eval. Every one returns a string; every refusal is ERROR: TEAM-ERROR with the
reason. A team is identical full-tool sessions working one task in parallel from one shared
directory; they never message each other or you, the directory's files are the only channel,
and each runs under a fixed budget of tokens and time.
The operator seats a session's standing team from their shell with /team TEXT: team.nodes nodes
under the session, working from team/<session>/standing/ in the home, each given TEXT behind its
own marker. Every later /team TEXT goes to the same nodes, a steer of the turn a node is running;
nothing else the operator types reaches them, and between inputs they sit idle. /team alone
reads where the team stands and /team stop stops it. A standing team's seconds count from the
latest /team, and a node's tokens are its cap over every input it takes: a node that has spent
them is never prompted again, and at the next /team a fresh node takes its seat. A standing team
has no scorer: its product is LOG, best/ and work-K/ in that directory, and nothing of it comes
back into the session by itself; (team:best DIR) reads it. A relative DIR is the calling
session's own directory's.
  (team:open \"task\" &key score dir)   make a directory and start team.nodes nodes (1 to 5) on
      TASK, answer the directory; call it again for more. SCORE is an executable, copied in as
      ./score: `./score CANDIDATE' prints one number on its last line (higher is better) and
      exits 0 only when the task is met, and the nodes score through `./verify K CANDIDATE',
      which runs it and records SCORES. Without SCORE the team is unscored, like the standing
      one: no ./score, ./verify or SCORES, and its best answers none.
  (team:watch \"dir\")   block until SOLVED exists, every node has settled or the deadline
      passes, then answer the best: the SCORES maximum, confirmed by one more ./verify run.
      It outlives the call's window: it keeps running in the background and its exit starts
      your next turn. Never poll for it: end your turn.
  (team:best \"dir\")   the same answer now, without waiting.
  (team:stop \"dir\")   stop every node; the directory stays.
The nodes are sessions team-<team>-node-<k>: TEAM is the identifier of the session they hang
under, whose standing team and every team it opens count K up together, and each node's input
opens with that same [team-<team>-node-<k>] marker. /sessions draws the nodes under their
session, and (recall-log :session-id ...) reads one. A node cannot open or
seat a team, and nothing a node defines is kept in the organism's layer."
  "What (help :team) answers while the cell runs.")

;;; --- the prompt ---------------------------------------------------------------
;;; Data, so a layer can reword it. A format control over three arguments,
;;; the node count, the highest slot number and the opener's directory (or
;;; NIL); every node's first input is the task's text then the paragraph,
;;; byte-identical. A team of one reads the task alone.

(defparameter *prompt*
  "You are one of ~d identical agents working the same task in parallel from this ~
directory, each with this same prompt; there are no roles and nobody assigns work. You never ~
message a peer: everything you share goes through the files here, and LOG is the channel. ~
Before anything else read TASK.md, LOG, SCORES, DISCONFIRMED, ADOPTED and every ~
slots/*/approach. Then claim a slot: run `for i in $(seq 0 ~d); do mkdir slots/slot-$i ~
2>/dev/null && echo $i && break; done` -- the mkdir that succeeds is yours, K; if none ~
succeeds the team is full, so say so and stop. Write slots/slot-K/approach: your approach in ~
one paragraph and what you deliberately will not assume, DISTINCT from every approach already ~
claimed -- if a lower slot took yours, change yours. Work only inside work-K/; peers may read ~
it, and you never write in another slot's. The Lisp image is shared like the directory: name ~
anything you define sK-... so you never redefine a peer's function or the organism's; nothing ~
you define is kept past this image. Score ~
every real attempt with `./verify K <candidate>`: it prints the score and records it in ~
SCORES itself, so never write SCORES by hand. Append one line to LOG with `./log K KIND \"TEXT\"`, which writes `[node K <UTC time>] KIND: ~
TEXT` in one atomic write, a newline always -- only for measured progress, giving the score, ~
the exact command that reproduces it, and the lineage (whose idea it builds on); label a weak ~
claim weak. Spend part of your effort trying to falsify the leading idea, yours included, and ~
send what failed and the command that showed it through `./log K fail \"TEXT\"`. Re-read LOG and SCORES every few attempts. Adopt a ~
peer's approach only when SCORES shows it clearly beating yours on a result you reproduced, or ~
yours is blocked; record the adoption in ADOPTED with why, and keep one meaningful variation ~
of your own (a parameter, a subcase, a representation, a fallback). best/ is the team's one ~
graded artifact: replace it only while holding `flock best.lock`, only after re-checking its ~
current score under the lock, and only with a strictly better verified candidate. Every turn ~
takes a real action and reads its result; bookkeeping is never a substitute for one. When ~
`./verify` exits 0 the task is met: promote the candidate to best/, write SOLVED (your slot, ~
the score, the reproducing command) and stop. If SOLVED already exists, reproduce it once with ~
./verify, append the result to LOG, and stop. Otherwise keep improving until your budget is ~
spent; when the spend line says it is nearly spent, log where you stand and what you would ~
try next."
  "The paragraph every node of a team of two or more reads after the task.")

(defparameter *unscored*
  "You are one of ~d identical agents working the same task in parallel from this directory, ~
each with this same prompt; there are no roles and nobody assigns work.~* Every input you are ~
given opens with your own marker, [team-<team>-node-<k>]: set it aside and do what the rest ~
asks. That rest is the task, put to every one of you alike; when this team stands under a ~
session it is what the operator put to the team from that session, which may lean on a ~
conversation you never saw, so read it as the whole task and do what it plainly asks.~@[ The session it came ~
from works in ~a: read what the task names there, but never write there -- what you change, ~
you change in a copy inside work-K/.~] K is the number that ends your marker: work only inside ~
work-K/ (make it), peers may read it, and you never write in another's. The Lisp image is ~
shared like the directory: name anything you define sK-... so you never redefine a peer's ~
function or the organism's; nothing you define is kept past this image. You never message a peer: everything you share goes through the ~
files here, and LOG is the channel. Read the team's standing with `(team:best \".\")` -- the ~
last findings and the size of every file -- and open a file only for the one thing you need; ~
re-read it every few actions rather than the files whole. Append with `./log K KIND \"TEXT\"`, ~
which writes `[node K <UTC time>] KIND: TEXT` in one atomic write, a newline always: one line ~
per result a peer should know -- what you did, what it showed, the exact command that ~
reproduces it, and whose idea it builds on; label a weak claim weak. A failure and the command ~
that showed it go to DISCONFIRMED with `./log K fail \"TEXT\"`. Adopt a peer's approach only when LOG ~
shows it clearly beating yours on a result you reproduced, or yours is blocked; record the ~
adoption in ADOPTED with why, and keep one meaningful variation of your own. This team has no ~
scorer: there is no ./score or ./verify, and you never write SCORES or SOLVED. best/ is the ~
team's one shared artifact: replace it only while holding `flock best.lock`, and only with ~
what you have shown is better. Every turn takes a real action and reads its result; ~
bookkeeping is never a substitute for one. When the input is done, log what you did and end ~
your turn: between inputs you sit idle. When the spend line says your budget is nearly spent, ~
log where you stand and what you would try next."
  "The paragraph every node of an unscored team of two or more reads after
the task -- a standing team's, or a TEAM:OPEN without :SCORE. A format
control over the node count, the highest slot (unused) and the directory of
the session the team hangs under, or NIL.")

(defparameter *continue*
  "Your budget is not spent and the task is not met. Re-read LOG and SCORES, then keep ~
working from where you stopped."
  "What a node that went idle with budget left is prompted with, in its own
session: the seat count never changes, so this is supervision, not
recruitment.")

(defparameter *resubmits* 3
  "How many times one node is prompted again after going idle.")

(defparameter *reserve* 1/20
  "The share of a budget under which an idle node is left idle.")

(defparameter *grace-polls* 4
  "How many polls the nodes still running get, once SOLVED exists or the
deadline has passed, before the watch stops them.")

(defun node-prompt (task n &optional (paragraph *prompt*) project)
  "The shared body of a node's first input: TASK, then PARAGRAPH over N, the
highest slot and PROJECT unless the team is one. Byte-identical for every
node of one team; the node's own marker (NODE-INPUT) leads it."
  (if (> n 1)
      (format nil "~a~%~%~?" task paragraph (list n (1- n) project))
      task))

(defun node-command (id command)
  "Node ID's command id for the input COMMAND names: `<node>:<command>',
so the same input twice is one admission; NIL, one SUBMIT generates."
  (and command (format nil "~a:~a" id command)))

(defun node-input (id text)
  "TEXT as node ID's input: the node's marker, then the text."
  (format nil "[~a] ~a" id text))

;;; --- the directory ----------------------------------------------------------------

(defparameter +verify+
  "#!/bin/sh
# ./verify K CANDIDATE -- score CANDIDATE with ./score and record the line in SCORES.
# The one writer of SCORES: a score is never self-reported. Exits 0 only when
# ./score says the task is met. The parent confirms the best with this same command.
cd \"$(dirname \"$0\")\" || exit 2
[ $# -eq 2 ] || { echo 'usage: ./verify K CANDIDATE' >&2; exit 2; }
out=$(./score \"$2\"); met=$?
n=$(printf '%s\\n' \"$out\" | tail -n 1)
case \"$n\" in
  ''|*[!0-9.-]*) echo \"./score did not print a number: $n\" >&2; exit 2 ;;
esac
[ \"$met\" -eq 0 ] && m=1 || m=0
( flock 9; printf '[slot %s %s] score=%s met=%s ref=%s\\n' \"$1\" \"$(date -u +%H:%M:%SZ)\" \"$n\" \"$m\" \"$2\" >&9 ) 9>>SCORES
echo \"score=$n met=$m\"
[ \"$m\" -eq 1 ]
"
  "The verifier TEAM:OPEN writes: the nodes' command and the parent's.")

(defparameter +log+
  "#!/bin/sh
# ./log K KIND TEXT... -- append one line to LOG, or to DISCONFIRMED for a
# failure and ADOPTED for an adoption. One write, one line, a newline always:
# three writers appending long lines by hand cut one entry mid-command.
cd \"$(dirname \"$0\")\" || exit 2
k=$1; kind=$2; shift 2
{ [ -n \"$k\" ] && [ $# -ge 1 ]; } || { echo 'usage: ./log K KIND TEXT...' >&2; exit 2; }
case \"$kind\" in
  fail|failed|failure|disconfirmed) file=DISCONFIRMED ;;
  adopt|adopted|adoption) file=ADOPTED ;;
  *) file=LOG ;;
esac
line=\"[node $k $(date -u +%Y-%m-%dT%H:%M:%SZ)] $kind: $*\"
( flock 9; printf '%s\\n' \"$line\" >&9 ) 9>>\"$file\"
printf '%s\\n' \"$line\"
"
  "The appender TEAM:OPEN writes into every directory: one flock-protected
write per line, so two writers never interleave and no line lands cut.")

(defparameter +shared-files+ '("LOG" "DISCONFIRMED" "ADOPTED" "best.lock")
  "The append-only files and the lock every fresh directory holds, empty.
SCORES is a scored team's alone, beside its ./verify. SOLVED is not among
them: its presence is the signal, written once by the node whose ./verify
exited 0.")

(defun file (dir name)
  (merge-pathnames name dir))

(defun node-id (team k)
  "A node's session id: TEAM is the team identifier — the id of the session
the node hangs under, its standing team and every team it opens alike — and
K is the node's own identifier inside it, counted from zero."
  (format nil "team-~a-node-~d" team k))

(defun fresh-node-ids (owner count)
  "COUNT node ids under OWNER, each the first no session holds and no
earlier one names: nodes are minted as they are needed, so a session's
second team, or a standing seat's replacement, carries on where the
numbers stopped."
  (loop for k from 0
        for id = (node-id owner k)
        until (<= count 0)
        unless (nlk:session-exists-p id) collect id and do (decf count)))

(defun fresh-dir-tag ()
  "A short tag for one team's directory: no two teams of a session share one."
  (loop for n from 0
        for tag = (string-downcase (format nil "~36r" (+ (get-universal-time) n)))
        unless (probe-file (nlk:home (format nil "team/~a/~a/"
                                             (or nlk:*scribe-session-id* "detached") tag)))
          return tag))

(defun read-config (dir &aux (path (file dir "team.sexp")))
  "DIR's team.sexp, the plist TEAM:OPEN wrote, or NIL."
  (and (probe-file path) (uiop:with-safe-io-syntax () (uiop:read-file-form path))))

(defun resolve-dir (dir)
  "DIR as a directory pathname. A relative one is under the calling
session's own directory, as `sh' reads it -- (team:best \".\") from a node
is its team -- and under the image's defaults only outside a session."
  (let* ((directory (uiop:ensure-directory-pathname dir))
         (cwd (and nlk:*scribe-session-id* (nlk:find-session-cwd nlk:*scribe-session-id*)))
         (merged (if (and cwd (uiop:relative-pathname-p directory))
                     (merge-pathnames directory (uiop:ensure-directory-pathname cwd))
                     directory)))
    (or (ignore-errors (probe-file merged)) merged)))

(defun team-directory (dir &aux (directory (resolve-dir dir))
                                (config (ignore-errors (read-config directory))))
  "=> (values DIRECTORY CONFIG) for the team at DIR, or a refusal."
  (unless config
    (fail "no team at ~a: (team:open ...) answers the directory of the one it made" dir))
  (values directory config))

(defun write-config (dir config)
  "CONFIG as DIR's team.sexp: one plist, read back by READ-CONFIG."
  (nlk:write-file-atomically (file dir "team.sexp")
                             (with-standard-io-syntax
                               (let ((*print-readably* nil))
                                 (prin1-to-string config)))))

(defun scored-p (dir)
  "Whether the team at DIR has a scorer: ./score is the one mark of it."
  (and (probe-file (file dir "score")) t))

(defun write-layout (dir task score config)
  "The whole directory of a fresh team under DIR. A scored team -- SCORE
copied in, or a ./score already there -- also holds ./verify, SCORES and
slots/; an unscored one holds none of them."
  (ensure-directories-exist (file dir "best/"))
  (nlk:write-file-atomically (file dir "TASK.md") task)
  (dolist (name +shared-files+)
    (nlk:write-file-atomically (file dir name) ""))
  ;; ./log is the one appender: three writers appending long lines by hand
  ;; cut a first-run entry mid-command, and the next write continued on the
  ;; same line.
  (nlk:write-file-atomically (file dir "log") +log+ :mode #o755)
  (when score
    (uiop:copy-file score (file dir "score")))
  (when (scored-p dir)
    (ensure-directories-exist (file dir "slots/"))
    (nlk:write-file-atomically (file dir "SCORES") "")
    (nlk:write-file-atomically (file dir "verify") +verify+ :mode #o755)
    (sb-posix:chmod (namestring (file dir "score")) #o755))
  (write-config dir config))

;;; --- the seams ---------------------------------------------------------------------
;;; The two places the cell reaches a node's turn; a test stubs neither.

(defun submit-prompt (session prompt command origin &key steer)
  "PROMPT into SESSION through the one in-process ingress => the
disposition. Provenance: source kind `team', source id the opener; a NIL
COMMAND is generated, as SUBMIT would. STEER is the running turn PROMPT
steers, or NIL for a follow-up."
  (nlk:active-input-admission-disposition
   (nle:submit session prompt :steer steer :command-id command :source "team" :source-id origin)))

(defun node-turns (id)
  "How many turns node ID was ever admitted."
  (nlk:events :session-id id :kind "turn.started" :as :count))

(defun node-active-turn (id)
  "The id of node ID's turn without a terminal fact, or NIL when idle."
  (first (nlk::active-turn-row id)))

;;; --- what a node has left -----------------------------------------------------------

(defun node-spend (id &aux (spend (list :input 0 :output 0 :reasoning 0)))
  "What node ID has spent, summed over every turn it ran: (:INPUT N :OUTPUT N
:REASONING N). A count a round left unknown adds nothing."
  (dolist (payload (nlk:events :session-id id :kind "turn.usage" :as :payloads) spend)
    (loop for (key field) on '(:input "input-tokens" :output "output-tokens"
                               :reasoning "reasoning-tokens") by #'cddr
          for value = (gethash field payload)
          when (integerp value) do (incf (getf spend key) value))))

(defun node-left (id config &optional (now (get-universal-time)) &aux (spend (node-spend id)))
  "=> (values TOKENS SECONDS): what node ID has left of CONFIG's budget,
either of them zero or less once spent."
  ;; The tokens are the kernel's billable ones -- uncached input, output and
  ;; reasoning -- summed over the session, so a turn prompted again starts
  ;; from what the turns before it left. The clock is the team's, not the
  ;; turn's: it started when the team opened, or a standing team's at the
  ;; operator's latest input (SEAT).
  (values (- (getf config :tokens)
             (getf spend :input) (getf spend :output) (getf spend :reasoning))
          (- (getf config :seconds) (- now (getf config :opened-at)))))

(defun node-spent-p (id config &optional (now (get-universal-time)))
  "Whether node ID has *RESERVE* or less of either of CONFIG's budgets left:
a spent node is never prompted again."
  (multiple-value-bind (tokens seconds) (node-left id config now)
    (or (<= tokens (* *reserve* (getf config :tokens)))
        (<= seconds (* *reserve* (getf config :seconds))))))

(defun node-config (id)
  "The config of the team session ID is a node of, or NIL."
  (when (and (stringp id) (uiop:string-prefix-p "team-" id))
    (let* ((cwd (nlk:find-session-cwd id))
           (config (and cwd (ignore-errors
                             (read-config (uiop:ensure-directory-pathname cwd))))))
      (and (member id (getf config :nodes) :test #'equal)
           config))))

(defun budget-hook (next turn &aux (id (getf turn :session-id))
                                   (config (node-config id)))
  "Advice on NLE:TURN-BUDGET: a node's turn runs under what its session
has left; every other session falls to NEXT."
  ;; A spent node's turn still opens -- under one token and one second, so
  ;; its first tool call is refused and it answers with where it stands.
  (if config
      (multiple-value-bind (tokens seconds) (node-left id config)
        (list :tokens (max 1 tokens) :seconds (max 1 seconds)))
      (funcall next turn)))

;;; --- open ----------------------------------------------------------------------------

(defun mint-nodes (dir origin nodes)
  "Mint NODES as sessions working from DIR under ORIGIN, the session they
hang under, or NIL. DIR's team.sexp already lists them."
  ;; Each node is minted as a DETACHED node of the opener: the edge is
  ;; recorded, so every surface reads the team as that session's own —
  ;; the /sessions picker draws the seats under the session that opened
  ;; them — while detached keeps them what a team is, identical sessions
  ;; that begin fresh and compose none of the opener's history. An
  ;; opener no session answers for (a snippet outside a turn) leaves them
  ;; roots. The opener's model pin and its reasoning ride each, carried by
  ;; the create; a node without one follows the gateway default, as the
  ;; opener does.
  (dolist (id nodes)
    (nlk:create-session :id id :cwd (namestring dir)
                        :parent (and origin (nlk:session-exists-p origin) origin)
                        :detached t)))

(defun start-nodes (dir origin nodes task n command)
  "Put TASK to freshly minted NODES as their first input: TASK, then the
paragraph of DIR's kind of team over N nodes."
  ;; A node's cwd stays the team's directory -- the guard and the budget
  ;; both read it -- so the opener's own directory rides in the task.
  (let ((prompt (node-prompt task n (if (scored-p dir) *prompt* *unscored*)
                             (nlk:find-session-cwd origin))))
    (dolist (id nodes)
      (submit-prompt id (node-input id prompt) (node-command id command) origin))))

(defun open-team (dir origin task &key score tag command)
  "Write a fresh team's directory at DIR and start team.nodes nodes on TASK:
the one path TEAM:OPEN and a session's standing seat both take. ORIGIN is
the session the nodes hang under, or NIL; SCORE the scorer to copy in, NIL
for none; COMMAND what each node's first command id names (NODE-COMMAND).
=> the node ids."
  (let* ((n (setting :nodes))
         (nodes (fresh-node-ids (or origin "detached") n)))
    ;; team.sexp lists every node before its first submit: the budget advice
    ;; reads it as the turn opens, so no first turn runs uncapped.
    (write-layout dir task score
                  (list :tag tag :nodes nodes
                        :tokens (setting :budget-tokens) :seconds (setting :budget-seconds)
                        :opened-at (get-universal-time) :origin origin))
    (mint-nodes dir origin nodes)
    (start-nodes dir origin nodes task n command)
    nodes))

(define-verb open (task &key score dir)
  "Make a team's directory and start its nodes on TASK. Answers the
directory: each node is a session named team-<team>-node-<k>, TEAM the
identifier of the session that opened it."
  (unless (and (stringp task) (plusp (length (string-trim '(#\Space #\Newline) task))))
    (fail "task must be a non-empty string"))
  (unless (nlk:store-open-p)
    (fail "no store is open; a node is a durable session"))
  (let ((origin nlk:*scribe-session-id*))
    (when (node-config origin)
      (fail "a team node cannot open a team"))
    (let* ((tag (fresh-dir-tag))
           (dir (resolve-dir (or dir (nlk:home (format nil "team/~a/~a/" (or origin "detached") tag))))))
      (when (probe-file (file dir "team.sexp"))
        (fail "~a already holds a team" dir))
      (when (and score (not (probe-file score)))
        (fail ":score names the scorer, an executable file: `./score CANDIDATE' prints ~
               one number and exits 0 only when the task is met; there is no ~a" score))
      (let ((nodes (open-team dir origin task :score score :tag tag :command "0")))
        (format nil "team ~a opened in ~a: ~d node~:p (~{~a~^ ~}), each under ~:d tokens and ~
                     ~:d s from now~:[, unscored~;~]. (team:watch ~s) waits for the result."
                tag (namestring dir) (length nodes) nodes
                (setting :budget-tokens) (setting :budget-seconds) (scored-p dir)
                (namestring dir))))))

;;; --- the seat ------------------------------------------------------------------------
;;; /team TEXT is the operator's statement of work for a session's standing
;;; team, and the only one: the first real runs seated a team on every
;;; operator input -- "Please cancel all sessions please", a status question
;;; -- and the nodes billed three and a half times what their session did.
;;; The directory and the store are the whole state; between inputs the
;;; nodes sit idle.

(defparameter +usage+ "/team TEXT | /team | /team stop")

(defun standing-dir (session-id)
  "SESSION-ID's standing team directory: one per session, for its whole life."
  (uiop:ensure-directory-pathname (nlk:home (format nil "team/~a/standing/" session-id))))

(defun put-input (id prompt command origin)
  "PROMPT to node ID: a steer of the turn it is running, a follow-up to a
node running none."
  (let ((input (node-input id prompt))
        (command (node-command id command)))
    (handler-case (submit-prompt id input command origin :steer (node-active-turn id))
      ;; the turn ended between the look and the steer: a follow-up, then
      (nlk:turn-lifecycle-error ()
        (submit-prompt id input command origin)))))

(defun seat (session-id prompt command &aux (dir (standing-dir session-id))
                                            (config (read-config dir)))
  "PROMPT put to SESSION-ID's standing team: seated through OPEN-TEAM at the
first /team, the same nodes at every later one, a spent node's seat taken by
a fresh node. COMMAND names each node's command id (NODE-COMMAND).
=> the node ids PROMPT was put to."
  (unless config
    (return-from seat (open-team dir session-id prompt :tag "standing" :command command)))
  ;; The clock is the task's: it re-bases at every input, so the seconds
  ;; are this input's window. The tokens stay each node's lifetime cap, so
  ;; a node with *RESERVE* or less of them left is spent: it is never
  ;; prompted again -- its turn would still open and pay for a whole
  ;; context -- and a fresh node, the next free K, takes its seat now.
  ;; The spent node stays on the roster, so its guard and its cap stand.
  (setf (getf config :opened-at) (get-universal-time))
  (let* ((n (setting :nodes))
         (roster (getf config :nodes))
         (usable (remove-if (lambda (id) (node-spent-p id config)) roster))
         (kept (subseq usable 0 (min n (length usable))))
         (fresh (fresh-node-ids session-id (- n (length kept)))))
    (setf (getf config :nodes) (append roster fresh))
    (write-config dir config)
    (dolist (id kept)
      (put-input id prompt command session-id))
    (mint-nodes dir session-id fresh)
    (start-nodes dir session-id fresh prompt n command)
    (append kept fresh)))

(define-verb run-slash (args session-id)
  "/team: TEXT seats SESSION-ID's standing team with TEXT, nothing reads
where it stands, `stop' stops it => the answer's text."
  (unless (nlk:store-open-p)
    (fail "no store is open; a node is a durable session"))
  (unless (and session-id (or (nlk:session-exists-p session-id) (nlk:on-standby-p session-id)))
    (fail "this shell has no session to seat a team under"))
  (when (node-config session-id)
    (fail "a team node seats no team"))
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or args "")))
         (dir (standing-dir session-id))
         (config (ignore-errors (read-config dir))))
    (cond ((and (string= text "") (null config))
           (format nil "team: no standing team yet; ~a" +usage+))
          ((string= text "")
           (result-text (result dir config :confirm nil)))
          ((string-equal text "stop")
           (if config
               (stop (namestring dir) "the operator stopped the team")
               "team: no standing team to stop"))
          (t
           ;; A shell that has not prompted yet stands by, and /team TEXT in it
           ;; says "put this to my team": a session with a team is a session.
           (when (nlk:on-standby-p session-id)
             (nlk:materialize-standby-session session-id))
           (let ((nodes (handler-case (seat session-id text (nlk:make-durable-id "command"))
                          (team-error (condition) (error condition))
                          (error (condition)
                            (fail "the standing team was not seated: ~a" condition)))))
             (format nil "team: ~d node~:p took it (~{~a~^ ~}); /team reads where they stand"
                     (length nodes) nodes))))))

;;; --- what a node keeps ---------------------------------------------------------------

(defun layer-hook (next form form-string end &rest keys
                   &key (session-id nlk:*scribe-session-id*) &allow-other-keys)
  "Advice on NLK:RECORD-DEFINITIONS: a node's definitions are its task's
scratch, live in the image and never kept in the operator's layer -- the
first real runs filed four there, one writing a dead team's LOG from every
boot. Every other session falls to NEXT."
  (unless (node-config session-id)
    (apply next form form-string end keys)))

;;; --- best ----------------------------------------------------------------------------

(defparameter +score-line-regex+
  "^\\[slot (\\S+) [^\\]]*\\] score=(-?[0-9]+(?:\\.[0-9]+)?) met=([01]) ref=(.+)$"
  "One SCORES line as ./verify writes it: the slot, the score, met, the candidate.")

(defparameter +verify-output-regex+ "score=(-?[0-9]+(?:\\.[0-9]+)?) met="
  "What ./verify prints once it has scored a candidate.")

(defun parse-number (text)
  "TEXT, digits one of the two regexes matched, as a number."
  (with-standard-io-syntax
    (let ((*read-default-float-format* 'double-float))
      (read-from-string text))))

(defun parse-score-line (line)
  "A SCORES line as (:SLOT :SCORE :MET :REF), or NIL for a line ./verify did
not write."
  (ppcre:register-groups-bind (slot score met ref) (+score-line-regex+ line)
    (list :slot slot :score (parse-number score) :met (string= met "1") :ref ref)))

(defun read-scores (dir &aux (path (file dir "SCORES")))
  "Every line of DIR's SCORES that parses, oldest first."
  (and (probe-file path)
       (remove nil (mapcar #'parse-score-line (uiop:read-file-lines path)))))

(defun run-verify (dir ref)
  "One run of DIR's ./verify on REF as slot `parent' => (values SCORE MET),
SCORE NIL when it printed none, an hour at most."
  (nlk:bind (((output _ status) (nlk:run-bounded (list "./verify" "parent" ref)
                                                 :directory dir :seconds 3600 :error-output nil)))
    (ppcre:register-groups-bind (score) (+verify-output-regex+ output)
      (values (parse-number score) (eql status 0)))))

(defparameter *confirm-runs* 5
  "How many candidates one TEAM:BEST may run ./verify on.")

(defun confirm-best (dir)
  "The best of DIR's SCORES that a fresh ./verify run stands behind, as
(:SLOT :SCORE :MET :REF :CLAIMED), or NIL."
  ;; The maximum is only a claim until the verifier is run once more on its
  ;; candidate: the ordinary path is that one run. A candidate that no
  ;; longer verifies, or verifies lower, loses to the next claim that beats
  ;; what is confirmed so far.
  ;; The claims: the nodes' lines, highest first, one per candidate -- the
  ;; parent's own confirming lines are no node's claim.
  (let ((claims (remove-duplicates
                 (stable-sort (remove "parent" (read-scores dir)
                                      :key (lambda (line) (getf line :slot)) :test #'equal)
                              #'> :key (lambda (line) (getf line :score)))
                 :key (lambda (line) (getf line :ref)) :test #'equal :from-end t))
        (confirmed nil))
    (loop for claim in claims
          repeat *confirm-runs*
          while (or (null confirmed) (> (getf claim :score) (getf confirmed :score)))
          do (multiple-value-bind (score met) (run-verify dir (getf claim :ref))
               (when (and score (or (null confirmed) (> score (getf confirmed :score))))
                 (setf confirmed (list :slot (getf claim :slot) :score score :met met
                                       :ref (getf claim :ref)
                                       :claimed (getf claim :score))))))
    confirmed))

(defun result (dir config &key (confirm t))
  "The team's standing as a plist: (:DIR :SOLVED :BEST :NODES); :BEST is
:UNSCORED for a team with no scorer, and :UNASKED unless CONFIRM, which
runs ./verify."
  (list :dir (namestring dir)
        :solved (and (probe-file (file dir "SOLVED")) t)
        :best (cond ((not (scored-p dir)) :unscored)
                    (confirm (confirm-best dir))
                    (t :unasked))
        :nodes (loop for id in (getf config :nodes)
                       collect (multiple-value-bind (tokens seconds) (node-left id config)
                                 (list :id id
                                       :active (and (node-active-turn id) t)
                                       :turns (node-turns id)
                                       :spend (node-spend id)
                                       :left-tokens (max 0 tokens)
                                       :left-seconds (max 0 seconds))))))

(defparameter *digest-findings* 12
  "How many of LOG's last findings a digest carries.")

(defparameter *digest-line-chars* 240
  "How much of one LOG line a digest carries.")

(defun log-findings (dir &optional (limit *digest-findings*))
  "The last LIMIT non-blank lines of DIR's LOG, trimmed, oldest first, or NIL."
  (last (remove "" (mapcar #'nlk:trimmed (nlk:lines (nlk:read-text (file dir "LOG"))))
                :test #'string=)
        limit))

(defun tree-bytes (path)
  "How many bytes PATH holds, files under it summed."
  (if (uiop:directory-pathname-p path)
      (loop for entry in (ignore-errors (uiop:directory* (merge-pathnames "*" path)))
            sum (tree-bytes entry))
      (or (nlk:file-bytes path) 0)))

(defun digest-inventory (dir)
  "What the team's directory holds, one line per file or directory, with
sizes -- the listing a reader would otherwise spend a context on."
  (append (loop for name in '("TASK.md" "LOG" "DISCONFIRMED" "ADOPTED")
                when (probe-file (file dir name))
                  collect (format nil "  ~a  ~:d chars" name (tree-bytes (file dir name))))
          (loop for sub in (ignore-errors (uiop:directory* (merge-pathnames "*/" dir)))
                collect (format nil "  ~a  ~:d file~:p  ~:d bytes"
                                (car (last (pathname-directory sub)))
                                (length (ignore-errors (uiop:directory* (merge-pathnames "*" sub))))
                                (tree-bytes sub)))))

(defun digest-text (dir &aux (findings (log-findings dir)))
  "The bounded read of an unscored team: its last findings, then what its
directory holds, with sizes -- what a reader would otherwise spend a whole
context reading raw."
  (format nil "last ~d finding~:p of LOG~:[~; -- none yet~]:~%~{~a~^~%~}~%the directory:~%~{~a~^~%~}"
          (length findings) (null findings)
          (mapcar (lambda (line) (format nil "  ~a" (nlk:clip line *digest-line-chars*))) findings)
          (digest-inventory dir)))

(defun result-text (result &aux (best (getf result :best)))
  "RESULT as the lines a verb answers."
  (format nil "team ~a: ~:[not solved~;SOLVED~]~%~a~%~{~a~^~%~}"
          (getf result :dir) (getf result :solved)
          (cond
            ((eq best :unscored)
             (format nil "best: none -- the team has no scorer; its product is the files.~%~a"
                     (digest-text (getf result :dir))))
            ((eq best :unasked)
             (format nil "best: (team:best ~s) confirms it with ./verify" (getf result :dir)))
            (best
             (format nil "best: score ~a~:[~; (task met)~] by slot ~a, artifact ~a, ~
                          confirmed by ./verify~@[ (SCORES claimed ~a)~]"
                     (getf best :score) (getf best :met) (getf best :slot) (getf best :ref)
                     (and (/= (getf best :score) (getf best :claimed)) (getf best :claimed))))
            (t "best: none -- no SCORES line was confirmed by ./verify"))
          (loop for row in (getf result :nodes)
                for spend = (getf row :spend)
                collect (format nil "~a  ~:[idle~;running~]  ~d turn~:p  spent ~:d in ~:d out ~
                                     ~:d reasoning  left ~:d tokens ~:d s"
                                (getf row :id) (getf row :active) (getf row :turns)
                                (getf spend :input) (getf spend :output) (getf spend :reasoning)
                                (getf row :left-tokens) (getf row :left-seconds)))))

(define-verb best (dir)
  "The team's best now: the SCORES maximum, confirmed by one ./verify run."
  (multiple-value-bind (dir config) (team-directory dir)
    (result-text (result dir config))))

;;; --- stop ----------------------------------------------------------------------------

(defun stop-nodes (config reason)
  "Ask every running node's turn to stop => how many were asked."
  (loop for id in (getf config :nodes)
        for turn = (node-active-turn id)
        count (and turn (nlk:request-cancel-turn id turn reason))))

(define-verb stop (dir &optional (reason "the team was stopped"))
  "Stop every node of the team at DIR. The directory stays as it is."
  ;; Nothing is scored here: a stop touches no file of the team's.
  (multiple-value-bind (dir config) (team-directory dir)
    (let ((asked (stop-nodes config reason)))
      (format nil "~d node~:p asked to stop; ~a is kept~%~a"
              asked (namestring dir) (result-text (result dir config :confirm nil))))))

;;; --- watch ---------------------------------------------------------------------------

(defun node-resumable-p (id config now)
  "Whether idle node ID is prompted again: not spent, and fewer than
*RESUBMITS* promptings behind it."
  (and (not (node-spent-p id config now))
       (<= (node-turns id) *resubmits*)))

(define-verb watch (dir &key (poll 15))
  "Block until the team at DIR is done -- SOLVED exists, every node has
settled, or the deadline passed -- then answer its best."
  ;; Stateless: what it knows it reads from the directory and the store on
  ;; every poll, so a watch that died with its image is run again as it was.
  ;; The command id names the node's turn count, so two watches never
  ;; prompt one idle twice. An unscored team is never prompted again: no
  ;; scorer says its task is unmet, so a node that went idle is done.
  (multiple-value-bind (dir config) (team-directory dir)
    (let ((deadline (+ (getf config :opened-at) (getf config :seconds)))
          (nodes (getf config :nodes))
          (scored (scored-p dir))
          (ending 0))
      (loop
        (let* ((now (get-universal-time))
               (closing (or (probe-file (file dir "SOLVED")) (>= now deadline)))
               (idle (remove-if #'node-active-turn nodes)))
          (when (and scored (not closing))
            (dolist (id idle)
              (when (node-resumable-p id config now)
                (submit-prompt id (node-input id *continue*)
                               (node-command id (node-turns id))
                               (getf config :origin))
                (setf idle (remove id idle)))))
          (when (= (length idle) (length nodes))
            (return))
          (when closing
            (when (>= ending *grace-polls*)
              (stop-nodes config "the team is done")
              (return))
            (incf ending)))
        (sleep poll))
      (result-text (result dir config)))))

;;; --- the entry: the one declaration ------------------------------------------------------

(nle:define-cell team
  (:section ("team")
    (:guide "A team is N identical full-tool sessions working one task from one shared directory, where the files are the only channel; nodes is how many (1 to 5; /team seats a session's standing team with that many, a spent node replaced at the next /team), budget_tokens bounds each node across every turn it runs, budget_seconds the team's clock")
    ("nodes" :integer :default 3 :min 1
               :doc "how many nodes /team seats under a session and one team:open starts, 1 to 5")
    ("budget_tokens" :integer :default 1500000 :min 1
                     :doc "billable tokens one node may spend, summed over its turns")
    ("budget_seconds" :integer :default 7200 :min 1
                      :doc "seconds a team runs, counted from when it opened; a standing team's from the latest /team"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (when (> (getf values :nodes) 5)
                 (nlk:config-error "team.nodes is ~d; a team is 1 to 5 sessions"
                                   (getf values :nodes)))
               values))
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook 'nlk:record-definitions #'layer-hook)
  (:help :team "team:open starts identical sessions on one task in parallel, scored or not; /team seats a standing team" +primer+)
  (:command "team" 'run-slash
            :catches 'team-error
            :description "The session's standing team: TEXT puts it to the team, nothing reads where it stands, stop stops it"
            :argument-hint "TEXT | stop"))
