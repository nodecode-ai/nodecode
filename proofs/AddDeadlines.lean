/-
Add deadlines: turning a cell on, as its waiters wait and as the organism
serving the home does it.

The waiters each send POST /api/gateway/cells {name, on} and hold a wall
clock on the answer: `nodecode add' (ADD-TEXT, src/src/surface/control.lisp),
the /cells sheet (src/src/tui/cells-sheet.lisp), the /models picker's pick
(src/src/surface/client.lisp) and `nodecode gateway setup'
(src/src/tui/ui.lisp). The work, on the owner (SWITCH-CELL-ON): the hub's two
indexes read (10 s each), then every folder of the closure installed
(CLONE-INSTALL, src/src/organism/cells.lisp) and loaded. A folder of a
repository unpacks the archive its index names (a 20 s download, a 60 s
tar), else checks out of the copy of its repository the home keeps (that
copy's lock, then init, fetch and checkout, 600 s each), else clones (clone
and checkout, 600 s each).

1. main 4017888. Every step is bounded on its own, the lock not at all, and
   the waiters wait 600 s (add), 300 s (the sheet, the picker) and 60 s
   (setup). `slowRun`: an archive that fails, a fetch that runs out its 600 s
   and a clone that then takes 590 s, every step within main's own bound,
   ends after 1 235 s; `main_waiters_quit`: every waiter has given up by
   then, add saying the cell "may still be coming on there" over an install
   that goes on and succeeds, and a retry runs beside it
   (`main_retry_overlaps`). `main_lock_unbounded`: behind another install's
   fetch, the lock is waited on for as long as that takes, any time at all.

2. The fix: one deadline for the whole of a turn-on. A waiter sends the
   seconds it waits, W, and waits W (CELL-SWITCH-SECONDS, 600, for every
   waiter; add's --wait). The owner gives the work W - margin(W), the margin
   min(30 s, W / 2) kept for what runs in the image (the loads, the starts,
   the answer). Under it each step - a child process (RUN-BOUNDED), an HTTP
   exchange (HTTP), the lock - gets at most what is left (WAIT-SECONDS), and
   a step that finds nothing left signals instead of starting. A step cut
   at the deadline spends it (STEP-RAN-OUT), so the next finds nothing left
   even when a timed wait wakes a hair before the clock reads the deadline,
   and the answer names the step the time ran out in.
   `within`: whatever each step takes within what it is given, the work ends
   by the deadline, the lock's waits included. `answer_lands`: with the
   image's part within the margin, the answer reaches the waiter before its
   wait ends - no waiter gives up on work that may still succeed.
   `retry_after`: a retry made once the wait is over starts after the work
   has ended. `cut_within`: the run WAIT-SECONDS makes of any steps, each
   running to what it is given or its own end, is such a run.

Stated, not proved: the image's part fits the margin. A load compiles in
the image, where nothing can kill it; past the margin, a waiter says what
main says.

The decided traces `trace100` and `trace570` are the ones the Lisp test
wait-seconds-holds-to-adddeadlines-lean-traces pins on the real WAIT-SECONDS.

Checked by `just proofs` (Lean 4, core only).
-/

namespace AddDeadlines

/-- One step of the work: its own bound in seconds, `none` for one that has
none (main's wait for the lock), and the seconds it took. -/
structure Step where
  bound : Option Nat
  took : Nat
  deriving DecidableEq, Repr

/-- When a run of steps begun at `t` ends. -/
def finish : Nat → List Step → Nat
  | t, [] => t
  | t, s :: rest => finish (t + s.took) rest

/-! ## 1. main -/

/-- main: a step within its own bound; one with none takes any time. -/
def mainOk : Step → Bool
  | ⟨some b, d⟩ => d ≤ b
  | ⟨none, _⟩ => true

/-- What each waiter waits on main: add, the /cells sheet, the picker, setup. -/
def mainWaits : List Nat := [600, 300, 300, 60]

/-- One folder of a repository: both indexes read, its archive's download
failing at its bound, the kept copy's lock free, init, a fetch that runs
out its 600 s, then a clone of 590 s and a checkout of 5. -/
def slowRun : List Step :=
  [⟨some 10, 10⟩, ⟨some 10, 10⟩, ⟨some 20, 20⟩, ⟨none, 0⟩, ⟨some 600, 0⟩,
   ⟨some 600, 600⟩, ⟨some 600, 590⟩, ⟨some 600, 5⟩]

theorem slowRun_main : slowRun.all mainOk = true := by decide

theorem slowRun_finish : finish 0 slowRun = 1235 := by decide

/-- Every waiter on main gives up while `slowRun` still works. -/
theorem main_waiters_quit : mainWaits.all (fun w => decide (w < finish 0 slowRun)) = true := by
  decide

/-- add's retry, made when its 600 s run out, starts while the first install
still runs. -/
theorem main_retry_overlaps : 600 < finish 0 slowRun := by decide

/-- The lock has no bound on main: behind another install's fetch a folder
waits for as long as that takes. -/
theorem main_lock_unbounded (N : Nat) :
    ∃ run : List Step, run.all mainOk = true ∧ N < finish 0 run :=
  ⟨[⟨none, N + 1⟩], by simp [mainOk], by simp [finish]⟩

/-- The coded worst case of one folder with no wait on the lock: both index
reads and every step of all three ways at its bound. -/
def worstFolder : List Step :=
  [⟨some 10, 10⟩, ⟨some 10, 10⟩, ⟨some 20, 20⟩, ⟨some 60, 60⟩, ⟨none, 0⟩,
   ⟨some 600, 600⟩, ⟨some 600, 600⟩, ⟨some 600, 600⟩, ⟨some 600, 600⟩, ⟨some 600, 600⟩]

theorem worstFolder_main : worstFolder.all mainOk = true := by decide

theorem worstFolder_finish : finish 0 worstFolder = 3100 := by decide

/-! ## 2. One deadline -/

/-- WAIT-SECONDS: what a step begun at `t` is given under deadline `D`: its
own bound or what is left, the less; what is left when it has no bound. -/
def given (D t : Nat) : Option Nat → Nat
  | some b => min b (D - t)
  | none => D - t

theorem given_le_left (D t : Nat) (b : Option Nat) : given D t b ≤ D - t := by
  cases b with
  | none => exact Nat.le_refl _
  | some b => exact Nat.min_le_right _ _

theorem given_le_bound (D t b : Nat) : given D t (some b) ≤ b := Nat.min_le_left _ _

/-- A run under deadline `D` from `t`: each step took at most what it was given. -/
def underDeadline (D : Nat) : Nat → List Step → Prop
  | _, [] => True
  | t, s :: rest => s.took ≤ given D t s.bound ∧ underDeadline D (t + s.took) rest

/-- Whatever each step takes within what it is given, the work ends by the
deadline: the lock's waits, which have no bound of their own, included. -/
theorem within (D : Nat) : ∀ (run : List Step) (t : Nat), t ≤ D → underDeadline D t run →
    finish t run ≤ D
  | [], _, ht, _ => ht
  | s :: rest, t, ht, ⟨hs, hr⟩ => by
      have hl := given_le_left D t s.bound
      exact within D rest (t + s.took) (by omega) hr

/-- Under a deadline every step stays within its own bound too: the fix only
ever shortens a wait. -/
theorem within_bound (D : Nat) : ∀ (run : List Step) (t : Nat), underDeadline D t run →
    run.all mainOk = true
  | [], _, _ => rfl
  | ⟨b, d⟩ :: rest, t, ⟨hs, hr⟩ => by
      have ih := within_bound D rest (t + d) hr
      cases b with
      | none => simpa [mainOk] using ih
      | some b =>
          have hb := given_le_bound D t b
          change d ≤ given D t (some b) at hs
          simp only [List.all_cons, Bool.and_eq_true]
          exact ⟨by simp [mainOk]; omega, ih⟩

/-- The seconds the owner keeps for what runs in the image. -/
def margin (W : Nat) : Nat := min 30 (W / 2)

/-- The owner's deadline for a waiter that waits `W`. -/
def deadline (W : Nat) : Nat := W - margin W

theorem deadline_add_margin (W : Nat) : deadline W + margin W = W := by
  unfold deadline margin; omega

theorem deadline_le (W : Nat) : deadline W ≤ W := by unfold deadline; omega

/-- The answer lands when the work's run ends plus the image's part: with
that part within the margin, before the waiter's wait ends. -/
theorem answer_lands (W : Nat) (run : List Step) (h : underDeadline (deadline W) 0 run)
    (image : Nat) (hi : image ≤ margin W) : finish 0 run + image ≤ W := by
  have := within (deadline W) run 0 (Nat.zero_le _) h
  have := deadline_add_margin W
  omega

/-- A retry made once the waiter's wait is over starts after the work ended. -/
theorem retry_after (W r : Nat) (run : List Step) (h : underDeadline (deadline W) 0 run)
    (hr : W ≤ r) : finish 0 run ≤ r := by
  have := within (deadline W) run 0 (Nat.zero_le _) h
  have := deadline_le W
  omega

/-- A step as the work would run it: its own bound and how long it would
take were nothing cut. -/
structure Want where
  bound : Option Nat
  wants : Nat
  deriving DecidableEq, Repr

/-- The run WAIT-SECONDS makes from `t` under `D`: each step runs to what it
is given or its own end, the earlier. -/
def cut (D : Nat) : Nat → List Want → List Step
  | _, [] => []
  | t, w :: rest =>
      let d := min (given D t w.bound) w.wants
      ⟨w.bound, d⟩ :: cut D (t + d) rest

theorem cut_under (D : Nat) : ∀ (ws : List Want) (t : Nat), underDeadline D t (cut D t ws)
  | [], _ => trivial
  | _ :: rest, _ => ⟨Nat.min_le_left _ _, cut_under D rest _⟩

theorem cut_within (D : Nat) (ws : List Want) : finish 0 (cut D 0 ws) ≤ D :=
  within D _ 0 (Nat.zero_le _) (cut_under D ws 0)

/-- What WAIT-SECONDS gives each step of a run, in order. -/
def givenTrace (D : Nat) : Nat → List Want → List Nat
  | _, [] => []
  | t, w :: rest =>
      let g := given D t w.bound
      g :: givenTrace D (t + min g w.wants) rest

/-! ## Decided traces (pinned on the real WAIT-SECONDS) -/

/-- Under 100 s: both indexes (3 s, 4 s), the archive's download stalling
past its 20 s, the lock held by another install for 1 000 s, then the clone:
given 10, 10, 20, 73 and then nothing, so the clone signals. -/
def wants100 : List Want :=
  [⟨some 10, 3⟩, ⟨some 10, 4⟩, ⟨some 20, 25⟩, ⟨none, 1000⟩, ⟨some 600, 5⟩]

theorem trace100 : givenTrace 100 0 wants100 = [10, 10, 20, 73, 0] := by decide

theorem trace100_ends : finish 0 (cut 100 0 wants100) = 100 := by decide

/-- `slowRun` under add's 600 s: the owner's deadline is 570 s; the fetch
that would hang is given what is left, 547 s, and the clone nothing. -/
def wants570 : List Want :=
  [⟨some 10, 1⟩, ⟨some 10, 1⟩, ⟨some 20, 20⟩, ⟨none, 0⟩, ⟨some 600, 1⟩,
   ⟨some 600, 100000⟩, ⟨some 600, 590⟩]

theorem deadline600 : deadline 600 = 570 := by decide

theorem trace570 : givenTrace 570 0 wants570 = [10, 10, 20, 548, 548, 547, 0] := by decide

theorem trace570_ends : finish 0 (cut 570 0 wants570) = 570 := by decide

/-- The margins at the waits the code uses, and at a scaled one. -/
theorem margins : (margin 600, margin 60, margin 12, margin 1) = (30, 30, 6, 0) := by decide

end AddDeadlines
