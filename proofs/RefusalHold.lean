/-
Refusal hold: the policy NEXT-ROUND-CONTEXT (src/src/engine/turn.lisp)
follows when the selected model refuses a round and its fallback answers it.

State: `streak`, the selection's refusals in a row; `remaining`, the rounds
the candidate still answers before the selection is asked again. After the
k-th refusal in a row the candidate holds `hold k = min (2^(k-1)) cap`
rounds; an answer from the selection resets the streak.

A round's verdict `v` says whether the selection would refuse if asked that
round. Verdicts are fixed in advance (an oblivious sequence): the policy only
chooses which rounds ask.

Checked with `lean proofs/RefusalHold.lean` (Lean 4, core only).
-/

namespace RefusalHold

structure St where
  streak : Nat
  remaining : Nat
  deriving DecidableEq, Repr

/-- `*refusal-hold-cap*` -/
def cap : Nat := 8

/-- `refusal-hold-rounds` -/
def hold (k : Nat) : Nat := min (2 ^ (k - 1)) cap

/-- One round: the next state, whether the selection was asked, whether that
ask was refused. -/
def step (s : St) (v : Bool) : St × Bool × Bool :=
  if s.remaining > 0 then ({ s with remaining := s.remaining - 1 }, false, false)
  else if v then ({ streak := s.streak + 1, remaining := hold (s.streak + 1) }, true, true)
  else ({ streak := 0, remaining := 0 }, true, false)

/-- Refused asks over a run of rounds. -/
def run : St → List Bool → Nat
  | _, [] => 0
  | s, v :: vs => (if (step s v).2.2 then 1 else 0) + run (step s v).1 vs

/-- The rounds (counted from `i`) at which the selection is asked. -/
def asks : St → List Bool → Nat → List Nat
  | _, [], _ => []
  | s, v :: vs, i => (if (step s v).2.1 then [i] else []) ++ asks (step s v).1 vs (i + 1)

/-- The policy before the hold asked every round: its refused asks are the
refusals themselves. -/
def runNow (vs : List Bool) : Nat := vs.count true

/-! ### 1. Never worse than asking every round -/

/-- Only a round whose verdict is a refusal can spend a refused ask. -/
theorem refused_imp (s : St) (v : Bool) : (step s v).2.2 = true → v = true := by
  unfold step
  by_cases hr : s.remaining > 0
  · simp [hr]
  · cases v <;> simp [hr]

theorem run_le_runNow : ∀ (vs : List Bool) (s : St), run s vs ≤ runNow vs := by
  intro vs
  induction vs with
  | nil => intro s; simp [run, runNow]
  | cons v vs ih =>
    intro s
    have h := ih (step s v).1
    simp only [runNow] at h ⊢
    unfold run
    rw [List.count_cons]
    cases hs : (step s v).2.2
    · simp; omega
    · have hv := refused_imp s v hs
      subst hv
      simp; omega

/-! ### 2. The operator's model is never away for more than `cap` rounds -/

theorem hold_le_cap (k : Nat) : hold k ≤ cap := Nat.min_le_right _ _

/-- The wait never exceeds the cap... -/
theorem step_remaining_le (s : St) (v : Bool) (h : s.remaining ≤ cap) :
    (step s v).1.remaining ≤ cap := by
  by_cases hr : s.remaining > 0
  · simp [step, hr]; omega
  · cases v
    · simp [step, hr]
    · simp [step, hr]; exact hold_le_cap _

/-- ...a held round does not ask and counts the wait down... -/
theorem step_held (s : St) (v : Bool) (h : s.remaining > 0) :
    (step s v).2.1 = false ∧ (step s v).1.remaining = s.remaining - 1 := by
  simp [step, h]

/-- ...and a round with no wait left asks the selection. So from any state
the selection is asked again within `remaining + 1 ≤ cap + 1` rounds. -/
theorem step_asks (s : St) (v : Bool) (h : s.remaining = 0) : (step s v).2.1 = true := by
  cases v <;> simp [step, h]

/-- An answer ends the streak: no wait follows it. -/
theorem answer_resets (s : St) (h : s.remaining = 0) :
    (step s false).1 = { streak := 0, remaining := 0 } := by
  simp [step, h]

/-- No refusals, no wait: every round asks the selection. -/
theorem no_refusal_no_wait (n : Nat) :
    asks { streak := 0, remaining := 0 } (List.replicate n false) 0 = List.range n := by
  suffices ∀ n i, asks { streak := 0, remaining := 0 } (List.replicate n false) i
      = (List.range n).map (· + i) by simpa using this n 0
  intro n
  induction n with
  | zero => intro i; simp [asks]
  | succ n ih =>
    intro i
    simp [List.replicate_succ, asks, step, ih, List.range_succ_eq_map, Function.comp_def]
    omega

/-! ### 3. A selection that refuses every ask: logarithmic, then linear at 1/9 -/

theorem hold_at_cap (k : Nat) (hk : 3 ≤ k) : hold (k + 1) = cap := by
  have : 2 ^ 3 ≤ 2 ^ k := Nat.pow_le_pow_right (by decide) hk
  simp [hold, cap]
  omega

theorem tail_bound : ∀ (n k r : Nat), 3 ≤ k → r ≤ cap →
    9 * run { streak := k, remaining := r } (List.replicate n true) + r ≤ n + 8 := by
  intro n
  induction n with
  | zero => intro k r _ hr; simp [run, cap] at *; omega
  | succ n ih =>
    intro k r hk hr
    simp only [List.replicate_succ]
    unfold run
    by_cases h0 : r > 0
    · have := ih k (r - 1) hk (by simp [cap] at hr ⊢; omega)
      simp [step, h0]
      omega
    · have hr0 : r = 0 := by omega
      subst hr0
      have := ih (k + 1) cap (by omega) (Nat.le_refl _)
      simp [step, hold_at_cap k hk]
      simp [cap] at this ⊢
      omega

/-- The first eleven rounds of an all-refusal run ask at 0, 2, 5, 10. -/
theorem prefix11 (rest : List Bool) :
    run { streak := 0, remaining := 0 } (List.replicate 11 true ++ rest)
      = 4 + run { streak := 4, remaining := 8 } rest := by
  simp [List.replicate, run, step, hold, cap]
  omega

theorem small_runs : ∀ n, n < 11 →
    run { streak := 0, remaining := 0 } (List.replicate n true) ≤ 4 := by
  decide

/-- Refused asks over `n` rounds of a selection that refuses every ask are at
most `4 + n / 9`; asking every round spends `n`. -/
theorem all_refuse_bound (n : Nat) :
    run { streak := 0, remaining := 0 } (List.replicate n true) ≤ 4 + n / 9 := by
  by_cases hn : n < 11
  · have := small_runs n hn; omega
  · obtain ⟨m, rfl⟩ : ∃ m, n = 11 + m := ⟨n - 11, by omega⟩
    rw [← List.replicate_append_replicate, prefix11]
    have := tail_bound m 4 cap (by decide) (Nat.le_refl _)
    simp [cap] at this
    omega

/-- The trace the Lisp test pins (turn-test.lisp,
a-refused-selection-waits-on-its-candidate-doubling-to-the-cap). -/
theorem trace40 :
    asks { streak := 0, remaining := 0 } (List.replicate 40 true) 0 = [0, 2, 5, 10, 19, 28, 37] := by
  decide

theorem trace_mixed :
    asks { streak := 0, remaining := 0 }
      -- the verdict of every round; held rounds never read theirs
      [true, false, true, false, false, true, false, false, false, false, false,
       true, false, true] 0 = [0, 2, 5, 10, 11, 13] := by
  decide

end RefusalHold
