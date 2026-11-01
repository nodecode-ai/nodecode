/-
Retry ladder: how long one provider call can go without content, as
src/src/engine/turn.lisp (the ladder in CALL-PROVIDER's round) and
src/src/engine/provider.lisp (PROVIDER-RETRY-DELAY, the idle clock and the
content clock) bound it.

1. Budget. A request-scope failure (no 200 head) spends the request budget R,
   any other scope the stream budget S and refills the request one (the
   attempt.zig reset rule), and a refused connection spends the request budget
   at once. `requests_le`: a call makes at most (S + 1) * (R + 1) requests to
   one provider - 30 at the defaults (S = 5, R = 4) - and `requests_tight`:
   30 happen. (A context overflow's eviction is its own capped loop, and a
   failover candidate gets a ladder of its own; neither is counted here.)

2. Delay. `delay_le`: each wait before a retry is at most 11/10 of the
   curve's cap, max(8 s, base, 60 s for a 503); a Retry-After is honoured up to
   60 s. At the default base (0.2 s) that is 8.8 s, 66 s for a 503.

3. Silence. An attempt reads its stream line by line; each read waits at most
   the idle timeout, and at most what the content clock's window has left:
   after content, *CONTENT-IDLE-WINDOWS* idle timeouts since the last content;
   before it, *FIRST-CONTENT-IDLE-WINDOWS* since the head. `silence_le`: no
   attempt lets more than the larger window pass without content, whatever
   the provider sends. `firstTokenMain`: main 75a703a ran no window before the
   first content, and with no failover candidate behind the attempt, keep-alive
   lines a second apart under a 2 s idle timeout held it 40 s (as long as they
   came) with nothing streamed. A line is read whole: its octets arrive within
   one read deadline of each other (READ-OCTET-LINE), and the clocks are
   checked between lines.

Composed by hand from the three, not a theorem here: at the defaults (idle
120 s) one provider call without content ends within 6 stream attempts of at
most 1800 s, 30 heads of at most 120 s and 29 waits of at most 66 s - about
4.5 hours, where main had no bound.

Checked by `just proofs` (Lean 4, core only).
-/

namespace RetryLadder

/-! ## 1. Budget -/

inductive Fail where
  | request  -- no 200 head: connect, DNS, a non-200 head
  | stream   -- after the head: truncation, an in-stream error, a stall
  | refused  -- nothing listens there
  deriving DecidableEq, Repr

/-- STREAM-ATTEMPT (from 1) and REQUEST-RETRIES-USED. -/
structure Ladder where
  attempt : Nat
  used : Nat
  deriving DecidableEq, Repr

/-- One failure: the ladder after its retry, or none when the budget it spends
is spent (the chain's next candidate takes the round, or the failure stands). -/
def next (S R : Nat) (l : Ladder) : Fail → Option Ladder
  | .refused => none
  | .request => if l.used ≥ R then none else some ⟨l.attempt, l.used + 1⟩
  | .stream => if l.attempt - 1 ≥ S then none else some ⟨l.attempt + 1, 0⟩

/-- The requests a call makes when FAILS come back in order and the request
after them, if the ladder gets there, succeeds. -/
def requests (S R : Nat) : Ladder → List Fail → Nat
  | _, [] => 1
  | l, f :: fs =>
    match next S R l f with
    | none => 1
    | some l' => 1 + requests S R l' fs

/-- What the ladder has left: requests in the stream attempts to come, and in
this one. -/
def left (S R : Nat) (l : Ladder) : Nat := (S + 1 - l.attempt) * (R + 1) + (R - l.used)

theorem requests_le_left (S R : Nat) :
    ∀ (fs : List Fail) (l : Ladder), 1 ≤ l.attempt → l.attempt ≤ S + 1 → l.used ≤ R →
      requests S R l fs ≤ left S R l + 1
  | [], _, _, _, _ => by simp [requests]
  | f :: fs, l, h1, h2, h3 => by
    unfold requests
    split
    · simp
    · rename_i l' hn
      cases f with
      | refused => simp [next] at hn
      | request =>
        simp only [next] at hn
        split at hn
        · simp at hn
        · rename_i hu
          simp only [Option.some.injEq] at hn
          subst hn
          have ih := requests_le_left S R fs ⟨l.attempt, l.used + 1⟩ h1 h2 (by simp; omega)
          simp only [left] at ih ⊢
          omega
      | stream =>
        simp only [next] at hn
        split at hn
        · simp at hn
        · rename_i ha
          simp only [Option.some.injEq] at hn
          subst hn
          have ih := requests_le_left S R fs ⟨l.attempt + 1, 0⟩ (by simp) (by simp; omega) (by simp)
          simp only [left] at ih ⊢
          have hk : S + 1 - l.attempt = (S + 1 - (l.attempt + 1)) + 1 := by omega
          rw [hk, Nat.succ_mul]
          omega

/-- At most (S + 1) * (R + 1) requests to one provider. -/
theorem requests_le (S R : Nat) (fs : List Fail) :
    requests S R ⟨1, 0⟩ fs ≤ (S + 1) * (R + 1) := by
  have := requests_le_left S R fs ⟨1, 0⟩ (Nat.le_refl 1) (show 1 ≤ S + 1 by omega) (Nat.zero_le R)
  simp only [left, Nat.add_sub_cancel, Nat.sub_zero] at this
  have hm : (S + 1) * (R + 1) = S * (R + 1) + (R + 1) := Nat.succ_mul S (R + 1)
  omega

/-- Four request retries in each of six stream attempts: thirty requests. -/
theorem requests_tight :
    requests 5 4 ⟨1, 0⟩
      ((List.replicate 5 [Fail.request, .request, .request, .request, .stream]).flatten ++
        [.request, .request, .request, .request]) = 30 := by
  decide

/-! ## 2. Delay

PROVIDER-RETRY-DELAY in milliseconds: the curve min(base * 2^(n-1), cap) with
cap = max(8000, base, 60000 for a 503), base at least 5000 for a 503,
jittered by (9000 + j) / 10000 for j in 0..2000 (the Lisp's
1 + (random 2001 - 1000) / 10000); a Retry-After is min(it, 60000), unjittered.
Scaled by 10000 so the jitter stays whole. -/

def base (baseMs status : Nat) : Nat := max baseMs (if status = 503 then 5000 else 1)

def cap (baseMs status : Nat) : Nat :=
  max 8000 (max (base baseMs status) (if status = 503 then 60000 else 0))

/-- The wait, in ten-thousandths of a millisecond. -/
def delay (n baseMs status j : Nat) (retryAfterMs : Option Nat) : Nat :=
  match retryAfterMs with
  | some ra => if ra > 0 then min ra 60000 * 10000
    else min (base baseMs status * 2 ^ (n - 1)) (cap baseMs status) * (9000 + min j 2000)
  | none => min (base baseMs status * 2 ^ (n - 1)) (cap baseMs status) * (9000 + min j 2000)

theorem curve_le (n baseMs status j : Nat) :
    min (base baseMs status * 2 ^ (n - 1)) (cap baseMs status) * (9000 + min j 2000) ≤
      cap baseMs status * 11000 :=
  Nat.mul_le_mul (Nat.min_le_right _ _) (by omega)

/-- Each wait is at most 11/10 of the cap, a Retry-After at most 60 s. -/
theorem delay_le (n baseMs status j : Nat) (ra : Option Nat) :
    delay n baseMs status j ra ≤ max (cap baseMs status * 11000) (60000 * 10000) := by
  unfold delay
  have hc := curve_le n baseMs status j
  split
  · split
    · rename_i ra _
      have : min ra 60000 * 10000 ≤ 60000 * 10000 :=
        Nat.mul_le_mul (Nat.min_le_right ra 60000) (Nat.le_refl 10000)
      omega
    · omega
  · omega

/-- At the default base (200 ms): 8.8 s, 66 s for a 503. -/
theorem default_caps : cap 200 500 * 11000 = 8800 * 10000 ∧ cap 200 503 * 11000 = 66000 * 10000 := by
  decide

/-! ## 3. Silence -/

inductive Line where
  | content  -- a part that carries content (CONTENT-PART-P)
  | noise    -- a comment, a ping, an empty delta
  deriving DecidableEq, Repr

/-- The idle timeout, the window after content, and the window before it
(none: main 75a703a, which ran none). -/
structure Clock where
  idle : Nat
  window : Nat
  first : Option Nat

/-- What the content clock's window has left at T, LAST the last content's
time (none before the first); none while no window runs. -/
def budget (c : Clock) (t : Nat) : Option Nat → Option Nat
  | some a => some (a + c.window - t)
  | none => c.first.map (· - t)

/-- The bound each read waits under. -/
def limit (c : Clock) (t : Nat) (last : Option Nat) : Nat :=
  match budget c t last with
  | some b => min c.idle b
  | none => c.idle

/-- Where silence is counted from: the last content, or the head. -/
def anchor : Option Nat → Nat
  | some a => a
  | none => 0

/-- The longest the walk lets pass without content: lines arrive GAP seconds
after the read before; a spent window cuts at once, a gap past the read's
bound cuts there, and content restarts the count. -/
def silence (c : Clock) : Nat → Option Nat → List (Nat × Line) → Nat
  | _, _, [] => 0
  | t, last, (g, l) :: rest =>
    if budget c t last = some 0 then t - anchor last
    else if g > limit c t last then t + limit c t last - anchor last
    else max (t + g - anchor last)
      (silence c (t + g) (if l = .content then some (t + g) else last) rest)

/-- How far silence may run at a moment: the window after content, the first
window before it. -/
def reach (c : Clock) (F : Nat) : Option Nat → Nat
  | some _ => c.window
  | none => F

theorem silence_le_reach (c : Clock) (F : Nat) (hF : c.first = some F) :
    ∀ (lines : List (Nat × Line)) (t : Nat) (last : Option Nat),
      t ≤ anchor last + reach c F last → anchor last ≤ t →
      silence c t last lines ≤ max c.window F
  | [], _, _, _, _ => by simp [silence]
  | (g, l) :: rest, t, last, hi, ha => by
    have hb : ∀ b, budget c t last = some b → t + b ≤ anchor last + reach c F last := by
      intro b hb
      cases last with
      | some a => simp [budget] at hb; simp [anchor, reach] at hi ⊢; omega
      | none => simp [budget, hF] at hb; simp [anchor, reach] at hi ⊢; omega
    have hl : t + limit c t last ≤ anchor last + reach c F last := by
      unfold limit
      split
      · rename_i b hbb; have := hb b hbb; have := Nat.min_le_right c.idle b; omega
      · rename_i hbn
        cases last with
        | some a => simp [budget] at hbn
        | none => simp [budget, hF] at hbn
    have hr : reach c F last ≤ max c.window F := by
      cases last <;> simp [reach] <;> omega
    unfold silence
    split
    · omega
    · split
      · omega
      · rename_i _ hg
        have hg' : g ≤ limit c t last := by omega
        refine Nat.max_le.mpr ⟨by omega, ?_⟩
        split
        · exact silence_le_reach c F hF rest (t + g) (some (t + g)) (by simp [anchor]) (by simp [anchor])
        · exact silence_le_reach c F hF rest (t + g) last (by omega) (by omega)

/-- No attempt lets more than the larger window pass without content. -/
theorem silence_le (c : Clock) (F : Nat) (hF : c.first = some F) (lines : List (Nat × Line)) :
    silence c 0 none lines ≤ max c.window F :=
  silence_le_reach c F hF lines 0 none (by simp [anchor]) (by simp [anchor])

/-- Main: keep-alives a second apart under a 2 s idle timeout, no content,
no window before it - forty seconds pass, and would for as long as they came. -/
theorem firstTokenMain :
    silence ⟨2, 6, none⟩ 0 none (List.replicate 40 (1, .noise)) = 40 := by
  decide

/-- The same stream under a first window of 10 s: cut at 10. -/
theorem firstTokenCut :
    silence ⟨2, 6, some 10⟩ 0 none (List.replicate 40 (1, .noise)) = 10 := by
  decide

/-- Content every second never trips either window. -/
theorem lively :
    silence ⟨2, 6, some 10⟩ 0 none (List.replicate 40 (1, .content)) = 1 := by
  decide

end RetryLadder
