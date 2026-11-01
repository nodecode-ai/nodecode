/-
Self-edit hold: what src/src/organism/provisional.lisp guarantees when an
eval redefines a function that is running on the stack it was dispatched
from. Without it, a definition that cannot run is written to the layer
before it runs, every later turn dies at the top of the loop, and the
restart that should heal the image loads the same cut again.

1. The refusal (namespace SelfEditHold.Refusal). TAKES-EVERY-CALL-P, with
   CALL-SHAPE and KEYWORD-START, refuses a definition whose lambda list
   cannot take every call the displaced one takes. A call is its argument
   list, each argument a keyword (by name) or any other value; `accepts` is
   Common Lisp's rule for required, &optional, &rest, &key and
   &allow-other-keys parameters (a call passing :allow-other-keys itself is
   left out). `takes_sound`: what the check takes, takes every call the
   displaced lambda list takes. `main_unsound`: main 75a703a had no keyword
   alignment clause and took (x &key y) for (x &optional y), whose callers'
   (f 1 2) it then refuses.

2. The hold (namespace SelfEditHold.Batch). A form's held names are one
   batch, written to the layer once every name of it has returned from a
   traversal outside the model's eval, and put back whole when one
   signals first. `written_returned`: on every run, every name of a batch in
   the layer returned. `never_both`: no batch is both written and reverted.
   `err_restores`: a signalling traversal puts back, for every name of its
   batch, what that name last ran with no hold over it. `pairBrokenMain`:
   main 75a703a confirmed a batch on the first return of any one name, and
   wrote the other, broken, name with it.

The decided traces and the shape table are the ones
src/test/organism/provisional-test.lisp pins on the real functions.

Checked by `just proofs` (Lean 4, core only).
-/

namespace SelfEditHold.Refusal

/-! ## 1. The refusal -/

/-- One argument of a call: a keyword, by name, or any other value. -/
inductive Arg where
  | kw (name : String)
  | val
  deriving DecidableEq, Repr

/-- A lambda list: its required and optional counts, whether it takes &rest,
&key and &allow-other-keys, and the keyword names it takes. -/
structure LL where
  req : Nat
  opt : Nat
  rest : Bool
  key : Bool
  allow : Bool
  keys : List String
  deriving DecidableEq, Repr

/-- A key of a keyword portion that the list takes: any one under
&allow-other-keys (SBCL then checks only the portion's length), else a
keyword it names. -/
def keyOk (allow : Bool) (keys : List String) : Arg → Bool
  | .kw k => allow || keys.contains k
  | .val => allow

/-- A keyword portion: key/value pairs, every key taken. -/
def plistOk (allow : Bool) (keys : List String) : List Arg → Bool
  | [] => true
  | [_] => false
  | k :: _ :: t => keyOk allow keys k && plistOk allow keys t

/-- Common Lisp's rule, as SBCL applies it: lambda list `l` takes the call
`args`. -/
def accepts (l : LL) (args : List Arg) : Bool :=
  decide (l.req ≤ args.length) &&
  (if l.key then plistOk l.allow l.keys (args.drop (l.req + l.opt))
   else if l.rest then true
   else decide (args.length ≤ l.req + l.opt))

/-- CALL-SHAPE's MAX: `none` when unbounded. -/
def shapeMax (l : LL) : Option Nat :=
  if l.rest || l.key then none else some (l.req + l.opt)

/-- CALL-SHAPE's KEYS: `none` for :ANY, else the names taken. -/
def shapeKeys (l : LL) : Option (List String) :=
  if l.allow || (l.rest && !l.key) then none else some l.keys

/-- KEYWORD-START. -/
def keywordStart (l : LL) : Option Nat :=
  if l.key then some (l.req + l.opt) else none

/-- CALL-SHAPE's MAX clause: NEW's bound is absent or covers OLD's. -/
def maxOk (new old : Option Nat) : Bool :=
  match new, old with
  | none, _ => true
  | some n, some o => decide (o ≤ n)
  | some _, none => false

/-- The KEYS clause: OLD takes no keyword, NEW takes any, or NEW takes every
name OLD takes. -/
def keysOk (old new : Option (List String)) : Bool :=
  match old, new with
  | some [], _ => true
  | _, none => true
  | some os, some ns => os.all ns.contains
  | none, some _ => false

/-- TAKES-EVERY-CALL-P as main 75a703a has it: counts and keywords. -/
def takesMain (new old : LL) : Bool :=
  decide (new.req ≤ old.req) && maxOk (shapeMax new) (shapeMax old) &&
    keysOk (shapeKeys old) (shapeKeys new)

/-- The clause this change adds, KEYWORDS-ALIGNED-P: where NEW's keywords
begin, every call OLD takes leaves a keyword portion NEW reads. Under
&allow-other-keys only its length must be even; else OLD's positional
arguments, being any values, must end there, and OLD's own keywords must
begin there. -/
def aligned (new old : LL) : Bool :=
  match keywordStart new with
  | none => true
  | some n =>
    if new.allow then
      match keywordStart old with
      | some o => decide ((o + n) % 2 = 0) && (decide (o ≤ n) || decide (o ≤ old.req))
      | none =>
        match shapeMax old with
        | some m => decide (m ≤ n) || (decide (max old.req (n + 1) = m) && decide ((m - n) % 2 = 0))
        | none => false
    else
      match keywordStart old with
      | some o => n == o
      | none =>
        match shapeMax old with
        | some m => decide (m ≤ n)
        | none => false

/-- TAKES-EVERY-CALL-P. -/
def takes (new old : LL) : Bool := takesMain new old && aligned new old

theorem plistOk_even {a : Bool} {ks : List String} :
    ∀ (l : List Arg), plistOk a ks l = true → l.length % 2 = 0
  | [], _ => rfl
  | [_], hl => by simp [plistOk] at hl
  | _ :: _ :: t, hl => by
      simp only [plistOk, Bool.and_eq_true] at hl
      have := plistOk_even t hl.2
      simp only [List.length_cons]
      omega

theorem plistOk_any {ks : List String} :
    ∀ (l : List Arg), l.length % 2 = 0 → plistOk true ks l = true
  | [], _ => rfl
  | [_], hl => by simp at hl
  | k :: _ :: t, hl => by
      simp only [List.length_cons] at hl
      have := plistOk_any (ks := ks) t (by omega)
      cases k <;> simp [plistOk, keyOk, this]

theorem plistOk_mono {a a' : Bool} {ks ks' : List String}
    (h : ∀ x, keyOk a ks x = true → keyOk a' ks' x = true) :
    ∀ (l : List Arg), plistOk a ks l = true → plistOk a' ks' l = true
  | [], _ => rfl
  | [_], hl => by simp [plistOk] at hl
  | k :: _ :: t, hl => by
      simp only [plistOk, Bool.and_eq_true] at hl ⊢
      exact ⟨h k hl.1, plistOk_mono h t hl.2⟩

/-- Between two &key lists, the KEYS clause covers every key the old one takes. -/
theorem cover (new old : LL) (hnk : new.key = true) (hok : old.key = true)
    (h : keysOk (shapeKeys old) (shapeKeys new) = true) :
    ∀ x, keyOk old.allow old.keys x = true → keyOk new.allow new.keys x = true := by
  intro x hx
  unfold shapeKeys at h
  cases hna : new.allow
  · cases hoa : old.allow
    · simp only [hna, hoa, hnk, hok, Bool.false_or, Bool.and_false, Bool.not_true,
        Bool.false_eq_true, ite_false] at h hx ⊢
      cases x with
      | val => simp [keyOk] at hx
      | kw k =>
        simp only [keyOk, Bool.false_or] at hx ⊢
        cases hks : old.keys with
        | nil => simp [hks] at hx
        | cons y ys =>
          rw [hks] at h hx
          simp only [keysOk, List.all_eq_true] at h
          exact h k (List.contains_iff_mem.mp hx)
    · simp [hna, hoa, hnk, hok, keysOk] at h
  · cases x <;> simp [keyOk]

/-- What the check takes, takes every call the displaced lambda list takes. -/
theorem takes_sound (new old : LL) (args : List Arg) (ht : takes new old = true)
    (ha : accepts old args = true) : accepts new args = true := by
  unfold takes takesMain at ht
  simp only [Bool.and_eq_true, decide_eq_true_eq] at ht
  obtain ⟨⟨⟨hreq, hmax⟩, hkeys⟩, hal⟩ := ht
  unfold accepts at ha ⊢
  simp only [Bool.and_eq_true, decide_eq_true_eq] at ha ⊢
  obtain ⟨hlen, hrest⟩ := ha
  refine ⟨by omega, ?_⟩
  cases hnk : new.key
  · -- NEW takes no &key
    cases hnr : new.rest
    · -- NEW is bounded, so OLD is, and no further
      cases hok : old.key <;> cases hor : old.rest <;>
        simp_all [maxOk, shapeMax] <;> omega
    · simp
  · -- NEW reads keywords from its KEYWORD-START on
    simp only [↓reduceIte]
    have hlen' := List.length_drop (l := args) (i := new.req + new.opt)
    cases hna : new.allow
    · -- NEW names its keys: OLD's positionals end where they begin
      cases hok : old.key
      · cases hor : old.rest
        · simp_all [aligned, keywordStart, shapeMax]
          rw [List.drop_eq_nil_of_le (by omega)]
          rfl
        · simp_all [aligned, keywordStart, shapeMax]
      · have hc := cover new old hnk hok hkeys
        have hstart : new.req + new.opt = old.req + old.opt := by
          simpa [aligned, keywordStart, hnk, hok, hna] using hal
        simp only [hok, ↓reduceIte] at hrest
        rw [hstart]
        have := plistOk_mono hc _ hrest
        rwa [hna] at this
    · -- NEW takes any key: only the portion's length must be even
      apply plistOk_any
      rw [hlen']
      cases hok : old.key
      · cases hor : old.rest
        · simp only [hok, hor, Bool.false_eq_true, ↓reduceIte, decide_eq_true_eq] at hrest
          simp only [aligned, keywordStart, shapeMax, hnk, hna, hok, hor, ↓reduceIte,
            Bool.or_false, Bool.false_eq_true, Bool.or_eq_true, Bool.and_eq_true,
            decide_eq_true_eq] at hal
          omega
        · simp [aligned, keywordStart, shapeMax, hnk, hna, hok, hor] at hal
      · simp only [hok, ↓reduceIte] at hrest
        have he := plistOk_even _ hrest
        rw [List.length_drop] at he
        simp only [aligned, keywordStart, hnk, hna, hok, ↓reduceIte, Bool.and_eq_true,
          Bool.or_eq_true, decide_eq_true_eq] at hal
        omega

/-- The shapes provisional-test.lisp asks the real TAKES-EVERY-CALL-P about,
as (new, old): the first five cannot take every call, the rest can. -/
def table : List (LL × LL) :=
  [ (⟨1, 0, false, true, false, ["Y"]⟩, ⟨1, 1, false, false, false, []⟩),         -- (x &key y) / (x &optional y)
    (⟨1, 0, false, true, false, ["K"]⟩, ⟨2, 0, false, true, false, ["K"]⟩),      -- (x &key k) / (x y &key k)
    (⟨1, 1, false, true, false, ["K"]⟩, ⟨1, 0, false, true, false, ["K"]⟩),      -- (x &optional y &key k) / (x &key k)
    (⟨1, 0, false, true, true, ["K"]⟩, ⟨1, 0, true, false, false, []⟩),          -- (x &key k &allow-other-keys) / (x &rest r)
    (⟨0, 0, true, true, true, []⟩, ⟨1, 1, false, false, false, []⟩),              -- (&rest a &key &allow-other-keys) / (x &optional y)
    (⟨1, 3, false, false, false, []⟩, ⟨2, 1, false, false, false, []⟩),          -- (x &optional y z w) / (x y &optional z)
    (⟨1, 1, false, true, false, ["K"]⟩, ⟨2, 0, false, false, false, []⟩),        -- (x &optional y &key k) / (x y)
    (⟨1, 1, false, true, false, ["K", "J"]⟩, ⟨1, 1, false, true, false, ["K"]⟩), -- (x &optional y &key k j) / (x &optional y &key k)
    (⟨1, 0, true, true, false, ["K"]⟩, ⟨1, 0, false, true, false, ["K"]⟩),       -- (x &rest r &key k) / (x &key k)
    (⟨1, 0, true, false, false, []⟩, ⟨2, 1, false, true, false, ["K"]⟩),         -- (x &rest r) / (x y &optional z &key k)
    (⟨0, 0, true, true, true, []⟩, ⟨2, 0, false, false, false, []⟩) ]             -- (&rest a &key &allow-other-keys) / (a b)

theorem table_takes :
    table.map (fun p => takes p.1 p.2) =
      [false, false, false, false, false, true, true, true, true, true, true] := by
  decide

/-- Main took all five that cannot. -/
theorem table_main :
    (table.take 5).map (fun p => takesMain p.1 p.2) = [true, true, true, true, true] := by
  decide

/-- Main's check took a list that refuses its callers' calls. -/
theorem main_unsound :
    let new : LL := ⟨1, 0, false, true, false, ["Y"]⟩   -- (x &key y)
    let old : LL := ⟨1, 1, false, false, false, []⟩     -- (x &optional y)
    takesMain new old = true ∧ accepts old [.val, .val] = true ∧
      accepts new [.val, .val] = false ∧ takes new old = false := by decide

end SelfEditHold.Refusal



namespace SelfEditHold.Batch

/-! ## 2. The hold

When an eval redefines names standing on its own stack, provisional.lisp
installs the new definitions, HOLDs them as one batch (the form), and
writes the batch's form to the layer only once every name of it has
RETurned from a traversal outside the model's eval. A traversal that
signals (ERR) before then puts back, for every name of the batch, the
definition the hold displaced, and wakes the session. Esc (DROP) is no
verdict; REVERT is the operator's verb.

`live n` is what n runs; `holds n = some (b, p)` while batch b holds n,
`p` the definition the hold displaced (a re-arming inherits the original
one); `base n` is what n last ran with no hold over it; `log` lists every
(batch, name) that returned.

`written_returned`: on every run, every name of a batch in the layer
returned. `never_both`: no batch is both written and reverted.
`err_restores`: a signalling traversal puts back, for every name of its
batch, what the hold displaced, which is what that name last ran with no
hold over it. The traces at the end are the ones provisional-test.lisp pins
on the real functions; `pairBrokenMain` is main 75a703a, where the first
return of any one name confirmed the batch.
-/

abbrev Def := Nat
abbrev Name := Nat

inductive BState where
  | idle | armed | confirmed | reverted
  deriving DecidableEq, Repr

structure HState where
  live : Name → Def
  holds : Name → Option (Nat × Def)
  base : Name → Def
  names : Nat → List Name
  proven : Nat → List Name
  bst : Nat → BState
  next : Nat
  layer : List Nat
  wakes : List Nat
  log : List (Nat × Name)

inductive Ev where
  | hold (names : List Name) (d : Def)
  | ret (name : Name)
  | err (name : Name)
  | drop (name : Name)
  | revert (name : Name)
  deriving DecidableEq, Repr

/-- What a hold of `m` keeps to put back: the original definition when `m` is
held already. -/
def escrow (s : HState) (m : Name) : Def :=
  match s.holds m with
  | some (_, p) => p
  | none => s.live m

/-- Batch `b`'s names, released. -/
def unheld (s : HState) (b : Nat) (m : Name) : Option (Nat × Def) :=
  match s.holds m with
  | some (b', p) => if b' = b then none else some (b', p)
  | none => none

/-- The last name returned: the batch is written, its names keep their new
definitions. -/
def confirm (s : HState) (b : Nat) : HState :=
  { s with
    bst := fun x => if x = b then .confirmed else s.bst x,
    layer := b :: s.layer,
    base := fun m => match s.holds m with
      | some (b', _) => if b' = b then s.live m else s.base m
      | none => s.base m,
    holds := unheld s b }

/-- A traversal signalled, or the operator reverted: every name of the batch
runs what its hold displaced. -/
def revertBatch (s : HState) (b : Nat) (wake : Bool) : HState :=
  { s with
    bst := fun x => if x = b then .reverted else s.bst x,
    wakes := if wake then b :: s.wakes else s.wakes,
    live := fun m => match s.holds m with
      | some (b', p) => if b' = b then p else s.live m
      | none => s.live m,
    holds := unheld s b }

/-- A traversal of `n` returned: `n` is proven for its batch. -/
def prove (s : HState) (b : Nat) (n : Name) : HState :=
  { s with
    proven := fun x => if x = b then n :: s.proven b else s.proven x,
    log := (b, n) :: s.log }

def step (s : HState) : Ev → HState
  | .hold ns d =>
    { s with
      live := fun m => if m ∈ ns then d else s.live m,
      holds := fun m => if m ∈ ns then some (s.next, escrow s m) else s.holds m,
      names := fun x => if x = s.next then ns else s.names x,
      proven := fun x => if x = s.next then [] else s.proven x,
      bst := fun x => if x = s.next then .armed else s.bst x,
      next := s.next + 1 }
  | .ret n =>
    match s.holds n with
    | some (b, _) =>
      if s.bst b = .armed then
        if (s.names b).all (fun m => decide (m ∈ n :: s.proven b)) then confirm (prove s b n) b
        else prove s b n
      else s
    | none => s
  | .err n =>
    match s.holds n with
    | some (b, _) => if s.bst b = .armed then revertBatch s b true else s
    | none => s
  | .drop _ => s
  | .revert n =>
    match s.holds n with
    | some (b, _) => revertBatch s b false
    | none => s

def run (s : HState) (es : List Ev) : HState := es.foldl step s

def root (n : Name) : Def := n + 100

def Z : HState where
  live := root
  holds := fun _ => none
  base := root
  names := fun _ => []
  proven := fun _ => []
  bst := fun _ => .idle
  next := 0
  layer := []
  wakes := []
  log := []

/-- What every reachable state keeps. -/
structure Inv (s : HState) : Prop where
  held : ∀ n b p, s.holds n = some (b, p) → s.bst b = .armed ∧ b < s.next ∧ p = s.base n
  free : ∀ n, s.holds n = none → s.live n = s.base n
  layered : ∀ b, b ∈ s.layer → s.bst b = .confirmed
  proved : ∀ b, s.bst b = .confirmed → ∀ m ∈ s.names b, m ∈ s.proven b
  logged : ∀ b m, m ∈ s.proven b → (b, m) ∈ s.log
  fresh : ∀ b, s.next ≤ b → s.bst b = .idle ∧ s.proven b = []
  woken : ∀ b, b ∈ s.wakes → s.bst b = .reverted

theorem inv_Z : Inv Z where
  held := by intro n b p h; simp [Z] at h
  free := by intro n _; rfl
  layered := by intro b h; simp [Z] at h
  proved := by intro b h; simp [Z] at h
  logged := by intro b m h; simp [Z] at h
  fresh := by intro b _; exact ⟨rfl, rfl⟩
  woken := by intro b h; simp [Z] at h

theorem armed_lt {s : HState} {b : Nat} (h : Inv s) (ha : s.bst b = .armed) : b < s.next :=
  Nat.lt_of_not_le fun hle => by
    have := (h.fresh b hle).1
    rw [this] at ha
    cases ha

theorem unheld_some {s : HState} {b b' : Nat} {m : Name} {p : Def}
    (h : unheld s b m = some (b', p)) : s.holds m = some (b', p) ∧ b' ≠ b := by
  unfold unheld at h
  split at h
  · rename_i b0 p0 hm
    by_cases hb : b0 = b
    · simp [hb] at h
    · simp [hb] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨hm, hb⟩
  · simp at h

theorem unheld_none {s : HState} {b : Nat} {m : Name} (h : unheld s b m = none) :
    s.holds m = none ∨ ∃ p, s.holds m = some (b, p) := by
  unfold unheld at h
  split at h
  · rename_i b0 p0 hm
    by_cases hb : b0 = b
    · exact Or.inr ⟨p0, by rw [hm, hb]⟩
    · simp [hb] at h
  · rename_i hm
    exact Or.inl hm

theorem inv_confirm {s : HState} {b : Nat} (h : Inv s) (ha : s.bst b = .armed)
    (hall : ∀ m ∈ s.names b, m ∈ s.proven b) : Inv (confirm s b) where
  held := by
    intro n b' p hn
    obtain ⟨hm, hne⟩ := unheld_some hn
    obtain ⟨h1, h2, h3⟩ := h.held n b' p hm
    exact ⟨by simp [confirm, hne, h1], h2, by simp [confirm, hm, hne, h3]⟩
  free := by
    intro n hn
    rcases unheld_none hn with hm | ⟨p, hm⟩
    · simp [confirm, hm, h.free n hm]
    · simp [confirm, hm]
  layered := by
    intro x hx
    simp only [confirm, List.mem_cons] at hx ⊢
    rcases hx with rfl | hx
    · simp
    · by_cases hxb : x = b
      · simp [hxb]
      · simp [hxb, h.layered x hx]
  proved := by
    intro x hx m hm
    simp only [confirm] at hx hm ⊢
    by_cases hxb : x = b
    · subst hxb; exact hall m hm
    · simp [hxb] at hx; exact h.proved x hx m hm
  logged := h.logged
  fresh := by
    intro x hx
    have hxb : x ≠ b := by have := armed_lt h ha; simp only [confirm] at hx; omega
    simp only [confirm] at hx ⊢
    simp [hxb, h.fresh x hx]
  woken := by
    intro x hx
    simp only [confirm] at hx ⊢
    by_cases hxb : x = b
    · subst hxb; rw [h.woken x hx] at ha; cases ha
    · simp [hxb, h.woken x hx]

theorem inv_revertBatch {s : HState} {b : Nat} {wake : Bool} (h : Inv s)
    (ha : s.bst b = .armed) : Inv (revertBatch s b wake) where
  held := by
    intro n b' p hn
    obtain ⟨hm, hne⟩ := unheld_some hn
    obtain ⟨h1, h2, h3⟩ := h.held n b' p hm
    exact ⟨by simp [revertBatch, hne, h1], h2, h3⟩
  free := by
    intro n hn
    rcases unheld_none hn with hm | ⟨p, hm⟩
    · simp [revertBatch, hm, h.free n hm]
    · simp [revertBatch, hm, (h.held n b p hm).2.2]
  layered := by
    intro x hx
    simp only [revertBatch] at hx ⊢
    have hl := h.layered x hx
    have hxb : x ≠ b := by intro hxb; subst hxb; rw [hl] at ha; cases ha
    simp [hxb, hl]
  proved := by
    intro x hx m hm
    simp only [revertBatch] at hx hm ⊢
    by_cases hxb : x = b
    · simp [hxb] at hx
    · simp [hxb] at hx; exact h.proved x hx m hm
  logged := h.logged
  fresh := by
    intro x hx
    have hxb : x ≠ b := by have := armed_lt h ha; simp only [revertBatch] at hx; omega
    simp only [revertBatch] at hx ⊢
    simp [hxb, h.fresh x hx]
  woken := by
    intro x hx
    simp only [revertBatch] at hx ⊢
    by_cases hxb : x = b
    · simp [hxb]
    · have hx' : x ∈ s.wakes := by
        cases wake <;> simp_all
      simp [hxb, h.woken x hx']

theorem inv_prove {s : HState} {b : Nat} {n : Name} (h : Inv s) (ha : s.bst b = .armed) :
    Inv (prove s b n) where
  held := h.held
  free := h.free
  layered := h.layered
  proved := by
    intro x hx m hm
    simp only [prove] at hx hm ⊢
    by_cases hxb : x = b
    · subst hxb; rw [ha] at hx; cases hx
    · simp [hxb] at hm ⊢; exact h.proved x hx m hm
  logged := by
    intro x m hm
    simp only [prove, List.mem_cons] at hm ⊢
    by_cases hxb : x = b
    · subst hxb
      simp at hm
      rcases hm with rfl | hm
      · exact Or.inl rfl
      · exact Or.inr (h.logged x m hm)
    · simp [hxb] at hm; exact Or.inr (h.logged x m hm)
  fresh := by
    intro x hx
    have hxb : x ≠ b := by have := armed_lt h ha; simp only [prove] at hx; omega
    simp only [prove] at hx ⊢
    simp [hxb, h.fresh x hx]
  woken := h.woken

theorem inv_hold {s : HState} {ns : List Name} {d : Def} (h : Inv s) :
    Inv (step s (.hold ns d)) where
  held := by
    intro n b p hn
    simp only [step] at hn ⊢
    by_cases hin : n ∈ ns
    · simp [hin] at hn
      obtain ⟨rfl, rfl⟩ := hn
      refine ⟨by simp, by omega, ?_⟩
      unfold escrow
      split
      · rename_i b0 p0 hm; exact (h.held n b0 p0 hm).2.2
      · rename_i hm; exact h.free n hm
    · simp [hin] at hn
      obtain ⟨h1, h2, h3⟩ := h.held n b p hn
      have hbn : b ≠ s.next := by omega
      exact ⟨by simp [hbn, h1], by omega, h3⟩
  free := by
    intro n hn
    simp only [step] at hn ⊢
    by_cases hin : n ∈ ns
    · simp [hin] at hn
    · simp [hin] at hn ⊢; exact h.free n hn
  layered := by
    intro x hx
    simp only [step] at hx ⊢
    have hl := h.layered x hx
    have hxn : x ≠ s.next := by
      intro hxn; subst hxn; rw [(h.fresh _ (Nat.le_refl _)).1] at hl; cases hl
    simp [hxn, hl]
  proved := by
    intro x hx m hm
    simp only [step] at hx hm ⊢
    by_cases hxn : x = s.next
    · simp [hxn] at hx
    · simp [hxn] at hx hm ⊢; exact h.proved x hx m hm
  logged := by
    intro x m hm
    simp only [step] at hm ⊢
    by_cases hxn : x = s.next
    · simp [hxn] at hm
    · simp [hxn] at hm; exact h.logged x m hm
  fresh := by
    intro x hx
    simp only [step] at hx ⊢
    have hxn : x ≠ s.next := by omega
    simp [hxn, h.fresh x (by omega)]
  woken := by
    intro x hx
    simp only [step] at hx ⊢
    have hw := h.woken x hx
    have hxn : x ≠ s.next := by
      intro hxn; subst hxn; rw [(h.fresh _ (Nat.le_refl _)).1] at hw; cases hw
    simp [hxn, hw]

theorem inv_step {s : HState} (e : Ev) (h : Inv s) : Inv (step s e) := by
  cases e with
  | hold ns d => exact inv_hold h
  | ret n =>
    simp only [step]
    split
    · rename_i b p hn
      by_cases ha : s.bst b = .armed
      · simp only [ha, ite_true]
        split
        · rename_i hall
          refine inv_confirm (inv_prove h ha) (by simp [prove, ha]) ?_
          intro m hm
          simp only [List.all_eq_true, decide_eq_true_eq] at hall
          simpa [prove] using hall m hm
        · exact inv_prove h ha
      · simp [ha]; exact h
    · exact h
  | err n =>
    simp only [step]
    split
    · rename_i b p hn
      by_cases ha : s.bst b = .armed
      · simp only [ha, ite_true]; exact inv_revertBatch h ha
      · simp [ha]; exact h
    · exact h
  | drop n => exact h
  | revert n =>
    simp only [step]
    split
    · rename_i b p hn
      exact inv_revertBatch h (h.held n b p hn).1
    · exact h

theorem inv_foldl : ∀ (es : List Ev) (s : HState), Inv s → Inv (es.foldl step s)
  | [], _, h => h
  | e :: es, s, h => inv_foldl es (step s e) (inv_step e h)

/-- Every name of a batch in the layer returned from a traversal. -/
theorem written_returned (es : List Ev) (b : Nat) (hb : b ∈ (run Z es).layer) :
    ∀ m ∈ (run Z es).names b, (b, m) ∈ (run Z es).log := by
  have h := inv_foldl es Z inv_Z
  intro m hm
  exact h.logged b m (h.proved b (h.layered b hb) m hm)

/-- No batch is both written and reverted. -/
theorem never_both (es : List Ev) (b : Nat) (hb : b ∈ (run Z es).layer) :
    b ∉ (run Z es).wakes := by
  have h := inv_foldl es Z inv_Z
  intro hw
  have h1 := h.layered b hb
  rw [h.woken b hw] at h1
  cases h1

/-- A signalling traversal puts back, for every name of its batch, what the
hold displaced: what that name last ran with no hold over it. -/
theorem err_restores {s : HState} {n : Name} {b : Nat} {p : Def} (h : Inv s)
    (hn : s.holds n = some (b, p)) :
    ∀ m q, s.holds m = some (b, q) →
      (step s (.err n)).live m = s.base m ∧ (step s (.err n)).holds m = none := by
  have ha := (h.held n b p hn).1
  intro m q hm
  have hq := (h.held m b q hm).2.2
  simp only [step, hn, ha, ite_true]
  exact ⟨by simp [revertBatch, hm, hq], by simp [revertBatch, unheld, hm]⟩

/-! ### Traces (provisional-test.lisp pins each on the real functions) -/

def X : Name := 0
def Y : Name := 1

/-- one clean return of each name writes the batch, once -/
theorem pairClean :
    (run Z [.hold [X, Y] 7, .ret X, .ret X, .ret Y]).layer = [0] := by decide

/-- one name's return is no proof of the other -/
theorem pairOneReturn :
    (run Z [.hold [X, Y] 7, .ret X]).layer = [] := by decide

/-- the other name signals: nothing written, both names put back, one wake -/
theorem pairBroken :
    let s := run Z [.hold [X, Y] 7, .ret X, .err Y]
    s.layer = [] ∧ s.wakes = [0] ∧ s.live X = root X ∧ s.live Y = root Y := by decide

/-- a re-arming inherits the original escrow -/
theorem reArm :
    (run Z [.hold [X] 7, .hold [X] 8, .err X]).live X = root X := by decide

/-- Esc is no verdict -/
theorem cancelled :
    (run Z [.hold [X] 7, .drop X, .ret X]).layer = [0] := by decide

/-- once reverted, a later return writes nothing -/
theorem afterRevert :
    (run Z [.hold [X] 7, .err X, .ret X]).layer = [] := by decide

/-- Main 75a703a: the first return of any name confirmed the batch. -/
def stepMain (s : HState) : Ev → HState
  | .ret n =>
    match s.holds n with
    | some (b, _) => if s.bst b = .armed then confirm s b else s
    | none => s
  | e => step s e

/-- Under main's rule the broken name is written and keeps running. -/
theorem pairBrokenMain :
    let s := [Ev.hold [X, Y] 7, .ret X, .err Y].foldl stepMain Z
    s.layer = [0] ∧ s.wakes = [] ∧ s.live Y = 7 := by decide

end SelfEditHold.Batch
