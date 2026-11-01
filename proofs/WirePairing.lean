/-
Wire pairing: WIRE-MESSAGES-FROM-EVENTS (src/src/engine/context.lisp) folds a
session's durable facts into the messages a provider receives. A provider
refuses the whole request when a tool message answers no call of the
assistant message before it, when a call has no tool message before the next
message, or when two calls share an id -- and the journal is permanent, so
one such fold would refuse every later round of the session.

The model: `step` is the fold, one event at a time; `wf` is the shape every
provider demands of the wire. `wire_wellFormed` says the fold's output passes
`wf` for every event log whatever, including the shapes a journal can hold
that no provider ever produced: a result recorded twice, a result landing
after a later message, an id a later message reuses, an id a message
carries twice, a call fact journaled while another call is opened.

Ids: a call keeps the id its provider sent unless it is blank or already on
the wire; then the fold mints `call-POS-K` from the message's log position
and the call's index in it (`WId.minted`), the same name on every fold.

Checked by `just proofs` (Lean 4, core only).
-/

namespace WirePairing

/-- A journaled call id: what the provider sent, "" when it sent none. -/
abbrev JId := String

/-- A call id on the wire. -/
inductive WId where
  | given (id : String)
  | minted (pos k : Nat)
  deriving DecidableEq, Repr

/-- A durable fact as the fold reads it. A stored assistant message carries
`some ids` when it has tool_calls (even none), `none` when it is plain. -/
inductive Ev where
  | user
  | asst (calls : Option (List JId))
  | call (id : JId)
  | result (id : JId)
  | reminder
  deriving DecidableEq, Repr

/-- A wire message. `asst []` is a plain assistant message. -/
inductive Msg where
  | user
  | sys
  | asst (calls : List WId)
  | tool (id : WId)
  deriving DecidableEq, Repr

/-! ## What a provider demands -/

/-- `opened`: the calls of the assistant message just before, still unanswered;
`seen`: every call id issued so far. -/
def wfFrom (opened seen : List WId) : List Msg → Bool
  | [] => opened.isEmpty
  | .tool w :: rest => opened.contains w && wfFrom (opened.filter (· != w)) seen rest
  | .asst calls :: rest =>
    opened.isEmpty && decide calls.Nodup && calls.all (fun c => !seen.contains c)
      && wfFrom calls (seen ++ calls) rest
  | .user :: rest => opened.isEmpty && wfFrom opened seen rest
  | .sys :: rest => opened.isEmpty && wfFrom opened seen rest

/-- Well-formed: every tool message answers a call of the assistant message
right before it, every call is answered exactly once before anything else
follows, and no call id is issued twice. -/
def wf (msgs : List Msg) : Bool := wfFrom [] [] msgs

/-! ## The fold -/

/-- An issued call awaiting its tool message: its wire id and the journaled
id its result is recorded under. -/
structure Entry where
  wire : WId
  key : JId
  deriving DecidableEq, Repr

structure St where
  /-- the log position of the next event -/
  pos : Nat
  /-- the wire so far, oldest first -/
  out : List Msg
  /-- call facts awaiting a flush, oldest first: (position, journaled id) -/
  pending : List (Nat × JId)
  /-- the newest assistant message carried its own tool_calls -/
  stored : Bool
  /-- issued calls awaiting a tool message, oldest first -/
  opened : List Entry
  /-- every wire id issued so far -/
  used : List WId
  /-- reminders held until the opened calls close -/
  held : Nat
  deriving Repr

def init (pos : Nat) : St := ⟨pos, [], [], false, [], [], 0⟩

/-- The wire id of one call: the journaled id, unless blank or already on
the wire -- then `call-MPOS-K`. -/
def wireId (used : List WId) (mpos k : Nat) (id : JId) : WId :=
  if id == "" || used.contains (WId.given id) then WId.minted mpos k else WId.given id

/-- Issue a message's calls, the k-th against everything issued before it. -/
def issue (mpos : Nat) (used : List WId) : List JId → List Entry → List Entry
  | [], acc => acc
  | id :: ids, acc =>
    issue mpos used ids (acc ++ [⟨wireId (used ++ acc.map Entry.wire) mpos acc.length id, id⟩])

/-- Release the held reminders. -/
def emitHeld (s : St) : St :=
  { s with out := s.out ++ List.replicate s.held .sys, held := 0 }

/-- Pending call facts flush as one synthetic assistant message, unless a
stored message already carries them. -/
def flush (s : St) : St :=
  match s.pending with
  | [] => s
  | (p, _) :: _ =>
    if s.stored then { s with pending := [] }
    else
      let new := issue p s.used (s.pending.map Prod.snd) []
      { s with out := s.out ++ [.asst (new.map Entry.wire)], pending := [],
               opened := s.opened ++ new, used := s.used ++ new.map Entry.wire }

/-- Before the next message: flush, answer every opened call that it has no
recorded result, release the reminders. -/
def close (s : St) : St :=
  let s := flush s
  emitHeld { s with out := s.out ++ s.opened.map (fun e => Msg.tool e.wire),
                    opened := [], stored := false }

/-- A result answers the oldest opened call recorded under its id, or nothing. -/
def pair (s : St) (id : JId) : St :=
  match s.opened.find? (fun e => e.key == id) with
  | none => s
  | some e => { s with out := s.out ++ [.tool e.wire],
                       opened := s.opened.filter (fun e' => e'.wire != e.wire) }

def step (s : St) : Ev → St
  | .user =>
    let s := close s
    { s with out := s.out ++ [.user], pos := s.pos + 1 }
  | .asst none =>
    let s := close s
    { s with out := s.out ++ [.asst []], pos := s.pos + 1 }
  | .asst (some ids) =>
    let s := close s
    let new := issue s.pos s.used ids []
    { s with out := s.out ++ [.asst (new.map Entry.wire)], stored := true,
             opened := s.opened ++ new, used := s.used ++ new.map Entry.wire, pos := s.pos + 1 }
  | .call id =>
    -- a call fact while a call is opened is nested work: it folds as nothing
    if s.opened.isEmpty then { s with pending := s.pending ++ [(s.pos, id)], pos := s.pos + 1 }
    else { s with pos := s.pos + 1 }
  | .result id =>
    let s := pair (flush s) id
    let s := if s.opened.isEmpty then emitHeld s else s
    { s with pos := s.pos + 1 }
  | .reminder =>
    let s := { s with held := s.held + 1 }
    let s := if s.pending.isEmpty && s.opened.isEmpty then emitHeld s else s
    { s with pos := s.pos + 1 }

def fold (s : St) (evs : List Ev) : St := evs.foldl step s

/-- The wire of a log whose first event sits at `pos`. -/
def run (pos : Nat) (evs : List Ev) : List Msg := (close (fold (init pos) evs)).out

/-- A cut point: no call pending or opened. -/
def clean (s : St) : Bool := s.pending.isEmpty && s.opened.isEmpty

/-! ## The five shapes a provider refuses, as the fold on main folded them -/

def dupResult : List Ev := [.user, .asst (some ["a"]), .call "a", .result "a", .result "a"]
def lateResult : List Ev := [.user, .asst (some ["a"]), .call "a", .user, .result "a"]
def reusedId : List Ev :=
  [.user, .asst (some ["a"]), .call "a", .result "a", .asst (some ["a"]), .call "a", .user]
def dupInMessage : List Ev :=
  [.user, .asst (some ["a", "a"]), .call "a", .call "a", .result "a", .user]
def flushWhileOpen : List Ev :=
  [.user, .call "a", .call "b", .result "a", .call "c", .result "c", .result "b"]

theorem trace_dupResult :
    run 1 dupResult = [.user, .asst [.given "a"], .tool (.given "a")] := by decide
theorem trace_lateResult :
    run 1 lateResult = [.user, .asst [.given "a"], .tool (.given "a"), .user] := by decide
theorem trace_reusedId :
    run 1 reusedId = [.user, .asst [.given "a"], .tool (.given "a"),
                      .asst [.minted 5 0], .tool (.minted 5 0), .user] := by decide
theorem trace_dupInMessage :
    run 1 dupInMessage = [.user, .asst [.given "a", .minted 2 1], .tool (.given "a"),
                          .tool (.minted 2 1), .user] := by decide
theorem trace_flushWhileOpen :
    run 1 flushWhileOpen = [.user, .asst [.given "a", .given "b"], .tool (.given "a"),
                            .tool (.given "b")] := by decide


/-! ## The checker -/

theorem wfFrom_sys (seen : List WId) (tail : List Msg) :
    ∀ n, wfFrom [] seen (List.replicate n .sys ++ tail) = wfFrom [] seen tail
  | 0 => rfl
  | n + 1 => by simp [List.replicate, wfFrom, wfFrom_sys seen tail n]

theorem filter_ne_self (ws : List WId) (w : WId) (h : w ∉ ws) : ws.filter (· != w) = ws := by
  induction ws with
  | nil => rfl
  | cons x xs ih =>
    have hx : x ≠ w := fun e => h (e ▸ List.mem_cons_self ..)
    have hxs : w ∉ xs := fun m => h (List.mem_cons_of_mem _ m)
    simp [hx, ih hxs]

/-- Every open call answered in order as having no result keeps the wire
well-formed. -/
theorem wfFrom_stubs (seen : List WId) (tail : List Msg) :
    ∀ ws : List WId, ws.Nodup → wfFrom [] seen tail = true →
      wfFrom ws seen (ws.map Msg.tool ++ tail) = true
  | [], _, h => by simpa using h
  | w :: ws, hn, h => by
    have hw : w ∉ ws := (List.nodup_cons.mp hn).1
    have hws : ws.Nodup := (List.nodup_cons.mp hn).2
    simp [wfFrom, filter_ne_self ws w hw, wfFrom_stubs seen tail ws hws h]

theorem map_filter_wire (l : List Entry) (w : WId) :
    (l.filter (fun e => e.wire != w)).map Entry.wire = (l.map Entry.wire).filter (· != w) := by
  induction l with
  | nil => rfl
  | cons h t ih => by_cases hw : h.wire = w <;> simp [hw, ih]

/-! ## Issuing -/

/-- The calls `issue` adds carry distinct wire ids, none issued before, and
mint only at `mpos`. -/
theorem issue_spec (mpos : Nat) (used : List WId) (ids : List JId) :
    ∀ acc : List Entry,
      (acc.map Entry.wire).Nodup →
      (∀ w ∈ acc.map Entry.wire, w ∉ used) →
      (∀ p k, WId.minted p k ∈ acc.map Entry.wire → p = mpos ∧ k < acc.length) →
      (∀ k, WId.minted mpos k ∉ used) →
      ((issue mpos used ids acc).map Entry.wire).Nodup ∧
      (∀ w ∈ (issue mpos used ids acc).map Entry.wire, w ∉ used) ∧
      (∀ p k, WId.minted p k ∈ (issue mpos used ids acc).map Entry.wire →
        p = mpos ∧ k < (issue mpos used ids acc).length) := by
  induction ids with
  | nil => intro acc h1 h2 h3 _; exact ⟨h1, h2, h3⟩
  | cons id ids ih =>
    intro acc h1 h2 h3 h4
    simp only [issue]
    apply ih
    · -- the new wire id is not among those issued in this message
      rw [List.map_append, List.nodup_append]
      refine ⟨h1, by simp, ?_⟩
      intro a ha b hb
      simp at hb
      subst hb
      unfold wireId
      split
      · rename_i hc
        intro e
        have := (h3 _ _ (e ▸ ha)).2
        omega
      · rename_i hc
        intro e
        apply hc
        simp only [Bool.or_eq_true, List.contains_iff_mem, List.mem_append]
        exact Or.inr (Or.inr (e ▸ ha))
    · intro w hw
      rw [List.map_append] at hw
      rcases List.mem_append.mp hw with h | h
      · exact h2 w h
      · simp at h
        subst h
        unfold wireId
        split
        · exact h4 _
        · rename_i hc
          intro m
          apply hc
          simp only [Bool.or_eq_true, List.contains_iff_mem, List.mem_append]
          exact Or.inr (Or.inl m)
    · intro p k hk
      rw [List.map_append] at hk
      rcases List.mem_append.mp hk with h | h
      · have := h3 p k h
        simp
        omega
      · simp at h
        unfold wireId at h
        split at h
        · cases h
          simp
        · cases h
    · exact h4

/-! ## The invariant -/

/-- What holds of the fold's state at every event. -/
structure Inv (s : St) : Prop where
  /-- the wire so far takes any continuation the open calls and issued ids allow -/
  wf : ∀ tail, wfFrom (s.opened.map Entry.wire) s.used tail = true →
    wfFrom [] [] (s.out ++ tail) = true
  /-- a pending call fact landed while nothing was open -/
  pend : s.pending ≠ [] → s.opened = []
  nodup : (s.opened.map Entry.wire).Nodup
  /-- a minted id names a position already passed -/
  usedPos : ∀ p k, WId.minted p k ∈ s.used → p < s.pos
  pendPos : ∀ q ∈ s.pending, q.1 < s.pos
  /-- every pending call fact is newer than every minted id -/
  pendFresh : ∀ p k, WId.minted p k ∈ s.used → ∀ q ∈ s.pending, p < q.1

theorem inv_init (pos : Nat) : Inv (init pos) where
  wf := fun tail h => by simpa [init] using h
  pend := fun h => (h rfl).elim
  nodup := List.nodup_nil
  usedPos := fun _ _ h => by simp [init] at h
  pendPos := fun _ h => by simp [init] at h
  pendFresh := fun _ _ h => by simp [init] at h

theorem inv_bump {s : St} (h : Inv s) : Inv { s with pos := s.pos + 1 } where
  wf := h.wf
  pend := h.pend
  nodup := h.nodup
  usedPos := fun p k m => Nat.lt_succ_of_lt (h.usedPos p k m)
  pendPos := fun q m => Nat.lt_succ_of_lt (h.pendPos q m)
  pendFresh := h.pendFresh

theorem inv_held {s : St} (h : Inv s) (n : Nat) : Inv { s with held := n } where
  wf := h.wf
  pend := h.pend
  nodup := h.nodup
  usedPos := h.usedPos
  pendPos := h.pendPos
  pendFresh := h.pendFresh

theorem inv_emitHeld {s : St} (h : Inv s) (ho : s.opened = []) : Inv (emitHeld s) where
  wf := fun tail ht => by
    have ht' : wfFrom [] s.used tail = true := by simpa [emitHeld, ho] using ht
    have := h.wf (List.replicate s.held .sys ++ tail)
    rw [ho, List.map_nil, wfFrom_sys] at this
    show wfFrom [] [] ((s.out ++ List.replicate s.held .sys) ++ tail) = true
    rw [List.append_assoc]
    exact this ht'
  pend := fun hp => by simpa [emitHeld] using h.pend hp
  nodup := h.nodup
  usedPos := h.usedPos
  pendPos := h.pendPos
  pendFresh := h.pendFresh

theorem inv_flush {s : St} (h : Inv s) : Inv (flush s) := by
  unfold flush
  split
  · exact h
  · rename_i p id rest hp
    have ho : s.opened = [] := h.pend (by simp [hp])
    have hpp : p < s.pos := h.pendPos (p, id) (by simp [hp])
    split
    · exact { wf := h.wf, pend := fun c => (c rfl).elim, nodup := h.nodup,
              usedPos := h.usedPos, pendPos := fun _ m => by simp at m,
              pendFresh := fun _ _ _ _ m => by simp at m }
    · have spec := issue_spec p s.used (s.pending.map Prod.snd) []
        List.nodup_nil (fun _ m => by simp at m) (fun _ _ m => by simp at m)
        (fun k m => by
          have := h.pendFresh p k m (p, id) (by simp [hp])
          omega)
      exact {
        wf := fun tail ht => by
          have := h.wf (.asst ((issue p s.used (s.pending.map Prod.snd) []).map Entry.wire) :: tail)
          simp only [ho, List.map_nil, wfFrom, List.isEmpty_nil, Bool.true_and,
                     List.nil_append] at this ht ⊢
          rw [List.append_assoc]
          apply this
          rw [decide_eq_true spec.1, Bool.true_and, List.all_eq_true.mpr, Bool.true_and]
          · exact ht
          · intro w hw
            simpa using spec.2.1 w hw
        pend := fun c => (c rfl).elim
        nodup := by simpa [ho] using spec.1
        usedPos := fun p' k m => by
          show p' < s.pos
          rcases List.mem_append.mp m with m | m
          · exact h.usedPos p' k m
          · have := (spec.2.2 p' k m).1
            omega
        pendPos := fun _ m => by simp at m
        pendFresh := fun _ _ _ _ m => by simp at m }

theorem close_opened (s : St) : (close s).opened = [] := by simp [close, emitHeld]

theorem close_pending (s : St) : (close s).pending = [] := by
  simp only [close, emitHeld]
  unfold flush
  split <;> (try split) <;> simp_all

theorem close_pos (s : St) : (close s).pos = s.pos := by
  simp only [close, emitHeld]
  unfold flush
  split <;> (try split) <;> rfl

theorem inv_close {s : St} (h : Inv s) : Inv (close s) := by
  have hf := inv_flush h
  unfold close
  apply inv_emitHeld _ rfl
  exact {
    wf := fun tail ht => by
      have := hf.wf ((flush s).opened.map (fun e => Msg.tool e.wire) ++ tail)
      simp only [List.map_nil] at ht
      rw [List.append_assoc]
      apply this
      have e : (flush s).opened.map (fun e => Msg.tool e.wire)
          = ((flush s).opened.map Entry.wire).map Msg.tool := by
        rw [List.map_map]; rfl
      rw [e]
      exact wfFrom_stubs _ _ _ hf.nodup ht
    pend := fun _ => rfl
    nodup := List.nodup_nil
    usedPos := hf.usedPos
    pendPos := hf.pendPos
    pendFresh := hf.pendFresh }

theorem inv_pair {s : St} (h : Inv s) (id : JId) : Inv (pair s id) := by
  unfold pair
  split
  · exact h
  · rename_i e he
    have hm : e ∈ s.opened := List.mem_of_find?_eq_some he
    have hw : e.wire ∈ s.opened.map Entry.wire := List.mem_map_of_mem hm
    exact {
      wf := fun tail ht => by
        have hc : (s.opened.map Entry.wire).contains e.wire = true := by simpa using hw
        have := h.wf (.tool e.wire :: tail)
        simp only [wfFrom, hc, Bool.true_and, map_filter_wire] at this ht
        rw [List.append_assoc]
        exact this ht
      pend := fun hp => by simp [h.pend hp]
      nodup := by rw [map_filter_wire]; exact h.nodup.filter _
      usedPos := h.usedPos
      pendPos := h.pendPos
      pendFresh := h.pendFresh }

theorem inv_step {s : St} (h : Inv s) : ∀ e, Inv (step s e)
  | .user => by
    have hc := inv_close h
    simp only [step]
    refine inv_bump (s := { close s with out := (close s).out ++ [.user] }) ?_
    exact {
      wf := fun tail ht => by
        have := hc.wf (.user :: tail)
        simp only [close_opened, List.map_nil, wfFrom, List.isEmpty_nil, Bool.true_and] at this ht ⊢
        rw [List.append_assoc]
        exact this ht
      pend := fun hp => by simp [close_opened]
      nodup := List.nodup_nil
      usedPos := hc.usedPos
      pendPos := hc.pendPos
      pendFresh := hc.pendFresh }
  | .asst none => by
    have hc := inv_close h
    simp only [step]
    refine inv_bump (s := { close s with out := (close s).out ++ [.asst []] }) ?_
    exact {
      wf := fun tail ht => by
        have := hc.wf (.asst [] :: tail)
        simp only [close_opened, List.map_nil, wfFrom, List.isEmpty_nil, Bool.true_and,
                   List.nodup_nil, decide_true, List.all_nil, List.append_nil] at this ht ⊢
        rw [List.append_assoc]
        exact this ht
      pend := fun hp => by simp [close_opened]
      nodup := List.nodup_nil
      usedPos := hc.usedPos
      pendPos := hc.pendPos
      pendFresh := hc.pendFresh }
  | .asst (some ids) => by
    have hc := inv_close h
    simp only [step]
    have spec := issue_spec (close s).pos (close s).used ids []
      List.nodup_nil (fun _ m => by simp at m) (fun _ _ m => by simp at m)
      (fun k m => by have := hc.usedPos _ k m; omega)
    exact {
      wf := fun tail ht => by
        have := hc.wf (.asst ((issue (close s).pos (close s).used ids []).map Entry.wire) :: tail)
        simp only [close_opened, List.map_nil, wfFrom, List.isEmpty_nil, Bool.true_and,
                   List.nil_append] at this ht ⊢
        rw [List.append_assoc]
        apply this
        rw [decide_eq_true spec.1, Bool.true_and, List.all_eq_true.mpr, Bool.true_and]
        · exact ht
        · intro w hw
          simpa using spec.2.1 w hw
      pend := fun c => (c (close_pending s)).elim
      nodup := by simpa [close_opened] using spec.1
      usedPos := fun p k m => by
        show p < (close s).pos + 1
        rcases List.mem_append.mp m with m | m
        · exact Nat.lt_succ_of_lt (hc.usedPos p k m)
        · have := (spec.2.2 p k m).1
          omega
      pendPos := fun q m => by simp [close_pending] at m
      pendFresh := fun _ _ _ q m => by simp [close_pending] at m }
  | .call id => by
    simp only [step]
    split
    · rename_i ho
      have ho' : s.opened = [] := List.isEmpty_iff.mp ho
      exact {
        wf := h.wf
        pend := fun _ => ho'
        nodup := h.nodup
        usedPos := fun p k m => Nat.lt_succ_of_lt (h.usedPos p k m)
        pendPos := fun q m => by
          rcases List.mem_append.mp m with m | m
          · exact Nat.lt_succ_of_lt (h.pendPos q m)
          · simp at m; subst m; simp
        pendFresh := fun p k mu q m => by
          rcases List.mem_append.mp m with m | m
          · exact h.pendFresh p k mu q m
          · simp at m; subst m; exact h.usedPos p k mu }
    · exact inv_bump h
  | .result id => by
    simp only [step]
    apply inv_bump
    split
    · rename_i ho
      exact inv_emitHeld (inv_pair (inv_flush h) id) (List.isEmpty_iff.mp ho)
    · exact inv_pair (inv_flush h) id
  | .reminder => by
    simp only [step]
    apply inv_bump
    split
    · rename_i ho
      have ho' : s.opened = [] := List.isEmpty_iff.mp (Bool.and_eq_true_iff.mp ho).2
      exact inv_emitHeld (inv_held h _) ho'
    · exact inv_held h _

theorem inv_fold {s : St} (h : Inv s) : ∀ evs, Inv (fold s evs)
  | [] => h
  | e :: evs => inv_fold (inv_step h e) evs

/-- Whatever the log holds, the wire the fold builds from it is one every
provider accepts. -/
theorem wire_wellFormed (pos : Nat) (evs : List Ev) : wf (run pos evs) = true := by
  have hc := inv_close (inv_fold (inv_init pos) evs)
  have := hc.wf []
  simp only [close_opened, List.map_nil, wfFrom, List.isEmpty_nil, List.append_nil] at this
  exact this trivial

/-- The five shapes, on the model: each wire well-formed. -/
theorem shapes_wellFormed :
    [dupResult, lateResult, reusedId, dupInMessage, flushWhileOpen].all
      (fun evs => wf (run 1 evs)) = true := by decide

end WirePairing
