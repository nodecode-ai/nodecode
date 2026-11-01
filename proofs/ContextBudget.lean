/-
Context budget: INSTRUCTIONS-FILES and INSTRUCTIONS-SECTION
(src/src/engine/context.lisp), the rules files a session reads into its
`instructions` section.

Every folder from the session's directory up to / offers its candidates in
name order (*INSTRUCTIONS-FILE-NAMES*: AGENTS.md, CLAUDE.md, .cursorrules),
each missing, unreadable, or a file of text. The folder's file is the first
candidate whose text is readable and not blank: a blank or unreadable one
passes to the next name, as a missing one does. The home's own AGENTS.md ends
its folder's search with no file: it rides as `operator-rules` instead.

The folders' files, nearest first, share one budget of `limit` bytes, a
file's size its UTF-8 bytes (NLK:UTF-8-SIZE), as it takes on disk. They fill
it nearest first: a file that fits beside the nearer files kept is kept; one
that does not is left out whole (and said, once per path), and a farther one
that fits is still read. The section lists the files kept, the farthest
first.

Proved, for every limit and every measure of size: a folder's file is its
first readable, non-blank candidate; the files kept never pass the budget; no
file left out would fit beside the files kept; a file alone over the limit is
never read; the nearest file is read whenever it fits alone; the files kept
and the files left out are the files offered, each in their order.
`readMain` is main 75a703a's reading: the first candidate that existed was
the folder's file, and the budget counted characters. Its decided traces read
nothing from a folder whose AGENTS.md is blank or unreadable beside a
CLAUDE.md, and read a file whose UTF-8 is over the limit.

Checked with `lean -DwarningAsError=true proofs/ContextBudget.lean` (Lean 4, core only).
-/

namespace ContextBudget

abbrev Text := List Char

/-- NLK:TRIMMED's bag: space, tab, return, newline. -/
def isWs (c : Char) : Bool := c == ' ' || c == '\t' || c == '\r' || c == '\n'

/-- A text blank after the trim. -/
def blank (t : Text) : Bool := t.all isWs

/-- NLK:UTF-8-SIZE: the bytes a text takes in UTF-8. -/
def bytes (t : Text) : Nat := (t.map Char.utf8Size).sum

/-- One name of *INSTRUCTIONS-FILE-NAMES* in one folder. -/
inductive Cand where
  | missing
  | unreadable
  /-- the home's own AGENTS.md, which rides as `operator-rules` -/
  | home
  | file (path : Nat) (text : Text)
  deriving DecidableEq, Repr

/-- A file read: its path and its text. -/
abbrev Read := Nat × Text

/-- INSTRUCTIONS-FILES' choice within one folder. -/
def pick : List Cand → Option Read
  | [] => none
  | .home :: _ => none
  | .file p t :: rest => if blank t then pick rest else some (p, t)
  | _ :: rest => pick rest

/-- main 75a703a's choice: the first candidate that exists, read only when
it is readable and not blank. -/
def pickMain : List Cand → Option Read
  | [] => none
  | .missing :: rest => pickMain rest
  | .file p t :: _ => if blank t then none else some (p, t)
  | _ :: _ => none

/-- INSTRUCTIONS-SECTION's fill, nearest first, with `held` bytes kept: the
files kept and the files left out, each nearest first. -/
def fill (size : Text → Nat) (limit : Nat) : Nat → List Read → List Read × List Read
  | _, [] => ([], [])
  | held, f :: fs =>
    if held + size f.2 ≤ limit then (fill size limit (held + size f.2) fs).map (f :: ·) id
    else (fill size limit held fs).map id (f :: ·)

/-- The paths that ride, the farthest first, from folders listed nearest first. -/
def readBy (choose : List Cand → Option Read) (size : Text → Nat) (limit : Nat)
    (folders : List (List Cand)) : List Nat :=
  ((fill size limit 0 (folders.filterMap choose)).1.map Prod.fst).reverse

/-- INSTRUCTIONS-FILES, then INSTRUCTIONS-SECTION -/
def read : Nat → List (List Cand) → List Nat := readBy pick bytes

/-- main 75a703a -/
def readMain : Nat → List (List Cand) → List Nat := readBy pickMain List.length

/-- +INSTRUCTIONS-BYTE-LIMIT+ -/
def limit : Nat := 65536

/-- The bytes a list of files takes. -/
def total (size : Text → Nat) (fs : List Read) : Nat := (fs.map (fun f => size f.2)).sum

/-! ### 1. A folder's file -/

theorem pick_missing (rest : List Cand) : pick (.missing :: rest) = pick rest := rfl

theorem pick_unreadable (rest : List Cand) : pick (.unreadable :: rest) = pick rest := rfl

theorem pick_blank (p : Nat) (t : Text) (rest : List Cand) (h : blank t = true) :
    pick (.file p t :: rest) = pick rest := by
  simp [pick, h]

theorem pick_home (rest : List Cand) : pick (.home :: rest) = none := rfl

theorem pick_file (p : Nat) (t : Text) (rest : List Cand) (h : blank t = false) :
    pick (.file p t :: rest) = some (p, t) := by
  simp [pick, h]

/-- What a folder gives is its first candidate that is a readable file whose
text is not blank: every candidate ahead of it is missing, unreadable or
blank, and none is the home's own AGENTS.md. -/
theorem pick_first : ∀ (cs : List Cand) (r : Read), pick cs = some r →
    ∃ pre post, cs = pre ++ .file r.1 r.2 :: post ∧ blank r.2 = false ∧
      ∀ c ∈ pre, c = .missing ∨ c = .unreadable ∨ ∃ p t, c = .file p t ∧ blank t = true
  | [], r, h => by simp [pick] at h
  | .missing :: rest, r, h => by
    obtain ⟨pre, post, e, hb, hpre⟩ := pick_first rest r h
    refine ⟨.missing :: pre, post, by simp [e], hb, fun c hc => ?_⟩
    rcases List.mem_cons.mp hc with hc | hc
    · exact Or.inl hc
    · exact hpre c hc
  | .unreadable :: rest, r, h => by
    obtain ⟨pre, post, e, hb, hpre⟩ := pick_first rest r h
    refine ⟨.unreadable :: pre, post, by simp [e], hb, fun c hc => ?_⟩
    rcases List.mem_cons.mp hc with hc | hc
    · exact Or.inr (Or.inl hc)
    · exact hpre c hc
  | .home :: rest, r, h => by simp [pick] at h
  | .file p t :: rest, r, h => by
    cases hb : blank t with
    | true =>
      rw [pick_blank p t rest hb] at h
      obtain ⟨pre, post, e, hb', hpre⟩ := pick_first rest r h
      refine ⟨.file p t :: pre, post, by simp [e], hb', fun c hc => ?_⟩
      rcases List.mem_cons.mp hc with hc | hc
      · exact Or.inr (Or.inr ⟨p, t, hc, hb⟩)
      · exact hpre c hc
    | false =>
      rw [pick_file p t rest hb] at h
      injection h with h
      subst h
      exact ⟨[], rest, rfl, hb, by simp⟩

/-! ### 2. The budget -/

theorem fill_within (size : Text → Nat) (limit : Nat) :
    ∀ (held : Nat) (fs : List Read), held ≤ limit →
      held + total size (fill size limit held fs).1 ≤ limit
  | held, [], h => by simpa [fill, total] using h
  | held, f :: fs, h => by
    unfold fill
    split
    · rename_i hfit
      have := fill_within size limit (held + size f.2) fs hfit
      simp only [Prod.map, total, List.map_cons, List.sum_cons] at this ⊢
      omega
    · simpa [Prod.map] using fill_within size limit held fs h

theorem fill_left_out (size : Text → Nat) (limit : Nat) :
    ∀ (held : Nat) (fs : List Read), ∀ g ∈ (fill size limit held fs).2,
      limit < held + total size (fill size limit held fs).1 + size g.2
  | held, [], g, hg => by simp [fill] at hg
  | held, f :: fs, g, hg => by
    unfold fill at hg ⊢
    split
    · rename_i hfit
      simp only [hfit, ↓reduceIte, Prod.map] at hg
      have := fill_left_out size limit (held + size f.2) fs g hg
      simp only [Prod.map, total, List.map_cons, List.sum_cons] at this ⊢
      omega
    · rename_i hfit
      simp only [hfit, ↓reduceIte, Prod.map, List.mem_cons] at hg
      simp only [Prod.map]
      rcases hg with hg | hg
      · subst hg
        omega
      · exact fill_left_out size limit held fs g hg

theorem fill_kept_sublist (size : Text → Nat) (limit : Nat) :
    ∀ (held : Nat) (fs : List Read), List.Sublist (fill size limit held fs).1 fs
  | _, [] => by simp [fill]
  | held, f :: fs => by
    unfold fill
    split
    · simp only [Prod.map]
      exact (fill_kept_sublist size limit _ fs).cons_cons f
    · simp only [Prod.map, id]
      exact (fill_kept_sublist size limit _ fs).cons f

theorem fill_left_out_sublist (size : Text → Nat) (limit : Nat) :
    ∀ (held : Nat) (fs : List Read), List.Sublist (fill size limit held fs).2 fs
  | _, [] => by simp [fill]
  | held, f :: fs => by
    unfold fill
    split
    · simp only [Prod.map, id]
      exact (fill_left_out_sublist size limit _ fs).cons f
    · simp only [Prod.map]
      exact (fill_left_out_sublist size limit _ fs).cons_cons f

theorem fill_count (size : Text → Nat) (limit : Nat) :
    ∀ (held : Nat) (fs : List Read),
      (fill size limit held fs).1.length + (fill size limit held fs).2.length = fs.length
  | _, [] => by simp [fill]
  | held, f :: fs => by
    unfold fill
    split
    · have := fill_count size limit (held + size f.2) fs
      simp only [Prod.map, id, List.length_cons] at this ⊢
      omega
    · have := fill_count size limit held fs
      simp only [Prod.map, id, List.length_cons] at this ⊢
      omega

theorem le_total (size : Text → Nat) :
    ∀ (fs : List Read) (g : Read), g ∈ fs → size g.2 ≤ total size fs
  | [], g, h => by simp at h
  | f :: fs, g, h => by
    simp only [total, List.map_cons, List.sum_cons]
    rcases List.mem_cons.mp h with h | h
    · subst h
      omega
    · have := le_total size fs g h
      simp only [total] at this
      omega

/-- The files read never take more than the budget. -/
theorem within_budget (size : Text → Nat) (limit : Nat) (fs : List Read) :
    total size (fill size limit 0 fs).1 ≤ limit := by
  have := fill_within size limit 0 fs (Nat.zero_le _)
  omega

/-- No file left out would fit beside the files read. -/
theorem left_out_would_not_fit (size : Text → Nat) (limit : Nat) (fs : List Read) :
    ∀ g ∈ (fill size limit 0 fs).2, limit < total size (fill size limit 0 fs).1 + size g.2 := by
  intro g hg
  have := fill_left_out size limit 0 fs g hg
  omega

/-- A file over the limit alone is never read. -/
theorem over_alone_never_read (size : Text → Nat) (limit : Nat) (fs : List Read) :
    ∀ g ∈ (fill size limit 0 fs).1, size g.2 ≤ limit := fun g hg =>
  Nat.le_trans (le_total size _ g hg) (within_budget size limit fs)

/-- The nearest file is read whenever it fits alone. -/
theorem nearest_read (size : Text → Nat) (limit : Nat) (f : Read) (fs : List Read)
    (h : size f.2 ≤ limit) : f ∈ (fill size limit 0 (f :: fs)).1 := by
  simp [fill, h, Prod.map]

/-! ### 3. Decided traces, pinned on the real functions by
`the-instructions-files-run-contextbudget-lean-s-traces` (src/test/engine/context-test.lisp) -/

/-- A blank AGENTS.md passes to its folder's CLAUDE.md... -/
theorem trace_blank_passes :
    read limit [[.file 1 "  \n".toList, .file 2 "claude rules".toList, .missing]] = [2] := by
  decide

/-- ...and so does an unreadable one... -/
theorem trace_unreadable_passes :
    read limit [[.unreadable, .file 2 "claude rules".toList, .missing]] = [2] := by
  decide

/-- ...where main read neither. -/
theorem main_blank_hides :
    readMain limit [[.file 1 "  \n".toList, .file 2 "claude rules".toList, .missing]] = [] ∧
    readMain limit [[.unreadable, .file 2 "claude rules".toList, .missing]] = [] := by
  decide

/-- A file that is not blank still wins its folder. -/
theorem trace_first_wins :
    read limit [[.file 1 "agents rules".toList, .file 2 "claude rules".toList, .missing]] = [1] := by
  decide

theorem trace_last_name :
    read limit [[.missing, .missing, .file 3 "cursor rules".toList]] = [3] := by
  decide

/-- The home reads no other name in its own AGENTS.md's place. -/
theorem trace_home :
    read limit [[.home, .file 2 "claude rules".toList, .missing]] = [] := by
  decide

/-- Over the budget, AGENTS.md is still its folder's file: left out, never
replaced by the next name. -/
theorem trace_over_budget_not_replaced :
    read 16 [[.file 1 "xxxxxxxxxxxxxxxxx".toList, .file 2 "claude".toList, .missing]] = [] := by
  decide

/-- Folders nearest first: the nearest file keeps its place, a farther one that
does not fit beside it is left out, and one still farther that fits is read;
the farthest rides first. -/
theorem trace_fill :
    read 16 [[.file 1 "pppppppppp".toList], [.file 2 "rrrrrrrrr".toList],
      [.file 3 "ssssss".toList]] = [3, 1] := by
  decide

theorem trace_exactly_the_limit : read 16 [[.file 1 "aaaaaaaaaaaaaaaa".toList]] = [1] := by
  decide

/-- Six CJK characters are 18 bytes of UTF-8: over a limit of 16, though six
characters are under it. main counted characters, and read them. -/
theorem trace_bytes_not_characters :
    read 16 [[.file 1 "日本語日本語".toList]] = [] ∧
    readMain 16 [[.file 1 "日本語日本語".toList]] = [1] := by
  decide

end ContextBudget
