/-
Credential chain: RESOLVE-PROVIDER-CREDENTIAL (src/src/engine/provider.lisp),
the key a request to a provider sends.

The tiers, first to last: the operator's config (NODECODE_API_KEY, the
startup parameter *API-KEY*, else providers.<id>.api_key), where a value
opening with `!` names a command whose output is the key; the store,
auth.json's api_keys.<id>.key (AUTH-API-KEY); the `:credential` hooks a cell
adds; the environment, <ID>_API_KEY first (CREDENTIAL-ENV); and the keyless
"public". Every tier reads its value by one rule, PRESENT-TEXT: trimmed of
NLK:TRIMMED's blanks, and a value blank after the trim is an absent one, so
the next tier answers. A command that is empty, fails or prints nothing stops
the chain (CREDENTIAL-FAIL) rather than passing to a later tier. A probe, a
question of where the key comes from, runs no command: the config answers.

Proved: a blank value is an absent one at every tier that reads a value;
blanks around a value change nothing; every key the chain's own tiers answer
is clean (not empty, no blank at either end); the tiers answer in order; a
command answers or fails, never a later tier. `resolveMain` is main 75a703a's
reading, which took NODECODE_API_KEY and auth.json's key untrimmed whenever
they were not empty: its decided traces send a blank key and a padded one.

Checked with `lean -DwarningAsError=true proofs/CredentialChain.lean` (Lean 4, core only).
-/

namespace CredentialChain

abbrev Text := List Char

/-- NLK:TRIMMED's bag: space, tab, return, newline. -/
def isWs (c : Char) : Bool := c == ' ' || c == '\t' || c == '\r' || c == '\n'

/-- `nlk:trimmed` -/
def trim (t : Text) : Text := ((t.dropWhile isWs).reverse.dropWhile isWs).reverse

/-- `present-text`: a value trimmed, or none when it is absent or blank after the trim. -/
def present : Option Text → Option Text
  | none => none
  | some t => if trim t = [] then none else some (trim t)

/-- main's reading of NODECODE_API_KEY and of auth.json's key: any value but the empty one, untrimmed. -/
def nonEmpty : Option Text → Option Text
  | some [] => none
  | v => v

/-- Where a key comes from; `keyless` is `:public`, the keyless default. -/
inductive Source where
  | config | apiKey | hook | env | keyless
  deriving DecidableEq, Repr

inductive Outcome where
  | key (source : Source) (key : Text)
  | fail
  deriving DecidableEq, Repr

/-- What one resolution reads. -/
structure World where
  /-- NODECODE_API_KEY, the startup parameter *API-KEY* -/
  flag : Option Text := none
  /-- providers.<id>.api_key -/
  config : Option Text := none
  /-- what a command prints, or none when it fails or runs past its bound -/
  run : Text → Option Text := fun _ => none
  /-- auth.json api_keys.<id>.key -/
  store : Option Text := none
  /-- a `:credential` hook's key, or none when every hook passes -/
  hook : Option Text := none
  /-- <ID>_API_KEY, then the SDK default -/
  env : Option Text := none
  /-- a question of where the key comes from: no command runs -/
  probe : Bool := false

def publicKey : Text := "public".toList

/-- The command tier: `c` is what follows the `!`. -/
def command (w : World) (c : Text) : Outcome :=
  match present (some c) with
  | none => .fail
  | some c =>
    match w.run c with
    | none => .fail
    | some out =>
      match present (some out) with
      | none => .fail
      | some k => .key .config k

/-- The tiers behind the config, auth.json's key read by `read`. -/
def behind (read : Option Text → Option Text) (w : World) : Outcome :=
  match read w.store with
  | some k => .key .apiKey k
  | none =>
    match w.hook with
    | some k => .key .hook k
    | none =>
      match present w.env with
      | some k => .key .env k
      | none => .key .keyless publicKey

/-- The chain, NODECODE_API_KEY and auth.json's key read by `read`. -/
def chain (read : Option Text → Option Text) (w : World) : Outcome :=
  match (read w.flag).or (present w.config) with
  | some ('!' :: c) => if w.probe then .key .config ('!' :: c) else command w c
  | some k => .key .config k
  | none => behind read w

/-- RESOLVE-PROVIDER-CREDENTIAL -/
def resolve : World → Outcome := chain present

/-- main 75a703a -/
def resolveMain : World → Outcome := chain nonEmpty

/-! ### 1. Trimming -/

theorem dropWhile_pre {p : α → Bool} :
    ∀ (pad l : List α), (∀ c ∈ pad, p c = true) → (pad ++ l).dropWhile p = l.dropWhile p
  | [], _, _ => rfl
  | c :: pad, l, h => by
    have hc : p c = true := h c (by simp)
    simp only [List.cons_append, List.dropWhile_cons, hc, ↓reduceIte]
    exact dropWhile_pre pad l (fun x hx => h x (by simp [hx]))

theorem dropWhile_all {p : α → Bool} (l : List α) (h : ∀ c ∈ l, p c = true) :
    l.dropWhile p = [] := by
  have := dropWhile_pre (p := p) l [] h
  simpa using this

/-- Blanks around a text are trimmed away. -/
theorem trim_pad (pad t pad' : Text) (hp : ∀ c ∈ pad, isWs c = true)
    (hq : ∀ c ∈ pad', isWs c = true) : trim (pad ++ t ++ pad') = trim t := by
  unfold trim
  rw [List.append_assoc, dropWhile_pre pad (t ++ pad') hp, List.dropWhile_append]
  split
  · rename_i hempty
    rw [dropWhile_all pad' hq, List.isEmpty_iff.mp hempty]
  · rw [List.reverse_append,
      dropWhile_pre pad'.reverse _ (fun c hc => hq c (List.mem_reverse.mp hc))]

theorem head_dropWhile {p : α → Bool} :
    ∀ (l : List α) (a : α), (l.dropWhile p).head? = some a → p a = false
  | [], _, h => by simp at h
  | x :: xs, a, h => by
    rw [List.dropWhile_cons] at h
    split at h
    · exact head_dropWhile xs a h
    · rename_i hx
      simp only [List.head?_cons, Option.some.injEq] at h
      subst h
      simpa using hx

theorem last_dropWhile {p : α → Bool} (l : List α) (h : l.dropWhile p ≠ []) :
    (l.dropWhile p).getLast? = l.getLast? := by
  conv => rhs; rw [← List.takeWhile_append_dropWhile (p := p) (l := l)]
  rw [List.getLast?_append]
  cases hd : (l.dropWhile p).getLast? with
  | none => exact absurd (List.getLast?_eq_none_iff.mp hd) h
  | some a => rfl

/-- A key fit to send: not empty, and no blank at either end. -/
def Clean (k : Text) : Prop :=
  k ≠ [] ∧ (∀ a, k.head? = some a → isWs a = false) ∧ (∀ a, k.getLast? = some a → isWs a = false)

theorem present_clean {v : Option Text} {k : Text} (h : present v = some k) : Clean k := by
  cases v with
  | none => simp [present] at h
  | some t =>
    simp only [present] at h
    split at h
    · simp at h
    · rename_i hne
      injection h with h
      subst h
      refine ⟨hne, fun a ha => ?_, fun a ha => ?_⟩
      · have hu : (t.dropWhile isWs).reverse.dropWhile isWs ≠ [] := by
          intro hu; apply hne; unfold trim; rw [hu]; rfl
        unfold trim at ha
        rw [List.head?_reverse, last_dropWhile _ hu, List.getLast?_reverse] at ha
        exact head_dropWhile _ _ ha
      · unfold trim at ha
        rw [List.getLast?_reverse] at ha
        exact head_dropWhile _ _ ha

/-- Blanks around a value change what it reads as not at all. -/
theorem present_pad (pad t pad' : Text) (hp : ∀ c ∈ pad, isWs c = true)
    (hq : ∀ c ∈ pad', isWs c = true) : present (some (pad ++ t ++ pad')) = present (some t) := by
  simp only [present, trim_pad pad t pad' hp hq]

theorem present_blank {t : Text} (h : trim t = []) : present (some t) = present none := by
  simp [present, h]

/-! ### 2. A tier is read only through `present` -/

theorem flag_by_present (w : World) (a b : Option Text) (h : present a = present b) :
    resolve { w with flag := a } = resolve { w with flag := b } := by
  simp only [resolve, chain, behind, command, h]

theorem config_by_present (w : World) (a b : Option Text) (h : present a = present b) :
    resolve { w with config := a } = resolve { w with config := b } := by
  simp only [resolve, chain, behind, command, h]

theorem store_by_present (w : World) (a b : Option Text) (h : present a = present b) :
    resolve { w with store := a } = resolve { w with store := b } := by
  simp only [resolve, chain, behind, command, h]

theorem env_by_present (w : World) (a b : Option Text) (h : present a = present b) :
    resolve { w with env := a } = resolve { w with env := b } := by
  simp only [resolve, chain, behind, command, h]

/-- A blank value is an absent one, at every tier that reads a value: the next tier answers. -/
theorem blank_is_absent (w : World) (t : Text) (h : trim t = []) :
    resolve { w with flag := some t } = resolve { w with flag := none } ∧
    resolve { w with config := some t } = resolve { w with config := none } ∧
    resolve { w with store := some t } = resolve { w with store := none } ∧
    resolve { w with env := some t } = resolve { w with env := none } :=
  have hb := present_blank h
  ⟨flag_by_present w _ _ hb, config_by_present w _ _ hb,
   store_by_present w _ _ hb, env_by_present w _ _ hb⟩

/-- Blanks around a value change nothing, at every tier that reads a value. -/
theorem padding_is_ignored (w : World) (pad t pad' : Text) (hp : ∀ c ∈ pad, isWs c = true)
    (hq : ∀ c ∈ pad', isWs c = true) :
    resolve { w with flag := some (pad ++ t ++ pad') } = resolve { w with flag := some t } ∧
    resolve { w with config := some (pad ++ t ++ pad') } = resolve { w with config := some t } ∧
    resolve { w with store := some (pad ++ t ++ pad') } = resolve { w with store := some t } ∧
    resolve { w with env := some (pad ++ t ++ pad') } = resolve { w with env := some t } :=
  have hb := present_pad pad t pad' hp hq
  ⟨flag_by_present w _ _ hb, config_by_present w _ _ hb,
   store_by_present w _ _ hb, env_by_present w _ _ hb⟩

/-! ### 3. Every key the chain's own tiers answer is clean -/

theorem config_clean {w : World} {k : Text}
    (h : (present w.flag).or (present w.config) = some k) : Clean k := by
  cases hf : present w.flag with
  | some v =>
    rw [hf] at h
    simp only [Option.or, Option.some.injEq] at h
    subst h
    exact present_clean hf
  | none =>
    rw [hf] at h
    simp only [Option.or] at h
    exact present_clean h

theorem command_clean (w : World) (c : Text) (s : Source) (k : Text)
    (h : command w c = .key s k) : s = .config ∧ Clean k := by
  unfold command at h
  split at h
  · simp at h
  · split at h
    · simp at h
    · split at h
      · simp at h
      · rename_i k' hk'
        injection h with hs hk
        subst hs hk
        exact ⟨rfl, present_clean hk'⟩

theorem public_clean : Clean publicKey := by
  refine ⟨by decide, fun a ha => ?_, fun a ha => ?_⟩
  · have e : publicKey.head? = some 'p' := by decide
    rw [e] at ha
    injection ha with ha
    subst ha
    decide
  · have e : publicKey.getLast? = some 'c' := by decide
    rw [e] at ha
    injection ha with ha
    subst ha
    decide

theorem behind_clean (w : World) (s : Source) (k : Text)
    (h : behind present w = .key s k) (hs : s ≠ .hook) : Clean k := by
  unfold behind at h
  split at h
  · rename_i k' hk'
    injection h with _ hk
    subst hk
    exact present_clean hk'
  · split at h
    · injection h with h1 _
      exact absurd h1.symm hs
    · split at h
      · rename_i k' hk'
        injection h with _ hk
        subst hk
        exact present_clean hk'
      · injection h with _ hk
        subst hk
        exact public_clean

/-- Every key the chain's own tiers answer is clean: not empty, no blank at
either end. A hook's key is its cell's own. -/
theorem key_clean (w : World) (s : Source) (k : Text) (h : resolve w = .key s k)
    (hs : s ≠ .hook) : Clean k := by
  unfold resolve chain at h
  split at h
  · rename_i c hc
    split at h
    · injection h with _ hk
      subst hk
      exact config_clean hc
    · exact (command_clean w c s k h).2
  · injection h with _ hk
    subst hk
    exact config_clean (by assumption)
  · exact behind_clean w s k h hs

/-! ### 4. The tiers answer in order, and a command decides -/

theorem store_answers (w : World) (k : Text)
    (h0 : (present w.flag).or (present w.config) = none) (h : present w.store = some k) :
    resolve w = .key .apiKey k := by
  simp only [resolve, chain, h0, behind, h]

theorem hook_answers (w : World) (k : Text)
    (h0 : (present w.flag).or (present w.config) = none) (h1 : present w.store = none)
    (h : w.hook = some k) : resolve w = .key .hook k := by
  simp only [resolve, chain, h0, behind, h1, h]

theorem env_answers (w : World) (k : Text)
    (h0 : (present w.flag).or (present w.config) = none) (h1 : present w.store = none)
    (h2 : w.hook = none) (h : present w.env = some k) : resolve w = .key .env k := by
  simp only [resolve, chain, h0, behind, h1, h2, h]

theorem public_last (w : World)
    (h0 : (present w.flag).or (present w.config) = none) (h1 : present w.store = none)
    (h2 : w.hook = none) (h3 : present w.env = none) : resolve w = .key .keyless publicKey := by
  simp only [resolve, chain, h0, behind, h1, h2, h3]

/-- A command answers the key, or fails the resolution: never a later tier. -/
theorem command_decides (w : World) (c : Text)
    (h0 : (present w.flag).or (present w.config) = some ('!' :: c)) (hp : w.probe = false) :
    resolve w = .fail ∨ ∃ k, resolve w = .key .config k := by
  have e : resolve w = command w c := by
    simp only [resolve, chain, h0, hp, Bool.false_eq_true, ite_false]
  rw [e]
  unfold command
  split
  · exact Or.inl rfl
  · split
    · exact Or.inl rfl
    · split
      · exact Or.inl rfl
      · exact Or.inr ⟨_, rfl⟩

/-- A probe runs no command: the config answers with the command itself. -/
theorem probe_runs_nothing (w : World) (c : Text)
    (h0 : (present w.flag).or (present w.config) = some ('!' :: c)) (hp : w.probe = true) :
    resolve w = .key .config ('!' :: c) := by
  simp only [resolve, chain, h0, hp, ite_true]

/-! ### 5. Decided traces, pinned on the real function by
`the-credential-chain-runs-credentialchain-lean-s-traces` (src/test/engine/turn-test.lisp) -/

theorem trace_blank_store :
    resolve { store := some "   ".toList, env := some "sk-env".toList } = .key .env "sk-env".toList := by
  decide

theorem trace_padded_store :
    resolve { store := some "  sk-auth  ".toList } = .key .apiKey "sk-auth".toList := by
  decide

theorem trace_blank_flag :
    resolve { flag := some "  ".toList, env := some "sk-env".toList } = .key .env "sk-env".toList := by
  decide

theorem trace_padded_flag :
    resolve { flag := some " sk-flag ".toList } = .key .config "sk-flag".toList := by
  decide

theorem trace_blank_flag_config :
    resolve { flag := some "\t".toList, config := some "sk-config".toList } =
      .key .config "sk-config".toList := by
  decide

theorem trace_store_over_env :
    resolve { store := some "sk-auth".toList, env := some "sk-env".toList } =
      .key .apiKey "sk-auth".toList := by
  decide

theorem trace_blank_store_hook :
    resolve { store := some "\n".toList, hook := some "sk-cell".toList } =
      .key .hook "sk-cell".toList := by
  decide

theorem trace_blank_env :
    resolve { env := some " \t".toList } = .key .keyless publicKey := by
  decide

theorem trace_command :
    resolve { flag := some " !print-key ".toList,
              run := fun c => if c = "print-key".toList then some " sk-cmd\n".toList else none } =
      .key .config "sk-cmd".toList := by
  decide

theorem trace_empty_command : resolve { flag := some "!  ".toList } = .fail := by
  decide

theorem trace_silent_command :
    resolve { flag := some "!print-key".toList, run := fun _ => some " \n".toList } = .fail := by
  decide

theorem trace_failed_command : resolve { flag := some "!false".toList } = .fail := by
  decide

theorem trace_probe :
    resolve { flag := some "!print-key".toList, probe := true } =
      .key .config "!print-key".toList := by
  decide

/-- main 75a703a sent auth.json's blank key, over the environment's... -/
theorem main_sends_a_blank_store_key :
    resolveMain { store := some "   ".toList, env := some "sk-env".toList } =
      .key .apiKey "   ".toList := by
  decide

/-- ...and NODECODE_API_KEY padded, as it stood. -/
theorem main_sends_a_padded_flag :
    resolveMain { flag := some " sk-flag ".toList } = .key .config " sk-flag ".toList := by
  decide

end CredentialChain
