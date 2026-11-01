/-
proof-axioms.lean --- what a proof under proofs/ rests on.

SPDX-License-Identifier: MIT

  lean --run src/tools/proof-axioms.lean proofs/NAME.lean

Elaborates the file in this process, then lists the axioms every theorem it
declares depends on, through the whole chain of definitions and lemmas it
uses. Exit 0 when each theorem rests only on the three axioms of classical
mathematics (propext, Classical.choice, Quot.sound), 1 otherwise, naming the
theorem and the axiom. So a `sorry` (sorryAx), an `axiom` declared in the
file, or a `native_decide` (admitted through an axiom Lean adds, which
trusts the compiler rather than the kernel) fails the check even where
`lean -DwarningAsError=true` exits 0.
-/
import Lean
open Lean

/-- An environment to read, for `collectAxioms`. -/
instance : MonadEnv (StateM Environment) where
  getEnv := get
  modifyEnv := modify

/-- The axioms a proof may rest on. -/
def allowed : List Name := [``propext, ``Classical.choice, ``Quot.sound]

-- unsafe: enableInitializersExecution, which elaborating a file needs.
unsafe def main (args : List String) : IO UInt32 := do
  let [file] := args | do IO.eprintln "usage: lean --run proof-axioms.lean FILE.lean"; return 2
  initSearchPath (← findSysroot)
  enableInitializersExecution
  let input ← IO.FS.readFile file
  let modName := (System.FilePath.mk file).fileStem.getD "Proof" |>.toName
  let some env ← Elab.runFrontend input {} file modName
    | do IO.eprintln s!"{file}: did not elaborate"; return 1
  let mut theorems := 0
  let mut bad := 0
  -- map₂ holds the constants this file added, the imports sit in map₁.
  -- Every theorem among them is audited, the ones Lean generates (equation
  -- and injectivity lemmas) too; only the ones written in the file, which
  -- carry a source range, are listed and counted.
  let sorted := env.constants.map₂.toList.toArray.qsort (fun a b => a.1.lt b.1)
  for (name, info) in sorted do
    unless info matches .thmInfo _ do continue
    let written := (declRangeExt.find? (level := .server) env name).isSome
    let axioms : Array Name := Id.run ((collectAxioms name : StateM Environment (Array Name)).run' env)
    let extra := axioms.filter (fun a => !allowed.contains a)
    if written then theorems := theorems + 1
    if !extra.isEmpty then
      bad := bad + 1
      IO.println s!"BAD  {name} rests on {", ".intercalate (extra.toList.map toString)}"
    else if written then
      IO.println s!"ok   {name} [{", ".intercalate (axioms.toList.map toString)}]"
  if theorems == 0 then
    IO.eprintln s!"{file}: declares no theorem"
    return 1
  IO.println s!"{file}: {theorems} theorems, {bad} resting on an axiom beyond {", ".intercalate (allowed.map toString)}"
  return (if bad == 0 then 0 else 1)
