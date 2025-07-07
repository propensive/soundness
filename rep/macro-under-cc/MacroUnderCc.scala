package quantitative

import soundness.*

import language.strictEquality
import language.experimental.into

// ── macro breakage under CC ──────────────────────────────────────────────────────────────────────
// Enabling CC changes the shape of the operand trees a macro sees. quantitative's `checkable` macro
// (driven by `===`) hits `scala.MatchError: None` on CC-annotated trees:
//   "Exception occurred while executing macro expansion … quantitative.protointernal.checkable".
// WHAT WE WANT: the units-checking macro should reduce as before; CC annotations on operand trees
// should not derail its pattern matches. Compile with `rep/compile.sh macro-under-cc` (dotc).
// WHERE (2 suites): quantitative (`checkable` MatchError), cataclysm (`Attribution` literal extract).
object MacroUnderCc:
  val equal: Boolean = Mile === 1760*Yard
