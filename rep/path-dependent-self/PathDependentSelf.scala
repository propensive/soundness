package plutocrat

import soundness.*
import currencies.Eur

// ── path-dependent opaque `Self` ─────────────────────────────────────────────────────────────────
// plutocrat's `Money` is an opaque type whose operations live on a path-dependent `Currency#Self`.
// Under CC the opaque seen via the concrete path `Eur.Self` is not judged identical to the same
// opaque seen via the abstract `internal.Money` self-path:
//   "Found: plutocrat.Money in soundness.currencies.Eur.Self / Required: internal$_this.Money".
// WHAT WE WANT: `Eur(1.01) > Eur(2.10)` should type-check — the two `Money` views are the same type.
// Compile with `rep/compile.sh path-dependent-self` (dotc). WHERE (1 suite): plutocrat.
object PathDependentSelf:
  val cheaper: Boolean = Eur(1.01) > Eur(2.10)
