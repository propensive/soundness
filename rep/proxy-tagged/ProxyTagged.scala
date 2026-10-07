//> using scala 3.8.4
//> using options -language:experimental.captureChecking -Ycc-new -experimental

// ── proxy-tagged — SELF-CONTAINED (no Soundness dependency) ───────────────────────────────────
// The "inliner-proxy / aka-Tagged" class (rep/DECISIONS.md, compiler fix #5 `castbox`): Setup
// boxes the type arguments of an undealiasable opaque application via `normalizeCaptures`, but
// skipped the `asInstanceOf`/type-test special case, so the cast inside the opaque companion's
// `apply` produced an UNBOXED spelling that could not compare with the boxed declaration-side
// one ("is boxed but ... is not"; with a singleton argument, the
// `Tagged[(any$proxy : ...)^{any$proxy}]` mismatch). FIXED in the fork (`castbox`); RED on stock.
// Compile with:  rep/compile.sh proxy-tagged      (or --stock for the upstream status)

import language.experimental.captureChecking
import scala.caps

object internal:
  opaque type Tagged[+value, tag] = value

  object Tagged:
    inline def apply[tag](value: Any): Tagged[value.type, tag] = value

  extension [value, tag](tagged: Tagged[value, tag]) inline def apply(): value =
    tagged.asInstanceOf[value]

infix type aka[subject, label <: String] = internal.Tagged[subject, label]

extension (any: Any)
  inline def aka[label <: String]: any.type aka label = internal.Tagged[label](any)

trait Tactic extends caps.ExclusiveCapability

trait TC[T]:
  def go(t: T): Int

given tc: (t: Tactic) => ((TC[Int])^{t}) = i => 42

inline def use[R](inline lambda: (TC[Int] aka "contextual") ?=> R): R =
  val inst = compiletime.summonInline[TC[Int]^]
  lambda(using inst.aka["contextual"])

def caller(using t: Tactic): Int =
  use { ctx ?=> ctx().go(3) }
