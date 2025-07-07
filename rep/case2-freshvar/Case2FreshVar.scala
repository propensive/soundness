//> using scala 3.8.4
//> using options -language:experimental.captureChecking -Ycc-new -language:experimental.modularity -source future

// ── case-2 "fresh-var" — SELF-CONTAINED (no Soundness dependency) ─────────────────────────────────
// Compile with:  scala-cli compile rep/case2-freshvar
//
// A carrier type with a TUPLE type-member, materialised by an INLINE given at a refinement fixing
// that member, gains a fresh non-unifying existential capture var `^'s1` on the Tuple. The derived
// `Showable`'s `text` method then has parameter type `Quanta{ type Form = (Pounds,Stones)^'s1 }`,
// which no longer overrides the trait's pure `text(value: Self)`:
//   "error overriding method text in trait Showable … has incompatible type".
//
// Reduced from abacist's `(Quanta(2): Weight).show`, where `Weight = Quanta[…] in (Pounds, Stones)`.
//
// WHAT WE WANT: the Tuple of pure elements should stay pure; no fresh capture var on `Form`.
// WHERE IN SOUNDNESS (≈23 suites — largest class): abacist, serpentine, mosquito, jacinta, xylophone,
// ypsiloid, stratiform, caesura, vicarious, wisteria, obligatory, panopticon, parasite, galilei, …

import language.experimental.captureChecking
import scala.caps

class Pounds extends caps.Pure
class Stones extends caps.Pure

trait Showable extends caps.Pure:
  type Self
  def text(value: Self): String

trait Quanta extends caps.Pure:
  type Form

inline given showable: [Q <: Quanta] => Q is Showable = new Showable:
  type Self = Q
  def text(value: Q): String = "x"

extension [L](left: L)
  inline def show(using value: L is Showable): String = value.text(left)

val q: Quanta { type Form = (Pounds, Stones) } = new Quanta { type Form = (Pounds, Stones) }
val s: String = q.show
