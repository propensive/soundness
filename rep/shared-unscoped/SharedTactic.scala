// ── shared-unscoped ────────────────────────────────────────────────────────────────────────────
// contingency's `Emit`/`Tactic` is an ExclusiveCapability, and its ambient strategies
// (`ThrowTactic`, `UncheckedTactic`, `FatalTactic`) are additionally `caps.Unscoped`: that is what
// let a tactic minted by a polymorphic given at the USE site flow into the `raises` alias's
// existential on the 3.8.4-era toolchain (rep/DECISIONS.md, "capturing-raises SOLVED at
// source"). Making the tactic SHARED — so that one tactic aliased by a codec and a
// using-argument is not a separation overlap (rep/sepcheck-probes/p15) — cannot keep Unscoped:
// the two classifiers are unrelated (p16).
//
// THIS FILE is the Shared-only variant, the design P1 of the capabilities roadmap wants. On
// 3.9.1-dev-p17 it is GREEN: a Shared capability is admitted by the result-capability rule
// (cc/Capability.scala, the `derivesFromShared` case), so this minimal shape no longer needs
// `Unscoped` for level visibility at all. `ExclusiveUnscoped.scala` is today's design (GREEN);
// `SharedUnscoped.scala` is the naive combination (RED: unrelated classifiers).
//
// WHAT WE WANT: contingency's `Emit` to become `caps.SharedCapability` with the ambient
// strategies dropping `Unscoped`. This repro says the checker allows it; what it cannot say is
// whether the full `capturing-raises` shape (rep/capturing-raises, Soundness-backed) agrees —
// that is the first gate of the contingency flip. Only if THAT is red does the fork need a
// classifier that is shared AND level-exempt (`caps.SharedUnscoped`: `isUnscopedClassifier` in
// cc/Capability, SepCheck, CheckCaptures), which would turn SharedUnscoped.scala GREEN.
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E]:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

infix type raises[S, E] = Tactic[E]^ ?=> S

given throwing: [E] => (Throwing[E]^) = Throwing()

def inner(): Int raises String = 1

def helper(): Int = inner()
