// The naive combination: a Shared tactic whose ambient strategy is also Unscoped. The two
// classifiers are unrelated, which the pinned toolchain rejects ("inherits two unrelated
// classifier traits"; rep/sepcheck-probes/p16) — RED, correctly. It turns GREEN only when
// `Throwing` extends the fork's `caps.SharedUnscoped` instead.
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E], caps.Unscoped:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

infix type raises[S, E] = Tactic[E]^ ?=> S

given throwing: [E] => (Throwing[E]^) = Throwing()

def inner(): Int raises String = 1

def helper(): Int = inner()

def keep(): () -> Unit =
  val tactic = Throwing[String]()
  () => tactic.abort("retained by a pure closure")
