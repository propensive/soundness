// Control: today's contingency design. The tactic is Exclusive and the ambient strategy is
// Unscoped, so the use-site mint is level-exempt and `helper` compiles (GREEN) — at the cost
// of every aliasing of one tactic being a separation overlap (rep/sepcheck-probes/p15).
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.ExclusiveCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E], caps.Unscoped:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

infix type raises[S, E] = Tactic[E]^ ?=> S

given throwing: [E] => (Throwing[E]^) = Throwing()

def inner(): Int raises String = 1

def helper(): Int = inner()
