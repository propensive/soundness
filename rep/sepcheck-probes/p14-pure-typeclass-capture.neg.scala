// P14 (neg twin): a `Typeclass.Pure` instance that captures a capability is
// rejected at its definition — the enforcement that turns the codec-thunk
// convention into a checked fact.
//EXPECT: (allowed capture set|Pure|pure)
import language.experimental.captureChecking

trait Tactic[E] extends caps.ExclusiveCapability:
  def abort(error: E): Nothing

trait Typeclass:
  type Self

object Typeclass:
  trait Pure extends Typeclass, caps.Pure

trait Decodable extends Typeclass.Pure:
  type Form
  def decoded(value: Form): Self

def make(using tactic: Tactic[String]): Decodable { type Self = Int; type Form = String } =
  new Decodable:
    type Self = Int
    type Form = String
    def decoded(value: String): Int =
      if value.isEmpty then tactic.abort("empty") else value.length
