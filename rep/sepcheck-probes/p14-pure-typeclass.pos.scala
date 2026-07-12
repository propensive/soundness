// P14: the checked-pure typeclass subset. `Typeclass.Pure` instances may hold
// only pure state; a given constructed under a resolution-scoped capability
// seals the CAPABILITY (one uniform, documented pattern) and then constructs
// purely — the instance itself is compiler-verified capability-free, unlike
// the old whole-instance seal, which was an unchecked assertion.
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

// (a) a plain pure instance
class TextDecoder extends Decodable:
  type Self = Int
  type Form = String
  def decoded(value: String): Int = value.length

// (c) the untracked-field pattern: a capturing codec given names exactly the
// capability it retains, with an annotation at the field — the rest of the
// instance stays compiler-checked pure.
def make(using tactic: Tactic[String]): Decodable { type Self = Int; type Form = String } =
  new Decodable:
    type Self = Int
    type Form = String

    @caps.unsafe.untrackedCaptures
    private val tactic0: Tactic[String] = tactic

    def decoded(value: String): Int =
      if value.isEmpty then tactic0.abort("empty") else value.length
