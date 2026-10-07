// P17 (fork): the SHARED strategy classified `caps.SharedUnscoped` — the fork classifier
// that is shared AND level-exempt (proscala 3.9.1-dev-p18, patch `sharedunscoped`).
// P17: a polymorphic ambient SHARED strategy summoned inside a by-name codec thunk. This is the
// shape `import strategies.throwUnsafely` + a derived codec with an `Optional`/`List` field
// takes once `Emit` is shared (safety-8): jacinta's `optional` given takes the element codec
// by-name, so given resolution synthesises `() => intCodec(using throwing[String])`, and the
// tactic minted INSIDE that thunk must flow into the by-name parameter's capture root. With the
// strategy `caps.Unscoped` (the Exclusive design) the level check exempts it; a Shared strategy
// cannot be Unscoped (p16), and the pinned 3.9 release rejects this with "capability `any`
// cannot flow into capture set {any²} … not visible from any² in value …". Expected GREEN
// is what safety-8 needs from the compiler (a 3.10 comparison or the fork's `SharedUnscoped`).
//FLAGS: -language:experimental.separationChecking
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E], caps.SharedUnscoped:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

given throwing: [E] => (Throwing[E]^) = Throwing()

trait Decodable[T]:
  def decoded(input: String): T

given intCodec: (tactic: Tactic[String]) => (Decodable[Int]^{tactic}) = new Decodable[Int]:
  def decoded(input: String): Int =
    if input.isEmpty then tactic.abort("empty") else input.length

// The by-name element codec, as jacinta's `optional`/`array`/`map` givens take it.
// Sealed pure as jacinta's `optional` is (a by-name parameter cannot be named in a capture
// set — safety-10); the level check on the synthesised thunk happens before the seal.
given optionCodec: [T] => (inner: => Decodable[T]^) => Decodable[Option[T]] =
  caps.unsafe.unsafeAssumePure:
    new Decodable[Option[T]]:
      def decoded(input: String): Option[T] =
        if input == "null" then None else Some(inner.decoded(input))

def use(): Option[Int] = summon[Decodable[Option[Int]]].decoded("abc")
