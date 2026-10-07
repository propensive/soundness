// P17 (negative): a polymorphic ambient SHARED strategy summoned inside a by-name codec thunk.
// This is the shape `import strategies.throwUnsafely` + a derived codec with an `Optional`/
// `List` field takes once `Emit` is shared (safety-8): jacinta's `optional` given takes the
// element codec by-name, so given resolution synthesises `() => intCodec(using throwing[String])`,
// and the tactic minted INSIDE that thunk must flow into the by-name parameter's capture root.
// A plain `SharedCapability` strategy is level-checked and rejected (on 3.9 and 3.10 alike);
// the `caps.Unscoped` control passes but cannot be combined with Shared (p16); the
// `caps.SharedUnscoped` twin passes from proscala 3.9.1-dev-p18 on. This file is the
// calibration: it must keep FAILING, or the classifier has become unnecessary.
//FLAGS: -language:experimental.separationChecking
//EXPECT: cannot flow into capture set
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

class Throwing[E] extends Tactic[E]:
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
