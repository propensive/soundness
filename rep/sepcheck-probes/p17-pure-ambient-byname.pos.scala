// P17 (positive, mainline): the same by-name codec thunk with a PURE ambient strategy, the shape
// `strategies.throwUnsafely` takes from #2328. An unscoped throw retains no return point, so the
// ambient tactic is sealed pure once (as upstream's `unsafeExceptions.canThrowAny` is, with
// `caps.unsafe.unsafeErasedValue`), and a pure value needs no level: the thunk is satisfied with
// no fork classifier at all. The tactic is still a `caps.Control` capability class, so a scoped
// instance (`ThrowTactic()` in `unsafely`) stays tracked.
//FLAGS: -language:experimental.separationChecking
import language.experimental.captureChecking
import scala.caps

trait Tactic[-E] extends caps.Control:
  def abort(error: E): Nothing

class Throwing[-E] extends Tactic[E]:
  def abort(error: E): Nothing = throw new RuntimeException(error.toString)

object Throwing:
  val ambient: Throwing[Any]^{} = caps.unsafe.unsafeAssumePure(Throwing())

given throwing: (Throwing[Any]^{}) = Throwing.ambient

trait Decodable[T]:
  def decoded(input: String): T

given intCodec: (tactic: Tactic[String]) => (Decodable[Int]^{tactic}) = new Decodable[Int]:
  def decoded(input: String): Int =
    if input.isEmpty then tactic.abort("empty") else input.length

given optionCodec: [T] => (inner: => Decodable[T]^) => Decodable[Option[T]] =
  caps.unsafe.unsafeAssumePure:
    new Decodable[Option[T]]:
      def decoded(input: String): Option[T] =
        if input == "null" then None else Some(inner.decoded(input))

def use(): Option[Int] = summon[Decodable[Option[Int]]].decoded("abc")
