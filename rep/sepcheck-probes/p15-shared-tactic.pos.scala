// P15: a SHARED tactic may be seen twice by one call — directly as a using-argument and
// inside the capture set of a codec it was summoned into — without a separation overlap.
// This is the `Json#as` shape (jacinta.Json.scala): the decodable summoned for `as` captures
// the ambient `Tactic[Json.Error]`, and `as` takes the same tactic as a using-parameter. With
// `Emit` classified `caps.ExclusiveCapability` the two uses overlap (p15-exclusive-tactic.neg);
// with `caps.SharedCapability` the tactic stays TRACKED (the `^{tactic}` on `intCodec`'s
// result is still demanded) but aliasing it is not a separation failure.
//FLAGS: -language:experimental.separationChecking
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.SharedCapability:
  def abort(error: E): Nothing

trait Decodable[T]:
  def decoded(input: String): T

def intCodec(using tactic: Tactic[String]): Decodable[Int]^{tactic} = new Decodable[Int]:
  def decoded(input: String): Int =
    if input.isEmpty then tactic.abort("empty") else input.length

def as[T](input: String)(using decodable: Decodable[T]^)(using Tactic[String]): T =
  decodable.decoded(input)

def use(using Tactic[String]): Int = as[Int]("x")(using intCodec)
