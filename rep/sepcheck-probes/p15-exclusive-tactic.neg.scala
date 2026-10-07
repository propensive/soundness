// P15 (negative twin): the same shape as p15-shared-tactic.pos with the tactic classified
// `caps.ExclusiveCapability` — contingency's `Emit` today. The codec's capture of the tactic
// and the direct using-argument are two uses of one exclusive capability in one call, which
// separation checking rejects. Every `caps.unsafe.unsafeAssumeSeparate(json.as[…])` in the
// tree (sibylline, tarantula, orthodoxy, embarcadero, …) is this error.
//FLAGS: -language:experimental.separationChecking
//EXPECT: Separation failure
import language.experimental.captureChecking
import scala.caps

trait Tactic[E] extends caps.ExclusiveCapability:
  def abort(error: E): Nothing

trait Decodable[T]:
  def decoded(input: String): T

def intCodec(using tactic: Tactic[String]): Decodable[Int]^{tactic} = new Decodable[Int]:
  def decoded(input: String): Int =
    if input.isEmpty then tactic.abort("empty") else input.length

def as[T](input: String)(using decodable: Decodable[T]^)(using Tactic[String]): T =
  decodable.decoded(input)

def use(using Tactic[String]): Int = as[Int]("x")(using intCodec)
