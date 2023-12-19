package rudiments

import vacuity.*

import scala.util as su

import java.util as ju
import java.security as js

case class Seed(value: Bytes)

trait RandomNumberGenerator:
  def make(): su.Random

package randomNumberGenerators:
  given unseeded: RandomNumberGenerator = () => su.Random(java.util.Random())
  given secureUnseeded: RandomNumberGenerator = () => su.Random(js.SecureRandom())
  given stronglySecure: RandomNumberGenerator = () => su.Random(js.SecureRandom.getInstanceStrong().nn)
  
  given seeded(using seed: Seed): RandomNumberGenerator = () =>
    su.Random(ju.Random(seed.value.foldLeft(0L)(_ << 8 | _ & 0xff)))
  
  given secureSeeded(using seed: Seed): RandomNumberGenerator = () =>
    su.Random(js.SecureRandom(seed.value.mutable(using Unsafe)))

object Randomizable:
  given int: Randomizable[Int] = _.toInt
  given long: Randomizable[Long] = identity(_)
  given char: Randomizable[Char] = _.toChar
  given boolean: Randomizable[Boolean] = _ < 0L

// Note that `gen` is side-effecting, and is therefore not threadsafe
trait Randomizable[+ValueType]:
  def from(gen: => Long): ValueType

object Random:
  lazy val generalPurpose: Random =
    import randomNumberGenerators.unseeded
    Random()

  def apply()(using generator: RandomNumberGenerator): Random = new Random(generator.make())

class Random(private val rng: su.Random):
  def apply[ValueType]()(using randomizable: Randomizable[ValueType]): ValueType =
    randomizable.from(rng.nextLong())

  transparent inline def shuffle[ElementType](seq: Seq[ElementType]): Seq[ElementType] = rng.shuffle(seq)

def random[ValueType: Randomizable](): ValueType = Random.generalPurpose[ValueType]()
