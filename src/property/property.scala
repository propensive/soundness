/*
    Probably, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import wisteria.*

case class Seed(value: Long):
  def apply(): Long = value
  
  def stream(count: Int): LazyList[Seed] =
    val rnd = java.util.Random(value)
    LazyList.continually(Seed(rnd.nextLong)).take(count)

object Arbitrary extends Derivation[Arbitrary]:
  def join[T](ctx: CaseClass[Arbitrary, T]): Arbitrary[T] = (seed, n) => ctx.rawConstruct {
    ctx.params.zip(spread(seed, n, ctx.params.size)).zip(seed.stream(ctx.params.size)).map {
      case ((param, i), s) => param.typeclass(s, i)
    } }

  def split[T](ctx: SealedTrait[Arbitrary, T]): Arbitrary[T] = (seed, n) =>
    ctx.subtypes.zip(spread(seed, n, ctx.subtypes.size)).zip(seed.stream(ctx.subtypes.size)).map {
      case ((subtype, i), s) => subtype.typeclass(s, i)
    } (n%ctx.subtypes.size)

  val seedStrings = Vector("", "a", "z", "\n", "0", "_", "\"", "\'", " ",
      "abcdefghijklmnopqrstuvwxyz")
  
  private val seedInts = Vector(0, 1, -1, 2, -2, 42, Int.MaxValue, Int.MinValue, Int.MaxValue - 1,
      Int.MinValue + 1)

  private val seedLongs = Vector[Long](0, 1, -1, 2, -2, 42, Long.MaxValue, Long.MinValue,
      Long.MaxValue - 1, Long.MinValue + 1)
  
  private val seedBytes = Vector[Byte](0, 1, -1, 2, -2, 42, Byte.MaxValue, Byte.MinValue,
      Byte.MaxValue - 1, Byte.MinValue + 1)
  
  private val seedShorts = Vector[Short](0, 1, -1, 2, -2, 42, Short.MaxValue, Short.MinValue,
      Short.MaxValue - 1, Short.MinValue + 1)
  
  given Arbitrary[Int] = (seed, n) => seedInts.lift(n).getOrElse(seed.stream(n).last.value.toInt)
  given Arbitrary[Long] = (seed, n) => seedLongs.lift(n).getOrElse(seed.stream(n).last.value)
  given Arbitrary[Byte] = (seed, n) => seedBytes.lift(n).getOrElse(seed.stream(n).last.value.toByte)
  
  given Arbitrary[Short] = (seed, n) =>
    seedShorts.lift(n).getOrElse(seed.stream(n).last.value.toShort)

  given Arbitrary[String] = (seed, n) => seedStrings.lift(n).getOrElse {
    val chars =
      seed.stream(n).last.stream(10).map(_()).map(_.toByte).filter { c => c > 31 && c < 128 }
    
    chars.string
  }

  private def spread(seed: Seed, total: Int, count: Int): List[Int] =
    val sample = seed.stream(count).map(_.value.toDouble).map(math.abs(_)).to(List)

    sample.tails.foldLeft(List[Int]()) { (acc, tail) => tail.headOption.fold(acc) { v =>
      (((v/(tail.sum))*(total - acc.sum)) + 0.5).toInt :: acc
    } }

trait Arbitrary[T]:
  def apply(seed: Seed, n: Int): T

object Generate:
  def stream[T: Arbitrary](using seed: Seed = Seed(0L)): LazyList[T] =
    LazyList.from(0).map(summon[Arbitrary[T]](seed, _))
