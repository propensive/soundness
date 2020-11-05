/*

    Probably, version 0.4.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably

import magnolia._

import language.experimental.macros

case class Seed(value: Long) {
  def apply(): Long = value
  def stream(count: Int): Stream[Seed] = {
    val rnd = new java.util.Random(value)
    Stream.continually(Seed(rnd.nextLong)).take(count)
  }
}

object Arbitrary {
  type Typeclass[T] = Arbitrary[T]

  def combine[T](ctx: CaseClass[Arbitrary, T]): Arbitrary[T] = (seed, n) => ctx.rawConstruct {
    ctx.parameters.zip(spread(seed, n, ctx.parameters.size)).zip(seed.stream(ctx.parameters.size)).map {
      case ((param, i), s) => param.typeclass(s, i)
    } }

  val interestingInts = Vector(0, 1, -1, 2, -2, 42, Int.MaxValue, Int.MinValue, Int.MaxValue - 1,
      Int.MinValue + 1)

  implicit val int: Arbitrary[Int] =
    (seed, n) => interestingInts.lift(n).getOrElse(seed.stream(n).last.value.toInt)

  val interestingStrings = Vector("", "a", "z", "\n", "0", "_", "\"", "\'", " ", "abcdefghijklmnopqrstuvwxyz")
  implicit def string: Arbitrary[String] = (seed, n) => interestingStrings.lift(n).getOrElse {
    val chars = seed.stream(n).last.stream(10).map(_()).map(_.toByte).filter { c => c > 31 && c < 128 }
    new String(chars.to[Array], "UTF-8")
  }

  implicit def gen[T]: Arbitrary[T] = macro Magnolia.gen[T]

  private def spread(seed: Seed, total: Int, count: Int): List[Int] = {
    val sample = seed.stream(count).map(_.value.toDouble).map(math.abs(_)).to[List]
    sample.tails.foldLeft(List[Int]()) { case (acc, tail) => tail.headOption.fold(acc) { v =>
      (((v/(tail.sum))*(total - acc.sum)) + 0.5).toInt :: acc
    } }
  }
}

trait Arbitrary[T] { def apply(seed: Seed, n: Int): T }

object Generate {
  def stream[T](implicit arbitrary: Arbitrary[T], seed: Seed = Seed(0L)): Stream[T] =
    Stream.from(0).map(arbitrary(seed, _))
}
