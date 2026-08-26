                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package capricious

import soundness.*

import randomization.seededRandomization
import strategies.throwUnsafely
given Seed = Seed(1L)

case class Point(x: Int, y: Int)

enum Colour:
  case Red, Green, Blue

given pointRandomizable: Point is Randomizable = Randomizable.derived
given colourRandomizable: Colour is Randomizable = Randomizable.derived

object Tests extends Suite(m"Capricious Tests"):
  def run(): Unit =
    suite(m"Distributions"):
      test(m"Normal distribution mean"):
        stochastic:
          given Distribution = Gaussian(0.0, 1.0)
          List.fill(10000)(arbitrary[Double]())

      . assert(_.mean.lay(false)(_ === 0.0 +/- 0.02))

      test(m"Normal distribution standard deviation"):
        stochastic:
          given Distribution = Gaussian(0.0, 2.0)
          List.fill(10000)(arbitrary[Double]())

      . assert(_.std.lay(false)(_ === 2.0 +/- 0.05))

      test(m"Gamma distribution mean"):
        stochastic:
          given distribution: Gamma = Gamma.approximate(100, 10)
          List.fill(100000)(arbitrary[Double]())

      . assert(_.mean.lay(false)(_ === 100.0 +/- 0.1))

      test(m"Gamma distribution standard deviation"):
        stochastic:
          given distribution: Gamma = Gamma.approximate(100, 10)
          List.fill(100000)(arbitrary[Double]())

      . assert(_.std.lay(false)(_ === 10.0 +/- 0.1))

    suite(m"Reproducibility"):
      // The point of a seeded generator: the same seed must replay the same draws. Nothing in
      // the suite checked this, which is the one property everything else depends on.
      test(m"the same seed replays the same sequence"):
        def draw(seed: Seed): List[Long] = seed.stochastic(List.fill(16)(arbitrary[Long]()))
        draw(Seed(42L)) == draw(Seed(42L))
      . assert(_ == true)

      test(m"different seeds give different sequences"):
        def draw(seed: Seed): List[Long] = seed.stochastic(List.fill(16)(arbitrary[Long]()))
        draw(Seed(42L)) == draw(Seed(43L))
      . assert(_ == false)

      test(m"the ambient seed makes separate stochastic blocks agree"):
        def draw: List[Long] = stochastic(List.fill(16)(arbitrary[Long]()))
        draw == draw
      . assert(_ == true)

      test(m"successive draws within one block differ"):
        stochastic(List.fill(16)(arbitrary[Long]())).distinct.size
      . assert(_ == 16)

    suite(m"Seed"):
      test(m"a seed from a Long recovers that Long"):
        Seed(42L).long
      . assert(_ == 42L)

      test(m"a seed from a Long carries sixty-four bits of entropy"):
        Seed(42L).entropy
      . assert(_ == 64)

      // Compared through `long`, not by `==`: a `Seed` wraps `Data`, so case-class equality is
      // array identity and never holds between two separately-drawn seeds.
      test(m"a seed is itself randomizable"):
        Seed(7L).stochastic(arbitrary[Seed]()).long
      . assert(_ == Seed(7L).stochastic(arbitrary[Seed]()).long)

      test(m"a randomly-drawn seed is the drawn long"):
        Seed(7L).stochastic(arbitrary[Seed]()).long
      . assert(_ == Seed(7L).stochastic(arbitrary[Long]()))

    suite(m"Primitive instances"):
      test(m"booleans take both values"):
        stochastic(List.fill(64)(arbitrary[Boolean]())).distinct.size
      . assert(_ == 2)

      test(m"a byte is drawn from the low bits of a long"):
        Seed(9L).stochastic((arbitrary[Long](), arbitrary[Long]()))
      . assert: (first, second) =>
          Seed(9L).stochastic((arbitrary[Byte](), arbitrary[Byte]()))
          == (first.toByte, second.toByte)

      test(m"an int is drawn from the low bits of a long"):
        Seed(9L).stochastic(arbitrary[Int]())
      . assert(_ == Seed(9L).stochastic(arbitrary[Long]()).toInt)

      test(m"a short is drawn from the low bits of a long"):
        Seed(9L).stochastic(arbitrary[Short]())
      . assert(_ == Seed(9L).stochastic(arbitrary[Long]()).toShort)

      test(m"a char is drawn from the low bits of a long"):
        Seed(9L).stochastic(arbitrary[Char]())
      . assert(_ == Seed(9L).stochastic(arbitrary[Long]()).toChar)

      test(m"a boolean is the sign of a long"):
        Seed(9L).stochastic(arbitrary[Boolean]())
      . assert(_ == (Seed(9L).stochastic(arbitrary[Long]()) < 0L))

    suite(m"Random capability"):
      test(m"the unit interval is bounded below"):
        stochastic(List.fill(256)(summon[Random].unitInterval())).forall(_ >= 0.0)
      . assert(_ == true)

      test(m"the unit interval is bounded above"):
        stochastic(List.fill(256)(summon[Random].unitInterval())).forall(_ < 1.0)
      . assert(_ == true)

      test(m"a toss comes up both ways"):
        stochastic(List.fill(64)(toss())).distinct.size
      . assert(_ == 2)

      test(m"the global generator draws without a seed in scope"):
        Random.global.long()
      . assert()

      test(m"shuffling keeps every element"):
        stochastic(summon[Random].shuffle(List(1, 2, 3, 4, 5, 6, 7, 8))).stdlib.sorted
      . assert(_ == scala.List(1, 2, 3, 4, 5, 6, 7, 8))

      test(m"shuffling is reproducible under a seed"):
        def shuffled(seed: Seed): List[Int] =
          seed.stochastic(summon[Random].shuffle(List(1, 2, 3, 4, 5, 6, 7, 8)))

        shuffled(Seed(3L)) == shuffled(Seed(3L))
      . assert(_ == true)

      test(m"shuffling does reorder"):
        val ordered = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

        def shuffled(seed: Seed): List[Int] =
          seed.stochastic(summon[Random].shuffle(ordered))

        shuffled(Seed(3L)) == ordered
      . assert(_ == false)

    suite(m"Uniform distribution"):
      test(m"draws lie within the stated range"):
        stochastic:
          given Distribution = UniformDistribution(-1.0, 1.0)
          List.fill(4096)(arbitrary[Double]())

        . forall { value => value >= -1.0 && value <= 1.0 }
      . assert(_ == true)

      test(m"the mean is the midpoint of the range"):
        stochastic:
          given Distribution = UniformDistribution(4.0, 6.0)
          List.fill(65536)(arbitrary[Double]())

        . mean.lay(false)(_ === 5.0 +/- 0.02)
      . assert(_ == true)

    suite(m"Derivation"):
      test(m"a product type is randomizable"):
        Seed(5L).stochastic(arbitrary[Point]()) == Seed(5L).stochastic(arbitrary[Point]())
      . assert(_ == true)

      test(m"a product's fields are not all equal"):
        stochastic(List.fill(32)(arbitrary[Point]())).exists { p => p.x != p.y }
      . assert(_ == true)

      test(m"a sum type is randomizable"):
        Seed(5L).stochastic(arbitrary[Colour]()) == Seed(5L).stochastic(arbitrary[Colour]())
      . assert(_ == true)

      test(m"a sum type reaches every variant"):
        stochastic(List.fill(256)(arbitrary[Colour]())).distinct.size
      . assert(_ == 3)

