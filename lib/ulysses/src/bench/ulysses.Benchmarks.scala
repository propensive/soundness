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
package ulysses

import java.nio.charset.StandardCharsets

import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import cardinality.*
import contingency.*
import corpuscular.*, strategies.throwUnsafely
import fulminate.*
import gastronomy.*, providers.soundnessProvider, cryptoPermits.permitNonCryptographicHashes
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import superlunary.embeddings.automaticEmbedding
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

import Blake3.hash

enum Engine:
  case UlyssesBlake3, UlyssesCrc32, Guava, Alexandrnikitin, CommonsCollections

object Benchmarks extends Suite(m"Ulysses Bloom filter benchmarks"):
  given decimalizer: Decimalizer     = Decimalizer(2)
  given device:      BenchmarkDevice = LocalhostDevice

  // Every filter is sized for the same (n, p), so the engines are compared at equal nominal
  // false-positive rates; whether each actually achieves it is checked in `run()`.
  val errorRate: 0.0 ~ 1.0 = 0.01
  val filled: Int = 100_000

  // Queries cycle through a ring of keys rather than repeating one, so the JIT cannot specialise
  // on a single key and the memory access pattern is the realistic one.
  val ring: Int = 1024
  private var cursor: Int = 0

  private def advance(): Int =
    cursor = (cursor + 1) & (ring - 1)
    cursor

  val present: scala.IArray[Text] = scala.IArray.tabulate(filled) { index => ("key-"+index).tt }
  val absent: scala.IArray[Text] = scala.IArray.tabulate(ring) { index => ("absent-"+index).tt }

  // The rivals take `String`s or UTF-8 bytes; conversion happens once, outside the timing.
  val presentStrings: scala.IArray[String] = present.map(_.s)
  val absentStrings: scala.IArray[String] = absent.map(_.s)

  val presentBytes: scala.IArray[scala.Array[Byte]] =
    presentStrings.map(_.getBytes(StandardCharsets.UTF_8).nn)

  val absentBytes: scala.IArray[scala.Array[Byte]] =
    absentStrings.map(_.getBytes(StandardCharsets.UTF_8).nn)

  def keys(size: Int): List[Text] = List.tabulate(size)(present(_))

  // Ulysses, built in one pass with `++`, under the default (BLAKE3) and a 32-bit checksum hash.
  def buildUlyssesBlake3(size: Int): BloomFilter[Text, Blake3] =
    BloomFilter[Text](size, errorRate) ++ keys(size).stdlib

  def buildUlyssesCrc32(size: Int): BloomFilter[Text, Crc32] =
    BloomFilter[Text](size, errorRate)[Crc32] ++ keys(size).stdlib

  // Element by element, which an immutable filter pays for with a copy per `+`.
  def buildUlyssesIncrementally(size: Int): BloomFilter[Text, Blake3] =
    keys(size).fuse(BloomFilter[Text](size, errorRate))(state + next)

  // The rivals, each written as its own users would write it.
  def buildGuava(size: Int): com.google.common.hash.BloomFilter[CharSequence] =
    val funnel = com.google.common.hash.Funnels.stringFunnel(StandardCharsets.UTF_8).nn
    val filter = com.google.common.hash.BloomFilter.create(funnel, size, errorRate.double).nn
    var index = 0

    while index < size do
      filter.put(presentStrings(index))
      index += 1

    filter

  def buildAlexandrnikitin(size: Int): bloomfilter.mutable.BloomFilter[String] =
    val filter = bloomfilter.mutable.BloomFilter[String](size.toLong, errorRate.double)
    var index = 0

    while index < size do
      filter.add(presentStrings(index))
      index += 1

    filter

  def buildCommons(size: Int): org.apache.commons.collections4.bloomfilter.SimpleBloomFilter =
    import org.apache.commons.collections4.bloomfilter.{EnhancedDoubleHasher, Shape, SimpleBloomFilter}
    val filter = SimpleBloomFilter(Shape.fromNP(size, errorRate.double).nn)
    var index = 0

    while index < size do
      filter.merge(EnhancedDoubleHasher(presentBytes(index)))
      index += 1

    filter

  lazy val ulyssesBlake3: BloomFilter[Text, Blake3] = buildUlyssesBlake3(filled)
  lazy val ulyssesCrc32: BloomFilter[Text, Crc32] = buildUlyssesCrc32(filled)
  lazy val guava: com.google.common.hash.BloomFilter[CharSequence] = buildGuava(filled)
  lazy val alexandrnikitin: bloomfilter.mutable.BloomFilter[String] = buildAlexandrnikitin(filled)

  lazy val commons: org.apache.commons.collections4.bloomfilter.SimpleBloomFilter =
    buildCommons(filled)

  def ulyssesBlake3Present(): Boolean = ulyssesBlake3.hits(present(advance()))
  def ulyssesBlake3Absent(): Boolean = ulyssesBlake3.hits(absent(advance()))
  def ulyssesCrc32Present(): Boolean = ulyssesCrc32.hits(present(advance()))
  def ulyssesCrc32Absent(): Boolean = ulyssesCrc32.hits(absent(advance()))
  def guavaPresent(): Boolean = guava.mightContain(presentStrings(advance()))
  def guavaAbsent(): Boolean = guava.mightContain(absentStrings(advance()))
  def alexandrnikitinPresent(): Boolean = alexandrnikitin.mightContain(presentStrings(advance()))
  def alexandrnikitinAbsent(): Boolean = alexandrnikitin.mightContain(absentStrings(advance()))

  def commonsPresent(): Boolean =
    commons.contains(org.apache.commons.collections4.bloomfilter.EnhancedDoubleHasher(presentBytes(advance())))

  def commonsAbsent(): Boolean =
    commons.contains(org.apache.commons.collections4.bloomfilter.EnhancedDoubleHasher(absentBytes(advance())))

  // The false-positive rate each engine actually achieves over the absent ring, as a check that
  // the engines are being compared at the error rate they were all asked for.
  def falsePositives(query: () => Boolean): Int =
    var count = 0
    var index = 0

    while index < ring do
      if query() then count += 1
      index += 1

    count

  def run(): Unit =
    var index = 0

    while index < ring do
      assert(ulyssesBlake3Present(), "Ulysses (BLAKE3) lost a key")
      assert(ulyssesCrc32Present(), "Ulysses (CRC32) lost a key")
      assert(guavaPresent(), "Guava lost a key")
      assert(alexandrnikitinPresent(), "alexandrnikitin lost a key")
      assert(commonsPresent(), "commons-collections lost a key")
      index += 1

    // A rate of 1% over 1024 queries is ~10 false positives; anything over 50 means the filter
    // is not delivering the rate it was asked for.
    assert(falsePositives(() => ulyssesBlake3Absent()) < 50, "Ulysses (BLAKE3) FPR is too high")
    assert(falsePositives(() => ulyssesCrc32Absent()) < 50, "Ulysses (CRC32) FPR is too high")
    assert(falsePositives(() => guavaAbsent()) < 50, "Guava FPR is too high")
    assert(falsePositives(() => alexandrnikitinAbsent()) < 50, "alexandrnikitin FPR is too high")
    assert(falsePositives(() => commonsAbsent()) < 50, "commons-collections FPR is too high")

    val bench = Bench()

    bench(m"Build a filter from N elements")
      ( target = 1*Second,
        baseline = Engine.Guava,
        comparison = Baseline(compare = Min) )

    . over(Engine, Axis(t"size")(1_000, 100_000)):
        case (Engine.UlyssesBlake3, size) =>
          '{ ulysses.Benchmarks.buildUlyssesBlake3($size) }

        case (Engine.UlyssesCrc32, size) =>
          '{ ulysses.Benchmarks.buildUlyssesCrc32($size) }

        case (Engine.Guava, size) =>
          '{ ulysses.Benchmarks.buildGuava($size) }

        case (Engine.Alexandrnikitin, size) =>
          '{ ulysses.Benchmarks.buildAlexandrnikitin($size) }

        case (Engine.CommonsCollections, size) =>
          '{ ulysses.Benchmarks.buildCommons($size) }

    bench(m"Query a present element")
      ( target = 1*Second,
        baseline = Engine.Guava,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.UlyssesBlake3      => '{ ulysses.Benchmarks.ulyssesBlake3Present() }
        case Engine.UlyssesCrc32       => '{ ulysses.Benchmarks.ulyssesCrc32Present() }
        case Engine.Guava              => '{ ulysses.Benchmarks.guavaPresent() }
        case Engine.Alexandrnikitin    => '{ ulysses.Benchmarks.alexandrnikitinPresent() }
        case Engine.CommonsCollections => '{ ulysses.Benchmarks.commonsPresent() }

    bench(m"Query an absent element")
      ( target = 1*Second,
        baseline = Engine.Guava,
        comparison = Baseline(compare = Min) )

    . over(Engine):
        case Engine.UlyssesBlake3      => '{ ulysses.Benchmarks.ulyssesBlake3Absent() }
        case Engine.UlyssesCrc32       => '{ ulysses.Benchmarks.ulyssesCrc32Absent() }
        case Engine.Guava              => '{ ulysses.Benchmarks.guavaAbsent() }
        case Engine.Alexandrnikitin    => '{ ulysses.Benchmarks.alexandrnikitinAbsent() }
        case Engine.CommonsCollections => '{ ulysses.Benchmarks.commonsAbsent() }

    // What `+` costs against `++`: the same keys into the same filter, one copy per element.
    bench(m"Add elements one at a time with +")(target = 1*Second)
    . over(Axis(t"size")(1_000, 10_000)): size =>
        '{ ulysses.Benchmarks.buildUlyssesIncrementally($size) }
