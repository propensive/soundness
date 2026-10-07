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

import soundness.*

import providers.soundnessProvider
import Blake3.hash   // a concrete `Hash in Blake3` in scope, so BloomFilter infers BLAKE3
import alphabets.hexUpperCase
import cryptoPermits.permitNonCryptographicHashes

case class Point(x: Int, y: Int)

object Tests extends Suite(m"Ulysses tests"):
  def run(): Unit =
    test(m"Check how many bits are required for a bloom filter"):
      val bloom = BloomFilter[Text](100, 0.01)
      bloom.bitSize

    . assert(_ == 959)

    test(m"Check that more bits are required to store more elements"):
      val bloom = BloomFilter[Text](1000, 0.01)
      bloom.bitSize

    . assert(_ == 9586)

    test(m"More bits required for more certainty"):
      val bloom = BloomFilter[Text](100, 0.001)
      bloom.bitSize

    . assert(_ == 1438)

    val bloom = test(m"Add an element to a Bloom filter"):
      BloomFilter[Text](100, 0.001) :+ t"Hello world"

    . check(_.hits(t"Hello world"))

    test(m"Check that Bloom filter does not contain other strings"):
      !bloom.hits(t"hello")

    . check(identity(_))

    test(m"Add multiple elements to a Bloom filter"):
      bloom ++ List(t"hello", t"world")

    . assert { b => b.hits(t"hello") && b.hits(t"world") }

    val keys = List.tabulate(10_000) { index => ("key-"+index).tt }
    val others = List.tabulate(10_000) { index => ("other-"+index).tt }

    val filled = test(m"Fill a filter with ten thousand keys"):
      BloomFilter[Text](10_000, 0.01) ++ keys

    . check(_.hashCount == 7)

    test(m"Every added key is reported present"):
      keys.all(filled.hits(_))

    . assert(identity(_))

    // Expect about 100 false positives in 10,000 at a 1% rate; the bounds are wide enough to
    // absorb the variance of one sample, but exclude a filter that is undersized or degenerate.
    test(m"False-positive rate is close to the target rate"):
      others.count(filled.hits(_))

    . assert { count => count >= 30 && count <= 150 }

    val checksummed = test(m"Fill a filter under a 32-bit checksum hash"):
      BloomFilter[Text](10_000, 0.01)[Crc32] ++ keys

    . check(_.hashCount == 7)

    test(m"Every added key is present under a 32-bit hash"):
      keys.all(checksummed.hits(_))

    . assert(identity(_))

    test(m"False-positive rate is close to the target under a 32-bit hash"):
      others.count(checksummed.hits(_))

    . assert { count => count >= 30 && count <= 150 }

    // A one-in-a-hundred-million filter needs 18 positions of 17 bits each — more than one
    // 256-bit digest supplies — so a single element must still land on 18 distinct positions.
    test(m"A demanding filter draws every position from fresh hash bits"):
      val filter = BloomFilter[Text](5000, 0.00000001) :+ t"Hello world"
      filter.population == filter.hashCount

    . assert(identity(_))

    test(m"Adding elements one at a time equals adding them together"):
      val empty = BloomFilter.freeze(BloomFilter[Text](1000, 0.01))
      keys.take(500).foldLeft(empty)(_ :+ _) == empty ++ keys.take(500)

    . assert(identity(_))

    test(m"Adding in place then freezing equals adding by copying"):
      val filter = BloomFilter[Text](1000, 0.01)
      filter.add(t"one")
      filter.addAll(keys.take(500))
      BloomFilter.freeze(filter) == (BloomFilter[Text](1000, 0.01) :+ t"one") ++ keys.take(500)

    . assert(identity(_))

    test(m"A frozen filter answers for everything added before it was frozen"):
      val filter = BloomFilter[Text](1000, 0.01)
      filter.addAll(keys.take(500))
      val frozen = BloomFilter.freeze(filter)
      keys.take(500).all(frozen.hits(_)) && !frozen.hits(t"other-1")

    . assert(identity(_))

    test(m"A case class can be an element"):
      val filter = BloomFilter[Point](100, 0.01) :+ Point(1, 2)
      filter.hits(Point(1, 2)) && !filter.hits(Point(2, 1))

    . assert(identity(_))

    test(m"A filter sized for no elements can still hold one"):
      (BloomFilter[Text](0, 0.01) :+ t"x").hits(t"x")

    . assert(identity(_))

    test(m"A filter tolerating every false positive does not fail"):
      (BloomFilter[Text](100, 1.0) :+ t"x").hits(t"x")

    . assert(identity(_))

    val numbers = List(t"one", t"two", t"three", t"four", t"five", t"six", t"seven", t"eight", t"9", t"10", t"11", t"12", t"13", t"14", t"15", t"16").map { n => Array.frozen(n.digest[Blake3].data.readable.slice(0, 12)) }
    val numbers2 = (1 to 20).map(_.toString.tt.digest[Blake3].data)

    test(m"Encode a Palimpsest under the default Cadence"):
      given bibliography: Bibliography = Bibliography(proscenium.List.from(numbers))
      Palimpsest(Sequence.from((1 to 3).map(numbers(_)))).resolve

    . assert(_ == (1 to 3).map(numbers(_)))

    def bytes(values: Int*): Data = Array.frozen(scala.IArray.from(values.map(_.toByte)))

    val shelf = Bibliography:
      proscenium.List(bytes(0x80, 1), bytes(0x10, 2), bytes(0x80, 0), bytes(0x7f, 9), bytes(0x81))

    def found(prefix: Data): List[Text] = List.from(shelf.lookup(prefix).map(_.serialize[Hex]))

    test(m"A prefix lookup finds every hash sharing the prefix"):
      found(bytes(0x80))

    . assert(_ == List(t"8000", t"8001"))

    test(m"A prefix lookup orders bytes as unsigned"):
      (found(bytes(0x7f)), found(bytes(0x81)))

    . assert(_ == (List(t"7F09"), List(t"81")))

    test(m"A prefix matching no hash finds nothing"):
      found(bytes(0x11))

    . assert(_ == Nil)

    test(m"A prefix longer than a hash does not match it"):
      found(bytes(0x81, 0))

    . assert(_ == Nil)

    test(m"An empty prefix finds every hash"):
      found(bytes())

    . assert(_ == List(t"1002", t"7F09", t"8000", t"8001", t"81"))

    val letters = List(t"alpha", t"beta", t"gamma", t"delta").map(_.digest[Blake3].data)

    test(m"Round-trip a Palimpsest under an overridden Cadence"):
      given cadence: Cadence = Cadence(initial = 4, regular = 2, hashSize = 32)
      given bibliography: Bibliography = Bibliography(proscenium.List.from(letters))
      Palimpsest(Sequence.from(letters.toSeq)).resolve

    . assert(_ == letters)
