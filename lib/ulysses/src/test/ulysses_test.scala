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

object Tests extends Suite(m"Ulysses tests"):
  def run(): Unit =
    test(m"Check how many bits are required for a bloom filter"):
      val bloom = BloomFilter[Text](100, 0.01)
      bloom.bitSize

    . assert(_ == 663)

    test(m"Check that more bits are required to store more elements"):
      val bloom = BloomFilter[Text](1000, 0.01)
      bloom.bitSize

    . assert(_ == 6631)

    test(m"More bits required for more certainty"):
      val bloom = BloomFilter[Text](100, 0.001)
      bloom.bitSize

    . assert(_ == 994)

    val bloom = test(m"Add an element to a Bloom filter"):
      BloomFilter[Text](100, 0.001) + t"Hello world"

    . check(_.hits(t"Hello world"))

    test(m"Check that Bloom filter does not contain other strings"):
      !bloom.hits(t"hello")

    . check(identity(_))

    test(m"Add multiple elements to a Bloom filter"):
      bloom ++ List(t"hello", t"world")

    . assert { b => b.hits(t"hello") && b.hits(t"world") }

    val numbers = List(t"one", t"two", t"three", t"four", t"five", t"six", t"seven", t"eight", t"9", t"10", t"11", t"12", t"13", t"14", t"15", t"16").map { n => Array.frozen(n.digest[Blake3].data.readable.slice(0, 12)) }
    val numbers2 = (1 to 20).map(_.toString.tt.digest[Blake3].data)

    test(m"Encode a Palimpsest under the default Cadence"):
      given bibliography: Bibliography = Bibliography(numbers)
      Palimpsest((1 to 3).map(numbers(_))).resolve

    . assert(_ == (1 to 3).map(numbers(_)))

    val letters = List(t"alpha", t"beta", t"gamma", t"delta").map(_.digest[Blake3].data)

    test(m"Round-trip a Palimpsest under an overridden Cadence"):
      given cadence: Cadence = Cadence(initial = 4, regular = 2, hashSize = 32)
      given bibliography: Bibliography = Bibliography(letters)
      Palimpsest(letters.toIndexedSeq).resolve

    . assert(_ == letters)
