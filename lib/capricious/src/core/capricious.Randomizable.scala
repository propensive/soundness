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

import scala.caps

import scala.language.experimental.genericNumberLiterals

import hypotenuse.*
import prepositional.*
import wisteria.*

object Randomizable extends Derivation[[derivation] =>> derivation is Randomizable]:
  given long: Long is Randomizable = _.long()
  given byte: Byte is Randomizable = long.map(_.toByte)
  given short: Short is Randomizable = long.map(_.toShort)
  given int: Int is Randomizable = long.map(_.toInt)
  given char: Char is Randomizable = long.map(_.toChar)
  given seed: Seed is Randomizable = long.map(Seed(_))
  given boolean: Boolean is Randomizable = long.map(_ < 0L)

  given list: [element] => (randomizable: => element is Randomizable) => (size: Random.Size)
  =>  List[element] is Randomizable =

    // Laundered pure: the by-name element instance shares this instance's given-resolution
    // lifetime (the codec-thunk seal pattern; see rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure:
      random =>
        given random0: (Random^{random}) = random
        List.fill(size.generate(random))(randomizable.randomize(random))

  given set: [element] => (randomizable: => element is Randomizable) => (size: Random.Size)
  =>  Set[element] is Randomizable =

    // Laundered pure: the by-name element instance shares this instance's given-resolution
    // lifetime (the codec-thunk seal pattern; see rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure:
      random =>
        given random0: (Random^{random}) = random
        (List.fill(size.generate(random))(randomizable.randomize(random)).stdlib).to(Set)

  given iarray: [element] => (randomizable: => element is Randomizable) => (tag: ClassTag[element])
  =>  ( size: Random.Size )
  =>  (Array[element]^{}) is Randomizable =

    // Laundered pure, as for `list` above.
    caps.unsafe.unsafeAssumePure:
      random =>
        given random0: (Random^{random}) = random
        Array.fill(size.generate(random))(randomizable.randomize(random))

  given double: Distribution => Double is Randomizable = summon[Distribution].transform(_)

  inline def conjunction[derivation <: Product: ProductReflection]: derivation is Randomizable =
    random =>
      stochastic(using infer[Randomization]):
        build[derivation]: [field] => _.randomize(summon[Random])

  inline def disjunction[derivation: SumReflection]: derivation is Randomizable = random =>
    stochastic(using infer[Randomization]):
      val labels = variantLabels.stdlib

      delegate(labels(random.long().toInt.abs%labels.length)):
        [variant <: derivation] => _.randomize(summon[Random])

trait Randomizable extends Typeclass:
  def apply()(using random: Random): Self = randomize(random)
  def randomize(random: Random): Self
  def map[self2](lambda: Self => self2): (self2 is Randomizable)^{this, lambda} = random => lambda(randomize(random))
