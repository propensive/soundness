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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import wisteria.*

import language.experimental.genericNumberLiterals

object Randomizable extends Derivation[[derivation] =>> derivation is Randomizable]:
  given long: Long is Randomizable = _.long()
  given byte: Byte is Randomizable = long.map(_.toByte)
  given short: Short is Randomizable = long.map(_.toShort)
  given int: Int is Randomizable = long.map(_.toInt)
  given char: Char is Randomizable = long.map(_.toChar)
  given seed: Seed is Randomizable = long.map(Seed(_))
  given boolean: Boolean is Randomizable = long.map(_ < 0L)

  given list: [element] => (randomizable: => element is Randomizable) => (size: RandomSize)
  =>  List[element] is Randomizable =

    random =>
      given random0: Random = random
      List.fill(size.generate(random))(randomizable.from(random))


  given set: [element] => (randomizable: => element is Randomizable) => (size: RandomSize)
  =>  Set[element] is Randomizable =

    random =>
      given random0: Random = random
      Set.fill(size.generate(random))(randomizable.from(random))


  given iarray: [element] => (randomizable: => element is Randomizable) => (tag: ClassTag[element])
  =>  (size: RandomSize)
  =>  IArray[element] is Randomizable =

    random =>
      given random0: Random = random
      IArray.fill(size.generate(random))(randomizable.from(random))


  given double: Distribution => Double is Randomizable = summon[Distribution].transform(_)

  inline def join[derivation <: Product: ProductReflection]: derivation is Randomizable = random =>
    stochastic(using infer[Randomization]):
      construct: [field] => _.from(summon[Random])

  inline def split[derivation: SumReflection]: derivation is Randomizable = random =>
    stochastic(using infer[Randomization]):
      delegate(variantLabels(random.long().abs.toInt%variantLabels.length)):
        [variant <: derivation] => _.from(summon[Random])

trait Randomizable extends Typeclass:
  def apply()(using random: Random): Self = from(random)
  def from(random: Random): Self
  def map[self2](lambda: Self => self2): self2 is Randomizable = random => lambda(from(random))
