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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package nomenclature

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import turbulence.*
import zephyrine.*

import charDecoders.utf8Decoder
import textSanitizers.skipSanitizer

object Vocabulary:
  def apply[source: Streamable by Data over Credit, transport](adjectives: source, animals: source)
  :   Vocabulary over transport =

    new Vocabulary(load(adjectives), load(animals)).asInstanceOf[Vocabulary over transport]

  private def load[source: Streamable by Data over Credit](resource: source): List[Text] =
    resource.read[Text].cut(t"\n").map(_.trim).filter(_ != t"")

class Vocabulary private (adjectives: List[Text], animals: List[Text]):
  type Transport

  private val adjectiveArray: IArray[Text] = IArray.from(adjectives)
  private val animalArray:    IArray[Text] = IArray.from(animals)
  private val animalCount:    Int          = animals.length
  private val adjectiveIndex: Map[Text, Int] = adjectives.zipWithIndex.to(Map)
  private val animalIndex:    Map[Text, Int] = animals.zipWithIndex.to(Map)

  def size: Int = adjectiveArray.length*animalCount

  def name(ordinal: Int)(using Tactic[MonikerError]): Text =
    if ordinal < 0 || ordinal >= size
    then abort(MonikerError(MonikerError.Reason.OutOfRange(ordinal)))
    else t"${adjectiveArray(ordinal/animalCount)}-${animalArray(ordinal%animalCount)}"

  def number(moniker: Text)(using Tactic[MonikerError]): Int =
    moniker.cut(t"-") match
      case List(adjective, animal) =>
        val first = adjectiveIndex.get(adjective).getOrElse:
          abort(MonikerError(MonikerError.Reason.UnknownWord(adjective)))

        val second = animalIndex.get(animal).getOrElse:
          abort(MonikerError(MonikerError.Reason.UnknownWord(animal)))

        first*animalCount + second

      case _ =>
        abort(MonikerError(MonikerError.Reason.Malformed(moniker)))
