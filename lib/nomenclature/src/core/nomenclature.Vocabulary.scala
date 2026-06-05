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
package nomenclature

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import turbulence.*

import charDecoders.utf8
import textSanitizers.skip

object Vocabulary:
  def apply[source: Streamable by Data, transport]
    ( adverbs: source, adjectives: source, animals: source )
  :   Vocabulary over transport =

    new Vocabulary(load(adverbs), load(adjectives), load(animals))
    . asInstanceOf[Vocabulary over transport]

  private def load[source: Streamable by Data](resource: source): List[Text] =
    resource.read[Text].cut(t"\n").map(_.trim).filter(_ != t"")

class Vocabulary private (adverbs: List[Text], adjectives: List[Text], animals: List[Text]):
  type Transport

  private val adverbArray:    IArray[Text] = IArray.from(adverbs)
  private val adjectiveArray: IArray[Text] = IArray.from(adjectives)
  private val animalArray:    IArray[Text] = IArray.from(animals)
  private val adverbCount:    Int          = adverbs.length
  private val adjectiveCount: Int          = adjectives.length
  private val animalCount:    Int          = animals.length
  private val adverbIndex:    Map[Text, Int] = adverbs.zipWithIndex.to(Map)
  private val adjectiveIndex: Map[Text, Int] = adjectives.zipWithIndex.to(Map)
  private val animalIndex:    Map[Text, Int] = animals.zipWithIndex.to(Map)

  // A moniker is animal-only below `adjectiveBase`, adjective-animal below `adverbBase`,
  // and adverb-adjective-animal above it.
  private val adjectiveBase: Int = animalCount
  private val adverbBase:    Int = adjectiveBase + adjectiveCount*animalCount

  def size: Int = adverbBase + adverbCount*adjectiveCount*animalCount

  def name(ordinal: Int)(using Tactic[MonikerError]): Text =
    if ordinal < 0 || ordinal >= size then
      abort(MonikerError(MonikerError.Reason.OutOfRange(ordinal)))
    else if ordinal < adjectiveBase then
      animalArray(ordinal)
    else if ordinal < adverbBase then
      val rest = ordinal - adjectiveBase
      t"${adjectiveArray(rest/animalCount)}-${animalArray(rest%animalCount)}"
    else
      val rest = ordinal - adverbBase
      val group = adjectiveCount*animalCount
      val tail = rest%group
      val adverb = adverbArray(rest/group)
      val adjective = adjectiveArray(tail/animalCount)
      val animal = animalArray(tail%animalCount)
      t"$adverb-$adjective-$animal"

  def number(moniker: Text)(using Tactic[MonikerError]): Int =
    moniker.cut(t"-") match
      case List(animal) =>
        lookup(animalIndex, animal)

      case List(adjective, animal) =>
        adjectiveBase + lookup(adjectiveIndex, adjective)*animalCount + lookup(animalIndex, animal)

      case List(adverb, adjective, animal) =>
        val prefix = lookup(adverbIndex, adverb)*adjectiveCount*animalCount
        val middle = lookup(adjectiveIndex, adjective)*animalCount
        adverbBase + prefix + middle + lookup(animalIndex, animal)

      case _ =>
        abort(MonikerError(MonikerError.Reason.Malformed(moniker)))

  private def lookup(index: Map[Text, Int], word: Text)(using Tactic[MonikerError]): Int =
    index.get(word).getOrElse(abort(MonikerError(MonikerError.Reason.UnknownWord(word))))
