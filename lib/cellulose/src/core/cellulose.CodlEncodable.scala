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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package cellulose

import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*
import wisteria.*

trait CodlEncodable extends Typeclass:
  def encoded(value: Self): Codl

trait CodlEncodable2:
  inline given derived: [value] => value is CodlEncodable = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => Codl.field[value]
    case given ProductReflection[`value`]     => CodlEncodableDerivation.derived[value]

object CodlEncodable extends CodlEncodable2:
  def encoder[value](lambda: value => Text): value is CodlEncodable = new CodlEncodable:
    type Self = value
    def encoded(value: value): Codl = Codl(List(IArray(CodlNode(Data(lambda(value))))))


  given booleanEncodable: Boolean is CodlEncodable = encoder(if _ then t"yes" else t"no")
  given textEncodable: Text is CodlEncodable = encoder(_.show)

  given optionalEncodable: [inner, value >: Unset.type: Mandatable to inner]
        => (encoder: => inner is CodlEncodable)
        => value is CodlEncodable =
    element =>
      element.let: element =>
        encoder.encoded(element.asInstanceOf[inner])
      . or(Codl(List()))


  given optionEncodable: [encodable: CodlEncodable] => Option[encodable] is CodlEncodable:
    def encoded(value: Option[encodable]): Codl = value match
      case None        => Codl(List())
      case Some(value) => encodable.encoded(value)

  given listEncodable: [element] => (element: => element is CodlEncodable) => List[element] is CodlEncodable:
    def encoded(value: List[element]): Codl = Codl(value.map(element.encoded(_).list.head))

  given setEncodable: [element: CodlEncodable] => Set[element] is CodlEncodable:
    def encoded(value: Set[element]): Codl = Codl(value.map(element.encoded(_).list.head).to(List))
