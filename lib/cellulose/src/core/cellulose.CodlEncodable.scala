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
  def encode(value: Self): List[IArray[CodlNode]]

trait CodlEncodable2:
  def field[encodable: Encodable in Text]: encodable is CodlEncodable = new CodlEncodable:
    type Self = encodable

    def encode(value: encodable): List[IArray[CodlNode]] =
      List(IArray(CodlNode(Data(encodable.encode(value)))))

  inline given derived: [value] => value is CodlEncodable = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => field[value]
    case given ProductReflection[`value`]     => CodlEncodableDerivation.derived[value]

object CodlEncodable extends CodlEncodable2:
  def apply[value](encode0: value => List[IArray[CodlNode]]): value is CodlEncodable =
    new:
      def encode(value: value): List[IArray[CodlNode]] = encode0(value)

  def apply2[value](lambda: value => Text): value is CodlEncodable = new CodlEncodable:
    type Self = value
    def encodeField(value: value): Text = lambda(value)

    def encode(value: value): List[IArray[CodlNode]] =
      List(IArray(CodlNode(Data(encodeField(value)))))


  given boolean: Boolean is CodlEncodable = apply2(if _ then t"yes" else t"no")
  given text: Text is CodlEncodable = apply2(_.show)

  given optional: [inner, value >: Unset.type: Mandatable to inner]
        => (encoder: => inner is CodlEncodable)
        => value is CodlEncodable:

    def encode(element: value): List[IArray[CodlNode]] =
      element.let: element =>
        encoder.encode(element.asInstanceOf[inner])
      . or(List())


  given option: [encodable: CodlEncodable] => Option[encodable] is CodlEncodable:
    def encode(value: Option[encodable]): List[IArray[CodlNode]] = value match
      case None        => List()
      case Some(value) => encodable.encode(value)

  given list: [element] => (element: => element is CodlEncodable) => List[element] is CodlEncodable:
    def encode(value: List[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head)

  given set: [element: CodlEncodable] => Set[element] is CodlEncodable:
    def encode(value: Set[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head).to(List)
