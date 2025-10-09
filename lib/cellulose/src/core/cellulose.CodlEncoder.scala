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

trait CodlEncoder extends Typeclass:
  def encode(value: Self): List[IArray[CodlNode]]
  def schema: CodlSchema

trait CodlEncoder2:
  def field[encodable: Encodable in Text]: encodable is CodlEncoder = new CodlEncoder:
    type Self = encodable
    def schema: CodlSchema = Field(Arity.One)

    def encode(value: encodable): List[IArray[CodlNode]] =
      List(IArray(CodlNode(Data(encodable.encode(value)))))

  inline given derived: [value] => value is CodlEncoder = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => field[value]
    case given ProductReflection[`value`]     => CodlEncoderDerivation.derived[value]

object CodlEncoder extends CodlEncoder2:
  def apply[value](schema0: CodlSchema, encode0: value => List[IArray[CodlNode]])
  : value is CodlEncoder =

      new:
        def schema: CodlSchema = schema0
        def encode(value: value): List[IArray[CodlNode]] = encode0(value)


  given boolean: CodlFieldWriter[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldWriter[Text] = _.show


  given optional: [inner, value >: Unset.type: Mandatable to inner]
        => (encoder: => inner is CodlEncoder)
        => value is CodlEncoder:

    def schema: CodlSchema = encoder.schema.optional

    def encode(element: value): List[IArray[CodlNode]] =
      element.let: element =>
        encoder.encode(element.asInstanceOf[inner])
      . or(List())


  given option: [encodable: CodlEncoder] => Option[encodable] is CodlEncoder:
    def schema: CodlSchema = encodable.schema.optional

    def encode(value: Option[encodable]): List[IArray[CodlNode]] = value match
      case None        => List()
      case Some(value) => encodable.encode(value)

  given list: [element] => (element: => element is CodlEncoder) => List[element] is CodlEncoder:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: List[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head)

  given set: [element: CodlEncoder] => Set[element] is CodlEncoder:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: Set[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head).to(List)
