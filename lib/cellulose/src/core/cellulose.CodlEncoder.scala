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
┃    Soundness, version 0.42.0.                                                                    ┃
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

trait CodlEncoder[value]:
  def encode(value: value): List[IArray[CodlNode]]
  def schema: CodlSchema

trait CodlEncoder2:
  def field[encodable: Encodable in Text]: CodlEncoder[encodable] = new CodlEncoder[encodable]:
    def schema: CodlSchema = Field(Arity.One)

    def encode(value: encodable): List[IArray[CodlNode]] =
      List(IArray(CodlNode(Data(encodable.encode(value)))))

  inline given derived: [value] => CodlEncoder[value] = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => field[value]
    case given ProductReflection[`value`]     => CodlEncoderDerivation.derived[value]

object CodlEncoder extends CodlEncoder2:
  def apply[value](schema0: CodlSchema, encode0: value => List[IArray[CodlNode]])
  : CodlEncoder[value] =

      new:
        def schema: CodlSchema = schema0
        def encode(value: value): List[IArray[CodlNode]] = encode0(value)


  given boolean: CodlFieldWriter[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldWriter[Text] = _.show


  given optional: [encodable] => (encodable: => CodlEncoder[encodable])
  =>  CodlEncoder[Optional[encodable]]:

        def schema: CodlSchema = encodable.schema.optional

        def encode(value: Optional[encodable]): List[IArray[CodlNode]] =
          value.let(encodable.encode(_)).or(List())


  given option: [encodable: CodlEncoder] => CodlEncoder[Option[encodable]]:
    def schema: CodlSchema = encodable.schema.optional

    def encode(value: Option[encodable]): List[IArray[CodlNode]] = value match
      case None        => List()
      case Some(value) => encodable.encode(value)

  given list: [element] => (element: => CodlEncoder[element]) => CodlEncoder[List[element]]:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: List[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head)

  given set: [element: CodlEncoder] => CodlEncoder[Set[element]]:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: Set[element]): List[IArray[CodlNode]] =
      value.map(element.encode(_).head).to(List)
