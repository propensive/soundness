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
┃    Soundness, version 0.37.0.                                                                    ┃
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
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*

import scala.deriving.*

trait CodlDecoder[value]:
  def decoded(value: List[Indexed]): value raises CodlReadError
  def schema: CodlSchema

object CodlDecoder:
  def apply[value]
       (schema0: => CodlSchema, decode0: Tactic[CodlReadError] ?=> List[Indexed] => value)
  : CodlDecoder[value] =

      new:
        def decoded(value: List[Indexed]): value raises CodlReadError = decode0(value)
        def schema: CodlSchema  = schema0


  inline given derived: [value] => CodlDecoder[value] = compiletime.summonFrom:
    case given (`value` is Decodable in Text) => field[`value`]
    case given ProductReflection[`value`]     => CodlDecoderDerivation.derived[`value`]

  def field[value: Decodable in Text]: CodlDecoder[value] = CodlFieldReader(value.decoded(_))

  given boolean: CodlDecoder[Boolean] = CodlFieldReader(_ == t"yes")
  given text: CodlDecoder[Text] = CodlFieldReader(identity(_))

  given unit: CodlDecoder[Unit]:
    val schema: CodlSchema = Field(Arity.One)
    def decoded(nodes: List[Indexed]): Unit raises CodlReadError = ()

  given optional: [value: CodlDecoder] => CodlDecoder[Optional[value]]:
    def schema: CodlSchema = value.schema.optional

    def decoded(nodes: List[Indexed]): Optional[value] raises CodlReadError =
      if nodes.isEmpty then Unset else value.decoded(nodes)

  given option: [value] => (value: => CodlDecoder[value]) => CodlDecoder[Option[value]]:
    def schema: CodlSchema = value.schema.optional

    def decoded(nodes: List[Indexed]): Option[value] raises CodlReadError =
      if nodes.isEmpty then None else Some(value.decoded(nodes))

  given list: [element] => (element: => CodlDecoder[element]) => CodlDecoder[List[element]] =
    new CodlDecoder[List[element]]:
      def schema: CodlSchema = element.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)

      def decoded(value: List[Indexed]): List[element] raises CodlReadError =
        element.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            element.decoded(List(CodlDoc(node)))

          case struct: Struct =>
            value.map { v => element.decoded(List(v)) }

  given set: [element] => (element: => CodlDecoder[element]) => CodlDecoder[Set[element]]:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def decoded(value: List[Indexed]): Set[element] raises CodlReadError =
      element.schema match
        case Field(_, validator) =>
          value.flatMap(_.children).map { node => element.decoded(List(CodlDoc(node))) }.to(Set)

        case struct: Struct =>
          value.map { v => element.decoded(List(v)) }.to(Set)
