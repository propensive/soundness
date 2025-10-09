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
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

trait CodlDecodable extends Typeclass:
  def decoded(value: List[Indexed]): Self raises CodlError
  def schema: CodlSchema

object CodlDecodable:
  def apply[value]
       (schema0: => CodlSchema, decode0: Tactic[CodlError] ?=> List[Indexed] => value)
  : value is CodlDecodable =

      new:
        def decoded(value: List[Indexed]): value raises CodlError = decode0(value)
        def schema: CodlSchema  = schema0

  def apply[value](lambda: Text => value): value is CodlDecodable = new CodlDecodable:
    type Self = value
    val schema: CodlSchema = Field(Arity.One)

    def decoded(nodes: List[Indexed]): value raises CodlError =
      nodes.prim.lest(CodlError(CodlError.Reason.BadFormat(Unset))).children match
        case IArray(CodlNode(Data(value, _, _, _), _)) => lambda(value)

        case _ =>
          abort(CodlError(CodlError.Reason.BadFormat(Unset)))

  inline given derived: [value] => value is CodlDecodable = compiletime.summonFrom:
    case given (`value` is Decodable in Text) => field[`value`]
    case given ProductReflection[`value`]     => CodlDecodableDerivation.derived[`value`]

  def field[value: Decodable in Text]: value is CodlDecodable = apply(value.decoded(_))

  given boolean: Boolean is CodlDecodable = apply(_ == t"yes")
  given text: Text is CodlDecodable = apply(identity(_))

  given unit: Unit is CodlDecodable:
    val schema: CodlSchema = Field(Arity.One)
    def decoded(nodes: List[Indexed]): Unit raises CodlError = ()

  given optional: [value >: Unset.type: Mandatable] => (decoder: => value.Result is CodlDecodable)
        => value is CodlDecodable:

    def schema: CodlSchema = decoder.schema.optional

    def decoded(nodes: List[Indexed]): value raises CodlError =
      if nodes.isEmpty then Unset else decoder.decoded(nodes)

  given option: [decodable] => (decoder: => decodable is CodlDecodable)
        => Option[decodable] is CodlDecodable:
    def schema: CodlSchema = decoder.schema.optional

    def decoded(nodes: List[Indexed]): Option[decodable] raises CodlError =
      if nodes.isEmpty then None else Some(decoder.decoded(nodes))

  given list: [element] => (element: => element is CodlDecodable) => List[element] is CodlDecodable =
    new CodlDecodable:
      type Self = List[element]
      def schema: CodlSchema = element.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)

      def decoded(value: List[Indexed]): List[element] raises CodlError =
        element.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            element.decoded(List(CodlDoc(node)))

          case struct: Struct =>
            value.map { v => element.decoded(List(v)) }

  given set: [element] => (element: => element is CodlDecodable) => Set[element] is CodlDecodable:
    def schema: CodlSchema = element.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def decoded(value: List[Indexed]): Set[element] raises CodlError =
      element.schema match
        case Field(_, validator) =>
          value.flatMap(_.children).map { node => element.decoded(List(CodlDoc(node))) }.to(Set)

        case struct: Struct =>
          value.map { v => element.decoded(List(v)) }.to(Set)
