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
import proscenium.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

trait CodlDecodable extends Typeclass:
  def decoded(value: List[Codllike]): Self

object CodlDecodable:
  def apply[value](decode0: Tactic[CodlError] ?=> List[Codllike] => value)
  : value is CodlDecodable raises CodlError =

      new:
        def decoded(value: List[Codllike]): value = decode0(value)

  def decodable[value](lambda: Text => value): value is CodlDecodable raises CodlError =
    new CodlDecodable:
      type Self = value

      def decoded(nodes: List[Codllike]): value =
        nodes.prim.lest(CodlError(CodlError.Reason.BadFormat(Unset))).children match
          case IArray(CodlNode(Data(value, _, _, _), _)) => lambda(value)

          case _ =>
            abort(CodlError(CodlError.Reason.BadFormat(Unset)))

  inline given derived: [value] => Tactic[CodlError] => value is CodlDecodable =
    compiletime.summonFrom:
      case given (`value` is Decodable in Text) => field[`value`]
      case given ProductReflection[`value`]     => CodlDecodableDerivation().derived[`value`]

  def field[value: Decodable in Text]: value is CodlDecodable raises CodlError =
    decodable(value.decoded(_))

  given boolean: Tactic[CodlError] => Boolean is CodlDecodable = decodable(_ == t"yes")
  given text: Tactic[CodlError] => Text is CodlDecodable = decodable(identity(_))
  given unit: Unit is CodlDecodable = _ => ()

  given optional: [value >: Unset.type: Mandatable] => (decoder: => value.Result is CodlDecodable)
        => value is CodlDecodable =
    nodes => if nodes.isEmpty then Unset else decoder.decoded(nodes)

  given option: [decodable] => (decoder: => decodable is CodlDecodable)
        => Option[decodable] is CodlDecodable =
    nodes => if nodes.isEmpty then None else Some(decoder.decoded(nodes))

  given list: [element: CodlSchematic] => (decodable: => element is CodlDecodable)
        => List[element] is CodlDecodable =

    value => element.schema() match
      case Field(_, validator) => value.flatMap(_.children).map: node =>
        decodable.decoded(List(CodlDoc(node)))

      case struct: Struct =>
        value.map(List(_)).map(decodable.decoded(_))

  given set: [element: CodlSchematic] => (decodable: => element is CodlDecodable)
        => Set[element] is CodlDecodable =
    value =>
      element.schema() match
        case Field(_, validator) =>
          value.flatMap(_.children).map { node => decodable.decoded(List(CodlDoc(node))) }.to(Set)

        case struct: Struct =>
          value.map(List(_)).map(decodable.decoded(_)).to(Set)
