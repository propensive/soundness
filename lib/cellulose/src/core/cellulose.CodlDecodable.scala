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
  def decoded(value: Codl): Self

object CodlDecodable:
  def apply[value](lambda: Text => value): value is CodlDecodable raises CodlError =
    codl =>
      codl.list.prim.lest(CodlError(CodlError.Reason.BadFormat(Unset))).children match
        case IArray(CodlNode(Data(value, _, _, _), _)) => lambda(value)

        case _ =>
          abort(CodlError(CodlError.Reason.BadFormat(Unset)))

  inline given derived: [value] => Tactic[CodlError] => value is CodlDecodable =
    compiletime.summonFrom:
      case given (`value` is Decodable in Text) => field[`value`]
      case given ProductReflection[`value`]     => CodlDecodableDerivation().derived[`value`]

  def field[value: Decodable in Text]: value is CodlDecodable raises CodlError =
    CodlDecodable(value.decoded(_))

  given booleanDecodable: Tactic[CodlError] => Boolean is CodlDecodable = CodlDecodable(_ == t"yes")
  given textDecodable: Tactic[CodlError] => Text is CodlDecodable = CodlDecodable(identity(_))
  given unitDecodable: Unit is CodlDecodable = _ => ()

  given optionalDecodable: [value >: Unset.type: Mandatable]
        => (decoder: => value.Result is CodlDecodable)
        =>  value is CodlDecodable =
    codl => if codl.list.isEmpty then Unset else decoder.decoded(codl)

  given optionDecodable: [decodable] => (decoder: => decodable is CodlDecodable)
        => Option[decodable] is CodlDecodable =
    codl => if codl.list.isEmpty then None else Some(decoder.decoded(codl))

  given listDecodable: [element: CodlSchematic] => (decodable: => element is CodlDecodable)
        => List[element] is CodlDecodable =

    value => element.schema() match
      case Field(_, validator) => value.list.flatMap(_.children).map: node =>
        decodable.decoded(Codl(List(CodlDoc(node))))

      case struct: Struct =>
        value.list.map(List(_)).map(Codl(_)).map(decodable.decoded(_))

  given setDecodable: [element: CodlSchematic] => (decodable: => element is CodlDecodable)
        => Set[element] is CodlDecodable =
    value =>
      element.schema() match
        case Field(_, validator) =>
          value.list.flatMap(_.children).map { node => decodable.decoded(Codl(List(CodlDoc(node)))) }.to(Set)

        case struct: Struct =>
          value.list.map(List(_)).map(Codl(_)).map(decodable.decoded(_)).to(Set)
