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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package legerdemain

import scala.language.dynamics

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*
import wisteria.*

object Query extends Dynamic:
  def apply(): Query = new Query(Nil)
  def apply(parameter: Text): Query = new Query(List(t"" -> parameter))

  given encodable: Query is Encodable in Text =
    _.values.map: (key, value) => t"${key.urlEncode}=${value.urlEncode}"
    . join(t"&")

  given decodable: Query is Decodable in Text = text => Query:
    text.cut(t"&").map: next =>
      next.cut(t"=", 2) match
        case List(key, value) => (key.urlDecode, value.urlDecode)
        case List(key)        => (key.urlDecode, t"")
        case _                => (t"", t"")

  object EncodableDerivation extends ProductDerivation[[Type] =>> Type is Encodable in Query]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Query =

      value =>
        Query:
          // Via the stdlib view: inline re-elaboration freshens the frozen array, defeating
          // both `to[List]` and the compat `toList`.
          proscenium.List.of:
              fields(value) { [field] => field => contextual.encoded(field).prefix(label) }
              . readable.toList
          . flatMap(_.values)

  object DecodableDerivation extends ProductDerivation[[Type] =>> Type is Decodable in Query]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Query =

      provide[Foci[Pointer]]:
        value =>
          build[derivation]:
            [field] => context =>
              focus(prior.lay(Pointer(label))(_(label))):
                context.decoded(value(label))

  given booleanEncodable: Boolean is Encodable in Query =
    boolean => if boolean then Query(t"on") else Query()

  given booleanDecodable: Boolean is Decodable in Query = _().present

  inline given encodable: [value] => value is Encodable in Query = summonFrom:
    case given (`value` is Encodable in Text) => value => Query(value.encode)

    case given ProductReflection[`value` & Product] =>
      EncodableDerivation.conjunction[value & Product].asInstanceOf[value is Encodable in Query]

  given showable: Query is Showable =
    _.values.map { case (key, value) => t"$key = \"${value}\"" }.join(t", ")

  inline given decodable: [value] => value is Decodable in Query =
    summonFrom:
      case given (`value` is Decodable in Text) =>
        provide[Tactic[QueryError]]:
          summonFrom:
            case default: Default[`value`] =>
              _().let(_.as).or:
                raise(QueryError(QueryError.Reason.Missing))
                default()

            case _ =>
              _().lest(QueryError(QueryError.Reason.Missing)).as

      case given ProductReflection[`value` & Product] =>
        DecodableDerivation.conjunction[value & Product].asInstanceOf[value is Decodable in Query]

  inline def applyDynamicNamed(method: "make")(inline parameters: (Label, Any)*): Query =
    ${legerdemain.internal.query('parameters)}

  def apply(parameters: List[(Text, Text)]): Query = new Query(parameters)

  given addable: Query is Addable by Query to Query =
    Addable: (left, right) => new Query(List.of(left.values.stdlib ++ right.values.stdlib))

case class Query private (values: List[(Text, Text)]) extends Dynamic:
  // private lazy val map: Map[Text, Text | List[Text]] = values.groupMap(_(0))(_(1))
  def append(more: Query): Query = new Query(List.of(values.stdlib ++ more.values.stdlib))
  def nil: Boolean = values.nil

  @targetName("appendAll")
  infix def ++ (query: Query) = Query(List.of(values.stdlib ++ query.values.stdlib))


  def selectDynamic[result](label: String)(using erased parametric: label.type is Parametric to result)
    ( using decodable: result is Decodable in Query )
  :   result =

    decodable.decoded(apply(label.tt))


  def updateDynamic(label: String)[result: Encodable in Query]
    ( using erased parametric: label.type is Parametric to result )
    ( value: result )
  :   Query =

    val updates = value.encode.values.stdlib

    val values2 =
      if updates.length == 1 && updates(0)(0) == ""
      then (label.tt, updates(0)(1)) :: values
      else List.of(values.stdlib ++ (updates.map { (key, value) => (t"$label.$key", value) }))

    new Query(values2)


  def at[value: Decodable in Text](name: Text): Optional[value] = apply(name)().let(_.as)
  def as[value: Decodable in Query]: value tracks Pointer = value.decoded(this)

  def apply(): Optional[Text] = values match
    case List((t"", value)) => value
    case other              => Unset

  def apply(label: Text): Query =
    val prefix = label+t"."

    Query:
      List.of:
        values.stdlib.collect:
          case (`label`, value)                   => (t"", value)
          case (key, value) if key.starts(prefix) => (key.skip(prefix.length), value)

  def prefix(string: Text): Query = Query:
    values.map: (key, value) =>
      if key.length == 0 then string -> value else t"$string.$key" -> value

  def queryString: Text =
    values.map: (key, value) =>
      if key.length == 0 then value.urlEncode else t"${key.urlEncode}=${value.urlEncode}"

    . join(t"&")
