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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package telekinesis

import language.dynamics

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

object Query extends Dynamic:
  object EncodableDerivation extends ProductDerivation[[Type] =>> Type is Encodable in Query]:
    inline def join[DerivationType <: Product: ProductReflection]
    :     DerivationType is Encodable in Query =

      value =>
        Query.of:
          fields(value) { [FieldType] => field => context.encoded(field).prefix(label) }
          . to(List)
          . flatMap(_.values)

  object DecodableDerivation extends ProductDerivation[[Type] =>> Type is Decodable in Query]:
      inline def join[DerivationType <: Product: ProductReflection]
      :     DerivationType is Decodable in Query =

        value =>
          construct:
            [FieldType] => decodable => decodable.decoded(value(label))

  inline given encodable: [ValueType] => ValueType is Encodable in Query = compiletime.summonFrom:
    case given (ValueType is Encodable in Text) =>
      value => Query.of(value.encode)

    case given ProductReflection[ValueType & Product] =>
      EncodableDerivation.join[ValueType & Product].asInstanceOf[ValueType is Encodable in Query]

  inline given textDecodable: [ValueType: Decodable in Text] => Tactic[QueryError]
  =>    ValueType is Decodable in Query =
    compiletime.summonFrom:
      case given Default[ValueType] =>
        _.values match
          case List((t"", value)) => value.decode
          case _                  => raise(QueryError()) yet default[ValueType]

      case _ =>
        _.values match
          case List((t"", value)) => value.decode
          case _                  => abort(QueryError())

  given Query is Showable = _.queryString

  inline given decodable: [ProductType <: Product: ProductReflection]
  =>    ProductType is Decodable in Query =

    DecodableDerivation.join[ProductType]

  inline def applyDynamicNamed(method: "apply")(inline parameters: (Label, Any)*): Query =
    ${Telekinesis.query('parameters)}

  def of(parameters: List[(Text, Text)]): Query = new Query(parameters)
  def of(parameter: Text): Query = new Query(List(t"" -> parameter))

case class Query private (val values: List[(Text, Text)]) extends Dynamic:
  def append(more: Query): Query = new Query(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty

  @targetName("appendAll")
  infix def ++ (query: Query) = Query(values ++ query.values)

  def selectDynamic(label: String): Query = apply(label.tt)

  def apply(label: Text): Query =
    val prefix = label+t"."
    Query:
      values.collect:
        case (`label`, value)                   => (t"", value)
        case (key, value) if key.starts(prefix) => (key.skip(prefix.length), value)

  def prefix(str: Text): Query = Query:
    values.map { (key, value) => if key.length == 0 then str -> value else t"$str.$key" -> value }

  def queryString: Text =
    values.map: (key, value) =>
      if key.length == 0 then value.urlEncode else t"${key.urlEncode}=${value.urlEncode}"

    . join(t"&")
