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
package legerdemain

import language.dynamics

import scala.compiletime.*

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
  def apply(): Query = new Query(Nil)
  def apply[ValueType: Encodable in Query](value: ValueType): Query = value.encode

  given encodable: Query is Encodable in Text =
    _.values.map { (key, value) => t"${key.urlEncode}=${value.urlEncode}" }
    . join(t"&")

  given decodable: Query is Decodable in Text = text => Query.of:
    text.cut(t"&").map: next =>
      next.cut(t"=", 2) match
        case List(key, value) => (key.urlDecode, value.urlDecode)
        case List(key)        => (key.urlDecode, t"")
        case _                => (t"", t"")

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

      summonInline[Foci[Text]].give:
        value =>
          construct:
            [FieldType] => context =>
              focus(prior.lay(label) { suffix => t"$label.$suffix" }):
                context.decoded(value(label))

  given booleanEncodable: Boolean is Encodable in Query =
    boolean => if boolean then Query.of(t"on") else Query()

  given booleanDecodable: Boolean is Decodable in Query = _().present

  given booleanEncodable: Boolean is Encodable in Query =
    boolean => if boolean then Query.of(t"on") else Query()

  given booleanDecodable: Boolean is Decodable in Query = _().present

  inline given encodable: [ValueType] => ValueType is Encodable in Query = summonFrom:
    case given (ValueType is Encodable in Text) =>
      value => Query.of(value.encode)

    case given ProductReflection[ValueType & Product] =>
      EncodableDerivation.join[ValueType & Product].asInstanceOf[ValueType is Encodable in Query]

  given Query is Showable = _.values.map { case (key, value) => t"$key = \"${value}\"" }.join(t", ")

  inline given decodable: [ValueType] => ValueType is Decodable in Query =
    summonFrom:
      case given (ValueType is Decodable in Text) =>
        summonInline[Tactic[QueryError]].give:
          summonFrom:
            case given Default[ValueType] =>
              _().let(_.decode).or(raise(QueryError()) yet default[ValueType])

            case _ =>
              _().lest(QueryError()).decode

      case given ProductReflection[ValueType & Product] =>
        DecodableDerivation.join[ValueType & Product].asInstanceOf[ValueType is Decodable in Query]

  inline def applyDynamicNamed(method: "apply")(inline parameters: (Label, Any)*): Query =
    ${Legerdemain.query('parameters)}

  def of(parameters: List[(Text, Text)]): Query = new Query(parameters)
  def of(parameter: Text): Query = new Query(List(t"" -> parameter))

case class Query private (values: List[(Text, Text)]) extends Dynamic:
  private lazy val map: Map[Text, Text | List[Text]] = values.groupMap(_(0))(_(1))
  def append(more: Query): Query = new Query(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty

  @targetName("appendAll")
  infix def ++ (query: Query) = Query(values ++ query.values)

  def selectDynamic[ResultType](label: String)
     (using erased (label.type is Parametric into ResultType))
     (using decodable: ResultType is Decodable in Query)
  :     ResultType =
    decodable.decoded(apply(label.tt))

  def at[ValueType: Decodable in Text](name: Text): Optional[Text] = apply(name)().let(_.decode)
  def as[ValueType: Decodable in Query]: ValueType tracks Text = ValueType.decoded(this)

  def apply(): Optional[Text] = values match
    case List((t"", value)) => value
    case other              => Unset

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
