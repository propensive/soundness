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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package jacinta

import language.dynamics
import language.experimental.pureFunctions

import scala.collection.Factory
import scala.compiletime.*
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import hieroglyph.*
import merino.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import JsonError.Reason

trait Json2:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
        => (encodable: inner is Encodable in Json)
        =>  value is Encodable in Json =
    new Encodable:
      type Self = Optional[value]
      type Form = Json

      def encoded(value: Optional[value]): Json =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Json.ast(JsonAst(Unset)))

  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[JsonError]
        => (decodable: => inner is Decodable in Json)
        =>  value is Decodable in Json = json =>
    if json.root.isAbsent then Unset else decodable.decoded(json)

  given bytes: Tactic[JsonError] => Bytes is Decodable in Json = json => json.root.long.b

  inline given decodable: [value] => value is Decodable in Json = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[JsonError]](_.root.string.decode[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Json = summonFrom:
    case given (`value` is Encodable in Text) => value => Json.ast(JsonAst(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Json]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Decodable in Json =
      json =>
        provide[Foci[JsonPointer]]:
          provide[Tactic[JsonError]]:
            val keyValues = json.root.obj
            val values = keyValues(0).zip(keyValues(1)).to(Map)

            construct: [field] =>
              context =>
                focus(prior.or(JsonPointer()) / label):
                  if !values.contains(label.s)
                  then default().or(context.decoded(new Json(JsonAst(Unset))))
                  else context.decoded(new Json(values(label.s)))

    inline def split[derivation: SumReflection]: derivation is Decodable in Json =
      json =>
        provide[Tactic[JsonError]]:
          provide[Tactic[VariantError]]:
            val values = json.root.obj

            values(0).indexOf("_type") match
              case -1 =>
                focus(prior.or(JsonPointer()) / t"_type"):
                  abort(JsonError(Reason.Absent))

              case index =>
                delegate(values(1)(index).string): [variant <: derivation] =>
                  context =>
                    // The cast became necessary when upgrading to Scala 3.7.1.
                    context.decoded(json).asInstanceOf[derivation]

  object EncodableDerivation extends Derivable[Encodable in Json]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Encodable in Json =
      value =>
        provide[Foci[JsonPointer]]:
          val labels: scm.ArrayBuffer[String] = scm.ArrayBuffer()
          val values: scm.ArrayBuffer[JsonAst] = scm.ArrayBuffer()

          fields(value): [field] =>
            field => focus(prior.or(JsonPointer()) / label):
              context.encode(field).root.tap: encoded =>
                if !encoded.isAbsent then
                  labels += label.s
                  values += encoded

          Json.ast
           (JsonAst
             ((labels.toArray.immutable(using Unsafe), values.toArray.immutable(using Unsafe))))

    inline def split[derivation: SumReflection]: derivation is Encodable in Json = value =>
      variant(value): [variant <: derivation] =>
        value =>
          Json.ast:
            context.encode(value).root.asMatchable.absolve match
              case (labels, values) => labels.asMatchable.absolve match
                case labels: IArray[String] @unchecked => values.asMatchable.absolve match
                  case values: IArray[JsonAst] @unchecked =>
                    JsonAst((("_type" +: labels), (label.asInstanceOf[JsonAst] +: values)))

object Json extends Json2, Dynamic:
  def ast(value: JsonAst): Json = new Json(value)

  given boolean: Json is Decodable in Json = identity(_)

  given boolean: Tactic[JsonError] => Boolean is Decodable in Json =
    value => value.root.boolean

  given double: Tactic[JsonError] => Double is Decodable in Json =
    value => value.root.double

  given float: Tactic[JsonError] => Float is Decodable in Json =
    value => value.root.double.toFloat

  given long: Tactic[JsonError] => Long is Decodable in Json = _.root.long
  given int: Tactic[JsonError] => Int is Decodable in Json = _.root.long.toInt
  given ordinal: Tactic[JsonError] => Ordinal is Decodable in Json = _.root.long.toInt.z
  given text: Tactic[JsonError] => Text is Decodable in Json = _.root.string

  given string: Tactic[JsonError] => String is Decodable in Json =
    value => value.root.string.s

  given option: [value: Decodable in Json] => Tactic[JsonError]
        =>  Option[value] is Decodable in Json =

    json => if json.root.isAbsent then None else Some(value.decoded(json))

  given optionEncodable: [value] => (encodable: value is Encodable in Json)
        =>  Option[value] is Encodable in Json =
    new Encodable:
      type Self = Option[value]
      type Form = Json

      def encoded(value: Option[value]): Json = value match
        case None        => Json.ast(JsonAst(Unset))
        case Some(value) => encodable.encode(value)

  given integralEncodable: [integral: Integral] => integral is Encodable in Json =
    int => Json.ast(JsonAst(integral.toLong(int)))

  given textEncodableInJson: Text is Encodable in Json = text => Json.ast(JsonAst(text.s))
  given stringEncodable: String is Encodable in Json = string => Json.ast(JsonAst(string))
  given doubleEncodable: Double is Encodable in Json = double => Json.ast(JsonAst(double))
  given intEncodable: Int is Encodable in Json = int => Json.ast(JsonAst(int.toLong))

  given ordinalEncodable: Ordinal is Encodable in Json =
    ordinal => Json.ast(JsonAst(ordinal.n0.toLong))

  given longEncodable: Long is Encodable in Json = long => Json.ast(JsonAst(long))
  given booleanEncodable: Boolean is Encodable in Json = boolean => Json.ast(JsonAst(boolean))
  given jsonEncodable: Json is Encodable in Json = identity(_)

  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Json)
        =>  list[element] is Encodable in Json =
    values => Json.ast(JsonAst(IArray.from(values.map(encodable.encoded(_).root))))

  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Json)
        =>  set[element] is Encodable in Json =
    values => Json.ast(JsonAst(IArray.from(values.map(encodable.encoded(_).root))))

  given trieEncodable: [trie <: Trie, element] => (encodable: => element is Encodable in Json)
        =>  trie[element] is Encodable in Json =
    values => Json.ast(JsonAst(IArray.from(values.map(encodable.encoded(_).root))))

  given array: [collection <: Iterable, element]
        => (factory:    Factory[element, collection[element]],
            jsonAccess: Tactic[JsonError],
            foci:       Foci[JsonPointer])
        => (decodable: => element is Decodable in Json)
        =>  collection[element] is Decodable in Json =
    value =>
      val builder = factory.newBuilder
      value.root.array.each: json =>
        focus(prior.or(JsonPointer()) / ordinal)
         (builder += decodable.decoded(Json.ast(json)))

      builder.result()

  given map: [key: Decodable in Text, element]
        => (decodable: => element is Decodable in Json)
        =>  Tactic[JsonError]
        =>  Map[key, element] is Decodable in Json =

    value =>
      val (keys, values) = value.root.obj

      keys.indices.fuse(Map[key, element]()):
        focus(prior.or(JsonPointer()) / keys(next).tt):
          state.updated(keys(next).tt.decode, decodable.decoded(Json.ast(values(next))))

  given mapEncodable: [key: Encodable in Text, element]
        => (encodable: element is Encodable in Json)
        => Map[key, element] is Encodable in Json =
    map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Json.ast(JsonAst((IArray.from(keys.map(_.encode.s)), values)))

  given jsonEncodableInText: Json is Encodable in Text = json => JsonPrinter.print(json.root, false)

  given aggregable: Tactic[ParseError] => Json is Aggregable by Data =
    bytes => Json(bytes.read[JsonAst])

  given aggregableDirect: [value: Decodable in Json] => Tactic[ParseError] => Tactic[JsonError]
        => (value over Json) is Aggregable by Data =
    bytes => Json(bytes.read[JsonAst]).as[value].asInstanceOf[value over Json]

  given showable: JsonPrinter => Json is Showable = json =>
    try json.root.show catch case err: JsonError => t"<${err.reason.show}>"

  given abstractable: (encoder: CharEncoder, printer: JsonPrinter)
        =>  Json is Abstractable across HttpStreams to HttpStreams.Content =
    new Abstractable:
      type Self = Json
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(json: Json): HttpStreams.Content =
        (t"application/json; charset=${encoder.encoding.name}", Stream(json.show.data))

  given decodable: Tactic[ParseError] => Json is Decodable in Text =
    text => Stream(text.data(using charEncoders.utf8)).read[Json]

  given instantiable: Tactic[ParseError] => Json is Instantiable across HttpRequests from Text =
    text => Stream(text.data(using charEncoders.utf8)).read[Json]

  def applyDynamicNamed(methodName: "of")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst((keys, values)))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(index: Int): Json raises JsonError = Json(root.array(index))

  def selectDynamic(field: String)(using erased DynamicJsonEnabler): Json raises JsonError =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler)
  : Json raises JsonError =

      apply(field.tt)(index)


  def apply(field: Text): Json raises JsonError =
    root.obj(0).indexWhere(_ == field.s) match
      case -1    => focus(prior.or(JsonPointer()) / field)(raise(JsonError(Reason.Absent)) yet this)
      case index => Json(root.obj(1)(index))

  override def hashCode: Int =
    def recur(value: JsonAst): Int = value.asMatchable match
      case value: Long       => value.hashCode
      case value: Double     => value.hashCode
      case value: BigDecimal => value.hashCode
      case value: String     => value.hashCode
      case value: Boolean    => value.hashCode

      case value: IArray[JsonAst] @unchecked =>
        value.fuse(value.length.hashCode)(state*31^recur(next))

      case (keys, values) => keys.asMatchable.absolve match
        case keys: IArray[String] @unchecked => values.asMatchable.absolve match
          case values: IArray[JsonAst] @unchecked =>
            keys.zip(values).to(Map).view.mapValues(recur(_)).hashCode

      case _ =>
        0

    recur(root)

  override def equals(right: Any): Boolean = right.asMatchable match
    case right: Json =>
      def recur(left: JsonAst, right: JsonAst): Boolean = right.asMatchable match
        case right: Long     => left.asMatchable match
          case left: Long       => left == right
          case left: Double     => left == right
          case left: BigDecimal => left == BigDecimal(right)
          case _             => false

        case right: Double => left.asMatchable match
          case left: Long       => left == right
          case left: Double     => left == right
          case left: BigDecimal => left == BigDecimal(right)
          case _             => false

        case right: BigDecimal => left.asMatchable match
          case left: Long       => BigDecimal(left) == right
          case left: Double     => BigDecimal(left) == right
          case left: BigDecimal => left == right
          case _             => false

        case right: String => left.asMatchable match
          case left: String => left == right
          case _         => false

        case right: Boolean => left.asMatchable match
          case left: Boolean => left == right
          case _         => false

        case right: IArray[JsonAst] @unchecked => left.asMatchable match
          case left: IArray[JsonAst] @unchecked =>
            right.length == left.length && right.indices.all: index =>
              recur(left(index), right(index))

          case _ =>
            false

        case (rightKeys, rightValues) => rightKeys.asMatchable.absolve match
           case rightKeys: IArray[String] @unchecked => rightValues.asMatchable.absolve match
            case rightValues: IArray[JsonAst] @unchecked => left.asMatchable.absolve match
              case (leftKeys, leftValues) => leftKeys.asMatchable.absolve match
                case leftKeys: IArray[String] @unchecked =>
                  leftValues.asMatchable.absolve match
                    case leftValues: IArray[JsonAst] @unchecked =>
                      val leftMap = leftKeys.zip(leftValues).to(Map)
                      val rightMap = rightKeys.zip(rightValues).to(Map)

                      leftMap.keySet == rightMap.keySet && leftMap.keySet.all: key =>
                        recur(leftMap(key), rightMap(key))

              case _ =>
                false

        case _ =>
          false

      recur(root, right.root)

    case _ =>
      false

  def as[value: Decodable in Json]: value raises JsonError tracks JsonPointer =
    value.decoded(this)
