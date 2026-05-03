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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import scala.collection.mutable as scm
import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import hieroglyph.*
import merino.*
import panopticon.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import JsonError.Reason

trait Json2:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Json )
  =>  value is Encodable in Json =

    new Encodable:
      type Self = Optional[value]
      type Form = Json

      def encoded(value: Optional[value]): Json =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Json.ast(JsonAst(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[JsonError]
  =>  ( decodable: => inner is Decodable in Json )
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
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Json =
      json =>
        provide[Foci[JsonPointer]]:
          provide[Tactic[JsonError]]:
            val root = json.root
            val n = root.objectSize
            val values = scm.HashMap.empty[String, JsonAst]
            var i = 0
            while i < n do
              values.update(root.objectKey(i), root.objectValue(i))
              i += 1

            build: [field] =>
              context =>
                focus(prior.or(JsonPointer()) / label):
                  values.get(label.s) match
                    case Some(value) => context.decoded(new Json(value))
                    case None        => default.or(context.decoded(new Json(JsonAst(Unset))))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Json =
      json =>
        provide[Tactic[JsonError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Json]

            val discriminant: Text = discriminable.discriminate(json).or:
              focus(prior.or(JsonPointer()))(abort(JsonError(Reason.Absent)))

            delegate(discriminant): [variant <: derivation] =>
              context => context.decoded(json)

  object EncodableDerivation extends Derivable[Encodable in Json]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Json =

      value =>
        provide[Foci[JsonPointer]]:
          val labels: scm.ArrayBuffer[String] = scm.ArrayBuffer()
          val values: scm.ArrayBuffer[JsonAst] = scm.ArrayBuffer()

          fields(value): [field] =>
            field => focus(prior.or(JsonPointer()) / label):
              contextual.encode(field).root.tap: encoded =>
                if !encoded.isAbsent then
                  labels += label.s
                  values += encoded

          Json.ast
            ( JsonAst.obj
                ( unsafely(labels.toArray.immutable), unsafely(values.toArray.immutable) ) )

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Json = value =>
      val discriminable = infer[derivation is Discriminable in Json]

      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(label, contextual.encode(value))

object Json extends Json2, Dynamic:
  def ast(value: JsonAst): Json = new Json(value)


  inline given interpolator: Json is Interpolable:
    type Result = Json

    transparent inline def interpolate[parts <: Tuple](inline insertions: Any*): Json =
      ${jacinta.internal.interpolator[parts]('insertions)}


  inline given extrapolator: Json is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple](scrutinee: Json)
    :   Boolean | Option[Tuple | Json] =

      ${jacinta.internal.extractor[parts]('scrutinee)}


  given lens: [name <: Label: ValueOf] => (erased DynamicJsonEnabler) => Tactic[JsonError]
  =>  name is Lens from Json onto Json =

    Lens(_.selectDynamic(valueOf[name]), _.modify(valueOf[name], _))


  given ordinalOptical: [element] => Ordinal is Optical from Json onto Json =
    ordinal =>
      Optic: (origin, lambda) =>
        if origin.root.isArray then
          val n = origin.root.arrayLength
          if n <= ordinal.n0 then origin else Json.ast:
            val updated = new Array[Any](n)
            var i = 0
            while i < n do
              updated(i) =
                if i == ordinal.n0
                then lambda(Json.ast(origin.root.arrayElement(i))).root
                else origin.root.arrayElement(i)
              i += 1
            JsonAst.arr(updated.asInstanceOf[IArray[Any]])
        else origin

  given boolean: Json is Decodable in Json = identity(_)
  given boolean: Tactic[JsonError] => Boolean is Decodable in Json = _.root.boolean
  given double: Tactic[JsonError] => Double is Decodable in Json = _.root.double
  given float: Tactic[JsonError] => Float is Decodable in Json = _.root.double.toFloat
  given long: Tactic[JsonError] => Long is Decodable in Json = _.root.long
  given int: Tactic[JsonError] => Int is Decodable in Json = _.root.long.toInt
  given ordinalDecodable: Tactic[JsonError] => Ordinal is Decodable in Json = _.root.long.toInt.z
  given text: Tactic[JsonError] => Text is Decodable in Json = _.root.string
  given string: Tactic[JsonError] => String is Decodable in Json = _.root.string.s

  given unit: Tactic[JsonError] => Unit is Decodable in Json =
    value =>
      if value.root.isNull then ()
      else
        val reason =
          if value.root.isAbsent then Reason.Absent
          else Reason.NotType(value.root.primitive, JsonPrimitive.Null)

        raise(JsonError(reason))


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
  given unitEncodable: Unit is Encodable in Json = unit => Json.ast(JsonAst(null))

  given ordinalEncodable: Ordinal is Encodable in Json =
    ordinal => Json.ast(JsonAst(ordinal.n0.toLong))

  given longEncodable: Long is Encodable in Json = long => Json.ast(JsonAst(long))
  given booleanEncodable: Boolean is Encodable in Json = boolean => Json.ast(JsonAst(boolean))
  given jsonEncodable: Json is Encodable in Json = identity(_)


  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Json)
  =>  list[element] is Encodable in Json =

    values => Json.ast(JsonAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Json)
  =>  set[element] is Encodable in Json =

    values => Json.ast(JsonAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given trieEncodable: [trie <: Trie, element] => (encodable: => element is Encodable in Json)
  =>  trie[element] is Encodable in Json =

    values => Json.ast(JsonAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given array: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[JsonError],
        foci:    Foci[JsonPointer] )
  =>  ( decodable: => element is Decodable in Json )
  =>  collection[element] is Decodable in Json =

    value =>
      val builder = factory.newBuilder
      value.root.array.each: json =>
        focus(prior.or(JsonPointer()) / ordinal):
          builder += decodable.decoded(Json.ast(json))

      builder.result()


  given map: [key: Decodable in Text, element] => (decodable: => element is Decodable in Json)
  =>  Tactic[JsonError]
  =>  Map[key, element] is Decodable in Json =

    value =>
      val root = value.root
      val n = root.objectSize
      var i = 0
      var acc = Map.empty[key, element]
      while i < n do
        acc = acc.updated
                  ( root.objectKey(i).tt.decode,
                    decodable.decoded(Json.ast(root.objectValue(i))) )
        i += 1
      acc


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Json )
  =>  Map[key, element] is Encodable in Json =

    map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Json.ast(JsonAst.obj(IArray.from(keys.map(_.encode.s)), values))


  given jsonEncodableInText: Json is Encodable in Text = json => JsonPrinter.print(json.root, false)

  given aggregable: Tactic[ParseError] => Json is Aggregable by Data =
    bytes => Json(bytes.read[JsonAst])


  given aggregableDirect: [value: Decodable in Json] => Tactic[ParseError] => Tactic[JsonError]
  =>  (value over Json) is Aggregable by Data =

    bytes => Json(bytes.read[JsonAst]).as[value].asInstanceOf[value over Json]


  given showable: JsonPrinter => Json is Showable = _.root.show


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

  def applyDynamicNamed(methodName: "make")(elements: (String, Json)*): Json =
    val keys: IArray[String] = IArray.from(elements.map(_(0)))
    val values: IArray[JsonAst] = IArray.from(elements.map(_(1).root))
    Json(JsonAst.obj(keys, values.asInstanceOf[IArray[Any]]))

  def discriminatedUnion[value](label: Text): value is Discriminable in Json = new Discriminable:
    type Form = Json
    type Self = value

    protected def key: String = label.s

    import dynamicJsonAccess.enabled

    def rewrite(kind: Text, json: Json): Json = unsafely(json.updateDynamic(key)(kind))
    def discriminate(json: Json): Optional[Text] = safely(json.selectDynamic(key).as[Text])
    def variant(json: Json): Json = unsafely(json.updateDynamic(key)(Unset))

class Json(rootValue: Any) extends Dynamic derives CanEqual:
  def root: JsonAst = rootValue.asInstanceOf[JsonAst]
  def apply(index: Int): Json raises JsonError = Json(root.array(index))

  def selectDynamic(field: String)(using erased DynamicJsonEnabler): Json raises JsonError =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    apply(field.tt)(index)


  def update[value: Encodable in Json](index: Int, value: value)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    if !root.isArray then raise(JsonError(Reason.NotType(root.primitive, JsonPrimitive.Array)))
    val n = root.arrayLength
    val updated = new Array[Any](n)
    var i = 0
    while i < n do
      updated(i) =
        if i == index then value.encode.root
        else root.arrayElement(i)
      i += 1
    Json.ast(JsonAst.arr(updated.asInstanceOf[IArray[Any]]))


  def updateDynamic(field: String)[value: Encodable in Json](value: value)
    ( using erased DynamicJsonEnabler )
  :   Json raises JsonError =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased DynamicJsonEnabler)
  :   Json raises JsonError =

    delete(field)


  private[jacinta] def modify(field: String, value: Json): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    val n = len/2
    root.objectIndexOf(field) match
      case -1 =>
        val out = new Array[Any](len + 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(len) = field
        out(len + 1) = value.root
        Json.ast(JsonAst(out.asInstanceOf[IArray[Any]]))

      case index =>
        val out = new Array[Any](len)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(index*2 + 1) = value.root
        Json.ast(JsonAst(out.asInstanceOf[IArray[Any]]))

  private[jacinta] def delete(field: String): Json raises JsonError =
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    root.objectIndexOf(field) match
      case -1 =>
        Json.ast(root)

      case index =>
        val out = new Array[Any](len - 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)
        System.arraycopy
                ( arr.asInstanceOf[Array[Any]],
                  index*2 + 2,
                  out,
                  index*2,
                  len - index*2 - 2 )
        Json.ast(JsonAst(out.asInstanceOf[IArray[Any]]))

  def apply(field: Text): Json raises JsonError =
    if root.isAbsent then Json.ast(JsonAst(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => Json.ast(JsonAst(Unset))
      case index => Json(root.objectValue(index))

  def unsafeApply(field: Text): Json =
    if root.isAbsent then Json.ast(JsonAst(Unset))
    else root.objectIndexOf(field.s) match
      case -1    => Json.ast(JsonAst(Unset))
      case index => Json(root.objectValue(index))

  override def hashCode: Int =
    def recur(value: JsonAst): Int = value.asMatchable match
      case value: Long       => value.hashCode
      case value: Double     => value.hashCode
      case value: BigDecimal => value.hashCode
      case value: String     => value.hashCode
      case value: Boolean    => value.hashCode

      case value: IArray[Any] @unchecked =>
        if value.isObject then
          val n = value.objectSize
          var acc = Map.empty[String, Int]
          var i = 0
          while i < n do
            acc = acc.updated(value.objectKey(i), recur(value.objectValue(i)))
            i += 1
          acc.hashCode
        else
          val n = value.arrayLength
          var acc = n.hashCode
          var i = 0
          while i < n do
            acc = acc*31 ^ recur(value.arrayElement(i))
            i += 1
          acc

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
          case _                => false

        case right: Double => left.asMatchable match
          case left: Long       => left == right
          case left: Double     => left == right
          case left: BigDecimal => left == BigDecimal(right)
          case _                => false

        case right: BigDecimal => left.asMatchable match
          case left: Long       => BigDecimal(left) == right
          case left: Double     => BigDecimal(left) == right
          case left: BigDecimal => left == right
          case _                => false

        case right: String => left.asMatchable match
          case left: String => left == right
          case _            => false

        case right: Boolean => left.asMatchable match
          case left: Boolean => left == right
          case _             => false

        case right: IArray[Any] @unchecked => left.asMatchable match
          case left: IArray[Any] @unchecked =>
            (right.isObject, left.isObject) match
              case (false, false) =>
                val rn = right.arrayLength
                val ln = left.arrayLength
                rn == ln && (0 until rn).forall: index =>
                  recur(left.arrayElement(index), right.arrayElement(index))

              case (true, true) =>
                val rn = right.objectSize
                val ln = left.objectSize
                if rn != ln then false
                else
                  var leftMap = Map.empty[String, JsonAst]
                  var i = 0
                  while i < ln do
                    leftMap = leftMap.updated(left.objectKey(i), left.objectValue(i))
                    i += 1
                  var rightMap = Map.empty[String, JsonAst]
                  i = 0
                  while i < rn do
                    rightMap = rightMap.updated(right.objectKey(i), right.objectValue(i))
                    i += 1
                  leftMap.keySet == rightMap.keySet && leftMap.keySet.forall: key =>
                    recur(leftMap(key), rightMap(key))

              case _ => false

          case _ => false

        case _ =>
          false

      recur(root, right.root)

    case _ =>
      false

  def as[value: Decodable in Json]: value raises JsonError tracks JsonPointer =
    value.decoded(this)
