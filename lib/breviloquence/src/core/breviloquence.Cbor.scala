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
package breviloquence

import language.dynamics
import language.experimental.pureFunctions

import scala.collection as sc
import scala.collection.mutable as scm
import scala.compiletime.*

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import panopticon.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import wisteria.*

import CborError.{Primitive, Reason}

trait Cbor2:
  this: Cbor.type =>
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Cbor )
  =>  value is Encodable in Cbor =

    new Encodable:
      type Self = Optional[value]
      type Form = Cbor

      def encoded(value: Optional[value]): Cbor =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(ast(Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[CborError]
  =>  ( decodable: => inner is Decodable in Cbor )
  =>  value is Decodable in Cbor = cbor =>

    if cbor.root.unset then Unset else decodable.decoded(cbor)


  inline given decodable: [value] => value is Decodable in Cbor = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[CborError]](_.root.string.tt.decode[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Cbor = summonFrom:
    case given (`value` is Encodable in Text) => value => ast(Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived


  object DecodableDerivation extends Derivable[Decodable in Cbor]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Cbor =

      cbor =>
        provide[Tactic[CborError]]:
          val root = cbor.root
          val count = if root.isMap then root.entries else 0
          val values = scala.collection.mutable.HashMap.empty[String, Ast]
          var index = 0

          while index < count do
            val key = root.key(index)
            if key.isTextString then values.update(key.string, root.value(index))
            index += 1

          // `@name[Cbor]` / bare `@name` renames: field name -> map key, read
          // back the same way they are written.
          val renames: Map[Text, Text] = relabelling[derivation, Cbor]

          build: [field] =>
            context =>
              val key: Text = renames.at(label).or(label)

              values.get(key.s) match
                case Some(value) => context.decoded(new Cbor(value))
                case None        => default.or(context.decoded(new Cbor(Ast(Unset))))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Cbor =
      cbor =>
        provide[Tactic[CborError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Cbor]

            // `@name[Cbor]` / bare `@name` variant renames: map the serialized
            // discriminator back to the variant name before delegating.
            val variantNames: Map[Text, Text] =
              variantRelabelling[derivation, Cbor].map((variant, wire) => wire -> variant)

            val wire: Text =
              discriminable.discriminate(cbor).lest(CborError(Reason.Absent))

            val discriminant: Text = variantNames.getOrElse(wire, wire)

            delegate(discriminant): [variant <: derivation] =>
              context => context.decoded(cbor)

  object EncodableDerivation extends Derivable[Encodable in Cbor]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Cbor =

      // `@name[Cbor]` / bare `@name` renames: field name -> map key.
      val mapping: Map[Text, Text] = relabelling[derivation, Cbor]

      value =>
        val labels: scm.ArrayBuffer[Any] = scm.ArrayBuffer()
        val values: scm.ArrayBuffer[Any] = scm.ArrayBuffer()

        fields(value): [field] =>
          field =>
            val encoded = contextual.encode(field).root

            if !encoded.unset then
              labels += mapping.at(label).or(label).s
              values += encoded

        ast(Ast.map(IArray.from(labels), IArray.from(values)))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Cbor = value =>
      val discriminable = infer[derivation is Discriminable in Cbor]

      // `@name[Cbor]` / bare `@name` variant renames: variant name -> wire
      // discriminator, read back the same way by the decoder.
      val variantNames: Map[Text, Text] = variantRelabelling[derivation, Cbor]

      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

object Cbor extends Cbor2, Dynamic:
  // CBOR major-type representation in storage. Arrays are stored as an
  // odd-length `IArray[Any]` (sentinel-padded if the logical count is even),
  // and maps as an even-length `IArray[Any]` with alternating key/value
  // entries; the two share the same JVM type and are told apart by parity.
  type CborInteger   = Long
  type CborFloat     = Double
  type CborText      = String
  type CborBytes     = IArray[Byte]
  type CborArray     = IArray[Any]
  type CborMap       = IArray[Any]
  type CborBoolean   = Boolean
  type CborNull      = Null
  type CborUndefined = vacuous.Unset.type

  type CborTypes =
    CborInteger | CborFloat | CborText | CborBytes | CborArray | CborMap | CborBoolean | CborNull
    | CborUndefined | Tag

  opaque type Ast = CborTypes

  object Ast:
    val Sentinel: AnyRef = new Object

    // Reinterpret a pre-boxed reference as `Ast` without unbox/rebox. Safe
    // because `Ast` is an opaque union whose erasure is `Object`. Useful for
    // callers that already hold a cached `java.lang.Long`, `String`, etc. and
    // want to avoid an auto-boxing round-trip through `apply`.
    private[breviloquence] inline def fromRef(value: AnyRef): Ast =
      value.asInstanceOf[Ast]

    def apply(value: CborTypes): Ast = value

    def map(keys: IArray[Any], values: IArray[Any]): Ast =
      val count = keys.length
      val array = new Array[Any](count*2)
      var index = 0

      while index < count do
        array(index*2) = keys(index)
        array(index*2 + 1) = values(index)
        index += 1

      array.asInstanceOf[IArray[Any]]

    def array(elements: IArray[Any]): Ast =
      val count = elements.length

      if (count&1) == 1 then elements else
        val padded = new Array[Any](count + 1)
        System.arraycopy(elements.asInstanceOf[Array[Any]], 0, padded, 0, count)
        padded(count) = Sentinel
        padded.asInstanceOf[IArray[Any]]

    def length(cbor: Ast): Int =
      val array = cbor.asInstanceOf[Array[AnyRef]]
      val count = array.length
      if count > 0 && (array(count - 1).asInstanceOf[AnyRef] eq Sentinel) then count - 1 else count

    def size(cbor: Ast): Int = cbor.asInstanceOf[IArray[Any]].length/2

  final class Tag(val tag: Long, val value: Any):
    override def hashCode: Int = (tag.hashCode*31)^value.hashCode

    override def equals(that: Any): Boolean = that match
      case that: Tag => tag == that.tag && value == that.value
      case _         => false

  def ast(value: Ast): Cbor = new Cbor(value)
  def unseal(cbor: Cbor): Ast = cbor.root

  // Panopticon optics: navigate and immutably update a CBOR document. `lens` is the
  // map-key (object-field) lens; `ordinalOptical` indexes an array; `eachOptical`
  // and `filterOptical` traverse every (or matching) array element. All reuse the
  // existing `selectDynamic`/`modify`/`element`/`Ast.array` primitives and rebuild
  // immutably. Mirrors jacinta's `Json` optics.
  given lens: [name <: Label: ValueOf] => (erased DynamicCborEnabler) => Tactic[CborError]
  =>  name is Lens from Cbor onto Cbor =
    Lens(_.selectDynamic(valueOf[name]), _.modify(valueOf[name], _))

  given ordinalOptical: [element] => Ordinal is Optical from Cbor onto Cbor = ordinal =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        if n <= ordinal.n0 then origin else Cbor.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            updated(i) =
              if i == ordinal.n0 then lambda(Cbor.ast(origin.root.element(i))).root
              else origin.root.element(i)

            i += 1

          Cbor.Ast.array(updated.asInstanceOf[IArray[Any]])
      else origin

  given eachOptical: Each.type is Optical from Cbor onto Cbor = _ =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        Cbor.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            updated(i) = lambda(Cbor.ast(origin.root.element(i))).root
            i += 1

          Cbor.Ast.array(updated.asInstanceOf[IArray[Any]])
      else origin

  given filterOptical: Filter[Cbor] is Optical from Cbor onto Cbor = filter =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        Cbor.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            val element = Cbor.ast(origin.root.element(i))
            updated(i) = (if filter.predicate(element) then lambda(element) else element).root
            i += 1

          Cbor.Ast.array(updated.asInstanceOf[IArray[Any]])
      else origin

  given boolean: Tactic[CborError] => Boolean is Decodable in Cbor = _.root.boolean
  given double: Tactic[CborError] => Double is Decodable in Cbor = _.root.double
  given float: Tactic[CborError] => Float is Decodable in Cbor = _.root.double.toFloat
  given long: Tactic[CborError] => Long is Decodable in Cbor = _.root.long
  given int: Tactic[CborError] => Int is Decodable in Cbor = _.root.long.toInt
  given text: Tactic[CborError] => Text is Decodable in Cbor = _.root.string.tt
  given string: Tactic[CborError] => String is Decodable in Cbor = _.root.string
  given byteString: Tactic[CborError] => IArray[Byte] is Decodable in Cbor = _.root.byteString
  given cbor: Cbor is Decodable in Cbor = identity(_)

  given aggregable: Tactic[CborError] => Cbor is Aggregable by Data =
    bytes => Cbor.ast(bytes.read[Cbor.Ast])

  // HTTP content-type integration: `Abstractable across HttpStreams` makes a
  // `Cbor` value usable as an HTTP request/response body (telekinesis derives
  // `Postable`/`Servable` from it). Decoding a response body back into `Cbor`
  // is already covered by `aggregable` (`Aggregable by Data`).
  given abstractable: Cbor is Abstractable across HttpStreams to HttpStreams.Content =
    new Abstractable:
      type Self = Cbor
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(value: Cbor): HttpStreams.Content =
        (t"application/cbor", Stream(CborPrinter.encode(Cbor.unseal(value))))

  // `source.read[Foo over Cbor]` shorthand for
  // `source.read[Cbor].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect`
  // for `value over Json`. The `Transport` type-tag is added by an
  // `asInstanceOf` cast — `value over Cbor` is just
  // `value { type Transport = Cbor }` so the cast is a no-op at runtime.
  given aggregableOver: [value: Decodable in Cbor] => Tactic[CborError]
  =>  (value over Cbor) is Aggregable by Data =
    bytes => Cbor.ast(bytes.read[Cbor.Ast]).as[value].asInstanceOf[value over Cbor]

  given unit: Tactic[CborError] => Unit is Decodable in Cbor =
    value =>
      if !value.root.nullary then
        val reason =
          if value.root.unset then Reason.Absent
          else Reason.NotType(value.root.primitive, Primitive.Null)

        abort(CborError(reason))


  given option: [value: Decodable in Cbor] => Tactic[CborError]
  =>  Option[value] is Decodable in Cbor =

    cbor => if cbor.root.unset then None else Some(value.decoded(cbor))


  given optionEncodable: [value] => (encodable: value is Encodable in Cbor)
  =>  Option[value] is Encodable in Cbor =

    new Encodable:
      type Self = Option[value]
      type Form = Cbor

      def encoded(value: Option[value]): Cbor = value match
        case None        => ast(Ast(Unset))
        case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Encodable in Cbor =
    int => ast(Ast(integral.toLong(int)))

  given textEncodable: Text is Encodable in Cbor = text => ast(Ast(text.s))
  given stringEncodable: String is Encodable in Cbor = string => ast(Ast(string))
  given doubleEncodable: Double is Encodable in Cbor = double => ast(Ast(double))
  given floatEncodable: Float is Encodable in Cbor = float => ast(Ast(float.toDouble))
  given intEncodable: Int is Encodable in Cbor = int => ast(Ast(int.toLong))
  given longEncodable: Long is Encodable in Cbor = long => ast(Ast(long))
  given booleanEncodable: Boolean is Encodable in Cbor = boolean => ast(Ast(boolean))
  given unitEncodable: Unit is Encodable in Cbor = _ => ast(Ast(null))
  given bytesEncodable: IArray[Byte] is Encodable in Cbor = bytes => ast(Ast(bytes))
  given cborEncodable: Cbor is Encodable in Cbor = identity(_)


  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Cbor)
  =>  list[element] is Encodable in Cbor =

    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Cbor)
  =>  set[element] is Encodable in Cbor =

    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given seriesEncodable: [series <: Series, element] => (encodable: => element is Encodable in Cbor)
  =>  series[element] is Encodable in Cbor =

    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: sc.Factory[element, collection[element]], tactic:  Tactic[CborError] )
  =>  ( decodable: => element is Decodable in Cbor )
  =>  collection[element] is Decodable in Cbor =

    value =>
      val builder = factory.newBuilder
      value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))

      builder.result()


  given mapDecodable: [key: Decodable in Text, element]
  =>  ( decodable: => element is Decodable in Cbor )
  =>  Tactic[CborError]
  =>  Map[key, element] is Decodable in Cbor =

    value =>
      val root = value.root
      val count = if root.isMap then root.entries else 0
      var index = 0
      var map = Map.empty[key, element]

      while index < count do
        val key = root.key(index)

        if key.isTextString
        then map = map.updated(key.string.tt.decode, decodable.decoded(ast(root.value(index))))
        else abort(CborError(Reason.NonStringKey))

        index += 1

      map


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Cbor )
  =>  Map[key, element] is Encodable in Cbor =

    map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      ast(Ast.map(IArray.from(keys.map{ k => k.encode.s }), values))


  def applyDynamicNamed(methodName: "make")(elements: (String, Cbor)*): Cbor =
    val keys: IArray[Any] = IArray.from(elements.map(_(0): Any))
    val values: IArray[Any] = IArray.from(elements.map(_(1).root.asInstanceOf[Any]))
    Cbor(Ast.map(keys, values))

  def discriminatedUnion[value](label: Text): value is Discriminable in Cbor = new Discriminable:
    type Form = Cbor
    type Self = value

    protected def key: String = label.s

    import dynamicCborAccess.enabled

    def rewrite(kind: Text, cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key)(kind))
    def discriminate(cbor: Cbor): Optional[Text] = safely(cbor.selectDynamic(key).as[Text])
    def variant(cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key)(Unset))


class Cbor(private[breviloquence] val root: Cbor.Ast) extends Dynamic derives CanEqual:
  def apply(index: Int): Cbor raises CborError = Cbor(root.array(index))

  def selectDynamic(field: String)(using erased DynamicCborEnabler): Cbor raises CborError =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased DynamicCborEnabler)
  :   Cbor raises CborError =

    apply(field.tt)(index)


  def updateDynamic(field: String)[value: Encodable in Cbor](value: value)
    ( using erased DynamicCborEnabler )
  :   Cbor raises CborError =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased DynamicCborEnabler)
  :   Cbor raises CborError =

    delete(field)


  private[breviloquence] def modify(field: String, value: Cbor): Cbor raises CborError =
    if !root.isMap then abort(CborError(Reason.NotType(root.primitive, Primitive.Map)))
    val array = root.asInstanceOf[IArray[Any]]
    val length = array.length

    root.index(field) match
      case -1 =>
        val out = new Array[Any](length + 2)
        System.arraycopy(array.asInstanceOf[Array[Any]], 0, out, 0, length)
        out(length) = field
        out(length + 1) = value.root
        Cbor.ast(Cbor.Ast(out.asInstanceOf[IArray[Any]]))

      case index =>
        val out = new Array[Any](length)
        System.arraycopy(array.asInstanceOf[Array[Any]], 0, out, 0, length)
        out(index*2 + 1) = value.root
        Cbor.ast(Cbor.Ast(out.asInstanceOf[IArray[Any]]))

  private[breviloquence] def delete(field: String): Cbor raises CborError =
    if !root.isMap then abort(CborError(Reason.NotType(root.primitive, Primitive.Map)))
    val array = root.asInstanceOf[Array[Any]]
    val length = array.length

    root.index(field) match
      case -1 => Cbor.ast(root)

      case index =>
        val out = new Array[Any](length - 2)
        System.arraycopy(array, 0, out, 0, index*2)

        System.arraycopy(array, index*2 + 2, out, index*2, length - index*2 - 2)
        Cbor.ast(Cbor.Ast(out.asInstanceOf[IArray[Any]]))

  def apply(field: Text): Cbor raises CborError =
    if root.unset then Cbor.ast(Cbor.Ast(Unset))
    else if !root.isMap then abort(CborError(Reason.NotType(root.primitive, Primitive.Map)))
    else root.index(field.s) match
      case -1    => Cbor.ast(Cbor.Ast(Unset))
      case index => Cbor(root.value(index))

  override def hashCode: Int = root.hashCode

  override def equals(right: Any): Boolean = right match
    case right: Cbor => recur(root, right.root)
    case _           => false

  private def recur(left: Cbor.Ast, right: Cbor.Ast): Boolean =
    if left.isInteger && right.isInteger then left.asInstanceOf[Long] == right.asInstanceOf[Long]
    else if left.isFloat && right.isFloat
    then left.asInstanceOf[Double] == right.asInstanceOf[Double]
    else if left.isTextString && right.isTextString
    then left.asInstanceOf[String] == right.asInstanceOf[String]
    else if left.isBoolean && right.isBoolean
    then left.asInstanceOf[Boolean] == right.asInstanceOf[Boolean]
    else if left.isByteString && right.isByteString
    then java.util.Arrays.equals(left.asInstanceOf[Array[Byte]], right.asInstanceOf[Array[Byte]])
    else if left.nullary && right.nullary then true
    else if left.unset && right.unset then true
    else if left.isTag && right.isTag
    then
      val leftTag = left.asInstanceOf[Cbor.Tag]
      val rightTag = right.asInstanceOf[Cbor.Tag]

      leftTag.tag == rightTag.tag
      && recur(leftTag.value.asInstanceOf[Cbor.Ast], rightTag.value.asInstanceOf[Cbor.Ast])

    else if left.isArray && right.isArray then
      val leftElements = left.elements
      val rightElements = right.elements

      if leftElements != rightElements then false else
        var index = 0
        var equal = true

        while index < leftElements && equal do
          if !recur(left.element(index), right.element(index)) then equal = false
          index += 1

        equal

    else if left.isMap && right.isMap then
      val ln = left.entries
      val rn = right.entries

      if ln != rn then false else
        // Maps with arbitrary keys: compare position-by-position. This is
        // strict — re-ordered maps compare as unequal. Canonical CBOR uses a
        // deterministic key order, so well-formed inputs round-trip cleanly.
        var index = 0
        var equal = true

        while index < ln && equal do
          if
            !recur(left.key(index), right.key(index))
            || !recur(left.value(index), right.value(index))
          then equal = false

          index += 1

        equal

    else
      false

  def as[value: Decodable in Cbor]: value raises CborError = value.decoded(this)
