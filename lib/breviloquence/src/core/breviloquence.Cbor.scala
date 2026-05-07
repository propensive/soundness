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

import scala.collection.Factory
import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import CborError.{Primitive, Reason}

trait Cbor2:
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Cbor )
  =>  value is Encodable in Cbor =

    new Encodable:
      type Self = Optional[value]
      type Form = Cbor

      def encoded(value: Optional[value]): Cbor =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Cbor.ast(CborAst(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner] => Tactic[CborError]
  =>  ( decodable: => inner is Decodable in Cbor )
  =>  value is Decodable in Cbor = cbor =>

    if cbor.root.isAbsent then Unset else decodable.decoded(cbor)


  inline given decodable: [value] => value is Decodable in Cbor = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[CborError]](_.root.string.tt.decode[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  inline given encodable: [value] => value is Encodable in Cbor = summonFrom:
    case given (`value` is Encodable in Text) => value => Cbor.ast(CborAst(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Cbor]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Cbor =
      cbor =>
        provide[Tactic[CborError]]:
          val root = cbor.root
          val n = if root.isMap then root.mapSize else 0
          val values = scala.collection.mutable.HashMap.empty[String, CborAst]
          var i = 0
          while i < n do
            val key = root.mapKey(i)
            if key.isTextString then values.update(key.string, root.mapValue(i))
            i += 1

          build: [field] =>
            context =>
              values.get(label.s) match
                case Some(value) => context.decoded(new Cbor(value))
                case None        => default.or(context.decoded(new Cbor(CborAst(Unset))))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Cbor =
      cbor =>
        provide[Tactic[CborError]]:
          provide[Tactic[VariantError]]:
            val discriminable = infer[derivation is Discriminable in Cbor]

            val discriminant: Text = discriminable.discriminate(cbor).or(abort(CborError(Reason.Absent)))

            delegate(discriminant): [variant <: derivation] =>
              context => context.decoded(cbor)

  object EncodableDerivation extends Derivable[Encodable in Cbor]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Cbor =

      value =>
        val labels: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()
        val values: scala.collection.mutable.ArrayBuffer[Any] = scala.collection.mutable.ArrayBuffer()

        fields(value): [field] =>
          field =>
            val encoded = contextual.encode(field).root
            if !encoded.isAbsent then
              labels += label.s
              values += encoded

        Cbor.ast(CborAst.map(IArray.from(labels), IArray.from(values)))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Cbor = value =>
      val discriminable = infer[derivation is Discriminable in Cbor]

      variant(value): [variant <: derivation] =>
        value => discriminable.rewrite(label, contextual.encode(value))

object Cbor extends Cbor2, Dynamic:
  def ast(value: CborAst): Cbor = new Cbor(value)

  // Canonical external accessor for the underlying AST.
  def unseal(cbor: Cbor): CborAst = cbor.root

  given boolean: Tactic[CborError] => Boolean is Decodable in Cbor = _.root.boolean
  given double: Tactic[CborError] => Double is Decodable in Cbor = _.root.double
  given float: Tactic[CborError] => Float is Decodable in Cbor = _.root.double.toFloat
  given long: Tactic[CborError] => Long is Decodable in Cbor = _.root.long
  given int: Tactic[CborError] => Int is Decodable in Cbor = _.root.long.toInt
  given text: Tactic[CborError] => Text is Decodable in Cbor = _.root.string.tt
  given string: Tactic[CborError] => String is Decodable in Cbor = _.root.string
  given byteString: Tactic[CborError] => IArray[Byte] is Decodable in Cbor = _.root.byteString
  given cbor: Cbor is Decodable in Cbor = identity(_)

  given unit: Tactic[CborError] => Unit is Decodable in Cbor =
    value =>
      if value.root.isNull then ()
      else
        val reason =
          if value.root.isAbsent then Reason.Absent
          else Reason.NotType(value.root.primitive, Primitive.Null)

        abort(CborError(reason))


  given option: [value: Decodable in Cbor] => Tactic[CborError]
  =>  Option[value] is Decodable in Cbor =

    cbor => if cbor.root.isAbsent then None else Some(value.decoded(cbor))


  given optionEncodable: [value] => (encodable: value is Encodable in Cbor)
  =>  Option[value] is Encodable in Cbor =

    new Encodable:
      type Self = Option[value]
      type Form = Cbor

      def encoded(value: Option[value]): Cbor = value match
        case None        => Cbor.ast(CborAst(Unset))
        case Some(value) => encodable.encode(value)


  given integralEncodable: [integral: Integral] => integral is Encodable in Cbor =
    int => Cbor.ast(CborAst(integral.toLong(int)))

  given textEncodable: Text is Encodable in Cbor = text => Cbor.ast(CborAst(text.s))
  given stringEncodable: String is Encodable in Cbor = string => Cbor.ast(CborAst(string))
  given doubleEncodable: Double is Encodable in Cbor = double => Cbor.ast(CborAst(double))
  given floatEncodable: Float is Encodable in Cbor = float => Cbor.ast(CborAst(float.toDouble))
  given intEncodable: Int is Encodable in Cbor = int => Cbor.ast(CborAst(int.toLong))
  given longEncodable: Long is Encodable in Cbor = long => Cbor.ast(CborAst(long))
  given booleanEncodable: Boolean is Encodable in Cbor = boolean => Cbor.ast(CborAst(boolean))
  given unitEncodable: Unit is Encodable in Cbor = _ => Cbor.ast(CborAst(null))
  given bytesEncodable: IArray[Byte] is Encodable in Cbor = bytes => Cbor.ast(CborAst(bytes))
  given cborEncodable: Cbor is Encodable in Cbor = identity(_)


  given listEncodable: [list <: List, element] => (encodable: => element is Encodable in Cbor)
  =>  list[element] is Encodable in Cbor =

    values => Cbor.ast(CborAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element] => (encodable: => element is Encodable in Cbor)
  =>  set[element] is Encodable in Cbor =

    values => Cbor.ast(CborAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given trieEncodable: [trie <: Trie, element] => (encodable: => element is Encodable in Cbor)
  =>  trie[element] is Encodable in Cbor =

    values => Cbor.ast(CborAst.arr(IArray.from(values.map(encodable.encoded(_).root))))


  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[CborError] )
  =>  ( decodable: => element is Decodable in Cbor )
  =>  collection[element] is Decodable in Cbor =

    value =>
      val builder = factory.newBuilder
      value.root.array.each: cbor =>
        builder += decodable.decoded(Cbor.ast(cbor))

      builder.result()


  given mapDecodable: [key: Decodable in Text, element]
  =>  (decodable: => element is Decodable in Cbor)
  =>  Tactic[CborError]
  =>  Map[key, element] is Decodable in Cbor =

    value =>
      val root = value.root
      val n = if root.isMap then root.mapSize else 0
      var i = 0
      var acc = Map.empty[key, element]
      while i < n do
        val mapKey = root.mapKey(i)
        if mapKey.isTextString then
          acc = acc.updated
                  ( mapKey.string.tt.decode,
                    decodable.decoded(Cbor.ast(root.mapValue(i))) )
        else abort(CborError(Reason.NonStringKey))
        i += 1
      acc


  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Cbor )
  =>  Map[key, element] is Encodable in Cbor =

    map =>
      val keys: List[key] = map.keys.to(List)
      val values = IArray.from(keys.map(map(_).encode.root))
      Cbor.ast(CborAst.map(IArray.from(keys.map(k => k.encode.s)), values))


  def applyDynamicNamed(methodName: "make")(elements: (String, Cbor)*): Cbor =
    val keys: IArray[Any] = IArray.from(elements.map(_(0): Any))
    val values: IArray[Any] = IArray.from(elements.map(_(1).root.asInstanceOf[Any]))
    Cbor(CborAst.map(keys, values))

  def discriminatedUnion[value](label: Text): value is Discriminable in Cbor = new Discriminable:
    type Form = Cbor
    type Self = value

    protected def key: String = label.s

    import dynamicCborAccess.enabled

    def rewrite(kind: Text, cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key)(kind))
    def discriminate(cbor: Cbor): Optional[Text] = safely(cbor.selectDynamic(key).as[Text])
    def variant(cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key)(Unset))

class Cbor(rootValue: Any) extends Dynamic derives CanEqual:
  private[breviloquence] def root: CborAst = rootValue.asInstanceOf[CborAst]

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
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    root.mapIndexOf(field) match
      case -1 =>
        val out = new Array[Any](len + 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(len) = field
        out(len + 1) = value.root
        Cbor.ast(CborAst(out.asInstanceOf[IArray[Any]]))

      case index =>
        val out = new Array[Any](len)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, len)
        out(index*2 + 1) = value.root
        Cbor.ast(CborAst(out.asInstanceOf[IArray[Any]]))

  private[breviloquence] def delete(field: String): Cbor raises CborError =
    if !root.isMap then abort(CborError(Reason.NotType(root.primitive, Primitive.Map)))
    val arr = root.asInstanceOf[IArray[Any]]
    val len = arr.length
    root.mapIndexOf(field) match
      case -1 => Cbor.ast(root)

      case index =>
        val out = new Array[Any](len - 2)
        System.arraycopy(arr.asInstanceOf[Array[Any]], 0, out, 0, index*2)
        System.arraycopy
                ( arr.asInstanceOf[Array[Any]],
                  index*2 + 2,
                  out,
                  index*2,
                  len - index*2 - 2 )
        Cbor.ast(CborAst(out.asInstanceOf[IArray[Any]]))

  def apply(field: Text): Cbor raises CborError =
    if root.isAbsent then Cbor.ast(CborAst(Unset))
    else if !root.isMap then abort(CborError(Reason.NotType(root.primitive, Primitive.Map)))
    else root.mapIndexOf(field.s) match
      case -1    => Cbor.ast(CborAst(Unset))
      case index => Cbor(root.mapValue(index))

  override def hashCode: Int = root.hashCode

  override def equals(right: Any): Boolean = right match
    case right: Cbor => deepEquals(root, right.root)
    case _           => false

  private def deepEquals(left: CborAst, right: CborAst): Boolean =
    if left.isInteger && right.isInteger then
      left.asInstanceOf[Long] == right.asInstanceOf[Long]
    else if left.isFloat && right.isFloat then
      left.asInstanceOf[Double] == right.asInstanceOf[Double]
    else if left.isTextString && right.isTextString then
      left.asInstanceOf[String] == right.asInstanceOf[String]
    else if left.isBoolean && right.isBoolean then
      left.asInstanceOf[Boolean] == right.asInstanceOf[Boolean]
    else if left.isByteString && right.isByteString then
      java.util.Arrays.equals(
        left.asInstanceOf[Array[Byte]],
        right.asInstanceOf[Array[Byte]])
    else if left.isNull && right.isNull then true
    else if left.isAbsent && right.isAbsent then true
    else if left.isTag && right.isTag then
      val lt = left.asInstanceOf[CborTag]
      val rt = right.asInstanceOf[CborTag]
      lt.tag == rt.tag && deepEquals(lt.value.asInstanceOf[CborAst], rt.value.asInstanceOf[CborAst])
    else if left.isArray && right.isArray then
      val ln = left.arrayLength
      val rn = right.arrayLength
      if ln != rn then false
      else
        var i = 0
        var eq = true
        while i < ln && eq do
          if !deepEquals(left.arrayElement(i), right.arrayElement(i)) then eq = false
          i += 1
        eq
    else if left.isMap && right.isMap then
      val ln = left.mapSize
      val rn = right.mapSize
      if ln != rn then false
      else
        // Maps with arbitrary keys: compare position-by-position. This is
        // strict — re-ordered maps compare as unequal. Canonical CBOR uses a
        // deterministic key order, so well-formed inputs round-trip cleanly.
        var i = 0
        var eq = true
        while i < ln && eq do
          if !deepEquals(left.mapKey(i), right.mapKey(i))
          || !deepEquals(left.mapValue(i), right.mapValue(i))
          then eq = false
          i += 1
        eq
    else false

  def as[value: Decodable in Cbor]: value raises CborError = value.decoded(this)
