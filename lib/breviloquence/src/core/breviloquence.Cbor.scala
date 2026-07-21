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
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import CborError.{Primitive, Reason}

trait Cbor2:
  this: Cbor.type =>
  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Cbor )
  =>  value is Encodable in Cbor =

    new Encodable:
      type Self = value
      type Form = Cbor

      def encoded(value: value): Cbor =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(ast(Ast(Unset)))


  given optional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( tactic: Tactic[CborError] )
  =>  ( decodable: => (inner is Decodable in Cbor)^ )
  =>  ((value is Decodable in Cbor)^{tactic, caps.any}) =
    // An honest capability: the instance retains the resolution-scoped tactic and
    // the by-name inner codec (every given that includes a tactic is a capability;
    // Jon, 2026-07-12).
    cbor => if cbor.root.unset then Unset else decodable.decoded(cbor)


  inline given decodable: [value] => value is Decodable in Cbor = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[CborError]](_.root.string.tt.as[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  // The AST-materializing read path: parse the whole input into a `Cbor`,
  // then decode. Lives at this priority so `object Cbor`'s direct-parsing
  // `aggregableParsed` wins whenever the value has a `Cbor.Parsable`; when
  // it does not, this resolves exactly as before. `source.read[Foo in Cbor]`
  // is shorthand for `source.read[Cbor].as[Foo]`; the `Form` type-tag is
  // added by an `asInstanceOf` cast — `value in Cbor` is just
  // `value { type Form = Cbor }` so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Cbor] => (tactic: Tactic[CborError])
  =>  (((value in Cbor) is Aggregable by Data)^{tactic}) =
    bytes => Cbor.ast(bytes.read[Cbor.Ast]).as[value].asInstanceOf[value in Cbor]

  inline given encodable: [value] => value is Encodable in Cbor = summonFrom:
    case given (`value` is Encodable in Text) => value => ast(Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived


  object DecodableDerivation extends Derivable[Decodable in Cbor]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Cbor =

        // The `Tactic` is summoned at the derivation site and supplied explicitly to `decodeRecord`,
        // rather than re-summoned via a `provide` inside the decoder body: that minted a distinct
        // root capability that failed to unify with `build`'s polymorphic per-field lambda. A
        // `Decodable` is `Pure`, so the SAM closing over the summoned `Tactic` adds nothing to its
        // capture set.
      cbor =>
        decodeRecord[derivation](cbor)
          (using infer[ProductReflection[derivation]], infer[Tactic[CborError]])

    private inline def decodeRecord[derivation <: Product]
      ( cbor: Cbor )
      ( using ProductReflection[derivation], Tactic[CborError] )
    :   derivation =

      val root = cbor.root
      val count = if root.isMap then root.entries else 0

      // Built immutably: `build`'s per-field lambda is polymorphic and must be pure, so it may only
      // close over pure values — a mutable map would be a capability.
      val values: Map[String, Ast] =
        val builder = Map.newBuilder[String, Ast]
        var index = 0
        while index < count do
          val key = root.key(index)
          if key.isTextString then builder += key.string -> root.value(index)
          index += 1
        builder.result()

      // `@name[Cbor]` / bare `@name` renames: field name -> map key, read
      // back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Cbor]

      build[derivation]: [field] =>
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
              variantRelabelling[derivation, Cbor].map: (variant, wire) => wire -> variant

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
        value =>
          discriminable.rewrite(variantNames.getOrElse(label, label), contextual.encode(value))

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
  // Distinct sentinel for a CBOR `null`, kept disjoint from the null-backed `Unset`
  // (CBOR `undefined`/absent): both would otherwise be the JVM `null` and collide.
  case object CborNull
  type CborNull      = CborNull.type
  type CborUndefined = vacuous.Unset

  type CborTypes =
    CborInteger | CborFloat | CborText | CborBytes | CborArray | CborMap | CborBoolean | CborNull |
      CborUndefined | Tag

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

    // Encodes a CBOR node to its binary form (RFC 8949 major types). The whole byte-level fold
    // lives in this instance so `.encode` is the single route to CBOR bytes: integers take the
    // shortest of the 1/2/4/8-byte head encodings, floats are always emitted as 64-bit, and arrays
    // and maps are length-prefixed.
    given encodable: Ast is Encodable in Data = cbor =>
      def u16(out: (Producer.Bytes)^, value: Int): Unit =
        out.push(((value >>> 8) & 0xFF).toByte)
        out.push((value & 0xFF).toByte)

      def u32(out: (Producer.Bytes)^, value: Long): Unit =
        out.push(((value >>> 24) & 0xFF).toByte)
        out.push(((value >>> 16) & 0xFF).toByte)
        out.push(((value >>> 8) & 0xFF).toByte)
        out.push((value & 0xFF).toByte)

      def u64(out: (Producer.Bytes)^, value: Long): Unit =
        out.push(((value >>> 56) & 0xFF).toByte)
        out.push(((value >>> 48) & 0xFF).toByte)
        out.push(((value >>> 40) & 0xFF).toByte)
        out.push(((value >>> 32) & 0xFF).toByte)
        out.push(((value >>> 24) & 0xFF).toByte)
        out.push(((value >>> 16) & 0xFF).toByte)
        out.push(((value >>> 8) & 0xFF).toByte)
        out.push((value & 0xFF).toByte)

      def head(out: (Producer.Bytes)^, major: Int, value: Long): Unit =
        val majorBits = major << 5

        if value < 0 then
          out.push((majorBits | 27).toByte)
          u64(out, value)
        else if value < 24 then
          out.push((majorBits | value.toInt).toByte)
        else if value < (1 << 8) then
          out.push((majorBits | 24).toByte)
          out.push(value.toByte)
        else if value < (1 << 16) then
          out.push((majorBits | 25).toByte)
          u16(out, value.toInt)
        else if value < (1L << 32) then
          out.push((majorBits | 26).toByte)
          u32(out, value)
        else
          out.push((majorBits | 27).toByte)
          u64(out, value)

      def write(out: (Producer.Bytes)^, cbor: Cbor.Ast): Unit =
        if cbor.isInteger then
          val long = cbor.asInstanceOf[Long]

          if long >= 0 then head(out, 0, long)
          else head(out, 1, -1L - long)

        else if cbor.isFloat then
          out.push((0xE0 | 27).toByte)
          u64(out, java.lang.Double.doubleToLongBits(cbor.asInstanceOf[Double]))

        else if cbor.isTextString then
          val text = cbor.asInstanceOf[String]
          val bytes = text.getBytes("UTF-8").nn
          head(out, 3, bytes.length.toLong)
          out.put(bytes.immutable(using Unsafe))

        else if cbor.isByteString then
          val bytes = cbor.asInstanceOf[Array[Byte]]
          head(out, 2, bytes.length.toLong)
          out.put(bytes.immutable(using Unsafe))

        else if cbor.isBoolean then
          out.push(if cbor.asInstanceOf[Boolean] then 0xF5.toByte else 0xF4.toByte)

        else if cbor.nullary then
          out.push(0xF6.toByte)
        else if cbor.unset then
          out.push(0xF7.toByte)

        else if cbor.isTag then
          val tag = cbor.asInstanceOf[Cbor.Tag]
          head(out, 6, tag.tag)
          write(out, tag.value.asInstanceOf[Cbor.Ast])

        else if cbor.isArray then
          val count = cbor.elements
          head(out, 4, count.toLong)
          var index = 0

          while index < count do
            write(out, cbor.element(index))
            index += 1

        else if cbor.isMap then
          val count = cbor.entries
          head(out, 5, count.toLong)
          var index = 0

          while index < count do
            write(out, cbor.key(index))
            write(out, cbor.value(index))
            index += 1

      Producer.collect[Data](): producer =>
        write(producer, cbor)

    // Renders a CBOR node in the RFC 8949 §8 diagnostic notation. The whole rendering lives in this
    // instance so `.show` is the single route to diagnostic text.
    given showable: Ast is Showable = cbor =>
      val builder = new java.lang.StringBuilder

      def append(builder: java.lang.StringBuilder, cbor: Cbor.Ast): Unit =
        if cbor.isInteger then builder.append(cbor.asInstanceOf[Long].toString)
        else if cbor.isFloat then
          val double = cbor.asInstanceOf[Double]

          if double.isNaN then builder.append("NaN")
          else if double == Double.PositiveInfinity then builder.append("Infinity")
          else if double == Double.NegativeInfinity then builder.append("-Infinity")
          else builder.append(double.toString)

        else if cbor.isTextString then
          builder.append('"')
          val text = cbor.asInstanceOf[String]
          var index = 0

          while index < text.length do builder.append:
            text.charAt(index) match
              case '"'                 => "\\\""
              case '\\'                => "\\\\"
              case '\n'                => "\\n"
              case '\r'                => "\\r"
              case '\t'                => "\\t"
              case char if char < 0x20 => f"\\u${char.toInt}%04x"
              case char                => char

            index += 1

          builder.append('"')

        else if cbor.isByteString then
          val bytes = cbor.asInstanceOf[Array[Byte]]
          builder.append("h'")
          var index = 0

          while index < bytes.length do
            builder.append(f"${bytes(index) & 0xFF}%02x")
            index += 1

          builder.append('\'')

        else if cbor.isBoolean then
          builder.append(cbor.asInstanceOf[Boolean].toString)
        else if cbor.nullary then
          builder.append("null")
        else if cbor.unset then
          builder.append("undefined")

        else if cbor.isTag then
          val tag = cbor.asInstanceOf[Cbor.Tag]
          builder.append(tag.tag.toString)
          builder.append('(')
          append(builder, tag.value.asInstanceOf[Cbor.Ast])
          builder.append(')')

        else if cbor.isArray then
          val count = cbor.elements
          builder.append('[')
          var index = 0

          while index < count do
            if index > 0 then builder.append(", ")
            append(builder, cbor.element(index))
            index += 1

          builder.append(']')

        else if cbor.isMap then
          val count = cbor.entries
          builder.append('{')
          var index = 0

          while index < count do
            if index > 0 then builder.append(", ")
            append(builder, cbor.key(index))
            builder.append(": ")
            append(builder, cbor.value(index))
            index += 1

          builder.append('}')

      append(builder, cbor)
      builder.toString.tt

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
  given lens: [name <: Label: ValueOf] => (erased dynamicCborEnabler: DynamicCborEnabler) => (tactic: Tactic[CborError])
  =>  ((name is Lens from Cbor onto Cbor)^{tactic}) =
    Lens
     ( cbor => cbor.selectDynamic(valueOf[name]),
       (cbor, value) => cbor.modify(valueOf[name], value) )

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
      else
        origin

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
      else
        origin

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM
  // pipeline — rejects the `Optic`'s capture of `filter.predicate` against the required
  // pure `Optic` type. (Compiler divergence; see #1520 and `caesura`'s `rowFilter`.)
  given filterOptical: Filter[Cbor] is Optical from Cbor onto Cbor = filter =>
    val predicate: Cbor -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        Cbor.ast:
          val updated = new Array[Any](n)
          var i = 0

          while i < n do
            val element = Cbor.ast(origin.root.element(i))
            updated(i) = (if predicate(element) then lambda(element) else element).root
            i += 1

          Cbor.Ast.array(updated.asInstanceOf[IArray[Any]])
      else
        origin

  given boolean: (tactic: Tactic[CborError])
  =>  ((Boolean is Decodable in Cbor)^{tactic}) = _.root.boolean
  given double: (tactic: Tactic[CborError])
  =>  ((Double is Decodable in Cbor)^{tactic}) = _.root.double
  given float: (tactic: Tactic[CborError])
  =>  ((Float is Decodable in Cbor)^{tactic}) = _.root.double.toFloat
  given long: (tactic: Tactic[CborError])
  =>  ((Long is Decodable in Cbor)^{tactic}) = _.root.long
  given int: (tactic: Tactic[CborError])
  =>  ((Int is Decodable in Cbor)^{tactic}) = _.root.long.toInt
  given text: (tactic: Tactic[CborError])
  =>  ((Text is Decodable in Cbor)^{tactic}) = _.root.string.tt
  given string: (tactic: Tactic[CborError])
  =>  ((String is Decodable in Cbor)^{tactic}) = _.root.string
  given byteString: (tactic: Tactic[CborError])
  =>  ((IArray[Byte] is Decodable in Cbor)^{tactic}) = _.root.byteString
  given cbor: Cbor is Decodable in Cbor = identity(_)

  given aggregable: (tactic: Tactic[CborError])
  =>  ((Cbor is Aggregable by Data)^{tactic}) =
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
        (t"application/cbor", HttpStreams.Body(Ast.encodable.encoded(Cbor.unseal(value))))

  object Parsable:
    // The base of generated parsers: generated code is capture-erased, so
    // the body receives the reader as a neutral carrier, and the capability
    // is asserted here at the rim — the audited point — like the reader's
    // own accessors. (A generated override of `parse` itself would narrow
    // the trait's `Reader^` parameter to a pure type, which capture
    // checking rejects at the instantiation site.)
    abstract class Direct[value] extends Cbor.Parsable:
      type Self = value

      protected def parseCarrier(reader: AnyRef): value

      def parse(reader: CborReader^): value = parseCarrier(reader.asInstanceOf[AnyRef])

    def apply[value](parser: (reader: CborReader^) => value)
    :   ((value is Cbor.Parsable)^{parser}) =

      new Cbor.Parsable:
        type Self = value
        def parse(reader: CborReader^): value = parser(reader)

    // The universal bridge from the AST world: parse one whole item into a
    // `Cbor` and decode it. Field types with only a `Decodable in Cbor`
    // keep working through this, and it is the user's one-line escape hatch
    // when a custom decoder must beat a generated direct parser.
    def fromDecodable[value](decodable: (value is Decodable in Cbor)^)
    :   ((value is Cbor.Parsable)^{decodable}) =

      new Cbor.Parsable:
        type Self = value
        def parse(reader: CborReader^): value = decodable.decoded(reader.value())

        override def absent()(using Tactic[CborError]): value =
          decodable.decoded(Cbor.ast(Ast(Unset)))

    // A required field whose key was absent from the map. Public because
    // generated parsers are spliced into user modules.
    def missing[value]()(using Tactic[CborError]): value = abort(CborError(Reason.Absent))

    // The call points for a nominal `Parsable` in a field position of a
    // *generated* parser (a recursive record's own instance, or a
    // hand-written one). Both travel as neutral carriers — generated code
    // is capture-erased — and the capability is reasserted here, at the
    // audited point, exactly as the reader's own rim accessors do.
    def parseField[value](parsable: AnyRef, reader: AnyRef): value =
      parsable.asInstanceOf[value is Cbor.Parsable].parse(reader.asInstanceOf[CborReader^])

    def absentField[value](parsable: AnyRef)(using Tactic[CborError]): value =
      parsable.asInstanceOf[value is Cbor.Parsable].absent()

  // The direct-parsing counterpart of `Decodable in Cbor`: consumes data
  // items straight off the input bytes through a `CborReader` instead of
  // walking a materialized `Cbor.Ast`, so `read[value in Cbor]` can
  // instantiate values without building the AST. `Parsable` is the opt-in
  // surface: explicit instances and `Cbor.Inlinable.parsable`. It has no
  // blanket fallback given, so no read changes behavior until a type opts
  // in; field types without one bridge through `Parsable.fromDecodable`.
  trait Parsable extends distillate.Parsable:
    type Transport = Cbor
    type Reader = CborReader

    // What a field of this type yields when its key is absent from the map,
    // mirroring the AST path's `decoded(Cbor(Ast(Unset)))`: an abort unless
    // overridden.
    def absent()(using Tactic[CborError]): Self = abort(CborError(Reason.Absent))

  // Direct-parsing counterpart of the `aggregable`/`aggregableIn` path:
  // drives a `Cbor.Parsable` instance over the input through a
  // `CborReader`, so no AST is built for the items the instance reads
  // directly. Trailing bytes are rejected exactly as `Parser.parse`.
  private def parseDirect[value]
    ( input: Data, parsable: (value is Cbor.Parsable)^ )
    ( using tactic: Tactic[CborError] )
  :   value =

    val parser = Parser(input)
    val result = parsable.parse(CborReader(parser, tactic))

    if parser.offset < parser.data.length
    then abort(CborError(Reason.Trailing(parser.offset.toLong)))

    result

  // Direct parsing: when the value knows how to consume CBOR items itself,
  // the AST is never materialized. Declared here (not in `Cbor2`, where the
  // `Decodable`-based `aggregableIn` lives) so it wins whenever a
  // `Cbor.Parsable` exists, and is otherwise inapplicable — existing code
  // resolves exactly as before. Sealed per the codec-thunk pattern: the
  // instance retains the resolution-scoped parsable and tactic.
  given aggregableParsed: [value]
  =>  (parsable: (value is Cbor.Parsable)^)
  =>  (tactic: Tactic[CborError])
  =>  ((value in Cbor) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      bytes => parseDirect(bytes.read[Data], parsable).asInstanceOf[value in Cbor]

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream.
  // Concrete in `Data`, so it beats the composed pipeline by specificity.
  // Sealed like `aggregableParsed` above.
  given readableParsed: [value]
  =>  (parsable: (value is Cbor.Parsable)^)
  =>  (tactic: Tactic[CborError])
  =>  (Data is Readable to (value in Cbor)) =

    caps.unsafe.unsafeAssumePure:
      data => parseDirect(data, parsable).asInstanceOf[value in Cbor]

  given unit: (tactic: Tactic[CborError])
  =>  ((Unit is Decodable in Cbor)^{tactic}) =
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
  given unitEncodable: Unit is Encodable in Cbor = _ => ast(Ast(CborNull))
  given bytesEncodable: IArray[Byte] is Encodable in Cbor = bytes => ast(Ast(bytes))
  given cborEncodable: Cbor is Encodable in Cbor = identity(_)


  // The collection instances below are honest capabilities: each retains its by-name
  // element codec (and, where present, a resolution-scoped `Tactic`), which share the
  // instance's given-resolution lifetime (every given that includes a tactic is a
  // capability; Jon, 2026-07-12). See rep/DECISIONS.md.
  given listEncodable: [list <: List, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((list[element] is Encodable in Cbor)^) =
    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given setEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((set[element] is Encodable in Cbor)^) =
    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given seriesEncodable: [series <: Series, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((series[element] is Encodable in Cbor)^) =
    values => ast(Ast.array(IArray.from(values.map(encodable.encoded(_).root))))


  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: sc.Factory[element, collection[element]], tactic:  Tactic[CborError] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((collection[element] is Decodable in Cbor)^{tactic, caps.any}) =

    // An honest capability, as `optional` above.
    value =>
        val builder = factory.newBuilder
        value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))

        builder.result()


  given mapDecodable: [key: Decodable in Text, element]
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ( tactic: Tactic[CborError] )
  =>  ((Map[key, element] is Decodable in Cbor)^{tactic, caps.any}) =

    // An honest capability, as `optional` above.
    value =>
        val root = value.root
        val count = if root.isMap then root.entries else 0
        var index = 0
        var map = Map.empty[key, element]

        while index < count do
          val key = root.key(index)

          if key.isTextString
          then map = map.updated(key.string.tt.as, decodable.decoded(ast(root.value(index))))
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

  // The map-key-discriminated `Discriminable` shape, as a nameable class so
  // that generated parsers (which dispatch on the discriminant
  // monomorphically) can recognize the shape and extract the key at
  // expansion time.
  final class DiscriminantKey[derivation](val key: Text) extends Discriminable:
    type Form = Cbor
    type Self = derivation

    import dynamicCborAccess.enabled

    def rewrite(kind: Text, cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key.s)(kind))
    def discriminate(cbor: Cbor): Optional[Text] = safely(cbor.selectDynamic(key.s).as[Text])
    def variant(cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key.s)(Unset))

  def discriminatedUnion[value](label: Text): value is Discriminable in Cbor =
    DiscriminantKey[value](label)


  private[breviloquence] object Parser:

    // The break stop code (0xFF) terminates an indefinite-length item.
    private inline val Break = 0xFF

    // Boxed-Long cache covering CBOR's uint16 range. The JDK's `Long.valueOf`
    // only caches -128..127; corpus payloads dominated by small unsigned
    // integers (timestamps, ids, counts) routinely fall outside that window
    // and pay a fresh `java.lang.Long` allocation per value. A flat array
    // lookup is two-to-three times cheaper than allocation in steady state.
    private inline val LongCacheSize = 65536

    private val longCache: Array[AnyRef] =
      val out = new Array[AnyRef](LongCacheSize)
      var index = 0

      while index < LongCacheSize do
        out(index) = java.lang.Long.valueOf(index.toLong).nn
        index += 1

      out

    private inline def boxLong(value: Long): AnyRef =
      if value >= 0L && value < LongCacheSize then longCache(value.toInt)
      else java.lang.Long.valueOf(value).nn

    def parse(source: IArray[Byte]): Cbor.Ast raises CborError =
      val parser = new Parser(source)
      val result = parser.value()

      if parser.offset < parser.data.length
      then abort(CborError(Reason.Trailing(parser.offset.toLong)))

      result

  // The class is public — generated parsers, spliced into user modules,
  // bind it once per record and read through its direct rim — but only
  // breviloquence's read paths can construct one.
  final class Parser private[breviloquence] (input: IArray[Byte]):
    import Parser.{Break, boxLong}

    // Cache the underlying primitive array so reads compile to BALOAD rather
    // than going through `IArray$.apply`. `data.length` is constant-folded by
    // the JIT and cheaper than going through a separate `length` accessor.
    private[breviloquence] val data: Array[Byte] = input.asInstanceOf[Array[Byte]]

    // `offset` is exposed only to the package-private parse() entry point so it
    // can detect trailing bytes after a successful parse. All hot-path reads
    // mutate it directly through the JVM PUTFIELD/GETFIELD.
    var offset: Int = 0

    private inline def expect(count: Int): Unit raises CborError =
      if data.length - offset < count then abort(CborError(Reason.Truncated(offset.toLong)))

    private inline def readByte(): Int =
      (data(offset)&0xFF).also(offset += 1)

    private inline def readUInt8(): Int raises CborError =
      expect(1)
      readByte()

    private inline def readUInt16(): Int raises CborError =
      expect(2)
      val pos = offset
      offset = pos + 2
      ((data(pos) & 0xFF) << 8) | (data(pos + 1) & 0xFF)

    private inline def readUInt32(): Long raises CborError =
      expect(4)
      val pos = offset
      offset = pos + 4
      ((data(pos) & 0xFFL) << 24) |
        ((data(pos + 1) & 0xFFL) << 16) |
        ((data(pos + 2) & 0xFFL) << 8) |
        (data(pos + 3) & 0xFFL)

    private inline def readUInt64(): Long raises CborError =
      expect(8)
      val pos = offset
      offset = pos + 8
      ((data(pos) & 0xFFL) << 56) |
        ((data(pos + 1) & 0xFFL) << 48) |
        ((data(pos + 2) & 0xFFL) << 40) |
        ((data(pos + 3) & 0xFFL) << 32) |
        ((data(pos + 4) & 0xFFL) << 24) |
        ((data(pos + 5) & 0xFFL) << 16) |
        ((data(pos + 6) & 0xFFL) << 8) |
        (data(pos + 7) & 0xFFL)

    // Decodes the additional-info length field, returning the unsigned value as
    // a `Long`. A negative result means indefinite length.
    //
    // The `info < 24` fast path covers the in-head case (RFC 8949 §3.1) which
    // dominates real-world workloads (small integers, short strings, small
    // arrays/maps). The remaining cases dispatch through a `match` so the JVM
    // can compile them to a tableswitch.
    private inline def readLength(info: Int, headOffset: Long): Long raises CborError =
      if info < 24 then info.toLong
      else info match
        case 24 => readUInt8().toLong
        case 25 => readUInt16().toLong
        case 26 => readUInt32()

        case 27 =>
          val v = readUInt64()
          // Bit 63 set means the value > Long.MaxValue; CBOR allows this for
          // major types 0/1 but breviloquence rejects it.
          if v < 0 then abort(CborError(Reason.Overflow(headOffset)))
          v

        case 31 => -1L
        case _  => abort(CborError(Reason.Reserved(headOffset, info)))

    private inline def readBytes(length: Int): IArray[Byte] =
      val result = new Array[Byte](length)
      System.arraycopy(data, offset, result, 0, length)
      offset += length
      result.asInstanceOf[IArray[Byte]]

    private inline def boundedLength(length: Long, headOffset: Long): Int raises CborError =
      if length < 0 || length > Int.MaxValue then abort(CborError(Reason.Overflow(headOffset)))
      val count = length.toInt
      expect(count)
      count

    // Reads an indefinite-length byte string by concatenating its definite-
    // length chunks (each prefixed with major type 2) until a Break stop code.
    // Uses `ByteArrayOutputStream` so chunk bytes flow through bulk `write`
    // (≈ `System.arraycopy`) without per-byte boxing into `java.lang.Byte`.
    private def readIndefiniteByteString(): IArray[Byte] raises CborError =
      val buffer = new java.io.ByteArrayOutputStream
      var done = false

      while !done do
        expect(1)
        val head = data(offset) & 0xFF

        if head == Break then
          offset += 1
          done = true
        else
          val major = head >>> 5
          val info = head & 0x1F
          if major != 2 then abort(CborError(Reason.Reserved(offset.toLong, head)))
          val chunkOffset = offset.toLong
          offset += 1
          val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
          buffer.write(data, offset, length)
          offset += length

      buffer.toByteArray.nn.asInstanceOf[IArray[Byte]]

    private def readIndefiniteTextString(): String raises CborError =
      val buffer = new java.io.ByteArrayOutputStream
      var done = false

      while !done do
        expect(1)
        val head = data(offset) & 0xFF

        if head == Break then
          offset += 1
          done = true
        else
          val major = head >>> 5
          val info = head & 0x1F
          if major != 3 then abort(CborError(Reason.Reserved(offset.toLong, head)))
          val chunkOffset = offset.toLong
          offset += 1
          val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
          buffer.write(data, offset, length)
          offset += length

      val bytes = buffer.toByteArray.nn
      decodeUtf8(bytes, 0, bytes.length, 0L)

    private inline def decodeUtf8
      ( bytes: Array[Byte], start: Int, length: Int, errorOffset: Long )
    :   String raises CborError =

      try new String(bytes, start, length, java.nio.charset.StandardCharsets.UTF_8)
      catch case _: Throwable => abort(CborError(Reason.InvalidUtf8(errorOffset)))

    // IEEE 754 half precision (16-bit) → Double, per RFC 8949 §3.3.
    // Assembles the 64-bit pattern directly rather than going through
    // `math.pow` and a multiplication: half-floats have only 65 536 possible
    // values and the conversion is a fixed sequence of bit moves.
    private def halfToDouble(half: Int): Double =
      val sign = (half.toLong & 0x8000L) << 48 // sign bit → bit 63
      val exp = (half >>> 10) & 0x1F
      val mant = half & 0x3FF

      val bits: Long =
        if exp == 0 then
          if mant == 0 then sign
          else
            // Subnormal half: re-normalise by shifting until bit 10 is set,
            // adjusting the (double) exponent accordingly.
            var m = mant
            var e = -14 + 1023
            while (m & 0x400) == 0 do { m <<= 1; e -= 1 }
            sign | (e.toLong << 52) | ((m.toLong & 0x3FF) << 42)
        else if exp == 31 then
          // Infinity (mant == 0) or NaN. Sign is preserved for both.
          sign | (2047L << 52) | (mant.toLong << 42)
        else
          sign | ((exp + 1023 - 15).toLong << 52) | (mant.toLong << 42)

      java.lang.Double.longBitsToDouble(bits)

    def value(): Cbor.Ast raises CborError =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF
      offset = pos + 1

      // Fast paths for in-head small integers — by far the most common CBOR
      // head bytes in real workloads. Returning early skips the major/info
      // split, the `readLength` dispatch and the `headOffset` capture. Boxing
      // routes through the shared `boxLong` cache so the resulting
      // `java.lang.Long` is reused on the next parse.
      //   head 0x00–0x17 : major 0, info 0–23  → value is head itself
      //   head 0x20–0x37 : major 1, info 0–23  → value is -1 - (head & 0x1F)
      if head < 0x18 then return Cbor.Ast.fromRef(boxLong(head.toLong))

      if head >= 0x20 && head < 0x38 then
        return Cbor.Ast.fromRef(boxLong(-1L - (head & 0x1F).toLong))

      // Fast path for short text strings (major 3, info 0–23, head 0x60–0x77).
      // These dominate map keys and short literals; a length-prefixed UTF-8
      // payload skips the major-switch and `readLength` chain.
      if head >= 0x60 && head < 0x78 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
        val str = new String(data, pos + 1, length, java.nio.charset.StandardCharsets.UTF_8)
        offset = end
        return Cbor.Ast(str)

      // Fast path for short byte strings (major 2, info 0–23, head 0x40–0x57).
      if head >= 0x40 && head < 0x58 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
        val out = new Array[Byte](length)
        System.arraycopy(data, pos + 1, out, 0, length)
        offset = end
        return Cbor.Ast(out.asInstanceOf[IArray[Byte]])

      val headOffset = pos.toLong
      val major = head >>> 5
      val info = head & 0x1F

      (major: @scala.annotation.switch) match
        case 0 =>
          val length = readLength(info, headOffset)
          if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
          Cbor.Ast.fromRef(boxLong(length))

        case 1 =>
          val length = readLength(info, headOffset)
          if length < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
          if length == Long.MinValue then abort(CborError(Reason.Overflow(headOffset)))
          Cbor.Ast.fromRef(boxLong(-1L - length))

        case 2 =>
          if info == 31 then Cbor.Ast(readIndefiniteByteString())
          else
            val length = boundedLength(readLength(info, headOffset), headOffset)
            Cbor.Ast(readBytes(length))

        case 3 =>
          if info == 31 then Cbor.Ast(readIndefiniteTextString())
          else
            val length = boundedLength(readLength(info, headOffset), headOffset)
            val str = decodeUtf8(data, offset, length, headOffset)
            offset += length
            Cbor.Ast(str)

        case 4 =>
          if info == 31 then
            // Build directly into an `Array[Any]`; flip to parity-padded shape
            // once the Break is seen rather than copying through `IArray.from`
            // and then re-allocating in `Ast.array`.
            val items = scm.ArrayBuffer.empty[Any]
            var done = false

            while !done do
              expect(1)

              if (data(offset) & 0xFF) == Break then
                offset += 1
                done = true
              else
                items += value()

            val count = items.length
            val padded = (count&1) == 0
            val out = new Array[Any](if padded then count + 1 else count)
            var index = 0

            while index < count do
              out(index) = items(index)
              index += 1

            if padded then out(count) = Cbor.Ast.Sentinel
            Cbor.Ast(out.asInstanceOf[IArray[Any]])
          else
            val length = readLength(info, headOffset)

            if length < 0 || length > Int.MaxValue
            then abort(CborError(Reason.Overflow(headOffset)))
            val count = length.toInt
            // Allocate directly in the parity-padded shape used by `Cbor.Ast.array`
            // (odd length, with sentinel pad if logical count is even). One allocation
            // instead of two; no separate IArray.from copy.
            val padded = (count&1) == 0
            val items = new Array[Any](if padded then count + 1 else count)
            var index = 0

            while index < count do
              items(index) = value()
              index += 1

            if padded then items(count) = Cbor.Ast.Sentinel
            Cbor.Ast(items.asInstanceOf[IArray[Any]])

        case 5 =>
          if info == 31 then
            // Build directly into one interleaved `Array[Any]`. The previous
            // shape (two `ArrayBuffer`s + `IArray.from` twice + `Ast.map`'s
            // own `new Array[Any](count*2)` copy) was four allocations and
            // three full passes; one buffer + one `arraycopy`-equivalent loop
            // is enough.
            val items = scm.ArrayBuffer.empty[Any]
            var done = false

            while !done do
              expect(1)

              if (data(offset) & 0xFF) == Break then
                offset += 1
                done = true
              else
                items += value()
                items += value()

            val out = new Array[Any](items.length)
            var index = 0
            while index < items.length do { out(index) = items(index); index += 1 }
            Cbor.Ast(out.asInstanceOf[IArray[Any]])

          else
            val length = readLength(info, headOffset)

            if length < 0 || length > Int.MaxValue
            then abort(CborError(Reason.Overflow(headOffset)))

            val count = length.toInt
            val items = new Array[Any](count*2)
            var index = 0

            while index < count do
              items(index*2) = value()
              items(index*2 + 1) = value()
              index += 1

            Cbor.Ast(items.asInstanceOf[IArray[Any]])

        case 6 =>
          val tag = readLength(info, headOffset)
          if tag < 0 then abort(CborError(Reason.Reserved(headOffset, head)))
          val inner = value()
          Cbor.Ast(Cbor.Tag(tag, inner))

        case 7 =>
          info match
            case 20 => Cbor.Ast(false)
            case 21 => Cbor.Ast(true)
            case 22 => Cbor.Ast(Cbor.CborNull)
            case 23 => Cbor.Ast(vacuous.Unset)
            case 25 => Cbor.Ast(halfToDouble(readUInt16()))
            case 26 => Cbor.Ast(java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble)
            case 27 => Cbor.Ast(java.lang.Double.longBitsToDouble(readUInt64()))
            case 24 => abort(CborError(Reason.BadSimpleValue(headOffset, readUInt8())))
            case 31 => abort(CborError(Reason.UnexpectedBreak(headOffset)))
            case _  => abort(CborError(Reason.BadSimpleValue(headOffset, info)))

        case _ => abort(CborError(Reason.Reserved(headOffset, head)))

    // ── The direct rim ───────────────────────────────────────────────────
    // Byte-level reads for direct parsing (`Cbor.Parsable`): each consumes
    // one complete item, with fast paths for the dominant head shapes and a
    // fallback through the general `value()` path on anything exotic (tags,
    // mistyped items, absence), so values and failures agree with the AST
    // accessors exactly.

    def directLong()(using Tactic[CborError]): Long =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head < 0x18 then
        offset = pos + 1
        head.toLong
      else if head >= 0x20 && head < 0x38 then
        offset = pos + 1
        -1L - (head & 0x1F).toLong
      else
        val major = head >>> 5

        if major == 0 then
          offset = pos + 1
          val length = readLength(head & 0x1F, pos.toLong)
          if length < 0 then abort(CborError(Reason.Reserved(pos.toLong, head)))
          length
        else if major == 1 then
          offset = pos + 1
          val length = readLength(head & 0x1F, pos.toLong)
          if length < 0 then abort(CborError(Reason.Reserved(pos.toLong, head)))
          if length == Long.MinValue then abort(CborError(Reason.Overflow(pos.toLong)))
          -1L - length
        else
          value().long

    def directDouble()(using Tactic[CborError]): Double =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head == 0xFB then
        offset = pos + 1
        java.lang.Double.longBitsToDouble(readUInt64())
      else if head == 0xFA then
        offset = pos + 1
        java.lang.Float.intBitsToFloat(readUInt32().toInt).toDouble
      else if head == 0xF9 then
        offset = pos + 1
        halfToDouble(readUInt16())
      else
        value().double

    def directBoolean()(using Tactic[CborError]): Boolean =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head == 0xF5 then
        offset = pos + 1
        true
      else if head == 0xF4 then
        offset = pos + 1
        false
      else
        value().boolean

    def directString()(using Tactic[CborError]): String =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head >= 0x60 && head < 0x78 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
        val str = new String(data, pos + 1, length, java.nio.charset.StandardCharsets.UTF_8)
        offset = end
        str
      else if (head >>> 5) == 3 then
        offset = pos + 1

        if (head & 0x1F) == 31 then readIndefiniteTextString() else
          val length = boundedLength(readLength(head & 0x1F, pos.toLong), pos.toLong)
          val str = decodeUtf8(data, offset, length, pos.toLong)
          offset += length
          str
      else
        value().string

    def directBytes()(using Tactic[CborError]): IArray[Byte] =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head >= 0x40 && head < 0x58 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(CborError(Reason.Truncated(pos.toLong)))
        val out = new Array[Byte](length)
        System.arraycopy(data, pos + 1, out, 0, length)
        offset = end
        out.asInstanceOf[IArray[Byte]]
      else if (head >>> 5) == 2 then
        offset = pos + 1

        if (head & 0x1F) == 31 then readIndefiniteByteString() else
          val length = boundedLength(readLength(head & 0x1F, pos.toLong), pos.toLong)
          readBytes(length)
      else
        value().byteString

    // The undefined-item peek for optional wrappers: a wire `undefined`
    // (0xF7) reads as an absent value, exactly as the AST path's `optional`.
    def directIsUndefined: Boolean =
      offset < data.length && (data(offset) & 0xFF) == 0xF7

    def directUndefined(): Unit = offset += 1

    // Opens a map, returning its entry count, or -1 for indefinite length.
    // Any other item is consumed whole and reads as an empty map (every
    // field absent), exactly as the AST record decoder's
    // `if root.isMap then root.entries else 0`.
    def directOpenMap()(using Tactic[CborError]): Int =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 5 then
        offset = pos + 1
        val info = head & 0x1F

        if info == 31 then -1 else
          val length = readLength(info, pos.toLong)
          if length < 0 || length > Int.MaxValue then abort(CborError(Reason.Overflow(pos.toLong)))
          length.toInt
      else
        directSkipValue()
        0

    // Opens an array, returning its element count, or -1 for indefinite
    // length. Any other item classifies through the AST accessor, so the
    // failure agrees with the AST collection decoder's `.array`.
    def directOpenArray()(using Tactic[CborError]): Int =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 4 then
        offset = pos + 1
        val info = head & 0x1F

        if info == 31 then -1 else
          val length = readLength(info, pos.toLong)
          if length < 0 || length > Int.MaxValue then abort(CborError(Reason.Overflow(pos.toLong)))
          length.toInt
      else
        value().array
        0

    // Consumes a Break stop code if one is next — the end step of an
    // indefinite-length map or array.
    def directBreak()(using Tactic[CborError]): Boolean =
      if offset >= data.length then abort(CborError(Reason.Truncated(offset.toLong)))

      if (data(offset) & 0xFF) == Break then
        offset += 1
        true
      else
        false

    // The next map key in packed form, for parsers that compare keys against
    // literal constants (generated parsers compile field names to
    // immediates): the packed low word of a definite-length, 1-16 byte,
    // 7-bit-clean text key (its high word left in `directKeyHigh`), or
    // `CborReader.KeyOpaque` without consuming anything — the caller then
    // takes the `directKeyName` step, which consumes the key generally.
    var directKeyHigh: Long = 0L

    def directKeyWord(): Long =
      val pos = offset
      if pos >= data.length then return CborReader.KeyOpaque
      val head = data(pos) & 0xFF

      if head > 0x60 && head <= 0x70 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then return CborReader.KeyOpaque
        var low = 0L
        var high = 0L
        var ascii = 0
        var position = 0

        while position < length do
          val byte = data(pos + 1 + position).toLong & 0xFF
          ascii |= byte.toInt

          if position < 8 then low |= byte << (position*8)
          else high |= byte << ((position - 8)*8)

          position += 1

        if (ascii & 0x80) != 0 then CborReader.KeyOpaque else
          offset = end
          directKeyHigh = high
          low
      else
        CborReader.KeyOpaque

    // The general key step: consumes the key and returns a text key's
    // content, or `null` for a non-text key — whose entry the AST record
    // decoder ignores, so the caller skips its value and continues.
    def directKeyName()(using Tactic[CborError]): String | Null =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 3 then directString()
      else
        directSkipValue()
        null

    // Skips one complete item, building nothing — for unknown keys and
    // non-map-shaped records. Rejects exactly the head shapes `value()`
    // rejects, so a skipped malformed item fails as the AST path (which
    // parses every entry) would.
    def directSkipValue()(using Tactic[CborError]): Unit =
      val pos = offset
      if pos >= data.length then abort(CborError(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF
      offset = pos + 1
      val major = head >>> 5
      val info = head & 0x1F

      (major: @scala.annotation.switch) match
        case 0 | 1 =>
          if readLength(info, pos.toLong) < 0
          then abort(CborError(Reason.Reserved(pos.toLong, head)))

        case 2 | 3 =>
          if info == 31 then
            var done = false

            while !done do
              expect(1)
              val chunkHead = data(offset) & 0xFF

              if chunkHead == Break then
                offset += 1
                done = true
              else
                if (chunkHead >>> 5) != major
                then abort(CborError(Reason.Reserved(offset.toLong, chunkHead)))

                val chunkOffset = offset.toLong
                offset += 1
                val length = boundedLength(readLength(chunkHead & 0x1F, chunkOffset), chunkOffset)
                offset += length
          else
            val length = boundedLength(readLength(info, pos.toLong), pos.toLong)
            offset += length

        case 4 =>
          if info == 31 then
            while !directBreak() do directSkipValue()
          else
            val length = readLength(info, pos.toLong)

            if length < 0 || length > Int.MaxValue
            then abort(CborError(Reason.Overflow(pos.toLong)))

            var index = 0

            while index < length.toInt do
              directSkipValue()
              index += 1

        case 5 =>
          if info == 31 then
            while !directBreak() do
              directSkipValue()
              directSkipValue()
          else
            val length = readLength(info, pos.toLong)

            if length < 0 || length > Int.MaxValue
            then abort(CborError(Reason.Overflow(pos.toLong)))

            var index = 0

            while index < length.toInt do
              directSkipValue()
              directSkipValue()
              index += 1

        case 6 =>
          if readLength(info, pos.toLong) < 0
          then abort(CborError(Reason.Reserved(pos.toLong, head)))

          directSkipValue()

        case 7 =>
          info match
            case 20 | 21 | 22 | 23 => ()

            case 25 =>
              expect(2)
              offset += 2

            case 26 =>
              expect(4)
              offset += 4

            case 27 =>
              expect(8)
              offset += 8

            case 24 => abort(CborError(Reason.BadSimpleValue(pos.toLong, readUInt8())))
            case 31 => abort(CborError(Reason.UnexpectedBreak(pos.toLong)))
            case _  => abort(CborError(Reason.BadSimpleValue(pos.toLong, info)))

        case _ => abort(CborError(Reason.Reserved(pos.toLong, head)))

    // Scans the upcoming map for the given text key and returns its text
    // value, leaving the parser where it started — the dispatch primitive
    // for a sum's discriminant entry, which may appear anywhere in the map.
    // `null` when the item is not a map, has no such key, or the key's
    // value is not text — the caller raises `Absent`, mirroring the AST
    // path's `discriminate(cbor).lest(...)`.
    def directDiscriminant(key: String)(using Tactic[CborError])
    :   String | Null =

      val start = offset

      try
        val head = if offset < data.length then data(offset) & 0xFF else 0
        if (head >>> 5) != 5 then return null
        offset += 1
        val info = head & 0x1F

        var remaining =
          if info == 31 then -1 else
            val length = readLength(info, start.toLong)

            if length < 0 || length > Int.MaxValue
            then abort(CborError(Reason.Overflow(start.toLong)))

            length.toInt

        while remaining != 0 do
          if remaining < 0 && directBreak() then return null
          val name = directKeyName()

          if name != null && name == key then
            val valueHead = if offset < data.length then data(offset) & 0xFF else 0
            return if (valueHead >>> 5) == 3 then directString() else null
          else
            directSkipValue()

          remaining -= 1

        null
      finally offset = start


class Cbor(private[breviloquence] val root: Cbor.Ast) extends Dynamic derives CanEqual:
  def apply(index: Int): Cbor raises CborError = Cbor(root.array(index))

  def selectDynamic(field: String)(using erased dynamicCborEnabler: DynamicCborEnabler): Cbor raises CborError =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased dynamicCborEnabler: DynamicCborEnabler)
  :   Cbor raises CborError =

    apply(field.tt)(index)


  def updateDynamic(field: String)[value: Encodable in Cbor](value: value)
    ( using erased dynamicCborEnabler: DynamicCborEnabler )
  :   Cbor raises CborError =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased dynamicCborEnabler: DynamicCborEnabler)
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

      leftTag.tag == rightTag.tag &&
        recur(leftTag.value.asInstanceOf[Cbor.Ast], rightTag.value.asInstanceOf[Cbor.Ast])

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
            !recur(left.key(index), right.key(index)) ||
              !recur(left.value(index), right.value(index))
          then equal = false

          index += 1

        equal

    else
      false

  def as[value](using decodable: (value is Decodable in Cbor)^): value raises CborError =
    decodable.decoded(this)
