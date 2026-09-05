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

import scala.collection.immutable.Vector

import scala.caps

import java.nio.charset.StandardCharsets
import fulminate.*

import scala.language.dynamics
import scala.language.experimental.pureFunctions

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

import Cbor.Error.{Primitive, Reason}

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
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (inner is Decodable in Cbor)^ )
  =>  ((value is Decodable in Cbor)^{tactic, decodable}) =
    // An honest capability: the instance retains the resolution-scoped tactic and
    // the by-name inner codec (every given that includes a tactic is a capability;
    // Jon, 2026-07-12).
    cbor => if cbor.root.unset then Unset else decodable.decoded(cbor)

  inline given decodable: [value] => value is Decodable in Cbor = summonFrom:
    case given (`value` is Decodable in Text) =>
      provide[Tactic[Cbor.Error]](_.root.string.tt.as[value])

    case given Reflection[`value`] =>
      DecodableDerivation.derived

  // The AST-materializing read path: parse the whole input into a `Cbor`,
  // then decode. Lives at this priority so `object Cbor`'s direct-parsing
  // `aggregableParsed` wins whenever the value has a `Cbor.Parsable`; when
  // it does not, this resolves exactly as before. `source.read[Foo in Cbor]`
  // is shorthand for `source.read[Cbor].as[Foo]`; the `Form` type-tag is
  // added by an `asInstanceOf` cast — `value in Cbor` is just
  // `value { type Form = Cbor }` so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Cbor] => (tactic: Tactic[Cbor.Error])
  =>  (((value in Cbor) is Aggregable by Data)^{tactic}) =
    bytes => Cbor.ast(bytes.read[Cbor.Ast]).as[value].asInstanceOf[value in Cbor]

  inline given encodable: [value] => value is Encodable in Cbor = summonFrom:
    case given (`value` is Encodable in Text) => value => ast(Ast(value.encode.s))
    case given Reflection[`value`]            => EncodableDerivation.derived

  object DecodableDerivation extends Derivable[Decodable in Cbor]:
    // Each outer `focus` runs *after* the inner one (contingency's try/finally order), so a
    // nested record's error must be extended at the ROOT side, landing at `outer.inner` rather
    // than `inner.outer`.
    private def prepend(pointer: Pointer, root: Text): Pointer = pointer match
      case Pointer.Self                 => Pointer(root)
      case Pointer.Child(parent, label) => prepend(parent, root)(label)

    // Scans the venture slots and constructs positionally through the threaded `Mirror` — a
    // plain method: the argument buffer must not be allocated inside an inline expansion,
    // where its fresh root capability leaks into the expansion site's capture sets. Returns
    // an unused null when any slot failed: the caller's accruing scope is tainted, so the
    // result is discarded.
    private final class ArrayProduct(values: Array[Any]^{}) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values.readUnchecked(index)

    private def gate[derivation <: Product]
      ( reflection: ProductReflection[derivation],
        slots:      Array[Venture[Any]]^{},
        active:     Boolean )
    :   derivation =

      var failed = false
      var slot = 0

      if active then
        while slot < slots.length do
          if !slots.readUnchecked(slot).ready then failed = true
          slot += 1

      if failed then null.asInstanceOf[derivation]
      else
        val arguments = Array.allocate[Any](slots.length)
        slot = 0

        while slot < slots.length do
          arguments(slot) = slots.readUnchecked(slot).vouch
          slot += 1

        reflection.fromProduct(ArrayProduct(Array.freeze(arguments)))

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Cbor =

        // The `Tactic` and `Foci` are summoned at the derivation site and supplied explicitly
        // to `decodeRecord`, rather than re-summoned via a `provide` inside the decoder body:
        // that minted a distinct root capability that failed to unify with the polymorphic
        // per-field lambda (and a fresh `Foci` would silently discard pointers). A `Decodable`
        // is `Pure`, so the SAM closing over the summoned capabilities adds nothing to its
        // capture set.
      cbor =>
        decodeRecord[derivation](cbor)
          ( using infer[ProductReflection[derivation]],
                  infer[Foci[Pointer]],
                  infer[Tactic[Cbor.Error]] )

    private inline def decodeRecord[derivation <: Product]
      ( cbor: Cbor )
      ( using reflection: ProductReflection[derivation],
              foci:       Foci[Pointer],
              tactic:     Tactic[Cbor.Error] )
    :   derivation =

      val root = cbor.root
      val count = if root.isMap then root.entries else 0

      // Built immutably: the per-field lambda is polymorphic and must be pure, so it may only
      // close over pure values — a mutable map would be a capability.
      val values: Map[String, Ast] =
        val builder = scala.collection.immutable.Map.newBuilder[String, Ast]
        var index = 0
        while index < count do
          val key = root.key(index)
          if key.isTextString then builder += key.string -> root.value(index)
          index += 1
        builder.result().to(Map)

      // `@name[Cbor]` / bare `@name` renames: field name -> map key, read
      // back the same way they are written.
      val renames: Map[Text, Text] = relabelling[derivation, Cbor]

      // A SINGLE field traversal serving both modes, branching on `foci.active` per field
      // (each additional wisteria traversal re-summons every field's decoder, multiplying
      // inline expansion exponentially with nesting depth). Accruing mode marks a slot failed
      // if its decode registered any focus, and constructs only when every slot is clean —
      // user constructor code never sees a garbage fallback value.
      val active = foci.active

      val slots: Array[Venture[Any]]^{} =
        contexts[derivation]()[Venture[Any]]: [field] =>
          context =>
            val key: Text = renames(label).or(label)

            def decodeNow(): field =
              values.stdlib.get(key.s) match
                case Some(value) => context.decoded(new Cbor(value))
                case None        => default.or(context.decoded(new Cbor(Ast(Unset))))

            if !active then Venture(decodeNow())
            else
              // `focus`'s `Foci` is passed explicitly: an inline def's using parameter would
              // otherwise resolve at this DEFINITION site (to the inert default), not at the
              // expansion site where the validation scope's instance is in context.
              focus(using foci)(prior.lay(Pointer(key))(prepend(_, key))):
                val before = foci.length
                val value: field = decodeNow()
                if foci.length > before then Venture.failed else Venture(value)

      gate[derivation](reflection, slots, active)

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Cbor =
      cbor =>
        provide[Tactic[Cbor.Error]]:
          provide[Tactic[Variant.Error]]:
            val discriminable = infer[derivation is Discriminable in Cbor]

            // `@name[Cbor]` / bare `@name` variant renames: map the serialized
            // discriminator back to the variant name before delegating.
            val variantNames: Map[Text, Text] =
              variantRelabelling[derivation, Cbor].remap: (variant, wire) => wire -> variant

            discriminable.discriminate(cbor).lay:
              // Under an accruing scope, a missing discriminator records ONE error and skips
              // the variant decode without killing the whole scope: the returned value is
              // never used (the caller sees the focus delta, or the tracking scope is
              // tainted), so siblings keep accruing. Fail-fast scopes abort as before.
              if infer[Foci[Pointer]].active then
                raise(Cbor.Error(Reason.Absent))
                null.asInstanceOf[derivation]
              else abort(Cbor.Error(Reason.Absent))

            . apply: wire =>
                val discriminant: Text = variantNames(wire).or(wire)

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
              labels += mapping(label).or(label).s
              values += encoded

        ast(Ast.map(Array.from(labels), Array.from(values)))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Cbor = value =>
      val discriminable = infer[derivation is Discriminable in Cbor]

      // `@name[Cbor]` / bare `@name` variant renames: variant name -> wire
      // discriminator, read back the same way by the decoder.
      val variantNames: Map[Text, Text] = variantRelabelling[derivation, Cbor]

      variant(value): [variant <: derivation] =>
        value =>
          discriminable.rewrite(variantNames(label).or(label), contextual.encode(value))

object Cbor extends Cbor2, Dynamic:
  // CBOR major-type representation in storage. Arrays are stored as an
  // odd-length `Array[Any]^{}` (sentinel-padded if the logical count is even),
  // and maps as an even-length `Array[Any]^{}` with alternating key/value
  // entries; the two share the same JVM type and are told apart by parity.
  type CborInteger   = Long
  type CborFloat     = Double
  type CborText      = String
  type CborBytes     = Array[Byte]^{}
  type CborArray     = Array[Any]^{}
  type CborMap       = Array[Any]^{}
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
    // In the companion (implicit scope), so aggregating a CBOR stream needs no import.
    given aggregable: (tactic: Tactic[Cbor.Error])
    =>  ((Ast is Aggregable by Data)^{tactic}) =
      source => Ast.parse(source.read[Data])

    val Sentinel: AnyRef = new Object

    // Reinterpret a pre-boxed reference as `Ast` without unbox/rebox. Safe
    // because `Ast` is an opaque union whose erasure is `Object`. Useful for
    // callers that already hold a cached `java.lang.Long`, `String`, etc. and
    // want to avoid an auto-boxing round-trip through `apply`.
    private[breviloquence] inline def fromRef(value: AnyRef): Ast =
      value.asInstanceOf[Ast]

    // Via `AnyRef`: the parameter's frozen-array union members freshen to an `any.rd` that
    // cannot flow back into the opaque union's own capture-free members.
    def apply(value: CborTypes): Ast = value.asInstanceOf[AnyRef].asInstanceOf[Ast]

    def map(keys: Array[Any]^{}, values: Array[Any]^{}): Ast =
      val count = keys.length
      val array = Array.allocate[Any](count*2)
      var index = 0

      while index < count do
        array(index*2) = keys.readUnchecked(index)
        array(index*2 + 1) = values.readUnchecked(index)
        index += 1

      Array.freeze(array)

    def array(elements: Array[Any]^{}): Ast =
      val count = elements.length

      if (count&1) == 1 then elements else
        val padded = Array.allocate[Any](count + 1)
        padded.place(elements, 0, 0, count)
        padded(count) = Sentinel
        Array.freeze(padded)

    def length(cbor: Ast): Int =
      val array = cbor.asInstanceOf[scala.Array[AnyRef]]
      val count = array.length
      if count > 0 && (array(count - 1).asInstanceOf[AnyRef] eq Sentinel) then count - 1 else count

    def size(cbor: Ast): Int = cbor.asInstanceOf[Array[Any]^{}].length/2

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

          if long >= 0 then head(out, 0, long) else head(out, 1, -1L - long)

        else if cbor.isFloat then
          out.push((0xE0 | 27).toByte)
          u64(out, java.lang.Double.doubleToLongBits(cbor.asInstanceOf[Double]))

        else if cbor.isTextString then
          val text = cbor.asInstanceOf[String]
          val bytes = Array.unsafeFrozen(text.getBytes(StandardCharsets.UTF_8).nn)
          head(out, 3, bytes.length.toLong)
          out.put(bytes)

        else if cbor.isByteString then
          // `CborBytes` *is* the frozen array, so the erased storage needs no launder.
          val bytes = cbor.asInstanceOf[Array[Byte]^{}]
          head(out, 2, bytes.length.toLong)
          out.put(bytes)

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

      Producer.collect[Data](): producer => write(producer, cbor)

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
          val bytes = cbor.asInstanceOf[scala.Array[Byte]]
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
  given lens: [name <: Label: ValueOf] => (erased dynamicCborEnabler: DynamicCborEnabler) => (tactic: Tactic[Cbor.Error])
  =>  ((name is Lens from Cbor onto Cbor)^{tactic}) =
    // Both lambdas only read through the same resolution-scoped tactic; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      Lens[name, Cbor, Cbor]
       ( (cbor: Cbor) => cbor.selectDynamic(valueOf[name]),
         (cbor: Cbor, value: Cbor) => cbor.modify(valueOf[name], value) )

  given ordinalOptical: [element] => Ordinal is Optical from Cbor onto Cbor = ordinal =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        if n <= ordinal.n0 then origin else Cbor.ast:
          val updated = Array.allocate[Any](n)
          var i = 0

          while i < n do
            updated(i) =
              if i == ordinal.n0 then lambda(Cbor.ast(origin.root.element(i))).root
              else origin.root.element(i)

            i += 1

          Cbor.Ast.array(Array.freeze(updated))
      else
        origin

  given eachOptical: Each.type is Optical from Cbor onto Cbor = _ =>
    Optic: (origin, lambda) =>
      if origin.root.isArray then
        val n = origin.root.elements

        Cbor.ast:
          val updated = Array.allocate[Any](n)
          var i = 0

          while i < n do
            updated(i) = lambda(Cbor.ast(origin.root.element(i))).root
            i += 1

          Cbor.Ast.array(Array.freeze(updated))
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
          val updated = Array.allocate[Any](n)
          var i = 0

          while i < n do
            val element = Cbor.ast(origin.root.element(i))
            updated(i) = (if predicate(element) then lambda(element) else element).root
            i += 1

          Cbor.Ast.array(Array.freeze(updated))
      else
        origin

  given boolean: (tactic: Tactic[Cbor.Error])
  =>  ((Boolean is Decodable in Cbor)^{tactic}) = _.root.boolean
  given double: (tactic: Tactic[Cbor.Error])
  =>  ((Double is Decodable in Cbor)^{tactic}) = _.root.double
  given float: (tactic: Tactic[Cbor.Error])
  =>  ((Float is Decodable in Cbor)^{tactic}) = _.root.double.toFloat
  given long: (tactic: Tactic[Cbor.Error])
  =>  ((Long is Decodable in Cbor)^{tactic}) = _.root.long
  given int: (tactic: Tactic[Cbor.Error])
  =>  ((Int is Decodable in Cbor)^{tactic}) = _.root.long.toInt
  given text: (tactic: Tactic[Cbor.Error])
  =>  ((Text is Decodable in Cbor)^{tactic}) = _.root.string.tt
  given string: (tactic: Tactic[Cbor.Error])
  =>  ((String is Decodable in Cbor)^{tactic}) = _.root.string
  given byteString: (tactic: Tactic[Cbor.Error])
  =>  (((Array[Byte]^{}) is Decodable in Cbor)^{tactic}) = _.root.byteString
  given cbor: Cbor is Decodable in Cbor = identity(_)

  given aggregable: (tactic: Tactic[Cbor.Error])
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

      def parse(reader: Cbor.Reader^): value = parseCarrier(reader.asInstanceOf[AnyRef])

    def apply[value](parser: (reader: Cbor.Reader^) => value)
    :   ((value is Cbor.Parsable)^{parser}) =

      new Cbor.Parsable:
        type Self = value
        def parse(reader: Cbor.Reader^): value = parser(reader)

    // The universal bridge from the AST world: parse one whole item into a
    // `Cbor` and decode it. Field types with only a `Decodable in Cbor`
    // keep working through this, and it is the user's one-line escape hatch
    // when a custom decoder must beat a generated direct parser.
    def fromDecodable[value](decodable: (value is Decodable in Cbor)^)
    :   ((value is Cbor.Parsable)^{decodable}) =

      new Cbor.Parsable:
        type Self = value
        def parse(reader: Cbor.Reader^): value = decodable.decoded(reader.value())

        override def absent()(using Tactic[Cbor.Error]): value =
          decodable.decoded(Cbor.ast(Ast(Unset)))

    // A required field whose key was absent from the map. Public because
    // generated parsers are spliced into user modules.
    def missing[value]()(using Tactic[Cbor.Error]): value = abort(Cbor.Error(Reason.Absent))

    // The call points for a nominal `Parsable` in a field position of a
    // *generated* parser (a recursive record's own instance, or a
    // hand-written one). Both travel as neutral carriers — generated code
    // is capture-erased — and the capability is reasserted here, at the
    // audited point, exactly as the reader's own rim accessors do.
    def parseField[value](parsable: AnyRef, reader: AnyRef): value =
      parsable.asInstanceOf[value is Cbor.Parsable].parse(reader.asInstanceOf[Cbor.Reader^])

    def absentField[value](parsable: AnyRef)(using Tactic[Cbor.Error]): value =
      parsable.asInstanceOf[value is Cbor.Parsable].absent()

  // The direct-parsing counterpart of `Decodable in Cbor`: consumes data
  // items straight off the input bytes through a `Cbor.Reader` instead of
  // walking a materialized `Cbor.Ast`, so `read[value in Cbor]` can
  // instantiate values without building the AST. `Parsable` is the opt-in
  // surface: explicit instances and `Cbor.Inlinable.parsable`. It has no
  // blanket fallback given, so no read changes behavior until a type opts
  // in; field types without one bridge through `Parsable.fromDecodable`.
  trait Parsable extends distillate.Parsable:
    type Transport = Cbor
    type Reader = Cbor.Reader

    // What a field of this type yields when its key is absent from the map,
    // mirroring the AST path's `decoded(Cbor(Ast(Unset)))`: an abort unless
    // overridden.
    def absent()(using Tactic[Cbor.Error]): Self = abort(Cbor.Error(Reason.Absent))

  // Direct-parsing counterpart of the `aggregable`/`aggregableIn` path:
  // drives a `Cbor.Parsable` instance over the input through a
  // `Cbor.Reader`, so no AST is built for the items the instance reads
  // directly. Trailing bytes are rejected exactly as `Parser.parse`.
  private def parseDirect[value]
    ( input: Data, parsable: (value is Cbor.Parsable)^ )
    ( using tactic: Tactic[Cbor.Error] )
  :   value =

    val parser = Parser(input)
    val result = parsable.parse(Cbor.Reader(parser, tactic))

    if parser.offset < parser.data.length
    then abort(Cbor.Error(Reason.Trailing(parser.offset.toLong)))

    result

  // Direct parsing: when the value knows how to consume CBOR items itself,
  // the AST is never materialized. Declared here (not in `Cbor2`, where the
  // `Decodable`-based `aggregableIn` lives) so it wins whenever a
  // `Cbor.Parsable` exists, and is otherwise inapplicable — existing code
  // resolves exactly as before. Sealed per the codec-thunk pattern: the
  // instance retains the resolution-scoped parsable and tactic.
  given aggregableParsed: [value]
  =>  (parsable: (value is Cbor.Parsable)^)
  =>  (tactic: Tactic[Cbor.Error])
  =>  ((value in Cbor) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      bytes => parseDirect(bytes.read[Data], parsable).asInstanceOf[value in Cbor]

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream.
  // Concrete in `Data`, so it beats the composed pipeline by specificity.
  // Sealed like `aggregableParsed` above.
  given readableParsed: [value]
  =>  (parsable: (value is Cbor.Parsable)^)
  =>  (tactic: Tactic[Cbor.Error])
  =>  (Data is Readable to (value in Cbor)) =

    caps.unsafe.unsafeAssumePure:
      data => parseDirect(data, parsable).asInstanceOf[value in Cbor]

  given unit: (tactic: Tactic[Cbor.Error])
  =>  ((Unit is Decodable in Cbor)^{tactic}) =
    value =>
      if !value.root.nullary then
        val reason =
          if value.root.unset then Reason.Absent
          else Reason.NotType(value.root.primitive, Primitive.Null)

        abort(Cbor.Error(reason))

  given option: [value: Decodable in Cbor] => Tactic[Cbor.Error]
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
  given bytesEncodable: (Array[Byte]^{}) is Encodable in Cbor = bytes => ast(Ast(bytes))
  given cborEncodable: Cbor is Encodable in Cbor = identity(_)

  // The collection instances below are honest capabilities: each retains its by-name
  // element codec (and, where present, a resolution-scoped `Tactic`), which share the
  // instance's given-resolution lifetime (every given that includes a tactic is a
  // capability; Jon, 2026-07-12). See rep/DECISIONS.md.
  given listEncodable: [list <: List, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((list[element] is Encodable in Cbor)^{encodable}) =
    values =>
      val roots: Array[Any]^{} =
        values.map { value => encodable.encoded(value).root: Any }.to[Array]

      ast(Ast.array(roots))

  given setEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((set[element] is Encodable in Cbor)^{encodable}) =
    values =>
      val roots: Array[Any]^{} =
        values.map { value => encodable.encoded(value).root: Any }.to[Array]

      ast(Ast.array(roots))


  given seriesEncodable: [sequence <: Sequence, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((sequence[element] is Encodable in Cbor)^{encodable}) =
    values =>
      val roots: Array[Any]^{} =
        values.map { value => encodable.encoded(value).root: Any }.to[Array]

      ast(Ast.array(roots))

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: sc.Factory[element, collection[element]], tactic:  Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((collection[element] is Decodable in Cbor)^{tactic, decodable}) =

    // An honest capability, as `optional` above.
    value =>
        val builder = factory.newBuilder
        value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))

        builder.result()


  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each inlines `collectionDecodable`'s loop at the underlying
  // stdlib type (using the by-name `decodable` directly, so a recursive
  // derivation — `List[Tree]` inside `Tree` — ties the knot exactly as the
  // Iterable instance did before the flip) and casts.
  given listDecodable: [list <: List, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((list[element] is Decodable in Cbor)^{tactic, decodable}) =
    value =>
      val builder = scala.collection.immutable.List.newBuilder[element]
      value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))
      builder.result().asInstanceOf[list[element]]

  given setDecodable: [set <: Set, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((set[element] is Decodable in Cbor)^{tactic, decodable}) =
    value =>
      val builder = scala.collection.immutable.Set.newBuilder[element]
      value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))
      builder.result().asInstanceOf[set[element]]

  given seriesDecodable: [sequence <: Sequence, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((sequence[element] is Decodable in Cbor)^{tactic, decodable}) =
    value =>
      val builder = Vector.newBuilder[element]
      value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))
      builder.result().asInstanceOf[sequence[element]]

  given mapDecodable: [key: Decodable in Text, element]
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ((Map[key, element] is Decodable in Cbor)^{tactic, decodable}) =

    // An honest capability, as `optional` above.
    value =>
        val root = value.root
        val count = if root.isMap then root.entries else 0
        var index = 0
        var map = Map.empty[key, element]

        while index < count do
          val key = root.key(index)

          if key.isTextString
          then map = map.define(key.string.tt.as, decodable.decoded(ast(root.value(index))))
          else abort(Cbor.Error(Reason.NonStringKey))

          index += 1

        map

  given mapEncodable: [key: Encodable in Text, element]
  =>  ( encodable: element is Encodable in Cbor )
  =>  Map[key, element] is Encodable in Cbor =

    map =>
      val keys: List[key] = map.keys.to[List]
      val values: Array[Any]^{} = keys.map { key => map(key).encode.root: Any }.to[Array]
      val names: Array[Any]^{} = keys.map { key => key.encode.s: Any }.to[Array]
      ast(Ast.map(names, values))

  def applyDynamicNamed(methodName: "make")(elements: (String, Cbor)*): Cbor =
    val keys: Array[Any]^{} = Array.from(elements.map(_(0): Any))
    val values: Array[Any]^{} = Array.from(elements.map(_(1).root.asInstanceOf[Any]))
    Cbor(Ast.map(keys, values))

  // The map-key-discriminated `Discriminable` shape, as a nameable class so
  // that generated parsers (which dispatch on the discriminant
  // monomorphically) can recognize the shape and extract the key at
  // expansion time.
  final class DiscriminantKey[derivation](val key: Text) extends Discriminable:
    type Form = Cbor
    type Self = derivation

    import dynamicAccess.dynamicCbor

    def rewrite(kind: Text, cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key.s)(kind))
    def discriminate(cbor: Cbor): Optional[Text] =
      // The optional tactic is created and consumed here; no aliased writer.
      scala.caps.unsafe.unsafeAssumeSeparate(safely(cbor.selectDynamic(key.s).as[Text]))
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

    private val longCache: Array[AnyRef]^{} =
      val out = Array.allocate[AnyRef](LongCacheSize)
      var index = 0

      while index < LongCacheSize do
        out(index) = java.lang.Long.valueOf(index.toLong).nn
        index += 1

      Array.freeze(out)

    private inline def boxLong(value: Long): AnyRef =
      if value >= 0L && value < LongCacheSize then longCache.readUnchecked(value.toInt)
      else java.lang.Long.valueOf(value).nn

    def parse(source: Array[Byte]^{}): Cbor.Ast raises Cbor.Error =
      val parser = new Parser(source)
      val result = parser.value()

      if parser.offset < parser.data.length
      then abort(Cbor.Error(Reason.Trailing(parser.offset.toLong)))

      result

  // The class is public — generated parsers, spliced into user modules,
  // bind it once per record and read through its direct rim — but only
  // breviloquence's read paths can construct one.
  final class Parser private[breviloquence] (input: Array[Byte]^{}):
    import Parser.{Break, boxLong}

    // Cache the underlying primitive array so reads compile to BALOAD rather
    // than going through the frozen-array read shim. `data.length` is constant-folded by
    // the JIT and cheaper than going through a separate `length` accessor.
    @scala.caps.unsafe.untrackedCaptures
    private[breviloquence] val data: scala.Array[Byte] = input.asInstanceOf[scala.Array[Byte]]

    // `offset` is exposed only to the package-private parse() entry point so it
    // can detect trailing bytes after a successful parse. All hot-path reads
    // mutate it directly through the JVM PUTFIELD/GETFIELD.
    @scala.caps.unsafe.untrackedCaptures
    var offset: Int = 0

    // These inline helpers take an explicit `Tactic` clause rather than `raises` sugar: the
    // context-function result the sugar expands to synthesizes a closure per inline expansion,
    // and from the 2026-07-17 upstream nightlies (#26547) a second expansion in the same
    // method fails cc root-visibility against the first expansion's memoized root capability.
    private inline def expect(count: Int)(using Tactic[Cbor.Error]): Unit =
      if data.length - offset < count then abort(Cbor.Error(Reason.Truncated(offset.toLong)))

    private inline def readByte(): Int =
      (data(offset)&0xFF).also(offset += 1)

    private inline def readUInt8()(using Tactic[Cbor.Error]): Int =
      expect(1)
      readByte()

    private inline def readUInt16()(using Tactic[Cbor.Error]): Int =
      expect(2)
      val pos = offset
      offset = pos + 2
      ((data(pos) & 0xFF) << 8) | (data(pos + 1) & 0xFF)

    private inline def readUInt32()(using Tactic[Cbor.Error]): Long =
      expect(4)
      val pos = offset
      offset = pos + 4
      ((data(pos) & 0xFFL) << 24) |
        ((data(pos + 1) & 0xFFL) << 16) |
        ((data(pos + 2) & 0xFFL) << 8) |
        (data(pos + 3) & 0xFFL)

    private inline def readUInt64()(using Tactic[Cbor.Error]): Long =
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
    private inline def readLength(info: Int, headOffset: Long)(using Tactic[Cbor.Error]): Long =
      if info < 24 then info.toLong
      else info match
        case 24 => readUInt8().toLong
        case 25 => readUInt16().toLong
        case 26 => readUInt32()

        case 27 =>
          val v = readUInt64()
          // Bit 63 set means the value > Long.MaxValue; CBOR allows this for
          // major types 0/1 but breviloquence rejects it.
          if v < 0 then abort(Cbor.Error(Reason.Overflow(headOffset)))
          v

        case 31 => -1L
        case _  => abort(Cbor.Error(Reason.Reserved(headOffset, info)))

    private def readBytes(length: Int): Array[Byte]^{} =
      val result = Array.allocate[Byte](length)
      System.arraycopy(data, offset, result.raw, 0, length)
      offset += length
      Array.freeze(result)

    private inline def boundedLength(length: Long, headOffset: Long)(using Tactic[Cbor.Error]): Int =
      if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
      val count = length.toInt
      expect(count)
      count

    // Reads an indefinite-length byte string by concatenating its definite-
    // length chunks (each prefixed with major type 2) until a Break stop code.
    // Chunk bytes flow through `Scribe`'s bulk `append` (one `System.arraycopy` per chunk)
    // rather than a byte at a time, which is why the JDK buffer was reached for here.
    private def readIndefiniteByteString(): Array[Byte]^{} raises Cbor.Error =
      Array.collect[Byte](): buffer =>
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
            if major != 2 then abort(Cbor.Error(Reason.Reserved(offset.toLong, head)))
            val chunkOffset = offset.toLong
            offset += 1
            val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
            buffer.append(Array.unsafeFrozen(data), offset, length)
            offset += length

    private def readIndefiniteTextString(): String raises Cbor.Error =
      val collected = Array.collect[Byte](): buffer =>
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
            if major != 3 then abort(Cbor.Error(Reason.Reserved(offset.toLong, head)))
            val chunkOffset = offset.toLong
            offset += 1
            val length = boundedLength(readLength(info, chunkOffset), chunkOffset)
            buffer.append(Array.unsafeFrozen(data), offset, length)
            offset += length

      val bytes = Array.unsafeJvm(collected)

      decodeUtf8(bytes, 0, bytes.length, 0L)

    private inline def decodeUtf8
      ( bytes: scala.Array[Byte], start: Int, length: Int, errorOffset: Long )
      ( using Tactic[Cbor.Error] )
    :   String =

      try new String(bytes, start, length, java.nio.charset.StandardCharsets.UTF_8)
      catch case _: Throwable => abort(Cbor.Error(Reason.InvalidUtf8(errorOffset)))

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

    def value(): Cbor.Ast raises Cbor.Error =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
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
        if end > data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
        val str = new String(data, pos + 1, length, java.nio.charset.StandardCharsets.UTF_8)
        offset = end
        return Cbor.Ast(str)

      // Fast path for short byte strings (major 2, info 0–23, head 0x40–0x57).
      if head >= 0x40 && head < 0x58 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
        val out = Array.allocate[Byte](length)
        System.arraycopy(data, pos + 1, out.raw, 0, length)
        offset = end
        return Cbor.Ast(Array.freeze(out))

      val headOffset = pos.toLong
      val major = head >>> 5
      val info = head & 0x1F

      (major: @scala.annotation.switch) match
        case 0 =>
          val length = readLength(info, headOffset)
          if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
          Cbor.Ast.fromRef(boxLong(length))

        case 1 =>
          val length = readLength(info, headOffset)
          if length < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
          if length == Long.MinValue then abort(Cbor.Error(Reason.Overflow(headOffset)))
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
            // once the Break is seen rather than copying through `Array.from`
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
            val out = Array.allocate[Any](if padded then count + 1 else count)
            var index = 0

            while index < count do
              out(index) = items(index)
              index += 1

            if padded then out(count) = Cbor.Ast.Sentinel
            Cbor.Ast(Array.freeze(out))
          else
            val length = readLength(info, headOffset)

            if length < 0 || length > Int.MaxValue
            then abort(Cbor.Error(Reason.Overflow(headOffset)))
            val count = length.toInt
            // Allocate directly in the parity-padded shape used by `Cbor.Ast.array`
            // (odd length, with sentinel pad if logical count is even). One allocation
            // instead of two; no separate Array.from copy.
            val padded = (count&1) == 0
            val items = Array.allocate[Any](if padded then count + 1 else count)
            var index = 0

            while index < count do
              items(index) = value()
              index += 1

            if padded then items(count) = Cbor.Ast.Sentinel
            Cbor.Ast(Array.freeze(items))

        case 5 =>
          if info == 31 then
            // Build directly into one interleaved `Array[Any]`. The previous
            // shape (two `ArrayBuffer`s + `Array.from` twice + `Ast.map`'s
            // own `new scala.Array[Any](count*2)` copy) was four allocations and
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

            val out = Array.allocate[Any](items.length)
            var index = 0
            while index < items.length do { out(index) = items(index); index += 1 }
            Cbor.Ast(Array.freeze(out))

          else
            val length = readLength(info, headOffset)

            if length < 0 || length > Int.MaxValue
            then abort(Cbor.Error(Reason.Overflow(headOffset)))

            val count = length.toInt
            val items = Array.allocate[Any](count*2)
            var index = 0

            while index < count do
              items(index*2) = value()
              items(index*2 + 1) = value()
              index += 1

            Cbor.Ast(Array.freeze(items))

        case 6 =>
          val tag = readLength(info, headOffset)
          if tag < 0 then abort(Cbor.Error(Reason.Reserved(headOffset, head)))
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
            case 24 =>
              // The error message reads this parser only to render its diagnostic detail.
              val value = readUInt8()
              scala.caps.unsafe.unsafeAssumeSeparate:
                abort(Cbor.Error(Reason.BadSimpleValue(headOffset, value)))
            case 31 => abort(Cbor.Error(Reason.UnexpectedBreak(headOffset)))
            case _  => abort(Cbor.Error(Reason.BadSimpleValue(headOffset, info)))

        case _ => abort(Cbor.Error(Reason.Reserved(headOffset, head)))

    // ── The direct rim ───────────────────────────────────────────────────
    // Byte-level reads for direct parsing (`Cbor.Parsable`): each consumes
    // one complete item, with fast paths for the dominant head shapes and a
    // fallback through the general `value()` path on anything exotic (tags,
    // mistyped items, absence), so values and failures agree with the AST
    // accessors exactly.

    def directLong()(using Tactic[Cbor.Error]): Long =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
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
          if length < 0 then abort(Cbor.Error(Reason.Reserved(pos.toLong, head)))
          length
        else if major == 1 then
          offset = pos + 1
          val length = readLength(head & 0x1F, pos.toLong)
          if length < 0 then abort(Cbor.Error(Reason.Reserved(pos.toLong, head)))
          if length == Long.MinValue then abort(Cbor.Error(Reason.Overflow(pos.toLong)))
          -1L - length
        else
          value().long

    def directDouble()(using Tactic[Cbor.Error]): Double =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
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

    def directBoolean()(using Tactic[Cbor.Error]): Boolean =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head == 0xF5 then
        offset = pos + 1
        true
      else if head == 0xF4 then
        offset = pos + 1
        false
      else
        value().boolean

    def directString()(using Tactic[Cbor.Error]): String =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head >= 0x60 && head < 0x78 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
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

    def directBytes()(using Tactic[Cbor.Error]): Array[Byte]^{} =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if head >= 0x40 && head < 0x58 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
        val out = Array.allocate[Byte](length)
        System.arraycopy(data, pos + 1, out.raw, 0, length)
        offset = end
        Array.freeze(out)
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
    def directOpenMap()(using Tactic[Cbor.Error]): Int =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 5 then
        offset = pos + 1
        val info = head & 0x1F

        if info == 31 then -1 else
          val length = readLength(info, pos.toLong)
          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(pos.toLong)))
          length.toInt
      else
        directSkipValue()
        0

    // Opens an array, returning its element count, or -1 for indefinite
    // length. Any other item classifies through the AST accessor, so the
    // failure agrees with the AST collection decoder's `.array`.
    def directOpenArray()(using Tactic[Cbor.Error]): Int =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 4 then
        offset = pos + 1
        val info = head & 0x1F

        if info == 31 then -1 else
          val length = readLength(info, pos.toLong)
          if length < 0 || length > Int.MaxValue then abort(Cbor.Error(Reason.Overflow(pos.toLong)))
          length.toInt
      else
        value().array
        0

    // Consumes a Break stop code if one is next — the end step of an
    // indefinite-length map or array.
    def directBreak()(using Tactic[Cbor.Error]): Boolean =
      if offset >= data.length then abort(Cbor.Error(Reason.Truncated(offset.toLong)))

      if (data(offset) & 0xFF) == Break then
        offset += 1
        true
      else
        false

    // The next map key in packed form, for parsers that compare keys against
    // literal constants (generated parsers compile field names to
    // immediates): the packed low word of a definite-length, 1-16 byte,
    // 7-bit-clean text key (its high word left in `directKeyHigh`), or
    // `Cbor.Reader.KeyOpaque` without consuming anything — the caller then
    // takes the `directKeyName` step, which consumes the key generally.
    @scala.caps.unsafe.untrackedCaptures
    var directKeyHigh: Long = 0L

    def directKeyWord(): Long =
      val pos = offset
      if pos >= data.length then return Cbor.Reader.KeyOpaque
      val head = data(pos) & 0xFF

      if head > 0x60 && head <= 0x70 then
        val length = head & 0x1F
        val end = pos + 1 + length
        if end > data.length then return Cbor.Reader.KeyOpaque
        var low = 0L
        var high = 0L
        var ascii = 0
        var position = 0

        while position < length do
          val byte = data(pos + 1 + position).toLong & 0xFF
          ascii |= byte.toInt

          if position < 8 then low |= byte << (position*8) else high |= byte << ((position - 8)*8)

          position += 1

        if (ascii & 0x80) != 0 then Cbor.Reader.KeyOpaque else
          offset = end
          directKeyHigh = high
          low
      else
        Cbor.Reader.KeyOpaque

    // The general key step: consumes the key and returns a text key's
    // content, or `null` for a non-text key — whose entry the AST record
    // decoder ignores, so the caller skips its value and continues.
    def directKeyName()(using Tactic[Cbor.Error]): String | Null =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF

      if (head >>> 5) == 3 then directString()
      else
        directSkipValue()
        null

    // Skips one complete item, building nothing — for unknown keys and
    // non-map-shaped records. Rejects exactly the head shapes `value()`
    // rejects, so a skipped malformed item fails as the AST path (which
    // parses every entry) would.
    def directSkipValue()(using Tactic[Cbor.Error]): Unit =
      val pos = offset
      if pos >= data.length then abort(Cbor.Error(Reason.Truncated(pos.toLong)))
      val head = data(pos) & 0xFF
      offset = pos + 1
      val major = head >>> 5
      val info = head & 0x1F

      (major: @scala.annotation.switch) match
        case 0 | 1 =>
          if readLength(info, pos.toLong) < 0
          then abort(Cbor.Error(Reason.Reserved(pos.toLong, head)))

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
                then abort(Cbor.Error(Reason.Reserved(offset.toLong, chunkHead)))

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
            then abort(Cbor.Error(Reason.Overflow(pos.toLong)))

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
            then abort(Cbor.Error(Reason.Overflow(pos.toLong)))

            var index = 0

            while index < length.toInt do
              directSkipValue()
              directSkipValue()
              index += 1

        case 6 =>
          if readLength(info, pos.toLong) < 0
          then abort(Cbor.Error(Reason.Reserved(pos.toLong, head)))

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

            case 24 =>
              // As above.
              val value = readUInt8()
              scala.caps.unsafe.unsafeAssumeSeparate:
                abort(Cbor.Error(Reason.BadSimpleValue(pos.toLong, value)))
            case 31 => abort(Cbor.Error(Reason.UnexpectedBreak(pos.toLong)))
            case _  => abort(Cbor.Error(Reason.BadSimpleValue(pos.toLong, info)))

        case _ => abort(Cbor.Error(Reason.Reserved(pos.toLong, head)))

    // Scans the upcoming map for the given text key and returns its text
    // value, leaving the parser where it started — the dispatch primitive
    // for a sum's discriminant entry, which may appear anywhere in the map.
    // `null` when the item is not a map, has no such key, or the key's
    // value is not text — the caller raises `Absent`, mirroring the AST
    // path's `discriminate(cbor).lest(...)`.
    def directDiscriminant(key: String)(using Tactic[Cbor.Error])
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
            then abort(Cbor.Error(Reason.Overflow(start.toLong)))

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

  // CborError → Cbor.Error
  object Error:
    object Primitive:
      given communicable: Primitive is Communicable =
        case Integer    => m"integer"
        case Float      => m"float"
        case ByteString => m"byte string"
        case TextString => m"text string"
        case Array      => m"array"
        case Map        => m"map"
        case Tag        => m"tag"
        case Boolean    => m"boolean"
        case Null       => m"null"
        case Undefined  => m"undefined"

    enum Primitive:
      case Integer, Float, ByteString, TextString, Array, Map, Tag, Boolean, Null, Undefined

    object Reason:
      given communicable: Reason is Communicable =
        case Truncated(offset)        => m"the input was truncated at byte $offset"
        case InvalidUtf8(offset)      => m"invalid UTF-8 was found at byte $offset"
        case Overflow(offset)         => m"an integer too large for Long was found at byte $offset"
        case UnexpectedBreak(offset)  => m"an unexpected break stop code was found at byte $offset"
        case Trailing(offset)         => m"unexpected trailing bytes were found from byte $offset"
        case OutOfRange               => m"the array index was out of range"
        case NotType(found, expected) => m"the CBOR value had type $found instead of $expected"
        case NonStringKey             => m"the map key was not a string"
        case Absent                   => m"the CBOR value was not present"

        case Reserved(offset, byte) =>
          m"a reserved CBOR head byte ${byte.toString} was found at byte $offset"

        case BadSimpleValue(offset, value) =>
          m"an invalid simple value ${value.toString} was found at byte $offset"

    enum Reason(val number: Int) extends Clarification:
      case Truncated(offset: Long) extends Reason(1)
      case Reserved(offset: Long, byte: Int) extends Reason(2)
      case BadSimpleValue(offset: Long, value: Int) extends Reason(3)
      case InvalidUtf8(offset: Long) extends Reason(4)
      case Overflow(offset: Long) extends Reason(5)
      case UnexpectedBreak(offset: Long) extends Reason(6)
      case Trailing(offset: Long) extends Reason(7)
      case OutOfRange extends Reason(8)
      case NotType(found: Primitive, expected: Primitive) extends Reason(9)
      case NonStringKey extends Reason(10)
      case Absent extends Reason(11)

  case class Error(reason: Cbor.Error.Reason)(using Diagnostics)
  extends fulminate.Error(595, reason.number)(m"could not process the CBOR value because $reason")

  // CborReader → Cbor.Reader
  object Reader:
    // Sentinel of `keyWord()`; impossible as a packed key, whose bytes are all
    // 7-bit ASCII.
    inline final val KeyOpaque = -2L

    // Only breviloquence's read path (`Cbor.parseDirect`) constructs readers,
    // so the exclusivity of the wrapped parser and the resolution scope of the
    // carried tactic are preserved by construction. The wrapped tactic travels
    // as a neutral carrier (jacinta's `Json.Reader` pattern): the field stays
    // pure, and each accessor reasserts the type at the rim — the audited
    // point.
    private[breviloquence] def apply(parser: Cbor.Parser, tactic: Tactic[Cbor.Error])
    :   Cbor.Reader^ =

      new Cbor.Reader(parser, tactic.asInstanceOf[AnyRef])

  // The public, restricted rim of the CBOR parser, handed to `Cbor.Parsable`
  // instances so they can consume data items straight off the input without an
  // intermediate `Cbor.Ast`. Each method consumes exactly one item (or one
  // structural step). The reader carries its own `Tactic[Cbor.Error]` — CBOR's
  // single error type covers both malformed input and mistyped items — so
  // instance `parse` bodies need no error vocabulary: failures abort through
  // the read call's ambient tactic.
  //
  // An exclusive, stateful capability, like the parser it wraps: it is owned
  // by one `Cbor.Parsable.parse` call at a time, for the duration of that
  // call, and nothing of it may be retained afterwards.
  final class Reader private (parser0: AnyRef, tactic0: AnyRef)
  extends caps.ExclusiveCapability, caps.Stateful:
    private inline def parser: Cbor.Parser = parser0.asInstanceOf[Cbor.Parser]

    // The sealed conduit for generated parsers: package-private, so the only
    // path to the wrapped capabilities from outside breviloquence is through
    // the accessor the compiler synthesizes for breviloquence's own
    // macro-generated splices — hand-written code cannot name it. Generated
    // code binds the parser once per record and reads through `Cbor.Parser`'s
    // direct rim without this class's per-item forwarders.
    private[breviloquence] def rawParser: AnyRef = parser0
    private[breviloquence] def rawTactic: AnyRef = tactic0
    private inline def tactic: Tactic[Cbor.Error] = tactic0.asInstanceOf[Tactic[Cbor.Error]]

    // ── Scalars: one data item each. Values and failures agree with the
    // `Cbor.Ast` accessors exactly, so direct and AST reads yield equal
    // values — integers coerce to floats and vice versa, as `.long` and
    // `.double` do. ──
    inline update def long(): Long = parser.directLong()(using tactic)
    inline update def int(): Int = parser.directLong()(using tactic).toInt
    inline update def double(): Double = parser.directDouble()(using tactic)
    inline update def boolean(): Boolean = parser.directBoolean()(using tactic)
    update def text(): Text = parser.directString()(using tactic).tt
    update def string(): String = parser.directString()(using tactic)
    update def byteString(): Array[Byte]^{} = parser.directBytes()(using tactic)

    // ── Undefined handling: `hasUndefined` peeks without consuming, for
    // optional wrappers that map a wire `undefined` (0xF7) to an absent
    // value, exactly as the AST path's `optional`. ──
    update def hasUndefined: Boolean = parser.directIsUndefined
    update def undefined(): Unit = parser.directUndefined()

    // ── Structure. `openMap()` and `openArray()` yield the entry or element
    // count, or -1 for an indefinite-length item, whose end is a Break stop
    // code consumed by `breakEnd()`. A non-map item under `openMap()` reads
    // as an empty map (the AST record decoder's semantics); a non-array item
    // under `openArray()` fails as the AST `.array` accessor. ──
    inline update def openMap(): Int = parser.directOpenMap()(using tactic)
    inline update def openArray(): Int = parser.directOpenArray()(using tactic)
    inline update def breakEnd(): Boolean = parser.directBreak()(using tactic)

    // The next map key in packed form, for parsers that compare keys against
    // literal constants (generated parsers compile field names to
    // immediates): the packed low word of the key (its high word from
    // `keyHigh`), or `KeyOpaque` when the key cannot be packed — the caller
    // then takes the `keyName` step instead, which consumes it generally.
    update def keyWord(): Long = parser.directKeyWord()

    update def keyHigh: Long = parser.directKeyHigh

    // The general key step: a text key's content, or `null` for a non-text
    // key, whose entry is ignored — the caller skips its value.
    update def keyName(): String | Null = parser.directKeyName()(using tactic)

    // ── The fallback seam: parse one whole item into an AST (for field types
    // that only have a `Decodable in Cbor`), or skip one whole item (for
    // unknown keys). ──
    update def value(): Cbor = Cbor.ast(parser.value()(using tactic))
    inline update def skipValue(): Unit = parser.directSkipValue()(using tactic)

    // Scans the upcoming map for the given key and returns its text value,
    // leaving the reader where it started — the dispatch primitive for a
    // sum's discriminant entry, which may appear anywhere in the map. `Unset`
    // when the item has no such key or its value is not text.
    update def discriminant(key: Text): Optional[Text] =
      parser.directDiscriminant(key.s)(using tactic) match
        case null        => Unset
        case tag: String => tag.tt


class Cbor(private[breviloquence] val root: Cbor.Ast) extends Dynamic derives CanEqual:
  def apply(index: Int): Cbor raises Cbor.Error = Cbor(root.array.readUnchecked(index))

  def selectDynamic(field: String)(using erased dynamicCborEnabler: DynamicCborEnabler): Cbor raises Cbor.Error =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased dynamicCborEnabler: DynamicCborEnabler)
  :   Cbor raises Cbor.Error =

    apply(field.tt)(index)


  def updateDynamic(field: String)[value: Encodable in Cbor](value: value)
    ( using erased dynamicCborEnabler: DynamicCborEnabler )
  :   Cbor raises Cbor.Error =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased dynamicCborEnabler: DynamicCborEnabler)
  :   Cbor raises Cbor.Error =

    delete(field)


  private[breviloquence] def modify(field: String, value: Cbor): Cbor raises Cbor.Error =
    if !root.isMap then abort(Cbor.Error(Reason.NotType(root.primitive, Primitive.Map)))
    val array = root.asInstanceOf[Array[Any]^{}]
    val length = array.length

    root.index(field) match
      case -1 =>
        val out = Array.allocate[Any](length + 2)
        out.place(array, 0, 0, length)
        out(length) = field
        out(length + 1) = value.root
        Cbor.ast(Cbor.Ast(Array.freeze(out)))

      case index =>
        val out = Array.allocate[Any](length)
        out.place(array, 0, 0, length)
        out(index*2 + 1) = value.root
        Cbor.ast(Cbor.Ast(Array.freeze(out)))

  private[breviloquence] def delete(field: String): Cbor raises Cbor.Error =
    if !root.isMap then abort(Cbor.Error(Reason.NotType(root.primitive, Primitive.Map)))
    val array = root.asInstanceOf[scala.Array[Any]]
    val length = array.length

    root.index(field) match
      case -1 => Cbor.ast(root)

      case index =>
        val out = Array.allocate[Any](length - 2)
        System.arraycopy(array, 0, out.raw, 0, index*2)

        System.arraycopy(array, index*2 + 2, out.raw, index*2, length - index*2 - 2)
        Cbor.ast(Cbor.Ast(Array.freeze(out)))

  def apply(field: Text): Cbor raises Cbor.Error =
    if root.unset then Cbor.ast(Cbor.Ast(Unset))
    else if !root.isMap then abort(Cbor.Error(Reason.NotType(root.primitive, Primitive.Map)))
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
    then java.util.Arrays.equals(left.asInstanceOf[scala.Array[Byte]], right.asInstanceOf[scala.Array[Byte]])
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

  def as[value](using decodable: (value is Decodable in Cbor)^)
  :   (Tactic[Cbor.Error]^) ?->{decodable} value =
    decodable.decoded(this)
