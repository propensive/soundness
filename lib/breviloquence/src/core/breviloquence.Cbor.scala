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

  // The `optionalityOptions` policies, captured at resolution: an absent key (or a wire
  // `undefined`, which is literally `Unset`) and a wire `null` read as `Unset` unless strict,
  // and a value the inner decoder rejects reads as `Unset` only under lenient faults, which
  // decode under `tactic.tolerate`. A strict null is handed to the inner decoder.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( absence: Decodable.Absence in Cbor,
        nullity: Decodable.Nullity in Cbor,
        fault:   Decodable.Fault in Cbor,
        tactic:  Tactic[Cbor.Error] )
  =>  ( decodable: => (inner is Decodable in Cbor)^ )
  =>  ((value is Decodable in Cbor)^{tactic, decodable}) =
    // An honest capability: the instance retains the resolution-scoped tactic and
    // the by-name inner codec (every given that includes a tactic is a capability;
    // Jon, 2026-07-12).
    cbor =>
      if cbor.root.unset then
        if absence.strict then abort(Cbor.Error(Reason.Absent)) else Unset
      else if cbor.root.nullary && !nullity.strict then
        Unset
      else if fault.strict then
        decodable.decoded(cbor)
      else
        tactic.tolerate(decodable.decoded(cbor)).or(Unset)

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
    Cbor.aggregable.map(_.as[value].asInstanceOf[value in Cbor])

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

      // `spot` stops at the first unready slot rather than scanning them all, and its index is
      // confined to `slots`, so the read needs no bounds check.
      val failed = active && slots.spot(slot => !slots(slot).ready).present
      var slot = 0

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
      CborParser.aggregable

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

    // Accessors over the opaque AST representation, in the companion so that they are in
    // implicit scope wherever a `Cbor.Ast` is used, including through `soundness`, without
    // being top-level names whose generic spellings (`long`, `string`, `array`, `index`) would
    // clash in the umbrella.
    extension (cbor: Cbor.Ast)
      inline def unset: Boolean = cbor == vacuous.Unset
      inline def isInteger: Boolean = cbor.isInstanceOf[Long]
      inline def isFloat: Boolean = cbor.isInstanceOf[Double]
      inline def isTextString: Boolean = cbor.isInstanceOf[String]
      inline def isBoolean: Boolean = cbor.isInstanceOf[Boolean]
      inline def nullary: Boolean = cbor.asInstanceOf[AnyRef] eq Cbor.CborNull
      inline def isTag: Boolean = cbor.isInstanceOf[Cbor.Tag]

      // Byte strings have runtime class `[B`; arrays/maps have `[Ljava/lang/Object;`.
      inline def isByteString: Boolean = cbor.isInstanceOf[scala.Array[Byte]]

      // Maps and arrays share the `Array[AnyRef]` runtime layout. Maps have an
      // even-length backing array; arrays are odd-length (with sentinel padding
      // when the logical element count is even).
      inline def isMap: Boolean =
        cbor.isInstanceOf[scala.Array[AnyRef]] && (cbor.asInstanceOf[scala.Array[?]].length & 1) == 0

      inline def isArray: Boolean =
        cbor.isInstanceOf[scala.Array[AnyRef]] && (cbor.asInstanceOf[scala.Array[?]].length & 1) == 1

      def primitive: Primitive =
        if isInteger then Primitive.Integer
        else if isFloat then Primitive.Float
        else if isTextString then Primitive.TextString
        else if isByteString then Primitive.ByteString
        else if isBoolean then Primitive.Boolean
        else if isMap then Primitive.Map
        else if isArray then Primitive.Array
        else if isTag then Primitive.Tag
        else if unset then Primitive.Undefined
        else Primitive.Null

      // `raise`, not `abort` (jacinta's leaf pattern): under an accruing scope every mistyped or
      // absent leaf registers its own error and continues with its caller's inconsequential `yet`
      // fallback — the derived record decoder detects the failure by foci delta and never lets the
      // fallback reach construction. Under a fail-fast tactic, the `raise` escapes identically.
      private def expected(expected: Primitive): Unit raises Cbor.Error =
        if unset then raise(Cbor.Error(Reason.Absent))
        else raise(Cbor.Error(Reason.NotType(primitive, expected)))

      inline def elements: Int = Cbor.Ast.length(cbor)
      inline def entries: Int = Cbor.Ast.size(cbor)

      def element(index: Int): Cbor.Ast = cbor.asInstanceOf[Array[Cbor.Ast]^{}].readable(index)

      inline def key(index: Int): Cbor.Ast = cbor.asInstanceOf[Array[Cbor.Ast]^{}].readable(index*2)
      inline def value(index: Int): Cbor.Ast = cbor.asInstanceOf[Array[Cbor.Ast]^{}].readable(index*2 + 1)

      def index(key: String): Int =
        val array = cbor.asInstanceOf[Array[Any]^{}]
        val count = array.length
        var index = 0

        while index < count do
          if array.readUnchecked(index) == key then return index/2
          index += 2

        -1

      def long: Long raises Cbor.Error =
        if isInteger then cbor.asInstanceOf[Long] else if isFloat then cbor.asInstanceOf[Double].toLong
        else expected(Primitive.Integer) yet 0L

      def double: Double raises Cbor.Error =
        if isFloat then cbor.asInstanceOf[Double]
        else if isInteger then cbor.asInstanceOf[Long].toDouble
        else expected(Primitive.Float) yet 0.0

      def string: String raises Cbor.Error =
        if isTextString then cbor.asInstanceOf[String] else expected(Primitive.TextString) yet ""

      def byteString: Array[Byte]^{} raises Cbor.Error =
        if isByteString then cbor.asInstanceOf[Array[Byte]^{}]
        else expected(Primitive.ByteString) yet Array.empty[Byte]

      def boolean: Boolean raises Cbor.Error =
        if isBoolean then cbor.asInstanceOf[Boolean] else expected(Primitive.Boolean) yet false

      def tag: Cbor.Tag raises Cbor.Error =
        if isTag then cbor.asInstanceOf[Cbor.Tag]
        else expected(Primitive.Tag) yet Cbor.Tag(0L, vacuous.Unset)

      def array: Array[Cbor.Ast]^{} raises Cbor.Error =
        if isArray then
          val full = cbor.asInstanceOf[Array[Cbor.Ast]^{}]
          val count = elements

          if count == full.length then full else Array.tabulate(count)(full.readable(_))
        else
          expected(Primitive.Array)
          Array.empty[Cbor.Ast]

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
  given lens: [name <: Label: ValueOf] => (erased dynamical: (? >: Cbor) is Dynamical) => (tactic: Tactic[Cbor.Error])
  =>  ((name is Lens from Cbor onto Cbor)^{tactic}) =
    // Both lambdas only read through the same resolution-scoped tactic; no aliased writer.
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

  given filterOptical: Filter[Cbor] is Optical from Cbor onto Cbor = filter =>
    val predicate: Cbor -> Boolean = filter.predicate

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
    Ast.aggregable.map(Cbor.ast)

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
    ( parser: CborParser^, parsable: (value is Cbor.Parsable)^ )
    ( using tactic: Tactic[Cbor.Error] )
  :   value =

    val result = parsable.parse(Cbor.Reader(parser, tactic))
    if parser.more then abort(Cbor.Error(Reason.Trailing(parser.position)))
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

    // [field-purity] given retains resolution-scoped parsable and tactic
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Cbor
        type Operand = Data

        def aggregate(bytes: Chain[Data]): value in Cbor =
          // A single in-memory block — the common case — is read in place; the general
          // path pulls the chain's cells as the parser needs them.
          if !bytes.nil && bytes.stdlib.tail.isEmpty
          then parseDirect(CborParser(bytes.stdlib.head), parsable).asInstanceOf[value in Cbor]
          else parseDirect(CborParser(bytes), parsable).asInstanceOf[value in Cbor]

        // The stream crosses as a neutral reference (see `CborParser.aggregable`).
        override def accept(stream: (Stream[Data] over Credit)^): value in Cbor =
          val moved: AnyRef = stream.asInstanceOf[AnyRef]
          parseDirect(CborParser(moved.asInstanceOf[(Stream[Data] over Credit)^]), parsable)
          . asInstanceOf[value in Cbor]

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream.
  // Concrete in `Data`, so it beats the composed pipeline by specificity.
  // Sealed like `aggregableParsed` above.
  given readableParsed: [value]
  =>  (parsable: (value is Cbor.Parsable)^)
  =>  (tactic: Tactic[Cbor.Error])
  =>  (Data is Readable to (value in Cbor)) =

    // [field-purity] given retains resolution-scoped parsable and tactic
    caps.unsafe.unsafeAssumePure:
      data => parseDirect(CborParser(data), parsable).asInstanceOf[value in Cbor]

  given unit: (tactic: Tactic[Cbor.Error])
  =>  ((Unit is Decodable in Cbor)^{tactic}) =
    value =>
      if !value.root.nullary then
        val reason =
          if value.root.unset then Reason.Absent
          else Reason.NotType(value.root.primitive, Primitive.Null)

        abort(Cbor.Error(reason))

  // The same three policies as `optional`, yielding `None`.
  given option: [value: Decodable in Cbor]
  =>  ( absence: Decodable.Absence in Cbor,
        nullity: Decodable.Nullity in Cbor,
        fault:   Decodable.Fault in Cbor,
        tactic:  Tactic[Cbor.Error] )
  =>  ((Option[value] is Decodable in Cbor)^{tactic}) =

    cbor =>
      if cbor.root.unset then
        if absence.strict then abort(Cbor.Error(Reason.Absent)) else None
      else if cbor.root.nullary && !nullity.strict then
        None
      else if fault.strict then
        Some(value.decoded(cbor))
      else
        tactic.tolerate(value.decoded(cbor)).let(Some(_)).or(None)

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
    arrayEncodable[list[element], element](encodable)

  given setEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((set[element] is Encodable in Cbor)^{encodable}) =
    arrayEncodable[set[element], element](encodable)

  given seriesEncodable: [sequence <: Sequence, element]
  =>  ( encodable: => (element is Encodable in Cbor)^ )
  =>  ((sequence[element] is Encodable in Cbor)^{encodable}) =
    arrayEncodable[sequence[element], element](encodable)

  // A collection as a CBOR array of its elements, for each collection type above.
  private def arrayEncodable[collection, element]
    ( encodable: => (element is Encodable in Cbor)^ )
    ( using traversable: collection is Traversable by element )
  :   ((collection is Encodable in Cbor)^{encodable}) =

    values =>
      val roots = traversable.traverse(values).map: value =>
        encodable.encoded(value).root: Any

      ast(Ast.array(Array.from(roots).asInstanceOf[Array[Any]^{}]))

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory: sc.Factory[element, collection[element]], tactic:  Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((collection[element] is Decodable in Cbor)^{tactic, decodable}) =

    // An honest capability, as `optional` above.
    value =>
        val builder = factory.newBuilder
        value.root.array.each: cbor => builder += decodable.decoded(ast(cbor))

        builder.result()


  // Alias counterparts: the opaque prelude collections do not conform to `Iterable`, so each
  // decodes at the underlying stdlib type and casts, passing the by-name `decodable` straight
  // through so that a recursive derivation (`List[Tree]` inside `Tree`) still ties the knot.
  given listDecodable: [list <: List, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((list[element] is Decodable in Cbor)^{tactic, decodable}) =
    collectionDecodable[scala.collection.immutable.List, element]
    . asInstanceOf[(list[element] is Decodable in Cbor)^{tactic, decodable}]

  given setDecodable: [set <: Set, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((set[element] is Decodable in Cbor)^{tactic, decodable}) =
    collectionDecodable[scala.collection.immutable.Set, element]
    . asInstanceOf[(set[element] is Decodable in Cbor)^{tactic, decodable}]

  given seriesDecodable: [sequence <: Sequence, element]
  =>  ( tactic: Tactic[Cbor.Error] )
  =>  ( decodable: => (element is Decodable in Cbor)^ )
  =>  ((sequence[element] is Decodable in Cbor)^{tactic, decodable}) =
    collectionDecodable[Vector, element]
    . asInstanceOf[(sequence[element] is Decodable in Cbor)^{tactic, decodable}]

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
      safely(cbor.selectDynamic(key.s).as[Text])
    def variant(cbor: Cbor): Cbor = unsafely(cbor.updateDynamic(key.s)(Unset))

  def discriminatedUnion[value](label: Text): value is Discriminable in Cbor =
    DiscriminantKey[value](label)

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
    private[breviloquence] def apply(parser: CborParser^, tactic: Tactic[Cbor.Error])
    :   Cbor.Reader^ =

      new Cbor.Reader(parser.asInstanceOf[AnyRef], tactic.asInstanceOf[AnyRef])

    // Reasserts the capability of a reader that travelled as a neutral carrier — the
    // generated parsers' `parseCarrier(reader0: AnyRef)` entry, which is spliced into user
    // modules, so this is public, as `Cbor.Parsable.parseField` is. Inline, so the cast
    // expression itself lands in the user module: a method's `^` result is read-only in a
    // module that is capture-checked but not separation-checked, and only the cast (or a
    // parameter) yields the exclusive reader every `update` call needs.
    inline def of(carrier: AnyRef): Cbor.Reader^ = carrier.asInstanceOf[Cbor.Reader^]

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
    private inline def parser: CborParser^ = parser0.asInstanceOf[CborParser^]

    // The sealed conduit for generated parsers: package-private, so the only
    // path to the wrapped capabilities from outside breviloquence is through
    // the accessor the compiler synthesizes for breviloquence's own
    // macro-generated splices — hand-written code cannot name it. Generated
    // code binds the parser once per record and reads through `CborParser`'s
    // direct rim without this class's per-item forwarders.
    private[breviloquence] def rawParser: AnyRef = parser0
    private[breviloquence] def rawTactic: AnyRef = tactic0
    private inline def tactic: Tactic[Cbor.Error] = tactic0.asInstanceOf[Tactic[Cbor.Error]]

    // ── Scalars: one data item each. Values and failures agree with the
    // `Cbor.Ast` accessors exactly, so direct and AST reads yield equal
    // values — integers coerce to floats and vice versa, as `.long` and
    // `.double` do. ──
    update def long(): Long = parser.directLong()(using tactic)
    update def int(): Int = parser.directLong()(using tactic).toInt
    update def double(): Double = parser.directDouble()(using tactic)
    update def boolean(): Boolean = parser.directBoolean()(using tactic)
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
    update def openMap(): Int = parser.directOpenMap()(using tactic)
    update def openArray(): Int = parser.directOpenArray()(using tactic)
    update def breakEnd(): Boolean = parser.directBreak()(using tactic)

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
    update def skipValue(): Unit = parser.directSkipValue()(using tactic)

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

  def selectDynamic(field: String)(using erased dynamical: (? >: Cbor) is Dynamical): Cbor raises Cbor.Error =
    apply(field.tt)


  def applyDynamic(field: String)(index: Int)(using erased dynamical: (? >: Cbor) is Dynamical)
  :   Cbor raises Cbor.Error =

    apply(field.tt)(index)


  def updateDynamic(field: String)[value: Encodable in Cbor](value: value)
    ( using erased dynamical: (? >: Cbor) is Dynamical )
  :   Cbor raises Cbor.Error =

    modify(field, value.encode)


  def updateDynamic(field: String)[value](unset: Unset.type)(using erased dynamical: (? >: Cbor) is Dynamical)
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
