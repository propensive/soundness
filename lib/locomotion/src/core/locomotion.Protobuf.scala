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
package locomotion

import language.experimental.pureFunctions

import java.lang as jl
import java.nio.charset.StandardCharsets.UTF_8

import scala.collection.Factory
import scala.compiletime.*

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import hypotenuse.*
import panopticon.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

import ProtobufError.Reason

trait Protobuf2:
  this: Protobuf.type =>

  inline given encodable: [value] => value is Encodable in Protobuf = summonFrom:
    case given Reflection[`value`] => EncodableDerivation.derived

  inline given decodable: [value] => value is Decodable in Protobuf = summonFrom:
    case given Reflection[`value`] => DecodableDerivation.derived

  // The AST-materializing read path: `bytes.read[Foo in Protobuf]` shorthand
  // for `bytes.read[Protobuf].as[Foo]`. Lives at this priority so `object
  // Protobuf`'s direct-parsing `aggregableParsed` wins whenever the value
  // has a `Protobuf.Parsable`; when it does not, this resolves exactly as
  // before. `value in Protobuf` is just `value { type Form = Protobuf }`,
  // so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Protobuf] => (tactic: Tactic[ProtobufError])
  =>  (((value in Protobuf) is Aggregable by Data)^{tactic}) =
    bytes => Protobuf.message(bytes.read[Data]).as[value].asInstanceOf[value in Protobuf]

  // Unpacked `repeated`: each element is emitted as a separate field (one tag per
  // value). This is the fallback for elements with no `Packable` instance — strings,
  // byte arrays and embedded messages. Numeric scalars are handled by the
  // higher-priority packed givens in `object Protobuf`. Decoding accepts either
  // form, so a packed field still round-trips here when no `Packable` is in scope.

  // The collection/optional instances below are honest capabilities: each retains its
  // by-name element codec (and, where present, a resolution-scoped `Tactic`), which
  // share the instance's given-resolution lifetime (every given that includes a tactic
  // is a capability; Jon, 2026-07-12). Derived products consume them through wisteria's
  // single erasing cast at the derivation boundary. See rep/DECISIONS.md.
  given listEncodable: [collection <: Iterable, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^ )
  =>  ((collection[element] is Encodable in Protobuf)^) =
    // An honest capability: the instance retains the by-name element codec, itself a
    // capability (every given that includes a tactic is a capability; Jon, 2026-07-12).
    values =>
      val occurrences = values.to(List).flatMap(encodable.encode(_).occurrences)
      if occurrences.isEmpty then Protobuf.Absent else Protobuf.Repeated(occurrences)

  given listDecodable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]] )
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ((collection[element] is Decodable in Protobuf)^) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val builder = factory.newBuilder

      protobuf.occurrences.foreach: wire =>
        builder += decodable.decoded(wire)

      builder.result()

object Protobuf extends Protobuf2:
  // Wraps a complete message payload as a length-delimited wire value — the shape
  // the derivations decode. Used by the `Aggregable` instances backing `read`.
  private[locomotion] def message(bytes: Data): Protobuf = Wire(WireType.Len, bytes)

  object Parsable:
    // The base of generated parsers: generated code is capture-erased, so
    // the body receives the reader as a neutral carrier, and the capability
    // is asserted here at the rim — the audited point — like the reader's
    // own accessors. (A generated override of `parse` itself would narrow
    // the trait's `Reader^` parameter to a pure type, which capture
    // checking rejects at the instantiation site.)
    abstract class Direct[value] extends Protobuf.Parsable:
      type Self = value

      protected def parseCarrier(reader: AnyRef): value

      def parse(reader: ProtobufReader^): value = parseCarrier(reader.asInstanceOf[AnyRef])

    def apply[value](parser: (reader: ProtobufReader^) => value)
    :   ((value is Protobuf.Parsable)^{parser}) =

      new Protobuf.Parsable:
        type Self = value
        def parse(reader: ProtobufReader^): value = parser(reader)

    // The call point for a nominal `Parsable` in a field position of a
    // *generated* parser (a recursive message's own instance, or a
    // hand-written one). Both travel as neutral carriers — generated code
    // is capture-erased — and the capability is reasserted here, at the
    // audited point, exactly as the reader's own rim accessors do.
    def parseField[value](parsable: AnyRef, reader: AnyRef): value =
      parsable.asInstanceOf[value is Protobuf.Parsable]
      . parse(reader.asInstanceOf[ProtobufReader^])

    // The universal bridge from the AST world: materialize the window as a
    // message and decode it. Types with only a `Decodable in Protobuf` keep
    // working through this, and it is the user's one-line escape hatch when
    // a custom decoder must beat a generated direct parser.
    def fromDecodable[value](decodable: (value is Decodable in Protobuf)^)
    :   ((value is Protobuf.Parsable)^{decodable}) =

      new Protobuf.Parsable:
        type Self = value
        def parse(reader: ProtobufReader^): value = decodable.decoded(reader.message())

  // The direct-parsing counterpart of `Decodable in Protobuf`: consumes
  // fields straight off the input bytes through a `ProtobufReader` instead
  // of walking the materialized number-keyed map, so `read[value in
  // Protobuf]` can instantiate values without buffering every field's wire
  // value. `Parsable` is the opt-in surface: explicit instances and
  // `Protobuf.Inlinable.parsable`. It has no blanket fallback given, so no
  // read changes behavior until a type opts in.
  trait Parsable extends distillate.Parsable:
    type Transport = Protobuf
    type Reader = ProtobufReader

  // Direct-parsing counterpart of the `aggregable`/`aggregableIn` path:
  // drives a `Protobuf.Parsable` instance over the input through a
  // `ProtobufReader`, its window set to the whole message.
  private def parseDirect[value]
    ( input: Data, parsable: (value is Protobuf.Parsable)^ )
    ( using tactic: Tactic[ProtobufError] )
  :   value =

    parsable.parse(ProtobufReader(ProtobufParser(input), tactic))

  // Direct parsing: when the value knows how to consume wire fields itself,
  // the field map is never materialized. Declared here (not in `Protobuf2`,
  // where the `Decodable`-based `aggregableIn` lives) so it wins whenever a
  // `Protobuf.Parsable` exists, and is otherwise inapplicable — existing
  // code resolves exactly as before. Sealed per the codec-thunk pattern:
  // the instance retains the resolution-scoped parsable and tactic.
  given aggregableParsed: [value]
  =>  (parsable: (value is Protobuf.Parsable)^)
  =>  (tactic: Tactic[ProtobufError])
  =>  ((value in Protobuf) is Aggregable by Data) =

    caps.unsafe.unsafeAssumePure:
      bytes => parseDirect(bytes.read[Data], parsable).asInstanceOf[value in Protobuf]

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream.
  // Concrete in `Data`, so it beats the composed pipeline by specificity.
  // Sealed like `aggregableParsed` above.
  given readableParsed: [value]
  =>  (parsable: (value is Protobuf.Parsable)^)
  =>  (tactic: Tactic[ProtobufError])
  =>  (Data is Readable to (value in Protobuf)) =

    caps.unsafe.unsafeAssumePure:
      data => parseDirect(data, parsable).asInstanceOf[value in Protobuf]

  given protobuf: Protobuf is Decodable in Protobuf = identity(_)

  // The wire bytes of a `Protobuf` message, reached with `.encode` (anticipation's
  // `Encodable`) — the same verb jacinta uses for `Json`. `serialize` is reserved
  // for monotonous's byte→text encodings (hex, base64, …).
  given encodableInData: Protobuf is Encodable in Data = _.payload

  // HTTP content-type integration: `Abstractable across HttpStreams` makes a
  // `Protobuf` message usable as an HTTP request/response body (telekinesis
  // derives `Postable`/`Servable` from it). Decoding a response body back into
  // `Protobuf` is already covered by `aggregable` (`Aggregable by Data`).
  given abstractable: Protobuf is Abstractable across HttpStreams to HttpStreams.Content =
    new Abstractable:
      type Self = Protobuf
      type Domain = HttpStreams
      type Result = HttpStreams.Content

      def genericize(value: Protobuf): HttpStreams.Content =
        (t"application/protobuf", HttpStreams.Body(value.encode))

  // `bytes.read[Protobuf]` aggregates the byte stream and wraps it as a message.
  given aggregable: Protobuf is Aggregable by Data = bytes => message(bytes.read[Data])

  // Number-keyed optic: Protobuf fields are addressed by number, not name, so an
  // `Ordinal` selects field `n` (`Prim` = field 1). Updating parses the message's
  // fields, replaces field `n`'s wire value with the transform's result, and
  // re-encodes. A parse failure, or an absent field number, leaves the message
  // unchanged. This is the only optic Protobuf affords — there are no field labels.
  given fieldOptical: [element] => Ordinal is Optical from Protobuf onto Protobuf = ordinal =>
    Optic: (origin, lambda) =>
      val number = ordinal.n0 + 1

      safely:
        val fields = ProtobufParser(origin.payload).fields()

        if !fields.contains(number) then origin else
          val bytes = printed: printer =>
            fields.each: (key, values) =>
              val value = Protobuf.Repeated(values)
              printer.field(key, if key == number then lambda(value) else value)

          Protobuf.Wire(WireType.Len, bytes)

      . or(origin)

  // Synchronously assemble wire bytes by running `lambda` against a `ProtobufPrinter`.
  private def printed(lambda: ProtobufPrinter^ => Unit): Data =
    Producer.collect[Data](): producer =>
      lambda(ProtobufPrinter(producer))

  // LazyList a message's wire bytes incrementally (chunked); the producing code runs on a separate
  // fiber. The bytes themselves are already assembled (Protobuf length-prefixes nested messages, so
  // their lengths must be known up front); this hands them out in bounded chunks.
  def emit(value: Protobuf)(using Monitor, Probate): Iterator[Data] =
    val producer = Producer[Data](4096)

    async:
      producer.put(value.payload)
      producer.finish()

    producer.iterator

  private def readVarint(protobuf: Protobuf)(using Tactic[ProtobufError]): Long =
    if protobuf.isAbsent then 0L else ProtobufParser(protobuf.payload).varint()

  private def readFixed32(protobuf: Protobuf)(using Tactic[ProtobufError]): Int =
    if protobuf.isAbsent then 0 else ProtobufParser(protobuf.payload).fixed32()

  private def readFixed64(protobuf: Protobuf)(using Tactic[ProtobufError]): Long =
    if protobuf.isAbsent then 0L else ProtobufParser(protobuf.payload).fixed64()

  // Zig-zag maps signed integers to unsigned so small-magnitude negatives stay
  // short as varints: 0→0, -1→1, 1→2, -2→3, …
  private def zigzag(value: Long): Long = (value << 1) ^ (value >> 63)
  private def unzigzag(value: Long): Long = (value >>> 1) ^ -(value & 1)

  private def utf8(text: Text): Data = text.s.getBytes(UTF_8).nn.immutable(using Unsafe)

  given intEncodable: Int is Encodable in Protobuf =
    int => Wire(WireType.Varint, printed(_.varint(int.toLong)))

  given longEncodable: Long is Encodable in Protobuf =
    long => Wire(WireType.Varint, printed(_.varint(long)))

  given booleanEncodable: Boolean is Encodable in Protobuf =
    boolean => Wire(WireType.Varint, printed(_.varint(if boolean then 1L else 0L)))

  given doubleEncodable: Double is Encodable in Protobuf =
    double => Wire(WireType.I64, printed(_.fixed64(jl.Double.doubleToLongBits(double))))

  given floatEncodable: Float is Encodable in Protobuf =
    float => Wire(WireType.I32, printed(_.fixed32(jl.Float.floatToIntBits(float))))

  given textEncodable: Text is Encodable in Protobuf = text => Wire(WireType.Len, utf8(text))
  given dataEncodable: Data is Encodable in Protobuf = bytes => Wire(WireType.Len, bytes)

  given intDecodable: (tactic: Tactic[ProtobufError])
  =>  ((Int is Decodable in Protobuf)^{tactic, caps.any}) =
    readVarint(_).toInt

  given longDecodable: (tactic: Tactic[ProtobufError])
  =>  ((Long is Decodable in Protobuf)^{tactic, caps.any}) =
    readVarint(_)

  given booleanDecodable: (tactic: Tactic[ProtobufError])
  =>  ((Boolean is Decodable in Protobuf)^{tactic, caps.any}) =
    readVarint(_) != 0

  given doubleDecodable: (tactic: Tactic[ProtobufError])
  =>  ((Double is Decodable in Protobuf)^{tactic, caps.any}) =
    protobuf =>
      if protobuf.isAbsent then 0.0
      else jl.Double.longBitsToDouble(ProtobufParser(protobuf.payload).fixed64())

  given floatDecodable: (tactic: Tactic[ProtobufError])
  =>  ((Float is Decodable in Protobuf)^{tactic, caps.any}) =
    protobuf =>
      if protobuf.isAbsent then 0.0f
      else jl.Float.intBitsToFloat(ProtobufParser(protobuf.payload).fixed32())

  given textDecodable: Text is Decodable in Protobuf =
    protobuf => Text(jl.String(protobuf.payload.mutable(using Unsafe), UTF_8).nn)

  given dataDecodable: Data is Decodable in Protobuf = _.payload

  // hypotenuse typed integers select the protobuf integer encoding by type, with no
  // annotation: U32/U64 → uint (plain varint), S32/S64 → sint (zig-zag varint),
  // B32/B64 → fixed32/fixed64 (little-endian).

  given u32Encodable: U32 is Encodable in Protobuf =
    u32 => Wire(WireType.Varint, printed(_.varint(u32.long)))

  given u64Encodable: U64 is Encodable in Protobuf =
    u64 => Wire(WireType.Varint, printed(_.varint(u64.bits.s64.long)))

  given s32Encodable: S32 is Encodable in Protobuf =
    s32 => Wire(WireType.Varint, printed(_.varint(zigzag(s32.long))))

  given s64Encodable: S64 is Encodable in Protobuf =
    s64 => Wire(WireType.Varint, printed(_.varint(zigzag(s64.long))))

  given b32Encodable: B32 is Encodable in Protobuf =
    b32 => Wire(WireType.I32, printed(_.fixed32(b32.s32.int)))

  given b64Encodable: B64 is Encodable in Protobuf =
    b64 => Wire(WireType.I64, printed(_.fixed64(b64.s64.long)))

  given u32Decodable: (tactic: Tactic[ProtobufError])
  =>  ((U32 is Decodable in Protobuf)^{tactic, caps.any}) =
    readVarint(_).toInt.bits.u32

  given u64Decodable: (tactic: Tactic[ProtobufError])
  =>  ((U64 is Decodable in Protobuf)^{tactic, caps.any}) =
    readVarint(_).bits.u64

  given s32Decodable: (tactic: Tactic[ProtobufError])
  =>  ((S32 is Decodable in Protobuf)^{tactic, caps.any}) =
    protobuf => unzigzag(readVarint(protobuf)).toInt.bits.s32

  given s64Decodable: (tactic: Tactic[ProtobufError])
  =>  ((S64 is Decodable in Protobuf)^{tactic, caps.any}) =
    protobuf => unzigzag(readVarint(protobuf)).bits.s64

  given b32Decodable: (tactic: Tactic[ProtobufError])
  =>  ((B32 is Decodable in Protobuf)^{tactic, caps.any}) =
    readFixed32(_).bits

  given b64Decodable: (tactic: Tactic[ProtobufError])
  =>  ((B64 is Decodable in Protobuf)^{tactic, caps.any}) =
    readFixed64(_).bits

  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: (inner is Encodable in Protobuf)^ )
  =>  ((value is Encodable in Protobuf)^{encodable}) =

    new Encodable:
      type Self = value
      type Form = Protobuf

      def encoded(value: value): Protobuf =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Absent)

  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( decodable: => (inner is Decodable in Protobuf)^ )
  =>  ((value is Decodable in Protobuf)^) =
    // An honest capability, as `listEncodable` above.
    protobuf => if protobuf.isAbsent then Unset else decodable.decoded(protobuf)

  // Packed `repeated` for numeric scalars (proto3 default): all elements are
  // concatenated into one length-delimited field. Higher priority than the unpacked
  // givens in `Protobuf2` because `Packable` constrains the element type.

  given packedEncodable: [collection <: Iterable, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^, packable: element is Packable )
  =>  ((collection[element] is Encodable in Protobuf)^) =
    // An honest capability, as `listEncodable` above.
    values =>
      val list = values.to(List)

      if list.isEmpty then Absent else
        val bytes = printed: printer =>
          var rest = list

          while rest.nonEmpty do
            printer.raw(encodable.encode(rest.head).payload)
            rest = rest.tail

        Wire(WireType.Len, bytes)

  given packedDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        decodable: => (element is Decodable in Protobuf)^,
        packable:  element is Packable )
  =>  ( tactic: Tactic[ProtobufError] )
  =>  ((collection[element] is Decodable in Protobuf)^{tactic, caps.any}) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val builder = factory.newBuilder

      // Accept both packed (one Len field of concatenated values) and unpacked (a
      // value per occurrence) wire forms, per the proto3 compatibility rule.
      protobuf.occurrences.foreach: wire =>
        if wire.wireKind == WireType.Len && packable.wireType != WireType.Len
        then ProtobufParser(wire.payload).packed(packable.wireType).foreach: element =>
          builder += decodable.decoded(element)
        else builder += decodable.decoded(wire)

      builder.result()

  // `Map[K, V]` encodes as a `repeated` message of map entries, each a two-field
  // message `{ key = 1; value = 2 }` — the proto3 map wire format.

  given mapEncodable: [key, value]
  =>  ( keyEncodable:   (key is Encodable in Protobuf)^,
        valueEncodable: (value is Encodable in Protobuf)^ )
  =>  ((Map[key, value] is Encodable in Protobuf)^{keyEncodable, valueEncodable}) =

    map =>
      if map.isEmpty then Absent else
        val entries = map.to(List).map: (key, value) =>
          val bytes = printed: printer =>
            printer.field(1, keyEncodable.encode(key))
            printer.field(2, valueEncodable.encode(value))

          Wire(WireType.Len, bytes)

        Repeated(entries)

  given mapDecodable: [key, value]
  =>  ( keyDecodable:   => (key is Decodable in Protobuf)^,
        valueDecodable: => (value is Decodable in Protobuf)^ )
  =>  ( tactic: Tactic[ProtobufError] )
  =>  ((Map[key, value] is Decodable in Protobuf)^{tactic, caps.any}) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val entries = protobuf.occurrences.map: entry =>
        val fields = ProtobufParser(entry.payload).fields()
        val key = keyDecodable.decoded(Repeated(fields.at(1).or(Nil)))
        val value = valueDecodable.decoded(Repeated(fields.at(2).or(Nil)))
        (key, value)

      entries.to(Map)

  object EncodableDerivation extends Derivable[Encodable in Protobuf]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Protobuf =

      val numbers = fieldNumbers[derivation]

      value =>
        val bytes = printed: printer =>
          fields(value):
            [field0] => fieldValue =>
              printer.field(numbers(label), contextual.encode(fieldValue))

        Protobuf.Wire(WireType.Len, bytes)

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Protobuf =
      value =>
        variant(value):
          [variant <: derivation] => variantValue =>
            val payload = printed(_.field(index + 1, contextual.encode(variantValue)))
            Protobuf.Wire(WireType.Len, payload)

    inline def fieldNumbers[derivation <: Product: ProductReflection]: Map[Text, Int] =
      val annotated: Map[Text, Set[field]] = infer[derivation is Annotated by field] match
        case annotated: Annotated.Fields => annotated.fields
        case _                           => Map()

      val pairs =
        contexts[derivation]():
          [field0] => context =>
            (label, annotated.at(label).let(_.head.number).or(index + 1))

      pairs.to(Map)

  object DecodableDerivation extends Derivable[Decodable in Protobuf]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Decodable in Protobuf)^ =

      val numbers = fieldNumbers[derivation]

      // The decode lambda closes over the resolution-scoped `Tactic` that `provide` summons
      // at the derivation site, sharing the instance's given-resolution lifetime; the fresh
      // (`^`) trait result honestly admits the capture — no seal.
      { protobuf =>
        provide[Tactic[ProtobufError]]:
          val map = ProtobufParser(protobuf.payload).fields()

          build[derivation]:
            [field0] => context =>
              map.at(numbers(label)).lay(default.or(context.decoded(Protobuf.Absent))): values =>
                context.decoded(Protobuf.Repeated(values)) }

    inline def disjunction[derivation: SumReflection]: (derivation is Decodable in Protobuf)^ =
      // A fresh (`^`) result honestly admitting the capture, as for `conjunction` above.
      { protobuf =>
        provide[Tactic[ProtobufError]]:
          provide[Tactic[VariantError]]:
            val map = ProtobufParser(protobuf.payload).fields()
            val labels = variantLabels

            var index = 0
            while index < labels.length && !map.contains(index + 1) do index += 1
            if index >= labels.length then abort(ProtobufError(Reason.MissingField(0)))

            delegate(labels(index)):
              [variant <: derivation] => context =>
                context.decoded(Protobuf.Repeated(map(index + 1))) }

    inline def fieldNumbers[derivation <: Product: ProductReflection]: Map[Text, Int] =
      val annotated: Map[Text, Set[field]] = infer[derivation is Annotated by field] match
        case annotated: Annotated.Fields => annotated.fields
        case _                           => Map()

      val pairs =
        contexts[derivation]():
          [field0] => context =>
            (label, annotated.at(label).let(_.head.number).or(index + 1))

      pairs.to(Map)

// A single Protocol Buffers wire value, tagged with its wire type — the `Form` of
// `Encodable`/`Decodable in Protobuf`. `Repeated` carries every occurrence of a
// field number (so repeated fields and last-wins scalars both work), and `Absent`
// marks a field that was not present (decode) or should be omitted (encode).
enum Protobuf:
  case Absent
  case Wire(wireType: WireType, bytes: Data)
  case Repeated(values: List[Protobuf])

  // The most recent single wire value (protobuf's last-one-wins rule for scalars).
  def single: Protobuf = this match
    case Repeated(values) => if values.isEmpty then Protobuf.Absent else values.last
    case other            => other

  // Every wire value recorded for this field — used to decode `repeated` fields.
  def occurrences: List[Protobuf] = this match
    case Repeated(values) => values
    case Absent           => Nil
    case wire             => wire :: Nil

  def payload: Data = single match
    case Wire(_, bytes) => bytes
    case _              => IArray.empty[Byte]

  // The wire type of the (single) value, or `Len` for absent — used to tell a packed
  // scalar field (`Len`) from inline-encoded scalar values during repeated decoding.
  def wireKind: WireType = single match
    case Wire(wireType, _) => wireType
    case _                 => WireType.Len

  def isAbsent: Boolean = this match
    case Absent        => true
    case Repeated(Nil) => true
    case _             => false

  def as[value: Decodable in Protobuf]: value raises ProtobufError = value.decoded(this)
