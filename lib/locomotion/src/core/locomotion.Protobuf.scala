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

  // Unpacked `repeated`: each element is emitted as a separate field (one tag per
  // value). This is the fallback for elements with no `Packable` instance — strings,
  // byte arrays and embedded messages. Numeric scalars are handled by the
  // higher-priority packed givens in `object Protobuf`. Decoding accepts either
  // form, so a packed field still round-trips here when no `Packable` is in scope.

  given listEncodable: [collection <: Iterable, element]
  =>  ( encodable: => element is Encodable in Protobuf )
  =>  collection[element] is Encodable in Protobuf =

    values =>
      val occurrences = values.to(List).flatMap(encodable.encode(_).occurrences)
      if occurrences.isEmpty then Protobuf.Absent else Protobuf.Repeated(occurrences)

  given listDecodable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]] )
  =>  ( decodable: => element is Decodable in Protobuf )
  =>  collection[element] is Decodable in Protobuf =

    protobuf =>
      val builder = factory.newBuilder

      protobuf.occurrences.foreach: wire =>
        builder += decodable.decoded(wire)

      builder.result()

object Protobuf extends Protobuf2:
  // Wraps a complete message payload as a length-delimited wire value — the shape
  // the derivations decode. Used by the `Aggregable` instances backing `read`.
  private def message(bytes: Data): Protobuf = Wire(WireType.Len, bytes)

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
        (t"application/protobuf", Stream(value.encode))

  // `bytes.read[Protobuf]` aggregates the byte stream and wraps it as a message.
  given aggregable: Protobuf is Aggregable by Data = bytes => message(bytes.read[Data])

  // `bytes.read[Foo in Protobuf]` shorthand for `bytes.read[Protobuf].as[Foo]`,
  // mirroring jacinta's `value in Json` and breviloquence's `value in Cbor`.
  // `value in Protobuf` is just `value { type Form = Protobuf }`, so the
  // cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Protobuf] => Tactic[ProtobufError]
  =>  (value in Protobuf) is Aggregable by Data =
    bytes => message(bytes.read[Data]).as[value].asInstanceOf[value in Protobuf]

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
  private def printed(lambda: ProtobufPrinter => Unit): Data =
    Producer.collect[Data](): producer =>
      lambda(ProtobufPrinter(producer))

  // Stream a message's wire bytes incrementally (chunked); the producing code runs on a separate
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

  given intDecodable: Tactic[ProtobufError] => Int is Decodable in Protobuf =
    readVarint(_).toInt

  given longDecodable: Tactic[ProtobufError] => Long is Decodable in Protobuf =
    readVarint(_)

  given booleanDecodable: Tactic[ProtobufError] => Boolean is Decodable in Protobuf =
    readVarint(_) != 0

  given doubleDecodable: Tactic[ProtobufError] => Double is Decodable in Protobuf =
    protobuf =>
      if protobuf.isAbsent then 0.0
      else jl.Double.longBitsToDouble(ProtobufParser(protobuf.payload).fixed64())

  given floatDecodable: Tactic[ProtobufError] => Float is Decodable in Protobuf =
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

  given u32Decodable: Tactic[ProtobufError] => U32 is Decodable in Protobuf =
    readVarint(_).toInt.bits.u32

  given u64Decodable: Tactic[ProtobufError] => U64 is Decodable in Protobuf =
    readVarint(_).bits.u64

  given s32Decodable: Tactic[ProtobufError] => S32 is Decodable in Protobuf =
    protobuf => unzigzag(readVarint(protobuf)).toInt.bits.s32

  given s64Decodable: Tactic[ProtobufError] => S64 is Decodable in Protobuf =
    protobuf => unzigzag(readVarint(protobuf)).bits.s64

  given b32Decodable: Tactic[ProtobufError] => B32 is Decodable in Protobuf =
    readFixed32(_).bits

  given b64Decodable: Tactic[ProtobufError] => B64 is Decodable in Protobuf =
    readFixed64(_).bits

  given optionalEncodable: [inner <: value, value >: Unset: Mandatable to inner]
  =>  ( encodable: inner is Encodable in Protobuf )
  =>  value is Encodable in Protobuf =

    new Encodable:
      type Self = Optional[value]
      type Form = Protobuf

      def encoded(value: Optional[value]): Protobuf =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Absent)

  given optionalDecodable: [inner <: value, value >: Unset: Mandatable to inner]
  =>  ( decodable: => inner is Decodable in Protobuf )
  =>  value is Decodable in Protobuf =

    protobuf => if protobuf.isAbsent then Unset else decodable.decoded(protobuf)

  // Packed `repeated` for numeric scalars (proto3 default): all elements are
  // concatenated into one length-delimited field. Higher priority than the unpacked
  // givens in `Protobuf2` because `Packable` constrains the element type.

  given packedEncodable: [collection <: Iterable, element]
  =>  ( encodable: => element is Encodable in Protobuf, packable: element is Packable )
  =>  collection[element] is Encodable in Protobuf =

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
        decodable: => element is Decodable in Protobuf,
        packable:  element is Packable )
  =>  Tactic[ProtobufError]
  =>  collection[element] is Decodable in Protobuf =

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
  =>  ( keyEncodable:   key is Encodable in Protobuf,
        valueEncodable: value is Encodable in Protobuf )
  =>  Map[key, value] is Encodable in Protobuf =

    map =>
      if map.isEmpty then Absent else
        val entries = map.to(List).map: (key, value) =>
          val bytes = printed: printer =>
            printer.field(1, keyEncodable.encode(key))
            printer.field(2, valueEncodable.encode(value))

          Wire(WireType.Len, bytes)

        Repeated(entries)

  given mapDecodable: [key, value]
  =>  ( keyDecodable:   => key is Decodable in Protobuf,
        valueDecodable: => value is Decodable in Protobuf )
  =>  Tactic[ProtobufError]
  =>  Map[key, value] is Decodable in Protobuf =

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
    :   derivation is Decodable in Protobuf =

      val numbers = fieldNumbers[derivation]

      protobuf =>
        provide[Tactic[ProtobufError]]:
          val map = ProtobufParser(protobuf.payload).fields()

          build[derivation]:
            [field0] => context =>
              map.at(numbers(label)).lay(default.or(context.decoded(Protobuf.Absent))): values =>
                context.decoded(Protobuf.Repeated(values))

    inline def disjunction[derivation: SumReflection]: derivation is Decodable in Protobuf =
      protobuf =>
        provide[Tactic[ProtobufError]]:
          provide[Tactic[VariantError]]:
            val map = ProtobufParser(protobuf.payload).fields()
            val labels = variantLabels

            var index = 0
            while index < labels.length && !map.contains(index + 1) do index += 1
            if index >= labels.length then abort(ProtobufError(Reason.MissingField(0)))

            delegate(labels(index)):
              [variant <: derivation] => context =>
                context.decoded(Protobuf.Repeated(map(index + 1)))

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
