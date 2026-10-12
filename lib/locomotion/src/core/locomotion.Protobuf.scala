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

import scala.collection.immutable.Vector

import scala.caps

import scala.language.experimental.pureFunctions

import java.lang as jl
import java.nio.charset.StandardCharsets.UTF_8

import scala.collection.Factory
import scala.compiletime.*
import scala.reflect.ClassTag

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

import Protobuf.Error.Reason
import fulminate.*
import denominative.dysasymptotics.linearSize

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

  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each alias gets its own instance, built at the underlying
  // stdlib type and cast (a no-op at erasure).
  given aliasListEncodable: [list <: List, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^ )
  =>  ( (list[element] is Encodable in Protobuf)^ ) =
    listEncodable[scala.collection.immutable.List, element]
    . asInstanceOf[(list[element] is Encodable in Protobuf)^]

  given aliasListDecodable: [list <: List, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ( (list[element] is Decodable in Protobuf)^ ) =
    listDecodable[scala.collection.immutable.List, element]
    . asInstanceOf[(list[element] is Decodable in Protobuf)^]

  given aliasSetEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^ )
  =>  ( (set[element] is Encodable in Protobuf)^ ) =
    listEncodable[scala.collection.immutable.Set, element]
    . asInstanceOf[(set[element] is Encodable in Protobuf)^]

  given aliasSetDecodable: [set <: Set, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ( (set[element] is Decodable in Protobuf)^ ) =
    listDecodable[scala.collection.immutable.Set, element]
    . asInstanceOf[(set[element] is Decodable in Protobuf)^]

  given aliasSeriesEncodable: [sequence <: Sequence, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^ )
  =>  ( (sequence[element] is Encodable in Protobuf)^ ) =
    listEncodable[Vector, element]
    . asInstanceOf[(sequence[element] is Encodable in Protobuf)^]

  given aliasSeriesDecodable: [sequence <: Sequence, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ( (sequence[element] is Decodable in Protobuf)^ ) =
    listDecodable[Vector, element]
    . asInstanceOf[(sequence[element] is Decodable in Protobuf)^]

  // A frozen array of any other element — messages, text, bytes — is an unpacked repeated
  // field, as a `List` of them is; the packable scalars' arrays pack, in `object Protobuf`.
  given arrayEncodable: [element: ClassTag]
  =>  ( encodable: => (element is Encodable in Protobuf)^ )
  =>  ( ((Array[element]^{}) is Encodable in Protobuf)^{encodable} ) =
    // An honest capability, as `listEncodable` below.
    values =>
      listEncodable[scala.collection.immutable.ArraySeq, element]
      . encoded(scala.collection.immutable.ArraySeq.unsafeWrapArray(Array.unsafeJvm(values)))

  given arrayDecodable: [element: ClassTag]
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ( ((Array[element]^{}) is Decodable in Protobuf)^{decodable} ) =
    // An honest capability, as `listEncodable` below.
    protobuf =>
      val values = listDecodable[scala.collection.immutable.List, element].decoded(protobuf)
      Array.unsafeFrozen(values.toArray)

  given aggregableIn: [value: Decodable in Protobuf] => (tactic: Tactic[Protobuf.Error])
  =>  ( ((value in Protobuf) is Aggregable by Data)^{tactic} ) =

    new Aggregable:
      type Self = value in Protobuf
      type Operand = Data

      // A single in-memory block — the common case — is decoded in place, not copied.
      def aggregate(bytes: Chain[Data]): value in Protobuf =
        val message =
          if !bytes.nil && Chain.tail(bytes).nil then Protobuf.message(Chain.head(bytes))
          else Protobuf.message(Aggregable.bytesData.aggregate(bytes))

        message.as[value].asInstanceOf[value in Protobuf]

      // Gathered once into a single block, rather than materialised as a chain of cells
      // and then copied, as the trait's default would.
      override def accept(stream: (Stream[Data] over Credit)^): value in Protobuf =
        Protobuf.message(Aggregable.bytesData.accept(stream)).as[value]
        . asInstanceOf[value in Protobuf]

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
  =>  ( (collection[element] is Encodable in Protobuf)^{encodable} ) =
    // An honest capability: the instance retains the by-name element codec, itself a
    // capability (every given that includes a tactic is a capability; Jon, 2026-07-12).
    values =>
      val builder = scala.collection.immutable.List.newBuilder[Protobuf]

      // An element's encoding is almost always a single wire value, appended as it is: only a
      // repeated element's occurrences need flattening.
      values.each: element =>
        encodable.encode(element) match
          case Protobuf.Absent           => ()
          case Protobuf.Repeated(inner)  => inner.fold(builder)(_ += _)
          case wire                      => builder += wire

      val occurrences = List.from(builder.result())
      if occurrences.nil then Protobuf.Absent else Protobuf.Repeated(occurrences)

  given listDecodable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]] )
  =>  ( decodable: => (element is Decodable in Protobuf)^ )
  =>  ( (collection[element] is Decodable in Protobuf)^{decodable} ) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val builder = factory.newBuilder

      protobuf.occurrences.each: wire => builder += decodable.decoded(wire)

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

  object Printable:
    // The base of generated printers, as `Parsable.Direct` is of generated parsers: generated
    // code is capture-erased, so the bodies receive the printer and the size table as neutral
    // carriers, and the capabilities are asserted here at the rim.
    abstract class Direct[value] extends Protobuf.Printable:
      type Self = value

      protected def measureCarrier(value: value, sizes: AnyRef): Int
      protected def writeCarrier(value: value, printer: AnyRef, sizes: AnyRef): Unit

      def measure(value: value, sizes: ProtobufSizes^): Int =
        measureCarrier(value, sizes.asInstanceOf[AnyRef])

      def write(value: value, printer: ProtobufPrinter^, sizes: ProtobufSizes^): Unit =
        writeCarrier(value, printer.asInstanceOf[AnyRef], sizes.asInstanceOf[AnyRef])

    def apply[value]
      ( measure0: (value, ProtobufSizes^) => Int,
        write0:   (value, ProtobufPrinter^, ProtobufSizes^) => Unit )
    :   ((value is Protobuf.Printable)^{measure0, write0}) =

      new Protobuf.Printable:
        type Self = value
        def measure(value: value, sizes: ProtobufSizes^): Int = measure0(value, sizes)

        def write(value: value, printer: ProtobufPrinter^, sizes: ProtobufSizes^): Unit =
          write0(value, printer, sizes)

    // The call points for a nominal `Printable` in a field position of a *generated* printer
    // (a recursive message's own instance, or a hand-written one), as `Parsable.parseField`:
    // the carriers are reasserted here, at the audited point.
    def measureField[value](printable: AnyRef, value: value, sizes: AnyRef): Int =
      printable.asInstanceOf[value is Protobuf.Printable]
      . measure(value, sizes.asInstanceOf[ProtobufSizes^])

    def writeField[value](printable: AnyRef, value: value, printer: AnyRef, sizes: AnyRef)
    :   Unit =

      printable.asInstanceOf[value is Protobuf.Printable]
      . write(value, printer.asInstanceOf[ProtobufPrinter^], sizes.asInstanceOf[ProtobufSizes^])

    // The bridge from the AST world: the value's message, encoded once in the measuring pass
    // and written in the second.
    def fromEncodable[value](encodable: (value is Encodable in Protobuf)^)
    :   ((value is Protobuf.Printable)^{encodable}) =

      new Protobuf.Printable:
        type Self = value

        def measure(value: value, sizes: ProtobufSizes^): Int =
          val wire = encodable.encode(value)
          sizes.stash(wire)
          wire.payload.length

        def write(value: value, printer: ProtobufPrinter^, sizes: ProtobufSizes^): Unit =
          printer.raw(sizes.unstash().asInstanceOf[Protobuf].payload)

    // A message's wire bytes through its `Printable`: measured, then written once into an
    // array of exactly that size.
    def print[value](value: value)(using printable: (value is Protobuf.Printable)^): Data =
      val sizes = ProtobufSizes.acquire()
      val size = printable.measure(value, sizes)
      sizes.rewind()

      sized(size): printer =>
        printable.write(value, printer, sizes)
        ProtobufSizes.release(sizes)

  // The direct-printing counterpart of `Encodable in Protobuf`: writes a value's fields
  // straight into the output array through a `ProtobufPrinter`, in two passes — `measure`
  // records every length-delimited size in a `ProtobufSizes`, `write` reads them back — so no
  // `Protobuf` wire value is built for a field, and no nested message is encoded and copied.
  // The opt-in surface is explicit instances and `Inlinable.printable`; `encodablePrinted`
  // below makes `value.in[Protobuf]` direct whenever one is in scope.
  trait Printable extends Typeclass:
    def measure(value: Self, sizes: ProtobufSizes^): Int
    def write(value: Self, printer: ProtobufPrinter^, sizes: ProtobufSizes^): Unit

  // Declared here, not in `Protobuf2` where the derived `encodable` lives, so it wins whenever
  // a `Printable` exists and is otherwise inapplicable — as `aggregableParsed` is for reads.
  // The message is the one `Wire` the printer fills, so `.encode` is its array, uncopied.
  given encodablePrinted: [value]
  =>  ( printable: (value is Protobuf.Printable)^ )
  =>  ( (value is Encodable in Protobuf)^{printable} ) =

    value => Wire(WireType.Len, Printable.print(value))

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
    ( parser: ProtobufParser^, parsable: (value is Protobuf.Parsable)^ )
    ( using tactic: Tactic[Protobuf.Error] )
  :   value =

    parsable.parse(ProtobufReader(parser, tactic))

  // Direct parsing: when the value knows how to consume wire fields itself,
  // the field map is never materialized. Declared here (not in `Protobuf2`,
  // where the `Decodable`-based `aggregableIn` lives) so it wins whenever a
  // `Protobuf.Parsable` exists, and is otherwise inapplicable — existing
  // code resolves exactly as before. It captures
  // the parsable and the tactic it uses.
  given aggregableParsed: [value]
  =>  ( parsable: (value is Protobuf.Parsable)^ )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( ((value in Protobuf) is Aggregable by Data)^{parsable, tactic} ) =

    new Aggregable:
      type Self = value in Protobuf
      type Operand = Data

      def aggregate(bytes: Chain[Data]): value in Protobuf =
        // A single in-memory block — the common case — is read in place; the general
        // path pulls the chain's cells as the parser needs them.
        if !bytes.nil && bytes.stdlib.tail.isEmpty
        then
          parseDirect(ProtobufParser(bytes.stdlib.head), parsable).asInstanceOf[value in Protobuf]
        else
          parseDirect(ProtobufParser(bytes), parsable).asInstanceOf[value in Protobuf]

      // The parameter is not `consume`: `Aggregable.accept`'s signature is pinned
      // non-consuming by overrides in modules outside separation checking, so the stream
      // crosses to the consuming parser as a neutral reference — each accept call delivers
      // a stream that is used exactly once, by construction.
      override def accept(stream: (Stream[Data] over Credit)^): value in Protobuf =
        val moved: AnyRef = stream.asInstanceOf[AnyRef]
        parseDirect(ProtobufParser(moved.asInstanceOf[(Stream[Data] over Credit)^]), parsable)
        . asInstanceOf[value in Protobuf]

  // A chain already in hand — `Chain(bytes).read[…]`, the common in-memory read — goes to
  // its `Aggregable`'s own `aggregate`, which reads a single block in place, rather than
  // through a stream over the chain, as the generic composed `Readable` would route it.
  // Concrete in `Chain[Data]`, so it beats the composed pipeline by specificity.
  given chainReadable: [value]
  =>  ( aggregable: ((value in Protobuf) is Aggregable by Data)^ )
  =>  ( (Chain[Data] is Readable to (value in Protobuf))^{aggregable} ) =

    (chain: Chain[Data]) => aggregable.aggregate(chain)

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream.
  // Concrete in `Data`, so it beats the composed pipeline by specificity.
  // Captures what it parses with, like `aggregableParsed` above.
  given readableParsed: [value]
  =>  ( parsable: (value is Protobuf.Parsable)^ )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (Data is Readable to (value in Protobuf))^{parsable, tactic} ) =

    (data: Data) => parseDirect(ProtobufParser(data), parsable).asInstanceOf[value in Protobuf]

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
      val number = ordinal.n1

      safely:
        val fields = ProtobufParser.fields(origin.payload)

        if !fields.defines(number) then origin else
          val bytes = printed(origin.payload.length): printer =>
            fields.each: (key, values) =>
              val value = Protobuf.Repeated(values)
              printer.field(key, if key == number then lambda(value) else value)

          Protobuf.Wire(WireType.Len, bytes)

      . or(origin)

  // Synchronously assemble wire bytes of unknown length by running `lambda` against a
  // `ProtobufPrinter`, starting from a buffer of `hint` bytes.
  private def printed(hint: Int)(lambda: ProtobufPrinter^ => Unit): Data =
    Array.collect[Byte](hint): scribe => lambda(ProtobufPrinter(scribe))

  // The same, for wire bytes whose length is known: written into an array of exactly that
  // size, which is returned without a copy.
  private def sized(size: Int)(lambda: ProtobufPrinter^ => Unit): Data =
    Array.scribe[Byte](size): scribe => _ => lambda(ProtobufPrinter(scribe))

  // A message from its fields' numbers and encoded values, in declaration order: one pass
  // sums their encoded size, the second writes them.
  private def message(numbers: Array[Int]^{}, wires: Array[Protobuf]^{}): Data =
    val numberView = numbers.readable
    val wireView = wires.readable
    var size = 0
    var index = 0

    repeat(numberView.length):
      size += ProtobufPrinter.fieldSize(numberView(index), wireView(index))
      index += 1

    sized(size): printer =>
      var index = 0

      repeat(numberView.length):
        printer.field(numberView(index), wireView(index))
        index += 1

  // Chain a message's wire bytes incrementally (chunked); the producing code runs on a separate
  // fiber. The bytes themselves are already assembled (Protobuf length-prefixes nested messages, so
  // their lengths must be known up front); this hands them out in bounded chunks.
  def emit(value: Protobuf)(using Monitor, Probate): Iterator[Data] =
    val producer = Producer[Data]()
    val output = producer.iterator

    producer.transfer: (producer, _, _) ?=>
      producer().put(value.payload)
      producer().finish()

    output

  private def readVarint(protobuf: Protobuf)(using Tactic[Protobuf.Error]): Long =
    if protobuf.isAbsent then 0L else ProtobufParser.varintOf(protobuf.payload)

  private def readFixed32(protobuf: Protobuf)(using Tactic[Protobuf.Error]): Int =
    if protobuf.isAbsent then 0 else ProtobufParser.fixed32Of(protobuf.payload)

  private def readFixed64(protobuf: Protobuf)(using Tactic[Protobuf.Error]): Long =
    if protobuf.isAbsent then 0L else ProtobufParser.fixed64Of(protobuf.payload)

  // Zig-zag maps signed integers to unsigned so small-magnitude negatives stay
  // short as varints: 0→0, -1→1, 1→2, -2→3, …
  private def zigzag(value: Long): Long = (value << 1) ^ (value >> 63)
  private def unzigzag(value: Long): Long = (value >>> 1) ^ -(value & 1)

  private def utf8(text: Text): Data = Array.unsafeFrozen(text.s.getBytes(UTF_8).nn)

  given intEncodable: Int is Encodable in Protobuf =
    int => Wire(WireType.Varint, ProtobufPrinter.varintData(int.toLong))

  given longEncodable: Long is Encodable in Protobuf =
    long => Wire(WireType.Varint, ProtobufPrinter.varintData(long))

  given booleanEncodable: Boolean is Encodable in Protobuf =
    boolean => Wire(WireType.Varint, ProtobufPrinter.varintData(if boolean then 1L else 0L))

  given doubleEncodable: Double is Encodable in Protobuf =
    double => Wire(WireType.I64, ProtobufPrinter.fixedData(jl.Double.doubleToLongBits(double), 8))

  given floatEncodable: Float is Encodable in Protobuf =
    float => Wire(WireType.I32, ProtobufPrinter.fixedData(jl.Float.floatToIntBits(float).toLong, 4))

  given textEncodable: Text is Encodable in Protobuf = text => Wire(WireType.Len, utf8(text))
  given dataEncodable: Data is Encodable in Protobuf = bytes => Wire(WireType.Len, bytes)

  given intDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (Int is Decodable in Protobuf)^{tactic, caps.any} ) =
    readVarint(_).toInt

  given longDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (Long is Decodable in Protobuf)^{tactic, caps.any} ) =
    readVarint(_)

  given booleanDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (Boolean is Decodable in Protobuf)^{tactic, caps.any} ) =
    readVarint(_) != 0

  given doubleDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (Double is Decodable in Protobuf)^{tactic, caps.any} ) =
    protobuf =>
      if protobuf.isAbsent then 0.0
      else jl.Double.longBitsToDouble(ProtobufParser.fixed64Of(protobuf.payload))

  given floatDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (Float is Decodable in Protobuf)^{tactic, caps.any} ) =
    protobuf =>
      if protobuf.isAbsent then 0.0f
      else jl.Float.intBitsToFloat(ProtobufParser.fixed32Of(protobuf.payload))

  given textDecodable: Text is Decodable in Protobuf =
    protobuf => jl.String(Array.unsafeJvm(protobuf.payload), UTF_8).nn.tt

  given dataDecodable: Data is Decodable in Protobuf = _.payload

  // hypotenuse typed integers select the protobuf integer encoding by type, with no
  // annotation: U32/U64 → uint (plain varint), S32/S64 → sint (zig-zag varint),
  // B32/B64 → fixed32/fixed64 (little-endian).

  given u32Encodable: U32 is Encodable in Protobuf =
    u32 => Wire(WireType.Varint, ProtobufPrinter.varintData(u32.long))

  given u64Encodable: U64 is Encodable in Protobuf =
    u64 => Wire(WireType.Varint, ProtobufPrinter.varintData(u64.bits.s64.long))

  given s32Encodable: S32 is Encodable in Protobuf =
    s32 => Wire(WireType.Varint, ProtobufPrinter.varintData(zigzag(s32.long)))

  given s64Encodable: S64 is Encodable in Protobuf =
    s64 => Wire(WireType.Varint, ProtobufPrinter.varintData(zigzag(s64.long)))

  given b32Encodable: B32 is Encodable in Protobuf =
    b32 => Wire(WireType.I32, ProtobufPrinter.fixedData(b32.s32.int.toLong, 4))

  given b64Encodable: B64 is Encodable in Protobuf =
    b64 => Wire(WireType.I64, ProtobufPrinter.fixedData(b64.s64.long, 8))

  given u32Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (U32 is Decodable in Protobuf)^{tactic, caps.any} ) =
    readVarint(_).toInt.bits.u32

  given u64Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (U64 is Decodable in Protobuf)^{tactic, caps.any} ) =
    readVarint(_).bits.u64

  given s32Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (S32 is Decodable in Protobuf)^{tactic, caps.any} ) =
    protobuf => unzigzag(readVarint(protobuf)).toInt.bits.s32

  given s64Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (S64 is Decodable in Protobuf)^{tactic, caps.any} ) =
    protobuf => unzigzag(readVarint(protobuf)).bits.s64

  given b32Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (B32 is Decodable in Protobuf)^{tactic, caps.any} ) =
    readFixed32(_).bits

  given b64Decodable: (tactic: Tactic[Protobuf.Error])
  =>  ( (B64 is Decodable in Protobuf)^{tactic, caps.any} ) =
    readFixed64(_).bits

  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: (inner is Encodable in Protobuf)^ )
  =>  ( (value is Encodable in Protobuf)^{encodable} ) =

    new Encodable:
      type Self = value
      type Form = Protobuf

      def encoded(value: value): Protobuf =
        value.let(_.asInstanceOf[inner]).let(encodable.encode(_)).or(Absent)

  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( decodable: => (inner is Decodable in Protobuf)^ )
  =>  ( (value is Decodable in Protobuf)^{decodable} ) =
    // An honest capability, as `listEncodable` above.
    protobuf => if protobuf.isAbsent then Unset else decodable.decoded(protobuf)

  // Packed `repeated` for numeric scalars (proto3 default): all elements are
  // concatenated into one length-delimited field. Higher priority than the unpacked
  // givens in `Protobuf2` because `Packable` constrains the element type.

  given packedEncodable: [collection <: Iterable, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^, packable: element is Packable )
  =>  ( (collection[element] is Encodable in Protobuf)^{encodable} ) =
    // An honest capability, as `listEncodable` above.
    values =>
      val list = values.to(List)

      if list.nil then Absent else
        val hint = list.size*(packable.wireType match
          case WireType.I64 => 8
          case WireType.I32 => 4
          case _            => 2)

        val bytes = printed(hint): printer =>
          list.each: element => printer.raw(encodable.encode(element).payload)

        Wire(WireType.Len, bytes)

  given packedDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        decodable: => (element is Decodable in Protobuf)^,
        packable:  element is Packable )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (collection[element] is Decodable in Protobuf)^{tactic, decodable} ) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val builder = factory.newBuilder

      // Accept both packed (one Len field of concatenated values) and unpacked (a
      // value per occurrence) wire forms, per the proto3 compatibility rule.
      protobuf.occurrences.each: wire =>
        if wire.wireKind == WireType.Len && packable.wireType != WireType.Len
        then ProtobufParser.packed(wire.payload, packable.wireType).each: element =>
          builder += decodable.decoded(element)
        else builder += decodable.decoded(wire)

      builder.result()

  // Alias counterparts of the packed givens: the opaque prelude collections do
  // not conform to `Iterable`, so each delegates at the underlying stdlib type
  // and casts. Same scope as `packedEncodable`/`packedDecodable`, so they take
  // priority over the unpacked alias givens in `Protobuf2` exactly as the
  // blanket packed givens do over the unpacked blankets.
  given packedListEncodable: [list <: List, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^, packable: element is Packable )
  =>  ( (list[element] is Encodable in Protobuf)^ ) =
    packedEncodable[scala.collection.immutable.List, element]
    . asInstanceOf[(list[element] is Encodable in Protobuf)^]

  given packedListDecodable: [list <: List, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^, packable: element is Packable )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (list[element] is Decodable in Protobuf)^{tactic, caps.any} ) =
    packedDecodable[scala.collection.immutable.List, element]
    . asInstanceOf[(list[element] is Decodable in Protobuf)^{tactic, caps.any}]

  given packedSetEncodable: [set <: Set, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^, packable: element is Packable )
  =>  ( (set[element] is Encodable in Protobuf)^ ) =
    packedEncodable[scala.collection.immutable.Set, element]
    . asInstanceOf[(set[element] is Encodable in Protobuf)^]

  given packedSetDecodable: [set <: Set, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^, packable: element is Packable )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (set[element] is Decodable in Protobuf)^{tactic, caps.any} ) =
    packedDecodable[scala.collection.immutable.Set, element]
    . asInstanceOf[(set[element] is Decodable in Protobuf)^{tactic, caps.any}]

  given packedSeriesEncodable: [sequence <: Sequence, element]
  =>  ( encodable: => (element is Encodable in Protobuf)^, packable: element is Packable )
  =>  ( (sequence[element] is Encodable in Protobuf)^ ) =
    packedEncodable[Vector, element]
    . asInstanceOf[(sequence[element] is Encodable in Protobuf)^]

  given packedSeriesDecodable: [sequence <: Sequence, element]
  =>  ( decodable: => (element is Decodable in Protobuf)^, packable: element is Packable )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (sequence[element] is Decodable in Protobuf)^{tactic, caps.any} ) =
    packedDecodable[Vector, element]
    . asInstanceOf[(sequence[element] is Decodable in Protobuf)^{tactic, caps.any}]

  // A frozen array of packable scalars — `Array[Long]^{}`, `Array[Double]^{}` — is a packed
  // `repeated` field too, in the unboxed representation: a generated parser fills it without
  // boxing an element and a generated printer walks it by index. The bytes are the `List`
  // form's. One instance per scalar, each as pure as the scalar's own, so a derivation over an
  // array field sees the shape it sees for `Data`. (`Array[Byte]^{}` is `Data`, a scalar.)
  private def packedArray[element: {Packable, ClassTag}]
    ( encodable: element is Encodable in Protobuf, values: Array[element]^{} )
  :   Protobuf =

    packedEncodable[scala.collection.immutable.ArraySeq, element](using encodable, infer)
    . encoded(scala.collection.immutable.ArraySeq.unsafeWrapArray(Array.unsafeJvm(values)))

  private def unpackedArray[element: {Packable, ClassTag}]
    ( decodable: (element is Decodable in Protobuf)^, protobuf: Protobuf )
    ( using Tactic[Protobuf.Error] )
  :   Array[element]^{} =

    val values =
      packedDecodable[scala.collection.immutable.List, element](using infer, decodable, infer)
      . decoded(protobuf)

    Array.unsafeFrozen(values.toArray)

  given intArrayEncodable: (Array[Int]^{}) is Encodable in Protobuf =
    packedArray(intEncodable, _)

  given longArrayEncodable: (Array[Long]^{}) is Encodable in Protobuf =
    packedArray(longEncodable, _)

  given booleanArrayEncodable: (Array[Boolean]^{}) is Encodable in Protobuf =
    packedArray(booleanEncodable, _)

  given doubleArrayEncodable: (Array[Double]^{}) is Encodable in Protobuf =
    packedArray(doubleEncodable, _)

  given floatArrayEncodable: (Array[Float]^{}) is Encodable in Protobuf =
    packedArray(floatEncodable, _)

  given intArrayDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( ((Array[Int]^{}) is Decodable in Protobuf)^{tactic, caps.any} ) =
    unpackedArray(intDecodable, _)

  given longArrayDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( ((Array[Long]^{}) is Decodable in Protobuf)^{tactic, caps.any} ) =
    unpackedArray(longDecodable, _)

  given booleanArrayDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( ((Array[Boolean]^{}) is Decodable in Protobuf)^{tactic, caps.any} ) =
    unpackedArray(booleanDecodable, _)

  given doubleArrayDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( ((Array[Double]^{}) is Decodable in Protobuf)^{tactic, caps.any} ) =
    unpackedArray(doubleDecodable, _)

  given floatArrayDecodable: (tactic: Tactic[Protobuf.Error])
  =>  ( ((Array[Float]^{}) is Decodable in Protobuf)^{tactic, caps.any} ) =
    unpackedArray(floatDecodable, _)

  // `Map[K, V]` encodes as a `repeated` message of map entries, each a two-field
  // message `{ key = 1; value = 2 }` — the proto3 map wire format.

  given mapEncodable: [key, value]
  =>  ( keyEncodable:   (key is Encodable in Protobuf)^,
        valueEncodable: (value is Encodable in Protobuf)^ )
  =>  ( (Map[key, value] is Encodable in Protobuf)^{keyEncodable, valueEncodable} ) =

    map =>
      if map.nil then Absent else
        val entries = map.to[List].map: (key, value) =>
          val keyWire = keyEncodable.encode(key)
          val valueWire = valueEncodable.encode(value)
          val size = ProtobufPrinter.fieldSize(1, keyWire) + ProtobufPrinter.fieldSize(2, valueWire)

          val bytes = sized(size): printer =>
            printer.field(1, keyWire)
            printer.field(2, valueWire)

          Wire(WireType.Len, bytes)

        Repeated(entries)

  given mapDecodable: [key, value]
  =>  ( keyDecodable:   => (key is Decodable in Protobuf)^,
        valueDecodable: => (value is Decodable in Protobuf)^ )
  =>  ( tactic: Tactic[Protobuf.Error] )
  =>  ( (Map[key, value] is Decodable in Protobuf)^{tactic, keyDecodable, valueDecodable} ) =
    // An honest capability, as `listEncodable` above.
    protobuf =>
      val entries = protobuf.occurrences.map: entry =>
        val fields = ProtobufParser.scan(entry)
        (keyDecodable.decoded(fields(1)), valueDecodable.decoded(fields(2)))

      entries.to[Map]

  object EncodableDerivation extends Derivable[Encodable in Protobuf]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Protobuf =

      val annotated: Map[Text, Set[field]] = infer[derivation is Annotated by field] match
        case annotated: Annotated.Fields => annotated.fields
        case _                           => Map()

      value =>
        // Each number is computed exactly as `fieldNumbers` would, from the field's own label
        // and index — no map lookup whose totality would need asserting.
        val numbers: Array[Int]^{} = fields(value):
          [field0] => _ => annotated(label).let(_.stdlib.head.number).or(index + 1)

        val wires: Array[Protobuf]^{} = fields(value):
          [field0] => fieldValue => contextual.encode(fieldValue)

        Protobuf.Wire(WireType.Len, message(numbers, wires))

    inline def disjunction[derivation: SumReflection]: derivation is Encodable in Protobuf =
      value =>
        variant(value):
          [variant <: derivation] => variantValue =>
            val wire = contextual.encode(variantValue)

            // The parameter ascription pins the lambda's type at each expansion site; without
            // it, repeated expansions in one unit reuse a memoized capability root (#26547).
            val payload = sized(ProtobufPrinter.fieldSize(index + 1, wire)):
              (printer: ProtobufPrinter^) => printer.field(index + 1, wire)

            Protobuf.Wire(WireType.Len, payload)

  object DecodableDerivation extends Derivable[Decodable in Protobuf]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   (derivation is Decodable in Protobuf)^ =

      val annotated: Map[Text, Set[field]] = infer[derivation is Annotated by field] match
        case annotated: Annotated.Fields => annotated.fields
        case _                           => Map()

      // The decode lambda closes over the resolution-scoped `Tactic` that `provide` summons
      // at the derivation site, sharing the instance's given-resolution lifetime; the fresh
      // (`^`) trait result honestly admits the capture — no seal.
      { protobuf =>
        provide[Tactic[Protobuf.Error]]:
          val table = ProtobufParser.scan(protobuf)

          build[derivation]:
            [field0] => context =>
              val wire = table(annotated(label).let(_.stdlib.head.number).or(index + 1))
              if wire.isAbsent then default.or(context.decoded(Protobuf.Absent))
              else context.decoded(wire) }

    inline def disjunction[derivation: SumReflection]: (derivation is Decodable in Protobuf)^ =
      // A fresh (`^`) result honestly admitting the capture, as for `conjunction` above.
      { protobuf =>
        provide[Tactic[Protobuf.Error]]:
          provide[Tactic[Variant.Error]]:
            val table = ProtobufParser.scan(protobuf)
            val labels = variantLabels

            val (label, index) =
              labels.indexed.seek { (_, ordinal) => table.defines(ordinal.n1) }.or:
                abort(Protobuf.Error(Reason.MissingField(0)))

            delegate(label):
              [variant <: derivation] => context =>
                val wire = table(index.n1)
                if wire.isAbsent then abort(Protobuf.Error(Reason.MissingField(index.n1)))
                else context.decoded(wire) }

  // ProtobufError → Protobuf.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Truncated(offset)       => m"the protobuf data ended unexpectedly at offset $offset"
        case MalformedVarint(offset) => m"a varint was malformed at offset $offset"
        case MissingField(number)    => m"the required field number $number was missing"

        case UnexpectedWireType(found, offset) =>
          m"an unexpected wire type, $found, was encountered at offset $offset"

        case Overflow(offset) =>
          m"a numeric value was too large for its type at offset $offset"

    // The `offset` on the byte-level reasons is the position, in bytes, within the
    // protobuf message (or sub-message payload) currently being parsed — not an
    // absolute offset into the original input. Scalar fields and nested messages are
    // each decoded with a fresh parser over their own payload slice, so only the
    // top-level message's offsets are absolute. `MissingField` is a semantic
    // (decode-level) failure and carries no offset.
    enum Reason:
      case Truncated(offset: Int)
      case MalformedVarint(offset: Int)
      case UnexpectedWireType(found: Int, offset: Int)
      case Overflow(offset: Int)
      case MissingField(number: Int)

  case class Error(reason: Protobuf.Error.Reason)(using Diagnostics)
  extends fulminate.Error(700, 0)(m"the protobuf data was not valid: $reason")

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
    case Repeated(values) => if values.nil then Protobuf.Absent else List.last(values)
    case other            => other

  // Every wire value recorded for this field — used to decode `repeated` fields.
  def occurrences: List[Protobuf] = this match
    case Repeated(values) => values
    case Absent           => Nil
    case wire             => wire :: Nil

  def payload: Data = single match
    case Wire(_, bytes) => bytes
    case _              => Array.empty[Byte]

  // The wire type of the (single) value, or `Len` for absent — used to tell a packed
  // scalar field (`Len`) from inline-encoded scalar values during repeated decoding.
  def wireKind: WireType = single match
    case Wire(wireType, _) => wireType
    case _                 => WireType.Len

  def isAbsent: Boolean = this match
    case Absent        => true
    case Repeated(Nil) => true
    case _             => false

  def as[value: Decodable in Protobuf]: value raises Protobuf.Error = value.decoded(this)
