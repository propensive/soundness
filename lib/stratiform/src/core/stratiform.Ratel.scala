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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gastronomy.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

import RatelLayout.{Component, Entry, Kind, Layout, Payload, Slot}

// A RaTEL image: the random-access encoding of one TEL document together with its schema
// (RaTEL §4), as the bytes of a file. It is written whole, and published by renaming it over
// its predecessor; it is read in place, through `Ratel.open`, from a view of the file which
// any number of processes may map.
case class Ratel(data: Data):
  // A view of the image where it lies on the heap, for reading it without a file.
  def terrain: Terrain = Terrain(data)

object Ratel:
  given encodable: Ratel is Encodable in Data = _.data

  // The image of a document's semantic model under a schema, which is given as it is
  // declared, with its layers, and is carried in the image (§5.2). Scalars with an encoding
  // are stored as the bytes of the codec bound to it; `digests` adds a digest of each section
  // (§5.7), by which a reader may detect corruption.
  def encode
    ( element: Tel.Element,
      schema:  Tels,
      codecs:  Tel.Codec.Bindings = Tel.Codec.Bindings.all,
      digests: Boolean            = true )
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Ratel =

    Ratel(RatelWriter.write(element, schema, codecs, digests))

  // An image is `Viewable`, as the view of its bytes where they lie on the heap, so it is
  // opened as a file which holds it is.
  given viewable: Ratel is Viewable:
    def view[result](value: Ratel)(lambda: Terrain^ => result): result = lambda(value.terrain)

  // Anything `Viewable` -- a path, an image, a view -- is opened as `Ratel` for reading in
  // place: `file.open[Ratel]()`. The reader is given to the block, and to nothing beyond it.
  given openable: [source]
  =>  ( viewable: (source is Viewable)^,
        tactic:   Tactic[Ratel.Error] )
  =>  ( Ratel.Openable[source]^{viewable, tactic} ) =
    Ratel.Openable[source]()

  // What may be asked of `open[Ratel]`: `Verify` makes it a verifying reader (§7.2), which
  // checks the whole image before the block runs, and `Codecs` binds the codecs of encodings
  // which this module does not define, ahead of those it does.
  enum Flag:
    case Verify
    case Codecs(bindings: Tel.Codec.Bindings)

  class Openable[source]
    ( using viewable: (source is Viewable)^, tactic: Tactic[Ratel.Error] )
  extends aperture.Openable:
    type Self = source
    type Form = Ratel
    type Operand = Ratel.Flag
    type Result = Reader

    def open[grants <: Grant, result]
      ( value: source, mode: Mode granting grants, flags: List[Ratel.Flag] )
      ( block: ((Reader & Granting[grants])^) ?=> result )
    :   result =

      viewable.view(value)(reading(_, mode, flags)(block))

  private def reading[grants <: Grant, result]
    ( view: Terrain^, mode: Mode granting grants, flags: List[Ratel.Flag] )
    ( block: ((Reader & Granting[grants])^) ?=> result )
    ( using tactic: Tactic[Ratel.Error] )
  :   result =

    // An image is never changed where it lies: a new one is published over it.
    if mode.atoms.has(Write) then fail(Error.Reason.Unwritable)

    val codecs = flags.fuse(Tel.Codec.Bindings.all):
      next match
        case Flag.Codecs(bindings) => bindings.orElse(state)
        case Flag.Verify           => state

    val reader = new Reader(view, inspect(view, codecs)) with Granting[grants]:
      type Topic = Unit

    if flags.has(Flag.Verify) then reader.verify()
    block(using reader)

  // What `open` learns of an image before anything is read from it: its schema and layout,
  // and where its sections, tables and indexes lie.
  private[stratiform] case class Opening
    ( schema:    Tels,
      signature: Data,
      valueHash: Optional[Digest in Blake3],
      layout:    Layout,
      codecs:    Tel.Codec.Bindings,
      sections:  Array[Extent]^{},
      tables:    Array[Extent]^{},
      indexes:   Array[Extent]^{} )

  private[stratiform] def open
    ( view: Terrain^, codecs: Tel.Codec.Bindings = Tel.Codec.Bindings.all )
    ( using Tactic[Ratel.Error] )
  :   Reader^{view} =

    val opening = inspect(view, codecs)
    new Unbranded(view, opening)

  // §7.1: everything which decides how the rest of an image is read -- the header, the schema
  // and its signature, the directories of tables and indexes -- is checked here, and nothing
  // else is read until it is asked for.
  private[stratiform] def inspect(view: Terrain^, codecs: Tel.Codec.Bindings)
    ( using Tactic[Ratel.Error] )
  :   Opening =

    import Error.Reason

    if view.size < RatelBytes.header then fail(Reason.BadLength)

    // The header is copied before it is acted on: a mapped file may change beneath a reader.
    val header: Data = view.read(0L, RatelBytes.header)
    val magic: Data = header.segment(Prim till Quin)
    if RatelBytes.order(magic, RatelBytes.magic) != 0 then fail(Reason.BadMagic)
    if header.u8(4) != 1 then fail(Reason.UnsupportedVersion)
    val flags = header.u16(6)
    if header.u8(5) != 0 || (flags & ~3) != 0 then fail(Reason.ReservedSet)
    val committed = header.u64(8)

    if committed < RatelBytes.header || committed%8 != 0 || committed > view.size
    then fail(Reason.BadLength)

    def extent(number: Int): Extent =
      val offset = header.u64(16 + 16*number)
      val length = header.u64(24 + 16*number)

      val aligned = offset%8 == 0 && length%8 == 0 && offset >= RatelBytes.header
      val within = length >= 0 && offset <= committed && length <= committed - offset
      if !aligned || !within then fail(Reason.BadDirectory)

      Extent(offset, length)

    val schemaExtent = extent(0)
    val stringsExtent = extent(1)
    val variableExtent = extent(2)
    val tablesExtent = extent(3)
    val indexesExtent = extent(4)
    val digestsExtent = extent(5)

    val extents: List[Extent] =
      List(schemaExtent, stringsExtent, variableExtent, tablesExtent, indexesExtent, digestsExtent)

    extents.each: left =>
      extents.each: right =>
        val occupied = !(left eq right) && left.length > 0 && right.length > 0
        val overlap = left.offset < right.end && right.offset < left.end
        if occupied && overlap then fail(Reason.SectionsOverlap)

    val digests = if (flags & 1) != 0 then RatelBytes.digests else 0
    if digestsExtent.length != digests then fail(Reason.DigestMismatch)
    if schemaExtent.length > Int.MaxValue then fail(Reason.SchemaUndecodable)

    // The schema section is a signature and a schema body, each behind its length (§5.2).
    val section: Data = view.read(schemaExtent.offset, schemaExtent.length.toInt)

    def varint(offset: Int, reason: Reason): Varint.Decoded =
      mitigate:
        case _: Varint.Error => error(reason)

      . protect(Varint.decode(section, offset))

    val signatureLength = varint(0, Reason.BadSignature)

    val signatureSpace = section.length - signatureLength.next
    if signatureLength.value > signatureSpace then fail(Reason.BadSignature)

    val signatureEnd = signatureLength.next + signatureLength.value.toInt
    val signature: Data = section.segment(signatureLength.next.z till signatureEnd.z)
    if Tel.Codec.signatureComponents(signature).absent then fail(Reason.BadSignature)
    val bodyLength = varint(signatureEnd, Reason.SchemaUndecodable)
    if bodyLength.value > section.length - bodyLength.next then fail(Reason.SchemaUndecodable)
    val bodyEnd = bodyLength.next + bodyLength.value.toInt
    val body: Data = section.segment(bodyLength.next.z till bodyEnd.z)
    val axiom = Tels.Axiom.tels

    val (schema, recomputed) =
      mitigate:
        case _: Tel.Error    => error(Reason.SchemaUndecodable)
        case _: Bintel.Error => error(Reason.SchemaUndecodable)

      . protect:
          Bintel.decode(body, axiom) match
            case root: Tel.Element.Node =>
              ( Tels.Layers.compose((root: Tel.Element).as[Tels]),
                SchemaSignature.fromElement(root, axiom) )

            case value: Tel.Element.Value =>
              fail(Reason.SchemaUndecodable)

    // Nothing else is interpreted until the schema is known to be the one which was signed.
    if RatelBytes.order(recomputed, signature) != 0 then fail(Reason.SignatureMismatch)
    val layout = RatelLayout(schema)

    val tables: List[Extent] =
      layout.tables.to[List].zip(List.range(0, layout.tables.length)).map: (table, number) =>
        val entry = tablesExtent.offset + 24L*number
        if 24L*(number + 1) > tablesExtent.length then fail(Reason.BadTableDirectory)
        val offset = view.u64(entry)
        val count = view.u32(entry + 8)

        val described = view.u32(entry + 12) == table.rowSize && view.u64(entry + 16) == 0L
        val within = offset%8 == 0 && offset >= 0 && offset <= tablesExtent.length
        val fits = count*table.rowSize <= tablesExtent.length - offset
        if !described || !within || !fits then fail(Reason.BadTableDirectory)

        Extent(tablesExtent.offset + offset, count)

    if tables.head.let(_.length).or(0L) != 1L then fail(Reason.BadRootTable)

    val indexes: List[Extent] = List.range(0, layout.indexes.length).map: number =>
      val entry = indexesExtent.offset + 16L*number
      if 16L*(number + 1) > indexesExtent.length then fail(Reason.BadIndex)
      val offset = view.u64(entry)
      val count = view.u32(entry + 8)

      val within = offset%8 == 0 && offset >= 0 && offset <= indexesExtent.length
      val fits = 4*(count + 1) <= indexesExtent.length - offset
      if view.u32(entry + 12) != 0L || !within || !fits then fail(Reason.BadIndex)

      val entries = view.u32(indexesExtent.offset + offset + 4*count)
      if entries > indexesExtent.length - offset - 4*(count + 1) then fail(Reason.BadIndex)
      Extent(indexesExtent.offset + offset, count)

    schema.scalars.each: scalar =>
      scalar.encoding.let: name =>
        if codecs(name).absent then fail(Reason.CodecUnresolved)

    val hash: Optional[Digest in Blake3] =
      if (flags & 2) != 0 then Digest[Blake3](header.segment(112.z till 144.z)) else Unset

    Opening
      ( schema, signature, hash, layout, codecs, extents.to[Array], tables.to[Array],
        indexes.to[Array] )

  private[stratiform] def error(reason: Error.Reason): Error =
    import errorDiagnostics.emptyDiagnostics
    Error(reason)

  private[stratiform] def fail(reason: Error.Reason)(using Tactic[Ratel.Error]): Nothing =
    abort(error(reason))

  // A range of a file: a section, or a table's rows, or an index. For a table or an index,
  // `length` counts its rows or its entries.
  private[stratiform] case class Extent(offset: Long, length: Long):
    def end: Long = offset + length

  // An open image. It serves reads for as long as the view it was opened on is mapped, and
  // may be shared between threads.
  abstract class Reader private[stratiform](view: Terrain^, opening: Opening):
    // The brand of this reader, which every row it gives carries, as `Row of reader.Topic`:
    // a type which belongs to this reader alone, so that a row is read through the reader of
    // the image it came from, and through no other.
    type Topic

    import Error.Reason
    import opening.{codecs, indexes, layout, sections, tables}

    // The composed schema which the image carries, and the signature it carries it under. A
    // reader which requires a schema it trusts compares the signature with those it knows.
    def schema: Tels = opening.schema
    def signature: Data = opening.signature

    // The document's value hash, if the header carries it.
    def valueHash: Optional[Digest in Blake3] = opening.valueHash

    private def section(number: Int): Extent = sections.at(number.z).or(Extent(0L, 0L))

    private[stratiform] def plan(table: Int)(using Tactic[Ratel.Error]): RatelLayout.Table =
      layout.table(table).or(fail(Reason.BadRecordIndex))

    // The number of rows in a table.
    private[stratiform] def count(table: Int): Long =
      tables.at(table.z).let(_.length).or(0L)

    // §7.2: the address of a row, once its record index is known to be within its table.
    private[stratiform] def address(table: Int, row: Long)(using Tactic[Ratel.Error]): Long =
      val extent = tables.at(table.z).or(fail(Reason.BadRecordIndex))
      if row < 0 || row >= extent.length then fail(Reason.BadRecordIndex)
      extent.offset + row*plan(table).rowSize

    private[stratiform] def present(address: Long, slot: Slot): Boolean =
      val bit = slot.presence
      bit < 0 || (view.u8(address + (bit >> 3)) & 1 << (bit & 7)) != 0

    private[stratiform] def word(address: Long): Long = view.u32(address)

    // The body of an item of a region, as its address and its header: a ref is a multiple of
    // four, and the item it denotes lies wholly within its region (R14). `unit` is the size
    // of each of the things which the header counts.
    private[stratiform] def item(strings: Boolean, ref: Long, unit: Long)
      ( using Tactic[Ratel.Error] )
    :   Extent =

      val region = section(if strings then 1 else 2)
      if ref%4 != 0 || ref > region.length - 4 then fail(Reason.BadRef)
      val header = view.u32(region.offset + ref)
      if header*unit > region.length - ref - 4 then fail(Reason.BadRef)
      Extent(region.offset + ref + 4, header)

    private[stratiform] def bytes(strings: Boolean, ref: Long)(using Tactic[Ratel.Error]): Data =
      val extent = item(strings, ref, 1L)
      if extent.length > Int.MaxValue then fail(Reason.BadRef)
      view.read(extent.offset, extent.length.toInt)

    private[stratiform] def copy(address: Long, length: Int): Data = view.read(address, length)

    // The text of a scalar's bytes: UTF-8, or whatever the codec of its encoding makes of
    // them.
    private[stratiform] def text(scalar: Tels.Scalar, bytes: Data)(using Tactic[Ratel.Error])
    :   Text =

      scalar.encoding.lay(if RatelBytes.utf8(bytes) then bytes.utf8 else fail(Reason.BadUtf8)):
        name =>
          codecs(name).or(fail(Reason.CodecUnresolved)).decode(bytes) match
            case Tel.Codec.Decoded.Value(text)      => text
            case Tel.Codec.Decoded.Failure(message) => fail(Reason.CodecDecodeFailed)

    // The bytes of the value in a scalar's slot of a row, where the member is present.
    private[stratiform] def load(address: Long, slot: Slot)(using Tactic[Ratel.Error]): Data =
      slot.kind match
        case Kind.Inline(_, width) => view.read(address + slot.offset, width)
        case Kind.Ref(_, intern)   => bytes(intern, word(address + slot.offset))
        case _                     => fail(Reason.Inapplicable(slot.keyword))

    // The document's root: the one row of table 0.
    def root(using Tactic[Ratel.Error]): Row of Topic =
      Row[Topic](0, 0L, plan(0).entries, plan(0).struct)

    // §8: the semantic model which the image encodes, from which it may be written again as
    // text or as BinTEL. Every row is visited once, and at most once.
    def materialise()(using Tactic[Ratel.Error]): Tel.Element.Node =
      val visited: java.util.HashMap[Int, java.util.BitSet] = java.util.HashMap()
      Tel.Element.Node(Unset, schema.document, record(0, 0L, visited, 0))

    private[stratiform] def record
      ( table: Int, row: Long, visited: java.util.HashMap[Int, java.util.BitSet], depth: Int )
      ( using Tactic[Ratel.Error] )
    :   Array[Tel.Element]^{} =

      if depth > 256 then fail(Reason.NestingLimitExceeded)
      val start = address(table, row)
      if row > Int.MaxValue then fail(Reason.Oversized)
      if !visited.containsKey(table) then visited.put(table, java.util.BitSet())
      val seen = visited.get(table)
      if seen.get(row.toInt) then fail(Reason.RowRevisited)
      seen.set(row.toInt)
      members(start, plan(table).entries, visited, depth)

    private[stratiform] def members
      ( address: Long,
        entries: List[Entry],
        visited: java.util.HashMap[Int, java.util.BitSet],
        depth:   Int )
      ( using Tactic[Ratel.Error] )
    :   Array[Tel.Element]^{} =

      val out: java.util.ArrayList[Tel.Element] = java.util.ArrayList()

      def node(keywordIndex: Int, table: Int, struct: Tels.Struct, row: Long): Unit =
        out.add(Tel.Element.Node(keywordIndex, struct, record(table, row, visited, depth + 1)))

      def variant(variants: List[RatelLayout.Variant], tag: Long, payload: Long): Unit =
        val chosen = variants.seek(_.keywordIndex == tag).or(fail(Reason.BadTag))

        chosen.payload match
          case Payload.Record(table, struct) =>
            node(chosen.keywordIndex, table, struct, payload)

          case Payload.Value(scalar, intern) =>
            val value = text(scalar, bytes(intern, payload))
            out.add(Tel.Element.Value(chosen.keywordIndex, scalar, value))

          case Payload.Flag =>
            out.add(Tel.Element.Node(chosen.keywordIndex, Tels.Flag, Array.empty))

      def repeat(count: Long)(lambda: Long => Unit): Unit =
        @scala.annotation.tailrec
        def recur(number: Long): Unit = if number < count then
          lambda(number)
          recur(number + 1)

        recur(0L)

      entries.each:
        case Entry.Flattened(keywordIndex, _, struct, inner) =>
          out.add(Tel.Element.Node(keywordIndex, struct, members(address, inner, visited, depth)))

        case Entry.Stored(slot) =>
          val at = address + slot.offset
          def flag(): Unit = out.add(Tel.Element.Node(slot.keywordIndex, Tels.Flag, Array.empty))

          slot.kind match
            case Kind.Always    => flag()
            case Kind.Flag      => if present(address, slot) then flag()
            case Kind.FlagCount => repeat(word(at))(_ => flag())

            case Kind.Inline(scalar, width) => if present(address, slot) then
              val value = text(scalar, view.read(at, width))
              out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Ref(scalar, intern) => if present(address, slot) then
              val value = text(scalar, bytes(intern, word(at)))
              out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Packed(scalar, width) =>
              val array = item(false, word(at), width)

              repeat(array.length): number =>
                val value = text(scalar, view.read(array.offset + number*width, width))
                out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Refs(scalar, intern) =>
              val array = item(false, word(at), 4L)

              repeat(array.length): number =>
                val value = text(scalar, bytes(intern, word(array.offset + 4*number)))
                out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Record(table, struct) =>
              if present(address, slot) then node(slot.keywordIndex, table, struct, word(at))

            case Kind.Range(table, struct) =>
              val start = word(at)
              val size = word(at + 4)
              if start + size > count(table) then fail(Reason.BadRecordIndex)
              repeat(size) { number => node(slot.keywordIndex, table, struct, start + number) }

            case Kind.Select(variants) =>
              if present(address, slot) then variant(variants, word(at), word(at + 4))

            case Kind.SelectList(variants) =>
              val list = item(false, word(at), 8L)

              repeat(list.length): number =>
                variant(variants, word(list.offset + 8*number), word(list.offset + 8*number + 4))

      Array.tabulate[Tel.Element](out.size)(out.get(_))

    // The indexes at a slot of a table, each with its number in the index directory.
    private[stratiform] def indexesAt(table: Int, slot: Slot): List[(RatelLayout.Index, Int)] =
      layout.indexes.to[List].zip(List.range(0, layout.indexes.length)).filter: (index, number) =>
        index.table == table && index.slot.number == slot.number

    // An entry of an index, as the range of the file it occupies (R23).
    private[stratiform] def entry(index: Int, number: Long)(using Tactic[Ratel.Error]): Extent =
      val extent = indexes.at(index.z).or(fail(Reason.BadIndex))
      val region = extent.offset + 4*(extent.length + 1)
      val start = view.u32(extent.offset + 4*number)
      val end = view.u32(extent.offset + 4*number + 4)
      if start > end || region + end > section(4).end || end - start < 8 then fail(Reason.BadIndex)
      Extent(region + start, end - start)

    private[stratiform] def entries(index: Int): Long = indexes.at(index.z).let(_.length).or(0L)

    // §9.4: the record indices of the children whose entries begin with `probe`, in the order
    // of the index, found by a search for the first entry which does not sort below it.
    private[stratiform] def search(index: Int, probe: Data)(using Tactic[Ratel.Error])
    :   List[Long] =

      val count = entries(index)

      @scala.annotation.tailrec
      def bound(low: Long, high: Long): Long =
        if low >= high then low else
          val middle = (low + high)/2
          val extent = entry(index, middle)
          val length = extent.length.min(probe.length.toLong)

          if view.compare(extent.offset, length, probe) < 0 then bound(middle + 1, high)
          else bound(low, middle)

      @scala.annotation.tailrec
      def matches(number: Long, found: List[Long]): List[Long] =
        if number >= count then found.reverse else
          val extent = entry(index, number)

          if extent.length < probe.length || !view.same(extent.offset, probe) then found.reverse
          else matches(number + 1, view.read(extent.end - 4, 4).u32be(0) :: found)

      matches(bound(0L, count), Nil)

    // The bytes of a value as an index holds them; a text which the scalar's codec does not
    // accept has none, and so is the key of nothing.
    private[stratiform] def key(scalar: Tels.Scalar, text: Text)(using Tactic[Ratel.Error])
    :   Tel.Codec.Encoded =

      val plain: Tel.Codec.Encoded = Tel.Codec.Encoded.Bytes(RatelBytes.utf8(text))

      scalar.encoding.lay(plain): name =>
        codecs(name).or(fail(Reason.CodecUnresolved)).encode(text)

    // Appends a component of an index key, as it is found by following its path from a
    // child's row.
    private[stratiform] def component
      ( out: Scribe[Byte], component: Component, table: Int, row: Long )
      ( using Tactic[Ratel.Error] )
    :   Unit =

      def follow(address: Long, hops: List[Slot]): Unit = hops match
        case Nil =>
          if !present(address, component.last) then RatelBytes.absent(out)
          else RatelBytes.component(out, load(address, component.last), component.width.present)

        case hop :: rest => hop.kind match
          case Kind.Record(table, _) =>
            if !present(address, hop) then RatelBytes.absent(out)
            else follow(this.address(table, word(address + hop.offset)), rest)

          case _ => RatelBytes.absent(out)

      follow(address(table, row), component.hops)

    // §9.5 and §12: a verifying reader's checks, which a lazy reader leaves to each access.
    // The section digests are compared, if the image has them (R07); the whole document is
    // materialised (R14-R18, R20); every index is checked against the rows it indexes
    // (R23-R25); and the value hash, if the image carries it, is recomputed (R27).
    def verify()(using Tactic[Ratel.Error]): Unit =
      val digests = section(5)

      if digests.length != 0L then
        List.range(0, 5).each: number =>
          val extent = section(number)
          if extent.length > Int.MaxValue then fail(Reason.Oversized)
          val digest = Blake3.hashOf(view.read(extent.offset, extent.length.toInt), 32)
          if !view.same(digests.offset + 32*number, digest) then fail(Reason.DigestMismatch)

        val digest = Blake3.hashOf(view.read(0L, RatelBytes.header), 32)
        if !view.same(digests.offset + 160, digest) then fail(Reason.DigestMismatch)

      val document = materialise()

      layout.indexes.to[List].zip(List.range(0, layout.indexes.length)).each: (index, number) =>
        verify(index, number)

      valueHash.let: hash =>
        val recomputed =
          mitigate:
            case _: Tel.Error    => error(Reason.ValueHashMismatch)
            case _: Bintel.Error => error(Reason.ValueHashMismatch)

          . protect(Blake3.hashOf(Bintel.encode(document, schema, codecs), 32))

        if RatelBytes.order(recomputed, hash.data) != 0 then fail(Reason.ValueHashMismatch)

    // Whether the bytes of an entry, between its two record indices, are well-formed
    // components: each a single zero for an absent value, or a one before the value's bytes,
    // which for a value without a fixed width are escaped and terminated.
    private def parses(entry: Data, widths: List[Optional[Int]]): Boolean =
      val end = entry.length - 4

      @scala.annotation.tailrec
      def terminated(index: Int): Int =
        if index + 1 >= end then -1
        else if entry.u8(index) != 0 then terminated(index + 1)
        else if entry.u8(index + 1) == 0xff then terminated(index + 2)
        else if entry.u8(index + 1) == 0 then index + 2
        else -1

      @scala.annotation.tailrec
      def recur(index: Int, widths: List[Optional[Int]]): Boolean = widths match
        case Nil => index == end

        case width :: rest =>
          if index < 4 || index >= end then false
          else if entry.u8(index) == 0 then recur(index + 1, rest)
          else if entry.u8(index) != 1 then false
          else recur(width.lay(terminated(index + 1))(index + 1 + _), rest)

      recur(4, widths)

    private def verify(index: RatelLayout.Index, number: Int)(using Tactic[Ratel.Error]): Unit =
      val count = entries(number)
      val slot = index.slot
      val hosts = this.count(index.table)

      val widths: List[Optional[Int]] =
        if index.child.absent then List(index.elementWidth) else index.components.map(_.width)

      // The children of a host at this site: where they begin, and how many they are.
      def children(host: Long): Extent =
        val at = address(index.table, host) + slot.offset

        slot.kind match
          case Kind.Range(table, _)  => Extent(word(at), word(at + 4))
          case Kind.Packed(_, width) => Extent(0L, item(false, word(at), width).length)
          case _                     => Extent(0L, item(false, word(at), 4L).length)

      def expected(host: Long, child: Long): Data = Array.collect[Byte](32): out =>
        RatelBytes.u32be(out, host)

        slot.kind match
          case Kind.Range(table, _) =>
            index.components.each(this.component(out, _, table, child))

          case Kind.Packed(_, width) =>
            val array = item(false, word(address(index.table, host) + slot.offset), width)
            RatelBytes.component(out, view.read(array.offset + child*width, width), true)

          case Kind.Refs(_, intern) =>
            val array = item(false, word(address(index.table, host) + slot.offset), 4L)
            RatelBytes.component(out, bytes(intern, word(array.offset + 4*child)), false)

          case _ => ()

        RatelBytes.u32be(out, child)

      @scala.annotation.tailrec
      def recur(position: Long, previous: Data): Unit = if position < count then
        val extent = entry(number, position)
        if extent.length > Int.MaxValue then fail(Reason.BadIndex)
        val stored = view.read(extent.offset, extent.length.toInt)
        if !parses(stored, widths) then fail(Reason.BadIndex)
        if position > 0 && RatelBytes.order(previous, stored) >= 0 then fail(Reason.IndexUnsorted)
        val host = stored.u32be(0)
        val child = stored.u32be(stored.length - 4)
        if host >= hosts then fail(Reason.IndexDisagrees)
        val range = children(host)

        if child < range.offset || child >= range.offset + range.length
        then fail(Reason.IndexDisagrees)

        if RatelBytes.order(expected(host, child), stored) != 0 then fail(Reason.IndexDisagrees)
        recur(position + 1, stored)

      recur(0L, Array.empty[Byte])

      @scala.annotation.tailrec
      def total(host: Long, sum: Long): Long =
        if host >= hosts then sum else total(host + 1, sum + children(host).length)

      if total(0L, 0L) != count then fail(Reason.IndexDisagrees)

  // A reader for this module's own use, whose brand nothing outside can name.
  private final class Unbranded(view: Terrain^, opening: Opening) extends Reader(view, opening):
    type Topic = Unit

  // One record, read in place: a row of a table, or a struct whose members are flattened
  // into the row of the struct which holds it. A member is named by its keyword; one which
  // is a variant of a select is present if it is the variant chosen. A row is a plain value,
  // which says where a record lies: it is read through the reader of the image it came from,
  // which the block that opened the image is given, and so can be read nowhere else.
  object Row:
    private[stratiform] def apply[brand]
      ( table: Int, index: Long, entries: List[Entry], struct: Tels.Struct )
    :   Row of brand =

      new Row(table, index, entries, struct) { type Topic = brand }

  abstract class Row private[stratiform]
    ( table:   Int,
      val index: Long,
      entries: List[Entry],
      struct:  Tels.Struct ):

    // The brand of the reader this row came from.
    type Topic

    import Error.Reason

    private def address
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Long =

      reader.address(table, index)

    // What a keyword names in this record: a member with a slot, a flattened struct, or a
    // variant of a select, together with the slot of the select.
    private enum Found:
      case Member(slot: Slot)
      case Inner(entry: Entry.Flattened)
      case Choice(slot: Slot, variant: RatelLayout.Variant)

    private def lookup(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Found =

      val found: List[Found] = entries.bind:
        case entry: Entry.Flattened =>
          if entry.keyword == keyword then List(Found.Inner(entry)) else Nil

        case Entry.Stored(slot) => slot.kind match
          case Kind.Select(variants) =>
            variants.filter(_.keyword == keyword).map(Found.Choice(slot, _))

          case Kind.SelectList(variants) =>
            Nil

          case _ =>
            if slot.keyword == keyword then List(Found.Member(slot)) else Nil

      found.head.or(fail(Reason.Unknown(keyword)))

    // The payload of a select's slot, if `variant` is the one chosen.
    private def chosen(slot: Slot, variant: RatelLayout.Variant)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Optional[Long] =

      val at = address + slot.offset

      if !reader.present(address, slot) || reader.word(at) != variant.keywordIndex then Unset
      else reader.word(at + 4)

    // Whether a member which is not repeatable is present: always, if it is required.
    def has(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Boolean =

      lookup(keyword) match
      case Found.Member(slot)          => reader.present(address, slot)
      case Found.Choice(slot, variant) => chosen(slot, variant).present
      case Found.Inner(entry)          => true

    // The bytes of a scalar member, as they are stored: UTF-8, or the bytes of its codec.
    def data(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Data =

      lookup(keyword) match
      case Found.Member(slot) =>
        if !reader.present(address, slot) then fail(Reason.Absent(keyword))
        else reader.load(address, slot)

      case Found.Choice(slot, variant) => variant.payload match
        case Payload.Value(scalar, intern) =>
          reader.bytes(intern, chosen(slot, variant).or(fail(Reason.Absent(keyword))))

        case _ =>
          fail(Reason.Inapplicable(keyword))

      case Found.Inner(entry) =>
        fail(Reason.Inapplicable(keyword))

    private def scalar(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Tels.Scalar =

      lookup(keyword) match
        case Found.Member(slot) => slot.kind match
          case Kind.Inline(scalar, _) => scalar
          case Kind.Ref(scalar, _)    => scalar
          case Kind.Packed(scalar, _) => scalar
          case Kind.Refs(scalar, _)   => scalar
          case _                      => fail(Reason.Inapplicable(keyword))

        case Found.Choice(slot, variant) => variant.payload match
          case Payload.Value(scalar, _) => scalar
          case _                        => fail(Reason.Inapplicable(keyword))

        case Found.Inner(entry) =>
          fail(Reason.Inapplicable(keyword))

    // The text of a scalar member, if it is present.
    def text(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Optional[Text] =

      if !has(keyword) then Unset else reader.text(scalar(keyword), data(keyword))

    // The texts of a repeatable scalar member, in the order they were written.
    def texts(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[Text] =

      val slot = lookup(keyword) match
        case Found.Member(slot) => slot
        case _                  => fail(Reason.Inapplicable(keyword))

      val at = address + slot.offset

      def collect(count: Long)(lambda: Long => Text): List[Text] =
        @scala.annotation.tailrec
        def recur(number: Long, done: List[Text]): List[Text] =
          if number >= count then done.reverse else recur(number + 1, lambda(number) :: done)

        recur(0L, Nil)

      slot.kind match
        case Kind.Packed(scalar, width) =>
          val array = reader.item(false, reader.word(at), width)

          collect(array.length): number =>
            reader.text(scalar, reader.copy(array.offset + number*width, width))

        case Kind.Refs(scalar, intern) =>
          val array = reader.item(false, reader.word(at), 4L)

          collect(array.length): number =>
            reader.text(scalar, reader.bytes(intern, reader.word(array.offset + 4*number)))

        case _ =>
          fail(Reason.Inapplicable(keyword))

    // Whether a repeatable scalar member has `value` among its values: by its index, if the
    // schema declares one, and by reading each value otherwise.
    def holds(keyword: Text, value: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Boolean =

      val slot = lookup(keyword) match
        case Found.Member(slot) => slot
        case _                  => fail(Reason.Inapplicable(keyword))

      reader.indexesAt(table, slot).head.lay(texts(keyword).has(value)): (index, number) =>
        reader.key(scalar(keyword), value) match
          case Tel.Codec.Encoded.Invalid(message) => false

          case Tel.Codec.Encoded.Bytes(key) =>
            val probe = Array.collect[Byte](key.length + 16): out =>
              RatelBytes.u32be(out, this.index)
              RatelBytes.component(out, key, index.elementWidth.present)

            !reader.search(number, probe).nil

    // How many times a flag occurs: for one which is not repeatable, once or not at all.
    def count(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Long =

      lookup(keyword) match
      case Found.Member(slot) => slot.kind match
        case Kind.Always    => 1L
        case Kind.Flag      => if reader.present(address, slot) then 1L else 0L
        case Kind.FlagCount => reader.word(address + slot.offset)
        case _              => fail(Reason.Inapplicable(keyword))

      case Found.Choice(slot, variant) => variant.payload match
        case Payload.Flag => if chosen(slot, variant).present then 1L else 0L
        case _            => fail(Reason.Inapplicable(keyword))

      case Found.Inner(entry) =>
        fail(Reason.Inapplicable(keyword))

    def flag(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Boolean =

      count(keyword) > 0L

    // A struct member which is not repeatable, if it is present.
    def record(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Optional[Row of Topic] =

      lookup(keyword) match
        case Found.Inner(entry) =>
          Row[Topic](table, index, entry.entries, entry.struct)

        case Found.Member(slot) => slot.kind match
          case Kind.Record(table, struct) =>
            if !reader.present(address, slot) then Unset else
              val row = reader.word(address + slot.offset)
              reader.address(table, row)
              Row[Topic](table, row, reader.plan(table).entries, struct)

          case _ =>
            fail(Reason.Inapplicable(keyword))

        case Found.Choice(slot, variant) => variant.payload match
          case Payload.Record(table, struct) =>
            val row = chosen(slot, variant)

            if row.absent then Unset else
              reader.address(table, row.or(0L))
              Row[Topic](table, row.or(0L), reader.plan(table).entries, struct)

          case _ =>
            fail(Reason.Inapplicable(keyword))

    // The records of a repeatable struct member. Nothing of them is read until one is.
    def / (keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Rows of Topic =

      lookup(keyword) match
      case Found.Member(slot) => slot.kind match
        case Kind.Range(child, struct) =>
          val start = reader.word(address + slot.offset)
          val size = reader.word(address + slot.offset + 4)
          if start + size > reader.count(child) then fail(Reason.BadRecordIndex)
          Rows[Topic](table, index, slot, child, struct, start, size)

        case _ =>
          fail(Reason.Inapplicable(keyword))

      case _ =>
        fail(Reason.Inapplicable(keyword))

    // The semantic model of this record alone, and of whatever it holds.
    def materialise()
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Tel.Element.Node =

      val visited: java.util.HashMap[Int, java.util.BitSet] = java.util.HashMap()
      Tel.Element.Node(Unset, struct, reader.members(address, entries, visited, 0))

    // A member as a `Ratel.Provider` types it (see `RatelShape`): a scalar as its number or
    // its text, a struct as a record, a repeatable struct as a table of records.
    private[stratiform] def member(keyword: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Any =

      def one(scalar: Tels.Scalar, required: Boolean): Any =
        if required then RatelShape.value(scalar, text(keyword).or(fail(Reason.Absent(keyword))))
        else text(keyword).let(RatelShape.value(scalar, _))

      def many(scalar: Tels.Scalar): Any = texts(keyword).map(RatelShape.value(scalar, _))

      def inner(required: Boolean): Any =
        val found = record(keyword).let(Record[Topic](_))
        if required then found.or(fail(Reason.Absent(keyword))) else found

      lookup(keyword) match
        case Found.Inner(_) => inner(true)

        case Found.Member(slot) => slot.kind match
          case Kind.Always            => true
          case Kind.Flag              => flag(keyword)
          case Kind.FlagCount         => count(keyword)
          case Kind.Inline(scalar, _) => one(scalar, slot.required)
          case Kind.Ref(scalar, _)    => one(scalar, slot.required)
          case Kind.Packed(scalar, _) => many(scalar)
          case Kind.Refs(scalar, _)   => many(scalar)
          case Kind.Record(_, _)      => inner(slot.required)
          case Kind.Range(_, _)       => Table[Topic](this / keyword)
          case _                      => fail(Reason.Inapplicable(keyword))

        case Found.Choice(_, variant) => variant.payload match
          case Payload.Flag             => has(keyword)
          case Payload.Value(scalar, _) => one(scalar, false)
          case Payload.Record(_, _)     => inner(false)

    // This record as a value of a type which is read from TEL. Only this record's rows, and
    // those it refers to, are read.
    def as[value: Tel.Decodable]
      ( using reader: Reader^, tactic: Tactic[Ratel.Error], decoding: Tactic[Tel.Error] )
      ( using reader.Topic =:= Topic )
    :   value =

      Bintel.present(materialise(), reader.schema.copy(document = struct)).as[value]

  // The records of a repeatable member of one record: a run of consecutive rows of a table,
  // in the order they were written, with the indexes which the schema declares over them.
  object Rows:
    private[stratiform] def apply[brand]
      ( host: Int, parent: Long, slot: Slot, table: Int, struct: Tels.Struct, start: Long,
        size: Long )
    :   Rows of brand =

      new Rows(host, parent, slot, table, struct, start, size) { type Topic = brand }

  abstract class Rows private[stratiform]
    ( host:   Int,
      parent: Long,
      slot:   Slot,
      table:  Int,
      struct: Tels.Struct,
      start:  Long,
      val size: Long ):

    // The brand of the reader these rows came from.
    type Topic

    import Error.Reason

    private def row(number: Long)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Row of Topic =

      Row[Topic](table, number, reader.plan(table).entries, struct)

    private def child(number: Long)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Row of Topic =

      if number < start || number >= start + size then fail(Reason.BadRecordIndex)
      row(number)

    def apply(ordinal: Ordinal)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Row of Topic =

      if ordinal.n0 >= size then fail(Reason.BadRecordIndex)
      row(start + ordinal.n0)

    def each(lambda: (Row of Topic) => Unit)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Unit =

      @scala.annotation.tailrec
      def recur(number: Long): Unit = if number < size then
        lambda(row(start + number))
        recur(number + 1)

      recur(0L)

    // The prefix of every entry for this parent, and of those among them whose first
    // component is the bytes of a value.
    private def prefix: Data = Array.collect[Byte](4)(RatelBytes.u32be(_, parent))

    private def probe(bytes: Data, fixed: Boolean): Data =
      Array.collect[Byte](bytes.length + 16): out =>
        RatelBytes.u32be(out, parent)
        RatelBytes.component(out, bytes, fixed)

    private def primary
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   (RatelLayout.Index, Int) =

      reader.indexesAt(host, slot).seek(_(0).primary).or(fail(Reason.Unindexed))

    private def lookup(index: RatelLayout.Index, number: Int, value: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[Long] =

      val component = index.components.head.or(fail(Reason.Unindexed))

      reader.key(component.scalar, value) match
        case Tel.Codec.Encoded.Invalid(message) => Nil

        case Tel.Codec.Encoded.Bytes(bytes) =>
          reader.search(number, probe(bytes, component.width.present))

    // The record whose key field has the value `key`, found by a binary search of the
    // primary index (§9.4).
    def find(key: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Optional[Row of Topic] =

      val (index, number) = primary

      lookup(index, number, key) match
        case first :: rest => child(first)
        case Nil           => Unset

    // The records in the order of their keys.
    def sorted
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[Row of Topic] =

      reader.search(primary(1), prefix).map(child(_))

    // The records for which the scalar at `path` has the value `value`, found through the
    // first index declared over the member whose first path it is.
    def where(path: Text, value: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[Row of Topic] =

      def telp(path: Text): Telp = safely(Telp.parse(path)).or(Telp(List(path)))

      val (index, number) =
        reader.indexesAt(host, slot).seek: (index, number) =>
          !index.primary && index.paths.head.let(telp(_) == telp(path)).or(false)

        . or(fail(Reason.Unindexed))

      lookup(index, number, value).map(child(_))

  object Record:
    private[stratiform] def apply[brand](row0: Row of brand): Record of brand =
      new Record:
        type Topic = brand
        def row: Row of brand = row0

  // One record, as a `Ratel.Provider` types it: each member of the struct is a member of the
  // record, of the type its schema gives it, and is read from the image when it is selected.
  // It is a plain value, as the row it stands for is, and is read through the reader it came
  // from, whose brand it carries.
  abstract class Record private[stratiform]() extends Selectable:
    // The brand of the reader this record came from.
    type Topic

    // The row this record reads, for whatever a provider does not type.
    def row: Row of Topic

    def selectDynamic(name: String)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Any =

      row.member(scala.reflect.NameTransformer.decode(name).nn.tt)

  object Table:
    private[stratiform] def apply[brand](rows0: Rows of brand): Table[Any] of brand =
      new Table[Any]:
        type Topic = brand
        def rows: Rows of brand = rows0

  // The records of a repeatable struct, as a `Ratel.Provider` types them: `Ratel.Rows`, giving
  // records where it gives rows.
  abstract class Table[+entry] private[stratiform]():
    // The brand of the reader these records came from.
    type Topic

    // The rows these records read.
    def rows: Rows of Topic

    // Sound because a provider gives `entry` as the type of the records of this table's struct,
    // every one of which is a `Record` and nothing more at runtime.
    private def entry(row: Row of Topic): entry = Record[Topic](row).asInstanceOf[entry]

    def size: Long = rows.size

    def apply(ordinal: Ordinal)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   entry =

      entry(rows(ordinal))

    // The record whose key is `key`, by the primary index.
    def apply(key: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Optional[entry] =

      rows.find(key).let(entry(_))

    def each(lambda: entry => Unit)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   Unit =

      rows.each { row => lambda(entry(row)) }

    // The records in the order of their keys.
    def sorted
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[entry] =

      rows.sorted.map(entry(_))

    // The records for which the scalar at `path` has the value `value`, by a secondary index.
    def where(path: Text, value: Text)
      ( using reader: Reader^, tactic: Tactic[Ratel.Error] )
      ( using reader.Topic =:= Topic )
    :   List[entry] =

      rows.where(path, value).map(entry(_))

  object Provider:
    // The root of an open image as a record, once the image is known to have been written
    // with the provider's schema: the macro behind `Provider#apply` gives the result its type.
    def root(provider: Provider)(using reader: Reader^, tactic: Tactic[Ratel.Error])
    :   Record of reader.Topic =

      val expected = provider.signature.or(fail(Error.Reason.Unencodable))
      if RatelBytes.order(expected, reader.signature) != 0 then fail(Error.Reason.Mismatched)
      Record[reader.Topic](reader.root)

  // A type provider over a RaTEL schema. An object which extends it gives the root of an open
  // image as a record whose members are those of the schema's document, typed as the schema
  // types them, each read in place when it is selected:
  //
  //     object Folio extends Ratel.Provider(schema)
  //
  //     file.open[Ratel]():
  //       val folio = Folio()
  //       folio.name                    // Text
  //       folio.role(t"Hamlet")         // an optional record, by its key
  //
  // A scalar reads as `Text`, or as `Int`, `Long` or `Double` where its encoding is a standard
  // codec whose every value one of them holds; a flag as `Boolean`, or as a `Long` count if it
  // is repeatable; a struct as a nested record; a repeatable scalar as a `List`; a repeatable
  // struct as a `Ratel.Table` of records; and each variant of a select as an optional member.
  // An optional member is `Optional`. The object must be compiled before it is used, as any
  // object a macro consults must.
  abstract class Provider(val schema: Tels):
    // The signature an image written with this schema carries.
    private[stratiform] lazy val signature: Optional[Data] =
      given Diagnostics = Diagnostics.omit
      val axiom = Tels.Axiom.tels
      safely(SchemaSignature.fromElement(Tels.Renderer.element(schema, axiom), axiom))

    // What the members of the schema's document read as: asked for by the macro, when a
    // record is given its type, of a schema which must compose and be laid out.
    private[stratiform] def shape(using Tactic[Ratel.Error]): List[(Text, RatelShape.Shape)] =
      given Diagnostics = Diagnostics.omit
      val composed = safely(Tels.Layers.compose(schema)).or(fail(Error.Reason.SchemaUnlayoutable))
      RatelShape(RatelLayout(composed))

    transparent inline def apply()
      ( using inline reader: Reader^, tactic: Tactic[Ratel.Error] )
    :   Record =

      ${stratiform.ratelInternal.ratelRecord[this.type]('reader, 'tactic)}

  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case BadMagic              => m"the file does not begin with the RaTEL magic number"
        case UnsupportedVersion    => m"the version of the format is not one this reader supports"
        case ReservedSet           => m"a reserved byte or flag of the header is set"
        case BadLength             => m"the committed length is not one the file can have"
        case BadDirectory          => m"a section is misaligned or lies beyond the committed length"
        case SectionsOverlap       => m"two sections overlap"
        case DigestMismatch        => m"a section does not match its digest"
        case BadSignature          => m"the schema signature is not structurally one"
        case SchemaUndecodable     => m"the embedded schema does not decode"
        case SignatureMismatch     => m"the embedded schema does not have the signature it carries"
        case SchemaUnlayoutable    => m"the schema cannot be laid out"
        case BadTableDirectory     => m"the table directory disagrees with the schema's layout"
        case BadRootTable          => m"the root table does not have exactly one row"
        case BadRef                => m"a ref does not denote an item within its region"
        case BadRecordIndex        => m"a record index lies beyond the end of its table"
        case RowRevisited          => m"a row is reached twice"
        case BadTag                => m"a select's tag is not the keyword index of a variant"
        case BadUtf8               => m"a text value is not valid UTF-8"
        case CodecUnresolved       => m"an encoding which the schema names is not bound to a codec"
        case CodecDecodeFailed     => m"a codec rejects the bytes of a value"
        case CodecNoncanonical     => m"the bytes of a value are not those its text encodes to"
        case WidthMismatch         => m"a value does not have the width its scalar declares"
        case BadIndex              => m"an index is malformed"
        case IndexUnsorted         => m"the entries of an index are not in increasing order"
        case IndexDisagrees        => m"an index disagrees with the rows it indexes"
        case NonzeroPadding        => m"padding, or the slot of an absent member, is not zero"
        case ValueHashMismatch     => m"the document does not have the value hash in its header"
        case NestingLimitExceeded  => m"the document is nested more deeply than a reader may follow"
        case Unencodable           => m"the document is not one its schema describes"
        case Oversized             => m"the image is larger than can be held in memory"
        case Unindexed             => m"the schema declares no index which can answer the lookup"
        case Unknown(keyword)      => m"the record has no member $keyword"
        case Absent(keyword)       => m"the member $keyword is absent"
        case Unwritable            => m"an image is read-only and is replaced by publishing another"
        case Mismatched            => m"the image was not written with the schema it is read by"
        case Inapplicable(keyword) => m"the member $keyword is not of the kind which was asked for"

    // The conditions of RaTEL §14, each numbered as its R-code is, followed by those which
    // are outside that taxonomy: the limits of this implementation, and misuse of a reader.
    enum Reason(val number: Int) extends Clarification:
      case BadMagic             extends Reason(1)
      case UnsupportedVersion   extends Reason(2)
      case ReservedSet          extends Reason(3)
      case BadLength            extends Reason(4)
      case BadDirectory         extends Reason(5)
      case SectionsOverlap      extends Reason(6)
      case DigestMismatch       extends Reason(7)
      case BadSignature         extends Reason(8)
      case SchemaUndecodable    extends Reason(9)
      case SignatureMismatch    extends Reason(10)
      case SchemaUnlayoutable   extends Reason(11)
      case BadTableDirectory    extends Reason(12)
      case BadRootTable         extends Reason(13)
      case BadRef               extends Reason(14)
      case BadRecordIndex       extends Reason(15)
      case RowRevisited         extends Reason(16)
      case BadTag               extends Reason(17)
      case BadUtf8              extends Reason(18)
      case CodecUnresolved      extends Reason(19)
      case CodecDecodeFailed    extends Reason(20)
      case CodecNoncanonical    extends Reason(21)
      case WidthMismatch        extends Reason(22)
      case BadIndex             extends Reason(23)
      case IndexUnsorted        extends Reason(24)
      case IndexDisagrees       extends Reason(25)
      case NonzeroPadding       extends Reason(26)
      case ValueHashMismatch    extends Reason(27)
      case NestingLimitExceeded extends Reason(28)
      case Unencodable          extends Reason(29)
      case Oversized            extends Reason(30)
      case Unindexed            extends Reason(31)
      case Unknown(keyword: Text)      extends Reason(32)
      case Inapplicable(keyword: Text) extends Reason(33)
      case Absent(keyword: Text)       extends Reason(34)
      case Unwritable                  extends Reason(35)
      case Mismatched                  extends Reason(36)

  case class Error(reason: Ratel.Error.Reason)(using Diagnostics)
  extends fulminate.Error(615, reason.number)(m"the RaTEL image cannot be used because $reason")
