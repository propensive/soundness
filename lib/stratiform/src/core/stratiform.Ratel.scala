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

import scala.language.dynamics
import scala.language.unsafeNulls

import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gastronomy.*
import gossamer.*
import polyvinyl.*
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
  // The type whose value the image is of, if that is known: `Ratel of Cast` is an image
  // written under the schema derived from `Cast`, which is what a value's `ratel` gives. An
  // owner which claims a name as `Ratel of Cast` publishes nothing else under it, and an
  // observer which opens it as `Ratel of Cast` reads nothing else from it.
  type Topic

  // A view of the image where it lies on the heap, for reading it without a file.
  def terrain: Terrain = Terrain(data)

object Ratel:
  // An image of a value of the type `topic`, under the schema derived from it.
  private[stratiform] def of[topic](data: Data): Ratel of topic =
    new Ratel(data) { type Topic = topic }

  // An image is published as its bytes, whatever it is an image of.
  given encodable: [ratel <: Ratel] => ratel is Encodable in Data = _.data

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
  given viewable: [ratel <: Ratel] => ratel is Viewable:
    def view[result](value: ratel)(lambda: Terrain^ => result): result = lambda(value.terrain)

  // Anything `Viewable` -- a path, an image, a view -- is opened as `Ratel` for reading in
  // place: `file.open[Ratel]()`. The reader is given to the block, and to nothing beyond it.
  given openable: [source]
  =>  ( viewable: (source is Viewable)^,
        tactic:   Tactic[Ratel.Error] )
  =>  ( Ratel.Openable[source]^{viewable, tactic} ) =
    Ratel.Openable[source]()

  // Anything `Viewable` is opened as `Ratel of topic` likewise, where `topic` is the type the
  // image is expected to be of: an image written under any schema but the one derived from
  // `topic` is refused, as `Mismatched`, before the block runs.
  given typedOpenable: [source, topic]
  =>  ( viewable:  (source is Viewable)^,
        schematic: topic is TelSchematic over Tels.Type,
        tactic:    Tactic[Ratel.Error] )
  =>  ( Ratel.TypedOpenable[source, topic]^{viewable, tactic} ) =
    Ratel.TypedOpenable[source, topic]()

  // An image is read in place from a view of its bytes, whoever lends the view: this is what
  // an observer of a published name opens it through.
  given surveyable: (tactic: Tactic[Ratel.Error])
  =>  ( (Ratel is Surveyable by Ratel.Flag to Reader)^{tactic} ) =
    new Surveyable:
      type Self = Ratel
      type Operand = Ratel.Flag
      type Result = Reader

      def survey[result](view: Terrain^, flags: List[Ratel.Flag])(block: (Reader^) ?=> result)
      :   result =

        reading(view, Read, flags, Array.empty[Byte])(block)

  given typedSurveyable: [topic]
  =>  ( schematic: topic is TelSchematic over Tels.Type, tactic: Tactic[Ratel.Error] )
  =>  ( ((Ratel of topic) is Surveyable by Ratel.Flag to Reader)^{tactic} ) =
    new Surveyable:
      type Self = Ratel of topic
      type Operand = Ratel.Flag
      type Result = Reader

      def survey[result](view: Terrain^, flags: List[Ratel.Flag])(block: (Reader^) ?=> result)
      :   result =

        reading(view, Read, flags, signatureOf[topic])(block)

  // The signature of the schema derived from a type, as an image of a value of the type
  // carries it. A type which has no such schema has no images to be mistaken for.
  private[stratiform] def signatureOf[topic]
    ( using schematic: topic is TelSchematic over Tels.Type, tactic: Tactic[Ratel.Error] )
  :   Data =

    mitigate:
      case _: Tel.Error           => error(Error.Reason.Mismatched)
      case _: Bintel.Error        => error(Error.Reason.Mismatched)
      case _: Tels.Renderer.Error => error(Error.Reason.Mismatched)

    . protect:
        val axiom = Tels.Axiom.tels
        SchemaSignature.fromElement(Tels.Renderer.element(Tels.tels[topic](t"root"), axiom), axiom)

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

      viewable.view(value)(reading(_, mode, flags, Array.empty[Byte])(block))

  class TypedOpenable[source, topic]
    ( using viewable:  (source is Viewable)^,
            schematic: topic is TelSchematic over Tels.Type,
            tactic:    Tactic[Ratel.Error] )
  extends aperture.Openable:
    type Self = source
    type Form = Ratel of topic
    type Operand = Ratel.Flag
    type Result = Reader

    def open[grants <: Grant, result]
      ( value: source, mode: Mode granting grants, flags: List[Ratel.Flag] )
      ( block: ((Reader & Granting[grants])^) ?=> result )
    :   result =

      val expected = signatureOf[topic]
      viewable.view(value)(reading(_, mode, flags, expected)(block))

  // `expected` is the signature the image must carry, or nothing if any will do.
  private def reading[grants <: Grant, result]
    ( view: Terrain^, mode: Mode granting grants, flags: List[Ratel.Flag], expected: Data )
    ( block: ((Reader & Granting[grants])^) ?=> result )
    ( using tactic: Tactic[Ratel.Error] )
  :   result =

    // An image is never changed where it lies: a new one is published over it.
    if mode.atoms.has(Write) then fail(Error.Reason.Unwritable)

    val codecs = flags.fuse(Tel.Codec.Bindings.all):
      next match
        case Flag.Codecs(bindings) => bindings.orElse(state)
        case Flag.Verify           => state

    val terrain: Terrain^{view} = view

    val opening = inspect(view, codecs)

    if expected.length > 0 && RatelBytes.order(expected, opening.signature) != 0
    then fail(Error.Reason.Mismatched)

    val reader = new Reader(opening) with Granting[grants]:
      type Topic = Unit
      protected def view: Terrain^{this} = terrain

    if flags.has(Flag.Verify) then reader.verify()
    block(using reader)

  // What `open` learns of an image before anything is read from it: its schema and layout,
  // its state, and where its sections, tables and indexes lie.
  private[stratiform] case class Opening
    ( schema:    Tels,
      signature: Data,
      valueHash: Optional[Digest in Blake3],
      layout:    Layout,
      codecs:    Tel.Codec.Bindings,
      state:     State,
      digested:  Boolean,
      sections:  Array[Extent]^{},
      tables:    Array[Extent]^{},
      indexes:   Array[Tree]^{} )

  private[stratiform] def open
    ( view: Terrain^, codecs: Tel.Codec.Bindings = Tel.Codec.Bindings.all )
    ( using Tactic[Ratel.Error] )
  :   Reader^{view} =

    val opening = inspect(view, codecs)
    new Unbranded(view, opening)

  // §7.1: everything which decides how the rest of an image is read -- the header, the schema
  // and its signature, the directories of tables and indexes -- is checked here, in the order
  // the specification gives, and nothing else is read until it is asked for.
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
    if header.u8(5) != 0 || (flags & ~15) != 0 then fail(Reason.ReservedSet)
    val digested = (flags & RatelBytes.digested) != 0
    val hashed = (flags & RatelBytes.hashed) != 0
    val sealing = (flags & RatelBytes.sealing) != 0
    val dirty = (flags & RatelBytes.dirty) != 0

    // §5.8: digests and a value hash belong to a sealed file alone, and a sealed file is
    // never one which is being changed.
    if (digested || hashed) && !sealing || sealing && dirty then fail(Reason.InvalidState)
    if dirty then fail(Reason.Dirty)
    val committed = header.u64(8)

    if committed < RatelBytes.header || committed%8 != 0 || committed > view.size
    then fail(Reason.BadLength)

    // A section, as the directory describes it: `length` of its `capacity` is in use. The
    // strings and the variable region grow by their length; the others are of a fixed size.
    def span(number: Int): Span =
      val offset = header.u64(16 + 24*number)
      val length = header.u64(24 + 24*number)
      val capacity = header.u64(32 + 24*number)
      val region = number == 1 || number == 2

      val aligned = offset%8 == 0 && capacity%8 == 0 && offset >= RatelBytes.header
      val within = length >= 0 && length <= capacity && offset <= committed
      val fits = capacity <= committed - offset
      val sized = if region then length%4 == 0 else length == capacity
      if !aligned || !within || !fits || !sized then fail(Reason.BadDirectory)

      Span(offset, length, capacity)

    val schemaSpan = span(0)
    val stringsSpan = span(1)
    val variableSpan = span(2)
    val tablesSpan = span(3)
    val indexesSpan = span(4)
    val digestsSpan = span(5)

    val spans: List[Span] =
      List(schemaSpan, stringsSpan, variableSpan, tablesSpan, indexesSpan, digestsSpan)

    // The ranges of the file which are spoken for, the header first: no two may overlap.
    val occupied: java.util.ArrayList[Extent] = java.util.ArrayList()
    occupied.add(Extent(0L, RatelBytes.header))
    spans.each { span => if span.capacity > 0 then occupied.add(Extent(span.offset, span.capacity)) }

    def disjoint(): Unit =
      occupied.sort(Extent.ordering)

      @scala.annotation.tailrec
      def recur(number: Int): Unit = if number < occupied.size then
        if occupied.get(number).offset < occupied.get(number - 1).end
        then fail(Reason.ExtentsOverlap)

        recur(number + 1)

      recur(1)

    disjoint()

    // §5.7: the digests are sixty-four bytes at the very end of a file which has them.
    val digestsPlaced = digestsSpan.length == RatelBytes.digests && digestsSpan.end == committed
    if digested && !digestsPlaced then fail(Reason.DigestMismatch)
    if !digested && digestsSpan.capacity != 0 then fail(Reason.DigestMismatch)
    if schemaSpan.length > Int.MaxValue then fail(Reason.SchemaUndecodable)

    // The schema section is a signature and a schema body, each behind its length (§5.2).
    val section: Data = view.read(schemaSpan.offset, schemaSpan.length.toInt)

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

    // §5.5: an entry of the table directory for every table of the layout, and no more.
    if tablesSpan.length != 32L*layout.tables.length then fail(Reason.BadTableDirectory)

    val tables: List[Extent] =
      layout.tables.to[List].zip(List.range(0, layout.tables.length)).map: (table, number) =>
        val entry = tablesSpan.offset + 32L*number
        val offset = view.u64(entry)
        val count = view.u32(entry + 8)
        val capacity = view.u32(entry + 12)
        val freeHead = view.u32(entry + 20)

        // The layout byte and the seven reserved bytes after it are one zero word.
        val described = view.u32(entry + 16) == table.rowSize && view.u64(entry + 24) == 0L
        val counted = count <= capacity && count != RatelBytes.none
        val freed = freeHead == RatelBytes.none || freeHead < count
        val within = offset%8 == 0 && offset >= 0 && offset <= committed
        val fits = capacity*table.rowSize <= committed - offset
        if !described || !counted || !freed || !within || !fits then fail(Reason.BadTableDirectory)
        if sealing && (capacity != count || freeHead != RatelBytes.none) then fail(Reason.InvalidState)
        if capacity > 0 then occupied.add(Extent(offset, capacity*table.rowSize))

        if number == 0 then
          val unique = count == 1L && freeHead == RatelBytes.none
          if !unique || (view.u16(offset + 6) & RatelBytes.live) == 0 then fail(Reason.BadRootTable)

        Extent(offset, count)

    // §5.6: an entry of the index directory for every index the schema determines.
    if indexesSpan.length != 32L*layout.indexes.length then fail(Reason.BadIndex)

    val indexes: List[Tree] = List.range(0, layout.indexes.length).map: number =>
      val entry = indexesSpan.offset + 32L*number
      val offset = view.u64(entry)
      val pages = view.u32(entry + 8)
      val capacity = view.u32(entry + 12)
      val root = view.u32(entry + 16)
      val freeHead = view.u32(entry + 20)
      val count = view.u32(entry + 24)
      val shift = view.u8(entry + 29)
      val height = view.u8(entry + 30)

      val described = view.u8(entry + 28) == 0 && view.u8(entry + 31) == 0 && shift >= 9 && shift <= 16
      val counted = pages <= capacity && pages != RatelBytes.none
      val rooted = root == RatelBytes.none || root < pages
      val freed = freeHead == RatelBytes.none || freeHead < pages
      val levelled = if root == RatelBytes.none then height == 0 && count == 0L else height > 0
      val within = offset%8 == 0 && offset >= 0 && offset <= committed
      val fits = (capacity << shift) <= committed - offset

      if !described || !counted || !rooted || !freed || !levelled || height > pages || !within || !fits
      then fail(Reason.BadIndex)

      if sealing && (capacity != pages || freeHead != RatelBytes.none) then fail(Reason.InvalidState)
      if capacity > 0 then occupied.add(Extent(offset, capacity << shift))
      Tree(offset, pages, root, freeHead, count, shift, height)

    disjoint()

    // §5.8: a sealed file is compact. Nothing in it is garbage, no section has room to
    // spare, and its extents lie end to end from the header to the committed length.
    if sealing then
      if header.u64(160) != 0L then fail(Reason.InvalidState)
      spans.each { span => if span.capacity != (span.length + 7)/8*8 then fail(Reason.InvalidState) }

      @scala.annotation.tailrec
      def tiled(number: Int): Unit = if number < occupied.size then
        if occupied.get(number).offset != occupied.get(number - 1).end then fail(Reason.InvalidState)
        tiled(number + 1)

      tiled(1)
      if occupied.get(occupied.size - 1).end != committed then fail(Reason.InvalidState)

    schema.scalars.each: scalar =>
      scalar.encoding.let: name =>
        if codecs(name).absent then fail(Reason.CodecUnresolved)

    val hash: Optional[Digest in Blake3] =
      if hashed then Digest[Blake3](header.segment(168.z till 200.z)) else Unset

    Opening
      ( schema, signature, hash, layout, codecs, if sealing then State.Sealed else State.Unsealed,
        digested, spans.map(span => Extent(span.offset, span.length)).to[Array], tables.to[Array],
        indexes.to[Array] )

  private[stratiform] def error(reason: Error.Reason): Error =
    import errorDiagnostics.emptyDiagnostics
    Error(reason)

  private[stratiform] def fail(reason: Error.Reason)(using Tactic[Ratel.Error]): Nothing =
    abort(error(reason))

  // The state a file is read in (§5.8): the compact, unchanging result of sealing, which may
  // be shared, or a working file which its owner has closed. A file which is being changed
  // is dirty, and is not read at all.
  enum State:
    case Sealed, Unsealed

  // A range of a file: a section, or a table's rows. For a table, `length` counts its rows;
  // for a region, it is the part of it which holds items.
  private[stratiform] case class Extent(offset: Long, length: Long):
    def end: Long = offset + length

  private[stratiform] object Extent:
    val ordering: java.util.Comparator[Extent] = new java.util.Comparator[Extent]:
      def compare(left: Extent | Null, right: Extent | Null): Int =
        java.lang.Long.compare(left.nn.offset, right.nn.offset)

  // A section as the header's directory describes it.
  private[stratiform] case class Span(offset: Long, length: Long, capacity: Long):
    def end: Long = offset + length

  // An index as its directory entry describes it (§5.6): a B+tree of `pages` pages of
  // `1 << shift` bytes each, `height` levels deep, holding `count` entries.
  private[stratiform] case class Tree
    ( offset: Long, pages: Long, root: Long, freeHead: Long, count: Long, shift: Int, height: Int ):

    def pageSize: Long = 1L << shift

  // A page of an index, once its header is known to be well-formed (§9.4): `kind` is one for
  // a leaf and two for a branch, `link` the next leaf or the leftmost child, and `start`
  // where its cells begin.
  private[stratiform] case class Page
    ( number: Long, offset: Long, size: Long, kind: Int, count: Int, link: Long, start: Long )

  // An open image. It serves reads for as long as the view it was opened on is mapped, and
  // may be shared between threads.
  @missingContext
    ( "a row or a record of a RaTEL image is read through the reader of that image, which is "+
      "not in scope here: read it within the `open[Ratel]` block it came from" )
  abstract class Reader private[stratiform](opening: Opening):
    // The view the image is read from. It is a member which a reader of this module defines,
    // and not a parameter, since a parameter which is tracked would be part of the type of
    // every reader, and so of every member of a record which is read through one.
    protected def view: Terrain^{this}

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

    // The state the image was opened in: sealed, or a working file its owner has closed.
    def state: State = opening.state

    private def section(number: Int): Extent = sections.at(number.z).or(Extent(0L, 0L))

    private[stratiform] def plan(table: Int)(using Tactic[Ratel.Error]): RatelLayout.Table =
      layout.table(table).or(fail(Reason.BadRecordIndex))

    // The number of rows in a table, live or free.
    private[stratiform] def count(table: Int): Long =
      tables.at(table.z).let(_.length).or(0L)

    // Where a row lies, if its record index is within its table.
    private def position(table: Int, row: Long)(using Tactic[Ratel.Error]): Long =
      val extent = tables.at(table.z).or(fail(Reason.BadRecordIndex))
      if row < 0 || row >= extent.length then fail(Reason.BadRecordIndex)
      extent.offset + row*plan(table).rowSize

    // §7.2: the address of a row, once its record index is known to be within its table, and
    // the row to be live (↯/t.5.15).
    private[stratiform] def address(table: Int, row: Long)(using Tactic[Ratel.Error]): Long =
      val start = position(table, row)
      if (view.u16(start + 6) & RatelBytes.live) == 0 then fail(Reason.BadRecordIndex)
      start

    // The presence bitmap follows the eight bytes of the row's header.
    private[stratiform] def present(address: Long, slot: Slot): Boolean =
      val bit = slot.presence
      bit < 0 || (view.u8(address + 8 + (bit >> 3)) & 1 << (bit & 7)) != 0

    private[stratiform] def word(address: Long): Long = view.u32(address)

    // The body of a value item of a region: a ref is a multiple of four, and the item it
    // denotes lies wholly within the part of its region which is in use (↯/t.5.14). An entry of
    // an index which is spilled is such an item, and one which does not resolve is ↯/t.5.23.
    private[stratiform] def item(strings: Boolean, ref: Long, reason: Reason = Reason.BadRef)
      ( using Tactic[Ratel.Error] )
    :   Extent =

      val region = section(if strings then 1 else 2)
      if ref%4 != 0 || ref > region.length - 4 then fail(reason)
      val length = view.u32(region.offset + ref)
      if length > region.length - ref - 4 then fail(reason)
      Extent(region.offset + ref + 4, length)

    // The elements of a growable item of the variable region, each of `unit` bytes, as where
    // they begin and how many of them are in use; the empty ref denotes none (↯/t.5.14).
    private[stratiform] def list(ref: Long, unit: Long)(using Tactic[Ratel.Error]): Extent =
      if ref == RatelBytes.none then Extent(0L, 0L) else
        val region = section(2)
        if ref%4 != 0 || ref > region.length - 8 then fail(Reason.BadRef)
        val capacity = view.u32(region.offset + ref)
        val count = view.u32(region.offset + ref + 4)
        if count > capacity || capacity*unit > region.length - ref - 8 then fail(Reason.BadRef)
        Extent(region.offset + ref + 8, count)

    private[stratiform] def bytes(strings: Boolean, ref: Long)(using Tactic[Ratel.Error]): Data =
      val extent = item(strings, ref)
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
      Tel.Element.Node(Unset, schema.document, document(Walk(false)))

    private def document(walk: Walk)(using Tactic[Ratel.Error]): Array[Tel.Element]^{} =
      record(0, 0L, walk, 0, RatelBytes.none, RatelBytes.unlinked)

    // A row which is reached through the slot with link number `link` of the row `parent`. A
    // verifying reader checks that the row's header says as much (↯/t.5.30).
    private[stratiform] def record
      ( table: Int, row: Long, walk: Walk, depth: Int, parent: Long, link: Int )
      ( using Tactic[Ratel.Error] )
    :   Array[Tel.Element]^{} =

      if depth > 256 then fail(Reason.NestingLimitExceeded)
      val start = address(table, row)
      if row > Int.MaxValue then fail(Reason.Oversized)
      if walk.seen(table, row.toInt) then fail(Reason.RowRevisited)

      if walk.strict then
        val named = view.u32(start) == parent && view.u16(start + 4) == link
        if !named || view.u16(start + 6) != RatelBytes.live then fail(Reason.BadRowHeader)

      members(start, row, plan(table).entries, walk, depth)

    private[stratiform] def members
      ( address: Long, host: Long, entries: List[Entry], walk: Walk, depth: Int )
      ( using Tactic[Ratel.Error] )
    :   Array[Tel.Element]^{} =

      val out: java.util.ArrayList[Tel.Element] = java.util.ArrayList()

      def node(keywordIndex: Int, table: Int, struct: Tels.Struct, row: Long, slot: Slot): Unit =
        val children = record(table, row, walk, depth + 1, host, slot.link)
        out.add(Tel.Element.Node(keywordIndex, struct, children))

      def variant(variants: List[RatelLayout.Variant], tag: Long, payload: Long, slot: Slot): Unit =
        val chosen = variants.seek(_.keywordIndex == tag).or(fail(Reason.BadTag))

        chosen.payload match
          case Payload.Record(table, struct) =>
            node(chosen.keywordIndex, table, struct, payload, slot)

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

      // The elements of a collection slot. A verifying reader of a sealed file checks that
      // the item is exactly as large as what it holds, and that an empty one is the empty
      // ref (↯/t.5.28).
      def elements(at: Long, unit: Long): Extent =
        val ref = word(at)
        val found = list(ref, unit)

        if walk.strict && state == State.Sealed && ref != RatelBytes.none then
          val capacity = view.u32(section(2).offset + ref)
          if found.length == 0L || capacity != found.length then fail(Reason.InvalidState)

        found

      entries.each:
        case Entry.Flattened(keywordIndex, _, struct, inner) =>
          val children = members(address, host, inner, walk, depth)
          out.add(Tel.Element.Node(keywordIndex, struct, children))

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
              val array = elements(at, width)

              repeat(array.length): number =>
                val value = text(scalar, view.read(array.offset + number*width, width))
                out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Refs(scalar, intern) =>
              val array = elements(at, 4L)

              repeat(array.length): number =>
                val value = text(scalar, bytes(intern, word(array.offset + 4*number)))
                out.add(Tel.Element.Value(slot.keywordIndex, scalar, value))

            case Kind.Record(table, struct) =>
              if present(address, slot) then node(slot.keywordIndex, table, struct, word(at), slot)

            case Kind.Children(table, struct) =>
              val children = elements(at, 4L)

              repeat(children.length): number =>
                node(slot.keywordIndex, table, struct, word(children.offset + 4*number), slot)

            case Kind.Select(variants) =>
              if present(address, slot) then variant(variants, word(at), word(at + 4), slot)

            case Kind.SelectList(variants) =>
              val pairs = elements(at, 8L)

              repeat(pairs.length): number =>
                val pair = pairs.offset + 8*number
                variant(variants, word(pair), word(pair + 4), slot)

      Array.tabulate[Tel.Element](out.size)(out.get(_))

    // The indexes at a slot of a table, each with its number in the index directory.
    private[stratiform] def indexesAt(table: Int, slot: Slot): List[(RatelLayout.Index, Int)] =
      layout.indexes.to[List].zip(List.range(0, layout.indexes.length)).filter: (index, number) =>
        index.table == table && index.slot.number == slot.number

    private def tree(index: Int)(using Tactic[Ratel.Error]): Tree =
      indexes.at(index.z).or(fail(Reason.BadIndex))

    // §9.4: a page of an index, whose header is checked before anything in it is read
    // (↯/t.5.23).
    private[stratiform] def page(index: Int, number: Long)(using Tactic[Ratel.Error]): Page =
      val tree = this.tree(index)
      if number < 0 || number >= tree.pages then fail(Reason.BadIndex)
      val size = tree.pageSize
      val offset = tree.offset + number*size
      val kind = view.u8(offset)
      val count = view.u16(offset + 2)
      val start = view.u32(offset + 4)
      val reserved = view.u8(offset + 1) != 0 || view.u32(offset + 12) != 0L
      if kind > 2 || reserved || start < 16 + 2*count || start > size then fail(Reason.BadIndex)
      Page(number, offset, size, kind, count, view.u32(offset + 8), start)

    // Where a cell of a page begins, within the page: a multiple of four, in the cell area.
    private def pointer(page: Page, number: Int)(using Tactic[Ratel.Error]): Long =
      val pointer = view.u16(page.offset + 16 + 2*number).toLong
      if pointer%4 != 0 || pointer < page.start || pointer > page.size - 4 then fail(Reason.BadIndex)
      pointer

    // The child which a cell of a branch leads to.
    private[stratiform] def child(page: Page, number: Int)(using Tactic[Ratel.Error]): Long =
      view.u32(page.offset + pointer(page, number))

    // The entry of a cell of a leaf, or the separator of a cell of a branch, as the range of
    // the file it occupies: within the page, or, if it is longer than an eighth of a page,
    // an item of the variable region (↯/t.5.23).
    private[stratiform] def entry(page: Page, number: Int)(using Tactic[Ratel.Error]): Extent =
      val pointer = this.pointer(page, number) + (if page.kind == 2 then 4 else 0)
      if pointer > page.size - 4 then fail(Reason.BadIndex)
      val at = page.offset + pointer
      val length = view.u16(at)

      if length == 0xffff then
        if pointer > page.size - 8 || view.u16(at + 2) != 0 then fail(Reason.BadIndex)
        val spilled = item(false, view.u32(at + 4), Reason.BadIndex)
        if spilled.length <= page.size/8 then fail(Reason.BadIndex)
        spilled
      else
        val fits = length >= 1 && length <= page.size/8 && pointer + 2 + length <= page.size
        if !fits then fail(Reason.BadIndex)
        Extent(at + 2, length)

    private[stratiform] def entries(index: Int): Long = indexes.at(index.z).let(_.count).or(0L)

    // §9.5: the record indices of the children whose entries begin with `probe`, in the order
    // of the index: a descent from the root to the leaf where the first of them would be, and
    // a walk along the leaves from there.
    private[stratiform] def search(index: Int, probe: Data)(using Tactic[Ratel.Error])
    :   List[Long] =

      val tree = this.tree(index)

      // The first cell of a page which does not sort below the probe, or, in a branch, the
      // number of separators which do not sort above it.
      @scala.annotation.tailrec
      def bound(page: Page, low: Int, high: Int, inclusive: Boolean): Int =
        if low >= high then low else
          val middle = (low + high)/2
          val extent = entry(page, middle)
          val order = view.compare(extent.offset, extent.length, probe)
          if order < 0 || inclusive && order == 0 then bound(page, middle + 1, high, inclusive)
          else bound(page, low, middle, inclusive)

      @scala.annotation.tailrec
      def descend(number: Long, level: Int): Page =
        val next = page(index, number)

        if level == tree.height - 1 then
          if next.kind != 1 then fail(Reason.BadTree)
          next
        else
          if next.kind != 2 then fail(Reason.BadTree)
          val separators = bound(next, 0, next.count, true)
          val below = if separators == 0 then next.link else child(next, separators - 1)
          if below >= tree.pages then fail(Reason.BadIndex)
          descend(below, level + 1)

      // A walk which has visited more leaves than the index has pages is going round a cycle.
      @scala.annotation.tailrec
      def walk(leaf: Page, cell: Int, visited: Long, found: List[Long]): List[Long] =
        if cell < leaf.count then
          val extent = entry(leaf, cell)
          if extent.length < 8 then fail(Reason.BadIndex)

          if extent.length < probe.length || !view.same(extent.offset, probe) then found.reverse
          else walk(leaf, cell + 1, visited, view.read(extent.end - 4, 4).u32be(0) :: found)
        else if leaf.link == RatelBytes.none then found.reverse
        else if visited >= tree.pages then fail(Reason.BadTree)
        else
          val next = page(index, leaf.link)
          if next.kind != 1 then fail(Reason.BadTree)
          walk(next, 0, visited + 1, found)

      if tree.root == RatelBytes.none then Nil else
        val leaf = descend(tree.root, 0)
        walk(leaf, bound(leaf, 0, leaf.count, false), 1L, Nil)

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

    // §7.2: a verifying reader's checks, which a lazy reader leaves to each access. The
    // digests are compared, if the image has them (↯/t.5.7); the whole document is
    // materialised (↯/t.5.14-↯/t.5.18, ↯/t.5.20), each row's header being checked against the slot
    // which reaches it, and every row being accounted for (↯/t.5.30); every index is checked
    // as a tree and against the rows it indexes (↯/t.5.23-↯/t.5.25); and the value hash, if the
    // image carries it, is recomputed (↯/t.5.27).
    def verify()(using Tactic[Ratel.Error]): Unit =
      val digests = section(5)

      if opening.digested then
        if digests.offset - RatelBytes.header > Int.MaxValue then fail(Reason.Oversized)
        val body = view.read(RatelBytes.header.toLong, (digests.offset - RatelBytes.header).toInt)
        if !view.same(digests.offset, Blake3.hashOf(body, 32)) then fail(Reason.DigestMismatch)
        val header = Blake3.hashOf(view.read(0L, RatelBytes.header), 32)
        if !view.same(digests.offset + 32, header) then fail(Reason.DigestMismatch)

      val walk = Walk(true)
      val children = document(walk)
      val document0 = Tel.Element.Node(Unset, schema.document, children)

      // Every row is live and was reached, or is free: and a sealed file has no free rows.
      List.range(0, layout.tables.length).each: table =>
        @scala.annotation.tailrec
        def recur(row: Long): Unit = if row < count(table) then
          val start = position(table, row)
          val flags = view.u16(start + 6)
          if (flags & ~RatelBytes.live) != 0 then fail(Reason.BadRowHeader)

          if flags == RatelBytes.live then
            if !walk.reached(table, row.toInt) then fail(Reason.BadRowHeader)
          else
            if state == State.Sealed then fail(Reason.InvalidState)
            if view.u16(start + 4) != RatelBytes.unlinked then fail(Reason.BadRowHeader)

          recur(row + 1)

        recur(0L)

      layout.indexes.to[List].zip(List.range(0, layout.indexes.length)).each: (index, number) =>
        verify(index, number)

      valueHash.let: hash =>
        val recomputed =
          mitigate:
            case _: Tel.Error    => error(Reason.ValueHashMismatch)
            case _: Bintel.Error => error(Reason.ValueHashMismatch)

          . protect(Blake3.hashOf(Bintel.encode(document0, schema, codecs), 32))

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

    private def stored(extent: Extent)(using Tactic[Ratel.Error]): Data =
      if extent.length > Int.MaxValue then fail(Reason.BadIndex)
      view.read(extent.offset, extent.length.toInt)

    // §9.6: an index is a well-formed tree (↯/t.5.23) which keeps the invariants of §9.4
    // (↯/t.5.24), and each of its entries agrees with the rows (↯/t.5.25).
    private def verify(index: RatelLayout.Index, number: Int)(using Tactic[Ratel.Error]): Unit =
      val tree = this.tree(number)
      val slot = index.slot
      val hosts = this.count(index.table)
      val reached: java.util.BitSet = java.util.BitSet()
      val leaves: java.util.ArrayList[Page] = java.util.ArrayList()

      val widths: List[Optional[Int]] =
        if index.child.absent then List(index.elementWidth) else index.components.map(_.width)

      def live(host: Long): Boolean =
        host < hosts && (view.u16(position(index.table, host) + 6) & RatelBytes.live) != 0

      // The children of a host at this site: where their list begins, and how many they are.
      def children(host: Long): Extent =
        val at = address(index.table, host) + slot.offset

        slot.kind match
          case Kind.Children(_, _)   => list(word(at), 4L)
          case Kind.Packed(_, width) => list(word(at), width)
          case _                     => list(word(at), 4L)

      // Whether a child is one of a host's: for an array, a position within it, and for a
      // collection of records, a row which its child list names. The rows of one host are
      // kept, since the entries of an index come a host at a time.
      val listed: java.util.HashSet[Long] = java.util.HashSet()
      var listing: Long = -1L

      def among(host: Long, child: Long): Boolean = slot.kind match
        case Kind.Children(_, _) =>
          if listing != host then
            listed.clear()
            val rows = children(host)

            @scala.annotation.tailrec
            def recur(position: Long): Unit = if position < rows.length then
              listed.add(word(rows.offset + 4*position))
              recur(position + 1)

            recur(0L)
            listing = host

          listed.contains(child)

        case _ =>
          child < children(host).length

      def expected(host: Long, child: Long): Data = Array.collect[Byte](32): out =>
        RatelBytes.u32be(out, host)

        slot.kind match
          case Kind.Children(table, _) =>
            index.components.each(this.component(out, _, table, child))

          case Kind.Packed(_, width) =>
            val array = children(host)
            RatelBytes.component(out, view.read(array.offset + child*width, width), true)

          case Kind.Refs(_, intern) =>
            val array = children(host)
            RatelBytes.component(out, bytes(intern, word(array.offset + 4*child)), false)

          case _ => ()

        RatelBytes.u32be(out, child)

      // The cells of a page lie within its cell area, each padded to a multiple of four, and
      // no two of them overlap.
      def cells(page: Page): Unit =
        val areas: java.util.ArrayList[Extent] = java.util.ArrayList()

        List.range(0, page.count).each: cell =>
          val start = pointer(page, cell)
          val head = if page.kind == 2 then 4 else 0
          val length = view.u16(page.offset + start + head)
          val size = head + (if length == 0xffff then 8 else 2 + length)
          val padded = (size + 3)/4*4
          if start + padded > page.size then fail(Reason.BadIndex)
          areas.add(Extent(start, padded))

        areas.sort(Extent.ordering)

        @scala.annotation.tailrec
        def recur(cell: Int): Unit = if cell < areas.size then
          if areas.get(cell).offset < areas.get(cell - 1).end then fail(Reason.BadIndex)
          recur(cell + 1)

        recur(1)

      var previous: Optional[Data] = Unset
      var total: Long = 0L

      // A leaf's entries each parse and agree with the rows, and follow the entry before, in
      // this leaf or the one before it; `low` and `high` are the separators which bound them.
      def leaf(page: Page, low: Optional[Data], high: Optional[Data]): Unit =
        leaves.add(page)

        List.range(0, page.count).each: cell =>
          val entry = stored(this.entry(page, cell))
          if entry.length < 8 || !parses(entry, widths) then fail(Reason.BadIndex)
          previous.let { before => if RatelBytes.order(before, entry) >= 0 then fail(Reason.BadTree) }
          low.let { bound => if RatelBytes.order(entry, bound) < 0 then fail(Reason.BadTree) }
          high.let { bound => if RatelBytes.order(entry, bound) >= 0 then fail(Reason.BadTree) }
          val host = entry.u32be(0)
          val child = entry.u32be(entry.length - 4)
          if !live(host) || !among(host, child) then fail(Reason.IndexDisagrees)
          if RatelBytes.order(expected(host, child), entry) != 0 then fail(Reason.IndexDisagrees)
          previous = entry
          total += 1L

      // Every page beneath the root is reached once, branches above the last level and
      // leaves at it.
      def descend(below: Long, level: Int, low: Optional[Data], high: Optional[Data]): Unit =
        if below >= tree.pages then fail(Reason.BadIndex)
        if reached.get(below.toInt) then fail(Reason.BadTree)
        reached.set(below.toInt)
        val page = this.page(number, below)
        cells(page)

        if level == tree.height - 1 then
          if page.kind != 1 then fail(Reason.BadTree)
          leaf(page, low, high)
        else
          if page.kind != 2 then fail(Reason.BadTree)
          val separators: java.util.ArrayList[Data] = java.util.ArrayList()
          List.range(0, page.count).each { cell => separators.add(stored(entry(page, cell))) }

          def separator(cell: Int): Optional[Data] =
            if cell < page.count then separators.get(cell) else high

          List.range(0, page.count).each: cell =>
            val bound = separator(cell).or(Array.empty[Byte])

            if cell > 0 then separator(cell - 1).let: before =>
              if RatelBytes.order(before, bound) >= 0 then fail(Reason.BadTree)

            low.let { least => if RatelBytes.order(bound, least) < 0 then fail(Reason.BadTree) }

          descend(page.link, level + 1, low, separator(0))

          List.range(0, page.count).each: cell =>
            descend(child(page, cell), level + 1, separator(cell), separator(cell + 1))

      if tree.root != RatelBytes.none then descend(tree.root, 0, Unset, Unset)

      // The leaves are chained in the order they were reached, and the last ends the chain.
      List.range(0, leaves.size).each: position =>
        val next = if position + 1 < leaves.size then leaves.get(position + 1).number else RatelBytes.none
        if leaves.get(position).link != next then fail(Reason.BadTree)

      if total != tree.count then fail(Reason.BadTree)

      // Whatever is not in the tree is on the list of free pages, which a sealed file has
      // none of.
      @scala.annotation.tailrec
      def free(spare: Long, steps: Long): Unit = if spare != RatelBytes.none then
        if spare >= tree.pages || steps > tree.pages then fail(Reason.BadIndex)
        if reached.get(spare.toInt) then fail(Reason.BadTree)
        reached.set(spare.toInt)
        val page = this.page(number, spare)
        if page.kind != 0 then fail(Reason.BadIndex)
        free(page.link, steps + 1)

      free(tree.freeHead, 0L)
      if reached.cardinality != tree.pages then fail(Reason.BadTree)

      // An index has an entry for every child of every host.
      @scala.annotation.tailrec
      def expectation(host: Long, sum: Long): Long =
        if host >= hosts then sum
        else expectation(host + 1, if live(host) then sum + children(host).length else sum)

      if expectation(0L, 0L) != tree.count then fail(Reason.IndexDisagrees)

  // One walk over the rows of an image, which reaches each at most once (↯/t.5.16). A strict
  // walk is a verifying reader's, which also checks what a lazy reader takes on trust.
  private[stratiform] final class Walk(val strict: Boolean):
    private val visited: java.util.HashMap[Int, java.util.BitSet] = java.util.HashMap()

    // Whether a row had been reached before now, when it is.
    def seen(table: Int, row: Int): Boolean =
      if !visited.containsKey(table) then visited.put(table, java.util.BitSet())
      val rows = visited.get(table)
      val before = rows.get(row)
      rows.set(row)
      before

    def reached(table: Int, row: Int): Boolean =
      visited.containsKey(table) && visited.get(table).get(row)

  // A reader for this module's own use, whose brand nothing outside can name.
  private final class Unbranded(terrain: Terrain^, opening: Opening) extends Reader(opening):
    type Topic = Unit
    protected def view: Terrain^{this} = terrain

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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Long =

      reader.address(table, index)

    // What a keyword names in this record: a member with a slot, a flattened struct, or a
    // variant of a select, together with the slot of the select.
    private enum Found:
      case Member(slot: Slot)
      case Inner(entry: Entry.Flattened)
      case Choice(slot: Slot, variant: RatelLayout.Variant)

    private def lookup(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Optional[Long] =

      val at = address + slot.offset

      if !reader.present(address, slot) || reader.word(at) != variant.keywordIndex then Unset
      else reader.word(at + 4)

    // Whether a member which is not repeatable is present: always, if it is required.
    def has(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Boolean =

      lookup(keyword) match
      case Found.Member(slot)          => reader.present(address, slot)
      case Found.Choice(slot, variant) => chosen(slot, variant).present
      case Found.Inner(entry)          => true

    // The bytes of a scalar member, as they are stored: UTF-8, or the bytes of its codec.
    def data(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Optional[Text] =

      if !has(keyword) then Unset else reader.text(scalar(keyword), data(keyword))

    // The texts of a repeatable scalar member, in the order they were written.
    def texts(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
          val array = reader.list(reader.word(at), width)

          collect(array.length): number =>
            reader.text(scalar, reader.copy(array.offset + number*width, width))

        case Kind.Refs(scalar, intern) =>
          val array = reader.list(reader.word(at), 4L)

          collect(array.length): number =>
            reader.text(scalar, reader.bytes(intern, reader.word(array.offset + 4*number)))

        case _ =>
          fail(Reason.Inapplicable(keyword))

    // Whether a repeatable scalar member has `value` among its values: by its index, if the
    // schema declares one, and by reading each value otherwise.
    def holds(keyword: Text, value: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Boolean =

      count(keyword) > 0L

    // A struct member which is not repeatable, if it is present.
    def record(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
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
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Rows of Topic =

      lookup(keyword) match
      case Found.Member(slot) => slot.kind match
        case Kind.Children(child, struct) =>
          val ref = reader.word(address + slot.offset)
          Rows[Topic](table, index, slot, child, struct, ref, reader.list(ref, 4L).length)

        case _ =>
          fail(Reason.Inapplicable(keyword))

      case _ =>
        fail(Reason.Inapplicable(keyword))

    // The semantic model of this record alone, and of whatever it holds.
    def materialise()
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Tel.Element.Node =

      Tel.Element.Node(Unset, struct, reader.members(address, index, entries, Walk(false), 0))

    // What a member holds, for a `Ratel.Provider`'s record: a struct as its row, a scalar as
    // its text, a flag as the number of times it occurs, and a member which is not there as
    // `Absent`.
    private[stratiform] def cell(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Cell =

      def value: Cell = text(keyword).lay(Cell.Absent)(Cell.Value(_))
      def inner: Cell = record(keyword).lay(Cell.Absent)(Cell.Struct(_))

      lookup(keyword) match
        case Found.Inner(_) => inner

        case Found.Member(slot) => slot.kind match
          case Kind.Always           => Cell.Flag(1L)
          case Kind.Flag             => Cell.Flag(count(keyword))
          case Kind.FlagCount        => Cell.Flag(count(keyword))
          case Kind.Inline(_, _)     => value
          case Kind.Ref(_, _)        => value
          case Kind.Record(_, _)     => inner
          case _                     => fail(Reason.Inapplicable(keyword))

        case Found.Choice(_, variant) => variant.payload match
          case Payload.Flag         => Cell.Flag(if has(keyword) then 1L else 0L)
          case Payload.Value(_, _)  => value
          case Payload.Record(_, _) => inner

    // The values of a repeatable scalar member, likewise.
    private[stratiform] def cells(keyword: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   List[Cell] =

      texts(keyword).map(Cell.Value(_))

    // This record as a value of a type which is read from TEL. Only this record's rows, and
    // those it refers to, are read.
    def as[value: Tel.Decodable]
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error], decoding: Tactic[Tel.Error] )
    :   value =

      Bintel.present(materialise(), reader.schema.copy(document = struct)).as[value]

  // The records of a repeatable member of one record: a run of consecutive rows of a table,
  // in the order they were written, with the indexes which the schema declares over them.
  object Rows:
    private[stratiform] def telp(path: Text): Telp =
      safely(Telp.parse(path)).or(Telp(List(path)))

    private[stratiform] def apply[brand]
      ( host: Int, parent: Long, slot: Slot, table: Int, struct: Tels.Struct, ref: Long,
        size: Long )
    :   Rows of brand =

      new Rows(host, parent, slot, table, struct, ref, size) { type Topic = brand }

  abstract class Rows private[stratiform]
    ( host:   Int,
      parent: Long,
      slot:   Slot,
      table:  Int,
      struct: Tels.Struct,
      ref:    Long,
      val size: Long ):

    // The brand of the reader these rows came from.
    type Topic

    import Error.Reason

    private def row(number: Long)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Row of Topic =

      Row[Topic](table, number, reader.plan(table).entries, struct)

    // The row which the child list names at a position: the list is read where it lies, each
    // time, so nothing is kept of where the file was mapped.
    private def listed(position: Long)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Row of Topic =

      val children = reader.list(ref, 4L)
      if position >= children.length then fail(Reason.BadRecordIndex)
      row(reader.word(children.offset + 4*position))

    // A child which an index names, by its record index: a live row of the children's table.
    private def child(number: Long)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Row of Topic =

      reader.address(table, number)
      row(number)

    // The record at a position, counted from the first.
    def apply(ordinal: Ordinal)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Row of Topic =

      if ordinal.n0 >= size then fail(Reason.BadRecordIndex)
      listed(ordinal.n0)

    // Each record, in the order the document has them.
    def each(lambda: (Row of Topic) => Unit)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Unit =

      @scala.annotation.tailrec
      def recur(number: Long): Unit = if number < size then
        lambda(listed(number))
        recur(number + 1)

      recur(0L)

    private def prefix: Data = Array.collect[Byte](4)(RatelBytes.u32be(_, parent))

    private def probe(bytes: Data, fixed: Boolean): Data =
      Array.collect[Byte](bytes.length + 16): out =>
        RatelBytes.u32be(out, parent)
        RatelBytes.component(out, bytes, fixed)

    private def primary
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   (RatelLayout.Index, Int) =

      reader.indexesAt(host, slot).seek(_(0).primary).or(fail(Reason.Unindexed))

    private def lookup(index: RatelLayout.Index, number: Int, value: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   List[Long] =

      val component = index.components.head.or(fail(Reason.Unindexed))

      reader.key(component.scalar, value) match
        case Tel.Codec.Encoded.Invalid(message) => Nil

        case Tel.Codec.Encoded.Bytes(bytes) =>
          reader.search(number, probe(bytes, component.width.present))

    // The record whose key field has the value `key`, found by a binary search of the
    // primary index (§9.4).
    def find(key: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   Optional[Row of Topic] =

      val (index, number) = primary

      lookup(index, number, key) match
        case first :: rest => child(first)
        case Nil           => Unset

    // The records in the order of their keys.
    def sorted
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   List[Row of Topic] =

      reader.search(primary(1), prefix).map(child(_))

    // The records for which the scalar at `path` has the value `value`, found through the
    // first index declared over the member whose first path it is.
    def where(path: Text, value: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   List[Row of Topic] =

      where(Rows.telp(path), value)

    private[stratiform] def where(path: Telp, value: Text)
      ( using reader: (Reader of Topic)^, tactic: Tactic[Ratel.Error] )
    :   List[Row of Topic] =

      val (index, number) =
        reader.indexesAt(host, slot).seek: (index, number) =>
          !index.primary && index.paths.head.let(Rows.telp(_) == path).or(false)

        . or(fail(Reason.Unindexed))

      lookup(index, number, value).map(child(_))

  // What a record of a `Ratel.Provider` is made over, and what its members are read from: a
  // struct's row, the text of a scalar, the number of times a flag occurs, or nothing.
  enum Cell:
    case Struct(row: Row)
    case Value(text: Text)
    case Flag(count: Long)
    case Absent

  // The records of a repeatable struct, as a `Ratel.Provider` types them: `Ratel.Rows`, giving
  // records where it gives rows. `context` is the reader a record was made with, by its
  // brand, which every read asks for.
  final class Table[context <: Reader, +element] private[stratiform]
    ( rows: Rows, read: (Cell, context^, Tactic[Ratel.Error]) -> element ):

    def size: Long = rows.size

    private def row(ordinal: Ordinal)(using access: context^, tactic: Tactic[Ratel.Error])
    :   Row =

      val reader: Reader^{access} = access
      rows(ordinal)(using Provider.bound(reader, rows), tactic)

    def apply(ordinal: Ordinal)(using access: context^, tactic: Tactic[Ratel.Error]): element =
      read(Cell.Struct(row(ordinal)), access, tactic)

    // The record whose key is `key`, by the primary index.
    def apply(key: Text)(using access: context^, tactic: Tactic[Ratel.Error])
    :   Optional[element] =

      val reader: Reader^{access} = access

      rows.find(key)(using Provider.bound(reader, rows), tactic)
      . let { row => read(Cell.Struct(row), access, tactic) }

    // By position, not with `rows.each`: a lambda which reads would hold the reader while
    // `each` was given it too.
    def each(lambda: element => Unit)(using access: context^, tactic: Tactic[Ratel.Error])
    :   Unit =

      @scala.annotation.tailrec
      def recur(number: Int): Unit = if number < rows.size then
        lambda(read(Cell.Struct(row(number.z)), access, tactic))
        recur(number + 1)

      recur(0)

    // The records in the order of their keys.
    def sorted(using access: context^, tactic: Tactic[Ratel.Error]): List[element] =
      val reader: Reader^{access} = access

      rows.sorted(using Provider.bound(reader, rows), tactic)
      . map { row => read(Cell.Struct(row), access, tactic) }

    // The records for which a scalar has a value, by a secondary index. The scalar is named as
    // a lens names what it updates, by a path from a record which ends in an assignment:
    //
    //     cast.role.where(_.play = t"Hamlet")
    //
    // The path is checked against the type of the record, and the value against the type of
    // the member it ends at.
    def where(query: Path[element] => Query)(using access: context^, tactic: Tactic[Ratel.Error])
    :   List[element] =

      val reader: Reader^{access} = access
      val sought = query(Path(Nil))

      rows.where(sought.path, sought.value)(using Provider.bound(reader, rows), tactic)

      . map { row => read(Cell.Struct(row), access, tactic) }

  // A path from a record of a `Ratel.Provider` to one of its members, written as a selection
  // of each member on the way, and ended by the assignment of the value which is sought.
  // `keywords` are those of the members passed, from the last to the first.
  final class Path[+record](val keywords: List[Text]) extends Dynamic:
    transparent inline def selectDynamic(inline name: String): Path[Any] =
      ${stratiform.ratelInternal.descend[record]('this, 'name)}

    inline def updateDynamic[value](inline name: String)(value: value): Query =
      ${stratiform.ratelInternal.query[record, value]('this, 'name, 'value)}

  // The scalar at a path, and the value it is to have, as text.
  final class Query private[stratiform](val path: Telp, val value: Text)

  object Query:
    // `keywords` are the path's, from its end to the record.
    def apply(keywords: List[Text], value: Text): Query = new Query(Telp(keywords.reverse), value)

  object Provider:
    // A record is read through a reader which has its brand, as the type of each of its
    // members says. A cell does not say which reader its row came from, so here, where a
    // member is read, that is taken on trust from the member's type.
    // The cast asserts the brand alone: the reader is tracked afterwards as it was before.
    private def branded[brand](reader: Reader^): (Reader of brand)^{reader} =
      reader.asInstanceOf[(Reader of brand)^{reader}]

    private[stratiform] def bound(reader: Reader^, row: Row): (Reader of row.Topic)^{reader} =
      branded[row.Topic](reader)

    private[stratiform] def bound(reader: Reader^, rows: Rows): (Reader of rows.Topic)^{reader} =
      branded[rows.Topic](reader)

    // The root of an open image, once the image is known to have been written with the
    // provider's schema.
    def root(provider: Provider)(using reader: Reader^, tactic: Tactic[Ratel.Error]): Cell =
      val expected = provider.signature.or(fail(Error.Reason.Unencodable))
      if RatelBytes.order(expected, reader.signature) != 0 then fail(Error.Reason.Mismatched)
      Cell.Struct(reader.root)

    private def text(cell: Cell): Text = cell match
      case Cell.Value(text) => text
      case _                => t""

    private def whole(cell: Cell): Int = Integer.parseInt(text(cell).s)
    private def large(cell: Cell): Long = java.lang.Long.parseLong(text(cell).s)

    // A scalar is found by the name of its encoding, if it has one, and of its first validator
    // if not. One whose encoding is a standard codec every value of which a number holds
    // exactly reads as that number, and any other as the text its codec gives.
    given string: ("string" is Intensional in Ratel.Provider from Cell to Text) =
      Intensional(text(_))

    given identifier: ("identifier" is Intensional in Ratel.Provider from Cell to Text) =
      Intensional(text(_))

    given typeName: ("type-name" is Intensional in Ratel.Provider from Cell to Text) =
      Intensional(text(_))

    given u8: ("u8" is Intensional in Ratel.Provider from Cell to Int) = Intensional(whole(_))
    given u16: ("u16" is Intensional in Ratel.Provider from Cell to Int) = Intensional(whole(_))
    given i8: ("i8" is Intensional in Ratel.Provider from Cell to Int) = Intensional(whole(_))
    given i16: ("i16" is Intensional in Ratel.Provider from Cell to Int) = Intensional(whole(_))
    given i32: ("i32" is Intensional in Ratel.Provider from Cell to Int) = Intensional(whole(_))
    given u32: ("u32" is Intensional in Ratel.Provider from Cell to Long) = Intensional(large(_))
    given i64: ("i64" is Intensional in Ratel.Provider from Cell to Long) = Intensional(large(_))
    given u64: ("u64" is Intensional in Ratel.Provider from Cell to Text) = Intensional(text(_))
    given uuid: ("uuid" is Intensional in Ratel.Provider from Cell to Text) = Intensional(text(_))

    given f64: ("f64" is Intensional in Ratel.Provider from Cell to Double) =
      Intensional { cell => java.lang.Double.parseDouble(text(cell).s) }

    given blake3: ("blake3-256" is Intensional in Ratel.Provider from Cell to Text) =
      Intensional(text(_))

    // A flag is whether it occurs, and a repeatable flag how often.
    given flag: ("flag" is Intensional in Ratel.Provider from Cell to Boolean) =
      Intensional:
        case Cell.Flag(count) => count > 0L
        case _                => false

    given count: ("count" is Intensional in Ratel.Provider from Cell to Long) =
      Intensional:
        case Cell.Flag(count) => count
        case _                => 0L

    // A struct which contains itself is read as its cell, since a type cannot go on for ever.
    given cell: ("cell" is Intensional in Ratel.Provider from Cell to Cell) = Intensional(identity)

  // A type provider over a RaTEL schema: a polyvinyl `Specification`, as `Tel.Provider` is,
  // whose records are read in the context of the reader of an open image. An object which
  // extends it gives the root of the image as a record whose members are those of the
  // schema's document, each typed as the schema types it and read in place when selected:
  //
  //     object Folio extends Ratel.Provider(schema)
  //
  //     file.open[Ratel]():
  //       val folio = Folio()
  //       folio.name                    // Text
  //       folio.role(t"Hamlet")         // an optional record, by its key
  //
  // A scalar reads through the `Intensional` instance found by the name of its encoding, or
  // of its first validator: those of the companion, or one of a caller's own. A flag is a
  // `Boolean`, or a `Long` count if it is repeatable; a struct is a nested record; a
  // repeatable scalar a `List`; a repeatable struct a `Ratel.Table`; and each variant of a
  // select an optional member. An optional member is `Optional`. The object must be compiled
  // before it is used, as any object a macro consults must.
  abstract class Provider(val schema: Tels) extends Specification:
    type Origin = Cell
    type Form = Ratel.Provider
    type Failure = Ratel.Error
    type Context = Reader
    type Series[context <: Reader, +element] = Table[context, element]

    // The signature an image written with this schema carries.
    private[stratiform] lazy val signature: Optional[Data] =
      given Diagnostics = Diagnostics.omit
      val axiom = Tels.Axiom.tels
      safely(SchemaSignature.fromElement(Tels.Renderer.element(schema, axiom), axiom))

    def fields(using Tactic[Specification.Error]): List[(Text, Specification.Member)] =
      given Diagnostics = Diagnostics.omit

      safely(RatelShape(RatelLayout(Tels.Layers.compose(schema)))).or:
        abort(Specification.Error(m"the schema cannot be laid out as RaTEL"))

    // Nothing is read from a cell alone: a record of this provider is made by `apply`, which
    // reads its members in the context of the reader.
    def access(name: Text, cell: Cell)(using Tactic[Ratel.Error]): Cell =
      panic(m"a RaTEL record is read in the context of its reader")

    def repeated(name: Text, cell: Cell)(using Tactic[Ratel.Error]): List[Cell] =
      panic(m"a RaTEL record is read in the context of its reader")

    def absent(cell: Cell): Boolean = cell == Cell.Absent

    override def required(name: Text, cell: Cell)(using Tactic[Ratel.Error]): Cell =
      if cell == Cell.Absent then fail(Error.Reason.Absent(name)) else cell

    override def accessWith(name: Text, cell: Cell)
      ( using access: Reader^, tactic: Tactic[Ratel.Error] )
    :   Cell =

      cell match
        case Cell.Struct(row) =>
          val reader: Reader^{access} = access
          row.cell(name)(using Provider.bound(reader, row), tactic)

        case _ =>
          fail(Error.Reason.Inapplicable(name))

    override def repeatedWith(name: Text, cell: Cell)
      ( using access: Reader^, tactic: Tactic[Ratel.Error] )
    :   List[Cell] =

      cell match
        case Cell.Struct(row) =>
          val reader: Reader^{access} = access
          row.cells(name)(using Provider.bound(reader, row), tactic)

        case _ =>
          fail(Error.Reason.Inapplicable(name))

    override def series[context <: Reader, element]
      ( name: Text, cell: Cell, read: (Cell, context^, Tactic[Ratel.Error]) -> element )
      ( using access: context^, tactic: Tactic[Ratel.Error] )
    :   Table[context, element] =

      cell match
        case Cell.Struct(row) =>
          val reader: Reader^{access} = access
          val rows = (row / name)(using Provider.bound(reader, row), tactic)

          new Table[context, element](rows, read)

        case _ =>
          fail(Error.Reason.Inapplicable(name))

    transparent inline def apply()
      ( using inline reader: Reader^, tactic: Tactic[Ratel.Error] )
    :   polyvinyl.Record =

      ${stratiform.ratelInternal.record[this.type]('{Provider.this}, 'reader, 'tactic)}

  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case BadMagic              => m"the file does not begin with the RaTEL magic number"
        case UnsupportedVersion    => m"the version of the format is not one this reader supports"
        case ReservedSet           => m"a reserved byte or flag of the header is set"
        case BadLength             => m"the committed length is not one the file can have"
        case BadDirectory          => m"a section is misaligned or lies beyond the committed length"
        case ExtentsOverlap        => m"two extents of the file overlap"
        case DigestMismatch        => m"the file does not match its digests"
        case BadSignature          => m"the schema signature is not structurally one"
        case SchemaUndecodable     => m"the embedded schema does not decode"
        case SignatureMismatch     => m"the embedded schema does not have the signature it carries"
        case SchemaUnlayoutable    => m"the schema cannot be laid out"
        case BadTableDirectory     => m"the table directory disagrees with the schema's layout"
        case BadRootTable          => m"the root table does not have exactly one row"
        case BadRef                => m"a ref does not denote an item within its region"
        case BadRecordIndex        => m"a record index does not name a live row of its table"
        case RowRevisited          => m"a row is reached twice"
        case BadTag                => m"a select's tag is not the keyword index of a variant"
        case BadUtf8               => m"a text value is not valid UTF-8"
        case CodecUnresolved       => m"an encoding which the schema names is not bound to a codec"
        case CodecDecodeFailed     => m"a codec rejects the bytes of a value"
        case CodecNoncanonical     => m"the bytes of a value are not those its text encodes to"
        case WidthMismatch         => m"a value does not have the width its scalar declares"
        case BadIndex              => m"an index, or a page or an entry of one, is malformed"
        case BadTree               => m"the pages of an index do not form an ordered tree"
        case IndexDisagrees        => m"an index disagrees with the rows it indexes"
        case NonzeroPadding        => m"padding, or the slot of an absent member, is not zero"
        case ValueHashMismatch     => m"the document does not have the value hash in its header"
        case InvalidState          => m"the file is in no valid state, or is sealed and not compact"
        case Dirty                 => m"the file is being changed, or was not closed by its owner"
        case BadRowHeader          => m"the header of a row disagrees with the slot which reaches it"
        case NestingLimitExceeded  => m"the document is nested more deeply than a reader may follow"
        case Unencodable           => m"the document is not one its schema describes"
        case Oversized             => m"the image is larger than can be held in memory"
        case Unindexed             => m"the schema declares no index which can answer the lookup"
        case Unknown(keyword)      => m"the record has no member $keyword"
        case Absent(keyword)       => m"the member $keyword is absent"
        case Unwritable            => m"an image is read-only and is replaced by publishing another"
        case Mismatched            => m"the image was not written with the schema it is read by"
        case Inapplicable(keyword) => m"the member $keyword is not of the kind which was asked for"

    // The conditions of RaTEL §14, each numbered as its ↯/t.5 code is, followed by those which
    // are outside that taxonomy: the limits of this implementation, and misuse of a reader.
    enum Reason(val number: Int) extends Clarification:
      case BadMagic             extends Reason(1)
      case UnsupportedVersion   extends Reason(2)
      case ReservedSet          extends Reason(3)
      case BadLength            extends Reason(4)
      case BadDirectory         extends Reason(5)
      case ExtentsOverlap       extends Reason(6)
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
      case BadTree              extends Reason(24)
      case IndexDisagrees       extends Reason(25)
      case NonzeroPadding       extends Reason(26)
      case ValueHashMismatch    extends Reason(27)
      case InvalidState         extends Reason(28)
      case Dirty                extends Reason(29)
      case BadRowHeader         extends Reason(30)
      case NestingLimitExceeded extends Reason(31)
      case Unencodable          extends Reason(32)
      case Oversized            extends Reason(33)
      case Unindexed            extends Reason(34)
      case Unknown(keyword: Text)      extends Reason(35)
      case Inapplicable(keyword: Text) extends Reason(36)
      case Absent(keyword: Text)       extends Reason(37)
      case Unwritable                  extends Reason(38)
      case Mismatched                  extends Reason(39)

  case class Error(reason: Ratel.Error.Reason)(using Diagnostics)
  extends fulminate.Error(118, 11, reason.number)
    ( m"the RaTEL image cannot be used because $reason" ):
    // A reason the RaTEL Specification numbers is the specification's own error, ↯/t.5, which
    // this one carries; a reason which is this implementation's own is a variant.
    override def bolt: Bolt =
      if reason.number > 30 then super.bolt
      else Bolt(Bolt.Soundness, module, number).carrying(Bolt('t', 5, reason.number))
