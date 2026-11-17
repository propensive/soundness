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

import java.util.concurrent.atomic as juca

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import gossamer.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

import Ratel.{Extent, Tree}
import RatelLayout.{Entry, Kind, Payload, Slot}

private[stratiform] object RatelEditor:
  // §9.4: a cell of a page, as an owner holds it while it alters the page: the child a branch's
  // cell leads to, the entry or separator, and the ref of the item which holds it if it is
  // too long for the page.
  case class Cell(child: Long, key: Data, ref: Long)

  // An entry of an index, with the number of the index it is an entry of.
  case class Posting(index: Int, entry: Data)

  // The row of a collection which a record belongs to: `child`, of table `inner`, in the list
  // which the slot `slot` of the row `host` of table `table` refers to.
  case class Anchor(table: Int, host: Long, slot: Slot, inner: Int, child: Long)

// The owner's side of RaTEL §10: a reader through which the one process which has an image
// open for changing changes it, where it lies. Everything it reads of where things are, it
// reads afresh from the image, since a change may move a table, a region or an index; and
// everything it holds between two steps of a change is a record index, a ref or a page
// number, never an address, since those are what a move leaves as they were (§10.2).
//
// A change is validated in full before a byte of it is written (§10.3): what is refused
// leaves the image as it was. Once writing has begun, a failure leaves the image dirty, as an
// owner which did not finish leaves it, and a reader will have none of it.
private[stratiform] abstract class RatelEditor(opening: Ratel.Opening)
extends Ratel.Reader(opening):
  import Ratel.Error.Reason
  import RatelBytes.none
  import RatelEditor.{Anchor, Cell, Posting}

  // The image, as what it is stored to: the same bytes as the reader's view of it.
  protected def holding: Holding^{this}

  private def round4(size: Long): Long = (size + 3)/4*4
  private def round8(size: Long): Long = (size + 7)/8*8

  private def fail(reason: Reason)(using Tactic[Ratel.Error]): Nothing = Ratel.fail(reason)

  private def invalid(reason: Tel.Error.Reason)(using Tactic[Tel.Error]): Nothing =
    import errorDiagnostics.emptyDiagnostics
    abort(Tel.Error(reason))

  // Where the directories say things are, now.
  private def tableEntry(table: Int): Long = holding.u64(16 + 24L*3) + 32L*table
  private def indexEntry(index: Int): Long = holding.u64(16 + 24L*4) + 32L*index

  override private[stratiform] def section(number: Int): Extent =
    Extent(holding.u64(16 + 24L*number), holding.u64(24 + 24L*number))

  override private[stratiform] def rows(table: Int): Optional[Extent] =
    if table < 0 || table >= arrangement.tables.length then Unset else
      val entry = tableEntry(table)
      Extent(holding.u64(entry), holding.u32(entry + 8))

  override private[stratiform] def tree(index: Int)(using Tactic[Ratel.Error]): Tree =
    if index < 0 || index >= arrangement.indexes.length then fail(Reason.BadIndex)
    val entry = indexEntry(index)

    Tree
      ( holding.u64(entry), holding.u32(entry + 8), holding.u32(entry + 16), holding.u32(entry + 20),
        holding.u32(entry + 24), holding.u8(entry + 29), holding.u8(entry + 30) )

  override def state: Ratel.State = Ratel.State.Unsealed
  override private[stratiform] def digested: Boolean = false

  private def garbage(bytes: Long): Unit = holding.set64(160L, holding.u64(160L) + bytes)

  // Whether a change is being written: one which does not finish leaves this set, and the
  // image dirty.
  private val changing: juca.AtomicBoolean = juca.AtomicBoolean(false)

  private def change[result](action: => result): result =
    changing.set(true)
    val result = action
    changing.set(false)
    result

  // §10.1: an owner unseals a sealed image -- which it does to a copy of its own, never to one
  // which others may have mapped -- and marks the image dirty before its first change.
  private[stratiform] def begin(): Unit =
    if (holding.u16(6L) & RatelBytes.sealing) != 0 then
      val digests = 16 + 24L*5
      garbage(holding.u64(digests + 16))
      holding.set64(digests + 8, 0L)
      holding.set64(digests + 16, 0L)
      holding.zero(168L, 32L)

    holding.set16(6L, RatelBytes.dirty)
    holding.flush()

  // On a clean close every change is made durable, and then the dirty flag is cleared.
  private[stratiform] def end(): Unit = if !changing.get then
    holding.flush()
    holding.set16(6L, 0)
    holding.flush()

  // §10.2: room for a new extent, at the committed length, which advances past it. The image
  // is extended if it is too short, by at least as much again, so that it is seldom remapped.
  private def allocate(size: Long): Long =
    val start = holding.u64(8L)
    val end = start + size
    if end > holding.size then holding.expand(end.max(2L*holding.size))
    holding.zero(start, size)
    holding.set64(8L, end)
    start

  // Appends an item to a region, which is moved to one of twice the capacity if it is full.
  private def append(region: Int, item: Data)(using Tactic[Ratel.Error]): Long =
    val entry = 16 + 24L*region
    val length = holding.u64(entry + 8)
    val capacity = holding.u64(entry + 16)

    if length + item.length > capacity then
      val room = round8((2L*capacity).max(length + item.length).max(64L))
      if room > 0xffffffffL then fail(Reason.Oversized)
      val moved = allocate(room)
      holding.move(holding.u64(entry), moved, length)
      garbage(capacity)
      holding.set64(entry, moved)
      holding.set64(entry + 16, room)

    holding.write(holding.u64(entry) + length, item)
    holding.set64(entry + 8, length + item.length)
    length

  // The size of a value item, and of a growable item, as they lie in their regions.
  private def valueSize(strings: Boolean, ref: Long): Long =
    4L + round4(holding.u32(section(if strings then 1 else 2).offset + ref))

  private def listSize(ref: Long, unit: Int): Long =
    8L + round4(holding.u32(section(2).offset + ref)*unit)

  // A value which is no longer referred to: garbage, unless it is interned, in which case
  // something else may refer to it still.
  private def abandon(intern: Boolean, ref: Long): Unit =
    if !intern then garbage(valueSize(false, ref))

  // A row: from the table's free list, or at its end, or -- once the table is moved to one of
  // twice the capacity -- at its new end. It is zeroed, and given its header.
  private def allocateRow(table: Int, parent: Long, link: Int)(using Tactic[Ratel.Error]): Long =
    val entry = tableEntry(table)
    val rowSize = plan(table).rowSize.toLong
    val freeHead = holding.u32(entry + 20)

    val row =
      if freeHead != none then
        holding.set32(entry + 20, holding.u32(holding.u64(entry) + freeHead*rowSize))
        freeHead
      else
        val count = holding.u32(entry + 8)
        val capacity = holding.u32(entry + 12)

        if count == capacity then
          val room = (2L*capacity).max(1L)
          val moved = allocate(room*rowSize)
          holding.move(holding.u64(entry), moved, count*rowSize)
          garbage(capacity*rowSize)
          holding.set64(entry, moved)
          holding.set32(entry + 12, room)

        holding.set32(entry + 8, count + 1)
        count

    val start = holding.u64(entry) + row*rowSize
    holding.zero(start, rowSize)
    holding.set32(start, parent)
    holding.set16(start + 4, link)
    holding.set16(start + 6, RatelBytes.live)
    row

  private def freeRow(table: Int, row: Long)(using Tactic[Ratel.Error]): Unit =
    val entry = tableEntry(table)
    val rowSize = plan(table).rowSize.toLong
    val start = holding.u64(entry) + row*rowSize
    holding.zero(start, rowSize)
    holding.set32(start, holding.u32(entry + 20))
    holding.set16(start + 4, RatelBytes.unlinked)
    holding.set32(entry + 20, row)

  private def at(table: Int, row: Long, slot: Slot)(using Tactic[Ratel.Error]): Long =
    address(table, row) + slot.offset

  private def mark(table: Int, row: Long, slot: Slot, present: Boolean)(using Tactic[Ratel.Error])
  :   Unit =

    if slot.presence >= 0 then
      val byte = address(table, row) + 8 + (slot.presence >> 3)
      val bit = 1 << (slot.presence & 7)
      holding.set8(byte, if present then holding.u8(byte) | bit else holding.u8(byte) & ~bit)

  // The bytes of a scalar's value: UTF-8, or what the codec of its encoding makes of it, which
  // for a scalar of a declared width must be of that width (↯/t.5.22).
  private def encode(scalar: Tels.Scalar, text: Text)(using Tactic[Tel.Error], Tactic[Ratel.Error])
  :   Data =

    val bytes: Data = scalar.encoding.lay(RatelBytes.utf8(text)): name =>
      bindings(name).or(fail(Reason.CodecUnresolved)).encode(text) match
        case Tel.Codec.Encoded.Bytes(data)      => data
        case Tel.Codec.Encoded.Invalid(message) => invalid(Tel.Error.Reason.EncodingRejected)

    RatelLayout.width(scalar).let: width =>
      if bytes.length != width then fail(Reason.WidthMismatch)

    bytes

  // A value item, in the string table if the scalar is interned and in the variable region
  // otherwise. An interned value is appended without looking for an equal one (§5.3).
  private def store(scalar: Tels.Scalar, text: Text)(using Tactic[Tel.Error], Tactic[Ratel.Error])
  :   Long =

    val bytes = encode(scalar, text)
    append(if scalar.intern then 1 else 2, RatelBytes.item(bytes.length, bytes))

  // A growable item which holds exactly `count` elements, or the empty ref for none.
  private def grow(count: Int, unit: Int, body: Data)(using Tactic[Ratel.Error]): Long =
    if count == 0 then none else append(2, RatelBytes.growable(count, count, unit, body))

  // §10.2: an element is put into a growable item where there is room for it, and otherwise
  // into a copy of twice the capacity, which the slot is then pointed at.
  private def splice(table: Int, row: Long, slot: Slot, unit: Int, position: Long, element: Data)
    ( using Tactic[Ratel.Error] )
  :   Unit =

    val ref = word(at(table, row, slot))
    val region = section(2).offset
    val capacity = if ref == none then 0L else holding.u32(region + ref)
    val count = if ref == none then 0L else holding.u32(region + ref + 4)

    if count == capacity then
      val body: Data = if count == 0L then Array.empty[Byte] else copy(region + ref + 8, (count*unit).toInt)
      val moved = append(2, RatelBytes.growable((2L*capacity).max(1L), count, unit, body))
      if ref != none then garbage(listSize(ref, unit))
      holding.set32(at(table, row, slot), moved)

    val base = section(2).offset + word(at(table, row, slot)) + 8
    holding.move(base + position*unit, base + (position + 1)*unit, (count - position)*unit)
    holding.write(base + position*unit, element)
    holding.set32(base - 4, count + 1)

  // Takes an element out of a growable item, which keeps its capacity for what comes later.
  private def excise(table: Int, row: Long, slot: Slot, unit: Int, position: Long)
    ( using Tactic[Ratel.Error] )
  :   Unit =

    val base = section(2).offset + word(at(table, row, slot)) + 8
    val count = holding.u32(base - 4)
    holding.move(base + (position + 1)*unit, base + position*unit, (count - position - 1)*unit)
    holding.zero(base + (count - 1)*unit, unit.toLong)
    holding.set32(base - 4, count - 1)

  private def little(value: Long): Data = Array.collect[Byte](4)(RatelBytes.u32(_, value))

  private def index(element: Tel.Element): Int = element match
    case Tel.Element.Node(index, _, _)  => index.or(0)
    case Tel.Element.Value(index, _, _) => index

  private def node(element: Tel.Element)(using Tactic[Ratel.Error]): Array[Tel.Element]^{} =
    element match
      case Tel.Element.Node(_, _, children) => children
      case value: Tel.Element.Value         => fail(Reason.Unencodable)

  private def text(element: Tel.Element)(using Tactic[Ratel.Error]): Text = element match
    case Tel.Element.Value(_, _, text) => text
    case node: Tel.Element.Node        => fail(Reason.Unencodable)

  // The elements of a struct's children which are a slot's: those of its member, or, for a
  // select, of any of its variants.
  private def those(slot: Slot, children: Array[Tel.Element]^{}): List[Tel.Element] =
    val span = slot.kind match
      case Kind.Select(variants)     => variants.size
      case Kind.SelectList(variants) => variants.size
      case _                         => 1

    children.to[List].filter: element =>
      index(element) >= slot.keywordIndex && index(element) < slot.keywordIndex + span

  // Whether every value of a record, and of the records within it, can be stored: what
  // `fill` would refuse is refused here, before anything is written.
  private def storable(entries: List[Entry], children: Array[Tel.Element]^{}, depth: Int)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    if depth > 256 then fail(Reason.NestingLimitExceeded)

    def within(table: Int, element: Tel.Element): Unit =
      storable(plan(table).entries, node(element), depth + 1)

    def choice(variants: List[RatelLayout.Variant], element: Tel.Element): Unit =
      variants.seek(_.keywordIndex == index(element)).or(fail(Reason.Unencodable)).payload match
        case Payload.Record(table, _) => within(table, element)
        case Payload.Value(scalar, _) => encode(scalar, text(element))
        case Payload.Flag             => ()

    entries.each:
      case Entry.Flattened(keywordIndex, _, _, inner) =>
        val found = children.seek(index(_) == keywordIndex).or(fail(Reason.Unencodable))
        storable(inner, node(found), depth)

      case Entry.Stored(slot) =>
        val mine = those(slot, children)

        val needed = slot.kind match
          case Kind.Inline(_, _) | Kind.Ref(_, _) | Kind.Record(_, _) | Kind.Select(_) => true
          case _                                                                       => false

        if needed && slot.required && mine.nil then fail(Reason.Unencodable)

        slot.kind match
          case Kind.Inline(scalar, _)    => mine.head.let { element => encode(scalar, text(element)) }
          case Kind.Ref(scalar, _)       => mine.head.let { element => encode(scalar, text(element)) }
          case Kind.Packed(scalar, _)    => mine.each { element => encode(scalar, text(element)) }
          case Kind.Refs(scalar, _)      => mine.each { element => encode(scalar, text(element)) }
          case Kind.Record(table, _)     => mine.head.let(within(table, _))
          case Kind.Children(table, _)   => mine.each(within(table, _))
          case Kind.Select(variants)     => mine.head.let(choice(variants, _))
          case Kind.SelectList(variants) => mine.each(choice(variants, _))
          case _                         => ()

  // Writes a record from its semantic model, into a row of its own, and answers the row. Its
  // values are appended, and the records within it written, as they are met.
  private def emit(table: Int, children: Array[Tel.Element]^{}, parent: Long, link: Int)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Long =

    val row = allocateRow(table, parent, link)
    fill(table, row, plan(table).entries, children)
    row

  private def fill(table: Int, row: Long, entries: List[Entry], children: Array[Tel.Element]^{})
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    def choice(variants: List[RatelLayout.Variant], element: Tel.Element, slot: Slot): (Long, Long) =
      val variant = variants.seek(_.keywordIndex == index(element)).or(fail(Reason.Unencodable))

      val payload: Long = variant.payload match
        case Payload.Record(inner, _) => emit(inner, node(element), row, slot.link)
        case Payload.Value(scalar, _) => store(scalar, text(element))
        case Payload.Flag             => 0L

      (variant.keywordIndex.toLong, payload)

    entries.each:
      case Entry.Flattened(keywordIndex, _, _, inner) =>
        val found = children.seek(index(_) == keywordIndex).or(fail(Reason.Unencodable))
        fill(table, row, inner, node(found))

      case Entry.Stored(slot) =>
        val mine = those(slot, children)

        slot.kind match
          case Kind.Always => ()
          case Kind.Flag   => if !mine.nil then mark(table, row, slot, true)

          case Kind.FlagCount =>
            holding.set32(at(table, row, slot), mine.size.toLong)

          case Kind.Inline(scalar, _) => mine.head.let: element =>
            holding.write(at(table, row, slot), encode(scalar, text(element)))
            mark(table, row, slot, true)

          case Kind.Ref(scalar, _) => mine.head.let: element =>
            val ref = store(scalar, text(element))
            holding.set32(at(table, row, slot), ref)
            mark(table, row, slot, true)

          case Kind.Packed(scalar, width) =>
            val values = mine.map { element => encode(scalar, text(element)) }
            val body = Array.collect[Byte](width*values.size + 8)(out => values.each(out.append(_)))
            val ref = grow(values.size, width, body)
            holding.set32(at(table, row, slot), ref)

          case Kind.Refs(scalar, _) =>
            val refs = mine.map { element => store(scalar, text(element)) }
            val body = Array.collect[Byte](4*refs.size + 8) { out => refs.each(RatelBytes.u32(out, _)) }
            val ref = grow(refs.size, 4, body)
            holding.set32(at(table, row, slot), ref)

          case Kind.Record(inner, _) => mine.head.let: element =>
            val child = emit(inner, node(element), row, slot.link)
            holding.set32(at(table, row, slot), child)
            mark(table, row, slot, true)

          case Kind.Children(inner, _) =>
            val numbers = mine.map { element => emit(inner, node(element), row, slot.link) }
            val body = Array.collect[Byte](4*numbers.size + 8) { out => numbers.each(RatelBytes.u32(out, _)) }
            val ref = grow(numbers.size, 4, body)
            holding.set32(at(table, row, slot), ref)

          case Kind.Select(variants) => mine.head.let: element =>
            val (tag, payload) = choice(variants, element, slot)
            holding.set32(at(table, row, slot), tag)
            holding.set32(at(table, row, slot) + 4, payload)
            mark(table, row, slot, true)

          case Kind.SelectList(variants) =>
            val pairs = mine.map(choice(variants, _, slot))

            val body = Array.collect[Byte](8*pairs.size + 8): out =>
              pairs.each: (tag, payload) =>
                RatelBytes.u32(out, tag)
                RatelBytes.u32(out, payload)

            val ref = grow(pairs.size, 8, body)
            holding.set32(at(table, row, slot), ref)

  // The rows a collection slot refers to, in order.
  private def listed(table: Int, row: Long, slot: Slot, unit: Long)(using Tactic[Ratel.Error])
  :   List[Long] =

    val found = list(word(at(table, row, slot)), unit)
    List.range(0, found.length.toInt).map { position => found.offset + unit*position }

  // The entries which the indexes at a site have for one child of one host.
  private def entriesOf(table: Int, host: Long, slot: Slot, child: Long)(using Tactic[Ratel.Error])
  :   List[Posting] =

    slot.kind match
      case Kind.Children(inner, _) =>
        indexesAt(table, slot).map: (index, number) =>
          val entry: Data = Array.collect[Byte](32): out =>
            RatelBytes.u32be(out, host)
            index.components.each(component(out, _, inner, child))
            RatelBytes.u32be(out, child)

          Posting(number, entry)

      case _ =>
        Nil

  // The entries which the indexes at an array's site have for the elements of one host.
  private def arrayEntries(table: Int, host: Long, slot: Slot)(using Tactic[Ratel.Error])
  :   List[Posting] =

    val indexes = indexesAt(table, slot)

    val unit: Long = slot.kind match
      case Kind.Packed(_, width) => width.toLong
      case _                     => 4L

    // An element's bytes, found afresh by its position, since nothing is held of where the
    // array lies.
    def element(position: Int): Data =
      val found = list(word(at(table, host, slot)), unit).offset + unit*position

      slot.kind match
        case Kind.Packed(_, width) => copy(found, width)
        case Kind.Refs(_, intern)  => bytes(intern, word(found))
        case _                     => Array.empty[Byte]

    val fixed = slot.kind match
      case Kind.Packed(_, _) => true
      case _                 => false

    if indexes.nil then Nil else
      val count = list(word(at(table, host, slot)), unit).length.toInt

      indexes.bind: (index, number) =>
        List.range(0, count).map: position =>
          val entry: Data = Array.collect[Byte](32): out =>
            RatelBytes.u32be(out, host)
            RatelBytes.component(out, element(position), fixed)
            RatelBytes.u32be(out, position)

          Posting(number, entry)

  // Adds to the indexes, or takes from them, the entries of every site within a record: of
  // the children of its repeatable members, and of the records within those, and so on down.
  private def reindex(table: Int, row: Long, entries: List[Entry], adding: Boolean)
    ( using Tactic[Ratel.Error] )
  :   Unit =

    def apply(found: List[Posting]): Unit = found.each: posting =>
      if adding then enter(posting.index, posting.entry) else leave(posting.index, posting.entry)

    def within(inner: Int, child: Long): Unit = reindex(inner, child, plan(inner).entries, adding)

    def chosen(variants: List[RatelLayout.Variant], tag: Long, payload: Long): Unit =
      variants.seek(_.keywordIndex == tag).let(_.payload).let:
        case Payload.Record(inner, _) => within(inner, payload)
        case _                        => ()

    entries.each:
      case Entry.Flattened(_, _, _, inner) =>
        reindex(table, row, inner, adding)

      case Entry.Stored(slot) =>
        slot.kind match
          case Kind.Record(inner, _) =>
            if present(address(table, row), slot) then within(inner, word(at(table, row, slot)))

          case Kind.Children(inner, _) =>
            listed(table, row, slot, 4L).map(word(_)).each: child =>
              apply(entriesOf(table, row, slot, child))
              within(inner, child)

          case Kind.Packed(_, _) | Kind.Refs(_, _) =>
            apply(arrayEntries(table, row, slot))

          case Kind.Select(variants) =>
            if present(address(table, row), slot)
            then chosen(variants, word(at(table, row, slot)), word(at(table, row, slot) + 4))

          case Kind.SelectList(variants) =>
            listed(table, row, slot, 8L).each { pair => chosen(variants, word(pair), word(pair + 4)) }

          case _ =>
            ()

  // Frees the rows of a record and of every record within it, and abandons the items which
  // they alone refer to. Its index entries are taken out first, by `reindex`.
  private def discard(table: Int, row: Long, entries: List[Entry])(using Tactic[Ratel.Error])
  :   Unit =

    def within(inner: Int, child: Long): Unit =
      discard(inner, child, plan(inner).entries)
      freeRow(inner, child)

    def chosen(variants: List[RatelLayout.Variant], tag: Long, payload: Long): Unit =
      variants.seek(_.keywordIndex == tag).let(_.payload).let:
        case Payload.Record(inner, _)   => within(inner, payload)
        case Payload.Value(_, intern)   => abandon(intern, payload)
        case Payload.Flag               => ()

    entries.each:
      case Entry.Flattened(_, _, _, inner) =>
        discard(table, row, inner)

      case Entry.Stored(slot) =>
        val ref = word(at(table, row, slot))
        val held = present(address(table, row), slot)

        slot.kind match
          case Kind.Ref(_, intern) =>
            if held then abandon(intern, ref)

          case Kind.Packed(_, width) =>
            if ref != none then garbage(listSize(ref, width))

          case Kind.Refs(_, intern) =>
            listed(table, row, slot, 4L).each { element => abandon(intern, word(element)) }
            if ref != none then garbage(listSize(ref, 4))

          case Kind.Record(inner, _) =>
            if held then within(inner, ref)

          case Kind.Children(inner, _) =>
            listed(table, row, slot, 4L).map(word(_)).each(within(inner, _))
            if ref != none then garbage(listSize(ref, 4))

          case Kind.Select(variants) =>
            if held then chosen(variants, ref, word(at(table, row, slot) + 4))

          case Kind.SelectList(variants) =>
            listed(table, row, slot, 8L).each { pair => chosen(variants, word(pair), word(pair + 4)) }
            if ref != none then garbage(listSize(ref, 8))

          case _ =>
            ()


  private def pageSize(index: Int)(using Tactic[Ratel.Error]): Long = tree(index).pageSize
  private def pageAt(index: Int, number: Long)(using Tactic[Ratel.Error]): Long =
    holding.u64(indexEntry(index)) + number*pageSize(index)

  // A cell for an entry: spilled to the variable region if it is longer than an eighth of a
  // page.
  private def cell(index: Int, child: Long, key: Data)(using Tactic[Ratel.Error]): Cell =
    if key.length <= pageSize(index)/8 then Cell(child, key, none)
    else Cell(child, key, append(2, RatelBytes.item(key.length, key)))

  private def cells(index: Int, number: Long)(using Tactic[Ratel.Error]): List[Cell] =
    val start = pageAt(index, number)
    val head = if holding.u8(start) == 2 then 4 else 0

    List.range(0, holding.u16(start + 2)).map: position =>
      val pointer = start + holding.u16(start + 16 + 2*position)
      val child = if head == 4 then holding.u32(pointer) else none
      val length = holding.u16(pointer + head)

      if length == 0xffff then
        val ref = holding.u32(pointer + head + 4)
        Cell(child, bytes(false, ref), ref)
      else Cell(child, copy(pointer + head + 2, length), none)

  private def cellSize(kind: Int, cell: Cell): Int =
    val head = if kind == 2 then 4 else 0
    if cell.ref != none then head + 8 else (head + 2 + cell.key.length + 3)/4*4

  // The bytes which some cells take in a page, each with the pointer to it.
  private def weight(kind: Int, all: List[Cell]): Int =
    @scala.annotation.tailrec
    def recur(rest: List[Cell], sum: Int): Int = rest match
      case Nil          => sum
      case cell :: more => recur(more, sum + 2 + cellSize(kind, cell))

    recur(all, 0)

  // Whether a page has room for some cells, beside its header.
  private def fits(index: Int, kind: Int, all: List[Cell])(using Tactic[Ratel.Error]): Boolean =
    16 + weight(kind, all) <= pageSize(index)

  // Writes a page afresh: its header, the pointers to its cells in the order of their keys,
  // and the cells, from the end of the page downwards in that order.
  private def lay(index: Int, number: Long, kind: Int, link: Long, all: List[Cell])
    ( using Tactic[Ratel.Error] )
  :   Unit =

    val start = pageAt(index, number)
    val size = pageSize(index)
    val head = if kind == 2 then 4 else 0
    holding.zero(start, size)

    @scala.annotation.tailrec
    def recur(rest: List[Cell], position: Int, end: Long): Long = rest match
      case Nil => end

      case cell :: more =>
        val pointer = end - cellSize(kind, cell)
        holding.set16(start + 16 + 2*position, pointer.toInt)
        if kind == 2 then holding.set32(start + pointer, cell.child)

        if cell.ref != none then
          holding.set16(start + pointer + head, 0xffff)
          holding.set32(start + pointer + head + 4, cell.ref)
        else
          holding.set16(start + pointer + head, cell.key.length)
          holding.write(start + pointer + head + 2, cell.key)

        recur(more, position + 1, pointer)

    val reached = recur(all, 0, size)

    holding.set8(start, kind)
    holding.set16(start + 2, all.size)
    holding.set32(start + 4, reached)
    holding.set32(start + 8, link)

  // A page: from the index's free list, or at its end, or -- once its pages are moved to an
  // extent of twice as many -- at its new end.
  private def allocatePage(index: Int)(using Tactic[Ratel.Error]): Long =
    val entry = indexEntry(index)
    val size = pageSize(index)
    val freeHead = holding.u32(entry + 20)

    if freeHead != none then
      holding.set32(entry + 20, holding.u32(holding.u64(entry) + freeHead*size + 8))
      freeHead
    else
      val count = holding.u32(entry + 8)
      val capacity = holding.u32(entry + 12)

      if count == capacity then
        val room = (2L*capacity).max(1L)
        val moved = allocate(room*size)
        holding.move(holding.u64(entry), moved, count*size)
        garbage(capacity*size)
        holding.set64(entry, moved)
        holding.set32(entry + 12, room)

      holding.set32(entry + 8, count + 1)
      count

  // The pages from the root to the leaf where an entry belongs: each branch with the position
  // among its children of the one which was followed, the leaf last.
  private def route(index: Int, entry: Data)(using Tactic[Ratel.Error]): (List[(Long, Int)], Long) =
    val tree = this.tree(index)

    @scala.annotation.tailrec
    def descend(number: Long, level: Int, path: List[(Long, Int)]): (List[(Long, Int)], Long) =
      if level == tree.height - 1 then (path, number) else
        val all = cells(index, number)
        val below = all.filter { cell => RatelBytes.order(cell.key, entry) <= 0 }.size
        val link = holding.u32(pageAt(index, number) + 8)
        val next = if below == 0 then link else all.skip(below - 1).head.let(_.child).or(link)
        descend(next, level + 1, (number, below) :: path)

    descend(tree.root, 0, Nil)

  // The cells of a page in two halves, of about as many bytes each, neither of them empty.
  private def halves(kind: Int, all: List[Cell]): Int =
    val total = weight(kind, all)

    @scala.annotation.tailrec
    def recur(rest: List[Cell], count: Int, sum: Int): Int = rest match
      case Nil          => count
      case cell :: more => if sum*2 >= total then count else recur(more, count + 1, sum + 2 + cellSize(kind, cell))

    recur(all, 0, 0).max(1).min(all.size - 1)

  // §9.7: puts an entry into an index, at the leaf where it belongs, which is split if it has
  // no room, as its parent is then, and so on up to a root which is given a new one above it.
  private def enter(index: Int, entry: Data)(using Tactic[Ratel.Error]): Unit =
    val directory = indexEntry(index)

    if holding.u32(directory + 16) == none then
      val page = allocatePage(index)
      lay(index, page, 1, none, List(cell(index, none, entry)))
      holding.set32(indexEntry(index) + 16, page)
      holding.set8(indexEntry(index) + 30, 1)
    else
      val (path, leaf) = route(index, entry)
      val before = cells(index, leaf)
      val position = before.filter { cell => RatelBytes.order(cell.key, entry) < 0 }.size
      val all: List[Cell] = before.keep(position) + List(cell(index, none, entry)) + before.skip(position)
      val link = holding.u32(pageAt(index, leaf) + 8)

      if fits(index, 1, all) then lay(index, leaf, 1, link, all) else
        val split = halves(1, all)
        val right = allocatePage(index)
        lay(index, leaf, 1, right, all.keep(split))
        lay(index, right, 1, link, all.skip(split))
        val separator = all.skip(split).head.let(_.key).or(entry)
        raise(index, path, cell(index, right, separator))

    holding.set32(indexEntry(index) + 24, holding.u32(indexEntry(index) + 24) + 1)

  // Puts the separator of a page which has just been split off into the branch above it.
  private def raise(index: Int, path: List[(Long, Int)], added: Cell)(using Tactic[Ratel.Error])
  :   Unit =

    path match
      case Nil =>
        val directory = indexEntry(index)
        val root = holding.u32(directory + 16)
        val page = allocatePage(index)
        lay(index, page, 2, root, List(added))
        holding.set32(indexEntry(index) + 16, page)
        holding.set8(indexEntry(index) + 30, holding.u8(indexEntry(index) + 30) + 1)

      case (branch, position) :: above =>
        val before = cells(index, branch)
        val all: List[Cell] = before.keep(position) + List(added) + before.skip(position)
        val link = holding.u32(pageAt(index, branch) + 8)

        if fits(index, 2, all) then lay(index, branch, 2, link, all) else
          val split = halves(2, all)
          val middle = all.skip(split).head.or(added)
          val right = allocatePage(index)
          lay(index, branch, 2, link, all.keep(split))
          lay(index, right, 2, middle.child, all.skip(split + 1))
          raise(index, above, Cell(right, middle.key, middle.ref))

  // Takes an entry out of an index. Its leaf is left where it is, however little it holds.
  private def leave(index: Int, entry: Data)(using Tactic[Ratel.Error]): Unit =
    if holding.u32(indexEntry(index) + 16) == none then fail(Reason.IndexDisagrees)
    val (path, leaf) = route(index, entry)
    val before = cells(index, leaf)
    val found = before.seek { cell => RatelBytes.order(cell.key, entry) == 0 }.or(fail(Reason.IndexDisagrees))
    if found.ref != none then garbage(valueSize(false, found.ref))
    val all = before.filter { cell => RatelBytes.order(cell.key, entry) != 0 }
    lay(index, leaf, 1, holding.u32(pageAt(index, leaf) + 8), all)
    holding.set32(indexEntry(index) + 24, holding.u32(indexEntry(index) + 24) - 1)

  // The row of a collection which a record belongs to, if it belongs to one: found by
  // following the links of row headers up through members which are single records, to the
  // first which is a collection. It is the row whose index entries a change to the record
  // may alter (§10.3).
  private[stratiform] def anchor(table: Int, row: Long)(using Tactic[Ratel.Error]): Optional[Anchor] =
    val start = address(table, row)
    val link = holding.u16(start + 4)

    if link == RatelBytes.unlinked then Unset else
      arrangement.link(link).let: found =>
        found.slot.kind match
          case Kind.Children(inner, _) => Anchor(found.table, holding.u32(start), found.slot, inner, row)
          case Kind.Record(_, _)       => anchor(found.table, holding.u32(start))
          case _                       => Unset

  // Makes a change to a record, keeping the indexes in step: the entries of the row it is
  // anchored to are worked out before and after, and those which differ are replaced.
  private def reindexing(table: Int, row: Long)(action: => Unit)(using Tactic[Ratel.Error]): Unit =
    val anchored = anchor(table, row)
    val before = anchored.lay(Nil) { found => entriesOf(found.table, found.host, found.slot, found.child) }
    action
    val after = anchored.lay(Nil) { found => entriesOf(found.table, found.host, found.slot, found.child) }

    before.zip(after).each: (old, fresh) =>
      if RatelBytes.order(old.entry, fresh.entry) != 0 then
        leave(old.index, old.entry)
        enter(fresh.index, fresh.entry)

  // ↯/t.3.14: no other child of a host has the key which a child is to have.
  private def unique(table: Int, host: Long, slot: Slot, key: Optional[Text], self: Long)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    indexesAt(table, slot).seek(_(0).primary).let: (index, number) =>
      index.components.head.let: component =>
        key.let: text =>
          val probe: Data = Array.collect[Byte](32): out =>
            RatelBytes.u32be(out, host)
            RatelBytes.component(out, encode(component.scalar, text), component.width.present)

          if search(number, probe).exists(_ != self) then invalid(Tel.Error.Reason.DuplicateKeyValue)

  // The value which a path of keyword indices leads to within a record's model.
  private def find(children: Array[Tel.Element]^{}, keywords: List[Int]): Optional[Text] =
    keywords match
      case Nil => Unset

      case keyword :: rest => children.seek(index(_) == keyword).let:
        case Tel.Element.Value(_, _, text)    => if rest.nil then text else Unset
        case Tel.Element.Node(_, _, children) => find(children, rest)

  private def position(table: Int, host: Long, slot: Slot, child: Long)(using Tactic[Ratel.Error])
  :   Long =

    val children = listed(table, host, slot, 4L).map(word(_))
    val found = children.zip(List.range(0, children.size)).seek(_(0) == child)
    found.let(_(1).toLong).or(fail(Reason.BadRowHeader))

  // The key which a record's model gives it, if its type has one.
  private def keyOf(table: Int, slot: Slot, children: Array[Tel.Element]^{}): Optional[Text] =
    indexesAt(table, slot).seek(_(0).primary).let: (index, number) =>
      index.components.head.let { component => find(children, component.keywords) }

  // Writes a record into a collection at a position, and enters it, and whatever is within
  // it, in the indexes.
  private def put(table: Int, host: Long, slot: Slot, inner: Int, position: Long, children: Array[Tel.Element]^{})
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Long =

    val child = emit(inner, children, host, slot.link)
    splice(table, host, slot, 4, position, little(child))
    entriesOf(table, host, slot, child).each { posting => enter(posting.index, posting.entry) }
    reindex(inner, child, plan(inner).entries, true)
    child

  // Takes a record out of a collection: its entries, and those of whatever is within it, out
  // of the indexes; its rows freed; and its place in its parent's list closed up.
  private def take(table: Int, host: Long, slot: Slot, inner: Int, row: Long, place: Long)
    ( using Tactic[Ratel.Error] )
  :   Unit =

    reindex(inner, row, plan(inner).entries, false)
    entriesOf(table, host, slot, row).each { posting => leave(posting.index, posting.entry) }
    discard(inner, row, plan(inner).entries)
    freeRow(inner, row)
    excise(table, host, slot, 4, place)

  // §10.3, `insert`: a record is put into a collection at a position. Its rows are written,
  // depth first; it is spliced into its parent's child list; and an entry is put into every
  // index whose site it, or anything within it, joins.
  private[stratiform] def insert
    ( table: Int, host: Long, slot: Slot, position: Long, element: Tel.Element )
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Long =

    slot.kind match
      case Kind.Children(inner, _) =>
        val children = node(element)
        val size = list(word(at(table, host, slot)), 4L).length
        if position < 0 || position > size then fail(Reason.BadRecordIndex)
        storable(plan(inner).entries, children, 0)
        unique(table, host, slot, keyOf(table, slot, children), none)
        change(put(table, host, slot, inner, position, children))

      case _ =>
        fail(Reason.Inapplicable(slot.keyword))

  // The collection a record is one of, and its place in it.
  private def placed(table: Int, row: Long)(using Tactic[Ratel.Error]): Anchor =
    val start = address(table, row)
    val found = arrangement.link(holding.u16(start + 4)).or(fail(Reason.Inapplicable(t"")))

    found.slot.kind match
      case Kind.Children(inner, _) => Anchor(found.table, holding.u32(start), found.slot, inner, row)
      case _                       => fail(Reason.Inapplicable(found.slot.keyword))

  // §10.3, `delete`: a record is taken out of the collection it belongs to, with every record
  // within it.
  private[stratiform] def remove(table: Int, row: Long)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    val found = placed(table, row)
    val place = position(found.table, found.host, found.slot, row)
    val size = list(word(at(found.table, found.host, found.slot)), 4L).length

    // ↯/t.3.7: a member which must be present keeps one record at least.
    if found.slot.required && size == 1L then invalid(Tel.Error.Reason.RequiredMemberAbsent)
    change(take(found.table, found.host, found.slot, found.inner, row, place))

  // §10.3, `replace`: a `delete` and an `insert` at the same position, validated as one
  // operation: the record which is to take the place is checked before the one which has it
  // is taken out.
  private[stratiform] def replace(table: Int, row: Long, element: Tel.Element)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Long =

    val found = placed(table, row)
    val children = node(element)
    val place = position(found.table, found.host, found.slot, row)
    storable(plan(found.inner).entries, children, 0)
    unique(found.table, found.host, found.slot, keyOf(found.table, found.slot, children), row)

    change:
      take(found.table, found.host, found.slot, found.inner, row, place)
      put(found.table, found.host, found.slot, found.inner, place, children)

  // §10.3, `reorder-within-group`: a record is moved to another position among its siblings.
  // Indexes name a child by its row, which does not change, so they are left as they are.
  private[stratiform] def move(table: Int, host: Long, slot: Slot, from: Long, to: Long)
    ( using Tactic[Ratel.Error] )
  :   Unit =

    val size = list(word(at(table, host, slot)), 4L).length
    if from < 0 || from >= size || to < 0 || to >= size then fail(Reason.BadRecordIndex)

    if from != to then change:
      val base = section(2).offset + word(at(table, host, slot)) + 8
      val moved = word(base + 4*from)
      if from < to then holding.move(base + 4*(from + 1), base + 4*from, 4*(to - from))
      else holding.move(base + 4*to, base + 4*(to + 1), 4*(from - to))
      holding.set32(base + 4*to, moved)

  // §10.3, `update-value`: a scalar member of a record is given a value, or, if it may be
  // absent, none. A value which is stored within the row is overwritten; one which is
  // referred to is appended afresh, and the old one abandoned.
  private[stratiform] def assign(table: Int, row: Long, slot: Slot, value: Optional[Text])
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    if value.absent && slot.required then invalid(Tel.Error.Reason.RequiredMemberAbsent)

    val scalar = slot.kind match
      case Kind.Inline(scalar, _) => scalar
      case Kind.Ref(scalar, _)    => scalar
      case _                      => fail(Reason.Inapplicable(slot.keyword))

    val stored: Optional[Data] = value.let(encode(scalar, _))

    // The key of a record is unique among its siblings.
    anchor(table, row).let: found =>
      if found.child == row && found.inner == table then
        indexesAt(found.table, found.slot).seek(_(0).primary).let: (index, number) =>
          val keyed = index.components.head.let { component =>
            component.hops.nil && component.last.number == slot.number }

          if keyed.or(false) then unique(found.table, found.host, found.slot, value, row)

    change:
      reindexing(table, row):
        val held = present(address(table, row), slot)

        slot.kind match
          case Kind.Inline(_, width) =>
            stored.lay(holding.zero(at(table, row, slot), width.toLong)): bytes =>
              holding.write(at(table, row, slot), bytes)

          case Kind.Ref(_, intern) =>
            if held then abandon(intern, word(at(table, row, slot)))

            stored.lay(holding.set32(at(table, row, slot), 0L)): bytes =>
              val ref = append(if intern then 1 else 2, RatelBytes.item(bytes.length, bytes))
              holding.set32(at(table, row, slot), ref)

          case _ => ()

        mark(table, row, slot, value.present)

  // The values of a repeatable scalar member are replaced, all together: which is how one is
  // added, taken away or moved, since the indexes over an array name its elements by position.
  private[stratiform] def assignAll(table: Int, row: Long, slot: Slot, values: List[Text])
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    val scalar = slot.kind match
      case Kind.Packed(scalar, _) => scalar
      case Kind.Refs(scalar, _)   => scalar
      case _                      => fail(Reason.Inapplicable(slot.keyword))

    val stored: List[Data] = values.map(encode(scalar, _))

    change:
      arrayEntries(table, row, slot).each { posting => leave(posting.index, posting.entry) }
      val old = word(at(table, row, slot))

      slot.kind match
        case Kind.Packed(_, width) =>
          if old != none then garbage(listSize(old, width))
          val body = Array.collect[Byte](width*stored.size + 8)(out => stored.each(out.append(_)))
          val ref = grow(stored.size, width, body)
          holding.set32(at(table, row, slot), ref)

        case Kind.Refs(_, intern) =>
          listed(table, row, slot, 4L).each { element => abandon(intern, word(element)) }
          if old != none then garbage(listSize(old, 4))

          val refs = stored.map: bytes =>
            append(if intern then 1 else 2, RatelBytes.item(bytes.length, bytes))

          val body = Array.collect[Byte](4*refs.size + 8) { out => refs.each(RatelBytes.u32(out, _)) }
          val ref = grow(refs.size, 4, body)
          holding.set32(at(table, row, slot), ref)

        case _ => ()

      arrayEntries(table, row, slot).each { posting => enter(posting.index, posting.entry) }

  // §10.3, `set-flag` and `unset-flag`: a flag which may be absent is made present or absent,
  // and one which may be repeated is counted up or down.
  private[stratiform] def flag(table: Int, row: Long, slot: Slot, on: Boolean)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Unit =

    slot.kind match
      case Kind.Flag =>
        change(mark(table, row, slot, on))

      case Kind.FlagCount =>
        val count = word(at(table, row, slot))
        if !on && count == 0L then invalid(Tel.Error.Reason.RequiredMemberAbsent)
        change(holding.set32(at(table, row, slot), if on then count + 1 else count - 1))

      case Kind.Always =>
        if !on then invalid(Tel.Error.Reason.RequiredMemberAbsent)

      case _ =>
        fail(Reason.Inapplicable(slot.keyword))
