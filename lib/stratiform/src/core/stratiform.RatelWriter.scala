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

import java.nio as jn

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import gastronomy.*
import gossamer.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

import RatelLayout.{Component, Entry, Kind, Payload, Slot}

// The writer of RaTEL §10.5, which `seal` and `repack` share: rows are allocated a table at
// a time as the semantic model is walked, the children of a repeatable member being reserved
// together before any of them is filled, so that they stay contiguous whatever their own
// children allocate; variable items are appended as they are met; and each index is sorted
// and loaded into its pages once every row is known. With no slack and pages of 4096 bytes,
// what is written is sealed, and is a function of the document alone.
//
// Where the specification leaves the order of items open, this is the order: the values of
// a repeatable scalar, or the payloads of a repeatable select, come before the array or list
// which refers to them; the entries and separators which are too long for a page come after
// everything else, an index at a time, its leaves first; and the pages of an index are
// numbered from its first leaf, a level at a time, so that the root is the last of them.
private[stratiform] object RatelWriter:
  private def fail(reason: Ratel.Error.Reason)(using Tactic[Ratel.Error]): Nothing =
    Ratel.fail(reason)

  private def index(element: Tel.Element): Int = element match
    case Tel.Element.Node(index, _, _)  => index.or(0)
    case Tel.Element.Value(index, _, _) => index

  // An entry of an index, held in a class so that a Java collection may sort it.
  private final class Sortable(val bytes: Data)

  private val bytewise: java.util.Comparator[Sortable] = new java.util.Comparator[Sortable]:
    def compare(left: Sortable | Null, right: Sortable | Null): Int =
      RatelBytes.order(left.nn.bytes, right.nn.bytes)

  // A row still to be filled, from the children of the node which it stores: `link` is the
  // number of the slot, of the row which reserved it, through which it is reached.
  private case class Pending(table: Int, row: Int, children: Array[Tel.Element]^{}, link: Int)

  // A page of an index which is being loaded: the child it is, the first entry beneath it,
  // and its cells, each a child of its own (for a branch) and an entry or a separator.
  private case class Loading(first: Data, leftmost: Long, cells: List[(Long, Data)])

  // What an index comes to: its pages, which of them is the root, and how deep it is.
  private case class Loaded(pages: List[Data], root: Long, height: Int, count: Int)

  // `slack` is the factor by which everything which can grow is given room to (§10.5): one
  // for a sealed file, which has none. A file which is not sealed carries neither a value
  // hash nor digests.
  def write
    ( root:     Tel.Element,
      declared: Tels,
      codecs:   Tel.Codec.Bindings,
      digests:  Boolean,
      sealing:  Boolean = true,
      slack:    Double  = 1.0,
      shift:    Int     = 12 )
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Data =

    import Ratel.Error.Reason

    val axiom = Tels.Axiom.tels
    val schema = Tels.Layers.compose(declared)
    val layout = RatelLayout(schema)
    val pageSize: Int = 1 << shift

    // The capacity of something which holds `count`: at least that, and room to grow.
    def roomy(count: Long): Long = if slack <= 1.0 then count else Math.ceil(count*slack).toLong

    schema.scalars.each: scalar =>
      scalar.encoding.let: name =>
        if codecs(name).absent then fail(Reason.CodecUnresolved)

    val (signature, schemaBody, valueHash) =
      mitigate:
        case _: Bintel.Error        => Ratel.error(Reason.Unencodable)
        case _: Tels.Renderer.Error => Ratel.error(Reason.Unencodable)

      . protect:
          val schemaRoot = Tels.Renderer.element(declared, axiom)

          ( SchemaSignature.fromElement(schemaRoot, axiom),
            Bintel.encode(schemaRoot, axiom),
            Blake3.hashOf(Bintel.encode(root, schema, codecs), 32) )

    val rows: java.util.ArrayList[java.util.ArrayList[Data]] = java.util.ArrayList()
    layout.tables.each(table => rows.add(java.util.ArrayList()))

    val entries: java.util.ArrayList[java.util.ArrayList[Sortable]] = java.util.ArrayList()
    layout.indexes.each(index => entries.add(java.util.ArrayList()))

    // The indexes at each site, by table and slot.
    val sites: java.util.HashMap[Long, List[(RatelLayout.Index, Int)]] = java.util.HashMap()

    layout.indexes.to[List].zip(List.range(0, layout.indexes.length)).each: (index, number) =>
      val site = index.table.toLong << 32 | index.slot.number

      val before: List[(RatelLayout.Index, Int)] =
        if sites.containsKey(site) then sites.get(site) else Nil

      sites.put(site, before + List((index, number)))

    val strings: java.util.ArrayList[Data] = java.util.ArrayList()
    val interned: java.util.HashMap[String, Long] = java.util.HashMap()
    val variable: java.util.ArrayList[Data] = java.util.ArrayList()
    var stringsSize: Long = 0L
    var variableSize: Long = 0L

    def bytes(scalar: Tels.Scalar, text: Text): Data =
      val bytes: Data = scalar.encoding.lay(RatelBytes.utf8(text)): name =>
        codecs(name).or(fail(Reason.CodecUnresolved)).encode(text) match
          case Tel.Codec.Encoded.Bytes(data) => data

          case Tel.Codec.Encoded.Invalid(message) =>
            import errorDiagnostics.emptyDiagnostics
            abort(Tel.Error(Tel.Error.Reason.EncodingRejected))

      // ↯/t.5.22: a declared width is a promise about every value, never an instruction to pad.
      RatelLayout.width(scalar).let: width =>
        if bytes.length != width then fail(Reason.WidthMismatch)

      bytes

    // Appends an item to the variable region, and answers its ref.
    def append(item: Data): Long =
      val ref = variableSize
      variable.add(item)
      variableSize += item.length
      if variableSize > Int.MaxValue then fail(Reason.Oversized)
      ref

    def vary(body: Data): Long = append(RatelBytes.item(body.length, body))

    // A growable item holding `count` elements of `unit` bytes each, or the empty ref if it
    // would hold none.
    def grow(count: Int, unit: Int, body: Data): Long =
      if count == 0 then RatelBytes.none
      else append(RatelBytes.growable(roomy(count), count, unit, body))

    // A value item: in the string table, each distinct value once, if the scalar is interned,
    // and in the variable region otherwise.
    def value(scalar: Tels.Scalar, text: Text): Long =
      val body = bytes(scalar, text)

      if !scalar.intern then vary(body) else
        val key = scalar.encoding.or(t"").s+"\u0000"+text.s

        if interned.containsKey(key) then interned.get(key).toLong else
          val ref = stringsSize
          val item = RatelBytes.item(body.length, body)
          strings.add(item)
          stringsSize += item.length
          if stringsSize > Int.MaxValue then fail(Reason.Oversized)
          interned.put(key, ref)
          ref

    // Adds an entry to an index, to be sorted when every entry is known.
    def keyed(number: Int)(lambda: Scribe[Byte] => Unit): Unit =
      entries.get(number).add(new Sortable(Array.collect[Byte](32)(lambda)))

    def allocate(table: Int): Int =
      val list = rows.get(table)
      list.add(Array.empty[Byte])
      list.size - 1

    // The value which an index component names within a child, if the child has it.
    def find(children: Array[Tel.Element]^{}, keywords: List[Int]): Optional[Text] =
      keywords match
        case Nil => Unset

        case keyword :: rest => children.seek(index(_) == keyword).let:
          case Tel.Element.Value(_, _, text)    => if rest.nil then text else Unset
          case Tel.Element.Node(_, _, children) => find(children, rest)

    def component(out: Scribe[Byte], component: Component, children: Array[Tel.Element]^{})
    :   Unit =

      find(children, component.keywords).lay(RatelBytes.absent(out)): text =>
        RatelBytes.component(out, bytes(component.scalar, text), component.width.present)

    def emit
      ( table: Int, row: Int, children: Array[Tel.Element]^{}, depth: Int, parent: Long, link: Int )
    :   Unit =

      if depth > 256 then fail(Reason.NestingLimitExceeded)
      val plan = layout.table(table).or(fail(Reason.Unencodable))
      val buffer = jn.ByteBuffer.allocate(plan.rowSize).order(jn.ByteOrder.LITTLE_ENDIAN)
      val pending: java.util.ArrayList[Pending] = java.util.ArrayList()

      // The row's header (§6.2): the row and the slot which refer to it, and that it is live.
      buffer.putInt(0, parent.toInt)
      buffer.putShort(4, link.toShort)
      buffer.putShort(6, RatelBytes.live.toShort)

      def mark(slot: Slot): Unit = if slot.presence >= 0 then
        val position = 8 + (slot.presence >> 3)
        buffer.put(position, (buffer.get(position) | 1 << (slot.presence & 7)).toByte)

      def node(element: Tel.Element): Array[Tel.Element]^{} = element match
        case Tel.Element.Node(_, _, children) => children
        case value: Tel.Element.Value         => fail(Reason.Unencodable)

      def text(element: Tel.Element): Text = element match
        case Tel.Element.Value(_, _, text) => text
        case node: Tel.Element.Node        => fail(Reason.Unencodable)

      def child(table: Int, element: Tel.Element, slot: Slot): Int =
        val number = allocate(table)
        pending.add(Pending(table, number, node(element), slot.link))
        number

      // A select's tag and payload: the keyword index of the variant chosen, and what it
      // refers to.
      def choice(variants: List[RatelLayout.Variant], element: Tel.Element, slot: Slot)
      :   (Long, Long) =

        val variant = variants.seek(_.keywordIndex == index(element)).or(fail(Reason.Unencodable))

        val payload: Long = variant.payload match
          case Payload.Record(table, _)      => child(table, element, slot)
          case Payload.Value(scalar, intern) => value(scalar, text(element))
          case Payload.Flag                  => 0L

        (variant.keywordIndex.toLong, payload)

      def fill(entries0: List[Entry], children: Array[Tel.Element]^{}): Unit = entries0.each:
        case Entry.Flattened(keywordIndex, _, _, inner) =>
          fill(inner, node(children.seek(index(_) == keywordIndex).or(fail(Reason.Unencodable))))

        case Entry.Stored(slot) =>
          val span = slot.kind match
            case Kind.Select(variants)     => variants.size
            case Kind.SelectList(variants) => variants.size
            case _                         => 1

          val mine: List[Tel.Element] = children.to[List].filter: element =>
            index(element) >= slot.keywordIndex && index(element) < slot.keywordIndex + span

          val site = table.toLong << 32 | slot.number
          val indexes: List[(RatelLayout.Index, Int)] =
            if sites.containsKey(site) then sites.get(site) else Nil

          // A required member which is not repeatable has a value in every valid document.
          val needed = slot.kind match
            case Kind.Inline(_, _) | Kind.Ref(_, _) | Kind.Record(_, _) | Kind.Select(_) => true
            case _                                                                       => false

          if needed && slot.required && mine.nil then fail(Reason.Unencodable)

          slot.kind match
            case Kind.Always => ()
            case Kind.Flag   => if !mine.nil then mark(slot)

            case Kind.FlagCount =>
              buffer.putInt(slot.offset, mine.size)

            case Kind.Inline(scalar, width) => mine.head.let: element =>
              val value = bytes(scalar, text(element))

              List.range(0, width).each: index =>
                buffer.put(slot.offset + index, value.u8(index).toByte)

              mark(slot)

            case Kind.Ref(scalar, intern) => mine.head.let: element =>
              buffer.putInt(slot.offset, value(scalar, text(element)).toInt)
              mark(slot)

            case Kind.Packed(scalar, width) =>
              val values = mine.map(element => bytes(scalar, text(element)))

              indexes.each: (index, number) =>
                mine.zip(List.range(0, mine.size)).each: (element, position) =>
                  keyed(number): out =>
                    RatelBytes.u32be(out, row)
                    RatelBytes.component(out, bytes(scalar, text(element)), true)
                    RatelBytes.u32be(out, position)

              val body = Array.collect[Byte](width*values.size + 8)(out => values.each(out.append(_)))
              buffer.putInt(slot.offset, grow(values.size, width, body).toInt)

            case Kind.Refs(scalar, intern) =>
              indexes.each: (index, number) =>
                mine.zip(List.range(0, mine.size)).each: (element, position) =>
                  keyed(number): out =>
                    RatelBytes.u32be(out, row)
                    RatelBytes.component(out, bytes(scalar, text(element)), false)
                    RatelBytes.u32be(out, position)

              val refs = mine.map(element => value(scalar, text(element)))

              val body = Array.collect[Byte](4*refs.size + 8): out =>
                refs.each(RatelBytes.u32(out, _))

              buffer.putInt(slot.offset, grow(refs.size, 4, body).toInt)

            case Kind.Record(table, _) => mine.head.let: element =>
              buffer.putInt(slot.offset, child(table, element, slot))
              mark(slot)

            // The rows of every child are reserved, and the list of them written, before any
            // child is filled.
            case Kind.Children(table, _) =>
              val numbers = mine.map(child(table, _, slot))

              val body = Array.collect[Byte](4*numbers.size + 8): out =>
                numbers.each(RatelBytes.u32(out, _))

              buffer.putInt(slot.offset, grow(numbers.size, 4, body).toInt)

              indexes.each: (index, number) =>
                mine.zip(numbers).each: (element, childRow) =>
                  keyed(number): out =>
                    RatelBytes.u32be(out, row)
                    index.components.each(component(out, _, node(element)))
                    RatelBytes.u32be(out, childRow)

            case Kind.Select(variants) => mine.head.let: element =>
              val (tag, payload) = choice(variants, element, slot)
              buffer.putInt(slot.offset, tag.toInt)
              buffer.putInt(slot.offset + 4, payload.toInt)
              mark(slot)

            case Kind.SelectList(variants) =>
              val pairs = mine.map(choice(variants, _, slot))

              val body = Array.collect[Byte](8*pairs.size + 8): out =>
                pairs.each: (tag, payload) =>
                  RatelBytes.u32(out, tag)
                  RatelBytes.u32(out, payload)

              buffer.putInt(slot.offset, grow(pairs.size, 8, body).toInt)

      fill(plan.entries, children)
      rows.get(table).set(row, Array.tabulate[Byte](plan.rowSize)(buffer.get(_)))

      List.range(0, pending.size).each: number =>
        val next = pending.get(number)
        emit(next.table, next.row, next.children, depth + 1, row, next.link)

    emit(0, allocate(0), node0(root), 0, RatelBytes.none, RatelBytes.unlinked)

    // §9.4: an entry or a separator as a cell holds it, after `head` bytes of the cell's own:
    // within the page if it is no longer than an eighth of one, and otherwise as a ref to an
    // item of the variable region.
    def cell(head: Int, bytes: Data): Int =
      if bytes.length <= pageSize/8 then (head + 2 + bytes.length + 3)/4*4 else head + 8

    // One page: its header, the pointers to its cells in the order of their keys, and the
    // cells themselves, placed from the end of the page downwards in that order.
    def page(kind: Int, link: Long, cells: List[(Long, Data)]): Data =
      val buffer = jn.ByteBuffer.allocate(pageSize).order(jn.ByteOrder.LITTLE_ENDIAN)
      val head = if kind == 2 then 4 else 0

      val start = cells.zip(List.range(0, cells.size)).fuse(pageSize):
        val ((child, bytes), number) = next
        val at = state - cell(head, bytes)
        buffer.putShort(16 + 2*number, at.toShort)
        if kind == 2 then buffer.putInt(at, child.toInt)

        if bytes.length <= pageSize/8 then
          buffer.putShort(at + head, bytes.length.toShort)
          List.range(0, bytes.length).each { index => buffer.put(at + head + 2 + index, bytes.u8(index).toByte) }
        else
          buffer.putShort(at + head, 0xffff.toShort)
          buffer.putInt(at + head + 4, vary(bytes).toInt)

        at

      buffer.put(0, kind.toByte)
      buffer.putShort(2, cells.size.toShort)
      buffer.putInt(4, start)
      buffer.putInt(8, link.toInt)
      Array.tabulate[Byte](pageSize)(buffer.get(_))

    // Fills pages from left to right, each with as many cells as fit: `head` is what a cell
    // holds besides its entry, and `first` whether the first thing given to a page is its
    // leftmost child, which takes no cell.
    def pack(items: List[(Long, Data)], head: Int, first: Boolean): List[Loading] =
      val groups: java.util.ArrayList[Loading] = java.util.ArrayList()

      @scala.annotation.tailrec
      def recur(items: List[(Long, Data)], current: Optional[Loading], room: Int): Unit =
        items match
          case Nil =>
            current.let(groups.add(_))

          case (child, bytes) :: rest =>
            val size = cell(head, bytes) + 2

            current.lay(recur(items, Loading(bytes, RatelBytes.none, Nil), pageSize - 16)): loading =>
              if first && loading.leftmost == RatelBytes.none then
                recur(rest, loading.copy(leftmost = child), room)
              else if size <= room then
                recur(rest, loading.copy(cells = (child, bytes) :: loading.cells), room - size)
              else
                groups.add(loading)
                recur(items, Unset, 0)

      recur(items, Unset, 0)
      List.range(0, groups.size).map(groups.get(_)).map { loading => loading.copy(cells = loading.cells.reverse) }

    // §9.7: an index, loaded from its entries in sorted order. The leaves are filled first,
    // then each level of branches over the level below, with the first entry beneath each
    // child but the leftmost as its separator, until one page remains: the root.
    def load(sorted: List[Data]): Loaded =
      if sorted.nil then Loaded(Nil, RatelBytes.none, 0, 0) else
        val pages: java.util.ArrayList[Data] = java.util.ArrayList()
        val leaves = pack(sorted.map((RatelBytes.none, _)), 0, false)

        leaves.zip(List.range(0, leaves.size)).each: (leaf, number) =>
          val link = if number + 1 < leaves.size then number + 1L else RatelBytes.none
          pages.add(page(1, link, leaf.cells))

        @scala.annotation.tailrec
        def raise(level: List[(Long, Data)], height: Int): Loaded =
          if level.size == 1 then
            Loaded(List.range(0, pages.size).map(pages.get(_)), pages.size - 1L, height, sorted.size)
          else
            val branches = pack(level, 4, true)

            val above = branches.map: branch =>
              pages.add(page(2, branch.leftmost, branch.cells))
              (pages.size - 1L, branch.first)

            raise(above, height + 1)

        raise(leaves.zip(List.range(0, leaves.size)).map((leaf, number) => (number.toLong, leaf.first)), 1)

    val trees: List[Loaded] = List.range(0, entries.size).map: number =>
      val sorted = entries.get(number)
      sorted.sort(bytewise)
      load(List.range(0, sorted.size).map(sorted.get(_).bytes))

    // The extents, each beginning at a multiple of eight, in the order of §10.5: the schema,
    // the strings, the variable region, the table directory and the rows of each table, the
    // index directory and the pages of each index, and the digests.
    val schemaSection = Array.collect[Byte](signature.length + schemaBody.length + 16): out =>
      out.appendVarint(signature.length)
      out.append(signature)
      out.appendVarint(schemaBody.length)
      out.append(schemaBody)
      RatelBytes.pad(out, 8)

    val tables: List[RatelLayout.Table] = layout.tables.to[List]
    val counts: List[Int] = List.range(0, tables.size).map(rows.get(_).size)
    val round8: Long => Long = size => (size + 7)/8*8

    val schemaAt: Long = RatelBytes.header
    val stringsAt: Long = schemaAt + schemaSection.length
    val stringsRoom: Long = round8(roomy(stringsSize))
    val variableAt: Long = stringsAt + stringsRoom
    val variableRoom: Long = round8(roomy(variableSize))
    val tablesAt: Long = variableAt + variableRoom

    // Where each table's rows lie, and how many it has room for; then likewise each index.
    val (rowsEnd, placedTables) = tables.zip(counts).fuse((tablesAt + 32L*tables.size, List[(Long, Long)]())):
      val (table, count) = next
      val (at, placed) = state
      val room = roomy(count)
      (at + room*table.rowSize, (at, room) :: placed)

    val indexesAt: Long = rowsEnd

    val (pagesEnd, placedTrees) = trees.fuse((indexesAt + 32L*trees.size, List[(Long, Long)]())):
      val (at, placed) = state
      val room = roomy(next.pages.size)
      (at + room*pageSize, (at, room) :: placed)

    val digestsAt: Long = pagesEnd
    val digesting = digests && sealing
    val committed: Long = digestsAt + (if digesting then RatelBytes.digests else 0)
    if committed > Int.MaxValue then fail(Reason.Oversized)

    val body = Array.collect[Byte]((digestsAt - RatelBytes.header).toInt): out =>
      def region(items: java.util.ArrayList[Data], room: Long): Unit =
        val before = out.mark
        List.range(0, items.size).each { number => out.append(items.get(number): Data) }
        RatelBytes.zeros(out, (room - (out.mark - before)).toInt)

      out.append(schemaSection)
      region(strings, stringsRoom)
      region(variable, variableRoom)

      tables.zip(counts).zip(placedTables.reverse).each:
        case ((table, count), (at, room)) =>
          RatelBytes.u64(out, at)
          RatelBytes.u32(out, count)
          RatelBytes.u32(out, room)
          RatelBytes.u32(out, table.rowSize)
          RatelBytes.u32(out, RatelBytes.none)
          RatelBytes.zeros(out, 8)

      tables.zip(List.range(0, tables.size)).zip(placedTables.reverse).each:
        case ((table, number), (at, room)) =>
          List.range(0, rows.get(number).size).each { row => out.append(rows.get(number).get(row): Data) }
          RatelBytes.zeros(out, ((room - rows.get(number).size)*table.rowSize).toInt)

      trees.zip(placedTrees.reverse).each:
        case (tree, (at, room)) =>
          RatelBytes.u64(out, at)
          RatelBytes.u32(out, tree.pages.size)
          RatelBytes.u32(out, room)
          RatelBytes.u32(out, tree.root)
          RatelBytes.u32(out, RatelBytes.none)
          RatelBytes.u32(out, tree.count)
          out.append(0.toByte)
          out.append(shift.toByte)
          out.append(tree.height.toByte)
          out.append(0.toByte)

      trees.zip(placedTrees.reverse).each:
        case (tree, (at, room)) =>
          tree.pages.each(out.append(_))
          RatelBytes.zeros(out, ((room - tree.pages.size)*pageSize).toInt)

    val flags: Int =
      if !sealing then 0
      else RatelBytes.sealing | RatelBytes.hashed | (if digesting then RatelBytes.digested else 0)

    val header = Array.collect[Byte](RatelBytes.header): out =>
      out.append(RatelBytes.magic)
      out.append(1.toByte)
      out.append(0.toByte)
      out.append(flags.toByte)
      out.append(0.toByte)
      RatelBytes.u64(out, committed)

      def entry(offset: Long, length: Long, capacity: Long): Unit =
        RatelBytes.u64(out, offset)
        RatelBytes.u64(out, length)
        RatelBytes.u64(out, capacity)

      entry(schemaAt, schemaSection.length, schemaSection.length)
      entry(stringsAt, stringsSize, stringsRoom)
      entry(variableAt, variableSize, variableRoom)
      entry(tablesAt, 32L*tables.size, 32L*tables.size)
      entry(indexesAt, 32L*trees.size, 32L*trees.size)
      entry(digestsAt, if digesting then RatelBytes.digests else 0, if digesting then RatelBytes.digests else 0)
      RatelBytes.u64(out, 0L)
      if sealing then out.append(valueHash) else RatelBytes.zeros(out, 32)

    Array.collect[Byte](committed.toInt): out =>
      out.append(header)
      out.append(body)

      if digesting then
        out.append(Blake3.hashOf(body, 32))
        out.append(Blake3.hashOf(header, 32))

  private def node0(root: Tel.Element)(using Tactic[Ratel.Error]): Array[Tel.Element]^{} =
    root match
      case Tel.Element.Node(_, _, children) => children
      case value: Tel.Element.Value         => fail(Ratel.Error.Reason.Unencodable)
