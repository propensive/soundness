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

// The canonical writer of RaTEL §10: rows are allocated a table at a time as the semantic
// model is walked, the children of a repeatable member being reserved together before any of
// them is filled, so that they stay contiguous whatever their own children allocate; variable
// items are appended as they are met; and the indexes are sorted once every row is known.
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

  // A row still to be filled, from the children of the node which it stores.
  private case class Pending(table: Int, row: Int, children: Array[Tel.Element]^{})

  def write(root: Tel.Element, declared: Tels, codecs: Tel.Codec.Bindings, digests: Boolean)
    ( using Tactic[Tel.Error], Tactic[Ratel.Error] )
  :   Data =

    import Ratel.Error.Reason

    val axiom = Tels.Axiom.tels
    val schema = Tels.Layers.compose(declared)
    val layout = RatelLayout(schema)

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

      // R22: a declared width is a promise about every value, never an instruction to pad.
      RatelLayout.width(scalar).let: width =>
        if bytes.length != width then fail(Reason.WidthMismatch)

      bytes

    def vary(header: Long, body: Data): Long =
      val ref = variableSize
      val item = RatelBytes.item(header, body)
      variable.add(item)
      variableSize += item.length
      if variableSize > Int.MaxValue then fail(Reason.Oversized)
      ref

    // A value item: in the string table, each distinct value once, if the scalar is interned,
    // and in the variable region otherwise.
    def value(scalar: Tels.Scalar, text: Text): Long =
      val body = bytes(scalar, text)

      if !scalar.intern then vary(body.length, body) else
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

    def emit(table: Int, row: Int, children: Array[Tel.Element]^{}, depth: Int): Unit =
      if depth > 256 then fail(Reason.NestingLimitExceeded)
      val plan = layout.table(table).or(fail(Reason.Unencodable))
      val buffer = jn.ByteBuffer.allocate(plan.rowSize).order(jn.ByteOrder.LITTLE_ENDIAN)
      val pending: java.util.ArrayList[Pending] = java.util.ArrayList()

      def mark(slot: Slot): Unit = if slot.presence >= 0 then
        val position = slot.presence >> 3
        buffer.put(position, (buffer.get(position) | 1 << (slot.presence & 7)).toByte)

      def node(element: Tel.Element): Array[Tel.Element]^{} = element match
        case Tel.Element.Node(_, _, children) => children
        case value: Tel.Element.Value         => fail(Reason.Unencodable)

      def text(element: Tel.Element): Text = element match
        case Tel.Element.Value(_, _, text) => text
        case node: Tel.Element.Node        => fail(Reason.Unencodable)

      def child(table: Int, element: Tel.Element): Int =
        val number = allocate(table)
        pending.add(Pending(table, number, node(element)))
        number

      // A select's tag and payload: the keyword index of the variant chosen, and what it
      // refers to.
      def choice(variants: List[RatelLayout.Variant], element: Tel.Element): (Long, Long) =
        val variant = variants.seek(_.keywordIndex == index(element)).or(fail(Reason.Unencodable))

        val payload: Long = variant.payload match
          case Payload.Record(table, _)      => child(table, element)
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
              buffer.putInt(slot.offset, vary(values.size, body).toInt)

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

              buffer.putInt(slot.offset, vary(refs.size, body).toInt)

            case Kind.Record(table, _) => mine.head.let: element =>
              buffer.putInt(slot.offset, child(table, element))
              mark(slot)

            // The rows of every child are reserved before any child is filled.
            case Kind.Range(table, _) =>
              val start = rows.get(table).size
              val numbers = mine.map(child(table, _))
              buffer.putInt(slot.offset, if mine.nil then 0 else start)
              buffer.putInt(slot.offset + 4, mine.size)

              indexes.each: (index, number) =>
                mine.zip(numbers).each: (element, childRow) =>
                  keyed(number): out =>
                    RatelBytes.u32be(out, row)
                    index.components.each(component(out, _, node(element)))
                    RatelBytes.u32be(out, childRow)

            case Kind.Select(variants) => mine.head.let: element =>
              val (tag, payload) = choice(variants, element)
              buffer.putInt(slot.offset, tag.toInt)
              buffer.putInt(slot.offset + 4, payload.toInt)
              mark(slot)

            case Kind.SelectList(variants) =>
              val pairs = mine.map(choice(variants, _))

              val body = Array.collect[Byte](8*pairs.size + 8): out =>
                pairs.each: (tag, payload) =>
                  RatelBytes.u32(out, tag)
                  RatelBytes.u32(out, payload)

              buffer.putInt(slot.offset, vary(pairs.size, body).toInt)

      fill(plan.entries, children)
      rows.get(table).set(row, Array.tabulate[Byte](plan.rowSize)(buffer.get(_)))

      List.range(0, pending.size).each: number =>
        val next = pending.get(number)
        emit(next.table, next.row, next.children, depth + 1)

    emit(0, allocate(0), node0(root), 0)

    // The sections, each a multiple of eight bytes, in the order of the directory.
    val schemaSection = Array.collect[Byte](signature.length + schemaBody.length + 16): out =>
      out.appendVarint(signature.length)
      out.append(signature)
      out.appendVarint(schemaBody.length)
      out.append(schemaBody)
      RatelBytes.pad(out, 8)

    def region(items: java.util.ArrayList[Data], size: Long): Data =
      Array.collect[Byte](size.toInt + 8): out =>
        List.range(0, items.size).each: number =>
          val item: Data = items.get(number)
          out.append(item)

        RatelBytes.pad(out, 8)

    val tables: List[RatelLayout.Table] = layout.tables.to[List]
    val counts: List[Int] = List.range(0, tables.size).map(rows.get(_).size)
    val rowBytes: Long = tables.zip(counts).fuse(0L)(state + next(0).rowSize.toLong*next(1))
    if 24L*tables.size + rowBytes > Int.MaxValue then fail(Reason.Oversized)

    val tablesSection = Array.collect[Byte]((24*tables.size + rowBytes).toInt): out =>
      tables.zip(counts).fuse(24L*tables.size):
        val (table, count) = next
        RatelBytes.u64(out, state)
        RatelBytes.u32(out, count)
        RatelBytes.u32(out, table.rowSize)
        RatelBytes.zeros(out, 8)
        state + table.rowSize.toLong*count

      List.range(0, tables.size).each: table =>
        List.range(0, rows.get(table).size).each: row =>
          val bytes: Data = rows.get(table).get(row)
          out.append(bytes)

    // Each index is its entries, sorted bytewise, behind the offsets at which they begin.
    val blocks: List[(Int, Data)] = List.range(0, entries.size).map: number =>
      val sorted = entries.get(number)
      sorted.sort(bytewise)
      val all: List[Data] = List.range(0, sorted.size).map(sorted.get(_).bytes)
      val size = all.fuse(0L)(state + next.length)
      if 4L*(all.size + 1) + size > Int.MaxValue then fail(Reason.Oversized)

      val block = Array.collect[Byte]((4*(all.size + 1) + size).toInt + 8): out =>
        RatelBytes.u32(out, all.fuse(0L) { RatelBytes.u32(out, state); state + next.length })
        all.each(out.append(_))
        RatelBytes.pad(out, 8)

      (all.size, block)

    val indexesSection = Array.collect[Byte](64): out =>
      blocks.fuse(16L*blocks.size):
        val (count, block) = next
        RatelBytes.u64(out, state)
        RatelBytes.u32(out, count)
        RatelBytes.zeros(out, 4)
        state + block.length

      blocks.each((count, block) => out.append(block))

    val sections: List[Data] =
      List
        ( schemaSection, region(strings, stringsSize), region(variable, variableSize),
          tablesSection, indexesSection )

    val body: Long = sections.fuse(0L)(state + next.length)
    val committed: Long = RatelBytes.header + body + (if digests then RatelBytes.digests else 0)
    if committed > Int.MaxValue then fail(Reason.Oversized)

    val header = Array.collect[Byte](RatelBytes.header): out =>
      out.append(RatelBytes.magic)
      out.append(1.toByte)
      out.append(0.toByte)
      out.append((if digests then 3 else 2).toByte)
      out.append(0.toByte)
      RatelBytes.u64(out, committed)

      val end = sections.fuse(RatelBytes.header.toLong):
        RatelBytes.u64(out, state)
        RatelBytes.u64(out, next.length)
        state + next.length

      RatelBytes.u64(out, end)
      RatelBytes.u64(out, if digests then RatelBytes.digests else 0)
      out.append(valueHash)

    Array.collect[Byte](committed.toInt): out =>
      out.append(header)
      sections.each(out.append(_))

      if digests then
        sections.each(section => out.append(Blake3.hashOf(section, 32)))
        out.append(Blake3.hashOf(header, 32))

  private def node0(root: Tel.Element)(using Tactic[Ratel.Error]): Array[Tel.Element]^{} =
    root match
      case Tel.Element.Node(_, _, children) => children
      case value: Tel.Element.Value         => fail(Ratel.Error.Reason.Unencodable)
