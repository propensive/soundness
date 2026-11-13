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
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import gossamer.*
import rudiments.*
import symbolism.*
import vacuous.*

// The layout of a RaTEL file (RaTEL §6): for every table -- the document root, then each
// record definition of the composed schema -- the slot of each member, its offset in a row,
// and the size of a row. A layout is a pure function of the schema, computed alike by every
// reader and writer, which is why none of it is stored in a file.
private[stratiform] object RatelLayout:
  // What a select's `payload` word refers to, by the resolved type of the chosen variant.
  enum Payload:
    case Record(table: Int, struct: Tels.Struct)
    case Value(scalar: Tels.Scalar, intern: Boolean)
    case Flag

  case class Variant(keywordIndex: Int, keyword: Text, payload: Payload)

  // The slot kinds of §6.1. A flattened struct has no slot of its own: its members' slots are
  // laid into the row of the struct that holds it.
  enum Kind:
    case Always
    case Flag
    case FlagCount
    case Inline(scalar: Tels.Scalar, width: Int)
    case Ref(scalar: Tels.Scalar, intern: Boolean)
    case Packed(scalar: Tels.Scalar, width: Int)
    case Refs(scalar: Tels.Scalar, intern: Boolean)
    case Record(table: Int, struct: Tels.Struct)
    case Range(table: Int, struct: Tels.Struct)
    case Select(variants: List[Variant])
    case SelectList(variants: List[Variant])

    def size: Int = this match
      case Always | Flag           => 0
      case Inline(_, width)        => width
      case Range(_, _) | Select(_) => 8
      case _                       => 4

    // The largest power of two not exceeding the lesser of the size and eight.
    def align: Int = this match
      case Inline(_, width) => Integer.highestOneBit(width.min(8))
      case _                => 4

  // One slot of a row: `number` is its position in slot order, `presence` the number of its
  // bit in the row's presence bitmap, or -1 if it has none, and `keywordIndex` the keyword
  // index of its member in the struct which declares it -- for a select, of its first variant.
  case class Slot
    ( number:       Int,
      keywordIndex: Int,
      keyword:      Text,
      required:     Boolean,
      kind:         Kind,
      offset:       Int,
      presence:     Int,
      indexes:      List[Tels.Index] )

  // A member of a struct as it is stored: in a slot of the row, or flattened into the row as
  // the members of the struct it holds.
  enum Entry:
    case Stored(slot: Slot)
    case Flattened(keywordIndex: Int, keyword: Text, struct: Tels.Struct, entries: List[Entry])

  case class Table
    ( name:        Optional[Text],
      struct:      Tels.Struct,
      entries:     List[Entry],
      slots:       List[Slot],
      bitmapBytes: Int,
      rowSize:     Int )

  // One component of an index key (§9.2), as it is found from a child: `keywords` are the
  // keyword indices to follow through the child's semantic model, and `hops` the record slots
  // to follow from the child's row to the row which holds `last`, the slot of the scalar.
  case class Component(keywords: List[Int], hops: List[Slot], last: Slot):
    def width: Optional[Int] = last.kind match
      case Kind.Inline(_, width) => width
      case _                     => Unset

    def scalar: Tels.Scalar = last.kind match
      case Kind.Inline(scalar, _) => scalar
      case Kind.Ref(scalar, _)    => scalar
      case _                      => Tels.Scalar(Array.empty)

  // An index (§9.1): over the children of the `Range` slot `slot` of table `table`, keyed by
  // `components`, or over the elements of an array slot, keyed by the elements themselves.
  case class Index
    ( table: Int, slot: Slot, primary: Boolean, paths: List[Text], components: List[Component] ):

    def child: Optional[Int] = slot.kind match
      case Kind.Range(table, _) => table
      case _                    => Unset

    // The width of an element of an indexed array, if it is fixed.
    def element: Optional[Tels.Scalar] = slot.kind match
      case Kind.Packed(scalar, _) => scalar
      case Kind.Refs(scalar, _)   => scalar
      case _                      => Unset

    def elementWidth: Optional[Int] = slot.kind match
      case Kind.Packed(_, width) => width
      case _                     => Unset

  case class Layout(schema: Tels, tables: Array[Table]^{}, indexes: Array[Index]^{}):
    def table(number: Int): Optional[Table] = if number < 0 then Unset else tables.at(number.z)

  // The fixed width of a scalar's values, if it has one: that of a standard codec, or the
  // width its definition declares.
  def width(scalar: Tels.Scalar): Optional[Int] =
    scalar.encoding.let(StandardCodecs.width(_)).or(scalar.width)

  private def builtin(name: Text): Boolean =
    name == Tels.Builtin.String || name == Tels.Builtin.Identifier
    || name == Tels.Builtin.TypeName || name == Tels.Builtin.Sigil

  // A member's type, resolved through the composed namespace.
  private enum Resolved:
    case Record(table: Int, name: Text, struct: Tels.Struct)
    case Anonymous(struct: Tels.Struct)
    case Value(scalar: Tels.Scalar)
    case Flag

  private def unlayoutable(using Tactic[Ratel.Error]): Nothing =
    import errorDiagnostics.emptyDiagnostics
    abort(Ratel.Error(Ratel.Error.Reason.SchemaUnlayoutable))

  def apply(schema: Tels): Layout raises Ratel.Error =
    val records: List[Tels.RecordDefinition] = schema.records.to[List]
    val numbered: List[(Tels.RecordDefinition, Int)] = records.zip(List.range(1, records.size + 1))

    def resolve(fieldType: Tels.Type): Resolved = fieldType match
      case struct: Tels.Struct => Resolved.Anonymous(struct)
      case scalar: Tels.Scalar => Resolved.Value(scalar)
      case Tels.Flag           => Resolved.Flag

      case Tels.Reference(name) =>
        numbered.seek(_(0).name == name).lay
          ( schema.scalars.seek(_.name == name).lay
              ( if builtin(name) then Resolved.Value(Tels.Scalar(Array.empty))
                else if name == Tels.Builtin.Flag then Resolved.Flag
                else unlayoutable )
              ( definition => Resolved.Value(definition.scalar) ) )
          ( (record, table) =>
              Resolved.Record(table, record.name, Tels.Struct(record.members, record.validators)) )

    // `intern` has no meaning for a value of fixed width (R11).
    def fixed(scalar: Tels.Scalar): Optional[Int] =
      width(scalar).let: width =>
        if scalar.intern || width < 1 then unlayoutable else width

    def variants(reference: Text, base: Int): List[Variant] =
      val select = schema.selects.seek(_.name == reference).or(unlayoutable)
      val all: List[Tels.Variant] = select.variants.to[List]

      all.zip(List.range(base, base + all.size)).map: (variant, keywordIndex) =>
        val payload = resolve(variant.variantType) match
          case Resolved.Record(table, _, struct) => Payload.Record(table, struct)
          case Resolved.Anonymous(_)             => unlayoutable
          case Resolved.Value(scalar)            => fixed(scalar); Payload.Value(scalar, scalar.intern)
          case Resolved.Flag                     => Payload.Flag

        Variant(keywordIndex, variant.keyword, payload)

    // A slot before its offset is known, and an entry which refers to slots by number.
    case class Draft
      ( keywordIndex: Int, keyword: Text, required: Boolean, kind: Kind, optional: Boolean,
        indexes: List[Tels.Index] )

    enum Sketch:
      case Stored(number: Int)
      case Flattened(keywordIndex: Int, keyword: Text, struct: Tels.Struct, entries: List[Sketch])

    def layout(name: Optional[Text], struct: Tels.Struct, stack: List[Text]): Table =
      val drafts: java.util.ArrayList[Draft] = java.util.ArrayList()

      def store(draft: Draft): Sketch =
        drafts.add(draft)
        Sketch.Stored(drafts.size - 1)

      // Members are visited in member order, in which a select is one member with one slot;
      // `base` counts the keyword indices which the members before have taken.
      def walk(members: List[Tels.Member], base: Int, stack: List[Text], depth: Int)
      :   List[Sketch] =

        if depth > 256 then unlayoutable

        members match
          case Nil => Nil

          case (field: Tels.Field) :: rest =>
            val required = field.required != Tels.Polarity.Loose
            val repeatable = field.repeatable == Tels.Polarity.Loose
            val optional = !required && !repeatable

            def slot(kind: Kind, presence: Boolean): Sketch =
              store(Draft(base, field.keyword, required, kind, presence, field.indexes))

            val sketch = resolve(field.fieldType) match
              case Resolved.Flag =>
                if repeatable then slot(Kind.FlagCount, false)
                else if required then slot(Kind.Always, false)
                else slot(Kind.Flag, true)

              case Resolved.Value(scalar) =>
                fixed(scalar).lay
                  ( if repeatable then slot(Kind.Refs(scalar, scalar.intern), false)
                    else slot(Kind.Ref(scalar, scalar.intern), optional) )
                  ( width =>
                      if repeatable then slot(Kind.Packed(scalar, width), false)
                      else slot(Kind.Inline(scalar, width), optional) )

              case Resolved.Record(table, name, inner) =>
                if repeatable then slot(Kind.Range(table, inner), false)
                else if !required then slot(Kind.Record(table, inner), true)
                else if stack.has(name) then slot(Kind.Record(table, inner), false)
                else
                  val entries = walk(inner.members.to[List], 0, name :: stack, depth + 1)
                  Sketch.Flattened(base, field.keyword, inner, entries)

              // A struct which no definition names has no table, so can only be flattened.
              case Resolved.Anonymous(inner) =>
                if repeatable || !required then unlayoutable else
                  val entries = walk(inner.members.to[List], 0, stack, depth + 1)
                  Sketch.Flattened(base, field.keyword, inner, entries)

            sketch :: walk(rest, base + 1, stack, depth)

          case (select: Tels.SelectRef) :: rest =>
            val required = select.required != Tels.Polarity.Loose
            val repeatable = select.repeatable == Tels.Polarity.Loose
            val all = variants(select.reference, base)
            val kind = if repeatable then Kind.SelectList(all) else Kind.Select(all)
            val draft = Draft(base, select.reference, required, kind, !required && !repeatable, Nil)
            store(draft) :: walk(rest, base + all.size, stack, depth)

          case (exclude: Tels.Exclude) :: rest =>
            walk(rest, base, stack, depth)

      val sketches = walk(struct.members.to[List], 0, stack, 0)
      val count = drafts.size
      val numbers: List[Int] = List.range(0, count)
      val presences: List[Int] = numbers.filter(drafts.get(_).optional)
      val bitmapBytes = (presences.size + 7)/8
      val bits: java.util.HashMap[Int, Int] = java.util.HashMap()
      presences.zip(List.range(0, presences.size)).each { (number, bit) => bits.put(number, bit) }

      // Slots are packed by descending alignment, each pass in slot order.
      val offsets: java.util.HashMap[Int, Int] = java.util.HashMap()

      val end = List(8, 4, 2, 1).fuse(bitmapBytes):
        val align = next

        numbers.fuse(state):
          val kind = drafts.get(next).kind

          if kind.size == 0 || kind.align != align then state else
            val offset = (state + align - 1)/align*align
            offsets.put(next, offset)
            offset + kind.size

      val slots: List[Slot] = numbers.map: number =>
        val draft = drafts.get(number)
        val offset = if offsets.containsKey(number) then offsets.get(number).toInt else 0

        Slot
          ( number, draft.keywordIndex, draft.keyword, draft.required, draft.kind, offset,
            if bits.containsKey(number) then bits.get(number).toInt else -1, draft.indexes )

      val indexed: java.util.ArrayList[Slot] = java.util.ArrayList()
      slots.each(indexed.add(_))

      def entries(sketches: List[Sketch]): List[Entry] = sketches.map:
        case Sketch.Stored(number) =>
          Entry.Stored(indexed.get(number))

        case Sketch.Flattened(keywordIndex, keyword, struct, inner) =>
          Entry.Flattened(keywordIndex, keyword, struct, entries(inner))

      Table(name, struct, entries(sketches), slots, bitmapBytes, (end + 7)/8*8)

    val root = layout(Unset, schema.document, Nil)

    val tables: List[Table] = root :: records.map: record =>
      layout(record.name, Tels.Struct(record.members, record.validators), List(record.name))

    val arranged: Array[Table]^{} = tables.to[Array]

    // A static index path (§9.1): each component names a field which is not repeatable, every
    // one but the last a struct and the last a scalar.
    def component(entries: List[Entry], path: List[Text], keywords: List[Int], hops: List[Slot])
    :   Component =

      path match
        case Nil => unlayoutable

        case name :: rest =>
          val entry = entries.seek:
            case Entry.Stored(slot)                 => slot.keyword == name
            case Entry.Flattened(_, keyword, _, _)  => keyword == name

          entry.or(unlayoutable) match
            case Entry.Flattened(keywordIndex, _, _, inner) =>
              component(inner, rest, keywordIndex :: keywords, hops)

            case Entry.Stored(slot) => slot.kind match
              case Kind.Record(table, _) =>
                val inner = arranged.at(table.z).or(unlayoutable).entries
                component(inner, rest, slot.keywordIndex :: keywords, slot :: hops)

              case Kind.Inline(_, _) | Kind.Ref(_, _) =>
                if !rest.nil then unlayoutable
                else Component((slot.keywordIndex :: keywords).reverse, hops.reverse, slot)

              case _ =>
                unlayoutable

    def key(child: Table): Optional[Component] =
      val keyword: Optional[Text] =
        child.struct.members.to[List].sweep { case field: Tels.Field if field.key => field.keyword }
        . head

      keyword.let: keyword => component(child.entries, List(keyword), Nil, Nil)

    val indexes: List[Index] =
      tables.zip(List.range(0, tables.size)).bind: (table, number) =>
        table.slots.bind: slot =>
          slot.kind match
            case Kind.Range(childNumber, _) =>
              val child = arranged.at(childNumber.z).or(unlayoutable)
              val primary: List[Index] =
                key(child).lay(Nil)(key => List(Index(number, slot, true, Nil, List(key))))


              val secondary = slot.indexes.map: index =>
                val components = index.paths.map: path =>
                  val telp = safely(Telp.parse(path)).or(unlayoutable)
                  component(child.entries, telp.components, Nil, Nil)

                Index(number, slot, false, index.paths, components)

              primary + secondary

            case Kind.Packed(_, _) | Kind.Refs(_, _) =>
              slot.indexes.map: index =>
                index.paths.each: path =>
                  if !safely(Telp.parse(path)).or(unlayoutable).components.nil then unlayoutable

                Index(number, slot, false, index.paths, Nil)

            case _ =>
              if !slot.indexes.nil then unlayoutable else Nil

    Layout(schema, arranged, indexes.to[Array])
