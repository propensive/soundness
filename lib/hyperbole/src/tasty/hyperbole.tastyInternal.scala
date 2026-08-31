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
package hyperbole

import scala.collection.mutable

import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyReader}

import anticipation.*
import digression.*
import gossamer.*
import vacuous.*

import StackTrace.Frame.Kind

// Readers for the three TASTy sections a stack frame needs. `TastyUnpickler` does the fiddly work
// of checking the header and decoding the name table (whose entries may be qualified, signed or
// otherwise derived), and hands out a plain reader positioned at each section's payload; from
// there the grammar in `TastyFormat`'s documentation is short enough to read directly, and doing
// so avoids the full tree unpickler, which would need a compiler `Context` and a classpath.
private[hyperbole] object stacksInternal:
  import TastyFormat.*

  // The line table pickled at the head of the positions section: the length of every line of the
  // source file. It is what makes the character offsets in that section resolvable to line
  // numbers without the source file itself being available.
  class Lines(sizes: Array[Int]^{}):
    private val starts: Array[Int]^{} =
      val array = new scala.Array[Int](sizes.length + 1)
      var index = 0

      while index < sizes.length do
        // The `+ 1` accounts for the line terminator, which the pickled size excludes.
        array(index + 1) = array(index) + 0.max(sizes.readUnchecked(index)) + 1
        index += 1

      Array.unsafeFrozen(array)

    def count: Int = sizes.length

    // The one-based line containing `offset`, which is what a stack trace reports.
    def line(offset: Int): Int =
      var min = 0
      var max = sizes.length - 1

      while min < max do
        val mid = (min + max + 1)/2
        if starts.readUnchecked(mid) <= offset then min = mid else max = mid - 1

      min + 1

  case class Positions(lines: Lines, spans: Map[Int, (Int, Int)])

  class PositionSection extends TastyUnpickler.SectionUnpickler[Positions](PositionsSection):
    def unpickle(reader: TastyReader, nameAtRef: TastyUnpickler.NameTable): Positions =
      import reader.*
      val count = readNat()
      val sizes = new scala.Array[Int](count)
      var index = 0

      while index < count do
        // Compilers before 3.3 could write `-1` here for a line of unknown length; treating it as
        // empty keeps every later line's offset monotonic, which is all the search below needs.
        val size = readLongNat()
        sizes(index) = if size == 0xFFFFFFFFL then 0 else size.toInt
        index += 1

      val spans = mutable.HashMap[Int, (Int, Int)]()
      var address = 0
      var start = 0
      var end = 0

      while !isAtEnd do
        val header = readInt()

        if header == SOURCE then readNameRef() else
          // Each field is a delta against the previously recorded node, and a node whose span
          // matches its parent's is omitted entirely.
          address += header >> 3
          if (header & 4) != 0 then start += readInt()
          if (header & 2) != 0 then end += readInt()
          if (header & 1) != 0 then readInt()
          spans(address) = (start, end)

      Positions(Lines(Array.unsafeFrozen(sizes)), spans.to(Map))

  class AttributeSection extends TastyUnpickler.SectionUnpickler[Optional[Text]](AttributesSection):
    def unpickle(reader: TastyReader, nameAtRef: TastyUnpickler.NameTable): Optional[Text] =
      import reader.*
      var path: Optional[Text] = Unset

      while !isAtEnd do
        val tag = readByte()

        if isStringAttrTag(tag) then
          val name = nameAtRef(readNameRef()).toString.tt
          if tag == SOURCEFILEattr then path = name

      path

  // Walks the tree section without building trees. Every tag falls into one of five categories
  // which fix what follows it, so the walk can step over any node it does not care about, and
  // needs to understand only the handful that introduce a name or a nesting level. This mirrors
  // the traversal in the compiler's own `TastyPrinter`.
  class DefinitionSection(positions: Positions)
  extends TastyUnpickler.SectionUnpickler[List[Tasty.Definition]](ASTsSection):

    def unpickle(reader: TastyReader, nameAtRef: TastyUnpickler.NameTable)
    :   List[Tasty.Definition] =

      import reader.*
      val definitions = mutable.ListBuffer[Tasty.Definition]()

      def label(): Text = nameAtRef(readNameRef()).toString.tt

      // Steps over a number without caring what it means. The widest reader is the right one:
      // every variable-length integer in TASTy occupies bytes up to the first with its high bit
      // set, so this consumes exactly as much as a narrower reader would, but it also accepts the
      // 64-bit payloads of `LONGconst` and `DOUBLEconst`, which a `Nat` reader rejects outright.
      def number(): Unit =
        val _ = readLongInt()

      def record(address: Int, tag: Int, name: Text, owners: List[Text], extension: Boolean)
      :   Unit =

        positions.spans.stdlib.get(address).foreach: (start, end) =>
          val kind = tag match
            case TYPEDEF                   => Kind.Class
            case VALDEF                    => Kind.Value
            case _ if extension            => Kind.Extension
            case _ if name == t"$$anonfun" => Kind.Lambda
            case _                         => Kind.Method

          val first = positions.lines.line(start)
          val last = positions.lines.line(end)
          definitions += Tasty.Definition(name, owners, kind, start, end, first, last)

      def walk(owners: List[Text]): Unit =
        val address = currentAddr.index
        val tag = readByte()

        def children(end: TastyBuffer.Addr, owners: List[Text]): Unit =
          while currentAddr.index < end.index do walk(owners)

        if tag >= firstLengthTreeTag then
          // The length counts from after itself, so the address has to be taken once it has been
          // read, not while reading it.
          val length = readNat()
          val end = currentAddr + length

          tag match
            case VALDEF | DEFDEF | TYPEDEF =>
              val name = label()
              val inner = name :: owners
              var extension = false

              // A top-level extension method keeps its own name when compiled, so nothing in a
              // stack frame marks it as one; the modifier the compiler recorded here does. The
              // modifiers follow the definition's other children, so this waits for them.
              while currentAddr.index < end.index do
                if nextByte == EXTENSION then extension = true
                walk(inner)

              record(address, tag, name, owners, extension)

            case PACKAGE =>
              // Qualifying definitions by their package costs nothing here: the package path is
              // the first child. Nested package nodes each name themselves in full, so the
              // innermost one is the whole path and replaces, rather than extends, what is known
              // so far; and packages only ever enclose, never nest inside a definition, so there
              // is nothing else in `owners` to lose.
              val inner =
                if currentAddr.index < end.index && nextByte == TERMREFpkg
                then
                  readByte()
                  val name = label()
                  if name == t"<empty>" then Nil else List(name)
                else
                  owners

              children(end, inner)

            case TYPEPARAM | PARAM | NAMEDARG | BIND =>
              readNameRef()
              children(end, owners)

            case REFINEDtype | TERMREFin | TYPEREFin | SELECTin =>
              readNameRef()
              walk(owners)
              children(end, owners)

            case RENAMED =>
              readNameRef()
              readNameRef()

            case RETURN | HOLE =>
              readNat()
              children(end, owners)

            case METHODtype | POLYtype | TYPELAMBDAtype =>
              walk(owners)

              while currentAddr.index < end.index && !isModifierTag(nextByte) do
                walk(owners)
                readNameRef()

              children(end, owners)

            case PARAMtype =>
              readNat()
              readNat()

            case _ =>
              children(end, owners)

          // A tag this walk mis-reads would derail everything after it; resynchronizing on the
          // recorded length confines the damage to the node itself.
          if currentAddr.index != end.index then goto(end)

        else if tag >= firstNatASTTreeTag then
          tag match
            case IDENT | IDENTtpt | SELECT | SELECTtpt | TERMREF | TYPEREF | SELFDEF =>
              readNameRef()

            case _ =>
              number()

          walk(owners)

        else if tag >= firstASTTreeTag then
          walk(owners)

        else if tag >= firstNatTreeTag then
          tag match
            case TERMREFpkg | TYPEREFpkg | STRINGconst | IMPORTED => readNameRef()
            case _                                                => number()

      while !isAtEnd do walk(Nil)

      definitions.to(List)
