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

import java.nio.charset.StandardCharsets

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

import dotty.tools.tasty.{TastyBuffer, TastyFormat, TastyHeaderUnpickler, TastyReader,
    UnpickleException}

import anticipation.*
import digression.*
import gossamer.*
import rudiments.at
import vacuous.*

import StackTrace.Frame.Kind

// Readers for the three TASTy sections a stack frame needs, over `tasty-core` alone: the small,
// compiler-free library that holds the format's constants, the byte reader and the header check,
// and which is published for Scala.js and Scala Native as well as the JVM. What the compiler's
// `TastyUnpickler` adds — decoding the name table, whose entries may be qualified, unique, signed
// or otherwise derived, and handing out a reader positioned at each section's payload — is
// reproduced here in `Unpickler`, rendering every name straight to `Text` instead of to the
// compiler's `Name` objects. From there the grammar in `TastyFormat`'s documentation is short
// enough to read directly, which avoids the full tree unpickler and its `Context` and classpath.
private[hyperbole] object stacksInternal:
  import TastyFormat.*
  import TastyFormat.NameTags.*

  // The name table: every name a file's sections refer to, by index, already rendered.
  class Names(entries: IndexedSeq[Text]):
    def apply(ref: TastyBuffer.NameRef): Text = entries(ref.index)

  // A reader of one named section's payload.
  abstract class Section[result](val name: Text):
    def unpickle(reader: TastyReader, names: Names): result

  object Unpickler:
    private val dollar: Text = t"$$"

    // Reads the header and the name table, then indexes the sections, in the order the file lays
    // them out. Anything malformed — a file that is not TASTy, a version this reader cannot read,
    // a name of a kind it does not know — ends in an exception, which the caller treats as having
    // no TASTy at all.
    def apply(bytes: scala.Array[Byte]^{}): Unpickler =
      val reader = TastyReader(bytes)
      TastyHeaderUnpickler(reader).readFullHeader()
      val names = readNames(reader)
      new Unpickler(names, readSections(reader, names))

    // Each name is a tag, a length and a payload, and may refer back to names already read. The
    // renderings follow the compiler's `NameKinds` — `prefix.name`, `name$1`, `method$default$2`,
    // `super$name` and so on — so that what a stack frame shows can be matched against them. A
    // signed name is rendered as the name it signs: the signature only disambiguates overloads,
    // which the walk here never needs to tell apart.
    private def readNames(reader: TastyReader): Names =
      import reader.*
      val entries = mutable.ArrayBuffer[Text]()

      def name(): Text = entries(readNameRef().index)

      def entry(): Text =
        val tag = readByte()
        val length = readNat()
        val start = currentAddr
        val end = start + length

        val result = tag match
          case UTF8 =>
            goto(end)
            String(bytes, start.index, length, StandardCharsets.UTF_8).tt

          case QUALIFIED =>
            val prefix = name()
            t"$prefix.${name()}"

          case EXPANDED =>
            val prefix = name()
            t"$prefix$dollar$dollar${name()}"

          case EXPANDPREFIX =>
            val prefix = name()
            t"$prefix$dollar${name()}"

          case UNIQUE =>
            val separator = name()
            val number = readNat()
            val originals = until(end)(name())

            // A fresh name with nothing to derive from, `$3$`, is the one shape the compiler
            // renders differently from `original`, separator, number.
            if originals.isEmpty && separator == dollar then t"$dollar$number$dollar"
            else t"${originals.headOption.getOrElse(t"")}$separator$number"

          case DEFAULTGETTER =>
            val method = name()
            val number = readNat()

            val prefix =
              if method == t"<init>" then t"${dollar}lessinit${dollar}greater" else method

            t"$prefix${dollar}default$dollar${number + 1}"

          case SIGNED | TARGETSIGNED =>
            val original = name()
            goto(end)
            original

          case SUPERACCESSOR  => t"super$dollar${name()}"
          case INLINEACCESSOR => t"inline$dollar${name()}"
          case BODYRETAINER   => t"${name()}${dollar}retainedBody"
          case OBJECTCLASS    => t"${name()}$dollar"
          case other          => throw UnpickleException(s"unknown name tag $other")

        if currentAddr != end then throw UnpickleException(s"name of tag $tag has the wrong length")
        result

      until(readEnd())(entries += entry())
      Names(entries.toIndexedSeq)

    // After the name table, the rest of the file is sections: a name, a length and a payload. Each
    // gets a reader confined to its payload, addressed from the payload's start as the format's
    // address-bearing sections expect.
    private def readSections(reader: TastyReader, names: Names): Map[Text, TastyReader] =
      import reader.*

      collectWhile(!isAtEnd):
        val name = names(readNameRef())
        val end = readEnd()
        val section = TastyReader(bytes, currentAddr.index, end.index, currentAddr.index)
        goto(end)
        name -> section

      . to(Map)

  // A TASTy file whose header has been checked, name table decoded and sections located, ready to
  // be read a section at a time. A section a file does not have reads as `Unset`.
  class Unpickler(names: Names, sections: Map[Text, TastyReader]):
    def unpickle[result](section: Section[result]): Optional[result] =
      sections.at(section.name).let(section.unpickle(_, names))

  // The line table pickled at the head of the positions section: the length of every line of the
  // source file. It is what makes the character offsets in that section resolvable to line
  // numbers without the source file itself being available.
  class Lines(sizes: Array[Int]^{}):
    private val starts: Array[Int]^{} =
      val array = Array.allocate[Int](sizes.length + 1)
      var index = 0

      while index < sizes.length do
        // The `+ 1` accounts for the line terminator, which the pickled size excludes.
        array(index + 1) = array(index) + 0.max(sizes.readUnchecked(index)) + 1
        index += 1

      Array.freeze(array)

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

  class PositionSection extends Section[Positions](PositionsSection.tt):
    def unpickle(reader: TastyReader, names: Names): Positions =
      import reader.*
      val count = readNat()
      val sizes = Array.allocate[Int](count)
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

      Positions(Lines(Array.freeze(sizes)), spans.to(Map))

  class AttributeSection extends Section[Optional[Text]](AttributesSection.tt):
    def unpickle(reader: TastyReader, names: Names): Optional[Text] =
      import reader.*
      var path: Optional[Text] = Unset

      while !isAtEnd do
        val tag = readByte()

        if isStringAttrTag(tag) then
          val name = names(readNameRef())
          if tag == SOURCEFILEattr then path = name

      path

  // Walks the tree section without building trees. Every tag falls into one of five categories
  // which fix what follows it, so the walk can step over any node it does not care about, and
  // needs to understand only the handful that introduce a name or a nesting level. This mirrors
  // the traversal in the compiler's own `TastyPrinter`.
  class DefinitionSection(positions: Positions)
  extends Section[List[Tasty.Definition]](ASTsSection.tt):

    def unpickle(reader: TastyReader, names: Names): List[Tasty.Definition] =
      import reader.*
      val definitions = mutable.ListBuffer[Tasty.Definition]()

      def label(): Text = names(readNameRef())

      // Steps over a number without caring what it means. The widest reader is the right one:
      // every variable-length integer in TASTy occupies bytes up to the first with its high bit
      // set, so this consumes exactly as much as a narrower reader would, but it also accepts the
      // 64-bit payloads of `LONGconst` and `DOUBLEconst`, which a `Nat` reader rejects outright.
      def number(): Unit =
        val _ = readLongInt()

      def record(address: Int, tag: Int, name: Text, owners: List[Text], extension: Boolean)
      :   Unit =

        positions.spans.at(address).lay(()): (start, end) =>
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
