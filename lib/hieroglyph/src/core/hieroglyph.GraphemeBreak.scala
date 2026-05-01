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
package hieroglyph

import java.io as ji

import scala.collection.mutable.ArrayBuilder

import anticipation.*
import contingency.*
import fulminate.*
import kaleidoscope.*
import rudiments.*
import vacuous.*

object GraphemeBreak:
  enum Property:
    case
      Other, Cr, Lf, Control, Extend, Zwj, RegionalIndicator, Prepend, SpacingMark, L, V, T, Lv, Lvt

  private object Hex:
    def unapply(text: Text): Option[Int] =
      try Some(Integer.parseInt(text.s, 16)) catch case _: NumberFormatException => None

  private case class Entry(start: Int, end: Int, prop: Int)

  private def loadResource(path: String, fallbackUrl: String): ji.InputStream =
    Optional(getClass.getResourceAsStream(path)).or:
      safely:
        val uri = new java.net.URI(fallbackUrl)
        uri.toURL().nn.openStream().nn: ji.InputStream

    . or(panic(m"could not find $path on the classpath"))

  private def parseEntries
    ( in: ji.InputStream, classify: Text => Optional[Int] )
  :   List[Entry] =

    scala.io.Source.fromInputStream(in).getLines().toList.flatMap: line =>
      Text(line) match
        case r"${Hex(from)}([0-9A-Fa-f]+)\.\.${Hex(to)}([0-9A-Fa-f]+)\s*;\s*$name([A-Za-z_]+).*" =>
          classify(name).option.map(Entry(from, to, _))

        case r"${Hex(from)}([0-9A-Fa-f]+)\s*;\s*${name}([A-Za-z_]+).*" =>
          classify(name).option.map(Entry(from, from, _))

        case _ =>
          None

  private def gbpClassify(name: Text): Optional[Int] =
    name.s.match
      case "CR"                 => Property.Cr
      case "LF"                 => Property.Lf
      case "Control"            => Property.Control
      case "Extend"             => Property.Extend
      case "ZWJ"                => Property.Zwj
      case "Regional_Indicator" => Property.RegionalIndicator
      case "Prepend"            => Property.Prepend
      case "SpacingMark"        => Property.SpacingMark
      case "L"                  => Property.L
      case "V"                  => Property.V
      case "T"                  => Property.T
      case "LV"                 => Property.Lv
      case "LVT"                => Property.Lvt
      case _                    => Unset

    . let(_.ordinal)

  // Indic Conjunct Break property (UAX #44 §5.7.7) — feeds rule GB9c.
  private object IncbValue:
    val Consonant: Int = 0
    val Extend: Int = 1
    val Linker: Int = 2

  private def parseIncbEntries(in: ji.InputStream): List[Entry] =
    scala.io.Source.fromInputStream(in).getLines().toList.flatMap: line =>
      Text(line) match
        case
          r"${Hex(from)}([0-9A-Fa-f]+)\.\.${Hex(to)}([0-9A-Fa-f]+)\s*;\s*InCB\s*;\s*$name([A-Za-z]+).*" =>
            incbClassify(name).option.map(Entry(from, to, _))

        case r"${Hex(from)}([0-9A-Fa-f]+)\s*;\s*InCB\s*;\s*$name([A-Za-z]+).*" =>
          incbClassify(name).option.map(Entry(from, from, _))

        case _ =>
          None

  private def incbClassify(name: Text): Optional[Int] = name.s match
    case "Consonant" => IncbValue.Consonant
    case "Extend"    => IncbValue.Extend
    case "Linker"    => IncbValue.Linker
    case _           => Unset

  private case class Tables(starts: IArray[Int], ends: IArray[Int], props: IArray[Byte])

  private def buildTables(entries: List[Entry]): Tables =
    val sorted = entries.sortBy(_.start).toArray
    val count = sorted.length
    val starts = new Array[Int](count)
    val ends = new Array[Int](count)
    val props = new Array[Byte](count)

    var index = 0

    while index < count do
      starts(index) = sorted(index).start
      ends(index) = sorted(index).end
      props(index) = sorted(index).prop.toByte
      index += 1

    unsafely(Tables(starts.immutable, ends.immutable, props.immutable))

  private lazy val gbpTables: Tables =
    val in = loadResource(
      "/hieroglyph/GraphemeBreakProperty.txt",
      "https://www.unicode.org/Public/16.0.0/ucd/auxiliary/GraphemeBreakProperty.txt")

    buildTables(parseEntries(in, gbpClassify))

  private lazy val extPictTables: Tables =
    val in = loadResource(
      "/hieroglyph/emoji-data.txt",
      "https://www.unicode.org/Public/16.0.0/ucd/emoji/emoji-data.txt")

    buildTables(parseEntries(in, name => if name.s == "Extended_Pictographic" then 0 else Unset))

  private lazy val incbTables: Tables =
    val in = loadResource(
      "/hieroglyph/IndicConjunctBreak.txt",
      "https://www.unicode.org/Public/16.0.0/ucd/DerivedCoreProperties.txt")

    buildTables(parseIncbEntries(in))

  private def lookup(tables: Tables, codepoint: Int): Int =
    val starts = tables.starts
    var low = 0
    var high = starts.length - 1
    var found = -1

    while low <= high do
      val mid = (low + high) >>> 1

      if starts(mid) > codepoint then high = mid - 1 else
        found = mid
        low = mid + 1

    if found >= 0 && codepoint <= tables.ends(found) then tables.props(found).toInt else -1

  def property(codepoint: Int): Property =
    val ord = lookup(gbpTables, codepoint)
    if ord < 0 then Property.Other else Property.fromOrdinal(ord)

  def extendedPictographic(codepoint: Int): Boolean = lookup(extPictTables, codepoint) >= 0
  def incb(codepoint: Int): Int = lookup(incbTables, codepoint)

  def boundaries(text: Text): IArray[Int] =
    import Property.*

    val s = text.s
    val n = s.length
    val builder = ArrayBuilder.make[Int]

    builder.addOne(0)

    if n == 0 then builder.result().immutable(using Unsafe) else
      var index = 0
      val firstCodepoint = Character.codePointAt(s, 0)
      var prev = property(firstCodepoint)
      var regionalCount = if prev == RegionalIndicator then 1 else 0
      var inEmojiSeq = extendedPictographic(firstCodepoint)

      // GB9c state: have we seen an InCB=Consonant whose tail (any number of InCB=Extend or
      // InCB=Linker codepoints up to and including `prev`) contains at least one Linker?
      var incbAfterConsonant = incb(firstCodepoint) == IncbValue.Consonant
      var incbHasLinker = false

      index = Character.charCount(firstCodepoint)

      while index < n do
        val codepoint = Character.codePointAt(s, index)
        val curr = property(codepoint)
        val currExtPict = extendedPictographic(codepoint)
        val currIncb = incb(codepoint)

        val brk =
          if prev == Cr && curr == Lf then false
          else if prev == Control || prev == Cr || prev == Lf then true
          else if curr == Control || curr == Cr || curr == Lf then true
          else if prev == L && (curr == L || curr == V || curr == Lv || curr == Lvt) then false
          else if (prev == Lv || prev == V) && (curr == V || curr == T) then false
          else if (prev == Lvt || prev == T) && curr == T then false
          else if curr == Extend || curr == Zwj then false
          else if curr == SpacingMark then false
          else if prev == Prepend then false
          // GB9c: InCB Consonant ([Extend Linker]* Linker [Extend Linker]*) × Consonant
          else if currIncb == IncbValue.Consonant && incbAfterConsonant && incbHasLinker
          then false
          else if inEmojiSeq && prev == Zwj && currExtPict then false
          else if prev == RegionalIndicator && curr == RegionalIndicator && regionalCount%2 == 1
          then false
          else true

        if brk then builder.addOne(index)

        inEmojiSeq =
          if currExtPict then true
          else if inEmojiSeq && (curr == Extend || curr == Zwj) then true
          else false

        regionalCount =
          if curr == RegionalIndicator
          then if prev == RegionalIndicator && !brk then regionalCount + 1 else 1
          else 0

        // Update GB9c state: a Consonant resets the chain to a fresh "after-consonant"; an
        // Extend or Linker continues an existing chain; anything else clears it.
        val nextIncbAfterConsonant =
          if currIncb == IncbValue.Consonant then true
          else if currIncb == IncbValue.Extend || currIncb == IncbValue.Linker
          then incbAfterConsonant
          else false

        val nextIncbHasLinker =
          if currIncb == IncbValue.Consonant then false
          else if currIncb == IncbValue.Linker && incbAfterConsonant then true
          else if currIncb == IncbValue.Extend && incbAfterConsonant then incbHasLinker
          else false

        incbAfterConsonant = nextIncbAfterConsonant
        incbHasLinker = nextIncbHasLinker

        prev = curr
        index += Character.charCount(codepoint)

      builder.addOne(n)
      builder.result().immutable(using Unsafe)
