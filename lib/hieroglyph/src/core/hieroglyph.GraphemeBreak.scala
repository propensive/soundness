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

// Unicode 16.0.0 grapheme cluster boundary detection (UAX #29 GB1–GB13).
// GB9c (Indic Conjunct Break, added in 15.1) is intentionally not implemented.
object GraphemeBreak:
  enum Property:
    case Other, CR, LF, Control, Extend, ZWJ, RegionalIndicator, Prepend, SpacingMark,
        L, V, T, LV, LVT

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
       (in: ji.InputStream, classify: Text => Optional[Int])
  :    List[Entry] =

    scala.io.Source.fromInputStream(in).getLines.toList.flatMap: line =>
      Text(line) match
        case r"${Hex(from)}([0-9A-Fa-f]+)\.\.${Hex(to)}([0-9A-Fa-f]+)\s*;\s*${name}([A-Za-z_]+).*" =>
          classify(name).option.map(Entry(from, to, _))
        case r"${Hex(from)}([0-9A-Fa-f]+)\s*;\s*${name}([A-Za-z_]+).*" =>
          classify(name).option.map(Entry(from, from, _))
        case _ =>
          None

  private def gbpClassify(name: Text): Optional[Int] = name.s match
    case "CR"                 => Property.CR.ordinal
    case "LF"                 => Property.LF.ordinal
    case "Control"            => Property.Control.ordinal
    case "Extend"             => Property.Extend.ordinal
    case "ZWJ"                => Property.ZWJ.ordinal
    case "Regional_Indicator" => Property.RegionalIndicator.ordinal
    case "Prepend"            => Property.Prepend.ordinal
    case "SpacingMark"        => Property.SpacingMark.ordinal
    case "L"                  => Property.L.ordinal
    case "V"                  => Property.V.ordinal
    case "T"                  => Property.T.ordinal
    case "LV"                 => Property.LV.ordinal
    case "LVT"                => Property.LVT.ordinal
    case _                    => Unset

  private case class Tables(starts: IArray[Int], ends: IArray[Int], props: IArray[Byte])

  private def buildTables(entries: List[Entry]): Tables =
    val sorted = entries.sortBy(_.start).toArray
    val n = sorted.length
    val starts = new Array[Int](n)
    val ends = new Array[Int](n)
    val props = new Array[Byte](n)
    var i = 0
    while i < n do
      starts(i) = sorted(i).start
      ends(i) = sorted(i).end
      props(i) = sorted(i).prop.toByte
      i += 1

    Tables(starts.immutable(using Unsafe), ends.immutable(using Unsafe), props.immutable(using Unsafe))

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

  private def lookup(tables: Tables, cp: Int): Int =
    val starts = tables.starts
    var lo = 0
    var hi = starts.length - 1
    var found = -1
    while lo <= hi do
      val mid = (lo + hi) >>> 1
      if starts(mid) <= cp then
        found = mid
        lo = mid + 1
      else hi = mid - 1

    if found >= 0 && cp <= tables.ends(found) then tables.props(found).toInt else -1

  def property(cp: Int): Property =
    val ord = lookup(gbpTables, cp)
    if ord < 0 then Property.Other else Property.fromOrdinal(ord)

  def extendedPictographic(cp: Int): Boolean = lookup(extPictTables, cp) >= 0

  def boundaries(text: Text): IArray[Int] =
    import Property.*
    val s = text.s
    val n = s.length
    val builder = ArrayBuilder.make[Int]
    builder.addOne(0)
    if n == 0 then builder.result().immutable(using Unsafe) else
      var i = 0
      val firstCp = Character.codePointAt(s, 0)
      var prev = property(firstCp)
      var regionalCount = if prev == RegionalIndicator then 1 else 0
      var inEmojiSeq = extendedPictographic(firstCp)
      i = Character.charCount(firstCp)

      while i < n do
        val cp = Character.codePointAt(s, i)
        val curr = property(cp)
        val currExtPict = extendedPictographic(cp)

        // GB1/GB2 are implicit (sentinels at 0 and n).
        // GB3..GB13 in order; first match wins; default GB999 = break.
        val brk =
          if prev == CR && curr == LF then false
          else if prev == Control || prev == CR || prev == LF then true
          else if curr == Control || curr == CR || curr == LF then true
          else if prev == L && (curr == L || curr == V || curr == LV || curr == LVT) then false
          else if (prev == LV || prev == V) && (curr == V || curr == T) then false
          else if (prev == LVT || prev == T) && curr == T then false
          else if curr == Extend || curr == ZWJ then false
          else if curr == SpacingMark then false
          else if prev == Prepend then false
          else if inEmojiSeq && prev == ZWJ && currExtPict then false
          else if
            prev == RegionalIndicator && curr == RegionalIndicator && regionalCount % 2 == 1
          then false
          else true

        if brk then builder.addOne(i)

        inEmojiSeq =
          if currExtPict then true
          else if inEmojiSeq && (curr == Extend || curr == ZWJ) then true
          else false

        regionalCount =
          if curr == RegionalIndicator then
            if prev == RegionalIndicator && !brk then regionalCount + 1 else 1
          else 0

        prev = curr
        i += Character.charCount(cp)

      builder.addOne(n)
      builder.result().immutable(using Unsafe)
