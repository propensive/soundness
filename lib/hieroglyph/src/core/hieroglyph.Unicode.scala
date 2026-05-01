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

import scala.collection.immutable.TreeMap

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import kaleidoscope.*
import proscenium.*
import rudiments.*
import vacuous.*

object Unicode:
  import hieroglyph.internal.*

  private object Hex:
    def unapply(text: Text): Option[Int] =
      try Some(Integer.parseInt(text.s, 16)) catch case error: NumberFormatException => None

  private val smallCapsAlphabet: String = "ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘǫʀsᴛᴜᴠᴡxʏᴢ"

  def smallCaps(text: Text): Text =
    text.s.map:
      case char if char >= 'a' && char <= 'z' => smallCapsAlphabet(char - 'a')
      case char                               => char

    . tt

  object EaWidth:
    def unapply(code: Text): Option[EaWidth] =
      code.s.only:
        case "N"  => Neutral
        case "W"  => Wide
        case "A"  => Ambiguous
        case "H"  => HalfWidth
        case "F"  => FullWidth
        case "Na" => Narrow

      . option

  enum EaWidth:
    case Neutral, Narrow, Wide, Ambiguous, FullWidth, HalfWidth

    def width: Int = this match
      case Wide | FullWidth => 2
      case _                => 1

  def eastAsianWidth(char: Char): Optional[EaWidth] = eastAsianWidth(char.toInt)

  def eastAsianWidth(codepoint: Int): Optional[EaWidth] =
    // Each entry's packed key is `(from << 32) + to`, so entries sort by `from` first. Searching
    // with `(codepoint + 1) << 32` and `maxBefore` (strict <) returns the entry with the largest
    // `from` ≤ `codepoint`; the membership test then confirms `codepoint` falls inside `to`.
    val searchKey: CharRange = CharRange(codepoint + 1, 0)

    eastAsianWidths.maxBefore(searchKey).match
      case Some((range, ea)) if codepoint >= range.from && codepoint <= range.to => ea
      case _                                                                     => Unset

  def visible(char: Char): Char = char match
    case '\u007f'            => '␥'
    case ' '                 => '␣'
    case char if char <= ' ' => ('\u2400' + char).toChar
    case char                => char

  def apply(name: Text): Optional[Char | Text] = unicodeData.at(name)
  def name(char: Char): Optional[Text] = unicodeNames.at(char)

  lazy val unicodeData: Map[Text, Char | Text] =
    val in: ji.InputStream =
      Optional(getClass.getResourceAsStream("/hieroglyph/UnicodeData.txt")).or:
        safely:
          val uri = new java.net.URI("https://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
          uri.toURL().nn.openStream().nn: ji.InputStream

      . or(panic(m"could not find hieroglyph/UnicodeData.txt on the classpath"))

    scala.io.Source.fromInputStream(in).getLines.map(_.split(";").nn.to(List)).flatMap:
      case hex :: name :: _ if !name.nn.startsWith("<") =>
        val hexInt = Integer.parseInt(hex, 16)

        if hexInt < 65536 then List((name.nn.tt, hexInt.toChar))
        else List((name.nn.tt, new String(Character.toChars(hexInt)).tt))

      case _ =>
        Nil

    . to(Map)

  lazy val unicodeNames: Map[Char | Text, Text] = unicodeData.map: (key, value) =>
    value -> key.s.split(" ").nn.map(_.nn.toLowerCase.nn.capitalize).mkString(" ").tt

  lazy val eastAsianWidths: TreeMap[CharRange, EaWidth] =
    extension (map: TreeMap[CharRange, EaWidth])
      def append(range: CharRange, width: EaWidth): TreeMap[CharRange, EaWidth] =
        if map.nil then map.updated(range, width)
        else if map.lastKey.to == (range.from - 1) && map(map.lastKey) == width
        then map.removed(map.lastKey).updated(CharRange(map.lastKey.from, range.to), width)
        else map.updated(range, width)

    @tailrec
    def recur(stream: Stream[Text], map: TreeMap[CharRange, EaWidth]): TreeMap[CharRange, EaWidth] =
      stream match
        case
          r"${Hex(from)}([0-9A-F]{4,6})\.\.${Hex(to)}([0-9A-F]{4,6});${EaWidth(w)}([AFHNW]a?).*"
          #:: tail =>
            recur(tail, map.append(CharRange(from, to), w))

        case r"${Hex(from)}([0-9A-F]{4,6});${EaWidth(w)}([AFHNW]a?).*" #:: tail =>
          recur(tail, map.append(CharRange(from, from), w))

        case head #:: tail =>
          recur(tail, map)

        case _ =>
          map

    val in: ji.InputStream =
      Optional(getClass.getResourceAsStream("/hieroglyph/EastAsianWidth.txt")).or:
        safely:
          val uri = new java.net.URI("https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt")
          uri.toURL().nn.openStream().nn: ji.InputStream

        . or:
            panic(m"could not find hieroglyph/EastAsianWidth.txt on the classpath")

    val stream = scala.io.Source.fromInputStream(in).getLines.map(Text(_)).to(Stream)

    recur(stream, TreeMap())
