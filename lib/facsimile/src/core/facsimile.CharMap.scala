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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import vacuous.*

// A `/ToUnicode` character map (ISO 32000-2 §9.10.3): CMap syntax lexes as ordinary COS
// tokens, so the whole PostScript scaffolding reduces to watching for the `endbfchar`,
// `endbfrange` and `endcodespacerange` operators and interpreting their operands. Values
// are UTF-16BE without a byte-order mark; a range destination increments its final code
// unit across the range.
private[facsimile] object CharMap:
  def parse(data: Data): CharMap raises PdfError =
    val parser = CosParser(CosLexer(Scan(data)), references = false)
    val entries = scala.collection.immutable.Map.newBuilder[Int, Text]
    var codeBytes = 1
    var done = false

    def code(value: Cos): Optional[Int] = value.chars.let: bytes =>
      var result = 0
      var i = 0

      while i < bytes.length do
        result = (result << 8) | (bytes(i) & 0xff)
        i += 1

      result

    def target(value: Cos): Optional[Text] = value.chars.let: bytes =>
      charDecoders.utf16BeDecoder.decoded(bytes)

    def increment(text: Text, by: Int): Text =
      if text.s.isEmpty then text
      else (text.s.substring(0, text.s.length - 1).nn
          + (text.s.charAt(text.s.length - 1) + by).toChar).tt

    while !done do
      parser.instruction().let: (operands, operator) =>
        operator.s match
          case "endcodespacerange" => operands match
            case low :: _ =>
              low.chars.let { bytes => codeBytes = bytes.length.max(1) }

            case _ =>
              ()

          case "endbfchar" =>
            operands.batched(2).each:
              case List(source, destination) =>
                code(source).let: code =>
                  target(destination).let { text => entries += code -> text }

              case _ =>
                ()

          case "endbfrange" =>
            operands.batched(3).each:
              case List(low, high, destination) =>
                code(low).let: start =>
                  code(high).let: end =>
                    if end - start >= 0 && end - start <= 65535 then destination match
                      case Cos.Sequence(elements) =>
                        elements.stdlib.zipWithIndex.each: (element, index) =>
                          target(element).let { text => entries += (start + index) -> text }

                      case single =>
                        target(single).let: base =>
                          var i = 0

                          while i <= end - start do
                            entries += (start + i) -> increment(base, i)
                            i += 1

              case _ =>
                ()

          case _ =>
            ()

      . or:
          done = true

    CharMap(Map.of(entries.result()), codeBytes)

private[facsimile] case class CharMap(entries: Map[Int, Text], codeBytes: Int):
  def apply(code: Int): Optional[Text] = entries.at(code)
