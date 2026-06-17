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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import parasite.*
import rudiments.*
import vacuous.*
import zephyrine.*

// Reverses the presentation parser. The printer is line-based: each
// emission appends one or more strings to a buffer that is finally joined
// with the document's line-ending character(s). Total newlines =
// `buffer.length - 1`; trailing blank-line counts are realised by
// appending that many empty strings, which yields exactly one more `\n`
// per trailing blank in the joined output.

object TelPrinter:
  def print(document: Tel.Document): Text =
    Producer.collect[Text](): producer =>
      write(producer, document)

  def emit(document: Tel.Document)(using Monitor, Probate): Iterator[Text] =
    val producer = Producer[Text](4096)

    async:
      write(producer, document)
      producer.finish()

    producer.iterator

  private def write(producer: Producer[Text], document: Tel.Document): Unit =
    val newline = document.lineEndings match
      case Tel.LineEndings.Lf   => "\n"
      case Tel.LineEndings.Crlf => "\r\n"

    var first = true

    // Emit one line, inserting the document's line ending between lines but never after the last,
    // so the joined output matches the former buffer-and-`mkString` exactly.
    def out(text: String): Unit =
      if first then first = false else producer.put(Text(newline))
      producer.put(Text(text))

    val sigil = document.pragma.let(_.sigil.or('#')).or('#')

    document.interpreterDirective.let: payload =>
      out("#!" + payload.s)

    document.pragma.let: pragma =>
      val parts = scala.collection.mutable.ArrayBuffer.empty[String]
      parts += "tel"
      parts += s"${pragma.version._1}.${pragma.version._2}"
      pragma.schema.let: s => parts += s.s
      pragma.sigil.let: c => parts += c.toString
      out(parts.mkString(" "))
      out("")

    document.children.each(emitBlock(out, _, 0, sigil))

  private def emitBlock(out: String => Unit, block: Tel.Block, indent: Int, sigil: Char): Unit =
    val pad = "  " * indent

    block.comments.each: comment =>
      val text = comment.text.s

      if text.isEmpty then out(s"$pad$sigil") else out(s"$pad$sigil $text")

    block.tabulation.let: tab =>
      val line = StringBuilder()
      var i = 0

      while i < tab.markerOffsets.length do
        val targetCol = tab.markerOffsets(i)
        while line.length < targetCol do line.append(' ')
        line.append(sigil)
        val heading = tab.headings(i).s

        if heading.nonEmpty then
          line.append(' ')
          line.append(heading)

        i += 1

      out(line.toString)

    block.compounds.each(emitCompound(out, _, indent, sigil))

    var b = 0

    while b < block.trailingBlankLines do
      out("")
      b += 1

  private def emitCompound
    ( out:      String => Unit,
      compound: Tel.Compound,
      indent:   Int,
      sigil:    Char )
  :   Unit =

    val pad = "  " * indent
    val line = StringBuilder()
    line.append(pad)
    line.append(compound.keyword.s)

    var trailingAtom: Optional[Tel.Atom] = Unset

    compound.atoms.each:
      case atom @ Tel.Atom.Source(_)     => trailingAtom = atom
      case atom @ Tel.Atom.Literal(_, _) => trailingAtom = atom

      case Tel.Atom.Inline(text, precedingSpaces) =>
        var k = 0
        while k < precedingSpaces do { line.append(' '); k += 1 }
        line.append(text.s)

    compound.remark.let: remark =>
      // Two spaces before the sigil ensure correct re-parsing regardless of whether the preceding
      // atoms put the line into hard-space mode: in hard mode only hard spaces terminate phrases,
      // so a single space before `#` would be absorbed as atom content. §18.1 permits a minimum
      // hard space before remark introducers.
      line.append("  ")
      line.append(sigil)
      line.append(' ')
      line.append(remark.s)

    out(line.toString)

    trailingAtom match
      case Tel.Atom.Source(text) =>
        val sourcePad = "  " * (indent + 2)
        // §14 "Convention A": `text` is LF-separated with no trailing LF, so each LF-delimited
        // segment is one source line (an empty segment is a blank line with no indentation).
        val sourceText = text.s
        var start = 0

        while start <= sourceText.length do
          val nl = sourceText.indexOf('\n', start)
          val end = if nl < 0 then sourceText.length else nl
          val seg = sourceText.substring(start, end)
          out(if seg.isEmpty then "" else sourcePad + seg)
          if nl < 0 then start = sourceText.length + 1 else start = nl + 1

      case Tel.Atom.Literal(delimiter, text) =>
        out("  " * (indent + 3) + delimiter.s)
        val payload = text.s
        var start = 0

        while start <= payload.length do
          val nl = payload.indexOf('\n', start)
          val end = if nl < 0 then payload.length else nl
          out(payload.substring(start, end))
          if nl < 0 then start = payload.length + 1 else start = nl + 1

        out(delimiter.s)

      case _ => ()

    compound.children.each(emitBlock(out, _, indent + 1, sigil))
