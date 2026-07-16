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

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

private[facsimile] object Xref:
  enum Entry:
    case Free
    case Direct(offset: Long, generation: Int)
    case Compressed(container: Int, index: Int)

  // Locates `startxref` near the end of the file and walks the chain of cross-reference
  // sections — classic tables and cross-reference streams — through `/Prev`, newest first.
  // Later (older) sections never override earlier entries, and the newest trailer's values
  // take precedence. `load` is the single seam a damaged-file `rebuild` can replace later.
  def load(source: ByteSource): Xref raises PdfError =
    def recur(offset: Long, entries: Map[Int, Entry], trailer: Map[Text, Cos], visited: Set[Long])
    :   Xref =

      if visited.contains(offset) || offset < 0 || offset >= source.size
      then abort(PdfError(PdfError.Reason.MalformedXref(offset)))

      val (sectionEntries, sectionTrailer) = section(source, offset)
      val mergedEntries = sectionEntries ++ entries
      val mergedTrailer = sectionTrailer ++ trailer

      sectionTrailer.at(t"Prev").let(_.long).lay(Xref(mergedEntries, mergedTrailer)): previous =>
        recur(previous, mergedEntries, mergedTrailer, visited + offset)

    recur(startxref(source), Map(), Map(), Set())

  private def startxref(source: ByteSource): Long raises PdfError =
    val windowSize = source.size.min(2048L).toInt
    val windowStart = source.size - windowSize
    val window = source.read(windowStart, windowSize)
    val marker = t"startxref"

    var i = window.length - marker.length

    while i >= 0 && !matches(window, i, marker) do i -= 1
    if i < 0 then abort(PdfError(PdfError.Reason.MissingStartxref))

    val lexer = CosLexer(new Scan(source, windowStart + i))

    lexer.next() // the `startxref` keyword itself

    lexer.next() match
      case CosToken.Integral(offset) => offset
      case _                         => abort(PdfError(PdfError.Reason.MissingStartxref))

  private def matches(window: Data, index: Int, marker: Text): Boolean =
    var j = 0
    while j < marker.length && (window(index + j) & 0xff) == marker.s.charAt(j).toInt do j += 1
    j == marker.length

  private def section(source: ByteSource, offset: Long)
  :   (Map[Int, Entry], Map[Text, Cos]) raises PdfError =

    val lexer = CosLexer(new Scan(source, offset))

    lexer.next() match
      case CosToken.Keyword(word) if word.s == "xref" =>
        classic(lexer, offset)

      case CosToken.Integral(_) =>
        stream(source, offset)

      case _ =>
        abort(PdfError(PdfError.Reason.MalformedXref(offset)))

  // A classic cross-reference table: subsections of fixed-format entries, then a trailer
  // dictionary. Entries are lexed rather than sliced at fixed widths, which also tolerates
  // the widespread 19-byte-line variant.
  private def classic(lexer: CosLexer, offset: Long)
  :   (Map[Int, Entry], Map[Text, Cos]) raises PdfError =

    var entries = Map[Int, Entry]()

    def subsections(): Map[Text, Cos] = lexer.next() match
      case CosToken.Integral(first) =>
        val count = lexer.next() match
          case CosToken.Integral(count) => count.toInt
          case _ => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

        for index <- 0 until count do
          val entry = (lexer.next(), lexer.next(), lexer.next()) match
            case (CosToken.Integral(position), CosToken.Integral(generation),
                  CosToken.Keyword(kind)) =>
              kind.s match
                case "n" => Entry.Direct(position, generation.toInt)
                case "f" => Entry.Free
                case _   => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

            case _ =>
              abort(PdfError(PdfError.Reason.MalformedXref(offset)))

          entries = entries.updated(first.toInt + index, entry)

        subsections()

      case CosToken.Keyword(word) if word.s == "trailer" =>
        CosParser(lexer).value() match
          case Cos.Dictionary(trailer) => trailer
          case _ => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

      case _ =>
        abort(PdfError(PdfError.Reason.MalformedXref(offset)))

    val trailer = subsections()
    (entries, trailer)

  // A cross-reference stream (ISO 32000-2 §7.5.8): the stream dictionary is the trailer, and
  // its decoded payload holds binary rows of `/W`-specified field widths. Everything needed
  // here — `/Length`, `/W`, `/Index`, `/Size`, filters — is required by the spec to be direct.
  private def stream(source: ByteSource, offset: Long)
  :   (Map[Int, Entry], Map[Text, Cos]) raises PdfError =

    val parser = CosParser(CosLexer(new Scan(source, offset)))

    parser.indirect() match
      case (_, _, Cos.Body(dictionary, start)) =>
        val length = dictionary.at(t"Length").let(_.long)
          . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Length")))).toInt

        val raw = source.read(start, length)
        if raw.length < length then abort(PdfError(PdfError.Reason.Truncated))

        val chain = Filter.chain(dictionary.at(t"Filter"), dictionary.at(t"DecodeParms"))
        val data = Filter.decode(raw, chain)

        val widths: List[Int] = dictionary.at(t"W").let(_.elements)
          . or(abort(PdfError(PdfError.Reason.MissingEntry(t"W"))))
          . map(_.long.or(abort(PdfError(PdfError.Reason.TypeMismatch(t"W", t"an integer")))).toInt)

        if widths.length != 3 then abort(PdfError(PdfError.Reason.MalformedXref(offset)))

        val size = dictionary.at(t"Size").let(_.long)
          . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Size"))))

        val ranges: List[(Long, Long)] =
          dictionary.at(t"Index").let(_.elements).lay(List((0L, size))): elements =>
            elements.map(_.long.or(abort(PdfError(PdfError.Reason.MalformedXref(offset)))))
            . grouped(2).to(List).map:
                case List(first, count) => (first, count)
                case _ => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

        val rowLength = widths.sum
        var entries = Map[Int, Entry]()
        var position = 0

        for (first, count) <- ranges do
          for index <- 0L until count do
            if position + rowLength > data.length
            then abort(PdfError(PdfError.Reason.MalformedXref(offset)))

            // A zero-width first field defaults to type 1; other absent fields default to 0.
            val kind = if widths(0) == 0 then 1L else field(data, position, widths(0))
            val second = field(data, position + widths(0), widths(1))
            val third = field(data, position + widths(0) + widths(1), widths(2))
            position += rowLength

            val entry = kind match
              case 0L => Entry.Free
              case 1L => Entry.Direct(second, third.toInt)
              case 2L => Entry.Compressed(second.toInt, third.toInt)
              case _  => Entry.Free // unknown types are reserved and must be treated as free

            entries = entries.updated((first + index).toInt, entry)

        (entries, dictionary)

      case _ =>
        abort(PdfError(PdfError.Reason.MalformedXref(offset)))

  private def field(data: Data, start: Int, width: Int): Long =
    var value = 0L
    var i = 0

    while i < width do
      value = (value << 8) + (data(start + i) & 0xff)
      i += 1

    value

private[facsimile] case class Xref(entries: Map[Int, Xref.Entry], trailer: Map[Text, Cos])
