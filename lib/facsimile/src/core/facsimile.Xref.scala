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
  // take precedence. When the cross-reference machinery is missing or corrupt — as it
  // frequently is in the wild — the whole file is scanned for objects instead (`rebuild`).
  def load(source: ByteSource): Xref raises PdfError =
    // Any structural failure in the cross-reference machinery drops through to a full-file
    // scan for objects.
    safely(strict(source)).or(rebuild(source))

  private def strict(source: ByteSource): Xref raises PdfError =
    val head = startxref(source)

    def recur(offset: Long, entries: Map[Int, Entry], trailer: Map[Text, Cos], visited: Set[Long])
    :   Xref =

      if visited.contains(offset) || offset < 0 || offset >= source.size
      then abort(PdfError(PdfError.Reason.MalformedXref(offset)))

      val (classicEntries, sectionTrailer) = section(source, offset)

      // A hybrid-reference file (ISO 32000-2 §7.5.8.4): the classic section points at a
      // cross-reference stream carrying the entries — typically for objects in object
      // streams — which legacy readers see as free. The table's live entries win, but its
      // free markers yield to the stream's.
      val sectionEntries = sectionTrailer.at(t"XRefStm").let(_.long).lay(classicEntries):
        hybrid =>
          val (hybridEntries, _) = stream(source, hybrid)

          hybridEntries ++ classicEntries.filter: (number, entry) =>
            entry != Entry.Free || !hybridEntries.contains(number)

      val mergedEntries = sectionEntries ++ entries
      val mergedTrailer = sectionTrailer ++ trailer

      sectionTrailer.at(t"Prev").let(_.long).lay(Xref(mergedEntries, mergedTrailer, head)): previous =>
        recur(previous, mergedEntries, mergedTrailer, visited + offset)

    recur(head, Map(), Map(), Set())

  // Recovers a cross-reference table from a damaged file by scanning for `N G obj` markers,
  // the latest offset of each object number winning (an incremental update's newer copy
  // appears later in the file). Object streams found this way are expanded to their compressed
  // members. The trailer is the last `trailer` dictionary if any, else synthesized by finding
  // the catalog among the recovered objects.
  private[facsimile] def rebuild(source: ByteSource): Xref raises PdfError =
    var entries = Map[Int, Entry]()
    val objectStreams = List.newBuilder[Int]
    val size = source.size
    val chunkSize = 65536
    val overlap = 32 // long enough to hold "NNNNNN GGGGG obj" split across a chunk boundary
    var base = 0L

    while base < size do
      val length = (size - base).min((chunkSize + overlap).toLong).toInt
      val chunk = source.read(base, length)
      val limit = if base + length >= size then chunk.length else chunk.length - overlap
      var i = 0

      while i < limit do
        // A candidate object header is `<digits> <digits> obj` at a token boundary.
        if matches(chunk, i, t"obj") && (i + 3 >= chunk.length || !CosLexer.regular(chunk(i + 3) & 0xff))
           && (i == 0 || CosLexer.whitespace(chunk(i - 1) & 0xff))
        then
          objectHeader(chunk, i).let: (number, generation, start) =>
            val offset = base + start
            entries = entries.updated(number, Entry.Direct(offset, generation))

        i += 1

      base += (if limit == chunk.length then chunk.length else limit).toLong

    // Expand object streams: each `/Type /ObjStm` object's members become compressed entries,
    // unless a direct copy of that member was already recovered (a later direct object wins).
    val direct = entries

    direct.foreach: (container, entry) =>
      entry match
        case Entry.Direct(offset, _) =>
          safely(CosParser(CosLexer(new Scan(source, offset))).indirect()).let: (_, _, content) =>
            content match
              case body @ Cos.Body(dictionary, _)
              if dictionary.at(t"Type").let(_.name) == t"ObjStm" =>
                objStmMembers(source, body).each: number =>
                  if !direct.contains(number)
                  then entries = entries.updated(number, Entry.Compressed(container, 0))

              case _ =>
                ()

        case _ =>
          ()

    Xref(entries, recoverTrailer(source, entries))

  // Parses `<num> <gen> obj` ending at `objAt`, returning the object number, generation, and
  // the offset the `<num>` begins at — by scanning backwards over the two preceding integers.
  private def objectHeader(chunk: Data, objAt: Int): Optional[(Int, Int, Int)] =
    def digits(end: Int): Optional[(Int, Int)] = // (value, startIndex), scanning back from end
      var i = end
      while i > 0 && CosLexer.whitespace(chunk(i - 1) & 0xff) do i -= 1
      val last = i
      while i > 0 && { val b = chunk(i - 1) & 0xff; b >= '0' && b <= '9' } do i -= 1
      if i == last then Unset else (parseInt(chunk, i, last), i)

    digits(objAt).let: (generation, genStart) =>
      digits(genStart).let: (number, numberStart) =>
        (number, generation, numberStart)

  private def parseInt(chunk: Data, start: Int, end: Int): Int =
    var value = 0
    var i = start
    while i < end do
      value = value*10 + (chunk(i) & 0xff) - '0'
      i += 1
    value

  // The object numbers packed into an object stream, from the header table of its decoded
  // payload; tolerant of any failure (a member simply stays unrecovered).
  private def objStmMembers(source: ByteSource, body: Cos.Body): List[Int] =
    safely:
      val length = body.entries.at(t"Length").let(_.long)
        . or(abort(PdfError(PdfError.Reason.MissingEntry(t"Length")))).toInt

      val count = body.entries.at(t"N").let(_.long)
        . or(abort(PdfError(PdfError.Reason.MissingEntry(t"N")))).toInt

      val raw = source.read(body.start, length)
      val chain = Filter.chain(body.entries.at(t"Filter"), body.entries.at(t"DecodeParms"))
      val data = Filter.decode(raw, chain)
      val lexer = CosLexer(Scan(data))

      List.range(0, count).map: _ =>
        (lexer.next(), lexer.next()) match
          case (CosToken.Integral(number), CosToken.Integral(_)) => number.toInt
          case _ => abort(PdfError(PdfError.Reason.CorruptStream(t"ObjStm")))

    . or(List())

  // A recovered trailer: the file's last `trailer` dictionary if one survives, otherwise
  // synthesized around the catalog found among the recovered objects.
  private def recoverTrailer(source: ByteSource, entries: Map[Int, Entry]): Map[Text, Cos] =
    lastTrailer(source).or:
      entries.view.flatMap: (number, entry) =>
        entry match
          case Entry.Direct(offset, generation) =>
            safely(CosParser(CosLexer(new Scan(source, offset))).indirect()).let: (_, _, content) =>
              content.dictionary.let(_.at(t"Type")).let(_.name) match
                case t"Catalog" => List(number -> generation)
                case _          => List()

            . or(List())

          case _ =>
            List()

      . headOption.map: (number, generation) =>
          Map(t"Root" -> Cos.Ref(number, generation))

      . getOrElse(Map())

  // The last `trailer` dictionary in the file, if any (classic-xref files have one even when
  // their cross-reference table is corrupt).
  private def lastTrailer(source: ByteSource): Optional[Map[Text, Cos]] =
    val windowSize = source.size.min(4096L).toInt
    val window = source.read(source.size - windowSize, windowSize)
    val marker = t"trailer"

    var i = window.length - marker.length

    while i >= 0 && !matches(window, i, marker) do i -= 1
    if i < 0 then Unset else
      safely:
        val lexer = CosLexer(new Scan(source, source.size - windowSize + i))
        lexer.next() // the `trailer` keyword
        CosParser(lexer).value().dictionary.or(abort(PdfError(PdfError.Reason.Truncated)))

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
          case _                        => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

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
          case _                       => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

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
                case _                  => abort(PdfError(PdfError.Reason.MalformedXref(offset)))

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

// `startxref` is the byte offset of the newest cross-reference section in the original file —
// the anchor an incremental update chains its own `/Prev` to. It is `Unset` for a table
// recovered by scanning, which has no valid section to chain to (so writing must rewrite in
// full rather than append).
private[facsimile] case class Xref
  ( entries: Map[Int, Xref.Entry], trailer: Map[Text, Cos], startxref: Optional[Long] = Unset )
