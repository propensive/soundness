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

// By name: `contingency.*` would otherwise shadow this package's own `Guard` (the PDF
// standard-security handler) with contingency's skip-scope capability of the same name.
import facsimile.Guard

import anticipation.*
import contingency.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import symbolism.*
import vacuous.*
import denominative.*
import denominative.dysasymptotics.linearSize
import rudiments.sortingAlgorithms.timsort

// Serialises a write overlay as a PDF incremental update (ISO 32000-2 §7.5.6): the changed
// and new objects, a cross-reference section covering just them, and a trailer chaining
// `/Prev` to the file's previous cross-reference offset. The result is appended to the
// original bytes, which are never rewritten — the signature- and diff-friendly model.
private[facsimile] object PdfWriter:
  // A complete PDF file for a freshly-authored document: a header, every live object, one
  // cross-reference table and a trailer with no `/Prev`. Used by `create`, where there is no
  // original file to append to.
  def full(pdf: Pdf)(using Tactic[Pdf.Error]): Data =
    val builder = DataBuilder()
    var length = 0L

    def raw(data: Data): Unit =
      builder.addAll(data)
      length += data.length

    def ascii(text: Text): Unit = raw(charEncoders.iso88591Encoder.encoded(text))

    // A binary comment after the header marks the file as containing binary data.
    ascii(t"%PDF-1.7\n")
    raw(Array[Byte]('%'.toByte, 0xe2.toByte, 0xe3.toByte, 0xcf.toByte, 0xd3.toByte, '\n'.toByte))

    val maxNumber = pdf.nextNumber - 1
    val offsets = scala.collection.mutable.HashMap[Int, Long]()

    (1 to maxNumber).each: number =>
      val value = pdf.apply(number)
      if value != Cos.Nil && !pdf.freed.contains(number) then
        offsets(number) = length
        ascii(t"$number 0 obj\n")
        // The writer thunks share only this append pass's own accumulators.
        scala.caps.unsafe.unsafeAssumeSeparate(appendObject(pdf, raw, ascii, value))
        ascii(t"\nendobj\n")

    val xrefOffset = length
    ascii(t"xref\n0 ${maxNumber + 1}\n0000000000 65535 f \n")

    (1 to maxNumber).each: number =>
      offsets.at(number) match
        case offset: Long => ascii(t"${pad10(offset)} 00000 n \n")
        case _            => ascii(t"0000000000 00000 f \n")

    ascii(t"trailer\n<< /Size ${maxNumber + 1}")

    List(t"Root", t"Info", t"ID").each: (key: Text) =>
      pdf.trailerOverrides.at(key).or(pdf.trailer(key)).let: value =>
        ascii(t" /$key ")
        // The writer thunks share only this append pass's own accumulators.
        scala.caps.unsafe.unsafeAssumeSeparate(appendObject(pdf, raw, ascii, value))

    ascii(t" >>\nstartxref\n$xrefOffset\n%%EOF\n")

    builder.result()

  // The bytes to append after `baseOffset` (the original file's length) to record the
  // overlay. Object offsets in the new section are absolute, so they include `baseOffset`.
  def increment(pdf: Pdf, baseOffset: Long)(using Tactic[Pdf.Error]): Data =
    val builder = DataBuilder()
    var length = 0L

    def raw(data: Data): Unit =
      builder.addAll(data)
      length += data.length

    def ascii(text: Text): Unit = raw(charEncoders.iso88591Encoder.encoded(text))

    // A leading end-of-line guards against the original file not ending in one.
    ascii(t"\n")

    val changed = pdf.overlay.keys.to(scala.List).sorted
    val offsets = scala.collection.mutable.HashMap[Int, Long]()

    changed.each: (number: Int) =>
      offsets(number) = baseOffset + length
      val generation = pdf.xref.entries(number) match
        case Xref.Entry.Direct(_, gen) => gen
        case _                         => 0

      ascii(t"$number $generation obj\n")

      // In an encrypted document, new and edited objects — held in the overlay as cleartext —
      // are encrypted with their own per-object key as they are written.
      val encryption = pdf.guard.let((_, number, generation))
      val value = pdf.guard.lay(pdf.overlay(number)):
        guard => encryptStrings(pdf.overlay(number), guard, number, generation)

      // The writer thunks share only this append pass's own accumulators.
      scala.caps.unsafe.unsafeAssumeSeparate(appendObject(pdf, raw, ascii, value, encryption))
      ascii(t"\nendobj\n")

    val xrefOffset = baseOffset + length
    val freed = pdf.freed.to(scala.List).sorted

    // Group the updated and freed object numbers (plus object 0, the free-list head, when
    // anything is freed) into ascending consecutive subsections.
    val zero = if freed.isEmpty then scala.List[Int]() else scala.List(0)
    val numbers = ((changed ::: freed ::: zero).distinct.sorted).to(List)

    // The trailer carries forward the original `/Root`, `/Info`, `/Encrypt` and `/ID`, with
    // any write-scope overrides (e.g. a newly-created `/Info`) taking precedence.
    val carried = List(t"Root", t"Info", t"Encrypt", t"ID").bind: key =>
      pdf.trailer(key).let(value => List(key -> value)).or(Nil)

    // Through a `Map` so a write-scope override replaces the carried-forward entry of the same
    // key rather than joining it.
    val entries: List[(Text, Cos)] =
      (carried + List.from(pdf.trailerOverrides)).to[Map].to[List]

    // A file whose newest cross-reference section is a stream takes a stream for its update too.
    // The two forms cannot be chained through `/Prev`, which is defined to address a section of
    // the same kind (ISO 32000-1 §7.5.8.4); a file that mixes them is one some readers accept and
    // others reject outright.
    if pdf.xref.streamed
    // The writer thunks share only this append pass's own accumulators.
    then scala.caps.unsafe.unsafeAssumeSeparate
          ( streamed(pdf, raw, ascii, xrefOffset, numbers, offsets, entries) )
    else
      ascii(t"xref\n")

      subsections(numbers).each: (first, run) =>
        ascii(t"$first ${run.size}\n")

        run.each: number =>
          if number == 0 then ascii(t"0000000000 65535 f \n")
          else if pdf.freed.contains(number) then
            val generation = pdf.xref.entries(number) match
              case Xref.Entry.Direct(_, gen) => gen + 1
              case _                         => 1

            ascii(t"0000000000 ${pad5(generation)} f \n")
          else
            val generation = pdf.xref.entries(number) match
              case Xref.Entry.Direct(_, gen) => gen
              case _                         => 0

            ascii(t"${pad10(offsets(number))} ${pad5(generation)} n \n")

      ascii(t"trailer\n<< /Size ${pdf.nextNumber}")

      entries.each: (key, value) =>
        ascii(t" /$key ")
        scala.caps.unsafe.unsafeAssumeSeparate(appendObject(pdf, raw, ascii, value))

      pdf.xref.startxref.let: previous => ascii(t" /Prev $previous")

      ascii(t" >>\nstartxref\n$xrefOffset\n%%EOF\n")

    builder.result()

  // The update's cross-reference section as a cross-reference stream (ISO 32000-2 §7.5.8): an
  // ordinary indirect object whose dictionary serves as the trailer and whose payload holds one
  // fixed-width binary row per object.
  //
  // The stream is an object in its own right, so it occupies the next free number and appears in
  // its own table. Rows are written uncompressed: a section covering a handful of changed objects
  // is a few dozen bytes, and `/Filter` would cost more in dictionary than it saved in payload.
  private def streamed
    ( pdf:     Pdf,
      raw:     Data => Unit,
      ascii:   Text => Unit,
      offset:  Long,
      numbers: List[Int],
      offsets: scala.collection.mutable.HashMap[Int, Long],
      entries: List[(Text, Cos)] )
  ( using Tactic[Pdf.Error] )
  :   Unit =

    val number = pdf.nextNumber
    val rows = (numbers :+ number).distinct.sort

    // `/W [1 4 2]`: one byte of entry type, four of offset — enough for any file this side of
    // 4 GB — and two of generation.
    def field(value: Long, width: Int): Unit =
      raw(Array.tabulate(width) { index => (value >> 8*(width - 1 - index) & 0xff).toByte })

    def row(kind: Int, second: Long, third: Int): Unit =
      field(kind, 1)
      field(second, 4)
      field(third, 2)

    val index = subsections(rows).flatMap((first, run) => List(first, run.size))

    ascii(t"$number 0 obj\n<< /Type /XRef /Size ${number + 1} /W [1 4 2] /Index [")
    ascii(index.map(_.toString.tt).join(t" "))
    ascii(t"] /Length ${rows.size*7}")

    entries.each: (key, value) =>
      ascii(t" /$key ")
      // The writer thunks share only this append pass's own accumulators.
      scala.caps.unsafe.unsafeAssumeSeparate(appendObject(pdf, raw, ascii, value))

    pdf.xref.startxref.let: previous => ascii(t" /Prev $previous")

    ascii(t" >>\nstream\n")

    rows.each: entry =>
      if entry == number then row(1, offset, 0)
      else if entry == 0 then row(0, 0, 65535)
      else if pdf.freed.contains(entry) then
        val generation = pdf.xref.entries(entry) match
          case Xref.Entry.Direct(_, gen) => gen + 1
          case _                         => 1

        row(0, 0, generation)
      else
        val generation = pdf.xref.entries(entry) match
          case Xref.Entry.Direct(_, gen) => gen
          case _                         => 0

        row(1, offsets(entry), generation)

    ascii(t"\nendstream\nendobj\nstartxref\n$offset\n%%EOF\n")

  private def appendObject
    ( pdf: Pdf, raw: Data => Unit, ascii: Text => Unit, cos: Cos,
      encryption: Optional[(Guard, Int, Int)] = Unset )
  ( using Tactic[Pdf.Error] )
  :   Unit =

    cos match
      case body: Cos.Body =>
        // A stream: its dictionary, then the payload framed by `stream`/`endstream`, with
        // `/Length` recomputed to the (possibly encrypted) byte count.
        val stored = pdf.raw(body)
        val payload = encryption.lay(stored): (guard, number, generation) =>
          guard.encryptStream(stored, number, generation)

        val entries = body.entries.define(t"Length", Cos.Integral(payload.length.toLong))
        raw(CosWriter.dictionaryBytes(entries))
        ascii(t"\nstream\n")
        raw(payload)
        ascii(t"\nendstream")

      case _ =>
        raw(CosWriter.write(cos))

  // Encrypts every string in an object with the object's key, for writing into an encrypted
  // document. Stream payloads are handled separately, at the point they are framed.
  private def encryptStrings(cos: Cos, guard: Guard, number: Int, generation: Int): Cos =
    cos match
      case Cos.Chars(bytes) =>
        Cos.Chars(guard.encryptString(bytes, number, generation))

      case Cos.Sequence(elements) =>
        Cos.Sequence(elements.map(encryptStrings(_, guard, number, generation)))

      case Cos.Dictionary(entries) =>
        Cos.Dictionary(entries.map(encryptStrings(_, guard, number, generation)))

      case Cos.Body(entries, start) =>
        Cos.Body(entries.map(encryptStrings(_, guard, number, generation)), start)

      case other =>
        other

  // Consecutive runs of ascending numbers, each as (first, members).
  private def subsections(numbers: List[Int]): List[(Int, List[Int])] =
    numbers match
      case Nil =>
        Nil

      case head :: _ =>
        val runs = scala.collection.immutable.List.newBuilder[(Int, List[Int])]
        var run = scala.collection.immutable.List.newBuilder[Int]
        var first = head
        var previous = head - 1

        numbers.each: number =>
          if number == previous + 1 then run += number
          else
            runs += ((first, run.result().to(List)))
            run = scala.collection.immutable.List.newBuilder[Int]
            run += number
            first = number

          previous = number

        runs += ((first, run.result().to(List)))
        runs.result().to(List)

  private def pad10(value: Long): Text =
    val digits = value.toString
    ("0".repeat(10 - digits.length).nn + digits).tt

  private def pad5(value: Int): Text =
    val digits = value.toString
    ("0".repeat(5 - digits.length).nn + digits).tt
