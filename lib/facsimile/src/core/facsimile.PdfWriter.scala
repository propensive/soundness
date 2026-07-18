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
import hieroglyph.*
import rudiments.*
import vacuous.*

// Serialises a write overlay as a PDF incremental update (ISO 32000-2 §7.5.6): the changed
// and new objects, a cross-reference section covering just them, and a trailer chaining
// `/Prev` to the file's previous cross-reference offset. The result is appended to the
// original bytes, which are never rewritten — the signature- and diff-friendly model.
private[facsimile] object PdfWriter:
  // A complete PDF file for a freshly-authored document: a header, every live object, one
  // cross-reference table and a trailer with no `/Prev`. Used by `create`, where there is no
  // original file to append to.
  def full(pdf: Pdf): Data raises PdfError =
    val builder = Array.newBuilder[Byte]
    var length = 0L

    def raw(data: Data): Unit =
      builder.addAll(data.mutable(using Unsafe))
      length += data.length

    def ascii(text: Text): Unit = raw(charEncoders.iso88591Encoder.encoded(text))

    // A binary comment after the header marks the file as containing binary data.
    ascii(t"%PDF-1.7\n")
    raw(IArray[Byte]('%'.toByte, 0xe2.toByte, 0xe3.toByte, 0xcf.toByte, 0xd3.toByte, '\n'.toByte))

    val maxNumber = pdf.nextNumber - 1
    val offsets = scala.collection.mutable.HashMap[Int, Long]()

    (1 to maxNumber).each: number =>
      val value = pdf.apply(number)
      if value != Cos.Nil && !pdf.freed.contains(number) then
        offsets(number) = length
        ascii(t"$number 0 obj\n")
        appendObject(pdf, raw, ascii, value)
        ascii(t"\nendobj\n")

    val xrefOffset = length
    ascii(t"xref\n0 ${maxNumber + 1}\n0000000000 65535 f \n")

    (1 to maxNumber).each: number =>
      offsets.at(number) match
        case offset: Long => ascii(t"${pad10(offset)} 00000 n \n")
        case _            => ascii(t"0000000000 00000 f \n")

    ascii(t"trailer\n<< /Size ${maxNumber + 1}")

    List(t"Root", t"Info", t"ID").each: key =>
      pdf.trailerOverrides.at(key).or(pdf.trailer.at(key)).let: value =>
        ascii(t" /$key ")
        appendObject(pdf, raw, ascii, value)

    ascii(t" >>\nstartxref\n$xrefOffset\n%%EOF\n")

    builder.result().immutable(using Unsafe)

  // The bytes to append after `baseOffset` (the original file's length) to record the
  // overlay. Object offsets in the new section are absolute, so they include `baseOffset`.
  def increment(pdf: Pdf, baseOffset: Long): Data raises PdfError =
    val builder = Array.newBuilder[Byte]
    var length = 0L

    def raw(data: Data): Unit =
      builder.addAll(data.mutable(using Unsafe))
      length += data.length

    def ascii(text: Text): Unit = raw(charEncoders.iso88591Encoder.encoded(text))

    // A leading end-of-line guards against the original file not ending in one.
    ascii(t"\n")

    val changed = pdf.overlay.keys.to(List).sorted
    val offsets = scala.collection.mutable.HashMap[Int, Long]()

    changed.each: number =>
      offsets(number) = baseOffset + length
      val generation = pdf.xref.entries.at(number) match
        case Xref.Entry.Direct(_, gen) => gen
        case _                         => 0

      ascii(t"$number $generation obj\n")

      // In an encrypted document, new and edited objects — held in the overlay as cleartext —
      // are encrypted with their own per-object key as they are written.
      val encryption = pdf.guard.let((_, number, generation))
      val value = pdf.guard.lay(pdf.overlay(number)):
        guard => encryptStrings(pdf.overlay(number), guard, number, generation)

      appendObject(pdf, raw, ascii, value, encryption)
      ascii(t"\nendobj\n")

    val xrefOffset = baseOffset + length
    val freed = pdf.freed.to(List).sorted

    // Group the updated and freed object numbers (plus object 0, the free-list head, when
    // anything is freed) into ascending consecutive subsections.
    val numbers = (changed ++ freed ++ (if freed.isEmpty then Nil else List(0))).distinct.sorted

    ascii(t"xref\n")

    subsections(numbers).each: (first, run) =>
      ascii(t"$first ${run.length}\n")

      run.each: number =>
        if number == 0 then ascii(t"0000000000 65535 f \n")
        else if pdf.freed.contains(number) then
          val generation = pdf.xref.entries.at(number) match
            case Xref.Entry.Direct(_, gen) => gen + 1
            case _                         => 1

          ascii(t"0000000000 ${pad5(generation)} f \n")
        else
          val generation = pdf.xref.entries.at(number) match
            case Xref.Entry.Direct(_, gen) => gen
            case _                         => 0

          ascii(t"${pad10(offsets(number))} ${pad5(generation)} n \n")

    val size = pdf.nextNumber

    // The trailer carries forward the original `/Root`, `/Info`, `/Encrypt` and `/ID`, with
    // any write-scope overrides (e.g. a newly-created `/Info`) taking precedence.
    val carried = List(t"Root", t"Info", t"Encrypt", t"ID").flatMap: key =>
      pdf.trailer.at(key).let(value => List(key -> value)).or(Nil)

    val entries = (carried.to(Map) ++ pdf.trailerOverrides).to(List)

    ascii(t"trailer\n<< /Size $size")

    entries.each: (key, value) =>
      ascii(t" /$key ")
      appendObject(pdf, raw, ascii, value)

    pdf.xref.startxref.let: previous =>
      ascii(t" /Prev $previous")

    ascii(t" >>\nstartxref\n$xrefOffset\n%%EOF\n")

    builder.result().immutable(using Unsafe)

  private def appendObject
    ( pdf: Pdf, raw: Data => Unit, ascii: Text => Unit, cos: Cos,
      encryption: Optional[(Guard, Int, Int)] = Unset )
  :   Unit raises PdfError =

    cos match
      case body: Cos.Body =>
        // A stream: its dictionary, then the payload framed by `stream`/`endstream`, with
        // `/Length` recomputed to the (possibly encrypted) byte count.
        val stored = pdf.raw(body)
        val payload = encryption.lay(stored): (guard, number, generation) =>
          guard.encryptStream(stored, number, generation)

        val entries = body.entries.updated(t"Length", Cos.Integral(payload.length.toLong))
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
        Cos.Dictionary(entries.view.mapValues(encryptStrings(_, guard, number, generation)).toMap)

      case Cos.Body(entries, start) =>
        Cos.Body(entries.view.mapValues(encryptStrings(_, guard, number, generation)).toMap, start)

      case other =>
        other

  // Consecutive runs of ascending numbers, each as (first, members).
  private def subsections(numbers: List[Int]): List[(Int, List[Int])] =
    numbers match
      case Nil =>
        Nil

      case head :: _ =>
        val runs = List.newBuilder[(Int, List[Int])]
        var run = List.newBuilder[Int]
        var first = head
        var previous = head - 1

        numbers.each: number =>
          if number == previous + 1 then run += number
          else
            runs += ((first, run.result()))
            run = List.newBuilder[Int]
            run += number
            first = number

          previous = number

        runs += ((first, run.result()))
        runs.result()

  private def pad10(value: Long): Text =
    val digits = value.toString
    ("0".repeat(10 - digits.length).nn + digits).tt

  private def pad5(value: Int): Text =
    val digits = value.toString
    ("0".repeat(5 - digits.length).nn + digits).tt
