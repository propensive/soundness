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


import scala.caps

import proscenium.compat.*

import anticipation.*
import aviation.*
import contingency.*
import denominative.*
import enigmatic.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import phoenicia.*
import pneumatic.*
import prepositional.*
import quantitative.*
import rudiments.*
import vacuous.*
import zephyrine.*

object Pdf:
  // A fresh, empty document: a catalog and an empty page tree, over which a creation scope's
  // edits accumulate before a full write. Built in memory so the write extensions — which
  // resolve through a `Pdf` — work identically to editing an existing file.
  private[facsimile] def blank()(using Tactic[Pdf.Error]): Pdf =
    val catalog = t"1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n"
    val pages = t"2 0 obj\n<< /Type /Pages /Kids [] /Count 0 >>\nendobj\n"
    val body = t"%PDF-1.7\n$catalog$pages"

    val offset1 = body.s.indexOf("1 0 obj")
    val offset2 = body.s.indexOf("2 0 obj")
    val xrefOffset = body.length

    def pad(value: Int): Text =
      val digits = value.toString
      ("0".repeat(10 - digits.length).nn + digits).tt

    val table =
      t"xref\n0 3\n0000000000 65535 f \n${pad(offset1)} 00000 n \n${pad(offset2)} 00000 n \n"

    val trailer = t"trailer\n<< /Size 3 /Root 1 0 R >>\nstartxref\n$xrefOffset\n%%EOF"
    val bytes = charEncoders.iso88591Encoder.encoded(t"$body$table$trailer")
    val source = DataSource(bytes)
    Pdf(source, Xref.load(source), Version(1, 7))

  case class Version(major: Int, minor: Int)

  // An embedded file from the `/EmbeddedFiles` name tree. Its metadata is materialized, but
  // `data` still reads through the document, so an `Attachment` is confined to the scope;
  // call `data` inside and keep the result.
  class Attachment private[facsimile]
    ( pdf:             Pdf,
      val name:        Text,
      val filename:    Optional[Text],
      val description: Optional[Text],
      val mediaType:   Optional[Text],
      body:            Optional[Cos.Body] ):

    def data(using Tactic[Pdf.Error]): Data =
      body.let(pdf.payload(_)).or(abort(Pdf.Error(Pdf.Error.Reason.MissingEntry(t"EF"))))

  // Builds the security handler, if the file is encrypted, and installs it on the document.
  // The `/Encrypt` dictionary and the trailer `/ID` are read before the guard exists — and
  // so are never themselves decrypted — and a wrong password fails here, at open, rather
  // than at first string or stream access. The password's cleartext is read only within
  // `uncloak`, so it is confined to this call; the empty password covers unprotected files.
  private[facsimile] def unlock(pdf: Pdf^, password: Optional[Password])(using Tactic[Pdf.Error]): Unit =
    pdf.trailer(t"Encrypt").let: encryptRef =>
      val encrypt = pdf.resolved(encryptRef).dictionary
        . or(abort(Pdf.Error(Pdf.Error.Reason.UnsupportedEncryption(0))))

      val id = pdf.trailer(t"ID") match
        case Cos.Sequence(first :: _) => first.chars.or(Array.empty[Byte])
        case _                        => Array.empty[Byte]

      pdf.guard = password.lay(Guard(encrypt, id, scala.Array.empty[Char])(using pdf)): password =>
        password.uncloak(Guard(encrypt, id, cleartext.chars)(using pdf))

  // The header comment is nominally at offset 0, but tolerated anywhere in the first 1KiB,
  // matching widespread reader behaviour for files with prepended junk.
  private[facsimile] def readVersion(source: ByteSource)(using Tactic[Pdf.Error]): Version =
    val window = source.read(0L, source.size.min(1024L).toInt)
    val marker = t"%PDF-"

    def digit(byte: Int): Boolean = byte >= '0' && byte <= '9'

    var found: Optional[Version] = Unset

    window.survey: surveyor =>
      while found.absent && surveyor.glimpse(8).present do
        if surveyor.matches(marker) { (byte, char) => (byte & 0xff) == char.toInt } then
          surveyor.glimpse(8).let: eight =>
            val start = (eight: Interval).start.n0
            val major = window.at(Ordinal.zerary(start + 5)).lay(-1)(_ & 0xff)
            val dot = window.at(Ordinal.zerary(start + 6)).lay(-1)(_ & 0xff)
            val minor = window.at(Ordinal.zerary(start + 7)).lay(-1)(_ & 0xff)

            if digit(major) && dot == '.' && digit(minor)
            then found = Version(major - '0', minor - '0')

        if found.absent then surveyor.advance()

    found.or(abort(Pdf.Error(Pdf.Error.Reason.NotPdf)))

  // PdfError → Pdf.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NotPdf                                       extends Reason(1)
      case Truncated                                    extends Reason(2)
      case MissingStartxref                             extends Reason(3)
      case MalformedXref(offset: Long)                  extends Reason(4)
      case Unparseable(offset: Long, expected: Text)    extends Reason(5)
      case MissingObject(objectNumber: Int, generation: Int)  extends Reason(6)
      case CircularReference(objectNumber: Int)                extends Reason(7)
      case MissingEntry(key: Text)                      extends Reason(8)
      case TypeMismatch(key: Text, expected: Text)      extends Reason(9)
      case UnknownFilter(name: Text)                    extends Reason(10)
      case CorruptStream(filter: Text)                  extends Reason(11)
      case MalformedOperator(operator: Text)            extends Reason(12)
      case UnsupportedEncryption(version: Int)          extends Reason(13)
      case BadPassword                                  extends Reason(14)
      case CircularPageTree                             extends Reason(15)
      case Io(detail: Text)                             extends Reason(16)
      case WriteUnsupported                             extends Reason(17)
      case MissingPage(page: Int)                       extends Reason(18)

    given communicable: Reason is Communicable =
      case Reason.NotPdf =>
        m"the file does not begin with a PDF header"

      case Reason.Truncated =>
        m"the PDF file ended unexpectedly"

      case Reason.MissingStartxref =>
        m"no startxref keyword could be found at the end of the file"

      case Reason.MalformedXref(offset) =>
        m"the cross-reference section at offset $offset could not be interpreted"

      case Reason.Unparseable(offset, expected) =>
        m"$expected was expected at offset $offset"

      case Reason.MissingObject(objectNumber, generation) =>
        m"the object $objectNumber $generation was missing or invalid"

      case Reason.CircularReference(objectNumber) =>
        m"resolving the object $objectNumber returned to itself"

      case Reason.MissingEntry(key) =>
        m"the required dictionary entry $key was absent"

      case Reason.TypeMismatch(key, expected) =>
        m"the dictionary entry $key was not $expected"

      case Reason.UnknownFilter(name) =>
        m"the stream filter $name is not recognized"

      case Reason.CorruptStream(filter) =>
        m"a stream could not be decoded with the $filter filter"

      case Reason.MalformedOperator(operator) =>
        m"the content operator $operator had malformed operands"

      case Reason.UnsupportedEncryption(version) =>
        m"the encryption scheme (version $version) is not supported"

      case Reason.BadPassword =>
        m"the password was incorrect"

      case Reason.CircularPageTree =>
        m"the page tree contains a cycle"

      case Reason.Io(detail) =>
        m"an I/O operation failed: $detail"

      case Reason.WriteUnsupported =>
        m"this document cannot be written (only an unencrypted, on-disk file with a valid " +
            m"cross-reference table can be edited in place)"

      case Reason.MissingPage(page) =>
        m"the document has no page $page"

  case class Error(reason: Pdf.Error.Reason)(using Diagnostics)
  extends fulminate.Error(280, reason.number)(m"the PDF could not be read because $reason")

  // PdfFont → Pdf.Font
  object Font:
    enum Standard:
      case Helvetica, HelveticaBold, HelveticaOblique, HelveticaBoldOblique,
          TimesRoman, TimesBold, TimesItalic, TimesBoldItalic,
          Courier, CourierBold, CourierOblique, CourierBoldOblique, Symbol, ZapfDingbats

    // Everything the variants share, materialized at load time so a `Font` is a pure value
    // that outlives the `open` scope.
    private[facsimile] case class Common
      ( baseFont:     Text,
        standard:     Optional[Standard],
        firstChar:    Int,
        widths:       Array[Double]^{},
        cidWidths:    Map[Int, Double],
        defaultWidth: Double,
        encoding:     Optional[Array[Char]^{}],
        differences:  Map[Int, Text],
        toUnicode:    Optional[CharMap],
        embedded:     Optional[Ttf],
        twoByte:      Boolean,
        descriptor:   Map[Text, Cos] )

    // Builds a font from its dictionary; anything unrecognizable is `Unset` rather than an
    // error, since fonts are consulted opportunistically during extraction.
    private[facsimile] def read(value: Cos)(using pdf: Pdf)(using Tactic[Error]): Optional[Font] =
      value.dictionary.let: entries =>
        val subtype = entries(t"Subtype").let(pdf.resolved(_).name).or(t"")
        val baseFont = entries(t"BaseFont").let(pdf.resolved(_).name).or(t"")
        val standard = StandardFonts.recognize(baseFont)

        val descriptor = pdf.resolved(entries(t"FontDescriptor").or(Cos.Nil))
          . dictionary.or(Map[Text, Cos]())

        val defaultWidth = descriptor(t"MissingWidth").let(pdf.resolved(_).double).or(0.0)
        val firstChar = entries(t"FirstChar").let(pdf.resolved(_).long).or(0L).toInt

        val widths: Array[Double]^{} =
          pdf.resolved(entries(t"Widths").or(Cos.Nil)).elements.lay(Array.empty[Double]):
            elements => Array.from(elements.stdlib.map(pdf.resolved(_).double.or(0.0)))

        val embedded: Optional[Ttf] =
          val program = descriptor(t"FontFile2").or:
            descriptor(t"FontFile3").let: value =>
              val body = pdf.resolved(value)
              val subtype = body.dictionary.or(Map[Text, Cos]())(t"Subtype").let(_.name)
              if subtype == t"OpenType" then value else Unset

          program.let(pdf.resolved(_)).let:
            case body: Cos.Body => safely(Ttf(pdf.payload(body)))
            case _              => Unset

        val toUnicode: Optional[CharMap] =
          pdf.resolved(entries(t"ToUnicode").or(Cos.Nil)) match
            case body: Cos.Body => safely(CharMap.parse(pdf.payload(body)))
            case _              => Unset

        // The `/Encoding` entry: a base name, or a dictionary of a base name plus differences.
        // A `match`, not `.let`: the frozen member of the `Optional` union freshens under
        // `let`'s type-variable instantiation.
        def encodingTable(name: Optional[Text]): Optional[Array[Char]^{}] = name.asInstanceOf[Matchable] match
          case t"WinAnsiEncoding"  => PdfEncoding.winAnsi: Array[Char]^{}
          case t"MacRomanEncoding" => PdfEncoding.macRoman: Array[Char]^{}
          case t"StandardEncoding" => PdfEncoding.standard: Array[Char]^{}
          case _                   => Unset

        val encodingValue = pdf.resolved(entries(t"Encoding").or(Cos.Nil))

        val encoding: Optional[Array[Char]^{}] = encodingValue match
          case Cos.Name(name)          => encodingTable(name)
          case dictionary: Cos.Dictionary => encodingTable(dictionary(t"BaseEncoding").let(_.name))
          case _                          => Unset

        val differences: Map[Int, Text] = encodingValue match
          case dictionary: Cos.Dictionary =>
            pdf.resolved(dictionary(t"Differences").or(Cos.Nil)).elements.lay(Map[Int, Text]()):
              elements =>
                var code = 0
                val builder = scala.collection.immutable.Map.newBuilder[Int, Text]

                elements.each:
                  case Cos.Integral(value) =>
                    code = value.toInt

                  case Cos.Name(name) =>
                    builder += code -> PdfEncoding.glyph(name).lay(t"�")(_.toString.tt)
                    code += 1

                  case _ =>
                    ()

                Map.of(builder.result())

          case _ =>
            Map()

        def common(twoByte: Boolean, cidWidths: Map[Int, Double], default: Double) = Common
          ( baseFont, standard, firstChar, widths, cidWidths, default, encoding, differences,
            toUnicode, embedded, twoByte, descriptor )

        subtype.s match
          case "Type1"    => Type1(common(false, Map(), defaultWidth))
          case "MMType1"  => MmType1(common(false, Map(), defaultWidth))
          case "TrueType" => TrueType(common(false, Map(), defaultWidth))

          case "Type3" =>
            val matrix = pdf.resolved(entries(t"FontMatrix").or(Cos.Nil)).elements
              . lay(Matrix(0.001, 0, 0, 0.001, 0, 0)): elements =>
                  elements.map(pdf.resolved(_).double.or(0.0)) match
                    case List(a, b, c, d, e, f) => Matrix(a, b, c, d, e, f)
                    case _                      => Matrix(0.001, 0, 0, 0.001, 0, 0)

            // Type 3 widths are in glyph space; normalize them to thousandths of an em.
            val scaled = Array.frozen(widths.readable.map(_*matrix.a*1000))
            Type3(matrix, common(false, Map(), defaultWidth).copy(widths = scaled))

          case "Type0" =>
            val descendant = pdf.resolved(entries(t"DescendantFonts").or(Cos.Nil)).elements
              . lay(Map[Text, Cos]()): elements =>
                  elements match
                    case List(first) => pdf.resolved(first).dictionary.or(Map[Text, Cos]())
                    case _           => Map[Text, Cos]()

            val cidDescriptor = pdf.resolved(descendant(t"FontDescriptor").or(Cos.Nil))
              . dictionary.or(Map[Text, Cos]())

            val cidEmbedded: Optional[Ttf] =
              cidDescriptor(t"FontFile2").or(cidDescriptor(t"FontFile3"))
              . let(pdf.resolved(_)).let:
                  case body: Cos.Body => safely(Ttf(pdf.payload(body)))
                  case _              => Unset

            val defaultCid = descendant(t"DW").let(pdf.resolved(_).double).or(1000.0)
            val cidWidths = cidWidthArray(descendant(t"W"))

            Type0:
              Common
                ( baseFont, standard, 0, Array.empty[Double], cidWidths, defaultCid, encoding,
                  differences, toUnicode, cidEmbedded, twoByte = true, cidDescriptor )

          case _ =>
            Unset

    // `/W` (ISO 32000-2 §9.7.4.3): `start [w w ...]` lists consecutive widths; `start end w`
    // spans a range.
    private def cidWidthArray(value: Optional[Cos])(using pdf: Pdf)
    ( using Tactic[Error] )
    :   Map[Int, Double] =

      pdf.resolved(value.or(Cos.Nil)).elements.lay(Map[Int, Double]()): elements =>
        val builder = scala.collection.immutable.Map.newBuilder[Int, Double]

        def recur(elements: List[Cos]): Unit = elements match
          case Cos.Integral(start) :: Cos.Sequence(widths) :: rest =>
            widths.stdlib.zipWithIndex.each: (width, index) =>
              pdf.resolved(width).double.let(builder += (start.toInt + index) -> _)

            recur(rest)

          case Cos.Integral(start) :: Cos.Integral(end) :: width :: rest =>
            pdf.resolved(width).double.let: value =>
              var cid = start.toInt

              while cid <= end.toInt do
                builder += cid -> value
                cid += 1

            recur(rest)

          case _ =>
            ()

        recur(elements.map(pdf.resolved(_)))
        Map.of(builder.result())

  // A font as a page's resources declare it (ISO 32000-2 §9): a pure, fully-materialized
  // value. Embedded TrueType and OpenType programs surface as phoenicia `Ttf`s; `decode`
  // maps show-text operands to Unicode as well as the file allows, preferring the font's own
  // `/ToUnicode` map, then its declared encoding and differences.
  enum Font:
    case Type1(common: Font.Common)
    case MmType1(common: Font.Common)
    case TrueType(common: Font.Common)
    case Type3(matrix: Matrix, common: Font.Common)
    case Type0(common: Font.Common)

    private def common: Font.Common = this match
      case Type1(common)    => common
      case MmType1(common)  => common
      case TrueType(common) => common
      case Type3(_, common) => common
      case Type0(common)    => common

    def baseFont: Text = common.baseFont
    def standard: Optional[Font.Standard] = common.standard
    def embedded: Optional[Ttf] = common.embedded
    def descriptor: Map[Text, Cos] = common.descriptor

    // The advance of a code, in thousandths of an em.
    def width(code: Int): Double = this match
      case Type0(common) =>
        common.cidWidths(code).or(common.defaultWidth)

      case _ =>
        val index = code - common.firstChar

        if index >= 0 && index < common.widths.length && common.widths.readUnchecked(index) > 0
        then common.widths.readUnchecked(index)
        else common.standard.lay(or(code)) { standard => StandardFonts.width(standard, code) }

    private def or(code: Int): Double = if common.defaultWidth > 0 then common.defaultWidth else 500

    // Word spacing applies only to the single-byte code 32 (ISO 32000-2 §9.3.3).
    private[facsimile] def wordBoundary(code: Int): Boolean = !common.twoByte && code == 32

    // The byte codes of a show-text operand: single bytes, or big-endian pairs for composite
    // fonts (Identity ordering, the overwhelming norm).
    def codes(string: Data): List[Int] =
      if common.twoByte then
        List.range(0, string.length/2).map: index =>
          ((string.readUnchecked(index*2) & 0xff) << 8) | (string.readUnchecked(index*2 + 1) & 0xff)
      else string.to[List].map(_.toInt & 0xff)

    def decode(string: Data): Text =
      val builder = StringBuilder()

      codes(string).each: code =>
        val mapped: Optional[Text] = common.toUnicode.let(_(code)).or:
          common.differences(code).or:
            common.encoding.let: table =>
              if code >= 0 && code < table.length then table(code).toString.tt else Unset

        val fallback: Text =
          if !common.twoByte && code >= 32 && code <= 126 then code.toChar.toString.tt else t"�"

        builder.append(mapped.or(fallback).s)

      builder.toString.tt

  // PdfInfo → Pdf.Info
  object Info:
    // Serialises document information back to an `/Info` dictionary: text fields as PDF text
    // strings, dates in the `D:` form. Absent fields are omitted.
    private[facsimile] def dictionary(info: Info): Map[Text, Cos] =
      var entries = Map[Text, Cos]()

      def string(key: Text, value: Optional[Text]): Unit =
        value.let { text => entries = entries.updated(key, Cos.Chars(Cos.encodeText(text))) }

      def date(key: Text, value: Optional[Timing]): Unit =
        value.let { timing => entries = entries.updated(key, Cos.Chars(Cos.encodeText(formatDate(timing)))) }

      string(t"Title", info.title)
      string(t"Author", info.author)
      string(t"Subject", info.subject)
      string(t"Keywords", info.keywords)
      string(t"Creator", info.creator)
      string(t"Producer", info.producer)
      date(t"CreationDate", info.created)
      date(t"ModDate", info.modified)
      entries

    private def formatDate(timing: Timing): Text =
      import calendars.gregorianCalendar
      val ts = timing.timestamp

      def pad(n: Int, width: Int): Text =
        val digits = n.toString
        ("0".repeat(width - digits.length).nn + digits).tt

      val year: Int = ts.year.apply()
      val month: Int = ts.month.ordinal + 1
      val day: Int = ts.day.apply()

      val stamp =
        t"D:${pad(year, 4)}${pad(month, 2)}${pad(day, 2)}${pad(ts.hour, 2)}${pad(ts.minute, 2)}${pad(ts.second, 2)}"

      val zone = timing.offset.lay(t""): duration =>
        val seconds = duration.value.toInt

        if seconds == 0 then t"Z" else
          val minutes = (if seconds < 0 then -seconds else seconds)/60
          t"${if seconds < 0 then t"-" else t"+"}${pad(minutes/60, 2)}'${pad(minutes%60, 2)}'"

      t"$stamp$zone"
    // A PDF date (ISO 32000-2 §7.9.4): local time with an *optional* UTC offset — absence
    // means the relationship to UTC is unknown, so a zoneless `Timestamp` carries the moment
    // and the offset rides alongside only when the file stated one.
    case class Timing(timestamp: Timestamp, offset: Optional[Duration])

    // `D:YYYYMMDDHHmmSS±HH'mm'`, everything after the year optional; a malformed date is
    // `Unset`, never an error, since real files abound with slightly-wrong dates.
    private[facsimile] def parseDate(value: Text): Optional[Timing] =
      val content = if value.s.startsWith("D:") then value.s.substring(2).nn else value.s

      def digits(start: Int, length: Int, minimum: Int, maximum: Int): Optional[Int] =
        if start + length > content.length then Unset else
          var i = start
          var number = 0
          var bad = false

          while i < start + length do
            val char = content.charAt(i)
            if char < '0' || char > '9' then bad = true else number = number*10 + (char - '0')
            i += 1

          if bad || number < minimum || number > maximum then Unset else number

      digits(0, 4, 0, 9999).let: year =>
        val month = digits(4, 2, 1, 12).or(1)
        val day = digits(6, 2, 1, 31).or(1)
        val hour = digits(8, 2, 0, 23).or(0)
        val minute = digits(10, 2, 0, 59).or(0)
        val second = digits(12, 2, 0, 59).or(0)

        val offset: Optional[Duration] =
          if content.length > 14 then content.charAt(14) match
            case 'Z' =>
              Quantity[Seconds[1]](0.0)

            case sign @ ('+' | '-') =>
              digits(15, 2, 0, 23).let: hours =>
                val minutes = digits(18, 2, 0, 59).or(0)
                val seconds = (hours*3600 + minutes*60)*(if sign == '-' then -1 else 1)
                Quantity[Seconds[1]](seconds.toDouble)

            case _ =>
              Unset
          else Unset

        import calendars.gregorianCalendar

        safely(Timestamp(Date(Year(year), Month(month), Day(day)),
            Clockface(Base24(hour), Base60(minute), Base60(second)))).let: timestamp =>
          Timing(timestamp, offset)

  // The document-information dictionary, fully materialized: a pure value that outlives the
  // `open` scope.
  case class Info
    ( title:    Optional[Text],
      author:   Optional[Text],
      subject:  Optional[Text],
      keywords: Optional[Text],
      creator:  Optional[Text],
      producer: Optional[Text],
      created:  Optional[Info.Timing],
      modified: Optional[Info.Timing] )

  // PdfMatrix → Pdf.Matrix
  object Matrix:
    val Identity: Matrix = Matrix(1, 0, 0, 1, 0, 0)

  // The six live entries of a PDF transformation matrix (ISO 32000-2 §8.3.4):
  //
  //   ⎡ a b 0 ⎤
  //   ⎢ c d 0 ⎥
  //   ⎣ e f 1 ⎦
  //
  // applied to row vectors, so `this * that` transforms by `this` first.
  case class Matrix(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double):
    def * (that: Matrix): Matrix =
      Matrix
        ( a*that.a + b*that.c,
          a*that.b + b*that.d,
          c*that.a + d*that.c,
          c*that.b + d*that.d,
          e*that.a + f*that.c + that.e,
          e*that.b + f*that.d + that.f )

    def apply(x: Double, y: Double): (Double, Double) = (a*x + c*y + e, b*x + d*y + f)

  // PdfOperator → Pdf.Operator
  object Operator:
    enum LineCap:
      case Butt, Round, Square

    enum LineJoin:
      case Miter, Round, Bevel

    enum FillRule:
      case NonZero, EvenOdd

    enum TextRenderMode:
      case Fill, Stroke, FillStroke, Invisible, FillClip, StrokeClip, FillStrokeClip, Clip

    // Interprets one lexed instruction as a typed operator. Unknown operators become
    // `Unrecognized` — required inside `BX`/`EX` compatibility sections, and kind to the
    // future — while known operators with malformed operands are errors.
    private[facsimile] def read(instruction: ContentTokens.Instruction)
    ( using Tactic[Error] )
    :   Operator =

      val operands = instruction.operands
      val operator = instruction.operator

      def malformed: Nothing = abort(Error(Error.Reason.MalformedOperator(operator)))

      def numbers(count: Int): List[Double] =
        val values = operands.bind(_.double.lay(List())(List(_)))
        if values.stdlib.length != count || operands.stdlib.length != count then malformed else values

      def name(index: Int): Text =
        if index < operands.stdlib.length then operands.stdlib(index).name.or(malformed) else malformed

      def chars(index: Int): Data =
        if index < operands.stdlib.length then operands.stdlib(index).chars.or(malformed) else malformed

      def matrix: Matrix = numbers(6) match
        case List(a, b, c, d, e, f) => Matrix(a, b, c, d, e, f)
        case _                      => malformed

      def int(limit: Int): Int =
        val value = numbers(1).stdlib(0).toInt
        if value < 0 || value >= limit then malformed else value

      def pair(index: Int): Optional[Cos] =
        if operands.stdlib.length > index then operands.stdlib(index) else Unset

      operator.s match
        // Graphics state (ISO 32000-2 §8.4.4)
        case "q"  => Save
        case "Q"  => Restore
        case "cm" => Concat(matrix)
        case "w"  => SetLineWidth(numbers(1).stdlib(0))
        case "J"  => SetLineCap(LineCap.fromOrdinal(int(LineCap.values.length)))
        case "j"  => SetLineJoin(LineJoin.fromOrdinal(int(LineJoin.values.length)))
        case "M"  => SetMiterLimit(numbers(1).stdlib(0))
        case "ri" => SetIntent(name(0))
        case "i"  => SetFlatness(numbers(1).stdlib(0))
        case "gs" => SetParameters(name(0))

        case "d" => operands match
          case List(Cos.Sequence(elements), phase) =>
            SetDashPattern(elements.map(_.double.or(malformed)), phase.double.or(malformed))

          case _ =>
            malformed

        // Path construction and painting (§8.5)
        case "m" => numbers(2) match
          case List(x, y) => Move(x, y)
          case _          => malformed

        case "l" => numbers(2) match
          case List(x, y) => Line(x, y)
          case _          => malformed

        case "c" => numbers(6) match
          case List(x1, y1, x2, y2, x3, y3) => Cubic(x1, y1, x2, y2, x3, y3)
          case _                            => malformed

        case "v" => numbers(4) match
          case List(x2, y2, x3, y3) => CubicStart(x2, y2, x3, y3)
          case _                    => malformed

        case "y" => numbers(4) match
          case List(x1, y1, x3, y3) => CubicEnd(x1, y1, x3, y3)
          case _                    => malformed

        case "re" => numbers(4) match
          case List(x, y, width, height) => Rectangle(x, y, width, height)
          case _                         => malformed

        case "h"  => Close
        case "S"  => Stroke
        case "s"  => CloseStroke
        case "f"  => Fill(FillRule.NonZero)
        case "F"  => Fill(FillRule.NonZero)
        case "f*" => Fill(FillRule.EvenOdd)
        case "B"  => FillStroke(FillRule.NonZero)
        case "B*" => FillStroke(FillRule.EvenOdd)
        case "b"  => CloseFillStroke(FillRule.NonZero)
        case "b*" => CloseFillStroke(FillRule.EvenOdd)
        case "n"  => EndPath
        case "W"  => Clip(FillRule.NonZero)
        case "W*" => Clip(FillRule.EvenOdd)

        // Text (§9.4)
        case "BT" => BeginText
        case "ET" => EndText
        case "Td" => numbers(2) match
          case List(dx, dy) => Offset(dx, dy)
          case _            => malformed

        case "TD" => numbers(2) match
          case List(dx, dy) => OffsetLeading(dx, dy)
          case _            => malformed
        case "Tm" => SetTextMatrix(matrix)
        case "T*" => NextLine
        case "Tc" => SetCharSpacing(numbers(1).stdlib(0))
        case "Tw" => SetWordSpacing(numbers(1).stdlib(0))
        case "Tz" => SetScaling(numbers(1).stdlib(0))
        case "TL" => SetLeading(numbers(1).stdlib(0))
        case "Tr" => SetRenderMode(TextRenderMode.fromOrdinal(int(TextRenderMode.values.length)))
        case "Ts" => SetRise(numbers(1).stdlib(0))
        case "Tj" => ShowText(chars(0))
        case "'"  => NextLineShow(chars(0))

        case "Tf" => operands match
          case List(Cos.Name(font), size) => SetFont(font, size.double.or(malformed))
          case _                          => malformed

        case "TJ" => operands match
          case List(Cos.Sequence(elements)) =>
            ShowTexts:
              elements.map: element => element.chars.or(element.double.or(malformed))

          case _ =>
            malformed

        case "\"" => operands match
          case List(word, char, Cos.Chars(bytes)) =>
            NextLineShowSpaced(word.double.or(malformed), char.double.or(malformed), bytes)

          case _ =>
            malformed

        // Colour (§8.6.8)
        case "CS" => StrokeSpace(name(0))
        case "cs" => FillSpace(name(0))
        case "G"  => StrokeGray(numbers(1).stdlib(0))
        case "g"  => FillGray(numbers(1).stdlib(0))

        case "RG" => numbers(3) match
          case List(red, green, blue) => StrokeRgb(Srgb(red, green, blue))
          case _                      => malformed

        case "rg" => numbers(3) match
          case List(red, green, blue) => FillRgb(Srgb(red, green, blue))
          case _                      => malformed

        case "K" => numbers(4) match
          case List(cyan, magenta, yellow, key) => StrokeCmyk(Cmyk(cyan, magenta, yellow, key))
          case _                                => malformed

        case "k" => numbers(4) match
          case List(cyan, magenta, yellow, key) => FillCmyk(Cmyk(cyan, magenta, yellow, key))
          case _                                => malformed

        case "SC" | "SCN" =>
          val (values, pattern) = components(operands, malformed)
          StrokeColor(values, pattern)

        case "sc" | "scn" =>
          val (values, pattern) = components(operands, malformed)
          FillColor(values, pattern)

        // XObjects, inline images and shading (§8.8–8.10)
        case "Do" => Draw(name(0))
        case "sh" => Shade(name(0))

        case "BI" => operands match
          case List(Cos.Dictionary(entries), Cos.Chars(data))  => InlineImage(entries, data)
          case _                                               => malformed

        // Marked content (§14.6) and compatibility (§7.8.2)
        case "MP"  => MarkPoint(name(0), Unset)
        case "DP"  => MarkPoint(name(0), pair(1))
        case "BMC" => BeginMarked(name(0), Unset)
        case "BDC" => BeginMarked(name(0), pair(1))
        case "EMC" => EndMarked
        case "BX"  => BeginCompatibility
        case "EX"  => EndCompatibility

        // Type 3 glyph metrics (§9.6.4)
        case "d0" => numbers(2) match
          case List(wx, wy) => GlyphWidth(wx, wy)
          case _            => malformed

        case "d1" => numbers(6) match
          case List(wx, wy, llx, lly, urx, ury) => GlyphMetrics(wx, wy, llx, lly, urx, ury)
          case _                                => malformed

        case _ =>
          Unrecognized(operator, operands)

    private def components(operands: List[Cos], malformed: => Nothing)
    :   (List[Double], Optional[Text]) =

      operands.reverse match
        case Cos.Name(pattern) :: rest => (rest.reverse.map(_.double.or(malformed)), pattern)
        case all                       => (all.reverse.map(_.double.or(malformed)), Unset)

  // A content-stream operator with typed operands (ISO 32000-2 §8–9, §14.6). Coordinates stay
  // `Double`: they are user-space values, meaningful only under the current transformation
  // matrix. Show-text operands stay encoded `Data`: mapping them to text needs the font. The
  // AST is deliberately symmetric enough to serialize back — the seed of the future writer.
  enum Operator:
    // Graphics state
    case Save, Restore
    case Concat(matrix: Matrix)
    case SetLineWidth(width: Double)
    case SetLineCap(cap: Operator.LineCap)
    case SetLineJoin(join: Operator.LineJoin)
    case SetMiterLimit(limit: Double)
    case SetDashPattern(pattern: List[Double], phase: Double)
    case SetIntent(intent: Text)
    case SetFlatness(tolerance: Double)
    case SetParameters(name: Text)

    // Path construction and painting
    case Move(x: Double, y: Double)
    case Line(x: Double, y: Double)
    case Cubic(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double)
    case CubicStart(x2: Double, y2: Double, x3: Double, y3: Double)
    case CubicEnd(x1: Double, y1: Double, x3: Double, y3: Double)
    case Close
    case Rectangle(x: Double, y: Double, width: Double, height: Double)
    case Stroke, CloseStroke
    case Fill(rule: Operator.FillRule)
    case FillStroke(rule: Operator.FillRule)
    case CloseFillStroke(rule: Operator.FillRule)
    case EndPath
    case Clip(rule: Operator.FillRule)

    // Text
    case BeginText, EndText
    case Offset(dx: Double, dy: Double)
    case OffsetLeading(dx: Double, dy: Double)
    case SetTextMatrix(matrix: Matrix)
    case NextLine
    case SetCharSpacing(space: Double)
    case SetWordSpacing(space: Double)
    case SetScaling(percent: Double)
    case SetLeading(leading: Double)
    case SetFont(font: Text, size: Double)
    case SetRenderMode(mode: Operator.TextRenderMode)
    case SetRise(rise: Double)
    case ShowText(string: Data)
    case ShowTexts(elements: List[Data | Double])
    case NextLineShow(string: Data)
    case NextLineShowSpaced(word: Double, char: Double, string: Data)

    // Colour
    case StrokeSpace(name: Text)
    case FillSpace(name: Text)
    case StrokeColor(components: List[Double], pattern: Optional[Text])
    case FillColor(components: List[Double], pattern: Optional[Text])
    case StrokeGray(gray: Double)
    case FillGray(gray: Double)
    case StrokeRgb(color: Srgb)
    case FillRgb(color: Srgb)
    case StrokeCmyk(color: Cmyk)
    case FillCmyk(color: Cmyk)

    // XObjects, inline images and shading
    case Draw(name: Text)
    case InlineImage(parameters: Map[Text, Cos], data: Data)
    case Shade(name: Text)

    // Marked content and compatibility
    case MarkPoint(tag: Text, properties: Optional[Cos])
    case BeginMarked(tag: Text, properties: Optional[Cos])
    case EndMarked
    case BeginCompatibility, EndCompatibility

    // Type 3 glyph metrics
    case GlyphWidth(wx: Double, wy: Double)
    case GlyphMetrics(wx: Double, wy: Double, llx: Double, lly: Double, urx: Double, ury: Double)

    case Unrecognized(operator: Text, operands: List[Cos])

  // PdfRect → Pdf.Rect
  object Rect:
    import symbolism.*

    // A `/Rect`-shaped array: four numbers whose corners may be given in any order, normalized
    // here to lower-left and upper-right (ISO 32000-2 §7.9.5). `scale` carries `/UserUnit`,
    // converting user-space units to points.
    private[facsimile] def read(cos: Cos, scale: Double)(using pdf: Pdf)
    ( using Tactic[Error] )
    :   Optional[Rect] =

      pdf.resolved(cos).elements.let: elements =>
        if elements.length != 4 then Unset else
          val values = elements.map(pdf.resolved(_).double.or(0.0)*scale)

          Rect
            ( Quantity[Points[1]](values.stdlib(0).min(values.stdlib(2))),
              Quantity[Points[1]](values.stdlib(1).min(values.stdlib(3))),
              Quantity[Points[1]](values.stdlib(0).max(values.stdlib(2))),
              Quantity[Points[1]](values.stdlib(1).max(values.stdlib(3))) )

  // A rectangle in default user space, held as typesafe lengths: one PDF point is exactly
  // 1/72 inch, which is `quantitative`'s `Points` unit.
  case class Rect
    ( left:   Quantity[Points[1]],
      bottom: Quantity[Points[1]],
      right:  Quantity[Points[1]],
      top:    Quantity[Points[1]] ):

    import symbolism.*

    def width: Quantity[Points[1]] = right - left
    def height: Quantity[Points[1]] = top - bottom

// An open PDF document: the scoped capability at the heart of the module. It owns the byte
// source and a cache of resolved objects, so anything that can still touch the file — the
// `Pdf` itself, and any lazily-resolving value that captures it — is confined to the `open`
// scope by capture checking, while decoded `Data` and parsed `Cos` values are pure and
// escape freely. No method anywhere dereferences a `Cos.Body` except through this class.
class Pdf private[facsimile]
  ( private[facsimile] val source: ByteSource,
    private[facsimile] val xref: Xref,
    val version: Pdf.Version )
extends caps.ExclusiveCapability:

  private val cache: scala.collection.mutable.HashMap[Int, Cos] =
    scala.collection.mutable.HashMap()

  private val loading: scala.collection.mutable.HashSet[Int] =
    scala.collection.mutable.HashSet()

  private val containers: scala.collection.mutable.HashMap[Int, ObjectStream] =
    scala.collection.mutable.HashMap()

  // The stream at each recorded payload offset belongs to this indirect object, so its bytes
  // can be decrypted with the right per-object key. Populated as `Direct` objects load.
  private val streamOwners: scala.collection.mutable.HashMap[Long, (Int, Int)] =
    scala.collection.mutable.HashMap()

  // The security handler, installed by `Pdf.unlock` after the document exists (it must read
  // the unencrypted `/Encrypt` dictionary through this same document first).
  @scala.caps.unsafe.untrackedCaptures
  private[facsimile] var guard: Optional[Guard] = Unset

  // The write overlay: an in-memory incremental update layered over the immutable read model.
  // New and replaced objects live in `overlay`, deleted objects in `freed`; `apply` consults
  // them first, so every existing read view reflects pending edits with no change of its own.
  // Empty in a read-only session; serialised and appended to the file when the write scope
  // closes.
  private[facsimile] val overlay: scala.collection.mutable.HashMap[Int, Cos] =
    scala.collection.mutable.HashMap()

  private[facsimile] val freed: scala.collection.mutable.HashSet[Int] =
    scala.collection.mutable.HashSet()

  // The next free object number, one past the largest the original file used.
  @scala.caps.unsafe.untrackedCaptures
  private[facsimile] var nextNumber: Int =
    (xref.entries.keys.maxOption.getOrElse(0).max(trailer(t"Size").let(_.long).or(0L).toInt - 1)) + 1

  // Payloads for streams created during the write scope. A `Cos.Body` locates its bytes by a
  // file offset; a new stream has none, so it is given a negative sentinel `start` that keys
  // its bytes here — inert to a reader, resolved to these bytes by `raw` and the writer.
  private[facsimile] val newStreams: scala.collection.mutable.HashMap[Long, Data] =
    scala.collection.mutable.HashMap()

  @scala.caps.unsafe.untrackedCaptures
  private var nextStreamId: Long = -1L

  // Trailer entries set or overridden during the write scope — e.g. a newly-created `/Info`
  // reference. Merged over the original trailer's carried-forward entries by the writer.
  private[facsimile] val trailerOverrides: scala.collection.mutable.HashMap[Text, Cos] =
    scala.collection.mutable.HashMap()

  // A new stream object carrying inline bytes, for content the write scope produces.
  private[facsimile] def newBody(entries: Map[Text, Cos], data: Data): Cos.Body =
    val id = nextStreamId
    nextStreamId -= 1
    newStreams(id) = data
    Cos.Body(entries.updated(t"Length", Cos.Integral(data.length.toLong)), id)

  private[facsimile] def dirty: Boolean =
    overlay.nonEmpty || freed.nonEmpty || trailerOverrides.nonEmpty

  // Records a new value for an existing or new object number; un-frees it if it was deleted.
  private[facsimile] def put(number: Int, value: Cos): Unit =
    overlay(number) = value
    freed.remove(number)

  // Allocates a fresh object number for a new value, returning a reference to it.
  private[facsimile] def allocate(value: Cos): Cos.Ref =
    val number = nextNumber
    nextNumber += 1
    overlay(number) = value
    Cos.Ref(number, 0)

  // Marks an object deleted: dropped from the overlay, and — if it exists in the base file —
  // recorded as freed so the incremental update writes a free entry for it.
  private[facsimile] def remove(number: Int): Unit =
    overlay.remove(number)
    if xref.entries.defines(number) then freed += number

  // Rewrites an object's dictionary in place, reading its current value (overlay-aware) so
  // successive edits within a scope compose.
  private[facsimile] def editDictionary(number: Int)(transform: Map[Text, Cos] => Map[Text, Cos])
  ( using Tactic[Pdf.Error] )
  :   Unit =

    put(number, Cos.Dictionary(transform(apply(number).dictionary.or(Map[Text, Cos]()))))

  // Rewrites the catalog (the `/Root` object) in place.
  private[facsimile] def editCatalog(transform: Map[Text, Cos] => Map[Text, Cos])
  ( using Tactic[Pdf.Error] )
  :   Unit =

    trailer(t"Root") match
      case ref: Cos.Ref => editDictionary(ref.number)(transform)
      case _            => ()

  // A reference to the page at a position in the flattened page sequence, for destinations.
  private[facsimile] def pageReference(ordinal: Ordinal)(using Tactic[Pdf.Error]): Optional[Cos.Ref] =
    val entries = pageEntries
    // The bounds check and the lookup are the same act: a confined ordinal deindexes bare.
    entries.confine(ordinal.n0.z).let { position => entries(position)(0).let(Cos.Ref(_, 0)) }

  def trailer: Map[Text, Cos] = xref.trailer

  def encrypted: Boolean = trailer.defines(t"Encrypt")

  def catalog(using Tactic[Pdf.Error]): Map[Text, Cos] =
    resolved(trailer(t"Root").or(Cos.Nil)).dictionary
    . or(abort(Pdf.Error(Pdf.Error.Reason.MissingEntry(t"Root"))))

  // The page tree flattened into reading order, with the inheritable attributes accumulated
  // along each path; the object number of each leaf is kept so that destinations can refer
  // back to a page by reference.
  private[facsimile] def pageEntries
  ( using Tactic[Pdf.Error] )
  :   Sequence[(Optional[Int], Map[Text, Cos], Page.Inherited)] =

    var visited: Set[Int] = Set()

    def recur(node: Cos, number: Optional[Int], inherited: Page.Inherited)
    :   Sequence[(Optional[Int], Map[Text, Cos], Page.Inherited)] =

      node match
        case Cos.Ref(reference, _) =>
          if visited.has(reference)
          then abort(Pdf.Error(Pdf.Error.Reason.CircularPageTree))

          visited += reference
          recur(resolved(node), reference, inherited)

        case Cos.Dictionary(entries) => entries(t"Type").let(_.name) match
          case t"Pages" =>
            val updated = inherited.update(entries)

            resolved(entries(t"Kids").or(Cos.Nil)).elements.lay(Sequence()): kids =>
              kids.to[Sequence].flatMap(recur(_, Unset, updated))

          case _ =>
            Sequence((number, entries, inherited))

        case _ =>
          Sequence()

    recur(catalog(t"Pages").or(Cos.Nil), Unset, Page.Inherited())

  // Pages are exposed by position rather than as a collection: a `Page` captures its
  // document, and capture-carrying elements do not yet flow through the opaque collections'
  // typeclass surface (their element positions box). Positional access is total through the
  // error channel every caller already carries.
  def page(ordinal: Ordinal)(using Tactic[Pdf.Error]): Page^{this} =
    val entries = pageEntries

    entries(ordinal).lay(abort(Pdf.Error(Pdf.Error.Reason.MissingPage(ordinal.n1)))): entry =>
      Page(this, ordinal, entry(0), entry(1), entry(2))

  def pageCount(using Tactic[Pdf.Error]): Int = pageEntries.length

  // Leaf object numbers mapped to positions in the flattened page sequence, for resolving
  // destinations that refer to pages by reference.
  private[facsimile] def pageNumbers(using Tactic[Pdf.Error]): Map[Int, Ordinal] =
    pageEntries.zipWithIndex.flatMap: (entry, index) =>
      entry(0).lay(Sequence()): number =>
        Sequence(number -> index.z)

    . pipe { sequence => Map.from(sequence.stdlib) }

  // Named destinations from both homes: the old-style `/Dests` dictionary and the
  // `/Names /Dests` name tree, still as raw COS values.
  private[facsimile] def rawDestinations(using Tactic[Pdf.Error]): Map[Text, Cos] =
    val old = resolved(catalog(t"Dests").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())

    val tree = resolved(catalog(t"Names").or(Cos.Nil))(t"Dests")
      . let(Trees.names(_)(using this).stdlib.pipe(Map.from(_))).or(Map[Text, Cos]())

    Map.of(old.stdlib ++ (tree: Map[Text, Cos]).stdlib)

  def destinations(using Tactic[Pdf.Error]): Map[Text, Destination] =
    val pages = pageNumbers
    val raw = rawDestinations

    raw.toList.bind: (name, value) =>
      Destination.read(value, pages, raw(_))(using this)
      . lay(List[(Text, Destination)]()): destination =>
          List(name -> destination)

    . toMap

  def bookmarks(using Tactic[Pdf.Error]): List[Bookmark] =
    val pages = pageNumbers
    val raw = rawDestinations
    var visited: Set[Int] = Set()

    // `/Dest` directly, or the `/D` of a `/GoTo` action.
    def target(entries: Map[Text, Cos])(using Tactic[Pdf.Error]): Optional[Cos] =
      entries(t"Dest").or:
        val action = resolved(entries(t"A").or(Cos.Nil))

        if action(t"S").let(_.name).or(t"") == t"GoTo" then action(t"D") else Unset

    def item(value: Cos)(using Tactic[Pdf.Error]): List[Bookmark] = value match
      case Cos.Ref(number, _) =>
        if visited.has(number) then List() else
          visited += number
          item(resolved(value))

      case Cos.Dictionary(entries) =>
        val title = entries(t"Title").let(resolved(_).text).or(t"")

        val destination =
          target(entries).let(Destination.read(_, pages, raw(_))(using this))

        Bookmark(title, destination, chain(entries(t"First"))) ::
          chain(entries(t"Next"))

      case _ =>
        List()

    def chain(first: Optional[Cos])(using Tactic[Pdf.Error]): List[Bookmark] =
      first.lay(List())(item(_))

    chain(resolved(catalog(t"Outlines").or(Cos.Nil))(t"First"))

  def attachments(using Tactic[Pdf.Error]): List[Pdf.Attachment^{this}] =
    resolved(catalog(t"Names").or(Cos.Nil))(t"EmbeddedFiles").lay(List()): tree =>
      Trees.names(tree)(using this).map: (name, value) =>
        val spec = resolved(value).dictionary.or(Map[Text, Cos]())
        val filename = spec(t"UF").or(spec(t"F")).let(resolved(_).text)
        val description = spec(t"Desc").let(resolved(_).text)
        val files = resolved(spec(t"EF").or(Cos.Nil))

        val body: Optional[Cos.Body] =
          resolved(files(t"UF").or(files(t"F")).or(Cos.Nil)) match
            case body: Cos.Body => body
            case _              => Unset

        val mediaType = body.let(_.entries(t"Subtype")).let(_.name)
        Pdf.Attachment(this, name, filename, description, mediaType, body)

  // The label a viewer displays for a page (ISO 32000-2 §12.4.2): styled and prefixed by
  // the `/PageLabels` number tree, or the plain one-based page number when absent.
  def pageLabel(index: Ordinal)(using Tactic[Pdf.Error]): Text =
    catalog(t"PageLabels").lay(index.n1.toString.tt): tree =>
      val ranges = Trees.numbers(tree)(using this).filter(_(0) <= index.n0)

      if ranges.isEmpty then index.n1.toString.tt else
        val (start, value) = ranges.maxBy(_(0))
        val entries = resolved(value).dictionary.or(Map[Text, Cos]())
        val prefix = entries(t"P").let(resolved(_).text).or(t"")
        val first = entries(t"St").let(resolved(_).long).or(1L)
        val number = first + (index.n0 - start)

        val formatted = entries(t"S").let(resolved(_).name).lay(t""):
          case t"D" => number.toString.tt
          case t"R" => roman(number)
          case t"r" => roman(number).s.toLowerCase.nn.tt
          case t"A" => alphabetic(number)
          case t"a" => alphabetic(number).s.toLowerCase.nn.tt
          case _    => t""

        t"$prefix$formatted"

  private def roman(number: Long): Text =
    val numerals =
      List
        ( 1000L -> "M", 900L -> "CM", 500L -> "D", 400L -> "CD", 100L -> "C", 90L -> "XC",
          50L -> "L", 40L -> "XL", 10L -> "X", 9L -> "IX", 5L -> "V", 4L -> "IV", 1L -> "I" )

    def recur(remaining: Long, numerals: List[(Long, String)], result: String): String =
      numerals match
        case (value, numeral) :: rest =>
          if remaining >= value then recur(remaining - value, numerals, result + numeral)
          else recur(remaining, rest, result)

        case _ =>
          result

    if number <= 0 then t"" else recur(number, numerals, "").tt

  // A, B, ..., Z, AA, BB, ..., ZZ, AAA, ... — the same letter repeated, per the spec.
  private def alphabetic(number: Long): Text =
    if number <= 0 then t"" else
      val letter = ('A' + ((number - 1)%26)).toChar.toString
      letter.repeat((((number - 1)/26) + 1).toInt).nn.tt

  // The document-level XMP packet, undecoded: XML parsing belongs downstream.
  def xmp(using Tactic[Pdf.Error]): Optional[Data] =
    resolved(catalog(t"Metadata").or(Cos.Nil)) match
      case body: Cos.Body => payload(body)
      case _              => Unset

  def info(using Tactic[Pdf.Error]): Pdf.Info =
    val entries = resolved(trailer(t"Info").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())
    def field(key: Text): Optional[Text] = entries(key).let(resolved(_).text)

    Pdf.Info
      ( field(t"Title"), field(t"Author"), field(t"Subject"), field(t"Keywords"),
        field(t"Creator"), field(t"Producer"),
        field(t"CreationDate").let(Pdf.Info.parseDate(_)),
        field(t"ModDate").let(Pdf.Info.parseDate(_)) )

  def apply(ref: Cos.Ref)(using Tactic[Pdf.Error]): Cos = apply(ref.number, ref.generation)

  // Resolves an object by number and generation: from the write overlay, the cache, its
  // recorded file offset, or a containing object stream. A freed, missing or invalid entry,
  // or a generation mismatch, is `null` per ISO 32000-2 §7.3.10.
  def apply(number: Int, generation: Int = 0)(using Tactic[Pdf.Error]): Cos =
    if freed.contains(number) then Cos.Nil
    else overlay.at(number).or(cache.at(number).or(load(number, generation)))

  private def load(number: Int, generation: Int)(using Tactic[Pdf.Error]): Cos =
    if !loading.add(number) then abort(Pdf.Error(Pdf.Error.Reason.CircularReference(number)))

    try
      val resolution = xref.entries(number) match
        case Xref.Entry.Direct(offset, expected) =>
          if expected != generation then Cos.Nil else
            // If the recorded offset does not hold this object — a corrupt or shifted
            // cross-reference table — fall back to the offset found by a full-file scan.
            val content = atOffset(number, generation, offset).or:
              recoveredOffset(number).let(atOffset(number, generation, _)).or(Cos.Nil)

            // A top-level indirect stream: record its owner so its payload can be
            // decrypted with the right key.
            content match
              case Cos.Body(_, start) => streamOwners(start) = (number, generation)
              case _                  => ()

            // Strings in a directly-stored object are encrypted individually; those inside
            // an object stream travel in its already-decrypted payload, so are skipped.
            guard.lay(content)(decryptStrings(content, number, generation, _))

        case Xref.Entry.Compressed(container, index) =>
          if generation != 0 then Cos.Nil else
            containerStream(container)(number)
            . or(abort(Pdf.Error(Pdf.Error.Reason.MissingObject(number, generation))))

        case _ =>
          Cos.Nil

      cache(number) = resolution
      resolution
    finally loading.remove(number)

  // Parses the object at an offset, returning its content only if the header matches the
  // number and generation asked for; a mismatch (a lie in the cross-reference table) is
  // `Unset`, so the caller can try a recovered offset instead.
  private def atOffset(number: Int, generation: Int, offset: Long)(using Tactic[Pdf.Error]): Optional[Cos] =
    if offset < 0 || offset >= source.size then Unset else
      safely(CosParser(CosLexer(new Scan(source, offset))).indirect()).let: (found, gen, content) =>
        if found == number && gen == generation then content else Unset

  // A cross-reference table rebuilt by scanning the whole file for objects, computed once and
  // only when the recorded table is found to be lying.
  private lazy val recovered: Xref = safely(Xref.rebuild(source)).or(Xref(Map(), Map()))

  private def recoveredOffset(number: Int): Optional[Long] = recovered.entries(number).let:
    case Xref.Entry.Direct(offset, _) => offset
    case _                            => Unset

  def resolved(value: Cos)(using Tactic[Pdf.Error]): Cos = value match
    case ref: Cos.Ref => apply(ref)
    case other        => other

  // Rewrites every string in an object with its decrypted bytes. A stream body's own
  // dictionary is decrypted, but its payload is left to `raw`; nothing is done for `Ref`s,
  // which resolve to their own separately-decrypted objects.
  private def decryptStrings(value: Cos, number: Int, generation: Int, guard: Guard): Cos =
    value match
      case Cos.Chars(bytes) =>
        Cos.Chars(guard.string(bytes, number, generation))

      case Cos.Sequence(elements) =>
        Cos.Sequence(elements.map(decryptStrings(_, number, generation, guard)))

      case Cos.Dictionary(entries) =>
        Cos.Dictionary(Map.of(entries.stdlib.view.mapValues(decryptStrings(_, number, generation, guard)).toMap))

      case Cos.Body(entries, start) =>
        val decrypted = Map.of(entries.stdlib.view.mapValues(decryptStrings(_, number, generation, guard)).toMap)
        Cos.Body(decrypted, start)

      case other =>
        other

  // The decoded content of a stream, decrypted (in a later milestone) and passed through its
  // filter chain, which stops at terminal image codecs. `/Length` may be indirect; filters in
  // a general stream may be too, so the chain inputs are resolved through this document.
  def payload(body: Cos.Body)(using Tactic[Pdf.Error]): Data =
    val chain =
      Filter.chain
        ( body.entries(t"Filter").let(deepResolved(_)),
          body.entries(t"DecodeParms").let(deepResolved(_)) )

    Filter.decode(raw(body), chain)

  // A re-materializable streaming view of the decoded payload: each `apply()` mints a fresh
  // pull endpoint reading the raw range in chunks and decoding through the filter chain, so
  // a large image or embedded file is never materialized whole. The endpoint reads through
  // this document, so — like everything that does — it cannot outlive the `open` scope.
  def spring(body: Cos.Body)(using tactic: Tactic[Pdf.Error]): Spring[Data]^{this, tactic} =
    val chain =
      Filter.chain
        ( body.entries(t"Filter").let(deepResolved(_)),
          body.entries(t"DecodeParms").let(deepResolved(_)) )

    val steps = Filter.steps(chain)
    val start = body.start
    val end = payloadEnd(body)

    // An encrypted stream is decrypted whole before filtering (a cipher spans the payload),
    // so it starts the pipeline as one materialized chunk; a plain stream reads in chunks.
    val decrypted: Optional[Data] = if encryptedStream(body) then raw(body) else Unset

    new Spring[Data]:
      def apply(): (Stream[Data] over Credit)^ =
        // Both branches build the pipeline over this document's own single-owner data.
        scala.caps.unsafe.unsafeAssumeSeparate:
          decrypted.lay(pipeline(steps, Stream(ranges(start, end)))): data =>
            pipeline(steps, Stream(List(data).iterator))

  // Interprets a streaming plan, minting each duct at its `via` call site.
  private def pipeline[plan^]
    ( steps: List[Filter.Step^{plan}], consume stream: (Stream[Data] over Credit)^ )
  :   (Stream[Data] over Credit)^ =

    steps match
      case Filter.Step.Inflate :: rest =>
        pipeline(rest, stream.viaDuct(pneumatic.Zlib.compression.decompressor()))

      case Filter.Step.Unlzw(earlyChange) :: rest =>
        pipeline(rest, stream.viaDuct(pneumatic.Lzw.decompressor(earlyChange)))

      case Filter.Step.Gather(transform) :: rest =>
        pipeline(rest, stream.viaDuct(Gathering(transform)))

      case _ =>
        stream

  // Chunked positional reads over a raw range: the pull side of `spring`.
  private def ranges(start: Long, end: Long): Iterator[Data]^{this} = new Iterator[Data]:
    @scala.caps.unsafe.untrackedCaptures
    private var position: Long = start

    def hasNext: Boolean = position < end

    def next(): Data =
      val length = (end - position).min(65536L).toInt
      val chunk = source.read(position, length)
      position = if chunk.length == 0 then end else position + chunk.length
      chunk

  // The raw payload, decrypted if the document is encrypted and this stream is not exempt. A
  // stream created in this scope (negative sentinel start) yields its inline bytes directly.
  private[facsimile] def raw(body: Cos.Body)(using Tactic[Pdf.Error]): Data =
    if body.start < 0 then newStreams.at(body.start).or(Array.empty[Byte]) else
      val bytes = source.read(body.start, (payloadEnd(body) - body.start).toInt)

      if !encryptedStream(body) then bytes else
        guard.lay(bytes): guard =>
          streamOwners.at(body.start).lay(bytes): (number, generation) =>
            guard.stream(bytes, number, generation, Unset)

  // Whether a stream's raw bytes need decrypting: the document is encrypted and the stream is
  // not exempt — cross-reference streams (never encrypted), metadata under `/EncryptMetadata
  // false`, and streams marked with the `Identity` crypt filter.
  private def encryptedStream(body: Cos.Body)(using Tactic[Pdf.Error]): Boolean = guard.lay(false): guard =>
    val kind = body.entries(t"Type").let(_.name).or(t"")

    val exempt =
      kind == t"XRef"
      || (kind == t"Metadata" && !guard.encryptMetadata)
      || cryptMethod(body) == Guard.Method.Identity

    !exempt && streamOwners.contains(body.start)

  // A `/Crypt` filter in the stream's filter chain selects a crypt method by name; `Identity`
  // (the default) means the stream is stored in the clear.
  private def cryptMethod(body: Cos.Body)(using Tactic[Pdf.Error]): Optional[Guard.Method] =
    val filters = deepResolved(body.entries(t"Filter").or(Cos.Nil))

    val hasCrypt = filters match
      case Cos.Name(t"Crypt")     => true
      case Cos.Sequence(elements) => elements.exists(_.name == t"Crypt")
      case _                      => false

    if !hasCrypt then Unset else
      val parms = deepResolved(body.entries(t"DecodeParms").or(Cos.Nil))

      val name = parms match
        case Cos.Dictionary(entries) => entries(t"Name").let(_.name)
        case Cos.Sequence(elements)  =>
          elements.flatMap(_.dictionary.let(_(t"Name")).let(_.name).lay(List())(List(_))).headOption
            . getOrElse(Unset)
        case _                       => Unset

      if name == t"Identity" || name.absent then Guard.Method.Identity else Unset

  // The exclusive end of the payload: `/Length` bytes when the declared length checks out —
  // the `endstream` keyword must follow it — and otherwise, since wrong lengths abound in
  // real files, the nearest `endstream`, less the end-of-line before it.
  private def payloadEnd(body: Cos.Body)(using Tactic[Pdf.Error]): Long =
    resolved(body.entries(t"Length").or(Cos.Nil)).long.let: length =>
      val end = body.start + length
      if length >= 0 && end <= source.size && endstreamFollows(end) then end else Unset

    . or:
        val marker = t"endstream"
        val chunkSize = 65536
        var offset = body.start
        var found: Optional[Long] = Unset

        while found.absent && offset < source.size do
          val chunk = source.read(offset, chunkSize + marker.length - 1)

          chunk.survey: surveyor =>
            while found.absent && surveyor.glimpse(marker.length).present do
              if surveyor.matches(marker) { (byte, char) => (byte & 0xff) == char.toInt }
              then found = offset + surveyor.passed
              else surveyor.advance()

          offset += chunkSize

        found.let: position =>
          // The end-of-line before `endstream` belongs to the syntax, not the payload.
          val windowStart = (position - 2).max(body.start)
          val window = source.read(windowStart, (position - windowStart).toInt)
          val last = if window.length >= 1 then window.readUnchecked(window.length - 1) & 0xff else -1
          val prior = if window.length >= 2 then window.readUnchecked(window.length - 2) & 0xff else -1

          if prior == 0x0d && last == 0x0a then position - 2
          else if last == 0x0a || last == 0x0d then position - 1
          else position

        . or(abort(Pdf.Error(Pdf.Error.Reason.Truncated)))

  private def endstreamFollows(position: Long): Boolean =
    val marker = t"endstream"
    val window = source.read(position, 24)

    window.survey: surveyor =>
      surveyor.pace { byte => CosLexer.whitespace(byte & 0xff) }
      surveyor.matches(marker) { (byte, char) => (byte & 0xff) == char.toInt }

  // Resolves a value and, one level down, the elements of an array or the values of a
  // dictionary: sufficient for `/Filter` and `/DecodeParms` shapes.
  private def deepResolved(value: Cos)(using Tactic[Pdf.Error]): Cos = resolved(value) match
    case Cos.Sequence(elements)  => Cos.Sequence(elements.map(resolved(_)))
    case Cos.Dictionary(entries) => Cos.Dictionary(Map.of(entries.stdlib.view.mapValues(resolved(_)).toMap))
    case other                   => other

  private def containerStream(container: Int)(using Tactic[Pdf.Error]): ObjectStream =
    containers.at(container).or:
      val stream = apply(container) match
        case body @ Cos.Body(entries, _) =>
          val data = payload(body)

          val first = entries(t"First").let(_.long)
            . or(abort(Pdf.Error(Pdf.Error.Reason.MissingEntry(t"First")))).toInt

          val count = entries(t"N").let(_.long)
            . or(abort(Pdf.Error(Pdf.Error.Reason.MissingEntry(t"N")))).toInt

          ObjectStream(data, first, count)

        case _ =>
          abort(Pdf.Error(Pdf.Error.Reason.MissingObject(container, 0)))

      containers(container) = stream
      stream
