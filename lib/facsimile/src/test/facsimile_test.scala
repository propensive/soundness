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

import soundness.*

import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

import _root_.java.io as ji
import _root_.java.util.zip as juz

object Tests extends Suite(m"Facsimile tests"):
  def run(): Unit =
    def parse(text: Text): Cos = CosParser(CosLexer(Scan(text.in[Data]))).value()

    def textOf(cos: Cos): Text =
      cos.chars.let: bytes =>
        String(bytes.mutable(using Unsafe), "UTF-8").tt
      . or(t"")

    def bytesOf(cos: Cos): List[Int] = cos.chars.let(_.to(List).map(_.toInt & 0xff)).or(List())

    def data(values: Int*): Data = values.map(_.toByte).to(Array).immutable(using Unsafe)

    def deflate(bytes: Data): Data =
      val deflater = juz.Deflater()
      deflater.setInput(bytes.mutable(using Unsafe))
      deflater.finish()
      val buffer = new Array[Byte](1024)
      val out = ji.ByteArrayOutputStream()
      while !deflater.finished do out.write(buffer, 0, deflater.deflate(buffer))
      deflater.end()
      out.toByteArray.nn.immutable(using Unsafe)

    def pad10(value: Long): Text =
      val digits = value.toString
      ("0".repeat(10 - digits.length).nn + digits).tt

    // Assembles a complete classic-xref PDF from pre-rendered object bodies, numbering them
    // from 1 and computing correct byte offsets; object 1 is the catalog by convention.
    def document(bodies: Data*): Data = documentWith(t"", bodies*)

    def documentWith(trailerExtra: Text, bodies: Data*): Data =
      var out: Data = t"%PDF-1.7\n".in[Data]
      val offsets = List.newBuilder[Long]

      bodies.zipWithIndex.each: (body, index) =>
        offsets += out.length.toLong
        out = out ++ t"${index + 1} 0 obj\n".in[Data] ++ body ++ t"\nendobj\n".in[Data]

      val xrefOffset = out.length
      out = out ++ t"xref\n0 ${bodies.length + 1}\n0000000000 65535 f \n".in[Data]

      offsets.result().each: offset =>
        out = out ++ t"${pad10(offset)} 00000 n \n".in[Data]

      out ++ t"trailer\n<< /Size ${bodies.length + 1} /Root 1 0 R $trailerExtra >>\nstartxref\n$xrefOffset\n%%EOF".in[Data]

    val catalog: Data = t"<< /Type /Catalog >>".in[Data]

    suite(m"COS object syntax"):
      test(m"a bare integer"):
        parse(t"42")
      . assert(_ == Cos.Integral(42))

      test(m"a negative integer"):
        parse(t"-17")
      . assert(_ == Cos.Integral(-17))

      test(m"an explicitly positive integer"):
        parse(t"+9")
      . assert(_ == Cos.Integral(9))

      test(m"a real number"):
        parse(t"3.25")
      . assert(_ == Cos.Real(3.25))

      test(m"a real number with no integer part"):
        parse(t"-.5")
      . assert(_ == Cos.Real(-0.5))

      test(m"a real number with no fractional part"):
        parse(t"4.")
      . assert(_ == Cos.Real(4.0))

      test(m"the true keyword"):
        parse(t"true")
      . assert(_ == Cos.Truth(true))

      test(m"the false keyword"):
        parse(t"false")
      . assert(_ == Cos.Truth(false))

      test(m"the null keyword"):
        parse(t"null")
      . assert(_ == Cos.Nil)

      test(m"a name"):
        parse(t"/Type")
      . assert(_ == Cos.Name(t"Type"))

      test(m"a name with hexadecimal escapes"):
        parse(t"/A#42C")
      . assert(_ == Cos.Name(t"ABC"))

      test(m"the empty name"):
        parse(t"/")
      . assert(_ == Cos.Name(t""))

      test(m"a literal string"):
        textOf(parse(t"(Hello, world)"))
      . assert(_ == t"Hello, world")

      test(m"a literal string with escapes"):
        textOf(parse(t"(a\\(b\\)c\\\\d)"))
      . assert(_ == t"a(b)c\\d")

      test(m"a literal string with control escapes"):
        bytesOf(parse(t"(a\\n\\t\\r\\b\\fz)"))
      . assert(_ == List('a', 0x0a, 0x09, 0x0d, 0x08, 0x0c, 'z'))

      test(m"a literal string with octal escapes"):
        textOf(parse(t"(\\101\\102\\103)"))
      . assert(_ == t"ABC")

      test(m"a short octal escape ends at a non-octal character"):
        bytesOf(parse(t"(\\53Z)"))
      . assert(_ == List(43, 'Z'))

      test(m"nested parentheses need no escaping"):
        textOf(parse(t"((nested) text)"))
      . assert(_ == t"(nested) text")

      test(m"a line continuation produces nothing"):
        textOf(parse(t"(one\\\ntwo)"))
      . assert(_ == t"onetwo")

      test(m"an unescaped CRLF in a string is a single line feed"):
        bytesOf(parse(t"(a\r\nb)"))
      . assert(_ == List('a', 0x0a, 'b'))

      test(m"an unknown escape drops the reverse solidus"):
        textOf(parse(t"(\\q)"))
      . assert(_ == t"q")

      test(m"a hexadecimal string"):
        textOf(parse(t"<48656C6C6F>"))
      . assert(_ == t"Hello")

      test(m"whitespace is ignored inside hexadecimal strings"):
        textOf(parse(t"<48 65\n6C 6C 6F>"))
      . assert(_ == t"Hello")

      test(m"an odd hexadecimal digit implies a trailing zero"):
        bytesOf(parse(t"<484>"))
      . assert(_ == List(0x48, 0x40))

      test(m"an array of integers"):
        parse(t"[1 2 3]")
      . assert(_ == Cos.Sequence(List(Cos.Integral(1), Cos.Integral(2), Cos.Integral(3))))

      test(m"an indirect reference"):
        parse(t"[12 0 R]")
      . assert(_ == Cos.Sequence(List(Cos.Ref(12, 0))))

      test(m"two integers followed by a name are not a reference"):
        parse(t"[12 0 /R]")
      . assert(_ == Cos.Sequence(List(Cos.Integral(12), Cos.Integral(0), Cos.Name(t"R"))))

      test(m"a dictionary"):
        parse(t"<< /A 1 /B (two) >>")(t"A")
      . assert(_ == Cos.Integral(1))

      test(m"a nested dictionary"):
        parse(t"<< /A << /B /C >> >>")(t"A")
      . assert(_ == Cos.Dictionary(Map(t"B" -> Cos.Name(t"C"))))

      test(m"a null dictionary value is equivalent to absence"):
        parse(t"<< /A null >>")(t"A")
      . assert(_ == Unset)

      test(m"comments are whitespace"):
        parse(t"[1 % comment ] 2\n3]")
      . assert(_ == Cos.Sequence(List(Cos.Integral(1), Cos.Integral(3))))

      test(m"a value spanning many scan windows"):
        textOf(parse(("(" + "a".repeat(20000).nn + ")").tt)).length
      . assert(_ == 20000)

    suite(m"Stream filters"):
      test(m"FlateDecode round-trips deflated data"):
        val expected = t"The quick brown fox jumps over the lazy dog".in[Data]
        Filter.decode(deflate(expected), List((Filter.Id.Flate, Map()))).to(List)
      . assert(_ == t"The quick brown fox jumps over the lazy dog".in[Data].to(List))

      test(m"ASCIIHexDecode"):
        Filter.decode(t"48656c6C6F>".in[Data], List((Filter.Id.AsciiHex, Map()))).to(List)
      . assert(_ == t"Hello".in[Data].to(List))

      test(m"RunLengthDecode literal and repeated runs"):
        Filter.decode(data(2, 'a', 'b', 'c', 254, 'x', 128), List((Filter.Id.RunLength, Map())))
        . to(List).map(_.toInt)
      . assert(_ == List[Int]('a', 'b', 'c', 'x', 'x', 'x'))

      test(m"a PNG Up predictor"):
        Predictor(data(2, 1, 2, 3, 2, 3, 3, 3), 12, 1, 8, 3).to(List).map(_.toInt)
      . assert(_ == List(1, 2, 3, 4, 5, 6))

      test(m"a PNG Sub predictor"):
        Predictor(data(1, 5, 1, 1), 11, 1, 8, 3).to(List).map(_.toInt)
      . assert(_ == List(5, 6, 7))

      test(m"a PNG Paeth predictor"):
        Predictor(data(4, 9, 1, 1, 4, 1, 1, 1), 15, 1, 8, 3).to(List).map(_.toInt)
      . assert(_ == List(9, 10, 11, 10, 11, 12))

      test(m"a TIFF predictor"):
        Predictor(data(10, 5, 5, 3, 1, 1), 2, 1, 8, 3).to(List).map(_.toInt)
      . assert(_ == List(10, 15, 20, 3, 4, 5))

      test(m"a predictor after FlateDecode"):
        val parms = Map(t"Predictor" -> Cos.Integral(12), t"Columns" -> Cos.Integral(3))

        Filter.decode(deflate(data(2, 1, 2, 3, 2, 3, 3, 3)), List((Filter.Id.Flate, parms)))
        . to(List).map(_.toInt)
      . assert(_ == List(1, 2, 3, 4, 5, 6))

      test(m"an unknown filter name is an error"):
        capture[PdfError](Filter.decode(data(1), List((Filter.Id.Lzw, Map())))).reason
      . assert(_ == PdfError.Reason.UnknownFilter(t"LZWDecode"))

    suite(m"Whole documents"):
      test(m"the version comes from the header"):
        PdfFile(document(catalog)).open():
          pdf.version
      . assert(_ == Pdf.Version(1, 7))

      test(m"the trailer holds the catalog reference"):
        PdfFile(document(catalog)).open():
          pdf.trailer.at(t"Root")
      . assert(_ == Cos.Ref(1, 0))

      test(m"an object resolves through the cross-reference table"):
        PdfFile(document(catalog)).open():
          pdf(1, 0)(t"Type")
      . assert(_ == Cos.Name(t"Catalog"))

      test(m"resolved objects are pure values and escape the scope"):
        val name = PdfFile(document(catalog)).open():
          pdf(1, 0)(t"Type").let(_.name)

        name
      . assert(_ == t"Catalog")

      test(m"a reference to an absent object is null"):
        PdfFile(document(catalog)).open():
          pdf(99, 0)
      . assert(_ == Cos.Nil)

      test(m"a generation mismatch is null"):
        PdfFile(document(catalog)).open():
          pdf(1, 3)
      . assert(_ == Cos.Nil)

      test(m"objects reference each other"):
        val doc = document(t"<< /Type /Catalog /Next 2 0 R >>".in[Data], t"[1 0 R (x)]".in[Data])

        PdfFile(doc).open():
          pdf.resolved(pdf(1, 0)(t"Next").or(Cos.Nil)).elements.let(_.length)
      . assert(_ == 2)

      test(m"a stream payload with no filter"):
        val body = t"<< /Length 5 >>\nstream\nHello\nendstream".in[Data]

        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"Hello")

      test(m"a stream payload with an indirect length"):
        val body = t"<< /Length 3 0 R >>\nstream\nHello\nendstream".in[Data]

        PdfFile(document(catalog, body, t"5".in[Data])).open():
          pdf(2, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"Hello")

      test(m"a FlateDecode stream payload"):
        val payload = deflate(t"compressed content".in[Data])

        val body = t"<< /Length ${payload.length} /Filter /FlateDecode >>\nstream\n".in[Data]
          ++ payload ++ t"\nendstream".in[Data]

        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"compressed content")

      test(m"an incremental update supersedes an object"):
        val base = document(catalog)

        // Find the base cross-reference offset from its own startxref value, then append an
        // updated object 1 and a new section chaining back through /Prev.
        val baseText = String(base.mutable(using Unsafe), "ISO-8859-1")

        val previous: Text = baseText.substring(baseText.lastIndexOf("startxref") + 10).nn
          . trim.nn.takeWhile(_.isDigit).tt

        var out = base
        val objectOffset = out.length
        out = out ++ t"1 0 obj\n<< /Type /Catalog /Version /2.0 >>\nendobj\n".in[Data]
        val newXref = out.length

        out = out
          ++ t"xref\n1 1\n${pad10(objectOffset)} 00000 n \n".in[Data]
          ++ t"trailer\n<< /Size 2 /Root 1 0 R /Prev $previous >>\nstartxref\n$newXref\n%%EOF"
             . in[Data]

        PdfFile(out).open():
          pdf(1, 0)(t"Version")
      . assert(_ == Cos.Name(t"2.0"))

      test(m"garbage input is not a PDF"):
        capture[PdfError](PdfFile(t"not a pdf at all".in[Data]).open()(pdf.version)).reason
      . assert(_ == PdfError.Reason.NotPdf)

      test(m"a missing startxref is an error"):
        capture[PdfError](PdfFile(t"%PDF-1.7\nnothing else".in[Data]).open()(pdf.version)).reason
      . assert(_ == PdfError.Reason.MissingStartxref)

      test(m"an encrypted document is refused for now"):
        val doc = documentWith(t"/Encrypt << /V 4 >>", catalog)
        capture[PdfError](PdfFile(doc).open()(pdf.version)).reason
      . assert(_ == PdfError.Reason.UnsupportedEncryption(4))

    suite(m"Cross-reference streams and object streams"):
      def xrefStreamDocument(): Data =
        var out: Data = t"%PDF-1.5\n".in[Data]

        val offset1 = out.length
        out = out ++ t"1 0 obj\n<< /Type /Catalog /Answer 4 0 R /Greeting 5 0 R >>\nendobj\n"
          . in[Data]

        // An object stream holding objects 4 and 5: the pair table is 8 bytes, so /First 8.
        val offset2 = out.length
        out = out ++ t"2 0 obj\n<< /Type /ObjStm /N 2 /First 8 /Length 15 >>\nstream\n".in[Data]
          ++ t"4 0 5 3\n42 (hi)".in[Data] ++ t"\nendstream\nendobj\n".in[Data]

        // The cross-reference stream itself, object 3: six unfiltered rows of /W [1 2 1].
        val offset3 = out.length

        val rows = data
          ( 0, 0, 0, 0,
            1, (offset1 >> 8) & 0xff, offset1 & 0xff, 0,
            1, (offset2 >> 8) & 0xff, offset2 & 0xff, 0,
            1, (offset3 >> 8) & 0xff, offset3 & 0xff, 0,
            2, 0, 2, 0,
            2, 0, 2, 1 )

        out = out
          ++ t"3 0 obj\n<< /Type /XRef /Size 6 /W [1 2 1] /Root 1 0 R /Length 24 >>\nstream\n"
             . in[Data]
          ++ rows ++ t"\nendstream\nendobj\n".in[Data]

        out ++ t"startxref\n$offset3\n%%EOF".in[Data]

      test(m"the trailer is the cross-reference stream dictionary"):
        PdfFile(xrefStreamDocument()).open():
          pdf.trailer.at(t"Type")
      . assert(_ == Cos.Name(t"XRef"))

      test(m"an object loads from a compressed object stream"):
        PdfFile(xrefStreamDocument()).open():
          pdf.resolved(pdf(1, 0)(t"Answer").or(Cos.Nil))
      . assert(_ == Cos.Integral(42))

      test(m"a string loads from a compressed object stream"):
        PdfFile(xrefStreamDocument()).open():
          textOf(pdf.resolved(pdf(1, 0)(t"Greeting").or(Cos.Nil)))
      . assert(_ == t"hi")

    // A two-page document: object 1 catalog, 2 page-tree root (A4 media box, inherited),
    // 3 a plain page, 4 a page with its own crop box and rotation.
    def paged(extraCatalog: Text = t"", page3: Text = t"", page4: Text = t""): Data =
      document
        ( t"<< /Type /Catalog /Pages 2 0 R $extraCatalog >>".in[Data],
          t"<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 /MediaBox [0 0 595 842] >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R $page3 >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R /CropBox [10 10 300 400] /Rotate 90 $page4 >>"
          . in[Data] )

    suite(m"Pages"):
      test(m"the page tree flattens in order"):
        PdfFile(paged()).open():
          pdf.pages.length
      . assert(_ == 2)

      test(m"the media box is inherited from the page-tree root"):
        PdfFile(paged()).open():
          pdf.pages(0).mediaBox.width
      . assert(_ == Quantity[Points[1]](595.0))

      test(m"the crop box defaults to the media box"):
        PdfFile(paged()).open():
          pdf.pages(0).cropBox.height
      . assert(_ == Quantity[Points[1]](842.0))

      test(m"a page's own crop box wins"):
        PdfFile(paged()).open():
          pdf.pages(1).cropBox.width
      . assert(_ == Quantity[Points[1]](290.0))

      test(m"the trim box defaults to the crop box"):
        PdfFile(paged()).open():
          pdf.pages(1).trimBox.height
      . assert(_ == Quantity[Points[1]](390.0))

      test(m"rotation is read from the page"):
        PdfFile(paged()).open():
          pdf.pages(1).rotation
      . assert(_ == Page.Rotation.Quarter)

      test(m"a quarter-turned page exchanges width and height"):
        PdfFile(paged()).open():
          pdf.pages(1).width
      . assert(_ == Quantity[Points[1]](390.0))

      test(m"an unrotated page keeps its axes"):
        PdfFile(paged()).open():
          pdf.pages(0).rotation
      . assert(_ == Page.Rotation.None)

      test(m"a UserUnit scales the boxes"):
        PdfFile(paged(page3 = t"/UserUnit 2")).open():
          pdf.pages(0).mediaBox.width
      . assert(_ == Quantity[Points[1]](1190.0))

      test(m"a cyclic page tree is an error"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [2 0 R] /Count 1 >>".in[Data] )

        capture[PdfError](PdfFile(doc).open()(pdf.pages.length)).reason
      . assert(_ == PdfError.Reason.CircularPageTree)

    suite(m"Document information"):
      def informed(info: Text): Data =
        val body = t"<< /Type /Catalog /Pages 3 0 R >>".in[Data]
        val pages = t"<< /Type /Pages /Kids [] /Count 0 >>".in[Data]
        documentWith(t"/Info 2 0 R", body, t"<< $info >>".in[Data], pages)

      test(m"the title is read"):
        PdfFile(informed(t"/Title (A Document)")).open():
          pdf.info.title
      . assert(_ == t"A Document")

      test(m"a UTF-16BE string decodes by its byte-order mark"):
        PdfFile(informed(t"/Author <FEFF00480069>")).open():
          pdf.info.author
      . assert(_ == t"Hi")

      test(m"PDFDocEncoding maps its differences from Latin-1"):
        PdfFile(informed(t"/Subject (caf\\351 \\200)")).open():
          pdf.info.subject
      . assert(_ == t"café •")

      test(m"a creation date parses with its offset"):
        PdfFile(informed(t"/CreationDate (D:20240102030405+01'30')")).open():
          pdf.info.created.let(_.offset)
      . assert(_ == Quantity[Seconds[1]](5400.0))

      test(m"a date with no offset has an unknown zone"):
        PdfFile(informed(t"/CreationDate (D:20240102030405)")).open():
          pdf.info.created.let(_.offset)
      . assert(_ == Unset)

      test(m"a malformed date is unset, not an error"):
        PdfFile(informed(t"/ModDate (yesterday)")).open():
          pdf.info.modified
      . assert(_ == Unset)

      test(m"a truncated date defaults its later components"):
        PdfFile(informed(t"/CreationDate (D:2024)")).open():
          pdf.info.created.let(_.timestamp)
      . assert: timestamp =>
          import calendars.gregorianCalendar
          timestamp == Timestamp(Date(Year(2024), Month(1), Day(1)),
              Clockface(Base24(0), Base60(0), Base60(0)))

      test(m"document information escapes the scope as a pure value"):
        val info = PdfFile(informed(t"/Title (Kept)")).open()(pdf.info)
        info.title
      . assert(_ == t"Kept")

    suite(m"Navigation"):
      def navigable(catalogExtra: Text, objects: Data*): Data =
        val standard = List
          ( t"<< /Type /Catalog /Pages 2 0 R $catalogExtra >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

        document((standard ++ objects)*)

      test(m"a named destination resolves through the name tree"):
        val doc = navigable
          ( t"/Names << /Dests << /Names [(intro) [3 0 R /XYZ 10 20 null]] >> >>" )

        PdfFile(doc).open():
          pdf.destinations.at(t"intro")
      . assert(_ == Destination.Xyz(Prim, 10.0, 20.0, Unset))

      test(m"an old-style /Dests dictionary also resolves"):
        val doc = navigable(t"/Dests << /intro [3 0 R /FitH 30] >>")

        PdfFile(doc).open():
          pdf.destinations.at(t"intro")
      . assert(_ == Destination.FitWidth(Prim, 30.0))

      test(m"bookmarks form a tree with destinations"):
        val doc = navigable
          ( t"/Outlines 4 0 R",
            t"<< /Type /Outlines /First 5 0 R >>".in[Data],
            t"<< /Title (One) /Parent 4 0 R /Next 6 0 R /Dest [3 0 R /Fit] >>".in[Data],
            t"<< /Title (Two) /Parent 4 0 R /First 7 0 R >>".in[Data],
            t"<< /Title (Child) /Parent 6 0 R >>".in[Data] )

        PdfFile(doc).open():
          pdf.bookmarks.map(bookmark => (bookmark.title, bookmark.children.length))
      . assert(_ == List((t"One", 0), (t"Two", 1)))

      test(m"a bookmark destination lands on its page"):
        val doc = navigable
          ( t"/Outlines 4 0 R",
            t"<< /Type /Outlines /First 5 0 R >>".in[Data],
            t"<< /Title (One) /Parent 4 0 R /Dest [3 0 R /Fit] >>".in[Data] )

        PdfFile(doc).open():
          pdf.bookmarks.head.destination
      . assert(_ == Destination.Fit(Prim))

      test(m"a cyclic outline terminates"):
        val doc = navigable
          ( t"/Outlines 4 0 R",
            t"<< /Type /Outlines /First 5 0 R >>".in[Data],
            t"<< /Title (Loop) /Parent 4 0 R /Next 5 0 R >>".in[Data] )

        PdfFile(doc).open():
          pdf.bookmarks.length
      . assert(_ == 1)

    suite(m"Annotations, attachments and labels"):
      test(m"a URI link annotation"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Annots [4 0 R] >>".in[Data],
            t"<< /Subtype /Link /Rect [0 0 10 20] /A << /S /URI /URI (https://x.com) >> >>"
            . in[Data] )

        PdfFile(doc).open():
          pdf.pages(0).annotations.head match
            case Annotation.Link(rect, _, uri, _) => (rect.height, uri)
            case _                                => (Quantity[Points[1]](0.0), Unset)
      . assert(_ == (Quantity[Points[1]](20.0), t"https://x.com"))

      test(m"a note annotation carries its contents"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Annots [4 0 R] >>".in[Data],
            t"<< /Subtype /Text /Rect [0 0 5 5] /Contents (Remember) /Open true >>".in[Data] )

        PdfFile(doc).open():
          pdf.pages(0).annotations.head match
            case Annotation.Note(_, contents, open, _) => (contents, open)
            case _                                     => (Unset, false)
      . assert(_ == (t"Remember", true))

      test(m"an attachment surfaces its metadata and content"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R /Names << /EmbeddedFiles << /Names [(notes.txt) 4 0 R] >> >> >>"
            . in[Data],
            t"<< /Type /Pages /Kids [] /Count 0 >>".in[Data],
            t"<< /Type /Page >>".in[Data],
            t"<< /Type /Filespec /F (notes.txt) /EF << /F 5 0 R >> >>".in[Data],
            t"<< /Type /EmbeddedFile /Subtype /text#2Fplain /Length 5 >>\nstream\nhello\nendstream"
            . in[Data] )

        PdfFile(doc).open():
          val attachment = pdf.attachments.head

          ( attachment.name,
            attachment.filename,
            attachment.mediaType,
            String(attachment.data.mutable(using Unsafe), "UTF-8").tt )
      . assert(_ == (t"notes.txt", t"notes.txt", t"text/plain", t"hello"))

      test(m"page labels follow the number-tree ranges"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R /PageLabels << /Nums [0 << /S /r >> 2 << /S /D /St 5 /P (A-) >>] >> >>"
            . in[Data],
            t"<< /Type /Pages /Kids [3 0 R 4 0 R 5 0 R] /Count 3 /MediaBox [0 0 9 9] >>"
            . in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

        PdfFile(doc).open():
          List(pdf.pageLabel(0.z), pdf.pageLabel(1.z), pdf.pageLabel(2.z))
      . assert(_ == List(t"i", t"ii", t"A-5"))

      test(m"a document without page labels numbers plainly"):
        PdfFile(paged()).open():
          pdf.pageLabel(1.z)
      . assert(_ == t"2")

      test(m"XMP metadata surfaces as raw bytes"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R /Metadata 3 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [] /Count 0 >>".in[Data],
            t"<< /Type /Metadata /Subtype /XML /Length 5 >>\nstream\n<xmp/\nendstream".in[Data] )

        PdfFile(doc).open():
          pdf.xmp.let(bytes => String(bytes.mutable(using Unsafe), "UTF-8").tt)
      . assert(_ == t"<xmp/")
