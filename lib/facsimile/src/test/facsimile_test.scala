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
import _root_.java.nio.file as jnf
import _root_.java.security as js
import _root_.java.util.zip as juz
import _root_.javax.crypto as jc
import _root_.javax.crypto.spec as jcs

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

    // A filename Text is its own abstract path, for opening temp files on disk.
    given (Text is Abstractable across Paths to Text) = identity(_)

    // Write `bytes` to a fresh temp file and return its path as Text; and read a file back.
    def tempPdf(bytes: Data): Text =
      val path = jnf.Files.createTempFile("facsimile", ".pdf").nn
      jnf.Files.write(path, bytes.mutable(using Unsafe))
      path.toString.tt

    def fileBytes(path: Text): Data =
      jnf.Files.readAllBytes(jnf.Paths.get(path.s)).nn.immutable(using Unsafe)

    // Big-endian byte assembly for the font fixture below.
    def big16(values: Int*): Data =
      IArray.from:
        values.flatMap: value =>
          Seq((value >> 8).toByte, value.toByte)

    def big32(values: Long*): Data =
      IArray.from:
        values.flatMap: value =>
          Seq((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

    def utf16(text: Text): Data = IArray.from(text.s.getBytes("UTF-16BE").nn)

    // A minimal but real TrueType font: glyphs A and B with simple outlines and C a composite
    // of both, at 1000 units per em, carrying the bounding box (-50, -200, 1000, 800), an
    // italic angle of -11.5, fixed pitch, cap height 730 and the PostScript name TestSans —
    // everything the embedder reads.
    val miniFont: Data =
      val head =
        big16(1, 0, 1, 0) ++ big32(0L, 0x5f0f3cf5L) ++ big16(0, 1000) ++ big32(0L, 0L, 0L, 0L) ++
          big16(-50, -200, 1000, 800) ++ big16(0, 8) ++ big16(2, 0, 0)

      val hhea = big16(1, 0) ++ big16(800, -200, 90, 600, 10, 10, 1000, 1, 0, 0, 0, 0, 0, 0, 0, 3)
      val hmtx = big16(600, 10, 500, 20, 400, 30) ++ big16(25)

      val cmap =
        big16(0, 1) ++ big16(3, 1) ++ big32(12L) ++ big16(4, 46, 0) ++ big16(6, 4, 1, 2) ++
          big16(0x43, 0x63, 0xffff) ++ big16(0) ++ big16(0x41, 0x61, 0xffff) ++
          big16(-64, 0, 1) ++ big16(0, 4, 0) ++ big16(2, 3, 1)

      val maxp = big32(0x00010000L) ++ big16(4)
      val post = big32(0x00030000L, 0xfff48000L) ++ big16(-100, 50) ++ big32(1L)

      val os2 =
        big16(2, 500, 700, 5, 8) ++ big16(0, 0, 0, 0, 0, 0, 0, 0, 0, 0) ++ big16(0) ++
          big16(0, 0, 0, 0, 0) ++ big32(0L, 0L, 0L, 0L) ++ t"TEST".in[Data] ++
          big16(0x40, 0x41, 0x7a) ++ big16(750, -250, 100) ++ big16(820, 220) ++
          big32(0L, 0L) ++ big16(530, 730, 0, 32, 0)

      val name =
        big16(0, 2, 30) ++ big16(3, 1, 0x409, 1, 18, 0) ++ big16(3, 1, 0x409, 6, 16, 18) ++
          utf16(t"Test Sans") ++ utf16(t"TestSans")

      val glyph3 =
        big16(-1, 0, 0, 500, 700) ++ big16(0x0021, 1, 0, 0) ++ big16(0x0008, 2, 0, 0x4000)

      val glyf = big16(1, 0, 0, 500, 700, 1, 0) ++ big16(1, 10, 20, 400, 600, 1, 0) ++ glyph3
      val loca = big16(0, 0, 7, 14, 27)

      val tables = List(t"cmap" -> cmap, t"glyf" -> glyf, t"head" -> head, t"hhea" -> hhea,
          t"hmtx" -> hmtx, t"loca" -> loca, t"maxp" -> maxp, t"name" -> name, t"OS/2" -> os2,
          t"post" -> post)

      var offset = 12 + tables.length*16
      val directory = List.newBuilder[Data]
      val parts = List.newBuilder[Data]

      tables.each: (tag, table) =>
        val padding = (4 - table.length%4)%4
        val entry = IArray.from(tag.s.getBytes("US-ASCII").nn)
        directory += entry ++ big32(0L, offset.toLong, table.length.toLong)
        parts += table ++ IArray.fill[Byte](padding)(0)
        offset += table.length + padding

      big32(0x00010000L) ++ big16(10, 128, 3, 32) ++ directory.result().reduce(_ ++ _) ++
        parts.result().reduce(_ ++ _)

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
        capture[PdfError](Filter.chain(Cos.Name(t"BogusDecode"), Unset)).reason
      . assert(_ == PdfError.Reason.UnknownFilter(t"BogusDecode"))

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

      test(m"opening a PDF for writing is refused"):
        capture[PdfError](PdfFile(document(catalog)).open(Write) { () }).reason
      . assert(_ == PdfError.Reason.WriteUnsupported)

      test(m"in-memory data opens directly in Pdf form"):
        document(catalog).open[Pdf]():
          pdf.version.major
      . assert(_ == 1)

      test(m"a public-key security handler is unsupported"):
        val doc = documentWith(t"/Encrypt << /Filter /Adobe.PubSec /V 4 >>", catalog)
        capture[PdfError](PdfFile(doc).open()(pdf.version)).reason
      . assert(_ == PdfError.Reason.UnsupportedEncryption(0))

    suite(m"Writing"):
      // A document with a catalog (obj 1), a note object (obj 2), and a page tree, for editing.
      def editable: Data = document
        ( t"<< /Type /Catalog /Pages 3 0 R /Note 2 0 R >>".in[Data],
          t"<< /Value (original) >>".in[Data],
          t"<< /Type /Pages /Kids [4 0 R] /Count 1 /MediaBox [0 0 200 300] >>".in[Data],
          t"<< /Type /Page /Parent 3 0 R >>".in[Data] )

      test(m"a replaced object is visible after the write, via an incremental update"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.set(Cos.Ref(2, 0), Cos.Dictionary(Map(t"Value" -> Cos.Chars(t"edited".in[Data]))))

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.resolved(pdf(2, 0))(t"Value").let(_.text)
      . assert(_ == t"edited")

      test(m"the original bytes are preserved (incremental append, not rewrite)"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.set(Cos.Ref(2, 0), Cos.Dictionary(Map(t"Value" -> Cos.Chars(t"edited".in[Data]))))

        fileBytes(path).slice(0, editable.length).to(List)
      . assert(_ == editable.to(List))

      test(m"the incremental update chains /Prev to the original cross-reference"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.set(Cos.Ref(2, 0), Cos.Integral(7))

        String(fileBytes(path).mutable(using Unsafe), "ISO-8859-1").nn.contains("/Prev")
      . assert(_ == true)

      test(m"an allocated object gets a fresh number and is readable after writing"):
        val path = tempPdf(editable)
        var allocated = 0

        PdfFile(path).open(Read & Write): doc ?=>
          val ref = doc.allocate(Cos.Dictionary(Map(t"Kind" -> Cos.Name(t"New"))))
          allocated = ref.number

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf(allocated)(t"Kind")
      . assert(_ == Cos.Name(t"New"))

      test(m"a fresh object number is one past the original size"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.allocate(Cos.Nil).number
      . assert(_ == 5)

      test(m"a deleted object reads back as null"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.free(Cos.Ref(2, 0))

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf(2, 0)
      . assert(_ == Cos.Nil)

      test(m"edits are visible to read views within the same write scope"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.set(Cos.Ref(2, 0), Cos.Dictionary(Map(t"Value" -> Cos.Chars(t"live".in[Data]))))
          doc.resolved(doc(2, 0))(t"Value").let(_.text)
      . assert(_ == t"live")

      test(m"a scope that changes nothing appends nothing"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.version

        fileBytes(path).length
      . assert(_ == editable.length)

      test(m"opening in-memory data for writing is refused"):
        capture[PdfError](PdfFile(editable).open(Read & Write) { doc ?=> () }).reason
      . assert(_ == PdfError.Reason.WriteUnsupported)

      // A one-page A4 document with Helvetica, for page and content edits.
      def onePage: Data = document
        ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
          t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 595 842] >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R /Resources << /Font << /F1 4 0 R >> >> >>".in[Data],
          t"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>".in[Data] )

      test(m"replacing a page's content is visible as extracted text"):
        val path = tempPdf(onePage)

        PdfFile(path).open(Read & Write): doc ?=>
          val operators = List
            ( PdfOperator.BeginText, PdfOperator.SetFont(t"F1", 12),
              PdfOperator.Offset(72, 720), PdfOperator.ShowText(t"Written".in[Data]),
              PdfOperator.EndText )

          doc.setContents(doc.pages(0), operators)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).text
      . assert(_ == t"Written")

      test(m"setting a page's rotation round-trips"):
        val path = tempPdf(onePage)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.setRotation(doc.pages(0), Page.Rotation.Quarter)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).rotation
      . assert(_ == Page.Rotation.Quarter)

      test(m"setting a page box round-trips"):
        val path = tempPdf(onePage)

        val target = PdfRect(Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
            Quantity[Points[1]](200.0), Quantity[Points[1]](400.0))

        PdfFile(path).open(Read & Write): doc ?=>
          doc.setBox(doc.pages(0), t"CropBox", target)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).cropBox.width
      . assert(_ == Quantity[Points[1]](200.0))

      test(m"an appended page increases the page count"):
        val path = tempPdf(onePage)

        val a4 = PdfRect(Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
            Quantity[Points[1]](595.0), Quantity[Points[1]](842.0))

        PdfFile(path).open(Read & Write): doc ?=>
          doc.appendPage(a4)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages.length
      . assert(_ == 2)

      test(m"an appended page's content extracts as text"):
        val path = tempPdf(onePage)

        val a4 = PdfRect(Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
            Quantity[Points[1]](595.0), Quantity[Points[1]](842.0))

        val helvetica = Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Font"),
            t"Subtype" -> Cos.Name(t"Type1"), t"BaseFont" -> Cos.Name(t"Helvetica")))

        val resources = Cos.Dictionary(Map(t"Font" -> Cos.Dictionary(Map(t"F1" -> helvetica))))

        PdfFile(path).open(Read & Write): doc ?=>
          val operators = List
            ( PdfOperator.BeginText, PdfOperator.SetFont(t"F1", 12),
              PdfOperator.Offset(72, 720), PdfOperator.ShowText(t"Second".in[Data]),
              PdfOperator.EndText )

          doc.appendPage(a4, operators, resources)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(1).text
      . assert(_ == t"Second")

      test(m"a removed page decreases the page count"):
        val twoPages = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

        val path = tempPdf(twoPages)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.removePage(doc.pages(0))

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages.length
      . assert(_ == 1)

      test(m"setting document information round-trips text fields"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.setInfo(PdfInfo(t"A Title", t"An Author", Unset, Unset, Unset, Unset, Unset, Unset))

        PdfFile(fileBytes(path)).open[Pdf]():
          (pdf.info.title, pdf.info.author)
      . assert(_ == (t"A Title", t"An Author"))

      test(m"a set creation date round-trips through the D: format"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          import calendars.gregorianCalendar
          val moment = Timestamp(Date(Year(2026), Month(7), Day(17)),
              Clockface(Base24(9), Base60(30), Base60(0)))

          val info = PdfInfo(Unset, Unset, Unset, Unset, Unset, Unset,
              PdfInfo.Timing(moment, Unset), Unset)

          doc.setInfo(info)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.info.created.let(_.timestamp)
      . assert: created =>
          import calendars.gregorianCalendar
          created == Timestamp(Date(Year(2026), Month(7), Day(17)),
              Clockface(Base24(9), Base60(30), Base60(0)))

      test(m"setting bookmarks builds a navigable outline"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          val marks = List
            ( Bookmark(t"Chapter 1", Unset, List(Bookmark(t"Section 1.1", Unset, Nil))),
              Bookmark(t"Chapter 2", Unset, Nil) )

          doc.setBookmarks(marks)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.bookmarks.map(bookmark => (bookmark.title, bookmark.children.length))
      . assert(_ == List((t"Chapter 1", 1), (t"Chapter 2", 0)))

      test(m"a bookmark destination targets a page"):
        val path = tempPdf(editable)

        PdfFile(path).open(Read & Write): doc ?=>
          doc.setBookmarks(List(Bookmark(t"Start", Destination.Fit(Prim), Nil)))

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.bookmarks.head.destination
      . assert(_ == Destination.Fit(Prim))

      test(m"adding a link annotation round-trips"):
        val onePage = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

        val path = tempPdf(onePage)

        val rect = PdfRect(Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
            Quantity[Points[1]](50.0), Quantity[Points[1]](20.0))

        PdfFile(path).open(Read & Write): doc ?=>
          doc.addLink(doc.pages(0), rect, uri = t"https://soundness.dev/")

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).annotations.head match
            case Annotation.Link(_, _, uri, _) => uri
            case _                             => Unset
      . assert(_ == t"https://soundness.dev/")

      // A one-page document with no font, for embedding a font into.
      def blankPage: Data = document
        ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
          t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 595 842] >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

      // Stand-in font-program bytes: the reader wraps FontFile2 bytes in `Ttf` without
      // validating, and the embedder degrades gracefully on an unparseable program, so this
      // exercises the embedding plumbing without a real font file in the repo.
      val fontProgram: Data = t"pretend-truetype-font-program".in[Data]

      test(m"an embedded font's program round-trips as FontFile2"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(fontProgram), t"MyFont")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1").embedded.let(_.data.to(List))
      . assert(_ == fontProgram.to(List))

      test(m"content using an embedded font extracts as text"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(fontProgram), t"MyFont")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

          val operators = List
            ( PdfOperator.BeginText, PdfOperator.SetFont(t"F1", 12),
              PdfOperator.Offset(72, 720), PdfOperator.ShowText(winAnsi(t"Embedded text")),
              PdfOperator.EndText )

          doc.setContents(doc.pages(0), operators)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).text
      . assert(_ == t"Embedded text")

      test(m"the embedded font is a simple WinAnsi TrueType font"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(fontProgram), t"MyFont")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1") match
            case _: PdfFont.TrueType => true
            case _                   => false
      . assert(_ == true)

      test(m"an unnamed embedded font takes its PostScript name"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont))
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1").baseFont
      . assert(_ == t"TestSans")

      test(m"the font descriptor carries the font's real metrics"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont))
          val dict = doc.resolved(font).dictionary.or(Map[Text, Cos]())

          val descriptor =
            doc.resolved(dict.at(t"FontDescriptor").or(Cos.Nil)).dictionary.or(Map[Text, Cos]())

          ( descriptor.at(t"FontBBox"),
            descriptor.at(t"ItalicAngle"),
            descriptor.at(t"CapHeight"),
            descriptor.at(t"Flags") )
      . assert(_ == (Cos.Sequence(List(Cos.Integral(-50), Cos.Integral(-200), Cos.Integral(1000),
            Cos.Integral(800))), Cos.Real(-11.5), Cos.Integral(730), Cos.Integral(97)))

      test(m"widths come from the font's own character mapping"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont))
          val dict = doc.resolved(font).dictionary.or(Map[Text, Cos]())

          dict.at(t"Widths") match
            case Cos.Sequence(widths) => (widths('A' - 32), widths('z' - 32))
            case _                    => Unset
      . assert(_ == (Cos.Integral(500), Cos.Integral(0)))

      test(m"a subset embed carries a six-letter tag prefix"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont), subset = t"AB")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1").baseFont
      . assert(_.s.matches("[A-Z]{6}\\+TestSans"))

      test(m"a subset program keeps used outlines and drops the rest"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont), subset = t"A")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1").embedded.let: ttf =>
            (ttf.glyf(1).empty, ttf.glyf(2).empty)
      . assert(_ == (false, true))

      test(m"a subset retains the components of composite glyphs"):
        val path = tempPdf(blankPage)

        PdfFile(path).open(Read & Write): doc ?=>
          val font = doc.embedFont(Ttf(miniFont), subset = t"C")
          doc.addResource(doc.pages(0), t"Font", t"F1", font)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).fonts(t"F1").embedded.let: ttf =>
            (ttf.glyf(1).empty, ttf.glyf(2).empty, ttf.glyf(3).composite)
      . assert(_ == (false, false, true))

      test(m"winAnsi encodes accented characters to their code page byte"):
        winAnsi(t"café").to(List).map(_.toInt & 0xff)
      . assert(_ == List('c', 'a', 'f', 0xe9))

    suite(m"Creation from scratch"):
      def freshPath: Text =
        val path = jnf.Files.createTempFile("facsimile-new", ".pdf").nn
        jnf.Files.delete(path)
        path.toString.tt

      val a4 = PdfRect(Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
          Quantity[Points[1]](595.0), Quantity[Points[1]](842.0))

      test(m"a created document is a valid, openable PDF"):
        val path = freshPath
        path.create[Pdf](): doc ?=>
          doc.appendPage(a4)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages.length
      . assert(_ == 1)

      test(m"a created page carries its media box"):
        val path = freshPath
        path.create[Pdf](): doc ?=>
          doc.appendPage(a4)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).mediaBox.width
      . assert(_ == Quantity[Points[1]](595.0))

      test(m"a created page's content extracts as text"):
        val path = freshPath

        val helvetica = Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Font"),
            t"Subtype" -> Cos.Name(t"Type1"), t"BaseFont" -> Cos.Name(t"Helvetica")))

        val resources = Cos.Dictionary(Map(t"Font" -> Cos.Dictionary(Map(t"F1" -> helvetica))))

        path.create[Pdf](): doc ?=>
          val operators = List
            ( PdfOperator.BeginText, PdfOperator.SetFont(t"F1", 12),
              PdfOperator.Offset(72, 720), PdfOperator.ShowText(t"From scratch".in[Data]),
              PdfOperator.EndText )

          doc.appendPage(a4, operators, resources)

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.pages(0).text
      . assert(_ == t"From scratch")

      test(m"document information set at creation round-trips"):
        val path = freshPath
        path.create[Pdf](): doc ?=>
          doc.appendPage(a4)
          doc.setInfo(PdfInfo(t"Made", Unset, Unset, Unset, Unset, Unset, Unset, Unset))

        PdfFile(fileBytes(path)).open[Pdf]():
          pdf.info.title
      . assert(_ == t"Made")

      test(m"creating over an existing file needs the Replace flag"):
        val path = freshPath
        path.create[Pdf](): doc ?=>
          doc.appendPage(a4)

        capture[PdfError](path.create[Pdf]() { doc ?=> doc.appendPage(a4) }).reason
      . assert(_ == PdfError.Reason.Io(t"the file already exists"))

    suite(m"Damaged-file recovery"):
      // A minimal well-formed document whose objects can be found by scanning, used as the
      // basis for various corruptions.
      def recoverable: Data = document
        ( t"<< /Type /Catalog /Pages 2 0 R /Note (kept) >>".in[Data],
          t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 300] >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R >>".in[Data] )

      test(m"a document with no startxref is recovered by scanning"):
        // Truncate the cross-reference section and trailer entirely.
        val text = String(recoverable.mutable(using Unsafe), "ISO-8859-1")
        val truncated = text.substring(0, text.indexOf("xref")).nn.tt.in[Data]

        PdfFile(truncated).open():
          (pdf.pages.length, pdf.pages(0).mediaBox.width)
      . assert(_ == (1, Quantity[Points[1]](200.0)))

      test(m"the catalog is found when the trailer is gone"):
        val text = String(recoverable.mutable(using Unsafe), "ISO-8859-1")
        val truncated = text.substring(0, text.indexOf("xref")).nn.tt.in[Data]

        PdfFile(truncated).open():
          pdf.resolved(pdf.trailer.at(t"Root").or(Cos.Nil))(t"Note").let(_.text)
      . assert(_ == t"kept")

      test(m"a corrupt startxref offset falls back to scanning"):
        val text = String(recoverable.mutable(using Unsafe), "ISO-8859-1")
        val corrupt = text.replaceAll("startxref\\n\\d+", "startxref\n999999").nn.tt.in[Data]

        PdfFile(corrupt).open():
          pdf.pages.length
      . assert(_ == 1)

      test(m"shifted cross-reference offsets are recovered per object"):
        // Prepend junk so every recorded offset is wrong by a fixed amount, but leave the
        // xref table itself syntactically valid.
        val shifted = t"% a comment prepended by some tool\n".in[Data] ++ recoverable

        PdfFile(shifted).open():
          pdf.pages(0).mediaBox.height
      . assert(_ == Quantity[Points[1]](300.0))

      test(m"garbage between objects does not prevent recovery"):
        val text = String(recoverable.mutable(using Unsafe), "ISO-8859-1")
        val messy = text.replace("endobj\n2 0 obj", "endobj\n%% junk %%\n2 0 obj").nn.tt.in[Data]
        val truncated =
          String(messy.mutable(using Unsafe), "ISO-8859-1").nn
          . pipe { s => s.substring(0, s.indexOf("xref")).nn.tt.in[Data] }

        PdfFile(truncated).open():
          pdf.pages.length
      . assert(_ == 1)

      test(m"an incremental update's newer object wins during recovery"):
        val full = String(recoverable.mutable(using Unsafe), "ISO-8859-1")
        val base = full.substring(0, full.indexOf("xref")).nn

        // Append a newer copy of object 1 with a changed note, then no valid xref.
        val updated =
          (base + "1 0 obj\n<< /Type /Catalog /Pages 2 0 R /Note (updated) >>\nendobj\n").tt

        PdfFile(updated.in[Data]).open():
          pdf.resolved(pdf.trailer.at(t"Root").or(Cos.Nil))(t"Note").let(_.text)
      . assert(_ == t"updated")

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

    // A one-page document with Helvetica as `/F1` and the given content stream.
    def contentPage(content: Text): Data =
      document
        ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
          t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 595 842] >>".in[Data],
          t"<< /Type /Page /Parent 2 0 R /Contents 4 0 R /Resources << /Font << /F1 5 0 R >> >> >>"
          . in[Data],
          t"<< /Length ${content.length} >>\nstream\n$content\nendstream".in[Data],
          t"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>".in[Data] )

    suite(m"Content operators"):
      def operators(content: Text): List[PdfOperator] =
        PdfFile(contentPage(content)).open():
          pdf.pages(0).operators

      test(m"a graphics-state and text block parses to typed operators"):
        operators(t"q 1 0 0 1 50 60 cm BT /F1 12 Tf 72 720 Td (Hi) Tj ET Q").map(_.ordinal)
      . assert: ordinals =>
          ordinals == List(PdfOperator.Save, PdfOperator.Concat(PdfMatrix.Identity),
              PdfOperator.BeginText, PdfOperator.SetFont(t"F1", 12), PdfOperator.Offset(72, 720),
              PdfOperator.ShowText(IArray[Byte]()), PdfOperator.EndText, PdfOperator.Restore)
            . map(_.ordinal)

      test(m"cm carries its matrix"):
        operators(t"2 0 0 2 10 20 cm")
      . assert(_ == List(PdfOperator.Concat(PdfMatrix(2, 0, 0, 2, 10, 20))))

      test(m"re carries its rectangle"):
        operators(t"1 2 30 40 re f")
      . assert(_ == List(PdfOperator.Rectangle(1, 2, 30, 40),
          PdfOperator.Fill(PdfOperator.FillRule.NonZero)))

      test(m"rg becomes an Srgb colour"):
        operators(t"1 0 0.5 rg")
      . assert(_ == List(PdfOperator.FillRgb(Srgb(1.0, 0.0, 0.5))))

      test(m"a dash pattern parses"):
        operators(t"[2 1] 0 d")
      . assert(_ == List(PdfOperator.SetDashPattern(List(2.0, 1.0), 0.0)))

      test(m"TJ mixes strings and kerning adjustments"):
        operators(t"BT [(A) -500 (B)] TJ ET") match
          case List(_, PdfOperator.ShowTexts(elements), _) =>
            elements.map:
              case value: Double => value

              case data: (Data @unchecked) =>
                String(data.mutable(using Unsafe), "UTF-8").tt

          case _ =>
            List()
      . assert(_ == List(t"A", -500.0, t"B"))

      test(m"unknown operators survive as Unrecognized"):
        operators(t"BX /x 7 fancyNewOp EX")
      . assert(_ == List(PdfOperator.BeginCompatibility,
          PdfOperator.Unrecognized(t"fancyNewOp", List(Cos.Name(t"x"), Cos.Integral(7))),
          PdfOperator.EndCompatibility))

      test(m"a known operator with missing operands is an error"):
        capture[PdfError](operators(t"w")).reason
      . assert(_ == PdfError.Reason.MalformedOperator(t"w"))

      test(m"an inline image folds into one operator"):
        operators(t"BI /W 2 /H 2 /L 4 ID  EI") match
          case List(PdfOperator.InlineImage(parameters, data)) =>
            (parameters.at(t"W"), data.length)

          case _ =>
            (Unset, 0)
      . assert(_ == (Cos.Integral(2), 4))

    suite(m"Fonts"):
      test(m"a standard-14 font is recognized with its metrics"):
        PdfFile(contentPage(t"")).open():
          val font = pdf.pages(0).fonts(t"F1")
          (font.standard, font.width('A'))
      . assert(_ == (PdfFont.Standard.Helvetica, 667.0))

      test(m"declared widths override standard metrics"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Resources << /Font << /F1 4 0 R >> >> >>".in[Data],
            t"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /FirstChar 65 /Widths [800] >>"
            . in[Data] )

        PdfFile(doc).open():
          val font = pdf.pages(0).fonts(t"F1")
          (font.width('A'), font.width('B'))
      . assert(_ == (800.0, 667.0))

      test(m"differences remap codes through glyph names"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Resources << /Font << /F1 4 0 R >> >> >>".in[Data],
            t"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding << /BaseEncoding /WinAnsiEncoding /Differences [65 /eacute] >> >>"
            . in[Data] )

        PdfFile(doc).open():
          pdf.pages(0).fonts(t"F1").decode(data('A', 'B', 0x93))
      . assert(_ == t"éB“")

      test(m"a ToUnicode map takes precedence"):
        val cmap = t"/CIDInit /ProcSet findresource begin begincmap 1 begincodespacerange <00> <FF> endcodespacerange 2 beginbfchar <41> <0042> endbfchar 1 beginbfrange <60> <62> <0070> endbfrange endcmap end"

        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Resources << /Font << /F1 4 0 R >> >> >>".in[Data],
            t"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /ToUnicode 5 0 R >>".in[Data],
            t"<< /Length ${cmap.length} >>\nstream\n$cmap\nendstream".in[Data] )

        PdfFile(doc).open():
          pdf.pages(0).fonts(t"F1").decode(data('A', 0x60, 0x61, 0x62))
      . assert(_ == t"Bpqr")

      test(m"a Type0 font reads two-byte codes and CID widths"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 100 100] >>".in[Data],
            t"<< /Type /Page /Parent 2 0 R /Resources << /Font << /F1 4 0 R >> >> >>".in[Data],
            t"<< /Type /Font /Subtype /Type0 /BaseFont /Test /Encoding /Identity-H /DescendantFonts [5 0 R] >>"
            . in[Data],
            t"<< /Type /Font /Subtype /CIDFontType2 /BaseFont /Test /DW 750 /W [10 [600 650] 20 22 500] >>"
            . in[Data] )

        PdfFile(doc).open():
          val font = pdf.pages(0).fonts(t"F1")
          (font.codes(data(0, 10, 0, 11)), font.width(10), font.width(21), font.width(99))
      . assert(_ == (List(10, 11), 600.0, 500.0, 750.0))

    suite(m"Text extraction"):
      def extracted(content: Text): Text =
        PdfFile(contentPage(content)).open():
          pdf.pages(0).text

      test(m"a single show operation extracts its text"):
        extracted(t"BT /F1 12 Tf 72 720 Td (Hello) Tj ET")
      . assert(_ == t"Hello")

      test(m"a gap between shows on a baseline becomes a space"):
        extracted(t"BT /F1 12 Tf 72 720 Td (Hello) Tj 100 0 Td (world) Tj ET")
      . assert(_ == t"Hello world")

      test(m"adjacent shows do not gain a space"):
        extracted(t"BT /F1 12 Tf 72 720 Td (Hel) Tj (lo) Tj ET")
      . assert(_ == t"Hello")

      test(m"a baseline change becomes a newline"):
        extracted(t"BT /F1 12 Tf 72 720 Td (one) Tj 0 -14 Td (two) Tj ET")
      . assert(_ == t"one\ntwo")

      test(m"T* advances by the leading"):
        extracted(t"BT /F1 12 Tf 14 TL 72 720 Td (one) Tj T* (two) Tj ET")
      . assert(_ == t"one\ntwo")

      test(m"a large TJ adjustment reads as a space"):
        extracted(t"BT /F1 12 Tf 72 720 Td [(Hello) -600 (world)] TJ ET")
      . assert(_ == t"Hello world")

      test(m"runs carry their positions in points"):
        PdfFile(contentPage(t"BT /F1 12 Tf 72 720 Td (Hello) Tj ET")).open():
          pdf.pages(0).runs match
            case List(run) => (run.x, run.y, run.size, run.text)
            case _         => (Quantity[Points[1]](0.0), Quantity[Points[1]](0.0),
                                  Quantity[Points[1]](0.0), t"")
      . assert(_ == (Quantity[Points[1]](72.0), Quantity[Points[1]](720.0),
          Quantity[Points[1]](12.0), t"Hello"))

      test(m"the transformation matrix scales positions"):
        PdfFile(contentPage(t"q 2 0 0 2 0 0 cm BT /F1 12 Tf 50 100 Td (X) Tj ET Q")).open():
          pdf.pages(0).runs match
            case List(run) => (run.x, run.size)
            case _         => (Quantity[Points[1]](0.0), Quantity[Points[1]](0.0))
      . assert(_ == (Quantity[Points[1]](100.0), Quantity[Points[1]](24.0)))

      test(m"text is a pure value and escapes the scope"):
        val kept = PdfFile(contentPage(t"BT /F1 12 Tf 72 720 Td (Kept) Tj ET")).open():
          pdf.pages(0).text

        kept
      . assert(_ == t"Kept")

    suite(m"Streaming payloads"):
      def drain(stream: (Stream[Data] over Credit)^): Data =
        val builder = Array.newBuilder[Byte]

        def recur(): Unit = stream.refill(Credit(4096)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[Array[Byte]]
            var i = 0

            while i < count do
              builder += window(stream.start + i)
              i += 1

            stream.skip(count)
            recur()

          case _ =>
            ()

        recur()
        builder.result().immutable(using Unsafe)

      def streamed(body: Data): Text =
        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body =>
              String(drain(pdf.spring(body)()).mutable(using Unsafe), "UTF-8").tt

            case _ =>
              t""

      test(m"a raw payload streams in chunks"):
        streamed(t"<< /Length 11 >>\nstream\nHello world\nendstream".in[Data])
      . assert(_ == t"Hello world")

      test(m"a Flate payload streams through the zlib duct"):
        val payload = deflate(t"streamed and inflated".in[Data])

        val body = t"<< /Length ${payload.length} /Filter /FlateDecode >>\nstream\n".in[Data]
          ++ payload ++ t"\nendstream".in[Data]

        streamed(body)
      . assert(_ == t"streamed and inflated")

      test(m"a gathered filter delivers through flush"):
        streamed(t"<< /Length 11 /Filter /ASCIIHexDecode >>\nstream\n48656C6C6F>\nendstream"
          . in[Data])
      . assert(_ == t"Hello")

      test(m"a spring re-materializes the same content"):
        val payload = deflate(t"again and again".in[Data])

        val body = t"<< /Length ${payload.length} /Filter /FlateDecode >>\nstream\n".in[Data]
          ++ payload ++ t"\nendstream".in[Data]

        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body =>
              val spring = pdf.spring(body)
              val first = String(drain(spring()).mutable(using Unsafe), "UTF-8").tt
              val second = String(drain(spring()).mutable(using Unsafe), "UTF-8").tt
              (first, second)

            case _ =>
              (t"", t"")
      . assert(_ == (t"again and again", t"again and again"))

    suite(m"Encryption"):
      val padding: Array[Byte] = Array[Byte]
        ( 0x28, 0xbf.toByte, 0x4e, 0x5e, 0x4e, 0x75, 0x8a.toByte, 0x41, 0x64, 0x00, 0x4e,
          0x56, 0xff.toByte, 0xfa.toByte, 0x01, 0x08, 0x2e, 0x2e, 0x00, 0xb6.toByte,
          0xd0.toByte, 0x68, 0x3e, 0x80.toByte, 0x2f, 0x0c, 0xa9.toByte, 0xfe.toByte,
          0x64, 0x53, 0x69, 0x7a )

      def hexOf(bytes: Array[Byte]): Text =
        val builder = StringBuilder()
        var i = 0
        while i < bytes.length do
          builder.append(f"${bytes(i) & 0xff}%02x")
          i += 1
        builder.toString.tt

      def xor(bytes: Array[Byte], value: Int): Array[Byte] =
        val out = new Array[Byte](bytes.length)
        var i = 0
        while i < bytes.length do
          out(i) = (bytes(i) ^ value).toByte
          i += 1
        out

      def md5(chunks: Array[Byte]*): Array[Byte] =
        val digest = js.MessageDigest.getInstance("MD5").nn
        chunks.foreach(digest.update(_))
        digest.digest().nn

      // A test-side implementation of the standard security handler's *encryption* — the
      // inverse of the reader's `Guard`, and independently written — used to build encrypted
      // fixtures with an empty user password.
      def rc4(key: Array[Byte], data: Array[Byte]): Array[Byte] =
        Rc4(key.immutable(using Unsafe), data.immutable(using Unsafe)).mutable(using Unsafe)

      val id: Array[Byte] =
        val bytes = new Array[Byte](16)
        var i = 0
        while i < 16 do
          bytes(i) = i.toByte
          i += 1
        bytes

      // Builds an RC4-encrypted document (revision 2 = 40-bit, revision 3 = 128-bit) of the
      // catalog plus one string-bearing object and one stream object.
      def rc4Document(revision: Int): Data =
        val keyBytes = if revision == 2 then 5 else 16
        val permissions = -44

        val ownerKey =
          var hash = md5(padding)
          if revision >= 3 then for _ <- 0 until 50 do hash = md5(hash.take(keyBytes))
          hash.take(keyBytes)

        val ownerEntry =
          var value = rc4(ownerKey, padding)
          if revision >= 3 then for i <- 1 to 19 do value = rc4(xor(ownerKey, i), value)
          value

        val permBytes = Array((permissions & 0xff).toByte, ((permissions >> 8) & 0xff).toByte,
            ((permissions >> 16) & 0xff).toByte, ((permissions >> 24) & 0xff).toByte)

        val fileKey =
          var hash = md5(padding, ownerEntry, permBytes, id)
          if revision >= 3 then for _ <- 0 until 50 do hash = md5(hash.take(keyBytes))
          hash.take(keyBytes)

        val userEntry =
          if revision == 2 then rc4(fileKey, padding)
          else
            var value = rc4(fileKey, md5(padding, id))
            for i <- 1 to 19 do value = rc4(xor(fileKey, i), value)
            value ++ new Array[Byte](16)

        def objectKey(number: Int, generation: Int): Array[Byte] =
          md5(fileKey, Array((number & 0xff).toByte, ((number >> 8) & 0xff).toByte,
              ((number >> 16) & 0xff).toByte, (generation & 0xff).toByte,
              ((generation >> 8) & 0xff).toByte)).take((keyBytes + 5).min(16))

        def hex(bytes: Array[Byte]): Text = hexOf(bytes)

        val version = if revision == 2 then 1 else 2
        val secret = rc4(objectKey(2, 0), t"Secret".s.getBytes("ISO-8859-1").nn)
        val streamPlain = t"encrypted stream".s.getBytes("ISO-8859-1").nn
        val streamCipher = rc4(objectKey(3, 0), streamPlain)

        val encrypt =
          t"<< /Filter /Standard /V $version /R $revision /Length ${keyBytes*8} /P $permissions /O <${hex(ownerEntry)}> /U <${hex(userEntry)}> >>"

        buildEncrypted
          ( encrypt,
            t"<< /Type /Catalog >>".in[Data],
            t"<< /Secret <${hex(secret)}> >>".in[Data],
            (t"<< /Length ${streamCipher.length} >>\nstream\n".in[Data]
                ++ streamCipher.immutable(using Unsafe) ++ t"\nendstream".in[Data]) )

      // Assembles a document with an `/Encrypt` entry (object N+1) and an `/ID`.
      def buildEncrypted(encrypt: Text, bodies: Data*): Data =
        var out: Data = t"%PDF-1.6\n".in[Data]
        val offsets = List.newBuilder[Long]

        bodies.zipWithIndex.each: (body, index) =>
          offsets += out.length.toLong
          out = out ++ t"${index + 1} 0 obj\n".in[Data] ++ body ++ t"\nendobj\n".in[Data]

        val encryptNumber = bodies.length + 1
        offsets += out.length.toLong
        out = out ++ t"$encryptNumber 0 obj\n".in[Data] ++ encrypt.in[Data] ++ t"\nendobj\n".in[Data]

        val idHex = hexOf(id)
        val xrefOffset = out.length
        out = out ++ t"xref\n0 ${encryptNumber + 1}\n0000000000 65535 f \n".in[Data]

        offsets.result().each: offset =>
          out = out ++ t"${pad10(offset)} 00000 n \n".in[Data]

        out ++ t"trailer\n<< /Size ${encryptNumber + 1} /Root 1 0 R /Encrypt $encryptNumber 0 R /ID [<$idHex> <$idHex>] >>\nstartxref\n$xrefOffset\n%%EOF".in[Data]

      // An AES-256 (revision 6) fixture, whose key derivation the reader must mirror exactly.
      def aes256Document(password: Text): Data =
        def hash6(pw: Array[Byte], salt: Array[Byte]): Array[Byte] =
          var k: Array[Byte] = md5(pw, salt) // placeholder, replaced below
          val sha256 = js.MessageDigest.getInstance("SHA-256").nn
          sha256.update(pw)
          sha256.update(salt)
          k = sha256.digest().nn
          var round = 0
          var done = false

          while !done do
            val block = Array.newBuilder[Byte]
            for _ <- 0 until 64 do
              block.addAll(pw)
              block.addAll(k)

            val input = block.result()
            val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn
            cipher.init(jc.Cipher.ENCRYPT_MODE, jcs.SecretKeySpec(k.take(16), "AES"),
                jcs.IvParameterSpec(k.slice(16, 32)))
            val e = cipher.doFinal(input).nn
            var sum = 0
            for i <- 0 until 16 do sum += e(i) & 0xff
            val algo = sum%3 match
              case 0 => "SHA-256"
              case 1 => "SHA-384"
              case _ => "SHA-512"
            k = js.MessageDigest.getInstance(algo).nn.digest(e).nn
            round += 1
            if round >= 64 && (e(e.length - 1) & 0xff) <= round - 32 then done = true

          k.take(32)

        val pw = password.s.getBytes("UTF-8").nn
        val random = js.SecureRandom()
        val userSalt = new Array[Byte](8)
        val userKeySalt = new Array[Byte](8)
        random.nextBytes(userSalt)
        random.nextBytes(userKeySalt)

        val fileKey = new Array[Byte](32)
        random.nextBytes(fileKey)

        val userEntry = hash6(pw, userSalt) ++ userSalt ++ userKeySalt
        val intermediate = hash6(pw, userKeySalt)

        val wrap = jc.Cipher.getInstance("AES/CBC/NoPadding").nn
        wrap.init(jc.Cipher.ENCRYPT_MODE, jcs.SecretKeySpec(intermediate, "AES"),
            jcs.IvParameterSpec(new Array[Byte](16)))
        val ue = wrap.doFinal(fileKey).nn

        def hex(bytes: Array[Byte]): Text = hexOf(bytes)

        def encryptStream(number: Int, plain: Array[Byte]): Array[Byte] =
          val iv = new Array[Byte](16)
          random.nextBytes(iv)
          val padLength = 16 - plain.length%16
          val padded = plain ++ Array.fill(padLength)(padLength.toByte)
          val cipher = jc.Cipher.getInstance("AES/CBC/NoPadding").nn
          cipher.init(jc.Cipher.ENCRYPT_MODE, jcs.SecretKeySpec(fileKey, "AES"),
              jcs.IvParameterSpec(iv))
          iv ++ cipher.doFinal(padded).nn

        val secret = encryptStream(2, t"Secret".s.getBytes("UTF-8").nn)
        val streamCipher = encryptStream(3, t"encrypted stream".s.getBytes("UTF-8").nn)

        val ownerHex = hex(new Array[Byte](48))
        val encrypt =
          t"<< /Filter /Standard /V 5 /R 6 /Length 256 /P -44 /O <$ownerHex> /U <${hex(userEntry)}> /UE <${hex(ue)}> /CF << /StdCF << /CFM /AESV3 >> >> /StmF /StdCF /StrF /StdCF >>"

        buildEncrypted
          ( encrypt,
            t"<< /Type /Catalog >>".in[Data],
            (t"<< /Secret <${hex(secret)}> >>".in[Data]),
            (t"<< /Length ${streamCipher.length} >>\nstream\n".in[Data]
                ++ streamCipher.immutable(using Unsafe) ++ t"\nendstream".in[Data]) )

      test(m"RC4 matches its known-answer vector"):
        Rc4(t"Key".in[Data], t"Plaintext".in[Data]).to(List).map(b => f"${b & 0xff}%02X").mkString.tt
      . assert(_ == t"BBF316E8D940AF0AD3")

      test(m"an encrypted document reports it"):
        PdfFile(rc4Document(3)).open():
          pdf.encrypted
      . assert(_ == true)

      test(m"a revision-3 string decrypts with the empty password"):
        PdfFile(rc4Document(3)).open():
          pdf.resolved(pdf(2, 0)(t"Secret").or(Cos.Nil)).text
      . assert(_ == t"Secret")

      test(m"a revision-3 stream decrypts"):
        PdfFile(rc4Document(3)).open():
          pdf(3, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"encrypted stream")

      test(m"a revision-2 (40-bit) string decrypts"):
        PdfFile(rc4Document(2)).open():
          pdf.resolved(pdf(2, 0)(t"Secret").or(Cos.Nil)).text
      . assert(_ == t"Secret")

      test(m"an AES-256 string decrypts with the right password"):
        PdfFile(aes256Document(t"open sesame")).open(Password(t"open sesame")):
          pdf.resolved(pdf(2, 0)(t"Secret").or(Cos.Nil)).text
      . assert(_ == t"Secret")

      test(m"an AES-256 stream decrypts"):
        PdfFile(aes256Document(t"open sesame")).open(Password(t"open sesame")):
          pdf(3, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"encrypted stream")

      test(m"a wrong password is rejected at open"):
        capture[PdfError]:
          PdfFile(aes256Document(t"open sesame")).open(Password(t"wrong"))(pdf.version)
        . reason
      . assert(_ == PdfError.Reason.BadPassword)

      test(m"an edit to an RC4 document is encrypted on write and decrypts on re-read"):
        val path = tempPdf(rc4Document(3))

        PdfFile(path).open(Read & Write): doc ?=>
          doc.set(Cos.Ref(1, 0),
              Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Catalog"), t"Marker" -> Cos.Chars(t"added".in[Data]))))

        // Re-reading (with the empty password) must decrypt the newly-written string; a
        // failure to encrypt on write would surface as garbage here.
        PdfFile(fileBytes(path)).open():
          pdf.resolved(pdf(1, 0))(t"Marker").let(_.text)
      . assert(_ == t"added")

      test(m"an edit to an AES-256 document round-trips"):
        val path = tempPdf(aes256Document(t"open sesame"))

        PdfFile(path).open(Read & Write, Password(t"open sesame")): doc ?=>
          doc.set(Cos.Ref(1, 0),
              Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Catalog"), t"Marker" -> Cos.Chars(t"aes".in[Data]))))

        PdfFile(fileBytes(path)).open(Password(t"open sesame")):
          pdf.resolved(pdf(1, 0))(t"Marker").let(_.text)
      . assert(_ == t"aes")

      test(m"a newly-written stream in an encrypted document decrypts on re-read"):
        val path = tempPdf(rc4Document(3))

        PdfFile(path).open(Read & Write): doc ?=>
          val ref = doc.allocate(doc.newStream(t"secret stream".in[Data]))
          doc.set(Cos.Ref(1, 0),
              Cos.Dictionary(Map(t"Type" -> Cos.Name(t"Catalog"), t"Extra" -> ref)))

        PdfFile(fileBytes(path)).open():
          pdf.resolved(pdf(1, 0)(t"Extra").or(Cos.Nil)) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"secret stream")

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

      test(m"ASCII85Decode decodes a full group"):
        String
          ( Filter.decode(t"9jqo^~>".in[Data], List((Filter.Id.Ascii85, Map())))
            . mutable(using Unsafe), "UTF-8" ).tt
      . assert(_ == t"Man ")

      test(m"ASCII85Decode decodes a partial final group"):
        String
          ( Filter.decode(t"9jqo~>".in[Data], List((Filter.Id.Ascii85, Map())))
            . mutable(using Unsafe), "UTF-8" ).tt
      . assert(_ == t"Man")

      test(m"the z shorthand is four zero bytes"):
        Filter.decode(t"z~>".in[Data], List((Filter.Id.Ascii85, Map()))).to(List).map(_.toInt)
      . assert(_ == List(0, 0, 0, 0))

      test(m"LZWDecode decodes the specification's example"):
        val encoded = data(0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01)
        String
          ( Filter.decode(encoded, List((Filter.Id.Lzw, Map()))).mutable(using Unsafe),
            "UTF-8" ).tt
      . assert(_ == t"-----A---B")

      test(m"a wrong stream length falls back to the endstream keyword"):
        val body = t"<< /Length 3 >>\nstream\nHello\nendstream".in[Data]

        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"Hello")

      test(m"a missing stream length falls back to the endstream keyword"):
        val body = t"<< /Kind /Bare >>\nstream\nHello\nendstream".in[Data]

        PdfFile(document(catalog, body)).open():
          pdf(2, 0) match
            case body: Cos.Body => String(pdf.payload(body).mutable(using Unsafe), "UTF-8").tt
            case _              => t""
      . assert(_ == t"Hello")

      test(m"a hybrid-reference file resolves its compressed objects"):
        var out: Data = t"%PDF-1.5\n".in[Data]

        val offset1 = out.length
        out = out ++ t"1 0 obj\n<< /Type /Catalog /Value 4 0 R >>\nendobj\n".in[Data]

        // An object stream holding just object 4, whose pair table is 4 bytes long.
        val offset2 = out.length
        out = out ++ t"2 0 obj\n<< /Type /ObjStm /N 1 /First 4 /Length 6 >>\nstream\n".in[Data]
          ++ t"4 0\n99".in[Data] ++ t"\nendstream\nendobj\n".in[Data]

        // The cross-reference stream covering only object 4, as compressed.
        val offset3 = out.length
        out = out
          ++ t"3 0 obj\n<< /Type /XRef /Size 5 /W [1 2 1] /Index [4 1] /Length 4 >>\nstream\n"
             . in[Data]
          ++ data(2, 0, 2, 0) ++ t"\nendstream\nendobj\n".in[Data]

        // The classic table marks object 4 free — the hybrid signature — and points at the
        // cross-reference stream through /XRefStm.
        val xrefOffset = out.length
        out = out ++ t"xref\n0 5\n0000000000 65535 f \n".in[Data]
          ++ t"${pad10(offset1)} 00000 n \n${pad10(offset2)} 00000 n \n".in[Data]
          ++ t"${pad10(offset3)} 00000 n \n0000000000 00000 f \n".in[Data]
          ++ t"trailer\n<< /Size 5 /Root 1 0 R /XRefStm $offset3 >>\n".in[Data]
          ++ t"startxref\n$xrefOffset\n%%EOF".in[Data]

        PdfFile(out).open():
          pdf.resolved(pdf(1, 0)(t"Value").or(Cos.Nil))
      . assert(_ == Cos.Integral(99))

      test(m"XMP metadata surfaces as raw bytes"):
        val doc = document
          ( t"<< /Type /Catalog /Pages 2 0 R /Metadata 3 0 R >>".in[Data],
            t"<< /Type /Pages /Kids [] /Count 0 >>".in[Data],
            t"<< /Type /Metadata /Subtype /XML /Length 5 >>\nstream\n<xmp/\nendstream".in[Data] )

        PdfFile(doc).open():
          pdf.xmp.let(bytes => String(bytes.mutable(using Unsafe), "UTF-8").tt)
      . assert(_ == t"<xmp/")
