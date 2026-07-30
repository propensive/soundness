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
package pneumatic

import java.io as ji

import soundness.*

import charEncoders.utf8Encoder, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
import threading.platformThreading
import strategies.throwUnsafely
import probates.panicProbate
import errorDiagnostics.emptyDiagnostics

import proscenium.compat.*

object Tests extends Suite(m"Pneumatic tests"):
  def run(): Unit =
    suite(m"Compression tests"):
      test(m"Compress a single block with GZip"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].stdlib.map(_.readable).flatten.to(proscenium.List)
      . assert(_ == Data(1, 1, 2, 3, 5, 8, 13, 21, 34).compress[Gzip].to[List])

      test(m"Roundtrip compress/decompress a single block with GZip"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].decompress[Gzip]
      . assert: stream => stream === proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      val longData: Progression[Data] =
        proscenium.Progression.from(proscenium.Progression.continually(Array.from((0 to 255).map(_.toByte))).stdlib.take(1000))

      test(m"Roundtrip compress/decompress a long repetitive stream with Gzip"):
        longData.compress[Gzip].decompress[Gzip]
      . assert(_.stdlib.map(_.readable).flatten == longData.stdlib.map(_.readable).flatten)

      // The whole-value forms (`Duct.feed` over the format ducts) must
      // interoperate with the stream forms in both directions, per format.
      val wholeData: Data = Array.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)

      for format <- List(t"Gzip", t"Zlib", t"Deflate") do
        test(m"whole-value compress roundtrips through whole-value decompress ($format)"):
          format.s match
            case "Gzip"    => wholeData.compress[Gzip].decompress[Gzip].to[List]
            case "Zlib"    => wholeData.compress[Zlib].decompress[Zlib].to[List]
            case _         => wholeData.compress[Deflate].decompress[Deflate].to[List]
        . assert(_ == wholeData.to[List])

        test(m"whole-value compress feeds the stream decompressor ($format)"):
          format.s match
            case "Gzip"    => wholeData.compress[Gzip].stream.decompress[Gzip].memoize.to[List]
            case "Zlib"    => wholeData.compress[Zlib].stream.decompress[Zlib].memoize.to[List]
            case _ => wholeData.compress[Deflate].stream.decompress[Deflate].memoize.to[List]
        . assert(_ == wholeData.to[List])

        test(m"stream compress feeds the whole-value decompressor ($format)"):
          format.s match
            case "Gzip"    => wholeData.stream.compress[Gzip].memoize.decompress[Gzip].to[List]
            case "Zlib"    => wholeData.stream.compress[Zlib].memoize.decompress[Zlib].to[List]
            case _ => wholeData.stream.compress[Deflate].memoize.decompress[Deflate].to[List]
        . assert(_ == wholeData.to[List])

      test(m"Roundtrip compress/decompress a single block with LZW"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.map(_.readable).flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.map(_.readable).flatten)

      // Varied enough to push the code table through its 9-, 10- and 11-bit widths.
      val variedData: Progression[Data] =
        proscenium.Progression(Array.from((0 until 20000).map { index => ((index*index + index/3)%251).toByte }).asInstanceOf[Data])

      test(m"Roundtrip compress/decompress across LZW width growth"):
        variedData.compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.map(_.readable).flatten == variedData.stdlib.map(_.readable).flatten)

      test(m"Roundtrip compress/decompress a long stream across LZW table clears"):
        longData.compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.map(_.readable).flatten == longData.stdlib.map(_.readable).flatten)

      test(m"LZW without early change also roundtrips"):
        Lzw.decompress(Lzw.compress(variedData, earlyChange = false), earlyChange = false)
      . assert(_.stdlib.map(_.readable).flatten == variedData.stdlib.map(_.readable).flatten)
      test(m"Compress a single block with Zlib"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].stdlib.map(_.readable).flatten.to(proscenium.List)
      . assert(_ == Data(1, 1, 2, 3, 5, 8, 13, 21, 34).compress[Zlib].to[List])

      test(m"Roundtrip compress/decompress a single block with Zlib"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].decompress[Zlib]
      . assert: stream => stream === proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip compress/decompress a long repetitive stream with Zlib"):
        longData.compress[Zlib].decompress[Zlib]
      . assert(_.stdlib.map(_.readable).flatten == longData.stdlib.map(_.readable).flatten)

      test(m"Compress a single block with Deflate"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].stdlib.map(_.readable).flatten.to(proscenium.List)
      . assert(_ == Data(1, 1, 2, 3, 5, 8, 13, 21, 34).compress[Deflate].to[List])

      test(m"Roundtrip compress/decompress a single block with Deflate"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].decompress[Deflate]
      . assert: stream => stream === proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip a long repetitive Deflate stream"):
        longData.compress[Deflate].decompress[Deflate]
      . assert(_.stdlib.map(_.readable).flatten == longData.stdlib.map(_.readable).flatten)

    suite(m"Pure DEFLATE implementation tests"):
      // On the JVM the formats run over `java.util.zip`, so the pure-Scala port (used on
      // Scala.js and WASI) is exercised directly here, cross-validated against the JDK's zlib
      // in both directions and via byte-for-byte output equality.
      val corpus: Data =
        Array.from((0 until 300000).map { index => ((index*31 + (index >> 6)) & 0xff).toByte })

      def jdkInflate(data: Data, nowrap: Boolean): List[Byte] =
        val inflater = java.util.zip.Inflater(nowrap)
        inflater.setInput(Array.unsafeJvm(data))
        val out = ji.ByteArrayOutputStream()
        val buffer = new scala.Array[Byte](4096)

        while !inflater.finished && !inflater.needsInput do
          val count = inflater.inflate(buffer)
          out.write(buffer, 0, count)

        inflater.end()
        Array.unsafeFrozen(out.toByteArray.nn).toList

      def jdkDeflate(data: Data, nowrap: Boolean): Data =
        val deflater = java.util.zip.Deflater(-1, nowrap)
        deflater.setInput(Array.unsafeJvm(data))
        deflater.finish()
        val out = ji.ByteArrayOutputStream()
        val buffer = new scala.Array[Byte](4096)

        while !deflater.finished do
          val count = deflater.deflate(buffer)
          out.write(buffer, 0, count)

        deflater.end()
        Array.unsafeFrozen(out.toByteArray.nn)

      def pureDeflate(data: Data, nowrap: Boolean): Data =
        val deflater = Deflater(-1, nowrap)
        deflater.setInput(Array.unsafeJvm(data))
        deflater.finish()
        val out = ji.ByteArrayOutputStream()
        val buffer = new scala.Array[Byte](4096)

        while !deflater.finished do
          val count = deflater.deflate(buffer, 0, buffer.length)
          out.write(buffer, 0, count)

        Array.unsafeFrozen(out.toByteArray.nn)

      def pureInflate(data: Data, nowrap: Boolean, chunk: Int): List[Byte] =
        val inflater = Inflater(nowrap)
        val bytes = Array.unsafeJvm(data)
        val out = ji.ByteArrayOutputStream()
        val buffer = new scala.Array[Byte](4096)
        var position = 0

        while position < bytes.length && !inflater.finished do
          val length = chunk.min(bytes.length - position)
          inflater.setInput(bytes, position, length)
          var run = 1

          while run > 0 do
            run = inflater.inflate(buffer, 0, buffer.length)
            out.write(buffer, 0, run)

          position += length - inflater.getRemaining

        Array.unsafeFrozen(out.toByteArray.nn).toList

      test(m"pure deflate output inflates with the JDK (raw)"):
        jdkInflate(pureDeflate(corpus, true), true)
      . assert(_ == corpus.to[List])

      test(m"pure deflate output inflates with the JDK (zlib)"):
        jdkInflate(pureDeflate(corpus, false), false)
      . assert(_ == corpus.to[List])

      test(m"JDK deflate output inflates with the pure implementation (raw)"):
        pureInflate(jdkDeflate(corpus, true), true, Int.MaxValue)
      . assert(_ == corpus.to[List])

      test(m"JDK deflate output inflates with the pure implementation (zlib)"):
        pureInflate(jdkDeflate(corpus, false), false, Int.MaxValue)
      . assert(_ == corpus.to[List])

      test(m"pure inflate succeeds fed seven bytes at a time"):
        pureInflate(jdkDeflate(corpus, false), false, 7)
      . assert(_ == corpus.to[List])

      test(m"pure deflate output is byte-identical to the JDK's (raw)"):
        pureDeflate(corpus, true).to[List]
      . assert(_ == jdkDeflate(corpus, true).to[List])

      test(m"pure deflate output is byte-identical to the JDK's (zlib)"):
        pureDeflate(corpus, false).to[List]
      . assert(_ == jdkDeflate(corpus, false).to[List])

      test(m"pure roundtrip without the JDK"):
        pureInflate(pureDeflate(corpus, false), false, 4096)
      . assert(_ == corpus.to[List])

      test(m"Whole-value gzip roundtrips through gunzip"):
        corpus.gzip.gunzip.to[List]
      . assert(_ == corpus.to[List])

      test(m"A gzip stream with optional header fields decodes"):
        // FLG = FEXTRA | FNAME | FCOMMENT exercises every optional-field state
        val out = ji.ByteArrayOutputStream()
        val payload: Data = t"optional header fields".in[Data]
        val deflated = jdkDeflate(payload, true)
        val crc = java.util.zip.CRC32()
        crc.update(Array.unsafeJvm(payload))

        val headerStart: scala.Array[Byte] =
          scala.Array[Byte](31, -117, 8, (4 | 8 | 16).toByte, 0, 0, 0, 0, 0, -1)

        out.write(headerStart)
        out.write(scala.Array[Byte](3, 0)) // XLEN = 3
        out.write(scala.Array[Byte](1, 2, 3)) // extra field
        out.write(scala.Array[Byte]('n', 'a', 'm', 'e', 0)) // zero-terminated name
        out.write(scala.Array[Byte]('c', 'o', 'm', 'm', 'e', 'n', 't', 0)) // zero-terminated comment
        out.write(Array.unsafeJvm(deflated))

        var index = 0
        while index < 4 do
          out.write(((crc.getValue >>> (index*8)) & 0xff).toInt)
          index += 1

        index = 0
        while index < 4 do
          out.write(((payload.length >>> (index*8)) & 0xff).toInt)
          index += 1

        Array.unsafeFrozen(out.toByteArray.nn).decompress[Gzip].to[List]
      . assert(_ == t"optional header fields".in[Data].to[List])


    suite(m"Brotli tests"):
      // Golden vectors: real output of the reference `brotli` CLI, decoded here. These validate the
      // decoder against the reference implementation, not merely against our own encoder.
      val xBrotli: Data = Data(11, 0, -128, 120, 3)
      val tenXtenYBrotli: Data = Data(27, 19, 0, 0, -92, -80, -78, -22, -127, 71, 2, 73)
      val ukkonooaBrotli: Data = Data(27, 81, 0, 0, 68, -73, 86, -86, -93, 91, -53, -62, -63, 13,
          -67, -7, -32, 11, 14, 57, -44, -125, 96, -96, 113, 64, -106, -76, 5, 27, 99, 56, -60, -79,
          106, 109, 102, -61, -35, 12, -16, -47, 37, -28, -38, 109, 60, -99, -119, -116, 75, 113, 44,
          12, 69, 90, -32, -45, -4, 66, 113, 47, 49, -73, 22)
      val foxBrotli: Data = Data(-113, 21, -128, 84, 104, 101, 32, 113, 117, 105, 99, 107, 32, 98,
          114, 111, 119, 110, 32, 102, 111, 120, 32, 106, 117, 109, 112, 115, 32, 111, 118, 101, 114,
          32, 116, 104, 101, 32, 108, 97, 122, 121, 32, 100, 111, 103, 46, 3)

      val ukkonooaPlain: Text = t"ukko nooa, ukko nooa oli kunnon mies, kun han meni saunaan, pisti laukun naulaan, "
      val foxPlain: Text = t"The quick brown fox jumps over the lazy dog."

      test(m"Decode reference Brotli output (single byte)"):
        xBrotli.decompress[Brotli].to[List]
      . assert(_ == t"x".in[Data].to[List])

      test(m"Decode reference Brotli output (run-length)"):
        tenXtenYBrotli.decompress[Brotli].to[List]
      . assert(_ == t"XXXXXXXXXXYYYYYYYYYY".in[Data].to[List])

      test(m"Decode reference Brotli output (natural-language text)"):
        ukkonooaBrotli.decompress[Brotli].to[List]
      . assert(_ == ukkonooaPlain.in[Data].to[List])

      test(m"Decode reference Brotli output using the static dictionary"):
        foxBrotli.decompress[Brotli].to[List]
      . assert(_ == foxPlain.in[Data].to[List])

      val brotliLong: Progression[Data] =
        proscenium.Progression.from(proscenium.Progression.continually(Array.from((0 to 255).map(_.toByte))).stdlib.take(1000))
      val brotliWhole: Data = Array.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)
      val brotliVaried: Data =
        Array.from((0 until 40000).map { index => ((index*index + index/3)%251).toByte })

      test(m"Roundtrip compress/decompress a single block with Brotli"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Brotli].decompress[Brotli]
      . assert(_.stdlib.map(_.readable).flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.map(_.readable).flatten)

      test(m"Roundtrip compress/decompress a long repetitive stream with Brotli"):
        brotliLong.compress[Brotli].decompress[Brotli]
      . assert(_.stdlib.map(_.readable).flatten == brotliLong.stdlib.map(_.readable).flatten)

      test(m"whole-value compress roundtrips through whole-value decompress (Brotli)"):
        brotliWhole.compress[Brotli].decompress[Brotli].to[List]
      . assert(_ == brotliWhole.to[List])

      test(m"whole-value compress feeds the stream decompressor (Brotli)"):
        brotliWhole.compress[Brotli].stream.decompress[Brotli].memoize.to[List]
      . assert(_ == brotliWhole.to[List])

      test(m"stream compress feeds the whole-value decompressor (Brotli)"):
        brotliWhole.stream.compress[Brotli].memoize.decompress[Brotli].to[List]
      . assert(_ == brotliWhole.to[List])

      test(m"Roundtrip varied data spanning many commands (Brotli)"):
        brotliVaried.compress[Brotli].decompress[Brotli].to[List]
      . assert(_ == brotliVaried.to[List])

      test(m"Brotli actually compresses a repetitive payload"):
        val payload = (t"the quick brown fox jumped " * 500).in[Data]
        payload.compress[Brotli].length < payload.length
      . assert(_ == true)

      test(m"Roundtrip a large multi-command payload (Brotli)"):
        val big = Array.from((0 until 2000000).map { index => ((index*31 + (index >> 6)) & 0xff).toByte })
        big.compress[Brotli].decompress[Brotli].to[List] == big.to[List]
      . assert(_ == true)

      test(m"Empty input roundtrips (Brotli)"):
        Data().compress[Brotli].decompress[Brotli].to[List]
      . assert(_ == Nil)

      test(m"Single byte roundtrips (Brotli)"):
        Data(42).compress[Brotli].decompress[Brotli].to[List]
      . assert(_ == List[Byte](42))


    suite(m"XZ tests"):
      // Golden vectors: real `xz` command-line output, decoded here — validating the decoder against
      // the reference implementation, not merely against our own encoder. All decode to "hello hello
      // hello world".
      val hello = t"hello hello hello world".in[Data].to[List]

      val crc64Xz: Data = Data(-3, 55, 122, 88, 90, 0, 0, 4, -26, -42, -76, 70, 4, -64, 24, 23, 33,
          1, 22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, -73, -61, 72, -32, 0, 22, 0, 16, 93, 0, 52, 25, 73,
          -18, -115, -23, 80, -106, 8, 6, -10, -24, -112, -109, -71, 32, 0, -64, 44, -125, 101, 28,
          18, -22, 117, 0, 1, 52, 23, 27, -61, 127, 24, 31, -74, -13, 125, 1, 0, 0, 0, 0, 4, 89, 90)

      val crc32Xz: Data = Data(-3, 55, 122, 88, 90, 0, 0, 1, 105, 34, -34, 54, 4, -64, 24, 23, 33, 1,
          22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, -73, -61, 72, -32, 0, 22, 0, 16, 93, 0, 52, 25, 73,
          -18, -115, -23, 80, -106, 8, 6, -10, -24, -112, -109, -71, 32, 0, 38, -26, 90, -127, 0, 1,
          48, 23, 31, 6, 19, 124, -112, 66, -103, 13, 1, 0, 0, 0, 0, 1, 89, 90)

      val noneXz: Data = Data(-3, 55, 122, 88, 90, 0, 0, 0, -1, 18, -39, 65, 4, -64, 24, 23, 33, 1,
          22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, -73, -61, 72, -32, 0, 22, 0, 16, 93, 0, 52, 25, 73,
          -18, -115, -23, 80, -106, 8, 6, -10, -24, -112, -109, -71, 32, 0, 0, 1, 44, 23, 66, 91,
          100, -102, 6, 114, -98, 122, 1, 0, 0, 0, 0, 0, 89, 90)

      val emptyXz: Data = Data(-3, 55, 122, 88, 90, 0, 0, 4, -26, -42, -76, 70, 0, 0, 0, 0, 28, -33,
          68, 33, 31, -74, -13, 125, 1, 0, 0, 0, 0, 4, 89, 90)

      val sha256Xz: Data = Data(-3, 55, 122, 88, 90, 0, 0, 10, -31, -5, 12, -95, 4, -64, 24, 23, 33,
          1, 22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, -73, -61, 72, -32, 0, 22, 0, 16, 93, 0, 52, 25, 73,
          -18, -115, -23, 80, -106, 8, 6, -10, -24, -112, -109, -71, 32, 0, 51, -116, -17, 103,
          -104, 86, 60, 79, -90, -31, 124, 97, 49, 108, -97, -109, -15, -37, 45, -122, -86, -120,
          17, -127, 55, 5, 109, 9, -32, -123, 9, 62, 0, 1, 76, 23, -27, 48, -103, -1, 24, -101, 75,
          -102, 1, 0, 0, 0, 0, 10, 89, 90)

      test(m"Decode reference xz output (SHA-256 check, verified)"):
        sha256Xz.decompress[Xz].to[List]
      . assert(_ == hello)

      test(m"A corrupted payload is detected by the integrity check"):
        // Flip a byte inside the LZMA2 payload (after the 12-byte stream and 12-byte block headers)
        // and confirm decoding rejects it — via either a decode error or a check mismatch.
        val original = (t"the quick brown fox " * 40).in[Data]
        val source = original.compress[Xz]

        // The tampered copy is built in an exclusive buffer and frozen once, so corrupting a
        // byte asserts nothing.
        val buffer = Array[Byte](source.length)
        buffer.copyFrom(source, 0, 0, source.length)
        buffer(36) = (buffer(36) ^ 0x55).toByte
        val corrupted: Data = Array.freeze(buffer)
        try corrupted.decompress[Xz].to[List] != original.to[List]
        catch case _: Exception => true
      . assert(_ == true)

      test(m"Decode reference xz output (CRC-64 check)"):
        crc64Xz.decompress[Xz].to[List]
      . assert(_ == hello)

      test(m"Decode reference xz output (CRC-32 check)"):
        crc32Xz.decompress[Xz].to[List]
      . assert(_ == hello)

      test(m"Decode reference xz output (no check)"):
        noneXz.decompress[Xz].to[List]
      . assert(_ == hello)

      test(m"Decode reference xz output (empty input)"):
        emptyXz.decompress[Xz].to[List]
      . assert(_ == Nil)

      val xzWhole: Data = Array.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)
      val xzLong: Progression[Data] =
        proscenium.Progression.from(proscenium.Progression.continually(Array.from((0 to 255).map(_.toByte))).stdlib.take(1000))
      val xzVaried: Data =
        Array.from((0 until 40000).map { index => ((index*index + index/3)%251).toByte })

      test(m"Roundtrip a single block with Xz"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Xz].decompress[Xz]
      . assert(_.stdlib.map(_.readable).flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.map(_.readable).flatten)

      test(m"Roundtrip a long repetitive stream with Xz"):
        xzLong.compress[Xz].decompress[Xz]
      . assert(_.stdlib.map(_.readable).flatten == xzLong.stdlib.map(_.readable).flatten)

      test(m"whole-value compress roundtrips through whole-value decompress (Xz)"):
        xzWhole.compress[Xz].decompress[Xz].to[List]
      . assert(_ == xzWhole.to[List])

      test(m"whole-value compress feeds the stream decompressor (Xz)"):
        xzWhole.compress[Xz].stream.decompress[Xz].memoize.to[List]
      . assert(_ == xzWhole.to[List])

      test(m"stream compress feeds the whole-value decompressor (Xz)"):
        xzWhole.stream.compress[Xz].memoize.decompress[Xz].to[List]
      . assert(_ == xzWhole.to[List])

      test(m"Roundtrip varied data spanning many commands (Xz)"):
        xzVaried.compress[Xz].decompress[Xz].to[List]
      . assert(_ == xzVaried.to[List])

      test(m"Xz actually compresses a repetitive payload"):
        val payload = (t"the quick brown fox jumped " * 500).in[Data]
        payload.compress[Xz].length < payload.length
      . assert(_ == true)

      test(m"Compress with an explicit fast preset and roundtrip (Xz)"):
        Xz.compress(proscenium.Progression(xzVaried), 1).decompress[Xz].stdlib.map(_.readable).flatten.to(proscenium.List)
      . assert(_ == xzVaried.to[List])

      test(m"Empty input roundtrips (Xz)"):
        Data().compress[Xz].decompress[Xz].to[List]
      . assert(_ == Nil)

      test(m"Single byte roundtrips (Xz)"):
        Data(42).compress[Xz].decompress[Xz].to[List]
      . assert(_ == List[Byte](42))

      test(m"Roundtrip a large multi-chunk payload (Xz)"):
        val big =
          Array.from((0 until 3000000).map { i => ((i*31 + (i >> 6)) & 0xff).toByte })
        big.compress[Xz].decompress[Xz].to[List] == big.to[List]
      . assert(_ == true)

      test(m"Streaming encoder emits multiple blocks past the dictionary (preset 0)"):
        // Preset 0 has a 256 KiB dictionary, so ~700 KiB spans several self-contained blocks; the
        // multi-block stream must roundtrip and be accepted by the reference `xz` binary.
        val payload: Data =
          Array.from((0 until 700000).map { i => ((i*31 + (i >> 6)) & 0xff).toByte })
        val encodedChunks = Xz.compress(Progression(payload), 0)
        val roundtrips = encodedChunks.decompress[Xz].stdlib.map(_.readable).flatten.to(proscenium.List) == payload.readable.to(proscenium.List)
        val encodedBytes: Data = Array.from(encodedChunks.stdlib.map(_.readable).flatten)
        val byXz =
          try
            val process = ProcessBuilder("xz", "-d", "-c").start().nn
            process.getOutputStream.nn.write(Array.unsafeJvm(encodedBytes))
            process.getOutputStream.nn.close()
            val decoded = process.getInputStream.nn.readAllBytes().nn
            process.waitFor()
            Array.unsafeFrozen(decoded).toList == payload.readable.to(proscenium.List)
          catch case _: ji.IOException => true
        roundtrips && byXz
      . assert(_ == true)

      // Cross-check: the real `xz` binary must decode what we produce.
      def xzBinaryDecodes(data: Data): Boolean =
        try
          val encoded = data.compress[Xz]
          val process = ProcessBuilder("xz", "-d", "-c").start().nn
          val stdin = process.getOutputStream.nn
          stdin.write(Array.unsafeJvm(encoded))
          stdin.close()
          val decoded = process.getInputStream.nn.readAllBytes().nn
          process.waitFor()
          process.exitValue() == 0 && Array.unsafeFrozen(decoded).toList == data.readable.to(proscenium.List)
        catch case _: ji.IOException => true // xz binary unavailable; skip

      test(m"The xz binary decodes our output (repetitive)"):
        xzBinaryDecodes((t"the quick brown fox " * 400).in[Data])
      . assert(_ == true)

      test(m"The xz binary decodes our output (varied)"):
        xzBinaryDecodes(xzVaried)
      . assert(_ == true)

    suite(m"LZMA2 tests"):
      val lzma2Whole: Data = Array.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)
      val lzma2Long: Progression[Data] =
        proscenium.Progression.from(proscenium.Progression.continually(Array.from((0 to 255).map(_.toByte))).stdlib.take(1000))
      val lzma2Varied: Data =
        Array.from((0 until 40000).map { index => ((index*index + index/3)%251).toByte })

      test(m"Roundtrip a single block with raw LZMA2"):
        proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Lzma2].decompress[Lzma2]
      . assert(_.stdlib.map(_.readable).flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.map(_.readable).flatten)

      test(m"Roundtrip a long repetitive stream with raw LZMA2"):
        lzma2Long.compress[Lzma2].decompress[Lzma2]
      . assert(_.stdlib.map(_.readable).flatten == lzma2Long.stdlib.map(_.readable).flatten)

      test(m"whole-value compress roundtrips through whole-value decompress (LZMA2)"):
        lzma2Whole.compress[Lzma2].decompress[Lzma2].to[List]
      . assert(_ == lzma2Whole.to[List])

      test(m"whole-value compress feeds the stream decompressor (LZMA2)"):
        lzma2Whole.compress[Lzma2].stream.decompress[Lzma2].memoize.to[List]
      . assert(_ == lzma2Whole.to[List])

      test(m"stream compress feeds the whole-value decompressor (LZMA2)"):
        lzma2Whole.stream.compress[Lzma2].memoize.decompress[Lzma2].to[List]
      . assert(_ == lzma2Whole.to[List])

      test(m"Roundtrip varied data (LZMA2)"):
        lzma2Varied.compress[Lzma2].decompress[Lzma2].to[List]
      . assert(_ == lzma2Varied.to[List])

      test(m"Empty input roundtrips (LZMA2)"):
        Data().compress[Lzma2].decompress[Lzma2].to[List]
      . assert(_ == Nil)

      test(m"Single byte roundtrips (LZMA2)"):
        Data(42).compress[Lzma2].decompress[Lzma2].to[List]
      . assert(_ == List[Byte](42))

      test(m"Explicit preset and dictionary size roundtrip (LZMA2)"):
        Lzma2.decompress(Lzma2.compress(Progression(lzma2Varied), 1),
            Lzma2Options.preset(1).dictSize).stdlib.map(_.readable).flatten.to(proscenium.List)
      . assert(_ == lzma2Varied.to[List])

    suite(m"Compression duct tests"):
      val mixed: Data =
        Data.fill(50000) { index => (index%251).toByte } ++ (t"repetition "*500).in[Data]

      test(m"gzip duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Gzip].decompress[Gzip].pump(gather)
        scala.caps.unsafe.unsafeAssumeSeparate(gather.data.to[List])
      . assert(_ == mixed.to[List])

      test(m"deflate duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Deflate].decompress[Deflate].pump(gather)
        scala.caps.unsafe.unsafeAssumeSeparate(gather.data.to[List])
      . assert(_ == mixed.to[List])

      test(m"zlib duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Zlib].decompress[Zlib].pump(gather)
        scala.caps.unsafe.unsafeAssumeSeparate(gather.data.to[List])
      . assert(_ == mixed.to[List])

      test(m"gzip duct output is genuine gzip"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed).compress[Gzip].pump(gather)
        val stream = scala.caps.unsafe.unsafeAssumeSeparate:
          java.util.zip.GZIPInputStream(ji.ByteArrayInputStream(Array.unsafeJvm(gather.data)))
        Array.unsafeFrozen(stream.readAllBytes().nn).toList
      . assert(_ == mixed.to[List])

      // JDK-produced gzip, delivered one byte per chunk: the header state
      // machine, the inflater's window re-feed and the trailer all span many
      // steps, each offered a single byte.
      test(m"gzip duct decompresses JDK gzip fed one byte at a time"):
        val buffer = ji.ByteArrayOutputStream()
        val zipped = java.util.zip.GZIPOutputStream(buffer)
        zipped.write(Array.unsafeJvm(mixed))
        zipped.close()
        val chunks = buffer.toByteArray.nn.iterator.map { byte => Data(byte) }
        val gather = Gather2()
        Stream(chunks).decompress[Gzip].pump(gather)
        scala.caps.unsafe.unsafeAssumeSeparate(gather.data.to[List])
      . assert(_ == mixed.to[List])

      // The mirror image: compress fed one byte per chunk, so the CRC and size
      // accumulate over single-byte consumptions, validated by the JDK.
      test(m"gzip duct compresses correctly when fed one byte at a time"):
        val chunks = mixed.to[List].iterator.map { byte => Data(byte) }
        val gather = Gather2()
        Stream(chunks).compress[Gzip].pump(gather)
        val stream = scala.caps.unsafe.unsafeAssumeSeparate:
          java.util.zip.GZIPInputStream(ji.ByteArrayInputStream(Array.unsafeJvm(gather.data)))
        Array.unsafeFrozen(stream.readAllBytes().nn).toList
      . assert(_ == mixed.to[List])

      // Tiny demand: each refill grants a few bytes, so the inflater retains
      // pending output and unconsumed input across many output-bound steps,
      // exercising the un-claim/re-feed path.
      test(m"gzip duct decompresses correctly under three-byte demand"):
        val stream = summon[Data is Streamable by Data over Credit].stream(mixed)
                     . compress[Gzip].decompress[Gzip]
        val builder = scala.collection.immutable.List.newBuilder[Byte]

        def recur(): Unit = scala.caps.unsafe.unsafeAssumeSeparate:
         stream.refill(Credit(3)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[scala.Array[Byte]]
            var index = 0
            while index < count do
              builder += window(stream.start + index)
              index += 1
            stream.skip(count)
            recur()

          case _ => ()

        scala.caps.unsafe.unsafeAssumeSeparate(recur())
        proscenium.List.of(builder.result())
      . assert(_ == mixed.readable.to(proscenium.List))

      test(m"gzip duct decompresses JDK-produced gzip"):
        val out = ji.ByteArrayOutputStream()
        val zipped = java.util.zip.GZIPOutputStream(out)
        zipped.write(Array.unsafeJvm(mixed))
        zipped.close()
        val gather = Gather2()

        summon[Progression[Data] is Streamable by Data over Credit]
        . stream(Array.unsafeFrozen(out.toByteArray.nn).readable.grouped(7).map(Array.frozen(_)).to(Progression))
        . decompress[Gzip].pump(gather)

        scala.caps.unsafe.unsafeAssumeSeparate(gather.data.to[List])
      . assert(_ == mixed.to[List])

class Gather2() extends Intake[Data]:
  type Transport = Credit

  private val block: Int = 16
  private val storage: addressable.Storage = addressable.allocate(block).asInstanceOf[addressable.Storage]
  private val target: addressable.Target = addressable.blank(64)
  private var mark1: Int = 0

  def demand: Credit = Credit(Long.MaxValue)
  protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
  def mark: Int = mark1

  update def reserve(min: Int): Int =
    val free = block - mark1

    if free >= min then free else
      drain()
      block

  update def commit(count: Int): Unit =
    mark1 += count
    if mark1 == block then drain()

  update def finish(): Unit = drain()

  update def data: Data =
    drain()
    addressable.build(target)

  private update def drain(): Unit =
    if mark1 > 0 then
      addressable.cloneStorage(storage, 0, mark1)(target)
      mark1 = 0

