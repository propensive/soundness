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
package pneumatic

import java.io as ji

import soundness.*
import proscenium.compat.*

import charEncoders.utf8Encoder, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
import threading.platformThreading
import strategies.throwUnsafely
import probates.panicProbate
import errorDiagnostics.emptyDiagnostics

object Tests extends Suite(m"Pneumatic tests"):
  def run(): Unit =
    suite(m"Compression tests"):
      test(m"Compress a single block with GZip"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].stdlib.to(List).map(_.to(List))
      . assert(_ == List(List(31, -117, 8, 0, 0, 0, 0, 0, 0, -1), List(99, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, -56, -16, -118, -53, 9, 0, 0, 0)))

      test(m"Roundtrip compress/decompress a single block with GZip"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].decompress[Gzip]
      . assert: stream => stream === Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      val longData: proscenium.Progression[Data] = proscenium.Progression.from(proscenium.Progression.continually(IArray.from((0 to 255).map(_.toByte))).stdlib.take(1000))

      test(m"Roundtrip compress/decompress a long repetitive stream with Gzip"):
        longData.compress[Gzip].decompress[Gzip]
      . assert(_.stdlib.flatten == longData.stdlib.flatten)

      // The whole-value forms (`Duct.feed` over the format ducts) must
      // interoperate with the stream forms in both directions, per format.
      val wholeData: Data = IArray.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)

      for format <- List(t"Gzip", t"Zlib", t"Deflate") do
        test(m"whole-value compress roundtrips through whole-value decompress ($format)"):
          format.s match
            case "Gzip"    => wholeData.compress[Gzip].decompress[Gzip].to(List)
            case "Zlib"    => wholeData.compress[Zlib].decompress[Zlib].to(List)
            case _         => wholeData.compress[Deflate].decompress[Deflate].to(List)
        . assert(_ == wholeData.to(List))

        test(m"whole-value compress feeds the stream decompressor ($format)"):
          format.s match
            case "Gzip"    => wholeData.compress[Gzip].stream.decompress[Gzip].memoize.to(List)
            case "Zlib"    => wholeData.compress[Zlib].stream.decompress[Zlib].memoize.to(List)
            case _ => wholeData.compress[Deflate].stream.decompress[Deflate].memoize.to(List)
        . assert(_ == wholeData.to(List))

        test(m"stream compress feeds the whole-value decompressor ($format)"):
          format.s match
            case "Gzip"    => wholeData.stream.compress[Gzip].memoize.decompress[Gzip].to(List)
            case "Zlib"    => wholeData.stream.compress[Zlib].memoize.decompress[Zlib].to(List)
            case _ => wholeData.stream.compress[Deflate].memoize.decompress[Deflate].to(List)
        . assert(_ == wholeData.to(List))

      test(m"Roundtrip compress/decompress a single block with LZW"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.flatten)

      // Varied enough to push the code table through its 9-, 10- and 11-bit widths.
      val variedData: proscenium.Progression[Data] =
        Progression(IArray.from((0 until 20000).map { index => ((index*index + index/3)%251).toByte }))

      test(m"Roundtrip compress/decompress across LZW width growth"):
        variedData.compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.flatten == variedData.stdlib.flatten)

      test(m"Roundtrip compress/decompress a long stream across LZW table clears"):
        longData.compress[Lzw].decompress[Lzw]
      . assert(_.stdlib.flatten == longData.stdlib.flatten)

      test(m"LZW without early change also roundtrips"):
        Lzw.decompress(Lzw.compress(variedData, earlyChange = false), earlyChange = false)
      . assert(_.stdlib.flatten == variedData.stdlib.flatten)
      test(m"Compress a single block with Zlib"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].stdlib.to(List).map(_.to(List))
      . assert(_ == List(List(120, -100, 98, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, 0, 0, -1, -1), List(3, 0, 0, -26, 0, 89)))

      test(m"Roundtrip compress/decompress a single block with Zlib"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].decompress[Zlib]
      . assert: stream => stream === Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip compress/decompress a long repetitive stream with Zlib"):
        longData.compress[Zlib].decompress[Zlib]
      . assert: stream => stream === longData

      test(m"Compress a single block with Deflate"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].stdlib.to(List).map(_.to(List))
      . assert(_ == List(List(98, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, 0, 0, -1, -1), List(3, 0)))

      test(m"Roundtrip compress/decompress a single block with Deflate"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].decompress[Deflate]
      . assert: stream => stream === Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip a long repetitive Deflate stream"):
        longData.compress[Deflate].decompress[Deflate]
      . assert: stream => stream === longData

    suite(m"Pure DEFLATE implementation tests"):
      // On the JVM the formats run over `java.util.zip`, so the pure-Scala port (used on
      // Scala.js and WASI) is exercised directly here, cross-validated against the JDK's zlib
      // in both directions and via byte-for-byte output equality.
      val corpus: Data =
        IArray.from((0 until 300000).map { index => ((index*31 + (index >> 6)) & 0xff).toByte })

      def jdkInflate(data: Data, nowrap: Boolean): List[Byte] =
        val inflater = java.util.zip.Inflater(nowrap)
        inflater.setInput(data.mutable(using Unsafe))
        val out = ji.ByteArrayOutputStream()
        val buffer = new Array[Byte](4096)

        while !inflater.finished && !inflater.needsInput do
          val count = inflater.inflate(buffer)
          out.write(buffer, 0, count)

        inflater.end()
        scala.collection.immutable.ArraySeq.unsafeWrapArray(out.toByteArray.nn).to(List)

      def jdkDeflate(data: Data, nowrap: Boolean): Data =
        val deflater = java.util.zip.Deflater(-1, nowrap)
        deflater.setInput(data.mutable(using Unsafe))
        deflater.finish()
        val out = ji.ByteArrayOutputStream()
        val buffer = new Array[Byte](4096)

        while !deflater.finished do
          val count = deflater.deflate(buffer)
          out.write(buffer, 0, count)

        deflater.end()
        out.toByteArray.nn.immutable(using Unsafe)

      def pureDeflate(data: Data, nowrap: Boolean): Data =
        val deflater = Deflater(-1, nowrap)
        deflater.setInput(data.mutable(using Unsafe))
        deflater.finish()
        val out = ji.ByteArrayOutputStream()
        val buffer = new Array[Byte](4096)

        while !deflater.finished do
          val count = deflater.deflate(buffer, 0, buffer.length)
          out.write(buffer, 0, count)

        out.toByteArray.nn.immutable(using Unsafe)

      def pureInflate(data: Data, nowrap: Boolean, chunk: Int): List[Byte] =
        val inflater = Inflater(nowrap)
        val bytes = data.mutable(using Unsafe)
        val out = ji.ByteArrayOutputStream()
        val buffer = new Array[Byte](4096)
        var position = 0

        while position < bytes.length && !inflater.finished do
          val length = chunk.min(bytes.length - position)
          inflater.setInput(bytes, position, length)
          var run = 1

          while run > 0 do
            run = inflater.inflate(buffer, 0, buffer.length)
            out.write(buffer, 0, run)

          position += length - inflater.getRemaining

        scala.collection.immutable.ArraySeq.unsafeWrapArray(out.toByteArray.nn).to(List)

      test(m"pure deflate output inflates with the JDK (raw)"):
        jdkInflate(pureDeflate(corpus, true), true)
      . assert(_ == corpus.to(List))

      test(m"pure deflate output inflates with the JDK (zlib)"):
        jdkInflate(pureDeflate(corpus, false), false)
      . assert(_ == corpus.to(List))

      test(m"JDK deflate output inflates with the pure implementation (raw)"):
        pureInflate(jdkDeflate(corpus, true), true, Int.MaxValue)
      . assert(_ == corpus.to(List))

      test(m"JDK deflate output inflates with the pure implementation (zlib)"):
        pureInflate(jdkDeflate(corpus, false), false, Int.MaxValue)
      . assert(_ == corpus.to(List))

      test(m"pure inflate succeeds fed seven bytes at a time"):
        pureInflate(jdkDeflate(corpus, false), false, 7)
      . assert(_ == corpus.to(List))

      test(m"pure deflate output is byte-identical to the JDK's (raw)"):
        pureDeflate(corpus, true).to(List)
      . assert(_ == jdkDeflate(corpus, true).to(List))

      test(m"pure deflate output is byte-identical to the JDK's (zlib)"):
        pureDeflate(corpus, false).to(List)
      . assert(_ == jdkDeflate(corpus, false).to(List))

      test(m"pure roundtrip without the JDK"):
        pureInflate(pureDeflate(corpus, false), false, 4096)
      . assert(_ == corpus.to(List))

      test(m"Whole-value gzip roundtrips through gunzip"):
        corpus.gzip.gunzip.to(List)
      . assert(_ == corpus.to(List))

      test(m"A gzip stream with optional header fields decodes"):
        // FLG = FEXTRA | FNAME | FCOMMENT exercises every optional-field state
        val out = ji.ByteArrayOutputStream()
        val payload: Data = t"optional header fields".in[Data]
        val deflated = jdkDeflate(payload, true)
        val crc = java.util.zip.CRC32()
        crc.update(payload.mutable(using Unsafe))

        val headerStart: Array[Byte] =
          Array[Byte](31, -117, 8, (4 | 8 | 16).toByte, 0, 0, 0, 0, 0, -1)

        out.write(headerStart)
        out.write(Array[Byte](3, 0)) // XLEN = 3
        out.write(Array[Byte](1, 2, 3)) // extra field
        out.write(Array[Byte]('n', 'a', 'm', 'e', 0)) // zero-terminated name
        out.write(Array[Byte]('c', 'o', 'm', 'm', 'e', 'n', 't', 0)) // zero-terminated comment
        out.write(deflated.mutable(using Unsafe))

        var index = 0
        while index < 4 do
          out.write(((crc.getValue >>> (index*8)) & 0xff).toInt)
          index += 1

        index = 0
        while index < 4 do
          out.write(((payload.length >>> (index*8)) & 0xff).toInt)
          index += 1

        out.toByteArray.nn.immutable(using Unsafe).decompress[Gzip].to(List)
      . assert(_ == t"optional header fields".in[Data].to(List))


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
        xBrotli.decompress[Brotli].to(List)
      . assert(_ == t"x".in[Data].to(List))

      test(m"Decode reference Brotli output (run-length)"):
        tenXtenYBrotli.decompress[Brotli].to(List)
      . assert(_ == t"XXXXXXXXXXYYYYYYYYYY".in[Data].to(List))

      test(m"Decode reference Brotli output (natural-language text)"):
        ukkonooaBrotli.decompress[Brotli].to(List)
      . assert(_ == ukkonooaPlain.in[Data].to(List))

      test(m"Decode reference Brotli output using the static dictionary"):
        foxBrotli.decompress[Brotli].to(List)
      . assert(_ == foxPlain.in[Data].to(List))

      val brotliLong: proscenium.Progression[Data] = proscenium.Progression.from(proscenium.Progression.continually(IArray.from((0 to 255).map(_.toByte))).stdlib.take(1000))
      val brotliWhole: Data = IArray.from((0 to 255).map(_.toByte)) ++ Data(1, 1, 2, 3, 5, 8, 13)
      val brotliVaried: Data =
        IArray.from((0 until 40000).map { index => ((index*index + index/3)%251).toByte })

      test(m"Roundtrip compress/decompress a single block with Brotli"):
        Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Brotli].decompress[Brotli]
      . assert(_.stdlib.flatten == proscenium.Progression(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).stdlib.flatten)

      test(m"Roundtrip compress/decompress a long repetitive stream with Brotli"):
        brotliLong.compress[Brotli].decompress[Brotli]
      . assert(_.stdlib.flatten == brotliLong.stdlib.flatten)

      test(m"whole-value compress roundtrips through whole-value decompress (Brotli)"):
        brotliWhole.compress[Brotli].decompress[Brotli].to(List)
      . assert(_ == brotliWhole.to(List))

      test(m"whole-value compress feeds the stream decompressor (Brotli)"):
        brotliWhole.compress[Brotli].stream.decompress[Brotli].memoize.to(List)
      . assert(_ == brotliWhole.to(List))

      test(m"stream compress feeds the whole-value decompressor (Brotli)"):
        brotliWhole.stream.compress[Brotli].memoize.decompress[Brotli].to(List)
      . assert(_ == brotliWhole.to(List))

      test(m"Roundtrip varied data spanning many commands (Brotli)"):
        brotliVaried.compress[Brotli].decompress[Brotli].to(List)
      . assert(_ == brotliVaried.to(List))

      test(m"Brotli actually compresses a repetitive payload"):
        val payload = (t"the quick brown fox jumped " * 500).in[Data]
        payload.compress[Brotli].length < payload.length
      . assert(_ == true)

      test(m"Roundtrip a large multi-command payload (Brotli)"):
        val big = IArray.from((0 until 2000000).map { index => ((index*31 + (index >> 6)) & 0xff).toByte })
        big.compress[Brotli].decompress[Brotli].to(List) == big.to(List)
      . assert(_ == true)

      test(m"Empty input roundtrips (Brotli)"):
        Data().compress[Brotli].decompress[Brotli].to(List)
      . assert(_ == Nil)

      test(m"Single byte roundtrips (Brotli)"):
        Data(42).compress[Brotli].decompress[Brotli].to(List)
      . assert(_ == List[Byte](42))


    suite(m"Compression duct tests"):
      val mixed: Data =
        Data.fill(50000) { index => (index%251).toByte } ++ (t"repetition "*500).in[Data]

      test(m"gzip duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Gzip].decompress[Gzip].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"deflate duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Deflate].decompress[Deflate].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"zlib duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed)
        . compress[Zlib].decompress[Zlib].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"gzip duct output is genuine gzip"):
        val gather = Gather2()
        summon[Data is Streamable by Data over Credit].stream(mixed).compress[Gzip].pump(gather)
        val stream = java.util.zip.GZIPInputStream(ji.ByteArrayInputStream(gather.data.mutable(using Unsafe)))
        scala.collection.immutable.ArraySeq.unsafeWrapArray(stream.readAllBytes().nn).to(List)
      . assert(_ == mixed.to(List))

      // JDK-produced gzip, delivered one byte per chunk: the header state
      // machine, the inflater's window re-feed and the trailer all span many
      // steps, each offered a single byte.
      test(m"gzip duct decompresses JDK gzip fed one byte at a time"):
        val buffer = ji.ByteArrayOutputStream()
        val zipped = java.util.zip.GZIPOutputStream(buffer)
        zipped.write(mixed.mutable(using Unsafe))
        zipped.close()
        val chunks = buffer.toByteArray.nn.iterator.map { byte => Data(byte) }
        val gather = Gather2()
        Stream(chunks).decompress[Gzip].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      // The mirror image: compress fed one byte per chunk, so the CRC and size
      // accumulate over single-byte consumptions, validated by the JDK.
      test(m"gzip duct compresses correctly when fed one byte at a time"):
        val chunks = mixed.to(List).iterator.map { byte => Data(byte) }
        val gather = Gather2()
        Stream(chunks).compress[Gzip].pump(gather)
        val stream = java.util.zip.GZIPInputStream(ji.ByteArrayInputStream(gather.data.mutable(using Unsafe)))
        scala.collection.immutable.ArraySeq.unsafeWrapArray(stream.readAllBytes().nn).to(List)
      . assert(_ == mixed.to(List))

      // Tiny demand: each refill grants a few bytes, so the inflater retains
      // pending output and unconsumed input across many output-bound steps,
      // exercising the un-claim/re-feed path.
      test(m"gzip duct decompresses correctly under three-byte demand"):
        val stream = summon[Data is Streamable by Data over Credit].stream(mixed)
                     . compress[Gzip].decompress[Gzip]
        val builder = scala.collection.immutable.List.newBuilder[Byte]

        def recur(): Unit = stream.refill(Credit(3)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[Array[Byte]]
            var index = 0
            while index < count do
              builder += window(stream.start + index)
              index += 1
            stream.skip(count)
            recur()

          case _ => ()

        recur()
        builder.result()
      . assert(_ == mixed.to(List))

      test(m"gzip duct decompresses JDK-produced gzip"):
        val out = ji.ByteArrayOutputStream()
        val zipped = java.util.zip.GZIPOutputStream(out)
        zipped.write(mixed.mutable(using Unsafe))
        zipped.close()
        val gather = Gather2()

        summon[Progression[Data] is Streamable by Data over Credit]
        . stream(out.toByteArray.nn.immutable(using Unsafe).grouped(7).to(Progression))
        . decompress[Gzip].pump(gather)

        gather.data.to(List)
      . assert(_ == mixed.to(List))

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

