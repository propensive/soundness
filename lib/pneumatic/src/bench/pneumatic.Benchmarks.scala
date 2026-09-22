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

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*
import zephyrine.*

// Every pneumatic codec over the same two 4 MB corpora, compressing and decompressing through
// the streaming `Duct` path. The XZ rows are measured beside xz-java (`org.tukaani.xz`), the
// reference Java implementation of the format and the library the JVM ecosystem otherwise
// reaches for; raw LZMA2 and LZW have no comparable rival, and Gzip (native zlib) and Brotli sit
// alongside as in-module reference points. Each benchmark's body is written inline in its
// `bench` block; the corpora and the `count` terminal are the only members the staged bodies
// reference, by fully-qualified name.
//
// Two corpora, because LZMA behaves very differently on each: the *pattern* corpus is a
// low-period arithmetic sequence that decodes almost entirely as long matches, so it measures
// the copy paths; the *text* corpus is pseudo-random prose from a small vocabulary, which is
// literal- and short-match-heavy and measures the range decoder and the literal coder.
object Benchmarks extends Suite(m"Pneumatic benchmarks: XZ, LZMA2, LZW, Gzip and Brotli"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))
  given Buffering                               = Buffering.standard

  // ── Corpora (forced once in warmup; referenced by fully-qualified name in the staged
  //    bodies) ──────────────────────────────────────────────────────────────────────

  // 4 MB of semi-compressible bytes: the same repeating low-period pattern as the turbulence
  // benchmarks, so the Gzip and Brotli rows here are comparable with those.
  lazy val pattern: Data = Data.fill(4 << 20)(i => ((i*31 + (i >> 6)) & 0xff).toByte)
  lazy val patternArray: scala.Array[Byte] = pattern.asInstanceOf[scala.Array[Byte]]

  // ~4 MB of prose-shaped text: words drawn from a small vocabulary by a fixed linear
  // congruential generator, so every run compresses exactly the same bytes.
  lazy val text: Data =
    val vocabulary = IArray(
      "the", "of", "and", "to", "in", "a", "is", "that", "for", "it", "as", "was", "with",
      "be", "by", "on", "not", "he", "this", "are", "or", "his", "from", "at", "which", "but",
      "have", "an", "had", "they", "you", "were", "their", "one", "all", "we", "can", "her",
      "has", "there", "been", "if", "more", "when", "will", "would", "who", "so", "no",
      "compression", "dictionary", "stream", "buffer", "window", "literal", "match", "distance",
      "length", "encoder", "decoder", "probability", "range", "symbol", "block", "header")

    val builder = new java.lang.StringBuilder(4 << 20 | 64)
    var seed = 0x2545f491L
    var sentence = 0

    while builder.length < (4 << 20) do
      seed = (seed*6364136223846793005L + 1442695040888963407L)
      val word = vocabulary(((seed >>> 33)%vocabulary.length).toInt)
      if sentence == 0 then builder.append(Character.toUpperCase(word.charAt(0)))
      builder.append(word, if sentence == 0 then 1 else 0, word.length)
      sentence += 1

      if sentence >= 8 + ((seed >>> 20)%9).toInt then
        builder.append(". ")
        sentence = 0
      else builder.append(' ')

    Data(builder.toString.getBytes("UTF-8").nn*)

  lazy val textArray: scala.Array[Byte] = text.asInstanceOf[scala.Array[Byte]]

  // The corpora pre-compressed by each codec, for the decompression rows.
  lazy val xzPattern: Data = pattern.stream.compress[Xz].memoize
  lazy val xzPatternArray: scala.Array[Byte] = xzPattern.asInstanceOf[scala.Array[Byte]]
  lazy val xzText: Data = text.stream.compress[Xz].memoize
  lazy val xzTextArray: scala.Array[Byte] = xzText.asInstanceOf[scala.Array[Byte]]
  lazy val lzma2Pattern: Data = pattern.stream.compress[Lzma2].memoize
  lazy val lzma2Text: Data = text.stream.compress[Lzma2].memoize
  lazy val lzwPattern: Data = pattern.stream.compress[Lzw].memoize
  lazy val lzwText: Data = text.stream.compress[Lzw].memoize
  lazy val gzipPattern: Data = pattern.stream.compress[Gzip].memoize
  lazy val gzipText: Data = text.stream.compress[Gzip].memoize
  lazy val brotliPattern: Data = pattern.stream.compress[Brotli].memoize
  lazy val brotliText: Data = text.stream.compress[Brotli].memoize

  // The counting terminal every row ends in: the stream is pulled to its end and its length
  // summed, never materialised, so the rows measure the codec and not a final concatenation.
  def count[medium](stream: Stream[medium] over Credit)(using Buffering): Long =
    stream.gather(0L)(_ => (total, range) => total + (range: Interval).size)

  // xz-java's whole-value compress, counted the same way: the bytes are written to a
  // counting sink rather than accumulated.
  def xzJavaCompress(bytes: scala.Array[Byte]): Long =
    val sink = CountingOutputStream()
    val out = org.tukaani.xz.XZOutputStream(sink, org.tukaani.xz.LZMA2Options(6))
    out.write(bytes)
    out.close()
    sink.total

  def xzJavaDecompress(bytes: scala.Array[Byte]): Long =
    val in = org.tukaani.xz.XZInputStream(java.io.ByteArrayInputStream(bytes))
    val buffer = new scala.Array[Byte](65536)
    var total = 0L
    var count = in.read(buffer)

    while count >= 0 do
      total += count
      count = in.read(buffer)

    total

  class CountingOutputStream extends java.io.OutputStream:
    var total: Long = 0L
    def write(byte: Int): Unit = total += 1
    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      total += length

  def run(): Unit =
    val bench = Bench()
    val patternSize = pattern.length*Byte
    val textSize = text.length*Byte

    // Force the corpora and their compressed forms outside the timed regions.
    xzPattern; xzText; lzma2Pattern; lzma2Text; lzwPattern; lzwText
    gzipPattern; gzipText; brotliPattern; brotliText

    suite(m"XZ compression (4 MB pattern)"):
      bench(m"Soundness  Stream.compress[Xz]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.pattern.stream.compress[Xz]) }

      bench(m"xz-java  XZOutputStream")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.xzJavaCompress(pneumatic.Benchmarks.patternArray) }

    suite(m"XZ compression (4 MB text)"):
      bench(m"Soundness  Stream.compress[Xz]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.text.stream.compress[Xz]) }

      bench(m"xz-java  XZOutputStream")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.xzJavaCompress(pneumatic.Benchmarks.textArray) }

    suite(m"XZ decompression (4 MB pattern)"):
      bench(m"Soundness  Stream.decompress[Xz]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.xzPattern.stream.decompress[Xz]) }

      bench(m"xz-java  XZInputStream")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.xzJavaDecompress(pneumatic.Benchmarks.xzPatternArray) }

    suite(m"XZ decompression (4 MB text)"):
      bench(m"Soundness  Stream.decompress[Xz]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.xzText.stream.decompress[Xz]) }

      bench(m"xz-java  XZInputStream")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.xzJavaDecompress(pneumatic.Benchmarks.xzTextArray) }

    suite(m"Raw LZMA2 (4 MB pattern)"):
      bench(m"Soundness  Stream.compress[Lzma2]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.pattern.stream.compress[Lzma2]) }

      bench(m"Soundness  Stream.decompress[Lzma2]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.lzma2Pattern.stream.decompress[Lzma2]) }

    suite(m"Raw LZMA2 (4 MB text)"):
      bench(m"Soundness  Stream.compress[Lzma2]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.text.stream.compress[Lzma2]) }

      bench(m"Soundness  Stream.decompress[Lzma2]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.lzma2Text.stream.decompress[Lzma2]) }

    suite(m"LZW (4 MB pattern)"):
      bench(m"Soundness  Stream.compress[Lzw]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.pattern.stream.compress[Lzw]) }

      bench(m"Soundness  Stream.decompress[Lzw]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.lzwPattern.stream.decompress[Lzw]) }

    suite(m"LZW (4 MB text)"):
      bench(m"Soundness  Stream.compress[Lzw]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.text.stream.compress[Lzw]) }

      bench(m"Soundness  Stream.decompress[Lzw]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.lzwText.stream.decompress[Lzw]) }

    suite(m"Gzip and Brotli, for reference (4 MB pattern)"):
      bench(m"Soundness  Stream.compress[Gzip]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.pattern.stream.compress[Gzip]) }

      bench(m"Soundness  Stream.decompress[Gzip]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.gzipPattern.stream.decompress[Gzip]) }

      bench(m"Soundness  Stream.compress[Brotli]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.pattern.stream.compress[Brotli]) }

      bench(m"Soundness  Stream.decompress[Brotli]")(target = 2*Second, operationSize = patternSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.brotliPattern.stream.decompress[Brotli]) }

    suite(m"Gzip and Brotli, for reference (4 MB text)"):
      bench(m"Soundness  Stream.compress[Gzip]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.text.stream.compress[Gzip]) }

      bench(m"Soundness  Stream.decompress[Gzip]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.gzipText.stream.decompress[Gzip]) }

      bench(m"Soundness  Stream.compress[Brotli]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.text.stream.compress[Brotli]) }

      bench(m"Soundness  Stream.decompress[Brotli]")(target = 2*Second, operationSize = textSize):
        '{ pneumatic.Benchmarks.count(pneumatic.Benchmarks.brotliText.stream.decompress[Brotli]) }
